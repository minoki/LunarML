local
    structure Instream : sig
                  (* instream-related part of STREAM_IO *)
                  type reader = BinPrimIO.reader
                  type instream
                  type elem = Word8.word
                  type vector = Word8Vector.vector
                  type pos = BinPrimIO.pos
                  val input : instream -> vector * instream
                  val input1 : instream -> (elem * instream) option
                  val inputN : instream * int -> vector * instream
                  val inputAll : instream -> vector * instream
                  val canInput : instream * int -> int option
                  val closeIn : instream -> unit
                  val endOfStream : instream -> bool
                  val mkInstream : reader * vector -> instream
                  val getReader : instream -> reader * vector
                  val filePosIn : instream -> pos
              end = struct
    type elem = Word8.word
    type vector = Word8Vector.vector
    type reader = BinPrimIO.reader
    datatype state = PRIM_READER of reader
                   | BUFFERED of { buffer : vector, position : int, next : instream, initialPosition : BinPrimIO.pos option } (* invariant: 0 <= position < String.size buffer *) (* name? *)
                   | CLOSED of string (* name *)
    withtype instream = state ref
    type pos = BinPrimIO.pos
    (*: val getPosOpt : BinPrimIO.reader -> BinPrimIO.pos option *)
    fun getPosOpt (BinPrimIO.RD { getPos = SOME getPos, ... }) = SOME (getPos ())
      | getPosOpt _ = NONE
    fun getName ins = case !ins of
                          PRIM_READER (BinPrimIO.RD { name, ... }) => name
                        | BUFFERED { next, ... } => getName next
                        | CLOSED name => name
    fun mkInstream (rd, content) = let val tip = ref (PRIM_READER rd)
                                   in if Word8Vector.length content > 0 then
                                          ref (BUFFERED { buffer = content, position = 0, next = tip, initialPosition = NONE })
                                      else
                                          tip
                                   end
    local
        fun doGetReader (ins, acc) = case !ins of
                                         PRIM_READER (rd as BinPrimIO.RD { name, ... }) => (ins := CLOSED name; (rd, Word8VectorSlice.concat (List.rev acc)))
                                       | BUFFERED { buffer, position, next, initialPosition = _ } => doGetReader (next, Word8VectorSlice.slice (buffer, position, NONE) :: acc)
                                       | CLOSED name => raise IO.Io { name = name, function = "getReader", cause = IO.ClosedStream }
    in
    fun getReader ins = doGetReader (ins, [])
    end
    fun filePosIn ins = case !ins of
                            PRIM_READER (BinPrimIO.RD { name, getPos = NONE, ... }) => raise IO.Io { name = name, function = "filePosIn", cause = IO.RandomAccessNotSupported }
                          | PRIM_READER (BinPrimIO.RD { getPos = SOME getPos, ... }) => getPos ()
                          | BUFFERED { buffer = _, position = _, next, initialPosition = NONE } => raise IO.Io { name = getName next, function = "filePosIn", cause = IO.RandomAccessNotSupported }
                          | BUFFERED { buffer = _, position, next = _, initialPosition = SOME initialPosition } => initialPosition + Position.fromInt position
                          | CLOSED name => raise IO.Io { name = name, function = "filePosIn", cause = IO.ClosedStream }
    fun fillBufferWithReader (rd as BinPrimIO.RD { name, readVec, readArr, readVecNB, readArrNB, block, ... } : reader, f : instream, n)
        = let val initialPosition = getPosOpt rd
              val chunk = case readVec of
                              SOME rv => rv n
                            | NONE => case readArr of
                                          SOME ra => let val arr = Word8Array.array (n, 0w0)
                                                         val actual = ra (Word8ArraySlice.full arr)
                                                     in Word8ArraySlice.vector (Word8ArraySlice.slice (arr, 0, SOME actual))
                                                     end
                                        | NONE => case (block, readVecNB, readArrNB) of
                                                      (SOME block', SOME rvNB, _) =>
                                                      (case rvNB n of
                                                           SOME content => content
                                                         | NONE => ( block' ()
                                                                   ; case rvNB n of
                                                                         SOME content => content
                                                                       | NONE => Word8Vector.fromList [] (* should not occur *)
                                                                   )
                                                      )
                                                    | (SOME block', NONE, SOME raNB) =>
                                                      let val arr = Word8Array.array (n, 0w0)
                                                          val aslice = Word8ArraySlice.full arr
                                                      in case raNB aslice of
                                                             SOME actual => Word8ArraySlice.vector (Word8ArraySlice.slice (arr, 0, SOME actual))
                                                           | NONE => ( block' ()
                                                                     ; case raNB aslice of
                                                                           SOME actual => Word8ArraySlice.vector (Word8ArraySlice.slice (arr, 0, SOME actual))
                                                                         | NONE => Word8Vector.fromList [] (* should not occur *)
                                                                     )
                                                      end
                                                    | _ => raise IO.Io { name = name, function = "<unknown>", cause = IO.BlockingNotSupported }
          in f := BUFFERED { buffer = chunk, position = 0, next = ref (PRIM_READER rd), initialPosition = initialPosition }
          end
    fun input (f : instream) = case !f of
                                   PRIM_READER (rd as BinPrimIO.RD { chunkSize, ... }) => (fillBufferWithReader (rd, f, chunkSize); input f)
                                 | BUFFERED { buffer, position, next, initialPosition = _ } => (Word8VectorSlice.vector (Word8VectorSlice.slice (buffer, position, NONE)), next)
                                 | CLOSED _ => (Word8Vector.fromList [], f)
    fun newStreamWithBufferAndPosition (buffer, position, next, initialPosition)
        = if position >= Word8Vector.length buffer then
              next
          else
              ref (BUFFERED { buffer = buffer, position = position, next = next, initialPosition = initialPosition })
    fun input1 (f : instream) = case !f of
                                    PRIM_READER (rd as BinPrimIO.RD { chunkSize, ... }) => (fillBufferWithReader (rd, f, chunkSize); input1 f)
                                  | BUFFERED { buffer, position, next, initialPosition } => SOME (Word8Vector.sub (buffer, position), newStreamWithBufferAndPosition (buffer, position + 1, next, initialPosition))
                                  | CLOSED _ => NONE
    fun inputN (f : instream, n) = case !f of
                                       PRIM_READER rd => (fillBufferWithReader (rd, f, n); inputN (f, n))
                                     | BUFFERED { buffer, position, next, initialPosition } =>
                                       let val newPosition = position + n
                                       in if newPosition <= Word8Vector.length buffer then
                                              (Word8VectorSlice.vector (Word8VectorSlice.slice (buffer, position, SOME n)), newStreamWithBufferAndPosition (buffer, newPosition, next, initialPosition))
                                          else
                                              let val buffer0 = Word8VectorSlice.slice (buffer, position, NONE)
                                                  val (buffer1, next) = inputN (next, n - Word8VectorSlice.length buffer0)
                                              in (Word8VectorSlice.concat [buffer0, Word8VectorSlice.full buffer1], next)
                                              end
                                       end
                                     | CLOSED _ => (Word8Vector.fromList [], f)
    fun inputAll (f : instream) = let fun go (contentsRev, f) = let val (content, f) = input f
                                                                in if Word8Vector.length content = 0 then
                                                                       (Word8Vector.concat (List.rev contentsRev), f)
                                                                   else
                                                                       go (content :: contentsRev, f)
                                                                end
                                  in go ([], f) (* Use PrimIO/avail? *)
                                  end
    fun canInput (f : instream, n) = if n < 0 then
                                         raise Size
                                     else
                                         case !f of
                                             PRIM_READER (BinPrimIO.RD { canInput = SOME canInput, ... }) => if canInput () then
                                                                                                                 SOME 1 (* Is this OK? *)
                                                                                                             else
                                                                                                                 NONE
                                           | PRIM_READER (BinPrimIO.RD { name, canInput = NONE, ... }) => raise IO.Io { name = name, function = "canInput", cause = IO.NonblockingNotSupported }
                                           | BUFFERED { buffer, position, next = _, initialPosition = _ } => SOME (Int.min (n, Word8Vector.length buffer - position))
                                           | CLOSED _ => SOME 0
    fun closeIn (f : instream) = case !f of
                                     PRIM_READER (BinPrimIO.RD { name, close, ... }) => ( close (); f := CLOSED name )
                                   | BUFFERED { buffer = _, position = _, next, initialPosition = _ } => closeIn next
                                   | CLOSED _ => ()
    fun endOfStream (f : instream) = case !f of
                                         PRIM_READER (rd as BinPrimIO.RD { chunkSize, ... }) => (fillBufferWithReader (rd, f, chunkSize); endOfStream f)
                                       | BUFFERED { buffer = _, position = _, next = _, initialPosition = _ } => false
                                       | CLOSED _ => true
    end (* structure Instream *)
    structure Outstream : sig
                  (* outstream-related part of STREAM_IO *)
                  type elem = Word8.word
                  type vector = Word8Vector.vector
                  type outstream
                  type out_pos
                  type pos = BinPrimIO.pos
                  type writer = BinPrimIO.writer
                  val output : outstream * vector -> unit
                  val output1 : outstream * elem -> unit
                  val flushOut : outstream -> unit
                  val closeOut : outstream -> unit
                  val setBufferMode : outstream * IO.buffer_mode -> unit
                  val getBufferMode : outstream -> IO.buffer_mode
                  val mkOutstream : writer * IO.buffer_mode -> outstream
                  val getWriter : outstream -> writer * IO.buffer_mode
                  val getPosOut : outstream -> out_pos
                  val setPosOut : out_pos -> outstream
                  val filePosOut : out_pos -> pos
                  val outputAndFlush : outstream * vector -> unit (* for implementing print *)
              end = struct
    type elem = Word8.word
    type vector = Word8Vector.vector
    type writer = BinPrimIO.writer
    datatype outstream = PRIM_WRITER of { writer : writer
                                        , buffer_mode : IO.buffer_mode ref
                                        , buffer : vector list ref (* the last-written item is the first in the list *)
                                        }
    type pos = BinPrimIO.pos
    type out_pos = { writer : writer
                   , buffer_mode : IO.buffer_mode ref
                   , buffer : vector list ref (* the last-written item is the first in the list *)
                   , pos : pos
                   }
    fun output (PRIM_WRITER { writer = BinPrimIO.WR { name, chunkSize, writeVec, ... }, buffer_mode, buffer }, content)
        = case !buffer_mode of
              IO.NO_BUF => (case writeVec of
                                SOME writeVec => ignore (writeVec (Word8VectorSlice.full content)) (* TODO: should we retry if partially written? *)
                              | NONE => raise IO.Io { name = name, function = "output", cause = IO.BlockingNotSupported }
                           )
            | _ (* LINE_BUF | BLOCK_BUF *) =>
              let val b = !buffer
                  val bufSize = List.foldl (fn (z, acc) => acc + Word8Vector.length z) (Word8Vector.length content) b
              in if bufSize >= chunkSize then
                     let val content' = Word8Vector.concat (List.rev (content :: !buffer))
                     in case writeVec of
                            SOME writeVec => ignore (writeVec (Word8VectorSlice.full content')) (* TODO: should we retry if partially written? *)
                          | NONE => raise IO.Io { name = name, function = "output", cause = IO.BlockingNotSupported }
                      ; buffer := []
                     end
                 else
                     buffer := content :: !buffer
              end
    fun output1 (stream, elem) = output (stream, Word8Vector.fromList [elem])
    fun outputAndFlush (PRIM_WRITER { writer = BinPrimIO.WR { name, chunkSize, writeVec, ... }, buffer_mode, buffer }, content)
        = let val content' = Word8Vector.concat (List.rev (content :: !buffer))
          in case writeVec of
                 SOME writeVec => ignore (writeVec (Word8VectorSlice.full content')) (* TODO: should we retry if partially written? *)
               | NONE => raise IO.Io { name = name, function = "output", cause = IO.BlockingNotSupported }
           ; buffer := []
          end
    fun flushOutPrim (name, writeVec, buffer)
        = let val content = Word8Vector.concat (List.rev (!buffer))
              val () = buffer := []
          in if Word8Vector.length content > 0 then
                 case writeVec of
                     SOME writeVec => ignore (writeVec (Word8VectorSlice.full content)) (* TODO: should we retry if partially written? *)
                   | NONE => raise IO.Io { name = name, function = "flushOut", cause = IO.BlockingNotSupported }
             else
                 ()
          end
    fun flushOut (PRIM_WRITER { writer = BinPrimIO.WR { name, writeVec, ... }, buffer_mode = _, buffer })
        = flushOutPrim (name, writeVec, buffer)
    fun closeOut (PRIM_WRITER { writer = BinPrimIO.WR { name, writeVec, close, ... }, buffer_mode = _, buffer })
        = ( flushOutPrim (name, writeVec, buffer)
          ; close ()
          ) (* TODO: Make idempotent *)
    fun setBufferMode (PRIM_WRITER { writer = BinPrimIO.WR { name, writeVec, ... }, buffer_mode, buffer }, mode)
        = ( if mode = IO.NO_BUF then
                flushOutPrim (name, writeVec, buffer)
            else
                ()
          ; buffer_mode := mode
          )
    fun getBufferMode (PRIM_WRITER { buffer_mode, ... }) = !buffer_mode
    fun mkOutstream (w, mode) = PRIM_WRITER { writer = w, buffer_mode = ref mode, buffer = ref [] }
    fun getWriter (PRIM_WRITER { writer as BinPrimIO.WR { name, writeVec, ... }, buffer_mode, buffer })
        = ( flushOutPrim (name, writeVec, buffer)
                         (* TODO: Mark the stream as terminated *)
          ; (writer, !buffer_mode)
          )
    fun getPosOut (PRIM_WRITER { writer as BinPrimIO.WR { name, writeVec, getPos = getPos, ... }, buffer_mode, buffer })
        = case getPos of
              SOME getPos => ( flushOutPrim (name, writeVec, buffer)
                             ; { writer = writer, buffer_mode = buffer_mode, buffer = buffer, pos = getPos () }
                             )
            | NONE => raise IO.Io { name = name, function = "getPosOut", cause = IO.RandomAccessNotSupported }
    fun setPosOut { writer as BinPrimIO.WR { name, setPos, ... }, buffer_mode, buffer, pos }
        = case setPos of
              SOME setPos => ( setPos pos
                             ; PRIM_WRITER { writer = writer, buffer_mode = buffer_mode, buffer = buffer }
                             )
            | NONE => raise IO.Io { name = name, function = "setPosOut", cause = IO.RandomAccessNotSupported }
    fun filePosOut ({ pos, ... } : out_pos) = pos
    end (* structure Outstream *)
in
structure BinIO :> BIN_IO = struct
local
    structure StreamIOImpl = struct
    open Instream
    open Outstream
    end
    structure Base = ImperativeIO (structure StreamIO = StreamIOImpl
                                   structure Vector = Word8Vector
                                   structure Array = Word8Array
                                  )
in
open Base
structure StreamIO = StreamIOImpl
fun openIn path = raise IO.Io { name = path, function = "BinIO.openIn", cause = Fail "BinIO not implemented"}
fun openOut path = raise IO.Io { name = path, function = "BinIO.openOut", cause = Fail "BinIO not implemented"}
fun openAppend path = raise IO.Io { name = path, function = "BinIO.openAppend", cause = Fail "BinIO not implemented"}
end (* local *)
end (* structure BinIO *)
end; (* local *)
