local
    structure Readable :> sig
                  type vector = Word8Vector.vector
                  type readable
                  val read : readable * int -> vector option
                  val readAll : readable -> vector
                  val endOfStream : readable -> bool
                  val close : readable -> unit
                  val getReader : readable * string -> BinPrimIO.reader
                  val fromValue : Lua.value -> readable
                  val toValue : readable -> Lua.value
              end = struct
    type vector = Word8Vector.vector
    type readable = Lua.value
    fun read (stream, n) = let val result = Vector.sub (Lua.method (stream, "read") #[Lua.fromInt n], 0)
                           in if Lua.isFalsy result then
                                  NONE (* EOF *)
                              else
                                  SOME (Lua.unsafeFromValue result : vector)
                           end
    fun readAll stream = let val result = Vector.sub (Lua.method (stream, "read") #[Lua.fromString "a"], 0)
                         in Lua.unsafeFromValue result : vector
                         end
    fun endOfStream stream = let val result = Vector.sub (Lua.method (stream, "read") #[Lua.fromInt 0], 0)
                             in Lua.isFalsy result
                             end
    fun close stream = ignore (Lua.method (stream, "close") #[])
    fun readVec stream n = case read (stream, n) of
                               NONE => Word8Vector.fromList []
                             | SOME s => s
    fun getReader (stream : readable, name : string)
        = let val ioDesc = IODesc.toDesc stream
          in BinPrimIO.RD { name = name
                          , chunkSize = 1024 (* tentative *)
                          , readVec = SOME (readVec stream)
                          , readArr = NONE
                          , readVecNB = NONE
                          , readArrNB = NONE
                          , block = NONE
                          , canInput = NONE
                          , avail = fn () => NONE
                          , getPos = NONE (* TODO: stream:seek() *)
                          , setPos = NONE (* TODO: stream:seek() *)
                          , endPos = NONE (* TODO: stream:seek() *)
                          , verifyPos = NONE (* TODO: stream:seek() *)
                          , close = fn () => (IODesc.release ioDesc; close stream)
                          , ioDesc = SOME ioDesc
                          }
          end
    fun fromValue x : Lua.value = x
    val toValue = fromValue
    end
    structure Instream : sig
                  (* instream-related part of TEXT_STREAM_IO *)
                  type reader
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
                  val openReadable : Readable.readable * string -> instream
              end = struct
    type elem = Word8.word
    type vector = Word8Vector.vector
    type reader = BinPrimIO.reader
    datatype state = READABLE of { readable : Readable.readable, name : string }
                   | PRIM_READER of reader
                   | BUFFERED of { buffer : vector, position : int, next : instream, initialPosition : BinPrimIO.pos option } (* invariant: 0 <= position < String.size buffer or String.size buffer = 0 *)
                   | CLOSED of string (* name *)
    withtype instream = state ref
    type pos = BinPrimIO.pos
    (*: val getPosOpt : BinPrimIO.reader -> BinPrimIO.pos option *)
    fun getPosOpt (BinPrimIO.RD { getPos = SOME getPos, ... }) = SOME (getPos ())
      | getPosOpt _ = NONE
    fun getName ins = case !ins of
                          READABLE { name, ... } => name
                        | PRIM_READER (BinPrimIO.RD { name, ... }) => name
                        | BUFFERED { next, ... } => getName next
                        | CLOSED name => name
    fun openReadable (stream, name) = ref (READABLE { readable = stream, name = name })
    fun mkInstream (rd, content) = let val tip = case rd of
                                                     BinPrimIO.RD { name, ioDesc = SOME ioDesc, ... } => ref (READABLE { readable = Readable.fromValue (IODesc.fromDesc ioDesc), name = name })
                                                   | _ => ref (PRIM_READER rd)
                                   in if Word8Vector.length content > 0 then
                                          ref (BUFFERED { buffer = content, position = 0, next = tip, initialPosition = NONE })
                                      else
                                          tip
                                   end
    local
        fun doGetReader (ins, acc) = case !ins of
                                         READABLE { readable, name } => (ins := CLOSED name; (Readable.getReader (readable, name), Word8VectorSlice.concat (List.rev acc)))
                                       | PRIM_READER (rd as BinPrimIO.RD { name, ... }) => (ins := CLOSED name; (rd, Word8VectorSlice.concat (List.rev acc)))
                                       | BUFFERED { buffer, position, next, initialPosition = _ } => doGetReader (next, Word8VectorSlice.slice (buffer, position, NONE) :: acc)
                                       | CLOSED name => raise IO.Io { name = name, function = "getReader", cause = IO.ClosedStream }
    in
    fun getReader ins = doGetReader (ins, [])
    end
    fun filePosIn ins = case !ins of
                            READABLE { name, ... } => raise IO.Io { name = name, function = "filePosIn", cause = IO.RandomAccessNotSupported }
                          | PRIM_READER (BinPrimIO.RD { name, getPos = NONE, ... }) => raise IO.Io { name = name, function = "filePosIn", cause = IO.RandomAccessNotSupported }
                          | PRIM_READER (BinPrimIO.RD { getPos = SOME getPos, ... }) => getPos ()
                          | BUFFERED { buffer, position, next, initialPosition = NONE } => raise IO.Io { name = getName next, function = "filePosIn", cause = IO.RandomAccessNotSupported }
                          | BUFFERED { buffer = _, position, next, initialPosition = SOME initialPosition } => initialPosition + Position.fromInt position
                          | CLOSED name => raise IO.Io { name = name, function = "filePosIn", cause = IO.ClosedStream }
    fun fillBuffer (stream as { readable, ... } : { readable : Readable.readable, name : string }, f : instream, n : int)
        = case Readable.read (readable, n) of
              SOME chunk => (f := BUFFERED { buffer = chunk, position = 0, next = ref (READABLE stream), initialPosition = NONE }; true)
            | NONE => false
    fun fillBufferWithAll (stream as { readable, ... } : { readable : Readable.readable, name : string }, f : instream)
        = let val chunk = Readable.readAll readable
              val next = ref (READABLE stream)
          in f := BUFFERED { buffer = chunk, position = 0, next = next, initialPosition = NONE }
           ; (chunk, next)
          end
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
                                   READABLE (s as { readable, name = _ }) => if fillBuffer (s, f, 1) then
                                                                                 input f
                                                                             else
                                                                                 (Word8Vector.fromList [], f)
                                 | PRIM_READER (rd as BinPrimIO.RD { chunkSize, ... }) => (fillBufferWithReader (rd, f, chunkSize); input f)
                                 | BUFFERED { buffer, position, next, initialPosition = _ } => (Word8VectorSlice.vector (Word8VectorSlice.slice (buffer, position, NONE)), next)
                                 | CLOSED _ => (Word8Vector.fromList [], f)
    fun newStreamWithBufferAndPosition (buffer, position, next, initialPosition)
        = if position >= Word8Vector.length buffer then
              next
          else
              ref (BUFFERED { buffer = buffer, position = position, next = next, initialPosition = initialPosition })
    fun input1 (f : instream) = case !f of
                                    READABLE (s as { readable, name = _ }) => if fillBuffer (s, f, 1) then
                                                                                  input1 f
                                                                              else
                                                                                  NONE
                                  | PRIM_READER (rd as BinPrimIO.RD { chunkSize, ... }) => (fillBufferWithReader (rd, f, chunkSize); input1 f)
                                  | BUFFERED { buffer, position, next, initialPosition } => SOME (Word8Vector.sub (buffer, position), newStreamWithBufferAndPosition (buffer, position + 1, next, initialPosition))
                                  | CLOSED _ => NONE
    fun inputN (f : instream, n) = case !f of
                                       READABLE (s as { readable, name = _ }) => if fillBuffer (s, f, n) then
                                                                                     inputN (f, n)
                                                                                 else
                                                                                     (Word8Vector.fromList [], f)
                                     | PRIM_READER rd => (fillBufferWithReader (rd, f, n); inputN (f, n))
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
    fun inputAll (f : instream) = case !f of
                                      READABLE (s as { readable, name = _ }) =>
                                      fillBufferWithAll (s, f)
                                    | _ =>
                                      let fun go (contentsRev, f) = let val (content, f) = input f
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
                                             READABLE { readable, name } => raise IO.Io { name = name, function = "canInput", cause = IO.NonblockingNotSupported }
                                           | PRIM_READER (BinPrimIO.RD { canInput = SOME canInput, ... }) => if canInput () then
                                                                                                                 SOME 1 (* Is this OK? *)
                                                                                                             else
                                                                                                                 NONE
                                           | PRIM_READER (BinPrimIO.RD { name, canInput = NONE, ... }) => raise IO.Io { name = name, function = "canInput", cause = IO.NonblockingNotSupported }
                                           | BUFFERED { buffer, position, next = _, initialPosition = _ } => SOME (Int.min (n, Word8Vector.length buffer - position))
                                           | CLOSED _ => SOME 0
    fun closeIn (f : instream) = case !f of
                                     READABLE { readable, name } => ( Readable.close readable (* TODO: release descriptor if there is one *)
                                                                    ; f := CLOSED name
                                                                    )
                                   | PRIM_READER (BinPrimIO.RD { name, close, ... }) => ( close (); f := CLOSED name )
                                   | BUFFERED { buffer = _, position = _, next, initialPosition = _ } => closeIn next
                                   | CLOSED _ => ()
    fun endOfStream (f : instream) = case !f of
                                         READABLE { readable, name = _ } => Readable.endOfStream readable
                                       | PRIM_READER (rd as BinPrimIO.RD { chunkSize, ... }) => (fillBufferWithReader (rd, f, chunkSize); endOfStream f)
                                       | BUFFERED { buffer = _, position = _, next = _, initialPosition = _ } => false
                                       | CLOSED _ => true
    end (* structure Instream *)
    structure Writable :> sig
                  type writable
                  type vector = Word8Vector.vector
                  val output : writable * string * vector -> unit
                  val flush : writable -> unit
                  val setBufferMode : writable * IO.buffer_mode -> unit
                  val closeOut : writable -> unit
                  val getWriter : writable * string -> BinPrimIO.writer
                  val fromValue : Lua.value -> writable
                  val toValue : writable -> Lua.value
              end = struct
    type writable = Lua.value
    type vector = Word8Vector.vector
    fun output (stream, name, s : vector)
        = let val result = Lua.method (stream, "write") #[Lua.unsafeToValue s]
          in if Lua.isFalsy (Vector.sub (result, 0)) then
                 let val message = Vector.sub (result, 1)
                 in raise IO.Io { name = name, function = "output", cause = Fail (Lua.unsafeFromValue message) }
                 end
             else
                 ()
          end
    fun flush stream = ignore (Lua.method (stream, "flush") #[])
    fun setBufferMode (stream, mode) = let val modeString = case mode of
                                                                IO.NO_BUF => "no"
                                                              | _ (* IO.BLOCK_BUF | IO.LINE_BUF *) => "full"
                                       in ignore (Lua.method (stream, "setvbuf") #[Lua.fromString modeString])
                                       end
    fun closeOut stream = ignore (Lua.method (stream, "close") #[])
    fun getWriter (stream : writable, name : string)
        = let val ioDesc = IODesc.toDesc stream
          in BinPrimIO.WR { name = name
                          , chunkSize = 1024 (* Should we see 'writableHighWaterMark' property? *)
                          , writeVec = SOME (fn slice => (output (stream, name, Word8VectorSlice.vector slice); flush stream; Word8VectorSlice.length slice))
                          , writeArr = NONE (* TODO *)
                          , writeVecNB = NONE (* TODO *)
                          , writeArrNB = NONE (* TODO *)
                          , block = NONE
                          , canOutput = NONE
                          , getPos = NONE
                          , setPos = NONE
                          , endPos = NONE
                          , verifyPos = NONE
                          , close = fn () => (IODesc.release ioDesc; closeOut stream)
                          , ioDesc = SOME ioDesc
                          }
          end
    fun fromValue (stream : Lua.value) = stream
    val toValue = fromValue
    end (* structure Writable *)
    structure Outstream : sig
                  (* outstream-related part of TEXT_STREAM_IO *)
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
                  val fromWritable : Writable.writable * IO.buffer_mode * string -> outstream
              end = struct
    type elem = Word8.word
    type vector = Word8Vector.vector
    type writer = BinPrimIO.writer
    datatype outstream = LUA_WRITABLE of { writable : Writable.writable, buffer_mode : IO.buffer_mode ref, name : string }
                       | PRIM_WRITER of { writer : writer
                                        , buffer_mode : IO.buffer_mode ref
                                        , buffer : vector list ref (* the last-written item is the first in the list *)
                                        }
    type pos = BinPrimIO.pos
    type out_pos = { writer : writer
                   , buffer_mode : IO.buffer_mode ref
                   , buffer : vector list ref (* the last-written item is the first in the list *)
                   , pos : pos
                   }
    fun fromWritable (w, mode, name) = LUA_WRITABLE { writable = w, buffer_mode = ref mode, name = name }
    fun output (LUA_WRITABLE { writable, buffer_mode = _, name }, content)
        = Writable.output (writable, name, content)
      | output (PRIM_WRITER { writer = BinPrimIO.WR { name, chunkSize, writeVec, ... }, buffer_mode, buffer }, content)
        = case !buffer_mode of
              IO.NO_BUF => (case writeVec of
                                SOME writeVec => ignore (writeVec (Word8VectorSlice.full content)) (* TODO: should we retry if partially written? *)
                              | NONE => raise IO.Io { name = name, function = "output", cause = IO.BlockingNotSupported }
                           )
            | _ (* IO.BLOCK_BUF | IO.LINE_BUF *) =>
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
    fun outputAndFlush (LUA_WRITABLE { writable, buffer_mode, name }, content) = (Writable.output (writable, name, content); Writable.flush writable)
      | outputAndFlush (PRIM_WRITER { writer = BinPrimIO.WR { name, chunkSize, writeVec, ... }, buffer_mode, buffer }, content)
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
    fun flushOut (LUA_WRITABLE { writable, ... }) = Writable.flush writable
      | flushOut (PRIM_WRITER { writer = BinPrimIO.WR { name, writeVec, ... }, buffer_mode = _, buffer })
        = flushOutPrim (name, writeVec, buffer)
    fun closeOut (LUA_WRITABLE { writable, ... }) = Writable.closeOut writable
      | closeOut (PRIM_WRITER { writer = BinPrimIO.WR { name, writeVec, close, ... }, buffer_mode = _, buffer })
        = ( flushOutPrim (name, writeVec, buffer)
          ; close ()
          ) (* TODO: Make idempotent *)
    fun setBufferMode (LUA_WRITABLE { writable, buffer_mode, name = _ }, mode)
        = ( Writable.setBufferMode (writable, mode)
          ; buffer_mode := mode
          )
      | setBufferMode (PRIM_WRITER { writer = BinPrimIO.WR { name, writeVec, ... }, buffer_mode, buffer }, mode)
        = ( if mode = IO.NO_BUF then
                flushOutPrim (name, writeVec, buffer)
            else
                ()
          ; buffer_mode := mode
          )
    fun getBufferMode (LUA_WRITABLE { buffer_mode, ... }) = !buffer_mode
      | getBufferMode (PRIM_WRITER { buffer_mode, ... }) = !buffer_mode
    fun mkOutstream (BinPrimIO.WR { name, ioDesc = SOME ioDesc, ... }, mode)
        = let val writable = Writable.fromValue (IODesc.fromDesc ioDesc) (* TODO: type check? *)
          in LUA_WRITABLE { writable = writable, buffer_mode = ref mode, name = name }
          end
      | mkOutstream (w, mode) = PRIM_WRITER { writer = w, buffer_mode = ref mode, buffer = ref [] }
    fun getWriter (LUA_WRITABLE { writable, buffer_mode, name })
        = (Writable.flush writable; (Writable.getWriter (writable, name), !buffer_mode))
      | getWriter (PRIM_WRITER { writer as BinPrimIO.WR { name, writeVec, ... }, buffer_mode, buffer })
        = ( flushOutPrim (name, writeVec, buffer)
          (* TODO: Mark the stream as terminated *)
          ; (writer, !buffer_mode)
          )
    fun getPosOut (LUA_WRITABLE { name, ... }) = raise IO.Io { name = name, function = "getPosOut", cause = IO.RandomAccessNotSupported } (* TODO: Use stream:seek() *)
      | getPosOut (PRIM_WRITER { writer as BinPrimIO.WR { name, writeVec, getPos = getPos, ... }, buffer_mode, buffer })
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
fun openIn path = let val (r0, message) = Lua.call2 Lua.Lib.io.open' #[Lua.fromString path, Lua.fromString "rb"]
                  in if Lua.isNil r0 then
                         raise IO.Io { name = path, function = "BinIO.openIn", cause = Fail (Lua.unsafeFromValue message) } (* TODO: cause *)
                     else
                         mkInstream (Instream.openReadable (Readable.fromValue r0, path))
                  end
fun openOut path = let val (r0, message) = Lua.call2 Lua.Lib.io.open' #[Lua.fromString path, Lua.fromString "wb"]
                   in if Lua.isNil r0 then
                          raise IO.Io { name = path, function = "BinIO.openOut", cause = Fail (Lua.unsafeFromValue message) } (* TODO: cause *)
                      else
                          mkOutstream (Outstream.fromWritable (Writable.fromValue r0, IO.BLOCK_BUF, path))
                   end
fun openAppend path = let val (r0, message) = Lua.call2 Lua.Lib.io.open' #[Lua.fromString path, Lua.fromString "ab"]
                      in if Lua.isNil r0 then
                             raise IO.Io { name = path, function = "BinIO.openAppend", cause = Fail (Lua.unsafeFromValue message) } (* TODO: cause *)
                         else
                             mkOutstream (Outstream.fromWritable (Writable.fromValue r0, IO.BLOCK_BUF, path))
                      end
end (* local *)
end (* structure BinIO *)
end; (* local *)
