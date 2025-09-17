local
    structure DelimCont = LunarML.DelimCont
    structure Readable :> sig
              type vector = string (* Buffer or Uint8Array *)
              type readable (* Readable / paused mode / Buffer *)
              val readSome : readable -> vector list
              val rawEnded : readable -> bool
              val destroy : readable -> unit
              val getReader : readable * string -> TextPrimIO.reader
              val fromValue : JavaScript.value -> readable
              val toValue : readable -> JavaScript.value
              end = struct
    type vector = string
    type readable = JavaScript.value
    fun readSome (stream : readable) : vector list
        = DelimCont.withSubCont
              (DelimCont.topLevel, fn cont : (vector list, unit) DelimCont.subcont =>
                                      let val handled = ref false
                                          val readableHandler = JavaScript.callback (fn _ =>
                                                                                        let fun doRead () : string list
                                                                                                = let val chunk = JavaScript.method (stream, "read") #[]
                                                                                                  in if JavaScript.=== (chunk, JavaScript.null) then
                                                                                                         nil
                                                                                                     else
                                                                                                         let val chunk : string = JavaScript.unsafeFromValue chunk (* Buffer as string *)
                                                                                                         in if chunk = "" then (* can this happen? *)
                                                                                                                doRead ()
                                                                                                            else
                                                                                                                [chunk]
                                                                                                         end
                                                                                                  end
                                                                                            val chunks = doRead ()
                                                                                            val () = handled := true
                                                                                        in DelimCont.pushSubCont (cont, fn () => chunks)
                                                                                        end
                                                                                    )
                                          val endHandler = JavaScript.callback (fn _ =>
                                                                                   if not (!handled) then
                                                                                       ( handled := true
                                                                                       ; DelimCont.pushSubCont (cont, fn () => [])
                                                                                       )
                                                                                   else
                                                                                       ()
                                                                               )
                                      in JavaScript.method (stream, "once") #[JavaScript.fromWideString "readable", readableHandler]
                                       ; JavaScript.method (stream, "once") #[JavaScript.fromWideString "end", endHandler]
                                       ; JavaScript.method (stream, "read") #[JavaScript.fromInt 0] (* This seems to be needed to trigger 'readadble'/'end' for interactive stdin *)
                                       ; ()
                                      end
              )
    fun readVec (stream : readable) (n : int) : vector
        = DelimCont.withSubCont
              (DelimCont.topLevel, fn cont : (vector, unit) DelimCont.subcont =>
                                      let val handled = ref false
                                          val readableHandler = JavaScript.callback (fn _ =>
                                                                                        let val chunk : string
                                                                                                = let val chunk = JavaScript.method (stream, "read") #[JavaScript.fromInt n]
                                                                                                  in if JavaScript.=== (chunk, JavaScript.null) then
                                                                                                         ""
                                                                                                     else
                                                                                                         JavaScript.unsafeFromValue chunk (* Buffer as string *)
                                                                                                  end
                                                                                            val () = handled := true
                                                                                        in DelimCont.pushSubCont (cont, fn () => chunk)
                                                                                        end
                                                                                    )
                                          val endHandler = JavaScript.callback (fn _ =>
                                                                                   if not (!handled) then
                                                                                       ( handled := true
                                                                                       ; DelimCont.pushSubCont (cont, fn () => "")
                                                                                       )
                                                                                   else
                                                                                       ()
                                                                               )
                                      in JavaScript.method (stream, "once") #[JavaScript.fromWideString "readable", readableHandler]
                                       ; JavaScript.method (stream, "once") #[JavaScript.fromWideString "end", endHandler]
                                       ; JavaScript.method (stream, "read") #[JavaScript.fromInt 0] (* This seems to be needed to trigger 'readadble'/'end' for interactive stdin *)
                                       ; ()
                                      end
              )
    fun rawEnded (stream : readable) : bool = JavaScript.unsafeFromValue (JavaScript.field (stream, "readableEnded"))
    fun destroy (stream : readable) : unit = (JavaScript.method (stream, "destroy") #[]; ())
    fun getReader (stream : readable, name : string)
        = let val ioDesc = IODesc.toDesc stream
              (* TODO: Should we register 'close' event handler to release the descriptor? *)
          in TextPrimIO.RD { name = name
                           , chunkSize = 1024 (* tentative *)
                           , readVec = SOME (readVec stream)
                           , readArr = NONE
                           , readVecNB = NONE
                           , readArrNB = NONE
                           , block = NONE
                           , canInput = NONE
                           , avail = fn () => NONE
                           , getPos = NONE
                           , setPos = NONE
                           , endPos = NONE
                           , verifyPos = NONE
                           , close = fn () => (IODesc.release ioDesc; destroy stream)
                           , ioDesc = SOME ioDesc
                           }
          end
    fun fromValue (stream : JavaScript.value) = stream
    val toValue = fromValue
    end;
    structure Instream : sig
                  (* instream-related part of TEXT_STREAM_IO *)
                  type reader
                  type instream
                  type elem = char
                  type vector = string
                  type pos = TextPrimIO.pos
                  val input : instream -> vector * instream
                  val input1 : instream -> (elem * instream) option
                  val inputN : instream * int -> vector * instream
                  val inputLine : instream -> (string * instream) option
                  val inputAll : instream -> vector * instream
                  val canInput : instream * int -> int option
                  val closeIn : instream -> unit
                  val endOfStream : instream -> bool
                  val mkInstream : reader * vector -> instream
                  val getReader : instream -> reader * vector
                  val filePosIn : instream -> pos
                  val openVector : vector -> instream
                  val openReadable : JavaScript.value * string -> instream
              end = struct
    type elem = char
    type vector = string
    type reader = TextPrimIO.reader
    datatype state = READABLE of { readable : Readable.readable, name : string } (* Readable / paused mode / Buffer *)
                   | PRIM_READER of reader
                   | BUFFERED of { buffer : vector, position : int, next : instream, initialPosition : TextPrimIO.pos option } (* invariant: 0 <= position < String.size buffer *) (* name? *)
                   | CLOSED of string (* name *)
    withtype instream = state ref
    type pos = TextPrimIO.pos
    (*: val getPosOpt : TextPrimIO.reader -> TextPrimIO.pos option *)
    fun getPosOpt (TextPrimIO.RD { getPos = SOME getPos, ... }) = SOME (getPos ())
      | getPosOpt _ = NONE
    fun getName ins = case !ins of
                          READABLE { name, ... } => name
                        | PRIM_READER (TextPrimIO.RD { name, ... }) => name
                        | BUFFERED { next, ... } => getName next
                        | CLOSED name => name
    fun openVector content = ref (BUFFERED { buffer = content, position = 0, next = ref (CLOSED "<vector>"), initialPosition = NONE })
    fun openReadable (stream, name) = ref (READABLE { readable = Readable.fromValue stream, name = name })
    fun mkInstream (rd, content) = let val tip = case rd of
                                                     TextPrimIO.RD { name, ioDesc = SOME ioDesc, ... } => ref (READABLE { readable = Readable.fromValue (IODesc.fromDesc ioDesc), name = name })
                                                   | _ => ref (PRIM_READER rd)
                                   in if CharVector.length content > 0 then
                                          ref (BUFFERED { buffer = content, position = 0, next = tip, initialPosition = NONE })
                                      else
                                          tip
                                   end
    local
        fun doGetReader (ins, acc) = case !ins of
                                         READABLE { readable, name } => (ins := CLOSED name; (Readable.getReader (readable, name), CharVectorSlice.concat (List.rev acc)))
                                       | PRIM_READER (rd as TextPrimIO.RD { name, ... }) => (ins := CLOSED name; (rd, CharVectorSlice.concat (List.rev acc)))
                                       | BUFFERED { buffer, position, next, initialPosition = _ } => doGetReader (next, CharVectorSlice.slice (buffer, position, NONE) :: acc)
                                       | CLOSED name => raise IO.Io { name = name, function = "getReader", cause = IO.ClosedStream }
    in
    fun getReader ins = doGetReader (ins, [])
    end
    fun filePosIn ins = case !ins of
                            READABLE { name, ... } => raise IO.Io { name = name, function = "filePosIn", cause = IO.RandomAccessNotSupported }
                          | PRIM_READER (TextPrimIO.RD { name, getPos = NONE, ... }) => raise IO.Io { name = name, function = "filePosIn", cause = IO.RandomAccessNotSupported }
                          | PRIM_READER (TextPrimIO.RD { getPos = SOME getPos, ... }) => getPos ()
                          | BUFFERED { buffer, position, next, initialPosition = NONE } => raise IO.Io { name = getName next, function = "filePosIn", cause = IO.RandomAccessNotSupported }
                          | BUFFERED { buffer = _, position, next, initialPosition = SOME initialPosition } =>
                            let fun go (PRIM_READER (TextPrimIO.RD { getPos = SOME getPos, setPos = SOME setPos, readVec = SOME readVec, ... }))
                                    = let val savedPosition = getPos ()
                                      in setPos initialPosition
                                       ; readVec position
                                       ; getPos () before setPos savedPosition
                                      end
                                  | go (BUFFERED { next, ... }) = go (!next)
                                  | go (CLOSED name) = raise IO.Io { name = name, function = "filePosIn", cause = IO.ClosedStream }
                                  | go (PRIM_READER (TextPrimIO.RD { name, ... })) = raise IO.Io { name = name, function = "filePosIn", cause = IO.RandomAccessNotSupported }
                                  | go (READABLE { name, ... }) = raise IO.Io { name = name, function = "filePosIn", cause = IO.RandomAccessNotSupported }
                            in go (!next)
                            end
                          | CLOSED name => raise IO.Io { name = name, function = "filePosIn", cause = IO.ClosedStream }
    fun fillBuffer (stream as { readable, ... } : { readable : Readable.readable, name : string }, f : instream)
        = let val chunks = Readable.readSome readable
          in f := List.foldr (fn (chunk, rest) => BUFFERED { buffer = chunk, position = 0, next = ref rest, initialPosition = NONE }) (READABLE stream) chunks
          end
    fun fillBufferWithReader (rd as TextPrimIO.RD { name, readVec, readArr, readVecNB, readArrNB, block, ... } : reader, f : instream, n)
        = let val initialPosition = getPosOpt rd
              val chunk = case readVec of
                              SOME rv => rv n
                            | NONE => case readArr of
                                          SOME ra => let val arr = CharArray.array (n, #"\000")
                                                         val actual = ra (CharArraySlice.full arr)
                                                     in CharArraySlice.vector (CharArraySlice.slice (arr, 0, SOME actual))
                                                     end
                                        | NONE => case (block, readVecNB, readArrNB) of
                                                      (SOME block', SOME rvNB, _) =>
                                                      (case rvNB n of
                                                           SOME content => content
                                                         | NONE => ( block' ()
                                                                   ; case rvNB n of
                                                                         SOME content => content
                                                                       | NONE => "" (* should not occur *)
                                                                   )
                                                      )
                                                    | (SOME block', NONE, SOME raNB) =>
                                                      let val arr = CharArray.array (n, #"\000")
                                                          val aslice = CharArraySlice.full arr
                                                      in case raNB aslice of
                                                             SOME actual => CharArraySlice.vector (CharArraySlice.slice (arr, 0, SOME actual))
                                                           | NONE => ( block' ()
                                                                     ; case raNB aslice of
                                                                           SOME actual => CharArraySlice.vector (CharArraySlice.slice (arr, 0, SOME actual))
                                                                         | NONE => "" (* should not occur *)
                                                                     )
                                                      end
                                                    | _ => raise IO.Io { name = name, function = "<unknown>", cause = IO.BlockingNotSupported }
          in f := BUFFERED { buffer = chunk, position = 0, next = ref (PRIM_READER rd), initialPosition = initialPosition }
          end
    fun input (f : instream) = case !f of
                                   READABLE (s as { readable, name = _ }) => if Readable.rawEnded readable then
                                                                                 ("", f)
                                                                             else
                                                                                 (fillBuffer (s, f); input f)
                                 | PRIM_READER (rd as TextPrimIO.RD { chunkSize, ... }) => (fillBufferWithReader (rd, f, chunkSize); input f)
                                 | BUFFERED { buffer, position, next, initialPosition = _ } => (String.extract (buffer, position, NONE), next)
                                 | CLOSED _ => ("", f)
    fun newStreamWithBufferAndPosition (buffer, position, next, initialPosition)
        = if position >= String.size buffer then
              next
          else
              ref (BUFFERED { buffer = buffer, position = position, next = next, initialPosition = initialPosition })
    fun input1 (f : instream) = case !f of
                                    READABLE (s as { readable, name = _ }) => if Readable.rawEnded readable then
                                                                                  NONE
                                                                              else
                                                                                  (fillBuffer (s, f); input1 f)
                                  | PRIM_READER (rd as TextPrimIO.RD { chunkSize, ... }) => (fillBufferWithReader (rd, f, chunkSize); input1 f)
                                  | BUFFERED { buffer, position, next, initialPosition } => SOME (String.sub (buffer, position), newStreamWithBufferAndPosition (buffer, position + 1, next, initialPosition))
                                  | CLOSED _ => NONE
    fun inputN (f : instream, n) = case !f of
                                       READABLE (s as { readable, name = _ }) => if Readable.rawEnded readable then
                                                                                     ("", f)
                                                                                 else
                                                                                     (fillBuffer (s, f); inputN (f, n))
                                     | PRIM_READER rd => (fillBufferWithReader (rd, f, n); inputN (f, n))
                                     | BUFFERED { buffer, position, next, initialPosition } =>
                                       let val newPosition = position + n
                                       in if newPosition <= String.size buffer then
                                              (String.substring (buffer, position, n), newStreamWithBufferAndPosition (buffer, newPosition, next, initialPosition))
                                          else
                                              let val buffer0 = String.extract (buffer, position, NONE)
                                                  val (buffer1, next) = inputN (next, n - String.size buffer0)
                                              in (buffer0 ^ buffer1, next)
                                              end
                                       end
                                     | CLOSED _ => ("", f)
    fun inputLine (f : instream) = case !f of
                                       READABLE (s as { readable, name = _ }) => if Readable.rawEnded readable then
                                                                                     NONE
                                                                                 else
                                                                                     (fillBuffer (s, f); inputLine f)
                                     | PRIM_READER (rd as TextPrimIO.RD { chunkSize, ... }) => (fillBufferWithReader (rd, f, chunkSize); inputLine f)
                                     | BUFFERED { buffer, position, next, initialPosition } =>
                                       let fun findNewline i = if i >= String.size buffer then
                                                                   case inputLine next of
                                                                       NONE => SOME (String.extract (buffer, position, NONE) ^ "\n", next)
                                                                     | SOME (line, next) => SOME (String.extract (buffer, position, NONE) ^ line, next)
                                                               else if String.sub (buffer, i) = #"\n" then
                                                                   let val newPosition = i + 1
                                                                   in SOME (String.substring (buffer, position, newPosition - position), newStreamWithBufferAndPosition (buffer, newPosition, next, initialPosition))
                                                                   end
                                                               else
                                                                   findNewline (i + 1)
                                       in findNewline position
                                       end
                                     | CLOSED _ => NONE
    fun inputAll (f : instream) = let fun go (contentsRev, f) = case input f of
                                                                    ("", f) => (String.concat (List.rev contentsRev), f)
                                                                  | (content, f) => go (content :: contentsRev, f)
                                  in go ([], f) (* Use PrimIO/avail? *)
                                  end
    fun canInput (f : instream, n) = if n < 0 then
                                         raise Size
                                     else
                                         case !f of
                                             READABLE { readable, name = _ } => if Readable.rawEnded readable then
                                                                                    SOME 0
                                                                                else
                                                                                    NONE
                                           | PRIM_READER (TextPrimIO.RD { canInput = SOME canInput, ... }) => if canInput () then
                                                                                                                  SOME 1 (* Is this OK? *)
                                                                                                              else
                                                                                                                  NONE
                                           | PRIM_READER (TextPrimIO.RD { name, canInput = NONE, ... }) => raise IO.Io { name = name, function = "canInput", cause = IO.NonblockingNotSupported }
                                           | BUFFERED { buffer, position, next = _, initialPosition = _ } => SOME (Int.min (n, String.size buffer - position))
                                           | CLOSED _ => SOME 0
    fun closeIn (f : instream) = case !f of
                                     READABLE { readable, name } => ( Readable.destroy readable (* TODO: release descriptor if there is one *)
                                                                    ; f := CLOSED name
                                                                    )
                                   | PRIM_READER (TextPrimIO.RD { name, close, ... }) => ( close (); f := CLOSED name )
                                   | BUFFERED { buffer = _, position = _, next, initialPosition = _ } => closeIn next
                                   | CLOSED _ => ()
    fun endOfStream (f : instream) = case !f of
                                         READABLE { readable, name = _ } => Readable.rawEnded readable
                                       | PRIM_READER (rd as TextPrimIO.RD { chunkSize, ... }) => (fillBufferWithReader (rd, f, chunkSize); endOfStream f)
                                       | BUFFERED { buffer = _, position = _, next = _, initialPosition = _ } => false
                                       | CLOSED _ => true
    end (* structure Instream *)
    structure Writable :> sig
                  type writable
                  type vector = string
                  val output : writable * vector -> unit
                  val outputAndFlush : writable * vector -> unit
                  val closeOut : writable -> unit
                  val getWriter : writable * string -> TextPrimIO.writer
                  val fromValue : JavaScript.value -> writable
                  val toValue : writable -> JavaScript.value
              end = struct
    type writable = JavaScript.value
    type vector = string
    fun output (stream, chunk) = let val result = JavaScript.method (stream, "write") #[JavaScript.unsafeToValue chunk (* as Uint8Array *)]
                                 in if JavaScript.isFalsy result then
                                        DelimCont.withSubCont (DelimCont.topLevel, fn cont : (unit, unit) DelimCont.subcont =>
                                                                                      let val onDrain = JavaScript.callback (fn _ => DelimCont.pushSubCont (cont, fn () => ()))
                                                                                      in JavaScript.method (stream, "once") #[JavaScript.fromWideString "drain", onDrain]
                                                                                       ; ()
                                                                                      end
                                                              )
                                    else
                                        ()
                                 end
    fun outputAndFlush (stream, chunk) = DelimCont.withSubCont (DelimCont.topLevel, fn cont : (unit, unit) DelimCont.subcont =>
                                                                                       let val callback = JavaScript.callback (fn _ => DelimCont.pushSubCont (cont, fn () => ()))
                                                                                       in JavaScript.method (stream, "write") #[JavaScript.unsafeToValue chunk (* as Uint8Array *), JavaScript.null, callback]
                                                                                        ; ()
                                                                                       end
                                                               )
    fun closeOut stream = DelimCont.withSubCont (DelimCont.topLevel, fn cont : (unit, unit) DelimCont.subcont =>
                                                                        let val callback = JavaScript.callback (fn _ => DelimCont.pushSubCont (cont, fn () => ()))
                                                                        in ignore (JavaScript.method (stream, "end") #[JavaScript.unsafeToValue callback])
                                                                        end
                                                )
    fun getWriter (stream : writable, name : string)
        = let val ioDesc = IODesc.toDesc stream
              (* TODO: Should we register 'close' event handler to release the descriptor? *)
          in TextPrimIO.WR { name = name
                           , chunkSize = 1024 (* Should we see 'writableHighWaterMark' property? *)
                           , writeVec = SOME (fn slice => (outputAndFlush (stream, CharVectorSlice.vector slice); CharVectorSlice.length slice)) (* TODO: Avoid copying *)
                           , writeArr = NONE (* TODO *)
                           , writeVecNB = NONE (* TODO *)
                           , writeArrNB = NONE (* TODO *)
                           , block = NONE
                           , canOutput = NONE (* 'writableNeedDrain' property? *)
                           , getPos = NONE
                           , setPos = NONE
                           , endPos = NONE
                           , verifyPos = NONE
                           , close = fn () => (IODesc.release ioDesc; closeOut stream)
                           , ioDesc = SOME ioDesc
                           }
          end
    fun fromValue (stream : JavaScript.value) = stream
    val toValue = fromValue
    end (* structure Writable *)
    structure Outstream : sig
                  (* outstream-related part of TEXT_STREAM_IO *)
                  type elem = char
                  type vector = string
                  type outstream
                  type out_pos
                  type pos = TextPrimIO.pos
                  type writer = TextPrimIO.writer
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
                  val outputSubstr : outstream * substring -> unit
                  val outputAndFlush : outstream * vector -> unit (* for implementing print *)
                  val fromWritable : Writable.writable * IO.buffer_mode * string -> outstream
              end = struct
    type elem = char
    type vector = string
    type writer = TextPrimIO.writer
    datatype outstream = JS_WRITABLE of { writable : Writable.writable, buffer_mode : IO.buffer_mode ref, name : string }
                       | PRIM_WRITER of { writer : writer
                                        , buffer_mode : IO.buffer_mode ref
                                        , buffer : vector list ref (* the last-written item is the first in the list *)
                                        }
    type pos = TextPrimIO.pos
    type out_pos = { writer : writer
                   , buffer_mode : IO.buffer_mode ref
                   , buffer : vector list ref (* the last-written item is the first in the list *)
                   , pos : pos
                   }
    fun fromWritable (w, mode, name) = JS_WRITABLE { writable = w, buffer_mode = ref mode, name = name }
    fun output (JS_WRITABLE { writable, buffer_mode, name = _ }, content)
        = (case !buffer_mode of
               IO.NO_BUF => Writable.outputAndFlush (writable, content)
             | IO.LINE_BUF => if CharVector.exists (fn c => c = #"\n") content then
                                  Writable.outputAndFlush (writable, content)
                              else
                                  Writable.output (writable, content)
             | IO.BLOCK_BUF => Writable.output (writable, content)
          )
      | output (PRIM_WRITER { writer = TextPrimIO.WR { name, chunkSize, writeVec, ... }, buffer_mode, buffer }, content)
        = case !buffer_mode of
              IO.NO_BUF => (case writeVec of
                                SOME writeVec => ignore (writeVec (CharVectorSlice.full content)) (* TODO: should we retry if partially written? *)
                              | NONE => raise IO.Io { name = name, function = "output", cause = IO.BlockingNotSupported }
                           )
            | IO.LINE_BUF => if CharVector.exists (fn c => c = #"\n") content then
                                 let val content' = String.concat (List.rev (content :: !buffer))
                                 in case writeVec of
                                        SOME writeVec => ignore (writeVec (CharVectorSlice.full content')) (* TODO: should we retry if partially written? *)
                                      | NONE => raise IO.Io { name = name, function = "output", cause = IO.BlockingNotSupported }
                                  ; buffer := []
                                 end
                             else
                                 buffer := content :: !buffer
            | IO.BLOCK_BUF => let val b = !buffer
                                  val bufSize = List.foldl (fn (z, acc) => acc + String.size z) (String.size content) b
                              in if bufSize >= chunkSize then
                                     let val content' = String.concat (List.rev (content :: !buffer))
                                     in case writeVec of
                                            SOME writeVec => ignore (writeVec (CharVectorSlice.full content')) (* TODO: should we retry if partially written? *)
                                          | NONE => raise IO.Io { name = name, function = "output", cause = IO.BlockingNotSupported }
                                      ; buffer := []
                                     end
                                 else
                                     buffer := content :: !buffer
                              end
    fun output1 (stream, elem) = output (stream, String.str elem)
    fun outputSubstr (stream, substring) = output (stream, Substring.string substring)
    fun outputAndFlush (JS_WRITABLE { writable, buffer_mode, name = _ }, content) = Writable.outputAndFlush (writable, content)
      | outputAndFlush (PRIM_WRITER { writer = TextPrimIO.WR { name, chunkSize, writeVec, ... }, buffer_mode, buffer }, content)
        = let val content' = String.concat (List.rev (content :: !buffer))
          in case writeVec of
                 SOME writeVec => ignore (writeVec (CharVectorSlice.full content')) (* TODO: should we retry if partially written? *)
               | NONE => raise IO.Io { name = name, function = "output", cause = IO.BlockingNotSupported }
           ; buffer := []
          end
    fun flushOutPrim (name, writeVec, buffer)
        = let val content = String.concat (List.rev (!buffer))
              val () = buffer := []
          in if content <> "" then
                 case writeVec of
                     SOME writeVec => ignore (writeVec (CharVectorSlice.full content)) (* TODO: should we retry if partially written? *)
                   | NONE => raise IO.Io { name = name, function = "flushOut", cause = IO.BlockingNotSupported }
             else
                 ()
          end
    fun flushOut (JS_WRITABLE { writable, ... }) = Writable.outputAndFlush (writable, "")
      | flushOut (PRIM_WRITER { writer = TextPrimIO.WR { name, writeVec, ... }, buffer_mode = _, buffer })
        = flushOutPrim (name, writeVec, buffer)
    fun closeOut (JS_WRITABLE { writable, ... }) = Writable.closeOut writable (* TODO: See 'writableEnded'? *)
      | closeOut (PRIM_WRITER { writer = TextPrimIO.WR { name, writeVec, close, ... }, buffer_mode = _, buffer })
        = ( flushOutPrim (name, writeVec, buffer)
          ; close ()
          ) (* TODO: Make idempotent *)
    fun setBufferMode (JS_WRITABLE { writable, buffer_mode, name = _ }, mode)
        = ( if mode = IO.NO_BUF then
                Writable.outputAndFlush (writable, "")
            else
                ()
          ; buffer_mode := mode
          )
      | setBufferMode (PRIM_WRITER { writer = TextPrimIO.WR { name, writeVec, ... }, buffer_mode, buffer }, mode)
        = ( if mode = IO.NO_BUF then
                flushOutPrim (name, writeVec, buffer)
            else
                ()
          ; buffer_mode := mode
          )
    fun getBufferMode (JS_WRITABLE { buffer_mode, ... }) = !buffer_mode
      | getBufferMode (PRIM_WRITER { buffer_mode, ... }) = !buffer_mode
    fun mkOutstream (TextPrimIO.WR { name, ioDesc = SOME ioDesc, ... }, mode)
        = let val writable = Writable.fromValue (IODesc.fromDesc ioDesc) (* TODO: type check? *)
          in JS_WRITABLE { writable = writable, buffer_mode = ref mode, name = name }
          end
      | mkOutstream (w, mode) = PRIM_WRITER { writer = w, buffer_mode = ref mode, buffer = ref [] }
    fun getWriter (JS_WRITABLE { writable, buffer_mode, name })
        = (Writable.outputAndFlush (writable, ""); (Writable.getWriter (writable, name), !buffer_mode))
      | getWriter (PRIM_WRITER { writer as TextPrimIO.WR { name, writeVec, ... }, buffer_mode, buffer })
        = ( flushOutPrim (name, writeVec, buffer)
                         (* TODO: Mark the stream as terminated *)
          ; (writer, !buffer_mode)
          )
    fun getPosOut (JS_WRITABLE { name, ... }) = raise IO.Io { name = name, function = "getPosOut", cause = IO.RandomAccessNotSupported }
      | getPosOut (PRIM_WRITER { writer as TextPrimIO.WR { name, writeVec, getPos = getPos, ... }, buffer_mode, buffer })
        = case getPos of
              SOME getPos => ( flushOutPrim (name, writeVec, buffer)
                             ; { writer = writer, buffer_mode = buffer_mode, buffer = buffer, pos = getPos () }
                             )
            | NONE => raise IO.Io { name = name, function = "getPosOut", cause = IO.RandomAccessNotSupported }
    fun setPosOut { writer as TextPrimIO.WR { name, setPos, ... }, buffer_mode, buffer, pos }
        = case setPos of
              SOME setPos => ( setPos pos
                             ; PRIM_WRITER { writer = writer, buffer_mode = buffer_mode, buffer = buffer }
                             )
            | NONE => raise IO.Io { name = name, function = "setPosOut", cause = IO.RandomAccessNotSupported }
    fun filePosOut ({ pos, ... } : out_pos) = pos
    end (* structure Outstream *)
in
structure TextIO :> TEXT_IO = struct
local
    _esImport [pure] { stdin, stdout, stderr } from "node:process";
    _esImport [pure] { createReadStream, createWriteStream } from "node:fs";
    structure StreamIOImpl = struct
    open Instream
    open Outstream
    end
    structure Base = ImperativeIO (structure StreamIO = StreamIOImpl
                                   structure Vector = CharVector
                                   structure Array = CharArray
                                  )
in
open Base
structure StreamIO = StreamIOImpl
fun inputLine stream = case StreamIO.inputLine (getInstream stream) of
                           NONE => NONE
                         | SOME (line, stream') => ( setInstream (stream, stream')
                                                   ; SOME line
                                                   )
fun openIn path = mkInstream (StreamIO.openReadable (JavaScript.call createReadStream #[JavaScript.unsafeToValue path (* as Buffer? *)], path))
fun openString content = mkInstream (StreamIO.openVector content)
val stdIn = LunarML.assumeDiscardable (fn () =>
  let val stdIn = StreamIO.openReadable (stdin, "<stdin>")
  in mkInstream stdIn
  end) ()
fun scanStream scan ins = case scan StreamIO.input1 (getInstream ins) of
                              NONE => NONE
                            | SOME (x, ins') => ( setInstream (ins, ins')
                                                ; SOME x
                                                )
fun outputSubstr (outs, s) = StreamIO.outputSubstr (getOutstream outs, s)
fun openOut path = let val writable = JavaScript.call createWriteStream #[JavaScript.unsafeToValue path (* as Buffer? *)]
                   in mkOutstream (Outstream.fromWritable (Writable.fromValue writable, IO.BLOCK_BUF, path))
                   end
fun openAppend path = let val options = JavaScript.newObject ()
                          val () = JavaScript.set (options, JavaScript.fromWideString "flags", JavaScript.fromWideString "a")
                          val writable = JavaScript.call createWriteStream #[JavaScript.unsafeToValue path (* as Buffer? *), options]
                      in mkOutstream (Outstream.fromWritable (Writable.fromValue writable, IO.BLOCK_BUF, path))
                      end
val stdOut = LunarML.assumeDiscardable (fn () =>
  let val stdOut = Outstream.fromWritable (Writable.fromValue stdout, IO.BLOCK_BUF, "<stdout>")
  in mkOutstream stdOut
  end) ()
val stdErr = LunarML.assumeDiscardable (fn () =>
  let val stdErr = Outstream.fromWritable (Writable.fromValue stderr, IO.NO_BUF, "<stderr>")
  in mkOutstream stdErr
  end) ()
fun print s = Outstream.outputAndFlush (getOutstream stdOut, s)
end (* local *)
end (* structure TextIO *)
end; (* local *)
val print = TextIO.print;
