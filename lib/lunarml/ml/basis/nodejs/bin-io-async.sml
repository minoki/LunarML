local
    structure DelimCont = LunarML.DelimCont
    structure Instream : sig
                  (* instream-related part of STREAM_IO *)
                  type instream
                  type elem = Word8.word
                  type vector = Word8Vector.vector
                  val input : instream -> vector * instream
                  val input1 : instream -> (elem * instream) option
                  val inputN : instream * int -> vector * instream
                  val inputAll : instream -> vector * instream
                  val canInput : instream * int -> int option
                  val closeIn : instream -> unit
                  val endOfStream : instream -> bool
                  (* mkInstream, getReader, filePosIn *)
                  val openReadable : JavaScript.value -> instream
              end = struct
    type elem = Word8.word
    type vector = Word8Vector.vector
    datatype state = TIP of JavaScript.value (* Readable / paused mode / Buffer *)
                   | BUFFERED of { buffer : vector, position : int, next : instream } (* invariant: 0 <= position < String.size buffer *)
                   | CLOSED
    withtype instream = state ref
    fun openReadable stream = ref (TIP stream)
    fun rawRead (stream : JavaScript.value) : vector list
        = DelimCont.withSubCont
              (DelimCont.topLevel, fn cont : (vector list, unit) DelimCont.subcont =>
                                      let val handled = ref false
                                          val readableHandler = JavaScript.callback (fn _ =>
                                                                                        let fun doRead () : Word8Vector.vector list
                                                                                                = let val chunk = JavaScript.method (stream, "read") #[]
                                                                                                  in if JavaScript.=== (chunk, JavaScript.null) then
                                                                                                         nil
                                                                                                     else
                                                                                                         let val chunk : string = JavaScript.unsafeFromValue chunk (* Buffer as string *)
                                                                                                         in if chunk = "" then (* can this happen? *)
                                                                                                                doRead ()
                                                                                                            else
                                                                                                                [Byte.stringToBytes chunk]
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
                                       ; ()
                                      end
              )
    fun rawEnded (stream : JavaScript.value) : bool = JavaScript.unsafeFromValue (JavaScript.field (stream, "readableEnded"))
    fun fillBuffer (stream : JavaScript.value, f : instream) = let val chunks = rawRead stream
                                                               in f := List.foldr (fn (chunk, rest) => BUFFERED { buffer = chunk, position = 0, next = ref rest }) (TIP stream) chunks
                                                               end
    fun input (f : instream) = case !f of
                                   TIP stream => if rawEnded stream then
                                                     (Word8Vector.fromList [], f)
                                                 else
                                                     (fillBuffer (stream, f); input f)
                                 | BUFFERED { buffer, position, next } => (Word8VectorSlice.vector (Word8VectorSlice.slice (buffer, position, NONE)), next)
                                 | CLOSED => (Word8Vector.fromList [], f)
    fun newStreamWithBufferAndPosition (buffer, position, next) = if position >= Word8Vector.length buffer then
                                                                      next
                                                                  else
                                                                      ref (BUFFERED { buffer = buffer, position = position, next = next })
    fun input1 (f : instream) = case !f of
                                    TIP stream => if rawEnded stream then
                                                      NONE
                                                  else
                                                      (fillBuffer (stream, f); input1 f)
                                  | BUFFERED { buffer, position, next } => SOME (Word8Vector.sub (buffer, position), newStreamWithBufferAndPosition (buffer, position + 1, next))
                                  | CLOSED => NONE
    fun inputN (f : instream, n) = case !f of
                                       TIP stream => if rawEnded stream then
                                                         (Word8Vector.fromList [], f)
                                                     else
                                                         (fillBuffer (stream, f); inputN (f, n))
                                     | BUFFERED { buffer, position, next } => let val newPosition = position + n
                                                                              in if newPosition <= Word8Vector.length buffer then
                                                                                     (Word8VectorSlice.vector (Word8VectorSlice.slice (buffer, position, SOME n)), newStreamWithBufferAndPosition (buffer, newPosition, next))
                                                                                 else
                                                                                     let val buffer0 = Word8VectorSlice.vector (Word8VectorSlice.slice (buffer, position, NONE))
                                                                                         val (buffer1, next) = inputN (next, n - Word8Vector.length buffer0)
                                                                                     in (Word8VectorSlice.concat [Word8VectorSlice.full buffer0, Word8VectorSlice.full buffer1], next)
                                                                                     end
                                                                              end
                                     | CLOSED => (Word8Vector.fromList [], f)
    fun inputAll (f : instream) = let fun go (contentsRev, f) = let val (content, f) = input f
                                                                in if Word8Vector.length content = 0 then
                                                                       (Word8Vector.concat (List.rev contentsRev), f)
                                                                   else
                                                                       go (content :: contentsRev, f)
                                                                end
                                  in go ([], f)
                                  end
    fun canInput (f : instream, n) = if n < 0 then
                                         raise Size
                                     else
                                         case !f of
                                             TIP stream => if rawEnded stream then
                                                               SOME 0
                                                           else
                                                               NONE
                                           | BUFFERED { buffer, position, next } => SOME (Int.min (n, Word8Vector.length buffer - position))
                                           | CLOSED => SOME 0
    fun closeIn (f : instream) = case !f of
                                     TIP stream => ( JavaScript.method (stream, "destroy") #[]
                                                   ; f := CLOSED
                                                   )
                                   | BUFFERED { buffer, position, next } => closeIn next
                                   | CLOSED => ()
    fun endOfStream (f : instream) = case !f of
                                         TIP stream => rawEnded stream
                                       | BUFFERED { buffer, position, next } => false
                                       | CLOSED => true
    end
in
structure BinIO :> sig
              type instream
              type vector = Word8Vector.vector
              type elem = Word8.word
              val input : instream -> vector
              val input1 : instream -> elem option
              val inputN : instream * int -> vector
              val inputAll : instream -> vector
              val canInput : instream * int -> int option
              val lookahead : instream -> elem option
              val closeIn : instream -> unit
              val endOfStream : instream -> bool
              val openIn : string -> instream
              (* val scanStream *)
              structure StreamIO : sig
                            type instream
                            type elem = Word8.word
                            type vector = Word8Vector.vector
                            val input : instream -> vector * instream
                            val input1 : instream -> (elem * instream) option
                            val inputN : instream * int -> vector * instream
                            val inputAll : instream -> vector * instream
                            val canInput : instream * int -> int option
                            val closeIn : instream -> unit
                            val endOfStream : instream -> bool
                            (* mkInstream, getReader, filePosIn *)
                        end
              val mkInstream : StreamIO.instream -> instream
              val getInstream : instream -> StreamIO.instream
              val setInstream : instream * StreamIO.instream -> unit

              type outstream
              val output : outstream * vector -> unit
              val output1 : outstream * elem -> unit
              val flushOut : outstream -> unit
              val closeOut : outstream -> unit
              val openOut : string -> outstream
              val openAppend : string -> outstream
          end = struct
local
    _esImport [pure] { createReadStream, createWriteStream } from "node:fs";
    structure Instream :> sig
                  type instream
                  type vector = Word8Vector.vector
                  type elem = Word8.word
                  val input : instream -> vector
                  val input1 : instream -> elem option
                  val inputN : instream * int -> vector
                  val inputAll : instream -> vector
                  val canInput : instream * int -> int option
                  val lookahead : instream -> elem option
                  val closeIn : instream -> unit
                  val endOfStream : instream -> bool
                  val openIn : string -> instream
                                  (* val scanStream *)
                  structure StreamIO : sig
                                type instream
                                type elem = Word8.word
                                type vector = Word8Vector.vector
                                val input : instream -> vector * instream
                                val input1 : instream -> (elem * instream) option
                                val inputN : instream * int -> vector * instream
                                val inputAll : instream -> vector * instream
                                val canInput : instream * int -> int option
                                val closeIn : instream -> unit
                                val endOfStream : instream -> bool
                                (* mkInstream, getReader, filePosIn *)
                                end
                  val mkInstream : StreamIO.instream -> instream
                  val getInstream : instream -> StreamIO.instream
                  val setInstream : instream * StreamIO.instream -> unit
              end = struct
    type vector = Word8Vector.vector
    type elem = Word8.word
    structure StreamIO = Instream
    type instream = Instream.instream ref
    fun input stream = case StreamIO.input (!stream) of
                           (chunk, stream') => ( stream := stream'
                                               ; chunk
                                               )
    fun input1 stream = case StreamIO.input1 (!stream) of
                            NONE => NONE
                          | SOME (e, stream') => ( stream := stream'
                                                 ; SOME e
                                                 )
    fun inputN (stream, n) = case StreamIO.inputN (!stream, n) of
                                 (chunk, stream') => ( stream := stream'
                                                     ; chunk
                                                     )
    fun inputAll stream = case StreamIO.inputAll (!stream) of
                              (chunk, stream') => ( stream := stream'
                                                  ; chunk
                                                  )
    fun canInput (stream, n) = StreamIO.canInput (!stream, n)
    fun lookahead stream = case StreamIO.input1 (!stream) of
                               NONE => NONE
                             | SOME (e, _) => SOME e
    fun closeIn stream = StreamIO.closeIn (!stream)
    fun endOfStream stream = StreamIO.endOfStream (!stream)
    fun mkInstream stream = ref stream
    fun getInstream stream = !stream
    fun setInstream (stream, stream') = stream := stream'
    fun openIn path = ref (StreamIO.openReadable (JavaScript.call createReadStream #[JavaScript.unsafeToValue path (* as Buffer? *)]))
    end
    structure Outstream :> sig
                  type outstream
                  type vector = Word8Vector.vector
                  type elem = Word8.word
                  val output : outstream * vector -> unit
                  val output1 : outstream * elem -> unit
                  val flushOut : outstream -> unit
                  val closeOut : outstream -> unit
                  val openOut : string -> outstream
                  val openAppend : string -> outstream
              end = struct
    type outstream = JavaScript.value (* Writable *)
    type vector = Word8Vector.vector
    type elem = Word8.word
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
    fun output1 (stream, elem) = output (stream, Word8Vector.fromList [elem])
    fun flushOut stream = outputAndFlush (stream, Word8Vector.fromList [])
    fun closeOut stream = DelimCont.withSubCont (DelimCont.topLevel, fn cont : (unit, unit) DelimCont.subcont =>
                                                                        let val callback = JavaScript.callback (fn _ => DelimCont.pushSubCont (cont, fn () => ()))
                                                                        in ignore (JavaScript.method (stream, "end") #[JavaScript.unsafeToValue callback])
                                                                        end
                                                )
    fun openOut path = JavaScript.call createWriteStream #[JavaScript.unsafeToValue path (* as Buffer? *)]
    fun openAppend path = let val options = JavaScript.newObject ()
                              val () = JavaScript.set (options, JavaScript.fromWideString "flags", JavaScript.fromWideString "a")
                          in JavaScript.call createWriteStream #[JavaScript.unsafeToValue path (* as Buffer? *), options]
                          end
    end
in
open Instream
open Outstream
structure StreamIO = struct
open Instream.StreamIO
(* open Outstream.StreamIO *)
end
end (* local *)
end (* structure BinIO *)
end; (* local *)
