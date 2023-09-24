local
    structure DelimCont = LunarML.DelimCont
    structure Instream : sig
                  (* instream-related part of TEXT_STREAM_IO *)
                  type instream
                  type elem = char
                  type vector = string
                  val input : instream -> vector * instream
                  val input1 : instream -> (elem * instream) option
                  val inputN : instream * int -> vector * instream
                  val inputLine : instream -> (string * instream) option
                  val inputAll : instream -> vector * instream
                  val canInput : instream * int -> int option
                  val closeIn : instream -> unit
                  val endOfStream : instream -> bool
                                                    (* mkInstream, getReader, filePosIn *)
                  val openVector : vector -> instream
                  val openReadable : JavaScript.value -> instream
              end = struct
    type elem = char
    type vector = string
    datatype state = TIP of JavaScript.value (* Readable / paused mode / Buffer *)
                   | BUFFERED of { buffer : vector, position : int, next : instream } (* invariant: 0 <= position < String.size buffer *)
                   | CLOSED
    withtype instream = state ref
    fun openVector content = ref (BUFFERED { buffer = content, position = 0, next = ref CLOSED })
    fun openReadable stream = ref (TIP stream)
    fun rawRead (stream : JavaScript.value) : vector list
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
                                       ; ()
                                      end
              )
    fun rawEnded (stream : JavaScript.value) : bool = JavaScript.unsafeFromValue (JavaScript.field (stream, "readableEnded"))
    fun fillBuffer (stream : JavaScript.value, f : instream) = let val chunks = rawRead stream
                                                               in f := List.foldr (fn (chunk, rest) => BUFFERED { buffer = chunk, position = 0, next = ref rest }) (TIP stream) chunks
                                                               end
    fun input (f : instream) = case !f of
                                   TIP stream => if rawEnded stream then
                                                     ("", f)
                                                 else
                                                     (fillBuffer (stream, f); input f)
                                 | BUFFERED { buffer, position, next } => (String.extract (buffer, position, NONE), next)
                                 | CLOSED => ("", f)
    fun newStreamWithBufferAndPosition (buffer, position, next) = if position >= String.size buffer then
                                                                      next
                                                                  else
                                                                      ref (BUFFERED { buffer = buffer, position = position, next = next })
    fun input1 (f : instream) = case !f of
                                    TIP stream => if rawEnded stream then
                                                      NONE
                                                  else
                                                      (fillBuffer (stream, f); input1 f)
                                  | BUFFERED { buffer, position, next } => SOME (String.sub (buffer, position), newStreamWithBufferAndPosition (buffer, position + 1, next))
                                  | CLOSED => NONE
    fun inputN (f : instream, n) = case !f of
                                       TIP stream => if rawEnded stream then
                                                         ("", f)
                                                     else
                                                         (fillBuffer (stream, f); inputN (f, n))
                                     | BUFFERED { buffer, position, next } => let val newPosition = position + n
                                                                              in if newPosition <= String.size buffer then
                                                                                     (String.substring (buffer, position, n), newStreamWithBufferAndPosition (buffer, newPosition, next))
                                                                                 else
                                                                                     let val buffer0 = String.extract (buffer, position, NONE)
                                                                                         val (buffer1, next) = inputN (next, n - String.size buffer0)
                                                                                     in (buffer0 ^ buffer1, next)
                                                                                     end
                                                                              end
                                     | CLOSED => ("", f)
    fun inputLine (f : instream) = case !f of
                                       TIP stream => if rawEnded stream then
                                                         NONE
                                                     else
                                                         (fillBuffer (stream, f); inputLine f)
                                     | BUFFERED { buffer, position, next } => let fun findNewline i = if i >= String.size buffer then
                                                                                                          case inputLine next of
                                                                                                              NONE => SOME (String.extract (buffer, position, NONE) ^ "\n", next)
                                                                                                            | SOME (line, next) => SOME (String.extract (buffer, position, NONE) ^ line, next)
                                                                                                      else if String.sub (buffer, i) = #"\n" then
                                                                                                          let val newPosition = i + 1
                                                                                                          in SOME (String.substring (buffer, position, newPosition - position), newStreamWithBufferAndPosition (buffer, newPosition, next))
                                                                                                          end
                                                                                                      else
                                                                                                          findNewline (i + 1)
                                                                              in findNewline position
                                                                              end
                                     | CLOSED => NONE
    fun inputAll (f : instream) = let fun go (contentsRev, f) = case input f of
                                                                    ("", f) => (String.concat (List.rev contentsRev), f)
                                                                  | (content, f) => go (content :: contentsRev, f)
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
                                           | BUFFERED { buffer, position, next } => SOME (Int.min (n, String.size buffer - position))
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
structure TextIO :> sig
              type instream
              type vector = string
              type elem = char
              val input : instream -> vector
              val input1 : instream -> elem option
              val inputN : instream * int -> vector
              val inputAll : instream -> vector
              val canInput : instream * int -> int option
              val lookahead : instream -> elem option
              val closeIn : instream -> unit
              val endOfStream : instream -> bool
              val inputLine : instream -> string option
              val openIn : string -> instream
              val openString : string -> instream
              val stdIn : instream
              (* val scanStream *)
              structure StreamIO : sig
                            type instream
                            type elem = char
                            type vector = string
                            val input : instream -> vector * instream
                            val input1 : instream -> (elem * instream) option
                            val inputN : instream * int -> vector * instream
                            val inputLine : instream -> (string * instream) option
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
              val outputSubstr : outstream * Substring.substring -> unit
              val flushOut : outstream -> unit
              val closeOut : outstream -> unit
              val openOut : string -> outstream
              val openAppend : string -> outstream
              val stdOut : outstream
              val stdErr : outstream
              val print : string -> unit
          end = struct
local
    _esImport [pure] { stdin, stdout, stderr } from "node:process";
    _esImport [pure] { createReadStream, createWriteStream } from "node:fs";
    structure Instream :> sig
                  type instream
                  type vector = string
                  type elem = char
                  val input : instream -> vector
                  val input1 : instream -> elem option
                  val inputN : instream * int -> vector
                  val inputAll : instream -> vector
                  val canInput : instream * int -> int option
                  val lookahead : instream -> elem option
                  val closeIn : instream -> unit
                  val endOfStream : instream -> bool
                  val inputLine : instream -> string option
                  val openIn : string -> instream
                  val openString : string -> instream
                  val stdIn : instream
                                  (* val scanStream *)
                  structure StreamIO : sig
                                type instream
                                type elem = char
                                type vector = string
                                val input : instream -> vector * instream
                                val input1 : instream -> (elem * instream) option
                                val inputN : instream * int -> vector * instream
                                val inputLine : instream -> (string * instream) option
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
    type vector = string
    type elem = char
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
    fun inputLine stream = case StreamIO.inputLine (!stream) of
                               NONE => NONE
                             | SOME (line, stream') => ( stream := stream'
                                                       ; SOME line
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
    fun openString content = ref (StreamIO.openVector content)
    val stdIn = LunarML.assumeDiscardable (fn () => ref (StreamIO.openReadable stdin)) ()
    end
    structure Outstream :> sig
                  type outstream
                  type vector = string
                  type elem = char
                  val output : outstream * vector -> unit
                  val output1 : outstream * elem -> unit
                  val outputSubstr : outstream * Substring.substring -> unit
                  val flushOut : outstream -> unit
                  val closeOut : outstream -> unit
                  val openOut : string -> outstream
                  val openAppend : string -> outstream
                  val stdOut : outstream
                  val stdErr : outstream
                  val print : string -> unit
              end = struct
    type outstream = JavaScript.value (* Writable *)
    type vector = string
    type elem = char
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
    fun output1 (stream, elem) = output (stream, String.str elem)
    fun outputSubstr (stream, substring) = output (stream, Substring.string substring)
    fun flushOut stream = outputAndFlush (stream, "")
    fun closeOut stream = ignore (JavaScript.method (stream, "end") #[])
    fun openOut path = JavaScript.call createWriteStream #[JavaScript.unsafeToValue path (* as Buffer? *)]
    fun openAppend path = let val options = JavaScript.newObject ()
                              val () = JavaScript.set (options, JavaScript.fromWideString "flags", JavaScript.fromWideString "a")
                          in JavaScript.call createWriteStream #[JavaScript.unsafeToValue path (* as Buffer? *), options]
                          end
    val stdOut = stdout
    val stdErr = stderr
    fun print s = outputAndFlush (stdOut, s)
    end
in
open Instream
open Outstream
structure StreamIO = struct
open Instream.StreamIO
(* open Outstream.StreamIO *)
end
end (* local *)
end (* structure TextIO *)
end; (* local *)
val print = TextIO.print;
