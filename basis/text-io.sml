structure TextIO :> sig
              type instream
              type outstream
              type vector = string
              type elem = char
              val input : instream -> vector
              val input1 : instream -> elem option
              val inputN : instream * int -> vector
              val inputAll : instream -> vector
              val closeIn : instream -> unit
              val endOfStream : instream -> bool
              val output : outstream * vector -> unit
              val output1 : outstream * elem -> unit
              val flushOut : outstream -> unit
              val closeOut : outstream -> unit
              val inputLine : instream -> string option
              val openIn : string -> instream
              val openOut : string -> outstream
              val openAppend : string -> outstream
              val stdIn : instream
              val stdOut : outstream
              val stdErr : outstream
              val print : string -> unit
          end = struct
local
    val io = LunarML.assumeDiscardable (Lua.global "io")
    val io_open = LunarML.assumeDiscardable (Lua.field (io, "open"))
    val io_write = LunarML.assumeDiscardable (Lua.field (io, "write"))
    structure Instream :> sig
                  type instream
                  type vector = string
                  type elem = char
                  val input : instream -> vector
                  val input1 : instream -> elem option
                  val inputN : instream * int -> vector
                  val inputAll : instream -> vector
                  (* val canInput *)
                  (* val lookahead *)
                  val closeIn : instream -> unit
                  val endOfStream : instream -> bool
                  val inputLine : instream -> string option
                  val openIn : string -> instream
                  (* val openString *)
                  val stdIn : instream
                  (* val scanStream *)
              end = struct
    type instream = Lua.value
    type vector = string
    type elem = char
    fun input f = let val result = Vector.sub (Lua.method (f, "read") #[Lua.fromInt 1024], 0)
                  in if Lua.isFalsy result then
                         ""
                     else
                         Lua.unsafeFromValue result : vector
                  end
    fun input1 f = let val result = Vector.sub (Lua.method (f, "read") (vector [Lua.fromInt 1]), 0)
                   in if Lua.isNil result then
                          NONE
                      else
                          SOME (Lua.unsafeFromValue result : elem)
                   end
    fun inputN (f, n : int) = if n < 0 then
                                  raise Size
                              else
                                  let val result = Vector.sub (Lua.method (f, "read") (vector [Lua.fromInt n]), 0)
                                  in if Lua.isNil result then
                                         ""
                                     else
                                         (Lua.unsafeFromValue result : vector)
                                  end
    fun inputAll f = let val result = Vector.sub (Lua.method (f, "read") (vector [Lua.fromString "a"]), 0)
                     in Lua.unsafeFromValue result : vector
                     end
    fun closeIn f = (Lua.method (f, "close") (vector []); ())
    fun endOfStream f = let val result = Vector.sub (Lua.method (f, "read") #[Lua.fromInt 0], 0)
                        in if Lua.isFalsy result then
                               true
                           else
                               false
                        end

    (* TEXT_IO *)
    fun inputLine f = let val result = Vector.sub (Lua.method (f, "read") (vector [Lua.fromString "L"]), 0)
                      in if Lua.isNil result then
                             NONE
                         else
                             SOME (Lua.unsafeFromValue result : string)
                      end
    fun openIn f = let val result = Lua.call io_open #[Lua.fromString f, Lua.fromString "r"]
                   in if Lua.isNil (Vector.sub (result, 0)) then
                          raise IO.Io { name = f, function = "TextIO.openIn", cause = Fail (Lua.unsafeFromValue (Vector.sub (result, 1))) } (* TODO: cause *)
                      else
                          Vector.sub (result, 0)
                   end
    val stdIn = LunarML.assumeDiscardable (Lua.field (io, "stdin"))
    end
    structure Outstream :> sig
                  type outstream
                  type vector = string
                  type elem = char
                  val output : outstream * vector -> unit
                  val output1 : outstream * elem -> unit
                  val flushOut : outstream -> unit
                  val closeOut : outstream -> unit
                  val openOut : string -> outstream
                  val openAppend : string -> outstream
                  val stdOut : outstream
                  val stdErr : outstream
                  val print : string -> unit
              end = struct
    type outstream = Lua.value
    type vector = string
    type elem = char
    fun output (f, s) = (Lua.method (f, "write") (vector [Lua.fromString s]); ())
    fun output1 (f, c) = (Lua.method (f, "write") (vector [Lua.fromString (String.str c)]); ())
    fun flushOut f = (Lua.method (f, "flush") (vector []); ())
    fun closeOut f = (Lua.method (f, "close") (vector []); ())
    (* outputsubstr : outstream * substring -> unit *)
    fun openOut f = let val result = Lua.call io_open #[Lua.fromString f, Lua.fromString "w"]
                    in if Lua.isNil (Vector.sub (result, 0)) then
                           raise IO.Io { name = f, function = "TextIO.openOut", cause = Fail (Lua.unsafeFromValue (Vector.sub (result, 1))) } (* TODO: cause *)
                       else
                           Vector.sub (result, 0)
                    end
    fun openAppend f = let val result = Lua.call io_open #[Lua.fromString f, Lua.fromString "a"]
                       in if Lua.isNil (Vector.sub (result, 0)) then
                              raise IO.Io { name = f, function = "TextIO.openAppend", cause = Fail (Lua.unsafeFromValue (Vector.sub (result, 1))) } (* TODO: cause *)
                          else
                              Vector.sub (result, 0)
                       end
    val stdOut = LunarML.assumeDiscardable (Lua.field (io, "stdout"))
    val stdErr = LunarML.assumeDiscardable (Lua.field (io, "stderr"))
    fun print s = (Lua.call io_write #[Lua.fromString s]; ())
    end
in
open Instream
open Outstream
(* IMPERATIVE_IO *)
(* fun openString f *)
(* scanStream *)
end (* local *)
end; (* structure TextIO *)
