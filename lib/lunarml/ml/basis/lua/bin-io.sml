structure BinIO :> sig
              type vector = Word8Vector.vector
              type elem = Word8.word
              type instream
              type outstream
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
              val openIn : string -> instream
              val openOut : string -> outstream
              val openAppend : string -> outstream
          end = struct
local
    structure Instream :> sig
                  type instream
                  type vector = Word8Vector.vector
                  type elem = Word8.word
                  val input : instream -> vector
                  val input1 : instream -> elem option
                  val inputN : instream * int -> vector
                  val inputAll : instream -> vector
                  (* val canInput *)
                  (* val lookahead *)
                  val closeIn : instream -> unit
                  val endOfStream : instream -> bool
                  val openIn : string -> instream
              end = struct
    type instream = Lua.value
    type vector = Word8Vector.vector
    type elem = Word8.word
    fun input f = let val result = Vector.sub (Lua.method (f, "read") #[Lua.fromInt 1024], 0)
                  in if Lua.isFalsy result then
                         Word8Vector.fromList []
                     else
                         Lua.unsafeFromValue result : vector
                  end
    fun input1 f = let val result = Vector.sub (Lua.method (f, "read") #[Lua.fromInt 1], 0)
                   in if Lua.isNil result then
                          NONE
                      else
                          SOME (Word8Vector.sub (Lua.unsafeFromValue result : vector, 0))
                   end
    fun inputN (f, n : int) = if n < 0 then
                                  raise Size
                              else
                                  let val result = Vector.sub (Lua.method (f, "read") #[Lua.fromInt n], 0)
                                  in if Lua.isNil result then
                                         Word8Vector.fromList []
                                     else
                                         Lua.unsafeFromValue result : vector
                                  end
    fun inputAll f = let val result = Vector.sub (Lua.method (f, "read") #[Lua.fromString "a"], 0)
                     in Lua.unsafeFromValue result : vector
                     end
    fun closeIn f = (Lua.method (f, "close") #[]; ())
    fun endOfStream f = let val result = Vector.sub (Lua.method (f, "read") #[Lua.fromInt 0], 0)
                        in if Lua.isFalsy result then
                               true
                           else
                               false
                        end
    (* BIN_IO *)
    fun openIn f = let val (r0, message) = Lua.call2 Lua.Lib.io.open' #[Lua.fromString f, Lua.fromString "rb"]
                   in if Lua.isNil r0 then
                          raise IO.Io { name = f, function = "BinIO.openIn", cause = Fail (Lua.unsafeFromValue message) } (* TODO: cause *)
                      else
                          r0
                   end
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
    type outstream = Lua.value
    type vector = Word8Vector.vector
    type elem = Word8.word
    fun output (f, s : Word8Vector.vector) = (Lua.method (f, "write") #[Lua.unsafeToValue s]; ())
    fun output1 (f, c : Word8.word) = (Lua.method (f, "write") #[Lua.fromString (String.str (Byte.byteToChar c))]; ())
    fun flushOut f = (Lua.method (f, "flush") #[]; ())
    fun closeOut f = (Lua.method (f, "close") #[]; ())
    (* BIN_IO *)
    fun openOut f = let val (r0, message) = Lua.call2 Lua.Lib.io.open' #[Lua.fromString f, Lua.fromString "wb"]
                    in if Lua.isNil r0 then
                           raise IO.Io { name = f, function = "BinIO.openOut", cause = Fail (Lua.unsafeFromValue message) } (* TODO: cause *)
                       else
                           r0
                    end
    fun openAppend f = let val (r0, message) = Lua.call2 Lua.Lib.io.open' #[Lua.fromString f, Lua.fromString "ab"]
                       in if Lua.isNil r0 then
                              raise IO.Io { name = f, function = "BinIO.openAppend", cause = Fail (Lua.unsafeFromValue message) } (* TODO: cause *)
                          else
                              r0
                       end
    end
in
open Instream
open Outstream
end (* local *)
end; (* structure BinIO *)
