structure Lua : sig
              type value
              exception Error of value
              exception TypeError of string
              val sub : value * value -> value  (* t[k] *)
              val field : value * string -> value  (* t[k] *)
              val set : value * value * value -> unit  (* t[k] = v *)
              val setField : value * string * value -> unit  (* t[k] = v *)
              val global : string -> value  (* _G[name] *)
              val setGlobal : string * value -> unit  (* _G[name] = v *)
              val call : value -> value vector -> value vector  (* f(args) *)
              val call0 : value -> value vector -> unit  (* f(args) *)
              val call1 : value -> value vector -> value  (* f(args) *)
              val call2 : value -> value vector -> value * value  (* f(args) *)
              val call3 : value -> value vector -> value * value * value  (* f(args) *)
              val method : value * string -> value vector -> value vector  (* f:name(args) *)
              val NIL : value  (* Lua nil *)
              val isNil : value -> bool  (* x == nil *)
              val isFalsy : value -> bool  (* not x *)
              val fromBool : bool -> value
              val fromInt : int -> value
              val fromWord : word -> value
              val fromReal : real -> value
              val fromString : string -> value
              val unsafeToValue : 'a -> value
              val unsafeFromValue : value -> 'a
              val newTable : unit -> value  (* {} *)
              val function : (value vector -> value vector) -> value
              val + : value * value -> value
              val - : value * value -> value
              val * : value * value -> value
              val / : value * value -> value
              (* val // : value * value -> value *)
              val % : value * value -> value
              val pow : value * value -> value  (* x ^ y *)
              val unm : value -> value  (* unary minus *)
              val andb : value * value -> value  (* bit.band(x, y) *)
              val orb : value * value -> value  (* bit.bor(x, y) *)
              val xorb : value * value -> value  (* bit.bxor(x, y) *)
              val notb : value -> value  (* bit.bnot(x) *)
              val << : value * value -> value  (* bit.lshift(x, y) *)
              val >> : value * value -> value  (* bit.rshift(x, y) *)
              val == : value * value -> bool
              val ~= : value * value -> bool
              val < : value * value -> bool
              val > : value * value -> bool
              val <= : value * value -> bool
              val >= : value * value -> bool
              val concat : value * value -> value  (* x .. y *)
              val length : value -> value  (* #x *)
              val typeof : value -> string (* type *)
              val checkString : value -> string
              val checkBoolean : value -> bool
              val checkInt : value -> int
              val checkWord : value -> word
              val checkReal : value -> real
              val optString : value -> string option
              structure Lib : sig
                            val assert : value
                            val error : value
                            val getmetatable : value
                            val math : value
                            val pairs : value
                            val pcall : value
                            val require : value
                            val setmetatable : value
                            val string : value
                            val table : value
                            val tonumber : value
                            val tostring : value
                            val type' : value
                            structure math : sig
                                          val abs : value
                                          val atan : value
                                          val ceil : value
                                          val floor : value
                                          val fmod : value
                                          val huge : value
                                          val log : value
                                          val modf : value
                                      end
                            structure string : sig
                                          val byte : value
                                          val char : value
                                          val find : value
                                          val format : value
                                          val gsub : value
                                          val match : value
                                          val sub : value
                                      end
                            structure table : sig
                                          val concat : value
                                          val pack : value
                                          val unpack : value
                                      end
                            structure bit : sig
                                          val tobit : value
                                          val tohex : value
                                          val bnot : value
                                          val band : value
                                          val bor : value
                                          val bxor : value
                                          val lshift : value
                                          val rshift : value
                                          val arshift : value
                                          val rol : value
                                          val ror : value
                                          val bswap : value
                                      end
                            val bit : value (* Lua BitOp *)
                            val lfs : value option (* LuaFileSystem *)
                        end
          end = struct
type value = _Prim.Lua.value
exception Error = _Prim.Lua.Error
fun global name = _primCall "Lua.global" (name)
fun setGlobal (name, value) = _primCall "Lua.setGlobal" (name, value)
fun call f args = _primCall "Lua.call" (f, args)
fun call0 f args = (_primCall "Lua.call" (f, args); ())
fun call1 f args = _primCall "Lua.call1" (f, args)
fun call2 f args = _primCall "Lua.call2" (f, args)
fun call3 f args = _primCall "Lua.call3" (f, args)
fun method (obj, name) args = _primCall "Lua.method" (obj, name, args)
val NIL = _Prim.Lua.NIL
fun newTable () = _primCall "Lua.newTable" ()
val function = _Prim.Lua.function
fun unsafeToValue x : value = _primCall "Unsafe.cast" (x)
fun unsafeFromValue (x : value) = _primCall "Unsafe.cast" (x)
val fromBool : bool -> value = unsafeToValue
val fromInt : int -> value = unsafeToValue
val fromWord : word -> value = unsafeToValue
val fromReal : real -> value = unsafeToValue
val fromString : string -> value = unsafeToValue
fun sub (t, k) = _primCall "Lua.sub" (t, k)
fun field (t : value, name : string) = sub (t, fromString name)
fun set (t, k, v) = _primCall "Lua.set" (t, k, v)
fun setField (t, name, v) = _primCall "Lua.set" (t, fromString name, v)
fun isNil x = _primCall "Lua.isNil" (x)
fun isFalsy x = _primCall "Lua.isFalsy" (x)
fun x + y = _primCall "Lua.+" (x, y)
fun x - y = _primCall "Lua.-" (x, y)
fun x * y = _primCall "Lua.*" (x, y)
fun x / y = _primCall "Lua./" (x, y)
(* fun // (x, y) = _primCall "Lua.//" (x, y) *)
fun % (x, y) = _primCall "Lua.%" (x, y)
fun pow (x, y) = _primCall "Lua.pow" (x, y)
fun unm x = _primCall "Lua.unm" (x)
val require = LunarML.assumeDiscardable global "require"
val bit = _Prim.Lua.Lib.bit
val band = LunarML.assumeDiscardable field (bit, "band")
val bor = LunarML.assumeDiscardable field (bit, "bor")
val bxor = LunarML.assumeDiscardable field (bit, "bxor")
val bnot = LunarML.assumeDiscardable field (bit, "bnot")
val lshift = _Prim.Lua.Lib.bit.lshift
val rshift = _Prim.Lua.Lib.bit.rshift
fun andb (x, y) = call1 band #[x, y]
fun orb (x, y) = call1 bor #[x, y]
fun xorb (x, y) = call1 bxor #[x, y]
fun notb x = call1 bnot #[x]
fun << (x, y) = call1 lshift #[x, y]
fun >> (x, y) = call1 rshift #[x, y]
fun == (x, y) = _primCall "Lua.==" (x, y)
fun ~= (x, y) = _primCall "Lua.~=" (x, y)
fun x < y = _primCall "Lua.<" (x, y)
fun x > y = _primCall "Lua.>" (x, y)
fun x <= y = _primCall "Lua.<=" (x, y)
fun x >= y = _primCall "Lua.>=" (x, y)
fun concat (x, y) = _primCall "Lua.concat" (x, y)
fun length x = _primCall "Lua.length" (x)
structure Lib = struct
val assert = _Prim.Lua.Lib.assert
val error = _Prim.Lua.Lib.error
val getmetatable = _Prim.Lua.Lib.getmetatable
val pairs = _Prim.Lua.Lib.pairs
val pcall = _Prim.Lua.Lib.pcall
val setmetatable = _Prim.Lua.Lib.setmetatable
val math = _Prim.Lua.Lib.math
val string = _Prim.Lua.Lib.string
val table = _Prim.Lua.Lib.table
val require = require
val tonumber = LunarML.assumeDiscardable global "tonumber"
val tostring = LunarML.assumeDiscardable global "tostring"
val type' = LunarML.assumeDiscardable global "type"
structure math = struct
val abs = _Prim.Lua.Lib.math.abs
val atan = LunarML.assumeDiscardable field (math, "atan")
val ceil = LunarML.assumeDiscardable field (math, "ceil")
val floor = LunarML.assumeDiscardable field (math, "floor")
val fmod = LunarML.assumeDiscardable field (math, "fmod")
val huge = LunarML.assumeDiscardable field (math, "huge")
val log = LunarML.assumeDiscardable field (math, "log")
val modf = LunarML.assumeDiscardable field (math, "modf")
end
structure string = struct
val format = _Prim.Lua.Lib.string.format
val byte = LunarML.assumeDiscardable field (string, "byte")
val char = _Prim.Lua.Lib.string.char
val find = LunarML.assumeDiscardable field (string, "find")
val gsub = LunarML.assumeDiscardable field (string, "gsub")
val match = LunarML.assumeDiscardable field (string, "match")
val sub = LunarML.assumeDiscardable field (string, "sub")
end
structure table = struct
val pack = _Prim.Lua.Lib.table.pack
val unpack = _Prim.Lua.Lib.table.unpack
val concat = LunarML.assumeDiscardable field (table, "concat")
end
val bit = _Prim.Lua.Lib.bit
structure bit = struct
val tobit = LunarML.assumeDiscardable field (bit, "tobit")
val tohex = LunarML.assumeDiscardable field (bit, "tohex")
val bnot = bnot
val band = band
val bor = bor
val bxor = bxor
val lshift = _Prim.Lua.Lib.bit.lshift
val rshift = _Prim.Lua.Lib.bit.rshift
val arshift = LunarML.assumeDiscardable field (bit, "arshift")
val rol = LunarML.assumeDiscardable field (bit, "rol")
val ror = LunarML.assumeDiscardable field (bit, "ror")
val bswap = LunarML.assumeDiscardable field (bit, "bswap")
end
val lfs = LunarML.assumeDiscardable
              (fn () => let val (ok, module) = call2 pcall #[require, fromString "lfs"]
                            val ok = unsafeFromValue ok : bool
                        in if ok then
                               SOME module
                           else
                               NONE
                        end
              ) ()
end
fun typeof x : string = let val result = call1 Lib.type' #[x]
                        in unsafeFromValue result
                        end
exception TypeError of string
fun checkString x : string = let val t = typeof x
                             in if t = "string" then
                                    unsafeFromValue x
                                else
                                    raise TypeError (String.^ ("expected a string, but got ", t))
                             end
fun checkBoolean x : bool = let val t = typeof x
                            in if t = "boolean" then
                                   unsafeFromValue x
                               else
                                   raise TypeError (String.^ ("expected a boolean, but got ", t))
                            end
fun checkInt x : int = let val t = typeof x
                       in if t = "number" andalso == (call1 Lib.bit.tobit #[x], x) then
                              unsafeFromValue x
                          else
                              raise TypeError (String.^ ("expected an integer, but got ", typeof x))
                       end
fun checkWord x : word = let val t = typeof x
                         in if t = "number" andalso == (x, % (x, fromReal 0x100000000.0)) then
                                unsafeFromValue x
                            else
                                raise TypeError (String.^ ("expected an integer, but got ", typeof x))
                         end
fun checkReal x : real = let val t = typeof x
                         in if t = "number" then
                                unsafeFromValue x
                            else
                                raise TypeError (String.^ ("expected a real number, but got ", typeof x))
                         end
fun optString x : string option = let val t = typeof x
                                  in if t = "string" then
                                         SOME (unsafeFromValue x)
                                     else if t = "nil" then
                                         NONE
                                     else
                                         raise TypeError (String.^ ("expected a string, but got ", t))
                                  end
end; (* structure Lua *)
