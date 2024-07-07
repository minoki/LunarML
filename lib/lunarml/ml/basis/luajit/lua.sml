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
              val method0 : value * string -> value vector -> unit  (* f:name(args) *)
              val method1 : value * string -> value vector -> value  (* f:name(args) *)
              val method2 : value * string -> value vector -> value * value  (* f:name(args) *)
              val method3 : value * string -> value vector -> value * value * value  (* f:name(args) *)
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
              val negate : value -> value  (* unary minus *)
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
                            val GLOBAL : value
                            val VERSION : value
                            val assert : value
                            val collectgarbage : value
                            val coroutine : value
                            val debug : value
                            val dofile : value
                            val error : value
                            val getfenv : value
                            val getmetatable : value
                            val io : value
                            val ipairs : value
                            val load : value
                            val loadfile : value
                            val loadstring : value
                            val module : value
                            val math : value
                            val next : value
                            val os : value
                            val package : value
                            val pairs : value
                            val pcall : value
                            val print : value
                            val rawequal : value
                            val rawget : value
                            val rawset : value
                            val require : value
                            val select : value
                            val setfenv : value
                            val setmetatable : value
                            val string : value
                            val table : value
                            val tonumber : value
                            val tostring : value
                            val type' : value
                            val unpack : value
                            val xpcall : value
                            structure coroutine : sig
                                          val create : value
                                          val resume : value
                                          val running : value
                                          val status : value
                                          val wrap : value
                                          val yield : value
                                      end
                            structure debug : sig
                                          val debug : value
                                          val getfenv : value
                                          val gethook : value
                                          val getinfo : value
                                          val getlocal : value
                                          val getmetatable : value
                                          val getregistry : value
                                          val getupvalue : value
                                          val setfenv : value
                                          val sethook : value
                                          val setlocal : value
                                          val setmetatable : value
                                          val setupvalue : value
                                          val traceback : value
                                          val upvalueid : value (* LuaJIT extension (from Lua 5.2) *)
                                          val upvaluejoin : value (* LuaJIT extension (from Lua 5.2) *)
                                      end
                            structure io : sig
                                          val close : value
                                          val flush : value
                                          val input : value
                                          val lines : value
                                          val open' : value
                                          val output : value
                                          val popen : value
                                          val read : value
                                          val stderr : value
                                          val stdin : value
                                          val stdout : value
                                          val tmpfile : value
                                          val type' : value
                                          val write : value
                                      end
                            structure math : sig
                                          val abs : value
                                          val acos : value
                                          val asin : value
                                          val atan : value
                                          val atan2 : value
                                          val ceil : value
                                          val cos : value
                                          val cosh : value
                                          val deg : value
                                          val exp : value
                                          val floor : value
                                          val fmod : value
                                          val frexp : value
                                          val huge : value
                                          val ldexp : value
                                          val log : value
                                          val log10 : value
                                          val max : value
                                          val min : value
                                          val modf : value
                                          val pi : value
                                          val pow : value
                                          val rad : value
                                          val random : value
                                          val randomseed : value
                                          val sin : value
                                          val sinh : value
                                          val sqrt : value
                                          val tan : value
                                          val tanh : value
                                      end
                            structure os : sig
                                          val clock : value
                                          val date : value
                                          val difftime : value
                                          val execute : value
                                          val exit : value
                                          val getenv : value
                                          val remove : value
                                          val rename : value
                                          val setlocale : value
                                          val time : value
                                          val tmpname : value
                                      end
                            structure package : sig
                                          val cpath : value
                                          val loaded : value
                                          val loaders : value
                                          val loadlib : value
                                          val path : value
                                          val preload : value
                                          (* val searchers : value (* LuaJIT extension (from Lua 5.2) *) *)
                                          val searchpath : value (* LuaJIT extension (from Lua 5.2) *)
                                          val seeall : value
                                      end
                            structure string : sig
                                          val byte : value
                                          val char : value
                                          val dump : value
                                          val find : value
                                          val format : value
                                          val gmatch : value
                                          val gsub : value
                                          val len : value
                                          val lower : value
                                          val match : value
                                          val rep : value
                                          val reverse : value
                                          val sub : value
                                          val upper : value
                                      end
                            structure table : sig
                                          val concat : value
                                          val insert : value
                                          val maxn : value
                                          val pack : value
                                          val remove : value
                                          val sort : value
                                          val unpack : value (* extension (equivalent to global unpack) *)
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
fun method0 (obj, name) args = (_primCall "Lua.method" (obj, name, args); ())
fun method1 (obj, name) args = _primCall "Lua.method1" (obj, name, args)
fun method2 (obj, name) args = _primCall "Lua.method2" (obj, name, args)
fun method3 (obj, name) args = _primCall "Lua.method3" (obj, name, args)
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
fun negate x = _primCall "Lua.negate" (x)
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
val GLOBAL = LunarML.assumeDiscardable global "_G"
val VERSION = LunarML.assumeDiscardable global "_VERSION"
val assert = _Prim.Lua.Lib.assert
val collectgarbage = LunarML.assumeDiscardable global "collectgarbage"
val coroutine = LunarML.assumeDiscardable global "coroutine"
val debug = LunarML.assumeDiscardable global "debug"
val dofile = LunarML.assumeDiscardable global "dofile"
val error = _Prim.Lua.Lib.error
val getfenv = LunarML.assumeDiscardable global "getfenv"
val getmetatable = _Prim.Lua.Lib.getmetatable
val io = LunarML.assumeDiscardable global "io"
val ipairs = LunarML.assumeDiscardable global "ipairs"
val load = LunarML.assumeDiscardable global "load"
val loadfile = LunarML.assumeDiscardable global "loadfile"
val loadstring = LunarML.assumeDiscardable global "loadstring"
val module = LunarML.assumeDiscardable global "module"
val math = _Prim.Lua.Lib.math
val next = LunarML.assumeDiscardable global "next"
val os = LunarML.assumeDiscardable global "os"
val package = LunarML.assumeDiscardable global "package"
val pairs = _Prim.Lua.Lib.pairs
val pcall = _Prim.Lua.Lib.pcall
val print = LunarML.assumeDiscardable global "print"
val rawequal = LunarML.assumeDiscardable global "rawequal"
val rawget = LunarML.assumeDiscardable global "rawget"
val rawset = LunarML.assumeDiscardable global "rawset"
val require = require
val select = LunarML.assumeDiscardable global "select"
val setfenv = LunarML.assumeDiscardable global "setfenv"
val setmetatable = _Prim.Lua.Lib.setmetatable
val string = _Prim.Lua.Lib.string
val table = _Prim.Lua.Lib.table
val tonumber = LunarML.assumeDiscardable global "tonumber"
val tostring = LunarML.assumeDiscardable global "tostring"
val type' = LunarML.assumeDiscardable global "type"
val unpack = _Prim.Lua.Lib.table.unpack
val xpcall = LunarML.assumeDiscardable global "xpcall"
structure coroutine = struct
val create = LunarML.assumeDiscardable field (coroutine, "create")
val resume = LunarML.assumeDiscardable field (coroutine, "resume")
val running = LunarML.assumeDiscardable field (coroutine, "running")
val status = LunarML.assumeDiscardable field (coroutine, "status")
val wrap = LunarML.assumeDiscardable field (coroutine, "wrap")
val yield = LunarML.assumeDiscardable field (coroutine, "yield")
end
structure debug = struct
val getfenv = LunarML.assumeDiscardable field (debug, "getfenv")
val gethook = LunarML.assumeDiscardable field (debug, "gethook")
val getinfo = LunarML.assumeDiscardable field (debug, "getinfo")
val getlocal = LunarML.assumeDiscardable field (debug, "getlocal")
val getmetatable = LunarML.assumeDiscardable field (debug, "getmetatable")
val getregistry = LunarML.assumeDiscardable field (debug, "getregistry")
val getupvalue = LunarML.assumeDiscardable field (debug, "getupvalue")
val setfenv = LunarML.assumeDiscardable field (debug, "setfenv")
val sethook = LunarML.assumeDiscardable field (debug, "sethook")
val setlocal = LunarML.assumeDiscardable field (debug, "setlocal")
val setmetatable = LunarML.assumeDiscardable field (debug, "setmetatable")
val setupvalue = LunarML.assumeDiscardable field (debug, "setupvalue")
val traceback = LunarML.assumeDiscardable field (debug, "traceback")
val upvalueid = LunarML.assumeDiscardable field (debug, "upvalueid")
val upvaluejoin = LunarML.assumeDiscardable field (debug, "upvaluejoin")
val debug = LunarML.assumeDiscardable field (debug, "debug")
end
structure io = struct
val close = LunarML.assumeDiscardable field (io, "close")
val flush = LunarML.assumeDiscardable field (io, "flush")
val input = LunarML.assumeDiscardable field (io, "input")
val lines = LunarML.assumeDiscardable field (io, "lines")
val open' = LunarML.assumeDiscardable field (io, "open")
val output = LunarML.assumeDiscardable field (io, "output")
val popen = LunarML.assumeDiscardable field (io, "popen")
val read = LunarML.assumeDiscardable field (io, "read")
val stderr = LunarML.assumeDiscardable field (io, "stderr")
val stdin = LunarML.assumeDiscardable field (io, "stdin")
val stdout = LunarML.assumeDiscardable field (io, "stdout")
val tmpfile = LunarML.assumeDiscardable field (io, "tmpfile")
val type' = LunarML.assumeDiscardable field (io, "type")
val write = LunarML.assumeDiscardable field (io, "write")
end
structure math = struct
val abs = _Prim.Lua.Lib.math.abs
val acos = LunarML.assumeDiscardable field (math, "acos")
val asin = LunarML.assumeDiscardable field (math, "asin")
val atan = LunarML.assumeDiscardable field (math, "atan")
val atan2 = LunarML.assumeDiscardable field (math, "atan2")
val ceil = LunarML.assumeDiscardable field (math, "ceil")
val cos = LunarML.assumeDiscardable field (math, "cos")
val cosh = LunarML.assumeDiscardable field (math, "cosh")
val deg = LunarML.assumeDiscardable field (math, "deg")
val exp = LunarML.assumeDiscardable field (math, "exp")
val floor = LunarML.assumeDiscardable field (math, "floor")
val fmod = LunarML.assumeDiscardable field (math, "fmod")
val frexp = LunarML.assumeDiscardable field (math, "frexp")
val huge = LunarML.assumeDiscardable field (math, "huge")
val ldexp = LunarML.assumeDiscardable field (math, "ldexp")
val log = LunarML.assumeDiscardable field (math, "log")
val log10 = LunarML.assumeDiscardable field (math, "log10")
val max = LunarML.assumeDiscardable field (math, "max")
val min = LunarML.assumeDiscardable field (math, "min")
val modf = LunarML.assumeDiscardable field (math, "modf")
val pi = LunarML.assumeDiscardable field (math, "pi")
val pow = LunarML.assumeDiscardable field (math, "pow")
val rad = LunarML.assumeDiscardable field (math, "rad")
val random = LunarML.assumeDiscardable field (math, "random")
val randomseed = LunarML.assumeDiscardable field (math, "randomseed")
val sin = LunarML.assumeDiscardable field (math, "sin")
val sinh = LunarML.assumeDiscardable field (math, "sinh")
val sqrt = LunarML.assumeDiscardable field (math, "sqrt")
val tan = LunarML.assumeDiscardable field (math, "tan")
val tanh = LunarML.assumeDiscardable field (math, "tanh")
end
structure os = struct
val clock = LunarML.assumeDiscardable field (os, "clock")
val date = LunarML.assumeDiscardable field (os, "date")
val difftime = LunarML.assumeDiscardable field (os, "difftime")
val execute = LunarML.assumeDiscardable field (os, "execute")
val exit = LunarML.assumeDiscardable field (os, "exit")
val getenv = LunarML.assumeDiscardable field (os, "getenv")
val remove = LunarML.assumeDiscardable field (os, "remove")
val rename = LunarML.assumeDiscardable field (os, "rename")
val setlocale = LunarML.assumeDiscardable field (os, "setlocale")
val time = LunarML.assumeDiscardable field (os, "time")
val tmpname = LunarML.assumeDiscardable field (os, "tmpname")
end
structure package = struct
val cpath = LunarML.assumeDiscardable field (package, "cpath")
val loaded = LunarML.assumeDiscardable field (package, "loaded")
val loaders = LunarML.assumeDiscardable field (package, "loaders")
val loadlib = LunarML.assumeDiscardable field (package, "loadlib")
val path = LunarML.assumeDiscardable field (package, "path")
val preload = LunarML.assumeDiscardable field (package, "preload")
val searchpath = LunarML.assumeDiscardable field (package, "searchpath")
val seeall = LunarML.assumeDiscardable field (package, "seeall")
end
structure string = struct
val byte = LunarML.assumeDiscardable field (string, "byte")
val char = _Prim.Lua.Lib.string.char
val dump = LunarML.assumeDiscardable field (string, "dump")
val find = LunarML.assumeDiscardable field (string, "find")
val format = _Prim.Lua.Lib.string.format
val gmatch = LunarML.assumeDiscardable field (string, "gmatch")
val gsub = LunarML.assumeDiscardable field (string, "gsub")
val len = LunarML.assumeDiscardable field (string, "len")
val lower = LunarML.assumeDiscardable field (string, "lower")
val match = LunarML.assumeDiscardable field (string, "match")
val rep = LunarML.assumeDiscardable field (string, "rep")
val reverse = LunarML.assumeDiscardable field (string, "reverse")
val sub = LunarML.assumeDiscardable field (string, "sub")
val upper = LunarML.assumeDiscardable field (string, "upper")
end
structure table = struct
val concat = _Prim.Lua.Lib.table.concat
val insert = LunarML.assumeDiscardable field (table, "insert")
val maxn = LunarML.assumeDiscardable field (table, "maxn")
val pack = _Prim.Lua.Lib.table.pack
val remove = LunarML.assumeDiscardable field (table, "remove")
val sort = LunarML.assumeDiscardable field (table, "sort")
val unpack = _Prim.Lua.Lib.table.unpack
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
