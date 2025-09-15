structure Lua : sig
              type value
              exception Error of value
              exception TypeError of string
              structure PrimEffect : sig
                type prim_effect
                val pure : prim_effect
                val discardable : prim_effect
                val impure : prim_effect
              end
              val sub : value * value -> value  (* t[k] *)
              val field : value * string -> value  (* t[k] *)
              val fieldWithEffect : value * string * PrimEffect.prim_effect -> value  (* t[k] *)
              val set : value * value * value -> unit  (* t[k] = v *)
              val setField : value * string * value -> unit  (* t[k] = v *)
              val global : string -> value  (* _ENV[name] *)
              val setGlobal : string * value -> unit (* _ENV[name] = v *)
              val call : value -> value vector -> value vector  (* f(args) *)
              val call0 : value -> value vector -> unit  (* f(args) *)
              val call1 : value -> value vector -> value  (* f(args) *)
              val call2 : value -> value vector -> value * value  (* f(args) *)
              val call3 : value -> value vector -> value * value * value  (* f(args) *)
              val call4 : value -> value vector -> value * value * value * value  (* f(args) *)
              val call5 : value -> value vector -> value * value * value * value * value  (* f(args) *)
              val call6 : value -> value vector -> value * value * value * value * value * value  (* f(args) *)
              val call7 : value -> value vector -> value * value * value * value * value * value * value  (* f(args) *)
              val call8 : value -> value vector -> value * value * value * value * value * value * value * value  (* f(args) *)
              val call9 : value -> value vector -> value * value * value * value * value * value * value * value * value  (* f(args) *)
              val callWithEffect : PrimEffect.prim_effect -> value -> value vector -> value vector  (* f(args) *)
              val call1WithEffect : PrimEffect.prim_effect -> value -> value vector -> value  (* f(args) *)
              val call2WithEffect : PrimEffect.prim_effect -> value -> value vector -> value * value  (* f(args) *)
              val call3WithEffect : PrimEffect.prim_effect -> value -> value vector -> value * value * value  (* f(args) *)
              val call4WithEffect : PrimEffect.prim_effect -> value -> value vector -> value * value * value * value  (* f(args) *)
              val call5WithEffect : PrimEffect.prim_effect -> value -> value vector -> value * value * value * value * value  (* f(args) *)
              val call6WithEffect : PrimEffect.prim_effect -> value -> value vector -> value * value * value * value * value * value  (* f(args) *)
              val call7WithEffect : PrimEffect.prim_effect -> value -> value vector -> value * value * value * value * value * value * value  (* f(args) *)
              val call8WithEffect : PrimEffect.prim_effect -> value -> value vector -> value * value * value * value * value * value * value * value  (* f(args) *)
              val call9WithEffect : PrimEffect.prim_effect -> value -> value vector -> value * value * value * value * value * value * value * value * value  (* f(args) *)
              val method : value * string -> value vector -> value vector  (* f:name(args) *)
              val method0 : value * string -> value vector -> unit  (* f:name(args) *)
              val method1 : value * string -> value vector -> value  (* f:name(args) *)
              val method2 : value * string -> value vector -> value * value  (* f:name(args) *)
              val method3 : value * string -> value vector -> value * value * value  (* f:name(args) *)
              val method4 : value * string -> value vector -> value * value * value * value  (* f:name(args) *)
              val method5 : value * string -> value vector -> value * value * value * value * value  (* f:name(args) *)
              val method6 : value * string -> value vector -> value * value * value * value * value * value  (* f:name(args) *)
              val method7 : value * string -> value vector -> value * value * value * value * value * value * value  (* f:name(args) *)
              val method8 : value * string -> value vector -> value * value * value * value * value * value * value * value  (* f:name(args) *)
              val method9 : value * string -> value vector -> value * value * value * value * value * value * value * value * value  (* f:name(args) *)
              val methodWithEffect : PrimEffect.prim_effect -> value * string -> value vector -> value vector  (* f:name(args) *)
              val method1WithEffect : PrimEffect.prim_effect -> value * string -> value vector -> value  (* f:name(args) *)
              val method2WithEffect : PrimEffect.prim_effect -> value * string -> value vector -> value * value  (* f:name(args) *)
              val method3WithEffect : PrimEffect.prim_effect -> value * string -> value vector -> value * value * value  (* f:name(args) *)
              val method4WithEffect : PrimEffect.prim_effect -> value * string -> value vector -> value * value * value * value  (* f:name(args) *)
              val method5WithEffect : PrimEffect.prim_effect -> value * string -> value vector -> value * value * value * value * value  (* f:name(args) *)
              val method6WithEffect : PrimEffect.prim_effect -> value * string -> value vector -> value * value * value * value * value * value  (* f:name(args) *)
              val method7WithEffect : PrimEffect.prim_effect -> value * string -> value vector -> value * value * value * value * value * value * value  (* f:name(args) *)
              val method8WithEffect : PrimEffect.prim_effect -> value * string -> value vector -> value * value * value * value * value * value * value * value  (* f:name(args) *)
              val method9WithEffect : PrimEffect.prim_effect -> value * string -> value vector -> value * value * value * value * value * value * value * value * value  (* f:name(args) *)
              val NIL : value  (* Lua nil *)
              val isNil : value -> bool  (* x == nil *)
              val isFalsy : value -> bool  (* not x *)
              val isTruthy : value -> bool  (* not (not x) *)
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
              val // : value * value -> value
              val % : value * value -> value
              val pow : value * value -> value  (* x ^ y *)
              val negate : value -> value  (* unary minus *)
              val andb : value * value -> value  (* x & y *)
              val orb : value * value -> value  (* x | y *)
              val xorb : value * value -> value  (* x ~ y *)
              val notb : value -> value  (* ~ x *)
              val << : value * value -> value
              val >> : value * value -> value
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
                            val GLOBAL : value (* _G *)
                            val VERSION : value (* _VERSION *)
                            val assert : value
                            val collectgarbage : value
                            val coroutine : value
                            val debug : value
                            val dofile : value
                            val error : value
                            val getmetatable : value
                            val io : value
                            val ipairs : value
                            val load : value
                            val loadfile : value
                            val math : value
                            val next : value
                            val os : value
                            val package : value
                            val pairs : value
                            val pcall : value
                            val print : value
                            val rawequal : value
                            val rawget : value
                            val rawlen : value
                            val rawset : value
                            val require : value
                            val select : value
                            val setmetatable : value
                            val string : value
                            val table : value
                            val tonumber : value
                            val tostring : value
                            val type' : value
                            val utf8 : value
                            (* val warn : value (* Lua 5.4 *) *)
                            val xpcall : value
                            structure coroutine : sig
                                          (* val close : value (* Lua 5.4 *) *)
                                          val create : value
                                          val isyieldable : value
                                          val resume : value
                                          val running : value
                                          val status : value
                                          val wrap : value
                                          val yield : value
                                      end
                            structure debug : sig
                                          val debug : value
                                          val gethook : value
                                          val getinfo : value
                                          val getlocal : value
                                          val getmetatable : value
                                          val getregistry : value
                                          val getupvalue : value
                                          val getuservalue : value
                                          val sethook : value
                                          val setlocal : value
                                          val setmetatable : value
                                          val setupvalue : value
                                          val setuservalue : value
                                          val traceback : value
                                          val upvalueid : value
                                          val upvaluejoin : value
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
                                          val ceil : value
                                          val cos : value
                                          val deg : value
                                          val exp : value
                                          val floor : value
                                          val fmod : value
                                          val huge : value
                                          val log : value
                                          val max : value
                                          val maxinteger : value
                                          val min : value
                                          val mininteger : value
                                          val modf : value
                                          val pi : value
                                          val rad : value
                                          val random : value
                                          val randomseed : value
                                          val sin : value
                                          val sqrt : value
                                          val tan : value
                                          val tointeger : value
                                          val type' : value
                                          val ult : value
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
                                          val config : value
                                          val cpath : value
                                          val loaded : value
                                          val loadlib : value
                                          val path : value
                                          val preload : value
                                          val searchers : value
                                          val searchpath : value
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
                                          val pack : value
                                          val packsize : value
                                          val rep : value
                                          val reverse : value
                                          val sub : value
                                          val unpack : value
                                          val upper : value
                                      end
                            structure table : sig
                                          val concat : value
                                          val insert : value
                                          val move : value
                                          val pack : value
                                          val remove : value
                                          val sort : value
                                          val unpack : value
                                      end
                            structure utf8 : sig
                                          val char : value
                                          val charpattern : value
                                          val codepoint : value
                                          val codes : value
                                          val len : value
                                          val offset : value
                                      end
                            val lfs : value option (* LuaFileSystem *)
                        end
          end = struct
type value = _Prim.Lua.value
exception Error = _Prim.Lua.Error
structure PrimEffect = struct
type prim_effect = _Prim.PrimEffect.prim_effect
val pure = _Prim.PrimEffect.pure
val discardable = _Prim.PrimEffect.discardable
val impure = _Prim.PrimEffect.impure
end
fun global name = _primCall "Lua.global" (name)
fun setGlobal (name, value) = _primCall "Lua.setGlobal" (name, value)
fun call f args = _primCall "Lua.call" (f, args, _Prim.PrimEffect.impure)
fun call0 f args = (_primCall "Lua.call" (f, args, _Prim.PrimEffect.impure); ())
fun call1 f args = _primCall "Lua.call1" (f, args, _Prim.PrimEffect.impure)
fun call2 f args = _primCall "Lua.call2" (f, args, _Prim.PrimEffect.impure)
fun call3 f args = _primCall "Lua.call3" (f, args, _Prim.PrimEffect.impure)
fun call4 f args = _primCall "Lua.call4" (f, args, _Prim.PrimEffect.impure)
fun call5 f args = _primCall "Lua.call5" (f, args, _Prim.PrimEffect.impure)
fun call6 f args = _primCall "Lua.call6" (f, args, _Prim.PrimEffect.impure)
fun call7 f args = _primCall "Lua.call7" (f, args, _Prim.PrimEffect.impure)
fun call8 f args = _primCall "Lua.call8" (f, args, _Prim.PrimEffect.impure)
fun call9 f args = _primCall "Lua.call9" (f, args, _Prim.PrimEffect.impure)
fun callWithEffect e f args = _primCall "Lua.call" (f, args, e)
fun call1WithEffect e f args = _primCall "Lua.call1" (f, args, e)
fun call2WithEffect e f args = _primCall "Lua.call2" (f, args, e)
fun call3WithEffect e f args = _primCall "Lua.call3" (f, args, e)
fun call4WithEffect e f args = _primCall "Lua.call4" (f, args, e)
fun call5WithEffect e f args = _primCall "Lua.call5" (f, args, e)
fun call6WithEffect e f args = _primCall "Lua.call6" (f, args, e)
fun call7WithEffect e f args = _primCall "Lua.call7" (f, args, e)
fun call8WithEffect e f args = _primCall "Lua.call8" (f, args, e)
fun call9WithEffect e f args = _primCall "Lua.call9" (f, args, e)
fun method (obj, name) args = _primCall "Lua.method" (obj, name, args, _Prim.PrimEffect.impure)
fun method0 (obj, name) args = (_primCall "Lua.method" (obj, name, args, _Prim.PrimEffect.impure); ())
fun method1 (obj, name) args = _primCall "Lua.method1" (obj, name, args, _Prim.PrimEffect.impure)
fun method2 (obj, name) args = _primCall "Lua.method2" (obj, name, args, _Prim.PrimEffect.impure)
fun method3 (obj, name) args = _primCall "Lua.method3" (obj, name, args, _Prim.PrimEffect.impure)
fun method4 (obj, name) args = _primCall "Lua.method4" (obj, name, args, _Prim.PrimEffect.impure)
fun method5 (obj, name) args = _primCall "Lua.method5" (obj, name, args, _Prim.PrimEffect.impure)
fun method6 (obj, name) args = _primCall "Lua.method6" (obj, name, args, _Prim.PrimEffect.impure)
fun method7 (obj, name) args = _primCall "Lua.method7" (obj, name, args, _Prim.PrimEffect.impure)
fun method8 (obj, name) args = _primCall "Lua.method8" (obj, name, args, _Prim.PrimEffect.impure)
fun method9 (obj, name) args = _primCall "Lua.method9" (obj, name, args, _Prim.PrimEffect.impure)
fun methodWithEffect e (obj, name) args = _primCall "Lua.method" (obj, name, args, e)
fun method1WithEffect e (obj, name) args = _primCall "Lua.method1" (obj, name, args, e)
fun method2WithEffect e (obj, name) args = _primCall "Lua.method2" (obj, name, args, e)
fun method3WithEffect e (obj, name) args = _primCall "Lua.method3" (obj, name, args, e)
fun method4WithEffect e (obj, name) args = _primCall "Lua.method4" (obj, name, args, e)
fun method5WithEffect e (obj, name) args = _primCall "Lua.method5" (obj, name, args, e)
fun method6WithEffect e (obj, name) args = _primCall "Lua.method6" (obj, name, args, e)
fun method7WithEffect e (obj, name) args = _primCall "Lua.method7" (obj, name, args, e)
fun method8WithEffect e (obj, name) args = _primCall "Lua.method8" (obj, name, args, e)
fun method9WithEffect e (obj, name) args = _primCall "Lua.method9" (obj, name, args, e)
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
fun sub (t, k) = _primCall "Lua.sub" (t, k, _Prim.PrimEffect.impure)
fun field (t : value, name : string) = sub (t, fromString name)
fun fieldWithEffect (t : value, name : string, e) = _primCall "Lua.sub" (t, fromString name, e)
fun set (t, k, v) = _primCall "Lua.set" (t, k, v)
fun setField (t, name, v) = _primCall "Lua.set" (t, fromString name, v)
fun isNil x = _primCall "Lua.isNil" (x)
fun isFalsy x = _primCall "Lua.isFalsy" (x)
fun isTruthy x = not (isFalsy x)
fun x + y = _primCall "Lua.+" (x, y, _Prim.PrimEffect.impure)
fun x - y = _primCall "Lua.-" (x, y, _Prim.PrimEffect.impure)
fun x * y = _primCall "Lua.*" (x, y, _Prim.PrimEffect.impure)
fun x / y = _primCall "Lua./" (x, y, _Prim.PrimEffect.impure)
fun // (x, y) = _primCall "Lua.//" (x, y, _Prim.PrimEffect.impure)
fun % (x, y) = _primCall "Lua.%" (x, y, _Prim.PrimEffect.impure)
fun pow (x, y) = _primCall "Lua.pow" (x, y, _Prim.PrimEffect.impure)
fun negate x = _primCall "Lua.negate" (x, _Prim.PrimEffect.impure)
fun andb (x, y) = _primCall "Lua.andb" (x, y, _Prim.PrimEffect.impure)
fun orb (x, y) = _primCall "Lua.orb" (x, y, _Prim.PrimEffect.impure)
fun xorb (x, y) = _primCall "Lua.xorb" (x, y, _Prim.PrimEffect.impure)
fun notb x = _primCall "Lua.notb" (x, _Prim.PrimEffect.impure)
fun << (x, y) = _primCall "Lua.<<" (x, y, _Prim.PrimEffect.impure)
fun >> (x, y) = _primCall "Lua.>>" (x, y, _Prim.PrimEffect.impure)
fun == (x, y) = _primCall "Lua.==" (x, y, _Prim.PrimEffect.impure)
fun ~= (x, y) = _primCall "Lua.~=" (x, y, _Prim.PrimEffect.impure)
fun x < y = _primCall "Lua.<" (x, y, _Prim.PrimEffect.impure)
fun x > y = _primCall "Lua.>" (x, y, _Prim.PrimEffect.impure)
fun x <= y = _primCall "Lua.<=" (x, y, _Prim.PrimEffect.impure)
fun x >= y = _primCall "Lua.>=" (x, y, _Prim.PrimEffect.impure)
fun concat (x, y) = _primCall "Lua.concat" (x, y, _Prim.PrimEffect.impure)
fun length x = _primCall "Lua.length" (x, _Prim.PrimEffect.impure)
structure Lib = struct
val GLOBAL = global "_G"
val VERSION = global "_VERSION"
val assert = _Prim.Lua.Lib.assert
val collectgarbage = global "collectgarbage"
val coroutine = global "coroutine"
val debug = global "debug"
val dofile = global "dofile"
val error = _Prim.Lua.Lib.error
val getmetatable = _Prim.Lua.Lib.getmetatable
val io = global "io"
val ipairs = global "ipairs"
val load = global "load"
val loadfile = global "loadfile"
val math = _Prim.Lua.Lib.math
val next = global "next"
val os = global "os"
val package = global "package"
val pairs = _Prim.Lua.Lib.pairs
val pcall = _Prim.Lua.Lib.pcall
val print = global "print"
val rawequal = global "rawequal"
val rawget = global "rawget"
val rawlen = global "rawlen"
val rawset = global "rawset"
val require = global "require"
val select = global "select"
val setmetatable = _Prim.Lua.Lib.setmetatable
val string = _Prim.Lua.Lib.string
val table = _Prim.Lua.Lib.table
val tonumber = global "tonumber"
val tostring = global "tostring"
val type' = global "type"
val utf8 = global "utf8"
val xpcall = global "xpcall"
structure coroutine = struct
val create = fieldWithEffect (coroutine, "create", _Prim.PrimEffect.discardable)
val isyieldable = fieldWithEffect (coroutine, "isyieldable", _Prim.PrimEffect.discardable)
val resume = fieldWithEffect (coroutine, "resume", _Prim.PrimEffect.discardable)
val running = fieldWithEffect (coroutine, "running", _Prim.PrimEffect.discardable)
val status = fieldWithEffect (coroutine, "status", _Prim.PrimEffect.discardable)
val wrap = fieldWithEffect (coroutine, "wrap", _Prim.PrimEffect.discardable)
val yield = fieldWithEffect (coroutine, "yield", _Prim.PrimEffect.discardable)
end
structure debug = struct
val gethook = fieldWithEffect (debug, "gethook", _Prim.PrimEffect.discardable)
val getinfo = fieldWithEffect (debug, "getinfo", _Prim.PrimEffect.discardable)
val getlocal = fieldWithEffect (debug, "getlocal", _Prim.PrimEffect.discardable)
val getmetatable = fieldWithEffect (debug, "getmetatable", _Prim.PrimEffect.discardable)
val getregistry = fieldWithEffect (debug, "getregistry", _Prim.PrimEffect.discardable)
val getupvalue = fieldWithEffect (debug, "getupvalue", _Prim.PrimEffect.discardable)
val getuservalue = fieldWithEffect (debug, "getuservalue", _Prim.PrimEffect.discardable)
val sethook = fieldWithEffect (debug, "sethook", _Prim.PrimEffect.discardable)
val setlocal = fieldWithEffect (debug, "setlocal", _Prim.PrimEffect.discardable)
val setmetatable = fieldWithEffect (debug, "setmetatable", _Prim.PrimEffect.discardable)
val setupvalue = fieldWithEffect (debug, "setupvalue", _Prim.PrimEffect.discardable)
val setuservalue = fieldWithEffect (debug, "setuservalue", _Prim.PrimEffect.discardable)
val traceback = fieldWithEffect (debug, "traceback", _Prim.PrimEffect.discardable)
val upvalueid = fieldWithEffect (debug, "upvalueid", _Prim.PrimEffect.discardable)
val upvaluejoin = fieldWithEffect (debug, "upvaluejoin", _Prim.PrimEffect.discardable)
val debug = fieldWithEffect (debug, "debug", _Prim.PrimEffect.discardable)
end
structure io = struct
val close = fieldWithEffect (io, "close", _Prim.PrimEffect.discardable)
val flush = fieldWithEffect (io, "flush", _Prim.PrimEffect.discardable)
val input = fieldWithEffect (io, "input", _Prim.PrimEffect.discardable)
val lines = fieldWithEffect (io, "lines", _Prim.PrimEffect.discardable)
val open' = fieldWithEffect (io, "open", _Prim.PrimEffect.discardable)
val output = fieldWithEffect (io, "output", _Prim.PrimEffect.discardable)
val popen = fieldWithEffect (io, "popen", _Prim.PrimEffect.discardable)
val read = fieldWithEffect (io, "read", _Prim.PrimEffect.discardable)
val stderr = fieldWithEffect (io, "stderr", _Prim.PrimEffect.discardable)
val stdin = fieldWithEffect (io, "stdin", _Prim.PrimEffect.discardable)
val stdout = fieldWithEffect (io, "stdout", _Prim.PrimEffect.discardable)
val tmpfile = fieldWithEffect (io, "tmpfile", _Prim.PrimEffect.discardable)
val type' = fieldWithEffect (io, "type", _Prim.PrimEffect.discardable)
val write = fieldWithEffect (io, "write", _Prim.PrimEffect.discardable)
end
structure math = struct
val abs = _Prim.Lua.Lib.math.abs
val acos = fieldWithEffect (math, "acos", _Prim.PrimEffect.discardable)
val asin = fieldWithEffect (math, "asin", _Prim.PrimEffect.discardable)
val atan = fieldWithEffect (math, "atan", _Prim.PrimEffect.discardable)
val ceil = fieldWithEffect (math, "ceil", _Prim.PrimEffect.discardable)
val cos = fieldWithEffect (math, "cos", _Prim.PrimEffect.discardable)
val deg = fieldWithEffect (math, "deg", _Prim.PrimEffect.discardable)
val exp = fieldWithEffect (math, "exp", _Prim.PrimEffect.discardable)
val floor = fieldWithEffect (math, "floor", _Prim.PrimEffect.discardable)
val fmod = fieldWithEffect (math, "fmod", _Prim.PrimEffect.discardable)
val huge = fieldWithEffect (math, "huge", _Prim.PrimEffect.discardable)
val log = fieldWithEffect (math, "log", _Prim.PrimEffect.discardable)
val max = fieldWithEffect (math, "max", _Prim.PrimEffect.discardable)
val maxinteger = _Prim.Lua.Lib.math.maxinteger
val min = fieldWithEffect (math, "min", _Prim.PrimEffect.discardable)
val mininteger = _Prim.Lua.Lib.math.mininteger
val modf = fieldWithEffect (math, "modf", _Prim.PrimEffect.discardable)
val pi = fieldWithEffect (math, "pi", _Prim.PrimEffect.discardable)
val rad = fieldWithEffect (math, "rad", _Prim.PrimEffect.discardable)
val random = fieldWithEffect (math, "random", _Prim.PrimEffect.discardable)
val randomseed = fieldWithEffect (math, "randomseed", _Prim.PrimEffect.discardable)
val sin = fieldWithEffect (math, "sin", _Prim.PrimEffect.discardable)
val sqrt = fieldWithEffect (math, "sqrt", _Prim.PrimEffect.discardable)
val tan = fieldWithEffect (math, "tan", _Prim.PrimEffect.discardable)
val tointeger = fieldWithEffect (math, "tointeger", _Prim.PrimEffect.discardable)
val type' = _Prim.Lua.Lib.math.type'
val ult = _Prim.Lua.Lib.math.ult
end
structure os = struct
val clock = fieldWithEffect (os, "clock", _Prim.PrimEffect.discardable)
val date = fieldWithEffect (os, "date", _Prim.PrimEffect.discardable)
val difftime = fieldWithEffect (os, "difftime", _Prim.PrimEffect.discardable)
val execute = fieldWithEffect (os, "execute", _Prim.PrimEffect.discardable)
val exit = fieldWithEffect (os, "exit", _Prim.PrimEffect.discardable)
val getenv = fieldWithEffect (os, "getenv", _Prim.PrimEffect.discardable)
val remove = fieldWithEffect (os, "remove", _Prim.PrimEffect.discardable)
val rename = fieldWithEffect (os, "rename", _Prim.PrimEffect.discardable)
val setlocale = fieldWithEffect (os, "setlocale", _Prim.PrimEffect.discardable)
val time = fieldWithEffect (os, "time", _Prim.PrimEffect.discardable)
val tmpname = fieldWithEffect (os, "tmpname", _Prim.PrimEffect.discardable)
end
structure package = struct
val config = fieldWithEffect (package, "config", _Prim.PrimEffect.discardable)
val cpath = fieldWithEffect (package, "cpath", _Prim.PrimEffect.discardable)
val loaded = fieldWithEffect (package, "loaded", _Prim.PrimEffect.discardable)
val loadlib = fieldWithEffect (package, "loadlib", _Prim.PrimEffect.discardable)
val path = fieldWithEffect (package, "path", _Prim.PrimEffect.discardable)
val preload = fieldWithEffect (package, "preload", _Prim.PrimEffect.discardable)
val searchers = fieldWithEffect (package, "searchers", _Prim.PrimEffect.discardable)
val searchpath = fieldWithEffect (package, "searchpath", _Prim.PrimEffect.discardable)
end
structure string = struct
val byte = fieldWithEffect (string, "byte", _Prim.PrimEffect.discardable)
val char = _Prim.Lua.Lib.string.char
val dump = fieldWithEffect (string, "dump", _Prim.PrimEffect.discardable)
val find = fieldWithEffect (string, "find", _Prim.PrimEffect.discardable)
val format = _Prim.Lua.Lib.string.format
val gmatch = fieldWithEffect (string, "gmatch", _Prim.PrimEffect.discardable)
val gsub = fieldWithEffect (string, "gsub", _Prim.PrimEffect.discardable)
val len = fieldWithEffect (string, "len", _Prim.PrimEffect.discardable)
val lower = fieldWithEffect (string, "lower", _Prim.PrimEffect.discardable)
val match = fieldWithEffect (string, "match", _Prim.PrimEffect.discardable)
val pack = fieldWithEffect (string, "pack", _Prim.PrimEffect.discardable)
val packsize = fieldWithEffect (string, "packsize", _Prim.PrimEffect.discardable)
val rep = fieldWithEffect (string, "rep", _Prim.PrimEffect.discardable)
val reverse = fieldWithEffect (string, "reverse", _Prim.PrimEffect.discardable)
val sub = fieldWithEffect (string, "sub", _Prim.PrimEffect.discardable)
val unpack = fieldWithEffect (string, "unpack", _Prim.PrimEffect.discardable)
val upper = fieldWithEffect (string, "upper", _Prim.PrimEffect.discardable)
end
structure table = struct
val concat = _Prim.Lua.Lib.table.concat
val insert = fieldWithEffect (table, "insert", _Prim.PrimEffect.discardable)
val move = fieldWithEffect (table, "move", _Prim.PrimEffect.discardable)
val pack = _Prim.Lua.Lib.table.pack
val remove = fieldWithEffect (table, "remove", _Prim.PrimEffect.discardable)
val sort = fieldWithEffect (table, "sort", _Prim.PrimEffect.discardable)
val unpack = _Prim.Lua.Lib.table.unpack
end
structure utf8 = struct
val char = fieldWithEffect (utf8, "char", _Prim.PrimEffect.discardable)
val charpattern = fieldWithEffect (utf8, "charpattern", _Prim.PrimEffect.discardable)
val codepoint = fieldWithEffect (utf8, "codepoint", _Prim.PrimEffect.discardable)
val codes = fieldWithEffect (utf8, "codes", _Prim.PrimEffect.discardable)
val len = fieldWithEffect (utf8, "len", _Prim.PrimEffect.discardable)
val offset = fieldWithEffect (utf8, "offset", _Prim.PrimEffect.discardable)
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
fun typeof x : string = unsafeFromValue (call1 Lib.type' #[x])
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
fun checkInt x : int = let val t = call1 Lib.math.type' #[x]
                       in if == (t, fromString "integer") then
                              unsafeFromValue x
                          else
                              raise TypeError (String.^ ("expected an integer, but got ", typeof x))
                       end
fun checkWord x : word = let val t = call1 Lib.math.type' #[x]
                         in if == (t, fromString "integer") then
                                unsafeFromValue x
                            else
                                raise TypeError (String.^ ("expected an integer, but got ", typeof x))
                         end
fun checkReal x : real = let val t = call1 Lib.math.type' #[x]
                         in if == (t, fromString "float") then
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
end;
