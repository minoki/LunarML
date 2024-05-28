structure Lua : sig
              type value
              exception Error of value
              exception TypeError of string
              val sub : value * value -> value  (* t[k] *)
              val field : value * string -> value  (* t[k] *)
              val set : value * value * value -> unit  (* t[k] = v *)
              val setField : value * string * value -> unit  (* t[k] = v *)
              val global : string -> value  (* _ENV[name] *)
              val setGlobal : string * value -> unit (* _ENV[name] = v *)
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
fun // (x, y) = _primCall "Lua.//" (x, y)
fun % (x, y) = _primCall "Lua.%" (x, y)
fun pow (x, y) = _primCall "Lua.pow" (x, y)
fun negate x = _primCall "Lua.negate" (x)
fun andb (x, y) = _primCall "Lua.andb" (x, y)
fun orb (x, y) = _primCall "Lua.orb" (x, y)
fun xorb (x, y) = _primCall "Lua.xorb" (x, y)
fun notb x = _primCall "Lua.notb" (x)
fun << (x, y) = _primCall "Lua.<<" (x, y)
fun >> (x, y) = _primCall "Lua.>>" (x, y)
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
val getmetatable = _Prim.Lua.Lib.getmetatable
val io = LunarML.assumeDiscardable global "io"
val ipairs = LunarML.assumeDiscardable global "ipairs"
val load = LunarML.assumeDiscardable global "load"
val loadfile = LunarML.assumeDiscardable global "loadfile"
val math = _Prim.Lua.Lib.math
val next = LunarML.assumeDiscardable global "next"
val os = LunarML.assumeDiscardable global "os"
val package = LunarML.assumeDiscardable global "package"
val pairs = _Prim.Lua.Lib.pairs
val pcall = _Prim.Lua.Lib.pcall
val print = LunarML.assumeDiscardable global "print"
val rawequal = LunarML.assumeDiscardable global "rawequal"
val rawget = LunarML.assumeDiscardable global "rawget"
val rawlen = LunarML.assumeDiscardable global "rawlen"
val rawset = LunarML.assumeDiscardable global "rawset"
val require = LunarML.assumeDiscardable global "require"
val select = LunarML.assumeDiscardable global "select"
val setmetatable = _Prim.Lua.Lib.setmetatable
val string = _Prim.Lua.Lib.string
val table = _Prim.Lua.Lib.table
val tonumber = LunarML.assumeDiscardable global "tonumber"
val tostring = LunarML.assumeDiscardable global "tostring"
val type' = LunarML.assumeDiscardable global "type"
val utf8 = LunarML.assumeDiscardable global "utf8"
val xpcall = LunarML.assumeDiscardable global "xpcall"
structure coroutine = struct
val create = LunarML.assumeDiscardable field (coroutine, "create")
val isyieldable = LunarML.assumeDiscardable field (coroutine, "isyieldable")
val resume = LunarML.assumeDiscardable field (coroutine, "resume")
val running = LunarML.assumeDiscardable field (coroutine, "running")
val status = LunarML.assumeDiscardable field (coroutine, "status")
val wrap = LunarML.assumeDiscardable field (coroutine, "wrap")
val yield = LunarML.assumeDiscardable field (coroutine, "yield")
end
structure debug = struct
val gethook = LunarML.assumeDiscardable field (debug, "gethook")
val getinfo = LunarML.assumeDiscardable field (debug, "getinfo")
val getlocal = LunarML.assumeDiscardable field (debug, "getlocal")
val getmetatable = LunarML.assumeDiscardable field (debug, "getmetatable")
val getregistry = LunarML.assumeDiscardable field (debug, "getregistry")
val getupvalue = LunarML.assumeDiscardable field (debug, "getupvalue")
val getuservalue = LunarML.assumeDiscardable field (debug, "getuservalue")
val sethook = LunarML.assumeDiscardable field (debug, "sethook")
val setlocal = LunarML.assumeDiscardable field (debug, "setlocal")
val setmetatable = LunarML.assumeDiscardable field (debug, "setmetatable")
val setupvalue = LunarML.assumeDiscardable field (debug, "setupvalue")
val setuservalue = LunarML.assumeDiscardable field (debug, "setuservalue")
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
val ceil = LunarML.assumeDiscardable field (math, "ceil")
val cos = LunarML.assumeDiscardable field (math, "cos")
val deg = LunarML.assumeDiscardable field (math, "deg")
val exp = LunarML.assumeDiscardable field (math, "exp")
val floor = LunarML.assumeDiscardable field (math, "floor")
val fmod = LunarML.assumeDiscardable field (math, "fmod")
val huge = LunarML.assumeDiscardable field (math, "huge")
val log = LunarML.assumeDiscardable field (math, "log")
val max = LunarML.assumeDiscardable field (math, "max")
val maxinteger = _Prim.Lua.Lib.math.maxinteger
val min = LunarML.assumeDiscardable field (math, "min")
val mininteger = _Prim.Lua.Lib.math.mininteger
val modf = LunarML.assumeDiscardable field (math, "modf")
val pi = LunarML.assumeDiscardable field (math, "pi")
val rad = LunarML.assumeDiscardable field (math, "rad")
val random = LunarML.assumeDiscardable field (math, "random")
val randomseed = LunarML.assumeDiscardable field (math, "randomseed")
val sin = LunarML.assumeDiscardable field (math, "sin")
val sqrt = LunarML.assumeDiscardable field (math, "sqrt")
val tan = LunarML.assumeDiscardable field (math, "tan")
val tointeger = LunarML.assumeDiscardable field (math, "tointeger")
val type' = _Prim.Lua.Lib.math.type'
val ult = _Prim.Lua.Lib.math.ult
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
val config = LunarML.assumeDiscardable field (package, "config")
val cpath = LunarML.assumeDiscardable field (package, "cpath")
val loaded = LunarML.assumeDiscardable field (package, "loaded")
val loadlib = LunarML.assumeDiscardable field (package, "loadlib")
val path = LunarML.assumeDiscardable field (package, "path")
val preload = LunarML.assumeDiscardable field (package, "preload")
val searchers = LunarML.assumeDiscardable field (package, "searchers")
val searchpath = LunarML.assumeDiscardable field (package, "searchpath")
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
val pack = LunarML.assumeDiscardable field (string, "pack")
val packsize = LunarML.assumeDiscardable field (string, "packsize")
val rep = LunarML.assumeDiscardable field (string, "rep")
val reverse = LunarML.assumeDiscardable field (string, "reverse")
val sub = LunarML.assumeDiscardable field (string, "sub")
val unpack = LunarML.assumeDiscardable field (string, "unpack")
val upper = LunarML.assumeDiscardable field (string, "upper")
end
structure table = struct
val concat = LunarML.assumeDiscardable field (table, "concat")
val insert = LunarML.assumeDiscardable field (table, "insert")
val move = LunarML.assumeDiscardable field (table, "move")
val pack = _Prim.Lua.Lib.table.pack
val remove = LunarML.assumeDiscardable field (table, "remove")
val sort = LunarML.assumeDiscardable field (table, "sort")
val unpack = _Prim.Lua.Lib.table.unpack
end
structure utf8 = struct
val char = LunarML.assumeDiscardable field (utf8, "char")
val charpattern = LunarML.assumeDiscardable field (utf8, "charpattern")
val codepoint = LunarML.assumeDiscardable field (utf8, "codepoint")
val codes = LunarML.assumeDiscardable field (utf8, "codes")
val len = LunarML.assumeDiscardable field (utf8, "len")
val offset = LunarML.assumeDiscardable field (utf8, "offset")
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
