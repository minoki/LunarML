structure Bool = struct
fun not x = _primCall "Bool.not" (x)
end;
_equality bool = fn (x, y) => _primCall "Bool.=" (x, y);

type unit = {}
datatype 'a option = NONE | SOME of 'a;

structure Int = struct
type int = int
val ~ = _Prim.Int.~
val abs = _Prim.Int.abs
fun x + y = _primCall "call2" (_Prim.Int.+, x, y)
fun x - y = _primCall "call2" (_Prim.Int.-, x, y)
fun x * y = _primCall "call2" (_Prim.Int.*, x, y)
fun x div y = _primCall "call2" (_Prim.Int.div, x, y)
fun x mod y = _primCall "call2" (_Prim.Int.mod, x, y)
fun x < y = _primCall "Int.<" (x, y)
fun x <= y = _primCall "Int.<=" (x, y)
fun x > y = _primCall "Int.>" (x, y)
fun x >= y = _primCall "Int.>=" (x, y)
fun fromInt (x : int) = x
end
_equality int = fn (x, y) => _primCall "Int.=" (x, y);
_overload "Int" [int] { + = Int.+
                      , - = Int.-
                      , * = Int.*
                      , div = Int.div
                      , mod = Int.mod
                      , ~ = Int.~
                      , abs = Int.abs
                      , < = Int.<
                      , <= = Int.<=
                      , > = Int.>
                      , >= = Int.>=
                      , fromInt = Int.fromInt
                      };

structure Word = struct
type word = word
fun ~ x = _primCall "Word.~" (x)
fun x + y = _primCall "Word.+" (x, y)
fun x - y = _primCall "Word.-" (x, y)
fun x * y = _primCall "Word.*" (x, y)
fun x div y = _primCall "call2" (_Prim.Word.div, x, y)
fun x mod y = _primCall "call2" (_Prim.Word.mod, x, y)
fun x < y = _primCall "Word.<" (x, y)
fun x <= y = _primCall "Word.<=" (x, y)
fun x > y = _primCall "Word.>" (x, y)
fun x >= y = _primCall "Word.>=" (x, y)
end
local
fun fromWord (x : word) = x
in
_equality word = fn (x, y) => _primCall "Word.=" (x, y);
_overload "Word" [word] { + = Word.+
                        , - = Word.-
                        , * = Word.*
                        , div = Word.div
                        , mod = Word.mod
                        , ~ = Word.~
                        , < = Word.<
                        , <= = Word.<=
                        , > = Word.>
                        , >= = Word.>=
                        , fromWord = fromWord
                        };
end

structure Real = struct
type real = real
fun ~ x = _primCall "Real.~" (x)
val abs = _Prim.Real.abs
fun x + y = _primCall "Real.+" (x, y)
fun x - y = _primCall "Real.-" (x, y)
fun x * y = _primCall "Real.*" (x, y)
fun x / y = _primCall "Real./" (x, y)
fun x < y = _primCall "Real.<" (x, y)
fun x <= y = _primCall "Real.<=" (x, y)
fun x > y = _primCall "Real.>" (x, y)
fun x >= y = _primCall "Real.>=" (x, y)
end
_overload "Real" [real] { + = Real.+
                        , - = Real.-
                        , * = Real.*
                        , / = Real./
                        , abs = Real.abs
                        , ~ = Real.~
                        , < = Real.<
                        , <= = Real.<=
                        , > = Real.>
                        , >= = Real.>=
                        };

structure Char = struct
type char = char
fun x < y = _primCall "Char.<" (x, y)
fun x <= y = _primCall "Char.<=" (x, y)
fun x > y = _primCall "Char.>" (x, y)
fun x >= y = _primCall "Char.>=" (x, y)
end
_equality char = fn (x, y) => _primCall "Char.=" (x, y);
_overload "Char" [char] { < = Char.<
                        , <= = Char.<=
                        , > = Char.>
                        , >= = Char.>=
                        };

structure WideChar = struct
type char = _Prim.WideChar.char
fun x < y = _primCall "WideChar.<" (x, y)
fun x <= y = _primCall "WideChar.<=" (x, y)
fun x > y = _primCall "WideChar.>" (x, y)
fun x >= y = _primCall "WideChar.>=" (x, y)
end
_equality WideChar.char = fn (x, y) => _primCall "WideChar.=" (x, y);
_overload "Char" [WideChar.char] { < = WideChar.<
                                 , <= = WideChar.<=
                                 , > = WideChar.>
                                 , >= = WideChar.>=
                                 };

structure String = struct
type string = string
fun x < y = _primCall "String.<" (x, y)
fun x <= y = Bool.not (y < x)
fun x > y = y < x
fun x >= y = Bool.not (x < y)
fun x ^ y = _primCall "String.^" (x, y)
fun size x = _primCall "String.size" (x)
fun str x = _primCall "String.str" (x)
end
_equality string = fn (x, y) => _primCall "String.=" (x, y);
_overload "String" [string] { < = String.<
                            , <= = String.<=
                            , > = String.>
                            , >= = String.>=
                            };

structure WideString = struct
type string = _Prim.WideString.string
type char = WideChar.char
fun x < y = _primCall "WideString.<" (x, y)
fun x <= y = _primCall "WideString.<=" (x, y)
fun x > y = _primCall "WideString.>" (x, y)
fun x >= y = _primCall "WideString.>=" (x, y)
fun x ^ y = _primCall "WideString.^" (x, y)
fun size x = _primCall "WideString.size" (x)
fun str (x : char) : string = _primCall "Unsafe.cast" (x)
end
_equality WideString.string = fn (x, y) => _primCall "WideString.=" (x, y);
_overload "String" [WideString.string] { < = WideString.<
                                       , <= = WideString.<=
                                       , > = WideString.>
                                       , >= = WideString.>=
                                       };

structure Vector = struct
fun length vec = _primCall "Vector.length" (vec)
fun sub (vec, i) = if i < 0 orelse length vec <= i then
                       raise Subscript
                   else
                       Unsafe.Vector.sub (vec, i)
val tabulate = _Prim.Vector.tabulate
val concat = _Prim.Vector.concat
end
_equality ''a vector = fn (x, y) => _primCall "Vector.=" (op = : ''a * ''a -> bool, x, y);

structure LunarML : sig
              val assumePure : 'a -> 'a
              val assumeDiscardable : 'a -> 'a
          end = struct
val assumePure = _Prim.assumePure
val assumeDiscardable = _Prim.assumeDiscardable
end

structure JavaScript : sig
              type value
              val sub : value * value -> value
              val field : value * WideString.string -> value
              val set : value * value * value -> unit
              val global : WideString.string -> value
              val call : value -> value vector -> value
              val new : value -> value vector -> value
              val method : value * WideString.string -> value vector -> value
              val fromBool : bool -> value
              val fromInt : int -> value
              val fromWord : word -> value
              val fromReal : real -> value
              val fromWideString : WideString.string -> value
              val unsafeToValue : 'a -> value
              val unsafeFromValue : value -> 'a
              val === : value * value -> bool
              val !== : value * value -> bool
              val < : value * value -> bool
              val > : value * value -> bool
              val <= : value * value -> bool
              val >= : value * value -> bool
              val + : value * value -> value
              val - : value * value -> value
              val * : value * value -> value
              val / : value * value -> value
              val % : value * value -> value
              val negate : value -> value
              val andb : value * value -> value
              val orb : value * value -> value
              val xorb : value * value -> value
              val notb : value -> value
              val << : value * value -> value
              val >> : value * value -> value
              val >>> : value * value -> value
              val ** : value * value -> value
              val isFalsy : value -> bool
              val typeof : value -> WideString.string
              val newObject : unit -> value
              val encodeUtf8 : WideString.string -> string
              val decodeUtf8 : string -> WideString.string
              val toInt32 : value -> int
              val toUint32 : value -> word
              val require : value (* Node.js *)
              structure Lib : sig
                            val Number : value
                            structure Number : sig
                                          val isFinite : value
                                          val isNaN : value
                                          val POSITIVE_INFINITY : value
                                          val NEGATIVE_INFINITY : value
                                          val MIN_VALUE : value
                                          val MAX_VALUE : value
                                          val NaN : value
                                      end
                            val Math : value
                            structure Math : sig
                                          val E : value
                                          val PI : value
                                          val abs : value
                                          val acos : value
                                          val acosh : value
                                          val asin : value
                                          val asinh : value
                                          val atan : value
                                          val atanh : value
                                          val atan2 : value
                                          val cbrt : value
                                          val ceil : value
                                          val clz32 : value
                                          val cos : value
                                          val cosh : value
                                          val exp : value
                                          val expm1 : value
                                          val floor : value
                                          val fround : value
                                          val hypot : value
                                          val imul : value
                                          val log : value
                                          val log1p : value
                                          val log10 : value
                                          val log2 : value
                                          val max : value
                                          val min : value
                                          val pow : value
                                          val random : value
                                          val round : value
                                          val sign : value
                                          val sin : value
                                          val sinh : value
                                          val sqrt : value
                                          val tan : value
                                          val tanh : value
                                          val trunc : value
                                      end
                            val BigInt : value
                            structure BigInt : sig
                                          val asIntN : value
                                          val asUintN : value
                                      end
                            val Uint8Array : value
                        end
          end = struct
type value = _Prim.JavaScript.value
val call = _Prim.JavaScript.call
val new = _Prim.JavaScript.new
val method = _Prim.JavaScript.method
val encodeUtf8 = _Prim.JavaScript.encodeUtf8
val decodeUtf8 = _Prim.JavaScript.decodeUtf8
val require = _Prim.JavaScript.require
fun unsafeToValue x : value = _primCall "Unsafe.cast" (x)
fun unsafeFromValue (x : value) = _primCall "Unsafe.cast" (x)
val fromBool : bool -> value = unsafeToValue
val fromInt : int -> value = unsafeToValue
val fromWord : word -> value = unsafeToValue
val fromReal : real -> value = unsafeToValue
val fromWideString : WideString.string -> value = unsafeToValue
fun sub (obj, key) = _primCall "JavaScript.sub" (obj, key)
fun field (obj, key : WideString.string) = _primCall "JavaScript.sub" (obj, _primCall "Unsafe.cast" (key))
fun set (obj, key, value) = _primCall "JavaScript.set" (obj, key, value)
fun global name = _primCall "JavaScript.global" (name)
fun isFalsy x = _primCall "JavaScript.isFalsy" (x)
fun x + y = _primCall "JavaScript.+" (x, y)
fun x - y = _primCall "JavaScript.-" (x, y)
fun x * y = _primCall "JavaScript.*" (x, y)
fun x / y = _primCall "JavaScript./" (x, y)
fun % (x, y) = _primCall "JavaScript.%" (x, y)
fun negate x = _primCall "JavaScript.negate" (x)
fun andb (x, y) = _primCall "JavaScript.andb" (x, y)
fun orb (x, y) = _primCall "JavaScript.orb" (x, y)
fun xorb (x, y) = _primCall "JavaScript.xorb" (x, y)
fun notb x = _primCall "JavaScript.notb" (x)
fun << (x, y) = _primCall "JavaScript.<<" (x, y)
fun >> (x, y) = _primCall "JavaScript.>>" (x, y)
fun >>> (x, y) = _primCall "JavaScript.>>>" (x, y)
fun === (x, y) = _primCall "JavaScript.===" (x, y)
fun !== (x, y) = _primCall "JavaScript.!==" (x, y)
fun x < y = _primCall "JavaScript.<" (x, y)
fun x > y = _primCall "JavaScript.>" (x, y)
fun x <= y = _primCall "JavaScript.<=" (x, y)
fun x >= y = _primCall "JavaScript.>=" (x, y)
fun ** (x, y) = _primCall "JavaScript.**" (x, y)
fun typeof x = _primCall "JavaScript.typeof" (x)
fun toInt32 x = unsafeFromValue (orb (x, fromInt 0)) : int
fun toUint32 x = unsafeFromValue (>>> (x, fromInt 0)) : word
structure Lib = struct
val Object = LunarML.assumeDiscardable (global "Object")
val Number = LunarML.assumeDiscardable (global "Number")
structure Number = struct
val isFinite = LunarML.assumeDiscardable (field (Number, "isFinite"))
val isNaN = LunarML.assumeDiscardable (field (Number, "isNaN"))
val POSITIVE_INFINITY = LunarML.assumeDiscardable (field (Number, "POSITIVE_INFINITY"))
val NEGATIVE_INFINITY = LunarML.assumeDiscardable (field (Number, "NEGATIVE_INFINITY"))
val MIN_VALUE = LunarML.assumeDiscardable (field (Number, "MIN_VALUE"))
val MAX_VALUE = LunarML.assumeDiscardable (field (Number, "MAX_VALUE"))
val NaN = LunarML.assumeDiscardable (field (Number, "NaN"))
end
val Math = LunarML.assumeDiscardable (global "Math")
structure Math = struct
val E = LunarML.assumeDiscardable (field (Math, "E"))
val PI = LunarML.assumeDiscardable (field (Math, "PI"))
val abs = LunarML.assumeDiscardable (field (Math, "abs"))
val acos = LunarML.assumeDiscardable (field (Math, "acos"))
val acosh = LunarML.assumeDiscardable (field (Math, "acosh"))
val asin = LunarML.assumeDiscardable (field (Math, "asin"))
val asinh = LunarML.assumeDiscardable (field (Math, "asinh"))
val atan = LunarML.assumeDiscardable (field (Math, "atan"))
val atanh = LunarML.assumeDiscardable (field (Math, "atanh"))
val atan2 = LunarML.assumeDiscardable (field (Math, "atan2"))
val cbrt = LunarML.assumeDiscardable (field (Math, "cbrt"))
val ceil = LunarML.assumeDiscardable (field (Math, "ceil"))
val clz32 = LunarML.assumeDiscardable (field (Math, "clz32"))
val cos = LunarML.assumeDiscardable (field (Math, "cos"))
val cosh = LunarML.assumeDiscardable (field (Math, "cosh"))
val exp = LunarML.assumeDiscardable (field (Math, "exp"))
val expm1 = LunarML.assumeDiscardable (field (Math, "expm1"))
val floor = LunarML.assumeDiscardable (field (Math, "floor"))
val fround = LunarML.assumeDiscardable (field (Math, "fround"))
val hypot = LunarML.assumeDiscardable (field (Math, "hypot"))
val imul = LunarML.assumeDiscardable (field (Math, "imul"))
val log = LunarML.assumeDiscardable (field (Math, "log"))
val log1p = LunarML.assumeDiscardable (field (Math, "log1p"))
val log10 = LunarML.assumeDiscardable (field (Math, "log10"))
val log2 = LunarML.assumeDiscardable (field (Math, "log2"))
val max = LunarML.assumeDiscardable (field (Math, "max"))
val min = LunarML.assumeDiscardable (field (Math, "min"))
val pow = LunarML.assumeDiscardable (field (Math, "pow"))
val random = LunarML.assumeDiscardable (field (Math, "random"))
val round = LunarML.assumeDiscardable (field (Math, "round"))
val sign = LunarML.assumeDiscardable (field (Math, "sign"))
val sin = LunarML.assumeDiscardable (field (Math, "sin"))
val sinh = LunarML.assumeDiscardable (field (Math, "sinh"))
val sqrt = LunarML.assumeDiscardable (field (Math, "sqrt"))
val tan = LunarML.assumeDiscardable (field (Math, "tan"))
val tanh = LunarML.assumeDiscardable (field (Math, "tanh"))
val trunc = LunarML.assumeDiscardable (field (Math, "trunc"))
end
val BigInt = LunarML.assumeDiscardable (global "BigInt")
structure BigInt = struct
val asIntN = LunarML.assumeDiscardable (field (BigInt, "asIntN"))
val asUintN = LunarML.assumeDiscardable (field (BigInt, "asUintN"))
end
val Uint8Array = LunarML.assumeDiscardable (global "Uint8Array")
end
fun newObject () = new Lib.Object #[]
end;

structure TextIO :> sig
              (*
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
              *)
              val print : string -> unit
          end = struct
local
    val process = LunarML.assumeDiscardable (JavaScript.call JavaScript.require #[JavaScript.fromWideString "process"])
    val stdout = LunarML.assumeDiscardable (JavaScript.field (process, "stdout"))
in
fun print (s : string) = ( JavaScript.method (stdout, "write") #[JavaScript.unsafeToValue s]; ())
end
end;
val print = TextIO.print;
