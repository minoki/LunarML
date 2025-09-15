structure JavaScript :> sig
              type value = _Prim.JavaScript.value
              exception Error of value
              structure PrimEffect : sig
                type prim_effect
                val pure : prim_effect
                val discardable : prim_effect
                val impure : prim_effect
              end
              val undefined : value
              val null : value
              val sub : value * value -> value
              val field : value * String16.string -> value
              val fieldWithEffect : value * String16.string * PrimEffect.prim_effect -> value
              val set : value * value * value -> unit
              val setField : value * String16.string * value -> unit
              val global : String16.string -> value
              val setGlobal : String16.string * value -> unit
              val call : value -> value vector -> value
              val callWithEffect : PrimEffect.prim_effect -> value -> value vector -> value
              val new : value -> value vector -> value
              val newWithEffect : PrimEffect.prim_effect -> value -> value vector -> value
              val method : value * String16.string -> value vector -> value
              val methodWithEffect : PrimEffect.prim_effect -> value * String16.string -> value vector -> value
              val function : (value vector -> value) -> value
              val fromBool : bool -> value
              val fromInt : int -> value
              val fromWord : word -> value
              val fromReal : real -> value
              val fromString16 : String16.string -> value
              val fromWideString : String16.string -> value
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
              val isTruthy : value -> bool
              val isNullOrUndefined : value -> bool
              val typeof : value -> String16.string
              val newObject : unit -> value
              val encodeUtf8 : String16.string -> string
              val decodeUtf8 : string -> String16.string
              val toInt32 : value -> _Prim.Int32.int
              val toUint32 : value -> _Prim.Word32.word
              structure Lib : sig
                            val parseFloat : value
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
                            val Int8Array : value
                            val Int16Array : value
                            val Int32Array : value
                            val BigInt64Array : value
                            val Uint8Array : value
                            val Uint16Array : value
                            val Uint32Array : value
                            val BigUint64Array : value
                            val Float32Array : value
                            val Float64Array : value
                            val Date : value
                            structure Date : sig
                                          val now : value
                                      end
                            val Map : value
                            val Set : value
                            val WeakMap : value
                            val WeakSet : value
                            val Promise : value
                            structure Promise : sig
                                          val all : value
                                          val allSettled : value
                                          val any : value
                                          val race : value
                                          val reject : value
                                          val resolve : value
                                          val try : value
                                          val withResolvers : value
                                      end
                        end
              end = struct
open JavaScript (* function, encodeUtf8, decodeUtf8 *)
type value = _Prim.JavaScript.value
exception Error = _Prim.JavaScript.Error
structure PrimEffect = struct
type prim_effect = _Prim.PrimEffect.prim_effect
val pure = _Prim.PrimEffect.pure
val discardable = _Prim.PrimEffect.discardable
val impure = _Prim.PrimEffect.impure
end
val undefined = _Prim.JavaScript.undefined
val null = _Prim.JavaScript.null
fun call f args = _primCall "JavaScript.call" (f, args, _Prim.PrimEffect.impure)
fun callWithEffect e f args = _primCall "JavaScript.call" (f, args, e)
fun new ctor args = _primCall "JavaScript.new" (ctor, args, _Prim.PrimEffect.impure)
fun newWithEffect e ctor args = _primCall "JavaScript.new" (ctor, args, e)
fun method (obj, name) args = _primCall "JavaScript.method" (obj, name, args, _Prim.PrimEffect.impure)
fun methodWithEffect e (obj, name) args = _primCall "JavaScript.method" (obj, name, args, e)
fun unsafeToValue x : value = _primCall "Unsafe.cast" (x)
fun unsafeFromValue (x : value) = _primCall "Unsafe.cast" (x)
val fromBool : bool -> value = unsafeToValue
val fromInt : int -> value = unsafeToValue
val fromWord : word -> value = unsafeToValue
val fromReal : real -> value = unsafeToValue
val fromString16 : String16.string -> value = unsafeToValue
val fromWideString = fromString16
fun sub (obj, key) = _primCall "JavaScript.sub" (obj, key, _Prim.PrimEffect.impure)
fun field (obj, key : String16.string) = _primCall "JavaScript.sub" (obj, fromString16 key, _Prim.PrimEffect.impure)
fun fieldWithEffect (obj, key : String16.string, e) = _primCall "JavaScript.sub" (obj, fromString16 key, e)
fun set (obj, key, value) = _primCall "JavaScript.set" (obj, key, value)
fun setField (obj, key, value) = _primCall "JavaScript.set" (obj, fromString16 key, value)
fun global name = _primCall "JavaScript.global" (name)
fun setGlobal (name, value) = _primCall "JavaScript.setGlobal" (name, value)
fun isFalsy x = _primCall "JavaScript.isFalsy" (x)
fun isTruthy x = not (isFalsy x)
fun isNullOrUndefined x = _primCall "JavaScript.isNullOrUndefined" (x)
fun x + y = _primCall "JavaScript.+" (x, y, _Prim.PrimEffect.impure)
fun x - y = _primCall "JavaScript.-" (x, y, _Prim.PrimEffect.impure)
fun x * y = _primCall "JavaScript.*" (x, y, _Prim.PrimEffect.impure)
fun x / y = _primCall "JavaScript./" (x, y, _Prim.PrimEffect.impure)
fun % (x, y) = _primCall "JavaScript.%" (x, y, _Prim.PrimEffect.impure)
fun negate x = _primCall "JavaScript.negate" (x, _Prim.PrimEffect.impure)
fun andb (x, y) = _primCall "JavaScript.andb" (x, y, _Prim.PrimEffect.impure)
fun orb (x, y) = _primCall "JavaScript.orb" (x, y, _Prim.PrimEffect.impure)
fun xorb (x, y) = _primCall "JavaScript.xorb" (x, y, _Prim.PrimEffect.impure)
fun notb x = _primCall "JavaScript.notb" (x, _Prim.PrimEffect.impure)
fun << (x, y) = _primCall "JavaScript.<<" (x, y, _Prim.PrimEffect.impure)
fun >> (x, y) = _primCall "JavaScript.>>" (x, y, _Prim.PrimEffect.impure)
fun >>> (x, y) = _primCall "JavaScript.>>>" (x, y, _Prim.PrimEffect.impure)
fun === (x, y) = _primCall "JavaScript.===" (x, y)
fun !== (x, y) = _primCall "JavaScript.!==" (x, y)
fun x < y = _primCall "JavaScript.<" (x, y, _Prim.PrimEffect.impure)
fun x > y = _primCall "JavaScript.>" (x, y, _Prim.PrimEffect.impure)
fun x <= y = _primCall "JavaScript.<=" (x, y, _Prim.PrimEffect.impure)
fun x >= y = _primCall "JavaScript.>=" (x, y, _Prim.PrimEffect.impure)
fun ** (x, y) = _primCall "JavaScript.**" (x, y, _Prim.PrimEffect.impure)
fun typeof x = _primCall "JavaScript.typeof" (x)
fun toInt32 x = unsafeFromValue (orb (x, fromInt 0)) : _Prim.Int32.int
fun toUint32 x = unsafeFromValue (>>> (x, fromInt 0)) : _Prim.Word32.word
structure Lib = struct
val parseFloat = global "parseFloat"
val Object = global "Object"
val Number = global "Number"
structure Number = struct
val isFinite = fieldWithEffect (Number, "isFinite", _Prim.PrimEffect.discardable)
val isNaN = fieldWithEffect (Number, "isNaN", _Prim.PrimEffect.discardable)
val POSITIVE_INFINITY = fieldWithEffect (Number, "POSITIVE_INFINITY", _Prim.PrimEffect.discardable)
val NEGATIVE_INFINITY = fieldWithEffect (Number, "NEGATIVE_INFINITY", _Prim.PrimEffect.discardable)
val MIN_VALUE = fieldWithEffect (Number, "MIN_VALUE", _Prim.PrimEffect.discardable)
val MAX_VALUE = fieldWithEffect (Number, "MAX_VALUE", _Prim.PrimEffect.discardable)
val NaN = fieldWithEffect (Number, "NaN", _Prim.PrimEffect.discardable)
end
val Math = global "Math"
structure Math = struct
val E = fieldWithEffect (Math, "E", _Prim.PrimEffect.discardable)
val PI = fieldWithEffect (Math, "PI", _Prim.PrimEffect.discardable)
val abs = fieldWithEffect (Math, "abs", _Prim.PrimEffect.discardable)
val acos = fieldWithEffect (Math, "acos", _Prim.PrimEffect.discardable)
val acosh = fieldWithEffect (Math, "acosh", _Prim.PrimEffect.discardable)
val asin = fieldWithEffect (Math, "asin", _Prim.PrimEffect.discardable)
val asinh = fieldWithEffect (Math, "asinh", _Prim.PrimEffect.discardable)
val atan = fieldWithEffect (Math, "atan", _Prim.PrimEffect.discardable)
val atanh = fieldWithEffect (Math, "atanh", _Prim.PrimEffect.discardable)
val atan2 = fieldWithEffect (Math, "atan2", _Prim.PrimEffect.discardable)
val cbrt = fieldWithEffect (Math, "cbrt", _Prim.PrimEffect.discardable)
val ceil = fieldWithEffect (Math, "ceil", _Prim.PrimEffect.discardable)
val clz32 = fieldWithEffect (Math, "clz32", _Prim.PrimEffect.discardable)
val cos = fieldWithEffect (Math, "cos", _Prim.PrimEffect.discardable)
val cosh = fieldWithEffect (Math, "cosh", _Prim.PrimEffect.discardable)
val exp = fieldWithEffect (Math, "exp", _Prim.PrimEffect.discardable)
val expm1 = fieldWithEffect (Math, "expm1", _Prim.PrimEffect.discardable)
val floor = fieldWithEffect (Math, "floor", _Prim.PrimEffect.discardable)
val fround = fieldWithEffect (Math, "fround", _Prim.PrimEffect.discardable)
val hypot = fieldWithEffect (Math, "hypot", _Prim.PrimEffect.discardable)
val imul = fieldWithEffect (Math, "imul", _Prim.PrimEffect.discardable)
val log = fieldWithEffect (Math, "log", _Prim.PrimEffect.discardable)
val log1p = fieldWithEffect (Math, "log1p", _Prim.PrimEffect.discardable)
val log10 = fieldWithEffect (Math, "log10", _Prim.PrimEffect.discardable)
val log2 = fieldWithEffect (Math, "log2", _Prim.PrimEffect.discardable)
val max = fieldWithEffect (Math, "max", _Prim.PrimEffect.discardable)
val min = fieldWithEffect (Math, "min", _Prim.PrimEffect.discardable)
val pow = fieldWithEffect (Math, "pow", _Prim.PrimEffect.discardable)
val random = fieldWithEffect (Math, "random", _Prim.PrimEffect.discardable)
val round = fieldWithEffect (Math, "round", _Prim.PrimEffect.discardable)
val sign = fieldWithEffect (Math, "sign", _Prim.PrimEffect.discardable)
val sin = fieldWithEffect (Math, "sin", _Prim.PrimEffect.discardable)
val sinh = fieldWithEffect (Math, "sinh", _Prim.PrimEffect.discardable)
val sqrt = fieldWithEffect (Math, "sqrt", _Prim.PrimEffect.discardable)
val tan = fieldWithEffect (Math, "tan", _Prim.PrimEffect.discardable)
val tanh = fieldWithEffect (Math, "tanh", _Prim.PrimEffect.discardable)
val trunc = fieldWithEffect (Math, "trunc", _Prim.PrimEffect.discardable)
end
val BigInt = global "BigInt"
structure BigInt = struct
val asIntN = fieldWithEffect (BigInt, "asIntN", _Prim.PrimEffect.discardable)
val asUintN = fieldWithEffect (BigInt, "asUintN", _Prim.PrimEffect.discardable)
end
val Int8Array = global "Int8Array"
val Int16Array = global "Int16Array"
val Int32Array = global "Int32Array"
val BigInt64Array = global "BigInt64Array"
val Uint8Array = global "Uint8Array"
val Uint16Array = global "Uint16Array"
val Uint32Array = global "Uint32Array"
val BigUint64Array = global "BigUint64Array"
val Float32Array = global "Float32Array"
val Float64Array = global "Float64Array"
val Date = global "Date"
structure Date = struct
val now = fieldWithEffect (Date, "now", _Prim.PrimEffect.discardable)
end
val Map = global "Map"
val Set = global "Set"
val WeakMap = global "WeakMap"
val WeakSet = global "WeakSet"
val Promise = global "Promise"
structure Promise = struct
val all = fieldWithEffect (Promise, "all", _Prim.PrimEffect.discardable)
val allSettled = fieldWithEffect (Promise, "allSettled", _Prim.PrimEffect.discardable)
val any = fieldWithEffect (Promise, "any", _Prim.PrimEffect.discardable)
val race = fieldWithEffect (Promise, "race", _Prim.PrimEffect.discardable)
val reject = fieldWithEffect (Promise, "reject", _Prim.PrimEffect.discardable)
val resolve = fieldWithEffect (Promise, "resolve", _Prim.PrimEffect.discardable)
val try = fieldWithEffect (Promise, "try", _Prim.PrimEffect.discardable)
val withResolvers = fieldWithEffect (Promise, "withResolvers", _Prim.PrimEffect.discardable)
end
end
fun newObject () = new Lib.Object #[]
end;
