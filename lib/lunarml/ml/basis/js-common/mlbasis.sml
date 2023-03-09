_equality word = fn (x, y) => _primCall "Word.=" (x, y);
structure Word = struct
type word = word
fun ~ x = _primCall "Word.~" (x)
fun x + y = _primCall "Word.+" (x, y)
fun x - y = _primCall "Word.-" (x, y)
fun x * y = _primCall "Word.*" (x, y)
fun x div y = if y = 0w0 then
                  raise Div
              else
                  _primCall "Word.div.unchecked" (x, y)
fun x mod y = if y = 0w0 then
                  raise Div
              else
                  _primCall "Word.mod.unchecked" (x, y)
fun x < y = _primCall "Word.<" (x, y)
fun x <= y = _primCall "Word.<=" (x, y)
fun x > y = _primCall "Word.>" (x, y)
fun x >= y = _primCall "Word.>=" (x, y)
end
local
fun fromWord (x : word) = x
in
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
                        , wordSize = 32
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

structure StringCvt : sig
              datatype radix = BIN | OCT | DEC | HEX
              datatype realfmt = SCI of int option
                               | FIX of int option
                               | GEN of int option
                               | EXACT
              type ('a, 'b) reader = 'b -> ('a * 'b) option
          end = struct
datatype radix = BIN | OCT | DEC | HEX
datatype realfmt = SCI of int option
                 | FIX of int option
                 | GEN of int option
                 | EXACT
type ('a, 'b) reader = 'b -> ('a * 'b) option
end

signature INTEGER = sig
    eqtype int
    (* val toLarge : int -> LargeInt.int *)
    (* val fromLarge : LargeInt.int -> int *)
    val toInt : int -> Int.int
    val fromInt : Int.int -> int
    val precision : Int.int option
    val minInt : int option
    val maxInt : int option
    val + : int * int -> int
    val - : int * int -> int
    val * : int * int -> int
    val div : int * int -> int
    val mod : int * int -> int
    val quot : int * int -> int
    val rem : int * int -> int
    val compare : int * int -> order
    val < : int * int -> bool
    val <= : int * int -> bool
    val > : int * int -> bool
    val >= : int * int -> bool
    val ~ : int -> int
    val abs : int -> int
    val min : int * int -> int
    val max : int * int -> int
    val sign : int -> Int.int
    val sameSign : int * int -> bool
    val fmt : StringCvt.radix -> int -> string
    val toString : int -> string
    (* val scan : StringCvt.radix -> (char, 'a) StringCvt.reader -> (int, 'a) StringCvt.reader; defined in scan-num.sml *)
    (* val fromString : string -> int option; defined in scan-num.sml *)
end;

structure Int : INTEGER where type int = int = struct
open Int (* +, -, *, div, mod, ~, abs, <, <=, >, >=, fromInt *)
(* toLarge, fromLarge *)
val toInt : int -> int = fn x => x
val precision : int option = SOME 32
val minInt : int option = SOME ~0x80000000
val maxInt : int option = SOME 0x7fffffff
fun quot (x, y) = _primCall "Int.quot" (x, y)
fun rem (x, y) = _primCall "Int.rem" (x, y)
val compare : int * int -> order = fn (x, y) => if x = y then
                                                    EQUAL
                                                else if x < y then
                                                    LESS
                                                else
                                                    GREATER
(* Maybe use Math.min/max? *)
val min : int * int -> int = fn (x, y) => if x < y then
                                              x
                                          else
                                              y
val max : int * int -> int = fn (x, y) => if x < y then
                                              y
                                          else
                                              x
val sign : int -> int = fn x => if x > 0 then
                                    1
                                else if x < 0 then
                                    ~1
                                else
                                    0
val sameSign : int * int -> bool = fn (x, y) => sign x = sign y
fun fmt StringCvt.BIN x = let val s = if x >= 0 then
                                          JavaScript.unsafeFromValue (JavaScript.method (JavaScript.fromInt x, "toString") #[JavaScript.fromInt 2])
                                      else
                                          WideString.^ ("~", JavaScript.unsafeFromValue (JavaScript.method (JavaScript.negate (JavaScript.fromInt x), "toString") #[JavaScript.fromInt 2]))
                          in JavaScript.encodeUtf8 s
                          end
  | fmt StringCvt.OCT x = let val s = if x >= 0 then
                                          JavaScript.unsafeFromValue (JavaScript.method (JavaScript.fromInt x, "toString") #[JavaScript.fromInt 8])
                                      else
                                          WideString.^ ("~", JavaScript.unsafeFromValue (JavaScript.method (JavaScript.negate (JavaScript.fromInt x), "toString") #[JavaScript.fromInt 8]))
                          in JavaScript.encodeUtf8 s
                          end
  | fmt StringCvt.DEC x = let val s = if x >= 0 then
                                          JavaScript.unsafeFromValue (JavaScript.method (JavaScript.fromInt x, "toString") #[])
                                      else
                                          WideString.^ ("~", JavaScript.unsafeFromValue (JavaScript.method (JavaScript.negate (JavaScript.fromInt x), "toString") #[]))
                          in JavaScript.encodeUtf8 s
                          end
  | fmt StringCvt.HEX x = let val s = if x >= 0 then
                                          JavaScript.unsafeFromValue (JavaScript.method (JavaScript.fromInt x, "toString") #[JavaScript.fromInt 16])
                                      else
                                          WideString.^ ("~", JavaScript.unsafeFromValue (JavaScript.method (JavaScript.negate (JavaScript.fromInt x), "toString") #[JavaScript.fromInt 16]))
                              val s = JavaScript.method (JavaScript.fromWideString s, "toUpperCase") #[]
                          in JavaScript.encodeUtf8 (JavaScript.unsafeFromValue s : WideString.string)
                          end
fun toString (x : int) : string = fmt StringCvt.DEC x
end; (* structure Int *)

signature WORD = sig
    eqtype word
    val wordSize : int
    (* val toLarge : word -> LargeWord.word; defined in word.sml *)
    (* val toLargeX : word -> LargeWord.word; defined in word.sml *)
    (* val toLargeWord : word -> LargeWord.word; defined in word.sml *)
    (* val toLargeWordX : word -> LargeWord.word; defined in word.sml *)
    (* val fromLarge : LargeWord.word -> word; defined in word.sml *)
    (* val fromLargeWord : LargeWord.word -> word; defined in word.sml *)
    (* val toLargeInt *)
    (* val toLargeIntX *)
    (* val fromLargeInt *)
    val toInt : word -> int
    val toIntX : word -> int
    val fromInt : int -> word
    val andb : word * word -> word
    val orb : word * word -> word
    val xorb : word * word -> word
    val notb : word -> word
    val << : word * Word.word -> word
    val >> : word * Word.word -> word
    val ~>> : word * Word.word -> word
    val + : word * word -> word
    val - : word * word -> word
    val * : word * word -> word
    val div : word * word -> word
    val mod : word * word -> word
    val compare : word * word -> order
    val < : word * word -> bool
    val <= : word * word -> bool
    val > : word * word -> bool
    val >= : word * word -> bool
    val ~ : word -> word
    val min : word * word -> word
    val max : word * word -> word
    val fmt : StringCvt.radix -> word -> string
    val toString : word -> string
    (* val scan : StringCvt.radix -> (char, 'a) StringCvt.reader -> (word, 'a) StringCvt.reader; defined in scan-num.sml *)
    (* val fromString : string -> word option; defined in scan-num.sml *)
end;

structure Word :> WORD where type word = word = struct
open Word (* +, -, *, div, mod, ~, <, <=, >, >= *)
val wordSize : int = 32
(* toLarge, toLargeX, toLargeWord, toLargeWordX, fromLarge, fromLargeWord, toLargeInt, toLargeIntX, fromLargeInt *)
val toInt : word -> int = fn x => if x >= 0wx80000000 then
                                      raise Overflow
                                  else
                                      Unsafe.cast x
val toIntX : word -> int = fn x => JavaScript.toInt32 (JavaScript.fromWord x)
val fromInt : int -> word = fn x => JavaScript.toUint32 (JavaScript.fromInt x)
val andb : word * word -> word = fn (x, y) => _primCall "Word.andb" (x, y)
val orb : word * word -> word = fn (x, y) => _primCall "Word.orb" (x, y)
val xorb : word * word -> word = fn (x, y) => _primCall "Word.xorb" (x, y)
val notb : word -> word = fn x => _primCall "Word.notb" (x)
val << : word * word -> word = fn (x, y) => if y >= 0w32 then
                                                0w0
                                            else
                                                _primCall "Word.<<.unchecked" (x, y)
val >> : word * word -> word = fn (x, y) => if y >= 0w32 then
                                                0w0
                                            else
                                                _primCall "Word.>>.unchecked" (x, y)
val ~>> : word * word -> word = fn (x, y) => if y >= 0w31 then
                                                 if x >= 0wx80000000 then
                                                     0wxFFFFFFFF
                                                 else
                                                     0w0
                                             else
                                                 JavaScript.toUint32 (JavaScript.>> (JavaScript.fromWord x, JavaScript.fromWord y))
val compare : word * word -> order = fn (x, y) => if x = y then
                                                      EQUAL
                                                  else if x < y then
                                                      LESS
                                                  else
                                                      GREATER
val min : word * word -> word = fn (x, y) => if x < y then
                                                 x
                                             else
                                                 y
val max : word * word -> word = fn (x, y) => if x < y then
                                                 y
                                             else
                                                 x
fun fmt StringCvt.BIN x = let val s = JavaScript.unsafeFromValue (JavaScript.method (JavaScript.fromWord x, "toString") #[JavaScript.fromInt 2])
                          in JavaScript.encodeUtf8 s
                          end
  | fmt StringCvt.OCT x = let val s = JavaScript.unsafeFromValue (JavaScript.method (JavaScript.fromWord x, "toString") #[JavaScript.fromInt 8])
                          in JavaScript.encodeUtf8 s
                          end
  | fmt StringCvt.DEC x = let val s = JavaScript.unsafeFromValue (JavaScript.method (JavaScript.fromWord x, "toString") #[])
                          in JavaScript.encodeUtf8 s
                          end
  | fmt StringCvt.HEX x = let val s = JavaScript.unsafeFromValue (JavaScript.method (JavaScript.fromWord x, "toString") #[JavaScript.fromInt 16])
                              val s = JavaScript.method (JavaScript.fromWideString s, "toUpperCase") #[]
                          in JavaScript.encodeUtf8 (JavaScript.unsafeFromValue s : WideString.string)
                          end
fun toString (x : word) : string = fmt StringCvt.HEX x
(* scan, fromString *)
end; (* structure Word *)

structure IEEEReal : sig
              exception Unordered
              datatype real_order = LESS | EQUAL | GREATER | UNORDERED
              datatype float_class = NAN | INF | ZERO | NORMAL | SUBNORMAL
              datatype rounding_mode = TO_NEAREST | TO_NEGINF | TO_POSINF | TO_ZERO
              type decimal_approx = { class : float_class, sign : bool, digits : int list, exp : int }
          end = struct
exception Unordered
datatype real_order = LESS | EQUAL | GREATER | UNORDERED
datatype float_class = NAN | INF | ZERO | NORMAL | SUBNORMAL
datatype rounding_mode = TO_NEAREST | TO_NEGINF | TO_POSINF | TO_ZERO
type decimal_approx = { class : float_class, sign : bool, digits : int list, exp : int }
end;

signature REAL = sig
    type real
    (* structure Math *)
    val radix : int
    val precision : int
    val maxFinite : real
    val minPos : real
    val minNormalPos : real
    val posInf : real
    val negInf : real
    val + : real * real -> real
    val - : real * real -> real
    val * : real * real -> real
    val / : real * real -> real
    val rem : real * real -> real
    (* val *+ : real * real * real -> real *)
    (* val *- : real * real * real -> real *)
    val ~ : real -> real
    val abs : real -> real
    val min : real * real -> real
    val max : real * real -> real
    val sign : real -> int
    val signBit : real -> bool
    val sameSign : real * real -> bool
    val copySign : real * real -> real
    val compare : real * real -> order
    val compareReal : real * real -> IEEEReal.real_order
    val < : real * real -> bool
    val <= : real * real -> bool
    val > : real * real -> bool
    val >= : real * real -> bool
    val == : real * real -> bool
    val != : real * real -> bool
    val ?= : real * real -> bool
    val unordered : real * real -> bool
    val isFinite : real -> bool
    val isNan : real -> bool
    val isNormal : real -> bool
    val class : real -> IEEEReal.float_class
    val toManExp : real -> { man : real, exp : int }
    val fromManExp : { man : real, exp : int } -> real
    val split : real -> { whole : real, frac : real }
    val realMod : real -> real
    (* val nextAfter : real * real -> real *)
    val checkFloat : real -> real
    val realFloor : real -> real
    val realCeil : real -> real
    val realTrunc : real -> real
    val realRound : real -> real
    val floor : real -> int
    val ceil : real -> int
    val trunc : real -> int
    val round : real -> int
    val toInt : IEEEReal.rounding_mode -> real -> int
    (* val toLargeInt : IEEEReal.rounding_mode -> real -> LargeInt.int *)
    val fromInt : int -> real
    (* val fromLargeInt : LargeInt.int -> real *)
    (* val toLarge : real -> LargeReal.real *)
    (* val fromLarge : IEEEReal.rounding_mode -> LargeReal.real -> real *)
    val fmt : StringCvt.realfmt -> real -> string
    val toString : real -> string
    (* val scan : (char, 'a) StringCvt.reader -> (real, 'a) StringCvt.reader; implemented in scan-num.sml *)
    (* val fromString : string -> real option; implemented in scan-num.sml *)
    (* val toDecimal : real -> IEEEReal.decimal_approx *)
    (* val fromDecimal : IEEEReal.decimal_approx -> real option *)
end;

structure Real : REAL where type real = real = struct
val radix : int = 2
val precision : int = 53 (* binary64 *)
val posInf = JavaScript.unsafeFromValue JavaScript.Lib.Number.POSITIVE_INFINITY : real
val negInf = JavaScript.unsafeFromValue JavaScript.Lib.Number.NEGATIVE_INFINITY : real
fun == (x, y) = JavaScript.=== (JavaScript.fromReal x, JavaScript.fromReal y)
fun != (x, y) = JavaScript.!== (JavaScript.fromReal x, JavaScript.fromReal y)
infix 4 == !=
fun isNan x = JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.Number.isNaN #[JavaScript.fromReal x]) : bool
fun ?= (x, y) = x == y orelse isNan x orelse isNan y (* EQUAL or UNORDERED *)
fun unordered (x, y) = isNan x orelse isNan y
fun isFinite x = JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.Number.isFinite #[JavaScript.fromReal x])
val maxFinite = JavaScript.unsafeFromValue JavaScript.Lib.Number.MAX_VALUE : real (* 0x1.fffffffffffffp1023; assuming binary64 *)
val minPos = JavaScript.unsafeFromValue JavaScript.Lib.Number.MIN_VALUE : real (* 0x1p-1074; assuming binary64 *)
val minNormalPos = 0x1p~1022 : real (* 0x1p-1022; assuming binary64 *)
fun isNormal x = isFinite x andalso minNormalPos <= abs x
fun class x = if x == 0.0 then
                  IEEEReal.ZERO
              else
                  if isFinite x then
                      (* normal or subnormal *)
                      if minNormalPos <= abs x then
                          IEEEReal.NORMAL
                      else
                          IEEEReal.SUBNORMAL
                  else
                      (* infinity or NaN *)
                      if isNan x then
                          IEEEReal.NAN
                      else
                          IEEEReal.INF
fun rem (x : real, y : real) : real = JavaScript.unsafeFromValue (JavaScript.% (JavaScript.fromReal x, JavaScript.fromReal y))
fun min (x : real, y : real) = if isNan x then
                                   y
                               else if isNan y then
                                   x
                               else
                                   (* Math.min: propagates NaN and honors the sign of zero *)
                                   JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.Math.min #[JavaScript.fromReal x, JavaScript.fromReal y])
fun max (x : real, y : real) = if isNan x then
                                   y
                               else if isNan y then
                                   x
                               else
                                   (* Math.max: propagates NaN and honors the sign of zero *)
                                   JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.Math.max #[JavaScript.fromReal x, JavaScript.fromReal y])
fun sign x = if x == 0.0 then
                 0
             else if x < 0.0 then
                 ~1
             else if x > 0.0 then
                 1
             else (* NaN *)
                 raise Domain
fun signBit x = if x < 0.0 then
                    true
                else if x > 0.0 then
                    false
                else
                    1.0 / x < 0.0 (* handle negative zero; NaN is not handled *)
fun sameSign (x, y) = signBit x = signBit y
fun copySign (x, y) = if signBit x = signBit y then
                          x
                      else
                          ~ x
fun compare (x, y) = if isNan x orelse isNan y then
                         raise IEEEReal.Unordered
                     else
                         if x < y then
                             LESS
                         else if x == y then
                             EQUAL
                         else
                             GREATER
fun compareReal (x, y) = if isNan x orelse isNan y then
                             IEEEReal.UNORDERED
                         else
                             if x < y then
                                 IEEEReal.LESS
                             else if x == y then
                                 IEEEReal.EQUAL
                             else
                                 IEEEReal.GREATER
(* TODO: We may have math.frexp *)
(* Assumption: 2^exp is exact *)
fun toManExp x = let val a = abs x
                 in if a == 0.0 orelse a == posInf orelse isNan a then
                        { man = x, exp = 0 }
                    else
                        let val e0 : int = JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.Math.floor #[JavaScript.call JavaScript.Lib.Math.log2 #[JavaScript.fromReal a]]) - 1
                            fun fixup e = let val lower = JavaScript.unsafeFromValue (JavaScript.** (JavaScript.fromReal 2.0, JavaScript.fromInt (e - 1))) : real
                                          in if lower <= a then
                                                 if a < lower * 2.0 then (* lower * 2.0 may be infinity *)
                                                     { exp = e, man = x / lower * 0.5 }
                                                 else
                                                     fixup (e + 1)
                                             else
                                                 fixup (e - 1)
                                          end
                        in fixup e0
                        end
                 end
(* TODO: We may have math.ldexp *)
(* Assumption: 2^exp is exact *)
fun fromManExp { man : real, exp : int } = if ~1022 <= exp then
                                               if exp < 1024 then
                                                   man * JavaScript.unsafeFromValue (JavaScript.** (JavaScript.fromReal 2.0, JavaScript.fromInt exp))
                                               else
                                                   let val exp' = if exp > 2098 then
                                                                      2098 (* 0x1p1023 / 0x1p~1074 = 0x1p2097 *)
                                                                  else
                                                                      exp
                                                   in fromManExp { man = man * 0x1p1023, exp = exp' - 1023 } (* Avoid undue overflow *)
                                                   end
                                           else
                                               let val exp' = if exp < ~2099 then
                                                                  ~2099 (* 0x1p~1074 / 0x1p1024 = 0x1p~2098 *)
                                                              else
                                                                  exp
                                                   val j = exp' mod ~1022 (* ~1022 < j <= 0 *)
                                               in if j <> 0 then
                                                      let val s = JavaScript.unsafeFromValue (JavaScript.** (JavaScript.fromReal 2.0, JavaScript.fromInt j))
                                                      in fromManExp { man = man * s, exp = exp' - j }
                                                      end
                                                  else
                                                      fromManExp { man = man * 0x1p~1022, exp = exp' + 1022 }
                                               end (* Avoid undue underflow and double rounding *)
fun split x = let val intPart = JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.Math.trunc #[JavaScript.fromReal x]) : real
                  val fracPart = JavaScript.unsafeFromValue (JavaScript.% (JavaScript.fromReal x, JavaScript.fromReal 1.0)) : real
                  val frac = if isNan fracPart then (* x: infinity or NaN *)
                                 0.0 / x
                             else
                                 fracPart
              in { whole = intPart, frac = frac }
              end
fun realMod x = let val y = JavaScript.unsafeFromValue (JavaScript.% (JavaScript.fromReal x, JavaScript.fromReal 1.0)) : real
                in if isNan y then (* x: infinity or NaN *)
                       0.0 / x
                   else
                       y
                end
fun checkFloat x = if isNan x then
                       raise Div
                   else if x == posInf orelse x == negInf then
                       raise Overflow
                   else
                       x
fun realFloor x = JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.Math.floor #[JavaScript.fromReal x]) : real
fun realCeil x = JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.Math.ceil #[JavaScript.fromReal x]) : real
fun realTrunc x = JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.Math.trunc #[JavaScript.fromReal x]) : real
(* round to nearest even; JavaScript's Math.round breaks ties by preferring the Number closer to +inf *)
fun realRound x = let val intPart = JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.Math.round #[JavaScript.fromReal x]) : real
                      val intPartIsEven = JavaScript.=== (JavaScript.% (JavaScript.fromReal intPart, JavaScript.fromReal 2.0), JavaScript.fromReal 0.0)
                      val fracPart = JavaScript.unsafeFromValue (JavaScript.% (JavaScript.fromReal x, JavaScript.fromReal 1.0)) : real
                  in if (fracPart == 0.5 orelse fracPart == ~0.5) andalso not intPartIsEven then
                         intPart - 1.0
                     else
                         intPart
                  end
fun resultToInt x = if isNan x then
                        raise Domain
                    else if x < ~0x80000000p0 orelse x > 0x7fffffffp0 then
                        raise Overflow
                    else
                        JavaScript.toInt32 (JavaScript.fromReal x)
fun floor x = resultToInt (realFloor x)
fun ceil x = resultToInt (realCeil x)
fun trunc x = resultToInt (realTrunc x)
fun round x = resultToInt (realRound x)
fun toInt IEEEReal.TO_NEGINF = floor
  | toInt IEEEReal.TO_POSINF = ceil
  | toInt IEEEReal.TO_ZERO = trunc
  | toInt IEEEReal.TO_NEAREST = round
fun fromInt (x : int) : real = Unsafe.cast x
fun fmt (StringCvt.SCI prec) r = let val prec = Option.getOpt (prec, 6)
                                     val () = if prec < 0 then
                                                  raise Size
                                              else
                                                  ()
                                     val result = JavaScript.method (JavaScript.fromReal r, "toExponential") #[JavaScript.fromInt prec] (* TODO: Is this OK? *)
                                     val result = JavaScript.method (result, "replaceAll") #[JavaScript.fromWideString "-", JavaScript.fromWideString "~"]
                                 in JavaScript.encodeUtf8 (JavaScript.unsafeFromValue result : WideString.string)
                                 end
  | fmt (StringCvt.FIX prec) r = let val prec = Option.getOpt (prec, 6)
                                     val () = if prec < 0 then
                                                  raise Size
                                              else
                                                  ()
                                     val result = JavaScript.method (JavaScript.fromReal r, "toFixed") #[JavaScript.fromInt prec] (* TODO: Is this OK? *)
                                     val result = JavaScript.method (result, "replaceAll") #[JavaScript.fromWideString "-", JavaScript.fromWideString "~"]
                                 in JavaScript.encodeUtf8 (JavaScript.unsafeFromValue result : WideString.string)
                                 end
  | fmt (StringCvt.GEN prec) r = let val prec = Option.getOpt (prec, 12)
                                     val () = if prec < 1 then
                                                  raise Size
                                              else
                                                  ()
                                     val result = JavaScript.method (JavaScript.fromReal r, "toPrecision") #[JavaScript.fromInt prec] (* TODO: Is this OK? *)
                                     val result = JavaScript.method (result, "replaceAll") #[JavaScript.fromWideString "-", JavaScript.fromWideString "~"]
                                     val result = JavaScript.method (result, "toUpperCase") #[]
                                 in JavaScript.encodeUtf8 (JavaScript.unsafeFromValue result : WideString.string)
                                 end
  | fmt StringCvt.EXACT r = raise Fail "Real.fmt StringCvt.EXACT: not implemented yet"
val toString = fmt (StringCvt.GEN NONE)
open Real (* +, -, *, /, ~, abs, <, <=, >, >= *)
end; (* structure Real *)

structure Math :> MATH where type real = Real.real = struct
type real = real
val pi : real = JavaScript.unsafeFromValue JavaScript.Lib.Math.PI
val sqrt : real -> real = fn x => JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.Math.sqrt #[JavaScript.fromReal x])
val sin : real -> real = fn x => JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.Math.sin #[JavaScript.fromReal x])
val cos : real -> real = fn x => JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.Math.cos #[JavaScript.fromReal x])
val tan : real -> real = fn x => JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.Math.tan #[JavaScript.fromReal x])
val asin : real -> real = fn x => JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.Math.asin #[JavaScript.fromReal x])
val acos : real -> real = fn x => JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.Math.acos #[JavaScript.fromReal x])
val atan : real -> real = fn x => JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.Math.atan #[JavaScript.fromReal x])
val atan2 : real * real -> real = fn (y, x) => JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.Math.atan2 #[JavaScript.fromReal y, JavaScript.fromReal x])
val exp : real -> real = fn x => JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.Math.exp #[JavaScript.fromReal x])
val e = JavaScript.unsafeFromValue JavaScript.Lib.Math.E : real
val pow : real * real -> real = fn (x, y) => JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.Math.pow #[JavaScript.fromReal x, JavaScript.fromReal y])
val ln : real -> real = fn x => JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.Math.log #[JavaScript.fromReal x])
val log10 : real -> real = fn x => JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.Math.log10 #[JavaScript.fromReal x])
val sinh : real -> real = fn x => JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.Math.sinh #[JavaScript.fromReal x])
val cosh : real -> real = fn x => JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.Math.cosh #[JavaScript.fromReal x])
val tanh : real -> real = fn x => JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.Math.tanh #[JavaScript.fromReal x])
end; (* structure Math *)

structure String : sig
              type string = string
              type char = char
              val size : string -> int
              val sub : string * int -> char
              val extract : string * int * int option -> string
              val substring : string * int * int -> string
              val ^ : string * string -> string
              val concat : string list -> string
              val concatWith : string -> string list -> string
              val str : char -> string
              val implode : char list -> string
              val explode : string -> char list
              val map : (char -> char) -> string -> string
              val translate : (char -> string) -> string -> string
              val tokens : (char -> bool) -> string -> string list
              val fields : (char -> bool) -> string -> string list
              val isPrefix : string -> string -> bool
              val isSuffix : string -> string -> bool
              val compare : string * string -> order
              val < : string * string -> bool
              val <= : string * string -> bool
              val > : string * string -> bool
              val >= : string * string -> bool
              val implodeRev : char list -> string
          end = struct
type string = string
type char = char
val size = String.size
val str = String.str
val op ^ = String.^
val sub = String.sub
val implode = String.implode
fun implodeRev l = implode (List.rev l)
fun substring (s : string, i : int, j : int) : string = if i < 0 orelse j < 0 orelse size s < i + j then
                                                            raise Subscript
                                                        else
                                                            JavaScript.unsafeFromValue (JavaScript.method (JavaScript.unsafeToValue s, "subarray") #[JavaScript.fromInt i, JavaScript.fromInt (i + j)])
fun extract (s : string, i : int, NONE : int option) : string = if i < 0 orelse size s < i then
                                                                    raise Subscript
                                                                else
                                                                    JavaScript.unsafeFromValue (JavaScript.method (JavaScript.unsafeToValue s, "subarray") #[JavaScript.fromInt i, JavaScript.fromInt (size s)])
  | extract (s, i, SOME j) = substring (s, i, j)
fun concatWith (s : string) (l : string list) : string = _primCall "call2" (_Prim.String.concatWith, s, l)
fun explode (s : string) : char list = Vector.foldr (op ::) [] (Vector.tabulate (size s, fn i => sub (s, i)))
fun tokens f s = let fun go (revTokens, acc, []) = List.rev (if List.null acc then revTokens else implodeRev acc :: revTokens)
                       | go (revTokens, acc, x :: xs) = if f x then
                                                            go (if List.null acc then revTokens else implodeRev acc :: revTokens, [], xs)
                                                        else
                                                            go (revTokens, x :: acc, xs)
                 in go ([], [], explode s)
                 end
fun fields f s = let fun go (revFields, acc, []) = List.rev (implodeRev acc :: revFields)
                       | go (revFields, acc, x :: xs) = if f x then
                                                            go (implodeRev acc :: revFields, [], xs)
                                                        else
                                                            go (revFields, x :: acc, xs)
                 in go ([], [], explode s)
                 end
fun isPrefix prefix s = let val n = size prefix
                        in if n > size s then
                               false
                           else
                               substring (s, 0, n) = prefix
                        end
fun isSuffix suffix s = let val n = size suffix
                            val m = size s
                        in if n > m then
                               false
                           else
                               substring (s, m - n, n) = suffix
                        end
(* isSubstring, collate, toString, scan, fromString, toCString, fromCString *)
fun compare (s, t) = if s = t then
                         EQUAL
                     else if String.< (s, t) then
                         LESS
                     else
                         GREATER
open String (* size, ^, str, <, <=, >, >=, concat, implode, translate, map *)
end (* structure String *)
val op ^ : string * string -> string = String.^;

signature CHAR = sig
    eqtype char
    eqtype string
    val minChar : char
    val maxChar : char
    val maxOrd : int
    val ord : char -> int
    val chr : int -> char
    val succ : char -> char
    val pred : char -> char
    val compare : char * char -> order
    val < : char * char -> bool
    val <= : char * char -> bool
    val > : char * char -> bool
    val >= : char * char -> bool
    val contains : string -> char -> bool
    val notContains : string -> char -> bool
    val isAscii : char -> bool
    val toLower : char -> char
    val toUpper : char -> char
    val isAlpha : char -> bool
    val isAlphaNum : char -> bool
    val isCntrl : char -> bool
    val isDigit : char -> bool
    val isGraph : char -> bool
    val isHexDigit : char -> bool
    val isLower : char -> bool
    val isPrint : char -> bool
    val isSpace : char -> bool
    val isPunct : char -> bool
    val isUpper : char -> bool
    val toString : char -> String.string
    (* val scan : (Char.char, 'a) StringCvt.reader -> (char, 'a) StringCvt.reader; implemented in scan-text.sml *)
    (* val fromString : String.string -> char option; implemented in scan-text.sml *)
    val toCString : char -> String.string
    (* val fromCString : String.string -> char option *)
end;

structure Char :> CHAR where type char = char where type string = String.string = struct
type char = char
type string = string
val minChar = #"\000"
val maxChar = #"\255"
val maxOrd = 255
val ord : char -> int = Unsafe.cast
val chr : int -> char = fn x => if x < 0 orelse x > 255 then
                                    raise Chr
                                else
                                    Unsafe.cast x : char
fun succ c = chr (ord c + 1)
fun pred c = chr (ord c - 1)
fun compare (x : char, y : char) = if x = y then
                                       EQUAL
                                   else if x < y then
                                       LESS
                                   else
                                       GREATER
fun contains (s : string) (c : char) : bool = JavaScript.unsafeFromValue (JavaScript.method (JavaScript.unsafeToValue s, "includes") #[JavaScript.unsafeToValue c])
fun notContains s c = not (contains s c)
fun isAscii (c : char) = c <= #"\127"
fun isUpper (c : char) = #"A" <= c andalso c <= #"Z"
fun isLower (c : char) = #"a" <= c andalso c <= #"z"
fun isDigit (c : char) = #"0" <= c andalso c <= #"9"
fun isAlpha (c : char) = isUpper c orelse isLower c
fun isAlphaNum (c : char) = isAlpha c orelse isDigit c
fun isHexDigit (c : char) = isDigit c orelse (#"a" <= c andalso c <= #"f") orelse (#"A" <= c andalso c <= #"Z")
fun isGraph (c : char) = #"!" <= c andalso c <= #"~"
fun isPrint (c : char) = isGraph c orelse c = #" "
fun isPunct (c : char) = isGraph c andalso not (isAlphaNum c)
fun isCntrl (c : char) = isAscii c andalso not (isPrint c)
fun isSpace (c : char) = (#"\t" <= c andalso c <= #"\r") orelse c = #" "
fun toLower (c : char) = if isUpper c then
                             chr (ord c - (ord #"A" - ord #"a"))
                         else
                             c
fun toUpper (c : char) = if isLower c then
                             chr (ord c - (ord #"a" - ord #"A"))
                         else
                             c
fun toString #"\\" = "\\\\"
  | toString #"\"" = "\\\""
  | toString c = if isPrint c then
                     String.str c
                 else
                     case c of
                         #"\a" => "\\a"
                       | #"\b" => "\\b"
                       | #"\t" => "\\t"
                       | #"\n" => "\\n"
                       | #"\v" => "\\v"
                       | #"\f" => "\\f"
                       | #"\r" => "\\r"
                       | _ => let val x = ord c
                              in if x < 32 then
                                     "\\^" ^ String.str (chr (x + 64))
                                 else if x < 100 then
                                     "\\0" ^ Int.toString x
                                 else
                                     "\\" ^ Int.toString x
                                 (* TODO: x >= 1000 *)
                              end
fun toCString #"\\" = "\\\\"
  | toCString #"\"" = "\\\""
  | toCString #"?" = "\\?"
  | toCString #"'" = "\\'"
  | toCString c = if isPrint c then
                      String.str c
                  else
                      case c of
                          #"\a" => "\\a"
                        | #"\b" => "\\b"
                        | #"\t" => "\\t"
                        | #"\n" => "\\n"
                        | #"\v" => "\\v"
                        | #"\f" => "\\f"
                        | #"\r" => "\\r"
                        | _ => let val x = ord c
                                   val s = Int.fmt StringCvt.OCT x
                               in if x < 8 then
                                      "\\00" ^ s
                                  else if x < 64 then
                                      "\\0" ^ s
                                  else
                                      "\\" ^ s
                                  (* TODO: x >= 512 *)
                               end
open Char (* <, <=, >, >= *)
(* scan, fromString, toCString, fromCString *)
end (* structure Char *)

structure StringCvt :> STRING_CVT where type radix = StringCvt.radix
                                  where type realfmt = StringCvt.realfmt = struct
open StringCvt
fun padLeft c i s = if String.size s >= i orelse i <= 0 then
                        s
                    else
                        let val c = String.str c
                            fun loop (j, acc) = if j <= 0 then
                                                    String.concat acc
                                                else
                                                    loop (j - 1, c :: acc)
                        in loop (i - String.size s, [s])
                        end
fun padRight c i s = if String.size s >= i orelse i <= 0 then
                         s
                     else
                         let val c = String.str c
                             fun loop (j, acc) = if j <= 0 then
                                                     String.concat (s :: acc)
                                                 else
                                                     loop (j - 1, c :: acc)
                         in loop (i - String.size s, [])
                         end
fun splitl f rdr src = let fun loop (acc, src) = case rdr src of
                                                     NONE => (String.implodeRev acc, src)
                                                   | SOME (x, src') => loop (x :: acc, src')
                       in loop ([], src)
                       end
fun takel f rdr s = #1 (splitl f rdr s)
fun dropl f rdr s = #2 (splitl f rdr s)
fun skipWS rdr = dropl Char.isSpace rdr
type cs = string * int (* the underlying string, the starting index *)
fun scanString scan s = case scan (fn (s, i) => if i < String.size s then
                                                    SOME (String.sub (s, i), (s, i + 1))
                                                else
                                                    NONE
                                  ) (s, 0) of
                            SOME (x, _) => SOME x
                          | NONE => NONE
end

signature STRING = sig
    eqtype string
    eqtype char
    val maxSize : int
    val size : string -> int
    val sub : string * int -> char
    val extract : string * int * int option -> string
    val substring : string * int * int -> string
    val ^ : string * string -> string
    val concat : string list -> string
    val concatWith : string -> string list -> string
    val str : char -> string
    val implode : char list -> string
    val explode : string -> char list
    val map : (char -> char) -> string -> string
    val translate : (char -> string) -> string -> string
    val tokens : (char -> bool) -> string -> string list
    val fields : (char -> bool) -> string -> string list
    val isPrefix : string -> string -> bool
    (* val isSubstring : string -> string -> bool *)
    val isSuffix : string -> string -> bool
    val compare : string * string -> order
    (* val collate : (char * char -> order) -> string * string -> order *)
    val < : string * string -> bool
    val <= : string * string -> bool
    val > : string * string -> bool
    val >= : string * string -> bool
    val toString : string -> string
    (* val scan : (Char.char, 'a) StringCvt.reader -> (string, 'a) StringCvt.reader; implemented in scan-text.sml *)
    (* val fromString : String.string -> string option; implemented in scan-text.sml *)
    val toCString : string -> String.string
    (* val fromCString : String.string -> string option *)
    (* from https://github.com/SMLFamily/BasisLibrary/wiki/2015-003d-STRING: *)
    (* val rev : string -> string *)
    val implodeRev : char list -> string
    (* val concatWithMap : string -> ('a -> string) -> 'a list -> string *)
end;

structure String :> STRING where type string = string where type char = Char.char = struct
open String
val maxSize = 0x7fffffff
fun toString s = translate Char.toString s
fun toCString s = translate Char.toCString s
end;

structure Vector : sig
              datatype vector = datatype vector
              val maxLen : int
              val fromList : 'a list -> 'a vector
              val tabulate : int * (int -> 'a) -> 'a vector
              val length : 'a vector -> int
              val sub : 'a vector * int -> 'a
              val update : 'a vector * int * 'a -> 'a vector
              val concat : 'a vector list -> 'a vector
              val appi : (int * 'a -> unit) -> 'a vector -> unit
              val app : ('a -> unit) -> 'a vector -> unit
              val mapi : (int * 'a -> 'b) -> 'a vector -> 'b vector
              val map : ('a -> 'b) -> 'a vector -> 'b vector
              val foldli : (int * 'a * 'b -> 'b) -> 'b -> 'a vector -> 'b
              val foldri : (int * 'a * 'b -> 'b) -> 'b -> 'a vector -> 'b
              val foldl : ('a * 'b -> 'b) -> 'b -> 'a vector -> 'b
              val foldr : ('a * 'b -> 'b) -> 'b -> 'a vector -> 'b
              val findi : (int * 'a -> bool) -> 'a vector -> (int * 'a) option
              val find : ('a -> bool) -> 'a vector -> 'a option
              val exists : ('a -> bool) -> 'a vector -> bool
              val all : ('a -> bool) -> 'a vector -> bool
              (* val collate : ('a * 'a -> order) -> 'a vector * 'a vector -> order; defined later *)
          end = struct
open Vector
val maxLen = 0x7fffffff
end;
