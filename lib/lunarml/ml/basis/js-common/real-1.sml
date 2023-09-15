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
                        Unsafe.cast (JavaScript.toInt32 (JavaScript.fromReal x)) : int
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
                                 in if isNan r then
                                        "nan"
                                    else if r == posInf then
                                        "inf"
                                    else if r == negInf then
                                        "~inf"
                                    else
                                        let val result = JavaScript.method (JavaScript.fromReal r, "toExponential") #[JavaScript.fromInt prec] (* TODO: Is this OK? *)
                                            val result = JavaScript.method (result, "replaceAll") #[JavaScript.fromWideString "-", JavaScript.fromWideString "~"]
                                        in JavaScript.encodeUtf8 (JavaScript.unsafeFromValue result : WideString.string)
                                        end
                                 end
  | fmt (StringCvt.FIX prec) r = let val prec = Option.getOpt (prec, 6)
                                     val () = if prec < 0 then
                                                  raise Size
                                              else
                                                  ()
                                 in if isNan r then
                                        "nan"
                                    else if r == posInf then
                                        "inf"
                                    else if r == negInf then
                                        "~inf"
                                    else
                                        let val result = JavaScript.method (JavaScript.fromReal r, "toFixed") #[JavaScript.fromInt prec] (* TODO: Is this OK? *)
                                            val result = JavaScript.method (result, "replaceAll") #[JavaScript.fromWideString "-", JavaScript.fromWideString "~"]
                                        in JavaScript.encodeUtf8 (JavaScript.unsafeFromValue result : WideString.string)
                                        end
                                 end
  | fmt (StringCvt.GEN prec) r = let val prec = Option.getOpt (prec, 12)
                                     val () = if prec < 1 then
                                                  raise Size
                                              else
                                                  ()
                                 in if isNan r then
                                        "nan"
                                    else if r == posInf then
                                        "inf"
                                    else if r == negInf then
                                        "~inf"
                                    else
                                        let val result = JavaScript.method (JavaScript.fromReal r, "toPrecision") #[JavaScript.fromInt prec] (* TODO: Is this OK? *)
                                            val result = JavaScript.method (result, "replaceAll") #[JavaScript.fromWideString "-", JavaScript.fromWideString "~"]
                                            val result = JavaScript.method (result, "toUpperCase") #[]
                                        in JavaScript.encodeUtf8 (JavaScript.unsafeFromValue result : WideString.string)
                                        end
                                 end
  | fmt StringCvt.EXACT r = raise Fail "Real.fmt StringCvt.EXACT: not implemented yet"
val toString = fmt (StringCvt.GEN NONE)
open Real (* +, -, *, /, ~, abs, <, <=, >, >= *)
end; (* structure Real *)
