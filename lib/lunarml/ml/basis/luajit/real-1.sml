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
val precision : int = 53 (* Assume binary64 *)
val posInf = Lua.unsafeFromValue Lua.Lib.math.huge : real
val negInf = Real.~ posInf
fun == (x, y) = Lua.== (Lua.fromReal x, Lua.fromReal y)
fun != (x, y) = Lua.~= (Lua.fromReal x, Lua.fromReal y)
infix 4 == !=
fun isNan x = x != x
fun ?= (x, y) = x == y orelse x != x orelse y != y (* EQUAL or UNORDERED *)
fun unordered (x, y) = x != x orelse y != y
fun isFinite x = negInf < x andalso x < posInf
val maxFinite = 0x1.fffffffffffffp1023 : real; (* approx. 1.7976931348623157e308; assuming binary64 *)
val minPos = 0x1p~1074 : real; (* approx. 5e~324; assuming binary64 *)
val minNormalPos = 0x1p~1022 : real; (* approx. 2.2250738585072014e~308; assuming binary64 *)
fun isNormal x = let val absX = abs x
                 in minNormalPos <= absX andalso absX < posInf
                 end
fun class x = if x == 0.0 then
                  IEEEReal.ZERO
              else
                  let val absX = abs x
                  in if absX < posInf then
                         (* normal or subnormal *)
                         if minNormalPos <= absX then
                             IEEEReal.NORMAL
                         else
                             IEEEReal.SUBNORMAL
                     else
                         (* infinity or NaN *)
                         if x != x then
                             IEEEReal.NAN
                         else
                             IEEEReal.INF
                  end
fun rem (x : real, y : real) : real = Lua.unsafeFromValue (Lua.call1 Lua.Lib.math.fmod #[Lua.fromReal x, Lua.fromReal y])
fun min (x : real, y : real) = if x < y then
                                   x
                               else if y < x then
                                   y
                               else if isNan x then
                                   y
                               else if isNan y then
                                   x
                               else if x == 0.0 then (* x == 0.0 andalso y == 0.0 *)
                                   ~ (~ x - y) (* Assume 0.0 + ~0.0 = ~0.0 + 0.0 = 0.0 *)
                               else (* x == y *)
                                   x
fun max (x : real, y : real) = if x < y then
                                   y
                               else if y < x then
                                   x
                               else if isNan x then
                                   y
                               else if isNan y then
                                   x
                               else if x == 0.0 then (* x == 0.0 andalso y == 0.0 *)
                                   x + y (* Assume 0.0 + ~0.0 = ~0.0 + 0.0 = 0.0 *)
                               else (* x == y *)
                                   x
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
(* TODO: We have math.frexp *)
(* Assumption: 2^exp is exact *)
fun toManExp x = let val a = abs x
                 in if a == 0.0 orelse a == posInf orelse isNan a then
                        { man = x, exp = 0 }
                    else
                        let val e0 : int = Lua.unsafeFromValue (Lua.call1 Lua.Lib.math.floor #[Lua.call1 Lua.Lib.math.log #[Lua.fromReal a, Lua.fromInt 2]]) - 1
                            fun fixup e = let val lower = Lua.unsafeFromValue (Lua.pow (Lua.fromReal 2.0, Lua.fromInt (e - 1))) : real
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
(* TODO: We have math.ldexp *)
(* Assumption: 2^exp is exact *)
fun fromManExp { man : real, exp : int } = if ~1022 <= exp then
                                               if exp < 1024 then
                                                   man * Lua.unsafeFromValue (Lua.pow (Lua.fromReal 2.0, Lua.fromInt exp))
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
                                                      let val s = Lua.unsafeFromValue (Lua.pow (Lua.fromReal 2.0, Lua.fromInt j))
                                                      in fromManExp { man = man * s, exp = exp' - j }
                                                      end
                                                  else
                                                      fromManExp { man = man * 0x1p~1022, exp = exp' + 1022 }
                                               end (* Avoid undue underflow and double rounding *)
(* LuaJIT's math.modf calls C's modf *)
fun split x = let val (intPart, fracPart) = Lua.call2 Lua.Lib.math.modf #[Lua.fromReal x]
              in { whole = Lua.unsafeFromValue intPart : real, frac = Lua.unsafeFromValue fracPart : real }
              end
fun realMod x = let val (_, fracPart) = Lua.call2 Lua.Lib.math.modf #[Lua.fromReal x]
                in Lua.unsafeFromValue fracPart : real
                end
fun checkFloat x = if isNan x then
                       raise Div
                   else if x == posInf orelse x == negInf then
                       raise Overflow
                   else
                       x
val realFloor : real -> real = Lua.unsafeFromValue Lua.Lib.math.floor
val realCeil : real -> real = Lua.unsafeFromValue Lua.Lib.math.ceil
fun realTrunc x = let val result = Lua.call1 Lua.Lib.math.modf #[Lua.fromReal x]
                  in Lua.unsafeFromValue result : real
                  end
fun realRound x = let val (intPartRaw, fracPart) = Lua.call2 Lua.Lib.math.modf #[Lua.fromReal x]
                      val intPartIsEven = Lua.== (Lua.% (intPartRaw, Lua.fromInt 2), Lua.fromInt 0)
                      val intPart = Lua.unsafeFromValue intPartRaw : real
                      val fracPart = Lua.unsafeFromValue fracPart : real
                      val absFracPart = abs fracPart
                  in if ~0.5 < fracPart andalso fracPart < 0.5 then
                         if intPart == 0.0 andalso 1.0 / x < 0.0 then
                             ~0.0 (* negative zero *)
                         else
                             (* intPart may be infinity *)
                             intPart
                     else if fracPart < ~0.5 orelse (fracPart == ~0.5 andalso not intPartIsEven)then
                         intPart - 1.0
                     else if fracPart > 0.5 orelse (fracPart == 0.5 andalso not intPartIsEven) then
                         intPart + 1.0
                     else (* ((fracPart == 0.5 orelse fracPart == ~0.5) andalso intPartIsEven) orelse isNan x *)
                         intPart
                  end
fun isInt x = Lua.== (Lua.call1 Lua.Lib.bit.tobit #[x], x)
fun floor x = let val result = Lua.call1 Lua.Lib.math.floor #[Lua.fromReal x]
              in if isInt result then
                     Lua.unsafeFromValue result : int
                 else
                     if isNan x then
                         raise Domain (* NaN *)
                     else
                         raise Overflow
              end
fun ceil x = let val result = Lua.call1 Lua.Lib.math.ceil #[Lua.fromReal x]
             in if isInt result then
                    Lua.unsafeFromValue result : int
                else
                    if isNan x then
                        raise Domain (* NaN *)
                    else
                        raise Overflow
             end
fun trunc x = let val result = Lua.call1 Lua.Lib.math.modf #[Lua.fromReal x]
              in if isInt result then
                     Lua.unsafeFromValue result : int
                 else
                     if isNan x then
                         raise Domain (* NaN *)
                     else
                         raise Overflow
              end
fun round x = let val (intPartRaw, fracPart) = Lua.call2 Lua.Lib.math.modf #[Lua.fromReal x]
              in if isInt intPartRaw then
                     let val intPartIsEven = Lua.== (Lua.% (intPartRaw, Lua.fromInt 2), Lua.fromInt 0)
                         val intPart = Lua.unsafeFromValue intPartRaw : int
                         val fracPart = Lua.unsafeFromValue fracPart : real
                         val absFracPart = abs fracPart
                     in if ~0.5 < fracPart andalso fracPart < 0.5 then
                            intPart
                        else if fracPart < ~0.5 orelse (fracPart == ~0.5 andalso not intPartIsEven)then
                            intPart - 1
                        else if fracPart > 0.5 orelse (fracPart == 0.5 andalso not intPartIsEven) then
                            intPart + 1
                        else (* ((fracPart == 0.5 orelse fracPart == ~0.5) andalso intPartIsEven) orelse isNan x *)
                            intPart
                     end
                 else
                     if isNan x then
                         raise Domain
                     else
                         raise Overflow
              end
fun toInt IEEEReal.TO_NEGINF = floor
  | toInt IEEEReal.TO_POSINF = ceil
  | toInt IEEEReal.TO_ZERO = trunc
  | toInt IEEEReal.TO_NEAREST = round
fun fromInt 0 = 0.0 (* input might be negative zero *)
  | fromInt x = Lua.unsafeFromValue (Lua.fromInt x) : real
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
                                        let val fmt = Lua.call1 Lua.Lib.string.format #[Lua.fromString "%%.%dE", Lua.fromInt prec]
                                            val result = Lua.call1 Lua.Lib.string.format #[fmt, Lua.fromReal r]
                                            val result = Lua.call1 Lua.Lib.string.gsub #[result, Lua.fromString "-", Lua.fromString "~"]
                                        in Lua.unsafeFromValue result : string
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
                                        let val fmt = Lua.call1 Lua.Lib.string.format #[Lua.fromString "%%.%df", Lua.fromInt prec]
                                            val result = Lua.call1 Lua.Lib.string.format #[fmt, Lua.fromReal r]
                                            val result = Lua.call1 Lua.Lib.string.gsub #[result, Lua.fromString "-", Lua.fromString "~"]
                                        in Lua.unsafeFromValue result : string
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
                                        let val fmt = Lua.call1 Lua.Lib.string.format #[Lua.fromString "%%.%dG", Lua.fromInt prec] (* TODO *)
                                            val result = Lua.call1 Lua.Lib.string.format #[fmt, Lua.fromReal r]
                                            val result = Lua.call1 Lua.Lib.string.gsub #[result, Lua.fromString "-", Lua.fromString "~"]
                                        in Lua.unsafeFromValue result : string
                                        end
                                 end
  | fmt StringCvt.EXACT r = raise Fail "Real.fmt StringCvt.EXACT: not implemented yet"
val toString = fmt (StringCvt.GEN NONE)
open Real (* +, -, *, /, ~, abs, <, <=, >, >= *)
end; (* structure Real *)
