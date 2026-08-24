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
    (* val rem : real * real -> real *)
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
    (* val fmt : StringCvt.realfmt -> real -> string *)
    (* val toString : real -> string *)
    (* val scan : (char, 'a) StringCvt.reader -> (real, 'a) StringCvt.reader; implemented in scan-num.sml *)
    (* val fromString : string -> real option; implemented in scan-num.sml *)
    (* val toDecimal : real -> IEEEReal.decimal_approx *)
    (* val fromDecimal : IEEEReal.decimal_approx -> real option *)
end;

structure Real : REAL where type real = real = struct
val radix : int = 2
val precision : int = 53 (* binary64 *)
val maxFinite : real = 0x1.fffffffffffffp1023
val minPos : real = 0x1p~1074
val minNormalPos : real = 0x1p~1022
val posInf : real = _primCall "Real.posInf" ()
val negInf : real = _primCall "Real.negInf" ()
fun == (x, y) = _primCall "Real.==" (x, y)
fun != (x, y) = not (_primCall "Real.==" (x, y))
infix 4 == !=
fun isNan x = x != x
fun ?= (x, y) = x == y orelse isNan x orelse isNan y (* EQUAL or UNORDERED *)
fun unordered (x, y) = isNan x orelse isNan y
fun isFinite x = negInf < x andalso x < posInf
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
fun min (x : real, y : real) = if isNan x then
                                   y
                               else if isNan y then
                                   x
                               else
                                   (* IEEE 754-2019 min: propagates NaN and honors the sign of zero *)
                                   _primCall "Real.minimum" (x, y)
fun max (x : real, y : real) = if isNan x then
                                   y
                               else if isNan y then
                                   x
                               else
                                   (* IEEE 754-2019 max: propagates NaN and honors the sign of zero *)
                                   _primCall "Real.maximum" (x, y)
fun sign x = if x == 0.0 then
                 0
             else if x < 0.0 then
                 ~1
             else if x > 0.0 then
                 1
             else (* NaN *)
                 raise Domain
fun copySign (x, y) = _primCall "Real.copySign" (x, y)
fun signBit x = copySign (1.0, x) < 0.0
fun sameSign (x, y) = copySign (1.0, x) == copySign (1.0, y)
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
fun split x = let val intPart = _primCall "Real.realTrunc" (x)
                  val fracPart = x - intPart (* Is this OK? *)
                  val frac = if isNan fracPart then (* x: infinity or NaN *)
                                 0.0 / x
                             else
                                 fracPart
              in { whole = intPart, frac = frac }
              end
fun realMod x = #frac (split x)
fun checkFloat x = if isNan x then
                       raise Div
                   else if x == posInf orelse x == negInf then
                       raise Overflow
                   else
                       x
fun realFloor x = _primCall "Real.realFloor" (x)
fun realCeil x = _primCall "Real.realCeil" (x)
fun realTrunc x = _primCall "Real.realTrunc" (x)
fun realRound x = _primCall "Real.realRound" (x)
fun resultToInt x = if isNan x then
                        raise Domain
                    else if x < ~0x80000000p0 orelse x > 0x7fffffffp0 then
                        raise Overflow
                    else
                        _primCall "Real.trunc.unchecked" (x)
fun floor x = resultToInt (realFloor x)
fun ceil x = resultToInt (realCeil x)
fun trunc x = resultToInt (realTrunc x)
fun round x = resultToInt (realRound x)
fun toInt IEEEReal.TO_NEGINF = floor
  | toInt IEEEReal.TO_POSINF = ceil
  | toInt IEEEReal.TO_ZERO = trunc
  | toInt IEEEReal.TO_NEAREST = round
fun fromInt (x : int) : real = _primCall "Real.fromInt" (x)
fun toManExp (x : real) : { man : real, exp : int } =
  if x == 0.0 then
    { man = x, exp = 0 }
  else
    let
      (* Goal: x = man * radix^exp, 1.0 / radix <= man < 1 *)
      (* radix^(exp-1) <= x < radix^exp *)
      val bits = _primCall "Real.reinterpretAsWord64" (x)
      val biasedExp = Word64.andb (bits, 0wx7ff0_0000_0000_0000) .Word64.>>. 0w52
      val trailingSignificand = Word64.andb (bits, 0wx000f_ffff_ffff_ffff)
    in
      if biasedExp = 0wx7ff then
        (* infinity, NaN *)
        { man = x, exp = 0 (* unspecified *) }
      else if biasedExp = 0w0 then
        (* subnormal *)
        (* abs x = trailingSignificand * 0x1p-1074 *)
        let
          fun loop i =
            if (trailingSignificand .Word64.>>. i) > 0w0 then
              (* 2^i <= trailingSignificand < 2^(i+1) *)
              (* 2^(i-1074) <= abs x < 2^(i-1074+1) *)
              let
                val exp = Word.toInt i - 1073
                val manWord64 = Word64.orb (0wx3fe0_0000_0000_0000, Word64.andb (trailingSignificand .Word64.<<. (0w52 - i), 0wx000f_ffff_ffff_ffff))
                val manAbs = _primCall "Real.reinterpretFromWord64" (manWord64)
              in { man = copySign (manAbs, x), exp = exp }
              end
            else
              loop (i - 0w1)
        in loop 0w51
        end
      else
        (* normal *)
        (* abs x = (2^53 + trailingSignificand) * 2^(biasedExp - 1023 - 53) *)
        (* 2^(biasedExp - 1023) <= abs x < 2^(biasedExp - 1023 + 1) *)
        let
          val exp = Word64.toInt biasedExp - 1022
          val manWord64 = Word64.orb (0wx3fe0_0000_0000_0000, trailingSignificand)
          val manAbs = _primCall "Real.reinterpretFromWord64" (manWord64)
        in { man = copySign (manAbs, x), exp = exp }
        end
    end
fun fromManExp { man : real, exp : int } : real =
  if man == 0.0 then
    man
  else
    let
      (* Goal: x = man * radix^exp, 1.0 / radix <= man < 1 *)
      (* radix^(exp-1) <= x < radix^exp *)
      val bits = _primCall "Real.reinterpretAsWord64" (man)
      val biasedExp = Word64.andb (bits, 0wx7ff0_0000_0000_0000) .Word64.>>. 0w52
      val trailingSignificand = Word64.andb (bits, 0wx000f_ffff_ffff_ffff)
    in
      if biasedExp = 0wx7ff then
        (* infinity, NaN *)
        man
      else
        let
          val (trailingSignificand, biasedExp') : Word64.word * int =
            if biasedExp = 0w0 then
              (* subnormal *)
              (* abs x = trailingSignificand * 0x1p-1074 *)
              let
                fun loop i =
                  if (trailingSignificand .Word64.>>. i) > 0w0 then
                    (* 2^i <= trailingSignificand < 2^(i+1) *)
                    (* 2^(i-1074) <= abs x < 2^(i-1074+1) *)
                    let
                      val biasedExp' = Word.toInt i - 51
                      val trailingSignificand = Word64.andb (trailingSignificand .Word64.<<. (0w52 - i), 0wx000f_ffff_ffff_ffff)
                    in (trailingSignificand, biasedExp')
                    end
                  else
                    loop (i - 0w1)
              in loop 0w51
              end
            else
              (* normal *)
              (trailingSignificand, Word64.toInt biasedExp)
        in
          (* abs x = (2^53 + trailingSignificand) * 2^(biasedExp - 1023 - 53) *)
          (* 2^(biasedExp - 1023) <= abs x < 2^(biasedExp - 1023 + 1) *)
          let
            val biasedExp'' = biasedExp' + exp
          in
            if biasedExp'' <= 0 then
              raise Fail "not implemented yet (underflow case)"
            else if biasedExp'' >= 0x7ff then
              raise Fail "not implemented yet (overflow case)"
            else
              let
                val resultWord64 = Word64.orb (Word64.fromInt biasedExp'' .Word64.<<. 0w52, trailingSignificand)
                val resultAbs = _primCall "Real.reinterpretFromWord64" (resultWord64)
              in copySign (resultAbs, man)
              end
          end
        end
    end
open Real (* +, -, *, /, ~, abs, <, <=, >, >= *)
end; (* structure Real *)
