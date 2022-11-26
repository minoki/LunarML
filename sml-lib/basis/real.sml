structure LargeReal = Real
signature REAL = sig
    include REAL
    val toLargeInt : IEEEReal.rounding_mode -> real -> LargeInt.int
    val fromLargeInt : LargeInt.int -> real
    val toLarge : real -> LargeReal.real
    val fromLarge : IEEEReal.rounding_mode -> LargeReal.real -> real
end;
structure Real : REAL = struct
local
    infix 4 ==
    val op == = Real.==
    fun toLargeIntImpl (x : real) : LargeInt.int = if x == Real.posInf orelse x == Real.negInf then
                                                       raise Overflow
                                                   else if Real.isNan x then
                                                       raise Domain
                                                   else (* x is normal, integral value *)
                                                       IntInfImpl.fromIntegralReal x
in
fun toLargeInt IEEEReal.TO_NEAREST = toLargeIntImpl o Real.realRound
  | toLargeInt IEEEReal.TO_NEGINF = toLargeIntImpl o Real.realFloor
  | toLargeInt IEEEReal.TO_POSINF = toLargeIntImpl o Real.realCeil
  | toLargeInt IEEEReal.TO_ZERO = toLargeIntImpl o Real.realTrunc
val fromLargeInt = IntInfImpl.toReal
fun toLarge (x : real) = x
fun fromLarge (_ : IEEEReal.rounding_mode) (x : real) = x
end
open Real
end;
structure LargeReal = Real;
