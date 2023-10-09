(*
 * Copyright (c) 2023 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure Numeric :> sig
              type float_format
              val binary32 : float_format
              val binary64 : float_format
              datatype float_notation = DecimalNotation of { sign : bool (* true if negative *)
                                                           , intPart : IntInf.int
                                                           , fracPart : int vector (* 0 <= _ <= 9 *)
                                                           , exponent : int (* decimal *)
                                                           }
                                      | HexadecimalNotation of { sign : bool
                                                               , intPart : IntInf.int
                                                               , fracPart : int vector (* 0 <= _ <= 15 *)
                                                               , exponent : int (* binary *)
                                                               }
              val checkExactness : float_format -> float_notation -> bool
              val toDecimal : { nominal_format : float_format, target_format : float_format } -> float_notation -> float_notation option
              structure Notation : sig
                            datatype t = datatype float_notation
                            val isNegative : float_notation -> bool
                            val isNegativeZero : float_notation -> bool
                            val toString : string -> float_notation -> string
                            val abs : float_notation -> float_notation
                        end
          end = struct
datatype radix = BINARY | DECIMAL
type float_format = { radix : radix
                    , precision : int (* 53 for Real64 *)
                    , maxExponent : int (* emax+1; 1024 for Real64 *)
                    }
(* val binary16 : float_format = { radix = BINARY, precision = 11, maxExponent = 16 } *)
val binary32 : float_format = { radix = BINARY, precision = 24, maxExponent = 128 }
val binary64 : float_format = { radix = BINARY, precision = 53, maxExponent = 1024 }
(*
val binary128 : float_format = { radix = BINARY, precision = 113, maxExponent = 16384 }
val bfloat16 : float_format = { radix = BINARY, precision = 8, maxExponent = 128 }
val x87_binary80 : float_format = { radix = BINARY, precision = 64, maxExponent = 16384 }
val decimal32 : float_format = { radix = DECIMAL, precision = 7, maxExponent = 97 }
val decimal64 : float_format = { radix = DECIMAL, precision = 16, maxExponent = 385 }
val decimal128 : float_format = { radix = DECIMAL, precision = 34, maxExponent = 6144 }
*)

datatype float_notation = DecimalNotation of { sign : bool (* true if negative *)
                                             , intPart : IntInf.int
                                             , fracPart : int vector (* 0 <= _ <= 9 *)
                                             , exponent : int (* decimal *)
                                             }
                        | HexadecimalNotation of { sign : bool
                                                 , intPart : IntInf.int
                                                 , fracPart : int vector (* 0 <= _ <= 15 *)
                                                 , exponent : int (* binary *)
                                                 }

datatype float_value = Number of { sign : bool
                                 , value : IntInf.int (* 0 <= value < radix^precision, normal: radix^(precision-1) <= value *)
                                 , exponent : int (* minExponent - precision <= _ <= maxExponent - precision *)
                                 } (* value * radix^exponent *)
                     | Infinity of bool
                     | NaN

local fun computeLog10 (acc, x) = if x < 10 then
                                      acc
                                  else
                                      computeLog10 (acc + 1, IntInf.rem (x, 10))
in
fun log10 (x : IntInf.int) = if x <= 0 then
                                 raise Domain
                             else
                                 computeLog10 (0, x)
end
fun ratioToFloat ({ radix, precision, maxExponent } : float_format) (sign, n, d) : { exact : bool, value : float_value }
    (* precondition: n > 0, d > 0 *)
    = let val minExponent = 3 - maxExponent (* -1021 for Real64 *)
          val e0 = case radix of
                       BINARY => IntInf.log2 n - IntInf.log2 d - precision
                     | DECIMAL => log10 n - log10 d - precision
          val base = case radix of
                         BINARY => 2
                       | DECIMAL => 10
          val (d0, (q0, r0)) = if e0 >= 0 then
                                   let val d' = d * IntInf.pow (base, e0)
                                   in (d', IntInf.quotRem (n, d'))
                                   end
                               else
                                   (d, IntInf.quotRem (n * IntInf.pow (base, ~ e0), d))
          (* invariant: n / d * base^(-e0) = q0 + r0 / d0 *)
          val (q, r, d', e) = if q0 < IntInf.pow (base, precision) then
                                  (q0, r0, d0, e0)
                              else
                                  let val (q', r') = IntInf.quotRem (q0, base)
                                  in (q', r' * d0 + r0, base * d0, e0 + 1)
                                  end
          (* invariant: n / d * base^(-e) = q + r / d', base^(precision-1) <= q < base^precision, 0 <= r < d' *)
          fun make (a, e) (* precondition: 0 <= a <= base^precision, minExponent <= e + precision <= maxExponent, (minExponent = e + precision or base^(precision-1) <= a) *)
              = if a = IntInf.pow (base, precision) then
                    if e = maxExponent - precision then
                        Infinity sign
                    else
                        Number { sign = sign, value = IntInf.quot (a, base), exponent = e + 1 }
                else
                    Number { sign = sign, value = a, exponent = e }
      in if maxExponent < e + precision then
             (* overflow *)
             { exact = false
             , value = Infinity sign
             }
         else if minExponent <= e + precision then
             (* normal or infinity *)
             let val value = case IntInf.compare (base * r, d') of
                                 LESS => make (q, e)
                               | EQUAL => if IntInf.rem (q, 2) = 0 then
                                              make (q, e)
                                          else
                                              make (q + 1, e)
                               | GREATER => make (q + 1, e)
             in { exact = r = 0 andalso (case value of Number _ => true | _ => false)
                , value = value
                }
             end
         else
             (* underflow (subnormal) *)
             let val (q', r') = IntInf.quotRem (q, IntInf.pow (base, minExponent - precision - e))
                 val cmp = case IntInf.compare (r', IntInf.pow (base, minExponent - precision - e - 1)) of
                               EQUAL => if r = 0 then EQUAL else GREATER
                             | lessOrGreater => lessOrGreater
                 val e' = minExponent - precision
             in { exact = r = 0 andalso r' = 0
                , value = case cmp of
                              LESS => make (q', e')
                            | EQUAL => if IntInf.rem (q', 2) = 0 then
                                           make (q', e')
                                       else
                                           make (q' + 1, e')
                            | GREATER => make (q' + 1, e')
                }
             end
      end
fun notationToRatio (DecimalNotation { sign, intPart, fracPart, exponent })
    = let val x = Vector.foldl (fn (x, acc) => acc * 10 + IntInf.fromInt x) intPart fracPart
          val e = exponent - Vector.length fracPart
          val (n, d) = if e < 0 then
                           (x, IntInf.pow (10, ~e))
                       else
                           (x * IntInf.pow (10, e), 1)
      in (sign, n, d)
      end
  | notationToRatio (HexadecimalNotation { sign, intPart, fracPart, exponent })
    = let val x = Vector.foldl (fn (x, acc) => acc * 16 + IntInf.fromInt x) intPart fracPart
          val e = exponent - 4 * Vector.length fracPart
          val (n, d) = if e < 0 then
                           (x, IntInf.<< (1, Word.fromInt (~e)))
                       else
                           (IntInf.<< (x, Word.fromInt e), 1)
      in (sign, n, d)
      end
fun checkExactness (format : float_format) (DecimalNotation _) = true (* OK *)
  | checkExactness format (notation as HexadecimalNotation { sign, intPart, fracPart, exponent }) = let val { exact, value } = ratioToFloat format (notationToRatio notation)
                                                                                                    in exact
                                                                                                    end

(*
 * Based on (FPP)^2 of [Dragon4], with modification to support subnormal
 * [Dragon4]: Guy L. Steele and Jon L. White. 1990. How to print floating-point numbers accurately. SIGPLAN Not. 25, 6 (Jun. 1990), 112–126. DOI:https://doi.org/10.1145/93548.93559
 *)
fun positiveBinaryFloatToDecimal { precision, maxExponent } { value, exponent }
    (* assumption: 0 < value < 2^precision, minExponent - precision <= exponent *)
    = let val minExponent = 3 - maxExponent
          val (value, exponent) = let val t = precision - IntInf.log2 value - 1
                                  in (IntInf.<< (value, Word.fromInt t), exponent - t)
                                  end
          val (value, exponent) = if exponent >= minExponent - precision then
                                      (value, exponent)
                                  else
                                      (IntInf.~>> (value, Word.fromInt (minExponent - precision - exponent)), minExponent - precision)
          (* let v = value * 2^exponent *)
          val (r0, s0, m0) = if exponent >= 0 then
                                 (IntInf.<< (value, Word.fromInt exponent), 1, IntInf.<< (1, Word.fromInt exponent))
                             else
                                 (value, IntInf.<< (1, Word.fromInt (~exponent)), 1)
          val (k1, r, s, m_below, m_above)
              = let fun simpleFixup (k, r, s, m_below, m_above)
                        (* invariant: r / s * 10^k = v, m_below / s * 10^k = v - nextDown v, m_above / s * 10^k = nextUp v - v *)
                        = if r < IntInf.quot (s + 9, 10) (* ceil (s / 10) *) then
                              simpleFixup (k - 1, r * 10, s, m_below * 10, m_above * 10)
                          else
                              let val limit2 = 2 * r + m_above
                                  fun simpleFixup2 (k, s)
                                      (* invariant : r / s * 10^k = v, m_below / s * 10^k = v - nextDown v, m_above / s * 10^k = nextUp v - v *)
                                      = if limit2 >= 2 * s then
                                            simpleFixup2 (k + 1, s * 10)
                                        else
                                            (* k = 1 + floor (log10 ((v + nextUp v) / 2)) *)
                                            (k, r, s, m_below, m_above)
                              in simpleFixup2 (k, s)
                              end
                in if value = IntInf.<< (1, Word.fromInt (precision - 1)) andalso exponent <> minExponent - precision then
                       simpleFixup (0, r0 * 2, s0 * 2, m0, m0 * 2)
                   else
                       simpleFixup (0, r0, s0, m0, m0)
                end
          val h = k1 - 1
          fun produceDigits (k, r, m_below, m_above)
              = let val (u, r) = IntInf.quotRem (r * 10, s)
                    val rr = 2 * r
                    val u = IntInf.toInt u
                    val m_below = m_below * 10
                    val m_above = m_above * 10
                    val low = rr < m_below
                    val high = rr > 2 * s - m_above
                in case (low, high) of
                       (false, false) => u :: produceDigits (k - 1, r, m_below, m_above)
                     | (true, false) => [u]
                     | (false, true) => [u + 1]
                     | (true, true) => (case IntInf.compare (rr, s) of
                                            LESS => [u]
                                          | GREATER => [u + 1]
                                          | EQUAL => if Int.rem (u, 2) = 0 then (* ties to even *)
                                                         [u]
                                                     else
                                                         [u + 1]
                                       )
                end
      in (h, produceDigits (k1, r, m_below, m_above))
      end

(* Assumption: #radix nominal_format = #radix target_format, #precision nominal_format <= #precision target_format, #maxExponent nominal_format <= #maxExponent target_format *)
fun toDecimal { nominal_format : float_format, target_format : float_format } (notation as DecimalNotation _) = SOME notation (* TODO: Convert to shorter representation if input is too long *)
  | toDecimal { nominal_format, target_format } (notation as HexadecimalNotation _)
    = let val { exact, value = x } = ratioToFloat nominal_format (notationToRatio notation)
      in case x of
             Number { sign, value, exponent } => if exact then
                                                     (* TODO: Check if nominal_format and target_format are binary *)
                                                     let val (decExponent, digits) = positiveBinaryFloatToDecimal { precision = #precision target_format, maxExponent = #maxExponent target_format } { value = value, exponent = exponent }
                                                     in SOME (DecimalNotation { sign = sign, intPart = IntInf.fromInt (hd digits), fracPart = Vector.fromList (tl digits), exponent = decExponent })
                                                     end
                                                 else
                                                     NONE
           | Infinity _ => NONE
           | NaN => NONE
      end

structure Notation = struct
datatype t = datatype float_notation
fun intToString negativeSign n = if n < 0 then
                                     negativeSign ^ Int.toString (~n)
                                 else
                                     Int.toString n;
fun isNegative (DecimalNotation { sign, ... }) = sign
  | isNegative (HexadecimalNotation { sign, ... }) = sign
fun isNegativeZero (DecimalNotation { sign = true, intPart = 0, fracPart, exponent = _ }) = Vector.all (fn x => x = 0) fracPart
  | isNegativeZero (HexadecimalNotation { sign = true, intPart = 0, fracPart, exponent = _ }) = Vector.all (fn x => x = 0) fracPart
  | isNegativeZero _ = false
fun toString negativeSign (DecimalNotation { sign, intPart, fracPart, exponent })
    = let val s = if sign then negativeSign else ""
          val ip = IntInf.toString intPart
          val fp = Vector.foldr (fn (x, acc) => Int.toString x ^ acc) "" fracPart
      in s ^ ip ^ (if Vector.length fracPart = 0 then "" else "." ^ fp) ^ (if exponent = 0 then "" else "e" ^ intToString negativeSign exponent)
      end
  | toString negativeSign (HexadecimalNotation { sign, intPart, fracPart, exponent })
    = let val s = if sign then negativeSign else ""
          val ip = IntInf.fmt StringCvt.HEX intPart
          val fp = Vector.foldr (fn (x, acc) => Int.fmt StringCvt.HEX x ^ acc) "" fracPart
      in s ^ "0x" ^ ip ^ (if Vector.length fracPart = 0 then "" else "." ^ fp) ^ (if exponent = 0 then "" else "p" ^ intToString negativeSign exponent)
      end;
fun abs (x as DecimalNotation { sign, intPart, fracPart, exponent }) = if sign then
                                                                           DecimalNotation { sign = false, intPart = intPart, fracPart = fracPart, exponent = exponent }
                                                                       else
                                                                           x
  | abs (x as HexadecimalNotation { sign, intPart, fracPart, exponent }) = if sign then
                                                                               HexadecimalNotation { sign = false, intPart = intPart, fracPart = fracPart, exponent = exponent }
                                                                           else
                                                                               x
end
end;
