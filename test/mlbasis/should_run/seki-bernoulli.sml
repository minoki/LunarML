structure Rational :> sig
              eqtype rational
              val fromInt : Int.int -> rational
              val fromIntInf : IntInf.int -> rational
              val makeRational : IntInf.int * IntInf.int -> rational
              val numerator : rational -> IntInf.int
              val denominator : rational -> IntInf.int
              val + : rational * rational -> rational
              val - : rational * rational -> rational
              val * : rational * rational -> rational
              val / : rational * rational -> rational
              val abs : rational -> rational
              val ~ : rational -> rational
              val compare : rational * rational -> order
              val < : rational * rational -> bool
              val <= : rational * rational -> bool
              val > : rational * rational -> bool
              val >= : rational * rational -> bool
              val toString : rational -> string
          end = struct
(* invariant: den is strictly positive, gcd (num, den) = 1 *)
type rational = { num : IntInf.int, den : IntInf.int }
fun gcd (x, y) = gcdLoop (IntInf.abs x, IntInf.abs y)
and gcdLoop (x, y) = if y = IntInf.fromInt 0 then
                         x
                     else
                         gcdLoop (y, IntInf.rem (x, y))
fun makeRational (x, y) = if y = IntInf.fromInt 0 then
                              raise Div
                          else
                              let val g = gcd (x, y)
                              in if y < IntInf.fromInt 0 then
                                     { num = (~ x) div g, den = (~ y) div g }
                                 else
                                     { num = x div g, den = y div g }
                              end
fun fromInt x = { num = IntInf.fromInt x, den = IntInf.fromInt 1 }
fun fromIntInf (x : IntInf.int) = { num = x, den = IntInf.fromInt 1 }
val numerator = #num : rational -> IntInf.int
val denominator = #den : rational -> IntInf.int
fun add (x, y) = makeRational (numerator x * denominator y + numerator y * denominator x, denominator x * denominator y)
fun sub (x, y) = makeRational (numerator x * denominator y - numerator y * denominator x, denominator x * denominator y)
fun mul (x, y) = makeRational (numerator x * numerator y, denominator x * denominator y)
fun divide (x : rational, y : rational) = makeRational (numerator x * denominator y, denominator x * numerator y)
fun abs_ { num, den } = { num = IntInf.abs num, den = den : IntInf.int }
fun negate { num, den } = { num = IntInf.~ num, den = den : IntInf.int }
fun compare (x : rational, y : rational) = IntInf.compare (numerator x * denominator y, numerator y * denominator x)
fun x < y = IntInf.< (numerator x * denominator y, numerator y * denominator x)
fun x <= y = IntInf.<= (numerator x * denominator y, numerator y * denominator x)
fun x > y = IntInf.> (numerator x * denominator y, numerator y * denominator x)
fun x >= y = IntInf.>= (numerator x * denominator y, numerator y * denominator x)
fun toString x = IntInf.toString (numerator x) ^ "/" ^ IntInf.toString (denominator x)
val op + = add
val op - = sub
val op * = mul
val op / = divide
val abs = abs_
val ~ = negate
end

val limit = 100
val bern = Array.array (limit, Rational.fromInt 0)
val () = Array.update (bern, 0, Rational.fromInt 1)
val () = let fun loop (n, comb) = if n >= limit then
                                      ()
                                  else
                                      let val comb = Vector.tabulate (Vector.length comb + 1, fn k => if k = 0 orelse k = Vector.length comb then
                                                                                                          IntInf.fromInt 1
                                                                                                      else
                                                                                                          Vector.sub (comb, k - 1) + Vector.sub (comb, k)
                                                                     )
                                          val sum = let fun s (k, acc) = if k >= n then
                                                                             acc
                                                                         else
                                                                             s (k + 1, Rational.+ (acc, Rational.* (Rational.fromIntInf (Vector.sub (comb, k)), Array.sub (bern, k))))
                                                    in s (0, Rational.fromInt 0)
                                                    end
                                          val bern_n = Rational.~ (Rational./ (sum, Rational.fromInt (n + 1)))
                                      in Array.update (bern, n, bern_n)
                                       ; loop (n + 1, comb)
                                      end
         in loop (1, vector [IntInf.fromInt 1, IntInf.fromInt 1])
         end
val () = Array.appi (fn (i, x) => print ("B[" ^ Int.toString i ^ "]=" ^ Rational.toString x ^ "\n")) bern;
