(* assumption: Int.precision = Word.wordSize, Word.wordSize is even *)

signature INT_INF = sig
    (* INTEGER *)
    eqtype int
    val toLarge : int -> int
    val fromLarge : int -> int
    val toInt : int -> Int.int
    val fromInt : Int.int -> int
    val precision : Int.int option (* = NONE *)
    val minInt : int option (* = NONE *)
    val maxInt : int option (* = NONE *)
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
    val scan : StringCvt.radix -> (char, 'a) StringCvt.reader -> (int, 'a) StringCvt.reader
    val fromString : string -> int option

    (* INT_INF *)
    val divMod : int * int -> int * int
    val quotRem : int * int -> int * int
    val pow : int * Int.int -> int
    val log2 : int -> Int.int
    val orb : int * int -> int
    val xorb : int * int -> int
    val andb : int * int -> int
    val notb : int -> int
    val << : int * Word.word -> int
    val ~>> : int * Word.word -> int
end

structure IntInfImpl :> sig
              include INT_INF
              val fromWord : Word.word -> int
              val fromWordX : Word.word -> int
              val toWord : int -> Word.word
              val fromWord8 : Word8.word -> int
              val fromWord8X : Word8.word -> int
              val toWord8 : int -> Word8.word
              val fromWord16 : Word16.word -> int
              val fromWord16X : Word16.word -> int
              val toWord16 : int -> Word16.word
              val fromWord32 : Word32.word -> int
              val fromWord32X : Word32.word -> int
              val toWord32 : int -> Word32.word
              val fromWord64 : Word64.word -> int
              val fromWord64X : Word64.word -> int
              val toWord64 : int -> Word64.word
              val fromIntegralReal : real -> int (* the input must be integral *)
              val toReal : int -> real (* use roundTiesToEven *)
          end = struct
structure Vector = struct
open Vector
val sub = Unsafe.Vector.sub
end
structure Array = struct
open Array
val sub = Unsafe.Array.sub
val update = Unsafe.Array.update
end

datatype int = ZERO
             | POSITIVE of Word.word vector (* invariant: nonempty and the last element is not zero *)
             | NEGATIVE of Word.word vector (* invariant: nonempty and the last element is not zero *)

fun mkNonNegative words = if Vector.length words = 0 then
                              ZERO
                          else
                              POSITIVE words

fun mkNonPositive words = if Vector.length words = 0 then
                              ZERO
                          else
                              NEGATIVE words

fun toLarge x = x
fun fromLarge x = x
fun toInt ZERO = 0
  | toInt (POSITIVE words) = if Vector.length words > 1 then
                                 raise Overflow
                             else
                                 Word.toInt (Vector.sub (words, 0)) (* may raise Overflow *)
  | toInt (NEGATIVE words) = if Vector.length words > 1 then
                                 raise Overflow
                             else
                                 let val w = Vector.sub (words, 0)
                                 in 2 * (~ (Word.toInt (Word.>> (w, 0w1)))) - Word.toInt (Word.andb (w, 0w1)) (* may raise Overflow *)
                                 end

fun fromInt 0 = ZERO
  | fromInt x = if x > 0 then
                    POSITIVE #[Word.fromInt x]
                else (* x < 0 *)
                    case Int.minInt of
                        SOME minInt => if x <> minInt then
                                           NEGATIVE #[Word.fromInt (abs x)]
                                       else
                                           NEGATIVE #[Word.fromInt (abs (x + 1)) + 0w1]
                      | NONE => NEGATIVE #[Word.fromInt (abs x)]

fun fromWord 0w0 = ZERO
  | fromWord w = POSITIVE #[w]
fun fromWordX w = fromInt (Word.toIntX w)
fun toWord ZERO = 0w0
  | toWord (POSITIVE words) = Vector.sub (words, 0)
  | toWord (NEGATIVE words) = ~ (Vector.sub (words, 0))
fun fromWord8 0w0 = ZERO
  | fromWord8 w = POSITIVE #[Word.fromLarge (Word8.toLarge w)]
fun fromWord8X w = fromInt (Word8.toIntX w)
fun toWord8 ZERO = 0w0
  | toWord8 (POSITIVE words) = Word8.fromLarge (Word.toLarge (Vector.sub (words, 0)))
  | toWord8 (NEGATIVE words) = ~ (Word8.fromLarge (Word.toLarge (Vector.sub (words, 0))))
fun fromWord16 0w0 = ZERO
  | fromWord16 w = POSITIVE #[Word.fromLarge (Word16.toLarge w)]
fun fromWord16X w = fromInt (Word16.toIntX w)
fun toWord16 ZERO = 0w0
  | toWord16 (POSITIVE words) = Word16.fromLarge (Word.toLarge (Vector.sub (words, 0)))
  | toWord16 (NEGATIVE words) = ~ (Word16.fromLarge (Word.toLarge (Vector.sub (words, 0))))
fun fromWord32 0w0 = ZERO
  | fromWord32 w = POSITIVE #[Word.fromLarge (Word32.toLarge w)]
fun fromWord32X w = fromInt (Word32.toIntX w)
fun toWord32 ZERO = 0w0
  | toWord32 (POSITIVE words) = Word32.fromLarge (Word.toLarge (Vector.sub (words, 0)))
  | toWord32 (NEGATIVE words) = ~ (Word32.fromLarge (Word.toLarge (Vector.sub (words, 0))))
fun fromWord64 0w0 = ZERO
  | fromWord64 w = POSITIVE #[Word.fromLarge w]
fun fromWord64X w = fromInt (Word64.toIntX w) (* assume 64-bit int *)
fun toWord64 ZERO = 0w0
  | toWord64 (POSITIVE words) = Word.toLarge (Vector.sub (words, 0))
  | toWord64 (NEGATIVE words) = ~ (Word.toLarge (Vector.sub (words, 0)))

val precision : Int.int option = NONE
val minInt : int option = NONE
val maxInt : int option = NONE

(* add2 postcondition: hi <= 0w1 *)
fun add2 (x : Word.word, y : Word.word) : { lo : Word.word, hi : Word.word }
    = let val z = x + y
      in if z < x then
             { lo = z, hi = 0w1 }
         else
             { lo = z, hi = 0w0 }
      end

(* add3: z <= 0w1 ==> hi <= 0w1 *)
fun add3 (x : Word.word, y : Word.word, z : Word.word) : { lo : Word.word, hi : Word.word }
    = let val { lo = lo1, hi = hi1 } = add2 (x, y)
          val { lo = lo2, hi = hi2 } = add2 (lo1, z)
      in { lo = lo2, hi = hi1 + hi2 }
      end

val wordSize_2 = Word.fromInt Word.wordSize div 0w2
val word_lo_mask = Word.<< (0w1, wordSize_2) - 0w1
fun mul2 (0w0, _) = { lo = 0w0, hi = 0w0 }
  | mul2 (_, 0w0) = { lo = 0w0, hi = 0w0 }
  | mul2 (x : Word.word, y : Word.word) : { lo : Word.word, hi : Word.word }
    = let val x_hi = Word.>> (x, wordSize_2)
          val x_lo = Word.andb (x, word_lo_mask)
          val y_hi = Word.>> (y, wordSize_2)
          val y_lo = Word.andb (y, word_lo_mask)
          (* x_lo * y_lo, x_hi * y_lo + x_lo * y_hi, x_hi * y_hi *)
          val lo1 = x_lo * y_lo
          val mid1 = x_hi * y_lo
          val lo2 = Word.<< (mid1, wordSize_2)
          val hi1 = Word.>> (mid1, wordSize_2)
          val mid2 = x_lo * y_hi
          val lo3 = Word.<< (mid2, wordSize_2)
          val hi2 = Word.>> (mid2, wordSize_2)
          val hi3 = x_hi * y_hi
          val { lo, hi = hi4 } = add3 (lo1, lo2, lo3)
          val hi = hi1 + hi2 + hi3 + hi4
      in { lo = lo, hi = hi }
      end

(* x * y + z *)
fun mulAdd (x : Word.word, y : Word.word, z : Word.word) : { lo : Word.word, hi : Word.word }
    = let val x_hi = Word.>> (x, wordSize_2)
          val x_lo = Word.andb (x, word_lo_mask)
          val y_hi = Word.>> (y, wordSize_2)
          val y_lo = Word.andb (y, word_lo_mask)
          val z_hi = Word.>> (z, wordSize_2)
          val z_lo = Word.andb (z, word_lo_mask)
          val lo = x_lo * y_lo + z_lo
          val mid1 = x_hi * y_lo + z_hi
          val lo' = Word.andb (lo, word_lo_mask)
          val mid2 = x_lo * y_hi + Word.>> (lo, wordSize_2)
          val hi1 = x_hi * y_hi
          val { lo = mid', hi = hi2 } = add2 (mid1, mid2)
      in { lo = lo' + Word.<< (mid', wordSize_2), hi = hi1 + Word.>> (mid', wordSize_2) + Word.<< (hi2, wordSize_2) }
      end

fun compareAbs (words, words') = let val m = Vector.length words
                                     val n = Vector.length words'
                                 in case Int.compare (m, n) of
                                        EQUAL => let fun loop i = if i < 0 then
                                                                      EQUAL
                                                                  else
                                                                      let val w = Vector.sub (words, i)
                                                                          val w' = Vector.sub (words', i)
                                                                      in case Word.compare (w, w') of
                                                                             EQUAL => loop (i - 1)
                                                                           | t => t
                                                                      end
                                                 in loop (m - 1)
                                                 end
                                      | t => t
                                 end

fun normalize (words : word array) : word vector
    = let val length = let fun loop i = let val i' = i - 1
                                        in if i' < 0 orelse Array.sub (words, i') <> 0w0 then
                                               i
                                           else
                                               loop i'
                                        end
                       in loop (Array.length words)
                       end
      in ArraySlice.vector (ArraySlice.slice (words, 0, SOME length))
      end

fun addAbs (words, words') = let val m = Vector.length words
                                 val n = Vector.length words'
                                 val l = Int.max (m, n) + 1
                                 val arr = Array.array (l, 0w0)
                                 val () = let (* invariant: carry <= 0w1 *)
                                              fun loop (carry, i) = if i = l then
                                                                        ()
                                                                    else
                                                                        let val w = if i < m then Vector.sub (words, i) else 0w0
                                                                            val w' = if i < n then Vector.sub (words', i) else 0w0
                                                                            val { lo = x, hi = carry } = add3 (w, w', carry)
                                                                        in Array.update (arr, i, x)
                                                                         ; loop (carry, i + 1)
                                                                        end
                                          in loop (0w0, 0)
                                 end
                             in normalize arr
                             end

(* subAbs precondition: words >= words' *)
fun subAbs (words, words') = let val m = Vector.length words
                                 val n = Vector.length words'
                                 val arr = Array.array (m, 0w0)
                                 val () = let (* invariant: carry <= 0w1 *)
                                              fun loop (carry, i) = if i = m then
                                                                        (* carry must be zero *)
                                                                        if carry = 0w0 then
                                                                            ()
                                                                        else
                                                                            raise Fail "subAbs: carry not zero"
                                                                    else
                                                                        let val w = Vector.sub (words, i)
                                                                            val w' = if i < n then Vector.sub (words', i) else 0w0
                                                                            val { lo, hi = carry } = if w < w' then
                                                                                                         { lo = w - w' - carry, hi = 0w1 }
                                                                                                     else if w > w' then
                                                                                                         { lo = w - w' - carry, hi = 0w0 }
                                                                                                     else (* w = w' *)
                                                                                                         if carry = 0w0 then
                                                                                                             { lo = 0w0, hi = 0w0 }
                                                                                                         else
                                                                                                             { lo = ~ carry, hi = 0w1 }
                                                                        in Array.update (arr, i, lo)
                                                                          ; loop (carry, i + 1)
                                                                        end
                                          in loop (0w0, 0)
                                          end
                             in normalize arr
                             end

(* naive algorithm *)
fun mulAbs (words, words') = let val m = Vector.length words
                                 val n = Vector.length words'
                                 val m_n = m + n
                                 val arr = Array.array (m_n, 0w0)
                                 val () = let fun outer j = if j >= n then
                                                                ()
                                                            else
                                                                let val v = Vector.sub (words', j)
                                                                    fun inner (i, k) = if i >= m then
                                                                                           Array.update (arr, i + j, k)
                                                                                       else
                                                                                           let val u = Vector.sub (words, i)
                                                                                               val { lo, hi } = mul2 (u, v)
                                                                                               val { lo, hi = hi' } = add3 (lo, Array.sub (arr, i + j), k)
                                                                                           in Array.update (arr, i + j, lo)
                                                                                            ; inner (i + 1, hi + hi')
                                                                                           end
                                                                in inner (0, 0w0)
                                                                 ; outer (j + 1)
                                                                end
                                          in outer 0
                                          end
                             in normalize arr
                             end

fun mulAbsSingle (words, 0w0) = #[]
  | mulAbsSingle (words, v) = let val m = Vector.length words
                                  val arr = Array.array (m + 1, 0w0)
                                  val () = let fun loop (i, k) = if i >= m then
                                                                     Array.update (arr, i, k)
                                                                 else
                                                                     let val u = Vector.sub (words, i)
                                                                         val { lo, hi } = mul2 (u, v)
                                                                         val { lo, hi = hi' } = add2 (lo, k)
                                                                     in Array.update (arr, i, lo)
                                                                      ; loop (i + 1, hi + hi')
                                                                     end
                                           in loop (0, 0w0)
                                           end
                              in normalize arr
                              end

fun negate (POSITIVE words) = NEGATIVE words
  | negate (NEGATIVE words) = POSITIVE words
  | negate x = x

fun add (ZERO, y) = y
  | add (x, ZERO) = x
  | add (POSITIVE words, POSITIVE words') = POSITIVE (addAbs (words, words'))
  | add (POSITIVE words, NEGATIVE words') = (case compareAbs (words, words') of
                                                 LESS => NEGATIVE (subAbs (words', words))
                                               | GREATER => POSITIVE (subAbs (words, words'))
                                               | EQUAL => ZERO
                                            )
  | add (NEGATIVE words, POSITIVE words') = (case compareAbs (words, words') of
                                                 LESS => POSITIVE (subAbs (words', words))
                                               | GREATER => NEGATIVE (subAbs (words, words'))
                                               | EQUAL => ZERO
                                            )
  | add (NEGATIVE words, NEGATIVE words') = NEGATIVE (addAbs (words, words'))

fun sub (x, ZERO) = x
  | sub (ZERO, POSITIVE words) = NEGATIVE words
  | sub (ZERO, NEGATIVE words) = POSITIVE words
  | sub (POSITIVE words, POSITIVE words') = (case compareAbs (words, words') of
                                                 LESS => NEGATIVE (subAbs (words', words))
                                               | GREATER => POSITIVE (subAbs (words, words'))
                                               | EQUAL => ZERO
                                            )
  | sub (POSITIVE words, NEGATIVE words') = POSITIVE (addAbs (words, words'))
  | sub (NEGATIVE words, POSITIVE words') = NEGATIVE (addAbs (words, words'))
  | sub (NEGATIVE words, NEGATIVE words') = (case compareAbs (words, words') of
                                                 LESS => POSITIVE (subAbs (words', words))
                                               | GREATER => NEGATIVE (subAbs (words, words'))
                                               | EQUAL => ZERO
                                            )

fun mul (z as ZERO, _) = z
  | mul (_, z as ZERO) = z
  | mul (POSITIVE words, POSITIVE words') = POSITIVE (mulAbs (words, words'))
  | mul (POSITIVE words, NEGATIVE words') = NEGATIVE (mulAbs (words, words'))
  | mul (NEGATIVE words, POSITIVE words') = NEGATIVE (mulAbs (words, words'))
  | mul (NEGATIVE words, NEGATIVE words') = POSITIVE (mulAbs (words, words'))

fun LT (ZERO, ZERO) = false
  | LT (ZERO, POSITIVE _) = true
  | LT (ZERO, NEGATIVE _) = false
  | LT (POSITIVE words, POSITIVE words') = compareAbs (words, words') = LESS
  | LT (POSITIVE _, (* ZERO | NEGATIVE _ *) _) = false
  | LT (NEGATIVE words, NEGATIVE words') = compareAbs (words, words') = GREATER
  | LT (NEGATIVE _, (* ZERO | POSITIVE _ *) _) = true

fun compare (ZERO, ZERO) = EQUAL
  | compare (ZERO, POSITIVE _) = LESS
  | compare (ZERO, NEGATIVE _) = GREATER
  | compare (POSITIVE words, POSITIVE words') = compareAbs (words, words')
  | compare (POSITIVE _, (* ZERO | NEGATIVE _ *) _) = GREATER
  | compare (NEGATIVE words, NEGATIVE words') = compareAbs (words', words)
  | compare (NEGATIVE _, (* ZERO | POSITIVE _ *) _) = LESS

fun pow (x : int, y : Int.int) : int = if y < 0 then
                                           if x = fromInt 1 then
                                               x
                                           else if x = fromInt ~1 then
                                               if Int.rem (y, 2) = 0 then
                                                   fromInt 1
                                               else
                                                   x
                                           else if LT (x, fromInt ~1) orelse LT (fromInt 1, x) then
                                               ZERO
                                           else
                                               raise Div
                                       else if y = 0 then
                                           fromInt 1
                                       else
                                           let fun loop (acc, _, 0) = acc
                                                 | loop (acc, m, 1) = mul (acc, m)
                                                 | loop (acc, m, y) = if y mod 2 = 0 then
                                                                          loop (acc, mul (m, m), y div 2)
                                                                      else
                                                                          loop (mul (acc, m), mul (m, m), y div 2)
                                           in loop (x, x, y - 1)
                                           end

fun log2Word 0w0 = raise Domain
  | log2Word 0w1 = 0
  | log2Word x = 1 + log2Word (x div 0w2)

fun log2Abs words = let val n = Vector.length words - 1
                    in Word.wordSize * n + log2Word (Vector.sub (words, n))
                    end

fun log2 (POSITIVE words) = log2Abs words
  | log2 _ = raise Domain

fun orbAbs (words, words') = let val m = Vector.length words
                                 val n = Vector.length words'
                                 val l = Int.max (m, n)
                                 val arr = Array.array (l, 0w0)
                                 fun loop i = if i = l then
                                                  ()
                                              else
                                                  let val w = if i < m then Vector.sub (words, i) else 0w0
                                                      val w' = if i < n then Vector.sub (words', i) else 0w0
                                                  in Array.update (arr, i, Word.orb (w, w'))
                                                   ; loop (i + 1)
                                                  end
                             in loop 0
                              ; Array.vector arr
                             end

fun xorbAbs (words, words') = let val m = Vector.length words
                                  val n = Vector.length words'
                                  val l = Int.max (m, n)
                                  val arr = Array.array (l, 0w0)
                                  fun loop i = if i = l then
                                                   ()
                                               else
                                                   let val w = if i < m then Vector.sub (words, i) else 0w0
                                                       val w' = if i < n then Vector.sub (words', i) else 0w0
                                                   in Array.update (arr, i, Word.xorb (w, w'))
                                                    ; loop (i + 1)
                                                   end
                              in loop 0
                               ; normalize arr
                              end

fun andbAbs (words, words') = let val m = Vector.length words
                                  val n = Vector.length words'
                                  val l = Int.min (m, n)
                                  val arr = Array.array (l, 0w0)
                                  fun loop i = if i = l then
                                                   ()
                                               else
                                                   let val w = Vector.sub (words, i)
                                                       val w' = Vector.sub (words', i)
                                                   in Array.update (arr, i, Word.andb (w, w'))
                                                    ; loop (i + 1)
                                                   end
                              in loop 0
                               ; normalize arr
                              end

(* words andb notb words' *)
fun andNotbAbs (words, words') = let val m = Vector.length words
                                     val n = Vector.length words'
                                     val l = m
                                     val arr = Array.array (l, 0w0)
                                     fun loop i = if i = l then
                                                      ()
                                                  else
                                                      let val w = Vector.sub (words, i)
                                                          val w' = if i < n then Vector.sub (words', i) else 0w0
                                                      in Array.update (arr, i, Word.andb (w, Word.notb w'))
                                                       ; loop (i + 1)
                                                      end
                                 in loop 0
                                  ; normalize arr
                                 end

(* notb (words xorb notb words') *)
fun notXorNotbAbs (words, words') = let val m = Vector.length words
                                        val n = Vector.length words'
                                        val l = Int.max (m, n)
                                        val arr = Array.array (l, 0w0)
                                        fun loop i = if i = l then
                                                         ()
                                                     else
                                                         let val w = if i < m then Vector.sub (words, i) else 0w0
                                                             val w' = if i < n then Vector.sub (words', i) else 0w0
                                                         in Array.update (arr, i, Word.notb (Word.xorb (w, Word.notb w')))
                                                          ; loop (i + 1)
                                                         end
                                    in loop 0
                                     ; normalize arr
                                    end

fun notb_NEGATIVE words = if Vector.length words = 1 andalso Vector.sub (words, 0) = 0w1 then
                              #[]
                          else
                              subAbs (words, #[0w1])
fun notb_NonNegative words = NEGATIVE (addAbs (#[0w1], words))

(* fun notb x = sub (fromInt ~1, x) *)
fun notb ZERO = NEGATIVE #[0w1]
  | notb (POSITIVE words) = NEGATIVE (addAbs (#[0w1], words))
  | notb (NEGATIVE words) = if Vector.length words = 1 andalso Vector.sub (words, 0) = 0w1 then
                                ZERO
                            else
                                POSITIVE (subAbs (words, #[0w1]))

fun orb (ZERO, y) = y
  | orb (x, ZERO) = x
  | orb (POSITIVE words, POSITIVE words') = POSITIVE (orbAbs (words, words'))
  | orb (POSITIVE words, NEGATIVE words') = notb_NonNegative (andNotbAbs (notb_NEGATIVE words', words))
  | orb (NEGATIVE words, POSITIVE words') = notb_NonNegative (andNotbAbs (notb_NEGATIVE words, words'))
  | orb (NEGATIVE words, NEGATIVE words') = notb_NonNegative (andbAbs (notb_NEGATIVE words, notb_NEGATIVE words'))

fun andb (z as ZERO, y) = z
  | andb (x, z as ZERO) = z
  | andb (POSITIVE words, POSITIVE words') = mkNonNegative (andbAbs (words, words'))
  | andb (POSITIVE words, NEGATIVE words') = mkNonNegative (andNotbAbs (words, notb_NEGATIVE words'))
  | andb (NEGATIVE words, POSITIVE words') = mkNonNegative (andNotbAbs (words', notb_NEGATIVE words))
  | andb (NEGATIVE words, NEGATIVE words') = notb_NonNegative (orbAbs (notb_NEGATIVE words, notb_NEGATIVE words'))

fun xorb (ZERO, y) = y
  | xorb (x, ZERO) = x
  | xorb (POSITIVE words, POSITIVE words') = mkNonNegative (xorbAbs (words, words'))
  | xorb (POSITIVE words, NEGATIVE words') = notb_NonNegative (notXorNotbAbs (words, notb_NEGATIVE words'))
  | xorb (NEGATIVE words, POSITIVE words') = notb_NonNegative (notXorNotbAbs (words', notb_NEGATIVE words))
  | xorb (NEGATIVE words, NEGATIVE words') = mkNonNegative (xorbAbs (notb_NEGATIVE words, notb_NEGATIVE words'))

fun LShiftAbs (words, amount) = let val major = amount div Word.fromInt Word.wordSize
                                    val minor = amount mod Word.fromInt Word.wordSize
                                    val nn = Vector.length words
                                    val n = nn + Word.toInt major
                                in if minor = 0w0 then
                                       Vector.tabulate (n, fn i => if i < Word.toInt major then
                                                                       0w0
                                                                   else
                                                                       Vector.sub (words, i - Word.toInt major)
                                                       )
                                   else
                                       let val m = n + 1
                                           val arr = Array.array (m, 0w0)
                                           val () = let fun loop (lo, i) = if i = nn + 1 then
                                                                               if lo = 0w0 then
                                                                                   ()
                                                                               else
                                                                                   raise Fail "LShiftAbs: carry not zero"
                                                                           else (* i < m *)
                                                                               let val w = if i < nn then Vector.sub (words, i) else 0w0
                                                                                   val v = Word.orb (Word.<< (w, minor), lo)
                                                                                   val hi = Word.>> (w, Word.fromInt Word.wordSize - minor)
                                                                               in Array.update (arr, i + Word.toInt major, v)
                                                                                ; loop (hi, i + 1)
                                                                               end
                                                    in loop (0w0, 0)
                                                    end
                                       in normalize arr
                                       end
                                end

fun RShiftAbs (words, amount) = let val major = amount div Word.fromInt Word.wordSize
                                    val minor = amount mod Word.fromInt Word.wordSize
                                    val n = Vector.length words - Word.toInt major
                                in if minor = 0w0 then
                                       ( Vector.tabulate (n, fn i => Vector.sub (words, i + Word.toInt major))
                                       , VectorSlice.exists (fn x => x <> 0w0) (VectorSlice.slice (words, 0, SOME (Word.toInt major)))
                                       )
                                   else
                                       let val arr = Array.array (n, 0w0)
                                           val hasRemainder = let fun loop (hi, i) = if i < 0 then
                                                                                         hi <> 0w0
                                                                                     else
                                                                                         let val w = Vector.sub (words, i + Word.toInt major)
                                                                                             val v = Word.orb (Word.>> (w, minor), hi)
                                                                                             val lo = Word.<< (w, Word.fromInt Word.wordSize - minor)
                                                                                         in Array.update (arr, i, v)
                                                                                          ; loop (lo, i - 1)
                                                                                         end
                                                              in loop (0w0, n - 1)
                                                              end
                                           val hasRemainder = hasRemainder orelse VectorSlice.exists (fn x => x <> 0w0) (VectorSlice.slice (words, 0, SOME (Word.toInt major)))
                                       in (normalize arr, hasRemainder)
                                       end
                                end

fun << (z as ZERO, _) = z
  | << (POSITIVE words, amount) = POSITIVE (LShiftAbs (words, amount))
  | << (NEGATIVE words, amount) = NEGATIVE (LShiftAbs (words, amount))

fun ~>> (z as ZERO, _) = z
  | ~>> (POSITIVE words, amount) = POSITIVE (#1 (RShiftAbs (words, amount)))
  | ~>> (NEGATIVE words, amount) = NEGATIVE (let val (x, y) = RShiftAbs (words, amount)
                                             in if y then
                                                    addAbs (x, #[0w1])
                                                else
                                                    x
                                             end
                                            )

fun clzWord (x : Word.word) = let fun loop (x, n) = if x = 0w0 then
                                                        n
                                                    else
                                                        loop (Word.>> (x, 0w1), n - 1)
                              in loop (x, Word.wordSize)
                              end

(* Compute (u1 * base + u0) div v *)
(* precondition: u1 < v *)
fun quot2' (u1, u0, v) = if u1 = 0w0 then
                             u0 div v
                         else
                             let val (b', b'') = let val s = Word.~ 0w1 div v
                                                     val t = Word.~ (s * v)
                                                 in if t < v then
                                                        (s, t)
                                                    else
                                                        (s + 0w1, t - v)
                                                 end (* Word.<< (1, wordSize) divMod v *)
                                 (* loop (acc, w1, w0) = acc + ((w1 << wordSize) + w0) div v *)
                                 fun loop (acc, w1, w0) = if w1 = 0w0 then
                                                              acc + w0 div v
                                                          else
                                                              let val w0' = w0 div v
                                                                  val w0'' = w0 mod v
                                                                  val { hi = z1, lo = z0 } = mulAdd (w1, b'', w0'')
                                                              in loop (acc + w1 * b' + w0', z1, z0)
                                                              end
                             in loop (0w0, u1, u0)
                             end
(* Compute ((u1 * base + u0) div v, (u1 * base + u0) mod v) *)
(* precondition: u1 < v *)
fun quotRem2' (u1, u0, v) = let val q = quot2' (u1, u0, v)
                            in (q, u0 - q * v)
                            end

(* Compute (u1 * base + u0) div v *)
(* precondition: u1 < base div 2 *)
fun quot2 (u1, u0, v) = let val v_hi = Word.>> (v, wordSize_2)
                            val q1 = u1 div v_hi
                            val r1 = u1 mod v_hi
                            val q = Word.<< (q1, wordSize_2) + Word.<< (r1, wordSize_2) div v_hi + u0 div v
                            fun loop q = if q = Word.notb 0w0 then
                                             q
                                         else
                                             let val { hi = v1, lo = v0 } = mul2 (q, v)
                                             in if u1 < v1 orelse (u1 = v1 andalso u0 < v0) then
                                                    loop (q - 0w1)
                                                else
                                                    let val { hi = v1', lo = v0' } = mul2 (q + 0w1, v)
                                                    in if v1' < u1 orelse (v1' = u1 andalso v0' <= u0) then
                                                           loop (q + 0w1)
                                                       else
                                                           q
                                                    end
                                             end
                        in loop q
                        end

(* Compute ((u1 * base + u0) div v, (u1 * base + u0) mod v) *)
(* precondition: u1 < base div 2 *)
fun quotRem2 (u1, u0, v) = let val q = quot2 (u1, u0, v)
                           in (q, u0 - q * v)
                           end

fun quotRemAbsSingle (words : word vector, v : word) : word vector * word
    = let val n = Vector.length words
          val quotient = Array.array (n, 0w0)
          fun loop (r, j) = if j < 0 then
                                r
                            else
                                let val (w, r') = quotRem2' (r, Vector.sub (words, j), v)
                                in Array.update (quotient, j, w)
                                 ; loop (r', j - 1)
                                end
          val r = loop (0w0, n - 1)
      in (normalize quotient, r)
      end

(* precondition: words' <> 0 *)
fun quotRemAbs (words, words') : word vector * word vector
    = let val n = Vector.length words'
          val m' = Vector.length words
          val m = m' - n
      in if m < 0 then
             (* words < words' *)
             (#[], words)
         else if n = 1 then
             let val (q, r) = quotRemAbsSingle (words, Vector.sub (words', 0))
             in (q, if r = 0w0 then #[] else #[r])
             end
         else
             let val offset = Word.fromInt (clzWord (Vector.sub (words', n - 1))) (* <= Word.wordSize - 1 *)
                 val words = LShiftAbs (words, offset)
                 val words = Array.tabulate (Vector.length words, fn i => Vector.sub (words, i))
                 val words' = LShiftAbs (words', offset)
                 val quotient = Array.array (m + 1, 0w0)
                 fun loop j = if j < 0 then
                                  ()
                              else
                                  let (* u1 < base div 2 *)
                                      val u1 = if Array.length words > j + n then
                                                   Array.sub (words, j + n)
                                               else
                                                   0w0
                                      val u0 = Array.sub (words, j + n - 1)
                                      val v = Vector.sub (words', n - 1)
                                      val (q', r') = quotRem2 (u1, u0, v)
                                      fun loop2 q' = let val w = mulAbsSingle (words', q') (* = mulAbs (#[q'], words') *)
                                                         val ws = ArraySlice.vector (ArraySlice.slice (words, j, NONE))
                                                     in case compareAbs (ws, w) of
                                                            LESS => loop2 (q' - 0w1)
                                                          | _ => let val d = subAbs (ws, w)
                                                                     fun l k = if k >= Array.length words then
                                                                                   ()
                                                                               else
                                                                                   ( Array.update (words, k, 0w0)
                                                                                   ; l (k + 1)
                                                                                   )
                                                                     val () = l (j + Vector.length d)
                                                                 in Array.copyVec { src = d, dst = words, di = j }
                                                                  ; q'
                                                                 end
                                                     end
                                      val q = loop2 q'
                                      val () = Array.update (quotient, j, q)
                                  in loop (j - 1)
                                  end
             in loop m
              ; (normalize quotient, #1 (RShiftAbs (normalize words, offset)))
             end
      end

fun div_ (_, ZERO) = raise Div
  | div_ (z as ZERO, _) = z
  | div_ (POSITIVE words, POSITIVE words') = let val (q, _) = quotRemAbs (words, words')
                                             in mkNonNegative q
                                             end
  | div_ (POSITIVE words, NEGATIVE words') = let val (q, r) = quotRemAbs (words, words')
                                             in if Vector.length r > 0 then
                                                    mkNonPositive (addAbs (q, #[0w1]))
                                                else
                                                    mkNonPositive q
                                             end
  | div_ (NEGATIVE words, POSITIVE words') = let val (q, r) = quotRemAbs (words, words')
                                             in if Vector.length r > 0 then
                                                    mkNonPositive (addAbs (q, #[0w1]))
                                                else
                                                    mkNonPositive q
                                             end
  | div_ (NEGATIVE words, NEGATIVE words') = let val (q, _) = quotRemAbs (words, words')
                                             in mkNonNegative q
                                             end

fun mod_ (_, ZERO) = raise Div
  | mod_ (z as ZERO, _) = z
  | mod_ (POSITIVE words, POSITIVE words') = let val (_, r) = quotRemAbs (words, words')
                                             in mkNonNegative r
                                             end
  | mod_ (POSITIVE words, NEGATIVE words') = let val (_, r) = quotRemAbs (words, words')
                                             in if Vector.length r > 0 then
                                                    mkNonPositive (subAbs (words', r))
                                                else
                                                    ZERO
                                             end
  | mod_ (NEGATIVE words, POSITIVE words') = let val (_, r) = quotRemAbs (words, words')
                                             in if Vector.length r > 0 then
                                                    mkNonNegative (subAbs (words', r))
                                                else
                                                    ZERO
                                             end
  | mod_ (NEGATIVE words, NEGATIVE words') = let val (_, r) = quotRemAbs (words, words')
                                             in mkNonPositive r
                                             end

fun divMod (_, ZERO) = raise Div
  | divMod (z as ZERO, _) = (z, z)
  | divMod (POSITIVE words, POSITIVE words') = let val (q, r) = quotRemAbs (words, words')
                                               in (mkNonNegative q, mkNonNegative r)
                                               end
  | divMod (POSITIVE words, NEGATIVE words') = let val (q, r) = quotRemAbs (words, words')
                                               in if Vector.length r > 0 then
                                                      (mkNonPositive (addAbs (q, #[0w1])), mkNonPositive (subAbs (words', r)))
                                                  else
                                                      (mkNonPositive q, ZERO)
                                               end
  | divMod (NEGATIVE words, POSITIVE words') = let val (q, r) = quotRemAbs (words, words')
                                               in if Vector.length r > 0 then
                                                      (mkNonPositive (addAbs (q, #[0w1])), mkNonNegative (subAbs (words', r)))
                                                  else
                                                      (mkNonPositive q, ZERO)
                                               end
  | divMod (NEGATIVE words, NEGATIVE words') = let val (q, r) = quotRemAbs (words, words')
                                               in (mkNonPositive q, mkNonNegative r)
                                               end

fun quot (_, ZERO) = raise Div
  | quot (z as ZERO, _) = z
  | quot (POSITIVE words, POSITIVE words') = let val (q, _) = quotRemAbs (words, words')
                                             in mkNonNegative q
                                             end
  | quot (POSITIVE words, NEGATIVE words') = let val (q, _) = quotRemAbs (words, words')
                                             in mkNonPositive q
                                             end
  | quot (NEGATIVE words, POSITIVE words') = let val (q, _) = quotRemAbs (words, words')
                                             in mkNonPositive q
                                             end
  | quot (NEGATIVE words, NEGATIVE words') = let val (q, _) = quotRemAbs (words, words')
                                             in mkNonNegative q
                                             end

fun rem (_, ZERO) = raise Div
  | rem (z as ZERO, _) = z
  | rem (POSITIVE words, POSITIVE words') = let val (_, r) = quotRemAbs (words, words')
                                            in mkNonNegative r
                                            end
  | rem (POSITIVE words, NEGATIVE words') = let val (_, r) = quotRemAbs (words, words')
                                            in mkNonNegative r
                                            end
  | rem (NEGATIVE words, POSITIVE words') = let val (_, r) = quotRemAbs (words, words')
                                            in mkNonPositive r
                                            end
  | rem (NEGATIVE words, NEGATIVE words') = let val (_, r) = quotRemAbs (words, words')
                                            in mkNonPositive r
                                            end

fun quotRem (_, ZERO) = raise Div
  | quotRem (z as ZERO, _) = (z, z)
  | quotRem (POSITIVE words, POSITIVE words') = let val (q, r) = quotRemAbs (words, words')
                                                in (mkNonNegative q, mkNonNegative r)
                                                end
  | quotRem (POSITIVE words, NEGATIVE words') = let val (q, r) = quotRemAbs (words, words')
                                                in (mkNonPositive q, mkNonNegative r)
                                                end
  | quotRem (NEGATIVE words, POSITIVE words') = let val (q, r) = quotRemAbs (words, words')
                                                in (mkNonPositive q, mkNonPositive r)
                                                end
  | quotRem (NEGATIVE words, NEGATIVE words') = let val (q, r) = quotRemAbs (words, words')
                                                in (mkNonNegative q, mkNonPositive r)
                                                end

fun min (x, y) = if LT (x, y) then
                     x
                 else
                     y

fun max (x, y) = if LT (x, y) then
                     y
                 else
                     x

fun sign ZERO = 0
  | sign (POSITIVE _) = 1
  | sign (NEGATIVE _) = ~1

fun sameSign (ZERO, ZERO) = true
  | sameSign (POSITIVE _, POSITIVE _) = true
  | sameSign (NEGATIVE _, NEGATIVE _) = true
  | sameSign (_, _) = false

fun notb x = negate (add (x, fromInt 1))

local
    fun stringFmt (f, x) : string = Lua.unsafeFromValue (Lua.call1 Lua.Lib.string.format #[Lua.fromString f, Lua.fromWord x])
    val ten_to_9 = 0w1000000000
in
fun toStringAbs words = if Vector.length words = 0 then
                            ""
                        else if Vector.length words = 1 andalso Vector.sub (words, 0) < ten_to_9 then
                            stringFmt ("%u", Vector.sub (words, 0))
                        else
                            let val (q, r) = quotRemAbsSingle (words, ten_to_9)
                            in toStringAbs q ^ stringFmt ("%09u", r)
                            end

fun toString ZERO = "0"
  | toString (POSITIVE words) = toStringAbs words
  | toString (NEGATIVE words) = "~" ^ toStringAbs words

(* precondition: Vector.length words > 0 *)
fun fmtHexAbs words = let val n = Vector.length words
                          val last = Vector.sub (words, n - 1)
                          val lasts = stringFmt ("%X", last)
                          val middlefmt = "%0" ^ Int.toString (Word.wordSize div 4) ^ "X"
                          fun loop (xs, i) = if i >= n - 1 then
                                                 lasts :: xs
                                             else
                                                 loop (stringFmt (middlefmt, Vector.sub (words, i)) :: xs, i + 1)
                      in String.concat (loop ([], 0))
                      end
end

fun fmt StringCvt.BIN x = raise Fail "StringCvt.BIN: not implemented yet"
  | fmt StringCvt.OCT x = raise Fail "StringCvt.OCT: not implemented yet"
  | fmt StringCvt.DEC x = toString x
  | fmt StringCvt.HEX x = (case x of
                               ZERO => "0"
                             | POSITIVE words => fmtHexAbs words
                             | NEGATIVE words => "~" ^ fmtHexAbs words
                          )

local
    open ScanNumUtils
    fun scanDigits (radix, isDigit, getc)
        = let fun go1 (x, strm) = case getc strm of
                                      SOME (c, strm') => if isDigit c then
                                                             go1 (add (mul (radix, x), fromInt (digitToInt c)), strm')
                                                         else
                                                             SOME (x, strm)
                                    | NONE => SOME (x, strm)
          in fn strm => case getc strm of
                            SOME (c, strm') => if isDigit c then
                                                   go1 (fromInt (digitToInt c), strm')
                                               else
                                                   NONE
                          | NONE => NONE
          end
    fun scanNegativeDigits (radix, isDigit, getc)
        = let fun go1 (x, strm) = case getc strm of
                                      SOME (c, strm') => if isDigit c then
                                                             go1 (sub (mul (radix, x), fromInt (digitToInt c)), strm')
                                                         else
                                                             SOME (x, strm)
                                    | NONE => SOME (x, strm)
          in fn strm => case getc strm of
                            SOME (c, strm') => if isDigit c then
                                                   go1 (fromInt (~ (digitToInt c)), strm')
                                               else
                                                   NONE
                          | NONE => NONE
          end
in
fun scan StringCvt.BIN getc strm = let val strm = skipInitialWhitespace (getc, strm)
                                       val (isNegative, strm) = scanSign (getc, strm)
                                   in if isNegative then
                                          scanNegativeDigits (fromInt 2, isBinDigit, getc) strm
                                      else
                                          scanDigits (fromInt 2, isBinDigit, getc) strm
                                   end
  | scan StringCvt.OCT getc strm = let val strm = skipInitialWhitespace (getc, strm)
                                       val (isNegative, strm) = scanSign (getc, strm)
                                   in if isNegative then
                                          scanNegativeDigits (fromInt 8, isOctDigit, getc) strm
                                      else
                                          scanDigits (fromInt 8, isOctDigit, getc) strm
                                   end
  | scan StringCvt.DEC getc strm = let val strm = skipInitialWhitespace (getc, strm)
                                       val (isNegative, strm) = scanSign (getc, strm)
                                   in if isNegative then
                                          scanNegativeDigits (fromInt 10, Char.isDigit, getc) strm
                                      else
                                          scanDigits (fromInt 10, Char.isDigit, getc) strm
                                   end
  | scan StringCvt.HEX getc strm = let val strm = skipInitialWhitespace (getc, strm)
                                       val (isNegative, strm) = scanSign (getc, strm)
                                       val strm = case getc strm of
                                                      SOME (#"0", strm') =>
                                                      (case getc strm' of
                                                           SOME (c, strm'') =>
                                                           if c = #"x" orelse c = #"X" then
                                                               case getc strm'' of
                                                                   SOME (c, _) => if Char.isHexDigit c then
                                                                                      strm''
                                                                                  else
                                                                                      strm
                                                                 | NONE => strm
                                                           else
                                                               strm
                                                         | NONE => strm
                                                      )
                                                    | _ => strm
                                   in if isNegative then
                                          scanNegativeDigits (fromInt 16, Char.isHexDigit, getc) strm
                                      else
                                          scanDigits (fromInt 16, Char.isHexDigit, getc) strm
                                   end
fun fromString s = StringCvt.scanString (scan StringCvt.DEC) s
end

local
    infix 4 ==
    val op == = Real.==
in
fun fromIntegralReal (x : real) = if x == 0.0 then
                                      ZERO
                                  else
                                      let val { man, exp } = Real.toManExp x
                                          val y = man * 0x1p53 (* 1 <= |y| <= 2^53-1 *)
                                          val e = exp - 53
                                          fun go x = if x == 0.0 then
                                                         #[]
                                                     else
                                                         let val { whole, frac } = Real.split (x / 0x1p31)
                                                             val i = Real.trunc (frac * 0x1p31) (* rounding do not occur *)
                                                         in orbAbs (LShiftAbs (go whole, 0w31), #[Word.fromInt i])
                                                         end
                                          fun adjust words = if e > 0 then
                                                                 LShiftAbs (words, Word.fromInt e)
                                                             else if e < 0 then
                                                                 #1 (RShiftAbs (words, Word.fromInt (~e)))
                                                             else
                                                                 words
                                      in if y < 0.0 then
                                             NEGATIVE (adjust (go (~y)))
                                         else
                                             POSITIVE (adjust (go y))
                                      end
val realBase : real = (* case Word.wordSize of
                          32 => 0x1p32
                        | 64 => 0x1p64
                        | _ => *) let fun pow (a, 0, acc) = acc
                                     | pow (a, 1, acc) = acc * a
                                     | pow (a, n, acc) = let val q = n div 2
                                                         in if n mod 2 = 0 then
                                                                pow (a * a, q, acc)
                                                            else
                                                                pow (a * a, q, acc * a)
                                                         end
                               in pow (2.0, Word.wordSize, 1.0)
                               end
fun wordToReal 0w0 = 0.0
  | wordToReal w = wordToReal (Word.>> (w, 0w31)) * 0x1p31 + Real.fromInt (Word.toInt (Word.andb (w, 0wx7fffffff)))
fun simpleNatToReal words = Vector.foldr (fn (w, acc) => acc * realBase + wordToReal w) 0.0 words
fun toRealAbs words = let val k = log2Abs words (* 2^k <= words < 2^(k+1) *)
                      in if k < Real.precision then
                             simpleNatToReal words
                         else if k >= 1024 then
                             Real.posInf (* overflow *)
                         else (* Real.precision <= k < 1024 *)
                             let val e = k - Real.precision + 1
                                 val q = #1 (RShiftAbs (words, Word.fromInt e))
                                 val r = andbAbs (words, subAbs (LShiftAbs (#[0w1], Word.fromInt e), #[0w1]))
                             in case compareAbs (r, LShiftAbs (#[0w1], Word.fromInt (e - 1))) of
                                    LESS => Real.fromManExp { man = simpleNatToReal q, exp = e }
                                  | EQUAL => if Word.andb (Vector.sub (q, 0), 0w1) = 0w0 then (* even *)
                                                 Real.fromManExp { man = simpleNatToReal q, exp = e }
                                             else
                                                 Real.fromManExp { man = simpleNatToReal (addAbs (q, #[0w1])), exp = e }
                                  | GREATER => Real.fromManExp { man = simpleNatToReal (addAbs (q, #[0w1])), exp = e }
                             end
                      end
fun toReal ZERO = 0.0
  | toReal (POSITIVE words) = toRealAbs words
  | toReal (NEGATIVE words) = ~ (toRealAbs words)
end

(* Overloaded identifiers *)
val op + = add
val op - = sub
val op * = mul
val op div = div_
val op mod = mod_
fun abs (NEGATIVE words) = POSITIVE words
  | abs x = x
val ~ = negate
val op < = LT
fun x <= y = not (y < x)
fun x > y = y < x
fun x >= y = not (x < y)
end
structure IntInf :> INT_INF where type int = IntInfImpl.int = IntInfImpl;
_overload "Int" [IntInf.int]
  { + = IntInf.+
  , - = IntInf.-
  , * = IntInf.*
  , div = IntInf.div
  , mod = IntInf.mod
  , abs = IntInf.abs
  , ~ = IntInf.~
  , < = IntInf.<
  , <= = IntInf.<=
  , > = IntInf.>
  , >= = IntInf.>=
  , fromInt = IntInf.fromInt
  };
