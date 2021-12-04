structure Word8 :> sig
              eqtype word
              val wordSize : int
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
          end = struct
type word = Word.word
val FULL : word = 0wxFF
val wordSize = 8
val toInt = Word.toInt
fun toIntX x = if Word.andb (x, Word.<< (0w1, Word.fromInt (wordSize - 1))) = 0w0 then
                   Word.toInt x
               else
                   ~ (Word.toInt (FULL - x) + 1)
fun fromInt x = Word.andb (Word.fromInt x, FULL)
val andb = Word.andb
val orb = Word.orb
val xorb = Word.xorb
fun notb x = Word.andb (Word.notb x, FULL)
fun << (x, y) = Word.andb (Word.<< (x, y), FULL)
val >> = Word.>>
fun ~>> (x, y) = Word.andb (Word.~>> (x, y), FULL)
fun x + y = Word.andb (Word.+ (x, y), FULL)
fun x - y = Word.andb (Word.- (x, y), FULL)
fun x * y = Word.andb (Word.* (x, y), FULL)
fun x div y = Word.andb (Word.div (x, y), FULL)
fun x mod y = Word.andb (Word.div (x, y), FULL)
val compare = Word.compare
val op < = Word.<
val op <= = Word.<=
val op > = Word.>
val op >= = Word.>=
fun ~ x = Word.andb (Word.~ x, FULL)
val min = Word.min
val max = Word.max
val fmt = Word.fmt
val toString = Word.toString
end
