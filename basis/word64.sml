structure Word64 :> sig
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
open Word
val () = if Word.wordSize <> 64 then
             raise Fail "Word64 is not available"
         else
             ()
end
