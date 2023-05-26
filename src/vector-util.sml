structure VectorUtil :> sig
              val foldlOption : ('a * 'b -> 'b option) -> 'b -> 'a vector -> 'b option
          end = struct
fun foldlOption (f : 'a * 'b -> 'b option) (init : 'b) (v : 'a vector) : 'b option
    = let fun loop (i, acc) = if i < Vector.length v then
                                  case f (Vector.sub (v, i), acc) of
                                      SOME acc => loop (i + 1, acc)
                                    | NONE => NONE
                              else
                                  SOME acc
      in loop (0, init)
      end
end; (* structure VectorUtil *)
