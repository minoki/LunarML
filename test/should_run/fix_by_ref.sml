val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
    = fn f => let val t = ref NONE : (('a -> 'b) option) ref
                  val () = t := SOME (fn x => f (valOf (!t)) x)
              in valOf (!t)
              end;

val fact = fix (fn fact => fn n => if n = 0 then 1 else n * fact (n - 1));
print (Int.toString (fact 5) ^ "\n");
