datatype 'a Rec = MkRec of ('a Rec -> 'a)

val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
    = fn (f : ('a -> 'b) -> 'a -> 'b) =>
         (fn MkRec x => f (fn (y : 'a) => x (MkRec x) y))
             (MkRec (fn MkRec x => f (fn (y : 'a) => x (MkRec x) y)));

val fact = fix (fn fact => fn n => if n = 0 then 1 else n * fact (n - 1));
print (Int.toString (fact 5) ^ "\n");
