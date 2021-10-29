functor Foo (eqtype u
             eqtype v
             val f : u -> v
            ) = struct
fun g (x, y) = (x = y, f x = f y)
end
structure A = struct
type u = int
type v = int * string
fun f x = (x, Int.toString x)
end
structure F = Foo (A)
val (a, b) = F.g (37, 42)
val _ = print (Bool.toString a ^ "," ^ Bool.toString b ^ "\n")
val (a, b) = F.g (0, 0)
val _ = print (Bool.toString a ^ "," ^ Bool.toString b ^ "\n");
