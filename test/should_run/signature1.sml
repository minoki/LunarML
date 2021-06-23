signature S = sig
    type t
    type u
    val x : t
    val y : t
    val z : u
    val f : t * u -> int
end
structure T = struct
datatype t = T of int
type u = t
val x = T 0
val y = T 1
val z = T 2
fun f (T a, T b) = a + b
end : S;
print (Int.toString (T.f (T.x, T.z)) ^ "\n");
