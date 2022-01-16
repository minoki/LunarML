(* From HaMLet S' manual *)
type 'a t = { a : 'a, b : bool }
type 'a u = { c : char, d : 'a list, ... : 'a t }

fun foo ({ a, b, c, d } : 'a u) = ()
val () = foo { a = 42, b = true, c = #"c", d = [2,3,5] }
