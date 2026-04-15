structure export : sig
  val factorial : int -> int
  val fib : int -> int
  val even : int -> bool
  val odd : int -> bool
end = struct
fun factorial 0 = 1
  | factorial n = n * factorial (n - 1)
fun fib 0 = 0
  | fib 1 = 1
  | fib n = fib (n - 1) + fib (n - 2)
fun even 0 = true
  | even n = odd (n - 1)
and odd 0 = false
  | odd n = even (n - 1)
end;
