structure export : sig
  val factorial : int -> int
  val fib : int -> int
end = struct
fun factorial 0 = 1
  | factorial n = n * factorial (n - 1)
fun fib 0 = 0
  | fib 1 = 1
  | fib n = fib (n - 1) + fib (n - 2)
end;
