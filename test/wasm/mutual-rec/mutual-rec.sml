structure export : sig
  val even : int -> int  (* returns 1 if even, 0 if odd *)
  val odd  : int -> int  (* returns 1 if odd,  0 if even *)
  val countDown : int -> int (* three-way mutual recursion, returns 0 *)
end = struct

(* Two-function mutual recursion: even/odd *)
fun even 0 = 1
  | even n = odd (n - 1)
and odd 0 = 0
  | odd n = even (n - 1)

(* Three-function mutual recursion *)
fun f3a 0 = 0
  | f3a n = f3b (n - 1)
and f3b 0 = 0
  | f3b n = f3c (n - 1)
and f3c 0 = 0
  | f3c n = f3a (n - 1)

fun countDown n = f3a n

end
