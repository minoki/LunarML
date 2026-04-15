structure export : sig
  (* Test mutual recursion with outer free-variable capture.
   * makeEvenCounter n: creates a counter starting at n, counts down by even/odd steps.
   * If n is even, returns n; if n is odd, returns 0. (Same as even/odd but using a captured value.)
   *)
  val evenFromN : int -> int  (* returns n if n is even (uses mutual rec internally) *)
  val tripleCountDown : int -> int  (* 3-way mutual rec with capture *)
end = struct

(* even/odd that captures an outer variable:
   We implement evenFromN by using a local mutual recursion with n captured.
   evenFromN n: if n is even, returns n; if n is odd, returns 0. *)
fun evenFromN n =
  let
    fun go_even 0 = n   (* base: n is even, return n *)
      | go_even k = go_odd (k - 1)
    and go_odd 0 = 0    (* base: n is odd, return 0 *)
      | go_odd k = go_even (k - 1)
  in
    go_even n
  end

(* 3-way mutual recursion that captures an outer 'offset' variable *)
fun tripleCountDown n =
  let
    fun fa 0 = n        (* capture n as return value when done *)
      | fa k = fb (k - 1)
    and fb 0 = n
      | fb k = fc (k - 1)
    and fc 0 = n
      | fc k = fa (k - 1)
  in
    fa n - n  (* always returns 0 when n mod 3 = 0, or wraps through all *)
  end

end
