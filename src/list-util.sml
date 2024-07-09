(*
 * Copyright (c) 2023 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure ListUtil :>
sig
  val splitAt:
    'a list * int
    -> 'a list
       * 'a list (* splitAt (xs, n) = (List.take (xs, n), List.drop (xs, n)) *)
  val mapCont: ('a * ('b -> 'r) -> 'r) -> 'a list -> ('b list -> 'r) -> 'r
  val foldlCont: ('a * 'b * ('b -> 'r) -> 'r)
                 -> 'b
                 -> 'a list
                 -> ('b -> 'r)
                 -> 'r
  val foldlOption: ('a * 'b -> 'b option) -> 'b -> 'a list -> 'b option
end =
struct
  local
    fun splitAt' (xs, acc, n) =
      if n = 0 then
        (List.rev acc, xs)
      else
        case xs of
          [] => raise Subscript
        | x :: xss => splitAt' (xss, x :: acc, n - 1)
  in
    fun splitAt (xs, n) = splitAt' (xs, [], n)
  end

  fun mapCont _ [] cont = cont []
    | mapCont f (x :: xs) cont =
        f (x, fn y => mapCont f xs (fn ys => cont (y :: ys)))

  fun foldlCont _ init [] cont = cont init
    | foldlCont f init (x :: xs) cont =
        f (x, init, fn y => foldlCont f y xs cont)

  fun foldlOption _ init [] = SOME init
    | foldlOption f init (x :: xs) =
        case f (x, init) of
          SOME y => foldlOption f y xs
        | NONE => NONE
end; (* structure ListUtil *)
