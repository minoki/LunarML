functor MapExtra(M: ORD_MAP): ORD_MAP_X
                              where type Key.ord_key = M.Key.ord_key
                              where type 'a map = 'a M.map =
struct
  open M
  (* Equivalent to unionWith #2 (a, b), but faster when b is small *)
  fun unionWithSecond (a, b) =
    foldli (fn (k, v, acc) => insert (acc, k, v)) a b
(* if isEmpty b then a else unionWith #2 (a, b) *)
end
