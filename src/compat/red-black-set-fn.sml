(* compatibility with older smlnj-lib *)
functor RedBlackSetFn(Key: ORD_KEY): ORD_SET =
let structure S = RedBlackSetFn(Key)
in
  struct
    open S
    (* fun toList s = foldr (op ::) [] s *)
    val toList = listItems
    fun disjoint (x, y) =
      isEmpty (intersection (x, y))
    fun minItem set =
      let
        val p =
          foldl
            (fn (x, NONE) => SOME x
              | (x, y' as SOME y) =>
               if Key.compare (x, y) = LESS then SOME x else y') NONE set
      in
        case p of
          NONE => raise Empty
        | SOME x => x
      end
    open S
  end
end;
