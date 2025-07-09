(* compatibility with older smlnj-lib *)
functor RedBlackMapFn(Key: ORD_KEY): ORD_MAP =
let structure M = RedBlackMapFn(Key)
in
  struct
    open M
    fun insertWith comb (map, key, value) =
      let
        val value =
          case find (map, key) of
            SOME x => comb (x, value)
          | NONE => value
      in
        insert (map, key, value)
      end
    fun findAndRemove (map, key) =
      if inDomain (map, key) then SOME (remove (map, key)) else NONE
    open M
  end
end;
