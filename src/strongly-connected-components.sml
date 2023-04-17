functor StronglyConnectedComponents (type t
                                     structure Map : sig
                                                   type 'a map
                                                   val map : ('a -> 'b) -> 'a map -> 'b map
                                                   val lookup : 'a map * t -> 'a
                                                   val foldli : (t * 'a * 'b -> 'b) -> 'b -> 'a map -> 'b
                                               end
                                     structure Set : sig
                                                   type set
                                                   val empty : set
                                                   val singleton : t -> set
                                                   val union : set * set -> set
                                                   val foldl : (t * 'a -> 'a) -> 'a -> set -> 'a
                                                   val isEmpty : set -> bool
                                               end
                                    ) : sig
            val components : ('node -> Set.set) * 'node Map.map -> Set.set list
        end = struct
fun components (destinations, graph)
    = let val map = Map.map (fn node => { refs = destinations node, invrefs = ref [], seen1 = ref false, seen2 = ref false }) graph
          fun dfs1 (from : t option, x : t) : t list
              = let val { refs, invrefs, seen1, ... } = Map.lookup (map, x)
                in case from of
                       SOME y => invrefs := y :: !invrefs
                     | NONE => ()
                 ; if !seen1 then
                       []
                   else
                       ( seen1 := true
                       ; Set.foldl (fn (y, acc) => acc @ dfs1 (SOME x, y)) [x] refs
                       )
                end
          val list : t list = Map.foldli (fn (x, _, acc) => acc @ dfs1 (NONE, x)) [] map
          fun dfs2 (x : t) : Set.set
              = let val { refs, invrefs = ref invrefs, seen2, ... } = Map.lookup (map, x)
                in if !seen2 then
                       Set.empty
                   else
                       ( seen2 := true
                       ; List.foldl (fn (y, acc) => Set.union (acc, dfs2 y)) (Set.singleton x) invrefs
                       )
                end
      in List.foldl (fn (x, acc) => let val set = dfs2 x
                                    in if Set.isEmpty set then
                                           acc
                                       else
                                           set :: acc
                                    end) [] list
      end
end;
