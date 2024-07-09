signature ORD_MAP =
sig
  structure Key: ORD_KEY
  type 'a map
  val empty: 'a map
  val isEmpty: 'a map -> bool
  val singleton: Key.ord_key * 'a -> 'a map
  val insert: 'a map * Key.ord_key * 'a -> 'a map
  val insert': (Key.ord_key * 'a) * 'a map -> 'a map
  val insertWith: ('a * 'a -> 'a) -> 'a map * Key.ord_key * 'a -> 'a map
  (* val insertWithi : (Key.ord_key * 'a * 'a -> 'a) -> 'a map * Key.ord_key * 'a -> 'a map *)
  val find: 'a map * Key.ord_key -> 'a option
  val lookup: 'a map * Key.ord_key -> 'a
  val inDomain: 'a map * Key.ord_key -> bool
  val remove: 'a map * Key.ord_key -> 'a map * 'a
  (* val findAndRemove : 'a map * Key.ord_key -> ('a map * 'a) option *)
  val first: 'a map -> 'a option
  val firsti: 'a map -> (Key.ord_key * 'a) option
  val numItems: 'a map -> int
  val listItems: 'a map -> 'a list
  val listItemsi: 'a map -> (Key.ord_key * 'a) list
  val listKeys: 'a map -> Key.ord_key list
  val collate: ('a * 'a -> order) -> 'a map * 'a map -> order
  val unionWith: ('a * 'a -> 'a) -> 'a map * 'a map -> 'a map
  val unionWithi: (Key.ord_key * 'a * 'a -> 'a) -> 'a map * 'a map -> 'a map
  val intersectWith: ('a * 'b -> 'c) -> 'a map * 'b map -> 'c map
  val intersectWithi: (Key.ord_key * 'a * 'b -> 'c) -> 'a map * 'b map -> 'c map
  val mergeWith: ('a option * 'b option -> 'c option)
                 -> 'a map * 'b map
                 -> 'c map
  val mergeWithi: (Key.ord_key * 'a option * 'b option -> 'c option)
                  -> 'a map * 'b map
                  -> 'c map
  val app: ('a -> unit) -> 'a map -> unit
  val appi: (Key.ord_key * 'a -> unit) -> 'a map -> unit
  val map: ('a -> 'b) -> 'a map -> 'b map
  val mapi: (Key.ord_key * 'a -> 'b) -> 'a map -> 'b map
  val foldl: ('a * 'b -> 'b) -> 'b -> 'a map -> 'b
  val foldli: (Key.ord_key * 'a * 'b -> 'b) -> 'b -> 'a map -> 'b
  val foldr: ('a * 'b -> 'b) -> 'b -> 'a map -> 'b
  val foldri: (Key.ord_key * 'a * 'b -> 'b) -> 'b -> 'a map -> 'b
  val filter: ('a -> bool) -> 'a map -> 'a map
  val filteri: (Key.ord_key * 'a -> bool) -> 'a map -> 'a map
  val mapPartial: ('a -> 'b option) -> 'a map -> 'b map
  val mapPartiali: (Key.ord_key * 'a -> 'b option) -> 'a map -> 'b map
  val exists: ('a -> bool) -> 'a map -> bool
  val existsi: (Key.ord_key * 'a -> bool) -> 'a map -> bool
  val all: ('a -> bool) -> 'a map -> bool
  val alli: (Key.ord_key * 'a -> bool) -> 'a map -> bool
end
