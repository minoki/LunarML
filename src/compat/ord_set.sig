signature ORD_SET =
sig
  structure Key: ORD_KEY
  type item = Key.ord_key
  type set
  val empty: set
  val singleton: item -> set
  val fromList: item list -> set
  val toList: set -> item list
  val add: set * item -> set
  val add': item * set -> set
  val addList: set * item list -> set
  val subtract: set * item -> set
  val subtract': item * set -> set
  val subtractList: set * item list -> set
  val delete: set * item -> set
  val member: set * item -> bool
  val isEmpty: set -> bool
  val minItem: set -> item
  (* val maxItem : set -> item *)
  val equal: set * set -> bool
  val compare: set * set -> order
  val isSubset: set * set -> bool
  val disjoint: set * set -> bool
  val numItems: set -> int
  val listItems: set -> item list
  val union: set * set -> set
  val intersection: set * set -> set
  val difference: set * set -> set
  val map: (item -> item) -> set -> set
  (* val mapPartial : (item -> item option) -> set -> set *)
  val app: (item -> unit) -> set -> unit
  val foldl: (item * 'b -> 'b) -> 'b -> set -> 'b
  val foldr: (item * 'b -> 'b) -> 'b -> set -> 'b
  val partition: (item -> bool) -> set -> set * set
  val filter: (item -> bool) -> set -> set
  val exists: (item -> bool) -> set -> bool
  val all: (item -> bool) -> set -> bool
  val find: (item -> bool) -> set -> item option
end
