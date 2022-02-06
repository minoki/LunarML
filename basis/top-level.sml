val op <> : ''a * ''a -> bool = op <>;
open General (* unit, exn, Bind, Match, Chr, Div, Domain, Fail, Overflow, Size, Span, Subscript, exnName, order (LESS, EQUAL, GREATER), !, :=, before, ignore, o *)

(* type unit *)
type int = Int.int
type word = Word.word
type real = Real.real
type char = Char.char
type string = String.string
type substring = Substring.substring
(* type exn *)
datatype array = datatype Array.array
datatype vector = datatype Vector.vector
datatype ref = datatype ref (* primitive *)
datatype bool = datatype bool (* primitive *)
datatype option = datatype Option.option
(* datatype order *)
datatype list = datatype list;

(* exception Bind, Chr, Div, Domain, Fail, Match, Overflow, Size, Span, Subscript *)
exception Empty = List.Empty
exception Option = Option.Option;

(* val ! *)
(* val := *)
val op @ : ('a list * 'a list) -> 'a list = List.@
val op ^ : string * string -> string = String.^
val app : ('a -> unit) -> 'a list -> unit = List.app
(* val before *)
(* val ceil : not implemented yet *)
val chr : int -> char = Char.chr
val concat : string list -> string = String.concat
(* val exnMessage : not implemented yet *)
(* val exnName *)
val explode : string -> char list = String.explode
(* val floor : not implemented yet *)
val foldl : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b = List.foldl
val foldr : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b = List.foldr
val getOpt : 'a option * 'a -> 'a = Option.getOpt
val hd : 'a list -> 'a = List.hd
(* val ignore *)
val implode : char list -> string = String.implode
val isSome : 'a option -> bool = Option.isSome
val length : 'a list -> int = List.length
val map : ('a -> 'b) -> 'a list -> 'b list = List.map
val not : bool -> bool = Bool.not
val null : 'a list -> bool = List.null
(* val o *)
val ord : char -> int = Char.ord
val print : string -> unit = TextIO.print
(* val real = Real.fromInt : not implemented yet *)
(* val ref *)
val rev : 'a list -> 'a list = List.rev
(* val round : not implemented yet *)
val size : string -> int = String.size
val str : char -> string = String.str
val substring : string * int * int -> string = String.substring
val tl : 'a list -> 'a list = List.tl
(* val trunc : not implemented yet *)
(* val use : not supported *)
val valOf : 'a option -> 'a = Option.valOf
val vector : 'a list -> 'a vector = Vector.fromList;
