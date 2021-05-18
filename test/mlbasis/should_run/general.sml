val () : unit = () : General.unit;
val Bind = General.Bind : General.exn;
val Match = General.Match : General.exn;
val Chr = General.Chr : exn;
val Div = General.Div : General.exn;
val General.Domain = Domain : exn;
val _ = General.Fail : string -> General.exn;
val Overflow = General.Overflow : General.exn;
val Size = General.Size : General.exn;
val General.Span = Span : exn;
val Subscript = General.Subscript : General.exn;
(* val _ = exnName : exn -> string; *)
(* val _ = exnMessage : exn -> string; *)
val General.LESS = LESS : General.order;
val General.EQUAL = EQUAL : order;
(* val true = General.GREATER = GREATER; *)
val 42 = General.! (ref 42 : int ref);
val r = ref 42;
val () = General.:= (r, 37);
val ref 37 = r;
val _ = General.before : 'a * unit -> 'a;
val _ = General.ignore : 'a -> unit;
val "43" = (Int.toString o (fn x => x + 1)) 42;
val _ = General.o : ('b -> 'c) * ('a -> 'b) -> 'a -> 'c;
