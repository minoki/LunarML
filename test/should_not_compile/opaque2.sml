signature S = sig
    type t
    val x : t
    val add : t * t -> t
    val toString : t -> string
end
structure Foo :> S = struct
type t = int
val x = 42
val add = Int.+
val toString = Int.toString
end;
structure Bar :> S = struct
type t = string
val x = "x"
val add = String.^
val toString = fn x => x
end;
Foo.toString Bar.x;
