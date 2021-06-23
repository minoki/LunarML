signature S = sig
    type t
    val x : t
end
signature T = sig
    structure S1 : S
    structure S2 : S
end
structure U = struct
structure S1 = struct
type t = int
val x = 123
end
structure S2 = struct
type t = string
val x = "foo"
end
end : T;
