signature S = sig
    type t
    type u
    sharing type t = u
end;
structure S : S = struct
datatype t = T
type u = t
end;
