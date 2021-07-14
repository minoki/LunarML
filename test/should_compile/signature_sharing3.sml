signature S = sig
    type t
    eqtype u
    sharing type t = u
end
structure S : S = struct
datatype t = T
type u = t
end;
