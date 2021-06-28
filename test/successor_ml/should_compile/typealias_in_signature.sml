type t = string
signature S = sig
    type t = int
         and u = t (* int in SML '97, string in Successor ML *)
end
structure S : S = struct
type t = int
type u = string
end;
