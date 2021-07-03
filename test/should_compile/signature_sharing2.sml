signature S = sig
    type ('a,'b) t
    type ('a,'b) u = ('a,'b) t
    sharing type t = u
end;
structure S : S = struct
type ('a,'b) t = int
     and ('a,'b) u = int
end;
