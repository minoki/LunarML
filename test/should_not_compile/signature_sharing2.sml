signature S = sig
    type ('a,'b) t
    type ('a,'b) u = ('b,'a) t
    sharing type t = u
end;
