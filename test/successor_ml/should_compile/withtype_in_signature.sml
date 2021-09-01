type s = char
type t = int
signature S = sig
    datatype u = U of s * t
    withtype s = s * t * u
         and t = string * s * t * u
end
structure S : S = struct
datatype u = U of (char * int * u) * (string * char * int * u)
type s = char * int * u
type t = string * char * int * u
end
type s' = S.s
type t' = S.t;
