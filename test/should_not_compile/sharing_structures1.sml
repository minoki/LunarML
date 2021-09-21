signature S = sig
    type 'a t
    eqtype u
    type v = int (* invalid *)
end
signature T = sig
    eqtype 'a t
    type u
    type v
end
signature V = sig
    type u
    eqtype v
end
signature V = sig
    structure A : S
    structure B : T
    structure C : V
    sharing A = B = C
end;
structure V : V = struct
structure A = struct
type 'a t = int
type u = string
type v = int
end
structure B = struct
type 'a t = int
type u = string
datatype v = V
end
structure C = struct
type u = string
type v = B.v
end
end;
