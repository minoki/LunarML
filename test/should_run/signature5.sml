signature S = sig
    eqtype t
    datatype u = U
    datatype 'a v = V1 of 'a | V2 of 'a * 'a
    datatype w = datatype bool
end
structure T = struct
datatype t = T
datatype u = U
datatype 'a v = V1 of 'a | V2 of 'a * 'a
datatype w = datatype bool
end : S;
T.true = true;
