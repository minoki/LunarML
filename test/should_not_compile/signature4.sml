signature S = sig
    datatype t = T
end
structure T = struct
datatype t = T | X
end : S;
