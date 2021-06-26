signature S = sig
    datatype 'a t = T of 'a
end
structure T = struct
datatype 'a t = T of 'a * 'a
end : S;
