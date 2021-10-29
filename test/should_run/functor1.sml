functor Double (S : sig
                    type t
                    val f : t -> t
                end) = struct
val ff = fn x => S.f (S.f x)
end
structure F = Double (struct
                       type t = int
                       fun f x = x + x
                       end)
structure G = Double (type t = int
                      fun f x = x * x
                     )
structure H = Double (type t = string
                      fun f x = x ^ x
                     )
val _ = print (Int.toString (F.ff 42) ^ "\n");
val _ = print (Int.toString (G.ff 4) ^ "\n");
val _ = print (H.ff "x" ^ "\n");
