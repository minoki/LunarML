functor Foo () = struct
datatype t = T
end : sig
                     datatype t = T
                 end
structure F = Foo ()
structure G = Foo ();
val _ = F.T = G.T;
