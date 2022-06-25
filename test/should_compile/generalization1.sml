fun f () = let val g = fn () => let val { a, ... } = f ()
                                in a
                                end
           in { a = "a", g = g }
           end;
