structure Foo = struct
exception Bar
end : sig val Bar : exn end;
() handle Foo.Bar => ();
