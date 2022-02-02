structure Foo = struct
fun f ({ x, ... } : { x : 'a, y : int }) = x
end : sig val f : { x : 'a, y : int } -> 'a end;
print (Int.toString (Foo.f { x = 42, y = 37 }) ^ "\n");
print (Foo.f { x = "Good morning\n", y = 0 });
