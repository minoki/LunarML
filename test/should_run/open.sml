structure Foo = struct
structure Bar = struct
val x = "Hello"
end
val x = 42
val y = " world!\n"
end
open Foo Foo.Bar;
print (x ^ y);
