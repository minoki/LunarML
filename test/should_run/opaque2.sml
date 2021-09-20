structure Foo :> sig
              eqtype t
              val x : t
              val y : t
              val add : t * t -> t
              val toString : t -> string
          end = struct
type t = int
val x = 42
val y = 37
val add = Int.+
val toString = Int.toString
end;
print (Foo.toString Foo.x ^ "\n");
print (Bool.toString (Foo.x = Foo.x) ^ "\n");
print (Bool.toString (Foo.x = Foo.y) ^ "\n");
