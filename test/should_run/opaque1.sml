structure Foo :> sig
              type t
              val x : t
              val add : t * t -> t
              val toString : t -> string
          end = struct
type t = int
val x = 42
val add = Int.+
val toString = Int.toString
end;
print (Foo.toString Foo.x ^ "\n");
print (Foo.toString (Foo.add (Foo.x, Foo.x)) ^ "\n");
