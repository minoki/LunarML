signature S = sig
    type t
    val v : t
    exception E of t and F of string
end
structure Foo : S = struct
type t = int
val v = 123
exception E of int
exception F = Fail
end;
(raise Foo.E Foo.v) handle Foo.E _ => print "OK\n";
(raise Fail "foo\n") handle Foo.F message => print message;
