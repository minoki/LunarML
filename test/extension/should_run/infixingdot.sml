infix 3 .foo.
infixr 4 .bar.
fun a .foo. b = "foo(" ^ a ^ "," ^ b ^ ")"
fun a .bar. b = "bar(" ^ a ^ "," ^ b ^ ")";
print (("a" .foo. "b" .foo. "c" .foo. "d") ^ "\n");
print (("a" .foo. "b" .foo. "c" .bar. "d") ^ "\n");
print (("a" .foo. "b" .bar. "c" .foo. "d") ^ "\n");
print (("a" .foo. "b" .bar. "c" .bar. "d") ^ "\n");
print (("a" .bar. "b" .foo. "c" .foo. "d") ^ "\n");
print (("a" .bar. "b" .foo. "c" .bar. "d") ^ "\n");
print (("a" .bar. "b" .bar. "c" .foo. "d") ^ "\n");
print (("a" .bar. "b" .bar. "c" .bar. "d") ^ "\n");
structure S = struct
val foo = foo
val bar = bar
end;
print (("a" .S.foo. "b" .S.foo. "c" .S.foo. "d") ^ "\n");
print (("a" .S.foo. "b" .S.foo. "c" .S.bar. "d") ^ "\n");
print (("a" .S.foo. "b" .S.bar. "c" .S.foo. "d") ^ "\n");
print (("a" .S.foo. "b" .S.bar. "c" .S.bar. "d") ^ "\n");
print (("a" .S.bar. "b" .S.foo. "c" .S.foo. "d") ^ "\n");
print (("a" .S.bar. "b" .S.foo. "c" .S.bar. "d") ^ "\n");
print (("a" .S.bar. "b" .S.bar. "c" .S.foo. "d") ^ "\n");
print (("a" .S.bar. "b" .S.bar. "c" .S.bar. "d") ^ "\n");
