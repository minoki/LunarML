fun foo { b, ... = x } = x = { a = 42 }
fun baz { d, ... = z } = bar z
and bar { c, ... = y } = foo y;
print (Bool.toString (baz { a = 42, b = (), c = (), d = () }) ^ "\n");
print (Bool.toString (baz { a = 0, b = 0w0, c = 3.14, d = "d" }) ^ "\n");
