fun foo { a, b, ... = x } = x = { c = 42 };
print (Bool.toString (foo { a = (), b = (), c = 42 }) ^ "\n");
print (Bool.toString (foo { a = "a", b = "b", c = 37 }) ^ "\n");
print (Bool.toString (foo { c = 42, a = fn _ => 42, b = () }) ^ "\n");
