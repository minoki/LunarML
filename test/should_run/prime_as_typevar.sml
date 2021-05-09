val '0 foo = fn (x : '0) => x : '0;
val ' bar = fn (x : ') => x : ';
print (foo "Hello " ^ bar "world!\n");
print (Int.toString (foo 40 + bar 2) ^ "\n");
