val xs = [Lua.NIL, Lua.fromString "foo", Lua.NIL, Lua.fromInt 123];
val v = vector (Lua.NIL :: xs @ [ Lua.fromString "bar" ]);
print (Int.toString (Vector.length v) ^ "\n");
print (Bool.toString (Lua.isNil (Vector.sub (v, 0))) ^ "\n");
print (Bool.toString (Lua.isNil (Vector.sub (v, 1))) ^ "\n");
print (Bool.toString (Lua.== (Vector.sub (v, 2), Lua.fromString "foo")) ^ "\n");
print (Bool.toString (Lua.isNil (Vector.sub (v, 3))) ^ "\n");
print (Bool.toString (Lua.== (Vector.sub (v, 4), Lua.fromInt 123)) ^ "\n");
print (Bool.toString (Lua.== (Vector.sub (v, 5), Lua.fromString "bar")) ^ "\n");
