abstype foo = T of int with
fun MkFoo x = T x
fun getValue (T x) = x
end;
val x = MkFoo 42;
print (Int.toString (getValue x) ^ "\n");
type baz = string
abstype bar = T of baz
withtype baz = int with
fun MkBar x = T x
fun getBar (T x) = x
end;
print (Int.toString (getBar (MkBar 37)) ^ "\n");
