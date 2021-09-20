abstype foo = T of int with
fun MkFoo x = T x
fun getValue (T x) = x
end;
case MkFoo 42 of T y => y;
