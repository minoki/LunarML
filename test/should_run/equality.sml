fun checkEquality (f : unit -> ''a) = if false then
                                          f () = f ()
                                      else
                                          true;
val true = checkEquality (fn () => true);
val true = checkEquality (fn () => 42);
val true = checkEquality (fn () => vector [42]);
val true = checkEquality (fn () => [42]);
val true = checkEquality (fn () => ref 42.0);
val true = checkEquality (fn () => Array.fromList [42.0]);
val true = checkEquality (fn () => { 1 = 123, foo = "bar", baz = ref 0 });
datatype unit' = Unit;
val true = checkEquality (fn () => Unit);
datatype 'a List = Nil
                 | Cons of 'a * 'a List;
val true = checkEquality (fn () => Cons (2, Cons (3, Nil)));
datatype void = Void of void;
val true = checkEquality (fn () => Void (raise Fail "void"));
datatype 'a foo = Foo of 'a bar
     and 'b bar = Bar of baz ref * 'b
     and baz = Baz of real;
val true = checkEquality (fn () => Foo (Bar (ref (Baz 0.0), "")));
fun ''a foo (x : ''a) = let datatype t = T of ''a
                        in checkEquality (fn () => T x)
                        end;
val true = foo (Array.array (5, 1.0));
