infix ==>
fun true ==> (y : bool) = y
  | false ==> _ = true
fun testEquality (x : ''a, y : ''a, z : ''a) = let val xy = x = y
                                                   val nxy = y <> x
                                                   val yz = y = z
                                                   val nyz = z <> y
                                                   val zx = z = x
                                                   val nzx = x <> z
                                               in if xy = not nxy andalso yz = not nyz andalso zx = not nzx andalso ((xy andalso yz) ==> zx) andalso ((yz andalso zx) ==> xy) andalso ((zx andalso xy) ==> yz) then
                                                      print (Bool.toString xy ^ "," ^ Bool.toString yz ^ "," ^ Bool.toString zx ^ "\n")
                                                  else
                                                      print "Error\n"
                                               end;
testEquality (true, true, false);
testEquality (42, 42, 43);
testEquality (0, ~0, 1);
testEquality (vector [42], Vector.fromList (42 :: nil), vector []);
testEquality ([42, 43], 42 :: 43 :: nil, 42 :: ~44 :: nil);
let val r = ref (42.0 : real)
in testEquality (r, r, ref 42.0)
end;
let val a = Array.fromList [42.0]
in testEquality (a, a, Array.fromList [42.0])
end;
let val r = ref 0
in testEquality ({1 = 123, foo = (#"b", "bar"), baz = r}, {foo = {2 = "bar", 1 = #"b"}, baz = r, 1 = 120 + 3}, {1 = 123, foo = (#"b", "bar"), baz = ref 0})
end;
testEquality ((), {}, ());
testEquality (LESS, LESS, EQUAL);
testEquality (SOME 3, SOME 3, NONE);
testEquality (SOME 4, SOME 2, NONE);
testEquality (0w15, 0wxF, 0w0);
datatype unit' = Unit;
testEquality (Unit, Unit, Unit);
datatype 'a List = Nil
                 | Cons of 'a * 'a List;
testEquality (Cons (2, Cons (3, Nil)), Cons (2, Cons (1 + 2, Nil)), Cons (2, Cons (3, Cons (4, Nil))));
datatype void = Void of void;
val _ = fn () => let fun void () = Void (void ())
                 in testEquality (void (), void (), void ())
                 end;
datatype 'a foo = Foo of 'a bar
     and 'b bar = Bar of baz ref * 'b
     and baz = Baz of real;
let val r = ref (Baz 0.0)
in testEquality (Foo (Bar (r, "foo")), Foo (Bar (r, "foo")), Foo (Bar (ref (Baz 0.0), "foo")))
end;
fun ''a foo (x : ''a, y : ''a) = let datatype t = T of ''a
                                 in testEquality (T x, T x, T y)
                                 end;
let val a = Array.array (5, 1.0)
in foo (a, Array.array (5, 1.0))
end;
