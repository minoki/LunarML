fun checkEquality (f : unit -> ''a) = f () = f ();
checkEquality (fn () => 0.0 : real);
