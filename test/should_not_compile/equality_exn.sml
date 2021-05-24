fun checkEquality (f : unit -> ''a) = f () = f ();
checkEquality (fn () => Match : exn);
