fun checkEquality (f : unit -> ''a) = f () = f ();
checkEquality (fn () => (fn x => x) : unit -> unit);
