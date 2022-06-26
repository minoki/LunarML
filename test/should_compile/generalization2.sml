datatype 'a t = T
fun f () = T
and g () = case f () of T => ()
and h () = f ();
f () : int t;
f () : string t;
h () : int t;
h () : string t;
fun f () : 'a t = T
and g () : unit = case f () of T => ()
and h () : 'a t = f ();
f () : int t;
f () : string t;
h () : int t;
h () : string t;
