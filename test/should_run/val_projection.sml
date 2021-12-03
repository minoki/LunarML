fun foo () : int * string = (42, "bar")
val (a, _) = foo ()
val _ = print (Int.toString a ^ "\n");
