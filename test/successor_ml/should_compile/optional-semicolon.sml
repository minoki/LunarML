val _ = (true; ) : bool;
val _ = (print "A"; print "B"; print "C"; "foo"; ) : string;
val _ = let val () = ()
        in print "D";
           print "E";
           print "F";
           42;
        end : int;
