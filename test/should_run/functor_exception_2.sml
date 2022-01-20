functor F () = struct
exception E
fun f () = raise E
end;
structure S = F ();
structure T = F ();
T.f () handle S.E => print "Bad\n"
            | T.E => print "Good\n"
            | _ => print "Bad\n";
S.f () handle T.E => print "Bad\n"
            | S.E => print "Good\n"
            | _ => print "Bad\n";
