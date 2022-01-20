functor F (exception E) = struct
fun f () = (raise E; false) handle E => true
end;
exception E1;
structure S = F (exception E = E1);
print (Bool.toString (S.f ()) ^ "\n");
