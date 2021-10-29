functor F () = let datatype t = T
               in struct
                   val x = T
                   end
               end
structure F = F ();
print (Bool.toString (F.x = F.x) ^ "\n");
