val () = case true of
           | true => ()
           | false => ();
val () = () handle | Fail s => ();
val _ = fn | () => ();
fun | f () = ();
fun 'a | g (_ : 'a) = ();
fun ('a,'b) | h (_ : 'a, _ : 'b) = ();
datatype t = | T
     and u = | U
signature S = sig
datatype t = | T
     and u = | U
end;
