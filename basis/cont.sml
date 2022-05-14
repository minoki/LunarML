structure Cont : sig
              type 'a cont
              val callcc : ('a cont -> 'a) -> 'a
              val throw : 'a cont -> 'a -> 'b
          end = struct
type 'a cont = 'a _Prim.Cont.cont
fun callcc (f : 'a cont -> 'a) : 'a = _primCall "Cont.callcc" (f)
fun throw (k : 'a cont) : 'a -> 'b = _primCall "Cont.throw" (k)
end;
