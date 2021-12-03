fun monoid_exp {unit,operation} (x,n) = let fun go (acc,x,0) = acc
                                              | go (acc,x,n) = let val x2 = operation (x, x)
                                                               in if n mod 2 = 0 then
                                                                      go (acc, x2, n div 2)
                                                                  else
                                                                      go (operation (acc, x), x2, n div 2)
                                                               end
                                        in go (unit,x,n)
                                        end;
datatype 'a fibPair = FibPair of 'a * 'a;
infix @@
fun (FibPair (a : IntInf.int,b)) @@ (FibPair (a',b')) = FibPair (a * b' + (b - a) * a', a * a' + b * b');
val fibZero = FibPair (IntInf.fromInt 0, IntInf.fromInt 1);
val fibOne = FibPair (IntInf.fromInt 1, IntInf.fromInt 1);
fun fibP 0 = fibZero
  | fibP n = let val x = fibP (n div 2)
             in if n mod 2 = 0 then
                    x @@ x
                else
                    fibOne @@ x @@ x
             end;
val fibPair_exp = monoid_exp { unit = fibZero, operation = op @@ };
fun fib n = case fibPair_exp (fibOne, n) of
                FibPair (a,_) => a;
let fun loop (n, x, y) = if n >= 1000 then
                             ()
                         else
                             let val z = fib n
                                 val () = if x = z then
                                              print "OK\n"
                                          else
                                              print ("Mismatch: " ^ IntInf.fmt StringCvt.HEX x ^ " vs " ^ IntInf.fmt StringCvt.HEX z ^ "\n")
                             in loop (n + 1, y, x + y)
                             end
in loop (0, IntInf.fromInt 0, IntInf.fromInt 1)
end;
