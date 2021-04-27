fun monoid_exp {unit,operation} (x,n) = let fun go (acc,x,0) = acc
                                              | go (acc,x,n) = let val x2 = operation (x, x)
                                                               in if n mod 2 = 0 then
                                                                      go (acc, x2, n div 2)
                                                                  else
                                                                      go (operation (acc, x), x2, n div 2)
                                                               end
                                        in go (unit,x,n)
                                        end;
print ("2^10 = " ^ Int.toString (monoid_exp { unit = 1, operation = Int.* } (2, 10)) ^ "\n");
print ("7^97 mod 97 = " ^ Int.toString (monoid_exp { unit = 1, operation = fn (x,y) => x * y mod 97 } (7, 96)) ^ "\n");
let
    datatype 'a fibPair = FibPair of 'a * 'a;
    infix @@
    fun (FibPair (a,b)) @@ (FibPair (a',b')) = FibPair (a * b' + (b - a) * a', a * a' + b * b');
    val fibZero = FibPair (0, 1);
    val fibOne = FibPair (1, 1);
    fun fibP 0 = fibZero
      | fibP n = let val x = fibP (n div 2)
                 in if n mod 2 = 0 then
                        x @@ x
                    else
                        fibOne @@ x @@ x
                 end;
    val fibPair_exp = monoid_exp { unit = fibZero, operation = op @@ };
    fun fib n = case fibPair_exp (fibOne, n) (* fibP n *) of
                    FibPair (a,_) => a;
in
    print ("fib(30) = " ^ Int.toString (fib 30) ^ "\n")
end;
