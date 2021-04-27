fun fib n = fibLoop 0 1 n
and fibLoop a b 0 = a
  | fibLoop a b n = fibLoop b (a + b) (n - 1);
val counter = ref 0;
fun readRef (ref x) = x;
while readRef counter < 40 do
                           let val i = !counter
                           in print ("fib(" ^ Int.toString i ^ ")=" ^ Int.toString (fib i) ^ "\n");
                              counter := i + 1
                           end;
