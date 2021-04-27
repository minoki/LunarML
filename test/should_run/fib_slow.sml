val rec fib = fn n => if n = 0 orelse n = 1 then
			  1
		      else
			  fib (n - 1) + fib (n - 2);
val rec loop = fn i => if i >= 10 then
			   ()
		       else
			   ( print ("fib(" ^ Int.toString i ^ ")=" ^ Int.toString (fib i) ^ "\n");
			     loop (i + 1)
			   );
loop 0;
