val rec loop = fn i => if i > 100 then
			   ()
		       else ( if i mod 15 = 0 then
				  print "FizzBuzz\n"
			      else if i mod 5 = 0 then
				  print "Buzz\n"
			      else if i mod 3 = 0 then
				  print "Fizz\n"
			      else
				  print (Int_toString i ^ "\n");
			      loop (i + 1)
			   );
loop 1;
