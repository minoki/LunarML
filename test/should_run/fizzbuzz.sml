val counter = ref 1;
while !counter <= 100 do
                      let val i = !counter
                          val s = case (i mod 3 = 0, i mod 5 = 0) of
                                      (true, true) => "FizzBuzz"
                                    | (true, false) => "Fizz"
                                    | (false, true) => "Buzz"
                                    | _ => Int.toString i
                      in print (s ^ "\n");
                         counter := i + 1
                      end;
