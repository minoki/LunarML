fun isPrime n = n >= 2 andalso let fun go i = if i * i > n then
                                                  true
                                              else if n mod i = 0 then
                                                  false
                                              else
                                                  go (i + 1)
                               in go 2
                               end;
fun primesBelow n = let fun go i = if i > n then
                                       []
                                   else if isPrime i then
                                       i :: go (i + 1)
                                   else
                                       go (i + 1)
                    in go 2
                    end;
fun list_toString showElem = let fun go [] = ""
				   | go (x :: xs) = "," ^ showElem x ^ go xs
			     in fn [] => "[]"
			      | (x :: xs) => "[" ^ showElem x ^ go xs ^ "]"
			     end;
print (list_toString Int.toString (primesBelow 1000) ^ "\n");
