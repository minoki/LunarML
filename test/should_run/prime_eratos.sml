fun sieve n : bool array
    = let val a = Array.array (n + 1, true)
          fun fillLoop (i, step) = if i > n then
                                       ()
                                   else
                                       ( Array.update (a, i, false)
                                       ; fillLoop (i + step, step)
                                       )
          fun loop i = if i * i > n then
                           ()
                       else
                           ( fillLoop (i * i, i)
                           ; loop (i + 1)
                           )
      in Array.update (a, 0, false)
       ; Array.update (a, 1, false)
       ; loop 2
       ; a
      end;
fun list_toString showElem = let fun go [] = ""
				   | go (x :: xs) = "," ^ showElem x ^ go xs
			     in fn [] => "[]"
			      | (x :: xs) => "[" ^ showElem x ^ go xs ^ "]"
			     end;
fun primesBelow n = let val a = sieve n
                        fun go i = if i > n then
                                       []
                                   else
                                       if Array.sub (a, i) then
                                           i :: go (i + 1)
                                       else
                                           go (i + 1)
                    in go 0
                    end;
print (list_toString Int.toString (primesBelow 1000) ^ "\n");
