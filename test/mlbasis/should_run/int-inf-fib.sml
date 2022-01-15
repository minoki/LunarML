fun fib (n, x, y) = if n >= 1000 then
                        ()
                    else
                        ( print (IntInf.toString x ^ "\n")
                        ; fib (n + 1, y, x + y)
                        )
val _ = fib (0, 0, 1);
