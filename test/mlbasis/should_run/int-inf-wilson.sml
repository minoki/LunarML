fun loop (i, acc) = if i >= 1000 then
                        ()
                    else
                        ( if (acc + IntInf.fromInt 1) mod IntInf.fromInt (i + 1) = IntInf.fromInt 0 then
                              print (Int.toString (i + 1) ^ " is prime.\n")
                          else
                              ()
                        ; loop (i + 1, acc * IntInf.fromInt (i + 1))
                        )
val () = loop (1, IntInf.fromInt 1);
