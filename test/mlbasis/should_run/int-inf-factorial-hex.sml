fun loop (i, acc) = if i >= 1000 then
                        ()
                    else
                        ( print (Int.toString i ^ "!=0x" ^ IntInf.fmt StringCvt.HEX acc ^ "\n")
                        ; loop (i + 1, acc * IntInf.fromInt (i + 1))
                        )
val () = loop (0, IntInf.fromInt 1);
