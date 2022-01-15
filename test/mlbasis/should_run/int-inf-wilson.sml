fun loop (i : int, acc : IntInf.int)
    = if i >= 1000 then
          ()
      else
          ( case (acc + 1) mod IntInf.fromInt (i + 1) of
                0 => print (Int.toString (i + 1) ^ " is prime.\n")
              | _ => ()
          ; loop (i + 1, acc * IntInf.fromInt (i + 1))
          )
val () = loop (1, 1);
