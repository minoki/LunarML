let val r = ref NONE
    fun f (x : 'a) : 'a option
        = let datatype t = T of 'a
              val y = case !r of
                          NONE => NONE
                        | SOME (T v) => SOME v
          in r := SOME (T x)
           ; y
          end
in f ()
 ; case f "" of
       NONE => print "NONE"
     | SOME v => print v
end;
