let val r = ref NONE
    fun f (x : 'a) : 'a option
        = let val y = !r
          in r := SOME x
           ; y
          end
in f {x=123}
 ; case f "" of
       NONE => print "NONE"
     | SOME v => print v
end;
