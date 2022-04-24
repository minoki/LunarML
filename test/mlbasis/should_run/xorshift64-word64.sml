if Word64.wordSize <> 64 then
    raise Fail "64-bit word is required"
else
    ();
fun xorshift64 (seed : Word64.word)
    = let val state = ref seed
      in fn () => let val x = !state
                      open Word64
                      infix << >> xorb
                      val x = x xorb (x << 0w13)
                      val x = x xorb (x >> 0w7)
                      val x = x xorb (x << 0w17)
                  in state := x
                   ; x
                  end
      end;
val gen = xorshift64 0wxBADCAFE;
fun loop n = if n <= 0 then
                 ()
             else
                 ( print ("0x" ^ Word64.toString (gen ()) ^ "\n")
                 ; loop (n - 1)
                 );
loop 100;
