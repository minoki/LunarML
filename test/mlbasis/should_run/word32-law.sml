infix $
fun f $ x = f x;

fun checkOrdering (x : Word32.word, y : Word32.word)
    = let val result = (x < y, x <= y, x > y, x >= y)
          val list = List.map (fn f => f (x, y)) [Word32.<, Word32.<=, Word32.>, Word32.>=]
      in if x = y then
             result = (false, true, false, true)
             andalso list = [false, true, false, true]
         else if x < y then
             result = (true, true, false, false)
             andalso list = [true, true, false, false]
         else
             result = (false, false, true, true)
             andalso list = [false, false, true, true]
      end;

fun checkDivMod (x : Word32.word, y : Word32.word)
    = if y = 0w0 then
          ((Word32.div (x, y); false) handle Div => true)
          andalso ((Word32.mod (x, y); false) handle Div => true)
      else
          let val q = x div y
              val r = x mod y
              val q' = Word32.div $ (x, y)
              val r' = Word32.mod $ (x, y)
          in q = q' andalso r = r' andalso r < y andalso x = q * y + r
          end;

fun checkSmall (x, y)
    = if x > 0w200 then
          true
      else if y > 0w200 then
          checkSmall (x + 0w1, 0w0)
      else if checkOrdering (x, y) andalso checkDivMod (x, y) then
          checkSmall (x, y + 0w1)
      else
          ( print ("Error with (x, y) = (0wx" ^ Word32.toString x ^ ", 0wx" ^ Word32.toString y ^ ")\n")
          ; false
          );
if checkSmall (0w0, 0w0) then
    print "Small test passed.\n"
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
                 true
             else
                 let val x = Word32.fromLarge (Word64.toLarge (gen ()))
                     val y = Word32.fromLarge (Word64.toLarge (gen ()))
                 in if checkOrdering (x, y) andalso checkDivMod (x, y) then
                        loop (n - 1)
                    else
                        ( print ("Error with (x, y) = (0wx" ^ Word32.toString x ^ ", 0wx" ^ Word32.toString y ^ ")\n")
                        ; false
                        )
                 end;
if loop 10000 then
    print "Random test passed.\n"
else
    ();
