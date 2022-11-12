fun product xs = let val p = LunarML.DelimCont.newPromptTag ()
                     fun loop [] = 1
                       | loop (0 :: _) = LunarML.DelimCont.abort (p, 0)
                       | loop (x :: xs) = x * loop xs
                 in LunarML.DelimCont.pushPrompt (p, fn () => loop xs)
                 end;
print (Int.toString (product [1, 2, 3]) ^ "\n");
print (Int.toString (product [0xffffffff, 0xffffffff, 0xffffffff, 0]) ^ "\n");
print ((Int.toString (product [0xffffffff, 0xffffffff, 0xffffffff]) handle Overflow => "Overflow") ^ "\n");
