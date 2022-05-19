fun amb (p : 'a list LunarML.DelimCont.prompt, xs : 'b list) : 'b
    = LunarML.DelimCont.withSubCont (p, fn cont : ('b,'a list) LunarML.DelimCont.subcont =>
                                           let fun loop [] = []
                                                 | loop (x :: xs) = LunarML.DelimCont.pushPrompt (p, fn () => LunarML.DelimCont.pushSubCont (cont, fn () => x)) @ loop xs
                                           in loop xs
                                           end
                                    )
fun run (f : 'a list LunarML.DelimCont.prompt -> 'a list) : 'a list
    = let val p = LunarML.DelimCont.newPrompt ()
      in LunarML.DelimCont.pushPrompt (p, fn () => f p)
      end;
val result = run (fn p =>
                     let val x = amb (p, [0,1,2])
                         val y = amb (p, [0,1,2])
                         val z = amb (p, [0,1,2])
                     in if x + y + z = 3 then
                            [(x,y,z)]
                        else
                            []
                     end
                 );
List.app (fn (x,y,z) => print ("(" ^ Int.toString x ^ "," ^ Int.toString y ^ "," ^ Int.toString z ^ ")\n")) result;
