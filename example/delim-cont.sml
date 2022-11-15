structure DelimCont : sig
              type 'a prompt_tag
              type ('a,'b) subcont
              val newPromptTag : unit -> 'a prompt_tag
              val pushPrompt : 'a prompt_tag * (unit -> 'a) -> 'a
              val withSubCont : 'b prompt_tag * (('a,'b) subcont -> 'b) -> 'a
              val pushSubCont : ('a,'b) subcont * (unit -> 'a) -> 'b
              val abort : 'b prompt_tag * 'b -> 'a
              val topLevel : unit prompt_tag
          end = LunarML.DelimCont

fun product xs = let val p = DelimCont.newPromptTag ()
                     fun loop [] = 1
                       | loop (0 :: _) = DelimCont.abort (p, 0)
                       | loop (x :: xs) = x * loop xs
                 in DelimCont.pushPrompt (p, fn () => loop xs)
                 end;
print (Int.toString (product [1, 2, 3]) ^ "\n");
print (Int.toString (product [0xffffffff, 0xffffffff, 0xffffffff, 0]) ^ "\n");
print ((Int.toString (product [0xffffffff, 0xffffffff, 0xffffffff]) handle Overflow => "Overflow") ^ "\n");

val toplevel : unit DelimCont.prompt_tag = DelimCont.newPromptTag ();
val setTimeout = JavaScript.global "setTimeout";
fun sleep delay_ms = DelimCont.withSubCont
                         (toplevel, fn cont : (unit, unit) DelimCont.subcont =>
                                       let val callback = JavaScript.function
                                                              (fn arguments =>
                                                                  ( DelimCont.pushPrompt (DelimCont.topLevel, fn () => DelimCont.pushPrompt (toplevel, fn () => DelimCont.pushSubCont (cont, fn () => ()))) (* TextIO.print needs DelimCont.topLevel pushed *)
                                                                  ; JavaScript.undefined
                                                                  )
                                                              )
                                       in JavaScript.call setTimeout #[callback, JavaScript.fromReal delay_ms]
                                        ; ()
                                       end
                         );
DelimCont.pushPrompt (toplevel, fn () =>
                                   ( print "LunarML\n"
                                   ; sleep 1000.0
                                   ; print "delimited\n"
                                   ; sleep 2000.0
                                   ; print "continuations!\n"
                                   ; raise Fail "Yay!\n"
                                   ) handle Fail s => (sleep 1000.0; print s)
                     );
print "supports\n";
