structure D = LunarML.DelimCont;
val p = D.newPromptTag () : unit D.prompt_tag;
D.pushPrompt (p, fn () => raise Fail "Yay") handle Fail s => print (s ^ "\n");
(* D.shift (p, fn k => k ()); (* should be an error *) *)
datatype t = T of (unit -> int, t) D.subcont
           | U of int
val q = D.newPromptTag () : t D.prompt_tag;
fun new () = let val T k = D.pushPrompt (q, fn () => let val f = D.withSubCont (q, fn sk => T sk)
                                                     in U (3 * f ())
                                                     end handle Fail s => (print ("Caught: " ^ s ^ "\n"); U 42));
             in k
             end
val k = new ();
print (Int.toString (case D.pushSubCont (k, fn () => fn () => 5) of T _ => ~1 | U x => x) ^ "\n"); (* should print 15 *)
val k = if D.supportsMultishot then
            k
        else
            new ();
print (Int.toString (case D.pushSubCont (k, fn () => raise Fail "Wow") of T _ => ~1 | U x => x) ^ "\n"); (* should print 42 *)
