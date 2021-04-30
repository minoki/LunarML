(*
 * This program is valid (and raises Bind) in Standard ML '97,
 * but is invalid in Successor ML.
 *)
let val SOME f = NONE
in f ();
   f "Hello world!"
end;

