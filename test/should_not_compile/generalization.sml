(*
 * This program is valid (and raises Bind) in Standard ML '97,
 * but is invalid in Successor ML.
 *)
datatype 'a option' = NONE' | SOME' of 'a;
let val SOME' f = NONE'
in f ();
   f "Hello world!"
end;

