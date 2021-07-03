(*
 * This program is valid in Standard ML '97
 * but is invalid in Successor ML.
 *)
datatype t = T;
val rec T = fn x => x;
