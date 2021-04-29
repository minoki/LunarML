(* Example from The Definition: *)
val x = (let val id : 'a -> 'a = fn z => z in id id end; fn z => z : 'a);
(* desugars to: val 'a x = (let val id : 'a -> 'a = fn z => z in id id end; fn z => z : 'a); *)
