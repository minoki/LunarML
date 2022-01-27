(* TODO: Make 'op =' bindable? *)
fun x <> y = _primCall "Bool.not" (x = y);
