structure Bool : sig
              datatype bool = datatype bool
              val not : bool -> bool
              val toString : bool -> string
          end = struct
datatype bool = datatype bool
fun not x = _primCall "Bool.not" (x)
fun toString true = "true"
  | toString false = "false"
(* scan, fromString *)
end;
_equality bool = fn (x, y) => _primCall "Bool.=" (x, y);
val not = Bool.not;
