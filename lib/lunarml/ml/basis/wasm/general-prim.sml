structure General = struct
(* exception Bind *)
(* exception Match *)
exception Chr
exception Div
exception Domain
exception Fail of string
exception Overflow
exception Size
exception Span
exception Subscript
val exnName = _Prim.General.exnName
end;
