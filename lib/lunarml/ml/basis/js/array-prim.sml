structure Array = struct
datatype array = datatype array
fun length arr = _primCall "Array.length" (arr)
fun sub (arr, i) = if i < 0 orelse length arr <= i then
                       raise Subscript
                   else
                       Unsafe.Array.sub (arr, i)
fun update (arr, i, value) = if i < 0 orelse length arr <= i then
                                 raise Subscript
                             else
                                 Unsafe.Array.update (arr, i, value)
fun array (n, init) = _primCall "Array.array" (n, init)
val fromList = _Prim.Array.fromList
fun tabulate (n, f) = _primCall "call2" (_Prim.Array.tabulate, n, f)
end;
(* equality is defined in equal.sml *)
