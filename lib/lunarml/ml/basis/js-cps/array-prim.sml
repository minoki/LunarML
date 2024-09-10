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
val array = _Prim.Array.array
fun fromList xs = _primCall "Array.fromList" (xs)
end;
(* equality is defined in equal.sml *)
