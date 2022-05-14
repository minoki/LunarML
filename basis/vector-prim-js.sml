structure Vector = struct
datatype vector = datatype vector
fun length vec = _primCall "Vector.length" (vec)
fun sub (vec, i) = if i < 0 orelse length vec <= i then
                       raise Subscript
                   else
                       Unsafe.Vector.sub (vec, i)
val fromList = _Prim.Vector.fromList
val tabulate = _Prim.Vector.tabulate
val concat = _Prim.Vector.concat
end
_equality ''a vector = fn (x, y) => let val n = Vector.length x
                                        fun go i = if i >= n then
                                                       true
                                                   else
                                                       Unsafe.Vector.sub (x, i) = Unsafe.Vector.sub (y, i) andalso go (i + 1)
                                    in n = Vector.length y andalso go 0
                                    end;
