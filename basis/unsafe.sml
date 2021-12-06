structure Unsafe : sig
              structure Vector : sig
                            val sub : 'a vector * int -> 'a
                        end
              structure Array : sig
                            val sub : 'a array * int -> 'a
                            val update : 'a array * int * 'a -> {}
                        end
              val cast : 'a -> 'b
          end = struct
structure Vector = struct
fun sub (vec, i) = _primCall "Unsafe.Vector.sub" (vec, i)
end
structure Array = struct
fun sub (arr, i) = _primCall "Unsafe.Array.sub" (arr, i)
fun update (arr, i, v) = _primCall "Unsafe.Array.update" (arr, i, v)
end
fun cast x = _primCall "Unsafe.cast" (x)
end
