structure VectorSlice :> sig
              type 'a slice
              val length : 'a slice -> int
              val sub : 'a slice * int -> 'a
              val full : 'a Vector.vector -> 'a slice
              val slice : 'a Vector.vector * int * int option -> 'a slice
              val subslice : 'a slice * int * int option -> 'a slice
              val vector : 'a slice -> 'a Vector.vector
              val exists : ('a -> bool) -> 'a slice -> bool
          end = struct
(* invariant: 0 <= start <= start + length <= Vector.length base *)
type 'a slice = { base : 'a vector
                , start : int
                , length : int
                }
val length : 'a slice -> int = #length
fun sub ({ base, start, length }, i) = if 0 <= i andalso i < length then
                                           Unsafe.Vector.sub (base, start + i)
                                       else
                                           raise Subscript
fun full a = { base = a, start = 0, length = Vector.length a }
fun slice (a, i, NONE) = if 0 <= i andalso i <= Vector.length a then
                             { base = a, start = i, length = Vector.length a - i }
                         else
                             raise Subscript
  | slice (a, i, SOME n) = if 0 <= i andalso 0 <= n andalso i + n <= Vector.length a then
                               { base = a, start = i, length = n }
                           else
                               raise Subscript
fun subslice ({ base, start, length }, i, NONE) = if 0 <= i andalso i <= length then
                                                      { base = base, start = start + i, length = length - i }
                                                  else
                                                      raise Subscript
  | subslice ({ base, start, length }, i, SOME n) = if 0 <= i andalso 0 <= n andalso i + n <= length then
                                                        { base = base, start = start + i, length = n }
                                                    else
                                                        raise Subscript
fun vector { base, start, length } = Vector.tabulate (length, fn i => Unsafe.Vector.sub (base, start + i))
fun exists f { base, start, length } = let fun loop i = if i >= length then
                                                            false
                                                        else
                                                            if f (Unsafe.Vector.sub (base, start + i)) then
                                                                true
                                                            else
                                                                loop (i + 1)
                                       in loop 0
                                       end
end
