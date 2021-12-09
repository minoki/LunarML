structure ArraySlice :> sig
              type 'a slice
              val length : 'a slice -> int
              val sub : 'a slice * int -> 'a
              val update : 'a slice * int * 'a -> unit
              val full : 'a Array.array -> 'a slice
              val slice : 'a Array.array * int * int option -> 'a slice
              val subslice : 'a slice * int * int option -> 'a slice
              val vector : 'a slice -> 'a Vector.vector
              val copy : { src : 'a slice, dst : 'a Array.array, di : int } -> unit
              val copyVec : { src : 'a VectorSlice.slice, dst : 'a Array.array, di : int } -> unit
          end = struct
(* invariant: 0 <= start <= start + length <= Array.length base *)
type 'a slice = { base : 'a array
                , start : int
                , length : int
                }
val length : 'a slice -> int = #length
fun sub ({ base, start, length }, i) = if 0 <= i andalso i < length then
                                           Unsafe.Array.sub (base, start + i)
                                       else
                                           raise Subscript
fun update ({ base, start, length }, i, x) = if 0 <= i andalso i < length then
                                                 Unsafe.Array.update (base, start + i)
                                             else
                                                 raise Subscript
fun full a = { base = a, start = 0, length = Array.length a }
fun slice (a, i, NONE) = if 0 <= i andalso i <= Array.length a then
                             { base = a, start = i, length = Array.length a - i }
                         else
                             raise Subscript
  | slice (a, i, SOME n) = if 0 <= i andalso 0 <= n andalso i + n <= Array.length a then
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
fun vector { base, start, length } = Vector.tabulate (length, fn i => Unsafe.Array.sub (base, start + i))
fun copy { src = { base, start, length }, dst, di } = let fun forward i = if i >= length then
                                                                              ()
                                                                          else
                                                                              ( Array.update (dst, di + i, Unsafe.Array.sub (base, start + i))
                                                                              ; forward (i + 1)
                                                                              )
                                                          fun backward i = if i < 0 then
                                                                               ()
                                                                           else
                                                                               ( Array.update (dst, di + i, Unsafe.Array.sub (base, start + i))
                                                                               ; backward (i - 1)
                                                                               )
                                                      in if start >= di then
                                                             forward 0
                                                         else
                                                             backward (length - 1)
                                                      end
fun copyVec { src = slice, dst, di } = let val length = VectorSlice.length slice
                                           fun loop i = if i >= length then
                                                            ()
                                                        else
                                                            ( Array.update (dst, di + i, VectorSlice.sub (slice, i))
                                                            ; loop (i + 1)
                                                            )
                                       in loop 0
                                       end
end
