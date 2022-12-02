signature VECTOR_SLICE = sig
    type 'a slice
    val length : 'a slice -> int
    val sub : 'a slice * int -> 'a
    val full : 'a Vector.vector -> 'a slice
    val slice : 'a Vector.vector * int * int option -> 'a slice
    val subslice : 'a slice * int * int option -> 'a slice
    val base : 'a slice -> 'a Vector.vector * int * int
    val vector : 'a slice -> 'a Vector.vector
    val concat : 'a slice list -> 'a Vector.vector
    val isEmpty : 'a slice -> bool
    val getItem : 'a slice -> ('a * 'a slice) option
    val appi : (int * 'a -> unit) -> 'a slice -> unit
    val app : ('a -> unit) -> 'a slice -> unit
    val mapi : (int * 'a -> 'b) -> 'a slice -> 'b Vector.vector
    val map : ('a -> 'b) -> 'a slice -> 'b Vector.vector
    val foldli : (int * 'a * 'b -> 'b) -> 'b -> 'a slice -> 'b
    val foldri : (int * 'a * 'b -> 'b) -> 'b -> 'a slice -> 'b
    val foldl : ('a * 'b -> 'b) -> 'b -> 'a slice -> 'b
    val foldr : ('a * 'b -> 'b) -> 'b -> 'a slice -> 'b
    val findi : (int * 'a -> bool) -> 'a slice -> (int * 'a) option
    val find : ('a -> bool) -> 'a slice -> 'a option
    val exists : ('a -> bool) -> 'a slice -> bool
    val all : ('a -> bool) -> 'a slice -> bool
    val collate : ('a * 'a -> order) -> 'a slice * 'a slice -> order
end

structure VectorSlice :> VECTOR_SLICE = struct
fun Vector_unsafeFromListRevN (n, xs) = _primCall "Vector.unsafeFromListRevN" (n, xs)
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
fun base { base = b, start, length } = (b, start, length)
fun vector { base, start, length } = Vector.tabulate (length, fn i => Unsafe.Vector.sub (base, start + i))
fun isEmpty { base, start, length } = length = 0
fun getItem { base, start, length } = if length > 0 then
                                          SOME (Unsafe.Vector.sub (base, start), { base = base, start = start + 1, length = length - 1 })
                                      else
                                          NONE
fun appi f { base, start, length } = let fun loop i = if i >= length then
                                                          ()
                                                      else
                                                          ( f (i, Unsafe.Vector.sub (base, start + i)) : unit
                                                          ; loop (i + 1)
                                                          )
                                     in loop 0
                                     end
fun app f { base, start, length } = let val n = start + length
                                        fun loop i = if i >= n then
                                                         ()
                                                     else
                                                         ( f (Unsafe.Vector.sub (base, i)) : unit
                                                         ; loop (i + 1)
                                                         )
                                    in loop start
                                    end
fun mapi f { base, start, length } = let fun loop (i, acc) = if i >= length then
                                                                 Vector_unsafeFromListRevN (length, acc)
                                                             else
                                                                 loop (i + 1, f (i, Unsafe.Vector.sub (base, start + i)) :: acc)
                                     in loop (0, [])
                                     end
fun map f { base, start, length } = let val n = start + length
                                        fun loop (i, acc) = if i >= n then
                                                                Vector_unsafeFromListRevN (length, acc)
                                                            else
                                                                loop (i + 1, f (Unsafe.Vector.sub (base, i)) :: acc)
                                     in loop (start, [])
                                     end
fun foldli f init { base, start, length } = let fun loop (i, acc) = if i >= length then
                                                                        acc
                                                                    else
                                                                        loop (i + 1, f (i, Unsafe.Vector.sub (base, start + i), acc))
                                            in loop (0, init)
                                            end
fun foldri f init { base, start, length } = let fun loop (i, acc) = if i < 0 then
                                                                        acc
                                                                    else
                                                                        loop (i - 1, f (i, Unsafe.Vector.sub (base, start + i), acc))
                                            in loop (length - 1, init)
                                            end
fun foldl f init { base, start, length } = let val n = start + length
                                               fun loop (i, acc) = if i >= n then
                                                                       acc
                                                                   else
                                                                       loop (i + 1, f (Unsafe.Vector.sub (base, i), acc))
                                           in loop (start, init)
                                           end
fun foldr f init { base, start, length } = let fun loop (i, acc) = if i < start then
                                                                       acc
                                                                   else
                                                                       loop (i - 1, f (Unsafe.Vector.sub (base, i), acc))
                                           in loop (start + length - 1, init)
                                           end
fun concat slices = Vector_unsafeFromListRevN (List.foldl (fn (slice, (n, revAcc)) => (n + length slice, foldl (op ::) revAcc slice)) (0, []) slices)
fun findi f { base, start, length } = let fun loop i = if i >= length then
                                                           NONE
                                                       else
                                                           let val x = Unsafe.Vector.sub (base, start + i)
                                                           in if f (i, x) then
                                                                  SOME (i, x)
                                                              else
                                                                  loop (i + 1)
                                                           end
                                      in loop 0
                                      end
fun find f { base, start, length } = let val n = start + length
                                         fun loop i = if i >= n then
                                                          NONE
                                                      else
                                                          let val x = Unsafe.Vector.sub (base, i)
                                                          in if f x then
                                                                 SOME x
                                                             else
                                                                 loop (i + 1)
                                                          end
                                     in loop start
                                     end
fun exists f { base, start, length } = let fun loop i = if i >= length then
                                                            false
                                                        else
                                                            if f (Unsafe.Vector.sub (base, start + i)) then
                                                                true
                                                            else
                                                                loop (i + 1)
                                       in loop 0
                                       end
fun all f { base, start, length } = let val n = start + length
                                        fun loop i = if i >= n then
                                                         true
                                                     else
                                                         f (Unsafe.Vector.sub (base, i)) andalso loop (i + 1)
                                    in loop start
                                    end
fun collate compare ({ base, start, length }, { base = base', start = start', length = length' })
    = let val xn = start + length
          val yn = start' + length'
          fun loop (i, j) = case (xn <= i, yn <= j) of
                                (true, true) => EQUAL
                              | (true, false) => LESS
                              | (false, true) => GREATER
                              | (false, false) => case compare (Unsafe.Vector.sub (base, i), Unsafe.Vector.sub (base', j)) of
                                                      EQUAL => loop (i + 1, j + 1)
                                                    | t => t
      in loop (start, start')
      end
end;
