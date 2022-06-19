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
              val isEmpty : 'a slice -> bool
              val getItem : 'a slice -> ('a * 'a slice) option
              val appi : (int * 'a -> unit) -> 'a slice -> unit
              val app : ('a -> unit) -> 'a slice -> unit
              val modifyi : (int * 'a -> 'a) -> 'a slice -> unit
              val modify : ('a -> 'a) -> 'a slice -> unit
              val foldli : (int * 'a * 'b -> 'b) -> 'b -> 'a slice -> 'b
              val foldri : (int * 'a * 'b -> 'b) -> 'b -> 'a slice -> 'b
              val foldl : ('a * 'b -> 'b) -> 'b -> 'a slice -> 'b
              val foldr : ('a * 'b -> 'b) -> 'b -> 'a slice -> 'b
              val findi : (int * 'a -> bool) -> 'a slice -> (int * 'a) option
              val find : ('a -> bool) -> 'a slice -> 'a option
              val exists : ('a -> bool) -> 'a slice -> bool
              val all : ('a -> bool) -> 'a slice -> bool
              val collate : ('a * 'a -> order) -> 'a slice * 'a slice -> order
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
fun isEmpty { base, start, length } = length = 0
fun getItem { base, start, length } = if length > 0 then
                                          SOME (Unsafe.Array.sub (base, start), { base = base, start = start + 1, length = length - 1 })
                                      else
                                          NONE
fun appi f { base, start, length } = let fun loop i = if i >= length then
                                                          ()
                                                      else
                                                          (f (i, Unsafe.Array.sub (base, start + i)) : unit; loop (i + 1))
                                     in loop 0
                                     end
fun app f { base, start, length } = let val z = start + length
                                        fun loop i = if i >= z then
                                                         ()
                                                     else
                                                         (f (Unsafe.Array.sub (base, i)) : unit; loop (i + 1))
                                    in loop start
                                    end
fun modifyi f { base, start, length } = let fun loop i = if i >= length then
                                                             ()
                                                         else
                                                             let val j = start + i
                                                             in Unsafe.Array.update (base, j, f (i, Unsafe.Array.sub (base, j)))
                                                              ; loop (i + 1)
                                                             end
                                        in loop 0
                                        end
fun modify f { base, start, length } = let val z = start + length
                                           fun loop i = if i >= z then
                                                            ()
                                                        else
                                                            ( Unsafe.Array.update (base, i, f (Unsafe.Array.sub (base, i)))
                                                            ; loop (i + 1)
                                                            )
                                       in loop start
                                       end
fun foldli f init { base, start, length } = let fun loop (i, acc) = if i >= length then
                                                                        acc
                                                                    else
                                                                        let val j = start + i
                                                                        in loop (i + 1, f (i, Unsafe.Array.sub (base, j), acc))
                                                                        end
                                            in loop (0, init)
                                            end
fun foldri f init { base, start, length } = let fun loop (i, acc) = if i < 0 then
                                                                        acc
                                                                    else
                                                                        let val j = start + i
                                                                        in loop (i - 1, f (i, Unsafe.Array.sub (base, j), acc))
                                                                        end
                                            in loop (length - 1, init)
                                            end
fun foldl f init { base, start, length } = let val z = start + length
                                               fun loop (i, acc) = if i >= z then
                                                                       acc
                                                                   else
                                                                       loop (i + 1, f (Unsafe.Array.sub (base, i), acc))
                                           in loop (start, init)
                                           end
fun foldr f init { base, start, length } = let fun loop (i, acc) = if i < start then
                                                                       acc
                                                                   else
                                                                       loop (i - 1, f (Unsafe.Array.sub (base, i), acc))
                                           in loop (start + length - 1, init)
                                           end
fun findi f { base, start, length } = let fun loop i = if i >= length then
                                                         NONE
                                                     else
                                                         let val x = Unsafe.Array.sub (base, start + i)
                                                         in if f (i, x) then
                                                                SOME (i, x)
                                                            else
                                                                loop (i + 1)
                                                         end
                                      in loop 0
                                      end
fun find f { base, start, length } = let val z = start + length
                                         fun loop i = if i >= z then
                                                          NONE
                                                      else
                                                          let val x = Unsafe.Array.sub (base, i)
                                                          in if f x then
                                                                 SOME x
                                                             else
                                                                 loop (i + 1)
                                                          end
                                     in loop start
                                     end
fun exists f { base, start, length } = let val z = start + length
                                           fun loop i = if i >= z then
                                                            false
                                                        else
                                                            if f (Unsafe.Array.sub (base, i)) then
                                                                true
                                                            else
                                                                loop (i + 1)
                                       in loop start
                                       end
fun all f { base, start, length } = let val z = start + length
                                        fun loop i = if i >= z then
                                                         true
                                                     else
                                                         if not (f (Unsafe.Array.sub (base, i))) then
                                                             false
                                                         else
                                                             loop (i + 1)
                                    in loop start
                                    end
fun collate compare (xs, ys) = let val xl = length xs
                                   val yl = length ys
                                   fun loop i = case (xl = i, yl = i) of
                                                    (true, true) => EQUAL
                                                  | (true, false) => LESS
                                                  | (false, true) => GREATER
                                                  | (false, false) => case compare (sub (xs, i), sub (ys, i)) of
                                                                          EQUAL => loop (i + 1)
                                                                        | t => t
                               in loop 0
                               end
end;
