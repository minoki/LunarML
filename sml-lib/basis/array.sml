signature ARRAY = sig
    datatype array = datatype array
    datatype vector = datatype vector
    val maxLen : int
    val array : int * 'a -> 'a array
    val fromList : 'a list -> 'a array
    val tabulate : int * (int -> 'a) -> 'a array
    val length : 'a array -> int
    val sub : 'a array * int -> 'a
    val update : 'a array * int * 'a -> unit
    val vector : 'a array -> 'a vector
    val copy : { src : 'a array, dst : 'a array, di : int } -> unit
    val copyVec : { src : 'a vector, dst : 'a array, di : int } -> unit
    val appi : (int * 'a -> unit) -> 'a array -> unit
    val app : ('a -> unit) -> 'a array -> unit
    val modifyi : (int * 'a -> 'a) -> 'a array -> unit
    val modify : ('a -> 'a) -> 'a array -> unit
    val foldli : (int * 'a * 'b -> 'b) -> 'b -> 'a array -> 'b
    val foldri : (int * 'a * 'b -> 'b) -> 'b -> 'a array -> 'b
    val foldl : ('a * 'b -> 'b) -> 'b -> 'a array -> 'b
    val foldr : ('a * 'b -> 'b) -> 'b -> 'a array -> 'b
    val findi : (int * 'a -> bool) -> 'a array -> (int * 'a) option
    val find : ('a -> bool) -> 'a array -> 'a option
    val exists : ('a -> bool) -> 'a array -> bool
    val all : ('a -> bool) -> 'a array -> bool
    val collate : ('a * 'a -> order) -> 'a array * 'a array -> order

    (* https://github.com/SMLFamily/BasisLibrary/wiki/2015-003g-Array *)
    val toList : 'a array -> 'a list
    val fromVector : 'a vector -> 'a array
    val toVector : 'a array -> 'a vector
end

structure Array :> ARRAY = struct
open Array (* length, sub, update, array, fromList [, tabulate] *)
datatype array = datatype array
datatype vector = datatype vector
val maxLen = Vector.maxLen
(*
fun length arr = _primCall "Array.length" (arr)
fun sub (arr, i) = if i < 0 orelse length arr <= i then
                       raise Subscript
                   else
                       Unsafe.Array.sub (arr, i)
fun update (arr, i, value) = if i < 0 orelse length arr <= i then
                                 raise Subscript
                             else
                                 Unsafe.Array.update (arr, i, value)
*)
fun vector a = Vector.tabulate (length a, fn i => Unsafe.Array.sub (a, i))
fun copy { src, dst, di } = let val srcLen = length src
                            in if 0 <= di andalso di + srcLen <= length dst then
                                   let fun loop i = if i >= srcLen then
                                                        ()
                                                    else
                                                        ( Unsafe.Array.update (dst, di + i, Unsafe.Array.sub (src, i))
                                                        ; loop (i + 1)
                                                        )
                                   in loop 0
                                   end
                               else
                                   raise Subscript
                            end
fun copyVec { src, dst, di } = let val srcLen = Vector.length src
                               in if 0 <= di andalso di + Vector.length src <= length dst then
                                      let fun loop i = if i >= srcLen then
                                                           ()
                                                       else
                                                           ( Unsafe.Array.update (dst, di + i, Unsafe.Vector.sub (src, i))
                                                           ; loop (i + 1)
                                                           )
                                      in loop 0
                                      end
                                  else
                                      raise Subscript
                               end
fun appi f arr = let val n = length arr
                     fun loop i = if i >= n then
                                      ()
                                  else
                                      ( f (i, Unsafe.Array.sub (arr, i))
                                      ; loop (i + 1)
                                      )
                 in loop 0
                 end
fun app f arr = let val n = length arr
                    fun loop i = if i >= n then
                                     ()
                                 else
                                     ( f (Unsafe.Array.sub (arr, i))
                                     ; loop (i + 1)
                                     )
                in loop 0
                end
fun modifyi f arr = let val n = length arr
                        fun loop i = if i >= n then
                                         ()
                                     else
                                         let val x = Unsafe.Array.sub (arr, i)
                                             val y = f (i, x)
                                         in Unsafe.Array.update (arr, i, y)
                                          ; loop (i + 1)
                                         end
                    in loop 0
                    end
fun modify f arr = let val n = length arr
                       fun loop i = if i >= n then
                                        ()
                                    else
                                        let val x = Unsafe.Array.sub (arr, i)
                                            val y = f x
                                        in Unsafe.Array.update (arr, i, y)
                                         ; loop (i + 1)
                                        end
                   in loop 0
                   end
fun foldli f init arr = let val n = length arr
                            fun loop (i, acc) = if i >= n then
                                                    acc
                                                else
                                                    loop (i + 1, f (i, Unsafe.Array.sub (arr, i), acc))
                        in loop (0, init)
                        end
fun foldri f init arr = let fun loop (i, acc) = if i < 0 then
                                                    acc
                                                else
                                                    loop (i - 1, f (i, Unsafe.Array.sub (arr, i), acc))
                        in loop (length arr - 1, init)
                        end
fun foldl f init arr = let val n = length arr
                           fun loop (i, acc) = if i >= n then
                                                   acc
                                               else
                                                   loop (i + 1, f (Unsafe.Array.sub (arr, i), acc))
                       in loop (0, init)
                       end
fun foldr f init arr = let fun loop (i, acc) = if i < 0 then
                                                   acc
                                               else
                                                   loop (i - 1, f (Unsafe.Array.sub (arr, i), acc))
                       in loop (length arr - 1, init)
                       end
fun findi f v = let val n = length v
                    fun loop i = if i >= n then
                                     NONE
                                 else
                                     let val x = Unsafe.Array.sub (v, i)
                                     in if f (i, x) then
                                            SOME (i, x)
                                        else
                                            loop (i + 1)
                                     end
                in loop 0
                end
fun find f v = let val n = length v
                   fun loop i = if i >= n then
                                    NONE
                                else
                                    let val x = Unsafe.Array.sub (v, i)
                                    in if f x then
                                           SOME x
                                       else
                                           loop (i + 1)
                                    end
               in loop 0
               end
fun exists f arr = let val n = length arr
                       fun loop i = if i >= n then
                                        false
                                    else
                                        f (Unsafe.Array.sub (arr, i)) orelse loop (i + 1)
                   in loop 0
                   end
fun all f arr = let val n = length arr
                    fun loop i = if i >= n then
                                     true
                                 else
                                     f (Unsafe.Array.sub (arr, i)) andalso loop (i + 1)
                in loop 0
                end
fun collate compare (xs, ys) = let val xl = length xs
                                   val yl = length ys
                                   fun loop i = case (xl <= i, yl <= i) of
                                                    (true, true) => EQUAL
                                                  | (true, false) => LESS
                                                  | (false, true) => GREATER
                                                  | (false, false) => case compare (Unsafe.Array.sub (xs, i), Unsafe.Array.sub (ys, i)) of
                                                                          EQUAL => loop (i + 1)
                                                                        | t => t
                               in loop 0
                               end
(*
val array = _Prim.Array.array
val fromList = _Prim.Array.fromList
*)
fun tabulate (n, f) = _Prim.Array.fromList (List.tabulate (n, f))
fun toList a = List.tabulate (length a, fn i => Unsafe.Array.sub (a, i))
fun fromVector v = tabulate (Vector.length v, fn i => Unsafe.Vector.sub (v, i))
val toVector = vector
open Array (* may override tabulate *)
end; (* structure Array *)
