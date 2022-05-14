structure Vector : sig
              datatype vector = datatype vector
              (* val maxLen : int; defined later *)
              val fromList : 'a list -> 'a vector
              val tabulate : int * (int -> 'a) -> 'a vector
              val length : 'a vector -> int
              val sub : 'a vector * int -> 'a
              val update : 'a vector * int * 'a -> 'a vector
              val concat : 'a vector list -> 'a vector
              val appi : (int * 'a -> unit) -> 'a vector -> unit
              val app : ('a -> unit) -> 'a vector -> unit
              val mapi : (int * 'a -> 'b) -> 'a vector -> 'b vector
              val map : ('a -> 'b) -> 'a vector -> 'b vector
              val foldli : (int * 'a * 'b -> 'b) -> 'b -> 'a vector -> 'b
              val foldri : (int * 'a * 'b -> 'b) -> 'b -> 'a vector -> 'b
              val foldl : ('a * 'b -> 'b) -> 'b -> 'a vector -> 'b
              val foldr : ('a * 'b -> 'b) -> 'b -> 'a vector -> 'b
              val findi : (int * 'a -> bool) -> 'a vector -> (int * 'a) option
              val find : ('a -> bool) -> 'a vector -> 'a option
              val exists : ('a -> bool) -> 'a vector -> bool
              val all : ('a -> bool) -> 'a vector -> bool
              (* val collate : ('a * 'a -> order) -> 'a vector * 'a vector -> order; defined later *)
          end = struct
open Vector (* datatype vector, tabulate, concat, length, sub, fromList *)
fun update (vec, n, x) = tabulate (length vec, fn i => if i = n then
                                                           x
                                                       else
                                                           Unsafe.Vector.sub (vec, i)
                                  )
local
    fun foldli' (f, acc, vec, i) = if i >= length vec then
                                       acc
                                   else
                                       foldli' (f, f (i, Unsafe.Vector.sub (vec, i), acc), vec, i + 1)
    fun foldri' (f, acc, vec, i) = if i < 0 then
                                       acc
                                   else
                                       foldri' (f, f (i, Unsafe.Vector.sub (vec, i), acc), vec, i - 1)
in
fun foldli f init vec : 'b = foldli' (f, init, vec, 0)
fun foldri f init vec : 'b = foldri' (f, init, vec, length vec - 1)
end
local
    fun foldl' (f, acc, vec, i) = if i >= length vec then
                                      acc
                                  else
                                      foldl' (f, f (Unsafe.Vector.sub (vec, i), acc), vec, i + 1)
    fun foldr' (f, acc, vec, i) = if i < 0 then
                                      acc
                                  else
                                      foldr' (f, f (Unsafe.Vector.sub (vec, i), acc), vec, i - 1)
in
fun foldl (f : 'a * 'b -> 'b) (init : 'b) (vec : 'a vector) : 'b = foldl' (f, init, vec, 0)
fun foldr (f : 'a * 'b -> 'b) (init : 'b) (vec : 'a vector) : 'b = foldr' (f, init, vec, length vec - 1)
end
fun appi f vec = let val n = length vec
                     fun go i = if i = n then
                                    ()
                                else
                                    ( f (i, Unsafe.Vector.sub (vec, i)) : unit
                                    ; go (i + 1)
                                    )
                 in go 0
                 end
fun app f vec = let val n = length vec
                    fun go i = if i = n then
                                   ()
                               else
                                   ( f (Unsafe.Vector.sub (vec, i)) : unit
                                   ; go (i + 1)
                                   )
                in go 0
                end
fun mapi f vec = tabulate (length vec, fn i => f (i, Unsafe.Vector.sub (vec, i)))
fun map f vec = tabulate (length vec, fn i => f (Unsafe.Vector.sub (vec, i)))
fun findi f vec = let val n = length vec
                      fun go i = if i = n then
                                     NONE
                                 else
                                     let val x = Unsafe.Vector.sub (vec, i)
                                     in if f (i, x) then
                                            SOME (i, x)
                                        else
                                            go (i + 1)
                                     end
                   in go 0
                   end
fun find f vec = let val n = length vec
                     fun go i = if i = n then
                                    NONE
                                else
                                    let val x = Unsafe.Vector.sub (vec, i)
                                    in if f x then
                                           SOME x
                                       else
                                           go (i + 1)
                                    end
                 in go 0
                 end
fun exists f vec = let val n = length vec
                       fun go i = if i = n then
                                      false
                                  else
                                      f (Unsafe.Vector.sub (vec, i)) orelse go (i + 1)
                   in go 0
                   end
fun all f vec = let val n = length vec
                    fun go i = if i = n then
                                   true
                               else
                                   f (Unsafe.Vector.sub (vec, i)) andalso go (i + 1)
                in go 0
                end
end;
