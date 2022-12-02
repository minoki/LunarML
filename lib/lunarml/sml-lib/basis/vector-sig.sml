signature VECTOR = sig
    datatype vector = datatype vector
    val maxLen : int
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
    val collate : ('a * 'a -> order) -> 'a vector * 'a vector -> order
end
structure Vector :> VECTOR = struct
open Vector
fun collate compare (xs, ys) = let val xl = length xs
                                   val yl = length ys
                                   fun go i = case (xl = i, yl = i) of
                                                  (true, true) => EQUAL
                                                | (true, false) => LESS
                                                | (false, true) => GREATER
                                                | (false, false) => case compare (Unsafe.Vector.sub (xs, i), Unsafe.Vector.sub (ys, i)) of
                                                                        EQUAL => go (i + 1)
                                                                      | t => t
                               in go 0
                               end
end;
