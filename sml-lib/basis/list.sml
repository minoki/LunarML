(* depends on int, option, General.Subscript, General.order *)
structure List : sig
              datatype list = datatype list
              exception Empty
              val null : 'a list -> bool
              val length : 'a list -> int
              val @ : 'a list * 'a list -> 'a list
              val hd : 'a list -> 'a
              val tl : 'a list -> 'a list
              val last : 'a list -> 'a
              val getItem : 'a list -> ('a * 'a list) option
              val nth : 'a list * int -> 'a
              val take : 'a list * int -> 'a list
              val drop : 'a list * int -> 'a list
              val rev : 'a list -> 'a list
              val concat : 'a list list -> 'a list
              val revAppend : 'a list * 'a list -> 'a list
              val app : ('a -> unit) -> 'a list -> unit
              val map : ('a -> 'b) -> 'a list -> 'b list
              val mapPartial : ('a -> 'b option) -> 'a list -> 'b list
              val find : ('a -> bool) -> 'a list -> 'a option
              val filter : ('a -> bool) -> 'a list -> 'a list
              val partition : ('a -> bool) -> 'a list -> 'a list * 'a list
              val foldl : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b
              val foldr : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b
              val exists : ('a -> bool) -> 'a list -> bool
              val all : ('a -> bool) -> 'a list -> bool
              val tabulate : int * (int -> 'a) -> 'a list
              val collate : ('a * 'a -> order) -> 'a list * 'a list -> order
          end = struct
datatype list = datatype list
exception Empty
fun null [] = true
  | null _ = false
local
    fun doLength (acc, []) = acc : int
      | doLength (acc, x :: xs) = doLength (acc + 1, xs)
in
fun length xs = doLength (0, xs)
end
fun [] @ ys = ys
  | (x :: xs) @ ys = x :: (xs @ ys)
fun hd [] = raise Empty
  | hd (x :: _) = x
fun tl [] = raise Empty
  | tl (_ :: xs) = xs
fun last [x] = x
  | last (_ :: xs) = last xs
  | last [] = raise Empty
fun getItem [] = NONE
  | getItem (x :: xs) = SOME (x, xs)
fun nth (x :: _, 0) = x
  | nth (_ :: xs, n) = nth (xs, n - 1)
  | nth ([], _) = raise Subscript
fun take (_, 0) = []
  | take (x :: xs, n) = x :: take (xs, n - 1)
  | take ([], _) = raise Subscript
fun drop (xs, 0) = xs
  | drop (_ :: xs, n) = drop (xs, n - 1)
  | drop ([], _) = raise Subscript
fun revAppend ([], ys) = ys
  | revAppend (x :: xs, ys) = revAppend (xs, x :: ys)
fun rev xs = revAppend (xs, [])
fun app f [] = ()
  | app f (x :: xs) = (f x; app f xs)
fun map f [] = []
  | map f (x :: xs) = f x :: map f xs
fun mapPartial f [] = []
  | mapPartial f (x :: xs) = case f x of
                                 NONE => mapPartial f xs
                               | SOME y => y :: mapPartial f xs
fun find f [] = NONE
  | find f (x :: xs) = if f x then
                           SOME x
                       else
                           find f xs
fun filter f [] = []
  | filter f (x :: xs) = if f x then
                             x :: filter f xs
                         else
                             filter f xs
fun partition f [] = ([], [])
  | partition f (x :: xs) = if f x then
                                let val (l, r) = partition f xs
                                in (x :: l, r)
                                end
                            else
                                let val (l, r) = partition f xs
                                in (l, x :: r)
                                end
fun foldl f init [] = init
  | foldl f init (x :: xs) = foldl f (f (x, init)) xs
fun foldr f init [] = init
  | foldr f init (x :: xs) = f (x, foldr f init xs)
fun concat xs = foldr (op @) [] xs
fun exists f [] = false
  | exists f (x :: xs) = f x orelse exists f xs
fun all f [] = true
  | all f (x :: xs) = f x andalso all f xs
fun tabulate (n, f) = if n < 0 then
                          raise Size
                      else
                          let fun go i = if i >= n then
                                             []
                                         else
                                             f i :: go (i + 1)
                          in go 0
                          end
fun collate compare ([], []) = EQUAL
  | collate compare (_ :: _, []) = GREATER
  | collate compare ([], _ :: _) = LESS
  | collate compare (x :: xs, y :: ys) = case compare (x, y) of
                                             EQUAL => collate compare (xs, ys)
                                           | c => c
end; (* structure List *)
val op @ = List.@;
