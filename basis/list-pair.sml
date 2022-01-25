structure ListPair : sig
              exception UnequalLengths
              val zip : 'a list * 'b list -> ('a * 'b) list
              val zipEq : 'a list * 'b list -> ('a * 'b) list
              val app : ('a * 'b -> unit) -> 'a list * 'b list -> unit
              val appEq : ('a * 'b -> unit) -> 'a list * 'b list -> unit
              val map : ('a * 'b -> 'c) -> 'a list * 'b list -> 'c list
              val mapEq : ('a * 'b -> 'c) -> 'a list * 'b list -> 'c list
              val foldl : ('a * 'b * 'c -> 'c) -> 'c -> 'a list * 'b list -> 'c
              val foldlEq : ('a * 'b * 'c -> 'c) -> 'c -> 'a list * 'b list -> 'c
              val all : ('a * 'b -> bool) -> 'a list * 'b list -> bool
              val allEq : ('a * 'b -> bool) -> 'a list * 'b list -> bool
          end = struct
exception UnequalLengths
fun checkEq ([], []) = true
  | checkEq (_ :: xs, _ :: ys) = checkEq (xs, ys)
  | checkEq (_, _) = raise UnequalLengths
fun zip ([], _) = []
  | zip (_, []) = []
  | zip (x :: xs, y :: ys) = (x, y) :: zip (xs, ys)
fun zipEq ([], []) = []
  | zipEq (x :: xs, y :: ys) = (x, y) :: zipEq (xs, ys)
  | zipEq (_, _) = raise UnequalLengths
fun app f = let fun go (x :: xs, y :: ys) = (f (x, y); go (xs, ys))
                  | go (_, _) = ()
            in go
            end
fun appEq f = let fun go (x :: xs, y :: ys) = (f (x, y); go (xs, ys))
                    | go ([], []) = ()
                    | go (_, _) = raise UnequalLengths
              in go
              end
fun map f = let fun go (x :: xs, y :: ys) = f (x, y) :: go (xs, ys)
                  | go (_, _) = []
            in go
            end
fun mapEq f = let fun go (x :: xs, y :: ys) = f (x, y) :: go (xs, ys)
                    | go ([], []) = []
                    | go (_, _) = raise UnequalLengths
              in go
              end
fun foldl f init (xs, ys) = let fun go (acc, [], _) = acc
                                  | go (acc, _, []) = acc
                                  | go (acc, x :: xs, y :: ys) = go (f (x, y, acc), xs, ys)
                            in go (init, xs, ys)
                            end
fun foldlEq f init (xs, ys) = let fun go (acc, [], []) = acc
                                    | go (acc, x :: xs, y :: ys) = go (f (x, y, acc), xs, ys)
                                    | go (_, _, _) = raise UnequalLengths
                              in go (init, xs, ys)
                              end
fun all f = let fun go ([], _) = true
                  | go (_, []) = true
                  | go (x :: xs, y :: ys) = f (x, y) andalso go (xs, ys)
            in go
            end
fun allEq f = let fun go ([], []) = true
                    | go (x :: xs, y :: ys) = f (x, y) andalso go (xs, ys)
                    | go (_, _) = false
              in go
              end
end
