(* depends on unit *)
structure Option : sig
              datatype 'a option = NONE | SOME of 'a
              exception Option
              val getOpt : 'a option * 'a -> 'a
              val isSome : 'a option -> bool
              val valOf : 'a option -> 'a
              val filter : ('a -> bool) -> 'a -> 'a option
              val join : 'a option option -> 'a option
              val app : ('a -> unit) -> 'a option -> unit
              val map : ('a -> 'b) -> 'a option -> 'b option
              val mapPartial : ('a -> 'b option) -> 'a option -> 'b option
              val compose : ('a -> 'b) * ('c -> 'a option) -> 'c -> 'b option
              val composePartial : ('a -> 'b option) * ('c -> 'a option) -> 'c -> 'b option
          end = struct
datatype 'a option = NONE | SOME of 'a;
exception Option
fun getOpt (NONE, default) = default
  | getOpt (SOME x, _) = x
fun isSome (SOME _) = true
  | isSome NONE = false
fun valOf (SOME x) = x
  | valOf NONE = raise Option
fun filter pred x = if pred x then
                        SOME x
                    else
                        NONE
fun join (SOME x) = x
  | join NONE = NONE
fun app f (SOME x) = f x
  | app f NONE = ()
fun map f (SOME x) = SOME (f x)
  | map f NONE = NONE
fun mapPartial f (SOME x) = f x
  | mapPartial f NONE = NONE
fun compose (f, g) x = case g x of
                           SOME y => SOME (f y)
                         | NONE => NONE
fun composePartial (f, g) x = case g x of
                                  SOME y => f y
                                | NONE => NONE
end; (* structure Option *)
datatype option = datatype Option.option;
