structure String : sig
              type string = string
              type char = char
              val size : string -> int
              val sub : string * int -> char
              val extract : string * int * int option -> string
              val substring : string * int * int -> string
              val ^ : string * string -> string
              val concat : string list -> string
              val concatWith : string -> string list -> string
              val str : char -> string
              val implode : char list -> string
              val explode : string -> char list
              val map : (char -> char) -> string -> string
              val translate : (char -> string) -> string -> string
              val tokens : (char -> bool) -> string -> string list
              val fields : (char -> bool) -> string -> string list
              val isPrefix : string -> string -> bool
              val isSuffix : string -> string -> bool
              val compare : string * string -> order
              val < : string * string -> bool
              val <= : string * string -> bool
              val > : string * string -> bool
              val >= : string * string -> bool
              val implodeRev : char list -> string
          end = struct
type string = string
type char = char
val size = String.size
val str = String.str
val op ^ = String.^
val sub = String.sub
val implode = String.implode
fun implodeRev l = implode (List.rev l)
fun substring (s : string, i : int, j : int) : string = if i < 0 orelse j < 0 orelse size s < i + j then
                                                            raise Subscript
                                                        else
                                                            JavaScript.unsafeFromValue (JavaScript.method (JavaScript.unsafeToValue s, "subarray") #[JavaScript.fromInt i, JavaScript.fromInt (i + j)])
fun extract (s : string, i : int, NONE : int option) : string = if i < 0 orelse size s < i then
                                                                    raise Subscript
                                                                else
                                                                    JavaScript.unsafeFromValue (JavaScript.method (JavaScript.unsafeToValue s, "subarray") #[JavaScript.fromInt i, JavaScript.fromInt (size s)])
  | extract (s, i, SOME j) = substring (s, i, j)
fun concatWith (s : string) (l : string list) : string = _primCall "call2" (_Prim.String.concatWith, s, l)
fun explode (s : string) : char list = Vector.foldr (op ::) [] (Vector.tabulate (size s, fn i => sub (s, i)))
fun tokens f s = let fun go (revTokens, acc, []) = List.rev (if List.null acc then revTokens else implodeRev acc :: revTokens)
                       | go (revTokens, acc, x :: xs) = if f x then
                                                            go (if List.null acc then revTokens else implodeRev acc :: revTokens, [], xs)
                                                        else
                                                            go (revTokens, x :: acc, xs)
                 in go ([], [], explode s)
                 end
fun fields f s = let fun go (revFields, acc, []) = List.rev (implodeRev acc :: revFields)
                       | go (revFields, acc, x :: xs) = if f x then
                                                            go (implodeRev acc :: revFields, [], xs)
                                                        else
                                                            go (revFields, x :: acc, xs)
                 in go ([], [], explode s)
                 end
fun isPrefix prefix s = let val n = size prefix
                        in if n > size s then
                               false
                           else
                               substring (s, 0, n) = prefix
                        end
fun isSuffix suffix s = let val n = size suffix
                            val m = size s
                        in if n > m then
                               false
                           else
                               substring (s, m - n, n) = suffix
                        end
(* isSubstring, collate, toString, scan, fromString, toCString, fromCString *)
fun compare (s, t) = if s = t then
                         EQUAL
                     else if String.< (s, t) then
                         LESS
                     else
                         GREATER
open String (* size, ^, str, <, <=, >, >=, concat, implode, translate, map *)
end (* structure String *)
val op ^ : string * string -> string = String.^;
