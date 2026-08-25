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
fun extract (s : string, i : int, NONE : int option) : string = if i < 0 orelse String.size s < i then
                                                                    raise Subscript
                                                                else
                                                                    String.substring (s, i, String.size s - i)
  | extract (s, i, SOME j) = String.substring (s, i, j)
(* isSubstring, collate, toString, scan, fromString, toCString, fromCString *)
open String (* size, ^, str, <, <=, >, >=, concat, implode, translate, map *)
end (* structure String *)
val op ^ : string * string -> string = String.^;
