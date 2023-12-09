signature STRING = sig
  eqtype string
  eqtype char
  val maxSize : int
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
  val isSubstring : string -> string -> bool
  val isSuffix : string -> string -> bool
  val compare : string * string -> order
  val collate : (char * char -> order) -> string * string -> order
  val < : string * string -> bool
  val <= : string * string -> bool
  val > : string * string -> bool
  val >= : string * string -> bool
  val toString : string -> String.string
  val scan : (Char.char, 'a) StringCvt.reader -> (string, 'a) StringCvt.reader
  val fromString : String.string -> string option
  val toCString : string -> String.string
  val fromCString : String.string -> string option

  (* https://github.com/SMLFamily/BasisLibrary/wiki/2015-003d-STRING *)
  val implodeRev : char list -> string
end
structure String : STRING where type string = string where type char = char = String
structure WideString : STRING where type char = WideChar.char = WideString;
