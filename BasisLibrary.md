# SML Basis Library

Available types and functions:

```sml
type unit = {}
eqtype int  (* primitive *)
eqtype word  (* primitive *)
type real  (* primitive *)
eqtype string  (* primitive *)
eqtype char  (* primitive *)
type exn  (* primitive *)
datatype 'a ref = ref of 'a  (* primitive *)
datatype bool = false | true
datatype 'a list = nil | :: of 'a * 'a list
eqtype 'a array  (* primitive *)
eqtype 'a vector  (* primitive *)

val = : ''a * ''a -> bool
val <> : ''a * ''a -> bool
val abs : ∀'a:realint. 'a -> 'a  (* overloaded *)
val ~ : ∀'a:num. 'a -> 'a  (* overloaded *)
val + : ∀'a:num. 'a * 'a -> 'a  (* overloaded *)
val - : ∀'a:num. 'a * 'a -> 'a  (* overloaded *)
val * : ∀'a:num. 'a * 'a -> 'a  (* overloaded *)
val / : ∀'a:Real. 'a * 'a -> 'a  (* overloaded *)
val div : ∀'a:wordint. 'a * 'a -> 'a  (* overloaded *)
val mod : ∀'a:wordint. 'a * 'a -> 'a  (* overloaded *)
val < : ∀'a:numtxt. 'a * 'a -> bool  (* overloaded *)
val <= : ∀'a:numtxt. 'a * 'a -> bool  (* overloaded *)
val > : ∀'a:numtxt. 'a * 'a -> bool  (* overloaded *)
val >= : ∀'a:numtxt. 'a * 'a -> bool  (* overloaded *)

structure General : sig
  type unit = {}
  type exn = exn
  exception Bind
  exception Match
  exception Chr
  exception Div
  exception Domain
  exception Fail of string
  exception Overflow
  exception Size
  exception Span
  exception Subscript
  datatype order = LESS | EQUAL | GREATER
  val ! : 'a ref -> 'a
  val := : 'a ref * 'a -> unit
  val before : 'a * unit -> 'a
  val ignore : 'a -> unit
  val o : ('b -> 'c) * ('a -> 'b) -> 'a -> 'c
end
open General

structure Bool : sig
  datatype bool = datatype bool
  val not : bool -> bool
  val toString : bool -> string
end
val not = Bool.not

structure Int : sig
  type int = int
  val toInt : int -> int
  val fromInt : int -> int
  val precision : int option
  val minInt : int option
  val maxInt : int option
  val + : int * int -> int
  val - : int * int -> int
  val * : int * int -> int
  val div : int * int -> int
  val mod : int * int -> int
  val compare : int * int -> order
  val < : int * int -> bool
  val <= : int * int -> bool
  val > : int * int -> bool
  val >= : int * int -> bool
  val ~ : int -> int
  val abs : int -> int
  val min : int * int -> int
  val max : int * int -> int
  val sign : int -> int
  val sameSign : int * int -> bool
  val toString : int -> string
  val fromString : string -> int option
end

structure Word : sig
  type word = word
  val wordSize : int
  val toInt : word -> int
  val toIntX : word -> int
  val fromInt : int -> word
  val andb : word * word -> word
  val orb : word * word -> word
  val xorb : word * word -> word
  val notb : word -> word
  val << : word * word -> word
  val >> : word * word -> word
  val ~>> : word * word -> word
  val + : word * word -> word
  val - : word * word -> word
  val * : word * word -> word
  val div : word * word -> word
  val mod : word * word -> word
  val ~ : word -> word
  val compare : word * word -> order
  val < : word * word -> bool
  val <= : word * word -> bool
  val > : word * word -> bool
  val >= : word * word -> bool
  val min : word * word -> word
  val max : word * word -> word
  val toString : word -> string
end

structure Real : sig
  type real = real
  val + : real * real -> real
  val - : real * real -> real
  val * : real * real -> real
  val / : real * real -> real
  val ~ : real -> real
  val abs : real -> real
  val < : real * real -> bool
  val <= : real * real -> bool
  val > : real * real -> bool
  val >= : real * real -> bool
end

structure Math : sig
  type real = real
  val pi : real
  val sqrt : real -> real
  val sin : real -> real
  val cos : real -> real
  val tan : real -> real
  val asin : real -> real
  val acos : real -> real
  val atan : real -> real
  val atan2 : real * real -> real
  val exp : real -> real
  val pow : real * real -> real
  val ln : real -> real
  val log10 : real -> real
end

structure Char : sig
  type char = char
  type string = string
  val minChar : char
  val maxChar : char
  val maxOrd : int
  val ord : char -> int
  val chr : int -> char
  val succ : char -> char
  val pred : char -> char
  val compare : char * char -> order
  val < : char * char -> bool
  val <= : char * char -> bool
  val > : char * char -> bool
  val >= : char * char -> bool
  val contains : string -> char -> bool
  val notContains : string -> char -> bool
  val isAscii : char -> bool
  val toLower : char -> char
  val toUpper : char -> char
  val isAlpha : char -> bool
  val isAlphaNum : char -> bool
  val isCntrl : char -> bool
  val isDigit : char -> bool
  val isGraph : char -> bool
  val isHexDigit : char -> bool
  val isLower : char -> bool
  val isPrint : char -> bool
  val isSpace : char -> bool
  val isPunct : char -> bool
  val isUpper : char -> bool
end

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
  val < : string * string -> bool
  val <= : string * string -> bool
  val > : string * string -> bool
  val >= : string * string -> bool
end
val size = String.size
val ^ = String.^
val str = String.str

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
end
val @ = List.@
val app = List.app
val foldl = List.foldl
val foldr = List.foldr
val hd = List.hd
val length = List.length
val map = List.map
val null = List.null
val rev = List.rev
val tl = List.tl

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
end
datatype option = datatype Option.option
val getOpt = Option.getOpt
val isSome = Option.isSome
val valOf = Option.valOf

structure Vector : sig
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
val vector = Vector.fromList

structure Array : sig
  datatype array = datatype array
  datatype vector = datatype vector
  val array : int * 'a -> 'a array
  val fromList : 'a list -> 'a array
  val tabulate : int * (int -> 'a) -> 'a array
  val length : 'a array -> int
  val sub : 'a array * int -> 'a
  val update : 'a array * int * 'a -> unit
end

structure IO : sig
  exception Io of { name : string
                  , function : string
                  , cause : exn
                  }
end

structure TextIO : sig
  type instream
  type outstream
  type vector = string
  type elem = char
  val input1 : instream -> elem option
  val inputN : instream * int -> vector
  val inputAll : instream -> vector
  val closeIn : instream -> unit
  val output : outstream * vector -> unit
  val output1 : outstream * elem -> unit
  val flushOut : outstream -> unit
  val closeOut : outstream -> unit
  val inputLine : instream -> string option
  val openIn : string -> instream
  val openOut : string -> outstream
  val openAppend : string -> outstream
  val stdIn : instream
  val stdOut : outstream
  val stdErr : outstream
  val print : string -> unit
end
val print = TextIO.print

structure OS : sig
  structure FileSys : sig
    val chDir : string -> unit (* requires LuaFileSystem *)
    val getDir : unit -> string (* requires LuaFileSystem *)
    val mkDir : string -> unit (* requires LuaFileSystem *)
    val rmDir : string -> unit (* requires LuaFileSystem *)
    val isDir : string -> bool (* requires LuaFileSystem *)
    val isLink : string -> bool (* requires LuaFileSystem *)
    val readLink : string -> string (* requires LuaFileSystem 1.7.0 or later *)
    val remove : string -> unit
    val rename : { old : string, new : string } -> unit
  end
  structure IO : sig
  end
  structure Path : sig
  end
  structure Process : sig
    type status
    val success : status
    val failure : status
    val isSuccess : status -> bool
    val system : string -> status
    val exit : status -> 'a
    val terminate : status -> 'a
    val getEnv : string -> string option
  end
  eqtype syserror
  exception SysErr of string * syserror option
end

structure CommandLine : sig
  val name : unit -> string
  val arguments : unit -> string list
end
```
