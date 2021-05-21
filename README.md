# LunarML

A Standard ML compiler that produces Lua.

## Building

You need MLton to build the executable, and Lua 5.3+ to run the compiled script.

```
$ make
$ make test
$ ./lunarml example/hello.sml
$ lua example/hello.lua
Hello world!
```

## Features

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

val abs : realint -> realint  (* overloaded *)
val ~ : num -> num  (* overloaded *)
val + : num * num -> num  (* overloaded *)
val - : num * num -> num  (* overloaded *)
val * : num * num -> num  (* overloaded *)
val / : Real * Real -> Real  (* overloaded *)
val div : wordint * wordint -> wordint  (* overloaded *)
val mod : wordint * wordint -> wordint  (* overloaded *)
val < : numtxt * numtxt -> bool  (* overloaded *)
val <= : numtxt * numtxt -> bool  (* overloaded *)
val > : numtxt * numtxt -> bool  (* overloaded *)
val >= : numtxt * numtxt -> bool  (* overloaded *)

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
  val < : char * char -> bool
  val <= : char * char -> bool
  val > : char * char -> bool
  val >= : char * char -> bool
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
  val < : char * char -> bool
  val <= : char * char -> bool
  val > : char * char -> bool
  val >= : char * char -> bool
end
val size = String.size
val ^ = String.^
val str = String.str

structure List : sig
  datatype list = datatype list
  exception Empty
  val @ : 'a list * 'a list -> 'a list
  val app : ('a -> unit) -> 'a list -> unit
  val foldl : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b
  val foldr : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b
  val hd : 'a list -> 'a
  val length : 'a list -> int
  val map : ('a -> 'b) -> 'a list -> 'b list
  val null : 'a list -> bool
  val rev : 'a list -> 'a list
  val tl : 'a list -> 'a list
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
structure Vector : sig
  datatype vector = datatype vector
  val fromList : 'a list -> 'a vector
  val tabulate : int * (int -> 'a) -> 'a vector
  val length : 'a vector -> int
  val sub : 'a vector * int -> 'a
  val foldl : ('a * 'b -> 'b) -> 'b -> 'a vector -> 'b
  val foldr : ('a * 'b -> 'b) -> 'b -> 'a vector -> 'b
end
val vector = Vector.fromList

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
```

Interface to Lua:

```sml
structure Lua : sig
  type value
  val sub : value * value -> value  (* t[k] *)
  val field : value * string -> value  (* t[k] *)
  val set : value * value * value -> unit  (* t[k] = v *)
  val global : string -> value  (* _ENV[name] *)
  val call : value -> value vector -> value vector  (* f(args) *)
  val method : value * string -> value vector -> value vector  (* f:name(args) *)
  val NIL : value  (* Lua nil *)
  val isNil : value -> bool  (* x == nil *)
  val isFalsy : value -> bool  (* not x *)
  val fromBool : bool -> value
  val fromInt : int -> value
  val fromWord : word -> value
  val fromReal : real -> value
  val fromString : string -> value
  val unsafeToValue : 'a -> value
  val unsafeFromValue : value -> 'a
  val newTable : unit -> value  (* {} *)
  val function : (value vector -> value vector) -> value
  val + : value * value -> value
  val - : value * value -> value
  val * : value * value -> value
  val / : value * value -> value
  val // : value * value -> value
  val % : value * value -> value
  val pow : value * value -> value  (* x ^ y *)
  val unm : value -> value  (* unary minus *)
  val andb : value * value -> value  (* x & y *)
  val orb : value * value -> value  (* x | y *)
  val xorb : value * value -> value  (* x ~ y *)
  val notb : value -> value  (* ~ x *)
  val << : value * value -> value
  val >> : value * value -> value
  val == : value * value -> bool
  val ~= : value * value -> bool
  val < : value * value -> bool
  val > : value * value -> bool
  val <= : value * value -> bool
  val >= : value * value -> bool
  val concat : value * value -> value  (* x .. y *)
  val length : value -> value  (* #x *)
end
```

The following features are not implemented yet:

* Functors and signatures

Intentional divergences from SML '97:

* The syntax of recursive value declaration is limited to the form of `val <tyvarseq> rec <valbind>` (SML '97-compatible) or `val rec <tyvarseq> <valbind>` (Successor ML).
* A value declaration is only generalized when the pattern is exhaustive: `let val SOME f = NONE in f (); f "" end` would not typecheck, rather than runtime error.  This behavior follows Successor ML.
* Recursive value declaration cannot override identifier status: `datatype t = f; val rec f = fn x => x;` is valid in SML '97, but not in this implementation.  This behavior follows Successor ML.
