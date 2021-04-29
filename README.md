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
type unit = ()
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
exception Match
exception Bind
val = : ''a * ''a -> bool
val := : 'a ref * 'a -> unit
val abs : realint -> realint  (* overloaded *)
val ~ : realint -> realint  (* overloaded *)
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
val ^ : string * string -> string
val print : string -> unit
val not : bool -> bool
structure Int : sig
  val + : int * int -> int
  val - : int * int -> int
  val * : int * int -> int
  val div : int * int -> int
  val mod : int * int -> int
  val < : int * int -> bool
  val <= : int * int -> bool
  val > : int * int -> bool
  val >= : int * int -> bool
  val ~ : int -> int
  val abs : int -> int
  val toString : int -> string
end
structure Array : sig
  val array : int * 'a -> 'a array
  val fromList : 'a list -> 'a array
  val tabulate : int * (int -> 'a) -> 'a array
  val length : 'a array -> int
  val sub : 'a array * int -> 'a
  val update : 'a array * int * 'a -> unit
end
structure Vector : sig
  val fromList : 'a list -> 'a vector
  val tabulate : int * (int -> 'a) -> 'a vector
  val length : 'a vector -> int
  val sub : 'a vector * int -> 'a
end
```

The following features are not implemented yet:

* Exceptions
* Modules

Intentional divergences from SML '97:

* The syntax of recursive value declaration is limited to the form of `val <tyvarseq> rec <valbind>` (SML '97-compatible) or `val rec <tyvarseq> <valbind>` (Successor ML).
* A value declaration is only generalized when the pattern is exhaustive: `let val SOME f = NONE in f (); f "" end` would not typecheck, rather than runtime error.  This behavior follows Successor ML.
* Recursive value declaration cannot override identifier status: `datatype t = f; val rec f = fn x => x;` is valid in SML '97, but not in this implementation.  This behavior follows Successor ML.
