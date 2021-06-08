# LunarML

A Standard ML compiler that produces Lua.

## Building

You need a recent version of MLton to build the executable, and Lua 5.3+ to run the compiled script.

```
$ make
$ make test
$ ./lunarml example/hello.sml
$ lua example/hello.lua
Hello world!
```

## Features

* [A subset of SML Basis Library](BasisLibrary.md)
* [Interface to Lua](LuaInterface.md)

The following SML '97 features are not implemented yet:

* `withtype`
* `abstype`
* Signatures
* Functors

Successor ML features:

* [x] Monomorphic non-exhaustive bindings
* [x] Simplified recursive value bindings
* [ ] Abstype as derived form
* [ ] Line comments
* [ ] Extended literal syntax
* [ ] Record punning
* [ ] Record extension
* [ ] Record update
* [ ] Conjunctive patterns
* [ ] Nested matches
* [ ] Pattern guards
* [ ] Optional bars and semicolons
* [ ] Optional else branch
* [ ] Do declarations
* [ ] Withtype in signatures

Other extensions planned:

* [ ] Vector expressions and patterns
* [ ] Packaged modules (like in Alice ML or HaMLet S)
* [ ] Hexadecimal floating-point literals
* [ ] Variably-encoded Unicode escape sequence in string literals

Intentional divergences from SML '97:

* The syntax of recursive value declaration is limited to the form of `val <tyvarseq> rec <valbind>` (SML '97-compatible) or `val rec <tyvarseq> <valbind>` (Successor ML).
* A value declaration is only generalized when the pattern is exhaustive: `let val SOME f = NONE in f (); f "" end` would not typecheck, rather than runtime error.  This behavior follows Successor ML.
* Recursive value declaration cannot override identifier status: `datatype t = f; val rec f = fn x => x;` is valid in SML '97, but not in this implementation.  This behavior follows Successor ML.
