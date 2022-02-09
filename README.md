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

* Most of SML '97 language, including signatures and functors
* [A subset of SML Basis Library](BasisLibrary.md)
* [Interface to Lua](LuaInterface.md)
* ML Basis system like [MLton](http://mlton.org/MLBasis)

Successor ML features:

* [x] Monomorphic non-exhaustive bindings
* [x] Simplified recursive value bindings
    * SML '97-compatible ordering for type variables is also supported: `val <tyvarseq> rec <valbind>`
* [x] Abstype as derived form
* [x] Fixed manifest type specifications
* [ ] Abolish sequenced type realizations
* [ ] Line comments
* [ ] Extended literal syntax
    * [x] Underscores (`3.1415_9265`, `0xffff_ffff`)
    * [ ] Binary (`0b`, `0wb`)
    * [ ] Eight hex digits in text (`\Uxxxxxxxx`)
* [ ] Record punning
* [x] Record extension
* [x] Record update
* [ ] Conjunctive patterns
* [ ] Disjunctive patterns
* [ ] Nested matches
* [ ] Pattern guards
* [ ] Optional bars and semicolons
* [ ] Optional else branch
* [ ] Do declarations
* [x] Withtype in signatures

Other extensions planned:

* [x] Vector expressions and patterns
* [ ] Packaged modules (like in Alice ML or HaMLet S)
* [ ] Hexadecimal floating-point literals
* [ ] Variably-encoded Unicode escape sequence in string literals
