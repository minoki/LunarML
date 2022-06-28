# LunarML

A Standard ML compiler that produces Lua/JavaScript.

## Building

You need a recent version of MLton to build the executable, and Lua 5.3+ or recent Node.js to run the compiled script.

```
$ make
$ make test
$ make test-nodejs
$ make test-nodejs-cps
$ ./lunarml example/hello.sml
$ lua example/hello.lua
Hello world!
```

## Usage

```
./lunarml [options] input.(sml|mlb)
```

Modes of operation:

* `-mexe` (default): Produce Lua program.
* `-mlib`: Produce a Lua module.
* `--js`: Produce a JavaScript program.
* `--js-cps`: Produce a JavaScript program (CPS mode; supports delimited continuations).

See `--help` for more information.

## Features

* Most of SML '97 language, including signatures and functors
* [A subset of SML Basis Library](doc/BasisLibrary.md)
* [Interface to Lua](doc/LuaInterface.md)
* ML Basis system like [MLton](http://mlton.org/MLBasis)

Successor ML features:

* [x] Monomorphic non-exhaustive bindings
* [x] Simplified recursive value bindings
    * SML '97-compatible ordering for type variables is also supported: `val <tyvarseq> rec <valbind>`
* [x] Abstype as derived form
* [x] Fixed manifest type specifications
* [ ] Abolish sequenced type realizations
* [ ] Line comments
* [x] Extended literal syntax
    * [x] Underscores (e.g. `3.1415_9265`, `0xffff_ffff`)
    * [x] Binary notation (`0b`, `0wb`)
    * [x] Eight hex digits in text (`\Uxxxxxxxx`)
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
* [x] Hexadecimal floating-point constants (e.g. `0x1p1024`, `0x1.ffff_ffff_ffff_f`)
* [x] Variably-encoded Unicode escape sequence in string literals (e.g. `\u{3042}`)

The syntax of hexadecimal floating-point constants is:

```
<hexadecimal-integer-constant> ::= '~'? '0' 'w'? 'x' <hexadecimal-digit-sequence>
<hexadecimal-floating-point-constant> ::= '~'? '0x' <hexadecimal-digit-sequence> (<binary-exponent-part> | '.' <hexadecimal-digit-sequence> <binary-exponent-part>?)
<hexadecimal-digit-sequence> ::= <hexadecimal-digit> ('_'* <hexadecimal-digit>)*
<binary-exponent-part> ::= [pP] '~'? <digit> ('_'* <digit>)?
```

In short: the (binary) exponent part is optional and use tilde (`~`) for the negation symbol.

The `\u{}` escape sequence allows you to embed a Unicode scalar value in a string literal.
The compiler encodes the character in UTF-(8|16|32), depending on the string type.
