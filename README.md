# LunarML

A Standard ML compiler that produces Lua/JavaScript.

## Building

You need a recent version of MLton to build the executable, and Lua 5.3+ or recent Node.js to run the compiled script.

```sh-session
$ make
$ make test-lua
$ make test-lua-continuations
$ make test-luajit
$ make test-nodejs
$ make test-nodejs-cps
$ bin/lunarml compile example/hello.sml
$ lua example/hello.lua
Hello world!
```

## Usage

```
lunarml [subcommand] [options] input.(sml|mlb)
```

Subcommands:

* `compile`: Compile a program.
* `help`: Show help.
* `version`: Show version information.

Targets:

* Lua
    * `--lua` (default): Targets Lua 5.3+.
    * `--lua-continuations`: Targets Lua 5.3+. Supports one-shot delimited continuations. Also, supports deeply nested `handle`.
    * `--luajit`: Targets LuaJIT.
* JavaScript (ES2020+)
    * `--js`: Produces a JavaScript program.
    * `--js-cps`: Produces a JavaScript program (CPS mode; supports delimited continuations).

Output type:

* `--exe` (default): Produces Lua/JavaScript program.
* `--lib`: Produces a Lua module.

## Features

* Full SML '97 language, including signatures and functors
    * Note that some features conform to Successor ML rather than SML '97.
* [Successor ML features](doc/SuccessorML.md)
* [Other language extensions](doc/Extensions.md)
* [A subset of SML Basis Library](doc/BasisLibrary.md)
* [Interface to Lua](doc/LuaInterface.md)
* [Interface to JavaScript](doc/JavaScriptInterface.md)
* [Delimited continuations](doc/DelimitedContinuations.md) (experimental)
* ML Basis system like [MLton](http://mlton.org/MLBasis)
