# LunarML

The Standard ML compiler that produces Lua/JavaScript.

The documentation is available at <https://lunarml.readthedocs.io/en/latest/>.

## Trying pre-built binaries

Docker image is available.

```
$ docker pull ghcr.io/minoki/lunarml:latest
$ docker run --rm --platform linux/amd64 -v "$(pwd)":/work -w /work ghcr.io/minoki/lunarml:latest lunarml compile example/hello.sml
$ lua example/hello.lua
Hello world!
```

In case you do not want to use Docker, there are precompiled scripts available in the tarball.
You can use `install-precompiled-lua` target to install `lunarml.lua` which can be run with Lua 5.3/5.4 and LuaFileSystem.

```
$ make install-precompiled-lua PREFIX=/opt/lunarml
$ export PATH=/opt/lunarml/bin:$PATH
$ lunarml compile example/hello.sml
```

You can use `install-precompiled-node` target to install `lunarml.mjs` to be run with Node.js.

```
$ make install-precompiled-node PREFIX=/opt/lunarml
$ export PATH=/opt/lunarml/bin:$PATH
$ lunarml compile example/hello.sml
```

Warning: Script-compiled LunarML is slow. Use native binary for serious use.

## Building and Installing

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

You can install the built binary with `make install`:

```
$ make install PREFIX=/opt/lunarml
$ export PATH="/opt/lunarml/bin:$PATH"
$ lunarml compile example/hello.sml
```

Alternatively, you can use Docker to build and run LunarML.

```sh-session
$ docker build --platform linux/amd64 -f package/docker/Dockerfile -t lunarml:0.2.1 .
$ docker run --rm -v "$(pwd)":/work -w /work --platform linux/amd64 lunarml:0.2.1 lunarml compile example/hello.sml
$ lua example/hello.lua
Hello world!
```

## Usage

```
lunarml compile [options] input.(sml|mlb)
```

Subcommands:

* `compile`: Compile a program.
* `check`: Type check only.
* `help`: Show help.
* `version`: Show version information.

Targets:

* Lua
    * `--lua` (default): Targets Lua 5.3+.
    * `--lua-continuations`: Targets Lua 5.3+. Supports one-shot delimited continuations. Also, supports deeply nested `handle`.
    * `--luajit`: Targets LuaJIT.
* JavaScript (ES2020+)
    * `--nodejs`: Produces a JavaScript program for Node.js. The default extension is `.mjs`.
    * `--nodejs-cps`: Produces a JavaScript program for Node.js (CPS mode; supports delimited continuations). The default extension is `.mjs`.
    * `--webjs`: Produces a JavaScript program for Web. The default extension is `.js`.
    * `--webjs-cps`: Produces a JavaScript program for Web (CPS mode; supports delimited continuations). The default extension is `.js`.

Output type:

* `--exe` (default): Produces Lua/JavaScript program.
* `--lib`: Produces a Lua/JavaScript module.

## Features

* Full SML '97 language, including signatures and functors
    * Note that some features conform to Successor ML rather than SML '97.
* Successor ML features
* Other language extensions
* A subset of SML Basis Library
* ML Basis system for multi-file project
* Interface to Lua
* Interface to JavaScript
* Delimited continuations
* Other libraries
