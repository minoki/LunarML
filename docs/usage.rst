
Usage
=====

The ``lunarml`` command takes a subcommand, zero or more options, and an input.
Example::

  lunarml compile [options] input.(sml|mlb)

Supported subcommands are:

``compile``
  Compile a program.

``help``
  Show help.

``version``
  Show version information.

Compiling: ``compile`` subcommand
---------------------------------

You can specify one of the target options, and specify output type.

Target options are:

``--lua`` (default)
  Targets Lua 5.3/5.4.

``--lua-continuations``
  Targets Lua 5.3/5.4.
  Supports one-shot delimited continuations.
  Also, supports deeply nested ``handle``.

``--luajit``
  Targets LuaJIT.

``--nodejs``
  Produces JavaScript program for Node.js.
  The produced JavaScript code is for ES2020 or later.
  The default extension for output is ``.mjs``.

``--nodejs-cps``
  Produces JavaScript program for Node.js.
  The produced JavaScript code is for ES2020 or later.
  This mode supports delimited continuations.
  The default extension for output is ``.mjs``.

``--webjs``
  Produces JavaScript program for Web.
  The produced JavaScript code is for ES2020 or later.
  The default extension for output is ``.js``.

``--webjs-cps``
  Produces JavaScript program for Web.
  The produced JavaScript code is for ES2020 or later.
  This mode supports delimited continuations.
  The default extension for output is ``.js``.

Output type is one of:

``--exe`` (default)
  Produce a program to be run by an interpreter.

``--lib``
  Produces a module to be loaded by other Lua/JavaScript programs.

Other options are:

``-o<file.ext>``, ``--output=<file.ext>``
  Specify a filename to output.

``--mlb-path-map=<file>``
  Specify an MLB path map.

``--mlb-path-var=<var>=<path>``
  Specify an MLB path variable.

``--default-ann <annotation>``
  Specify an MLB annotatinon.
