Introduction
============

LunarML is the Standard ML compiler that produces Lua/JavaScript.

Features
--------

LunarML supports full SML '97 language, including module system.
LunarML also supports some of Successor ML features and other language extensions.

A subset of :ref:`SML Basis Library <sml-basis-library>` is supported.

For multi-file project, :ref:`ML Basis system <ml-basis-system>` is supported.

The program can communicate with other Lua or JavaScript code.

Some backends support delimited continuations.

Installation
------------

Pre-built binaries
^^^^^^^^^^^^^^^^^^

Docker image is available::

  $ docker pull ghcr.io/minoki/lunarml:latest
  $ docker run --rm --platform linux/amd64 -v "$(pwd)":/work -w /work ghcr.io/minoki/lunarml:latest lunarml compile example/hello.sml
  $ lua example/hello.lua
  Hello world!

In case you do not want to use Docker, there are precompiled scripts available in the tarball.
You can use ``install-precompiled-lua`` target to install ``lunarml.lua`` which can be run with Lua 5.3/5.4 and LuaFileSystem::

  $ make install-precompiled-lua PREFIX=/opt/lunarml
  $ export PATH=/opt/lunarml/bin:$PATH
  $ lunarml compile example/hello.sml

You can use ``install-precompiled-node`` target to install ``lunarml.mjs`` to be run with Node.js::

  $ make install-precompiled-node PREFIX=/opt/lunarml
  $ export PATH=/opt/lunarml/bin:$PATH
  $ lunarml compile example/hello.sml

Warning: Script-compiled LunarML is slow. Use native binary for serious use.

Building
^^^^^^^^

You need a recent version of MLton to build the executable, and Lua 5.3+ or recent Node.js to run the compiled script.

Run ``make`` to build ``bin/lunarml``::

  $ make
  $ make test-lua
  $ make test-lua-continuations
  $ make test-luajit
  $ make test-nodejs
  $ make test-nodejs-cps
  $ bin/lunarml compile example/hello.sml
  $ lua example/hello.lua
  Hello world!

You can install the built binary with ``make install``::

  $ make install PREFIX=/opt/lunarml
  $ export PATH="/opt/lunarml/bin:$PATH"
  $ lunarml compile example/hello.sml

Alternatively, you can use Docker to build and run LunarML::

  $ docker build --platform linux/amd64 -f package/docker/Dockerfile -t lunarml:0.1.0 .
  $ docker run --rm -v "$(pwd)":/work -w /work --platform linux/amd64 lunarml:0.1.0 lunarml compile example/hello.sml
  $ lua example/hello.lua
  Hello world!
