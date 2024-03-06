Lua interface
=============

Lua features are accessible via :ref:`lua-structure`.

Exporting a value
-----------------

To produce a Lua module, use ``--lib`` compiler option and define a variable or structure named ``export``.

For example,

::

   val export = <some value>;

will compile to

::

   return <some value>

and

.. code-block:: sml

   structure export = struct
     val foo = "string"
     val bar = 42
     val fun' = "fun" (* SML keywords can be escaped by suffixing with a prime *)
   end

will compile to:

.. code-block:: lua

   return {
     foo = "string",
     bar = 42,
     fun = "fun",
   }

Internal representation
-----------------------

Warning: The internal representation of data types may change in the future.
Do not rely on it!

``unit`` (empty record)
   ``nil``.

``bool``
   Lua's boolean; ``true`` or ``false``.

``int``
   Lua 5.3 or later: Lua's native integer (typically 64-bit).
   LuaJIT: 54-bit signed integer, using Lua's native number.
   Overflows are always checked.

``word``
   Lua 5.3 or later: Lua's native integer (typically 64-bit), using negative values for large values.
   LuaJIT: 32-bit unsigned integer, using Lua's native number.

``real``
   Lua's native number, typically 64-bit.

``char``
   8-bit unsigned integer.

``string``
   Lua's native string.

``Int54.int``
   Lua 5.3 or later: Lua's native integer.
   LuaJIT: Lua's native number.

``Int64.int``
   Lua 5.3 or later: Lua's native integer.
   LuaJIT: Boxed 64-bit signed integer: ``int64_t``.

``Word64.word``
   Lua 5.3 or later: Lua's native integer.
   LuaJIT: Boxed 64-bit unsigned integer: ``uint64_t``.

``'a list``
   ``nil`` for ``nil`` and ``{ [1] = <head>, [2] = <tail> }`` for ``<head> :: <tail>``.

``'a vector``
   ``{ n = <length>, [1] = <0th element>, [2] = <1st element>, ... }``; compatible with ``table.pack``.

``'a array``
   Same as ``'a vector``, but mutable.

``'a ref``
   ``{ [1] = <the payload> }``

``Lua.value``
   Any Lua value, including ``nil``.

Non-empty record
   ``{ [1] = <#1 of the record>, foo = <#foo of the record> }``
