JavaScript interface
====================

JavaScript features are accessible via :ref:`javascript-structure`.

ES modules can be imported with ``_esImport`` declaration: :ref:`importing-ecmascript-modules`.

ES export
---------

To produce an ES module, use ``--lib`` compiler option and define a variable or structure named ``export``.

For example,

::

   val export = <some value>;

will compile to

::

   export default <some value>;

and

.. code-block:: sml

   structure export = struct
     val foo = "string"
     val bar = 42
     val fun' = "fun" (* SML keywords can be escaped by suffixing with a prime *)
   end

will compile to:

.. code-block:: javascript

   const foo = "string";
   const bar = 42;
   const fun = "fun";
   export { foo, bar, fun };

Note on CPS mode: You cannot call certain functions including ``print`` at top-level, because they are "async".
In future, this limitation may be lifted by using top-level await.

Internal representation
-----------------------

Warning: The internal representation of data types may change in the future.
Do not rely on it!

``unit`` (empty record)
   ``undefined``.

``bool``
   JavaScript's boolean; ``true`` or ``false``.

``int``
   54-bit signed integer, as a subset of Number.
   Overflows are always checked.

``word``
   32-bit unsigned integer, as a subset of Number.
   The sign of zero must be positive.

``real``
   64-bit floating-point number (JavaScript's native Number).

``char``
   8-bit unsigned integer.
   The sign of zero must be positive.

``string``
   ``Uint8Array``. Must not be modified.

``WideChar.char``
   16-bit unsigned integer.
   The sign of zero must be positive.

``WideString.string``
   16-bit string (JavaScript's native String).

``Int54.int``
   54-bit signed integer, as a subset of Number (close to "safe integer").
   The range is :math:`[-2^{53},2^{53}-1]`.
   The sign of zero may be negative.
   Overflows are always checked.

``Int64.int``
   BigInt.
   Overflows are always checked.

``IntInf.int``
   BigInt.

``Word64.word``
   BigInt.

``'a vector``
   Array.
   Must not be modified.

``'a array``
   Array.

``JavaScript.value``
   Any JavaScript value, including ``undefined`` or ``null``.

Non-empty record
   ``{ "0": <#1 of the record>, "foo": <#foo of the record> }``
