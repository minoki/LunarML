Language
========

This section describes the language accepted by LunarML.

Standard ML '97
---------------

LunarML supports full SML '97 language, including the module system.
Some features conform to Successor ML rather than SML '97.

Successor ML
------------

LunarML supports some of `Successor ML <https://github.com/SMLFamily/Successor-ML>`_ features:

* ☑ Monomorphic non-exhaustive bindings
* ☑ Simplified recursive value bindings

   * SML '97-compatible ordering for type variables is also supported: ``val <tyvarseq> rec <valbind>``

* ☑ Abstype as derived form
* ☑ Fixed manifest type specifications
* ☑ Abolish sequenced type realizations

   * ``and type`` is allowed by default; You can use ``"allowWhereAndType false"`` annotation to disable it.

* ☑ Line comments
* ☑ Extended literal syntax

   * ☑ Underscores (e.g. ``3.1415_9265``, ``0xffff_ffff``)
   * ☑ Binary notation (``0b``, ``0wb``)
   * ☑ Eight hex digits in text (``\Uxxxxxxxx``)

* ☑ Record punning
* ☑ Record extension
* ☑ Record update
* ☐ Conjunctive patterns
* ☐ Disjunctive patterns
* ☐ Nested matches
* ☐ Pattern guards
* ☑ Optional bars and semicolons
* ☐ Optional else branch
* ☑ Do declarations
* ☑ Withtype in signatures

Most of the implemented features are enabled by default; some features can be disabled by MLB annotations.

Language extensions
-------------------

Vector expressions and patterns
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Prior art:

* `SML/NJ <https://www.smlnj.org/doc/features.html>`_
* `MLton <http://mlton.org/SuccessorML>`_
* `Moscow ML <https://mosml.org/mosmlref.pdf>`_

:math:`\mathtt{\#[}\mathit{exp}_0\mathtt{,} \mathit{exp}_1\mathtt{,} \ldots\mathtt{,} \mathit{exp}_{n-1}\mathtt{]}` is equivalent to :math:`\mathtt{Vector.fromList [}\mathit{exp}_0\mathtt{,} \mathit{exp}_1\mathtt{,} \ldots\mathtt{,} \mathit{exp}_{n-1}\mathtt{]}` (with built-in value of ``Vector.fromList``) except that the vector expression is non-expansive if every :math:`\mathit{exp}_i` is non-expansive.

:math:`\mathtt{\#[}\mathit{pat}_0\mathtt{,} \mathit{pat}_1\mathtt{,} \ldots\mathtt{,} \mathit{pat}_{n-1}\mathtt{]}` matches a vector ``v`` if ``Vector.length v = n`` and for each ``i``, :math:`\mathit{pat}_i` matches ``Vector.sub (v, i)`` (with built-in values of ``Vector.length`` and ``Vector.sub``).

:math:`\mathtt{\#[}\mathit{pat}_0\mathtt{,} \mathit{pat}_1\mathtt{,} \ldots\mathtt{,} \mathit{pat}_{n-1}\mathtt{, ...]}` (the last ``...`` is verbatim) matches a vector ``v`` if ``Vector.length v >= n`` and for each ``i``, :math:`\mathit{pat}_i` matches ``Vector.sub (v, i)`` (with built-in values of ``Vector.length`` and ``Vector.sub``).

Hexadecimal floating-point constants
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Examples: ``0x1p~1022``, ``0x1.ffff_ffff_ffff_f``

The syntax of hexadecimal floating-point constants is::

   <hexadecimal-integer-constant> ::= '~'? '0' 'w'? 'x' <hexadecimal-digit-sequence>
   <hexadecimal-floating-point-constant> ::= '~'? '0x' <hexadecimal-digit-sequence> (<binary-exponent-part> | '.' <hexadecimal-digit-sequence> <binary-exponent-part>?)
   <hexadecimal-digit-sequence> ::= <hexadecimal-digit> ('_'* <hexadecimal-digit>)*
   <binary-exponent-part> ::= [pP] '~'? <digit> ('_'* <digit>)?

In short: the (binary) exponent part is optional and use tilde (``~``) for the negation symbol.

The hexadecimal floating-point constants must be exact; ``0x1.0000_0000_0000_01p0`` and ``0x1p1024`` are examples of invalid constants (assuming the type is ``Real64.real``).

UTF-encoded escape sequence in text constants
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The ``\u{}`` escape sequence allows you to embed a Unicode scalar value in a text constant.
The compiler encodes the character in UTF-8/16/32, depending on the type.

The scalar value is expressed in hexadecimal format; ``\u{3B1}`` or ``\u{3b1}`` for U+03B1 GREEK SMALL LETTER ALPHA.
Underscores are not allowed between hexadecimal digits.

When ``\u{}`` is used in a character constant, the character must be encoded as a single code unit in the corresponding UTF.

Examples:

.. code-block::

   ("\u{3042}" : string) = "\227\129\130"
   ("\u{80}" : string) = "\194\128"
   ("\u{1F600}" : string) = "\240\159\152\128"
   ("\u{1F600}" : String16.string) = "\uD83D\uDE00"
   if WideChar.maxOrd = 255 then
       ("\u{1F600}" : WideString.string) = "\240\159\152\128"
   else if WideChar.maxOrd = 65535 then
       ("\u{1F600}" : WideString.string) = "\uD83D\uDE00"
   else if WideChar.maxOrd = 1114111 then
       ("\u{1F600}" : WideString.string) = "\U0001F600"

Examples of invalid constants:

.. code-block::

   #"\u{80}" : char (* equivalent to #"\194\128" *)
   #"\u{10000}" : Char16.char (* would be equivalent to #"\uD800\uDC00" *)
   if WideChar.maxOrd = 65535 then
       #"\u{10000}" : WideChar.char (* would be equivalent to #"\uD800\uDC00" *)

Infix operators with surrounding dots
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Status: experimental.

To use this extension, ``allowInfixingDot`` annotation in MLB file is needed::

   ann "allowInfixingDot true" in
   ...
   end

``infexp_1 .longvid. infexp_2`` is equivalent to ``op longvid (infexp_1, infexp_2)``.

``pat_1 .longvid. pat_2`` is equivalent to ``op longvid (pat_1, pat_2)``.

Associativity of ``.strid1...stridN.vid.`` can be controlled by ``infix(r) <prec> .vid.`` declaration.
If no such declaration is found, ``infix 0`` is assumed.

Examples:

.. code-block::

   0wxdead .Word.andb. 0wxbeef; (* equivalent to Word.andb (0wxdead, 0wxbeef) *)
   fun a .foo. b = print (a ^ ", " ^ b ^ "\n"); (* equivalent to fun foo (a, b) = ... *)
   infix 7 .*.
   infix 6 .+.
   val x = 1 .Int.*. 2 .Int.+. 3 .Int.*. 4 (* equivalent to Int.+ (Int.* (1, 2), Int.* (3, 4)) *)

The standard library ``$(SML_LIB)/basis/basis.mlb`` contains the following declarations:

.. code-block::

   infix 7 .*. ./. .div. .mod. .quot. .rem.
   infix 6 .+. .-. .^.
   infix 4 .>. .>=. .<. .<=. .==. .!=. .?=.

Value description in comments
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Status: experimental (the starting symbol ``(*:`` may change).

To use this extension, ``valDescInComments`` annotation in MLB file is needed::

   ann "valDescInComments warn" in
   ...
   end
   (* Or:
   ann "valDescInComments error" in
   ...
   end
   *)

With this extension, comments that start with ``(*:`` will be parsed and the compatibility with the following value declaration (``val``, ``fun``) is checked against.
Type mismatch is reported as a warning or an error.

The content in the special comment does not affect type inference.

Good examples:

.. code-block:: sml

   (*: val fact : int -> int *)
   fun fact 0 = 1
     | fact n = n * fact (n - 1);

   (*: val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c *)
   fun curry f x y = f (x, y);

Bad examples:

.. code-block:: sml

   (*: val fact : IntInf.int -> IntInf.int *)
   (* Invalid: The inferred type is int -> int *)
   fun fact 0 = 1
     | fact n = n * fact (n - 1);

   (*: val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c *)
   (* Invalid: The inferred type is ('a * 'b -> 'c) -> 'b -> 'a -> 'c *)
   fun curry f x y = f (y, x);

Syntax::

   <valspec> ::= 'val' <valdesc>
   <valspecs> ::= <valspec> <valspecs>
                | <valspec>
   <valdescincomment> ::= '(*:' <valspecs> '*)'
   <dec> ::= <valdescincomment> 'val' ...
           | <valdescincomment> 'fun' ...

.. _importing-ecmascript-modules:

Importing ECMAScript Modules
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

An ECMAScript module can be imported with the ``_esImport`` declaration.
Examples::

   _esImport "module-name"; (* -> import "module-name"; *)
   _esImport defaultItem from "module-name"; (* -> import defaultItem from "module-name"; *)
   _esImport [pure] defaultItem from "module-name"; (* -> import defaultItem from "module-name"; with dead-code elimination enabled *)
   _esImport [pure] { foo, bar as barr, "fun" as fun' } from "module-name"; (* -> import { foo, bar as barr, fun as fun$PRIME } from "module-name"; with dead-code elimination enabled *)
   _esImport defaultItem, { foo, bar as barr, "fun" as fun' } from "module-name"; (* -> import defaultItem, { foo, bar as barr, fun as fun$PRIME } from "module-name"; *)

Syntax::

   _esImport <attrs> "module-name"; (* side-effect only *)
   _esImport <attrs> <vid> from "module-name"; (* default import *)
   _esImport <attrs> { <spec>, <spec>... } from "module-name"; (* named imports *)
   _esImport <attrs> <vid>, { <spec>, <spec>... } from "module-name"; (* default and named imports *)
   (*
   <attrs> ::=        (* default; the module may have side-effects *)
             | [pure] (* allow dead-code elimination *)
   <spec> ::= <vid>
            | <vid> as <vid>
            | <string> as <vid>
            | <vid> : <ty>
            | <vid> as <vid> : <ty>
            | <string> as <vid> : <ty>
    *)

Namespace imports are not supported.
