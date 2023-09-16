# JavaScript interface

## JavaScript API

JavaScript features are accessible via `JavaScript` structure in `$(SML_LIB)/basis/javascript.mlb`.

```sml
structure JavaScript : sig
  type value
  val undefined : value
  val null : value
  val sub : value * value -> value
  val field : value * WideString.string -> value
  val set : value * value * value -> unit
  val setField : value * WideString.string * value -> unit
  val global : WideString.string -> value
  val setGlobal : WideString.string * value -> unit
  val call : value -> value vector -> value
  val new : value -> value vector -> value
  val method : value * WideString.string -> value vector -> value
  val function : (value vector -> value) -> value
  val callback : (value vector -> unit) -> value
  val fromBool : bool -> value
  val fromInt : int -> value
  val fromWord : word -> value
  val fromReal : real -> value
  val fromWideString : WideString.string -> value
  val unsafeToValue : 'a -> value
  val unsafeFromValue : value -> 'a
  val === : value * value -> bool
  val !== : value * value -> bool
  val < : value * value -> bool
  val > : value * value -> bool
  val <= : value * value -> bool
  val >= : value * value -> bool
  val + : value * value -> value
  val - : value * value -> value
  val * : value * value -> value
  val / : value * value -> value
  val % : value * value -> value
  val negate : value -> value
  val andb : value * value -> value
  val orb : value * value -> value
  val xorb : value * value -> value
  val notb : value -> value
  val << : value * value -> value
  val >> : value * value -> value
  val >>> : value * value -> value
  val ** : value * value -> value
  val isFalsy : value -> bool
  val typeof : value -> WideString.string
  val newObject : unit -> value
  val encodeUtf8 : WideString.string -> string
  val decodeUtf8 : string -> WideString.string
  val toInt32 : value -> Int32.int
  val toUint32 : value -> Word32.word
end
```

## Using ES modules

You can use `_esImport` declaration to import ECMAScript modules.

```sml
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
```

Examples:

```sml
_esImport "module-name"; (* -> import "module-name"; *)
_esImport defaultItem from "module-name"; (* -> import defaultItem from "module-name"; *)
_esImport [pure] defaultItem from "module-name"; (* -> import defaultItem from "module-name"; with dead-code elimination enabled *)
_esImport [pure] { foo, bar as barr, "fun" as fun' } from "module-name"; (* -> import { foo, bar as barr, fun as fun$PRIME } from "module-name"; with dead-code elimination enabled *)
_esImport defaultItem, { foo, bar as barr, "fun" as fun' } from "module-name"; (* -> import defaultItem, { foo, bar as barr, fun as fun$PRIME } from "module-name"; *)
```

Namespace imports are not supported yet.

## Internal representation

Primitives

* `unit` (empty record): `undefined`
* `bool`: JavaScript's boolean; `true` or `false`
* `int`: 54-bit signed integer, as a subset of Number. Overflows are always checked.
* `word`: 32-bit unsigned integer, as a subset of Number. The sign of zero must be positive.
* `real`: 64-bit floating-point number (JavaScript's native Number).
* `char`: 8-bit unsigned integer. The sign of zero must be positive.
* `string`: Uint8Array. Must not be modified.
* `WideChar.char`: 16-bit unsigned integer. The sign of zero must be positive.
* `WideString.string`: 16-bit string (JavaScript's native String).
* `IntInf.int`: BigInt.
* `Int54.int`: 54-bit signed integer, as a subset of Number (close to "safe integer"). The range is `[-(2**53),2**53-1]`. The sign of zero may be negative. Overflows are always checked.
* `Int64.int`: BigInt. Overflows are always checked.
* `Word64.word`: BigInt.
* `'a vector`: Array. Must not be modified.
* `'a array`: Array.
* `JavaScript.value`: Any JavaScript value, including `undefined` or `null`.

Non-empty records: `{ "0": <#1 of the record>, foo = <#foo of the record> }`
