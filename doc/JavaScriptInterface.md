# JavaScript interface

## JavaScript API

JavaScript features are accessible via `JavaScript` structure.

```sml
structure JavaScript : sig
  type value
  val undefined : value
  val null : value
  val sub : value * value -> value
  val field : value * WideString.string -> value
  val set : value * value * value -> unit
  val global : WideString.string -> value
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
  val toInt32 : value -> int
  val toUint32 : value -> word
  val require : value (* Node.js *)
end
```

## Internal representation

Primitives

* `unit` (empty record): `undefined`
* `bool`: JavaScript's boolean; `true` or `false`
* `int`: 32-bit signed integer, as a subset of Number. The sign of zero must be positive. Overflows are always checked.
* `word`: 32-bit unsigned integer, as a subset of Number. The sign of zero must be positive.
* `real`: 64-bit floating-point number (JavaScript's native Number).
* `char`: 8-bit unsigned integer. The sign of zero must be positive.
* `string`: Uint8Array. Must not be modified.
* `WideChar.char`: 16-bit unsigned integer. The sign of zero must be positive.
* `WideString.string`: 16-bit string (JavaScript's native String).
* `IntInf.int`: BigInt.
* `Int64.int`: BigInt.
* `Word64.word`: BigInt.
* `'a vector`: Array. Must not be modified.
* `'a array`: Array.
* `JavaScript.value`: Any JavaScript value, including `undefined` or `null`.

Non-empty records: `{ "0": <#1 of the record>, foo = <#foo of the record> }`
