# Lua interface

## Lua API

Lua features are accessible via `Lua` structure.

```sml
structure Lua : sig
  type value
  val sub : value * value -> value  (* t[k] *)
  val field : value * string -> value  (* t[k] *)
  val set : value * value * value -> unit  (* t[k] = v *)
  val global : string -> value  (* _ENV[name] *)
  val call : value -> value vector -> value vector  (* f(args...) *)
  val call0 : value -> value vector -> unit  (* f(args...) *)
  val call1 : value -> value vector -> value  (* f(args...) *)
  val call2 : value -> value vector -> value * value  (* f(args...) *)
  val call3 : value -> value vector -> value * value * value  (* f(args...) *)
  val method : value * string -> value vector -> value vector  (* f:name(args...) *)
  val NIL : value  (* Lua nil *)
  val isNil : value -> bool  (* x == nil *)
  val isFalsy : value -> bool  (* not x *)
  val fromBool : bool -> value
  val fromInt : int -> value
  val fromInt54 : Int54.int -> value
  val fromInt64 : Int64.int -> value
  val fromWord : word -> value
  val fromReal : real -> value
  val fromString : string -> value
  val unsafeToValue : 'a -> value
  val unsafeFromValue : value -> 'a
  val newTable : unit -> value  (* {} *)
  val function : (value vector -> value vector) -> value
  val + : value * value -> value
  val - : value * value -> value
  val * : value * value -> value
  val / : value * value -> value
  val // : value * value -> value
  val % : value * value -> value
  val pow : value * value -> value  (* x ^ y *)
  val unm : value -> value  (* unary minus *)
  val andb : value * value -> value  (* x & y *)
  val orb : value * value -> value  (* x | y *)
  val xorb : value * value -> value  (* x ~ y *)
  val notb : value -> value  (* ~ x *)
  val << : value * value -> value
  val >> : value * value -> value
  val == : value * value -> bool
  val ~= : value * value -> bool
  val < : value * value -> bool
  val > : value * value -> bool
  val <= : value * value -> bool
  val >= : value * value -> bool
  val concat : value * value -> value  (* x .. y *)
  val length : value -> value  (* #x *)
  val typeof : value -> string (* type *)
  val checkString : value -> string
  val checkBoolean : value -> bool
  val checkInt : value -> int
  val checkWord : value -> word
  val checkReal : value -> real
  val optString : value -> string option
end
```

## Exporting a value

To produce a Lua module, set `--lib` compiler option and define a variable or structure named `export`.

For example,

```sml
val export = <some value>;
```

will compile to

```lua
return <some value>
```

and

```sml
structure export = struct
  val foo = "string"
  val bar = 42
  val fun' = "fun" (* SML keywords can be escaped by suffixing with a prime *)
end;
```

will compile to

```lua
return {
  foo = "string",
  bar = 42,
  fun = "fun",
}
```
.

## Internal representation

Primitives

* `unit` (empty record): `nil`.
* `bool`: Lua's boolean; `true` or `false`.
* `int`:
    * Lua 5.3 or later: Lua's native integer (typically 64-bit). Overflows are always checked.
    * LuaJIT: 32-bit signed integer, using Lua's native number. Overflows are always checked.
* `word`:
    * Lua 5.3 or later: Lua's native integer (typically 64-bit), using negative values for large values.
    * LuaJIT: 32-bit unsigned integer, using Lua's native number.
* `real`: Lua's native number, typically 64-bit.
* `char`: 8-bit unsigned integer.
* `string`: Lua's native string.
* `Int54.int`:
    * Lua 5.3 or later: Lua's native integer, assuming the width is 64-bit.
    * LuaJIT: Lua's native number.
* `Int64.int`:
    * Lua 5.3 or later: Lua's native integer, assuming the width is 64-bit.
    * LuaJIT: Boxed 64-bit signed integer: `int64_t`
* `Word64.word`:
    * Lua 5.3 or later: Lua's native integer, assuming the width is 64-bit.
    * LuaJIT: Boxed 64-bit unsigned integer: `uint64_t`.
* `'a list`: `nil` for `nil` and `{[1] = <head>, [2] = <tail>}` for `::`.
* `'a vector`: `{ n = <length>, [1] = <0th element>, [2] = <1st element>, ... }`; compatible with `table.pack`.
* `'a array`: Same as `'a vector`, but mutable.
* `'a ref`: `{ tag = "ref", payload = <the payload> }`.
* `Lua.value`: any Lua value, including `nil`.

Non-empty records: `{ [1] = <#1 of the record>, foo = <#foo of the record> }`

Datatypes:

For datatypes like

```sml
datatype 'a option = NONE | SOME of 'a
datatype 'a list = nil | :: of 'a * 'a list
```

the values for `'a option` will be `{ tag = "NONE" }` or `{ tag = "SOME", payload = x }`.

Functions:

SML functions are always converted to one-argument function.

Use `Lua.call` and `Lua.function` to call and define functions that take multiple arguments.

Structures:

```sml
struct
  val x = 123
  structure Sub = struct ... end
  exception Foo
  datatype t = T
end
```

```lua
{
  x = 123,
  _Sub = { ... }, -- prefixed with '_'
  Foo = <constructor of Foo>,
  ["Foo.tag"] = <tag of Foo>, -- suffixed with '.tag'
  T = <constructor of T>,
}
```

Functors:

Implemented as a function that takes a structure and returns a structure.
