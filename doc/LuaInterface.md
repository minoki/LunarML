# Lua interface

## Lua API

Lua features are accessible via `Lua` structure in `$(SML_LIB)/basis/lua.mlb`.

```sml
structure Lua : sig
  type value
  exception Error of value
  exception TypeError of string
  val sub : value * value -> value  (* t[k] *)
  val field : value * string -> value  (* t[k] *)
  val set : value * value * value -> unit  (* t[k] = v *)
  val setField : value * string * value -> unit  (* t[k] = v *)
  val global : string -> value  (* _ENV[name] or _G[name] *)
  val setGlobal : string * value -> unit  (* _ENV[name] = v or _G[name] = v *)
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
  val negate : value -> value  (* unary minus *)
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
  structure Lib : sig
    val GLOBAL : value (* _G *)
    val VERSION : value (* _VERSION *)
    val assert : value
    val collectgarbage : value
    val coroutine : value
    val debug : value
    val dofile : value
    val error : value
    val getfenv : value (* LuaJIT only *)
    val getmetatable : value
    val io : value
    val ipairs : value
    val load : value
    val loadfile : value
    val loadstring : value (* LuaJIT only *)
    val module : value (* LuaJIT only *)
    val math : value
    val next : value
    val os : value
    val package : value
    val pairs : value
    val pcall : value
    val print : value
    val rawequal : value
    val rawget : value
    val rawlen : value (* Lua 5.3/5.4 only *)
    val rawset : value
    val require : value
    val select : value
    val setfenv : value (* LuaJIT only *)
    val setmetatable : value
    val string : value
    val table : value
    val tonumber : value
    val tostring : value
    val type' : value
    val unpack : value (* LuaJIT only *)
    val utf8 : value (* Lua 5.3/5.4 only *)
    val xpcall : value
    structure coroutine : sig
      val create : value
      val isyieldable : value (* Lua 5.3/5.4 only *)
      val resume : value
      val running : value
      val status : value
      val wrap : value
      val yield : value
    end
    structure debug : sig
      val debug : value
      val getfenv : value (* LuaJIT only *)
      val gethook : value
      val getinfo : value
      val getlocal : value
      val getmetatable : value
      val getregistry : value
      val getupvalue : value
      val getuservalue : value (* Lua 5.3/5.4 only *)
      val setfenv : value (* LuaJIT only *)
      val sethook : value
      val setlocal : value
      val setmetatable : value
      val setupvalue : value
      val setuservalue : value (* Lua 5.3/5.4 only *)
      val traceback : value
      val upvalueid : value
      val upvaluejoin : value
    end
    structure io : sig
      val close : value
      val flush : value
      val input : value
      val lines : value
      val open' : value
      val output : value
      val popen : value
      val read : value
      val stderr : value
      val stdin : value
      val stdout : value
      val tmpfile : value
      val type' : value
      val write : value
    end
    structure math : sig
      val abs : value
      val acos : value
      val asin : value
      val atan : value
      val atan2 : value (* LuaJIT only *)
      val ceil : value
      val cos : value
      val cosh : value
      val deg : value
      val exp : value
      val floor : value
      val fmod : value
      val frexp : value (* LuaJIT only *)
      val huge : value
      val ldexp : value (* LuaJIT only *)
      val log : value
      val log10 : value (* LuaJIT only *)
      val max : value
      val maxinteger : value (* Lua 5.3/5.4 only *)
      val min : value
      val mininteger : value (* Lua 5.3/5.4 only *)
      val modf : value
      val pi : value
      val pow : value (* LuaJIT only *)
      val rad : value
      val random : value
      val randomseed : value
      val sin : value
      val sqrt : value
      val tan : value
      val tanh : value (* LuaJIT only *)
      val tointeger : value (* Lua 5.3/5.4 only *)
      val type' : value (* Lua 5.3/5.4 only *)
      val ult : value (* Lua 5.3/5.4 only *)
    end
    structure os : sig
      val clock : value
      val date : value
      val difftime : value
      val execute : value
      val exit : value
      val getenv : value
      val remove : value
      val rename : value
      val setlocale : value
      val time : value
      val tmpname : value
    end
    structure package : sig
      val config : value (* Lua 5.3/5.4 only *)
      val cpath : value
      val loaded : value
      val loaders : value (* LuaJIT only *)
      val loadlib : value
      val path : value
      val preload : value
      val searchers : value (* Lua 5.3/5.4 only *)
      val searchpath : value
      val seeall : value (* LuaJIT only *)
    end
    structure string : sig
      val byte : value
      val char : value
      val dump : value
      val find : value
      val format : value
      val gmatch : value
      val gsub : value
      val len : value
      val lower : value
      val match : value
      val pack : value (* Lua 5.3/5.4 only *)
      val packsize : value (* Lua 5.3/5.4 only *)
      val rep : value
      val reverse : value
      val sub : value
      val unpack : value (* Lua 5.3/5.4 only *)
      val upper : value
    end
    structure table : sig
      val concat : value
      val insert : value
      val maxn : value (* LuaJIT only *)
      val move : value (* Lua 5.3/5.4 only *)
      val pack : value
      val remove : value
      val sort : value
      val unpack : value
    end
    structure utf8 : sig (* Lua 5.3/5.4 only *)
      val char : value
      val charpattern : value
      val codepoint : value
      val codes : value
      val len : value
      val offset : value
    end
    structure bit : sig (* LuaJIT only *)
      val tobit : value
      val tohex : value
      val bnot : value
      val band : value
      val bor : value
      val bxor : value
      val lshift : value
      val rshift : value
      val arshift : value
      val rol : value
      val ror : value
      val bswap : value
    end
    val bit : value (* Lua BitOp; LuaJIT only *)
    val lfs : value option (* LuaFileSystem *)
  end
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

Warning: The internal representation of data types may change in the future. Do not rely on it!

Primitives

* `unit` (empty record): `nil`.
* `bool`: Lua's boolean; `true` or `false`.
* `int`:
    * Lua 5.3 or later: Lua's native integer (typically 64-bit). Overflows are always checked.
    * LuaJIT: 54-bit signed integer, using Lua's native number. Overflows are always checked.
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
* `'a ref`: `{ [1] = <the payload> }`.
* `Lua.value`: any Lua value, including `nil`.

Non-empty records: `{ [1] = <#1 of the record>, foo = <#foo of the record> }`

Datatypes:

Suppose we have:

```sml
datatype 'a option = NONE | SOME of 'a
datatype alias_of_int = ALIAS_OF_INT of int
datatype enum_like = A | B | C
datatype unit_like = UNIT_LIKE
```

* The values for `'a option` will be `{ tag = "NONE" }` or `{ tag = "SOME", payload = x }`.
* The value `ALIAS_OF_INT 42` will be represented by `42`; the constructor is erased.
* The values for `enum_like` will be `"A"`, `"B"` or `"C"`.
* The value `UNIT_LIKE` will be represented by `nil`.


Functions:

SML functions are always converted to one-argument function.

Use `Lua.call` and `Lua.function` to call and define functions that take multiple arguments.
