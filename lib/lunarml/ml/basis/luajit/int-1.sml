signature INTEGER = sig
    eqtype int
    (* val toLarge : int -> LargeInt.int *)
    (* val fromLarge : LargeInt.int -> int *)
    val toInt : int -> Int.int
    val fromInt : Int.int -> int
    val precision : Int.int option
    val minInt : int option
    val maxInt : int option
    val + : int * int -> int
    val - : int * int -> int
    val * : int * int -> int
    val div : int * int -> int
    val mod : int * int -> int
    val quot : int * int -> int
    val rem : int * int -> int
    val compare : int * int -> order
    val < : int * int -> bool
    val <= : int * int -> bool
    val > : int * int -> bool
    val >= : int * int -> bool
    val ~ : int -> int
    val abs : int -> int
    val min : int * int -> int
    val max : int * int -> int
    val sign : int -> Int.int
    val sameSign : int * int -> bool
    val fmt : StringCvt.radix -> int -> string
    val toString : int -> string
    (* val scan : StringCvt.radix -> (char, 'a) StringCvt.reader -> (int, 'a) StringCvt.reader; defined in scan-num.sml *)
    (* val fromString : string -> int option; defined in scan-num.sml *)
end;

structure Int : INTEGER where type int = int = struct
open Int (* +, -, *, div, ~, abs, <, <=, >, >=, fromInt *)
(* toLarge, fromLarge *)
val toInt : int -> int = fn x => x
val precision : int option = SOME 54
val minInt : int option = SOME ~0x20_0000_0000_0000 (* -2^53 *)
val maxInt : int option = SOME 0x1F_FFFF_FFFF_FFFF (* 2^53-1 *)
fun quot (x, y) = _primCall "Int.quot" (x, y)
fun rem (x, y) = if y = ~1 then
                     0
                 else
                     x - quot (x, y) * y (* raise Div if y = 0 *)
val compare : int * int -> order = fn (x, y) => if x = y then
                                                    EQUAL
                                                else if x < y then
                                                    LESS
                                                else
                                                    GREATER
val min : int * int -> int = fn (x, y) => if x < y then
                                              x
                                          else
                                              y
val max : int * int -> int = fn (x, y) => if x < y then
                                              y
                                          else
                                              x
val sign : int -> int = fn x => if x > 0 then
                                    1
                                else if x < 0 then
                                    ~1
                                else
                                    0
val sameSign : int * int -> bool = fn (x, y) => sign x = sign y
fun toOctString x : string = if x >= 0 then
                                 let val result = Lua.call1 Lua.Lib.string.format #[Lua.fromString "%o", Lua.fromInt x]
                                 in Lua.unsafeFromValue result
                                 end
                             else
                                 let val result = Lua.call1 Lua.Lib.string.format #[Lua.fromString "~%o", Lua.negate (Lua.fromInt x)]
                                 in Lua.unsafeFromValue result
                                 end
fun toString x : string = let val result = Lua.call1 Lua.Lib.string.format #[Lua.fromString "%d", Lua.fromInt x]
                              val result = Lua.call1 Lua.Lib.string.gsub #[result, Lua.fromString "-", Lua.fromString "~"]
                          in Lua.unsafeFromValue result
                          end
fun toHexString x : string = if x >= 0 then
                                 let val result = Lua.call1 Lua.Lib.string.format #[Lua.fromString "%X", Lua.fromInt x]
                                 in Lua.unsafeFromValue result
                                 end
                             else
                                 let val result = Lua.call1 Lua.Lib.string.format #[Lua.fromString "~%X", Lua.negate (Lua.fromInt x)]
                                 in Lua.unsafeFromValue result
                                 end
fun fmt StringCvt.BIN = (fn _ => raise Fail "StringCvt.BIN: not implemented yet")
  | fmt StringCvt.OCT = toOctString
  | fmt StringCvt.DEC = toString
  | fmt StringCvt.HEX = toHexString
end; (* structure Int *)
