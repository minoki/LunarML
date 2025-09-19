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
open Int (* +, -, *, div, mod, ~, abs, <, <=, >, >=, fromInt *)
(* toLarge, fromLarge *)
val toInt : int -> int = fn x => x
val precision : int option = let fun computeWordSize (x : int, n) = if x = 0 then
                                                                        n
                                                                    else
                                                                        computeWordSize (Lua.unsafeFromValue (Lua.>> (Lua.fromInt x, Lua.fromInt 1)), n + 1)
                             in SOME (LunarML.assumeDiscardable computeWordSize (Lua.unsafeFromValue Lua.Lib.math.maxinteger, 1))
                             end
val minInt : int option = SOME (Lua.unsafeFromValue Lua.Lib.math.mininteger)
val maxInt : int option = SOME (Lua.unsafeFromValue Lua.Lib.math.maxinteger)
fun quot (x, y) = if (x >= 0 andalso y >= 0) orelse (x <= 0 andalso y <= 0) then
                      x div y (* raise Overflow if x = minInt and y = ~1, Div if y = 0 *)
                  else
                      if x = Lua.unsafeFromValue Lua.Lib.math.mininteger then
                          if y = 1 then
                              x
                          else
                              ~ (x div (~y)) (* y must be positive, so ~y cannot overflow *)
                      else
                          ~ ((~x) div y)
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
fun fmtBIN (x : int) : string = raise Fail "StringCvt.BIN: not implemented yet"
fun fmtOCT (x : int) : string =
  if x >= 0 then
      let val result = Lua.call1 Lua.Lib.string.format #[Lua.fromString "%o", Lua.fromInt x]
      in Lua.unsafeFromValue result
      end
  else
      let val result = Lua.call1 Lua.Lib.string.format #[Lua.fromString "~%o", Lua.negate (Lua.fromInt x)]
      in Lua.unsafeFromValue result
      end
fun toString (x : int) : string =
  let val result = Lua.call1 Lua.Lib.tostring #[Lua.fromInt x]
      val result = Lua.call1 Lua.Lib.string.gsub #[result, Lua.fromString "-", Lua.fromString "~"]
  in Lua.unsafeFromValue result
  end
fun fmtHEX (x : int) : string =
  if x >= 0 then
      let val result = Lua.call1 Lua.Lib.string.format #[Lua.fromString "%X", Lua.fromInt x]
      in Lua.unsafeFromValue result
      end
  else
      let val result = Lua.call1 Lua.Lib.string.format #[Lua.fromString "~%X", Lua.negate (Lua.fromInt x)]
      in Lua.unsafeFromValue result
      end
fun fmt StringCvt.BIN = fmtBIN
  | fmt StringCvt.OCT = fmtOCT
  | fmt StringCvt.DEC = toString
  | fmt StringCvt.HEX = fmtHEX
(* scan *)
(*
fun fromString (s : string) : int option = let val (r0, r1) = Lua.call2 Lua.Lib.string.match #[Lua.fromString s, Lua.fromString "^%s*([%+~%-]?)([0-9]+)"]
                                           in if Lua.isNil r0 then
                                                  NONE
                                              else
                                                  let val sign = Lua.unsafeFromValue r0 : string
                                                      val digits = Lua.unsafeFromValue r1 : string
                                                      val result' = if sign = "~" orelse sign = "-" then
                                                                        Lua.call1 Lua.Lib.tonumber #[Lua.fromString (String.^ ("-", digits))]
                                                                    else
                                                                        Lua.call1 Lua.Lib.tonumber #[Lua.fromString digits]
                                                  in SOME (Lua.unsafeFromValue result')
                                                  end
                                           end
*)
end; (* structure Int *)
