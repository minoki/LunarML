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
val precision : int option = SOME 54
val minInt : int option = SOME ~0x20000000000000
val maxInt : int option = SOME 0x1fffffffffffff
fun quot (x, y) = _primCall "Int.quot" (x, y)
fun rem (x, y) = if y = 0 then
                     raise Div
                 else
                     _primCall "Int.rem.unchecked" (x, y)
val compare : int * int -> order = fn (x, y) => if x = y then
                                                    EQUAL
                                                else if x < y then
                                                    LESS
                                                else
                                                    GREATER
(* Maybe use Math.min/max? *)
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
fun fmtBIN (x : int) : string =
  let val s = if x >= 0 then
                  JavaScript.unsafeFromValue (JavaScript.method (JavaScript.fromInt x, "toString") #[JavaScript.fromInt 2])
              else
                  String16.^ ("~", JavaScript.unsafeFromValue (JavaScript.method (JavaScript.negate (JavaScript.fromInt x), "toString") #[JavaScript.fromInt 2]))
  in JavaScript.encodeUtf8 s
  end
fun fmtOCT (x : int) : string =
  let val s = if x >= 0 then
                  JavaScript.unsafeFromValue (JavaScript.method (JavaScript.fromInt x, "toString") #[JavaScript.fromInt 8])
              else
                  String16.^ ("~", JavaScript.unsafeFromValue (JavaScript.method (JavaScript.negate (JavaScript.fromInt x), "toString") #[JavaScript.fromInt 8]))
  in JavaScript.encodeUtf8 s
  end
fun toString (x : int) : string =
  let val s = if x >= 0 then
                  JavaScript.unsafeFromValue (JavaScript.method (JavaScript.fromInt x, "toString") #[])
              else
                  String16.^ ("~", JavaScript.unsafeFromValue (JavaScript.method (JavaScript.negate (JavaScript.fromInt x), "toString") #[]))
  in JavaScript.encodeUtf8 s
  end
fun fmtHEX (x : int) : string =
  let val s = if x >= 0 then
                  JavaScript.unsafeFromValue (JavaScript.method (JavaScript.fromInt x, "toString") #[JavaScript.fromInt 16])
              else
                  String16.^ ("~", JavaScript.unsafeFromValue (JavaScript.method (JavaScript.negate (JavaScript.fromInt x), "toString") #[JavaScript.fromInt 16]))
      val s = JavaScript.method (JavaScript.fromString16 s, "toUpperCase") #[]
  in JavaScript.encodeUtf8 (JavaScript.unsafeFromValue s : String16.string)
  end
fun fmt StringCvt.BIN = fmtBIN
  | fmt StringCvt.OCT = fmtOCT
  | fmt StringCvt.DEC = toString
  | fmt StringCvt.HEX = fmtHEX
end; (* structure Int *)
