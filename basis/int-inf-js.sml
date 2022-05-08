signature INT_INF = sig
    (* INTEGER *)
    eqtype int
    (* val toLarge, fromLarge *)
    val toInt : int -> Int.int
    val fromInt : Int.int -> int
    val precision : Int.int option (* = NONE *)
    val minInt : int option (* = NONE *)
    val maxInt : int option (* = NONE *)
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
    (*
    val scan : StringCvt.radix -> (char, 'a) StringCvt.reader -> (int, 'a) StringCvt.reader
    val fromString : string -> int option
    *)

    (* INT_INF *)
    val divMod : int * int -> int * int
    val quotRem : int * int -> int * int
    val pow : int * Int.int -> int
    (*
    val log2 : int -> Int.int
    *)
    val orb : int * int -> int
    val xorb : int * int -> int
    val andb : int * int -> int
    val notb : int -> int
    val << : int * Word.word -> int
    val ~>> : int * Word.word -> int
end

structure IntInf : INT_INF = struct
type int = _primType "IntInf.int"
_equality int = fn (x, y) => _primCall "IntInf.=" (x, y);
val maxSmallInt : int = 0x7fffffff
val minSmallInt : int = ~0x80000000
val maxSmallWord : int = 0xffffffff
fun LT (x, y) = _primCall "IntInf.<" (x, y)
fun LE (x, y) = _primCall "IntInf.<=" (x, y)
fun GT (x, y) = _primCall "IntInf.>" (x, y)
fun GE (x, y) = _primCall "IntInf.>=" (x, y)
fun toInt (x : int) : Int.int = if LE (minSmallInt, x) andalso LE (x, maxSmallInt) then
                                    JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.Number #[JavaScript.unsafeToValue x])
                                else
                                    raise Overflow
fun fromInt (x : Int.int) : int = JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.BigInt #[JavaScript.unsafeToValue x])
val precision : Int.int option = NONE
val minInt : int option = NONE
val maxInt : int option = NONE
fun x + y = _primCall "IntInf.+" (x, y)
fun x - y = _primCall "IntInf.-" (x, y)
fun x * y = _primCall "IntInf.*" (x, y)
fun quot (x, y) = if y = 0 then
                      raise Div
                  else
                      _primCall "IntInf.quot.unchecked" (x, y)
fun rem (x, y) = if y = 0 then
                     raise Div
                 else
                     _primCall "IntInf.rem.unchecked" (x, y)
fun x div y = if y = 0 then
                  raise Div
              else if (LE (x, 0) andalso LT (y, 0)) orelse (GE (x, 0) andalso GT (y, 0)) orelse _primCall "IntInf.rem.unchecked" (x, y) = 0 then
                  _primCall "IntInf.quot.unchecked" (x, y)
              else
                  _primCall "IntInf.quot.unchecked" (x, y) - 1
fun x mod y = if y = 0 then
                  raise Div
              else if (LE (x, 0) andalso LT (y, 0)) orelse (GE (x, 0) andalso GT (y, 0)) then
                  _primCall "IntInf.rem.unchecked" (x, y)
              else
                  let val r = _primCall "IntInf.rem.unchecked" (x, y)
                  in if r = 0 then
                         r
                     else
                         y + r
                  end
fun divMod (x, y) = if y = 0 then
                        raise Div
                    else if (LE (x, 0) andalso LT (y, 0)) orelse (GE (x, 0) andalso GT (y, 0)) then
                        (_primCall "IntInf.quot.unchecked" (x, y), _primCall "IntInf.rem.unchecked" (x, y))
                    else
                        let val q = _primCall "IntInf.quot.unchecked" (x, y)
                            val r = _primCall "IntInf.rem.unchecked" (x, y)
                            (* x = q * y + r *)
                        in if r = 0 then
                               (q, r)
                           else
                               (q - 1, y + r)
                        end
fun quotRem (x, y) = if y = 0 then
                         raise Div
                     else
                         (_primCall "IntInf.quot.unchecked" (x, y), _primCall "IntInf.rem.unchecked" (x, y))
fun compare (x, y) = if x = y then
                         EQUAL
                     else if LT (x, y) then
                         LESS
                     else
                         GREATER
fun ~ x = _primCall "IntInf.~" (x)
fun abs x = if GE (x, 0) then
                x
            else
                ~ x
fun min (x, y) = if LT (x, y) then
                     x
                 else
                     y
fun max (x, y) = if LT (x, y) then
                     y
                 else
                     x
fun sign (x : int) : Int.int = if x = 0 then
                                   1
                               else if GT (x, 0) then
                                   1
                               else
                                   ~1
fun sameSign (x, y) = sign x = sign y
fun fmt StringCvt.BIN (x : int) = let val s = if GE (x, 0) then
                                                  JavaScript.unsafeFromValue (JavaScript.method (JavaScript.unsafeToValue x, "toString") #[JavaScript.fromInt 2])
                                              else
                                                  WideString.^ ("~", JavaScript.unsafeFromValue (JavaScript.method (JavaScript.negate (JavaScript.unsafeToValue x), "toString") #[JavaScript.fromInt 2]))
                                  in JavaScript.encodeUtf8 s
                                  end
  | fmt StringCvt.OCT x = let val s = if GE (x, 0) then
                                          JavaScript.unsafeFromValue (JavaScript.method (JavaScript.unsafeToValue x, "toString") #[JavaScript.fromInt 8])
                                      else
                                          WideString.^ ("~", JavaScript.unsafeFromValue (JavaScript.method (JavaScript.negate (JavaScript.unsafeToValue x), "toString") #[JavaScript.fromInt 8]))
                          in JavaScript.encodeUtf8 s
                          end
  | fmt StringCvt.DEC x = let val s = if GE (x, 0) then
                                          JavaScript.unsafeFromValue (JavaScript.method (JavaScript.unsafeToValue x, "toString") #[])
                                      else
                                          WideString.^ ("~", JavaScript.unsafeFromValue (JavaScript.method (JavaScript.negate (JavaScript.unsafeToValue x), "toString") #[]))
                          in JavaScript.encodeUtf8 s
                          end
  | fmt StringCvt.HEX x = let val s = if GE (x, 0) then
                                          JavaScript.unsafeFromValue (JavaScript.method (JavaScript.unsafeToValue x, "toString") #[JavaScript.fromInt 16])
                                      else
                                          WideString.^ ("~", JavaScript.unsafeFromValue (JavaScript.method (JavaScript.negate (JavaScript.unsafeToValue x), "toString") #[JavaScript.fromInt 16]))
                              val s = JavaScript.method (JavaScript.fromWideString s, "toUpperCase") #[]
                          in JavaScript.encodeUtf8 (JavaScript.unsafeFromValue s : WideString.string)
                          end
fun toString (x : int) : string = fmt StringCvt.DEC x
fun pow (x : int, y : Int.int) : int = if y < 0 then
                                           if x = 1 then
                                               x
                                           else if x = ~1 then
                                               if Int.rem (y, 2) = 0 then
                                                   1
                                               else
                                                   x
                                           else if LT (x, ~1) orelse LT (1, x) then
                                               0
                                           else
                                               raise Div
                                       else
                                           let val y = fromInt y
                                           in JavaScript.unsafeFromValue (JavaScript.** (JavaScript.unsafeToValue x, JavaScript.unsafeToValue y))
                                           end
fun orb (x, y) = _primCall "IntInf.orb" (x, y)
fun xorb (x, y) = _primCall "IntInf.xorb" (x, y)
fun andb (x, y) = _primCall "IntInf.andb" (x, y)
fun notb x = _primCall "IntInf.notb" (x)
fun << (x : int, y : Word.word) : int = JavaScript.unsafeFromValue (JavaScript.<< (JavaScript.unsafeToValue x, JavaScript.call JavaScript.Lib.BigInt #[JavaScript.fromWord y]))
fun ~>> (x : int, y : Word.word) : int = JavaScript.unsafeFromValue (JavaScript.>> (JavaScript.unsafeToValue x, JavaScript.call JavaScript.Lib.BigInt #[JavaScript.fromWord y]))
val op < = LT
val op <= = LE
val op > = GT
val op >= = GE
end
_overload "Int" [IntInf.int]
  { + = IntInf.+
  , - = IntInf.-
  , * = IntInf.*
  , div = IntInf.div
  , mod = IntInf.mod
  , abs = IntInf.abs
  , ~ = IntInf.~
  , < = IntInf.<
  , <= = IntInf.<=
  , > = IntInf.>
  , >= = IntInf.>=
  , fromInt = IntInf.fromInt
  }
