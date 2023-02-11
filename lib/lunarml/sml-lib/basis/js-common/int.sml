structure Int8 :> INTEGER = struct
type int = int
val MAX = 127
val MIN = ~128
val precision = SOME 8
val minInt = SOME MIN
val maxInt = SOME MAX
fun toInt (x : int) = x
fun fromInt (x : Int.int) = if MIN <= x andalso x <= MAX then
                                x
                            else
                                raise Overflow
fun toLarge x = Int.toLarge (toInt x)
fun fromLarge x = fromInt (Int.fromLarge x)
val op + = fn (x, y) => fromInt (x + y)
val op - = fn (x, y) => fromInt (x - y)
val op * = fn (x, y) => fromInt (x * y)
val op div = fn (x, y) => fromInt (x div y)
val op mod = fn (x, y) => fromInt (x mod y)
val quot = fn (x, y) => fromInt (Int.quot (x, y))
val rem = fn (x, y) => fromInt (Int.rem (x, y))
val compare = Int.compare
val op < = Int.<
val op <= = Int.<=
val op > = Int.>
val op >= = Int.>=
val ~ = fn x => fromInt (~ x)
val abs = fn x => fromInt (abs x)
val min = Int.min
val max = Int.max
val sign = Int.sign
val sameSign = Int.sameSign
val fmt = Int.fmt
val toString = Int.toString
fun scan radix getc strm = case Int.scan radix getc strm of
                               SOME (x, strm') => SOME (fromInt x, strm')
                             | NONE => NONE
fun fromString s = Option.map fromInt (Int.fromString s)
end;
_overload "Int" [Int8.int] { + = Int8.+
                           , - = Int8.-
                           , * = Int8.*
                           , div = Int8.div
                           , mod = Int8.mod
                           , ~ = Int8.~
                           , abs = Int8.abs
                           , < = Int8.<
                           , <= = Int8.<=
                           , > = Int8.>
                           , >= = Int8.>=
                           , fromInt = Int8.fromInt
                           , minInt = ~0x80
                           , maxInt = 0x7f
                           };

structure Int16 :> INTEGER = struct
type int = int
val MAX = 0x7fff
val MIN = ~0x8000
val precision = SOME 16
val minInt = SOME MIN
val maxInt = SOME MAX
fun toInt (x : int) = x
fun fromInt (x : Int.int) = if MIN <= x andalso x <= MAX then
                                x
                            else
                                raise Overflow
fun toLarge x = Int.toLarge (toInt x)
fun fromLarge x = fromInt (Int.fromLarge x)
val op + = fn (x, y) => fromInt (x + y)
val op - = fn (x, y) => fromInt (x - y)
val op * = fn (x, y) => fromInt (x * y)
val op div = fn (x, y) => fromInt (x div y)
val op mod = fn (x, y) => fromInt (x mod y)
val quot = fn (x, y) => fromInt (Int.quot (x, y))
val rem = fn (x, y) => fromInt (Int.rem (x, y))
val compare = Int.compare
val op < = Int.<
val op <= = Int.<=
val op > = Int.>
val op >= = Int.>=
val ~ = fn x => fromInt (~ x)
val abs = fn x => fromInt (abs x)
val min = Int.min
val max = Int.max
val sign = Int.sign
val sameSign = Int.sameSign
val fmt = Int.fmt
val toString = Int.toString
fun scan radix getc strm = case Int.scan radix getc strm of
                               SOME (x, strm') => SOME (fromInt x, strm')
                             | NONE => NONE
fun fromString s = Option.map fromInt (Int.fromString s)
end;
_overload "Int" [Int16.int] { + = Int16.+
                            , - = Int16.-
                            , * = Int16.*
                            , div = Int16.div
                            , mod = Int16.mod
                            , ~ = Int16.~
                            , abs = Int16.abs
                            , < = Int16.<
                            , <= = Int16.<=
                            , > = Int16.>
                            , >= = Int16.>=
                            , fromInt = Int16.fromInt
                            , minInt = ~0x8000
                            , maxInt = 0x7fff
                            };

structure Int32 :> INTEGER = Int;
_overload "Int" [Int32.int] { + = Int32.+
                            , - = Int32.-
                            , * = Int32.*
                            , div = Int32.div
                            , mod = Int32.mod
                            , ~ = Int32.~
                            , abs = Int32.abs
                            , < = Int32.<
                            , <= = Int32.<=
                            , > = Int32.>
                            , >= = Int32.>=
                            , fromInt = Int32.fromInt
                            , minInt = ~0x8000_0000
                            , maxInt = 0x7fff_ffff
                            };

local
    val unsafeFromValue : JavaScript.value -> _Prim.Int54.int = JavaScript.unsafeFromValue
    val toValue : _Prim.Int54.int -> JavaScript.value = JavaScript.unsafeToValue
    val fromInt : int -> _Prim.Int54.int = Unsafe.cast
    val unsafeToInt : _Prim.Int54.int -> int = Unsafe.cast
    structure UncheckedInt54 : sig
                  type int
                  val + : int * int -> JavaScript.value
                  val - : int * int -> JavaScript.value
                  val * : int * int -> JavaScript.value
                  val div : int * int -> int (* assume no overflow *)
                  val mod2 : int -> int
                  val quot : int * int -> int (* assume no overflow *)
                  val rem : int * int -> int
                  val ~ : int -> int (* assume no overflow *)
                  val < : int * int -> bool
                  val <= : int * int -> bool
                  val > : int * int -> bool
                  val >= : int * int -> bool
                  end = struct
    type int = _Prim.Int54.int
    val op + = fn (x, y) => JavaScript.+ (toValue x, toValue y)
    val op - = fn (x, y) => JavaScript.- (toValue x, toValue y)
    val op * = fn (x, y) => JavaScript.* (toValue x, toValue y)
    val op div = fn (x, y) => unsafeFromValue (JavaScript.call JavaScript.Lib.Math.floor #[JavaScript./ (toValue x, toValue y)])
    val mod2 = fn x => unsafeFromValue (JavaScript.andb (toValue x, JavaScript.fromInt 1))
    val quot = fn (x, y) => unsafeFromValue (JavaScript.call JavaScript.Lib.Math.trunc #[JavaScript./ (toValue x, toValue y)])
    val rem = fn (x, y) => unsafeFromValue (JavaScript.% (toValue x, toValue y))
    val ~ = fn x => unsafeFromValue (JavaScript.negate (toValue x))
    val op < = fn (x, y) => JavaScript.< (toValue x, toValue y)
    val op <= = fn (x, y) => JavaScript.<= (toValue x, toValue y)
    val op > = fn (x, y) => JavaScript.> (toValue x, toValue y)
    val op >= = fn (x, y) => JavaScript.>= (toValue x, toValue y)
    end
    fun Int54_EQUAL (x, y) = JavaScript.=== (toValue x, toValue y);
in
_equality _Prim.Int54.int = Int54_EQUAL;
structure Int54 :> INTEGER where type int = _Prim.Int54.int = struct
type int = _Prim.Int54.int
val MIN_INT32_AS_INT54 : int = ~0x8000_0000
val MAX_INT32_AS_INT54 : int = 0x7FFF_FFFF
val MIN : int = ~0x20_0000_0000_0000 (* -2^53 *)
val MAX : int = 0x1F_FFFF_FFFF_FFFF (* 2^53-1 *)
val precision = SOME 54
val minInt = SOME MIN
val maxInt = SOME MAX
fun ADD (x, y) = let val z = UncheckedInt54.+ (x, y)
                 in if (JavaScript.< (toValue MIN, z) andalso JavaScript.<= (z, toValue MAX)) orelse (JavaScript.=== (z, toValue MIN) andalso UncheckedInt54.mod2 x = UncheckedInt54.mod2 y) then
                        unsafeFromValue z
                    else
                        raise Overflow
                 end
fun SUB (x, y) = let val z = UncheckedInt54.- (x, y)
                 in if (JavaScript.< (toValue MIN, z) andalso JavaScript.<= (z, toValue MAX)) orelse (JavaScript.=== (z, toValue MIN) andalso UncheckedInt54.mod2 x = UncheckedInt54.mod2 y) then
                        unsafeFromValue z
                    else
                        raise Overflow
                 end
fun MUL (x, y) = let val z = UncheckedInt54.* (x, y)
                 in if (JavaScript.< (toValue MIN, z) andalso JavaScript.<= (z, toValue MAX)) orelse (JavaScript.=== (z, toValue MIN) andalso (UncheckedInt54.mod2 x = 0 orelse UncheckedInt54.mod2 y = 0)) then
                        unsafeFromValue z
                    else
                        raise Overflow
                 end
fun NEGATE x = if x = MIN then
                   raise Overflow
               else
                   UncheckedInt54.~ x
fun ABS x = if x = MIN then
                raise Overflow
            else if UncheckedInt54.< (x, 0) then
                UncheckedInt54.~ x
            else
                x
fun DIV (x, y) = if y = 0 then
                     raise Div
                 else if x = MIN andalso y = ~1 then
                     raise Overflow
                 else
                     UncheckedInt54.div (x, y)
fun MOD (x, y) = if y = 0 then
                     raise Div
                 else
                     let val r = UncheckedInt54.rem (x, y)
                     in if r = 0 orelse JavaScript.>= (UncheckedInt54.* (x, y), JavaScript.fromInt 0) then
                            r
                        else
                            unsafeFromValue (UncheckedInt54.+ (r, y))
                     end
fun quot (x, y) = if y = 0 then
                      raise Div
                  else if x = MIN andalso y = ~1 then
                      raise Overflow
                  else
                      UncheckedInt54.quot (x, y)
fun rem (x, y) = if y = 0 then
                     raise Div
                 else if y = ~1 then
                     0
                 else
                     UncheckedInt54.rem (x, y)
val toLarge = IntInfImpl.fromInt54
fun fromLarge x = if ~0x20_0000_0000_0000 <= x andalso x <= 0x1f_ffff_ffff_ffff then
                      IntInfImpl.unsafeToInt54 x
                  else
                      raise Overflow
fun toInt x = if UncheckedInt54.<= (MIN_INT32_AS_INT54, x) andalso UncheckedInt54.<= (x, MAX_INT32_AS_INT54) then
                  unsafeToInt x
              else
                  raise Overflow
val fromInt = fromInt
fun compare (x, y) = if UncheckedInt54.< (x, y) then
                         LESS
                     else if UncheckedInt54.> (x, y) then
                         GREATER
                     else
                         EQUAL
fun min (x, y) = if UncheckedInt54.< (x, y) then
                     x
                 else
                     y
fun max (x, y) = if UncheckedInt54.> (x, y) then
                     x
                 else
                     y
fun sign x : Int.int = if UncheckedInt54.< (x, 0) then
                           ~1
                       else if UncheckedInt54.> (x, 0) then
                           1
                       else
                           0
fun sameSign (x, y) = sign x = sign y
(* JavaScript's toString prints -0.0 as "0" *)
fun toBinString x = let val s = if UncheckedInt54.>= (x, 0) then
                                    JavaScript.unsafeFromValue (JavaScript.method (toValue x, "toString") #[JavaScript.fromInt 2])
                                else
                                    WideString.^ ("~", JavaScript.unsafeFromValue (JavaScript.method (JavaScript.negate (toValue x), "toString") #[JavaScript.fromInt 2]))
                    in JavaScript.encodeUtf8 s
                    end
fun toOctString x = let val s = if UncheckedInt54.>= (x, 0) then
                                    JavaScript.unsafeFromValue (JavaScript.method (toValue x, "toString") #[JavaScript.fromInt 8])
                                else
                                    WideString.^ ("~", JavaScript.unsafeFromValue (JavaScript.method (JavaScript.negate (toValue x), "toString") #[JavaScript.fromInt 8]))
                    in JavaScript.encodeUtf8 s
                    end
fun toString x = let val s = if UncheckedInt54.>= (x, 0) then
                                 JavaScript.unsafeFromValue (JavaScript.method (toValue x, "toString") #[])
                             else
                                 WideString.^ ("~", JavaScript.unsafeFromValue (JavaScript.method (JavaScript.negate (toValue x), "toString") #[]))
                 in JavaScript.encodeUtf8 s
                 end
fun toHexString x = let val s = if UncheckedInt54.>= (x, 0) then
                                    JavaScript.unsafeFromValue (JavaScript.method (toValue x, "toString") #[JavaScript.fromInt 16])
                                else
                                    WideString.^ ("~", JavaScript.unsafeFromValue (JavaScript.method (JavaScript.negate (toValue x), "toString") #[JavaScript.fromInt 16]))
                        val s = JavaScript.method (JavaScript.fromWideString s, "toUpperCase") #[]
                    in JavaScript.encodeUtf8 (JavaScript.unsafeFromValue s : WideString.string)
                    end
fun fmt StringCvt.BIN = toBinString
  | fmt StringCvt.OCT = toOctString
  | fmt StringCvt.DEC = toString
  | fmt StringCvt.HEX = toHexString
local
    open ScanNumUtils
    fun scanDigits (radix, isDigit, getc)
        = let fun go1 (x, strm) = case getc strm of
                                      SOME (c, strm') => if isDigit c then
                                                             go1 (ADD (MUL (fromInt radix, x), fromInt (digitToInt c)), strm')
                                                         else
                                                             SOME (x, strm)
                                    | NONE => SOME (x, strm)
          in fn strm => case getc strm of
                            SOME (c, strm') => if isDigit c then
                                                   go1 (fromInt (digitToInt c), strm')
                                               else
                                                   NONE
                          | NONE => NONE
          end
    fun scanNegativeDigits (radix, isDigit, getc)
        = let fun go1 (x, strm) = case getc strm of
                                      SOME (c, strm') => if isDigit c then
                                                             go1 (SUB (MUL (fromInt radix, x), fromInt (digitToInt c)), strm')
                                                         else
                                                             SOME (x, strm)
                                    | NONE => SOME (x, strm)
          in fn strm => case getc strm of
                            SOME (c, strm') => if isDigit c then
                                                   go1 (fromInt (~ (digitToInt c)), strm')
                                               else
                                                   NONE
                          | NONE => NONE
          end
in
fun scan StringCvt.BIN getc strm = let val strm = skipInitialWhitespace (getc, strm)
                                       val (isNegative, strm) = scanSign (getc, strm)
                                   in if isNegative then
                                          scanNegativeDigits (2, isBinDigit, getc) strm
                                      else
                                          scanDigits (2, isBinDigit, getc) strm
                                   end
  | scan StringCvt.OCT getc strm = let val strm = skipInitialWhitespace (getc, strm)
                                       val (isNegative, strm) = scanSign (getc, strm)
                                   in if isNegative then
                                          scanNegativeDigits (8, isOctDigit, getc) strm
                                      else
                                          scanDigits (8, isOctDigit, getc) strm
                                   end
  | scan StringCvt.DEC getc strm = let val strm = skipInitialWhitespace (getc, strm)
                                       val (isNegative, strm) = scanSign (getc, strm)
                                   in if isNegative then
                                          scanNegativeDigits (10, Char.isDigit, getc) strm
                                      else
                                          scanDigits (10, Char.isDigit, getc) strm
                                   end
  | scan StringCvt.HEX getc strm = let val strm = skipInitialWhitespace (getc, strm)
                                       val (isNegative, strm) = scanSign (getc, strm)
                                       val strm = case getc strm of
                                                      SOME (#"0", strm') =>
                                                      (case getc strm' of
                                                           SOME (c, strm'') =>
                                                           if c = #"x" orelse c = #"X" then
                                                               case getc strm'' of
                                                                   SOME (c, _) => if Char.isHexDigit c then
                                                                                      strm''
                                                                                  else
                                                                                      strm
                                                                 | NONE => strm
                                                           else
                                                               strm
                                                         | NONE => strm
                                                      )
                                                    | _ => strm
                                   in if isNegative then
                                          scanNegativeDigits (16, Char.isHexDigit, getc) strm
                                      else
                                          scanDigits (16, Char.isHexDigit, getc) strm
                                   end
fun fromString s = StringCvt.scanString (scan StringCvt.DEC) s
end
val op + = ADD
val op - = SUB
val op * = MUL
val op div = DIV
val op mod = MOD
val ~ = NEGATE
val abs = ABS
val op < = UncheckedInt54.<
val op <= = UncheckedInt54.<=
val op > = UncheckedInt54.>
val op >= = UncheckedInt54.>=
end
end;

structure Int64 :> INTEGER = struct
type int = IntInf.int
val MIN : int = ~0x8000_0000_0000_0000
val MAX : int = 0x7fff_ffff_ffff_ffff
fun toLarge x = x
fun fromLarge x = if MIN <= x andalso x <= MAX then
                      x
                  else
                      raise Overflow
val toInt = IntInf.toInt
val fromInt = IntInf.fromInt
val precision = SOME 64
val minInt = SOME MIN
val maxInt = SOME MAX
fun ADD (x, y) = let val z = x + y
                 in if MIN <= z andalso z <= MAX then
                        z
                    else
                        raise Overflow
                 end
fun SUB (x, y) = let val z = x - y
                 in if MIN <= z andalso z <= MAX then
                        z
                    else
                        raise Overflow
                 end
fun MUL (x, y) = let val z = x * y
                 in if MIN <= z andalso z <= MAX then
                        z
                    else
                        raise Overflow
                 end
fun DIV (x, y) = if x = MIN andalso y = ~1 then
                     raise Overflow
                 else
                     x div y
fun quot (x, y) = if x = MIN andalso y = ~1 then
                      raise Overflow
                  else
                      IntInf.quot (x, y)
val rem = IntInf.rem
val compare = IntInf.compare
fun NEGATE x = if x = MIN then
                   raise Overflow
               else
                   ~x
fun ABS x = if x = MIN then
                raise Overflow
            else
                abs x
val min = IntInf.min
val max = IntInf.max
val sign = IntInf.sign
val sameSign = IntInf.sameSign
val fmt = IntInf.fmt
val toString = IntInf.toString
fun scan radix getc strm = case IntInf.scan radix getc strm of
                               SOME (x, strm') => SOME (fromLarge x, strm')
                             | NONE => NONE
fun fromString s = Option.map fromLarge (IntInf.fromString s)
val op + = ADD
val op - = SUB
val op * = MUL
val op div = DIV
val op mod = IntInf.mod
val ~ = NEGATE
val abs = ABS
val op < = IntInf.<
val op <= = IntInf.<=
val op > = IntInf.>
val op >= = IntInf.>=
end;
_overload "Int" [Int64.int] { + = Int64.+
                            , - = Int64.-
                            , * = Int64.*
                            , div = Int64.div
                            , mod = Int64.mod
                            , ~ = Int64.~
                            , abs = Int64.abs
                            , < = Int64.<
                            , <= = Int64.<=
                            , > = Int64.>
                            , >= = Int64.>=
                            , fromInt = Int64.fromInt
                            , minInt = ~0x8000_0000_0000_0000
                            , maxInt = 0x7fff_ffff_ffff_ffff
                            };
