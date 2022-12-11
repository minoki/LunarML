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

structure Int32 :> INTEGER = struct
type int = int
val MAX = 0x7fffffff
val MIN = ~0x80000000
val precision = SOME 32
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

(* Assume Int is 64-bit *)
structure Int64 :> INTEGER = Int;
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
