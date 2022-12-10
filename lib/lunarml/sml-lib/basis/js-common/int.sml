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
                            };

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
                 in if MIN <= x andalso x <= MAX then
                        x
                    else
                        raise Overflow
                 end
fun SUB (x, y) = let val z = x - y
                 in if MIN <= x andalso x <= MAX then
                        x
                    else
                        raise Overflow
                 end
fun MUL (x, y) = let val z = x * y
                 in if MIN <= x andalso x <= MAX then
                        x
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
