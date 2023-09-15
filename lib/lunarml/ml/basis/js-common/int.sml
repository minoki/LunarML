structure Int8Impl :> sig include INTEGER; val fromIntUnchecked : Int.int -> int end = struct
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
fun fromIntUnchecked (x : Int.int) = x
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
structure Int8 : INTEGER = Int8Impl;
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
                           , fromInt = Int8Impl.fromIntUnchecked
                           , minInt = ~0x80
                           , maxInt = 0x7f
                           };

structure Int16Impl :> sig include INTEGER; val fromIntUnchecked : Int.int -> int end = struct
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
fun fromIntUnchecked (x : Int.int) = x
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
structure Int16 : INTEGER = Int16Impl;
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
                            , fromInt = Int16Impl.fromIntUnchecked
                            , minInt = ~0x8000
                            , maxInt = 0x7fff
                            };

structure Int32Impl :> sig include INTEGER; val fromIntUnchecked : Int.int -> int end = struct
type int = _Prim.Int32.int
fun Int32_equal (x, y) = _primCall "Int32.=" (x, y)
_equality int = Int32_equal;
val precision = SOME 32
val minInt : int option = SOME ~0x80000000
val maxInt : int option = SOME 0x7fffffff
val toInt : int -> Int.int = fn x => _primCall "Int32.toInt.unchecked" (x)
fun fromInt (x : Int.int) : int = if ~0x80000000 <= x andalso x <= 0x7fffffff then
                                      _primCall "Int.toInt32.unchecked" (x)
                                  else
                                      raise Overflow
fun fromIntUnchecked (x : Int.int) = _primCall "Int.toInt32.unchecked" (x)
fun toLarge x = Int.toLarge (toInt x)
fun fromLarge x = fromInt (Int.fromLarge x)
fun x + y = _primCall "Int32.+" (x, y)
fun x - y = _primCall "Int32.-" (x, y)
fun x * y = _primCall "Int32.*" (x, y)
fun x div y = _primCall "Int32.div" (x, y)
fun x mod y = _primCall "Int32.mod" (x, y)
fun quot (x, y) = _primCall "Int32.quot" (x, y)
fun rem (x, y) = if y = 0 then
                     raise Div
                 else
                     _primCall "Int32.rem.unchecked" (x, y)
fun x < y = _primCall "Int32.<" (x, y)
fun x <= y = _primCall "Int32.<=" (x, y)
fun x > y = _primCall "Int32.>" (x, y)
fun x >= y = _primCall "Int32.>=" (x, y)
fun ~ x = _primCall "Int32.~" (x)
fun abs x = _primCall "Int32.abs" (x)
fun compare (x, y) = if x < y then
                         LESS
                     else if x > y then
                         GREATER
                     else
                         EQUAL
fun min (x, y) = if x < y then
                     x
                 else
                     y
fun max (x, y) = if x > y then
                     x
                 else
                     y
fun sign x = if x < 0 then
                 ~1
             else if x > 0 then
                 1
             else
                 0
fun sameSign (x, y) = sign x = sign y
fun fmt r x = Int.fmt r (toInt x)
fun toString x = Int.toString (toInt x)
fun scan radix getc strm = case Int.scan radix getc strm of
                               SOME (x, strm') => SOME (fromInt x, strm')
                             | NONE => NONE
fun fromString s = Option.map fromInt (Int.fromString s)
end;
structure Int32 : INTEGER = Int32Impl;
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
                            , fromInt = Int32Impl.fromIntUnchecked
                            , minInt = ~0x8000_0000
                            , maxInt = 0x7fff_ffff
                            };

structure Int54 :> INTEGER = Int;
_overload "Int" [Int54.int] { + = Int54.+
                            , - = Int54.-
                            , * = Int54.*
                            , div = Int54.div
                            , mod = Int54.mod
                            , ~ = Int54.~
                            , abs = Int54.abs
                            , < = Int54.<
                            , <= = Int54.<=
                            , > = Int54.>
                            , >= = Int54.>=
                            , fromInt = Int54.fromInt
                            , minInt = ~0x20_0000_0000_0000
                            , maxInt = 0x1f_ffff_ffff_ffff
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

structure Position :> INTEGER = Int;
_overload "Int" [Position.int] { + = Position.+
                               , - = Position.-
                               , * = Position.*
                               , div = Position.div
                               , mod = Position.mod
                               , ~ = Position.~
                               , abs = Position.abs
                               , < = Position.<
                               , <= = Position.<=
                               , > = Position.>
                               , >= = Position.>=
                               , fromInt = Position.fromInt
                               , minInt = ~0x20_0000_0000_0000
                               , maxInt = 0x1f_ffff_ffff_ffff
                               };
