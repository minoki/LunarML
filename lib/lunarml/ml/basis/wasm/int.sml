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
(*
fun toLarge x = Int.toLarge (toInt x)
fun fromLarge x = fromInt (Int.fromLarge x)
*)
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
(*
fun scan radix getc strm = case Int.scan radix getc strm of
                               SOME (x, strm') => SOME (fromInt x, strm')
                             | NONE => NONE
fun fromString s = Option.map fromInt (Int.fromString s)
*)
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
(*
fun toLarge x = Int.toLarge (toInt x)
fun fromLarge x = fromInt (Int.fromLarge x)
*)
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
(*
fun scan radix getc strm = case Int.scan radix getc strm of
                               SOME (x, strm') => SOME (fromInt x, strm')
                             | NONE => NONE
fun fromString s = Option.map fromInt (Int.fromString s)
*)
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

_equality _Prim.Int64.int = fn (x, y) => _primCall "Int64.=" (x, y);
structure Int64 :> INTEGER where type int = _Prim.Int64.int = struct
type int = _Prim.Int64.int
val precision : Int.int option = SOME 64
val MIN : int = ~0x8000_0000_0000_0000
val minInt : int option = SOME MIN
val maxInt : int option = SOME 0x7fff_ffff_ffff_ffff
fun x < y = _primCall "Int64.<" (x, y)
fun x <= y = _primCall "Int64.<=" (x, y)
fun x > y = _primCall "Int64.>" (x, y)
fun x >= y = _primCall "Int64.>=" (x, y)
fun ~ (x : int) =
  if x = MIN then
    raise Overflow
  else
    _primCall "Int64.~.wrapping" (x)
fun abs (x : int) =
  if x >= 0 then
    x
  else
    ~x
fun x + y =
  let val z = _primCall "Int64.+.wrapping" (x, y)
  in
    if (y > 0 andalso z < x) orelse (y < 0 andalso z > x) then
      raise Overflow
    else
      z
  end
fun x - y =
  let val z = _primCall "Int64.-.wrapping" (x, y)
  in
    if (y < 0 andalso z < x) orelse (y > 0 andalso z > x) then
      raise Overflow
    else
      z
  end
fun x * y =
  let val z = _primCall "Int64.*.wrapping" (x, y)
  in
    if (x = MIN andalso y = ~1) orelse (y = MIN andalso x = ~1) orelse (x <> 0 andalso _primCall "Int64.quot.unchecked" (z, x) <> y) orelse (y <> 0 andalso _primCall "Int64.quot.unchecked" (z, y) <> x) then
      raise Overflow
    else
      z
  end
fun quot (x, y) =
  if y = 0 then
    raise Div
  else if x = MIN andalso y = ~1 then
    raise Overflow
  else
    _primCall "Int64.quot.unchecked" (x, y)
fun rem (x, y) =
  if y = 0 then
    raise Div
  else
    _primCall "Int64.rem.unchecked" (x, y)
fun x div y =
  if y = 0 then
    raise Div
  else if x = MIN andalso y = ~1 then
    raise Overflow
  else
    let val r = _primCall "Int64.rem.unchecked" (x, y)
    in if (x >= 0 andalso y > 0) orelse (x <= 0 andalso y < 0) orelse r = 0 then
         _primCall "Int64.quot.unchecked" (x, y)
       else
         _primCall "Int64.-.wrapping" (_primCall "Int64.quot.unchecked" (x, y), 1)
    end
fun x mod y =
  if y = 0 then
    raise Div
  else
    let val r = _primCall "Int64.rem.unchecked" (x, y)
    in if (x >= 0 andalso y > 0) orelse (x <= 0 andalso y < 0) orelse r = 0 then
         r
       else
         _primCall "Int64.+.wrapping" (r, y)
    end
fun fromInt (x : Int.int) = _primCall "Int.toInt64.unchecked" (x)
val toInt : int -> Int.int =
  fn x =>
    if ~0x8000_0000 <= x andalso x <= 0x7fff_ffff then
      _primCall "Int64.toInt.unchecked" (x)
    else
      raise Overflow
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
val sign : int -> Int.int = fn x => if x > 0 then
                                    1
                                else if x < 0 then
                                    ~1
                                else
                                    0
val sameSign : int * int -> bool = fn (x, y) => sign x = sign y
local
  infix 6 +! -!
  fun x +! y = _primCall "Int.+.wrapping" (x, y)
  fun x -! y = _primCall "Int.-.wrapping" (x, y)
  fun ~! x = _primCall "Int64.~.wrapping" (x)
  fun quot' (x, y) = _primCall "Int64.quot.unchecked" (x, y)
  fun rem' (x, y) = _primCall "Int64.rem.unchecked" (x, y)
  fun intToDigit i =
    _primCall "Char.chr.unchecked" (_primCall "Char.ord" (#"0") +! _primCall "Int64.toInt.unchecked" (i))
  fun intToHexDigit i =
    if i < 10 then
      _primCall "Char.chr.unchecked" (_primCall "Char.ord" (#"0") +! _primCall "Int64.toInt.unchecked" (i))
    else
      _primCall "Char.chr.unchecked" (_primCall "Char.ord" (#"A") -! 10 +! _primCall "Int64.toInt.unchecked" (i))
  fun fmtBIN (0 : int) = "0"
    | fmtBIN x =
        let
          val initialBufSize = 65
          val radix = 2
          val buf = _primCall "CharArray.alloc" (initialBufSize)
          fun goPositive (i, 0) = i +! 1
            | goPositive (i, x) =
                let val r = rem' (x, radix)
                in _primCall "Unsafe.CharArray.update" (buf, i, intToDigit r)
                 ; goPositive (i -! 1, quot' (x, radix))
                end
          fun goNegative (i, 0) =
                ( _primCall "Unsafe.CharArray.update" (buf, i, #"~")
                ; i
                )
            | goNegative (i, x) =
                let val r = ~! (rem' (x, radix))
                in _primCall "Unsafe.CharArray.update" (buf, i, intToDigit r)
                 ; goNegative (i -! 1, quot' (x, radix))
                end
          val i = if x < 0 then
                    goNegative (initialBufSize -! 1, x)
                  else
                    goPositive (initialBufSize -! 1, x)
          val n = initialBufSize -! i
          val buf2 = _primCall "CharArray.alloc" (n)
        in
          _primCall "CharArray.copy" (buf2, 0, buf, i, n)
        ; _primCall "CharArray.unsafeFreeze" (buf2)
        end
  fun fmtOCT (0 : int) = "0"
    | fmtOCT x =
        let
          (* ~400000000000000000000 *)
          val initialBufSize = 22
          val radix = 8
          val buf = _primCall "CharArray.alloc" (initialBufSize)
          fun goPositive (i, 0) = i +! 1
            | goPositive (i, x) =
                let val r = rem' (x, radix)
                in _primCall "Unsafe.CharArray.update" (buf, i, intToDigit r)
                 ; goPositive (i -! 1, quot' (x, radix))
                end
          fun goNegative (i, 0) =
                ( _primCall "Unsafe.CharArray.update" (buf, i, #"~")
                ; i
                )
            | goNegative (i, x) =
                let val r = ~! (rem' (x, radix))
                in _primCall "Unsafe.CharArray.update" (buf, i, intToDigit r)
                 ; goNegative (i -! 1, quot' (x, radix))
                end
          val i = if x < 0 then
                    goNegative (initialBufSize -! 1, x)
                  else
                    goPositive (initialBufSize -! 1, x)
          val n = initialBufSize -! i
          val buf2 = _primCall "CharArray.alloc" (n)
        in
          _primCall "CharArray.copy" (buf2, 0, buf, i, n)
        ; _primCall "CharArray.unsafeFreeze" (buf2)
        end
  fun fmtDEC (0 : int) = "0"
    | fmtDEC x =
        let
          (* ~9223372036854775808 *)
          val initialBufSize = 20
          val radix = 10
          val buf = _primCall "CharArray.alloc" (initialBufSize)
          fun goPositive (i, 0) = i +! 1
            | goPositive (i, x) =
                let val r = rem' (x, radix)
                in _primCall "Unsafe.CharArray.update" (buf, i, intToDigit r)
                 ; goPositive (i -! 1, quot' (x, radix))
                end
          fun goNegative (i, 0) =
                ( _primCall "Unsafe.CharArray.update" (buf, i, #"~")
                ; i
                )
            | goNegative (i, x) =
                let val r = ~! (rem' (x, radix))
                in _primCall "Unsafe.CharArray.update" (buf, i, intToDigit r)
                 ; goNegative (i -! 1, quot' (x, radix))
                end
          val i = if x < 0 then
                    goNegative (initialBufSize -! 1, x)
                  else
                    goPositive (initialBufSize -! 1, x)
          val n = initialBufSize -! i
          val buf2 = _primCall "CharArray.alloc" (n)
        in
          _primCall "CharArray.copy" (buf2, 0, buf, i, n)
        ; _primCall "CharArray.unsafeFreeze" (buf2)
        end
  fun fmtHEX (0 : int) = "0"
    | fmtHEX x =
        let
          (* ~8000000000000000 *)
          val initialBufSize = 17
          val radix = 16
          val buf = _primCall "CharArray.alloc" (initialBufSize)
          fun goPositive (i, 0) = i +! 1
            | goPositive (i, x) =
                let val r = rem' (x, radix)
                in _primCall "Unsafe.CharArray.update" (buf, i, intToHexDigit r)
                 ; goPositive (i -! 1, quot' (x, radix))
                end
          fun goNegative (i, 0) =
                ( _primCall "Unsafe.CharArray.update" (buf, i, #"~")
                ; i
                )
            | goNegative (i, x) =
                let val r = ~! (rem' (x, radix))
                in _primCall "Unsafe.CharArray.update" (buf, i, intToHexDigit r)
                 ; goNegative (i -! 1, quot' (x, radix))
                end
          val i = if x < 0 then
                    goNegative (initialBufSize -! 1, x)
                  else
                    goPositive (initialBufSize -! 1, x)
          val n = initialBufSize -! i
          val buf2 = _primCall "CharArray.alloc" (n)
        in
          _primCall "CharArray.copy" (buf2, 0, buf, i, n)
        ; _primCall "CharArray.unsafeFreeze" (buf2)
        end
in
  val toString = fmtDEC
  fun fmt StringCvt.BIN = fmtBIN
    | fmt StringCvt.OCT = fmtOCT
    | fmt StringCvt.DEC = fmtDEC
    | fmt StringCvt.HEX = fmtHEX
end
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

structure Int54Impl :> sig
  include INTEGER
  val toInt64 : int -> Int64.int
  val fromInt64 : Int64.int -> int
  val fromInt64Unchecked : Int64.int -> int
end = struct
type int = Int64.int
val MAX : int = 0x1f_ffff_ffff_ffff
val MIN : int = ~0x20_0000_0000_0000
val precision = SOME 54
val minInt = SOME MIN
val maxInt = SOME MAX
val toInt = Int64.toInt
fun toInt64 (x : int) = x
val fromInt = Int64.fromInt
fun fromInt64 (x : Int64.int) = if MIN <= x andalso x <= MAX then
                                    x
                                else
                                    raise Overflow
fun fromInt64Unchecked (x : Int64.int) = x
(*
fun toLarge x = Int.toLarge (toInt x)
fun fromLarge x = fromInt (Int.fromLarge x)
*)
val op + = fn (x, y) => fromInt64 (x + y)
val op - = fn (x, y) => fromInt64 (x - y)
val op * = fn (x, y) => fromInt64 (x * y)
val op div = fn (x, y) => fromInt64 (x div y)
val op mod = fn (x, y) => fromInt64 (x mod y)
val quot = fn (x, y) => fromInt64 (Int64.quot (x, y))
val rem = fn (x, y) => fromInt64 (Int64.rem (x, y))
val compare = Int64.compare
val op < = Int64.<
val op <= = Int64.<=
val op > = Int64.>
val op >= = Int64.>=
val ~ = fn x => fromInt64 (~ x)
val abs = fn x => fromInt64 (abs x)
val min = Int64.min
val max = Int64.max
val sign = Int64.sign
val sameSign = Int64.sameSign
val fmt = Int64.fmt
val toString = Int64.toString
(*
fun scan radix getc strm = case Int64.scan radix getc strm of
                               SOME (x, strm') => SOME (fromInt x, strm')
                             | NONE => NONE
fun fromString s = Option.map fromInt64 (Int64.fromString s)
*)
end;
structure Int54 : INTEGER = Int54Impl;
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
                               , minInt = ~0x8000_0000
                               , maxInt = 0x7fff_ffff
                               };
