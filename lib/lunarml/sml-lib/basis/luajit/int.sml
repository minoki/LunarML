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
    val unsafeFromValue : Lua.value -> _Prim.Int54.int = Lua.unsafeFromValue
    val toValue : _Prim.Int54.int -> Lua.value = Lua.unsafeToValue
    val fromInt : int -> _Prim.Int54.int = Unsafe.cast
    val unsafeToInt : _Prim.Int54.int -> int = Unsafe.cast
    structure UncheckedInt54 : sig
                  type int
                  val + : int * int -> Lua.value
                  val - : int * int -> Lua.value
                  val * : int * int -> Lua.value
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
    val op + = fn (x, y) => Lua.+ (toValue x, toValue y)
    val op - = fn (x, y) => Lua.- (toValue x, toValue y)
    val op * = fn (x, y) => Lua.* (toValue x, toValue y)
    val op div = fn (x, y) => unsafeFromValue (Lua.call1 Lua.Lib.math.floor #[Lua./ (toValue x, toValue y)])
    val mod2 = fn x => unsafeFromValue (Lua.% (toValue x, toValue 2))
    val quot = fn (x, y) => unsafeFromValue (Lua.call1 Lua.Lib.math.modf #[Lua./ (toValue x, toValue y)])
    val rem = fn (x, y) => unsafeFromValue (Lua.call1 Lua.Lib.math.fmod #[toValue x, toValue y])
    val ~ = fn x => unsafeFromValue (Lua.unm (toValue x))
    val op < = fn (x, y) => Lua.< (toValue x, toValue y)
    val op <= = fn (x, y) => Lua.<= (toValue x, toValue y)
    val op > = fn (x, y) => Lua.> (toValue x, toValue y)
    val op >= = fn (x, y) => Lua.>= (toValue x, toValue y)
    end
    fun Int54_EQUAL (x, y) = Lua.== (toValue x, toValue y);
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
                 in if (Lua.< (toValue MIN, z) andalso Lua.<= (z, toValue MAX)) orelse (Lua.== (z, toValue MIN) andalso UncheckedInt54.mod2 x = UncheckedInt54.mod2 y) then
                        unsafeFromValue z
                    else
                        raise Overflow
                 end
fun SUB (x, y) = let val z = UncheckedInt54.- (x, y)
                 in if (Lua.< (toValue MIN, z) andalso Lua.<= (z, toValue MAX)) orelse (Lua.== (z, toValue MIN) andalso UncheckedInt54.mod2 x = UncheckedInt54.mod2 y) then
                        unsafeFromValue z
                    else
                        raise Overflow
                 end
fun MUL (x, y) = let val z = UncheckedInt54.* (x, y)
                 in if (Lua.< (toValue MIN, z) andalso Lua.<= (z, toValue MAX)) orelse (Lua.== (z, toValue MIN) andalso (UncheckedInt54.mod2 x = 0 orelse UncheckedInt54.mod2 y = 0)) then
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
                     (* For floating-point numbers, x % y is defined as x - math.floor(x/y)*y, which may not be mathematically correct (e.g. (-9007199254740992) % 3 should be 1, but yields 0 on Lua 5.1/5.2/LuaJIT) *)
                     let val r = UncheckedInt54.rem (x, y)
                     in if r = 0 orelse Lua.>= (UncheckedInt54.* (x, y), Lua.fromInt 0) then
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
fun toLarge x = let val d = MIN_INT32_AS_INT54 (* -2^31 *)
                    val q = UncheckedInt54.quot (x, d) (* ~0x3f_ffff <= q <= 0x40_0000 *)
                    val r = UncheckedInt54.rem (x, d) (* ~0x7fff_ffff <= r <= 0x7fff_ffff *)
                    (* x = q * d + r *)
                    val q' = IntInf.fromInt (unsafeToInt q)
                    val r' = IntInf.fromInt (unsafeToInt r)
                    val d' = ~0x80000000 : IntInf.int
                in q' * d' + r'
                end
fun fromLarge x = if ~0x20_0000_0000_0000 <= x andalso x <= 0x1f_ffff_ffff_ffff then
                      let val d = ~0x8000_0000
                          val (q, r) = IntInf.quotRem (x, d)
                          val q' = fromInt (IntInf.toInt q)
                          val r' = fromInt (IntInf.toInt r)
                          val d' = MIN_INT32_AS_INT54
                      in unsafeFromValue (UncheckedInt54.+ (unsafeFromValue (UncheckedInt54.* (q', d')), r'))
                      end
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
fun toOctString x = if UncheckedInt54.>= (x, 0) then
                        Lua.unsafeFromValue (Lua.call1 Lua.Lib.string.format #[Lua.fromString "%o", toValue x]) : string
                    else
                        Lua.unsafeFromValue (Lua.call1 Lua.Lib.string.format #[Lua.fromString "~%o", Lua.unm (toValue x)]) : string (* overflow is ignored *)
fun toString x = if UncheckedInt54.>= (x, 0) then
                     Lua.unsafeFromValue (Lua.call1 Lua.Lib.string.format #[Lua.fromString "%u", toValue x]) : string
                 else
                     Lua.unsafeFromValue (Lua.call1 Lua.Lib.string.format #[Lua.fromString "~%u", Lua.unm (toValue x)]) : string (* overflow is ignored *)
fun toHexString x = if UncheckedInt54.>= (x, 0) then
                        Lua.unsafeFromValue (Lua.call1 Lua.Lib.string.format #[Lua.fromString "%X", toValue x]) : string
                    else
                        Lua.unsafeFromValue (Lua.call1 Lua.Lib.string.format #[Lua.fromString "~%X", Lua.unm (toValue x)]) : string (* overflow is ignored *)
fun fmt StringCvt.BIN = raise Fail "StringCvt.BIN: not implemented yet"
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

local
    val ffi = LunarML.assumeDiscardable (fn () => Lua.call1 Lua.Lib.require #[Lua.fromString "ffi"]) ()
    val int64_t = LunarML.assumeDiscardable (fn () => Lua.call1 (Lua.field (ffi, "typeof")) #[Lua.fromString "int64_t"]) () (* ffi.typeof("int64_t") *)
    val unsafeFromValue : Lua.value -> _Prim.Int64.int = Lua.unsafeFromValue
    val toValue : _Prim.Int64.int -> Lua.value = Lua.unsafeToValue
    structure UncheckedInt64 : sig
                  type int
                  val + : int * int -> int
                  val - : int * int -> int
                  val * : int * int -> int
                  val quot : int * int -> int
                  val rem : int * int -> int
                  val ~ : int -> int
                  val < : int * int -> bool
                  val <= : int * int -> bool
                  val > : int * int -> bool
                  val >= : int * int -> bool
              end = struct
    type int = _Prim.Int64.int
    val op + = fn (x, y) => unsafeFromValue (Lua.+ (toValue x, toValue y))
    val op - = fn (x, y) => unsafeFromValue (Lua.- (toValue x, toValue y))
    val op * = fn (x, y) => unsafeFromValue (Lua.* (toValue x, toValue y))
    val quot = fn (x, y) => unsafeFromValue (Lua./ (toValue x, toValue y))
    val rem = fn (x, y) => unsafeFromValue (Lua.% (toValue x, toValue y))
    val ~ = fn x => unsafeFromValue (Lua.unm (toValue x))
    val op < = fn (x, y) => Lua.< (toValue x, toValue y)
    val op <= = fn (x, y) => Lua.<= (toValue x, toValue y)
    val op > = fn (x, y) => Lua.> (toValue x, toValue y)
    val op >= = fn (x, y) => Lua.>= (toValue x, toValue y)
    end
    fun Int64_EQUAL (x, y) = Lua.== (toValue x, toValue y);
in
_equality _Prim.Int64.int = Int64_EQUAL;
structure Int64 :> INTEGER where type int = _Prim.Int64.int = struct
type int = _Prim.Int64.int
fun IntToInt64 (x : Int.int) : int = unsafeFromValue (Lua.call1 int64_t #[Lua.fromInt x])
fun Int64ToInt (x : int) : Int.int = Lua.unsafeFromValue (Lua.call1 Lua.Lib.tonumber #[toValue x])
val MIN_INT32_AS_INT64 : int = ~0x8000_0000
val MAX_INT32_AS_INT64 : int = 0x7FFF_FFFF
val MIN : int = ~0x8000_0000_0000_0000 (* -2^63 *)
val MAX : int = 0x7FFF_FFFF_FFFF_FFFF (* 2^63-1 *)
val precision = SOME 64
val minInt = SOME MIN
val maxInt = SOME MAX
fun ADD (x, y) = let val z = UncheckedInt64.+ (x, y)
                 in if UncheckedInt64.> (y, 0) andalso UncheckedInt64.< (z, x) then
                        raise Overflow
                    else if UncheckedInt64.< (y, 0) andalso UncheckedInt64.> (z, x) then
                        raise Overflow
                    else
                        z
                 end
fun SUB (x, y) = let val z = UncheckedInt64.- (x, y)
                 in if UncheckedInt64.< (y, 0) andalso UncheckedInt64.< (z, x) then
                        raise Overflow
                    else if UncheckedInt64.> (y, 0) andalso UncheckedInt64.> (z, x) then
                        raise Overflow
                    else
                        z
                 end
fun MUL (x, y) = let val z = UncheckedInt64.* (x, y)
                 in if (x <> 0 andalso UncheckedInt64.quot (z, x) <> y) orelse (y <> 0 andalso UncheckedInt64.quot (z, y) <> x) then
                        raise Overflow
                    else
                        z
                 end
fun NEGATE x = if x = MIN then
                   raise Overflow
               else
                   UncheckedInt64.~ x
fun ABS x = if x = MIN then
                raise Overflow
            else if UncheckedInt64.< (x, 0) then
                UncheckedInt64.~ x
            else
                x
fun quot (x, y) = if y = 0 then
                      raise Div
                  else if x = MIN andalso y = ~1 then
                      raise Overflow
                  else
                      UncheckedInt64.quot (x, y)
fun rem (x, y) = if y = 0 then
                     raise Div
                 else
                     UncheckedInt64.rem (x, y)
fun DIV (x, y) = if y = 0 then
                     raise Div
                 else if x = MIN andalso y = ~1 then
                     raise Overflow
                 else if (UncheckedInt64.>= (x, 0) andalso UncheckedInt64.> (y, 0)) orelse (UncheckedInt64.<= (x, 0) andalso UncheckedInt64.< (y, 0)) then
                     UncheckedInt64.quot (x, y) (* same as quot *)
                 else
                     let val q = UncheckedInt64.quot (x, y)
                         val r = UncheckedInt64.rem (x, y)
                     in if r = 0 then
                            q
                        else
                            UncheckedInt64.- (q, 1)
                     end
fun MOD (x, y) = if y = 0 then
                     raise Div
                 else if (UncheckedInt64.>= (x, 0) andalso UncheckedInt64.> (y, 0)) orelse (UncheckedInt64.<= (x, 0) andalso UncheckedInt64.< (y, 0)) then
                     UncheckedInt64.rem (x, y) (* same as rem *)
                 else
                     let val r = UncheckedInt64.rem (x, y)
                     in if r = 0 then
                            r
                        else
                            UncheckedInt64.+ (r, y)
                     end
fun toLarge x = let val d = MIN_INT32_AS_INT64 (* -2^31 *)
                    val q = UncheckedInt64.quot (x, d) (* ~0xffff_ffff <= q <= 0x1_0000_0000 *)
                    val r = UncheckedInt64.rem (x, d) (* ~0x7fff_ffff <= r <= 0x7fff_ffff *)
                    val q2 = UncheckedInt64.quot (q, d)
                    val r2 = UncheckedInt64.rem (q, d)
                    (* x = (q2 * d + r2) * d + r *)
                    val q2' = IntInf.fromInt (Int64ToInt q2)
                    val r2' = IntInf.fromInt (Int64ToInt r2)
                    val r' = IntInf.fromInt (Int64ToInt r)
                    val d' = ~0x80000000 : IntInf.int
                in (q2' * d' + r2') * d' + r'
                end
fun fromLarge x = if ~0x8000_0000_0000_0000 <= x andalso x <= 0x7fff_ffff_ffff_ffff then
                      let val d = ~0x8000_0000
                          val (q, r) = IntInf.divMod (x, d)
                          val (q2, r2) = IntInf.divMod (q, d)
                          val q2' = IntToInt64 (IntInf.toInt q2)
                          val r2' = IntToInt64 (IntInf.toInt r2)
                          val r' = IntToInt64 (IntInf.toInt r)
                          val d' = MIN_INT32_AS_INT64
                      in UncheckedInt64.+ (UncheckedInt64.* (UncheckedInt64.+ (UncheckedInt64.* (q2', d'), r2'), d'), r')
                      end
                  else
                      raise Overflow
fun toInt x = if UncheckedInt64.<= (MIN_INT32_AS_INT64, x) andalso UncheckedInt64.<= (x, MAX_INT32_AS_INT64) then
                  Int64ToInt x
              else
                  raise Overflow
val fromInt = IntToInt64
fun compare (x, y) = if UncheckedInt64.< (x, y) then
                         LESS
                     else if UncheckedInt64.> (x, y) then
                         GREATER
                     else
                         EQUAL
fun min (x, y) = if UncheckedInt64.< (x, y) then
                     x
                 else
                     y
fun max (x, y) = if UncheckedInt64.> (x, y) then
                     x
                 else
                     y
fun sign x : Int.int = if UncheckedInt64.< (x, 0) then
                           ~1
                       else if UncheckedInt64.> (x, 0) then
                           1
                       else
                           0
fun sameSign (x, y) = sign x = sign y
(* Newer LuaJIT (after https://github.com/LuaJIT/LuaJIT/commit/1b7171c339a8d33cb1fd332e31787ebc23266f10):
fun toString x = if UncheckedInt64.>= (x, 0) then
                     Lua.unsafeFromValue (Lua.call1 Lua.Lib.string.format #[Lua.fromString "%u", toValue x]) : string
                 else
                     Lua.unsafeFromValue (Lua.call1 Lua.Lib.string.format #[Lua.fromString "~%u", toValue (UncheckedInt64.~ x)]) : string
fun fmt StringCvt.BIN x = raise Fail "StringCvt.BIN: not implemented yet"
  | fmt StringCvt.OCT x = if UncheckedInt64.>= (x, 0) then
                              Lua.unsafeFromValue (Lua.call1 Lua.Lib.string.format #[Lua.fromString "%o", toValue x]) : string
                          else
                              Lua.unsafeFromValue (Lua.call1 Lua.Lib.string.format #[Lua.fromString "~%o", toValue (UncheckedInt64.~ x)]) : string
  | fmt StringCvt.DEC x = toString x
  | fmt StringCvt.HEX x = if UncheckedInt64.>= (x, 0) then
                              Lua.unsafeFromValue (Lua.call1 Lua.Lib.string.format #[Lua.fromString "%X", toValue x]) : string
                          else
                              Lua.unsafeFromValue (Lua.call1 Lua.Lib.string.format #[Lua.fromString "~%X", toValue (UncheckedInt64.~ x)]) : string
 *)
fun toStringAbs x = if UncheckedInt64.<= (x, 0x7fff_ffff) then (* small or -2^63 *)
                        Lua.unsafeFromValue (Lua.call1 Lua.Lib.string.format #[Lua.fromString "%u", Lua.fromInt (Int64ToInt x)]) : string
                    else
                        let val d = 1000000000
                            val q = UncheckedInt64.quot (x, d)
                            val r = UncheckedInt64.rem (x, d)
                        in toStringAbs q ^ Lua.unsafeFromValue (Lua.call1 Lua.Lib.string.format #[Lua.fromString "%09u", Lua.fromInt (Int64ToInt r)])
                        end
fun toString x = if UncheckedInt64.>= (x, 0) then
                     toStringAbs x
                 else
                     "~" ^ toStringAbs (UncheckedInt64.~ x) (* overflow is fine *)
fun toOctStringAbs x = if UncheckedInt64.<= (x, 0x7fff_ffff) then (* small or -2^63 *)
                           Lua.unsafeFromValue (Lua.call1 Lua.Lib.string.format #[Lua.fromString "%o", Lua.fromInt (Int64ToInt x)]) : string
                       else
                           let val d = 0x40000000 (* 2^30 = 0o10000000000 *)
                               val q = UncheckedInt64.quot (x, d)
                               val r = UncheckedInt64.rem (x, d)
                           in toOctStringAbs q ^ Lua.unsafeFromValue (Lua.call1 Lua.Lib.string.format #[Lua.fromString "%010o", Lua.fromInt (Int64ToInt r)])
                           end
fun toOctString x = if UncheckedInt64.>= (x, 0) then
                        toOctStringAbs x
                    else
                        "~" ^ toOctStringAbs (UncheckedInt64.~ x) (* overflow is fine *)
fun toHexStringAbs x = if UncheckedInt64.<= (x, 0x7fff_ffff) then (* small or -2^63 *)
                           Lua.unsafeFromValue (Lua.call1 Lua.Lib.string.format #[Lua.fromString "%X", Lua.fromInt (Int64ToInt x)]) : string
                       else
                           let val d = 0x10000000 (* 2^28 *)
                               val q = UncheckedInt64.quot (x, d)
                               val r = UncheckedInt64.rem (x, d)
                           in toHexStringAbs q ^ Lua.unsafeFromValue (Lua.call1 Lua.Lib.string.format #[Lua.fromString "%07X", Lua.fromInt (Int64ToInt r)])
                           end
fun toHexString x = if UncheckedInt64.>= (x, 0) then
                        toHexStringAbs x
                    else
                        "~" ^ toHexStringAbs (UncheckedInt64.~ x) (* overflow is fine *)
fun fmt StringCvt.BIN = raise Fail "StringCvt.BIN: not implemented yet"
  | fmt StringCvt.OCT = toOctString
  | fmt StringCvt.DEC = toString
  | fmt StringCvt.HEX = toHexString
local
    open ScanNumUtils
    fun scanDigits (radix, isDigit, getc)
        = let fun go1 (x, strm) = case getc strm of
                                      SOME (c, strm') => if isDigit c then
                                                             go1 (unsafeFromValue (Lua.+ (Lua.* (Lua.fromInt radix, toValue x), Lua.fromInt (digitToInt c))), strm')
                                                         else
                                                             SOME (x, strm)
                                    | NONE => SOME (x, strm)
          in fn strm => case getc strm of
                            SOME (c, strm') => if isDigit c then
                                                   go1 (IntToInt64 (digitToInt c), strm')
                                               else
                                                   NONE
                          | NONE => NONE
          end
    fun scanNegativeDigits (radix, isDigit, getc)
        = let fun go1 (x, strm) = case getc strm of
                                      SOME (c, strm') => if isDigit c then
                                                             go1 (unsafeFromValue (Lua.- (Lua.* (Lua.fromInt radix, toValue x), Lua.fromInt (digitToInt c))), strm')
                                                         else
                                                             SOME (x, strm)
                                    | NONE => SOME (x, strm)
          in fn strm => case getc strm of
                            SOME (c, strm') => if isDigit c then
                                                   go1 (IntToInt64 (~ (digitToInt c)), strm')
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
val op < = UncheckedInt64.<
val op <= = UncheckedInt64.<=
val op > = UncheckedInt64.>
val op >= = UncheckedInt64.>=
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
