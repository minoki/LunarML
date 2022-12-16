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
    val ffi = LunarML.assumeDiscardable (fn () => Lua.call1 Lua.Lib.require #[Lua.fromString "ffi"]) ()
    val int64_t = LunarML.assumeDiscardable (fn () => Lua.call1 (Lua.field (ffi, "typeof")) #[Lua.fromString "int64_t"]) () (* ffi.typeof("int64_t") *)
    fun IntToInt64 (x : int) : Lua.value = Lua.call1 int64_t #[Lua.fromInt x]
    fun Int64ToInt (x : Lua.value) : int = Lua.unsafeFromValue (Lua.call1 Lua.Lib.tonumber #[x])
    structure Int64Impl :> sig
                  type int
                  val EQ : int * int -> bool
                  val toLarge : int -> LargeInt.int
                  val fromLarge : LargeInt.int -> int
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
                  val scan : StringCvt.radix -> (char, 'a) StringCvt.reader -> (int, 'a) StringCvt.reader
                  val fromString : string -> int option
              end = struct
    type int = Lua.value
    val EQ = Lua.==
    val ZERO = IntToInt64 0
    val ONE = IntToInt64 1
    val MINUS_ONE = IntToInt64 ~1
    val MIN_INT32_AS_INT64 = IntToInt64 ~0x8000_0000
    val MAX_INT32_AS_INT64 = IntToInt64 0x7FFF_FFFF
    val MIN = Lua.* (IntToInt64 ~2, Lua.* (MIN_INT32_AS_INT64, MIN_INT32_AS_INT64)) (* -2^63 *)
    val MAX = Lua.+ (Lua.* (Lua.+ (MIN_INT32_AS_INT64, IntToInt64 ~0x7FFFFFFF), MIN_INT32_AS_INT64), IntToInt64 0x7FFFFFFF) (* 2^63-1 *)
    val precision = SOME 64
    val minInt = SOME MIN
    val maxInt = SOME MAX
    fun ADD (x, y) = let val z = Lua.+ (x, y)
                     in if Lua.> (y, ZERO) andalso Lua.< (z, x) then
                            raise Overflow
                        else if Lua.< (y, ZERO) andalso Lua.> (z, x) then
                            raise Overflow
                        else
                            z
                     end
    fun SUB (x, y) = let val z = Lua.- (x, y)
                     in if Lua.< (y, ZERO) andalso Lua.< (z, x) then
                            raise Overflow
                        else if Lua.> (y, ZERO) andalso Lua.> (z, x) then
                            raise Overflow
                        else
                            z
                     end
    fun MUL (x, y) = let val z = Lua.* (x, y)
                     in if (Lua.~= (x, ZERO) andalso Lua.~= (Lua./ (z, x), y)) orelse (Lua.~= (y, ZERO) andalso Lua.~= (Lua./ (z, y), x)) then
                            raise Overflow
                        else
                            z
                     end
    fun NEGATE x = if Lua.== (x, MIN) then
                       raise Overflow
                   else
                       Lua.unm x
    fun ABS x = if Lua.== (x, MIN) then
                    raise Overflow
                else if Lua.< (x, ZERO) then
                    Lua.unm x
                else
                    x
    fun quot (x, y) = if Lua.== (y, ZERO) then
                          raise Div
                      else if Lua.== (x, MIN) andalso Lua.== (y, MINUS_ONE) then
                          raise Overflow
                      else
                          Lua./ (x, y)
    fun rem (x, y) = if Lua.== (y, ZERO) then
                         raise Div
                     else
                         Lua.% (x, y)
    fun DIV (x, y) = if Lua.== (y, ZERO) then
                         raise Div
                     else if Lua.== (x, MIN) andalso Lua.== (y, MINUS_ONE) then
                         raise Overflow
                     else if (Lua.>= (x, ZERO) andalso Lua.> (y, ZERO)) orelse (Lua.<= (x, ZERO) andalso Lua.< (y, ZERO)) then
                         Lua./ (x, y) (* same as quot *)
                     else
                         let val q = Lua./ (x, y)
                             val r = Lua.% (x, y)
                         in if Lua.== (r, ZERO) then
                                q
                            else
                                Lua.- (q, Lua.fromInt 1)
                         end
    fun MOD (x, y) = if Lua.== (y, ZERO) then
                         raise Div
                     else if (Lua.>= (x, ZERO) andalso Lua.> (y, ZERO)) orelse (Lua.<= (x, ZERO) andalso Lua.< (y, ZERO)) then
                         Lua.% (x, y) (* same as rem *)
                     else
                         let val r = Lua.% (x, y)
                         in if Lua.== (r, ZERO) then
                                r
                            else
                                Lua.+ (r, y)
                         end
    fun toLarge x = let val d = MIN_INT32_AS_INT64 (* -2^31 *)
                        val q = Lua./ (x, d) (* ~0xffff_ffff <= q <= 0x1_0000_0000 *)
                        val r = Lua.% (x, d) (* ~0x7fff_ffff <= r <= 0x7fff_ffff *)
                        val q2 = Lua./ (q, d)
                        val r2 = Lua.% (q, d)
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
                          in Lua.+ (Lua.* (Lua.+ (Lua.* (q2', d'), r2'), d'), r')
                          end
                      else
                          raise Overflow
    fun toInt x = if Lua.<= (MIN_INT32_AS_INT64, x) andalso Lua.<= (x, MAX_INT32_AS_INT64) then
                      Int64ToInt x
                  else
                      raise Overflow
    val fromInt = IntToInt64
    fun compare (x, y) = if Lua.< (x, y) then
                             LESS
                         else if Lua.> (x, y) then
                             GREATER
                         else
                             EQUAL
    fun min (x, y) = if Lua.< (x, y) then
                         x
                     else
                         y
    fun max (x, y) = if Lua.> (x, y) then
                         x
                     else
                         y
    fun sign x : Int.int = if Lua.< (x, ZERO) then
                               ~1
                           else if Lua.> (x, ZERO) then
                               1
                           else
                               0
    fun sameSign (x, y) = sign x = sign y
    (* Newer LuaJIT (after https://github.com/LuaJIT/LuaJIT/commit/1b7171c339a8d33cb1fd332e31787ebc23266f10):
    fun toString x = if Lua.>= (x, ZERO) then
                         Lua.unsafeFromValue (Lua.call1 Lua.Lib.string.format #[Lua.fromString "%u", x]) : string
                     else
                         Lua.unsafeFromValue (Lua.call1 Lua.Lib.string.format #[Lua.fromString "~%u", Lua.unm x]) : string
    fun fmt StringCvt.BIN x = raise Fail "StringCvt.BIN: not implemented yet"
      | fmt StringCvt.OCT x = if Lua.>= (x, ZERO) then
                                  Lua.unsafeFromValue (Lua.call1 Lua.Lib.string.format #[Lua.fromString "%o", x]) : string
                              else
                                  Lua.unsafeFromValue (Lua.call1 Lua.Lib.string.format #[Lua.fromString "~%o", Lua.unm x]) : string
      | fmt StringCvt.DEC x = toString x
      | fmt StringCvt.HEX x = if Lua.>= (x, ZERO) then
                                  Lua.unsafeFromValue (Lua.call1 Lua.Lib.string.format #[Lua.fromString "%X", x]) : string
                              else
                                  Lua.unsafeFromValue (Lua.call1 Lua.Lib.string.format #[Lua.fromString "~%X", Lua.unm x]) : string
     *)
    fun toStringAbs x = if Lua.<= (x, IntToInt64 0x7fff_ffff) then (* small or -2^63 *)
                            Lua.unsafeFromValue (Lua.call1 Lua.Lib.string.format #[Lua.fromString "%u", Lua.fromInt (Int64ToInt x)]) : string
                        else
                            let val d = IntToInt64 1000000000
                                val q = Lua./ (x, d)
                                val r = Lua.% (x, d)
                            in toStringAbs q ^ Lua.unsafeFromValue (Lua.call1 Lua.Lib.string.format #[Lua.fromString "%09u", Lua.fromInt (Int64ToInt r)])
                            end
    fun toString x = if Lua.>= (x, ZERO) then
                         toStringAbs x
                     else
                         "~" ^ toStringAbs (Lua.unm x) (* overflow is fine *)
    fun toOctStringAbs x = if Lua.<= (x, IntToInt64 0x7fff_ffff) then (* small or -2^63 *)
                               Lua.unsafeFromValue (Lua.call1 Lua.Lib.string.format #[Lua.fromString "%o", Lua.fromInt (Int64ToInt x)]) : string
                           else
                               let val d = IntToInt64 0x40000000 (* 2^30 = 0o10000000000 *)
                                   val q = Lua./ (x, d)
                                   val r = Lua.% (x, d)
                               in toOctStringAbs q ^ Lua.unsafeFromValue (Lua.call1 Lua.Lib.string.format #[Lua.fromString "%010o", Lua.fromInt (Int64ToInt r)])
                               end
    fun toOctString x = if Lua.>= (x, ZERO) then
                            toOctStringAbs x
                        else
                            "~" ^ toOctStringAbs (Lua.unm x) (* overflow is fine *)
    fun toHexStringAbs x = if Lua.<= (x, IntToInt64 0x7fff_ffff) then (* small or -2^63 *)
                               Lua.unsafeFromValue (Lua.call1 Lua.Lib.string.format #[Lua.fromString "%X", Lua.fromInt (Int64ToInt x)]) : string
                           else
                               let val d = IntToInt64 0x10000000 (* 2^28 *)
                                   val q = Lua./ (x, d)
                                   val r = Lua.% (x, d)
                               in toHexStringAbs q ^ Lua.unsafeFromValue (Lua.call1 Lua.Lib.string.format #[Lua.fromString "%07X", Lua.fromInt (Int64ToInt r)])
                               end
    fun toHexString x = if Lua.>= (x, ZERO) then
                            toHexStringAbs x
                        else
                            "~" ^ toHexStringAbs (Lua.unm x) (* overflow is fine *)
    fun fmt StringCvt.BIN = raise Fail "StringCvt.BIN: not implemented yet"
      | fmt StringCvt.OCT = toOctString
      | fmt StringCvt.DEC = toString
      | fmt StringCvt.HEX = toHexString
    local
        open ScanNumUtils
        fun scanDigits (radix, isDigit, getc)
            = let fun go1 (x, strm) = case getc strm of
                                          SOME (c, strm') => if isDigit c then
                                                                 go1 (Lua.+ (Lua.* (Lua.fromInt radix, x), Lua.fromInt (digitToInt c)), strm')
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
                                                                 go1 (Lua.- (Lua.* (Lua.fromInt radix, x), Lua.fromInt (digitToInt c)), strm')
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
    val op < = Lua.<
    val op <= = Lua.<=
    val op > = Lua.>
    val op >= = Lua.>=
    end
in
_equality Int64Impl.int = Int64Impl.EQ
structure Int64 :> INTEGER where type int = Int64Impl.int = Int64Impl
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
