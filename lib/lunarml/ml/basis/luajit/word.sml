local
    val ffi = LunarML.assumeDiscardable (fn () => Lua.call1 Lua.Lib.require #[Lua.fromString "ffi"]) ()
    val int64_t = LunarML.assumeDiscardable (fn () => Lua.call1 (Lua.field (ffi, "typeof")) #[Lua.fromString "int64_t"]) () (* ffi.typeof("int64_t") *)
    val uint64_t = LunarML.assumeDiscardable (fn () => Lua.call1 (Lua.field (ffi, "typeof")) #[Lua.fromString "uint64_t"]) () (* ffi.typeof("uint64_t") *)
    structure UncheckedWord64 : sig
                  type word
                  val unsafeFromValue : Lua.value -> word
                  val toValue : word -> Lua.value
                  val + : word * word -> word
                  val - : word * word -> word
                  val * : word * word -> word
                  val div : word * word -> word
                  val mod : word * word -> word
                  val ~ : word -> word
                  val < : word * word -> bool
                  val <= : word * word -> bool
                  val > : word * word -> bool
                  val >= : word * word -> bool
              end = struct
    type word = _Prim.Word64.word
    val unsafeFromValue : Lua.value -> _Prim.Word64.word = Lua.unsafeFromValue
    val toValue : _Prim.Word64.word -> Lua.value = Lua.unsafeToValue
    val op + = fn (x, y) => _primCall "Word64.+" (x, y)
    val op - = fn (x, y) => _primCall "Word64.-" (x, y)
    val op * = fn (x, y) => _primCall "Word64.*" (x, y)
    val op div = fn (x, y) => _primCall "Word64.div.unchecked" (x, y)
    val op mod = fn (x, y) => _primCall "Word64.mod.unchecked" (x, y)
    val ~ = fn x => _primCall "Word64.~" (x)
    val op < = fn (x, y) => _primCall "Word64.<" (x, y)
    val op <= = fn (x, y) => _primCall "Word64.<=" (x, y)
    val op > = fn (x, y) => _primCall "Word64.>" (x, y)
    val op >= = fn (x, y) => _primCall "Word64.>=" (x, y)
    end
    fun Word64_EQUAL (x, y) = Lua.== (UncheckedWord64.toValue x, UncheckedWord64.toValue y);
    _equality _Prim.Word64.word = Word64_EQUAL;
    fun WordToUint64 (x : word) : _Prim.Word64.word = UncheckedWord64.unsafeFromValue (Lua.call1 uint64_t #[Lua.fromWord x])
    fun Uint64ToWord (x : _Prim.Word64.word) : word = Lua.unsafeFromValue (Lua.call1 Lua.Lib.tonumber #[UncheckedWord64.toValue x])
    structure WordImpl :> sig
                  structure LargeWord : sig
                                eqtype word
                                val wordSize : int
                                val toLarge : word -> word
                                val toLargeX : word -> word
                                val toLargeWord : word -> word
                                val toLargeWordX : word -> word
                                val fromLarge : word -> word
                                val fromLargeWord : word -> word
                                val toInt : word -> int
                                val toIntX : word -> int
                                val fromInt : int -> word
                                val andb : word * word -> word
                                val orb : word * word -> word
                                val xorb : word * word -> word
                                val notb : word -> word
                                val << : word * Word.word -> word
                                val >> : word * Word.word -> word
                                val ~>> : word * Word.word -> word
                                val + : word * word -> word
                                val - : word * word -> word
                                val * : word * word -> word
                                val div : word * word -> word
                                val mod : word * word -> word
                                val compare : word * word -> order
                                val < : word * word -> bool
                                val <= : word * word -> bool
                                val > : word * word -> bool
                                val >= : word * word -> bool
                                val ~ : word -> word
                                val min : word * word -> word
                                val max : word * word -> word
                                val fmt : StringCvt.radix -> word -> string
                                val toString : word -> string
                            end
                  structure Word8 : sig
                                eqtype word
                                val wordSize : int
                                val toLarge : word -> LargeWord.word
                                val toLargeX : word -> LargeWord.word
                                val toLargeWord : word -> LargeWord.word
                                val toLargeWordX : word -> LargeWord.word
                                val fromLarge : LargeWord.word -> word
                                val fromLargeWord : LargeWord.word -> word
                                val toInt : word -> int
                                val toIntX : word -> int
                                val fromInt : int -> word
                                val andb : word * word -> word
                                val orb : word * word -> word
                                val xorb : word * word -> word
                                val notb : word -> word
                                val << : word * Word.word -> word
                                val >> : word * Word.word -> word
                                val ~>> : word * Word.word -> word
                                val + : word * word -> word
                                val - : word * word -> word
                                val * : word * word -> word
                                val div : word * word -> word
                                val mod : word * word -> word
                                val compare : word * word -> order
                                val < : word * word -> bool
                                val <= : word * word -> bool
                                val > : word * word -> bool
                                val >= : word * word -> bool
                                val ~ : word -> word
                                val min : word * word -> word
                                val max : word * word -> word
                                val fmt : StringCvt.radix -> word -> string
                                val toString : word -> string
                            end
                  structure Word16 : sig
                                eqtype word
                                val wordSize : int
                                val toLarge : word -> LargeWord.word
                                val toLargeX : word -> LargeWord.word
                                val toLargeWord : word -> LargeWord.word
                                val toLargeWordX : word -> LargeWord.word
                                val fromLarge : LargeWord.word -> word
                                val fromLargeWord : LargeWord.word -> word
                                val toInt : word -> int
                                val toIntX : word -> int
                                val fromInt : int -> word
                                val andb : word * word -> word
                                val orb : word * word -> word
                                val xorb : word * word -> word
                                val notb : word -> word
                                val << : word * Word.word -> word
                                val >> : word * Word.word -> word
                                val ~>> : word * Word.word -> word
                                val + : word * word -> word
                                val - : word * word -> word
                                val * : word * word -> word
                                val div : word * word -> word
                                val mod : word * word -> word
                                val compare : word * word -> order
                                val < : word * word -> bool
                                val <= : word * word -> bool
                                val > : word * word -> bool
                                val >= : word * word -> bool
                                val ~ : word -> word
                                val min : word * word -> word
                                val max : word * word -> word
                                val fmt : StringCvt.radix -> word -> string
                                val toString : word -> string
                            end
                  structure Word32 : sig
                                eqtype word
                                val wordSize : int
                                val toLarge : word -> LargeWord.word
                                val toLargeX : word -> LargeWord.word
                                val toLargeWord : word -> LargeWord.word
                                val toLargeWordX : word -> LargeWord.word
                                val fromLarge : LargeWord.word -> word
                                val fromLargeWord : LargeWord.word -> word
                                val toInt : word -> int
                                val toIntX : word -> int
                                val fromInt : int -> word
                                val andb : word * word -> word
                                val orb : word * word -> word
                                val xorb : word * word -> word
                                val notb : word -> word
                                val << : word * Word.word -> word
                                val >> : word * Word.word -> word
                                val ~>> : word * Word.word -> word
                                val + : word * word -> word
                                val - : word * word -> word
                                val * : word * word -> word
                                val div : word * word -> word
                                val mod : word * word -> word
                                val compare : word * word -> order
                                val < : word * word -> bool
                                val <= : word * word -> bool
                                val > : word * word -> bool
                                val >= : word * word -> bool
                                val ~ : word -> word
                                val min : word * word -> word
                                val max : word * word -> word
                                val fmt : StringCvt.radix -> word -> string
                                val toString : word -> string
                            end
                  structure Word64 : sig
                                eqtype word
                                val wordSize : int
                                val toLarge : word -> LargeWord.word
                                val toLargeX : word -> LargeWord.word
                                val toLargeWord : word -> LargeWord.word
                                val toLargeWordX : word -> LargeWord.word
                                val fromLarge : LargeWord.word -> word
                                val fromLargeWord : LargeWord.word -> word
                                val toInt : word -> int
                                val toIntX : word -> int
                                val fromInt : int -> word
                                val andb : word * word -> word
                                val orb : word * word -> word
                                val xorb : word * word -> word
                                val notb : word -> word
                                val << : word * Word.word -> word
                                val >> : word * Word.word -> word
                                val ~>> : word * Word.word -> word
                                val + : word * word -> word
                                val - : word * word -> word
                                val * : word * word -> word
                                val div : word * word -> word
                                val mod : word * word -> word
                                val compare : word * word -> order
                                val < : word * word -> bool
                                val <= : word * word -> bool
                                val > : word * word -> bool
                                val >= : word * word -> bool
                                val ~ : word -> word
                                val min : word * word -> word
                                val max : word * word -> word
                                val fmt : StringCvt.radix -> word -> string
                                val toString : word -> string
                            end
                  sharing type LargeWord.word = Word64.word
                  val wordToWord8 : Word.word -> Word8.word
                  val wordToWord16 : Word.word -> Word16.word
                  val wordToWord32 : Word.word -> Word32.word
                  val wordToWord64 : Word.word -> Word64.word
                  val wordToLarge : Word.word -> LargeWord.word
                  val wordToLargeX : Word.word -> LargeWord.word
                  val wordFromLarge : LargeWord.word -> Word.word
              end = struct
    structure Word64 = struct
    type word = UncheckedWord64.word
    val wordSize = 64
    fun toLarge x = x
    fun toLargeX x = x
    fun fromLarge x = x
    val toLargeWord = toLarge
    val toLargeWordX = toLargeX
    val fromLargeWord = fromLarge
    fun toInt x = if UncheckedWord64.>= (x, 0wx20_0000_0000_0000) then
                      raise Overflow
                  else
                      Lua.unsafeFromValue (Lua.call1 Lua.Lib.tonumber #[UncheckedWord64.toValue x]) : int
    val toIntX = toInt
    fun fromInt (x : int) = UncheckedWord64.unsafeFromValue (Lua.call1 uint64_t #[Lua.call1 int64_t #[Lua.fromInt x]])
    fun andb (x, y) = UncheckedWord64.unsafeFromValue (Lua.andb (UncheckedWord64.toValue x, UncheckedWord64.toValue y))
    fun orb (x, y) = UncheckedWord64.unsafeFromValue (Lua.orb (UncheckedWord64.toValue x, UncheckedWord64.toValue y))
    fun xorb (x, y) = UncheckedWord64.unsafeFromValue (Lua.xorb (UncheckedWord64.toValue x, UncheckedWord64.toValue y))
    fun notb x = UncheckedWord64.unsafeFromValue (Lua.notb (UncheckedWord64.toValue x))
    fun << (x, y) = if y >= 0w64 then
                        0w0
                    else
                        UncheckedWord64.unsafeFromValue (Lua.<< (UncheckedWord64.toValue x, Lua.fromWord y))
    fun >> (x, y) = if y >= 0w64 then
                        0w0
                    else
                        UncheckedWord64.unsafeFromValue (Lua.>> (UncheckedWord64.toValue x, Lua.fromWord y))
    fun ~>> (x, y) = let val y = Word.min (y, 0w63)
                     in UncheckedWord64.unsafeFromValue (Lua.call1 Lua.Lib.bit.arshift #[UncheckedWord64.toValue x, Lua.fromWord y])
                     end
    val op + = UncheckedWord64.+
    val op - = UncheckedWord64.-
    val op * = UncheckedWord64.*
    fun x div y = if y = 0w0 then
                      raise Div
                  else
                      UncheckedWord64.div (x, y)
    fun x mod y = if y = 0w0 then
                      raise Div
                  else
                      UncheckedWord64.mod (x, y)
    fun compare (x, y) = if UncheckedWord64.< (x, y) then
                             LESS
                         else if UncheckedWord64.> (x, y) then
                             GREATER
                         else
                             EQUAL
    val op < = UncheckedWord64.<
    val op <= = UncheckedWord64.<=
    val op > = UncheckedWord64.>
    val op >= = UncheckedWord64.>=
    val ~ = UncheckedWord64.~
    fun min (x, y) = if UncheckedWord64.< (x, y) then
                         x
                     else
                         y
    fun max (x, y) = if UncheckedWord64.< (x, y) then
                         y
                     else
                         x
    (* Newer LuaJIT (after https://github.com/LuaJIT/LuaJIT/commit/1b7171c339a8d33cb1fd332e31787ebc23266f10):
    fun fmt StringCvt.BIN x = raise Fail "StringCvt.BIN: not implemented yet"
      | fmt StringCvt.OCT x = let val result = Lua.call1 Lua.Lib.string.format #[Lua.fromString "%o", UncheckedWord64.toValue x]
                              in Lua.unsafeFromValue result
                              end
      | fmt StringCvt.DEC x = let val result = Lua.call1 Lua.Lib.string.format #[Lua.fromString "%u", UncheckedWord64.toValue x]
                              in Lua.unsafeFromValue result
                              end
      | fmt StringCvt.HEX x = let val result = Lua.call1 Lua.Lib.string.format #[Lua.fromString "%X", UncheckedWord64.toValue x]
                              in Lua.unsafeFromValue result
                              end
    val toString : word -> string = fn x => Lua.unsafeFromValue (Lua.call1 Lua.Lib.string.format #[Lua.fromString "%X", x])
     *)
    fun fmtBIN (x : word) : string = raise Fail "StringCvt.BIN: not implemented yet"
    fun fmtOCT (x : word) : string =
      if UncheckedWord64.<= (x, 0wxffff_ffff) then (* small *)
          Lua.unsafeFromValue (Lua.call1 Lua.Lib.string.format #[Lua.fromString "%o", Lua.fromWord (Uint64ToWord x)]) : string
      else
          let val d = 0wx40000000 (* 2^30 = 0o10000000000 *)
              val q = UncheckedWord64.div (x, d)
              val r = UncheckedWord64.mod (x, d)
          in fmtOCT q ^ Lua.unsafeFromValue (Lua.call1 Lua.Lib.string.format #[Lua.fromString "%010o", Lua.fromWord (Uint64ToWord r)])
          end
    fun fmtDEC (x : word) : string =
      if UncheckedWord64.<= (x, 0wxffff_ffff) then (* small *)
          Lua.unsafeFromValue (Lua.call1 Lua.Lib.string.format #[Lua.fromString "%u", Lua.fromWord (Uint64ToWord x)]) : string
      else
          let val d = 0w1000000000
              val q = UncheckedWord64.div (x, d)
              val r = UncheckedWord64.mod (x, d)
          in fmtDEC q ^ Lua.unsafeFromValue (Lua.call1 Lua.Lib.string.format #[Lua.fromString "%09u", Lua.fromWord (Uint64ToWord r)])
          end
    fun toString (x : word) : string =
      if UncheckedWord64.<= (x, 0wxffff_ffff) then (* small *)
          Lua.unsafeFromValue (Lua.call1 Lua.Lib.string.format #[Lua.fromString "%X", Lua.fromWord (Uint64ToWord x)]) : string
      else
          let val d = 0wx10000000 (* 2^28 *)
              val q = UncheckedWord64.div (x, d)
              val r = UncheckedWord64.mod (x, d)
          in toString q ^ Lua.unsafeFromValue (Lua.call1 Lua.Lib.string.format #[Lua.fromString "%07X", Lua.fromWord (Uint64ToWord r)])
          end
    fun fmt StringCvt.BIN = fmtBIN
      | fmt StringCvt.OCT = fmtOCT
      | fmt StringCvt.DEC = fmtDEC
      | fmt StringCvt.HEX = toString
    end
    fun wordToWord64 x = WordToUint64 x
    fun wordToLarge x = WordToUint64 x
    fun wordToLargeX x = if x >= 0wx80000000 then
                             Word64.- (WordToUint64 x, WordToUint64 0wx80000000)
                         else
                             WordToUint64 x
    fun wordFromLarge x = Uint64ToWord (Word64.andb (x, WordToUint64 0wxffffffff))
    structure LargeWord = Word64
    structure Word8 = struct
    type word = Word.word
    val FULL : word = 0wxFF
    val wordSize = 8
    fun toLarge x = WordToUint64 x
    fun toLargeX x = if x >= 0wx80 then
                         Word64.- (WordToUint64 x, WordToUint64 0wx80)
                     else
                         WordToUint64 x
    fun fromLarge x = Uint64ToWord (Word64.andb (x, WordToUint64 FULL))
    val toInt = Word.toInt
    fun toIntX x = if Word.andb (x, _primCall "Word.<<.unchecked" (0w1, Word.fromInt (wordSize - 1))) = 0w0 then
                       Word.toInt x
                   else
                       ~ (Word.toInt (FULL - x) + 1)
    fun fromInt x = Word.andb (Word.fromInt x, FULL)
    val andb = Word.andb
    val orb = Word.orb
    val xorb = Word.xorb
    fun notb x = Word.andb (Word.notb x, FULL)
    fun << (x, y) = Word.andb (Word.<< (x, y), FULL)
    val >> = Word.>>
    fun ~>> (x, y) = if x >= 0wx80 then
                         Word.andb (Word.~>> (Word.andb (x, 0wx7F) - 0wx80, y), FULL)
                     else
                         Word.>> (x, y)
    fun x + y = Word.andb (Word.+ (x, y), FULL)
    fun x - y = Word.andb (Word.- (x, y), FULL)
    fun x * y = Word.andb (Word.* (x, y), FULL)
    fun x div y = Word.andb (Word.div (x, y), FULL)
    fun x mod y = Word.andb (Word.mod (x, y), FULL)
    val compare = Word.compare
    val op < = Word.<
    val op <= = Word.<=
    val op > = Word.>
    val op >= = Word.>=
    fun ~ x = Word.andb (Word.~ x, FULL)
    val min = Word.min
    val max = Word.max
    val fmt = Word.fmt
    val toString = Word.toString
    val toLargeWord = toLarge
    val toLargeWordX = toLargeX
    val fromLargeWord = fromLarge
    end
    structure Word16  = struct
    type word = Word.word
    val FULL : word = 0wxFFFF
    val wordSize = 16
    fun toLarge x = WordToUint64 x
    fun toLargeX x = if x >= 0wx8000 then
                         Word64.- (WordToUint64 x, WordToUint64 0wx8000)
                     else
                         WordToUint64 x
    fun fromLarge x = Uint64ToWord (Word64.andb (x, WordToUint64 FULL))
    val toInt = Word.toInt
    fun toIntX x = if Word.andb (x, _primCall "Word.<<.unchecked" (0w1, Word.fromInt (wordSize - 1))) = 0w0 then
                       Word.toInt x
                   else
                       ~ (Word.toInt (FULL - x) + 1)
    fun fromInt x = Word.andb (Word.fromInt x, FULL)
    val andb = Word.andb
    val orb = Word.orb
    val xorb = Word.xorb
    fun notb x = Word.andb (Word.notb x, FULL)
    fun << (x, y) = Word.andb (Word.<< (x, y), FULL)
    val >> = Word.>>
    fun ~>> (x, y) = if x >= 0wx8000 then
                         Word.andb (Word.~>> (Word.andb (x, 0wx7FFF) - 0wx8000, y), FULL)
                     else
                         Word.>> (x, y)
    fun x + y = Word.andb (Word.+ (x, y), FULL)
    fun x - y = Word.andb (Word.- (x, y), FULL)
    fun x * y = Word.andb (Word.* (x, y), FULL)
    fun x div y = Word.andb (Word.div (x, y), FULL)
    fun x mod y = Word.andb (Word.mod (x, y), FULL)
    val compare = Word.compare
    val op < = Word.<
    val op <= = Word.<=
    val op > = Word.>
    val op >= = Word.>=
    fun ~ x = Word.andb (Word.~ x, FULL)
    val min = Word.min
    val max = Word.max
    val fmt = Word.fmt
    val toString = Word.toString
    val toLargeWord = toLarge
    val toLargeWordX = toLargeX
    val fromLargeWord = fromLarge
    end
    structure Word32 = struct
    val FULL : word = 0wxFFFFFFFF
    fun toLarge x = WordToUint64 x
    fun toLargeX x = if x >= 0wx80000000 then
                         Word64.- (WordToUint64 x, WordToUint64 0wx80000000)
                     else
                         WordToUint64 x
    fun fromLarge x = Uint64ToWord (Word64.andb (x, WordToUint64 FULL))
    val toLargeWord = toLarge
    val toLargeWordX = toLargeX
    val fromLargeWord = fromLarge
    open Word
    end
    fun wordToWord8 x = Word.andb (x, Word8.FULL)
    fun wordToWord16 x = Word.andb (x, Word16.FULL)
    fun wordToWord32 x = Word.andb (x, Word32.FULL)
    end
in
structure LargeWord = WordImpl.LargeWord
structure Word8 = WordImpl.Word8
structure Word16 = WordImpl.Word16
structure Word32 = WordImpl.Word32
structure Word64 = WordImpl.Word64 : sig
                       eqtype word
                       val wordSize : int
                       val toLarge : word -> LargeWord.word
                       val toLargeX : word -> LargeWord.word
                       val toLargeWord : word -> LargeWord.word
                       val toLargeWordX : word -> LargeWord.word
                       val fromLarge : LargeWord.word -> word
                       val fromLargeWord : LargeWord.word -> word
                       val toInt : word -> int
                       val toIntX : word -> int
                       val fromInt : int -> word
                       val andb : word * word -> word
                       val orb : word * word -> word
                       val xorb : word * word -> word
                       val notb : word -> word
                       val << : word * Word.word -> word
                       val >> : word * Word.word -> word
                       val ~>> : word * Word.word -> word
                       val + : word * word -> word
                       val - : word * word -> word
                       val * : word * word -> word
                       val div : word * word -> word
                       val mod : word * word -> word
                       val compare : word * word -> order
                       val < : word * word -> bool
                       val <= : word * word -> bool
                       val > : word * word -> bool
                       val >= : word * word -> bool
                       val ~ : word -> word
                       val min : word * word -> word
                       val max : word * word -> word
                       val fmt : StringCvt.radix -> word -> string
                       val toString : word -> string
                   end
structure Word = struct
open Word
val toLarge = WordImpl.wordToLarge
val toLargeX = WordImpl.wordToLargeX
val fromLarge = WordImpl.wordFromLarge
val toLargeWord = WordImpl.wordToLarge
val toLargeWordX = WordImpl.wordToLargeX
val fromLargeWord = WordImpl.wordFromLarge
end;
_overload "Word" [Word8.word] { + = Word8.+
                              , - = Word8.-
                              , * = Word8.*
                              , div = Word8.div
                              , mod = Word8.mod
                              , ~ = Word8.~
                              , < = Word8.<
                              , <= = Word8.<=
                              , > = Word8.>
                              , >= = Word8.>=
                              , fromWord = WordImpl.wordToWord8
                              , wordSize = 8
                              }
_overload "Word" [Word16.word] { + = Word16.+
                               , - = Word16.-
                               , * = Word16.*
                               , div = Word16.div
                               , mod = Word16.mod
                               , ~ = Word16.~
                               , < = Word16.<
                               , <= = Word16.<=
                               , > = Word16.>
                               , >= = Word16.>=
                               , fromWord = WordImpl.wordToWord16
                               , wordSize = 16
                               }
_overload "Word" [Word32.word] { + = Word32.+
                               , - = Word32.-
                               , * = Word32.*
                               , div = Word32.div
                               , mod = Word32.mod
                               , ~ = Word32.~
                               , < = Word32.<
                               , <= = Word32.<=
                               , > = Word32.>
                               , >= = Word32.>=
                               , fromWord = WordImpl.wordToWord32
                               , wordSize = 32
                               }
_overload "Word" [Word64.word] { + = Word64.+
                               , - = Word64.-
                               , * = Word64.*
                               , div = Word64.div
                               , mod = Word64.mod
                               , ~ = Word64.~
                               , < = Word64.<
                               , <= = Word64.<=
                               , > = Word64.>
                               , >= = Word64.>=
                               , fromWord = WordImpl.wordToWord64
                               , wordSize = 64
                               }
end

signature WORD = sig
    include WORD
    val toLarge : word -> LargeWord.word
    val toLargeX : word -> LargeWord.word
    val toLargeWord : word -> LargeWord.word
    val toLargeWordX : word -> LargeWord.word
    val fromLarge : LargeWord.word -> word
    val fromLargeWord : LargeWord.word -> word
end;
