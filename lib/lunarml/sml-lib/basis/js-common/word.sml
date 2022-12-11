local
    fun BigIntToWord (x : IntInf.int) : word = JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.Number #[JavaScript.unsafeToValue x])
    fun WordToBigInt (x : word) : IntInf.int = JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.BigInt #[JavaScript.unsafeToValue x])
    fun asBigInt8 (x : IntInf.int) : IntInf.int = JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.BigInt.asIntN #[JavaScript.fromInt 8, JavaScript.unsafeToValue x])
    fun asBigUint8 (x : IntInf.int) : IntInf.int = JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.BigInt.asUintN #[JavaScript.fromInt 8, JavaScript.unsafeToValue x])
    fun asBigInt16 (x : IntInf.int) : IntInf.int = JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.BigInt.asIntN #[JavaScript.fromInt 16, JavaScript.unsafeToValue x])
    fun asBigUint16 (x : IntInf.int) : IntInf.int = JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.BigInt.asUintN #[JavaScript.fromInt 16, JavaScript.unsafeToValue x])
    fun asBigInt32 (x : IntInf.int) : IntInf.int = JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.BigInt.asIntN #[JavaScript.fromInt 32, JavaScript.unsafeToValue x])
    fun asBigUint32 (x : IntInf.int) : IntInf.int = JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.BigInt.asUintN #[JavaScript.fromInt 32, JavaScript.unsafeToValue x])
    fun asBigInt64 (x : IntInf.int) : IntInf.int = JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.BigInt.asIntN #[JavaScript.fromInt 64, JavaScript.unsafeToValue x])
    fun asBigUint64 (x : IntInf.int) : IntInf.int = JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.BigInt.asUintN #[JavaScript.fromInt 64, JavaScript.unsafeToValue x])
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
                  structure IntInfImpl : sig
                                include INT_INF
                                val fromWord : Word.word -> int
                                val fromWordX : Word.word -> int
                                val toWord : int -> Word.word
                                val fromWord8 : Word8.word -> int
                                val fromWord8X : Word8.word -> int
                                val toWord8 : int -> Word8.word
                                val fromWord16 : Word16.word -> int
                                val fromWord16X : Word16.word -> int
                                val toWord16 : int -> Word16.word
                                val fromWord32 : Word32.word -> int
                                val fromWord32X : Word32.word -> int
                                val toWord32 : int -> Word32.word
                                val fromWord64 : Word64.word -> int
                                val fromWord64X : Word64.word -> int
                                val toWord64 : int -> Word64.word
                                val fromIntegralReal : real -> int
                                val toReal : int -> real
                            end where type int = IntInf.int
              end = struct
    structure Word8 = struct
    type word = Word.word
    val FULL : word = 0wxFF
    val wordSize = 8
    val toLarge = WordToBigInt
    fun toLargeX x = if x >= 0wx80 then
                         asBigUint64 (WordToBigInt x - 0x80)
                     else
                         WordToBigInt x
    fun fromLarge x = BigIntToWord (IntInf.andb (x, 0xFF))
    val toInt = Word.toInt
    fun toIntX x = if Word.andb (x, Word.<< (0w1, Word.fromInt (wordSize - 1))) = 0w0 then
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
    val toLarge = WordToBigInt
    fun toLargeX x = if x >= 0wx8000 then
                         asBigUint64 (WordToBigInt x - 0x8000)
                     else
                         WordToBigInt x
    fun fromLarge x = BigIntToWord (IntInf.andb (x, 0xFFFF))
    val toInt = Word.toInt
    fun toIntX x = if Word.andb (x, Word.<< (0w1, Word.fromInt (wordSize - 1))) = 0w0 then
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
    fun toLarge x = WordToBigInt x
    fun toLargeX x = if x >= 0wx80000000 then
                         asBigUint64 (WordToBigInt x - 0x80000000)
                     else
                         WordToBigInt x
    fun fromLarge x = BigIntToWord (IntInf.andb (x, 0xFFFFFFFF))
    val toLargeWord = toLarge
    val toLargeWordX = toLargeX
    val fromLargeWord = fromLarge
    open Word
    end
    structure Word64 = struct
    type word = IntInf.int
    val FULL : word = 0xFFFF_FFFF_FFFF_FFFF
    val wordSize = 64
    fun toLarge x = x
    fun toLargeX x = x
    fun fromLarge x = x
    val toInt = IntInf.toInt
    fun toIntX x = IntInf.toInt (asBigInt64 x)
    fun fromInt x = asBigUint64 (IntInf.fromInt x)
    val andb = IntInf.andb
    val orb = IntInf.orb
    val xorb = IntInf.xorb
    fun notb x = asBigUint64 (IntInf.notb x)
    fun << (x, y) = asBigUint64 (IntInf.<< (x, y))
    fun >> (x, y) = IntInf.~>> (x, y)
    fun ~>> (x, y) = asBigUint64 (IntInf.~>> (asBigInt64 x, y))
    fun x + y = asBigUint64 (IntInf.+ (x, y))
    fun x - y = asBigUint64 (IntInf.- (x, y))
    fun x * y = asBigUint64 (IntInf.* (x, y))
    fun ~ x = asBigUint64 (IntInf.~ x)
    fun x div y = IntInf.quot (x, y)
    fun x mod y = IntInf.rem (x, y)
    val compare = IntInf.compare
    val op < = IntInf.<
    val op <= = IntInf.<=
    val op > = IntInf.>
    val op >= = IntInf.>=
    val min = IntInf.min
    val max = IntInf.max
    val fmt = IntInf.fmt
    fun toString x = fmt StringCvt.HEX x
    val toLargeWord = toLarge
    val toLargeWordX = toLargeX
    val fromLargeWord = fromLarge
    end
    structure LargeWord = Word64
    fun wordToWord8 x = Word.andb (x, Word8.FULL)
    fun wordToWord16 x = Word.andb (x, Word16.FULL)
    fun wordToWord32 x = x
    fun wordToWord64 x = WordToBigInt x
    fun wordToLarge x = WordToBigInt x
    val wordToLargeX = Word32.toLargeX
    val wordFromLarge = Word32.fromLarge
    structure IntInfImpl = struct
    fun fromWord (x : Word.word) : IntInf.int = WordToBigInt x
    fun fromWord8 (x : Word8.word) : IntInf.int = WordToBigInt x
    fun fromWord16 (x : Word16.word) : IntInf.int = WordToBigInt x
    fun fromWord32 (x : Word32.word) : IntInf.int = WordToBigInt x
    fun fromWord64 (x : Word64.word) : IntInf.int = x
    fun fromWordX (x : Word.word) : IntInf.int = asBigInt32 (WordToBigInt x)
    fun fromWord8X (x : Word8.word) : IntInf.int = asBigInt8 (WordToBigInt x)
    fun fromWord16X (x : Word16.word) : IntInf.int = asBigInt16 (WordToBigInt x)
    fun fromWord32X (x : Word32.word) : IntInf.int = asBigInt32 (WordToBigInt x)
    fun fromWord64X (x : Word64.word) : IntInf.int = asBigInt64 x
    fun toWord (x : IntInf.int) : Word.word = BigIntToWord (asBigUint32 x)
    fun toWord8 (x : IntInf.int) : Word8.word = BigIntToWord (asBigUint8 x)
    fun toWord16 (x : IntInf.int) : Word16.word = BigIntToWord (asBigUint16 x)
    fun toWord32 (x : IntInf.int) : Word32.word = BigIntToWord (asBigUint32 x)
    fun toWord64 (x : IntInf.int) : Word64.word = asBigUint64 x
    open IntInfImpl
    end
    end
in
structure LargeWord = WordImpl.LargeWord
structure Word8 = WordImpl.Word8
structure Word16 = WordImpl.Word16
structure Word32 = WordImpl.Word32
structure Word64 = WordImpl.Word64
structure Word = struct
open Word
val toLarge = WordImpl.wordToLarge
val toLargeX = WordImpl.wordToLargeX
val fromLarge = WordImpl.wordFromLarge
val toLargeWord = WordImpl.wordToLarge
val toLargeWordX = WordImpl.wordToLargeX
val fromLargeWord = WordImpl.wordFromLarge
end
structure IntInfImpl = WordImpl.IntInfImpl;
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
