local
    structure WordImpl :> sig
                  structure LargeWord : sig
                                eqtype word
                                val wordSize : int
                                val toLarge : word -> word
                                val fromLarge : word -> word
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
                                val fromLarge : LargeWord.word -> word
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
                                val fromLarge : LargeWord.word -> word
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
                                val fromLarge : LargeWord.word -> word
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
                                val fromLarge : LargeWord.word -> word
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
                  val wordToLargeWord : Word.word -> LargeWord.word
                  val wordFromLargeWord : LargeWord.word -> Word.word
              end = struct
    structure Word8 = struct
    type word = Word.word
    val FULL : word = 0wxFF
    val wordSize = 8
    fun toLarge x = x
    fun fromLarge x = Word.andb (x, FULL)
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
    fun ~>> (x, y) = Word.andb (Word.~>> (x, y), FULL)
    fun x + y = Word.andb (Word.+ (x, y), FULL)
    fun x - y = Word.andb (Word.- (x, y), FULL)
    fun x * y = Word.andb (Word.* (x, y), FULL)
    fun x div y = Word.andb (Word.div (x, y), FULL)
    fun x mod y = Word.andb (Word.div (x, y), FULL)
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
    end
    structure Word16  = struct
    type word = Word.word
    val FULL : word = 0wxFFFF
    val wordSize = 16
    fun toLarge x = x
    fun fromLarge x = Word.andb (x, FULL)
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
    fun ~>> (x, y) = Word.andb (Word.~>> (x, y), FULL)
    fun x + y = Word.andb (Word.+ (x, y), FULL)
    fun x - y = Word.andb (Word.- (x, y), FULL)
    fun x * y = Word.andb (Word.* (x, y), FULL)
    fun x div y = Word.andb (Word.div (x, y), FULL)
    fun x mod y = Word.andb (Word.div (x, y), FULL)
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
    end
    structure Word32 = struct
    type word = Word.word
    val FULL : word = 0wxFFFFFFFF
    val wordSize = 32
    fun toLarge x = x
    fun fromLarge x = Word.andb (x, FULL)
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
    fun ~>> (x, y) = Word.andb (Word.~>> (x, y), FULL)
    fun x + y = Word.andb (Word.+ (x, y), FULL)
    fun x - y = Word.andb (Word.- (x, y), FULL)
    fun x * y = Word.andb (Word.* (x, y), FULL)
    fun x div y = Word.andb (Word.div (x, y), FULL)
    fun x mod y = Word.andb (Word.div (x, y), FULL)
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
    end
    structure Word64 = struct
    open Word
    fun toLarge x = x
    fun fromLarge x = x
    end
    structure LargeWord = Word64
    val () = if Word.wordSize <> 64 then
                 raise Fail "Word64 is not available"
             else
                 ()
    fun wordToWord8 x = Word.andb (x, Word8.FULL)
    fun wordToWord16 x = Word.andb (x, Word16.FULL)
    fun wordToWord32 x = Word.andb (x, Word32.FULL)
    fun wordToWord64 x = x
    fun wordToLargeWord x = x
    fun wordFromLargeWord x = x
    end
in
structure LargeWord = WordImpl.LargeWord
structure Word8 = WordImpl.Word8
structure Word16 = WordImpl.Word16
structure Word32 = WordImpl.Word32
structure Word64 = WordImpl.Word64
structure Word = struct
open Word
val fromLarge = WordImpl.wordFromLargeWord
val toLarge = WordImpl.wordToLargeWord
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
