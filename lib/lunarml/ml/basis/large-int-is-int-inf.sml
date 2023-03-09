signature INTEGER = sig
    include INTEGER
    val toLarge : int -> IntInf.int
    val fromLarge : IntInf.int -> int
end

structure LargeInt : INTEGER = IntInf;

structure Int : INTEGER = struct
val toLarge = IntInf.fromInt
val fromLarge = IntInf.toInt
open Int
end;

signature WORD = sig
    include WORD
    val toLargeInt : word -> LargeInt.int
    val toLargeIntX : word -> LargeInt.int
    val fromLargeInt : LargeInt.int -> word
end;

structure Word : WORD where type word = Word.word = struct
val toLargeInt = IntInfImpl.fromWord
val toLargeIntX = IntInfImpl.fromWordX
val fromLargeInt = IntInfImpl.toWord
open Word
end

structure Word8 : WORD where type word = Word8.word = struct
val toLargeInt = IntInfImpl.fromWord8
val toLargeIntX = IntInfImpl.fromWord8X
val fromLargeInt = IntInfImpl.toWord8
open Word8
end

structure Word16 : WORD where type word = Word16.word = struct
val toLargeInt = IntInfImpl.fromWord16
val toLargeIntX = IntInfImpl.fromWord16X
val fromLargeInt = IntInfImpl.toWord16
open Word16
end

structure Word32 : WORD where type word = Word32.word = struct
val toLargeInt = IntInfImpl.fromWord32
val toLargeIntX = IntInfImpl.fromWord32X
val fromLargeInt = IntInfImpl.toWord32
open Word32
end

structure Word64 : WORD where type word = Word64.word = struct
val toLargeInt = IntInfImpl.fromWord64
val toLargeIntX = IntInfImpl.fromWord64X
val fromLargeInt = IntInfImpl.toWord64
open Word64
end

structure LargeWord = Word64;
