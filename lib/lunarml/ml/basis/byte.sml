signature BYTE = sig
    val byteToChar : Word8.word -> char
    val charToByte : char -> Word8.word
    val bytesToString : Word8Vector.vector -> string
    val stringToBytes : string -> Word8Vector.vector
    val unpackStringVec : Word8VectorSlice.slice -> string
    val unpackString : Word8ArraySlice.slice -> string
    val packString : Word8Array.array * int * Substring.substring -> unit
end

structure Byte :> BYTE = struct
fun byteToChar x = Char.chr (Word8.toInt x)
fun charToByte x = Word8.fromInt (Char.ord x)
open ByteImpl
end;
