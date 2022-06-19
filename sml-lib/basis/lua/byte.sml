signature BYTE = sig
    val byteToChar : Word8.word -> char
    val charToByte : char -> Word8.word
    val bytesToString : Word8Vector.vector -> string
    val stringToBytes : string -> Word8Vector.vector
end

structure Byte :> BYTE = struct
fun byteToChar x = Char.chr (Word8.toInt x)
fun charToByte x = Word8.fromInt (Char.ord x)
val bytesToString = Word8VectorExtra.bytesToString
val stringToBytes = Word8VectorExtra.stringToBytes
end;
