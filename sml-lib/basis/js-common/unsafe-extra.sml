signature UNSAFE_MONO_VECTOR = sig
    type vector
    type elem
    val sub : vector * int -> elem
    (* update, create: not supported *)
end
signature UNSAFE_MONO_ARRAY = sig
    type array
    type elem
    val sub : array * int -> elem
    val update : array * int * elem -> unit
    val create : int -> array
end
structure Unsafe : sig
              structure Vector : sig
                            val sub : 'a vector * int -> 'a
                        end
              structure Array : sig
                            val sub : 'a array * int -> 'a
                            val update : 'a array * int * 'a -> {}
                        end
              structure CharVector : UNSAFE_MONO_VECTOR where type vector = CharVector.vector where type elem = Char.char
              structure Word8Vector : UNSAFE_MONO_VECTOR where type vector = Word8Vector.vector where type elem = Word8.word
              structure CharArray : UNSAFE_MONO_ARRAY where type array = CharArray.array where type elem = Char.char
              structure Word8Array : UNSAFE_MONO_ARRAY where type array = Word8Array.array where type elem = Word8.word
              val cast : 'a -> 'b
          end = struct
structure CharVector = struct
type vector = CharVector.vector
type elem = Char.char
val sub = CharVector.sub
end
structure Word8Vector = struct
type vector = Word8Vector.vector
type elem = Word8.word
val sub = Word8Vector.sub
end
structure CharArray = struct
type array = CharArray.array
type elem = Char.char
val sub = CharArray.sub
val update = CharArray.update
fun create n = CharArray.array (n, #"\000")
end
structure Word8Array = struct
type array = Word8Array.array
type elem = Word8.word
val sub = Word8Array.sub
val update = Word8Array.update
fun create n = Word8Array.array (n, 0w0)
end
open Unsafe
end;
