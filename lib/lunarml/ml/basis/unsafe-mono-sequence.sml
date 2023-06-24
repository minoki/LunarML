structure Unsafe : sig
              structure Vector : sig
                            val sub : 'a vector * int -> 'a
                        end
              structure Array : sig
                            val sub : 'a array * int -> 'a
                            val update : 'a array * int * 'a -> {}
                        end
              structure BoolVector : UNSAFE_MONO_VECTOR where type elem = bool where type vector = BoolVector.vector
              structure BoolArray : UNSAFE_MONO_ARRAY where type elem = bool where type array = BoolArray.array
              structure CharVector : UNSAFE_MONO_VECTOR where type elem = Char.char where type vector = CharVector.vector
              structure CharArray : UNSAFE_MONO_ARRAY where type elem = Char.char where type array = CharArray.array
              structure IntVector : UNSAFE_MONO_VECTOR where type elem = Int.int where type vector = IntVector.vector
              structure IntArray : UNSAFE_MONO_ARRAY where type elem = Int.int where type array = IntArray.array
              structure Int8Vector : UNSAFE_MONO_VECTOR where type elem = Int8.int where type vector = Int8Vector.vector
              structure Int8Array : UNSAFE_MONO_ARRAY where type elem = Int8.int where type array = Int8Array.array
              structure Int16Vector : UNSAFE_MONO_VECTOR where type elem = Int16.int where type vector = Int16Vector.vector
              structure Int16Array : UNSAFE_MONO_ARRAY where type elem = Int16.int where type array = Int16Array.array
              structure Int32Vector : UNSAFE_MONO_VECTOR where type elem = Int32.int where type vector = Int32Vector.vector
              structure Int32Array : UNSAFE_MONO_ARRAY where type elem = Int32.int where type array = Int32Array.array
              structure Int64Vector : UNSAFE_MONO_VECTOR where type elem = Int64.int where type vector = Int64Vector.vector
              structure Int64Array : UNSAFE_MONO_ARRAY where type elem = Int64.int where type array = Int64Array.array
              structure WordVector : UNSAFE_MONO_VECTOR where type elem = Word.word where type vector = WordVector.vector
              structure WordArray : UNSAFE_MONO_ARRAY where type elem = Word.word where type array = WordArray.array
              structure Word8Vector : UNSAFE_MONO_VECTOR where type elem = Word8.word where type vector = Word8Vector.vector
              structure Word8Array : UNSAFE_MONO_ARRAY where type elem = Word8.word where type array = Word8Array.array
              structure Word16Vector : UNSAFE_MONO_VECTOR where type elem = Word16.word where type vector = Word16Vector.vector
              structure Word16Array : UNSAFE_MONO_ARRAY where type elem = Word16.word where type array = Word16Array.array
              structure Word32Vector : UNSAFE_MONO_VECTOR where type elem = Word32.word where type vector = Word32Vector.vector
              structure Word32Array : UNSAFE_MONO_ARRAY where type elem = Word32.word where type array = Word32Array.array
              structure Word64Vector : UNSAFE_MONO_VECTOR where type elem = Word64.word where type vector = Word64Vector.vector
              structure Word64Array : UNSAFE_MONO_ARRAY where type elem = Word64.word where type array = Word64Array.array
              structure RealVector : UNSAFE_MONO_VECTOR where type elem = Real.real where type vector = RealVector.vector
              structure RealArray : UNSAFE_MONO_ARRAY where type elem = Real.real where type array = RealArray.array
              val cast : 'a -> 'b
          end = struct
structure BoolVector = UnsafeBoolVector
structure BoolArray = UnsafeBoolArray
structure CharVector = UnsafeCharVector
structure CharArray = UnsafeCharArray
structure IntVector = UnsafeIntVector
structure IntArray = UnsafeIntArray
structure Int8Vector = UnsafeInt8Vector
structure Int8Array = UnsafeInt8Array
structure Int16Vector = UnsafeInt16Vector
structure Int16Array = UnsafeInt16Array
structure Int32Vector = UnsafeInt32Vector
structure Int32Array = UnsafeInt32Array
structure Int64Vector = UnsafeInt64Vector
structure Int64Array = UnsafeInt64Array
structure WordVector = UnsafeWordVector
structure WordArray = UnsafeWordArray
structure Word8Vector = UnsafeWord8Vector
structure Word8Array = UnsafeWord8Array
structure Word16Vector = UnsafeWord16Vector
structure Word16Array = UnsafeWord16Array
structure Word32Vector = UnsafeWord32Vector
structure Word32Array = UnsafeWord32Array
structure Word64Vector = UnsafeWord64Vector
structure Word64Array = UnsafeWord64Array
structure RealVector = UnsafeRealVector
structure RealArray = UnsafeRealArray
open Unsafe
end;
