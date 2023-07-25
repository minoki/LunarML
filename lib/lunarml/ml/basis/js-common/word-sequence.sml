local
    structure Word8Sequence :> sig
                  structure Word8Vector : MONO_VECTOR where type elem = Word8.word
                  structure Word8VectorSlice : MONO_VECTOR_SLICE where type elem = Word8.word where type vector = Word8Vector.vector
                  structure Word8Array : MONO_ARRAY where type elem = Word8.word
                  structure Word8ArraySlice : MONO_ARRAY_SLICE where type elem = Word8.word
                  structure ByteImpl : sig
                                val bytesToString : Word8Vector.vector -> string
                                val stringToBytes : string -> Word8Vector.vector
                                val unpackStringVec : Word8VectorSlice.slice -> string
                                val unpackString : Word8ArraySlice.slice -> string
                                val packString : Word8Array.array * int * Substring.substring -> unit
                            end
                  structure UnsafeWord8Vector : UNSAFE_MONO_VECTOR where type elem = Word8.word
                  structure UnsafeWord8Array : UNSAFE_MONO_ARRAY where type elem = Word8.word
                  sharing type Word8Vector.vector = Word8Array.vector = Word8ArraySlice.vector = UnsafeWord8Vector.vector
                  sharing type Word8Array.array = Word8ArraySlice.array = UnsafeWord8Array.array
                  sharing type Word8VectorSlice.slice = Word8ArraySlice.vector_slice
              end = struct
    local
        structure Prim : MONO_SEQUENCE_PRIM = struct
        type elem = Word8.word
        type vector = String.string
        type array = Word8.word Array.array
        structure MonoVector = struct
        val maxLen = String.maxSize
        val length = String.size
        fun unsafeSub (v, i) = Word8.fromInt (Char.ord (String.sub (v, i))) (* TODO *)
        fun fromList xs = String.implode (List.map (Char.chr o Word8.toInt) xs)
        fun unsafeFromListN (n, xs) = fromList xs (* TODO *)
        fun unsafeFromListRevN (n, xs) = String.implodeRev (List.map (Char.chr o Word8.toInt) xs) (* TODO *)
        val concat = String.concat
        fun sliceToVector { base, start, length } = String.substring (base, start, length)
        val shallowSliceToVector = sliceToVector
        end
        structure MonoArray = struct
        val maxLen = Array.maxLen
        val eq = op = : array * array -> bool
        val length = Array.length
        fun unsafeCreateWithZero n = Array.array (n, 0w0 : Word8.word) (* TODO *)
        val unsafeCreate = Array.array (* TODO *)
        val fromList = Array.fromList
        fun unsafeFromListN (n, xs) = fromList xs (* TODO *)
        val unsafeSub = Unsafe.Array.sub
        val unsafeUpdate = Unsafe.Array.update
        end
        end
        structure Base = MonoSequence (Prim)
    in
    structure Word8Vector = Base.MonoVector
    structure Word8VectorSlice = Base.MonoVectorSlice
    structure Word8Array = Base.MonoArray
    structure Word8ArraySlice = Base.MonoArraySlice
    structure UnsafeWord8Vector = Base.UnsafeMonoVector
    structure UnsafeWord8Array = Base.UnsafeMonoArray
    structure ByteImpl = struct
    fun bytesToString x = x
    fun stringToBytes x = x
    fun unpackStringVec { base, start, length } = String.substring (base, start, length)
    fun unpackString { base, start, length } = CharVector.tabulate (length, fn i => Char.chr (Word8.toInt (Unsafe.Array.sub (base, start + i))))
    fun packString (arr, i, s) = let val length = Substring.size s
                                 in if i < 0 orelse length + i > Array.length arr then
                                        raise Subscript
                                    else
                                        let fun go j = if j < length then
                                                           ( Unsafe.Array.update (arr, i + j, Word8.fromInt (Char.ord (Substring.sub (s, i)))); go (j + 1) )
                                                       else
                                                           ()
                                        in go 0
                                        end
                                 end
    end (* structure Word8VectorExtra *)
    end (* local *)
    end (* structure Word8Sequence *)
in
open Word8Sequence
end;
