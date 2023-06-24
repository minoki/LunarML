signature WORD8_VECTOR_EXTRA = sig
    include MONO_VECTOR where type elem = Word8.word
    val bytesToString : vector -> string
    val stringToBytes : string -> vector
end

local
    structure Word8Sequence :> sig
                  structure Word8VectorExtra : WORD8_VECTOR_EXTRA
                  structure Word8Vector : MONO_VECTOR where type elem = Word8.word
                  structure Word8VectorSlice : MONO_VECTOR_SLICE where type elem = Word8.word where type vector = Word8Vector.vector
                  structure Word8Array : MONO_ARRAY where type elem = Word8.word
                  structure Word8ArraySlice : MONO_ARRAY_SLICE where type elem = Word8.word
                  structure UnsafeWord8Vector : UNSAFE_MONO_VECTOR where type elem = Word8.word
                  structure UnsafeWord8Array : UNSAFE_MONO_ARRAY where type elem = Word8.word
                  sharing type Word8VectorExtra.vector = Word8Vector.vector = Word8Array.vector = Word8ArraySlice.vector = UnsafeWord8Vector.vector
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
    structure Word8VectorExtra = struct
    fun bytesToString x = x
    fun stringToBytes x = x
    open Word8Vector
    end (* structure Word8VectorExtra *)
    end (* local *)
    end (* structure Word8Sequence *)
in
open Word8Sequence
end;
