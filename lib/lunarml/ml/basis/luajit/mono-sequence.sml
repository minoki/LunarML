functor FFIMonoSequence (type elem; val ctype : string) :> sig
            structure MonoVector : MONO_VECTOR where type elem = elem
            structure MonoVectorSlice : MONO_VECTOR_SLICE where type elem = elem
            structure MonoArray : MONO_ARRAY_NOEQTYPE where type elem = elem
            structure MonoArraySlice : MONO_ARRAY_SLICE where type elem = elem
            structure UnsafeMonoVector : UNSAFE_MONO_VECTOR where type elem = elem
            structure UnsafeMonoArray : UNSAFE_MONO_ARRAY where type elem = elem
            sharing type MonoVector.vector = MonoVectorSlice.vector = MonoArray.vector = MonoArraySlice.vector = UnsafeMonoVector.vector
            sharing type MonoVectorSlice.slice = MonoArraySlice.vector_slice
            sharing type MonoArray.array = MonoArraySlice.array = UnsafeMonoArray.array
            val arrayEq : MonoArray.array * MonoArray.array -> bool
        end = struct
val ffi = LunarML.assumeDiscardable (fn () => Lua.call1 Lua.Lib.require #[Lua.fromString "ffi"]) ()
val arraytype = LunarML.assumeDiscardable (fn () => Lua.call1 (Lua.field (ffi, "typeof")) #[Lua.fromString (ctype ^ "[?]")]) ()
val elemSize : int = LunarML.assumeDiscardable (fn () => Lua.unsafeFromValue (Lua.call1 (Lua.field (ffi, "sizeof")) #[Lua.fromString ctype])) ()
structure P = struct
type elem = elem
type vector = Lua.value * int
type array = Lua.value * int
structure MonoVector = struct
val maxLen = 0x7fffffff (* not really used *)
fun length ((_, n) : vector) : int = n
fun unsafeSub ((v, _) : vector, i) : elem = Lua.unsafeFromValue (Lua.sub (v, Lua.fromInt i)) (* zero-based index *)
fun unsafeFromListN (n, xs : elem list) = let val v = Lua.call1 arraytype #[Lua.fromInt n]
                                              fun go (i, []) = (v, n)
                                                | go (i, x :: xs) = (Lua.set (v, Lua.fromInt i, Lua.unsafeToValue x); go (i + 1, xs))
                                          in go (0, xs)
                                          end
fun unsafeFromListRevN (n, xs : elem list) = let val v = Lua.call1 arraytype #[Lua.fromInt n]
                                                 fun go (i, []) = (v, n)
                                                   | go (i, x :: xs) = (Lua.set (v, Lua.fromInt i, Lua.unsafeToValue x); go (i - 1, xs))
                                             in go (n - 1, xs)
                                             end
fun fromList xs = unsafeFromListN (List.length xs, xs)
fun concat vs = let val n = List.foldl (fn ((_, m), n) => n + m) 0 vs
                    val w = Lua.call1 arraytype #[Lua.fromInt n]
                    val copy = Lua.field (ffi, "copy")
                    fun go (i, []) = (w, n)
                      | go (i, (src, m) :: vs) = let val dst = Lua.+ (w, Lua.fromInt i)
                                                 in Lua.call0 copy #[dst, src, Lua.fromInt (elemSize * m)]
                                                  ; go (i + m, vs)
                                                 end
                in go (0, vs)
                end
fun sliceToVector { base = (v, _) : vector, start, length }
    = let val dst = Lua.call1 arraytype #[Lua.fromInt length]
          val copy = Lua.field (ffi, "copy")
      in Lua.call0 copy #[dst, Lua.+ (v, Lua.fromInt start), Lua.fromInt length]
       ; (dst, length)
      end
val shallowSliceToVector = sliceToVector
end
structure MonoArray = struct
val maxLen = 0x7fffffff (* not really used *)
fun eq ((a, _) : array, (a', _) : array) = Lua.== (a, a')
fun length ((_, n) : array) : int = n
fun unsafeCreateWithZero n : array = (Lua.call1 arraytype #[Lua.fromInt n], n)
fun unsafeCreate (n, x : elem) : array = (Lua.call1 arraytype #[Lua.fromInt n, Lua.unsafeToValue x], n)
val fromList = MonoVector.fromList
val unsafeFromListN = MonoVector.unsafeFromListN
val unsafeSub = MonoVector.unsafeSub
fun unsafeUpdate ((a, _), i, x : elem) = Lua.set (a, Lua.fromInt i, Lua.unsafeToValue x)
end
end
structure S = MonoSequence (P)
open S
val arrayEq = P.MonoArray.eq
end;

local
    structure S = FFIMonoSequence (type elem = bool; val ctype = "bool")
    _equality S.MonoArray.array = S.arrayEq
in
structure BoolVector = S.MonoVector
structure BoolVectorSlice = S.MonoVectorSlice
structure BoolArray = S.MonoArray
structure BoolArraySlice = S.MonoArraySlice
structure UnsafeBoolVector = S.UnsafeMonoVector
structure UnsafeBoolArray = S.UnsafeMonoArray
end;

local
    structure S = FFIMonoSequence (type elem = int; val ctype = "int32_t")
    _equality S.MonoArray.array = S.arrayEq
in
structure IntVector = S.MonoVector
structure IntVectorSlice = S.MonoVectorSlice
structure IntArray = S.MonoArray
structure IntArraySlice = S.MonoArraySlice
structure UnsafeIntVector = S.UnsafeMonoVector
structure UnsafeIntArray = S.UnsafeMonoArray
end;

local
    structure S = FFIMonoSequence (type elem = Int8.int; val ctype = "int8_t")
    _equality S.MonoArray.array = S.arrayEq
in
structure Int8Vector = S.MonoVector
structure Int8VectorSlice = S.MonoVectorSlice
structure Int8Array = S.MonoArray
structure Int8ArraySlice = S.MonoArraySlice
structure UnsafeInt8Vector = S.UnsafeMonoVector
structure UnsafeInt8Array = S.UnsafeMonoArray
end;

local
    structure S = FFIMonoSequence (type elem = Int16.int; val ctype = "int16_t")
    _equality S.MonoArray.array = S.arrayEq
in
structure Int16Vector = S.MonoVector
structure Int16VectorSlice = S.MonoVectorSlice
structure Int16Array = S.MonoArray
structure Int16ArraySlice = S.MonoArraySlice
structure UnsafeInt16Vector = S.UnsafeMonoVector
structure UnsafeInt16Array = S.UnsafeMonoArray
end;

local
    structure S = FFIMonoSequence (type elem = Int32.int; val ctype = "int32_t")
    _equality S.MonoArray.array = S.arrayEq
in
structure Int32Vector = S.MonoVector
structure Int32VectorSlice = S.MonoVectorSlice
structure Int32Array = S.MonoArray
structure Int32ArraySlice = S.MonoArraySlice
structure UnsafeInt32Vector = S.UnsafeMonoVector
structure UnsafeInt32Array = S.UnsafeMonoArray
end;

local
    structure S = FFIMonoSequence (type elem = Int64.int; val ctype = "int64_t")
    _equality S.MonoArray.array = S.arrayEq
in
structure Int64Vector = S.MonoVector
structure Int64VectorSlice = S.MonoVectorSlice
structure Int64Array = S.MonoArray
structure Int64ArraySlice = S.MonoArraySlice
structure UnsafeInt64Vector = S.UnsafeMonoVector
structure UnsafeInt64Array = S.UnsafeMonoArray
end;

local
    structure S = FFIMonoSequence (type elem = word; val ctype = "uint32_t")
    _equality S.MonoArray.array = S.arrayEq
in
structure WordVector = S.MonoVector
structure WordVectorSlice = S.MonoVectorSlice
structure WordArray = S.MonoArray
structure WordArraySlice = S.MonoArraySlice
structure UnsafeWordVector = S.UnsafeMonoVector
structure UnsafeWordArray = S.UnsafeMonoArray
end;

local
    structure S = FFIMonoSequence (type elem = Word16.word; val ctype = "uint16_t")
    _equality S.MonoArray.array = S.arrayEq
in
structure Word16Vector = S.MonoVector
structure Word16VectorSlice = S.MonoVectorSlice
structure Word16Array = S.MonoArray
structure Word16ArraySlice = S.MonoArraySlice
structure UnsafeWord16Vector = S.UnsafeMonoVector
structure UnsafeWord16Array = S.UnsafeMonoArray
end;

local
    structure S = FFIMonoSequence (type elem = Word32.word; val ctype = "uint32_t")
    _equality S.MonoArray.array = S.arrayEq
in
structure Word32Vector = S.MonoVector
structure Word32VectorSlice = S.MonoVectorSlice
structure Word32Array = S.MonoArray
structure Word32ArraySlice = S.MonoArraySlice
structure UnsafeWord32Vector = S.UnsafeMonoVector
structure UnsafeWord32Array = S.UnsafeMonoArray
end;

local
    structure S = FFIMonoSequence (type elem = Word64.word; val ctype = "uint64_t")
    _equality S.MonoArray.array = S.arrayEq
in
structure Word64Vector = S.MonoVector
structure Word64VectorSlice = S.MonoVectorSlice
structure Word64Array = S.MonoArray
structure Word64ArraySlice = S.MonoArraySlice
structure UnsafeWord64Vector = S.UnsafeMonoVector
structure UnsafeWord64Array = S.UnsafeMonoArray
end;

local
    structure S = FFIMonoSequence (type elem = real; val ctype = "double")
    _equality S.MonoArray.array = S.arrayEq
in
structure RealVector = S.MonoVector
structure RealVectorSlice = S.MonoVectorSlice
structure RealArray = S.MonoArray
structure RealArraySlice = S.MonoArraySlice
structure UnsafeRealVector = S.UnsafeMonoVector
structure UnsafeRealArray = S.UnsafeMonoArray
end;
