functor GenericMonoSequence (type elem; val zero : elem) :> sig
            structure MonoVector : MONO_VECTOR where type elem = elem
            structure MonoVectorSlice : MONO_VECTOR_SLICE where type elem = elem
            structure MonoArray : MONO_ARRAY where type elem = elem
            structure MonoArraySlice : MONO_ARRAY_SLICE where type elem = elem
            structure UnsafeMonoVector : UNSAFE_MONO_VECTOR where type elem = elem
            structure UnsafeMonoArray : UNSAFE_MONO_ARRAY where type elem = elem
            sharing type MonoVector.vector = MonoVectorSlice.vector = MonoArray.vector = MonoArraySlice.vector = UnsafeMonoVector.vector
            sharing type MonoVectorSlice.slice = MonoArraySlice.vector_slice
            sharing type MonoArray.array = MonoArraySlice.array = UnsafeMonoArray.array
        end = struct
structure MonoVector = struct
type elem = elem
open Vector
type vector = elem Vector.vector
fun toList a = foldr (op ::) [] a
fun append (v, x) = Vector.concat [v, Vector.fromList [x]]
fun prepend (x, v) = Vector.concat [Vector.fromList [x], v]
end
structure MonoVectorSlice = struct
type elem = elem
open VectorSlice
type vector = elem Vector.vector
type slice = elem VectorSlice.slice
end
structure MonoArray = struct
type elem = elem
open Array
type vector = elem Vector.vector
type array = elem Array.array
end
structure MonoArraySlice = struct
type elem = elem
open ArraySlice
type vector = elem Vector.vector
type vector_slice = elem VectorSlice.slice
type array = elem Array.array
type slice = elem ArraySlice.slice
end
structure UnsafeMonoVector = struct
type elem = elem
type vector = elem Vector.vector
fun sub (vec, i) = _primCall "Unsafe.Vector.sub" (vec, i)
end
structure UnsafeMonoArray = struct
type elem = elem
type array = elem Array.array
fun sub (arr, i) = _primCall "Unsafe.Array.sub" (arr, i)
fun update (arr, i, x) = _primCall "Unsafe.Array.update" (arr, i, x)
fun create n = Array.array (n, zero)
end
end;

local
    structure S = GenericMonoSequence (type elem = bool; val zero = false)
in
structure BoolVector = S.MonoVector
structure BoolVectorSlice = S.MonoVectorSlice
structure BoolArray = S.MonoArray
structure BoolArraySlice = S.MonoArraySlice
structure UnsafeBoolVector = S.UnsafeMonoVector
structure UnsafeBoolArray = S.UnsafeMonoArray
end;

local
    structure S = GenericMonoSequence (type elem = Int.int; val zero = 0)
in
structure IntVector = S.MonoVector
structure IntVectorSlice = S.MonoVectorSlice
structure IntArray = S.MonoArray
structure IntArraySlice = S.MonoArraySlice
structure UnsafeIntVector = S.UnsafeMonoVector
structure UnsafeIntArray = S.UnsafeMonoArray
end;

local
    structure S = GenericMonoSequence (type elem = Int8.int; val zero = 0 : Int8.int)
in
structure Int8Vector = S.MonoVector
structure Int8VectorSlice = S.MonoVectorSlice
structure Int8Array = S.MonoArray
structure Int8ArraySlice = S.MonoArraySlice
structure UnsafeInt8Vector = S.UnsafeMonoVector
structure UnsafeInt8Array = S.UnsafeMonoArray
end;

local
    structure S = GenericMonoSequence (type elem = Int16.int; val zero = 0 : Int16.int)
in
structure Int16Vector = S.MonoVector
structure Int16VectorSlice = S.MonoVectorSlice
structure Int16Array = S.MonoArray
structure Int16ArraySlice = S.MonoArraySlice
structure UnsafeInt16Vector = S.UnsafeMonoVector
structure UnsafeInt16Array = S.UnsafeMonoArray
end;

local
    structure S = GenericMonoSequence (type elem = Int32.int; val zero = 0 : Int32.int)
in
structure Int32Vector = S.MonoVector
structure Int32VectorSlice = S.MonoVectorSlice
structure Int32Array = S.MonoArray
structure Int32ArraySlice = S.MonoArraySlice
structure UnsafeInt32Vector = S.UnsafeMonoVector
structure UnsafeInt32Array = S.UnsafeMonoArray
end;

local
    structure S = GenericMonoSequence (type elem = Int64.int; val zero = 0 : Int64.int)
in
structure Int64Vector = S.MonoVector
structure Int64VectorSlice = S.MonoVectorSlice
structure Int64Array = S.MonoArray
structure Int64ArraySlice = S.MonoArraySlice
structure UnsafeInt64Vector = S.UnsafeMonoVector
structure UnsafeInt64Array = S.UnsafeMonoArray
end;

local
    structure S = GenericMonoSequence (type elem = Word.word; val zero = 0w0)
in
structure WordVector = S.MonoVector
structure WordVectorSlice = S.MonoVectorSlice
structure WordArray = S.MonoArray
structure WordArraySlice = S.MonoArraySlice
structure UnsafeWordVector = S.UnsafeMonoVector
structure UnsafeWordArray = S.UnsafeMonoArray
end;

local
    structure S = GenericMonoSequence (type elem = Word16.word; val zero = 0w0 : Word16.word)
in
structure Word16Vector = S.MonoVector
structure Word16VectorSlice = S.MonoVectorSlice
structure Word16Array = S.MonoArray
structure Word16ArraySlice = S.MonoArraySlice
structure UnsafeWord16Vector = S.UnsafeMonoVector
structure UnsafeWord16Array = S.UnsafeMonoArray
end;

local
    structure S = GenericMonoSequence (type elem = Word32.word; val zero = 0w0 : Word32.word)
in
structure Word32Vector = S.MonoVector
structure Word32VectorSlice = S.MonoVectorSlice
structure Word32Array = S.MonoArray
structure Word32ArraySlice = S.MonoArraySlice
structure UnsafeWord32Vector = S.UnsafeMonoVector
structure UnsafeWord32Array = S.UnsafeMonoArray
end;

local
    structure S = GenericMonoSequence (type elem = Word64.word; val zero = 0w0 : Word64.word)
in
structure Word64Vector = S.MonoVector
structure Word64VectorSlice = S.MonoVectorSlice
structure Word64Array = S.MonoArray
structure Word64ArraySlice = S.MonoArraySlice
structure UnsafeWord64Vector = S.UnsafeMonoVector
structure UnsafeWord64Array = S.UnsafeMonoArray
end;

local
    structure S = GenericMonoSequence (type elem = Real.real; val zero = 0.0)
in
structure RealVector = S.MonoVector
structure RealVectorSlice = S.MonoVectorSlice
structure RealArray = S.MonoArray
structure RealArraySlice = S.MonoArraySlice
structure UnsafeRealVector = S.UnsafeMonoVector
structure UnsafeRealArray = S.UnsafeMonoArray
end;
