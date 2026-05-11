exception Fail of string
signature ARRAY = sig end
signature ARRAY_SLICE = sig end
signature BOOL = sig end
signature BYTE = sig end
signature CHAR = sig end
signature DATE = sig end
signature INT_INF = sig end
signature LIST = sig end
signature MONO_ARRAY = sig end
signature MONO_ARRAY_SLICE = sig end
signature MONO_VECTOR = sig end
signature MONO_VECTOR_SLICE = sig end
signature PACK_REAL = sig end
signature PACK_WORD = sig end
signature REAL = sig end
signature STRING = sig end
signature SUBSTRING = sig end
signature TEXT = sig end
signature TIME = sig end
signature TIMER = sig end
signature UNSAFE_MONO_ARRAY = sig end
signature UNSAFE_MONO_VECTOR = sig end
signature VECTOR = sig end
signature VECTOR_SLICE = sig end
structure ArraySlice = struct end
structure BinIO = struct end
structure BoolArray = struct end
structure BoolArraySlice = struct end
structure BoolVector = struct end
structure BoolVectorSlice = struct end
structure Byte = struct end
structure Char = struct
  val chr : int -> char = fn _ => raise Fail "Char.chr: not implemented yet"
  val ord : char -> int = fn _ => raise Fail "Char.ord: not implemented yet"
  open Char
end
structure CharArraySlice = struct end
structure CharVector = struct end
structure CharVectorSlice = struct end
structure CommandLine = struct end
structure Date = struct end
structure Int16Array = struct end
structure Int16ArraySlice = struct end
structure Int16Vector = struct end
structure Int16VectorSlice = struct end
structure Int32Array = struct end
structure Int32ArraySlice = struct end
structure Int32Vector = struct end
structure Int32VectorSlice = struct end
structure Int64Array = struct end
structure Int64ArraySlice = struct end
structure Int64Vector = struct end
structure Int64VectorSlice = struct end
structure Int8Array = struct end
structure Int8ArraySlice = struct end
structure Int8Vector = struct end
structure Int8VectorSlice = struct end
structure IntArray = struct end
structure IntArraySlice = struct end
structure IntVector = struct end
structure IntVectorSlice = struct end
structure IntInf = struct end
structure LargeInt = struct end
structure LargeReal = struct end
structure Math = struct end
structure OS = struct end
structure PackRealBig = struct end
structure PackRealLittle = struct end
structure PackWord16Big = struct end
structure PackWord16Little = struct end
structure PackWord32Big = struct end
structure PackWord32Little = struct end
structure PackWord64Big = struct end
structure PackWord64Little = struct end
structure PackWord8Big = struct end
structure PackWord8Little = struct end
structure Real = struct
  val ceil : real -> int = fn _ => raise Fail "Real.ceil: not implemented yet"
  val floor : real -> int = fn _ => raise Fail "Real.floor: not implemented yet"
  val round : real -> int = fn _ => raise Fail "Real.round: not implemented yet"
  val trunc : real -> int = fn _ => raise Fail "Real.trunc: not implemented yet"
  val fromInt : int -> real = fn _ => raise Fail "Real.fromInt: not implemented yet"
  open Real
end
structure RealArray = struct end
structure RealArraySlice = struct end
structure RealVector = struct end
structure RealVectorSlice = struct end
structure String = struct
  val explode : string -> char list = fn _ => raise Fail "String.explode: not implemented yet"
  val substring : string * int * int -> string = fn _ => raise Fail "String.substring: not implemented yet"
  open String
end
structure Substring = struct type substring = unit end
structure Text = struct end
structure Time = struct end
structure Timer = struct end
structure Vector = struct
  datatype 'a vector = VECTOR_STUB
  val fromList : 'a list -> 'a vector = fn _ => raise Fail "Vector.fromList: not implemented yet"
end
structure VectorSlice = struct end
structure WideChar = struct end
structure WideCharArray = struct end
structure WideCharArraySlice = struct end
structure WideCharVector = struct end
structure WideCharVectorSlice = struct end
structure WideString = struct end
structure WideSubstring = struct end
structure WideText = struct end
structure Word16Array = struct end
structure Word16ArraySlice = struct end
structure Word16Vector = struct end
structure Word16VectorSlice = struct end
structure Word32Array = struct end
structure Word32ArraySlice = struct end
structure Word32Vector = struct end
structure Word32VectorSlice = struct end
structure Word64Array = struct end
structure Word64ArraySlice = struct end
structure Word64Vector = struct end
structure Word64VectorSlice = struct end
structure Word8Vector = struct end
structure Word8VectorSlice = struct end
structure Word8Array = struct end
structure Word8ArraySlice = struct end
structure WordArray = struct end
structure WordArraySlice = struct end
structure WordVector = struct end
structure WordVectorSlice = struct end
