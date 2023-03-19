signature TEXT = sig
    structure Char : CHAR
    structure String : STRING
    structure Substring : SUBSTRING
    structure CharVector : MONO_VECTOR
    structure CharArray : MONO_ARRAY
    structure CharVectorSlice : MONO_VECTOR_SLICE
    structure CharArraySlice : MONO_ARRAY_SLICE
    sharing type Char.char = String.char = Substring.char
                             = CharVector.elem = CharArray.elem
                             = CharVectorSlice.elem = CharArraySlice.elem
    sharing type Char.string = String.string = Substring.string
                               = CharVector.vector = CharArray.vector
                               = CharVectorSlice.vector = CharArraySlice.vector
    sharing type CharArray.array = CharArraySlice.array
    sharing type CharVectorSlice.slice = CharArraySlice.vector_slice
end;

structure Text :> TEXT where type Char.char = Char.char
                       where type String.string = String.string
                       where type Substring.substring = Substring.substring
                       where type CharArray.array = CharArray.array
                       where type CharVectorSlice.slice = CharVectorSlice.slice
                       where type CharArraySlice.slice = CharArraySlice.slice
  = struct
structure Char = Char
structure String = String
structure Substring = Substring
structure CharVector = CharVector
structure CharArray = CharArray
structure CharVectorSlice = CharVectorSlice
structure CharArraySlice = CharArraySlice
end;
