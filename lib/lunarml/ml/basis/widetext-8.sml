structure WideTextImpl :> sig
              include TEXT
              structure UnsafeCharVector : UNSAFE_MONO_VECTOR where type elem = Char.char where type vector = CharVector.vector
              structure UnsafeCharArray : UNSAFE_MONO_ARRAY where type elem = Char.char where type array = CharArray.array
          end = struct
structure UnsafeCharVector = UnsafeCharVector
structure UnsafeCharArray = UnsafeCharArray
open Text
end;
structure WideText : TEXT = WideTextImpl;
structure WideChar = WideText.Char;
structure WideString = WideText.String;
structure WideSubstring = WideText.Substring;
structure WideCharVector = WideText.CharVector;
structure WideCharArray = WideText.CharArray;
structure WideCharVectorSlice = WideText.CharVectorSlice;
structure WideCharArraySlice = WideText.CharArraySlice;
structure UnsafeWideCharVector = WideTextImpl.UnsafeCharVector
structure UnsafeWideCharArray = WideTextImpl.UnsafeCharArray;
_overload "Char" [WideChar.char] { < = WideChar.<
                                 , <= = WideChar.<=
                                 , > = WideChar.>
                                 , >= = WideChar.>=
                                 , maxOrd = 0xff
                                 };
_overload "String" [WideString.string] { < = WideString.<
                                       , <= = WideString.<=
                                       , > = WideString.>
                                       , >= = WideString.>=
                                       , maxOrd = 0xff
                                       };
