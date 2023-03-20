structure WideText :> TEXT = Text;
structure WideChar = WideText.Char;
structure WideString = WideText.String;
structure WideSubstring = WideText.Substring;
structure WideCharVector = WideText.CharVector;
structure WideCharArray = WideText.CharArray;
structure WideCharVectorSlice = WideText.CharVectorSlice;
structure WideCharArraySlice = WideText.CharArraySlice;
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
