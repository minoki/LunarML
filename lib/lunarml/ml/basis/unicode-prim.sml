structure UChar = struct
type char = _Prim.UChar.char
fun x < y = _primCall "UChar.<" (x, y)
fun x <= y = _primCall "UChar.<=" (x, y)
fun x > y = _primCall "UChar.>" (x, y)
fun x >= y = _primCall "UChar.>=" (x, y)
end
_equality UChar.char = fn (x, y) => _primCall "UChar.=" (x, y);
_overload "Char" [UChar.char] { < = UChar.<
                              , <= = UChar.<=
                              , > = UChar.>
                              , >= = UChar.>=
                              , maxOrd = 0x10ffff
                              };

structure UString = struct
type string = _Prim.UString.string
type char = UChar.char
fun x < y = _primCall "UString.<" (x, y)
fun x <= y = _primCall "UString.<=" (x, y)
fun x > y = _primCall "UString.>" (x, y)
fun x >= y = _primCall "UString.>=" (x, y)
fun x ^ y = _primCall "UString.^" (x, y)
(* fun size x = _primCall "UString.size" (x) *)
end
_equality UString.string = fn (x, y) => _primCall "UString.=" (x, y);
_overload "String" [UString.string] { < = UString.<
                                    , <= = UString.<=
                                    , > = UString.>
                                    , >= = UString.>=
                                    , maxOrd = 0x10ffff
                                    };
