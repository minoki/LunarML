structure Char16 = struct
type char = _Prim.Char16.char
fun x < y = _primCall "Char16.<" (x, y)
fun x <= y = _primCall "Char16.<=" (x, y)
fun x > y = _primCall "Char16.>" (x, y)
fun x >= y = _primCall "Char16.>=" (x, y)
end
_equality Char16.char = fn (x, y) => _primCall "Char16.=" (x, y);
_overload "Char" [Char16.char] { < = Char16.<
                               , <= = Char16.<=
                               , > = Char16.>
                               , >= = Char16.>=
                               , maxOrd = 0xffff
                               };

structure String16 = struct
type string = _Prim.String16.string
type char = Char16.char
fun x < y = _primCall "String16.<" (x, y)
fun x <= y = _primCall "String16.<=" (x, y)
fun x > y = _primCall "String16.>" (x, y)
fun x >= y = _primCall "String16.>=" (x, y)
fun x ^ y = _primCall "String16.^" (x, y)
fun size x = _primCall "String16.size" (x)
end
_equality String16.string = fn (x, y) => _primCall "String16.=" (x, y);
_overload "String" [String16.string] { < = String16.<
                                     , <= = String16.<=
                                     , > = String16.>
                                     , >= = String16.>=
                                     , maxOrd = 0xffff
                                     };
