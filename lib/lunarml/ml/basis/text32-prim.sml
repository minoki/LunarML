structure Char32 = struct
type char = _Prim.Char32.char
fun x < y = _primCall "Char32.<" (x, y)
fun x <= y = _primCall "Char32.<=" (x, y)
fun x > y = _primCall "Char32.>" (x, y)
fun x >= y = _primCall "Char32.>=" (x, y)
end
_equality Char32.char = fn (x, y) => _primCall "Char32.=" (x, y);
_overload "Char" [Char32.char] { < = Char32.<
                               , <= = Char32.<=
                               , > = Char32.>
                               , >= = Char32.>=
                               , maxOrd = 0x10ffff
                               };

structure String32 = struct
type string = _Prim.String32.string
type char = Char32.char
fun x < y = _primCall "String32.<" (x, y)
fun x <= y = _primCall "String32.<=" (x, y)
fun x > y = _primCall "String32.>" (x, y)
fun x >= y = _primCall "String32.>=" (x, y)
fun x ^ y = _primCall "String32.^" (x, y)
fun size x = _primCall "String32.size" (x)
end
_equality String32.string = fn (x, y) => _primCall "String32.=" (x, y);
_overload "String" [String32.string] { < = String32.<
                                     , <= = String32.<=
                                     , > = String32.>
                                     , >= = String32.>=
                                     , maxOrd = 0x10ffff
                                     };
