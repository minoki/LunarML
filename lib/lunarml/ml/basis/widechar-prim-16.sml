structure WideChar = struct
type char = _Prim.Char16.char
fun x < y = _primCall "Char16.<" (x, y)
fun x <= y = _primCall "Char16.<=" (x, y)
fun x > y = _primCall "Char16.>" (x, y)
fun x >= y = _primCall "Char16.>=" (x, y)
end
_equality WideChar.char = fn (x, y) => _primCall "Char16.=" (x, y);
_overload "Char" [WideChar.char] { < = WideChar.<
                                 , <= = WideChar.<=
                                 , > = WideChar.>
                                 , >= = WideChar.>=
                                 };
