structure WideChar = struct
type char = _Prim.WideChar.char
fun x < y = _primCall "WideChar.<" (x, y)
fun x <= y = _primCall "WideChar.<=" (x, y)
fun x > y = _primCall "WideChar.>" (x, y)
fun x >= y = _primCall "WideChar.>=" (x, y)
end
_equality WideChar.char = fn (x, y) => _primCall "WideChar.=" (x, y);
_overload "Char" [WideChar.char] { < = WideChar.<
                                 , <= = WideChar.<=
                                 , > = WideChar.>
                                 , >= = WideChar.>=
                                 };
