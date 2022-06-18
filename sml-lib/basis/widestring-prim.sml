structure WideString = struct
type string = _Prim.WideString.string
type char = WideChar.char
fun x < y = _primCall "WideString.<" (x, y)
fun x <= y = _primCall "WideString.<=" (x, y)
fun x > y = _primCall "WideString.>" (x, y)
fun x >= y = _primCall "WideString.>=" (x, y)
fun x ^ y = _primCall "WideString.^" (x, y)
fun size x = _primCall "WideString.size" (x)
fun str (x : char) : string = _primCall "Unsafe.cast" (x)
end
_equality WideString.string = fn (x, y) => _primCall "WideString.=" (x, y);
_overload "String" [WideString.string] { < = WideString.<
                                       , <= = WideString.<=
                                       , > = WideString.>
                                       , >= = WideString.>=
                                       };
