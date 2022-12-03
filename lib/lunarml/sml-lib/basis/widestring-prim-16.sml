structure WideString = struct
type string = _Prim.String16.string
type char = WideChar.char
fun x < y = _primCall "String16.<" (x, y)
fun x <= y = _primCall "String16.<=" (x, y)
fun x > y = _primCall "String16.>" (x, y)
fun x >= y = _primCall "String16.>=" (x, y)
fun x ^ y = _primCall "String16.^" (x, y)
fun size x = _primCall "String16.size" (x)
fun str (x : char) : string = _primCall "Unsafe.cast" (x)
end
_equality WideString.string = fn (x, y) => _primCall "String16.=" (x, y);
_overload "String" [WideString.string] { < = WideString.<
                                       , <= = WideString.<=
                                       , > = WideString.>
                                       , >= = WideString.>=
                                       };
