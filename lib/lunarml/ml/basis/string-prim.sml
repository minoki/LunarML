structure String = struct
type string = string
fun x < y = _primCall "String.<" (x, y)
fun x <= y = _primCall "String.<=" (x, y)
fun x > y = _primCall "String.>" (x, y)
fun x >= y = _primCall "String.>=" (x, y)
fun x ^ y = _primCall "String.^" (x, y)
fun size x = _primCall "String.size" (x)
fun str (x : char) : string = _primCall "String.str" (x)
end
_equality string = fn (x, y) => _primCall "String.=" (x, y);
_overload "String" [string] { < = String.<
                            , <= = String.<=
                            , > = String.>
                            , >= = String.>=
                            , maxOrd = 255
                            };
