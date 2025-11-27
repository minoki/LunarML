structure Char7 = struct
type char = _Prim.Char7.char
fun x < y = _primCall "Char7.<" (x, y)
fun x <= y = _primCall "Char7.<=" (x, y)
fun x > y = _primCall "Char7.>" (x, y)
fun x >= y = _primCall "Char7.>=" (x, y)
end
_equality Char7.char = fn (x, y) => _primCall "Char7.=" (x, y);
_overload "Char" [Char7.char] { < = Char7.<
                              , <= = Char7.<=
                              , > = Char7.>
                              , >= = Char7.>=
                              , maxOrd = 127
                              };

structure String7 = struct
type string = _Prim.String7.string
type char = Char7.char
fun x < y = _primCall "String7.<" (x, y)
fun x <= y = _primCall "String7.<=" (x, y)
fun x > y = _primCall "String7.>" (x, y)
fun x >= y = _primCall "String7.>=" (x, y)
fun x ^ y = _primCall "String7.^" (x, y)
fun size x = _primCall "String7.size" (x)
end
_equality String7.string = fn (x, y) => _primCall "String7.=" (x, y);
_overload "String" [String7.string] { < = String7.<
                                    , <= = String7.<=
                                    , > = String7.>
                                    , >= = String7.>=
                                    , maxOrd = 127
                                    };
