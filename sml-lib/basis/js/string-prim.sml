structure String = struct
type string = string
fun x ^ y = _primCall "String.^" (x, y)
fun size x = _primCall "String.size" (x)
fun str x = _primCall "String.str" (x)
fun sub (s : string, i : int) : char = if i < 0 orelse size s <= i then
                                           raise Subscript
                                       else
                                           JavaScript.unsafeFromValue (JavaScript.sub (JavaScript.unsafeToValue s, JavaScript.fromInt i)) : char
val concat : string list -> string = _Prim.String.concat
val implode : char list -> string = _Prim.String.implode
fun translate (f : char -> string) (s : string) : string = _primCall "call2" (_Prim.String.translate, f, s)
fun map (f : char -> char) (s : string) : string = let val s = JavaScript.unsafeToValue s
                                                   in JavaScript.unsafeFromValue (JavaScript.method (s, "map") #[JavaScript.unsafeToValue f])
                                                   end
fun x < y = _primCall "String.<" (x, y)
fun x <= y = Bool.not (y < x)
fun x > y = y < x
fun x >= y = Bool.not (x < y)
end
_equality string = fn (x, y) => _primCall "String.=" (x, y);
_overload "String" [string] { < = String.<
                            , <= = String.<=
                            , > = String.>
                            , >= = String.>=
                            };
