structure String = struct
type string = string
fun x ^ y = _primCall "String.^" (x, y)
fun size x = _primCall "String.size" (x)
fun str x = _primCall "String.str" (x)
fun sub (s : string, i : int) : char = if i < 0 orelse size s <= i then
                                           raise Subscript
                                       else
                                           JavaScript.unsafeFromValue (JavaScript.sub (JavaScript.unsafeToValue s, JavaScript.fromInt i)) : char
fun concat xs = _primCall "String.concat" (xs)
fun implode xs = _primCall "String.implode" (xs)
fun translate (f : char -> string) (s : string) : string
    = let val n = size s
          fun go i = if i >= n then
                         []
                     else
                         f (sub (s, i)) :: go (i + 1)
      in concat (go 0)
      end
fun map (f : char -> char) (s : string) : string
    = let val n = size s
          fun go i = if i >= n then
                         []
                     else
                         f (sub (s, i)) :: go (i + 1)
      in implode (go 0)
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
                            , maxOrd = 255
                            };
