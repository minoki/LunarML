structure Int = struct
type int = int
val ~ = _Prim.Int.~
val abs = _Prim.Int.abs
fun x + y = _primCall "Int.+" (x, y)
fun x - y = _primCall "Int.-" (x, y)
fun x * y = _primCall "Int.*" (x, y)
fun x div y = _primCall "Int.div" (x, y)
fun x mod y = _primCall "Int.mod" (x, y)
fun x < y = _primCall "Int.<" (x, y)
fun x <= y = _primCall "Int.<=" (x, y)
fun x > y = _primCall "Int.>" (x, y)
fun x >= y = _primCall "Int.>=" (x, y)
fun fromInt (x : int) = x
end
_equality int = fn (x, y) => _primCall "Int.=" (x, y);
_overload "Int" [int] { + = Int.+
                      , - = Int.-
                      , * = Int.*
                      , div = Int.div
                      , mod = Int.mod
                      , ~ = Int.~
                      , abs = Int.abs
                      , < = Int.<
                      , <= = Int.<=
                      , > = Int.>
                      , >= = Int.>=
                      , fromInt = Int.fromInt
                      , minInt = ~0x8000000000000000
                      , maxInt = 0x7fffffffffffffff
                      };
