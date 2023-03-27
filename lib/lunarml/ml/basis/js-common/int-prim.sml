structure Int = struct
type int = int
fun ~ x = _primCall "Int.~" (x)
fun abs x = _primCall "Int.abs" (x)
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
                      , minInt = ~0x8000_0000
                      , maxInt = 0x7fff_ffff
                      };
