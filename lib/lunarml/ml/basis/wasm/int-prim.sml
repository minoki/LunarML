_equality int = fn (x, y) => _primCall "Int.=" (x, y);
structure Int = struct
type int = int
fun ~ x = _primCall "Int.~" (x)
fun abs x = _primCall "Int.abs" (x)
fun x + y = _primCall "Int.+" (x, y)
fun x - y = _primCall "Int.-" (x, y)
fun x * y = _primCall "Int.*" (x, y)
fun x < y = _primCall "Int.<" (x, y)
fun x <= y = _primCall "Int.<=" (x, y)
fun x > y = _primCall "Int.>" (x, y)
fun x >= y = _primCall "Int.>=" (x, y)
fun quot (x, y) =
  if y = 0 then
    raise Div
  else if x = ~0x8000_0000 andalso y = ~1 then
    raise Overflow
  else
    _primCall "Int.quot.unchecked" (x, y)
fun rem (x, y) =
  if y = 0 then
    raise Div
  else
    _primCall "Int.rem.unchecked" (x, y)
fun x div y =
  if y = 0 then
    raise Div
  else if x = ~0x8000_0000 andalso y = ~1 then
    raise Overflow
  else
    let val r = _primCall "Int.rem.unchecked" (x, y)
    in if (x >= 0 andalso y > 0) orelse (x <= 0 andalso y < 0) orelse r = 0 then
         _primCall "Int.quot.unchecked" (x, y)
       else
         _primCall "Int.-.wrapping" (_primCall "Int.quot.unchecked" (x, y), 1)
    end
fun x mod y =
  if y = 0 then
    raise Div
  else
    let val r = _primCall "Int.rem.unchecked" (x, y)
    in if (x >= 0 andalso y > 0) orelse (x <= 0 andalso y < 0) orelse r = 0 then
         r
       else
         _primCall "Int.+.wrapping" (r, y)
    end
fun fromInt (x : int) = x
end
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
