_equality int = fn (x, y) => _primCall "Int.=" (x, y);
structure Int = struct
type int = int
fun x < y = _primCall "Int.<" (x, y)
fun x <= y = _primCall "Int.<=" (x, y)
fun x > y = _primCall "Int.>" (x, y)
fun x >= y = _primCall "Int.>=" (x, y)
val MIN : int = ~0x8000_0000
fun ~ (x : int) =
  if x = MIN then
    raise Overflow
  else
    _primCall "Int.~.wrapping" (x)
fun abs (x : int) =
  if x >= 0 then
    x
  else
    ~x
fun x + y =
  let val z = _primCall "Int.+.wrapping" (x, y)
  in
    if (y > 0 andalso z < x) orelse (y < 0 andalso z > x) then
      raise Overflow
    else
      z
  end
fun x - y =
  let val z = _primCall "Int.-.wrapping" (x, y)
  in
    if (y < 0 andalso z < x) orelse (y > 0 andalso z > x) then
      raise Overflow
    else
      z
  end
fun x * y =
  let val z = _primCall "Int64.*.wrapping" (_primCall "Int.toInt64.unchecked" (x), _primCall "Int.toInt64.unchecked" (y))
  in
    if _primCall "Int64.<" (z, ~0x8000_0000) orelse _primCall "Int64.>" (z, 0x7fff_ffff) then
      raise Overflow
    else
      _primCall "Int64.toInt.unchecked" (z)
  end
fun quot (x, y) =
  if y = 0 then
    raise Div
  else if x = MIN andalso y = ~1 then
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
  else if x = MIN andalso y = ~1 then
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
