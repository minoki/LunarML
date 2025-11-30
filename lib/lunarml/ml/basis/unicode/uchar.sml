_equality _Prim.UChar.char = fn (x, y) => _primCall "UChar.=" (x, y);
structure UChar : sig
  type uchar = _Prim.UChar.char
  datatype ('char, 'substring) decode_result
    = SCALAR of uchar * 'substring
    | INVALID of 'char * 'substring
    | END
  val < : uchar * uchar -> bool
  val <= : uchar * uchar -> bool
  val > : uchar * uchar -> bool
  val >= : uchar * uchar -> bool
  val fromAscii : Char7.char -> uchar
  val toChar32 : uchar -> Char32.char
  val ord : uchar -> int
  val chr : int -> uchar
  structure Unsafe : sig
    val chr : int -> uchar
    val fromChar32 : Char32.char -> uchar
  end
end = struct
type uchar = _Prim.UChar.char
datatype ('char, 'substring) decode_result
  = SCALAR of uchar * 'substring
  | INVALID of 'char * 'substring
  | END
fun fromAscii x = _primCall "UChar.fromChar7" (x)
fun toChar32 x = _primCall "Char32.fromUChar" (x)
fun ord x = _primCall "UChar.ord" (x)
fun chr x = if (0 <= x andalso x < 0xD800) orelse (0xE000 <= x andalso x < 0x110000) then
              _primCall "UChar.chr.unchecked" (x)
            else
              raise Chr
structure Unsafe = struct
fun chr x = _primCall "UChar.chr.unchecked" (x)
fun fromChar32 c = chr (Char32.ord c)
end
fun x < y = _primCall "UChar.<" (x, y)
fun x <= y = _primCall "UChar.<=" (x, y)
fun x > y = _primCall "UChar.>" (x, y)
fun x >= y = _primCall "UChar.>=" (x, y)
end
_overload "Char" [UChar.uchar] { < = UChar.<
                               , <= = UChar.<=
                               , > = UChar.>
                               , >= = UChar.>=
                               , maxOrd = 0x10ffff
                               };
