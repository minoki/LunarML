structure UTF32 : sig
  type codeunit = Char32.char
  type string = String32.string
  type substring = Substring32.substring
  datatype ('char, 'substring) decode_result
    = SCALAR of UChar.uchar * 'substring
    | INVALID of 'char * 'substring
    | END
  val isWellFormed : string -> bool
  val isWellFormedSubstring : substring -> bool
  val encode1 : UChar.uchar -> string
  val decode1 : substring -> (codeunit, substring) decode_result
  val decode1Replace : substring -> (UChar.uchar * substring) option (* return U+FFFD on error *)
  val decode1Raise : substring -> (UChar.uchar * substring) option (* raises Chr on error *)
  structure Unsafe : sig
    val decode1 : substring -> (UChar.uchar * substring) option (* unchecked *)
  end
end = struct
type codeunit = Char32.char
type string = String32.string
type substring = Substring32.substring
datatype decode_result = datatype UChar.decode_result
fun check1 (c : Char32.char) = c < #"\uD800" orelse #"\uE000" <= c
fun isWellFormed x = Char32Vector.all check1 x
fun isWellFormedSubstring (x : substring) = Char32VectorSlice.all check1 x
fun encode1 x = String32.str (UChar.toChar32 x)
fun decode1 s =
  case Substring32.getc s of
    NONE => END
  | SOME (c, s') =>
      if c < #"\uD800" orelse #"\uE000" <= c then
        SCALAR (UChar.Unsafe.chr (Char32.ord c), s')
      else
        INVALID (c, s')
fun decode1Replace s =
  case Substring32.getc s of
    NONE => NONE
  | SOME (c, s') =>
      if c < #"\uD800" orelse #"\uE000" <= c then
        SOME (UChar.Unsafe.chr (Char32.ord c), s')
      else
        SOME (#"\uFFFD", s')
fun decode1Raise s =
  case Substring32.getc s of
    NONE => NONE
  | SOME (c, s') =>
      if c < #"\uD800" orelse #"\uE000" <= c then
        SOME (UChar.Unsafe.chr (Char32.ord c), s')
      else
        raise Chr
structure Unsafe = struct
fun decode1 s =
  case Substring32.getc s of
    NONE => NONE
  | SOME (c, s') => SOME (UChar.Unsafe.chr (Char32.ord c), s')
end
end;
