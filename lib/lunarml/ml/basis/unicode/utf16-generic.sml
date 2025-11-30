structure UTF16 : sig
  type codeunit = Char16.char
  type string = String16.string
  type substring = Substring16.substring
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
end = struct
type codeunit = Char16.char
type string = String16.string
type substring = Substring16.substring
datatype decode_result = datatype UChar.decode_result
local
  fun start s =
        case Substring16.getc s of
          NONE => true
        | SOME (c, s') =>
          let val x = Char16.ord c
          in
            if x < 0xD800 orelse 0xE000 <= x then
              start s'
            else if x < 0xDC00 then
              surrogate s'
            else
              false
          end
  and surrogate s =
        case Substring16.getc s of
          NONE => false
        | SOME (c, s') =>
          let val x = Char16.ord c
          in
            if 0xDC00 <= x andalso x < 0xE000 then
              start s'
            else
              false
          end
in
  val isWellFormedSubstring = start
end
fun isWellFormed s = isWellFormedSubstring (Substring16.full s)
fun encode1 x =
  let val u = UChar.ord x
      val w = Word.fromInt u
  in if u < 0x10000 then
       String16.str (Char16.chr u)
     else
       let val c0 = Char16.chr (Word.toInt (0wxD800 .Word.orb. ((w - 0wx10000) .Word.>>. 0w10)))
           val c1 = Char16.chr (Word.toInt (0wxDC00 .Word.orb. (w .Word.andb. 0wx3FF)))
       in String16.implode [c0, c1]
       end
  end
fun decode1 s =
  case Substring16.getc s of
    NONE => END
  | SOME (c0, s') =>
      let val x0 = Char16.ord c0
          val w0 = Word.fromInt x0
      in
        if x0 < 0xD800 orelse 0xE000 <= x0 then
          SCALAR (UChar.Unsafe.chr x0, s')
        else if x0 < 0xDC00 then
          case Substring16.getc s' of
            NONE => INVALID (c0, s')
          | SOME (c1, s'') =>
              let val x1 = Char16.ord c1
                  val w1 = Word.fromInt x1
              in
                if 0xDC00 <= x1 andalso x1 < 0xE000 then
                  let val w = 0wx10000 + (((w0 .Word.andb. 0wx3FF) .Word.<<. 0w10) .Word.orb. (w1 .Word.andb. 0wx3FF))
                  in SCALAR (UChar.Unsafe.chr (Word.toInt w), s'')
                  end
                else
                  INVALID (c0, s')
              end
        else
          INVALID (c0, s')
      end
fun decode1Replace s = case decode1 s of
    SCALAR (c, s') => SOME (c, s')
  | INVALID (_, s') => SOME (#"\uFFFD", s')
  | END => NONE
fun decode1Raise s = case decode1 s of
    SCALAR (c, s') => SOME (c, s')
  | INVALID (_, s') => raise Chr
  | END => NONE
end;
