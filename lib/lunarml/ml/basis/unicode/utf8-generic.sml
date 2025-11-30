structure UTF8 : sig
  type codeunit = Char.char
  type string = String.string
  type substring = Substring.substring
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
type codeunit = Char.char
type string = String.string
type substring = Substring.substring
datatype decode_result = datatype UChar.decode_result
local
  fun start s =
        case Substring.getc s of
          NONE => true
        | SOME (c, s') =>
          let val x = Char.ord c
          in
            if x < 0x80 then
              start s'
            else if 0xC2 <= x then
              if x < 0xE0 then
                tail1 s'
              else if x = 0xE0 then
                e0 s'
              else if x < 0xED then
                tail2 s'
              else if x = 0xED then
                ed s'
              else if x < 0xF0 then
                tail2 s'
              else if x = 0xF0 then
                f0 s'
              else if x < 0xF4 then
                tail3 s'
              else if x = 0xF4 then
                f4 s'
              else
                false
            else
              false
          end
  and tail1 s =
        case Substring.getc s of
          NONE => false
        | SOME (c, s') =>
          let val x = Char.ord c
          in
            if 0x80 <= x andalso x <= 0xBF then
              start s'
            else
              false
          end
  and tail2 s =
        case Substring.getc s of
          NONE => false
        | SOME (c, s') =>
          let val x = Char.ord c
          in
            if 0x80 <= x andalso x <= 0xBF then
              tail1 s'
            else
              false
          end
  and tail3 s =
        case Substring.getc s of
          NONE => false
        | SOME (c, s') =>
          let val x = Char.ord c
          in
            if 0x80 <= x andalso x <= 0xBF then
              tail2 s'
            else
              false
          end
  and e0 s =
        case Substring.getc s of
          NONE => false
        | SOME (c, s') =>
          let val x = Char.ord c
          in
            if 0xA0 <= x andalso x <= 0xBF then
              tail1 s'
            else
              false
          end
  and ed s =
        case Substring.getc s of
          NONE => false
        | SOME (c, s') =>
          let val x = Char.ord c
          in
            if 0x80 <= x andalso x <= 0x9F then
              tail1 s'
            else
              false
          end
  and f0 s =
        case Substring.getc s of
          NONE => false
        | SOME (c, s') =>
          let val x = Char.ord c
          in
            if 0x90 <= x andalso x <= 0xBF then
              tail2 s'
            else
              false
          end
  and f4 s =
        case Substring.getc s of
          NONE => false
        | SOME (c, s') =>
          let val x = Char.ord c
          in
            if 0x80 <= x andalso x <= 0x8F then
              tail2 s'
            else
              false
          end
in
  val isWellFormedSubstring = start
end
fun isWellFormed s = isWellFormedSubstring (Substring.full s)
fun encode1 x =
  let val u = UChar.ord x
      val w = Word.fromInt u
  in if u < 128 then
       String.str (Char.chr u)
     else if u < 0x0800 then
       let val c0 = Char.chr (Word.toInt (0wxC0 .Word.orb. (w .Word.>>. 0w6)))
           val c1 = Char.chr (Word.toInt (0wx80 .Word.orb. (w .Word.andb. 0wx3F)))
       in String.implode [c0, c1]
       end
     else if u < 0x10000 then
       let val c0 = Char.chr (Word.toInt (0wxE0 .Word.orb. (w .Word.>>. 0w12)))
           val c1 = Char.chr (Word.toInt (0wx80 .Word.orb. ((w .Word.>>. 0w6) .Word.andb. 0wx3F)))
           val c2 = Char.chr (Word.toInt (0wx80 .Word.orb. (w .Word.andb. 0wx3F)))
       in String.implode [c0, c1, c2]
       end
     else
       let val c0 = Char.chr (Word.toInt (0wxF0 .Word.orb. (w .Word.>>. 0w18)))
           val c1 = Char.chr (Word.toInt (0wx80 .Word.orb. ((w .Word.>>. 0w12) .Word.andb. 0wx3F)))
           val c2 = Char.chr (Word.toInt (0wx80 .Word.orb. ((w .Word.>>. 0w6) .Word.andb. 0wx3F)))
           val c3 = Char.chr (Word.toInt (0wx80 .Word.orb. (w .Word.andb. 0wx3F)))
       in String.implode [c0, c1, c2, c3]
       end
  end
fun decode1 s =
  case Substring.getc s of
    NONE => END
  | SOME (c0, s') =>
      let val x0 = Char.ord c0
          val w0 = Word.fromInt x0
          fun tail1 (acc, s'') =
                case Substring.getc s'' of
                  NONE => INVALID (c0, s')
                | SOME (c1, s''') =>
                  let val x1 = Char.ord c1
                      val w1 = Word.fromInt x1
                  in
                    if 0x80 <= x1 andalso x1 <= 0xBF then
                      let val w = (acc .Word.<<. 0w6) .Word.orb. (w1 .Word.andb. 0wx3F)
                      in SCALAR (UChar.chr (Word.toInt w), s''')
                      end
                    else
                      INVALID (c0, s')
                  end
          fun tail2 (acc, s'') =
                case Substring.getc s'' of
                  NONE => INVALID (c0, s')
                | SOME (c1, s''') =>
                  let val x1 = Char.ord c1
                      val w1 = Word.fromInt x1
                  in
                    if 0x80 <= x1 andalso x1 <= 0xBF then
                      let val w = (acc .Word.<<. 0w6) .Word.orb. (w1 .Word.andb. 0wx3F)
                      in tail1 (w, s''')
                      end
                    else
                      INVALID (c0, s')
                  end
          fun tail3 acc =
                case Substring.getc s' of
                  NONE => INVALID (c0, s')
                | SOME (c1, s'') =>
                  let val x1 = Char.ord c1
                      val w1 = Word.fromInt x1
                  in
                    if 0x80 <= x1 andalso x1 <= 0xBF then
                      let val w = (acc .Word.<<. 0w6) .Word.orb. (w1 .Word.andb. 0wx3F)
                      in tail2 (w, s'')
                      end
                    else
                      INVALID (c0, s')
                  end
          fun e0 acc =
                case Substring.getc s' of
                  NONE => INVALID (c0, s')
                | SOME (c1, s'') =>
                  let val x1 = Char.ord c1
                      val w1 = Word.fromInt x1
                  in
                    if 0xA0 <= x1 andalso x1 <= 0xBF then
                      let val w = (acc .Word.<<. 0w6) .Word.orb. (w1 .Word.andb. 0wx3F)
                      in tail1 (w, s'')
                      end
                    else
                      INVALID (c0, s')
                  end
          fun ed acc =
                case Substring.getc s' of
                  NONE => INVALID (c0, s')
                | SOME (c1, s'') =>
                  let val x1 = Char.ord c1
                      val w1 = Word.fromInt x1
                  in
                    if 0x80 <= x1 andalso x1 <= 0x9F then
                      let val w = (acc .Word.<<. 0w6) .Word.orb. (w1 .Word.andb. 0wx3F)
                      in tail1 (w, s'')
                      end
                    else
                      INVALID (c0, s')
                  end
          fun f0 acc =
                case Substring.getc s' of
                  NONE => INVALID (c0, s')
                | SOME (c1, s'') =>
                  let val x1 = Char.ord c1
                      val w1 = Word.fromInt x1
                  in
                    if 0x90 <= x1 andalso x1 <= 0xBF then
                      let val w = (acc .Word.<<. 0w6) .Word.orb. (w1 .Word.andb. 0wx3F)
                      in tail2 (w, s'')
                      end
                    else
                      INVALID (c0, s')
                  end
          fun f4 acc =
                case Substring.getc s' of
                  NONE => INVALID (c0, s')
                | SOME (c1, s'') =>
                  let val x1 = Char.ord c1
                      val w1 = Word.fromInt x1
                  in
                    if 0x80 <= x1 andalso x1 <= 0x8F then
                      let val w = (acc .Word.<<. 0w6) .Word.orb. (w1 .Word.andb. 0wx3F)
                      in tail2 (w, s'')
                      end
                    else
                      INVALID (c0, s')
                  end
      in
        if x0 < 0x80 then
          SCALAR (UChar.Unsafe.chr x0, s')
        else if 0xC2 <= x0 then
          if x0 < 0xE0 then
            tail1 (w0 .Word.andb. 0wx1F, s')
          else if x0 = 0xE0 then
            e0 (w0 .Word.andb. 0wx0F)
          else if x0 < 0xED then
            tail2 (w0 .Word.andb. 0wx0F, s')
          else if x0 = 0xED then
            ed (w0 .Word.andb. 0wx0F)
          else if x0 < 0xF0 then
            tail2 (w0 .Word.andb. 0wx0F, s')
          else if x0 = 0xF0 then
            f0 (w0 .Word.andb. 0wx07)
          else if x0 < 0xF4 then
            tail3 (w0 .Word.andb. 0wx07)
          else if x0 = 0xF4 then
            f4 (w0 .Word.andb. 0wx07)
          else
            INVALID (c0, s')
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
