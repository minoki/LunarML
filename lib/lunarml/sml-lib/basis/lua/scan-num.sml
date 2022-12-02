signature INTEGER = sig
    include INTEGER
    val scan : StringCvt.radix -> (char, 'a) StringCvt.reader -> (int, 'a) StringCvt.reader
    val fromString : string -> int option
end;

signature WORD = sig
    include WORD
    val scan : StringCvt.radix -> (char, 'a) StringCvt.reader -> (word, 'a) StringCvt.reader
    val fromString : string -> word option
end;

signature REAL = sig
    include REAL
    val scan : (char, 'a) StringCvt.reader -> (real, 'a) StringCvt.reader
    val fromString : string -> real option
end;

local open ScanNumUtils in
structure Int : INTEGER = struct
local
    fun scanDigits (radix, isDigit, getc)
        = let fun go1 (x, strm) = case getc strm of
                                      SOME (c, strm') => if isDigit c then
                                                             go1 (radix * x + digitToInt c, strm')
                                                         else
                                                             SOME (x, strm)
                                    | NONE => SOME (x, strm)
          in fn strm => case getc strm of
                            SOME (c, strm') => if isDigit c then
                                                   go1 (digitToInt c, strm')
                                               else
                                                   NONE
                          | NONE => NONE
          end
    fun scanNegativeDigits (radix, isDigit, getc)
        = let fun go1 (x, strm) = case getc strm of
                                      SOME (c, strm') => if isDigit c then
                                                             go1 (radix * x - digitToInt c, strm')
                                                         else
                                                             SOME (x, strm)
                                    | NONE => SOME (x, strm)
          in fn strm => case getc strm of
                            SOME (c, strm') => if isDigit c then
                                                   go1 (~ (digitToInt c), strm')
                                               else
                                                   NONE
                          | NONE => NONE
          end
in
fun scan StringCvt.BIN getc strm = let val strm = skipInitialWhitespace (getc, strm)
                                       val (isNegative, strm) = scanSign (getc, strm)
                                   in if isNegative then
                                          scanNegativeDigits (2, isBinDigit, getc) strm
                                      else
                                          scanDigits (2, isBinDigit, getc) strm
                                   end
  | scan StringCvt.OCT getc strm = let val strm = skipInitialWhitespace (getc, strm)
                                       val (isNegative, strm) = scanSign (getc, strm)
                                   in if isNegative then
                                          scanNegativeDigits (8, isOctDigit, getc) strm
                                      else
                                          scanDigits (8, isOctDigit, getc) strm
                                   end
  | scan StringCvt.DEC getc strm = let val strm = skipInitialWhitespace (getc, strm)
                                       val (isNegative, strm) = scanSign (getc, strm)
                                   in if isNegative then
                                          scanNegativeDigits (10, Char.isDigit, getc) strm
                                      else
                                          scanDigits (10, Char.isDigit, getc) strm
                                   end
  | scan StringCvt.HEX getc strm = let val strm = skipInitialWhitespace (getc, strm)
                                       val (isNegative, strm) = scanSign (getc, strm)
                                       val strm = case getc strm of
                                                      SOME (#"0", strm') =>
                                                      (case getc strm' of
                                                           SOME (c, strm'') =>
                                                           if c = #"x" orelse c = #"X" then
                                                               case getc strm'' of
                                                                   SOME (c, _) => if Char.isHexDigit c then
                                                                                      strm''
                                                                                  else
                                                                                      strm
                                                                 | NONE => strm
                                                           else
                                                               strm
                                                         | NONE => strm
                                                      )
                                                    | _ => strm
                                   in if isNegative then
                                          scanNegativeDigits (16, Char.isHexDigit, getc) strm
                                      else
                                          scanDigits (16, Char.isHexDigit, getc) strm
                                   end
fun fromString s = StringCvt.scanString (scan StringCvt.DEC) s
end
open Int
end;

structure Word :> WORD where type word = word = struct
local
    fun scanDigits (radix, isDigit, getc)
        = let fun go1 (x, strm) = case getc strm of
                                      SOME (c, strm') => if isDigit c then
                                                             let val y = radix * x
                                                             in if y div radix <> x then
                                                                    raise Overflow
                                                                else
                                                                    let val z = y + Word.fromInt (digitToInt c)
                                                                    in if z < y then
                                                                           raise Overflow
                                                                       else
                                                                           go1 (z, strm')
                                                                    end
                                                             end
                                                         else
                                                             SOME (x, strm)
                                    | NONE => SOME (x, strm)
          in fn strm => case getc strm of
                            SOME (c, strm') => if isDigit c then
                                                   go1 (Word.fromInt (digitToInt c), strm')
                                               else
                                                   NONE
                          | NONE => NONE
          end
in
fun scan StringCvt.BIN getc strm = let val strm = skipInitialWhitespace (getc, strm)
                                       val strm = skip0w (isBinDigit, getc, strm)
                                   in scanDigits (0w2, isBinDigit, getc) strm
                                   end
  | scan StringCvt.OCT getc strm = let val strm = skipInitialWhitespace (getc, strm)
                                       val strm = skip0w (isOctDigit, getc, strm)
                                   in scanDigits (0w8, isOctDigit, getc) strm
                                   end
  | scan StringCvt.DEC getc strm = let val strm = skipInitialWhitespace (getc, strm)
                                       val strm = skip0w (Char.isDigit, getc, strm)
                                   in scanDigits (0w10, Char.isDigit, getc) strm
                                   end
  | scan StringCvt.HEX getc strm = let val strm = skipInitialWhitespace (getc, strm)
                                       val strm = skip0wx (getc, strm)
                                   in scanDigits (0w16, Char.isHexDigit, getc) strm
                                   end
fun fromString s = StringCvt.scanString (scan StringCvt.DEC) s
end
open Word
end;

structure Word8 :> WORD where type word = Word8.word = struct
local
    fun scanDigits (radix, isDigit, getc)
        = let fun go1 (x, strm) = case getc strm of
                                      SOME (c, strm') => if isDigit c then
                                                             let val y = radix * x
                                                             in if y div radix <> x then
                                                                    raise Overflow
                                                                else
                                                                    let val z = y + Word8.fromInt (digitToInt c)
                                                                    in if z < y then
                                                                           raise Overflow
                                                                       else
                                                                           go1 (z, strm')
                                                                    end
                                                             end
                                                         else
                                                             SOME (x, strm)
                                    | NONE => SOME (x, strm)
          in fn strm => case getc strm of
                            SOME (c, strm') => if isDigit c then
                                                   go1 (Word8.fromInt (digitToInt c), strm')
                                               else
                                                   NONE
                          | NONE => NONE
          end
in
fun scan StringCvt.BIN getc strm = let val strm = skipInitialWhitespace (getc, strm)
                                       val strm = skip0w (isBinDigit, getc, strm)
                                   in scanDigits (0w2, isBinDigit, getc) strm
                                   end
  | scan StringCvt.OCT getc strm = let val strm = skipInitialWhitespace (getc, strm)
                                       val strm = skip0w (isOctDigit, getc, strm)
                                   in scanDigits (0w8, isOctDigit, getc) strm
                                   end
  | scan StringCvt.DEC getc strm = let val strm = skipInitialWhitespace (getc, strm)
                                       val strm = skip0w (Char.isDigit, getc, strm)
                                   in scanDigits (0w10, Char.isDigit, getc) strm
                                   end
  | scan StringCvt.HEX getc strm = let val strm = skipInitialWhitespace (getc, strm)
                                       val strm = skip0wx (getc, strm)
                                   in scanDigits (0w16, Char.isHexDigit, getc) strm
                                   end
fun fromString s = StringCvt.scanString (scan StringCvt.DEC) s
end
open Word8
end;

structure Word16 :> WORD where type word = Word16.word = struct
local
    fun scanDigits (radix, isDigit, getc)
        = let fun go1 (x, strm) = case getc strm of
                                      SOME (c, strm') => if isDigit c then
                                                             let val y = radix * x
                                                             in if y div radix <> x then
                                                                    raise Overflow
                                                                else
                                                                    let val z = y + Word16.fromInt (digitToInt c)
                                                                    in if z < y then
                                                                           raise Overflow
                                                                       else
                                                                           go1 (z, strm')
                                                                    end
                                                             end
                                                         else
                                                             SOME (x, strm)
                                    | NONE => SOME (x, strm)
          in fn strm => case getc strm of
                            SOME (c, strm') => if isDigit c then
                                                   go1 (Word16.fromInt (digitToInt c), strm')
                                               else
                                                   NONE
                          | NONE => NONE
          end
in
fun scan StringCvt.BIN getc strm = let val strm = skipInitialWhitespace (getc, strm)
                                       val strm = skip0w (isBinDigit, getc, strm)
                                   in scanDigits (0w2, isBinDigit, getc) strm
                                   end
  | scan StringCvt.OCT getc strm = let val strm = skipInitialWhitespace (getc, strm)
                                       val strm = skip0w (isOctDigit, getc, strm)
                                   in scanDigits (0w8, isOctDigit, getc) strm
                                   end
  | scan StringCvt.DEC getc strm = let val strm = skipInitialWhitespace (getc, strm)
                                       val strm = skip0w (Char.isDigit, getc, strm)
                                   in scanDigits (0w10, Char.isDigit, getc) strm
                                   end
  | scan StringCvt.HEX getc strm = let val strm = skipInitialWhitespace (getc, strm)
                                       val strm = skip0wx (getc, strm)
                                   in scanDigits (0w16, Char.isHexDigit, getc) strm
                                   end
fun fromString s = StringCvt.scanString (scan StringCvt.DEC) s
end
open Word16
end;

structure Word32 :> WORD where type word = Word32.word = struct
local
    fun scanDigits (radix, isDigit, getc)
        = let fun go1 (x, strm) = case getc strm of
                                      SOME (c, strm') => if isDigit c then
                                                             let val y = radix * x
                                                             in if y div radix <> x then
                                                                    raise Overflow
                                                                else
                                                                    let val z = y + Word32.fromInt (digitToInt c)
                                                                    in if z < y then
                                                                           raise Overflow
                                                                       else
                                                                           go1 (z, strm')
                                                                    end
                                                             end
                                                         else
                                                             SOME (x, strm)
                                    | NONE => SOME (x, strm)
          in fn strm => case getc strm of
                            SOME (c, strm') => if isDigit c then
                                                   go1 (Word32.fromInt (digitToInt c), strm')
                                               else
                                                   NONE
                          | NONE => NONE
          end
in
fun scan StringCvt.BIN getc strm = let val strm = skipInitialWhitespace (getc, strm)
                                       val strm = skip0w (isBinDigit, getc, strm)
                                   in scanDigits (0w2, isBinDigit, getc) strm
                                   end
  | scan StringCvt.OCT getc strm = let val strm = skipInitialWhitespace (getc, strm)
                                       val strm = skip0w (isOctDigit, getc, strm)
                                   in scanDigits (0w8, isOctDigit, getc) strm
                                   end
  | scan StringCvt.DEC getc strm = let val strm = skipInitialWhitespace (getc, strm)
                                       val strm = skip0w (Char.isDigit, getc, strm)
                                   in scanDigits (0w10, Char.isDigit, getc) strm
                                   end
  | scan StringCvt.HEX getc strm = let val strm = skipInitialWhitespace (getc, strm)
                                       val strm = skip0wx (getc, strm)
                                   in scanDigits (0w16, Char.isHexDigit, getc) strm
                                   end
fun fromString s = StringCvt.scanString (scan StringCvt.DEC) s
end
open Word32
end;

structure Word64 :> WORD where type word = Word64.word = struct
local
    fun scanDigits (radix, isDigit, getc)
        = let fun go1 (x, strm) = case getc strm of
                                      SOME (c, strm') => if isDigit c then
                                                             let val y = radix * x
                                                             in if y div radix <> x then
                                                                    raise Overflow
                                                                else
                                                                    let val z = y + Word64.fromInt (digitToInt c)
                                                                    in if z < y then
                                                                           raise Overflow
                                                                       else
                                                                           go1 (z, strm')
                                                                    end
                                                             end
                                                         else
                                                             SOME (x, strm)
                                    | NONE => SOME (x, strm)
          in fn strm => case getc strm of
                            SOME (c, strm') => if isDigit c then
                                                   go1 (Word64.fromInt (digitToInt c), strm')
                                               else
                                                   NONE
                          | NONE => NONE
          end
in
fun scan StringCvt.BIN getc strm = let val strm = skipInitialWhitespace (getc, strm)
                                       val strm = skip0w (isBinDigit, getc, strm)
                                   in scanDigits (0w2, isBinDigit, getc) strm
                                   end
  | scan StringCvt.OCT getc strm = let val strm = skipInitialWhitespace (getc, strm)
                                       val strm = skip0w (isOctDigit, getc, strm)
                                   in scanDigits (0w8, isOctDigit, getc) strm
                                   end
  | scan StringCvt.DEC getc strm = let val strm = skipInitialWhitespace (getc, strm)
                                       val strm = skip0w (Char.isDigit, getc, strm)
                                   in scanDigits (0w10, Char.isDigit, getc) strm
                                   end
  | scan StringCvt.HEX getc strm = let val strm = skipInitialWhitespace (getc, strm)
                                       val strm = skip0wx (getc, strm)
                                   in scanDigits (0w16, Char.isHexDigit, getc) strm
                                   end
fun fromString s = StringCvt.scanString (scan StringCvt.DEC) s
end
open Word64
end;

structure Real : REAL where type real = real = struct
fun scanSubstring (getc, strm, s) = case Substring.getc s of
                                        NONE => SOME strm
                                      | SOME (c, s') => case getc strm of
                                                            SOME (c', strm') => if c = c' orelse (Char.isAlpha c andalso Char.isAlpha c' andalso Char.toUpper c = Char.toUpper c') then
                                                                                    scanSubstring (getc, strm', s')
                                                                                else
                                                                                    NONE
fun scanZeroOrMoreDigits (getc, strm, revAcc) = case getc strm of
                                                    SOME (c, strm') => if Char.isDigit c then
                                                                           scanZeroOrMoreDigits (getc, strm', c :: revAcc)
                                                                       else
                                                                           (revAcc, strm)
                                                  | NONE => (revAcc, strm)
fun scanOneOrMoreDigits (getc, strm, revAcc) = case getc strm of
                                                   SOME (c, strm') => if Char.isDigit c then
                                                                          SOME (scanZeroOrMoreDigits (getc, strm', c :: revAcc))
                                                                      else
                                                                          NONE
                                                 | NONE => NONE
fun scanOptFracPart (getc, strm, revAcc) = case getc strm of
                                               SOME (#".", strm') => (case scanOneOrMoreDigits (getc, strm', #"." :: revAcc) of
                                                                          SOME (revAcc, strm'') => (revAcc, strm'')
                                                                        | NONE => (revAcc, strm)
                                                                     )
                                             | NONE => (revAcc, strm)
fun scanOptExpPart (getc, strm, revAcc) = case getc strm of
                                              SOME (c, strm') => if c = #"e" orelse c = #"E" then
                                                                     let val (isNegative, strm'') = scanSign (getc, strm')
                                                                     in case scanOneOrMoreDigits (getc, strm'', if isNegative then [#"-", #"e"] else [#"e"]) of
                                                                            SOME (revAcc, strm''') => (revAcc, strm''')
                                                                          | NONE => (revAcc, strm)
                                                                     end
                                                                 else
                                                                     (revAcc, strm)
                                            | NONE => (revAcc, strm)
fun toNumber s = let val result = Lua.call1 Lua.Lib.tonumber #[Lua.fromString s]
                 in Lua.unsafeFromValue (Lua.* (result, Lua.fromReal 1.0))
                 end
(* [+~-]? ([0-9]+(.[0-9]+?)?|.[0-9]+)([eE][+~-]?[0-9]+?)? / [+~-]?(inf|infinity|nan) *)
fun scan getc strm = let val strm = skipInitialWhitespace (getc, strm)
                         val (isNegative, strm) = scanSign (getc, strm)
                         val signPart = if isNegative then [#"-"] else []
                     in case getc strm of
                            SOME (#".", strm') => (case scanOneOrMoreDigits (getc, strm', #"." :: #"0" :: signPart) of
                                                       SOME (revAcc, strm'') => let val (revAcc, strm''') = scanOptExpPart (getc, strm'', revAcc)
                                                                                in SOME (toNumber (String.implodeRev revAcc), strm''')
                                                                                end
                                                     | NONE => NONE
                                                  )
                          | SOME (c, strm') => if c = #"i" orelse c = #"I" then
                                                   (* inf|infinity *)
                                                   case scanSubstring (getc, strm', Substring.full "nf") of
                                                       SOME strm'' => let val inf = if isNegative then Real.negInf else Real.posInf
                                                                      in case scanSubstring (getc, strm'', Substring.full "inity") of
                                                                             SOME strm''' => SOME (inf, strm''')
                                                                           | NONE => SOME (inf, strm'')
                                                                      end
                                                     | NONE => NONE
                                               else if c = #"n" orelse c = #"N" then
                                                   (* nan *)
                                                   case scanSubstring (getc, strm', Substring.full "an") of
                                                       SOME strm'' => SOME (0.0 / 0.0, strm'')
                                                     | NONE => NONE
                                               else if Char.isDigit c then
                                                   let val (revAcc, strm'') = scanZeroOrMoreDigits (getc, strm', c :: signPart)
                                                       val (revAcc, strm''') = scanOptFracPart (getc, strm'', revAcc)
                                                       val (revAcc, strm'''') = scanOptExpPart (getc, strm''', revAcc)
                                                   in SOME (toNumber (String.implodeRev revAcc), strm'''')
                                                   end
                                               else
                                                   NONE
                     end
fun fromString s = StringCvt.scanString scan s
open Real
end;
end;
