signature CHAR = sig
    include CHAR
    val scan : (Char.char, 'a) StringCvt.reader -> (char, 'a) StringCvt.reader
    val fromString : String.string -> char option
end;

signature STRING = sig
    include STRING
    val scan : (Char.char, 'a) StringCvt.reader -> (string, 'a) StringCvt.reader (* The spec says 'char' here, but I think 'Char.char' is intended *)
    val fromString : String.string -> string option
end;

local
    fun digitToInt c = if #"0" <= c andalso c <= #"9" then
                           Char.ord c - Char.ord #"0"
                       else if #"a" <= c andalso c <= #"f" then
                           Char.ord c - Char.ord #"a" + 10
                       else
                           Char.ord c - Char.ord #"A" + 10
    fun scanDecimalDigits (getc, strm, n, acc) = if n = 0 then
                                                     SOME (acc, strm)
                                                 else
                                                     case getc strm of
                                                         SOME (c, strm') => if Char.isDigit c then
                                                                                scanDecimalDigits (getc, strm', n - 1, acc * 10 + digitToInt c)
                                                                            else
                                                                                NONE
                                                       | NONE => NONE
    fun scanHexadecimalDigits (getc, strm, n, acc) = if n = 0 then
                                                         SOME (acc, strm)
                                                     else
                                                         case getc strm of
                                                             SOME (c, strm') => if Char.isHexDigit c then
                                                                                    scanHexadecimalDigits (getc, strm', n - 1, acc * 16 + digitToInt c)
                                                                                else
                                                                                    NONE
                                                           | NONE => NONE
    fun skipSpaces (getc, strm) = case getc strm of
                                      SOME (#"\\", strm') => SOME strm'
                                    | SOME (c, strm') => if Char.isSpace c then
                                                             skipSpaces (getc, strm')
                                                         else
                                                             NONE
                                    | NONE => NONE
    datatype 'strm ScanCharResult = Parsed of char * 'strm
                                  | Skipped of 'strm
                                  | Error
                                  | Empty
    fun scanChar (getc, strm) = case getc strm of
                                    SOME (#"\\", strm') => (case getc strm' of
                                                                SOME (#"a", strm'') => Parsed (#"\a", strm'')
                                                              | SOME (#"b", strm'') => Parsed (#"\b", strm'')
                                                              | SOME (#"t", strm'') => Parsed (#"\t", strm'')
                                                              | SOME (#"n", strm'') => Parsed (#"\n", strm'')
                                                              | SOME (#"v", strm'') => Parsed (#"\v", strm'')
                                                              | SOME (#"f", strm'') => Parsed (#"\f", strm'')
                                                              | SOME (#"r", strm'') => Parsed (#"\r", strm'')
                                                              | SOME (#"\\", strm'') => Parsed (#"\\", strm'')
                                                              | SOME (#"\"", strm'') => Parsed (#"\"", strm'')
                                                              | SOME (#"^", strm'') => (case getc strm'' of
                                                                                            SOME (c, strm''') => let val r = Char.ord c
                                                                                                                 in if 64 <= r andalso r <= 95 then
                                                                                                                        Parsed (Char.chr (r - 64), strm''')
                                                                                                                    else
                                                                                                                        Error
                                                                                                                 end
                                                                                          | NONE => Error
                                                                                       )
                                                              | SOME (#"u", strm'') => (case scanHexadecimalDigits (getc, strm'', 4, 0) of
                                                                                            SOME (value, strm''') => if value <= Char.maxOrd then
                                                                                                                         Parsed (Char.chr value, strm''')
                                                                                                                     else
                                                                                                                         Error
                                                                                          | NONE => Error
                                                                                       )
                                                              | SOME (c, strm'') => if Char.isDigit c then
                                                                                        (case scanDecimalDigits (getc, strm'', 2, 0) of
                                                                                             SOME (value, strm''') => if value <= Char.maxOrd then
                                                                                                                          Parsed (Char.chr value, strm''')
                                                                                                                      else
                                                                                                                          Error
                                                                                           | NONE => Error
                                                                                        )
                                                                                    else if Char.isSpace c then
                                                                                        case skipSpaces (getc, strm'') of
                                                                                            SOME strm''' => Skipped strm'''
                                                                                          | NONE => Error
                                                                                    else
                                                                                        Error
                                                              | NONE => Error
                                                           )
                                  | SOME (c, strm') => if Char.isPrint c then
                                                           Parsed (c, strm')
                                                       else
                                                           Error
                                  | NONE => Empty
in
structure Char : CHAR where type char = Char.char where type string = String.string = struct
fun scan getc strm = case scanChar (getc, strm) of
                         Parsed (c, strm') => SOME (c, strm')
                       | Skipped strm' => scan getc strm'
                       | Error => NONE
                       | Empty => NONE
fun fromString s = StringCvt.scanString scan s
open Char
end

structure String :> STRING where type string = string where type char = Char.char = struct
fun scan getc strm = let fun go (strm, revAcc) = case scanChar (getc, strm) of
                                                     Parsed (c, strm') => go (strm', c :: revAcc)
                                                   | Skipped strm' => go (strm', revAcc)
                                                   | Error => SOME (String.implodeRev revAcc, strm)
                                                   | Empty => SOME (String.implodeRev revAcc, strm)
                     in case scanChar (getc, strm) of
                            Parsed (c, strm') => go (strm', [c])
                          | Skipped strm' => go (strm', [])
                          | Error => NONE
                          | Empty => SOME ("", strm)
                     end
val fromString = StringCvt.scanString scan
open String
end
end;
