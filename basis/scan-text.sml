local
    fun digitToInt c = if #"0" <= c andalso c <= #"9" then
                           Char.ord c - Char.ord #"0"
                       else if #"a" <= c andalso c <= #"f" then
                           Char.ord c - Char.ord #"a"
                       else
                           Char.ord c - Char.ord #"A"
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
structure Char : sig
              type char = char
              type string = string
              val minChar : char
              val maxChar : char
              val maxOrd : int
              val ord : char -> int
              val chr : int -> char
              val succ : char -> char
              val pred : char -> char
              val compare : char * char -> order
              val < : char * char -> bool
              val <= : char * char -> bool
              val > : char * char -> bool
              val >= : char * char -> bool
              val contains : string -> char -> bool
              val notContains : string -> char -> bool
              val isAscii : char -> bool
              val toLower : char -> char
              val toUpper : char -> char
              val isAlpha : char -> bool
              val isAlphaNum : char -> bool
              val isCntrl : char -> bool
              val isDigit : char -> bool
              val isGraph : char -> bool
              val isHexDigit : char -> bool
              val isLower : char -> bool
              val isPrint : char -> bool
              val isSpace : char -> bool
              val isPunct : char -> bool
              val isUpper : char -> bool
              val toString : char -> String.string
              val scan : (Char.char, 'a) StringCvt.reader -> (char, 'a) StringCvt.reader
              val fromString : String.string -> char option
          end = struct
fun scan getc strm = case scanChar (getc, strm) of
                         Parsed (c, strm') => SOME (c, strm')
                       | Skipped strm' => scan getc strm'
                       | Error => NONE
                       | Empty => NONE
fun fromString s = StringCvt.scanString scan s
open Char
end

structure String : sig
              type string = string
              type char = char
              val maxSize : int
              val size : string -> int
              val sub : string * int -> char
              val extract : string * int * int option -> string
              val substring : string * int * int -> string
              val ^ : string * string -> string
              val concat : string list -> string
              val concatWith : string -> string list -> string
              val str : char -> string
              val implode : char list -> string
              val explode : string -> char list
              val map : (char -> char) -> string -> string
              val translate : (char -> string) -> string -> string
              val fields : (char -> bool) -> string -> string list
              val isPrefix : string -> string -> bool
              val compare : string * string -> order
              val < : string * string -> bool
              val <= : string * string -> bool
              val > : string * string -> bool
              val >= : string * string -> bool
              val toString : string -> string
              val scan : (Char.char, 'a) StringCvt.reader -> (string, 'a) StringCvt.reader (* char? Char.char? *)
              val fromString : String.string -> string option
          end = struct
fun scan getc strm = let fun go (strm, revAcc) = case scanChar (getc, strm) of
                                                     Parsed (c, strm') => go (strm', c :: revAcc)
                                                   | Skipped strm' => go (strm', revAcc)
                                                   | Error => SOME (String.implode (List.rev revAcc), strm)
                                                   | Empty => SOME (String.implode (List.rev revAcc), strm)
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
