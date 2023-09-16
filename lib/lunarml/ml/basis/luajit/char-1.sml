signature CHAR = sig
    eqtype char
    eqtype string
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
    (* val scan : (Char.char, 'a) StringCvt.reader -> (char, 'a) StringCvt.reader; implemented in scan-text.sml *)
    (* val fromString : String.string -> char option; implemented in scan-text.sml *)
    val toCString : char -> String.string
    (* val fromCString : String.string -> char option *)
end;

structure Char :> CHAR where type char = char where type string = String.string = struct
type char = char
type string = string
val minChar = #"\000"
val maxChar = #"\255"
val maxOrd = 255
val ord : char -> int = fn x => _primCall "Char.ord" (x)
val chr : int -> char = fn x => if x < 0 orelse x > 255 then
                                    raise Chr
                                else
                                    _primCall "Char.chr.unchecked" (x)
fun succ c = chr (ord c + 1)
fun pred c = chr (ord c - 1)
fun compare (x : char, y : char) = if x = y then
                                       EQUAL
                                   else if x < y then
                                       LESS
                                   else
                                       GREATER
fun notContains (s : string) (c : char) : bool = let val result = Lua.call1 Lua.Lib.string.find #[Lua.fromString s, Lua.fromString (String.str c), Lua.fromInt 1, Lua.fromBool true]
                                                 in Lua.isNil result
                                                 end
fun contains s c = not (notContains s c)
fun isAscii (c : char) = c <= #"\127"
fun isUpper (c : char) = #"A" <= c andalso c <= #"Z"
fun isLower (c : char) = #"a" <= c andalso c <= #"z"
fun isDigit (c : char) = #"0" <= c andalso c <= #"9"
fun isAlpha (c : char) = isUpper c orelse isLower c
fun isAlphaNum (c : char) = isAlpha c orelse isDigit c
fun isHexDigit (c : char) = isDigit c orelse (#"a" <= c andalso c <= #"f") orelse (#"A" <= c andalso c <= #"F")
fun isGraph (c : char) = #"!" <= c andalso c <= #"~"
fun isPrint (c : char) = isGraph c orelse c = #" "
fun isPunct (c : char) = isGraph c andalso not (isAlphaNum c)
fun isCntrl (c : char) = isAscii c andalso not (isPrint c)
fun isSpace (c : char) = (#"\t" <= c andalso c <= #"\r") orelse c = #" "
(* string.lower and string.upper depends on the locale *)
fun toLower (c : char) = if isUpper c then
                             chr (ord c - (ord #"A" - ord #"a"))
                         else
                             c
fun toUpper (c : char) = if isLower c then
                             chr (ord c - (ord #"a" - ord #"A"))
                         else
                             c
fun toString #"\\" = "\\\\"
  | toString #"\"" = "\\\""
  | toString c = if isPrint c then
                     String.str c
                 else
                     case c of
                         #"\a" => "\\a"
                       | #"\b" => "\\b"
                       | #"\t" => "\\t"
                       | #"\n" => "\\n"
                       | #"\v" => "\\v"
                       | #"\f" => "\\f"
                       | #"\r" => "\\r"
                       | _ => let val x = ord c
                              in if x < 32 then
                                     "\\^" ^ String.str (chr (x + 64))
                                 else if x < 100 then
                                     "\\0" ^ Int.toString x
                                 else
                                     "\\" ^ Int.toString x
                                 (* TODO: x >= 1000 *)
                              end
fun toCString #"\\" = "\\\\"
  | toCString #"\"" = "\\\""
  | toCString #"?" = "\\?"
  | toCString #"'" = "\\'"
  | toCString c = if isPrint c then
                      String.str c
                  else
                      case c of
                          #"\a" => "\\a"
                        | #"\b" => "\\b"
                        | #"\t" => "\\t"
                        | #"\n" => "\\n"
                        | #"\v" => "\\v"
                        | #"\f" => "\\f"
                        | #"\r" => "\\r"
                        | _ => let val x = ord c
                                   val s = Int.fmt StringCvt.OCT x
                               in if x < 8 then
                                      "\\00" ^ s
                                  else if x < 64 then
                                      "\\0" ^ s
                                  else
                                      "\\" ^ s
                                  (* TODO: x >= 512 *)
                               end
open Char (* <, <=, >, >= *)
(* scan, fromString, toCString, fromCString *)
end (* structure Char *)
val chr = Char.chr
val ord = Char.ord;
