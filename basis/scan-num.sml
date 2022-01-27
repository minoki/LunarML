signature INTEGER = sig
    eqtype int
    (* val toLarge : int -> LargeInt.int *)
    (* val fromLarge : LargeInt.int -> int *)
    val toInt : int -> Int.int
    val fromInt : Int.int -> int
    val precision : int option
    val minInt : int option
    val maxInt : int option
    val + : int * int -> int
    val - : int * int -> int
    val * : int * int -> int
    val div : int * int -> int
    val mod : int * int -> int
    val quot : int * int -> int
    val rem : int * int -> int
    val compare : int * int -> order
    val < : int * int -> bool
    val <= : int * int -> bool
    val > : int * int -> bool
    val >= : int * int -> bool
    val ~ : int -> int
    val abs : int -> int
    val min : int * int -> int
    val max : int * int -> int
    val sign : int -> int
    val sameSign : int * int -> bool
    val fmt : StringCvt.radix -> int -> string
    val toString : int -> string
    val scan : StringCvt.radix -> (char, 'a) StringCvt.reader -> (int, 'a) StringCvt.reader
    val fromString : string -> int option
end;

local
    fun skipInitialWhitespace (getc, strm) = case getc strm of
                                                 NONE => strm
                                               | SOME (c, strm') => if Char.isSpace c then
                                                                        skipInitialWhitespace (getc, strm')
                                                                    else
                                                                        strm
    fun isBinDigit c = c = #"0" orelse c = #"1"
    fun isOctDigit c = #"0" <= c andalso c <= #"7"
    fun digitToInt c = if #"0" <= c andalso c <= #"9" then
                           Char.ord c - Char.ord #"0"
                       else if #"a" <= c andalso c <= #"f" then
                           Char.ord c - Char.ord #"a"
                       else
                           Char.ord c - Char.ord #"A"
    (* scanSign: true if negative *)
    fun scanSign (getc, strm) = case getc strm of
                                    SOME (#"+", strm) => (false, strm)
                                  | SOME (#"~", strm) => (true, strm)
                                  | SOME (#"-", strm) => (true, strm)
                                  | _ => (false, strm)
    fun skip0w (isDigit, getc, strm) = case getc strm of
                                           NONE => strm
                                         | SOME (#"0", strm') => (case getc strm' of
                                                                      SOME (#"w", strm'') => (case getc strm'' of
                                                                                                  SOME (c, _) => if isDigit c then
                                                                                                                     strm''
                                                                                                                 else
                                                                                                                     strm
                                                                                                | NONE => strm
                                                                                             )
                                                                    | _ => strm
                                                                 )
                                         | _ => strm
    fun skip0wx (getc, strm) = case getc strm of
                                   NONE => strm
                                 | SOME (#"0", strm') =>
                                   (case getc strm' of
                                        SOME (#"w", strm'') =>
                                        (case getc strm'' of
                                             SOME (x, strm''') =>
                                             if x = #"x" orelse x = #"X" then
                                                 case getc strm''' of
                                                     SOME (c, _) => if Char.isHexDigit c then
                                                                        strm'''
                                                                    else
                                                                        strm
                                                   | NONE => strm
                                             else
                                                 strm
                                        )
                                      | SOME (#"x", strm'') =>
                                        (case getc strm'' of
                                             SOME (c, _) => if Char.isHexDigit c then
                                                                strm''
                                                            else
                                                                strm
                                           | NONE => strm
                                        )
                                      | SOME (#"X", strm'') =>
                                        (case getc strm'' of
                                             SOME (c, _) => if Char.isHexDigit c then
                                                                strm''
                                                            else
                                                                strm
                                           | NONE => strm
                                        )
                                      | _ => strm
                                   )
                                 | _ => strm
in
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

structure Word : sig
              type word = word
              val wordSize : int
              val toLarge : word -> LargeWord.word
              val toLargeX : word -> LargeWord.word
              val fromLarge : LargeWord.word -> word
              val toInt : word -> int
              val toIntX : word -> int
              val fromInt : int -> word
              val andb : word * word -> word
              val orb : word * word -> word
              val xorb : word * word -> word
              val notb : word -> word
              val << : word * word -> word
              val >> : word * word -> word
              val ~>> : word * word -> word
              val + : word * word -> word
              val - : word * word -> word
              val * : word * word -> word
              val div : word * word -> word
              val mod : word * word -> word
              val ~ : word -> word
              val compare : word * word -> order
              val < : word * word -> bool
              val <= : word * word -> bool
              val > : word * word -> bool
              val >= : word * word -> bool
              val min : word * word -> word
              val max : word * word -> word
              val fmt : StringCvt.radix -> word -> string
              val toString : word -> string
              val scan : StringCvt.radix -> (char, 'a) StringCvt.reader -> (word, 'a) StringCvt.reader
              val fromString : string -> word option
          end = struct
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

structure Word8 : sig
              type word = Word8.word
              val wordSize : int
              val toLarge : word -> LargeWord.word
              val toLargeX : word -> LargeWord.word
              val fromLarge : LargeWord.word -> word
              val toInt : word -> int
              val toIntX : word -> int
              val fromInt : int -> word
              val andb : word * word -> word
              val orb : word * word -> word
              val xorb : word * word -> word
              val notb : word -> word
              val << : word * Word.word -> word
              val >> : word * Word.word -> word
              val ~>> : word * Word.word -> word
              val + : word * word -> word
              val - : word * word -> word
              val * : word * word -> word
              val div : word * word -> word
              val mod : word * word -> word
              val compare : word * word -> order
              val < : word * word -> bool
              val <= : word * word -> bool
              val > : word * word -> bool
              val >= : word * word -> bool
              val ~ : word -> word
              val min : word * word -> word
              val max : word * word -> word
              val fmt : StringCvt.radix -> word -> string
              val toString : word -> string
              val scan : StringCvt.radix -> (char, 'a) StringCvt.reader -> (word, 'a) StringCvt.reader
              val fromString : string -> word option
          end = struct
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

structure Word16 : sig
              type word = Word16.word
              val wordSize : int
              val toLarge : word -> LargeWord.word
              val toLargeX : word -> LargeWord.word
              val fromLarge : LargeWord.word -> word
              val toInt : word -> int
              val toIntX : word -> int
              val fromInt : int -> word
              val andb : word * word -> word
              val orb : word * word -> word
              val xorb : word * word -> word
              val notb : word -> word
              val << : word * Word.word -> word
              val >> : word * Word.word -> word
              val ~>> : word * Word.word -> word
              val + : word * word -> word
              val - : word * word -> word
              val * : word * word -> word
              val div : word * word -> word
              val mod : word * word -> word
              val compare : word * word -> order
              val < : word * word -> bool
              val <= : word * word -> bool
              val > : word * word -> bool
              val >= : word * word -> bool
              val ~ : word -> word
              val min : word * word -> word
              val max : word * word -> word
              val fmt : StringCvt.radix -> word -> string
              val toString : word -> string
              val scan : StringCvt.radix -> (char, 'a) StringCvt.reader -> (word, 'a) StringCvt.reader
              val fromString : string -> word option
          end = struct
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

structure Word32 : sig
              type word = Word32.word
              val wordSize : int
              val toLarge : word -> LargeWord.word
              val toLargeX : word -> LargeWord.word
              val fromLarge : LargeWord.word -> word
              val toInt : word -> int
              val toIntX : word -> int
              val fromInt : int -> word
              val andb : word * word -> word
              val orb : word * word -> word
              val xorb : word * word -> word
              val notb : word -> word
              val << : word * Word.word -> word
              val >> : word * Word.word -> word
              val ~>> : word * Word.word -> word
              val + : word * word -> word
              val - : word * word -> word
              val * : word * word -> word
              val div : word * word -> word
              val mod : word * word -> word
              val compare : word * word -> order
              val < : word * word -> bool
              val <= : word * word -> bool
              val > : word * word -> bool
              val >= : word * word -> bool
              val ~ : word -> word
              val min : word * word -> word
              val max : word * word -> word
              val fmt : StringCvt.radix -> word -> string
              val toString : word -> string
              val scan : StringCvt.radix -> (char, 'a) StringCvt.reader -> (word, 'a) StringCvt.reader
              val fromString : string -> word option
          end = struct
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

structure Word64 : sig
              type word = Word64.word
              val wordSize : int
              val toLarge : word -> LargeWord.word
              val toLargeX : word -> LargeWord.word
              val fromLarge : LargeWord.word -> word
              val toInt : word -> int
              val toIntX : word -> int
              val fromInt : int -> word
              val andb : word * word -> word
              val orb : word * word -> word
              val xorb : word * word -> word
              val notb : word -> word
              val << : word * Word.word -> word
              val >> : word * Word.word -> word
              val ~>> : word * Word.word -> word
              val + : word * word -> word
              val - : word * word -> word
              val * : word * word -> word
              val div : word * word -> word
              val mod : word * word -> word
              val compare : word * word -> order
              val < : word * word -> bool
              val <= : word * word -> bool
              val > : word * word -> bool
              val >= : word * word -> bool
              val ~ : word -> word
              val min : word * word -> word
              val max : word * word -> word
              val fmt : StringCvt.radix -> word -> string
              val toString : word -> string
              val scan : StringCvt.radix -> (char, 'a) StringCvt.reader -> (word, 'a) StringCvt.reader
              val fromString : string -> word option
          end = struct
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
end;
