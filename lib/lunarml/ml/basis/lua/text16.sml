structure Char16 = struct
type char = Char16.char
type string = String16.string
val minChar : char = #"\000"
val maxChar : char = #"\uFFFF"
val maxOrd = 0xFFFF
val ord : char -> int = Unsafe.cast
fun chr (x : int) : char = if 0 <= x andalso x <= 0xFFFF then
                               Unsafe.cast x
                           else
                               raise Chr
fun succ c = chr (ord c + 1)
fun pred c = chr (ord c - 1)
fun compare (x : char, y : char) = if x = y then
                                       EQUAL
                                   else if x < y then
                                       LESS
                                   else
                                       GREATER
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
fun toLower (c : char) = if isUpper c then
                             chr (ord c - (ord #"A" - ord #"a"))
                         else
                             c
fun toUpper (c : char) = if isLower c then
                             chr (ord c - (ord #"a" - ord #"A"))
                         else
                             c
open Char16
end

structure String16 = struct
open String16
fun str (c : Char16.char) : String16.string
  = let val i = Char16.ord c
        (* big endian *)
        val a = Int.div (i, 256)
        val b = Int.mod (i, 256)
    in Lua.unsafeFromValue (Lua.call1 Lua.Lib.string.char #[Lua.fromInt a, Lua.fromInt b])
    end
fun compare (s, t) = if s = t then
                         EQUAL
                     else if s < t then
                         LESS
                     else
                         GREATER
end

local
    open ScanTextUtils
    fun scanChar (getc, strm : 'strm) : ('strm, Char16.char) ScanCharResult
        = case getc strm of
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
                                                                                                  Parsed (Char16.chr (r - 64), strm''')
                                                                                              else
                                                                                                  Error
                                                                                           end
                                                                    | NONE => Error
                                                                 )
                                        | SOME (#"u", strm'') => (case scanHexadecimalDigits (getc, strm'', 4, 0) of
                                                                      SOME (value, strm''') => if value <= Char16.maxOrd then
                                                                                                   Parsed (Char16.chr value, strm''')
                                                                                               else
                                                                                                   Error
                                                                    | NONE => Error
                                                                 )
                                        | SOME (c, strm'') => if Char.isDigit c then
                                                                  case scanDecimalDigits (getc, strm'', 2, digitToInt c) of
                                                                      SOME (value, strm''') => if value <= Char16.maxOrd then
                                                                                                   Parsed (Char16.chr value, strm''')
                                                                                               else
                                                                                                   Error
                                                                    | NONE => Error
                                                              else if Char.isSpace c then
                                                                  case skipSpaces (getc, strm'') of
                                                                      SOME strm''' => Skipped strm'''
                                                                    | NONE => Error
                                                              else
                                                                  Error
                                        | NONE => Error
                                     )
            | SOME (c, strm') => if Char.isPrint c then
                                     Parsed (Char16.chr (Char.ord c), strm')
                                 else
                                     Error
            | NONE => Empty
    fun scanCChar (getc, strm) : ('strm, Char16.char) ScanCharResult
        = case getc strm of
              SOME (#"\\", strm') => (case getc strm' of
                                          SOME (#"a", strm'') => Parsed (#"\a", strm'')
                                        | SOME (#"b", strm'') => Parsed (#"\b", strm'')
                                        | SOME (#"t", strm'') => Parsed (#"\t", strm'')
                                        | SOME (#"n", strm'') => Parsed (#"\n", strm'')
                                        | SOME (#"v", strm'') => Parsed (#"\v", strm'')
                                        | SOME (#"f", strm'') => Parsed (#"\f", strm'')
                                        | SOME (#"r", strm'') => Parsed (#"\r", strm'')
                                        | SOME (#"?", strm'') => Parsed (#"?", strm'')
                                        | SOME (#"\\", strm'') => Parsed (#"\\", strm'')
                                        | SOME (#"\"", strm'') => Parsed (#"\"", strm'')
                                        | SOME (#"'", strm'') => Parsed (#"'", strm'')
                                        | SOME (#"^", strm'') => (case getc strm'' of
                                                                      SOME (c, strm''') => let val r = Char.ord c
                                                                                           in if 64 <= r andalso r <= 95 then
                                                                                                  Parsed (Char16.chr (r - 64), strm''')
                                                                                              else
                                                                                                  Error
                                                                                           end
                                                                    | NONE => Error
                                                                 )
                                        | SOME (#"x", strm'') => (case getc strm'' of
                                                                      SOME (c, strm''') => (case scanHexadecimalDigits' (getc, strm''', digitToInt c) of
                                                                                                SOME (value, strm'''') => if value <= Char.maxOrd then
                                                                                                                              Parsed (Char16.chr value, strm'''')
                                                                                                                          else
                                                                                                                              Error
                                                                                              | NONE => Error
                                                                                           )
                                                                    | NONE => Error
                                                                 )
                                        | SOME (#"u", strm'') => (case scanHexadecimalDigits (getc, strm'', 4, 0) of
                                                                      SOME (value, strm''') => if value <= Char16.maxOrd then
                                                                                                   Parsed (Char16.chr value, strm''')
                                                                                               else
                                                                                                   Error
                                                                    | NONE => Error
                                                                 )
                                        | SOME (c, strm'') => if isOctDigit c then
                                                                  case scanOctalDigits (getc, strm'', 2, digitToInt c) of
                                                                      (value, strm''') => if value <= Char16.maxOrd then
                                                                                              Parsed (Char16.chr value, strm''')
                                                                                          else
                                                                                              Error
                                                              else
                                                                  Error
                                        | NONE => Error
                                     )
            | SOME (#"\"", strm') => Error
            | SOME (c, strm') => if Char.isPrint c then
                                     Parsed (Char16.chr (Char.ord c), strm')
                                 else
                                     Error
            | NONE => Empty
in
structure UnsafeString16 = struct
fun sub (s : String16.string, i : int) : Char16.char
  = let val op + = Lua.+
        val op * = Lua.*
        val ii = Lua.fromInt 2 * Lua.fromInt i
        val (a, b) = Lua.call2 Lua.Lib.string.byte #[Lua.unsafeToValue s, ii + Lua.fromInt 1, ii + Lua.fromInt 2]
    in Lua.unsafeFromValue (Lua.fromInt 256 * a + b) (* big endian *)
    end
end;

structure Char16 :> CHAR where type char = Char16.char
                         where type string = String16.string
  = struct
fun contains (s : String16.string) (c : Char16.char) : bool
    = let fun go i = if i >= String16.size s then
                         false
                     else if UnsafeString16.sub (s, i) = c then
                         true
                     else
                         go (i + 1)
      in go 0
      end
fun notContains s c = not (contains s c)
fun toString #"\\" = "\\\\"
  | toString #"\"" = "\\\""
  | toString c = if Char16.isPrint c then
                     String.str (Char.chr (Char16.ord c)) (* the character must be ASCII *)
                 else
                     case c of
                         #"\a" => "\\a"
                       | #"\b" => "\\b"
                       | #"\t" => "\\t"
                       | #"\n" => "\\n"
                       | #"\v" => "\\v"
                       | #"\f" => "\\f"
                       | #"\r" => "\\r"
                       | _ => let val x = Char16.ord c
                              in if x < 32 then
                                     "\\^" ^ String.str (Char.chr (x + 64))
                                 else if x < 1000 then
                                     Lua.unsafeFromValue (Lua.call1 Lua.Lib.string.format #[Lua.fromString "\\%03d", Lua.fromInt x])
                                 else
                                     Lua.unsafeFromValue (Lua.call1 Lua.Lib.string.format #[Lua.fromString "\\u%04X", Lua.fromInt x])
                              end
fun toCString #"\\" = "\\\\"
  | toCString #"\"" = "\\\""
  | toCString #"?" = "\\?"
  | toCString #"'" = "\\'"
  | toCString c = if Char16.isPrint c then
                      String.str (Char.chr (Char16.ord c)) (* the character must be ASCII *)
                  else
                      case c of
                          #"\a" => "\\a"
                        | #"\b" => "\\b"
                        | #"\t" => "\\t"
                        | #"\n" => "\\n"
                        | #"\v" => "\\v"
                        | #"\f" => "\\f"
                        | #"\r" => "\\r"
                        | _ => let val x = Char16.ord c
                               in if x < 512 then
                                      Lua.unsafeFromValue (Lua.call1 Lua.Lib.string.format #[Lua.fromString "\\%03o", Lua.fromInt x])
                                  else
                                      Lua.unsafeFromValue (Lua.call1 Lua.Lib.string.format #[Lua.fromString "\\u%04X", Lua.fromInt x])
                               end
fun scan getc strm = case scanChar (getc, strm) of
                         Parsed (c, strm') => SOME (c, strm')
                       | Skipped strm' => scan getc strm'
                       | Error => NONE
                       | Empty => NONE
fun fromString s = StringCvt.scanString scan s
fun scanC getc strm = case scanCChar (getc, strm) of
                          Parsed (c, strm') => SOME (c, strm')
                        | Skipped strm' => scanC getc strm' (* cannot happen *)
                        | Error => NONE
                        | Empty => NONE
fun fromCString s = StringCvt.scanString scanC s
open Char16 (* type char, type string, minChar, maxChar, maxOrd, ord, chr, succ, pred, <, <=, >, >=, compare, isAscii, isUpper, isLower, isDigit, isAlpha, isAlphaNum, isHexDigit, isGraph, isPrint, isPunct, isCntrl, isSpace, toLower, toUpper *)
end;

structure String16 = struct
val maxSize = 0x7fffffff
fun sub (s : String16.string, i : int) : Char16.char
    = if 0 <= i andalso i < String16.size s then
          UnsafeString16.sub (s, i)
      else
          raise Subscript
fun substring (s : String16.string, i : int, j : int) : String16.string
    = if i < 0 orelse j < 0 orelse String16.size s < i + j then
          raise Subscript
      else
          let val result = Lua.call1 Lua.Lib.string.sub #[Lua.unsafeToValue s, Lua.fromInt (i + i + 1), Lua.fromInt (2 * (i + j))]
          in Lua.unsafeFromValue result
          end
fun extract (s : String16.string, i : int, NONE : int option) : String16.string
    = if i < 0 orelse String16.size s < i then
          raise Subscript
      else
          let val result = Lua.call1 Lua.Lib.string.sub #[Lua.unsafeToValue s, Lua.fromInt (i + i + 1)]
          in Lua.unsafeFromValue result
          end
  | extract (s, i, SOME j) = substring (s, i, j)
fun concat (l : String16.string list) : String16.string
    = let val result = Lua.call1 Lua.Lib.table.concat #[Lua.unsafeToValue (Vector.fromList l)]
      in Lua.unsafeFromValue result
      end
fun concatWith (s : String16.string) (l : String16.string list) : String16.string
    = let val result = Lua.call1 Lua.Lib.table.concat #[Lua.unsafeToValue (Vector.fromList l), Lua.unsafeToValue s]
      in Lua.unsafeFromValue result
      end
fun implodeRev (l : Char16.char list) : String16.string
    = let val v = _primCall "Vector.unsafeFromListRevN" (List.length l, l)
          val result = Lua.call1 Lua.Lib.table.concat #[Lua.unsafeToValue (Vector.map String16.str v)]
      in Lua.unsafeFromValue result
      end
fun implode (l : Char16.char list) : String16.string
    = let val result = Lua.call1 Lua.Lib.table.concat #[Lua.unsafeToValue (Vector.map String16.str (Vector.fromList l))]
      in Lua.unsafeFromValue result
      end
fun explode (s : String16.string) : Char16.char list
    = Vector.foldr (op ::) [] (Vector.tabulate (String16.size s, fn i => sub (s, i)))
fun translate (f : Char16.char -> String16.string) (s : String16.string) : String16.string
    = let val n = String16.size s
          fun go i = if i >= n then
                         []
                     else
                         f (sub (s, i)) :: go (i + 1)
      in concat (go 0)
      end
fun map (f : Char16.char -> Char16.char) (s : String16.string) : String16.string
    = let val n = String16.size s
          fun go i = if i >= n then
                         []
                     else
                         String16.str (f (sub (s, i))) :: go (i + 1)
      in concat (go 0)
      end
fun tokens f s = let fun go (revTokens, acc, []) = List.rev (if List.null acc then revTokens else implodeRev acc :: revTokens)
                       | go (revTokens, acc, x :: xs) = if f x then
                                                            go (if List.null acc then revTokens else implodeRev acc :: revTokens, [], xs)
                                                        else
                                                            go (revTokens, x :: acc, xs)
                 in go ([], [], explode s)
                 end
fun fields f s = let fun go (revFields, acc, []) = List.rev (implodeRev acc :: revFields)
                       | go (revFields, acc, x :: xs) = if f x then
                                                            go (implodeRev acc :: revFields, [], xs)
                                                        else
                                                            go (revFields, x :: acc, xs)
                 in go ([], [], explode s)
                 end
fun isPrefix prefix s = let val n = String16.size prefix
                        in if n > String16.size s then
                               false
                           else
                               substring (s, 0, n) = prefix
                        end
fun isSubstring needle haystack : bool
  = let val m = String16.size needle
        val n = String16.size haystack - m
        fun go i = if i > n then
                       false
                   else if substring (haystack, i, m) = needle then
                       true
                   else
                       go (i + 1)
    in go 0
    end
fun isSuffix suffix s = let val n = String16.size suffix
                            val m = String16.size s
                        in if n > m then
                               false
                           else
                               substring (s, m - n, n) = suffix
                        end
fun toString s = String.concat (List.map Char16.toString (explode s))
fun toCString s = String.concat (List.map Char16.toCString (explode s))
fun scan getc strm = let fun go (strm, revAcc) = case scanChar (getc, strm) of
                                                     Parsed (c, strm') => go (strm', c :: revAcc)
                                                   | Skipped strm' => go (strm', revAcc)
                                                   | Error => SOME (implodeRev revAcc, strm)
                                                   | Empty => SOME (implodeRev revAcc, strm)
                     in case scanChar (getc, strm) of
                            Parsed (c, strm') => go (strm', [c])
                          | Skipped strm' => go (strm', [])
                          | Error => NONE
                          | Empty => SOME ("", strm)
                     end
fun fromString s = StringCvt.scanString scan s
fun scanC getc strm = let fun go (strm, revAcc) = case scanCChar (getc, strm) of
                                                      Parsed (c, strm') => go (strm', c :: revAcc)
                                                    | Skipped strm' => go (strm', revAcc) (* cannot happen *)
                                                    | Error => SOME (implodeRev revAcc, strm)
                                                    | Empty => SOME (implodeRev revAcc, strm)
                      in case scanCChar (getc, strm) of
                             Parsed (c, strm') => go (strm', [c])
                           | Skipped strm' => go (strm', []) (* cannot happen *)
                           | Error => NONE
                           | Empty => SOME ("", strm)
                      end
fun fromCString s = StringCvt.scanString scanC s
open String16 (* type string, type char, <, <=, >, >=, ^, size, str, compare *)
end (* structure String16 *)
end; (* local *)

structure Text16Impl :> sig
              include TEXT
              structure UnsafeCharVector : UNSAFE_MONO_VECTOR where type elem = Char.char where type vector = CharVector.vector
              structure UnsafeCharArray : UNSAFE_MONO_ARRAY where type elem = Char.char where type array = CharArray.array
          end where type Char.char = Char16.char
              where type String.string = String16.string
  = struct
local
    structure Prim : MONO_SEQUENCE_PRIM = struct
    type elem = Char16.char
    type vector = String16.string
    type array = Char16.char Array.array (* TODO: Use Uint16Array *)
    structure MonoVector = struct
    val maxLen = String16.maxSize
    val length = String16.size
    val unsafeSub = UnsafeString16.sub
    val fromList = String16.implode
    fun unsafeFromListN (n, xs) = String16.implode xs (* TODO *)
    fun unsafeFromListRevN (n, xs) = String16.implodeRev xs (* TODO *)
    val concat = String16.concat
    fun sliceToVector { base, start, length } = String16.substring (base, start, length)
    val shallowSliceToVector = sliceToVector
    end
    structure MonoArray = struct
    val maxLen = Array.maxLen
    val eq = op = : array * array -> bool
    val length = Array.length
    fun unsafeCreateWithZero n = Array.array (n, #"\000" : Char16.char) (* TODO *)
    val unsafeCreate = Array.array (* TODO *)
    val fromList = Array.fromList
    fun unsafeFromListN (n, xs) = fromList xs (* TODO *)
    val unsafeSub = Unsafe.Array.sub
    val unsafeUpdate = Unsafe.Array.update
    end
    end
    structure Base = MonoSequence (Prim)
in
structure CharVector = Base.MonoVector
structure CharVectorSlice = Base.MonoVectorSlice
structure CharArray = Base.MonoArray
structure CharArraySlice = Base.MonoArraySlice
structure UnsafeCharVector = Base.UnsafeMonoVector
structure UnsafeCharArray = Base.UnsafeMonoArray
structure String = struct
val collate = CharVector.collate
open String16
end
structure Substring = struct
type char = Char16.char
type string = String16.string
type substring = CharVectorSlice.slice
val sub = CharVectorSlice.sub
val size = CharVectorSlice.length
val base = CharVectorSlice.base
val extract = CharVectorSlice.slice
fun substring (base, start, length) = if 0 <= start andalso 0 <= length andalso start + length <= String16.size base then
                                          { base = base, start = start, length = length }
                                      else
                                          raise Subscript
val full = CharVectorSlice.full
val string = CharVectorSlice.vector
val isEmpty = CharVectorSlice.isEmpty
val getc = CharVectorSlice.getItem
fun first { base, start, length } = if length = 0 then
                                        NONE
                                    else
                                        SOME (String16.sub (base, start))
fun triml k = if k < 0 then
                  raise Subscript
              else
                  fn { base, start, length } => if k < length then
                                                    { base = base, start = start + k, length = length - k }
                                                else
                                                    { base = base, start = start + length, length = 0 }
fun trimr k = if k < 0 then
                  raise Subscript
              else
                  fn { base, start, length } => if k < length then
                                                    { base = base, start = start, length = length - k }
                                                else
                                                    { base = base, start = start, length = 0 }
val slice = CharVectorSlice.subslice
val concat = CharVectorSlice.concat
fun concatWith s xs = String16.concatWith s (List.map string xs)
fun explode { base, start, length } = let fun loop (i, acc) = if i < start then
                                                                  acc
                                                              else
                                                                  loop (i - 1, String16.sub (base, i) :: acc)
                                      in loop (start + length - 1, [])
                                      end
fun isPrefix s ss = String16.isPrefix s (string ss)
fun isSuffix s ss = String16.isSuffix s (string ss)
fun compare (s, t) = String16.compare (string s, string t)
val collate = CharVectorSlice.collate
fun splitl f (s as { base, start, length }) = let fun loop j = if j >= length then
                                                                   (s, { base = base, start = start + j, length = 0 })
                                                               else if f (sub (s, j)) then
                                                                   loop (j + 1)
                                                               else
                                                                   ({ base = base, start = start, length = j }, { base = base, start = start + j, length = length - j })
                                              in loop 0
                                              end
fun splitr f (s as { base, start, length }) = let fun loop j = if j < 0 then
                                                                   ({ base = base, start = start, length = 0 }, s)
                                                               else if f (sub (s, j)) then
                                                                   loop (j - 1)
                                                               else
                                                                   let val j' = j + 1
                                                                   in ({ base = base, start = start, length = j' }, { base = base, start = start + j', length = length - j' })
                                                                   end
                                              in loop (length - 1)
                                              end
fun splitAt ({ base, start, length }, i) = if 0 <= i andalso i <= length then
                                               ({ base = base, start = start, length = i }, {base = base, start = start + i, length = length - i })
                                           else
                                               raise Subscript
fun dropl p s = #2 (splitl p s)
fun dropr p s = #1 (splitr p s)
fun takel p s = #1 (splitl p s)
fun taker p s = #2 (splitr p s)
fun translate f s = String16.concat (List.map f (explode s))
fun tokens f { base, start, length }
    = let val n = start + length
          fun loop (revTokens, s, i) = if i >= n then
                                           List.rev (if s = i then revTokens else { base = base, start = s, length = i - s } :: revTokens)
                                       else if f (String16.sub (base, i)) then
                                           let val ip1 = i + 1
                                           in loop (if s = i then revTokens else { base = base, start = s, length = i - s } :: revTokens, ip1, ip1)
                                           end
                                       else
                                           loop (revTokens, s, i + 1)
      in loop ([], start, start)
      end
fun fields f { base, start, length }
    = let val n = start + length
          fun loop (revFields, s, i) = if i >= n then
                                           List.rev ({ base = base, start = s, length = i - s } :: revFields)
                                       else if f (String16.sub (base, i)) then
                                           let val ip1 = i + 1
                                           in loop ({base = base, start = s, length = i - s} :: revFields, ip1, ip1)
                                           end
                                       else
                                           loop (revFields, s, i + 1)
      in loop ([], start, start)
          end
val app = CharVectorSlice.app
val foldl = CharVectorSlice.foldl
val foldr = CharVectorSlice.foldr
end (* structure Substring *)
end (* local *)
structure Char = Char16
end; (* structure Text16Impl *)
structure Text16 : TEXT = Text16Impl;
structure String16 = Text16.String;
structure Substring16 = Text16.Substring;
structure Char16Vector = Text16.CharVector;
structure Char16Array = Text16.CharArray;
structure Char16VectorSlice = Text16.CharVectorSlice;
structure Char16ArraySlice = Text16.CharArraySlice;
structure Unsafe : sig
              structure Vector : sig
                            val sub : 'a vector * int -> 'a
                        end
              structure Array : sig
                            val sub : 'a array * int -> 'a
                            val update : 'a array * int * 'a -> {}
                        end
              structure BoolVector : UNSAFE_MONO_VECTOR where type elem = bool where type vector = BoolVector.vector
              structure BoolArray : UNSAFE_MONO_ARRAY where type elem = bool where type array = BoolArray.array
              structure CharVector : UNSAFE_MONO_VECTOR where type elem = Char.char where type vector = CharVector.vector
              structure CharArray : UNSAFE_MONO_ARRAY where type elem = Char.char where type array = CharArray.array
              structure Char16Vector : UNSAFE_MONO_VECTOR where type elem = Char16.char where type vector = Char16Vector.vector
              structure Char16Array : UNSAFE_MONO_ARRAY where type elem = Char16.char where type array = Char16Array.array
              structure IntVector : UNSAFE_MONO_VECTOR where type elem = Int.int where type vector = IntVector.vector
              structure IntArray : UNSAFE_MONO_ARRAY where type elem = Int.int where type array = IntArray.array
              structure Int8Vector : UNSAFE_MONO_VECTOR where type elem = Int8.int where type vector = Int8Vector.vector
              structure Int8Array : UNSAFE_MONO_ARRAY where type elem = Int8.int where type array = Int8Array.array
              structure Int16Vector : UNSAFE_MONO_VECTOR where type elem = Int16.int where type vector = Int16Vector.vector
              structure Int16Array : UNSAFE_MONO_ARRAY where type elem = Int16.int where type array = Int16Array.array
              structure Int32Vector : UNSAFE_MONO_VECTOR where type elem = Int32.int where type vector = Int32Vector.vector
              structure Int32Array : UNSAFE_MONO_ARRAY where type elem = Int32.int where type array = Int32Array.array
              structure Int64Vector : UNSAFE_MONO_VECTOR where type elem = Int64.int where type vector = Int64Vector.vector
              structure Int64Array : UNSAFE_MONO_ARRAY where type elem = Int64.int where type array = Int64Array.array
              structure WordVector : UNSAFE_MONO_VECTOR where type elem = Word.word where type vector = WordVector.vector
              structure WordArray : UNSAFE_MONO_ARRAY where type elem = Word.word where type array = WordArray.array
              structure Word8Vector : UNSAFE_MONO_VECTOR where type elem = Word8.word where type vector = Word8Vector.vector
              structure Word8Array : UNSAFE_MONO_ARRAY where type elem = Word8.word where type array = Word8Array.array
              structure Word16Vector : UNSAFE_MONO_VECTOR where type elem = Word16.word where type vector = Word16Vector.vector
              structure Word16Array : UNSAFE_MONO_ARRAY where type elem = Word16.word where type array = Word16Array.array
              structure Word32Vector : UNSAFE_MONO_VECTOR where type elem = Word32.word where type vector = Word32Vector.vector
              structure Word32Array : UNSAFE_MONO_ARRAY where type elem = Word32.word where type array = Word32Array.array
              structure Word64Vector : UNSAFE_MONO_VECTOR where type elem = Word64.word where type vector = Word64Vector.vector
              structure Word64Array : UNSAFE_MONO_ARRAY where type elem = Word64.word where type array = Word64Array.array
              structure RealVector : UNSAFE_MONO_VECTOR where type elem = Real.real where type vector = RealVector.vector
              structure RealArray : UNSAFE_MONO_ARRAY where type elem = Real.real where type array = RealArray.array
              val cast : 'a -> 'b
          end = struct
structure Char16Vector = Text16Impl.UnsafeCharVector
structure Char16Array = Text16Impl.UnsafeCharArray
open Unsafe
end;
