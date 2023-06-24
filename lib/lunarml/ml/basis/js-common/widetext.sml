structure WideChar = struct
type char = WideChar.char
type string = WideString.string
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
open WideChar
end

structure WideString = struct
open WideString
fun compare (s, t) = if s = t then
                         EQUAL
                     else if s < t then
                         LESS
                     else
                         GREATER
end

local
    open ScanTextUtils
    fun scanChar (getc, strm : 'strm) : ('strm, WideChar.char) ScanCharResult
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
                                                                                                  Parsed (WideChar.chr (r - 64), strm''')
                                                                                              else
                                                                                                  Error
                                                                                           end
                                                                    | NONE => Error
                                                                 )
                                        | SOME (#"u", strm'') => (case scanHexadecimalDigits (getc, strm'', 4, 0) of
                                                                      SOME (value, strm''') => if value <= WideChar.maxOrd then
                                                                                                   Parsed (WideChar.chr value, strm''')
                                                                                               else
                                                                                                   Error
                                                                    | NONE => Error
                                                                 )
                                        | SOME (c, strm'') => if Char.isDigit c then
                                                                  case scanDecimalDigits (getc, strm'', 2, digitToInt c) of
                                                                      SOME (value, strm''') => if value <= WideChar.maxOrd then
                                                                                                   Parsed (WideChar.chr value, strm''')
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
                                     Parsed (WideChar.chr (Char.ord c), strm')
                                 else
                                     Error
            | NONE => Empty
    fun scanCChar (getc, strm) : ('strm, WideChar.char) ScanCharResult
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
                                                                                                  Parsed (WideChar.chr (r - 64), strm''')
                                                                                              else
                                                                                                  Error
                                                                                           end
                                                                    | NONE => Error
                                                                 )
                                        | SOME (#"x", strm'') => (case getc strm'' of
                                                                      SOME (c, strm''') => (case scanHexadecimalDigits' (getc, strm''', digitToInt c) of
                                                                                                SOME (value, strm'''') => if value <= Char.maxOrd then
                                                                                                                              Parsed (WideChar.chr value, strm'''')
                                                                                                                          else
                                                                                                                              Error
                                                                                              | NONE => Error
                                                                                           )
                                                                    | NONE => Error
                                                                 )
                                        | SOME (#"u", strm'') => (case scanHexadecimalDigits (getc, strm'', 4, 0) of
                                                                      SOME (value, strm''') => if value <= WideChar.maxOrd then
                                                                                                   Parsed (WideChar.chr value, strm''')
                                                                                               else
                                                                                                   Error
                                                                    | NONE => Error
                                                                 )
                                        | SOME (c, strm'') => if isOctDigit c then
                                                                  case scanOctalDigits (getc, strm'', 2, digitToInt c) of
                                                                      (value, strm''') => if value <= WideChar.maxOrd then
                                                                                              Parsed (WideChar.chr value, strm''')
                                                                                          else
                                                                                              Error
                                                              else
                                                                  Error
                                        | NONE => Error
                                     )
            | SOME (#"\"", strm') => Error
            | SOME (c, strm') => if Char.isPrint c then
                                     Parsed (WideChar.chr (Char.ord c), strm')
                                 else
                                     Error
            | NONE => Empty
in
structure WideChar :> CHAR where type char = WideChar.char
                           where type string = WideString.string
  = struct
fun contains (s : WideString.string) (c : WideChar.char) : bool
    = JavaScript.unsafeFromValue (JavaScript.method (JavaScript.fromWideString s, "includes") #[JavaScript.fromWideString (WideString.str c)])
fun notContains s c = not (contains s c)
fun toString #"\\" = "\\\\"
  | toString #"\"" = "\\\""
  | toString c = if WideChar.isPrint c then
                     String.str (Char.chr (WideChar.ord c)) (* the character must be ASCII *)
                 else
                     case c of
                         #"\a" => "\\a"
                       | #"\b" => "\\b"
                       | #"\t" => "\\t"
                       | #"\n" => "\\n"
                       | #"\v" => "\\v"
                       | #"\f" => "\\f"
                       | #"\r" => "\\r"
                       | _ => let val x = WideChar.ord c
                              in if x < 32 then
                                     "\\^" ^ String.str (Char.chr (x + 64))
                                 else if x < 100 then
                                     "\\0" ^ Int.toString x
                                 else if x < 1000 then
                                     "\\" ^ Int.toString x
                                 else if x < 0x1000 (* 4096 *) then
                                     "\\u0" ^ Int.fmt StringCvt.HEX x
                                 else
                                     "\\u" ^ Int.fmt StringCvt.HEX x
                              end
fun toCString #"\\" = "\\\\"
  | toCString #"\"" = "\\\""
  | toCString #"?" = "\\?"
  | toCString #"'" = "\\'"
  | toCString c = if WideChar.isPrint c then
                      String.str (Char.chr (WideChar.ord c)) (* the character must be ASCII *)
                  else
                      case c of
                          #"\a" => "\\a"
                        | #"\b" => "\\b"
                        | #"\t" => "\\t"
                        | #"\n" => "\\n"
                        | #"\v" => "\\v"
                        | #"\f" => "\\f"
                        | #"\r" => "\\r"
                        | _ => let val x = WideChar.ord c
                               in if x < 8 then
                                      "\\00" ^ Int.fmt StringCvt.OCT x
                                  else if x < 64 then
                                      "\\0" ^ Int.fmt StringCvt.OCT x
                                  else if x < 512 then
                                      "\\" ^ Int.fmt StringCvt.OCT x
                                  else if x < 0x1000 (* 4096 *) then
                                      "\\u0" ^ Int.fmt StringCvt.HEX x
                                  else
                                      "\\u" ^ Int.fmt StringCvt.HEX x
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
open WideChar (* type char, type string, minChar, maxChar, maxOrd, ord, chr, succ, pred, <, <=, >, >=, compare, isAscii, isUpper, isLower, isDigit, isAlpha, isAlphaNum, isHexDigit, isGraph, isPrint, isPunct, isCntrl, isSpace, toLower, toUpper *)
end;

structure WideString :> STRING where type string = WideString.string
                               where type char = WideChar.char
= struct
val maxSize = 0x7fffffff
fun sub (s : WideString.string, i : int) : WideChar.char = JavaScript.unsafeFromValue (JavaScript.method (JavaScript.fromWideString s, "charCodeAt") #[JavaScript.fromInt i])
fun substring (s : WideString.string, i : int, j : int) : WideString.string
    = if i < 0 orelse j < 0 orelse WideString.size s < i + j then
          raise Subscript
      else
          JavaScript.unsafeFromValue (JavaScript.method (JavaScript.fromWideString s, "substring") #[JavaScript.fromInt i, JavaScript.fromInt (i + j)])
fun extract (s : WideString.string, i : int, NONE : int option) : WideString.string
    = if i < 0 orelse WideString.size s < i then
          raise Subscript
      else
          JavaScript.unsafeFromValue (JavaScript.method (JavaScript.fromWideString s, "substring") #[JavaScript.fromInt i])
  | extract (s, i, SOME j) = substring (s, i, j)
fun concat (l : WideString.string list) : WideString.string
    = let val v = Vector.fromList l
      in JavaScript.unsafeFromValue (JavaScript.method (JavaScript.unsafeToValue v, "join") #[JavaScript.fromWideString ""])
      end
fun concatWith (s : WideString.string) (l : WideString.string list) : WideString.string
    = let val v = Vector.fromList l
      in JavaScript.unsafeFromValue (JavaScript.method (JavaScript.unsafeToValue v, "join") #[JavaScript.fromWideString s])
      end
fun implodeRev (l : WideChar.char list) : WideString.string
    = let val v = _primCall "Vector.unsafeFromListRevN" (List.length l, l)
          val v = Vector.map WideString.str v
      in JavaScript.unsafeFromValue (JavaScript.method (JavaScript.unsafeToValue v, "join") #[JavaScript.fromWideString ""])
      end
fun implode (l : WideChar.char list) : WideString.string
    = let val v = Vector.fromList l
          val v = Vector.map WideString.str v
      in JavaScript.unsafeFromValue (JavaScript.method (JavaScript.unsafeToValue v, "join") #[JavaScript.fromWideString ""])
      end
fun explode (s : WideString.string) : WideChar.char list
    = Vector.foldr (op ::) [] (Vector.tabulate (WideString.size s, fn i => sub (s, i)))
fun translate (f : WideChar.char -> WideString.string) (s : WideString.string) : WideString.string
    = let val n = WideString.size s
          fun go i = if i >= n then
                         []
                     else
                         f (sub (s, i)) :: go (i + 1)
      in concat (go 0)
      end
fun map (f : WideChar.char -> WideChar.char) (s : WideString.string) : WideString.string
    = let val n = WideString.size s
          fun go i = if i >= n then
                         []
                     else
                         WideString.str (f (sub (s, i))) :: go (i + 1)
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
fun isPrefix prefix s = let val n = WideString.size prefix
                        in if n > WideString.size s then
                               false
                           else
                               substring (s, 0, n) = prefix
                        end
fun isSuffix suffix s = let val n = WideString.size suffix
                            val m = WideString.size s
                        in if n > m then
                               false
                           else
                               substring (s, m - n, n) = suffix
                        end
fun toString s = String.concat (List.map WideChar.toString (explode s))
fun toCString s = String.concat (List.map WideChar.toCString (explode s))
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
open WideString (* type string, type char, <, <=, >, >=, ^, size, str, compare *)
end (* structure WideString *)
end; (* local *)

structure WideText :> TEXT where type Char.char = WideChar.char
                           where type String.string = WideString.string
  = struct
local
    structure Prim : MONO_SEQUENCE_PRIM = struct
    type elem = WideChar.char
    type vector = WideString.string
    type array = WideChar.char Array.array (* TODO: Use Uint16Array *)
    structure MonoVector = struct
    val maxLen = WideString.maxSize
    val length = WideString.size
    val unsafeSub = WideString.sub (* TODO *)
    val fromList = WideString.implode
    fun unsafeFromListN (n, xs) = WideString.implode xs (* TODO *)
    fun unsafeFromListRevN (n, xs) = WideString.implodeRev xs (* TODO *)
    val concat = WideString.concat
    fun sliceToVector { base, start, length } = WideString.substring (base, start, length)
    val shallowSliceToVector = sliceToVector
    end
    structure MonoArray = struct
    val maxLen = Array.maxLen
    val eq = op = : array * array -> bool
    val length = Array.length
    fun unsafeCreateWithZero n = Array.array (n, #"\000" : WideChar.char) (* TODO *)
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
structure Substring = struct
type char = WideChar.char
type string = WideString.string
type substring = CharVectorSlice.slice
val sub = CharVectorSlice.sub
val size = CharVectorSlice.length
val base = CharVectorSlice.base
val extract = CharVectorSlice.slice
fun substring (base, start, length) = if 0 <= start andalso 0 <= length andalso start + length <= WideString.size base then
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
                                        SOME (WideString.sub (base, start))
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
fun concatWith s xs = WideString.concatWith s (List.map string xs)
fun explode { base, start, length } = let fun loop (i, acc) = if i < start then
                                                                  acc
                                                              else
                                                                  loop (i - 1, WideString.sub (base, i) :: acc)
                                      in loop (start + length - 1, [])
                                      end
fun isPrefix s ss = WideString.isPrefix s (string ss)
fun isSuffix s ss = WideString.isSuffix s (string ss)
fun compare (s, t) = WideString.compare (string s, string t)
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
fun translate f s = WideString.concat (List.map f (explode s))
fun tokens f { base, start, length }
    = let val n = start + length
          fun loop (revTokens, s, i) = if i >= n then
                                           List.rev (if s = i then revTokens else { base = base, start = s, length = i - s } :: revTokens)
                                       else if f (WideString.sub (base, i)) then
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
                                       else if f (WideString.sub (base, i)) then
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
structure Char = WideChar
structure String = WideString
end; (* structure WideText *)
structure WideSubstring = WideText.Substring;
structure WideCharVector = WideText.CharVector;
structure WideCharArray = WideText.CharArray;
structure WideCharVectorSlice = WideText.CharVectorSlice;
structure WideCharArraySlice = WideText.CharArraySlice;
