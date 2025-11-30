structure Unicode :> UNICODE where type uchar = UChar.uchar = struct
  type ustring = String16.string
  type uchar = UChar.uchar
  type usubstring = Substring16.substring
  structure UTF8 = struct
    type uchar = UChar.uchar
    type ustring = ustring
    type usubstring = usubstring
    open UTF8 (* type string, type substring, isWellFormed, isWellFormedSubstring, encode1, decode1 *)
    fun encode x = _primCall "JavaScript.encodeUtf8" (x)
    fun encodeSubstring x = _primCall "JavaScript.encodeUtf8" (Substring16.string x)
    fun decode x = _primCall "JavaScript.decodeUtf8" (x)
    fun validate x = if isWellFormed x then SOME (decode x) else NONE
    structure Unsafe = struct
      fun decode x = _primCall "JavaScript.decodeUtf8" (x)
      val decode1 = decode1Raise
    end
  end
  structure UTF16 = struct
    type uchar = UChar.uchar
    type ustring = ustring
    type usubstring = usubstring
    open UTF16 (* type string, type substring, isWellFormed, isWellFormedSubstring, encode1, decode1 *)
    fun isWellFormed x = _primCall "UTF16.isWellFormed" (x)
    fun encode x = x (* no-op *)
    val encodeSubstring = Substring16.string
    fun decode x = _primCall "UTF16.toWellFormed" (x)
    fun validate x = if isWellFormed x then SOME x else NONE
    fun encode1 x = _primCall "UTF16.str" (x) (* String.fromCodePoint *)
    fun decode1 ss =
      let val (s, i, n) = Substring16.base ss
      in
        if n > 0 then
          let val c = _primCall "UTF16.codePointAt" (s, i)
          in
            if c >= #"\U00010000" then
              SCALAR (UChar.Unsafe.fromChar32 c, Substring16.substring (s, i + 2, n - 2))
            else if #"\uD800" <= c andalso c < #"\uE000" then
              INVALID (Char16.chr (Char32.ord c), Substring16.substring (s, i + 1, n - 1))
            else
              SCALAR (UChar.Unsafe.fromChar32 c, Substring16.substring (s, i + 1, n - 1))
          end
        else
          END
      end
    fun decode1Replace s = case decode1 s of
        SCALAR (c, s') => SOME (c, s')
      | INVALID (_, s') => SOME (#"\uFFFD", s')
      | END => NONE
    fun decode1Raise s = case decode1 s of
        SCALAR (c, s') => SOME (c, s')
      | INVALID (_, s') => raise Chr
      | END => NONE
    structure Unsafe = struct
      fun decode x = x
      fun decode1 ss =
        let val (s, i, n) = Substring16.base ss
        in
          if n > 0 then
            let val c = _primCall "UTF16.codePointAt" (s, i)
            in
              if c >= #"\U00010000" then
                SOME (UChar.Unsafe.fromChar32 c, Substring16.substring (s, i + 2, n - 2))
              else
                SOME (UChar.Unsafe.fromChar32 c, Substring16.substring (s, i + 1, n - 1))
            end
          else
            NONE
        end
    end
  end
  structure USubstring = struct
    val full = Substring16.full
    val string = Substring16.string
    val isEmpty = Substring16.isEmpty
    val getc = UTF16.Unsafe.decode1
    fun foldl f init s =
      let fun go (acc, s) = case UTF16.Unsafe.decode1 s of
                NONE => acc
              | SOME (c, s') => go (f (c, acc), s')
      in go (init, s)
      end
    fun size s = foldl (fn (_, n) => n + 1) 0 s
    fun explode s = List.rev (foldl (op ::) [] s)
    fun map f s = String16.concat (List.rev (foldl (fn (c, acc) => UTF16.encode1 (f c) :: acc) [] s))
    fun app f =
      let fun go s = case UTF16.Unsafe.decode1 s of
                NONE => ()
              | SOME (c, s') => (f c; go s')
      in go
      end
    fun all f =
      let fun go s = case UTF16.Unsafe.decode1 s of
                NONE => true
              | SOME (c, s') => f c andalso go s'
      in go
      end
    fun exists f =
      let fun go s = case UTF16.Unsafe.decode1 s of
                NONE => false
              | SOME (c, s') => f c orelse go s'
      in go
      end
    val concat = Substring16.concat
    val concatWith = Substring16.concatWith
  end
  structure UTF32 = struct
    type uchar = UChar.uchar
    type ustring = ustring
    type usubstring = usubstring
    open UTF32 (* type string, type substring, isWellFormed, isWellFormedSubstring, encode1, decode1 *)
    fun encodeSubstring s = String32.concat (List.rev (USubstring.foldl (fn (c, acc) => String32.str (UChar.toChar32 c) :: acc) [] s))
    fun encode s = encodeSubstring (Substring16.full s)
    fun decode s =
      String16.concat (Char32Vector.foldr (fn (c, acc) =>
        if #"\uD800" <= c andalso c < #"\uE000" then
          "\uFFFD" :: acc
        else
          _primCall "UTF16.str" (UChar.Unsafe.fromChar32 c) :: acc
      ) [] s)
    structure Unsafe = struct
      fun decode s =
        String16.concat (Char32Vector.foldr (fn (c, acc) =>
          _primCall "UTF16.str" (UChar.Unsafe.fromChar32 c) :: acc
        ) [] s)
      open Unsafe (* decode1 *)
    end
    fun validate x = if isWellFormed x then SOME (Unsafe.decode x) else NONE
  end
  structure String = struct
    type ('string, 'substring) encoding =
      { uncheckedDecode : 'string -> ustring
      , uncheckedDecodeSubstring : 'substring -> usubstring
      , encodeSubstring : usubstring -> 'substring
      }
    datatype representation = UTF8 of (String.string, Substring.substring) encoding
                            | UTF16 of (String16.string, Substring16.substring) encoding
                            | UTF32 of (String32.string, Substring32.substring) encoding
    val representation = UTF16 { uncheckedDecode = UTF16.Unsafe.decode
                               , uncheckedDecodeSubstring = fn x => x
                               , encodeSubstring = fn x => x
                               }
    fun fromAscii x = _primCall "String16.fromString7" (x)
    fun x < y = _primCall "UTF16.<" (x, y)
    fun x <= y = not (_primCall "UTF16.<" (y, x))
    fun x > y = _primCall "UTF16.<" (y, x)
    fun x >= y = not (_primCall "UTF16.<" (x, y))
    val op ^ = String16.^
    fun str x = _primCall "UTF16.str" (x)
    fun foldl f init s = USubstring.foldl f init (Substring16.full s)
    fun size s = foldl (fn (_, n) => n + 1) 0 s
    fun explode s = List.rev (foldl (op ::) [] s)
    fun implode xs = String16.concat (List.map UTF16.encode1 xs)
    fun map f s = String16.concat (List.rev (foldl (fn (c, acc) => UTF16.encode1 (f c) :: acc) [] s))
    fun app f s = USubstring.app f (Substring16.full s)
    fun all f s = USubstring.all f (Substring16.full s)
    fun exists f s = USubstring.exists f (Substring16.full s)
    val concat = String16.concat
    val concatWith = String16.concatWith
  end
  structure Substring = USubstring
  structure Char = UChar
end
_overload "UnicodeString" [Unicode.ustring] { < = Unicode.String.<
                                            , <= = Unicode.String.<=
                                            , > = Unicode.String.>
                                            , >= = Unicode.String.>=
                                            , maxOrd = 0x10ffff
                                            , maxCodeUnit = 0xffff
                                            };
