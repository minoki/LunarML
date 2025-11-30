structure Unicode :> UNICODE where type uchar = UChar.uchar = struct
  type ustring = String.string
  type uchar = UChar.uchar
  type usubstring = Substring.substring
  structure UTF8 = struct
    type uchar = UChar.uchar
    type ustring = ustring
    type usubstring = usubstring
    open UTF8 (* type string, type substring, isWellFormed, isWellFormedSubstring, encode1, decode1, decode1Replace, decode1Raise *)
    fun encode x = x (* no-op *)
    val encodeSubstring = Substring.string
    fun decode s =
      let fun go (acc, s) =
                case decode1 s of
                  END => String.concat (List.rev acc)
                | SCALAR (c, s') => go (encode1 c :: acc, s')
                | INVALID (_, s') => go ("\u{FFFD}" :: acc, s')
      in go ([], Substring.full s)
      end
    fun validate x = if isWellFormed x then SOME x else NONE
    structure Unsafe = struct
      fun decode x = x (* no-op *)
      val decode1 = decode1Raise
    end
  end
  structure USubstring = struct
    val full = Substring.full
    val string = Substring.string
    val isEmpty = Substring.isEmpty
    val getc = UTF8.Unsafe.decode1
    fun foldl f init s =
      let fun go (acc, s) = case UTF8.Unsafe.decode1 s of
                NONE => acc
              | SOME (c, s') => go (f (c, acc), s')
      in go (init, s)
      end
    fun size s = foldl (fn (_, n) => n + 1) 0 s
    fun explode s = List.rev (foldl (op ::) [] s)
    fun map f s = String.concat (List.rev (foldl (fn (c, acc) => UTF8.encode1 (f c) :: acc) [] s))
    fun app f =
      let fun go s = case UTF8.Unsafe.decode1 s of
                NONE => ()
              | SOME (c, s') => (f c; go s')
      in go
      end
    fun all f =
      let fun go s = case UTF8.Unsafe.decode1 s of
                NONE => true
              | SOME (c, s') => f c andalso go s'
      in go
      end
    fun exists f =
      let fun go s = case UTF8.Unsafe.decode1 s of
                NONE => false
              | SOME (c, s') => f c orelse go s'
      in go
      end
    val concat = Substring.concat
    val concatWith = Substring.concatWith
  end
  structure UTF16 = struct
    type uchar = UChar.uchar
    type ustring = ustring
    type usubstring = usubstring
    open UTF16 (* type string, type substring, isWellFormed, isWellFormedSubstring, encode1, decode1, decode1Replace, decode1Raise *)
    fun encodeSubstring s = String16.concat (List.rev (USubstring.foldl (fn (c, acc) => encode1 c :: acc) [] s))
    fun encode s = encodeSubstring (Substring.full s)
    fun decode s =
      let fun go (acc, s) =
                case decode1 s of
                  END => String.concat (List.rev acc)
                | SCALAR (c, s') => go (UTF8.encode1 c :: acc, s')
                | INVALID (_, s') => go ("\u{FFFD}" :: acc, s')
      in go ([], Substring16.full s)
      end
    fun validate x = if isWellFormed x then SOME (decode x) else NONE
    structure Unsafe = struct
      val decode = decode
      val decode1 = decode1Raise
    end
  end
  structure UTF32 = struct
    type uchar = UChar.uchar
    type ustring = ustring
    type usubstring = usubstring
    open UTF32 (* type string, type substring, isWellFormed, isWellFormedSubstring, encode1, decode1, decode1Replace, decode1Raise *)
    fun encodeSubstring s = String32.concat (List.rev (USubstring.foldl (fn (c, acc) => encode1 c :: acc) [] s))
    fun encode s = encodeSubstring (Substring.full s)
    fun decode s =
      String.concat (Char32Vector.foldr (fn (c, acc) =>
        if #"\uD800" <= c andalso c < #"\uE000" then
          "\u{FFFD}" :: acc
        else
          UTF8.encode1 (UChar.Unsafe.fromChar32 c) :: acc
      ) [] s)
    structure Unsafe = struct
      fun decode s =
        String.concat (Char32Vector.foldr (fn (c, acc) =>
          UTF8.encode1 (UChar.Unsafe.fromChar32 c) :: acc
        ) [] s)
      open Unsafe
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
    val representation = UTF8 { uncheckedDecode = UTF8.Unsafe.decode
                              , uncheckedDecodeSubstring = fn x => x
                              , encodeSubstring = fn x => x
                              }
    fun fromAscii x = _primCall "String.fromString7" (x)
    val op < = String.<
    val op <= = String.<=
    val op > = String.>
    val op >= = String.>=
    val op ^ = String.^
    val str = UTF8.encode1
    fun foldl f init s = USubstring.foldl f init (Substring.full s)
    fun size s = foldl (fn (_, n) => n + 1) 0 s
    fun explode s = List.rev (foldl (op ::) [] s)
    fun implode xs = String.concat (List.map UTF8.encode1 xs)
    fun map f s = String.concat (List.rev (foldl (fn (c, acc) => UTF8.encode1 (f c) :: acc) [] s)) (* TODO: Use string.gsub? *)
    fun app f s = USubstring.app f (Substring.full s)
    fun all f s = USubstring.all f (Substring.full s)
    fun exists f s = USubstring.exists f (Substring.full s)
    val concat = String.concat
    val concatWith = String.concatWith
  end
  structure Substring = USubstring
  structure Char = UChar
end
_overload "UnicodeString" [Unicode.ustring] { < = Unicode.String.<
                                            , <= = Unicode.String.<=
                                            , > = Unicode.String.>
                                            , >= = Unicode.String.>=
                                            , maxOrd = 0x10ffff
                                            , maxCodeUnit = 0xff
                                            };
