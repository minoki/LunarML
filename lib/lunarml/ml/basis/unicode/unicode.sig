signature UTF = sig
  eqtype codeunit
  eqtype string
  type substring
  eqtype uchar (* Unicode scalar value *)
  eqtype ustring (* a sequence of Unicode scalar values *)
  type usubstring
  val encode : ustring -> string
  val encodeSubstring : usubstring -> string
  val decode : string -> ustring (* invalid code units are replaced with U+FFFD *)
  val validate : string -> ustring option
  val isWellFormed : string -> bool
  val isWellFormedSubstring : substring -> bool
  val encode1 : uchar -> string
  datatype ('char, 'substring) decode_result
    = SCALAR of uchar * 'substring
    | INVALID of 'char * 'substring
    | END
  val decode1 : substring -> (codeunit, substring) decode_result
  val decode1Replace : substring -> (uchar * substring) option (* return U+FFFD on error *)
  val decode1Raise : substring -> (uchar * substring) option (* raises Chr on error *)
  structure Unsafe : sig
    val decode : string -> ustring (* unchecked decode *)
    val decode1 : substring -> (uchar * substring) option (* unchecked decode *)
  end
end
signature UNICODE = sig
  eqtype uchar (* Unicode scalar value *)
  eqtype ustring (* a sequence of Unicode scalar values *)
  type usubstring
  structure Char : sig
    val fromAscii : Char7.char -> uchar
    val toChar32 : uchar -> Char32.char
    val ord : uchar -> int
    val chr : int -> uchar (* raises Chr on error *)
    val < : uchar * uchar -> bool
    val <= : uchar * uchar -> bool
    val > : uchar * uchar -> bool
    val >= : uchar * uchar -> bool
  end
  structure UTF8 : UTF where type string = String.string
                       where type substring = Substring.substring
                       where type ustring = ustring
                       where type uchar = uchar
                       where type usubstring = usubstring
  structure UTF16 : UTF where type string = String16.string
                        where type substring = Substring16.substring
                        where type ustring = ustring
                        where type uchar = uchar
                        where type usubstring = usubstring
  structure UTF32 : UTF where type string = String32.string
                        where type substring = Substring32.substring
                        where type ustring = ustring
                        where type uchar = uchar
                        where type usubstring = usubstring
  structure String : sig
    type ('string, 'substring) encoding =
      { uncheckedDecode : 'string -> ustring
      , uncheckedDecodeSubstring : 'substring -> usubstring
      , encodeSubstring : usubstring -> 'substring
      }
    datatype representation = UTF8 of (String.string, Substring.substring) encoding
                            | UTF16 of (String16.string, Substring16.substring) encoding
                            | UTF32 of (String32.string, Substring32.substring) encoding
    val representation : representation
    val fromAscii : String7.string -> ustring
    val < : ustring * ustring -> bool
    val <= : ustring * ustring -> bool
    val > : ustring * ustring -> bool
    val >= : ustring * ustring -> bool
    val ^ : ustring * ustring -> ustring
    val size : ustring -> int (* the number of Unicode scalar values; O(n) in general *)
    val str : uchar -> ustring
    val explode : ustring -> uchar list
    val implode : uchar list -> ustring
    val foldl : (uchar * 'a -> 'a) -> 'a -> ustring -> 'a
    val map : (uchar -> uchar) -> ustring -> ustring
    val app : (uchar -> unit) -> ustring -> unit
    val all : (uchar -> bool) -> ustring -> bool
    val exists : (uchar -> bool) -> ustring -> bool
    val concat : ustring list -> ustring
    val concatWith : ustring -> ustring list -> ustring
  end
  structure Substring : sig
    val size : usubstring -> int (* the number of Unicode scalar values; O(n) in general *)
    val full : ustring -> usubstring
    val string : usubstring -> ustring
    val isEmpty : usubstring -> bool
    val getc : usubstring -> (uchar * usubstring) option
    val explode : usubstring -> uchar list
    val foldl : (uchar * 'a -> 'a) -> 'a -> usubstring -> 'a
    val map : (uchar -> uchar) -> usubstring -> ustring
    val app : (uchar -> unit) -> usubstring -> unit
    val all : (uchar -> bool) -> usubstring -> bool
    val exists : (uchar -> bool) -> usubstring -> bool
    val concat : usubstring list -> ustring
    val concatWith : ustring -> usubstring list -> ustring
  end
end;
