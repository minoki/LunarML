type uchar = Unicode.uchar
type ustring = Unicode.ustring
fun collectDecoder decode1 s =
  let fun go (acc, s) =
            case decode1 s of
              NONE => SOME (List.rev acc)
            | SOME (c, s') => go (c :: acc, s')
  in go ([], s) handle Chr => NONE
  end
fun dumpOption dumpElem NONE = "NONE"
  | dumpOption dumpElem (SOME x) = "SOME " ^ dumpElem x
fun dumpList dumpElem xs = "[" ^ String.concatWith "," (List.map dumpElem xs) ^ "]"
val someStr : ustring = "Hello\u3042\u{10ABCD}\000\u00A5\u03B1\uD7FF\uE000\u{1D800}\u{10000}\u{10FFFF}";
val someSubstring = Unicode.Substring.full someStr;
val () = print (dumpList (Int.fmt StringCvt.HEX o Unicode.Char.ord) (Unicode.String.explode someStr) ^ "\n");
val () = print (Bool.toString (Unicode.String.implode (Unicode.String.explode someStr) = someStr) ^ "\n");
val () = print (Int.toString (Unicode.String.size someStr) ^ "\n");
val () = print (dumpList (Int.fmt StringCvt.HEX o Unicode.Char.ord) (Unicode.Substring.explode someSubstring) ^ "\n");
val () = print (Int.toString (Unicode.Substring.size someSubstring) ^ "\n");
val strings : ustring list = ["a", "\u3042", "\u3042a", "\uFFFF", "\u{10000}", "\u{10000}a", "\u{10FFFF}", "\u{10FFFF}a"];
val () = List.app (fn s1 =>
    List.app (fn s2 =>
      case (s1 = s2, s1 < s2, s1 <= s2, s1 > s2, s1 >= s2) of
        (true, false, true, false, true) => print "OK\n"
      | (false, true, true, false, false) => print "OK\n"
      | (false, false, false, true, true) => print "OK\n"
      | _ => print "Wrong\n"
    ) strings
  ) strings;

(* UTF-8 *)
val () = print (String.toString (Unicode.UTF8.encode someStr) ^ "\n");
val () = print (String.toString (Unicode.UTF8.encodeSubstring someSubstring) ^ "\n");
val () = print (String.toString (Unicode.UTF8.encode1 #"a") ^ "\n");
val () = print (String.toString (Unicode.UTF8.encode1 #"\u00A5") ^ "\n");
val () = print (String.toString (Unicode.UTF8.encode1 #"\u03B1") ^ "\n");
val () = print (String.toString (Unicode.UTF8.encode1 #"\u3042") ^ "\n");
val () = print (String.toString (Unicode.UTF8.encode1 #"\uFFFF") ^ "\n");
val () = print (String.toString (Unicode.UTF8.encode1 #"\u{1D49C}") ^ "\n");
val () = print (String.toString (Unicode.UTF8.encode1 #"\u{10FFFF}") ^ "\n");
val someUtf8Str = Unicode.UTF8.encode someStr;
val () = print (Bool.toString (Unicode.UTF8.decode someUtf8Str = someStr) ^ "\n");
val () = print (Bool.toString (Unicode.UTF8.Unsafe.decode someUtf8Str = someStr) ^ "\n");
val () = print (Bool.toString (Unicode.UTF8.validate someUtf8Str = SOME someStr) ^ "\n");
val () = print (Bool.toString (Unicode.UTF8.isWellFormed someUtf8Str) ^ "\n");
val () = print (Bool.toString (Unicode.UTF8.isWellFormedSubstring (Substring.full someUtf8Str)) ^ "\n");
val () = print (dumpOption (dumpList (Int.fmt StringCvt.HEX o Unicode.Char.ord)) (collectDecoder Unicode.UTF8.decode1Replace (Substring.full someUtf8Str)) ^ "\n");
val () = print (dumpOption (dumpList (Int.fmt StringCvt.HEX o Unicode.Char.ord)) (collectDecoder Unicode.UTF8.decode1Raise (Substring.full someUtf8Str)) ^ "\n");
val () = print (dumpOption (dumpList (Int.fmt StringCvt.HEX o Unicode.Char.ord)) (collectDecoder Unicode.UTF8.Unsafe.decode1 (Substring.full someUtf8Str)) ^ "\n");
val invalidUtf8 : string list =
  [ "\u0080"
  , "\u00C0\u0080a"
  , "\u00FE\u0080a"
  , "\u00FF\u0080a"
  , "\128"
  , "\191"
  , "\192"
  , "\193"
  , "\245"
  , "\246"
  , "\247"
  , "\248"
  , "\249"
  , "\250"
  , "\251"
  , "\252"
  , "\253"
  , "\254"
  , "\255"
  , "\194\194"
  , "\227\129a"
  , "\194\128\128"
  , "\192\128"
  , "\193\128"
  , "\224\128\128"
  , "\237\160\128"
  , "\237\191\191"
  , "\244\144\128\128"
  , "\247\191\191\191"
  , "\248\136\128\128\128"
  , "\248\144\128\128\128"
  , "\248\160\128\128\128"
  , "\249\128\128\128\128"
  , "\250\128\128\128\128"
  , "\251\128\128\128\128"
  , "\252\132\128\128\128\128"
  , "\252\136\128\128\128\128"
  , "\252\144\128\128\128\128"
  , "\252\160\128\128\128\128"
  , "\253\160\128\128\128\128"
  , "\253\191\191\191\191\191"
  ]
val () = List.app (fn s =>
  ( print ("isWellFormed " ^ String.toString s ^ ": " ^ Bool.toString (Unicode.UTF8.isWellFormed s) ^ "\n")
  ; print ("isWellFormedSubstring " ^ String.toString s ^ ": " ^ Bool.toString (Unicode.UTF8.isWellFormedSubstring (Substring.full s)) ^ "\n")
  ; print ("decode1Replace " ^ String.toString s ^ ": " ^ dumpOption (dumpList (Int.fmt StringCvt.HEX o Unicode.Char.ord)) (collectDecoder Unicode.UTF8.decode1Replace (Substring.full s)) ^ "\n")
  ; print ("decode1Raise " ^ String.toString s ^ ": " ^ dumpOption (dumpList (Int.fmt StringCvt.HEX o Unicode.Char.ord)) (collectDecoder Unicode.UTF8.decode1Raise (Substring.full s)) ^ "\n")
  )) invalidUtf8;

(* UTF-16 *)
val () = print (String16.toString (Unicode.UTF16.encode someStr) ^ "\n");
val () = print (String16.toString (Unicode.UTF16.encodeSubstring someSubstring) ^ "\n");
val () = print (String16.toString (Unicode.UTF16.encode1 #"a") ^ "\n");
val () = print (String16.toString (Unicode.UTF16.encode1 #"\u00A5") ^ "\n");
val () = print (String16.toString (Unicode.UTF16.encode1 #"\u03B1") ^ "\n");
val () = print (String16.toString (Unicode.UTF16.encode1 #"\u3042") ^ "\n");
val () = print (String16.toString (Unicode.UTF16.encode1 #"\uFFFF") ^ "\n");
val () = print (String16.toString (Unicode.UTF16.encode1 #"\u{1D49C}") ^ "\n");
val () = print (String16.toString (Unicode.UTF16.encode1 #"\u{10FFFF}") ^ "\n");
val someUtf16Str = Unicode.UTF16.encode someStr;
val () = print (Bool.toString (Unicode.UTF16.decode someUtf16Str = someStr) ^ "\n");
val () = print (Bool.toString (Unicode.UTF16.Unsafe.decode someUtf16Str = someStr) ^ "\n");
val () = print (Bool.toString (Unicode.UTF16.validate someUtf16Str = SOME someStr) ^ "\n");
val () = print (Bool.toString (Unicode.UTF16.isWellFormed someUtf16Str) ^ "\n");
val () = print (Bool.toString (Unicode.UTF16.isWellFormedSubstring (Substring16.full someUtf16Str)) ^ "\n");
val () = print (dumpOption (dumpList (Int.fmt StringCvt.HEX o Unicode.Char.ord)) (collectDecoder Unicode.UTF16.decode1Replace (Substring16.full someUtf16Str)) ^ "\n");
val () = print (dumpOption (dumpList (Int.fmt StringCvt.HEX o Unicode.Char.ord)) (collectDecoder Unicode.UTF16.decode1Raise (Substring16.full someUtf16Str)) ^ "\n");
val () = print (dumpOption (dumpList (Int.fmt StringCvt.HEX o Unicode.Char.ord)) (collectDecoder Unicode.UTF16.Unsafe.decode1 (Substring16.full someUtf16Str)) ^ "\n");
val invalidUtf16 : String16.string list =
  [ "\uD800"
  , "\uD800\uD800"
  , "\uDC00"
  , "\uFFFF\uDFFF"
  , "\uD800\uD800\uDC00"
  ]
val () = List.app (fn s =>
  ( print ("isWellFormed " ^ String16.toString s ^ ": " ^ Bool.toString (Unicode.UTF16.isWellFormed s) ^ "\n")
  ; print ("isWellFormedSubstring " ^ String16.toString s ^ ": " ^ Bool.toString (Unicode.UTF16.isWellFormedSubstring (Substring16.full s)) ^ "\n")
  ; print ("decode1Replace " ^ String16.toString s ^ ": " ^ dumpOption (dumpList (Int.fmt StringCvt.HEX o Unicode.Char.ord)) (collectDecoder Unicode.UTF16.decode1Replace (Substring16.full s)) ^ "\n")
  ; print ("decode1Raise " ^ String16.toString s ^ ": " ^ dumpOption (dumpList (Int.fmt StringCvt.HEX o Unicode.Char.ord)) (collectDecoder Unicode.UTF16.decode1Raise (Substring16.full s)) ^ "\n")
  )) invalidUtf16;

(* UTF-32 *)
val () = print (String32.toString (Unicode.UTF32.encode someStr) ^ "\n");
val () = print (String32.toString (Unicode.UTF32.encodeSubstring someSubstring) ^ "\n");
val () = print (String32.toString (Unicode.UTF32.encode1 #"a") ^ "\n");
val () = print (String32.toString (Unicode.UTF32.encode1 #"\u00A5") ^ "\n");
val () = print (String32.toString (Unicode.UTF32.encode1 #"\u03B1") ^ "\n");
val () = print (String32.toString (Unicode.UTF32.encode1 #"\u3042") ^ "\n");
val () = print (String32.toString (Unicode.UTF32.encode1 #"\uFFFF") ^ "\n");
val () = print (String32.toString (Unicode.UTF32.encode1 #"\u{1D49C}") ^ "\n");
val () = print (String32.toString (Unicode.UTF32.encode1 #"\u{10FFFF}") ^ "\n");
val someUtf32Str = Unicode.UTF32.encode someStr;
val () = print (Bool.toString (Unicode.UTF32.decode someUtf32Str = someStr) ^ "\n");
val () = print (Bool.toString (Unicode.UTF32.Unsafe.decode someUtf32Str = someStr) ^ "\n");
val () = print (Bool.toString (Unicode.UTF32.validate someUtf32Str = SOME someStr) ^ "\n");
val () = print (Bool.toString (Unicode.UTF32.isWellFormed someUtf32Str) ^ "\n");
val () = print (Bool.toString (Unicode.UTF32.isWellFormedSubstring (Substring32.full someUtf32Str)) ^ "\n");
val () = print (dumpOption (dumpList (Int.fmt StringCvt.HEX o Unicode.Char.ord)) (collectDecoder Unicode.UTF32.decode1Replace (Substring32.full someUtf32Str)) ^ "\n");
val () = print (dumpOption (dumpList (Int.fmt StringCvt.HEX o Unicode.Char.ord)) (collectDecoder Unicode.UTF32.decode1Raise (Substring32.full someUtf32Str)) ^ "\n");
val () = print (dumpOption (dumpList (Int.fmt StringCvt.HEX o Unicode.Char.ord)) (collectDecoder Unicode.UTF32.Unsafe.decode1 (Substring32.full someUtf32Str)) ^ "\n");
val invalidUtf32 : String32.string list =
  [ "\uD800"
  , "\uD800\uDC00"
  , "\uD800\uD800"
  , "\uDC00"
  , "\uFFFF\uDFFF"
  , "\uD800\uD800\uDC00"
  ]
val () = List.app (fn s =>
  ( print ("isWellFormed " ^ String32.toString s ^ ": " ^ Bool.toString (Unicode.UTF32.isWellFormed s) ^ "\n")
  ; print ("isWellFormedSubstring " ^ String32.toString s ^ ": " ^ Bool.toString (Unicode.UTF32.isWellFormedSubstring (Substring32.full s)) ^ "\n")
  ; print ("decode1Replace " ^ String32.toString s ^ ": " ^ dumpOption (dumpList (Int.fmt StringCvt.HEX o Unicode.Char.ord)) (collectDecoder Unicode.UTF32.decode1Replace (Substring32.full s)) ^ "\n")
  ; print ("decode1Raise " ^ String32.toString s ^ ": " ^ dumpOption (dumpList (Int.fmt StringCvt.HEX o Unicode.Char.ord)) (collectDecoder Unicode.UTF32.decode1Raise (Substring32.full s)) ^ "\n")
  )) invalidUtf32;
