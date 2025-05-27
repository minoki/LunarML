infix ^^
val op ^^ = String16.^
val (a, b) = Substring16.splitl (fn c => c <> #"X") (Substring16.full "aaaXbbbbXccc")
val () = print (String16.toString (Substring16.string a ^^ "|" ^^ Substring16.string b) ^ "\n")
val (a, b) = Substring16.splitr (fn c => c <> #"X") (Substring16.full "aaaXbbbbXccc")
val () = print (String16.toString (Substring16.string a ^^ "|" ^^ Substring16.string b) ^ "\n")
val a = Substring16.triml 3 (Substring16.full "foobar")
val () = print (String16.toString (Substring16.string a) ^ "\n");
val a = Substring16.trimr 3 (Substring16.full "foobar")
val () = print (String16.toString (Substring16.string a) ^ "\n");
val s = Substring16.substring ("XfooXXbXarXX", 1, 10)
val () = print (String16.toString (String16.concatWith "," (List.map String16.str (Substring16.explode s))) ^ "\n")
val () = print (String16.toString (Substring16.translate (fn c => "[" ^^ String16.str (Char16.toUpper c) ^^ "]") s) ^ "\n")
val () = print (String16.toString (String16.concatWith "," (List.map Substring16.string (Substring16.tokens (fn x => x = #"X") s))) ^ "\n")
val () = print (String16.toString (String16.concatWith "," (List.map Substring16.string (Substring16.fields (fn x => x = #"X") s))) ^ "\n")
