infix ^^
val op ^^ = String32.^
val (a, b) = Substring32.splitl (fn c => c <> #"X") (Substring32.full "aaaXbbbbXccc")
val () = print (String32.toString (Substring32.string a ^^ "|" ^^ Substring32.string b) ^ "\n")
val (a, b) = Substring32.splitr (fn c => c <> #"X") (Substring32.full "aaaXbbbbXccc")
val () = print (String32.toString (Substring32.string a ^^ "|" ^^ Substring32.string b) ^ "\n")
val a = Substring32.triml 3 (Substring32.full "foobar")
val () = print (String32.toString (Substring32.string a) ^ "\n");
val a = Substring32.trimr 3 (Substring32.full "foobar")
val () = print (String32.toString (Substring32.string a) ^ "\n");
val s = Substring32.substring ("XfooXXbXarXX", 1, 10)
val () = print (String32.toString (String32.concatWith "," (List.map String32.str (Substring32.explode s))) ^ "\n")
val () = print (String32.toString (Substring32.translate (fn c => "[" ^^ String32.str (Char32.toUpper c) ^^ "]") s) ^ "\n")
val () = print (String32.toString (String32.concatWith "," (List.map Substring32.string (Substring32.tokens (fn x => x = #"X") s))) ^ "\n")
val () = print (String32.toString (String32.concatWith "," (List.map Substring32.string (Substring32.fields (fn x => x = #"X") s))) ^ "\n")
