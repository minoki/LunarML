val (a, b) = Substring.splitl (fn c => c <> #"X") (Substring.full "aaaXbbbbXccc")
val () = print (Substring.string a ^ "|" ^ Substring.string b ^ "\n")
val (a, b) = Substring.splitr (fn c => c <> #"X") (Substring.full "aaaXbbbbXccc")
val () = print (Substring.string a ^ "|" ^ Substring.string b ^ "\n")
val a = Substring.triml 3 (Substring.full "foobar")
val () = print (Substring.string a ^ "\n");
val a = Substring.trimr 3 (Substring.full "foobar")
val () = print (Substring.string a ^ "\n");
val s = Substring.substring ("XfooXXbXarXX", 1, 10)
val () = print (String.concatWith "," (List.map String.str (Substring.explode s)) ^ "\n")
val () = print (Substring.translate (fn c => "[" ^ String.str (Char.toUpper c) ^ "]") s ^ "\n")
val () = print (String.concatWith "," (List.map Substring.string (Substring.tokens (fn x => x = #"X") s)) ^ "\n")
val () = print (String.concatWith "," (List.map Substring.string (Substring.fields (fn x => x = #"X") s)) ^ "\n")
