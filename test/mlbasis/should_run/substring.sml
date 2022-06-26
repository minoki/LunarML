val (a, b) = Substring.splitl (fn c => c <> #"X") (Substring.full "aaaXbbbbXccc")
val () = print (Substring.string a ^ "|" ^ Substring.string b ^ "\n")
val (a, b) = Substring.splitr (fn c => c <> #"X") (Substring.full "aaaXbbbbXccc")
val () = print (Substring.string a ^ "|" ^ Substring.string b ^ "\n")
val a = Substring.triml 3 (Substring.full "foobar")
val () = print (Substring.string a ^ "\n");
val a = Substring.trimr 3 (Substring.full "foobar")
val () = print (Substring.string a ^ "\n");
