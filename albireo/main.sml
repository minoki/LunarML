fun makeInputFromString s =
  let val r = ref s
  in fn _ => let val x = !r in r := ""; x end
  end;
val name = "<test input>";
val lexer = AlbireoParser.makeLexer (makeInputFromString "func foo(x: i32) -> i32 { return x * x; }") name;
fun onError (message, p1, p2) = raise Fail message;
val () =
  let val (program, _) = AlbireoParser.parse ((* lookahead *) 0, lexer, onError, name)
  in print "OK.\n"
  end
  handle AlbireoParser.ParseError => ()
       | Fail s => print (s ^ "\n");
