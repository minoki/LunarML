fun compile filename =
  let
    val strm = TextIO.openIn filename
    fun getInput n = TextIO.inputN (strm, n)
    val lexer = AlbireoParser.makeLexer getInput filename;
    fun onError (message, p1, p2) = raise Fail message;
  in
    let val (program, _) = AlbireoParser.parse ((* lookahead *) 0, lexer, onError, filename)
    in print (AlbireoEmit.emit program ^ "\n")
    end
    handle AlbireoParser.ParseError => ()
         | Fail s => print (s ^ "\n");
    TextIO.closeIn strm
  end;
fun main [filename] = compile filename
  | main _ = print "albireo <filename>";
val () = main (CommandLine.arguments ());
