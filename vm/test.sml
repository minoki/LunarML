fun print_error (s, p1, p2) = print (s ^ "\n");
val content = case CommandLine.arguments () of
                  [filename] => let val instream = TextIO.openIn filename
                                in TextIO.inputAll instream before TextIO.closeIn instream
                                end
                | _ => ( print "> "
                       ; case TextIO.inputLine TextIO.stdIn of
                             SOME ln => ln
                           | NONE => ""
                       );
val lexer = let val i = ref 0
            in IRParser.makeLexer (fn _ => if !i = 0 then
                                               (i := 1; content)
                                           else
                                               "")
            end;
val (program, _) = IRParser.parse (0, lexer, print_error, ());
List.app (fn stmt => print (USyntax.stmtToString stmt ^ "\n")) program;
print "---\n";
val { instructions, constants, functions } = Compile.compileProgram (Compile.StringMap.empty, 1, program);
Disasm.disasm instructions;
print "---\n";
VM.runProgram { instructions = instructions, constants = constants, functions = functions };
