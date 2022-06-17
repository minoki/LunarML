structure CMTool = struct
fun readFile filename = let val ins = TextIO.openIn filename (* may raise Io *)
                            val content = TextIO.inputAll ins
                            val () = TextIO.closeIn ins
                        in content
                        end;

fun run [] = ()
  | run (filename :: args) = let val content = readFile filename
                                 val lines = Vector.fromList (String.fields (fn x => x = #"\n") content)
                             in case CMAnalyzer.parse ({ languageOptions = LanguageOptions.default }, filename, lines, content) of
                                    NONE => ()
                                  | SOME program => let val { definedStructures, definedSignatures, definedFunctors, usedStructures, usedSignatures, usedFunctors } = CMAnalyzer.goProgram program
                                                    in print (filename ^ "\n")
                                                     ; print "  defined structures:\n"
                                                     ; Syntax.StrIdSet.app (fn Syntax.MkStrId name => print ("    " ^ name ^ "\n")) definedStructures
                                                     ; print "  defined signatures:\n"
                                                     ; Syntax.SigIdSet.app (fn Syntax.MkSigId name => print ("    " ^ name ^ "\n")) definedSignatures
                                                     ; print "  defined functors:\n"
                                                     ; Syntax.FunIdSet.app (fn Syntax.MkFunId name => print ("    " ^ name ^ "\n")) definedFunctors
                                                     ; print "  used structures:\n"
                                                     ; Syntax.StrIdSet.app (fn Syntax.MkStrId name => print ("    " ^ name ^ "\n")) usedStructures
                                                     ; print "  used signatures:\n"
                                                     ; Syntax.SigIdSet.app (fn Syntax.MkSigId name => print ("    " ^ name ^ "\n")) usedSignatures
                                                     ; print "  used functors:\n"
                                                     ; Syntax.FunIdSet.app (fn Syntax.MkFunId name => print ("    " ^ name ^ "\n")) usedFunctors
                                                    end
                              ; run args
                             end;
val () = run (CommandLine.arguments ());
end;
