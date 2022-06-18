structure CMTool = struct
fun readFile filename = let val ins = TextIO.openIn filename (* may raise Io *)
                            val content = TextIO.inputAll ins
                            val () = TextIO.closeIn ins
                        in content
                        end;

fun run [] = ()
  | run (filename :: args) = if String.isSuffix ".cm" filename then
                                 let val content = readFile filename
                                 in case CMParser.P.runParser CMParser.cmfile () filename (StringStream.fromString { file = filename, content = content }) of
                                        CMParser.P.Ok ((exports, members), ()) =>
                                        let val state as { definedIn, uses, allFilesRev } = CMEval.processList (CMEval.initialState, members)
                                            val graph = CMEval.graph state
                                            val sorted = CMEval.sort (state, graph)
                                            val exports = CMEval.processExports (state, exports)
                                        in (* CMSyntax.MLSymbolMap.appi (fn (sym, file) => print (CMSyntax.MLSymbol.toString sym ^ ": " ^ file ^ "\n")) definedIn
                                         ; CMEval.StringMap.appi (fn (file, set) => print (file ^ ": " ^ String.concatWith "," (List.map CMSyntax.MLSymbol.toString (CMSyntax.MLSymbolSet.listItems set)) ^ "\n")) uses
                                         ; CMEval.StringMap.appi (fn (file, set) => print (file ^ ": " ^ String.concatWith "," (CMEval.StringSet.listItems set) ^ "\n")) graph
                                         ; print "===\n"
                                         ; List.app (fn file => print (file ^ "\n")) (List.rev allFilesRev)
                                         ; print "===\n"
                                            *)
                                            print "local\n"
                                         ; List.app (fn file => print (file ^ "\n")) (List.rev sorted)
                                         ; print "in\n"
                                         ; List.app (fn item => print (CMSyntax.MLSymbol.toString item ^ "\n")) exports
                                         ; print "end\n"
                                        end
                                                           (*
                                        let fun go (CMSyntax.PPJust (CMSyntax.Member { pathname })) = print (pathname ^ "\n")
                                              | go (CMSyntax.PPConditional (cond, then', else')) = (print "===\n"; List.app go then'; print "===\n"; List.app go else'; print "===\n")
                                              | go (CMSyntax.PPError e) = print ("#error " ^ e ^ "\n")
                                        in List.app go members
                                        end *)
                                      | CMParser.P.ParseError e => print (e ^ "\n")
                                  ; run args
                                 end
                             else
                                 let val content = readFile filename
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
