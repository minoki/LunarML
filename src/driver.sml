structure Driver = struct
fun rep(c, n) = CharVector.tabulate(n, fn _ => c)
fun printPos(name, lines, p) =
    if #file p = name then
        let val l = #line p - 1
            val c = #column p - 1
        in if 0 <= l andalso l < Vector.length lines then
               let val text = Vector.sub (lines, l)
                   val start = Int.max (0, c - 30)
                   val e = Int.min(String.size text, c + 30)
               in print (String.substring (text, start, e - start) ^ "\n")
                ; print (rep(#" ", c - start) ^ "^" ^ "\n")
               end
           else
               () (* not available *)
        end
    else
        () (* what to do? *)
fun printSpan(name, lines, {start=p1, end_=p2}) =
    if #file p1 = name andalso #file p2 = name then
        if #line p1 = #line p2 then
            let val l = #line p1 - 1
                val c1 = #column p1 - 1
                val c2 = #column p2 - 1
            in if 0 <= l andalso l < Vector.length lines then
                   let val text = Vector.sub (lines, l)
                       val start = Int.max (0, c1 - 30)
                       val e = Int.min (String.size text, c2 + 30)
                   in print (String.substring (text, start, e - start) ^ "\n")
                    ; print (rep(#" ", c1 - start) ^ "^" ^ rep(#"~", c2 - c1) ^ "\n")
                   end
               else
                   () (* not available *)
            end
        else
            ( let val l1 = #line p1 - 1
                  val c1 = #column p1 - 1
              in if 0 <= l1 andalso l1 < Vector.length lines then
                     let val text = Vector.sub (lines, l1)
                         val start = Int.max (0, c1 - 30)
                         val e = Int.min(String.size text, c1 + 30)
                     in print (String.substring (text, start, e - start) ^ "\n")
                      ; print (rep(#" ", c1 - start) ^ "^" ^ rep(#"~", e - c1 - 1) ^ "\n")
                     end
                 else
                     () (* not available *)
              end
            ; let val l2 = #line p2 - 1
                  val c2 = #column p2 - 1
              in if 0 <= l2 andalso l2 < Vector.length lines then
                     let val text = Vector.sub (lines, l2)
                         val start = Int.max (0, c2 - 30)
                         val e = Int.min(String.size text, c2 + 30)
                     in print (String.substring (text, start, e - start) ^ "\n")
                      ; print (rep(#"~", c2 - start + 1) ^ "\n")
                     end
                 else
                     () (* not available *)
              end
            )
    else
        () (* what to do? *)

exception Abort

fun parse(name, lines, str) = let fun printError (s,p1 as {file=f1,line=l1,column=c1},p2 as {file=f2,line=l2,column=c2}) =
                                      ( if p1 = p2 then
                                            print (name ^ ":" ^ Int.toString l1 ^ ":" ^ Int.toString c1 ^ ": " ^ s ^ "\n")
                                        else
                                            print (name ^ ":" ^ Int.toString l1 ^ ":" ^ Int.toString c1 ^ "-" ^ Int.toString l2 ^ ":" ^ Int.toString c2 ^ ": " ^ s ^ "\n")
                                      ; printSpan(name, lines, {start=p1, end_=p2})
                                      )
                                  val lexErrors = ref []
                                  val lexer = DamepoMLParser.makeLexer (DamepoMLLex.makeInputFromString str) (name, lexErrors)
                              in case !lexErrors of
                                     [] => #2 (Fixity.doDecs({}, InitialEnv.initialFixity, #1 (DamepoMLParser.parse((* lookahead *) 0, lexer, printError, name))))
                                   | errors => ( List.app (fn DamepoMLLex.TokError (pos, message) => ( print (name ^ ":" ^ Int.toString (#line pos) ^ ":" ^ Int.toString (#column pos) ^ ": syntax error: " ^ message ^ "\n")
                                                                                                     ; printPos (name, lines, pos)
                                                                                                     )
                                                          | DamepoMLLex.TokWarning (pos, message) => ( print (name ^ ":" ^ Int.toString (#line pos) ^ ":" ^ Int.toString (#column pos) ^ ": warning: " ^ message ^ "\n")
                                                                                                     ; printPos (name, lines, pos)
                                                                                                     )
                                                          ) errors
                                               ; DamepoMLParser.parse((* lookahead *) 0, lexer, printError, name)
                                               ; raise Abort
                                               )
                              end

fun compile(name, source) =
    let val lines = Vector.fromList (String.fields (fn x => x = #"\n") source)
    in let val ast1 = parse(name, lines, source)
           val ctx = Typing.newContext()
           val ast1' = PostParsing.scopeTyVarsInDecs(Syntax.TyVarSet.empty, ast1)
           val (_, ast2) = ToTypedSyntax.toUDecs(ctx, Syntax.TyVarMap.empty, InitialEnv.initialEnv_ToTypedSyntax, ast1')
           val (env, tvc, (topdecs, decs)) = Typing.typeCheckProgram(ctx, InitialEnv.initialEnv, ast2)
           val decs' = Typing.applyDefaultTypes(tvc, decs)
           val fctx = { nextVId = #nextVId ctx }
           val fdecs = ToFSyntax.toFDecs(fctx, ToFSyntax.emptyEnv, decs')
           val fdecs' = List.map (#doDec (FTransform.desugarPatternMatches fctx) FTransform.initialEnv) fdecs
           val lua = CodeGenLua.doDecs { nextLuaId = ref 0 } CodeGenLua.MkEnv fdecs'
       in (topdecs, ast1, ast2, decs', fdecs, fdecs', lua)
       end handle Syntax.SyntaxError ([], message) =>
                  ( print ("error: " ^ message ^ "\n")
                  ; raise Abort
                  )
                | Syntax.SyntaxError (spans as ({start=p1 as {file=f1,line=l1,column=c1},end_=p2 as {file=f2,line=l2,column=c2}} :: _), message) =>
                  ( if f1 = f2 then
                        if p1 = p2 then
                            print (f1 ^ ":" ^ Int.toString l1 ^ ":" ^ Int.toString c1 ^ ": " ^ message ^ "\n")
                        else
                            print (f1 ^ ":" ^ Int.toString l1 ^ ":" ^ Int.toString c1 ^ "-" ^ Int.toString l2 ^ ":" ^ Int.toString c2 ^ ": " ^ message ^ "\n")
                    else
                        print (f1 ^ ":" ^ Int.toString l1 ^ ":" ^ Int.toString c1 ^ "-" ^ f2 ^ ":" ^ Int.toString l2 ^ ":" ^ Int.toString c2 ^ ": " ^ message ^ "\n")
                  ; List.app (fn s => printSpan(name, lines, s)) spans
                  ; raise Abort
                  )
    end
end (* structure Driver *)
