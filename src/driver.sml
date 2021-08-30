(*
 * Copyright (c) 2021 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure Driver = struct

datatype OutputMode = ExecutableMode | LibraryMode

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

fun parse({ nextVId }, fixityEnv, name, lines, str) = let fun printError (s,p1 as {file=f1,line=l1,column=c1},p2 as {file=f2,line=l2,column=c2}) =
                                      ( if p1 = p2 then
                                            print (name ^ ":" ^ Int.toString l1 ^ ":" ^ Int.toString c1 ^ ": " ^ s ^ "\n")
                                        else
                                            print (name ^ ":" ^ Int.toString l1 ^ ":" ^ Int.toString c1 ^ "-" ^ Int.toString l2 ^ ":" ^ Int.toString c2 ^ ": " ^ s ^ "\n")
                                      ; printSpan(name, lines, {start=p1, end_=p2})
                                      )
                                  val lexErrors = ref []
                                  val lexer = LunarMLParser.makeLexer (LunarMLLex.makeInputFromString str) (name, lexErrors)
                              in case !lexErrors of
                                     [] => Fixity.doProgram({ nextVId = nextVId }, fixityEnv, #1 (LunarMLParser.parse((* lookahead *) 0, lexer, printError, name)))
                                   | errors => ( List.app (fn LunarMLLex.TokError (pos, message) => ( print (name ^ ":" ^ Int.toString (#line pos) ^ ":" ^ Int.toString (#column pos) ^ ": syntax error: " ^ message ^ "\n")
                                                                                                    ; printPos (name, lines, pos)
                                                                                                    )
                                                          | LunarMLLex.TokWarning (pos, message) => ( print (name ^ ":" ^ Int.toString (#line pos) ^ ":" ^ Int.toString (#column pos) ^ ": warning: " ^ message ^ "\n")
                                                                                                    ; printPos (name, lines, pos)
                                                                                                    )
                                                          ) errors
                                               ; LunarMLParser.parse((* lookahead *) 0, lexer, printError, name)
                                               ; raise Abort
                                               )
                              end

type Context = { typingContext : Typing.ProgramContext
               , toFContext : ToFSyntax.Context
               }
fun newContext() : Context = let val typingContext = Typing.newContext()
                             in { typingContext = typingContext
                                , toFContext = { nextVId = #nextVId typingContext }
                                }
                             end

type Env = { fixity : Fixity.Env
           , typingEnv : Typing.Env
           , tynameset : USyntax.TyNameSet.set
           , toFEnv : ToFSyntax.Env
           , fTransEnv : FTransform.Env
           }
val initialEnv : Env = { fixity = InitialEnv.initialFixityEnv
                       , typingEnv = InitialEnv.initialEnv
                       , tynameset = InitialEnv.initialTyNameSet
                       , toFEnv = ToFSyntax.initialEnv
                       , fTransEnv = FTransform.initialEnv
                       }

fun compile({ typingContext, toFContext } : Context, outputMode, { fixity, typingEnv, tynameset, toFEnv, fTransEnv } : Env, name, source) =
    let val lines = Vector.fromList (String.fields (fn x => x = #"\n") source)
    in let val (fixity', ast1) = parse({ nextVId = #nextVId typingContext }, fixity, name, lines, source)
           val () = CheckSyntacticRestrictions.checkProgram ast1
           val ast1' = PostParsing.scopeTyVarsInProgram(ast1)
           val (typingEnv', decs) = Typing.typeCheckProgram(typingContext, typingEnv, ast1')
           val tynameset = Typing.checkTyScopeOfProgram(typingContext, tynameset, decs)
           val (toFEnv, fdecs) = case outputMode of
                                     ExecutableMode => ToFSyntax.programToFDecs(toFContext, toFEnv, List.concat decs)
                                   | LibraryMode => ToFSyntax.libraryToFDecs(toFContext, typingEnv', toFEnv, List.concat decs)
           val (fTransEnv', fdecs') = FTransform.doDecs toFContext fTransEnv fdecs
           val modifiedEnv = { fixity = Fixity.mergeEnv (fixity, fixity')
                             , typingEnv = Typing.mergeEnv (typingEnv, typingEnv')
                             , tynameset = tynameset
                             , toFEnv = toFEnv
                             , fTransEnv = fTransEnv'
                             }
       in (modifiedEnv, fdecs')
       end handle LunarMLParser.ParseError => raise Abort
                | Syntax.SyntaxError ([], message) =>
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
                | Typing.TypeError ([], message) =>
                  ( print ("type error: " ^ message ^ "\n")
                  ; raise Abort
                  )
                | Typing.TypeError (spans as ({start=p1 as {file=f1,line=l1,column=c1},end_=p2 as {file=f2,line=l2,column=c2}} :: _), message) =>
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
fun wholeProgramOptimization decs = case DeadCodeElimination.doDecs (USyntax.VIdSet.empty, decs) of
                                        (_, decs) => decs
end (* structure Driver *)
