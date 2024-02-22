(*
 * Copyright (c) 2023 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure MLBEval :> sig
              type Context = { driverContext : Driver.Context
                             , baseDir : string
                             , pathMap : string StringMap.map
                             , targetInfo : TargetInfo.target_info
                             , defaultLanguageOptions : LanguageOptions.options
                             , messageHandler : Message.handler
                             }
              datatype Env' = MkEnv of { bas : Env' MLBSyntax.BasMap.map
                                       , fixity : Fixity.Env (* fixity & id status *)
                                       , typing : Typing.Env
                                       }
              type Env = { bas : Env' MLBSyntax.BasMap.map
                         , fixity : Fixity.Env (* fixity & id status *)
                         , typing : Typing.Env
                         }
              val emptyEnv : Env
              type Code = { tynameset : TypedSyntax.TyNameSet.set
                          , toFEnv : ToFSyntax.Env
                          , fdecs : FSyntax.Dec list
                          , cache : Env StringMap.map
                          }
              val initialCode : Code
              val applyAnnotation : Message.handler -> string * LanguageOptions.options -> LanguageOptions.options
              val doDecs : Context -> LanguageOptions.options -> Env -> MLBSyntax.BasDec list -> Code -> Env * Code
              val doMlbSource : Context -> Env -> string -> Code -> Env * Code
              datatype path_setting = PATH_MAP of string
                                    | PATH_VAR of string
              val loadPathVar : Message.handler -> path_setting * string StringMap.map -> string StringMap.map
          end = struct
local structure M = MLBSyntax in
type Context = { driverContext : Driver.Context
               , baseDir : string
               , pathMap : string StringMap.map
               , targetInfo : TargetInfo.target_info
               , defaultLanguageOptions : LanguageOptions.options
               , messageHandler : Message.handler
               }
datatype Env' = MkEnv of Env
withtype Env = { bas : Env' M.BasMap.map
               , fixity : Fixity.Env (* fixity & id status *)
               , typing : Typing.Env
               }
val emptyEnv : Env = { bas = M.BasMap.empty
                     , fixity = Fixity.emptyEnv
                     , typing = Typing.emptyEnv
                     }
fun envWithBasis bas : Env = { bas = bas
                             , fixity = Fixity.emptyEnv
                             , typing = Typing.emptyEnv
                             }
fun mergeEnv (e1 : Env, e2 : Env) = { bas = M.BasMap.unionWith #2 (#bas e1, #bas e2)
                                    , fixity = Fixity.mergeEnv (#fixity e1, #fixity e2)
                                    , typing = Typing.mergeEnv (#typing e1, #typing e2)
                                    }
type Code = { tynameset : TypedSyntax.TyNameSet.set
            , toFEnv : ToFSyntax.Env
            , fdecs : FSyntax.Dec list
            , cache : Env StringMap.map
            }
val initialCode = { tynameset = InitialEnv.initialTyNameSet
                  , toFEnv = ToFSyntax.initialEnv
                  , fdecs = []
                  , cache = StringMap.empty
                  }
(* MLton annotations:
 * allowFFI {false|true}
 * allowSuccessorML {false|true}
 *   allowDoDecs {false|true}
 *   allowExtendedConsts {false|true}
 *     allowExtendedNumConsts {false|true}
 *     allowExtendedTextConsts {false|true}
 *   allowLineComments {false|true}
 *   allowOptBar {false|true}
 *   allowOptSemicolon {false|true}
 *   allowOrPats {false|true}
 *   allowRecordPunExps {false|true}
 *   allowSigWithtype {false|true}
 *   allowVectorExpsAndPats {false|true}
 *     allowVectorExps {false|true}
 *     allowVectorPats {false|true}
 * forceUsed
 * nonexhaustiveBind {warn|error|ignore}
 * nonexhaustiveExnBind {default|ignore}
 * nonexhaustiveExnMatch {default|ignore}
 * nonexhaustiveExnRaise {ignore|default}
 * nonexhaustiveMatch {warn|error|ignore}
 * nonexhaustiveRaise {ignore|warn|error}
 * redundantBind {warn|error|ignore}
 * redundantMatch {warn|error|ignore}
 * redundantRaise {warn|error|ignore}
 * resolveScope {strdec|dec|topdec|program}
 * sequenceNonUnit {ignore|error|warn}
 * valrecConstr {warn|error|ignore}
 * warnUnused {false|true}
 *)
fun parseIgnoreWarnError "ignore" = SOME LanguageOptions.IGNORE
  | parseIgnoreWarnError "warn" = SOME LanguageOptions.WARN
  | parseIgnoreWarnError "error" = SOME LanguageOptions.ERROR
  | parseIgnoreWarnError _ = NONE
fun applyAnnotation messageHandler (ann, langopt)
    = let fun unrecognized () = ( Message.warning (messageHandler, [], "MLB", "unrecognized annotation: " ^ ann)
                                ; langopt
                                )
      in case String.tokens Char.isSpace ann of
             ["nonexhaustiveBind", x] => (case parseIgnoreWarnError x of
                                              SOME x => LanguageOptions.set.nonexhaustiveBind x langopt
                                            | NONE => unrecognized ()
                                         )
           | ["nonexhaustiveMatch", x] => (case parseIgnoreWarnError x of
                                               SOME x => LanguageOptions.set.nonexhaustiveMatch x langopt
                                             | NONE => unrecognized ()
                                          )
           | ["nonexhaustiveRaise", x] => (case parseIgnoreWarnError x of
                                               SOME x => LanguageOptions.set.nonexhaustiveRaise x langopt
                                             | NONE => unrecognized ()
                                          )
           | ["redundantBind", x] => (case parseIgnoreWarnError x of
                                          SOME x => LanguageOptions.set.redundantBind x langopt
                                        | NONE => unrecognized ()
                                     )
           | ["redundantMatch", x] => (case parseIgnoreWarnError x of
                                           SOME x => LanguageOptions.set.redundantMatch x langopt
                                         | NONE => unrecognized ()
                                      )
           | ["redundantRaise", x] => (case parseIgnoreWarnError x of
                                           SOME x => LanguageOptions.set.redundantRaise x langopt
                                         | NONE => unrecognized ()
                                      )
           | ["sequenceNonUnit", x] => (case parseIgnoreWarnError x of
                                            SOME x => LanguageOptions.set.sequenceNonUnit x langopt
                                          | NONE => unrecognized ()
                                       )
           | ["valDescInComments", x] => (case parseIgnoreWarnError x of
                                              SOME x => LanguageOptions.set.valDescInComments x langopt
                                            | NONE => unrecognized ()
                                         )
           | [name, value] => (case LanguageOptions.setByName name of
                                   SOME setter => (case value of
                                                       "true" => setter true langopt
                                                     | "false" => setter false langopt
                                                     | _ => unrecognized ()
                                                  )
                                 | NONE => unrecognized ()
                              )
           | _ => unrecognized ()
      end
fun doDec (ctx : Context) langopt env (M.BasisDec binds) acc = let val (bas, acc) = List.foldl (fn ((basid, basexp), (bas, acc)) =>
                                                                                                   let val (env', acc) = doExp ctx langopt env basexp acc
                                                                                                   in (M.BasMap.insert (bas, basid, MkEnv env'), acc)
                                                                                                   end
                                                                                               ) (M.BasMap.empty, acc) binds
                                                               in (envWithBasis bas, acc)
                                                               end
  | doDec _ _ env (M.OpenDec basids) acc = let val env' = List.foldl (fn (basid, newenv) =>
                                                                         case M.BasMap.find (#bas env, basid) of
                                                                             SOME (MkEnv env) => mergeEnv (newenv, env)
                                                                           | NONE => raise Fail ("undefined basis: " ^ basid)
                                                                     ) emptyEnv basids
                                           in (env', acc)
                                           end
  | doDec ctx langopt env (M.LocalDec (decs1, decs2)) acc = let val (env', acc) = doDecs ctx langopt env decs1 acc
                                                                val (env'', acc) = doDecs ctx langopt (mergeEnv (env, env')) decs2 acc
                                                                val typingEnv = { valMap = #valMap (#typing env'')
                                                                                , tyConMap = #tyConMap (#typing env'')
                                                                                , tyNameMap = TypedSyntax.TyNameMap.unionWith #2 (#tyNameMap (#typing env'), #tyNameMap (#typing env''))
                                                                                , strMap = #strMap (#typing env'')
                                                                                , sigMap = #sigMap (#typing env'')
                                                                                , funMap = #funMap (#typing env'')
                                                                                , boundTyVars = #boundTyVars (#typing env'')
                                                                                }
                                                            in ({ bas = #bas env'', fixity = #fixity env'', typing = typingEnv }, acc)
                                                            end
  | doDec _ _ env (M.StructureDec binds) acc = let val strMap_fixity = #strMap (#idStatusMap (#fixity env))
                                                   val strMap_fixity' = List.foldl (fn ((id1, id2), m) =>
                                                                                       case Syntax.StrIdMap.find (strMap_fixity, id2) of
                                                                                           SOME value => Syntax.StrIdMap.insert (m, id1, value)
                                                                                         | NONE => raise Fail ("undefined structure: " ^ Syntax.print_StrId id2)
                                                                                   ) Syntax.StrIdMap.empty binds
                                                   val strMap_typing = #strMap (#typing env)
                                                   val strMap_typing' = List.foldl (fn ((id1, id2), m) =>
                                                                                       case Syntax.StrIdMap.find (strMap_typing, id2) of
                                                                                           SOME value => Syntax.StrIdMap.insert (m, id1, value)
                                                                                         | NONE => raise Fail ("undefined structure: " ^ Syntax.print_StrId id2)
                                                                                   ) Syntax.StrIdMap.empty binds
                                               in ({ bas = M.BasMap.empty, fixity = Fixity.envWithStrMap strMap_fixity', typing = Typing.envWithStrMap strMap_typing' }, acc)
                                               end
  | doDec _ _ env (M.SignatureDec binds) acc = let val sigMap_fixity = #sigMap (#fixity env)
                                                   val sigMap_fixity' = List.foldl (fn ((id1, id2), m) =>
                                                                                       case Syntax.SigIdMap.find (sigMap_fixity, id2) of
                                                                                           SOME value => Syntax.SigIdMap.insert (m, id1, value)
                                                                                         | NONE => raise Fail ("undefined signature: " ^ Syntax.print_SigId id2)
                                                                                   ) Syntax.SigIdMap.empty binds
                                                   val sigMap_typing = #sigMap (#typing env)
                                                   val sigMap_typing' = List.foldl (fn ((id1, id2), m) =>
                                                                                       case Syntax.SigIdMap.find (sigMap_typing, id2) of
                                                                                           SOME value => Syntax.SigIdMap.insert (m, id1, value)
                                                                                         | NONE => raise Fail ("undefined signature: " ^ Syntax.print_SigId id2)
                                                                                   ) Syntax.SigIdMap.empty binds
                                               in ({ bas = M.BasMap.empty, fixity = Fixity.envWithSigMap sigMap_fixity', typing = Typing.envWithSigMap sigMap_typing' }, acc)
                                               end
  | doDec _ _ env (M.FunctorDec binds) acc = let val funMap_fixity = #funMap (#fixity env)
                                                 val funMap_fixity' = List.foldl (fn ((id1, id2), m) =>
                                                                                     case Syntax.FunIdMap.find (funMap_fixity, id2) of
                                                                                         SOME value => Syntax.FunIdMap.insert (m, id1, value)
                                                                                       | NONE => raise Fail ("undefined functor: " ^ Syntax.print_FunId id2)
                                                                                 ) Syntax.FunIdMap.empty binds
                                                 val funMap_typing = #funMap (#typing env)
                                                 val funMap_typing' = List.foldl (fn ((id1, id2), m) =>
                                                                                     case Syntax.FunIdMap.find (funMap_typing, id2) of
                                                                                         SOME value => Syntax.FunIdMap.insert (m, id1, value)
                                                                                       | NONE => raise Fail ("undefined functor: " ^ Syntax.print_FunId id2)
                                                                                 ) Syntax.FunIdMap.empty binds
                                             in ({ bas = M.BasMap.empty, fixity = Fixity.envWithFunMap funMap_fixity', typing = Typing.envWithFunMap funMap_typing' }, acc)
                                             end
  | doDec ctx langopt env (M.PathDec path) acc = (case OS.Path.ext path of
                                                      SOME "sml" => doSmlSource ctx langopt env path acc
                                                    | SOME "fun" => doSmlSource ctx langopt env path acc
                                                    | SOME "sig" => doSmlSource ctx langopt env path acc
                                                    | SOME "mlb" => doMlbSource ctx env path acc
                                                    | _ => raise Fail ("unrecognized file extension: " ^ path)
                                                 )
  | doDec ctx langopt env (M.AnnotationDec (anns, decs)) acc = let val langopt = List.foldl (applyAnnotation (#messageHandler ctx)) langopt anns
                                                               in doDecs ctx langopt env decs acc
                                                               end
  | doDec _ _ _ M.PrimDec acc = ({ bas = M.BasMap.empty, fixity = InitialEnv.initialFixityEnv, typing = InitialEnv.initialEnv }, acc)
  | doDec _ _ _ M.PrimOverloadDec acc = ({ bas = M.BasMap.empty, fixity = Fixity.emptyEnv, typing = InitialEnv.primOverloadEnv }, acc)
and doDecs ctx langopt env decs acc = List.foldl (fn (dec, (newenv, acc)) => let val (env', acc) = doDec ctx langopt (mergeEnv (env, newenv)) dec acc
                                                                             in (mergeEnv (newenv, env'), acc)
                                                                             end
                                         ) (emptyEnv, acc) decs
and doExp ctx langopt env (M.BasisExp decs) acc = doDecs ctx langopt env decs acc
  | doExp _ _ env (M.BasIdExp basid) acc = (case M.BasMap.find (#bas env, basid) of
                                                SOME (MkEnv env) => (env, acc)
                                              | NONE => raise Fail ("undefined basis: " ^ basid)
                                           )
  | doExp ctx langopt env (M.LetExp (decs, exp)) acc = let val (env', acc) = doDecs ctx langopt env decs acc
                                                       in doExp ctx langopt (mergeEnv (env, env')) exp acc
                                                       end
and doSmlSource ctx langopt env path acc = let val path = OS.Path.mkAbsolute { path = M.evalPath (#pathMap ctx) path, relativeTo = #baseDir ctx }
                                       val source = let val ins = TextIO.openIn path (* may raise Io *)
                                                    in TextIO.inputAll ins before TextIO.closeIn ins
                                                    end
                                       val ({ fixity, typingEnv, tynameset, toFEnv }, fdecs) = Driver.compile (#driverContext ctx, langopt, { fixity = #fixity env, typingEnv = #typing env, tynameset = #tynameset acc, toFEnv = #toFEnv acc }, path, source) (* TODO: use langopt *)
                                       val newenv = { bas = M.BasMap.empty
                                                    , fixity = fixity
                                                    , typing = typingEnv
                                                    }
                                   in (newenv, { tynameset = tynameset, toFEnv = toFEnv, fdecs = #fdecs acc @ fdecs, cache = #cache acc })
                                   end
and doMlbSource ctx _ path acc = let val baseDir = #baseDir ctx
                                     val path = OS.Path.mkAbsolute { path = M.evalPath (#pathMap ctx) path, relativeTo = baseDir }
                                 in case StringMap.find (#cache acc, path) of
                                        NONE => let val content = let val ins = TextIO.openIn path (* may raise Io *)
                                                                  in TextIO.inputAll ins before TextIO.closeIn ins
                                                                  end
                                                in case MLBParser.P.runParser MLBParser.basfile MLBParser.initialState path (StringStream.fromString { file = path, content = content }) of
                                                       MLBParser.P.Ok (decs, _) => let val ctx' = { driverContext = #driverContext ctx
                                                                                                  , baseDir = OS.Path.dir path
                                                                                                  , pathMap = #pathMap ctx
                                                                                                  , targetInfo = #targetInfo ctx
                                                                                                  , defaultLanguageOptions = #defaultLanguageOptions ctx
                                                                                                  , messageHandler = #messageHandler ctx
                                                                                                  }
                                                                                       val (env', acc) = doDecs ctx' (#defaultLanguageOptions ctx) emptyEnv decs acc
                                                                                       val cache = StringMap.insert (#cache acc, path, env')
                                                                                   in (env', { tynameset = #tynameset acc, toFEnv = #toFEnv acc, fdecs = #fdecs acc, cache = cache })
                                                                                   end
                                                     | MLBParser.P.ParseError e => ( TextIO.output (TextIO.stdErr, e ^ "\n") ; raise Message.Abort )
                                                end
                                      | SOME e => (e, acc)
                                 end
datatype path_setting = PATH_MAP of string
                      | PATH_VAR of string
fun loadPathVar messageHandler (PATH_MAP file, pathMap)
    = let fun loop (ins, n, pathMap) = (case TextIO.inputLine ins of
                                            NONE => (TextIO.closeIn ins; pathMap)
                                          | SOME line => (case String.tokens Char.isSpace line of
                                                              [name, value] => let val path = M.evalPath pathMap value
                                                                                   val pathMap = StringMap.insert (pathMap, name, path)
                                                                               in loop (ins, n + 1, pathMap)
                                                                               end
                                                            | unrecognized => let val pos = { file = file, line = n, column = 1 }
                                                                                  val span = { start = pos, end_ = pos }
                                                                              in case unrecognized of
                                                                                     _ :: _ :: _ => Message.error (messageHandler, [span], "MLB path map", "invalid line: path must not contain spaces")
                                                                                   | _ => Message.error (messageHandler, [span], "MLB path map", "invalid line: too few fields")
                                                                               ; loop (ins, n + 1, pathMap)
                                                                              end
                                                         )
                                       )
      in loop (TextIO.openIn file, 1, pathMap)
      end
  | loadPathVar messageHandler (PATH_VAR setting, pathMap)
    = (case String.tokens (fn c => c = #"=") setting of
           [name, value] => StringMap.insert (pathMap, name, M.evalPath pathMap value)
         | _ => (Message.error (messageHandler, [], "MLB path map", "invalid --mlb-path-var option"); pathMap)
      )
end (* local *)
end; (* structure MLBEval *)
