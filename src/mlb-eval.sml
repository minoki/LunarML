structure MLBEval = struct
local structure M = MLBSyntax in
type Context = { driverContext : Driver.Context
               , baseDir : string
               , pathMap : string M.StringMap.map
               , targetInfo : TargetInfo.target_info
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
type Code = { tynameset : USyntax.TyNameSet.set
            , toFEnv : ToFSyntax.Env
            , fdecs : FSyntax.Dec list
            , cache : Env M.StringMap.map
            }
val initialCode = { tynameset = InitialEnv.initialTyNameSet
                  , toFEnv = ToFSyntax.initialEnv
                  , fdecs = []
                  , cache = M.StringMap.empty
                  }
fun applyAnnotation (ann, langopt) = case String.tokens Char.isSpace ann of
                                         [name, value] => (case LanguageOptions.setByName name of
                                                              SOME setter => (case value of
                                                                                  "true" => setter true langopt
                                                                                | "false" => setter false langopt
                                                                                | _ => raise Fail ("unrecognized annotation: " ^ ann)
                                                                             )
                                                            | NONE => raise Fail ("unrecognized annotation: " ^ ann)
                                                          )
                                       | _ => raise Fail ("unrecognized annotation: " ^ ann)
fun doDec (ctx : Context) langopt env (M.BasisDec binds) acc = let val (bas, acc) = List.foldl (fn ((basid, basexp), (bas, acc)) =>
                                                                                                   let val (env', acc) = doExp ctx langopt env basexp acc
                                                                                                   in (M.BasMap.insert (bas, basid, MkEnv env'), acc)
                                                                                                   end
                                                                                               ) (M.BasMap.empty, acc) binds
                                                               in (envWithBasis bas, acc)
                                                               end
  | doDec ctx langopt env (M.OpenDec basids) acc = let val env' = List.foldl (fn (basid, newenv) =>
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
                                                                                , tyNameMap = USyntax.TyNameMap.unionWith #2 (#tyNameMap (#typing env'), #tyNameMap (#typing env''))
                                                                                , strMap = #strMap (#typing env'')
                                                                                , sigMap = #sigMap (#typing env'')
                                                                                , funMap = #funMap (#typing env'')
                                                                                , boundTyVars = #boundTyVars (#typing env'')
                                                                                }
                                                            in ({ bas = #bas env'', fixity = #fixity env'', typing = typingEnv }, acc)
                                                            end
  | doDec ctx langopt env (M.StructureDec binds) acc = let val strMap_fixity = #strMap (#idStatusMap (#fixity env))
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
  | doDec ctx langopt env (M.SignatureDec binds) acc = let val sigMap_fixity = #sigMap (#fixity env)
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
                                                       in ({ bas = M.BasMap.empty, fixity = Fixity.envWithSigMap sigMap_fixity, typing = Typing.envWithSigMap sigMap_typing' }, acc)
                                                       end
  | doDec ctx langopt env (M.FunctorDec binds) acc = let val funMap_fixity = #funMap (#fixity env)
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
                                                     in ({ bas = M.BasMap.empty, fixity = Fixity.envWithFunMap funMap_fixity, typing = Typing.envWithFunMap funMap_typing' }, acc)
                                                     end
  | doDec ctx langopt env (M.PathDec path) acc = (case OS.Path.ext path of
                                                      SOME "sml" => doSmlSource ctx langopt env path acc
                                                    | SOME "fun" => doSmlSource ctx langopt env path acc
                                                    | SOME "sig" => doSmlSource ctx langopt env path acc
                                                    | SOME "mlb" => doMlbSource ctx env path acc
                                                    | _ => raise Fail ("unrecognized file extension: " ^ path)
                                                 )
  | doDec ctx langopt env (M.AnnotationDec (anns, decs)) acc = let val langopt = List.foldl applyAnnotation langopt anns
                                                               in doDecs ctx langopt env decs acc
                                                               end
  | doDec ctx langopt env M.PrimDec acc = ({ bas = M.BasMap.empty, fixity = InitialEnv.initialFixityEnv, typing = InitialEnv.initialEnv }, acc)
  | doDec ctx langopt env M.PrimOverloadDec acc = ({ bas = M.BasMap.empty, fixity = Fixity.emptyEnv, typing = InitialEnv.primOverloadEnv }, acc)
and doDecs ctx langopt env decs acc = List.foldl (fn (dec, (newenv, acc)) => let val (env', acc) = doDec ctx langopt (mergeEnv (env, newenv)) dec acc
                                                                             in (mergeEnv (newenv, env'), acc)
                                                                             end
                                         ) (emptyEnv, acc) decs
and doExp ctx langopt env (M.BasisExp decs) acc = doDecs ctx langopt env decs acc
  | doExp ctx langopt env (M.BasIdExp basid) acc = (case M.BasMap.find (#bas env, basid) of
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
and doMlbSource ctx env path acc = let val baseDir = #baseDir ctx
                                       val path = OS.Path.mkAbsolute { path = M.evalPath (#pathMap ctx) path, relativeTo = baseDir }
                                   in case M.StringMap.find (#cache acc, path) of
                                          NONE => let val content = let val ins = TextIO.openIn path (* may raise Io *)
                                                                   in TextIO.inputAll ins before TextIO.closeIn ins
                                                                   end
                                                  in case MLBParser.P.runParser MLBParser.basfile () path (StringStream.fromString { file = path, content = content }) of
                                                         MLBParser.P.Ok (decs, ()) => let val ctx' = { driverContext = #driverContext ctx, baseDir = OS.Path.dir path, pathMap = #pathMap ctx, targetInfo = #targetInfo ctx }
                                                                                          val (env', acc) = doDecs ctx' LanguageOptions.default emptyEnv decs acc
                                                                                          val cache = M.StringMap.insert (#cache acc, path, env')
                                                                                      in (env', { tynameset = #tynameset acc, toFEnv = #toFEnv acc, fdecs = #fdecs acc, cache = cache })
                                                                                      end
                                                       | MLBParser.P.ParseError e => ( print (e ^ "\n") ; raise Driver.Abort )
                                                  end
                                        | SOME e => (e, acc)
                                   end
end
end
