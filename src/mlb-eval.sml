structure MLBEval = struct
local structure M = MLBSyntax in
type Context = { driverContext : Driver.Context
               , baseDir : string
               , pathMap : string M.StringMap.map
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
fun envWithTyping typing : Env = { bas = M.BasMap.empty
                                 , fixity = Fixity.emptyEnv
                                 , typing = typing
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
fun doDec ctx env (M.BasisDec binds) acc = let val (bas, acc) = List.foldl (fn ((basid, basexp), (bas, acc)) =>
                                                                               let val (env', acc) = doExp ctx env basexp acc
                                                                               in (M.BasMap.insert (bas, basid, MkEnv env'), acc)
                                                                               end
                                                                           ) (M.BasMap.empty, acc) binds
                                           in (envWithBasis bas, acc)
                                           end
  | doDec ctx env (M.OpenDec basids) acc = let val env' = List.foldl (fn (basid, newenv) =>
                                                                         case M.BasMap.find (#bas env, basid) of
                                                                             SOME (MkEnv env) => mergeEnv (newenv, env)
                                                                           | NONE => raise Fail ("undefined basis: " ^ basid)
                                                                     ) emptyEnv basids
                                           in (env', acc)
                                           end
  | doDec ctx env (M.LocalDec (decs1, decs2)) acc = let val (env', acc) = doDecs ctx env decs1 acc
                                                    in doDecs ctx (mergeEnv (env, env')) decs2 acc
                                                    end
  | doDec ctx env (M.StructureDec binds) acc = let val strMap = #strMap (#typing env)
                                                   val strMap' = List.foldl (fn ((id1, id2), m) =>
                                                                                case Syntax.StrIdMap.find (strMap, id2) of
                                                                                    SOME value => Syntax.StrIdMap.insert (m, id1, value)
                                                                                  | NONE => raise Fail ("undefined structure: " ^ Syntax.print_StrId id2)
                                                                            ) Syntax.StrIdMap.empty binds
                                               in (envWithTyping (Typing.envWithStrMap strMap'), acc)
                                               end
  | doDec ctx env (M.SignatureDec binds) acc = let val sigMap = #sigMap (#typing env)
                                                   val sigMap' = List.foldl (fn ((id1, id2), m) =>
                                                                                case Syntax.SigIdMap.find (sigMap, id2) of
                                                                                    SOME value => Syntax.SigIdMap.insert (m, id1, value)
                                                                                  | NONE => raise Fail ("undefined signature: " ^ Syntax.print_SigId id2)
                                                                            ) Syntax.SigIdMap.empty binds
                                               in (envWithTyping (Typing.envWithSigMap sigMap'), acc)
                                               end
  | doDec ctx env (M.FunctorDec binds) acc = let val funMap = #funMap (#typing env)
                                                 val funMap' = List.foldl (fn ((id1, id2), m) =>
                                                                              case Syntax.FunIdMap.find (funMap, id2) of
                                                                                  SOME value => Syntax.FunIdMap.insert (m, id1, value)
                                                                                | NONE => raise Fail ("undefined functor: " ^ Syntax.print_FunId id2)
                                                                          ) Syntax.FunIdMap.empty binds
                                             in (envWithTyping (Typing.envWithFunMap funMap'), acc)
                                             end
  | doDec ctx env (M.PathDec path) acc = (case OS.Path.ext path of
                                              SOME "sml" => doSmlSource ctx env path acc
                                            | SOME "fun" => doSmlSource ctx env path acc
                                            | SOME "sig" => doSmlSource ctx env path acc
                                            | SOME "mlb" => doMlbSource ctx env path acc
                                            | _ => raise Fail ("unrecognized file extension: " ^ path)
                                         )
  | doDec ctx env (M.AnnotationDec (_, decs)) acc = doDecs ctx env decs acc (* not implemented yet *)
  | doDec ctx env M.PrimDec acc = ({ bas = M.BasMap.empty, fixity = InitialEnv.initialFixityEnv, typing = InitialEnv.initialEnv }, acc)
and doDecs ctx env decs acc = List.foldl (fn (dec, (newenv, acc)) => let val (env', acc) = doDec ctx (mergeEnv (env, newenv)) dec acc
                                                                     in (mergeEnv (newenv, env'), acc)
                                                                     end
                                         ) (emptyEnv, acc) decs
and doExp ctx env (M.BasisExp decs) acc = doDecs ctx env decs acc
  | doExp ctx env (M.BasIdExp basid) acc = (case M.BasMap.find (#bas env, basid) of
                                                SOME (MkEnv env) => (env, acc)
                                              | NONE => raise Fail ("undefined basis: " ^ basid)
                                           )
  | doExp ctx env (M.LetExp (decs, exp)) acc = let val (env', acc) = doDecs ctx env decs acc
                                               in doExp ctx (mergeEnv (env, env')) exp acc
                                               end
and doSmlSource ctx env path acc = let val path = OS.Path.mkAbsolute { path = M.evalPath (#pathMap ctx) path, relativeTo = #baseDir ctx }
                                       val source = let val ins = TextIO.openIn path (* may raise Io *)
                                                    in TextIO.inputAll ins before TextIO.closeIn ins
                                                    end
                                       val ({ fixity, typingEnv, tynameset, toFEnv }, fdecs) = Driver.compile (#driverContext ctx, { fixity = #fixity env, typingEnv = #typing env, tynameset = #tynameset acc, toFEnv = #toFEnv acc }, path, source)
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
                                                         MLBParser.P.Ok (decs, ()) => let val ctx' = { driverContext = #driverContext ctx, baseDir = OS.Path.dir path, pathMap = #pathMap ctx }
                                                                                          val (env', acc) = doDecs ctx' emptyEnv decs acc
                                                                                          val cache = M.StringMap.insert (#cache acc, path, env')
                                                                                      in (env', { tynameset = #tynameset acc, toFEnv = #toFEnv acc, fdecs = #fdecs acc, cache = cache })
                                                                                      end
                                                       | MLBParser.P.ParseError e => ( print (e ^ "\n") ; raise Driver.Abort )
                                                  end
                                        | SOME e => (e, acc)
                                   end
end
end
