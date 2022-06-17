structure CMAnalyzer = struct
fun rep (c, n) = CharVector.tabulate (n, fn _ => c)
fun untab s = String.map (fn #"\t" => #" " | c => c) s
fun printPos (name, lines, p) =
    if #file p = name then
        let val l = #line p - 1
            val c = #column p - 1
        in if 0 <= l andalso l < Vector.length lines then
               let val text = Vector.sub (lines, l)
                   val start = Int.max (0, c - 30)
                   val e = Int.min (String.size text, c + 30)
               in print (String.substring (text, start, e - start) ^ "\n")
                ; print (rep (#" ", c - start) ^ "^" ^ "\n")
               end
           else
               () (* not available *)
        end
    else
        () (* what to do? *)
fun printSpan (name, lines, { start = p1, end_ = p2 }) =
    if #file p1 = name andalso #file p2 = name then
        if #line p1 = #line p2 then
            let val l = #line p1 - 1
                val c1 = #column p1 - 1
                val c2 = #column p2 - 1
            in if 0 <= l andalso l < Vector.length lines then
                   let val text = Vector.sub (lines, l)
                       val start = Int.max (0, c1 - 30)
                       val e = Int.min (String.size text, c2 + 30)
                   in print (untab (String.substring (text, start, e - start)) ^ "\n")
                    ; print (rep (#" ", c1 - start) ^ "^" ^ rep (#"~", c2 - c1) ^ "\n")
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
                         val e = Int.min (String.size text, c1 + 30)
                     in print (untab (String.substring (text, start, e - start)) ^ "\n")
                      ; print (rep (#" ", c1 - start) ^ "^" ^ rep (#"~", e - c1 - 1) ^ "\n")
                     end
                 else
                     () (* not available *)
              end
            ; let val l2 = #line p2 - 1
                  val c2 = #column p2 - 1
              in if 0 <= l2 andalso l2 < Vector.length lines then
                     let val text = Vector.sub (lines, l2)
                         val start = Int.max (0, c2 - 30)
                         val e = Int.min (String.size text, c2 + 30)
                     in print (untab (String.substring (text, start, e - start)) ^ "\n")
                      ; print (rep (#"~", c2 - start + 1) ^ "\n")
                     end
                 else
                     () (* not available *)
              end
            )
    else
        () (* what to do? *)

fun parse ({ languageOptions }, name, lines, str) : UnfixedSyntax.Program option
    = let fun printError (s, p1 as {file = f1, line = l1, column = c1}, p2 as {file = f2, line = l2, column = c2})
              = ( if p1 = p2 then
                      print (name ^ ":" ^ Int.toString l1 ^ ":" ^ Int.toString c1 ^ ": " ^ s ^ "\n")
                  else
                      print (name ^ ":" ^ Int.toString l1 ^ ":" ^ Int.toString c1 ^ "-" ^ Int.toString l2 ^ ":" ^ Int.toString c2 ^ ": " ^ s ^ "\n")
                ; printSpan (name, lines, { start = p1, end_ = p2 })
                )
          val lexErrors = ref []
          val lexer = LunarMLParser.makeLexer (LunarMLLex.makeInputFromString str) (name, languageOptions, lexErrors)
          val error = case !lexErrors of
                          [] => false
                        | errors => ( List.app (fn LunarMLLex.TokError (pos, message) => ( print (name ^ ":" ^ Int.toString (#line pos) ^ ":" ^ Int.toString (#column pos) ^ ": syntax error: " ^ message ^ "\n")
                                                                                         ; printPos (name, lines, pos)
                                                                                         )
                                               | LunarMLLex.TokWarning (pos, message) => ( print (name ^ ":" ^ Int.toString (#line pos) ^ ":" ^ Int.toString (#column pos) ^ ": warning: " ^ message ^ "\n")
                                                                                         ; printPos (name, lines, pos)
                                                                                         )
                                               ) errors
                                    ; List.exists (fn LunarMLLex.TokError _ => true | _ => false) errors
                                    )
          val result = #1 (LunarMLParser.parse ((* lookahead *) 0, lexer, printError, name))
      in if error then
             NONE
         else
             SOME result
      end

datatype env' = MkEnv of env
withtype env = env' Syntax.StrIdMap.map
val emptyEnv : env = Syntax.StrIdMap.empty
fun mergeEnv (strMap, strMap') : env = Syntax.StrIdMap.unionWith #2 (strMap, strMap')

type core_usage = (* usedStructures *) Syntax.StrIdSet.set
structure S = Syntax
structure U = UnfixedSyntax

fun doOpen (env : env) (S.MkQualified (strid0 :: strids, lastpart)) = (case Syntax.StrIdMap.find (env, strid0) of
                                                                           NONE => emptyEnv
                                                                         | SOME (MkEnv env') => doOpen env' (S.MkQualified (strids, lastpart))
                                                                      )
  | doOpen env (S.MkQualified ([], strid)) = (case Syntax.StrIdMap.find (env, strid) of
                                                  NONE => emptyEnv
                                                | SOME (MkEnv env') => env'
                                             )

fun goQualified (env : env) (S.MkQualified (strid :: _, _)) = (case Syntax.StrIdMap.find (env, strid) of
                                                                   NONE => Syntax.StrIdSet.singleton strid
                                                                 | SOME _ => Syntax.StrIdSet.empty
                                                              )
  | goQualified env (S.MkQualified ([], _)) = Syntax.StrIdSet.empty

fun goTy env (S.TyVar (_, _)) = Syntax.StrIdSet.empty
  | goTy env (S.RecordType (_, fields, SOME base)) = List.foldl (fn ((_, ty), acc) => Syntax.StrIdSet.union (goTy env ty, acc)) (goTy env base) fields
  | goTy env (S.RecordType (_, fields, NONE)) = List.foldl (fn ((_, ty), acc) => Syntax.StrIdSet.union (goTy env ty, acc)) Syntax.StrIdSet.empty fields
  | goTy env (S.TyCon (_, tyargs, longtycon)) = List.foldl (fn (ty, acc) => Syntax.StrIdSet.union (goTy env ty, acc)) (goQualified env longtycon) tyargs
  | goTy env (S.FnType (_, ty1, ty2)) = Syntax.StrIdSet.union (goTy env ty1, goTy env ty2)

fun goPat env (U.WildcardPat _) = Syntax.StrIdSet.empty
  | goPat env (U.SConPat (_, _)) = Syntax.StrIdSet.empty
  | goPat env (U.NonInfixVIdPat (_, longvid)) = goQualified env longvid
  | goPat env (U.InfixOrVIdPat (_, _)) = Syntax.StrIdSet.empty
  | goPat env (U.JuxtapositionPat (_, pats)) = List.foldl (fn (pat, acc) => Syntax.StrIdSet.union (goPat env pat, acc)) Syntax.StrIdSet.empty pats
  | goPat env (U.ConPat (_, longvid, pat)) = Syntax.StrIdSet.union (goQualified env longvid, goPat env pat)
  | goPat env (U.RecordPat (_, fields)) = List.foldl (fn (U.Field (_, pat), acc) => Syntax.StrIdSet.union (goPat env pat, acc)
                                                     | (U.Ellipsis pat, acc) => Syntax.StrIdSet.union (goPat env pat, acc)
                                                     ) Syntax.StrIdSet.empty fields
  | goPat env (U.TypedPat (_, pat, ty)) = Syntax.StrIdSet.union (goPat env pat, goTy env ty)
  | goPat env (U.ConjunctivePat (_, pat1, pat2)) = Syntax.StrIdSet.union (goPat env pat1, goPat env pat2)
  | goPat env (U.VectorPat (_, pats, _)) = Vector.foldl (fn (pat, acc) => Syntax.StrIdSet.union (goPat env pat, acc)) Syntax.StrIdSet.empty pats

fun goExp env (U.SConExp (_, _)) = Syntax.StrIdSet.empty
  | goExp env (U.NonInfixVIdExp (_, longvid)) = goQualified env longvid
  | goExp env (U.InfixOrVIdExp (_, _)) = Syntax.StrIdSet.empty
  | goExp env (U.RecordExp (_, items)) = List.foldl (fn (U.Field (_, e), acc) => Syntax.StrIdSet.union (goExp env e, acc)
                                                    | (U.Ellipsis e, acc) => Syntax.StrIdSet.union (goExp env e, acc)
                                                    ) Syntax.StrIdSet.empty items
  | goExp env (U.RecordUpdateExp (_, base, items)) = List.foldl (fn (U.Field (_, e), acc) => Syntax.StrIdSet.union (goExp env e, acc)
                                                                | (U.Ellipsis e, acc) => Syntax.StrIdSet.union (goExp env e, acc)
                                                                ) (goExp env base) items
  | goExp env (U.LetInExp (_, decs, exp)) = let val (env', usage) = goDecs env decs
                                            in Syntax.StrIdSet.union (usage, goExp (mergeEnv (env, env')) exp)
                                            end
  | goExp env (U.JuxtapositionExp (_, exps)) = List.foldl (fn (e, acc) => Syntax.StrIdSet.union (goExp env e, acc)) Syntax.StrIdSet.empty exps
  | goExp env (U.AppExp (_, exp1, exp2)) = Syntax.StrIdSet.union (goExp env exp1, goExp env exp2)
  | goExp env (U.TypedExp (_, exp, ty)) = Syntax.StrIdSet.union (goExp env exp, goTy env ty)
  | goExp env (U.HandleExp (_, exp, matches)) = List.foldl (fn ((pat, body), acc) => Syntax.StrIdSet.union (goPat env pat, Syntax.StrIdSet.union (goExp env body, acc))) (goExp env exp) matches
  | goExp env (U.RaiseExp (_, exp)) = goExp env exp
  | goExp env (U.IfThenElseExp (_, exp1, exp2, exp3)) = Syntax.StrIdSet.union (goExp env exp1, Syntax.StrIdSet.union (goExp env exp2, goExp env exp3))
  | goExp env (U.WhileDoExp (_, exp1, exp2)) = Syntax.StrIdSet.union (goExp env exp1, goExp env exp2)
  | goExp env (U.CaseExp (_, exp, matches)) = List.foldl (fn ((pat, body), acc) => Syntax.StrIdSet.union (goPat env pat, Syntax.StrIdSet.union (goExp env body, acc))) (goExp env exp) matches
  | goExp env (U.FnExp (_, matches)) = List.foldl (fn ((pat, body), acc) => Syntax.StrIdSet.union (goPat env pat, Syntax.StrIdSet.union (goExp env body, acc))) Syntax.StrIdSet.empty matches
  | goExp env (U.ProjectionExp (_, _)) = Syntax.StrIdSet.empty
  | goExp env (U.ListExp (_, exps)) = Vector.foldl (fn (exp, acc) => Syntax.StrIdSet.union (goExp env exp, acc)) Syntax.StrIdSet.empty exps
  | goExp env (U.VectorExp (_, exps)) = Vector.foldl (fn (exp, acc) => Syntax.StrIdSet.union (goExp env exp, acc)) Syntax.StrIdSet.empty exps
  | goExp env (U.PrimExp (_, _, tys, exps)) = Vector.foldl (fn (exp, acc) => Syntax.StrIdSet.union (goExp env exp, acc)) (Vector.foldl (fn (ty, acc) => Syntax.StrIdSet.union (goTy env ty, acc)) Syntax.StrIdSet.empty tys) exps
and goDec env (U.ValDec (_, _, valbinds)) = (emptyEnv, List.foldl (fn (U.PatBind (_, pat, exp), acc) => Syntax.StrIdSet.union (goPat env pat, Syntax.StrIdSet.union (goExp env exp, acc))) Syntax.StrIdSet.empty valbinds)
  | goDec env (U.RecValDec (_, _, valbinds)) = (emptyEnv, List.foldl (fn (U.PatBind (_, pat, exp), acc) => Syntax.StrIdSet.union (goPat env pat, Syntax.StrIdSet.union (goExp env exp, acc))) Syntax.StrIdSet.empty valbinds)
  | goDec env (U.FValDec (_, _, fvalbinds)) = (emptyEnv, List.foldl (fn (U.FValBind (_, rules), acc) =>
                                                                        List.foldl (fn (U.FMRule (_, U.FPat (_, pats), optTy, exp), acc) =>
                                                                                       let val acc = Syntax.StrIdSet.union (goExp env exp, case optTy of NONE => acc | SOME ty => Syntax.StrIdSet.union (goTy env ty, acc))
                                                                                       in List.foldl (fn (pat, acc) => Syntax.StrIdSet.union (goPat env pat, acc)) acc pats
                                                                                       end
                                                                                   ) acc rules
                                                                    ) Syntax.StrIdSet.empty fvalbinds)
  | goDec env (U.TypeDec (_, typbinds)) = (emptyEnv, List.foldl (fn (S.TypBind (_, _, _, ty), acc) => Syntax.StrIdSet.union (goTy env ty, acc)) Syntax.StrIdSet.empty typbinds)
  | goDec env (U.DatatypeDec (_, datbinds, typbinds)) = let val acc = List.foldl (fn (S.DatBind (_, _, _, conbinds), acc) =>
                                                                                     List.foldl (fn (S.ConBind (_, _, SOME ty), acc) => Syntax.StrIdSet.union (goTy env ty, acc)
                                                                                                | (S.ConBind (_, _, NONE), acc) => acc) acc conbinds
                                                                                 ) Syntax.StrIdSet.empty datbinds
                                                        in (emptyEnv, List.foldl (fn (S.TypBind (_, _, _, ty), acc) => Syntax.StrIdSet.union (goTy env ty, acc)) acc typbinds)
                                                        end
  | goDec env (U.DatatypeRepDec (_, _, longtycon)) = (emptyEnv, goQualified env longtycon)
  | goDec env (U.AbstypeDec (_, datbinds, typbinds, decs)) = let val acc = List.foldl (fn (S.DatBind (_, _, _, conbinds), acc) =>
                                                                                          List.foldl (fn (S.ConBind (_, _, SOME ty), acc) => Syntax.StrIdSet.union (goTy env ty, acc)
                                                                                                     | (S.ConBind (_, _, NONE), acc) => acc) acc conbinds
                                                                                      ) Syntax.StrIdSet.empty datbinds
                                                                 val acc = List.foldl (fn (S.TypBind (_, _, _, ty), acc) => Syntax.StrIdSet.union (goTy env ty, acc)) acc typbinds
                                                                 val (env', decs') = goDecs env decs
                                                             in (env', Syntax.StrIdSet.union (acc, decs'))
                                                             end
  | goDec env (U.ExceptionDec (_, exbinds)) = (emptyEnv, List.foldl (fn (S.ExBind (_, _, SOME ty), acc) => Syntax.StrIdSet.union (goTy env ty, acc)
                                                                    | (S.ExBind (_, _, NONE), acc) => acc
                                                                    | (S.ExReplication (_, _, longvid), acc) => Syntax.StrIdSet.union (goQualified env longvid, acc)
                                                                    ) Syntax.StrIdSet.empty exbinds)
  | goDec env (U.LocalDec (_, decs1, decs2)) = let val (env', acc1) = goDecs env decs1
                                                   val (env'', acc2) = goDecs (mergeEnv (env, env')) decs2
                                               in (env'', Syntax.StrIdSet.union (acc1, acc2))
                                               end
  | goDec env (U.OpenDec (_, longstrids)) = List.foldl (fn (longstrid, (env', usage)) =>
                                                           let val usage = case longstrid of
                                                                               Syntax.MkQualified (strid :: _, _) => (case Syntax.StrIdMap.find (env, strid) of
                                                                                                                          NONE => Syntax.StrIdSet.add (usage, strid)
                                                                                                                        | SOME _ => usage
                                                                                                                     )
                                                                             | Syntax.MkQualified ([], strid) => (case Syntax.StrIdMap.find (env, strid) of
                                                                                                                      NONE => Syntax.StrIdSet.add (usage, strid)
                                                                                                                    | SOME _ => usage
                                                                                                                 )
                                                           in (mergeEnv (env', doOpen env longstrid), usage)
                                                           end
                                                       ) (emptyEnv, Syntax.StrIdSet.empty) longstrids
  | goDec env (U.FixityDec (_, _, _)) = (emptyEnv, Syntax.StrIdSet.empty)
  | goDec env (U.OverloadDec (_, _, longtycon, fields)) = (emptyEnv, List.foldl (fn ((_, e), acc) => Syntax.StrIdSet.union (goExp env e, acc)) (goQualified env longtycon) fields)
  | goDec env (U.EqualityDec (_, _, longtycon, exp)) = (emptyEnv, Syntax.StrIdSet.union (goQualified env longtycon, goExp env exp))
and goDecs env [] = (emptyEnv, Syntax.StrIdSet.empty)
  | goDecs env (dec :: decs) = let val (env', set) = goDec env dec
                                   val (env'', set') = goDecs (mergeEnv (env, env')) decs
                               in (mergeEnv (env', env''), Syntax.StrIdSet.union (set, set'))
                               end

type module_env = { strMap : env' Syntax.StrIdMap.map
                  , sigMap : env' Syntax.SigIdMap.map
                  , funMap : env' Syntax.FunIdMap.map
                  }
val emptyModuleEnv : module_env = { strMap = Syntax.StrIdMap.empty, sigMap = Syntax.SigIdMap.empty, funMap = Syntax.FunIdMap.empty }
fun mergeModuleEnv ({ strMap, sigMap, funMap }, { strMap = strMap', sigMap = sigMap', funMap = funMap' }) : module_env
    = { strMap = Syntax.StrIdMap.unionWith #2 (strMap, strMap'), sigMap = Syntax.SigIdMap.unionWith #2 (sigMap, sigMap'), funMap = Syntax.FunIdMap.unionWith #2 (funMap, funMap') }

fun toCoreEnv ({ strMap, ... } : module_env) : env = strMap

type module_usage = { usedStructures : Syntax.StrIdSet.set
                    , usedSignatures : Syntax.SigIdSet.set
                    , usedFunctors : Syntax.FunIdSet.set
                    }
val emptyModuleUsage : module_usage = { usedStructures = Syntax.StrIdSet.empty
                                      , usedSignatures = Syntax.SigIdSet.empty
                                      , usedFunctors = Syntax.FunIdSet.empty
                                      }
fun unionModuleUsage (a, b) : module_usage = { usedStructures = Syntax.StrIdSet.union (#usedStructures a, #usedStructures b)
                                             , usedSignatures = Syntax.SigIdSet.union (#usedSignatures a, #usedSignatures b)
                                             , usedFunctors = Syntax.FunIdSet.union (#usedFunctors a, #usedFunctors b)
                                             }
fun fromCoreUsage set : module_usage = { usedStructures = set, usedSignatures = Syntax.SigIdSet.empty, usedFunctors = Syntax.FunIdSet.empty }

fun goSpec (env : module_env) (S.ValDesc (_, items)) : env' Syntax.StrIdMap.map * module_usage
    = (Syntax.StrIdMap.empty, fromCoreUsage (List.foldl (fn ((_, ty), acc) => Syntax.StrIdSet.union (goTy (toCoreEnv env) ty, acc)) Syntax.StrIdSet.empty items))
  | goSpec env (S.TypeDesc (_, _)) = (Syntax.StrIdMap.empty, emptyModuleUsage)
  | goSpec env (S.EqtypeDesc (_, _)) = (Syntax.StrIdMap.empty, emptyModuleUsage)
  | goSpec env (S.DatDesc (_, datdescs, typdescs)) = let val acc = List.foldl (fn ((_, _, condescs), acc) =>
                                                                                  List.foldl (fn (S.ConBind (_, _, SOME ty), acc) => Syntax.StrIdSet.union (goTy (toCoreEnv env) ty, acc)
                                                                                             | (S.ConBind (_, _, NONE), acc) => acc) acc condescs
                                                                              ) Syntax.StrIdSet.empty datdescs
                                                         val acc = List.foldl (fn (S.TypBind (_, _, _, ty), acc) => Syntax.StrIdSet.union (goTy (toCoreEnv env) ty, acc)) acc typdescs
                                                     in (Syntax.StrIdMap.empty, fromCoreUsage acc)
                                                     end
  | goSpec env (S.DatatypeRepSpec (_, _, longtycon)) = (Syntax.StrIdMap.empty, fromCoreUsage (goQualified (toCoreEnv env) longtycon))
  | goSpec env (S.ExDesc (_, descs)) = (Syntax.StrIdMap.empty, fromCoreUsage (List.foldl (fn ((_, NONE), acc) => acc
                                                                                         | ((_, SOME ty), acc) => Syntax.StrIdSet.union (goTy (toCoreEnv env) ty, acc)) Syntax.StrIdSet.empty descs))
  | goSpec env (S.StrDesc (_, descs)) = List.foldl (fn ((strid, sigexp), (strMap, usage)) =>
                                                       let val (sig', usage') = goSigExp env sigexp
                                                       in (Syntax.StrIdMap.insert (strMap, strid, MkEnv sig'), unionModuleUsage (usage, usage'))
                                                       end) (Syntax.StrIdMap.empty, emptyModuleUsage) descs
  | goSpec env (S.Include (_, sigexp)) = goSigExp env sigexp
  | goSpec env (S.Sharing (_, specs, longtycons)) = goSpecs env specs
  | goSpec env (S.SharingStructure (_, specs, longstrids)) = goSpecs env specs
  | goSpec env (S.TypeAliasDesc (_, descs)) = let val usage = List.foldl (fn ((_, _, ty), acc) => Syntax.StrIdSet.union (goTy (toCoreEnv env) ty, acc)) Syntax.StrIdSet.empty descs
                                              in (Syntax.StrIdMap.empty, fromCoreUsage usage)
                                              end
and goSpecs env [] = (Syntax.StrIdMap.empty, emptyModuleUsage)
  | goSpecs env (spec :: specs) = let val (env', usage) = goSpec env spec
                                      val (env'', usage') = goSpecs (mergeModuleEnv (env, { strMap = env', sigMap = Syntax.SigIdMap.empty, funMap = Syntax.FunIdMap.empty })) specs
                                  in (Syntax.StrIdMap.unionWith #2 (env', env''), unionModuleUsage (usage, usage'))
                                  end
and goSigExp env (S.BasicSigExp (_, specs)) : env' Syntax.StrIdMap.map * module_usage = goSpecs env specs
  | goSigExp env (S.SigIdExp (_, sigid)) = (case Syntax.SigIdMap.find (#sigMap env, sigid) of
                                                NONE => (Syntax.StrIdMap.empty, { usedStructures = Syntax.StrIdSet.empty, usedSignatures = Syntax.SigIdSet.singleton sigid, usedFunctors = Syntax.FunIdSet.empty })
                                              | SOME (MkEnv strMap) => (strMap, emptyModuleUsage)
                                           )
  | goSigExp env (S.TypeRealisationExp (_, sigexp, _, longtycon, ty)) = let val usage = goTy (toCoreEnv env) ty
                                                                        in (Syntax.StrIdMap.empty, fromCoreUsage usage)
                                                                        end

fun goStrExp (env : module_env) (S.StructExp (_, strdecs)) : env' Syntax.StrIdMap.map * module_usage = goStrDecs env strdecs
  | goStrExp env (S.StrIdExp (_, longstrid)) = (case longstrid of
                                                    S.MkQualified ([], strid) => (case Syntax.StrIdMap.find (#strMap env, strid) of
                                                                                      NONE => (Syntax.StrIdMap.empty, { usedStructures = Syntax.StrIdSet.singleton strid, usedSignatures = Syntax.SigIdSet.empty, usedFunctors = Syntax.FunIdSet.empty })
                                                                                    | SOME (MkEnv strMap) => (strMap, emptyModuleUsage)
                                                                                 )
                                                  | S.MkQualified (strid :: strids, lastpart) => (case Syntax.StrIdMap.find (#strMap env, strid) of
                                                                                                      NONE => (Syntax.StrIdMap.empty, { usedStructures = Syntax.StrIdSet.singleton strid, usedSignatures = Syntax.SigIdSet.empty, usedFunctors = Syntax.FunIdSet.empty })
                                                                                                    | SOME (MkEnv strMap) =>
                                                                                                      let fun go (strMap, [], strid) = (case Syntax.StrIdMap.find (strMap, strid) of
                                                                                                                                            NONE => Syntax.StrIdMap.empty
                                                                                                                                          | SOME (MkEnv strMap) => strMap
                                                                                                                                       )
                                                                                                            | go (strMap, strid :: strids, lastpart) = (case Syntax.StrIdMap.find (strMap, strid) of
                                                                                                                                                            NONE => Syntax.StrIdMap.empty
                                                                                                                                                          | SOME (MkEnv strMap) => go (strMap, strids, lastpart)
                                                                                                                                                       )
                                                                                                      in (go (strMap, strids, lastpart), emptyModuleUsage)
                                                                                                      end
                                                                                                 )
                                               )
  | goStrExp env (S.TransparentConstraintExp (_, strexp, sigexp)) = let val (_, usage) = goStrExp env strexp
                                                                        val (s, usage') = goSigExp env sigexp
                                                                    in (s, unionModuleUsage (usage, usage'))
                                                                    end
  | goStrExp env (S.OpaqueConstraintExp (_, strexp, sigexp)) = let val (_, usage) = goStrExp env strexp
                                                                   val (s, usage') = goSigExp env sigexp
                                                               in (s, unionModuleUsage (usage, usage'))
                                                               end
  | goStrExp env (S.FunctorAppExp (_, funid, strexp)) = let val (_, usage) = goStrExp env strexp
                                                        in case Syntax.FunIdMap.find (#funMap env, funid) of
                                                               NONE => (Syntax.StrIdMap.empty, { usedStructures = #usedStructures usage, usedSignatures = #usedSignatures usage, usedFunctors = Syntax.FunIdSet.add (#usedFunctors usage, funid) })
                                                             | SOME (MkEnv s) => (s, usage)
                                                        end
  | goStrExp env (S.LetInStrExp (_, strdecs, strexp)) = let val (s, usage) = goStrDecs env strdecs
                                                            val env' = { strMap = Syntax.StrIdMap.unionWith #2 (#strMap env, s), sigMap = #sigMap env, funMap = #funMap env }
                                                            val (s', usage') = goStrExp env' strexp
                                                        in (s', unionModuleUsage (usage, usage'))
                                                        end
and goStrDec (env : module_env) (S.CoreDec (_, dec)) = let val (strMap, usage) = goDec (toCoreEnv env) dec
                                                       in (strMap, fromCoreUsage usage)
                                                       end
  | goStrDec env (S.StrBindDec (_, binds)) = List.foldl (fn ((strid, strexp), (strMap, usage)) =>
                                                            let val (s, usage') = goStrExp env strexp
                                                            in (Syntax.StrIdMap.insert (strMap, strid, MkEnv s), unionModuleUsage (usage, usage'))
                                                            end) (Syntax.StrIdMap.empty, emptyModuleUsage) binds
  | goStrDec env (S.LocalStrDec (_, decs1, decs2)) = let val (env', usage1) = goStrDecs env decs1
                                                         val (env'', usage2) = goStrDecs (mergeModuleEnv (env, { strMap = env', sigMap = Syntax.SigIdMap.empty, funMap = Syntax.FunIdMap.empty })) decs2
                                                     in (env'', unionModuleUsage (usage1, usage2))
                                                     end
and goStrDecs env [] = (Syntax.StrIdMap.empty, emptyModuleUsage)
  | goStrDecs env (dec :: decs) = let val (env', usage1) = goStrDec env dec
                                      val (env'', usage2) = goStrDecs (mergeModuleEnv (env, { strMap = env', sigMap = Syntax.SigIdMap.empty, funMap = Syntax.FunIdMap.empty })) decs
                                  in (Syntax.StrIdMap.unionWith #2 (env', env''), unionModuleUsage (usage1, usage2))
                                  end

fun goTopDec env (S.StrDec dec) = let val (strMap, usage) = goStrDec env dec
                                  in ({ strMap = strMap, sigMap = Syntax.SigIdMap.empty, funMap = Syntax.FunIdMap.empty }, usage)
                                  end
  | goTopDec env (S.SigDec binds) = let val (sigMap, usage) = List.foldl (fn ((sigid, sigexp), (sigMap, usage)) =>
                                                                             let val (s, usage') = goSigExp env sigexp
                                                                             in (Syntax.SigIdMap.insert (sigMap, sigid, MkEnv s), unionModuleUsage (usage, usage'))
                                                                             end) (Syntax.SigIdMap.empty, emptyModuleUsage) binds
                                    in ({ strMap = Syntax.StrIdMap.empty, sigMap = sigMap, funMap = Syntax.FunIdMap.empty }, usage)
                                    end
  | goTopDec env (S.FunDec binds) = let val (funMap, usage) = List.foldl (fn ((_, funid, S.NamedFunExp (paramId, sigexp, body)), (funMap, usage)) =>
                                                                             let val (p, usage') = goSigExp env sigexp
                                                                                 val env' = { strMap = Syntax.StrIdMap.insert (#strMap env, paramId, MkEnv p), sigMap = #sigMap env, funMap = #funMap env }
                                                                                 val (s, usage'') = goStrExp env' body
                                                                             in (Syntax.FunIdMap.insert (funMap, funid, MkEnv s), unionModuleUsage (usage, unionModuleUsage (usage', usage'')))
                                                                             end
                                                                         | ((_, funid, S.AnonymousFunExp (sigexp, body)), (funMap, usage)) =>
                                                                           let val (p, usage') = goSigExp env sigexp
                                                                               val env' = { strMap = Syntax.StrIdMap.unionWith #2 (#strMap env, p), sigMap = #sigMap env, funMap = #funMap env }
                                                                               val (s, usage'') = goStrExp env' body
                                                                           in (Syntax.FunIdMap.insert (funMap, funid, MkEnv s), unionModuleUsage (usage, unionModuleUsage (usage', usage'')))
                                                                           end) (Syntax.FunIdMap.empty, emptyModuleUsage) binds
                                    in ({ strMap = Syntax.StrIdMap.empty, sigMap = Syntax.SigIdMap.empty, funMap = funMap }, usage)
                                    end

fun goTopDecs env [] = (emptyModuleEnv, emptyModuleUsage)
  | goTopDecs env (dec :: decs) = let val (env', usage1) = goTopDec env dec
                                      val (env'', usage2) = goTopDecs (mergeModuleEnv (env, env')) decs
                                  in (mergeModuleEnv (env', env''), unionModuleUsage (usage1, usage2))
                                  end

type file_info = { definedStructures : Syntax.StrIdSet.set
                 , definedSignatures : Syntax.SigIdSet.set
                 , definedFunctors : Syntax.FunIdSet.set
                 , usedStructures : Syntax.StrIdSet.set
                 , usedSignatures : Syntax.SigIdSet.set
                 , usedFunctors : Syntax.FunIdSet.set
                 }

fun goProgram program : file_info
    = let val ({ strMap, sigMap, funMap }, { usedStructures, usedSignatures, usedFunctors }) = goTopDecs emptyModuleEnv (List.concat program)
      in { definedStructures = Syntax.StrIdMap.foldli (fn (strid, _, set) => Syntax.StrIdSet.add (set, strid)) Syntax.StrIdSet.empty strMap
         , definedSignatures = Syntax.SigIdMap.foldli (fn (sigid, _, set) => Syntax.SigIdSet.add (set, sigid)) Syntax.SigIdSet.empty sigMap
         , definedFunctors = Syntax.FunIdMap.foldli (fn (funid, _, set) => Syntax.FunIdSet.add (set, funid)) Syntax.FunIdSet.empty funMap
         , usedStructures = usedStructures
         , usedSignatures = usedSignatures
         , usedFunctors = usedFunctors
         }
      end
end;
