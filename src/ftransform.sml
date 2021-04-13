structure FTransform = struct
structure F = FSyntax
type Context = { nextVId : int ref
               }
fun freshVId(ctx : Context, name: string) = let val n = !(#nextVId ctx)
                                            in #nextVId ctx := n + 1
                                             ; USyntax.MkVId(name, n)
                                            end
datatype Env = MkEnv of { strMap : Env Syntax.StrIdMap.map
                        }
val emptyEnv = MkEnv { strMap = Syntax.StrIdMap.empty }
fun desugarPatternMatches (ctx: Context): { doExp: Env -> F.Exp -> F.Exp, doValBind: Env -> F.ValBind -> F.ValBind, doDec : Env -> F.Dec -> F.Dec }
    = let fun doExp (env: Env) exp0
              = (case exp0 of
                     F.SConExp scon => exp0
                   | F.VarExp longvid => exp0
                   | F.RecordExp fields => F.RecordExp (List.map (fn (label, e) => (label, doExp env e)) fields)
                   | F.LetExp (valbind, exp) => F.LetExp (doValBind env valbind, doExp env exp) (* TODO: modify environment *)
                   | F.LetRecExp (valbinds, exp) => F.LetRecExp (List.map (doValBind env) valbinds, doExp env exp)
                   | F.AppExp(exp1, exp2) => F.AppExp(doExp env exp1, doExp env exp2)
                   | F.IfThenElseExp(exp1, exp2, exp3) => F.IfThenElseExp(doExp env exp1, doExp env exp2, doExp env exp3)
                   | F.FnExp(vid, ty, exp) => F.FnExp(vid, ty, doExp env exp) (* TODO: modify environment *)
                   | F.ProjectionExp _ => exp0
                   | F.TyAbsExp(tv, exp) => F.TyAbsExp(tv, doExp env exp) (* TODO: update type environment? *)
                   | F.TyAppExp(exp, ty) => F.TyAppExp(exp, ty)
                   | F.RecordEqualityExp fields => F.RecordEqualityExp(List.map (fn (label, e) => (label, doExp env e)) fields)
                   | F.DataTagExp _ => raise Fail "DataTagExp should not occur here"
                   | F.DataPayloadExp _ => raise Fail "DataPayloadExp should not occur here"
                   | F.CaseExp(exp, ty, matches) =>
                     let val examinedVId = freshVId(ctx, "exp")
                         val examinedExp = F.VarExp(Syntax.MkQualified([], examinedVId))
                         fun go [] = F.AppExp(F.VarExp(Syntax.MkQualified([], InitialEnv.VId_raise)), F.RecordExp []) (* TODO: raise Match or Bind *)
                           | go ((pat, innerExp) :: rest)
                             = let val binders = genBinders env examinedExp pat
                               in if isExhaustive env pat then
                                      if List.null rest then
                                          List.foldr F.LetExp (doExp env innerExp) binders
                                      else
                                          raise Fail "A redundant pattern match found"
                                  else
                                      let val matcher = genMatcher env examinedExp ty pat
                                      in F.IfThenElseExp(matcher, List.foldr F.LetExp (doExp env innerExp) binders, go rest) (* TODO: modify environment? *)
                                      end
                               end
                     in F.LetExp(F.SimpleBind(examinedVId, ty, doExp env exp), go matches)
                     end
                )
          and doValBind env (F.SimpleBind (v, ty, exp)) = F.SimpleBind (v, ty, doExp env exp)
            | doValBind env (F.TupleBind (vars, exp)) = F.TupleBind (vars, doExp env exp)
          and genMatcher env exp _ F.WildcardPat = F.VarExp(Syntax.MkQualified([], InitialEnv.VId_true)) (* always match *)
            | genMatcher env exp ty (F.SConPat(scon as Syntax.IntegerConstant _)) = F.AppExp(F.VarExp(Syntax.MkQualified([], InitialEnv.VId_EQUAL_int)), F.TupleExp [exp, F.SConExp scon])
            | genMatcher env exp ty (F.SConPat(scon as Syntax.WordConstant _)) = F.AppExp(F.VarExp(Syntax.MkQualified([], InitialEnv.VId_EQUAL_word)), F.TupleExp [exp, F.SConExp scon])
            | genMatcher env exp ty (F.SConPat(scon as Syntax.StringConstant _)) = F.AppExp(F.VarExp(Syntax.MkQualified([], InitialEnv.VId_EQUAL_string)), F.TupleExp [exp, F.SConExp scon])
            | genMatcher env exp ty (F.SConPat(scon as Syntax.CharacterConstant _)) = F.AppExp(F.VarExp(Syntax.MkQualified([], InitialEnv.VId_EQUAL_char)), F.TupleExp [exp, F.SConExp scon])
            | genMatcher env exp ty (F.SConPat(Syntax.RealConstant _)) = raise Fail "genMatcher: cannot match a real constant"
            | genMatcher env exp ty (F.VarPat _) = F.VarExp(Syntax.MkQualified([], InitialEnv.VId_true)) (* always match *)
            | genMatcher env exp (recordTy as F.RecordType fieldTypes) (F.RecordPat (fields, _))
              = List.foldr (fn ((label, pat), e) =>
                               case List.find (fn (label', _) => label = label') fieldTypes of
                                   SOME (_, fieldTy) => F.SimplifyingAndalsoExp(genMatcher env (F.AppExp (F.ProjectionExp { label = label, recordTy = recordTy, fieldTy = fieldTy }, exp)) fieldTy pat, e)
                                 | NONE => raise Fail "internal error: record field not found"
                           )
                           (F.VarExp(Syntax.MkQualified([], InitialEnv.VId_true)))
                           fields
            | genMatcher env exp _ (F.RecordPat (fields, _)) = raise Fail "internal error: record pattern against non-record type"
            (* TODO: true and false *)
            | genMatcher env exp ty (F.InstantiatedConPat (longvid as Syntax.MkQualified(_, USyntax.MkVId(name, _)), SOME innerPat, tyargs))
              = let val payloadTy = F.RecordType [] (* not implemented yet... *)
                in F.SimplifyingAndalsoExp(F.AppExp(F.VarExp(Syntax.MkQualified([], InitialEnv.VId_EQUAL_string)), F.TupleExp [F.DataTagExp exp, F.SConExp (Syntax.StringConstant name)]),
                                           genMatcher env (F.DataPayloadExp exp) payloadTy innerPat)
                end
            | genMatcher env exp ty (F.InstantiatedConPat (longvid as Syntax.MkQualified(_, USyntax.MkVId(name, _)), NONE, tyargs))
              = F.AppExp(F.VarExp(Syntax.MkQualified([], InitialEnv.VId_EQUAL_string)), F.TupleExp [F.DataTagExp exp, F.SConExp (Syntax.StringConstant name)])
            | genMatcher env exp ty0 (F.LayeredPat (vid, ty1, innerPat)) = genMatcher env exp ty0 innerPat
          and genBinders env exp F.WildcardPat = [] : F.ValBind list
            | genBinders env exp (F.SConPat _) = []
            | genBinders env exp (F.VarPat (vid, ty)) = [F.SimpleBind (vid, ty, exp)]
            | genBinders env exp (F.RecordPat (fields, _)) = List.concat (List.map (fn (label, innerPat) => genBinders env (F.AppExp (F.ProjectionExp { label = label, recordTy = F.RecordType [], fieldTy = F.RecordType [] }, exp)) innerPat) fields)
            | genBinders env exp (F.InstantiatedConPat(longvid, SOME innerPat, tyargs)) = genBinders env (F.DataPayloadExp exp) innerPat
            | genBinders env exp (F.InstantiatedConPat(longvid, NONE, tyargs)) = []
            | genBinders env exp (F.LayeredPat(vid, ty, pat)) = F.SimpleBind (vid, ty, exp) :: genBinders env exp pat
          and isExhaustive env F.WildcardPat = true
            | isExhaustive env (F.SConPat _) = false
            | isExhaustive env (F.VarPat _) = true
            | isExhaustive env (F.RecordPat (row, _)) = List.all (fn (_, e) => isExhaustive env e) row
            | isExhaustive env (F.InstantiatedConPat (longvid, pat, _)) = false (* TODO *)
            | isExhaustive env (F.LayeredPat (_, _, innerPat)) = isExhaustive env innerPat
          fun doDec env (F.ValDec valbind) = F.ValDec (doValBind env valbind)
            | doDec env (F.RecValDec valbinds) = F.RecValDec (List.map (doValBind env) valbinds)
      in { doExp = doExp
         , doValBind = doValBind
         , doDec = doDec
         }
      end
end (* structure FTransform *)
