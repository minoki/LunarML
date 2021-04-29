(*
 * Copyright (c) 2021 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure FTransform = struct
structure F = FSyntax
type Context = { nextVId : int ref
               }
fun freshVId(ctx : Context, name: string) = let val n = !(#nextVId ctx)
                                            in #nextVId ctx := n + 1
                                             ; USyntax.MkVId(name, n)
                                            end
datatype Env' = MkEnv of Env
withtype Env = { strMap : Env' Syntax.StrIdMap.map
               , dataConMap : FSyntax.Ty USyntax.VIdMap.map
               , exnConMap : USyntax.VId USyntax.VIdMap.map (* exception constructor -> exception tag *)
               }
val emptyEnv : Env = { strMap = Syntax.StrIdMap.empty, dataConMap = USyntax.VIdMap.empty, exnConMap = USyntax.VIdMap.empty }
(* true, false, nil, ::, ref *)
val initialEnv : Env = { strMap = Syntax.StrIdMap.empty
                       , dataConMap = let open InitialEnv
                                          val tyVarA = USyntax.AnonymousTyVar(0)
                                          val primTyCon_list = Typing.primTyCon_list
                                          val primTyCon_ref = Typing.primTyCon_ref
                                      in List.foldl USyntax.VIdMap.insert' USyntax.VIdMap.empty
                                                    [(VId_true, FSyntax.TyCon([], Typing.primTyCon_bool))
                                                    ,(VId_false, FSyntax.TyCon([], Typing.primTyCon_bool))
                                                    ,(VId_nil, FSyntax.ForallType(tyVarA, FSyntax.TyCon([FSyntax.TyVar(tyVarA)], primTyCon_list)))
                                                    ,(VId_DCOLON, FSyntax.ForallType(tyVarA, FSyntax.FnType(FSyntax.PairType(FSyntax.TyVar(tyVarA), FSyntax.TyCon([FSyntax.TyVar(tyVarA)], primTyCon_list)), FSyntax.TyCon([FSyntax.TyVar(tyVarA)], primTyCon_list))))
                                                    ,(VId_ref, FSyntax.ForallType(tyVarA, FSyntax.FnType(FSyntax.TyVar(tyVarA), FSyntax.TyCon([FSyntax.TyVar(tyVarA)], primTyCon_ref))))
                                                    ,(VId_Match, FSyntax.TyCon([], Typing.primTyCon_exn))
                                                    ,(VId_Bind, FSyntax.TyCon([], Typing.primTyCon_exn))
                                                    ,(VId_Div, FSyntax.TyCon([], Typing.primTyCon_exn))
                                                    ,(VId_Overflow, FSyntax.TyCon([], Typing.primTyCon_exn))
                                                    ,(VId_Size, FSyntax.TyCon([], Typing.primTyCon_exn))
                                                    ,(VId_Subscript, FSyntax.TyCon([], Typing.primTyCon_exn))
                                                    ,(VId_Fail, FSyntax.FnType(FSyntax.TyCon([], Typing.primTyCon_string), FSyntax.TyCon([], Typing.primTyCon_exn)))
                                                    ]
                                      end
                       , exnConMap = let open InitialEnv
                                     in List.foldl USyntax.VIdMap.insert' USyntax.VIdMap.empty
                                                   [(VId_Match, VId_Match_tag)
                                                   ,(VId_Bind, VId_Bind_tag)
                                                   ,(VId_Div, VId_Div_tag)
                                                   ,(VId_Overflow, VId_Overflow_tag)
                                                   ,(VId_Size, VId_Size_tag)
                                                   ,(VId_Subscript, VId_Subscript_tag)
                                                   ,(VId_Fail, VId_Fail_tag)
                                                   ]
                                     end
                       }
fun addTopDecs (ctx : Context) : Env -> USyntax.TopDec list -> Env = List.foldl (addTopDec ctx)
and addTopDec (ctx : Context) (USyntax.TypeDec _, env) = env
  | addTopDec ctx (USyntax.DatatypeDec(span, datbinds), env) = List.foldl (addDatBind ctx) env datbinds (* TODO: equality *)
  | addTopDec ctx (USyntax.DatatypeRepDec(span, _, _), env) = env
  | addTopDec ctx (USyntax.AbstypeDec(span, _, _), env) = env
  | addTopDec ctx (USyntax.ExceptionDec(span, _), env) = env (* not implemented yet *)
and addDatBind ctx (USyntax.DatBind(span, tyvars, tycon, conbinds), { strMap, dataConMap, exnConMap })
    = let fun quantify [] ty = ty
            | quantify (tv :: tyvars) ty = F.ForallType(tv, quantify tyvars ty)
          fun doConBind(USyntax.ConBind(span, vid, NONE), dataConMap)
              = let val typeScheme = quantify tyvars (F.TyCon(List.map FSyntax.TyVar tyvars, Syntax.MkQualified([], tycon)))
                in USyntax.VIdMap.insert(dataConMap, vid, typeScheme)
                end
            | doConBind(USyntax.ConBind(span, vid, SOME payloadTy), dataConMap)
              = let val typeScheme = quantify tyvars (F.FnType(ToFSyntax.toFTy(ctx, ToFSyntax.emptyEnv, payloadTy), F.TyCon(List.map FSyntax.TyVar tyvars, Syntax.MkQualified([], tycon))))
                in USyntax.VIdMap.insert(dataConMap, vid, typeScheme)
                end
      in { strMap = strMap
         , dataConMap = List.foldl doConBind dataConMap conbinds
         , exnConMap = exnConMap
         }
      end
fun getPayloadTy ([], FSyntax.FnType(payloadTy, _)) = payloadTy
  | getPayloadTy (ty :: tys, FSyntax.ForallType(tv, rest)) = getPayloadTy (tys, FSyntax.substituteTy (tv, ty) rest)
  | getPayloadTy _ = raise Fail "getPayloadTy: invalid"
fun desugarPatternMatches (ctx: Context): { doExp: Env -> F.Exp -> F.Exp, doValBind: Env -> F.ValBind -> F.ValBind, doDec : Env -> F.Dec -> F.Dec }
    = let fun doExp (env: Env) exp0
              = (case exp0 of
                     F.SConExp scon => exp0
                   | F.VarExp longvid => exp0
                   | F.RecordExp fields => F.RecordExp (List.map (fn (label, e) => (label, doExp env e)) fields)
                   | F.LetExp (valbind, exp) => F.LetExp (doValBind env valbind, doExp env exp) (* TODO: modify environment *)
                   | F.LetRecExp (valbinds, exp) => F.LetRecExp (List.map (doValBind env) valbinds, doExp env exp)
                   | F.AppExp(exp1, exp2) => F.AppExp(doExp env exp1, doExp env exp2)
                   | F.HandleExp{body, exnName, handler} => F.HandleExp { body = doExp env body
                                                                        , exnName = exnName
                                                                        , handler = doExp ( (* TODO *) env) handler
                                                                        }
                   | F.RaiseExp(span, exp) => F.RaiseExp(span, doExp env exp)
                   | F.IfThenElseExp(exp1, exp2, exp3) => F.IfThenElseExp(doExp env exp1, doExp env exp2, doExp env exp3)
                   | F.FnExp(vid, ty, exp) => F.FnExp(vid, ty, doExp env exp) (* TODO: modify environment *)
                   | F.ProjectionExp _ => exp0
                   | F.TyAbsExp(tv, exp) => F.TyAbsExp(tv, doExp env exp) (* TODO: update type environment? *)
                   | F.TyAppExp(exp, ty) => F.TyAppExp(exp, ty)
                   | F.RecordEqualityExp fields => F.RecordEqualityExp(List.map (fn (label, e) => (label, doExp env e)) fields)
                   | F.DataTagExp _ => raise Fail "DataTagExp should not occur here"
                   | F.DataPayloadExp _ => raise Fail "DataPayloadExp should not occur here"
                   | F.CaseExp(span, exp, ty, matches) =>
                     let val examinedVId = freshVId(ctx, "exp")
                         val examinedExp = F.VarExp(Syntax.MkQualified([], examinedVId))
                         fun go [] = F.RaiseExp(span, F.VarExp(Syntax.MkQualified([], InitialEnv.VId_Match)))
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
                                 | NONE => raise Fail ("internal error: record field not found (fieldTypes=" ^ FSyntax.PrettyPrint.print_Ty recordTy ^ ", " ^ Syntax.PrettyPrint.print_Label label ^ ")")
                           )
                           (F.VarExp(Syntax.MkQualified([], InitialEnv.VId_true)))
                           fields
            | genMatcher env exp _ (F.RecordPat (fields, _)) = raise Fail "internal error: record pattern against non-record type"
            | genMatcher (env as { dataConMap, exnConMap, ... }) exp ty (F.InstantiatedConPat (longvid as Syntax.MkQualified(_, vid as USyntax.MkVId(name, _)), SOME innerPat, tyargs))
              = (case USyntax.VIdMap.find(dataConMap, vid) of
                     SOME dataConTy => let val payloadTy = getPayloadTy(tyargs, dataConTy)
                                       in if ty = FSyntax.TyCon([], Typing.primTyCon_exn) then
                                              case USyntax.VIdMap.find(exnConMap, vid) of
                                                  SOME exntag => F.SimplifyingAndalsoExp(F.AppExp(F.VarExp(Syntax.MkQualified([], InitialEnv.VId_EQUAL_exntag)), F.TupleExp [F.DataTagExp exp, F.VarExp(Syntax.MkQualified([], exntag))]),
                                                                                         genMatcher env (F.DataPayloadExp exp) payloadTy innerPat)
                                                | NONE => raise Fail ("internal error: exception constructor not found (" ^ USyntax.PrettyPrint.print_LongVId longvid ^ ")")
                                          else
                                              F.SimplifyingAndalsoExp(F.AppExp(F.VarExp(Syntax.MkQualified([], InitialEnv.VId_EQUAL_string)), F.TupleExp [F.DataTagExp exp, F.SConExp (Syntax.StringConstant name)]),
                                                                      genMatcher env (F.DataPayloadExp exp) payloadTy innerPat)
                                       end
                  | NONE => raise Fail ("internal error: data constructor not found (" ^ USyntax.PrettyPrint.print_LongVId longvid ^ ")")
                )
            | genMatcher (env as { exnConMap, ... }) exp ty (F.InstantiatedConPat (longvid as Syntax.MkQualified(_, vid as USyntax.MkVId(name, _)), NONE, tyargs))
              = if USyntax.eqVId(vid, InitialEnv.VId_true) then
                    exp
                else if USyntax.eqVId(vid, InitialEnv.VId_false) then
                    F.AppExp(F.VarExp(Syntax.MkQualified([], InitialEnv.VId_not)), exp)
                else if ty = FSyntax.TyCon([], Typing.primTyCon_exn) then
                    case USyntax.VIdMap.find(exnConMap, vid) of
                        SOME exntag => F.AppExp(F.VarExp(Syntax.MkQualified([], InitialEnv.VId_EQUAL_exntag)), F.TupleExp [F.DataTagExp exp, F.VarExp(Syntax.MkQualified([], exntag))])
                      | NONE => raise Fail ("internal error: exception constructor not found (" ^ USyntax.PrettyPrint.print_LongVId longvid ^ ")")
                else
                    F.AppExp(F.VarExp(Syntax.MkQualified([], InitialEnv.VId_EQUAL_string)), F.TupleExp [F.DataTagExp exp, F.SConExp (Syntax.StringConstant name)])
            | genMatcher env exp ty0 (F.LayeredPat (vid, ty1, innerPat)) = genMatcher env exp ty0 innerPat
          and genBinders env exp F.WildcardPat = [] : F.ValBind list
            | genBinders env exp (F.SConPat _) = []
            | genBinders env exp (F.VarPat (vid, ty)) = [F.SimpleBind (vid, ty, exp)]
            | genBinders env exp (F.RecordPat (fields, _)) = List.concat (List.map (fn (label, innerPat) => genBinders env (F.AppExp (F.ProjectionExp { label = label, recordTy = F.RecordType [], fieldTy = F.RecordType [] }, exp)) innerPat) fields)
            | genBinders env exp (F.InstantiatedConPat(longvid, SOME innerPat, tyargs)) = if USyntax.eqULongVId(longvid, Syntax.MkQualified([], InitialEnv.VId_ref)) then
                                                                                              case tyargs of
                                                                                                  [tyarg] => genBinders env (F.AppExp(F.TyAppExp(F.VarExp(Syntax.MkQualified([], InitialEnv.VId_EXCLAM)), tyarg), exp)) innerPat
                                                                                                | _ => raise Fail "invalid type arguments to 'ref'"
                                                                                          else
                                                                                              genBinders env (F.DataPayloadExp exp) innerPat
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
