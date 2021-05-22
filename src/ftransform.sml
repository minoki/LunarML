(*
 * Copyright (c) 2021 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure DesugarPatternMatches = struct
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
(* Check if the pattern is exhaustive and binds no variable *)
fun isWildcardPat F.WildcardPat = true
  | isWildcardPat (F.SConPat _) = false
  | isWildcardPat (F.VarPat _) = false
  | isWildcardPat (F.RecordPat (fields, _)) = List.all (fn (label, pat) => isWildcardPat pat) fields
  | isWildcardPat (F.ConPat (longvid, optPat, tyargs)) = false (* TODO *)
  | isWildcardPat (F.LayeredPat _) = false
fun getPayloadTy ([], FSyntax.FnType(payloadTy, _)) = payloadTy
  | getPayloadTy (ty :: tys, FSyntax.ForallType(tv, rest)) = getPayloadTy (tys, FSyntax.substituteTy (tv, ty) rest)
  | getPayloadTy _ = raise Fail "getPayloadTy: invalid"
fun desugarPatternMatches (ctx: Context): { doExp: Env -> F.Exp -> F.Exp, doValBind: Env -> F.ValBind -> F.ValBind, doDec : Env -> F.Dec -> Env * F.Dec, doDecs : Env -> F.Dec list -> Env * F.Dec list }
    = let fun doExp (env: Env) exp0
              = (case exp0 of
                     F.SConExp scon => exp0
                   | F.VarExp longvid => exp0
                   | F.RecordExp fields => F.RecordExp (List.map (fn (label, e) => (label, doExp env e)) fields)
                   | F.LetExp (dec, exp) => let val (env', dec') = doDec env dec
                                            in F.LetExp (dec', doExp env' exp)
                                            end
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
                   | F.ListExp(xs, ty) => F.ListExp(Vector.map (doExp env) xs, ty)
                   | F.VectorExp(xs, ty) => F.VectorExp(Vector.map (doExp env) xs, ty)
                   | F.CaseExp(span, exp, ty, [(F.VarPat (vid, ty'), exp2 as F.VarExp (Syntax.MkQualified ([], vid')))]) =>
                                              if USyntax.eqVId(vid, vid') then
                                                  doExp env exp
                                              else
                                                  F.LetExp(F.ValDec(F.SimpleBind(vid, ty', doExp env exp)), exp2)
                   | F.CaseExp(span, exp, ty, matches) =>
                     let val canIgnore = case matches of
                                             [(pat, innerExp)] => if isWildcardPat pat then SOME innerExp else NONE
                                           | _ => NONE
                     in case canIgnore of
                            SOME innerExp => F.LetExp(F.IgnoreDec(doExp env exp), doExp env innerExp)
                          | NONE => let val examinedVId = freshVId(ctx, "exp")
                                        val examinedExp = F.VarExp(Syntax.MkQualified([], examinedVId))
                                        fun go [] = F.RaiseExp(span, F.VarExp(Syntax.MkQualified([], InitialEnv.VId_Match)))
                                          | go ((pat, innerExp) :: rest)
                                            = let val binders = genBinders env examinedExp pat
                                              in if isExhaustive env pat then
                                                     if List.null rest then
                                                         List.foldr (fn (valbind, exp) => F.LetExp(F.ValDec(valbind), exp)) (doExp env innerExp) binders
                                                     else
                                                         raise Fail "A redundant pattern match found"
                                                 else
                                                     let val matcher = genMatcher env examinedExp ty pat
                                                     in F.IfThenElseExp(matcher, List.foldr (fn (valbind, exp) => F.LetExp(F.ValDec(valbind), exp)) (doExp env innerExp) binders, go rest) (* TODO: modify environment? *)
                                                     end
                                              end
                                    in F.LetExp(F.ValDec(F.SimpleBind(examinedVId, ty, doExp env exp)), go matches)
                                    end
                     end
                )
          and doDec env (F.ValDec valbind) = (env, F.ValDec (doValBind env valbind))
            | doDec env (F.RecValDec valbinds) = (env, F.RecValDec (List.map (doValBind env) valbinds))
            | doDec env (F.IgnoreDec exp) = (env, F.IgnoreDec (doExp env exp))
            | doDec env (dec as F.DatatypeDec datbinds) = (List.foldl doDatBind env datbinds, dec) (* TODO: equality *)
            | doDec env (dec as F.ExceptionDec { conName, tagName, payloadTy })
              = let val exnTy = FSyntax.TyCon([], Typing.primTyCon_exn)
                    val env' = { strMap = #strMap env
                               , dataConMap = USyntax.VIdMap.insert (#dataConMap env, conName, case payloadTy of
                                                                                                   NONE => exnTy
                                                                                                 | SOME ty => FSyntax.FnType(ty, exnTy))
                               , exnConMap = USyntax.VIdMap.insert (#exnConMap env, conName, tagName)
                               }
                in (env', dec)
                end
          and doValBind env (F.SimpleBind (v, ty, exp)) = F.SimpleBind (v, ty, doExp env exp)
            | doValBind env (F.TupleBind (vars, exp)) = F.TupleBind (vars, doExp env exp)
          and doDatBind (F.DatBind (tyvars, tycon, conbinds), { strMap, dataConMap, exnConMap })
              = let fun quantify [] ty = ty
                      | quantify (tv :: tyvars) ty = F.ForallType(tv, quantify tyvars ty)
                    fun doConBind (F.ConBind (vid, NONE), dataConMap)
                        = let val typeScheme = quantify tyvars (F.TyCon(List.map FSyntax.TyVar tyvars, tycon))
                          in USyntax.VIdMap.insert(dataConMap, vid, typeScheme)
                          end
                      | doConBind(F.ConBind (vid, SOME payloadTy), dataConMap)
                        = let val typeScheme = quantify tyvars (F.FnType(payloadTy, F.TyCon(List.map FSyntax.TyVar tyvars, tycon)))
                          in USyntax.VIdMap.insert(dataConMap, vid, typeScheme)
                          end
                in { strMap = strMap
                   , dataConMap = List.foldl doConBind dataConMap conbinds
                   , exnConMap = exnConMap
                   }
                end
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
            | genMatcher (env as { dataConMap, exnConMap, ... }) exp ty (F.ConPat (longvid as Syntax.MkQualified(_, vid as USyntax.MkVId(name, _)), SOME innerPat, tyargs))
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
            | genMatcher (env as { exnConMap, ... }) exp ty (F.ConPat (longvid as Syntax.MkQualified(_, vid as USyntax.MkVId(name, _)), NONE, tyargs))
              = if USyntax.eqVId(vid, InitialEnv.VId_true) then
                    exp
                else if USyntax.eqVId(vid, InitialEnv.VId_false) then
                    F.AppExp(F.VarExp(Syntax.MkQualified([], InitialEnv.VId_Bool_not)), exp)
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
            | genBinders env exp (F.ConPat(longvid, SOME innerPat, tyargs)) = if USyntax.eqULongVId(longvid, Syntax.MkQualified([], InitialEnv.VId_ref)) then
                                                                                  case tyargs of
                                                                                      [tyarg] => genBinders env (F.AppExp(F.TyAppExp(F.VarExp(Syntax.MkQualified([], InitialEnv.VId_EXCLAM)), tyarg), exp)) innerPat
                                                                                    | _ => raise Fail "invalid type arguments to 'ref'"
                                                                              else
                                                                                  genBinders env (F.DataPayloadExp exp) innerPat
            | genBinders env exp (F.ConPat(longvid, NONE, tyargs)) = []
            | genBinders env exp (F.LayeredPat(vid, ty, pat)) = F.SimpleBind (vid, ty, exp) :: genBinders env exp pat
          and isExhaustive env F.WildcardPat = true
            | isExhaustive env (F.SConPat _) = false
            | isExhaustive env (F.VarPat _) = true
            | isExhaustive env (F.RecordPat (row, _)) = List.all (fn (_, e) => isExhaustive env e) row
            | isExhaustive env (F.ConPat (longvid, pat, _)) = false (* TODO *)
            | isExhaustive env (F.LayeredPat (_, _, innerPat)) = isExhaustive env innerPat
          fun doDecs env [] = (env, [])
            | doDecs env (dec :: decs) = let val (env', dec') = doDec env dec
                                             val (env'', decs') = doDecs env' decs
                                         in (env'', dec' :: decs')
                                         end
      in { doExp = doExp
         , doValBind = doValBind
         , doDec = doDec
         , doDecs = doDecs
         }
      end
end (* structure DesugarPatternMatches *)

structure EliminateVariables = struct
local structure F = FSyntax in
type Context = { nextVId : int ref }
type Env = { vidMap : F.Exp USyntax.VIdMap.map }
val emptyEnv : Env = { vidMap = USyntax.VIdMap.empty }
fun freeVarsInPat F.WildcardPat = USyntax.VIdSet.empty
  | freeVarsInPat (F.SConPat _) = USyntax.VIdSet.empty
  | freeVarsInPat (F.VarPat (vid, _)) = USyntax.VIdSet.singleton vid
  | freeVarsInPat (F.RecordPat (fields, wildcard)) = List.foldl (fn ((_, pat), acc) => USyntax.VIdSet.union (freeVarsInPat pat, acc)) USyntax.VIdSet.empty fields
  | freeVarsInPat (F.ConPat (longvid, optPat, tyargs)) = (case optPat of
                                                              NONE => USyntax.VIdSet.empty
                                                            | SOME pat => freeVarsInPat pat
                                                         )
  | freeVarsInPat (F.LayeredPat (vid, ty, pat)) = USyntax.VIdSet.add (freeVarsInPat pat, vid)
fun removeFromEnv (vid, env as { vidMap } : Env) = if USyntax.VIdMap.inDomain (vidMap, vid) then
                                                       { vidMap = #1 (USyntax.VIdMap.remove (vidMap, vid)) }
                                                   else
                                                       env
(* Should we inline an expression? *)
fun isSimpleExp (F.VarExp _) = true
  | isSimpleExp (F.TyAbsExp (tv, exp)) = isSimpleExp exp
  | isSimpleExp (F.TyAppExp (exp, ty)) = isSimpleExp exp
  | isSimpleExp _ = false
fun eliminateVariables (ctx : Context) : { doExp : Env -> F.Exp -> F.Exp
                                         , doDec : Env -> F.Dec -> (* modified environment *) Env * F.Dec option
                                         , doDecs : Env -> F.Dec list -> (* modified environment *) Env * F.Dec list
                                         }
    = let fun doExp (env : Env) exp0
              = (case exp0 of
                     F.SConExp _ => exp0
                   | F.VarExp (Syntax.MkQualified (_, vid)) => (case USyntax.VIdMap.find (#vidMap env, vid) of
                                                                    NONE => exp0
                                                                  | SOME exp => exp
                                                               )
                   | F.RecordExp fields => F.RecordExp (List.map (fn (label, exp) => (label, doExp env exp)) fields)
                   | F.LetExp (dec, exp) => (case doDec env dec of
                                                 (env', NONE) => doExp env' exp
                                               | (env', SOME dec') => F.LetExp (dec', doExp env' exp)
                                            )
                   | F.AppExp (exp1, exp2) => F.AppExp (doExp env exp1, doExp env exp2)
                   | F.HandleExp { body, exnName, handler } => F.HandleExp { body = doExp env body
                                                                           , exnName = exnName
                                                                           , handler = let val env' = removeFromEnv (exnName, env)
                                                                                       in doExp env' handler
                                                                                       end
                                                                           }
                   | F.RaiseExp (span, exp) => F.RaiseExp (span, doExp env exp)
                   | F.IfThenElseExp (exp1, exp2, exp3) => F.IfThenElseExp (doExp env exp1, doExp env exp2, doExp env exp3)
                   | F.CaseExp (span, exp, ty, matches) => let fun doMatch (pat, exp) = let val vars = freeVarsInPat pat
                                                                                            val env' = { vidMap = USyntax.VIdMap.filteri (fn (vid, _) => not (USyntax.VIdSet.member (vars, vid))) (#vidMap env) }
                                                                                        in (pat, doExp env' exp)
                                                                                        end
                                                           in F.CaseExp (span, doExp env exp, ty, List.map doMatch matches)
                                                           end
                   | F.FnExp (vid, ty, exp) => let val env' = removeFromEnv (vid, env)
                                               in F.FnExp (vid, ty, doExp env' exp)
                                               end
                   | F.ProjectionExp { label, recordTy, fieldTy } => exp0
                   | F.ListExp (xs, ty) => F.ListExp (Vector.map (doExp env) xs, ty)
                   | F.VectorExp (xs, ty) => F.VectorExp (Vector.map (doExp env) xs, ty)
                   | F.TyAbsExp (tyvar, exp) => F.TyAbsExp (tyvar, doExp env exp)
                   | F.TyAppExp (exp, ty) => F.TyAppExp (doExp env exp, ty)
                   | F.RecordEqualityExp fields => F.RecordEqualityExp (List.map (fn (label, exp) => (label, doExp env exp)) fields)
                   | F.DataTagExp exp => F.DataTagExp (doExp env exp)
                   | F.DataPayloadExp exp => F.DataPayloadExp (doExp env exp)
                )
          and doDec (env : Env) (F.ValDec valbind) = (case doValBind env valbind of
                                                          (env', NONE) => (env', NONE)
                                                        | (env', SOME valbind') => (env', SOME (F.ValDec valbind'))
                                                     )
            | doDec env (F.RecValDec valbinds) = let fun go (env, acc, []) = (env, SOME (F.RecValDec (List.rev acc)))
                                                       | go (env, acc, valbind :: valbinds) = let val (env', optBind) = doValBind env valbind
                                                                                                  val acc' = case optBind of
                                                                                                                 NONE => acc 
                                                                                                               | SOME bind => bind :: acc
                                                                                              in go (env', acc', valbinds)
                                                                                              end
                                                 in go (env, [], valbinds)
                                                 end
            | doDec env (F.IgnoreDec exp) = (env, SOME (F.IgnoreDec (doExp env exp)))
            | doDec env (dec as F.DatatypeDec datbinds) = (env, SOME dec) (* TODO *)
            | doDec env (dec as F.ExceptionDec _) = (env, SOME dec) (* TODO *)
          and doValBind env (F.SimpleBind (vid, ty, exp)) = let val exp' = doExp env exp
                                                            in if isSimpleExp exp' then
                                                                   ({ vidMap = USyntax.VIdMap.insert (#vidMap env, vid, exp') }, NONE)
                                                               else
                                                                   (removeFromEnv (vid, env), SOME (F.SimpleBind (vid, ty, exp')))
                                                            end
            | doValBind env (F.TupleBind (binds, exp)) = let val vars = List.map #1 binds
                                                             val env' = List.foldl removeFromEnv env vars
                                                         in (env', SOME (F.TupleBind (binds, doExp env exp)))
                                                         end
          fun doDecs env [] = (env, [])
            | doDecs env (dec :: decs) = (case doDec env dec of
                                              (env', NONE) => doDecs env' decs
                                            | (env', SOME dec') => let val (env'', decs') = doDecs env' decs
                                                                   in (env'', dec' :: decs')
                                                                   end
                                         )
      in { doExp = doExp
         , doDec = doDec
         , doDecs = doDecs
         }
      end
end (* local *)
end (* structure EliminateVariables *)

structure Fuse = struct
local structure F = FSyntax in
type Context = { nextVId : int ref }
type Env = {}
val emptyEnv : Env = {}
fun fuse (ctx : Context) : { doExp : Env -> F.Exp -> F.Exp
                           , doDec : Env -> F.Dec -> (* modified environment *) Env * F.Dec
                           , doDecs : Env -> F.Dec list -> (* modified environment *) Env * F.Dec list
                           }
    = let fun doExp (env : Env) exp0
              = (case exp0 of
                     F.SConExp _ => exp0
                   | F.VarExp _ => exp0
                   | F.RecordExp fields => F.RecordExp (List.map (fn (label, exp) => (label, doExp env exp)) fields)
                   | F.LetExp (dec, exp) => let val (env', dec') = doDec env dec
                                            in F.LetExp (dec', doExp env' exp)
                                            end
                   | F.AppExp (exp1, exp2) => (case (doExp env exp1, doExp env exp2) of
                                                   (exp1 as F.TyAppExp (F.VarExp (Syntax.MkQualified (_, vid)), ty1), exp2 as F.ListExp (xs, ty2)) =>
                                                   if USyntax.eqVId(vid, InitialEnv.VId_Vector_fromList) then
                                                       F.VectorExp (xs, ty2)
                                                   else
                                                       F.AppExp (exp1, exp2)
                                                 | (exp1, exp2) => F.AppExp (exp1, exp2)
                                              )
                   | F.HandleExp { body, exnName, handler } => F.HandleExp { body = doExp env body
                                                                           , exnName = exnName
                                                                           , handler = doExp env handler
                                                                           }
                   | F.RaiseExp (span, exp) => F.RaiseExp (span, doExp env exp)
                   | F.IfThenElseExp (exp1, exp2, exp3) => F.IfThenElseExp (doExp env exp1, doExp env exp2, doExp env exp3)
                   | F.CaseExp (span, exp, ty, matches) => let fun doMatch (pat, exp) = (pat, doExp env exp)
                                                           in F.CaseExp (span, doExp env exp, ty, List.map doMatch matches)
                                                           end
                   | F.FnExp (vid, ty, exp) => F.FnExp (vid, ty, doExp env exp)
                   | F.ProjectionExp { label, recordTy, fieldTy } => exp0
                   | F.ListExp (xs, ty) => F.ListExp (Vector.map (doExp env) xs, ty)
                   | F.VectorExp (xs, ty) => F.VectorExp (Vector.map (doExp env) xs, ty)
                   | F.TyAbsExp (tyvar, exp) => F.TyAbsExp (tyvar, doExp env exp)
                   | F.TyAppExp (exp, ty) => (case doExp env exp of
                                                  F.TyAbsExp(tyvar, exp') => let val substExp = #doExp (F.substTy (USyntax.TyVarMap.singleton (tyvar, ty)))
                                                                             in substExp exp'
                                                                             end
                                                | exp => F.TyAppExp (doExp env exp, ty)
                                             )
                   | F.RecordEqualityExp fields => F.RecordEqualityExp (List.map (fn (label, exp) => (label, doExp env exp)) fields)
                   | F.DataTagExp exp => F.DataTagExp (doExp env exp)
                   | F.DataPayloadExp exp => F.DataPayloadExp (doExp env exp)
                )
          and doDec (env : Env) (F.ValDec valbind) = (env, F.ValDec (doValBind env valbind))
            | doDec env (F.RecValDec valbinds) = (env, F.RecValDec (List.map (doValBind env) valbinds))
            | doDec env (F.IgnoreDec exp) = (env, F.IgnoreDec (doExp env exp))
            | doDec env (dec as F.DatatypeDec datbinds) = (env, dec)
            | doDec env (dec as F.ExceptionDec datbinds) = (env, dec)
          and doValBind env (F.SimpleBind (v, ty, exp)) = F.SimpleBind (v, ty, doExp env exp)
            | doValBind env (F.TupleBind (vars, exp)) = F.TupleBind (vars, doExp env exp)
          fun doDecs env [] = (env, [])
            | doDecs env (dec :: decs) = let val (env', dec') = doDec env dec
                                             val (env'', decs') = doDecs env' decs
                                         in (env'', dec' :: decs')
                                         end
      in { doExp = doExp
         , doDec = doDec
         , doDecs = doDecs
         }
      end
end (* local *)
end (* structure Fuse *)

structure FTransform = struct
type Env = { desugarPatternMatches : DesugarPatternMatches.Env
           , eliminateVariables : EliminateVariables.Env
           , fuse : Fuse.Env
           }
val initialEnv : Env = { desugarPatternMatches = DesugarPatternMatches.initialEnv
                       , eliminateVariables = EliminateVariables.emptyEnv
                       , fuse = Fuse.emptyEnv
                       }
fun doDecs ctx (env : Env) decs = let val (dpEnv, decs) = #doDecs (DesugarPatternMatches.desugarPatternMatches ctx) (#desugarPatternMatches env) decs
                                      val (evEnv, decs) = #doDecs (EliminateVariables.eliminateVariables ctx) (#eliminateVariables env) decs
                                      val (fuseEnv, decs) = #doDecs (Fuse.fuse ctx) (#fuse env) decs
                              in ({desugarPatternMatches = dpEnv, eliminateVariables = evEnv, fuse = fuseEnv}, decs)
                              end
end (* structure FTransform *)

structure DeadCodeElimination = struct
structure F = FSyntax
fun isDiscardable (F.SConExp _) = true
  | isDiscardable (F.VarExp _) = true
  | isDiscardable (F.RecordExp fields) = List.all (fn (label, exp) => isDiscardable exp) fields
  | isDiscardable (F.LetExp (dec, exp)) = false (* TODO *)
  | isDiscardable (F.AppExp (exp1, exp2)) = false (* TODO *)
  | isDiscardable (F.HandleExp { body, exnName, handler }) = false (* TODO *)
  | isDiscardable (F.RaiseExp (span, exp)) = false
  | isDiscardable (F.IfThenElseExp (exp1, exp2, exp3)) = isDiscardable exp1 andalso isDiscardable exp2 andalso isDiscardable exp3
  | isDiscardable (F.CaseExp (span, exp, ty, matches)) = false (* TODO *)
  | isDiscardable (F.FnExp (vid, ty, exp)) = true
  | isDiscardable (F.ProjectionExp { label, recordTy, fieldTy }) = true
  | isDiscardable (F.ListExp (xs, ty)) = Vector.all isDiscardable xs
  | isDiscardable (F.VectorExp (xs, ty)) = Vector.all isDiscardable xs
  | isDiscardable (F.TyAbsExp (tyvar, exp)) = isDiscardable exp
  | isDiscardable (F.TyAppExp (exp, ty)) = isDiscardable exp
  | isDiscardable (F.RecordEqualityExp fields) = List.all (fn (label, exp) => isDiscardable exp) fields
  | isDiscardable (F.DataTagExp exp) = isDiscardable exp
  | isDiscardable (F.DataPayloadExp exp) = isDiscardable exp
(* doPat : F.Pat -> (* constructors used *) USyntax.VIdSet.set *)
fun doPat F.WildcardPat = USyntax.VIdSet.empty
  | doPat (F.SConPat _) = USyntax.VIdSet.empty
  | doPat (F.VarPat _) = USyntax.VIdSet.empty
  | doPat (F.RecordPat (fields, wildcard)) = List.foldl (fn ((label, pat), acc) => USyntax.VIdSet.union (acc, doPat pat)) USyntax.VIdSet.empty fields
  | doPat (F.ConPat (Syntax.MkQualified (_, vid), NONE, tyargs)) = USyntax.VIdSet.singleton vid
  | doPat (F.ConPat (Syntax.MkQualified (_, vid), SOME innerPat, tyargs)) = USyntax.VIdSet.add (doPat innerPat, vid)
  | doPat (F.LayeredPat (vid, ty, innerPat)) = doPat innerPat
(* doExp : F.Exp -> USyntax.VIdSet.set * F.Exp *)
fun doExp (exp as F.SConExp _ : F.Exp) : USyntax.VIdSet.set * F.Exp = (USyntax.VIdSet.empty, exp)
  | doExp (exp as F.VarExp (Syntax.MkQualified (_, vid))) = (USyntax.VIdSet.singleton vid, exp)
  | doExp (F.RecordExp fields) = let val fields = List.map (fn (label, exp) => (label, doExp exp)) fields
                                 in (List.foldl USyntax.VIdSet.union USyntax.VIdSet.empty (List.map (#1 o #2) fields), F.RecordExp (List.map (fn (label, (_, exp)) => (label, exp)) fields))
                                 end
  | doExp (F.LetExp (dec, exp)) = let val (used, exp) = doExp exp
                                      val (used', decs) = doDec (used, dec)
                                  in (used', List.foldr F.LetExp exp decs)
                                  end
  | doExp (F.AppExp (exp1, exp2)) = let val (used, exp1) = doExp exp1
                                        val (used', exp2) = doExp exp2
                                    in (USyntax.VIdSet.union (used, used'), F.AppExp (exp1, exp2))
                                    end
  | doExp (F.HandleExp { body, exnName, handler }) = let val (used, body) = doExp body
                                                         val (used', handler) = doExp handler
                                                     in (USyntax.VIdSet.union (used, USyntax.VIdSet.subtract (used', exnName)), F.HandleExp { body = body, exnName = exnName, handler = handler })
                                                     end
  | doExp (F.RaiseExp (span, exp)) = let val (used, exp) = doExp exp
                                     in (used, F.RaiseExp (span, exp))
                                     end
  | doExp (F.IfThenElseExp (exp1, exp2, exp3)) = let val (used1, exp1) = doExp exp1
                                                     val (used2, exp2) = doExp exp2
                                                     val (used3, exp3) = doExp exp3
                                                 in (USyntax.VIdSet.union (used1, USyntax.VIdSet.union (used2, used3)), F.IfThenElseExp (exp1, exp2, exp3))
                                                 end
  | doExp (F.CaseExp (span, exp, ty, matches)) = let val (used, exp) = doExp exp
                                                     val (used, matches) = List.foldr (fn ((pat, exp), (used, matches)) => let val (used', exp) = doExp exp
                                                                                                                           in (USyntax.VIdSet.union (USyntax.VIdSet.union (used, used'), doPat pat), (pat, exp) :: matches)
                                                                                                                           end)
                                                                                      (used, []) matches
                                                 in (used, F.CaseExp (span, exp, ty, matches))
                                                 end
  | doExp (F.FnExp (vid, ty, exp)) = let val (used, exp) = doExp exp
                                     in (used, F.FnExp (vid, ty, exp))
                                     end
  | doExp (exp as F.ProjectionExp _) = (USyntax.VIdSet.empty, exp)
  | doExp (F.ListExp (xs, ty)) = let val xs' = Vector.map doExp xs
                                 in (Vector.foldl USyntax.VIdSet.union USyntax.VIdSet.empty (Vector.map #1 xs'), F.ListExp (Vector.map #2 xs', ty))
                                 end
  | doExp (F.VectorExp (xs, ty)) = let val xs' = Vector.map doExp xs
                                   in (Vector.foldl USyntax.VIdSet.union USyntax.VIdSet.empty (Vector.map #1 xs'), F.VectorExp (Vector.map #2 xs', ty))
                                   end
  | doExp (F.TyAbsExp (tyvar, exp)) = let val (used, exp) = doExp exp
                                      in (used, F.TyAbsExp (tyvar, exp))
                                      end
  | doExp (F.TyAppExp (exp, ty)) = let val (used, exp) = doExp exp
                                   in (used, F.TyAppExp (exp, ty))
                                   end
  | doExp (F.RecordEqualityExp fields) = let val fields' = List.map (fn (label, exp) => (label, doExp exp)) fields
                                         in (List.foldl (fn ((_, (used, _)), acc) => USyntax.VIdSet.union (used, acc)) USyntax.VIdSet.empty fields', F.RecordEqualityExp (List.foldr (fn ((label, (_, exp)), acc) => (label, exp) :: acc) [] fields'))
                                         end
  | doExp (F.DataTagExp exp) = let val (used, exp) = doExp exp
                               in (used, F.DataTagExp exp)
                               end
  | doExp (F.DataPayloadExp exp) = let val (used, exp) = doExp exp
                                   in (used, F.DataPayloadExp exp)
                                   end
and doIgnoredExpAsExp exp = let val (used, exps) = doIgnoredExp exp
                            in (used, List.foldr (fn (e1, e2) => F.LetExp (F.IgnoreDec e1, e2)) (F.RecordExp []) exps)
                            end
(* doIgnoredExp : F.Exp -> USyntax.VIdSet.set * F.Exp list *)
and doIgnoredExp (F.SConExp _) = (USyntax.VIdSet.empty, [])
  | doIgnoredExp (F.VarExp _) = (USyntax.VIdSet.empty, [])
  | doIgnoredExp (F.RecordExp fields) = let val fields' = List.map (fn (label, exp) => doIgnoredExp exp) fields
                                        in (List.foldl (fn ((used, _), acc) => USyntax.VIdSet.union (used, acc)) USyntax.VIdSet.empty fields', List.foldr (fn ((_, exp), exps) => exp @ exps) [] fields')
                                        end
  | doIgnoredExp (F.LetExp (dec, exp)) = let val (used, exp) = doIgnoredExpAsExp exp
                                             val (used, decs) = doDec (used, dec)
                                         in case List.foldr F.LetExp exp decs of
                                                F.RecordExp [] => (used, [])
                                              | exp => (used, [exp])
                                         end
  | doIgnoredExp (F.AppExp (exp1, exp2)) = let val (used1, exp1) = doExp exp1
                                               val (used2, exp2) = doExp exp2
                                           in (USyntax.VIdSet.union (used1, used2), [F.AppExp (exp1, exp2)])
                                           end
  | doIgnoredExp (F.HandleExp { body, exnName, handler }) = let val (used1, body) = doIgnoredExpAsExp body
                                                                val (used2, handler) = doIgnoredExpAsExp handler
                                                            in case body of
                                                                   F.RecordExp [] => (used1, [])
                                                                 | _ => (USyntax.VIdSet.union (used1, USyntax.VIdSet.subtract (used2, exnName)), [F.HandleExp { body = body, exnName = exnName, handler = handler }])
                                                            end
  | doIgnoredExp (exp as F.RaiseExp _) = let val (used, exp) = doExp exp
                                         in (used, [exp])
                                         end
  | doIgnoredExp (F.IfThenElseExp (exp1, exp2, exp3)) = let val (used2, exp2) = doIgnoredExpAsExp exp2
                                                            val (used3, exp3) = doIgnoredExpAsExp exp3
                                                        in case (exp2, exp3) of
                                                               (F.RecordExp [], F.RecordExp []) => doIgnoredExp exp1
                                                             | (exp2, exp3) => let val (used1, exp1) = doExp exp1
                                                                               in (USyntax.VIdSet.union (used1, USyntax.VIdSet.union (used2, used3)), [F.IfThenElseExp (exp1, exp2, exp3)])
                                                                               end
                                                        end
  | doIgnoredExp (F.CaseExp (span, exp, ty, matches)) = let val (used, exp) = doExp exp
                                                            val (used, matches) = List.foldr (fn ((pat, exp), (used, matches)) => let val (used', exp) = doIgnoredExpAsExp exp
                                                                                                                                      val used'' = doPat pat
                                                                                                                                  in (USyntax.VIdSet.union (used, USyntax.VIdSet.union (used', used'')), (pat, exp) :: matches)
                                                                                                                                  end)
                                                                                             (used, []) matches
                                                        in (used, [F.CaseExp (span, exp, ty, matches)])
                                                        end
  | doIgnoredExp (F.FnExp _) = (USyntax.VIdSet.empty, [])
  | doIgnoredExp (F.ProjectionExp _) = (USyntax.VIdSet.empty, [])
  | doIgnoredExp (F.ListExp (xs, ty)) = let val xs' = Vector.map doIgnoredExp xs
                                        in (Vector.foldl (fn ((used, _), acc) => USyntax.VIdSet.union (used, acc)) USyntax.VIdSet.empty xs', Vector.foldr (fn ((_, e), xs) => e @ xs) [] xs')
                                        end
  | doIgnoredExp (F.VectorExp (xs, ty)) = let val xs' = Vector.map doIgnoredExp xs
                                        in (Vector.foldl (fn ((used, _), acc) => USyntax.VIdSet.union (used, acc)) USyntax.VIdSet.empty xs', Vector.foldr (fn ((_, e), xs) => e @ xs) [] xs')
                                        end
  | doIgnoredExp (F.TyAbsExp (tyvar, exp)) = let val (used, exp) = doIgnoredExpAsExp exp (* should be pure *)
                                             in case exp of
                                                    F.RecordExp [] => (used, [])
                                                  | exp => (used, [F.TyAbsExp (tyvar, exp)])
                                             end
  | doIgnoredExp (F.TyAppExp (exp, ty)) = let val (used, exp) = doIgnoredExpAsExp exp
                                          in case exp of
                                                 F.RecordExp [] => (used, [])
                                               | exp => (used, [F.TyAppExp (exp, ty)])
                                          end
  | doIgnoredExp (F.RecordEqualityExp fields) = let val fields' = List.map (fn (label, exp) => doIgnoredExp exp) fields
                                                in (List.foldl (fn ((used, _), acc) => USyntax.VIdSet.union (used, acc)) USyntax.VIdSet.empty fields', List.foldr (fn ((_, e), xs) => e @ xs) [] fields')
                                                end
  | doIgnoredExp (F.DataTagExp exp) = doIgnoredExp exp
  | doIgnoredExp (F.DataPayloadExp exp) = doIgnoredExp exp
(* doDec : USyntax.VIdSet.set * F.Dec -> USyntax.VIdSet.set * F.Dec *)
and doDec (used : USyntax.VIdSet.set, F.ValDec (F.SimpleBind (vid, ty, exp))) : USyntax.VIdSet.set * F.Dec list
    = if not (USyntax.VIdSet.member (used, vid)) then
          if isDiscardable exp then
              (used, [])
          else
              let val (used', exps) = doIgnoredExp exp
              in (USyntax.VIdSet.union (used, used'), List.map F.IgnoreDec exps)
              end
      else
          let val (used', exp') = doExp exp
          in (USyntax.VIdSet.union (used, used'), [F.ValDec (F.SimpleBind (vid, ty, exp'))])
          end
  | doDec (used, F.ValDec (F.TupleBind (binds, exp)))
    = let val bound = USyntax.VIdSet.fromList (List.map #1 binds)
      in if USyntax.VIdSet.disjoint (used, bound) then
             if isDiscardable exp then
                 (used, [])
             else
                 let val (used', exps) = doIgnoredExp exp
                 in (USyntax.VIdSet.union (used, used'), List.map F.IgnoreDec exps)
                 end
         else
             let val (used', exp) = doExp exp
             in (USyntax.VIdSet.union (used, used'), [F.ValDec (F.TupleBind (binds, exp))])
             end
      end
  | doDec (used, F.RecValDec valbinds)
    = let val bound = List.foldl USyntax.VIdSet.union USyntax.VIdSet.empty
                                 (List.map (fn F.SimpleBind (vid, _, _) => USyntax.VIdSet.singleton vid
                                           | F.TupleBind (binds, _) => USyntax.VIdSet.fromList (List.map #1 binds)
                                           ) valbinds)
      in if USyntax.VIdSet.disjoint (used, bound) then
             (used, []) (* RHS should be fn _ => _, and therefore discardable *)
         else
             let val (used, valbinds) = List.foldr (fn (F.SimpleBind (vid, ty, exp), (used, valbinds)) => let val (used', exp) = doExp exp
                                                                                                          in (USyntax.VIdSet.union (used, used'), F.SimpleBind (vid, ty, exp) :: valbinds)
                                                                                                          end
                                                   | (F.TupleBind (binds, exp), (used, valbinds)) => let val (used', exp) = doExp exp
                                                                                                     in (USyntax.VIdSet.union (used, used'), F.TupleBind (binds, exp) :: valbinds)
                                                                                                     end
                                                   ) (used, []) valbinds
             in (used, [F.RecValDec valbinds])
             end
      end
  | doDec (used, F.IgnoreDec exp) = let val (used', exps) = doIgnoredExp exp
                                    in (USyntax.VIdSet.union (used, used'), List.map F.IgnoreDec exps)
                                    end
  | doDec (used, dec as F.DatatypeDec datbinds) = (used, [dec]) (* TODO *)
  | doDec (used, dec as F.ExceptionDec { conName, tagName, payloadTy }) = if USyntax.VIdSet.member (used, conName) orelse USyntax.VIdSet.member (used, tagName) then
                                                                              (used, [dec])
                                                                          else
                                                                              (used, [])
(* doDecs : USyntax.VIdSet.set * F.Dec list -> USyntax.VIdSet.set * F.Dec list *)
fun doDecs (used, decs) = List.foldr (fn (dec, (used, decs)) => let val (used, dec) = doDec (used, dec)
                                                                in (used, dec @ decs)
                                                                end) (used, []) decs
end (* structure DeadCodeElimination *)
