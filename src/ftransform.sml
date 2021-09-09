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
type Env = { valMap : FSyntax.Ty USyntax.VIdMap.map
           , exnTagMap : FSyntax.Path USyntax.VIdMap.map (* exception constructor -> exception tag *)
           }
fun addVar(env : Env, vid, ty) = { valMap = USyntax.VIdMap.insert(#valMap env, vid, ty)
                                 , exnTagMap = #exnTagMap env
                                 }
val emptyEnv : Env = { valMap = USyntax.VIdMap.empty
                     , exnTagMap = USyntax.VIdMap.empty
                     }
(* true, false, nil, ::, ref *)
val initialEnv : Env = { valMap = let open InitialEnv
                                      val tyVarA = USyntax.AnonymousTyVar(0)
                                      val primTyName_list = Typing.primTyName_list
                                      val primTyName_ref = Typing.primTyName_ref
                                  in List.foldl USyntax.VIdMap.insert' USyntax.VIdMap.empty
                                                [(VId_true, FSyntax.TyCon([], Typing.primTyName_bool))
                                                ,(VId_false, FSyntax.TyCon([], Typing.primTyName_bool))
                                                ,(VId_nil, FSyntax.ForallType(tyVarA, F.TypeKind, FSyntax.TyCon([FSyntax.TyVar(tyVarA)], primTyName_list)))
                                                ,(VId_DCOLON, FSyntax.ForallType(tyVarA, F.TypeKind, FSyntax.FnType(FSyntax.PairType(FSyntax.TyVar(tyVarA), FSyntax.TyCon([FSyntax.TyVar(tyVarA)], primTyName_list)), FSyntax.TyCon([FSyntax.TyVar(tyVarA)], primTyName_list))))
                                                ,(VId_ref, FSyntax.ForallType(tyVarA, F.TypeKind, FSyntax.FnType(FSyntax.TyVar(tyVarA), FSyntax.TyCon([FSyntax.TyVar(tyVarA)], primTyName_ref))))
                                                ,(VId_Match, FSyntax.TyCon([], Typing.primTyName_exn))
                                                ,(VId_Bind, FSyntax.TyCon([], Typing.primTyName_exn))
                                                ,(VId_Div, FSyntax.TyCon([], Typing.primTyName_exn))
                                                ,(VId_Overflow, FSyntax.TyCon([], Typing.primTyName_exn))
                                                ,(VId_Size, FSyntax.TyCon([], Typing.primTyName_exn))
                                                ,(VId_Subscript, FSyntax.TyCon([], Typing.primTyName_exn))
                                                ,(VId_Fail, FSyntax.FnType(FSyntax.TyCon([], Typing.primTyName_string), FSyntax.TyCon([], Typing.primTyName_exn)))
                                                ]
                                  end
                       , exnTagMap = let open InitialEnv
                                     in List.foldl (fn ((con, tag), m) => USyntax.VIdMap.insert(m, con, ToFSyntax.LongVIdToPath tag)) USyntax.VIdMap.empty
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
  | isWildcardPat (F.VectorPat (pats, ellipsis, _)) = ellipsis andalso Vector.length pats = 0
fun getPayloadTy ([], FSyntax.FnType(payloadTy, _)) = payloadTy
  | getPayloadTy (ty :: tys, FSyntax.ForallType(tv, F.TypeKind, rest)) = getPayloadTy (tys, FSyntax.substituteTy (tv, ty) rest)
  | getPayloadTy _ = raise Fail "getPayloadTy: invalid"
fun isExnType (F.TyVar tv) = tv = F.tyNameToTyVar Typing.primTyName_exn
  | isExnType _ = false
fun splitPath (components, F.Child(parent, label)) = splitPath (label :: components, parent)
  | splitPath (components, F.Root vid) = (vid, components)
fun lookupPath ({ valMap, ... } : Env, path) = let val (vid, components) = splitPath ([], path)
                                               in case USyntax.VIdMap.find(valMap, vid) of
                                                      SOME ty => List.foldl (fn (label, F.SigType { valMap, strMap, equalityMap, ... }) =>
                                                                                (case label of
                                                                                     F.ValueLabel vid => (case Syntax.VIdMap.find(valMap, vid) of
                                                                                                              SOME ty => ty
                                                                                                            | NONE => raise Fail ("child not found: " ^ Syntax.print_VId vid)
                                                                                                         )
                                                                                   | F.StructLabel strid => (case Syntax.StrIdMap.find(strMap, strid) of
                                                                                                                 SOME ty => ty
                                                                                                               | NONE => raise Fail ("child not found: " ^ Syntax.print_StrId strid)
                                                                                                            )
                                                                                   | F.ExnTagLabel vid => (case Syntax.VIdMap.find(valMap, vid) of (* ??? *)
                                                                                                              SOME ty => ty
                                                                                                            | NONE => raise Fail ("child not found: " ^ Syntax.print_VId vid)
                                                                                                          )
                                                                                   | F.EqualityLabel tycon => (case Syntax.TyConMap.find(equalityMap, tycon) of
                                                                                                                   SOME ty => ty
                                                                                                                 | NONE => raise Fail ("child not found: " ^ Syntax.print_TyCon tycon)
                                                                                                              )
                                                                                )
                                                                            | (label, _) => raise Fail "lookupPath: invalid type"
                                                                            ) ty components
                                                    | NONE => raise Fail ("root not found: " ^ USyntax.print_VId vid)
                                               end
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
                                                                        , handler = let val env = addVar(env, exnName, F.TyCon([], Typing.primTyName_exn))
                                                                                    in doExp env handler
                                                                                    end
                                                                        }
                   | F.RaiseExp(span, exp) => F.RaiseExp(span, doExp env exp)
                   | F.IfThenElseExp(exp1, exp2, exp3) => F.IfThenElseExp(doExp env exp1, doExp env exp2, doExp env exp3)
                   | F.FnExp(vid, ty, exp) => let val env = addVar(env, vid, ty)
                                              in F.FnExp(vid, ty, doExp env exp)
                                              end
                   | F.ProjectionExp _ => exp0
                   | F.TyAbsExp(tv, kind, exp) => F.TyAbsExp(tv, kind, doExp env exp) (* TODO: update type environment? *)
                   | F.TyAppExp(exp, ty) => F.TyAppExp(exp, ty)
                   | F.RecordEqualityExp fields => F.RecordEqualityExp(List.map (fn (label, e) => (label, doExp env e)) fields)
                   | F.DataTagExp _ => raise Fail "DataTagExp should not occur here"
                   | F.DataPayloadExp _ => raise Fail "DataPayloadExp should not occur here"
                   | F.ListExp(xs, ty) => F.ListExp(Vector.map (doExp env) xs, ty)
                   | F.VectorExp(xs, ty) => F.VectorExp(Vector.map (doExp env) xs, ty)
                   | F.StructExp { valMap, strMap, exnTagMap, equalityMap } => F.StructExp { valMap = valMap
                                                                                           , strMap = strMap
                                                                                           , exnTagMap = exnTagMap
                                                                                           , equalityMap = equalityMap
                                                                                           }
                   | F.SProjectionExp (exp, label) => F.SProjectionExp (doExp env exp, label)
                   | F.CaseExp(span, exp, ty, [(F.VarPat (vid, ty'), exp2 as F.VarExp (vid'))]) =>
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
                                        val examinedExp = F.VarExp(examinedVId)
                                        val env = addVar(env, examinedVId, ty)
                                        fun go [] = F.RaiseExp(span, F.VarExp(InitialEnv.VId_Match))
                                          | go ((pat, innerExp) :: rest)
                                            = let val binders = genBinders env examinedExp pat
                                                  val (env', matcher) = genMatcher env examinedExp ty pat
                                              in if isExhaustive env pat then
                                                     if List.null rest then
                                                         List.foldr (fn (valbind, exp) => F.LetExp(F.ValDec(valbind), exp)) (doExp env' innerExp) binders
                                                     else
                                                         raise Fail "A redundant pattern match found"
                                                 else
                                                     F.IfThenElseExp(matcher, List.foldr (fn (valbind, exp) => F.LetExp(F.ValDec(valbind), exp)) (doExp env' innerExp) binders, go rest) (* TODO: modify environment? *)
                                              end
                                    in F.LetExp(F.ValDec(F.SimpleBind(examinedVId, ty, doExp env exp)), go matches)
                                    end
                     end
                )
          and doDec env (F.ValDec valbind) = let val env' = case valbind of
                                                                F.SimpleBind (v, ty, _) => addVar(env, v, ty)
                                                              | F.TupleBind (vars, _) => List.foldl (fn ((v, ty), env) => addVar(env, v, ty)) env vars
                                             in (env', F.ValDec (doValBind env valbind))
                                             end
            | doDec env (F.RecValDec valbinds) = let val env = List.foldl (fn ((v, ty, _), env) => addVar(env, v, ty)) env valbinds
                                                 in (env, F.RecValDec (List.map (fn (v, ty, exp) => (v, ty, doExp env exp)) valbinds))
                                                 end
            | doDec env (F.IgnoreDec exp) = (env, F.IgnoreDec (doExp env exp))
            | doDec env (dec as F.DatatypeDec datbinds) = (List.foldl doDatBind env datbinds, dec) (* TODO: equality *)
            | doDec env (dec as F.ExceptionDec { conName, tagName, payloadTy })
              = let val exnTy = FSyntax.TyCon([], Typing.primTyName_exn)
                    val conTy = case payloadTy of
                                    NONE => exnTy
                                  | SOME ty => F.FnType(ty, exnTy)
                    val env' = { valMap = USyntax.VIdMap.insert (#valMap env, conName, conTy)
                               , exnTagMap = USyntax.VIdMap.insert (#exnTagMap env, conName, F.Root(tagName))
                               }
                in (env', dec)
                end
            | doDec env (dec as F.ExceptionRepDec { conName, conPath, tagPath, payloadTy })
              = let val exnTy = FSyntax.TyCon([], Typing.primTyName_exn)
                    val conTy = case payloadTy of
                                    NONE => exnTy
                                  | SOME ty => FSyntax.FnType(ty, exnTy)
                    val env' = { valMap = USyntax.VIdMap.insert (#valMap env, conName, conTy)
                               , exnTagMap = USyntax.VIdMap.insert (#exnTagMap env, conName, tagPath)
                               }
                in (env', F.ValDec(F.SimpleBind(conName, conTy, F.PathToExp(conPath))))
                end
            | doDec env (F.ExportValue exp) = (env, F.ExportValue (doExp env exp))
            | doDec env (F.ExportModule fields) = (env, F.ExportModule (Vector.map (fn (label, exp) => (label, doExp env exp)) fields))
            | doDec env (F.GroupDec (v, decs)) = let val (env, decs) = doDecs env decs
                                                 in (env, F.GroupDec (v, decs))
                                                 end
          and doValBind env (F.SimpleBind (v, ty, exp)) = F.SimpleBind (v, ty, doExp env exp)
            | doValBind env (F.TupleBind (vars, exp)) = F.TupleBind (vars, doExp env exp)
          and doDatBind (F.DatBind (tyvars, tycon, conbinds), { valMap, exnTagMap })
              = let fun doConBind(F.ConBind (vid, payloadTy), valMap)
                        = let val ty = F.TyCon(List.map FSyntax.TyVar tyvars, tycon)
                              val ty = case payloadTy of
                                           NONE => ty
                                         | SOME payloadTy => F.FnType(payloadTy, ty)
                              val ty = List.foldr (fn (tv, ty) => F.ForallType(tv, F.TypeKind, ty)) ty tyvars
                          in USyntax.VIdMap.insert(valMap, vid, ty)
                          end
                in { valMap = List.foldl doConBind valMap conbinds
                   , exnTagMap = exnTagMap
                   }
                end
          and genMatcher (env : Env) exp _ F.WildcardPat : Env * F.Exp = (env, F.VarExp(InitialEnv.VId_true)) (* always match *)
            | genMatcher env exp ty (F.SConPat(scon as Syntax.IntegerConstant _)) = (env, F.AppExp(F.LongVarExp(InitialEnv.VId_EQUAL_int), F.TupleExp [exp, F.SConExp scon]))
            | genMatcher env exp ty (F.SConPat(scon as Syntax.WordConstant _)) = (env, F.AppExp(F.LongVarExp(InitialEnv.VId_EQUAL_word), F.TupleExp [exp, F.SConExp scon]))
            | genMatcher env exp ty (F.SConPat(scon as Syntax.StringConstant _)) = (env, F.AppExp(F.LongVarExp(InitialEnv.VId_EQUAL_string), F.TupleExp [exp, F.SConExp scon]))
            | genMatcher env exp ty (F.SConPat(scon as Syntax.CharacterConstant _)) = (env, F.AppExp(F.LongVarExp(InitialEnv.VId_EQUAL_char), F.TupleExp [exp, F.SConExp scon]))
            | genMatcher env exp ty (F.SConPat(Syntax.RealConstant _)) = raise Fail "genMatcher: cannot match a real constant"
            | genMatcher env exp ty (F.VarPat(vid, _)) = (addVar(env, vid, ty), F.VarExp(InitialEnv.VId_true)) (* always match *)
            | genMatcher env exp (recordTy as F.RecordType fieldTypes) (F.RecordPat (fields, _))
              = List.foldr (fn ((label, pat), (env, e)) =>
                               case List.find (fn (label', _) => label = label') fieldTypes of
                                   SOME (_, fieldTy) => let val (env, exp) = genMatcher env (F.AppExp (F.ProjectionExp { label = label, recordTy = recordTy, fieldTy = fieldTy }, exp)) fieldTy pat
                                                        in (env, F.SimplifyingAndalsoExp(exp, e))
                                                        end
                                 | NONE => raise Fail ("internal error: record field not found (fieldTypes=" ^ FSyntax.PrettyPrint.print_Ty recordTy ^ ", " ^ Syntax.PrettyPrint.print_Label label ^ ")")
                           )
                           (env, F.VarExp(InitialEnv.VId_true))
                           fields
            | genMatcher env exp _ (F.RecordPat (fields, _)) = raise Fail "internal error: record pattern against non-record type"
            | genMatcher (env as { exnTagMap, ... }) exp ty (F.ConPat (path, SOME innerPat, tyargs))
              = let val conTy = lookupPath(env, path)
                    val payloadTy = getPayloadTy(tyargs, conTy)
                in if isExnType ty then
                       let val tag = case path of
                                         F.Root vid => (case USyntax.VIdMap.find(exnTagMap, vid) of
                                                            SOME path => F.PathToExp path
                                                          | NONE => raise Fail ("internal error: exception constructor not found (" ^ FSyntax.PrettyPrint.print_Path path ^ ")")
                                                       )
                                       | F.Child (parent, F.ValueLabel vid) => F.PathToExp (F.Child (parent, F.ExnTagLabel vid))
                                       | _ => raise Fail ("internal error: invalid exception constructor (" ^ FSyntax.PrettyPrint.print_Path path ^ ")")
                           val (env, payload) = genMatcher env (F.DataPayloadExp exp) payloadTy innerPat
                       in (env, F.SimplifyingAndalsoExp(F.AppExp(F.LongVarExp(InitialEnv.VId_exn_instanceof), F.TupleExp [exp, tag]), payload))
                       end
                   else
                       let val tag = case path of
                                         F.Root (USyntax.MkVId (name, _)) => name
                                       | F.Child (parent, F.ValueLabel vid) => Syntax.getVIdName vid
                                       | _ => raise Fail ("internal error: invalid value constructor (" ^ FSyntax.PrettyPrint.print_Path path ^ ")")
                           val (env, payload) = genMatcher env (F.DataPayloadExp exp) payloadTy innerPat
                       in (env, F.SimplifyingAndalsoExp(F.AppExp(F.LongVarExp(InitialEnv.VId_EQUAL_string), F.TupleExp [F.DataTagExp exp, F.SConExp (Syntax.StringConstant tag)]), payload))
                       end
                end
            | genMatcher (env as { exnTagMap, ... }) exp ty (F.ConPat (path, NONE, tyargs))
              = if (case path of F.Root vid => USyntax.eqVId(vid, InitialEnv.VId_true) | _ => false) then
                    (env, exp)
                else if (case path of F.Root vid => USyntax.eqVId(vid, InitialEnv.VId_false) | _ => false) then
                    (env, F.AppExp(F.LongVarExp(InitialEnv.VId_Bool_not), exp))
                else if isExnType ty then
                    let val tag = case path of
                                      F.Root vid => (case USyntax.VIdMap.find(exnTagMap, vid) of
                                                         SOME path => F.PathToExp path
                                                       | NONE => raise Fail ("internal error: exception constructor not found (" ^ FSyntax.PrettyPrint.print_Path path ^ ")")
                                                    )
                                    | F.Child (parent, F.ValueLabel vid) => F.PathToExp (F.Child (parent, F.ExnTagLabel vid))
                                    | _ => raise Fail ("internal error: invalid exception constructor (" ^ FSyntax.PrettyPrint.print_Path path ^ ")")
                    in (env, F.AppExp(F.LongVarExp(InitialEnv.VId_exn_instanceof), F.TupleExp [exp, tag]))
                    end
                else
                    let val tag = case path of
                                      F.Root (USyntax.MkVId (name, _)) => name
                                    | F.Child (parent, F.ValueLabel vid) => Syntax.getVIdName vid
                                    | _ => raise Fail ("internal error: invalid value constructor (" ^ FSyntax.PrettyPrint.print_Path path ^ ")")
                    in (env, F.AppExp(F.LongVarExp(InitialEnv.VId_EQUAL_string), F.TupleExp [F.DataTagExp exp, F.SConExp (Syntax.StringConstant tag)]))
                    end
            | genMatcher env exp ty0 (F.LayeredPat (vid, ty1, innerPat)) = let val env = addVar(env, vid, ty1)
                                                                           in genMatcher env exp ty0 innerPat
                                                                           end
            | genMatcher env exp ty0 (F.VectorPat (pats, ellipsis, elemTy)) = let val e0 = F.AppExp(if ellipsis then F.LongVarExp(InitialEnv.VId_Int_GE) else F.LongVarExp(InitialEnv.VId_EQUAL_int), F.TupleExp [F.AppExp(F.TyAppExp(F.LongVarExp(InitialEnv.VId_Vector_length), elemTy), exp), F.SConExp (Syntax.IntegerConstant (Vector.length pats))])
                                                                              in Vector.foldri (fn (i, pat, (env, e)) => let val (env, exp) = genMatcher env (F.AppExp(F.TyAppExp(F.LongVarExp(InitialEnv.VId_Vector_sub), elemTy), F.TupleExp [exp, F.SConExp (Syntax.IntegerConstant i)])) elemTy pat
                                                                                                                         in (env, F.SimplifyingAndalsoExp(e, exp))
                                                                                                                         end
                                                                                               ) (env, e0) pats
                                                                              end
          and genBinders env exp F.WildcardPat = [] : F.ValBind list
            | genBinders env exp (F.SConPat _) = []
            | genBinders env exp (F.VarPat (vid, ty)) = [F.SimpleBind (vid, ty, exp)]
            | genBinders env exp (F.RecordPat (fields, _)) = List.concat (List.map (fn (label, innerPat) => genBinders env (F.AppExp (F.ProjectionExp { label = label, recordTy = F.RecordType [], fieldTy = F.RecordType [] }, exp)) innerPat) fields)
            | genBinders env exp (F.ConPat(path, SOME innerPat, tyargs)) = if (case path of F.Root vid => USyntax.eqVId(vid, InitialEnv.VId_ref) | _ => false) then
                                                                               case tyargs of
                                                                                   [tyarg] => genBinders env (F.AppExp(F.TyAppExp(F.LongVarExp(InitialEnv.VId_EXCLAM), tyarg), exp)) innerPat
                                                                                 | _ => raise Fail "invalid type arguments to 'ref'"
                                                                           else
                                                                               genBinders env (F.DataPayloadExp exp) innerPat
            | genBinders env exp (F.ConPat(path, NONE, tyargs)) = []
            | genBinders env exp (F.LayeredPat(vid, ty, pat)) = F.SimpleBind (vid, ty, exp) :: genBinders env exp pat
            | genBinders env exp (F.VectorPat(pats, ellipsis, elemTy)) = Vector.foldri (fn (i, pat, acc) => genBinders env (F.AppExp(F.TyAppExp(F.LongVarExp(InitialEnv.VId_Vector_sub), elemTy), F.TupleExp [exp, F.SConExp (Syntax.IntegerConstant i)])) pat @ acc) [] pats
          and isExhaustive env F.WildcardPat = true
            | isExhaustive env (F.SConPat _) = false
            | isExhaustive env (F.VarPat _) = true
            | isExhaustive env (F.RecordPat (row, _)) = List.all (fn (_, e) => isExhaustive env e) row
            | isExhaustive env (F.ConPat (longvid, pat, _)) = false (* TODO *)
            | isExhaustive env (F.LayeredPat (_, _, innerPat)) = isExhaustive env innerPat
            | isExhaustive env (F.VectorPat (pats, ellipsis, elemTy)) = ellipsis andalso Vector.length pats = 0
          and doDecs env [] = (env, [])
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
datatype InlineExp = Path of F.Path
                   | StructExp of { valMap : F.Path Syntax.VIdMap.map
                                  , strMap : F.Path Syntax.StrIdMap.map
                                  , exnTagMap : F.Path Syntax.VIdMap.map
                                  , equalityMap : F.Path Syntax.TyConMap.map
                                  }
                   | FnExp of USyntax.VId * F.Ty * F.Exp
                   | TyAbsExp of F.TyVar * F.Kind * InlineExp
                   | TyAppExp of InlineExp * F.Ty
fun lookupSLabel ({ valMap, strMap, exnTagMap, equalityMap }, label) = case label of
                                                                           F.ValueLabel vid => Syntax.VIdMap.find(valMap, vid)
                                                                         | F.StructLabel strid => Syntax.StrIdMap.find(strMap, strid)
                                                                         | F.ExnTagLabel vid => Syntax.VIdMap.find(exnTagMap, vid)
                                                                         | F.EqualityLabel tycon => Syntax.TyConMap.find(equalityMap, tycon)
type Env = { vidMap : InlineExp USyntax.VIdMap.map }
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
  | freeVarsInPat (F.VectorPat (pats, ellipsis, elemTy)) = Vector.foldl (fn (pat, acc) => USyntax.VIdSet.union (freeVarsInPat pat, acc)) USyntax.VIdSet.empty pats
fun removeFromEnv (vid, env as { vidMap } : Env) = if USyntax.VIdMap.inDomain (vidMap, vid) then
                                                       { vidMap = #1 (USyntax.VIdMap.remove (vidMap, vid)) }
                                                   else
                                                       env
fun costOfExp (F.SConExp _) = 1
  | costOfExp (F.VarExp _) = 0
  | costOfExp (F.RecordExp fields) = List.foldl (fn ((label, exp), acc) => acc + costOfExp exp) 1 fields
  | costOfExp (F.LetExp (dec, exp)) = costOfDec dec + costOfExp exp
  | costOfExp (F.AppExp (e1, e2)) = costOfExp e1 + costOfExp e2
  | costOfExp (F.HandleExp { body, exnName, handler }) = 5 + costOfExp body + costOfExp handler
  | costOfExp (F.RaiseExp (span, exp)) = 1 + costOfExp exp
  | costOfExp (F.IfThenElseExp (e1, e2, e3)) = 1 + costOfExp e1 + costOfExp e2 + costOfExp e3
  | costOfExp (F.CaseExp (span, exp, ty, matches)) = List.foldl (fn ((pat, exp), acc) => acc + costOfExp exp) (costOfExp exp) matches
  | costOfExp (F.FnExp (vid, ty, exp)) = 1 + costOfExp exp
  | costOfExp (F.ProjectionExp { label, recordTy, fieldTy }) = 1
  | costOfExp (F.ListExp (exps, elemTy)) = Vector.foldl (fn (exp, acc) => acc + costOfExp exp) 1 exps
  | costOfExp (F.VectorExp (exps, elemTy)) = Vector.foldl (fn (exp, acc) => acc + costOfExp exp) 1 exps
  | costOfExp (F.TyAbsExp (tv, kind, exp)) = costOfExp exp
  | costOfExp (F.TyAppExp (exp, ty)) = costOfExp exp
  | costOfExp (F.RecordEqualityExp fields) = List.foldl (fn ((label, exp), acc) => acc + costOfExp exp) 1 fields
  | costOfExp (F.DataTagExp exp) = 1 + costOfExp exp
  | costOfExp (F.DataPayloadExp exp) = 1 + costOfExp exp
  | costOfExp (F.StructExp { valMap, strMap, exnTagMap, equalityMap }) = 1
  | costOfExp (F.SProjectionExp (exp, _)) = costOfExp exp
and costOfDec (F.ValDec valbind) = costOfValBind valbind
  | costOfDec (F.RecValDec valbinds) = List.foldl (fn ((vid, ty, exp), acc) => acc + costOfExp exp) 0 valbinds
  | costOfDec (F.IgnoreDec exp) = costOfExp exp
  | costOfDec (F.DatatypeDec datbinds) = List.length datbinds
  | costOfDec (F.ExceptionDec { conName, tagName, payloadTy }) = 1
  | costOfDec (F.ExceptionRepDec { conName, conPath, tagPath, payloadTy }) = 0
  | costOfDec (F.ExportValue exp) = costOfExp exp
  | costOfDec (F.ExportModule entities) = Vector.foldl (fn ((name, exp), acc) => acc + costOfExp exp) 0 entities
  | costOfDec (F.GroupDec (_, decs)) = List.foldl (fn (dec, acc) => acc + costOfDec dec) 0 decs
and costOfValBind (F.SimpleBind (vid, ty, exp)) = costOfExp exp
  | costOfValBind (F.TupleBind (binds, exp)) = costOfExp exp
val INLINE_THRESHOLD = 10
fun substTyInInlineExp subst = let val { doTy, doExp, ... } = F.substTy subst
                                   fun doInlineExp (iexp as Path path) = iexp
                                     | doInlineExp (iexp as StructExp _) = iexp
                                     | doInlineExp (FnExp (vid, ty, exp)) = FnExp (vid, doTy ty, doExp exp)
                                     | doInlineExp (TyAbsExp (tv, kind, iexp)) = if USyntax.TyVarMap.inDomain (subst, tv) then
                                                                                     TyAbsExp (tv, kind, substTyInInlineExp (#1 (USyntax.TyVarMap.remove (subst, tv))) iexp) (* TODO: use fresh tyvar if necessary *)
                                                                                 else
                                                                                     TyAbsExp (tv, kind, doInlineExp iexp)
                                     | doInlineExp (TyAppExp (iexp, ty)) = TyAppExp (doInlineExp iexp, doTy ty)
                               in doInlineExp
                               end
fun uninlineExp (Path path) = F.PathToExp path
  | uninlineExp (StructExp { valMap, strMap, exnTagMap, equalityMap }) = F.StructExp { valMap = valMap, strMap = strMap, exnTagMap = exnTagMap, equalityMap = equalityMap }
  | uninlineExp (FnExp (vid, ty, exp)) = F.FnExp (vid, ty, exp)
  | uninlineExp (TyAbsExp (tv, kind, iexp)) = F.TyAbsExp (tv, kind, uninlineExp iexp)
  | uninlineExp (TyAppExp (iexp, ty)) = F.TyAppExp (uninlineExp iexp, ty)
fun tryInlineExp (F.VarExp vid) = SOME (Path (F.Root vid))
  | tryInlineExp (F.SProjectionExp (exp, label)) = (case tryInlineExp exp of
                                                        SOME (Path path) => SOME (Path (F.Child (path, label)))
                                                      | _ => NONE
                                                   )
  | tryInlineExp (F.StructExp { valMap, strMap, exnTagMap, equalityMap }) = SOME (StructExp { valMap = valMap
                                                                                            , strMap = strMap
                                                                                            , exnTagMap = exnTagMap
                                                                                            , equalityMap = equalityMap
                                                                                            }
                                                                                 )
  | tryInlineExp (F.FnExp (vid, paramTy, body)) = if costOfExp body <= INLINE_THRESHOLD then
                                                      SOME (FnExp (vid, paramTy, body))
                                                  else
                                                      NONE
  | tryInlineExp (F.TyAbsExp (tv, kind, exp)) = (case tryInlineExp exp of
                                                     SOME iexp => SOME (TyAbsExp (tv, kind, iexp))
                                                   | NONE => NONE
                                                )
  | tryInlineExp (F.TyAppExp (exp, ty)) = (case tryInlineExp exp of
                                               SOME iexp => SOME (TyAppExp (iexp, ty))
                                             | NONE => NONE
                                          )
  | tryInlineExp _ = NONE
fun eliminateVariables (ctx : Context) : { doExp : Env -> F.Exp -> F.Exp
                                         , doDec : Env -> F.Dec -> (* modified environment *) Env * F.Dec
                                         , doDecs : Env -> F.Dec list -> (* modified environment *) Env * F.Dec list
                                         }
    = let fun doPath (env : Env) (path as F.Root vid) = (case USyntax.VIdMap.find (#vidMap env, vid) of
                                                             SOME (Path path) => doPath env path
                                                           | SOME iexp => (path, SOME iexp)
                                                           | NONE => (path, SOME (Path path))
                                                        )
            | doPath env (F.Child (parent, label)) = (case doPath env parent of
                                                          (parent, SOME (StructExp m)) =>
                                                          (case lookupSLabel(m, label) of
                                                               SOME path => doPath env path
                                                             | NONE => (F.Child (parent, label), NONE) (* should be an error *)
                                                          )
                                                        | (parent, SOME (Path path)) => (F.Child (parent, label), SOME (Path (F.Child (path, label))))
                                                        | (parent, _) => (F.Child (parent, label), SOME (Path (F.Child (parent, label))))
                                                     )
          fun doExp env exp = #1 (doExp' env exp)
          and doExp' (env : Env) exp0
              = (case exp0 of
                     F.SConExp _ => (exp0, NONE)
                   | F.VarExp vid => (case USyntax.VIdMap.find (#vidMap env, vid) of
                                          SOME (Path path) => let val (path, iexpOpt) = doPath env path
                                                              in (F.PathToExp path, iexpOpt)
                                                              end
                                        | iexpOpt as SOME _ => (exp0, iexpOpt)
                                        | NONE => (exp0, SOME (Path (F.Root vid)))
                                     )
                   | F.RecordExp fields => (F.RecordExp (List.map (fn (label, exp) => (label, doExp env exp)) fields), NONE)
                   | F.LetExp (dec, exp) => let val (env, dec) = doDec env dec
                                            in (F.LetExp (dec, doExp env exp), NONE)
                                            end
                   | F.AppExp (exp1, exp2) => let val (exp1, iexp1) = doExp' env exp1
                                              in case iexp1 of
                                                     SOME (FnExp (vid, paramTy, body)) => doExp' env (F.LetExp (F.ValDec (F.SimpleBind (vid, paramTy, exp2)), body))
                                                   | SOME (Path path) => (F.AppExp (F.PathToExp path, doExp env exp2), NONE)
                                                   | _ => (F.AppExp (exp1, doExp env exp2), NONE)
                                              end
                   | F.HandleExp { body, exnName, handler } => ( F.HandleExp { body = doExp env body
                                                                             , exnName = exnName
                                                                             , handler = let val env' = removeFromEnv (exnName, env)
                                                                                         in doExp env' handler
                                                                                         end
                                                                             }
                                                               , NONE
                                                               )
                   | F.RaiseExp (span, exp) => (F.RaiseExp (span, doExp env exp), NONE)
                   | F.IfThenElseExp (exp1, exp2, exp3) => (F.IfThenElseExp (doExp env exp1, doExp env exp2, doExp env exp3), NONE)
                   | F.CaseExp (span, exp, ty, matches) => let fun doMatch (pat, exp) = let val vars = freeVarsInPat pat
                                                                                            val env' = { vidMap = USyntax.VIdMap.filteri (fn (vid, _) => not (USyntax.VIdSet.member (vars, vid))) (#vidMap env) }
                                                                                        in (pat, doExp env' exp)
                                                                                        end
                                                           in (F.CaseExp (span, doExp env exp, ty, List.map doMatch matches), NONE)
                                                           end
                   | F.FnExp (vid, ty, exp) => let val env' = removeFromEnv (vid, env)
                                                   val exp' = doExp env' exp
                                               in (F.FnExp (vid, ty, exp'), if costOfExp exp' <= INLINE_THRESHOLD then SOME (FnExp (vid, ty, exp')) else NONE)
                                               end
                   | F.ProjectionExp { label, recordTy, fieldTy } => (exp0, NONE)
                   | F.ListExp (xs, ty) => (F.ListExp (Vector.map (doExp env) xs, ty), NONE)
                   | F.VectorExp (xs, ty) => (F.VectorExp (Vector.map (doExp env) xs, ty), NONE)
                   | F.TyAbsExp (tv, kind, exp) => let val (exp, iexp) = doExp' env exp
                                                       val c = case exp of
                                                                   F.TyAppExp(exp', F.TyVar tv') => if tv = tv' then
                                                                                                        let val fv = F.freeTyVarsInExp (USyntax.TyVarSet.empty, exp')
                                                                                                        in if USyntax.TyVarSet.member (fv, tv) then
                                                                                                               NONE
                                                                                                           else
                                                                                                               SOME (doExp' env exp)
                                                                                                        end
                                                                                                    else
                                                                                                        NONE
                                                                 | _ => NONE
                                                   in case c of
                                                          SOME result => result
                                                        | NONE => (F.TyAbsExp (tv, kind, exp), case iexp of
                                                                                                   SOME iexp => SOME (TyAbsExp (tv, kind, iexp)) (* TODO: hoisting? *)
                                                                                                 | NONE => NONE)
                                                   end
                   | F.TyAppExp (exp, ty) => let val (exp, iexp) = doExp' env exp
                                             in case iexp of
                                                    SOME (TyAbsExp (tv, kind, exp)) => let val iexp = substTyInInlineExp (USyntax.TyVarMap.singleton (tv, ty)) exp
                                                                                       in (uninlineExp iexp, SOME iexp)
                                                                                       end
                                                  | SOME (iexp as Path path) => (F.TyAppExp (F.PathToExp path, ty), SOME (TyAppExp (iexp, ty)))
                                                  | SOME iexp => (F.TyAppExp (exp, ty), SOME (TyAppExp (iexp, ty)))
                                                  | NONE => (F.TyAppExp (exp, ty), NONE)
                                             end
                   | F.RecordEqualityExp fields => (F.RecordEqualityExp (List.map (fn (label, exp) => (label, doExp env exp)) fields), NONE)
                   | F.DataTagExp exp => (F.DataTagExp (doExp env exp), NONE)
                   | F.DataPayloadExp exp => (F.DataPayloadExp (doExp env exp), NONE)
                   | F.StructExp { valMap, strMap, exnTagMap, equalityMap } => ( F.StructExp { valMap = Syntax.VIdMap.map (#1 o doPath env) valMap
                                                                                             , strMap = Syntax.StrIdMap.map (#1 o doPath env) strMap
                                                                                             , exnTagMap = Syntax.VIdMap.map (#1 o doPath env) exnTagMap
                                                                                             , equalityMap = Syntax.TyConMap.map (#1 o doPath env) equalityMap
                                                                                             }
                                                                               , SOME (StructExp { valMap = valMap
                                                                                                 , strMap = strMap
                                                                                                 , exnTagMap = exnTagMap
                                                                                                 , equalityMap = equalityMap
                                                                                                 }
                                                                                      )
                                                                               )
                   | F.SProjectionExp (exp, label) => (case doExp' env exp of
                                                           (exp, SOME (Path path)) => let val (path, iexpOpt) = doPath env path
                                                                                      in (F.SProjectionExp (exp, label), case iexpOpt of
                                                                                                                             SOME (Path path) => SOME (Path (F.Child (path, label)))
                                                                                                                           | SOME (StructExp m) => (case lookupSLabel(m, label) of
                                                                                                                                                        SOME path => #2 (doPath env path)
                                                                                                                                                      | NONE => NONE (* should be an error *)
                                                                                                                                                   )
                                                                                                                           | _ => NONE
                                                                                         )
                                                                                      end
                                                         | (exp, SOME (StructExp m)) => (case lookupSLabel(m, label) of
                                                                                             SOME path => let val (path, iexpOpt) = doPath env path
                                                                                                          in (F.PathToExp path, iexpOpt)
                                                                                                          end
                                                                                           | NONE => (F.SProjectionExp (exp, label), NONE) (* should be an error *)
                                                                                        )
                                                         | (exp, _) => (F.SProjectionExp (exp, label), NONE)
                                                      )
                )
          and doDec (env : Env) (F.ValDec valbind) = let val (env, valbind) = doValBind env valbind
                                                     in (env, F.ValDec valbind)
                                                     end
            | doDec env (F.RecValDec valbinds) = let fun go (env, acc, []) = (env, F.RecValDec (List.rev acc))
                                                       | go (env, acc, (vid, ty, exp) :: valbinds) = let val exp = doExp env exp
                                                                                                         val acc = (vid, ty, exp) :: acc
                                                                                                     in go (removeFromEnv (vid, env), acc, valbinds)
                                                                                                     end
                                                 in go (env, [], valbinds)
                                                 end
            | doDec env (F.IgnoreDec exp) = (env, F.IgnoreDec (doExp env exp))
            | doDec env (dec as F.DatatypeDec datbinds) = (env, dec) (* TODO *)
            | doDec env (dec as F.ExceptionDec _) = (env, dec) (* TODO *)
            | doDec env (dec as F.ExceptionRepDec _) = (env, dec) (* TODO *)
            | doDec env (F.ExportValue exp) = (env, F.ExportValue (doExp env exp))
            | doDec env (F.ExportModule fields) = (env, F.ExportModule (Vector.map (fn (label, exp) => (label, doExp env exp)) fields))
            | doDec env (F.GroupDec (v, decs)) = let val (env, decs) = doDecs env decs
                                                 in (env, case decs of
                                                              [dec] => dec
                                                            | _ => F.GroupDec (v, decs)
                                                    )
                                                 end
          and doValBind env (F.SimpleBind (vid, ty, exp)) = let val (exp, iexpOpt) = doExp' env exp
                                                                val env' = case iexpOpt of
                                                                               SOME iexp => { vidMap = USyntax.VIdMap.insert (#vidMap env, vid, iexp) }
                                                                             | NONE => removeFromEnv (vid, env)
                                                            in (env', F.SimpleBind (vid, ty, exp))
                                                            end
            | doValBind env (F.TupleBind (binds, exp)) = let val vars = List.map #1 binds
                                                             val env' = List.foldl removeFromEnv env vars
                                                         in (env', F.TupleBind (binds, doExp env exp))
                                                         end
          and doDecs env [] = (env, [])
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
                                                   (exp1 as F.TyAppExp (exp1', ty1), exp2 as F.ListExp (xs, ty2)) =>
                                                   if F.isLongVId(exp1', InitialEnv.VId_Vector_fromList) then
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
                   | F.TyAbsExp (tyvar, kind, exp) => F.TyAbsExp (tyvar, kind, doExp env exp)
                   | F.TyAppExp (exp, ty) => (case doExp env exp of
                                                  F.TyAbsExp(tyvar, kind, exp') => let val substExp = #doExp (F.substTy (USyntax.TyVarMap.singleton (tyvar, ty)))
                                                                                   in substExp exp'
                                                                                   end
                                                | exp => F.TyAppExp (doExp env exp, ty)
                                             )
                   | F.RecordEqualityExp fields => F.RecordEqualityExp (List.map (fn (label, exp) => (label, doExp env exp)) fields)
                   | F.DataTagExp exp => F.DataTagExp (doExp env exp)
                   | F.DataPayloadExp exp => F.DataPayloadExp (doExp env exp)
                   | F.StructExp { valMap, strMap, exnTagMap, equalityMap } => F.StructExp { valMap = valMap
                                                                                           , strMap = strMap
                                                                                           , exnTagMap = exnTagMap
                                                                                           , equalityMap = equalityMap
                                                                                           }
                   | F.SProjectionExp (exp, label) => F.SProjectionExp (doExp env exp, label)
                )
          and doDec (env : Env) (F.ValDec valbind) = (env, F.ValDec (doValBind env valbind))
            | doDec env (F.RecValDec valbinds) = (env, F.RecValDec (List.map (fn (v, ty, exp) => (v, ty, doExp env exp)) valbinds))
            | doDec env (F.IgnoreDec exp) = (env, F.IgnoreDec (doExp env exp))
            | doDec env (dec as F.DatatypeDec datbinds) = (env, dec)
            | doDec env (dec as F.ExceptionDec _) = (env, dec)
            | doDec env (dec as F.ExceptionRepDec _) = (env, dec)
            | doDec env (F.ExportValue exp) = (env, F.ExportValue (doExp env exp))
            | doDec env (F.ExportModule fields) = (env, F.ExportModule (Vector.map (fn (label, exp) => (label, doExp env exp)) fields))
            | doDec env (F.GroupDec (v, decs)) = let val (env, decs) = doDecs env decs
                                                 in (env, F.GroupDec (v, decs))
                                                 end
          and doValBind env (F.SimpleBind (v, ty, exp)) = F.SimpleBind (v, ty, doExp env exp)
            | doValBind env (F.TupleBind (vars, exp)) = F.TupleBind (vars, doExp env exp)
          and doDecs env [] = (env, [])
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

structure FlattenLet = struct
local structure F = FSyntax in
fun extractLet (F.LetExp (dec, exp)) = let val (decs, exp) = extractLet exp
                                       in (dec :: decs, exp)
                                       end
  | extractLet exp = ([], exp)
fun doExp (exp as F.SConExp _) = exp
  | doExp (exp as F.VarExp _) = exp
  | doExp (F.RecordExp fields) = F.RecordExp (List.map (fn (label, exp) => (label, doExp exp)) fields)
  | doExp (F.LetExp (dec, exp2)) = F.LetExp (doDec dec, doExp exp2)
  | doExp (F.AppExp (exp1, exp2)) = F.AppExp (doExp exp1, doExp exp2)
  | doExp (F.HandleExp { body, exnName, handler }) = F.HandleExp { body = doExp body, exnName = exnName, handler = doExp handler }
  | doExp (F.RaiseExp (span, exp)) = F.RaiseExp (span, doExp exp)
  | doExp (F.IfThenElseExp (exp1, exp2, exp3)) = F.IfThenElseExp (doExp exp1, doExp exp2, doExp exp3)
  | doExp (F.CaseExp (span, exp, ty, matches)) = F.CaseExp (span, doExp exp, ty, List.map (fn (pat, exp) => (pat, doExp exp)) matches)
  | doExp (F.FnExp (vid, ty, exp)) = F.FnExp (vid, ty, doExp exp)
  | doExp (exp as F.ProjectionExp _) = exp
  | doExp (F.ListExp (xs, ty)) = F.ListExp (Vector.map doExp xs, ty)
  | doExp (F.VectorExp (xs, ty)) = F.VectorExp (Vector.map doExp xs, ty)
  | doExp (F.TyAbsExp (tv, kind, exp)) = F.TyAbsExp (tv, kind, doExp exp)
  | doExp (F.TyAppExp (exp, ty)) = F.TyAppExp (doExp exp, ty)
  | doExp (F.RecordEqualityExp fields) = F.RecordEqualityExp (List.map (fn (label, exp) => (label, doExp exp)) fields)
  | doExp (F.DataTagExp exp) = F.DataTagExp (doExp exp)
  | doExp (F.DataPayloadExp exp) = F.DataPayloadExp (doExp exp)
  | doExp (F.StructExp { valMap, strMap, exnTagMap, equalityMap }) = F.StructExp { valMap = valMap
                                                                                 , strMap = strMap
                                                                                 , exnTagMap = exnTagMap
                                                                                 , equalityMap = equalityMap
                                                                                 }
  | doExp (F.SProjectionExp (exp, label)) = F.SProjectionExp (doExp exp, label)
and doDec (F.ValDec (F.SimpleBind (vid, ty, exp1))) = let val (decs, exp1) = extractLet (doExp exp1)
                                                          val dec = F.ValDec (F.SimpleBind (vid, ty, exp1))
                                                      in if List.null decs then
                                                             dec
                                                         else
                                                             F.GroupDec (NONE, decs @ [dec])
                                                      end
  | doDec (F.ValDec (F.TupleBind (binds, exp1))) = let val (decs, exp1) = extractLet (doExp exp1)
                                                       val dec = F.ValDec (F.TupleBind (binds, exp1))
                                                   in if List.null decs then
                                                          dec
                                                      else
                                                          F.GroupDec (NONE, decs @ [dec])
                                                   end
  | doDec (F.RecValDec valbinds) = F.RecValDec (List.map (fn (vid, ty, exp) => (vid, ty, doExp exp)) valbinds)
  | doDec (F.IgnoreDec exp) = F.IgnoreDec (doExp exp)
  | doDec (dec as F.DatatypeDec _) = dec
  | doDec (dec as F.ExceptionDec _) = dec
  | doDec (dec as F.ExceptionRepDec _) = dec
  | doDec (F.ExportValue exp) = F.ExportValue (doExp exp)
  | doDec (F.ExportModule xs) = F.ExportModule (Vector.map (fn (name, exp) => (name, doExp exp)) xs)
  | doDec (F.GroupDec (e, decs)) = F.GroupDec (e, doDecs decs)
and doDecs decs = List.map doDec decs
end (* local *)
end (* structure FlattenLet *)

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
                                      val decs = FlattenLet.doDecs decs
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
  | isDiscardable (F.AppExp (F.TyAppExp (exp1', _), exp2)) = F.isLongVId (exp1', InitialEnv.VId_assumePure) orelse F.isLongVId (exp1', InitialEnv.VId_assumeDiscardable)
  | isDiscardable (F.AppExp (exp1, exp2)) = false (* TODO *)
  | isDiscardable (F.HandleExp { body, exnName, handler }) = false (* TODO *)
  | isDiscardable (F.RaiseExp (span, exp)) = false
  | isDiscardable (F.IfThenElseExp (exp1, exp2, exp3)) = isDiscardable exp1 andalso isDiscardable exp2 andalso isDiscardable exp3
  | isDiscardable (F.CaseExp (span, exp, ty, matches)) = false (* TODO *)
  | isDiscardable (F.FnExp (vid, ty, exp)) = true
  | isDiscardable (F.ProjectionExp { label, recordTy, fieldTy }) = true
  | isDiscardable (F.ListExp (xs, ty)) = Vector.all isDiscardable xs
  | isDiscardable (F.VectorExp (xs, ty)) = Vector.all isDiscardable xs
  | isDiscardable (F.TyAbsExp (tyvar, kind, exp)) = isDiscardable exp
  | isDiscardable (F.TyAppExp (exp, ty)) = isDiscardable exp
  | isDiscardable (F.RecordEqualityExp fields) = List.all (fn (label, exp) => isDiscardable exp) fields
  | isDiscardable (F.DataTagExp exp) = isDiscardable exp
  | isDiscardable (F.DataPayloadExp exp) = isDiscardable exp
  | isDiscardable (F.StructExp { valMap, strMap, exnTagMap, equalityMap }) = true
  | isDiscardable (F.SProjectionExp (exp, label)) = isDiscardable exp
(* doPat : F.Pat -> (* constructors used *) USyntax.VIdSet.set *)
fun doPat F.WildcardPat = USyntax.VIdSet.empty
  | doPat (F.SConPat _) = USyntax.VIdSet.empty
  | doPat (F.VarPat _) = USyntax.VIdSet.empty
  | doPat (F.RecordPat (fields, wildcard)) = List.foldl (fn ((label, pat), acc) => USyntax.VIdSet.union (acc, doPat pat)) USyntax.VIdSet.empty fields
  | doPat (F.ConPat (F.Root vid, NONE, tyargs)) = USyntax.VIdSet.singleton vid
  | doPat (F.ConPat (F.Root vid, SOME innerPat, tyargs)) = USyntax.VIdSet.add (doPat innerPat, vid)
  | doPat (F.ConPat (F.Child _, _, tyargs)) = raise Fail "not implemented yet"
  | doPat (F.LayeredPat (vid, ty, innerPat)) = doPat innerPat
  | doPat (F.VectorPat (pats, ellipsis, elemTy)) = Vector.foldl (fn (pat, acc) => USyntax.VIdSet.union (acc, doPat pat)) USyntax.VIdSet.empty pats
(* doExp : F.Exp -> USyntax.VIdSet.set * F.Exp *)
fun doExp (exp as F.SConExp _ : F.Exp) : USyntax.VIdSet.set * F.Exp = (USyntax.VIdSet.empty, exp)
  | doExp (exp as F.VarExp vid) = (USyntax.VIdSet.singleton vid, exp)
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
  | doExp (F.TyAbsExp (tyvar, kind, exp)) = let val (used, exp) = doExp exp
                                            in (used, F.TyAbsExp (tyvar, kind, exp))
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
  | doExp (F.StructExp { valMap, strMap, exnTagMap, equalityMap }) = let val used = Syntax.VIdMap.foldl (fn (path, acc) => USyntax.VIdSet.add (acc, F.rootOfPath path)) USyntax.VIdSet.empty valMap
                                                                         val used = Syntax.StrIdMap.foldl (fn (path, acc) => USyntax.VIdSet.add (acc, F.rootOfPath path)) used strMap
                                                                         val used = Syntax.VIdMap.foldl (fn (path, acc) => USyntax.VIdSet.add (acc, F.rootOfPath path)) used exnTagMap
                                                                         val used = Syntax.TyConMap.foldl (fn (path, acc) => USyntax.VIdSet.add (acc, F.rootOfPath path)) used equalityMap
                                                                     in (used, F.StructExp { valMap = valMap
                                                                                           , strMap = strMap
                                                                                           , exnTagMap = exnTagMap
                                                                                           , equalityMap = equalityMap
                                                                                           }
                                                                        )
                                                                     end
  | doExp (F.SProjectionExp (exp, label)) = let val (used, exp) = doExp exp
                                            in (used, F.SProjectionExp (exp, label))
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
  | doIgnoredExp (F.TyAbsExp (tyvar, kind, exp)) = let val (used, exp) = doIgnoredExpAsExp exp (* should be pure *)
                                                   in case exp of
                                                          F.RecordExp [] => (used, [])
                                                        | exp => (used, [F.TyAbsExp (tyvar, kind, exp)])
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
  | doIgnoredExp (F.StructExp { valMap, strMap, exnTagMap, equalityMap }) = (USyntax.VIdSet.empty, [])
  | doIgnoredExp (F.SProjectionExp (exp, label)) = doIgnoredExp exp
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
                                 (List.map (fn (vid, _, _) => USyntax.VIdSet.singleton vid) valbinds)
      in if USyntax.VIdSet.disjoint (used, bound) then
             (used, []) (* RHS should be fn _ => _, and therefore discardable *)
         else
             let val (used, valbinds) = List.foldr (fn ((vid, ty, exp), (used, valbinds)) => let val (used', exp) = doExp exp
                                                                                             in (USyntax.VIdSet.union (used, used'), (vid, ty, exp) :: valbinds)
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
  | doDec (used, dec as F.ExceptionRepDec { conName, conPath, tagPath, payloadTy }) = if USyntax.VIdSet.member (used, conName) then
                                                                                          (used, [dec])
                                                                                      else
                                                                                          (used, [])
  | doDec (used, F.ExportValue exp) = let val (used', exp) = doExp exp
                                      in (USyntax.VIdSet.union (used, used'), [F.ExportValue exp])
                                      end
  | doDec (used, F.ExportModule fields) = let val fields' = Vector.map (fn (label, exp) => (label, doExp exp)) fields
                                          in (Vector.foldl (fn ((_, (used', _)), acc) => USyntax.VIdSet.union (used', acc)) used fields', [F.ExportModule (Vector.map (fn (label, (_, exp)) => (label, exp)) fields')])
                                          end
  | doDec (used, F.GroupDec (_, decs)) = let val (used', decs) = doDecs (used, decs)
                                         in (used', case decs of
                                                        [] => decs
                                                      | [_] => decs
                                                      | _ => let val defined = definedInDecs(decs)
                                                             in [F.GroupDec (SOME (USyntax.VIdSet.intersection (used, defined)), decs)]
                                                             end
                                            )
                                         end
(* doDecs : USyntax.VIdSet.set * F.Dec list -> USyntax.VIdSet.set * F.Dec list *)
and doDecs (used, decs) = List.foldr (fn (dec, (used, decs)) => let val (used, dec) = doDec (used, dec)
                                                                in (used, dec @ decs)
                                                                end) (used, []) decs
and definedInDecs decs = List.foldl (fn (dec, s) => USyntax.VIdSet.union(definedInDec dec, s)) USyntax.VIdSet.empty decs
and definedInDec (F.ValDec valbind) = definedInValBind valbind
  | definedInDec (F.RecValDec valbinds) = List.foldl (fn ((vid, _, _), s) => USyntax.VIdSet.add(s, vid)) USyntax.VIdSet.empty valbinds
  | definedInDec (F.IgnoreDec _) = USyntax.VIdSet.empty
  | definedInDec (F.DatatypeDec datbinds) = List.foldl (fn (F.DatBind (tyvars, tycon, conbinds), s) => List.foldl (fn (F.ConBind(vid, _), s) => USyntax.VIdSet.add(s, vid)) s conbinds) USyntax.VIdSet.empty datbinds
  | definedInDec (F.ExceptionDec { conName, tagName, ... }) = USyntax.VIdSet.add(USyntax.VIdSet.singleton conName, tagName)
  | definedInDec (F.ExceptionRepDec { conName, ... }) = USyntax.VIdSet.singleton conName
  | definedInDec (F.ExportValue _) = USyntax.VIdSet.empty (* should not occur *)
  | definedInDec (F.ExportModule _) = USyntax.VIdSet.empty (* should not occur *)
  | definedInDec (F.GroupDec(_, decs)) = definedInDecs decs (* should not occur *)
and definedInValBind (F.SimpleBind (vid, _, _)) = USyntax.VIdSet.singleton vid
  | definedInValBind (F.TupleBind (binds, _)) = List.foldl (fn ((vid, _), s) => USyntax.VIdSet.add(s, vid)) USyntax.VIdSet.empty binds
end (* structure DeadCodeElimination *)
