(*
 * Copyright (c) 2021 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure DesugarPatternMatches = struct
structure F = FSyntax
type Context = { nextVId : int ref
               , nextTyVar : int ref
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
                     F.PrimExp (primOp, tyargs, args) => F.PrimExp (primOp, tyargs, Vector.map (doExp env) args)
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
                   | F.IfThenElseExp(exp1, exp2, exp3) => F.IfThenElseExp(doExp env exp1, doExp env exp2, doExp env exp3)
                   | F.FnExp(vid, ty, exp) => let val env = addVar(env, vid, ty)
                                              in F.FnExp(vid, ty, doExp env exp)
                                              end
                   | F.ProjectionExp _ => exp0
                   | F.TyAbsExp(tv, kind, exp) => F.TyAbsExp(tv, kind, doExp env exp) (* TODO: update type environment? *)
                   | F.TyAppExp(exp, ty) => F.TyAppExp(exp, ty)
                   | F.StructExp { valMap, strMap, exnTagMap, equalityMap } => F.StructExp { valMap = valMap
                                                                                           , strMap = strMap
                                                                                           , exnTagMap = exnTagMap
                                                                                           , equalityMap = equalityMap
                                                                                           }
                   | F.SProjectionExp (exp, label) => F.SProjectionExp (doExp env exp, label)
                   | F.PackExp { payloadTy, exp, packageTy } => F.PackExp { payloadTy = payloadTy, exp = doExp env exp, packageTy = packageTy }
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
                                        fun go [] = F.RaiseExp(span, (* TODO: type of raise *) F.RecordType [], F.VarExp(InitialEnv.VId_Match))
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
            | doDec env (F.UnpackDec (tv, kind, vid, ty, exp)) = let val env' = addVar(env, vid, ty)
                                                                 in (env', F.UnpackDec (tv, kind, vid, ty, doExp env exp))
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
                        = let val ty = List.foldl (fn (arg, applied) => F.AppType { applied = applied, arg = F.TyVar arg }) (F.TyVar tycon) tyvars
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

structure DecomposeValRec = struct
structure F = FSyntax
type Context = {}
fun doExp (F.PrimExp (primOp, tyargs, args)) = F.PrimExp (primOp, tyargs, Vector.map doExp args)
  | doExp (exp as F.VarExp _) = exp
  | doExp (F.RecordExp fields) = F.RecordExp (List.map (fn (label, exp) => (label, doExp exp)) fields)
  | doExp (F.LetExp (dec, exp)) = let val decs = doDec dec
                                  in List.foldr F.LetExp (doExp exp) decs
                                  end
  | doExp (F.AppExp (exp1, exp2)) = F.AppExp (doExp exp1, doExp exp2)
  | doExp (F.HandleExp { body, exnName, handler }) = F.HandleExp { body = doExp body
                                                                 , exnName = exnName
                                                                 , handler = doExp handler
                                                                 }
  | doExp (F.IfThenElseExp (exp1, exp2, exp3)) = F.IfThenElseExp (doExp exp1, doExp exp2, doExp exp3)
  | doExp (F.CaseExp (span, exp, ty, matches)) = F.CaseExp (span, doExp exp, ty, List.map (fn (pat, exp) => (pat, doExp exp)) matches)
  | doExp (F.FnExp (vid, ty, exp)) = F.FnExp (vid, ty, doExp exp)
  | doExp (exp as F.ProjectionExp _) = exp
  | doExp (F.TyAbsExp (tv, kind, exp)) = F.TyAbsExp (tv, kind, doExp exp)
  | doExp (F.TyAppExp (exp, ty)) = F.TyAppExp (doExp exp, ty)
  | doExp (F.StructExp maps) = F.StructExp maps
  | doExp (F.SProjectionExp (exp, label)) = F.SProjectionExp (doExp exp, label)
  | doExp (F.PackExp { payloadTy, exp, packageTy }) = F.PackExp { payloadTy = payloadTy, exp = doExp exp, packageTy = packageTy }
and doDec (F.ValDec (F.SimpleBind (vid, ty, exp))) = [F.ValDec (F.SimpleBind (vid, ty, doExp exp))]
  | doDec (F.ValDec (F.TupleBind (binds, exp))) = [F.ValDec (F.TupleBind (binds, doExp exp))]
  | doDec (F.RecValDec valbinds)
    = let val bound = List.foldl (fn ((vid, ty, exp), set) => USyntax.VIdSet.add (set, vid)) USyntax.VIdSet.empty valbinds
          val map = List.foldl (fn ((vid, ty, exp), map) => let val exp = doExp exp
                                                            in USyntax.VIdMap.insert (map, vid, (ty, exp, USyntax.VIdSet.intersection (F.freeVarsInExp (USyntax.VIdSet.empty, exp), bound), ref [], ref false, ref false))
                                                            end) USyntax.VIdMap.empty valbinds
          fun dfs1 (from, vid) = let val (ty, exp, refs, invref, seen1, _) = USyntax.VIdMap.lookup (map, vid)
                                     val () = case from of
                                                  SOME vid' => invref := vid' :: !invref
                                                | NONE => ()
                                 in if !seen1 then
                                        []
                                    else
                                        ( seen1 := true
                                        ; USyntax.VIdSet.foldl (fn (vid', acc) => acc @ dfs1 (SOME vid, vid')) [vid] refs
                                        )
                                 end
          val list = USyntax.VIdMap.foldli (fn (vid, _, acc) => acc @ dfs1 (NONE, vid)) [] map
          fun dfs2 vid = let val (ty, exp, refs, ref invrefs, _, seen2) = USyntax.VIdMap.lookup (map, vid)
                         in if !seen2 then
                                USyntax.VIdSet.empty
                            else
                                ( seen2 := true
                                ; List.foldl (fn (vid', acc) => USyntax.VIdSet.union (acc, dfs2 vid')) (USyntax.VIdSet.singleton vid) invrefs
                                )
                         end
          val sccs = List.foldl (fn (vid, acc) => let val set = dfs2 vid
                                                  in if USyntax.VIdSet.isEmpty set then
                                                         acc
                                                     else
                                                         set :: acc
                                                  end) [] list
      in List.foldl (fn (scc, decs) => let val dec = case USyntax.VIdSet.listItems scc of
                                                         [vid] => let val (ty, exp, refs, _, _, _) = USyntax.VIdMap.lookup (map, vid)
                                                                  in if USyntax.VIdSet.member (refs, vid) then
                                                                         F.RecValDec [(vid, ty, exp)]
                                                                     else
                                                                         F.ValDec (F.SimpleBind (vid, ty, exp))
                                                                  end
                                                       | scc => F.RecValDec (List.foldl (fn (vid, xs) =>
                                                                                            let val (ty, exp, _, _, _, _) = USyntax.VIdMap.lookup (map, vid)
                                                                                            in (vid, ty, exp) :: xs
                                                                                            end
                                                                                        ) [] scc)
                                       in dec :: decs
                                       end
                    ) [] sccs
      end
  | doDec (F.UnpackDec (tv, kind, vid, ty, exp)) = [F.UnpackDec (tv, kind, vid, ty, doExp exp)]
  | doDec (F.IgnoreDec exp) = [F.IgnoreDec (doExp exp)]
  | doDec (F.DatatypeDec datbinds) = [F.DatatypeDec datbinds]
  | doDec (F.ExceptionDec names) = [F.ExceptionDec names]
  | doDec (F.ExceptionRepDec names) = [F.ExceptionRepDec names]
  | doDec (F.ExportValue exp) = [F.ExportValue (doExp exp)]
  | doDec (F.ExportModule fields) = [F.ExportModule (Vector.map (fn (label, exp) => (label, doExp exp)) fields)]
  | doDec (F.GroupDec (set, decs)) = [F.GroupDec (set, doDecs decs)]
and doDecs decs = List.foldr (fn (dec, rest) => doDec dec @ rest) [] decs
end

structure RefreshBoundNames = struct
local structure F = FSyntax in
type Context = { nextVId : int ref, nextTyVar : int ref }
type Env = { valMap : USyntax.VId USyntax.VIdMap.map
           , tyMap : USyntax.TyVar USyntax.TyVarMap.map
           }
val emptyEnv : Env = { valMap = USyntax.VIdMap.empty, tyMap = USyntax.TyVarMap.empty }
fun mergeEnv ({ valMap = valMap1, tyMap = tyMap1 } : Env, { valMap = valMap2, tyMap = tyMap2 } : Env)
    = { valMap = USyntax.VIdMap.unionWith #2 (valMap1, valMap2)
      , tyMap = USyntax.TyVarMap.unionWith #2 (tyMap1, tyMap2)
      }
fun insertVId ({ valMap, tyMap } : Env, vid, vid')
    = { valMap = USyntax.VIdMap.insert (valMap, vid, vid')
      , tyMap = tyMap
      }
fun insertTyVar ({ valMap, tyMap } : Env, tv, tv')
    = { valMap = valMap
      , tyMap = USyntax.TyVarMap.insert (tyMap, tv, tv')
      }
fun refreshVId (ctx : Context) (USyntax.MkVId (name, _)) = let val n = !(#nextVId ctx)
                                                           in #nextVId ctx := n + 1
                                                            ; USyntax.MkVId(name, n)
                                                           end
fun run (ctx : Context) : { doTy : Env -> F.Ty -> F.Ty
                          , doPat : Env -> F.Pat -> (* created environment *) Env * F.Pat
                          , doExp : Env -> F.Exp -> F.Exp
                          , doDec : Env -> F.Dec -> (* modified environment *) Env * F.Dec
                          , doDecs : Env -> F.Dec list -> (* modified environment *) Env * F.Dec list
                          }
    = let val refreshVId = refreshVId ctx
          fun refreshTyVar (USyntax.NamedTyVar (name, eq, _)) = let val n = !(#nextTyVar ctx)
                                                                in #nextTyVar ctx := n + 1
                                                                 ; USyntax.NamedTyVar (name, eq, n)
                                                                end
            | refreshTyVar (USyntax.AnonymousTyVar _) = let val n = !(#nextTyVar ctx)
                                                        in #nextTyVar ctx := n + 1
                                                         ; USyntax.AnonymousTyVar n
                                                        end
          fun doTy env (ty as F.TyVar tv) = (case USyntax.TyVarMap.find (#tyMap env, tv) of
                                                 SOME tv => F.TyVar tv
                                               | NONE => ty
                                            )
            | doTy env (F.RecordType fields) = F.RecordType (List.map (fn (label, ty) => (label, doTy env ty)) fields)
            | doTy env (F.AppType { applied, arg }) = F.AppType { applied = doTy env applied, arg = doTy env arg }
            | doTy env (F.FnType (ty1, ty2)) = F.FnType (doTy env ty1, doTy env ty2)
            | doTy env (F.ForallType (tv, kind, ty)) = let val tv' = refreshTyVar tv
                                                       in F.ForallType (tv', kind, doTy (insertTyVar (env, tv, tv')) ty)
                                                       end
            | doTy env (F.ExistsType (tv, kind, ty)) = let val tv' = refreshTyVar tv
                                                       in F.ExistsType (tv', kind, doTy (insertTyVar (env, tv, tv')) ty)
                                                       end
            | doTy env (F.SigType { valMap, strMap, exnTags, equalityMap }) = F.SigType { valMap = Syntax.VIdMap.map (doTy env) valMap
                                                                                        , strMap = Syntax.StrIdMap.map (doTy env) strMap
                                                                                        , exnTags = exnTags
                                                                                        , equalityMap = Syntax.TyConMap.map (doTy env) equalityMap
                                                                                        }
          fun doPath (env : Env) (path as F.Root vid) = (case USyntax.VIdMap.find (#valMap env, vid) of
                                                             SOME vid => F.Root vid
                                                           | NONE => path
                                                        )
            | doPath env (F.Child (parent, label)) = F.Child (doPath env parent, label)
          fun doPat env (pat as F.WildcardPat) = (emptyEnv, pat)
            | doPat env (pat as F.SConPat scon) = (emptyEnv, pat)
            | doPat env (F.VarPat (vid, ty)) = let val vid' = refreshVId vid
                                               in (insertVId (emptyEnv, vid, vid'), F.VarPat (vid', doTy env ty))
                                               end
            | doPat env (F.RecordPat (fields, wildcard))
              = let val (env', fields) = List.foldr (fn ((label, pat), (env', fields)) => let val (env'', pat) = doPat env pat
                                                                                          in (mergeEnv (env', env''), (label, pat) :: fields)
                                                                                          end
                                                    ) (emptyEnv, []) fields
                in (env', F.RecordPat (fields, wildcard))
                end
            | doPat env (F.ConPat (path, NONE, tyargs)) = (emptyEnv, F.ConPat (doPath env path, NONE, List.map (doTy env) tyargs))
            | doPat env (F.ConPat (path, SOME pat, tyargs)) = let val (env', pat) = doPat env pat
                                                              in (env', F.ConPat (doPath env path, SOME pat, List.map (doTy env) tyargs))
                                                              end
            | doPat env (F.LayeredPat (vid, ty, pat)) = let val vid' = refreshVId vid
                                                            val (env', pat) = doPat env pat
                                                        in (insertVId (env', vid, vid'), F.LayeredPat (vid', doTy env ty, pat))
                                                        end
            | doPat env (F.VectorPat (pats, ellipsis, ty))
              = let val (env', pats) = Vector.foldr (fn (pat, (env', pats)) => let val (env'', pat) = doPat env pat
                                                                               in (mergeEnv (env', env''), pat :: pats)
                                                                               end
                                                    ) (emptyEnv, []) pats
                in (env', F.VectorPat (Vector.fromList pats, ellipsis, doTy env ty))
                end
          fun doExp (env : Env) (F.PrimExp (primOp, tyargs, args)) = F.PrimExp (primOp, Vector.map (doTy env) tyargs, Vector.map (doExp env) args)
            | doExp env (exp as F.VarExp vid) = (case USyntax.VIdMap.find (#valMap env, vid) of
                                                     SOME vid => F.VarExp vid
                                                   | NONE => exp
                                                )
            | doExp env (F.RecordExp fields) = F.RecordExp (List.map (fn (label, exp) => (label, doExp env exp)) fields)
            | doExp env (F.LetExp (dec, exp)) = let val (env, dec) = doDec env dec
                                                in F.LetExp (dec, doExp env exp)
                                                end
            | doExp env (F.AppExp (exp1, exp2)) = F.AppExp (doExp env exp1, doExp env exp2)
            | doExp env (F.HandleExp { body, exnName, handler }) = let val exnName' = refreshVId exnName
                                                                   in F.HandleExp { body = doExp env body
                                                                                  , exnName = exnName'
                                                                                  , handler = doExp (insertVId (env, exnName, exnName')) handler
                                                                                  }
                                                                   end
            | doExp env (F.IfThenElseExp (exp1, exp2, exp3)) = F.IfThenElseExp (doExp env exp1, doExp env exp2, doExp env exp3)
            | doExp env (F.CaseExp (span, exp, ty, matches))
              = F.CaseExp (span, doExp env exp, doTy env ty, List.map (fn (pat, exp) => let val (env', pat) = doPat env pat
                                                                                        in (pat, doExp (mergeEnv (env, env')) exp)
                                                                                        end) matches)
            | doExp env (F.FnExp (vid, ty, exp)) = let val vid' = refreshVId vid
                                                   in F.FnExp (vid', doTy env ty, doExp (insertVId (env, vid, vid')) exp)
                                                   end
            | doExp env (F.ProjectionExp { label, recordTy, fieldTy }) = F.ProjectionExp { label = label, recordTy = doTy env recordTy, fieldTy = doTy env fieldTy }
            | doExp env (F.TyAbsExp (tv, kind, exp)) = let val tv' = refreshTyVar tv
                                                       in F.TyAbsExp (tv', kind, doExp (insertTyVar (env, tv, tv')) exp)
                                                       end
            | doExp env (F.TyAppExp (exp, ty)) = F.TyAppExp (doExp env exp, doTy env ty)
            | doExp env (exp as F.StructExp { valMap, strMap, exnTagMap, equalityMap }) = exp
            | doExp env (F.SProjectionExp (exp, label)) = F.SProjectionExp (doExp env exp, label)
            | doExp env (F.PackExp { payloadTy, exp, packageTy }) = F.PackExp { payloadTy = doTy env payloadTy
                                                                              , exp = doExp env exp
                                                                              , packageTy = doTy env packageTy
                                                                              }
          and doDec env (F.ValDec (F.SimpleBind (vid, ty, exp))) = let val vid' = refreshVId vid
                                                                   in (insertVId (env, vid, vid'), F.ValDec (F.SimpleBind (vid', doTy env ty, doExp env exp)))
                                                                   end
            | doDec env (F.ValDec (F.TupleBind (binds, exp))) = let val binds' = List.map (fn (vid, ty) => (vid, refreshVId vid, doTy env ty)) binds
                                                                in (List.foldl (fn ((vid, vid', _), env) => insertVId (env, vid, vid')) env binds', F.ValDec (F.TupleBind (List.map (fn (_, vid', ty) => (vid', ty)) binds', doExp env exp)))
                                                                end
            | doDec env (F.RecValDec valbinds) = let val valbinds' = List.map (fn (vid, ty, exp) => (vid, refreshVId vid, ty, exp)) valbinds
                                                     val env = List.foldl (fn ((vid, vid', _, _), env) => insertVId (env, vid, vid')) env valbinds'
                                                 in (env, F.RecValDec (List.map (fn (_, vid', ty, exp) => (vid', doTy env ty, doExp env exp)) valbinds'))
                                                 end
            | doDec env (F.UnpackDec (tv, kind, vid, ty, exp)) = let val exp = doExp env exp
                                                                     val tv' = refreshTyVar tv
                                                                     val env = insertTyVar (env, tv, tv')
                                                                     val ty = doTy env ty
                                                                     val vid' = refreshVId vid
                                                                     val env = insertVId (env, vid, vid')
                                                                 in (env, F.UnpackDec (tv', kind, vid', ty, exp))
                                                                 end
            | doDec env (F.IgnoreDec exp) = (env, F.IgnoreDec (doExp env exp))
            | doDec env (F.DatatypeDec datbinds)
              = let fun doDatBind (F.DatBind (tyvars, tyname, conbinds), (env, datbinds))
                        = let val tyname' = refreshTyVar tyname
                              val (env, conbinds) = List.foldr (fn (F.ConBind (vid, optTy), (env, conbinds)) =>
                                                                   let val vid' = refreshVId vid
                                                                   in (insertVId (env, vid, vid'), (vid', optTy) :: conbinds)
                                                                   end
                                                               ) (env, []) conbinds
                          in (insertTyVar (env, tyname, tyname'), (tyvars, tyname', conbinds) :: datbinds)
                          end
                    val (env, datbinds) = List.foldr doDatBind (env, []) datbinds
                in (env, F.DatatypeDec (List.map (fn (tyvars, tyname, conbinds) =>
                                                     let val tyvars = List.map (fn tv => (tv, refreshTyVar tv)) tyvars
                                                         val env' = List.foldl (fn ((tv, tv'), env) => insertTyVar (env, tv, tv')) env tyvars
                                                         fun doConBind (vid', optTy) = F.ConBind (vid', Option.map (doTy env') optTy)
                                                     in F.DatBind (List.map #2 tyvars, tyname, List.map doConBind conbinds)
                                                     end
                                                 ) datbinds))
                end
            | doDec env (F.ExceptionDec { conName, tagName, payloadTy })
              = let val conName' = refreshVId conName
                    val tagName' = refreshVId tagName
                    val payloadTy = Option.map (doTy env) payloadTy
                in (insertVId (insertVId (env, conName, conName'), tagName, tagName'), F.ExceptionDec { conName = conName', tagName = tagName', payloadTy = payloadTy })
                end
            | doDec env (F.ExceptionRepDec { conName, conPath, tagPath, payloadTy })
              = let val conName' = refreshVId conName
                    val payloadTy = Option.map (doTy env) payloadTy
                in (insertVId (env, conName, conName'), F.ExceptionRepDec { conName = conName', conPath = doPath env conPath, tagPath = doPath env tagPath, payloadTy = payloadTy })
                end
            | doDec env (F.ExportValue exp) = (env, F.ExportValue (doExp env exp))
            | doDec env (F.ExportModule xs) = (env, F.ExportModule (Vector.map (fn (label, exp) => (label, doExp env exp)) xs))
            | doDec env (F.GroupDec (set, decs)) = let val (env, decs) = doDecs env decs
                                                   in (env, F.GroupDec (NONE, decs))
                                                   end
          and doDecs env decs = let val (env, decs) = List.foldl (fn (dec, (env, decs)) => let val (env, dec) = doDec env dec
                                                                                           in (env, dec :: decs)
                                                                                           end
                                                                 ) (env, []) decs
                                in (env, List.rev decs)
                                end
      in { doTy = doTy
         , doPat = doPat
         , doExp = doExp
         , doDec = doDec
         , doDecs = doDecs
         }
      end
end
end

structure Inliner = struct
local structure F = FSyntax in
type Context = { nextVId : int ref, nextTyVar : int ref }
datatype InlineExp = VarExp of USyntax.VId
                   | StructExp of { valMap : F.Path Syntax.VIdMap.map
                                  , strMap : F.Path Syntax.StrIdMap.map
                                  , exnTagMap : F.Path Syntax.VIdMap.map
                                  , equalityMap : F.Path Syntax.TyConMap.map
                                  }
                   | SProjectionExp of InlineExp * F.SLabel
                   | FnExp of USyntax.VId * F.Ty * F.Exp
                   | TyAbsExp of F.TyVar * F.Kind * InlineExp
                   | TyAppExp of InlineExp * F.Ty
fun lookupSLabel ({ valMap, strMap, exnTagMap, equalityMap }, label) = case label of
                                                                           F.ValueLabel vid => Syntax.VIdMap.find(valMap, vid)
                                                                         | F.StructLabel strid => Syntax.StrIdMap.find(strMap, strid)
                                                                         | F.ExnTagLabel vid => Syntax.VIdMap.find(exnTagMap, vid)
                                                                         | F.EqualityLabel tycon => Syntax.TyConMap.find(equalityMap, tycon)
type Env = { valMap : InlineExp USyntax.VIdMap.map }
val emptyEnv : Env = { valMap = USyntax.VIdMap.empty }
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
fun removeFromEnv (vid, env as { valMap } : Env) = if USyntax.VIdMap.inDomain (valMap, vid) then
                                                       { valMap = #1 (USyntax.VIdMap.remove (valMap, vid)) }
                                                   else
                                                       env
fun costOfExp (F.PrimExp (primOp, tyargs, args)) = Vector.foldl (fn (exp, acc) => acc + costOfExp exp) 1 args
  | costOfExp (F.VarExp _) = 0
  | costOfExp (F.RecordExp fields) = List.foldl (fn ((label, exp), acc) => acc + costOfExp exp) 1 fields
  | costOfExp (F.LetExp (dec, exp)) = costOfDec dec + costOfExp exp
  | costOfExp (F.AppExp (e1, e2)) = costOfExp e1 + costOfExp e2
  | costOfExp (F.HandleExp { body, exnName, handler }) = 5 + costOfExp body + costOfExp handler
  | costOfExp (F.IfThenElseExp (e1, e2, e3)) = 1 + costOfExp e1 + costOfExp e2 + costOfExp e3
  | costOfExp (F.CaseExp (span, exp, ty, matches)) = List.foldl (fn ((pat, exp), acc) => acc + costOfExp exp) (costOfExp exp) matches
  | costOfExp (F.FnExp (vid, ty, exp)) = 1 + costOfExp exp
  | costOfExp (F.ProjectionExp { label, recordTy, fieldTy }) = 1
  | costOfExp (F.TyAbsExp (tv, kind, exp)) = costOfExp exp
  | costOfExp (F.TyAppExp (exp, ty)) = costOfExp exp
  | costOfExp (F.StructExp { valMap, strMap, exnTagMap, equalityMap }) = 1
  | costOfExp (F.SProjectionExp (exp, _)) = costOfExp exp
  | costOfExp (F.PackExp { payloadTy, exp, packageTy }) = costOfExp exp
and costOfDec (F.ValDec valbind) = costOfValBind valbind
  | costOfDec (F.RecValDec valbinds) = List.foldl (fn ((vid, ty, exp), acc) => acc + costOfExp exp) 0 valbinds
  | costOfDec (F.UnpackDec (tv, kind, vid, ty, exp)) = costOfExp exp
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
                                   fun doInlineExp (iexp as VarExp vid) = iexp
                                     | doInlineExp (iexp as StructExp _) = iexp
                                     | doInlineExp (SProjectionExp (exp, label)) = SProjectionExp (doInlineExp exp, label)
                                     | doInlineExp (FnExp (vid, ty, exp)) = FnExp (vid, doTy ty, doExp exp)
                                     | doInlineExp (TyAbsExp (tv, kind, iexp)) = if USyntax.TyVarMap.inDomain (subst, tv) then
                                                                                     TyAbsExp (tv, kind, substTyInInlineExp (#1 (USyntax.TyVarMap.remove (subst, tv))) iexp) (* TODO: use fresh tyvar if necessary *)
                                                                                 else
                                                                                     TyAbsExp (tv, kind, doInlineExp iexp)
                                     | doInlineExp (TyAppExp (iexp, ty)) = TyAppExp (doInlineExp iexp, doTy ty)
                               in doInlineExp
                               end
fun freeTyVarsInInlineExp (bound, VarExp _) = USyntax.TyVarSet.empty
  | freeTyVarsInInlineExp (bound, StructExp { valMap, strMap, exnTagMap, equalityMap }) = USyntax.TyVarSet.empty
  | freeTyVarsInInlineExp (bound, SProjectionExp (exp, label)) = freeTyVarsInInlineExp (bound, exp)
  | freeTyVarsInInlineExp (bound, FnExp (vid, ty, exp)) = USyntax.TyVarSet.union (F.freeTyVarsInTy (bound, ty), F.freeTyVarsInExp (bound, exp))
  | freeTyVarsInInlineExp (bound, TyAbsExp (tv, kind, exp)) = freeTyVarsInInlineExp (USyntax.TyVarSet.add (bound, tv), exp)
  | freeTyVarsInInlineExp (bound, TyAppExp (exp, ty)) = USyntax.TyVarSet.union (freeTyVarsInInlineExp (bound, exp), F.freeTyVarsInTy (bound, ty))
fun uninlineExp (VarExp vid) = F.VarExp vid
  | uninlineExp (StructExp { valMap, strMap, exnTagMap, equalityMap }) = F.StructExp { valMap = valMap, strMap = strMap, exnTagMap = exnTagMap, equalityMap = equalityMap }
  | uninlineExp (SProjectionExp (exp, label)) = F.SProjectionExp (uninlineExp exp, label)
  | uninlineExp (FnExp (vid, ty, exp)) = F.FnExp (vid, ty, exp)
  | uninlineExp (TyAbsExp (tv, kind, iexp)) = F.TyAbsExp (tv, kind, uninlineExp iexp)
  | uninlineExp (TyAppExp (iexp, ty)) = F.TyAppExp (uninlineExp iexp, ty)
fun PathToInlineExp (F.Root vid) = VarExp vid
  | PathToInlineExp (F.Child (parent, label)) = SProjectionExp (PathToInlineExp parent, label)
fun run (ctx : Context) : { doExp : Env -> F.Exp -> F.Exp
                          , doDec : Env -> F.Dec -> (* modified environment *) Env * F.Dec
                          , doDecs : Env -> F.Dec list -> (* modified environment *) Env * F.Dec list
                          }
    = let fun doPath (env : Env) (path as F.Root vid) = (case USyntax.VIdMap.find (#valMap env, vid) of
                                                             SOME (VarExp vid) => doPath env (F.Root vid)
                                                           | SOME iexp => (path, SOME iexp)
                                                           | NONE => (path, NONE)
                                                        )
            | doPath env (F.Child (parent, label)) = (case doPath env parent of
                                                          (_, SOME (StructExp m)) =>
                                                          (case lookupSLabel(m, label) of
                                                               SOME path => doPath env path
                                                             | NONE => (F.Child (parent, label), NONE) (* should be an error *)
                                                          )
                                                        | (parent, SOME iexp) => (F.Child (parent, label), SOME (SProjectionExp (iexp, label)))
                                                        | (parent, NONE) => (F.Child (parent, label), NONE)
                                                     )
          fun evalPath env (path as F.Root vid) = (case USyntax.VIdMap.find (#valMap env, vid) of
                                                       SOME iexp => iexp
                                                     | NONE => PathToInlineExp path
                                                  )
            | evalPath env (F.Child (parent, label)) = (case evalPath env parent of
                                                            StructExp m =>
                                                            (case lookupSLabel (m, label) of
                                                                 SOME path => evalPath env path
                                                               | NONE => raise Fail "evalPath"
                                                            )
                                                          | iexp => SProjectionExp (iexp, label)
                                                       )
          fun doExp env exp = #1 (doExp' env exp)
          and doExp' (env : Env) exp0 : F.Exp * InlineExp option
              = (case exp0 of
                     F.PrimExp (primOp, tyargs, args) => (F.PrimExp (primOp, tyargs, Vector.map (doExp env) args), NONE)
                   | F.VarExp vid => (case USyntax.VIdMap.find (#valMap env, vid) of
                                          SOME (VarExp vid) => (F.VarExp vid, SOME (evalPath env (F.Root vid)))
                                        | iexpOpt as SOME _ => (exp0, iexpOpt)
                                        | NONE => (exp0, SOME (VarExp vid))
                                     )
                   | F.RecordExp fields => (F.RecordExp (List.map (fn (label, exp) => (label, doExp env exp)) fields), NONE)
                   | F.LetExp (dec, exp) => let val (env, dec) = doDec env dec
                                            in (F.LetExp (dec, doExp env exp), NONE)
                                            end
                   | F.AppExp (exp1, exp2) => let val (exp1, iexp1) = doExp' env exp1
                                              in case iexp1 of
                                                     SOME (FnExp (vid, paramTy, body)) => let val vid' = RefreshBoundNames.refreshVId ctx vid
                                                                                              val body = #doExp (RefreshBoundNames.run ctx) (RefreshBoundNames.insertVId (RefreshBoundNames.emptyEnv, vid, vid')) body
                                                                                          in doExp' env (F.LetExp (F.ValDec (F.SimpleBind (vid', paramTy, exp2)), body))
                                                                                          end
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
                   | F.IfThenElseExp (exp1, exp2, exp3) => (F.IfThenElseExp (doExp env exp1, doExp env exp2, doExp env exp3), NONE)
                   | F.CaseExp (span, exp, ty, matches) => let fun doMatch (pat, exp) = let val vars = freeVarsInPat pat
                                                                                            val env' = { valMap = USyntax.VIdMap.filteri (fn (vid, _) => not (USyntax.VIdSet.member (vars, vid))) (#valMap env) }
                                                                                        in (pat, doExp env' exp)
                                                                                        end
                                                           in (F.CaseExp (span, doExp env exp, ty, List.map doMatch matches), NONE)
                                                           end
                   | F.FnExp (vid, ty, exp) => let val env' = removeFromEnv (vid, env)
                                                   val exp' = doExp env' exp
                                               in (F.FnExp (vid, ty, exp'), if costOfExp exp' <= INLINE_THRESHOLD then SOME (FnExp (vid, ty, exp')) else NONE)
                                               end
                   | F.ProjectionExp { label, recordTy, fieldTy } => (exp0, NONE)
                   | F.TyAbsExp (tv, kind, exp) => let val (exp, iexp) = doExp' env exp
                                                       val tryEtaReductionI = case iexp of
                                                                                  SOME (TyAppExp (exp', F.TyVar tv')) => if tv = tv' then
                                                                                                                             let val fv = freeTyVarsInInlineExp (USyntax.TyVarSet.empty, exp')
                                                                                                                             in if USyntax.TyVarSet.member (fv, tv) then
                                                                                                                                    NONE
                                                                                                                                else
                                                                                                                                    SOME (uninlineExp exp', SOME exp')
                                                                                                                             end
                                                                                                                         else
                                                                                                                             NONE
                                                                                | _ => NONE
                                                       val tryEtaReduction = case tryEtaReductionI of
                                                                                 SOME result => tryEtaReductionI
                                                                               | NONE => case exp of
                                                                                             F.TyAppExp(exp', F.TyVar tv') => if tv = tv' then
                                                                                                                                  let val fv = F.freeTyVarsInExp (USyntax.TyVarSet.empty, exp')
                                                                                                                                  in if USyntax.TyVarSet.member (fv, tv) then
                                                                                                                                         NONE
                                                                                                                                     else
                                                                                                                                         SOME (doExp' env exp')
                                                                                                                                  end
                                                                                                                              else
                                                                                                                                  NONE
                                                                                           | _ => NONE
                                                   in case tryEtaReduction of
                                                          SOME result => result
                                                        | NONE => (F.TyAbsExp (tv, kind, exp), case iexp of
                                                                                                   SOME iexp => SOME (TyAbsExp (tv, kind, iexp)) (* TODO: hoisting? *)
                                                                                                 | NONE => NONE)
                                                   end
                   | F.TyAppExp (exp, ty) => let val (exp, iexp) = doExp' env exp
                                             in case iexp of
                                                    SOME (TyAbsExp (tv, kind, exp)) => let val iexp = substTyInInlineExp (USyntax.TyVarMap.singleton (tv, ty)) exp
                                                                                           val exp = #doExp (RefreshBoundNames.run ctx) RefreshBoundNames.emptyEnv (uninlineExp iexp)
                                                                                       in (exp, SOME iexp)
                                                                                       end
                                                  | SOME iexp => (F.TyAppExp (exp, ty), SOME (TyAppExp (iexp, ty)))
                                                  | NONE => (case exp of
                                                                 F.TyAbsExp (tv, kind, exp') => let val substExp = #doExp (F.substTy (USyntax.TyVarMap.singleton (tv, ty)))
                                                                                                in substExp exp'
                                                                                                end
                                                               | _ => F.TyAppExp (exp, ty), NONE)
                                             end
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
                                                           (exp, SOME (StructExp m)) => (case lookupSLabel(m, label) of
                                                                                             SOME path => let val iexp = evalPath env path
                                                                                                          in (uninlineExp iexp, SOME iexp)
                                                                                                          end
                                                                                           | NONE => (F.SProjectionExp (exp, label), NONE) (* should be an error *)
                                                                                        )
                                                         | (exp, SOME iexp) => (F.SProjectionExp (exp, label), SOME (SProjectionExp (iexp, label)))
                                                         | (exp, NONE) => (F.SProjectionExp (exp, label), NONE)
                                                      )
                   | F.PackExp { payloadTy, exp, packageTy } => (F.PackExp { payloadTy = payloadTy, exp = doExp env exp, packageTy = packageTy }, NONE)
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
            | doDec env (F.UnpackDec (tv, kind, vid, ty, exp)) = (removeFromEnv (vid, env), F.UnpackDec (tv, kind, vid, ty, doExp env exp))
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
                                                                               SOME iexp => { valMap = USyntax.VIdMap.insert (#valMap env, vid, iexp) }
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
end (* structure Inliner *)

structure Fuse = struct
local structure F = FSyntax in
type Context = { nextVId : int ref, nextTyVar : int ref }
type Env = {}
val emptyEnv : Env = {}
fun fuse (ctx : Context) : { doExp : Env -> F.Exp -> F.Exp
                           , doDec : Env -> F.Dec -> (* modified environment *) Env * F.Dec
                           , doDecs : Env -> F.Dec list -> (* modified environment *) Env * F.Dec list
                           }
    = let fun doExp (env : Env) exp0
              = (case exp0 of
                     F.PrimExp (primOp, tyargs, args) => F.PrimExp (primOp, tyargs, Vector.map (doExp env) args)
                   | F.VarExp _ => exp0
                   | F.RecordExp fields => F.RecordExp (List.map (fn (label, exp) => (label, doExp env exp)) fields)
                   | F.LetExp (dec, exp) => let val (env', dec') = doDec env dec
                                            in F.LetExp (dec', doExp env' exp)
                                            end
                   | F.AppExp (exp1, exp2) => (case (doExp env exp1, doExp env exp2) of
                                                   (exp1 as F.TyAppExp (exp1', ty1), exp2 as F.PrimExp (F.ListOp, ty2, xs)) =>
                                                   if F.isLongVId(exp1', InitialEnv.VId_Vector_fromList) andalso Vector.length ty2 = 1 then
                                                       F.VectorExp (xs, Vector.sub (ty2, 0))
                                                   else
                                                       F.AppExp (exp1, exp2)
                                                 | (exp1, exp2) => F.AppExp (exp1, exp2)
                                              )
                   | F.HandleExp { body, exnName, handler } => F.HandleExp { body = doExp env body
                                                                           , exnName = exnName
                                                                           , handler = doExp env handler
                                                                           }
                   | F.IfThenElseExp (exp1, exp2, exp3) => F.IfThenElseExp (doExp env exp1, doExp env exp2, doExp env exp3)
                   | F.CaseExp (span, exp, ty, matches) => let fun doMatch (pat, exp) = (pat, doExp env exp)
                                                           in F.CaseExp (span, doExp env exp, ty, List.map doMatch matches)
                                                           end
                   | F.FnExp (vid, ty, exp) => F.FnExp (vid, ty, doExp env exp)
                   | F.ProjectionExp { label, recordTy, fieldTy } => exp0
                   | F.TyAbsExp (tyvar, kind, exp) => F.TyAbsExp (tyvar, kind, doExp env exp)
                   | F.TyAppExp (exp, ty) => (case doExp env exp of
                                                  F.TyAbsExp(tyvar, kind, exp') => let val substExp = #doExp (F.substTy (USyntax.TyVarMap.singleton (tyvar, ty)))
                                                                                   in substExp exp'
                                                                                   end
                                                | exp => F.TyAppExp (doExp env exp, ty)
                                             )
                   | F.StructExp { valMap, strMap, exnTagMap, equalityMap } => F.StructExp { valMap = valMap
                                                                                           , strMap = strMap
                                                                                           , exnTagMap = exnTagMap
                                                                                           , equalityMap = equalityMap
                                                                                           }
                   | F.SProjectionExp (exp, label) => F.SProjectionExp (doExp env exp, label)
                   | F.PackExp { payloadTy, exp, packageTy } => F.PackExp { payloadTy = payloadTy, exp = doExp env exp, packageTy = packageTy }
                )
          and doDec (env : Env) (F.ValDec valbind) = (env, F.ValDec (doValBind env valbind))
            | doDec env (F.RecValDec valbinds) = (env, F.RecValDec (List.map (fn (v, ty, exp) => (v, ty, doExp env exp)) valbinds))
            | doDec env (F.UnpackDec (tv, kind, vid, ty, exp)) = (env, F.UnpackDec (tv, kind, vid, ty, doExp env exp))
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
fun doExp (F.PrimExp (primOp, tyargs, args)) = F.PrimExp (primOp, tyargs, Vector.map doExp args)
  | doExp (exp as F.VarExp _) = exp
  | doExp (F.RecordExp fields) = F.RecordExp (List.map (fn (label, exp) => (label, doExp exp)) fields)
  | doExp (F.LetExp (dec, exp2)) = F.LetExp (doDec dec, doExp exp2)
  | doExp (F.AppExp (exp1, exp2)) = F.AppExp (doExp exp1, doExp exp2)
  | doExp (F.HandleExp { body, exnName, handler }) = F.HandleExp { body = doExp body, exnName = exnName, handler = doExp handler }
  | doExp (F.IfThenElseExp (exp1, exp2, exp3)) = F.IfThenElseExp (doExp exp1, doExp exp2, doExp exp3)
  | doExp (F.CaseExp (span, exp, ty, matches)) = F.CaseExp (span, doExp exp, ty, List.map (fn (pat, exp) => (pat, doExp exp)) matches)
  | doExp (F.FnExp (vid, ty, exp)) = F.FnExp (vid, ty, doExp exp)
  | doExp (exp as F.ProjectionExp _) = exp
  | doExp (F.TyAbsExp (tv, kind, exp)) = F.TyAbsExp (tv, kind, doExp exp)
  | doExp (F.TyAppExp (exp, ty)) = F.TyAppExp (doExp exp, ty)
  | doExp (F.StructExp { valMap, strMap, exnTagMap, equalityMap }) = F.StructExp { valMap = valMap
                                                                                 , strMap = strMap
                                                                                 , exnTagMap = exnTagMap
                                                                                 , equalityMap = equalityMap
                                                                                 }
  | doExp (F.SProjectionExp (exp, label)) = F.SProjectionExp (doExp exp, label)
  | doExp (F.PackExp { payloadTy, exp, packageTy }) = F.PackExp { payloadTy = payloadTy, exp = doExp exp, packageTy = packageTy }
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
  | doDec (F.UnpackDec (tv, kind, vid, ty, exp)) = F.UnpackDec (tv, kind, vid, ty, doExp exp)
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
           , inliner : Inliner.Env
           , fuse : Fuse.Env
           }
val initialEnv : Env = { desugarPatternMatches = DesugarPatternMatches.initialEnv
                       , inliner = Inliner.emptyEnv
                       , fuse = Fuse.emptyEnv
                       }
fun doDecs ctx (env : Env) decs = let val (dpEnv, decs) = #doDecs (DesugarPatternMatches.desugarPatternMatches ctx) (#desugarPatternMatches env) decs
                                      val decs = DecomposeValRec.doDecs decs
                                      val decs = FlattenLet.doDecs decs
                                      val (inlinerEnv, decs) = #doDecs (Inliner.run ctx) (#inliner env) decs
                                      val (fuseEnv, decs) = #doDecs (Fuse.fuse ctx) (#fuse env) decs
                              in ({desugarPatternMatches = dpEnv, inliner = inlinerEnv, fuse = fuseEnv}, decs)
                              end
end (* structure FTransform *)

structure DeadCodeElimination = struct
structure F = FSyntax
fun isDiscardablePrimOp (F.SConOp _) = true
  | isDiscardablePrimOp (F.RaiseOp _) = false
  | isDiscardablePrimOp F.ListOp = true
  | isDiscardablePrimOp F.VectorOp = true
  | isDiscardablePrimOp F.RecordEqualityOp = true
  | isDiscardablePrimOp F.DataTagOp = true
  | isDiscardablePrimOp F.DataPayloadOp = true
fun isDiscardable (F.PrimExp (primOp, tyargs, args)) = isDiscardablePrimOp primOp andalso Vector.all isDiscardable args
  | isDiscardable (F.VarExp _) = true
  | isDiscardable (F.RecordExp fields) = List.all (fn (label, exp) => isDiscardable exp) fields
  | isDiscardable (F.LetExp (dec, exp)) = false (* TODO *)
  | isDiscardable (F.AppExp (F.TyAppExp (exp1', _), exp2)) = F.isLongVId (exp1', InitialEnv.VId_assumePure) orelse F.isLongVId (exp1', InitialEnv.VId_assumeDiscardable)
  | isDiscardable (F.AppExp (exp1, exp2)) = false (* TODO *)
  | isDiscardable (F.HandleExp { body, exnName, handler }) = false (* TODO *)
  | isDiscardable (F.IfThenElseExp (exp1, exp2, exp3)) = isDiscardable exp1 andalso isDiscardable exp2 andalso isDiscardable exp3
  | isDiscardable (F.CaseExp (span, exp, ty, matches)) = false (* TODO *)
  | isDiscardable (F.FnExp (vid, ty, exp)) = true
  | isDiscardable (F.ProjectionExp { label, recordTy, fieldTy }) = true
  | isDiscardable (F.TyAbsExp (tyvar, kind, exp)) = isDiscardable exp
  | isDiscardable (F.TyAppExp (exp, ty)) = isDiscardable exp
  | isDiscardable (F.StructExp { valMap, strMap, exnTagMap, equalityMap }) = true
  | isDiscardable (F.SProjectionExp (exp, label)) = isDiscardable exp
  | isDiscardable (F.PackExp { payloadTy, exp, packageTy }) = isDiscardable exp
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
fun doExp (F.PrimExp (primOp, tyargs, args) : F.Exp) : USyntax.VIdSet.set * F.Exp
    = let val args' = Vector.map doExp args
      in (Vector.foldl USyntax.VIdSet.union USyntax.VIdSet.empty (Vector.map #1 args'), F.PrimExp (primOp, tyargs, Vector.map #2 args'))
      end
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
  | doExp (F.TyAbsExp (tyvar, kind, exp)) = let val (used, exp) = doExp exp
                                            in (used, F.TyAbsExp (tyvar, kind, exp))
                                            end
  | doExp (F.TyAppExp (exp, ty)) = let val (used, exp) = doExp exp
                                   in (used, F.TyAppExp (exp, ty))
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
  | doExp (F.PackExp { payloadTy, exp, packageTy }) = let val (used, exp) = doExp exp
                                                      in (used, F.PackExp { payloadTy = payloadTy, exp = exp, packageTy = packageTy })
                                                      end
and doIgnoredExpAsExp exp = let val (used, exps) = doIgnoredExp exp
                            in (used, List.foldr (fn (e1, e2) => F.LetExp (F.IgnoreDec e1, e2)) (F.RecordExp []) exps)
                            end
(* doIgnoredExp : F.Exp -> USyntax.VIdSet.set * F.Exp list *)
and doIgnoredExp (exp as F.PrimExp (primOp, tyargs, args))
    = if isDiscardablePrimOp primOp then
          let val args' = Vector.map doIgnoredExp args
          in (Vector.foldl (fn ((used, _), acc) => USyntax.VIdSet.union (used, acc)) USyntax.VIdSet.empty args', Vector.foldr (fn ((_, e), xs) => e @ xs) [] args')
          end
      else
          let val (used, exp) = doExp exp
          in (used, [exp])
          end
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
  | doIgnoredExp (F.StructExp { valMap, strMap, exnTagMap, equalityMap }) = (USyntax.VIdSet.empty, [])
  | doIgnoredExp (F.SProjectionExp (exp, label)) = doIgnoredExp exp
  | doIgnoredExp (F.PackExp { payloadTy, exp, packageTy }) = let val (used, exp) = doIgnoredExpAsExp exp
                                                             in case exp of
                                                                    F.RecordExp [] => (used, [])
                                                                  | exp => (used, [F.PackExp { payloadTy = payloadTy, exp = exp, packageTy = packageTy }])
                                                             end
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
  | doDec (used, F.UnpackDec (tv, kind, vid, ty, exp))
    = if not (USyntax.VIdSet.member (used, vid)) then
          if isDiscardable exp then
              (used, [])
          else
              let val (used', exps) = doIgnoredExp exp
              in (USyntax.VIdSet.union (used, used'), List.map F.IgnoreDec exps)
              end
      else
          let val (used', exp') = doExp exp
          in (USyntax.VIdSet.union (used, used'), [F.UnpackDec (tv, kind, vid, ty, exp')])
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
  | definedInDec (F.UnpackDec (tv, kind, vid, ty, exp)) = USyntax.VIdSet.singleton vid
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
