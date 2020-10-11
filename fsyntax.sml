structure FSyntax = struct
type TyVar = USyntax.TyVar
type TyCon = USyntax.TyCon
type LongTyCon = USyntax.LongTyCon
datatype Ty = TyVar of TyVar
            | RecordType of (Syntax.Label * Ty) list
            | TyCon of Ty list * LongTyCon
            | FnType of Ty * Ty
            | ForallType of TyVar * Ty
datatype Pat = WildcardPat
             | SConPat of Syntax.SCon
             | VarPat of USyntax.VId * Ty
             | RecordPat of (Syntax.Label * Pat) list * bool
             | InstantiatedConPat of USyntax.LongVId * Pat option * Ty list
             | LayeredPat of USyntax.VId * Ty * Pat
(* datatype DatBind = DatBind of TyVar list * TyCon * ConBind list *)
(* datatype ExBind = ExBind of Syntax.VId of Ty option *)
datatype Exp = SConExp of Syntax.SCon
             | VarExp of USyntax.LongVId
             | RecordExp of (Syntax.Label * Exp) list
             | LetExp of ValBind * Exp
             | LetRecExp of ValBind list * Exp
             | AppExp of Exp * Exp
             (* | HandleExp of Exp * (Pat * Exp) list *)
             (* | RaiseExp of Exp *)
             | IfThenElseExp of Exp * Exp * Exp
             | CaseExp of Exp * (Pat * Exp) list
             | FnExp of USyntax.VId * Ty * Exp
             | ProjectionExp of { label : Syntax.Label, recordTy : Ty, fieldTy : Ty }
             | TyAbsExp of TyVar * Exp
             | TyAppExp of Exp * Ty
             | RecordEqualityExp of (Syntax.Label * Exp) list
     and ValBind = TupleBind of (USyntax.VId * Ty) list * Exp
datatype Dec = ValDec of ValBind
             | RecValDec of ValBind list
fun PairType(a, b) = RecordType [(Syntax.NumericLabel 1, a), (Syntax.NumericLabel 2, b)]

structure PrettyPrint = struct
val print_TyVar = USyntax.print_TyVar
val print_VId = USyntax.print_VId
val print_LongVId = USyntax.print_LongVId
val print_LongTyCon = USyntax.print_LongTyCon
fun print_Ty (TyVar x) = "TyVar(" ^ print_TyVar x ^ ")"
  | print_Ty (RecordType xs) = (case Syntax.extractTuple (1, xs) of
                                    NONE => "RecordType " ^ Syntax.print_list (Syntax.print_pair (Syntax.print_Label,print_Ty)) xs
                                  | SOME ys => "TupleType " ^ Syntax.print_list print_Ty ys
                               )
  | print_Ty (TyCon(x,y)) = "TyCon(" ^ Syntax.print_list print_Ty x ^ "," ^ print_LongTyCon y ^ ")"
  | print_Ty (FnType(x,y)) = "FnType(" ^ print_Ty x ^ "," ^ print_Ty y ^ ")"
  | print_Ty (ForallType(tv,x)) = "ForallType(" ^ print_TyVar tv ^ "," ^ print_Ty x ^ ")"
fun print_Pat WildcardPat = "WildcardPat"
  | print_Pat (SConPat x) = "SConPat(" ^ Syntax.print_SCon x ^ ")"
  | print_Pat (VarPat(vid, ty)) = "VarPat(" ^ print_VId vid ^ "," ^ print_Ty ty ^ ")"
  | print_Pat (LayeredPat (vid, ty, pat)) = "TypedPat(" ^ print_VId vid ^ "," ^ print_Ty ty ^ "," ^ print_Pat pat ^ ")"
  | print_Pat (InstantiatedConPat(longvid, pat, tyargs)) = "InstantiatedConPat(" ^ print_LongVId longvid ^ "," ^ Syntax.print_option print_Pat pat ^ "," ^ Syntax.print_list print_Ty tyargs ^ ")"
  | print_Pat (RecordPat(x, false)) = (case Syntax.extractTuple (1, x) of
                                           NONE => "RecordPat(" ^ Syntax.print_list (Syntax.print_pair (Syntax.print_Label, print_Pat)) x ^ ",false)"
                                         | SOME ys => "TuplePat " ^ Syntax.print_list print_Pat ys
                                      )
  | print_Pat (RecordPat(x, true)) = "RecordPat(" ^ Syntax.print_list (Syntax.print_pair (Syntax.print_Label, print_Pat)) x ^ ",true)"
fun print_Exp (SConExp x) = "SConExp(" ^ Syntax.print_SCon x ^ ")"
  | print_Exp (VarExp(Syntax.MkQualified([], vid))) = "SimpleVarExp(" ^ print_VId vid ^ ")"
  | print_Exp (VarExp(x)) = "VarExp(" ^ print_LongVId x ^ ")"
  | print_Exp (RecordExp x) = (case Syntax.extractTuple (1, x) of
                                   NONE => "RecordExp " ^ Syntax.print_list (Syntax.print_pair (Syntax.print_Label, print_Exp)) x
                                 | SOME ys => "TupleExp " ^ Syntax.print_list print_Exp ys
                              )
  | print_Exp (LetExp(valbind,x)) = "LetExp(" ^ print_ValBind valbind ^ "," ^ print_Exp x ^ ")"
  | print_Exp (LetRecExp(valbinds,x)) = "LetRecExp(" ^ Syntax.print_list print_ValBind valbinds ^ "," ^ print_Exp x ^ ")"
  | print_Exp (AppExp(x,y)) = "AppExp(" ^ print_Exp x ^ "," ^ print_Exp y ^ ")"
  | print_Exp (IfThenElseExp(x,y,z)) = "IfThenElseExp(" ^ print_Exp x ^ "," ^ print_Exp y ^ "," ^ print_Exp z ^ ")"
  | print_Exp (CaseExp(x,y)) = "CaseExp(" ^ print_Exp x ^ "," ^ Syntax.print_list (Syntax.print_pair (print_Pat,print_Exp)) y ^ ")"
  | print_Exp (FnExp(pname,pty,body)) = "FnExp(" ^ print_VId pname ^ "," ^ print_Ty pty ^ "," ^ print_Exp body ^ ")"
  | print_Exp (ProjectionExp { label = label, recordTy = recordTy, fieldTy = fieldTy }) = "ProjectionExp{label=" ^ Syntax.print_Label label ^ ",recordTy=" ^ print_Ty recordTy ^ ",fieldTy=" ^ print_Ty fieldTy ^ "}"
  | print_Exp (TyAbsExp(tv, exp)) = "TyAbsExp(" ^ print_TyVar tv ^ "," ^ print_Exp exp ^ ")"
  | print_Exp (TyAppExp(exp, ty)) = "TyAppExp(" ^ print_Exp exp ^ "," ^ print_Ty ty ^ ")"
  | print_Exp (RecordEqualityExp(fields)) = "RecordEqualityExp(" ^ Syntax.print_list (Syntax.print_pair (Syntax.print_Label, print_Exp)) fields ^ ")"
and print_ValBind (TupleBind (xs, exp)) = "TupleBind(" ^ Syntax.print_list (Syntax.print_pair (print_VId, print_Ty)) xs ^ "," ^ print_Exp exp ^ ")"
fun print_Dec (ValDec (valbind)) = "ValDec(" ^ print_ValBind valbind ^ ")"
  | print_Dec (RecValDec (valbinds)) = "RecValDec(" ^ Syntax.print_list print_ValBind valbinds ^ ")"
end (* structure PrettyPrint *)
end (* structure FSyntax *)

structure ToFSyntax = struct
type Context = { nextVId : int ref
               }
datatype Env = MkEnv of { valMap : {} USyntax.VIdMap.map
                        , tyConMap : FSyntax.TyCon USyntax.TyConMap.map
                        , equalityForTyVarMap : USyntax.VId USyntax.TyVarMap.map
                        , strMap : Env Syntax.StrIdMap.map
                        }
val emptyEnv = MkEnv { valMap = USyntax.VIdMap.empty
                     , tyConMap = USyntax.TyConMap.empty
                     , equalityForTyVarMap = USyntax.TyVarMap.empty
                     , strMap = Syntax.StrIdMap.empty
                     }
fun mergeEnv(MkEnv env1 : Env, MkEnv env2 : Env)
    = MkEnv { valMap = USyntax.VIdMap.unionWith #2 (#valMap env1, #valMap env2)
            , tyConMap = USyntax.TyConMap.unionWith #2 (#tyConMap env1, #tyConMap env2)
            , equalityForTyVarMap = USyntax.TyVarMap.unionWith #2 (#equalityForTyVarMap env1, #equalityForTyVarMap env2)
            , strMap = Syntax.StrIdMap.unionWith #2 (#strMap env1, #strMap env2)
            }

fun updateEqualityForTyVarMap(f, MkEnv r) = MkEnv { valMap = #valMap r
                                                  , tyConMap = #tyConMap r
                                                  , equalityForTyVarMap = f (#equalityForTyVarMap r)
                                                  , strMap = #strMap r
                                                  }

fun envWithValEnv valEnv = MkEnv { valMap = valEnv
                                 , tyConMap = USyntax.TyConMap.empty
                                 , equalityForTyVarMap = USyntax.TyVarMap.empty
                                 , strMap = Syntax.StrIdMap.empty
                                 }

fun freshVId(ctx : Context, name: string) = let val n = !(#nextVId ctx)
                                            in #nextVId ctx := n + 1
                                             ; USyntax.MkVId(name, n)
                                            end

local structure U = USyntax
      structure F = FSyntax
      (* toFTy : Context * Env * USyntax.Ty -> FSyntax.Ty *)
      (* toFPat : Context * Env * USyntax.Pat -> unit USyntax.VIdMap.map * FSyntax.Pat *)
      (* toFExp : Context * Env * USyntax.Exp -> FSyntax.Exp *)
      (* toFDecs : Context * Env * USyntax.Dec list -> Env * FSyntax.Dec list *)
      (* getEquality : Context * Env * USyntax.Ty -> FSyntax.Exp *)
      fun isSimpleTy(U.TyCon([], longtycon1), longtycon2) = U.eqULongTyCon(longtycon1, longtycon2)
        | isSimpleTy _ = false
in
fun toFTy(ctx : Context, env : Env, U.TyVar tv) = F.TyVar tv
  | toFTy(ctx, env, U.RecordType fields) = let fun doField(label, ty) = (label, toFTy(ctx, env, ty))
                                           in F.RecordType (List.map doField fields)
                                           end
  | toFTy(ctx, env, U.TyCon(tyargs, longtycon)) = let fun doTy ty = toFTy(ctx, env, ty)
                                                  in F.TyCon(List.map doTy tyargs, longtycon)
                                                  end
  | toFTy(ctx, env, U.FnType(paramTy, resultTy)) = let fun doTy ty = toFTy(ctx, env, ty)
                                                   in F.FnType(doTy paramTy, doTy resultTy)
                                                   end
and toFPat(ctx, env, U.WildcardPat) = (USyntax.VIdMap.empty, F.WildcardPat)
  | toFPat(ctx, env, U.SConPat(scon)) = (USyntax.VIdMap.empty, F.SConPat(scon))
  | toFPat(ctx, env, U.VarPat(vid, ty)) = (USyntax.VIdMap.empty, F.VarPat(vid, toFTy(ctx, env, ty))) (* TODO *)
  | toFPat(ctx, env, U.RecordPat(fields, ellipsis)) = let fun doField(label, pat) = let val (_, pat') = toFPat(ctx, env, pat)
                                                                                    in (label, pat')
                                                                                    end
                                                      in (USyntax.VIdMap.empty, F.RecordPat(List.map doField fields, ellipsis)) (* TODO *)
                                                      end
  | toFPat(ctx, env, U.ConPat(longvid, optpat)) = toFPat(ctx, env, U.InstantiatedConPat(longvid, optpat, [])) (* should not reach here *)
  | toFPat(ctx, env, U.InstantiatedConPat(longvid, NONE, tyargs)) = (USyntax.VIdMap.empty, F.InstantiatedConPat(longvid, NONE, List.map (fn ty => toFTy(ctx, env, ty)) tyargs))
  | toFPat(ctx, env, U.InstantiatedConPat(longvid, SOME payloadPat, tyargs)) = let val (m, payloadPat') = toFPat(ctx, env, payloadPat)
                                                                               in (USyntax.VIdMap.empty, F.InstantiatedConPat(longvid, SOME payloadPat', List.map (fn ty => toFTy(ctx, env, ty)) tyargs))
                                                                               end
  | toFPat(ctx, env, U.TypedPat(pat, _)) = toFPat(ctx, env, pat)
  | toFPat(ctx, env, U.LayeredPat(vid, ty, innerPat)) = let val (m, innerPat') = toFPat(ctx, env, innerPat)
                                                        in (USyntax.VIdMap.empty, F.LayeredPat(vid, toFTy(ctx, env, ty), innerPat')) (* TODO *)
                                                        end
and toFExp(ctx, env, U.SConExp(scon)) = F.SConExp(scon)
  | toFExp(ctx, env, U.VarExp(longvid, _)) = F.VarExp(longvid)
  | toFExp(ctx, env, U.InstantiatedVarExp(longvid as Syntax.MkQualified([], vid), _, [tyarg]))
    = let open InitialEnv
      in if U.eqVId(vid, VId_EQUAL) then
             getEquality(ctx, env, tyarg)
         else if U.eqVId(vid, VId_abs) then
             if isSimpleTy(tyarg, Typing.primTyCon_int) then
                 F.VarExp(Syntax.MkQualified([], VId_abs_int))
             else if isSimpleTy(tyarg, Typing.primTyCon_real) then
                 F.VarExp(Syntax.MkQualified([], VId_abs_real))
             else
                 raise Fail "invalid use of abs"
         else if U.eqVId(vid, VId_TILDE) then
             if isSimpleTy(tyarg, Typing.primTyCon_int) then
                 F.VarExp(Syntax.MkQualified([], VId_TILDE_int))
             else if isSimpleTy(tyarg, Typing.primTyCon_real) then
                 F.VarExp(Syntax.MkQualified([], VId_TILDE_real))
             else
                 raise Fail "invalid use of ~ operator"
         else if U.eqVId(vid, VId_div) then
             if isSimpleTy(tyarg, Typing.primTyCon_int) then
                 F.VarExp(Syntax.MkQualified([], VId_div_int))
             else if isSimpleTy(tyarg, Typing.primTyCon_word) then
                 F.VarExp(Syntax.MkQualified([], VId_div_word))
             else
                 raise Fail "invalid use of div operator"
         else if U.eqVId(vid, VId_mod) then
             if isSimpleTy(tyarg, Typing.primTyCon_int) then
                 F.VarExp(Syntax.MkQualified([], VId_mod_int))
             else if isSimpleTy(tyarg, Typing.primTyCon_word) then
                 F.VarExp(Syntax.MkQualified([], VId_mod_word))
             else
                 raise Fail "invalid use of mod operator"
         else if U.eqVId(vid, VId_TIMES) then
             if isSimpleTy(tyarg, Typing.primTyCon_int) then
                 F.VarExp(Syntax.MkQualified([], VId_TIMES_int))
             else if isSimpleTy(tyarg, Typing.primTyCon_word) then
                 F.VarExp(Syntax.MkQualified([], VId_TIMES_word))
             else if isSimpleTy(tyarg, Typing.primTyCon_real) then
                 F.VarExp(Syntax.MkQualified([], VId_TIMES_real))
             else
                 raise Fail "invalid use of * operator"
         else if U.eqVId(vid, VId_DIVIDE) then
             if isSimpleTy(tyarg, Typing.primTyCon_real) then
                 F.VarExp(Syntax.MkQualified([], VId_DIVIDE_real))
             else
                 raise Fail "invalid use of / operator"
         else if U.eqVId(vid, VId_PLUS) then
             if isSimpleTy(tyarg, Typing.primTyCon_int) then
                 F.VarExp(Syntax.MkQualified([], VId_PLUS_int))
             else if isSimpleTy(tyarg, Typing.primTyCon_word) then
                 F.VarExp(Syntax.MkQualified([], VId_PLUS_word))
             else if isSimpleTy(tyarg, Typing.primTyCon_real) then
                 F.VarExp(Syntax.MkQualified([], VId_PLUS_real))
             else
                 raise Fail "invalid use of + operator"
         else if U.eqVId(vid, VId_MINUS) then
             if isSimpleTy(tyarg, Typing.primTyCon_int) then
                 F.VarExp(Syntax.MkQualified([], VId_MINUS_int))
             else if isSimpleTy(tyarg, Typing.primTyCon_word) then
                 F.VarExp(Syntax.MkQualified([], VId_MINUS_word))
             else if isSimpleTy(tyarg, Typing.primTyCon_real) then
                 F.VarExp(Syntax.MkQualified([], VId_MINUS_real))
             else
                 raise Fail "invalid use of - operator"
         else if U.eqVId(vid, VId_LT) then
             if isSimpleTy(tyarg, Typing.primTyCon_int) then
                 F.VarExp(Syntax.MkQualified([], VId_LT_int))
             else if isSimpleTy(tyarg, Typing.primTyCon_word) then
                 F.VarExp(Syntax.MkQualified([], VId_LT_word))
             else if isSimpleTy(tyarg, Typing.primTyCon_real) then
                 F.VarExp(Syntax.MkQualified([], VId_LT_real))
             else if isSimpleTy(tyarg, Typing.primTyCon_string) then
                 F.VarExp(Syntax.MkQualified([], VId_LT_string))
             else if isSimpleTy(tyarg, Typing.primTyCon_char) then
                 F.VarExp(Syntax.MkQualified([], VId_LT_char))
             else
                 raise Fail "invalid use of < operator"
         else if U.eqVId(vid, VId_GT) then
             if isSimpleTy(tyarg, Typing.primTyCon_int) then
                 F.VarExp(Syntax.MkQualified([], VId_GT_int))
             else if isSimpleTy(tyarg, Typing.primTyCon_word) then
                 F.VarExp(Syntax.MkQualified([], VId_GT_word))
             else if isSimpleTy(tyarg, Typing.primTyCon_real) then
                 F.VarExp(Syntax.MkQualified([], VId_GT_real))
             else if isSimpleTy(tyarg, Typing.primTyCon_string) then
                 F.VarExp(Syntax.MkQualified([], VId_GT_string))
             else if isSimpleTy(tyarg, Typing.primTyCon_char) then
                 F.VarExp(Syntax.MkQualified([], VId_GT_char))
             else
                 raise Fail "invalid use of > operator"
         else if U.eqVId(vid, VId_LE) then
             if isSimpleTy(tyarg, Typing.primTyCon_int) then
                 F.VarExp(Syntax.MkQualified([], VId_LE_int))
             else if isSimpleTy(tyarg, Typing.primTyCon_word) then
                 F.VarExp(Syntax.MkQualified([], VId_LE_word))
             else if isSimpleTy(tyarg, Typing.primTyCon_real) then
                 F.VarExp(Syntax.MkQualified([], VId_LE_real))
             else if isSimpleTy(tyarg, Typing.primTyCon_string) then
                 F.VarExp(Syntax.MkQualified([], VId_LE_string))
             else if isSimpleTy(tyarg, Typing.primTyCon_char) then
                 F.VarExp(Syntax.MkQualified([], VId_LE_char))
             else
                 raise Fail "invalid use of <= operator"
         else if U.eqVId(vid, VId_GE) then
             if isSimpleTy(tyarg, Typing.primTyCon_int) then
                 F.VarExp(Syntax.MkQualified([], VId_GE_int))
             else if isSimpleTy(tyarg, Typing.primTyCon_word) then
                 F.VarExp(Syntax.MkQualified([], VId_GE_word))
             else if isSimpleTy(tyarg, Typing.primTyCon_real) then
                 F.VarExp(Syntax.MkQualified([], VId_GE_real))
             else if isSimpleTy(tyarg, Typing.primTyCon_string) then
                 F.VarExp(Syntax.MkQualified([], VId_GE_string))
             else if isSimpleTy(tyarg, Typing.primTyCon_char) then
                 F.VarExp(Syntax.MkQualified([], VId_GE_char))
             else
                 raise Fail "invalid use of >= operator"
         else
             F.TyAppExp(F.VarExp(longvid), toFTy(ctx, env, tyarg))
      end
  | toFExp(ctx, env, U.InstantiatedVarExp(longvid, _, tyargs))
    = List.foldl (fn (ty, e) => F.TyAppExp(e, toFTy(ctx, env, ty))) (F.VarExp(longvid)) tyargs
  | toFExp(ctx, env, U.RecordExp fields) = let fun doField (label, e) = (label, toFExp(ctx, env, e))
                                           in F.RecordExp (List.map doField fields)
                                           end
  | toFExp(ctx, env, U.LetInExp(decs, e))
    = let fun go env' [] = toFExp(ctx, env', e)
            | go env' (U.ValDec(_, valbinds, _) :: decs)
              = let fun go' env'' [] = go env'' decs
                      | go' env'' (valbind :: valbinds')
                        = let val valbind' = doValBind ctx env'' valbind
                          in F.LetExp(valbind', go' env'' valbinds')
                          end
                in go' env' valbinds
                end
            | go env' (U.RecValDec(_, valbinds, _) :: decs)
              = let val valbinds' = List.map (doValBind ctx env') valbinds
                in F.LetRecExp(valbinds', go env' decs)
                end
      in go env decs
      end
  | toFExp(ctx, env, U.AppExp(e1, e2)) = F.AppExp(toFExp(ctx, env, e1), toFExp(ctx, env, e2))
  | toFExp(ctx, env, U.TypedExp(exp, _)) = toFExp(ctx, env, exp)
  | toFExp(ctx, env, U.IfThenElseExp(e1, e2, e3)) = F.IfThenElseExp(toFExp(ctx, env, e1), toFExp(ctx, env, e2), toFExp(ctx, env, e3))
  | toFExp(ctx, env, U.CaseExp(e, matches))
    = let fun doMatch(pat, exp) = raise Fail "not implemented yet" (* TODO *)
      in F.CaseExp(toFExp(ctx, env, e), List.map doMatch matches)
      end
  | toFExp(ctx, env, U.FnExp(vid, ty, body))
    = let val env' = env (* TODO *)
      in F.FnExp(vid, toFTy(ctx, env, ty), toFExp(ctx, env', body))
      end
  | toFExp(ctx, env, U.ProjectionExp { label = label, recordTy = recordTy, fieldTy = fieldTy })
    = F.ProjectionExp { label = label, recordTy = toFTy(ctx, env, recordTy), fieldTy = toFTy(ctx, env, fieldTy) }
  | toFExp(ctx, env, U.HandleExp _) = raise Fail "HandleExp: not implemented yet"
  | toFExp(ctx, env, U.RaiseExp _) = raise Fail "RaiseExp: not implemented yet"
and addValBindsToEnv ctx env (U.PatBind _) = raise Fail "internal error: PatBind cannot occur here"
  | addValBindsToEnv ctx env (U.TupleBind (vars, exp)) = env (* do nothing... for now *)
  | addValBindsToEnv ctx env (U.PolyVarBind (vid, tysc, exp)) = env (* do nothing... for now *)
and doValBind ctx env (U.PatBind _) = raise Fail "internal error: PatBind cannot occur here"
  | doValBind ctx env (U.TupleBind (vars, exp)) = F.TupleBind (List.map (fn (vid,ty) => (vid, toFTy(ctx, env, ty))) vars, toFExp(ctx, env, exp))
  | doValBind ctx env (U.PolyVarBind (vid, U.TypeScheme(tvs, ty), exp))
    = let val ty0 = toFTy (ctx, env, ty)
          val ty' = List.foldr (fn ((tv,cts),ty1) =>
                                   case cts of
                                       [] => F.ForallType (tv, ty1)
                                     | [U.IsEqType] => F.ForallType (tv, F.FnType (F.FnType (F.PairType (F.TyVar tv, F.TyVar tv), F.TyCon([], Typing.primTyCon_bool)), ty1))
                                     | _ => raise Fail "invalid type constraint"
                               ) ty0 tvs
          fun doExp (env', [])
              = toFExp(ctx, env', exp)
            | doExp (env', (tv,cts) :: rest)
              = (case cts of
                     [] => F.TyAbsExp (tv, doExp (env', rest))
                   | [U.IsEqType] => let val vid = freshVId(ctx, "eq")
                                         val eqTy = F.FnType (F.PairType (F.TyVar tv, F.TyVar tv), F.TyCon([], Typing.primTyCon_bool))
                                         val env'' = updateEqualityForTyVarMap(fn m => USyntax.TyVarMap.insert(m, tv, vid), env')
                                     in F.TyAbsExp (tv, F.FnExp(vid, eqTy, doExp(env'', rest)))
                                     end
                   | _ => raise Fail "invalid type constraint"
                )
      in F.TupleBind ([(vid, ty')], doExp(env, tvs))
      end
          (*
and decToValBinds(ctx, env, U.ValDec(_tvs, valbinds, _valenv))
    = let val valbinds' = List.map (doValBind ctx env) valbinds (* TODO: Use foldr *)
          val env' = env (* TODO: Add new bindings *)
      in (env'', false, valbinds' @ valbinds'')
      end
  | decToValBinds(ctx, env, U.RecValDec(tvs, valbinds, valenv))
    = let val env' = env (* TODO: Add new bindings *)
          val valbinds' = List.map (doValBind ctx env') valbinds
      in (env'', true, valbinds' @ valbinds'')
      end
*)
and toFDecs(ctx, env, []) = (env, [])
  | toFDecs(ctx, env, U.ValDec(tvs, valbinds, valenv) :: decs)
    = (* let val env' = env
          val (env'', decs') = toFDecs(ctx, env', decs)
      in (env'', F.ValDec decs')
      end *) raise Fail "toFDecs: not implemented yet"
  | toFDecs(ctx, env, U.RecValDec(tvs, valbinds, valenv) :: decs)
    = (* let val env' = env
      in toFDecs(ctx, env', decs)
      end *) raise Fail "toFDecs: not implemented yet"
and typeSchemeToTy(ctx, env, USyntax.TypeScheme(vars, ty))
    = let fun go env [] = toFTy(ctx, env, ty)
            | go env ((tv, []) :: xs) = let val env' = env (* TODO *)
                                        in F.ForallType(tv, go env' xs)
                                        end
            | go env ((tv, [U.IsEqType]) :: xs) = let val env' = env (* TODO *)
                                                      val eqTy = F.FnType(F.PairType(F.TyVar tv, F.TyVar tv), F.TyCon([], Typing.primTyCon_bool))
                                                  in F.ForallType(tv, F.FnType(eqTy, go env' xs))
                                                  end
            | go env ((tv, _) :: xs) = raise Fail "invalid type constraint"
      in go env vars
      end
and getEquality(ctx, env, U.TyCon([], longtycon))
    = let open InitialEnv
      in if U.eqULongTyCon(longtycon, Typing.primTyCon_int) then
             F.VarExp(Syntax.MkQualified([], VId_EQUAL_int))
         else if U.eqULongTyCon(longtycon, Typing.primTyCon_word) then
             F.VarExp(Syntax.MkQualified([], VId_EQUAL_word))
         else if U.eqULongTyCon(longtycon, Typing.primTyCon_string) then
             F.VarExp(Syntax.MkQualified([], VId_EQUAL_string))
         else if U.eqULongTyCon(longtycon, Typing.primTyCon_char) then
             F.VarExp(Syntax.MkQualified([], VId_EQUAL_char))
         else if U.eqULongTyCon(longtycon, Typing.primTyCon_bool) then
             F.VarExp(Syntax.MkQualified([], VId_EQUAL_bool))
         else if U.eqULongTyCon(longtycon, Typing.primTyCon_real) then
             raise Fail "'real' does not admit equality; this should have been a type error"
         else if U.eqULongTyCon(longtycon, Typing.primTyCon_exn) then
             raise Fail "'exn' does not admit equality; this should have been a type error"
         else
             raise Fail "equality for used-defined data types are not implemented yet"
      end
  | getEquality(ctx, env, U.TyCon([tyarg], longtycon))
    = if U.eqULongTyCon(longtycon, Typing.primTyCon_ref) then
          F.TyAppExp(F.VarExp(Syntax.MkQualified([], InitialEnv.VId_EQUAL_ref)), toFTy(ctx, env, tyarg))
      else if U.eqULongTyCon(longtycon, Typing.primTyCon_list) then
          F.AppExp(F.TyAppExp(F.VarExp(Syntax.MkQualified([], InitialEnv.VId_EQUAL_list)), toFTy(ctx, env, tyarg)), getEquality(ctx, env, tyarg))
      else
          raise Fail "equality for used-defined data types are not implemented yet"
  | getEquality (ctx, env, U.TyCon(tyargs, longtycon)) = raise Fail "equality for used-defined data types are not implemented yet"
  | getEquality (ctx, env as MkEnv r, U.TyVar tv) = (case USyntax.TyVarMap.find(#equalityForTyVarMap r, tv) of
                                                         NONE => raise Fail "equality for the type variable not found"
                                                      | SOME vid => F.VarExp(Syntax.MkQualified([], vid))
                                                    )
  | getEquality (ctx, env, U.RecordType fields) = let fun doField (label, ty) = (label, getEquality(ctx, env, ty))
                                                  in F.RecordEqualityExp (List.map doField fields)
                                                  end
  | getEquality (ctx, env, U.FnType _) = raise Fail "functions are not equatable; this should have been a type error"
end
end
