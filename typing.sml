structure Typing = struct
datatype TypeScheme = TypeScheme of USyntax.TyVar list * USyntax.Ty
datatype IdStatus = ValueVariable
                  | ValueConstructor
                  | ExceptionConstructor
type ValEnv = (TypeScheme * IdStatus) Syntax.VIdMap.map
type TyConEnv = (int) Syntax.TyConMap.map
datatype TyStr = TyStr (* of TypeFunction * ValEnv *)
datatype Env = MkEnv of { tyConMap : TyConEnv (* Syntax.TyCon -> (tycon id) *)
                        , valMap : ValEnv (* Syntax.VId -> (type scheme * id status) *)
                        , strMap : Env Syntax.StrIdMap.map
                        }
type Context = { nextTyVar : int ref
               , env : Env
               }
exception TypeError of string
exception NameError of string

fun lookupStr(env, nil) = env
  | lookupStr(env as MkEnv { strMap = strMap, ... }, (str0 as Syntax.MkStrId name) :: str1)
    = (case Syntax.StrIdMap.find(strMap, str0) of
           NONE => raise NameError("unknown structure name " ^ name)
         | SOME innerEnv => lookupStr(innerEnv, str1)
      )
fun lookupTyConInEnv(MkEnv env, tycon as Syntax.MkTyCon name)
    = (case Syntax.TyConMap.find(#tyConMap env, tycon) of
           NONE => raise NameError("unknown type constructor " ^ name)
         | SOME x => x
      )
fun lookupValInEnv(MkEnv env, vid as Syntax.MkVId name)
    = (case Syntax.VIdMap.find(#valMap env, vid) of
           NONE => raise NameError("unknown value name " ^ name)
         | SOME x => x
      )

val emptyEnv : Env
    = MkEnv { tyConMap = Syntax.TyConMap.empty
            , valMap = Syntax.VIdMap.empty
            , strMap = Syntax.StrIdMap.empty
            }
fun newContext(env : Env) : Context
    = { env = env
      , nextTyVar = ref 100
      }

fun freshTyVar(ctx : Context) : USyntax.TyVar
    = let val nextTyVar = #nextTyVar ctx
          val i = !nextTyVar
      in nextTyVar := i + 1
       ; USyntax.UTyVar(Syntax.MkTyVar "_", i)
      end

datatype Constraint
  = EqConstr of USyntax.Ty * USyntax.Ty (* ty1 = ty2 *)
  | FieldConstr of { label : Syntax.Label
                   , recordTy : USyntax.Ty
                   , fieldTy : USyntax.Ty
                   } (* recordTy = {label: fieldTy, ...} *)
  | IsEqType of USyntax.Ty
(* | Is(Int|Word|Real|String|Char) of USyntax.Ty *)
(* IsWordInt|IsRealInt|IsNum|IsNumTxt *)

datatype TyVarConstraint
  = TVFieldConstr of { label : Syntax.Label
                     , fieldTy : USyntax.Ty
                     }
  | TVIsEqType

local open USyntax
      val primTy_int    = USyntax.TyCon([], USyntax.ULongTyCon(Syntax.MkLongTyCon([], Syntax.MkTyCon "int"), 0))
      val primTy_word   = USyntax.TyCon([], USyntax.ULongTyCon(Syntax.MkLongTyCon([], Syntax.MkTyCon "word"), 0))
      val primTy_real   = USyntax.TyCon([], USyntax.ULongTyCon(Syntax.MkLongTyCon([], Syntax.MkTyCon "real"), 0))
      val primTy_string = USyntax.TyCon([], USyntax.ULongTyCon(Syntax.MkLongTyCon([], Syntax.MkTyCon "string"), 0))
      val primTy_char   = USyntax.TyCon([], USyntax.ULongTyCon(Syntax.MkLongTyCon([], Syntax.MkTyCon "char"), 0))
      val primTy_exn    = USyntax.TyCon([], USyntax.ULongTyCon(Syntax.MkLongTyCon([], Syntax.MkTyCon "exn"), 0))
      val primTy_bool   = USyntax.TyCon([], USyntax.ULongTyCon(Syntax.MkLongTyCon([], Syntax.MkTyCon "bool"), 0))
      val primTyCon_ref = USyntax.ULongTyCon(Syntax.MkLongTyCon([], Syntax.MkTyCon "ref"), 0)
in
(* constraints : Context * USyntax.Exp -> Constraint list * USyntax.Ty *)
fun constraints(ctx, SConExp(Syntax.IntegerConstant x))   = ([], primTy_int) (* TODO: overloaded literals *)
  | constraints(ctx, SConExp(Syntax.WordConstant x))      = ([], primTy_word) (* TODO: overloaded literals *)
  | constraints(ctx, SConExp(Syntax.RealConstant x))      = ([], primTy_real) (* TODO: overloaded literals *)
  | constraints(ctx, SConExp(Syntax.StringConstant x))    = ([], primTy_string) (* TODO: overloaded literals *)
  | constraints(ctx, SConExp(Syntax.CharacterConstant x)) = ([], primTy_char) (* TODO: overloaded literals *)
  | constraints(ctx, VarExp(Syntax.MkLongVId(str, vid as Syntax.MkVId name)))
    = (case lookupValInEnv(lookupStr(#env ctx, str), vid) of
          (TypeScheme([], ty), ids) => ([], ty)
        | _ => raise TypeError("not impl")
      )
  | constraints(ctx, RecordExp(row))
    = let val (ct, row') = constraintsFromRow(ctx, row)
      in (ct, RecordType(row'))
      end
  | constraints(ctx, LetInExp(decls, inner))
    = raise TypeError "let-in not implemented yet"
  | constraints(ctx, AppExp(f, x))
          (* f: s -> t, x: s *)
    = let val (ct1, funcTy) = constraints(ctx, f)
          val (ct2, argTy) = constraints(ctx, x)
          val retTy = TyVar(freshTyVar(ctx))
          (* funcTy = (argTy -> retTy) *)
          val ct = EqConstr(funcTy, FnType(argTy, retTy))
      in (ct :: (ct1 @ ct2), retTy)
      end
  | constraints(ctx, TypedExp(exp, ty))
    = let val (ct1, expTy) = constraints(ctx, exp)
          val ct = EqConstr(expTy, ty) (* ety = ty *)
      in (ct :: ct1, ty)
      end
  | constraints(ctx, HandleExp(e, handlers))
          (* e: t, handlers: exn -> t *)
    = raise TypeError "handle expression not implemented yet"
  | constraints(ctx, RaiseExp(exp))
    = let val (ct1, expTy) = constraints(ctx, exp)
          (* expTy = exn *)
          val ct = EqConstr(expTy, primTy_exn)
          val retTy = TyVar(freshTyVar(ctx))
      in (ct :: ct1, retTy)
      end
  | constraints(ctx, IfThenElseExp(cond, thenPart, elsePart))
    = let val (ct1, condTy) = constraints(ctx, cond)
          val (ct2, thenTy) = constraints(ctx, thenPart)
          val (ct3, elseTy) = constraints(ctx, elsePart)
          (* condTy = bool *)
          val ect1 = EqConstr(condTy, primTy_bool)
          (* thenTy = elseTy *)
          val ect2 = EqConstr(thenTy, elseTy)
      in (ect1 :: ect2 :: (ct1 @ ct2 @ ct3), thenTy)
      end
  | constraints(ctx, CaseExp(exp, matches))
    = let val (ct1, expTy) = constraints(ctx, exp)
          val (ct2, patTy, retTy) = constraintsFromMatch(ctx, matches)
      in (EqConstr(expTy, patTy) :: (ct1 @ ct2), retTy)
      end
  | constraints(ctx, FnExp([(VIdPat(Syntax.MkLongVId([], vid)), body)]))
    = let val argTy = TyVar(freshTyVar(ctx))
          val MkEnv env = #env ctx
          val valMap' = Syntax.VIdMap.insert(#valMap env, vid, (TypeScheme([], argTy), ValueVariable))
          val env' = MkEnv { tyConMap = #tyConMap env, valMap = valMap', strMap = #strMap env }
          val ctx' = { nextTyVar = #nextTyVar ctx, env = env' }
          val (ct1, retTy) = constraints(ctx', body) (* TODO: Add vid to the ctx *)
      in (ct1, USyntax.FnType(argTy, retTy))
      end
  | constraints(ctx, FnExp([(TypedPat(VIdPat(Syntax.MkLongVId([], vid)), argTy), body)]))
    = let val MkEnv env = #env ctx
          val valMap' = Syntax.VIdMap.insert(#valMap env, vid, (TypeScheme([], argTy), ValueVariable))
          val env' = MkEnv { tyConMap = #tyConMap env, valMap = valMap', strMap = #strMap env }
          val ctx' = { nextTyVar = #nextTyVar ctx, env = env' }
          val (ct1, retTy) = constraints(ctx', body) (* TODO: Add vid to the ctx *)
      in (ct1, USyntax.FnType(argTy, retTy))
      end
  | constraints(ctx, FnExp(matches)) (* not really implemented yet *)
    = let val (ct, argTy, retTy) = constraintsFromMatch(ctx, matches)
      in (ct, USyntax.FnType(argTy, retTy))
      end
 (* constraintsFromRow : Ctx * (Label * Exp) list -> Constraint list * (Label * Syntax.Ty) list *)
and constraintsFromRow(ctx, xs)
    = let fun oneField(label, exp) = let val (ct, ty) = constraints(ctx, exp)
                                     in (ct, (label, ty))
                                     end
          val (cts, row) = ListPair.unzip (List.map oneField xs)
      in (List.concat cts, row)
      end
 (* constraintsFromMatch : Ctx * (Pat * Exp) list -> Constraint list * (* pattern *) Syntax.Ty * (* expression *) Syntax.Ty *)
and constraintsFromMatch(ctx, (pat0, exp0) :: rest)
    = let val (ct0p, patTy) = constraintsFromPat(ctx, pat0)
          val (ct0e, expTy) = constraints(ctx, exp0)
          fun oneBranch(pat, exp) = let val (ctp, patTy) = constraintsFromPat(ctx, pat)
                                        val (cte, expTy) = constraints(ctx, exp)
                                    in ctp @ cte
                                    end
          val cts = List.map oneBranch rest
      in (ct0p @ ct0e @ List.concat cts, patTy, expTy)
      end
  | constraintsFromMatch(ctx, nil) = raise TypeError "invalid syntax tree: match is empty"
 (* constraintsFromPat : Ctx * Pat -> Constraint list * Syntax.Ty *)
and constraintsFromPat(ctx, WildcardPat)
    = let val ty = TyVar(freshTyVar(ctx))
      in ([], ty)
      end
  | constraintsFromPat(ctx, SConPat(Syntax.IntegerConstant(_)))   = ([], primTy_int)
  | constraintsFromPat(ctx, SConPat(Syntax.WordConstant(_)))      = ([], primTy_word)
  | constraintsFromPat(ctx, SConPat(Syntax.RealConstant(_)))      = ([], primTy_real)
  | constraintsFromPat(ctx, SConPat(Syntax.StringConstant(_)))    = ([], primTy_string)
  | constraintsFromPat(ctx, SConPat(Syntax.CharacterConstant(_))) = ([], primTy_char)
  | constraintsFromPat(ctx, VIdPat(longvid)) = raise TypeError "VIdPat: not implemented yet"
  | constraintsFromPat(ctx, RecordPat(row, wildcard))
    = let val (ct, row') = constraintsFromPatRow(ctx, row)
      in if wildcard then
             let val recordTy = TyVar(freshTyVar(ctx))
                 fun oneField(label, ty) = FieldConstr { label = label, recordTy = recordTy, fieldTy = ty }
                 val fieldCts = List.map oneField row'
             in (fieldCts @ ct, recordTy)
             end
         else
             (ct, RecordType(row'))
      end
  | constraintsFromPat(ctx, ConPat(_, _)) = raise TypeError "ConPat"
  | constraintsFromPat(ctx, TypedPat(WildcardPat, ty))
    = ([], ty)
  | constraintsFromPat(ctx, TypedPat(VIdPat(Syntax.MkLongVId([], vid)), ty)) (* fresh variable? *)
    = ([], ty)
  | constraintsFromPat(ctx, TypedPat(pat, ty))
    = let val (ct, inferredTy) = constraintsFromPat(ctx, pat)
      in (EqConstr(ty, inferredTy) :: ct, ty)
      end
  | constraintsFromPat(ctx, LayeredPat(_, SOME ty, pat))
    = let val (ct, inferredTy) = constraintsFromPat(ctx, pat)
      in (EqConstr(ty, inferredTy) :: ct, ty)
      end
  | constraintsFromPat(ctx, LayeredPat(_, NONE, pat)) = constraintsFromPat(ctx, pat)
 (* constraintsFromPatRow : Ctx * (Label * Pat) list -> Constraint list * (Label * Syntax.Ty) list *)
and constraintsFromPatRow(ctx, xs)
    = let fun oneField(label, pat) = let val (ct, ty) = constraintsFromPat(ctx, pat)
                                     in (ct, (label, ty))
                                     end
          val (cts, row) = ListPair.unzip (List.map oneField xs)
      in (List.concat cts, row)
      end

(* occurCheck : TyVar -> Ty -> bool; returns true if the type variable occurs in the type *)
fun occurCheck tv = let fun check (TyVar tv') = tv = tv'
                          | check (RecordType xs) = List.exists (fn (label, ty) => check ty) xs
                          | check (TyCon(tyargs, longtycon)) = List.exists check tyargs
                          | check (FnType(ty1, ty2)) = check ty1 orelse check ty2
                    in check
                    end

(* substituteTy : TyVar * Ty -> Ty -> Ty *)
fun substituteTy (tv, replacement) =
    let fun substTy (ty as TyVar tv') = if tv = tv' then
                                            replacement
                                        else
                                            ty
          | substTy (RecordType fields) = RecordType (List.map (fn (label, ty) => (label, substTy ty)) fields)
          | substTy (TyCon(tyargs, longtycon)) = TyCon(List.map substTy tyargs, longtycon)
          | substTy (FnType(ty1, ty2)) = FnType(substTy ty1, substTy ty2)
    in substTy
    end

(* substituteConstraint : TyVar * Ty -> Constraint -> Constraint *)
fun substituteConstraint (tv, replacement) =
    let val substTy = substituteTy (tv, replacement)
    in fn EqConstr(ty1, ty2) => EqConstr(substTy ty1, substTy ty2)
     | FieldConstr{label = label, recordTy = recordTy, fieldTy = fieldTy } => FieldConstr{label = label, recordTy = substTy recordTy, fieldTy = substTy fieldTy}
     | IsEqType ty => IsEqType(substTy ty)
    end

type Subst = (TyVar * Ty) list

(* applySubstTy : Subst -> Ty -> Ty *)
fun applySubstTy subst =
    let fun substTy (ty as TyVar tv')
            = (case List.find (fn (tv, _) => tv = tv') subst of
                   NONE => ty
                 | SOME (_, replacement) => replacement (* TODO: single replacement is sufficient? *)
              )
          | substTy (RecordType fields) = RecordType (List.map (fn (label, ty) => (label, substTy ty)) fields)
          | substTy (TyCon(tyargs, longtycon)) = TyCon(List.map substTy tyargs, longtycon)
          | substTy (FnType(ty1, ty2)) = FnType(substTy ty1, substTy ty2)
    in substTy
    end

 (* unify : Context * (TyVar * TyVarConstraint) list * Constraint list -> Subst * (TyVar * TyVarConstraint) list *)
fun unify(ctx, tvc, EqConstr(TyVar(tv), ty) :: ctrs) : Subst * (TyVar * TyVarConstraint) list
    = unifyTyVarAndTy(ctx, tvc, tv, ty, ctrs)
  | unify(ctx, tvc, EqConstr(ty, TyVar(tv)) :: ctrs)
    = unifyTyVarAndTy(ctx, tvc, tv, ty, ctrs)
  | unify(ctx, tvc, EqConstr(FnType(s0, s1), FnType(t0, t1)) :: ctrs) = unify(ctx, tvc, EqConstr(s0, t0) :: EqConstr(s1, t1) :: ctrs)
  | unify(ctx, tvc, EqConstr(RecordType(fields), RecordType(fields')) :: ctrs)
    = if List.length fields <> List.length fields then
          raise TypeError("unification failed: incompatible record types (different number of fields)")
      else
          unify(ctx, tvc, List.foldl (fn ((label, ty), acc) => case List.find (fn (label', _) => label = label') fields' of
                                                                   NONE => raise TypeError("unification failed: incompatible record types")
                                                                 | SOME(_,ty') => EqConstr(ty, ty') :: acc) ctrs fields)
  | unify(ctx, tvc, EqConstr(TyCon(tyarg, con), TyCon(tyarg', con')) :: ctrs)
    = if con = con' then
          unify(ctx, tvc, ListPair.mapEq EqConstr (tyarg, tyarg') @ ctrs)
          handle ListPair.UnequalLengths => raise TypeError("unification failed: the number of type arguments differ")
      else
          raise TypeError("unification failed: type constructor mismatch")
  | unify(ctx, tvc, EqConstr(_, _) :: ctrs) = raise TypeError("unification failed: not match")
  | unify(ctx, tvc, FieldConstr{label = label, recordTy = RecordType(fields), fieldTy = fieldTy} :: ctrs)
    = (case List.find (fn (label', _) => label = label') fields of
           NONE => raise TypeError("unification failed: no field")
         | SOME(_, ty') => unify(ctx, tvc, EqConstr(fieldTy, ty') :: ctrs)
      )
  | unify(ctx, tvc, FieldConstr{label = label, recordTy = TyCon(_, _), fieldTy = fieldTy} :: ctrs) = raise TypeError("record field for a non-record type")
  | unify(ctx, tvc, FieldConstr{label = label, recordTy = FnType(_, _), fieldTy = fieldTy} :: ctrs) = raise TypeError("record field for a function type")
  | unify(ctx, tvc, FieldConstr{label = label, recordTy = TyVar tv, fieldTy = fieldTy} :: ctrs) = unify (ctx, (tv, TVFieldConstr { label = label, fieldTy = fieldTy }) :: tvc, ctrs)
  | unify(ctx, tvc, IsEqType(RecordType fields) :: ctrs) = unify(ctx, tvc, List.map (fn (label, ty) => IsEqType ty) fields @ ctrs)
  | unify(ctx, tvc, IsEqType(FnType _) :: ctrs) = raise TypeError("function type does not admit equality")
  | unify(ctx, tvc, IsEqType(TyCon(tyargs, longtycon)) :: ctrs)
    = if longtycon = primTyCon_ref then
          unify(ctx, tvc, ctrs)
      else
          (* (longtycon???) : List.map IsEqType tyargs @ ctrs *)
          raise TypeError("not impl")
  | unify(ctx, tvc, IsEqType(TyVar(tv)) :: ctrs) = unify(ctx, (tv, TVIsEqType) :: tvc, ctrs)
  | unify(ctx, tvc, nil) = (nil, tvc)
and unifyTyVarAndTy(ctx : Context, tvc : (TyVar * TyVarConstraint) list, tv : TyVar, ty : Ty, ctrs : Constraint list)
    = if (case ty of TyVar(tv') => tv = tv' | _ => false) then (* ty = TyVar tv *)
          ([], []) (* do nothing *)
      else if occurCheck tv ty then
          raise TypeError("unification failed: occurrence check (" ^ USyntax.print_TyVar tv ^ " in " ^ USyntax.print_Ty ty ^ ")")
      else
          let val (e, tvc') = List.partition (fn (tv', c) => tv = tv') tvc
              fun toConstraint (_, TVFieldConstr { label = label, fieldTy = fieldTy }) = FieldConstr { label = label, recordTy = ty, fieldTy = fieldTy }
                | toConstraint (_, TVIsEqType) = IsEqType ty
              val (ss, tvc'') = unify(ctx, tvc', List.map toConstraint e @ List.map (substituteConstraint (tv, ty)) ctrs)
          in ((tv, ty) :: ss, tvc'')
          end

(* typeCheckExp : Context * USyntax.Exp -> Subst * (TyVar * TyVarConstraint) list * USyntax.Ty * USyntax.Exp *)
fun typeCheckExp(ctx, exp) = let val (constraints, ty) = constraints(ctx, exp)
                                 val (subst, tvc) = unify(ctx, [], constraints)
                                 val applySubst = applySubstTy subst
                             in (subst, tvc, applySubst ty, USyntax.mapTyInExp applySubst exp)
                             end
end (* local *)
end (* structure Typing *)
