structure Typing = struct
datatype TypeScheme = TypeScheme of USyntax.TyVar list * USyntax.Ty
type ValEnv = (TypeScheme * Syntax.IdStatus) Syntax.VIdMap.map
type TyConEnv = (int) Syntax.TyConMap.map
datatype TyStr = TyStr (* of TypeFunction * ValEnv *)

datatype Env = MkEnv of { tyConMap : TyConEnv (* Syntax.TyCon -> (tycon id) *)
                        , valMap : ValEnv (* Syntax.VId -> (type scheme * id status) *)
                        , strMap : Env Syntax.StrIdMap.map
                        }

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

type Subst = (USyntax.TyVar * USyntax.Ty) list

type Context = { nextTyVar : int ref
               (*
               , constraints : (Constraint list) ref
               , tyVarConstraints : ((USyntax.TyVar * TyVarConstraint) list) ref (* should use (multi-)map? *)
               , tyVarSubst : Subst ref
               *)
               }

exception TypeError of string
exception NameError of string

(* lookupStr : Env * Syntax.StrId list -> Env *)
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

(* The Definition, 4.7 Non-expansive Expressions *)
(* isNonexpansive : Env * USyntax.Exp -> bool *)
fun isNonexpansive(env, USyntax.SConExp _) = true
  | isNonexpansive(env, USyntax.VarExp _) = true (* <op> longvid *)
  | isNonexpansive(env, USyntax.RecordExp fields) = List.all (fn (_, e) => isNonexpansive(env, e)) fields
  | isNonexpansive(env, USyntax.TypedExp(e, _)) = isNonexpansive(env, e)
  | isNonexpansive(env, USyntax.AppExp(conexp, e)) = isConexp(env, conexp) andalso isNonexpansive(env, e)
  | isNonexpansive(env, USyntax.FnExp _) = true
  | isNonexpansive(env, _) = false
and isConexp(env, USyntax.TypedExp(e, _)) = isConexp(env, e)
  | isConexp(env, USyntax.VarExp(Syntax.MkLongVId([], Syntax.MkVId "ref"), _)) = false
  | isConexp(env, USyntax.VarExp(Syntax.MkLongVId(strid, tycon), idstatus))
    = (case idstatus of
           Syntax.ValueVariable => false
         | Syntax.ValueConstructor => true
         | Syntax.ExceptionConstructor => true
      )
  | isConexp(env, _) = false

val primTy_int    = USyntax.TyCon([], USyntax.ULongTyCon(Syntax.MkLongTyCon([], Syntax.MkTyCon "int"), 0))
val primTy_word   = USyntax.TyCon([], USyntax.ULongTyCon(Syntax.MkLongTyCon([], Syntax.MkTyCon "word"), 1))
val primTy_real   = USyntax.TyCon([], USyntax.ULongTyCon(Syntax.MkLongTyCon([], Syntax.MkTyCon "real"), 2))
val primTy_string = USyntax.TyCon([], USyntax.ULongTyCon(Syntax.MkLongTyCon([], Syntax.MkTyCon "string"), 3))
val primTy_char   = USyntax.TyCon([], USyntax.ULongTyCon(Syntax.MkLongTyCon([], Syntax.MkTyCon "char"), 4))
val primTy_exn    = USyntax.TyCon([], USyntax.ULongTyCon(Syntax.MkLongTyCon([], Syntax.MkTyCon "exn"), 5))
val primTy_bool   = USyntax.TyCon([], USyntax.ULongTyCon(Syntax.MkLongTyCon([], Syntax.MkTyCon "bool"), 6))
val primTyCon_ref = USyntax.ULongTyCon(Syntax.MkLongTyCon([], Syntax.MkTyCon "ref"), 7)

val emptyEnv : Env
    = MkEnv { tyConMap = Syntax.TyConMap.empty
            , valMap = Syntax.VIdMap.empty
            , strMap = Syntax.StrIdMap.empty
            }
            (*
val initialEnv : Env
    = let open Syntax
      in MkEnv { tyConMap = List.foldl Syntax.TyConMap.insert' Syntax.TyConMap.empty
                                       [(MkTyCon "unit", _)
                                       ,(MkTyCon "bool", _)
                                       ,(MkTyCon "int", _)
                                       ,(MkTyCon "word", _)
                                       ,(MkTyCon "real", _)
                                       ,(MkTyCon "string", _)
                                       ,(MkTyCon "char", _)
                                       ,(MkTyCon "list", _) (* 'a list *)
                                       ,(MkTyCon "ref", _) (* 'a ref *)
                                       ,(MkTyCon "exn", _)
                                       ]
               , valMap = List.foldl Syntax.VIdMap.insert' Syntax.VIdMap.empty
                                     (* C Appendix: The Initial Static Basis *)
                                     [(MkVId "ref", (TypeScheme ([], _), ValueConstructor)) (* forall 'a. 'a -> 'a ref *)
                                     ,(MkVId "nil", (TypeScheme ([], _), ValueConstructor)) (* forall 'a. 'a list *)
                                     ,(MkVId "true", (TypeScheme ([], primTy_bool), ValueConstructor))
                                     ,(MkVId "false", (TypeScheme ([], primTy_bool), ValueConstructor))
                                     ,(MkVId "Match", (TypeScheme ([], primTy_exn), ExceptionConstructor))
                                     ,(MkVId "Bind", (TypeScheme ([], primTy_exn), ExceptionConstructor))
                                     ,(MkVId "::", (TypeScheme ([], _), ValueConstructor)) (* forall 'a. 'a * 'a list -> 'a list *)
                                     ,(MkVId "=", (TypeScheme ([], _), ValueVariable)) (* forall ''a. ''a * ''a -> bool *)
                                     ,(MkVId ":=", (TypeScheme ([], _), ValueVariable)) (* forall 'a. 'a ref * 'a -> {} *)
                                     (* Overloaded identifiers *)
                                     ,(MkVId "abs", (_, ValueVariable)) (* realint -> realint, default: int -> int *)
                                     ,(MkVId "~", (_, ValueVariable)) (* realint -> realint, default: int -> int *)
                                     ,(MkVId "div", (_, ValueVariable)) (* wordint * wordint -> wordint, default: int * int -> int *)
                                     ,(MkVId "mod", (_, ValueVariable)) (* wordint * wordint -> wordint, default: int * int -> int *)
                                     ,(MkVId "*", (_, ValueVariable)) (* num * num -> num, default: int * int -> int *)
                                     ,(MkVId "/", (_, ValueVariable)) (* Real * Real -> Real, default: real * real -> real *)
                                     ,(MkVId "+", (_, ValueVariable)) (* num * num -> num, default: int * int -> int *)
                                     ,(MkVId "-", (_, ValueVariable)) (* num * num -> num, default: int * int -> int *)
                                     ,(MkVId "<", (_, ValueVariable)) (* numtxt * numtxt -> bool, default: int * int -> bool *)
                                     ,(MkVId ">", (_, ValueVariable)) (* numtxt * numtxt -> bool, default: int * int -> bool *)
                                     ,(MkVId "<=", (_, ValueVariable)) (* numtxt * numtxt -> bool, default: int * int -> bool *)
                                     ,(MkVId ">=", (_, ValueVariable)) (* numtxt * numtxt -> bool, default: int * int -> bool *)
                                     ]
               , strMap = Syntax.StrIdMap.empty
               }
            *)
fun newContext() : Context
    = { nextTyVar = ref 100
      (*
      , constraints = ref []
      , tyVarSubst = ref []
      *)
      }

fun freshTyVar(ctx : Context) : USyntax.TyVar
    = let val nextTyVar = #nextTyVar ctx
          val i = !nextTyVar
      in nextTyVar := i + 1
       ; USyntax.UTyVar(Syntax.MkTyVar "_", i)
      end

local open USyntax
in
(* occurCheck : TyVar -> Ty -> bool; returns true if the type variable occurs in the type *)
fun occurCheck tv = let fun check (TyVar tv') = eqUTyVar(tv, tv')
                          | check (RecordType xs) = List.exists (fn (label, ty) => check ty) xs
                          | check (TyCon(tyargs, longtycon)) = List.exists check tyargs
                          | check (FnType(ty1, ty2)) = check ty1 orelse check ty2
                    in check
                    end

(* substituteTy : TyVar * Ty -> Ty -> Ty *)
fun substituteTy (tv, replacement) =
    let fun substTy (ty as TyVar tv') = if eqUTyVar(tv, tv') then
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

(* applySubstTy : Subst -> Ty -> Ty *)
fun applySubstTy subst =
    let fun substTy (ty as TyVar tv')
            = (case List.find (fn (tv, _) => eqUTyVar(tv, tv')) subst of
                   NONE => ty
                 | SOME (_, replacement) => replacement (* TODO: single replacement is sufficient? *)
              )
          | substTy (RecordType fields) = RecordType (List.map (fn (label, ty) => (label, substTy ty)) fields)
          | substTy (TyCon(tyargs, longtycon)) = TyCon(List.map substTy tyargs, longtycon)
          | substTy (FnType(ty1, ty2)) = FnType(substTy ty1, substTy ty2)
    in substTy
    end

 (* instantiate : Context * TypeScheme -> Ty *)
fun instantiate(ctx, TypeScheme(vars, ty)) = List.foldl (fn (v, ty) => let val v' = freshTyVar(ctx)
                                                                       in substituteTy (v, TyVar(v')) ty
                                                                       end) ty vars

 (* unify : Context * (TyVar * TyVarConstraint) list * Constraint -> Subst * (TyVar * TyVarConstraint) list *)
fun unify(ctx, tvc, EqConstr(TyVar(tv), ty) :: ctrs) : Subst * (TyVar * TyVarConstraint) list
    = unifyTyVarAndTy(ctx, tvc, tv, ty, ctrs)
  | unify(ctx, tvc, EqConstr(ty, TyVar(tv)) :: ctrs)
    = unifyTyVarAndTy(ctx, tvc, tv, ty, ctrs)
  | unify(ctx, tvc, EqConstr(FnType(s0, s1), FnType(t0, t1)) :: ctrs)
    = unify(ctx, tvc, EqConstr(s0, t0) :: EqConstr(s1, t1) :: ctrs)
  | unify(ctx, tvc, EqConstr(RecordType(fields), RecordType(fields')) :: ctrs)
    = if List.length fields <> List.length fields then
          raise TypeError("unification failed: incompatible record types (different number of fields)")
      else
          unify(ctx, tvc, List.foldl (fn ((label, ty), acc) => case List.find (fn (label', _) => label = label') fields' of
                                                                   NONE => raise TypeError("unification failed: incompatible record types")
                                                                 | SOME(_,ty') => EqConstr(ty, ty') :: acc)
                                     ctrs fields)
  | unify(ctx, tvc, EqConstr(TyCon(tyarg, con), TyCon(tyarg', con')) :: ctrs)
    = if eqULongTyCon(con, con') then
          unify(ctx, tvc, (ListPair.mapEq EqConstr (tyarg, tyarg')
                           handle ListPair.UnequalLengths => raise TypeError("unification failed: the number of type arguments differ")
                          ) @ ctrs)
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
  | unify(ctx, tvc, FieldConstr{label = label, recordTy = TyVar tv, fieldTy = fieldTy} :: ctrs)
    = unify(ctx, (tv, TVFieldConstr { label = label, fieldTy = fieldTy }) :: tvc, ctrs)
  | unify(ctx, tvc, IsEqType(RecordType fields) :: ctrs) = unify(ctx, tvc, List.map (fn (label, ty) => IsEqType ty) fields @ ctrs)
  | unify(ctx, tvc, IsEqType(FnType _) :: ctrs) = raise TypeError("function type does not admit equality")
  | unify(ctx, tvc, IsEqType(TyCon(tyargs, longtycon)) :: ctrs)
    = if eqULongTyCon(longtycon, primTyCon_ref) then
          unify(ctx, tvc, ctrs) (* do nothing *)
      else
          (* (longtycon???) : List.map IsEqType tyargs @ ctrs *)
          raise Fail "IsEqType TyCon: not impl"
  | unify(ctx, tvc, IsEqType(TyVar(tv)) :: ctrs) = unify(ctx, (tv, TVIsEqType) :: tvc, ctrs)
  | unify(ctx, tvc, nil) = (nil, tvc)
and unifyTyVarAndTy(ctx : Context, tvc : (TyVar * TyVarConstraint) list, tv : TyVar, ty : Ty, ctrs : Constraint list)
    = if (case ty of TyVar(tv') => eqUTyVar(tv, tv') | _ => false) then (* ty = TyVar tv *)
          ([], []) (* do nothing *)
      else if occurCheck tv ty then
          raise TypeError("unification failed: occurrence check (" ^ USyntax.print_TyVar tv ^ " in " ^ USyntax.print_Ty ty ^ ")")
      else
          let val (e, tvc') = List.partition (fn (tv', c) => eqUTyVar(tv, tv')) tvc
              fun toConstraint (_, TVFieldConstr { label = label, fieldTy = fieldTy }) = FieldConstr { label = label, recordTy = ty, fieldTy = fieldTy }
                | toConstraint (_, TVIsEqType) = IsEqType ty
              val (ss, tvc'') = unify(ctx, tvc', List.map toConstraint e @ List.map (substituteConstraint (tv, ty)) ctrs)
          in ((tv, ty) :: ss, tvc'')
          end

(* constraintsExp : Context * Env * USyntax.Exp -> Constraint list * USyntax.Ty *)
fun constraintsExp(ctx : Context, env : Env, SConExp(scon))
    = (case scon of (* TODO: overloaded literals *)
           Syntax.IntegerConstant x   => ([], primTy_int)
         | Syntax.WordConstant x      => ([], primTy_word)
         | Syntax.RealConstant x      => ([], primTy_real)
         | Syntax.StringConstant x    => ([], primTy_string)
         | Syntax.CharacterConstant x => ([], primTy_char)
      )
  | constraintsExp(ctx, env, VarExp(Syntax.MkLongVId(str, vid as Syntax.MkVId name), idstatus))
    = (case lookupValInEnv(lookupStr(env, str), vid) of
          (TypeScheme([], ty), ids) => ([], ty)
        | _ => raise Fail "type scheme: not impl"
      )
  | constraintsExp(ctx, env, RecordExp(row))
    = let val (ct, row') = constraintsFromRow(ctx, env, row)
      in (ct, RecordType(row'))
      end
  | constraintsExp(ctx, env, LetInExp(decls, inner))
    = raise Fail "let-in not implemented yet"
  | constraintsExp(ctx, env, AppExp(f, x))
          (* f: s -> t, x: s *)
    = let val (ct1, funcTy) = constraintsExp(ctx, env, f)
          val (ct2, argTy) = constraintsExp(ctx, env, x)
          val retTy = TyVar(freshTyVar(ctx))
          (* funcTy = (argTy -> retTy) *)
          val ct = EqConstr(funcTy, FnType(argTy, retTy))
      in (ct :: (ct1 @ ct2), retTy)
      end
  | constraintsExp(ctx, env, TypedExp(exp, ty))
    = let val (ct1, expTy) = constraintsExp(ctx, env, exp)
          val ct = EqConstr(expTy, ty) (* ety = ty *)
      in (ct :: ct1, ty)
      end
  | constraintsExp(ctx, env, HandleExp(e, handlers))
          (* e: t, handlers: exn -> t *)
    = raise Fail "handle expression not implemented yet"
  | constraintsExp(ctx, env, RaiseExp(exp))
    = let val (ct1, expTy) = constraintsExp(ctx, env, exp)
          (* expTy = exn *)
          val ct = EqConstr(expTy, primTy_exn)
          val retTy = TyVar(freshTyVar(ctx))
      in (ct :: ct1, retTy)
      end
  | constraintsExp(ctx, env, IfThenElseExp(cond, thenPart, elsePart))
    = let val (ct1, condTy) = constraintsExp(ctx, env, cond)
          val (ct2, thenTy) = constraintsExp(ctx, env, thenPart)
          val (ct3, elseTy) = constraintsExp(ctx, env, elsePart)
          (* condTy = bool *)
          val ect1 = EqConstr(condTy, primTy_bool)
          (* thenTy = elseTy *)
          val ect2 = EqConstr(thenTy, elseTy)
      in (ect1 :: ect2 :: (ct1 @ ct2 @ ct3), thenTy)
      end
  | constraintsExp(ctx, env, CaseExp(exp, matches))
    = let val (ct1, expTy) = constraintsExp(ctx, env, exp)
          val (ct2, patTy, retTy) = constraintsFromMatch(ctx, env, matches)
      in (EqConstr(expTy, patTy) :: (ct1 @ ct2), retTy)
      end
  | constraintsExp(ctx, env, FnExp(matches))
    = let val (ct, argTy, retTy) = constraintsFromMatch(ctx, env, matches)
      in (ct, USyntax.FnType(argTy, retTy))
      end
 (* constraintsFromRow : Ctx * Env * (Label * Exp) list -> Constraint list * (Label * Syntax.Ty) list *)
and constraintsFromRow(ctx, env, xs)
    = let fun oneField(label, exp) = let val (ct, ty) = constraintsExp(ctx, env, exp)
                                     in (ct, (label, ty))
                                     end
          val (cts, row) = ListPair.unzip (List.map oneField xs)
      in (List.concat cts, row)
      end
 (* constraintsFromMatch : Ctx * Env * (Pat * Exp) list -> Constraint list * (* pattern *) Syntax.Ty * (* expression *) Syntax.Ty *)
and constraintsFromMatch(ctx, env, (pat0, exp0) :: rest)
    = let val (ct0, patTy, expTy) = constraintsFromMatchBranch(ctx, env, pat0, exp0)
          fun oneBranch((pat, exp), ct)
              = let val (ct', patTy', expTy') = constraintsFromMatchBranch(ctx, env, pat, exp)
                in EqConstr(patTy, patTy') :: EqConstr(expTy, expTy') :: ct' @ ct
                end
          val cts = List.foldl oneBranch ct0 rest
      in (cts, patTy, expTy)
      end
  | constraintsFromMatch(ctx, env, nil) = raise TypeError "invalid syntax tree: match is empty"
and constraintsFromMatchBranch(ctx : Context, env as MkEnv env' : Env, pat, exp)
    = let val (ctp, patTy, vars) = constraintsFromPat(ctx, env, pat)
          val env'' = MkEnv { tyConMap = #tyConMap env'
                            , valMap = Syntax.VIdMap.unionWith #2 (#valMap env', vars)
                            , strMap = #strMap env'
                            }
          val (cte, expTy) = constraintsExp(ctx, env'', exp)
      in (ctp @ cte, patTy, expTy)
      end
 (* constraintsFromPat : Ctx * Env * Pat -> Constraint list * USyntax.Ty * USyntax.Ty Syntax.VIdMap.map *)
and constraintsFromPat(ctx, env, WildcardPat) : Constraint list * USyntax.Ty * ValEnv
    = let val ty = TyVar(freshTyVar(ctx))
      in ([], ty, Syntax.VIdMap.empty)
      end
  | constraintsFromPat(ctx, env, SConPat(Syntax.IntegerConstant(_)))   = ([], primTy_int, Syntax.VIdMap.empty)
  | constraintsFromPat(ctx, env, SConPat(Syntax.WordConstant(_)))      = ([], primTy_word, Syntax.VIdMap.empty)
  | constraintsFromPat(ctx, env, SConPat(Syntax.RealConstant(_)))      = ([], primTy_real, Syntax.VIdMap.empty)
  | constraintsFromPat(ctx, env, SConPat(Syntax.StringConstant(_)))    = ([], primTy_string, Syntax.VIdMap.empty)
  | constraintsFromPat(ctx, env, SConPat(Syntax.CharacterConstant(_))) = ([], primTy_char, Syntax.VIdMap.empty)
  | constraintsFromPat(ctx, MkEnv env, VarPat(vid))
    = (case Syntax.VIdMap.find(#valMap env, vid) of
           SOME (tysc, Syntax.ValueConstructor) => raise TypeError "VarPat: invalid pattern"
         | SOME (tysc, Syntax.ExceptionConstructor) => raise TypeError "VarPat: invalid pattern"
         | SOME (_, Syntax.ValueVariable) => (* shadowing *) let val ty = TyVar(freshTyVar(ctx))
                                                      in ([], ty, Syntax.VIdMap.insert(Syntax.VIdMap.empty, vid, (TypeScheme([], ty), Syntax.ValueVariable)))
                                                      end
         | NONE => let val ty = TyVar(freshTyVar(ctx))
                   in ([], ty, Syntax.VIdMap.insert(Syntax.VIdMap.empty, vid, (TypeScheme([], ty), Syntax.ValueVariable)))
                   end
      )
  | constraintsFromPat(ctx, env, NulConPat(Syntax.MkLongVId(strid, vid)))
    = let val MkEnv strEnv = lookupStr(env, strid)
      in case Syntax.VIdMap.find(#valMap strEnv, vid) of
             SOME (tysc, Syntax.ValueConstructor) => raise Fail "NulConPat: not implemented yet"
           | SOME (tysc, Syntax.ExceptionConstructor) => ([], primTy_exn, Syntax.VIdMap.empty)
           | SOME (_, Syntax.ValueVariable) => raise TypeError "invalid pattern"
           | NONE => raise TypeError "invalid pattern"
      end
  | constraintsFromPat(ctx, env, RecordPat(row, wildcard))
    = let val (ct, row', vars) = constraintsFromPatRow(ctx, env, row)
      in if wildcard then
             let val recordTy = TyVar(freshTyVar(ctx))
                 fun oneField(label, ty) = FieldConstr { label = label, recordTy = recordTy, fieldTy = ty }
                 val fieldCts = List.map oneField row'
             in (fieldCts @ ct, recordTy, vars)
             end
         else
             (ct, RecordType(row'), vars)
      end
  | constraintsFromPat(ctx, env, ConPat(longvid, pat)) = raise TypeError "ConPat"
  | constraintsFromPat(ctx, env, TypedPat(WildcardPat, ty))
    = ([], ty, Syntax.VIdMap.empty)
  | constraintsFromPat(ctx, MkEnv env, TypedPat(VarPat(vid), ty)) (* fresh variable? *)
    = (case Syntax.VIdMap.find(#valMap env, vid) of
           SOME (tysc, Syntax.ValueConstructor) => raise TypeError "VarPat: invalid pattern"
         | SOME (tysc, Syntax.ExceptionConstructor) => raise TypeError "VarPat: invalid pattern"
         | SOME (_, Syntax.ValueVariable) => (* shadowing *) ([], ty, Syntax.VIdMap.insert(Syntax.VIdMap.empty, vid, (TypeScheme([], ty), Syntax.ValueVariable)))
         | NONE => ([], ty, Syntax.VIdMap.insert(Syntax.VIdMap.empty, vid, (TypeScheme([], ty), Syntax.ValueVariable)))
      )
  | constraintsFromPat(ctx, env, TypedPat(pat, ty))
    = let val (ct, inferredTy, vars) = constraintsFromPat(ctx, env, pat)
      in (EqConstr(ty, inferredTy) :: ct, ty, vars)
      end
  | constraintsFromPat(ctx, env, LayeredPat(vid, SOME ty, pat))
    = let val (ct, inferredTy, vars) = constraintsFromPat(ctx, env, pat)
      in case Syntax.VIdMap.find(vars, vid) of
             NONE => (EqConstr(ty, inferredTy) :: ct, ty, Syntax.VIdMap.insert(vars, vid, (TypeScheme([], ty), Syntax.ValueVariable)))
           | SOME _ => raise TypeError "trying to bind the same identifier twice"
      end
  | constraintsFromPat(ctx, env, LayeredPat(vid, NONE, pat))
    = let val (ct, inferredTy, vars) = constraintsFromPat(ctx, env, pat)
      in case Syntax.VIdMap.find(vars, vid) of
             NONE => (ct, inferredTy, Syntax.VIdMap.insert(vars, vid, (TypeScheme([], inferredTy), Syntax.ValueVariable)))
           | SOME _ => raise TypeError "trying to bind the same identifier twice"
      end
 (* constraintsFromPatRow : Ctx * Env * (Label * Pat) list -> Constraint list * (Label * Syntax.Ty) list * Syntax.Ty Syntax.VIdMap.map *)
and constraintsFromPatRow(ctx, env, row)
    = let fun oneField((label, pat), (cts, row, vars))
              = let val (ct, ty, vars') = constraintsFromPat(ctx, env, pat)
                in (ct @ cts, (label, ty) :: row, Syntax.VIdMap.unionWith (fn _ => raise TypeError "trying to bind the same identifier twice") (vars, vars'))
                end
      in List.foldl oneField ([], [], Syntax.VIdMap.empty) row
      end

(* typeCheckExp : Context * Env * USyntax.Exp -> Subst * (TyVar * TyVarConstraint) list * USyntax.Ty * USyntax.Exp *)
fun typeCheckExp(ctx, env, exp) = let val (constraints, ty) = constraintsExp(ctx, env, exp)
                                      val (subst, tvc) = unify(ctx, [], constraints)
                                      val applySubst = applySubstTy subst
                                  in (subst, tvc, applySubst ty, USyntax.mapTyInExp applySubst exp)
                                  end
end (* local *)
end (* structure Typing *)
