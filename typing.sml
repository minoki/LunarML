structure Typing = struct
datatype TypeFcn = TypeFcn of USyntax.TyVar list * USyntax.Ty
datatype TypeScheme = TypeScheme of USyntax.TyVar list * USyntax.Ty
type ValEnv = (TypeScheme * Syntax.IdStatus) Syntax.VIdMap.map
val emptyValEnv = Syntax.VIdMap.empty
datatype TyStr = TyStr of TypeFcn * ValEnv

datatype Env = MkEnv of { tyMap : TyStr Syntax.TyConMap.map
                        , valMap : (TypeScheme * Syntax.IdStatus) Syntax.VIdMap.map
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

type Subst = USyntax.Ty USyntax.TyVarMap.map

fun freeTyVarsInTypeScheme(bound, TypeScheme(tyvars, ty)) = USyntax.freeTyVarsInTy(USyntax.TyVarSet.addList(bound, tyvars), ty)
fun freeTyVarsInEnv(bound, MkEnv { tyMap = tyMap, valMap = valMap, strMap = strMap })
    = let val valMapSet = Syntax.VIdMap.foldl (fn ((tysc, _), set) => USyntax.TyVarSet.union(set, freeTyVarsInTypeScheme(bound, tysc))) USyntax.TyVarSet.empty valMap
          (* TODO: tyMap? *)
      in Syntax.StrIdMap.foldl (fn (env, set) => USyntax.TyVarSet.union(set, freeTyVarsInEnv(bound, env))) valMapSet strMap
      end
fun freeTyVarsInConstraint(bound, EqConstr(ty1, ty2)) = USyntax.TyVarSet.union(USyntax.freeTyVarsInTy(bound, ty1), USyntax.freeTyVarsInTy(bound, ty2))
  | freeTyVarsInConstraint(bound, FieldConstr{recordTy = recordTy, fieldTy = fieldTy, ...}) = USyntax.TyVarSet.union(USyntax.freeTyVarsInTy(bound, recordTy), USyntax.freeTyVarsInTy(bound, fieldTy))
  | freeTyVarsInConstraint(bound, IsEqType ty) = USyntax.freeTyVarsInTy(bound, ty)
fun freeTyVarsInTyVarConstraint(bound, TVFieldConstr{fieldTy = fieldTy, ...}) = USyntax.freeTyVarsInTy(bound, fieldTy)
  | freeTyVarsInTyVarConstraint(bound, TVIsEqType) = USyntax.TyVarSet.empty

fun substToConstraints(subst : Subst) : Constraint list
    = USyntax.TyVarMap.foldli (fn (tv, ty, cts) => EqConstr(USyntax.TyVar(tv), ty) :: cts) [] subst

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
    = (case Syntax.TyConMap.find(#tyMap env, tycon) of
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
fun isNonexpansive(env : Env, USyntax.SConExp _) = true
  | isNonexpansive(env, USyntax.VarExp _) = true (* <op> longvid *)
  | isNonexpansive(env, USyntax.RecordExp fields) = List.all (fn (_, e) => isNonexpansive(env, e)) fields
  | isNonexpansive(env, USyntax.TypedExp(e, _)) = isNonexpansive(env, e)
  | isNonexpansive(env, USyntax.AppExp(conexp, e)) = isConexp(env, conexp) andalso isNonexpansive(env, e)
  | isNonexpansive(env, USyntax.FnExp _) = true
  | isNonexpansive(env, _) = false
and isConexp(env : Env, USyntax.TypedExp(e, _)) = isConexp(env, e)
  | isConexp(env, USyntax.VarExp(Syntax.MkLongVId([], Syntax.MkVId "ref"), _)) = false
  | isConexp(env, USyntax.VarExp(_, Syntax.ValueVariable)) = false
  | isConexp(env, USyntax.VarExp(_, Syntax.ValueConstructor)) = true
  | isConexp(env, USyntax.VarExp(_, Syntax.ExceptionConstructor)) = true
  | isConexp(env, _) = false

(* isExhaustive : Env * USyntax.Pat -> bool *)
fun isExhaustive(env : Env, USyntax.WildcardPat) = true
  | isExhaustive(env, USyntax.SConPat _) = false
  | isExhaustive(env, USyntax.VarPat _) = true
  | isExhaustive(env, USyntax.NulConPat longvid) = false (* TODO: Check if this if the sole constructor of the type *)
  | isExhaustive(env, USyntax.RecordPat(row, _)) = List.all (fn (_, e) => isExhaustive(env, e)) row
  | isExhaustive(env, USyntax.ConPat(longvid, innerPat)) = false andalso isExhaustive(env, innerPat) (* TODO: Check if this if the sole constructor of the type *)
  | isExhaustive(env, USyntax.TypedPat(innerPat, _)) = isExhaustive(env, innerPat)
  | isExhaustive(env, USyntax.LayeredPat(_, _, innerPat)) = isExhaustive(env, innerPat)

val primTyCon_int    = USyntax.ULongTyCon(Syntax.MkLongTyCon([], Syntax.MkTyCon "int"), 0)
val primTyCon_word   = USyntax.ULongTyCon(Syntax.MkLongTyCon([], Syntax.MkTyCon "word"), 1)
val primTyCon_real   = USyntax.ULongTyCon(Syntax.MkLongTyCon([], Syntax.MkTyCon "real"), 2)
val primTyCon_string = USyntax.ULongTyCon(Syntax.MkLongTyCon([], Syntax.MkTyCon "string"), 3)
val primTyCon_char   = USyntax.ULongTyCon(Syntax.MkLongTyCon([], Syntax.MkTyCon "char"), 4)
val primTyCon_exn    = USyntax.ULongTyCon(Syntax.MkLongTyCon([], Syntax.MkTyCon "exn"), 5)
val primTyCon_bool   = USyntax.ULongTyCon(Syntax.MkLongTyCon([], Syntax.MkTyCon "bool"), 6)
val primTyCon_ref    = USyntax.ULongTyCon(Syntax.MkLongTyCon([], Syntax.MkTyCon "ref"), 7)
val primTyCon_list    = USyntax.ULongTyCon(Syntax.MkLongTyCon([], Syntax.MkTyCon "list"), 8)
val primTy_unit   = USyntax.RecordType []
val primTy_int    = USyntax.TyCon([], primTyCon_int)
val primTy_word   = USyntax.TyCon([], primTyCon_word)
val primTy_real   = USyntax.TyCon([], primTyCon_real)
val primTy_string = USyntax.TyCon([], primTyCon_string)
val primTy_char   = USyntax.TyCon([], primTyCon_char)
val primTy_exn    = USyntax.TyCon([], primTyCon_exn)
val primTy_bool   = USyntax.TyCon([], primTyCon_bool)

val emptyEnv : Env
    = MkEnv { tyMap = Syntax.TyConMap.empty
            , valMap = Syntax.VIdMap.empty
            , strMap = Syntax.StrIdMap.empty
            }
val initialEnv : Env
    = let open Syntax
          val mkTyMap = List.foldl Syntax.TyConMap.insert' Syntax.TyConMap.empty
          val mkValMap = List.foldl Syntax.VIdMap.insert' Syntax.VIdMap.empty
          val tyVarA = USyntax.UTyVar(Syntax.MkTyVar("a"), 0)
      in MkEnv { tyMap = mkTyMap
                             [(MkTyCon "unit", TyStr(TypeFcn([], primTy_unit), emptyValEnv))
                             ,(MkTyCon "bool", TyStr(TypeFcn([], primTy_bool)
                                                    , mkValMap [(MkVId "true", (TypeScheme ([], primTy_bool), ValueConstructor))
                                                               ,(MkVId "false", (TypeScheme ([], primTy_bool), ValueConstructor))
                                                               ]))
                             ,(MkTyCon "int", TyStr(TypeFcn([], primTy_int), emptyValEnv))
                             ,(MkTyCon "word", TyStr(TypeFcn([], primTy_word), emptyValEnv))
                             ,(MkTyCon "real", TyStr(TypeFcn([], primTy_real), emptyValEnv))
                             ,(MkTyCon "string", TyStr(TypeFcn([], primTy_string), emptyValEnv))
                             ,(MkTyCon "char", TyStr(TypeFcn([], primTy_char), emptyValEnv))
                             ,(MkTyCon "list", TyStr(TypeFcn([tyVarA], USyntax.TyCon([USyntax.TyVar(tyVarA)], primTyCon_list))
                                                    , mkValMap [(MkVId "nil", (TypeScheme ([tyVarA], USyntax.TyCon([USyntax.TyVar(tyVarA)], primTyCon_list)), ValueConstructor))
                                                               ,(MkVId "::", (TypeScheme ([tyVarA], USyntax.FnType(USyntax.PairType(USyntax.TyVar(tyVarA), USyntax.TyCon([USyntax.TyVar(tyVarA)], primTyCon_list)), USyntax.TyCon([USyntax.TyVar(tyVarA)], primTyCon_list))), ValueConstructor))
                                                               ]))
                             ,(MkTyCon "ref", TyStr(TypeFcn([tyVarA], USyntax.TyCon([USyntax.TyVar(tyVarA)], primTyCon_ref))
                                                   , mkValMap [(MkVId "ref", (TypeScheme ([tyVarA], USyntax.FnType(USyntax.TyVar(tyVarA), USyntax.TyCon([USyntax.TyVar(tyVarA)], primTyCon_ref))), ValueConstructor))
                                                              ]))
                             ,(MkTyCon "exn", TyStr(TypeFcn([], primTy_exn), emptyValEnv))
                             ]
               , valMap = mkValMap
                              (* C Appendix: The Initial Static Basis *)
                              [(MkVId "ref", (TypeScheme ([tyVarA], USyntax.FnType(USyntax.TyVar(tyVarA), USyntax.TyCon([USyntax.TyVar(tyVarA)], primTyCon_ref))), ValueConstructor)) (* forall 'a. 'a -> 'a ref *)
                              ,(MkVId "nil", (TypeScheme ([tyVarA], USyntax.TyCon([USyntax.TyVar(tyVarA)], primTyCon_list)), ValueConstructor)) (* forall 'a. 'a list *)
                              ,(MkVId "true", (TypeScheme ([], primTy_bool), ValueConstructor))
                              ,(MkVId "false", (TypeScheme ([], primTy_bool), ValueConstructor))
                              ,(MkVId "Match", (TypeScheme ([], primTy_exn), ExceptionConstructor))
                              ,(MkVId "Bind", (TypeScheme ([], primTy_exn), ExceptionConstructor))
                              ,(MkVId "::", (TypeScheme ([tyVarA], USyntax.FnType(USyntax.PairType(USyntax.TyVar(tyVarA), USyntax.TyCon([USyntax.TyVar(tyVarA)], primTyCon_list)), USyntax.TyCon([USyntax.TyVar(tyVarA)], primTyCon_list))), ValueConstructor)) (* forall 'a. 'a * 'a list -> 'a list *)
                              (* ,(MkVId "=", (TypeScheme ([], _), ValueVariable)) *) (* forall ''a. ''a * ''a -> bool *)
                              ,(MkVId ":=", (TypeScheme ([tyVarA], USyntax.FnType(USyntax.PairType(USyntax.TyCon([USyntax.TyVar(tyVarA)], primTyCon_ref), USyntax.TyVar(tyVarA)), primTy_unit)), ValueVariable)) (* forall 'a. 'a ref * 'a -> {} *)
                              (* Overloaded identifiers *)
                               (*
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
                               *)
                              ]
               , strMap = Syntax.StrIdMap.empty
               }
      end
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
            = (case USyntax.TyVarMap.find(subst, tv') of
                   NONE => ty
                 | SOME replacement => replacement (* TODO: single replacement is sufficient? *)
              )
                  (*
            = (case List.find (fn (tv, _) => eqUTyVar(tv, tv')) subst of
                   NONE => ty
                 | SOME (_, replacement) => replacement (* TODO: single replacement is sufficient? *)
              )*)
          | substTy (RecordType fields) = RecordType (List.map (fn (label, ty) => (label, substTy ty)) fields)
          | substTy (TyCon(tyargs, longtycon)) = TyCon(List.map substTy tyargs, longtycon)
          | substTy (FnType(ty1, ty2)) = FnType(substTy ty1, substTy ty2)
    in substTy
    end
fun applySubstEnv subst =
    let val substTy = applySubstTy subst
        fun substTypeScheme(TypeScheme(tyvars, ty))
            = let val subst' = USyntax.TyVarMap.filteri (fn (tv, ty) => not (List.exists (fn tv' => eqUTyVar(tv', tv)) tyvars)) subst
              in TypeScheme(tyvars, applySubstTy subst' ty)
                 (* TODO: unwanted capture? e.g. 'a. 'a list * 'c, 'c := 'b * 'a *)
              end
        fun substEnv (MkEnv { tyMap = tyMap, valMap = valMap, strMap = strMap })
            = MkEnv { tyMap = tyMap (* ??? *)
                    , valMap = Syntax.VIdMap.map (fn (tysc, ids) => (substTypeScheme(tysc), ids)) valMap
                    , strMap = Syntax.StrIdMap.map substEnv strMap
                    }
    in substEnv
    end

(* instantiate : Context * TypeScheme -> Ty *)
fun instantiate(ctx, TypeScheme(vars, ty))
    = let val subst = List.foldl (fn (v, set) => USyntax.TyVarMap.insert(set, v, TyVar(freshTyVar(ctx)))) USyntax.TyVarMap.empty vars
      in applySubstTy subst ty
      end

(* mergeEnv : Env * Env -> Env *)
fun mergeEnv(MkEnv env1, MkEnv env2) = MkEnv { tyMap = Syntax.TyConMap.unionWith #2 (#tyMap env1, #tyMap env2)
                                             , valMap = Syntax.VIdMap.unionWith #2 (#valMap env1, #valMap env2)
                                             , strMap = Syntax.StrIdMap.unionWith #2 (#strMap env1, #strMap env2) (* TODO *)
                                             }

 (* unify : Context * (TyVar * TyVarConstraint) list * Constraint list -> Subst * (TyVar * TyVarConstraint) list *)
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
  | unify(ctx, tvc, nil) = (USyntax.TyVarMap.empty, tvc)
and unifyTyVarAndTy(ctx : Context, tvc : (TyVar * TyVarConstraint) list, tv : TyVar, ty : Ty, ctrs : Constraint list)
    = if (case ty of TyVar(tv') => eqUTyVar(tv, tv') | _ => false) then (* ty = TyVar tv *)
          (USyntax.TyVarMap.empty, []) (* do nothing *)
      else if occurCheck tv ty then
          raise TypeError("unification failed: occurrence check (" ^ USyntax.print_TyVar tv ^ " in " ^ USyntax.print_Ty ty ^ ")")
      else
          let val (e, tvc') = List.partition (fn (tv', c) => eqUTyVar(tv, tv')) tvc
              fun toConstraint (_, TVFieldConstr { label = label, fieldTy = fieldTy }) = FieldConstr { label = label, recordTy = ty, fieldTy = fieldTy }
                | toConstraint (_, TVIsEqType) = IsEqType ty
              val (ss, tvc'') = unify(ctx, tvc', List.map toConstraint e @ List.map (substituteConstraint (tv, ty)) ctrs)
          in (USyntax.TyVarMap.insert(ss, tv, ty), tvc'')
          end

(* constraintsExp : Context * Env * Constraint list * USyntax.Exp -> Constraint list * USyntax.Ty *)
fun constraintsExp(ctx : Context, env : Env, cts : Constraint list, SConExp(scon))
    = (case scon of (* TODO: overloaded literals *)
           Syntax.IntegerConstant x   => (cts, primTy_int)
         | Syntax.WordConstant x      => (cts, primTy_word)
         | Syntax.RealConstant x      => (cts, primTy_real)
         | Syntax.StringConstant x    => (cts, primTy_string)
         | Syntax.CharacterConstant x => (cts, primTy_char)
      )
  | constraintsExp(ctx, env, cts, VarExp(Syntax.MkLongVId(str, vid as Syntax.MkVId name), idstatus))
    = (case lookupValInEnv(lookupStr(env, str), vid) of
          (TypeScheme([], ty), ids) => (cts, ty)
        | (tysc, _) => (cts, instantiate(ctx, tysc))
      )
  | constraintsExp(ctx, env, cts, RecordExp(row))
    = let val (cts', row') = constraintsFromRow(ctx, env, cts, row)
      in (cts', RecordType(row'))
      end
  | constraintsExp(ctx, env, cts, LetInExp(decls, innerExp))
    = let val (cts, env') = constraintsDecl(ctx, env, cts, decls)
          val (cts, innerTy) = constraintsExp(ctx, mergeEnv(env, env'), cts, innerExp)
      in (cts, innerTy)
      end
  | constraintsExp(ctx, env, cts, AppExp(f, x))
          (* f: s -> t, x: s *)
    = let val (cts, funcTy) = constraintsExp(ctx, env, cts, f)
          val (cts, argTy) = constraintsExp(ctx, env, cts, x)
          val retTy = TyVar(freshTyVar(ctx))
          (* funcTy = (argTy -> retTy) *)
          val ct = EqConstr(funcTy, FnType(argTy, retTy))
      in (ct :: cts, retTy)
      end
  | constraintsExp(ctx, env, cts, TypedExp(exp, ty))
    = let val (cts, expTy) = constraintsExp(ctx, env, cts, exp)
          val ct = EqConstr(expTy, ty) (* ety = ty *)
      in (ct :: cts, ty)
      end
  | constraintsExp(ctx, env, cts, HandleExp(e, handlers))
          (* e: t, handlers: exn -> t *)
    = raise Fail "handle expression not implemented yet"
  | constraintsExp(ctx, env, cts, RaiseExp(exp))
    = let val (cts, expTy) = constraintsExp(ctx, env, cts, exp)
          (* expTy = exn *)
          val ct = EqConstr(expTy, primTy_exn)
          val retTy = TyVar(freshTyVar(ctx))
      in (ct :: cts, retTy)
      end
  | constraintsExp(ctx, env, cts, IfThenElseExp(cond, thenPart, elsePart))
    = let val (cts, condTy) = constraintsExp(ctx, env, cts, cond)
          val (cts, thenTy) = constraintsExp(ctx, env, cts, thenPart)
          val (cts, elseTy) = constraintsExp(ctx, env, cts, elsePart)
          (* condTy = bool *)
          val ect1 = EqConstr(condTy, primTy_bool)
          (* thenTy = elseTy *)
          val ect2 = EqConstr(thenTy, elseTy)
      in (ect1 :: ect2 :: cts, thenTy)
      end
  | constraintsExp(ctx, env, cts, CaseExp(exp, matches))
    = let val (cts, expTy) = constraintsExp(ctx, env, cts, exp)
          val (cts, patTy, retTy) = constraintsFromMatch(ctx, env, cts, matches)
      in (EqConstr(expTy, patTy) :: cts, retTy)
      end
  | constraintsExp(ctx, env, cts, FnExp(matches))
    = let val (cts, argTy, retTy) = constraintsFromMatch(ctx, env, cts, matches)
      in (cts, USyntax.FnType(argTy, retTy))
      end
(* constraintsDecl : Context * Env * Constraints list * Dec list -> Constraints list * Env *)
and constraintsDecl(ctx, env, cts, nil) = (cts, env)
  | constraintsDecl(ctx, env, cts, decl :: decls)
    = (case decl of
           ValDec(tyvarseq, valbinds) => let val MkEnv { valMap = valMap, tyMap = tyMap, strMap = strMap } = env
                                             val (cts, vars) = constraintsValBinds(ctx, env, cts, Syntax.VIdMap.empty, valbinds)
                                             val (subst, tyvarconstraint) = unify(ctx, [], cts)
                                             fun doVar(ty, false) = (TypeScheme([], ty), Syntax.ValueVariable)
                                               | doVar(ty, true) = let val f_ty = freeTyVarsInTy(TyVarSet.empty, ty)
                                                                       val f_env = freeTyVarsInEnv(TyVarSet.empty, env)
                                                                   in (TypeScheme([], ty), Syntax.ValueVariable)
                                                                   end
                                             val valMap' = Syntax.VIdMap.map doVar vars
                                             val env' = MkEnv { valMap = Syntax.VIdMap.unionWith #2 (valMap, valMap')
                                                              , tyMap = tyMap
                                                              , strMap = strMap
                                                              }
                                         in constraintsDecl(ctx, env', cts, decls)
                                         end
         | RecValDec(tyvarseq, valbinds) => raise Fail "let-in: val rec: not impl"
         | TypeDec(_) => raise Fail "let-in: type: not impl"
         | DatatypeDec(_) => raise Fail "let-in: datatype: not impl"
         | DatatypeRepDec(_) => raise Fail "let-in: datatype rep: not impl"
         | AbstypeDec(_) => raise Fail "let-in: abstype: not impl"
         | ExceptionDec(_) => raise Fail "let-in: exception: not impl"
         | LocalDec(localDecls, decls') => let val (cts, env') = constraintsDecl(ctx, env, cts, localDecls)
                                               val (cts, env'') = constraintsDecl(ctx, mergeEnv(env, env'), cts, decls')
                                           in constraintsDecl(ctx, mergeEnv(env, env''), cts, decls)
                                           end
         | OpenDec(_) => raise Fail "let-in: open: not impl"
      )
(* constraintsValBinds : Context * Env * (USyntax.Ty * bool) Syntax.VIdMap.map * ValBind list * Constraints list *)
and constraintsValBinds(ctx, env, cts, vars, []) = (cts, vars)
  | constraintsValBinds(ctx, env, cts, vars, PatBind(pat, exp) :: rest)
    = let val (cts, patTy, newVars) = constraintsFromPat(ctx, env, cts, pat)
          val (cts, expTy) = constraintsExp(ctx, env, cts, exp)
          val generalize = isExhaustive(env, pat) andalso isNonexpansive(env, exp)
          val vars' = Syntax.VIdMap.unionWith #2 (vars, Syntax.VIdMap.map (fn ty => (ty, generalize)) newVars) (* TODO: generalize *)
      in constraintsValBinds(ctx, env, EqConstr(patTy, expTy) :: cts, vars', rest)
      end
(* constraintsFromRow : Ctx * Env * Constraint list * (Label * Exp) list -> Constraint list * (Label * Syntax.Ty) list *)
and constraintsFromRow(ctx, env, cts, xs)
    = let fun oneField((label, exp), (cts, xs))
              = let val (cts, ty) = constraintsExp(ctx, env, cts, exp)
                in (cts, (label, ty) :: xs)
                end
      in List.foldl oneField (cts, []) xs
      end
 (* constraintsFromMatch : Ctx * Env * Constraint list * (Pat * Exp) list -> Constraint list * (* pattern *) Syntax.Ty * (* expression *) Syntax.Ty *)
and constraintsFromMatch(ctx, env, cts, (pat0, exp0) :: rest)
    = let val (cts, patTy, expTy) = constraintsFromMatchBranch(ctx, env, cts, pat0, exp0)
          fun oneBranch((pat, exp), cts)
              = let val (cts, patTy', expTy') = constraintsFromMatchBranch(ctx, env, cts, pat, exp)
                in EqConstr(patTy, patTy') :: EqConstr(expTy, expTy') :: cts
                end
          val cts = List.foldl oneBranch cts rest
      in (cts, patTy, expTy)
      end
  | constraintsFromMatch(ctx, env, cts, nil) = raise TypeError "invalid syntax tree: match is empty"
and constraintsFromMatchBranch(ctx : Context, env as MkEnv env' : Env, cts, pat, exp)
    = let val (cts, patTy, vars) = constraintsFromPat(ctx, env, cts, pat)
          val env'' = MkEnv { tyMap = #tyMap env'
                            , valMap = Syntax.VIdMap.unionWith #2 (#valMap env', Syntax.VIdMap.map (fn ty => (TypeScheme([], ty), Syntax.ValueVariable)) vars)
                            , strMap = #strMap env'
                            }
          val (cts, expTy) = constraintsExp(ctx, env'', cts, exp)
      in (cts, patTy, expTy)
      end
 (* constraintsFromPat : Ctx * Env * Constraint list * Pat -> Constraint list * USyntax.Ty * USyntax.Ty Syntax.VIdMap.map *)
and constraintsFromPat(ctx, env, cts, WildcardPat) : Constraint list * USyntax.Ty * USyntax.Ty Syntax.VIdMap.map
    = let val ty = TyVar(freshTyVar(ctx))
      in (cts, ty, Syntax.VIdMap.empty)
      end
  | constraintsFromPat(ctx, env, cts, SConPat scon)
    = (case scon of
           Syntax.IntegerConstant(_)   => (cts, primTy_int, Syntax.VIdMap.empty)
         | Syntax.WordConstant(_)      => (cts, primTy_word, Syntax.VIdMap.empty)
         | Syntax.RealConstant(_)      => raise Syntax.SyntaxError "No real constant may occur in a pattern"
         | Syntax.StringConstant(_)    => (cts, primTy_string, Syntax.VIdMap.empty)
         | Syntax.CharacterConstant(_) => (cts, primTy_char, Syntax.VIdMap.empty)
      )
  | constraintsFromPat(ctx, MkEnv env, cts, VarPat(vid, ty))
    = (case Syntax.VIdMap.find(#valMap env, vid) of
           SOME (tysc, Syntax.ValueConstructor) => raise TypeError "VarPat: invalid pattern"
         | SOME (tysc, Syntax.ExceptionConstructor) => raise TypeError "VarPat: invalid pattern"
         | SOME (_, Syntax.ValueVariable) => (* shadowing *) (cts, ty, Syntax.VIdMap.insert(Syntax.VIdMap.empty, vid, ty))
         | NONE => (cts, ty, Syntax.VIdMap.insert(Syntax.VIdMap.empty, vid, ty))
      )
  | constraintsFromPat(ctx, env, cts, NulConPat(Syntax.MkLongVId(strid, vid)))
    = let val MkEnv strEnv = lookupStr(env, strid)
      in case Syntax.VIdMap.find(#valMap strEnv, vid) of
             SOME (tysc, Syntax.ValueConstructor) => raise Fail "NulConPat: not implemented yet"
           | SOME (tysc, Syntax.ExceptionConstructor) => (cts, primTy_exn, Syntax.VIdMap.empty)
           | SOME (_, Syntax.ValueVariable) => raise TypeError "invalid pattern"
           | NONE => raise TypeError "invalid pattern"
      end
  | constraintsFromPat(ctx, env, cts, RecordPat(row, wildcard))
    = let val (cts, row', vars) = constraintsFromPatRow(ctx, env, cts, row)
      in if wildcard then
             let val recordTy = TyVar(freshTyVar(ctx))
                 fun oneField((label, ty), cts) = FieldConstr { label = label, recordTy = recordTy, fieldTy = ty } :: cts
                 val cts = List.foldl oneField cts row'
             in (cts, recordTy, vars)
             end
         else
             (cts, RecordType(row'), vars)
      end
  | constraintsFromPat(ctx, env, cts, ConPat(longvid, pat)) = raise TypeError "ConPat"
  | constraintsFromPat(ctx, env, cts, TypedPat(WildcardPat, ty))
    = (cts, ty, Syntax.VIdMap.empty)
  | constraintsFromPat(ctx, env, cts, TypedPat(pat, ty))
    = let val (cts, inferredTy, vars) = constraintsFromPat(ctx, env, cts, pat)
      in (EqConstr(ty, inferredTy) :: cts, ty, vars)
      end
  | constraintsFromPat(ctx, env, cts, LayeredPat(vid, ty, pat))
    = let val (cts, inferredTy, vars) = constraintsFromPat(ctx, env, cts, pat)
      in case Syntax.VIdMap.find(vars, vid) of
             NONE => (EqConstr(ty, inferredTy) :: cts, ty, Syntax.VIdMap.insert(vars, vid, ty))
           | SOME _ => raise TypeError "trying to bind the same identifier twice"
      end
 (* constraintsFromPatRow : Ctx * Env * Constraint list * (Label * Pat) list -> Constraint list * (Label * Syntax.Ty) list * Syntax.Ty Syntax.VIdMap.map *)
and constraintsFromPatRow(ctx, env, cts, row)
    = let fun oneField((label, pat), (cts, row, vars))
              = let val (cts, ty, vars') = constraintsFromPat(ctx, env, cts, pat)
                in (cts, (label, ty) :: row, Syntax.VIdMap.unionWith (fn _ => raise TypeError "trying to bind the same identifier twice") (vars, vars'))
                end
      in List.foldl oneField (cts, [], Syntax.VIdMap.empty) row
      end

(* typeCheckExp : Context * Env * USyntax.Exp -> Subst * (TyVar * TyVarConstraint) list * USyntax.Ty * USyntax.Exp *)
fun typeCheckExp(ctx, env, exp) = let val (constraints, ty) = constraintsExp(ctx, env, [], exp)
                                      val (subst, tvc) = unify(ctx, [], constraints)
                                      val applySubst = applySubstTy subst
                                  in (subst, tvc, applySubst ty, USyntax.mapTyInExp applySubst exp)
                                  end
end (* local *)
end (* structure Typing *)
