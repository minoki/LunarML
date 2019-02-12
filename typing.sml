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
               , constraints : (Constraint list) ref
               , tyVarConstraints : ((USyntax.TyVar * TyVarConstraint) list) ref (* should use (multi-)map? *)
               , tyVarSubst : Subst ref
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
      , constraints = ref []
      , tyVarConstraints = ref []
      , tyVarSubst = ref USyntax.TyVarMap.empty
      }

fun addConstraint(ctx : Context, ct : Constraint)
    = let val cts = !(#constraints ctx)
      in #constraints ctx := ct :: cts
      end
fun addTyVarConstraint(ctx : Context, tv : USyntax.TyVar, ct : TyVarConstraint)
    = let val cts = !(#tyVarConstraints ctx)
      in #tyVarConstraints ctx := (tv, ct) :: cts
      end

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

fun addSubst(ctx : Context, tv : USyntax.TyVar, ty : USyntax.Ty)
    = let val subst = !(#tyVarSubst ctx)
          val subst' = USyntax.TyVarMap.map (substituteTy (tv, ty)) subst
      in #tyVarSubst ctx := USyntax.TyVarMap.insert(subst', tv, ty)
      end

(* applySubstTy : Subst -> Ty -> Ty *)
fun applySubstTy subst =
    let fun substTy (ty as TyVar tv')
            = (case USyntax.TyVarMap.find(subst, tv') of
                   NONE => ty
                 | SOME replacement => replacement (* TODO: single replacement is sufficient? *)
              )
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

 (* unify : Context * Constraint list -> unit *)
fun unify(ctx : Context, nil : Constraint list) : unit = ()
  | unify(ctx, ct :: ctrs)
    = (case ct of
           EqConstr(TyVar(tv), ty) => unifyTyVarAndTy(ctx, tv, ty, ctrs)
         | EqConstr(ty, TyVar(tv)) => unifyTyVarAndTy(ctx, tv, ty, ctrs)
         | EqConstr(FnType(s0, s1), FnType(t0, t1)) => unify(ctx, EqConstr(s0, t0) :: EqConstr(s1, t1) :: ctrs)
         | EqConstr(RecordType(fields), RecordType(fields')) =>
           if List.length fields <> List.length fields then
               raise TypeError("unification failed: incompatible record types (different number of fields)")
           else
               unify(ctx, List.foldl (fn ((label, ty), acc) => case List.find (fn (label', _) => label = label') fields' of
                                                                   NONE => raise TypeError("unification failed: incompatible record types")
                                                                 | SOME(_,ty') => EqConstr(ty, ty') :: acc)
                                     ctrs fields)
         | EqConstr(TyCon(tyarg, con), TyCon(tyarg', con')) =>
           if eqULongTyCon(con, con') then
               unify(ctx, (ListPair.mapEq EqConstr (tyarg, tyarg')
                           handle ListPair.UnequalLengths => raise TypeError("unification failed: the number of type arguments differ")
                          ) @ ctrs)
           else
               raise TypeError("unification failed: type constructor mismatch") (* ??? *)
         | EqConstr(_, _) => raise TypeError("unification failed: not match")
         | FieldConstr{label = label, recordTy = recordTy, fieldTy = fieldTy} =>
           (case recordTy of
                RecordType(fields) =>
                (case List.find (fn (label', _) => label = label') fields of
                     NONE => raise TypeError("unification failed: no field")
                   | SOME(_, ty') => unify(ctx, EqConstr(fieldTy, ty') :: ctrs)
                )
              | TyCon(_, _) => raise TypeError("record field for a non-record type")
              | FnType(_, _) => raise TypeError("record field for a function type")
              | TyVar tv =>
                ( addTyVarConstraint(ctx, tv, TVFieldConstr { label = label, fieldTy = fieldTy })
                ; unify(ctx, ctrs)
                )
           )
         | IsEqType(RecordType fields) => unify(ctx, List.map (fn (label, ty) => IsEqType ty) fields @ ctrs)
         | IsEqType(FnType _) => raise TypeError("function type does not admit equality")
         | IsEqType(TyCon(tyargs, longtycon)) =>
           if eqULongTyCon(longtycon, primTyCon_ref) then
               unify(ctx, ctrs) (* do nothing *)
           else
               (* (longtycon???) : List.map IsEqType tyargs @ ctrs *)
               raise Fail "IsEqType TyCon: not impl"
         | IsEqType(TyVar(tv)) => (addTyVarConstraint(ctx, tv, TVIsEqType) ; unify(ctx, ctrs))
      )
and unifyTyVarAndTy(ctx : Context, tv : TyVar, ty : Ty, ctrs : Constraint list) : unit
    = if (case ty of TyVar(tv') => eqUTyVar(tv, tv') | _ => false) then (* ty = TyVar tv *)
          unify(ctx, ctrs) (* do nothing *)
      else if occurCheck tv ty then
          raise TypeError("unification failed: occurrence check (" ^ USyntax.print_TyVar tv ^ " in " ^ USyntax.print_Ty ty ^ ")")
      else
          let val tvc = !(#tyVarConstraints ctx)
              val (e, tvc') = List.partition (fn (tv', c) => eqUTyVar(tv, tv')) tvc
              val () = #tyVarConstraints ctx := tvc'
              fun toConstraint (_, TVFieldConstr { label = label, fieldTy = fieldTy }) = FieldConstr { label = label, recordTy = ty, fieldTy = fieldTy }
                | toConstraint (_, TVIsEqType) = IsEqType ty
          in addSubst(ctx, tv, ty)
           ; unify(ctx, List.map toConstraint e @ List.map (substituteConstraint (tv, ty)) ctrs)
          end

(* constraintsExp : Context * Env * USyntax.Exp -> USyntax.Ty *)
fun constraintsExp(ctx : Context, env : Env, SConExp(scon)) : USyntax.Ty
    = (case scon of (* TODO: overloaded literals *)
           Syntax.IntegerConstant x   => primTy_int
         | Syntax.WordConstant x      => primTy_word
         | Syntax.RealConstant x      => primTy_real
         | Syntax.StringConstant x    => primTy_string
         | Syntax.CharacterConstant x => primTy_char
      )
  | constraintsExp(ctx, env, VarExp(Syntax.MkLongVId(str, vid as Syntax.MkVId name), idstatus))
    = (case lookupValInEnv(lookupStr(env, str), vid) of
          (TypeScheme([], ty), ids) => ty
        | (tysc, _) => instantiate(ctx, tysc)
      )
  | constraintsExp(ctx, env, RecordExp(row))
    = let val row' = constraintsFromRow(ctx, env, row)
      in RecordType(row')
      end
  | constraintsExp(ctx, env, LetInExp(decls, innerExp))
    = let val env' = constraintsDecl(ctx, env, decls)
      in constraintsExp(ctx, mergeEnv(env, env'), innerExp)
      end
  | constraintsExp(ctx, env, AppExp(f, x))
          (* f: s -> t, x: s *)
    = let val funcTy = constraintsExp(ctx, env, f)
          val argTy = constraintsExp(ctx, env, x)
          val retTy = TyVar(freshTyVar(ctx))
      in addConstraint(ctx, EqConstr(funcTy, FnType(argTy, retTy))) (* funcTy = (argTy -> retTy) *)
       ; retTy
      end
  | constraintsExp(ctx, env, TypedExp(exp, ty))
    = let val expTy = constraintsExp(ctx, env, exp)
      in addConstraint(ctx, EqConstr(expTy, ty)) (* ety = ty *)
       ; ty
      end
  | constraintsExp(ctx, env, HandleExp(e, handlers))
          (* e: t, handlers: exn -> t *)
    = raise Fail "handle expression not implemented yet"
  | constraintsExp(ctx, env, RaiseExp(exp))
    = let val expTy = constraintsExp(ctx, env, exp)
      in addConstraint(ctx, EqConstr(expTy, primTy_exn)) (* expTy = exn *)
       ; TyVar(freshTyVar(ctx))
      end
  | constraintsExp(ctx, env, IfThenElseExp(cond, thenPart, elsePart))
    = let val condTy = constraintsExp(ctx, env, cond)
          val thenTy = constraintsExp(ctx, env, thenPart)
          val elseTy = constraintsExp(ctx, env, elsePart)
      in addConstraint(ctx, EqConstr(condTy, primTy_bool)) (* condTy = bool *)
       ; addConstraint(ctx, EqConstr(thenTy, elseTy)) (* thenTy = elseTy *)
       ; thenTy
      end
  | constraintsExp(ctx, env, CaseExp(exp, matches))
    = let val expTy = constraintsExp(ctx, env, exp)
          val (patTy, retTy) = constraintsFromMatch(ctx, env, matches)
      in addConstraint(ctx, EqConstr(expTy, patTy))
       ; retTy
      end
  | constraintsExp(ctx, env, FnExp(matches))
    = let val (argTy, retTy) = constraintsFromMatch(ctx, env, matches)
      in USyntax.FnType(argTy, retTy)
      end
(* constraintsDecl : Context * Env * Dec list -> Env *)
and constraintsDecl(ctx, env, nil) : Env = env
  | constraintsDecl(ctx, env, decl :: decls)
    = (case decl of
           ValDec(tyvarseq, valbinds) =>
           let val MkEnv { valMap = valMap, tyMap = tyMap, strMap = strMap } = env
               val vars = constraintsValBinds(ctx, env, Syntax.VIdMap.empty, valbinds)
               val () = (unify(ctx, !(#constraints ctx)) ; #constraints ctx := [])
               val subst = !(#tyVarSubst ctx);
               val env' = applySubstEnv subst env
               val tyVars_env = freeTyVarsInEnv(TyVarSet.empty, env)
               fun doVar(ty, false) = (TypeScheme([], ty), Syntax.ValueVariable)
                 | doVar(ty, true) = let val ty' = applySubstTy subst ty
                                         val tyVars_ty = freeTyVarsInTy(TyVarSet.empty, ty')
                                         val tyVars = TyVarSet.difference(tyVars_ty, tyVars_env)
                                     in (TypeScheme(TyVarSet.listItems tyVars, ty'), Syntax.ValueVariable)
                                     end
               val valMap' = Syntax.VIdMap.map doVar vars
               val env' = MkEnv { valMap = Syntax.VIdMap.unionWith #2 (valMap, valMap')
                                , tyMap = tyMap
                                , strMap = strMap
                                }
           in constraintsDecl(ctx, env', decls)
           end
         | RecValDec(tyvarseq, valbinds) => raise Fail "let-in: val rec: not impl"
         | TypeDec(_) => raise Fail "let-in: type: not impl"
         | DatatypeDec(_) => raise Fail "let-in: datatype: not impl"
         | DatatypeRepDec(_) => raise Fail "let-in: datatype rep: not impl"
         | AbstypeDec(_) => raise Fail "let-in: abstype: not impl"
         | ExceptionDec(_) => raise Fail "let-in: exception: not impl"
         | LocalDec(localDecls, decls') => let val env' = constraintsDecl(ctx, env, localDecls)
                                               val env'' = constraintsDecl(ctx, mergeEnv(env, env'), decls')
                                           in constraintsDecl(ctx, mergeEnv(env, env''), decls)
                                           end
         | OpenDec(_) => raise Fail "let-in: open: not impl"
      )
(* constraintsValBinds : Context * Env * (USyntax.Ty * bool) Syntax.VIdMap.map * ValBind list -> (USyntax.Ty * bool) Syntax.VIdMap.map *)
and constraintsValBinds(ctx, env, vars, []): (USyntax.Ty * bool) Syntax.VIdMap.map = vars
  | constraintsValBinds(ctx, env, vars, PatBind(pat, exp) :: rest)
    = let val (patTy, newVars) = constraintsFromPat(ctx, env, pat)
          val expTy = constraintsExp(ctx, env, exp)
          val generalize = isExhaustive(env, pat) andalso isNonexpansive(env, exp)
          val vars' = Syntax.VIdMap.unionWith #2 (vars, Syntax.VIdMap.map (fn ty => (ty, generalize)) newVars) (* TODO: generalize *)
      in addConstraint(ctx, EqConstr(patTy, expTy))
       ; constraintsValBinds(ctx, env, vars', rest)
      end
(* constraintsFromRow : Context * Env * (Label * Exp) list -> (Label * Syntax.Ty) list *)
and constraintsFromRow(ctx, env, xs) : (Syntax.Label * USyntax.Ty) list
    = let fun oneField(label, exp) = (label, constraintsExp(ctx, env, exp))
      in List.map oneField xs
      end
 (* constraintsFromMatch : Context * Env * (Pat * Exp) list -> (* pattern *) Syntax.Ty * (* expression *) Syntax.Ty *)
and constraintsFromMatch(ctx, env, (pat0, exp0) :: rest) : USyntax.Ty * USyntax.Ty
    = let val (patTy, expTy) = constraintsFromMatchBranch(ctx, env, pat0, exp0)
          fun oneBranch(pat, exp)
              = let val (patTy', expTy') = constraintsFromMatchBranch(ctx, env, pat, exp)
                in addConstraint(ctx, EqConstr(patTy, patTy'))
                 ; addConstraint(ctx, EqConstr(expTy, expTy'))
                end
      in List.app oneBranch rest
       ; (patTy, expTy)
      end
  | constraintsFromMatch(ctx, env, nil) = raise TypeError "invalid syntax tree: match is empty"
and constraintsFromMatchBranch(ctx : Context, env as MkEnv env' : Env, pat, exp) : USyntax.Ty * USyntax.Ty
    = let val (patTy, vars) = constraintsFromPat(ctx, env, pat)
          val env'' = MkEnv { tyMap = #tyMap env'
                            , valMap = Syntax.VIdMap.unionWith #2 (#valMap env', Syntax.VIdMap.map (fn ty => (TypeScheme([], ty), Syntax.ValueVariable)) vars)
                            , strMap = #strMap env'
                            }
          val expTy = constraintsExp(ctx, env'', exp)
      in (patTy, expTy)
      end
 (* constraintsFromPat : Context * Env * Pat -> USyntax.Ty * USyntax.Ty Syntax.VIdMap.map *)
and constraintsFromPat(ctx, env, WildcardPat) : USyntax.Ty * USyntax.Ty Syntax.VIdMap.map
    = let val ty = TyVar(freshTyVar(ctx))
      in (ty, Syntax.VIdMap.empty)
      end
  | constraintsFromPat(ctx, env, SConPat scon)
    = (case scon of
           Syntax.IntegerConstant(_)   => (primTy_int, Syntax.VIdMap.empty)
         | Syntax.WordConstant(_)      => (primTy_word, Syntax.VIdMap.empty)
         | Syntax.RealConstant(_)      => raise Syntax.SyntaxError "No real constant may occur in a pattern"
         | Syntax.StringConstant(_)    => (primTy_string, Syntax.VIdMap.empty)
         | Syntax.CharacterConstant(_) => (primTy_char, Syntax.VIdMap.empty)
      )
  | constraintsFromPat(ctx, MkEnv env, VarPat(vid, ty))
    = (case Syntax.VIdMap.find(#valMap env, vid) of
           SOME (tysc, Syntax.ValueConstructor) => raise TypeError "VarPat: invalid pattern"
         | SOME (tysc, Syntax.ExceptionConstructor) => raise TypeError "VarPat: invalid pattern"
         | SOME (_, Syntax.ValueVariable) => (* shadowing *) (ty, Syntax.VIdMap.insert(Syntax.VIdMap.empty, vid, ty))
         | NONE => (ty, Syntax.VIdMap.insert(Syntax.VIdMap.empty, vid, ty))
      )
  | constraintsFromPat(ctx, env, NulConPat(Syntax.MkLongVId(strid, vid)))
    = let val MkEnv strEnv = lookupStr(env, strid)
      in case Syntax.VIdMap.find(#valMap strEnv, vid) of
             SOME (tysc, idstatus) =>
             (if idstatus = Syntax.ValueConstructor orelse idstatus = Syntax.ExceptionConstructor then
                  let val ty = instantiate(ctx, tysc)
                  in (ty, Syntax.VIdMap.empty)
                  end
              else (* idstatus = Syntax.ValueVariable *)
                  raise TypeError "invalid pattern"
             )
           | NONE => raise TypeError "invalid pattern"
      end
  | constraintsFromPat(ctx, env, RecordPat(row, wildcard))
    = let val (row', vars) = constraintsFromPatRow(ctx, env, row)
      in if wildcard then
             let val recordTy = TyVar(freshTyVar(ctx))
                 fun oneField(label, ty) = addConstraint(ctx, FieldConstr { label = label, recordTy = recordTy, fieldTy = ty })
             in List.app oneField row'
              ; (recordTy, vars)
             end
         else
             (RecordType(row'), vars)
      end
  | constraintsFromPat(ctx, env, ConPat(longvid as Syntax.MkLongVId(strid, vid), innerPat))
    = let val MkEnv strEnv = lookupStr(env, strid)
      in case Syntax.VIdMap.find(#valMap strEnv, vid) of
             SOME (tysc, idstatus) =>
             if idstatus = Syntax.ValueConstructor orelse idstatus = Syntax.ExceptionConstructor then
                 case instantiate(ctx, tysc) of
                     USyntax.FnType(argTy, resultTy) => let val (argTy', innerVars) = constraintsFromPat(ctx, env, innerPat)
                                                        in addConstraint(ctx, EqConstr(argTy, argTy'))
                                                         ; (resultTy, innerVars)
                                                        end
                   | _ => raise TypeError "invalid pattern"
             else (* idstatus = Syntax.ValueVariable *)
                 raise TypeError "invalid pattern"
           | NONE => raise TypeError "invalid pattern"
      end
  | constraintsFromPat(ctx, env, TypedPat(WildcardPat, ty))
    = (ty, Syntax.VIdMap.empty)
  | constraintsFromPat(ctx, env, TypedPat(pat, ty))
    = let val (inferredTy, vars) = constraintsFromPat(ctx, env, pat)
      in addConstraint(ctx, EqConstr(ty, inferredTy))
       ; (ty, vars)
      end
  | constraintsFromPat(ctx, env, LayeredPat(vid, ty, pat))
    = let val (inferredTy, vars) = constraintsFromPat(ctx, env, pat)
      in case Syntax.VIdMap.find(vars, vid) of
             NONE => ( addConstraint(ctx, EqConstr(ty, inferredTy))
                     ; (ty, Syntax.VIdMap.insert(vars, vid, ty)))
           | SOME _ => raise TypeError "trying to bind the same identifier twice"
      end
 (* constraintsFromPatRow : Context * Env * (Label * Pat) list -> (Label * Syntax.Ty) list * Syntax.Ty Syntax.VIdMap.map *)
and constraintsFromPatRow(ctx, env, row)
    = let fun oneField((label, pat), (row, vars))
              = let val (ty, vars') = constraintsFromPat(ctx, env, pat)
                in ((label, ty) :: row, Syntax.VIdMap.unionWith (fn _ => raise TypeError "trying to bind the same identifier twice") (vars, vars'))
                end
      in List.foldl oneField ([], Syntax.VIdMap.empty) row
      end

(* typeCheckExp : Context * Env * USyntax.Exp -> Subst * (TyVar * TyVarConstraint) list * USyntax.Ty * USyntax.Exp *)
fun typeCheckExp(ctx, env, exp) = let val ty = constraintsExp(ctx, env, exp)
                                      val constraints = !(#constraints ctx)
                                      val () = unify(ctx, constraints)
                                      val subst = !(#tyVarSubst ctx)
                                      val tvc = !(#tyVarConstraints ctx)
                                      val applySubst = applySubstTy subst
                                  in (subst, tvc, applySubst ty, USyntax.mapTyInExp applySubst exp)
                                  end
end (* local *)
end (* structure Typing *)
