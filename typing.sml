structure Typing = struct
datatype UnaryConstraint
  = HasField of { label : Syntax.Label
                , fieldTy : USyntax.Ty
                }
  | IsEqType
  | IsIntegral (* Int, Word; div, mod; defaults to int *)
  | IsSignedReal (* Int, Real; abs; defaults to int *)
  | IsRing (* Int, Word, Real; *, +, -; defaults to int *)
  | IsField (* Real; /; defaults to real *)
  | IsSigned (* Int, Real; ~; defaults to int *)
  | IsOrdered (* NumTxt; <, >, <=, >=; defaults to int *)

datatype Constraint
  = EqConstr of USyntax.Ty * USyntax.Ty (* ty1 = ty2 *)
  | UnaryConstraint of USyntax.Ty * UnaryConstraint

datatype TypeFcn = TypeFcn of USyntax.TyVar list * USyntax.Ty
datatype TypeScheme = TypeScheme of (USyntax.TyVar * UnaryConstraint list) list * USyntax.Ty
type ValEnv = (TypeScheme * Syntax.IdStatus) Syntax.VIdMap.map
val emptyValEnv = Syntax.VIdMap.empty
datatype TyStr = TyStr of TypeFcn * ValEnv

datatype Env = MkEnv of { tyMap : TyStr Syntax.TyConMap.map
                        , valMap : (TypeScheme * Syntax.IdStatus) Syntax.VIdMap.map
                        , strMap : Env Syntax.StrIdMap.map
                        }

type Subst = USyntax.Ty USyntax.TyVarMap.map

fun freeTyVarsInTypeScheme(bound, TypeScheme(tyvars, ty)) = USyntax.freeTyVarsInTy(USyntax.TyVarSet.addList(bound, List.map #1 tyvars), ty)
fun freeTyVarsInEnv(bound, MkEnv { tyMap = tyMap, valMap = valMap, strMap = strMap })
    = let val valMapSet = Syntax.VIdMap.foldl (fn ((tysc, _), set) => USyntax.TyVarSet.union(set, freeTyVarsInTypeScheme(bound, tysc))) USyntax.TyVarSet.empty valMap
          (* TODO: tyMap? *)
      in Syntax.StrIdMap.foldl (fn (env, set) => USyntax.TyVarSet.union(set, freeTyVarsInEnv(bound, env))) valMapSet strMap
      end
fun freeTyVarsInConstraint(bound, EqConstr(ty1, ty2)) = USyntax.TyVarSet.union(USyntax.freeTyVarsInTy(bound, ty1), USyntax.freeTyVarsInTy(bound, ty2))
  | freeTyVarsInConstraint(bound, UnaryConstraint(recordTy, HasField{fieldTy = fieldTy, ...})) = USyntax.TyVarSet.union(USyntax.freeTyVarsInTy(bound, recordTy), USyntax.freeTyVarsInTy(bound, fieldTy))
  | freeTyVarsInConstraint(bound, UnaryConstraint(ty, IsEqType)) = USyntax.freeTyVarsInTy(bound, ty)
  | freeTyVarsInConstraint(bound, UnaryConstraint(ty, IsIntegral)) = USyntax.freeTyVarsInTy(bound, ty)
  | freeTyVarsInConstraint(bound, UnaryConstraint(ty, IsSignedReal)) = USyntax.freeTyVarsInTy(bound, ty)
  | freeTyVarsInConstraint(bound, UnaryConstraint(ty, IsRing)) = USyntax.freeTyVarsInTy(bound, ty)
  | freeTyVarsInConstraint(bound, UnaryConstraint(ty, IsField)) = USyntax.freeTyVarsInTy(bound, ty)
  | freeTyVarsInConstraint(bound, UnaryConstraint(ty, IsSigned)) = USyntax.freeTyVarsInTy(bound, ty)
  | freeTyVarsInConstraint(bound, UnaryConstraint(ty, IsOrdered)) = USyntax.freeTyVarsInTy(bound, ty)
fun freeTyVarsInUnaryConstraint(bound, HasField{fieldTy = fieldTy, ...}) = USyntax.freeTyVarsInTy(bound, fieldTy)
  | freeTyVarsInUnaryConstraint(bound, IsEqType) = USyntax.TyVarSet.empty
  | freeTyVarsInUnaryConstraint(bound, IsIntegral) = USyntax.TyVarSet.empty
  | freeTyVarsInUnaryConstraint(bound, IsSignedReal) = USyntax.TyVarSet.empty
  | freeTyVarsInUnaryConstraint(bound, IsRing) = USyntax.TyVarSet.empty
  | freeTyVarsInUnaryConstraint(bound, IsField) = USyntax.TyVarSet.empty
  | freeTyVarsInUnaryConstraint(bound, IsSigned) = USyntax.TyVarSet.empty
  | freeTyVarsInUnaryConstraint(bound, IsOrdered) = USyntax.TyVarSet.empty

type Context = { nextTyVar : int ref
               , tyVarConstraints : ((USyntax.TyVar * UnaryConstraint) list) ref (* should use (multi-)map? *)
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
fun lookupLongVIdInEnv(env, Syntax.MkLongVId(strid, vid))
    = let val MkEnv strEnv = lookupStr(env, strid)
      in Syntax.VIdMap.find(#valMap strEnv, vid)
      end

(* The Definition, 4.7 Non-expansive Expressions *)
(* isNonexpansive : Env * USyntax.Exp -> bool *)
fun isNonexpansive(env : Env, USyntax.SConExp _) = true
  | isNonexpansive(env, USyntax.VarExp _) = true (* <op> longvid *)
  | isNonexpansive(env, USyntax.RecordExp fields) = List.all (fn (_, e) => isNonexpansive(env, e)) fields
  | isNonexpansive(env, USyntax.TypedExp(e, _)) = isNonexpansive(env, e)
  | isNonexpansive(env, USyntax.AppExp(conexp, e)) = isConexp(env, conexp) andalso isNonexpansive(env, e)
  | isNonexpansive(env, USyntax.FnExp _) = true
  | isNonexpansive(env, USyntax.ProjectionExp _) = true
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

val primTyCon_int    = USyntax.MkLongTyCon(Syntax.MkLongTyCon([], Syntax.MkTyCon "int"), 0)
val primTyCon_word   = USyntax.MkLongTyCon(Syntax.MkLongTyCon([], Syntax.MkTyCon "word"), 1)
val primTyCon_real   = USyntax.MkLongTyCon(Syntax.MkLongTyCon([], Syntax.MkTyCon "real"), 2)
val primTyCon_string = USyntax.MkLongTyCon(Syntax.MkLongTyCon([], Syntax.MkTyCon "string"), 3)
val primTyCon_char   = USyntax.MkLongTyCon(Syntax.MkLongTyCon([], Syntax.MkTyCon "char"), 4)
val primTyCon_exn    = USyntax.MkLongTyCon(Syntax.MkLongTyCon([], Syntax.MkTyCon "exn"), 5)
val primTyCon_bool   = USyntax.MkLongTyCon(Syntax.MkLongTyCon([], Syntax.MkTyCon "bool"), 6)
val primTyCon_ref    = USyntax.MkLongTyCon(Syntax.MkLongTyCon([], Syntax.MkTyCon "ref"), 7)
val primTyCon_list   = USyntax.MkLongTyCon(Syntax.MkLongTyCon([], Syntax.MkTyCon "list"), 8)
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
          val tyVarA = USyntax.MkTyVar("a", 0)
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
                                                    , mkValMap [(MkVId "nil", (TypeScheme ([(tyVarA, [])], USyntax.TyCon([USyntax.TyVar(tyVarA)], primTyCon_list)), ValueConstructor))
                                                               ,(MkVId "::", (TypeScheme ([(tyVarA, [])], USyntax.FnType(USyntax.PairType(USyntax.TyVar(tyVarA), USyntax.TyCon([USyntax.TyVar(tyVarA)], primTyCon_list)), USyntax.TyCon([USyntax.TyVar(tyVarA)], primTyCon_list))), ValueConstructor))
                                                               ]))
                             ,(MkTyCon "ref", TyStr(TypeFcn([tyVarA], USyntax.TyCon([USyntax.TyVar(tyVarA)], primTyCon_ref))
                                                   , mkValMap [(MkVId "ref", (TypeScheme ([(tyVarA, [])], USyntax.FnType(USyntax.TyVar(tyVarA), USyntax.TyCon([USyntax.TyVar(tyVarA)], primTyCon_ref))), ValueConstructor))
                                                              ]))
                             ,(MkTyCon "exn", TyStr(TypeFcn([], primTy_exn), emptyValEnv))
                             ]
               , valMap = mkValMap
                              (* C Appendix: The Initial Static Basis *)
                              [(MkVId "ref", (TypeScheme ([(tyVarA, [])], USyntax.FnType(USyntax.TyVar(tyVarA), USyntax.TyCon([USyntax.TyVar(tyVarA)], primTyCon_ref))), ValueConstructor)) (* forall 'a. 'a -> 'a ref *)
                              ,(MkVId "nil", (TypeScheme ([(tyVarA, [])], USyntax.TyCon([USyntax.TyVar(tyVarA)], primTyCon_list)), ValueConstructor)) (* forall 'a. 'a list *)
                              ,(MkVId "true", (TypeScheme ([], primTy_bool), ValueConstructor))
                              ,(MkVId "false", (TypeScheme ([], primTy_bool), ValueConstructor))
                              ,(MkVId "Match", (TypeScheme ([], primTy_exn), ExceptionConstructor))
                              ,(MkVId "Bind", (TypeScheme ([], primTy_exn), ExceptionConstructor))
                              ,(MkVId "::", (TypeScheme ([(tyVarA, [])], USyntax.FnType(USyntax.PairType(USyntax.TyVar(tyVarA), USyntax.TyCon([USyntax.TyVar(tyVarA)], primTyCon_list)), USyntax.TyCon([USyntax.TyVar(tyVarA)], primTyCon_list))), ValueConstructor)) (* forall 'a. 'a * 'a list -> 'a list *)
                              ,(MkVId "=", (TypeScheme ([(tyVarA, [IsEqType])], USyntax.FnType(USyntax.PairType(USyntax.TyVar(tyVarA), USyntax.TyVar(tyVarA)), primTy_bool)), ValueVariable)) (* forall ''a. ''a * ''a -> bool *)
                              ,(MkVId ":=", (TypeScheme ([(tyVarA, [])], USyntax.FnType(USyntax.PairType(USyntax.TyCon([USyntax.TyVar(tyVarA)], primTyCon_ref), USyntax.TyVar(tyVarA)), primTy_unit)), ValueVariable)) (* forall 'a. 'a ref * 'a -> {} *)
                              (* Overloaded identifiers *)
                              ,(MkVId "abs", (TypeScheme([(tyVarA, [IsSignedReal])], USyntax.FnType(USyntax.TyVar(tyVarA), USyntax.TyVar(tyVarA))), ValueVariable)) (* realint -> realint, default: int -> int *)
                              ,(MkVId "~", (TypeScheme([(tyVarA, [IsSigned])], USyntax.FnType(USyntax.TyVar(tyVarA), USyntax.TyVar(tyVarA))), ValueVariable)) (* realint -> realint, default: int -> int *)
                              ,(MkVId "div", (TypeScheme([(tyVarA, [IsIntegral])], USyntax.FnType(USyntax.PairType(USyntax.TyVar(tyVarA), USyntax.TyVar(tyVarA)), USyntax.TyVar(tyVarA))), ValueVariable)) (* wordint * wordint -> wordint, default: int * int -> int *)
                              ,(MkVId "mod", (TypeScheme([(tyVarA, [IsIntegral])], USyntax.FnType(USyntax.PairType(USyntax.TyVar(tyVarA), USyntax.TyVar(tyVarA)), USyntax.TyVar(tyVarA))), ValueVariable)) (* wordint * wordint -> wordint, default: int * int -> int *)
                              ,(MkVId "*", (TypeScheme([(tyVarA, [IsRing])], USyntax.FnType(USyntax.PairType(USyntax.TyVar(tyVarA), USyntax.TyVar(tyVarA)), USyntax.TyVar(tyVarA))), ValueVariable)) (* num * num -> num, default: int * int -> int *)
                              ,(MkVId "/", (TypeScheme([(tyVarA, [IsField])], USyntax.FnType(USyntax.PairType(USyntax.TyVar(tyVarA), USyntax.TyVar(tyVarA)), USyntax.TyVar(tyVarA))), ValueVariable)) (* Real * Real -> Real, default: real * real -> real *)
                              ,(MkVId "+", (TypeScheme([(tyVarA, [IsRing])], USyntax.FnType(USyntax.PairType(USyntax.TyVar(tyVarA), USyntax.TyVar(tyVarA)), USyntax.TyVar(tyVarA))), ValueVariable)) (* num * num -> num, default: int * int -> int *)
                              ,(MkVId "-", (TypeScheme([(tyVarA, [IsRing])], USyntax.FnType(USyntax.PairType(USyntax.TyVar(tyVarA), USyntax.TyVar(tyVarA)), USyntax.TyVar(tyVarA))), ValueVariable)) (* num * num -> num, default: int * int -> int *)
                              ,(MkVId "<", (TypeScheme([(tyVarA, [IsOrdered])], USyntax.FnType(USyntax.PairType(USyntax.TyVar(tyVarA), USyntax.TyVar(tyVarA)), primTy_bool)), ValueVariable)) (* numtxt * numtxt -> bool, default: int * int -> bool *)
                              ,(MkVId ">", (TypeScheme([(tyVarA, [IsOrdered])], USyntax.FnType(USyntax.PairType(USyntax.TyVar(tyVarA), USyntax.TyVar(tyVarA)), primTy_bool)), ValueVariable)) (* numtxt * numtxt -> bool, default: int * int -> bool *)
                              ,(MkVId "<=", (TypeScheme([(tyVarA, [IsOrdered])], USyntax.FnType(USyntax.PairType(USyntax.TyVar(tyVarA), USyntax.TyVar(tyVarA)), primTy_bool)), ValueVariable)) (* numtxt * numtxt -> bool, default: int * int -> bool *)
                              ,(MkVId ">=", (TypeScheme([(tyVarA, [IsOrdered])], USyntax.FnType(USyntax.PairType(USyntax.TyVar(tyVarA), USyntax.TyVar(tyVarA)), primTy_bool)), ValueVariable)) (* numtxt * numtxt -> bool, default: int * int -> bool *)
                              ]
               , strMap = Syntax.StrIdMap.empty
               }
      end

fun newContext() : Context
    = { nextTyVar = ref 100
      , tyVarConstraints = ref []
      , tyVarSubst = ref USyntax.TyVarMap.empty
      }

fun addTyVarConstraint(ctx : Context, tv : USyntax.TyVar, ct : UnaryConstraint)
    = let val cts = !(#tyVarConstraints ctx)
      in #tyVarConstraints ctx := (tv, ct) :: cts
      end

fun freshTyVar(ctx : Context) : USyntax.TyVar
    = let val nextTyVar = #nextTyVar ctx
          val i = !nextTyVar
      in nextTyVar := i + 1
       ; USyntax.MkTyVar("_", i)
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
     | UnaryConstraint(recordTy, HasField{label = label, fieldTy = fieldTy}) => UnaryConstraint(substTy recordTy, HasField{label = label, fieldTy = substTy fieldTy})
     | UnaryConstraint(ty, IsEqType) => UnaryConstraint(substTy ty, IsEqType)
     | UnaryConstraint(ty, IsIntegral) => UnaryConstraint(substTy ty, IsIntegral)
     | UnaryConstraint(ty, IsSignedReal) => UnaryConstraint(substTy ty, IsSignedReal)
     | UnaryConstraint(ty, IsRing) => UnaryConstraint(substTy ty, IsRing)
     | UnaryConstraint(ty, IsField) => UnaryConstraint(substTy ty, IsField)
     | UnaryConstraint(ty, IsSigned) => UnaryConstraint(substTy ty, IsSigned)
     | UnaryConstraint(ty, IsOrdered) => UnaryConstraint(substTy ty, IsOrdered)
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
            = let val subst' = USyntax.TyVarMap.filteri (fn (tv, ty) => not (List.exists (fn (tv', _) => eqUTyVar(tv', tv)) tyvars)) subst
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
    = let val subst = List.foldl (fn ((v, preds), set) => let val tv = freshTyVar(ctx)
                                                          in List.app (fn pred => addTyVarConstraint(ctx, tv, pred)) preds
                                                           ; USyntax.TyVarMap.insert(set, v, TyVar(tv))
                                                          end) USyntax.TyVarMap.empty vars
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
         | UnaryConstraint(recordTy, HasField{label = label, fieldTy = fieldTy}) =>
           (case recordTy of
                RecordType(fields) =>
                (case List.find (fn (label', _) => label = label') fields of
                     NONE => raise TypeError("unification failed: no field")
                   | SOME(_, ty') => unify(ctx, EqConstr(fieldTy, ty') :: ctrs)
                )
              | TyCon(_, _) => raise TypeError("record field for a non-record type")
              | FnType(_, _) => raise TypeError("record field for a function type")
              | TyVar tv =>
                (case USyntax.TyVarMap.find(!(#tyVarSubst ctx), tv) of
                     SOME replacement => unify(ctx, UnaryConstraint(replacement, HasField{label = label, fieldTy = fieldTy}) :: ctrs)
                   | NONE => ( addTyVarConstraint(ctx, tv, HasField{ label = label, fieldTy = fieldTy })
                             ; unify(ctx, ctrs)
                             )
                )
           )
         | UnaryConstraint(RecordType fields, IsEqType) => unify(ctx, List.map (fn (label, ty) => UnaryConstraint(ty, IsEqType)) fields @ ctrs)
         | UnaryConstraint(RecordType _, IsIntegral) => raise TypeError("cannot apply arithmetic operator on record type")
         | UnaryConstraint(RecordType _, IsSignedReal) => raise TypeError("cannot apply arithmetic operator on record type")
         | UnaryConstraint(RecordType _, IsRing) => raise TypeError("cannot apply arithmetic operator on record type")
         | UnaryConstraint(RecordType _, IsField) => raise TypeError("cannot apply arithmetic operator on record type")
         | UnaryConstraint(RecordType _, IsSigned) => raise TypeError("cannot apply arithmetic operator on record type")
         | UnaryConstraint(RecordType _, IsOrdered) => raise TypeError("cannot compare records")
         | UnaryConstraint(FnType _, IsEqType) => raise TypeError("function type does not admit equality")
         | UnaryConstraint(FnType _, IsIntegral) => raise TypeError("cannot apply arithmetic operator on function type")
         | UnaryConstraint(FnType _, IsSignedReal) => raise TypeError("cannot apply arithmetic operator on function type")
         | UnaryConstraint(FnType _, IsRing) => raise TypeError("cannot apply arithmetic operator on function type")
         | UnaryConstraint(FnType _, IsField) => raise TypeError("cannot apply arithmetic operator on function type")
         | UnaryConstraint(FnType _, IsSigned) => raise TypeError("cannot apply arithmetic operator on function type")
         | UnaryConstraint(FnType _, IsOrdered) => raise TypeError("cannot compare functions")
         | UnaryConstraint(TyCon(tyargs, longtycon), IsEqType) =>
           if eqULongTyCon(longtycon, primTyCon_ref) then
               unify(ctx, ctrs) (* do nothing *)
           else
               (* (longtycon???) : List.map IsEqType tyargs @ ctrs *)
               raise Fail "IsEqType TyCon: not impl"
         | UnaryConstraint(TyCon(tyargs, longtycon), IsIntegral) =>
           if eqULongTyCon(longtycon, primTyCon_int) orelse eqULongTyCon(longtycon, primTyCon_word) then
               unify(ctx, ctrs) (* do nothing *)
           else
               raise TypeError("arithmetic operator on unsupported type")
         | UnaryConstraint(TyCon(tyargs, longtycon), IsSignedReal) =>
           if eqULongTyCon(longtycon, primTyCon_int) orelse eqULongTyCon(longtycon, primTyCon_real) then
               unify(ctx, ctrs) (* do nothing *)
           else
               raise TypeError("arithmetic operator on unsupported type")
         | UnaryConstraint(TyCon(tyargs, longtycon), IsRing) =>
           if eqULongTyCon(longtycon, primTyCon_int) orelse eqULongTyCon(longtycon, primTyCon_word) orelse eqULongTyCon(longtycon, primTyCon_real) then
               unify(ctx, ctrs) (* do nothing *)
           else
               raise TypeError("arithmetic operator on unsupported type")
         | UnaryConstraint(TyCon(tyargs, longtycon), IsField) =>
           if eqULongTyCon(longtycon, primTyCon_real) then
               unify(ctx, ctrs) (* do nothing *)
           else
               raise TypeError("arithmetic operator on unsupported type")
         | UnaryConstraint(TyCon(tyargs, longtycon), IsSigned) =>
           if eqULongTyCon(longtycon, primTyCon_int) orelse eqULongTyCon(longtycon, primTyCon_real) then
               unify(ctx, ctrs) (* do nothing *)
           else
               raise TypeError("arithmetic operator on unsupported type")
         | UnaryConstraint(TyCon(tyargs, longtycon), IsOrdered) =>
           if eqULongTyCon(longtycon, primTyCon_int) orelse eqULongTyCon(longtycon, primTyCon_word) orelse eqULongTyCon(longtycon, primTyCon_real) orelse eqULongTyCon(longtycon, primTyCon_string) orelse eqULongTyCon(longtycon, primTyCon_char) then
               unify(ctx, ctrs) (* do nothing *)
           else
               raise TypeError("comparison operator on unsupported type")
         | UnaryConstraint(TyVar tv, pred) => (case USyntax.TyVarMap.find(!(#tyVarSubst ctx), tv) of
                                                   SOME replacement => unify(ctx, UnaryConstraint(replacement, pred) :: ctrs)
                                                 | NONE => (addTyVarConstraint(ctx, tv, pred) ; unify(ctx, ctrs))
                                              )
      )
and unifyTyVarAndTy(ctx : Context, tv : TyVar, ty : Ty, ctrs : Constraint list) : unit
    = if (case ty of TyVar(tv') => eqUTyVar(tv, tv') | _ => false) then (* ty = TyVar tv *)
          unify(ctx, ctrs) (* do nothing *)
      else
          let val subst = !(#tyVarSubst ctx)
          in case USyntax.TyVarMap.find(subst, tv) of
                 SOME replacement => unify(ctx, EqConstr(replacement, ty) :: ctrs)
               | NONE =>
                 let val ty = applySubstTy subst ty
                 in if occurCheck tv ty then
                        raise TypeError("unification failed: occurrence check (" ^ USyntax.print_TyVar tv ^ " in " ^ USyntax.print_Ty ty ^ ")")
                    else
                        let val tvc = !(#tyVarConstraints ctx)
                            val (e, tvc') = List.partition (fn (tv', c) => eqUTyVar(tv, tv')) tvc
                            val () = #tyVarConstraints ctx := tvc'
                            fun toConstraint (_, predicate) = UnaryConstraint(ty, predicate)
                            val subst' = USyntax.TyVarMap.map (substituteTy (tv, ty)) subst
                        in #tyVarSubst ctx := USyntax.TyVarMap.insert(subst', tv, ty)
                         ; unify(ctx, List.map toConstraint e @ List.map (substituteConstraint (tv, ty)) ctrs)
                        end
                 end
          end
fun addConstraint(ctx : Context, ct : Constraint) = unify(ctx, [ct])

(* typeCheckExp : Context * Env * USyntax.Exp -> USyntax.Ty *)
fun typeCheckExp(ctx : Context, env : Env, SConExp(scon)) : USyntax.Ty
    = (case scon of (* TODO: overloaded literals *)
           Syntax.IntegerConstant x   => primTy_int
         | Syntax.WordConstant x      => primTy_word
         | Syntax.RealConstant x      => primTy_real
         | Syntax.StringConstant x    => primTy_string
         | Syntax.CharacterConstant x => primTy_char
      )
  | typeCheckExp(ctx, env, VarExp(longvid as Syntax.MkLongVId(_, Syntax.MkVId name), idstatus))
    = (case lookupLongVIdInEnv(env, longvid) of
           SOME (tysc, ids) => instantiate(ctx, tysc)
         | NONE => raise NameError("unknown value name " ^ name)
      )
  | typeCheckExp(ctx, env, RecordExp(row))
    = let val row' = typeCheckExpRow(ctx, env, row)
      in RecordType(row')
      end
  | typeCheckExp(ctx, env, LetInExp(decls, innerExp))
    = let val env' = typeCheckDecl(ctx, env, decls)
      in typeCheckExp(ctx, mergeEnv(env, env'), innerExp)
      end
  | typeCheckExp(ctx, env, AppExp(f, x))
          (* f: s -> t, x: s *)
    = let val funcTy = typeCheckExp(ctx, env, f)
          val argTy = typeCheckExp(ctx, env, x)
          val retTy = TyVar(freshTyVar(ctx))
      in addConstraint(ctx, EqConstr(funcTy, FnType(argTy, retTy))) (* funcTy = (argTy -> retTy) *)
       ; retTy
      end
  | typeCheckExp(ctx, env, TypedExp(exp, ty))
    = let val expTy = typeCheckExp(ctx, env, exp)
      in addConstraint(ctx, EqConstr(expTy, ty)) (* ety = ty *)
       ; ty
      end
  | typeCheckExp(ctx, env, HandleExp(e, handlers))
          (* e: t, handlers: exn -> t *)
    = raise Fail "handle expression not implemented yet"
  | typeCheckExp(ctx, env, RaiseExp(exp))
    = let val expTy = typeCheckExp(ctx, env, exp)
      in addConstraint(ctx, EqConstr(expTy, primTy_exn)) (* expTy = exn *)
       ; TyVar(freshTyVar(ctx))
      end
  | typeCheckExp(ctx, env, IfThenElseExp(cond, thenPart, elsePart))
    = let val condTy = typeCheckExp(ctx, env, cond)
          val thenTy = typeCheckExp(ctx, env, thenPart)
          val elseTy = typeCheckExp(ctx, env, elsePart)
      in addConstraint(ctx, EqConstr(condTy, primTy_bool)) (* condTy = bool *)
       ; addConstraint(ctx, EqConstr(thenTy, elseTy)) (* thenTy = elseTy *)
       ; thenTy
      end
  | typeCheckExp(ctx, env, CaseExp(exp, matches))
    = let val expTy = typeCheckExp(ctx, env, exp)
          val (patTy, retTy) = typeCheckMatch(ctx, env, matches)
      in addConstraint(ctx, EqConstr(expTy, patTy))
       ; retTy
      end
  | typeCheckExp(ctx, env, FnExp(matches))
    = let val (argTy, retTy) = typeCheckMatch(ctx, env, matches)
      in USyntax.FnType(argTy, retTy)
      end
  | typeCheckExp(ctx, env, ProjectionExp { label = label, recordTy = recordTy, fieldTy = fieldTy })
    = ( addConstraint(ctx, UnaryConstraint(recordTy, HasField { label = label, fieldTy = fieldTy }))
      ; USyntax.FnType(recordTy, fieldTy)
      )
(* typeCheckDecl : Context * Env * Dec list -> Env *)
and typeCheckDecl(ctx, env, nil) : Env = env
  | typeCheckDecl(ctx, env, decl :: decls)
    = (case decl of
           ValDec(tyvarseq, valbinds) =>
           let val MkEnv { valMap = valMap, tyMap = tyMap, strMap = strMap } = env
               val vars = typeCheckValBinds(ctx, env, Syntax.VIdMap.empty, valbinds)
               val tvc = !(#tyVarConstraints ctx)
               val subst = !(#tyVarSubst ctx)
               val env' = applySubstEnv subst env
               val tyVars_env = freeTyVarsInEnv(TyVarSet.empty, env)
               fun doVar(ty, false) = (TypeScheme([], ty), Syntax.ValueVariable)
                 | doVar(ty, true) = let val ty' = applySubstTy subst ty
                                         val tyVars_ty = freeTyVarsInTy(TyVarSet.empty, ty')
                                         val unconstrainedTyVars = TyVarSet.filter (fn tv => not (List.exists (fn (tv', _) => eqUTyVar(tv, tv')) tvc)) tyVars_ty (* TODO: Allow equality constraint *)
                                         val tyVars = TyVarSet.difference(unconstrainedTyVars, tyVars_env)
                                     in (TypeScheme(List.map (fn x => (x, [])) (TyVarSet.listItems tyVars), ty'), Syntax.ValueVariable)
                                     end
               val valMap' = Syntax.VIdMap.map doVar vars
               val env' = MkEnv { valMap = Syntax.VIdMap.unionWith #2 (valMap, valMap')
                                , tyMap = tyMap
                                , strMap = strMap
                                }
           in typeCheckDecl(ctx, env', decls)
           end
         | RecValDec(tyvarseq, valbinds) => raise Fail "let-in: val rec: not impl"
         | TypeDec(_) => raise Fail "let-in: type: not impl"
         | DatatypeDec(_) => raise Fail "let-in: datatype: not impl"
         | DatatypeRepDec(_) => raise Fail "let-in: datatype rep: not impl"
         | AbstypeDec(_) => raise Fail "let-in: abstype: not impl"
         | ExceptionDec(_) => raise Fail "let-in: exception: not impl"
         | LocalDec(localDecls, decls') => let val env' = typeCheckDecl(ctx, env, localDecls)
                                               val env'' = typeCheckDecl(ctx, mergeEnv(env, env'), decls')
                                           in typeCheckDecl(ctx, mergeEnv(env, env''), decls)
                                           end
         | OpenDec(_) => raise Fail "let-in: open: not impl"
      )
(* typeCheckValBinds : Context * Env * (USyntax.Ty * bool) Syntax.VIdMap.map * ValBind list -> (USyntax.Ty * bool) Syntax.VIdMap.map *)
and typeCheckValBinds(ctx, env, vars, []): (USyntax.Ty * bool) Syntax.VIdMap.map = vars
  | typeCheckValBinds(ctx, env, vars, PatBind(pat, exp) :: rest)
    = let val (patTy, newVars) = typeCheckPat(ctx, env, pat)
          val expTy = typeCheckExp(ctx, env, exp)
          val generalize = isExhaustive(env, pat) andalso isNonexpansive(env, exp)
          val vars' = Syntax.VIdMap.unionWith #2 (vars, Syntax.VIdMap.map (fn ty => (ty, generalize)) newVars) (* TODO: generalize *)
      in addConstraint(ctx, EqConstr(patTy, expTy))
       ; typeCheckValBinds(ctx, env, vars', rest)
      end
(* typeCheckExpRow : Context * Env * (Label * Exp) list -> (Label * Syntax.Ty) list *)
and typeCheckExpRow(ctx, env, xs) : (Syntax.Label * USyntax.Ty) list
    = let fun oneField(label, exp) = (label, typeCheckExp(ctx, env, exp))
      in List.map oneField xs
      end
 (* typeCheckMatch : Context * Env * (Pat * Exp) list -> (* pattern *) Syntax.Ty * (* expression *) Syntax.Ty *)
and typeCheckMatch(ctx, env, (pat0, exp0) :: rest) : USyntax.Ty * USyntax.Ty
    = let val (patTy, expTy) = typeCheckMatchBranch(ctx, env, pat0, exp0)
          fun oneBranch(pat, exp)
              = let val (patTy', expTy') = typeCheckMatchBranch(ctx, env, pat, exp)
                in addConstraint(ctx, EqConstr(patTy, patTy'))
                 ; addConstraint(ctx, EqConstr(expTy, expTy'))
                end
      in List.app oneBranch rest
       ; (patTy, expTy)
      end
  | typeCheckMatch(ctx, env, nil) = raise TypeError "invalid syntax tree: match is empty"
and typeCheckMatchBranch(ctx : Context, env as MkEnv env' : Env, pat, exp) : USyntax.Ty * USyntax.Ty
    = let val (patTy, vars) = typeCheckPat(ctx, env, pat)
          val env'' = MkEnv { tyMap = #tyMap env'
                            , valMap = Syntax.VIdMap.unionWith #2 (#valMap env', Syntax.VIdMap.map (fn ty => (TypeScheme([], ty), Syntax.ValueVariable)) vars)
                            , strMap = #strMap env'
                            }
          val expTy = typeCheckExp(ctx, env'', exp)
      in (patTy, expTy)
      end
 (* typeCheckPat : Context * Env * Pat -> USyntax.Ty * USyntax.Ty Syntax.VIdMap.map *)
and typeCheckPat(ctx, env, WildcardPat) : USyntax.Ty * USyntax.Ty Syntax.VIdMap.map
    = let val ty = TyVar(freshTyVar(ctx))
      in (ty, Syntax.VIdMap.empty)
      end
  | typeCheckPat(ctx, env, SConPat scon)
    = (case scon of
           Syntax.IntegerConstant(_)   => (primTy_int, Syntax.VIdMap.empty)
         | Syntax.WordConstant(_)      => (primTy_word, Syntax.VIdMap.empty)
         | Syntax.RealConstant(_)      => raise Syntax.SyntaxError "No real constant may occur in a pattern"
         | Syntax.StringConstant(_)    => (primTy_string, Syntax.VIdMap.empty)
         | Syntax.CharacterConstant(_) => (primTy_char, Syntax.VIdMap.empty)
      )
  | typeCheckPat(ctx, MkEnv env, VarPat(vid, ty))
    = (case Syntax.VIdMap.find(#valMap env, vid) of
           SOME (tysc, Syntax.ValueConstructor) => raise TypeError "VarPat: invalid pattern"
         | SOME (tysc, Syntax.ExceptionConstructor) => raise TypeError "VarPat: invalid pattern"
         | SOME (_, Syntax.ValueVariable) => (* shadowing *) (ty, Syntax.VIdMap.insert(Syntax.VIdMap.empty, vid, ty))
         | NONE => (ty, Syntax.VIdMap.insert(Syntax.VIdMap.empty, vid, ty))
      )
  | typeCheckPat(ctx, env, NulConPat(longvid))
    = (case lookupLongVIdInEnv(env, longvid) of
           SOME (tysc, idstatus) =>
           (if idstatus = Syntax.ValueConstructor orelse idstatus = Syntax.ExceptionConstructor then
                let val ty = instantiate(ctx, tysc)
                in (ty, Syntax.VIdMap.empty)
                end
            else (* idstatus = Syntax.ValueVariable *)
                raise TypeError "invalid pattern"
           )
         | NONE => raise TypeError "invalid pattern"
      )
  | typeCheckPat(ctx, env, RecordPat(row, wildcard))
    = let val (row', vars) = typeCheckPatRow(ctx, env, row)
      in if wildcard then
             let val recordTy = TyVar(freshTyVar(ctx))
                 fun oneField(label, ty) = addConstraint(ctx, UnaryConstraint(recordTy, HasField { label = label, fieldTy = ty }))
             in List.app oneField row'
              ; (recordTy, vars)
             end
         else
             (RecordType(row'), vars)
      end
  | typeCheckPat(ctx, env, ConPat(longvid, innerPat))
    = (case lookupLongVIdInEnv(env, longvid) of
           SOME (tysc, idstatus) =>
           if idstatus = Syntax.ValueConstructor orelse idstatus = Syntax.ExceptionConstructor then
               case instantiate(ctx, tysc) of
                   USyntax.FnType(argTy, resultTy) => let val (argTy', innerVars) = typeCheckPat(ctx, env, innerPat)
                                                      in addConstraint(ctx, EqConstr(argTy, argTy'))
                                                       ; (resultTy, innerVars)
                                                      end
                 | _ => raise TypeError "invalid pattern"
           else (* idstatus = Syntax.ValueVariable *)
               raise TypeError "invalid pattern"
         | NONE => raise TypeError "invalid pattern"
      )
  | typeCheckPat(ctx, env, TypedPat(WildcardPat, ty))
    = (ty, Syntax.VIdMap.empty)
  | typeCheckPat(ctx, env, TypedPat(pat, ty))
    = let val (inferredTy, vars) = typeCheckPat(ctx, env, pat)
      in addConstraint(ctx, EqConstr(ty, inferredTy))
       ; (ty, vars)
      end
  | typeCheckPat(ctx, env, LayeredPat(vid, ty, pat))
    = let val (inferredTy, vars) = typeCheckPat(ctx, env, pat)
      in case Syntax.VIdMap.find(vars, vid) of
             NONE => ( addConstraint(ctx, EqConstr(ty, inferredTy))
                     ; (ty, Syntax.VIdMap.insert(vars, vid, ty)))
           | SOME _ => raise TypeError "trying to bind the same identifier twice"
      end
 (* typeCheckPatRow : Context * Env * (Label * Pat) list -> (Label * Syntax.Ty) list * Syntax.Ty Syntax.VIdMap.map *)
and typeCheckPatRow(ctx, env, row)
    = let fun oneField((label, pat), (row, vars))
              = let val (ty, vars') = typeCheckPat(ctx, env, pat)
                in ((label, ty) :: row, Syntax.VIdMap.unionWith (fn _ => raise TypeError "trying to bind the same identifier twice") (vars, vars'))
                end
      in List.foldl oneField ([], Syntax.VIdMap.empty) row
      end

(* typeCheckExp : Context * Env * USyntax.Exp -> Subst * (TyVar * UnaryConstraint) list * USyntax.Ty * USyntax.Exp *)
fun typeCheckExp_(ctx, env, exp) = let val ty = typeCheckExp(ctx, env, exp)
                                       val subst = !(#tyVarSubst ctx)
                                       val tvc = !(#tyVarConstraints ctx)
                                       val applySubst = applySubstTy subst
                                   in (subst, tvc, applySubst ty, USyntax.mapTyInExp applySubst exp)
                                   end

(* typeCheckProgram : Context * Env * USyntax.Dec list -> Subst * (TyVar * UnaryConstraint) list * USyntax.Dec list *)
fun typeCheckProgram(ctx, env, decls) = let val env' = typeCheckDecl(ctx, env, decls)
                                            val subst = !(#tyVarSubst ctx)
                                            val tvc = !(#tyVarConstraints ctx)
                                            val applySubst = applySubstTy subst
                                        in (subst, tvc, List.map (USyntax.mapTyInDec applySubst) decls)
                                        end
end (* local *)
end (* structure Typing *)
