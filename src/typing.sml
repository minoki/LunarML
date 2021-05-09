(*
 * Copyright (c) 2021 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure Typing = struct

datatype TyStr = TyStr of USyntax.TypeFcn * USyntax.ValEnv

datatype Env' = MkEnv of Env
withtype Env = { tyMap : TyStr USyntax.TyConMap.map
               , valMap : (USyntax.TypeScheme * Syntax.IdStatus) USyntax.VIdMap.map
               , strMap : Env' Syntax.StrIdMap.map
               , boundTyVars : USyntax.TyVarSet.set (* type variables bound by outer declarations *)
               }

type Subst = USyntax.Ty USyntax.TyVarMap.map

fun freeTyVarsInTypeScheme(bound, USyntax.TypeScheme(tyvars, ty)) = USyntax.freeTyVarsInTy(USyntax.TyVarSet.addList(bound, List.map #1 tyvars), ty)
fun freeTyVarsInEnv(bound, { tyMap, valMap, strMap, boundTyVars })
    = let val valMapSet = USyntax.VIdMap.foldl (fn ((tysc, _), set) => USyntax.TyVarSet.union(set, freeTyVarsInTypeScheme(bound, tysc))) boundTyVars valMap
          (* TODO: tyMap? *)
      in Syntax.StrIdMap.foldl (fn (MkEnv env, set) => USyntax.TyVarSet.union(set, freeTyVarsInEnv(bound, env))) valMapSet strMap
      end
fun freeTyVarsInConstraint(bound, USyntax.EqConstr(ty1, ty2)) = USyntax.TyVarSet.union(USyntax.freeTyVarsInTy(bound, ty1), USyntax.freeTyVarsInTy(bound, ty2))
  | freeTyVarsInConstraint(bound, USyntax.UnaryConstraint(ty, unaryConstraint))
    = (case unaryConstraint of
           USyntax.HasField{fieldTy = fieldTy, ...} => USyntax.TyVarSet.union(USyntax.freeTyVarsInTy(bound, ty), USyntax.freeTyVarsInTy(bound, fieldTy))
         | USyntax.IsEqType     => USyntax.freeTyVarsInTy(bound, ty)
         | USyntax.IsIntegral   => USyntax.freeTyVarsInTy(bound, ty)
         | USyntax.IsSignedReal => USyntax.freeTyVarsInTy(bound, ty)
         | USyntax.IsRing       => USyntax.freeTyVarsInTy(bound, ty)
         | USyntax.IsField      => USyntax.freeTyVarsInTy(bound, ty)
         | USyntax.IsSigned     => USyntax.freeTyVarsInTy(bound, ty)
         | USyntax.IsOrdered    => USyntax.freeTyVarsInTy(bound, ty)
      )

type Context = { nextTyVar : int ref
               , nextVId : int ref
               , nextTyCon : int ref
               , tyVarConstraints : ((USyntax.UnaryConstraint list) USyntax.TyVarMap.map) ref
               , tyVarSubst : Subst ref
               }

exception TypeError of SourcePos.span list * string

fun emitError(ctx : Context, spans, message) = raise TypeError (spans, message)

(* lookupStr : Context * Env * SourcePos.span * Syntax.StrId list -> Env *)
fun lookupStr(ctx, env, span, nil) = env
  | lookupStr(ctx, env as { strMap = strMap, ... }, span, (str0 as Syntax.MkStrId name) :: str1)
    = (case Syntax.StrIdMap.find(strMap, str0) of
           NONE => emitError(ctx, [span], "unknown structure name '" ^ name ^ "'")
         | SOME (MkEnv innerEnv) => lookupStr(ctx, innerEnv, span, str1)
      )
fun lookupTyConInEnv(ctx, env, span, tycon as USyntax.MkTyCon(name, _))
    = (case USyntax.TyConMap.find(#tyMap env, tycon) of
           NONE => emitError(ctx, [span], "unknown type constructor '" ^ name ^ "'")
         | SOME x => x
      )
(* Context * Env * SourcePos.span * USyntax.LongVId -> (USyntax.TypeScheme * Syntax.IdStatus) option *)
fun lookupLongVIdInEnv(ctx, env, span, Syntax.MkQualified(strid, vid))
    = let val strEnv = lookupStr(ctx, env, span, strid)
      in USyntax.VIdMap.find(#valMap strEnv, vid)
      end

(* getConstructedType : Context * SourcePos.span * USyntax.Ty -> USyntax.LongTyCon *)
fun getConstructedType(ctx, span, USyntax.TyVar _) = emitError(ctx, [span], "getConstructedType: got a type variable")
  | getConstructedType(ctx, span, USyntax.RecordType _) = emitError(ctx, [span], "getConstructedType: got a record")
  | getConstructedType(ctx, span, USyntax.TyCon(_, tyargs, longtycon)) = longtycon
  | getConstructedType(ctx, span, USyntax.FnType(_, _, t)) = getConstructedType(ctx, span, t)

(* isSoleConstructor : Context * Env * SourcePos.span * USyntax.LongVId -> bool *)
fun isSoleConstructor(ctx : Context, env : Env, span : SourcePos.span, longvid: USyntax.LongVId) =
    (case lookupLongVIdInEnv(ctx, env, span, longvid) of
         NONE => false (* probably an error *)
       | SOME (USyntax.TypeScheme(_, ty), Syntax.ValueConstructor) =>
         let val Syntax.MkQualified(strids, tycon) = getConstructedType(ctx, span, ty)
             val TyStr (_, valenv) = lookupTyConInEnv(ctx, lookupStr(ctx, env, span, strids), span, tycon)
         in USyntax.VIdMap.numItems valenv = 1
         end
       | SOME (_, Syntax.ValueVariable) => false
       | SOME (_, Syntax.ExceptionConstructor) => false
    )

(* The Definition, 4.7 Non-expansive Expressions *)
(* isNonexpansive : Env * USyntax.Exp -> bool *)
fun isNonexpansive(env : Env, USyntax.SConExp _) = true
  | isNonexpansive(env, USyntax.VarExp _) = true (* <op> longvid *)
  | isNonexpansive(env, USyntax.InstantiatedVarExp _) = true (* <op> longvid *)
  | isNonexpansive(env, USyntax.RecordExp(_, fields)) = List.all (fn (_, e) => isNonexpansive(env, e)) fields
  | isNonexpansive(env, USyntax.TypedExp(_, e, _)) = isNonexpansive(env, e)
  | isNonexpansive(env, USyntax.AppExp(_, conexp, e)) = isConexp(env, conexp) andalso isNonexpansive(env, e)
  | isNonexpansive(env, USyntax.FnExp _) = true
  | isNonexpansive(env, USyntax.ProjectionExp _) = true
  | isNonexpansive(env, _) = false
and isConexp(env : Env, USyntax.TypedExp(_, e, _)) = isConexp(env, e)
  | isConexp(env, USyntax.VarExp(_, Syntax.MkQualified([], USyntax.MkVId("ref", 0)), _)) = false
  | isConexp(env, USyntax.VarExp(_, _, Syntax.ValueVariable)) = false
  | isConexp(env, USyntax.VarExp(_, _, Syntax.ValueConstructor)) = true
  | isConexp(env, USyntax.VarExp(_, _, Syntax.ExceptionConstructor)) = true
  | isConexp(env, USyntax.InstantiatedVarExp(_, _, Syntax.ValueVariable, _)) = false
  | isConexp(env, USyntax.InstantiatedVarExp(_, _, Syntax.ValueConstructor, _)) = true
  | isConexp(env, USyntax.InstantiatedVarExp(_, _, Syntax.ExceptionConstructor, _)) = true
  | isConexp(env, _) = false

(* isExhaustive : Context * Env * USyntax.Pat -> bool *)
fun isExhaustive(ctx, env : Env, USyntax.WildcardPat _) = true
  | isExhaustive(ctx, env, USyntax.SConPat _) = false
  | isExhaustive(ctx, env, USyntax.VarPat _) = true
  | isExhaustive(ctx, env, USyntax.RecordPat{fields, ...}) = List.all (fn (_, e) => isExhaustive(ctx, env, e)) fields
  | isExhaustive(ctx, env, USyntax.ConPat(span, longvid, NONE)) = isSoleConstructor(ctx, env, span, longvid)
  | isExhaustive(ctx, env, USyntax.ConPat(span, longvid, SOME innerPat)) = isSoleConstructor(ctx, env, span, longvid) andalso isExhaustive(ctx, env, innerPat)
  | isExhaustive(ctx, env, USyntax.InstantiatedConPat(span, longvid, NONE, tyargs)) = isSoleConstructor(ctx, env, span, longvid)
  | isExhaustive(ctx, env, USyntax.InstantiatedConPat(span, longvid, SOME innerPat, tyargs)) = isSoleConstructor(ctx, env, span, longvid) andalso isExhaustive(ctx, env, innerPat)
  | isExhaustive(ctx, env, USyntax.TypedPat(_, innerPat, _)) = isExhaustive(ctx, env, innerPat)
  | isExhaustive(ctx, env, USyntax.LayeredPat(_, _, _, innerPat)) = isExhaustive(ctx, env, innerPat)

val primTyCon_int    = USyntax.MkLongTyCon(Syntax.MkLongTyCon([], Syntax.MkTyCon "int"), 0)
val primTyCon_word   = USyntax.MkLongTyCon(Syntax.MkLongTyCon([], Syntax.MkTyCon "word"), 1)
val primTyCon_real   = USyntax.MkLongTyCon(Syntax.MkLongTyCon([], Syntax.MkTyCon "real"), 2)
val primTyCon_string = USyntax.MkLongTyCon(Syntax.MkLongTyCon([], Syntax.MkTyCon "string"), 3)
val primTyCon_char   = USyntax.MkLongTyCon(Syntax.MkLongTyCon([], Syntax.MkTyCon "char"), 4)
val primTyCon_exn    = USyntax.MkLongTyCon(Syntax.MkLongTyCon([], Syntax.MkTyCon "exn"), 5)
val primTyCon_bool   = USyntax.MkLongTyCon(Syntax.MkLongTyCon([], Syntax.MkTyCon "bool"), 6)
val primTyCon_ref    = USyntax.MkLongTyCon(Syntax.MkLongTyCon([], Syntax.MkTyCon "ref"), 7)
val primTyCon_list   = USyntax.MkLongTyCon(Syntax.MkLongTyCon([], Syntax.MkTyCon "list"), 8)
val primTyCon_array  = USyntax.MkLongTyCon(Syntax.MkLongTyCon([], Syntax.MkTyCon "array"), 9)
val primTyCon_vector = USyntax.MkLongTyCon(Syntax.MkLongTyCon([], Syntax.MkTyCon "vector"), 10)
val primTy_unit   = USyntax.RecordType(SourcePos.nullSpan, [])
val primTy_int    = USyntax.TyCon(SourcePos.nullSpan, [], primTyCon_int)
val primTy_word   = USyntax.TyCon(SourcePos.nullSpan, [], primTyCon_word)
val primTy_real   = USyntax.TyCon(SourcePos.nullSpan, [], primTyCon_real)
val primTy_string = USyntax.TyCon(SourcePos.nullSpan, [], primTyCon_string)
val primTy_char   = USyntax.TyCon(SourcePos.nullSpan, [], primTyCon_char)
val primTy_exn    = USyntax.TyCon(SourcePos.nullSpan, [], primTyCon_exn)
val primTy_bool   = USyntax.TyCon(SourcePos.nullSpan, [], primTyCon_bool)
val VId_Bind = USyntax.MkVId("Bind", ~1)

val emptyEnv : Env
    = { tyMap = USyntax.TyConMap.empty
      , valMap = USyntax.VIdMap.empty
      , strMap = Syntax.StrIdMap.empty
      , boundTyVars = USyntax.TyVarSet.empty
      }

fun newContext() : Context
    = { nextTyVar = ref 100
      , nextVId = ref 100
      , nextTyCon = ref 100
      , tyVarConstraints = ref USyntax.TyVarMap.empty
      , tyVarSubst = ref USyntax.TyVarMap.empty
      }

fun addTyVarConstraint(ctx : Context, tv : USyntax.TyVar, ct : USyntax.UnaryConstraint)
    = let val cts = !(#tyVarConstraints ctx)
          val xs = Option.getOpt(USyntax.TyVarMap.find(cts, tv), [])
      in #tyVarConstraints ctx := USyntax.TyVarMap.insert(cts, tv, ct :: xs)
      end

fun freshTyVar(ctx : Context) : USyntax.TyVar
    = let val nextTyVar = #nextTyVar ctx
          val i = !nextTyVar
      in nextTyVar := i + 1
       ; USyntax.AnonymousTyVar(i)
      end

local open USyntax
in
(* occurCheck : TyVar -> Ty -> bool; returns true if the type variable occurs in the type *)
fun occurCheck tv = let fun check (TyVar(_, tv')) = eqUTyVar(tv, tv')
                          | check (RecordType(_, xs)) = List.exists (fn (label, ty) => check ty) xs
                          | check (TyCon(_, tyargs, longtycon)) = List.exists check tyargs
                          | check (FnType(_, ty1, ty2)) = check ty1 orelse check ty2
                    in check
                    end

(* substituteTy : TyVar * Ty -> Ty -> Ty *)
fun substituteTy (tv, replacement) =
    let fun substTy (ty as TyVar(_, tv')) = if eqUTyVar(tv, tv') then
                                                replacement
                                            else
                                                ty
          | substTy (RecordType(span, fields)) = RecordType (span, Syntax.mapRecordRow substTy fields)
          | substTy (TyCon(span, tyargs, longtycon)) = TyCon(span, List.map substTy tyargs, longtycon)
          | substTy (FnType(span, ty1, ty2)) = FnType(span, substTy ty1, substTy ty2)
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

val applySubstTy = USyntax.applySubstTy
fun applySubstEnv subst =
    let val substTy = applySubstTy subst
        fun substTypeScheme(TypeScheme(tyvars, ty))
            = let val subst' = USyntax.TyVarMap.filteri (fn (tv, ty) => not (List.exists (fn (tv', _) => eqUTyVar(tv', tv)) tyvars)) subst
              in TypeScheme(tyvars, applySubstTy subst' ty)
                 (* TODO: unwanted capture? e.g. 'a. 'a list * 'c, 'c := 'b * 'a *)
              end
        fun substEnv ({ tyMap, valMap, strMap, boundTyVars } : Env)
            = { tyMap = tyMap (* ??? *)
              , valMap = USyntax.VIdMap.map (fn (tysc, ids) => (substTypeScheme(tysc), ids)) valMap
              , strMap = Syntax.StrIdMap.map (fn MkEnv env => MkEnv (substEnv env)) strMap
              , boundTyVars = boundTyVars
              }
    in substEnv
    end

(* instantiate : Context * SourcePos.span * TypeScheme -> Ty * (Ty * UnaryConstraint list) list *)
fun instantiate(ctx, span, TypeScheme(vars, ty))
    = let val (subst, tyargs) = List.foldl (fn ((v, preds), (set, rest)) =>
                                               let val tv = freshTyVar(ctx)
                                                   val tyarg = TyVar(span, tv)
                                               in List.app (fn pred => addTyVarConstraint(ctx, tv, pred)) preds
                                                ; (USyntax.TyVarMap.insert(set, v, tyarg), (tyarg, preds) :: rest)
                                               end
                                           ) (USyntax.TyVarMap.empty, []) vars
      in (applySubstTy subst ty, List.rev tyargs)
      end

(* mergeEnv : Env * Env -> Env *)
fun mergeEnv(env1, env2) = { tyMap = USyntax.TyConMap.unionWith #2 (#tyMap env1, #tyMap env2)
                           , valMap = USyntax.VIdMap.unionWith #2 (#valMap env1, #valMap env2)
                           , strMap = Syntax.StrIdMap.unionWith #2 (#strMap env1, #strMap env2) (* TODO *)
                           , boundTyVars = USyntax.TyVarSet.union (#boundTyVars env1, #boundTyVars env2)
                           }

 (* unify : Context * Constraint list -> unit *)
fun unify(ctx : Context, nil : Constraint list) : unit = ()
  | unify(ctx, ct :: ctrs)
    = (case ct of
           (* TODO: Disallow unifying NamedTyVar *)
           EqConstr(TyVar(span, tv as AnonymousTyVar _), ty) => unifyTyVarAndTy(ctx, tv, ty, ctrs)
         | EqConstr(ty, TyVar(span, tv as AnonymousTyVar _)) => unifyTyVarAndTy(ctx, tv, ty, ctrs)
         | EqConstr(TyVar(span, tv as NamedTyVar (name, eq, x)), TyVar(span', tv' as NamedTyVar (name', eq', x'))) => if USyntax.eqUTyVar (tv, tv') then () else emitError(ctx, [span], "cannot unify named type variable: " ^ name ^ " and " ^ name')
         | EqConstr(TyVar(span, NamedTyVar (name, eq, _)), ty) => emitError(ctx, [span], "cannot unify named type variable: " ^ name)
         | EqConstr(ty, TyVar(span, NamedTyVar (name, eq, _))) => emitError(ctx, [span], "cannot unify named type variable: " ^ name)
         | EqConstr(FnType(_, s0, s1), FnType(_, t0, t1)) => unify(ctx, EqConstr(s0, t0) :: EqConstr(s1, t1) :: ctrs)
         | EqConstr(RecordType(span, fields), RecordType(span', fields')) =>
           if List.length fields <> List.length fields then
               emitError(ctx, [span, span'], "unification failed: incompatible record types (different number of fields)")
           else
               unify(ctx, List.foldl (fn ((label, ty), acc) => case List.find (fn (label', _) => label = label') fields' of
                                                                   NONE => emitError(ctx, [span, span'], "unification failed: incompatible record types")
                                                                 | SOME(_,ty') => EqConstr(ty, ty') :: acc)
                                     ctrs fields)
         | EqConstr(t1 as TyCon(span, tyarg, con), t2 as TyCon(span', tyarg', con')) =>
           if eqULongTyCon(con, con') then
               unify(ctx, (ListPair.mapEq EqConstr (tyarg, tyarg')
                           handle ListPair.UnequalLengths => emitError(ctx, [span, span'], "unification failed: the number of type arguments differ")
                          ) @ ctrs)
           else
               emitError(ctx, [span, span'], "unification failed: type constructor mismatch (" ^ USyntax.PrettyPrint.print_Ty t1 ^ " vs " ^ USyntax.PrettyPrint.print_Ty t2 ^ ")") (* ??? *)
         | EqConstr(ty1, ty2) => emitError(ctx, [], "unification failed: not match (" ^ USyntax.PrettyPrint.print_Ty ty1 ^ " vs " ^ USyntax.PrettyPrint.print_Ty ty2 ^ ")")
         | UnaryConstraint(recordTy, HasField{label = label, fieldTy = fieldTy}) =>
           (case recordTy of
                RecordType(span, fields) =>
                (case List.find (fn (label', _) => label = label') fields of
                     NONE => emitError(ctx, [span], "unification failed: no field")
                   | SOME(_, ty') => unify(ctx, EqConstr(fieldTy, ty') :: ctrs)
                )
              | TyCon(span, _, _) => emitError(ctx, [span], "record field for a non-record type")
              | FnType(span, _, _) => emitError(ctx, [span], "record field for a function type")
              | TyVar(span, tv) =>
                (case USyntax.TyVarMap.find(!(#tyVarSubst ctx), tv) of
                     SOME replacement => unify(ctx, UnaryConstraint(replacement, HasField{label = label, fieldTy = fieldTy}) :: ctrs)
                   | NONE => ( addTyVarConstraint(ctx, tv, HasField{ label = label, fieldTy = fieldTy })
                             ; unify(ctx, ctrs)
                             )
                )
           )
         | UnaryConstraint(RecordType(span, fields), IsEqType) => unify(ctx, List.map (fn (label, ty) => UnaryConstraint(ty, IsEqType)) fields @ ctrs)
         | UnaryConstraint(RecordType(span, _), IsIntegral) => emitError(ctx, [span], "cannot apply arithmetic operator on record type")
         | UnaryConstraint(RecordType(span, _), IsSignedReal) => emitError(ctx, [span], "cannot apply arithmetic operator on record type")
         | UnaryConstraint(RecordType(span, _), IsRing) => emitError(ctx, [span], "cannot apply arithmetic operator on record type")
         | UnaryConstraint(RecordType(span, _), IsField) => emitError(ctx, [span], "cannot apply arithmetic operator on record type")
         | UnaryConstraint(RecordType(span, _), IsSigned) => emitError(ctx, [span], "cannot apply arithmetic operator on record type")
         | UnaryConstraint(RecordType(span, _), IsOrdered) => emitError(ctx, [span], "cannot compare records")
         | UnaryConstraint(FnType(span, _, _), IsEqType) => emitError(ctx, [span], "function type does not admit equality")
         | UnaryConstraint(FnType(span, _, _), IsIntegral) => emitError(ctx, [span], "cannot apply arithmetic operator on function type")
         | UnaryConstraint(FnType(span, _, _), IsSignedReal) => emitError(ctx, [span], "cannot apply arithmetic operator on function type")
         | UnaryConstraint(FnType(span, _, _), IsRing) => emitError(ctx, [span], "cannot apply arithmetic operator on function type")
         | UnaryConstraint(FnType(span, _, _), IsField) => emitError(ctx, [span], "cannot apply arithmetic operator on function type")
         | UnaryConstraint(FnType(span, _, _), IsSigned) => emitError(ctx, [span], "cannot apply arithmetic operator on function type")
         | UnaryConstraint(FnType(span, _, _), IsOrdered) => emitError(ctx, [span], "cannot compare functions")
         | UnaryConstraint(TyCon(span, tyargs, longtycon), IsEqType) =>
           if List.exists (fn x => eqULongTyCon(longtycon, x)) [primTyCon_int, primTyCon_word, primTyCon_string, primTyCon_char, primTyCon_bool, primTyCon_ref, primTyCon_array] then
               (* TODO: check tyargs? *)
               unify(ctx, ctrs) (* do nothing *)
           else if eqULongTyCon(longtycon, primTyCon_list) then
               case tyargs of
                   [tyarg] => unify(ctx, [UnaryConstraint(tyarg, IsEqType)])
                 | _ => emitError(ctx, [span], "bad type arguments to list")
           else if eqULongTyCon(longtycon, primTyCon_vector) then
               case tyargs of
                   [tyarg] => unify(ctx, [UnaryConstraint(tyarg, IsEqType)])
                 | _ => emitError(ctx, [span], "bad type arguments to vector")
           else
               (* (longtycon???) : List.map IsEqType tyargs @ ctrs *)
               emitError(ctx, [span], "IsEqType TyCon: not impl")
         | UnaryConstraint(TyCon(span, tyargs, longtycon), IsIntegral) =>
           if eqULongTyCon(longtycon, primTyCon_int) orelse eqULongTyCon(longtycon, primTyCon_word) then
               unify(ctx, ctrs) (* do nothing *)
           else
               emitError(ctx, [span], "arithmetic operator on unsupported type")
         | UnaryConstraint(TyCon(span, tyargs, longtycon), IsSignedReal) =>
           if eqULongTyCon(longtycon, primTyCon_int) orelse eqULongTyCon(longtycon, primTyCon_real) then
               unify(ctx, ctrs) (* do nothing *)
           else
               emitError(ctx, [span], "arithmetic operator on unsupported type")
         | UnaryConstraint(TyCon(span, tyargs, longtycon), IsRing) =>
           if eqULongTyCon(longtycon, primTyCon_int) orelse eqULongTyCon(longtycon, primTyCon_word) orelse eqULongTyCon(longtycon, primTyCon_real) then
               unify(ctx, ctrs) (* do nothing *)
           else
               emitError(ctx, [span], "arithmetic operator on unsupported type")
         | UnaryConstraint(TyCon(span, tyargs, longtycon), IsField) =>
           if eqULongTyCon(longtycon, primTyCon_real) then
               unify(ctx, ctrs) (* do nothing *)
           else
               emitError(ctx, [span], "arithmetic operator on unsupported type")
         | UnaryConstraint(TyCon(span, tyargs, longtycon), IsSigned) =>
           if eqULongTyCon(longtycon, primTyCon_int) orelse eqULongTyCon(longtycon, primTyCon_real) then
               unify(ctx, ctrs) (* do nothing *)
           else
               emitError(ctx, [span], "arithmetic operator on unsupported type")
         | UnaryConstraint(TyCon(span, tyargs, longtycon), IsOrdered) =>
           if eqULongTyCon(longtycon, primTyCon_int) orelse eqULongTyCon(longtycon, primTyCon_word) orelse eqULongTyCon(longtycon, primTyCon_real) orelse eqULongTyCon(longtycon, primTyCon_string) orelse eqULongTyCon(longtycon, primTyCon_char) then
               unify(ctx, ctrs) (* do nothing *)
           else
               emitError(ctx, [span], "comparison operator on unsupported type")
         (* TODO: Equality type variables *)
         | UnaryConstraint(TyVar(span, tv), pred) => (case USyntax.TyVarMap.find(!(#tyVarSubst ctx), tv) of
                                                          SOME replacement => unify(ctx, UnaryConstraint(replacement, pred) :: ctrs)
                                                        | NONE => (addTyVarConstraint(ctx, tv, pred) ; unify(ctx, ctrs))
                                                     )
      )
and unifyTyVarAndTy(ctx : Context, tv : TyVar, ty : Ty, ctrs : Constraint list) : unit
    = let val subst = !(#tyVarSubst ctx)
      in case USyntax.TyVarMap.find(subst, tv) of
             SOME replacement => unify(ctx, EqConstr(replacement, ty) :: ctrs)
           | NONE =>
             let val ty = applySubstTy subst ty
             in if (case ty of TyVar(_, tv') => eqUTyVar(tv, tv') | _ => false) then (* ty = TyVar tv *)
                    unify(ctx, ctrs) (* do nothing *)
                else if occurCheck tv ty then
                    emitError(ctx, [getSourceSpanOfTy ty], "unification failed: occurrence check (" ^ USyntax.print_TyVar tv ^ " in " ^ USyntax.print_Ty ty ^ ((case ty of TyVar(_, tv') => if eqUTyVar(tv, tv') then "eqtyvar" else ", not eqtyvar" | _ => ", not tyvar")) ^ ")")
                else
                    let val tvc = !(#tyVarConstraints ctx)
                        val xs = case USyntax.TyVarMap.find(tvc, tv) of
                                     SOME xs => ( #tyVarConstraints ctx := #1 (USyntax.TyVarMap.remove(tvc, tv))
                                                ; xs
                                                )
                                   | NONE => []
                        fun toConstraint predicate = UnaryConstraint(ty, predicate)
                        val subst' = USyntax.TyVarMap.map (substituteTy (tv, ty)) subst
                    in #tyVarSubst ctx := USyntax.TyVarMap.insert(subst', tv, ty)
                     ; unify(ctx, List.map toConstraint xs @ List.map (substituteConstraint (tv, ty)) ctrs)
                    end
             end
      end
fun addConstraint(ctx : Context, ct : Constraint) = unify(ctx, [ct])

(* typeCheckExp : Context * Env * USyntax.Exp -> USyntax.Ty * USyntax.Exp *)
fun typeCheckExp(ctx : Context, env : Env, exp as SConExp(span, scon)) : USyntax.Ty * USyntax.Exp
    = let val ty = case scon of (* TODO: overloaded literals *)
                       Syntax.IntegerConstant x   => primTy_int
                     | Syntax.WordConstant x      => primTy_word
                     | Syntax.RealConstant x      => primTy_real
                     | Syntax.StringConstant x    => primTy_string
                     | Syntax.CharacterConstant x => primTy_char
      in (ty, exp)
      end
  | typeCheckExp(ctx, env, exp as VarExp(span, longvid as Syntax.MkQualified(_, USyntax.MkVId(name, _)), idstatus))
    = (case lookupLongVIdInEnv(ctx, env, span, longvid) of
           SOME (tysc, ids) => let val (ty, tyargs) = instantiate(ctx, span, tysc)
                               in (ty, InstantiatedVarExp(span, longvid, idstatus, tyargs))
                               end
         | NONE => emitError(ctx, [span], "unknown value name " ^ name)
      )
  | typeCheckExp(ctx, env, exp as InstantiatedVarExp(span, longvid as Syntax.MkQualified(_, USyntax.MkVId(name, _)), idstatus, tyargs)) (* should not reach here *)
    = let val ty = case lookupLongVIdInEnv(ctx, env, span, longvid) of
                       SOME (TypeScheme(vars, ty), ids) =>
                       let val subst = ListPair.foldlEq (fn ((var, constraints), (tyarg, _), set) =>
                                                            ( List.app (fn c => addConstraint(ctx, UnaryConstraint(tyarg, c))) constraints
                                                            ; USyntax.TyVarMap.insert(set, var, tyarg)
                                                            )
                                                        ) USyntax.TyVarMap.empty (vars, tyargs)
                       in applySubstTy subst ty
                       end
                     | NONE => emitError(ctx, [span], "unknown value name " ^ name)
      in (ty, exp)
      end
  | typeCheckExp(ctx, env, RecordExp(span, row))
    = let val (rowTy, row') = typeCheckExpRow(ctx, env, row)
      in (RecordType(span, rowTy), RecordExp(span, row'))
      end
  | typeCheckExp(ctx, env, LetInExp(span, decls, innerExp))
    = let val (env', decls') = typeCheckDecs(ctx, env, decls)
          val (ty, innerExp') = typeCheckExp(ctx, mergeEnv (env, env'), innerExp)
      in (ty, LetInExp(span, decls', innerExp'))
      end
  | typeCheckExp(ctx, env, AppExp(span, f, x))
          (* f: s -> t, x: s *)
    = let val (funcTy, f') = typeCheckExp(ctx, env, f)
          val (argTy, x') = typeCheckExp(ctx, env, x)
          val retTy = TyVar(span, freshTyVar(ctx))
      in addConstraint(ctx, EqConstr(funcTy, FnType(span, argTy, retTy))) (* funcTy = (argTy -> retTy) *)
       ; (retTy, AppExp(span, f', x'))
      end
  | typeCheckExp(ctx, env, TypedExp(span, exp, ty))
    = let val (expTy, exp') = typeCheckExp(ctx, env, exp)
      in addConstraint(ctx, EqConstr(expTy, ty)) (* ety = ty *)
       ; (ty, TypedExp(span, exp', ty))
      end
  | typeCheckExp(ctx, env, HandleExp(span, exp, matches))
          (* exp: t, matches: exn -> t *)
    = let val (expTy, exp') = typeCheckExp(ctx, env, exp)
          val (patTy, retTy, matches') = typeCheckMatch(ctx, env, matches)
      in addConstraint(ctx, EqConstr(patTy, primTy_exn)) (* patTy = exn *)
       ; addConstraint(ctx, EqConstr(expTy, retTy))
       ; (expTy, HandleExp(span, exp', matches'))
      end
  | typeCheckExp(ctx, env, RaiseExp(span, exp))
    = let val (expTy, exp') = typeCheckExp(ctx, env, exp)
      in addConstraint(ctx, EqConstr(expTy, primTy_exn)) (* expTy = exn *)
       ; (TyVar(span, freshTyVar(ctx)), RaiseExp(span, exp'))
      end
  | typeCheckExp(ctx, env, IfThenElseExp(span, cond, thenPart, elsePart))
    = let val (condTy, cond') = typeCheckExp(ctx, env, cond)
          val (thenTy, thenPart') = typeCheckExp(ctx, env, thenPart)
          val (elseTy, elsePart') = typeCheckExp(ctx, env, elsePart)
      in addConstraint(ctx, EqConstr(condTy, primTy_bool)) (* condTy = bool *)
       ; addConstraint(ctx, EqConstr(thenTy, elseTy)) (* thenTy = elseTy *)
       ; (thenTy, IfThenElseExp(span, cond', thenPart', elsePart'))
      end
  | typeCheckExp(ctx, env, CaseExp(span, exp, ty, matches))
    = let val (expTy, exp') = typeCheckExp(ctx, env, exp)
          val (patTy, retTy, matches') = typeCheckMatch(ctx, env, matches)
      in addConstraint(ctx, EqConstr(expTy, ty))
       ; addConstraint(ctx, EqConstr(expTy, patTy))
       ; (retTy, CaseExp(span, exp', ty, matches'))
      end
  | typeCheckExp(ctx, env, FnExp(span, vid, argTy, body))
    = let val env' = mergeEnv(env, { tyMap = USyntax.TyConMap.empty
                                   , valMap = USyntax.VIdMap.insert(USyntax.VIdMap.empty, vid, (TypeScheme([], argTy), Syntax.ValueVariable))
                                   , strMap = Syntax.StrIdMap.empty
                                   , boundTyVars = USyntax.TyVarSet.empty
                                   }
                             )
          val (retTy, body') = typeCheckExp(ctx, env', body)
      in (USyntax.FnType(span, argTy, retTy), FnExp(span, vid, argTy, body'))
      end
  | typeCheckExp(ctx, env, exp as ProjectionExp { sourceSpan, label, recordTy, fieldTy })
    = ( addConstraint(ctx, UnaryConstraint(recordTy, HasField { label = label, fieldTy = fieldTy }))
      ; (USyntax.FnType(sourceSpan, recordTy, fieldTy), exp)
      )
(* typeCheckDec : Context * Env * Dec -> (* created environment *) Env * Dec *)
and typeCheckDec(ctx, env, ValDec(span, tyvarseq, valbinds, _))
    = let val valbinds' = let val innerEnv = { valMap = #valMap env, tyMap = #tyMap env, strMap = #strMap env, boundTyVars = USyntax.TyVarSet.addList (#boundTyVars env, tyvarseq) }
                          in List.map (fn valbind => typeCheckValBind(ctx, innerEnv, valbind)) valbinds
                          end
          val tvc = !(#tyVarConstraints ctx)
          val subst = !(#tyVarSubst ctx)
          val env' = applySubstEnv subst env
          val tyVars_env = freeTyVarsInEnv(TyVarSet.empty, env)
          fun generalize((valbind as PatBind(span, pat, exp), expTy, valEnv, (* generalizable *) false), (valbinds, valEnvRest))
              = let val vars = USyntax.VIdMap.listItemsi valEnv
                in case vars of
                       [(vid, ty)] => let val valbind' = PolyVarBind(span, vid, TypeScheme([], ty), case pat of
                                                                                                        USyntax.VarPat _ => exp
                                                                                                      | USyntax.TypedPat (_, USyntax.VarPat _, _) => exp
                                                                                                      | _ => let val espan = USyntax.getSourceSpanOfExp exp
                                                                                                             in USyntax.CaseExp(espan, exp, ty, if isExhaustive(ctx, env, pat) then
                                                                                                                                                    [(pat, VarExp(espan, USyntax.MkLongVId([], vid), Syntax.ValueVariable))]
                                                                                                                                                else
                                                                                                                                                    [(pat, VarExp(espan, USyntax.MkLongVId([], vid), Syntax.ValueVariable))
                                                                                                                                                    ,(USyntax.WildcardPat span, USyntax.RaiseExp(span, VarExp(span, USyntax.MkLongVId([], VId_Bind), Syntax.ExceptionConstructor)))
                                                                                                                                                    ]
                                                                                                                               )
                                                                                                             end
                                                                    )
                                      in (valbind' :: valbinds, USyntax.VIdMap.unionWith #2 (USyntax.VIdMap.map (fn ty => TypeScheme([], ty)) valEnv, valEnvRest))
                                      end
                     | _ => let val espan = USyntax.getSourceSpanOfExp exp
                                val tup = USyntax.TupleExp(espan, List.map (fn (vid, _) => VarExp(espan, USyntax.MkLongVId([], vid), Syntax.ValueVariable)) vars) (* TODO: fresh vid *)
                                val valbind' = TupleBind(span, vars, USyntax.CaseExp(espan, exp, expTy, if isExhaustive(ctx, env, pat) then
                                                                                                            [(pat, tup)]
                                                                                                        else
                                                                                                            [(pat, tup)
                                                                                                            ,(USyntax.WildcardPat span, USyntax.RaiseExp(span, VarExp(span, USyntax.MkLongVId([], VId_Bind), Syntax.ExceptionConstructor)))
                                                                                                            ]
                                                                                    )
                                                        )
                            in (valbind' :: valbinds, USyntax.VIdMap.unionWith #2 (USyntax.VIdMap.map (fn ty => TypeScheme([], ty)) valEnv, valEnvRest))
                            end
                end
            | generalize((PatBind(span, pat, exp), expTy, valEnv, (* generalizable *) true), (valbinds, valEnvRest))
              = let fun doVal (vid,ty)
                        = let val ty' = applySubstTy subst ty
                              val tyVars_ty = freeTyVarsInTy(TyVarSet.empty, ty')
                              fun isEqualityType USyntax.IsEqType = true
                                | isEqualityType _ = false
                              fun isGeneralizable(tv: TyVar) = case USyntax.TyVarMap.find(tvc, tv) of
                                                                   NONE => true
                                                                 | SOME tvs => List.all isEqualityType tvs
                              val tyVars = TyVarSet.difference(TyVarSet.filter isGeneralizable tyVars_ty, tyVars_env) (* TODO: Allow equality constraint *)
                              fun doTyVar tv = case USyntax.TyVarMap.find(tvc, tv) of
                                                   NONE => (tv, [])
                                                 | SOME tvs => if List.exists isEqualityType tvs then
                                                                   (tv, [USyntax.IsEqType])
                                                               else (* should not reach here *)
                                                                   (tv, [])
                              val tysc = TypeScheme(List.map doTyVar (TyVarSet.listItems tyVars), ty')
                          in tysc
                          end
                    val valEnv' = USyntax.VIdMap.mapi doVal valEnv
                    val valEnv'L = USyntax.VIdMap.listItemsi valEnv'
                    val allPoly = List.all (fn (_, TypeScheme(tv, _)) => not (List.null tv)) valEnv'L (* all bindings are generalized? *)
                    val espan = USyntax.getSourceSpanOfExp exp
                    fun polyPart [] = []
                      | polyPart ((vid, TypeScheme([], _)) :: rest) = polyPart rest
                      | polyPart ((vid, tysc) :: rest) = PolyVarBind(span, vid, tysc, USyntax.CaseExp(espan, exp, expTy, [(USyntax.filterVarsInPat (fn x => x = vid) pat, USyntax.VarExp(espan, USyntax.MkLongVId([], vid), Syntax.ValueVariable))])) :: polyPart rest
                    fun isMonoVar vid = case USyntax.VIdMap.find(valEnv', vid) of
                                            NONE => emitError(ctx, [span], "isMonoVar: internal error")
                                          | SOME (TypeScheme([], _)) => true
                                          | SOME (TypeScheme(_ :: _, _)) => false
                    val valbind' = if allPoly then
                                       polyPart valEnv'L
                                   else
                                       let val xs = List.mapPartial (fn (vid, tysc) => case tysc of
                                                                                           TypeScheme([], ty) => SOME (vid, ty)
                                                                                         | TypeScheme(_ :: _, _) => NONE
                                                                    ) valEnv'L
                                       in case xs of
                                              [(vid, ty)] => PolyVarBind(span, vid, TypeScheme([], ty), USyntax.CaseExp(espan, exp, expTy, [(USyntax.filterVarsInPat isMonoVar pat, VarExp(espan, USyntax.MkLongVId([], vid), Syntax.ValueVariable))])) :: polyPart valEnv'L
                                            | _ => let val tup = USyntax.TupleExp(espan, List.map (fn (vid, _) => VarExp(espan, USyntax.MkLongVId([], vid), Syntax.ValueVariable)) xs)
                                                   in TupleBind(span, xs, USyntax.CaseExp(espan, exp, expTy, [(USyntax.filterVarsInPat isMonoVar pat, tup)])) :: polyPart valEnv'L
                                                   end
                                       end
                in (valbind' @ valbinds, USyntax.VIdMap.unionWith #2 (valEnv', valEnvRest))
                end
            | generalize((TupleBind(span, _, _), _, valEnv, _), (valbinds, valEnvRest)) = emitError(ctx, [span], "unexpected TupleBind")
            | generalize((PolyVarBind(span, _, _, _), _, valEnv, _), (valbinds, valEnvRest)) = emitError(ctx, [span], "unexpected PolyVarBind")
          val (valbinds'', valEnv'') = List.foldr generalize ([], USyntax.VIdMap.empty) valbinds'
          val valEnv''' = USyntax.VIdMap.map (fn tysc => (tysc, Syntax.ValueVariable)) valEnv''
          val env' = { valMap = valEnv'''
                     , tyMap = #tyMap emptyEnv
                     , strMap = #strMap emptyEnv
                     , boundTyVars = #boundTyVars emptyEnv
                     }
      in (env', ValDec(span, [], valbinds'', valEnv'''))
      end
  | typeCheckDec(ctx, env, RecValDec(span, tyvarseq, valbinds : ValBind list, _))
    = let val valbinds' : (SourcePos.span * (USyntax.Ty * USyntax.Ty USyntax.VIdMap.map * USyntax.Pat) * USyntax.Exp) list
              = List.map (fn valbind => case valbind of
                                            PatBind (span, pat, exp) => (span, typeCheckPat(ctx, env, pat), exp)
                                          | TupleBind (span, _, _) => emitError(ctx, [span], "unexpected TupleBind")
                                          | PolyVarBind (span, _, _, _) => emitError(ctx, [span], "unexpected PolyVarBind")) valbinds
          val localValEnv = List.foldl (fn ((_, (_, ve, _), _), acc) => USyntax.VIdMap.unionWith #1 (acc, ve)) USyntax.VIdMap.empty valbinds'
          val localValMap = USyntax.VIdMap.map (fn ty => (USyntax.TypeScheme ([], ty), Syntax.ValueVariable)) localValEnv
          val { valMap, tyMap, strMap, boundTyVars } = env
          val localEnv = { valMap = USyntax.VIdMap.unionWith #2 (valMap, localValMap), tyMap = tyMap, strMap = strMap, boundTyVars = USyntax.TyVarSet.addList (boundTyVars, tyvarseq) }
          val valbinds'' = List.map (fn (span, (patTy, newValEnv, pat), exp) =>
                                        let val (expTy, exp') = typeCheckExp(ctx, localEnv, exp)
                                            val () = addConstraint(ctx, EqConstr(patTy, expTy))
                                            val generalizable = isExhaustive(ctx, env, pat) andalso isNonexpansive(env, exp)
                                        in if generalizable then
                                               (span, (pat, exp'), expTy, newValEnv)
                                           else
                                               emitError(ctx, [span], "'val rec' must be generalizable")
                                        end
                                    ) valbinds'
          val tvc = !(#tyVarConstraints ctx)
          val subst = !(#tyVarSubst ctx)
          val env' = applySubstEnv subst localEnv
          val tyVars_env = freeTyVarsInEnv(TyVarSet.empty, env)
          fun generalize ((span, (pat, exp), expTy, valEnv), (valbinds, valEnvRest))
              = let fun doVal (vid, ty)
                        = let val ty' = applySubstTy subst ty
                              val tyVars_ty = freeTyVarsInTy(TyVarSet.empty, ty')
                              fun isEqualityType USyntax.IsEqType = true
                                | isEqualityType _ = false
                              fun isGeneralizable(tv: TyVar) = case USyntax.TyVarMap.find(tvc, tv) of
                                                                   NONE => true
                                                                 | SOME tvs => List.all isEqualityType tvs
                              val tyVars = TyVarSet.difference(TyVarSet.filter isGeneralizable tyVars_ty, tyVars_env)
                              fun doTyVar tv = case USyntax.TyVarMap.find(tvc, tv) of
                                                   NONE => (tv, [])
                                                 | SOME tvs => if List.exists isEqualityType tvs then
                                                                   (tv, [USyntax.IsEqType])
                                                               else (* should not reach here *)
                                                                   (tv, [])
                              val tysc = TypeScheme(List.map doTyVar (TyVarSet.listItems tyVars), ty')
                          in tysc
                          end
                    val valEnv' = USyntax.VIdMap.mapi doVal valEnv
                    val valEnv'L = USyntax.VIdMap.listItemsi valEnv'
                    val valbind' = List.map (fn (vid, tysc) => PolyVarBind(span, vid, tysc, exp)) valEnv'L
                in (valbind' @ valbinds, USyntax.VIdMap.unionWith #2 (valEnv', valEnvRest))
                end
          val (valbinds'', valEnv'') = List.foldr generalize ([], USyntax.VIdMap.empty) valbinds''
          val valEnv''' = USyntax.VIdMap.map (fn tysc => (tysc, Syntax.ValueVariable)) valEnv''
          val env' = { valMap = valEnv'''
                     , tyMap = #tyMap emptyEnv
                     , strMap = #strMap emptyEnv
                     , boundTyVars = #boundTyVars emptyEnv
                     }
      in (env', RecValDec(span, [], valbinds'', valEnv'''))
      end
  | typeCheckDec(ctx, env, dec as TypeDec(span, typbinds))
    = (emptyEnv, dec)
  | typeCheckDec(ctx, env, dec as DatatypeDec(span, datbinds))
    = let val env' = List.foldl (fn (datbind, env) => addDatBind(ctx, env, datbind)) emptyEnv datbinds
                                (* TODO: escape check *)
      in (env', dec)
      end
  | typeCheckDec(ctx, env, dec as ExceptionDec(span, exbinds))
    = (emptyEnv, dec) (* not implemented yet *)
(* typeCheckDecs : Context * Env * Dec list -> (* created environment *) Env * Dec list *)
and typeCheckDecs(ctx, env, []) : Env * Dec list = (emptyEnv, [])
  | typeCheckDecs(ctx, env, dec :: decs) = let val (env', dec') = typeCheckDec(ctx, env, dec)
                                               val (env'', decs') = typeCheckDecs(ctx, mergeEnv(env, env'), decs)
                                           in (mergeEnv(env', env''), dec' :: decs')
                                           end
(* typeCheckValBind : Context * Env * ValBind -> ValBind * USyntax.Ty * USyntax.Ty USyntax.VIdMap.map * bool *)
and typeCheckValBind(ctx, env, PatBind(span, pat, exp))
    = let val (patTy, newValEnv, pat') = typeCheckPat(ctx, env, pat)
          val (expTy, exp') = typeCheckExp(ctx, env, exp)
          val () = addConstraint(ctx, EqConstr(patTy, expTy))
          val generalizable = isExhaustive(ctx, env, pat) andalso isNonexpansive(env, exp)
      in (PatBind(span, pat', exp'), expTy, newValEnv, generalizable)
      end
  | typeCheckValBind(ctx, env, TupleBind(span, xs, exp))
    = emitError(ctx, [span], "unexpected TupleBind")
  | typeCheckValBind(ctx, env, PolyVarBind(span, vid, tysc, exp))
    = emitError(ctx, [span], "unexpected PolyVarBind")
and addDatBind(ctx, { tyMap, valMap, strMap, boundTyVars }, USyntax.DatBind(span, tyvars, tycon, conbinds))
    = let val ty = USyntax.TyCon(span, List.map (fn tv => USyntax.TyVar(span, tv)) tyvars, Syntax.MkQualified([], tycon))
          fun doConBind(USyntax.ConBind(span, vid, NONE), valMap)
              = let val typeScheme = USyntax.TypeScheme(List.map (fn tv => (tv, [])) tyvars, ty)
                in USyntax.VIdMap.insert(valMap, vid, (typeScheme, Syntax.ValueConstructor))
                end
            | doConBind(USyntax.ConBind(span, vid, SOME payloadTy), valMap)
              = let val typeScheme = USyntax.TypeScheme(List.map (fn tv => (tv, [])) tyvars, USyntax.FnType(span, payloadTy, ty))
                in USyntax.VIdMap.insert(valMap, vid, (typeScheme, Syntax.ValueConstructor))
                end
          val valEnv = List.foldl doConBind USyntax.VIdMap.empty conbinds
          val tyStr = TyStr (USyntax.TypeFcn(tyvars, ty), valEnv)
      in { tyMap = USyntax.TyConMap.insert(tyMap, tycon, tyStr)
         , valMap = USyntax.VIdMap.unionWith (fn _ => emitError(ctx, [span], "internal error: duplicate identifier")) (valMap, valEnv)
         , strMap = strMap
         , boundTyVars = boundTyVars
         }
      end
(* typeCheckExpRow : Context * Env * (Label * Exp) list -> (Label * Syntax.Ty) list * (Label * Exp) list *)
and typeCheckExpRow(ctx, env, xs) : (Syntax.Label * USyntax.Ty) list * (Syntax.Label * Exp) list
    = let fun oneField(label, exp) = case typeCheckExp(ctx, env, exp) of
                                         (ty, exp') => ((label, ty), (label, exp'))
      in ListPair.unzip (List.map oneField xs)
      end
 (* typeCheckMatch : Context * Env * (Pat * Exp) list -> (* pattern *) Syntax.Ty * (* expression *) Syntax.Ty * (Pat * Exp) list *)
and typeCheckMatch(ctx, env, (pat0, exp0) :: rest) : USyntax.Ty * USyntax.Ty * (Pat * Exp) list
    = let val (patTy, expTy, pat0', exp0') = typeCheckMatchBranch(ctx, env, pat0, exp0)
          fun oneBranch(pat, exp)
              = let val (patTy', expTy', pat', exp') = typeCheckMatchBranch(ctx, env, pat, exp)
                in addConstraint(ctx, EqConstr(patTy, patTy'))
                 ; addConstraint(ctx, EqConstr(expTy, expTy'))
                 ; (pat', exp')
                end
          val rest' = List.map oneBranch rest
      in (patTy, expTy, (pat0', exp0') :: rest')
      end
  | typeCheckMatch(ctx, env, nil) = emitError(ctx, [], "invalid syntax tree: match is empty")
and typeCheckMatchBranch(ctx : Context, env : Env, pat : Pat, exp : Exp) : USyntax.Ty * USyntax.Ty * Pat * Exp
    = let val (patTy, vars, pat') = typeCheckPat(ctx, env, pat)
          val env' = { tyMap = #tyMap env
                     , valMap = USyntax.VIdMap.unionWith #2 (#valMap env, USyntax.VIdMap.map (fn ty => (TypeScheme([], ty), Syntax.ValueVariable)) vars)
                     , strMap = #strMap env
                     , boundTyVars = #boundTyVars env
                     }
          val (expTy, exp') = typeCheckExp(ctx, env', exp)
      in (patTy, expTy, pat', exp')
      end
 (* typeCheckPat : Context * Env * Pat -> USyntax.Ty * USyntax.Ty USyntax.VIdMap.map * Pat *)
and typeCheckPat(ctx, env, pat as WildcardPat span) : USyntax.Ty * USyntax.Ty USyntax.VIdMap.map * Pat
    = let val ty = TyVar(span, freshTyVar(ctx))
      in (ty, USyntax.VIdMap.empty, pat)
      end
  | typeCheckPat(ctx, env, pat as SConPat(span, scon))
    = (case scon of
           Syntax.IntegerConstant(_)   => (primTy_int, USyntax.VIdMap.empty, pat)
         | Syntax.WordConstant(_)      => (primTy_word, USyntax.VIdMap.empty, pat)
         | Syntax.RealConstant(_)      => emitError(ctx, [span], "no real constant may occur in a pattern")
         | Syntax.StringConstant(_)    => (primTy_string, USyntax.VIdMap.empty, pat)
         | Syntax.CharacterConstant(_) => (primTy_char, USyntax.VIdMap.empty, pat)
      )
  | typeCheckPat(ctx, env, pat as VarPat(span, vid, ty))
    = (case USyntax.VIdMap.find(#valMap env, vid) of
           SOME (tysc, Syntax.ValueConstructor) => emitError(ctx, [span], "VarPat: invalid pattern")
         | SOME (tysc, Syntax.ExceptionConstructor) => emitError(ctx, [span], "VarPat: invalid pattern")
         | SOME (_, Syntax.ValueVariable) => (* shadowing *) (ty, USyntax.VIdMap.insert(USyntax.VIdMap.empty, vid, ty), pat)
         | NONE => (ty, USyntax.VIdMap.insert(USyntax.VIdMap.empty, vid, ty), pat)
      )
  | typeCheckPat(ctx, env, RecordPat{sourceSpan, fields, wildcard})
    = let val (rowTy, vars, row') = typeCheckPatRow(ctx, env, fields)
      in if wildcard then
             let val recordTy = TyVar(sourceSpan, freshTyVar(ctx))
                 fun oneField(label, ty) = addConstraint(ctx, UnaryConstraint(recordTy, HasField { label = label, fieldTy = ty }))
             in List.app oneField rowTy
              ; (recordTy, vars, RecordPat{sourceSpan=sourceSpan, fields=row', wildcard=wildcard})
             end
         else
             (RecordType(sourceSpan, rowTy), vars, RecordPat{sourceSpan=sourceSpan, fields=row', wildcard=wildcard})
      end
  | typeCheckPat(ctx, env, ConPat(span, longvid, opt_innerPat))
    = (case lookupLongVIdInEnv(ctx, env, span, longvid) of
           SOME (tysc, idstatus) =>
           (if idstatus = Syntax.ValueConstructor orelse idstatus = Syntax.ExceptionConstructor then
                let val (ty, tyargs) = instantiate(ctx, span, tysc)
                in case opt_innerPat of
                       NONE => (ty, USyntax.VIdMap.empty, InstantiatedConPat(span, longvid, NONE, List.map #1 tyargs))
                     | SOME innerPat =>
                       (case ty of
                            USyntax.FnType(span', argTy, resultTy) =>
                            let val (argTy', innerVars, innerPat') = typeCheckPat(ctx, env, innerPat)
                            in addConstraint(ctx, EqConstr(argTy, argTy'))
                             ; (resultTy, innerVars, InstantiatedConPat(span, longvid, SOME innerPat', List.map #1 tyargs))
                            end
                          | _ => emitError(ctx, [span], "invalid pattern")
                       )
                end
            else (* idstatus = Syntax.ValueVariable *)
                emitError(ctx, [span], "invalid pattern")
           )
         | NONE => emitError(ctx, [span], "invalid pattern")
      )
  | typeCheckPat(ctx, env, pat as InstantiatedConPat(span, longvid, opt_innerPat, tyargs)) (* should not reach here *)
    = (case lookupLongVIdInEnv(ctx, env, span, longvid) of
           SOME (TypeScheme(vars, ty), idstatus) =>
           (if idstatus = Syntax.ValueConstructor orelse idstatus = Syntax.ExceptionConstructor then
                let val subst = ListPair.foldlEq (fn ((var, constraints), tyarg, set) =>
                                                     ( List.app (fn c => addConstraint(ctx, UnaryConstraint(tyarg, c))) constraints
                                                     ; USyntax.TyVarMap.insert(set, var, tyarg)
                                                     )
                                                 ) USyntax.TyVarMap.empty (vars, tyargs)
                    val ty' = applySubstTy subst ty
                in case opt_innerPat of
                       NONE => (ty', USyntax.VIdMap.empty, pat)
                     | SOME innerPat => (case ty' of
                                             USyntax.FnType(span', argTy, resultTy) => let val (argTy', innerVars, innerPat') = typeCheckPat(ctx, env, innerPat)
                                                                                       in addConstraint(ctx, EqConstr(argTy, argTy'))
                                                                                        ; (resultTy, innerVars, InstantiatedConPat(span, longvid, SOME innerPat', tyargs))
                                                                                       end
                                           | _ => emitError(ctx, [span], "invalid pattern")
                                        )
                end
            else (* idstatus = Syntax.ValueVariable *)
                emitError(ctx, [span], "invalid pattern")
           )
         | NONE => emitError(ctx, [span], "invalid pattern")
      )
  | typeCheckPat(ctx, env, pat as TypedPat(_, WildcardPat _, ty))
    = (ty, USyntax.VIdMap.empty, pat)
  | typeCheckPat(ctx, env, TypedPat(span, pat, ty))
    = let val (inferredTy, vars, pat') = typeCheckPat(ctx, env, pat)
      in addConstraint(ctx, EqConstr(ty, inferredTy))
       ; (ty, vars, TypedPat(span, pat', ty))
      end
  | typeCheckPat(ctx, env, LayeredPat(span, vid, ty, pat))
    = let val (inferredTy, vars, pat') = typeCheckPat(ctx, env, pat)
      in case USyntax.VIdMap.find(vars, vid) of
             NONE => ( addConstraint(ctx, EqConstr(ty, inferredTy))
                     ; (ty, USyntax.VIdMap.insert(vars, vid, ty), LayeredPat(span, vid, ty, pat'))
                     )
           | SOME _ => emitError(ctx, [span], "trying to bind the same identifier twice")
      end
 (* typeCheckPatRow : Context * Env * (Label * Pat) list -> (Label * Syntax.Ty) list * Syntax.Ty USyntax.VIdMap.map * (Label * Pat) list *)
and typeCheckPatRow(ctx, env, row)
    = let fun oneField((label, pat), (row, vars, rest))
              = let val (ty, vars', pat') = typeCheckPat(ctx, env, pat)
                in ((label, ty) :: row, USyntax.VIdMap.unionWith (fn _ => emitError(ctx, [], "trying to bind the same identifier twice")) (vars, vars'), (label, pat') :: rest)
                end
      in List.foldr oneField ([], USyntax.VIdMap.empty, []) row (* TODO: Is this right? *)
      end

(* typeCheckExp : Context * Env * USyntax.Exp -> (UnaryConstraint list) USyntax.TyVarMap.map * USyntax.Ty * USyntax.Exp *)
fun typeCheckExp_(ctx, env, exp) = let val (ty, exp') = typeCheckExp(ctx, env, exp)
                                       val subst = !(#tyVarSubst ctx)
                                       val tvc = !(#tyVarConstraints ctx)
                                       val applySubst = applySubstTy subst
                                   in (tvc, applySubst ty, USyntax.mapTyInExp applySubst exp')
                                   end

(* typeCheckProgram : Context * Env * USyntax.Dec list -> Env * (UnaryConstraint list) USyntax.TyVarMap.map * USyntax.Dec list *)
fun typeCheckProgram(ctx, env, decls) = let val (env', decls') = typeCheckDecs(ctx, env, decls)
                                            val subst = !(#tyVarSubst ctx)
                                            val tvc = !(#tyVarConstraints ctx)
                                            val applySubst = applySubstTy subst
                                        in (env', tvc, List.map (USyntax.mapTyInDec applySubst) decls')
                                        end

(* pretty printing *)
structure PrettyPrint = struct
fun print_Env ({ tyMap, valMap, strMap, boundTyVars } : Env) = "Env{tyMap=" ^ USyntax.print_TyConMap (fn (TyStr _) => "TyStr _") tyMap ^ ",valMap=" ^ USyntax.print_VIdMap (Syntax.print_pair (USyntax.print_TypeScheme, Syntax.print_IdStatus)) valMap ^ ",strMap=" ^ Syntax.print_StrIdMap (fn MkEnv env => print_Env env) strMap ^ ",boundTyVars=...}"
end (* structure PrettyPrint *)
open PrettyPrint

(* applyDefaultTypes : Context * (UnaryConstraint list) USyntax.TyVarMap.map * USyntax.Dec list -> USyntax.Dec list *)
fun applyDefaultTypes(ctx, tvc, decs) =
    let fun doInt [] = primTy_int
          | doInt (USyntax.HasField{...} :: xs) = emitError(ctx, [], "invalid record syntax for int")
          | doInt (USyntax.IsEqType :: xs) = doInt xs
          | doInt (USyntax.IsIntegral :: xs) = doInt xs
          | doInt (USyntax.IsSignedReal :: xs) = doInt xs
          | doInt (USyntax.IsRing :: xs) = doInt xs
          | doInt (USyntax.IsField :: xs) = emitError(ctx, [], "cannot apply / operator for int")
          | doInt (USyntax.IsSigned :: xs) = doInt xs
          | doInt (USyntax.IsOrdered :: xs) = doInt xs
        fun doReal [] = primTy_real
          | doReal (USyntax.HasField{...} :: xs) = emitError(ctx, [], "invalid record syntax for real")
          | doReal (USyntax.IsEqType :: xs) = emitError(ctx, [], "real does not admit equality")
          | doReal (USyntax.IsIntegral :: xs) = emitError(ctx, [], "div, mod is invalid for real")
          | doReal (USyntax.IsSignedReal :: xs) = doReal xs
          | doReal (USyntax.IsRing :: xs) = doReal xs
          | doReal (USyntax.IsField :: xs) = doReal xs
          | doReal (USyntax.IsSigned :: xs) = doReal xs
          | doReal (USyntax.IsOrdered :: xs) = doReal xs
        fun doIntOrReal [] = primTy_int
          | doIntOrReal (USyntax.HasField{...} :: _) = emitError(ctx, [], "unresolved flex record")
          | doIntOrReal (USyntax.IsEqType :: xs) = doInt xs
          | doIntOrReal (USyntax.IsIntegral :: xs) = doInt xs
          | doIntOrReal (USyntax.IsSignedReal :: xs) = doIntOrReal xs
          | doIntOrReal (USyntax.IsRing :: xs) = doIntOrReal xs
          | doIntOrReal (USyntax.IsField :: xs) = doReal xs
          | doIntOrReal (USyntax.IsSigned :: xs) = doIntOrReal xs
          | doIntOrReal (USyntax.IsOrdered :: xs) = doIntOrReal xs
        fun defaultTyForConstraints(eq, []) = primTy_unit
          | defaultTyForConstraints(eq, USyntax.HasField{...} :: _) = emitError(ctx, [], "unresolved flex record")
          | defaultTyForConstraints(eq, USyntax.IsEqType :: xs) = defaultTyForConstraints(true, xs)
          | defaultTyForConstraints(eq, USyntax.IsIntegral :: xs) = doInt xs
          | defaultTyForConstraints(eq, USyntax.IsSignedReal :: xs) = if eq then doInt xs else doIntOrReal xs
          | defaultTyForConstraints(eq, USyntax.IsRing :: xs) = if eq then doInt xs else doIntOrReal xs
          | defaultTyForConstraints(eq, USyntax.IsField :: xs) = if eq then emitError(ctx, [], "real does not admit equality") else doReal xs
          | defaultTyForConstraints(eq, USyntax.IsSigned :: xs) = if eq then doInt xs else doIntOrReal xs
          | defaultTyForConstraints(eq, USyntax.IsOrdered :: xs) = if eq then doInt xs else doIntOrReal xs
        fun doTyVar tv = case TyVarMap.find(tvc, tv) of
                             NONE => primTy_unit
                           | SOME constraints => defaultTyForConstraints(false, constraints)
        val freeTyVars = USyntax.freeTyVarsInDecs(USyntax.TyVarSet.empty, decs)
        val subst = USyntax.TyVarSet.foldl (fn (tv, map) => USyntax.TyVarMap.insert(map, tv, doTyVar tv)) USyntax.TyVarMap.empty freeTyVars
    in List.map (USyntax.mapTyInDec (applySubstTy subst)) decs
    end

end (* local *)
end (* structure Typing *)
