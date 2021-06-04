(*
 * Copyright (c) 2021 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure Typing = struct

type TypeStructure = { typeFunction : USyntax.TypeFunction
                     , valEnv : USyntax.ValEnv
                     , admitsEquality : bool
                     }

datatype Env' = MkEnv of Env
withtype Env = { tyMap : TypeStructure USyntax.TyConMap.map
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
fun freeTyVarsInConstraint(bound, USyntax.EqConstr(span, ty1, ty2)) = USyntax.TyVarSet.union(USyntax.freeTyVarsInTy(bound, ty1), USyntax.freeTyVarsInTy(bound, ty2))
  | freeTyVarsInConstraint(bound, USyntax.UnaryConstraint(span, ty, unaryConstraint))
    = (case unaryConstraint of
           USyntax.HasField{fieldTy = fieldTy, ...} => USyntax.TyVarSet.union(USyntax.freeTyVarsInTy(bound, ty), USyntax.freeTyVarsInTy(bound, fieldTy))
         | USyntax.IsEqType _    => USyntax.freeTyVarsInTy(bound, ty)
         | USyntax.IsIntegral _   => USyntax.freeTyVarsInTy(bound, ty)
         | USyntax.IsSignedReal _ => USyntax.freeTyVarsInTy(bound, ty)
         | USyntax.IsRing _       => USyntax.freeTyVarsInTy(bound, ty)
         | USyntax.IsField _      => USyntax.freeTyVarsInTy(bound, ty)
         | USyntax.IsSigned _     => USyntax.freeTyVarsInTy(bound, ty)
         | USyntax.IsOrdered _    => USyntax.freeTyVarsInTy(bound, ty)
      )

type ProgramContext = { nextTyVar : int ref
                      , nextVId : int ref
                      , nextTyCon : int ref
                      }

type Context = { nextTyVar : int ref
               , nextVId : int ref
               , nextTyCon : int ref
               , tyVarConstraints : ((USyntax.UnaryConstraint list) USyntax.TyVarMap.map) ref
               , tyVarSubst : Subst ref
               }

exception TypeError of SourcePos.span list * string

fun emitErrorP(ctx : ProgramContext, spans, message) = raise TypeError (spans, message)
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

(* getConstructedType : Context * SourcePos.span * USyntax.Ty -> USyntax.TyCon *)
fun getConstructedType(ctx, span, USyntax.TyVar _) = emitError(ctx, [span], "getConstructedType: got a type variable")
  | getConstructedType(ctx, span, USyntax.RecordType _) = emitError(ctx, [span], "getConstructedType: got a record")
  | getConstructedType(ctx, span, USyntax.TyCon(_, tyargs, tycon)) = tycon
  | getConstructedType(ctx, span, USyntax.FnType(_, _, t)) = getConstructedType(ctx, span, t)

(* isSoleConstructor : Context * Env * SourcePos.span * USyntax.LongVId -> bool *)
fun isSoleConstructor(ctx : Context, env : Env, span : SourcePos.span, longvid: USyntax.LongVId) =
    (case lookupLongVIdInEnv(ctx, env, span, longvid) of
         NONE => false (* probably an error *)
       | SOME (USyntax.TypeScheme(_, ty), Syntax.ValueConstructor) =>
         let val tycon = getConstructedType(ctx, span, ty)
             val { valEnv = valenv, ... } = lookupTyConInEnv(ctx, env, span, tycon)
         in USyntax.VIdMap.numItems valenv = 1
         end
       | SOME (_, Syntax.ValueVariable) => false
       | SOME (_, Syntax.ExceptionConstructor) => false
    )

val VId_ref = USyntax.MkVId("ref", ~2)

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
  | isNonexpansive(env, USyntax.ListExp(_, xs, _)) = Vector.all (fn x => isNonexpansive(env, x)) xs
  | isNonexpansive(env, _) = false
and isConexp(env : Env, USyntax.TypedExp(_, e, _)) = isConexp(env, e)
  | isConexp(env, USyntax.VarExp(_, _, Syntax.ValueVariable)) = false
  | isConexp(env, USyntax.VarExp(_, Syntax.MkQualified(_, vid), Syntax.ValueConstructor)) = not (USyntax.eqVId(vid, VId_ref))
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

val primTyCon_int    = USyntax.MkTyCon("int", 0)
val primTyCon_word   = USyntax.MkTyCon("word", 1)
val primTyCon_real   = USyntax.MkTyCon("real", 2)
val primTyCon_string = USyntax.MkTyCon("string", 3)
val primTyCon_char   = USyntax.MkTyCon("char", 4)
val primTyCon_exn    = USyntax.MkTyCon("exn", 5)
val primTyCon_bool   = USyntax.MkTyCon("bool", 6)
val primTyCon_ref    = USyntax.MkTyCon("ref", 7)
val primTyCon_list   = USyntax.MkTyCon("list", 8)
val primTyCon_array  = USyntax.MkTyCon("array", 9)
val primTyCon_vector = USyntax.MkTyCon("vector", 10)
(* primTyCon_Lua_value : 11 *)
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

fun newContext() : ProgramContext
    = { nextTyVar = ref 100
      , nextVId = ref 100
      , nextTyCon = ref 100
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

fun renewVId (ctx : Context) (USyntax.MkVId(name, _)) : USyntax.VId
    = let val n = !(#nextVId ctx)
      in #nextVId ctx := n + 1
       ; USyntax.MkVId(name, n)
      end

local structure U = USyntax
in
(* occurCheck : U.TyVar -> U.Ty -> bool; returns true if the type variable occurs in the type *)
fun occurCheck tv = let fun check (U.TyVar(_, tv')) = U.eqUTyVar(tv, tv')
                          | check (U.RecordType(_, xs)) = List.exists (fn (label, ty) => check ty) xs
                          | check (U.TyCon(_, tyargs, tycon)) = List.exists check tyargs
                          | check (U.FnType(_, ty1, ty2)) = check ty1 orelse check ty2
                    in check
                    end

(* substituteTy : U.TyVar * U.Ty -> U.Ty -> U.Ty *)
fun substituteTy (tv, replacement) =
    let fun substTy (ty as U.TyVar(_, tv')) = if U.eqUTyVar(tv, tv') then
                                                  replacement
                                              else
                                                  ty
          | substTy (U.RecordType(span, fields)) = U.RecordType (span, Syntax.mapRecordRow substTy fields)
          | substTy (U.TyCon(span, tyargs, tycon)) = U.TyCon(span, List.map substTy tyargs, tycon)
          | substTy (U.FnType(span, ty1, ty2)) = U.FnType(span, substTy ty1, substTy ty2)
    in substTy
    end

(* substituteConstraint : TyVar * Ty -> Constraint -> Constraint *)
fun substituteConstraint (tv, replacement) =
    let val substTy = substituteTy (tv, replacement)
    in fn U.EqConstr(span, ty1, ty2) => U.EqConstr(span, substTy ty1, substTy ty2)
     | U.UnaryConstraint(span1, recordTy, U.HasField{sourceSpan, label, fieldTy }) => U.UnaryConstraint(span1, substTy recordTy, U.HasField{sourceSpan = sourceSpan, label = label, fieldTy = substTy fieldTy})
     | U.UnaryConstraint(span1, ty, U.IsEqType span2) => U.UnaryConstraint(span1, substTy ty, U.IsEqType span2)
     | U.UnaryConstraint(span1, ty, U.IsIntegral span2) => U.UnaryConstraint(span1, substTy ty, U.IsIntegral span2)
     | U.UnaryConstraint(span1, ty, U.IsSignedReal span2) => U.UnaryConstraint(span1, substTy ty, U.IsSignedReal span2)
     | U.UnaryConstraint(span1, ty, U.IsRing span2) => U.UnaryConstraint(span1, substTy ty, U.IsRing span2)
     | U.UnaryConstraint(span1, ty, U.IsField span2) => U.UnaryConstraint(span1, substTy ty, U.IsField span2)
     | U.UnaryConstraint(span1, ty, U.IsSigned span2) => U.UnaryConstraint(span1, substTy ty, U.IsSigned span2)
     | U.UnaryConstraint(span1, ty, U.IsOrdered span2) => U.UnaryConstraint(span1, substTy ty, U.IsOrdered span2)
    end

val applySubstTy = U.applySubstTy
fun applySubstEnv subst =
    let val substTy = applySubstTy subst
        fun substTypeScheme(U.TypeScheme(tyvars, ty))
            = let val subst' = U.TyVarMap.filteri (fn (tv, ty) => not (List.exists (fn (tv', _) => U.eqUTyVar(tv', tv)) tyvars)) subst
              in U.TypeScheme(tyvars, applySubstTy subst' ty)
                 (* TODO: unwanted capture? e.g. 'a. 'a list * 'c, 'c := 'b * 'a *)
              end
        fun substEnv ({ tyMap, valMap, strMap, boundTyVars } : Env)
            = { tyMap = tyMap (* ??? *)
              , valMap = U.VIdMap.map (fn (tysc, ids) => (substTypeScheme(tysc), ids)) valMap
              , strMap = Syntax.StrIdMap.map (fn MkEnv env => MkEnv (substEnv env)) strMap
              , boundTyVars = boundTyVars
              }
    in substEnv
    end

(* instantiate : Context * SourcePos.span * U.TypeScheme -> U.Ty * (U.Ty * U.UnaryConstraint list) list *)
fun instantiate(ctx, span, U.TypeScheme(vars, ty))
    = let val (subst, tyargs) = List.foldl (fn ((v, preds), (set, rest)) =>
                                               let val tv = freshTyVar(ctx)
                                                   val tyarg = U.TyVar(span, tv)
                                               in List.app (fn pred => addTyVarConstraint(ctx, tv, pred)) preds
                                                ; (U.TyVarMap.insert(set, v, tyarg), (tyarg, preds) :: rest)
                                               end
                                           ) (U.TyVarMap.empty, []) vars
      in (applySubstTy subst ty, List.rev tyargs)
      end

(* mergeEnv : Env * Env -> Env *)
fun mergeEnv(env1, env2) = { tyMap = USyntax.TyConMap.unionWith #2 (#tyMap env1, #tyMap env2)
                           , valMap = USyntax.VIdMap.unionWith #2 (#valMap env1, #valMap env2)
                           , strMap = Syntax.StrIdMap.unionWith #2 (#strMap env1, #strMap env2) (* TODO *)
                           , boundTyVars = USyntax.TyVarSet.union (#boundTyVars env1, #boundTyVars env2)
                           }

(* unify : Context * Env * Constraint list -> unit *)
(* The environment is used to determine if a data type admits equality *)
fun unify(ctx : Context, env : Env, nil : U.Constraint list) : unit = ()
  | unify(ctx, env, ct :: ctrs)
    = (case ct of
           U.EqConstr(span1, U.TyVar(span2, tv as U.AnonymousTyVar _), ty) => unifyTyVarAndTy(ctx, env, span1, tv, ty, ctrs)
         | U.EqConstr(span1, ty, U.TyVar(span2, tv as U.AnonymousTyVar _)) => unifyTyVarAndTy(ctx, env, span1, tv, ty, ctrs)
         | U.EqConstr(span1, U.TyVar(span2, tv as U.NamedTyVar (name, eq, x)), U.TyVar(span3, tv' as U.NamedTyVar (name', eq', x'))) =>
           if U.eqUTyVar (tv, tv') then
               unify(ctx, env, ctrs) (* do nothing *)
           else
               emitError(ctx, [span1, span2, span3], "cannot unify named type variable: " ^ name ^ " and " ^ name')
         | U.EqConstr(span1, U.TyVar(span2, U.NamedTyVar (name, eq, _)), ty) => emitError(ctx, [span1, span2], "cannot unify named type variable: " ^ name)
         | U.EqConstr(span1, ty, U.TyVar(span2, U.NamedTyVar (name, eq, _))) => emitError(ctx, [span1, span2], "cannot unify named type variable: " ^ name)
         | U.EqConstr(span, U.FnType(_, s0, s1), U.FnType(_, t0, t1)) => unify(ctx, env, U.EqConstr(span, s0, t0) :: U.EqConstr(span, s1, t1) :: ctrs)
         | U.EqConstr(span1, U.RecordType(span2, fields), U.RecordType(span3, fields')) =>
           if List.length fields <> List.length fields then
               emitError(ctx, [span1, span2, span3], "unification failed: incompatible record types (different number of fields)")
           else
               unify(ctx, env, List.foldl (fn ((label, ty), acc) => case List.find (fn (label', _) => label = label') fields' of
                                                                        NONE => emitError(ctx, [span1, span2, span3], "unification failed: incompatible record types")
                                                                      | SOME(_,ty') => U.EqConstr(span1, ty, ty') :: acc)
                                          ctrs fields)
         | U.EqConstr(span1, t1 as U.TyCon(span2, tyarg, con), t2 as U.TyCon(span3, tyarg', con')) =>
           if U.eqUTyCon(con, con') then
               unify(ctx, env, (ListPair.mapEq (fn (x, y) => U.EqConstr(span1, x, y)) (tyarg, tyarg')
                                handle ListPair.UnequalLengths => emitError(ctx, [span1, span2, span3], "unification failed: the number of type arguments differ")
                               ) @ ctrs)
           else
               emitError(ctx, [span1, span2, span3], "unification failed: type constructor mismatch (" ^ USyntax.PrettyPrint.print_Ty t1 ^ " vs " ^ USyntax.PrettyPrint.print_Ty t2 ^ ")") (* ??? *)
         | U.EqConstr(span, ty1, ty2) => emitError(ctx, [span], "unification failed: not match (" ^ USyntax.PrettyPrint.print_Ty ty1 ^ " vs " ^ USyntax.PrettyPrint.print_Ty ty2 ^ ")")
         | U.UnaryConstraint(span1, recordTy, U.HasField{sourceSpan = span3, label = label, fieldTy = fieldTy}) =>
           (case recordTy of
                U.RecordType(span2, fields) =>
                (case List.find (fn (label', _) => label = label') fields of
                     NONE => emitError(ctx, [span1, span2, span3], "unification failed: no field")
                   | SOME(_, ty') => unify(ctx, env, U.EqConstr(span1, fieldTy, ty') :: ctrs)
                )
              | U.TyCon(span2, _, _) => emitError(ctx, [span1, span2, span3], "record field for a non-record type")
              | U.FnType(span2, _, _) => emitError(ctx, [span1, span2, span3], "record field for a function type")
              | U.TyVar(span2, tv) =>
                (case USyntax.TyVarMap.find(!(#tyVarSubst ctx), tv) of
                     SOME replacement => unify(ctx, env, U.UnaryConstraint(span1, replacement, U.HasField{sourceSpan = span3, label = label, fieldTy = fieldTy}) :: ctrs)
                   | NONE => ( addTyVarConstraint(ctx, tv, U.HasField{ sourceSpan = span3, label = label, fieldTy = fieldTy })
                             ; unify(ctx, env, ctrs)
                             )
                )
           )
         | U.UnaryConstraint(span1, U.RecordType(span2, fields), U.IsEqType span3) => unify(ctx, env, List.map (fn (label, ty) => U.UnaryConstraint(span1, ty, U.IsEqType span3)) fields @ ctrs)
         | U.UnaryConstraint(span1, U.RecordType(span2, _), U.IsIntegral span3) => emitError(ctx, [span1, span2, span3], "cannot apply arithmetic operator on record type")
         | U.UnaryConstraint(span1, U.RecordType(span2, _), U.IsSignedReal span3) => emitError(ctx, [span1, span2, span3], "cannot apply arithmetic operator on record type")
         | U.UnaryConstraint(span1, U.RecordType(span2, _), U.IsRing span3) => emitError(ctx, [span1, span2, span3], "cannot apply arithmetic operator on record type")
         | U.UnaryConstraint(span1, U.RecordType(span2, _), U.IsField span3) => emitError(ctx, [span1, span2, span3], "cannot apply arithmetic operator on record type")
         | U.UnaryConstraint(span1, U.RecordType(span2, _), U.IsSigned span3) => emitError(ctx, [span1, span2, span3], "cannot apply arithmetic operator on record type")
         | U.UnaryConstraint(span1, U.RecordType(span2, _), U.IsOrdered span3) => emitError(ctx, [span1, span2, span3], "cannot compare records")
         | U.UnaryConstraint(span1, U.FnType(span2, _, _), U.IsEqType span3) => emitError(ctx, [span1, span2, span3], "function type does not admit equality")
         | U.UnaryConstraint(span1, U.FnType(span2, _, _), U.IsIntegral span3) => emitError(ctx, [span1, span2, span3], "cannot apply arithmetic operator on function type")
         | U.UnaryConstraint(span1, U.FnType(span2, _, _), U.IsSignedReal span3) => emitError(ctx, [span1, span2, span3], "cannot apply arithmetic operator on function type")
         | U.UnaryConstraint(span1, U.FnType(span2, _, _), U.IsRing span3) => emitError(ctx, [span1, span2, span3], "cannot apply arithmetic operator on function type")
         | U.UnaryConstraint(span1, U.FnType(span2, _, _), U.IsField span3) => emitError(ctx, [span1, span2, span3], "cannot apply arithmetic operator on function type")
         | U.UnaryConstraint(span1, U.FnType(span2, _, _), U.IsSigned span3) => emitError(ctx, [span1, span2, span3], "cannot apply arithmetic operator on function type")
         | U.UnaryConstraint(span1, U.FnType(span2, _, _), U.IsOrdered span3) => emitError(ctx, [span1, span2, span3], "cannot compare functions")
         | U.UnaryConstraint(span1, U.TyCon(span2, tyargs, tycon), U.IsEqType span3) =>
           let val { admitsEquality, ... } = lookupTyConInEnv(ctx, env, span2, tycon)
           in if admitsEquality then
                  unify(ctx, env, List.map (fn tyarg => U.UnaryConstraint(span1, tyarg, U.IsEqType span3)) tyargs @ ctrs)
              else if U.eqUTyCon(tycon, primTyCon_ref) orelse U.eqUTyCon(tycon, primTyCon_array) then
                  unify(ctx, env, ctrs)
              else
                  emitError(ctx, [span1, span2, span3], USyntax.PrettyPrint.print_TyCon tycon ^ " does not admit equality")
           end
         | U.UnaryConstraint(span1, U.TyCon(span2, tyargs, tycon), U.IsIntegral span3) =>
           if U.eqUTyCon(tycon, primTyCon_int) orelse U.eqUTyCon(tycon, primTyCon_word) then
               unify(ctx, env, ctrs) (* do nothing *)
           else
               emitError(ctx, [span1, span2, span3], "arithmetic operator on unsupported type")
         | U.UnaryConstraint(span1, U.TyCon(span2, tyargs, tycon), U.IsSignedReal span3) =>
           if U.eqUTyCon(tycon, primTyCon_int) orelse U.eqUTyCon(tycon, primTyCon_real) then
               unify(ctx, env, ctrs) (* do nothing *)
           else
               emitError(ctx, [span1, span2, span3], "arithmetic operator on unsupported type")
         | U.UnaryConstraint(span1, U.TyCon(span2, tyargs, tycon), U.IsRing span3) =>
           if U.eqUTyCon(tycon, primTyCon_int) orelse U.eqUTyCon(tycon, primTyCon_word) orelse U.eqUTyCon(tycon, primTyCon_real) then
               unify(ctx, env, ctrs) (* do nothing *)
           else
               emitError(ctx, [span1, span2, span3], "arithmetic operator on unsupported type")
         | U.UnaryConstraint(span1, U.TyCon(span2, tyargs, tycon), U.IsField span3) =>
           if U.eqUTyCon(tycon, primTyCon_real) then
               unify(ctx, env, ctrs) (* do nothing *)
           else
               emitError(ctx, [span1, span2, span3], "arithmetic operator on unsupported type")
         | U.UnaryConstraint(span1, U.TyCon(span2, tyargs, tycon), U.IsSigned span3) =>
           if U.eqUTyCon(tycon, primTyCon_int) orelse U.eqUTyCon(tycon, primTyCon_real) then
               unify(ctx, env, ctrs) (* do nothing *)
           else
               emitError(ctx, [span1, span2, span3], "arithmetic operator on unsupported type")
         | U.UnaryConstraint(span1, U.TyCon(span2, tyargs, tycon), U.IsOrdered span3) =>
           if U.eqUTyCon(tycon, primTyCon_int) orelse U.eqUTyCon(tycon, primTyCon_word) orelse U.eqUTyCon(tycon, primTyCon_real) orelse U.eqUTyCon(tycon, primTyCon_string) orelse U.eqUTyCon(tycon, primTyCon_char) then
               unify(ctx, env, ctrs) (* do nothing *)
           else
               emitError(ctx, [span1, span2, span3], "comparison operator on unsupported type")
         | U.UnaryConstraint(span1, U.TyVar(span2, tv), pred) =>
           (case USyntax.TyVarMap.find(!(#tyVarSubst ctx), tv) of
                SOME replacement => unify(ctx, env, U.UnaryConstraint(span1, replacement, pred) :: ctrs)
              | NONE => case (tv, pred) of
                            (U.NamedTyVar (name, eq, _), U.IsEqType span3) =>
                            if eq then
                                unify(ctx, env, ctrs)
                            else
                                emitError(ctx, [span1, span2, span3], "the type variable " ^ name ^ " does not admit equality")
                          | (U.NamedTyVar(name, _, _), _) => emitError(ctx, [span1, span2], "the use of " ^ name ^ " is non-free")
                          | _ => (addTyVarConstraint(ctx, tv, pred) ; unify(ctx, env, ctrs))
           )
      )
and unifyTyVarAndTy(ctx : Context, env : Env, span : SourcePos.span, tv : U.TyVar, ty : U.Ty, ctrs : U.Constraint list) : unit
    = let val subst = !(#tyVarSubst ctx)
      in case USyntax.TyVarMap.find(subst, tv) of
             SOME replacement => unify(ctx, env, U.EqConstr(span, replacement, ty) :: ctrs)
           | NONE =>
             let val ty = applySubstTy subst ty
             in if (case ty of U.TyVar(_, tv') => U.eqUTyVar(tv, tv') | _ => false) then (* ty = TyVar tv *)
                    unify(ctx, env, ctrs) (* do nothing *)
                else if occurCheck tv ty then
                    emitError(ctx, [span, U.getSourceSpanOfTy ty], "unification failed: occurrence check (" ^ USyntax.print_TyVar tv ^ " in " ^ USyntax.print_Ty ty ^ ((case ty of U.TyVar(_, tv') => if U.eqUTyVar(tv, tv') then "eqtyvar" else ", not eqtyvar" | _ => ", not tyvar")) ^ ")")
                else
                    let val tvc = !(#tyVarConstraints ctx)
                        val xs = case USyntax.TyVarMap.find(tvc, tv) of
                                     SOME xs => ( #tyVarConstraints ctx := #1 (USyntax.TyVarMap.remove(tvc, tv))
                                                ; xs
                                                )
                                   | NONE => []
                        fun toConstraint predicate = U.UnaryConstraint(span, ty, predicate)
                        val subst' = USyntax.TyVarMap.map (substituteTy (tv, ty)) subst
                    in #tyVarSubst ctx := USyntax.TyVarMap.insert(subst', tv, ty)
                     ; unify(ctx, env, List.map toConstraint xs @ List.map (substituteConstraint (tv, ty)) ctrs)
                    end
             end
      end
fun addConstraint(ctx : Context, env : Env, ct : U.Constraint) = unify(ctx, env, [ct])

(* typeCheckExp : Context * Env * U.Exp -> U.Ty * U.Exp *)
fun typeCheckExp(ctx : Context, env : Env, exp as U.SConExp(span, scon)) : U.Ty * U.Exp
    = let val ty = case scon of (* TODO: overloaded literals *)
                       Syntax.IntegerConstant x   => primTy_int
                     | Syntax.WordConstant x      => primTy_word
                     | Syntax.RealConstant x      => primTy_real
                     | Syntax.StringConstant x    => primTy_string
                     | Syntax.CharacterConstant x => primTy_char
      in (ty, exp)
      end
  | typeCheckExp(ctx, env, exp as U.VarExp(span, longvid as Syntax.MkQualified(_, USyntax.MkVId(name, _)), idstatus))
    = (case lookupLongVIdInEnv(ctx, env, span, longvid) of
           SOME (tysc, ids) => let val (ty, tyargs) = instantiate(ctx, span, tysc)
                               in (ty, U.InstantiatedVarExp(span, longvid, idstatus, tyargs))
                               end
         | NONE => emitError(ctx, [span], "unknown value name " ^ name)
      )
  | typeCheckExp(ctx, env, exp as U.InstantiatedVarExp(span, longvid as Syntax.MkQualified(_, USyntax.MkVId(name, _)), idstatus, tyargs)) (* should not reach here *)
    = let val ty = case lookupLongVIdInEnv(ctx, env, span, longvid) of
                       SOME (U.TypeScheme(vars, ty), ids) =>
                       let val subst = ListPair.foldlEq (fn ((var, constraints), (tyarg, _), set) =>
                                                            ( List.app (fn c => addConstraint(ctx, env, U.UnaryConstraint(span, tyarg, c))) constraints
                                                            ; USyntax.TyVarMap.insert(set, var, tyarg)
                                                            )
                                                        ) USyntax.TyVarMap.empty (vars, tyargs)
                       in applySubstTy subst ty
                       end
                     | NONE => emitError(ctx, [span], "unknown value name " ^ name)
      in (ty, exp)
      end
  | typeCheckExp(ctx, env, U.RecordExp(span, row))
    = let val (rowTy, row') = typeCheckExpRow(ctx, env, row)
      in (U.RecordType(span, rowTy), U.RecordExp(span, row'))
      end
  | typeCheckExp(ctx, env, U.LetInExp(span, decls, innerExp))
    = let val (env', decls') = typeCheckDecs(ctx, env, decls)
          val (ty, innerExp') = typeCheckExp(ctx, mergeEnv (env, env'), innerExp)
      in (ty, U.LetInExp(span, decls', innerExp'))
      end
  | typeCheckExp(ctx, env, U.AppExp(span, f, x))
          (* f: s -> t, x: s *)
    = let val (funcTy, f') = typeCheckExp(ctx, env, f)
          val (argTy, x') = typeCheckExp(ctx, env, x)
          val retTy = case funcTy of
                          U.FnType(_, argTy', retTy) => ( addConstraint(ctx, env, U.EqConstr(span, argTy, argTy')); retTy )
                        | _ => let val retTy = U.TyVar(span, freshTyVar(ctx))
                               in addConstraint(ctx, env, U.EqConstr(span, funcTy, U.FnType(span, argTy, retTy))) (* funcTy = (argTy -> retTy) *)
                                ; retTy
                               end
      in (retTy, U.AppExp(span, f', x'))
      end
  | typeCheckExp(ctx, env, U.TypedExp(span, exp, ty))
    = let val (expTy, exp') = typeCheckExp(ctx, env, exp)
      in addConstraint(ctx, env, U.EqConstr(span, expTy, ty)) (* ety = ty *)
       ; (ty, U.TypedExp(span, exp', ty))
      end
  | typeCheckExp(ctx, env, U.HandleExp(span, exp, matches))
          (* exp: t, matches: exn -> t *)
    = let val (expTy, exp') = typeCheckExp(ctx, env, exp)
          val (patTy, retTy, matches') = typeCheckMatch(ctx, env, span, matches)
      in addConstraint(ctx, env, U.EqConstr(span, patTy, primTy_exn)) (* patTy = exn *)
       ; addConstraint(ctx, env, U.EqConstr(span, expTy, retTy))
       ; (expTy, U.HandleExp(span, exp', matches'))
      end
  | typeCheckExp(ctx, env, U.RaiseExp(span, exp))
    = let val (expTy, exp') = typeCheckExp(ctx, env, exp)
      in addConstraint(ctx, env, U.EqConstr(span, expTy, primTy_exn)) (* expTy = exn *)
       ; (U.TyVar(span, freshTyVar(ctx)), U.RaiseExp(span, exp'))
      end
  | typeCheckExp(ctx, env, U.IfThenElseExp(span, cond, thenPart, elsePart))
    = let val (condTy, cond') = typeCheckExp(ctx, env, cond)
          val (thenTy, thenPart') = typeCheckExp(ctx, env, thenPart)
          val (elseTy, elsePart') = typeCheckExp(ctx, env, elsePart)
      in addConstraint(ctx, env, U.EqConstr(span, condTy, primTy_bool)) (* condTy = bool *)
       ; addConstraint(ctx, env, U.EqConstr(span, thenTy, elseTy)) (* thenTy = elseTy *)
       ; (thenTy, U.IfThenElseExp(span, cond', thenPart', elsePart'))
      end
  | typeCheckExp(ctx, env, U.CaseExp(span, exp, ty, matches))
    = let val (expTy, exp') = typeCheckExp(ctx, env, exp)
          val (patTy, retTy, matches') = typeCheckMatch(ctx, env, span, matches)
      in addConstraint(ctx, env, U.EqConstr(span, expTy, ty))
       ; addConstraint(ctx, env, U.EqConstr(span, expTy, patTy))
       ; (retTy, U.CaseExp(span, exp', ty, matches'))
      end
  | typeCheckExp(ctx, env, U.FnExp(span, vid, argTy, body))
    = let val env' = mergeEnv(env, { tyMap = USyntax.TyConMap.empty
                                   , valMap = USyntax.VIdMap.insert(USyntax.VIdMap.empty, vid, (U.TypeScheme([], argTy), Syntax.ValueVariable))
                                   , strMap = Syntax.StrIdMap.empty
                                   , boundTyVars = USyntax.TyVarSet.empty
                                   }
                             )
          val (retTy, body') = typeCheckExp(ctx, env', body)
      in (U.FnType(span, argTy, retTy), U.FnExp(span, vid, argTy, body'))
      end
  | typeCheckExp(ctx, env, exp as U.ProjectionExp { sourceSpan, label, recordTy, fieldTy })
    = ( addConstraint(ctx, env, U.UnaryConstraint(sourceSpan, recordTy, U.HasField { sourceSpan = sourceSpan, label = label, fieldTy = fieldTy }))
      ; (U.FnType(sourceSpan, recordTy, fieldTy), exp)
      )
  | typeCheckExp(ctx, env, U.ListExp(span, xs, ty))
    = let val xs' = Vector.map (fn exp => let val (expTy, exp') = typeCheckExp(ctx, env, exp)
                                          in addConstraint(ctx, env, U.EqConstr(span, expTy, ty))
                                           ; exp'
                                          end) xs
      in (U.TyCon(span, [ty], primTyCon_list), U.ListExp(span, xs', ty))
      end
(* typeCheckDec : Context * Env * Dec -> (* created environment *) Env * Dec *)
and typeCheckDec(ctx, env, U.ValDec(span, tyvarseq, valbinds))
    = let val valbinds' = let val innerEnv = { valMap = #valMap env, tyMap = #tyMap env, strMap = #strMap env, boundTyVars = USyntax.TyVarSet.addList (#boundTyVars env, tyvarseq) }
                          in List.map (fn valbind => typeCheckValBind(ctx, innerEnv, valbind)) valbinds
                          end
          val tvc = !(#tyVarConstraints ctx)
          val subst = !(#tyVarSubst ctx)
          val env' = applySubstEnv subst env
          val tyVars_env = freeTyVarsInEnv(U.TyVarSet.empty, env')
          fun generalize((valbind as U.PatBind(span, pat, exp), expTy, valEnv, (* generalizable *) false), (valbinds, valEnvRest))
              = let val vars = USyntax.VIdMap.listItemsi valEnv
                in case vars of
                       [(vid, ty)] => let val valbind' = U.PolyVarBind(span, vid, U.TypeScheme([], ty), case pat of
                                                                                                        U.VarPat _ => exp
                                                                                                      | U.TypedPat (_, U.VarPat _, _) => exp
                                                                                                      | _ => let val espan = U.getSourceSpanOfExp exp
                                                                                                                 val vid' = renewVId ctx vid
                                                                                                                 val pat' = U.renameVarsInPat (U.VIdMap.insert(U.VIdMap.empty, vid, vid')) pat
                                                                                                             in U.CaseExp(espan, exp, ty, if isExhaustive(ctx, env, pat) then
                                                                                                                                              [(pat', U.VarExp(espan, U.MkLongVId([], vid'), Syntax.ValueVariable))]
                                                                                                                                          else
                                                                                                                                              [(pat', U.VarExp(espan, U.MkLongVId([], vid'), Syntax.ValueVariable))
                                                                                                                                              ,(U.WildcardPat span, U.RaiseExp(span, U.VarExp(span, U.MkLongVId([], VId_Bind), Syntax.ExceptionConstructor)))
                                                                                                                                              ]
                                                                                                                         )
                                                                                                             end
                                                                      )
                                      in (valbind' :: valbinds, U.VIdMap.unionWith #2 (U.VIdMap.map (fn ty => U.TypeScheme([], ty)) valEnv, valEnvRest))
                                      end
                     | _ => let val espan = U.getSourceSpanOfExp exp
                                val vars' = List.map (fn (vid, _) => (vid, renewVId ctx vid)) vars
                                val varsMap = List.foldl U.VIdMap.insert' U.VIdMap.empty vars'
                                val pat' = U.renameVarsInPat varsMap pat
                                val tup = U.TupleExp(espan, List.map (fn (_, vid') => U.VarExp(espan, U.MkLongVId([], vid'), Syntax.ValueVariable)) vars') (* TODO: fresh vid *)
                                val valbind' = U.TupleBind(span, vars, U.CaseExp(espan, exp, expTy, if isExhaustive(ctx, env, pat) then
                                                                                                        [(pat', tup)]
                                                                                                    else
                                                                                                        [(pat', tup)
                                                                                                        ,(U.WildcardPat span, U.RaiseExp(span, U.VarExp(span, U.MkLongVId([], VId_Bind), Syntax.ExceptionConstructor)))
                                                                                                        ]
                                                                                )
                                                          )
                            in (valbind' :: valbinds, U.VIdMap.unionWith #2 (U.VIdMap.map (fn ty => U.TypeScheme([], ty)) valEnv, valEnvRest))
                            end
                end
            | generalize((U.PatBind(span, pat, exp), expTy, valEnv, (* generalizable *) true), (valbinds, valEnvRest))
              = let fun doVal (vid,ty)
                        = let val ty' = applySubstTy subst ty
                              val tyVars_ty = U.freeTyVarsInTy(U.TyVarSet.empty, ty')
                              fun isEqualityType (USyntax.IsEqType _) = true
                                | isEqualityType _ = false
                              fun isGeneralizable(tv: U.TyVar) = case USyntax.TyVarMap.find(tvc, tv) of
                                                                     NONE => true
                                                                   | SOME tvs => List.all isEqualityType tvs
                              val tyVars = U.TyVarSet.difference(U.TyVarSet.filter isGeneralizable tyVars_ty, tyVars_env) (* TODO: Allow equality constraint *)
                              fun doTyVar (tv as USyntax.NamedTyVar (_, true, _)) = (tv, [U.IsEqType span])
                                | doTyVar tv = case USyntax.TyVarMap.find(tvc, tv) of
                                                   NONE => (tv, [])
                                                 | SOME tvs => if List.exists isEqualityType tvs then
                                                                   (tv, [U.IsEqType span])
                                                               else (* should not reach here *)
                                                                   (tv, [])
                              val tysc = U.TypeScheme(List.map doTyVar (U.TyVarSet.listItems tyVars), ty')
                          in tysc
                          end
                    val valEnv' = USyntax.VIdMap.mapi doVal valEnv
                    val valEnv'L = USyntax.VIdMap.listItemsi valEnv'
                    val allPoly = List.all (fn (_, U.TypeScheme(tv, _)) => not (List.null tv)) valEnv'L (* all bindings are generalized? *)
                    val espan = USyntax.getSourceSpanOfExp exp
                    fun polyPart [] = []
                      | polyPart ((vid, U.TypeScheme([], _)) :: rest) = polyPart rest
                      | polyPart ((vid, tysc) :: rest) = let val vid' = renewVId ctx vid
                                                             val pat' = USyntax.renameVarsInPat (USyntax.VIdMap.insert(USyntax.VIdMap.empty, vid, vid')) pat
                                                         in U.PolyVarBind(span, vid, tysc, USyntax.CaseExp(espan, exp, expTy, [(USyntax.filterVarsInPat (fn x => x = vid') pat', USyntax.VarExp(espan, USyntax.MkLongVId([], vid'), Syntax.ValueVariable))])) :: polyPart rest
                                                         end
                    fun isMonoVar vid = case USyntax.VIdMap.find(valEnv', vid) of
                                            NONE => emitError(ctx, [span], "isMonoVar: internal error")
                                          | SOME (U.TypeScheme([], _)) => true
                                          | SOME (U.TypeScheme(_ :: _, _)) => false
                    val valbind' = if allPoly then
                                       polyPart valEnv'L
                                   else
                                       let val xs = List.mapPartial (fn (vid, tysc) => case tysc of
                                                                                           U.TypeScheme([], ty) => SOME (vid, ty)
                                                                                         | U.TypeScheme(_ :: _, _) => NONE
                                                                    ) valEnv'L
                                       in case xs of
                                              [(vid, ty)] => let val vid' = renewVId ctx vid
                                                                 val pat' = USyntax.renameVarsInPat (USyntax.VIdMap.insert(USyntax.VIdMap.empty, vid, vid')) (USyntax.filterVarsInPat isMonoVar pat)
                                                             in U.PolyVarBind(span, vid, U.TypeScheme([], ty), U.CaseExp(espan, exp, expTy, [(pat', U.VarExp(espan, USyntax.MkLongVId([], vid'), Syntax.ValueVariable))])) :: polyPart valEnv'L
                                                             end
                                            | _ => let val vars' = List.map (fn (vid, _) => (vid, renewVId ctx vid)) xs
                                                       val varsMap = List.foldl USyntax.VIdMap.insert' USyntax.VIdMap.empty vars'
                                                       val pat' = USyntax.renameVarsInPat varsMap (USyntax.filterVarsInPat isMonoVar pat)
                                                       val tup = U.TupleExp(espan, List.map (fn (_, vid') => U.VarExp(espan, USyntax.MkLongVId([], vid'), Syntax.ValueVariable)) vars')
                                                   in U.TupleBind(span, xs, U.CaseExp(espan, exp, expTy, [(pat', tup)])) :: polyPart valEnv'L
                                                   end
                                       end
                in (valbind' @ valbinds, USyntax.VIdMap.unionWith #2 (valEnv', valEnvRest))
                end
          val (valbinds'', valEnv'') = List.foldr generalize ([], USyntax.VIdMap.empty) valbinds'
          val env' = { valMap = USyntax.VIdMap.map (fn tysc => (tysc, Syntax.ValueVariable)) valEnv''
                     , tyMap = #tyMap emptyEnv
                     , strMap = #strMap emptyEnv
                     , boundTyVars = #boundTyVars emptyEnv
                     }
      in (env', U.ValDec'(span, valbinds''))
      end
  | typeCheckDec(ctx, env, U.RecValDec(span, tyvarseq, valbinds : U.ValBind list))
    = let val valbinds' : (SourcePos.span * (USyntax.Ty * USyntax.Ty USyntax.VIdMap.map * USyntax.Pat) * USyntax.Exp) list
              = List.map (fn U.PatBind (span, pat, exp) => (span, typeCheckPat(ctx, env, pat), exp)) valbinds
          val localValEnv = List.foldl (fn ((_, (_, ve, _), _), acc) => USyntax.VIdMap.unionWith #1 (acc, ve)) USyntax.VIdMap.empty valbinds'
          val localValMap = USyntax.VIdMap.map (fn ty => (U.TypeScheme ([], ty), Syntax.ValueVariable)) localValEnv
          val { valMap, tyMap, strMap, boundTyVars } = env
          val localEnv = { valMap = USyntax.VIdMap.unionWith #2 (valMap, localValMap), tyMap = tyMap, strMap = strMap, boundTyVars = USyntax.TyVarSet.addList (boundTyVars, tyvarseq) }
          val valbinds'' = List.map (fn (span, (patTy, newValEnv, pat), exp) =>
                                        let val (expTy, exp') = typeCheckExp(ctx, localEnv, exp)
                                            val () = addConstraint(ctx, env, U.EqConstr(span, patTy, expTy))
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
          val tyVars_env = freeTyVarsInEnv(U.TyVarSet.empty, applySubstEnv subst env)
          fun generalize ((span, (pat, exp), expTy, valEnv), (valbinds, valEnvRest))
              = let fun doVal (vid, ty)
                        = let val ty' = applySubstTy subst ty
                              val tyVars_ty = U.freeTyVarsInTy(U.TyVarSet.empty, ty')
                              fun isEqualityType (USyntax.IsEqType _) = true
                                | isEqualityType _ = false
                              fun isGeneralizable(tv: U.TyVar) = case USyntax.TyVarMap.find(tvc, tv) of
                                                                     NONE => true
                                                                   | SOME tvs => List.all isEqualityType tvs
                              val tyVars = U.TyVarSet.difference(U.TyVarSet.filter isGeneralizable tyVars_ty, tyVars_env)
                              fun doTyVar (tv as U.NamedTyVar (_, true, _)) = (tv, [U.IsEqType span])
                                | doTyVar tv = case U.TyVarMap.find(tvc, tv) of
                                                   NONE => (tv, [])
                                                 | SOME tvs => if List.exists isEqualityType tvs then
                                                                   (tv, [U.IsEqType span])
                                                               else (* should not reach here *)
                                                                   (tv, [])
                              val tysc = U.TypeScheme(List.map doTyVar (U.TyVarSet.listItems tyVars), ty')
                          in tysc
                          end
                    val valEnv' = USyntax.VIdMap.mapi doVal valEnv
                    val valbind' = USyntax.VIdMap.foldri (fn (vid, tysc, rest) => U.PolyVarBind(span, vid, tysc, exp) :: rest) [] valEnv'
                in (valbind' @ valbinds, USyntax.VIdMap.unionWith #2 (valEnv', valEnvRest))
                end
          val (valbinds'', valEnv'') = List.foldr generalize ([], USyntax.VIdMap.empty) valbinds''
          val env' = { valMap = USyntax.VIdMap.map (fn tysc => (tysc, Syntax.ValueVariable)) valEnv''
                     , tyMap = #tyMap emptyEnv
                     , strMap = #strMap emptyEnv
                     , boundTyVars = #boundTyVars emptyEnv
                     }
      in (env', U.RecValDec'(span, valbinds''))
      end
  | typeCheckDec(ctx, env, U.ValDec'(span, _)) = emitError (ctx, [span], "internal error: unexpected ValDec'")
  | typeCheckDec(ctx, env, U.RecValDec'(span, _)) = emitError (ctx, [span], "internal error: unexpected RecValDec'")
  | typeCheckDec(ctx, env, dec as U.TypeDec(span, typbinds))
    = (emptyEnv, dec)
  | typeCheckDec(ctx, env, U.DatatypeDec(span, datbinds))
    = let val equality = determineDatatypeEquality(ctx, env, datbinds)
          val env' = List.foldl (fn (datbind as U.DatBind(_, _, tycon, _, _), env) => addDatBind(ctx, env, datbind, USyntax.TyConMap.lookup(equality, tycon))) emptyEnv datbinds
      in (env', U.DatatypeDec(span, List.map (fn U.DatBind(span, tyvars, tycon, conbinds, _) => U.DatBind(span, tyvars, tycon, conbinds, USyntax.TyConMap.lookup(equality, tycon))) datbinds))
      end
  | typeCheckDec(ctx, env, dec as U.ExceptionDec(span, exbinds))
    = let fun doExBind(U.ExBind(span, vid, optTy), valMap)
              = U.VIdMap.insert(valMap, vid, (U.TypeScheme([], case optTy of NONE => primTy_exn
                                                                           | SOME ty => U.FnType(span, ty, primTy_exn)
                                                          ), Syntax.ExceptionConstructor))
          val valMap = List.foldl doExBind USyntax.VIdMap.empty exbinds
      in ({ tyMap = USyntax.TyConMap.empty, valMap = valMap, strMap = Syntax.StrIdMap.empty, boundTyVars = USyntax.TyVarSet.empty }, dec)
      end
(* typeCheckDecs : Context * Env * Dec list -> (* created environment *) Env * Dec list *)
and typeCheckDecs(ctx, env, []) : Env * U.Dec list = (emptyEnv, [])
  | typeCheckDecs(ctx, env, dec :: decs) = let val (env', dec') = typeCheckDec(ctx, env, dec)
                                               val (env'', decs') = typeCheckDecs(ctx, mergeEnv(env, env'), decs)
                                           in (mergeEnv(env', env''), dec' :: decs')
                                           end
(* typeCheckValBind : Context * Env * ValBind -> ValBind * USyntax.Ty * USyntax.Ty USyntax.VIdMap.map * bool *)
and typeCheckValBind(ctx, env, U.PatBind(span, pat, exp))
    = let val (patTy, newValEnv, pat') = typeCheckPat(ctx, env, pat)
          val (expTy, exp') = typeCheckExp(ctx, env, exp)
          val () = addConstraint(ctx, env, U.EqConstr(span, patTy, expTy))
          val generalizable = isExhaustive(ctx, env, pat) andalso isNonexpansive(env, exp)
      in (U.PatBind(span, pat', exp'), expTy, newValEnv, generalizable)
      end
and determineDatatypeEquality(ctx, env, datbinds) : bool USyntax.TyConMap.map
    = let val localTyCons = List.foldl (fn (U.DatBind (span, tyvars, tycon, conbinds, _), set) => U.TyConSet.add(set, tycon)) U.TyConSet.empty datbinds
          val graph : (U.TyConSet.set ref) U.TyConMap.map = U.TyConSet.foldl (fn (tycon, map) => U.TyConMap.insert(map, tycon, ref U.TyConSet.empty)) U.TyConMap.empty localTyCons
          val nonEqualitySet = ref U.TyConSet.empty
          fun doDatBind (U.DatBind (span, tyvars, tycon, conbinds, _))
              = let val r = U.TyConMap.lookup (graph, tycon)
                    fun doTy (U.TyVar (span, tv)) = if List.exists (fn tv' => U.eqUTyVar(tv, tv')) tyvars then
                                                        SOME []
                                                    else if U.TyVarSet.member(#boundTyVars env, tv) then
                                                        case tv of
                                                            U.NamedTyVar(_, eq, _) => if eq then
                                                                                          SOME []
                                                                                      else
                                                                                          NONE
                                                          | _ => emitError(ctx, [span], "unexpected anonymous type variable")
                                                    else
                                                        emitError(ctx, [span], "unbound type variable")
                      | doTy (U.RecordType (span, fields)) = doTypes (List.map #2 fields)
                      | doTy (U.TyCon (span, tyargs, tycon)) = if U.eqUTyCon(tycon, primTyCon_ref) orelse U.eqUTyCon(tycon, primTyCon_array) then
                                                                   SOME []
                                                               else if U.TyConSet.member(localTyCons, tycon) then
                                                                   SOME [tycon]
                                                               else
                                                                   (case lookupTyConInEnv(ctx, env, span, tycon) of
                                                                        { admitsEquality = e, ... } => if e then
                                                                                                           doTypes tyargs
                                                                                                       else
                                                                                                           NONE
                                                                   )
                      | doTy (U.FnType _) = NONE
                    and doTypes types = let fun go (acc, ty :: types) = (case doTy ty of
                                                                              NONE => NONE
                                                                            | SOME xs => go (xs @ acc, types)
                                                                         )
                                              | go (acc, []) = SOME acc
                                        in go ([], types)
                                        end
                    fun collectPayloads [] = []
                      | collectPayloads (U.ConBind (span, vid, NONE) :: conbinds) = collectPayloads conbinds
                      | collectPayloads (U.ConBind (span, vid, SOME ty) :: conbinds) = ty :: collectPayloads conbinds
                in case doTypes (collectPayloads conbinds) of
                       NONE => nonEqualitySet := U.TyConSet.add (!nonEqualitySet, tycon)
                     | SOME xs => List.app (fn member => let val r = U.TyConMap.lookup (graph, member)
                                                         in r := U.TyConSet.add (!r, tycon)
                                                         end) xs
                end
          fun dfs tycon = let val set = !(U.TyConMap.lookup (graph, tycon))
                          in U.TyConSet.app (fn t => let val s = !nonEqualitySet
                                                     in if U.TyConSet.member(s, t) then
                                                            ()
                                                        else
                                                            ( nonEqualitySet := U.TyConSet.add(s, t)
                                                            ; dfs t
                                                            )
                                                     end
                                            ) set
                          end
          val () = List.app doDatBind datbinds (* construct the graph *)
          val () = U.TyConSet.app dfs (!nonEqualitySet)
          val nonEqualitySet = !nonEqualitySet
      in U.TyConSet.foldl (fn (tycon, map) => U.TyConMap.insert(map, tycon, not (U.TyConSet.member(nonEqualitySet, tycon)))) U.TyConMap.empty localTyCons
      end
and addDatBind(ctx, { tyMap, valMap, strMap, boundTyVars }, U.DatBind(span, tyvars, tycon, conbinds, _), equality)
    = let val ty = U.TyCon(span, List.map (fn tv => U.TyVar(span, tv)) tyvars, tycon)
          fun doConBind(U.ConBind(span, vid, NONE), valMap)
              = let val typeScheme = U.TypeScheme(List.map (fn tv => (tv, [])) tyvars, ty)
                in U.VIdMap.insert(valMap, vid, (typeScheme, Syntax.ValueConstructor))
                end
            | doConBind(U.ConBind(span, vid, SOME payloadTy), valMap)
              = let val typeScheme = U.TypeScheme(List.map (fn tv => (tv, [])) tyvars, U.FnType(span, payloadTy, ty))
                in U.VIdMap.insert(valMap, vid, (typeScheme, Syntax.ValueConstructor))
                end
          val valEnv = List.foldl doConBind U.VIdMap.empty conbinds
          val tyStr = { typeFunction = U.TypeFunction(tyvars, ty)
                      , valEnv = valEnv
                      , admitsEquality = equality
                      }
      in { tyMap = U.TyConMap.insert(tyMap, tycon, tyStr)
         , valMap = U.VIdMap.unionWith (fn _ => emitError(ctx, [span], "internal error: duplicate identifier")) (valMap, valEnv)
         , strMap = strMap
         , boundTyVars = boundTyVars
         }
      end
(* typeCheckExpRow : Context * Env * (Label * Exp) list -> (Label * Syntax.Ty) list * (Label * Exp) list *)
and typeCheckExpRow(ctx, env, xs) : (Syntax.Label * U.Ty) list * (Syntax.Label * U.Exp) list
    = let fun oneField(label, exp) = case typeCheckExp(ctx, env, exp) of
                                         (ty, exp') => ((label, ty), (label, exp'))
      in ListPair.unzip (List.map oneField xs)
      end
 (* typeCheckMatch : Context * Env * SourcePos.span * (Pat * Exp) list -> (* pattern *) Syntax.Ty * (* expression *) Syntax.Ty * (Pat * Exp) list *)
and typeCheckMatch(ctx, env, span, (pat0, exp0) :: rest) : U.Ty * U.Ty * (U.Pat * U.Exp) list
    = let val (patTy, expTy, pat0', exp0') = typeCheckMatchBranch(ctx, env, pat0, exp0)
          fun oneBranch(pat, exp)
              = let val (patTy', expTy', pat', exp') = typeCheckMatchBranch(ctx, env, pat, exp)
                in addConstraint(ctx, env, U.EqConstr(span, patTy, patTy'))
                 ; addConstraint(ctx, env, U.EqConstr(span, expTy, expTy'))
                 ; (pat', exp')
                end
          val rest' = List.map oneBranch rest
      in (patTy, expTy, (pat0', exp0') :: rest')
      end
  | typeCheckMatch(ctx, env, span, nil) = emitError(ctx, [span], "invalid syntax tree: match is empty")
and typeCheckMatchBranch(ctx : Context, env : Env, pat : U.Pat, exp : U.Exp) : U.Ty * U.Ty * U.Pat * U.Exp
    = let val (patTy, vars, pat') = typeCheckPat(ctx, env, pat)
          val env' = { tyMap = #tyMap env
                     , valMap = USyntax.VIdMap.unionWith #2 (#valMap env, USyntax.VIdMap.map (fn ty => (U.TypeScheme([], ty), Syntax.ValueVariable)) vars)
                     , strMap = #strMap env
                     , boundTyVars = #boundTyVars env
                     }
          val (expTy, exp') = typeCheckExp(ctx, env', exp)
      in (patTy, expTy, pat', exp')
      end
 (* typeCheckPat : Context * Env * U.Pat -> U.Ty * U.Ty U.VIdMap.map * Pat *)
and typeCheckPat(ctx, env, pat as U.WildcardPat span) : U.Ty * U.Ty U.VIdMap.map * U.Pat
    = let val ty = U.TyVar(span, freshTyVar(ctx))
      in (ty, U.VIdMap.empty, pat)
      end
  | typeCheckPat(ctx, env, pat as U.SConPat(span, scon))
    = (case scon of
           Syntax.IntegerConstant(_)   => (primTy_int, USyntax.VIdMap.empty, pat)
         | Syntax.WordConstant(_)      => (primTy_word, USyntax.VIdMap.empty, pat)
         | Syntax.RealConstant(_)      => emitError(ctx, [span], "no real constant may occur in a pattern")
         | Syntax.StringConstant(_)    => (primTy_string, USyntax.VIdMap.empty, pat)
         | Syntax.CharacterConstant(_) => (primTy_char, USyntax.VIdMap.empty, pat)
      )
  | typeCheckPat(ctx, env, pat as U.VarPat(span, vid, ty))
    = (case USyntax.VIdMap.find(#valMap env, vid) of
           SOME (tysc, Syntax.ValueConstructor) => emitError(ctx, [span], "VarPat: invalid pattern")
         | SOME (tysc, Syntax.ExceptionConstructor) => emitError(ctx, [span], "VarPat: invalid pattern")
         | SOME (_, Syntax.ValueVariable) => (* shadowing *) (ty, USyntax.VIdMap.insert(USyntax.VIdMap.empty, vid, ty), pat)
         | NONE => (ty, USyntax.VIdMap.insert(USyntax.VIdMap.empty, vid, ty), pat)
      )
  | typeCheckPat(ctx, env, U.RecordPat{sourceSpan, fields, wildcard})
    = let val (rowTy, vars, row') = typeCheckPatRow(ctx, env, fields)
      in if wildcard then
             let val recordTy = U.TyVar(sourceSpan, freshTyVar(ctx))
                 fun oneField(label, ty) = addConstraint(ctx, env, U.UnaryConstraint(sourceSpan, recordTy, U.HasField { sourceSpan = sourceSpan, label = label, fieldTy = ty }))
             in List.app oneField rowTy
              ; (recordTy, vars, U.RecordPat{sourceSpan=sourceSpan, fields=row', wildcard=wildcard})
             end
         else
             (U.RecordType(sourceSpan, rowTy), vars, U.RecordPat{sourceSpan=sourceSpan, fields=row', wildcard=wildcard})
      end
  | typeCheckPat(ctx, env, U.ConPat(span, longvid, opt_innerPat))
    = (case lookupLongVIdInEnv(ctx, env, span, longvid) of
           SOME (tysc, idstatus) =>
           (if idstatus = Syntax.ValueConstructor orelse idstatus = Syntax.ExceptionConstructor then
                let val (ty, tyargs) = instantiate(ctx, span, tysc)
                in case opt_innerPat of
                       NONE => (ty, USyntax.VIdMap.empty, U.InstantiatedConPat(span, longvid, NONE, List.map #1 tyargs))
                     | SOME innerPat =>
                       (case ty of
                            U.FnType(span', argTy, resultTy) =>
                            let val (argTy', innerVars, innerPat') = typeCheckPat(ctx, env, innerPat)
                            in addConstraint(ctx, env, U.EqConstr(span, argTy, argTy'))
                             ; (resultTy, innerVars, U.InstantiatedConPat(span, longvid, SOME innerPat', List.map #1 tyargs))
                            end
                          | _ => emitError(ctx, [span], "invalid pattern")
                       )
                end
            else (* idstatus = Syntax.ValueVariable *)
                emitError(ctx, [span], "invalid pattern")
           )
         | NONE => emitError(ctx, [span], "invalid pattern")
      )
  | typeCheckPat(ctx, env, pat as U.InstantiatedConPat(span, longvid, opt_innerPat, tyargs)) (* should not reach here *)
    = (case lookupLongVIdInEnv(ctx, env, span, longvid) of
           SOME (U.TypeScheme(vars, ty), idstatus) =>
           (if idstatus = Syntax.ValueConstructor orelse idstatus = Syntax.ExceptionConstructor then
                let val subst = ListPair.foldlEq (fn ((var, constraints), tyarg, set) =>
                                                     ( List.app (fn c => addConstraint(ctx, env, U.UnaryConstraint(span, tyarg, c))) constraints
                                                     ; USyntax.TyVarMap.insert(set, var, tyarg)
                                                     )
                                                 ) USyntax.TyVarMap.empty (vars, tyargs)
                    val ty' = applySubstTy subst ty
                in case opt_innerPat of
                       NONE => (ty', USyntax.VIdMap.empty, pat)
                     | SOME innerPat => (case ty' of
                                             U.FnType(span', argTy, resultTy) => let val (argTy', innerVars, innerPat') = typeCheckPat(ctx, env, innerPat)
                                                                                 in addConstraint(ctx, env, U.EqConstr(span, argTy, argTy'))
                                                                                  ; (resultTy, innerVars, U.InstantiatedConPat(span, longvid, SOME innerPat', tyargs))
                                                                                 end
                                           | _ => emitError(ctx, [span], "invalid pattern")
                                        )
                end
            else (* idstatus = Syntax.ValueVariable *)
                emitError(ctx, [span], "invalid pattern")
           )
         | NONE => emitError(ctx, [span], "invalid pattern")
      )
  | typeCheckPat(ctx, env, pat as U.TypedPat(_, U.WildcardPat _, ty))
    = (ty, USyntax.VIdMap.empty, pat)
  | typeCheckPat(ctx, env, U.TypedPat(span, pat, ty))
    = let val (inferredTy, vars, pat') = typeCheckPat(ctx, env, pat)
      in addConstraint(ctx, env, U.EqConstr(span, ty, inferredTy))
       ; (ty, vars, U.TypedPat(span, pat', ty))
      end
  | typeCheckPat(ctx, env, U.LayeredPat(span, vid, ty, pat))
    = let val (inferredTy, vars, pat') = typeCheckPat(ctx, env, pat)
      in case USyntax.VIdMap.find(vars, vid) of
             NONE => ( addConstraint(ctx, env, U.EqConstr(span, ty, inferredTy))
                     ; (ty, USyntax.VIdMap.insert(vars, vid, ty), U.LayeredPat(span, vid, ty, pat'))
                     )
           | SOME _ => emitError(ctx, [span], "trying to bind the same identifier twice")
      end
 (* typeCheckPatRow : Context * Env * (Label * U.Pat) list -> (Label * U.Ty) list * U.Ty USyntax.VIdMap.map * (Label * U.Pat) list *)
and typeCheckPatRow(ctx, env, row)
    = let fun oneField((label, pat), (row, vars, rest))
              = let val (ty, vars', pat') = typeCheckPat(ctx, env, pat)
                in ((label, ty) :: row, USyntax.VIdMap.unionWith (fn _ => emitError(ctx, [], "trying to bind the same identifier twice")) (vars, vars'), (label, pat') :: rest)
                end
      in List.foldr oneField ([], USyntax.VIdMap.empty, []) row (* TODO: Is this right? *)
      end

(* pretty printing *)
structure PrettyPrint = struct
fun print_Env ({ tyMap, valMap, strMap, boundTyVars } : Env) = "Env{tyMap=" ^ USyntax.print_TyConMap (fn _ => "TypeStructure _") tyMap ^ ",valMap=" ^ USyntax.print_VIdMap (Syntax.print_pair (USyntax.print_TypeScheme, Syntax.print_IdStatus)) valMap ^ ",strMap=" ^ Syntax.print_StrIdMap (fn MkEnv env => print_Env env) strMap ^ ",boundTyVars=...}"
end (* structure PrettyPrint *)
open PrettyPrint

(* applyDefaultTypes : Context * (U.UnaryConstraint list) USyntax.TyVarMap.map * USyntax.Dec list -> USyntax.Dec list *)
fun applyDefaultTypes(ctx, tvc, decs) =
    let fun doInt [] = primTy_int
          | doInt (USyntax.HasField{...} :: xs) = emitError(ctx, [], "invalid record syntax for int")
          | doInt (USyntax.IsEqType _ :: xs) = doInt xs
          | doInt (USyntax.IsIntegral _ :: xs) = doInt xs
          | doInt (USyntax.IsSignedReal _ :: xs) = doInt xs
          | doInt (USyntax.IsRing _ :: xs) = doInt xs
          | doInt (USyntax.IsField _ :: xs) = emitError(ctx, [], "cannot apply / operator for int")
          | doInt (USyntax.IsSigned _ :: xs) = doInt xs
          | doInt (USyntax.IsOrdered _ :: xs) = doInt xs
        fun doReal [] = primTy_real
          | doReal (USyntax.HasField{...} :: xs) = emitError(ctx, [], "invalid record syntax for real")
          | doReal (USyntax.IsEqType _ :: xs) = emitError(ctx, [], "real does not admit equality")
          | doReal (USyntax.IsIntegral _ :: xs) = emitError(ctx, [], "div, mod is invalid for real")
          | doReal (USyntax.IsSignedReal _ :: xs) = doReal xs
          | doReal (USyntax.IsRing _ :: xs) = doReal xs
          | doReal (USyntax.IsField _ :: xs) = doReal xs
          | doReal (USyntax.IsSigned _ :: xs) = doReal xs
          | doReal (USyntax.IsOrdered _ :: xs) = doReal xs
        fun doIntOrReal [] = primTy_int
          | doIntOrReal (USyntax.HasField{...} :: _) = emitError(ctx, [], "unresolved flex record")
          | doIntOrReal (USyntax.IsEqType _ :: xs) = doInt xs
          | doIntOrReal (USyntax.IsIntegral _ :: xs) = doInt xs
          | doIntOrReal (USyntax.IsSignedReal _ :: xs) = doIntOrReal xs
          | doIntOrReal (USyntax.IsRing _ :: xs) = doIntOrReal xs
          | doIntOrReal (USyntax.IsField _ :: xs) = doReal xs
          | doIntOrReal (USyntax.IsSigned _ :: xs) = doIntOrReal xs
          | doIntOrReal (USyntax.IsOrdered _ :: xs) = doIntOrReal xs
        fun defaultTyForConstraints(eq, []) = primTy_unit
          | defaultTyForConstraints(eq, USyntax.HasField{...} :: _) = emitError(ctx, [], "unresolved flex record")
          | defaultTyForConstraints(eq, USyntax.IsEqType _ :: xs) = defaultTyForConstraints(true, xs)
          | defaultTyForConstraints(eq, USyntax.IsIntegral _ :: xs) = doInt xs
          | defaultTyForConstraints(eq, USyntax.IsSignedReal _ :: xs) = if eq then doInt xs else doIntOrReal xs
          | defaultTyForConstraints(eq, USyntax.IsRing _ :: xs) = if eq then doInt xs else doIntOrReal xs
          | defaultTyForConstraints(eq, USyntax.IsField _ :: xs) = if eq then emitError(ctx, [], "real does not admit equality") else doReal xs
          | defaultTyForConstraints(eq, USyntax.IsSigned _ :: xs) = if eq then doInt xs else doIntOrReal xs
          | defaultTyForConstraints(eq, USyntax.IsOrdered _ :: xs) = if eq then doInt xs else doIntOrReal xs
        fun doTyVar tv = case U.TyVarMap.find(tvc, tv) of
                             NONE => primTy_unit
                           | SOME constraints => defaultTyForConstraints(false, constraints)
        val freeTyVars = USyntax.freeTyVarsInDecs(USyntax.TyVarSet.empty, decs)
        val subst = USyntax.TyVarSet.foldl (fn (tv, map) => USyntax.TyVarMap.insert(map, tv, doTyVar tv)) USyntax.TyVarMap.empty freeTyVars
    in #doDecs (USyntax.mapTy (ctx, subst, false)) decs
    end

fun checkTyScope (ctx, tvset : U.TyVarSet.set, tyconset : U.TyConSet.set)
    = let fun goTy (U.TyVar(span, tv))
              = if U.TyVarSet.member(tvset, tv) then
                    ()
                else
                    emitErrorP(ctx, [span], "type variable scope violation " ^ USyntax.PrettyPrint.print_TyVar tv)
            | goTy (U.RecordType(span, fields)) = List.app (fn (label, ty) => goTy ty) fields
            | goTy (U.TyCon(span, tyargs, tycon))
              = if U.TyConSet.member(tyconset, tycon) then
                    List.app goTy tyargs
                else
                    emitErrorP(ctx, [span], "type constructor scope violation")
            | goTy (U.FnType(span, ty1, ty2)) = ( goTy ty1; goTy ty2 )
          fun goUnaryConstraint (U.HasField { sourceSpan, label, fieldTy }) = goTy fieldTy
            | goUnaryConstraint _ = ()
          fun goTypeScheme (U.TypeScheme (typarams, ty)) = ( List.app (fn (tv, cts) => List.app goUnaryConstraint cts) typarams
                                                           ; #goTy (checkTyScope (ctx, U.TyVarSet.addList (tvset, List.map #1 typarams), tyconset)) ty
                                                           )
          fun goPat (U.WildcardPat _) = ()
            | goPat (U.SConPat _) = ()
            | goPat (U.VarPat (_, _, ty)) = goTy ty
            | goPat (U.RecordPat { sourceSpan, fields, wildcard }) = List.app (fn (label, pat) => goPat pat) fields
            | goPat (U.ConPat(span, longvid, optPat)) = Option.app goPat optPat
            | goPat (U.InstantiatedConPat(span, longvid, optPat, tyargs)) = ( List.app goTy tyargs
                                                                            ; Option.app goPat optPat
                                                                            )
            | goPat (U.TypedPat(span, pat, ty)) = ( goTy ty; goPat pat )
            | goPat (U.LayeredPat(span, vid, ty, pat)) = ( goTy ty; goPat pat )
          fun goExp (U.SConExp (span, scon)) = ()
            | goExp (U.VarExp (span, longvid, ids)) = ()
            | goExp (U.InstantiatedVarExp (span, longvid, ids, tyargs)) = List.app (fn (ty, cts) => (goTy ty; List.app goUnaryConstraint cts)) tyargs
            | goExp (U.RecordExp (span, fields)) = List.app (fn (label, exp) => goExp exp) fields
            | goExp (U.LetInExp (span, decs, exp)) = let val tyconset = goDecs decs
                                                         val { goExp, ... } = checkTyScope (ctx, tvset, tyconset)
                                                     in goExp exp
                                                     end
            | goExp (U.AppExp (span, exp1, exp2)) = ( goExp exp1; goExp exp2 )
            | goExp (U.TypedExp (span, exp, ty)) = ( goExp exp; goTy ty )
            | goExp (U.HandleExp (span, exp, matches)) = ( goExp exp; List.app (fn (pat, exp) => (goPat pat; goExp exp)) matches )
            | goExp (U.RaiseExp (span, exp)) = goExp exp
            | goExp (U.IfThenElseExp (span, exp1, exp2, exp3)) = ( goExp exp1; goExp exp2; goExp exp3 )
            | goExp (U.CaseExp (span, exp, ty, matches)) = ( goExp exp; goTy ty; List.app (fn (pat, exp) => (goPat pat; goExp exp)) matches )
            | goExp (U.FnExp (span, vid, ty, exp)) = ( goTy ty; goExp exp )
            | goExp (U.ProjectionExp { sourceSpan, label, recordTy, fieldTy }) = ( goTy recordTy; goTy fieldTy )
            | goExp (U.ListExp (span, xs, ty)) = ( Vector.app goExp xs ; goTy ty )
          and goDec (U.ValDec (span, tyvars, valbinds)) = let val { goTypeScheme, goValBind, ... } = checkTyScope (ctx, U.TyVarSet.addList (tvset, tyvars), tyconset)
                                                          in List.app goValBind valbinds
                                                           ; tyconset
                                                          end
            | goDec (U.RecValDec (span, tyvars, valbinds)) = let val { goTypeScheme, goValBind, ... } = checkTyScope (ctx, U.TyVarSet.addList (tvset, tyvars), tyconset)
                                                             in List.app goValBind valbinds
                                                              ; tyconset
                                                             end
            | goDec (U.ValDec' (span, valbinds)) = ( List.app goValBind' valbinds
                                                   ; tyconset
                                                   )
            | goDec (U.RecValDec' (span, valbinds)) = ( List.app goValBind' valbinds
                                                      ; tyconset
                                                      )
            | goDec (U.TypeDec (span, typbinds)) = let fun goTypBind (U.TypBind (span, tyvars, tycon, ty), acc) = let val { goTy, ... } = checkTyScope (ctx, U.TyVarSet.addList (tvset, tyvars), tyconset)
                                                                                                                  in goTy ty
                                                                                                                   ; U.TyConSet.add (acc, tycon)
                                                                                                                  end
                                                   in List.foldl goTypBind tyconset typbinds
                                                   end
            | goDec (U.DatatypeDec (span, datbinds)) = let val tyconset = List.foldl (fn (U.DatBind (span, _, tycon, _, _), tyconset) => U.TyConSet.add (tyconset, tycon)) tyconset datbinds
                                                           fun goDatBind (U.DatBind (span, tyvars, tycon, conbinds, _))
                                                               = let val { goTy, ... } = checkTyScope (ctx, U.TyVarSet.addList (tvset, tyvars), tyconset)
                                                                     fun goConBind (U.ConBind (span, vid, optTy)) = Option.app goTy optTy
                                                                 in List.app goConBind conbinds
                                                                 end
                                                       in List.app goDatBind datbinds
                                                        ; tyconset
                                                       end
            | goDec (U.ExceptionDec (span, exbinds)) = ( List.app (fn U.ExBind (span, vid, optTy) => Option.app goTy optTy) exbinds
                                                       ; tyconset
                                                       )
          and goDecs decs = List.foldl (fn (dec, tyconset) => let val { goDec, ... } = checkTyScope (ctx, tvset, tyconset)
                                                              in goDec dec
                                                              end)
                                       tyconset decs
          and goValBind' (U.TupleBind (span, binds, exp)) = ( List.app (fn (vid, ty) => goTy ty) binds
                                                            ; goExp exp
                                                            )
            | goValBind' (U.PolyVarBind (span, vid, U.TypeScheme (typarams, ty), exp))
              = let val { goTy, goExp, ... } = checkTyScope (ctx, U.TyVarSet.addList (tvset, List.map #1 typarams), tyconset)
                in List.app (fn (tv, cts) => List.app goUnaryConstraint cts) typarams
                 ; goTy ty
                 ; goExp exp
                end
          fun goValBind (U.PatBind (span, pat, exp)) = ( goPat pat; goExp exp )
          fun goTopDec (U.StrDec dec) = goDec dec
                                              (* | goTopDec (U.SigDec _) =  *)
          fun goTopDecs decs = List.foldl (fn (dec, tyconset) => let val { goTopDec, ... } = checkTyScope (ctx, tvset, tyconset)
                                                                 in goTopDec dec
                                                                 end)
                                          tyconset decs
      in { goTy = goTy
         , goTypeScheme = goTypeScheme
         , goPat = goPat
         , goExp = goExp
         , goDec = goDec
         , goDecs = goDecs
         , goValBind = goValBind
         , goTopDec = goTopDec
         , goTopDecs = goTopDecs
         }
      end
fun checkTyScopeOfProgram (ctx, tyconset : U.TyConSet.set, program : U.Program)
    = List.foldl (fn (topdec, tyconset) => let val { goTopDecs, ... } = checkTyScope (ctx, U.TyVarSet.empty, tyconset)
                                           in goTopDecs topdec
                                           end)
                 tyconset program

(* typeCheckTopDec : ProgramContext * Env * USyntax.TopDec -> (* created environment *) Env * USyntax.TopDec *)
fun typeCheckTopDec(pctx : ProgramContext, env, topdecs) : Env * USyntax.TopDec list =
    let val ctx = { nextTyVar = #nextTyVar pctx
                  , nextVId = #nextVId pctx
                  , nextTyCon = #nextTyCon pctx
                  , tyVarConstraints = ref USyntax.TyVarMap.empty
                  , tyVarSubst = ref USyntax.TyVarMap.empty
                  }
        val (env', decs) = typeCheckDecs(ctx, env, List.map (fn USyntax.StrDec dec => dec) topdecs)
        val subst = !(#tyVarSubst ctx)
        val tvc = !(#tyVarConstraints ctx)
        val mapTyInDec = #doDec (USyntax.mapTy (ctx, subst, false))
        val decs = List.map mapTyInDec decs
        val decs = applyDefaultTypes(ctx, tvc, decs)
    in (env', List.map USyntax.StrDec decs)
    end
(* typeCheckProgram : ProgramContext * Env * (USyntax.TopDec list) list -> Env * USyntax.TopDec list *)
fun typeCheckProgram(ctx, env, [] : (USyntax.TopDec list) list) : Env * (USyntax.TopDec list) list = (emptyEnv, [])
  | typeCheckProgram(ctx, env, topdec :: topdecs) = let val (env', topdec') = typeCheckTopDec(ctx, env, topdec)
                                                        val (env'', topdecs') = typeCheckProgram(ctx, mergeEnv(env, env'), topdecs)
                                                    in (mergeEnv(env', env''), topdec' :: topdecs')
                                                    end
end (* local *)
end (* structure Typing *)
