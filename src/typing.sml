(*
 * Copyright (c) 2021 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure Typing = struct

type Env = { valMap : (USyntax.LongVId * USyntax.TypeScheme * Syntax.IdStatus) Syntax.VIdMap.map
           , tyConMap : USyntax.TypeStructure Syntax.TyConMap.map
           , allTyConMap : USyntax.TypeStructure USyntax.TyConMap.map
           , strMap : (USyntax.LongStrId * USyntax.Signature) Syntax.StrIdMap.map
           , sigMap : USyntax.QSignature Syntax.SigIdMap.map
           , boundTyVars : USyntax.TyVar Syntax.TyVarMap.map
           }

val emptyEnv : Env
    = { valMap = Syntax.VIdMap.empty
      , tyConMap = Syntax.TyConMap.empty
      , allTyConMap = USyntax.TyConMap.empty
      , strMap = Syntax.StrIdMap.empty
      , sigMap = Syntax.SigIdMap.empty
      , boundTyVars = Syntax.TyVarMap.empty
      }

(* mergeEnv : Env * Env -> Env *)
fun mergeEnv(env1 : Env, env2 : Env) : Env
    = { valMap = Syntax.VIdMap.unionWith #2 (#valMap env1, #valMap env2)
      , tyConMap = Syntax.TyConMap.unionWith #2 (#tyConMap env1, #tyConMap env2)
      , allTyConMap = USyntax.TyConMap.unionWith #2 (#allTyConMap env1, #allTyConMap env2)
      , strMap = Syntax.StrIdMap.unionWith #2 (#strMap env1, #strMap env2)
      , sigMap = Syntax.SigIdMap.unionWith #2 (#sigMap env1, #sigMap env2)
      , boundTyVars = Syntax.TyVarMap.unionWith #2 (#boundTyVars env1, #boundTyVars env2)
      }

fun envWithValEnv valMap : Env
    = { valMap = valMap
      , tyConMap = Syntax.TyConMap.empty
      , allTyConMap = USyntax.TyConMap.empty
      , strMap = Syntax.StrIdMap.empty
      , sigMap = Syntax.SigIdMap.empty
      , boundTyVars = Syntax.TyVarMap.empty
      }

fun envWithTyConEnv (tyConMap, allTyConMap) : Env
    = { valMap = Syntax.VIdMap.empty
      , tyConMap = tyConMap
      , allTyConMap = allTyConMap
      , strMap = Syntax.StrIdMap.empty
      , sigMap = Syntax.SigIdMap.empty
      , boundTyVars = Syntax.TyVarMap.empty
      }

type Subst = USyntax.Ty USyntax.TyVarMap.map

fun freeTyVarsInTypeScheme(bound, USyntax.TypeScheme(tyvars, ty)) = USyntax.freeTyVarsInTy(USyntax.TyVarSet.addList(bound, List.map #1 tyvars), ty)
fun freeTyVarsInSignature(bound, { valMap, tyConMap, strMap } : USyntax.Signature)
    = let val valMapSet = Syntax.VIdMap.foldl (fn ((tysc, _), set) => USyntax.TyVarSet.union(set, freeTyVarsInTypeScheme(bound, tysc))) USyntax.TyVarSet.empty valMap
      in Syntax.StrIdMap.foldl (fn (USyntax.MkSignature s, set) => USyntax.TyVarSet.union(set, freeTyVarsInSignature(bound, s))) valMapSet strMap
      end
fun freeTyVarsInEnv(bound, { valMap, tyConMap, allTyConMap, strMap, sigMap, boundTyVars } : Env)
    = let val boundTyVars = Syntax.TyVarMap.foldl (fn (tv, set) => USyntax.TyVarSet.add(set, tv)) USyntax.TyVarSet.empty boundTyVars
          val valMapSet = Syntax.VIdMap.foldl (fn ((_, tysc, _), set) => USyntax.TyVarSet.union(set, freeTyVarsInTypeScheme(bound, tysc))) boundTyVars valMap
          (* TODO: tyConMap? *)
      in Syntax.StrIdMap.foldl (fn ((_, s), set) => USyntax.TyVarSet.union(set, freeTyVarsInSignature(bound, s))) valMapSet strMap
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
                      }

type Context = { nextTyVar : int ref
               , nextVId : int ref
               , tyVarConstraints : ((USyntax.UnaryConstraint list) USyntax.TyVarMap.map) ref
               , tyVarSubst : Subst ref
               }

exception TypeError of SourcePos.span list * string

fun emitErrorP(ctx : ProgramContext, spans, message) = raise TypeError (spans, message)
fun emitError(ctx : Context, spans, message) = raise TypeError (spans, message)

(* lookupStr : Context * USyntax..Signature * SourcePos.span * Syntax.StrId list -> USyntax.Signature *)
fun lookupStr(ctx, s : USyntax.Signature, span, nil) = s
  | lookupStr(ctx, s as { strMap = strMap, ... }, span, (strid0 as Syntax.MkStrId name) :: strids)
    = (case Syntax.StrIdMap.find(strMap, strid0) of
           NONE => emitError(ctx, [span], "unknown structure name '" ^ name ^ "'")
         | SOME (USyntax.MkSignature innerEnv) => lookupStr(ctx, innerEnv, span, strids)
      )
(* lookupTyConInEnv : Context * Env * SourcePos.span * Syntax.LongTyCon -> U.TypeStructure *)
fun lookupTyConInEnv(ctx, env : Env, span, Syntax.MkQualified([], tycon as Syntax.MkTyCon name))
    = (case Syntax.TyConMap.find(#tyConMap env, tycon) of
           SOME tystr => tystr
         | NONE => emitError(ctx, [span], "unknown type constructor '" ^ name ^ "'")
      )
  | lookupTyConInEnv(ctx, env, span, Syntax.MkQualified(strid0 :: strids, tycon as Syntax.MkTyCon name))
    = (case Syntax.StrIdMap.find(#strMap env, strid0) of
           SOME (_, s) => (case lookupStr(ctx, s, span, strids) of
                               { tyConMap, ... } => case Syntax.TyConMap.find(tyConMap, tycon) of
                                                        SOME tystr => tystr
                                                      | NONE => emitError(ctx, [span], "unknown type constructor '" ^ name ^ "'")
                          )
         | NONE => emitError(ctx, [span], "unknown structure name '" ^ (case strid0 of Syntax.MkStrId name => name) ^ "'")
      )
fun lookupUTyConInEnv(ctx, { allTyConMap, ... } : Env, span, tycon)
    = (case USyntax.TyConMap.find(allTyConMap, tycon) of
           SOME tystr => tystr
         | NONE => emitError(ctx, [span], "unknown type constructor " ^ USyntax.print_TyCon tycon ^ " (internal error)")
      )
(* Context * Env * SourcePos.span * Syntax.LongVId -> (USyntax.LongVId, USyntax.TypeScheme * Syntax.IdStatus) option *)
fun lookupLongVIdInEnv(ctx, env : Env, span, Syntax.MkQualified([], vid))
    = (case Syntax.VIdMap.find(#valMap env, vid) of
           SOME (longvid, tysc, ids) => SOME (longvid, tysc, ids)
         | NONE => NONE
      )
  | lookupLongVIdInEnv(ctx, env, span, Syntax.MkQualified(strid0 :: strids, vid))
    = (case Syntax.StrIdMap.find(#strMap env, strid0) of
           SOME (USyntax.MkLongStrId(strid0, strids0), s) =>
           (case lookupStr(ctx, s, span, strids) of
                { valMap, ... } => case Syntax.VIdMap.find(valMap, vid) of
                                       SOME (tysc, ids) => SOME (USyntax.MkLongVId(strid0, strids0 @ strids, vid), tysc, ids)
                                     | NONE => NONE
           )
         | NONE => NONE
      )

(* getConstructedType : Context * SourcePos.span * USyntax.Ty -> USyntax.TyCon *)
fun getConstructedType(ctx, span, USyntax.TyVar _) = emitError(ctx, [span], "getConstructedType: got a type variable")
  | getConstructedType(ctx, span, USyntax.RecordType _) = emitError(ctx, [span], "getConstructedType: got a record")
  | getConstructedType(ctx, span, USyntax.TyCon(_, tyargs, tycon)) = tycon
  | getConstructedType(ctx, span, USyntax.FnType(_, _, t)) = getConstructedType(ctx, span, t)

(*
(* isSoleConstructor : Context * Env * SourcePos.span * USyntax.LongVId -> bool *)
fun isSoleConstructor(ctx : Context, env : Env, span : SourcePos.span, longvid: USyntax.LongVId) =
    (case lookupLongVIdInEnv(ctx, env, span, longvid) of
         NONE => false (* probably an error *)
       | SOME (_, USyntax.TypeScheme(_, ty), Syntax.ValueConstructor) =>
         let val tycon = getConstructedType(ctx, span, ty)
             val { valEnv, ... } = lookupTyConInEnv(ctx, env, span, tycon)
         in USyntax.VIdMap.numItems valEnv = 1
         end
       | SOME (_, _, Syntax.ValueVariable) => false
       | SOME (_, _, Syntax.ExceptionConstructor) => false
    )
*)

(* The Definition, 4.7 Non-expansive Expressions *)
(* isNonexpansive : Env * USyntax.Exp -> bool *)
fun isNonexpansive(env : Env, USyntax.SConExp _) = true
  | isNonexpansive(env, USyntax.VarExp _) = true (* <op> longvid *)
  | isNonexpansive(env, USyntax.RecordExp(_, fields)) = List.all (fn (_, e) => isNonexpansive(env, e)) fields
  | isNonexpansive(env, USyntax.TypedExp(_, e, _)) = isNonexpansive(env, e)
  | isNonexpansive(env, USyntax.AppExp(_, conexp, e)) = isConexp(env, conexp) andalso isNonexpansive(env, e)
  | isNonexpansive(env, USyntax.FnExp _) = true
  | isNonexpansive(env, USyntax.ProjectionExp _) = true
  | isNonexpansive(env, USyntax.ListExp(_, xs, _)) = Vector.all (fn x => isNonexpansive(env, x)) xs
  | isNonexpansive(env, _) = false
and isConexp(env : Env, USyntax.TypedExp(_, e, _)) = isConexp(env, e)
  | isConexp(env, USyntax.VarExp(_, _, Syntax.ValueVariable, _)) = false
  | isConexp(env, USyntax.VarExp(_, USyntax.MkShortVId(USyntax.MkVId(name, _)), Syntax.ValueConstructor, _)) = name <> "ref"
  | isConexp(env, USyntax.VarExp(_, USyntax.MkLongVId(_, _, Syntax.MkVId(name)), Syntax.ValueConstructor, _)) = name <> "ref"
  | isConexp(env, USyntax.VarExp(_, _, Syntax.ExceptionConstructor, _)) = true
  | isConexp(env, _) = false

(* isExhaustive : Context * Env * USyntax.Pat -> bool *)
fun isExhaustive(ctx, env : Env, USyntax.WildcardPat _) = true
  | isExhaustive(ctx, env, USyntax.SConPat _) = false
  | isExhaustive(ctx, env, USyntax.VarPat _) = true
  | isExhaustive(ctx, env, USyntax.RecordPat{fields, ...}) = List.all (fn (_, e) => isExhaustive(ctx, env, e)) fields
  | isExhaustive(ctx, env, USyntax.ConPat{ sourceSpan, longvid, payload = NONE, tyargs, isSoleConstructor }) = isSoleConstructor
  | isExhaustive(ctx, env, USyntax.ConPat{ sourceSpan, longvid, payload = SOME innerPat, tyargs, isSoleConstructor }) = isSoleConstructor andalso isExhaustive(ctx, env, innerPat)
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
val primTyCon_exntag = USyntax.MkTyCon("exntag", 11)
(* primTyCon_Lua_value : 12 *)
val primTy_unit   = USyntax.RecordType(SourcePos.nullSpan, [])
val primTy_int    = USyntax.TyCon(SourcePos.nullSpan, [], primTyCon_int)
val primTy_word   = USyntax.TyCon(SourcePos.nullSpan, [], primTyCon_word)
val primTy_real   = USyntax.TyCon(SourcePos.nullSpan, [], primTyCon_real)
val primTy_string = USyntax.TyCon(SourcePos.nullSpan, [], primTyCon_string)
val primTy_char   = USyntax.TyCon(SourcePos.nullSpan, [], primTyCon_char)
val primTy_exn    = USyntax.TyCon(SourcePos.nullSpan, [], primTyCon_exn)
val primTy_bool   = USyntax.TyCon(SourcePos.nullSpan, [], primTyCon_bool)
val VId_Bind = USyntax.MkVId("Bind", ~1)
val LongVId_Bind = USyntax.MkShortVId(VId_Bind)

fun newContext() : ProgramContext
    = { nextTyVar = ref 100
      , nextVId = ref 100
      }

fun addTyVarConstraint(ctx : Context, tv : USyntax.TyVar, ct : USyntax.UnaryConstraint)
    = let val cts = !(#tyVarConstraints ctx)
          val xs = Option.getOpt(USyntax.TyVarMap.find(cts, tv), [])
      in #tyVarConstraints ctx := USyntax.TyVarMap.insert(cts, tv, ct :: xs)
      end

fun renewVId (ctx : Context) (USyntax.MkVId(name, _)) : USyntax.VId
    = let val n = !(#nextVId ctx)
      in #nextVId ctx := n + 1
       ; USyntax.MkVId(name, n)
      end

fun genTyVarId(ctx : Context)
    = let val id = !(#nextTyVar ctx)
      in #nextTyVar ctx := id + 1 ; id end
fun genTyVar(ctx, Syntax.MkTyVar tvname) = if String.isPrefix "''" tvname then
                                               USyntax.NamedTyVar(tvname, true, genTyVarId(ctx))
                                           else
                                               USyntax.NamedTyVar(tvname, false, genTyVarId(ctx))
fun freshTyVar(ctx : Context) = USyntax.AnonymousTyVar(genTyVarId(ctx))
                                                      
fun newVId(ctx : Context, Syntax.MkVId name) = let val n = !(#nextVId ctx)
                                               in #nextVId ctx := n + 1
                                                ; USyntax.MkVId(name, n)
                                               end
  | newVId(ctx, Syntax.GeneratedVId(name, _)) = let val n = !(#nextVId ctx)
                                                in #nextVId ctx := n + 1
                                                 ; USyntax.MkVId(name, n)
                                                end

fun newStrId(ctx : Context, Syntax.MkStrId name) = let val n = !(#nextVId ctx)
                                                   in #nextVId ctx := n + 1
                                                    ; USyntax.MkStrId(name, n)
                                                   end

fun genTyConId(ctx : Context)
    = let val id = !(#nextTyVar ctx)
      in #nextTyVar ctx := id + 1 ; id end
fun newTyCon(ctx, Syntax.MkTyCon name) = USyntax.MkTyCon(name, genTyConId(ctx))

local
    structure S = Syntax
    structure U = USyntax
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
        fun substSignature ({ valMap, tyConMap, strMap } : U.Signature) : U.Signature
            = { valMap = S.VIdMap.map (fn (tysc, ids) => (substTypeScheme(tysc), ids)) valMap
              , tyConMap = tyConMap (* ??? *)
              , strMap = Syntax.StrIdMap.map (fn U.MkSignature s => U.MkSignature (substSignature s)) strMap
              }
        fun substEnv ({ valMap, tyConMap, allTyConMap, strMap, sigMap, boundTyVars } : Env) : Env
            = { valMap = S.VIdMap.map (fn (longvid, tysc, ids) => (longvid, substTypeScheme(tysc), ids)) valMap
              , tyConMap = tyConMap (* ??? *)
              , allTyConMap = allTyConMap
              , strMap = Syntax.StrIdMap.map (fn (strid, s) => (strid, substSignature s)) strMap
              , sigMap = sigMap
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
           let val { admitsEquality, isRefOrArray, ... } = lookupUTyConInEnv(ctx, env, span2, tycon)
           in if isRefOrArray then
                  unify(ctx, env, ctrs)
              else if admitsEquality then
                  unify(ctx, env, List.map (fn tyarg => U.UnaryConstraint(span1, tyarg, U.IsEqType span3)) tyargs @ ctrs)
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

(* evalTy : Context * Env * S.Ty -> U.Ty *)
fun evalTy(ctx : Context, env : Env, S.TyVar(span, tv)) : U.Ty
    = (case Syntax.TyVarMap.find(#boundTyVars env, tv) of
           SOME tv => U.TyVar(span, tv)
         | NONE => emitError(ctx, [span], "unknown type varibale `" ^ Syntax.print_TyVar tv ^ "`")
      )
  | evalTy(ctx, env, S.RecordType(span, fields)) = U.RecordType(span, List.map (fn (label, ty) => (label, evalTy(ctx, env, ty))) fields)
  | evalTy(ctx, env, S.TyCon(span, args, tycon))
    = let val { typeFunction = U.TypeFunction(tyvars, ty), ... } = lookupTyConInEnv(ctx, env, span, tycon)
          val subst = (ListPair.foldlEq (fn (tv, arg, m) => USyntax.TyVarMap.insert (m, tv, evalTy(ctx, env, arg))) USyntax.TyVarMap.empty (tyvars, args))
                      handle ListPair.UnequalLengths => emitError(ctx, [span], "invalid type construction")
      in U.applySubstTy subst ty
      end
  | evalTy(ctx, env, S.FnType(span, ty1, ty2)) = U.FnType(span, evalTy(ctx, env, ty1), evalTy(ctx, env, ty2))

(* typeCheckPat : Context * Env * S.Pat -> U.Ty * (U.VId * U.Ty) S.VIdMap.map * U.Pat *)
fun typeCheckPat(ctx : Context, env : Env, S.WildcardPat span) : U.Ty * (U.VId * U.Ty) S.VIdMap.map * U.Pat
    = let val ty = U.TyVar(span, freshTyVar(ctx))
      in (ty, S.VIdMap.empty, U.WildcardPat span)
      end
  | typeCheckPat(ctx, env, S.SConPat(span, scon))
    = (case scon of
           Syntax.IntegerConstant(_)   => (primTy_int, S.VIdMap.empty, U.SConPat(span, scon))
         | Syntax.WordConstant(_)      => (primTy_word, S.VIdMap.empty, U.SConPat(span, scon))
         | Syntax.RealConstant(_)      => emitError(ctx, [span], "no real constant may occur in a pattern")
         | Syntax.StringConstant(_)    => (primTy_string, S.VIdMap.empty, U.SConPat(span, scon))
         | Syntax.CharacterConstant(_) => (primTy_char, S.VIdMap.empty, U.SConPat(span, scon))
      )
  | typeCheckPat(ctx, env, pat as S.VarPat(span, vid))
    = (case Syntax.VIdMap.find(#valMap env, vid) of
           SOME (_, _, Syntax.ValueConstructor) => emitError(ctx, [span], "VarPat: invalid pattern")
         | SOME (_, _, Syntax.ExceptionConstructor) => emitError(ctx, [span], "VarPat: invalid pattern")
         | _ => let val ty = USyntax.TyVar(span, freshTyVar(ctx))
                    val vid' = newVId(ctx, vid)
                in (ty, S.VIdMap.singleton(vid, (vid', ty)), U.VarPat(span, vid', ty))
                end
      )
  | typeCheckPat(ctx, env, S.RecordPat{sourceSpan, fields, wildcard})
    = let fun oneField((label, pat), (fieldTypes, vars, fieldPats))
              = let val (ty, vars', pat') = typeCheckPat(ctx, env, pat)
                in ((label, ty) :: fieldTypes, Syntax.VIdMap.unionWith (fn _ => emitError(ctx, [], "trying to bind the same identifier twice")) (vars, vars'), (label, pat') :: fieldPats)
                end
          val (fieldTypes, vars, fieldPats) = List.foldr oneField ([], Syntax.VIdMap.empty, []) fields
      in if wildcard then
             let val recordTy = U.TyVar(sourceSpan, freshTyVar(ctx))
                 fun oneField(label, ty) = addConstraint(ctx, env, U.UnaryConstraint(sourceSpan, recordTy, U.HasField { sourceSpan = sourceSpan, label = label, fieldTy = ty }))
             in List.app oneField fieldTypes
              ; (recordTy, vars, U.RecordPat{sourceSpan=sourceSpan, fields=fieldPats, wildcard=wildcard})
             end
         else
             (U.RecordType(sourceSpan, fieldTypes), vars, U.RecordPat{sourceSpan=sourceSpan, fields=fieldPats, wildcard=wildcard})
      end
  | typeCheckPat(ctx, env, S.ConPat(span, longvid, optInnerPat))
    = (case lookupLongVIdInEnv(ctx, env, span, longvid) of
           SOME (longvid, tysc, idstatus) =>
           (if idstatus = Syntax.ValueConstructor orelse idstatus = Syntax.ExceptionConstructor then
                let val (ty, tyargs) = instantiate(ctx, span, tysc)
                    val isSoleConstructor = let val tycon = getConstructedType(ctx, span, ty)
                                                val { valEnv, ... } = lookupUTyConInEnv(ctx, env, span, tycon)
                                            in S.VIdMap.numItems valEnv = 1
                                            end

                in case optInnerPat of
                       NONE => (ty, Syntax.VIdMap.empty, U.ConPat { sourceSpan = span, longvid = longvid, payload = NONE, tyargs = List.map #1 tyargs, isSoleConstructor = isSoleConstructor })
                     | SOME innerPat =>
                       (case ty of
                            U.FnType(span', argTy, resultTy) =>
                            let val (argTy', innerVars, innerPat') = typeCheckPat(ctx, env, innerPat)
                            in addConstraint(ctx, env, U.EqConstr(span, argTy, argTy'))
                             ; (resultTy, innerVars, U.ConPat { sourceSpan = span, longvid = longvid, payload = SOME innerPat', tyargs = List.map #1 tyargs, isSoleConstructor = isSoleConstructor })
                            end
                          | _ => emitError(ctx, [span], "invalid pattern")
                       )
                end
            else (* idstatus = Syntax.ValueVariable *)
                emitError(ctx, [span], "invalid pattern")
           )
         | NONE => emitError(ctx, [span], "invalid pattern")
      )
  | typeCheckPat(ctx, env, pat as S.TypedPat(span1, S.WildcardPat span2, ty))
    = let val ty = evalTy(ctx, env, ty)
      in (ty, Syntax.VIdMap.empty, U.TypedPat(span1, U.WildcardPat span2, ty))
      end
  | typeCheckPat(ctx, env, pat as S.TypedPat(span1, S.VarPat(span2, vid), ty))
    = let val ty = evalTy(ctx, env, ty)
      in case Syntax.VIdMap.find(#valMap env, vid) of
             SOME (_, _, Syntax.ValueConstructor) => emitError(ctx, [span2], "VarPat: invalid pattern")
           | SOME (_, _, Syntax.ExceptionConstructor) => emitError(ctx, [span2], "VarPat: invalid pattern")
           | _ => let val vid' = newVId(ctx, vid)
                  in (ty, Syntax.VIdMap.singleton(vid, (vid', ty)), U.VarPat(span1, vid', ty))
                  end
      end
  | typeCheckPat(ctx, env, S.TypedPat(span, pat, ty))
    = let val ty = evalTy(ctx, env, ty)
          val (inferredTy, vars, pat) = typeCheckPat(ctx, env, pat)
      in addConstraint(ctx, env, U.EqConstr(span, ty, inferredTy))
       ; (ty, vars, U.TypedPat(span, pat, ty))
      end
  | typeCheckPat(ctx, env, S.LayeredPat(span, vid, optTy, pat))
    = let val (inferredTy, vars, pat) = typeCheckPat(ctx, env, pat)
      in case Syntax.VIdMap.find(vars, vid) of
             NONE => let val ty = case optTy of
                                      SOME ty => let val ty = evalTy(ctx, env, ty)
                                                 in addConstraint(ctx, env, U.EqConstr(span, ty, inferredTy))
                                                  ; ty
                                                 end
                                    | NONE => inferredTy
                         val vid' = newVId(ctx, vid)
                     in (ty, Syntax.VIdMap.insert(vars, vid, (vid', ty)), U.LayeredPat(span, vid', ty, pat))
                     end
           | SOME _ => emitError(ctx, [span], "trying to bind the same identifier twice")
      end

(* typeCheckExp : Context * Env * S.Exp -> U.Ty * U.Exp *)
fun typeCheckExp(ctx : Context, env : Env, S.SConExp(span, scon)) : U.Ty * U.Exp
    = let val ty = case scon of (* TODO: overloaded literals *)
                       Syntax.IntegerConstant x   => primTy_int
                     | Syntax.WordConstant x      => primTy_word
                     | Syntax.RealConstant x      => primTy_real
                     | Syntax.StringConstant x    => primTy_string
                     | Syntax.CharacterConstant x => primTy_char
      in (ty, U.SConExp(span, scon))
      end
  | typeCheckExp(ctx, env, exp as S.VarExp(span, longvid as Syntax.MkQualified(_, vid)))
    = (case lookupLongVIdInEnv(ctx, env, span, longvid) of
           SOME (longvid, tysc, ids) => let val (ty, tyargs) = instantiate(ctx, span, tysc)
                               in (ty, U.VarExp(span, longvid, ids, tyargs))
                               end
         | NONE => emitError(ctx, [span], "unknown value name " ^ Syntax.getVIdName vid)
      )
  | typeCheckExp(ctx, env, S.RecordExp(span, fields))
    = let fun oneField(label, exp) = case typeCheckExp(ctx, env, exp) of
                                         (ty, exp) => ((label, ty), (label, exp))
          val (fieldTypes, fields) = ListPair.unzip (List.map oneField fields)
      in (U.RecordType(span, fieldTypes), U.RecordExp(span, fields))
      end
  | typeCheckExp(ctx, env, S.LetInExp(span, decs, innerExp))
    = let val (env', decs) = typeCheckDecs(ctx, env, decs)
          val (ty, innerExp) = typeCheckExp(ctx, mergeEnv(env, env'), innerExp)
      in (ty, U.LetInExp(span, decs, innerExp))
      end
  | typeCheckExp(ctx, env, S.AppExp(span, f, x))
          (* f: s -> t, x: s *)
    = let val (funTy, f) = typeCheckExp(ctx, env, f)
          val (argTy, x) = typeCheckExp(ctx, env, x)
          val retTy = case funTy of
                          U.FnType(_, argTy', retTy) => ( addConstraint(ctx, env, U.EqConstr(span, argTy, argTy')); retTy )
                        | _ => let val retTy = U.TyVar(span, freshTyVar(ctx))
                               in addConstraint(ctx, env, U.EqConstr(span, funTy, U.FnType(span, argTy, retTy))) (* funTy = (argTy -> retTy) *)
                                ; retTy
                               end
      in (retTy, U.AppExp(span, f, x))
      end
  | typeCheckExp(ctx, env, S.TypedExp(span, exp, ty))
    = let val (expTy, exp) = typeCheckExp(ctx, env, exp)
          val ty = evalTy(ctx, env, ty)
      in addConstraint(ctx, env, U.EqConstr(span, expTy, ty))
       ; (ty, U.TypedExp(span, exp, ty))
      end
  | typeCheckExp(ctx, env, S.HandleExp(span, exp, matches))
          (* exp: t, matches: exn -> t *)
    = let val (expTy, exp) = typeCheckExp(ctx, env, exp)
          val (patTy, retTy, matches) = typeCheckMatch(ctx, env, span, matches)
      in addConstraint(ctx, env, U.EqConstr(span, patTy, primTy_exn)) (* patTy = exn *)
       ; addConstraint(ctx, env, U.EqConstr(span, expTy, retTy))
       ; (expTy, U.HandleExp(span, exp, matches))
      end
  | typeCheckExp(ctx, env, S.RaiseExp(span, exp))
    = let val (expTy, exp) = typeCheckExp(ctx, env, exp)
      in addConstraint(ctx, env, U.EqConstr(span, expTy, primTy_exn)) (* expTy = exn *)
       ; (U.TyVar(span, freshTyVar(ctx)), U.RaiseExp(span, exp))
      end
  | typeCheckExp(ctx, env, S.IfThenElseExp(span, cond, thenPart, elsePart))
    = let val (condTy, cond) = typeCheckExp(ctx, env, cond)
          val (thenTy, thenPart) = typeCheckExp(ctx, env, thenPart)
          val (elseTy, elsePart) = typeCheckExp(ctx, env, elsePart)
      in addConstraint(ctx, env, U.EqConstr(span, condTy, primTy_bool)) (* condTy = bool *)
       ; addConstraint(ctx, env, U.EqConstr(span, thenTy, elseTy)) (* thenTy = elseTy *)
       ; (thenTy, U.IfThenElseExp(span, cond, thenPart, elsePart))
      end
  | typeCheckExp(ctx, env, S.CaseExp(span, exp, matches))
    = let val (expTy, exp) = typeCheckExp(ctx, env, exp)
          val (patTy, retTy, matches) = typeCheckMatch(ctx, env, span, matches)
      in addConstraint(ctx, env, U.EqConstr(span, expTy, patTy))
       ; (retTy, U.CaseExp(span, exp, expTy, matches))
      end
  | typeCheckExp(ctx, env, S.FnExp(span, matches))
    = let val (argTy, retTy, matches) = typeCheckMatch(ctx, env, span, matches)
          val fnExp = case matches of
                          [(U.VarPat(span2, vid, _), body)] => U.FnExp(span, vid, argTy, body)
                        | _ => let val vid = newVId(ctx, Syntax.MkVId "a")
                               in U.FnExp(span, vid, argTy, U.CaseExp(span, U.VarExp(span, U.MkShortVId(vid), Syntax.ValueVariable, []), argTy, matches))
                               end
      in (U.FnType(span, argTy, retTy), fnExp)
      end
  | typeCheckExp(ctx, env, S.ProjectionExp(span, label))
    = let val recordTy = USyntax.TyVar(span, freshTyVar(ctx))
          val fieldTy = USyntax.TyVar(span, freshTyVar(ctx))
      in addConstraint(ctx, env, U.UnaryConstraint(span, recordTy, U.HasField { sourceSpan = span, label = label, fieldTy = fieldTy }))
       ; (U.FnType(span, recordTy, fieldTy), U.ProjectionExp { sourceSpan = span, label = label, recordTy = recordTy, fieldTy = fieldTy })
      end
  | typeCheckExp(ctx, env, S.ListExp(span, xs))
    = let val elemTy = USyntax.TyVar(span, freshTyVar(ctx))
          val xs = Vector.map (fn exp => let val (expTy, exp) = typeCheckExp(ctx, env, exp)
                                         in addConstraint(ctx, env, U.EqConstr(span, expTy, elemTy))
                                          ; exp
                                         end) xs
      in (U.TyCon(span, [elemTy], primTyCon_list), U.ListExp(span, xs, elemTy))
      end
(* typeCheckDec : Context * Env * S.Dec -> (* created environment *) Env * U.Dec list *)
and typeCheckDec(ctx, env : Env, S.ValDec(span, tyvarseq, valbinds))
    = let val valbinds = let val env = { valMap = #valMap env
                                       , tyConMap = #tyConMap env
                                       , allTyConMap = #allTyConMap env
                                       , strMap = #strMap env
                                       , sigMap = #sigMap env
                                       , boundTyVars = List.foldl (fn (tv, m) => Syntax.TyVarMap.insert(m, tv, genTyVar(ctx, tv))) (#boundTyVars env) tyvarseq
                                       }
                         in List.map (fn S.PatBind(span, pat, exp) =>
                                         let val (patTy, newValEnv, pat) = typeCheckPat(ctx, env, pat)
                                             val (expTy, exp) = typeCheckExp(ctx, env, exp)
                                             val () = addConstraint(ctx, env, U.EqConstr(span, patTy, expTy))
                                             val generalizable = isExhaustive(ctx, env, pat) andalso isNonexpansive(env, exp)
                                         in { sourceSpan = span, pat = pat, exp = exp, expTy = expTy, valEnv = newValEnv, generalizable = generalizable }
                                         end
                                     ) valbinds
                          end
          val tvc = !(#tyVarConstraints ctx)
          val subst = !(#tyVarSubst ctx)
          val env' = applySubstEnv subst env
          val tyVars_env = freeTyVarsInEnv(U.TyVarSet.empty, env')
          fun generalize({ sourceSpan = span, pat, exp, expTy, valEnv : (U.VId * U.Ty) S.VIdMap.map, generalizable = false }, (valbinds, valEnvRest : (U.VId * U.TypeScheme) S.VIdMap.map))
              = let val vars = Syntax.VIdMap.listItems valEnv
                in case vars of
                       [(vid, ty)] => let val valbind' = U.PolyVarBind( span
                                                                      , vid
                                                                      , U.TypeScheme([], ty)
                                                                      , case pat of
                                                                            U.VarPat _ => exp
                                                                          | U.TypedPat (_, U.VarPat _, _) => exp
                                                                          | _ => let val espan = U.getSourceSpanOfExp exp
                                                                                     val vid' = renewVId ctx vid
                                                                                     val pat' = U.renameVarsInPat (U.VIdMap.insert(U.VIdMap.empty, vid, vid')) pat
                                                                                 in U.CaseExp(espan, exp, ty, if isExhaustive(ctx, env, pat) then
                                                                                                                  [(pat', U.VarExp(espan, U.MkShortVId(vid'), Syntax.ValueVariable, []))]
                                                                                                              else
                                                                                                                  [(pat', U.VarExp(espan, U.MkShortVId(vid'), Syntax.ValueVariable, []))
                                                                                                                  ,(U.WildcardPat span, U.RaiseExp(span, U.VarExp(span, LongVId_Bind, Syntax.ExceptionConstructor, [])))
                                                                                                                  ]
                                                                                             )
                                                                                 end
                                                                      )
                                      in (valbind' :: valbinds, S.VIdMap.unionWith #2 (S.VIdMap.map (fn (vid, ty) => (vid, U.TypeScheme([], ty))) valEnv, valEnvRest))
                                      end
                     | _ => let val espan = U.getSourceSpanOfExp exp
                                val vars' = List.map (fn (vid, _) => (vid, renewVId ctx vid)) vars
                                val varsMap = List.foldl U.VIdMap.insert' U.VIdMap.empty vars'
                                val pat' = U.renameVarsInPat varsMap pat
                                val tup = U.TupleExp(espan, List.map (fn (_, vid') => U.VarExp(espan, U.MkShortVId(vid'), Syntax.ValueVariable, [])) vars')
                                val valbind' = U.TupleBind( span
                                                          , vars
                                                          , U.CaseExp( espan
                                                                     , exp
                                                                     , expTy
                                                                     , if isExhaustive(ctx, env, pat) then
                                                                           [(pat', tup)]
                                                                       else
                                                                           [(pat', tup)
                                                                           ,(U.WildcardPat span, U.RaiseExp(span, U.VarExp(span, LongVId_Bind, Syntax.ExceptionConstructor, [])))
                                                                           ]
                                                                     )
                                                          )
                            in (valbind' :: valbinds, S.VIdMap.unionWith #2 (S.VIdMap.map (fn (vid, ty) => (vid, U.TypeScheme([], ty))) valEnv, valEnvRest))
                            end
                end
            | generalize({ sourceSpan = span, pat, exp, expTy, valEnv, generalizable = true }, (valbinds, valEnvRest))
              = let fun doVal (vid,ty)
                        = let val ty' = applySubstTy subst ty
                              val tyVars_ty = U.freeTyVarsInTy(U.TyVarSet.empty, ty')
                              fun isEqualityType (USyntax.IsEqType _) = true
                                | isEqualityType _ = false
                              fun isGeneralizable(tv: U.TyVar) = case USyntax.TyVarMap.find(tvc, tv) of
                                                                     NONE => true
                                                                   | SOME tvs => List.all isEqualityType tvs
                              val tyVars = U.TyVarSet.difference(U.TyVarSet.filter isGeneralizable tyVars_ty, tyVars_env)
                              fun doTyVar (tv as USyntax.NamedTyVar (_, true, _)) = (tv, [U.IsEqType span])
                                | doTyVar tv = case USyntax.TyVarMap.find(tvc, tv) of
                                                   NONE => (tv, [])
                                                 | SOME tvs => if List.exists isEqualityType tvs then
                                                                   (tv, [U.IsEqType span])
                                                               else (* should not reach here *)
                                                                   (tv, [])
                              val tysc = U.TypeScheme(List.map doTyVar (U.TyVarSet.listItems tyVars), ty')
                          in (vid, tysc)
                          end
                    val valEnv' = Syntax.VIdMap.map doVal valEnv
                    val valEnv'L = Syntax.VIdMap.listItems valEnv'
                    val allPoly = List.all (fn (_, U.TypeScheme(tv, _)) => not (List.null tv)) valEnv'L (* all bindings are generalized? *)
                    val espan = USyntax.getSourceSpanOfExp exp
                    fun polyPart [] = []
                      | polyPart ((vid, U.TypeScheme([], _)) :: rest) = polyPart rest
                      | polyPart ((vid, tysc) :: rest) = let val vid' = renewVId ctx vid
                                                             val pat' = USyntax.renameVarsInPat (USyntax.VIdMap.insert(USyntax.VIdMap.empty, vid, vid')) pat
                                                         in U.PolyVarBind(span, vid, tysc, USyntax.CaseExp(espan, exp, expTy, [(USyntax.filterVarsInPat (fn x => x = vid') pat', USyntax.VarExp(espan, USyntax.MkShortVId(vid'), Syntax.ValueVariable, []))])) :: polyPart rest
                                                         end
                    fun isMonoVar vid = List.exists (fn (vid', U.TypeScheme(tvs, _)) => U.eqVId(vid, vid') andalso List.null tvs) valEnv'L
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
                                                             in U.PolyVarBind(span, vid, U.TypeScheme([], ty), U.CaseExp(espan, exp, expTy, [(pat', U.VarExp(espan, USyntax.MkShortVId(vid'), Syntax.ValueVariable, []))])) :: polyPart valEnv'L
                                                             end
                                            | _ => let val vars' = List.map (fn (vid, _) => (vid, renewVId ctx vid)) xs
                                                       val varsMap = List.foldl USyntax.VIdMap.insert' USyntax.VIdMap.empty vars'
                                                       val pat' = USyntax.renameVarsInPat varsMap (USyntax.filterVarsInPat isMonoVar pat)
                                                       val tup = U.TupleExp(espan, List.map (fn (_, vid') => U.VarExp(espan, USyntax.MkShortVId(vid'), Syntax.ValueVariable, [])) vars')
                                                   in U.TupleBind(span, xs, U.CaseExp(espan, exp, expTy, [(pat', tup)])) :: polyPart valEnv'L
                                                   end
                                       end
                in (valbind' @ valbinds, Syntax.VIdMap.unionWith #2 (Syntax.VIdMap.map (fn (vid, tysc) => (vid, tysc)) valEnv', valEnvRest))
                end
          val (valbinds, valEnv) = List.foldr generalize ([], Syntax.VIdMap.empty) valbinds
          val env' = envWithValEnv (Syntax.VIdMap.map (fn (vid, tysc) => (U.MkShortVId vid, tysc, Syntax.ValueVariable)) valEnv)
      in (env', [U.ValDec(span, valbinds)])
      end
  | typeCheckDec(ctx, env, S.RecValDec(span, tyvarseq, valbinds))
    = let val valbinds' : (SourcePos.span * (U.Ty * (U.VId * U.Ty) S.VIdMap.map * U.Pat) * S.Exp) list
              = List.map (fn S.PatBind (span, pat, exp) => (span, typeCheckPat(ctx, env, pat), exp)) valbinds
          val localValEnv = List.foldl (fn ((_, (_, ve, _), _), acc) => Syntax.VIdMap.unionWith #1 (acc, ve)) Syntax.VIdMap.empty valbinds'
          val localValMap = Syntax.VIdMap.map (fn (vid', ty) => (U.MkShortVId vid', U.TypeScheme ([], ty), Syntax.ValueVariable)) localValEnv
          val localEnv = let val { valMap, tyConMap, allTyConMap, strMap, sigMap, boundTyVars } = env
                         in { valMap = Syntax.VIdMap.unionWith #2 (valMap, localValMap)
                            , tyConMap = tyConMap
                            , allTyConMap = allTyConMap
                            , strMap = strMap
                            , sigMap = sigMap
                            , boundTyVars = List.foldl (fn (tv, m) => Syntax.TyVarMap.insert(m, tv, genTyVar(ctx, tv))) boundTyVars tyvarseq
                            }
                         end
          val valbinds'' = List.map (fn (span, (patTy, newValEnv, pat), exp) =>
                                        let val (expTy, exp) = typeCheckExp(ctx, localEnv, exp)
                                            val () = addConstraint(ctx, env, U.EqConstr(span, patTy, expTy))
                                            val generalizable = isExhaustive(ctx, env, pat) andalso isNonexpansive(env, exp)
                                        in if generalizable then
                                               { sourceSpan = span, pat = pat, exp = exp, expTy = expTy, valEnv = newValEnv }
                                           else
                                               emitError(ctx, [span], "'val rec' must be generalizable")
                                        end
                                    ) valbinds'
          val tvc = !(#tyVarConstraints ctx)
          val subst = !(#tyVarSubst ctx)
          val env' = applySubstEnv subst localEnv
          val tyVars_env = freeTyVarsInEnv(U.TyVarSet.empty, applySubstEnv subst env)
          fun generalize ({ sourceSpan = span, pat, exp, expTy, valEnv }, (valbinds, valEnvRest))
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
                          in (vid, tysc)
                          end
                    val valEnv' = Syntax.VIdMap.map doVal valEnv
                    val valbinds = Syntax.VIdMap.foldr (fn ((vid, tysc), rest) => U.PolyVarBind(span, vid, tysc, exp) :: rest) valbinds valEnv'
                in (valbinds, Syntax.VIdMap.unionWith #2 (valEnv', valEnvRest))
                end
          val (valbinds, valEnv) = List.foldr generalize ([], Syntax.VIdMap.empty) valbinds''
          val env' = envWithValEnv (Syntax.VIdMap.map (fn (vid, tysc) => (USyntax.MkShortVId vid, tysc, Syntax.ValueVariable)) valEnv)
      in (env', [U.RecValDec(span, valbinds)])
      end
  | typeCheckDec(ctx, env, S.TypeDec(span, typbinds))
    = let fun doTypBind (S.TypBind(span, tyvars, tycon, ty), (tyConEnv, typbinds))
              = let val tyvars = List.map (fn tv => (tv, genTyVar(ctx, tv))) tyvars
                    val env' = { valMap = #valMap env
                               , tyConMap = #tyConMap env
                               , allTyConMap = #allTyConMap env
                               , strMap = #strMap env
                               , sigMap = #sigMap env
                               , boundTyVars = List.foldl Syntax.TyVarMap.insert' (#boundTyVars env) tyvars
                               }
                    val ty = evalTy(ctx, env', ty)
                    val tystr : U.TypeStructure = { typeFunction = U.TypeFunction(List.map #2 tyvars, ty)
                                                  , valEnv = U.emptyValEnv
                                                  , admitsEquality = false
                                                  , isRefOrArray = false
                                                  }
                in (Syntax.TyConMap.insert(tyConEnv, tycon, tystr) (* TODO: error on duplicate *), U.TypBind(span, List.map #2 tyvars, newTyCon(ctx, tycon), ty) :: typbinds)
                end
          val (tyConEnv, typbinds) = List.foldr doTypBind (Syntax.TyConMap.empty, []) typbinds
      in (envWithTyConEnv(tyConEnv, USyntax.TyConMap.empty), [U.TypeDec(span, typbinds)])
      end
  | typeCheckDec(ctx, env, S.DatatypeDec(span, datbinds))
    = let val equalityMap : bool S.TyConMap.map = determineDatatypeEquality(ctx, env, List.foldl (fn (S.DatBind(_, tyvars, tycon, conbinds), m) => S.TyConMap.insert(m, tycon, (tyvars, List.mapPartial (fn S.ConBind(_, _, optTy) => optTy) conbinds))) S.TyConMap.empty datbinds)
          val datbinds = List.map (fn datbind as S.DatBind(span, tyvars, tycon, conbinds) => (datbind, newTyCon(ctx, tycon))) datbinds
          val partialEnv = envWithTyConEnv (List.foldl (fn ((S.DatBind(span, tyvars, tycon, conbinds), tycon'), (m, m')) =>
                                                           let val tyvars = List.map (fn tv => genTyVar(ctx, tv)) tyvars
                                                               val tystr = { typeFunction = U.TypeFunction(tyvars, U.TyCon(span, List.map (fn tv => U.TyVar(span, tv)) tyvars, tycon'))
                                                                           , valEnv = U.emptyValEnv
                                                                           , admitsEquality = S.TyConMap.lookup(equalityMap, tycon)
                                                                           , isRefOrArray = false
                                                                           }
                                                           in (Syntax.TyConMap.insert(m, tycon, tystr), USyntax.TyConMap.insert(m', tycon', tystr))
                                                           end
                                                       ) (Syntax.TyConMap.empty, USyntax.TyConMap.empty) datbinds)
          val (tyConMap, allTyConMap, valMap, datbinds)
              = let fun doDatBind ((S.DatBind(span, tyvars, tycon, conbinds), tycon'), (tyConMap, allTyConMap, accValEnv, datbinds))
                        = let val tyvars = List.map (fn tv => (tv, genTyVar(ctx, tv))) tyvars
                              val env = mergeEnv(env, { valMap = #valMap partialEnv
                                                      , tyConMap = #tyConMap partialEnv
                                                      , allTyConMap = #allTyConMap partialEnv
                                                      , strMap = #strMap partialEnv
                                                      , sigMap = #sigMap partialEnv
                                                      , boundTyVars = List.foldl Syntax.TyVarMap.insert' (#boundTyVars partialEnv) tyvars
                                                      }
                                                )
                              val tyvars = List.map #2 tyvars
                              val (valEnv, conbinds) = List.foldr (fn (S.ConBind(span, vid, optTy), (valEnv, conbinds)) =>
                                                                      let val vid' = newVId(ctx, vid)
                                                                          val optTy = Option.map (fn ty => evalTy(ctx, env, ty)) optTy
                                                                          val conbind = U.ConBind(span, vid', optTy)
                                                                          val tysc = U.TypeScheme(List.map (fn tv => (tv, [])) tyvars, case optTy of
                                                                                                                                           NONE => U.TyCon(span, List.map (fn tv => U.TyVar(span, tv)) tyvars, tycon')
                                                                                                                                         | SOME payloadTy => U.FnType(span, payloadTy, U.TyCon(span, List.map (fn tv => U.TyVar(span, tv)) tyvars, tycon'))
                                                                                                 )
                                                                      in (Syntax.VIdMap.insert(valEnv, vid, (U.MkShortVId vid', tysc, Syntax.ValueConstructor)) (* TODO: check for duplicate *), conbind :: conbinds)
                                                                      end
                                                                  ) (Syntax.VIdMap.empty, []) conbinds
                              val datbind = U.DatBind(span, tyvars, tycon', conbinds, S.TyConMap.lookup(equalityMap, tycon))
                              val tystr = { typeFunction = U.TypeFunction(tyvars, U.TyCon(span, List.map (fn tv => U.TyVar(span, tv)) tyvars, tycon'))
                                          , valEnv = Syntax.VIdMap.map (fn (_, tysc, ids) => (tysc, ids)) valEnv
                                          , admitsEquality = Syntax.TyConMap.lookup(equalityMap, tycon)
                                          , isRefOrArray = false
                                          }
                          in (Syntax.TyConMap.insert(tyConMap, tycon, tystr), USyntax.TyConMap.insert(allTyConMap, tycon', tystr), Syntax.VIdMap.unionWith #2 (accValEnv, valEnv) (* TODO: check for duplicate *), datbind :: datbinds)
                          end
                in List.foldr doDatBind (Syntax.TyConMap.empty, USyntax.TyConMap.empty, Syntax.VIdMap.empty, []) datbinds
                end
          val env' = { valMap = valMap
                     , tyConMap = tyConMap
                     , allTyConMap = allTyConMap
                     , strMap = Syntax.StrIdMap.empty
                     , sigMap = Syntax.SigIdMap.empty
                     , boundTyVars = Syntax.TyVarMap.empty
                     }
      in (env', [U.DatatypeDec(span, datbinds)])
      end
  | typeCheckDec(ctx, env, S.DatatypeRepDec(span, tycon, longtycon))
    = let val tystr = lookupTyConInEnv(ctx, env, span, longtycon)
          val getLongVId = case longtycon of
                               Syntax.MkQualified([], _) =>
                               (fn vid => case Syntax.VIdMap.find(#valMap env, vid) of
                                              SOME (longvid, _, _) => longvid
                                            | NONE => emitError(ctx, [span], "datatype replication: value identifier " ^ Syntax.print_VId vid ^ " not found (internal error)")
                               )
                             | Syntax.MkQualified(strid0 :: strids, _) =>
                               case Syntax.StrIdMap.find(#strMap env, strid0) of
                                   SOME (U.MkLongStrId(strid0, strids0), s) => (fn vid => U.MkLongVId(strid0, strids0 @ strids, vid))
                                 | NONE => emitError(ctx, [span], "datatype replication: structure " ^ Syntax.print_StrId strid0 ^ " not found (internal error)")
          val env' = { valMap = Syntax.VIdMap.mapi (fn (vid, (tysc, ids)) => (getLongVId vid, tysc, ids)) (#valEnv tystr)
                     , tyConMap = Syntax.TyConMap.singleton(tycon, tystr)
                     , allTyConMap = USyntax.TyConMap.empty
                     , strMap = Syntax.StrIdMap.empty
                     , sigMap = Syntax.SigIdMap.empty
                     , boundTyVars = Syntax.TyVarMap.empty
                     }
      in (env', [])
      end
  | typeCheckDec(ctx, env, S.AbstypeDec(span, _, _)) = emitError(ctx, [span], "abstype: not implemented yet")
  | typeCheckDec(ctx, env, S.ExceptionDec(span, exbinds))
    = let fun doExBind(S.ExBind(span, vid, optTy), (valMap, exbinds))
              = let val optTy = Option.map (fn ty => evalTy(ctx, env, ty)) optTy
                    val vid' = newVId(ctx, vid)
                    val valMap = S.VIdMap.insert(valMap, vid, (U.MkShortVId(vid'), U.TypeScheme([], case optTy of
                                                                                                        NONE => primTy_exn
                                                                                                      | SOME ty => U.FnType(span, ty, primTy_exn)
                                                                                               ), Syntax.ExceptionConstructor))
                in (valMap, U.ExBind(span, vid', optTy) :: exbinds)
                end
            | doExBind(S.ExReplication(span, vid, longvid), (valMap, exbinds))
              = let val vid' = newVId(ctx, vid)
                in case lookupLongVIdInEnv(ctx, env, span, longvid) of
                       SOME (longvid, tysc, ids as Syntax.ExceptionConstructor) =>
                       let val optTy = case tysc of
                                           U.TypeScheme([], U.FnType(_, payloadTy, _)) => SOME payloadTy
                                         | U.TypeScheme([], _) => NONE
                                         | U.TypeScheme(_ :: _, _) => emitError(ctx, [span], "exception constructor must have monomorphic type")
                       in (S.VIdMap.insert(valMap, vid, (U.MkShortVId(vid'), tysc, ids)), U.ExReplication(span, vid', longvid, optTy) :: exbinds)
                       end
                     | _ => emitError(ctx, [span], "exception replication: RHS must be an exception constructor")
                end
          val (valMap, exbinds) = List.foldr doExBind (Syntax.VIdMap.empty, []) exbinds
      in (envWithValEnv valMap, [U.ExceptionDec(span, exbinds)])
      end
  | typeCheckDec(ctx, env, S.LocalDec(span, decs1, decs2))
    = let val (env', decs1) = typeCheckDecs(ctx, env, decs1)
          val (env'', decs2) = typeCheckDecs(ctx, mergeEnv(env, env'), decs2)
          val env'' = { valMap = #valMap env''
                      , tyConMap = #tyConMap env''
                      , allTyConMap = USyntax.TyConMap.unionWith #2 (#allTyConMap env', #allTyConMap env'')
                      , strMap = #strMap env''
                      , sigMap = #sigMap env''
                      , boundTyVars = #boundTyVars env''
                      }
      in (env'', case decs1 @ decs2 of
                     decs as [] => decs
                   | decs as [dec] => decs
                   | decs => [U.GroupDec(span, decs)])
      end
  | typeCheckDec(ctx, env, S.OpenDec(span, longstrids))
    = let fun getStructure(Syntax.MkQualified([], strid))
              = (case Syntax.StrIdMap.find(#strMap env, strid) of
                     SOME (U.MkLongStrId(strid0, strids0), s) => { valMap = Syntax.VIdMap.mapi (fn (vid, (tysc, ids)) => (U.MkLongVId(strid0, strids0, vid), tysc, ids)) (#valMap s)
                                                                 , tyConMap = #tyConMap s
                                                                 , allTyConMap = USyntax.TyConMap.empty
                                                                 , strMap = Syntax.StrIdMap.mapi (fn (strid', U.MkSignature s) => (U.MkLongStrId(strid0, strids0 @ [strid']), s)) (#strMap s)
                                                                 , sigMap = Syntax.SigIdMap.empty
                                                                 , boundTyVars = Syntax.TyVarMap.empty
                                                                 }
                   | NONE => emitError(ctx, [span], "structure not found")
                )
            | getStructure(Syntax.MkQualified(strid0 :: strids, strid'))
              = (case Syntax.StrIdMap.find(#strMap env, strid0) of
                     SOME (U.MkLongStrId(strid0, strids0), s0) => let val s = lookupStr(ctx, s0, span, strids @ [strid'])
                                                                      val strids = strids0 @ strids @ [strid']
                                                                  in { valMap = Syntax.VIdMap.mapi (fn (vid, (tysc, ids)) => (U.MkLongVId(strid0, strids, vid), tysc, ids)) (#valMap s)
                                                                     , tyConMap = #tyConMap s
                                                                     , allTyConMap = USyntax.TyConMap.empty
                                                                     , strMap = Syntax.StrIdMap.mapi (fn (strid', U.MkSignature s) => (U.MkLongStrId(strid0, strids @ [strid']), s)) (#strMap s)
                                                                     , sigMap = Syntax.SigIdMap.empty
                                                                     , boundTyVars = Syntax.TyVarMap.empty
                                                                     }
                                                                  end
                   | NONE => emitError(ctx, [span], "structure not found")
                )
          val env = List.foldl (fn (longstrid, acc) => mergeEnv(acc, getStructure longstrid)) emptyEnv longstrids
      in (env, [])
      end
(* typeCheckDecs : Context * Env * S.Dec list -> (* created environment *) Env * U.Dec list *)
and typeCheckDecs(ctx, env, []) : Env * U.Dec list = (emptyEnv, [])
  | typeCheckDecs(ctx, env, dec :: decs) = let val (env', dec) = typeCheckDec(ctx, env, dec)
                                               val (env'', decs) = typeCheckDecs(ctx, mergeEnv(env, env'), decs)
                                           in (mergeEnv(env', env''), dec @ decs)
                                           end
and determineDatatypeEquality(ctx, env, datbinds : (S.TyVar list * S.Ty list) S.TyConMap.map) : bool S.TyConMap.map
    = let val localTyCons = S.TyConMap.foldli (fn (tycon, _, s) => S.TyConSet.add (s, tycon)) S.TyConSet.empty datbinds
          val graph : (S.TyConSet.set ref) S.TyConMap.map = Syntax.TyConMap.map (fn _ => ref S.TyConSet.empty) datbinds
          val nonEqualitySet = ref S.TyConSet.empty
          fun doDatBind (tycon, (tyvars, payloads))
              = let val r = S.TyConMap.lookup (graph, tycon)
                    fun doTy (S.TyVar (span, tv)) = if List.exists (fn tv' => tv = tv') tyvars then
                                                        SOME []
                                                    else
                                                        (case tv of
                                                             S.MkTyVar name => if String.isPrefix "''" name then
                                                                                   SOME []
                                                                               else
                                                                                   NONE
                                                        )
                      | doTy (S.RecordType (span, fields)) = doTypes (List.map #2 fields)
                      | doTy (S.TyCon (span, tyargs, longtycon)) = let val l = case longtycon of
                                                                                   Syntax.MkQualified([], tycon) =>
                                                                                   if S.TyConSet.member (localTyCons, tycon) then
                                                                                       SOME [tycon]
                                                                                   else
                                                                                       NONE
                                                                                 | _ => NONE
                                                                   in case l of
                                                                          result as SOME _ => result
                                                                        | NONE => let val { admitsEquality, isRefOrArray, ... } = lookupTyConInEnv(ctx, env, span, longtycon)
                                                                                  in if isRefOrArray then
                                                                                         SOME []
                                                                                     else if admitsEquality then
                                                                                         doTypes tyargs
                                                                                     else
                                                                                         NONE
                                                                                  end
                                                                   end
                      | doTy (S.FnType _) = NONE
                    and doTypes types = let fun go (acc, ty :: types) = (case doTy ty of
                                                                             NONE => NONE
                                                                           | SOME xs => go (xs @ acc, types)
                                                                        )
                                              | go (acc, []) = SOME acc
                                        in go ([], types)
                                        end
                in case doTypes payloads of
                       NONE => nonEqualitySet := S.TyConSet.add (!nonEqualitySet, tycon)
                     | SOME xs => List.app (fn member => let val r = S.TyConMap.lookup (graph, member)
                                                         in r := S.TyConSet.add (!r, tycon)
                                                         end) xs
                end
          fun dfs tycon = let val set = !(S.TyConMap.lookup (graph, tycon))
                          in S.TyConSet.app (fn t => let val s = !nonEqualitySet
                                                     in if S.TyConSet.member (s, t) then
                                                            ()
                                                        else
                                                            ( nonEqualitySet := S.TyConSet.add (s, t)
                                                            ; dfs t
                                                            )
                                                     end
                                            ) set
                          end
          val () = S.TyConMap.appi doDatBind datbinds
          val () = S.TyConSet.app dfs (!nonEqualitySet)
          val nonEqualitySet = !nonEqualitySet
      in S.TyConSet.foldl (fn (tycon, map) => S.TyConMap.insert (map, tycon, not (S.TyConSet.member (nonEqualitySet, tycon)))) S.TyConMap.empty localTyCons
      end
 (* typeCheckMatch : Context * Env * SourcePos.span * (S.Pat * S.Exp) list -> (* pattern *) Syntax.Ty * (* expression *) Syntax.Ty * (Pat * Exp) list *)
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
and typeCheckMatchBranch(ctx : Context, env : Env, pat : S.Pat, exp : S.Exp) : U.Ty * U.Ty * U.Pat * U.Exp
    = let val (patTy, vars, pat') = typeCheckPat(ctx, env, pat)
          val env' = mergeEnv(env, envWithValEnv (Syntax.VIdMap.map (fn (vid, ty) => (U.MkShortVId vid, U.TypeScheme([], ty), Syntax.ValueVariable)) vars))
          val (expTy, exp') = typeCheckExp(ctx, env', exp)
      in (patTy, expTy, pat', exp')
      end

(* pretty printing *)
          (*
structure PrettyPrint = struct
fun print_Env ({ tyConMap, valMap, strMap, boundTyVars, ... } : Env) = "Env{tyMap=" ^ USyntax.print_TyConMap (fn _ => "TypeStructure _") tyConMap ^ ",valMap=" ^ USyntax.print_VIdMap (Syntax.print_pair (USyntax.print_TypeScheme, Syntax.print_IdStatus)) valMap ^ ",strMap=...,boundTyVars=...}"
end (* structure PrettyPrint *)
open PrettyPrint
          *)
(* applyDefaultTypes : Context * (U.UnaryConstraint list) USyntax.TyVarMap.map * USyntax.TopDec list -> USyntax.TopDec list *)
fun applyDefaultTypes(ctx, tvc, decs : U.TopDec list) : U.TopDec list =
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
        val freeTyVars = USyntax.freeTyVarsInTopDecs(USyntax.TyVarSet.empty, decs)
        val subst = USyntax.TyVarSet.foldl (fn (tv, map) => USyntax.TyVarMap.insert(map, tv, doTyVar tv)) USyntax.TyVarMap.empty freeTyVars
    in #doTopDecs (USyntax.mapTy (ctx, subst, false)) decs
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
            | goPat (U.ConPat { sourceSpan, longvid, payload, tyargs, isSoleConstructor }) = ( List.app goTy tyargs
                                                                                             ; Option.app goPat payload
                                                                                             )
            | goPat (U.TypedPat(span, pat, ty)) = ( goTy ty; goPat pat )
            | goPat (U.LayeredPat(span, vid, ty, pat)) = ( goTy ty; goPat pat )
          fun goExp (U.SConExp (span, scon)) = ()
            | goExp (U.VarExp (span, longvid, ids, tyargs)) = List.app (fn (ty, cts) => (goTy ty; List.app goUnaryConstraint cts)) tyargs
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
          and goDec (U.ValDec (span, valbinds)) = ( List.app goValBind valbinds
                                                  ; tyconset
                                                  )
            | goDec (U.RecValDec (span, valbinds)) = ( List.app goValBind valbinds
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
            | goDec (U.ExceptionDec (span, exbinds)) = ( List.app (fn U.ExBind (span, vid, optTy) => Option.app goTy optTy
                                                                  | U.ExReplication (span, vid, longvid, optTy) => Option.app goTy optTy
                                                                  ) exbinds
                                                       ; tyconset
                                                       )
            | goDec (U.GroupDec (span, decs)) = goDecs decs
          and goDecs decs = List.foldl (fn (dec, tyconset) => let val { goDec, ... } = checkTyScope (ctx, tvset, tyconset)
                                                              in goDec dec
                                                              end)
                                       tyconset decs
          and goValBind (U.TupleBind (span, binds, exp)) = ( List.app (fn (vid, ty) => goTy ty) binds
                                                           ; goExp exp
                                                           )
            | goValBind (U.PolyVarBind (span, vid, U.TypeScheme (typarams, ty), exp))
              = let val { goTy, goExp, ... } = checkTyScope (ctx, U.TyVarSet.addList (tvset, List.map #1 typarams), tyconset)
                in List.app (fn (tv, cts) => List.app goUnaryConstraint cts) typarams
                 ; goTy ty
                 ; goExp exp
                end
          fun goStrExp (U.StructExp _) = tyconset
            | goStrExp (U.StrIdExp _) = tyconset
            | goStrExp (U.TransparentConstraintExp _) = tyconset
            | goStrExp (U.OpaqueConstraintExp _) = tyconset (* TODO *)
            | goStrExp (U.LetInStrExp (span, strdecs, strexp)) = let val tyconset = goStrDecs strdecs
                                                                     val { goStrExp, ... } = checkTyScope (ctx, tvset, tyconset)
                                                                 in goStrExp strexp
                                                                 end
          and goStrDec (U.CoreDec(_, dec)) = goDec dec
            | goStrDec (U.StrBindDec(_, strid, strexp, s)) = goStrExp strexp (* TODO: signature *)
            | goStrDec (U.GroupStrDec(_, decs)) = goStrDecs decs
          and goStrDecs decs = List.foldl (fn (dec, tyconset) => let val { goStrDec, ... } = checkTyScope (ctx, tvset, tyconset)
                                                                 in goStrDec dec
                                                                 end)
                                          tyconset decs
          fun goTopDec (U.StrDec dec) = goStrDec dec
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
         , goStrExp = goStrExp
         , goStrDec = goStrDec
         , goStrDecs = goStrDecs
         , goTopDec = goTopDec
         , goTopDecs = goTopDecs
         }
      end
fun checkTyScopeOfProgram (ctx, tyconset : U.TyConSet.set, program : U.Program)
    = List.foldl (fn (topdec, tyconset) => let val { goTopDecs, ... } = checkTyScope (ctx, U.TyVarSet.empty, tyconset)
                                           in goTopDecs topdec
                                           end)
                 tyconset program

type SigEnv = { valMap : (USyntax.TypeScheme * Syntax.IdStatus) Syntax.VIdMap.map
              , tyConMap : USyntax.TypeStructure Syntax.TyConMap.map
              , strMap : USyntax.Signature Syntax.StrIdMap.map
              , sigMap : USyntax.QSignature Syntax.SigIdMap.map
              , boundTyVars : USyntax.TyVar Syntax.TyVarMap.map
              }

fun envToSigEnv(env : Env) : SigEnv
    = { valMap = Syntax.VIdMap.map (fn (longvid, tysc, ids) => (tysc, ids)) (#valMap env)
      , tyConMap = #tyConMap env
      , strMap = Syntax.StrIdMap.map #2 (#strMap env)
      , sigMap = #sigMap env
      , boundTyVars = #boundTyVars env
      }

(* lookupTyConInSigEnv : Context * SigEnv * SourcePos.span * Syntax.LongTyCon -> U.TypeStructure *)
fun lookupTyConInSigEnv(ctx, env : SigEnv, span, Syntax.MkQualified([], tycon as Syntax.MkTyCon name))
    = (case Syntax.TyConMap.find(#tyConMap env, tycon) of
           SOME tystr => tystr
         | NONE => emitError(ctx, [span], "unknown type constructor '" ^ name ^ "'")
      )
  | lookupTyConInSigEnv(ctx, env, span, Syntax.MkQualified(strid0 :: strids, tycon as Syntax.MkTyCon name))
    = (case Syntax.StrIdMap.find(#strMap env, strid0) of
           SOME s => (case lookupStr(ctx, s, span, strids) of
                          { tyConMap, ... } => case Syntax.TyConMap.find(tyConMap, tycon) of
                                                   SOME tystr => tystr
                                                 | NONE => emitError(ctx, [span], "unknown type constructor '" ^ name ^ "'")
                     )
         | NONE => emitError(ctx, [span], "unknown structure name '" ^ (case strid0 of Syntax.MkStrId name => name) ^ "'")
      )

(* evalTyInSig : Context * SigEnv * S.Ty -> U.Ty *)
fun evalTyInSig(ctx : Context, env : SigEnv, S.TyVar(span, tv)) : U.Ty
    = (case Syntax.TyVarMap.find(#boundTyVars env, tv) of
           SOME tv => U.TyVar(span, tv)
         | NONE => emitError(ctx, [span], "unknown type varibale `" ^ Syntax.print_TyVar tv ^ "`")
      )
  | evalTyInSig(ctx, env, S.RecordType(span, fields)) = U.RecordType(span, List.map (fn (label, ty) => (label, evalTyInSig(ctx, env, ty))) fields)
  | evalTyInSig(ctx, env, S.TyCon(span, args, tycon))
    = let val { typeFunction = U.TypeFunction(tyvars, ty), ... } = lookupTyConInSigEnv(ctx, env, span, tycon)
          val subst = (ListPair.foldlEq (fn (tv, arg, m) => USyntax.TyVarMap.insert (m, tv, evalTyInSig(ctx, env, arg))) USyntax.TyVarMap.empty (tyvars, args))
                      handle ListPair.UnequalLengths => emitError(ctx, [span], "invalid type construction")
      in U.applySubstTy subst ty
      end
  | evalTyInSig(ctx, env, S.FnType(span, ty1, ty2)) = U.FnType(span, evalTyInSig(ctx, env, ty1), evalTyInSig(ctx, env, ty2))

val emptySignature : USyntax.Signature = { valMap = Syntax.VIdMap.empty
                                         , tyConMap = Syntax.TyConMap.empty
                                         , strMap = Syntax.StrIdMap.empty
                                         }

fun mergeSignature(s1 : U.Signature, s2 : U.Signature) : U.Signature
    = { valMap = Syntax.VIdMap.mergeWith #2 (#valMap s1, #valMap s2)
      , tyConMap = Syntax.TyConMap.mergeWith #2 (#tyConMap s1, #tyConMap s2)
      , strMap = Syntax.StrIdMap.mergeWith #2 (#strMap s1, #strMap s2)
      }
fun mergeQSignature(s1 : U.QSignature, s2 : U.QSignature) : U.QSignature
    = { s = mergeSignature(#s s1, #s s2)
      , bound = U.TyConMap.unionWith #2 (#bound s1, #bound s2)
      }

fun addSignatureToEnv(env : SigEnv, s : U.Signature) : SigEnv
    = { valMap = #valMap env (* not used *)
      , tyConMap = Syntax.TyConMap.unionWith #2 (#tyConMap env, #tyConMap s)
      , strMap = Syntax.StrIdMap.unionWith #2 (#strMap env, Syntax.StrIdMap.map (fn U.MkSignature s => s) (#strMap s))
      , sigMap = #sigMap env
      , boundTyVars = #boundTyVars env
      }

fun applySubstTyConInTy (ctx : Context, subst : U.TypeFunction U.TyConMap.map) : U.Ty -> U.Ty
    = let fun goTy (ty as U.TyVar _) = ty
            | goTy (U.RecordType(span, fields)) = U.RecordType(span, Syntax.mapRecordRow goTy fields)
            | goTy (U.TyCon(span, tyargs, tycon)) = (case U.TyConMap.find(subst, tycon) of
                                                         NONE => U.TyCon(span, List.map goTy tyargs, tycon)
                                                       | SOME (U.TypeFunction (tyvars, ty)) =>
                                                         let val subst' = (ListPair.foldlEq (fn (tv, tyarg, m) => USyntax.TyVarMap.insert (m, tv, goTy tyarg)) USyntax.TyVarMap.empty (tyvars, tyargs))
                                                                          handle ListPair.UnequalLengths => emitError(ctx, [span], "invalid type constructor substitution")
                                                         in U.applySubstTy subst' ty
                                                         end
                                                    )
            | goTy (U.FnType(span, ty1, ty2)) = U.FnType(span, goTy ty1, goTy ty2)
      in goTy
      end
fun applySubstTyConInSig (ctx : Context, subst : U.TypeFunction U.TyConMap.map) : U.Signature -> U.Signature
    = let val goTy = applySubstTyConInTy (ctx, subst)
          fun goTypeScheme (U.TypeScheme (tvs, ty)) = U.TypeScheme (tvs, goTy ty)
          fun goTypeFunction (U.TypeFunction (tvs, ty)) = U.TypeFunction (tvs, goTy ty)
          fun goSig { valMap, tyConMap, strMap } = { valMap = Syntax.VIdMap.map (fn (tysc, ids) => (goTypeScheme tysc, ids)) valMap
                                                   , tyConMap = Syntax.TyConMap.map (fn { typeFunction, valEnv, admitsEquality, isRefOrArray } =>
                                                                                        { typeFunction = goTypeFunction typeFunction
                                                                                        , valEnv = Syntax.VIdMap.map (fn (tysc, ids) => (goTypeScheme tysc, ids)) valEnv
                                                                                        , admitsEquality = admitsEquality
                                                                                        , isRefOrArray = false (* TODO *)
                                                                                        }
                                                                                    ) tyConMap
                                                   , strMap = Syntax.StrIdMap.map (fn U.MkSignature s => U.MkSignature (goSig s)) strMap
                                                   }
      in goSig
      end


fun evalSignature(ctx : Context, env : SigEnv, S.BasicSigExp(span, specs))
    = List.foldl (fn (spec, s) => let val env' = addSignatureToEnv(env, #s s)
                                  in mergeQSignature(s, addSpec(ctx, env, spec))
                                  end) { s = emptySignature, bound = U.TyConMap.empty } specs
  | evalSignature(ctx, env, S.SigIdExp(span, sigid as Syntax.MkSigId name))
    = (case Syntax.SigIdMap.find(#sigMap env, sigid) of
           SOME s => s
         | NONE => emitError(ctx, [span], "unknown signature name '" ^ name ^ "'")
      )
  | evalSignature(ctx, env, S.TypeRealisationExp(span, sigexp, tyvars, longtycon, ty))
    = let val s = evalSignature(ctx, env, sigexp)
          val tyvars = List.map (fn tv => (tv, genTyVar(ctx, tv))) tyvars
          val ty = let val env = { valMap = #valMap env
                                 , tyConMap = #tyConMap env
                                 , strMap = #strMap env
                                 , sigMap = #sigMap env
                                 , boundTyVars = List.foldl Syntax.TyVarMap.insert' (#boundTyVars env) tyvars
                                 }
                   in evalTyInSig(ctx, env, ty)
                   end
          val tystr = let val S.MkQualified(strids, tycon as Syntax.MkTyCon name) = longtycon
                          val { tyConMap, ... } = lookupStr(ctx, #s s, span, strids)
                      in case Syntax.TyConMap.find(tyConMap, tycon) of
                             SOME tystr => tystr
                           | NONE => emitError(ctx, [span], "unknown type constructor '" ^ name ^ "'")
                      end
      in case #typeFunction tystr of
             U.TypeFunction (tyvars', U.TyCon(_, tyargs', tycon)) =>
             if List.length tyvars = List.length tyvars' then
                 if U.TyConMap.inDomain(#bound s, tycon) andalso ListPair.allEq (fn (tv, U.TyVar(_, tv')) => tv = tv' | _ => false) (tyvars', tyargs') then
                     let val subst = U.TyConMap.singleton(tycon, U.TypeFunction(List.map #2 tyvars, ty))
                     in { s = applySubstTyConInSig (ctx, subst) (#s s), bound = #1 (U.TyConMap.remove (#bound s, tycon)) }
                     end
                 else
                     emitError(ctx, [span], "type realisation against a rigid type")
             else
                 emitError(ctx, [span], "type realisation against a rigid type")
          | _ => emitError(ctx, [span], "type realisation against a rigid type")
      end
and addSpec(ctx : Context, env : SigEnv, S.ValDesc(span, descs)) : U.QSignature
    = { s = { valMap = List.foldl (fn ((vid, ty), valMap) => let val tvs = PostParsing.freeTyVarsInTy(Syntax.TyVarSet.empty, ty)
                                                                 val tvs = Syntax.TyVarSet.foldr (fn (tv, m) => Syntax.TyVarMap.insert(m, tv, genTyVar(ctx, tv))) Syntax.TyVarMap.empty tvs
                                                                 val env' = { valMap = #valMap env
                                                                            , tyConMap = #tyConMap env
                                                                            , strMap = #strMap env
                                                                            , sigMap = #sigMap env
                                                                            , boundTyVars = tvs
                                                                            }
                                                                 val ty = evalTyInSig(ctx, env', ty)
                                                             in Syntax.VIdMap.insert(valMap, vid, (U.TypeScheme(Syntax.TyVarMap.foldr (fn (tv, xs) => (tv, []) :: xs) [] tvs, ty), Syntax.ValueVariable))
                                                             end) Syntax.VIdMap.empty descs
            , tyConMap = Syntax.TyConMap.empty
            , strMap = Syntax.StrIdMap.empty
            }
      , bound = USyntax.TyConMap.empty
      }
  | addSpec(ctx, env, S.TypeDesc(span, descs))
    = List.foldl (fn ((tyvars, tycon), s) => let val tycon' = newTyCon(ctx, tycon)
                                                 val tyvars = List.map (fn tv => genTyVar(ctx, tv)) tyvars
                                                 val tystr = { typeFunction = U.TypeFunction(tyvars, U.TyCon(span, List.map (fn tv => U.TyVar(span, tv)) tyvars, tycon'))
                                                             , valEnv = Syntax.VIdMap.empty
                                                             , admitsEquality = false
                                                             , isRefOrArray = false
                                                             }
                                             in { s = { valMap = #valMap (#s s)
                                                      , tyConMap = Syntax.TyConMap.insert(#tyConMap (#s s), tycon, tystr)
                                                      , strMap = #strMap (#s s)
                                                      }
                                                , bound = USyntax.TyConMap.insert(#bound s, tycon', Syntax.MkQualified([], tycon))
                                                }
                                             end
                 )
                 { s = { valMap = Syntax.VIdMap.empty
                       , tyConMap = Syntax.TyConMap.empty
                       , strMap = Syntax.StrIdMap.empty
                       }
                 , bound = USyntax.TyConMap.empty
                 } descs
  | addSpec(ctx, env, S.EqtypeDesc(span, descs))
    = List.foldl (fn ((tyvars, tycon), s) => let val tycon' = newTyCon(ctx, tycon)
                                                 val tyvars = List.map (fn tv => genTyVar(ctx, tv)) tyvars
                                                 val tystr = { typeFunction = U.TypeFunction(tyvars, U.TyCon(span, List.map (fn tv => U.TyVar(span, tv)) tyvars, tycon'))
                                                             , valEnv = Syntax.VIdMap.empty
                                                             , admitsEquality = true
                                                             , isRefOrArray = false
                                                             }
                                             in { s = { valMap = #valMap (#s s)
                                                      , tyConMap = Syntax.TyConMap.insert(#tyConMap (#s s), tycon, tystr)
                                                      , strMap = #strMap (#s s)
                                                      }
                                                , bound = USyntax.TyConMap.insert(#bound s, tycon', Syntax.MkQualified([], tycon))
                                                }
                                             end
                 )
                 { s = { valMap = Syntax.VIdMap.empty
                       , tyConMap = Syntax.TyConMap.empty
                       , strMap = Syntax.StrIdMap.empty
                       }
                 , bound = USyntax.TyConMap.empty
                 } descs
  | addSpec(ctx, env, S.DatDesc(span, descs)) = raise Fail "DatDesc: not implemented yet"
  | addSpec(ctx, env, S.DatatypeRepSpec(span, tycon, longtycon)) = raise Fail "DatatypeRepSpec: not implemented yet"
  | addSpec(ctx, env, S.ExDesc(span, descs)) = raise Fail "ExDesc: not implemented yet"
  | addSpec(ctx, env, S.StrDesc(span, descs)) = let val strMap = List.foldl (fn ((strid, sigexp), m) => Syntax.StrIdMap.insert(m, strid, evalSignature(ctx, env, sigexp))) Syntax.StrIdMap.empty descs
                                                in { s = { valMap = Syntax.VIdMap.empty
                                                         , tyConMap = Syntax.TyConMap.empty
                                                         , strMap = Syntax.StrIdMap.map (fn { s, bound } => U.MkSignature s) strMap
                                                         }
                                                   , bound = Syntax.StrIdMap.foldli (fn (strid, { bound, ... }, map) => USyntax.TyConMap.unionWith #2 (map, USyntax.TyConMap.map (fn Syntax.MkQualified(strids, tycon) => Syntax.MkQualified(strid :: strids, tycon)) bound)) USyntax.TyConMap.empty strMap
                                                   }
                                                end
  | addSpec(ctx, env, S.Include(span, sigexp)) = evalSignature(ctx, env, sigexp)

fun typeCheckStrExp(ctx : Context, env : Env, S.StructExp(span, decs)) : U.QSignature * U.TypeStructure U.TyConMap.map * U.StrExp
    = let val ({ valMap, tyConMap, allTyConMap, strMap, ... }, decs) = typeCheckStrDecs(ctx, env, decs)
          val s = { s = { valMap = Syntax.VIdMap.map (fn (_, tysc, ids) => (tysc, ids)) valMap
                        , tyConMap = tyConMap
                        , strMap = Syntax.StrIdMap.map (fn (_, s) => U.MkSignature s) strMap
                        }
                  , bound = U.TyConMap.empty
                  }
          val e = U.StructExp { sourceSpan = span
                              , valMap = Syntax.VIdMap.map (fn (longvid, _, ids) => (longvid, ids)) valMap
                              , tyConMap = tyConMap
                              , strMap = Syntax.StrIdMap.map (fn (longstrid, _) => longstrid) strMap
                              }
      in (s, allTyConMap, if List.null decs then
                              e
                          else
                              U.LetInStrExp(span, decs, e)
         )
      end
  | typeCheckStrExp(ctx, env, S.StrIdExp(span, longstrid))
    = (case longstrid of
           Syntax.MkQualified([], strid) => (case Syntax.StrIdMap.find(#strMap env, strid) of
                                                 SOME (longstrid, s) => ({ s = s, bound = USyntax.TyConMap.empty }, USyntax.TyConMap.empty, U.StrIdExp(span, longstrid))
                                               | NONE => emitError(ctx, [span], "structure not found")
                                            )
         | Syntax.MkQualified(strid0 :: strids, strid') => (case Syntax.StrIdMap.find(#strMap env, strid0) of
                                                                SOME (U.MkLongStrId(strid0, strids0), s) => ({ s = s, bound = USyntax.TyConMap.empty }, USyntax.TyConMap.empty, U.StrIdExp(span, U.MkLongStrId(strid0, strids0 @ strids @ [strid'])))
                                                              | NONE => emitError(ctx, [span], "structure not found")
                                                           )
      )
  | typeCheckStrExp(ctx, env, S.TransparentConstraintExp(span, strexp, sigexp))
    = let val (s, allTyConMap, strexp) = typeCheckStrExp(ctx, env, strexp)
          val s' = evalSignature(ctx, envToSigEnv env, sigexp)
      in emitError(ctx, [span], "transparent constraint: not implemented yet") (* TODO *)
      end
  | typeCheckStrExp(ctx, env, S.OpaqueConstraintExp(span, strexp, sigexp))
    = let val (s, allTyConMap, strexp) = typeCheckStrExp(ctx, env, strexp)
          val s' = evalSignature(ctx, envToSigEnv env, sigexp)
      in emitError(ctx, [span], "opaque constraint: not implemented yet") (* TODO *)
      end
  | typeCheckStrExp(ctx, env, S.LetInStrExp(span, strdecs, strexp)) = let val (env', strdecs) = typeCheckStrDecs(ctx, env, strdecs)
                                                                          val (s, atc, strexp) = typeCheckStrExp(ctx, mergeEnv(env, env'), strexp)
                                                                      in (s, U.TyConMap.unionWith #2 (#allTyConMap env', atc), U.LetInStrExp(span, strdecs, strexp))
                                                                      end
and typeCheckStrDec(ctx : Context, env : Env, S.CoreDec(span, dec)) : Env * USyntax.StrDec list
    = let val (env', decs) = typeCheckDec(ctx, env, dec)
      in (env', List.map (fn dec => U.CoreDec(span, dec)) decs)
      end
  | typeCheckStrDec(ctx, env, S.StrBindDec(span, binds))
    = let val (strMap, allTyConMap, binds) = List.foldr (fn ((strid, strexp), (strMap, allTyConMap, binds)) => let val ({ s, bound }, tc, strexp) = typeCheckStrExp(ctx, env, strexp)
                                                                                                                   val strid' = newStrId(ctx, strid)
                                                                                                                   (* TODO: unpack variables *)
                                                                                                               in (S.StrIdMap.insert(strMap, strid, (U.MkLongStrId(strid', []), s)), USyntax.TyConMap.unionWith #2 (allTyConMap, tc), (strid', strexp, s) :: binds)
                                                                                                               end
                                                 ) (Syntax.StrIdMap.empty, USyntax.TyConMap.empty, []) binds
          val env' = { valMap = Syntax.VIdMap.empty
                     , tyConMap = Syntax.TyConMap.empty
                     , allTyConMap = allTyConMap
                     , strMap = strMap
                     , sigMap = Syntax.SigIdMap.empty
                     , boundTyVars = Syntax.TyVarMap.empty
                     }
      in (env', List.map (fn (strid, strexp, s) => U.StrBindDec(span, strid, strexp, s)) binds)
      end
  | typeCheckStrDec(ctx, env, S.LocalStrDec(span, decs1, decs2))
    = let val (env', decs1) = typeCheckStrDecs(ctx, env, decs1)
          val (env'', decs2) = typeCheckStrDecs(ctx, mergeEnv(env, env'), decs2)
          val env'' = { valMap = #valMap env''
                      , tyConMap = #tyConMap env''
                      , allTyConMap = USyntax.TyConMap.unionWith #2 (#allTyConMap env', #allTyConMap env'')
                      , strMap = #strMap env''
                      , sigMap = #sigMap env''
                      , boundTyVars = #boundTyVars env''
                      }
      in (env'', case decs1 @ decs2 of
                     decs as [] => decs
                   | decs as [_] => decs
                   | decs => [U.GroupStrDec(span, decs)]
         )
      end
and typeCheckStrDecs(ctx : Context, env : Env, []) = (emptyEnv, [])
  | typeCheckStrDecs(ctx, env, dec :: decs) = let val (env', dec) = typeCheckStrDec(ctx, env, dec)
                                                  val (env'', decs) = typeCheckStrDecs(ctx, mergeEnv(env, env'), decs)
                                              in (mergeEnv(env', env''), dec @ decs)
                                              end

fun typeCheckTopDec(ctx, env, S.StrDec strdec) = let val (env', strdec) = typeCheckStrDec(ctx, env, strdec)
                                                 in (env', List.map U.StrDec strdec)
                                                 end
  | typeCheckTopDec(ctx, env, S.SigDec binds) = let val sigenv = envToSigEnv env
                                                    val sigMap = List.foldl (fn ((sigid, sigexp), m) => Syntax.SigIdMap.insert(m, sigid, evalSignature(ctx, sigenv, sigexp))) (#sigMap env) binds
                                                    val env = { valMap = #valMap env
                                                              , tyConMap = #tyConMap env
                                                              , allTyConMap = #allTyConMap env
                                                              , strMap = #strMap env
                                                              , sigMap = sigMap
                                                              , boundTyVars = #boundTyVars env
                                                              }
                                                in (env, [])
                                                end
fun typeCheckTopDecs(ctx, env, []) = (emptyEnv, [])
  | typeCheckTopDecs(ctx, env, dec :: decs) = let val (env', dec) = typeCheckTopDec(ctx, env, dec)
                                                  val (env'', decs) = typeCheckTopDecs(ctx, mergeEnv(env, env'), decs)
                                              in (mergeEnv(env', env''), dec @ decs)
                                              end
(* typeCheckClosedTopDecs : ProgramContext * Env * USyntax.TopDec list -> (* created environment *) Env * USyntax.TopDec list *)
fun typeCheckClosedTopDecs(pctx : ProgramContext, env, topdecs) : Env * USyntax.TopDec list
    = let val ctx = { nextTyVar = #nextTyVar pctx
                    , nextVId = #nextVId pctx
                    , tyVarConstraints = ref USyntax.TyVarMap.empty
                    , tyVarSubst = ref USyntax.TyVarMap.empty
                    }
          val (env', decs) = typeCheckTopDecs(ctx, env, topdecs)
          val subst = !(#tyVarSubst ctx)
          val tvc = !(#tyVarConstraints ctx)
          val mapTyInTopDec = #doTopDec (USyntax.mapTy (ctx, subst, false))
          val decs = List.map mapTyInTopDec decs
          val decs = applyDefaultTypes(ctx, tvc, decs)
      in (env', decs)
      end
(* typeCheckProgram : ProgramContext * Env * ((Syntax.Dec Syntax.TopDec) list) list -> Env * USyntax.TopDec list *)
fun typeCheckProgram(ctx, env, [] : ((Syntax.Dec Syntax.TopDec) list) list) : Env * (USyntax.TopDec list) list = (emptyEnv, [])
  | typeCheckProgram(ctx, env, topdec :: topdecs) = let val (env', topdec') = typeCheckClosedTopDecs(ctx, env, topdec)
                                                        val (env'', topdecs') = typeCheckProgram(ctx, mergeEnv(env, env'), topdecs)
                                                    in (mergeEnv(env', env''), topdec' :: topdecs')
                                                    end
end (* local *)
end (* structure Typing *)
