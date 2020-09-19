structure Typing = struct

datatype TyStr = TyStr of USyntax.TypeFcn * USyntax.ValEnv

datatype Env = MkEnv of { tyMap : TyStr Syntax.TyConMap.map
                        , valMap : (USyntax.TypeScheme * Syntax.IdStatus) USyntax.VIdMap.map
                        , strMap : Env Syntax.StrIdMap.map
                        }

type Subst = USyntax.Ty USyntax.TyVarMap.map

fun freeTyVarsInTypeScheme(bound, USyntax.TypeScheme(tyvars, ty)) = USyntax.freeTyVarsInTy(USyntax.TyVarSet.addList(bound, List.map #1 tyvars), ty)
fun freeTyVarsInEnv(bound, MkEnv { tyMap = tyMap, valMap = valMap, strMap = strMap })
    = let val valMapSet = USyntax.VIdMap.foldl (fn ((tysc, _), set) => USyntax.TyVarSet.union(set, freeTyVarsInTypeScheme(bound, tysc))) USyntax.TyVarSet.empty valMap
          (* TODO: tyMap? *)
      in Syntax.StrIdMap.foldl (fn (env, set) => USyntax.TyVarSet.union(set, freeTyVarsInEnv(bound, env))) valMapSet strMap
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
fun freeTyVarsInUnaryConstraint(bound, unaryConstraint)
    = (case unaryConstraint of
           USyntax.HasField{fieldTy = fieldTy, ...} => USyntax.freeTyVarsInTy(bound, fieldTy)
         | USyntax.IsEqType     => USyntax.TyVarSet.empty
         | USyntax.IsIntegral   => USyntax.TyVarSet.empty
         | USyntax.IsSignedReal => USyntax.TyVarSet.empty
         | USyntax.IsRing       => USyntax.TyVarSet.empty
         | USyntax.IsField      => USyntax.TyVarSet.empty
         | USyntax.IsSigned     => USyntax.TyVarSet.empty
         | USyntax.IsOrdered    => USyntax.TyVarSet.empty
      )

type Context = { nextTyVar : int ref
               , nextVId : int ref
               , nextTyCon : int ref
               , tyVarConstraints : ((USyntax.UnaryConstraint list) USyntax.TyVarMap.map) ref
               , tyVarSubst : Subst ref
               , topDecs : (USyntax.TopDec list) ref
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
(* Env * USyntax.LongVId -> (USyntax.TypeScheme * Syntax.IdStatus) option *)
fun lookupLongVIdInEnv(env, USyntax.MkLongVId(strid, vid))
    = let val MkEnv strEnv = lookupStr(env, strid)
      in USyntax.VIdMap.find(#valMap strEnv, vid)
      end

(* getConstructedType : USyntax.Ty -> USyntax.LongTyCon *)
fun getConstructedType(USyntax.TyVar _) = raise TypeError "getConstructedType: got a type variable"
  | getConstructedType(USyntax.RecordType _) = raise TypeError "getConstructedType: got a record"
  | getConstructedType(USyntax.TyCon(tyargs, longtycon)) = longtycon
  | getConstructedType(USyntax.FnType(_, t)) = getConstructedType t

(* isSoleConstructor : Env * USyntax.LongVId -> bool *)
fun isSoleConstructor(env : Env, longvid: USyntax.LongVId) =
    (case lookupLongVIdInEnv(env, longvid) of
         NONE => false (* probably an error *)
       | SOME (USyntax.TypeScheme(_, ty), Syntax.ValueConstructor) =>
         let val USyntax.MkLongTyCon(Syntax.MkLongTyCon(strids, tycon), x) = getConstructedType ty
             val TyStr (_, valenv) = lookupTyConInEnv(lookupStr(env, strids), tycon)
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
  | isNonexpansive(env, USyntax.RecordExp fields) = List.all (fn (_, e) => isNonexpansive(env, e)) fields
  | isNonexpansive(env, USyntax.TypedExp(e, _)) = isNonexpansive(env, e)
  | isNonexpansive(env, USyntax.AppExp(conexp, e)) = isConexp(env, conexp) andalso isNonexpansive(env, e)
  | isNonexpansive(env, USyntax.FnExp _) = true
  | isNonexpansive(env, USyntax.ProjectionExp _) = true
  | isNonexpansive(env, _) = false
and isConexp(env : Env, USyntax.TypedExp(e, _)) = isConexp(env, e)
  | isConexp(env, USyntax.VarExp(USyntax.MkLongVId([], USyntax.MkVId("ref", 0)), _)) = false
  | isConexp(env, USyntax.VarExp(_, Syntax.ValueVariable)) = false
  | isConexp(env, USyntax.VarExp(_, Syntax.ValueConstructor)) = true
  | isConexp(env, USyntax.VarExp(_, Syntax.ExceptionConstructor)) = true
  | isConexp(env, USyntax.InstantiatedVarExp(_, Syntax.ValueVariable, _)) = false
  | isConexp(env, USyntax.InstantiatedVarExp(_, Syntax.ValueConstructor, _)) = true
  | isConexp(env, USyntax.InstantiatedVarExp(_, Syntax.ExceptionConstructor, _)) = true
  | isConexp(env, _) = false

(* isExhaustive : Env * USyntax.Pat -> bool *)
fun isExhaustive(env : Env, USyntax.WildcardPat) = true
  | isExhaustive(env, USyntax.SConPat _) = false
  | isExhaustive(env, USyntax.VarPat _) = true
  | isExhaustive(env, USyntax.RecordPat(row, _)) = List.all (fn (_, e) => isExhaustive(env, e)) row
  | isExhaustive(env, USyntax.ConPat(longvid, NONE)) = isSoleConstructor(env, longvid)
  | isExhaustive(env, USyntax.ConPat(longvid, SOME innerPat)) = isSoleConstructor(env, longvid) andalso isExhaustive(env, innerPat)
  | isExhaustive(env, USyntax.InstantiatedConPat(longvid, NONE, tyargs)) = isSoleConstructor(env, longvid)
  | isExhaustive(env, USyntax.InstantiatedConPat(longvid, SOME innerPat, tyargs)) = isSoleConstructor(env, longvid) andalso isExhaustive(env, innerPat)
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
            , valMap = USyntax.VIdMap.empty
            , strMap = Syntax.StrIdMap.empty
            }

fun newContext() : Context
    = { nextTyVar = ref 100
      , nextVId = ref 100
      , nextTyCon = ref 100
      , tyVarConstraints = ref USyntax.TyVarMap.empty
      , tyVarSubst = ref USyntax.TyVarMap.empty
      , topDecs = ref []
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
          | substTy (RecordType fields) = RecordType (Syntax.mapRecordRow substTy fields)
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
          | substTy (RecordType fields) = RecordType (Syntax.mapRecordRow substTy fields)
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
                    , valMap = USyntax.VIdMap.map (fn (tysc, ids) => (substTypeScheme(tysc), ids)) valMap
                    , strMap = Syntax.StrIdMap.map substEnv strMap
                    }
    in substEnv
    end

(* instantiate : Context * TypeScheme -> Ty * Ty list *)
fun instantiate(ctx, TypeScheme(vars, ty))
    = let val (subst, tyargs) = List.foldl (fn ((v, preds), (set, rest)) =>
                                               let val tv = freshTyVar(ctx)
                                                   val tyarg = TyVar(tv)
                                               in List.app (fn pred => addTyVarConstraint(ctx, tv, pred)) preds
                                                ; (USyntax.TyVarMap.insert(set, v, tyarg), tyarg :: rest)
                                               end
                                           ) (USyntax.TyVarMap.empty, []) vars
      in (applySubstTy subst ty, List.rev tyargs)
      end

(* mergeEnv : Env * Env -> Env *)
fun mergeEnv(MkEnv env1, MkEnv env2) = MkEnv { tyMap = Syntax.TyConMap.unionWith #2 (#tyMap env1, #tyMap env2)
                                             , valMap = USyntax.VIdMap.unionWith #2 (#valMap env1, #valMap env2)
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
fun typeCheckExp(ctx : Context, env : Env, exp as SConExp(scon)) : USyntax.Ty * USyntax.Exp
    = let val ty = case scon of (* TODO: overloaded literals *)
                       Syntax.IntegerConstant x   => primTy_int
                     | Syntax.WordConstant x      => primTy_word
                     | Syntax.RealConstant x      => primTy_real
                     | Syntax.StringConstant x    => primTy_string
                     | Syntax.CharacterConstant x => primTy_char
      in (ty, exp)
      end
  | typeCheckExp(ctx, env, exp as VarExp(longvid as USyntax.MkLongVId(_, USyntax.MkVId(name, _)), idstatus))
    = (case lookupLongVIdInEnv(env, longvid) of
           SOME (tysc, ids) => let val (ty, tyargs) = instantiate(ctx, tysc)
                               in (ty, InstantiatedVarExp(longvid, idstatus, tyargs))
                               end
         | NONE => raise NameError("unknown value name " ^ name)
      )
  | typeCheckExp(ctx, env, exp as InstantiatedVarExp(longvid as USyntax.MkLongVId(_, USyntax.MkVId(name, _)), idstatus, tyargs)) (* should not reach here *)
    = let val ty = case lookupLongVIdInEnv(env, longvid) of
                       SOME (TypeScheme(vars, ty), ids) =>
                       let val subst = ListPair.foldlEq (fn ((var, constraints), tyarg, set) =>
                                                            ( List.app (fn c => addConstraint(ctx, UnaryConstraint(tyarg, c))) constraints
                                                            ; USyntax.TyVarMap.insert(set, var, tyarg)
                                                            )
                                                        ) USyntax.TyVarMap.empty (vars, tyargs)
                       in applySubstTy subst ty
                       end
                     | NONE => raise NameError("unknown value name " ^ name)
      in (ty, exp)
      end
  | typeCheckExp(ctx, env, RecordExp(row))
    = let val (rowTy, row') = typeCheckExpRow(ctx, env, row)
      in (RecordType(rowTy), RecordExp(row'))
      end
  | typeCheckExp(ctx, env, LetInExp(decls, innerExp))
    = let val (env', decls') = typeCheckDecl(ctx, env, decls)
          val (ty, innerExp') = typeCheckExp(ctx, mergeEnv(env, env'), innerExp)
      in (ty, LetInExp(decls', innerExp'))
      end
  | typeCheckExp(ctx, env, AppExp(f, x))
          (* f: s -> t, x: s *)
    = let val (funcTy, f') = typeCheckExp(ctx, env, f)
          val (argTy, x') = typeCheckExp(ctx, env, x)
          val retTy = TyVar(freshTyVar(ctx))
      in addConstraint(ctx, EqConstr(funcTy, FnType(argTy, retTy))) (* funcTy = (argTy -> retTy) *)
       ; (retTy, AppExp(f', x'))
      end
  | typeCheckExp(ctx, env, TypedExp(exp, ty))
    = let val (expTy, exp') = typeCheckExp(ctx, env, exp)
      in addConstraint(ctx, EqConstr(expTy, ty)) (* ety = ty *)
       ; (ty, TypedExp(exp', ty))
      end
  | typeCheckExp(ctx, env, HandleExp(e, handlers))
          (* e: t, handlers: exn -> t *)
    = raise Fail "handle expression not implemented yet"
  | typeCheckExp(ctx, env, RaiseExp(exp))
    = let val (expTy, exp') = typeCheckExp(ctx, env, exp)
      in addConstraint(ctx, EqConstr(expTy, primTy_exn)) (* expTy = exn *)
       ; (TyVar(freshTyVar(ctx)), RaiseExp(exp'))
      end
  | typeCheckExp(ctx, env, IfThenElseExp(cond, thenPart, elsePart))
    = let val (condTy, cond') = typeCheckExp(ctx, env, cond)
          val (thenTy, thenPart') = typeCheckExp(ctx, env, thenPart)
          val (elseTy, elsePart') = typeCheckExp(ctx, env, elsePart)
      in addConstraint(ctx, EqConstr(condTy, primTy_bool)) (* condTy = bool *)
       ; addConstraint(ctx, EqConstr(thenTy, elseTy)) (* thenTy = elseTy *)
       ; (thenTy, IfThenElseExp(cond', thenPart', elsePart'))
      end
  | typeCheckExp(ctx, env, CaseExp(exp, matches))
    = let val (expTy, exp') = typeCheckExp(ctx, env, exp)
          val (patTy, retTy, matches') = typeCheckMatch(ctx, env, matches)
      in addConstraint(ctx, EqConstr(expTy, patTy))
       ; (retTy, CaseExp(exp', matches'))
      end
  | typeCheckExp(ctx, env, FnExp(vid, argTy, body))
    = let val env' = mergeEnv(env, MkEnv { tyMap = Syntax.TyConMap.empty
                                         , valMap = USyntax.VIdMap.insert(USyntax.VIdMap.empty, vid, (TypeScheme([], argTy), Syntax.ValueVariable))
                                         , strMap = Syntax.StrIdMap.empty
                                         }
                             )
          val (retTy, body') = typeCheckExp(ctx, env', body)
      in (USyntax.FnType(argTy, retTy), FnExp(vid, argTy, body'))
      end
  | typeCheckExp(ctx, env, exp as ProjectionExp { label = label, recordTy = recordTy, fieldTy = fieldTy })
    = ( addConstraint(ctx, UnaryConstraint(recordTy, HasField { label = label, fieldTy = fieldTy }))
      ; (USyntax.FnType(recordTy, fieldTy), exp)
      )
(* typeCheckDecl : Context * Env * Dec list -> Env * Dec list *)
and typeCheckDecl(ctx, env, nil) : Env * Dec list = (emptyEnv, nil)
  | typeCheckDecl(ctx, env, decl :: decls)
    = (case decl of
           ValDec(tyvarseq, valbinds, _) =>
           let val MkEnv { valMap = valMap, tyMap = tyMap, strMap = strMap } = env
               val valbinds' = List.map (fn valbind => typeCheckValBind(ctx, env, valbind)) valbinds
               val tvc = !(#tyVarConstraints ctx)
               val subst = !(#tyVarSubst ctx)
               val env' = applySubstEnv subst env
               val tyVars_env = freeTyVarsInEnv(TyVarSet.empty, env)
               fun generalize((valbind, valEnv, false), (valbinds, valEnvRest)) = (valbind :: valbinds, USyntax.VIdMap.unionWith #2 (USyntax.VIdMap.map (fn ty => TypeScheme([], ty)) valEnv, valEnvRest))
                 | generalize((PatBind(pat, exp), valEnv, true), (valbinds, valEnvRest)) =
                   let val valEnv' = USyntax.VIdMap.mapi (fn (vid,ty) =>
                                                             let val ty' = applySubstTy subst ty
                                                                 val tyVars_ty = freeTyVarsInTy(TyVarSet.empty, ty')
                                                                 fun isGeneralizable(tv: TyVar) = case USyntax.TyVarMap.find(tvc, tv) of
                                                                                                      NONE => true
                                                                                                    | SOME tvs => false
                                                                 val tyVars = TyVarSet.difference(TyVarSet.filter isGeneralizable tyVars_ty, tyVars_env) (* TODO: Allow equality constraint *)
                                                                 val tysc = TypeScheme(List.map (fn x => (x, [])) (TyVarSet.listItems tyVars), ty')
                                                             in tysc
                                                             end) valEnv
                       val valEnv'L = USyntax.VIdMap.listItemsi valEnv'
                       val allPoly = List.all (fn (_, TypeScheme(tv, _)) => not (List.null tv)) valEnv'L (* all bindings are generalized? *)
                       fun polyPart [] = []
                         | polyPart ((vid, TypeScheme([], _)) :: rest) = polyPart rest
                         | polyPart ((vid, tysc) :: rest) = PolyVarBind(vid, tysc, USyntax.CaseExp(exp, [(USyntax.filterVarsInPat (fn x => x = vid) pat, USyntax.VarExp(USyntax.MkLongVId([], vid), Syntax.ValueVariable))])) :: polyPart rest
                       fun isMonoVar vid = case USyntax.VIdMap.find(valEnv', vid) of
                                               NONE => raise TypeError "isMonoVar: internal error"
                                             | SOME (TypeScheme([], _)) => true
                                             | SOME (TypeScheme(_ :: _, _)) => false
                       val valbind' = if allPoly then
                                          polyPart valEnv'L
                                      else
                                          PatBind(USyntax.filterVarsInPat isMonoVar pat, exp) :: polyPart valEnv'L
                   in (valbind' @ valbinds, USyntax.VIdMap.unionWith #2 (valEnv', valEnvRest))
                   end
                 | generalize((PolyVarBind(_, _, _), valEnv, _), (valbinds, valEnvRest)) = raise TypeError "unexpected PolyVarBind"
               val (valbinds'', valEnv'') = List.foldr generalize ([], USyntax.VIdMap.empty) valbinds'
               val valEnv''' = USyntax.VIdMap.map (fn tysc => (tysc, Syntax.ValueVariable)) valEnv''
               val env' = MkEnv { valMap = USyntax.VIdMap.unionWith #2 (valMap, valEnv''')
                                , tyMap = tyMap
                                , strMap = strMap
                                }
               val (MkEnv restEnv, decls') = typeCheckDecl(ctx, env', decls)
               val env'' = MkEnv { valMap = USyntax.VIdMap.unionWith #2 (valEnv''', #valMap restEnv)
                                 , tyMap = #tyMap restEnv
                                 , strMap = #strMap restEnv
                                 }
           in (env'', ValDec([], valbinds'', valEnv''') :: decls')
           end
         | RecValDec(tyvarseq, valbinds, _) => raise Fail "let-in: val rec: not impl"
      )
(* typeCheckValBind : Context * Env * ValBind -> ValBind * USyntax.Ty USyntax.VIdMap.map * bool *)
and typeCheckValBind(ctx, env, PatBind(pat, exp))
    = let val (patTy, newValEnv, pat') = typeCheckPat(ctx, env, pat)
          val (expTy, exp') = typeCheckExp(ctx, env, exp)
          val () = addConstraint(ctx, EqConstr(patTy, expTy))
          val generalizable = isExhaustive(env, pat) andalso isNonexpansive(env, exp)
      in (PatBind(pat', exp'), newValEnv, generalizable)
      end
  | typeCheckValBind(ctx, env, PolyVarBind(vid, tysc, exp))
    = raise TypeError "unexpected PolyVarBind"
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
  | typeCheckMatch(ctx, env, nil) = raise TypeError "invalid syntax tree: match is empty"
and typeCheckMatchBranch(ctx : Context, env as MkEnv env' : Env, pat : Pat, exp : Exp) : USyntax.Ty * USyntax.Ty * Pat * Exp
    = let val (patTy, vars, pat') = typeCheckPat(ctx, env, pat)
          val env'' = MkEnv { tyMap = #tyMap env'
                            , valMap = USyntax.VIdMap.unionWith #2 (#valMap env', USyntax.VIdMap.map (fn ty => (TypeScheme([], ty), Syntax.ValueVariable)) vars)
                            , strMap = #strMap env'
                            }
          val (expTy, exp') = typeCheckExp(ctx, env'', exp)
      in (patTy, expTy, pat', exp')
      end
 (* typeCheckPat : Context * Env * Pat -> USyntax.Ty * USyntax.Ty USyntax.VIdMap.map * Pat *)
and typeCheckPat(ctx, env, pat as WildcardPat) : USyntax.Ty * USyntax.Ty USyntax.VIdMap.map * Pat
    = let val ty = TyVar(freshTyVar(ctx))
      in (ty, USyntax.VIdMap.empty, pat)
      end
  | typeCheckPat(ctx, env, pat as SConPat scon)
    = (case scon of
           Syntax.IntegerConstant(_)   => (primTy_int, USyntax.VIdMap.empty, pat)
         | Syntax.WordConstant(_)      => (primTy_word, USyntax.VIdMap.empty, pat)
         | Syntax.RealConstant(_)      => raise Syntax.SyntaxError "No real constant may occur in a pattern"
         | Syntax.StringConstant(_)    => (primTy_string, USyntax.VIdMap.empty, pat)
         | Syntax.CharacterConstant(_) => (primTy_char, USyntax.VIdMap.empty, pat)
      )
  | typeCheckPat(ctx, MkEnv env, pat as VarPat(vid, ty))
    = (case USyntax.VIdMap.find(#valMap env, vid) of
           SOME (tysc, Syntax.ValueConstructor) => raise TypeError "VarPat: invalid pattern"
         | SOME (tysc, Syntax.ExceptionConstructor) => raise TypeError "VarPat: invalid pattern"
         | SOME (_, Syntax.ValueVariable) => (* shadowing *) (ty, USyntax.VIdMap.insert(USyntax.VIdMap.empty, vid, ty), pat)
         | NONE => (ty, USyntax.VIdMap.insert(USyntax.VIdMap.empty, vid, ty), pat)
      )
  | typeCheckPat(ctx, env, RecordPat(row, wildcard))
    = let val (rowTy, vars, row') = typeCheckPatRow(ctx, env, row)
      in if wildcard then
             let val recordTy = TyVar(freshTyVar(ctx))
                 fun oneField(label, ty) = addConstraint(ctx, UnaryConstraint(recordTy, HasField { label = label, fieldTy = ty }))
             in List.app oneField rowTy
              ; (recordTy, vars, RecordPat(row', wildcard))
             end
         else
             (RecordType(rowTy), vars, RecordPat(row', wildcard))
      end
  | typeCheckPat(ctx, env, ConPat(longvid, opt_innerPat))
    = (case lookupLongVIdInEnv(env, longvid) of
           SOME (tysc, idstatus) =>
           (if idstatus = Syntax.ValueConstructor orelse idstatus = Syntax.ExceptionConstructor then
                let val (ty, tyargs) = instantiate(ctx, tysc)
                in case opt_innerPat of
                       NONE => (ty, USyntax.VIdMap.empty, InstantiatedConPat(longvid, NONE, tyargs))
                     | SOME innerPat =>
                       (case ty of
                            USyntax.FnType(argTy, resultTy) =>
                            let val (argTy', innerVars, innerPat') = typeCheckPat(ctx, env, innerPat)
                            in addConstraint(ctx, EqConstr(argTy, argTy'))
                             ; (resultTy, innerVars, InstantiatedConPat(longvid, SOME innerPat', tyargs))
                            end
                          | _ => raise TypeError "invalid pattern"
                       )
                end
            else (* idstatus = Syntax.ValueVariable *)
                raise TypeError "invalid pattern"
           )
         | NONE => raise TypeError "invalid pattern"
      )
  | typeCheckPat(ctx, env, pat as InstantiatedConPat(longvid, opt_innerPat, tyargs)) (* should not reach here *)
    = (case lookupLongVIdInEnv(env, longvid) of
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
                                             USyntax.FnType(argTy, resultTy) => let val (argTy', innerVars, innerPat') = typeCheckPat(ctx, env, innerPat)
                                                                                in addConstraint(ctx, EqConstr(argTy, argTy'))
                                                                                 ; (resultTy, innerVars, InstantiatedConPat(longvid, SOME innerPat', tyargs))
                                                                                end
                                           | _ => raise TypeError "invalid pattern"
                                        )
                end
            else (* idstatus = Syntax.ValueVariable *)
                raise TypeError "invalid pattern"
           )
         | NONE => raise TypeError "invalid pattern"
      )
  | typeCheckPat(ctx, env, pat as TypedPat(WildcardPat, ty))
    = (ty, USyntax.VIdMap.empty, pat)
  | typeCheckPat(ctx, env, TypedPat(pat, ty))
    = let val (inferredTy, vars, pat') = typeCheckPat(ctx, env, pat)
      in addConstraint(ctx, EqConstr(ty, inferredTy))
       ; (ty, vars, TypedPat(pat', ty))
      end
  | typeCheckPat(ctx, env, LayeredPat(vid, ty, pat))
    = let val (inferredTy, vars, pat') = typeCheckPat(ctx, env, pat)
      in case USyntax.VIdMap.find(vars, vid) of
             NONE => ( addConstraint(ctx, EqConstr(ty, inferredTy))
                     ; (ty, USyntax.VIdMap.insert(vars, vid, ty), LayeredPat(vid, ty, pat'))
                     )
           | SOME _ => raise TypeError "trying to bind the same identifier twice"
      end
 (* typeCheckPatRow : Context * Env * (Label * Pat) list -> (Label * Syntax.Ty) list * Syntax.Ty USyntax.VIdMap.map * (Label * Pat) list *)
and typeCheckPatRow(ctx, env, row)
    = let fun oneField((label, pat), (row, vars, rest))
              = let val (ty, vars', pat') = typeCheckPat(ctx, env, pat)
                in ((label, ty) :: row, USyntax.VIdMap.unionWith (fn _ => raise TypeError "trying to bind the same identifier twice") (vars, vars'), (label, pat') :: rest)
                end
      in List.foldl oneField ([], USyntax.VIdMap.empty, []) row (* TODO: Is this right? *)
      end

(* typeCheckExp : Context * Env * USyntax.Exp -> (UnaryConstraint list) USyntax.TyVarMap.map * USyntax.Ty * USyntax.Exp *)
fun typeCheckExp_(ctx, env, exp) = let val (ty, exp') = typeCheckExp(ctx, env, exp)
                                       val subst = !(#tyVarSubst ctx)
                                       val tvc = !(#tyVarConstraints ctx)
                                       val applySubst = applySubstTy subst
                                   in (tvc, applySubst ty, USyntax.mapTyInExp applySubst exp')
                                   end

(* typeCheckProgram : Context * Env * USyntax.Dec list -> Env * (UnaryConstraint list) USyntax.TyVarMap.map * USyntax.Dec list *)
fun typeCheckProgram(ctx, env, decls) = let val (env', decls') = typeCheckDecl(ctx, env, decls)
                                            val subst = !(#tyVarSubst ctx)
                                            val tvc = !(#tyVarConstraints ctx)
                                            val applySubst = applySubstTy subst
                                            val topDecs = !(#topDecs ctx)
                                        in (env', tvc, (topDecs, List.map (USyntax.mapTyInDec applySubst) decls'))
                                        end

(* pretty printing *)
structure PrettyPrint = struct
fun print_Env (MkEnv { tyMap = tyMap, valMap = valMap, strMap = strMap }) = "MkEnv{tyMap=" ^ Syntax.print_TyConMap (fn (TyStr _) => "TyStr _") tyMap ^ ",valMap=" ^ USyntax.print_VIdMap (Syntax.print_pair (USyntax.print_TypeScheme, Syntax.print_IdStatus)) valMap ^ ",strMap=" ^ Syntax.print_StrIdMap print_Env strMap ^ "}"
end (* structure PrettyPrint *)
open PrettyPrint

(* applyDefaultTypes : (UnaryConstraint list) USyntax.TyVarMap.map * USyntax.Dec list -> USyntax.Dec list *)
fun applyDefaultTypes(tvc, decs) =
    let fun doInt [] = primTy_int
          | doInt (USyntax.HasField{...} :: xs) = raise TypeError "invalid record syntax for int"
          | doInt (USyntax.IsEqType :: xs) = doInt xs
          | doInt (USyntax.IsIntegral :: xs) = doInt xs
          | doInt (USyntax.IsSignedReal :: xs) = doInt xs
          | doInt (USyntax.IsRing :: xs) = doInt xs
          | doInt (USyntax.IsField :: xs) = raise TypeError "cannot apply / operator for int"
          | doInt (USyntax.IsSigned :: xs) = doInt xs
          | doInt (USyntax.IsOrdered :: xs) = doInt xs
        fun doReal [] = primTy_real
          | doReal (USyntax.HasField{...} :: xs) = raise TypeError "invalid record syntax for real"
          | doReal (USyntax.IsEqType :: xs) = raise TypeError "real does not admit equality"
          | doReal (USyntax.IsIntegral :: xs) = raise TypeError "div, mod is invalid for real"
          | doReal (USyntax.IsSignedReal :: xs) = doReal xs
          | doReal (USyntax.IsRing :: xs) = doReal xs
          | doReal (USyntax.IsField :: xs) = doReal xs
          | doReal (USyntax.IsSigned :: xs) = doReal xs
          | doReal (USyntax.IsOrdered :: xs) = doReal xs
        fun doIntOrReal [] = primTy_int
          | doIntOrReal (USyntax.HasField{...} :: _) = raise TypeError "unresolved flex record"
          | doIntOrReal (USyntax.IsEqType :: xs) = doInt xs
          | doIntOrReal (USyntax.IsIntegral :: xs) = doInt xs
          | doIntOrReal (USyntax.IsSignedReal :: xs) = doIntOrReal xs
          | doIntOrReal (USyntax.IsRing :: xs) = doIntOrReal xs
          | doIntOrReal (USyntax.IsField :: xs) = doReal xs
          | doIntOrReal (USyntax.IsSigned :: xs) = doIntOrReal xs
          | doIntOrReal (USyntax.IsOrdered :: xs) = doIntOrReal xs
        fun defaultTyForConstraints(eq, []) = primTy_unit
          | defaultTyForConstraints(eq, USyntax.HasField{...} :: _) = raise TypeError "unresolved flex record"
          | defaultTyForConstraints(eq, USyntax.IsEqType :: xs) = defaultTyForConstraints(true, xs)
          | defaultTyForConstraints(eq, USyntax.IsIntegral :: xs) = doInt xs
          | defaultTyForConstraints(eq, USyntax.IsSignedReal :: xs) = if eq then doInt xs else doIntOrReal xs
          | defaultTyForConstraints(eq, USyntax.IsRing :: xs) = if eq then doInt xs else doIntOrReal xs
          | defaultTyForConstraints(eq, USyntax.IsField :: xs) = if eq then raise TypeError "real does not admit equality" else doReal xs
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
