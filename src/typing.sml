(*
 * Copyright (c) 2022 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure Typing = struct

type TyNameAttr = { arity : int
                  , admitsEquality : bool
                  , overloadClass : Syntax.OverloadClass option
                  }

type ('val,'str) Env' = { valMap : (TypedSyntax.TypeScheme * Syntax.ValueConstructorInfo Syntax.IdStatus * 'val) Syntax.VIdMap.map
                        , tyConMap : TypedSyntax.TypeStructure Syntax.TyConMap.map
                        , tyNameMap : TyNameAttr TypedSyntax.TyNameMap.map
                        , strMap : (TypedSyntax.Signature * 'str) Syntax.StrIdMap.map
                        , sigMap : TypedSyntax.QSignature Syntax.SigIdMap.map
                        , funMap : (TypedSyntax.FunSig * TypedSyntax.FunId) Syntax.FunIdMap.map
                        , boundTyVars : TypedSyntax.TyVar Syntax.TyVarMap.map
                        }
type Env = (TypedSyntax.LongVId, TypedSyntax.LongStrId) Env'
type SigEnv = (unit, unit) Env'

val emptyEnv : Env
    = { valMap = Syntax.VIdMap.empty
      , tyConMap = Syntax.TyConMap.empty
      , tyNameMap = TypedSyntax.TyNameMap.empty
      , strMap = Syntax.StrIdMap.empty
      , sigMap = Syntax.SigIdMap.empty
      , funMap = Syntax.FunIdMap.empty
      , boundTyVars = Syntax.TyVarMap.empty
      }

(* mergeEnv : Env * Env -> Env *)
fun mergeEnv(env1 : ('val,'str) Env', env2 : ('val,'str) Env') : ('val,'str) Env'
    = { valMap = Syntax.VIdMap.unionWith #2 (#valMap env1, #valMap env2)
      , tyConMap = Syntax.TyConMap.unionWith #2 (#tyConMap env1, #tyConMap env2)
      , tyNameMap = TypedSyntax.TyNameMap.unionWith #2 (#tyNameMap env1, #tyNameMap env2)
      , strMap = Syntax.StrIdMap.unionWith #2 (#strMap env1, #strMap env2)
      , sigMap = Syntax.SigIdMap.unionWith #2 (#sigMap env1, #sigMap env2)
      , funMap = Syntax.FunIdMap.unionWith #2 (#funMap env1, #funMap env2)
      , boundTyVars = Syntax.TyVarMap.unionWith #2 (#boundTyVars env1, #boundTyVars env2)
      }

fun envWithValEnv valMap : Env
    = { valMap = valMap
      , tyConMap = Syntax.TyConMap.empty
      , tyNameMap = TypedSyntax.TyNameMap.empty
      , strMap = Syntax.StrIdMap.empty
      , sigMap = Syntax.SigIdMap.empty
      , funMap = Syntax.FunIdMap.empty
      , boundTyVars = Syntax.TyVarMap.empty
      }

fun envWithTyConEnv (tyConMap, tyNameMap) : ('val,'str) Env'
    = { valMap = Syntax.VIdMap.empty
      , tyConMap = tyConMap
      , tyNameMap = tyNameMap
      , strMap = Syntax.StrIdMap.empty
      , sigMap = Syntax.SigIdMap.empty
      , funMap = Syntax.FunIdMap.empty
      , boundTyVars = Syntax.TyVarMap.empty
      }

fun envWithStrMap strMap
    = { valMap = Syntax.VIdMap.empty
      , tyConMap = Syntax.TyConMap.empty
      , tyNameMap = TypedSyntax.TyNameMap.empty
      , strMap = strMap
      , sigMap = Syntax.SigIdMap.empty
      , funMap = Syntax.FunIdMap.empty
      , boundTyVars = Syntax.TyVarMap.empty
      }

fun envWithSigMap sigMap
    = { valMap = Syntax.VIdMap.empty
      , tyConMap = Syntax.TyConMap.empty
      , tyNameMap = TypedSyntax.TyNameMap.empty
      , strMap = Syntax.StrIdMap.empty
      , sigMap = sigMap
      , funMap = Syntax.FunIdMap.empty
      , boundTyVars = Syntax.TyVarMap.empty
      }

fun envWithFunMap funMap
    = { valMap = Syntax.VIdMap.empty
      , tyConMap = Syntax.TyConMap.empty
      , tyNameMap = TypedSyntax.TyNameMap.empty
      , strMap = Syntax.StrIdMap.empty
      , sigMap = Syntax.SigIdMap.empty
      , funMap = funMap
      , boundTyVars = Syntax.TyVarMap.empty
      }

fun envToSigEnv(env : Env) : SigEnv
    = { valMap = Syntax.VIdMap.map (fn (tysc, ids, longvid) => (tysc, ids, ())) (#valMap env)
      , tyConMap = #tyConMap env
      , tyNameMap = #tyNameMap env
      , strMap = Syntax.StrIdMap.map (fn (s, longstrid) => (s, ())) (#strMap env)
      , sigMap = #sigMap env
      , funMap = #funMap env
      , boundTyVars = #boundTyVars env
      }

fun freeTyVarsInEnv (bound, { valMap, tyConMap, tyNameMap, strMap, sigMap, funMap, boundTyVars } : Env)
    = Syntax.TyVarMap.foldl (fn (tv, set) => TypedSyntax.TyVarSet.add (set, tv)) TypedSyntax.TyVarSet.empty boundTyVars

type Context = { nextTyVar : int ref
               , nextVId : int ref
               }

type InferenceContext = { context : Context
                        , level : TypedSyntax.level
                        }

fun enterLevel ({ context, level } : InferenceContext) : InferenceContext
    = { context = context, level = level + 1 }

exception TypeError of SourcePos.span list * string

fun emitError (ctx : Context, spans, message) = raise TypeError (spans, message)
fun emitTypeError (ctx : InferenceContext, spans, message) = emitError (#context ctx, spans, message)

(* lookupStr : Context * TypedSyntax.Signature * SourcePos.span * Syntax.StrId list -> TypedSyntax.Signature *)
fun lookupStr (ctx, s : TypedSyntax.Signature, span, nil) = s
  | lookupStr(ctx, s as { strMap = strMap, ... }, span, (strid0 as Syntax.MkStrId name) :: strids)
    = (case Syntax.StrIdMap.find(strMap, strid0) of
           NONE => emitError(ctx, [span], "unknown structure name '" ^ name ^ "'")
         | SOME (TypedSyntax.MkSignature innerEnv) => lookupStr (ctx, innerEnv, span, strids)
      )
(* lookupTyConInEnv : Context * ('val,'str) Env' * SourcePos.span * Syntax.LongTyCon -> T.TypeStructure *)
fun lookupTyConInEnv(ctx, env : ('val,'str) Env', span, Syntax.MkQualified([], tycon as Syntax.MkTyCon name))
    = (case Syntax.TyConMap.find(#tyConMap env, tycon) of
           SOME tystr => tystr
         | NONE => emitError(ctx, [span], "unknown type constructor '" ^ name ^ "'")
      )
  | lookupTyConInEnv(ctx, env, span, Syntax.MkQualified(strid0 :: strids, tycon as Syntax.MkTyCon name))
    = (case Syntax.StrIdMap.find(#strMap env, strid0) of
           SOME (s, _) => (case lookupStr(ctx, s, span, strids) of
                               { tyConMap, ... } => case Syntax.TyConMap.find(tyConMap, tycon) of
                                                        SOME tystr => tystr
                                                      | NONE => emitError(ctx, [span], "unknown type constructor '" ^ name ^ "'")
                          )
         | NONE => emitError(ctx, [span], "unknown structure name '" ^ (case strid0 of Syntax.MkStrId name => name) ^ "'")
      )
fun lookupTyNameInEnv(ctx, { tyNameMap, ... } : ('val,'str) Env', span, tyname)
    = (case TypedSyntax.TyNameMap.find (tyNameMap, tyname) of
           SOME attr => attr
         | NONE => emitError (ctx, [span], "unknown type constructor " ^ TypedSyntax.print_TyName tyname ^ " (internal error)")
      )

datatype 'a LookupResult = Found of 'a
                         | ValueNotFound of Syntax.VId Syntax.Qualified
                         | StructureNotFound of Syntax.StrId Syntax.Qualified

(* lookupStr' : Context * TypedSyntax.Signature * Syntax.StrId list -> TypedSyntax.Signature LookupResult *)
fun lookupStr' (ctx, s : TypedSyntax.Signature, _, nil) = Found s
  | lookupStr' (ctx, s as { strMap, ... }, revStrIds, (strid0 as Syntax.MkStrId name) :: strids)
    = (case Syntax.StrIdMap.find (strMap, strid0) of
           NONE => StructureNotFound (Syntax.MkQualified (List.rev revStrIds, strid0))
         | SOME (TypedSyntax.MkSignature innerEnv) => lookupStr' (ctx, innerEnv, strid0 :: revStrIds, strids)
      )

(* Context * Env * SourcePos.span * Syntax.LongVId -> (TypedSyntax.LongVId, TypedSyntax.TypeScheme * Syntax.IdStatus) LookupResult *)
fun lookupLongVIdInEnv (ctx, env : Env, span, longvid as Syntax.MkQualified ([], vid))
    = (case Syntax.VIdMap.find (#valMap env, vid) of
           SOME (tysc, ids, longvid) => Found (longvid, tysc, ids)
         | NONE => ValueNotFound longvid
      )
  | lookupLongVIdInEnv (ctx, env, span, longvid as Syntax.MkQualified (strid0 :: strids, vid))
    = (case Syntax.StrIdMap.find (#strMap env, strid0) of
           SOME (s, TypedSyntax.MkLongStrId (strid0, strids0)) =>
           (case lookupStr' (ctx, s, [], strids) of
                Found { valMap, ... } => (case Syntax.VIdMap.find(valMap, vid) of
                                              SOME (tysc, ids) => Found (TypedSyntax.MkLongVId (strid0, strids0 @ strids, vid), tysc, ids)
                                            | NONE => ValueNotFound longvid
                                         )
             | StructureNotFound notfound => StructureNotFound notfound
             | ValueNotFound notfound => ValueNotFound notfound (* cannot occur *)
           )
         | NONE => StructureNotFound (Syntax.MkQualified ([], strid0))
      )

(* getConstructedType : Context * SourcePos.span * TypedSyntax.Ty -> TypedSyntax.TyCon *)
fun getConstructedType (ctx, span, TypedSyntax.TyVar _) = emitError (ctx, [span], "getConstructedType: got a type variable")
  | getConstructedType (ctx, span, TypedSyntax.AnonymousTyVar _) = emitError (ctx, [span], "getConstructedType: got a type variable")
  | getConstructedType (ctx, span, TypedSyntax.RecordType _) = emitError (ctx, [span], "getConstructedType: got a record")
  | getConstructedType (ctx, span, TypedSyntax.TyCon (_, tyargs, tycon)) = tycon
  | getConstructedType (ctx, span, TypedSyntax.FnType (_, _, t)) = getConstructedType (ctx, span, t)
  | getConstructedType (ctx, span, TypedSyntax.RecordExtType _) = emitError (ctx, [span], "getConstructedType: got a record")

(* The Definition, 4.7 Non-expansive Expressions *)
(* isNonexpansive : Env * TypedSyntax.Exp -> bool *)
fun isNonexpansive (env : Env, TypedSyntax.SConExp _) = true
  | isNonexpansive (env, TypedSyntax.VarExp _) = true (* <op> longvid *)
  | isNonexpansive (env, TypedSyntax.RecordExp (_, fields)) = List.all (fn (_, e) => isNonexpansive (env, e)) fields
  | isNonexpansive (env, TypedSyntax.TypedExp (_, e, _)) = isNonexpansive (env, e)
  | isNonexpansive (env, TypedSyntax.AppExp (_, conexp, e)) = isConexp (env, conexp) andalso isNonexpansive (env, e)
  | isNonexpansive (env, TypedSyntax.FnExp _) = true
  | isNonexpansive (env, TypedSyntax.ProjectionExp _) = true
  | isNonexpansive (env, TypedSyntax.ListExp (_, xs, _)) = Vector.all (fn x => isNonexpansive (env, x)) xs
  | isNonexpansive (env, TypedSyntax.VectorExp (_, xs, _)) = Vector.all (fn x => isNonexpansive (env, x)) xs
  | isNonexpansive (env, _) = false
and isConexp (env : Env, TypedSyntax.TypedExp (_, e, _)) = isConexp (env, e)
  | isConexp (env, TypedSyntax.VarExp (_, _, Syntax.ValueVariable, _)) = false
  | isConexp (env, TypedSyntax.VarExp (_, TypedSyntax.MkShortVId (TypedSyntax.MkVId (name, _)), Syntax.ValueConstructor _, _)) = name <> "ref"
  | isConexp (env, TypedSyntax.VarExp (_, TypedSyntax.MkLongVId (_, _, Syntax.MkVId name), Syntax.ValueConstructor _, _)) = name <> "ref"
  | isConexp (env, TypedSyntax.VarExp (_, _, Syntax.ExceptionConstructor, _)) = true
  | isConexp(env, _) = false

(* isExhaustive : Context * Env * TypedSyntax.Pat -> bool *)
fun isExhaustive (ctx, env : Env, TypedSyntax.WildcardPat _) = true
  | isExhaustive (ctx, env, TypedSyntax.SConPat _) = false
  | isExhaustive (ctx, env, TypedSyntax.VarPat _) = true
  | isExhaustive (ctx, env, TypedSyntax.RecordPat { fields, ... }) = List.all (fn (_, e) => isExhaustive (ctx, env, e)) fields
  | isExhaustive (ctx, env, TypedSyntax.ConPat { sourceSpan, longvid, payload = NONE, tyargs, valueConstructorInfo = SOME info }) = Syntax.VIdSet.numItems (#allConstructors info) = 1
  | isExhaustive (ctx, env, TypedSyntax.ConPat { sourceSpan, longvid, payload = SOME (innerTy, innerPat), tyargs, valueConstructorInfo = SOME info }) = Syntax.VIdSet.numItems (#allConstructors info) = 1 andalso isExhaustive (ctx, env, innerPat)
  | isExhaustive (ctx, env, TypedSyntax.ConPat { sourceSpan, longvid, payload, tyargs, valueConstructorInfo = NONE }) = false
  | isExhaustive (ctx, env, TypedSyntax.TypedPat (_, innerPat, _)) = isExhaustive (ctx, env, innerPat)
  | isExhaustive (ctx, env, TypedSyntax.LayeredPat (_, _, _, innerPat)) = isExhaustive (ctx, env, innerPat)
  | isExhaustive (ctx, env, TypedSyntax.VectorPat (_, pats, ellipsis, elemTy)) = ellipsis andalso Vector.length pats = 0

val primTyName_int = TypedSyntax.MkTyName ("int", 0)
val primTyName_word = TypedSyntax.MkTyName ("word", 1)
val primTyName_real = TypedSyntax.MkTyName ("real", 2)
val primTyName_string = TypedSyntax.MkTyName ("string", 3)
val primTyName_char = TypedSyntax.MkTyName ("char", 4)
val primTyName_exn = TypedSyntax.MkTyName ("exn", 5)
val primTyName_bool = TypedSyntax.MkTyName ("bool", 6)
val primTyName_ref = TypedSyntax.MkTyName ("ref", 7)
val primTyName_list = TypedSyntax.MkTyName ("list", 8)
val primTyName_array = TypedSyntax.MkTyName ("array", 9)
val primTyName_vector = TypedSyntax.MkTyName ("vector", 10)
val primTyName_exntag = TypedSyntax.MkTyName ("exntag", 11)
val primTyName_function2 = TypedSyntax.MkTyName ("function2", 12)
val primTyName_function3 = TypedSyntax.MkTyName ("function3", 13)
val primTyName_wideChar = TypedSyntax.MkTyName ("WideChar.char", 14)
val primTyName_wideString = TypedSyntax.MkTyName ("WideString.string", 15)
val primTyName_intInf = TypedSyntax.MkTyName ("IntInf.int", 16)
val primTyName_Lua_value = TypedSyntax.MkTyName ("Lua.value", 17)
val primTyName_JavaScript_value = TypedSyntax.MkTyName ("JavaScript.value", 18)
val primTyName_cont = TypedSyntax.MkTyName ("Cont.cont", 19)
val primTyName_prompt = TypedSyntax.MkTyName ("DelimCont.prompt", 20)
val primTyName_subcont = TypedSyntax.MkTyName ("DelimCont.subcont", 21)
val primTy_unit = TypedSyntax.RecordType (SourcePos.nullSpan, Syntax.LabelMap.empty)
val primTy_int = TypedSyntax.TyCon (SourcePos.nullSpan, [], primTyName_int)
val primTy_word = TypedSyntax.TyCon (SourcePos.nullSpan, [], primTyName_word)
val primTy_real = TypedSyntax.TyCon (SourcePos.nullSpan, [], primTyName_real)
val primTy_string = TypedSyntax.TyCon (SourcePos.nullSpan, [], primTyName_string)
val primTy_char = TypedSyntax.TyCon (SourcePos.nullSpan, [], primTyName_char)
val primTy_exn = TypedSyntax.TyCon (SourcePos.nullSpan, [], primTyName_exn)
val primTy_exntag = TypedSyntax.TyCon (SourcePos.nullSpan, [], primTyName_exntag)
val primTy_bool = TypedSyntax.TyCon (SourcePos.nullSpan, [], primTyName_bool)
val primTy_wideChar = TypedSyntax.TyCon (SourcePos.nullSpan, [], primTyName_wideChar)
val primTy_wideString = TypedSyntax.TyCon (SourcePos.nullSpan, [], primTyName_wideString)
val primTy_intInf = TypedSyntax.TyCon (SourcePos.nullSpan, [], primTyName_intInf)
val primTy_Lua_value = TypedSyntax.TyCon (SourcePos.nullSpan, [], primTyName_Lua_value)
val primTy_JavaScript_value = TypedSyntax.TyCon (SourcePos.nullSpan, [], primTyName_JavaScript_value)
val VId_Bind = TypedSyntax.MkVId ("Bind", ~1)
val LongVId_Bind = TypedSyntax.MkShortVId VId_Bind

fun isRefOrArray (tyname : TypedSyntax.TyName) = TypedSyntax.eqTyName (tyname, primTyName_ref) orelse TypedSyntax.eqTyName (tyname, primTyName_array)

structure TypeOfPrimitives = TypeOfPrimitives (type ty = TypedSyntax.Ty
                                               type tv = TypedSyntax.TyVar
                                               type constraint = TypedSyntax.UnaryConstraint
                                               val tyVarA = TypedSyntax.MkTyVar ("'a", 0)
                                               val tyVarB = TypedSyntax.MkTyVar ("'b", 1)
                                               val tyVarC = TypedSyntax.MkTyVar ("'c", 2)
                                               val tyVarD = TypedSyntax.MkTyVar ("'d", 3)
                                               val tyVarEqA = TypedSyntax.MkTyVar ("''a", 0)
                                               val tyA = TypedSyntax.TyVar (SourcePos.nullSpan, tyVarA)
                                               val tyB = TypedSyntax.TyVar (SourcePos.nullSpan, tyVarB)
                                               val tyC = TypedSyntax.TyVar (SourcePos.nullSpan, tyVarC)
                                               val tyD = TypedSyntax.TyVar (SourcePos.nullSpan, tyVarD)
                                               val tyEqA = TypedSyntax.TyVar (SourcePos.nullSpan, tyVarEqA)
                                               val unit = primTy_unit
                                               val bool = primTy_bool
                                               val int = primTy_int
                                               val word = primTy_word
                                               val real = primTy_real
                                               val char = primTy_char
                                               val wideChar = primTy_wideChar
                                               val string = primTy_string
                                               val wideString = primTy_wideString
                                               val intInf = primTy_intInf
                                               val exn = primTy_exn
                                               val exntag = primTy_exntag
                                               val LuaValue = primTy_Lua_value
                                               val JavaScriptValue = primTy_JavaScript_value
                                               fun refOf ty = TypedSyntax.TyCon (SourcePos.nullSpan, [ty], primTyName_ref)
                                               fun listOf ty = TypedSyntax.TyCon (SourcePos.nullSpan, [ty], primTyName_list)
                                               fun vectorOf ty = TypedSyntax.TyCon (SourcePos.nullSpan, [ty], primTyName_vector)
                                               fun arrayOf ty = TypedSyntax.TyCon (SourcePos.nullSpan, [ty], primTyName_array)
                                               fun pairOf (ty1, ty2) = TypedSyntax.PairType (SourcePos.nullSpan, ty1, ty2)
                                               fun tupleOf types = TypedSyntax.TupleType (SourcePos.nullSpan, types)
                                               fun function1Of (a, b) = TypedSyntax.FnType (SourcePos.nullSpan, b, a)
                                               fun function2Of (a, b, c) = TypedSyntax.TyCon (SourcePos.nullSpan, [a, b, c], primTyName_function2)
                                               fun function3Of (a, b, c, d) = TypedSyntax.TyCon (SourcePos.nullSpan, [a, b, c, d], primTyName_function3)
                                               fun contOf ty = TypedSyntax.TyCon (SourcePos.nullSpan, [ty], primTyName_cont)
                                               fun promptOf ty = TypedSyntax.TyCon (SourcePos.nullSpan, [ty], primTyName_prompt)
                                               fun subcontOf (a, b) = TypedSyntax.TyCon (SourcePos.nullSpan, [a, b], primTyName_subcont)
                                               val IsEqType = TypedSyntax.IsEqType
                                              ) : sig
                                 val typeOf : Primitives.PrimOp -> { vars : (TypedSyntax.TyVar * TypedSyntax.UnaryConstraint list) list, args : TypedSyntax.Ty vector, result : TypedSyntax.Ty }
                                 end

fun newContext() : Context
    = { nextTyVar = ref 100
      , nextVId = ref 100
      }

fun renewVId (ctx : Context) (TypedSyntax.MkVId (name, _)) : TypedSyntax.VId
    = let val n = !(#nextVId ctx)
      in #nextVId ctx := n + 1
       ; TypedSyntax.MkVId (name, n)
      end

fun genTyVarId(ctx : Context)
    = let val id = !(#nextTyVar ctx)
      in #nextTyVar ctx := id + 1 ; id end
fun genTyVar (ctx, Syntax.MkTyVar tvname) = TypedSyntax.MkTyVar (tvname, genTyVarId ctx)

fun freshTyVar (ctx : InferenceContext, span : SourcePos.span, cts : TypedSyntax.UnaryConstraint list) : TypedSyntax.AnonymousTyVar
    = ref (TypedSyntax.Unbound (List.map (fn ct => (span, ct)) cts, #level ctx))

fun newVId(ctx : Context, Syntax.MkVId name) = let val n = !(#nextVId ctx)
                                               in #nextVId ctx := n + 1
                                                ; TypedSyntax.MkVId (name, n)
                                               end
  | newVId(ctx, Syntax.GeneratedVId(name, _)) = let val n = !(#nextVId ctx)
                                                in #nextVId ctx := n + 1
                                                 ; TypedSyntax.MkVId (name, n)
                                                end

fun newStrId(ctx : Context, Syntax.MkStrId name) = let val n = !(#nextVId ctx)
                                                   in #nextVId ctx := n + 1
                                                    ; TypedSyntax.MkStrId (name, n)
                                                   end

fun newFunId(ctx : Context, Syntax.MkFunId name) = let val n = !(#nextVId ctx)
                                                   in #nextVId ctx := n + 1
                                                    ; TypedSyntax.MkFunId (name, n)
                                                   end

fun genTyConId(ctx : Context)
    = let val id = !(#nextTyVar ctx)
      in #nextTyVar ctx := id + 1 ; id end
fun newTyName (ctx, Syntax.MkTyCon name) = TypedSyntax.MkTyName (name, genTyConId ctx)
fun renewTyName (ctx : Context, TypedSyntax.MkTyName (name, _))
    = let val id = !(#nextTyVar ctx)
      in #nextTyVar ctx := id + 1
       ; TypedSyntax.MkTyName (name, id)
      end

local
    structure S = Syntax
    structure T = TypedSyntax
in
(* occurCheckAndAdjustLevel : T.AnonymousTyVar -> T.Ty -> bool; returns true if the type variable occurs in the type *)
fun occurCheckAndAdjustLevel tv
    = let fun check (T.AnonymousTyVar (_, tv'))
              = if tv = tv' then
                    true
                else
                    (case !tv' of
                         T.Unbound (cts, l') => let val minLevel = case !tv of
                                                                       T.Unbound (_, l) => Int.min (l, l')
                                                                     | _ => l'
                                                in tv' := T.Unbound (cts, minLevel)
                                                 ; false
                                                end
                       | T.Link ty => check ty
                    )
            | check (T.TyVar (_, _)) = false
            | check (T.RecordType (_, xs)) = Syntax.LabelMap.exists check xs
            | check (T.TyCon (_, tyargs, tycon)) = List.exists check tyargs
            | check (T.FnType (_, ty1, ty2)) = check ty1 orelse check ty2
            | check (T.RecordExtType (_, xs, baseTy)) = Syntax.LabelMap.exists check xs orelse check baseTy
      in check
      end

val applySubstTy = T.applySubstTy

(* instantiate : InferenceContext * SourcePos.span * T.TypeScheme -> T.Ty * (T.Ty * T.UnaryConstraint list) list *)
fun instantiate (ctx : InferenceContext, span, T.TypeScheme (vars, ty))
    = let val (subst, tyargs) = List.foldl (fn ((v, preds), (set, rest)) =>
                                               let val tv = freshTyVar (ctx, span, preds)
                                                   val tyarg = T.AnonymousTyVar (span, tv)
                                               in (T.TyVarMap.insert (set, v, tyarg), (tyarg, preds) :: rest)
                                               end
                                           ) (T.TyVarMap.empty, []) vars
      in (applySubstTy subst ty, List.rev tyargs)
      end

(* unify : InferenceContext * Env * Constraint list -> unit *)
(* The environment is used to determine if a data type admits equality *)
fun unify (ctx : InferenceContext, env : Env, nil : T.Constraint list) : unit = ()
  | unify (ctx, env, ct :: ctrs)
    = (case ct of
           T.EqConstr (span1, T.AnonymousTyVar (span2, tv), ty) => unifyTyVarAndTy (ctx, env, span1, tv, ty, ctrs)
         | T.EqConstr (span1, ty, T.AnonymousTyVar (span2, tv)) => unifyTyVarAndTy (ctx, env, span1, tv, ty, ctrs)
         | T.EqConstr (span1, T.TyVar (span2, tv as T.MkTyVar (name, x)), T.TyVar (span3, tv' as T.MkTyVar (name', x'))) =>
           if T.eqUTyVar (tv, tv') then
               unify(ctx, env, ctrs) (* do nothing *)
           else
               emitTypeError (ctx, [span1, span2, span3], "cannot unify named type variable: " ^ name ^ " and " ^ name')
         | T.EqConstr (span1, T.TyVar (span2, T.MkTyVar (name, _)), ty) => emitTypeError (ctx, [span1, span2], "cannot unify named type variable: " ^ name)
         | T.EqConstr (span1, ty, T.TyVar (span2, T.MkTyVar (name, _))) => emitTypeError (ctx, [span1, span2], "cannot unify named type variable: " ^ name)
         | T.EqConstr (span, T.FnType (_, s0, s1), T.FnType (_, t0, t1)) => unify (ctx, env, T.EqConstr (span, s0, t0) :: T.EqConstr (span, s1, t1) :: ctrs)
         | T.EqConstr (span1, T.RecordType (span2, fields), T.RecordType (span3, fields')) =>
           if Syntax.LabelMap.numItems fields <> Syntax.LabelMap.numItems fields then
               emitTypeError (ctx, [span1, span2, span3], "unification failed: incompatible record types (different number of fields)")
           else
               unify(ctx, env, Syntax.LabelMap.foldli (fn (label, ty, acc) =>
                                                          case Syntax.LabelMap.find (fields', label) of
                                                              NONE => emitTypeError (ctx, [span1, span2, span3], "unification failed: incompatible record types")
                                                            | SOME ty' => T.EqConstr (span1, ty, ty') :: acc)
                                                      ctrs fields)
         | T.EqConstr (span1, T.RecordType (span2, fields), T.RecordExtType (span3, fields', baseTy)) =>
           if Syntax.LabelMap.numItems fields < Syntax.LabelMap.numItems fields' then
               emitTypeError (ctx, [span1, span2, span3], "unification failed: incompatible record types (different number of fields)")
           else
               let val extraFields = Syntax.LabelMap.filteri (fn (label, ty) => not (Syntax.LabelMap.inDomain (fields', label))) fields
               in unify (ctx, env, Syntax.LabelMap.foldli (fn (label, ty, acc) =>
                                                              case Syntax.LabelMap.find (fields, label) of
                                                                  NONE => emitTypeError (ctx, [span1, span2, span3], "unification failed: incompatible record types")
                                                                | SOME ty' => T.EqConstr (span1, ty, ty') :: acc) (T.EqConstr (span1, T.RecordType (span2, extraFields), baseTy) :: ctrs) fields')
               end
         | T.EqConstr (span1, T.RecordExtType (span3, fields', baseTy), T.RecordType (span2, fields)) =>
           if Syntax.LabelMap.numItems fields < Syntax.LabelMap.numItems fields' then
               emitTypeError (ctx, [span1, span2, span3], "unification failed: incompatible record types (different number of fields)")
           else
               let val extraFields = Syntax.LabelMap.filteri (fn (label, ty) => not (Syntax.LabelMap.inDomain (fields', label))) fields
               in unify (ctx, env, Syntax.LabelMap.foldli (fn (label, ty, acc) =>
                                                              case Syntax.LabelMap.find (fields, label) of
                                                                  NONE => emitTypeError (ctx, [span1, span2, span3], "unification failed: incompatible record types")
                                                                | SOME ty' => T.EqConstr (span1, ty, ty') :: acc) (T.EqConstr (span1, baseTy, T.RecordType (span2, extraFields)) :: ctrs) fields')
               end
         | T.EqConstr (span1, T.RecordExtType (span2, fields, baseTy), T.RecordExtType (span3, fields', baseTy')) =>
           let val commonFields = Syntax.LabelMap.listItems (Syntax.LabelMap.intersectWith (fn (ty, ty') => T.EqConstr (span1, ty, ty')) (fields, fields'))
               val uniqueFields = Syntax.LabelMap.filteri (fn (label, ty) => not (Syntax.LabelMap.inDomain (fields', label))) fields
               val uniqueFields' = Syntax.LabelMap.filteri (fn (label, ty) => not (Syntax.LabelMap.inDomain (fields, label))) fields'
               val ctrs = commonFields @ ctrs
           in case (Syntax.LabelMap.isEmpty uniqueFields, Syntax.LabelMap.isEmpty uniqueFields') of
                  (true, true) => unify (ctx, env, T.EqConstr (span1, baseTy, baseTy') :: ctrs)
                | (true, false) => unify (ctx, env, T.EqConstr (span1, baseTy, T.RecordExtType (span3, uniqueFields', baseTy')) :: ctrs) (* fields is a proper submap of fields' *)
                | (false, true) => unify (ctx, env, T.EqConstr (span1, T.RecordExtType (span3, uniqueFields, baseTy), baseTy') :: ctrs) (* fields' is a proper submap of fields *)
                | (false, false) => let val unionFields = Syntax.LabelSet.toList (Syntax.LabelMap.foldli (fn (label, _, acc) => Syntax.LabelSet.add (acc, label)) (Syntax.LabelMap.foldli (fn (label, _, acc) => Syntax.LabelSet.add (acc, label)) Syntax.LabelSet.empty fields) fields')
                                        val commonBaseTy = T.AnonymousTyVar (span1, freshTyVar (ctx, span1, T.IsRecord :: List.map T.NoField unionFields))
                                    in unify (ctx, env, T.EqConstr (span1, baseTy, T.RecordExtType (span3, uniqueFields', commonBaseTy)) :: T.EqConstr (span1, baseTy', T.RecordExtType (span2, uniqueFields, commonBaseTy)) :: ctrs)
                                    end
           end
         | T.EqConstr (span1, t1 as T.TyCon (span2, tyarg, con), t2 as T.TyCon (span3, tyarg', con')) =>
           if T.eqTyName (con, con') then
               unify (ctx, env, (ListPair.mapEq (fn (x, y) => T.EqConstr (span1, x, y)) (tyarg, tyarg')
                                 handle ListPair.UnequalLengths => emitTypeError (ctx, [span1, span2, span3], "unification failed: the number of type arguments differ")
                                ) @ ctrs)
           else
               emitTypeError (ctx, [span1, span2, span3], "unification failed: type constructor mismatch (" ^ TypedSyntax.PrettyPrint.print_Ty t1 ^ " vs " ^ TypedSyntax.PrettyPrint.print_Ty t2 ^ ")") (* ??? *)
         | T.EqConstr (span, ty1, ty2) => emitTypeError (ctx, [span], "unification failed: not match (" ^ TypedSyntax.PrettyPrint.print_Ty ty1 ^ " vs " ^ TypedSyntax.PrettyPrint.print_Ty ty2 ^ ")")
         | T.UnaryConstraint (span1, recordTy, T.NoField label) =>
           (case recordTy of
                T.RecordType (span2, fields) => (case Syntax.LabelMap.find (fields, label) of
                                                     NONE => unify (ctx, env, ctrs)
                                                   | SOME _ => emitTypeError (ctx, [span1, span2], "duplicate record field")
                                                )
              | T.RecordExtType (span2, fields, baseTy) => (case Syntax.LabelMap.find (fields, label) of
                                                                NONE => unify (ctx, env, T.UnaryConstraint (span1, baseTy, T.NoField label) :: ctrs)
                                                              | SOME _ => emitTypeError (ctx, [span1, span2], "duplicate record field")
                                                           )
              | T.TyCon (span2, _, _) => emitTypeError (ctx, [span1, span2], "record field for a non-record type")
              | T.FnType (span2, _, _) => emitTypeError (ctx, [span1, span2], "record field for a function type")
              | T.TyVar (span2, _) => emitTypeError (ctx, [span1, span2], "record field for an named type variable")
              | T.AnonymousTyVar (span2, tv) =>
                (case !tv of
                     T.Link replacement => unify (ctx, env, T.UnaryConstraint (span1, replacement, T.NoField label) :: ctrs)
                   | T.Unbound (cts, level) => ( tv := T.Unbound ((span1, T.NoField label) :: cts, level)
                                               ; unify(ctx, env, ctrs)
                                               )
                )
           )
         | T.UnaryConstraint (span1, recordTy, T.IsRecord) =>
           (case recordTy of
                T.RecordType (span2, fields) => unify (ctx, env, ctrs)
              | T.RecordExtType (span2, fields, baseTy) => unify (ctx, env, T.UnaryConstraint (span1, baseTy, T.IsRecord) :: ctrs)
              | T.TyCon (span2, _, _) => emitTypeError (ctx, [span1, span2], "record field for a non-record type")
              | T.FnType (span2, _, _) => emitTypeError (ctx, [span1, span2], "record field for a function type")
              | T.TyVar (span2, _) => emitTypeError (ctx, [span1, span2], "record field for an named type variable")
              | T.AnonymousTyVar (span2, tv) =>
                (case !tv of
                     T.Link replacement => unify (ctx, env, T.UnaryConstraint (span1, replacement, T.IsRecord) :: ctrs)
                   | T.Unbound (cts, level) => ( tv := T.Unbound ((span1, T.IsRecord) :: cts, level)
                                               ; unify(ctx, env, ctrs)
                                               )
                )
           )
         | T.UnaryConstraint (span1, T.RecordType (span2, fields), T.IsEqType) => unify (ctx, env, Syntax.LabelMap.foldr (fn (ty, acc) => T.UnaryConstraint (span1, ty, T.IsEqType) :: acc) ctrs fields)
         | T.UnaryConstraint (span1, T.RecordType (span2, _), T.IsIntegral) => emitTypeError (ctx, [span1, span2], "cannot apply arithmetic operator on record type")
         | T.UnaryConstraint (span1, T.RecordType (span2, _), T.IsSignedReal) => emitTypeError (ctx, [span1, span2], "cannot apply arithmetic operator on record type")
         | T.UnaryConstraint (span1, T.RecordType (span2, _), T.IsRing) => emitTypeError (ctx, [span1, span2], "cannot apply arithmetic operator on record type")
         | T.UnaryConstraint (span1, T.RecordType (span2, _), T.IsField) => emitTypeError (ctx, [span1, span2], "cannot apply arithmetic operator on record type")
         | T.UnaryConstraint (span1, T.RecordType (span2, _), T.IsSigned) => emitTypeError (ctx, [span1, span2], "cannot apply arithmetic operator on record type")
         | T.UnaryConstraint (span1, T.RecordType (span2, _), T.IsOrdered) => emitTypeError (ctx, [span1, span2], "cannot compare records")
         | T.UnaryConstraint (span1, T.RecordType (span2, _), T.IsInt) => emitTypeError (ctx, [span1, span2], "cannot unify a record with an int")
         | T.UnaryConstraint (span1, T.RecordType (span2, _), T.IsWord) => emitTypeError (ctx, [span1, span2], "cannot unify a record with a word")
         | T.UnaryConstraint (span1, T.RecordType (span2, _), T.IsReal) => emitTypeError (ctx, [span1, span2], "cannot unify a record with a real")
         | T.UnaryConstraint (span1, T.RecordType (span2, _), T.IsChar) => emitTypeError (ctx, [span1, span2], "cannot unify a record with a char")
         | T.UnaryConstraint (span1, T.RecordType (span2, _), T.IsString) => emitTypeError (ctx, [span1, span2], "cannot unify a record with a string")
         | T.UnaryConstraint (span1, T.RecordExtType (span2, fields, baseTy), T.IsEqType) => unify (ctx, env, T.UnaryConstraint (span1, baseTy, T.IsEqType) :: Syntax.LabelMap.foldr (fn (ty, acc) => T.UnaryConstraint (span1, ty, T.IsEqType) :: acc) ctrs fields)
         | T.UnaryConstraint (span1, T.RecordExtType (span2, _, _), T.IsIntegral) => emitTypeError (ctx, [span1, span2], "cannot apply arithmetic operator on record type")
         | T.UnaryConstraint (span1, T.RecordExtType (span2, _, _), T.IsSignedReal) => emitTypeError (ctx, [span1, span2], "cannot apply arithmetic operator on record type")
         | T.UnaryConstraint (span1, T.RecordExtType (span2, _, _), T.IsRing) => emitTypeError (ctx, [span1, span2], "cannot apply arithmetic operator on record type")
         | T.UnaryConstraint (span1, T.RecordExtType (span2, _, _), T.IsField) => emitTypeError (ctx, [span1, span2], "cannot apply arithmetic operator on record type")
         | T.UnaryConstraint (span1, T.RecordExtType (span2, _, _), T.IsSigned) => emitTypeError (ctx, [span1, span2], "cannot apply arithmetic operator on record type")
         | T.UnaryConstraint (span1, T.RecordExtType (span2, _, _), T.IsOrdered) => emitTypeError (ctx, [span1, span2], "cannot compare records")
         | T.UnaryConstraint (span1, T.RecordExtType (span2, _, _), T.IsInt) => emitTypeError (ctx, [span1, span2], "cannot unify a record with an int")
         | T.UnaryConstraint (span1, T.RecordExtType (span2, _, _), T.IsWord) => emitTypeError (ctx, [span1, span2], "cannot unify a record with a word")
         | T.UnaryConstraint (span1, T.RecordExtType (span2, _, _), T.IsReal) => emitTypeError (ctx, [span1, span2], "cannot unify a record with a real")
         | T.UnaryConstraint (span1, T.RecordExtType (span2, _, _), T.IsChar) => emitTypeError (ctx, [span1, span2], "cannot unify a record with a char")
         | T.UnaryConstraint (span1, T.RecordExtType (span2, _, _), T.IsString) => emitTypeError (ctx, [span1, span2], "cannot unify a record with a string")
         | T.UnaryConstraint (span1, T.FnType (span2, _, _), T.IsEqType) => emitTypeError (ctx, [span1, span2], "function type does not admit equality")
         | T.UnaryConstraint (span1, T.FnType (span2, _, _), T.IsIntegral) => emitTypeError (ctx, [span1, span2], "cannot apply arithmetic operator on function type")
         | T.UnaryConstraint (span1, T.FnType (span2, _, _), T.IsSignedReal) => emitTypeError (ctx, [span1, span2], "cannot apply arithmetic operator on function type")
         | T.UnaryConstraint (span1, T.FnType (span2, _, _), T.IsRing) => emitTypeError (ctx, [span1, span2], "cannot apply arithmetic operator on function type")
         | T.UnaryConstraint (span1, T.FnType (span2, _, _), T.IsField) => emitTypeError (ctx, [span1, span2], "cannot apply arithmetic operator on function type")
         | T.UnaryConstraint (span1, T.FnType (span2, _, _), T.IsSigned) => emitTypeError (ctx, [span1, span2], "cannot apply arithmetic operator on function type")
         | T.UnaryConstraint (span1, T.FnType (span2, _, _), T.IsOrdered) => emitTypeError (ctx, [span1, span2], "cannot compare functions")
         | T.UnaryConstraint (span1, T.FnType (span2, _, _), T.IsInt) => emitTypeError (ctx, [span1, span2], "cannot unify a function with an int")
         | T.UnaryConstraint (span1, T.FnType (span2, _, _), T.IsWord) => emitTypeError (ctx, [span1, span2], "cannot unify a function with a word")
         | T.UnaryConstraint (span1, T.FnType (span2, _, _), T.IsReal) => emitTypeError (ctx, [span1, span2], "cannot unify a function with a real")
         | T.UnaryConstraint (span1, T.FnType (span2, _, _), T.IsChar) => emitTypeError (ctx, [span1, span2], "cannot unify a function with a char")
         | T.UnaryConstraint (span1, T.FnType (span2, _, _), T.IsString) => emitTypeError (ctx, [span1, span2], "cannot unify a function with a string")
         | T.UnaryConstraint (span1, T.TyCon (span2, tyargs, tyname), T.IsEqType) =>
           let val { admitsEquality, ... } = lookupTyNameInEnv (#context ctx, env, span2, tyname)
           in if isRefOrArray tyname then
                  unify(ctx, env, ctrs)
              else if admitsEquality then
                  unify (ctx, env, List.map (fn tyarg => T.UnaryConstraint (span1, tyarg, T.IsEqType)) tyargs @ ctrs)
              else
                  emitTypeError (ctx, [span1, span2], TypedSyntax.PrettyPrint.print_TyName tyname ^ " does not admit equality")
           end
         | T.UnaryConstraint (span1, T.TyCon (span2, tyargs, tyname), T.IsIntegral) =>
           let val { overloadClass, ... } = lookupTyNameInEnv (#context ctx, env, span2, tyname)
               val isIntegral = case overloadClass of
                                    SOME Syntax.CLASS_INT => true
                                  | SOME Syntax.CLASS_WORD => true
                                  | _ => false
           in if isIntegral then
                  unify(ctx, env, ctrs) (* do nothing *)
              else
                  emitTypeError (ctx, [span1, span2], "arithmetic operator on unsupported type")
           end
         | T.UnaryConstraint (span1, T.TyCon (span2, tyargs, tyname), T.IsSignedReal) =>
           let val { overloadClass, ... } = lookupTyNameInEnv (#context ctx, env, span2, tyname)
               val isSignedReal = case overloadClass of
                                      SOME Syntax.CLASS_INT => true
                                    | SOME Syntax.CLASS_REAL => true
                                    | _ => false
           in if isSignedReal then
                  unify(ctx, env, ctrs) (* do nothing *)
              else
                  emitTypeError (ctx, [span1, span2], "arithmetic operator on unsupported type")
           end
         | T.UnaryConstraint (span1, T.TyCon (span2, tyargs, tyname), T.IsRing) =>
           let val { overloadClass, ... } = lookupTyNameInEnv (#context ctx, env, span2, tyname)
               val isRing = case overloadClass of
                                SOME Syntax.CLASS_INT => true
                              | SOME Syntax.CLASS_WORD => true
                              | SOME Syntax.CLASS_REAL => true
                              | _ => false
           in if isRing then
                  unify(ctx, env, ctrs) (* do nothing *)
              else
                  emitTypeError (ctx, [span1, span2], "arithmetic operator on unsupported type")
           end
         | T.UnaryConstraint (span1, T.TyCon (span2, tyargs, tyname), T.IsField) =>
           let val { overloadClass, ... } = lookupTyNameInEnv (#context ctx, env, span2, tyname)
               val isField = case overloadClass of
                                 SOME Syntax.CLASS_REAL => true
                               | _ => false
           in if isField then
                  unify(ctx, env, ctrs) (* do nothing *)
              else
                  emitTypeError (ctx, [span1, span2], "arithmetic operator on unsupported type")
           end
         | T.UnaryConstraint (span1, T.TyCon (span2, tyargs, tyname), T.IsSigned) =>
           let val { overloadClass, ... } = lookupTyNameInEnv (#context ctx, env, span2, tyname)
               val isSigned = case overloadClass of
                                  SOME Syntax.CLASS_INT => true
                                | SOME Syntax.CLASS_REAL => true
                                | _ => false
           in if isSigned then
                  unify(ctx, env, ctrs) (* do nothing *)
              else
                  emitTypeError (ctx, [span1, span2], "arithmetic operator on unsupported type")
           end
         | T.UnaryConstraint (span1, T.TyCon (span2, tyargs, tyname), T.IsOrdered) =>
           let val { overloadClass, ... } = lookupTyNameInEnv (#context ctx, env, span2, tyname)
               val isOrdered = case overloadClass of
                                   SOME Syntax.CLASS_INT => true
                                 | SOME Syntax.CLASS_WORD => true
                                 | SOME Syntax.CLASS_REAL => true
                                 | SOME Syntax.CLASS_CHAR => true
                                 | SOME Syntax.CLASS_STRING => true
                                 | NONE => false
           in if isOrdered then
                  unify(ctx, env, ctrs) (* do nothing *)
              else
                  emitTypeError (ctx, [span1, span2], "comparison operator on unsupported type")
           end
         | T.UnaryConstraint (span1, T.TyCon (span2, tyargs, tyname), T.IsInt) =>
           if TypedSyntax.eqTyName (tyname, primTyName_int) then
               unify(ctx, env, ctrs) (* do nothing *)
           else if TypedSyntax.eqTyName (tyname, primTyName_intInf) then
               unify(ctx, env, ctrs) (* do nothing *)
           else
               let val { overloadClass, ... } = lookupTyNameInEnv (#context ctx, env, span2, tyname)
               in if overloadClass = SOME Syntax.CLASS_INT then
                      unify(ctx, env, ctrs) (* do nothing *)
                  else
                      emitTypeError (ctx, [span1, span2], "invalid integer constant: " ^ TypedSyntax.print_TyName tyname)
               end
         | T.UnaryConstraint (span1, T.TyCon (span2, tyargs, tyname), T.IsWord) =>
           if TypedSyntax.eqTyName (tyname, primTyName_word) then
               unify(ctx, env, ctrs) (* do nothing *)
           else
               let val { overloadClass, ... } = lookupTyNameInEnv (#context ctx, env, span2, tyname)
               in if overloadClass = SOME Syntax.CLASS_WORD then
                      unify(ctx, env, ctrs) (* do nothing *)
                  else
                      emitTypeError (ctx, [span1, span2], "invalid word constant")
               end
         | T.UnaryConstraint (span1, T.TyCon (span2, tyargs, tyname), T.IsReal) =>
           if TypedSyntax.eqTyName (tyname, primTyName_real) then
               unify(ctx, env, ctrs) (* do nothing *)
           else
               let val { overloadClass, ... } = lookupTyNameInEnv (#context ctx, env, span2, tyname)
               in if overloadClass = SOME Syntax.CLASS_REAL then
                      unify(ctx, env, ctrs) (* do nothing *)
                  else
                      emitTypeError (ctx, [span1, span2], "invalid real constant")
               end
         | T.UnaryConstraint (span1, T.TyCon (span2, tyargs, tyname), T.IsChar) =>
           if TypedSyntax.eqTyName (tyname, primTyName_char) then
               unify(ctx, env, ctrs) (* do nothing *)
           else
               let val { overloadClass, ... } = lookupTyNameInEnv (#context ctx, env, span2, tyname)
               in if overloadClass = SOME Syntax.CLASS_CHAR then
                      unify(ctx, env, ctrs) (* do nothing *)
                  else
                      emitTypeError (ctx, [span1, span2], "invalid character constant")
               end
         | T.UnaryConstraint (span1, T.TyCon (span2, tyargs, tyname), T.IsString) =>
           if TypedSyntax.eqTyName (tyname, primTyName_string) then
               unify(ctx, env, ctrs) (* do nothing *)
           else
               let val { overloadClass, ... } = lookupTyNameInEnv (#context ctx, env, span2, tyname)
               in if overloadClass = SOME Syntax.CLASS_STRING then
                      unify(ctx, env, ctrs) (* do nothing *)
                  else
                      emitTypeError (ctx, [span1, span2], "invalid string constant")
               end
         | T.UnaryConstraint (span1, T.TyVar (span2, tv as T.MkTyVar (name, _)), T.IsEqType) =>
           if T.tyVarAdmitsEquality tv then
               unify (ctx, env, ctrs)
           else
               emitTypeError (ctx, [span1, span2], "the type variable " ^ name ^ " does not admit equality")
         | T.UnaryConstraint (span1, T.TyVar (span2, tv as T.MkTyVar (name, _)), _) =>
           emitTypeError (ctx, [span1, span2], "the use of " ^ name ^ " is non-free")
         | T.UnaryConstraint (span1, T.AnonymousTyVar (span2, tv), pred) =>
           (case !tv of
                T.Link replacement => unify (ctx, env, T.UnaryConstraint (span1, replacement, pred) :: ctrs)
              | T.Unbound (cts, level) => ( tv := T.Unbound ((span1, pred) :: cts, level)
                                          ; unify (ctx, env, ctrs)
                                          )
           )
      )
and unifyTyVarAndTy (ctx : InferenceContext, env : Env, span : SourcePos.span, tv : T.AnonymousTyVar, ty : T.Ty, ctrs : T.Constraint list) : unit
    = (case !tv of
           T.Link replacement => unify (ctx, env, T.EqConstr (span, replacement, ty) :: ctrs)
         | T.Unbound (cts, level) =>
           let val ty = T.forceTy ty
           in if (case ty of T.AnonymousTyVar (_, tv') => tv = tv' | _ => false) then (* ty = AnonymousTyVar tv *)
                  unify (ctx, env, ctrs) (* do nothing *)
              else if occurCheckAndAdjustLevel tv ty then
                  emitTypeError (ctx, [span, T.getSourceSpanOfTy ty], "unification failed: occurrence check (" ^ TypedSyntax.print_AnonymousTyVar tv ^ " in " ^ TypedSyntax.print_Ty ty ^ ")")
              else
                  let fun toConstraint (span, predicate) = T.UnaryConstraint (span, ty, predicate)
                  in tv := T.Link ty
                   ; unify (ctx, env, List.map toConstraint cts @ ctrs)
                  end
           end
     )
fun addConstraint (ctx : InferenceContext, env : Env, ct : T.Constraint) = unify (ctx, env, [ct])

(* evalTy : Context * Env * S.Ty -> T.Ty *)
fun evalTy (ctx : Context, env : ('val,'str) Env', S.TyVar (span, tv)) : T.Ty
    = (case Syntax.TyVarMap.find(#boundTyVars env, tv) of
           SOME tv => T.TyVar (span, tv)
         | NONE => emitError(ctx, [span], "unknown type varibale `" ^ Syntax.print_TyVar tv ^ "`")
      )
  | evalTy (ctx, env, S.RecordType (span, fields, NONE)) = T.RecordType (span, List.foldl (fn ((label, ty), m) => Syntax.LabelMap.insert (m, label, evalTy (ctx, env, ty))) Syntax.LabelMap.empty fields)
  | evalTy (ctx, env, S.RecordType (span, fields, SOME baseTy))
    = (case evalTy (ctx, env, baseTy) of
           T.RecordType (_, fields') => T.RecordType (span, List.foldl (fn ((label, ty), m) =>
                                                                           if Syntax.LabelMap.inDomain (m, label) then
                                                                               emitError (ctx, [span], "duplicate record field: " ^ Syntax.print_Label label)
                                                                           else
                                                                               Syntax.LabelMap.insert (m, label, evalTy (ctx, env, ty))
                                                                       ) fields' fields)
         | _ => emitError (ctx, [span], "invalid record extension")
      )
  | evalTy (ctx, env, S.TyCon (span, args, tycon))
    = let val { typeFunction = T.TypeFunction (tyvars, ty), ... } = lookupTyConInEnv (ctx, env, span, tycon)
          val subst = (ListPair.foldlEq (fn (tv, arg, m) => TypedSyntax.TyVarMap.insert (m, tv, evalTy (ctx, env, arg))) TypedSyntax.TyVarMap.empty (tyvars, args))
                      handle ListPair.UnequalLengths => emitError(ctx, [span], "invalid type construction")
      in T.applySubstTy subst ty
      end
  | evalTy (ctx, env, S.FnType (span, ty1, ty2)) = T.FnType (span, evalTy (ctx, env, ty1), evalTy (ctx, env, ty2))

(* typeCheckPat : InferenceContext * Env * S.Pat * (* type hint *) T.Ty option -> T.Ty * (T.VId * T.Ty) S.VIdMap.map * T.Pat *)
fun typeCheckPat (ctx : InferenceContext, env : Env, S.WildcardPat span, typeHint : T.Ty option) : T.Ty * (T.VId * T.Ty) S.VIdMap.map * T.Pat
    = (case typeHint of
           NONE => let val ty = T.AnonymousTyVar (span, freshTyVar (ctx, span, []))
                   in (ty, S.VIdMap.empty, T.WildcardPat span)
                   end
         | SOME expectedTy => (expectedTy, S.VIdMap.empty, T.WildcardPat span)
      )
  | typeCheckPat (ctx, env, S.SConPat (span, scon), typeHint)
    = (case scon of
           Syntax.IntegerConstant _   => (case typeHint of
                                              NONE => let val tv = freshTyVar (ctx, span, [T.IsInt, T.IsEqType])
                                                          val ty = T.AnonymousTyVar (span, tv)
                                                      in (ty, S.VIdMap.empty, T.SConPat (span, scon, ty))
                                                      end
                                            | SOME expectedTy => ( unify (ctx, env, [T.UnaryConstraint (span, expectedTy, T.IsInt), T.UnaryConstraint (span, expectedTy, T.IsEqType)])
                                                                 ; (expectedTy, S.VIdMap.empty, T.SConPat (span, scon, expectedTy))
                                                                 )
                                         )
         | Syntax.WordConstant _      => (case typeHint of
                                              NONE => let val tv = freshTyVar (ctx, span, [T.IsWord, T.IsEqType])
                                                          val ty = T.AnonymousTyVar (span, tv)
                                                      in (ty, S.VIdMap.empty, T.SConPat (span, scon, ty))
                                                      end
                                            | SOME expectedTy => ( unify (ctx, env, [T.UnaryConstraint (span, expectedTy, T.IsWord), T.UnaryConstraint (span, expectedTy, T.IsEqType)])
                                                                 ; (expectedTy, S.VIdMap.empty, T.SConPat (span, scon, expectedTy))
                                                                 )
                                         )
         | Syntax.RealConstant _      => emitTypeError (ctx, [span], "no real constant may occur in a pattern")
         | Syntax.CharacterConstant _ => (case typeHint of
                                              NONE => let val tv = freshTyVar (ctx, span, [T.IsChar, T.IsEqType])
                                                          val ty = T.AnonymousTyVar (span, tv)
                                                      in (ty, S.VIdMap.empty, T.SConPat (span, scon, ty))
                                                      end
                                            | SOME expectedTy => ( unify (ctx, env, [T.UnaryConstraint (span, expectedTy, T.IsChar), T.UnaryConstraint (span, expectedTy, T.IsEqType)])
                                                                 ; (expectedTy, S.VIdMap.empty, T.SConPat (span, scon, expectedTy))
                                                                 )
                                         )
         | Syntax.StringConstant _    => (case typeHint of
                                              NONE => let val tv = freshTyVar (ctx, span, [T.IsString, T.IsEqType])
                                                          val ty = T.AnonymousTyVar (span, tv)
                                                      in (ty, S.VIdMap.empty, T.SConPat (span, scon, ty))
                                                      end
                                            | SOME expectedTy => ( unify (ctx, env, [T.UnaryConstraint (span, expectedTy, T.IsString), T.UnaryConstraint (span, expectedTy, T.IsEqType)])
                                                                 ; (expectedTy, S.VIdMap.empty, T.SConPat (span, scon, expectedTy))
                                                                 )
                                         )
      )
  | typeCheckPat (ctx, env, S.VarPat (span, vid), typeHint)
    = (case Syntax.VIdMap.find(#valMap env, vid) of
           SOME (_, Syntax.ValueConstructor _, _) => emitTypeError (ctx, [span], "VarPat: invalid pattern")
         | SOME (_, Syntax.ExceptionConstructor, _) => emitTypeError (ctx, [span], "VarPat: invalid pattern")
         | _ => (case typeHint of
                     NONE => let val ty = TypedSyntax.AnonymousTyVar (span, freshTyVar (ctx, span, []))
                                 val vid' = newVId (#context ctx, vid)
                             in (ty, S.VIdMap.singleton (vid, (vid', ty)), T.VarPat (span, vid', ty))
                             end
                   | SOME expectedTy => let val vid' = newVId (#context ctx, vid)
                                        in (expectedTy, S.VIdMap.singleton (vid, (vid', expectedTy)), T.VarPat (span, vid', expectedTy))
                                        end
                )
      )
  | typeCheckPat (ctx, env, S.RecordPat { sourceSpan, fields, ellipsis }, typeHint)
    = let fun oneField((label, pat), (fieldTypes, vars, fieldPats))
              = let val typeHint' = case typeHint of
                                        SOME (T.RecordType (_, m)) => Syntax.LabelMap.find (m, label)
                                      | _ => NONE
                    val (ty, vars', pat') = typeCheckPat (ctx, env, pat, typeHint')
                in (Syntax.LabelMap.insert (fieldTypes, label, ty), Syntax.VIdMap.unionWith (fn _ => emitTypeError (ctx, [sourceSpan], "duplicate identifier in a pattern")) (vars, vars'), (label, pat') :: fieldPats)
                end
          val (fieldTypes, vars, fieldPats) = List.foldr oneField (Syntax.LabelMap.empty, Syntax.VIdMap.empty, []) fields
      in case ellipsis of
             SOME basePat =>
             let val recordTy = case typeHint of
                                    NONE => T.AnonymousTyVar (sourceSpan, freshTyVar (ctx, sourceSpan, []))
                                  | SOME expectedTy => expectedTy
                 val (baseTy, vars', basePat) = typeCheckPat (ctx, env, basePat, NONE)
                 val fieldConstrs = T.UnaryConstraint (sourceSpan, baseTy, T.IsRecord) :: List.map (fn (label, _) => T.UnaryConstraint (sourceSpan, baseTy, T.NoField label)) fields
             in unify (ctx, env, T.EqConstr (sourceSpan, recordTy, T.RecordExtType (sourceSpan, fieldTypes, baseTy)) :: fieldConstrs)
              ; (recordTy, Syntax.VIdMap.unionWith (fn _ => emitTypeError (ctx, [sourceSpan], "duplicate identifier in a pattern")) (vars, vars'), T.RecordPat { sourceSpan = sourceSpan, fields = fieldPats, ellipsis = SOME basePat })
             end
           | NONE => (T.RecordType (sourceSpan, fieldTypes), vars, T.RecordPat { sourceSpan = sourceSpan, fields = fieldPats, ellipsis = NONE })
      end
  | typeCheckPat (ctx, env, S.ConPat (span, longvid, optInnerPat), typeHint)
    = (case lookupLongVIdInEnv(ctx, env, span, longvid) of
           Found (longvid, tysc, idstatus) =>
           (if (case idstatus of Syntax.ValueConstructor _ => true | Syntax.ExceptionConstructor => true | _ => false) then
                let val (ty, tyargs) = instantiate(ctx, span, tysc)
                    val valueConstructorInfo = case idstatus of
                                                   Syntax.ValueConstructor info => SOME info
                                                 | _ => NONE
                in case optInnerPat of
                       NONE => (ty, Syntax.VIdMap.empty, T.ConPat { sourceSpan = span, longvid = longvid, payload = NONE, tyargs = List.map #1 tyargs, valueConstructorInfo = valueConstructorInfo })
                     | SOME innerPat =>
                       (case ty of
                            T.FnType (span', argTy, resultTy) =>
                            let val (argTy', innerVars, innerPat') = typeCheckPat (ctx, env, innerPat, SOME argTy)
                            in addConstraint (ctx, env, T.EqConstr (span, argTy, argTy'))
                             ; (resultTy, innerVars, T.ConPat { sourceSpan = span, longvid = longvid, payload = SOME (argTy, innerPat'), tyargs = List.map #1 tyargs, valueConstructorInfo = valueConstructorInfo })
                            end
                          | _ => emitTypeError (ctx, [span], "invalid pattern")
                       )
                end
            else (* idstatus = Syntax.ValueVariable *)
                emitTypeError (ctx, [span], "invalid pattern")
           )
         | ValueNotFound notfound => emitTypeError (ctx, [span], "invalid pattern: value name '" ^ Syntax.print_LongVId notfound ^ "' not found")
         | StructureNotFound notfound => emitTypeError (ctx, [span], "invalid pattern: structure name '" ^ Syntax.print_LongStrId notfound ^ "' not found")
      )
  | typeCheckPat (ctx, env, S.TypedPat (span, pat, ty), typeHint)
    = let val ty = evalTy(#context ctx, env, ty)
          val (inferredTy, vars, pat) = typeCheckPat (ctx, env, pat, SOME ty)
      in addConstraint (ctx, env, T.EqConstr (span, ty, inferredTy))
       ; (ty, vars, T.TypedPat (span, pat, ty))
      end
  | typeCheckPat (ctx, env, S.LayeredPat (span, vid, optTy, pat), typeHint) (* TODO: optTy *)
    = let val (optTy, typeHint) = case optTy of
                                      SOME ty => let val ty = evalTy (#context ctx, env, ty)
                                                     val someTy = SOME ty
                                                 in (someTy, someTy)
                                                 end
                                    | NONE => (NONE, typeHint)
      val (inferredTy, vars, pat) = typeCheckPat (ctx, env, pat, typeHint)
      in case Syntax.VIdMap.find(vars, vid) of
             NONE => let val ty = case optTy of
                                      SOME ty => ( addConstraint (ctx, env, T.EqConstr (span, ty, inferredTy))
                                                 ; ty
                                                 )
                                    | NONE => inferredTy
                         val vid' = newVId (#context ctx, vid)
                     in (ty, Syntax.VIdMap.insert (vars, vid, (vid', ty)), T.LayeredPat (span, vid', ty, pat))
                     end
           | SOME _ => emitTypeError (ctx, [span], "duplicate identifier in a pattern")
      end
  | typeCheckPat (ctx, env, S.VectorPat (span, pats, ellipsis), typeHint)
    = let val elemTy = case typeHint of
                           SOME (T.TyCon (_, [elemTy], _)) => elemTy
                         | _ => TypedSyntax.AnonymousTyVar (span, freshTyVar (ctx, span, []))
          val pats = Vector.map (fn pat => let val (elemTy', vars, pat) = typeCheckPat (ctx, env, pat, SOME elemTy)
                                           in addConstraint (ctx, env, T.EqConstr (span, elemTy, elemTy'))
                                            ; (vars, pat)
                                           end
                                ) pats
          val vars = Vector.foldr (fn ((vars, _), vars') => Syntax.VIdMap.unionWith (fn _ => emitTypeError (ctx, [], "duplicate identifier in a pattern")) (vars, vars')) Syntax.VIdMap.empty pats
          val pats = Vector.map #2 pats
      in (T.TyCon (span, [elemTy], primTyName_vector), vars, T.VectorPat (span, pats, ellipsis, elemTy))
      end

fun doWithtype(ctx, env, typbinds : S.TypBind list) : S.ConBind -> S.ConBind
    = let val map = List.foldl (fn (S.TypBind (span, tyvars, tycon, ty), map) => S.TyConMap.insert (map, tycon, (tyvars, ty))) S.TyConMap.empty typbinds
          fun goTyAlias env (ty as S.TyVar (span, tv)) = (case S.TyVarMap.find (env, tv) of
                                                              SOME ty => ty
                                                            | NONE => ty
                                                         )
            | goTyAlias env (S.RecordType (span, fields, optBaseTy)) = S.RecordType (span, List.map (fn (label, ty) => (label, goTyAlias env ty)) fields, Option.map (goTyAlias env) optBaseTy)
            | goTyAlias env (S.TyCon (span, tyargs, longtycon)) = S.TyCon (span, List.map (goTyAlias env) tyargs, longtycon)
            | goTyAlias env (S.FnType (span, s, t)) = S.FnType (span, goTyAlias env s, goTyAlias env t)
          fun goTy (ty as S.TyVar (span, tv)) = ty
            | goTy (S.RecordType (span, fields, optBaseTy)) = S.RecordType (span, List.map (fn (label, ty) => (label, goTy ty)) fields, Option.map goTy optBaseTy)
            | goTy (S.TyCon (span, tyargs, Syntax.MkQualified ([], tycon))) = let val tyargs = List.map goTy tyargs
                                                                              in case S.TyConMap.find (map, tycon) of
                                                                                     SOME (tyvars, ty) => let val env' = ListPair.foldlEq (fn (tv, tyarg, m) => S.TyVarMap.insert (m, tv, tyarg)) S.TyVarMap.empty (tyvars, tyargs)
                                                                                                                         handle ListPair.UnequalLengths => emitError (ctx, [span], "invalid type construction: arity mismatch")
                                                                                                          in goTyAlias env' ty
                                                                                                          end
                                                                                   | NONE => S.TyCon (span, tyargs, S.MkQualified ([], tycon))
                                                                              end
            | goTy (S.TyCon (span, tyargs, longtycon)) = S.TyCon (span, List.map goTy tyargs, longtycon)
            | goTy (S.FnType (span, s, t)) = S.FnType (span, goTy s, goTy t)
          fun goConBind (conbind as S.ConBind (span, vid, NONE)) = conbind
            | goConBind (S.ConBind (span, vid, SOME payloadTy)) = S.ConBind (span, vid, SOME (goTy payloadTy))
      in goConBind
      end

fun determineDatatypeEquality(ctx, env : ('val,'str) Env', datbinds : (S.TyVar list * S.Ty list) S.TyConMap.map) : bool S.TyConMap.map
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
                      | doTy (S.RecordType (span, fields, NONE)) = doTypes (List.map #2 fields)
                      | doTy (S.RecordType (span, fields, SOME baseTy)) = (case doTypes (List.map #2 fields) of
                                                                               SOME xs => (case doTy baseTy of
                                                                                               SOME ys => SOME (xs @ ys)
                                                                                             | none as NONE => none
                                                                                          )
                                                                             | none as NONE => none
                                                                          )
                      | doTy (S.TyCon (span, tyargs, longtycon))
                        = let val l = case longtycon of
                                          Syntax.MkQualified([], tycon) =>
                                          if S.TyConSet.member (localTyCons, tycon) then
                                              SOME [tycon]
                                          else
                                              NONE
                                        | _ => NONE
                          in case l of
                                 result as SOME _ => result
                               | NONE => let val { typeFunction = T.TypeFunction (tyvars, ty), ... } = lookupTyConInEnv (ctx, env, span, longtycon)
                                             val tyVarMap = ListPair.foldlEq (fn (tv, ty, m) => T.TyVarMap.insert (m, tv, ty)) T.TyVarMap.empty (tyvars, tyargs)
                                             fun doUTy (T.TyVar (span, tv)) = (case T.TyVarMap.find (tyVarMap, tv) of
                                                                                   SOME ty => doTy ty
                                                                                 | NONE => if T.tyVarAdmitsEquality tv then
                                                                                               SOME []
                                                                                           else
                                                                                               NONE
                                                                              )
                                               | doUTy (T.AnonymousTyVar _) = NONE (* should not occur *)
                                               | doUTy (T.RecordType (span, fields)) = doUTypes (Syntax.LabelMap.foldl (op ::) [] fields)
                                               | doUTy (T.RecordExtType (span, fields, baseTy)) = doUTypes (Syntax.LabelMap.foldl (op ::) [baseTy] fields) (* should not occur *)
                                               | doUTy (T.TyCon (span, tyargs, tyname)) = if isRefOrArray tyname then
                                                                                              SOME []
                                                                                          else
                                                                                              let val { admitsEquality, ... } = lookupTyNameInEnv(ctx, env, span, tyname)
                                                                                              in if admitsEquality then
                                                                                                     doUTypes tyargs
                                                                                                 else
                                                                                                     NONE
                                                                                              end
                                               | doUTy (T.FnType _) = NONE
                                             and doUTypes types = let fun go (acc, ty :: types) = (case doUTy ty of
                                                                                                       NONE => NONE
                                                                                                     | SOME xs => go (xs @ acc, types)
                                                                                                  )
                                                                        | go (acc, []) = SOME acc
                                                                  in go ([], types)
                                                                  end
                                         in doUTy ty
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

(* typeCheckExp : InferenceContext * Env * S.Exp * (* type hint *) T.Ty option -> T.Ty * T.Exp *)
fun typeCheckExp (ctx : InferenceContext, env : Env, S.SConExp (span, scon), typeHint : T.Ty option) : T.Ty * T.Exp
    = let val ty = case scon of
                       Syntax.IntegerConstant x   => (case typeHint of
                                                          NONE => let val tv = freshTyVar (ctx, span, [T.IsInt])
                                                                  in T.AnonymousTyVar (span, tv)
                                                                  end
                                                        | SOME expectedTy => ( addConstraint (ctx, env, T.UnaryConstraint (span, expectedTy, T.IsInt))
                                                                             ; expectedTy
                                                                             )
                                                     )
                     | Syntax.WordConstant x      => (case typeHint of
                                                          NONE => let val tv = freshTyVar (ctx, span, [T.IsWord])
                                                                  in T.AnonymousTyVar (span, tv)
                                                                  end
                                                        | SOME expectedTy => ( addConstraint (ctx, env, T.UnaryConstraint (span, expectedTy, T.IsWord))
                                                                             ; expectedTy
                                                                             )
                                                     )
                     | Syntax.RealConstant x      => primTy_real (* TODO: overloaded literals *)
                     | Syntax.CharacterConstant x => (case typeHint of
                                                          NONE => let val tv = freshTyVar (ctx, span, [T.IsChar])
                                                                  in T.AnonymousTyVar (span, tv)
                                                                  end
                                                        | SOME expectedTy => ( addConstraint (ctx, env, T.UnaryConstraint (span, expectedTy, T.IsChar))
                                                                             ; expectedTy
                                                                             )
                                                     )
                     | Syntax.StringConstant x    => (case typeHint of
                                                          NONE => let val tv = freshTyVar (ctx, span, [T.IsString])
                                                                  in T.AnonymousTyVar (span, tv)
                                                                  end
                                                        | SOME expectedTy => ( addConstraint (ctx, env, T.UnaryConstraint (span, expectedTy, T.IsString))
                                                                             ; expectedTy
                                                                             )
                                                     )
      in (ty, T.SConExp (span, scon, ty))
      end
  | typeCheckExp (ctx, env, exp as S.VarExp (span, longvid), typeHint)
    = (case lookupLongVIdInEnv(ctx, env, span, longvid) of
           Found (longvid, tysc, ids) => let val (ty, tyargs) = instantiate(ctx, span, tysc)
                                         in (ty, T.VarExp (span, longvid, ids, tyargs))
                                         end
         | ValueNotFound notfound => emitTypeError (ctx, [span], "unknown value name " ^ Syntax.print_LongVId notfound)
         | StructureNotFound notfound => emitTypeError (ctx, [span], "unknown structure name " ^ Syntax.print_LongStrId notfound)
      )
  | typeCheckExp (ctx, env, S.RecordExp (span, fields, NONE), typeHint)
    = let fun oneField (label, exp) = let val typeHint' = case typeHint of
                                                              SOME (T.RecordType (_, m)) => Syntax.LabelMap.find (m, label)
                                                            | _ => NONE
                                          val (ty, exp) = typeCheckExp (ctx, env, exp, typeHint')
                                      in ((label, ty), (label, exp))
                                      end
          val (fieldTypes, fields) = ListPair.unzip (List.map oneField fields)
      in (T.RecordType (span, Syntax.LabelMapFromList fieldTypes), T.RecordExp (span, fields))
      end
  | typeCheckExp (ctx, env, S.RecordExp (span, fields, SOME baseExp), typeHint)
    = let val (baseTy, baseExp) = typeCheckExp (ctx, env, baseExp, NONE)
          val recordTy = T.AnonymousTyVar (span, freshTyVar (ctx, span, []))
          fun oneField ((label, exp), (fieldTypes, fields))
              = let val typeHint' = case typeHint of
                                        SOME (T.RecordType (_, m)) => Syntax.LabelMap.find (m, label)
                                      | _ => NONE
                    val (ty, exp) = typeCheckExp (ctx, env, exp, typeHint')
                in (Syntax.LabelMap.insert (fieldTypes, label, ty), (label, exp) :: fields)
                end
          val (fieldTypes, fields) = List.foldr oneField (Syntax.LabelMap.empty, []) fields
          val fieldConstrs = T.UnaryConstraint (span, baseTy, T.IsRecord) :: List.map (fn (label, _) => T.UnaryConstraint (span, baseTy, T.NoField label)) fields
      in unify (ctx, env, T.EqConstr (span, recordTy, T.RecordExtType (span, fieldTypes, baseTy)) :: fieldConstrs)
       ; (recordTy, T.RecordExtExp { sourceSpan = span, fields = fields, baseExp = baseExp, baseTy = baseTy })
      end
  | typeCheckExp (ctx, env, S.LetInExp (span, decs, innerExp), typeHint)
    = let val (env', decs) = typeCheckDecs(ctx, env, decs)
          val (ty, innerExp) = typeCheckExp (ctx, mergeEnv (env, env'), innerExp, typeHint)
      in (ty, T.LetInExp (span, decs, innerExp))
      end
  | typeCheckExp (ctx, env, S.AppExp (span, f, x), typeHint)
          (* f: s -> t, x: s *)
    = let val (funTy, f) = typeCheckExp (ctx, env, f, NONE)
          val argTyHint = case funTy of
                              T.FnType (_, argTy, _) => SOME argTy
                            | _ => NONE
          val (argTy, x) = typeCheckExp (ctx, env, x, argTyHint)
          val retTy = case funTy of
                          T.FnType (_, argTy', retTy) => ( addConstraint (ctx, env, T.EqConstr (span, argTy, argTy')) ; retTy )
                        | _ => let val retTy = case typeHint of
                                                   SOME expectedTy => expectedTy
                                                 | _ => T.AnonymousTyVar (span, freshTyVar (ctx, span, []))
                               in addConstraint (ctx, env, T.EqConstr (span, funTy, T.FnType (span, argTy, retTy))) (* funTy = (argTy -> retTy) *)
                                ; retTy
                               end
      in (retTy, T.AppExp (span, f, x))
      end
  | typeCheckExp (ctx, env, S.TypedExp (span, exp, ty), typeHint)
    = let val ty = evalTy (#context ctx, env, ty)
          val (expTy, exp) = typeCheckExp (ctx, env, exp, SOME ty)
      in addConstraint (ctx, env, T.EqConstr (span, expTy, ty))
       ; (ty, T.TypedExp (span, exp, ty))
      end
  | typeCheckExp (ctx, env, S.HandleExp (span, exp, matches), typeHint)
          (* exp: t, matches: exn -> t *)
    = let val (expTy, exp) = typeCheckExp (ctx, env, exp, typeHint)
          val (patTy, retTy, matches) = typeCheckMatch (ctx, env, span, matches, SOME primTy_exn, SOME expTy)
      in addConstraint (ctx, env, T.EqConstr (span, patTy, primTy_exn)) (* patTy = exn *)
       ; addConstraint (ctx, env, T.EqConstr (span, expTy, retTy))
       ; (expTy, T.HandleExp (span, exp, matches))
      end
  | typeCheckExp (ctx, env, S.RaiseExp (span, exp), typeHint)
    = let val (expTy, exp) = typeCheckExp (ctx, env, exp, SOME primTy_exn)
          val resultTy = case typeHint of
                             SOME expectedTy => expectedTy
                           | NONE => T.AnonymousTyVar (span, freshTyVar (ctx, span, []))
      in addConstraint (ctx, env, T.EqConstr (span, expTy, primTy_exn)) (* expTy = exn *)
       ; (resultTy, T.RaiseExp (span, resultTy, exp))
      end
  | typeCheckExp (ctx, env, S.IfThenElseExp (span, cond, thenPart, elsePart), typeHint)
    = let val (condTy, cond) = typeCheckExp (ctx, env, cond, SOME primTy_bool)
          val (thenTy, thenPart) = typeCheckExp (ctx, env, thenPart, typeHint)
          val (elseTy, elsePart) = typeCheckExp (ctx, env, elsePart, typeHint)
      in addConstraint (ctx, env, T.EqConstr (span, condTy, primTy_bool)) (* condTy = bool *)
       ; addConstraint (ctx, env, T.EqConstr (span, thenTy, elseTy)) (* thenTy = elseTy *)
       ; (thenTy, T.IfThenElseExp (span, cond, thenPart, elsePart))
      end
  | typeCheckExp (ctx, env, S.CaseExp (span, exp, matches), typeHint)
    = let val (expTy, exp) = typeCheckExp (ctx, env, exp, NONE)
          val (patTy, retTy, matches) = typeCheckMatch (ctx, env, span, matches, SOME expTy, typeHint)
      in addConstraint (ctx, env, T.EqConstr (span, expTy, patTy))
       ; (retTy, T.CaseExp (span, exp, expTy, matches))
      end
  | typeCheckExp (ctx, env, S.FnExp (span, matches), typeHint)
    = let val (argTyHint, retTyHint) = case typeHint of
                                           SOME (T.FnType (_, argTy, retTy)) => (SOME argTy, SOME retTy)
                                         | _ => (NONE, NONE)
          val (argTy, retTy, matches) = typeCheckMatch (ctx, env, span, matches, argTyHint, retTyHint)
          val fnExp = case matches of
                          [(T.VarPat (span2, vid, _), body)] => T.FnExp (span, vid, argTy, body)
                        | _ => let val vid = newVId (#context ctx, Syntax.MkVId "a")
                               in T.FnExp (span, vid, argTy, T.CaseExp (span, T.VarExp (span, T.MkShortVId vid, Syntax.ValueVariable, []), argTy, matches))
                               end
      in (T.FnType (span, argTy, retTy), fnExp)
      end
  | typeCheckExp (ctx, env, S.ProjectionExp (span, label), typeHint)
    = let val (recordTy, fieldTy) = case typeHint of
                                        SOME (T.FnType (_, argTy, retTy)) => (argTy, retTy)
                                      | _ => (TypedSyntax.AnonymousTyVar (span, freshTyVar (ctx, span, [])), TypedSyntax.AnonymousTyVar (span, freshTyVar (ctx, span, [])))
          val baseTy = T.AnonymousTyVar (span, freshTyVar (ctx, span, [T.NoField label, T.IsRecord]))
      in unify (ctx, env, [T.EqConstr (span, recordTy, T.RecordExtType (span, Syntax.LabelMap.insert (Syntax.LabelMap.empty, label, fieldTy), baseTy))])
       ; (T.FnType (span, recordTy, fieldTy), T.ProjectionExp { sourceSpan = span, label = label, recordTy = recordTy, fieldTy = fieldTy })
      end
  | typeCheckExp (ctx, env, S.ListExp (span, xs), typeHint)
    = let val elemTy = case typeHint of
                           SOME (T.TyCon (_, [elemTy], _)) => elemTy
                         | _ => TypedSyntax.AnonymousTyVar (span, freshTyVar (ctx, span, []))
          val xs = Vector.map (fn exp => let val (expTy, exp) = typeCheckExp (ctx, env, exp, SOME elemTy)
                                         in addConstraint (ctx, env, T.EqConstr (span, expTy, elemTy))
                                          ; exp
                                         end) xs
      in (T.TyCon (span, [elemTy], primTyName_list), T.ListExp (span, xs, elemTy))
      end
  | typeCheckExp (ctx, env, S.VectorExp (span, xs), typeHint)
    = let val elemTy = case typeHint of
                           SOME (T.TyCon (_, [elemTy], _)) => elemTy
                         | _ => TypedSyntax.AnonymousTyVar (span, freshTyVar (ctx, span, []))
          val xs = Vector.map (fn exp => let val (expTy, exp) = typeCheckExp (ctx, env, exp, SOME elemTy)
                                         in addConstraint (ctx, env, T.EqConstr (span, expTy, elemTy))
                                          ; exp
                                         end) xs
      in (T.TyCon (span, [elemTy], primTyName_vector), T.VectorExp (span, xs, elemTy))
      end
  | typeCheckExp (ctx, env, S.PrimExp (span, primOp, tyargs, args), typeHint)
    = let val () = if Vector.length tyargs = 0 then
                       ()
                   else
                       emitTypeError (ctx, [span], "type arguments to _primCall is not supported currently")
          val { vars = typeVariables, args = argTypes, result = resultType } = TypeOfPrimitives.typeOf primOp
          val () = if Vector.length args = Vector.length argTypes then
                       ()
                   else
                       emitTypeError (ctx, [span], "wrong number of arguments to _primCall; expected " ^ Int.toString (Vector.length argTypes) ^ ", but got " ^ Int.toString (Vector.length args))
          val (subst, tyargs) = List.foldr (fn ((v, preds), (m, rest)) =>
                                               let val tv = freshTyVar (ctx, span, preds)
                                                   val tyarg = T.AnonymousTyVar (span, tv)
                                               in (T.TyVarMap.insert (m, v, tyarg), tyarg :: rest)
                                               end
                                           ) (T.TyVarMap.empty, []) typeVariables
          val argTypes = Vector.map (applySubstTy subst) argTypes
          val resultType = applySubstTy subst resultType
          val args = Vector.mapi (fn (i, argTy) =>
                                     let val (argTy', arg) = typeCheckExp (ctx, env, Vector.sub (args, i), NONE)
                                     in addConstraint (ctx, env, T.EqConstr (span, argTy, argTy'))
                                      ; arg
                                     end
                                 ) argTypes
      in (resultType, T.PrimExp (span, primOp, vector tyargs, args))
      end
(* typeCheckDec : InferenceContext * Env * S.Dec -> (* created environment *) Env * T.Dec list *)
and typeCheckDec (ctx : InferenceContext, env : Env, S.ValDec (span, tyvarseq, valbinds))
    = let val ctx' = enterLevel ctx
          val valbinds = let val env = { valMap = #valMap env
                                       , tyConMap = #tyConMap env
                                       , tyNameMap = #tyNameMap env
                                       , strMap = #strMap env
                                       , sigMap = #sigMap env
                                       , funMap = #funMap env
                                       , boundTyVars = List.foldl (fn (tv, m) => Syntax.TyVarMap.insert (m, tv, genTyVar (#context ctx', tv))) (#boundTyVars env) tyvarseq
                                       }
                         in List.map (fn S.PatBind(span, pat, exp) =>
                                         let val (expTy, exp) = typeCheckExp (ctx', env, exp, NONE)
                                             val (patTy, newValEnv, pat) = typeCheckPat (ctx', env, pat, SOME expTy)
                                             val () = addConstraint (ctx', env, T.EqConstr (span, patTy, expTy))
                                             val generalizable = isExhaustive (ctx', env, pat) andalso isNonexpansive (env, exp)
                                         in { sourceSpan = span, pat = pat, exp = exp, expTy = expTy, valEnv = newValEnv, generalizable = generalizable }
                                         end
                                     ) valbinds
                         end
          val tyVars_env = freeTyVarsInEnv (T.TyVarSet.empty, env)
          fun generalize ({ sourceSpan = span, pat, exp, expTy, valEnv : (T.VId * T.Ty) S.VIdMap.map, generalizable = false }, (valbinds, valEnvRest : (T.VId * T.TypeScheme) S.VIdMap.map))
              = let val vars = Syntax.VIdMap.listItems valEnv
                in List.app (fn (_, ty) => let val fv = T.freeAnonymousTyVarsInTy ty
                                           in List.app (fn tv as ref (T.Unbound (tvs, level)) => if level > #level ctx then
                                                                                                     tv := T.Unbound (tvs, #level ctx)
                                                                                                 else
                                                                                                     ()
                                                       | _ => ()
                                                       ) fv
                                           end
                            ) vars
                 ; case vars of
                       [(vid, ty)] => let val valbind' = T.PolyVarBind ( span
                                                                       , vid
                                                                       , T.TypeScheme ([], ty)
                                                                       , case pat of
                                                                             T.VarPat _ => exp
                                                                           | T.TypedPat (_, T.VarPat _, _) => exp
                                                                           | _ => let val espan = T.getSourceSpanOfExp exp
                                                                                      val vid' = renewVId (#context ctx) vid
                                                                                      val pat' = T.renameVarsInPat (T.VIdMap.insert (T.VIdMap.empty, vid, vid')) pat
                                                                                  in T.CaseExp (espan, exp, expTy, if isExhaustive (ctx, env, pat) then
                                                                                                                       [(pat', T.VarExp (espan, T.MkShortVId vid', Syntax.ValueVariable, []))]
                                                                                                                   else
                                                                                                                       [(pat', T.VarExp (espan, T.MkShortVId vid', Syntax.ValueVariable, []))
                                                                                                                       ,(T.WildcardPat span, T.RaiseExp (span, ty, T.VarExp (span, LongVId_Bind, Syntax.ExceptionConstructor, [])))
                                                                                                                       ]
                                                                                               )
                                                                                  end
                                                                       )
                                      in (valbind' :: valbinds, S.VIdMap.unionWith (fn _ => emitTypeError (ctx, [span], "duplicate identifier in a binding")) (S.VIdMap.map (fn (vid, ty) => (vid, T.TypeScheme ([], ty))) valEnv, valEnvRest))
                                      end
                     | _ => let val espan = T.getSourceSpanOfExp exp
                                val vars' = List.map (fn (vid, _) => (vid, renewVId (#context ctx) vid)) vars
                                val varsMap = List.foldl T.VIdMap.insert' T.VIdMap.empty vars'
                                val pat' = T.renameVarsInPat varsMap pat
                                val tup = T.TupleExp (espan, List.map (fn (_, vid') => T.VarExp (espan, T.MkShortVId vid', Syntax.ValueVariable, [])) vars')
                                val valbind' = T.TupleBind ( span
                                                           , vars
                                                           , T.CaseExp ( espan
                                                                       , exp
                                                                       , expTy
                                                                       , if isExhaustive (ctx, env, pat) then
                                                                             [(pat', tup)]
                                                                         else
                                                                             [(pat', tup)
                                                                             ,(T.WildcardPat span, T.RaiseExp (span, T.TupleType (span, List.map #2 vars), T.VarExp (span, LongVId_Bind, Syntax.ExceptionConstructor, [])))
                                                                             ]
                                                                       )
                                                           )
                            in (valbind' :: valbinds, S.VIdMap.unionWith (fn _ => emitTypeError (ctx, [span], "duplicate identifier in a binding")) (S.VIdMap.map (fn (vid, ty) => (vid, T.TypeScheme ([], ty))) valEnv, valEnvRest))
                            end
                end
            | generalize({ sourceSpan = span, pat, exp, expTy, valEnv, generalizable = true }, (valbinds, valEnvRest))
              = let fun doVal (vid,ty)
                        = let val ty = T.forceTy ty
                              val tyVars = T.freeTyVarsInTy (tyVars_env, ty)
                              fun isEqualityType (_, TypedSyntax.IsEqType) = true
                                | isEqualityType _ = false
                              val aTyVars_ty = T.freeAnonymousTyVarsInTy ty
                              val aTyVars = List.foldl (fn (tv, vars) =>
                                                           case !tv of
                                                               T.Unbound (tvs, level) =>
                                                               if level > #level ctx andalso List.all isEqualityType tvs then
                                                                   if List.null tvs then
                                                                       let val tv' = genTyVar (#context ctx, Syntax.MkTyVar "'?")
                                                                       in tv := T.Link (T.TyVar (span, tv'))
                                                                        ; (tv', []) :: vars
                                                                       end
                                                                   else
                                                                       let val tv' = genTyVar (#context ctx, Syntax.MkTyVar "''?")
                                                                       in tv := T.Link (T.TyVar (span, tv'))
                                                                        ; (tv', [T.IsEqType]) :: vars
                                                                       end
                                                               else if level > #level ctx then
                                                                   (tv := T.Unbound (tvs, #level ctx); vars)
                                                               else
                                                                   vars
                                                             | T.Link _ => vars
                                                       ) [] aTyVars_ty
                              fun doTyVar tv = if T.tyVarAdmitsEquality tv then
                                                   (tv, [T.IsEqType])
                                               else
                                                   (tv, [])
                              val tysc = T.TypeScheme (List.map doTyVar (T.TyVarSet.listItems tyVars) @ aTyVars, ty)
                          in (vid, tysc)
                          end
                    val valEnv' = Syntax.VIdMap.map doVal valEnv
                    val valEnv'L = Syntax.VIdMap.listItems valEnv'
                    val allPoly = List.all (fn (_, T.TypeScheme (tv, _)) => not (List.null tv)) valEnv'L (* all bindings are generalized? *)
                    val espan = TypedSyntax.getSourceSpanOfExp exp
                    fun polyPart [] = []
                      | polyPart ((vid, T.TypeScheme ([], _)) :: rest) = polyPart rest
                      | polyPart ((vid, tysc) :: rest) = let val vid' = renewVId (#context ctx) vid
                                                             val pat' = TypedSyntax.renameVarsInPat (TypedSyntax.VIdMap.insert (TypedSyntax.VIdMap.empty, vid, vid')) pat
                                                         in T.PolyVarBind (span, vid, tysc, TypedSyntax.CaseExp (espan, exp, expTy, [(TypedSyntax.filterVarsInPat (fn x => x = vid') pat', TypedSyntax.VarExp (espan, TypedSyntax.MkShortVId vid', Syntax.ValueVariable, []))])) :: polyPart rest
                                                         end
                    fun isMonoVar vid = List.exists (fn (vid', T.TypeScheme (tvs, _)) => T.eqVId (vid, vid') andalso List.null tvs) valEnv'L
                    val valbind' = if allPoly then
                                       polyPart valEnv'L
                                   else
                                       let val xs = List.mapPartial (fn (vid, tysc) => case tysc of
                                                                                           T.TypeScheme ([], ty) => SOME (vid, ty)
                                                                                         | T.TypeScheme (_ :: _, _) => NONE
                                                                    ) valEnv'L
                                       in case xs of
                                              [(vid, ty)] => let val vid' = renewVId (#context ctx) vid
                                                                 val pat' = TypedSyntax.renameVarsInPat (TypedSyntax.VIdMap.insert (TypedSyntax.VIdMap.empty, vid, vid')) (TypedSyntax.filterVarsInPat isMonoVar pat)
                                                             in T.PolyVarBind (span, vid, T.TypeScheme ([], ty), T.CaseExp (espan, exp, expTy, [(pat', T.VarExp (espan, TypedSyntax.MkShortVId vid', Syntax.ValueVariable, []))])) :: polyPart valEnv'L
                                                             end
                                            | _ => let val vars' = List.map (fn (vid, _) => (vid, renewVId (#context ctx) vid)) xs
                                                       val varsMap = List.foldl TypedSyntax.VIdMap.insert' TypedSyntax.VIdMap.empty vars'
                                                       val pat' = TypedSyntax.renameVarsInPat varsMap (TypedSyntax.filterVarsInPat isMonoVar pat)
                                                       val tup = T.TupleExp (espan, List.map (fn (_, vid') => T.VarExp (espan, TypedSyntax.MkShortVId vid', Syntax.ValueVariable, [])) vars')
                                                   in T.TupleBind (span, xs, T.CaseExp (espan, exp, expTy, [(pat', tup)])) :: polyPart valEnv'L
                                                   end
                                       end
                in (valbind' @ valbinds, Syntax.VIdMap.unionWith (fn _ => emitTypeError (ctx, [span], "duplicate identifier in a binding")) (valEnv', valEnvRest))
                end
          val (valbinds, valEnv) = List.foldr generalize ([], Syntax.VIdMap.empty) valbinds
          val env' = envWithValEnv (Syntax.VIdMap.map (fn (vid, tysc) => (tysc, Syntax.ValueVariable, T.MkShortVId vid)) valEnv)
      in (env', [T.ValDec (span, valbinds)])
      end
  | typeCheckDec(ctx, env, S.RecValDec(span, tyvarseq, valbinds))
    = let val ctx' = enterLevel ctx
          val valbinds' : (SourcePos.span * (T.Ty * (T.VId * T.Ty) S.VIdMap.map * T.Pat) * S.Exp) list
              = List.map (fn S.PatBind (span, pat, exp) => (span, typeCheckPat (ctx', env, pat, NONE), exp)) valbinds
          val localValEnv = List.foldl (fn ((_, (_, ve, _), _), acc) => Syntax.VIdMap.unionWith #1 (acc, ve)) Syntax.VIdMap.empty valbinds'
          val localValMap = Syntax.VIdMap.map (fn (vid', ty) => (T.TypeScheme ([], ty), Syntax.ValueVariable, T.MkShortVId vid')) localValEnv
          val tyvarseq' = List.map (fn tv => (tv, genTyVar (#context ctx, tv))) tyvarseq
          val localEnv = let val { valMap, tyConMap, tyNameMap, strMap, sigMap, funMap, boundTyVars } = env
                         in { valMap = Syntax.VIdMap.unionWith #2 (valMap, localValMap)
                            , tyConMap = tyConMap
                            , tyNameMap = tyNameMap
                            , strMap = strMap
                            , sigMap = sigMap
                            , funMap = funMap
                            , boundTyVars = List.foldl Syntax.TyVarMap.insert' boundTyVars tyvarseq'
                            }
                         end
          val valbinds'' = List.map (fn (span, (patTy, newValEnv, pat), exp) =>
                                        let val (expTy, exp) = typeCheckExp (ctx', localEnv, exp, SOME patTy)
                                            val () = addConstraint (ctx', env, T.EqConstr (span, patTy, expTy))
                                            val generalizable = isExhaustive (ctx', env, pat) andalso isNonexpansive (env, exp)
                                        in if generalizable then
                                               { sourceSpan = span, pat = pat, exp = exp, expTy = expTy, valEnv = newValEnv }
                                           else
                                               emitTypeError (ctx', [span], "'val rec' must be generalizable")
                                        end
                                    ) valbinds'
          val tyVars_env = freeTyVarsInEnv (T.TyVarSet.empty, env)
          fun generalize ({ sourceSpan = span, pat, exp, expTy, valEnv }, (valbinds, valEnvRest, tyVarsAcc))
              = let fun doVal (vid, ty)
                        = let val ty = T.forceTy ty
                              val tyVars = T.freeTyVarsInTy (tyVars_env, ty)
                              fun isEqualityType (_, TypedSyntax.IsEqType) = true
                                | isEqualityType _ = false
                              val aTyVars_ty = T.freeAnonymousTyVarsInTy ty
                              val aTyVars = List.foldl (fn (tv, vars) =>
                                                           case !tv of
                                                               T.Unbound (tvs, level) =>
                                                               if level > #level ctx andalso List.all isEqualityType tvs then
                                                                   if List.null tvs then
                                                                       let val tv' = genTyVar (#context ctx, Syntax.MkTyVar "'?")
                                                                       in tv := T.Link (T.TyVar (span, tv'))
                                                                        ; (tv', []) :: vars
                                                                       end
                                                                   else
                                                                       let val tv' = genTyVar (#context ctx, Syntax.MkTyVar "''?")
                                                                       in tv := T.Link (T.TyVar (span, tv'))
                                                                        ; (tv', [T.IsEqType]) :: vars
                                                                       end
                                                               else if level > #level ctx then
                                                                   (tv := T.Unbound (tvs, #level ctx); vars)
                                                               else
                                                                   vars
                                                             | T.Link _ => vars
                                                       ) [] aTyVars_ty
                              fun doTyVar tv = if T.tyVarAdmitsEquality tv then
                                                   (tv, [T.IsEqType])
                                               else
                                                   (tv, [])
                              val tysc = T.TypeScheme (List.map doTyVar (T.TyVarSet.listItems tyVars) @ aTyVars, ty)
                          in (vid, tysc, aTyVars)
                          end
                    val valEnv' = Syntax.VIdMap.map doVal valEnv
                    val aTyVars = Syntax.VIdMap.foldl (fn ((_, _, aTyVars), acc) => List.foldl (fn ((tv, _), acc) => T.TyVarSet.add (acc, tv)) acc aTyVars) T.TyVarSet.empty valEnv'
                    val valbinds = Syntax.VIdMap.foldr (fn ((vid, tysc, _), rest) => (span, vid, tysc, exp) :: rest) valbinds valEnv'
                in (valbinds, Syntax.VIdMap.unionWith #2 (valEnv', valEnvRest), T.TyVarSet.union (aTyVars, tyVarsAcc))
                end
          val (valbinds, valEnv, allTyVars) = List.foldr generalize ([], Syntax.VIdMap.empty, T.TyVarSet.fromList (List.map #2 tyvarseq')) valbinds''
          fun fixRecursion (span, vid, tysc as T.TypeScheme (tyvars, ty), exp)
              = let val unboundTyVars = T.TyVarSet.foldl (fn (tv, acc) =>
                                                             if List.exists (fn (tv', _) => tv = tv') tyvars then
                                                                 acc
                                                             else
                                                                 T.TyVarMap.insert (acc, tv, T.AnonymousTyVar (span, freshTyVar (ctx, span, if T.tyVarAdmitsEquality tv then [T.IsEqType] else [])))
                                                         ) T.TyVarMap.empty allTyVars
                    val exp = #doExp (T.applySubstTyInExpOrDec unboundTyVars) exp
                    val subst = List.foldl (fn ((_, vid', T.TypeScheme (tyvars', _), _), subst) =>
                                               T.VIdMap.insert (subst, vid', fn (span, idstatus as Syntax.ValueVariable, []) =>
                                                                                let val tyargs' = List.map (fn (tv, c) => case T.TyVarMap.find (unboundTyVars, tv) of
                                                                                                                              NONE => (T.TyVar (span, tv), c)
                                                                                                                            | SOME a => (a, c)) tyvars'
                                                                                in T.VarExp (span, T.MkShortVId vid', idstatus, tyargs')
                                                                                end
                                                                           | (_, _, _) => emitTypeError (ctx, [span], "invalid use of recursive identifier"))
                                           ) T.VIdMap.empty valbinds
                    val exp = #doExp (T.substVId subst) exp
                in T.PolyVarBind (span, vid, tysc, exp)
                end
          val valbinds' = List.map fixRecursion valbinds
          val env' = envWithValEnv (Syntax.VIdMap.map (fn (vid, tysc, _) => (tysc, Syntax.ValueVariable, TypedSyntax.MkShortVId vid)) valEnv)
      in (env', [T.RecValDec (span, valbinds')])
      end
  | typeCheckDec(ctx, env, S.TypeDec(span, typbinds))
    = let fun doTypBind (S.TypBind(span, tyvars, tycon, ty), (tyConEnv, typbinds))
              = let val tyvars = List.map (fn tv => (tv, genTyVar (#context ctx, tv))) tyvars
                    val env' = { valMap = #valMap env
                               , tyConMap = #tyConMap env
                               , tyNameMap = #tyNameMap env
                               , strMap = #strMap env
                               , sigMap = #sigMap env
                               , funMap = #funMap env
                               , boundTyVars = List.foldl Syntax.TyVarMap.insert' (#boundTyVars env) tyvars
                               }
                    val ty = evalTy (#context ctx, env', ty)
                    val tystr : T.TypeStructure = { typeFunction = T.TypeFunction (List.map #2 tyvars, ty)
                                                  , valEnv = T.emptyValEnv
                                                  }
                in (Syntax.TyConMap.insert (tyConEnv, tycon, tystr) (* TODO: error on duplicate *), T.TypBind (span, List.map #2 tyvars, tycon, ty) :: typbinds)
                end
          val (tyConEnv, typbinds) = List.foldr doTypBind (Syntax.TyConMap.empty, []) typbinds
      in (envWithTyConEnv (tyConEnv, TypedSyntax.TyNameMap.empty), [T.TypeDec (span, typbinds)])
      end
  | typeCheckDec(ctx, env, S.DatatypeDec(span, datbinds, typbinds))
    = let val datbinds = let val goConBind = doWithtype (#context ctx, env, typbinds)
                         in List.map (fn S.DatBind(span, tyvars, tycon, conbinds) => S.DatBind(span, tyvars, tycon, List.map goConBind conbinds)) datbinds
                         end
          val equalityMap : bool S.TyConMap.map = determineDatatypeEquality (#context ctx, env, List.foldl (fn (S.DatBind (_, tyvars, tycon, conbinds), m) => S.TyConMap.insert (m, tycon, (tyvars, List.mapPartial (fn S.ConBind (_, _, optTy) => optTy) conbinds))) S.TyConMap.empty datbinds)
          val datbinds = List.map (fn datbind as S.DatBind (span, tyvars, tycon, conbinds) => (datbind, newTyName (#context ctx, tycon))) datbinds
          val partialEnv = envWithTyConEnv (List.foldl (fn ((S.DatBind(span, tyvars, tycon, conbinds), tycon'), (m, m')) =>
                                                           let val tyvars = List.map (fn tv => genTyVar (#context ctx, tv)) tyvars
                                                               val tystr = { typeFunction = T.TypeFunction (tyvars, T.TyCon (span, List.map (fn tv => T.TyVar (span, tv)) tyvars, tycon'))
                                                                           , valEnv = T.emptyValEnv
                                                                           }
                                                               val tynameattr = { arity = List.length tyvars
                                                                                , admitsEquality = S.TyConMap.lookup(equalityMap, tycon)
                                                                                , overloadClass = NONE
                                                                                }
                                                           in (Syntax.TyConMap.insert (m, tycon, tystr), TypedSyntax.TyNameMap.insert (m', tycon', tynameattr))
                                                           end
                                                       ) (Syntax.TyConMap.empty, TypedSyntax.TyNameMap.empty) datbinds)
          val (tyConMap, tyNameMap, valMap, datbinds)
              = let fun doDatBind ((S.DatBind(span, tyvars, tycon, conbinds), tyname), (tyConMap, tyNameMap, accValEnv, datbinds))
                        = let val tyvars = List.map (fn tv => (tv, genTyVar (#context ctx, tv))) tyvars
                              val env = mergeEnv(env, { valMap = #valMap partialEnv
                                                      , tyConMap = #tyConMap partialEnv
                                                      , tyNameMap = #tyNameMap partialEnv
                                                      , strMap = #strMap partialEnv
                                                      , sigMap = #sigMap partialEnv
                                                      , funMap = #funMap partialEnv
                                                      , boundTyVars = List.foldl Syntax.TyVarMap.insert' (#boundTyVars partialEnv) tyvars
                                                      }
                                                )
                              val tyvars = List.map #2 tyvars
                              val allConstructors = List.foldl (fn (Syntax.ConBind (span, vid, _), set) => Syntax.VIdSet.add (set, vid)) Syntax.VIdSet.empty conbinds
                              val (valEnv, conbinds) = List.foldr (fn (S.ConBind(span, vid, optTy), (valEnv, conbinds)) =>
                                                                      let val vid' = newVId (#context ctx, vid)
                                                                          val optTy = Option.map (fn ty => evalTy (#context ctx, env, ty)) optTy
                                                                          val info = { tag = Syntax.getVIdName vid
                                                                                     , allConstructors = allConstructors
                                                                                     , representation = Syntax.REP_BOXED
                                                                                     }
                                                                          val idstatus = Syntax.ValueConstructor info
                                                                          val conbind = T.ConBind (span, vid', optTy, info)
                                                                          val tysc = T.TypeScheme (List.map (fn tv => (tv, [])) tyvars, case optTy of
                                                                                                                                            NONE => T.TyCon (span, List.map (fn tv => T.TyVar (span, tv)) tyvars, tyname)
                                                                                                                                          | SOME payloadTy => T.FnType (span, payloadTy, T.TyCon (span, List.map (fn tv => T.TyVar (span, tv)) tyvars, tyname))
                                                                                                  )
                                                                      in (Syntax.VIdMap.insert (valEnv, vid, (tysc, idstatus, T.MkShortVId vid')) (* TODO: check for duplicate *), conbind :: conbinds)
                                                                      end
                                                                  ) (Syntax.VIdMap.empty, []) conbinds
                              val datbind = T.DatBind (span, tyvars, tyname, conbinds, S.TyConMap.lookup (equalityMap, tycon))
                              val tystr = { typeFunction = T.TypeFunction (tyvars, T.TyCon (span, List.map (fn tv => T.TyVar (span, tv)) tyvars, tyname))
                                          , valEnv = Syntax.VIdMap.map (fn (tysc, ids, _) => (tysc, ids)) valEnv
                                          }
                              val tynameattr = { arity = List.length tyvars
                                               , admitsEquality = Syntax.TyConMap.lookup(equalityMap, tycon)
                                               , overloadClass = NONE
                                               }
                          in (Syntax.TyConMap.insert (tyConMap, tycon, tystr), TypedSyntax.TyNameMap.insert (tyNameMap, tyname, tynameattr), Syntax.VIdMap.unionWith #2 (accValEnv, valEnv) (* TODO: check for duplicate *), datbind :: datbinds)
                          end
                in List.foldr doDatBind (Syntax.TyConMap.empty, TypedSyntax.TyNameMap.empty, Syntax.VIdMap.empty, []) datbinds
                end
          val (tyConMap, typbinds) = let fun doTypBind (S.TypBind(span, tyvars, tycon, ty), (tyConEnv, typbinds))
                                             = let val tyvars = List.map (fn tv => (tv, genTyVar (#context ctx, tv))) tyvars
                                                   val env' = { valMap = Syntax.VIdMap.unionWith #2 (#valMap env, valMap)
                                                              , tyConMap = Syntax.TyConMap.unionWith #2 (#tyConMap env, tyConMap)
                                                              , tyNameMap = TypedSyntax.TyNameMap.unionWith #2 (#tyNameMap env, tyNameMap)
                                                              , strMap = #strMap env
                                                              , sigMap = #sigMap env
                                                              , funMap = #funMap env
                                                              , boundTyVars = List.foldl Syntax.TyVarMap.insert' (#boundTyVars env) tyvars
                                                              }
                                                   val ty = evalTy (#context ctx, env', ty)
                                                   val tystr : T.TypeStructure = { typeFunction = T.TypeFunction (List.map #2 tyvars, ty)
                                                                                 , valEnv = T.emptyValEnv
                                                                                 }
                                               in (Syntax.TyConMap.insert (tyConEnv, tycon, tystr) (* TODO: error on duplicate *), T.TypBind (span, List.map #2 tyvars, tycon, ty) :: typbinds)
                                               end
                                     in List.foldr doTypBind (tyConMap, []) typbinds
                                     end
          val env' = { valMap = valMap
                     , tyConMap = tyConMap
                     , tyNameMap = tyNameMap
                     , strMap = Syntax.StrIdMap.empty
                     , sigMap = Syntax.SigIdMap.empty
                     , funMap = Syntax.FunIdMap.empty
                     , boundTyVars = Syntax.TyVarMap.empty
                     }
      in (env', if List.null typbinds then
                    [T.DatatypeDec (span, datbinds)]
                else
                    [T.DatatypeDec (span, datbinds), T.TypeDec (span, typbinds)]
         )
      end
  | typeCheckDec(ctx, env, S.DatatypeRepDec(span, tycon, longtycon))
    = let val tystr = lookupTyConInEnv (#context ctx, env, span, longtycon)
          val getLongVId = case longtycon of
                               Syntax.MkQualified([], _) =>
                               (fn vid => case Syntax.VIdMap.find(#valMap env, vid) of
                                              SOME (_, _, longvid) => longvid
                                            | NONE => emitTypeError (ctx, [span], "datatype replication: value identifier " ^ Syntax.print_VId vid ^ " not found (internal error)")
                               )
                             | Syntax.MkQualified(strid0 :: strids, _) =>
                               case Syntax.StrIdMap.find(#strMap env, strid0) of
                                   SOME (s, T.MkLongStrId (strid0, strids0)) => (fn vid => T.MkLongVId (strid0, strids0 @ strids, vid))
                                 | NONE => emitTypeError (ctx, [span], "datatype replication: structure " ^ Syntax.print_StrId strid0 ^ " not found (internal error)")
          val env' = { valMap = Syntax.VIdMap.mapi (fn (vid, (tysc, ids)) => (tysc, ids, getLongVId vid)) (#valEnv tystr)
                     , tyConMap = Syntax.TyConMap.singleton(tycon, tystr)
                     , tyNameMap = TypedSyntax.TyNameMap.empty
                     , strMap = Syntax.StrIdMap.empty
                     , sigMap = Syntax.SigIdMap.empty
                     , funMap = Syntax.FunIdMap.empty
                     , boundTyVars = Syntax.TyVarMap.empty
                     }
      in (env', [])
      end
  | typeCheckDec(ctx, env, S.AbstypeDec(span, datbinds, typbinds, decs))
    = let val (env', datbinds') = typeCheckDec(ctx, env, S.DatatypeDec(span, datbinds, typbinds))
          val (env'', decs') = typeCheckDecs(ctx, mergeEnv(env, env'), decs)
          val resultingEnv = { valMap = #valMap env''
                             , tyConMap = Syntax.TyConMap.unionWith #2 (Syntax.TyConMap.map (fn { typeFunction, valEnv = _ } => { typeFunction = typeFunction, valEnv = T.emptyValEnv }) (#tyConMap env'), #tyConMap env'')
                             , tyNameMap = TypedSyntax.TyNameMap.unionWith #2 (#tyNameMap env', #tyNameMap env'') (* should be disjoint *)
                             , strMap = Syntax.StrIdMap.unionWith #2 (#strMap env', #strMap env'') (* should be empty *)
                             , sigMap = Syntax.SigIdMap.unionWith #2 (#sigMap env', #sigMap env'') (* should be empty *)
                             , funMap = Syntax.FunIdMap.unionWith #2 (#funMap env', #funMap env'')
                             , boundTyVars = Syntax.TyVarMap.unionWith #2 (#boundTyVars env', #boundTyVars env'') (* should be empty *)
                             }
      in (resultingEnv, datbinds' @ decs')
      end
  | typeCheckDec(ctx, env, S.ExceptionDec(span, exbinds))
    = let fun doExBind(S.ExBind(span, vid, optTy), (valMap, exbinds))
              = let val optTy = Option.map (fn ty => evalTy (#context ctx, env, ty)) optTy
                    val vid' = newVId (#context ctx, vid)
                    val valMap = S.VIdMap.insert (valMap, vid, (T.TypeScheme ([], case optTy of
                                                                                      NONE => primTy_exn
                                                                                    | SOME ty => T.FnType (span, ty, primTy_exn)
                                                                             ), Syntax.ExceptionConstructor, T.MkShortVId vid'))
                in (valMap, T.ExBind (span, vid', optTy) :: exbinds)
                end
            | doExBind(S.ExReplication(span, vid, longvid), (valMap, exbinds))
              = let val vid' = newVId (#context ctx, vid)
                in case lookupLongVIdInEnv(ctx, env, span, longvid) of
                       Found (longvid, tysc, ids as Syntax.ExceptionConstructor) =>
                       let val optTy = case tysc of
                                           T.TypeScheme ([], T.FnType (_, payloadTy, _)) => SOME payloadTy
                                         | T.TypeScheme ([], _) => NONE
                                         | T.TypeScheme (_ :: _, _) => emitTypeError (ctx, [span], "exception constructor must have monomorphic type")
                       in (S.VIdMap.insert (valMap, vid, (tysc, ids, T.MkShortVId vid')), T.ExReplication (span, vid', longvid, optTy) :: exbinds)
                       end
                     | Found _ => emitTypeError (ctx, [span], "exception replication: RHS must be an exception constructor")
                     | ValueNotFound notfound => emitTypeError (ctx, [span], "exception replication: RHS must be an exception constructor, but value name '" ^ Syntax.print_LongVId notfound ^ "' was not found")
                     | StructureNotFound notfound => emitTypeError (ctx, [span], "exception replication: RHS must be an exception constructor, but structure name '" ^ Syntax.print_LongStrId notfound ^ "' was not found")
                end
          val (valMap, exbinds) = List.foldr doExBind (Syntax.VIdMap.empty, []) exbinds
      in (envWithValEnv valMap, [T.ExceptionDec (span, exbinds)])
      end
  | typeCheckDec(ctx, env, S.LocalDec(span, decs1, decs2))
    = let val (env', decs1) = typeCheckDecs(ctx, env, decs1)
          val (env'', decs2) = typeCheckDecs(ctx, mergeEnv(env, env'), decs2)
          val env'' = { valMap = #valMap env''
                      , tyConMap = #tyConMap env''
                      , tyNameMap = TypedSyntax.TyNameMap.unionWith #2 (#tyNameMap env', #tyNameMap env'')
                      , strMap = #strMap env''
                      , sigMap = #sigMap env''
                      , funMap = #funMap env''
                      , boundTyVars = #boundTyVars env''
                      }
      in (env'', case decs1 @ decs2 of
                     decs as [] => decs
                   | decs as [dec] => decs
                   | decs => [T.GroupDec (span, decs)])
      end
  | typeCheckDec(ctx, env, S.OpenDec(span, longstrids))
    = let fun getStructure(Syntax.MkQualified([], strid))
              = (case Syntax.StrIdMap.find(#strMap env, strid) of
                     SOME (s, T.MkLongStrId (strid0, strids0)) => { valMap = Syntax.VIdMap.mapi (fn (vid, (tysc, ids)) => (tysc, ids, T.MkLongVId (strid0, strids0, vid))) (#valMap s)
                                                                  , tyConMap = #tyConMap s
                                                                  , tyNameMap = TypedSyntax.TyNameMap.empty
                                                                  , strMap = Syntax.StrIdMap.mapi (fn (strid', T.MkSignature s) => (s, T.MkLongStrId (strid0, strids0 @ [strid']))) (#strMap s)
                                                                  , sigMap = Syntax.SigIdMap.empty
                                                                  , funMap = Syntax.FunIdMap.empty
                                                                  , boundTyVars = Syntax.TyVarMap.empty
                                                                  }
                   | NONE => emitTypeError (ctx, [span], "structure not found")
                )
            | getStructure(Syntax.MkQualified(strid0 :: strids, strid'))
              = (case Syntax.StrIdMap.find(#strMap env, strid0) of
                     SOME (s0, T.MkLongStrId (strid0, strids0)) => let val s = lookupStr (#context ctx, s0, span, strids @ [strid'])
                                                                       val strids = strids0 @ strids @ [strid']
                                                                   in { valMap = Syntax.VIdMap.mapi (fn (vid, (tysc, ids)) => (tysc, ids, T.MkLongVId (strid0, strids, vid))) (#valMap s)
                                                                      , tyConMap = #tyConMap s
                                                                      , tyNameMap = TypedSyntax.TyNameMap.empty
                                                                      , strMap = Syntax.StrIdMap.mapi (fn (strid', T.MkSignature s) => (s, T.MkLongStrId (strid0, strids @ [strid']))) (#strMap s)
                                                                      , sigMap = Syntax.SigIdMap.empty
                                                                      , funMap = Syntax.FunIdMap.empty
                                                                      , boundTyVars = Syntax.TyVarMap.empty
                                                                      }
                                                                   end
                   | NONE => emitTypeError (ctx, [span], "structure not found")
                )
          val env = List.foldl (fn (longstrid, acc) => mergeEnv(acc, getStructure longstrid)) emptyEnv longstrids
      in (env, [])
      end
  | typeCheckDec(ctx, env, S.OverloadDec(span, class, longtycon, map))
    = let val { typeFunction = T.TypeFunction (tyvars, ty), ... } = lookupTyConInEnv (#context ctx, env, span, longtycon)
          val tyname = if List.null tyvars then
                           case ty of
                               T.TyCon (_, [], tyname) => tyname
                             | _ => emitTypeError (ctx, [span], "overload declaration: longtycon must refer to a concrete type")
                       else
                           emitTypeError (ctx, [span], "overload declaration: longtycon must refer to a concrete type")
          val ty = T.TyCon (span, [], tyname)
          val map : (T.Ty * T.Exp) Syntax.OverloadKeyMap.map = Syntax.OverloadKeyMap.map (fn exp => typeCheckExp (ctx, env, exp, NONE)) map
          val () = case Syntax.OverloadKeyMap.find(map, Syntax.OVERLOAD_abs) of
                       NONE => ()
                     | SOME (ty', _) => addConstraint (ctx, env, T.EqConstr (span, ty', T.FnType (span, ty, ty)))
          val () = case Syntax.OverloadKeyMap.find(map, Syntax.OVERLOAD_TILDE) of
                       NONE => ()
                     | SOME (ty', _) => addConstraint (ctx, env, T.EqConstr (span, ty', T.FnType (span, ty, ty)))
          val () = case Syntax.OverloadKeyMap.find(map, Syntax.OVERLOAD_div) of
                       NONE => ()
                     | SOME (ty', _) => addConstraint (ctx, env, T.EqConstr (span, ty', T.FnType (span, T.PairType (span, ty, ty), ty)))
          val () = case Syntax.OverloadKeyMap.find(map, Syntax.OVERLOAD_mod) of
                       NONE => ()
                     | SOME (ty', _) => addConstraint (ctx, env, T.EqConstr (span, ty', T.FnType (span, T.PairType (span, ty, ty), ty)))
          val () = case Syntax.OverloadKeyMap.find(map, Syntax.OVERLOAD_TIMES) of
                       NONE => ()
                     | SOME (ty', _) => addConstraint (ctx, env, T.EqConstr (span, ty', T.FnType (span, T.PairType (span, ty, ty), ty)))
          val () = case Syntax.OverloadKeyMap.find(map, Syntax.OVERLOAD_DIVIDE) of
                       NONE => ()
                     | SOME (ty', _) => addConstraint (ctx, env, T.EqConstr (span, ty', T.FnType (span, T.PairType (span, ty, ty), ty)))
          val () = case Syntax.OverloadKeyMap.find(map, Syntax.OVERLOAD_PLUS) of
                       NONE => ()
                     | SOME (ty', _) => addConstraint (ctx, env, T.EqConstr (span, ty', T.FnType (span, T.PairType (span, ty, ty), ty)))
          val () = case Syntax.OverloadKeyMap.find(map, Syntax.OVERLOAD_MINUS) of
                       NONE => ()
                     | SOME (ty', _) => addConstraint (ctx, env, T.EqConstr (span, ty', T.FnType (span, T.PairType (span, ty, ty), ty)))
          val () = case Syntax.OverloadKeyMap.find(map, Syntax.OVERLOAD_LT) of
                       NONE => ()
                     | SOME (ty', _) => addConstraint (ctx, env, T.EqConstr (span, ty', T.FnType (span, T.PairType (span, ty, ty), primTy_bool)))
          val () = case Syntax.OverloadKeyMap.find(map, Syntax.OVERLOAD_LE) of
                       NONE => ()
                     | SOME (ty', _) => addConstraint (ctx, env, T.EqConstr (span, ty', T.FnType (span, T.PairType (span, ty, ty), primTy_bool)))
          val () = case Syntax.OverloadKeyMap.find(map, Syntax.OVERLOAD_GT) of
                       NONE => ()
                     | SOME (ty', _) => addConstraint (ctx, env, T.EqConstr (span, ty', T.FnType (span, T.PairType (span, ty, ty), primTy_bool)))
          val () = case Syntax.OverloadKeyMap.find(map, Syntax.OVERLOAD_GE) of
                       NONE => ()
                     | SOME (ty', _) => addConstraint (ctx, env, T.EqConstr (span, ty', T.FnType (span, T.PairType (span, ty, ty), primTy_bool)))
          val () = case Syntax.OverloadKeyMap.find(map, Syntax.OVERLOAD_fromInt) of
                       NONE => ()
                     | SOME (ty', _) => addConstraint (ctx, env, T.EqConstr (span, ty', T.FnType (span, primTy_int, ty)))
          val () = case Syntax.OverloadKeyMap.find(map, Syntax.OVERLOAD_fromWord) of
                       NONE => ()
                     | SOME (ty', _) => addConstraint (ctx, env, T.EqConstr (span, ty', T.FnType (span, primTy_word, ty)))
          val attr = lookupTyNameInEnv (#context ctx, env, span, tyname)
          val attr = { arity = #arity attr
                     , admitsEquality = #admitsEquality attr
                     , overloadClass = SOME class
                     }
          val env' = envWithTyConEnv (Syntax.TyConMap.empty, TypedSyntax.TyNameMap.singleton (tyname, attr))
      in (env', [T.OverloadDec (span, class, tyname, Syntax.OverloadKeyMap.map #2 map)])
      end
  | typeCheckDec (ctx, env, S.EqualityDec (span, typarams, longtycon, exp))
    = let val { typeFunction = T.TypeFunction (tyvars, ty), ... } = lookupTyConInEnv (#context ctx, env, span, longtycon)
          val tyname = if List.length tyvars = List.length typarams then
                           case ty of
                               T.TyCon (_, tyvars', tyname) => if List.length tyvars' = List.length typarams then
                                                                   tyname
                                                               else
                                                                   emitTypeError (ctx, [span], "equality declaration: longtycon must refer to a concrete type")
                             | _ => emitTypeError (ctx, [span], "equality declaration: longtycon must refer to a concrete type")
                       else
                           emitTypeError (ctx, [span], "equality declaration: number of type parameters mismatch")
          val typarams' = List.map (fn tv => (tv, genTyVar (#context ctx, tv))) typarams
          val () = if isRefOrArray tyname then
                       List.app (fn Syntax.MkTyVar tvname => if String.isPrefix "''" tvname then
                                                                 emitTypeError (ctx, [span], "equality for ref or array cannot depend on the equality of its type parameter")
                                                             else
                                                                 ()
                                ) typarams
                   else
                       ()
          val ty = T.TyCon (span, List.map (fn (_, tv) => T.TyVar (span, tv)) typarams', tyname)
          val attr = lookupTyNameInEnv (#context ctx, env, span, tyname)
          val () = if #admitsEquality attr then
                       emitTypeError (ctx, [span], "equality declaration: the type already admits equality")
                   else
                       ()
          val attr = { arity = #arity attr
                     , admitsEquality = true
                     , overloadClass = #overloadClass attr
                     }
          val env' = envWithTyConEnv (Syntax.TyConMap.empty, TypedSyntax.TyNameMap.singleton (tyname, attr))
          val innerEnv = mergeEnv (env, env')
          val innerEnv = { valMap = #valMap innerEnv
                         , tyConMap = #tyConMap innerEnv
                         , tyNameMap = #tyNameMap innerEnv
                         , strMap = #strMap innerEnv
                         , sigMap = #sigMap innerEnv
                         , funMap = #funMap innerEnv
                         , boundTyVars = List.foldl Syntax.TyVarMap.insert' (#boundTyVars innerEnv) typarams'
                         }
          val (eqTy, exp) = typeCheckExp (ctx, innerEnv, exp, SOME (T.FnType (span, T.PairType (span, ty, ty), primTy_bool))) (* allow recursion *)
          val () = addConstraint (ctx, env, T.EqConstr (span, eqTy, T.FnType (span, T.PairType (span, ty, ty), primTy_bool)))
      in (env', [T.EqualityDec (span, List.map #2 typarams', tyname, exp)])
      end
(* typeCheckDecs : InferenceContext * Env * S.Dec list -> (* created environment *) Env * T.Dec list *)
and typeCheckDecs (ctx, env, []) : Env * T.Dec list = (emptyEnv, [])
  | typeCheckDecs(ctx, env, dec :: decs) = let val (env', dec) = typeCheckDec(ctx, env, dec)
                                               val (env'', decs) = typeCheckDecs(ctx, mergeEnv(env, env'), decs)
                                           in (mergeEnv(env', env''), dec @ decs)
                                           end
 (* typeCheckMatch : InferenceContext * Env * SourcePos.span * (S.Pat * S.Exp) list * (* pattern type hint *) T.Ty option * (* expression type hint *) T.Ty option -> (* pattern *) Syntax.Ty * (* expression *) Syntax.Ty * (Pat * Exp) list *)
and typeCheckMatch (ctx, env, span, (pat0, exp0) :: rest, patTyHint : T.Ty option, expTyHint : T.Ty option) : T.Ty * T.Ty * (T.Pat * T.Exp) list
    = let val (patTy, expTy, pat0', exp0') = typeCheckMatchBranch (ctx, env, pat0, exp0, patTyHint, expTyHint)
          fun oneBranch(pat, exp)
              = let val (patTy', expTy', pat', exp') = typeCheckMatchBranch (ctx, env, pat, exp, SOME patTy, SOME expTy)
                in addConstraint (ctx, env, T.EqConstr (span, patTy, patTy'))
                 ; addConstraint (ctx, env, T.EqConstr (span, expTy, expTy'))
                 ; (pat', exp')
                end
          val rest' = List.map oneBranch rest
      in (patTy, expTy, (pat0', exp0') :: rest')
      end
  | typeCheckMatch (ctx, env, span, nil, _, _) = emitTypeError (ctx, [span], "invalid syntax tree: match is empty")
and typeCheckMatchBranch (ctx : InferenceContext, env : Env, pat : S.Pat, exp : S.Exp, patTyHint, expTyHint) : T.Ty * T.Ty * T.Pat * T.Exp
    = let val (patTy, vars, pat') = typeCheckPat (ctx, env, pat, patTyHint)
          val env' = mergeEnv (env, envWithValEnv (Syntax.VIdMap.map (fn (vid, ty) => (T.TypeScheme ([], ty), Syntax.ValueVariable, T.MkShortVId vid)) vars))
          val (expTy, exp') = typeCheckExp (ctx, env', exp, expTyHint)
      in (patTy, expTy, pat', exp')
      end

(* pretty printing *)
          (*
structure PrettyPrint = struct
fun print_Env ({ tyConMap, valMap, strMap, boundTyVars, ... } : Env) = "Env{tyMap=" ^ TypedSyntax.print_TyConMap (fn _ => "TypeStructure _") tyConMap ^ ",valMap=" ^ TypedSyntax.print_VIdMap (Syntax.print_pair (TypedSyntax.print_TypeScheme, Syntax.print_IdStatus)) valMap ^ ",strMap=...,boundTyVars=...}"
end (* structure PrettyPrint *)
open PrettyPrint
          *)
(* applyDefaultTypes : Context * TypedSyntax.Dec list -> unit *)
fun applyDefaultTypes (ctx, decs : T.Dec list) : unit =
    let fun findClass [] = NONE
          | findClass ((span, TypedSyntax.IsInt) :: _) = SOME (span, Syntax.CLASS_INT)
          | findClass ((span, TypedSyntax.IsWord) :: _) = SOME (span, Syntax.CLASS_WORD)
          | findClass ((span, TypedSyntax.IsReal) :: _) = SOME (span, Syntax.CLASS_REAL)
          | findClass ((span, TypedSyntax.IsChar) :: _) = SOME (span, Syntax.CLASS_CHAR)
          | findClass ((span, TypedSyntax.IsString) :: _) = SOME (span, Syntax.CLASS_STRING)
          | findClass (_ :: xs) = findClass xs
        fun doInt (span1, []) = primTy_int
          | doInt (span1, (span2, c) :: xs)
            = case c of
                  TypedSyntax.NoField _ => emitError (ctx, [span1, span2], "invalid record syntax for int")
                | TypedSyntax.IsRecord => emitError (ctx, [span1, span2], "invalid record syntax for int")
                | TypedSyntax.IsEqType => doInt (span1, xs)
                | TypedSyntax.IsIntegral => doInt (span1, xs)
                | TypedSyntax.IsSignedReal => doInt (span1, xs)
                | TypedSyntax.IsRing => doInt (span1, xs)
                | TypedSyntax.IsField => emitError (ctx, [span1, span2], "cannot apply / operator on ints")
                | TypedSyntax.IsSigned => doInt (span1, xs)
                | TypedSyntax.IsOrdered => doInt (span1, xs)
                | TypedSyntax.IsInt => doInt (span1, xs)
                | TypedSyntax.IsWord => emitError (ctx, [span1, span2], "type mismatch: int vs word")
                | TypedSyntax.IsReal => emitError (ctx, [span1, span2], "type mismatch: int vs real")
                | TypedSyntax.IsChar => emitError (ctx, [span1, span2], "type mismatch: int vs char")
                | TypedSyntax.IsString => emitError (ctx, [span1, span2], "type mismatch: int vs string")
        fun doWord (span1, []) = primTy_word
          | doWord (span1, (span2, c) :: xs)
            = case c of
                  TypedSyntax.NoField _ => emitError (ctx, [span1, span2], "invalid record syntax for word")
                | TypedSyntax.IsRecord => emitError (ctx, [span1, span2], "invalid record syntax for word")
                | TypedSyntax.IsEqType => doWord (span1, xs)
                | TypedSyntax.IsIntegral => doWord (span1, xs)
                | TypedSyntax.IsSignedReal => emitError (ctx, [span1, span2], "abs is invalid for word")
                | TypedSyntax.IsRing => doWord (span1, xs)
                | TypedSyntax.IsField => emitError (ctx, [span1, span2], "cannot apply / operator on words")
                | TypedSyntax.IsSigned => doWord (span1, xs)
                | TypedSyntax.IsOrdered => doWord (span1, xs)
                | TypedSyntax.IsInt => emitError (ctx, [span1, span2], "type mismatch: word vs int")
                | TypedSyntax.IsWord => doWord (span1, xs)
                | TypedSyntax.IsReal => emitError (ctx, [span1, span2], "type mismatch: word vs real")
                | TypedSyntax.IsChar => emitError (ctx, [span1, span2], "type mismatch: word vs char")
                | TypedSyntax.IsString => emitError (ctx, [span1, span2], "type mismatch: word vs string")
        fun doReal (span1, []) = primTy_real
          | doReal (span1, (span2, c) :: xs)
            = case c of
                  TypedSyntax.NoField _ => emitError (ctx, [span1, span2], "invalid record syntax for real")
                | TypedSyntax.IsRecord => emitError (ctx, [span1, span2], "invalid record syntax for real")
                | TypedSyntax.IsEqType => emitError (ctx, [span1, span2], "real does not admit equality")
                | TypedSyntax.IsIntegral => emitError (ctx, [span1, span2], "div, mod are invalid for real")
                | TypedSyntax.IsSignedReal => doReal (span1, xs)
                | TypedSyntax.IsRing => doReal (span1, xs)
                | TypedSyntax.IsField => doReal (span1, xs)
                | TypedSyntax.IsSigned => doReal (span1, xs)
                | TypedSyntax.IsOrdered => doReal (span1, xs)
                | TypedSyntax.IsInt => emitError (ctx, [span1, span2], "type mismatch: real vs int")
                | TypedSyntax.IsWord => emitError (ctx, [span1, span2], "type mismatch: real vs word")
                | TypedSyntax.IsReal => doReal (span1, xs)
                | TypedSyntax.IsChar => emitError (ctx, [span1, span2], "type mismatch: real vs char")
                | TypedSyntax.IsString => emitError (ctx, [span1, span2], "type mismatch: real vs string")
        fun doChar (span1, []) = primTy_char
          | doChar (span1, (span2, c) :: xs)
            = case c of
                  TypedSyntax.NoField _ => emitError (ctx, [span1, span2], "invalid record syntax for char")
                | TypedSyntax.IsRecord => emitError (ctx, [span1, span2], "invalid record syntax for char")
                | TypedSyntax.IsEqType => doChar (span1, xs)
                | TypedSyntax.IsIntegral => emitError (ctx, [span1, span2], "invalid operation on char")
                | TypedSyntax.IsSignedReal => emitError (ctx, [span1, span2], "invalid operation on char")
                | TypedSyntax.IsRing => emitError (ctx, [span1, span2], "invalid operation on char")
                | TypedSyntax.IsField => emitError (ctx, [span1, span2], "invalid operation on char")
                | TypedSyntax.IsSigned => emitError (ctx, [span1, span2], "invalid operation on char")
                | TypedSyntax.IsOrdered => doChar (span1, xs)
                | TypedSyntax.IsInt => emitError (ctx, [span1, span2], "type mismatch: char vs int")
                | TypedSyntax.IsWord => emitError (ctx, [span1, span2], "type mismatch: char vs word")
                | TypedSyntax.IsReal => emitError (ctx, [span1, span2], "type mismatch: char vs real")
                | TypedSyntax.IsChar => doChar (span1, xs)
                | TypedSyntax.IsString => emitError (ctx, [span1, span2], "type mismatch: char vs string")
        fun doString (span1, []) = primTy_string
          | doString (span1, (span2, c) :: xs)
            = case c of
                  TypedSyntax.NoField _ => emitError (ctx, [span1, span2], "invalid record syntax for string")
                | TypedSyntax.IsRecord => emitError (ctx, [span1, span2], "invalid record syntax for string")
                | TypedSyntax.IsEqType => doString (span1, xs)
                | TypedSyntax.IsIntegral => emitError (ctx, [span1, span2], "invalid operation on string")
                | TypedSyntax.IsSignedReal => emitError (ctx, [span1, span2], "invalid operation on string")
                | TypedSyntax.IsRing => emitError (ctx, [span1, span2], "invalid operation on string")
                | TypedSyntax.IsField => emitError (ctx, [span1, span2], "invalid operation on string")
                | TypedSyntax.IsSigned => emitError (ctx, [span1, span2], "invalid operation on string")
                | TypedSyntax.IsOrdered => doString (span1, xs)
                | TypedSyntax.IsInt => emitError (ctx, [span1, span2], "type mismatch: string vs int")
                | TypedSyntax.IsWord => emitError (ctx, [span1, span2], "type mismatch: string vs word")
                | TypedSyntax.IsReal => emitError (ctx, [span1, span2], "type mismatch: string vs real")
                | TypedSyntax.IsChar => emitError (ctx, [span1, span2], "type mismatch: string vs char")
                | TypedSyntax.IsString => doString (span1, xs)
        fun doIntOrReal (span1, []) = primTy_int
          | doIntOrReal (span1, (span2, c) :: xs)
            = case c of
                  TypedSyntax.NoField _ => emitError (ctx, [span1, span2], "unresolved flex record")
                | TypedSyntax.IsRecord => emitError (ctx, [span1, span2], "unresolved flex record")
                | TypedSyntax.IsEqType => doInt (span1, xs)
                | TypedSyntax.IsIntegral => doInt (span1, xs)
                | TypedSyntax.IsSignedReal => doIntOrReal (span1, xs)
                | TypedSyntax.IsRing => doIntOrReal (span1, xs)
                | TypedSyntax.IsField => doReal (span1, xs)
                | TypedSyntax.IsSigned => doIntOrReal (span1, xs)
                | TypedSyntax.IsOrdered => doIntOrReal (span1, xs)
                | TypedSyntax.IsInt => doInt (span1, xs) (* cannot occur *)
                | TypedSyntax.IsWord => doIntOrReal (span1, xs) (* cannot occur *)
                | TypedSyntax.IsReal => doReal (span1, xs) (* cannot occur *)
                | TypedSyntax.IsChar => doIntOrReal (span1, xs) (* cannot occur *)
                | TypedSyntax.IsString => doIntOrReal (span1, xs) (* cannot occur *)
        fun defaultTyForConstraints (eq, spans, []) = primTy_unit
          | defaultTyForConstraints (eq, spans, (span1, c) :: xs)
            = case c of
                  TypedSyntax.NoField _ => emitError (ctx, [span1], "unresolved flex record")
                | TypedSyntax.IsRecord => emitError (ctx, [span1], "unresolved flex record")
                | TypedSyntax.IsEqType => defaultTyForConstraints (true, if List.null spans then [span1] else spans, xs)
                | TypedSyntax.IsIntegral => doInt (span1, xs)
                | TypedSyntax.IsSignedReal => if eq then doInt (span1, xs) else doIntOrReal (span1, xs)
                | TypedSyntax.IsRing => if eq then doInt (span1, xs) else doIntOrReal (span1, xs)
                | TypedSyntax.IsField => if eq then emitError (ctx, span1 :: spans, "real does not admit equality") else doReal (span1, xs)
                | TypedSyntax.IsSigned => if eq then doInt (span1, xs) else doIntOrReal (span1, xs)
                | TypedSyntax.IsOrdered => if eq then doInt (span1, xs) else doIntOrReal (span1, xs)
                | TypedSyntax.IsInt => doInt (span1, xs) (* cannot occur *)
                | TypedSyntax.IsWord => doWord (span1, xs) (* cannot occur *)
                | TypedSyntax.IsReal => if eq then emitError (ctx, span1 :: spans, "real does not admit equality") else doReal (span1, xs) (* cannot occur *)
                | TypedSyntax.IsChar => doChar (span1, xs) (* cannot occur *)
                | TypedSyntax.IsString => doString (span1, xs) (* cannot occur *)
        fun doTyVar tv = case !tv of
                             T.Link _ => ()
                           | T.Unbound (constraints, _) => let val ty = case findClass constraints of
                                                                            SOME (span, Syntax.CLASS_INT) => doInt (span, constraints)
                                                                          | SOME (span, Syntax.CLASS_WORD) => doWord (span, constraints)
                                                                          | SOME (span, Syntax.CLASS_REAL) => doReal (span, constraints)
                                                                          | SOME (span, Syntax.CLASS_CHAR) => doChar (span, constraints)
                                                                          | SOME (span, Syntax.CLASS_STRING) => doString (span, constraints)
                                                                          | NONE => defaultTyForConstraints (false, [], constraints)
                                                           in tv := T.Link ty
                                                           end
    in List.app doTyVar (TypedSyntax.freeTyVarsInDecs ([], decs))
    end

fun typeCheckCoreDecs (ctx : Context, env, decs) : Env * TypedSyntax.Dec list
    = let val ictx = { context = ctx
                     , level = 0
                     }
          val (env, decs) = typeCheckDecs (ictx, env, decs)
          val () = applyDefaultTypes (ctx, decs)
          val decs = #doDecs (TypedSyntax.forceTyIn ctx) decs
      in (env, decs)
      end

fun checkTyScope (ctx, tvset : T.TyVarSet.set, tynameset : T.TyNameSet.set)
    = let fun goTy (T.TyVar (span, tv))
              = if T.TyVarSet.member (tvset, tv) then
                    ()
                else
                    emitError (ctx, [span], "type variable scope violation: " ^ TypedSyntax.PrettyPrint.print_TyVar tv)
            | goTy (T.AnonymousTyVar (span, ref (T.Link ty))) = goTy ty
            | goTy (T.AnonymousTyVar (span, tv as ref (T.Unbound _)))
              = emitError (ctx, [span], "type variable scope violation: " ^ TypedSyntax.PrettyPrint.print_AnonymousTyVar tv)
            | goTy (T.RecordType (span, fields)) = Syntax.LabelMap.app goTy fields
            | goTy (T.RecordExtType (span, fields, baseTy)) = ( Syntax.LabelMap.app goTy fields; goTy baseTy )
            | goTy (T.TyCon (span, tyargs, tyname))
              = if T.TyNameSet.member (tynameset, tyname) then
                    List.app goTy tyargs
                else
                    emitError (ctx, [span], "type constructor scope violation: " ^ TypedSyntax.PrettyPrint.print_TyName tyname)
            | goTy (T.FnType (span, ty1, ty2)) = ( goTy ty1; goTy ty2 )
          fun goTypeScheme (T.TypeScheme (typarams, ty)) = #goTy (checkTyScope (ctx, T.TyVarSet.addList (tvset, List.map #1 typarams), tynameset)) ty
          fun goPat (T.WildcardPat _) = ()
            | goPat (T.SConPat _) = ()
            | goPat (T.VarPat (_, _, ty)) = goTy ty
            | goPat (T.RecordPat { sourceSpan, fields, ellipsis }) = ( List.app (fn (label, pat) => goPat pat) fields
                                                                     ; Option.app goPat ellipsis
                                                                     )
            | goPat (T.ConPat { sourceSpan, longvid, payload, tyargs, valueConstructorInfo }) = ( List.app goTy tyargs
                                                                                                ; Option.app (fn (ty, pat) => (goTy ty; goPat pat)) payload
                                                                                                )
            | goPat (T.TypedPat (span, pat, ty)) = ( goTy ty; goPat pat )
            | goPat (T.LayeredPat (span, vid, ty, pat)) = ( goTy ty; goPat pat )
            | goPat (T.VectorPat (span, pats, ellipsis, elemTy)) = ( goTy elemTy; Vector.app goPat pats )
          fun goExp (T.SConExp (span, scon, ty)) = goTy ty
            | goExp (T.VarExp (span, longvid, ids, tyargs)) = List.app (fn (ty, cts) => goTy ty) tyargs
            | goExp (T.RecordExp (span, fields)) = List.app (fn (label, exp) => goExp exp) fields
            | goExp (T.RecordExtExp { sourceSpan, fields, baseExp, baseTy })
              = ( List.app (fn (label, exp) => goExp exp) fields
                ; goExp baseExp
                ; goTy baseTy
                )
            | goExp (T.LetInExp (span, decs, exp)) = let val tynameset = goDecs decs
                                                         val { goExp, ... } = checkTyScope (ctx, tvset, tynameset)
                                                     in goExp exp
                                                     end
            | goExp (T.AppExp (span, exp1, exp2)) = ( goExp exp1; goExp exp2 )
            | goExp (T.TypedExp (span, exp, ty)) = ( goExp exp; goTy ty )
            | goExp (T.HandleExp (span, exp, matches)) = ( goExp exp; List.app (fn (pat, exp) => (goPat pat; goExp exp)) matches )
            | goExp (T.RaiseExp (span, ty, exp)) = ( goTy ty; goExp exp )
            | goExp (T.IfThenElseExp (span, exp1, exp2, exp3)) = ( goExp exp1; goExp exp2; goExp exp3 )
            | goExp (T.CaseExp (span, exp, ty, matches)) = ( goExp exp; goTy ty; List.app (fn (pat, exp) => (goPat pat; goExp exp)) matches )
            | goExp (T.FnExp (span, vid, ty, exp)) = ( goTy ty; goExp exp )
            | goExp (T.ProjectionExp { sourceSpan, label, recordTy, fieldTy }) = ( goTy recordTy; goTy fieldTy )
            | goExp (T.ListExp (span, xs, ty)) = ( Vector.app goExp xs ; goTy ty )
            | goExp (T.VectorExp (span, xs, ty)) = ( Vector.app goExp xs ; goTy ty )
            | goExp (T.PrimExp (span, primOp, tyargs, args)) = ( Vector.app goTy tyargs ; Vector.app goExp args )
          and goDec (T.ValDec (span, valbinds)) = ( List.app goValBind valbinds
                                                  ; tynameset
                                                  )
            | goDec (T.RecValDec (span, valbinds)) = ( List.app goValBind valbinds
                                                     ; tynameset
                                                     )
            | goDec (T.TypeDec (span, typbinds)) = let fun goTypBind (T.TypBind (span, tyvars, tycon, ty)) = let val { goTy, ... } = checkTyScope (ctx, T.TyVarSet.addList (tvset, tyvars), tynameset)
                                                                                                             in goTy ty
                                                                                                             end
                                                   in List.app goTypBind typbinds
                                                    ; tynameset
                                                   end
            | goDec (T.DatatypeDec (span, datbinds)) = let val tynameset = List.foldl (fn (T.DatBind (span, _, tyname, _, _), tynameset) => T.TyNameSet.add (tynameset, tyname)) tynameset datbinds
                                                           fun goDatBind (T.DatBind (span, tyvars, tyname, conbinds, _))
                                                               = let val { goTy, ... } = checkTyScope (ctx, T.TyVarSet.addList (tvset, tyvars), tynameset)
                                                                     fun goConBind (T.ConBind (span, vid, optTy, info)) = Option.app goTy optTy
                                                                 in List.app goConBind conbinds
                                                                 end
                                                       in List.app goDatBind datbinds
                                                        ; tynameset
                                                       end
            | goDec (T.ExceptionDec (span, exbinds)) = ( List.app (fn T.ExBind (span, vid, optTy) => Option.app goTy optTy
                                                                  | T.ExReplication (span, vid, longvid, optTy) => Option.app goTy optTy
                                                                  ) exbinds
                                                       ; tynameset
                                                       )
            | goDec (T.GroupDec (span, decs)) = goDecs decs
            | goDec (T.OverloadDec (span, class, tyname, map)) = if T.TyNameSet.member (tynameset, tyname) then
                                                                     ( Syntax.OverloadKeyMap.app goExp map
                                                                     ; tynameset
                                                                     )
                                                                 else
                                                                     emitError (ctx, [span], "type constructor scope violation: " ^ TypedSyntax.PrettyPrint.print_TyName tyname)
            | goDec (T.EqualityDec (span, typarams, tyname, exp)) = if T.TyNameSet.member (tynameset, tyname) then
                                                                        ( #goExp (checkTyScope (ctx, T.TyVarSet.addList (tvset, typarams), tynameset)) exp
                                                                        ; tynameset
                                                                        )
                                                                    else
                                                                        emitError (ctx, [span], "type constructor scope violation: " ^ TypedSyntax.PrettyPrint.print_TyName tyname)
          and goDecs decs = List.foldl (fn (dec, tynameset) => let val { goDec, ... } = checkTyScope (ctx, tvset, tynameset)
                                                               in goDec dec
                                                               end)
                                       tynameset decs
          and goValBind (T.TupleBind (span, binds, exp)) = ( List.app (fn (vid, ty) => goTy ty) binds
                                                           ; goExp exp
                                                           )
            | goValBind (T.PolyVarBind (span, vid, T.TypeScheme (typarams, ty), exp))
              = let val { goTy, goExp, ... } = checkTyScope (ctx, T.TyVarSet.addList (tvset, List.map #1 typarams), tynameset)
                in goTy ty
                 ; goExp exp
                end
          fun goStrExp (T.StructExp _) = tynameset
            | goStrExp (T.StrIdExp _) = tynameset
            | goStrExp (T.PackedStrExp { sourceSpan, strExp, payloadTypes, packageSig }) = ( goStrExp strExp ; List.foldl (fn ({ tyname, ... }, set) => T.TyNameSet.add (set, tyname)) tynameset (#bound packageSig) )
            | goStrExp (T.FunctorAppExp { sourceSpan, funId, argumentTypes, argumentStr, packageSig })
              = let val tynameset = goStrExp argumentStr
                    (* TODO: Check argumentTypes *)
                in List.foldl (fn ({ tyname, ... }, set) => T.TyNameSet.add (set, tyname)) tynameset (#bound packageSig)
                end
            | goStrExp (T.LetInStrExp (span, strdecs, strexp)) = let val tynameset = goStrDecs strdecs
                                                                     val { goStrExp, ... } = checkTyScope (ctx, tvset, tynameset)
                                                                 in goStrExp strexp
                                                                 end
          and goStrDec (T.CoreDec (_, dec)) = goDec dec
            | goStrDec (T.StrBindDec (_, strid, strexp, { s, bound })) = List.foldl (fn ({ tyname, ... }, set) => T.TyNameSet.add (set, tyname)) (goStrExp strexp) bound
            | goStrDec (T.GroupStrDec (_, decs)) = goStrDecs decs
          and goStrDecs decs = List.foldl (fn (dec, tynameset) => let val { goStrDec, ... } = checkTyScope (ctx, tvset, tynameset)
                                                                  in goStrDec dec
                                                                  end)
                                          tynameset decs
          fun goFunExp (tynames, strid, s, strexp) = let val tynameset' = List.foldl (fn ({ tyname, ...}, set) => T.TyNameSet.add (set, tyname)) tynameset tynames
                                                         val { goStrExp, ... } = checkTyScope (ctx, tvset, tynameset')
                                                     in goStrExp strexp
                                                      ; tynameset
                                                     end
          fun goTopDec (T.StrDec dec) = goStrDec dec
            | goTopDec (T.FunDec (funid, funexp)) = goFunExp funexp
          fun goTopDecs decs = List.foldl (fn (dec, tynameset) => let val { goTopDec, ... } = checkTyScope (ctx, tvset, tynameset)
                                                                  in goTopDec dec
                                                                  end)
                                          tynameset decs
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
fun checkTyScopeOfProgram (ctx, tynameset : T.TyNameSet.set, program : T.Program)
    = List.foldl (fn (topdec, tynameset) => let val { goTopDecs, ... } = checkTyScope (ctx, T.TyVarSet.empty, tynameset)
                                            in goTopDecs topdec
                                            end)
                 tynameset program

val emptySignature : TypedSyntax.Signature = { valMap = Syntax.VIdMap.empty
                                             , tyConMap = Syntax.TyConMap.empty
                                             , strMap = Syntax.StrIdMap.empty
                                             }

fun mergeSignature (s1 : T.Signature, s2 : T.Signature) : T.Signature
    = { valMap = Syntax.VIdMap.unionWith #2 (#valMap s1, #valMap s2)
      , tyConMap = Syntax.TyConMap.unionWith #2 (#tyConMap s1, #tyConMap s2)
      , strMap = Syntax.StrIdMap.unionWith #2 (#strMap s1, #strMap s2)
      }
fun mergeQSignature (s1 : T.QSignature, s2 : T.QSignature) : T.QSignature
    = { s = mergeSignature(#s s1, #s s2)
      , bound = T.TyNameMap.unionWith #2 (#bound s1, #bound s2)
      }

fun canonicalOrderForQSignature ({ s, bound } : T.QSignature) : T.TyName list
    = let val bound' = T.TyNameMap.mapi (fn (tyname, { arity, admitsEquality, longtycon }) => Option.getOpt (canonicalPathForTyName (s, tyname), longtycon)) bound
          fun insert ([], key, value) = [(key, value)]
            | insert (xs0 as ((k, v) :: xs), key, value) = case Syntax.LongTyCon.compare (k, key) of
                                                               LESS => (k, v) :: insert (xs, key, value)
                                                             | GREATER => (key, value) :: xs0
                                                             | EQUAL => (key, value) :: xs (* cannot happen *)
      in List.map #2 (T.TyNameMap.foldli (fn (tyname, longtycon, acc) => insert (acc, longtycon, tyname)) [] bound')
      end
and canonicalPathForTyName ({ valMap, tyConMap, strMap } : T.Signature, tyname : T.TyName) : Syntax.LongTyCon option
    = let val t = Syntax.TyConMap.filter (fn { typeFunction = T.TypeFunction (tyvars, ty), valEnv } =>
                                             case ty of
                                                 T.TyCon (span, tyargs, tyname') =>
                                                 T.eqTyName (tyname, tyname') andalso ListPair.allEq (fn (tv, T.TyVar (_, tv')) => T.eqUTyVar (tv, tv')
                                                                                                     | _ => false
                                                                                                     ) (tyvars, tyargs)
                                               | _ => false
                                         ) tyConMap
      in case Option.map #1 (Syntax.TyConMap.firsti t) of
             SOME tycon => SOME (Syntax.MkQualified ([], tycon))
           | NONE => let val t = Syntax.StrIdMap.mapPartiali (fn (strid, T.MkSignature s) => Option.map (fn Syntax.MkQualified (strids, tycon) => Syntax.MkQualified (strid :: strids, tycon)) (canonicalPathForTyName (s, tyname))) strMap
                     in Syntax.StrIdMap.first t
                     end
      end

fun addSignatureToEnv (env : SigEnv, s : T.Signature, tyNameMap : TyNameAttr T.TyNameMap.map) : SigEnv
    = { valMap = #valMap env (* not used *)
      , tyConMap = Syntax.TyConMap.unionWith #2 (#tyConMap env, #tyConMap s)
      , tyNameMap = T.TyNameMap.unionWith #2 (#tyNameMap env, tyNameMap) (* should not overlap *)
      , strMap = Syntax.StrIdMap.unionWith #2 (#strMap env, Syntax.StrIdMap.map (fn T.MkSignature s => (s, ())) (#strMap s))
      , sigMap = #sigMap env
      , funMap = #funMap env
      , boundTyVars = #boundTyVars env
      }

fun applySubstTyConInTy (ctx : Context, subst : T.TypeFunction T.TyNameMap.map) : T.Ty -> T.Ty
    = let fun goTy (ty as T.TyVar _) = ty
            | goTy (ty as T.AnonymousTyVar _) = ty
            | goTy (T.RecordType (span, fields)) = T.RecordType (span, Syntax.LabelMap.map goTy fields)
            | goTy (T.RecordExtType (span, fields, baseTy)) = T.RecordExtType (span, Syntax.LabelMap.map goTy fields, goTy baseTy)
            | goTy (T.TyCon (span, tyargs, tycon)) = (case T.TyNameMap.find (subst, tycon) of
                                                          NONE => T.TyCon (span, List.map goTy tyargs, tycon)
                                                        | SOME (T.TypeFunction (tyvars, ty)) =>
                                                          let val subst' = (ListPair.foldlEq (fn (tv, tyarg, m) => TypedSyntax.TyVarMap.insert (m, tv, goTy tyarg)) TypedSyntax.TyVarMap.empty (tyvars, tyargs))
                                                                           handle ListPair.UnequalLengths => emitError (ctx, [span], "invalid type constructor substitution")
                                                          in T.applySubstTy subst' ty
                                                          end
                                                     )
            | goTy (T.FnType (span, ty1, ty2)) = T.FnType (span, goTy ty1, goTy ty2)
      in goTy
      end
fun applySubstTyConInSig (ctx : Context, subst : T.TypeFunction T.TyNameMap.map) : T.Signature -> T.Signature
    = let val goTy = applySubstTyConInTy (ctx, subst)
          fun goTypeScheme (T.TypeScheme (tvs, ty)) = T.TypeScheme (tvs, goTy ty)
          fun goTypeFunction (T.TypeFunction (tvs, ty)) = T.TypeFunction (tvs, goTy ty)
          fun goSig { valMap, tyConMap, strMap } = { valMap = Syntax.VIdMap.map (fn (tysc, ids) => (goTypeScheme tysc, ids)) valMap
                                                   , tyConMap = Syntax.TyConMap.map (fn { typeFunction, valEnv } =>
                                                                                        { typeFunction = goTypeFunction typeFunction
                                                                                        , valEnv = Syntax.VIdMap.map (fn (tysc, ids) => (goTypeScheme tysc, ids)) valEnv
                                                                                        }
                                                                                    ) tyConMap
                                                   , strMap = Syntax.StrIdMap.map (fn T.MkSignature s => T.MkSignature (goSig s)) strMap
                                                   }
      in goSig
      end

fun refreshTyNameInTy (ctx : Context, subst : T.TyName T.TyNameMap.map) : T.Ty -> T.Ty
    = let fun goTy (ty as T.TyVar _) = ty
            | goTy (ty as T.AnonymousTyVar _) = ty
            | goTy (T.RecordType (span, fields)) = T.RecordType (span, Syntax.LabelMap.map goTy fields)
            | goTy (T.RecordExtType (span, fields, baseTy)) = T.RecordExtType (span, Syntax.LabelMap.map goTy fields, goTy baseTy)
            | goTy (T.TyCon (span, tyargs, tycon)) = let val tyargs = List.map goTy tyargs
                                                    in case T.TyNameMap.find (subst, tycon) of
                                                           NONE => T.TyCon (span, tyargs, tycon)
                                                         | SOME tycon' => T.TyCon (span, tyargs, tycon')
                                                    end
            | goTy (T.FnType (span, ty1, ty2)) = T.FnType (span, goTy ty1, goTy ty2)
      in goTy
      end
fun refreshTyNameInSig (ctx : Context, subst : T.TyName T.TyNameMap.map) : T.Signature -> T.Signature
    = let val goTy = refreshTyNameInTy (ctx, subst)
          fun goTypeScheme (T.TypeScheme (tvs, ty)) = T.TypeScheme (tvs, goTy ty)
          fun goTypeFunction (T.TypeFunction (tvs, ty)) = T.TypeFunction (tvs, goTy ty)
          fun goSig { valMap, tyConMap, strMap } = { valMap = Syntax.VIdMap.map (fn (tysc, ids) => (goTypeScheme tysc, ids)) valMap
                                                   , tyConMap = Syntax.TyConMap.map (fn { typeFunction, valEnv } =>
                                                                                        { typeFunction = goTypeFunction typeFunction
                                                                                        , valEnv = Syntax.VIdMap.map (fn (tysc, ids) => (goTypeScheme tysc, ids)) valEnv
                                                                                        }
                                                                                    ) tyConMap
                                                   , strMap = Syntax.StrIdMap.map (fn T.MkSignature s => T.MkSignature (goSig s)) strMap
                                                   }
      in goSig
      end

fun checkEquality (ctx : Context, env : ('val,'str) Env', tyvars : T.TyVarSet.set) : T.Ty -> bool
    = let fun goTy (T.TyVar (span, tv)) = if T.TyVarSet.member (tyvars, tv) then
                                              true
                                          else
                                              T.tyVarAdmitsEquality tv
            | goTy (T.AnonymousTyVar _) = false (* should be an error *)
            | goTy (T.RecordType (span, fields)) = Syntax.LabelMap.all goTy fields
            | goTy (T.RecordExtType (span, fields, baseTy)) = Syntax.LabelMap.all goTy fields andalso goTy baseTy
            | goTy (T.TyCon (span, tyargs, tyname)) = isRefOrArray tyname
                                                      orelse (let val { admitsEquality, ... } = lookupTyNameInEnv (ctx, env, span, tyname)
                                                              in admitsEquality andalso List.all goTy tyargs
                                                              end
                                                             )
            | goTy (T.FnType _) = false
      in goTy
      end

fun lookupLongTyConInQSignature (ctx, span, s : T.QSignature, longtycon) : T.TypeStructure
    = let val S.MkQualified(strids, tycon as Syntax.MkTyCon name) = longtycon
          val { tyConMap, ... } = lookupStr (ctx, #s s, span, strids)
      in case Syntax.TyConMap.find(tyConMap, tycon) of
             SOME tystr => tystr
           | NONE => emitError (ctx, [span], "unknown type constructor '" ^ name ^ "'")
      end
fun getTypeNameFromTypeStructure (ctx, { typeFunction = T.TypeFunction (tyvars, T.TyCon (_, tyargs, tyname)), ... } : T.TypeStructure) : (T.TyName * int) option
    = let val arity = List.length tyvars
      in if List.length tyargs = arity then
             if ListPair.allEq (fn (tv, T.TyVar (_, tv')) => tv = tv' | _ => false) (tyvars, tyargs) then
                 SOME (tyname, arity)
             else
                 NONE
         else
             NONE
      end
  | getTypeNameFromTypeStructure _ = NONE

fun evalSignature (ctx : Context, env : SigEnv, S.BasicSigExp (span, specs)) : T.QSignature
    = evalSpecs(ctx, env, specs)
  | evalSignature(ctx, env, S.SigIdExp(span, sigid as Syntax.MkSigId name))
    = (case Syntax.SigIdMap.find(#sigMap env, sigid) of
           SOME { s, bound } => let val subst = T.TyNameMap.mapi (fn (tycon, _) => renewTyName (ctx, tycon)) bound
                                in { s = refreshTyNameInSig (ctx, subst) s
                                   , bound = T.TyNameMap.foldli (fn (tycon, attr, map) => T.TyNameMap.insert (map, T.TyNameMap.lookup (subst, tycon), attr)) T.TyNameMap.empty bound
                                   }
                                end
         | NONE => emitError (ctx, [span], "unknown signature name '" ^ name ^ "'")
      )
  | evalSignature(ctx, env, S.TypeRealisationExp(span, sigexp, tyvars, longtycon, ty))
    = let val s = evalSignature(ctx, env, sigexp)
          val tyvars = List.map (fn tv => (tv, genTyVar (ctx, tv))) tyvars
          val ty = let val env = { valMap = #valMap env
                                 , tyConMap = #tyConMap env
                                 , tyNameMap = #tyNameMap env
                                 , strMap = #strMap env
                                 , sigMap = #sigMap env
                                 , funMap = #funMap env
                                 , boundTyVars = List.foldl Syntax.TyVarMap.insert' (#boundTyVars env) tyvars
                                 }
                   in evalTy(ctx, env, ty)
                   end
          val tystr = lookupLongTyConInQSignature(ctx, span, s, longtycon)
      in case getTypeNameFromTypeStructure(ctx, tystr) of
             SOME (tyname, arity) =>
             if List.length tyvars = arity then
                 case T.TyNameMap.find (#bound s, tyname) of
                     SOME { admitsEquality, ... } =>
                     let val () = if admitsEquality andalso not (checkEquality (ctx, env, List.foldl (fn ((_, tv), set) => T.TyVarSet.add (set, tv)) T.TyVarSet.empty tyvars) ty) then
                                      emitError(ctx, [span], "type realisation failed (equality)")
                                  else
                                      ()
                         val subst = T.TyNameMap.singleton (tyname, T.TypeFunction (List.map #2 tyvars, ty))
                     in { s = applySubstTyConInSig (ctx, subst) (#s s), bound = #1 (T.TyNameMap.remove (#bound s, tyname)) }
                     end
                   | NONE => emitError (ctx, [span], "type realisation against a rigid type")
             else
                 emitError (ctx, [span], "type realisation against a rigid type")
           | NONE => emitError (ctx, [span], "type realisation against a rigid type")
      end
and evalSpecs (ctx : Context, env : SigEnv, specs) : T.QSignature
    = List.foldl (fn (spec, s) => let val env' = addSignatureToEnv (env, #s s, T.TyNameMap.map (fn { arity, admitsEquality, longtycon } => { arity = arity, admitsEquality = admitsEquality, overloadClass = NONE }) (#bound s))
                                  in mergeQSignature(s, addSpec(ctx, env', spec))
                                  end) { s = emptySignature, bound = T.TyNameMap.empty } specs
and addSpec (ctx : Context, env : SigEnv, S.ValDesc (span, descs)) : T.QSignature
    = { s = { valMap = List.foldl (fn ((vid, ty), valMap) => let val tvs = PostParsing.freeTyVarsInTy(Syntax.TyVarSet.empty, ty)
                                                                 val tvs = Syntax.TyVarSet.foldr (fn (tv, m) => Syntax.TyVarMap.insert(m, tv, genTyVar(ctx, tv))) Syntax.TyVarMap.empty tvs
                                                                 val env' = { valMap = #valMap env
                                                                            , tyConMap = #tyConMap env
                                                                            , tyNameMap = #tyNameMap env
                                                                            , strMap = #strMap env
                                                                            , sigMap = #sigMap env
                                                                            , funMap = #funMap env
                                                                            , boundTyVars = tvs
                                                                            }
                                                                 val ty = evalTy(ctx, env', ty)
                                                             in Syntax.VIdMap.insert (valMap, vid, (T.TypeScheme (Syntax.TyVarMap.foldr (fn (tv, xs) => (tv, []) :: xs) [] tvs, ty), Syntax.ValueVariable))
                                                             end) Syntax.VIdMap.empty descs
            , tyConMap = Syntax.TyConMap.empty
            , strMap = Syntax.StrIdMap.empty
            }
      , bound = TypedSyntax.TyNameMap.empty
      }
  | addSpec(ctx, env, S.TypeDesc(span, descs))
    = List.foldl (fn ((tyvars, tycon), s) => let val tyname = newTyName(ctx, tycon)
                                                 val tyvars = List.map (fn tv => genTyVar(ctx, tv)) tyvars
                                                 val tystr = { typeFunction = T.TypeFunction (tyvars, T.TyCon (span, List.map (fn tv => T.TyVar (span, tv)) tyvars, tyname))
                                                             , valEnv = Syntax.VIdMap.empty
                                                             }
                                             in { s = { valMap = #valMap (#s s)
                                                      , tyConMap = Syntax.TyConMap.insert(#tyConMap (#s s), tycon, tystr)
                                                      , strMap = #strMap (#s s)
                                                      }
                                                , bound = TypedSyntax.TyNameMap.insert (#bound s, tyname, { arity = List.length tyvars
                                                                                                          , admitsEquality = false
                                                                                                          , longtycon = Syntax.MkQualified ([], tycon)
                                                                                                          }
                                                                                       )
                                                }
                                             end
                 )
                 { s = { valMap = Syntax.VIdMap.empty
                       , tyConMap = Syntax.TyConMap.empty
                       , strMap = Syntax.StrIdMap.empty
                       }
                 , bound = TypedSyntax.TyNameMap.empty
                 } descs
  | addSpec(ctx, env, S.EqtypeDesc(span, descs))
    = List.foldl (fn ((tyvars, tycon), s) => let val tyname = newTyName(ctx, tycon)
                                                 val tyvars = List.map (fn tv => genTyVar(ctx, tv)) tyvars
                                                 val tystr = { typeFunction = T.TypeFunction (tyvars, T.TyCon (span, List.map (fn tv => T.TyVar (span, tv)) tyvars, tyname))
                                                             , valEnv = Syntax.VIdMap.empty
                                                             }
                                             in { s = { valMap = #valMap (#s s)
                                                      , tyConMap = Syntax.TyConMap.insert(#tyConMap (#s s), tycon, tystr)
                                                      , strMap = #strMap (#s s)
                                                      }
                                                , bound = TypedSyntax.TyNameMap.insert (#bound s, tyname, { arity = List.length tyvars
                                                                                                          , admitsEquality = true
                                                                                                          , longtycon = Syntax.MkQualified ([], tycon)
                                                                                                          }
                                                                                       )
                                                }
                                             end
                 )
                 { s = { valMap = Syntax.VIdMap.empty
                       , tyConMap = Syntax.TyConMap.empty
                       , strMap = Syntax.StrIdMap.empty
                       }
                 , bound = TypedSyntax.TyNameMap.empty
                 } descs
  | addSpec(ctx, env, S.DatDesc(span, descs : (S.TyVar list * S.TyCon * S.ConBind list) list, typbinds))
    = let val descs = let val goConBind = doWithtype(ctx, env, typbinds)
                      in List.map (fn (tyvars, tycon, conbinds) => (tyvars, tycon, List.map goConBind conbinds)) descs
                      end
          val localTyConMap = List.foldl (fn ((tyvars, tycon, conbinds), map) => S.TyConMap.insert(map, tycon, (tyvars, List.mapPartial (fn S.ConBind(_, _, optTy) => optTy) conbinds))) S.TyConMap.empty descs
          val equalityMap : bool S.TyConMap.map = determineDatatypeEquality(ctx, env, localTyConMap)
          val (partialTyConMap, tyNameMap, descs) = List.foldl (fn ((tyvars, tycon, condescs), (tyConMap, tyNameMap, descs)) =>
                                                                   let val tyname = newTyName(ctx, tycon)
                                                                       val tyvarPairs = List.map (fn tv => (tv, genTyVar(ctx, tv))) tyvars
                                                                       val tystr = { typeFunction = T.TypeFunction (List.map #2 tyvarPairs, T.TyCon (span, List.map (fn (_, tv) => T.TyVar (span, tv)) tyvarPairs, tyname))
                                                                                   , valEnv = Syntax.VIdMap.empty (* filled later *)
                                                                                   }
                                                                       val tyConMap = Syntax.TyConMap.insert(tyConMap, tycon, tystr)
                                                                       val tyNameMap = TypedSyntax.TyNameMap.insert (tyNameMap, tyname, { arity = List.length tyvars
                                                                                                                                        , admitsEquality = S.TyConMap.lookup (equalityMap, tycon)
                                                                                                                                        , overloadClass = NONE
                                                                                                                                        }
                                                                                                                    )
                                                                   in (tyConMap, tyNameMap, (tycon, tyname, tyvarPairs, tystr, condescs) :: descs)
                                                                   end
                                                               ) (Syntax.TyConMap.empty, TypedSyntax.TyNameMap.empty, []) descs
          val env' = mergeEnv(env, envWithTyConEnv(partialTyConMap, tyNameMap))
          val withtypeMap = List.foldl (fn (S.TypBind (span, tyvars, tycon, ty), tyConMap) =>
                                           let val tyvars = List.map (fn tv => (tv, genTyVar(ctx, tv))) tyvars
                                               val ty = let val env = { valMap = #valMap env'
                                                                      , tyConMap = #tyConMap env'
                                                                      , tyNameMap = #tyNameMap env'
                                                                      , strMap = #strMap env'
                                                                      , sigMap = #sigMap env'
                                                                      , funMap = #funMap env'
                                                                      , boundTyVars = List.foldl Syntax.TyVarMap.insert' (#boundTyVars env) tyvars
                                                                      }
                                                        in evalTy(ctx, env, ty)
                                                        end
                                               val tystr = { typeFunction = T.TypeFunction (List.map #2 tyvars, ty)
                                                           , valEnv = Syntax.VIdMap.empty
                                                           }
                                           in Syntax.TyConMap.insert(tyConMap, tycon, tystr)
                                           end
                                       ) Syntax.TyConMap.empty typbinds
      in List.foldl (fn ((tycon, tyname, tyvarPairs, tystr, condescs), s) =>
                        let val { typeFunction as T.TypeFunction (tyvars, ty), ... } = tystr
                            val env'' = { valMap = #valMap env'
                                        , tyConMap = #tyConMap env'
                                        , tyNameMap = #tyNameMap env'
                                        , strMap = #strMap env'
                                        , sigMap = #sigMap env'
                                        , funMap = #funMap env'
                                        , boundTyVars = List.foldl Syntax.TyVarMap.insert' (#boundTyVars env') tyvarPairs
                                        }
                            val allConstructors = List.foldl (fn (Syntax.ConBind (span, vid, _), set) => Syntax.VIdSet.add (set, vid)) Syntax.VIdSet.empty condescs
                            val valEnv = List.foldl (fn (S.ConBind(span, vid, optTy), valEnv) =>
                                                        let val tysc = T.TypeScheme (List.map (fn tv => (tv, [])) tyvars, case optTy of
                                                                                                                              NONE => ty
                                                                                                                            | SOME payloadTy => T.FnType (span, evalTy (ctx, env'', payloadTy), ty)
                                                                                    )
                                                            val idstatus = Syntax.ValueConstructor { tag = Syntax.getVIdName vid
                                                                                                   , allConstructors = allConstructors
                                                                                                   , representation = Syntax.REP_BOXED
                                                                                                   }
                                                        in Syntax.VIdMap.insert(valEnv, vid, (tysc, idstatus))
                                                        end
                                                    ) Syntax.VIdMap.empty condescs
                            val tystr = { typeFunction = typeFunction
                                        , valEnv = valEnv
                                        }
                        in { s = { valMap = Syntax.VIdMap.unionWith #2 (#valMap (#s s), valEnv)
                                 , tyConMap = Syntax.TyConMap.insert(#tyConMap (#s s), tycon, tystr)
                                 , strMap = #strMap (#s s)
                                 }
                           , bound = TypedSyntax.TyNameMap.insert (#bound s, tyname, { arity = List.length tyvars
                                                                                     , admitsEquality = S.TyConMap.lookup (equalityMap, tycon)
                                                                                     , longtycon = Syntax.MkQualified ([], tycon)
                                                                                     }
                                                                  )
                           }
                        end
                    )
                    { s = { valMap = Syntax.VIdMap.empty
                          , tyConMap = withtypeMap
                          , strMap = Syntax.StrIdMap.empty
                          }
                    , bound = TypedSyntax.TyNameMap.empty
                    } descs
      end
  | addSpec(ctx, env, S.DatatypeRepSpec(span, tycon, longtycon))
    = let val tystr = lookupTyConInEnv(ctx, env, span, longtycon)
      in { s = { valMap = #valEnv tystr
               , tyConMap = Syntax.TyConMap.singleton(tycon, tystr)
               , strMap = Syntax.StrIdMap.empty
               }
         , bound = TypedSyntax.TyNameMap.empty
         }
      end
  | addSpec(ctx, env, S.ExDesc(span, descs : (S.VId * S.Ty option) list))
    = { s = { valMap = List.foldl (fn ((vid, optTy), valMap) => let val ty = case optTy of
                                                                                 NONE => primTy_exn
                                                                               | SOME ty => T.FnType (span, evalTy (ctx, env, ty), primTy_exn)
                                                                in Syntax.VIdMap.insert (valMap, vid, (T.TypeScheme ([], ty), Syntax.ExceptionConstructor))
                                                                end) Syntax.VIdMap.empty descs
            , tyConMap = Syntax.TyConMap.empty
            , strMap = Syntax.StrIdMap.empty
            }
      , bound = TypedSyntax.TyNameMap.empty
      }
  | addSpec(ctx, env, S.StrDesc(span, descs)) = let val strMap = List.foldl (fn ((strid, sigexp), m) => Syntax.StrIdMap.insert(m, strid, evalSignature(ctx, env, sigexp))) Syntax.StrIdMap.empty descs
                                                in { s = { valMap = Syntax.VIdMap.empty
                                                         , tyConMap = Syntax.TyConMap.empty
                                                         , strMap = Syntax.StrIdMap.map (fn { s, bound } => T.MkSignature s) strMap
                                                         }
                                                   , bound = Syntax.StrIdMap.foldli (fn (strid, { bound, ... }, map) =>
                                                                                        TypedSyntax.TyNameMap.unionWith #2 (map, TypedSyntax.TyNameMap.map (fn { arity, admitsEquality, longtycon = Syntax.MkQualified (strids, tycon) } =>
                                                                                                                                                               { arity = arity
                                                                                                                                                               , admitsEquality = admitsEquality
                                                                                                                                                               , longtycon = Syntax.MkQualified (strid :: strids, tycon)
                                                                                                                                                               }
                                                                                                                                                           ) bound)) TypedSyntax.TyNameMap.empty strMap
                                                   }
                                                end
  | addSpec(ctx, env, S.Include(span, sigexp)) = evalSignature(ctx, env, sigexp)
  | addSpec(ctx, env, S.Sharing(span, specs, longtycon0 :: longtycons))
    = let val s = evalSpecs(ctx, env, specs)
      in shareLongTyCons(ctx, span, s, longtycon0, longtycons)
      end
  | addSpec(ctx, env, S.Sharing(span, specs, [])) = emitError(ctx, [span], "sharing: empty longtycons (internal error)")
  | addSpec(ctx, env, S.SharingStructure(span, specs, longstrids))
    = let val s = evalSpecs(ctx, env, specs)
          val strs = List.map (fn Syntax.MkQualified(strids, strid) =>
                                  let val strids = strids @ [strid]
                                  in (strids, collectLongTyCons(ctx, [], lookupStr(ctx, #s s, span, strids)))
                                  end
                              ) longstrids
          fun doStructure(s, (longstrid0, longtycons0) :: strs)
              = let val s = List.foldl (fn ((longstrid1, longtycons1), s) =>
                                           let val longtycons = Syntax.LongTyConSet.intersection (longtycons0, longtycons1)
                                           in Syntax.LongTyConSet.foldl (fn (Syntax.MkQualified(strids', tycon), s) =>
                                                                            shareLongTyCons(ctx, span, s, Syntax.MkQualified(longstrid0 @ strids', tycon), [Syntax.MkQualified(longstrid1 @ strids', tycon)])
                                                                        ) s longtycons
                                           end
                                       ) s strs
                in doStructure(s, strs)
                end
            | doStructure(s, []) = s
      in doStructure(s, strs)
      end
  | addSpec(ctx, env, S.TypeAliasDesc(span, descs))
    = { s = { valMap = Syntax.VIdMap.empty
            , tyConMap = List.foldl (fn ((tyvars, tycon, ty), tyConMap) =>
                                        let val tyvars = List.map (fn tv => (tv, genTyVar(ctx, tv))) tyvars
                                            val ty = let val env = { valMap = #valMap env
                                                                   , tyConMap = #tyConMap env (* not accumulated (Successor ML) *)
                                                                   , tyNameMap = #tyNameMap env
                                                                   , strMap = #strMap env
                                                                   , sigMap = #sigMap env
                                                                   , funMap = #funMap env
                                                                   , boundTyVars = List.foldl Syntax.TyVarMap.insert' (#boundTyVars env) tyvars
                                                                   }
                                                     in evalTy(ctx, env, ty)
                                                     end
                                            val tystr = { typeFunction = T.TypeFunction (List.map #2 tyvars, ty)
                                                        , valEnv = Syntax.VIdMap.empty
                                                        }
                                        in Syntax.TyConMap.insert(tyConMap, tycon, tystr)
                                        end
                                    ) Syntax.TyConMap.empty descs
            , strMap = Syntax.StrIdMap.empty
            }
      , bound = TypedSyntax.TyNameMap.empty
      }
and shareLongTyCons (ctx, span, s : T.QSignature, longtycon0, longtycons) : T.QSignature
    = let val tystr0 = lookupLongTyConInQSignature(ctx, span, s, longtycon0)
          val tystrs = List.map (fn longtycon => lookupLongTyConInQSignature(ctx, span, s, longtycon)) longtycons
      in case getTypeNameFromTypeStructure(ctx, tystr0) of
             SOME (tyname0, arity0) =>
             (case T.TyNameMap.find (#bound s, tyname0) of
                  SOME { arity, admitsEquality, ... } =>
                  if arity0 = arity then
                      let val typeFn = let val tyvars = List.tabulate(arity, fn _ => genTyVar(ctx, Syntax.MkTyVar "a"))
                                       in T.TypeFunction (tyvars, T.TyCon (span, List.map (fn tv => T.TyVar (span, tv)) tyvars, tyname0))
                                       end
                          val (subst, admitsEquality) = List.foldl (fn (tystr, (subst, admitsEquality)) =>
                                                                       case getTypeNameFromTypeStructure(ctx, tystr) of
                                                                           SOME (tyname, arity') =>
                                                                           if arity' <> arity then
                                                                               emitError(ctx, [span], "sharing: arity mismatch")
                                                                           else
                                                                               (case T.TyNameMap.find (#bound s, tyname) of
                                                                                    SOME { arity = arity', admitsEquality = admitsEquality', ... } =>
                                                                                    if arity' <> arity then
                                                                                        emitError(ctx, [span], "sharing: arity mismatch")
                                                                                    else if T.eqTyName (tyname, tyname0) then
                                                                                        (subst, admitsEquality) (* do nothing *)
                                                                                    else
                                                                                        (T.TyNameMap.insert (subst, tyname, typeFn), admitsEquality orelse admitsEquality')
                                                                                  | NONE => emitError(ctx, [span], "sharing: type alias is invalid")
                                                                               )
                                                                         | NONE => emitError(ctx, [span], "sharing: type alias is invalid")
                                                                   ) (T.TyNameMap.empty, admitsEquality) tystrs
                      in { s = applySubstTyConInSig(ctx, subst) (#s s)
                         , bound = T.TyNameMap.mapPartiali (fn (tyname, x as { arity, admitsEquality = _, longtycon }) =>
                                                               if T.TyNameMap.inDomain (subst, tyname) then
                                                                   NONE
                                                               else if T.eqTyName (tyname, tyname0) then
                                                                   SOME { arity = arity, admitsEquality = admitsEquality, longtycon = longtycon }
                                                               else
                                                                   SOME x
                                                           ) (#bound s)
                         }
                      end
                  else
                      emitError(ctx, [span], "sharing: invalid arity")
                | NONE => emitError(ctx, [span], "sharing: type alias is invalid")
             )
           | NONE => emitError(ctx, [span], "sharing: type alias is invalid")
      end
and collectLongTyCons (ctx, strids : Syntax.StrId list, { valMap = _, tyConMap, strMap } : T.Signature) : Syntax.LongTyConSet.set
    = let val set = Syntax.TyConMap.foldli (fn (tycon, _, set) => Syntax.LongTyConSet.add(set, Syntax.MkQualified(strids, tycon))) Syntax.LongTyConSet.empty tyConMap
      in Syntax.StrIdMap.foldli (fn (strid, T.MkSignature s, set) =>
                                    let val set' = collectLongTyCons(ctx, strids @ [strid], s)
                                    in Syntax.LongTyConSet.union (set, set')
                                    end
                                ) set strMap
      end

fun sameType (T.TyVar (span1, tv), T.TyVar (span2, tv')) = tv = tv'
  | sameType (T.RecordType (span1, fields), T.RecordType (span2, fields')) = Syntax.LabelMap.numItems fields = Syntax.LabelMap.numItems fields'
                                                                             andalso Syntax.LabelMap.alli (fn (label, ty) => case Syntax.LabelMap.find (fields', label) of
                                                                                                                                 SOME ty' => sameType (ty, ty')
                                                                                                                               | NONE => false
                                                                                                          ) fields
  | sameType (T.TyCon (span1, tyargs, tycon), T.TyCon (span2, tyargs', tycon')) = T.eqTyName (tycon, tycon') andalso (ListPair.allEq sameType (tyargs, tyargs') handle ListPair.UnequalLengths => false)
  | sameType (T.FnType (span1, ty1, ty2), T.FnType (span2, ty1', ty2')) = sameType (ty1, ty1') andalso sameType (ty2, ty2')
  | sameType (_, _) = false

fun sameTypeScheme (ctx, span, T.TypeScheme (tyvarsE, tyE), T.TypeScheme (tyvarsA, tyA))
    = if List.length tyvarsE = List.length tyvarsA then
          let val tyvars = List.map (fn _ => genTyVar (ctx, Syntax.MkTyVar "?")) tyvarsE
              (* constraints are ignored *)
              val substE = ListPair.foldlEq (fn ((tv, _), tv', m) => T.TyVarMap.insert (m, tv, T.TyVar (span, tv'))) T.TyVarMap.empty (tyvarsE, tyvars)
              val substA = ListPair.foldlEq (fn ((tv, _), tv', m) => T.TyVarMap.insert (m, tv, T.TyVar (span, tv'))) T.TyVarMap.empty (tyvarsA, tyvars)
              val tyE = applySubstTy substE tyE
              val tyA = applySubstTy substA tyA
          in sameType(tyE, tyA)
          end
      else
          false

fun matchQSignature (ctx : Context, env : Env, span : SourcePos.span, expected : T.QSignature, strid : T.StrId, actual : T.Signature) : T.Signature * T.StrExp
    = let val env' = env (* addSignatureToEnv(envToSigEnv env, actual) *)
          val instantiation = TypedSyntax.TyNameMap.map (fn { arity, admitsEquality, longtycon as Syntax.MkQualified (strids, tycon) } =>
                                                            let val { typeFunction as T.TypeFunction (tyvars, actualTy), ... }
                                                                    = let val s = lookupStr (ctx, actual, span, strids)
                                                                      in case Syntax.TyConMap.find (#tyConMap s, tycon) of
                                                                             SOME tystr => tystr
                                                                           | NONE => emitError (ctx, [span], "signature matching: type not found: " ^ Syntax.print_LongTyCon longtycon)
                                                                      end
                                                                val () = if List.length tyvars = arity then
                                                                             () (* OK *)
                                                                         else
                                                                             emitError (ctx, [span], "signature matching: arity mismatch (" ^ Syntax.print_LongTyCon longtycon ^ ")")
                                                                val () = if admitsEquality andalso not (checkEquality (ctx, env', T.TyVarSet.fromList tyvars) actualTy) then
                                                                             emitError (ctx, [span], "signature matching: equality mismatch (" ^ Syntax.print_LongTyCon longtycon ^ ")")
                                                                         else
                                                                             ()
                                                            in typeFunction
                                                            end
                                                        ) (#bound expected)
          val instantiated = applySubstTyConInSig (ctx, instantiation) (#s expected)
      in matchSignature (ctx, env, span, instantiated, T.MkLongStrId (strid, []), actual)
      end
and matchSignature (ctx, env, span, expected : T.Signature, longstrid : T.LongStrId, actual : T.Signature) : T.Signature * T.StrExp
    = let val strMap : (T.Signature * T.StrExp) S.StrIdMap.map
              = Syntax.StrIdMap.mapi (fn (strid, T.MkSignature s) =>
                                         case Syntax.StrIdMap.find(#strMap actual, strid) of
                                             SOME (T.MkSignature s') => let val longstrid' = case longstrid of
                                                                                                 T.MkLongStrId (strid0, strids0) => T.MkLongStrId (strid0, strids0 @ [strid])
                                                                        in matchSignature(ctx, env, span, s, longstrid', s')
                                                                        end
                                           | NONE => emitError(ctx, [span], "signature matching: structure not found (" ^ Syntax.print_StrId strid ^ ")")
                                     ) (#strMap expected)
          val tyConMap : T.TypeStructure S.TyConMap.map
              = Syntax.TyConMap.mapi (fn (tycon, expectedTyStr) =>
                                         case Syntax.TyConMap.find (#tyConMap actual, tycon) of
                                             SOME actualTyStr => matchTyDesc(ctx, env, span, expectedTyStr, actualTyStr)
                                           | NONE => emitError(ctx, [span], "signature matching: " ^ (case tycon of Syntax.MkTyCon name => name) ^ " not found")
                                     ) (#tyConMap expected)
          val valMap : (T.TypeScheme * T.Dec list * T.LongVId * Syntax.ValueConstructorInfo Syntax.IdStatus) S.VIdMap.map
              = Syntax.VIdMap.mapi (fn (vid, (tyscE, idsE)) =>
                                       case Syntax.VIdMap.find (#valMap actual, vid) of
                                           SOME (tyscA, idsA) => let val longvid = case longstrid of
                                                                                       T.MkLongStrId (strid0, strids0) => T.MkLongVId (strid0, strids0, vid)
                                                                     val (tysc, decs, longvid') = matchValDesc(ctx, env, span, tyscE, longvid, tyscA, idsA) handle TypeError (spans, msg) => raise TypeError (spans, msg ^ " during matching " ^ Syntax.print_VId vid)
                                                                     val () = if (case idsE of Syntax.ExceptionConstructor => true | _ => false) andalso (case idsA of Syntax.ExceptionConstructor => false | _ => true) then
                                                                                  emitError(ctx, [span], "signature matching: id status mismatch: " ^ Syntax.getVIdName vid)
                                                                              else if Syntax.isValueConstructor idsE andalso not (Syntax.isValueConstructor idsA) then
                                                                                  emitError(ctx, [span], "signature matching: id status mismatch: " ^ Syntax.getVIdName vid)
                                                                              else
                                                                                  ()
                                                                 in (tysc, decs, longvid', idsE)
                                                                 end
                                         | NONE => emitError(ctx, [span], "signature matching: " ^ Syntax.getVIdName vid ^ " not found")
                                   ) (#valMap expected)
          val (decs, strMap) = Syntax.StrIdMap.foldli (fn (strid, (s, strexp), (decs, strMap)) =>
                                                           let val strid' = newStrId(ctx, strid)
                                                               val decs = T.StrBindDec (span, strid', strexp, { s = s, bound = [] }) :: decs
                                                           in (decs, Syntax.StrIdMap.insert (strMap, strid, T.MkLongStrId (strid', [])))
                                                           end
                                                       ) ([], Syntax.StrIdMap.empty) strMap
          val (decs, valMap) = Syntax.VIdMap.foldli (fn (vid, (tysc, decs, longvid, ids), (decs', valMap)) =>
                                                        let val decs = List.foldr (fn (dec, decs) => T.CoreDec (span, dec) :: decs) decs' decs
                                                        in (decs, Syntax.VIdMap.insert(valMap, vid, (longvid, ids)))
                                                        end
                                                    ) (decs, Syntax.VIdMap.empty) valMap
          val strexp = T.StructExp { sourceSpan = span
                                   , valMap = valMap
                                   , tyConMap = tyConMap
                                   , strMap = strMap
                                   }
      in (expected, if List.null decs then
                        strexp
                    else
                        T.LetInStrExp (span, decs, strexp)
         )
      end
and matchTyDesc (ctx, env, span, expected : T.TypeStructure, actual : T.TypeStructure) : T.TypeStructure
    = let val { typeFunction = T.TypeFunction (tyvarsE, tyE), valEnv = valEnvE } = expected
          val numE = Syntax.VIdMap.numItems valEnvE
          val { typeFunction = T.TypeFunction (tyvarsA, tyA), valEnv = valEnvA } = actual
      in if List.length tyvarsE = List.length tyvarsA then
             let val tyvars = List.map (fn _ => genTyVar (ctx, Syntax.MkTyVar "?")) tyvarsE
                 val substE = ListPair.foldlEq (fn (tv, tv', m) => T.TyVarMap.insert (m, tv, T.TyVar (span, tv'))) T.TyVarMap.empty (tyvarsE, tyvars)
                 val substA = ListPair.foldlEq (fn (tv, tv', m) => T.TyVarMap.insert (m, tv, T.TyVar (span, tv'))) T.TyVarMap.empty (tyvarsA, tyvars)
                 val tyE = applySubstTy substE tyE
                 val tyA = applySubstTy substA tyA
                 fun checkConstructor (vid, (tyscE, _)) = case Syntax.VIdMap.find(valEnvA, vid) of
                                                              SOME (tyscA, _) => sameTypeScheme(ctx, span, tyscE, tyscA)
                                                            | NONE => emitError(ctx, [span], "signature matching: value constructor mismatch")
             in if sameType(tyE, tyA) then
                    if numE > 0 then
                        if Syntax.VIdMap.numItems valEnvA = numE andalso Syntax.VIdMap.alli checkConstructor valEnvE then
                            actual
                        else
                            emitError(ctx, [span], "signature matching: value constructor mismatch")
                    else
                        actual
                else
                    emitError(ctx, [span], "signature matching: type mismatch")
             end
         else
             emitError(ctx, [span], "signature matching: arity mismatch")
      end
and matchValDesc (ctx, env, span, expected : T.TypeScheme, longvid : T.LongVId, actual : T.TypeScheme, ids : Syntax.ValueConstructorInfo Syntax.IdStatus) : T.TypeScheme * T.Dec list * T.LongVId
    = let val T.TypeScheme (tyvarsE, tyE) = expected
          val ictx = { context = ctx
                     , level = 0
                     }
          val (tyA, tyargsA) = instantiate (ictx, span, actual)
          val () = addConstraint (ictx, env, T.EqConstr (span, tyE, tyA))
          val vid = newVId (ctx, case longvid of
                                     T.MkShortVId (T.MkVId (name, _)) => Syntax.MkVId name
                                   | T.MkLongVId (_, _, vid) => vid
                           )
          val tyargsA = List.map (fn (ty, c) => (T.forceTy ty, c)) tyargsA
          val trivial = ListPair.allEq (fn ((tvE, []), (T.TyVar (span2, tvA), [])) => T.eqUTyVar (tvE, tvA)
                                       | ((tvE, [T.IsEqType]), (T.TyVar (span2, tvA), [T.IsEqType])) => T.eqUTyVar (tvE, tvA)
                                       | _ => false
                                       ) (tyvarsE, tyargsA)
      in if trivial then
             (expected, [], longvid) (* includes the case where ids = ExceptionConstructor *)
         else
             let val dec = T.ValDec (span, [T.PolyVarBind (span, vid, expected, T.VarExp (span, longvid, ids, tyargsA))])
             in (expected, [dec], T.MkShortVId vid)
             end
      end

fun typeCheckStrExp (ctx : Context, env : Env, S.StructExp (span, decs)) : T.PackedSignature * TyNameAttr T.TyNameMap.map * T.StrDec list * T.StrExp
    = let val ({ valMap, tyConMap, tyNameMap, strMap, ... }, decs) = typeCheckStrDecs(ctx, env, decs)
          val s = { s = { valMap = Syntax.VIdMap.map (fn (tysc, ids, _) => (tysc, ids)) valMap
                        , tyConMap = tyConMap
                        , strMap = Syntax.StrIdMap.map (fn (s, _) => T.MkSignature s) strMap
                        }
                  , bound = []
                  }
          val e = T.StructExp { sourceSpan = span
                              , valMap = Syntax.VIdMap.map (fn (_, ids, longvid) => (longvid, ids)) valMap
                              , tyConMap = tyConMap
                              , strMap = Syntax.StrIdMap.map (fn (_, longstrid) => longstrid) strMap
                              }
      in (s, tyNameMap, decs, e)
      end
  | typeCheckStrExp(ctx, env, S.StrIdExp(span, longstrid))
    = (case longstrid of
           Syntax.MkQualified([], strid) => (case Syntax.StrIdMap.find(#strMap env, strid) of
                                                 SOME (s, longstrid) => ({ s = s, bound = [] }, TypedSyntax.TyNameMap.empty, [], T.StrIdExp (span, longstrid))
                                               | NONE => emitError (ctx, [span], "structure not found")
                                            )
         | Syntax.MkQualified(strid0 :: strids, strid') =>
           (case Syntax.StrIdMap.find(#strMap env, strid0) of
                SOME (s, T.MkLongStrId (strid0, strids0)) =>
                let val s' = lookupStr (ctx, s, span, strids @ [strid'])
                in ({ s = s', bound = [] }, TypedSyntax.TyNameMap.empty, [], T.StrIdExp (span, T.MkLongStrId (strid0, strids0 @ strids @ [strid'])))
                end
              | NONE => emitError (ctx, [span], "structure not found")
           )
      )
  | typeCheckStrExp(ctx, env, S.TransparentConstraintExp(span, strexp, sigexp))
    = let val (sA, tyNameMap, decs, strexp) = typeCheckStrExp(ctx, env, strexp)
          val sE = evalSignature(ctx, envToSigEnv env, sigexp)
          val strid = newStrId(ctx, Syntax.MkStrId "tmp")
          val env' = { valMap = #valMap env
                     , tyConMap = #tyConMap env
                     , tyNameMap = TypedSyntax.TyNameMap.unionWith #2 (#tyNameMap env, tyNameMap)
                     , strMap = #strMap env
                     , sigMap = #sigMap env
                     , funMap = #funMap env
                     , boundTyVars = #boundTyVars env
                     }
          val (s, strexp') = matchQSignature(ctx, env', span, sE, strid, #s sA)
      in ({ s = s, bound = [] }, tyNameMap, decs @ [T.StrBindDec (span, strid, strexp, sA)], strexp')
      end
  | typeCheckStrExp(ctx, env, S.OpaqueConstraintExp(span, strexp, sigexp))
    = let val (sA, tyNameMap, decs, strexp) = typeCheckStrExp(ctx, env, strexp)
          val sE = evalSignature(ctx, envToSigEnv env, sigexp)
          val env' = { valMap = #valMap env
                     , tyConMap = #tyConMap env
                     , tyNameMap = TypedSyntax.TyNameMap.unionWith #2 (#tyNameMap env, tyNameMap)
                     , strMap = #strMap env
                     , sigMap = #sigMap env
                     , funMap = #funMap env
                     , boundTyVars = #boundTyVars env
                     }
          val strid = newStrId(ctx, Syntax.MkStrId "tmp")
          val (s, strexp') = matchQSignature(ctx, env', span, sE, strid, #s sA)
          val tynames = canonicalOrderForQSignature sE
          val packageSig = { s = #s sE
                           , bound = List.map (fn tyname => let val { arity, admitsEquality, longtycon } = T.TyNameMap.lookup (#bound sE, tyname)
                                                            in { tyname = tyname, arity = arity, admitsEquality = admitsEquality }
                                                            end
                                              ) tynames
                           }
          val tyNameMapOutside = T.TyNameMap.foldli (fn (tyname, { arity, admitsEquality, longtycon }, acc) =>
                                                        T.TyNameMap.insert (acc, tyname, { arity = arity, admitsEquality = admitsEquality, overloadClass = NONE })
                                                    ) T.TyNameMap.empty (#bound sE)
          val payloadTypes = List.map (fn tyname => let val { longtycon = Syntax.MkQualified (strids, tycon), ... } = T.TyNameMap.lookup (#bound sE, tyname)
                                                        val { tyConMap, ... } = lookupStr (ctx, #s sA, span, strids)
                                                    in case Syntax.TyConMap.find (tyConMap, tycon) of
                                                           SOME { typeFunction, valEnv } => typeFunction
                                                         | NONE => emitError (ctx, [span], "unknown type constructor")
                                                    end) tynames
      in (packageSig, tyNameMapOutside, [], T.PackedStrExp { sourceSpan = span
                                                           , strExp = T.LetInStrExp (span, decs @ [T.StrBindDec (span, strid, strexp, sA)], strexp')
                                                           , payloadTypes = payloadTypes
                                                           , packageSig = packageSig
                                                           }
         )
      end
  | typeCheckStrExp(ctx, env, S.FunctorAppExp(span, funid, strexp))
    = (case S.FunIdMap.find (#funMap env, funid) of
           NONE => emitError (ctx, [span], "undefined functor")
         | SOME (funsig, funid) =>
           let val { bound, paramSig, resultSig } = funsig (* TODO: refresh bound tynames? *)
               val resultSig = let val subst = List.foldl (fn ({ tyname, ... }, map) => T.TyNameMap.insert (map, tyname, renewTyName (ctx, tyname))) T.TyNameMap.empty (#bound resultSig)
                               in { s = refreshTyNameInSig (ctx, subst) (#s resultSig)
                                  , bound = List.map (fn { tyname, arity, admitsEquality } => { tyname = T.TyNameMap.lookup (subst, tyname), arity = arity, admitsEquality = admitsEquality }) (#bound resultSig)
                                  }
                               end
               val (sA, tyNameMap, decs, strexp) = typeCheckStrExp (ctx, env, strexp)
               val strid = newStrId (ctx, Syntax.MkStrId "tmp")
               val env' = { valMap = #valMap env
                          , tyConMap = #tyConMap env
                          , tyNameMap = TypedSyntax.TyNameMap.unionWith #2 (#tyNameMap env, tyNameMap)
                          , strMap = #strMap env
                          , sigMap = #sigMap env
                          , funMap = #funMap env
                          , boundTyVars = #boundTyVars env
                          }
               val argumentTypes = List.map (fn { tyname, arity, admitsEquality, longtycon as Syntax.MkQualified (strids, tycon) } =>
                                                let val { typeFunction as T.TypeFunction (tyvars, actualTy), ... }
                                                        = let val s = lookupStr (ctx, #s sA, span, strids)
                                                          in case Syntax.TyConMap.find (#tyConMap s, tycon) of
                                                                 SOME tystr => tystr
                                                               | NONE => emitError (ctx, [span], "signature matching: type not found: " ^ Syntax.print_LongTyCon longtycon)
                                                          end
                                                    val () = if List.length tyvars = arity then
                                                                 () (* OK *)
                                                             else
                                                                 emitError (ctx, [span], "signature matching: arity mismatch (" ^ Syntax.print_LongTyCon longtycon ^ ")")
                                                    val () = if admitsEquality andalso not (checkEquality (ctx, env', T.TyVarSet.fromList tyvars) actualTy) then
                                                                 emitError (ctx, [span], "signature matching: equality mismatch (" ^ Syntax.print_LongTyCon longtycon ^ ")")
                                                             else
                                                                 () (* OK *)
                                                in (tyname, typeFunction, admitsEquality)
                                                end
                                            ) bound
               val instantiation = List.foldl (fn ((tyname, typeFunction, admitsEquality), map) => T.TyNameMap.insert (map, tyname, typeFunction)) T.TyNameMap.empty argumentTypes
               val instantiated = applySubstTyConInSig (ctx, instantiation) paramSig
               val (_, strexp') = matchSignature (ctx, env', span, instantiated, T.MkLongStrId (strid, []), #s sA)
               val resultSig = { s = applySubstTyConInSig (ctx, instantiation) (#s resultSig)
                               , bound = #bound resultSig
                               }
               val tyNameMapOutside = List.foldl (fn ({ tyname, arity, admitsEquality }, map) =>
                                                     T.TyNameMap.insert (map, tyname, { arity = arity, admitsEquality = admitsEquality, overloadClass = NONE })
                                                 ) tyNameMap (#bound resultSig)
           in ( resultSig
              , tyNameMapOutside
              , decs @ [T.StrBindDec (span, strid, strexp, sA)]
              , T.FunctorAppExp { sourceSpan = span
                                , funId = funid
                                , argumentTypes = List.map (fn (tyname, typeFunction, admitsEquality) => { typeFunction = typeFunction, admitsEquality = admitsEquality }) argumentTypes
                                , argumentStr = strexp'
                                , packageSig = resultSig
                                }
              )
           end
      )
  | typeCheckStrExp(ctx, env, S.LetInStrExp(span, strdecs, strexp)) = let val (env', strdecs) = typeCheckStrDecs(ctx, env, strdecs)
                                                                          val (s, tyNameMap, strdecs', strexp) = typeCheckStrExp(ctx, mergeEnv(env, env'), strexp)
                                                                      in (s, T.TyNameMap.unionWith #2 (#tyNameMap env', tyNameMap), strdecs @ strdecs', strexp)
                                                                      end
and typeCheckStrDec (ctx : Context, env : Env, S.CoreDec (span, dec)) : Env * TypedSyntax.StrDec list
    = let val (env', decs) = typeCheckCoreDecs(ctx, env, [dec])
      in (env', List.map (fn dec => T.CoreDec (span, dec)) decs)
      end
  | typeCheckStrDec(ctx, env, S.StrBindDec(span, binds))
    = let val (strMap, tyNameMap, binds) = List.foldr (fn ((strid, strexp), (strMap, tyNameMap, binds)) =>
                                                          let val (ps, tc, strdecs, strexp) = typeCheckStrExp(ctx, env, strexp)
                                                              val strid' = newStrId(ctx, strid)
                                                          in (S.StrIdMap.insert (strMap, strid, (#s ps, T.MkLongStrId (strid', []))), TypedSyntax.TyNameMap.unionWith #2 (tyNameMap, tc), (strid', strdecs, strexp, ps) :: binds)
                                                          end
                                                      ) (Syntax.StrIdMap.empty, TypedSyntax.TyNameMap.empty, []) binds
          val env' = { valMap = Syntax.VIdMap.empty
                     , tyConMap = Syntax.TyConMap.empty
                     , tyNameMap = tyNameMap
                     , strMap = strMap
                     , sigMap = Syntax.SigIdMap.empty
                     , funMap = Syntax.FunIdMap.empty
                     , boundTyVars = Syntax.TyVarMap.empty
                     }
      in (env', List.foldr (fn ((strid, strdecs, strexp, s), strdecs') =>
                               let val decs = case strdecs @ [T.StrBindDec (span, strid, strexp, s)] of
                                                  decs as [] => decs
                                                | decs as [_] => decs
                                                | decs => [T.GroupStrDec (span, decs)]
                               in decs @ strdecs'
                               end) [] binds)
      end
  | typeCheckStrDec(ctx, env, S.LocalStrDec(span, decs1, decs2))
    = let val (env', decs1) = typeCheckStrDecs(ctx, env, decs1)
          val (env'', decs2) = typeCheckStrDecs(ctx, mergeEnv(env, env'), decs2)
          val env'' = { valMap = #valMap env''
                      , tyConMap = #tyConMap env''
                      , tyNameMap = TypedSyntax.TyNameMap.unionWith #2 (#tyNameMap env', #tyNameMap env'')
                      , strMap = #strMap env''
                      , sigMap = #sigMap env''
                      , funMap = #funMap env''
                      , boundTyVars = #boundTyVars env''
                      }
      in (env'', case decs1 @ decs2 of
                     decs as [] => decs
                   | decs as [_] => decs
                   | decs => [T.GroupStrDec (span, decs)]
         )
      end
and typeCheckStrDecs(ctx : Context, env : Env, []) = (emptyEnv, [])
  | typeCheckStrDecs(ctx, env, dec :: decs) = let val (env', dec) = typeCheckStrDec(ctx, env, dec)
                                                  val (env'', decs) = typeCheckStrDecs(ctx, mergeEnv(env, env'), decs)
                                              in (mergeEnv(env', env''), dec @ decs)
                                              end

fun typeCheckFunExp' (ctx, span, paramEnv, paramSig, strid, strexp) : TypedSyntax.FunSig * TypedSyntax.FunExp
    = let val (actualSignature : T.PackedSignature, bodyTyNameMap, strdecs, strexp) = typeCheckStrExp (ctx, paramEnv, strexp)
          val tynamesInParam = canonicalOrderForQSignature paramSig
          val stridTmp = newStrId(ctx, Syntax.MkStrId "tmp")
          val additionalTyNames = T.TyNameMap.foldli (fn (tyname, { arity, admitsEquality, overloadClass = _ }, xs) =>
                                                         if List.exists (fn { tyname = tyname', ... } => T.eqTyName (tyname, tyname')) (#bound actualSignature) then
                                                             xs
                                                         else
                                                             { tyname = tyname, arity = arity, admitsEquality = admitsEquality } :: xs
                                                     ) [] bodyTyNameMap
          val resultSig = { s = #s actualSignature
                          , bound = #bound actualSignature @ additionalTyNames
                          }
          val payloadTypes = List.map (fn { tyname, arity, admitsEquality } =>
                                          let val tyvars = List.tabulate (arity, fn _ => genTyVar (ctx, Syntax.MkTyVar "?"))
                                          in T.TypeFunction (tyvars, T.TyCon (span, List.map (fn tv => T.TyVar (span, tv)) tyvars, tyname))
                                          end) (#bound resultSig)
          val funsig = { bound = List.map (fn tyname => let val { arity, admitsEquality, longtycon } = T.TyNameMap.lookup (#bound paramSig, tyname)
                                                        in { tyname = tyname, arity = arity, admitsEquality = admitsEquality, longtycon = longtycon }
                                                        end
                                          ) tynamesInParam
                       , paramSig = #s paramSig
                       , resultSig = resultSig
                       }
          val funexp = ( List.map (fn tyname => let val { arity, admitsEquality, longtycon } = T.TyNameMap.lookup (#bound paramSig, tyname)
                                                in { tyname = tyname, arity = arity, admitsEquality = admitsEquality }
                                                end
                                  ) tynamesInParam
                       , strid
                       , #s paramSig
                       , T.LetInStrExp ( span
                                       , strdecs @ [T.StrBindDec (span, stridTmp, strexp, actualSignature)]
                                       , T.PackedStrExp { sourceSpan = span
                                                        , strExp = T.StrIdExp (span, T.MkLongStrId (stridTmp, []))
                                                        , payloadTypes = payloadTypes
                                                        , packageSig = resultSig
                                                        }
                                       )
                       )
      in (funsig, funexp)
      end
fun typeCheckFunExp (ctx, span, env, S.NamedFunExp (strid, sigexp, strexp)) : TypedSyntax.FunSig * TypedSyntax.FunExp
    = let val strid' = newStrId(ctx, strid)
          val paramSig : T.QSignature = evalSignature (ctx, envToSigEnv env, sigexp)
          val tyNameMap : TyNameAttr T.TyNameMap.map
              = T.TyNameMap.mapi (fn (tyname, { arity, admitsEquality, longtycon }) =>
                                     { arity = arity
                                     , admitsEquality = admitsEquality
                                     , overloadClass = NONE
                                     }
                                ) (#bound paramSig)
          val paramEnv = { valMap = #valMap env
                         , tyConMap = #tyConMap env
                         , tyNameMap = T.TyNameMap.unionWith #2 (#tyNameMap env, tyNameMap)
                         , strMap = S.StrIdMap.insert (#strMap env, strid, (#s paramSig, T.MkLongStrId (strid', [])))
                         , sigMap = #sigMap env
                         , funMap = #funMap env
                         , boundTyVars = #boundTyVars env
                         }
      in typeCheckFunExp'(ctx, span, paramEnv, paramSig, strid', strexp)
      end
  | typeCheckFunExp(ctx, span, env, S.AnonymousFunExp (sigexp, strexp))
    = let val strid0 = newStrId(ctx, S.MkStrId "param")
          val paramSig : T.QSignature = evalSignature (ctx, envToSigEnv env, sigexp)
          val tyNameMap : TyNameAttr T.TyNameMap.map
              = T.TyNameMap.mapi (fn (tyname, { arity, admitsEquality, longtycon }) =>
                                     { arity = arity
                                     , admitsEquality = admitsEquality
                                     , overloadClass = NONE
                                     }
                                 ) (#bound paramSig)
          val paramEnv = { valMap = Syntax.VIdMap.mapi (fn (vid, (tysc, ids)) => (tysc, ids, TypedSyntax.MkLongVId (strid0, [], vid))) (#valMap (#s paramSig))
                         , tyConMap = #tyConMap (#s paramSig)
                         , tyNameMap = tyNameMap
                         , strMap = Syntax.StrIdMap.mapi (fn (strid, T.MkSignature s) => (s, T.MkLongStrId (strid0, [strid]))) (#strMap (#s paramSig))
                         , sigMap = Syntax.SigIdMap.empty
                         , funMap = Syntax.FunIdMap.empty
                         , boundTyVars = Syntax.TyVarMap.empty
                         }
      in typeCheckFunExp'(ctx, span, mergeEnv(env, paramEnv), paramSig, strid0, strexp)
      end

fun typeCheckTopDec(ctx, env, S.StrDec strdec) = let val (env', strdec) = typeCheckStrDec(ctx, env, strdec)
                                                 in (env', List.map T.StrDec strdec)
                                                 end
  | typeCheckTopDec(ctx, env, S.SigDec binds) = let val sigenv = envToSigEnv env
                                                    val sigMap = List.foldl (fn ((sigid, sigexp), m) => Syntax.SigIdMap.insert(m, sigid, evalSignature(ctx, sigenv, sigexp))) (#sigMap env) binds
                                                    val env = { valMap = Syntax.VIdMap.empty
                                                              , tyConMap = Syntax.TyConMap.empty
                                                              , tyNameMap = TypedSyntax.TyNameMap.empty
                                                              , strMap = Syntax.StrIdMap.empty
                                                              , sigMap = sigMap
                                                              , funMap = Syntax.FunIdMap.empty
                                                              , boundTyVars = Syntax.TyVarMap.empty
                                                              }
                                                in (env, [])
                                                end
  | typeCheckTopDec(ctx, env, S.FunDec binds)
    = let val (funMap, binds) = List.foldr (fn ((span, funid, funexp), (funMap, binds)) =>
                                               let val funid' = newFunId(ctx, funid)
                                                   val (funsig, funexp) = typeCheckFunExp(ctx, span, env, funexp)
                                               in (Syntax.FunIdMap.insert (funMap, funid, (funsig, funid')), T.FunDec (funid', funexp) :: binds)
                                               end
                                           ) (Syntax.FunIdMap.empty, []) binds
          val env = { valMap = Syntax.VIdMap.empty
                    , tyConMap = Syntax.TyConMap.empty
                    , tyNameMap = TypedSyntax.TyNameMap.empty
                    , strMap = Syntax.StrIdMap.empty
                    , sigMap = Syntax.SigIdMap.empty
                    , funMap = funMap
                    , boundTyVars = Syntax.TyVarMap.empty
                    }
      in (env, binds)
      end

fun typeCheckTopDecs(ctx, env, []) = (emptyEnv, [])
  | typeCheckTopDecs(ctx, env, dec :: decs) = let val (env', dec) = typeCheckTopDec(ctx, env, dec)
                                                  val (env'', decs) = typeCheckTopDecs(ctx, mergeEnv(env, env'), decs)
                                              in (mergeEnv(env', env''), dec @ decs)
                                              end

(* typeCheckProgram : ProgramContext * Env * ((Syntax.Dec Syntax.TopDec) list) list -> Env * TypedSyntax.TopDec list *)
fun typeCheckProgram (ctx, env, [] : ((Syntax.Dec Syntax.TopDec) list) list) : Env * (TypedSyntax.TopDec list) list = (emptyEnv, [])
  | typeCheckProgram(ctx, env, topdec :: topdecs) = let val (env', topdec') = typeCheckTopDecs (ctx, env, topdec)
                                                        val (env'', topdecs') = typeCheckProgram(ctx, mergeEnv(env, env'), topdecs)
                                                    in (mergeEnv(env', env''), topdec' :: topdecs')
                                                    end
end (* local *)
end (* structure Typing *)
