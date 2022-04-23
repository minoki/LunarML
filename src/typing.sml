(*
 * Copyright (c) 2022 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure Typing = struct

type TyNameAttr = { arity : int
                  , admitsEquality : bool
                  , overloadClass : Syntax.OverloadClass option
                  }

type ('val,'str) Env' = { valMap : (USyntax.TypeScheme * Syntax.IdStatus * 'val) Syntax.VIdMap.map
                        , tyConMap : USyntax.TypeStructure Syntax.TyConMap.map
                        , tyNameMap : TyNameAttr USyntax.TyNameMap.map
                        , strMap : (USyntax.Signature * 'str) Syntax.StrIdMap.map
                        , sigMap : USyntax.QSignature Syntax.SigIdMap.map
                        , funMap : (USyntax.FunSig * USyntax.FunId) Syntax.FunIdMap.map
                        , boundTyVars : USyntax.TyVar Syntax.TyVarMap.map
                        }
type Env = (USyntax.LongVId, USyntax.LongStrId) Env'
type SigEnv = (unit, unit) Env'

val emptyEnv : Env
    = { valMap = Syntax.VIdMap.empty
      , tyConMap = Syntax.TyConMap.empty
      , tyNameMap = USyntax.TyNameMap.empty
      , strMap = Syntax.StrIdMap.empty
      , sigMap = Syntax.SigIdMap.empty
      , funMap = Syntax.FunIdMap.empty
      , boundTyVars = Syntax.TyVarMap.empty
      }

(* mergeEnv : Env * Env -> Env *)
fun mergeEnv(env1 : ('val,'str) Env', env2 : ('val,'str) Env') : ('val,'str) Env'
    = { valMap = Syntax.VIdMap.unionWith #2 (#valMap env1, #valMap env2)
      , tyConMap = Syntax.TyConMap.unionWith #2 (#tyConMap env1, #tyConMap env2)
      , tyNameMap = USyntax.TyNameMap.unionWith #2 (#tyNameMap env1, #tyNameMap env2)
      , strMap = Syntax.StrIdMap.unionWith #2 (#strMap env1, #strMap env2)
      , sigMap = Syntax.SigIdMap.unionWith #2 (#sigMap env1, #sigMap env2)
      , funMap = Syntax.FunIdMap.unionWith #2 (#funMap env1, #funMap env2)
      , boundTyVars = Syntax.TyVarMap.unionWith #2 (#boundTyVars env1, #boundTyVars env2)
      }

fun envWithValEnv valMap : Env
    = { valMap = valMap
      , tyConMap = Syntax.TyConMap.empty
      , tyNameMap = USyntax.TyNameMap.empty
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
      , tyNameMap = USyntax.TyNameMap.empty
      , strMap = strMap
      , sigMap = Syntax.SigIdMap.empty
      , funMap = Syntax.FunIdMap.empty
      , boundTyVars = Syntax.TyVarMap.empty
      }

fun envWithSigMap sigMap
    = { valMap = Syntax.VIdMap.empty
      , tyConMap = Syntax.TyConMap.empty
      , tyNameMap = USyntax.TyNameMap.empty
      , strMap = Syntax.StrIdMap.empty
      , sigMap = sigMap
      , funMap = Syntax.FunIdMap.empty
      , boundTyVars = Syntax.TyVarMap.empty
      }

fun envWithFunMap funMap
    = { valMap = Syntax.VIdMap.empty
      , tyConMap = Syntax.TyConMap.empty
      , tyNameMap = USyntax.TyNameMap.empty
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

fun freeTyVarsInTypeScheme(bound, USyntax.TypeScheme(tyvars, ty)) = USyntax.freeTyVarsInTy(USyntax.TyVarSet.addList(bound, List.map #1 tyvars), ty)
fun freeTyVarsInSignature(bound, { valMap, tyConMap, strMap } : USyntax.Signature)
    = let val valMapSet = Syntax.VIdMap.foldl (fn ((tysc, _), set) => USyntax.TyVarSet.union(set, freeTyVarsInTypeScheme(bound, tysc))) USyntax.TyVarSet.empty valMap
      in Syntax.StrIdMap.foldl (fn (USyntax.MkSignature s, set) => USyntax.TyVarSet.union(set, freeTyVarsInSignature(bound, s))) valMapSet strMap
      end
fun freeTyVarsInEnv(bound, { valMap, tyConMap, tyNameMap, strMap, sigMap, funMap, boundTyVars } : Env)
    = let val boundTyVars = Syntax.TyVarMap.foldl (fn (tv, set) => USyntax.TyVarSet.add(set, tv)) USyntax.TyVarSet.empty boundTyVars
          val valMapSet = Syntax.VIdMap.foldl (fn ((tysc, _, _), set) => USyntax.TyVarSet.union(set, freeTyVarsInTypeScheme(bound, tysc))) boundTyVars valMap
          (* TODO: tyConMap? *)
      in Syntax.StrIdMap.foldl (fn ((s, _), set) => USyntax.TyVarSet.union(set, freeTyVarsInSignature(bound, s))) valMapSet strMap
      end
fun freeTyVarsInConstraint(bound, USyntax.EqConstr(span, ty1, ty2)) = USyntax.TyVarSet.union(USyntax.freeTyVarsInTy(bound, ty1), USyntax.freeTyVarsInTy(bound, ty2))
  | freeTyVarsInConstraint(bound, USyntax.UnaryConstraint(span, ty, unaryConstraint)) = USyntax.TyVarSet.union (USyntax.freeTyVarsInTy(bound, ty), USyntax.freeTyVarsInUnaryConstraint (bound, unaryConstraint))

type Context = { nextTyVar : int ref
               , nextVId : int ref
               }

type InferenceContext = { context : Context
                        , tyVarConstraints : ((USyntax.UnaryConstraint list) USyntax.TyVarMap.map) ref
                        , tyVarSubst : (USyntax.Ty USyntax.TyVarMap.map) ref
                        }

exception TypeError of SourcePos.span list * string

fun emitError (ctx : Context, spans, message) = raise TypeError (spans, message)
fun emitTypeError (ctx : InferenceContext, spans, message) = emitError (#context ctx, spans, message)

(* lookupStr : Context * USyntax.Signature * SourcePos.span * Syntax.StrId list -> USyntax.Signature *)
fun lookupStr(ctx, s : USyntax.Signature, span, nil) = s
  | lookupStr(ctx, s as { strMap = strMap, ... }, span, (strid0 as Syntax.MkStrId name) :: strids)
    = (case Syntax.StrIdMap.find(strMap, strid0) of
           NONE => emitError(ctx, [span], "unknown structure name '" ^ name ^ "'")
         | SOME (USyntax.MkSignature innerEnv) => lookupStr(ctx, innerEnv, span, strids)
      )
(* lookupTyConInEnv : Context * ('val,'str) Env' * SourcePos.span * Syntax.LongTyCon -> U.TypeStructure *)
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
    = (case USyntax.TyNameMap.find(tyNameMap, tyname) of
           SOME attr => attr
         | NONE => emitError(ctx, [span], "unknown type constructor " ^ USyntax.print_TyName tyname ^ " (internal error)")
      )

datatype 'a LookupResult = Found of 'a
                         | ValueNotFound of Syntax.VId Syntax.Qualified
                         | StructureNotFound of Syntax.StrId Syntax.Qualified

(* lookupStr' : Context * USyntax.Signature * Syntax.StrId list -> USyntax.Signature LookupResult *)
fun lookupStr' (ctx, s : USyntax.Signature, _, nil) = Found s
  | lookupStr' (ctx, s as { strMap, ... }, revStrIds, (strid0 as Syntax.MkStrId name) :: strids)
    = (case Syntax.StrIdMap.find (strMap, strid0) of
           NONE => StructureNotFound (Syntax.MkQualified (List.rev revStrIds, strid0))
         | SOME (USyntax.MkSignature innerEnv) => lookupStr' (ctx, innerEnv, strid0 :: revStrIds, strids)
      )

(* Context * Env * SourcePos.span * Syntax.LongVId -> (USyntax.LongVId, USyntax.TypeScheme * Syntax.IdStatus) LookupResult *)
fun lookupLongVIdInEnv (ctx, env : Env, span, longvid as Syntax.MkQualified ([], vid))
    = (case Syntax.VIdMap.find (#valMap env, vid) of
           SOME (tysc, ids, longvid) => Found (longvid, tysc, ids)
         | NONE => ValueNotFound longvid
      )
  | lookupLongVIdInEnv (ctx, env, span, longvid as Syntax.MkQualified (strid0 :: strids, vid))
    = (case Syntax.StrIdMap.find (#strMap env, strid0) of
           SOME (s, USyntax.MkLongStrId (strid0, strids0)) =>
           (case lookupStr' (ctx, s, [], strids) of
                Found { valMap, ... } => (case Syntax.VIdMap.find(valMap, vid) of
                                              SOME (tysc, ids) => Found (USyntax.MkLongVId(strid0, strids0 @ strids, vid), tysc, ids)
                                            | NONE => ValueNotFound longvid
                                         )
             | StructureNotFound notfound => StructureNotFound notfound
             | ValueNotFound notfound => ValueNotFound notfound (* cannot occur *)
           )
         | NONE => StructureNotFound (Syntax.MkQualified ([], strid0))
      )

(* getConstructedType : Context * SourcePos.span * USyntax.Ty -> USyntax.TyCon *)
fun getConstructedType(ctx, span, USyntax.TyVar _) = emitError(ctx, [span], "getConstructedType: got a type variable")
  | getConstructedType(ctx, span, USyntax.RecordType _) = emitError(ctx, [span], "getConstructedType: got a record")
  | getConstructedType(ctx, span, USyntax.TyCon(_, tyargs, tycon)) = tycon
  | getConstructedType(ctx, span, USyntax.FnType(_, _, t)) = getConstructedType(ctx, span, t)

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
  | isNonexpansive(env, USyntax.VectorExp(_, xs, _)) = Vector.all (fn x => isNonexpansive(env, x)) xs
  | isNonexpansive(env, USyntax.PrimExp(_, Syntax.PrimOp_call2, _, args)) = false
  | isNonexpansive(env, _) = false
and isConexp(env : Env, USyntax.TypedExp(_, e, _)) = isConexp(env, e)
  | isConexp(env, USyntax.VarExp(_, _, Syntax.ValueVariable, _)) = false
  | isConexp(env, USyntax.VarExp(_, USyntax.MkShortVId(USyntax.MkVId(name, _)), Syntax.ValueConstructor _, _)) = name <> "ref"
  | isConexp(env, USyntax.VarExp(_, USyntax.MkLongVId(_, _, Syntax.MkVId(name)), Syntax.ValueConstructor _, _)) = name <> "ref"
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
  | isExhaustive(ctx, env, USyntax.VectorPat(_, pats, ellipsis, elemTy)) = ellipsis andalso Vector.length pats = 0

val primTyName_int    = USyntax.MkTyName("int", 0)
val primTyName_word   = USyntax.MkTyName("word", 1)
val primTyName_real   = USyntax.MkTyName("real", 2)
val primTyName_string = USyntax.MkTyName("string", 3)
val primTyName_char   = USyntax.MkTyName("char", 4)
val primTyName_exn    = USyntax.MkTyName("exn", 5)
val primTyName_bool   = USyntax.MkTyName("bool", 6)
val primTyName_ref    = USyntax.MkTyName("ref", 7)
val primTyName_list   = USyntax.MkTyName("list", 8)
val primTyName_array  = USyntax.MkTyName("array", 9)
val primTyName_vector = USyntax.MkTyName("vector", 10)
val primTyName_exntag = USyntax.MkTyName("exntag", 11)
val primTyName_function2 = USyntax.MkTyName("function2", 12)
val primTyName_function3 = USyntax.MkTyName("function3", 13)
val primTyName_wideChar = USyntax.MkTyName ("WideChar.char", 14)
val primTyName_wideString = USyntax.MkTyName ("WideString.string", 15)
val primTyName_Lua_value = USyntax.MkTyName ("Lua.value", 16)
val primTyName_JavaScript_value = USyntax.MkTyName ("JavaScript.value", 17)
val primTy_unit   = USyntax.RecordType(SourcePos.nullSpan, Syntax.LabelMap.empty)
val primTy_int    = USyntax.TyCon(SourcePos.nullSpan, [], primTyName_int)
val primTy_word   = USyntax.TyCon(SourcePos.nullSpan, [], primTyName_word)
val primTy_real   = USyntax.TyCon(SourcePos.nullSpan, [], primTyName_real)
val primTy_string = USyntax.TyCon(SourcePos.nullSpan, [], primTyName_string)
val primTy_char   = USyntax.TyCon(SourcePos.nullSpan, [], primTyName_char)
val primTy_exn    = USyntax.TyCon(SourcePos.nullSpan, [], primTyName_exn)
val primTy_bool   = USyntax.TyCon(SourcePos.nullSpan, [], primTyName_bool)
val primTy_wideChar = USyntax.TyCon (SourcePos.nullSpan, [], primTyName_wideChar)
val primTy_wideString = USyntax.TyCon (SourcePos.nullSpan, [], primTyName_wideString)
val primTy_Lua_value = USyntax.TyCon(SourcePos.nullSpan, [], primTyName_Lua_value)
val primTy_JavaScript_value = USyntax.TyCon (SourcePos.nullSpan, [], primTyName_JavaScript_value)
val VId_Bind = USyntax.MkVId("Bind", ~1)
val LongVId_Bind = USyntax.MkShortVId(VId_Bind)

fun isRefOrArray (tyname : USyntax.TyName) = USyntax.eqTyName (tyname, primTyName_ref) orelse USyntax.eqTyName (tyname, primTyName_array)

type PrimTypeScheme = { typeVariables : (USyntax.TyVar * USyntax.UnaryConstraint list) list, argTypes : USyntax.Ty vector, resultType : USyntax.Ty }
local
    val tyVarA = USyntax.AnonymousTyVar 0
    val tyVarB = USyntax.AnonymousTyVar 1
    val tyVarC = USyntax.AnonymousTyVar 2
    val tyVarD = USyntax.AnonymousTyVar 3
    val tyA = USyntax.TyVar (SourcePos.nullSpan, tyVarA)
    val tyB = USyntax.TyVar (SourcePos.nullSpan, tyVarB)
    val tyC = USyntax.TyVar (SourcePos.nullSpan, tyVarC)
    val tyD = USyntax.TyVar (SourcePos.nullSpan, tyVarD)
    fun Binary (a, b) result = { typeVariables = []
                               , argTypes = vector [a, b]
                               , resultType = result
                               }
    fun HomoBinary a = Binary (a, a) a
    fun Compare a = Binary (a, a) primTy_bool
    fun LuaUnary resultType = { typeVariables = []
                              , argTypes = vector [primTy_Lua_value]
                              , resultType = resultType
                              }
    fun LuaBinary resultType = { typeVariables = []
                               , argTypes = vector [primTy_Lua_value, primTy_Lua_value]
                               , resultType = resultType
                               }
    fun JsUnary resultType = { typeVariables = []
                             , argTypes = vector [primTy_JavaScript_value]
                             , resultType = resultType
                             }
    fun JsBinary resultType = { typeVariables = []
                              , argTypes = vector [primTy_JavaScript_value, primTy_JavaScript_value]
                              , resultType = resultType
                              }
in
(* PRIMITIVES *)
fun typeOfPrimCall Syntax.PrimOp_call2 : PrimTypeScheme
    = { typeVariables = [(tyVarA, []) (* result *)
                        ,(tyVarB, []) (* arg1 *)
                        ,(tyVarC, []) (* arg2 *)
                        ]
      , argTypes = vector [USyntax.TyCon (SourcePos.nullSpan, [tyA, tyB, tyC], primTyName_function2), tyB, tyC]
      , resultType = tyA
      }
  | typeOfPrimCall Syntax.PrimOp_call3
    = { typeVariables = [(tyVarA, []) (* result *)
                        ,(tyVarB, []) (* arg1 *)
                        ,(tyVarC, []) (* arg2 *)
                        ,(tyVarD, []) (* arg3 *)
                        ]
      , argTypes = vector [USyntax.TyCon (SourcePos.nullSpan, [tyA, tyB, tyC, tyD], primTyName_function3), tyB, tyC, tyD]
      , resultType = tyA
      }
  | typeOfPrimCall Syntax.PrimOp_Ref_set = { typeVariables = [(tyVarA, [])]
                                           , argTypes = vector [USyntax.TyCon (SourcePos.nullSpan, [tyA], primTyName_ref), tyA]
                                           , resultType = primTy_unit
                                           }
  | typeOfPrimCall Syntax.PrimOp_Ref_read = { typeVariables = [(tyVarA, [])]
                                            , argTypes = vector [USyntax.TyCon (SourcePos.nullSpan, [tyA], primTyName_ref)]
                                            , resultType = tyA
                                            }
  | typeOfPrimCall Syntax.PrimOp_Bool_not = { typeVariables = []
                                            , argTypes = vector [primTy_bool]
                                            , resultType = primTy_bool
                                            }
  | typeOfPrimCall Syntax.PrimOp_Int_LT = Compare primTy_int
  | typeOfPrimCall Syntax.PrimOp_Int_LE = Compare primTy_int
  | typeOfPrimCall Syntax.PrimOp_Int_GT = Compare primTy_int
  | typeOfPrimCall Syntax.PrimOp_Int_GE = Compare primTy_int
  | typeOfPrimCall Syntax.PrimOp_Word_PLUS = HomoBinary primTy_word
  | typeOfPrimCall Syntax.PrimOp_Word_MINUS = HomoBinary primTy_word
  | typeOfPrimCall Syntax.PrimOp_Word_TIMES = HomoBinary primTy_word
  | typeOfPrimCall Syntax.PrimOp_Word_TILDE = { typeVariables = []
                                              , argTypes = vector [primTy_word]
                                              , resultType = primTy_word
                                              }
  | typeOfPrimCall Syntax.PrimOp_Word_LT = Compare primTy_word
  | typeOfPrimCall Syntax.PrimOp_Word_LE = Compare primTy_word
  | typeOfPrimCall Syntax.PrimOp_Word_GT = Compare primTy_word
  | typeOfPrimCall Syntax.PrimOp_Word_GE = Compare primTy_word
  | typeOfPrimCall Syntax.PrimOp_Real_PLUS = HomoBinary primTy_real
  | typeOfPrimCall Syntax.PrimOp_Real_MINUS = HomoBinary primTy_real
  | typeOfPrimCall Syntax.PrimOp_Real_TIMES = HomoBinary primTy_real
  | typeOfPrimCall Syntax.PrimOp_Real_DIVIDE = HomoBinary primTy_real
  | typeOfPrimCall Syntax.PrimOp_Real_TILDE = { typeVariables = []
                                              , argTypes = vector [primTy_real]
                                              , resultType = primTy_real
                                              }
  | typeOfPrimCall Syntax.PrimOp_Real_LT = Compare primTy_real
  | typeOfPrimCall Syntax.PrimOp_Real_LE = Compare primTy_real
  | typeOfPrimCall Syntax.PrimOp_Real_GT = Compare primTy_real
  | typeOfPrimCall Syntax.PrimOp_Real_GE = Compare primTy_real
  | typeOfPrimCall Syntax.PrimOp_Char_LT = Compare primTy_char
  | typeOfPrimCall Syntax.PrimOp_Char_LE = Compare primTy_char
  | typeOfPrimCall Syntax.PrimOp_Char_GT = Compare primTy_char
  | typeOfPrimCall Syntax.PrimOp_Char_GE = Compare primTy_char
  | typeOfPrimCall Syntax.PrimOp_WideChar_LT = Compare primTy_wideChar
  | typeOfPrimCall Syntax.PrimOp_WideChar_LE = Compare primTy_wideChar
  | typeOfPrimCall Syntax.PrimOp_WideChar_GT = Compare primTy_wideChar
  | typeOfPrimCall Syntax.PrimOp_WideChar_GE = Compare primTy_wideChar
  | typeOfPrimCall Syntax.PrimOp_String_LT = Compare primTy_string
  | typeOfPrimCall Syntax.PrimOp_String_LE = Compare primTy_string
  | typeOfPrimCall Syntax.PrimOp_String_GT = Compare primTy_string
  | typeOfPrimCall Syntax.PrimOp_String_GE = Compare primTy_string
  | typeOfPrimCall Syntax.PrimOp_String_HAT = HomoBinary primTy_string
  | typeOfPrimCall Syntax.PrimOp_String_size = { typeVariables = []
                                               , argTypes = vector [primTy_string]
                                               , resultType = primTy_int
                                               }
  | typeOfPrimCall Syntax.PrimOp_WideString_LT = Compare primTy_wideString
  | typeOfPrimCall Syntax.PrimOp_WideString_LE = Compare primTy_wideString
  | typeOfPrimCall Syntax.PrimOp_WideString_GT = Compare primTy_wideString
  | typeOfPrimCall Syntax.PrimOp_WideString_GE = Compare primTy_wideString
  | typeOfPrimCall Syntax.PrimOp_WideString_HAT = HomoBinary primTy_wideString
  | typeOfPrimCall Syntax.PrimOp_WideString_size = { typeVariables = []
                                                   , argTypes = vector [primTy_wideString]
                                                   , resultType = primTy_int
                                                   }
  | typeOfPrimCall Syntax.PrimOp_Vector_length = { typeVariables = [(tyVarA, [])]
                                                 , argTypes = vector [USyntax.TyCon (SourcePos.nullSpan, [tyA], primTyName_vector)]
                                                 , resultType = primTy_int
                                                 }
  | typeOfPrimCall Syntax.PrimOp_Array_length = { typeVariables = [(tyVarA, [])]
                                                , argTypes = vector [USyntax.TyCon (SourcePos.nullSpan, [tyA], primTyName_array)]
                                                , resultType = primTy_int
                                                }
  | typeOfPrimCall Syntax.PrimOp_Unsafe_cast = { typeVariables = [(tyVarA, []), (tyVarB, [])]
                                                , argTypes = vector [tyA]
                                                , resultType = tyB
                                                }
  | typeOfPrimCall Syntax.PrimOp_Unsafe_Vector_sub = { typeVariables = [(tyVarA, [])]
                                                     , argTypes = vector [USyntax.TyCon (SourcePos.nullSpan, [tyA], primTyName_vector), primTy_int]
                                                     , resultType = tyA
                                                     }
  | typeOfPrimCall Syntax.PrimOp_Unsafe_Array_sub = { typeVariables = [(tyVarA, [])]
                                                    , argTypes = vector [USyntax.TyCon (SourcePos.nullSpan, [tyA], primTyName_array), primTy_int]
                                                    , resultType = tyA
                                                    }
  | typeOfPrimCall Syntax.PrimOp_Unsafe_Array_update = { typeVariables = [(tyVarA, [])]
                                                       , argTypes = vector [USyntax.TyCon (SourcePos.nullSpan, [tyA], primTyName_array), primTy_int, tyA]
                                                       , resultType = primTy_unit
                                                       }
  | typeOfPrimCall Syntax.PrimOp_Lua_sub = LuaBinary primTy_Lua_value
  | typeOfPrimCall Syntax.PrimOp_Lua_set = { typeVariables = []
                                           , argTypes = vector [primTy_Lua_value, primTy_Lua_value, primTy_Lua_value]
                                           , resultType = primTy_unit
                                           }
  | typeOfPrimCall Syntax.PrimOp_Lua_isNil = LuaUnary primTy_bool
  | typeOfPrimCall Syntax.PrimOp_Lua_EQUAL = LuaBinary primTy_bool
  | typeOfPrimCall Syntax.PrimOp_Lua_NOTEQUAL = LuaBinary primTy_bool
  | typeOfPrimCall Syntax.PrimOp_Lua_LT = LuaBinary primTy_bool
  | typeOfPrimCall Syntax.PrimOp_Lua_GT = LuaBinary primTy_bool
  | typeOfPrimCall Syntax.PrimOp_Lua_LE = LuaBinary primTy_bool
  | typeOfPrimCall Syntax.PrimOp_Lua_GE = LuaBinary primTy_bool
  | typeOfPrimCall Syntax.PrimOp_Lua_PLUS = LuaBinary primTy_Lua_value
  | typeOfPrimCall Syntax.PrimOp_Lua_MINUS = LuaBinary primTy_Lua_value
  | typeOfPrimCall Syntax.PrimOp_Lua_TIMES = LuaBinary primTy_Lua_value
  | typeOfPrimCall Syntax.PrimOp_Lua_DIVIDE = LuaBinary primTy_Lua_value
  | typeOfPrimCall Syntax.PrimOp_Lua_INTDIV = LuaBinary primTy_Lua_value
  | typeOfPrimCall Syntax.PrimOp_Lua_MOD = LuaBinary primTy_Lua_value
  | typeOfPrimCall Syntax.PrimOp_Lua_pow = LuaBinary primTy_Lua_value
  | typeOfPrimCall Syntax.PrimOp_Lua_unm = LuaUnary primTy_Lua_value
  | typeOfPrimCall Syntax.PrimOp_Lua_andb = LuaBinary primTy_Lua_value
  | typeOfPrimCall Syntax.PrimOp_Lua_orb = LuaBinary primTy_Lua_value
  | typeOfPrimCall Syntax.PrimOp_Lua_xorb = LuaBinary primTy_Lua_value
  | typeOfPrimCall Syntax.PrimOp_Lua_notb = LuaUnary primTy_Lua_value
  | typeOfPrimCall Syntax.PrimOp_Lua_LSHIFT = LuaBinary primTy_Lua_value
  | typeOfPrimCall Syntax.PrimOp_Lua_RSHIFT = LuaBinary primTy_Lua_value
  | typeOfPrimCall Syntax.PrimOp_Lua_concat = LuaBinary primTy_Lua_value
  | typeOfPrimCall Syntax.PrimOp_Lua_length = LuaUnary primTy_Lua_value
  | typeOfPrimCall Syntax.PrimOp_Lua_isFalsy = LuaUnary primTy_bool
  | typeOfPrimCall Syntax.PrimOp_JavaScript_sub = JsBinary primTy_JavaScript_value
  | typeOfPrimCall Syntax.PrimOp_JavaScript_set = { typeVariables = []
                                                  , argTypes = vector [primTy_JavaScript_value, primTy_JavaScript_value, primTy_JavaScript_value]
                                                  , resultType = primTy_unit
                                                  }
  | typeOfPrimCall Syntax.PrimOp_JavaScript_EQUAL = JsBinary primTy_bool
  | typeOfPrimCall Syntax.PrimOp_JavaScript_NOTEQUAL = JsBinary primTy_bool
  | typeOfPrimCall Syntax.PrimOp_JavaScript_LT = JsBinary primTy_bool
  | typeOfPrimCall Syntax.PrimOp_JavaScript_GT = JsBinary primTy_bool
  | typeOfPrimCall Syntax.PrimOp_JavaScript_LE = JsBinary primTy_bool
  | typeOfPrimCall Syntax.PrimOp_JavaScript_GE = JsBinary primTy_bool
  | typeOfPrimCall Syntax.PrimOp_JavaScript_PLUS = JsBinary primTy_JavaScript_value
  | typeOfPrimCall Syntax.PrimOp_JavaScript_MINUS = JsBinary primTy_JavaScript_value
  | typeOfPrimCall Syntax.PrimOp_JavaScript_TIMES = JsBinary primTy_JavaScript_value
  | typeOfPrimCall Syntax.PrimOp_JavaScript_DIVIDE = JsBinary primTy_JavaScript_value
  | typeOfPrimCall Syntax.PrimOp_JavaScript_MOD = JsBinary primTy_JavaScript_value
  | typeOfPrimCall Syntax.PrimOp_JavaScript_negate = JsUnary primTy_JavaScript_value
  | typeOfPrimCall Syntax.PrimOp_JavaScript_andb = JsBinary primTy_JavaScript_value
  | typeOfPrimCall Syntax.PrimOp_JavaScript_orb = JsBinary primTy_JavaScript_value
  | typeOfPrimCall Syntax.PrimOp_JavaScript_xorb = JsBinary primTy_JavaScript_value
  | typeOfPrimCall Syntax.PrimOp_JavaScript_notb = JsUnary primTy_JavaScript_value
  | typeOfPrimCall Syntax.PrimOp_JavaScript_LSHIFT = JsBinary primTy_JavaScript_value
  | typeOfPrimCall Syntax.PrimOp_JavaScript_RSHIFT = JsBinary primTy_JavaScript_value
  | typeOfPrimCall Syntax.PrimOp_JavaScript_URSHIFT = JsBinary primTy_JavaScript_value
  | typeOfPrimCall Syntax.PrimOp_JavaScript_isFalsy = JsUnary primTy_bool
end

fun newContext() : Context
    = { nextTyVar = ref 100
      , nextVId = ref 100
      }

fun addTyVarConstraint(ctx : InferenceContext, tv : USyntax.TyVar, ct : USyntax.UnaryConstraint)
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

fun newFunId(ctx : Context, Syntax.MkFunId name) = let val n = !(#nextVId ctx)
                                                   in #nextVId ctx := n + 1
                                                    ; USyntax.MkFunId(name, n)
                                                   end

fun genTyConId(ctx : Context)
    = let val id = !(#nextTyVar ctx)
      in #nextTyVar ctx := id + 1 ; id end
fun newTyName(ctx, Syntax.MkTyCon name) = USyntax.MkTyName(name, genTyConId(ctx))
fun renewTyName (ctx : Context, USyntax.MkTyName (name, _))
    = let val id = !(#nextTyVar ctx)
      in #nextTyVar ctx := id + 1
       ; USyntax.MkTyName (name, id)
      end

local
    structure S = Syntax
    structure U = USyntax
in
(* occurCheck : U.TyVar -> U.Ty -> bool; returns true if the type variable occurs in the type *)
fun occurCheck tv = let fun check (U.TyVar(_, tv')) = U.eqUTyVar(tv, tv')
                          | check (U.RecordType(_, xs)) = Syntax.LabelMap.exists check xs
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
          | substTy (U.RecordType(span, fields)) = U.RecordType (span, Syntax.LabelMap.map substTy fields)
          | substTy (U.TyCon(span, tyargs, tycon)) = U.TyCon(span, List.map substTy tyargs, tycon)
          | substTy (U.FnType(span, ty1, ty2)) = U.FnType(span, substTy ty1, substTy ty2)
    in substTy
    end

(* substituteConstraint : TyVar * Ty -> Constraint -> Constraint *)
fun substituteConstraint (tv, replacement) =
    let val substTy = substituteTy (tv, replacement)
    in fn U.EqConstr(span, ty1, ty2) => U.EqConstr(span, substTy ty1, substTy ty2)
     | U.UnaryConstraint(span1, recordTy, U.HasField { sourceSpan, label, fieldTy }) => U.UnaryConstraint(span1, substTy recordTy, U.HasField { sourceSpan = sourceSpan, label = label, fieldTy = substTy fieldTy })
     | U.UnaryConstraint(span1, recordTy, U.RecordExt { sourceSpan, fields, baseTy }) => U.UnaryConstraint(span1, substTy recordTy, U.RecordExt { sourceSpan = sourceSpan, fields = List.map (fn (label, ty) => (label, substTy ty)) fields, baseTy = substTy baseTy })
     | U.UnaryConstraint(span1, recordTy, U.SubrecordOf { sourceSpan, extraFields, extendedTy }) => U.UnaryConstraint(span1, substTy recordTy, U.SubrecordOf { sourceSpan = sourceSpan, extraFields = List.map (fn (label, ty) => (label, substTy ty)) extraFields, extendedTy = substTy extendedTy })
     | U.UnaryConstraint(span1, ty, c as U.IsEqType _) => U.UnaryConstraint(span1, substTy ty, c)
     | U.UnaryConstraint(span1, ty, c as U.IsIntegral _) => U.UnaryConstraint(span1, substTy ty, c)
     | U.UnaryConstraint(span1, ty, c as U.IsSignedReal _) => U.UnaryConstraint(span1, substTy ty, c)
     | U.UnaryConstraint(span1, ty, c as U.IsRing _) => U.UnaryConstraint(span1, substTy ty, c)
     | U.UnaryConstraint(span1, ty, c as U.IsField _) => U.UnaryConstraint(span1, substTy ty, c)
     | U.UnaryConstraint(span1, ty, c as U.IsSigned _) => U.UnaryConstraint(span1, substTy ty, c)
     | U.UnaryConstraint(span1, ty, c as U.IsOrdered _) => U.UnaryConstraint(span1, substTy ty, c)
     | U.UnaryConstraint(span1, ty, c as U.IsInt _) => U.UnaryConstraint(span1, substTy ty, c)
     | U.UnaryConstraint(span1, ty, c as U.IsWord _) => U.UnaryConstraint(span1, substTy ty, c)
     | U.UnaryConstraint(span1, ty, c as U.IsReal _) => U.UnaryConstraint(span1, substTy ty, c)
     | U.UnaryConstraint(span1, ty, c as U.IsChar _) => U.UnaryConstraint(span1, substTy ty, c)
     | U.UnaryConstraint(span1, ty, c as U.IsString _) => U.UnaryConstraint(span1, substTy ty, c)
    end

val applySubstTy = U.applySubstTy
fun applySubstEnv subst =
    let fun substTypeScheme(U.TypeScheme(tyvars, ty))
            = let val subst' = U.TyVarMap.filteri (fn (tv, ty) => not (List.exists (fn (tv', _) => U.eqUTyVar(tv', tv)) tyvars)) subst
              in U.TypeScheme(tyvars, applySubstTy subst' ty)
                 (* TODO: unwanted capture? e.g. 'a. 'a list * 'c, 'c := 'b * 'a *)
              end
        fun substSignature ({ valMap, tyConMap, strMap } : U.Signature) : U.Signature
            = { valMap = S.VIdMap.map (fn (tysc, ids) => (substTypeScheme(tysc), ids)) valMap
              , tyConMap = tyConMap (* ??? *)
              , strMap = Syntax.StrIdMap.map (fn U.MkSignature s => U.MkSignature (substSignature s)) strMap
              }
        fun substEnv ({ valMap, tyConMap, tyNameMap, strMap, sigMap, funMap, boundTyVars } : Env) : Env
            = { valMap = S.VIdMap.map (fn (tysc, ids, longvid) => (substTypeScheme(tysc), ids, longvid)) valMap
              , tyConMap = tyConMap (* ??? *)
              , tyNameMap = tyNameMap
              , strMap = Syntax.StrIdMap.map (fn (s, strid) => (substSignature s, strid)) strMap
              , sigMap = sigMap
              , funMap = funMap
              , boundTyVars = boundTyVars
              }
    in substEnv
    end

(* instantiate : InferenceContext * SourcePos.span * U.TypeScheme -> U.Ty * (U.Ty * U.UnaryConstraint list) list *)
fun instantiate (ctx : InferenceContext, span, U.TypeScheme (vars, ty))
    = let val (subst, tyargs) = List.foldl (fn ((v, preds), (set, rest)) =>
                                               let val tv = freshTyVar (#context ctx)
                                                   val tyarg = U.TyVar(span, tv)
                                               in List.app (fn pred => addTyVarConstraint(ctx, tv, pred)) preds
                                                ; (U.TyVarMap.insert(set, v, tyarg), (tyarg, preds) :: rest)
                                               end
                                           ) (U.TyVarMap.empty, []) vars
      in (applySubstTy subst ty, List.rev tyargs)
      end

(* unify : InferenceContext * Env * Constraint list -> unit *)
(* The environment is used to determine if a data type admits equality *)
fun unify(ctx : InferenceContext, env : Env, nil : U.Constraint list) : unit = ()
  | unify(ctx, env, ct :: ctrs)
    = (case ct of
           U.EqConstr(span1, U.TyVar(span2, tv as U.AnonymousTyVar _), ty) => unifyTyVarAndTy(ctx, env, span1, tv, ty, ctrs)
         | U.EqConstr(span1, ty, U.TyVar(span2, tv as U.AnonymousTyVar _)) => unifyTyVarAndTy(ctx, env, span1, tv, ty, ctrs)
         | U.EqConstr(span1, U.TyVar(span2, tv as U.NamedTyVar (name, eq, x)), U.TyVar(span3, tv' as U.NamedTyVar (name', eq', x'))) =>
           if U.eqUTyVar (tv, tv') then
               unify(ctx, env, ctrs) (* do nothing *)
           else
               emitTypeError (ctx, [span1, span2, span3], "cannot unify named type variable: " ^ name ^ " and " ^ name')
         | U.EqConstr(span1, U.TyVar(span2, U.NamedTyVar (name, eq, _)), ty) => emitTypeError (ctx, [span1, span2], "cannot unify named type variable: " ^ name)
         | U.EqConstr(span1, ty, U.TyVar(span2, U.NamedTyVar (name, eq, _))) => emitTypeError (ctx, [span1, span2], "cannot unify named type variable: " ^ name)
         | U.EqConstr(span, U.FnType(_, s0, s1), U.FnType(_, t0, t1)) => unify(ctx, env, U.EqConstr(span, s0, t0) :: U.EqConstr(span, s1, t1) :: ctrs)
         | U.EqConstr(span1, U.RecordType(span2, fields), U.RecordType(span3, fields')) =>
           if Syntax.LabelMap.numItems fields <> Syntax.LabelMap.numItems fields then
               emitTypeError (ctx, [span1, span2, span3], "unification failed: incompatible record types (different number of fields)")
           else
               unify(ctx, env, Syntax.LabelMap.foldli (fn (label, ty, acc) =>
                                                          case Syntax.LabelMap.find (fields', label) of
                                                              NONE => emitTypeError (ctx, [span1, span2, span3], "unification failed: incompatible record types")
                                                            | SOME ty' => U.EqConstr(span1, ty, ty') :: acc)
                                                      ctrs fields)
         | U.EqConstr(span1, t1 as U.TyCon(span2, tyarg, con), t2 as U.TyCon(span3, tyarg', con')) =>
           if U.eqTyName(con, con') then
               unify(ctx, env, (ListPair.mapEq (fn (x, y) => U.EqConstr(span1, x, y)) (tyarg, tyarg')
                                handle ListPair.UnequalLengths => emitTypeError (ctx, [span1, span2, span3], "unification failed: the number of type arguments differ")
                               ) @ ctrs)
           else
               emitTypeError (ctx, [span1, span2, span3], "unification failed: type constructor mismatch (" ^ USyntax.PrettyPrint.print_Ty t1 ^ " vs " ^ USyntax.PrettyPrint.print_Ty t2 ^ ")") (* ??? *)
         | U.EqConstr (span, ty1, ty2) => emitTypeError (ctx, [span], "unification failed: not match (" ^ USyntax.PrettyPrint.print_Ty ty1 ^ " vs " ^ USyntax.PrettyPrint.print_Ty ty2 ^ ")")
         | U.UnaryConstraint(span1, recordTy, U.HasField{sourceSpan = span3, label = label, fieldTy = fieldTy}) =>
           (case recordTy of
                U.RecordType(span2, fields) =>
                (case Syntax.LabelMap.find (fields, label) of
                     NONE => emitTypeError (ctx, [span1, span2, span3], "unification failed: no field")
                   | SOME ty' => unify(ctx, env, U.EqConstr(span1, fieldTy, ty') :: ctrs)
                )
              | U.TyCon (span2, _, _) => emitTypeError (ctx, [span1, span2, span3], "record field for a non-record type")
              | U.FnType (span2, _, _) => emitTypeError (ctx, [span1, span2, span3], "record field for a function type")
              | U.TyVar(span2, tv) =>
                (case USyntax.TyVarMap.find(!(#tyVarSubst ctx), tv) of
                     SOME replacement => unify(ctx, env, U.UnaryConstraint(span1, replacement, U.HasField{sourceSpan = span3, label = label, fieldTy = fieldTy}) :: ctrs)
                   | NONE => ( addTyVarConstraint(ctx, tv, U.HasField{ sourceSpan = span3, label = label, fieldTy = fieldTy })
                             ; unify(ctx, env, ctrs)
                             )
                )
           )
         | U.UnaryConstraint(span1, recordTy, c as U.RecordExt { sourceSpan = span3, fields, baseTy }) =>
           (case recordTy of
                U.RecordType (span2, fields') =>
                let val (fields', ctrs) = List.foldl (fn ((label, ty), (fields', ctrs)) =>
                                                         if Syntax.LabelMap.inDomain (fields', label) then
                                                             let val (fields', ty') = Syntax.LabelMap.remove (fields', label)
                                                             in (fields', U.EqConstr (span1, ty, ty') :: ctrs)
                                                             end
                                                         else
                                                             emitTypeError (ctx, [span1, span2, span3], "record field mismatch: " ^ Syntax.print_Label label)
                                                     ) (fields', ctrs) fields
                in unify (ctx, env, U.EqConstr (span1, baseTy, U.RecordType (span1, fields')) :: ctrs)
                end
              | U.TyCon (span2, _, _) => emitTypeError (ctx, [span1, span2, span3], "record field for a non-record type")
              | U.FnType (span2, _, _) => emitTypeError (ctx, [span1, span2, span3], "record field for a function type")
              | U.TyVar (span2, tv) =>
                case USyntax.TyVarMap.find (!(#tyVarSubst ctx), tv) of
                    SOME replacement => unify (ctx, env, U.UnaryConstraint (span1, replacement, c) :: ctrs)
                  | NONE => ( addTyVarConstraint (ctx, tv, c)
                            ; unify (ctx, env, ctrs)
                            )
           )
         | U.UnaryConstraint(span1, recordTy, c as U.SubrecordOf { sourceSpan = span3, extraFields, extendedTy }) =>
           (case recordTy of
                U.RecordType (span2, fields') =>
                let val fields' = List.foldl (fn ((label, ty), fields') =>
                                                 if Syntax.LabelMap.inDomain (fields', label) then
                                                     emitTypeError (ctx, [span1, span2, span3], "duplicate record field: " ^ Syntax.print_Label label)
                                                 else
                                                     Syntax.LabelMap.insert (fields', label, ty)
                                             ) fields' extraFields
                in unify (ctx, env, U.EqConstr (span1, extendedTy, U.RecordType (span1, fields')) :: ctrs)
                end
              | U.TyCon (span2, _, _) => emitTypeError (ctx, [span1, span2, span3], "record extension for a non-record type")
              | U.FnType (span2, _, _) => emitTypeError (ctx, [span1, span2, span3], "record extension for a function type")
              | U.TyVar (span2, tv) =>
                case USyntax.TyVarMap.find (!(#tyVarSubst ctx), tv) of
                    SOME replacement => unify (ctx, env, U.UnaryConstraint (span1, replacement, c) :: ctrs)
                  | NONE => ( addTyVarConstraint (ctx, tv, c)
                            ; unify (ctx, env, ctrs)
                            )
           )
         | U.UnaryConstraint(span1, U.RecordType(span2, fields), U.IsEqType span3) => unify(ctx, env, Syntax.LabelMap.foldr (fn (ty, acc) => U.UnaryConstraint(span1, ty, U.IsEqType span3) :: acc) ctrs fields)
         | U.UnaryConstraint (span1, U.RecordType (span2, _), U.IsIntegral span3) => emitTypeError (ctx, [span1, span2, span3], "cannot apply arithmetic operator on record type")
         | U.UnaryConstraint (span1, U.RecordType (span2, _), U.IsSignedReal span3) => emitTypeError (ctx, [span1, span2, span3], "cannot apply arithmetic operator on record type")
         | U.UnaryConstraint (span1, U.RecordType (span2, _), U.IsRing span3) => emitTypeError (ctx, [span1, span2, span3], "cannot apply arithmetic operator on record type")
         | U.UnaryConstraint (span1, U.RecordType (span2, _), U.IsField span3) => emitTypeError (ctx, [span1, span2, span3], "cannot apply arithmetic operator on record type")
         | U.UnaryConstraint (span1, U.RecordType (span2, _), U.IsSigned span3) => emitTypeError (ctx, [span1, span2, span3], "cannot apply arithmetic operator on record type")
         | U.UnaryConstraint (span1, U.RecordType (span2, _), U.IsOrdered span3) => emitTypeError (ctx, [span1, span2, span3], "cannot compare records")
         | U.UnaryConstraint (span1, U.RecordType (span2, _), U.IsInt span3) => emitTypeError (ctx, [span1, span2, span3], "cannot unify a record with an int")
         | U.UnaryConstraint (span1, U.RecordType (span2, _), U.IsWord span3) => emitTypeError (ctx, [span1, span2, span3], "cannot unify a record with a word")
         | U.UnaryConstraint (span1, U.RecordType (span2, _), U.IsReal span3) => emitTypeError (ctx, [span1, span2, span3], "cannot unify a record with a real")
         | U.UnaryConstraint (span1, U.RecordType (span2, _), U.IsChar span3) => emitTypeError (ctx, [span1, span2, span3], "cannot unify a record with a char")
         | U.UnaryConstraint (span1, U.RecordType (span2, _), U.IsString span3) => emitTypeError (ctx, [span1, span2, span3], "cannot unify a record with a string")
         | U.UnaryConstraint (span1, U.FnType (span2, _, _), U.IsEqType span3) => emitTypeError (ctx, [span1, span2, span3], "function type does not admit equality")
         | U.UnaryConstraint (span1, U.FnType (span2, _, _), U.IsIntegral span3) => emitTypeError (ctx, [span1, span2, span3], "cannot apply arithmetic operator on function type")
         | U.UnaryConstraint (span1, U.FnType (span2, _, _), U.IsSignedReal span3) => emitTypeError (ctx, [span1, span2, span3], "cannot apply arithmetic operator on function type")
         | U.UnaryConstraint (span1, U.FnType (span2, _, _), U.IsRing span3) => emitTypeError (ctx, [span1, span2, span3], "cannot apply arithmetic operator on function type")
         | U.UnaryConstraint (span1, U.FnType (span2, _, _), U.IsField span3) => emitTypeError (ctx, [span1, span2, span3], "cannot apply arithmetic operator on function type")
         | U.UnaryConstraint (span1, U.FnType (span2, _, _), U.IsSigned span3) => emitTypeError (ctx, [span1, span2, span3], "cannot apply arithmetic operator on function type")
         | U.UnaryConstraint (span1, U.FnType (span2, _, _), U.IsOrdered span3) => emitTypeError (ctx, [span1, span2, span3], "cannot compare functions")
         | U.UnaryConstraint (span1, U.FnType (span2, _, _), U.IsInt span3) => emitTypeError (ctx, [span1, span2, span3], "cannot unify a function with an int")
         | U.UnaryConstraint (span1, U.FnType (span2, _, _), U.IsWord span3) => emitTypeError (ctx, [span1, span2, span3], "cannot unify a function with a word")
         | U.UnaryConstraint (span1, U.FnType (span2, _, _), U.IsReal span3) => emitTypeError (ctx, [span1, span2, span3], "cannot unify a function with a real")
         | U.UnaryConstraint (span1, U.FnType (span2, _, _), U.IsChar span3) => emitTypeError (ctx, [span1, span2, span3], "cannot unify a function with a char")
         | U.UnaryConstraint (span1, U.FnType (span2, _, _), U.IsString span3) => emitTypeError (ctx, [span1, span2, span3], "cannot unify a function with a string")
         | U.UnaryConstraint(span1, U.TyCon(span2, tyargs, tyname), U.IsEqType span3) =>
           let val { admitsEquality, ... } = lookupTyNameInEnv (#context ctx, env, span2, tyname)
           in if isRefOrArray tyname then
                  unify(ctx, env, ctrs)
              else if admitsEquality then
                  unify(ctx, env, List.map (fn tyarg => U.UnaryConstraint(span1, tyarg, U.IsEqType span3)) tyargs @ ctrs)
              else
                  emitTypeError (ctx, [span1, span2, span3], USyntax.PrettyPrint.print_TyName tyname ^ " does not admit equality")
           end
         | U.UnaryConstraint(span1, U.TyCon(span2, tyargs, tyname), U.IsIntegral span3) =>
           let val { overloadClass, ... } = lookupTyNameInEnv (#context ctx, env, span2, tyname)
               val isIntegral = case overloadClass of
                                    SOME Syntax.CLASS_INT => true
                                  | SOME Syntax.CLASS_WORD => true
                                  | _ => false
           in if isIntegral then
                  unify(ctx, env, ctrs) (* do nothing *)
              else
                  emitTypeError (ctx, [span1, span2, span3], "arithmetic operator on unsupported type")
           end
         | U.UnaryConstraint(span1, U.TyCon(span2, tyargs, tyname), U.IsSignedReal span3) =>
           let val { overloadClass, ... } = lookupTyNameInEnv (#context ctx, env, span2, tyname)
               val isSignedReal = case overloadClass of
                                      SOME Syntax.CLASS_INT => true
                                    | SOME Syntax.CLASS_REAL => true
                                    | _ => false
           in if isSignedReal then
                  unify(ctx, env, ctrs) (* do nothing *)
              else
                  emitTypeError (ctx, [span1, span2, span3], "arithmetic operator on unsupported type")
           end
         | U.UnaryConstraint(span1, U.TyCon(span2, tyargs, tyname), U.IsRing span3) =>
           let val { overloadClass, ... } = lookupTyNameInEnv (#context ctx, env, span2, tyname)
               val isRing = case overloadClass of
                                SOME Syntax.CLASS_INT => true
                              | SOME Syntax.CLASS_WORD => true
                              | SOME Syntax.CLASS_REAL => true
                              | _ => false
           in if isRing then
                  unify(ctx, env, ctrs) (* do nothing *)
              else
                  emitTypeError (ctx, [span1, span2, span3], "arithmetic operator on unsupported type")
           end
         | U.UnaryConstraint(span1, U.TyCon(span2, tyargs, tyname), U.IsField span3) =>
           let val { overloadClass, ... } = lookupTyNameInEnv (#context ctx, env, span2, tyname)
               val isField = case overloadClass of
                                 SOME Syntax.CLASS_REAL => true
                               | _ => false
           in if isField then
                  unify(ctx, env, ctrs) (* do nothing *)
              else
                  emitTypeError (ctx, [span1, span2, span3], "arithmetic operator on unsupported type")
           end
         | U.UnaryConstraint(span1, U.TyCon(span2, tyargs, tyname), U.IsSigned span3) =>
           let val { overloadClass, ... } = lookupTyNameInEnv (#context ctx, env, span2, tyname)
               val isSigned = case overloadClass of
                                  SOME Syntax.CLASS_INT => true
                                | SOME Syntax.CLASS_REAL => true
                                | _ => false
           in if isSigned then
                  unify(ctx, env, ctrs) (* do nothing *)
              else
                  emitTypeError (ctx, [span1, span2, span3], "arithmetic operator on unsupported type")
           end
         | U.UnaryConstraint(span1, U.TyCon(span2, tyargs, tyname), U.IsOrdered span3) =>
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
                  emitTypeError (ctx, [span1, span2, span3], "comparison operator on unsupported type")
           end
         | U.UnaryConstraint(span1, U.TyCon(span2, tyargs, tyname), U.IsInt span3) =>
           if USyntax.eqTyName(tyname, primTyName_int) then
               unify(ctx, env, ctrs) (* do nothing *)
           else
               let val { overloadClass, ... } = lookupTyNameInEnv (#context ctx, env, span2, tyname)
               in if overloadClass = SOME Syntax.CLASS_INT then
                      unify(ctx, env, ctrs) (* do nothing *)
                  else
                      emitTypeError (ctx, [span1, span2, span3], "invalid integer constant: " ^ USyntax.print_TyName tyname)
               end
         | U.UnaryConstraint(span1, U.TyCon(span2, tyargs, tyname), U.IsWord span3) =>
           if USyntax.eqTyName(tyname, primTyName_word) then
               unify(ctx, env, ctrs) (* do nothing *)
           else
               let val { overloadClass, ... } = lookupTyNameInEnv (#context ctx, env, span2, tyname)
               in if overloadClass = SOME Syntax.CLASS_WORD then
                      unify(ctx, env, ctrs) (* do nothing *)
                  else
                      emitTypeError (ctx, [span1, span2, span3], "invalid word constant")
               end
         | U.UnaryConstraint(span1, U.TyCon(span2, tyargs, tyname), U.IsReal span3) =>
           if USyntax.eqTyName(tyname, primTyName_real) then
               unify(ctx, env, ctrs) (* do nothing *)
           else
               let val { overloadClass, ... } = lookupTyNameInEnv (#context ctx, env, span2, tyname)
               in if overloadClass = SOME Syntax.CLASS_REAL then
                      unify(ctx, env, ctrs) (* do nothing *)
                  else
                      emitTypeError (ctx, [span1, span2, span3], "invalid real constant")
               end
         | U.UnaryConstraint(span1, U.TyCon(span2, tyargs, tyname), U.IsChar span3) =>
           if USyntax.eqTyName(tyname, primTyName_char) then
               unify(ctx, env, ctrs) (* do nothing *)
           else
               let val { overloadClass, ... } = lookupTyNameInEnv (#context ctx, env, span2, tyname)
               in if overloadClass = SOME Syntax.CLASS_CHAR then
                      unify(ctx, env, ctrs) (* do nothing *)
                  else
                      emitTypeError (ctx, [span1, span2, span3], "invalid character constant")
               end
         | U.UnaryConstraint(span1, U.TyCon(span2, tyargs, tyname), U.IsString span3) =>
           if USyntax.eqTyName(tyname, primTyName_string) then
               unify(ctx, env, ctrs) (* do nothing *)
           else
               let val { overloadClass, ... } = lookupTyNameInEnv (#context ctx, env, span2, tyname)
               in if overloadClass = SOME Syntax.CLASS_STRING then
                      unify(ctx, env, ctrs) (* do nothing *)
                  else
                      emitTypeError (ctx, [span1, span2, span3], "invalid string constant")
               end
         | U.UnaryConstraint(span1, U.TyVar(span2, tv), pred) =>
           (case USyntax.TyVarMap.find(!(#tyVarSubst ctx), tv) of
                SOME replacement => unify(ctx, env, U.UnaryConstraint(span1, replacement, pred) :: ctrs)
              | NONE => case (tv, pred) of
                            (U.NamedTyVar (name, eq, _), U.IsEqType span3) =>
                            if eq then
                                unify(ctx, env, ctrs)
                            else
                                emitTypeError (ctx, [span1, span2, span3], "the type variable " ^ name ^ " does not admit equality")
                          | (U.NamedTyVar (name, _, _), _) => emitTypeError (ctx, [span1, span2], "the use of " ^ name ^ " is non-free")
                          | _ => (addTyVarConstraint(ctx, tv, pred) ; unify(ctx, env, ctrs))
           )
      )
and unifyTyVarAndTy(ctx : InferenceContext, env : Env, span : SourcePos.span, tv : U.TyVar, ty : U.Ty, ctrs : U.Constraint list) : unit
    = let val subst = !(#tyVarSubst ctx)
      in case USyntax.TyVarMap.find(subst, tv) of
             SOME replacement => unify(ctx, env, U.EqConstr(span, replacement, ty) :: ctrs)
           | NONE =>
             let val ty = applySubstTy subst ty
             in if (case ty of U.TyVar(_, tv') => U.eqUTyVar(tv, tv') | _ => false) then (* ty = TyVar tv *)
                    unify(ctx, env, ctrs) (* do nothing *)
                else if occurCheck tv ty then
                    emitTypeError (ctx, [span, U.getSourceSpanOfTy ty], "unification failed: occurrence check (" ^ USyntax.print_TyVar tv ^ " in " ^ USyntax.print_Ty ty ^ ((case ty of U.TyVar(_, tv') => if U.eqUTyVar(tv, tv') then "eqtyvar" else ", not eqtyvar" | _ => ", not tyvar")) ^ ")")
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
fun addConstraint(ctx : InferenceContext, env : Env, ct : U.Constraint) = unify(ctx, env, [ct])

(* evalTy : Context * Env * S.Ty -> U.Ty *)
fun evalTy (ctx : Context, env : ('val,'str) Env', S.TyVar (span, tv)) : U.Ty
    = (case Syntax.TyVarMap.find(#boundTyVars env, tv) of
           SOME tv => U.TyVar(span, tv)
         | NONE => emitError(ctx, [span], "unknown type varibale `" ^ Syntax.print_TyVar tv ^ "`")
      )
  | evalTy (ctx, env, S.RecordType (span, fields, NONE)) = U.RecordType (span, List.foldl (fn ((label, ty), m) => Syntax.LabelMap.insert (m, label, evalTy (ctx, env, ty))) Syntax.LabelMap.empty fields)
  | evalTy (ctx, env, S.RecordType (span, fields, SOME baseTy))
    = (case evalTy (ctx, env, baseTy) of
           U.RecordType (_, fields') => U.RecordType (span, List.foldl (fn ((label, ty), m) =>
                                                                           if Syntax.LabelMap.inDomain (m, label) then
                                                                               emitError (ctx, [span], "duplicate record field: " ^ Syntax.print_Label label)
                                                                           else
                                                                               Syntax.LabelMap.insert (m, label, evalTy (ctx, env, ty))
                                                                       ) fields' fields)
         | _ => emitError (ctx, [span], "invalid record extension")
      )
  | evalTy (ctx, env, S.TyCon (span, args, tycon))
    = let val { typeFunction = U.TypeFunction (tyvars, ty), ... } = lookupTyConInEnv (ctx, env, span, tycon)
          val subst = (ListPair.foldlEq (fn (tv, arg, m) => USyntax.TyVarMap.insert (m, tv, evalTy (ctx, env, arg))) USyntax.TyVarMap.empty (tyvars, args))
                      handle ListPair.UnequalLengths => emitError(ctx, [span], "invalid type construction")
      in U.applySubstTy subst ty
      end
  | evalTy (ctx, env, S.FnType (span, ty1, ty2)) = U.FnType (span, evalTy (ctx, env, ty1), evalTy (ctx, env, ty2))

(* typeCheckPat : InferenceContext * Env * S.Pat * (* type hint *) U.Ty option -> U.Ty * (U.VId * U.Ty) S.VIdMap.map * U.Pat *)
fun typeCheckPat (ctx : InferenceContext, env : Env, S.WildcardPat span, typeHint : U.Ty option) : U.Ty * (U.VId * U.Ty) S.VIdMap.map * U.Pat
    = (case typeHint of
           NONE => let val ty = U.TyVar (span, freshTyVar (#context ctx))
                   in (ty, S.VIdMap.empty, U.WildcardPat span)
                   end
         | SOME expectedTy => (expectedTy, S.VIdMap.empty, U.WildcardPat span)
      )
  | typeCheckPat (ctx, env, S.SConPat (span, scon), typeHint)
    = (case scon of
           Syntax.IntegerConstant _   => (case typeHint of
                                              NONE => let val tv = freshTyVar (#context ctx)
                                                          val ty = U.TyVar (span, tv)
                                                      in addTyVarConstraint (ctx, tv, U.IsInt span)
                                                       ; addTyVarConstraint (ctx, tv, U.IsEqType span)
                                                       ; (ty, S.VIdMap.empty, U.SConPat (span, scon, ty))
                                                      end
                                            | SOME expectedTy => ( unify (ctx, env, [U.UnaryConstraint (span, expectedTy, U.IsInt span), U.UnaryConstraint (span, expectedTy, U.IsEqType span)])
                                                                 ; (expectedTy, S.VIdMap.empty, U.SConPat (span, scon, expectedTy))
                                                                 )
                                         )
         | Syntax.WordConstant _      => (case typeHint of
                                              NONE => let val tv = freshTyVar (#context ctx)
                                                          val ty = U.TyVar (span, tv)
                                                      in addTyVarConstraint (ctx, tv, U.IsWord span)
                                                       ; addTyVarConstraint (ctx, tv, U.IsEqType span)
                                                       ; (ty, S.VIdMap.empty, U.SConPat (span, scon, ty))
                                                      end
                                            | SOME expectedTy => ( unify (ctx, env, [U.UnaryConstraint (span, expectedTy, U.IsWord span), U.UnaryConstraint (span, expectedTy, U.IsEqType span)])
                                                                 ; (expectedTy, S.VIdMap.empty, U.SConPat (span, scon, expectedTy))
                                                                 )
                                         )
         | Syntax.RealConstant _      => emitTypeError (ctx, [span], "no real constant may occur in a pattern")
         | Syntax.CharacterConstant _ => (case typeHint of
                                              NONE => let val tv = freshTyVar (#context ctx)
                                                          val ty = U.TyVar (span, tv)
                                                      in addTyVarConstraint (ctx, tv, U.IsChar span)
                                                       ; addTyVarConstraint (ctx, tv, U.IsEqType span)
                                                       ; (ty, S.VIdMap.empty, U.SConPat (span, scon, ty))
                                                      end
                                            | SOME expectedTy => ( unify (ctx, env, [U.UnaryConstraint (span, expectedTy, U.IsChar span), U.UnaryConstraint (span, expectedTy, U.IsEqType span)])
                                                                 ; (expectedTy, S.VIdMap.empty, U.SConPat (span, scon, expectedTy))
                                                                 )
                                         )
         | Syntax.StringConstant _    => (case typeHint of
                                              NONE => let val tv = freshTyVar (#context ctx)
                                                          val ty = U.TyVar (span, tv)
                                                      in addTyVarConstraint (ctx, tv, U.IsString span)
                                                       ; addTyVarConstraint (ctx, tv, U.IsEqType span)
                                                       ; (ty, S.VIdMap.empty, U.SConPat (span, scon, ty))
                                                      end
                                            | SOME expectedTy => ( unify (ctx, env, [U.UnaryConstraint (span, expectedTy, U.IsString span), U.UnaryConstraint (span, expectedTy, U.IsEqType span)])
                                                                 ; (expectedTy, S.VIdMap.empty, U.SConPat (span, scon, expectedTy))
                                                                 )
                                         )
      )
  | typeCheckPat (ctx, env, S.VarPat (span, vid), typeHint)
    = (case Syntax.VIdMap.find(#valMap env, vid) of
           SOME (_, Syntax.ValueConstructor _, _) => emitTypeError (ctx, [span], "VarPat: invalid pattern")
         | SOME (_, Syntax.ExceptionConstructor, _) => emitTypeError (ctx, [span], "VarPat: invalid pattern")
         | _ => (case typeHint of
                     NONE => let val ty = USyntax.TyVar (span, freshTyVar (#context ctx))
                                 val vid' = newVId (#context ctx, vid)
                             in (ty, S.VIdMap.singleton (vid, (vid', ty)), U.VarPat (span, vid', ty))
                             end
                   | SOME expectedTy => let val vid' = newVId (#context ctx, vid)
                                        in (expectedTy, S.VIdMap.singleton (vid, (vid', expectedTy)), U.VarPat (span, vid', expectedTy))
                                        end
                )
      )
  | typeCheckPat (ctx, env, S.RecordPat { sourceSpan, fields, ellipsis }, typeHint)
    = let fun oneField((label, pat), (fieldTypes, vars, fieldPats))
              = let val typeHint' = case typeHint of
                                        SOME (U.RecordType (_, m)) => Syntax.LabelMap.find (m, label)
                                      | _ => NONE
                    val (ty, vars', pat') = typeCheckPat (ctx, env, pat, typeHint')
                in ((label, ty) :: fieldTypes, Syntax.VIdMap.unionWith (fn _ => emitTypeError (ctx, [sourceSpan], "duplicate identifier in a pattern")) (vars, vars'), (label, pat') :: fieldPats)
                end
          val (fieldTypes, vars, fieldPats) = List.foldr oneField ([], Syntax.VIdMap.empty, []) fields
      in case ellipsis of
             SOME (S.WildcardPat wspan) =>
             let val recordTy = case typeHint of
                                    NONE => U.TyVar (sourceSpan, freshTyVar (#context ctx))
                                  | SOME expectedTy => expectedTy
                 fun oneField(label, ty) = addConstraint(ctx, env, U.UnaryConstraint(sourceSpan, recordTy, U.HasField { sourceSpan = sourceSpan, label = label, fieldTy = ty }))
             in List.app oneField fieldTypes
              ; (recordTy, vars, U.RecordPat{sourceSpan=sourceSpan, fields=fieldPats, ellipsis=SOME (U.WildcardPat wspan)})
             end
           | SOME basePat =>
             let val recordTy = case typeHint of
                                    NONE => U.TyVar (sourceSpan, freshTyVar (#context ctx))
                                  | SOME expectedTy => expectedTy
                 val (baseTy, vars', basePat) = typeCheckPat (ctx, env, basePat, NONE)
             in addConstraint(ctx, env, U.UnaryConstraint(sourceSpan, recordTy, U.RecordExt { sourceSpan = sourceSpan, fields = fieldTypes, baseTy = baseTy }))
              ; addConstraint(ctx, env, U.UnaryConstraint(sourceSpan, baseTy, U.SubrecordOf { sourceSpan = sourceSpan, extraFields = fieldTypes, extendedTy = recordTy }))
              ; (recordTy, Syntax.VIdMap.unionWith (fn _ => emitTypeError (ctx, [sourceSpan], "duplicate identifier in a pattern")) (vars, vars'), U.RecordPat { sourceSpan = sourceSpan, fields = fieldPats, ellipsis = SOME basePat })
             end
           | NONE => (U.RecordType(sourceSpan, Syntax.LabelMapFromList fieldTypes), vars, U.RecordPat{sourceSpan=sourceSpan, fields=fieldPats, ellipsis=NONE})
      end
  | typeCheckPat (ctx, env, S.ConPat (span, longvid, optInnerPat), typeHint)
    = (case lookupLongVIdInEnv(ctx, env, span, longvid) of
           Found (longvid, tysc, idstatus) =>
           (if (case idstatus of Syntax.ValueConstructor _ => true | Syntax.ExceptionConstructor => true | _ => false) then
                let val (ty, tyargs) = instantiate(ctx, span, tysc)
                    val isSoleConstructor = case idstatus of
                                                Syntax.ValueConstructor sole => sole
                                              | _ => false
                in case optInnerPat of
                       NONE => (ty, Syntax.VIdMap.empty, U.ConPat { sourceSpan = span, longvid = longvid, payload = NONE, tyargs = List.map #1 tyargs, isSoleConstructor = isSoleConstructor })
                     | SOME innerPat =>
                       (case ty of
                            U.FnType(span', argTy, resultTy) =>
                            let val (argTy', innerVars, innerPat') = typeCheckPat (ctx, env, innerPat, SOME argTy)
                            in addConstraint(ctx, env, U.EqConstr(span, argTy, argTy'))
                             ; (resultTy, innerVars, U.ConPat { sourceSpan = span, longvid = longvid, payload = SOME innerPat', tyargs = List.map #1 tyargs, isSoleConstructor = isSoleConstructor })
                            end
                          | _ => emitTypeError (ctx, [span], "invalid pattern")
                       )
                end
            else (* idstatus = Syntax.ValueVariable *)
                emitTypeError (ctx, [span], "invalid pattern")
           )
         | ValueNotFound notfound => emitTypeError (ctx, [span], "invalid pattern: value name '" ^ Syntax.print_LongVId notfound ^ "' nout found")
         | StructureNotFound notfound => emitTypeError (ctx, [span], "invalid pattern: structure name '" ^ Syntax.print_LongStrId notfound ^ "' not found")
      )
  | typeCheckPat (ctx, env, S.TypedPat (span, pat, ty), typeHint)
    = let val ty = evalTy(#context ctx, env, ty)
          val (inferredTy, vars, pat) = typeCheckPat (ctx, env, pat, SOME ty)
      in addConstraint(ctx, env, U.EqConstr(span, ty, inferredTy))
       ; (ty, vars, U.TypedPat(span, pat, ty))
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
                                      SOME ty => ( addConstraint (ctx, env, U.EqConstr (span, ty, inferredTy))
                                                 ; ty
                                                 )
                                    | NONE => inferredTy
                         val vid' = newVId (#context ctx, vid)
                     in (ty, Syntax.VIdMap.insert(vars, vid, (vid', ty)), U.LayeredPat(span, vid', ty, pat))
                     end
           | SOME _ => emitTypeError (ctx, [span], "duplicate identifier in a pattern")
      end
  | typeCheckPat (ctx, env, S.VectorPat (span, pats, ellipsis), typeHint)
    = let val elemTy = case typeHint of
                           SOME (U.TyCon (_, [elemTy], _)) => elemTy
                         | _ => USyntax.TyVar (span, freshTyVar (#context ctx))
          val pats = Vector.map (fn pat => let val (elemTy', vars, pat) = typeCheckPat (ctx, env, pat, SOME elemTy)
                                           in addConstraint(ctx, env, U.EqConstr(span, elemTy, elemTy'))
                                            ; (vars, pat)
                                           end
                                ) pats
          val vars = Vector.foldr (fn ((vars, _), vars') => Syntax.VIdMap.unionWith (fn _ => emitTypeError (ctx, [], "duplicate identifier in a pattern")) (vars, vars')) Syntax.VIdMap.empty pats
          val pats = Vector.map #2 pats
      in (U.TyCon(span, [elemTy], primTyName_vector), vars, U.VectorPat(span, pats, ellipsis, elemTy))
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
                               | NONE => let val { typeFunction = U.TypeFunction(tyvars, ty), ... } = lookupTyConInEnv(ctx, env, span, longtycon)
                                             val tyVarMap = ListPair.foldlEq (fn (tv, ty, m) => U.TyVarMap.insert(m, tv, ty)) U.TyVarMap.empty (tyvars, tyargs)
                                             fun doUTy (U.TyVar (span, tv)) = (case U.TyVarMap.find(tyVarMap, tv) of
                                                                                   SOME ty => doTy ty
                                                                                 | NONE => case tv of
                                                                                               U.NamedTyVar(_, eq, _) => if eq then
                                                                                                                             SOME []
                                                                                                                         else
                                                                                                                             NONE
                                                                                             | _ => NONE (* error *)
                                                                              )
                                               | doUTy (U.RecordType (span, fields)) = doUTypes (Syntax.LabelMap.foldl (op ::) [] fields)
                                               | doUTy (U.TyCon (span, tyargs, tyname)) = if isRefOrArray tyname then
                                                                                              SOME []
                                                                                          else
                                                                                              let val { admitsEquality, ... } = lookupTyNameInEnv(ctx, env, span, tyname)
                                                                                              in if admitsEquality then
                                                                                                     doUTypes tyargs
                                                                                                 else
                                                                                                     NONE
                                                                                              end
                                               | doUTy (U.FnType _) = NONE
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

(* typeCheckExp : InferenceContext * Env * S.Exp * (* type hint *) U.Ty option -> U.Ty * U.Exp *)
fun typeCheckExp (ctx : InferenceContext, env : Env, S.SConExp (span, scon), typeHint : U.Ty option) : U.Ty * U.Exp
    = let val ty = case scon of
                       Syntax.IntegerConstant x   => (case typeHint of
                                                          NONE => let val tv = freshTyVar (#context ctx)
                                                                      val ty = U.TyVar (span, tv)
                                                                  in addTyVarConstraint (ctx, tv, U.IsInt span)
                                                                   ; ty
                                                                  end
                                                        | SOME expectedTy => ( addConstraint (ctx, env, U.UnaryConstraint (span, expectedTy, U.IsInt span))
                                                                             ; expectedTy
                                                                             )
                                                     )
                     | Syntax.WordConstant x      => (case typeHint of
                                                          NONE => let val tv = freshTyVar (#context ctx)
                                                                      val ty = U.TyVar (span, tv)
                                                                  in addTyVarConstraint (ctx, tv, U.IsWord span)
                                                                   ; ty
                                                                  end
                                                        | SOME expectedTy => ( addConstraint (ctx, env, U.UnaryConstraint (span, expectedTy, U.IsWord span))
                                                                             ; expectedTy
                                                                             )
                                                     )
                     | Syntax.RealConstant x      => primTy_real (* TODO: overloaded literals *)
                     | Syntax.CharacterConstant x => (case typeHint of
                                                          NONE => let val tv = freshTyVar (#context ctx)
                                                                      val ty = U.TyVar (span, tv)
                                                                  in addTyVarConstraint (ctx, tv, U.IsChar span)
                                                                   ; ty
                                                                  end
                                                        | SOME expectedTy => ( addConstraint (ctx, env, U.UnaryConstraint (span, expectedTy, U.IsChar span))
                                                                             ; expectedTy
                                                                             )
                                                     )
                     | Syntax.StringConstant x    => (case typeHint of
                                                          NONE => let val tv = freshTyVar (#context ctx)
                                                                      val ty = U.TyVar (span, tv)
                                                                  in addTyVarConstraint (ctx, tv, U.IsString span)
                                                                   ; ty
                                                                  end
                                                        | SOME expectedTy => ( addConstraint (ctx, env, U.UnaryConstraint (span, expectedTy, U.IsString span))
                                                                             ; expectedTy
                                                                             )
                                                     )
      in (ty, U.SConExp(span, scon, ty))
      end
  | typeCheckExp (ctx, env, exp as S.VarExp (span, longvid), typeHint)
    = (case lookupLongVIdInEnv(ctx, env, span, longvid) of
           Found (longvid, tysc, ids) => let val (ty, tyargs) = instantiate(ctx, span, tysc)
                                         in (ty, U.VarExp (span, longvid, ids, tyargs))
                                         end
         | ValueNotFound notfound => emitTypeError (ctx, [span], "unknown value name " ^ Syntax.print_LongVId notfound)
         | StructureNotFound notfound => emitTypeError (ctx, [span], "unknown structure name " ^ Syntax.print_LongStrId notfound)
      )
  | typeCheckExp (ctx, env, S.RecordExp (span, fields, NONE), typeHint)
    = let fun oneField (label, exp) = let val typeHint' = case typeHint of
                                                              SOME (U.RecordType (_, m)) => Syntax.LabelMap.find (m, label)
                                                            | _ => NONE
                                          val (ty, exp) = typeCheckExp (ctx, env, exp, typeHint')
                                      in ((label, ty), (label, exp))
                                      end
          val (fieldTypes, fields) = ListPair.unzip (List.map oneField fields)
      in (U.RecordType(span, Syntax.LabelMapFromList fieldTypes), U.RecordExp(span, fields))
      end
  | typeCheckExp (ctx, env, S.RecordExp (span, fields, SOME baseExp), typeHint)
    = let val (baseTy, baseExp) = typeCheckExp (ctx, env, baseExp, NONE)
          val recordTy = U.TyVar (span, freshTyVar (#context ctx))
          fun oneField (label, exp) = let val typeHint' = case typeHint of
                                                              SOME (U.RecordType (_, m)) => Syntax.LabelMap.find (m, label)
                                                            | _ => NONE
                                          val (ty, exp) = typeCheckExp (ctx, env, exp, typeHint')
                                      in ((label, ty), (label, exp))
                                      end
          val (fieldTypes, fields) = ListPair.unzip (List.map oneField fields)
      in addConstraint (ctx, env, U.UnaryConstraint (span, recordTy, U.RecordExt { sourceSpan = span, fields = fieldTypes, baseTy = baseTy }))
       ; addConstraint (ctx, env, U.UnaryConstraint (span, baseTy, U.SubrecordOf { sourceSpan = span, extraFields = fieldTypes, extendedTy = recordTy }))
       ; (recordTy, U.RecordExtExp { sourceSpan = span, fields = fields, baseExp = baseExp, baseTy = baseTy })
      end
  | typeCheckExp (ctx, env, S.LetInExp (span, decs, innerExp), typeHint)
    = let val (env', decs) = typeCheckDecs(ctx, env, decs)
          val (ty, innerExp) = typeCheckExp (ctx, mergeEnv (env, env'), innerExp, typeHint)
      in (ty, U.LetInExp(span, decs, innerExp))
      end
  | typeCheckExp (ctx, env, S.AppExp (span, f, x), typeHint)
          (* f: s -> t, x: s *)
    = let val (funTy, f) = typeCheckExp (ctx, env, f, NONE)
          val argTyHint = case funTy of
                              U.FnType (_, argTy, _) => SOME argTy
                            | _ => NONE
          val (argTy, x) = typeCheckExp (ctx, env, x, argTyHint)
          val retTy = case funTy of
                          U.FnType(_, argTy', retTy) => ( addConstraint(ctx, env, U.EqConstr(span, argTy, argTy')); retTy )
                        | _ => let val retTy = case typeHint of
                                                   SOME expectedTy => expectedTy
                                                 | _ => U.TyVar (span, freshTyVar (#context ctx))
                               in addConstraint(ctx, env, U.EqConstr(span, funTy, U.FnType(span, argTy, retTy))) (* funTy = (argTy -> retTy) *)
                                ; retTy
                               end
      in (retTy, U.AppExp(span, f, x))
      end
  | typeCheckExp (ctx, env, S.TypedExp (span, exp, ty), typeHint)
    = let val ty = evalTy (#context ctx, env, ty)
          val (expTy, exp) = typeCheckExp (ctx, env, exp, SOME ty)
      in addConstraint(ctx, env, U.EqConstr(span, expTy, ty))
       ; (ty, U.TypedExp(span, exp, ty))
      end
  | typeCheckExp (ctx, env, S.HandleExp (span, exp, matches), typeHint)
          (* exp: t, matches: exn -> t *)
    = let val (expTy, exp) = typeCheckExp (ctx, env, exp, typeHint)
          val (patTy, retTy, matches) = typeCheckMatch (ctx, env, span, matches, SOME primTy_exn, SOME expTy)
      in addConstraint(ctx, env, U.EqConstr(span, patTy, primTy_exn)) (* patTy = exn *)
       ; addConstraint(ctx, env, U.EqConstr(span, expTy, retTy))
       ; (expTy, U.HandleExp(span, exp, matches))
      end
  | typeCheckExp (ctx, env, S.RaiseExp (span, exp), typeHint)
    = let val (expTy, exp) = typeCheckExp (ctx, env, exp, SOME primTy_exn)
          val resultTy = case typeHint of
                             SOME expectedTy => expectedTy
                           | NONE => U.TyVar (span, freshTyVar (#context ctx))
      in addConstraint(ctx, env, U.EqConstr(span, expTy, primTy_exn)) (* expTy = exn *)
       ; (resultTy, U.RaiseExp(span, resultTy, exp))
      end
  | typeCheckExp (ctx, env, S.IfThenElseExp (span, cond, thenPart, elsePart), typeHint)
    = let val (condTy, cond) = typeCheckExp (ctx, env, cond, SOME primTy_bool)
          val (thenTy, thenPart) = typeCheckExp (ctx, env, thenPart, typeHint)
          val (elseTy, elsePart) = typeCheckExp (ctx, env, elsePart, typeHint)
      in addConstraint(ctx, env, U.EqConstr(span, condTy, primTy_bool)) (* condTy = bool *)
       ; addConstraint(ctx, env, U.EqConstr(span, thenTy, elseTy)) (* thenTy = elseTy *)
       ; (thenTy, U.IfThenElseExp(span, cond, thenPart, elsePart))
      end
  | typeCheckExp (ctx, env, S.CaseExp (span, exp, matches), typeHint)
    = let val (expTy, exp) = typeCheckExp (ctx, env, exp, NONE)
          val (patTy, retTy, matches) = typeCheckMatch (ctx, env, span, matches, SOME expTy, typeHint)
      in addConstraint(ctx, env, U.EqConstr(span, expTy, patTy))
       ; (retTy, U.CaseExp(span, exp, expTy, matches))
      end
  | typeCheckExp (ctx, env, S.FnExp (span, matches), typeHint)
    = let val (argTyHint, retTyHint) = case typeHint of
                                           SOME (U.FnType (_, argTy, retTy)) => (SOME argTy, SOME retTy)
                                         | _ => (NONE, NONE)
          val (argTy, retTy, matches) = typeCheckMatch (ctx, env, span, matches, argTyHint, retTyHint)
          val fnExp = case matches of
                          [(U.VarPat(span2, vid, _), body)] => U.FnExp(span, vid, argTy, body)
                        | _ => let val vid = newVId (#context ctx, Syntax.MkVId "a")
                               in U.FnExp(span, vid, argTy, U.CaseExp(span, U.VarExp(span, U.MkShortVId(vid), Syntax.ValueVariable, []), argTy, matches))
                               end
      in (U.FnType(span, argTy, retTy), fnExp)
      end
  | typeCheckExp (ctx, env, S.ProjectionExp (span, label), typeHint)
    = let val (recordTy, fieldTy) = case typeHint of
                                        SOME (U.FnType (_, argTy, retTy)) => (argTy, retTy)
                                      | _ => (USyntax.TyVar (span, freshTyVar (#context ctx)), USyntax.TyVar (span, freshTyVar (#context ctx)))
      in addConstraint(ctx, env, U.UnaryConstraint(span, recordTy, U.HasField { sourceSpan = span, label = label, fieldTy = fieldTy }))
       ; (U.FnType(span, recordTy, fieldTy), U.ProjectionExp { sourceSpan = span, label = label, recordTy = recordTy, fieldTy = fieldTy })
      end
  | typeCheckExp (ctx, env, S.ListExp (span, xs), typeHint)
    = let val elemTy = case typeHint of
                           SOME (U.TyCon (_, [elemTy], _)) => elemTy
                         | _ => USyntax.TyVar (span, freshTyVar (#context ctx))
          val xs = Vector.map (fn exp => let val (expTy, exp) = typeCheckExp (ctx, env, exp, SOME elemTy)
                                         in addConstraint(ctx, env, U.EqConstr(span, expTy, elemTy))
                                          ; exp
                                         end) xs
      in (U.TyCon(span, [elemTy], primTyName_list), U.ListExp(span, xs, elemTy))
      end
  | typeCheckExp (ctx, env, S.VectorExp (span, xs), typeHint)
    = let val elemTy = case typeHint of
                           SOME (U.TyCon (_, [elemTy], _)) => elemTy
                         | _ => USyntax.TyVar (span, freshTyVar (#context ctx))
          val xs = Vector.map (fn exp => let val (expTy, exp) = typeCheckExp (ctx, env, exp, SOME elemTy)
                                         in addConstraint(ctx, env, U.EqConstr(span, expTy, elemTy))
                                          ; exp
                                         end) xs
      in (U.TyCon(span, [elemTy], primTyName_vector), U.VectorExp(span, xs, elemTy))
      end
  | typeCheckExp (ctx, env, S.PrimExp (span, primOp, tyargs, args), typeHint)
    = let val () = if Vector.length tyargs = 0 then
                       ()
                   else
                       emitTypeError (ctx, [span], "type arguments to _primCall is not supported currently")
          val { typeVariables, argTypes, resultType } = typeOfPrimCall primOp
          val () = if Vector.length args = Vector.length argTypes then
                       ()
                   else
                       emitTypeError (ctx, [span], "wrong number of arguments to _primCall; expected " ^ Int.toString (Vector.length argTypes) ^ ", but got " ^ Int.toString (Vector.length args))
          val (subst, tyargs) = List.foldl (fn ((v, preds), (m, rest)) =>
                                               let val tv = freshTyVar (#context ctx)
                                                   val tyarg = U.TyVar (span, tv)
                                               in List.app (fn pred => addTyVarConstraint (ctx, tv, pred)) preds
                                                ; (U.TyVarMap.insert (m, v, tyarg), (tyarg, preds) :: rest)
                                               end
                                           ) (U.TyVarMap.empty, []) typeVariables
          val argTypes = Vector.map (applySubstTy subst) argTypes
          val resultType = applySubstTy subst resultType
          val args = Vector.mapi (fn (i, argTy) =>
                                     let val (argTy', arg) = typeCheckExp (ctx, env, Vector.sub (args, i), NONE)
                                     in addConstraint (ctx, env, U.EqConstr (span, argTy, argTy'))
                                      ; arg
                                     end
                                 ) argTypes
      in (resultType, U.PrimExp (span, primOp, argTypes, args))
      end
(* typeCheckDec : InferenceContext * Env * S.Dec -> (* created environment *) Env * U.Dec list *)
and typeCheckDec (ctx : InferenceContext, env : Env, S.ValDec (span, tyvarseq, valbinds))
    = let val valbinds = let val env = { valMap = #valMap env
                                       , tyConMap = #tyConMap env
                                       , tyNameMap = #tyNameMap env
                                       , strMap = #strMap env
                                       , sigMap = #sigMap env
                                       , funMap = #funMap env
                                       , boundTyVars = List.foldl (fn (tv, m) => Syntax.TyVarMap.insert (m, tv, genTyVar (#context ctx, tv))) (#boundTyVars env) tyvarseq
                                       }
                         in List.map (fn S.PatBind(span, pat, exp) =>
                                         let val (expTy, exp) = typeCheckExp (ctx, env, exp, NONE)
                                             val (patTy, newValEnv, pat) = typeCheckPat (ctx, env, pat, SOME expTy)
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
                                                                                     val vid' = renewVId (#context ctx) vid
                                                                                     val pat' = U.renameVarsInPat (U.VIdMap.insert(U.VIdMap.empty, vid, vid')) pat
                                                                                 in U.CaseExp(espan, exp, expTy, if isExhaustive(ctx, env, pat) then
                                                                                                                     [(pat', U.VarExp(espan, U.MkShortVId(vid'), Syntax.ValueVariable, []))]
                                                                                                                 else
                                                                                                                     [(pat', U.VarExp(espan, U.MkShortVId(vid'), Syntax.ValueVariable, []))
                                                                                                                     ,(U.WildcardPat span, U.RaiseExp(span, ty, U.VarExp(span, LongVId_Bind, Syntax.ExceptionConstructor, [])))
                                                                                                                     ]
                                                                                             )
                                                                                 end
                                                                      )
                                      in (valbind' :: valbinds, S.VIdMap.unionWith (fn _ => emitTypeError (ctx, [span], "duplicate identifier in a binding")) (S.VIdMap.map (fn (vid, ty) => (vid, U.TypeScheme ([], ty))) valEnv, valEnvRest))
                                      end
                     | _ => let val espan = U.getSourceSpanOfExp exp
                                val vars' = List.map (fn (vid, _) => (vid, renewVId (#context ctx) vid)) vars
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
                                                                           ,(U.WildcardPat span, U.RaiseExp(span, U.TupleType(span, List.map #2 vars), U.VarExp(span, LongVId_Bind, Syntax.ExceptionConstructor, [])))
                                                                           ]
                                                                     )
                                                          )
                            in (valbind' :: valbinds, S.VIdMap.unionWith (fn _ => emitTypeError (ctx, [span], "duplicate identifier in a binding")) (S.VIdMap.map (fn (vid, ty) => (vid, U.TypeScheme([], ty))) valEnv, valEnvRest))
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
                      | polyPart ((vid, tysc) :: rest) = let val vid' = renewVId (#context ctx) vid
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
                                              [(vid, ty)] => let val vid' = renewVId (#context ctx) vid
                                                                 val pat' = USyntax.renameVarsInPat (USyntax.VIdMap.insert(USyntax.VIdMap.empty, vid, vid')) (USyntax.filterVarsInPat isMonoVar pat)
                                                             in U.PolyVarBind(span, vid, U.TypeScheme([], ty), U.CaseExp(espan, exp, expTy, [(pat', U.VarExp(espan, USyntax.MkShortVId(vid'), Syntax.ValueVariable, []))])) :: polyPart valEnv'L
                                                             end
                                            | _ => let val vars' = List.map (fn (vid, _) => (vid, renewVId (#context ctx) vid)) xs
                                                       val varsMap = List.foldl USyntax.VIdMap.insert' USyntax.VIdMap.empty vars'
                                                       val pat' = USyntax.renameVarsInPat varsMap (USyntax.filterVarsInPat isMonoVar pat)
                                                       val tup = U.TupleExp(espan, List.map (fn (_, vid') => U.VarExp(espan, USyntax.MkShortVId(vid'), Syntax.ValueVariable, [])) vars')
                                                   in U.TupleBind(span, xs, U.CaseExp(espan, exp, expTy, [(pat', tup)])) :: polyPart valEnv'L
                                                   end
                                       end
                in (valbind' @ valbinds, Syntax.VIdMap.unionWith (fn _ => emitTypeError (ctx, [span], "duplicate identifier in a binding")) (Syntax.VIdMap.map (fn (vid, tysc) => (vid, tysc)) valEnv', valEnvRest))
                end
          val (valbinds, valEnv) = List.foldr generalize ([], Syntax.VIdMap.empty) valbinds
          val env' = envWithValEnv (Syntax.VIdMap.map (fn (vid, tysc) => (tysc, Syntax.ValueVariable, U.MkShortVId vid)) valEnv)
      in (env', [U.ValDec(span, valbinds)])
      end
  | typeCheckDec(ctx, env, S.RecValDec(span, tyvarseq, valbinds))
    = let val valbinds' : (SourcePos.span * (U.Ty * (U.VId * U.Ty) S.VIdMap.map * U.Pat) * S.Exp) list
              = List.map (fn S.PatBind (span, pat, exp) => (span, typeCheckPat (ctx, env, pat, NONE), exp)) valbinds
          val localValEnv = List.foldl (fn ((_, (_, ve, _), _), acc) => Syntax.VIdMap.unionWith #1 (acc, ve)) Syntax.VIdMap.empty valbinds'
          val localValMap = Syntax.VIdMap.map (fn (vid', ty) => (U.TypeScheme ([], ty), Syntax.ValueVariable, U.MkShortVId vid')) localValEnv
          val localEnv = let val { valMap, tyConMap, tyNameMap, strMap, sigMap, funMap, boundTyVars } = env
                         in { valMap = Syntax.VIdMap.unionWith #2 (valMap, localValMap)
                            , tyConMap = tyConMap
                            , tyNameMap = tyNameMap
                            , strMap = strMap
                            , sigMap = sigMap
                            , funMap = funMap
                            , boundTyVars = List.foldl (fn (tv, m) => Syntax.TyVarMap.insert (m, tv, genTyVar (#context ctx, tv))) boundTyVars tyvarseq
                            }
                         end
          val valbinds'' = List.map (fn (span, (patTy, newValEnv, pat), exp) =>
                                        let val (expTy, exp) = typeCheckExp (ctx, localEnv, exp, SOME patTy)
                                            val () = addConstraint(ctx, env, U.EqConstr(span, patTy, expTy))
                                            val generalizable = isExhaustive(ctx, env, pat) andalso isNonexpansive(env, exp)
                                        in if generalizable then
                                               { sourceSpan = span, pat = pat, exp = exp, expTy = expTy, valEnv = newValEnv }
                                           else
                                               emitTypeError (ctx, [span], "'val rec' must be generalizable")
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
          val env' = envWithValEnv (Syntax.VIdMap.map (fn (vid, tysc) => (tysc, Syntax.ValueVariable, USyntax.MkShortVId vid)) valEnv)
      in (env', [U.RecValDec(span, valbinds)])
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
                    val tystr : U.TypeStructure = { typeFunction = U.TypeFunction(List.map #2 tyvars, ty)
                                                  , valEnv = U.emptyValEnv
                                                  }
                in (Syntax.TyConMap.insert(tyConEnv, tycon, tystr) (* TODO: error on duplicate *), U.TypBind(span, List.map #2 tyvars, tycon, ty) :: typbinds)
                end
          val (tyConEnv, typbinds) = List.foldr doTypBind (Syntax.TyConMap.empty, []) typbinds
      in (envWithTyConEnv(tyConEnv, USyntax.TyNameMap.empty), [U.TypeDec(span, typbinds)])
      end
  | typeCheckDec(ctx, env, S.DatatypeDec(span, datbinds, typbinds))
    = let val datbinds = let val goConBind = doWithtype (#context ctx, env, typbinds)
                         in List.map (fn S.DatBind(span, tyvars, tycon, conbinds) => S.DatBind(span, tyvars, tycon, List.map goConBind conbinds)) datbinds
                         end
          val equalityMap : bool S.TyConMap.map = determineDatatypeEquality (#context ctx, env, List.foldl (fn (S.DatBind (_, tyvars, tycon, conbinds), m) => S.TyConMap.insert (m, tycon, (tyvars, List.mapPartial (fn S.ConBind (_, _, optTy) => optTy) conbinds))) S.TyConMap.empty datbinds)
          val datbinds = List.map (fn datbind as S.DatBind (span, tyvars, tycon, conbinds) => (datbind, newTyName (#context ctx, tycon))) datbinds
          val partialEnv = envWithTyConEnv (List.foldl (fn ((S.DatBind(span, tyvars, tycon, conbinds), tycon'), (m, m')) =>
                                                           let val tyvars = List.map (fn tv => genTyVar (#context ctx, tv)) tyvars
                                                               val tystr = { typeFunction = U.TypeFunction(tyvars, U.TyCon(span, List.map (fn tv => U.TyVar(span, tv)) tyvars, tycon'))
                                                                           , valEnv = U.emptyValEnv
                                                                           }
                                                               val tynameattr = { arity = List.length tyvars
                                                                                , admitsEquality = S.TyConMap.lookup(equalityMap, tycon)
                                                                                , overloadClass = NONE
                                                                                }
                                                           in (Syntax.TyConMap.insert(m, tycon, tystr), USyntax.TyNameMap.insert(m', tycon', tynameattr))
                                                           end
                                                       ) (Syntax.TyConMap.empty, USyntax.TyNameMap.empty) datbinds)
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
                              val isSoleConstructor = case conbinds of
                                                          [_] => true
                                                        | _ => false
                              val idstatus = Syntax.ValueConstructor isSoleConstructor
                              val (valEnv, conbinds) = List.foldr (fn (S.ConBind(span, vid, optTy), (valEnv, conbinds)) =>
                                                                      let val vid' = newVId (#context ctx, vid)
                                                                          val optTy = Option.map (fn ty => evalTy (#context ctx, env, ty)) optTy
                                                                          val conbind = U.ConBind(span, vid', optTy)
                                                                          val tysc = U.TypeScheme(List.map (fn tv => (tv, [])) tyvars, case optTy of
                                                                                                                                           NONE => U.TyCon(span, List.map (fn tv => U.TyVar(span, tv)) tyvars, tyname)
                                                                                                                                         | SOME payloadTy => U.FnType(span, payloadTy, U.TyCon(span, List.map (fn tv => U.TyVar(span, tv)) tyvars, tyname))
                                                                                                 )
                                                                      in (Syntax.VIdMap.insert(valEnv, vid, (tysc, idstatus, U.MkShortVId vid')) (* TODO: check for duplicate *), conbind :: conbinds)
                                                                      end
                                                                  ) (Syntax.VIdMap.empty, []) conbinds
                              val datbind = U.DatBind(span, tyvars, tyname, conbinds, S.TyConMap.lookup(equalityMap, tycon))
                              val tystr = { typeFunction = U.TypeFunction(tyvars, U.TyCon(span, List.map (fn tv => U.TyVar(span, tv)) tyvars, tyname))
                                          , valEnv = Syntax.VIdMap.map (fn (tysc, ids, _) => (tysc, ids)) valEnv
                                          }
                              val tynameattr = { arity = List.length tyvars
                                               , admitsEquality = Syntax.TyConMap.lookup(equalityMap, tycon)
                                               , overloadClass = NONE
                                               }
                          in (Syntax.TyConMap.insert(tyConMap, tycon, tystr), USyntax.TyNameMap.insert(tyNameMap, tyname, tynameattr), Syntax.VIdMap.unionWith #2 (accValEnv, valEnv) (* TODO: check for duplicate *), datbind :: datbinds)
                          end
                in List.foldr doDatBind (Syntax.TyConMap.empty, USyntax.TyNameMap.empty, Syntax.VIdMap.empty, []) datbinds
                end
          val (tyConMap, typbinds) = let fun doTypBind (S.TypBind(span, tyvars, tycon, ty), (tyConEnv, typbinds))
                                             = let val tyvars = List.map (fn tv => (tv, genTyVar (#context ctx, tv))) tyvars
                                                   val env' = { valMap = Syntax.VIdMap.unionWith #2 (#valMap env, valMap)
                                                              , tyConMap = Syntax.TyConMap.unionWith #2 (#tyConMap env, tyConMap)
                                                              , tyNameMap = USyntax.TyNameMap.unionWith #2 (#tyNameMap env, tyNameMap)
                                                              , strMap = #strMap env
                                                              , sigMap = #sigMap env
                                                              , funMap = #funMap env
                                                              , boundTyVars = List.foldl Syntax.TyVarMap.insert' (#boundTyVars env) tyvars
                                                              }
                                                   val ty = evalTy (#context ctx, env', ty)
                                                   val tystr : U.TypeStructure = { typeFunction = U.TypeFunction(List.map #2 tyvars, ty)
                                                                                 , valEnv = U.emptyValEnv
                                                                                 }
                                               in (Syntax.TyConMap.insert(tyConEnv, tycon, tystr) (* TODO: error on duplicate *), U.TypBind(span, List.map #2 tyvars, tycon, ty) :: typbinds)
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
                    [U.DatatypeDec(span, datbinds)]
                else
                    [U.DatatypeDec(span, datbinds), U.TypeDec(span, typbinds)]
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
                                   SOME (s, U.MkLongStrId(strid0, strids0)) => (fn vid => U.MkLongVId(strid0, strids0 @ strids, vid))
                                 | NONE => emitTypeError (ctx, [span], "datatype replication: structure " ^ Syntax.print_StrId strid0 ^ " not found (internal error)")
          val env' = { valMap = Syntax.VIdMap.mapi (fn (vid, (tysc, ids)) => (tysc, ids, getLongVId vid)) (#valEnv tystr)
                     , tyConMap = Syntax.TyConMap.singleton(tycon, tystr)
                     , tyNameMap = USyntax.TyNameMap.empty
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
                             , tyConMap = Syntax.TyConMap.unionWith #2 (Syntax.TyConMap.map (fn { typeFunction, valEnv = _ } => { typeFunction = typeFunction, valEnv = U.emptyValEnv }) (#tyConMap env'), #tyConMap env'')
                             , tyNameMap = USyntax.TyNameMap.unionWith #2 (#tyNameMap env', #tyNameMap env'') (* should be disjoint *)
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
                    val valMap = S.VIdMap.insert(valMap, vid, (U.TypeScheme([], case optTy of
                                                                                    NONE => primTy_exn
                                                                                  | SOME ty => U.FnType(span, ty, primTy_exn)
                                                                           ), Syntax.ExceptionConstructor, U.MkShortVId(vid')))
                in (valMap, U.ExBind(span, vid', optTy) :: exbinds)
                end
            | doExBind(S.ExReplication(span, vid, longvid), (valMap, exbinds))
              = let val vid' = newVId (#context ctx, vid)
                in case lookupLongVIdInEnv(ctx, env, span, longvid) of
                       Found (longvid, tysc, ids as Syntax.ExceptionConstructor) =>
                       let val optTy = case tysc of
                                           U.TypeScheme([], U.FnType(_, payloadTy, _)) => SOME payloadTy
                                         | U.TypeScheme([], _) => NONE
                                         | U.TypeScheme(_ :: _, _) => emitTypeError (ctx, [span], "exception constructor must have monomorphic type")
                       in (S.VIdMap.insert(valMap, vid, (tysc, ids, U.MkShortVId(vid'))), U.ExReplication(span, vid', longvid, optTy) :: exbinds)
                       end
                     | Found _ => emitTypeError (ctx, [span], "exception replication: RHS must be an exception constructor")
                     | ValueNotFound notfound => emitTypeError (ctx, [span], "exception replication: RHS must be an exception constructor, but value name '" ^ Syntax.print_LongVId notfound ^ "' was not found")
                     | StructureNotFound notfound => emitTypeError (ctx, [span], "exception replication: RHS must be an exception constructor, but structure name '" ^ Syntax.print_LongStrId notfound ^ "' was not found")
                end
          val (valMap, exbinds) = List.foldr doExBind (Syntax.VIdMap.empty, []) exbinds
      in (envWithValEnv valMap, [U.ExceptionDec(span, exbinds)])
      end
  | typeCheckDec(ctx, env, S.LocalDec(span, decs1, decs2))
    = let val (env', decs1) = typeCheckDecs(ctx, env, decs1)
          val (env'', decs2) = typeCheckDecs(ctx, mergeEnv(env, env'), decs2)
          val env'' = { valMap = #valMap env''
                      , tyConMap = #tyConMap env''
                      , tyNameMap = USyntax.TyNameMap.unionWith #2 (#tyNameMap env', #tyNameMap env'')
                      , strMap = #strMap env''
                      , sigMap = #sigMap env''
                      , funMap = #funMap env''
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
                     SOME (s, U.MkLongStrId(strid0, strids0)) => { valMap = Syntax.VIdMap.mapi (fn (vid, (tysc, ids)) => (tysc, ids, U.MkLongVId(strid0, strids0, vid))) (#valMap s)
                                                                 , tyConMap = #tyConMap s
                                                                 , tyNameMap = USyntax.TyNameMap.empty
                                                                 , strMap = Syntax.StrIdMap.mapi (fn (strid', U.MkSignature s) => (s, U.MkLongStrId(strid0, strids0 @ [strid']))) (#strMap s)
                                                                 , sigMap = Syntax.SigIdMap.empty
                                                                 , funMap = Syntax.FunIdMap.empty
                                                                 , boundTyVars = Syntax.TyVarMap.empty
                                                                 }
                   | NONE => emitTypeError (ctx, [span], "structure not found")
                )
            | getStructure(Syntax.MkQualified(strid0 :: strids, strid'))
              = (case Syntax.StrIdMap.find(#strMap env, strid0) of
                     SOME (s0, U.MkLongStrId(strid0, strids0)) => let val s = lookupStr (#context ctx, s0, span, strids @ [strid'])
                                                                      val strids = strids0 @ strids @ [strid']
                                                                  in { valMap = Syntax.VIdMap.mapi (fn (vid, (tysc, ids)) => (tysc, ids, U.MkLongVId(strid0, strids, vid))) (#valMap s)
                                                                     , tyConMap = #tyConMap s
                                                                     , tyNameMap = USyntax.TyNameMap.empty
                                                                     , strMap = Syntax.StrIdMap.mapi (fn (strid', U.MkSignature s) => (s, U.MkLongStrId(strid0, strids @ [strid']))) (#strMap s)
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
    = let val { typeFunction = U.TypeFunction (tyvars, ty), ... } = lookupTyConInEnv (#context ctx, env, span, longtycon)
          val tyname = if List.null tyvars then
                           case ty of
                               U.TyCon(_, [], tyname) => tyname
                             | _ => emitTypeError (ctx, [span], "overload declaration: longtycon must refer to a concrete type")
                       else
                           emitTypeError (ctx, [span], "overload declaration: longtycon must refer to a concrete type")
          val ty = U.TyCon(span, [], tyname)
          val map : (U.Ty * U.Exp) Syntax.OverloadKeyMap.map = Syntax.OverloadKeyMap.map (fn exp => typeCheckExp (ctx, env, exp, NONE)) map
          val () = case Syntax.OverloadKeyMap.find(map, Syntax.OVERLOAD_abs) of
                       NONE => ()
                     | SOME (ty', _) => addConstraint(ctx, env, U.EqConstr(span, ty', U.FnType(span, ty, ty)))
          val () = case Syntax.OverloadKeyMap.find(map, Syntax.OVERLOAD_TILDE) of
                       NONE => ()
                     | SOME (ty', _) => addConstraint(ctx, env, U.EqConstr(span, ty', U.FnType(span, ty, ty)))
          val () = case Syntax.OverloadKeyMap.find(map, Syntax.OVERLOAD_div) of
                       NONE => ()
                     | SOME (ty', _) => addConstraint(ctx, env, U.EqConstr(span, ty', U.FnType(span, U.PairType(span, ty, ty), ty)))
          val () = case Syntax.OverloadKeyMap.find(map, Syntax.OVERLOAD_mod) of
                       NONE => ()
                     | SOME (ty', _) => addConstraint(ctx, env, U.EqConstr(span, ty', U.FnType(span, U.PairType(span, ty, ty), ty)))
          val () = case Syntax.OverloadKeyMap.find(map, Syntax.OVERLOAD_TIMES) of
                       NONE => ()
                     | SOME (ty', _) => addConstraint(ctx, env, U.EqConstr(span, ty', U.FnType(span, U.PairType(span, ty, ty), ty)))
          val () = case Syntax.OverloadKeyMap.find(map, Syntax.OVERLOAD_DIVIDE) of
                       NONE => ()
                     | SOME (ty', _) => addConstraint(ctx, env, U.EqConstr(span, ty', U.FnType(span, U.PairType(span, ty, ty), ty)))
          val () = case Syntax.OverloadKeyMap.find(map, Syntax.OVERLOAD_PLUS) of
                       NONE => ()
                     | SOME (ty', _) => addConstraint(ctx, env, U.EqConstr(span, ty', U.FnType(span, U.PairType(span, ty, ty), ty)))
          val () = case Syntax.OverloadKeyMap.find(map, Syntax.OVERLOAD_MINUS) of
                       NONE => ()
                     | SOME (ty', _) => addConstraint(ctx, env, U.EqConstr(span, ty', U.FnType(span, U.PairType(span, ty, ty), ty)))
          val () = case Syntax.OverloadKeyMap.find(map, Syntax.OVERLOAD_LT) of
                       NONE => ()
                     | SOME (ty', _) => addConstraint(ctx, env, U.EqConstr(span, ty', U.FnType(span, U.PairType(span, ty, ty), primTy_bool)))
          val () = case Syntax.OverloadKeyMap.find(map, Syntax.OVERLOAD_LE) of
                       NONE => ()
                     | SOME (ty', _) => addConstraint(ctx, env, U.EqConstr(span, ty', U.FnType(span, U.PairType(span, ty, ty), primTy_bool)))
          val () = case Syntax.OverloadKeyMap.find(map, Syntax.OVERLOAD_GT) of
                       NONE => ()
                     | SOME (ty', _) => addConstraint(ctx, env, U.EqConstr(span, ty', U.FnType(span, U.PairType(span, ty, ty), primTy_bool)))
          val () = case Syntax.OverloadKeyMap.find(map, Syntax.OVERLOAD_GE) of
                       NONE => ()
                     | SOME (ty', _) => addConstraint(ctx, env, U.EqConstr(span, ty', U.FnType(span, U.PairType(span, ty, ty), primTy_bool)))
          val () = case Syntax.OverloadKeyMap.find(map, Syntax.OVERLOAD_fromInt) of
                       NONE => ()
                     | SOME (ty', _) => addConstraint(ctx, env, U.EqConstr(span, ty', U.FnType(span, primTy_int, ty)))
          val () = case Syntax.OverloadKeyMap.find(map, Syntax.OVERLOAD_fromWord) of
                       NONE => ()
                     | SOME (ty', _) => addConstraint(ctx, env, U.EqConstr(span, ty', U.FnType(span, primTy_word, ty)))
          val attr = lookupTyNameInEnv (#context ctx, env, span, tyname)
          val attr = { arity = #arity attr
                     , admitsEquality = #admitsEquality attr
                     , overloadClass = SOME class
                     }
          val env' = envWithTyConEnv (Syntax.TyConMap.empty, USyntax.TyNameMap.singleton (tyname, attr))
      in (env', [U.OverloadDec(span, class, tyname, Syntax.OverloadKeyMap.map #2 map)])
      end
(* typeCheckDecs : InferenceContext * Env * S.Dec list -> (* created environment *) Env * U.Dec list *)
and typeCheckDecs(ctx, env, []) : Env * U.Dec list = (emptyEnv, [])
  | typeCheckDecs(ctx, env, dec :: decs) = let val (env', dec) = typeCheckDec(ctx, env, dec)
                                               val (env'', decs) = typeCheckDecs(ctx, mergeEnv(env, env'), decs)
                                           in (mergeEnv(env', env''), dec @ decs)
                                           end
 (* typeCheckMatch : InferenceContext * Env * SourcePos.span * (S.Pat * S.Exp) list * (* pattern type hint *) U.Ty option * (* expression type hint *) U.Ty option -> (* pattern *) Syntax.Ty * (* expression *) Syntax.Ty * (Pat * Exp) list *)
and typeCheckMatch (ctx, env, span, (pat0, exp0) :: rest, patTyHint : U.Ty option, expTyHint : U.Ty option) : U.Ty * U.Ty * (U.Pat * U.Exp) list
    = let val (patTy, expTy, pat0', exp0') = typeCheckMatchBranch (ctx, env, pat0, exp0, patTyHint, expTyHint)
          fun oneBranch(pat, exp)
              = let val (patTy', expTy', pat', exp') = typeCheckMatchBranch (ctx, env, pat, exp, SOME patTy, SOME expTy)
                in addConstraint(ctx, env, U.EqConstr(span, patTy, patTy'))
                 ; addConstraint(ctx, env, U.EqConstr(span, expTy, expTy'))
                 ; (pat', exp')
                end
          val rest' = List.map oneBranch rest
      in (patTy, expTy, (pat0', exp0') :: rest')
      end
  | typeCheckMatch (ctx, env, span, nil, _, _) = emitTypeError (ctx, [span], "invalid syntax tree: match is empty")
and typeCheckMatchBranch (ctx : InferenceContext, env : Env, pat : S.Pat, exp : S.Exp, patTyHint, expTyHint) : U.Ty * U.Ty * U.Pat * U.Exp
    = let val (patTy, vars, pat') = typeCheckPat (ctx, env, pat, patTyHint)
          val env' = mergeEnv(env, envWithValEnv (Syntax.VIdMap.map (fn (vid, ty) => (U.TypeScheme([], ty), Syntax.ValueVariable, U.MkShortVId vid)) vars))
          val (expTy, exp') = typeCheckExp (ctx, env', exp, expTyHint)
      in (patTy, expTy, pat', exp')
      end

(* pretty printing *)
          (*
structure PrettyPrint = struct
fun print_Env ({ tyConMap, valMap, strMap, boundTyVars, ... } : Env) = "Env{tyMap=" ^ USyntax.print_TyConMap (fn _ => "TypeStructure _") tyConMap ^ ",valMap=" ^ USyntax.print_VIdMap (Syntax.print_pair (USyntax.print_TypeScheme, Syntax.print_IdStatus)) valMap ^ ",strMap=...,boundTyVars=...}"
end (* structure PrettyPrint *)
open PrettyPrint
          *)
(* applyDefaultTypes : Context * (U.UnaryConstraint list) USyntax.TyVarMap.map * USyntax.TopDec list -> U.Ty U.TyVarMap.map *)
fun applyDefaultTypes(ctx, tvc, decs : U.Dec list) : U.Ty U.TyVarMap.map =
    let fun findClass [] = NONE
          | findClass (USyntax.IsInt _ :: _) = SOME Syntax.CLASS_INT
          | findClass (USyntax.IsWord _ :: _) = SOME Syntax.CLASS_WORD
          | findClass (USyntax.IsReal _ :: _) = SOME Syntax.CLASS_REAL
          | findClass (USyntax.IsChar _ :: _) = SOME Syntax.CLASS_CHAR
          | findClass (USyntax.IsString _ :: _) = SOME Syntax.CLASS_STRING
          | findClass (_ :: xs) = findClass xs
        fun doInt [] = primTy_int
          | doInt (USyntax.HasField{...} :: xs) = emitError(ctx, [], "invalid record syntax for int")
          | doInt (USyntax.RecordExt{...} :: xs) = emitError(ctx, [], "invalid record syntax for int")
          | doInt (USyntax.SubrecordOf{...} :: xs) = emitError(ctx, [], "invalid record syntax for int")
          | doInt (USyntax.IsEqType _ :: xs) = doInt xs
          | doInt (USyntax.IsIntegral _ :: xs) = doInt xs
          | doInt (USyntax.IsSignedReal _ :: xs) = doInt xs
          | doInt (USyntax.IsRing _ :: xs) = doInt xs
          | doInt (USyntax.IsField _ :: xs) = emitError(ctx, [], "cannot apply / operator on ints")
          | doInt (USyntax.IsSigned _ :: xs) = doInt xs
          | doInt (USyntax.IsOrdered _ :: xs) = doInt xs
          | doInt (USyntax.IsInt _ :: xs) = doInt xs
          | doInt (USyntax.IsWord _ :: xs) = emitError(ctx, [], "type mismatch: int vs word")
          | doInt (USyntax.IsReal _ :: xs) = emitError(ctx, [], "type mismatch: int vs real")
          | doInt (USyntax.IsChar _ :: xs) = emitError(ctx, [], "type mismatch: int vs char")
          | doInt (USyntax.IsString _ :: xs) = emitError(ctx, [], "type mismatch: int vs string")
        fun doWord [] = primTy_word
          | doWord (USyntax.HasField{...} :: xs) = emitError(ctx, [], "invalid record syntax for word")
          | doWord (USyntax.RecordExt{...} :: xs) = emitError(ctx, [], "invalid record syntax for word")
          | doWord (USyntax.SubrecordOf{...} :: xs) = emitError(ctx, [], "invalid record syntax for word")
          | doWord (USyntax.IsEqType _ :: xs) = doWord xs
          | doWord (USyntax.IsIntegral _ :: xs) = doWord xs
          | doWord (USyntax.IsSignedReal _ :: xs) = emitError(ctx, [], "abs is invalid for word")
          | doWord (USyntax.IsRing _ :: xs) = doWord xs
          | doWord (USyntax.IsField _ :: xs) = emitError(ctx, [], "cannot apply / operator on words")
          | doWord (USyntax.IsSigned _ :: xs) = doWord xs
          | doWord (USyntax.IsOrdered _ :: xs) = doWord xs
          | doWord (USyntax.IsInt _ :: xs) = emitError(ctx, [], "type mismatch: word vs int")
          | doWord (USyntax.IsWord _ :: xs) = doWord xs
          | doWord (USyntax.IsReal _ :: xs) = emitError(ctx, [], "type mismatch: word vs real")
          | doWord (USyntax.IsChar _ :: xs) = emitError(ctx, [], "type mismatch: word vs char")
          | doWord (USyntax.IsString _ :: xs) = emitError(ctx, [], "type mismatch: word vs string")
        fun doReal [] = primTy_real
          | doReal (USyntax.HasField{...} :: xs) = emitError(ctx, [], "invalid record syntax for real")
          | doReal (USyntax.RecordExt{...} :: xs) = emitError(ctx, [], "invalid record syntax for real")
          | doReal (USyntax.SubrecordOf{...} :: xs) = emitError(ctx, [], "invalid record syntax for real")
          | doReal (USyntax.IsEqType _ :: xs) = emitError(ctx, [], "real does not admit equality")
          | doReal (USyntax.IsIntegral _ :: xs) = emitError(ctx, [], "div, mod are invalid for real")
          | doReal (USyntax.IsSignedReal _ :: xs) = doReal xs
          | doReal (USyntax.IsRing _ :: xs) = doReal xs
          | doReal (USyntax.IsField _ :: xs) = doReal xs
          | doReal (USyntax.IsSigned _ :: xs) = doReal xs
          | doReal (USyntax.IsOrdered _ :: xs) = doReal xs
          | doReal (USyntax.IsInt _ :: xs) = emitError(ctx, [], "type mismatch: real vs int")
          | doReal (USyntax.IsWord _ :: xs) = emitError(ctx, [], "type mismatch: real vs word")
          | doReal (USyntax.IsReal _ :: xs) = doReal xs
          | doReal (USyntax.IsChar _ :: xs) = emitError(ctx, [], "type mismatch: real vs char")
          | doReal (USyntax.IsString _ :: xs) = emitError(ctx, [], "type mismatch: real vs string")
        fun doChar [] = primTy_char
          | doChar (USyntax.HasField{...} :: xs) = emitError(ctx, [], "invalid record syntax for char")
          | doChar (USyntax.RecordExt{...} :: xs) = emitError(ctx, [], "invalid record syntax for char")
          | doChar (USyntax.SubrecordOf{...} :: xs) = emitError(ctx, [], "invalid record syntax for char")
          | doChar (USyntax.IsEqType _ :: xs) = doChar xs
          | doChar (USyntax.IsIntegral _ :: xs) = emitError(ctx, [], "invalid operation on char")
          | doChar (USyntax.IsSignedReal _ :: xs) = emitError(ctx, [], "invalid operation on char")
          | doChar (USyntax.IsRing _ :: xs) = emitError(ctx, [], "invalid operation on char")
          | doChar (USyntax.IsField _ :: xs) = emitError(ctx, [], "invalid operation on char")
          | doChar (USyntax.IsSigned _ :: xs) = emitError(ctx, [], "invalid operation on char")
          | doChar (USyntax.IsOrdered _ :: xs) = doChar xs
          | doChar (USyntax.IsInt _ :: xs) = emitError(ctx, [], "type mismatch: char vs int")
          | doChar (USyntax.IsWord _ :: xs) = emitError(ctx, [], "type mismatch: char vs word")
          | doChar (USyntax.IsReal _ :: xs) = emitError(ctx, [], "type mismatch: char vs real")
          | doChar (USyntax.IsChar _ :: xs) = doChar xs
          | doChar (USyntax.IsString _ :: xs) = emitError(ctx, [], "type mismatch: char vs string")
        fun doString [] = primTy_string
          | doString (USyntax.HasField{...} :: xs) = emitError(ctx, [], "invalid record syntax for string")
          | doString (USyntax.RecordExt{...} :: xs) = emitError(ctx, [], "invalid record syntax for string")
          | doString (USyntax.SubrecordOf{...} :: xs) = emitError(ctx, [], "invalid record syntax for string")
          | doString (USyntax.IsEqType _ :: xs) = doString xs
          | doString (USyntax.IsIntegral _ :: xs) = emitError(ctx, [], "invalid operation on string")
          | doString (USyntax.IsSignedReal _ :: xs) = emitError(ctx, [], "invalid operation on string")
          | doString (USyntax.IsRing _ :: xs) = emitError(ctx, [], "invalid operation on string")
          | doString (USyntax.IsField _ :: xs) = emitError(ctx, [], "invalid operation on string")
          | doString (USyntax.IsSigned _ :: xs) = emitError(ctx, [], "invalid operation on string")
          | doString (USyntax.IsOrdered _ :: xs) = doString xs
          | doString (USyntax.IsInt _ :: xs) = emitError(ctx, [], "type mismatch: string vs int")
          | doString (USyntax.IsWord _ :: xs) = emitError(ctx, [], "type mismatch: string vs word")
          | doString (USyntax.IsReal _ :: xs) = emitError(ctx, [], "type mismatch: string vs real")
          | doString (USyntax.IsChar _ :: xs) = emitError(ctx, [], "type mismatch: string vs char")
          | doString (USyntax.IsString _ :: xs) = doString xs
        fun doIntOrReal [] = primTy_int
          | doIntOrReal (USyntax.HasField{...} :: _) = emitError(ctx, [], "unresolved flex record")
          | doIntOrReal (USyntax.RecordExt{...} :: _) = emitError(ctx, [], "unresolved flex record")
          | doIntOrReal (USyntax.SubrecordOf{...} :: _) = emitError(ctx, [], "unresolved flex record")
          | doIntOrReal (USyntax.IsEqType _ :: xs) = doInt xs
          | doIntOrReal (USyntax.IsIntegral _ :: xs) = doInt xs
          | doIntOrReal (USyntax.IsSignedReal _ :: xs) = doIntOrReal xs
          | doIntOrReal (USyntax.IsRing _ :: xs) = doIntOrReal xs
          | doIntOrReal (USyntax.IsField _ :: xs) = doReal xs
          | doIntOrReal (USyntax.IsSigned _ :: xs) = doIntOrReal xs
          | doIntOrReal (USyntax.IsOrdered _ :: xs) = doIntOrReal xs
          | doIntOrReal (USyntax.IsInt _ :: xs) = doInt xs (* cannot occur *)
          | doIntOrReal (USyntax.IsWord _ :: xs) = doIntOrReal xs (* cannot occur *)
          | doIntOrReal (USyntax.IsReal _ :: xs) = doReal xs (* cannot occur *)
          | doIntOrReal (USyntax.IsChar _ :: xs) = doIntOrReal xs (* cannot occur *)
          | doIntOrReal (USyntax.IsString _ :: xs) = doIntOrReal xs (* cannot occur *)
        fun defaultTyForConstraints(eq, []) = primTy_unit
          | defaultTyForConstraints(eq, USyntax.HasField{...} :: _) = emitError(ctx, [], "unresolved flex record")
          | defaultTyForConstraints(eq, USyntax.RecordExt{...} :: _) = emitError(ctx, [], "unresolved flex record")
          | defaultTyForConstraints(eq, USyntax.SubrecordOf{...} :: _) = emitError(ctx, [], "unresolved flex record")
          | defaultTyForConstraints(eq, USyntax.IsEqType _ :: xs) = defaultTyForConstraints(true, xs)
          | defaultTyForConstraints(eq, USyntax.IsIntegral _ :: xs) = doInt xs
          | defaultTyForConstraints(eq, USyntax.IsSignedReal _ :: xs) = if eq then doInt xs else doIntOrReal xs
          | defaultTyForConstraints(eq, USyntax.IsRing _ :: xs) = if eq then doInt xs else doIntOrReal xs
          | defaultTyForConstraints(eq, USyntax.IsField _ :: xs) = if eq then emitError(ctx, [], "real does not admit equality") else doReal xs
          | defaultTyForConstraints(eq, USyntax.IsSigned _ :: xs) = if eq then doInt xs else doIntOrReal xs
          | defaultTyForConstraints(eq, USyntax.IsOrdered _ :: xs) = if eq then doInt xs else doIntOrReal xs
          | defaultTyForConstraints(eq, USyntax.IsInt _ :: xs) = doInt xs (* cannot occur *)
          | defaultTyForConstraints(eq, USyntax.IsWord _ :: xs) = doWord xs (* cannot occur *)
          | defaultTyForConstraints(eq, USyntax.IsReal _ :: xs) = if eq then emitError(ctx, [], "real does not admit equality") else doReal xs (* cannot occur *)
          | defaultTyForConstraints(eq, USyntax.IsChar _ :: xs) = doChar xs (* cannot occur *)
          | defaultTyForConstraints(eq, USyntax.IsString _ :: xs) = doString xs (* cannot occur *)
        fun doTyVar tv = case U.TyVarMap.find(tvc, tv) of
                             NONE => primTy_unit
                           | SOME constraints => case findClass constraints of
                                                     SOME Syntax.CLASS_INT => doInt constraints
                                                   | SOME Syntax.CLASS_WORD => doWord constraints
                                                   | SOME Syntax.CLASS_REAL => doReal constraints
                                                   | SOME Syntax.CLASS_CHAR => doChar constraints
                                                   | SOME Syntax.CLASS_STRING => doString constraints
                                                   | NONE => defaultTyForConstraints(false, constraints)
        val freeTyVars = USyntax.freeTyVarsInDecs(USyntax.TyVarSet.empty, decs)
    in USyntax.TyVarSet.foldl (fn (tv, map) => USyntax.TyVarMap.insert(map, tv, doTyVar tv)) USyntax.TyVarMap.empty freeTyVars
    end

fun typeCheckCoreDecs (ctx : Context, env, decs) : Env * USyntax.Dec list
    = let val ictx = { context = ctx
                     , tyVarConstraints = ref USyntax.TyVarMap.empty
                     , tyVarSubst = ref USyntax.TyVarMap.empty
                     }
          val (env, decs) = typeCheckDecs (ictx, env, decs)
          val subst = !(#tyVarSubst ictx)
          val tvc = !(#tyVarConstraints ictx)
          val env = applySubstEnv subst env
          val decs = #doDecs (USyntax.mapTy (ctx, subst, false)) decs
          val subst = applyDefaultTypes (ctx, tvc, decs)
          val env = applySubstEnv subst env
          val decs = #doDecs (USyntax.mapTy (ctx, subst, false)) decs
      in (env, decs)
      end

fun checkTyScope (ctx, tvset : U.TyVarSet.set, tynameset : U.TyNameSet.set)
    = let fun goTy (U.TyVar(span, tv))
              = if U.TyVarSet.member(tvset, tv) then
                    ()
                else
                    emitError (ctx, [span], "type variable scope violation: " ^ USyntax.PrettyPrint.print_TyVar tv)
            | goTy (U.RecordType(span, fields)) = Syntax.LabelMap.app goTy fields
            | goTy (U.TyCon(span, tyargs, tyname))
              = if U.TyNameSet.member(tynameset, tyname) then
                    List.app goTy tyargs
                else
                    emitError (ctx, [span], "type constructor scope violation: " ^ USyntax.PrettyPrint.print_TyName tyname)
            | goTy (U.FnType(span, ty1, ty2)) = ( goTy ty1; goTy ty2 )
          fun goUnaryConstraint (U.HasField { sourceSpan, label, fieldTy }) = goTy fieldTy
            | goUnaryConstraint _ = ()
          fun goTypeScheme (U.TypeScheme (typarams, ty)) = ( List.app (fn (tv, cts) => List.app goUnaryConstraint cts) typarams
                                                           ; #goTy (checkTyScope (ctx, U.TyVarSet.addList (tvset, List.map #1 typarams), tynameset)) ty
                                                           )
          fun goPat (U.WildcardPat _) = ()
            | goPat (U.SConPat _) = ()
            | goPat (U.VarPat (_, _, ty)) = goTy ty
            | goPat (U.RecordPat { sourceSpan, fields, ellipsis }) = ( List.app (fn (label, pat) => goPat pat) fields
                                                                     ; Option.app goPat ellipsis
                                                                     )
            | goPat (U.ConPat { sourceSpan, longvid, payload, tyargs, isSoleConstructor }) = ( List.app goTy tyargs
                                                                                             ; Option.app goPat payload
                                                                                             )
            | goPat (U.TypedPat(span, pat, ty)) = ( goTy ty; goPat pat )
            | goPat (U.LayeredPat(span, vid, ty, pat)) = ( goTy ty; goPat pat )
            | goPat (U.VectorPat(span, pats, ellipsis, elemTy)) = ( goTy elemTy; Vector.app goPat pats )
          fun goExp (U.SConExp (span, scon, ty)) = goTy ty
            | goExp (U.VarExp (span, longvid, ids, tyargs)) = List.app (fn (ty, cts) => (goTy ty; List.app goUnaryConstraint cts)) tyargs
            | goExp (U.RecordExp (span, fields)) = List.app (fn (label, exp) => goExp exp) fields
            | goExp (U.RecordExtExp { sourceSpan, fields, baseExp, baseTy })
              = ( List.app (fn (label, exp) => goExp exp) fields
                ; goExp baseExp
                ; goTy baseTy
                )
            | goExp (U.LetInExp (span, decs, exp)) = let val tynameset = goDecs decs
                                                         val { goExp, ... } = checkTyScope (ctx, tvset, tynameset)
                                                     in goExp exp
                                                     end
            | goExp (U.AppExp (span, exp1, exp2)) = ( goExp exp1; goExp exp2 )
            | goExp (U.TypedExp (span, exp, ty)) = ( goExp exp; goTy ty )
            | goExp (U.HandleExp (span, exp, matches)) = ( goExp exp; List.app (fn (pat, exp) => (goPat pat; goExp exp)) matches )
            | goExp (U.RaiseExp (span, ty, exp)) = ( goTy ty; goExp exp )
            | goExp (U.IfThenElseExp (span, exp1, exp2, exp3)) = ( goExp exp1; goExp exp2; goExp exp3 )
            | goExp (U.CaseExp (span, exp, ty, matches)) = ( goExp exp; goTy ty; List.app (fn (pat, exp) => (goPat pat; goExp exp)) matches )
            | goExp (U.FnExp (span, vid, ty, exp)) = ( goTy ty; goExp exp )
            | goExp (U.ProjectionExp { sourceSpan, label, recordTy, fieldTy }) = ( goTy recordTy; goTy fieldTy )
            | goExp (U.ListExp (span, xs, ty)) = ( Vector.app goExp xs ; goTy ty )
            | goExp (U.VectorExp (span, xs, ty)) = ( Vector.app goExp xs ; goTy ty )
            | goExp (U.PrimExp (span, primOp, tyargs, args)) = ( Vector.app goTy tyargs ; Vector.app goExp args )
          and goDec (U.ValDec (span, valbinds)) = ( List.app goValBind valbinds
                                                  ; tynameset
                                                  )
            | goDec (U.RecValDec (span, valbinds)) = ( List.app goValBind valbinds
                                                     ; tynameset
                                                     )
            | goDec (U.TypeDec (span, typbinds)) = let fun goTypBind (U.TypBind (span, tyvars, tycon, ty)) = let val { goTy, ... } = checkTyScope (ctx, U.TyVarSet.addList (tvset, tyvars), tynameset)
                                                                                                             in goTy ty
                                                                                                             end
                                                   in List.app goTypBind typbinds
                                                    ; tynameset
                                                   end
            | goDec (U.DatatypeDec (span, datbinds)) = let val tynameset = List.foldl (fn (U.DatBind (span, _, tyname, _, _), tynameset) => U.TyNameSet.add (tynameset, tyname)) tynameset datbinds
                                                           fun goDatBind (U.DatBind (span, tyvars, tyname, conbinds, _))
                                                               = let val { goTy, ... } = checkTyScope (ctx, U.TyVarSet.addList (tvset, tyvars), tynameset)
                                                                     fun goConBind (U.ConBind (span, vid, optTy)) = Option.app goTy optTy
                                                                 in List.app goConBind conbinds
                                                                 end
                                                       in List.app goDatBind datbinds
                                                        ; tynameset
                                                       end
            | goDec (U.ExceptionDec (span, exbinds)) = ( List.app (fn U.ExBind (span, vid, optTy) => Option.app goTy optTy
                                                                  | U.ExReplication (span, vid, longvid, optTy) => Option.app goTy optTy
                                                                  ) exbinds
                                                       ; tynameset
                                                       )
            | goDec (U.GroupDec (span, decs)) = goDecs decs
            | goDec (U.OverloadDec (span, class, tyname, map)) = if U.TyNameSet.member (tynameset, tyname) then
                                                                     ( Syntax.OverloadKeyMap.app goExp map
                                                                     ; tynameset
                                                                     )
                                                                 else
                                                                     emitError (ctx, [span], "type constructor scope violation: " ^ USyntax.PrettyPrint.print_TyName tyname)
          and goDecs decs = List.foldl (fn (dec, tynameset) => let val { goDec, ... } = checkTyScope (ctx, tvset, tynameset)
                                                               in goDec dec
                                                               end)
                                       tynameset decs
          and goValBind (U.TupleBind (span, binds, exp)) = ( List.app (fn (vid, ty) => goTy ty) binds
                                                           ; goExp exp
                                                           )
            | goValBind (U.PolyVarBind (span, vid, U.TypeScheme (typarams, ty), exp))
              = let val { goTy, goExp, ... } = checkTyScope (ctx, U.TyVarSet.addList (tvset, List.map #1 typarams), tynameset)
                in List.app (fn (tv, cts) => List.app goUnaryConstraint cts) typarams
                 ; goTy ty
                 ; goExp exp
                end
          fun goStrExp (U.StructExp _) = tynameset
            | goStrExp (U.StrIdExp _) = tynameset
            | goStrExp (U.PackedStrExp { sourceSpan, strExp, payloadTypes, packageSig }) = ( goStrExp strExp ; List.foldl (fn ({ tyname, ... }, set) => U.TyNameSet.add (set, tyname)) tynameset (#bound packageSig) )
            | goStrExp (U.FunctorAppExp { sourceSpan, funId, argumentTypes, argumentStr, packageSig })
              = let val tynameset = goStrExp argumentStr
                    (* TODO: Check argumentTypes *)
                in List.foldl (fn ({ tyname, ... }, set) => U.TyNameSet.add (set, tyname)) tynameset (#bound packageSig)
                end
            | goStrExp (U.LetInStrExp (span, strdecs, strexp)) = let val tynameset = goStrDecs strdecs
                                                                     val { goStrExp, ... } = checkTyScope (ctx, tvset, tynameset)
                                                                 in goStrExp strexp
                                                                 end
          and goStrDec (U.CoreDec(_, dec)) = goDec dec
            | goStrDec (U.StrBindDec(_, strid, strexp, { s, bound })) = List.foldl (fn ({ tyname, ... }, set) => U.TyNameSet.add (set, tyname)) (goStrExp strexp) bound
            | goStrDec (U.GroupStrDec(_, decs)) = goStrDecs decs
          and goStrDecs decs = List.foldl (fn (dec, tynameset) => let val { goStrDec, ... } = checkTyScope (ctx, tvset, tynameset)
                                                                  in goStrDec dec
                                                                  end)
                                          tynameset decs
          fun goFunExp (tynames, strid, s, strexp) = let val tynameset' = List.foldl (fn ({ tyname, ...}, set) => U.TyNameSet.add (set, tyname)) tynameset tynames
                                                         val { goStrExp, ... } = checkTyScope (ctx, tvset, tynameset')
                                                     in goStrExp strexp
                                                      ; tynameset
                                                     end
          fun goTopDec (U.StrDec dec) = goStrDec dec
            | goTopDec (U.FunDec (funid, funexp)) = goFunExp funexp
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
fun checkTyScopeOfProgram (ctx, tynameset : U.TyNameSet.set, program : U.Program)
    = List.foldl (fn (topdec, tynameset) => let val { goTopDecs, ... } = checkTyScope (ctx, U.TyVarSet.empty, tynameset)
                                            in goTopDecs topdec
                                            end)
                 tynameset program

val emptySignature : USyntax.Signature = { valMap = Syntax.VIdMap.empty
                                         , tyConMap = Syntax.TyConMap.empty
                                         , strMap = Syntax.StrIdMap.empty
                                         }

fun mergeSignature(s1 : U.Signature, s2 : U.Signature) : U.Signature
    = { valMap = Syntax.VIdMap.unionWith #2 (#valMap s1, #valMap s2)
      , tyConMap = Syntax.TyConMap.unionWith #2 (#tyConMap s1, #tyConMap s2)
      , strMap = Syntax.StrIdMap.unionWith #2 (#strMap s1, #strMap s2)
      }
fun mergeQSignature(s1 : U.QSignature, s2 : U.QSignature) : U.QSignature
    = { s = mergeSignature(#s s1, #s s2)
      , bound = U.TyNameMap.unionWith #2 (#bound s1, #bound s2)
      }

fun canonicalOrderForQSignature ({ s, bound } : U.QSignature) : U.TyName list
    = let val bound' = U.TyNameMap.mapi (fn (tyname, { arity, admitsEquality, longtycon }) => Option.getOpt (canonicalPathForTyName (s, tyname), longtycon)) bound
          fun insert ([], key, value) = [(key, value)]
            | insert (xs0 as ((k, v) :: xs), key, value) = case Syntax.LongTyCon.compare (k, key) of
                                                               LESS => (k, v) :: insert (xs, key, value)
                                                             | GREATER => (key, value) :: xs0
                                                             | EQUAL => (key, value) :: xs (* cannot happen *)
      in List.map #2 (U.TyNameMap.foldli (fn (tyname, longtycon, acc) => insert (acc, longtycon, tyname)) [] bound')
      end
and canonicalPathForTyName ({ valMap, tyConMap, strMap } : U.Signature, tyname : U.TyName) : Syntax.LongTyCon option
    = let val t = Syntax.TyConMap.filter (fn { typeFunction = U.TypeFunction (tyvars, ty), valEnv } =>
                                             case ty of
                                                 U.TyCon (span, tyargs, tyname') =>
                                                 U.eqTyName (tyname, tyname') andalso ListPair.allEq (fn (tv, U.TyVar (_, tv')) => U.eqUTyVar (tv, tv')
                                                                                                     | _ => false
                                                                                                     ) (tyvars, tyargs)
                                               | _ => false
                                         ) tyConMap
      in case Option.map #1 (Syntax.TyConMap.firsti t) of
             SOME tycon => SOME (Syntax.MkQualified ([], tycon))
           | NONE => let val t = Syntax.StrIdMap.mapPartiali (fn (strid, U.MkSignature s) => Option.map (fn Syntax.MkQualified (strids, tycon) => Syntax.MkQualified (strid :: strids, tycon)) (canonicalPathForTyName (s, tyname))) strMap
                     in Syntax.StrIdMap.first t
                     end
      end

fun addSignatureToEnv(env : SigEnv, s : U.Signature, tyNameMap : TyNameAttr U.TyNameMap.map) : SigEnv
    = { valMap = #valMap env (* not used *)
      , tyConMap = Syntax.TyConMap.unionWith #2 (#tyConMap env, #tyConMap s)
      , tyNameMap = U.TyNameMap.unionWith #2 (#tyNameMap env, tyNameMap) (* should not overlap *)
      , strMap = Syntax.StrIdMap.unionWith #2 (#strMap env, Syntax.StrIdMap.map (fn U.MkSignature s => (s, ())) (#strMap s))
      , sigMap = #sigMap env
      , funMap = #funMap env
      , boundTyVars = #boundTyVars env
      }

fun applySubstTyConInTy (ctx : Context, subst : U.TypeFunction U.TyNameMap.map) : U.Ty -> U.Ty
    = let fun goTy (ty as U.TyVar _) = ty
            | goTy (U.RecordType(span, fields)) = U.RecordType(span, Syntax.LabelMap.map goTy fields)
            | goTy (U.TyCon(span, tyargs, tycon)) = (case U.TyNameMap.find(subst, tycon) of
                                                         NONE => U.TyCon(span, List.map goTy tyargs, tycon)
                                                       | SOME (U.TypeFunction (tyvars, ty)) =>
                                                         let val subst' = (ListPair.foldlEq (fn (tv, tyarg, m) => USyntax.TyVarMap.insert (m, tv, goTy tyarg)) USyntax.TyVarMap.empty (tyvars, tyargs))
                                                                          handle ListPair.UnequalLengths => emitError (ctx, [span], "invalid type constructor substitution")
                                                         in U.applySubstTy subst' ty
                                                         end
                                                    )
            | goTy (U.FnType(span, ty1, ty2)) = U.FnType(span, goTy ty1, goTy ty2)
      in goTy
      end
fun applySubstTyConInSig (ctx : Context, subst : U.TypeFunction U.TyNameMap.map) : U.Signature -> U.Signature
    = let val goTy = applySubstTyConInTy (ctx, subst)
          fun goTypeScheme (U.TypeScheme (tvs, ty)) = U.TypeScheme (tvs, goTy ty)
          fun goTypeFunction (U.TypeFunction (tvs, ty)) = U.TypeFunction (tvs, goTy ty)
          fun goSig { valMap, tyConMap, strMap } = { valMap = Syntax.VIdMap.map (fn (tysc, ids) => (goTypeScheme tysc, ids)) valMap
                                                   , tyConMap = Syntax.TyConMap.map (fn { typeFunction, valEnv } =>
                                                                                        { typeFunction = goTypeFunction typeFunction
                                                                                        , valEnv = Syntax.VIdMap.map (fn (tysc, ids) => (goTypeScheme tysc, ids)) valEnv
                                                                                        }
                                                                                    ) tyConMap
                                                   , strMap = Syntax.StrIdMap.map (fn U.MkSignature s => U.MkSignature (goSig s)) strMap
                                                   }
      in goSig
      end

fun refreshTyNameInTy (ctx : Context, subst : U.TyName U.TyNameMap.map) : U.Ty -> U.Ty
    = let fun goTy (ty as U.TyVar _) = ty
            | goTy (U.RecordType(span, fields)) = U.RecordType(span, Syntax.LabelMap.map goTy fields)
            | goTy (U.TyCon(span, tyargs, tycon)) = let val tyargs = List.map goTy tyargs
                                                    in case U.TyNameMap.find(subst, tycon) of
                                                           NONE => U.TyCon(span, tyargs, tycon)
                                                         | SOME tycon' => U.TyCon(span, tyargs, tycon')
                                                    end
            | goTy (U.FnType(span, ty1, ty2)) = U.FnType(span, goTy ty1, goTy ty2)
      in goTy
      end
fun refreshTyNameInSig (ctx : Context, subst : U.TyName U.TyNameMap.map) : U.Signature -> U.Signature
    = let val goTy = refreshTyNameInTy (ctx, subst)
          fun goTypeScheme (U.TypeScheme (tvs, ty)) = U.TypeScheme (tvs, goTy ty)
          fun goTypeFunction (U.TypeFunction (tvs, ty)) = U.TypeFunction (tvs, goTy ty)
          fun goSig { valMap, tyConMap, strMap } = { valMap = Syntax.VIdMap.map (fn (tysc, ids) => (goTypeScheme tysc, ids)) valMap
                                                   , tyConMap = Syntax.TyConMap.map (fn { typeFunction, valEnv } =>
                                                                                        { typeFunction = goTypeFunction typeFunction
                                                                                        , valEnv = Syntax.VIdMap.map (fn (tysc, ids) => (goTypeScheme tysc, ids)) valEnv
                                                                                        }
                                                                                    ) tyConMap
                                                   , strMap = Syntax.StrIdMap.map (fn U.MkSignature s => U.MkSignature (goSig s)) strMap
                                                   }
      in goSig
      end

fun checkEquality(ctx : Context, env : ('val,'str) Env', tyvars : U.TyVarSet.set) : U.Ty -> bool
    = let fun goTy (U.TyVar (span, tv)) = if U.TyVarSet.member (tyvars, tv) then
                                              true
                                          else
                                              (case tv of
                                                   U.NamedTyVar (_, eq, _) => eq
                                                 | _ => false (* error *)
                                              )
            | goTy (U.RecordType (span, fields)) = Syntax.LabelMap.all goTy fields
            | goTy (U.TyCon (span, tyargs, tyname)) = isRefOrArray tyname
                                                      orelse (let val { admitsEquality, ... } = lookupTyNameInEnv (ctx, env, span, tyname)
                                                              in admitsEquality andalso List.all goTy tyargs
                                                              end
                                                             )
            | goTy (U.FnType _) = false
      in goTy
      end

fun lookupLongTyConInQSignature(ctx, span, s : U.QSignature, longtycon) : U.TypeStructure
    = let val S.MkQualified(strids, tycon as Syntax.MkTyCon name) = longtycon
          val { tyConMap, ... } = lookupStr (ctx, #s s, span, strids)
      in case Syntax.TyConMap.find(tyConMap, tycon) of
             SOME tystr => tystr
           | NONE => emitError (ctx, [span], "unknown type constructor '" ^ name ^ "'")
      end
fun getTypeNameFromTypeStructure(ctx, { typeFunction = U.TypeFunction(tyvars, U.TyCon(_, tyargs, tyname)), ... }) : (U.TyName * int) option
    = let val arity = List.length tyvars
      in if List.length tyargs = arity then
             if ListPair.allEq (fn (tv, U.TyVar(_, tv')) => tv = tv' | _ => false) (tyvars, tyargs) then
                 SOME (tyname, arity)
             else
                 NONE
         else
             NONE
      end
  | getTypeNameFromTypeStructure _ = NONE

fun evalSignature(ctx : Context, env : SigEnv, S.BasicSigExp(span, specs)) : U.QSignature
    = evalSpecs(ctx, env, specs)
  | evalSignature(ctx, env, S.SigIdExp(span, sigid as Syntax.MkSigId name))
    = (case Syntax.SigIdMap.find(#sigMap env, sigid) of
           SOME { s, bound } => let val subst = U.TyNameMap.mapi (fn (tycon, _) => renewTyName(ctx, tycon)) bound
                                in { s = refreshTyNameInSig (ctx, subst) s
                                   , bound = U.TyNameMap.foldli (fn (tycon, attr, map) => U.TyNameMap.insert(map, U.TyNameMap.lookup(subst, tycon), attr)) U.TyNameMap.empty bound
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
                 case U.TyNameMap.find(#bound s, tyname) of
                     SOME { admitsEquality, ... } =>
                     let val () = if admitsEquality andalso not (checkEquality (ctx, env, List.foldl (fn ((_, tv), set) => U.TyVarSet.add(set, tv)) U.TyVarSet.empty tyvars) ty) then
                                      emitError(ctx, [span], "type realisation failed (equality)")
                                  else
                                      ()
                         val subst = U.TyNameMap.singleton(tyname, U.TypeFunction(List.map #2 tyvars, ty))
                     in { s = applySubstTyConInSig (ctx, subst) (#s s), bound = #1 (U.TyNameMap.remove (#bound s, tyname)) }
                     end
                   | NONE => emitError (ctx, [span], "type realisation against a rigid type")
             else
                 emitError (ctx, [span], "type realisation against a rigid type")
           | NONE => emitError (ctx, [span], "type realisation against a rigid type")
      end
and evalSpecs(ctx : Context, env : SigEnv, specs) : U.QSignature
    = List.foldl (fn (spec, s) => let val env' = addSignatureToEnv(env, #s s, U.TyNameMap.map (fn { arity, admitsEquality, longtycon } => { arity = arity, admitsEquality = admitsEquality, overloadClass = NONE }) (#bound s))
                                  in mergeQSignature(s, addSpec(ctx, env', spec))
                                  end) { s = emptySignature, bound = U.TyNameMap.empty } specs
and addSpec(ctx : Context, env : SigEnv, S.ValDesc(span, descs)) : U.QSignature
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
                                                             in Syntax.VIdMap.insert(valMap, vid, (U.TypeScheme(Syntax.TyVarMap.foldr (fn (tv, xs) => (tv, []) :: xs) [] tvs, ty), Syntax.ValueVariable))
                                                             end) Syntax.VIdMap.empty descs
            , tyConMap = Syntax.TyConMap.empty
            , strMap = Syntax.StrIdMap.empty
            }
      , bound = USyntax.TyNameMap.empty
      }
  | addSpec(ctx, env, S.TypeDesc(span, descs))
    = List.foldl (fn ((tyvars, tycon), s) => let val tyname = newTyName(ctx, tycon)
                                                 val tyvars = List.map (fn tv => genTyVar(ctx, tv)) tyvars
                                                 val tystr = { typeFunction = U.TypeFunction(tyvars, U.TyCon(span, List.map (fn tv => U.TyVar(span, tv)) tyvars, tyname))
                                                             , valEnv = Syntax.VIdMap.empty
                                                             }
                                             in { s = { valMap = #valMap (#s s)
                                                      , tyConMap = Syntax.TyConMap.insert(#tyConMap (#s s), tycon, tystr)
                                                      , strMap = #strMap (#s s)
                                                      }
                                                , bound = USyntax.TyNameMap.insert(#bound s, tyname, { arity = List.length tyvars
                                                                                                     , admitsEquality = false
                                                                                                     , longtycon = Syntax.MkQualified([], tycon)
                                                                                                     }
                                                                                  )
                                                }
                                             end
                 )
                 { s = { valMap = Syntax.VIdMap.empty
                       , tyConMap = Syntax.TyConMap.empty
                       , strMap = Syntax.StrIdMap.empty
                       }
                 , bound = USyntax.TyNameMap.empty
                 } descs
  | addSpec(ctx, env, S.EqtypeDesc(span, descs))
    = List.foldl (fn ((tyvars, tycon), s) => let val tyname = newTyName(ctx, tycon)
                                                 val tyvars = List.map (fn tv => genTyVar(ctx, tv)) tyvars
                                                 val tystr = { typeFunction = U.TypeFunction(tyvars, U.TyCon(span, List.map (fn tv => U.TyVar(span, tv)) tyvars, tyname))
                                                             , valEnv = Syntax.VIdMap.empty
                                                             }
                                             in { s = { valMap = #valMap (#s s)
                                                      , tyConMap = Syntax.TyConMap.insert(#tyConMap (#s s), tycon, tystr)
                                                      , strMap = #strMap (#s s)
                                                      }
                                                , bound = USyntax.TyNameMap.insert(#bound s, tyname, { arity = List.length tyvars
                                                                                                     , admitsEquality = true
                                                                                                     , longtycon = Syntax.MkQualified([], tycon)
                                                                                                     }
                                                                                  )
                                                }
                                             end
                 )
                 { s = { valMap = Syntax.VIdMap.empty
                       , tyConMap = Syntax.TyConMap.empty
                       , strMap = Syntax.StrIdMap.empty
                       }
                 , bound = USyntax.TyNameMap.empty
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
                                                                       val tystr = { typeFunction = U.TypeFunction(List.map #2 tyvarPairs, U.TyCon(span, List.map (fn (_, tv) => U.TyVar(span, tv)) tyvarPairs, tyname))
                                                                                   , valEnv = Syntax.VIdMap.empty (* filled later *)
                                                                                   }
                                                                       val tyConMap = Syntax.TyConMap.insert(tyConMap, tycon, tystr)
                                                                       val tyNameMap = USyntax.TyNameMap.insert(tyNameMap, tyname, { arity = List.length tyvars
                                                                                                                                   , admitsEquality = S.TyConMap.lookup(equalityMap, tycon)
                                                                                                                                   , overloadClass = NONE
                                                                                                                                   }
                                                                                                               )
                                                                   in (tyConMap, tyNameMap, (tycon, tyname, tyvarPairs, tystr, condescs) :: descs)
                                                                   end
                                                               ) (Syntax.TyConMap.empty, USyntax.TyNameMap.empty, []) descs
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
                                               val tystr = { typeFunction = U.TypeFunction(List.map #2 tyvars, ty)
                                                           , valEnv = Syntax.VIdMap.empty
                                                           }
                                           in Syntax.TyConMap.insert(tyConMap, tycon, tystr)
                                           end
                                       ) Syntax.TyConMap.empty typbinds
      in List.foldl (fn ((tycon, tyname, tyvarPairs, tystr, condescs), s) =>
                        let val { typeFunction as U.TypeFunction(tyvars, ty), ... } = tystr
                            val env'' = { valMap = #valMap env'
                                        , tyConMap = #tyConMap env'
                                        , tyNameMap = #tyNameMap env'
                                        , strMap = #strMap env'
                                        , sigMap = #sigMap env'
                                        , funMap = #funMap env'
                                        , boundTyVars = List.foldl Syntax.TyVarMap.insert' (#boundTyVars env') tyvarPairs
                                        }
                            val idstatus = Syntax.ValueConstructor (case condescs of
                                                                        [_] => true
                                                                      | _ => false
                                                                   )
                            val valEnv = List.foldl (fn (S.ConBind(span, vid, optTy), valEnv) =>
                                                        let val tysc = U.TypeScheme(List.map (fn tv => (tv, [])) tyvars, case optTy of
                                                                                                                             NONE => ty
                                                                                                                           | SOME payloadTy => U.FnType(span, evalTy(ctx, env'', payloadTy), ty)
                                                                                   )
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
                           , bound = USyntax.TyNameMap.insert(#bound s, tyname, { arity = List.length tyvars
                                                                                , admitsEquality = S.TyConMap.lookup(equalityMap, tycon)
                                                                                , longtycon = Syntax.MkQualified([], tycon)
                                                                                }
                                                             )
                           }
                        end
                    )
                    { s = { valMap = Syntax.VIdMap.empty
                          , tyConMap = withtypeMap
                          , strMap = Syntax.StrIdMap.empty
                          }
                    , bound = USyntax.TyNameMap.empty
                    } descs
      end
  | addSpec(ctx, env, S.DatatypeRepSpec(span, tycon, longtycon))
    = let val tystr = lookupTyConInEnv(ctx, env, span, longtycon)
      in { s = { valMap = #valEnv tystr
               , tyConMap = Syntax.TyConMap.singleton(tycon, tystr)
               , strMap = Syntax.StrIdMap.empty
               }
         , bound = USyntax.TyNameMap.empty
         }
      end
  | addSpec(ctx, env, S.ExDesc(span, descs : (S.VId * S.Ty option) list))
    = { s = { valMap = List.foldl (fn ((vid, optTy), valMap) => let val ty = case optTy of
                                                                                 NONE => primTy_exn
                                                                               | SOME ty => U.FnType(span, evalTy(ctx, env, ty), primTy_exn)
                                                                in Syntax.VIdMap.insert(valMap, vid, (U.TypeScheme([], ty), Syntax.ExceptionConstructor))
                                                                end) Syntax.VIdMap.empty descs
            , tyConMap = Syntax.TyConMap.empty
            , strMap = Syntax.StrIdMap.empty
            }
      , bound = USyntax.TyNameMap.empty
      }
  | addSpec(ctx, env, S.StrDesc(span, descs)) = let val strMap = List.foldl (fn ((strid, sigexp), m) => Syntax.StrIdMap.insert(m, strid, evalSignature(ctx, env, sigexp))) Syntax.StrIdMap.empty descs
                                                in { s = { valMap = Syntax.VIdMap.empty
                                                         , tyConMap = Syntax.TyConMap.empty
                                                         , strMap = Syntax.StrIdMap.map (fn { s, bound } => U.MkSignature s) strMap
                                                         }
                                                   , bound = Syntax.StrIdMap.foldli (fn (strid, { bound, ... }, map) =>
                                                                                        USyntax.TyNameMap.unionWith #2 (map, USyntax.TyNameMap.map (fn { arity, admitsEquality, longtycon = Syntax.MkQualified(strids, tycon) } =>
                                                                                                                                                       { arity = arity
                                                                                                                                                       , admitsEquality = admitsEquality
                                                                                                                                                       , longtycon = Syntax.MkQualified(strid :: strids, tycon)
                                                                                                                                                       }
                                                                                                                                                   ) bound)) USyntax.TyNameMap.empty strMap
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
                                            val tystr = { typeFunction = U.TypeFunction(List.map #2 tyvars, ty)
                                                        , valEnv = Syntax.VIdMap.empty
                                                        }
                                        in Syntax.TyConMap.insert(tyConMap, tycon, tystr)
                                        end
                                    ) Syntax.TyConMap.empty descs
            , strMap = Syntax.StrIdMap.empty
            }
      , bound = USyntax.TyNameMap.empty
      }
and shareLongTyCons(ctx, span, s : U.QSignature, longtycon0, longtycons) : U.QSignature
    = let val tystr0 = lookupLongTyConInQSignature(ctx, span, s, longtycon0)
          val tystrs = List.map (fn longtycon => lookupLongTyConInQSignature(ctx, span, s, longtycon)) longtycons
      in case getTypeNameFromTypeStructure(ctx, tystr0) of
             SOME (tyname0, arity0) =>
             (case U.TyNameMap.find(#bound s, tyname0) of
                  SOME { arity, admitsEquality, ... } =>
                  if arity0 = arity then
                      let val typeFn = let val tyvars = List.tabulate(arity, fn _ => genTyVar(ctx, Syntax.MkTyVar "a"))
                                       in U.TypeFunction(tyvars, U.TyCon(span, List.map (fn tv => U.TyVar(span, tv)) tyvars, tyname0))
                                       end
                          val (subst, admitsEquality) = List.foldl (fn (tystr, (subst, admitsEquality)) =>
                                                                       case getTypeNameFromTypeStructure(ctx, tystr) of
                                                                           SOME (tyname, arity') =>
                                                                           if arity' <> arity then
                                                                               emitError(ctx, [span], "sharing: arity mismatch")
                                                                           else
                                                                               (case U.TyNameMap.find(#bound s, tyname) of
                                                                                    SOME { arity = arity', admitsEquality = admitsEquality', ... } =>
                                                                                    if arity' <> arity then
                                                                                        emitError(ctx, [span], "sharing: arity mismatch")
                                                                                    else if U.eqTyName(tyname, tyname0) then
                                                                                        (subst, admitsEquality) (* do nothing *)
                                                                                    else
                                                                                        (U.TyNameMap.insert(subst, tyname, typeFn), admitsEquality orelse admitsEquality')
                                                                                  | NONE => emitError(ctx, [span], "sharing: type alias is invalid")
                                                                               )
                                                                         | NONE => emitError(ctx, [span], "sharing: type alias is invalid")
                                                                   ) (U.TyNameMap.empty, admitsEquality) tystrs
                      in { s = applySubstTyConInSig(ctx, subst) (#s s)
                         , bound = U.TyNameMap.mapPartiali (fn (tyname, x as { arity, admitsEquality = _, longtycon }) =>
                                                               if U.TyNameMap.inDomain(subst, tyname) then
                                                                   NONE
                                                               else if U.eqTyName(tyname, tyname0) then
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
and collectLongTyCons(ctx, strids : Syntax.StrId list, { valMap = _, tyConMap, strMap } : U.Signature) : Syntax.LongTyConSet.set
    = let val set = Syntax.TyConMap.foldli (fn (tycon, _, set) => Syntax.LongTyConSet.add(set, Syntax.MkQualified(strids, tycon))) Syntax.LongTyConSet.empty tyConMap
      in Syntax.StrIdMap.foldli (fn (strid, U.MkSignature s, set) =>
                                    let val set' = collectLongTyCons(ctx, strids @ [strid], s)
                                    in Syntax.LongTyConSet.union (set, set')
                                    end
                                ) set strMap
      end

fun sameType(U.TyVar(span1, tv), U.TyVar(span2, tv')) = tv = tv'
  | sameType(U.RecordType(span1, fields), U.RecordType(span2, fields')) = Syntax.LabelMap.numItems fields = Syntax.LabelMap.numItems fields'
                                                                          andalso Syntax.LabelMap.alli (fn (label, ty) => case Syntax.LabelMap.find (fields', label) of
                                                                                                                              SOME ty' => sameType(ty, ty')
                                                                                                                            | NONE => false
                                                                                                       ) fields
  | sameType(U.TyCon(span1, tyargs, tycon), U.TyCon(span2, tyargs', tycon')) = U.eqTyName(tycon, tycon') andalso (ListPair.allEq sameType (tyargs, tyargs') handle ListPair.UnequalLengths => false)
  | sameType(U.FnType(span1, ty1, ty2), U.FnType(span2, ty1', ty2')) = sameType(ty1, ty1') andalso sameType(ty2, ty2')
  | sameType(_, _) = false

fun sameTypeScheme(ctx, span, U.TypeScheme(tyvarsE, tyE), U.TypeScheme(tyvarsA, tyA))
    = if List.length tyvarsE = List.length tyvarsA then
          let val tyvars = List.map (fn _ => freshTyVar(ctx)) tyvarsE
              (* constraints are ignored *)
              val substE = ListPair.foldlEq (fn ((tv, _), tv', m) => U.TyVarMap.insert(m, tv, U.TyVar(span, tv'))) U.TyVarMap.empty (tyvarsE, tyvars)
              val substA = ListPair.foldlEq (fn ((tv, _), tv', m) => U.TyVarMap.insert(m, tv, U.TyVar(span, tv'))) U.TyVarMap.empty (tyvarsA, tyvars)
              val tyE = applySubstTy substE tyE
              val tyA = applySubstTy substA tyA
          in sameType(tyE, tyA)
          end
      else
          false

fun matchQSignature(ctx : Context, env : Env, span : SourcePos.span, expected : U.QSignature, strid : U.StrId, actual : U.Signature) : U.Signature * U.StrExp
    = let val env' = env (* addSignatureToEnv(envToSigEnv env, actual) *)
          val instantiation = USyntax.TyNameMap.map (fn { arity, admitsEquality, longtycon as Syntax.MkQualified(strids, tycon) } =>
                                                        let val { typeFunction as U.TypeFunction(tyvars, actualTy), ... }
                                                                = let val s = lookupStr(ctx, actual, span, strids)
                                                                  in case Syntax.TyConMap.find(#tyConMap s, tycon) of
                                                                         SOME tystr => tystr
                                                                       | NONE => emitError(ctx, [span], "signature matching: type not found: " ^ Syntax.print_LongTyCon longtycon)
                                                                  end
                                                            val () = if List.length tyvars = arity then
                                                                         () (* OK *)
                                                                     else
                                                                         emitError(ctx, [span], "signature matching: arity mismatch (" ^ Syntax.print_LongTyCon longtycon ^ ")")
                                                            val () = if admitsEquality andalso not (checkEquality (ctx, env', U.TyVarSet.fromList tyvars) actualTy) then
                                                                         emitError(ctx, [span], "signature matching: equality mismatch (" ^ Syntax.print_LongTyCon longtycon ^ ")")
                                                                     else
                                                                         ()
                                                        in typeFunction
                                                        end
                                                    ) (#bound expected)
          val instantiated = applySubstTyConInSig (ctx, instantiation) (#s expected)
      in matchSignature(ctx, env, span, instantiated, U.MkLongStrId(strid, []), actual)
      end
and matchSignature(ctx, env, span, expected : U.Signature, longstrid : U.LongStrId, actual : U.Signature) : U.Signature * U.StrExp
    = let val strMap : (U.Signature * U.StrExp) S.StrIdMap.map
              = Syntax.StrIdMap.mapi (fn (strid, U.MkSignature s) =>
                                         case Syntax.StrIdMap.find(#strMap actual, strid) of
                                             SOME (U.MkSignature s') => let val longstrid' = case longstrid of
                                                                                                 U.MkLongStrId(strid0, strids0) => U.MkLongStrId(strid0, strids0 @ [strid])
                                                                        in matchSignature(ctx, env, span, s, longstrid', s')
                                                                        end
                                           | NONE => emitError(ctx, [span], "signature matching: structure not found (" ^ Syntax.print_StrId strid ^ ")")
                                     ) (#strMap expected)
          val tyConMap : U.TypeStructure S.TyConMap.map
              = Syntax.TyConMap.mapi (fn (tycon, expectedTyStr) =>
                                         case Syntax.TyConMap.find (#tyConMap actual, tycon) of
                                             SOME actualTyStr => matchTyDesc(ctx, env, span, expectedTyStr, actualTyStr)
                                           | NONE => emitError(ctx, [span], "signature matching: " ^ (case tycon of Syntax.MkTyCon name => name) ^ " not found")
                                     ) (#tyConMap expected)
          val valMap : (U.TypeScheme * U.Dec list * U.LongVId * Syntax.IdStatus) S.VIdMap.map
              = Syntax.VIdMap.mapi (fn (vid, (tyscE, idsE)) =>
                                       case Syntax.VIdMap.find (#valMap actual, vid) of
                                           SOME (tyscA, idsA) => let val longvid = case longstrid of
                                                                                       U.MkLongStrId(strid0, strids0) => U.MkLongVId(strid0, strids0, vid)
                                                                     val (tysc, decs, longvid') = matchValDesc(ctx, env, span, tyscE, longvid, tyscA, idsA)
                                                                     val () = if idsE = Syntax.ExceptionConstructor andalso idsA <> Syntax.ExceptionConstructor then
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
                                                               val decs = U.StrBindDec(span, strid', strexp, { s = s, bound = [] }) :: decs
                                                           in (decs, Syntax.StrIdMap.insert(strMap, strid, U.MkLongStrId(strid', [])))
                                                           end
                                                       ) ([], Syntax.StrIdMap.empty) strMap
          val (decs, valMap) = Syntax.VIdMap.foldli (fn (vid, (tysc, decs, longvid, ids), (decs', valMap)) =>
                                                        let val decs = List.foldr (fn (dec, decs) => U.CoreDec(span, dec) :: decs) decs' decs
                                                        in (decs, Syntax.VIdMap.insert(valMap, vid, (longvid, ids)))
                                                        end
                                                    ) (decs, Syntax.VIdMap.empty) valMap
          val strexp = U.StructExp { sourceSpan = span
                                   , valMap = valMap
                                   , tyConMap = tyConMap
                                   , strMap = strMap
                                   }
      in (expected, if List.null decs then
                        strexp
                    else
                        U.LetInStrExp(span, decs, strexp)
         )
      end
and matchTyDesc(ctx, env, span, expected : U.TypeStructure, actual : U.TypeStructure) : U.TypeStructure
    = let val { typeFunction = U.TypeFunction(tyvarsE, tyE), valEnv = valEnvE } = expected
          val numE = Syntax.VIdMap.numItems valEnvE
          val { typeFunction = U.TypeFunction(tyvarsA, tyA), valEnv = valEnvA } = actual
      in if List.length tyvarsE = List.length tyvarsA then
             let val tyvars = List.map (fn _ => freshTyVar(ctx)) tyvarsE
                 val substE = ListPair.foldlEq (fn (tv, tv', m) => U.TyVarMap.insert(m, tv, U.TyVar(span, tv'))) U.TyVarMap.empty (tyvarsE, tyvars)
                 val substA = ListPair.foldlEq (fn (tv, tv', m) => U.TyVarMap.insert(m, tv, U.TyVar(span, tv'))) U.TyVarMap.empty (tyvarsA, tyvars)
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
and matchValDesc(ctx, env, span, expected : U.TypeScheme, longvid : U.LongVId, actual : U.TypeScheme, ids : Syntax.IdStatus) : U.TypeScheme * U.Dec list * U.LongVId
    = let val U.TypeScheme (tyvarsE, tyE) = expected
          val ictx = { context = ctx
                     , tyVarConstraints = ref USyntax.TyVarMap.empty
                     , tyVarSubst = ref USyntax.TyVarMap.empty
                     }
          val (tyA, tyargsA) = instantiate (ictx, span, actual)
          val () = addConstraint (ictx, env, U.EqConstr (span, tyE, tyA))
          val subst = !(#tyVarSubst ictx)
          val doTy = USyntax.applySubstTy subst
          val vid = newVId (ctx, case longvid of
                                     U.MkShortVId (U.MkVId (name, _)) => Syntax.MkVId name
                                   | U.MkLongVId (_, _, vid) => vid
                           )
          val tyargsA = List.map (fn (ty, c) => (doTy ty, c)) tyargsA
          val trivial = ListPair.allEq (fn ((tvE, []), (U.TyVar (span2, tvA), [])) => U.eqUTyVar (tvE, tvA)
                                       | ((tvE, [U.IsEqType _]), (U.TyVar (span2, tvA), [U.IsEqType _])) => U.eqUTyVar (tvE, tvA)
                                       | _ => false
                                       ) (tyvarsE, tyargsA)
      in if trivial then
             (expected, [], longvid) (* includes the case where ids = ExceptionConstructor *)
         else
             let val dec = U.ValDec (span, [U.PolyVarBind (span, vid, expected, U.VarExp (span, longvid, ids, tyargsA))])
             in (expected, [dec], U.MkShortVId vid)
             end
      end

fun typeCheckStrExp(ctx : Context, env : Env, S.StructExp(span, decs)) : U.PackedSignature * TyNameAttr U.TyNameMap.map * U.StrDec list * U.StrExp
    = let val ({ valMap, tyConMap, tyNameMap, strMap, ... }, decs) = typeCheckStrDecs(ctx, env, decs)
          val s = { s = { valMap = Syntax.VIdMap.map (fn (tysc, ids, _) => (tysc, ids)) valMap
                        , tyConMap = tyConMap
                        , strMap = Syntax.StrIdMap.map (fn (s, _) => U.MkSignature s) strMap
                        }
                  , bound = []
                  }
          val e = U.StructExp { sourceSpan = span
                              , valMap = Syntax.VIdMap.map (fn (_, ids, longvid) => (longvid, ids)) valMap
                              , tyConMap = tyConMap
                              , strMap = Syntax.StrIdMap.map (fn (_, longstrid) => longstrid) strMap
                              }
      in (s, tyNameMap, decs, e)
      end
  | typeCheckStrExp(ctx, env, S.StrIdExp(span, longstrid))
    = (case longstrid of
           Syntax.MkQualified([], strid) => (case Syntax.StrIdMap.find(#strMap env, strid) of
                                                 SOME (s, longstrid) => ({ s = s, bound = [] }, USyntax.TyNameMap.empty, [], U.StrIdExp(span, longstrid))
                                               | NONE => emitError (ctx, [span], "structure not found")
                                            )
         | Syntax.MkQualified(strid0 :: strids, strid') =>
           (case Syntax.StrIdMap.find(#strMap env, strid0) of
                SOME (s, U.MkLongStrId(strid0, strids0)) =>
                let val s' = lookupStr (ctx, s, span, strids @ [strid'])
                in ({ s = s', bound = [] }, USyntax.TyNameMap.empty, [], U.StrIdExp(span, U.MkLongStrId(strid0, strids0 @ strids @ [strid'])))
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
                     , tyNameMap = USyntax.TyNameMap.unionWith #2 (#tyNameMap env, tyNameMap)
                     , strMap = #strMap env
                     , sigMap = #sigMap env
                     , funMap = #funMap env
                     , boundTyVars = #boundTyVars env
                     }
          val (s, strexp') = matchQSignature(ctx, env', span, sE, strid, #s sA)
      in ({ s = s, bound = [] }, tyNameMap, decs @ [U.StrBindDec(span, strid, strexp, sA)], strexp')
      end
  | typeCheckStrExp(ctx, env, S.OpaqueConstraintExp(span, strexp, sigexp))
    = let val (sA, tyNameMap, decs, strexp) = typeCheckStrExp(ctx, env, strexp)
          val sE = evalSignature(ctx, envToSigEnv env, sigexp)
          val env' = { valMap = #valMap env
                     , tyConMap = #tyConMap env
                     , tyNameMap = USyntax.TyNameMap.unionWith #2 (#tyNameMap env, tyNameMap)
                     , strMap = #strMap env
                     , sigMap = #sigMap env
                     , funMap = #funMap env
                     , boundTyVars = #boundTyVars env
                     }
          val strid = newStrId(ctx, Syntax.MkStrId "tmp")
          val (s, strexp') = matchQSignature(ctx, env', span, sE, strid, #s sA)
          val tynames = canonicalOrderForQSignature sE
          val packageSig = { s = #s sE
                           , bound = List.map (fn tyname => let val { arity, admitsEquality, longtycon } = U.TyNameMap.lookup(#bound sE, tyname)
                                                            in { tyname = tyname, arity = arity, admitsEquality = admitsEquality }
                                                            end
                                              ) tynames
                           }
          val tyNameMapOutside = U.TyNameMap.foldli (fn (tyname, { arity, admitsEquality, longtycon }, acc) =>
                                                        U.TyNameMap.insert (acc, tyname, { arity = arity, admitsEquality = admitsEquality, overloadClass = NONE })
                                                    ) U.TyNameMap.empty (#bound sE)
          val payloadTypes = List.map (fn tyname => let val { longtycon = Syntax.MkQualified (strids, tycon), ... } = U.TyNameMap.lookup (#bound sE, tyname)
                                                        val { tyConMap, ... } = lookupStr (ctx, #s sA, span, strids)
                                                    in case Syntax.TyConMap.find (tyConMap, tycon) of
                                                           SOME { typeFunction, valEnv } => typeFunction
                                                         | NONE => emitError (ctx, [span], "unknown type constructor")
                                                    end) tynames
      in (packageSig, tyNameMapOutside, [], U.PackedStrExp { sourceSpan = span
                                                           , strExp = U.LetInStrExp(span, decs @ [U.StrBindDec(span, strid, strexp, sA)], strexp')
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
               val resultSig = let val subst = List.foldl (fn ({ tyname, ... }, map) => U.TyNameMap.insert (map, tyname, renewTyName (ctx, tyname))) U.TyNameMap.empty (#bound resultSig)
                               in { s = refreshTyNameInSig (ctx, subst) (#s resultSig)
                                  , bound = List.map (fn { tyname, arity, admitsEquality } => { tyname = U.TyNameMap.lookup (subst, tyname), arity = arity, admitsEquality = admitsEquality }) (#bound resultSig)
                                  }
                               end
               val (sA, tyNameMap, decs, strexp) = typeCheckStrExp (ctx, env, strexp)
               val strid = newStrId (ctx, Syntax.MkStrId "tmp")
               val env' = { valMap = #valMap env
                          , tyConMap = #tyConMap env
                          , tyNameMap = USyntax.TyNameMap.unionWith #2 (#tyNameMap env, tyNameMap)
                          , strMap = #strMap env
                          , sigMap = #sigMap env
                          , funMap = #funMap env
                          , boundTyVars = #boundTyVars env
                          }
               val argumentTypes = List.map (fn { tyname, arity, admitsEquality, longtycon as Syntax.MkQualified (strids, tycon) } =>
                                                let val { typeFunction as U.TypeFunction (tyvars, actualTy), ... }
                                                        = let val s = lookupStr (ctx, #s sA, span, strids)
                                                          in case Syntax.TyConMap.find (#tyConMap s, tycon) of
                                                                 SOME tystr => tystr
                                                               | NONE => emitError (ctx, [span], "signature matching: type not found: " ^ Syntax.print_LongTyCon longtycon)
                                                          end
                                                    val () = if List.length tyvars = arity then
                                                                 () (* OK *)
                                                             else
                                                                 emitError (ctx, [span], "signature matching: arity mismatch (" ^ Syntax.print_LongTyCon longtycon ^ ")")
                                                    val () = if admitsEquality andalso not (checkEquality (ctx, env', U.TyVarSet.fromList tyvars) actualTy) then
                                                                 emitError (ctx, [span], "signature matching: equality mismatch (" ^ Syntax.print_LongTyCon longtycon ^ ")")
                                                             else
                                                                 () (* OK *)
                                                in (tyname, typeFunction, admitsEquality)
                                                end
                                            ) bound
               val instantiation = List.foldl (fn ((tyname, typeFunction, admitsEquality), map) => U.TyNameMap.insert (map, tyname, typeFunction)) U.TyNameMap.empty argumentTypes
               val instantiated = applySubstTyConInSig (ctx, instantiation) paramSig
               val (_, strexp') = matchSignature (ctx, env', span, instantiated, U.MkLongStrId(strid, []), #s sA)
               val resultSig = { s = applySubstTyConInSig (ctx, instantiation) (#s resultSig)
                               , bound = #bound resultSig
                               }
               val tyNameMapOutside = List.foldl (fn ({ tyname, arity, admitsEquality }, map) =>
                                                     U.TyNameMap.insert (map, tyname, { arity = arity, admitsEquality = admitsEquality, overloadClass = NONE })
                                                 ) tyNameMap (#bound resultSig)
           in ( resultSig
              , tyNameMapOutside
              , decs @ [U.StrBindDec(span, strid, strexp, sA)]
              , U.FunctorAppExp { sourceSpan = span
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
                                                                      in (s, U.TyNameMap.unionWith #2 (#tyNameMap env', tyNameMap), strdecs @ strdecs', strexp)
                                                                      end
and typeCheckStrDec(ctx : Context, env : Env, S.CoreDec(span, dec)) : Env * USyntax.StrDec list
    = let val (env', decs) = typeCheckCoreDecs(ctx, env, [dec])
      in (env', List.map (fn dec => U.CoreDec(span, dec)) decs)
      end
  | typeCheckStrDec(ctx, env, S.StrBindDec(span, binds))
    = let val (strMap, tyNameMap, binds) = List.foldr (fn ((strid, strexp), (strMap, tyNameMap, binds)) =>
                                                          let val (ps, tc, strdecs, strexp) = typeCheckStrExp(ctx, env, strexp)
                                                              val strid' = newStrId(ctx, strid)
                                                          in (S.StrIdMap.insert(strMap, strid, (#s ps, U.MkLongStrId(strid', []))), USyntax.TyNameMap.unionWith #2 (tyNameMap, tc), (strid', strdecs, strexp, ps) :: binds)
                                                          end
                                                 ) (Syntax.StrIdMap.empty, USyntax.TyNameMap.empty, []) binds
          val env' = { valMap = Syntax.VIdMap.empty
                     , tyConMap = Syntax.TyConMap.empty
                     , tyNameMap = tyNameMap
                     , strMap = strMap
                     , sigMap = Syntax.SigIdMap.empty
                     , funMap = Syntax.FunIdMap.empty
                     , boundTyVars = Syntax.TyVarMap.empty
                     }
      in (env', List.foldr (fn ((strid, strdecs, strexp, s), strdecs') =>
                               let val decs = case strdecs @ [U.StrBindDec(span, strid, strexp, s)] of
                                                  decs as [] => decs
                                                | decs as [_] => decs
                                                | decs => [U.GroupStrDec(span, decs)]
                               in decs @ strdecs'
                               end) [] binds)
      end
  | typeCheckStrDec(ctx, env, S.LocalStrDec(span, decs1, decs2))
    = let val (env', decs1) = typeCheckStrDecs(ctx, env, decs1)
          val (env'', decs2) = typeCheckStrDecs(ctx, mergeEnv(env, env'), decs2)
          val env'' = { valMap = #valMap env''
                      , tyConMap = #tyConMap env''
                      , tyNameMap = USyntax.TyNameMap.unionWith #2 (#tyNameMap env', #tyNameMap env'')
                      , strMap = #strMap env''
                      , sigMap = #sigMap env''
                      , funMap = #funMap env''
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

fun typeCheckFunExp'(ctx, span, paramEnv, paramSig, strid, strexp) : USyntax.FunSig * USyntax.FunExp
    = let val (actualSignature : U.PackedSignature, bodyTyNameMap, strdecs, strexp) = typeCheckStrExp(ctx, paramEnv, strexp)
          val tynamesInParam = canonicalOrderForQSignature paramSig
          val stridTmp = newStrId(ctx, Syntax.MkStrId "tmp")
          val additionalTyNames = U.TyNameMap.foldli (fn (tyname, { arity, admitsEquality, overloadClass = _ }, xs) =>
                                                         if List.exists (fn { tyname = tyname', ... } => U.eqTyName(tyname, tyname')) (#bound actualSignature) then
                                                             xs
                                                         else
                                                             { tyname = tyname, arity = arity, admitsEquality = admitsEquality } :: xs
                                                     ) [] bodyTyNameMap
          val resultSig = { s = #s actualSignature
                          , bound = #bound actualSignature @ additionalTyNames
                          }
          val payloadTypes = List.map (fn { tyname, arity, admitsEquality } =>
                                          let val tyvars = List.tabulate (arity, fn _ => freshTyVar(ctx))
                                          in U.TypeFunction (tyvars, U.TyCon (span, List.map (fn tv => U.TyVar(span, tv)) tyvars, tyname))
                                          end) (#bound resultSig)
          val funsig = { bound = List.map (fn tyname => let val { arity, admitsEquality, longtycon } = U.TyNameMap.lookup(#bound paramSig, tyname)
                                                        in { tyname = tyname, arity = arity, admitsEquality = admitsEquality, longtycon = longtycon }
                                                        end
                                          ) tynamesInParam
                       , paramSig = #s paramSig
                       , resultSig = resultSig
                       }
          val funexp = ( List.map (fn tyname => let val { arity, admitsEquality, longtycon } = U.TyNameMap.lookup(#bound paramSig, tyname)
                                                in { tyname = tyname, arity = arity, admitsEquality = admitsEquality }
                                                end
                                  ) tynamesInParam
                       , strid
                       , #s paramSig
                       , U.LetInStrExp ( span
                                       , strdecs @ [U.StrBindDec (span, stridTmp, strexp, actualSignature)]
                                       , U.PackedStrExp { sourceSpan = span
                                                        , strExp = U.StrIdExp (span, U.MkLongStrId (stridTmp, []))
                                                        , payloadTypes = payloadTypes
                                                        , packageSig = resultSig
                                                        }
                                       )
                       )
      in (funsig, funexp)
      end
fun typeCheckFunExp(ctx, span, env, S.NamedFunExp (strid, sigexp, strexp)) : USyntax.FunSig * USyntax.FunExp
    = let val strid' = newStrId(ctx, strid)
          val paramSig : U.QSignature = evalSignature(ctx, envToSigEnv env, sigexp)
          val tyNameMap : TyNameAttr U.TyNameMap.map
              = U.TyNameMap.mapi (fn (tyname, { arity, admitsEquality, longtycon }) =>
                                     { arity = arity
                                     , admitsEquality = admitsEquality
                                     , overloadClass = NONE
                                     }
                                ) (#bound paramSig)
          val paramEnv = { valMap = #valMap env
                         , tyConMap = #tyConMap env
                         , tyNameMap = U.TyNameMap.unionWith #2 (#tyNameMap env, tyNameMap)
                         , strMap = S.StrIdMap.insert(#strMap env, strid, (#s paramSig, U.MkLongStrId(strid', [])))
                         , sigMap = #sigMap env
                         , funMap = #funMap env
                         , boundTyVars = #boundTyVars env
                         }
      in typeCheckFunExp'(ctx, span, paramEnv, paramSig, strid', strexp)
      end
  | typeCheckFunExp(ctx, span, env, S.AnonymousFunExp (sigexp, strexp))
    = let val strid0 = newStrId(ctx, S.MkStrId "param")
          val paramSig : U.QSignature = evalSignature(ctx, envToSigEnv env, sigexp)
          val tyNameMap : TyNameAttr U.TyNameMap.map
              = U.TyNameMap.mapi (fn (tyname, { arity, admitsEquality, longtycon }) =>
                                     { arity = arity
                                     , admitsEquality = admitsEquality
                                     , overloadClass = NONE
                                     }
                                 ) (#bound paramSig)
          val paramEnv = { valMap = Syntax.VIdMap.mapi (fn (vid, (tysc, ids)) => (tysc, ids, USyntax.MkLongVId (strid0, [], vid))) (#valMap (#s paramSig))
                         , tyConMap = #tyConMap (#s paramSig)
                         , tyNameMap = tyNameMap
                         , strMap = Syntax.StrIdMap.mapi (fn (strid, U.MkSignature s) => (s, U.MkLongStrId (strid0, [strid]))) (#strMap (#s paramSig))
                         , sigMap = Syntax.SigIdMap.empty
                         , funMap = Syntax.FunIdMap.empty
                         , boundTyVars = Syntax.TyVarMap.empty
                         }
      in typeCheckFunExp'(ctx, span, mergeEnv(env, paramEnv), paramSig, strid0, strexp)
      end

fun typeCheckTopDec(ctx, env, S.StrDec strdec) = let val (env', strdec) = typeCheckStrDec(ctx, env, strdec)
                                                 in (env', List.map U.StrDec strdec)
                                                 end
  | typeCheckTopDec(ctx, env, S.SigDec binds) = let val sigenv = envToSigEnv env
                                                    val sigMap = List.foldl (fn ((sigid, sigexp), m) => Syntax.SigIdMap.insert(m, sigid, evalSignature(ctx, sigenv, sigexp))) (#sigMap env) binds
                                                    val env = { valMap = Syntax.VIdMap.empty
                                                              , tyConMap = Syntax.TyConMap.empty
                                                              , tyNameMap = USyntax.TyNameMap.empty
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
                                               in (Syntax.FunIdMap.insert(funMap, funid, (funsig, funid')), U.FunDec(funid', funexp) :: binds)
                                               end
                                           ) (Syntax.FunIdMap.empty, []) binds
          val env = { valMap = Syntax.VIdMap.empty
                    , tyConMap = Syntax.TyConMap.empty
                    , tyNameMap = USyntax.TyNameMap.empty
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

(* typeCheckProgram : ProgramContext * Env * ((Syntax.Dec Syntax.TopDec) list) list -> Env * USyntax.TopDec list *)
fun typeCheckProgram(ctx, env, [] : ((Syntax.Dec Syntax.TopDec) list) list) : Env * (USyntax.TopDec list) list = (emptyEnv, [])
  | typeCheckProgram(ctx, env, topdec :: topdecs) = let val (env', topdec') = typeCheckTopDecs (ctx, env, topdec)
                                                        val (env'', topdecs') = typeCheckProgram(ctx, mergeEnv(env, env'), topdecs)
                                                    in (mergeEnv(env', env''), topdec' :: topdecs')
                                                    end
end (* local *)
end (* structure Typing *)
