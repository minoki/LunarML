(*
 * Copyright (c) 2022 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure Typing :>
sig
  val VId_Bind: TypedSyntax.VId
  val VId_ref: TypedSyntax.VId
  val VId_DCOLON: TypedSyntax.VId
  val VId_unit_equal: TypedSyntax.VId
  val isRefOrArray: TypedSyntax.TyName -> bool
  type TyNameAttr =
    { arity: int
    , admitsEquality: bool
    , overloadClass: Syntax.OverloadClass option
    }
  type ('val, 'str) Env' =
    { valMap:
        (TypedSyntax.TypeScheme
         * Syntax.ValueConstructorInfo Syntax.IdStatus
         * 'val) Syntax.VIdMap.map
    , tyConMap: TypedSyntax.TypeStructure Syntax.TyConMap.map
    , tyNameMap: TyNameAttr TypedSyntax.TyNameMap.map
    , strMap: (TypedSyntax.Signature * 'str) Syntax.StrIdMap.map
    , sigMap: TypedSyntax.QSignature Syntax.SigIdMap.map
    , funMap: (TypedSyntax.FunSig * TypedSyntax.FunId) Syntax.FunIdMap.map
    , boundTyVars: TypedSyntax.TyVar Syntax.TyVarMap.map
    }
  type Env = (TypedSyntax.LongVId, TypedSyntax.LongStrId) Env'
  type SigEnv = (unit, unit) Env'
  val emptyEnv: Env
  val mergeEnv: ('val, 'str) Env' * ('val, 'str) Env' -> ('val, 'str) Env'
  val envWithStrMap: (TypedSyntax.Signature * 'str) Syntax.StrIdMap.map
                     -> ('val, 'str) Env'
  val envWithSigMap: TypedSyntax.QSignature Syntax.SigIdMap.map
                     -> ('val, 'str) Env'
  val envWithFunMap:
    (TypedSyntax.FunSig * TypedSyntax.FunId) Syntax.FunIdMap.map
    -> ('val, 'str) Env'
  val newTyVarCounter: unit -> int ref
  val newVIdCounter: unit -> int ref
  type Context =
    { nextTyVar: int ref
    , nextVId: int ref
    , messageHandler: Message.handler
    , matchContext: Syntax.VId list
    , languageOptions: LanguageOptions.options
    }
  val checkTyScopeOfProgram:
    Context * TypedSyntax.TyNameSet.set * TypedSyntax.Program
    -> TypedSyntax.TyNameSet.set
  val typeCheckProgram: Context * Env * ((Syntax.Dec Syntax.TopDec) list) list
                        -> Env * (TypedSyntax.TopDec list) list
end =
struct

  type TyNameAttr =
    { arity: int
    , admitsEquality: bool
    , overloadClass: Syntax.OverloadClass option
    }

  type ('val, 'str) Env' =
    { valMap:
        (TypedSyntax.TypeScheme
         * Syntax.ValueConstructorInfo Syntax.IdStatus
         * 'val) Syntax.VIdMap.map
    , tyConMap: TypedSyntax.TypeStructure Syntax.TyConMap.map
    , tyNameMap: TyNameAttr TypedSyntax.TyNameMap.map
    , strMap: (TypedSyntax.Signature * 'str) Syntax.StrIdMap.map
    , sigMap: TypedSyntax.QSignature Syntax.SigIdMap.map
    , funMap: (TypedSyntax.FunSig * TypedSyntax.FunId) Syntax.FunIdMap.map
    , boundTyVars: TypedSyntax.TyVar Syntax.TyVarMap.map
    }
  type Env = (TypedSyntax.LongVId, TypedSyntax.LongStrId) Env'
  type SigEnv = (unit, unit) Env'

  val emptyEnv: Env =
    { valMap = Syntax.VIdMap.empty
    , tyConMap = Syntax.TyConMap.empty
    , tyNameMap = TypedSyntax.TyNameMap.empty
    , strMap = Syntax.StrIdMap.empty
    , sigMap = Syntax.SigIdMap.empty
    , funMap = Syntax.FunIdMap.empty
    , boundTyVars = Syntax.TyVarMap.empty
    }

  (*: val mergeEnv : ('val, 'str) Env' * ('val, 'str) Env' -> ('val, 'str) Env' *)
  fun mergeEnv (env1: ('val, 'str) Env', env2: ('val, 'str) Env') :
    ('val, 'str) Env' =
    { valMap = Syntax.VIdMap.unionWithSecond (#valMap env1, #valMap env2)
    , tyConMap =
        Syntax.TyConMap.unionWithSecond (#tyConMap env1, #tyConMap env2)
    , tyNameMap =
        TypedSyntax.TyNameMap.unionWithSecond (#tyNameMap env1, #tyNameMap env2)
    , strMap = Syntax.StrIdMap.unionWithSecond (#strMap env1, #strMap env2)
    , sigMap = Syntax.SigIdMap.unionWithSecond (#sigMap env1, #sigMap env2)
    , funMap = Syntax.FunIdMap.unionWithSecond (#funMap env1, #funMap env2)
    , boundTyVars =
        Syntax.TyVarMap.unionWithSecond (#boundTyVars env1, #boundTyVars env2)
    }

  fun envWithValEnv valMap : Env =
    { valMap = valMap
    , tyConMap = Syntax.TyConMap.empty
    , tyNameMap = TypedSyntax.TyNameMap.empty
    , strMap = Syntax.StrIdMap.empty
    , sigMap = Syntax.SigIdMap.empty
    , funMap = Syntax.FunIdMap.empty
    , boundTyVars = Syntax.TyVarMap.empty
    }

  fun mergeWithValEnv (env1: ('val, 'str) Env', valMap) : ('val, 'str) Env' =
    { valMap = Syntax.VIdMap.unionWithSecond (#valMap env1, valMap)
    , tyConMap = #tyConMap env1
    , tyNameMap = #tyNameMap env1
    , strMap = #strMap env1
    , sigMap = #sigMap env1
    , funMap = #funMap env1
    , boundTyVars = #boundTyVars env1
    }

  fun envWithTyConEnv (tyConMap, tyNameMap) : ('val, 'str) Env' =
    { valMap = Syntax.VIdMap.empty
    , tyConMap = tyConMap
    , tyNameMap = tyNameMap
    , strMap = Syntax.StrIdMap.empty
    , sigMap = Syntax.SigIdMap.empty
    , funMap = Syntax.FunIdMap.empty
    , boundTyVars = Syntax.TyVarMap.empty
    }

  fun mergeWithTyConEnv (env1: ('val, 'str) Env', {tyConMap, tyNameMap}) :
    ('val, 'str) Env' =
    { valMap = #valMap env1
    , tyConMap = Syntax.TyConMap.unionWithSecond (#tyConMap env1, tyConMap)
    , tyNameMap =
        TypedSyntax.TyNameMap.unionWithSecond (#tyNameMap env1, tyNameMap)
    , strMap = #strMap env1
    , sigMap = #sigMap env1
    , funMap = #funMap env1
    , boundTyVars = #boundTyVars env1
    }

  fun envWithStrMap strMap =
    { valMap = Syntax.VIdMap.empty
    , tyConMap = Syntax.TyConMap.empty
    , tyNameMap = TypedSyntax.TyNameMap.empty
    , strMap = strMap
    , sigMap = Syntax.SigIdMap.empty
    , funMap = Syntax.FunIdMap.empty
    , boundTyVars = Syntax.TyVarMap.empty
    }

  fun envWithSigMap sigMap =
    { valMap = Syntax.VIdMap.empty
    , tyConMap = Syntax.TyConMap.empty
    , tyNameMap = TypedSyntax.TyNameMap.empty
    , strMap = Syntax.StrIdMap.empty
    , sigMap = sigMap
    , funMap = Syntax.FunIdMap.empty
    , boundTyVars = Syntax.TyVarMap.empty
    }

  fun envWithFunMap funMap =
    { valMap = Syntax.VIdMap.empty
    , tyConMap = Syntax.TyConMap.empty
    , tyNameMap = TypedSyntax.TyNameMap.empty
    , strMap = Syntax.StrIdMap.empty
    , sigMap = Syntax.SigIdMap.empty
    , funMap = funMap
    , boundTyVars = Syntax.TyVarMap.empty
    }

  fun envToSigEnv (env: Env) : SigEnv =
    { valMap =
        Syntax.VIdMap.map (fn (tysc, ids, _) => (tysc, ids, ())) (#valMap env)
    , tyConMap = #tyConMap env
    , tyNameMap = #tyNameMap env
    , strMap = Syntax.StrIdMap.map (fn (s, _) => (s, ())) (#strMap env)
    , sigMap = #sigMap env
    , funMap = #funMap env
    , boundTyVars = #boundTyVars env
    }

  fun freeTyVarsInEnv
    ( _ (* bound *)
    , { valMap = _
      , tyConMap = _
      , tyNameMap = _
      , strMap = _
      , sigMap = _
      , funMap = _
      , boundTyVars
      }: Env
    ) =
    Syntax.TyVarMap.foldl (fn (tv, set) => TypedSyntax.TyVarSet.add (set, tv))
      TypedSyntax.TyVarSet.empty boundTyVars

  type Context =
    { nextTyVar: int ref
    , nextVId: int ref
    , messageHandler: Message.handler
    , matchContext: Syntax.VId list
    , languageOptions: LanguageOptions.options
    }

  type InferenceContext = {context: Context, level: TypedSyntax.level}

  fun enterLevel ({context, level}: InferenceContext) : InferenceContext =
    {context = context, level = level + 1}

  fun emitError (ctx: Context, spans, message) =
    let
      val {matchContext, messageHandler, ...} = ctx
      val message =
        List.foldl
          (fn (vid, message) =>
             message ^ " during matching " ^ Syntax.print_VId vid) message
          matchContext
    in
      Message.error (messageHandler, spans, "type", message)
    end
  fun emitTypeError (ctx: InferenceContext, spans, message) =
    emitError (#context ctx, spans, message)
  fun emitFatalError (ctx: Context, spans, message) =
    let
      val {matchContext, messageHandler, ...} = ctx
      val message =
        List.foldl
          (fn (vid, message) =>
             message ^ " during matching " ^ Syntax.print_VId vid) message
          matchContext
    in
      Message.fatalError (messageHandler, spans, "type", message)
    end
  fun emitFatalTypeError (ctx: InferenceContext, spans, message) =
    emitFatalError (#context ctx, spans, message)

  (*: val lookupStr : Context * 'l TypedSyntax.BaseSignature * SourcePos.span * Syntax.StrId list -> 'l TypedSyntax.BaseSignature *)
  fun lookupStr (_, s: 'l TypedSyntax.BaseSignature, _, nil) = s
    | lookupStr
        ( ctx
        , {strMap = strMap, ...}
        , span
        , (strid0 as Syntax.MkStrId name) :: strids
        ) =
        (case Syntax.StrIdMap.find (strMap, strid0) of
           NONE =>
             emitFatalError
               (ctx, [span], "unknown structure name '" ^ name ^ "'")
         | SOME (TypedSyntax.MkSignature innerEnv) =>
             lookupStr (ctx, innerEnv, span, strids))
  (*: val lookupTyConInEnv : Context * ('val, 'str) Env' * SourcePos.span * Syntax.LongTyCon -> TypedSyntax.TypeStructure *)
  fun lookupTyConInEnv
        ( ctx
        , env: ('val, 'str) Env'
        , span
        , Syntax.MkQualified ([], tycon as Syntax.MkTyCon name)
        ) =
        (case Syntax.TyConMap.find (#tyConMap env, tycon) of
           SOME tystr => tystr
         | NONE =>
             emitFatalError
               (ctx, [span], "unknown type constructor '" ^ name ^ "'"))
    | lookupTyConInEnv
        ( ctx
        , env
        , span
        , Syntax.MkQualified (strid0 :: strids, tycon as Syntax.MkTyCon name)
        ) =
        (case Syntax.StrIdMap.find (#strMap env, strid0) of
           SOME (s, _) =>
             (case lookupStr (ctx, s, span, strids) of
                {tyConMap, ...} =>
                  case Syntax.TyConMap.find (tyConMap, tycon) of
                    SOME tystr => tystr
                  | NONE =>
                      emitFatalError
                        (ctx, [span], "unknown type constructor '" ^ name ^ "'"))
         | NONE =>
             emitFatalError
               ( ctx
               , [span]
               , "unknown structure name '"
                 ^ (case strid0 of Syntax.MkStrId name => name) ^ "'"
               ))
  fun lookupTyNameInEnv (ctx, {tyNameMap, ...}: ('val, 'str) Env', span, tyname) =
    (case TypedSyntax.TyNameMap.find (tyNameMap, tyname) of
       SOME attr => attr
     | NONE =>
         emitFatalError
           ( ctx
           , [span]
           , "unknown type constructor " ^ TypedSyntax.print_TyName tyname
             ^ " (internal error)"
           ))

  datatype 'a LookupResult =
    Found of 'a
  | ValueNotFound of Syntax.VId Syntax.Qualified
  | StructureNotFound of Syntax.StrId Syntax.Qualified

  (*: val lookupStr' : 'context * 'l TypedSyntax.BaseSignature * Syntax.StrId list * Syntax.StrId list -> ('l TypedSyntax.BaseSignature) LookupResult *)
  fun lookupStr' (_, s: 'l TypedSyntax.BaseSignature, _, nil) = Found s
    | lookupStr' (ctx, {strMap, ...}, revStrIds, strid0 :: strids) =
        (case Syntax.StrIdMap.find (strMap, strid0) of
           NONE =>
             StructureNotFound (Syntax.MkQualified (List.rev revStrIds, strid0))
         | SOME (TypedSyntax.MkSignature innerEnv) =>
             lookupStr' (ctx, innerEnv, strid0 :: revStrIds, strids))

  (*: val lookupLongVIdInEnv : 'context * Env * 'span * Syntax.LongVId -> (TypedSyntax.LongVId * TypedSyntax.TypeScheme * Syntax.ValueConstructorInfo Syntax.IdStatus) LookupResult *)
  fun lookupLongVIdInEnv
        ( _: 'context
        , env: Env
        , _ (* span *)
        , longvid as Syntax.MkQualified ([], vid)
        ) =
        (case Syntax.VIdMap.find (#valMap env, vid) of
           SOME (tysc, ids, longvid) => Found (longvid, tysc, ids)
         | NONE => ValueNotFound longvid)
    | lookupLongVIdInEnv
        (ctx, env, _, longvid as Syntax.MkQualified (strid0 :: strids, vid)) =
        (case Syntax.StrIdMap.find (#strMap env, strid0) of
           SOME (s, TypedSyntax.MkLongStrId (strid0, strids0)) =>
             (case lookupStr' (ctx, s, [], strids) of
                Found {valMap, ...} =>
                  (case Syntax.VIdMap.find (valMap, vid) of
                     SOME (tysc, ids) =>
                       Found
                         ( TypedSyntax.MkLongVId (strid0, strids0 @ strids, vid)
                         , tysc
                         , ids
                         )
                   | NONE => ValueNotFound longvid)
              | StructureNotFound notfound => StructureNotFound notfound
              | ValueNotFound notfound =>
                  ValueNotFound notfound (* cannot occur *))
         | NONE => StructureNotFound (Syntax.MkQualified ([], strid0)))

  (*: val getConstructedType : Context * SourcePos.span * 'l TypedSyntax.BaseTy -> TypedSyntax.TyName *)
  fun getConstructedType (ctx, span, TypedSyntax.TyVar _) =
        emitFatalError (ctx, [span], "getConstructedType: got a type variable")
    | getConstructedType (ctx, span, TypedSyntax.AnonymousTyVar _) =
        emitFatalError (ctx, [span], "getConstructedType: got a type variable")
    | getConstructedType (ctx, span, TypedSyntax.RecordType _) =
        emitFatalError (ctx, [span], "getConstructedType: got a record")
    | getConstructedType (_, _, TypedSyntax.TyCon (_, _, tycon)) = tycon
    | getConstructedType (ctx, span, TypedSyntax.FnType (_, _, t)) =
        getConstructedType (ctx, span, t)
    | getConstructedType (ctx, span, TypedSyntax.RecordExtType _) =
        emitFatalError (ctx, [span], "getConstructedType: got a record")

  (* The Definition, 4.7 Non-expansive Expressions *)
  (*: val isNonexpansive : Env * TypedSyntax.Exp -> bool *)
  fun isNonexpansive (_: Env, TypedSyntax.SConExp _) = true
    | isNonexpansive (_, TypedSyntax.VarExp _) = true (* <op> longvid *)
    | isNonexpansive (env, TypedSyntax.RecordExp (_, fields)) =
        List.all (fn (_, e) => isNonexpansive (env, e)) fields
    | isNonexpansive (env, TypedSyntax.TypedExp (_, e, _)) =
        isNonexpansive (env, e)
    | isNonexpansive (env, TypedSyntax.AppExp (_, conexp, e)) =
        isConexp (env, conexp) andalso isNonexpansive (env, e)
    | isNonexpansive (_, TypedSyntax.FnExp _) = true
    | isNonexpansive (_, TypedSyntax.ProjectionExp _) = true
    | isNonexpansive (env, TypedSyntax.ListExp (_, xs, _)) =
        Vector.all (fn x => isNonexpansive (env, x)) xs
    | isNonexpansive (env, TypedSyntax.VectorExp (_, xs, _)) =
        Vector.all (fn x => isNonexpansive (env, x)) xs
    | isNonexpansive (_, _) = false
  and isConexp (env: Env, TypedSyntax.TypedExp (_, e, _)) = isConexp (env, e)
    | isConexp (_, TypedSyntax.VarExp (_, _, Syntax.ValueVariable, _)) = false
    | isConexp
        ( _
        , TypedSyntax.VarExp
            ( _
            , TypedSyntax.MkShortVId (TypedSyntax.MkVId (name, _))
            , Syntax.ValueConstructor _
            , _
            )
        ) =
        Syntax.SourceName.getStringWithDefault (name, "") <> "ref"
    | isConexp
        ( _
        , TypedSyntax.VarExp
            ( _
            , TypedSyntax.MkLongVId (_, _, Syntax.MkVId name)
            , Syntax.ValueConstructor _
            , _
            )
        ) = name <> "ref"
    | isConexp (_, TypedSyntax.VarExp (_, _, Syntax.ExceptionConstructor, _)) =
        true
    | isConexp (_, _) = false

  (*: val isExhaustive : 'context * Env * TypedSyntax.Pat -> bool *)
  fun isExhaustive (_, _: Env, TypedSyntax.WildcardPat _) = true
    | isExhaustive (_, _, TypedSyntax.SConPat _) = false
    | isExhaustive (_, _, TypedSyntax.VarPat _) = true
    | isExhaustive
        ( ctx
        , env
        , TypedSyntax.RecordPat
            {sourceSpan = _, fields, ellipsis = NONE, wholeRecordType = _}
        ) =
        List.all (fn (_, e) => isExhaustive (ctx, env, e)) fields
    | isExhaustive
        ( ctx
        , env
        , TypedSyntax.RecordPat
            { sourceSpan = _
            , fields
            , ellipsis = SOME ellipsisPat
            , wholeRecordType = _
            }
        ) =
        List.all (fn (_, e) => isExhaustive (ctx, env, e)) fields
        andalso isExhaustive (ctx, env, ellipsisPat)
    | isExhaustive
        ( _
        , _
        , TypedSyntax.ConPat
            { sourceSpan = _
            , longvid = _
            , payload = NONE
            , tyargs = _
            , valueConstructorInfo = SOME info
            }
        ) =
        Syntax.VIdSet.numItems (#allConstructors info) = 1
    | isExhaustive
        ( ctx
        , env
        , TypedSyntax.ConPat
            { sourceSpan = _
            , longvid = _
            , payload = SOME (_, innerPat)
            , tyargs = _
            , valueConstructorInfo = SOME info
            }
        ) =
        Syntax.VIdSet.numItems (#allConstructors info) = 1
        andalso isExhaustive (ctx, env, innerPat)
    | isExhaustive
        ( _
        , _
        , TypedSyntax.ConPat
            { sourceSpan = _
            , longvid = _
            , payload = _
            , tyargs = _
            , valueConstructorInfo = NONE
            }
        ) = false
    | isExhaustive (ctx, env, TypedSyntax.TypedPat (_, innerPat, _)) =
        isExhaustive (ctx, env, innerPat)
    | isExhaustive (ctx, env, TypedSyntax.LayeredPat (_, _, _, innerPat)) =
        isExhaustive (ctx, env, innerPat)
    | isExhaustive (_, _, TypedSyntax.VectorPat (_, pats, ellipsis, _)) =
        ellipsis andalso Vector.length pats = 0
    | isExhaustive (_, _, TypedSyntax.BogusPat _) = false

  val VId_Bind = TypedSyntax.MkVId (Syntax.SourceName.fromString "Bind", ~1)

  (* Index of user-defined identifiers start with 100 *)
  val VId_ref = TypedSyntax.MkVId (Syntax.SourceName.fromString "ref", 0)
  val VId_DCOLON = TypedSyntax.MkVId (Syntax.SourceName.fromString "::", 1)
  val VId_unit_equal =
    TypedSyntax.MkVId (Syntax.SourceName.fromString "unit_equal", 2)

  fun isRefOrArray (tyname: TypedSyntax.TyName) =
    TypedSyntax.eqTyName (tyname, PrimTypes.Names.ref_)
    orelse TypedSyntax.eqTyName (tyname, PrimTypes.Names.array)

  structure TypeOfPrimitives =
    TypeOfPrimitives
      (type ty = TypedSyntax.Ty
       type tv = TypedSyntax.TyVar
       type constraint = bool (* equality? *)
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
       val unit = PrimTypes.unit
       val bool = PrimTypes.bool
       val int = PrimTypes.int
       val word = PrimTypes.word
       val real = PrimTypes.real
       val char = PrimTypes.char
       val char16 = PrimTypes.char16
       val string = PrimTypes.string
       val string16 = PrimTypes.string16
       val intInf = PrimTypes.intInf
       val int32 = PrimTypes.int32
       val int54 = PrimTypes.int54
       val int64 = PrimTypes.int64
       val word32 = PrimTypes.word32
       val word64 = PrimTypes.word64
       val exn = PrimTypes.exn
       val exntag = PrimTypes.exntag
       val LuaValue = PrimTypes.lua_value
       val JavaScriptValue = PrimTypes.js_value
       val prim_effect = PrimTypes.prim_effect
       fun refOf ty =
         TypedSyntax.TyCon (SourcePos.nullSpan, [ty], PrimTypes.Names.ref_)
       fun listOf ty =
         TypedSyntax.TyCon (SourcePos.nullSpan, [ty], PrimTypes.Names.list)
       fun vectorOf ty =
         TypedSyntax.TyCon (SourcePos.nullSpan, [ty], PrimTypes.Names.vector)
       fun arrayOf ty =
         TypedSyntax.TyCon (SourcePos.nullSpan, [ty], PrimTypes.Names.array)
       fun pairOf (ty1, ty2) =
         TypedSyntax.PairType (SourcePos.nullSpan, ty1, ty2)
       fun tupleOf types = TypedSyntax.TupleType (SourcePos.nullSpan, types)
       fun function1Of (a, result) =
         TypedSyntax.FnType (SourcePos.nullSpan, a, result)
       fun function2Of (a, b, result) =
         TypedSyntax.TyCon
           (SourcePos.nullSpan, [a, b, result], PrimTypes.Names.function2)
       fun function3Of (a, b, c, result) =
         TypedSyntax.TyCon
           (SourcePos.nullSpan, [a, b, c, result], PrimTypes.Names.function3)
       fun promptTagOf ty =
         TypedSyntax.TyCon
           (SourcePos.nullSpan, [ty], PrimTypes.Names.prompt_tag)
       fun subcontOf (a, b) =
         TypedSyntax.TyCon (SourcePos.nullSpan, [a, b], PrimTypes.Names.subcont)
       val Unconstrained = false
       val IsEqType = true) :
      sig
        val typeOf:
          Primitives.PrimOp
          -> { vars: (TypedSyntax.TyVar * bool) list
             , args: TypedSyntax.Ty vector
             , results: TypedSyntax.Ty list
             }
      end

  fun newTyVarCounter () : int ref = ref 100
  fun newVIdCounter () : int ref = ref 100

  fun freshVId (ctx: Context, name) : TypedSyntax.VId =
    let
      val n = !(#nextVId ctx)
    in
      #nextVId ctx := n + 1;
      TypedSyntax.MkVId (Syntax.SourceName.fromString name, n)
    end

  fun renewVId (ctx: Context) (TypedSyntax.MkVId (name, _)) : TypedSyntax.VId =
    let val n = !(#nextVId ctx)
    in #nextVId ctx := n + 1; TypedSyntax.MkVId (name, n)
    end

  fun genTyVarId (ctx: Context) =
    let val id = !(#nextTyVar ctx)
    in #nextTyVar ctx := id + 1; id
    end
  fun genTyVar (ctx, Syntax.MkTyVar tvname) =
    TypedSyntax.MkTyVar (tvname, genTyVarId ctx)

  fun freshTyVar
    ( ctx: InferenceContext
    , span: SourcePos.span
    , equality: bool
    , class: TypedSyntax.class option
    ) : TypedSyntax.AnonymousTyVar =
    ref (TypedSyntax.Unbound
      ( {sourceSpan = span, equalityRequired = equality, class = class}
      , #level ctx
      ))

  fun newVIdWithName (ctx: Context, name) =
    let val n = !(#nextVId ctx)
    in #nextVId ctx := n + 1; TypedSyntax.MkVId (name, n)
    end

  fun newVId (ctx: Context, Syntax.MkVId name) =
        newVIdWithName (ctx, Syntax.SourceName.fromString name)
    | newVId (ctx, Syntax.GeneratedVId (name, _)) = newVIdWithName (ctx, name)

  fun newStrId (ctx: Context, Syntax.MkStrId name) =
    let val n = !(#nextVId ctx)
    in #nextVId ctx := n + 1; TypedSyntax.MkStrId (name, n)
    end

  fun newFunId (ctx: Context, Syntax.MkFunId name) =
    let val n = !(#nextVId ctx)
    in #nextVId ctx := n + 1; TypedSyntax.MkFunId (name, n)
    end

  fun genTyConId (ctx: Context) =
    let val id = !(#nextTyVar ctx)
    in #nextTyVar ctx := id + 1; id
    end
  fun newTyName (ctx, Syntax.MkTyCon name) =
    TypedSyntax.MkTyName (name, genTyConId ctx)
  fun renewTyName (ctx: Context, TypedSyntax.MkTyVar (name, _)) =
    let val id = !(#nextTyVar ctx)
    in #nextTyVar ctx := id + 1; TypedSyntax.MkTyName (name, id)
    end

  local structure T = TypedSyntax
  in
    (*: val typePrinter : Env -> { ty : T.Ty -> string, tyName : T.TyName -> string, anonymousTyVar : T.AnonymousTyVar -> string } *)
    fun typePrinter (env: Env) =
      let
        val anonymous: (T.AnonymousTyVar * int * string) list ref = ref []
        fun freshName v =
          let
            val a = !anonymous
            val i =
              1 + List.foldl (fn ((_, j, _), acc) => Int.max (acc, j)) ~1 a
            val name = "?" ^ Int.toString i
          in
            anonymous := (v, i, name) :: a;
            name
          end
        fun nameOf v =
          case List.find (fn (u, _, _) => v = u) (!anonymous) of
            SOME (_, _, name) => name
          | NONE => freshName v
        fun lookupTyNameInTyConMap
          (tyConMap: T.TypeStructure Syntax.TyConMap.map, tyname) =
          let
            val map =
              Syntax.TyConMap.mapPartiali
                (fn ( Syntax.MkTyCon tycon
                    , { typeFunction =
                          T.TypeFunction (params, T.TyCon (_, args, tyname'))
                      , valEnv = _
                      }
                    ) =>
                   if
                     tyname = tyname'
                     andalso
                     ListPair.allEq
                       (fn (tv, T.TyVar (_, tv')) => T.eqTyVar (tv, tv')
                         | _ => false) (params, args)
                   then SOME tycon
                   else NONE
                  | _ => NONE) tyConMap
          in
            Syntax.TyConMap.first map
          end
        (* Breadth-first search *)
        fun lookupTyNameInSignatures
          (ss: (T.Signature * string list) list, tyname) =
          let
            fun go [] =
                  lookupTyNameInSignatures
                    ( List.foldr
                        (fn ((s, revPath), acc) =>
                           Syntax.StrIdMap.foldri
                             (fn (Syntax.MkStrId strid, T.MkSignature s', acc) =>
                                (s', strid :: revPath) :: acc) acc (#strMap s))
                        [] ss
                    , tyname
                    ) (* go deeper *)
              | go ((s, revPath) :: ss) =
                  case lookupTyNameInTyConMap (#tyConMap s, tyname) of
                    SOME tycon =>
                      SOME (String.concatWith "."
                        (List.revAppend (revPath, [tycon])))
                  | NONE => go ss
          in
            if List.null ss then NONE else go ss
          end
        val seenTyNames: (string T.TyNameMap.map) ref = ref T.TyNameMap.empty
        fun lookupTyName tyname =
          let
            val seen = !seenTyNames
          in
            case T.TyNameMap.find (seen, tyname) of
              SOME name => name
            | NONE =>
                let
                  val unqualified =
                    lookupTyNameInTyConMap (#tyConMap env, tyname)
                  val unqualifiedNoPrim =
                    case unqualified of
                      n as SOME name =>
                        if String.isPrefix "_Prim." name then
                          NONE (* Should avoid internal identifiers like _Prim.* *)
                        else
                          n
                    | n => n
                  val named =
                    case unqualifiedNoPrim of
                      n as SOME _ => n
                    | NONE =>
                        let
                          val qualified = lookupTyNameInSignatures
                            ( Syntax.StrIdMap.foldri
                                (fn (Syntax.MkStrId strid, (s, _), acc) =>
                                   (s, [strid]) :: acc) [] (#strMap env)
                            , tyname
                            )
                        in
                          case qualified of
                            n as SOME _ => n
                          | NONE => unqualified
                        end
                in
                  case named of
                    SOME name =>
                      ( seenTyNames := T.TyNameMap.insert (seen, tyname, name)
                      ; name
                      )
                  | NONE =>
                      let
                        val name =
                          case tyname of
                            T.MkTyVar (n, i) => "?" ^ n ^ "." ^ Int.toString i
                      in
                        seenTyNames := T.TyNameMap.insert (seen, tyname, name);
                        name
                      end
                end
          end
        (*
         * AtTy ::= TyVar
         *         | LongTyCon
         *        | '{' TyRow '}'
         *        | '(' Ty ')'
         * ConTy ::= AtTy
         *         | ConTy LongTyCon
         *         | '(' Ty ',' ... ')' LongTyCon
         * TupTy ::= ConTy
         *         | ConTy '*' TupTy
         * Ty ::= TupTy
         *      | TupTy '->' Ty
         * prec = 0: only AtTy is allowed.
         * prec = 1: ConTy is allowed.
         * prec = 2: TupTy is allowed
         * prec = 3: Ty is allowed.
         *)
        fun paren true s acc =
              "(" :: s @ ")" :: acc
          | paren false s acc = s @ acc
        fun labelToString (Syntax.NumericLabel i) = Int.toString i
          | labelToString (Syntax.IdentifierLabel x) = x
        fun go (_, T.TyVar (_, T.MkTyVar (name, _))) acc = name :: acc
          | go (prec, T.AnonymousTyVar (_, ref (T.Link t))) acc =
              go (prec, t) acc
          | go (_, T.AnonymousTyVar (_, a as ref (T.Unbound _))) acc =
              nameOf a :: acc
          | go (prec, T.RecordType (_, fields)) acc =
              let
                val l_fields = Syntax.LabelMap.listItemsi fields
                val t = Syntax.extractTuple (1, l_fields)
              in
                if
                  (case t of
                     SOME u => List.null u
                   | NONE => false)
                then
                  let
                    val isUnit =
                      case
                        Syntax.TyConMap.find
                          (#tyConMap env, Syntax.MkTyCon "unit")
                      of
                        SOME
                          { typeFunction =
                              T.TypeFunction ([], T.RecordType (_, fields'))
                          , valEnv = _
                          } => Syntax.LabelMap.isEmpty fields'
                      | _ => false
                  in
                    (if isUnit then "unit" else "{}") :: acc
                  end
                else
                  case t of
                    SOME (ls as (_ :: _ :: _)) =>
                      let
                        fun makeTupleTy (ty, acc) =
                          let
                            val acc =
                              if List.null acc then acc else " * " :: acc
                          in
                            go (1, ty) acc
                          end
                      in
                        paren (prec < 2) (List.foldr makeTupleTy [] ls) acc
                      end
                  | _ =>
                      let
                        fun makeField ((label, ty), acc) =
                          let
                            val acc = if List.null acc then acc else ", " :: acc
                          in
                            labelToString label :: " : " :: go (3, ty) acc
                          end
                      in
                        "{" :: List.foldr makeField [] l_fields @ "}" :: acc
                      end
              end
          | go (_, T.RecordExtType (_, fields, base)) acc =
              let
                val l_fields = Syntax.LabelMap.listItemsi fields
                val t = Syntax.extractTuple (1, l_fields)
                fun makeField ((label, ty), acc) =
                  labelToString label :: " : " :: go (3, ty) (", " :: acc)
              in
                "{"
                ::
                List.foldr makeField ("... : " :: go (3, base) []) l_fields
                @ "}" :: acc
              end
          | go (prec, T.FnType (_, ty0, ty1)) acc =
              paren (prec < 3) (go (2, ty0) (" -> " :: go (3, ty1) [])) acc
          | go (prec, T.TyCon (_, args, tyname)) acc =
              let
                val tyname' = lookupTyName tyname
              in
                case args of
                  [] => tyname' :: acc
                | [ty] =>
                    paren (prec < 1) (go (0, ty) [" ", tyname'])
                      acc (* Use redundant parens for types like '(int option) list' *)
                | _ =>
                    "("
                    ::
                    #2
                      (List.foldr
                         (fn (ty, (last, acc)) =>
                            ( false
                            , go (3, ty) (if last then acc else ", " :: acc)
                            )) (true, ") " :: tyname' :: acc) args)
              end
        fun print_Ty ty =
          String.concat (go (3, ty) [])
      in
        { ty = print_Ty
        , tyName = lookupTyName
        , anonymousTyVar = fn ref (T.Link ty) => print_Ty ty | a => nameOf a
        }
      end
    fun print_Ty env =
      #ty (typePrinter env)
  end

  local structure S = Syntax structure T = TypedSyntax
  in
    (*: val occurCheckAndAdjustLevel : T.AnonymousTyVar -> T.Ty -> bool (* returns true if the type variable occurs in the type *) *)
    fun occurCheckAndAdjustLevel tv =
      let
        fun check (T.AnonymousTyVar (_, tv')) =
              if tv = tv' then
                true
              else
                (case !tv' of
                   T.Unbound (ct, l') =>
                     let
                       val minLevel =
                         case !tv of
                           T.Unbound (_, l) => Int.min (l, l')
                         | _ => l'
                     in
                       tv' := T.Unbound (ct, minLevel);
                       false
                     end
                 | T.Link ty => check ty)
          | check (T.TyVar (_, _)) = false
          | check (T.RecordType (_, xs)) = Syntax.LabelMap.exists check xs
          | check (T.TyCon (_, tyargs, _)) = List.exists check tyargs
          | check (T.FnType (_, ty1, ty2)) = check ty1 orelse check ty2
          | check (T.RecordExtType (_, xs, baseTy)) =
              Syntax.LabelMap.exists check xs orelse check baseTy
      in
        check
      end

    val applySubstTy = T.applySubstTy

    (*: val instantiate : InferenceContext * SourcePos.span * T.TypeScheme -> T.Ty * (T.Ty * T.UnaryConstraint option) list *)
    fun instantiate (ctx: InferenceContext, span, T.TypeScheme (vars, ty)) =
      let
        val (subst, tyargs) =
          List.foldl
            (fn ((v, ct), (set, rest)) =>
               let
                 val (equality, class) =
                   case ct of
                     NONE => (false, NONE)
                   | SOME (T.IsRecord excluded) =>
                       ( false
                       , SOME (T.Record
                           (List.foldl Syntax.LabelSet.add'
                              Syntax.LabelSet.empty excluded))
                       )
                   | SOME T.IsEqType => (true, NONE)
                   | SOME T.IsIntegral => (false, SOME T.IntWord)
                   | SOME T.IsSignedReal => (false, SOME T.IntReal)
                   | SOME T.IsRing => (false, SOME T.IntWordReal)
                   | SOME T.IsOrdered => (false, SOME T.NumTxt)
                   | SOME T.IsInt => (false, SOME T.Int)
                   | SOME T.IsWord => (false, SOME T.Word)
                   | SOME T.IsReal => (false, SOME T.Real)
                   | SOME T.IsChar => (false, SOME T.Char)
                   | SOME T.IsString => (false, SOME T.String)
                 val tv = freshTyVar (ctx, span, equality, class)
                 val tyarg = T.AnonymousTyVar (span, tv)
               in
                 (T.TyVarMap.insert (set, v, tyarg), (tyarg, ct) :: rest)
               end) (T.TyVarMap.empty, []) vars
      in
        (applySubstTy subst ty, List.rev tyargs)
      end

    structure ConstraintInfo:
    sig
      datatype constraint_place = SEQUENCE | BRANCH
      datatype constraint_info =
        EXPECTED_ACTUAL
      | ACTUAL_EXPECTED
      | PREVIOUS_CURRENT of constraint_place
      | CURRENT_PREVIOUS of constraint_place
      val flip: constraint_info -> constraint_info
      datatype 'a sorted =
        SUBSUMPTION of {expected: 'a, actual: 'a}
      | SEQUENCE_OR_BRANCH of
          {previous: 'a, current: 'a, place: constraint_place}
      val sort: constraint_info * 'a * 'a -> 'a sorted
    end =
    struct
      (* datatype constraint_place = VECTOR_PAT | LIST_EXP | VECTOR_EXP | IF_EXP | HANDLE_EXP | CASE_EXP | FN_EXP (* | FUN_DEC? *) *)
      datatype constraint_place =
        SEQUENCE
      | BRANCH
      datatype constraint_info =
        EXPECTED_ACTUAL
      | ACTUAL_EXPECTED
      | PREVIOUS_CURRENT of constraint_place
      | CURRENT_PREVIOUS of constraint_place
      fun flip EXPECTED_ACTUAL = ACTUAL_EXPECTED
        | flip ACTUAL_EXPECTED = EXPECTED_ACTUAL
        | flip (PREVIOUS_CURRENT place) = CURRENT_PREVIOUS place
        | flip (CURRENT_PREVIOUS place) = PREVIOUS_CURRENT place
      datatype 'a sorted =
        SUBSUMPTION of {expected: 'a, actual: 'a}
      | SEQUENCE_OR_BRANCH of
          {previous: 'a, current: 'a, place: constraint_place}
      fun sort (EXPECTED_ACTUAL, a, b) = SUBSUMPTION {expected = a, actual = b}
        | sort (ACTUAL_EXPECTED, a, b) = SUBSUMPTION {expected = b, actual = a}
        | sort (PREVIOUS_CURRENT place, a, b) =
            SEQUENCE_OR_BRANCH {previous = a, current = b, place = place}
        | sort (CURRENT_PREVIOUS place, a, b) =
            SEQUENCE_OR_BRANCH {previous = a, current = b, place = place}
    end

    (*:
    val unify : InferenceContext * Env * ConstraintInfo.constraint_info * SourcePos.span * T.Ty * T.Ty -> unit
    and solveUnary : InferenceContext * Env * ConstraintInfo.constraint_info * SourcePos.span * T.Ty * T.UnaryConstraint -> unit
    and unifyTyVarAndTy : InferenceContext * Env * ConstraintInfo.constraint_info * SourcePos.span * T.AnonymousTyVar * T.Ty -> unit
     *)
    (* The environment is used to determine if a data type admits equality *)
    fun unify
          ( ctx: InferenceContext
          , env: Env
          , ci
          , span1
          , T.AnonymousTyVar (_, tv)
          , ty
          ) =
          unifyTyVarAndTy (ctx, env, ci, span1, tv, ty)
      | unify (ctx, env, ci, span1, ty, T.AnonymousTyVar (_, tv)) =
          unifyTyVarAndTy (ctx, env, ConstraintInfo.flip ci, span1, tv, ty)
      | unify
          ( ctx
          , _
          , ci
          , span1
          , T.TyVar (_, tv as T.MkTyVar (name, _))
          , T.TyVar (_, tv' as T.MkTyVar (name', _))
          ) =
          if T.eqTyVar (tv, tv') then
            () (* do nothing *)
          else
            (case ConstraintInfo.sort (ci, name, name') of
               ConstraintInfo.SUBSUMPTION {expected, actual} =>
                 emitTypeError
                   ( ctx
                   , [span1]
                   , "expected " ^ expected ^ ", but got " ^ actual
                   )
             | ConstraintInfo.SEQUENCE_OR_BRANCH {previous, current, place = _} =>
                 emitTypeError
                   ( ctx
                   , [span1]
                   , "previous was " ^ previous ^ ", but now is " ^ current
                   ))
      | unify (ctx, env, ci, span, T.FnType (_, s0, s1), T.FnType (_, t0, t1)) =
          ( unify (ctx, env, ci, span, s0, t0)
          ; unify (ctx, env, ci, span, s1, t1)
          )
      | unify
          ( ctx
          , env
          , ci
          , span1
          , T.RecordType (span2, fields)
          , T.RecordType (span3, fields')
          ) =
          let
            val incompatible =
              Syntax.LabelMap.numItems fields
              <> Syntax.LabelMap.numItems fields'
            val incompatible =
              Syntax.LabelMap.foldli
                (fn (label, ty, incompatible) =>
                   case Syntax.LabelMap.find (fields', label) of
                     NONE => true
                   | SOME ty' =>
                       (unify (ctx, env, ci, span1, ty, ty'); incompatible))
                incompatible fields
          in
            if incompatible then
              emitTypeError
                ( ctx
                , [span1, span2, span3]
                , "unification failed: incompatible record types (different fields)"
                )
            else
              ()
          end
      | unify
          ( ctx
          , env
          , ci
          , span1
          , T.RecordType (span2, fields)
          , T.RecordExtType (span3, fields', baseTy)
          ) =
          let
            val incompatible =
              Syntax.LabelMap.numItems fields < Syntax.LabelMap.numItems fields'
            val incompatible =
              Syntax.LabelMap.foldli
                (fn (label, ty, incompatible) =>
                   case Syntax.LabelMap.find (fields, label) of
                     NONE => true
                   | SOME ty' =>
                       (unify (ctx, env, ci, span1, ty, ty'); incompatible))
                incompatible fields'
            val extraFields =
              Syntax.LabelMap.filteri
                (fn (label, _) =>
                   not (Syntax.LabelMap.inDomain (fields', label))) fields
          in
            unify
              (ctx, env, ci, span1, T.RecordType (span2, extraFields), baseTy);
            if incompatible then
              emitTypeError
                ( ctx
                , [span1, span2, span3]
                , "unification failed: incompatible record types (different fields)"
                )
            else
              ()
          end
      | unify
          ( ctx
          , env
          , ci
          , span1
          , T.RecordExtType (span3, fields', baseTy)
          , T.RecordType (span2, fields)
          ) =
          let
            val incompatible =
              Syntax.LabelMap.numItems fields < Syntax.LabelMap.numItems fields'
            val incompatible =
              Syntax.LabelMap.foldli
                (fn (label, ty, incompatible) =>
                   case Syntax.LabelMap.find (fields, label) of
                     NONE => true
                   | SOME ty' =>
                       (unify (ctx, env, ci, span1, ty, ty'); incompatible))
                incompatible fields'
            val extraFields =
              Syntax.LabelMap.filteri
                (fn (label, _) =>
                   not (Syntax.LabelMap.inDomain (fields', label))) fields
          in
            unify
              (ctx, env, ci, span1, baseTy, T.RecordType (span2, extraFields));
            if incompatible then
              emitTypeError
                ( ctx
                , [span1, span2, span3]
                , "unification failed: incompatible record types (different fields)"
                )
            else
              ()
          end
      | unify
          ( ctx
          , env
          , ci
          , span1
          , T.RecordExtType (span2, fields, baseTy)
          , T.RecordExtType (span3, fields', baseTy')
          ) =
          let
            val () =
              Syntax.LabelMap.app
                (fn (ty, ty') => unify (ctx, env, ci, span1, ty, ty'))
                (Syntax.LabelMap.intersectWith (fn (ty, ty') => (ty, ty'))
                   (fields, fields'))
            val uniqueFields =
              Syntax.LabelMap.filteri
                (fn (label, _) =>
                   not (Syntax.LabelMap.inDomain (fields', label))) fields
            val uniqueFields' =
              Syntax.LabelMap.filteri
                (fn (label, _) => not (Syntax.LabelMap.inDomain (fields, label)))
                fields'
          in
            case
              ( Syntax.LabelMap.isEmpty uniqueFields
              , Syntax.LabelMap.isEmpty uniqueFields'
              )
            of
              (true, true) => unify (ctx, env, ci, span1, baseTy, baseTy')
            | (true, false) =>
                unify
                  ( ctx
                  , env
                  , ci
                  , span1
                  , baseTy
                  , T.RecordExtType (span3, uniqueFields', baseTy')
                  ) (* fields is a proper submap of fields' *)
            | (false, true) =>
                unify
                  ( ctx
                  , env
                  , ci
                  , span1
                  , T.RecordExtType (span3, uniqueFields, baseTy)
                  , baseTy'
                  ) (* fields' is a proper submap of fields *)
            | (false, false) =>
                let
                  val unionFields =
                    Syntax.LabelMap.foldli
                      (fn (label, _, acc) => Syntax.LabelSet.add (acc, label))
                      (Syntax.LabelMap.foldli
                         (fn (label, _, acc) => Syntax.LabelSet.add (acc, label))
                         Syntax.LabelSet.empty fields) fields'
                  val commonBaseTy = T.AnonymousTyVar (span1, freshTyVar
                    (ctx, span1, false, SOME (T.Record unionFields)))
                in
                  unify
                    ( ctx
                    , env
                    , ci
                    , span1
                    , baseTy
                    , T.RecordExtType (span3, uniqueFields', commonBaseTy)
                    );
                  unify
                    ( ctx
                    , env
                    , ci
                    , span1
                    , baseTy'
                    , T.RecordExtType (span2, uniqueFields, commonBaseTy)
                    )
                end
          end
      | unify
          ( ctx
          , env
          , ci
          , span1
          , ty1 as T.TyCon (span2, tyarg, con)
          , ty2 as T.TyCon (span3, tyarg', con')
          ) =
          if T.eqTyName (con, con') then
            ListPair.appEq (fn (x, y) => unify (ctx, env, ci, span1, x, y))
              (tyarg, tyarg')
            handle ListPair.UnequalLengths =>
              emitTypeError
                ( ctx
                , [span1, span2, span3]
                , "unification failed: the number of type arguments differ"
                )
          else
            (case ConstraintInfo.sort (ci, ty1, ty2) of
               ConstraintInfo.SUBSUMPTION {expected, actual} =>
                 let
                   val pTy = print_Ty env
                 in
                   emitTypeError
                     ( ctx
                     , [span1]
                     , "expected " ^ pTy expected ^ ", but got " ^ pTy actual
                     )
                 end
             | ConstraintInfo.SEQUENCE_OR_BRANCH {previous, current, place = _} =>
                 let
                   val pTy = print_Ty env
                 in
                   emitTypeError
                     ( ctx
                     , [span1]
                     , "previous was " ^ pTy previous ^ ", but now is "
                       ^ pTy current
                     )
                 end)
      | unify (ctx, _, _, span1, T.TyVar (span2, T.MkTyVar (name, _)), _) =
          emitTypeError
            (ctx, [span1, span2], "cannot unify named type variable: " ^ name)
      | unify (ctx, _, _, span1, _, T.TyVar (span2, T.MkTyVar (name, _))) =
          emitTypeError
            (ctx, [span1, span2], "cannot unify named type variable: " ^ name)
      | unify (ctx, env, ci, span, ty1, ty2) =
          let
            val pTy = print_Ty env
          in
            case ConstraintInfo.sort (ci, ty1, ty2) of
              ConstraintInfo.SUBSUMPTION {expected, actual} =>
                emitTypeError
                  ( ctx
                  , [span]
                  , "expected " ^ pTy expected ^ ", but got " ^ pTy actual
                  )
            | ConstraintInfo.SEQUENCE_OR_BRANCH {previous, current, place = _} =>
                emitTypeError
                  ( ctx
                  , [span]
                  , "previous was " ^ pTy previous ^ ", but now is "
                    ^ pTy current
                  )
          end
    and solveUnary (ctx: InferenceContext, env: Env, ci, span1, ty, ct) =
      let
        fun mismatch () =
          let
            val lhs = #ty (typePrinter env) ty
            (*
              case ty of
                T.RecordType (_, _) => "a record"
              | T.RecordExtType (_, _, _) => "a record"
              | T.TyCon (_, _, T.MkTyVar (name, _)) => name
              | T.FnType (_, _, _) => "a function"
              | T.TyVar (_, T.MkTyVar (name, _)) => name
              | T.AnonymousTyVar _ => "<anonymous>" (* should not occur *)
             *)
            val rhs =
              case ct of
                T.IsRecord _ => "(a record)"
              | T.IsEqType => "(an equality type)" (* should not occur *)
              | T.IsIntegral => "(int or word)"
              | T.IsSignedReal => "(int or real)"
              | T.IsRing => "(int, word or real)"
              | T.IsOrdered => "(int, word, real, char or string)"
              | T.IsInt => "(int)"
              | T.IsWord => "(word)"
              | T.IsReal => "(real)"
              | T.IsChar => "(char)"
              | T.IsString => "(string)"
          in
            case ConstraintInfo.sort (ci, lhs, rhs) of
              ConstraintInfo.SUBSUMPTION {expected, actual} =>
                emitTypeError
                  (ctx, [span1], "expected " ^ expected ^ ", but got " ^ actual)
            | ConstraintInfo.SEQUENCE_OR_BRANCH {previous, current, place = _} =>
                emitTypeError
                  ( ctx
                  , [span1]
                  , "previous was " ^ previous ^ ", but now is " ^ current
                  )
          end
      in
        case (ty, ct) of
          (T.RecordType (span2, fields), T.IsRecord labels) =>
            List.app
              (fn label =>
                 case Syntax.LabelMap.find (fields, label) of
                   NONE => ()
                 | SOME _ =>
                     emitTypeError
                       (ctx, [span1, span2], "duplicate record field")) labels
        | (T.RecordExtType (span2, fields, baseTy), T.IsRecord labels) =>
            let
              val baseExcludedLabels =
                List.filter
                  (fn label =>
                     case Syntax.LabelMap.find (fields, label) of
                       NONE => true
                     | SOME _ => false) labels
              val () =
                List.app
                  (fn label =>
                     case Syntax.LabelMap.find (fields, label) of
                       NONE => ()
                     | SOME _ =>
                         emitTypeError
                           (ctx, [span1, span2], "duplicate record field"))
                  labels
            in
              solveUnary
                (ctx, env, ci, span1, baseTy, T.IsRecord baseExcludedLabels)
            end
        | (T.TyCon _, T.IsRecord _) => mismatch ()
        | (T.FnType _, T.IsRecord _) => mismatch ()
        | (T.TyVar _, T.IsRecord _) => mismatch ()
        | (T.RecordType (_, fields), T.IsEqType) =>
            Syntax.LabelMap.app
              (fn ty => solveUnary (ctx, env, ci, span1, ty, T.IsEqType)) fields
        | (T.RecordType _, T.IsIntegral) => mismatch ()
        | (T.RecordType _, T.IsSignedReal) => mismatch ()
        | (T.RecordType _, T.IsRing) => mismatch ()
        | (T.RecordType _, T.IsOrdered) => mismatch ()
        | (T.RecordType _, T.IsInt) => mismatch ()
        | (T.RecordType _, T.IsWord) => mismatch ()
        | (T.RecordType _, T.IsReal) => mismatch ()
        | (T.RecordType _, T.IsChar) => mismatch ()
        | (T.RecordType _, T.IsString) => mismatch ()
        | (T.RecordExtType (_, fields, baseTy), T.IsEqType) =>
            ( Syntax.LabelMap.app
                (fn ty => solveUnary (ctx, env, ci, span1, ty, T.IsEqType))
                fields
            ; solveUnary (ctx, env, ci, span1, baseTy, T.IsEqType)
            )
        | (T.RecordExtType _, T.IsIntegral) => mismatch ()
        | (T.RecordExtType _, T.IsSignedReal) => mismatch ()
        | (T.RecordExtType _, T.IsRing) => mismatch ()
        | (T.RecordExtType _, T.IsOrdered) => mismatch ()
        | (T.RecordExtType _, T.IsInt) => mismatch ()
        | (T.RecordExtType _, T.IsWord) => mismatch ()
        | (T.RecordExtType _, T.IsReal) => mismatch ()
        | (T.RecordExtType _, T.IsChar) => mismatch ()
        | (T.RecordExtType _, T.IsString) => mismatch ()
        | (T.FnType _, T.IsEqType) => mismatch ()
        | (T.FnType _, T.IsIntegral) => mismatch ()
        | (T.FnType _, T.IsSignedReal) => mismatch ()
        | (T.FnType _, T.IsRing) => mismatch ()
        | (T.FnType _, T.IsOrdered) => mismatch ()
        | (T.FnType _, T.IsInt) => mismatch ()
        | (T.FnType _, T.IsWord) => mismatch ()
        | (T.FnType _, T.IsReal) => mismatch ()
        | (T.FnType _, T.IsChar) => mismatch ()
        | (T.FnType _, T.IsString) => mismatch ()
        | (T.TyCon (span2, tyargs, tyname), T.IsEqType) =>
            let
              val {admitsEquality, ...} =
                lookupTyNameInEnv (#context ctx, env, span2, tyname)
            in
              if isRefOrArray tyname then
                ()
              else if admitsEquality then
                List.app
                  (fn tyarg =>
                     solveUnary (ctx, env, ci, span1, tyarg, T.IsEqType)) tyargs
              else
                emitTypeError
                  ( ctx
                  , [span1, span2]
                  , #tyName (typePrinter env) tyname
                    ^ " does not admit equality"
                  )
            end
        | (T.TyCon (span2, _, tyname), T.IsIntegral) =>
            let
              val {overloadClass, ...} =
                lookupTyNameInEnv (#context ctx, env, span2, tyname)
              val isIntegral =
                case overloadClass of
                  SOME Syntax.CLASS_INT => true
                | SOME Syntax.CLASS_WORD => true
                | _ => false
            in
              if isIntegral then () (* do nothing *) else mismatch ()
            end
        | (T.TyCon (span2, _, tyname), T.IsSignedReal) =>
            let
              val {overloadClass, ...} =
                lookupTyNameInEnv (#context ctx, env, span2, tyname)
              val isSignedReal =
                case overloadClass of
                  SOME Syntax.CLASS_INT => true
                | SOME Syntax.CLASS_REAL => true
                | _ => false
            in
              if isSignedReal then () (* do nothing *) else mismatch ()
            end
        | (T.TyCon (span2, _, tyname), T.IsRing) =>
            let
              val {overloadClass, ...} =
                lookupTyNameInEnv (#context ctx, env, span2, tyname)
              val isRing =
                case overloadClass of
                  SOME Syntax.CLASS_INT => true
                | SOME Syntax.CLASS_WORD => true
                | SOME Syntax.CLASS_REAL => true
                | _ => false
            in
              if isRing then () (* do nothing *) else mismatch ()
            end
        | (T.TyCon (span2, _, tyname), T.IsOrdered) =>
            let
              val {overloadClass, ...} =
                lookupTyNameInEnv (#context ctx, env, span2, tyname)
              val isOrdered =
                case overloadClass of
                  SOME Syntax.CLASS_INT => true
                | SOME Syntax.CLASS_WORD => true
                | SOME Syntax.CLASS_REAL => true
                | SOME Syntax.CLASS_CHAR => true
                | SOME Syntax.CLASS_STRING => true
                | NONE => false
            in
              if isOrdered then () (* do nothing *) else mismatch ()
            end
        | (T.TyCon (span2, _, tyname), T.IsInt) =>
            if TypedSyntax.eqTyName (tyname, PrimTypes.Names.int) then
              () (* do nothing *)
            else if TypedSyntax.eqTyName (tyname, PrimTypes.Names.intInf) then
              () (* do nothing *)
            else if TypedSyntax.eqTyName (tyname, PrimTypes.Names.int32) then
              () (* do nothing *)
            else if TypedSyntax.eqTyName (tyname, PrimTypes.Names.int54) then
              () (* do nothing *)
            else if TypedSyntax.eqTyName (tyname, PrimTypes.Names.int64) then
              () (* do nothing *)
            else
              let
                val {overloadClass, ...} =
                  lookupTyNameInEnv (#context ctx, env, span2, tyname)
              in
                if overloadClass = SOME Syntax.CLASS_INT then
                  () (* do nothing *)
                else
                  mismatch ()
              end
        | (T.TyCon (span2, _, tyname), T.IsWord) =>
            if TypedSyntax.eqTyName (tyname, PrimTypes.Names.word) then
              () (* do nothing *)
            else if TypedSyntax.eqTyName (tyname, PrimTypes.Names.word32) then
              () (* do nothing *)
            else if TypedSyntax.eqTyName (tyname, PrimTypes.Names.word64) then
              () (* do nothing *)
            else
              let
                val {overloadClass, ...} =
                  lookupTyNameInEnv (#context ctx, env, span2, tyname)
              in
                if overloadClass = SOME Syntax.CLASS_WORD then
                  () (* do nothing *)
                else
                  mismatch ()
              end
        | (T.TyCon (span2, _, tyname), T.IsReal) =>
            if TypedSyntax.eqTyName (tyname, PrimTypes.Names.real) then
              () (* do nothing *)
            else
              let
                val {overloadClass, ...} =
                  lookupTyNameInEnv (#context ctx, env, span2, tyname)
              in
                if overloadClass = SOME Syntax.CLASS_REAL then
                  () (* do nothing *)
                else
                  mismatch ()
              end
        | (T.TyCon (span2, _, tyname), T.IsChar) =>
            if TypedSyntax.eqTyName (tyname, PrimTypes.Names.char) then
              () (* do nothing *)
            else
              let
                val {overloadClass, ...} =
                  lookupTyNameInEnv (#context ctx, env, span2, tyname)
              in
                if overloadClass = SOME Syntax.CLASS_CHAR then
                  () (* do nothing *)
                else
                  mismatch ()
              end
        | (T.TyCon (span2, _, tyname), T.IsString) =>
            if TypedSyntax.eqTyName (tyname, PrimTypes.Names.string) then
              () (* do nothing *)
            else
              let
                val {overloadClass, ...} =
                  lookupTyNameInEnv (#context ctx, env, span2, tyname)
              in
                if overloadClass = SOME Syntax.CLASS_STRING then
                  () (* do nothing *)
                else
                  mismatch ()
              end
        | (T.TyVar (span2, tv as T.MkTyVar (name, _)), T.IsEqType) =>
            if T.tyVarAdmitsEquality tv then
              ()
            else
              emitTypeError
                ( ctx
                , [span1, span2]
                , "the type variable " ^ name ^ " does not admit equality"
                )
        | (T.TyVar (span2, T.MkTyVar (name, _)), _) =>
            emitTypeError
              (ctx, [span1, span2], "the use of " ^ name ^ " is non-free")
        | (T.AnonymousTyVar (_, tv), pred) =>
            (case !tv of
               T.Link replacement =>
                 solveUnary (ctx, env, ci, span1, replacement, pred)
             | T.Unbound ({sourceSpan, equalityRequired, class}, level) =>
                 let
                   val class' =
                     case (pred, class) of
                       (T.IsRecord labels, NONE) =>
                         SOME (T.Record
                           (List.foldl Syntax.LabelSet.add'
                              Syntax.LabelSet.empty labels))
                     | (T.IsRecord labels, SOME (T.Record labels')) =>
                         SOME (T.Record
                           (List.foldl Syntax.LabelSet.add' labels' labels))
                     | (T.IsEqType, _) => class
                     | (T.IsIntegral, NONE) => SOME T.IntWord
                     | (T.IsIntegral, SOME T.NumTxt) => SOME T.IntWord
                     | (T.IsIntegral, SOME T.IntWordReal) => SOME T.IntWord
                     | (T.IsIntegral, SOME T.IntWord) => SOME T.IntWord
                     | (T.IsIntegral, SOME T.IntReal) => SOME T.Int
                     | (T.IsIntegral, SOME T.Int) => SOME T.Int
                     | (T.IsIntegral, SOME T.Word) => SOME T.Word
                     | (T.IsSignedReal, NONE) => SOME T.IntReal
                     | (T.IsSignedReal, SOME T.NumTxt) => SOME T.IntReal
                     | (T.IsSignedReal, SOME T.IntWordReal) => SOME T.IntReal
                     | (T.IsSignedReal, SOME T.IntWord) => SOME T.Int
                     | (T.IsSignedReal, SOME T.IntReal) => SOME T.IntReal
                     | (T.IsSignedReal, SOME T.Int) => SOME T.Int
                     | (T.IsSignedReal, SOME T.Real) => SOME T.Real
                     | (T.IsRing, NONE) => SOME T.IntWordReal
                     | (T.IsRing, SOME T.NumTxt) => SOME T.IntWordReal
                     | (T.IsRing, SOME T.IntWordReal) => SOME T.IntWordReal
                     | (T.IsRing, SOME T.IntWord) => SOME T.IntWord
                     | (T.IsRing, SOME T.IntReal) => SOME T.IntReal
                     | (T.IsRing, SOME T.Int) => SOME T.Int
                     | (T.IsRing, SOME T.Word) => SOME T.Word
                     | (T.IsRing, SOME T.Real) => SOME T.Real
                     | (T.IsOrdered, NONE) => SOME T.NumTxt
                     | (T.IsOrdered, SOME T.NumTxt) => SOME T.NumTxt
                     | (T.IsOrdered, SOME T.IntWordReal) => SOME T.IntWordReal
                     | (T.IsOrdered, SOME T.IntWord) => SOME T.IntWord
                     | (T.IsOrdered, SOME T.IntReal) => SOME T.IntReal
                     | (T.IsOrdered, SOME T.Int) => SOME T.Int
                     | (T.IsOrdered, SOME T.Word) => SOME T.Word
                     | (T.IsOrdered, SOME T.Real) => SOME T.Real
                     | (T.IsOrdered, SOME T.Char) => SOME T.Char
                     | (T.IsOrdered, SOME T.String) => SOME T.String
                     | (T.IsInt, NONE) => SOME T.Int
                     | (T.IsInt, SOME T.NumTxt) => SOME T.Int
                     | (T.IsInt, SOME T.IntWordReal) => SOME T.Int
                     | (T.IsInt, SOME T.IntWord) => SOME T.Int
                     | (T.IsInt, SOME T.IntReal) => SOME T.Int
                     | (T.IsInt, SOME T.Int) => SOME T.Int
                     | (T.IsWord, NONE) => SOME T.Word
                     | (T.IsWord, SOME T.NumTxt) => SOME T.Word
                     | (T.IsWord, SOME T.IntWordReal) => SOME T.Word
                     | (T.IsWord, SOME T.IntWord) => SOME T.Word
                     | (T.IsWord, SOME T.Word) => SOME T.Word
                     | (T.IsReal, NONE) => SOME T.Real
                     | (T.IsReal, SOME T.NumTxt) => SOME T.Real
                     | (T.IsReal, SOME T.IntWordReal) => SOME T.Real
                     | (T.IsReal, SOME T.IntReal) => SOME T.Real
                     | (T.IsReal, SOME T.Real) => SOME T.Real
                     | (T.IsChar, NONE) => SOME T.Char
                     | (T.IsChar, SOME T.NumTxt) => SOME T.Char
                     | (T.IsChar, SOME T.Char) => SOME T.Char
                     | (T.IsString, NONE) => SOME T.String
                     | (T.IsString, SOME T.NumTxt) => SOME T.String
                     | (T.IsString, SOME T.String) => SOME T.String
                     | (_, SOME prevClass) =>
                         let
                           val lhs =
                             case prevClass of
                               T.Record _ => "a record"
                             | T.NumTxt => "int, word, real, char or string"
                             | T.IntWordReal => "int, word or real"
                             | T.IntWord => "int or word"
                             | T.IntReal => "int or real"
                             | T.Int => "int"
                             | T.Word => "word"
                             | T.Real => "real"
                             | T.Char => "char"
                             | T.String => "string"
                           val rhs =
                             case ct of
                               T.IsRecord _ => "a record"
                             | T.IsEqType =>
                                 "an equality type" (* should not occur *)
                             | T.IsIntegral => "int or word"
                             | T.IsSignedReal => "int or real"
                             | T.IsRing => "int, word or real"
                             | T.IsOrdered => "int, word, real, char or string"
                             | T.IsInt => "int"
                             | T.IsWord => "word"
                             | T.IsReal => "real"
                             | T.IsChar => "char"
                             | T.IsString => "string"
                         in
                           case ConstraintInfo.sort (ci, lhs, rhs) of
                             ConstraintInfo.SUBSUMPTION {expected, actual} =>
                               emitTypeError
                                 ( ctx
                                 , [span1]
                                 , "expected " ^ expected ^ ", but got "
                                   ^ actual
                                 )
                           | ConstraintInfo.SEQUENCE_OR_BRANCH
                               {previous, current, place = _} =>
                               emitTypeError
                                 ( ctx
                                 , [span1]
                                 , "previous was " ^ previous ^ ", but now is "
                                   ^ current
                                 );
                           class
                         end
                   val equalityRequired' =
                     case (pred, class') of
                       (T.IsEqType, SOME T.Real) =>
                         ( emitTypeError
                             (ctx, [span1], "real does not admit equality")
                         ; true
                         )
                     | (T.IsEqType, _) => true
                     | _ => equalityRequired
                 in
                   tv
                   :=
                   T.Unbound
                     ( { sourceSpan = sourceSpan
                       , equalityRequired = equalityRequired'
                       , class = class'
                       }
                     , level
                     )
                 end)
      end
    and unifyTyVarAndTy
      ( ctx: InferenceContext
      , env: Env
      , ci: ConstraintInfo.constraint_info
      , span: SourcePos.span
      , tv: T.AnonymousTyVar
      , ty: T.Ty
      ) : unit =
      (case !tv of
         T.Link replacement => unify (ctx, env, ci, span, replacement, ty)
       | T.Unbound ({sourceSpan = _, equalityRequired, class}, _) =>
           let
             val ty = T.forceTy ty
           in
             if
               (case ty of
                  T.AnonymousTyVar (_, tv') => tv = tv'
                | _ => false)
             then (* ty = AnonymousTyVar tv *)
               () (* do nothing *)
             else if
               occurCheckAndAdjustLevel tv ty
             then
               let
                 val printer = typePrinter env
               in
                 emitTypeError
                   ( ctx
                   , [span, T.getSourceSpanOfTy ty]
                   , "unification failed: occurrence check ("
                     ^ #anonymousTyVar printer tv ^ " in " ^ #ty printer ty
                     ^ ")"
                   )
               end
             else
               let
                 val ci' = ConstraintInfo.flip ci
               in
                 tv := T.Link ty;
                 if equalityRequired then
                   solveUnary (ctx, env, ci', span, ty, T.IsEqType)
                 else
                   ();
                 case class of
                   NONE => ()
                 | SOME (T.Record excludedLabels) =>
                     solveUnary
                       ( ctx
                       , env
                       , ci'
                       , span
                       , ty
                       , T.IsRecord (Syntax.LabelSet.toList excludedLabels)
                       )
                 | SOME T.NumTxt =>
                     solveUnary (ctx, env, ci', span, ty, T.IsOrdered)
                 | SOME T.IntWordReal =>
                     solveUnary (ctx, env, ci', span, ty, T.IsRing)
                 | SOME T.IntWord =>
                     solveUnary (ctx, env, ci', span, ty, T.IsIntegral)
                 | SOME T.IntReal =>
                     solveUnary (ctx, env, ci', span, ty, T.IsSignedReal)
                 | SOME T.Int => solveUnary (ctx, env, ci', span, ty, T.IsInt)
                 | SOME T.Word => solveUnary (ctx, env, ci', span, ty, T.IsWord)
                 | SOME T.Real => solveUnary (ctx, env, ci', span, ty, T.IsReal)
                 | SOME T.Char => solveUnary (ctx, env, ci', span, ty, T.IsChar)
                 | SOME T.String =>
                     solveUnary (ctx, env, ci', span, ty, T.IsString)
               end
           end)
    (*: val commonType : InferenceContext * Env * ConstraintInfo.constraint_place * SourcePos.span * T.Ty * T.Ty -> T.Ty *)
    (* Used by list exps, vector pats/exps, if-then-else, match *)
    fun commonType
      (ctx: InferenceContext, env: Env, place, span, previousTy, currentTy) =
      ( unify
          ( ctx
          , env
          , ConstraintInfo.PREVIOUS_CURRENT place
          , span
          , previousTy
          , currentTy
          )
      ; previousTy
      )
    (*: val checkSubsumption : InferenceContext * Env * SourcePos.span * T.Ty * T.Ty -> unit *)
    fun checkSubsumption
      (ctx: InferenceContext, env: Env, span, actualTy, expectedTy) =
      unify
        (ctx, env, ConstraintInfo.ACTUAL_EXPECTED, span, actualTy, expectedTy)

    (*: val evalBaseTy : (('a T.BaseTy) T.TyVarMap.map -> T.PureTy -> 'a T.BaseTy) -> Context * ('val, 'str) Env' * S.Ty -> 'a T.BaseTy *)
    fun evalBaseTy
      (applySubst: ('a T.BaseTy) T.TyVarMap.map -> T.PureTy -> 'a T.BaseTy) :
      Context * ('val, 'str) Env' * S.Ty
      -> 'a T.BaseTy =
      let
        fun evalTy (ctx: Context, env, S.TyVar (span, tv)) : 'a T.BaseTy =
              (case Syntax.TyVarMap.find (#boundTyVars env, tv) of
                 SOME tv => T.TyVar (span, tv)
               | NONE =>
                   emitFatalError
                     ( ctx
                     , [span]
                     , "unknown type varibale `" ^ Syntax.print_TyVar tv ^ "`"
                     ))
          | evalTy (ctx, env, S.RecordType (span, fields, NONE)) =
              T.RecordType
                ( span
                , List.foldl
                    (fn ((label, ty), m) =>
                       Syntax.LabelMap.insert (m, label, evalTy (ctx, env, ty)))
                    Syntax.LabelMap.empty fields
                )
          | evalTy (ctx, env, S.RecordType (span, fields, SOME baseTy)) =
              (case evalTy (ctx, env, baseTy) of
                 T.RecordType (_, fields') =>
                   T.RecordType
                     ( span
                     , List.foldl
                         (fn ((label, ty), m) =>
                            ( if Syntax.LabelMap.inDomain (m, label) then
                                emitError
                                  ( ctx
                                  , [span]
                                  , "duplicate record field: "
                                    ^ Syntax.print_Label label
                                  )
                              else
                                ()
                            ; Syntax.LabelMap.insert
                                (m, label, evalTy (ctx, env, ty))
                            )) fields' fields
                     )
               | _ => emitFatalError (ctx, [span], "invalid record extension"))
          | evalTy (ctx, env, S.TyCon (span, args, tycon)) =
              let
                val {typeFunction = T.TypeFunction (tyvars, ty), ...} =
                  lookupTyConInEnv (ctx, env, span, tycon)
                val subst =
                  (ListPair.foldlEq
                     (fn (tv, arg, m) =>
                        TypedSyntax.TyVarMap.insert
                          (m, tv, evalTy (ctx, env, arg)))
                     TypedSyntax.TyVarMap.empty (tyvars, args))
                  handle ListPair.UnequalLengths =>
                    emitFatalError (ctx, [span], "invalid type construction")
              in
                applySubst subst ty
              end
          | evalTy (ctx, env, S.FnType (span, ty1, ty2)) =
              T.FnType (span, evalTy (ctx, env, ty1), evalTy (ctx, env, ty2))
      in
        evalTy
      end

    fun evalPureTy x = evalBaseTy T.applySubstPureTy x
    fun evalTy x = evalBaseTy T.applySubstPureTyAsTy x

    (*: val forceTy : T.Ty -> T.Ty *)
    fun forceTy (T.AnonymousTyVar (_, ref (T.Link ty))) = forceTy ty
      | forceTy (ty as T.RecordExtType (span, fieldTypes, baseTy)) =
          let
            val baseTy = forceTy baseTy
          in
            case baseTy of
              T.RecordType (_, fieldTypes') =>
                T.RecordType
                  (span, Syntax.LabelMap.unionWith #1 (fieldTypes, fieldTypes'))
            | _ => ty
          end
      | forceTy ty = ty

    (*:
    val synthTypeOfPat : InferenceContext * Env * S.Pat * (T.VId * T.Ty) S.VIdMap.map -> T.Ty * (T.VId * T.Ty) S.VIdMap.map * T.Pat
    and checkTypeOfPat : InferenceContext * Env * S.Pat * T.Ty * (T.VId * T.Ty) S.VIdMap.map -> (T.VId * T.Ty) S.VIdMap.map * T.Pat
     *)
    fun synthTypeOfPat (ctx: InferenceContext, _: Env, S.WildcardPat span, vars) :
      T.Ty * (T.VId * T.Ty) S.VIdMap.map * T.Pat =
          let
            val ty = T.AnonymousTyVar
              (span, freshTyVar (ctx, span, false, NONE))
          in
            (ty, vars, T.WildcardPat span)
          end
      | synthTypeOfPat (ctx, _, S.SConPat (span, scon), vars) =
          (case scon of
             Syntax.IntegerConstant _ =>
               let
                 val tv = freshTyVar (ctx, span, true, SOME T.Int)
                 val ty = T.AnonymousTyVar (span, tv)
               in
                 (ty, vars, T.SConPat (span, scon, ty))
               end
           | Syntax.WordConstant _ =>
               let
                 val tv = freshTyVar (ctx, span, true, SOME T.Word)
                 val ty = T.AnonymousTyVar (span, tv)
               in
                 (ty, vars, T.SConPat (span, scon, ty))
               end
           | Syntax.RealConstant _ =>
               emitFatalTypeError
                 (ctx, [span], "no real constant may occur in a pattern")
           | Syntax.CharacterConstant _ =>
               let
                 val tv = freshTyVar (ctx, span, true, SOME T.Char)
                 val ty = T.AnonymousTyVar (span, tv)
               in
                 (ty, vars, T.SConPat (span, scon, ty))
               end
           | Syntax.StringConstant _ =>
               let
                 val tv = freshTyVar (ctx, span, true, SOME T.String)
                 val ty = T.AnonymousTyVar (span, tv)
               in
                 (ty, vars, T.SConPat (span, scon, ty))
               end)
      | synthTypeOfPat
          (ctx, _, S.VarPat (span, vid as S.MkVId "_Prim.ref"), vars) =
          let
            val payloadTy = TypedSyntax.AnonymousTyVar
              (span, freshTyVar (ctx, span, false, NONE))
            val ty = T.FnType
              ( span
              , payloadTy
              , T.TyCon (span, [payloadTy], PrimTypes.Names.ref_)
              )
          in
            ( ty
            , S.VIdMap.insertWith
                (fn (_, y) =>
                   ( emitTypeError
                       (ctx, [span], "duplicate identifier in a pattern")
                   ; y
                   )) (vars, vid, (VId_ref, ty))
            , T.VarPat (span, VId_ref, ty)
            )
          end
      | synthTypeOfPat
          (ctx, _, S.VarPat (span, vid as S.MkVId "_Prim.::"), vars) =
          let
            val elemTy = TypedSyntax.AnonymousTyVar
              (span, freshTyVar (ctx, span, false, NONE))
            val listTy = T.TyCon (span, [elemTy], PrimTypes.Names.list)
            val ty = T.FnType (span, T.PairType (span, elemTy, listTy), listTy)
          in
            ( ty
            , S.VIdMap.insertWith
                (fn (_, y) =>
                   ( emitTypeError
                       (ctx, [span], "duplicate identifier in a pattern")
                   ; y
                   )) (vars, vid, (VId_DCOLON, ty))
            , T.VarPat (span, VId_DCOLON, ty)
            )
          end
      | synthTypeOfPat
          (ctx, _, S.VarPat (span, vid as S.MkVId "_Prim.unit.equal"), vars) =
          let
            val ty =
              T.TyCon
                ( span
                , [PrimTypes.unit, PrimTypes.unit, PrimTypes.bool]
                , PrimTypes.Names.function2
                )
          in
            ( ty
            , S.VIdMap.insertWith
                (fn (_, y) =>
                   ( emitTypeError
                       (ctx, [span], "duplicate identifier in a pattern")
                   ; y
                   )) (vars, vid, (VId_unit_equal, ty))
            , T.VarPat (span, VId_unit_equal, ty)
            )
          end
      | synthTypeOfPat (ctx, env, S.VarPat (span, vid), vars) =
          (case Syntax.VIdMap.find (#valMap env, vid) of
             SOME (_, Syntax.ValueConstructor _, _) =>
               emitFatalTypeError (ctx, [span], "VarPat: invalid pattern")
           | SOME (_, Syntax.ExceptionConstructor, _) =>
               emitFatalTypeError (ctx, [span], "VarPat: invalid pattern")
           | _ =>
               let
                 val ty = TypedSyntax.AnonymousTyVar
                   (span, freshTyVar (ctx, span, false, NONE))
                 val vid' = newVId (#context ctx, vid)
               in
                 ( ty
                 , S.VIdMap.insertWith
                     (fn (_, y) =>
                        ( emitTypeError
                            (ctx, [span], "duplicate identifier in a pattern")
                        ; y
                        )) (vars, vid, (vid', ty))
                 , T.VarPat (span, vid', ty)
                 )
               end)
      | synthTypeOfPat
          (ctx, env, S.RecordPat {sourceSpan, fields, ellipsis}, vars) =
          let
            fun oneField ((label, pat), (fieldTypes, vars, fieldPats)) =
              let
                val (ty, vars, pat') = synthTypeOfPat (ctx, env, pat, vars)
              in
                ( Syntax.LabelMap.insert (fieldTypes, label, ty)
                , vars
                , (label, pat') :: fieldPats
                )
              end
            val (fieldTypes, vars, fieldPats) =
              List.foldr oneField (Syntax.LabelMap.empty, vars, []) fields
          in
            case ellipsis of
              SOME basePat =>
                let
                  val span = S.getSourceSpanOfPat basePat
                  val (baseTy, vars, basePat) =
                    synthTypeOfPat (ctx, env, basePat, vars)
                  val recordTy =
                    T.RecordExtType (sourceSpan, fieldTypes, baseTy)
                  val () =
                    solveUnary
                      ( ctx
                      , env
                      , ConstraintInfo.ACTUAL_EXPECTED
                      , span
                      , baseTy
                      , T.IsRecord (List.map #1 fields)
                      )
                in
                  ( recordTy
                  , vars
                  , T.RecordPat
                      { sourceSpan = sourceSpan
                      , fields = fieldPats
                      , ellipsis = SOME basePat
                      , wholeRecordType = recordTy
                      }
                  )
                end
            | NONE =>
                let
                  val recordTy = T.RecordType (sourceSpan, fieldTypes)
                in
                  ( recordTy
                  , vars
                  , T.RecordPat
                      { sourceSpan = sourceSpan
                      , fields = fieldPats
                      , ellipsis = NONE
                      , wholeRecordType = recordTy
                      }
                  )
                end
          end
      | synthTypeOfPat (ctx, env, S.ConPat (span, longvid, optInnerPat), vars) =
          let
            fun makeBogus () =
              let
                val freshTy = T.AnonymousTyVar
                  (span, freshTyVar (ctx, span, false, NONE))
              in
                case optInnerPat of
                  NONE => (freshTy, vars, T.BogusPat (span, freshTy, []))
                | SOME innerPat =>
                    let
                      val (innerTy, vars, innerPat) =
                        synthTypeOfPat (ctx, env, innerPat, vars)
                    in
                      ( freshTy
                      , vars
                      , T.BogusPat (span, freshTy, [(innerTy, innerPat)])
                      )
                    end
              end
          in
            case lookupLongVIdInEnv (ctx, env, span, longvid) of
              Found (longvid, tysc, idstatus) =>
                let
                  val isConstructor =
                    case idstatus of
                      Syntax.ValueConstructor _ => true
                    | Syntax.ExceptionConstructor => true
                    | _ => false
                in
                  if isConstructor then
                    let
                      val (ty, tyargs) = instantiate (ctx, span, tysc)
                      val valueConstructorInfo =
                        case idstatus of
                          Syntax.ValueConstructor info => SOME info
                        | _ => NONE
                    in
                      case optInnerPat of
                        NONE =>
                          ( ty
                          , vars
                          , T.ConPat
                              { sourceSpan = span
                              , longvid = longvid
                              , payload = NONE
                              , tyargs = List.map #1 tyargs
                              , valueConstructorInfo = valueConstructorInfo
                              }
                          )
                      | SOME innerPat =>
                          (case ty of
                             T.FnType (_, argTy, resultTy) =>
                               let
                                 val (vars, innerPat') = checkTypeOfPat
                                   (ctx, env, innerPat, argTy, vars)
                               in
                                 ( resultTy
                                 , vars
                                 , T.ConPat
                                     { sourceSpan = span
                                     , longvid = longvid
                                     , payload = SOME (argTy, innerPat')
                                     , tyargs = List.map #1 tyargs
                                     , valueConstructorInfo =
                                         valueConstructorInfo
                                     }
                                 )
                               end
                           | _ =>
                               emitFatalTypeError
                                 (ctx, [span], "invalid pattern"))
                    end
                  else (* idstatus = Syntax.ValueVariable *)
                    ( emitTypeError
                        ( ctx
                        , [span]
                        , "invalid pattern; a constructor is expected"
                        )
                    ; makeBogus ()
                    )
                end
            | ValueNotFound notfound =>
                ( emitTypeError
                    ( ctx
                    , [span]
                    , "invalid pattern: constructor '"
                      ^ Syntax.print_LongVId notfound ^ "' not found"
                    )
                ; makeBogus ()
                )
            | StructureNotFound notfound =>
                ( emitTypeError
                    ( ctx
                    , [span]
                    , "invalid pattern: structure name '"
                      ^ Syntax.print_LongStrId notfound ^ "' not found"
                    )
                ; makeBogus ()
                )
          end
      | synthTypeOfPat (ctx, env, S.TypedPat (span, pat, ty), vars) =
          let
            val ty = evalTy (#context ctx, env, ty)
            val (vars, pat) = checkTypeOfPat (ctx, env, pat, ty, vars)
          in
            (ty, vars, T.TypedPat (span, pat, ty))
          end
      | synthTypeOfPat (ctx, env, S.LayeredPat (span, vid, NONE, pat), vars) =
          let
            val (ty, vars, pat) = synthTypeOfPat (ctx, env, pat, vars)
            val vid' = newVId (#context ctx, vid)
          in
            ( ty
            , Syntax.VIdMap.insertWith
                (fn (_, y) =>
                   ( emitTypeError
                       (ctx, [span], "duplicate identifier in a pattern")
                   ; y
                   )) (vars, vid, (vid', ty))
            , T.LayeredPat (span, vid', ty, pat)
            )
          end
      | synthTypeOfPat (ctx, env, S.LayeredPat (span, vid, SOME ty, pat), vars) =
          let
            val ty = evalTy (#context ctx, env, ty)
            val (vars, pat) = checkTypeOfPat (ctx, env, pat, ty, vars)
            val vid' = newVId (#context ctx, vid)
          in
            ( ty
            , Syntax.VIdMap.insertWith
                (fn (_, y) =>
                   ( emitTypeError
                       (ctx, [span], "duplicate identifier in a pattern")
                   ; y
                   )) (vars, vid, (vid', ty))
            , T.LayeredPat (span, vid', ty, pat)
            )
          end
      | synthTypeOfPat (ctx, env, S.VectorPat (span, pats, ellipsis), vars) =
          let
            val (revResults, vars) =
              Vector.foldl
                (fn (pat, (acc, vars)) =>
                   let
                     val (ty, vars, tpat) = synthTypeOfPat (ctx, env, pat, vars)
                   in
                     ((Syntax.getSourceSpanOfPat pat, ty, tpat) :: acc, vars)
                   end) ([], vars) pats
            val results = Vector.fromList (List.rev revResults)
            val elemTy =
              case VectorSlice.getItem (VectorSlice.full results) of
                NONE =>
                  TypedSyntax.AnonymousTyVar
                    (span, freshTyVar (ctx, span, false, NONE))
              | SOME ((_, elemTy0, _), xs) =>
                  VectorSlice.foldl
                    (fn ((elemSpan, elemTy, _), ty) =>
                       commonType
                         ( ctx
                         , env
                         , ConstraintInfo.SEQUENCE
                         , elemSpan
                         , ty
                         , elemTy
                         )) elemTy0 xs
            val pats = Vector.map #3 results
          in
            ( T.TyCon (span, [elemTy], PrimTypes.Names.vector)
            , vars
            , T.VectorPat (span, pats, ellipsis, elemTy)
            )
          end
    and checkTypeOfPat
          (_: InferenceContext, _: Env, S.WildcardPat span, _, vars) :
      (T.VId * T.Ty) S.VIdMap.map * T.Pat = (vars, T.WildcardPat span)
      | checkTypeOfPat (ctx, env, S.SConPat (span, scon), expectedTy, vars) =
          (case scon of
             Syntax.IntegerConstant _ =>
               ( solveUnary
                   ( ctx
                   , env
                   , ConstraintInfo.EXPECTED_ACTUAL
                   , span
                   , expectedTy
                   , T.IsInt
                   )
               ; solveUnary
                   ( ctx
                   , env
                   , ConstraintInfo.EXPECTED_ACTUAL
                   , span
                   , expectedTy
                   , T.IsEqType
                   )
               ; (vars, T.SConPat (span, scon, expectedTy))
               )
           | Syntax.WordConstant _ =>
               ( solveUnary
                   ( ctx
                   , env
                   , ConstraintInfo.EXPECTED_ACTUAL
                   , span
                   , expectedTy
                   , T.IsWord
                   )
               ; solveUnary
                   ( ctx
                   , env
                   , ConstraintInfo.EXPECTED_ACTUAL
                   , span
                   , expectedTy
                   , T.IsEqType
                   )
               ; (vars, T.SConPat (span, scon, expectedTy))
               )
           | Syntax.RealConstant _ =>
               emitFatalTypeError
                 (ctx, [span], "no real constant may occur in a pattern")
           | Syntax.CharacterConstant _ =>
               ( solveUnary
                   ( ctx
                   , env
                   , ConstraintInfo.EXPECTED_ACTUAL
                   , span
                   , expectedTy
                   , T.IsChar
                   )
               ; solveUnary
                   ( ctx
                   , env
                   , ConstraintInfo.EXPECTED_ACTUAL
                   , span
                   , expectedTy
                   , T.IsEqType
                   )
               ; (vars, T.SConPat (span, scon, expectedTy))
               )
           | Syntax.StringConstant _ =>
               ( solveUnary
                   ( ctx
                   , env
                   , ConstraintInfo.EXPECTED_ACTUAL
                   , span
                   , expectedTy
                   , T.IsString
                   )
               ; solveUnary
                   ( ctx
                   , env
                   , ConstraintInfo.EXPECTED_ACTUAL
                   , span
                   , expectedTy
                   , T.IsEqType
                   )
               ; (vars, T.SConPat (span, scon, expectedTy))
               ))
      | checkTypeOfPat
          ( ctx
          , env
          , pat as S.VarPat (span, S.MkVId "_Prim.ref")
          , expectedTy
          , vars
          ) =
          let
            val (actualTy, vars, pat) = synthTypeOfPat (ctx, env, pat, vars)
          in
            checkSubsumption (ctx, env, span, actualTy, expectedTy);
            (vars, pat)
          end
      | checkTypeOfPat
          ( ctx
          , env
          , pat as S.VarPat (span, S.MkVId "_Prim.::")
          , expectedTy
          , vars
          ) =
          let
            val (actualTy, vars, pat) = synthTypeOfPat (ctx, env, pat, vars)
          in
            checkSubsumption (ctx, env, span, actualTy, expectedTy);
            (vars, pat)
          end
      | checkTypeOfPat
          ( ctx
          , env
          , pat as S.VarPat (span, S.MkVId "_Prim.unit.equal")
          , expectedTy
          , vars
          ) =
          let
            val (actualTy, vars, pat) = synthTypeOfPat (ctx, env, pat, vars)
          in
            checkSubsumption (ctx, env, span, actualTy, expectedTy);
            (vars, pat)
          end
      | checkTypeOfPat (ctx, env, S.VarPat (span, vid), expectedTy, vars) =
          (case Syntax.VIdMap.find (#valMap env, vid) of
             SOME (_, Syntax.ValueConstructor _, _) =>
               emitFatalTypeError (ctx, [span], "VarPat: invalid pattern")
           | SOME (_, Syntax.ExceptionConstructor, _) =>
               emitFatalTypeError (ctx, [span], "VarPat: invalid pattern")
           | _ =>
               let
                 val vid' = newVId (#context ctx, vid)
               in
                 ( S.VIdMap.insertWith
                     (fn (_, y) =>
                        ( emitTypeError
                            (ctx, [span], "duplicate identifier in a pattern")
                        ; y
                        )) (vars, vid, (vid', expectedTy))
                 , T.VarPat (span, vid', expectedTy)
                 )
               end)
      | checkTypeOfPat
          ( ctx
          , env
          , pat as S.RecordPat {sourceSpan, fields, ellipsis}
          , expectedTy
          , vars
          ) =
          (case forceTy expectedTy of
             T.RecordType (span', fieldTypes) =>
               let
                 fun oneField ((label, pat), (vars, fieldPats)) =
                   case Syntax.LabelMap.find (fieldTypes, label) of
                     SOME ty =>
                       let
                         val (vars, pat') = checkTypeOfPat
                           (ctx, env, pat, ty, vars)
                       in
                         (vars, (label, pat') :: fieldPats)
                       end
                   | NONE =>
                       ( emitTypeError
                           ( ctx
                           , [sourceSpan]
                           , "expected " ^ Syntax.print_Label label
                             ^ " to be present"
                           )
                       ; (vars, fieldPats)
                       )
                 val (vars, fieldPats) = List.foldr oneField (vars, []) fields
                 val baseFields =
                   List.foldl
                     (fn ((label, _), map) =>
                        case Syntax.LabelMap.findAndRemove (map, label) of
                          SOME (map, _) => map
                        | NONE => map) fieldTypes fields
               in
                 case ellipsis of
                   SOME basePat =>
                     let
                       val (vars, basePat) = checkTypeOfPat
                         ( ctx
                         , env
                         , basePat
                         , T.RecordType (span', baseFields)
                         , vars
                         )
                     in
                       ( vars
                       , T.RecordPat
                           { sourceSpan = sourceSpan
                           , fields = fieldPats
                           , ellipsis = SOME basePat
                           , wholeRecordType = expectedTy
                           }
                       )
                     end
                 | NONE =>
                     ( if Syntax.LabelMap.isEmpty baseFields then
                         ()
                       else
                         emitTypeError
                           ( ctx
                           , [sourceSpan]
                           , "missing fields: "
                             ^
                             Syntax.print_list Syntax.print_Label
                               (Syntax.LabelMap.foldri
                                  (fn (label, _, acc) => label :: acc) []
                                  baseFields)
                           )
                     ; ( vars
                       , T.RecordPat
                           { sourceSpan = sourceSpan
                           , fields = fieldPats
                           , ellipsis = NONE
                           , wholeRecordType = expectedTy
                           }
                       )
                     )
               end
           | _ =>
               let
                 val span = S.getSourceSpanOfPat pat
                 val (actualTy, vars, pat) =
                   synthTypeOfPat (ctx, env, pat, vars)
               in
                 checkSubsumption (ctx, env, span, actualTy, expectedTy);
                 (vars, pat)
               end)
      | checkTypeOfPat
          (ctx, env, S.LayeredPat (span, vid, NONE, pat), expectedTy, vars) =
          let
            val (vars, pat) = checkTypeOfPat (ctx, env, pat, expectedTy, vars)
            val vid' = newVId (#context ctx, vid)
          in
            ( Syntax.VIdMap.insertWith
                (fn (_, y) =>
                   ( emitTypeError
                       (ctx, [span], "duplicate identifier in a pattern")
                   ; y
                   )) (vars, vid, (vid', expectedTy))
            , T.LayeredPat (span, vid', expectedTy, pat)
            )
          end
      | checkTypeOfPat
          ( ctx
          , env
          , pat as S.VectorPat (span, pats, ellipsis)
          , expectedTy
          , vars
          ) =
          (case forceTy expectedTy of
             T.TyCon (_, [elemTy], con) =>
               if T.eqTyName (con, PrimTypes.Names.vector) then
                 let
                   val (revPats, vars) =
                     Vector.foldl
                       (fn (pat, (acc, vars)) =>
                          let
                            val (vars, pat) = checkTypeOfPat
                              (ctx, env, pat, elemTy, vars)
                          in
                            (pat :: acc, vars)
                          end) ([], vars) pats
                   val pats = Vector.fromList (List.rev revPats)
                 in
                   (vars, T.VectorPat (span, pats, ellipsis, elemTy))
                 end
               else
                 let
                   val (actualTy, vars, pat) =
                     synthTypeOfPat (ctx, env, pat, vars)
                 in
                   checkSubsumption (ctx, env, span, actualTy, expectedTy);
                   (vars, pat)
                 end
           | _ =>
               let
                 val (actualTy, vars, pat) =
                   synthTypeOfPat (ctx, env, pat, vars)
               in
                 checkSubsumption (ctx, env, span, actualTy, expectedTy);
                 (vars, pat)
               end)
      | checkTypeOfPat (ctx, env, pat, expectedTy, vars) =
          let
            val span = S.getSourceSpanOfPat pat
            val (actualTy, vars, pat) = synthTypeOfPat (ctx, env, pat, vars)
          in
            checkSubsumption (ctx, env, span, actualTy, expectedTy);
            (vars, pat)
          end

    fun doWithtype (ctx, _ (* Env *), typbinds: S.TypBind list) : S.ConBind
                                                                  -> S.ConBind =
      let
        val map =
          List.foldl
            (fn (S.TypBind (_, tyvars, tycon, ty), map) =>
               S.TyConMap.insert (map, tycon, (tyvars, ty))) S.TyConMap.empty
            typbinds
        fun goTyAlias env (ty as S.TyVar (_, tv)) =
              (case S.TyVarMap.find (env, tv) of
                 SOME ty => ty
               | NONE => ty)
          | goTyAlias env (S.RecordType (span, fields, optBaseTy)) =
              S.RecordType
                ( span
                , List.map (fn (label, ty) => (label, goTyAlias env ty)) fields
                , Option.map (goTyAlias env) optBaseTy
                )
          | goTyAlias env (S.TyCon (span, tyargs, longtycon)) =
              S.TyCon (span, List.map (goTyAlias env) tyargs, longtycon)
          | goTyAlias env (S.FnType (span, s, t)) =
              S.FnType (span, goTyAlias env s, goTyAlias env t)
        fun goTy (ty as S.TyVar (_, _)) = ty
          | goTy (S.RecordType (span, fields, optBaseTy)) =
              S.RecordType
                ( span
                , List.map (fn (label, ty) => (label, goTy ty)) fields
                , Option.map goTy optBaseTy
                )
          | goTy (S.TyCon (span, tyargs, Syntax.MkQualified ([], tycon))) =
              let
                val tyargs = List.map goTy tyargs
              in
                case S.TyConMap.find (map, tycon) of
                  SOME (tyvars, ty) =>
                    let
                      val env' =
                        ListPair.foldlEq
                          (fn (tv, tyarg, m) => S.TyVarMap.insert (m, tv, tyarg))
                          S.TyVarMap.empty (tyvars, tyargs)
                        handle ListPair.UnequalLengths =>
                          emitFatalError
                            ( ctx
                            , [span]
                            , "invalid type construction: arity mismatch"
                            )
                    in
                      goTyAlias env' ty
                    end
                | NONE => S.TyCon (span, tyargs, S.MkQualified ([], tycon))
              end
          | goTy (S.TyCon (span, tyargs, longtycon)) =
              S.TyCon (span, List.map goTy tyargs, longtycon)
          | goTy (S.FnType (span, s, t)) =
              S.FnType (span, goTy s, goTy t)
        fun goConBind (conbind as S.ConBind (_, _, NONE)) = conbind
          | goConBind (S.ConBind (span, vid, SOME payloadTy)) =
              S.ConBind (span, vid, SOME (goTy payloadTy))
      in
        goConBind
      end

    fun determineDatatypeEquality
      ( ctx
      , env: ('val, 'str) Env'
      , datbinds: (S.TyVar list * S.Ty list) S.TyConMap.map
      ) : bool S.TyConMap.map =
      let
        val localTyCons =
          S.TyConMap.foldli (fn (tycon, _, s) => S.TyConSet.add (s, tycon))
            S.TyConSet.empty datbinds
        val graph: (S.TyConSet.set ref) S.TyConMap.map =
          Syntax.TyConMap.map (fn _ => ref S.TyConSet.empty) datbinds
        val nonEqualitySet = ref S.TyConSet.empty
        fun doDatBind (tycon, (tyvars, payloads)) =
          let
            fun doTy (S.TyVar (_, tv)) =
                  if List.exists (fn tv' => tv = tv') tyvars then
                    SOME []
                  else
                    (case tv of
                       S.MkTyVar name =>
                         if String.isPrefix "''" name then SOME [] else NONE)
              | doTy (S.RecordType (_, fields, NONE)) =
                  doTypes (List.map #2 fields)
              | doTy (S.RecordType (_, fields, SOME baseTy)) =
                  (case doTypes (List.map #2 fields) of
                     SOME xs =>
                       (case doTy baseTy of
                          SOME ys => SOME (xs @ ys)
                        | none as NONE => none)
                   | none as NONE => none)
              | doTy (S.TyCon (span, tyargs, longtycon)) =
                  let
                    val l =
                      case longtycon of
                        Syntax.MkQualified ([], tycon) =>
                          if S.TyConSet.member (localTyCons, tycon) then
                            SOME [tycon]
                          else
                            NONE
                      | _ => NONE
                  in
                    case l of
                      result as SOME _ => result
                    | NONE =>
                        let
                          val {typeFunction = T.TypeFunction (tyvars, ty), ...} =
                            lookupTyConInEnv (ctx, env, span, longtycon)
                          val tyVarMap =
                            ListPair.foldlEq
                              (fn (tv, ty, m) => T.TyVarMap.insert (m, tv, ty))
                              T.TyVarMap.empty (tyvars, tyargs)
                          fun doUTy (T.TyVar (_, tv)) =
                                (case T.TyVarMap.find (tyVarMap, tv) of
                                   SOME ty => doTy ty
                                 | NONE =>
                                     if T.tyVarAdmitsEquality tv then SOME []
                                     else NONE)
                            | doUTy (T.AnonymousTyVar _) =
                                NONE (* should not occur *)
                            | doUTy (T.RecordType (_, fields)) =
                                doUTypes
                                  (Syntax.LabelMap.foldl (op::) [] fields)
                            | doUTy (T.RecordExtType (_, fields, baseTy)) =
                                doUTypes
                                  (Syntax.LabelMap.foldl (op::) [baseTy] fields) (* should not occur *)
                            | doUTy (T.TyCon (span, tyargs, tyname)) =
                                if isRefOrArray tyname then
                                  SOME []
                                else
                                  let
                                    val {admitsEquality, ...} =
                                      lookupTyNameInEnv (ctx, env, span, tyname)
                                  in
                                    if admitsEquality then doUTypes tyargs
                                    else NONE
                                  end
                            | doUTy (T.FnType _) = NONE
                          and doUTypes types =
                            let
                              fun go (acc, ty :: types) =
                                    (case doUTy ty of
                                       NONE => NONE
                                     | SOME xs => go (xs @ acc, types))
                                | go (acc, []) = SOME acc
                            in
                              go ([], types)
                            end
                        in
                          doUTy ty
                        end
                  end
              | doTy (S.FnType _) = NONE
            and doTypes types =
              let
                fun go (acc, ty :: types) =
                      (case doTy ty of
                         NONE => NONE
                       | SOME xs => go (xs @ acc, types))
                  | go (acc, []) = SOME acc
              in
                go ([], types)
              end
          in
            case doTypes payloads of
              NONE => nonEqualitySet := S.TyConSet.add (!nonEqualitySet, tycon)
            | SOME xs =>
                List.app
                  (fn member => let val r = S.TyConMap.lookup (graph, member)
                                in r := S.TyConSet.add (!r, tycon)
                                end) xs
          end
        fun dfs tycon =
          let
            val set = !(S.TyConMap.lookup (graph, tycon))
          in
            S.TyConSet.app
              (fn t =>
                 let
                   val s = !nonEqualitySet
                 in
                   if S.TyConSet.member (s, t) then ()
                   else (nonEqualitySet := S.TyConSet.add (s, t); dfs t)
                 end) set
          end
        val () = S.TyConMap.appi doDatBind datbinds
        val () = S.TyConSet.app dfs (!nonEqualitySet)
        val nonEqualitySet = !nonEqualitySet
      in
        S.TyConSet.foldl
          (fn (tycon, map) =>
             S.TyConMap.insert (map, tycon, not
               (S.TyConSet.member (nonEqualitySet, tycon)))) S.TyConMap.empty
          localTyCons
      end

    fun nameFromPat (T.VarPat (_, T.MkVId (name, _), _)) = name
      | nameFromPat
          (T.RecordPat
             {sourceSpan = _, fields, ellipsis = _, wholeRecordType = _}) =
          Syntax.SourceName.record
            (List.map (fn (label, pat) => (label, nameFromPat pat)) fields)
      | nameFromPat (T.TypedPat (_, pat, _)) = nameFromPat pat
      | nameFromPat (T.LayeredPat (_, vid, _, innerPat)) =
          Syntax.SourceName.merge
            (TypedSyntax.getVIdName vid, nameFromPat innerPat)
      | nameFromPat _ = Syntax.SourceName.absent


    (*:
    val synthTypeOfExp : InferenceContext * Env * S.Exp -> T.Ty * T.Exp
    and checkTypeOfExp : InferenceContext * Env * S.Exp * T.Ty -> T.Exp
    and typeCheckDec : InferenceContext * Env * S.Dec -> (* created environment *) Env * T.Dec list
    and typeCheckDecs : InferenceContext * Env * S.Dec list -> (* created environment *) Env * T.Dec list
    and synthTypeOfMatch : InferenceContext * Env * SourcePos.span * (S.Pat * S.Exp) list -> (* pattern type *) T.Ty * (* expression type *) T.Ty * (T.Pat * T.Exp) list
    and checkAndSynthTypeOfMatch : InferenceContext * Env * SourcePos.span * (S.Pat * S.Exp) list * (* pattern type *) T.Ty -> (* expression type *) T.Ty * (T.Pat * T.Exp) list
    and checkTypeOfMatch : InferenceContext * Env * (S.Pat * S.Exp) list * (* pattern type *) T.Ty * (* expression type *) T.Ty -> (T.Pat * T.Exp) list
     *)
    fun synthTypeOfExp (ctx: InferenceContext, _: Env, S.SConExp (span, scon)) :
      T.Ty * T.Exp =
          let
            val ty =
              case scon of
                Syntax.IntegerConstant _ =>
                  let val tv = freshTyVar (ctx, span, false, SOME T.Int)
                  in T.AnonymousTyVar (span, tv)
                  end
              | Syntax.WordConstant _ =>
                  let val tv = freshTyVar (ctx, span, false, SOME T.Word)
                  in T.AnonymousTyVar (span, tv)
                  end
              | Syntax.RealConstant _ =>
                  PrimTypes.real (* TODO: overloaded literals *)
              | Syntax.CharacterConstant _ =>
                  let val tv = freshTyVar (ctx, span, false, SOME T.Char)
                  in T.AnonymousTyVar (span, tv)
                  end
              | Syntax.StringConstant _ =>
                  let val tv = freshTyVar (ctx, span, false, SOME T.String)
                  in T.AnonymousTyVar (span, tv)
                  end
          in
            (ty, T.SConExp (span, scon, ty))
          end
      | synthTypeOfExp (ctx, env, S.VarExp (span, longvid)) =
          (case lookupLongVIdInEnv (ctx, env, span, longvid) of
             Found (longvid, tysc, ids) =>
               let val (ty, tyargs) = instantiate (ctx, span, tysc)
               in (ty, T.VarExp (span, longvid, ids, tyargs))
               end
           | ValueNotFound notfound =>
               let
                 val () = emitTypeError
                   ( ctx
                   , [span]
                   , "unknown value name " ^ Syntax.print_LongVId notfound
                   )
                 val ty = T.AnonymousTyVar
                   (span, freshTyVar (ctx, span, false, NONE))
               in
                 (ty, T.BogusExp (span, ty))
               end
           | StructureNotFound notfound =>
               let
                 val () = emitTypeError
                   ( ctx
                   , [span]
                   , "unknown structure name " ^ Syntax.print_LongStrId notfound
                   )
                 val ty = T.AnonymousTyVar
                   (span, freshTyVar (ctx, span, false, NONE))
               in
                 (ty, T.BogusExp (span, ty))
               end)
      | synthTypeOfExp (ctx, env, S.RecordExp (span, fields, NONE)) =
          let
            fun doField ((label, exp), (typeMap, expList)) =
              let
                val (ty, exp) = synthTypeOfExp (ctx, env, exp)
              in
                ( Syntax.LabelMap.insert (typeMap, label, ty)
                , (label, exp) :: expList
                )
              end
            val (fieldTypes, revFields) =
              List.foldl doField (Syntax.LabelMap.empty, []) fields
          in
            ( T.RecordType (span, fieldTypes)
            , T.RecordExp (span, List.rev revFields)
            )
          end
      | synthTypeOfExp (ctx, env, S.RecordExp (span, fields, SOME baseExp)) =
          let
            val baseSpan = S.getSourceSpanOfExp baseExp
            val (baseTy, baseExp) = synthTypeOfExp (ctx, env, baseExp)
            val () =
              solveUnary
                ( ctx
                , env
                , ConstraintInfo.ACTUAL_EXPECTED
                , baseSpan
                , baseTy
                , T.IsRecord (List.map #1 fields)
                )
            fun doField ((label, exp), (typeMap, expList)) =
              let
                val (ty, exp) = synthTypeOfExp (ctx, env, exp)
              in
                ( Syntax.LabelMap.insert (typeMap, label, ty)
                , (label, exp) :: expList
                )
              end
            val (fieldTypes, revFields) =
              List.foldl doField (Syntax.LabelMap.empty, []) fields
            val recordTy =
              case forceTy baseTy of
                T.RecordType (span, fieldTypes') =>
                  T.RecordType
                    ( span
                    , Syntax.LabelMap.unionWith #1 (fieldTypes, fieldTypes')
                    )
              | baseTy => T.RecordExtType (span, fieldTypes, baseTy)
          in
            ( recordTy
            , T.RecordExtExp
                { sourceSpan = span
                , fields = List.rev revFields
                , baseExp = baseExp
                , baseTy = baseTy
                }
            )
          end
      | synthTypeOfExp (ctx, env, S.LetInExp (span, decs, innerExp)) =
          let
            val (env', decs) = typeCheckDecs (ctx, env, decs)
            val (ty, innerExp) = synthTypeOfExp
              (ctx, mergeEnv (env, env'), innerExp)
          in
            (ty, T.LetInExp (span, decs, innerExp))
          end
      | synthTypeOfExp
          (ctx, env, S.AppExp (span, S.ProjectionExp (span', label), record)) =
          let
            val (recordTy, record) = synthTypeOfExp (ctx, env, record)
            val recordTy = forceTy recordTy
            fun withFieldTy fieldTy =
              ( fieldTy
              , T.AppExp
                  ( span
                  , T.ProjectionExp
                      { sourceSpan = span'
                      , label = label
                      , recordTy = recordTy
                      , fieldTy = fieldTy
                      }
                  , record
                  )
              )
            fun generalCase () =
              let
                val fieldTy = T.AnonymousTyVar
                  (span, freshTyVar (ctx, span, false, NONE))
                val baseTy =
                  T.AnonymousTyVar (span, freshTyVar (ctx, span, false, SOME
                    (T.Record (Syntax.LabelSet.singleton label))))
                val recordTy' = T.RecordExtType
                  (span, Syntax.LabelMap.singleton (label, fieldTy), baseTy)
                val () = checkSubsumption (ctx, env, span, recordTy, recordTy')
              in
                withFieldTy fieldTy
              end
          in
            case recordTy of
              T.RecordType (_, fieldTypes) =>
                (case Syntax.LabelMap.find (fieldTypes, label) of
                   SOME fieldTy => withFieldTy fieldTy
                 | NONE => generalCase ())
            | T.RecordExtType (_, fieldTypes, _) =>
                (case Syntax.LabelMap.find (fieldTypes, label) of
                   SOME fieldTy => withFieldTy fieldTy
                 | NONE => generalCase ())
            | _ => generalCase ()
          end
      | synthTypeOfExp (ctx, env, S.AppExp (span, f, arg)) =
          let
            val (funTy, f) = synthTypeOfExp (ctx, env, f)
          in
            case forceTy funTy of
              T.FnType (_, argTy, resultTy) =>
                ( resultTy
                , T.AppExp (span, f, checkTypeOfExp (ctx, env, arg, argTy))
                )
            | funTy =>
                let
                  val (argTy, arg) = synthTypeOfExp (ctx, env, arg)
                  val resultTy = T.AnonymousTyVar
                    (span, freshTyVar (ctx, span, false, NONE))
                  val () = checkSubsumption
                    (ctx, env, span, funTy, T.FnType (span, argTy, resultTy))
                in
                  (resultTy, T.AppExp (span, f, arg))
                end
          end
      | synthTypeOfExp (ctx, env, S.TypedExp (span, exp, ty)) =
          let
            val ty = evalTy (#context ctx, env, ty)
            val exp = checkTypeOfExp (ctx, env, exp, ty)
          in
            (ty, T.TypedExp (span, exp, ty))
          end
      | synthTypeOfExp
          ( ctx
          , env
          , S.HandleExp (span, exp, matches)
          ) (* exp : t, matches : exn -> t *) =
          let
            val (expTy, exp) = synthTypeOfExp (ctx, env, exp)
            val matches = checkTypeOfMatch
              (ctx, env, matches, PrimTypes.exn, expTy)
          in
            (expTy, T.HandleExp (span, exp, matches, expTy))
          end
      | synthTypeOfExp (ctx, env, S.RaiseExp (span, exp)) =
          let
            val exp = checkTypeOfExp (ctx, env, exp, PrimTypes.exn)
            val resultTy = T.AnonymousTyVar
              (span, freshTyVar (ctx, span, false, NONE))
          in
            (resultTy, T.RaiseExp (span, resultTy, exp))
          end
      | synthTypeOfExp
          (ctx, env, S.IfThenElseExp (span, cond, thenPart, elsePart)) =
          let
            val elseSpan = S.getSourceSpanOfExp elsePart
            val cond = checkTypeOfExp (ctx, env, cond, PrimTypes.bool)
            val (thenTy, thenPart) = synthTypeOfExp (ctx, env, thenPart)
            val (elseTy, elsePart) = synthTypeOfExp (ctx, env, elsePart)
            val resultTy = commonType
              (ctx, env, ConstraintInfo.BRANCH, elseSpan, thenTy, elseTy)
          in
            (resultTy, T.IfThenElseExp (span, cond, thenPart, elsePart))
          end
      | synthTypeOfExp (ctx, env, S.CaseExp (span, exp, matches)) =
          let
            val (expTy, exp) = synthTypeOfExp (ctx, env, exp)
            val (retTy, matches) = checkAndSynthTypeOfMatch
              (ctx, env, span, matches, expTy)
          in
            ( retTy
            , T.CaseExp
                { sourceSpan = span
                , subjectExp = exp
                , subjectTy = expTy
                , matches = matches
                , matchType = T.CASE
                , resultTy = retTy
                }
            )
          end
      | synthTypeOfExp (ctx, env, S.FnExp (span, matches)) =
          let
            val (argTy, retTy, matches) =
              synthTypeOfMatch (ctx, env, span, matches)
            val fnExp =
              case matches of
                [(T.VarPat (_, vid, _), body)] =>
                  T.FnExp (span, vid, argTy, body)
              | _ =>
                  let
                    val paramName =
                      List.foldr
                        (fn ((p, _), acc) =>
                           Syntax.SourceName.merge (nameFromPat p, acc))
                        (Syntax.SourceName.fromString "a") matches
                    val vid = newVIdWithName (#context ctx, paramName)
                  in
                    T.FnExp (span, vid, argTy, T.CaseExp
                      { sourceSpan = span
                      , subjectExp =
                          T.VarExp
                            (span, T.MkShortVId vid, Syntax.ValueVariable, [])
                      , subjectTy = argTy
                      , matches = matches
                      , matchType = T.CASE
                      , resultTy = retTy
                      })
                  end
          in
            (T.FnType (span, argTy, retTy), fnExp)
          end
      | synthTypeOfExp (ctx, _, S.ProjectionExp (span, label)) =
          let
            val fieldTy = TypedSyntax.AnonymousTyVar
              (span, freshTyVar (ctx, span, false, NONE))
            val baseTy =
              T.AnonymousTyVar (span, freshTyVar (ctx, span, false, SOME
                (T.Record (Syntax.LabelSet.singleton label))))
            val recordTy = T.RecordExtType
              (span, Syntax.LabelMap.singleton (label, fieldTy), baseTy)
          in
            ( T.FnType (span, recordTy, fieldTy)
            , T.ProjectionExp
                { sourceSpan = span
                , label = label
                , recordTy = recordTy
                , fieldTy = fieldTy
                }
            )
          end
      | synthTypeOfExp (ctx, env, S.ListExp (span, xs)) =
          let
            val ys =
              Vector.map
                (fn exp =>
                   ( Syntax.getSourceSpanOfExp exp
                   , synthTypeOfExp (ctx, env, exp)
                   )) xs
            val elemTy =
              case VectorSlice.getItem (VectorSlice.full ys) of
                NONE =>
                  TypedSyntax.AnonymousTyVar
                    (span, freshTyVar (ctx, span, false, NONE))
              | SOME ((_, (elemTy0, _)), rest) =>
                  VectorSlice.foldl
                    (fn ((elemSpan, (elemTy, _)), ty) =>
                       commonType
                         ( ctx
                         , env
                         , ConstraintInfo.SEQUENCE
                         , elemSpan
                         , ty
                         , elemTy
                         )) elemTy0 rest
            val xs' = Vector.map (#2 o #2) ys
          in
            ( T.TyCon (span, [elemTy], PrimTypes.Names.list)
            , T.ListExp (span, xs', elemTy)
            )
          end
      | synthTypeOfExp (ctx, env, S.VectorExp (span, xs)) =
          let
            val ys =
              Vector.map
                (fn exp =>
                   ( Syntax.getSourceSpanOfExp exp
                   , synthTypeOfExp (ctx, env, exp)
                   )) xs
            val elemTy =
              case VectorSlice.getItem (VectorSlice.full ys) of
                NONE =>
                  TypedSyntax.AnonymousTyVar
                    (span, freshTyVar (ctx, span, false, NONE))
              | SOME ((_, (elemTy0, _)), rest) =>
                  VectorSlice.foldl
                    (fn ((elemSpan, (elemTy, _)), ty) =>
                       commonType
                         ( ctx
                         , env
                         , ConstraintInfo.SEQUENCE
                         , elemSpan
                         , ty
                         , elemTy
                         )) elemTy0 rest
            val xs' = Vector.map (#2 o #2) ys
          in
            ( T.TyCon (span, [elemTy], PrimTypes.Names.vector)
            , T.VectorExp (span, xs', elemTy)
            )
          end
      | synthTypeOfExp (ctx, env, S.PrimExp (span, primOp, tyargs, args)) =
          let
            val () =
              if Vector.length tyargs = 0 then
                ()
              else
                emitTypeError
                  ( ctx
                  , [span]
                  , "type arguments to _primCall is not supported currently"
                  )
            val {vars = typeVariables, args = argTypes, results = resultTypes} =
              TypeOfPrimitives.typeOf primOp
            val resultType =
              case resultTypes of
                [t] => t
              | _ => TypedSyntax.TupleType (SourcePos.nullSpan, resultTypes)
            val () =
              if Vector.length args = Vector.length argTypes then
                ()
              else
                emitTypeError
                  ( ctx
                  , [span]
                  , "wrong number of arguments to _primCall; expected "
                    ^ Int.toString (Vector.length argTypes) ^ ", but got "
                    ^ Int.toString (Vector.length args)
                  )
            val (subst, tyargs) =
              List.foldr
                (fn ((v, equality), (m, rest)) =>
                   let
                     val tv = freshTyVar (ctx, span, equality, NONE)
                     val tyarg = T.AnonymousTyVar (span, tv)
                   in
                     (T.TyVarMap.insert (m, v, tyarg), tyarg :: rest)
                   end) (T.TyVarMap.empty, []) typeVariables
            val argTypes = Vector.map (applySubstTy subst) argTypes
            val resultType = applySubstTy subst resultType
            val args =
              Vector.mapi
                (fn (i, argTy) =>
                   checkTypeOfExp (ctx, env, Vector.sub (args, i), argTy))
                argTypes
          in
            (resultType, T.PrimExp (span, primOp, vector tyargs, args))
          end
      | synthTypeOfExp (ctx, env, S.SequentialExp (span, xs, y)) =
          let
            val decs =
              Vector.foldr
                (fn (x, decs) =>
                   let
                     val span = S.getSourceSpanOfExp x
                     val (ty, x) = synthTypeOfExp (ctx, env, x)
                   in
                     if
                       #sequenceNonUnit (#languageOptions (#context ctx))
                       = LanguageOptions.IGNORE
                     then
                       T.IgnoreDec (span, x, ty) :: decs
                     else
                       T.IgnoreDec (span, x, ty)
                       ::
                       T.ValDescDec
                         { sourceSpan = span
                         , expected = T.TypeScheme ([], PrimTypes.unit)
                         , actual = T.TypeScheme ([], ty)
                         , origin = T.VALDESC_SEQUENCE
                         } :: decs
                   end) [] xs
            val (resultType, y) = synthTypeOfExp (ctx, env, y)
          in
            (resultType, T.LetInExp (span, decs, y))
          end
    and checkTypeOfExp (ctx, env, S.SConExp (span, scon), expectedTy: T.Ty) :
      T.Exp =
          ( case scon of
              Syntax.IntegerConstant _ =>
                solveUnary
                  ( ctx
                  , env
                  , ConstraintInfo.EXPECTED_ACTUAL
                  , span
                  , expectedTy
                  , T.IsInt
                  )
            | Syntax.WordConstant _ =>
                solveUnary
                  ( ctx
                  , env
                  , ConstraintInfo.EXPECTED_ACTUAL
                  , span
                  , expectedTy
                  , T.IsWord
                  )
            | Syntax.RealConstant _ =>
                checkSubsumption
                  ( ctx
                  , env
                  , span
                  , PrimTypes.real
                  , expectedTy
                  ) (* TODO: overloaded literals *)
            | Syntax.CharacterConstant _ =>
                solveUnary
                  ( ctx
                  , env
                  , ConstraintInfo.EXPECTED_ACTUAL
                  , span
                  , expectedTy
                  , T.IsChar
                  )
            | Syntax.StringConstant _ =>
                solveUnary
                  ( ctx
                  , env
                  , ConstraintInfo.EXPECTED_ACTUAL
                  , span
                  , expectedTy
                  , T.IsString
                  )
          ; T.SConExp (span, scon, expectedTy)
          )
      | checkTypeOfExp
          (ctx, env, exp as S.RecordExp (span, fields, NONE), expectedTy) =
          (case forceTy expectedTy of
             T.RecordType (_, fieldTypes) =>
               let
                 fun doField (label, exp) =
                   case Syntax.LabelMap.find (fieldTypes, label) of
                     SOME fieldType =>
                       SOME (label, checkTypeOfExp (ctx, env, exp, fieldType))
                   | NONE =>
                       ( emitTypeError
                           ( ctx
                           , [span]
                           , "extra field: " ^ Syntax.print_Label label
                           )
                       ; NONE
                       )
                 val extraFields =
                   List.foldl
                     (fn ((label, _), map) =>
                        case Syntax.LabelMap.findAndRemove (map, label) of
                          SOME (map, _) => map
                        | NONE => map) fieldTypes fields
                 val () =
                   if Syntax.LabelMap.isEmpty extraFields then
                     ()
                   else
                     emitTypeError
                       ( ctx
                       , [span]
                       , "missing fields: "
                         ^
                         Syntax.print_list Syntax.print_Label
                           (Syntax.LabelMap.foldri
                              (fn (label, _, acc) => label :: acc) []
                              extraFields)
                       )
               in
                 T.RecordExp (span, List.mapPartial doField fields)
               end
           | expectedTy =>
               let
                 val (actualTy, exp) = synthTypeOfExp (ctx, env, exp)
                 val () = checkSubsumption
                   (ctx, env, span, actualTy, expectedTy)
               in
                 exp
               end)
      | checkTypeOfExp
          ( ctx
          , env
          , exp as S.RecordExp (span, fields, SOME baseExp)
          , expectedTy
          ) =
          (case forceTy expectedTy of
             T.RecordType (span', fieldTypes) =>
               let
                 fun doField (label, exp) =
                   case Syntax.LabelMap.find (fieldTypes, label) of
                     SOME fieldType =>
                       SOME (label, checkTypeOfExp (ctx, env, exp, fieldType))
                   | NONE =>
                       ( emitTypeError
                           ( ctx
                           , [span]
                           , "extra field: " ^ Syntax.print_Label label
                           )
                       ; NONE
                       )
                 val fields = List.mapPartial doField fields
                 val baseFields =
                   List.foldl
                     (fn ((label, _), map) =>
                        case Syntax.LabelMap.findAndRemove (map, label) of
                          SOME (map, _) => map
                        | NONE => map) fieldTypes fields
                 val baseTy = T.RecordType (span', baseFields)
                 val baseExp = checkTypeOfExp (ctx, env, baseExp, baseTy)
               in
                 T.RecordExtExp
                   { sourceSpan = span
                   , fields = fields
                   , baseExp = baseExp
                   , baseTy = baseTy
                   }
               end
           | expectedTy =>
               let
                 val (actualTy, exp) = synthTypeOfExp (ctx, env, exp)
                 val () = checkSubsumption
                   (ctx, env, span, actualTy, expectedTy)
               in
                 exp
               end)
      | checkTypeOfExp (ctx, env, S.LetInExp (span, decs, innerExp), expectedTy) =
          let
            val (env', decs) = typeCheckDecs (ctx, env, decs)
            val innerExp = checkTypeOfExp
              (ctx, mergeEnv (env, env'), innerExp, expectedTy)
          in
            T.LetInExp (span, decs, innerExp)
          end
      | checkTypeOfExp
          ( ctx
          , env
          , S.HandleExp (span, exp, matches)
          , expectedTy
          ) (* exp : t, matches : exn -> t *) =
          let
            val exp = checkTypeOfExp (ctx, env, exp, expectedTy)
            val matches = checkTypeOfMatch
              (ctx, env, matches, PrimTypes.exn, expectedTy)
          in
            T.HandleExp (span, exp, matches, expectedTy)
          end
      | checkTypeOfExp (ctx, env, S.RaiseExp (span, exp), expectedTy) =
          let val exp = checkTypeOfExp (ctx, env, exp, PrimTypes.exn)
          in T.RaiseExp (span, expectedTy, exp)
          end
      | checkTypeOfExp
          ( ctx
          , env
          , S.IfThenElseExp (span, cond, thenPart, elsePart)
          , expectedTy
          ) =
          let
            val cond = checkTypeOfExp (ctx, env, cond, PrimTypes.bool)
            val thenPart = checkTypeOfExp (ctx, env, thenPart, expectedTy)
            val elsePart = checkTypeOfExp (ctx, env, elsePart, expectedTy)
          in
            T.IfThenElseExp (span, cond, thenPart, elsePart)
          end
      | checkTypeOfExp (ctx, env, S.CaseExp (span, exp, matches), expectedTy) =
          let
            val (expTy, exp) = synthTypeOfExp (ctx, env, exp)
            val matches = checkTypeOfMatch
              (ctx, env, matches, expTy, expectedTy)
          in
            T.CaseExp
              { sourceSpan = span
              , subjectExp = exp
              , subjectTy = expTy
              , matches = matches
              , matchType = T.CASE
              , resultTy = expectedTy
              }
          end
      | checkTypeOfExp (ctx, env, exp as S.FnExp (span, matches), expectedTy) =
          (case forceTy expectedTy of
             T.FnType (_, argTy, retTy) =>
               let
                 val matches = checkTypeOfMatch
                   (ctx, env, matches, argTy, retTy)
               in
                 case matches of
                   [(T.VarPat (_, vid, _), body)] =>
                     T.FnExp (span, vid, argTy, body)
                 | _ =>
                     let
                       val paramName =
                         List.foldr
                           (fn ((p, _), acc) =>
                              Syntax.SourceName.merge (nameFromPat p, acc))
                           (Syntax.SourceName.fromString "a") matches
                       val vid = newVIdWithName (#context ctx, paramName)
                     in
                       T.FnExp (span, vid, argTy, T.CaseExp
                         { sourceSpan = span
                         , subjectExp =
                             T.VarExp
                               ( span
                               , T.MkShortVId vid
                               , Syntax.ValueVariable
                               , []
                               )
                         , subjectTy = argTy
                         , matches = matches
                         , matchType = T.CASE
                         , resultTy = retTy
                         })
                     end
               end
           | expectedTy =>
               let
                 val (actualTy, exp) = synthTypeOfExp (ctx, env, exp)
                 val () = checkSubsumption
                   (ctx, env, span, actualTy, expectedTy)
               in
                 exp
               end)
      | checkTypeOfExp
          (ctx, env, exp as S.ProjectionExp (span, label), expectedTy) =
          (case forceTy expectedTy of
             T.FnType (_, recordTy, fieldTy) =>
               (case forceTy recordTy of
                  T.RecordType (_, fieldTypes) =>
                    (case Syntax.LabelMap.find (fieldTypes, label) of
                       SOME fieldTy' =>
                         ( checkSubsumption (ctx, env, span, fieldTy', fieldTy)
                         ; T.ProjectionExp
                             { sourceSpan = span
                             , label = label
                             , recordTy = recordTy
                             , fieldTy = fieldTy
                             }
                         )
                     | NONE =>
                         ( emitTypeError (ctx, [span], "invalid projection")
                         ; T.BogusExp (span, expectedTy)
                         ))
                | _ =>
                    let
                      val (actualTy, exp) = synthTypeOfExp (ctx, env, exp)
                      val () = checkSubsumption
                        (ctx, env, span, actualTy, expectedTy)
                    in
                      exp
                    end)
           | _ =>
               let
                 val (actualTy, exp) = synthTypeOfExp (ctx, env, exp)
                 val () = checkSubsumption
                   (ctx, env, span, actualTy, expectedTy)
               in
                 exp
               end)
      | checkTypeOfExp (ctx, env, exp as S.ListExp (span, xs), expectedTy) =
          (case forceTy expectedTy of
             T.TyCon (_, [elemTy], con) =>
               if T.eqTyName (con, PrimTypes.Names.list) then
                 let
                   val xs =
                     Vector.map
                       (fn exp => checkTypeOfExp (ctx, env, exp, elemTy)) xs
                 in
                   T.ListExp (span, xs, elemTy)
                 end
               else
                 let
                   val (actualTy, exp) = synthTypeOfExp (ctx, env, exp)
                   val () = checkSubsumption
                     (ctx, env, span, actualTy, expectedTy)
                 in
                   exp
                 end
           | _ =>
               let
                 val (actualTy, exp) = synthTypeOfExp (ctx, env, exp)
                 val () = checkSubsumption
                   (ctx, env, span, actualTy, expectedTy)
               in
                 exp
               end)
      | checkTypeOfExp (ctx, env, exp as S.VectorExp (span, xs), expectedTy) =
          (case forceTy expectedTy of
             T.TyCon (_, [elemTy], con) =>
               if T.eqTyName (con, PrimTypes.Names.vector) then
                 let
                   val xs =
                     Vector.map
                       (fn exp => checkTypeOfExp (ctx, env, exp, elemTy)) xs
                 in
                   T.VectorExp (span, xs, elemTy)
                 end
               else
                 let
                   val (actualTy, exp) = synthTypeOfExp (ctx, env, exp)
                   val () = checkSubsumption
                     (ctx, env, span, actualTy, expectedTy)
                 in
                   exp
                 end
           | _ =>
               let
                 val (actualTy, exp) = synthTypeOfExp (ctx, env, exp)
                 val () = checkSubsumption
                   (ctx, env, span, actualTy, expectedTy)
               in
                 exp
               end)
      | checkTypeOfExp (ctx, env, S.SequentialExp (span, xs, y), expectedTy) =
          let
            val decs =
              Vector.foldr
                (fn (x, decs) =>
                   let
                     val span = S.getSourceSpanOfExp x
                     val (ty, x) = synthTypeOfExp (ctx, env, x)
                   in
                     if
                       #sequenceNonUnit (#languageOptions (#context ctx))
                       = LanguageOptions.IGNORE
                     then
                       T.IgnoreDec (span, x, ty) :: decs
                     else
                       T.IgnoreDec (span, x, ty)
                       ::
                       T.ValDescDec
                         { sourceSpan = span
                         , expected = T.TypeScheme ([], PrimTypes.unit)
                         , actual = T.TypeScheme ([], ty)
                         , origin = T.VALDESC_SEQUENCE
                         } :: decs
                   end) [] xs
            val y = checkTypeOfExp (ctx, env, y, expectedTy)
          in
            T.LetInExp (span, decs, y)
          end
      | checkTypeOfExp (ctx, env, exp, expectedTy) =
          let
            val span = S.getSourceSpanOfExp exp
            val (actualTy, exp) = synthTypeOfExp (ctx, env, exp)
            val () = checkSubsumption (ctx, env, span, actualTy, expectedTy)
          in
            exp
          end
    and typeCheckDec
          ( ctx: InferenceContext
          , env: Env
          , S.ValDec (span, tyvarseq, descs, valbinds)
          ) =
          let
            val ctx' = enterLevel ctx
            val valbinds =
              let
                val env =
                  { valMap = #valMap env
                  , tyConMap = #tyConMap env
                  , tyNameMap = #tyNameMap env
                  , strMap = #strMap env
                  , sigMap = #sigMap env
                  , funMap = #funMap env
                  , boundTyVars =
                      List.foldl
                        (fn (tv, m) =>
                           Syntax.TyVarMap.insert
                             (m, tv, genTyVar (#context ctx', tv)))
                        (#boundTyVars env) tyvarseq
                  }
              in
                List.map
                  (fn S.PatBind (span, pat, exp) =>
                     let
                       val (expTy, exp) =
                         case pat of
                           S.TypedPat (_, _, ty) =>
                             let val ty = evalTy (#context ctx', env, ty)
                             in (ty, checkTypeOfExp (ctx', env, exp, ty))
                             end
                         | _ => synthTypeOfExp (ctx', env, exp)
                       val (newValEnv, pat) = checkTypeOfPat
                         (ctx', env, pat, expTy, Syntax.VIdMap.empty)
                       val generalizable =
                         isExhaustive (ctx', env, pat)
                         andalso isNonexpansive (env, exp)
                     in
                       { sourceSpan = span
                       , pat = pat
                       , exp = exp
                       , expTy = expTy
                       , valEnv = newValEnv
                       , generalizable = generalizable
                       }
                     end) valbinds
              end
            val tyVars_env = freeTyVarsInEnv (T.TyVarSet.empty, env)
            fun generalize
                  ( { sourceSpan = span
                    , pat
                    , exp
                    , expTy
                    , valEnv: (T.VId * T.Ty) S.VIdMap.map
                    , generalizable = false
                    }
                  , (valbinds, valEnvRest: (T.VId * T.TypeScheme) S.VIdMap.map)
                  ) =
                  let
                    val vars = Syntax.VIdMap.listItems valEnv
                  in
                    List.app
                      (fn (_, ty) =>
                         let
                           val fv = T.freeAnonymousTyVarsInTy ty
                         in
                           List.app
                             (fn tv as ref (T.Unbound (ct, level)) =>
                                if level > #level ctx then
                                  tv := T.Unbound (ct, #level ctx)
                                else
                                  ()
                               | _ => ()) fv
                         end) vars;
                    case vars of
                      [(vid, ty)] =>
                        let
                          val valbind' = T.PolyVarBind
                            ( span
                            , vid
                            , T.TypeScheme ([], ty)
                            , case pat of
                                T.VarPat _ => exp
                              | T.TypedPat (_, T.VarPat _, _) => exp
                              | _ =>
                                  let
                                    val espan = T.getSourceSpanOfExp exp
                                    val vid' = renewVId (#context ctx) vid
                                    val pat' =
                                      T.renameVarsInPat
                                        (T.VIdMap.insert
                                           (T.VIdMap.empty, vid, vid')) pat
                                  in
                                    T.CaseExp
                                      { sourceSpan = span
                                      , subjectExp = exp
                                      , subjectTy = expTy
                                      , matches =
                                          [( pat'
                                           , T.VarExp
                                               ( espan
                                               , T.MkShortVId vid'
                                               , Syntax.ValueVariable
                                               , []
                                               )
                                           )]
                                      , matchType = T.VAL
                                      , resultTy = ty
                                      }
                                  end
                            )
                        in
                          ( valbind' :: valbinds
                          , S.VIdMap.unionWith
                              (fn (_, y) =>
                                 ( emitTypeError
                                     ( ctx
                                     , [span]
                                     , "duplicate identifier in a binding"
                                     )
                                 ; y
                                 ))
                              ( S.VIdMap.map
                                  (fn (vid, ty) => (vid, T.TypeScheme ([], ty)))
                                  valEnv
                              , valEnvRest
                              )
                          )
                        end
                    | _ =>
                        let
                          val espan = T.getSourceSpanOfExp exp
                          val vars' =
                            List.map
                              (fn (vid, _) => (vid, renewVId (#context ctx) vid))
                              vars
                          val varsMap =
                            List.foldl T.VIdMap.insert' T.VIdMap.empty vars'
                          val pat' = T.renameVarsInPat varsMap pat
                          val tup = T.TupleExp
                            ( espan
                            , List.map
                                (fn (_, vid') =>
                                   T.VarExp
                                     ( espan
                                     , T.MkShortVId vid'
                                     , Syntax.ValueVariable
                                     , []
                                     )) vars'
                            )
                          val tupleTy = T.TupleType (espan, List.map #2 vars)
                          val valbind' = T.TupleBind (span, vars, T.CaseExp
                            { sourceSpan = span
                            , subjectExp = exp
                            , subjectTy = expTy
                            , matches = [(pat', tup)]
                            , matchType = T.VAL
                            , resultTy = tupleTy
                            })
                        in
                          ( valbind' :: valbinds
                          , S.VIdMap.unionWith
                              (fn (_, y) =>
                                 ( emitTypeError
                                     ( ctx
                                     , [span]
                                     , "duplicate identifier in a binding"
                                     )
                                 ; y
                                 ))
                              ( S.VIdMap.map
                                  (fn (vid, ty) => (vid, T.TypeScheme ([], ty)))
                                  valEnv
                              , valEnvRest
                              )
                          )
                        end
                  end
              | generalize
                  ( { sourceSpan = span
                    , pat
                    , exp
                    , expTy
                    , valEnv
                    , generalizable = true
                    }
                  , (valbinds, valEnvRest)
                  ) =
                  let
                    fun doVal (vid, ty) =
                      let
                        val ty = T.forceTy ty
                        val tyVars = T.freeTyVarsInTy (tyVars_env, ty)
                        val aTyVars_ty = T.freeAnonymousTyVarsInTy ty
                        val aTyVars =
                          List.foldl
                            (fn (tv, vars) =>
                               case !tv of
                                 T.Unbound
                                   ( ct as
                                       {sourceSpan = _, equalityRequired, class}
                                   , level
                                   ) =>
                                   if
                                     level > #level ctx
                                     andalso not (Option.isSome class)
                                   then
                                     if not equalityRequired then
                                       let
                                         val tv' =
                                           genTyVar
                                             (#context ctx, Syntax.MkTyVar "'?")
                                       in
                                         tv := T.Link (T.TyVar (span, tv'));
                                         (tv', NONE) :: vars
                                       end
                                     else
                                       let
                                         val tv' =
                                           genTyVar
                                             ( #context ctx
                                             , Syntax.MkTyVar "''?"
                                             )
                                       in
                                         tv := T.Link (T.TyVar (span, tv'));
                                         (tv', SOME T.IsEqType) :: vars
                                       end
                                   else if
                                     level > #level ctx
                                   then
                                     (tv := T.Unbound (ct, #level ctx); vars)
                                   else
                                     vars
                               | T.Link _ => vars) [] aTyVars_ty
                        fun doTyVar tv =
                          if T.tyVarAdmitsEquality tv then (tv, SOME T.IsEqType)
                          else (tv, NONE)
                        val tysc = T.TypeScheme
                          ( List.map doTyVar (T.TyVarSet.listItems tyVars)
                            @ aTyVars
                          , ty
                          )
                      in
                        (vid, tysc)
                      end
                    val valEnv' = Syntax.VIdMap.map doVal valEnv
                    val valEnv'L = Syntax.VIdMap.listItems valEnv'
                    val allPoly =
                      List.all
                        (fn (_, T.TypeScheme (tv, _)) => not (List.null tv))
                        valEnv'L (* all bindings are generalized? *)
                    val espan = TypedSyntax.getSourceSpanOfExp exp
                    fun polyPart [] = []
                      | polyPart ((_, T.TypeScheme ([], _)) :: rest) =
                          polyPart rest
                      | polyPart ((vid, tysc as T.TypeScheme (_, ty)) :: rest) =
                          let
                            val vid' = renewVId (#context ctx) vid
                            val pat' =
                              TypedSyntax.renameVarsInPat
                                (TypedSyntax.VIdMap.insert
                                   (TypedSyntax.VIdMap.empty, vid, vid')) pat
                          in
                            T.PolyVarBind (span, vid, tysc, TypedSyntax.CaseExp
                              { sourceSpan = span
                              , subjectExp = exp
                              , subjectTy = expTy
                              , matches =
                                  [( TypedSyntax.filterVarsInPat
                                       (fn x => TypedSyntax.eqVId (x, vid'))
                                       pat'
                                   , TypedSyntax.VarExp
                                       ( espan
                                       , TypedSyntax.MkShortVId vid'
                                       , Syntax.ValueVariable
                                       , []
                                       )
                                   )]
                              , matchType = T.VAL
                              , resultTy = ty
                              }) :: polyPart rest
                          end
                    fun isMonoVar vid =
                      List.exists
                        (fn (vid', T.TypeScheme (tvs, _)) =>
                           T.eqVId (vid, vid') andalso List.null tvs) valEnv'L
                    val valbind' =
                      if allPoly then
                        if List.null valEnv'L then
                          let
                            val vid = freshVId (#context ctx, "dummy")
                            val (_, tysc) = doVal (vid, expTy)
                          in
                            [T.PolyVarBind (span, vid, tysc, exp)] (* Make sure the constants in exp are range-checked *)
                          end
                        else
                          polyPart valEnv'L
                      else
                        let
                          val xs =
                            List.mapPartial
                              (fn (vid, tysc) =>
                                 case tysc of
                                   T.TypeScheme ([], ty) => SOME (vid, ty)
                                 | T.TypeScheme (_ :: _, _) => NONE) valEnv'L
                        in
                          case xs of
                            [(vid, ty)] =>
                              let
                                val vid' = renewVId (#context ctx) vid
                                val pat' =
                                  TypedSyntax.renameVarsInPat
                                    (TypedSyntax.VIdMap.insert
                                       (TypedSyntax.VIdMap.empty, vid, vid'))
                                    (TypedSyntax.filterVarsInPat isMonoVar pat)
                              in
                                T.PolyVarBind
                                  ( span
                                  , vid
                                  , T.TypeScheme ([], ty)
                                  , T.CaseExp
                                      { sourceSpan = span
                                      , subjectExp = exp
                                      , subjectTy = expTy
                                      , matches =
                                          [( pat'
                                           , T.VarExp
                                               ( espan
                                               , TypedSyntax.MkShortVId vid'
                                               , Syntax.ValueVariable
                                               , []
                                               )
                                           )]
                                      , matchType = T.VAL
                                      , resultTy = ty
                                      }
                                  ) :: polyPart valEnv'L
                              end
                          | _ =>
                              let
                                val vars' =
                                  List.map
                                    (fn (vid, _) =>
                                       (vid, renewVId (#context ctx) vid)) xs
                                val varsMap =
                                  List.foldl TypedSyntax.VIdMap.insert'
                                    TypedSyntax.VIdMap.empty vars'
                                val pat' = TypedSyntax.renameVarsInPat varsMap
                                  (TypedSyntax.filterVarsInPat isMonoVar pat)
                                val tup = T.TupleExp
                                  ( espan
                                  , List.map
                                      (fn (_, vid') =>
                                         T.VarExp
                                           ( espan
                                           , TypedSyntax.MkShortVId vid'
                                           , Syntax.ValueVariable
                                           , []
                                           )) vars'
                                  )
                                val tupleTy = T.TupleType
                                  (espan, List.map #2 xs)
                              in
                                T.TupleBind (span, xs, T.CaseExp
                                  { sourceSpan = span
                                  , subjectExp = exp
                                  , subjectTy = expTy
                                  , matches = [(pat', tup)]
                                  , matchType = T.VAL
                                  , resultTy = tupleTy
                                  }) :: polyPart valEnv'L
                              end
                        end
                  in
                    ( valbind' @ valbinds
                    , Syntax.VIdMap.unionWith
                        (fn (_, y) =>
                           ( emitTypeError
                               ( ctx
                               , [span]
                               , "duplicate identifier in a binding"
                               )
                           ; y
                           )) (valEnv', valEnvRest)
                    )
                  end
            val (valbinds, valEnv) =
              List.foldr generalize ([], Syntax.VIdMap.empty) valbinds
            val env' = envWithValEnv
              (Syntax.VIdMap.map
                 (fn (vid, tysc) =>
                    (tysc, Syntax.ValueVariable, T.MkShortVId vid)) valEnv)
            val descs =
              List.mapPartial
                (fn (span, vid, tyvarseq', ty) =>
                   case S.VIdMap.find (valEnv, vid) of
                     SOME (_, tysc) =>
                       let
                         val tyvarseq = tyvarseq @ tyvarseq'
                         val boundTyVars =
                           List.foldl
                             (fn (tv, m) =>
                                Syntax.TyVarMap.insert
                                  (m, tv, genTyVar (#context ctx', tv)))
                             (#boundTyVars env) tyvarseq
                         val env =
                           { valMap = #valMap env
                           , tyConMap = #tyConMap env
                           , tyNameMap = #tyNameMap env
                           , strMap = #strMap env
                           , sigMap = #sigMap env
                           , funMap = #funMap env
                           , boundTyVars = boundTyVars
                           }
                         val ty = evalTy (#context ctx, env, ty)
                         fun doTyVar tv =
                           case S.TyVarMap.find (boundTyVars, tv) of
                             SOME tv =>
                               if T.tyVarAdmitsEquality tv then
                                 (tv, SOME T.IsEqType)
                               else
                                 (tv, NONE)
                           | NONE =>
                               emitFatalTypeError
                                 (ctx, [span], "undefined type variable")
                       in
                         SOME (T.ValDescDec
                           { sourceSpan = span
                           , expected = T.TypeScheme
                               (List.map doTyVar tyvarseq, ty)
                           , actual = tysc
                           , origin = T.VALDESC_COMMENT
                           })
                       end
                   | NONE =>
                       ( emitTypeError
                           ( ctx
                           , [span]
                           , "type description for undefined variable"
                           )
                       ; NONE
                       )) descs
          in
            (env', T.ValDec (span, valbinds) :: descs)
          end
      | typeCheckDec (ctx, env, S.RecValDec (span, tyvarseq, descs, valbinds)) =
          let
            val ctx' = enterLevel ctx
            val
              valbinds':
                (SourcePos.span
                 * (T.Ty * (T.VId * T.Ty) S.VIdMap.map * T.Pat)
                 * S.Exp) list =
              List.map
                (fn S.PatBind (span, pat, exp) =>
                   ( span
                   , synthTypeOfPat (ctx', env, pat, Syntax.VIdMap.empty)
                   , exp
                   )) valbinds
            val localValEnv =
              List.foldl
                (fn ((_, (_, ve, _), _), acc) =>
                   Syntax.VIdMap.unionWith #1 (acc, ve)) Syntax.VIdMap.empty
                valbinds'
            val localValMap =
              Syntax.VIdMap.map
                (fn (vid', ty) =>
                   ( T.TypeScheme ([], ty)
                   , Syntax.ValueVariable
                   , T.MkShortVId vid'
                   )) localValEnv
            val tyvarseq' =
              List.map (fn tv => (tv, genTyVar (#context ctx, tv))) tyvarseq
            val localEnv =
              let
                val
                  { valMap
                  , tyConMap
                  , tyNameMap
                  , strMap
                  , sigMap
                  , funMap
                  , boundTyVars
                  } = env
              in
                { valMap = Syntax.VIdMap.unionWithSecond (valMap, localValMap)
                , tyConMap = tyConMap
                , tyNameMap = tyNameMap
                , strMap = strMap
                , sigMap = sigMap
                , funMap = funMap
                , boundTyVars =
                    List.foldl Syntax.TyVarMap.insert' boundTyVars tyvarseq'
                }
              end
            val valbinds'' =
              List.map
                (fn (span, (ty, newValEnv, pat), exp) =>
                   let
                     val exp = checkTypeOfExp (ctx', localEnv, exp, ty)
                     val generalizable =
                       isExhaustive (ctx', env, pat)
                       andalso isNonexpansive (env, exp)
                   in
                     if generalizable then
                       ()
                     else
                       emitTypeError
                         (ctx', [span], "'val rec' must be generalizable");
                     { sourceSpan = span
                     , pat = pat
                     , exp = exp
                     , expTy = ty
                     , valEnv = newValEnv
                     }
                   end) valbinds'
            val tyVars_env = freeTyVarsInEnv (T.TyVarSet.empty, env)
            fun generalize
              ( {sourceSpan = span, pat = _, exp, expTy, valEnv}
              , (valbinds, valEnvRest, tyVarsAcc)
              ) =
              let
                fun doVal (vid, ty) =
                  let
                    val ty = T.forceTy ty
                    val tyVars = T.freeTyVarsInTy (tyVars_env, ty)
                    val aTyVars_ty = T.freeAnonymousTyVarsInTy ty
                    val aTyVars =
                      List.foldl
                        (fn (tv, vars) =>
                           case !tv of
                             T.Unbound
                               ( ct as {sourceSpan = _, equalityRequired, class}
                               , level
                               ) =>
                               if
                                 level > #level ctx
                                 andalso not (Option.isSome class)
                               then
                                 if not equalityRequired then
                                   let
                                     val tv' =
                                       genTyVar
                                         (#context ctx, Syntax.MkTyVar "'?")
                                   in
                                     tv := T.Link (T.TyVar (span, tv'));
                                     (tv', NONE) :: vars
                                   end
                                 else
                                   let
                                     val tv' =
                                       genTyVar
                                         (#context ctx, Syntax.MkTyVar "''?")
                                   in
                                     tv := T.Link (T.TyVar (span, tv'));
                                     (tv', SOME T.IsEqType) :: vars
                                   end
                               else if
                                 level > #level ctx
                               then
                                 (tv := T.Unbound (ct, #level ctx); vars)
                               else
                                 vars
                           | T.Link _ => vars) [] aTyVars_ty
                    fun doTyVar tv =
                      if T.tyVarAdmitsEquality tv then (tv, SOME T.IsEqType)
                      else (tv, NONE)
                    val tysc = T.TypeScheme
                      ( List.map doTyVar (T.TyVarSet.listItems tyVars) @ aTyVars
                      , ty
                      )
                  in
                    (vid, tysc, aTyVars)
                  end
                val (valEnv', valEnv'L) =
                  if Syntax.VIdMap.isEmpty valEnv then
                    let
                      val vid = freshVId (#context ctx, "dummy")
                      val (vid, tysc, aTyVars) = doVal (vid, expTy)
                    in
                      ( Syntax.VIdMap.empty
                      , [(vid, tysc, aTyVars)]
                      ) (* Make sure the constants in exp are range-checked *)
                    end
                  else
                    let val valEnv' = Syntax.VIdMap.map doVal valEnv
                    in (valEnv', Syntax.VIdMap.listItems valEnv')
                    end
                val aTyVars =
                  List.foldl
                    (fn ((_, _, aTyVars), acc) =>
                       List.foldl
                         (fn ((tv, _), acc) => T.TyVarSet.add (acc, tv)) acc
                         aTyVars) T.TyVarSet.empty valEnv'L
                val valbinds =
                  List.foldr
                    (fn ((vid, tysc, _), rest) => (span, vid, tysc, exp) :: rest)
                    valbinds valEnv'L
              in
                ( valbinds
                , Syntax.VIdMap.unionWith #2 (valEnv', valEnvRest)
                , T.TyVarSet.union (aTyVars, tyVarsAcc)
                )
              end
            val (valbinds, valEnv, allTyVars) =
              List.foldr generalize
                ( []
                , Syntax.VIdMap.empty
                , T.TyVarSet.fromList (List.map #2 tyvarseq')
                ) valbinds''
            fun fixRecursion (span, vid, tysc as T.TypeScheme (tyvars, _), exp) =
              let
                val unboundTyVars =
                  T.TyVarSet.foldl
                    (fn (tv, acc) =>
                       if List.exists (fn (tv', _) => tv = tv') tyvars then
                         acc
                       else
                         T.TyVarMap.insert (acc, tv, T.AnonymousTyVar
                           ( span
                           , freshTyVar
                               (ctx, span, T.tyVarAdmitsEquality tv, NONE)
                           ))) T.TyVarMap.empty allTyVars
                val exp = #doExp (T.applySubstTyInExpOrDec unboundTyVars) exp
                val subst =
                  List.foldl
                    (fn ((_, vid', T.TypeScheme (tyvars', _), _), subst) =>
                       T.VIdMap.insert
                         ( subst
                         , vid'
                         , fn (span, idstatus as Syntax.ValueVariable, []) =>
                             let
                               val tyargs' =
                                 List.map
                                   (fn (tv, c) =>
                                      case T.TyVarMap.find (unboundTyVars, tv) of
                                        NONE => (T.TyVar (span, tv), c)
                                      | SOME a => (a, c)) tyvars'
                             in
                               T.VarExp
                                 (span, T.MkShortVId vid', idstatus, tyargs')
                             end
                            | (_, _, _) =>
                             emitFatalTypeError
                               ( ctx
                               , [span]
                               , "invalid use of recursive identifier"
                               )
                         )) T.VIdMap.empty valbinds
                val exp = #doExp (T.substVId subst) exp
              in
                T.PolyVarBind (span, vid, tysc, exp)
              end
            val valbinds' = List.map fixRecursion valbinds
            val env' = envWithValEnv
              (Syntax.VIdMap.map
                 (fn (vid, tysc, _) =>
                    (tysc, Syntax.ValueVariable, TypedSyntax.MkShortVId vid))
                 valEnv)
            val descs =
              List.mapPartial
                (fn (span, vid, tyvarseq', ty) =>
                   case S.VIdMap.find (valEnv, vid) of
                     SOME (_, tysc, _) =>
                       let
                         val tyvarseq = tyvarseq @ tyvarseq'
                         val boundTyVars =
                           List.foldl
                             (fn (tv, m) =>
                                Syntax.TyVarMap.insert
                                  (m, tv, genTyVar (#context ctx', tv)))
                             (#boundTyVars env) tyvarseq
                         val env =
                           { valMap = #valMap env
                           , tyConMap = #tyConMap env
                           , tyNameMap = #tyNameMap env
                           , strMap = #strMap env
                           , sigMap = #sigMap env
                           , funMap = #funMap env
                           , boundTyVars = boundTyVars
                           }
                         val ty = evalTy (#context ctx, env, ty)
                         fun doTyVar tv =
                           case S.TyVarMap.find (boundTyVars, tv) of
                             SOME tv =>
                               if T.tyVarAdmitsEquality tv then
                                 (tv, SOME T.IsEqType)
                               else
                                 (tv, NONE)
                           | NONE =>
                               emitFatalTypeError
                                 (ctx, [span], "undefined type variable")
                       in
                         SOME (T.ValDescDec
                           { sourceSpan = span
                           , expected = T.TypeScheme
                               (List.map doTyVar tyvarseq, ty)
                           , actual = tysc
                           , origin = T.VALDESC_COMMENT
                           })
                       end
                   | NONE =>
                       ( emitTypeError
                           ( ctx
                           , [span]
                           , "type description for undefined variable"
                           )
                       ; NONE
                       )) descs
          in
            (env', T.RecValDec (span, valbinds') :: descs)
          end
      | typeCheckDec (ctx, env, S.TypeDec (span, typbinds)) =
          let
            fun doTypBind
              (S.TypBind (span, tyvars, tycon, ty), (tyConEnv, typbinds)) =
              let
                val tyvars =
                  List.map (fn tv => (tv, genTyVar (#context ctx, tv))) tyvars
                val env' =
                  { valMap = #valMap env
                  , tyConMap = #tyConMap env
                  , tyNameMap = #tyNameMap env
                  , strMap = #strMap env
                  , sigMap = #sigMap env
                  , funMap = #funMap env
                  , boundTyVars =
                      List.foldl Syntax.TyVarMap.insert' (#boundTyVars env)
                        tyvars
                  }
                val ty = evalPureTy (#context ctx, env', ty)
                val tystr: T.TypeStructure =
                  { typeFunction = T.TypeFunction (List.map #2 tyvars, ty)
                  , valEnv = T.emptyValEnv
                  }
              in
                ( Syntax.TyConMap.insert
                    (tyConEnv, tycon, tystr) (* TODO: error on duplicate *)
                , T.TypBind (span, List.map #2 tyvars, tycon, ty) :: typbinds
                )
              end
            val (tyConEnv, typbinds) =
              List.foldr doTypBind (Syntax.TyConMap.empty, []) typbinds
          in
            ( envWithTyConEnv (tyConEnv, TypedSyntax.TyNameMap.empty)
            , [T.TypeDec (span, typbinds)]
            )
          end
      | typeCheckDec (ctx, env, S.DatatypeDec (span, datbinds, typbinds)) =
          let
            val datbinds =
              let
                val goConBind = doWithtype (#context ctx, env, typbinds)
              in
                List.map
                  (fn S.DatBind (span, tyvars, tycon, optBar, conbinds) =>
                     S.DatBind
                       ( span
                       , tyvars
                       , tycon
                       , optBar
                       , List.map goConBind conbinds
                       )) datbinds
              end
            val equalityMap: bool S.TyConMap.map = determineDatatypeEquality
              ( #context ctx
              , env
              , List.foldl
                  (fn (S.DatBind (_, tyvars, tycon, _, conbinds), m) =>
                     S.TyConMap.insert
                       ( m
                       , tycon
                       , ( tyvars
                         , List.mapPartial (fn S.ConBind (_, _, optTy) => optTy)
                             conbinds
                         )
                       )) S.TyConMap.empty datbinds
              )
            val datbinds =
              List.map
                (fn datbind as S.DatBind (_, _, tycon, _, _) =>
                   (datbind, newTyName (#context ctx, tycon))) datbinds
            val partialEnv =
              List.foldl
                (fn ( (S.DatBind (span, tyvars, tycon, _, _), tycon')
                    , {tyConMap, tyNameMap}
                    ) =>
                   let
                     val tyvars =
                       List.map (fn tv => genTyVar (#context ctx, tv)) tyvars
                     val tystr =
                       { typeFunction = T.TypeFunction (tyvars, T.TyCon
                           ( span
                           , List.map (fn tv => T.TyVar (span, tv)) tyvars
                           , tycon'
                           ))
                       , valEnv = T.emptyValEnv
                       }
                     val tynameattr =
                       { arity = List.length tyvars
                       , admitsEquality = S.TyConMap.lookup (equalityMap, tycon)
                       , overloadClass = NONE
                       }
                   in
                     { tyConMap =
                         Syntax.TyConMap.insert (tyConMap, tycon, tystr)
                     , tyNameMap =
                         TypedSyntax.TyNameMap.insert
                           (tyNameMap, tycon', tynameattr)
                     }
                   end)
                { tyConMap = Syntax.TyConMap.empty
                , tyNameMap = TypedSyntax.TyNameMap.empty
                } datbinds
            val conEnv = mergeWithTyConEnv (env, partialEnv)
            val (tyConMap, tyNameMap, valMap, datbinds) =
              let
                fun doDatBind
                  ( (S.DatBind (span, tyvars, tycon, _, conbinds), tyname)
                  , (tyConMap, tyNameMap, accValEnv, datbinds)
                  ) =
                  let
                    val tyvars =
                      List.map (fn tv => (tv, genTyVar (#context ctx, tv)))
                        tyvars
                    val conEnv =
                      { valMap = #valMap conEnv
                      , tyConMap = #tyConMap conEnv
                      , tyNameMap = #tyNameMap conEnv
                      , strMap = #strMap conEnv
                      , sigMap = #sigMap conEnv
                      , funMap = #funMap conEnv
                      , boundTyVars =
                          List.foldl Syntax.TyVarMap.insert'
                            (#boundTyVars conEnv) tyvars
                      }
                    val tyvars = List.map #2 tyvars
                    val allConstructors =
                      List.foldl
                        (fn (Syntax.ConBind (_, vid, _), set) =>
                           Syntax.VIdSet.add (set, vid)) Syntax.VIdSet.empty
                        conbinds
                    val constructorsWithPayload =
                      List.foldl
                        (fn (Syntax.ConBind (_, vid, optTy), set) =>
                           if Option.isSome optTy then
                             Syntax.VIdSet.add (set, vid)
                           else
                             set) Syntax.VIdSet.empty conbinds
                    val representation =
                      case conbinds of
                        [S.ConBind (_, _, SOME _)] => Syntax.REP_ALIAS
                      | [S.ConBind (_, _, NONE)] => Syntax.REP_UNIT
                      | _ =>
                          if Syntax.VIdSet.isEmpty constructorsWithPayload then
                            Syntax.REP_ENUM
                          else
                            Syntax.REP_BOXED
                    val (valEnv, conbinds) =
                      List.foldr
                        (fn (S.ConBind (span, vid, optTy), (valEnv, conbinds)) =>
                           let
                             val vid' = newVId (#context ctx, vid)
                             val optTy =
                               Option.map
                                 (fn ty => evalPureTy (#context ctx, conEnv, ty))
                                 optTy
                             val info =
                               { tag = Syntax.getVIdName vid
                               , allConstructors = allConstructors
                               , constructorsWithPayload =
                                   constructorsWithPayload
                               , representation = representation
                               }
                             val idstatus = Syntax.ValueConstructor info
                             val conbind = T.ConBind (span, vid', optTy, info)
                             val tysc = T.TypeScheme
                               ( List.map (fn tv => (tv, NONE)) tyvars
                               , case optTy of
                                   NONE =>
                                     T.TyCon
                                       ( span
                                       , List.map (fn tv => T.TyVar (span, tv))
                                           tyvars
                                       , tyname
                                       )
                                 | SOME payloadTy =>
                                     T.FnType (span, payloadTy, T.TyCon
                                       ( span
                                       , List.map (fn tv => T.TyVar (span, tv))
                                           tyvars
                                       , tyname
                                       ))
                               )
                           in
                             ( Syntax.VIdMap.insert
                                 ( valEnv
                                 , vid
                                 , ( T.thawPureTypeScheme tysc
                                   , idstatus
                                   , T.MkShortVId vid'
                                   )
                                 ) (* TODO: check for duplicate *)
                             , conbind :: conbinds
                             )
                           end) (Syntax.VIdMap.empty, []) conbinds
                    val datbind = T.DatBind
                      ( span
                      , tyvars
                      , tyname
                      , conbinds
                      , S.TyConMap.lookup (equalityMap, tycon)
                      )
                    val tystr =
                      { typeFunction = T.TypeFunction (tyvars, T.TyCon
                          ( span
                          , List.map (fn tv => T.TyVar (span, tv)) tyvars
                          , tyname
                          ))
                      , valEnv =
                          Syntax.VIdMap.map (fn (tysc, ids, _) => (tysc, ids))
                            valEnv
                      }
                    val tynameattr =
                      { arity = List.length tyvars
                      , admitsEquality =
                          Syntax.TyConMap.lookup (equalityMap, tycon)
                      , overloadClass = NONE
                      }
                  in
                    ( Syntax.TyConMap.insert (tyConMap, tycon, tystr)
                    , TypedSyntax.TyNameMap.insert
                        (tyNameMap, tyname, tynameattr)
                    , Syntax.VIdMap.unionWithSecond
                        (accValEnv, valEnv) (* TODO: check for duplicate *)
                    , datbind :: datbinds
                    )
                  end
              in
                List.foldr doDatBind
                  ( Syntax.TyConMap.empty
                  , TypedSyntax.TyNameMap.empty
                  , Syntax.VIdMap.empty
                  , []
                  ) datbinds
              end
            val valMap =
              Syntax.VIdMap.map
                (fn (tysc, idstatus, longvid) =>
                   (T.thawPureTypeScheme tysc, idstatus, longvid)) valMap
            val (tyConMap, typbinds) =
              let
                fun doTypBind
                  (S.TypBind (span, tyvars, tycon, ty), (tyConEnv, typbinds)) =
                  let
                    val tyvars =
                      List.map (fn tv => (tv, genTyVar (#context ctx, tv)))
                        tyvars
                    val env' =
                      { valMap =
                          Syntax.VIdMap.unionWithSecond (#valMap env, valMap)
                      , tyConMap =
                          Syntax.TyConMap.unionWithSecond
                            (#tyConMap env, tyConMap)
                      , tyNameMap =
                          TypedSyntax.TyNameMap.unionWithSecond
                            (#tyNameMap env, tyNameMap)
                      , strMap = #strMap env
                      , sigMap = #sigMap env
                      , funMap = #funMap env
                      , boundTyVars =
                          List.foldl Syntax.TyVarMap.insert' (#boundTyVars env)
                            tyvars
                      }
                    val ty = evalPureTy (#context ctx, env', ty)
                    val tystr: T.TypeStructure =
                      { typeFunction = T.TypeFunction (List.map #2 tyvars, ty)
                      , valEnv = T.emptyValEnv
                      }
                  in
                    ( Syntax.TyConMap.insert
                        (tyConEnv, tycon, tystr) (* TODO: error on duplicate *)
                    , T.TypBind (span, List.map #2 tyvars, tycon, ty)
                      :: typbinds
                    )
                  end
              in
                List.foldr doTypBind (tyConMap, []) typbinds
              end
            val env' =
              { valMap = valMap
              , tyConMap = tyConMap
              , tyNameMap = tyNameMap
              , strMap = Syntax.StrIdMap.empty
              , sigMap = Syntax.SigIdMap.empty
              , funMap = Syntax.FunIdMap.empty
              , boundTyVars = Syntax.TyVarMap.empty
              }
          in
            ( env'
            , if List.null typbinds then [T.DatatypeDec (span, datbinds)]
              else [T.DatatypeDec (span, datbinds), T.TypeDec (span, typbinds)]
            )
          end
      | typeCheckDec (ctx, env, S.DatatypeRepDec (span, tycon, longtycon)) =
          let
            val tystr = lookupTyConInEnv (#context ctx, env, span, longtycon)
            val getLongVId =
              case longtycon of
                Syntax.MkQualified ([], _) =>
                  (fn vid =>
                     case Syntax.VIdMap.find (#valMap env, vid) of
                       SOME (_, _, longvid) => longvid
                     | NONE =>
                         emitFatalTypeError
                           ( ctx
                           , [span]
                           , "datatype replication: value identifier "
                             ^ Syntax.print_VId vid
                             ^ " not found (internal error)"
                           ))
              | Syntax.MkQualified (strid0 :: strids, _) =>
                  case Syntax.StrIdMap.find (#strMap env, strid0) of
                    SOME (_, T.MkLongStrId (strid0, strids0)) =>
                      (fn vid => T.MkLongVId (strid0, strids0 @ strids, vid))
                  | NONE =>
                      emitFatalTypeError
                        ( ctx
                        , [span]
                        , "datatype replication: structure "
                          ^ Syntax.print_StrId strid0
                          ^ " not found (internal error)"
                        )
            val env' =
              { valMap =
                  Syntax.VIdMap.mapi
                    (fn (vid, (tysc, ids)) =>
                       (T.thawPureTypeScheme tysc, ids, getLongVId vid))
                    (#valEnv tystr)
              , tyConMap = Syntax.TyConMap.singleton (tycon, tystr)
              , tyNameMap = TypedSyntax.TyNameMap.empty
              , strMap = Syntax.StrIdMap.empty
              , sigMap = Syntax.SigIdMap.empty
              , funMap = Syntax.FunIdMap.empty
              , boundTyVars = Syntax.TyVarMap.empty
              }
          in
            (env', [])
          end
      | typeCheckDec (ctx, env, S.AbstypeDec (span, datbinds, typbinds, decs)) =
          let
            val (env', datbinds') = typeCheckDec
              (ctx, env, S.DatatypeDec (span, datbinds, typbinds))
            val (env'', decs') = typeCheckDecs (ctx, mergeEnv (env, env'), decs)
            val resultingEnv =
              { valMap = #valMap env''
              , tyConMap = Syntax.TyConMap.unionWith #2
                  ( Syntax.TyConMap.map
                      (fn {typeFunction, valEnv = _} =>
                         {typeFunction = typeFunction, valEnv = T.emptyValEnv})
                      (#tyConMap env')
                  , #tyConMap env''
                  )
              , tyNameMap =
                  TypedSyntax.TyNameMap.unionWithSecond
                    (#tyNameMap env', #tyNameMap env'') (* should be disjoint *)
              , strMap =
                  Syntax.StrIdMap.unionWithSecond
                    (#strMap env', #strMap env'') (* should be empty *)
              , sigMap =
                  Syntax.SigIdMap.unionWithSecond
                    (#sigMap env', #sigMap env'') (* should be empty *)
              , funMap =
                  Syntax.FunIdMap.unionWithSecond (#funMap env', #funMap env'')
              , boundTyVars =
                  Syntax.TyVarMap.unionWithSecond
                    ( #boundTyVars env'
                    , #boundTyVars env''
                    ) (* should be empty *)
              }
          in
            (resultingEnv, datbinds' @ decs')
          end
      | typeCheckDec (ctx, env, S.ExceptionDec (span, exbinds)) =
          let
            fun doExBind (S.ExBind (span, vid, optTy), (valMap, exbinds)) =
                  let
                    val optTy =
                      Option.map (fn ty => evalPureTy (#context ctx, env, ty))
                        optTy
                    val vid' = newVId (#context ctx, vid)
                    val valMap = S.VIdMap.insert
                      ( valMap
                      , vid
                      , ( T.TypeScheme
                            ( []
                            , case optTy of
                                NONE => PrimTypes.exn
                              | SOME ty =>
                                  T.FnType
                                    (span, T.thawPureTy ty, PrimTypes.exn)
                            )
                        , Syntax.ExceptionConstructor
                        , T.MkShortVId vid'
                        )
                      )
                  in
                    (valMap, T.ExBind (span, vid', optTy) :: exbinds)
                  end
              | doExBind
                  (S.ExReplication (span, vid, longvid), (valMap, exbinds)) =
                  let
                    val vid' = newVId (#context ctx, vid)
                  in
                    case lookupLongVIdInEnv (ctx, env, span, longvid) of
                      Found (longvid, tysc, ids as Syntax.ExceptionConstructor) =>
                        let
                          fun toPureTy (T.TyVar (span, tv)) = T.TyVar (span, tv)
                            | toPureTy (T.AnonymousTyVar _) =
                                emitFatalTypeError
                                  ( ctx
                                  , [span]
                                  , "exception constructor must not refer to anonymous type variable"
                                  )
                            | toPureTy (T.RecordType (span, fields)) =
                                T.RecordType
                                  (span, Syntax.LabelMap.map toPureTy fields)
                            | toPureTy (T.TyCon (span, ta, tn)) =
                                T.TyCon (span, List.map toPureTy ta, tn)
                            | toPureTy (T.FnType (span, a, b)) =
                                T.FnType (span, toPureTy a, toPureTy b)
                            | toPureTy (T.RecordExtType (span, fields, baseTy)) =
                                T.RecordExtType
                                  ( span
                                  , Syntax.LabelMap.map toPureTy fields
                                  , toPureTy baseTy
                                  )
                          val optTy =
                            case tysc of
                              T.TypeScheme ([], T.FnType (_, payloadTy, _)) =>
                                SOME (toPureTy payloadTy)
                            | T.TypeScheme ([], _) => NONE
                            | T.TypeScheme (_ :: _, _) =>
                                ( emitTypeError
                                    ( ctx
                                    , [span]
                                    , "exception constructor must have monomorphic type"
                                    )
                                ; NONE
                                )
                        in
                          ( S.VIdMap.insert
                              (valMap, vid, (tysc, ids, T.MkShortVId vid'))
                          , T.ExReplication (span, vid', longvid, optTy)
                            :: exbinds
                          )
                        end
                    | Found _ =>
                        emitFatalTypeError
                          ( ctx
                          , [span]
                          , "exception replication: RHS must be an exception constructor"
                          )
                    | ValueNotFound notfound =>
                        emitFatalTypeError
                          ( ctx
                          , [span]
                          , "exception replication: RHS must be an exception constructor, but value name '"
                            ^ Syntax.print_LongVId notfound ^ "' was not found"
                          )
                    | StructureNotFound notfound =>
                        emitFatalTypeError
                          ( ctx
                          , [span]
                          , "exception replication: RHS must be an exception constructor, but structure name '"
                            ^ Syntax.print_LongStrId notfound
                            ^ "' was not found"
                          )
                  end
            val (valMap, exbinds) =
              List.foldr doExBind (Syntax.VIdMap.empty, []) exbinds
          in
            (envWithValEnv valMap, [T.ExceptionDec (span, exbinds)])
          end
      | typeCheckDec (ctx, env, S.LocalDec (_, decs1, decs2)) =
          let
            val (env', decs1) = typeCheckDecs (ctx, env, decs1)
            val (env'', decs2) = typeCheckDecs
              (ctx, mergeEnv (env, env'), decs2)
            val env'' =
              { valMap = #valMap env''
              , tyConMap = #tyConMap env''
              , tyNameMap =
                  TypedSyntax.TyNameMap.unionWithSecond
                    (#tyNameMap env', #tyNameMap env'')
              , strMap = #strMap env''
              , sigMap = #sigMap env''
              , funMap = #funMap env''
              , boundTyVars = #boundTyVars env''
              }
          in
            (env'', decs1 @ decs2)
          end
      | typeCheckDec (ctx, env, S.OpenDec (span, longstrids)) =
          let
            fun getStructure (Syntax.MkQualified ([], strid)) =
                  (case Syntax.StrIdMap.find (#strMap env, strid) of
                     SOME (s, T.MkLongStrId (strid0, strids0)) =>
                       { valMap =
                           Syntax.VIdMap.mapi
                             (fn (vid, (tysc, ids)) =>
                                (tysc, ids, T.MkLongVId (strid0, strids0, vid)))
                             (#valMap s)
                       , tyConMap = #tyConMap s
                       , tyNameMap = TypedSyntax.TyNameMap.empty
                       , strMap =
                           Syntax.StrIdMap.mapi
                             (fn (strid', T.MkSignature s) =>
                                (s, T.MkLongStrId (strid0, strids0 @ [strid'])))
                             (#strMap s)
                       , sigMap = Syntax.SigIdMap.empty
                       , funMap = Syntax.FunIdMap.empty
                       , boundTyVars = Syntax.TyVarMap.empty
                       }
                   | NONE =>
                       emitFatalTypeError (ctx, [span], "structure not found"))
              | getStructure (Syntax.MkQualified (strid0 :: strids, strid')) =
                  (case Syntax.StrIdMap.find (#strMap env, strid0) of
                     SOME (s0, T.MkLongStrId (strid0, strids0)) =>
                       let
                         val s = lookupStr
                           (#context ctx, s0, span, strids @ [strid'])
                         val strids = strids0 @ strids @ [strid']
                       in
                         { valMap =
                             Syntax.VIdMap.mapi
                               (fn (vid, (tysc, ids)) =>
                                  (tysc, ids, T.MkLongVId (strid0, strids, vid)))
                               (#valMap s)
                         , tyConMap = #tyConMap s
                         , tyNameMap = TypedSyntax.TyNameMap.empty
                         , strMap =
                             Syntax.StrIdMap.mapi
                               (fn (strid', T.MkSignature s) =>
                                  (s, T.MkLongStrId (strid0, strids @ [strid'])))
                               (#strMap s)
                         , sigMap = Syntax.SigIdMap.empty
                         , funMap = Syntax.FunIdMap.empty
                         , boundTyVars = Syntax.TyVarMap.empty
                         }
                       end
                   | NONE =>
                       emitFatalTypeError (ctx, [span], "structure not found"))
            val env =
              List.foldl
                (fn (longstrid, acc) => mergeEnv (acc, getStructure longstrid))
                emptyEnv longstrids
          in
            (env, [])
          end
      | typeCheckDec (ctx, env, S.OverloadDec (span, class, longtycon, map)) =
          let
            val {typeFunction = T.TypeFunction (tyvars, ty), ...} =
              lookupTyConInEnv (#context ctx, env, span, longtycon)
            val tyname =
              if List.null tyvars then
                case ty of
                  T.TyCon (_, [], tyname) => tyname
                | _ =>
                    emitFatalTypeError
                      ( ctx
                      , [span]
                      , "overload declaration: longtycon must refer to a concrete type"
                      )
              else
                emitFatalTypeError
                  ( ctx
                  , [span]
                  , "overload declaration: longtycon must refer to a concrete type"
                  )
            val ty = T.TyCon (span, [], tyname)
            val map: (T.Ty * T.Exp) Syntax.OverloadKeyMap.map =
              Syntax.OverloadKeyMap.map
                (fn exp => synthTypeOfExp (ctx, env, exp)) map
            val () =
              case Syntax.OverloadKeyMap.find (map, Syntax.OVERLOAD_abs) of
                NONE => ()
              | SOME (ty', _) =>
                  checkSubsumption
                    (ctx, env, span, ty', T.FnType (span, ty, ty))
            val () =
              case Syntax.OverloadKeyMap.find (map, Syntax.OVERLOAD_TILDE) of
                NONE => ()
              | SOME (ty', _) =>
                  checkSubsumption
                    (ctx, env, span, ty', T.FnType (span, ty, ty))
            val () =
              case Syntax.OverloadKeyMap.find (map, Syntax.OVERLOAD_div) of
                NONE => ()
              | SOME (ty', _) =>
                  checkSubsumption (ctx, env, span, ty', T.FnType
                    (span, T.PairType (span, ty, ty), ty))
            val () =
              case Syntax.OverloadKeyMap.find (map, Syntax.OVERLOAD_mod) of
                NONE => ()
              | SOME (ty', _) =>
                  checkSubsumption (ctx, env, span, ty', T.FnType
                    (span, T.PairType (span, ty, ty), ty))
            val () =
              case Syntax.OverloadKeyMap.find (map, Syntax.OVERLOAD_TIMES) of
                NONE => ()
              | SOME (ty', _) =>
                  checkSubsumption (ctx, env, span, ty', T.FnType
                    (span, T.PairType (span, ty, ty), ty))
            val () =
              case Syntax.OverloadKeyMap.find (map, Syntax.OVERLOAD_DIVIDE) of
                NONE => ()
              | SOME (ty', _) =>
                  checkSubsumption (ctx, env, span, ty', T.FnType
                    (span, T.PairType (span, ty, ty), ty))
            val () =
              case Syntax.OverloadKeyMap.find (map, Syntax.OVERLOAD_PLUS) of
                NONE => ()
              | SOME (ty', _) =>
                  checkSubsumption (ctx, env, span, ty', T.FnType
                    (span, T.PairType (span, ty, ty), ty))
            val () =
              case Syntax.OverloadKeyMap.find (map, Syntax.OVERLOAD_MINUS) of
                NONE => ()
              | SOME (ty', _) =>
                  checkSubsumption (ctx, env, span, ty', T.FnType
                    (span, T.PairType (span, ty, ty), ty))
            val () =
              case Syntax.OverloadKeyMap.find (map, Syntax.OVERLOAD_LT) of
                NONE => ()
              | SOME (ty', _) =>
                  checkSubsumption (ctx, env, span, ty', T.FnType
                    (span, T.PairType (span, ty, ty), PrimTypes.bool))
            val () =
              case Syntax.OverloadKeyMap.find (map, Syntax.OVERLOAD_LE) of
                NONE => ()
              | SOME (ty', _) =>
                  checkSubsumption (ctx, env, span, ty', T.FnType
                    (span, T.PairType (span, ty, ty), PrimTypes.bool))
            val () =
              case Syntax.OverloadKeyMap.find (map, Syntax.OVERLOAD_GT) of
                NONE => ()
              | SOME (ty', _) =>
                  checkSubsumption (ctx, env, span, ty', T.FnType
                    (span, T.PairType (span, ty, ty), PrimTypes.bool))
            val () =
              case Syntax.OverloadKeyMap.find (map, Syntax.OVERLOAD_GE) of
                NONE => ()
              | SOME (ty', _) =>
                  checkSubsumption (ctx, env, span, ty', T.FnType
                    (span, T.PairType (span, ty, ty), PrimTypes.bool))
            val () =
              case Syntax.OverloadKeyMap.find (map, Syntax.OVERLOAD_fromInt) of
                NONE => ()
              | SOME (ty', _) =>
                  checkSubsumption
                    (ctx, env, span, ty', T.FnType (span, PrimTypes.int, ty))
            val () =
              case Syntax.OverloadKeyMap.find (map, Syntax.OVERLOAD_fromWord) of
                NONE => ()
              | SOME (ty', _) =>
                  checkSubsumption
                    (ctx, env, span, ty', T.FnType (span, PrimTypes.word, ty))
            val () =
              case Syntax.OverloadKeyMap.find (map, Syntax.OVERLOAD_minInt) of
                NONE => ()
              | SOME (ty', T.SConExp (_, Syntax.IntegerConstant _, _)) =>
                  checkSubsumption (ctx, env, span, ty', PrimTypes.intInf)
              | SOME (_, _) =>
                  emitTypeError
                    (ctx, [span], "overload: minInt must be literal")
            val () =
              case Syntax.OverloadKeyMap.find (map, Syntax.OVERLOAD_maxInt) of
                NONE => ()
              | SOME (ty', T.SConExp (_, Syntax.IntegerConstant _, _)) =>
                  checkSubsumption (ctx, env, span, ty', PrimTypes.intInf)
              | SOME (_, _) =>
                  emitTypeError
                    (ctx, [span], "overload: maxInt must be literal")
            val () =
              case Syntax.OverloadKeyMap.find (map, Syntax.OVERLOAD_wordSize) of
                NONE => ()
              | SOME (ty', T.SConExp (_, Syntax.IntegerConstant _, _)) =>
                  checkSubsumption (ctx, env, span, ty', PrimTypes.int)
              | SOME (_, _) =>
                  emitTypeError
                    (ctx, [span], "overload: wordSize must be literal")
            val () =
              case Syntax.OverloadKeyMap.find (map, Syntax.OVERLOAD_maxOrd) of
                NONE => ()
              | SOME (ty', T.SConExp (_, Syntax.IntegerConstant _, _)) =>
                  checkSubsumption (ctx, env, span, ty', PrimTypes.intInf)
              | SOME (_, _) =>
                  emitTypeError
                    (ctx, [span], "overload: maxOrd must be literal")
            val attr = lookupTyNameInEnv (#context ctx, env, span, tyname)
            val attr =
              { arity = #arity attr
              , admitsEquality = #admitsEquality attr
              , overloadClass = SOME class
              }
            val env' = envWithTyConEnv
              ( Syntax.TyConMap.empty
              , TypedSyntax.TyNameMap.singleton (tyname, attr)
              )
          in
            ( env'
            , [T.OverloadDec
                 (span, class, tyname, Syntax.OverloadKeyMap.map #2 map)]
            )
          end
      | typeCheckDec (ctx, env, S.EqualityDec (span, typarams, longtycon, exp)) =
          let
            val {typeFunction = T.TypeFunction (tyvars, ty), ...} =
              lookupTyConInEnv (#context ctx, env, span, longtycon)
            val tyname =
              if List.length tyvars = List.length typarams then
                case ty of
                  T.TyCon (_, tyvars', tyname) =>
                    if List.length tyvars' = List.length typarams then
                      tyname
                    else
                      emitFatalTypeError
                        ( ctx
                        , [span]
                        , "equality declaration: longtycon must refer to a concrete type"
                        )
                | _ =>
                    emitFatalTypeError
                      ( ctx
                      , [span]
                      , "equality declaration: longtycon must refer to a concrete type"
                      )
              else
                emitFatalTypeError
                  ( ctx
                  , [span]
                  , "equality declaration: number of type parameters mismatch"
                  )
            val typarams' =
              List.map (fn tv => (tv, genTyVar (#context ctx, tv))) typarams
            val () =
              if isRefOrArray tyname then
                List.app
                  (fn Syntax.MkTyVar tvname =>
                     if String.isPrefix "''" tvname then
                       emitTypeError
                         ( ctx
                         , [span]
                         , "equality for ref or array cannot depend on the equality of its type parameter"
                         )
                     else
                       ()) typarams
              else
                ()
            val ty = T.TyCon
              ( span
              , List.map (fn (_, tv) => T.TyVar (span, tv)) typarams'
              , tyname
              )
            val attr = lookupTyNameInEnv (#context ctx, env, span, tyname)
            val () =
              if #admitsEquality attr then
                emitTypeError
                  ( ctx
                  , [span]
                  , "equality declaration: the type already admits equality"
                  )
              else
                ()
            val attr =
              { arity = #arity attr
              , admitsEquality = true
              , overloadClass = #overloadClass attr
              }
            val env' = envWithTyConEnv
              ( Syntax.TyConMap.empty
              , TypedSyntax.TyNameMap.singleton (tyname, attr)
              )
            val innerEnv = mergeEnv (env, env')
            val innerEnv =
              { valMap = #valMap innerEnv
              , tyConMap = #tyConMap innerEnv
              , tyNameMap = #tyNameMap innerEnv
              , strMap = #strMap innerEnv
              , sigMap = #sigMap innerEnv
              , funMap = #funMap innerEnv
              , boundTyVars =
                  List.foldl Syntax.TyVarMap.insert' (#boundTyVars innerEnv)
                    typarams'
              }
            val exp = checkTypeOfExp (ctx, innerEnv, exp, T.FnType
              (span, T.PairType (span, ty, ty), PrimTypes.bool)) (* allow recursion *)
          in
            (env', [T.EqualityDec (span, List.map #2 typarams', tyname, exp)])
          end
      | typeCheckDec
          (ctx, env, S.ESImportDec {sourceSpan, pure, specs, moduleName}) =
          let
            val specs' =
              List.map
                (fn (name, vid, NONE) =>
                   (name, vid, newVId (#context ctx, vid), PrimTypes.js_value)
                  | (name, vid, SOME ty) =>
                   ( name
                   , vid
                   , newVId (#context ctx, vid)
                   , evalTy (#context ctx, env, ty)
                   )) specs
            val valMap =
              List.foldl
                (fn ((_, vid, vid', ty), valMap) =>
                   ( if Syntax.VIdMap.inDomain (valMap, vid) then
                       emitTypeError
                         ( ctx
                         , [sourceSpan]
                         , "duplicate identifier in a binding"
                         )
                     else
                       ()
                   ; Syntax.VIdMap.insert
                       ( valMap
                       , vid
                       , ( T.TypeScheme
                             ([], ty) (* for now, monomorphic type only *)
                         , Syntax.ValueVariable
                         , T.MkShortVId vid'
                         )
                       )
                   )) Syntax.VIdMap.empty specs'
            val env' =
              { valMap = valMap
              , tyConMap = Syntax.TyConMap.empty
              , tyNameMap = TypedSyntax.TyNameMap.empty
              , strMap = Syntax.StrIdMap.empty
              , sigMap = Syntax.SigIdMap.empty
              , funMap = Syntax.FunIdMap.empty
              , boundTyVars = Syntax.TyVarMap.empty
              }
          in
            ( env'
            , [T.ESImportDec
                 { sourceSpan = sourceSpan
                 , pure = pure
                 , specs =
                     List.map (fn (name, _, vid, ty) => (name, vid, ty)) specs'
                 , moduleName = moduleName
                 }]
            )
          end
    and typeCheckDecs (_, _, []) : Env * T.Dec list = (emptyEnv, [])
      | typeCheckDecs (ctx, env, dec :: decs) =
          let
            val (env', dec) = typeCheckDec (ctx, env, dec)
            val (env'', decs) = typeCheckDecs (ctx, mergeEnv (env, env'), decs)
          in
            (mergeEnv (env', env''), dec @ decs)
          end
    and synthTypeOfMatch (ctx, env, _, (pat0, exp0) :: rest) :
      T.Ty * T.Ty * (T.Pat * T.Exp) list =
          let
            fun doBranch (pat, exp) =
              let
                val patSpan = Syntax.getSourceSpanOfPat pat
                val (patTy, vars, pat') =
                  synthTypeOfPat (ctx, env, pat, Syntax.VIdMap.empty)
                val env' = mergeWithValEnv
                  ( env
                  , Syntax.VIdMap.map
                      (fn (vid, ty) =>
                         ( T.TypeScheme ([], ty)
                         , Syntax.ValueVariable
                         , T.MkShortVId vid
                         )) vars
                  )
                val expSpan = Syntax.getSourceSpanOfExp exp
                val (expTy, exp') = synthTypeOfExp (ctx, env', exp)
              in
                (patSpan, expSpan, patTy, expTy, pat', exp')
              end
            val (_, _, patTy0, expTy0, pat0', exp0') = doBranch (pat0, exp0)
            val rest' = List.map doBranch rest
            val (patTy, expTy) =
              List.foldl
                (fn ((patSpan, expSpan, patTy, expTy, _, _), (patTy', expTy')) =>
                   ( commonType
                       (ctx, env, ConstraintInfo.BRANCH, patSpan, patTy', patTy)
                   , commonType
                       (ctx, env, ConstraintInfo.BRANCH, expSpan, expTy', expTy)
                   )) (patTy0, expTy0) rest'
          in
            ( patTy
            , expTy
            , (pat0', exp0')
              :: List.map (fn (_, _, _, _, pat, exp) => (pat, exp)) rest'
            )
          end
      | synthTypeOfMatch (ctx, _, span, nil) =
          emitFatalTypeError
            (ctx, [span], "invalid syntax tree: match is empty")
    and checkAndSynthTypeOfMatch
          (ctx, env, _, (pat0, exp0) :: rest, expectedPatTy) :
      T.Ty * (T.Pat * T.Exp) list =
          let
            fun doBranch (pat, exp) =
              let
                val (vars, pat') = checkTypeOfPat
                  (ctx, env, pat, expectedPatTy, Syntax.VIdMap.empty)
                val env' = mergeWithValEnv
                  ( env
                  , Syntax.VIdMap.map
                      (fn (vid, ty) =>
                         ( T.TypeScheme ([], ty)
                         , Syntax.ValueVariable
                         , T.MkShortVId vid
                         )) vars
                  )
                val expSpan = Syntax.getSourceSpanOfExp exp
                val (expTy, exp') = synthTypeOfExp (ctx, env', exp)
              in
                (expSpan, expTy, pat', exp')
              end
            val (_, expTy0, pat0', exp0') = doBranch (pat0, exp0)
            val rest' = List.map doBranch rest
            val expTy =
              List.foldl
                (fn ((expSpan, expTy, _, _), expTy') =>
                   commonType
                     (ctx, env, ConstraintInfo.BRANCH, expSpan, expTy', expTy))
                expTy0 rest'
          in
            ( expTy
            , (pat0', exp0')
              :: List.map (fn (_, _, pat, exp) => (pat, exp)) rest'
            )
          end
      | checkAndSynthTypeOfMatch (ctx, _, span, nil, _) =
          emitFatalTypeError
            (ctx, [span], "invalid syntax tree: match is empty")
    and checkTypeOfMatch
      ( ctx
      , env
      , patsAndExps: (S.Pat * S.Exp) list
      , expectedPatTy: T.Ty
      , expectedExpTy: T.Ty
      ) : (T.Pat * T.Exp) list =
      let
        fun doBranch (pat, exp) =
          let
            val (vars, pat') = checkTypeOfPat
              (ctx, env, pat, expectedPatTy, Syntax.VIdMap.empty)
            val env' = mergeWithValEnv
              ( env
              , Syntax.VIdMap.map
                  (fn (vid, ty) =>
                     ( T.TypeScheme ([], ty)
                     , Syntax.ValueVariable
                     , T.MkShortVId vid
                     )) vars
              )
          in
            (pat', checkTypeOfExp (ctx, env', exp, expectedExpTy))
          end
      in
        List.map doBranch patsAndExps
      end

    (* pretty printing *)
    (*
    structure PrettyPrint = struct
    fun print_Env ({ tyConMap, valMap, strMap, boundTyVars, ... } : Env) = "Env{tyMap=" ^ TypedSyntax.print_TyConMap (fn _ => "TypeStructure _") tyConMap ^ ",valMap=" ^ TypedSyntax.print_VIdMap (Syntax.print_pair (TypedSyntax.print_TypeScheme, Syntax.print_IdStatus)) valMap ^ ",strMap=...,boundTyVars=...}"
    end (* structure PrettyPrint *)
    open PrettyPrint
    *)
    (*: val applyDefaultTypes : Context * TypedSyntax.Dec list -> unit *)
    fun applyDefaultTypes (ctx, decs: T.Dec list) : unit =
      let
        fun findClass [] = NONE
          | findClass ((span, TypedSyntax.IsInt) :: _) =
              SOME (span, Syntax.CLASS_INT)
          | findClass ((span, TypedSyntax.IsWord) :: _) =
              SOME (span, Syntax.CLASS_WORD)
          | findClass ((span, TypedSyntax.IsReal) :: _) =
              SOME (span, Syntax.CLASS_REAL)
          | findClass ((span, TypedSyntax.IsChar) :: _) =
              SOME (span, Syntax.CLASS_CHAR)
          | findClass ((span, TypedSyntax.IsString) :: _) =
              SOME (span, Syntax.CLASS_STRING)
          | findClass (_ :: xs) = findClass xs
        fun doInt (span1, xs) =
          ( List.app
              (fn (span2, c) =>
                 case c of
                   TypedSyntax.IsRecord _ =>
                     emitError
                       (ctx, [span1, span2], "invalid record syntax for int")
                 | TypedSyntax.IsEqType => ()
                 | TypedSyntax.IsIntegral => ()
                 | TypedSyntax.IsSignedReal => ()
                 | TypedSyntax.IsRing => ()
                 | TypedSyntax.IsOrdered => ()
                 | TypedSyntax.IsInt => ()
                 | TypedSyntax.IsWord =>
                     emitError
                       (ctx, [span1, span2], "type mismatch: int vs word")
                 | TypedSyntax.IsReal =>
                     emitError
                       (ctx, [span1, span2], "type mismatch: int vs real")
                 | TypedSyntax.IsChar =>
                     emitError
                       (ctx, [span1, span2], "type mismatch: int vs char")
                 | TypedSyntax.IsString =>
                     emitError
                       (ctx, [span1, span2], "type mismatch: int vs string")) xs
          ; PrimTypes.int
          )
        fun doWord (span1, xs) =
          ( List.app
              (fn (span2, c) =>
                 case c of
                   TypedSyntax.IsRecord _ =>
                     emitError
                       (ctx, [span1, span2], "invalid record syntax for word")
                 | TypedSyntax.IsEqType => ()
                 | TypedSyntax.IsIntegral => ()
                 | TypedSyntax.IsSignedReal =>
                     emitError (ctx, [span1, span2], "abs is invalid for word")
                 | TypedSyntax.IsRing => ()
                 | TypedSyntax.IsOrdered => ()
                 | TypedSyntax.IsInt =>
                     emitError
                       (ctx, [span1, span2], "type mismatch: word vs int")
                 | TypedSyntax.IsWord => ()
                 | TypedSyntax.IsReal =>
                     emitError
                       (ctx, [span1, span2], "type mismatch: word vs real")
                 | TypedSyntax.IsChar =>
                     emitError
                       (ctx, [span1, span2], "type mismatch: word vs char")
                 | TypedSyntax.IsString =>
                     emitError
                       (ctx, [span1, span2], "type mismatch: word vs string"))
              xs
          ; PrimTypes.word
          )
        fun doReal (span1, xs) =
          ( List.app
              (fn (span2, c) =>
                 case c of
                   TypedSyntax.IsRecord _ =>
                     emitError
                       (ctx, [span1, span2], "invalid record syntax for real")
                 | TypedSyntax.IsEqType =>
                     emitError
                       (ctx, [span1, span2], "real does not admit equality")
                 | TypedSyntax.IsIntegral =>
                     emitError
                       (ctx, [span1, span2], "div, mod are invalid for real")
                 | TypedSyntax.IsSignedReal => ()
                 | TypedSyntax.IsRing => ()
                 | TypedSyntax.IsOrdered => ()
                 | TypedSyntax.IsInt =>
                     emitError
                       (ctx, [span1, span2], "type mismatch: real vs int")
                 | TypedSyntax.IsWord =>
                     emitError
                       (ctx, [span1, span2], "type mismatch: real vs word")
                 | TypedSyntax.IsReal => ()
                 | TypedSyntax.IsChar =>
                     emitError
                       (ctx, [span1, span2], "type mismatch: real vs char")
                 | TypedSyntax.IsString =>
                     emitError
                       (ctx, [span1, span2], "type mismatch: real vs string"))
              xs
          ; PrimTypes.real
          )
        fun doChar (span1, xs) =
          ( List.app
              (fn (span2, c) =>
                 case c of
                   TypedSyntax.IsRecord _ =>
                     emitError
                       (ctx, [span1, span2], "invalid record syntax for char")
                 | TypedSyntax.IsEqType => ()
                 | TypedSyntax.IsIntegral =>
                     emitError
                       (ctx, [span1, span2], "invalid operation on char")
                 | TypedSyntax.IsSignedReal =>
                     emitError
                       (ctx, [span1, span2], "invalid operation on char")
                 | TypedSyntax.IsRing =>
                     emitError
                       (ctx, [span1, span2], "invalid operation on char")
                 | TypedSyntax.IsOrdered => ()
                 | TypedSyntax.IsInt =>
                     emitError
                       (ctx, [span1, span2], "type mismatch: char vs int")
                 | TypedSyntax.IsWord =>
                     emitError
                       (ctx, [span1, span2], "type mismatch: char vs word")
                 | TypedSyntax.IsReal =>
                     emitError
                       (ctx, [span1, span2], "type mismatch: char vs real")
                 | TypedSyntax.IsChar => ()
                 | TypedSyntax.IsString =>
                     emitError
                       (ctx, [span1, span2], "type mismatch: char vs string"))
              xs
          ; PrimTypes.char
          )
        fun doString (span1, xs) =
          ( List.app
              (fn (span2, c) =>
                 case c of
                   TypedSyntax.IsRecord _ =>
                     emitError
                       (ctx, [span1, span2], "invalid record syntax for string")
                 | TypedSyntax.IsEqType => ()
                 | TypedSyntax.IsIntegral =>
                     emitError
                       (ctx, [span1, span2], "invalid operation on string")
                 | TypedSyntax.IsSignedReal =>
                     emitError
                       (ctx, [span1, span2], "invalid operation on string")
                 | TypedSyntax.IsRing =>
                     emitError
                       (ctx, [span1, span2], "invalid operation on string")
                 | TypedSyntax.IsOrdered => ()
                 | TypedSyntax.IsInt =>
                     emitError
                       (ctx, [span1, span2], "type mismatch: string vs int")
                 | TypedSyntax.IsWord =>
                     emitError
                       (ctx, [span1, span2], "type mismatch: string vs word")
                 | TypedSyntax.IsReal =>
                     emitError
                       (ctx, [span1, span2], "type mismatch: string vs real")
                 | TypedSyntax.IsChar =>
                     emitError
                       (ctx, [span1, span2], "type mismatch: string vs char")
                 | TypedSyntax.IsString => ()) xs
          ; PrimTypes.string
          )
        fun doIntOrReal (_, []) = PrimTypes.int
          | doIntOrReal (span1, (span2, c) :: xs) =
              case c of
                TypedSyntax.IsRecord _ =>
                  ( emitError (ctx, [span1, span2], "unresolved flex record")
                  ; doIntOrReal (span1, xs)
                  )
              | TypedSyntax.IsEqType => doInt (span1, xs)
              | TypedSyntax.IsIntegral => doInt (span1, xs)
              | TypedSyntax.IsSignedReal => doIntOrReal (span1, xs)
              | TypedSyntax.IsRing => doIntOrReal (span1, xs)
              | TypedSyntax.IsOrdered => doIntOrReal (span1, xs)
              | TypedSyntax.IsInt => doInt (span1, xs) (* cannot occur *)
              | TypedSyntax.IsWord => doIntOrReal (span1, xs) (* cannot occur *)
              | TypedSyntax.IsReal => doReal (span1, xs) (* cannot occur *)
              | TypedSyntax.IsChar => doIntOrReal (span1, xs) (* cannot occur *)
              | TypedSyntax.IsString =>
                  doIntOrReal (span1, xs) (* cannot occur *)
        fun defaultTyForConstraints (_, _, []) = PrimTypes.unit
          | defaultTyForConstraints (eq, spans, (span1, c) :: xs) =
              case c of
                TypedSyntax.IsRecord _ =>
                  ( emitError (ctx, [span1], "unresolved flex record")
                  ; defaultTyForConstraints (eq, spans, xs)
                  )
              | TypedSyntax.IsEqType =>
                  defaultTyForConstraints
                    (true, if List.null spans then [span1] else spans, xs)
              | TypedSyntax.IsIntegral => doInt (span1, xs)
              | TypedSyntax.IsSignedReal =>
                  if eq then doInt (span1, xs) else doIntOrReal (span1, xs)
              | TypedSyntax.IsRing =>
                  if eq then doInt (span1, xs) else doIntOrReal (span1, xs)
              | TypedSyntax.IsOrdered =>
                  if eq then doInt (span1, xs) else doIntOrReal (span1, xs)
              | TypedSyntax.IsInt => doInt (span1, xs) (* cannot occur *)
              | TypedSyntax.IsWord => doWord (span1, xs) (* cannot occur *)
              | TypedSyntax.IsReal =>
                  ( if eq then
                      emitError
                        (ctx, span1 :: spans, "real does not admit equality")
                    else
                      ()
                  ; doReal (span1, xs)
                  ) (* cannot occur *)
              | TypedSyntax.IsChar => doChar (span1, xs) (* cannot occur *)
              | TypedSyntax.IsString => doString (span1, xs) (* cannot occur *)
        fun doTyVar tv =
          case !tv of
            T.Link _ => ()
          | T.Unbound ({sourceSpan, equalityRequired = _, class}, _) =>
              let
                val ty =
                  case class of
                    SOME T.Int => PrimTypes.int
                  | SOME T.Word => PrimTypes.word
                  | SOME T.Real => PrimTypes.real
                  | SOME T.Char => PrimTypes.char
                  | SOME T.String => PrimTypes.string
                  | SOME T.NumTxt => PrimTypes.int
                  | SOME T.IntWordReal => PrimTypes.int
                  | SOME T.IntWord => PrimTypes.int
                  | SOME T.IntReal => PrimTypes.int
                  | SOME (T.Record _) =>
                      ( emitError (ctx, [sourceSpan], "unresolved flex record")
                      ; PrimTypes.unit
                      )
                  | NONE => PrimTypes.unit
              in
                tv := T.Link ty
              end
      in
        List.app doTyVar (TypedSyntax.freeTyVarsInDecs ([], decs))
      end

    local
      fun checkValDesc
        ( ctx
        , _
        , span
        , expected as T.TypeScheme (_, _)
        , actual as T.TypeScheme (_, _)
        , origin
        ) =
        let
          val ictx = {context = ctx, level = 0}
          val (tyE, _) = instantiate (ictx, span, expected)
          val (tyA, _) = instantiate (ictx, span, actual)
          fun onMismatch (_ (* expected *)) =
            case origin of
              T.VALDESC_COMMENT =>
                (case #valDescInComments (#languageOptions ctx) of
                   LanguageOptions.ERROR =>
                     ( Message.error
                         ( #messageHandler ctx
                         , [span]
                         , "type"
                         , "value description mismatch"
                         )
                     ; false
                     )
                 | LanguageOptions.WARN =>
                     ( Message.warning
                         ( #messageHandler ctx
                         , [span]
                         , "type"
                         , "value description mismatch"
                         )
                     ; false
                     )
                 | LanguageOptions.IGNORE => false)
            | T.VALDESC_SEQUENCE =>
                (case #sequenceNonUnit (#languageOptions ctx) of
                   LanguageOptions.ERROR =>
                     ( Message.error
                         ( #messageHandler ctx
                         , [span]
                         , "type"
                         , "sequence expression not of type unit"
                         )
                     ; false
                     )
                 | LanguageOptions.WARN =>
                     ( Message.warning
                         ( #messageHandler ctx
                         , [span]
                         , "type"
                         , "sequence expression not of type unit"
                         )
                     ; false
                     )
                 | LanguageOptions.IGNORE => false)
          fun equalTy (expected as T.TyVar (_, tv), T.TyVar (_, tv')) =
                if tv = tv' then true else onMismatch expected
            | equalTy
                ( expected as
                    T.AnonymousTyVar
                      ( _
                      , data as
                          ref
                            (T.Unbound ({equalityRequired = isEqType, ...}, _))
                      )
                , T.AnonymousTyVar
                    ( _
                    , data' as
                        ref (T.Unbound ({equalityRequired = isEqType', ...}, _))
                    )
                ) =
                if data <> data' then
                  if isEqType = isEqType' then
                    let
                      val ty = T.TyVar
                        (span, genTyVar (ctx, Syntax.MkTyVar "?"))
                    in
                      data := T.Link ty;
                      data' := T.Link ty;
                      true
                    end
                  else
                    onMismatch expected
                else
                  true
            | equalTy (T.AnonymousTyVar (_, ref (T.Link ty)), ty') =
                equalTy (ty, ty')
            | equalTy (ty, T.AnonymousTyVar (_, ref (T.Link ty'))) =
                equalTy (ty, ty')
            | equalTy
                ( expected as T.RecordType (_, fields)
                , T.RecordType (_, fields')
                ) =
                if S.LabelMap.numItems fields = S.LabelMap.numItems fields' then
                  S.LabelMap.alli
                    (fn (label, ty) =>
                       case S.LabelMap.find (fields', label) of
                         SOME ty' => equalTy (ty, ty')
                       | NONE => onMismatch expected) fields
                else
                  onMismatch expected
            | equalTy
                ( expected as T.TyCon (_, tyargs, tyname)
                , T.TyCon (_, tyargs', tyname')
                ) =
                if
                  tyname = tyname'
                  andalso List.length tyargs = List.length tyargs'
                then ListPair.allEq equalTy (tyargs, tyargs')
                else onMismatch expected
            | equalTy (T.FnType (_, ty1, ty2), T.FnType (_, ty1', ty2')) =
                equalTy (ty1, ty1') andalso equalTy (ty2, ty2')
            | equalTy (ty1, _) = onMismatch ty1
        in
          ignore (equalTy (tyE, tyA))
        end
      fun checkExp (_: Context, _, T.SConExp _) : unit = ()
        | checkExp (_, _, T.VarExp _) = ()
        | checkExp (ctx, env, T.RecordExp (_, fields)) =
            List.app (fn (_, exp) => checkExp (ctx, env, exp)) fields
        | checkExp
            ( ctx
            , env
            , T.RecordExtExp {sourceSpan = _, fields, baseExp, baseTy = _}
            ) =
            ( List.app (fn (_, exp) => checkExp (ctx, env, exp)) fields
            ; checkExp (ctx, env, baseExp)
            )
        | checkExp (ctx, env, T.LetInExp (_, decs, exp)) =
            (checkDecs (ctx, env, decs); checkExp (ctx, env, exp))
        | checkExp (ctx, env, T.AppExp (_, e1, e2)) =
            (checkExp (ctx, env, e1); checkExp (ctx, env, e2))
        | checkExp (ctx, env, T.TypedExp (_, e, _)) = checkExp (ctx, env, e)
        | checkExp (ctx, env, T.HandleExp (_, exp, match, _)) =
            (checkExp (ctx, env, exp); checkMatch (ctx, env, match))
        | checkExp (ctx, env, T.RaiseExp (_, _, exp)) = checkExp (ctx, env, exp)
        | checkExp (ctx, env, T.IfThenElseExp (_, e1, e2, e3)) =
            ( checkExp (ctx, env, e1)
            ; checkExp (ctx, env, e2)
            ; checkExp (ctx, env, e3)
            )
        | checkExp
            ( ctx
            , env
            , T.CaseExp
                { sourceSpan = _
                , subjectExp
                , subjectTy = _
                , matches
                , matchType = _
                , resultTy = _
                }
            ) =
            (checkExp (ctx, env, subjectExp); checkMatch (ctx, env, matches))
        | checkExp (ctx, env, T.FnExp (_, _, _, exp)) = checkExp (ctx, env, exp)
        | checkExp (_, _, T.ProjectionExp _) = ()
        | checkExp (ctx, env, T.ListExp (_, elems, _)) =
            Vector.app (fn e => checkExp (ctx, env, e)) elems
        | checkExp (ctx, env, T.VectorExp (_, elems, _)) =
            Vector.app (fn e => checkExp (ctx, env, e)) elems
        | checkExp (ctx, env, T.PrimExp (_, _, _, args)) =
            Vector.app (fn e => checkExp (ctx, env, e)) args
        | checkExp (_, _, T.BogusExp _) = ()
      and checkDec (ctx, env, T.ValDec (_, valbinds)) =
            List.app (fn valbind => checkValBind (ctx, env, valbind)) valbinds
        | checkDec (ctx, env, T.RecValDec (_, valbinds)) =
            List.app (fn valbind => checkValBind (ctx, env, valbind)) valbinds
        | checkDec (ctx, env, T.IgnoreDec (_, exp, _)) =
            checkExp (ctx, env, exp)
        | checkDec (_, _, T.TypeDec _) = ()
        | checkDec (_, _, T.DatatypeDec _) = ()
        | checkDec (_, _, T.ExceptionDec _) = ()
        | checkDec (ctx, env, T.OverloadDec (_, _, _, map)) =
            Syntax.OverloadKeyMap.app (fn exp => checkExp (ctx, env, exp)) map
        | checkDec (ctx, env, T.EqualityDec (_, tyvars, _, exp)) =
            checkExp (ctx, T.TyVarSet.addList (env, tyvars), exp)
        | checkDec
            (ctx, env, T.ValDescDec {sourceSpan, expected, actual, origin}) =
            checkValDesc (ctx, env, sourceSpan, expected, actual, origin)
        | checkDec (_, _, T.ESImportDec _) = ()
      and checkDecs (ctx, env, decs) =
        List.app (fn dec => checkDec (ctx, env, dec)) decs
      and checkMatch (ctx, env, matches) =
        List.app (fn (_, exp) => checkExp (ctx, env, exp)) matches
      and checkValBind (ctx, env, T.TupleBind (_, _, exp)) =
            checkExp (ctx, env, exp)
        | checkValBind
            (ctx, env, T.PolyVarBind (_, _, T.TypeScheme (tyvars, _), exp)) =
            checkExp (ctx, T.TyVarSet.addList (env, List.map #1 tyvars), exp)
    in val checkTypeDescriptionInDecs = checkDecs
    end

    fun typeCheckCoreDecs (ctx: Context, env, decs) : Env * TypedSyntax.Dec list =
      let
        val ictx = {context = ctx, level = 0}
        val (env, decs) = typeCheckDecs (ictx, env, decs)
        val () = applyDefaultTypes (ctx, decs)
        val decs = #doDecs (TypedSyntax.forceTyIn ctx) decs
        val () = checkTypeDescriptionInDecs (ctx, T.TyVarSet.empty, decs)
      in
        (env, decs)
      end

    fun checkTyScope (ctx, tvset: T.TyVarSet.set, tynameset: T.TyNameSet.set) =
      let
        fun goTy (T.TyVar (span, tv)) =
              if T.TyVarSet.member (tvset, tv) then
                ()
              else
                emitError
                  ( ctx
                  , [span]
                  , "type variable scope violation: "
                    ^ TypedSyntax.PrettyPrint.print_TyVar tv
                  )
          | goTy (T.AnonymousTyVar (_, ref (T.Link ty))) = goTy ty
          | goTy (T.AnonymousTyVar (span, tv as ref (T.Unbound _))) =
              emitError
                ( ctx
                , [span]
                , "type variable scope violation: "
                  ^ TypedSyntax.PrettyPrint.print_AnonymousTyVar tv
                )
          | goTy (T.RecordType (_, fields)) = Syntax.LabelMap.app goTy fields
          | goTy (T.RecordExtType (_, fields, baseTy)) =
              (Syntax.LabelMap.app goTy fields; goTy baseTy)
          | goTy (T.TyCon (span, tyargs, tyname)) =
              ( if T.TyNameSet.member (tynameset, tyname) then
                  ()
                else
                  emitError
                    ( ctx
                    , [span]
                    , "type constructor scope violation: "
                      ^ TypedSyntax.PrettyPrint.print_TyName tyname
                    )
              ; List.app goTy tyargs
              )
          | goTy (T.FnType (_, ty1, ty2)) =
              (goTy ty1; goTy ty2)
        fun goPureTy (T.TyVar (span, tv)) =
              if T.TyVarSet.member (tvset, tv) then
                ()
              else
                emitError
                  ( ctx
                  , [span]
                  , "type variable scope violation: "
                    ^ TypedSyntax.PrettyPrint.print_TyVar tv
                  )
          | goPureTy (T.AnonymousTyVar (_, x)) = Void.absurd x
          | goPureTy (T.RecordType (_, fields)) =
              Syntax.LabelMap.app goPureTy fields
          | goPureTy (T.RecordExtType (_, fields, baseTy)) =
              (Syntax.LabelMap.app goPureTy fields; goPureTy baseTy)
          | goPureTy (T.TyCon (span, tyargs, tyname)) =
              ( if T.TyNameSet.member (tynameset, tyname) then
                  ()
                else
                  emitError
                    ( ctx
                    , [span]
                    , "type constructor scope violation: "
                      ^ TypedSyntax.PrettyPrint.print_TyName tyname
                    )
              ; List.app goPureTy tyargs
              )
          | goPureTy (T.FnType (_, ty1, ty2)) =
              (goPureTy ty1; goPureTy ty2)
        fun goTypeScheme (T.TypeScheme (typarams, ty)) =
          #goTy
            (checkTyScope
               ( ctx
               , T.TyVarSet.addList (tvset, List.map #1 typarams)
               , tynameset
               )) ty
        fun goPat (T.WildcardPat _) = ()
          | goPat (T.SConPat _) = ()
          | goPat (T.VarPat (_, _, ty)) = goTy ty
          | goPat
              (T.RecordPat {sourceSpan = _, fields, ellipsis, wholeRecordType}) =
              ( List.app (fn (_, pat) => goPat pat) fields
              ; Option.app goPat ellipsis
              ; goTy wholeRecordType
              )
          | goPat
              (T.ConPat
                 { sourceSpan = _
                 , longvid = _
                 , payload
                 , tyargs
                 , valueConstructorInfo = _
                 }) =
              ( List.app goTy tyargs
              ; Option.app (fn (ty, pat) => (goTy ty; goPat pat)) payload
              )
          | goPat (T.TypedPat (_, pat, ty)) =
              (goTy ty; goPat pat)
          | goPat (T.LayeredPat (_, _, ty, pat)) =
              (goTy ty; goPat pat)
          | goPat (T.VectorPat (_, pats, _, elemTy)) =
              (goTy elemTy; Vector.app goPat pats)
          | goPat (T.BogusPat (_, ty, pats)) =
              (goTy ty; List.app (fn (ty, pat) => (goTy ty; goPat pat)) pats)
        fun goExp (T.SConExp (_, _, ty)) = goTy ty
          | goExp (T.VarExp (_, _, _, tyargs)) =
              List.app (fn (ty, _) => goTy ty) tyargs
          | goExp (T.RecordExp (_, fields)) =
              List.app (fn (_, exp) => goExp exp) fields
          | goExp (T.RecordExtExp {sourceSpan = _, fields, baseExp, baseTy}) =
              ( List.app (fn (_, exp) => goExp exp) fields
              ; goExp baseExp
              ; goTy baseTy
              )
          | goExp (T.LetInExp (_, decs, exp)) =
              let
                val tynameset = goDecs decs
                val {goExp, ...} = checkTyScope (ctx, tvset, tynameset)
              in
                goExp exp
              end
          | goExp (T.AppExp (_, exp1, exp2)) =
              (goExp exp1; goExp exp2)
          | goExp (T.TypedExp (_, exp, ty)) =
              (goExp exp; goTy ty)
          | goExp (T.HandleExp (_, exp, matches, resultTy)) =
              ( goExp exp
              ; goTy resultTy
              ; List.app (fn (pat, exp) => (goPat pat; goExp exp)) matches
              )
          | goExp (T.RaiseExp (_, ty, exp)) =
              (goTy ty; goExp exp)
          | goExp (T.IfThenElseExp (_, exp1, exp2, exp3)) =
              (goExp exp1; goExp exp2; goExp exp3)
          | goExp
              (T.CaseExp
                 { sourceSpan = _
                 , subjectExp
                 , subjectTy
                 , matches
                 , matchType = _
                 , resultTy
                 }) =
              ( goExp subjectExp
              ; goTy subjectTy
              ; goTy resultTy
              ; List.app (fn (pat, exp) => (goPat pat; goExp exp)) matches
              )
          | goExp (T.FnExp (_, _, ty, exp)) =
              (goTy ty; goExp exp)
          | goExp
              (T.ProjectionExp {sourceSpan = _, label = _, recordTy, fieldTy}) =
              (goTy recordTy; goTy fieldTy)
          | goExp (T.ListExp (_, xs, ty)) =
              (Vector.app goExp xs; goTy ty)
          | goExp (T.VectorExp (_, xs, ty)) =
              (Vector.app goExp xs; goTy ty)
          | goExp (T.PrimExp (_, _, tyargs, args)) =
              (Vector.app goTy tyargs; Vector.app goExp args)
          | goExp (T.BogusExp (_, ty)) = goTy ty
        and goDec (T.ValDec (_, valbinds)) =
              (List.app goValBind valbinds; tynameset)
          | goDec (T.RecValDec (_, valbinds)) =
              (List.app goValBind valbinds; tynameset)
          | goDec (T.IgnoreDec (_, exp, ty)) =
              (goExp exp; goTy ty; tynameset)
          | goDec (T.TypeDec (_, typbinds)) =
              let
                fun goTypBind (T.TypBind (_, tyvars, _, ty)) =
                  let
                    val {goPureTy, ...} = checkTyScope
                      (ctx, T.TyVarSet.addList (tvset, tyvars), tynameset)
                  in
                    goPureTy ty
                  end
              in
                List.app goTypBind typbinds;
                tynameset
              end
          | goDec (T.DatatypeDec (_, datbinds)) =
              let
                val tynameset =
                  List.foldl
                    (fn (T.DatBind (_, _, tyname, _, _), tynameset) =>
                       T.TyNameSet.add (tynameset, tyname)) tynameset datbinds
                fun goDatBind (T.DatBind (_, tyvars, _, conbinds, _)) =
                  let
                    val {goPureTy, ...} = checkTyScope
                      (ctx, T.TyVarSet.addList (tvset, tyvars), tynameset)
                    fun goConBind (T.ConBind (_, _, optTy, _)) =
                      Option.app goPureTy optTy
                  in
                    List.app goConBind conbinds
                  end
              in
                List.app goDatBind datbinds;
                tynameset
              end
          | goDec (T.ExceptionDec (_, exbinds)) =
              ( List.app
                  (fn T.ExBind (_, _, optTy) => Option.app goPureTy optTy
                    | T.ExReplication (_, _, _, optTy) =>
                     Option.app goPureTy optTy) exbinds
              ; tynameset
              )
          | goDec (T.OverloadDec (span, _, tyname, map)) =
              ( if T.TyNameSet.member (tynameset, tyname) then
                  ()
                else
                  emitError
                    ( ctx
                    , [span]
                    , "type constructor scope violation: "
                      ^ TypedSyntax.PrettyPrint.print_TyName tyname
                    )
              ; Syntax.OverloadKeyMap.app goExp map
              ; tynameset
              )
          | goDec (T.EqualityDec (span, typarams, tyname, exp)) =
              ( if T.TyNameSet.member (tynameset, tyname) then
                  ()
                else
                  emitError
                    ( ctx
                    , [span]
                    , "type constructor scope violation: "
                      ^ TypedSyntax.PrettyPrint.print_TyName tyname
                    )
              ; #goExp
                  (checkTyScope
                     (ctx, T.TyVarSet.addList (tvset, typarams), tynameset)) exp
              ; tynameset
              )
          | goDec
              (T.ValDescDec
                 { sourceSpan = _
                 , expected = T.TypeScheme (tyvars, ty)
                 , actual = T.TypeScheme (tyvars', ty')
                 , origin = _
                 }) =
              ( #goTy
                  (checkTyScope
                     ( ctx
                     , T.TyVarSet.addList (tvset, List.map #1 tyvars)
                     , tynameset
                     )) ty
              ; #goTy
                  (checkTyScope
                     ( ctx
                     , T.TyVarSet.addList (tvset, List.map #1 tyvars')
                     , tynameset
                     )) ty'
              ; tynameset
              )
          | goDec
              (T.ESImportDec {sourceSpan = _, pure = _, specs, moduleName = _}) =
              (List.app (fn (_, _, ty) => goTy ty) specs; tynameset)
        and goDecs decs =
          List.foldl
            (fn (dec, tynameset) =>
               let val {goDec, ...} = checkTyScope (ctx, tvset, tynameset)
               in goDec dec
               end) tynameset decs
        and goValBind (T.TupleBind (_, binds, exp)) =
              (List.app (fn (_, ty) => goTy ty) binds; goExp exp)
          | goValBind (T.PolyVarBind (_, _, T.TypeScheme (typarams, ty), exp)) =
              let
                val {goTy, goExp, ...} = checkTyScope
                  ( ctx
                  , T.TyVarSet.addList (tvset, List.map #1 typarams)
                  , tynameset
                  )
              in
                goTy ty;
                goExp exp
              end
        fun goStrExp (T.StructExp _) = tynameset
          | goStrExp (T.StrIdExp _) = tynameset
          | goStrExp
              (T.PackedStrExp
                 {sourceSpan = _, strExp, payloadTypes = _, packageSig}) =
              let
                val _ = goStrExp strExp : T.TyNameSet.set
              in
                List.foldl
                  (fn ({tyname, ...}, set) => T.TyNameSet.add (set, tyname))
                  tynameset (#bound packageSig)
              end
          | goStrExp
              (T.FunctorAppExp
                 { sourceSpan = _
                 , funId = _
                 , argumentTypes = _
                 , argumentStr
                 , packageSig
                 }) =
              let
                val tynameset = goStrExp argumentStr
              (* TODO: Check argumentTypes *)
              in
                List.foldl
                  (fn ({tyname, ...}, set) => T.TyNameSet.add (set, tyname))
                  tynameset (#bound packageSig)
              end
          | goStrExp (T.LetInStrExp (_, strdecs, strexp)) =
              let
                val tynameset = goStrDecs strdecs
                val {goStrExp, ...} = checkTyScope (ctx, tvset, tynameset)
              in
                goStrExp strexp
              end
        and goStrDec (T.CoreDec (_, dec)) = goDec dec
          | goStrDec (T.StrBindDec (_, _, strexp, {s = _, bound})) =
              List.foldl
                (fn ({tyname, ...}, set) => T.TyNameSet.add (set, tyname))
                (goStrExp strexp) bound
        and goStrDecs decs =
          List.foldl
            (fn (dec, tynameset) =>
               let val {goStrDec, ...} = checkTyScope (ctx, tvset, tynameset)
               in goStrDec dec
               end) tynameset decs
        fun goFunExp (tynames, _, _, strexp) =
          let
            val tynameset' =
              List.foldl
                (fn ({tyname, ...}, set) => T.TyNameSet.add (set, tyname))
                tynameset tynames
            val {goStrExp, ...} = checkTyScope (ctx, tvset, tynameset')
            val _ = goStrExp strexp : T.TyNameSet.set
          in
            tynameset
          end
        fun goTopDec (T.StrDec dec) = goStrDec dec
          | goTopDec (T.FunDec (_, funexp)) = goFunExp funexp
        fun goTopDecs decs =
          List.foldl
            (fn (dec, tynameset) =>
               let val {goTopDec, ...} = checkTyScope (ctx, tvset, tynameset)
               in goTopDec dec
               end) tynameset decs
      in
        { goTy = goTy
        , goPureTy = goPureTy
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
    fun checkTyScopeOfProgram
      (ctx, tynameset: T.TyNameSet.set, program: T.Program) =
      List.foldl
        (fn (topdec, tynameset) =>
           let
             val {goTopDecs, ...} =
               checkTyScope (ctx, T.TyVarSet.empty, tynameset)
           in
             goTopDecs topdec
           end) tynameset program

    val emptySignature: 'link TypedSyntax.BaseSignature =
      { valMap = Syntax.VIdMap.empty
      , tyConMap = Syntax.TyConMap.empty
      , strMap = Syntax.StrIdMap.empty
      }

    fun mergeSignature (s1: 'link T.BaseSignature, s2: 'link T.BaseSignature) :
      'link T.BaseSignature =
      { valMap = Syntax.VIdMap.unionWithSecond (#valMap s1, #valMap s2)
      , tyConMap = Syntax.TyConMap.unionWithSecond (#tyConMap s1, #tyConMap s2)
      , strMap = Syntax.StrIdMap.unionWithSecond (#strMap s1, #strMap s2)
      }
    fun mergeQSignature (s1: T.QSignature, s2: T.QSignature) : T.QSignature =
      { s = mergeSignature (#s s1, #s s2)
      , bound = T.TyNameMap.unionWithSecond (#bound s1, #bound s2)
      }

    fun canonicalOrderForQSignature ({s, bound}: T.QSignature) : T.TyName list =
      let
        val bound' =
          T.TyNameMap.mapi
            (fn (tyname, {arity = _, admitsEquality = _, longtycon}) =>
               Option.getOpt (canonicalPathForTyName (s, tyname), longtycon))
            bound
        fun insert ([], key, value) = [(key, value)]
          | insert (xs0 as ((k, v) :: xs), key, value) =
              case Syntax.LongTyCon.compare (k, key) of
                LESS => (k, v) :: insert (xs, key, value)
              | GREATER => (key, value) :: xs0
              | EQUAL => (key, value) :: xs (* cannot happen *)
      in
        List.map #2
          (T.TyNameMap.foldli
             (fn (tyname, longtycon, acc) => insert (acc, longtycon, tyname)) []
             bound')
      end
    and canonicalPathForTyName
      ({valMap = _, tyConMap, strMap}: T.WrittenSignature, tyname: T.TyName) :
      Syntax.LongTyCon option =
      let
        val t =
          Syntax.TyConMap.filter
            (fn {typeFunction = T.TypeFunction (tyvars, ty), valEnv = _} =>
               case ty of
                 T.TyCon (_, tyargs, tyname') =>
                   T.eqTyName (tyname, tyname')
                   andalso
                   ListPair.allEq
                     (fn (tv, T.TyVar (_, tv')) => T.eqTyVar (tv, tv')
                       | _ => false) (tyvars, tyargs)
               | _ => false) tyConMap
      in
        case Option.map #1 (Syntax.TyConMap.firsti t) of
          SOME tycon => SOME (Syntax.MkQualified ([], tycon))
        | NONE =>
            let
              val t =
                Syntax.StrIdMap.mapPartiali
                  (fn (strid, T.MkSignature s) =>
                     Option.map
                       (fn Syntax.MkQualified (strids, tycon) =>
                          Syntax.MkQualified (strid :: strids, tycon))
                       (canonicalPathForTyName (s, tyname))) strMap
            in
              Syntax.StrIdMap.first t
            end
      end

    fun addSignatureToEnv
      ( env: SigEnv
      , s: T.WrittenSignature
      , tyNameMap: TyNameAttr T.TyNameMap.map
      ) : SigEnv =
      { valMap = #valMap env (* not used *)
      , tyConMap = Syntax.TyConMap.unionWithSecond (#tyConMap env, #tyConMap s)
      , tyNameMap =
          T.TyNameMap.unionWithSecond
            (#tyNameMap env, tyNameMap) (* should not overlap *)
      , strMap = Syntax.StrIdMap.unionWithSecond
          ( #strMap env
          , Syntax.StrIdMap.map
              (fn T.MkSignature s => (T.thawWrittenSignature s, ())) (#strMap s)
          )
      , sigMap = #sigMap env
      , funMap = #funMap env
      , boundTyVars = #boundTyVars env
      }

    fun applySubstTyConInTy
      (ctx: Context, subst: T.TypeFunction T.TyNameMap.map) : T.Ty -> T.Ty =
      let
        fun goTy (ty as T.TyVar _) = ty
          | goTy (ty as T.AnonymousTyVar _) = ty
          | goTy (T.RecordType (span, fields)) =
              T.RecordType (span, Syntax.LabelMap.map goTy fields)
          | goTy (T.RecordExtType (span, fields, baseTy)) =
              T.RecordExtType
                (span, Syntax.LabelMap.map goTy fields, goTy baseTy)
          | goTy (T.TyCon (span, tyargs, tycon)) =
              (case T.TyNameMap.find (subst, tycon) of
                 NONE => T.TyCon (span, List.map goTy tyargs, tycon)
               | SOME (T.TypeFunction (tyvars, ty)) =>
                   let
                     val subst' =
                       (ListPair.foldlEq
                          (fn (tv, tyarg, m) =>
                             TypedSyntax.TyVarMap.insert (m, tv, goTy tyarg))
                          TypedSyntax.TyVarMap.empty (tyvars, tyargs))
                       handle ListPair.UnequalLengths =>
                         emitFatalError
                           ( ctx
                           , [span]
                           , "invalid type constructor substitution"
                           )
                   in
                     T.applySubstTy subst' (T.thawPureTy ty)
                   end)
          | goTy (T.FnType (span, ty1, ty2)) =
              T.FnType (span, goTy ty1, goTy ty2)
      in
        goTy
      end
    fun applySubstTyConInPureTy
      (ctx: Context, subst: T.TypeFunction T.TyNameMap.map) : T.PureTy
                                                              -> T.PureTy =
      let
        fun goTy (ty as T.TyVar _) = ty
          | goTy (T.AnonymousTyVar (_, x)) = Void.absurd x
          | goTy (T.RecordType (span, fields)) =
              T.RecordType (span, Syntax.LabelMap.map goTy fields)
          | goTy (T.RecordExtType (span, fields, baseTy)) =
              T.RecordExtType
                (span, Syntax.LabelMap.map goTy fields, goTy baseTy)
          | goTy (T.TyCon (span, tyargs, tycon)) =
              (case T.TyNameMap.find (subst, tycon) of
                 NONE => T.TyCon (span, List.map goTy tyargs, tycon)
               | SOME (T.TypeFunction (tyvars, ty)) =>
                   let
                     val subst' =
                       (ListPair.foldlEq
                          (fn (tv, tyarg, m) =>
                             TypedSyntax.TyVarMap.insert (m, tv, goTy tyarg))
                          TypedSyntax.TyVarMap.empty (tyvars, tyargs))
                       handle ListPair.UnequalLengths =>
                         emitFatalError
                           ( ctx
                           , [span]
                           , "invalid type constructor substitution"
                           )
                   in
                     T.applySubstPureTy subst' ty
                   end)
          | goTy (T.FnType (span, ty1, ty2)) =
              T.FnType (span, goTy ty1, goTy ty2)
      in
        goTy
      end
    fun applySubstTyConInSig
      (ctx: Context, subst: T.TypeFunction T.TyNameMap.map) : T.Signature
                                                              -> T.Signature =
      let
        val goTy: T.Ty -> T.Ty = applySubstTyConInTy (ctx, subst)
        val goPureTy: T.PureTy -> T.PureTy =
          applySubstTyConInPureTy (ctx, subst)
        fun goTypeScheme (T.TypeScheme (tvs, ty)) =
          T.TypeScheme (tvs, goTy ty)
        fun goPureTypeScheme (T.TypeScheme (tvs, ty)) =
          T.TypeScheme (tvs, goPureTy ty)
        fun goTypeFunction (T.TypeFunction (tvs, ty)) =
          T.TypeFunction (tvs, goPureTy ty)
        fun goSig {valMap, tyConMap, strMap} =
          { valMap =
              Syntax.VIdMap.map (fn (tysc, ids) => (goTypeScheme tysc, ids))
                valMap
          , tyConMap =
              Syntax.TyConMap.map
                (fn {typeFunction, valEnv} =>
                   { typeFunction = goTypeFunction typeFunction
                   , valEnv =
                       Syntax.VIdMap.map
                         (fn (tysc, ids) => (goPureTypeScheme tysc, ids)) valEnv
                   }) tyConMap
          , strMap =
              Syntax.StrIdMap.map
                (fn T.MkSignature s => T.MkSignature (goSig s)) strMap
          }
      in
        goSig
      end
    fun applySubstTyConInWrittenSig
      (ctx: Context, subst: T.TypeFunction T.TyNameMap.map) :
      T.WrittenSignature
      -> T.WrittenSignature =
      let
        val goPureTy: T.PureTy -> T.PureTy =
          applySubstTyConInPureTy (ctx, subst)
        fun goPureTypeScheme (T.TypeScheme (tvs, ty)) =
          T.TypeScheme (tvs, goPureTy ty)
        fun goTypeFunction (T.TypeFunction (tvs, ty)) =
          T.TypeFunction (tvs, goPureTy ty)
        fun goSig {valMap, tyConMap, strMap} =
          { valMap =
              Syntax.VIdMap.map (fn (tysc, ids) => (goPureTypeScheme tysc, ids))
                valMap
          , tyConMap =
              Syntax.TyConMap.map
                (fn {typeFunction, valEnv} =>
                   { typeFunction = goTypeFunction typeFunction
                   , valEnv =
                       Syntax.VIdMap.map
                         (fn (tysc, ids) => (goPureTypeScheme tysc, ids)) valEnv
                   }) tyConMap
          , strMap =
              Syntax.StrIdMap.map
                (fn T.MkSignature s => T.MkSignature (goSig s)) strMap
          }
      in
        goSig
      end

    fun refreshTyNameInTy (_: Context, subst: T.TyName T.TyNameMap.map) :
      'l T.BaseTy
      -> 'l T.BaseTy =
      let
        fun goTy (ty as T.TyVar _) = ty
          | goTy (ty as T.AnonymousTyVar _) = ty
          | goTy (T.RecordType (span, fields)) =
              T.RecordType (span, Syntax.LabelMap.map goTy fields)
          | goTy (T.RecordExtType (span, fields, baseTy)) =
              T.RecordExtType
                (span, Syntax.LabelMap.map goTy fields, goTy baseTy)
          | goTy (T.TyCon (span, tyargs, tycon)) =
              let
                val tyargs = List.map goTy tyargs
              in
                case T.TyNameMap.find (subst, tycon) of
                  NONE => T.TyCon (span, tyargs, tycon)
                | SOME tycon' => T.TyCon (span, tyargs, tycon')
              end
          | goTy (T.FnType (span, ty1, ty2)) =
              T.FnType (span, goTy ty1, goTy ty2)
      in
        goTy
      end
    fun refreshTyNameInSig (ctx: Context, subst: T.TyName T.TyNameMap.map) :
      'l T.BaseSignature
      -> 'l T.BaseSignature =
      let
        val goTy: 'l T.BaseTy -> 'l T.BaseTy = refreshTyNameInTy (ctx, subst)
        val goPureTy: T.PureTy -> T.PureTy = refreshTyNameInTy (ctx, subst)
        fun goTypeScheme (T.TypeScheme (tvs, ty)) =
          T.TypeScheme (tvs, goTy ty)
        fun goPureTypeScheme (T.TypeScheme (tvs, ty)) =
          T.TypeScheme (tvs, goPureTy ty)
        fun goTypeFunction (T.TypeFunction (tvs, ty)) =
          T.TypeFunction (tvs, goPureTy ty)
        fun goSig {valMap, tyConMap, strMap} =
          { valMap =
              Syntax.VIdMap.map (fn (tysc, ids) => (goTypeScheme tysc, ids))
                valMap
          , tyConMap =
              Syntax.TyConMap.map
                (fn {typeFunction, valEnv} =>
                   { typeFunction = goTypeFunction typeFunction
                   , valEnv =
                       Syntax.VIdMap.map
                         (fn (tysc, ids) => (goPureTypeScheme tysc, ids)) valEnv
                   }) tyConMap
          , strMap =
              Syntax.StrIdMap.map
                (fn T.MkSignature s => T.MkSignature (goSig s)) strMap
          }
      in
        goSig
      end
    fun refreshTyNameInPackedSig (ctx: Context, {s, bound}: T.PackedSignature) :
      T.PackedSignature =
      let
        val subst =
          List.foldl
            (fn ({tyname, ...}, acc) =>
               T.TyNameMap.insert (acc, tyname, renewTyName (ctx, tyname)))
            T.TyNameMap.empty bound
      in
        { s = refreshTyNameInSig (ctx, subst) s
        , bound =
            List.map
              (fn {tyname, arity, admitsEquality} =>
                 { tyname = T.TyNameMap.lookup (subst, tyname)
                 , arity = arity
                 , admitsEquality = admitsEquality
                 }) bound
        }
      end
    fun refreshTyNameInFunSig
      ( ctx: Context
      , {bound, paramSig, resultSig = {s, bound = boundE}}: T.FunSig
      ) : T.FunSig =
      let
        val substA =
          List.foldl
            (fn ({tyname, ...}, acc) =>
               T.TyNameMap.insert (acc, tyname, renewTyName (ctx, tyname)))
            T.TyNameMap.empty bound
        val substE =
          List.foldl
            (fn ({tyname, ...}, acc) =>
               T.TyNameMap.insert (acc, tyname, renewTyName (ctx, tyname)))
            T.TyNameMap.empty boundE
      in
        { bound =
            List.map
              (fn {tyname, arity, admitsEquality, longtycon} =>
                 { tyname = T.TyNameMap.lookup (substA, tyname)
                 , arity = arity
                 , admitsEquality = admitsEquality
                 , longtycon = longtycon
                 }) bound
        , paramSig = refreshTyNameInSig (ctx, substA) paramSig
        , resultSig =
            { s =
                refreshTyNameInSig
                  (ctx, T.TyNameMap.unionWithSecond (substA, substE)) s
            , bound =
                List.map
                  (fn {tyname, arity, admitsEquality} =>
                     { tyname = T.TyNameMap.lookup (substE, tyname)
                     , arity = arity
                     , admitsEquality = admitsEquality
                     }) boundE
            }
        }
      end

    fun checkEquality
      (ctx: Context, env: ('val, 'str) Env', tyvars: T.TyVarSet.set) : T.PureTy
                                                                       -> bool =
      let
        fun goTy (T.TyVar (_, tv)) =
              if T.TyVarSet.member (tyvars, tv) then true
              else T.tyVarAdmitsEquality tv
          | goTy (T.AnonymousTyVar (_, x)) = Void.absurd x
          | goTy (T.RecordType (_, fields)) = Syntax.LabelMap.all goTy fields
          | goTy (T.RecordExtType (_, fields, baseTy)) =
              Syntax.LabelMap.all goTy fields andalso goTy baseTy
          | goTy (T.TyCon (span, tyargs, tyname)) =
              isRefOrArray tyname
              orelse
              (let
                 val {admitsEquality, ...} =
                   lookupTyNameInEnv (ctx, env, span, tyname)
               in
                 admitsEquality andalso List.all goTy tyargs
               end)
          | goTy (T.FnType _) = false
      in
        goTy
      end

    fun lookupLongTyConInQSignature (ctx, span, s: T.QSignature, longtycon) :
      T.TypeStructure =
      let
        val S.MkQualified (strids, tycon as Syntax.MkTyCon name) = longtycon
        val {tyConMap, ...} = lookupStr (ctx, #s s, span, strids)
      in
        case Syntax.TyConMap.find (tyConMap, tycon) of
          SOME tystr => tystr
        | NONE =>
            emitFatalError
              (ctx, [span], "unknown type constructor '" ^ name ^ "'")
      end
    fun getTypeNameFromTypeStructure
          ( _ (* ctx *)
          , { typeFunction =
                T.TypeFunction (tyvars, T.TyCon (_, tyargs, tyname))
            , ...
            }: T.TypeStructure
          ) : (T.TyName * int) option =
          let
            val arity = List.length tyvars
          in
            if List.length tyargs = arity then
              if
                ListPair.allEq
                  (fn (tv, T.TyVar (_, tv')) => tv = tv' | _ => false)
                  (tyvars, tyargs)
              then SOME (tyname, arity)
              else NONE
            else
              NONE
          end
      | getTypeNameFromTypeStructure _ = NONE

    fun evalSignature (ctx: Context, env: SigEnv, S.BasicSigExp (_, specs)) :
      T.QSignature = evalSpecs (ctx, env, specs)
      | evalSignature
          (ctx, env, S.SigIdExp (span, sigid as Syntax.MkSigId name)) =
          (case Syntax.SigIdMap.find (#sigMap env, sigid) of
             SOME {s, bound} =>
               let
                 val subst =
                   T.TyNameMap.mapi (fn (tycon, _) => renewTyName (ctx, tycon))
                     bound
               in
                 { s = refreshTyNameInSig (ctx, subst) s
                 , bound =
                     T.TyNameMap.foldli
                       (fn (tycon, attr, map) =>
                          T.TyNameMap.insert
                            (map, T.TyNameMap.lookup (subst, tycon), attr))
                       T.TyNameMap.empty bound
                 }
               end
           | NONE =>
               emitFatalError
                 (ctx, [span], "unknown signature name '" ^ name ^ "'"))
      | evalSignature
          ( ctx
          , env
          , S.TypeRealisationExp (span, sigexp, tyvars, longtycon, ty, _)
          ) =
          let
            val s = evalSignature (ctx, env, sigexp)
            val tyvars = List.map (fn tv => (tv, genTyVar (ctx, tv))) tyvars
            val ty =
              let
                val env =
                  { valMap = #valMap env
                  , tyConMap = #tyConMap env
                  , tyNameMap = #tyNameMap env
                  , strMap = #strMap env
                  , sigMap = #sigMap env
                  , funMap = #funMap env
                  , boundTyVars =
                      List.foldl Syntax.TyVarMap.insert' (#boundTyVars env)
                        tyvars
                  }
              in
                evalPureTy (ctx, env, ty)
              end
            val tystr = lookupLongTyConInQSignature (ctx, span, s, longtycon)
          in
            case getTypeNameFromTypeStructure (ctx, tystr) of
              SOME (tyname, arity) =>
                if List.length tyvars = arity then
                  case T.TyNameMap.find (#bound s, tyname) of
                    SOME {admitsEquality, ...} =>
                      let
                        val () =
                          if
                            admitsEquality
                            andalso
                            not
                              (checkEquality
                                 ( ctx
                                 , env
                                 , List.foldl
                                     (fn ((_, tv), set) =>
                                        T.TyVarSet.add (set, tv))
                                     T.TyVarSet.empty tyvars
                                 ) ty)
                          then
                            emitError
                              ( ctx
                              , [span]
                              , "type realisation failed (equality)"
                              )
                          else
                            ()
                        val subst =
                          T.TyNameMap.singleton (tyname, T.TypeFunction
                            (List.map #2 tyvars, ty))
                      in
                        { s = applySubstTyConInWrittenSig (ctx, subst) (#s s)
                        , bound = #1 (T.TyNameMap.remove (#bound s, tyname))
                        }
                      end
                  | NONE =>
                      emitFatalError
                        (ctx, [span], "type realisation against a rigid type")
                else
                  emitFatalError
                    (ctx, [span], "type realisation against a rigid type")
            | NONE =>
                emitFatalError
                  (ctx, [span], "type realisation against a rigid type")
          end
    and evalSpecs (ctx: Context, env: SigEnv, specs) : T.QSignature =
      List.foldl
        (fn (spec, s) =>
           let
             val env' = addSignatureToEnv
               ( env
               , #s s
               , T.TyNameMap.map
                   (fn {arity, admitsEquality, longtycon = _} =>
                      { arity = arity
                      , admitsEquality = admitsEquality
                      , overloadClass = NONE
                      }) (#bound s)
               )
           in
             mergeQSignature (s, addSpec (ctx, env', spec))
           end) {s = emptySignature, bound = T.TyNameMap.empty} specs
    and addSpec (ctx: Context, env: SigEnv, S.ValDesc (_, descs)) : T.QSignature =
          { s =
              { valMap =
                  List.foldl
                    (fn ((vid, ty), valMap) =>
                       let
                         val tvs =
                           PostParsing.freeTyVarsInTy
                             (Syntax.TyVarSet.empty, ty)
                         val tvs =
                           Syntax.TyVarSet.foldr
                             (fn (tv, m) =>
                                Syntax.TyVarMap.insert
                                  (m, tv, genTyVar (ctx, tv)))
                             Syntax.TyVarMap.empty tvs
                         val env' =
                           { valMap = #valMap env
                           , tyConMap = #tyConMap env
                           , tyNameMap = #tyNameMap env
                           , strMap = #strMap env
                           , sigMap = #sigMap env
                           , funMap = #funMap env
                           , boundTyVars = tvs
                           }
                         val ty = evalPureTy (ctx, env', ty)
                       in
                         Syntax.VIdMap.insert
                           ( valMap
                           , vid
                           , ( T.TypeScheme
                                 ( Syntax.TyVarMap.foldr
                                     (fn (tv, xs) => (tv, NONE) :: xs) [] tvs
                                 , ty
                                 )
                             , Syntax.ValueVariable
                             )
                           )
                       end) Syntax.VIdMap.empty descs
              , tyConMap = Syntax.TyConMap.empty
              , strMap = Syntax.StrIdMap.empty
              }
          , bound = TypedSyntax.TyNameMap.empty
          }
      | addSpec (ctx, _, S.TypeDesc (span, descs)) =
          List.foldl
            (fn ((tyvars, tycon), s) =>
               let
                 val tyname = newTyName (ctx, tycon)
                 val tyvars = List.map (fn tv => genTyVar (ctx, tv)) tyvars
                 val tystr =
                   { typeFunction = T.TypeFunction (tyvars, T.TyCon
                       ( span
                       , List.map (fn tv => T.TyVar (span, tv)) tyvars
                       , tyname
                       ))
                   , valEnv = Syntax.VIdMap.empty
                   }
               in
                 { s =
                     { valMap = #valMap (#s s)
                     , tyConMap = Syntax.TyConMap.insert
                         (#tyConMap (#s s), tycon, tystr)
                     , strMap = #strMap (#s s)
                     }
                 , bound = TypedSyntax.TyNameMap.insert
                     ( #bound s
                     , tyname
                     , { arity = List.length tyvars
                       , admitsEquality = false
                       , longtycon = Syntax.MkQualified ([], tycon)
                       }
                     )
                 }
               end)
            { s =
                { valMap = Syntax.VIdMap.empty
                , tyConMap = Syntax.TyConMap.empty
                , strMap = Syntax.StrIdMap.empty
                }
            , bound = TypedSyntax.TyNameMap.empty
            } descs
      | addSpec (ctx, _, S.EqtypeDesc (span, descs)) =
          List.foldl
            (fn ((tyvars, tycon), s) =>
               let
                 val tyname = newTyName (ctx, tycon)
                 val tyvars = List.map (fn tv => genTyVar (ctx, tv)) tyvars
                 val tystr =
                   { typeFunction = T.TypeFunction (tyvars, T.TyCon
                       ( span
                       , List.map (fn tv => T.TyVar (span, tv)) tyvars
                       , tyname
                       ))
                   , valEnv = Syntax.VIdMap.empty
                   }
               in
                 { s =
                     { valMap = #valMap (#s s)
                     , tyConMap = Syntax.TyConMap.insert
                         (#tyConMap (#s s), tycon, tystr)
                     , strMap = #strMap (#s s)
                     }
                 , bound = TypedSyntax.TyNameMap.insert
                     ( #bound s
                     , tyname
                     , { arity = List.length tyvars
                       , admitsEquality = true
                       , longtycon = Syntax.MkQualified ([], tycon)
                       }
                     )
                 }
               end)
            { s =
                { valMap = Syntax.VIdMap.empty
                , tyConMap = Syntax.TyConMap.empty
                , strMap = Syntax.StrIdMap.empty
                }
            , bound = TypedSyntax.TyNameMap.empty
            } descs
      | addSpec
          ( ctx
          , env
          , S.DatDesc
              ( span
              , descs:
                  (S.TyVar list * S.TyCon * S.optional_bar * S.ConBind list) list
              , typbinds
              )
          ) =
          let
            val descs =
              let
                val goConBind = doWithtype (ctx, env, typbinds)
              in
                List.map
                  (fn (tyvars, tycon, _, conbinds) =>
                     (tyvars, tycon, List.map goConBind conbinds)) descs
              end
            val localTyConMap =
              List.foldl
                (fn ((tyvars, tycon, conbinds), map) =>
                   S.TyConMap.insert
                     ( map
                     , tycon
                     , ( tyvars
                       , List.mapPartial (fn S.ConBind (_, _, optTy) => optTy)
                           conbinds
                       )
                     )) S.TyConMap.empty descs
            val equalityMap: bool S.TyConMap.map =
              determineDatatypeEquality (ctx, env, localTyConMap)
            val (partialTyConMap, tyNameMap, descs) =
              List.foldl
                (fn ((tyvars, tycon, condescs), (tyConMap, tyNameMap, descs)) =>
                   let
                     val tyname = newTyName (ctx, tycon)
                     val tyvarPairs =
                       List.map (fn tv => (tv, genTyVar (ctx, tv))) tyvars
                     val tystr =
                       { typeFunction =
                           T.TypeFunction (List.map #2 tyvarPairs, T.TyCon
                             ( span
                             , List.map (fn (_, tv) => T.TyVar (span, tv))
                                 tyvarPairs
                             , tyname
                             ))
                       , valEnv = Syntax.VIdMap.empty (* filled later *)
                       }
                     val tyConMap =
                       Syntax.TyConMap.insert (tyConMap, tycon, tystr)
                     val tyNameMap = TypedSyntax.TyNameMap.insert
                       ( tyNameMap
                       , tyname
                       , { arity = List.length tyvars
                         , admitsEquality =
                             S.TyConMap.lookup (equalityMap, tycon)
                         , overloadClass = NONE
                         }
                       )
                   in
                     ( tyConMap
                     , tyNameMap
                     , (tycon, tyname, tyvarPairs, tystr, condescs) :: descs
                     )
                   end) (Syntax.TyConMap.empty, TypedSyntax.TyNameMap.empty, [])
                descs
            val env' =
              mergeWithTyConEnv
                (env, {tyConMap = partialTyConMap, tyNameMap = tyNameMap})
            val withtypeMap =
              List.foldl
                (fn (S.TypBind (_, tyvars, tycon, ty), tyConMap) =>
                   let
                     val tyvars =
                       List.map (fn tv => (tv, genTyVar (ctx, tv))) tyvars
                     val ty =
                       let
                         val env =
                           { valMap = #valMap env'
                           , tyConMap = #tyConMap env'
                           , tyNameMap = #tyNameMap env'
                           , strMap = #strMap env'
                           , sigMap = #sigMap env'
                           , funMap = #funMap env'
                           , boundTyVars =
                               List.foldl Syntax.TyVarMap.insert'
                                 (#boundTyVars env) tyvars
                           }
                       in
                         evalPureTy (ctx, env, ty)
                       end
                     val tystr =
                       { typeFunction = T.TypeFunction (List.map #2 tyvars, ty)
                       , valEnv = Syntax.VIdMap.empty
                       }
                   in
                     Syntax.TyConMap.insert (tyConMap, tycon, tystr)
                   end) Syntax.TyConMap.empty typbinds
          in
            List.foldl
              (fn ((tycon, tyname, tyvarPairs, tystr, condescs), s) =>
                 let
                   val {typeFunction as T.TypeFunction (tyvars, ty), ...} =
                     tystr
                   val env'' =
                     { valMap = #valMap env'
                     , tyConMap = #tyConMap env'
                     , tyNameMap = #tyNameMap env'
                     , strMap = #strMap env'
                     , sigMap = #sigMap env'
                     , funMap = #funMap env'
                     , boundTyVars =
                         List.foldl Syntax.TyVarMap.insert' (#boundTyVars env')
                           tyvarPairs
                     }
                   val allConstructors =
                     List.foldl
                       (fn (Syntax.ConBind (_, vid, _), set) =>
                          Syntax.VIdSet.add (set, vid)) Syntax.VIdSet.empty
                       condescs
                   val constructorsWithPayload =
                     List.foldl
                       (fn (Syntax.ConBind (_, vid, optTy), set) =>
                          if Option.isSome optTy then
                            Syntax.VIdSet.add (set, vid)
                          else
                            set) Syntax.VIdSet.empty condescs
                   val representation =
                     case condescs of
                       [S.ConBind (_, _, SOME _)] => Syntax.REP_ALIAS
                     | [S.ConBind (_, _, NONE)] => Syntax.REP_UNIT
                     | _ =>
                         if Syntax.VIdSet.isEmpty constructorsWithPayload then
                           Syntax.REP_ENUM
                         else
                           Syntax.REP_BOXED
                   val valEnv =
                     List.foldl
                       (fn (S.ConBind (span, vid, optTy), valEnv) =>
                          let
                            val tysc = T.TypeScheme
                              ( List.map (fn tv => (tv, NONE)) tyvars
                              , case optTy of
                                  NONE => ty
                                | SOME payloadTy =>
                                    T.FnType
                                      ( span
                                      , evalPureTy (ctx, env'', payloadTy)
                                      , ty
                                      )
                              )
                            val idstatus = Syntax.ValueConstructor
                              { tag = Syntax.getVIdName vid
                              , allConstructors = allConstructors
                              , constructorsWithPayload =
                                  constructorsWithPayload
                              , representation = representation
                              }
                          in
                            Syntax.VIdMap.insert (valEnv, vid, (tysc, idstatus))
                          end) Syntax.VIdMap.empty condescs
                   val tystr = {typeFunction = typeFunction, valEnv = valEnv}
                 in
                   { s =
                       { valMap = Syntax.VIdMap.unionWithSecond
                           (#valMap (#s s), valEnv)
                       , tyConMap = Syntax.TyConMap.insert
                           (#tyConMap (#s s), tycon, tystr)
                       , strMap = #strMap (#s s)
                       }
                   , bound = TypedSyntax.TyNameMap.insert
                       ( #bound s
                       , tyname
                       , { arity = List.length tyvars
                         , admitsEquality =
                             S.TyConMap.lookup (equalityMap, tycon)
                         , longtycon = Syntax.MkQualified ([], tycon)
                         }
                       )
                   }
                 end)
              { s =
                  { valMap = Syntax.VIdMap.empty
                  , tyConMap = withtypeMap
                  , strMap = Syntax.StrIdMap.empty
                  }
              , bound = TypedSyntax.TyNameMap.empty
              } descs
          end
      | addSpec (ctx, env, S.DatatypeRepSpec (span, tycon, longtycon)) =
          let
            val tystr = lookupTyConInEnv (ctx, env, span, longtycon)
          in
            { s =
                { valMap = #valEnv tystr
                , tyConMap = Syntax.TyConMap.singleton (tycon, tystr)
                , strMap = Syntax.StrIdMap.empty
                }
            , bound = TypedSyntax.TyNameMap.empty
            }
          end
      | addSpec (ctx, env, S.ExDesc (span, descs: (S.VId * S.Ty option) list)) =
          { s =
              { valMap =
                  List.foldl
                    (fn ((vid, optTy), valMap) =>
                       let
                         val ty =
                           case optTy of
                             NONE => PrimTypes.exn
                           | SOME ty =>
                               T.FnType
                                 ( span
                                 , evalPureTy (ctx, env, ty)
                                 , PrimTypes.exn
                                 )
                       in
                         Syntax.VIdMap.insert
                           ( valMap
                           , vid
                           , ( T.TypeScheme ([], ty)
                             , Syntax.ExceptionConstructor
                             )
                           )
                       end) Syntax.VIdMap.empty descs
              , tyConMap = Syntax.TyConMap.empty
              , strMap = Syntax.StrIdMap.empty
              }
          , bound = TypedSyntax.TyNameMap.empty
          }
      | addSpec (ctx, env, S.StrDesc (_, descs)) =
          let
            val strMap =
              List.foldl
                (fn ((strid, sigexp), m) =>
                   Syntax.StrIdMap.insert
                     (m, strid, evalSignature (ctx, env, sigexp)))
                Syntax.StrIdMap.empty descs
          in
            { s =
                { valMap = Syntax.VIdMap.empty
                , tyConMap = Syntax.TyConMap.empty
                , strMap =
                    Syntax.StrIdMap.map (fn {s, bound = _} => T.MkSignature s)
                      strMap
                }
            , bound =
                Syntax.StrIdMap.foldli
                  (fn (strid, {bound, ...}, map) =>
                     TypedSyntax.TyNameMap.unionWithSecond
                       ( map
                       , TypedSyntax.TyNameMap.map
                           (fn { arity
                               , admitsEquality
                               , longtycon = Syntax.MkQualified (strids, tycon)
                               } =>
                              { arity = arity
                              , admitsEquality = admitsEquality
                              , longtycon =
                                  Syntax.MkQualified (strid :: strids, tycon)
                              }) bound
                       )) TypedSyntax.TyNameMap.empty strMap
            }
          end
      | addSpec (ctx, env, S.Include (_, sigexp)) =
          evalSignature (ctx, env, sigexp)
      | addSpec (ctx, env, S.Sharing (span, specs, longtycon0 :: longtycons)) =
          let val s = evalSpecs (ctx, env, specs)
          in shareLongTyCons (ctx, span, s, longtycon0, longtycons)
          end
      | addSpec (ctx, _, S.Sharing (span, _, [])) =
          emitFatalError
            (ctx, [span], "sharing: empty longtycons (internal error)")
      | addSpec (ctx, env, S.SharingStructure (span, specs, longstrids)) =
          let
            val s = evalSpecs (ctx, env, specs)
            val strs =
              List.map
                (fn Syntax.MkQualified (strids, strid) =>
                   let
                     val strids = strids @ [strid]
                   in
                     ( strids
                     , collectLongTyCons
                         (ctx, [], lookupStr (ctx, #s s, span, strids))
                     )
                   end) longstrids
            fun doStructure (s, (longstrid0, longtycons0) :: strs) =
                  let
                    val s =
                      List.foldl
                        (fn ((longstrid1, longtycons1), s) =>
                           let
                             val longtycons =
                               Syntax.LongTyConSet.intersection
                                 (longtycons0, longtycons1)
                           in
                             Syntax.LongTyConSet.foldl
                               (fn (Syntax.MkQualified (strids', tycon), s) =>
                                  shareLongTyCons
                                    ( ctx
                                    , span
                                    , s
                                    , Syntax.MkQualified
                                        (longstrid0 @ strids', tycon)
                                    , [Syntax.MkQualified
                                         (longstrid1 @ strids', tycon)]
                                    )) s longtycons
                           end) s strs
                  in
                    doStructure (s, strs)
                  end
              | doStructure (s, []) = s
          in
            doStructure (s, strs)
          end
      | addSpec (ctx, env, S.TypeAliasDesc (_, descs)) =
          { s =
              { valMap = Syntax.VIdMap.empty
              , tyConMap =
                  List.foldl
                    (fn ((tyvars, tycon, ty), tyConMap) =>
                       let
                         val tyvars =
                           List.map (fn tv => (tv, genTyVar (ctx, tv))) tyvars
                         val ty =
                           let
                             val env =
                               { valMap = #valMap env
                               , tyConMap =
                                   #tyConMap
                                     env (* not accumulated (Successor ML) *)
                               , tyNameMap = #tyNameMap env
                               , strMap = #strMap env
                               , sigMap = #sigMap env
                               , funMap = #funMap env
                               , boundTyVars =
                                   List.foldl Syntax.TyVarMap.insert'
                                     (#boundTyVars env) tyvars
                               }
                           in
                             evalPureTy (ctx, env, ty)
                           end
                         val tystr =
                           { typeFunction = T.TypeFunction
                               (List.map #2 tyvars, ty)
                           , valEnv = Syntax.VIdMap.empty
                           }
                       in
                         Syntax.TyConMap.insert (tyConMap, tycon, tystr)
                       end) Syntax.TyConMap.empty descs
              , strMap = Syntax.StrIdMap.empty
              }
          , bound = TypedSyntax.TyNameMap.empty
          }
    and shareLongTyCons (ctx, span, s: T.QSignature, longtycon0, longtycons) :
      T.QSignature =
      let
        val tystr0 = lookupLongTyConInQSignature (ctx, span, s, longtycon0)
        val tystrs =
          List.map
            (fn longtycon =>
               lookupLongTyConInQSignature (ctx, span, s, longtycon)) longtycons
      in
        case getTypeNameFromTypeStructure (ctx, tystr0) of
          SOME (tyname0, arity0) =>
            (case T.TyNameMap.find (#bound s, tyname0) of
               SOME {arity, admitsEquality, ...} =>
                 if arity0 = arity then
                   let
                     val typeFn =
                       let
                         val tyvars = List.tabulate (arity, fn _ =>
                           genTyVar (ctx, Syntax.MkTyVar "a"))
                       in
                         T.TypeFunction (tyvars, T.TyCon
                           ( span
                           , List.map (fn tv => T.TyVar (span, tv)) tyvars
                           , tyname0
                           ))
                       end
                     val (subst, admitsEquality) =
                       List.foldl
                         (fn (tystr, (subst, admitsEquality)) =>
                            case getTypeNameFromTypeStructure (ctx, tystr) of
                              SOME (tyname, arity') =>
                                if arity' <> arity then
                                  emitFatalError
                                    (ctx, [span], "sharing: arity mismatch")
                                else
                                  (case T.TyNameMap.find (#bound s, tyname) of
                                     SOME
                                       { arity = arity'
                                       , admitsEquality = admitsEquality'
                                       , ...
                                       } =>
                                       if arity' <> arity then
                                         emitFatalError
                                           ( ctx
                                           , [span]
                                           , "sharing: arity mismatch"
                                           )
                                       else if T.eqTyName (tyname, tyname0) then
                                         ( subst
                                         , admitsEquality
                                         ) (* do nothing *)
                                       else
                                         ( T.TyNameMap.insert
                                             (subst, tyname, typeFn)
                                         , admitsEquality orelse admitsEquality'
                                         )
                                   | NONE =>
                                       emitFatalError
                                         ( ctx
                                         , [span]
                                         , "sharing: type alias is invalid"
                                         ))
                            | NONE =>
                                emitFatalError
                                  ( ctx
                                  , [span]
                                  , "sharing: type alias is invalid"
                                  )) (T.TyNameMap.empty, admitsEquality) tystrs
                   in
                     { s = applySubstTyConInWrittenSig (ctx, subst) (#s s)
                     , bound =
                         T.TyNameMap.mapPartiali
                           (fn ( tyname
                               , x as {arity, admitsEquality = _, longtycon}
                               ) =>
                              if T.TyNameMap.inDomain (subst, tyname) then
                                NONE
                              else if T.eqTyName (tyname, tyname0) then
                                SOME
                                  { arity = arity
                                  , admitsEquality = admitsEquality
                                  , longtycon = longtycon
                                  }
                              else
                                SOME x) (#bound s)
                     }
                   end
                 else
                   emitFatalError (ctx, [span], "sharing: invalid arity")
             | NONE =>
                 emitFatalError (ctx, [span], "sharing: type alias is invalid"))
        | NONE => emitFatalError (ctx, [span], "sharing: type alias is invalid")
      end
    and collectLongTyCons
      ( ctx
      , strids: Syntax.StrId list
      , {valMap = _, tyConMap, strMap}: T.WrittenSignature
      ) : Syntax.LongTyConSet.set =
      let
        val set =
          Syntax.TyConMap.foldli
            (fn (tycon, _, set) =>
               Syntax.LongTyConSet.add (set, Syntax.MkQualified (strids, tycon)))
            Syntax.LongTyConSet.empty tyConMap
      in
        Syntax.StrIdMap.foldli
          (fn (strid, T.MkSignature s, set) =>
             let val set' = collectLongTyCons (ctx, strids @ [strid], s)
             in Syntax.LongTyConSet.union (set, set')
             end) set strMap
      end

    fun sameType (T.TyVar (_, tv), T.TyVar (_, tv')) = tv = tv'
      | sameType (T.RecordType (_, fields), T.RecordType (_, fields')) =
          Syntax.LabelMap.numItems fields = Syntax.LabelMap.numItems fields'
          andalso
          Syntax.LabelMap.alli
            (fn (label, ty) =>
               case Syntax.LabelMap.find (fields', label) of
                 SOME ty' => sameType (ty, ty')
               | NONE => false) fields
      | sameType (T.TyCon (_, tyargs, tycon), T.TyCon (_, tyargs', tycon')) =
          T.eqTyName (tycon, tycon')
          andalso
          (ListPair.allEq sameType (tyargs, tyargs')
           handle ListPair.UnequalLengths => false)
      | sameType (T.FnType (_, ty1, ty2), T.FnType (_, ty1', ty2')) =
          sameType (ty1, ty1') andalso sameType (ty2, ty2')
      | sameType (_, _) = false

    fun sameTypeScheme
      (ctx, span, T.TypeScheme (tyvarsE, tyE), T.TypeScheme (tyvarsA, tyA)) =
      if List.length tyvarsE = List.length tyvarsA then
        let
          val tyvars =
            List.map (fn _ => genTyVar (ctx, Syntax.MkTyVar "?")) tyvarsE
          (* constraints are ignored *)
          val substE =
            ListPair.foldlEq
              (fn ((tv, _), tv', m) =>
                 T.TyVarMap.insert (m, tv, T.TyVar (span, tv')))
              T.TyVarMap.empty (tyvarsE, tyvars)
          val substA =
            ListPair.foldlEq
              (fn ((tv, _), tv', m) =>
                 T.TyVarMap.insert (m, tv, T.TyVar (span, tv')))
              T.TyVarMap.empty (tyvarsA, tyvars)
          val tyE = T.applySubstPureTy substE tyE
          val tyA = T.applySubstPureTy substA tyA
        in
          sameType (tyE, tyA)
        end
      else
        false

    fun matchQSignature
      ( ctx: Context
      , env: Env
      , span: SourcePos.span
      , expected: T.QSignature
      , strid: T.StrId
      , actual: T.Signature
      ) : T.Signature * T.StrExp =
      let
        val env' = env (* addSignatureToEnv(envToSigEnv env, actual) *)
        val instantiation =
          TypedSyntax.TyNameMap.map
            (fn { arity
                , admitsEquality
                , longtycon as Syntax.MkQualified (strids, tycon)
                } =>
               let
                 val {typeFunction as T.TypeFunction (tyvars, actualTy), ...} =
                   let
                     val s = lookupStr (ctx, actual, span, strids)
                   in
                     case Syntax.TyConMap.find (#tyConMap s, tycon) of
                       SOME tystr => tystr
                     | NONE =>
                         emitFatalError
                           ( ctx
                           , [span]
                           , "signature matching: type not found: "
                             ^ Syntax.print_LongTyCon longtycon
                           )
                   end
                 val () =
                   if List.length tyvars = arity then
                     () (* OK *)
                   else
                     emitError
                       ( ctx
                       , [span]
                       , "signature matching: arity mismatch ("
                         ^ Syntax.print_LongTyCon longtycon ^ ")"
                       )
                 val () =
                   if
                     admitsEquality
                     andalso
                     not
                       (checkEquality (ctx, env', T.TyVarSet.fromList tyvars)
                          actualTy)
                   then
                     emitError
                       ( ctx
                       , [span]
                       , "signature matching: equality mismatch ("
                         ^ Syntax.print_LongTyCon longtycon ^ ")"
                       )
                   else
                     ()
               in
                 typeFunction
               end) (#bound expected)
        val instantiated =
          applySubstTyConInWrittenSig (ctx, instantiation) (#s expected)
      in
        matchSignature
          (ctx, env, span, instantiated, T.MkLongStrId (strid, []), actual)
      end
    and matchSignature
      ( ctx
      , env
      , span
      , expected: T.WrittenSignature
      , longstrid: T.LongStrId
      , actual: T.Signature
      ) : T.Signature * T.StrExp =
      let
        val strMap: (T.Signature * T.StrExp) S.StrIdMap.map =
          Syntax.StrIdMap.mapi
            (fn (strid, T.MkSignature s) =>
               case Syntax.StrIdMap.find (#strMap actual, strid) of
                 SOME (T.MkSignature s') =>
                   let
                     val longstrid' =
                       case longstrid of
                         T.MkLongStrId (strid0, strids0) =>
                           T.MkLongStrId (strid0, strids0 @ [strid])
                   in
                     matchSignature (ctx, env, span, s, longstrid', s')
                   end
               | NONE =>
                   emitFatalError
                     ( ctx
                     , [span]
                     , "signature matching: structure not found ("
                       ^ Syntax.print_StrId strid ^ ")"
                     )) (#strMap expected)
        val tyConMap: T.TypeStructure S.TyConMap.map =
          Syntax.TyConMap.mapi
            (fn (tycon, expectedTyStr) =>
               case Syntax.TyConMap.find (#tyConMap actual, tycon) of
                 SOME actualTyStr =>
                   matchTyDesc (ctx, env, span, expectedTyStr, actualTyStr)
               | NONE =>
                   emitFatalError
                     ( ctx
                     , [span]
                     , "signature matching: "
                       ^ (case tycon of Syntax.MkTyCon name => name)
                       ^ " not found"
                     )) (#tyConMap expected)
        val
          valMap:
            (T.PureTypeScheme
             * T.Dec list
             * T.LongVId
             * Syntax.ValueConstructorInfo Syntax.IdStatus) S.VIdMap.map =
          Syntax.VIdMap.mapi
            (fn (vid, (tyscE, idsE)) =>
               case Syntax.VIdMap.find (#valMap actual, vid) of
                 SOME (tyscA, idsA) =>
                   let
                     val longvid =
                       case longstrid of
                         T.MkLongStrId (strid0, strids0) =>
                           T.MkLongVId (strid0, strids0, vid)
                     val innerContext =
                       { nextTyVar = #nextTyVar ctx
                       , nextVId = #nextVId ctx
                       , messageHandler = #messageHandler ctx
                       , matchContext = vid :: #matchContext ctx
                       , languageOptions = #languageOptions ctx
                       }
                     val (tysc, decs, longvid') = matchValDesc
                       (innerContext, env, span, tyscE, longvid, tyscA, idsA)
                     val () =
                       if
                         (case idsE of
                            Syntax.ExceptionConstructor => true
                          | _ => false)
                         andalso
                         (case idsA of
                            Syntax.ExceptionConstructor => false
                          | _ => true)
                       then
                         emitError
                           ( ctx
                           , [span]
                           , "signature matching: id status mismatch: "
                             ^ Syntax.getVIdName vid
                           )
                       else if
                         Syntax.isValueConstructor idsE
                         andalso not (Syntax.isValueConstructor idsA)
                       then
                         emitError
                           ( ctx
                           , [span]
                           , "signature matching: id status mismatch: "
                             ^ Syntax.getVIdName vid
                           )
                       else
                         ()
                   in
                     (tysc, decs, longvid', idsE)
                   end
               | NONE =>
                   emitFatalError
                     ( ctx
                     , [span]
                     , "signature matching: " ^ Syntax.getVIdName vid
                       ^ " not found"
                     )) (#valMap expected)
        val (decs, strMap) =
          Syntax.StrIdMap.foldli
            (fn (strid, (s, strexp), (decs, strMap)) =>
               let
                 val strid' = newStrId (ctx, strid)
                 val decs =
                   T.StrBindDec (span, strid', strexp, {s = s, bound = []})
                   :: decs
               in
                 ( decs
                 , Syntax.StrIdMap.insert
                     (strMap, strid, T.MkLongStrId (strid', []))
                 )
               end) ([], Syntax.StrIdMap.empty) strMap
        val (decs, valMap) =
          Syntax.VIdMap.foldli
            (fn (vid, (_, decs, longvid, ids), (decs', valMap)) =>
               let
                 val decs =
                   List.foldr (fn (dec, decs) => T.CoreDec (span, dec) :: decs)
                     decs' decs
               in
                 (decs, Syntax.VIdMap.insert (valMap, vid, (longvid, ids)))
               end) (decs, Syntax.VIdMap.empty) valMap
        val strexp = T.StructExp
          { sourceSpan = span
          , valMap = valMap
          , tyConMap = tyConMap
          , strMap = strMap
          }
      in
        ( T.thawWrittenSignature expected
        , if List.null decs then strexp else T.LetInStrExp (span, decs, strexp)
        )
      end
    and matchTyDesc
      (ctx, _, span, expected: T.TypeStructure, actual: T.TypeStructure) :
      T.TypeStructure =
      let
        val {typeFunction = T.TypeFunction (tyvarsE, tyE), valEnv = valEnvE} =
          expected
        val numE = Syntax.VIdMap.numItems valEnvE
        val {typeFunction = T.TypeFunction (tyvarsA, tyA), valEnv = valEnvA} =
          actual
      in
        if List.length tyvarsE = List.length tyvarsA then
          let
            val tyvars =
              List.map (fn _ => genTyVar (ctx, Syntax.MkTyVar "?")) tyvarsE
            val substE =
              ListPair.foldlEq
                (fn (tv, tv', m) =>
                   T.TyVarMap.insert (m, tv, T.TyVar (span, tv')))
                T.TyVarMap.empty (tyvarsE, tyvars)
            val substA =
              ListPair.foldlEq
                (fn (tv, tv', m) =>
                   T.TyVarMap.insert (m, tv, T.TyVar (span, tv')))
                T.TyVarMap.empty (tyvarsA, tyvars)
            val tyE = T.applySubstPureTy substE tyE
            val tyA = T.applySubstPureTy substA tyA
            fun checkConstructor (vid, (tyscE, _)) =
              case Syntax.VIdMap.find (valEnvA, vid) of
                SOME (tyscA, _) => sameTypeScheme (ctx, span, tyscE, tyscA)
              | NONE => false
          in
            if sameType (tyE, tyA) then
              if numE > 0 then
                if
                  Syntax.VIdMap.numItems valEnvA = numE
                  andalso Syntax.VIdMap.alli checkConstructor valEnvE
                then
                  actual
                else
                  ( emitError
                      ( ctx
                      , [span]
                      , "signature matching: value constructor mismatch"
                      )
                  ; expected
                  )
              else
                actual
            else
              ( emitError (ctx, [span], "signature matching: type mismatch")
              ; expected
              )
          end
        else
          ( emitError (ctx, [span], "signature matching: arity mismatch")
          ; expected
          )
      end
    and matchValDesc
      ( ctx
      , env
      , span
      , expected: T.PureTypeScheme
      , longvid: T.LongVId
      , actual: T.TypeScheme
      , ids: Syntax.ValueConstructorInfo Syntax.IdStatus
      ) : T.PureTypeScheme * T.Dec list * T.LongVId =
      let
        val T.TypeScheme (tyvarsE, tyE) = expected
        val ictx = {context = ctx, level = 0}
        val (tyA, tyargsA) = instantiate (ictx, span, actual)
        val () = checkSubsumption (ictx, env, span, tyA, T.thawPureTy tyE)
        val vid = newVIdWithName
          ( ctx
          , case longvid of
              T.MkShortVId (T.MkVId (name, _)) => name
            | T.MkLongVId (_, _, Syntax.MkVId name) =>
                Syntax.SourceName.fromString name
            | T.MkLongVId (_, _, Syntax.GeneratedVId (name, _)) => name
          )
        val tyargsA = List.map (fn (ty, c) => (T.forceTy ty, c)) tyargsA
        val trivial =
          ListPair.allEq
            (fn ((tvE, NONE), (T.TyVar (_, tvA), NONE)) => T.eqTyVar (tvE, tvA)
              | ((tvE, SOME T.IsEqType), (T.TyVar (_, tvA), SOME T.IsEqType)) =>
               T.eqTyVar (tvE, tvA)
              | _ => false) (tyvarsE, tyargsA)
      in
        if trivial then
          ( expected
          , []
          , longvid
          ) (* includes the case where ids = ExceptionConstructor *)
        else
          let
            val dec = T.ValDec
              ( span
              , [T.PolyVarBind
                   ( span
                   , vid
                   , T.thawPureTypeScheme expected
                   , T.VarExp (span, longvid, ids, tyargsA)
                   )]
              )
          in
            (expected, [dec], T.MkShortVId vid)
          end
      end

    fun typeCheckStrExp (ctx: Context, env: Env, S.StructExp (span, decs)) :
      T.PackedSignature * TyNameAttr T.TyNameMap.map * T.StrDec list * T.StrExp =
          let
            val ({valMap, tyConMap, tyNameMap, strMap, ...}, decs) =
              typeCheckStrDecs (ctx, env, decs)
            val s =
              { s =
                  { valMap =
                      Syntax.VIdMap.map (fn (tysc, ids, _) => (tysc, ids))
                        valMap
                  , tyConMap = tyConMap
                  , strMap =
                      Syntax.StrIdMap.map (fn (s, _) => T.MkSignature s) strMap
                  }
              , bound = []
              }
            val e = T.StructExp
              { sourceSpan = span
              , valMap =
                  Syntax.VIdMap.map (fn (_, ids, longvid) => (longvid, ids))
                    valMap
              , tyConMap = tyConMap
              , strMap =
                  Syntax.StrIdMap.map (fn (_, longstrid) => longstrid) strMap
              }
          in
            (s, tyNameMap, decs, e)
          end
      | typeCheckStrExp (ctx, env, S.StrIdExp (span, longstrid)) =
          (case longstrid of
             Syntax.MkQualified ([], strid) =>
               (case Syntax.StrIdMap.find (#strMap env, strid) of
                  SOME (s, longstrid) =>
                    ( {s = s, bound = []}
                    , TypedSyntax.TyNameMap.empty
                    , []
                    , T.StrIdExp (span, longstrid)
                    )
                | NONE => emitFatalError (ctx, [span], "structure not found"))
           | Syntax.MkQualified (strid0 :: strids, strid') =>
               (case Syntax.StrIdMap.find (#strMap env, strid0) of
                  SOME (s, T.MkLongStrId (strid0, strids0)) =>
                    let
                      val s' = lookupStr (ctx, s, span, strids @ [strid'])
                    in
                      ( {s = s', bound = []}
                      , TypedSyntax.TyNameMap.empty
                      , []
                      , T.StrIdExp (span, T.MkLongStrId
                          (strid0, strids0 @ strids @ [strid']))
                      )
                    end
                | NONE => emitFatalError (ctx, [span], "structure not found")))
      | typeCheckStrExp
          (ctx, env, S.TransparentConstraintExp (span, strexp, sigexp)) =
          let
            val (sA, tyNameMap, decs, strexp) =
              typeCheckStrExp (ctx, env, strexp)
            val sE = evalSignature (ctx, envToSigEnv env, sigexp)
            val strid = newStrId (ctx, Syntax.MkStrId "tmp")
            val env' =
              { valMap = #valMap env
              , tyConMap = #tyConMap env
              , tyNameMap =
                  TypedSyntax.TyNameMap.unionWithSecond
                    (#tyNameMap env, tyNameMap)
              , strMap = #strMap env
              , sigMap = #sigMap env
              , funMap = #funMap env
              , boundTyVars = #boundTyVars env
              }
            val (s, strexp') = matchQSignature
              (ctx, env', span, sE, strid, #s sA)
          in
            ( {s = s, bound = []}
            , tyNameMap
            , decs @ [T.StrBindDec (span, strid, strexp, sA)]
            , strexp'
            )
          end
      | typeCheckStrExp (ctx, env, S.OpaqueConstraintExp (span, strexp, sigexp)) =
          let
            val (sA, tyNameMap, decs, strexp) =
              typeCheckStrExp (ctx, env, strexp)
            val sE = evalSignature (ctx, envToSigEnv env, sigexp)
            val env' =
              { valMap = #valMap env
              , tyConMap = #tyConMap env
              , tyNameMap =
                  TypedSyntax.TyNameMap.unionWithSecond
                    (#tyNameMap env, tyNameMap)
              , strMap = #strMap env
              , sigMap = #sigMap env
              , funMap = #funMap env
              , boundTyVars = #boundTyVars env
              }
            val strid = newStrId (ctx, Syntax.MkStrId "tmp")
            val (_, strexp') = matchQSignature
              (ctx, env', span, sE, strid, #s sA)
            val tynames = canonicalOrderForQSignature sE
            val packageSig =
              { s = T.thawWrittenSignature (#s sE)
              , bound =
                  List.map
                    (fn tyname =>
                       let
                         val {arity, admitsEquality, longtycon = _} =
                           T.TyNameMap.lookup (#bound sE, tyname)
                       in
                         { tyname = tyname
                         , arity = arity
                         , admitsEquality = admitsEquality
                         }
                       end) tynames
              }
            val tyNameMapOutside =
              T.TyNameMap.foldli
                (fn (tyname, {arity, admitsEquality, longtycon = _}, acc) =>
                   T.TyNameMap.insert
                     ( acc
                     , tyname
                     , { arity = arity
                       , admitsEquality = admitsEquality
                       , overloadClass = NONE
                       }
                     )) T.TyNameMap.empty (#bound sE)
            val payloadTypes =
              List.map
                (fn tyname =>
                   let
                     val {longtycon = Syntax.MkQualified (strids, tycon), ...} =
                       T.TyNameMap.lookup (#bound sE, tyname)
                     val {tyConMap, ...} = lookupStr (ctx, #s sA, span, strids)
                   in
                     case Syntax.TyConMap.find (tyConMap, tycon) of
                       SOME {typeFunction, valEnv = _} => typeFunction
                     | NONE =>
                         emitFatalError
                           (ctx, [span], "unknown type constructor")
                   end) tynames
          in
            ( packageSig
            , tyNameMapOutside
            , []
            , T.PackedStrExp
                { sourceSpan = span
                , strExp = T.LetInStrExp
                    ( span
                    , decs @ [T.StrBindDec (span, strid, strexp, sA)]
                    , strexp'
                    )
                , payloadTypes = payloadTypes
                , packageSig = packageSig
                }
            )
          end
      | typeCheckStrExp (ctx, env, S.FunctorAppExp (span, funid, strexp)) =
          (case S.FunIdMap.find (#funMap env, funid) of
             NONE => emitFatalError (ctx, [span], "undefined functor")
           | SOME (funsig, funid) =>
               let
                 val {bound, paramSig, resultSig} =
                   refreshTyNameInFunSig (ctx, funsig)
                 val (sA, tyNameMap, decs, strexp) =
                   typeCheckStrExp (ctx, env, strexp)
                 val strid = newStrId (ctx, Syntax.MkStrId "tmp")
                 val env' =
                   { valMap = #valMap env
                   , tyConMap = #tyConMap env
                   , tyNameMap =
                       TypedSyntax.TyNameMap.unionWithSecond
                         (#tyNameMap env, tyNameMap)
                   , strMap = #strMap env
                   , sigMap = #sigMap env
                   , funMap = #funMap env
                   , boundTyVars = #boundTyVars env
                   }
                 val argumentTypes =
                   List.map
                     (fn { tyname
                         , arity
                         , admitsEquality
                         , longtycon as Syntax.MkQualified (strids, tycon)
                         } =>
                        let
                          val
                            { typeFunction as T.TypeFunction (tyvars, actualTy)
                            , ...
                            } =
                            let
                              val s = lookupStr (ctx, #s sA, span, strids)
                            in
                              case Syntax.TyConMap.find (#tyConMap s, tycon) of
                                SOME tystr => tystr
                              | NONE =>
                                  emitFatalError
                                    ( ctx
                                    , [span]
                                    , "signature matching: type not found: "
                                      ^ Syntax.print_LongTyCon longtycon
                                    )
                            end
                          val () =
                            if List.length tyvars = arity then
                              () (* OK *)
                            else
                              emitError
                                ( ctx
                                , [span]
                                , "signature matching: arity mismatch ("
                                  ^ Syntax.print_LongTyCon longtycon ^ ")"
                                )
                          val () =
                            if
                              admitsEquality
                              andalso
                              not
                                (checkEquality
                                   (ctx, env', T.TyVarSet.fromList tyvars)
                                   actualTy)
                            then
                              emitError
                                ( ctx
                                , [span]
                                , "signature matching: equality mismatch ("
                                  ^ Syntax.print_LongTyCon longtycon ^ ")"
                                )
                            else
                              () (* OK *)
                        in
                          (tyname, typeFunction, admitsEquality)
                        end) bound
                 val instantiation =
                   List.foldl
                     (fn ((tyname, typeFunction, _), map) =>
                        T.TyNameMap.insert (map, tyname, typeFunction))
                     T.TyNameMap.empty argumentTypes
                 val instantiated =
                   applySubstTyConInWrittenSig (ctx, instantiation) paramSig
                 val (_, strexp') = matchSignature
                   ( ctx
                   , env'
                   , span
                   , instantiated
                   , T.MkLongStrId (strid, [])
                   , #s sA
                   )
                 val resultSig =
                   { s =
                       applySubstTyConInSig (ctx, instantiation) (#s resultSig)
                   , bound = #bound resultSig
                   }
                 val tyNameMapOutside =
                   List.foldl
                     (fn ({tyname, arity, admitsEquality}, map) =>
                        T.TyNameMap.insert
                          ( map
                          , tyname
                          , { arity = arity
                            , admitsEquality = admitsEquality
                            , overloadClass = NONE
                            }
                          )) tyNameMap (#bound resultSig)
               in
                 ( resultSig
                 , tyNameMapOutside
                 , decs @ [T.StrBindDec (span, strid, strexp, sA)]
                 , T.FunctorAppExp
                     { sourceSpan = span
                     , funId = funid
                     , argumentTypes =
                         List.map
                           (fn (_, typeFunction, admitsEquality) =>
                              { typeFunction = typeFunction
                              , admitsEquality = admitsEquality
                              }) argumentTypes
                     , argumentStr = strexp'
                     , packageSig = resultSig
                     }
                 )
               end)
      | typeCheckStrExp (ctx, env, S.LetInStrExp (_, strdecs, strexp)) =
          let
            val (env', strdecs) = typeCheckStrDecs (ctx, env, strdecs)
            val (s, tyNameMap, strdecs', strexp) = typeCheckStrExp
              (ctx, mergeEnv (env, env'), strexp)
          in
            ( s
            , T.TyNameMap.unionWithSecond (#tyNameMap env', tyNameMap)
            , strdecs @ strdecs'
            , strexp
            )
          end
    and typeCheckStrDec (ctx: Context, env: Env, S.CoreDec (span, dec)) :
      Env * TypedSyntax.StrDec list =
          let val (env', decs) = typeCheckCoreDecs (ctx, env, [dec])
          in (env', List.map (fn dec => T.CoreDec (span, dec)) decs)
          end
      | typeCheckStrDec (ctx, env, S.StrBindDec (span, binds)) =
          let
            val (strMap, tyNameMap, binds) =
              List.foldr
                (fn ((strid, strexp), (strMap, tyNameMap, binds)) =>
                   let
                     val (packedSignature, tc, strdecs, strexp) =
                       typeCheckStrExp (ctx, env, strexp)
                     val strid' = newStrId (ctx, strid)
                   in
                     ( S.StrIdMap.insert
                         ( strMap
                         , strid
                         , (#s packedSignature, T.MkLongStrId (strid', []))
                         )
                     , TypedSyntax.TyNameMap.unionWithSecond (tyNameMap, tc)
                     , (strid', strdecs, strexp, packedSignature) :: binds
                     )
                   end) (Syntax.StrIdMap.empty, TypedSyntax.TyNameMap.empty, [])
                binds
            val env' =
              { valMap = Syntax.VIdMap.empty
              , tyConMap = Syntax.TyConMap.empty
              , tyNameMap = tyNameMap
              , strMap = strMap
              , sigMap = Syntax.SigIdMap.empty
              , funMap = Syntax.FunIdMap.empty
              , boundTyVars = Syntax.TyVarMap.empty
              }
          in
            ( env'
            , List.foldr
                (fn ((strid, strdecs, strexp, packedSignature), strdecs') =>
                   strdecs
                   @
                   T.StrBindDec (span, strid, strexp, packedSignature)
                   :: strdecs') [] binds
            )
          end
      | typeCheckStrDec (ctx, env, S.LocalStrDec (_, decs1, decs2)) =
          let
            val (env', decs1) = typeCheckStrDecs (ctx, env, decs1)
            val (env'', decs2) = typeCheckStrDecs
              (ctx, mergeEnv (env, env'), decs2)
            val env'' =
              { valMap = #valMap env''
              , tyConMap = #tyConMap env''
              , tyNameMap =
                  TypedSyntax.TyNameMap.unionWithSecond
                    (#tyNameMap env', #tyNameMap env'')
              , strMap = #strMap env''
              , sigMap = #sigMap env''
              , funMap = #funMap env''
              , boundTyVars = #boundTyVars env''
              }
          in
            (env'', decs1 @ decs2)
          end
    and typeCheckStrDecs (_: Context, _: Env, []) = (emptyEnv, [])
      | typeCheckStrDecs (ctx, env, dec :: decs) =
          let
            val (env', dec) = typeCheckStrDec (ctx, env, dec)
            val (env'', decs) = typeCheckStrDecs
              (ctx, mergeEnv (env, env'), decs)
          in
            (mergeEnv (env', env''), dec @ decs)
          end

    fun typeCheckFunExp' (ctx, span, paramEnv, paramSig, strid, strexp) :
      TypedSyntax.FunSig * TypedSyntax.FunExp =
      let
        val (actualSignature: T.PackedSignature, bodyTyNameMap, strdecs, strexp) =
          typeCheckStrExp (ctx, paramEnv, strexp)
        val tynamesInParam = canonicalOrderForQSignature paramSig
        val stridTmp = newStrId (ctx, Syntax.MkStrId "tmp")
        val additionalTyNames =
          T.TyNameMap.foldli
            (fn (tyname, {arity, admitsEquality, overloadClass = _}, xs) =>
               if
                 List.exists
                   (fn {tyname = tyname', ...} => T.eqTyName (tyname, tyname'))
                   (#bound actualSignature)
               then
                 xs
               else
                 { tyname = tyname
                 , arity = arity
                 , admitsEquality = admitsEquality
                 } :: xs) [] bodyTyNameMap
        val actualSignature' =
          { s = #s actualSignature
          , bound = #bound actualSignature @ additionalTyNames
          }
        val resultSig = refreshTyNameInPackedSig (ctx, actualSignature')
        val payloadTypes =
          List.map
            (fn {tyname, arity, admitsEquality = _} =>
               let
                 val tyvars = List.tabulate (arity, fn _ =>
                   genTyVar (ctx, Syntax.MkTyVar "?"))
               in
                 T.TypeFunction (tyvars, T.TyCon
                   (span, List.map (fn tv => T.TyVar (span, tv)) tyvars, tyname))
               end) (#bound actualSignature')
        val funsig =
          { bound =
              List.map
                (fn tyname =>
                   let
                     val {arity, admitsEquality, longtycon} =
                       T.TyNameMap.lookup (#bound paramSig, tyname)
                   in
                     { tyname = tyname
                     , arity = arity
                     , admitsEquality = admitsEquality
                     , longtycon = longtycon
                     }
                   end) tynamesInParam
          , paramSig = #s paramSig
          , resultSig = resultSig
          }
        val funexp =
          ( List.map
              (fn tyname =>
                 let
                   val {arity, admitsEquality, longtycon = _} =
                     T.TyNameMap.lookup (#bound paramSig, tyname)
                 in
                   { tyname = tyname
                   , arity = arity
                   , admitsEquality = admitsEquality
                   }
                 end) tynamesInParam
          , strid
          , #s paramSig
          , T.LetInStrExp
              ( span
              , strdecs
                @ [T.StrBindDec (span, stridTmp, strexp, actualSignature)]
              , T.PackedStrExp
                  { sourceSpan = span
                  , strExp = T.StrIdExp (span, T.MkLongStrId (stridTmp, []))
                  , payloadTypes = payloadTypes
                  , packageSig = resultSig
                  }
              )
          )
      in
        (funsig, funexp)
      end
    fun typeCheckFunExp (ctx, span, env, S.NamedFunExp (strid, sigexp, strexp)) :
      TypedSyntax.FunSig * TypedSyntax.FunExp =
          let
            val strid' = newStrId (ctx, strid)
            val paramSig: T.QSignature =
              evalSignature (ctx, envToSigEnv env, sigexp)
            val tyNameMap: TyNameAttr T.TyNameMap.map =
              T.TyNameMap.map
                (fn {arity, admitsEquality, longtycon = _} =>
                   { arity = arity
                   , admitsEquality = admitsEquality
                   , overloadClass = NONE
                   }) (#bound paramSig)
            val paramEnv =
              { valMap = #valMap env
              , tyConMap = #tyConMap env
              , tyNameMap =
                  T.TyNameMap.unionWithSecond (#tyNameMap env, tyNameMap)
              , strMap = S.StrIdMap.insert
                  ( #strMap env
                  , strid
                  , ( T.thawWrittenSignature (#s paramSig)
                    , T.MkLongStrId (strid', [])
                    )
                  )
              , sigMap = #sigMap env
              , funMap = #funMap env
              , boundTyVars = #boundTyVars env
              }
          in
            typeCheckFunExp' (ctx, span, paramEnv, paramSig, strid', strexp)
          end
      | typeCheckFunExp (ctx, span, env, S.AnonymousFunExp (sigexp, strexp)) =
          let
            val strid0 = newStrId (ctx, S.MkStrId "param")
            val paramSig: T.QSignature =
              evalSignature (ctx, envToSigEnv env, sigexp)
            val tyNameMap: TyNameAttr T.TyNameMap.map =
              T.TyNameMap.map
                (fn {arity, admitsEquality, longtycon = _} =>
                   { arity = arity
                   , admitsEquality = admitsEquality
                   , overloadClass = NONE
                   }) (#bound paramSig)
            val paramSig' = T.thawWrittenSignature (#s paramSig)
            val paramEnv =
              { valMap =
                  Syntax.VIdMap.mapi
                    (fn (vid, (tysc, ids)) =>
                       (tysc, ids, TypedSyntax.MkLongVId (strid0, [], vid)))
                    (#valMap paramSig')
              , tyConMap = #tyConMap paramSig'
              , tyNameMap = tyNameMap
              , strMap =
                  Syntax.StrIdMap.mapi
                    (fn (strid, T.MkSignature s) =>
                       (s, T.MkLongStrId (strid0, [strid]))) (#strMap paramSig')
              , sigMap = Syntax.SigIdMap.empty
              , funMap = Syntax.FunIdMap.empty
              , boundTyVars = Syntax.TyVarMap.empty
              }
          in
            typeCheckFunExp'
              (ctx, span, mergeEnv (env, paramEnv), paramSig, strid0, strexp)
          end

    fun typeCheckTopDec (ctx, env, S.StrDec strdec) =
          let val (env', strdec) = typeCheckStrDec (ctx, env, strdec)
          in (env', List.map T.StrDec strdec)
          end
      | typeCheckTopDec (ctx, env, S.SigDec binds) =
          let
            val sigenv = envToSigEnv env
            val sigMap =
              List.foldl
                (fn ((sigid, sigexp), m) =>
                   Syntax.SigIdMap.insert
                     (m, sigid, evalSignature (ctx, sigenv, sigexp)))
                (#sigMap env) binds
            val env =
              { valMap = Syntax.VIdMap.empty
              , tyConMap = Syntax.TyConMap.empty
              , tyNameMap = TypedSyntax.TyNameMap.empty
              , strMap = Syntax.StrIdMap.empty
              , sigMap = sigMap
              , funMap = Syntax.FunIdMap.empty
              , boundTyVars = Syntax.TyVarMap.empty
              }
          in
            (env, [])
          end
      | typeCheckTopDec (ctx, env, S.FunDec binds) =
          let
            val (funMap, binds) =
              List.foldr
                (fn ((span, funid, funexp), (funMap, binds)) =>
                   let
                     val funid' = newFunId (ctx, funid)
                     val (funsig, funexp) =
                       typeCheckFunExp (ctx, span, env, funexp)
                   in
                     ( Syntax.FunIdMap.insert (funMap, funid, (funsig, funid'))
                     , T.FunDec (funid', funexp) :: binds
                     )
                   end) (Syntax.FunIdMap.empty, []) binds
            val env =
              { valMap = Syntax.VIdMap.empty
              , tyConMap = Syntax.TyConMap.empty
              , tyNameMap = TypedSyntax.TyNameMap.empty
              , strMap = Syntax.StrIdMap.empty
              , sigMap = Syntax.SigIdMap.empty
              , funMap = funMap
              , boundTyVars = Syntax.TyVarMap.empty
              }
          in
            (env, binds)
          end

    fun typeCheckTopDecs (_, _, []) = (emptyEnv, [])
      | typeCheckTopDecs (ctx, env, dec :: decs) =
          let
            val (env', dec) = typeCheckTopDec (ctx, env, dec)
            val (env'', decs) = typeCheckTopDecs
              (ctx, mergeEnv (env, env'), decs)
          in
            (mergeEnv (env', env''), dec @ decs)
          end

    (*: val typeCheckProgram : Context * Env * ((Syntax.Dec Syntax.TopDec) list) list -> Env * (TypedSyntax.TopDec list) list *)
    fun typeCheckProgram (_, _, []: ((Syntax.Dec Syntax.TopDec) list) list) :
      Env * (TypedSyntax.TopDec list) list = (emptyEnv, [])
      | typeCheckProgram (ctx, env, topdec :: topdecs) =
          let
            val (env', topdec') = typeCheckTopDecs (ctx, env, topdec)
            val (env'', topdecs') = typeCheckProgram
              (ctx, mergeEnv (env, env'), topdecs)
          in
            (mergeEnv (env', env''), topdec' :: topdecs')
          end
  end (* local *)
end (* structure Typing *)
