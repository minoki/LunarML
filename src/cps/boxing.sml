(*
 * Copyright (c) 2026 ARATA Mizuki
 * This file is part of LunarML.
 *)
(* This module inserts BoxOp/UnboxOp at type boundaries for WasmGC.
 * After this pass, function params, return values, and continuation params
 * that had unboxed types are converted to BoxedType with explicit
 * Box/Unbox operations at boundaries.
 *
 * NOTE: The CPS type checker does not fully support the mixed BoxedType
 * representation, so internal consistency checks should be disabled
 * after this pass. *)
structure CpsBoxing:
sig
  val transform: CpsSimplify.Context * CSyntax.Stat -> CSyntax.Stat
end =
struct
  local
    structure F = FSyntax
    structure C = CSyntax
    structure TyVarMap = TypedSyntax.TyVarMap
    structure VIdMap = TypedSyntax.VIdMap
  in
    type Context = CpsSimplify.Context
    type env = {tyMap: F.Ty TyVarMap.map, varTys: F.Ty VIdMap.map}
    fun tyToUnboxedTy (F.TyVar tv) =
          if TypedSyntax.eqTyVar (tv, PrimTypes.Names.int32) then
            SOME F.UBTyInt32
          else if TypedSyntax.eqTyVar (tv, PrimTypes.Names.int64) then
            SOME F.UBTyInt64
          else if TypedSyntax.eqTyVar (tv, PrimTypes.Names.word32) then
            SOME F.UBTyWord32
          else if TypedSyntax.eqTyVar (tv, PrimTypes.Names.word64) then
            SOME F.UBTyWord64
          else if TypedSyntax.eqTyVar (tv, PrimTypes.Names.real) then
            SOME F.UBTyReal
          else if TypedSyntax.eqTyVar (tv, PrimTypes.Names.char) then
            SOME F.UBTyChar
          else if TypedSyntax.eqTyVar (tv, PrimTypes.Names.char16) then
            SOME F.UBTyChar16
          else if TypedSyntax.eqTyVar (tv, PrimTypes.Names.char32) then
            SOME F.UBTyChar32
          else if TypedSyntax.eqTyVar (tv, PrimTypes.Names.bool) then
            SOME F.UBTyBool
          else if TypedSyntax.eqTyVar (tv, PrimTypes.Names.wasm_ptr) then
            SOME F.UBTyWasmPtr
          else
            NONE
      | tyToUnboxedTy _ = NONE
    (* goTy: transform a type using the type environment.
       Unboxed type variables are preserved; others become BoxedType. *)
    fun goTy (tyMap: F.Ty TyVarMap.map) (ty as F.TyVar tv) =
          (case TyVarMap.find (tyMap, tv) of
             SOME replacement => replacement
           | NONE => ty (* should not happen after erase-poly *))
      | goTy tyMap (F.RecordType fields) =
          F.RecordType (Syntax.LabelMap.map (goTy tyMap) fields)
      | goTy tyMap (F.AppType {applied, arg}) =
          F.AppType {applied = goTy tyMap applied, arg = goTy tyMap arg}
      | goTy tyMap (F.MultiFnType (params, result)) =
          F.MultiFnType (List.map (goTy tyMap) params, goTy tyMap result)
      | goTy tyMap (F.ForallType (tv, _, ty)) =
          goTy (TyVarMap.insert (tyMap, tv, F.BoxedType)) ty
      | goTy tyMap (F.ExistsType (tv, _, ty)) =
          goTy (TyVarMap.insert (tyMap, tv, F.BoxedType)) ty
      | goTy tyMap (F.TypeFn (tv, kind, ty)) =
          (case goTy tyMap ty of
             F.BoxedType => F.BoxedType
           | ty' => F.TypeFn (tv, kind, ty'))
      | goTy _ (ty as F.AnyType _) = ty
      | goTy _ (ty as F.BoxedType) = ty
      | goTy tyMap (ty as F.DelayedSubst _) =
          goTy tyMap (F.forceTy ty)
    (* typeOfValue: get the original type of a value *)
    fun typeOfValue ({varTys, ...}: env) (C.Var vid) =
          (case VIdMap.find (varTys, vid) of
             SOME ty => ty
           | NONE => F.BoxedType)
      | typeOfValue _ C.Unit = F.BoxedType
      | typeOfValue _ C.Nil = F.BoxedType
      | typeOfValue _ (C.TypedNil _) = F.BoxedType
      | typeOfValue _ (C.BoolConst _) = F.Types.bool
      | typeOfValue _ (C.IntConst (Primitives.I32, _)) = F.Types.int32
      | typeOfValue _ (C.IntConst (Primitives.I64, _)) = F.Types.int64
      | typeOfValue _ (C.IntConst _) = F.BoxedType
      | typeOfValue _ (C.WordConst (Primitives.W32, _)) = F.Types.word32
      | typeOfValue _ (C.WordConst (Primitives.W64, _)) = F.Types.word64
      | typeOfValue _ (C.WordConst _) = F.BoxedType
      | typeOfValue _ (C.CharConst (C.C8, _)) = F.Types.char
      | typeOfValue _ (C.CharConst (C.C16, _)) = F.Types.char16
      | typeOfValue _ (C.CharConst (C.C32, _)) = F.Types.char32
      | typeOfValue _ (C.CharConst _) = F.BoxedType
      | typeOfValue _ (C.StringConst _) = F.BoxedType
      | typeOfValue _ (C.String7Const _) = F.BoxedType
      | typeOfValue _ (C.String16Const _) = F.BoxedType
      | typeOfValue _ (C.String32Const _) = F.BoxedType
      | typeOfValue _ (C.PrimEffect _) = F.BoxedType
      | typeOfValue _ (C.Cast {to, ...}) = to
      | typeOfValue _ (C.Pack _) = F.BoxedType
    fun prependDec (dec, C.Let {decs = decs, cont}) =
          C.Let {decs = dec :: decs, cont = cont}
      | prependDec (dec, cont) =
          C.Let {decs = [dec], cont = cont}
    fun prependDecs ([], stat) = stat
      | prependDecs (dec :: rest, stat) =
          prependDec (dec, prependDecs (rest, stat))
    (* Box a value if its type is unboxed *)
    fun boxValue (ctx: Context) (v: C.Value, ty: F.Ty) : C.Dec list * C.Value =
      case tyToUnboxedTy ty of
        SOME ubt =>
          let
            val fresh = CpsSimplify.newVId (ctx, "boxed")
          in
            ( [C.ValDec
                 { exp =
                     C.PrimOp {primOp = F.BoxOp ubt, tyargs = [], args = [v]}
                 , results = [(SOME fresh, F.BoxedType)]
                 }]
            , C.Var fresh
            )
          end
      | NONE => ([], v)
    fun boxValues ctx (vals: C.Value list, tys: F.Ty list) :
      C.Dec list * C.Value list =
      let
        val pairs =
          ListPair.mapEq (fn (v, ty) => boxValue ctx (v, ty)) (vals, tys)
      in
        (List.concat (List.map #1 pairs), List.map #2 pairs)
      end
    (* Unbox a param: if unboxed type, rename param to fresh with BoxedType,
       prepend UnboxOp to body.
       Returns (newParam, prependedDecs, envUpdate) *)
    fun unboxParam (ctx: Context) (v: TypedSyntax.VId option, ty: F.Ty) :
      (TypedSyntax.VId option * F.Ty)
      * C.Dec list
      * (TypedSyntax.VId * F.Ty) option =
      case (v, tyToUnboxedTy ty) of
        (SOME vid, SOME ubt) =>
          let
            val fresh = CpsSimplify.renewVId (ctx, vid)
          in
            ( (SOME fresh, F.BoxedType)
            , [C.ValDec
                 { exp = C.PrimOp
                     {primOp = F.UnboxOp ubt, tyargs = [], args = [C.Var fresh]}
                 , results = [(SOME vid, ty)]
                 }]
            , SOME (vid, ty)
            )
          end
      | (SOME vid, NONE) => ((SOME vid, ty), [], SOME (vid, ty))
      | (NONE, _) => ((NONE, ty), [], NONE)
    fun unboxParams ctx params =
      let
        val results = List.map (unboxParam ctx) params
        val params' = List.map #1 results
        val decs = List.concat (List.map #2 results)
        val envUpdates = List.mapPartial #3 results
      in
        (params', decs, envUpdates)
      end
    fun transform (ctx: Context, program) =
      let
        fun addToVarEnv ({tyMap, varTys}: env, updates) : env =
          { tyMap = tyMap
          , varTys =
              List.foldl (fn ((v, ty), acc) => VIdMap.insert (acc, v, ty))
                varTys updates
          }
        fun addResults (env, results) =
          addToVarEnv
            ( env
            , List.mapPartial
                (fn (SOME v, ty) => SOME (v, ty) | (NONE, _) => NONE) results
            )
        fun goDecs (env as {tyMap, varTys}, dec :: decs, cont) =
              (case dec of
                 C.ValDec {exp, results} =>
                   let
                     fun simple exp =
                       prependDec
                         ( C.ValDec {exp = exp, results = results}
                         , goDecs (addResults (env, results), decs, cont)
                         )
                   in
                     case exp of
                       C.PrimOp _ => simple exp
                     | C.Record _ => simple exp
                     | C.ExnTag {name, payloadTy} =>
                         simple (C.ExnTag
                           { name = name
                           , payloadTy = Option.map (goTy tyMap) payloadTy
                           })
                     | C.Projection _ => simple exp
                     | C.Abs
                         { contParam
                         , tyParams
                         , params
                         , body
                         , resultTy
                         , attr as {typeOnly, ...}
                         } =>
                         let
                           val tyMap' =
                             List.foldl
                               (fn ((tv, _), acc) =>
                                  TyVarMap.insert (acc, tv, F.BoxedType)) tyMap
                               tyParams
                           val env' = {tyMap = tyMap', varTys = varTys}
                         in
                           case (params, typeOnly) of
                             ([], true) =>
                               (* Type-only abstraction: eliminate like erase-poly *)
                               C.Let
                                 { decs =
                                     [C.ContDec
                                        { name = contParam
                                        , params =
                                            List.map
                                              (fn (v, ty) => (v, goTy tyMap' ty))
                                              results
                                        , body = goDecs (env, decs, cont)
                                        , attr = {alwaysInline = false}
                                        }]
                                 , cont = goStat (env', body)
                                 }
                           | (_ :: _, true) =>
                               raise Fail "invalid type abstraction"
                           | (_, false) =>
                               let
                                 val (params', unboxDecs, paramEnvUpdates) =
                                   unboxParams ctx
                                     (List.map (fn (v, ty) => (SOME v, ty))
                                        params)
                                 val params'' =
                                   List.map
                                     (fn (SOME v, ty) => (v, ty)
                                       | (NONE, _) =>
                                        raise Fail "unexpected NONE") params'
                                 val bodyEnv =
                                   addToVarEnv (env', paramEnvUpdates)
                                 val body' = prependDecs
                                   (unboxDecs, goStat (bodyEnv, body))
                                 val resultTy' =
                                   if F.isUnboxedTy resultTy then F.BoxedType
                                   else resultTy
                               in
                                 prependDec
                                   ( C.ValDec
                                       { exp = C.Abs
                                           { contParam = contParam
                                           , tyParams = []
                                           , params = params''
                                           , body = body'
                                           , resultTy = resultTy'
                                           , attr = attr
                                           }
                                       , results = results
                                       }
                                   , goDecs
                                       (addResults (env, results), decs, cont)
                                   )
                               end
                         end
                   end
               | C.RecDec defs =>
                   let
                     fun stripTyAbs
                           ( name
                           , tyMap
                           , k
                           , C.Let
                               { decs =
                                   [C.ValDec
                                      { exp =
                                          C.Abs
                                            { contParam
                                            , tyParams
                                            , params
                                            , body
                                            , resultTy
                                            , attr as {typeOnly, ...}
                                            }
                                      , results = [(SOME v, _)]
                                      }]
                               , cont =
                                   C.AppCont {applied = k', args = [C.Var v']}
                               }
                           ) =
                           if k = k' andalso TypedSyntax.eqVId (v, v') then
                             case (params, typeOnly) of
                               ([], true) =>
                                 let
                                   val tyMap =
                                     List.foldl
                                       (fn ((tv, _), acc) =>
                                          TyVarMap.insert (acc, tv, F.BoxedType))
                                       tyMap tyParams
                                 in
                                   stripTyAbs (name, tyMap, contParam, body)
                                 end
                             | (_ :: _, true) =>
                                 raise Fail "invalid type abstraction"
                             | (_, false) =>
                                 let
                                   val env' = {tyMap = tyMap, varTys = varTys}
                                   val (params', unboxDecs, paramEnvUpdates) =
                                     unboxParams ctx
                                       (List.map (fn (v, ty) => (SOME v, ty))
                                          params)
                                   val params'' =
                                     List.map
                                       (fn (SOME v, ty) => (v, ty)
                                         | (NONE, _) =>
                                          raise Fail "unexpected NONE") params'
                                   val bodyEnv =
                                     addToVarEnv (env', paramEnvUpdates)
                                   val body' = prependDecs
                                     (unboxDecs, goStat (bodyEnv, body))
                                   val resultTy' =
                                     if F.isUnboxedTy resultTy then F.BoxedType
                                     else resultTy
                                 in
                                   { name = name
                                   , contParam = contParam
                                   , tyParams = []
                                   , params = params''
                                   , body = body'
                                   , resultTy = resultTy'
                                   , attr = attr
                                   }
                                 end
                           else
                             raise Fail "invalid recursive definition"
                       | stripTyAbs (_, _, _, _) =
                           raise Fail "invalid recursive definition"
                     fun goDef
                       { name
                       , contParam
                       , tyParams
                       , params
                       , body
                       , resultTy
                       , attr as {typeOnly, ...}
                       } =
                       let
                         val tyMap' =
                           List.foldl
                             (fn ((tv, _), acc) =>
                                TyVarMap.insert (acc, tv, F.BoxedType)) tyMap
                             tyParams
                         val env' = {tyMap = tyMap', varTys = varTys}
                       in
                         case (params, typeOnly) of
                           ([], true) =>
                             stripTyAbs (name, tyMap', contParam, body)
                         | (_ :: _, true) =>
                             raise Fail "invalid type abstraction"
                         | (_, false) =>
                             let
                               val (params', unboxDecs, paramEnvUpdates) =
                                 unboxParams ctx
                                   (List.map (fn (v, ty) => (SOME v, ty)) params)
                               val params'' =
                                 List.map
                                   (fn (SOME v, ty) => (v, ty)
                                     | (NONE, _) => raise Fail "unexpected NONE")
                                   params'
                               val bodyEnv = addToVarEnv (env', paramEnvUpdates)
                               val body' = prependDecs
                                 (unboxDecs, goStat (bodyEnv, body))
                               val resultTy' =
                                 if F.isUnboxedTy resultTy then F.BoxedType
                                 else resultTy
                             in
                               { name = name
                               , contParam = contParam
                               , tyParams = []
                               , params = params''
                               , body = body'
                               , resultTy = resultTy'
                               , attr = attr
                               }
                             end
                       end
                   in
                     prependDec
                       ( C.RecDec (List.map goDef defs)
                       , goDecs (env, decs, cont)
                       )
                   end
               | C.ContDec {name, params, body, attr} =>
                   let
                     val (params', unboxDecs, paramEnvUpdates) =
                       unboxParams ctx params
                     val bodyEnv = addToVarEnv (env, paramEnvUpdates)
                     val body' = prependDecs (unboxDecs, goStat (bodyEnv, body))
                   in
                     prependDec
                       ( C.ContDec
                           { name = name
                           , params = params'
                           , body = body'
                           , attr = attr
                           }
                       , goDecs (env, decs, cont)
                       )
                   end
               | C.RecContDec defs =>
                   let
                     fun goDef (name, params, body) =
                       let
                         val (params', unboxDecs, paramEnvUpdates) =
                           unboxParams ctx params
                         val bodyEnv = addToVarEnv (env, paramEnvUpdates)
                         val body' = prependDecs
                           (unboxDecs, goStat (bodyEnv, body))
                       in
                         (name, params', body')
                       end
                   in
                     prependDec
                       ( C.RecContDec (List.map goDef defs)
                       , goDecs (env, decs, cont)
                       )
                   end
               | C.UnpackDec _ => prependDec (dec, goDecs (env, decs, cont))
               | C.DatatypeDec _ => prependDec (dec, goDecs (env, decs, cont))
               | C.ESImportDec {pure, specs, moduleName} =>
                   let
                     val specs' =
                       List.map (fn (n, v, ty) => (n, v, goTy tyMap ty)) specs
                     val envUpdates = List.map (fn (_, v, ty) => (v, ty)) specs
                     val env' = addToVarEnv (env, envUpdates)
                   in
                     prependDec
                       ( C.ESImportDec
                           { pure = pure
                           , specs = specs'
                           , moduleName = moduleName
                           }
                       , goDecs (env', decs, cont)
                       )
                   end)
          | goDecs (env, [], cont) = goStat (env, cont)
        and goStat (env, C.Let {decs, cont}) = goDecs (env, decs, cont)
          | goStat (env, C.App {applied, cont, tyArgs = _, args, attr}) =
              let
                val argTys = List.map (typeOfValue env) args
                val (boxDecs, args') = boxValues ctx (args, argTys)
              in
                prependDecs (boxDecs, C.App
                  { applied = applied
                  , cont = cont
                  , tyArgs = []
                  , args = args'
                  , attr = attr
                  })
              end
          | goStat (env, C.AppCont {applied, args}) =
              let
                val argTys = List.map (typeOfValue env) args
                val (boxDecs, args') = boxValues ctx (args, argTys)
              in
                prependDecs
                  (boxDecs, C.AppCont {applied = applied, args = args'})
              end
          | goStat (env, C.If {cond, thenCont, elseCont}) =
              C.If
                { cond = cond
                , thenCont = goStat (env, thenCont)
                , elseCont = goStat (env, elseCont)
                }
          | goStat
              ( env as {tyMap, ...}
              , C.Handle
                  { body
                  , handler = (v, h)
                  , successfulExitIn
                  , successfulExitOut
                  , resultTy
                  }
              ) =
              C.Handle
                { body = goStat (env, body)
                , handler = (v, goStat (env, h))
                , successfulExitIn = successfulExitIn
                , successfulExitOut = successfulExitOut
                , resultTy = goTy tyMap resultTy
                }
          | goStat (_, C.Raise (span, v)) = C.Raise (span, v)
          | goStat (_, s as C.Unreachable) = s
      in
        goStat ({tyMap = TyVarMap.empty, varTys = VIdMap.empty}, program)
      end
  end
end;
