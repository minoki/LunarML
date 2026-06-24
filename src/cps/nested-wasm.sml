(*
 * Copyright (c) 2026 ARATA Mizuki
 * This file is part of LunarML.
 *)
(* Wasm-specific CSyntax -> NSyntax conversion that fuses the boxing pass
 * (formerly CpsBoxing in cps/boxing.sml) into the nesting reconstruction.
 *
 * Unlike NSyntax.fromStat (used by the Lua/JS backends, a pure structural
 * map), this pass threads a type environment and inserts BoxOp/UnboxOp at
 * function/continuation boundaries: arguments of unboxed type are boxed at
 * App/AppCont sites, and unboxed parameters are renamed to BoxedType with an
 * UnboxOp prepended to the body.
 *
 * Because it runs after all CPS optimizations (at emit time, the same point
 * NSyntax.fromStat runs), boxing sees fully-inlined code, so no wasted
 * boundaries are created and the CPS type checker can stay enabled through
 * the whole CPS pipeline. The output type matches NSyntax.fromStat
 * (FSyntax.Ty NSyntax.stat); the machine-oriented RepType is a later step. *)
structure NSyntaxFromCpsWasm:
sig
  val fromStatWasm: CpsSimplify.Context * CSyntax.Stat
                    -> FSyntax.Ty NSyntax.stat
end =
struct
  local
    structure F = FSyntax
    structure C = CSyntax
    structure N = NSyntax
    structure TyVarMap = TypedSyntax.TyVarMap
    structure VIdMap = TypedSyntax.VIdMap
  in
    type Context = CpsSimplify.Context
    type env = {tyMap: F.Ty TyVarMap.map, varTys: F.Ty VIdMap.map}
    (* Same as CpsBoxing.tyToUnboxedTy *)
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
    (* Same as CpsBoxing.goTy *)
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
    (* Same as CpsBoxing.typeOfValue *)
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
    (* Prepend NSyntax decs to a body, merging into a leading Let if present. *)
    fun prependDecs ([], body) = body
      | prependDecs (decs, N.Let {decs = decs', cont}) =
          N.Let {decs = decs @ decs', cont = cont}
      | prependDecs (decs, body) = N.Let {decs = decs, cont = body}
    fun prependDec (dec, body) =
      prependDecs ([dec], body)
    (* Box a value (as an inline NSyntax exp) if its type is unboxed. *)
    fun boxArg (env: env) (v: C.Value) : F.Ty N.exp =
      case tyToUnboxedTy (typeOfValue env v) of
        SOME ubt =>
          N.PrimOp {primOp = F.BoxOp ubt, tyargs = [], args = [N.Value v]}
      | NONE => N.Value v
    fun boxArgs env vals =
      List.map (boxArg env) vals
    (* Unbox a param: if unboxed, rename to fresh BoxedType and prepend an
       UnboxOp dec binding the original var.
       Returns (newParam, prependedDecs, envUpdate). *)
    fun unboxParam (ctx: Context) (v: TypedSyntax.VId option, ty: F.Ty) :
      (TypedSyntax.VId option * F.Ty)
      * F.Ty N.dec list
      * (TypedSyntax.VId * F.Ty) option =
      case (v, tyToUnboxedTy ty) of
        (SOME vid, SOME ubt) =>
          let
            val fresh = CpsSimplify.renewVId (ctx, vid)
          in
            ( (SOME fresh, F.BoxedType)
            , [N.ValDec
                 { exp = N.PrimOp
                     { primOp = F.UnboxOp ubt
                     , tyargs = []
                     , args = [N.Value (C.Var fresh)]
                     }
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
    (* Unbox non-optional params (Abs/RecDec). *)
    fun unboxNamedParams ctx params =
      let
        val (params', decs, envUpdates) = unboxParams ctx
          (List.map (fn (v, ty) => (SOME v, ty)) params)
        val params'' =
          List.map
            (fn (SOME v, ty) => (v, ty)
              | (NONE, _) => raise Fail "unexpected NONE") params'
      in
        (params'', decs, envUpdates)
      end
    fun fromStatWasm (ctx: Context, program) =
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
        fun boundResultTy resultTy =
          if F.isUnboxedTy resultTy then F.BoxedType else resultTy
        fun goDecs (env as {tyMap, varTys}, dec :: decs, cont) =
              (case dec of
                 C.ValDec {exp, results} =>
                   let
                     fun simple exp' =
                       prependDec
                         ( N.ValDec {exp = exp', results = results}
                         , goDecs (addResults (env, results), decs, cont)
                         )
                   in
                     case exp of
                       C.PrimOp {primOp, tyargs, args} =>
                         simple (N.PrimOp
                           { primOp = primOp
                           , tyargs = tyargs
                           , args = List.map N.Value args
                           })
                     | C.Record fields =>
                         simple (N.Record (Syntax.LabelMap.map N.Value fields))
                     | C.ExnTag {name, payloadTy} =>
                         simple (N.ExnTag
                           { name = name
                           , payloadTy = Option.map (goTy tyMap) payloadTy
                           })
                     | C.Projection {label, record, fieldTypes} =>
                         simple
                           (N.Projection
                              { label = label
                              , record = N.Value record
                              , fieldTypes = fieldTypes
                              })
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
                               N.Let
                                 { decs =
                                     [N.ContDec
                                        { name = contParam
                                        , params =
                                            List.map
                                              (fn (v, ty) => (v, goTy tyMap' ty))
                                              results
                                        , body = goDecs (env, decs, cont)
                                        }]
                                 , cont = goStat (env', body)
                                 }
                           | (_ :: _, true) =>
                               raise Fail "invalid type abstraction"
                           | (_, false) =>
                               let
                                 val (params', unboxDecs, paramEnvUpdates) =
                                   unboxNamedParams ctx params
                                 val bodyEnv =
                                   addToVarEnv (env', paramEnvUpdates)
                                 val body' = prependDecs
                                   (unboxDecs, goStat (bodyEnv, body))
                               in
                                 prependDec
                                   ( N.ValDec
                                       { exp = N.Abs
                                           { contParam = contParam
                                           , params = params'
                                           , body = body'
                                           , resultTy = boundResultTy resultTy
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
                                     unboxNamedParams ctx params
                                   val bodyEnv =
                                     addToVarEnv (env', paramEnvUpdates)
                                   val body' = prependDecs
                                     (unboxDecs, goStat (bodyEnv, body))
                                 in
                                   { name = name
                                   , contParam = contParam
                                   , params = params'
                                   , body = body'
                                   , resultTy = boundResultTy resultTy
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
                                 unboxNamedParams ctx params
                               val bodyEnv = addToVarEnv (env', paramEnvUpdates)
                               val body' = prependDecs
                                 (unboxDecs, goStat (bodyEnv, body))
                             in
                               { name = name
                               , contParam = contParam
                               , params = params'
                               , body = body'
                               , resultTy = boundResultTy resultTy
                               , attr = attr
                               }
                             end
                       end
                   in
                     prependDec
                       ( N.RecDec (List.map goDef defs)
                       , goDecs (env, decs, cont)
                       )
                   end
               | C.UnpackDec _ =>
                   raise Fail "NSyntaxFromCpsWasm: unexpected UnpackDec"
               | C.ContDec {name, params, body, attr = _} =>
                   let
                     val (params', unboxDecs, paramEnvUpdates) =
                       unboxParams ctx params
                     val bodyEnv = addToVarEnv (env, paramEnvUpdates)
                     val body' = prependDecs (unboxDecs, goStat (bodyEnv, body))
                   in
                     prependDec
                       ( N.ContDec {name = name, params = params', body = body'}
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
                       ( N.RecContDec (List.map goDef defs)
                       , goDecs (env, decs, cont)
                       )
                   end
               | C.DatatypeDec _ => goDecs (env, decs, cont)
               | C.ESImportDec {pure, specs, moduleName} =>
                   let
                     val specs' =
                       List.map (fn (n, v, ty) => (n, v, goTy tyMap ty)) specs
                     val envUpdates = List.map (fn (_, v, ty) => (v, ty)) specs
                     val env' = addToVarEnv (env, envUpdates)
                   in
                     prependDec
                       ( N.ESImportDec
                           { pure = pure
                           , specs = specs'
                           , moduleName = moduleName
                           }
                       , goDecs (env', decs, cont)
                       )
                   end)
          | goDecs (env, [], cont) = goStat (env, cont)
        and goStat (env, C.Let {decs, cont}) = goDecs (env, decs, cont)
          | goStat (env, C.App {applied, cont, tyArgs, args, attr}) =
              ( case tyArgs of
                  [] => ()
                | _ =>
                    raise Fail "NSyntaxFromCpsWasm: App with non-empty tyArgs"
              ; N.App
                  { applied = N.Value applied
                  , cont = cont
                  , args = boxArgs env args
                  , attr = attr
                  }
              )
          | goStat (env, C.AppCont {applied, args}) =
              N.AppCont {applied = applied, args = boxArgs env args}
          | goStat (env, C.If {cond, thenCont, elseCont}) =
              N.If
                { cond = N.Value cond
                , thenCont = goStat (env, thenCont)
                , elseCont = goStat (env, elseCont)
                }
          | goStat
              ( env as {tyMap, ...}
              , C.Handle
                  { body
                  , handler = (e, h)
                  , successfulExitIn
                  , successfulExitOut
                  , resultTy
                  }
              ) =
              N.Handle
                { body = goStat (env, body)
                , handler = (e, goStat (env, h))
                , successfulExitIn = successfulExitIn
                , successfulExitOut = successfulExitOut
                , resultTy = goTy tyMap resultTy
                }
          | goStat (_, C.Raise (span, x)) =
              N.Raise (span, N.Value x)
          | goStat (_, C.Unreachable) = N.Unreachable
      in
        goStat ({tyMap = TyVarMap.empty, varTys = VIdMap.empty}, program)
      end
  end
end;
