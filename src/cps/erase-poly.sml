(*
 * Copyright (c) 2025 ARATA Mizuki
 * This file is part of LunarML.
 *)
(* This module erases polymorphic types *)
structure CpsErasePoly:
sig
  val goTy: FSyntax.Ty TypedSyntax.TyVarMap.map -> FSyntax.Ty -> FSyntax.Ty
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
    type env = {tyMap: F.Ty TyVarMap.map, valMap: C.Value VIdMap.map}
    fun goTy (env: F.Ty TyVarMap.map) (ty as F.TyVar tv) =
          (case TyVarMap.find (env, tv) of
             NONE => ty
           | SOME replacement => replacement)
      | goTy env (F.RecordType fields) =
          F.RecordType (Syntax.LabelMap.map (goTy env) fields)
      | goTy env (F.AppType {applied, arg}) =
          (case goTy env applied of
             F.AnyType (F.ArrowKind (_, kind)) => F.AnyType kind
           | applied => F.AppType {applied = applied, arg = goTy env arg})
      | goTy env (F.MultiFnType (params, result)) =
          F.MultiFnType (List.map (goTy env) params, goTy env result)
      | goTy env (F.ForallType (tv, kind, ty)) =
          goTy (TyVarMap.insert (env, tv, F.AnyType kind)) ty
      | goTy env (F.ExistsType (tv, kind, ty)) =
          goTy (TyVarMap.insert (env, tv, F.AnyType kind)) ty
      | goTy env (F.TypeFn (tv, kind, ty)) =
          (case goTy env ty of
             F.AnyType kind2 => F.AnyType (F.ArrowKind (kind, kind2))
           | ty => F.TypeFn (tv, kind, goTy env ty))
      | goTy _ (ty as F.AnyType _) = ty
      | goTy env (ty as F.DelayedSubst _) =
          goTy env (F.forceTy ty)
    fun goValue ({valMap, ...}: env) (v as C.Var vid) =
          (case VIdMap.find (valMap, vid) of
             NONE => v
           | SOME replacement => replacement)
      | goValue _ (v as C.Unit) = v
      | goValue _ C.Nil =
          C.TypedNil (F.AnyType F.TypeKind)
      | goValue {tyMap, ...} (C.TypedNil ty) =
          C.TypedNil (goTy tyMap ty)
      | goValue _ (v as C.BoolConst _) = v
      | goValue _ (v as C.IntConst _) = v
      | goValue _ (v as C.WordConst _) = v
      | goValue _ (v as C.CharConst _) = v
      | goValue _ (v as C.StringConst _) = v
      | goValue _ (v as C.String7Const _) = v
      | goValue _ (v as C.String16Const _) = v
      | goValue _ (v as C.String32Const _) = v
      | goValue _ (v as C.PrimEffect _) = v
      | goValue (env as {tyMap, ...}) (C.Cast {value, from, to}) =
          C.Cast
            { value = goValue env value
            , from = goTy tyMap from
            , to = goTy tyMap to
            }
      | goValue env (C.Pack {value, payloadTy = _, packageTy = _}) =
          goValue env value
    fun prependDec (dec, C.Let {decs = decs, cont}) =
          C.Let {decs = dec :: decs, cont = cont}
      | prependDec (dec, cont) =
          C.Let {decs = [dec], cont = cont}
    fun transform (ctx: Context, program) =
      let
        fun goDecs (env as {tyMap, valMap}, dec :: decs, cont) =
              (case dec of
                 C.ValDec {exp, results} =>
                   let
                     fun simple exp =
                       prependDec
                         ( C.ValDec
                             { exp = exp
                             , results =
                                 List.map (fn (v, ty) => (v, goTy tyMap ty))
                                   results
                             }
                         , goDecs (env, decs, cont)
                         )
                   in
                     case exp of
                       C.PrimOp {primOp, tyargs, args} =>
                         simple (C.PrimOp
                           { primOp = primOp
                           , tyargs = List.map (goTy tyMap) tyargs
                           , args = List.map (goValue env) args
                           })
                     | C.Record fields =>
                         simple (C.Record
                           (Syntax.LabelMap.map (goValue env) fields))
                     | C.ExnTag {name, payloadTy} =>
                         simple (C.ExnTag
                           { name = name
                           , payloadTy = Option.map (goTy tyMap) payloadTy
                           })
                     | C.Projection {label, record, fieldTypes} =>
                         simple (C.Projection
                           { label = label
                           , record = goValue env record
                           , fieldTypes =
                               Syntax.LabelMap.map (goTy tyMap) fieldTypes
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
                               (fn ((tv, kind), acc) =>
                                  TyVarMap.insert (acc, tv, F.AnyType kind))
                               tyMap tyParams
                           val env' = {tyMap = tyMap', valMap = valMap}
                         in
                           case (params, typeOnly) of
                             ([], true) =>
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
                               simple (C.Abs
                                 { contParam = contParam
                                 , tyParams = []
                                 , params =
                                     List.map
                                       (fn (v, ty) => (v, goTy tyMap' ty))
                                       params
                                 , body = goStat (env', body)
                                 , resultTy = goTy tyMap' resultTy
                                 , attr = attr
                                 })
                         end
                   end
               | C.RecDec defs =>
                   let
                     fun stripTyAbs
                           ( name
                           , tyMap
                           , k
                           , stat as
                               C.Let
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
                                       (fn ((tv, kind), acc) =>
                                          TyVarMap.insert
                                            (acc, tv, F.AnyType kind)) tyMap
                                       tyParams
                                 in
                                   stripTyAbs (name, tyMap, contParam, body)
                                 end
                             | (_ :: _, true) =>
                                 raise Fail "invalid type abstraction"
                             | (_, false) =>
                                 { name = name
                                 , contParam = contParam
                                 , tyParams = []
                                 , params =
                                     List.map (fn (v, ty) => (v, goTy tyMap ty))
                                       params
                                 , body =
                                     goStat
                                       ({tyMap = tyMap, valMap = valMap}, body)
                                 , resultTy = goTy tyMap resultTy
                                 , attr = attr
                                 }
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
                         val tyMap =
                           List.foldl
                             (fn ((tv, kind), acc) =>
                                TyVarMap.insert (acc, tv, F.AnyType kind)) tyMap
                             tyParams
                         val env = {tyMap = tyMap, valMap = valMap}
                         val goTy' = goTy tyMap
                       in
                         case (params, typeOnly) of
                           ([], true) =>
                             stripTyAbs (name, tyMap, contParam, body)
                         | (_ :: _, true) =>
                             raise Fail "invalid type abstraction"
                         | (_, false) =>
                             { name = name
                             , contParam = contParam
                             , tyParams = []
                             , params =
                                 List.map (fn (v, ty) => (v, goTy' ty)) params
                             , body = goStat (env, body)
                             , resultTy = goTy' resultTy
                             , attr = attr
                             }
                       end
                   in
                     prependDec
                       ( C.RecDec (List.map goDef defs)
                       , goDecs (env, decs, cont)
                       )
                   end
               | C.UnpackDec {tyVar, kind, vid, unpackedTy, package} =>
                   let
                     val tyMap = TyVarMap.insert (tyMap, tyVar, F.AnyType kind)
                     val package =
                       goValue env
                         package (* C.Cast { value = _, from = F.AnyType F.TypeKind (* TODO *), to = F.substituteTy (tyVar, F.AnyType kind) unpackedTy } *)
                     val valMap = VIdMap.insert (valMap, vid, package)
                     val env = {tyMap = tyMap, valMap = valMap}
                   in
                     goDecs (env, decs, cont)
                   end
               | C.ContDec {name, params, body, attr} =>
                   prependDec
                     ( C.ContDec
                         { name = name
                         , params =
                             List.map (fn (v, ty) => (v, goTy tyMap ty)) params
                         , body = goStat (env, body)
                         , attr = attr
                         }
                     , goDecs (env, decs, cont)
                     )
               | C.RecContDec defs =>
                   let
                     fun goDef (name, params, body) =
                       ( name
                       , List.map (fn (v, ty) => (v, goTy tyMap ty)) params
                       , goStat (env, body)
                       )
                   in
                     prependDec
                       ( C.RecContDec (List.map goDef defs)
                       , goDecs (env, decs, cont)
                       )
                   end
               | C.DatatypeDec _ => prependDec (dec, goDecs (env, decs, cont))
               | C.ESImportDec {pure, specs, moduleName} =>
                   prependDec
                     ( C.ESImportDec
                         { pure = pure
                         , specs =
                             List.map (fn (n, v, ty) => (n, v, goTy tyMap ty))
                               specs
                         , moduleName = moduleName
                         }
                     , goDecs (env, decs, cont)
                     ))
          | goDecs (env, [], cont) = goStat (env, cont)
        and goStat (env, C.Let {decs, cont}) = goDecs (env, decs, cont)
          | goStat
              (env, C.App {applied, cont, tyArgs = _, args, attr as {typeOnly}}) =
              (case (args, typeOnly) of
                 ([], true) =>
                   C.AppCont {applied = cont, args = [goValue env applied]}
               | (_ :: _, true) => raise Fail "invalid type application"
               | (_, false) =>
                   C.App
                     { applied = goValue env applied
                     , cont = cont
                     , tyArgs = []
                     , args = List.map (goValue env) args
                     , attr = attr
                     })
          | goStat (env, C.AppCont {applied, args}) =
              C.AppCont {applied = applied, args = List.map (goValue env) args}
          | goStat (env, C.If {cond, thenCont, elseCont}) =
              C.If
                { cond = goValue env cond
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
          | goStat (env, C.Raise (span, v)) =
              C.Raise (span, goValue env v)
          | goStat (_, s as C.Unreachable) = s
      in
        goStat ({tyMap = TyVarMap.empty, valMap = VIdMap.empty}, program)
      end
  end
end;
