(*
 * Copyright (c) 2024 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure CpsUncurry:
sig
  val goStat: CpsSimplify.Context * CSyntax.Stat -> CSyntax.Stat
end =
struct
  local structure C = CSyntax
  in
    (*: val tryUncurry : C.SimpleExp -> (((FSyntax.TyVar * FSyntax.Kind) list * (C.Var * C.Ty) list * bool) list * C.CVar * C.Stat * C.Ty * bool) option *)
    fun tryUncurry
          (C.Abs
             { contParam
             , tyParams
             , params
             , body as
                 C.Let {decs, cont = C.AppCont {applied = k, args = [C.Var v]}}
             , resultTy
             , attr = {alwaysInline = false, typeOnly}
             }) =
          (case decs of
             [C.ValDec {exp, results = [(SOME f, _)]}] =>
               if contParam = k andalso v = f then
                 case tryUncurry exp of
                   SOME (rest, k, b, r, typeOnly') =>
                     SOME
                       ( (tyParams, params, typeOnly) :: rest
                       , k
                       , b
                       , r
                       , typeOnly andalso typeOnly'
                       )
                 | NONE =>
                     SOME
                       ( [(tyParams, params, typeOnly)]
                       , contParam
                       , body
                       , resultTy
                       , typeOnly
                       )
               else
                 SOME
                   ( [(tyParams, params, typeOnly)]
                   , contParam
                   , body
                   , resultTy
                   , typeOnly
                   )
           | _ =>
               SOME
                 ( [(tyParams, params, typeOnly)]
                 , contParam
                 , body
                 , resultTy
                 , typeOnly
                 ))
      | tryUncurry _ = NONE
    fun doUncurry (ctx, name, exp, ty, acc) =
      case tryUncurry exp of
        SOME (allParams as (_ :: _ :: _), k, body, resultTy, typeOnly) =>
          let
            val () = #simplificationOccurred ctx := true
            val allTyParams = List.concat (List.map #1 allParams)
            val allValParams = List.concat (List.map #2 allParams)
            val workerName = CpsSimplify.renewVId (ctx, name)
            val workerTy =
              if typeOnly then resultTy
              else FSyntax.MultiFnType (List.map #2 allValParams, resultTy)
            val workerTy =
              List.foldr
                (fn ((tv, kind), ty) => FSyntax.ForallType (tv, kind, ty))
                workerTy allTyParams
            val body = simplifyStat (ctx, body)
            val workerDec = C.ValDec
              { exp = C.Abs
                  { contParam = k
                  , tyParams = allTyParams
                  , params = allValParams
                  , body = body
                  , resultTy = resultTy
                  , attr = {alwaysInline = false, typeOnly = typeOnly}
                  }
              , results = [(SOME workerName, workerTy)]
              }
            val (tysubst, allParamsRev') =
              List.foldl
                (fn ((tyParams, params, typeOnly), (tysubst, acc)) =>
                   let
                     val (tysubst, tyParams) =
                       List.foldr
                         (fn ((tv, kind), (tysubst, acc)) =>
                            let
                              val tv' = CpsSimplify.renewTyVar (ctx, tv)
                            in
                              ( TypedSyntax.TyVarMap.insert
                                  (tysubst, tv, FSyntax.TyVar tv')
                              , (tv', kind) :: acc
                              )
                            end) (tysubst, []) tyParams
                   in
                     ( tysubst
                     , ( tyParams
                       , List.map
                           (fn (p, ty) =>
                              ( CpsSimplify.renewVId (ctx, p)
                              , #doTy (FSyntax.lazySubstTy tysubst) ty
                              )) params
                       , typeOnly
                       ) :: acc
                     )
                   end) (TypedSyntax.TyVarMap.empty, []) allParams
            val allParams' = List.rev allParamsRev'
            val wrapperResultTy = #doTy (FSyntax.lazySubstTy tysubst) resultTy
            val wrapperTy =
              List.foldr
                (fn ((tyParams, params, typeOnly), ty) =>
                   let
                     val ty =
                       if typeOnly then ty
                       else FSyntax.MultiFnType (List.map #2 params, ty)
                   in
                     List.foldr
                       (fn ((tv, kind), ty) => FSyntax.ForallType (tv, kind, ty))
                       ty tyParams
                   end) wrapperResultTy allParams'
            fun mkWrapperBody (k, _, []) =
                  C.App
                    { applied = C.Var workerName
                    , cont = k
                    , tyArgs =
                        List.map (fn (tv, _) => FSyntax.TyVar tv) (List.concat
                          (List.map #1 allParams'))
                    , args = List.map (fn (v, _) => C.Var v) (List.concat
                        (List.map #2 allParams'))
                    , attr = {typeOnly = typeOnly}
                    }
              | mkWrapperBody (k, absTy, p as (tyParams, params, _) :: _) =
                  let
                    val name = CpsSimplify.renewVId (ctx, name)
                  (* val absTy = FSyntax.MultiFnType
                    (List.map #2 params, wrapperResultTy)
                  val absTy =
                    List.foldr
                      (fn ((tv, kind), ty) =>
                         FSyntax.ForallType (tv, kind, ty)) absTy tyParams *)
                  in
                    C.Let
                      { decs =
                          [C.ValDec
                             { exp = mkWrapperAbs (absTy, p)
                             , results = [(SOME name, absTy)]
                             }]
                      , cont = C.AppCont {applied = k, args = [C.Var name]}
                      }
                  end
            and mkWrapperAbs (absTy, (tyParams, params, typeOnly) :: rest) =
                  let
                    val k = CpsSimplify.genContSym ctx
                    val absTy' =
                      List.foldl
                        (fn (_, FSyntax.ForallType (_, _, ty)) => ty
                          | _ => raise Fail "mkWrapperAbs: expected ForallType")
                        absTy tyParams
                    val absTy' =
                      case (typeOnly, absTy') of
                        (true, ty) => ty
                      | (false, FSyntax.MultiFnType (_, ty)) => ty
                      | (false, _) =>
                          raise Fail "mkWrapperAbs: expected MultiFnType"
                  in
                    C.Abs
                      { contParam = k
                      , tyParams = tyParams
                      , params = params
                      , body = mkWrapperBody (k, absTy', rest)
                      , resultTy = absTy'
                      , attr = {alwaysInline = true, typeOnly = typeOnly}
                      }
                  end
              | mkWrapperAbs (_, []) = raise Fail "mkWrapperAbs"
          (* val () = print ("CpsUncurry: wrapperTy=" ^ Printer.build (FPrinter.doTy 0 wrapperTy) ^ "\n")
          val () = print ("CpsUncurry: workerTy=" ^ Printer.build (FPrinter.doTy 0 workerTy) ^ "\n")
          val () = print ("CpsUncurry: ty=" ^ Printer.build (FPrinter.doTy 0 ty) ^ "\n") *)
          in
            C.ValDec
              { exp = mkWrapperAbs (wrapperTy, allParams')
              , results = [(SOME name, ty)]
              } :: workerDec :: acc
          end
      | _ => C.ValDec {exp = exp, results = [(SOME name, ty)]} :: acc
    and simplifyDec ctx (dec, acc) =
      case dec of
        C.ValDec {exp as C.Abs _, results = [(SOME name, ty)]} =>
          doUncurry (ctx, name, exp, ty, acc)
      | C.ValDec {exp = C.Abs _, results = [(NONE, _)]} => acc
      | C.ValDec {exp = _, results = _} => dec :: acc
      | C.RecDec defs =>
          let
            val defs =
              List.map
                (fn {name, contParam, tyParams, params, body, resultTy, attr} =>
                   { name = name
                   , contParam = contParam
                   , tyParams = tyParams
                   , params = params
                   , body = simplifyStat (ctx, body)
                   , resultTy = resultTy
                   , attr = attr
                   }) defs
          in
            C.RecDec defs :: acc
          end
      | C.UnpackDec _ => dec :: acc
      | C.ContDec {name, params, body, attr} =>
          C.ContDec
            { name = name
            , params = params
            , body = simplifyStat (ctx, body)
            , attr = attr
            } :: acc
      | C.DatatypeDec _ => dec :: acc
      | C.RecContDec defs =>
          let
            val defs =
              List.map
                (fn (name, params, body) =>
                   (name, params, simplifyStat (ctx, body))) defs
          in
            C.RecContDec defs :: acc
          end
      | C.ESImportDec _ => dec :: acc
    and simplifyStat (ctx: CpsSimplify.Context, exp) =
      case exp of
        C.Let {decs, cont} =>
          C.Let
            { decs = List.rev (List.foldl (simplifyDec ctx) [] decs)
            , cont = simplifyStat (ctx, cont)
            }
      | C.App _ => exp
      | C.AppCont _ => exp
      | C.If {cond, thenCont, elseCont} =>
          C.If
            { cond = cond
            , thenCont = simplifyStat (ctx, thenCont)
            , elseCont = simplifyStat (ctx, elseCont)
            }
      | C.Handle
          { body
          , handler = (e, h)
          , successfulExitIn
          , successfulExitOut
          , resultTy
          } =>
          C.Handle
            { body = simplifyStat (ctx, body)
            , handler = (e, simplifyStat (ctx, h))
            , successfulExitIn = successfulExitIn
            , successfulExitOut = successfulExitOut
            , resultTy = resultTy
            }
      | C.Raise _ => exp
      | C.Unreachable => exp
    val goStat = simplifyStat
  end (* local *)
end; (* structure CpsUncurry *)
