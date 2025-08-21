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
            val workerTy = FSyntax.MultiFnType
              (List.map #2 allValParams, resultTy)
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
            val allParams' =
              List.map
                (fn (tyParams, params, typeOnly) =>
                   ( List.map
                       (fn (tv, kind) =>
                          (CpsSimplify.renewTyVar (ctx, tv), kind)) tyParams
                   , List.map
                       (fn (p, ty) => (CpsSimplify.renewVId (ctx, p), ty))
                       params
                   , typeOnly
                   )) allParams
            fun mkWrapperBody (k, []) =
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
              | mkWrapperBody (k, p as (tyParams, params, _) :: _) =
                  let
                    val name = CpsSimplify.renewVId (ctx, name)
                  in
                    C.Let
                      { decs =
                          [C.ValDec
                             { exp = mkWrapperAbs p
                             , results =
                                 [( SOME name
                                  , FSyntax.MultiFnType
                                      (List.map #2 params, resultTy)
                                  )]
                             }]
                      , cont = C.AppCont {applied = k, args = [C.Var name]}
                      }
                  end
            and mkWrapperAbs ((tyParams, params, typeOnly) :: rest) =
                  let
                    val k = CpsSimplify.genContSym ctx
                  in
                    C.Abs
                      { contParam = k
                      , tyParams = tyParams
                      , params = params
                      , body = mkWrapperBody (k, rest)
                      , resultTy = resultTy
                      , attr = {alwaysInline = true, typeOnly = typeOnly}
                      }
                  end
              | mkWrapperAbs [] = raise Fail "mkWrapperAbs"
          in
            C.ValDec
              {exp = mkWrapperAbs allParams', results = [(SOME name, ty)]}
            :: workerDec :: acc
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
      | C.Handle {body, handler = (e, h), successfulExitIn, successfulExitOut} =>
          C.Handle
            { body = simplifyStat (ctx, body)
            , handler = (e, simplifyStat (ctx, h))
            , successfulExitIn = successfulExitIn
            , successfulExitOut = successfulExitOut
            }
      | C.Raise _ => exp
      | C.Unreachable => exp
    val goStat = simplifyStat
  end (* local *)
end; (* structure CpsUncurry *)
