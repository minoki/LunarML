(*
 * Copyright (c) 2024 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure CpsEtaConvert:
sig
  val go: CpsSimplify.Context * CSyntax.Stat -> CSyntax.Stat
end =
struct
  structure C = CSyntax
  fun goCont (env, cont) =
    (case C.CVarMap.find (env, cont) of
       SOME cont' => cont'
     | NONE => cont)
  fun goDec (dec, (env, acc)) =
    case dec of
      C.ValDec
        { exp = C.Abs {contParam, tyParams, params, body, resultTy, attr}
        , results as [(SOME _, _)]
        } =>
        let
          (* Eta conversion of a function is not implemented yet *)
          val dec' = C.ValDec
            { exp = C.Abs
                { contParam = contParam
                , tyParams = tyParams
                , params = params
                , body = goFunction body
                , resultTy = resultTy
                , attr = attr
                }
            , results = results
            }
        in
          (env, dec' :: acc)
        end
    | C.ValDec {exp = _, results = _} => (env, dec :: acc)
    | C.RecDec defs =>
        let
          val defs =
            List.map
              (fn {name, contParam, tyParams, params, body, resultTy, attr} =>
                 { name = name
                 , contParam = contParam
                 , tyParams = tyParams
                 , params = params
                 , body = goFunction body
                 , resultTy = resultTy
                 , attr = attr
                 }) defs
        in
          (env, C.RecDec defs :: acc)
        end
    | C.UnpackDec _ => (env, dec :: acc)
    | C.ContDec {name, params, body as C.AppCont {applied, args}, attr} =>
        (* Eta conversion *)
        let
          fun sameValue ((SOME p, _), C.Var q) = TypedSyntax.eqVId (p, q)
            | sameValue ((_, FSyntax.RecordType fields), C.Unit) =
                Syntax.LabelMap.isEmpty fields
            | sameValue _ = false
        in
          if ListPair.allEq sameValue (params, args) then
            (C.CVarMap.insert (env, name, goCont (env, applied)), acc)
          else
            ( env
            , C.ContDec
                { name = name
                , params = params
                , body = goStat (env, body)
                , attr = attr
                } :: acc
            )
        end
    | C.ContDec {name, params, body, attr} =>
        ( env
        , C.ContDec
            { name = name
            , params = params
            , body = goStat (env, body)
            , attr = attr
            } :: acc
        )
    | C.RecContDec defs =>
        let
          val defs =
            List.map
              (fn (name, params, body) => (name, params, goStat (env, body)))
              defs
        in
          (env, C.RecContDec defs :: acc)
        end
    | C.DatatypeDec _ => (env, dec :: acc)
    | C.ESImportDec _ => (env, dec :: acc)
  and goStat (env, exp) =
    case exp of
      C.Let {decs, cont} =>
        let val (env, revDecs) = List.foldl goDec (env, []) decs
        in C.Let {decs = List.rev revDecs, cont = goStat (env, cont)}
        end
    | C.App {applied, cont, tyArgs, args, attr} =>
        C.App
          { applied = applied
          , cont = goCont (env, cont)
          , tyArgs = tyArgs
          , args = args
          , attr = attr
          }
    | C.AppCont {applied, args} =>
        C.AppCont {applied = goCont (env, applied), args = args}
    | C.If {cond, thenCont, elseCont} =>
        C.If
          { cond = cond
          , thenCont = goStat (env, thenCont)
          , elseCont = goStat (env, elseCont)
          }
    | C.Handle
        {body, handler = (e, h), successfulExitIn, successfulExitOut, resultTy} =>
        C.Handle
          { body = goFunction body
          , handler = (e, goStat (env, h))
          , successfulExitIn = successfulExitIn
          , successfulExitOut = goCont (env, successfulExitOut)
          , resultTy = resultTy
          }
    | C.Raise _ => exp
    | C.Unreachable => exp
  and goFunction exp = goStat (C.CVarMap.empty, exp)
  fun go (_: CpsSimplify.Context, exp) = goFunction exp
end; (* structure CpsEtaConvert *)
