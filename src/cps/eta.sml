(*
 * Copyright (c) 2024 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure CpsEtaConvert:
sig
  val go: CpsSimplify.Context * CSyntax.CExp -> CSyntax.CExp
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
        {exp = C.Abs {contParam, params, body, attr}, results as [SOME _]} =>
        let
          (* Eta conversion of a function is not implemented yet *)
          val dec' = C.ValDec
            { exp = C.Abs
                { contParam = contParam
                , params = params
                , body = goFunction body
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
              (fn {name, contParam, params, body, attr} =>
                 { name = name
                 , contParam = contParam
                 , params = params
                 , body = goFunction body
                 , attr = attr
                 }) defs
        in
          (env, C.RecDec defs :: acc)
        end
    | C.ContDec {name, params, body as C.AppCont {applied, args}} =>
        (* Eta conversion *)
        if
          ListPair.allEq (fn (SOME p, C.Var q) => p = q | _ => false)
            (params, args)
        then
          (C.CVarMap.insert (env, name, goCont (env, applied)), acc)
        else
          ( env
          , C.ContDec {name = name, params = params, body = goCExp (env, body)}
            :: acc
          )
    | C.ContDec {name, params, body} =>
        ( env
        , C.ContDec {name = name, params = params, body = goCExp (env, body)}
          :: acc
        )
    | C.RecContDec defs =>
        let
          val defs =
            List.map
              (fn (name, params, body) => (name, params, goCExp (env, body)))
              defs
        in
          (env, C.RecContDec defs :: acc)
        end
    | C.ESImportDec _ => (env, dec :: acc)
  and goCExp (env, exp) =
    case exp of
      C.Let {decs, cont} =>
        let val (env, revDecs) = List.foldl goDec (env, []) decs
        in C.Let {decs = List.rev revDecs, cont = goCExp (env, cont)}
        end
    | C.App {applied, cont, args, attr} =>
        C.App
          { applied = applied
          , cont = goCont (env, cont)
          , args = args
          , attr = attr
          }
    | C.AppCont {applied, args} =>
        C.AppCont {applied = goCont (env, applied), args = args}
    | C.If {cond, thenCont, elseCont} =>
        C.If
          { cond = cond
          , thenCont = goCExp (env, thenCont)
          , elseCont = goCExp (env, elseCont)
          }
    | C.Handle {body, handler = (e, h), successfulExitIn, successfulExitOut} =>
        C.Handle
          { body = goFunction body
          , handler = (e, goCExp (env, h))
          , successfulExitIn = successfulExitIn
          , successfulExitOut = goCont (env, successfulExitOut)
          }
    | C.Unreachable => exp
  and goFunction exp = goCExp (C.CVarMap.empty, exp)
  fun go (_: CpsSimplify.Context, exp) = goFunction exp
end; (* structure CpsEtaConvert *)
