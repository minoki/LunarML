(*
 * Copyright (c) 2024 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure CpsUncurry : sig
              val goCExp : CpsSimplify.Context * CSyntax.CExp -> CSyntax.CExp
          end = struct
local structure F = FSyntax
      structure C = CSyntax
      structure P = Primitives
in
(*: val tryUncurry : C.SimpleExp -> ((C.Var list) list * C.CVar * C.CExp) option *)
fun tryUncurry (exp as C.Abs { contParam, params, body as C.Let { decs, cont = C.AppCont { applied = k, args = [C.Var v] } }, attr = { isWrapper = false } })
    = if Vector.length decs = 1 andalso contParam = k then
          case Vector.sub (decs, 0) of
              C.ValDec { exp, result = SOME f } =>
              if v = f then
                  case tryUncurry exp of
                      SOME (pp, k, b) => SOME (params :: pp, k, b)
                    | NONE => SOME ([params], contParam, body)
              else
                  SOME ([params], contParam, body)
            | _ => SOME ([params], contParam, body)
      else
          SOME ([params], contParam, body)
  | tryUncurry _ = NONE
fun doUncurry (ctx, name, exp, acc)
    = case tryUncurry exp of
          SOME (params :: (pp as _ :: _), k, body) =>
          let val workerName = CpsSimplify.renewVId (ctx, name)
              val body = simplifyCExp (ctx, body)
              val workerDec = C.ValDec { exp = C.Abs { contParam = k, params = params @ List.concat pp, body = body, attr = { isWrapper = false } }, result = SOME workerName }
              val params' = List.map (fn p => CpsSimplify.renewVId (ctx, p)) params
              val pp' = List.map (List.map (fn p => CpsSimplify.renewVId (ctx, p))) pp
              fun mkWrapper (k, []) = C.App { applied = C.Var workerName, cont = k, args = List.map C.Var (params' @ List.concat pp') }
                | mkWrapper (k, params :: pp) = let val name = CpsSimplify.renewVId (ctx, name)
                                                    val l = CpsSimplify.genContSym ctx
                                                in C.Let { decs = vector [C.ValDec { exp = C.Abs { contParam = l, params = params, body = mkWrapper (l, pp), attr = { isWrapper = true } }, result = SOME name }]
                                                         , cont = C.AppCont { applied = k, args = [C.Var name] }
                                                         }
                                                end
              val l = CpsSimplify.genContSym ctx
          in C.ValDec { exp = C.Abs { contParam = l, params = params', body = mkWrapper (l, pp'), attr = { isWrapper = true } }, result = SOME name } :: workerDec :: acc
          end
        | _ => C.ValDec { exp = exp, result = SOME name } :: acc
and simplifyDec ctx (dec, acc)
    = case dec of
          C.ValDec { exp as C.Abs _, result = SOME name } =>
          doUncurry (ctx, name, exp, acc)
        | C.ValDec { exp = C.Abs _, result = NONE } => acc
        | C.ValDec { exp = _, result = _ } => dec :: acc
        | C.RecDec defs =>
          let val defs = List.map (fn { name, contParam, params, body, attr } =>
                                      { name = name
                                      , contParam = contParam
                                      , params = params
                                      , body = simplifyCExp (ctx, body)
                                      , attr = attr
                                      }
                                  ) defs
          in C.RecDec defs :: acc
          end
        | C.ContDec { name, params, body } => C.ContDec { name = name, params = params, body = simplifyCExp (ctx, body) } :: acc
        | C.RecContDec defs =>
          let val defs = List.map (fn (name, params, body) => (name, params, simplifyCExp (ctx, body))) defs
          in C.RecContDec defs :: acc
          end
        | C.ESImportDec _ => dec :: acc
and simplifyCExp (ctx : CpsSimplify.Context, exp)
    = case exp of
          C.Let { decs, cont } => C.Let { decs = vector (List.rev (Vector.foldl (simplifyDec ctx) [] decs)), cont = simplifyCExp (ctx, cont) }
        | C.App _ => exp
        | C.AppCont _ => exp
        | C.If { cond, thenCont, elseCont } => C.If { cond = cond
                                                    , thenCont = simplifyCExp (ctx, thenCont)
                                                    , elseCont = simplifyCExp (ctx, elseCont)
                                                    }
        | C.Handle { body, handler = (e, h), successfulExitIn, successfulExitOut } =>
          C.Handle { body = simplifyCExp (ctx, body)
                   , handler = (e, simplifyCExp (ctx, h))
                   , successfulExitIn = successfulExitIn
                   , successfulExitOut = successfulExitOut
                   }
        | C.Unreachable => exp
val goCExp = simplifyCExp
end (* local *)
end; (* structure CpsUncurry *)
