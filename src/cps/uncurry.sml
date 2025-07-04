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
    (*: val tryUncurry : C.SimpleExp -> ((C.Var list) list * C.CVar * C.Stat) option *)
    fun tryUncurry
          (C.Abs
             { contParam
             , params
             , body as
                 C.Let {decs, cont = C.AppCont {applied = k, args = [C.Var v]}}
             , attr = {alwaysInline = false}
             }) =
          (case decs of
             [C.ValDec {exp, results = [SOME f]}] =>
               if contParam = k andalso v = f then
                 case tryUncurry exp of
                   SOME (pp, k, b) => SOME (params :: pp, k, b)
                 | NONE => SOME ([params], contParam, body)
               else
                 SOME ([params], contParam, body)
           | _ => SOME ([params], contParam, body))
      | tryUncurry _ = NONE
    fun doUncurry (ctx, name, exp, acc) =
      case tryUncurry exp of
        SOME (params :: (pp as _ :: _), k, body) =>
          let
            val () = #simplificationOccurred ctx := true
            val workerName = CpsSimplify.renewVId (ctx, name)
            val body = simplifyStat (ctx, body)
            val workerDec = C.ValDec
              { exp = C.Abs
                  { contParam = k
                  , params = params @ List.concat pp
                  , body = body
                  , attr = {alwaysInline = false}
                  }
              , results = [SOME workerName]
              }
            val params' =
              List.map (fn p => CpsSimplify.renewVId (ctx, p)) params
            val pp' =
              List.map (List.map (fn p => CpsSimplify.renewVId (ctx, p))) pp
            fun mkWrapper (k, []) =
                  C.App
                    { applied = C.Var workerName
                    , cont = k
                    , args = List.map C.Var (params' @ List.concat pp')
                    , attr = {}
                    }
              | mkWrapper (k, params :: pp) =
                  let
                    val name = CpsSimplify.renewVId (ctx, name)
                    val l = CpsSimplify.genContSym ctx
                  in
                    C.Let
                      { decs =
                          [C.ValDec
                             { exp = C.Abs
                                 { contParam = l
                                 , params = params
                                 , body = mkWrapper (l, pp)
                                 , attr = {alwaysInline = true}
                                 }
                             , results = [SOME name]
                             }]
                      , cont = C.AppCont {applied = k, args = [C.Var name]}
                      }
                  end
            val l = CpsSimplify.genContSym ctx
          in
            C.ValDec
              { exp = C.Abs
                  { contParam = l
                  , params = params'
                  , body = mkWrapper (l, pp')
                  , attr = {alwaysInline = true}
                  }
              , results = [SOME name]
              } :: workerDec :: acc
          end
      | _ => C.ValDec {exp = exp, results = [SOME name]} :: acc
    and simplifyDec ctx (dec, acc) =
      case dec of
        C.ValDec {exp as C.Abs _, results = [SOME name]} =>
          doUncurry (ctx, name, exp, acc)
      | C.ValDec {exp = C.Abs _, results = [NONE]} => acc
      | C.ValDec {exp = _, results = _} => dec :: acc
      | C.RecDec defs =>
          let
            val defs =
              List.map
                (fn {name, contParam, params, body, attr} =>
                   { name = name
                   , contParam = contParam
                   , params = params
                   , body = simplifyStat (ctx, body)
                   , attr = attr
                   }) defs
          in
            C.RecDec defs :: acc
          end
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
