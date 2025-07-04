(*
 * Copyright (c) 2024 ARATA Mizuki
 * This file is part of LunarML.
 *)
(*
 * Convert recursive function to loop,
 *)
local
  structure CpsUsageAnalysis :>
  sig
    type usage = {returnConts: CSyntax.CVarSet.set}
    val neverUsed: usage
    type usage_table
    val getValueUsage: usage_table * TypedSyntax.VId -> usage
    val analyze: CSyntax.Stat -> {rec_usage: usage_table}
  end =
  struct
    local structure C = CSyntax
    in
      type usage = {returnConts: CSyntax.CVarSet.set}
      val neverUsed: usage = {returnConts = CSyntax.CVarSet.empty}
      type usage_table = (usage ref) TypedSyntax.VIdTable.hash_table
      fun getValueUsage (table: usage_table, v) =
        case TypedSyntax.VIdTable.find table v of
          SOME r => !r
        | NONE => {returnConts = CSyntax.CVarSet.empty} (* unknown *)
      fun useValueAsCallee (env, cont, C.Var v) =
            (case TypedSyntax.VIdTable.find env v of
               SOME r =>
                 let val {returnConts} = !r
                 in r := {returnConts = C.CVarSet.add (returnConts, cont)}
                 end
             | NONE => ())
        | useValueAsCallee (_, _, C.Unit) = ()
        | useValueAsCallee (_, _, C.Nil) = ()
        | useValueAsCallee (_, _, C.BoolConst _) = ()
        | useValueAsCallee (_, _, C.IntConst _) = ()
        | useValueAsCallee (_, _, C.WordConst _) = ()
        | useValueAsCallee (_, _, C.CharConst _) = ()
        | useValueAsCallee (_, _, C.Char16Const _) = ()
        | useValueAsCallee (_, _, C.StringConst _) = ()
        | useValueAsCallee (_, _, C.String16Const _) = ()
      local
        fun add (env, v) =
          if TypedSyntax.VIdTable.inDomain env v then
            raise Fail
              ("goStat: duplicate name in AST: " ^ TypedSyntax.print_VId v)
          else
            TypedSyntax.VIdTable.insert env (v, ref neverUsed)
      in
        fun goSimpleExp (_, _, C.PrimOp _) = ()
          | goSimpleExp (_, _, C.Record _) = ()
          | goSimpleExp (_, _, C.ExnTag _) = ()
          | goSimpleExp (_, _, C.Projection _) = ()
          | goSimpleExp
              (env, renv, C.Abs {contParam = _, params, body, attr = _}) =
              (List.app (fn p => add (env, p)) params; goStat (env, renv, body))
        and goDec (env, renv) =
          fn C.ValDec {exp, results} =>
            ( goSimpleExp (env, renv, exp)
            ; List.app (fn SOME result => add (env, result) | NONE => ())
                results
            )
           | C.RecDec defs =>
            let
              val recursiveEnv =
                List.foldl
                  (fn ({name, ...}, m) =>
                     TypedSyntax.VIdMap.insert (m, name, ref neverUsed))
                  TypedSyntax.VIdMap.empty defs
            in
              TypedSyntax.VIdMap.appi
                (fn (f, v) => TypedSyntax.VIdTable.insert env (f, v))
                recursiveEnv;
              List.app
                (fn {params, body, ...} =>
                   ( List.app (fn p => add (env, p)) params
                   ; goStat (env, renv, body)
                   )) defs;
              TypedSyntax.VIdMap.appi
                (fn (f, v) => TypedSyntax.VIdTable.insert renv (f, v))
                recursiveEnv;
              List.app
                (fn {name, ...} =>
                   TypedSyntax.VIdTable.insert env (name, ref neverUsed)) defs
            end
           | C.ContDec {name = _, params, body, attr = _} =>
            ( List.app (Option.app (fn p => add (env, p))) params
            ; goStat (env, renv, body)
            )
           | C.RecContDec defs =>
            List.app
              (fn (_, params, body) =>
                 ( List.app (Option.app (fn p => add (env, p))) params
                 ; goStat (env, renv, body)
                 )) defs
           | C.ESImportDec {pure = _, specs, moduleName = _} =>
            List.app (fn (_, vid) => add (env, vid)) specs
        and goStat
          (env: (usage ref) TypedSyntax.VIdTable.hash_table, renv, stat) =
          case stat of
            C.Let {decs, cont} =>
              (List.app (goDec (env, renv)) decs; goStat (env, renv, cont))
          | C.App {applied, cont, args = _, attr = _} =>
              (useValueAsCallee (env, cont, applied))
          | C.AppCont {applied = _, args = _} => ()
          | C.If {cond = _, thenCont, elseCont} =>
              (goStat (env, renv, thenCont); goStat (env, renv, elseCont))
          | C.Handle
              { body
              , handler = (e, h)
              , successfulExitIn = _
              , successfulExitOut = _
              } =>
              (goStat (env, renv, body); add (env, e); goStat (env, renv, h))
          | C.Raise _ => ()
          | C.Unreachable => ()
      end (* local *)
      fun analyze exp =
        let
          val usage =
            TypedSyntax.VIdTable.mkTable (1, Fail "usage table lookup failed")
          val rusage =
            TypedSyntax.VIdTable.mkTable (1, Fail "rusage table lookup failed")
        in
          goStat (usage, rusage, exp);
          {rec_usage = rusage}
        end
    end (* local *)
  end (* strucuture CpsUsageAnalysis *)
in
  structure CpsLoopOptimization:
  sig
    val goStat: CpsSimplify.Context * CSyntax.Stat -> CSyntax.Stat
  end =
  struct
    local structure C = CSyntax
    in
      type Context =
        {base: CpsSimplify.Context, rec_usage: CpsUsageAnalysis.usage_table}
      fun simplifyDec (ctx: Context) (dec, acc: C.Dec list) =
        case dec of
          C.ValDec {exp, results} =>
            (case (exp, results) of
               (C.Abs {contParam, params, body, attr}, results) =>
                 let
                   val body = simplifyStat (ctx, body)
                   val exp = C.Abs
                     { contParam = contParam
                     , params = params
                     , body = body
                     , attr = attr
                     }
                   val dec = C.ValDec {exp = exp, results = results}
                 in
                   dec :: acc
                 end
             | _ =>
                 let val dec = C.ValDec {exp = exp, results = results}
                 in dec :: acc
                 end)
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
              fun tryConvertToLoop
                (def as {name, contParam, params, body, attr}) =
                if
                  C.CVarSet.member
                    ( #returnConts
                        (CpsUsageAnalysis.getValueUsage (#rec_usage ctx, name))
                    , contParam
                    )
                then
                  let
                    val loop = CpsSimplify.genContSym (#base ctx)
                    val params' =
                      List.map (fn v => CpsSimplify.renewVId (#base ctx, v))
                        params
                    val body' =
                      C.recurseStat
                        (fn call as
                             C.App
                               {applied = C.Var applied, cont, args, attr = _} =>
                           if applied = name andalso cont = contParam then
                             C.AppCont {applied = loop, args = args}
                           else
                             call
                          | c => c) body
                    val body'' = C.Let
                      { decs =
                          [C.RecContDec [(loop, List.map SOME params, body')]]
                      , cont = C.AppCont
                          {applied = loop, args = List.map C.Var params'}
                      }
                  in
                    { name = name
                    , contParam = contParam
                    , params = params'
                    , body = body''
                    , attr = attr
                    }
                  end
                else
                  def
            in
              C.RecDec (List.map tryConvertToLoop defs) :: acc
            end
        | C.ContDec {name, params, body, attr} =>
            let
              val dec = C.ContDec
                { name = name
                , params = params
                , body = simplifyStat (ctx, body)
                , attr = attr
                }
            in
              dec :: acc
            end
        | C.RecContDec defs =>
            let
              val dec = C.RecContDec
                (List.map
                   (fn (name, params, body) =>
                      (name, params, simplifyStat (ctx, body))) defs)
            in
              dec :: acc
            end
        | C.ESImportDec _ => dec :: acc
      and simplifyStat (ctx: Context, e) =
        case e of
          C.Let {decs, cont} =>
            let val revDecs = List.foldl (simplifyDec ctx) [] decs
            in CpsTransform.prependRevDecs (revDecs, simplifyStat (ctx, cont))
            end
        | C.App _ => e
        | C.AppCont _ => e
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
        | C.Raise _ => e
        | C.Unreachable => e
      fun goStat (ctx: CpsSimplify.Context, exp) =
        let
          val usage = CpsUsageAnalysis.analyze exp
          val ctx' = {base = ctx, rec_usage = #rec_usage usage}
        in
          simplifyStat (ctx', exp)
        end
    end (* local *)
  end (* structure CpsLoopOptimization *)
end; (* local *)
