(*
 * Copyright (c) 2024 ARATA Mizuki
 * This file is part of LunarML.
 *)
(*
 * Eliminate constant ref-cells.
 *)
local
  structure CpsUsageAnalysis :>
  sig
    datatype frequency = NEVER | ONCE | MANY
    type usage = {ref_read: frequency, ref_write: frequency, other: frequency}
    val neverUsed: usage
    type usage_table
    val getValueUsage: usage_table * TypedSyntax.VId -> usage
    val analyze: CSyntax.Stat -> {usage: usage_table, rec_usage: usage_table}
  end =
  struct
    local structure C = CSyntax
    in
      datatype frequency = NEVER | ONCE | MANY
      fun oneMore NEVER = ONCE
        | oneMore ONCE = MANY
        | oneMore (many as MANY) = many
      type usage = {ref_read: frequency, ref_write: frequency, other: frequency}
      val neverUsed: usage =
        {ref_read = NEVER, ref_write = NEVER, other = NEVER}
      type usage_table = (usage ref) TypedSyntax.VIdTable.hash_table
      fun getValueUsage (table: usage_table, v) =
        case TypedSyntax.VIdTable.find table v of
          SOME r => !r
        | NONE =>
            {ref_read = MANY, ref_write = MANY, other = MANY} (* unknown *)
      fun useValue env v =
        case C.extractVarFromValue v of
          NONE => ()
        | SOME var =>
            case TypedSyntax.VIdTable.find env var of
              SOME r =>
                let
                  val {ref_read, ref_write, other} = !r
                in
                  r
                  :=
                  { ref_read = ref_read
                  , ref_write = ref_write
                  , other = oneMore other
                  }
                end
            | NONE => ()
      fun useValueAsRefRead (env, v) =
        case C.extractVarFromValue v of
          NONE => ()
        | SOME var =>
            case TypedSyntax.VIdTable.find env var of
              SOME r =>
                let
                  val {ref_read, ref_write, other} = !r
                in
                  r
                  :=
                  { ref_read = oneMore ref_read
                  , ref_write = ref_write
                  , other = other
                  }
                end
            | NONE => ()
      fun useValueAsRefWrite (env, v) =
        case C.extractVarFromValue v of
          NONE => ()
        | SOME var =>
            case TypedSyntax.VIdTable.find env var of
              SOME r =>
                let
                  val {ref_read, ref_write, other} = !r
                in
                  r
                  :=
                  { ref_read = ref_read
                  , ref_write = oneMore ref_write
                  , other = other
                  }
                end
            | NONE => ()
      local
        fun add (env, v) =
          if TypedSyntax.VIdTable.inDomain env v then
            raise Fail
              ("goStat: duplicate name in AST: " ^ TypedSyntax.print_VId v)
          else
            TypedSyntax.VIdTable.insert env (v, ref neverUsed)
      in
        fun goSimpleExp
              ( env
              , _
              , C.PrimOp
                  { primOp = FSyntax.PrimCall Primitives.Ref_set
                  , tyargs = _
                  , args = [r, v]
                  }
              ) =
              (useValueAsRefWrite (env, r); useValue env v)
          | goSimpleExp
              ( env
              , _
              , C.PrimOp
                  { primOp = FSyntax.PrimCall Primitives.Ref_read
                  , tyargs = _
                  , args = [r]
                  }
              ) = useValueAsRefRead (env, r)
          | goSimpleExp (env, _, C.PrimOp {primOp = _, tyargs = _, args}) =
              List.app (useValue env) args
          | goSimpleExp (env, _, C.Record fields) =
              Syntax.LabelMap.app (useValue env) fields
          | goSimpleExp (_, _, C.ExnTag {name = _, payloadTy = _}) = ()
          | goSimpleExp
              (env, _, C.Projection {label = _, record, fieldTypes = _}) =
              useValue env record
          | goSimpleExp
              ( env
              , renv
              , C.Abs
                  { contParam = _
                  , tyParams = _
                  , params
                  , body
                  , resultTy = _
                  , attr = _
                  }
              ) =
              ( List.app (fn (p, _) => add (env, p)) params
              ; goStat (env, renv, body)
              )
        and goDec (env, renv) =
          fn C.ValDec {exp, results} =>
            ( goSimpleExp (env, renv, exp)
            ; List.app
                (fn (SOME result, _) => add (env, result) | (NONE, _) => ())
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
                (fn {contParam = _, params, body, ...} =>
                   ( List.app (fn (p, _) => add (env, p)) params
                   ; goStat (env, renv, body)
                   )) defs;
              TypedSyntax.VIdMap.appi
                (fn (f, v) => TypedSyntax.VIdTable.insert renv (f, v))
                recursiveEnv;
              List.app
                (fn {name, ...} =>
                   TypedSyntax.VIdTable.insert env (name, ref neverUsed)) defs
            end
           | C.UnpackDec {tyVar = _, kind = _, vid, payloadTy = _, package = _} =>
            add (env, vid)
           | C.ContDec {name = _, params, body, attr = _} =>
            ( List.app (fn (SOME p, _) => add (env, p) | (NONE, _) => ()) params
            ; goStat (env, renv, body)
            )
           | C.RecContDec defs =>
            List.app
              (fn (_, params, body) =>
                 ( List.app (fn (SOME p, _) => add (env, p) | (NONE, _) => ())
                     params
                 ; goStat (env, renv, body)
                 )) defs
           | C.ESImportDec {pure = _, specs, moduleName = _} =>
            List.app (fn (_, vid, _) => add (env, vid)) specs
        and goStat
          (env: (usage ref) TypedSyntax.VIdTable.hash_table, renv, stat) =
          case stat of
            C.Let {decs, cont} =>
              (List.app (goDec (env, renv)) decs; goStat (env, renv, cont))
          | C.App {applied, cont = _, tyArgs = _, args, attr = _} =>
              (useValue env applied; List.app (useValue env) args)
          | C.AppCont {applied = _, args} => List.app (useValue env) args
          | C.If {cond, thenCont, elseCont} =>
              ( useValue env cond
              ; goStat (env, renv, thenCont)
              ; goStat (env, renv, elseCont)
              )
          | C.Handle
              { body
              , handler = (e, h)
              , successfulExitIn = _
              , successfulExitOut = _
              } =>
              (goStat (env, renv, body); add (env, e); goStat (env, renv, h))
          | C.Raise (_, x) => useValue env x
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
          {usage = usage, rec_usage = rusage}
        end
    end (* local *)
  end (* strucuture CpsUsageAnalysis *)
in
  structure CpsConstantRefCell:
  sig
    val goStat: CpsSimplify.Context * CSyntax.Stat -> CSyntax.Stat
  end =
  struct
    local
      structure F = FSyntax
      structure C = CSyntax
      structure P = Primitives
      datatype frequency = datatype CpsUsageAnalysis.frequency
    in
      type Context =
        { base: CpsSimplify.Context
        , usage: CpsUsageAnalysis.usage_table
        , rec_usage: CpsUsageAnalysis.usage_table
        }
      datatype simplify_result =
        VALUE of C.Value
      | SIMPLE_EXP of C.SimpleExp
      | NOT_SIMPLIFIED
      (*: val simplifySimpleExp : CpsUsageAnalysis.usage_table * { exp : C.SimpleExp option } TypedSyntax.VIdMap.map * C.SimpleExp -> simplify_result *)
      fun simplifySimpleExp (usage, env, C.PrimOp {primOp, tyargs = _, args}) =
            (case (primOp, args) of
               (F.PrimCall P.Ref_read, [C.Var v]) =>
                 let
                   val u = CpsUsageAnalysis.getValueUsage (usage, v)
                 in
                   case (#ref_write u, #other u) of
                     (CpsUsageAnalysis.NEVER, CpsUsageAnalysis.NEVER) =>
                       (case TypedSyntax.VIdMap.find (env, v) of
                          SOME
                            {exp =
                               SOME
                                 (C.PrimOp
                                    { primOp = F.PrimCall P.Ref_ref
                                    , tyargs = _
                                    , args = [initialValue]
                                    })} => VALUE initialValue
                        | _ => NOT_SIMPLIFIED)
                   | _ => NOT_SIMPLIFIED
                 end
             | _ => NOT_SIMPLIFIED)
        | simplifySimpleExp (_, _, _) = NOT_SIMPLIFIED
      and simplifyDec (ctx: Context) (dec, (env, subst, acc: C.Dec list)) =
        case dec of
          C.ValDec {exp, results} =>
            let
              val exp =
                CpsSimplify.substSimpleExp
                  (TypedSyntax.TyVarMap.empty, subst, C.CVarMap.empty, exp)
            in
              case simplifySimpleExp (#usage ctx, env, exp) of
                VALUE v =>
                  let
                    val () = #simplificationOccurred (#base ctx) := true
                    val subst =
                      case results of
                        [(SOME result, _)] =>
                          TypedSyntax.VIdMap.insert (subst, result, v)
                      | [(NONE, _)] => subst
                      | _ => subst (* should not occur *)
                  in
                    (env, subst, acc)
                  end
              | simplified =>
                  let
                    val () =
                      case simplified of
                        SIMPLE_EXP _ =>
                          #simplificationOccurred (#base ctx) := true
                      | VALUE _ =>
                          #simplificationOccurred (#base ctx)
                          := true (* shoud not occur *)
                      | NOT_SIMPLIFIED => ()
                    val exp =
                      case simplified of
                        SIMPLE_EXP exp => exp
                      | _ => exp
                  in
                    case (exp, results) of
                      ( C.Abs
                          {contParam, tyParams, params, body, resultTy, attr}
                      , [(SOME result, ty)]
                      ) =>
                        let
                          val body = simplifyStat (ctx, env, subst, body)
                          val exp = C.Abs
                            { contParam = contParam
                            , tyParams = tyParams
                            , params = params
                            , body = body
                            , resultTy = resultTy
                            , attr = attr
                            }
                          val env =
                            TypedSyntax.VIdMap.insert
                              (env, result, {exp = NONE})
                          val dec = C.ValDec
                            {exp = exp, results = [(SOME result, ty)]}
                        in
                          (env, subst, dec :: acc)
                        end
                    | _ =>
                        (case (C.isDiscardable exp, results) of
                           (true, [(NONE, _)]) => (env, subst, acc)
                         | (_, [(SOME result, ty)]) =>
                             let
                               val dec = C.ValDec
                                 {exp = exp, results = [(SOME result, ty)]}
                               val env = TypedSyntax.VIdMap.insert
                                 (env, result, {exp = SOME exp})
                             in
                               (env, subst, dec :: acc)
                             end
                         | _ =>
                             let
                               val dec = C.ValDec {exp = exp, results = results}
                             in
                               (env, subst, dec :: acc)
                             end)
                  end
            end
        | C.RecDec defs =>
            let
              val defs =
                List.map
                  (fn {name, contParam, tyParams, params, body, resultTy, attr} =>
                     { name = name
                     , contParam = contParam
                     , tyParams = tyParams
                     , params = params
                     , body = simplifyStat (ctx, env, subst, body)
                     , resultTy = resultTy
                     , attr = attr
                     }) defs
              val decs = C.RecDec defs :: acc
            in
              (env, subst, decs)
            end
        | C.UnpackDec _ => (env, subst, dec :: acc)
        | C.ContDec {name, params, body, attr} =>
            let
              val body = simplifyStat (ctx, env, subst, body)
              val dec = C.ContDec
                {name = name, params = params, body = body, attr = attr}
            in
              (env, subst, dec :: acc)
            end
        | C.RecContDec defs =>
            let
              val dec = C.RecContDec
                (List.map
                   (fn (name, params, body) =>
                      (name, params, simplifyStat (ctx, env, subst, body))) defs)
            in
              (env, subst, dec :: acc)
            end
        | C.ESImportDec _ => (env, subst, dec :: acc)
      and simplifyStat
        ( ctx: Context
        , env: {exp: CSyntax.SimpleExp option} TypedSyntax.VIdMap.map
        , subst: C.Value TypedSyntax.VIdMap.map
        , e
        ) =
        case e of
          C.Let {decs, cont} =>
            let
              val (env, subst, revDecs) =
                List.foldl (simplifyDec ctx) (env, subst, []) decs
            in
              CpsTransform.prependRevDecs
                (revDecs, simplifyStat (ctx, env, subst, cont))
            end
        | C.App {applied, cont, tyArgs, args, attr} =>
            let
              val substValue =
                CpsSimplify.substValue (TypedSyntax.TyVarMap.empty, subst)
              val applied = substValue applied
              val args = List.map substValue args
            in
              C.App
                { applied = applied
                , cont = cont
                , tyArgs = tyArgs
                , args = args
                , attr = attr
                }
            end
        | C.AppCont {applied, args} =>
            let
              val args =
                List.map
                  (CpsSimplify.substValue (TypedSyntax.TyVarMap.empty, subst))
                  args
            in
              C.AppCont {applied = applied, args = args}
            end
        | C.If {cond, thenCont, elseCont} =>
            C.If
              { cond = cond
              , thenCont = simplifyStat (ctx, env, subst, thenCont)
              , elseCont = simplifyStat (ctx, env, subst, elseCont)
              }
        | C.Handle {body, handler = (e, h), successfulExitIn, successfulExitOut} =>
            C.Handle
              { body = simplifyStat (ctx, env, subst, body)
              , handler = (e, simplifyStat (ctx, env, subst, h))
              , successfulExitIn = successfulExitIn
              , successfulExitOut = successfulExitOut
              }
        | C.Raise _ => e
        | C.Unreachable => e
      fun goStat (ctx: CpsSimplify.Context, exp) =
        let
          val usage = CpsUsageAnalysis.analyze exp
          val ctx' =
            {base = ctx, usage = #usage usage, rec_usage = #rec_usage usage}
        in
          simplifyStat
            (ctx', TypedSyntax.VIdMap.empty, TypedSyntax.VIdMap.empty, exp)
        end
    end (* local *)
  end (* structure CpsConstantRefCell *)
end; (* local *)
