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
    val analyze: CSyntax.CExp -> {usage: usage_table, rec_usage: usage_table}
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
      fun useValue env (C.Var v) =
            (case TypedSyntax.VIdTable.find env v of
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
             | NONE => ())
        | useValue _ C.Unit = ()
        | useValue _ C.Nil = ()
        | useValue _ (C.BoolConst _) = ()
        | useValue _ (C.IntConst _) = ()
        | useValue _ (C.WordConst _) = ()
        | useValue _ (C.CharConst _) = ()
        | useValue _ (C.Char16Const _) = ()
        | useValue _ (C.StringConst _) = ()
        | useValue _ (C.String16Const _) = ()
      fun useValueAsRefRead (env, C.Var v) =
            (case TypedSyntax.VIdTable.find env v of
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
             | NONE => ())
        | useValueAsRefRead (_, C.Unit) = ()
        | useValueAsRefRead (_, C.Nil) = ()
        | useValueAsRefRead (_, C.BoolConst _) = ()
        | useValueAsRefRead (_, C.IntConst _) = ()
        | useValueAsRefRead (_, C.WordConst _) = ()
        | useValueAsRefRead (_, C.CharConst _) = ()
        | useValueAsRefRead (_, C.Char16Const _) = ()
        | useValueAsRefRead (_, C.StringConst _) = ()
        | useValueAsRefRead (_, C.String16Const _) = ()
      fun useValueAsRefWrite (env, C.Var v) =
            (case TypedSyntax.VIdTable.find env v of
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
             | NONE => ())
        | useValueAsRefWrite (_, C.Unit) = ()
        | useValueAsRefWrite (_, C.Nil) = ()
        | useValueAsRefWrite (_, C.BoolConst _) = ()
        | useValueAsRefWrite (_, C.IntConst _) = ()
        | useValueAsRefWrite (_, C.WordConst _) = ()
        | useValueAsRefWrite (_, C.CharConst _) = ()
        | useValueAsRefWrite (_, C.Char16Const _) = ()
        | useValueAsRefWrite (_, C.StringConst _) = ()
        | useValueAsRefWrite (_, C.String16Const _) = ()
      local
        fun add (env, v) =
          if TypedSyntax.VIdTable.inDomain env v then
            raise Fail
              ("goCExp: duplicate name in AST: " ^ TypedSyntax.print_VId v)
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
              (env, renv, C.Abs {contParam = _, params, body, attr = _}) =
              (List.app (fn p => add (env, p)) params; goCExp (env, renv, body))
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
                (fn {contParam = _, params, body, ...} =>
                   ( List.app (fn p => add (env, p)) params
                   ; goCExp (env, renv, body)
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
            ; goCExp (env, renv, body)
            )
           | C.RecContDec defs =>
            List.app
              (fn (_, params, body) =>
                 ( List.app (Option.app (fn p => add (env, p))) params
                 ; goCExp (env, renv, body)
                 )) defs
           | C.ESImportDec {pure = _, specs, moduleName = _} =>
            List.app (fn (_, vid) => add (env, vid)) specs
        and goCExp
          (env: (usage ref) TypedSyntax.VIdTable.hash_table, renv, cexp) =
          case cexp of
            C.Let {decs, cont} =>
              (List.app (goDec (env, renv)) decs; goCExp (env, renv, cont))
          | C.App {applied, cont = _, args, attr = _} =>
              (useValue env applied; List.app (useValue env) args)
          | C.AppCont {applied = _, args} => List.app (useValue env) args
          | C.If {cond, thenCont, elseCont} =>
              ( useValue env cond
              ; goCExp (env, renv, thenCont)
              ; goCExp (env, renv, elseCont)
              )
          | C.Handle
              { body
              , handler = (e, h)
              , successfulExitIn = _
              , successfulExitOut = _
              } =>
              (goCExp (env, renv, body); add (env, e); goCExp (env, renv, h))
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
          goCExp (usage, rusage, exp);
          {usage = usage, rec_usage = rusage}
        end
    end (* local *)
  end (* strucuture CpsUsageAnalysis *)
in
  structure CpsConstantRefCell:
  sig
    val goCExp: CpsSimplify.Context * CSyntax.CExp -> CSyntax.CExp
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
              val exp = CpsSimplify.substSimpleExp (subst, C.CVarMap.empty, exp)
            in
              case simplifySimpleExp (#usage ctx, env, exp) of
                VALUE v =>
                  let
                    val () = #simplificationOccurred (#base ctx) := true
                    val subst =
                      case results of
                        [SOME result] =>
                          TypedSyntax.VIdMap.insert (subst, result, v)
                      | [NONE] => subst
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
                      (C.Abs {contParam, params, body, attr}, [SOME result]) =>
                        let
                          val body = simplifyCExp (ctx, env, subst, body)
                          val exp = C.Abs
                            { contParam = contParam
                            , params = params
                            , body = body
                            , attr = attr
                            }
                          val env =
                            TypedSyntax.VIdMap.insert
                              (env, result, {exp = NONE})
                          val dec = C.ValDec
                            {exp = exp, results = [SOME result]}
                        in
                          (env, subst, dec :: acc)
                        end
                    | _ =>
                        (case (C.isDiscardable exp, results) of
                           (true, [NONE]) => (env, subst, acc)
                         | (_, [SOME result]) =>
                             let
                               val dec = C.ValDec
                                 {exp = exp, results = [SOME result]}
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
                  (fn {name, contParam, params, body, attr} =>
                     { name = name
                     , contParam = contParam
                     , params = params
                     , body = simplifyCExp (ctx, env, subst, body)
                     , attr = attr
                     }) defs
              val decs = C.RecDec defs :: acc
            in
              (env, subst, decs)
            end
        | C.ContDec {name, params, body, attr} =>
            let
              val body = simplifyCExp (ctx, env, subst, body)
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
                      (name, params, simplifyCExp (ctx, env, subst, body))) defs)
            in
              (env, subst, dec :: acc)
            end
        | C.ESImportDec _ => (env, subst, dec :: acc)
      and simplifyCExp
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
                (revDecs, simplifyCExp (ctx, env, subst, cont))
            end
        | C.App {applied, cont, args, attr} =>
            let
              val applied = CpsSimplify.substValue subst applied
              val args = List.map (CpsSimplify.substValue subst) args
            in
              C.App {applied = applied, cont = cont, args = args, attr = attr}
            end
        | C.AppCont {applied, args} =>
            let val args = List.map (CpsSimplify.substValue subst) args
            in C.AppCont {applied = applied, args = args}
            end
        | C.If {cond, thenCont, elseCont} =>
            C.If
              { cond = cond
              , thenCont = simplifyCExp (ctx, env, subst, thenCont)
              , elseCont = simplifyCExp (ctx, env, subst, elseCont)
              }
        | C.Handle {body, handler = (e, h), successfulExitIn, successfulExitOut} =>
            C.Handle
              { body = simplifyCExp (ctx, env, subst, body)
              , handler = (e, simplifyCExp (ctx, env, subst, h))
              , successfulExitIn = successfulExitIn
              , successfulExitOut = successfulExitOut
              }
        | C.Raise _ => e
        | C.Unreachable => e
      fun goCExp (ctx: CpsSimplify.Context, exp) =
        let
          val usage = CpsUsageAnalysis.analyze exp
          val ctx' =
            {base = ctx, usage = #usage usage, rec_usage = #rec_usage usage}
        in
          simplifyCExp
            (ctx', TypedSyntax.VIdMap.empty, TypedSyntax.VIdMap.empty, exp)
        end
    end (* local *)
  end (* structure CpsConstantRefCell *)
end; (* local *)
