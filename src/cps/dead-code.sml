(*
 * Copyright (c) 2024 ARATA Mizuki
 * This file is part of LunarML.
 *)
(*
 * Dead code elimination
 *)
local
  structure CpsDeadCodeAnalysis :>
  sig
    type usage
    val analyze: CSyntax.Stat -> usage
    val isUsed: usage * TypedSyntax.VId -> bool
  end =
  struct
    local structure C = CSyntax
    in
      type graph = TypedSyntax.VIdSet.set TypedSyntax.VIdTable.hash_table
      type usage = bool TypedSyntax.VIdTable.hash_table
      fun addValue (v, set) =
        case C.extractVarFromValue v of
          SOME var => TypedSyntax.VIdSet.add (set, var)
        | NONE => set
      fun goSimpleExp (_, C.PrimOp {primOp = _, tyargs = _, args}) =
            List.foldl addValue TypedSyntax.VIdSet.empty args
        | goSimpleExp (_, C.Record fields) =
            Syntax.LabelMap.foldl addValue TypedSyntax.VIdSet.empty fields
        | goSimpleExp (_, C.ExnTag {name = _, payloadTy = _}) =
            TypedSyntax.VIdSet.empty
        | goSimpleExp (_, C.Projection {label = _, record, fieldTypes = _}) =
            addValue (record, TypedSyntax.VIdSet.empty)
        | goSimpleExp
            ( g
            , C.Abs
                { contParam = _
                , tyParams = _
                , params = _
                , body
                , resultTy = _
                , attr = _
                }
            ) =
            goExp
              (g, body, TypedSyntax.VIdSet.empty) (* What to do with params? *)
      and goDec g (C.ValDec {exp, results}, acc) =
            let
              val s = goSimpleExp (g, exp)
            in
              List.app
                (fn (SOME r, _) => TypedSyntax.VIdTable.insert g (r, s)
                  | (NONE, _) => ()) results;
              if C.isDiscardable exp then acc
              else TypedSyntax.VIdSet.union (acc, s)
            end
        | goDec g (C.RecDec defs, acc) =
            let
              val s =
                List.foldl (fn ({body, ...}, acc) => goExp (g, body, acc)) acc
                  defs
            in
              List.app
                (fn {name, ...} => TypedSyntax.VIdTable.insert g (name, s)) defs;
              acc
            end
        | goDec g
            ( C.UnpackDec {tyVar = _, kind = _, vid, unpackedTy = _, package}
            , acc
            ) =
            let
              val s = addValue (package, TypedSyntax.VIdSet.empty)
            in
              TypedSyntax.VIdTable.insert g (vid, s);
              TypedSyntax.VIdSet.union (acc, s) (* TODO: just return acc *)
            end
        | goDec g (C.ContDec {name = _, params = _, body, attr = _}, acc) =
            goExp (g, body, acc)
        | goDec g (C.RecContDec defs, acc) =
            List.foldl (fn ((_, _, body), acc) => goExp (g, body, acc)) acc defs
        | goDec _ (C.DatatypeDec _, acc) = acc
        | goDec g (C.ESImportDec {pure = _, specs, moduleName = _}, acc) =
            ( List.app
                (fn (_, vid, _) =>
                   TypedSyntax.VIdTable.insert g (vid, TypedSyntax.VIdSet.empty))
                specs
            ; acc
            )
      and goExp (g, C.Let {decs, cont}, acc) =
            goExp (g, cont, List.foldl (goDec g) acc decs)
        | goExp (_, C.App {applied, cont = _, tyArgs = _, args, attr = _}, acc) =
            List.foldl addValue (addValue (applied, acc)) args
        | goExp (_, C.AppCont {applied = _, args}, acc) =
            List.foldl addValue acc args
        | goExp (g, C.If {cond, thenCont, elseCont}, acc) =
            goExp (g, elseCont, goExp (g, thenCont, addValue (cond, acc)))
        | goExp
            ( g
            , C.Handle
                { body
                , handler = (_, h)
                , successfulExitIn = _
                , successfulExitOut = _
                , resultTy = _
                }
            , acc
            ) =
            goExp (g, body, goExp (g, h, acc))
        | goExp (_, C.Raise (_, x), acc) = addValue (x, acc)
        | goExp (_, C.Unreachable, acc) = acc
      (*: val makeGraph : CSyntax.Stat -> graph * TypedSyntax.VIdSet.set *)
      fun makeGraph program =
        let
          val g = TypedSyntax.VIdTable.mkTable (1, Fail
            "dead code analysis table lookup failed")
        in
          (g, goExp (g, program, TypedSyntax.VIdSet.empty))
        end
      fun analyze program =
        let
          val (g, root) = makeGraph program
          val usage = TypedSyntax.VIdTable.mkTable (1, Fail
            "dead code analysis table lookup failed")
          fun go vid =
            case TypedSyntax.VIdTable.find usage vid of
              SOME true => ()
            | _ =>
                ( TypedSyntax.VIdTable.insert usage (vid, true)
                ; case TypedSyntax.VIdTable.find g vid of
                    SOME set => TypedSyntax.VIdSet.app go set
                  | NONE => ()
                )
        in
          TypedSyntax.VIdSet.app go root;
          usage
        end
      fun isUsed (usage, vid) =
        case TypedSyntax.VIdTable.find usage vid of
          SOME true => true
        | _ => false
    end (* local *)
  end (* structure CpsDeadCodeAnalysis *)

  structure CpsUsageAnalysis :>
  sig
    datatype frequency = NEVER | ONCE | MANY
    type usage = {call: frequency, other: frequency}
    type cont_usage = {direct: frequency, indirect: frequency}
    val neverUsed: usage
    val neverUsedCont: cont_usage
    type usage_table
    type cont_usage_table
    val getValueUsage: usage_table * TypedSyntax.VId -> usage option
    val getContUsage: cont_usage_table * CSyntax.CVar -> cont_usage option
    val analyze:
      CSyntax.Stat
      -> { usage: usage_table
         , rec_usage: usage_table
         , cont_usage: cont_usage_table
         , cont_rec_usage: cont_usage_table
         , dead_code_analysis: CpsDeadCodeAnalysis.usage
         }
  end =
  struct
    local structure C = CSyntax
    in
      datatype frequency = NEVER | ONCE | MANY
      fun oneMore NEVER = ONCE
        | oneMore ONCE = MANY
        | oneMore (many as MANY) = many
      type usage = {call: frequency, other: frequency}
      type cont_usage = {direct: frequency, indirect: frequency}
      val neverUsed: usage = {call = NEVER, other = NEVER}
      val neverUsedCont: cont_usage = {direct = NEVER, indirect = NEVER}
      type usage_table = (usage ref) TypedSyntax.VIdTable.hash_table
      type cont_usage_table = (cont_usage ref) CSyntax.CVarTable.hash_table
      fun getValueUsage (table: usage_table, v) =
        case TypedSyntax.VIdTable.find table v of
          SOME r => SOME (!r)
        | NONE => NONE (* unknown *)
      fun getContUsage (table: cont_usage_table, c) =
        case CSyntax.CVarTable.find table c of
          SOME r => SOME (!r)
        | NONE => NONE (* unknown *)
      fun useValue env v =
        case C.extractVarFromValue v of
          NONE => ()
        | SOME var =>
            case TypedSyntax.VIdTable.find env var of
              SOME r =>
                let val {call, other} = !r
                in r := {call = call, other = oneMore other}
                end
            | NONE => ()
      fun useValueAsCallee (env, v) =
        case C.extractVarFromValue v of
          NONE => ()
        | SOME var =>
            case TypedSyntax.VIdTable.find env var of
              SOME r =>
                let val {call, other} = !r
                in r := {call = oneMore call, other = other}
                end
            | NONE => ()
      fun useContVarIndirect cenv (v: C.CVar) =
        (case C.CVarTable.find cenv v of
           SOME r =>
             let val {direct, indirect} = !r
             in r := {direct = direct, indirect = oneMore indirect}
             end
         | NONE => ())
      fun useContVarDirect cenv (v: C.CVar) =
        (case C.CVarTable.find cenv v of
           SOME r =>
             let val {direct, indirect} = !r
             in r := {direct = oneMore direct, indirect = indirect}
             end
         | NONE => ())
      local
        fun add (env, v) =
          if TypedSyntax.VIdTable.inDomain env v then
            raise Fail
              ("goStat: duplicate name in AST: " ^ TypedSyntax.print_VId v)
          else
            TypedSyntax.VIdTable.insert env (v, ref neverUsed)
        fun addC (cenv, v) =
          if C.CVarTable.inDomain cenv v then
            raise Fail
              ("goStat: duplicate continuation name in AST: "
               ^ Int.toString (C.CVar.toInt v))
          else
            C.CVarTable.insert cenv (v, ref neverUsedCont)
      in
        fun goSimpleExp (env, _, _, _, C.PrimOp {primOp = _, tyargs = _, args}) =
              List.app (useValue env) args
          | goSimpleExp (env, _, _, _, C.Record fields) =
              Syntax.LabelMap.app (useValue env) fields
          | goSimpleExp (_, _, _, _, C.ExnTag {name = _, payloadTy = _}) = ()
          | goSimpleExp
              (env, _, _, _, C.Projection {label = _, record, fieldTypes = _}) =
              useValue env record
          | goSimpleExp
              ( env
              , renv
              , cenv
              , crenv
              , C.Abs
                  { contParam
                  , tyParams = _
                  , params
                  , body
                  , resultTy = _
                  , attr = _
                  }
              ) =
              ( List.app (fn (p, _) => add (env, p)) params
              ; addC (cenv, contParam)
              ; goStat (env, renv, cenv, crenv, body)
              )
        and goDec (env, renv, cenv, crenv) =
          fn C.ValDec {exp, results} =>
            ( goSimpleExp (env, renv, cenv, crenv, exp)
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
                (fn {contParam, params, body, ...} =>
                   ( addC (cenv, contParam)
                   ; List.app (fn (p, _) => add (env, p)) params
                   ; goStat (env, renv, cenv, crenv, body)
                   )) defs;
              TypedSyntax.VIdMap.appi
                (fn (f, v) => TypedSyntax.VIdTable.insert renv (f, v))
                recursiveEnv;
              List.app
                (fn {name, ...} =>
                   TypedSyntax.VIdTable.insert env (name, ref neverUsed)) defs
            end
           | C.UnpackDec {tyVar = _, kind = _, vid, package, unpackedTy = _} =>
            (useValue env package; add (env, vid))
           | C.ContDec {name, params, body, attr = _} =>
            ( List.app (fn (SOME p, _) => add (env, p) | (NONE, _) => ()) params
            ; goStat (env, renv, cenv, crenv, body)
            ; addC (cenv, name)
            )
           | C.RecContDec defs =>
            let
              val recursiveCEnv =
                List.foldl
                  (fn ((f, _, _), m) =>
                     C.CVarMap.insert (m, f, ref neverUsedCont)) C.CVarMap.empty
                  defs
            in
              C.CVarMap.appi (fn (f, v) => C.CVarTable.insert cenv (f, v))
                recursiveCEnv;
              List.app
                (fn (_, params, body) =>
                   ( List.app (fn (SOME p, _) => add (env, p) | (NONE, _) => ())
                       params
                   ; goStat (env, renv, cenv, crenv, body)
                   )) defs;
              C.CVarMap.appi (fn (f, v) => C.CVarTable.insert crenv (f, v))
                recursiveCEnv;
              List.app
                (fn (f, _, _) => C.CVarTable.insert cenv (f, ref neverUsedCont))
                defs
            end
           | C.DatatypeDec _ => ()
           | C.ESImportDec {pure = _, specs, moduleName = _} =>
            List.app (fn (_, vid, _) => add (env, vid)) specs
        and goStat
          ( env: (usage ref) TypedSyntax.VIdTable.hash_table
          , renv
          , cenv: (cont_usage ref) C.CVarTable.hash_table
          , crenv
          , stat
          ) =
          case stat of
            C.Let {decs, cont} =>
              ( List.app (goDec (env, renv, cenv, crenv)) decs
              ; goStat (env, renv, cenv, crenv, cont)
              )
          | C.App {applied, cont, tyArgs, args, attr = _} =>
              ( useValueAsCallee (env, applied)
              ; useContVarIndirect cenv cont
              ; List.app (useValue env) args
              )
          | C.AppCont {applied, args} =>
              (useContVarDirect cenv applied; List.app (useValue env) args)
          | C.If {cond, thenCont, elseCont} =>
              ( useValue env cond
              ; goStat (env, renv, cenv, crenv, thenCont)
              ; goStat (env, renv, cenv, crenv, elseCont)
              )
          | C.Handle
              { body
              , handler = (e, h)
              , successfulExitIn
              , successfulExitOut
              , resultTy = _
              } =>
              ( useContVarIndirect cenv successfulExitOut
              ; addC (cenv, successfulExitIn)
              ; goStat (env, renv, cenv, crenv, body)
              ; add (env, e)
              ; goStat (env, renv, cenv, crenv, h)
              )
          | C.Raise (_, x) => useValue env x
          | C.Unreachable => ()
      end (* local *)
      fun analyze exp =
        let
          val dca = CpsDeadCodeAnalysis.analyze exp
          val usage =
            TypedSyntax.VIdTable.mkTable (1, Fail "usage table lookup failed")
          val rusage =
            TypedSyntax.VIdTable.mkTable (1, Fail "rusage table lookup failed")
          val cusage =
            CSyntax.CVarTable.mkTable (1, Fail "cusage table lookup failed")
          val crusage =
            CSyntax.CVarTable.mkTable (1, Fail "crusage table lookup failed")
        in
          goStat (usage, rusage, cusage, crusage, exp);
          { usage = usage
          , rec_usage = rusage
          , cont_usage = cusage
          , cont_rec_usage = crusage
          , dead_code_analysis = dca
          }
        end
    end (* local *)
  end (* strucuture CpsUsageAnalysis *)
in
  structure CpsDeadCodeElimination:
  sig
    val goStat: CpsSimplify.Context * bool * CSyntax.Stat -> CSyntax.Stat
  end =
  struct
    local
      structure C = CSyntax
      datatype frequency = datatype CpsUsageAnalysis.frequency
    in
      type Context =
        { base: CpsSimplify.Context
        , usage: CpsUsageAnalysis.usage_table
        , rec_usage: CpsUsageAnalysis.usage_table
        , cont_usage: CpsUsageAnalysis.cont_usage_table
        , cont_rec_usage: CpsUsageAnalysis.cont_usage_table
        , dead_code_analysis: CpsDeadCodeAnalysis.usage
        , is_final_stage: bool
        }
      fun simplifyDec (ctx: Context, appliedCont: C.CVar option)
        (dec, (env, cenv, acc: C.Dec list)) =
        case dec of
          C.ValDec {exp, results} =>
            let
              val results =
                List.map
                  (fn (result as SOME name, ty) =>
                     if
                       CpsDeadCodeAnalysis.isUsed
                         (#dead_code_analysis ctx, name)
                     then (result, ty)
                     else (NONE, ty)
                    | (NONE, ty) => (NONE, ty)) results
            in
              case (exp, results) of
                ( C.Abs {contParam, tyParams, params, body, resultTy, attr}
                , [(SOME result, ty)]
                ) =>
                  (case CpsUsageAnalysis.getValueUsage (#usage ctx, result) of
                     SOME {call = NEVER, other = NEVER} =>
                       ( #simplificationOccurred (#base ctx) := true
                       ; (env, cenv, acc)
                       )
                   | SOME {call = ONCE, other = NEVER} =>
                       let
                         val body = simplifyStat (ctx, env, cenv, body)
                         val env = TypedSyntax.VIdMap.insert
                           ( env
                           , result
                           , { exp = SOME (C.Abs
                                 { contParam = contParam
                                 , tyParams = tyParams
                                 , params = params
                                 , body = body
                                 , resultTy = resultTy
                                 , attr = attr
                                 })
                             , isDiscardableFunction =
                                 CpsSimplify.isDiscardableExp (env, body)
                             }
                           )
                         val () = #simplificationOccurred (#base ctx) := true
                       in
                         (env, cenv, acc)
                       end
                   | _ =>
                       let
                         val body = simplifyStat (ctx, env, cenv, body)
                         val exp = C.Abs
                           { contParam = contParam
                           , tyParams = tyParams
                           , params = params
                           , body = body
                           , resultTy = resultTy
                           , attr = attr
                           }
                         val env = TypedSyntax.VIdMap.insert
                           ( env
                           , result
                           , { exp = NONE
                             , isDiscardableFunction =
                                 CpsSimplify.isDiscardableExp (env, body)
                             }
                           )
                         val dec = C.ValDec
                           {exp = exp, results = [(SOME result, ty)]}
                       in
                         (env, cenv, dec :: acc)
                       end)
              | _ =>
                  (case (C.isDiscardable exp, results) of
                     (true, [(NONE, _)]) => (env, cenv, acc)
                   | (_, [(SOME result, ty)]) =>
                       let
                         val dec = C.ValDec
                           {exp = exp, results = [(SOME result, ty)]}
                         val env = TypedSyntax.VIdMap.insert
                           ( env
                           , result
                           , {exp = SOME exp, isDiscardableFunction = false}
                           )
                       in
                         (env, cenv, dec :: acc)
                       end
                   | _ =>
                       let val dec = C.ValDec {exp = exp, results = results}
                       in (env, cenv, dec :: acc)
                       end)
            end
        | C.RecDec defs =>
            if
              List.exists
                (fn {name, ...} =>
                   CpsDeadCodeAnalysis.isUsed (#dead_code_analysis ctx, name))
                defs
            then
              let
                val defs =
                  List.map
                    (fn { name
                        , contParam
                        , tyParams
                        , params
                        , body
                        , resultTy
                        , attr
                        } =>
                       { name = name
                       , contParam = contParam
                       , tyParams = tyParams
                       , params = params
                       , body = simplifyStat (ctx, env, cenv, body)
                       , resultTy = resultTy
                       , attr = attr
                       }) defs
                val decs = C.RecDec defs :: acc
              in
                (env, cenv, decs)
              end
            else
              (#simplificationOccurred (#base ctx) := true; (env, cenv, acc))
        | C.UnpackDec
            {tyVar = _, kind = _, vid = _, unpackedTy = _, package = _} =>
            ( (* case CpsUsageAnalysis.getValueUsage (#usage ctx, vid) of
                SOME {call = NEVER, other = NEVER} =>
                  ( #simplificationOccurred (#base ctx) := true
                  ; (env, cenv, acc)
                  )
              | _ => *)
              (* print ("DCE.UnpackDec: " ^ TypedSyntax.print_VId vid ^ " / " ^ C.valueToString package ^ "\n"); *)
              ( env
              , cenv
              , dec :: acc
              ))
        | C.ContDec {name, params, body, attr} =>
            (case CpsUsageAnalysis.getContUsage (#cont_usage ctx, name) of
               SOME {direct = NEVER, indirect = NEVER} =>
                 (#simplificationOccurred (#base ctx) := true; (env, cenv, acc))
             | SOME {direct = direct_usage, indirect = indirect_usage} =>
                 if
                   direct_usage = ONCE andalso indirect_usage = NEVER
                   andalso
                   ((case appliedCont of
                       SOME c => c = name
                     | NONE => false)
                    orelse CpsSimplify.sizeOfStat (body, 10) >= 0)
                 then (* Inline small continuations *)
                   let
                     val () = #simplificationOccurred (#base ctx) := true
                     val body = simplifyStat (ctx, env, cenv, body)
                     val cenv = C.CVarMap.insert
                       (cenv, name, (params, SOME body))
                   in
                     (env, cenv, acc)
                   end
                 else
                   let
                     val body = simplifyStat (ctx, env, cenv, body)
                     val params =
                       List.map
                         (fn (SOME p, ty) =>
                            (case CpsUsageAnalysis.getValueUsage (#usage ctx, p) of
                               SOME {call = NEVER, other = NEVER, ...} =>
                                 (NONE, ty)
                             | _ => (SOME p, ty))
                           | (NONE, ty) => (NONE, ty)) params
                   in
                     if
                       List.exists (fn (p, _) => Option.isSome p) params
                       orelse List.null params orelse #is_final_stage ctx
                     then
                       let
                         val cenv =
                           C.CVarMap.insert (cenv, name, (params, NONE))
                         val dec = C.ContDec
                           { name = name
                           , params = params
                           , body = body
                           , attr = attr
                           }
                       in
                         (env, cenv, dec :: acc)
                       end
                     else
                       let
                         (*
                            letcont k a = ... (a unused)
                            ~>
                            letcont k_worker () = ...
                            letcont[always_inline] k a = goto k_worker ()
                          *)
                         val workerName =
                           CpsSimplify.renewCVar (#base ctx, name)
                         val wrapperBody =
                           C.AppCont {applied = workerName, args = []}
                         val cenv = C.CVarMap.insert
                           (cenv, name, (params, SOME wrapperBody))
                         val cenv =
                           C.CVarMap.insert (cenv, workerName, ([], NONE))
                         val dec1 = C.ContDec
                           { name = workerName
                           , params = []
                           , body = body
                           , attr = attr
                           }
                         val dec2 = C.ContDec
                           { name = name
                           , params = params
                           , body = wrapperBody
                           , attr = {alwaysInline = true}
                           }
                       in
                         (env, cenv, dec2 :: dec1 :: acc)
                       end
                   end
             | NONE =>
                 raise Fail "CpsDeadCodeElimination: undefined continuation")
        | C.RecContDec defs =>
            if
              List.all
                (fn (f, _, _) =>
                   CpsUsageAnalysis.getContUsage (#cont_usage ctx, f)
                   = SOME {direct = NEVER, indirect = NEVER}) defs
            then
              (#simplificationOccurred (#base ctx) := true; (env, cenv, acc))
            else
              let
                val cenv =
                  List.foldl
                    (fn ((name, params, _), cenv) =>
                       C.CVarMap.insert (cenv, name, (params, NONE))) cenv defs
                val dec = C.RecContDec
                  (List.map
                     (fn (name, params, body) =>
                        (name, params, simplifyStat (ctx, env, cenv, body)))
                     defs)
              in
                (env, cenv, dec :: acc)
              end
        | C.DatatypeDec _ => (env, cenv, dec :: acc) (* TODO: DCE for types *)
        | C.ESImportDec {pure, specs, moduleName} =>
            let
              val specs =
                List.filter
                  (fn (_, vid, _) =>
                     CpsDeadCodeAnalysis.isUsed (#dead_code_analysis ctx, vid))
                  specs
            in
              if pure andalso List.null specs then
                (env, cenv, acc)
              else
                let
                  val dec =
                    C.ESImportDec
                      {pure = pure, specs = specs, moduleName = moduleName}
                in
                  (env, cenv, dec :: acc)
                end
            end
      and simplifyStat
        ( ctx: Context
        , env: CpsSimplify.value_info TypedSyntax.VIdMap.map
        , cenv: ((C.Var option * C.Ty) list * C.Stat option) C.CVarMap.map
        , e
        ) =
        case e of
          C.Let {decs, cont} =>
            let
              val appliedCont =
                case cont of
                  C.AppCont {applied, args = _} => SOME applied
                | _ => NONE
              val (env, cenv, revDecs) =
                List.foldl (simplifyDec (ctx, appliedCont)) (env, cenv, []) decs
            in
              CpsTransform.prependRevDecs
                (revDecs, simplifyStat (ctx, env, cenv, cont))
            end
        | C.App {applied, cont, tyArgs, args, attr} =>
            (case applied of
               C.Var applied =>
                 (case TypedSyntax.VIdMap.find (env, applied) of
                    SOME {exp, isDiscardableFunction} =>
                      let
                        val contBody =
                          case CSyntax.CVarMap.find (cenv, cont) of
                            SOME (_, body) => body
                          | NONE => NONE
                      in
                        case (isDiscardableFunction, contBody) of
                          (true, SOME (C.AppCont {applied = worker, args = []})) =>
                            (* Eliminate function calls like `val _ = f ()` *)
                            ( #simplificationOccurred (#base ctx) := true
                            ; C.AppCont {applied = worker, args = []}
                            )
                        | _ =>
                            case exp of
                              SOME
                                (C.Abs
                                   { contParam
                                   , tyParams
                                   , params
                                   , body
                                   , resultTy = _
                                   , attr = _
                                   }) =>
                                let
                                  val () =
                                    #simplificationOccurred (#base ctx) := true
                                  val tysubst =
                                    ListPair.foldl
                                      (fn ((p, _), a, acc) =>
                                         TypedSyntax.TyVarMap.insert (acc, p, a))
                                      TypedSyntax.TyVarMap.empty
                                      (tyParams, tyArgs)
                                  val subst =
                                    ListPair.foldlEq
                                      (fn ((p, _), a, subst) =>
                                         TypedSyntax.VIdMap.insert (subst, p, a))
                                      TypedSyntax.VIdMap.empty (params, args)
                                  val csubst =
                                    C.CVarMap.singleton (contParam, cont)
                                  val canOmitAlphaConversion =
                                    case
                                      CpsUsageAnalysis.getValueUsage
                                        (#usage ctx, applied)
                                    of
                                      SOME {call = ONCE, other = NEVER} =>
                                        (case
                                           CpsUsageAnalysis.getValueUsage
                                             (#rec_usage ctx, applied)
                                         of
                                           NONE => true
                                         | _ => false)
                                    | _ => false
                                in
                                  if canOmitAlphaConversion then
                                    CpsSimplify.substStat
                                      ( tysubst
                                      , subst
                                      , csubst
                                      , body
                                      ) (* no alpha conversion *)
                                  else
                                    CpsSimplify.alphaConvert
                                      (#base ctx, tysubst, subst, csubst, body)
                                end
                            | _ =>
                                C.App
                                  { applied = C.Var applied
                                  , cont = cont
                                  , tyArgs = tyArgs
                                  , args = args
                                  , attr = attr
                                  }
                      end
                  | NONE =>
                      C.App
                        { applied = C.Var applied
                        , cont = cont
                        , tyArgs = tyArgs
                        , args = args
                        , attr = attr
                        })
             | _ =>
                 C.App
                   { applied = applied
                   , cont = cont
                   , tyArgs = tyArgs
                   , args = args
                   , attr = attr
                   } (* should not occur *))
        | C.AppCont {applied, args} =>
            (case C.CVarMap.find (cenv, applied) of
               SOME (params, SOME body) =>
                 let
                   val () = #simplificationOccurred (#base ctx) := true
                   val subst =
                     ListPair.foldlEq
                       (fn ((SOME p, _), a, subst) =>
                          TypedSyntax.VIdMap.insert (subst, p, a)
                         | ((NONE, _), _, subst) => subst)
                       TypedSyntax.VIdMap.empty (params, args)
                   val canOmitAlphaConversion =
                     case
                       CpsUsageAnalysis.getContUsage (#cont_usage ctx, applied)
                     of
                       SOME {direct = ONCE, indirect = NEVER} =>
                         (case
                            CpsUsageAnalysis.getContUsage
                              (#cont_rec_usage ctx, applied)
                          of
                            NONE => true
                          | _ => false)
                     | _ => false
                 in
                   if canOmitAlphaConversion then
                     CpsSimplify.substStat
                       ( TypedSyntax.TyVarMap.empty
                       , subst
                       , C.CVarMap.empty
                       , body
                       ) (* no alpha conversion *)
                   else
                     CpsSimplify.alphaConvert
                       ( #base ctx
                       , TypedSyntax.TyVarMap.empty
                       , subst
                       , C.CVarMap.empty
                       , body
                       )
                 end
             | _ => C.AppCont {applied = applied, args = args})
        | C.If {cond, thenCont, elseCont} =>
            (case cond of
               C.BoolConst true =>
                 ( #simplificationOccurred (#base ctx) := true
                 ; simplifyStat (ctx, env, cenv, thenCont)
                 )
             | C.BoolConst false =>
                 ( #simplificationOccurred (#base ctx) := true
                 ; simplifyStat (ctx, env, cenv, elseCont)
                 )
             | cond =>
                 C.If
                   { cond = cond
                   , thenCont = simplifyStat (ctx, env, cenv, thenCont)
                   , elseCont = simplifyStat (ctx, env, cenv, elseCont)
                   })
        | C.Handle
            { body
            , handler = (e, h)
            , successfulExitIn
            , successfulExitOut
            , resultTy
            } =>
            C.Handle
              { body =
                  simplifyStat
                    ( ctx
                    , env
                    , C.CVarMap.empty (* do not inline across 'handle' *)
                    , body
                    )
              , handler = (e, simplifyStat (ctx, env, cenv, h))
              , successfulExitIn = successfulExitIn
              , successfulExitOut = successfulExitOut
              , resultTy = resultTy
              }
        | C.Raise (_, _) => e
        | C.Unreachable => e
      fun goStat (ctx: CpsSimplify.Context, is_final_stage: bool, exp) =
        let
          val usage = CpsUsageAnalysis.analyze exp
          val ctx' =
            { base = ctx
            , usage = #usage usage
            , rec_usage = #rec_usage usage
            , cont_usage = #cont_usage usage
            , cont_rec_usage = #cont_rec_usage usage
            , dead_code_analysis = #dead_code_analysis usage
            , is_final_stage = is_final_stage
            }
        in
          simplifyStat (ctx', TypedSyntax.VIdMap.empty, C.CVarMap.empty, exp)
        end
    end (* local *)
  end; (* structure CpsDeadCodeElimination *)
end; (* local *)
