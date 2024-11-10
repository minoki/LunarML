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
    val analyze: CSyntax.CExp -> usage
    val isUsed: usage * TypedSyntax.VId -> bool
  end =
  struct
    local structure C = CSyntax
    in
      type graph = TypedSyntax.VIdSet.set TypedSyntax.VIdTable.hash_table
      type usage = bool TypedSyntax.VIdTable.hash_table
      fun addValue (C.Var v, set) = TypedSyntax.VIdSet.add (set, v)
        | addValue (C.Unit, set) = set
        | addValue (C.Nil, set) = set
        | addValue (C.BoolConst _, set) = set
        | addValue (C.IntConst _, set) = set
        | addValue (C.WordConst _, set) = set
        | addValue (C.CharConst _, set) = set
        | addValue (C.Char16Const _, set) = set
        | addValue (C.StringConst _, set) = set
        | addValue (C.String16Const _, set) = set
      fun goSimpleExp (_, C.PrimOp {primOp = _, tyargs = _, args}) =
            List.foldl addValue TypedSyntax.VIdSet.empty args
        | goSimpleExp (_, C.Record fields) =
            Syntax.LabelMap.foldl addValue TypedSyntax.VIdSet.empty fields
        | goSimpleExp (_, C.ExnTag {name = _, payloadTy = _}) =
            TypedSyntax.VIdSet.empty
        | goSimpleExp (_, C.Projection {label = _, record, fieldTypes = _}) =
            addValue (record, TypedSyntax.VIdSet.empty)
        | goSimpleExp (g, C.Abs {contParam = _, params = _, body, attr = _}) =
            goExp
              (g, body, TypedSyntax.VIdSet.empty) (* What to do with params? *)
      and goDec g (C.ValDec {exp, results}, acc) =
            let
              val s = goSimpleExp (g, exp)
            in
              List.app
                (fn SOME r => TypedSyntax.VIdTable.insert g (r, s) | NONE => ())
                results;
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
        | goDec g (C.ContDec {name = _, params = _, body, attr = _}, acc) =
            goExp (g, body, acc)
        | goDec g (C.RecContDec defs, acc) =
            List.foldl (fn ((_, _, body), acc) => goExp (g, body, acc)) acc defs
        | goDec g (C.ESImportDec {pure = _, specs, moduleName = _}, acc) =
            ( List.app
                (fn (_, vid) =>
                   TypedSyntax.VIdTable.insert g (vid, TypedSyntax.VIdSet.empty))
                specs
            ; acc
            )
      and goExp (g, C.Let {decs, cont}, acc) =
            goExp (g, cont, List.foldl (goDec g) acc decs)
        | goExp (_, C.App {applied, cont = _, args, attr = _}, acc) =
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
                }
            , acc
            ) =
            goExp (g, body, goExp (g, h, acc))
        | goExp (_, C.Raise (_, x), acc) = addValue (x, acc)
        | goExp (_, C.Unreachable, acc) = acc
      (*: val makeGraph : CSyntax.CExp -> graph * TypedSyntax.VIdSet.set *)
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
      CSyntax.CExp
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
      fun useValue env (C.Var v) =
            (case TypedSyntax.VIdTable.find env v of
               SOME r =>
                 let val {call, other} = !r
                 in r := {call = call, other = oneMore other}
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
      fun useValueAsCallee (env, C.Var v) =
            (case TypedSyntax.VIdTable.find env v of
               SOME r =>
                 let val {call, other} = !r
                 in r := {call = oneMore call, other = other}
                 end
             | NONE => ())
        | useValueAsCallee (_, C.Unit) = ()
        | useValueAsCallee (_, C.Nil) = ()
        | useValueAsCallee (_, C.BoolConst _) = ()
        | useValueAsCallee (_, C.IntConst _) = ()
        | useValueAsCallee (_, C.WordConst _) = ()
        | useValueAsCallee (_, C.CharConst _) = ()
        | useValueAsCallee (_, C.Char16Const _) = ()
        | useValueAsCallee (_, C.StringConst _) = ()
        | useValueAsCallee (_, C.String16Const _) = ()
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
              ("goCExp: duplicate name in AST: " ^ TypedSyntax.print_VId v)
          else
            TypedSyntax.VIdTable.insert env (v, ref neverUsed)
        fun addC (cenv, v) =
          if C.CVarTable.inDomain cenv v then
            raise Fail
              ("goCExp: duplicate continuation name in AST: "
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
              , C.Abs {contParam, params, body, attr = _}
              ) =
              ( List.app (fn p => add (env, p)) params
              ; addC (cenv, contParam)
              ; goCExp (env, renv, cenv, crenv, body)
              )
        and goDec (env, renv, cenv, crenv) =
          fn C.ValDec {exp, results} =>
            ( goSimpleExp (env, renv, cenv, crenv, exp)
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
                (fn {contParam, params, body, ...} =>
                   ( addC (cenv, contParam)
                   ; List.app (fn p => add (env, p)) params
                   ; goCExp (env, renv, cenv, crenv, body)
                   )) defs;
              TypedSyntax.VIdMap.appi
                (fn (f, v) => TypedSyntax.VIdTable.insert renv (f, v))
                recursiveEnv;
              List.app
                (fn {name, ...} =>
                   TypedSyntax.VIdTable.insert env (name, ref neverUsed)) defs
            end
           | C.ContDec {name, params, body, attr = _} =>
            ( List.app (Option.app (fn p => add (env, p))) params
            ; goCExp (env, renv, cenv, crenv, body)
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
                   ( List.app (Option.app (fn p => add (env, p))) params
                   ; goCExp (env, renv, cenv, crenv, body)
                   )) defs;
              C.CVarMap.appi (fn (f, v) => C.CVarTable.insert crenv (f, v))
                recursiveCEnv;
              List.app
                (fn (f, _, _) => C.CVarTable.insert cenv (f, ref neverUsedCont))
                defs
            end
           | C.ESImportDec {pure = _, specs, moduleName = _} =>
            List.app (fn (_, vid) => add (env, vid)) specs
        and goCExp
          ( env: (usage ref) TypedSyntax.VIdTable.hash_table
          , renv
          , cenv: (cont_usage ref) C.CVarTable.hash_table
          , crenv
          , cexp
          ) =
          case cexp of
            C.Let {decs, cont} =>
              ( List.app (goDec (env, renv, cenv, crenv)) decs
              ; goCExp (env, renv, cenv, crenv, cont)
              )
          | C.App {applied, cont, args, attr = _} =>
              ( useValueAsCallee (env, applied)
              ; useContVarIndirect cenv cont
              ; List.app (useValue env) args
              )
          | C.AppCont {applied, args} =>
              (useContVarDirect cenv applied; List.app (useValue env) args)
          | C.If {cond, thenCont, elseCont} =>
              ( useValue env cond
              ; goCExp (env, renv, cenv, crenv, thenCont)
              ; goCExp (env, renv, cenv, crenv, elseCont)
              )
          | C.Handle
              {body, handler = (e, h), successfulExitIn, successfulExitOut} =>
              ( useContVarIndirect cenv successfulExitOut
              ; addC (cenv, successfulExitIn)
              ; goCExp (env, renv, cenv, crenv, body)
              ; add (env, e)
              ; goCExp (env, renv, cenv, crenv, h)
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
          goCExp (usage, rusage, cusage, crusage, exp);
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
    val goCExp: CpsSimplify.Context * CSyntax.CExp -> CSyntax.CExp
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
        }
      fun simplifyDec (ctx: Context, appliedCont: C.CVar option)
        (dec, (env, cenv, subst, csubst, acc: C.Dec list)) =
        case dec of
          C.ValDec {exp, results} =>
            let
              val exp = CpsSimplify.substSimpleExp (subst, csubst, exp)
              val results =
                List.map
                  (fn result as SOME name =>
                     if
                       CpsDeadCodeAnalysis.isUsed
                         (#dead_code_analysis ctx, name)
                     then result
                     else NONE
                    | NONE => NONE) results
            in
              case (exp, results) of
                (C.Abs {contParam, params, body, attr}, [SOME result]) =>
                  (case CpsUsageAnalysis.getValueUsage (#usage ctx, result) of
                     SOME {call = NEVER, other = NEVER} =>
                       ( #simplificationOccurred (#base ctx) := true
                       ; (env, cenv, subst, csubst, acc)
                       )
                   | SOME {call = ONCE, other = NEVER} =>
                       let
                         val body = simplifyCExp
                           (ctx, env, cenv, subst, csubst, body)
                         val env = TypedSyntax.VIdMap.insert
                           ( env
                           , result
                           , { exp = SOME (C.Abs
                                 { contParam = contParam
                                 , params = params
                                 , body = body
                                 , attr = attr
                                 })
                             , isDiscardableFunction =
                                 CpsSimplify.isDiscardableExp (env, body)
                             }
                           )
                         val () = #simplificationOccurred (#base ctx) := true
                       in
                         (env, cenv, subst, csubst, acc)
                       end
                   | _ =>
                       let
                         val body = simplifyCExp
                           (ctx, env, cenv, subst, csubst, body)
                         val exp = C.Abs
                           { contParam = contParam
                           , params = params
                           , body = body
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
                         val dec = C.ValDec {exp = exp, results = [SOME result]}
                       in
                         (env, cenv, subst, csubst, dec :: acc)
                       end)
              | _ =>
                  (case (C.isDiscardable exp, results) of
                     (true, [NONE]) => (env, cenv, subst, csubst, acc)
                   | (_, [SOME result]) =>
                       let
                         val dec = C.ValDec {exp = exp, results = [SOME result]}
                         val env = TypedSyntax.VIdMap.insert
                           ( env
                           , result
                           , {exp = SOME exp, isDiscardableFunction = false}
                           )
                       in
                         (env, cenv, subst, csubst, dec :: acc)
                       end
                   | _ =>
                       let val dec = C.ValDec {exp = exp, results = results}
                       in (env, cenv, subst, csubst, dec :: acc)
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
                    (fn {name, contParam, params, body, attr} =>
                       { name = name
                       , contParam = contParam
                       , params = params
                       , body = simplifyCExp
                           (ctx, env, cenv, subst, csubst, body)
                       , attr = attr
                       }) defs
                val decs = C.RecDec defs :: acc
              in
                (env, cenv, subst, csubst, decs)
              end
            else
              ( #simplificationOccurred (#base ctx) := true
              ; (env, cenv, subst, csubst, acc)
              )
        | C.ContDec {name, params, body, attr} =>
            (case CpsUsageAnalysis.getContUsage (#cont_usage ctx, name) of
               SOME {direct = NEVER, indirect = NEVER} =>
                 ( #simplificationOccurred (#base ctx) := true
                 ; (env, cenv, subst, csubst, acc)
                 )
             | SOME {direct = direct_usage, indirect = indirect_usage} =>
                 if
                   direct_usage = ONCE andalso indirect_usage = NEVER
                   andalso
                   ((case appliedCont of
                       SOME c => c = name
                     | NONE => false)
                    orelse CpsSimplify.sizeOfCExp (body, 10) >= 0)
                 then (* Inline small continuations *)
                   let
                     val () = #simplificationOccurred (#base ctx) := true
                     val body = simplifyCExp
                       (ctx, env, cenv, subst, csubst, body)
                     val cenv = C.CVarMap.insert
                       (cenv, name, (params, SOME body))
                   in
                     (env, cenv, subst, csubst, acc)
                   end
                 else
                   let
                     val body = simplifyCExp
                       (ctx, env, cenv, subst, csubst, body)
                     val params =
                       List.map
                         (fn SOME p =>
                            (case CpsUsageAnalysis.getValueUsage (#usage ctx, p) of
                               SOME {call = NEVER, other = NEVER, ...} => NONE
                             | _ => SOME p)
                           | NONE => NONE) params
                     val cenv = C.CVarMap.insert (cenv, name, (params, NONE))
                     val dec = C.ContDec
                       {name = name, params = params, body = body, attr = attr}
                   in
                     (env, cenv, subst, csubst, dec :: acc)
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
              ( #simplificationOccurred (#base ctx) := true
              ; (env, cenv, subst, csubst, acc)
              )
            else
              let
                val cenv =
                  List.foldl
                    (fn ((name, params, _), cenv) =>
                       C.CVarMap.insert (cenv, name, (params, NONE))) cenv defs
                val dec = C.RecContDec
                  (List.map
                     (fn (name, params, body) =>
                        ( name
                        , params
                        , simplifyCExp (ctx, env, cenv, subst, csubst, body)
                        )) defs)
              in
                (env, cenv, subst, csubst, dec :: acc)
              end
        | C.ESImportDec {pure, specs, moduleName} =>
            let
              val specs =
                List.filter
                  (fn (_, vid) =>
                     CpsDeadCodeAnalysis.isUsed (#dead_code_analysis ctx, vid))
                  specs
            in
              if pure andalso List.null specs then
                (env, cenv, subst, csubst, acc)
              else
                let
                  val dec =
                    C.ESImportDec
                      {pure = pure, specs = specs, moduleName = moduleName}
                in
                  (env, cenv, subst, csubst, dec :: acc)
                end
            end
      and simplifyCExp
        ( ctx: Context
        , env: CpsSimplify.value_info TypedSyntax.VIdMap.map
        , cenv: ((C.Var option) list * C.CExp option) C.CVarMap.map
        , subst: C.Value TypedSyntax.VIdMap.map
        , csubst: C.CVar C.CVarMap.map
        , e
        ) =
        case e of
          C.Let {decs, cont} =>
            let
              val appliedCont =
                case cont of
                  C.AppCont {applied, args = _} => SOME applied
                | _ => NONE
              val (env, cenv, subst, csubst, revDecs) =
                List.foldl (simplifyDec (ctx, appliedCont))
                  (env, cenv, subst, csubst, []) decs
            in
              CpsTransform.prependRevDecs (revDecs, simplifyCExp
                (ctx, env, cenv, subst, csubst, cont))
            end
        | C.App {applied, cont, args, attr} =>
            let
              val applied = CpsSimplify.substValue subst applied
              val cont = CpsSimplify.substCVar csubst cont
              val args = List.map (CpsSimplify.substValue subst) args
            in
              case applied of
                C.Var applied =>
                  (case TypedSyntax.VIdMap.find (env, applied) of
                     SOME {exp, isDiscardableFunction} =>
                       let
                         val isDiscardable =
                           if isDiscardableFunction then
                             case C.CVarMap.find (cenv, cont) of
                               SOME (params, _) =>
                                 if List.exists Option.isSome params then NONE
                                 else SOME params
                             | _ => NONE
                           else
                             NONE
                       in
                         case isDiscardable of
                           SOME params =>
                             ( #simplificationOccurred (#base ctx) := true
                             ; C.AppCont
                                 { applied = cont
                                 , args =
                                     List.map (fn _ => C.Unit (* dummy *))
                                       params
                                 }
                             )
                         | NONE =>
                             case exp of
                               SOME (C.Abs {contParam, params, body, attr = _}) =>
                                 let
                                   val () =
                                     #simplificationOccurred (#base ctx) := true
                                   val subst =
                                     ListPair.foldlEq
                                       (fn (p, a, subst) =>
                                          TypedSyntax.VIdMap.insert
                                            (subst, p, a)) subst (params, args)
                                   val csubst =
                                     C.CVarMap.insert (csubst, contParam, cont)
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
                                     CpsSimplify.substCExp
                                       ( subst
                                       , csubst
                                       , body
                                       ) (* no alpha conversion *)
                                   else
                                     CpsSimplify.alphaConvert
                                       (#base ctx, subst, csubst, body)
                                 end
                             | _ =>
                                 C.App
                                   { applied = C.Var applied
                                   , cont = cont
                                   , args = args
                                   , attr = attr
                                   }
                       end
                   | NONE =>
                       C.App
                         { applied = C.Var applied
                         , cont = cont
                         , args = args
                         , attr = attr
                         })
              | _ =>
                  C.App
                    { applied = applied
                    , cont = cont
                    , args = args
                    , attr = attr
                    } (* should not occur *)
            end
        | C.AppCont {applied, args} =>
            let
              val applied = CpsSimplify.substCVar csubst applied
              val args = List.map (CpsSimplify.substValue subst) args
            in
              case C.CVarMap.find (cenv, applied) of
                SOME (params, SOME body) =>
                  let
                    val () = #simplificationOccurred (#base ctx) := true
                    val subst =
                      ListPair.foldlEq
                        (fn (SOME p, a, subst) =>
                           TypedSyntax.VIdMap.insert (subst, p, a)
                          | (NONE, _, subst) => subst) subst (params, args)
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
                      CpsSimplify.substCExp
                        (subst, csubst, body) (* no alpha conversion *)
                    else
                      CpsSimplify.alphaConvert (#base ctx, subst, csubst, body)
                  end
              | _ => C.AppCont {applied = applied, args = args}
            end
        | C.If {cond, thenCont, elseCont} =>
            (case CpsSimplify.substValue subst cond of
               C.BoolConst true =>
                 ( #simplificationOccurred (#base ctx) := true
                 ; simplifyCExp (ctx, env, cenv, subst, csubst, thenCont)
                 )
             | C.BoolConst false =>
                 ( #simplificationOccurred (#base ctx) := true
                 ; simplifyCExp (ctx, env, cenv, subst, csubst, elseCont)
                 )
             | cond =>
                 C.If
                   { cond = cond
                   , thenCont = simplifyCExp
                       (ctx, env, cenv, subst, csubst, thenCont)
                   , elseCont = simplifyCExp
                       (ctx, env, cenv, subst, csubst, elseCont)
                   })
        | C.Handle {body, handler = (e, h), successfulExitIn, successfulExitOut} =>
            C.Handle
              { body = simplifyCExp
                  ( ctx
                  , env
                  , C.CVarMap.empty (* do not inline across 'handle' *)
                  , subst
                  , csubst
                  , body
                  )
              , handler = (e, simplifyCExp (ctx, env, cenv, subst, csubst, h))
              , successfulExitIn = successfulExitIn
              , successfulExitOut =
                  CpsSimplify.substCVar csubst successfulExitOut
              }
        | C.Raise (span, x) => C.Raise (span, CpsSimplify.substValue subst x)
        | C.Unreachable => e
      fun goCExp (ctx: CpsSimplify.Context, exp) =
        let
          val usage = CpsUsageAnalysis.analyze exp
          val ctx' =
            { base = ctx
            , usage = #usage usage
            , rec_usage = #rec_usage usage
            , cont_usage = #cont_usage usage
            , cont_rec_usage = #cont_rec_usage usage
            , dead_code_analysis = #dead_code_analysis usage
            }
        in
          simplifyCExp
            ( ctx'
            , TypedSyntax.VIdMap.empty
            , C.CVarMap.empty
            , TypedSyntax.VIdMap.empty
            , C.CVarMap.empty
            , exp
            )
        end
    end (* local *)
  end; (* structure CpsDeadCodeElimination *)
end; (* local *)
