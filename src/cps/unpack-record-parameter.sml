(*
 * Copyright (c) 2024 ARATA Mizuki
 * This file is part of LunarML.
 *)
(*
 * Unpack record parameters,
 *)
local
    structure CpsUsageAnalysis :> sig
                  datatype frequency = NEVER | ONCE | MANY
                  type usage = { call : frequency
                               , project : frequency
                               , other : frequency
                               , labels : (string option) Syntax.LabelMap.map
                               }
                  type cont_usage = { indirect : frequency }
                  val neverUsed : usage
                  val neverUsedCont : cont_usage
                  type usage_table
                  type cont_usage_table
                  val getValueUsage : usage_table * TypedSyntax.VId -> usage
                  val getContUsage : cont_usage_table * CSyntax.CVar -> cont_usage
                  val analyze : CSyntax.CExp -> { usage : usage_table
                                                , rec_usage : usage_table
                                                , cont_usage : cont_usage_table
                                                , cont_rec_usage : cont_usage_table
                                                }
              end = struct
    local structure C = CSyntax
    in
    datatype frequency = NEVER | ONCE | MANY
    fun oneMore NEVER = ONCE
      | oneMore ONCE = MANY
      | oneMore (many as MANY) = many
    type usage = { call : frequency
                 , project : frequency
                 , other : frequency
                 , labels : (string option) Syntax.LabelMap.map
                 }
    type cont_usage = { indirect : frequency }
    val neverUsed : usage = { call = NEVER
                            , project = NEVER
                            , other = NEVER
                            , labels = Syntax.LabelMap.empty
                            }
    val neverUsedCont : cont_usage = { indirect = NEVER }
    type usage_table = (usage ref) TypedSyntax.VIdTable.hash_table
    type cont_usage_table = (cont_usage ref) CSyntax.CVarTable.hash_table
    fun getValueUsage (table : usage_table, v)
        = case TypedSyntax.VIdTable.find table v of
              SOME r => !r
            | NONE => { call = MANY, project = MANY, other = MANY, labels = Syntax.LabelMap.empty } (* unknown *)
    fun getContUsage (table : cont_usage_table, c)
        = case CSyntax.CVarTable.find table c of
              SOME r => !r
            | NONE => { indirect = MANY } (* unknown *)
    fun useValue env (C.Var v) = (case TypedSyntax.VIdTable.find env v of
                                      SOME r => let val { call, project, other, labels } = !r
                                                in r := { call = call, project = project, other = oneMore other, labels = labels }
                                                end
                                    | NONE => ()
                                 )
      | useValue _ C.Unit = ()
      | useValue _ C.Nil = ()
      | useValue _ (C.BoolConst _) = ()
      | useValue _ (C.IntConst _) = ()
      | useValue _ (C.WordConst _) = ()
      | useValue _ (C.CharConst _) = ()
      | useValue _ (C.Char16Const _) = ()
      | useValue _ (C.StringConst _) = ()
      | useValue _ (C.String16Const _) = ()
    fun useValueAsCallee (env, cont, C.Var v)
        = (case TypedSyntax.VIdTable.find env v of
               SOME r => let val { call, project, other, labels } = !r
                         in r := { call = oneMore call, project = project, other = other, labels = labels }
                         end
             | NONE => ()
          )
      | useValueAsCallee (_, _, C.Unit) = ()
      | useValueAsCallee (_, _, C.Nil) = ()
      | useValueAsCallee (_, _, C.BoolConst _) = ()
      | useValueAsCallee (_, _, C.IntConst _) = ()
      | useValueAsCallee (_, _, C.WordConst _) = ()
      | useValueAsCallee (_, _, C.CharConst _) = ()
      | useValueAsCallee (_, _, C.Char16Const _) = ()
      | useValueAsCallee (_, _, C.StringConst _) = ()
      | useValueAsCallee (_, _, C.String16Const _) = ()
    fun useValueAsRecord (env, label, result, C.Var v)
        = (case TypedSyntax.VIdTable.find env v of
               SOME r => let val { call, project, other, labels } = !r
                             val result' = case result of
                                               SOME (TypedSyntax.MkVId (name, _)) => SOME name
                                             | NONE => NONE
                             fun mergeOption (x as SOME _, _) = x
                               | mergeOption (NONE, y) = y
                         in r := { call = call, project = oneMore project, other = other, labels = Syntax.LabelMap.insertWith mergeOption (labels, label, result') }
                         end
             | NONE => ()
          )
      | useValueAsRecord (_, _, _, C.Unit) = ()
      | useValueAsRecord (_, _, _, C.Nil) = ()
      | useValueAsRecord (_, _, _, C.BoolConst _) = ()
      | useValueAsRecord (_, _, _, C.IntConst _) = ()
      | useValueAsRecord (_, _, _, C.WordConst _) = ()
      | useValueAsRecord (_, _, _, C.CharConst _) = ()
      | useValueAsRecord (_, _, _, C.Char16Const _) = ()
      | useValueAsRecord (_, _, _, C.StringConst _) = ()
      | useValueAsRecord (_, _, _, C.String16Const _) = ()
    fun useContVarIndirect cenv (v : C.CVar) = (case C.CVarTable.find cenv v of
                                                    SOME r => let val { indirect } = !r
                                                              in r := { indirect = oneMore indirect }
                                                              end
                                                  | NONE => ()
                                               )
    fun useContVarDirect cenv (v : C.CVar) = ()
    local
        fun add (env, v) = if TypedSyntax.VIdTable.inDomain env v then
                               raise Fail ("goCExp: duplicate name in AST: " ^ TypedSyntax.print_VId v)
                           else
                               TypedSyntax.VIdTable.insert env (v, ref neverUsed)
        fun addC (cenv, v) = if C.CVarTable.inDomain cenv v then
                                 raise Fail ("goCExp: duplicate continuation name in AST: " ^ Int.toString (C.CVar.toInt v))
                             else
                                 C.CVarTable.insert cenv (v, ref neverUsedCont)
    in
    fun goSimpleExp (env, _, _, _, _, C.PrimOp { primOp = _, tyargs = _, args }) = List.app (useValue env) args
      | goSimpleExp (env, _, _, _, _, C.Record fields) = Syntax.LabelMap.app (useValue env) fields
      | goSimpleExp (_, _, _, _, _, C.ExnTag { name = _, payloadTy = _ }) = ()
      | goSimpleExp (env, _, _, _, results, C.Projection { label, record, fieldTypes = _ }) = (case results of
                                                                                                   [result] => useValueAsRecord (env, label, result, record)
                                                                                                 | _ => () (* should not occur *)
                                                                                              )
      | goSimpleExp (env, renv, cenv, crenv, _, C.Abs { contParam, params, body, attr = _ })
        = ( List.app (fn p => add (env, p)) params
          ; addC (cenv, contParam)
          ; goCExp (env, renv, cenv, crenv, body)
          )
    and goDec (env, renv, cenv, crenv)
        = fn C.ValDec { exp, results } =>
             ( goSimpleExp (env, renv, cenv, crenv, results, exp)
             ; List.app (fn SOME result => add (env, result)
                        | NONE => ()
                        ) results
             )
      | C.RecDec defs =>
        let val recursiveEnv = List.foldl (fn ({ name, ... }, m) => TypedSyntax.VIdMap.insert (m, name, ref neverUsed)) TypedSyntax.VIdMap.empty defs
        in TypedSyntax.VIdMap.appi (fn (f, v) => TypedSyntax.VIdTable.insert env (f, v)) recursiveEnv
         ; List.app (fn { contParam, params, body, ... } =>
                        ( addC (cenv, contParam)
                        ; List.app (fn p => add (env, p)) params
                        ; goCExp (env, renv, cenv, crenv, body)
                        )
                    ) defs
         ; TypedSyntax.VIdMap.appi (fn (f, v) => TypedSyntax.VIdTable.insert renv (f, v)) recursiveEnv
         ; List.app (fn { name, ... } => TypedSyntax.VIdTable.insert env (name, ref neverUsed)) defs
        end
      | C.ContDec { name, params, body } =>
        ( List.app (Option.app (fn p => add (env, p))) params
        ; goCExp (env, renv, cenv, crenv, body)
        ; addC (cenv, name)
        )
      | C.RecContDec defs =>
        let val recursiveCEnv = List.foldl (fn ((f, _, _), m) => C.CVarMap.insert (m, f, ref neverUsedCont)) C.CVarMap.empty defs
        in C.CVarMap.appi (fn (f, v) => C.CVarTable.insert cenv (f, v)) recursiveCEnv
         ; List.app (fn (_, params, body) => ( List.app (Option.app (fn p => add (env, p))) params
                                             ; goCExp (env, renv, cenv, crenv, body)
                                             )
                    ) defs
         ; C.CVarMap.appi (fn (f, v) => C.CVarTable.insert crenv (f, v)) recursiveCEnv
         ; List.app (fn (f, _, _) => C.CVarTable.insert cenv (f, ref neverUsedCont)) defs
        end
      | C.ESImportDec { pure = _, specs, moduleName = _ } => List.app (fn (_, vid) => add (env, vid)) specs
    and goCExp (env : (usage ref) TypedSyntax.VIdTable.hash_table, renv, cenv : (cont_usage ref) C.CVarTable.hash_table, crenv, cexp)
        = case cexp of
              C.Let { decs, cont } =>
              ( List.app (goDec (env, renv, cenv, crenv)) decs
              ; goCExp (env, renv, cenv, crenv, cont)
              )
            | C.App { applied, cont, args, attr = _ } =>
              ( useValueAsCallee (env, cont, applied)
              ; useContVarIndirect cenv cont
              ; List.app (useValue env) args
              )
            | C.AppCont { applied, args } =>
              ( useContVarDirect cenv applied
              ; List.app (useValue env) args
              )
            | C.If { cond, thenCont, elseCont } =>
              ( useValue env cond
              ; goCExp (env, renv, cenv, crenv, thenCont)
              ; goCExp (env, renv, cenv, crenv, elseCont)
              )
            | C.Handle { body, handler = (e, h), successfulExitIn, successfulExitOut } =>
              ( useContVarIndirect cenv successfulExitOut
              ; addC (cenv, successfulExitIn)
              ; goCExp (env, renv, cenv, crenv, body)
              ; add (env, e)
              ; goCExp (env, renv, cenv, crenv, h)
              )
            | C.Unreachable => ()
    end (* local *)
    fun analyze exp = let val usage = TypedSyntax.VIdTable.mkTable (1, Fail "usage table lookup failed")
                          val rusage = TypedSyntax.VIdTable.mkTable (1, Fail "rusage table lookup failed")
                          val cusage = CSyntax.CVarTable.mkTable (1, Fail "cusage table lookup failed")
                          val crusage = CSyntax.CVarTable.mkTable (1, Fail "crusage table lookup failed")
                      in goCExp (usage, rusage, cusage, crusage, exp)
                       ; { usage = usage, rec_usage = rusage, cont_usage = cusage, cont_rec_usage = crusage }
                      end
    end (* local *)
    end (* strucuture CpsUsageAnalysis *)
in
structure CpsUnpackRecordParameter : sig
              val goCExp : CpsSimplify.Context * CSyntax.CExp -> CSyntax.CExp
          end = struct
local structure F = FSyntax
      structure C = CSyntax
      structure P = Primitives
      datatype frequency = datatype CpsUsageAnalysis.frequency
in
type Context = { base : CpsSimplify.Context
               , usage : CpsUsageAnalysis.usage_table
               , rec_usage : CpsUsageAnalysis.usage_table
               , cont_usage : CpsUsageAnalysis.cont_usage_table
               , cont_rec_usage : CpsUsageAnalysis.cont_usage_table
               }
datatype param_transform = KEEP | ELIMINATE | UNPACK of (C.Var * Syntax.Label) list
fun tryUnpackParam (ctx : Context, usage) param
    = case CpsUsageAnalysis.getValueUsage (usage, param) of
          { call = NEVER, project = NEVER, other = NEVER, ... } => ELIMINATE
        | { call = NEVER, project = _, other = NEVER, labels, ... } =>
          UNPACK (Syntax.LabelMap.foldri (fn (label, optName, acc) =>
                                             let val name = case optName of
                                                                SOME name => name
                                                              | NONE => case label of
                                                                            Syntax.IdentifierLabel name => name
                                                                          | Syntax.NumericLabel n => "_" ^ Int.toString n
                                             in (CpsSimplify.newVId (#base ctx, name), label) :: acc
                                             end
                                         ) [] labels)
        | _ => KEEP
fun tryUnpackContParam (ctx : Context, usage) (SOME param)
    = (case CpsUsageAnalysis.getValueUsage (usage, param) of
           { call = NEVER, project = NEVER, other = NEVER, ... } => ELIMINATE
         | { call = NEVER, project = _, other = NEVER, labels, ... } =>
           UNPACK (Syntax.LabelMap.foldri (fn (label, optName, acc) =>
                                              let val name = case optName of
                                                                 SOME name => name
                                                               | NONE => case label of
                                                                             Syntax.IdentifierLabel name => name
                                                                           | Syntax.NumericLabel n => "_" ^ Int.toString n
                                              in (CpsSimplify.newVId (#base ctx, name), label) :: acc
                                              end
                                          ) [] labels)
         | _ => KEEP
      )
  | tryUnpackContParam (_, _) NONE = ELIMINATE
fun simplifyDec (ctx : Context) (dec, acc : C.Dec list)
    = case dec of
          C.ValDec { exp = C.Abs { contParam, params, body, attr }, results = [SOME result]} =>
          let val shouldTransformParams = case CpsUsageAnalysis.getValueUsage (#usage ctx, result) of
                                              { call = _, project = NEVER, other = NEVER, ... } =>
                                              let val t = List.map (tryUnpackParam (ctx, #usage ctx)) params
                                              in if List.exists (fn ELIMINATE => true | UNPACK _ => true | KEEP => false) t then
                                                     SOME t
                                                 else
                                                     NONE
                                              end
                                            | _ => NONE
          in case shouldTransformParams of
                 SOME paramTransforms =>
                 let val () = #simplificationOccurred (#base ctx) := true
                     val params' = ListPair.foldrEq (fn (p, KEEP, acc) => p :: acc
                                                    | (_, ELIMINATE, acc) => acc
                                                    | (_, UNPACK fields, acc) => List.map #1 fields @ acc
                                                    ) [] (params, paramTransforms)
                     val decs = ListPair.foldrEq (fn (p, UNPACK fields, decs) => C.ValDec { exp = C.Record (List.foldl (fn ((fieldVar, label), map) => Syntax.LabelMap.insert (map, label, C.Var fieldVar)) Syntax.LabelMap.empty fields), results = [SOME p] } :: decs
                                                 | (_, KEEP, decs) => decs
                                                 | (_, ELIMINATE, decs) => decs
                                                 ) [] (params, paramTransforms)
                     val body = case decs of
                                    [] => body
                                  | _ => C.Let { decs = decs, cont = body }
                     val body = simplifyCExp (ctx, body)
                     val exp = C.Abs { contParam = contParam, params = params', body = body, attr = attr }
                     val result' = CpsSimplify.renewVId (#base ctx, result)
                     val wrapperBody = let val k = CpsSimplify.genContSym (#base ctx)
                                           val params' = List.map (fn p => CpsSimplify.renewVId (#base ctx, p)) params
                                           val (decs, args) = ListPair.foldrEq (fn (p, KEEP, (decs, args)) => (decs, C.Var p :: args)
                                                                               | (_, ELIMINATE, acc) => acc
                                                                               | (p, UNPACK fields, (decs, args)) =>
                                                                                 List.foldr (fn ((v, label), (decs, args)) =>
                                                                                                let val dec = C.ValDec { exp = C.Projection { label = label, record = C.Var p, fieldTypes = Syntax.LabelMap.empty (* dummy *) }
                                                                                                                       , results = [SOME v]
                                                                                                                       }
                                                                                                in (dec :: decs, C.Var v :: args)
                                                                                                end
                                                                                            ) (decs, args) fields
                                                                               ) ([], []) (params', paramTransforms)
                                       in C.Abs { contParam = k
                                                , params = params'
                                                , body = C.Let { decs = decs
                                                               , cont = C.App { applied = C.Var result'
                                                                              , cont = k
                                                                              , args = args
                                                                              , attr = {}
                                                                              }
                                                               }
                                                , attr = { isWrapper = true }
                                                }
                                       end
                     val dec1 = C.ValDec { exp = wrapperBody, results = [SOME result] }
                     val dec2 = C.ValDec { exp = exp, results = [SOME result'] }
                 in dec2 :: dec1 :: acc
                 end
               | NONE => let val body = simplifyCExp (ctx, body)
                             val exp = C.Abs { contParam = contParam, params = params, body = body, attr = attr }
                             val dec = C.ValDec { exp = exp
                                                , results = [SOME result]
                                                }
                         in dec :: acc
                         end
          end
        | C.ValDec _ => dec :: acc
        | C.RecDec defs =>
          let fun transform ({ name, contParam, params, body, attr }, (wrappers, acc))
                  = let val shouldTransformParams = case CpsUsageAnalysis.getValueUsage (#usage ctx, name) of
                                                        { call = _, project = NEVER, other = NEVER, ... } =>
                                                        (case CpsUsageAnalysis.getValueUsage (#rec_usage ctx, name) of
                                                             { call = _, project = NEVER, other = NEVER, ... } =>
                                                             let val t = List.map (tryUnpackParam (ctx, #usage ctx)) params
                                                             in if List.exists (fn ELIMINATE => true | UNPACK _ => true | KEEP => false) t then
                                                                    SOME t
                                                                else
                                                                    NONE
                                                             end
                                                           | _ => NONE
                                                        )
                                                      | _ => NONE
                    in case shouldTransformParams of
                           SOME paramTransforms =>
                           let val () = #simplificationOccurred (#base ctx) := true
                               val params' = ListPair.foldrEq (fn (p, KEEP, acc) => p :: acc
                                                              | (_, ELIMINATE, acc) => acc
                                                              | (_, UNPACK fields, acc) => List.map #1 fields @ acc
                                                              ) [] (params, paramTransforms)
                               val decs = ListPair.foldrEq (fn (p, UNPACK fields, decs) => C.ValDec { exp = C.Record (List.foldl (fn ((fieldVar, label), map) => Syntax.LabelMap.insert (map, label, C.Var fieldVar)) Syntax.LabelMap.empty fields), results = [SOME p] } :: decs
                                                           | (_, KEEP, decs) => decs
                                                           | (_, ELIMINATE, decs) => decs
                                                           ) [] (params, paramTransforms)
                               val body = case decs of
                                              [] => body
                                            | _ => C.Let { decs = decs, cont = body }
                               val name' = CpsSimplify.renewVId (#base ctx, name)
                               val wrapper = let val k = CpsSimplify.genContSym (#base ctx)
                                                 val params' = List.map (fn p => CpsSimplify.renewVId (#base ctx, p)) params
                                                 val (decs, args) = ListPair.foldrEq (fn (p, KEEP, (decs, args)) => (decs, C.Var p :: args)
                                                                                     | (_, ELIMINATE, acc) => acc
                                                                                     | (p, UNPACK fields, (decs, args)) =>
                                                                                       List.foldr (fn ((v, label), (decs, args)) =>
                                                                                                      let val v = CpsSimplify.renewVId (#base ctx, v)
                                                                                                          val dec = C.ValDec { exp = C.Projection { label = label, record = C.Var p, fieldTypes = Syntax.LabelMap.empty (* dummy *) }
                                                                                                                             , results = [SOME v]
                                                                                                                             }
                                                                                                      in (dec :: decs, C.Var v :: args)
                                                                                                      end
                                                                                                  ) (decs, args) fields
                                                                                     ) ([], []) (params', paramTransforms)
                                             in { contParam = k
                                                , params = params'
                                                , body = C.Let { decs = decs
                                                               , cont = C.App { applied = C.Var name'
                                                                              , cont = k
                                                                              , args = args
                                                                              , attr = {}
                                                                              }
                                                               }
                                                , attr = { isWrapper = true }
                                                }
                                             end
                               val wrappers = TypedSyntax.VIdMap.insert (wrappers, name, wrapper)
                           in (wrappers, { name = name', contParam = contParam, params = params', body = body, attr = attr } :: acc)
                           end
                         | NONE => (wrappers, { name = name, contParam = contParam, params = params, body = body, attr = attr } :: acc)
                    end
              val (wrappers, defs) = List.foldr transform (TypedSyntax.VIdMap.empty, []) defs
              val defs = List.map (fn { name, contParam, params, body, attr } =>
                                      let val body = if TypedSyntax.VIdMap.isEmpty wrappers then
                                                         body
                                                     else
                                                         C.recurseCExp (fn e as C.App { applied = C.Var applied, cont, args, attr } =>
                                                                           (case TypedSyntax.VIdMap.find (wrappers, applied) of
                                                                                NONE => e
                                                                              | SOME { contParam, params, body, attr } =>
                                                                                let val subst = ListPair.foldlEq (fn (p, a, subst) => TypedSyntax.VIdMap.insert (subst, p, a)) TypedSyntax.VIdMap.empty (params, args)
                                                                                    val csubst = C.CVarMap.singleton (contParam, cont)
                                                                                in CpsSimplify.alphaConvert (#base ctx, subst, csubst, body)
                                                                                end
                                                                           )
                                                                       | e => e
                                                                       ) body
                                          val body = simplifyCExp (ctx, body)
                                      in { name = name, contParam = contParam, params = params, body = body, attr = attr }
                                      end
                                  ) defs
              val decs = C.RecDec defs :: acc
          in decs
          end
        | C.ContDec { name, params, body } =>
          let val shouldTransformParams = if #indirect (CpsUsageAnalysis.getContUsage (#cont_usage ctx, name)) = NEVER then
                                              let val t = List.map (tryUnpackContParam (ctx, #usage ctx)) params
                                              in if List.exists (fn ELIMINATE => true | UNPACK _ => true | KEEP => false) t then
                                                     SOME t
                                                 else
                                                     NONE
                                              end
                                          else
                                              NONE
          in case shouldTransformParams of
                 SOME paramTransforms =>
                 let val () = #simplificationOccurred (#base ctx) := true
                     val params' = ListPair.foldrEq (fn (SOME p, KEEP, acc) => p :: acc
                                                    | (SOME _, ELIMINATE, acc) => acc
                                                    | (SOME _, UNPACK fields, acc) => List.map #1 fields @ acc
                                                    | (NONE, _, acc) => acc
                                                    ) [] (params, paramTransforms)
                     val decs = ListPair.foldrEq (fn (p, UNPACK fields, decs) => C.ValDec { exp = C.Record (List.foldl (fn ((fieldVar, label), map) => Syntax.LabelMap.insert (map, label, C.Var fieldVar)) Syntax.LabelMap.empty fields), results = [p] } :: decs
                                                 | (_, KEEP, decs) => decs
                                                 | (_, ELIMINATE, decs) => decs
                                                 ) [] (params, paramTransforms)
                     val body = case decs of
                                    [] => body
                                  | _ => C.Let { decs = decs, cont = body }
                     val body = simplifyCExp (ctx, body)
                     val name' = CpsSimplify.renewCVar (#base ctx, name)
                     val wrapper = let val params' = List.map (Option.map (fn p => CpsSimplify.renewVId (#base ctx, p))) params
                                       val (decs, args) = ListPair.foldrEq (fn (SOME p, KEEP, (decs, args)) => (decs, C.Var p :: args)
                                                                           | (SOME _, ELIMINATE, acc) => acc
                                                                           | (SOME p, UNPACK fields, (decs, args)) =>
                                                                             List.foldr (fn ((v, label), (decs, args)) =>
                                                                                            let val v = CpsSimplify.renewVId (#base ctx, v)
                                                                                                val dec = C.ValDec { exp = C.Projection { label = label, record = C.Var p, fieldTypes = Syntax.LabelMap.empty (* dummy *) }
                                                                                                                   , results = [SOME v]
                                                                                                                   }
                                                                                            in (dec :: decs, C.Var v :: args)
                                                                                            end
                                                                                        ) (decs, args) fields
                                                                           | (NONE, _, acc) => acc
                                                                           ) ([], []) (params', paramTransforms)
                                   in C.ContDec { name = name
                                                , params = params'
                                                , body = C.Let { decs = decs
                                                               , cont = C.AppCont { applied = name'
                                                                                  , args = args
                                                                                  }
                                                               }
                                                }
                                   end
                     val dec = C.ContDec { name = name'
                                         , params = List.map SOME params'
                                         , body = body
                                         }
                 in wrapper :: dec :: acc
                 end
               | NONE =>
                 let val dec = C.ContDec { name = name
                                         , params = params
                                         , body = simplifyCExp (ctx, body)
                                         }
                 in dec :: acc
                 end
          end
        | C.RecContDec defs =>
          let fun transform (name, params, body)
                  = let val shouldTransformParams = if #indirect (CpsUsageAnalysis.getContUsage (#cont_usage ctx, name)) = NEVER then
                                                        let val t = List.map (tryUnpackContParam (ctx, #usage ctx)) params
                                                        in if List.exists (fn ELIMINATE => true | UNPACK _ => true | KEEP => false) t then
                                                               SOME t
                                                           else
                                                               NONE
                                                        end
                                                    else
                                                        NONE
                    in case shouldTransformParams of
                           SOME paramTransforms =>
                           let val () = #simplificationOccurred (#base ctx) := true
                               val params' = ListPair.foldrEq (fn (SOME p, KEEP, acc) => p :: acc
                                                              | (SOME _, ELIMINATE, acc) => acc
                                                              | (SOME _, UNPACK fields, acc) => List.map #1 fields @ acc
                                                              | (NONE, _, acc) => acc
                                                              ) [] (params, paramTransforms)
                               val decs = ListPair.foldrEq (fn (p, UNPACK fields, decs) => C.ValDec { exp = C.Record (List.foldl (fn ((fieldVar, label), map) => Syntax.LabelMap.insert (map, label, C.Var fieldVar)) Syntax.LabelMap.empty fields), results = [p] } :: decs
                                                           | (_, KEEP, decs) => decs
                                                           | (_, ELIMINATE, decs) => decs
                                                           ) [] (params, paramTransforms)
                               val body = case decs of
                                              [] => body
                                            | _ => C.Let { decs = decs, cont = body }
                               val name' = CpsSimplify.renewCVar (#base ctx, name)
                               val wrapper = let val params' = List.map (Option.map (fn p => CpsSimplify.renewVId (#base ctx, p))) params
                                                 val (decs, args) = ListPair.foldrEq (fn (SOME p, KEEP, (decs, args)) => (decs, C.Var p :: args)
                                                                                     | (SOME _, ELIMINATE, acc) => acc
                                                                                     | (SOME p, UNPACK fields, (decs, args)) =>
                                                                                       List.foldr (fn ((v, label), (decs, args)) =>
                                                                                                      let val v = CpsSimplify.renewVId (#base ctx, v)
                                                                                                          val dec = C.ValDec { exp = C.Projection { label = label, record = C.Var p, fieldTypes = Syntax.LabelMap.empty (* dummy *) }
                                                                                                                             , results = [SOME v]
                                                                                                                             }
                                                                                                      in (dec :: decs, C.Var v :: args)
                                                                                                      end
                                                                                                  ) (decs, args) fields
                                                                                     | (NONE, _, acc) => acc
                                                                                     ) ([], []) (params', paramTransforms)
                                             in (params', SOME (C.Let { decs = decs
                                                                      , cont = C.AppCont { applied = name'
                                                                                         , args = args
                                                                                         }
                                                                      }
                                                               )
                                                )
                                             end
                           in { origName = name, body = body, newName = name', newParams = List.map SOME params', inline = wrapper }
                           end
                         | NONE => { origName = name, body = body, newName = name, newParams = params, inline = (params, NONE) }
                    end
              val defs' = List.map transform defs
              val dec = C.RecContDec (List.map (fn { newName, newParams, body, ... } => (newName, newParams, simplifyCExp (ctx, body))) defs')
          in dec :: acc
          end
        | C.ESImportDec { pure, specs, moduleName } => dec :: acc
and simplifyCExp (ctx : Context, e)
    = case e of
          C.Let { decs, cont } =>
          let val revDecs = List.foldl (simplifyDec ctx) [] decs
          in CpsTransform.prependRevDecs (revDecs, simplifyCExp (ctx, cont))
          end
        | C.App _ => e
        | C.AppCont _ => e
        | C.If { cond, thenCont, elseCont } =>
          C.If { cond = cond
               , thenCont = simplifyCExp (ctx, thenCont)
               , elseCont = simplifyCExp (ctx, elseCont)
               }
        | C.Handle { body, handler = (e, h), successfulExitIn, successfulExitOut } =>
          C.Handle { body = simplifyCExp (ctx, body)
                   , handler = (e, simplifyCExp (ctx, h))
                   , successfulExitIn = successfulExitIn
                   , successfulExitOut = successfulExitOut
                   }
        | C.Unreachable => e
fun goCExp (ctx : CpsSimplify.Context, exp)
    = let val usage = CpsUsageAnalysis.analyze exp
          val ctx' = { base = ctx
                     , usage = #usage usage
                     , rec_usage = #rec_usage usage
                     , cont_usage = #cont_usage usage
                     , cont_rec_usage = #cont_rec_usage usage
                     }
      in simplifyCExp (ctx', exp)
      end
end (* local *)
end (* structure CpsUnpackRecordParameter *)
end; (* local *)
