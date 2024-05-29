(*
 * Copyright (c) 2024 ARATA Mizuki
 * This file is part of LunarML.
 *)
(*
 * This module does:
 *  * Dead code elimination,
 *  * Decompose recursive definitions,
 *  * Convert recursive function to loop,
 *  * Unpack tuple parameters,
 *  * Eliminate constant ref-cells.
 *)
structure CpsDeadCodeElimination : sig
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
               , dead_code_analysis : CpsDeadCodeAnalysis.usage
               }
datatype param_transform = KEEP | ELIMINATE | UNPACK of (C.Var * Syntax.Label) list
fun tryUnpackParam (ctx : Context, usage) param
    = case CpsUsageAnalysis.getValueUsage (usage, param) of
          { call = NEVER, project = NEVER, ref_read = NEVER, ref_write = NEVER, other = NEVER, ... } => ELIMINATE
        | { call = NEVER, project = _, ref_read = NEVER, ref_write = NEVER, other = NEVER, labels, ... } =>
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
           { call = NEVER, project = NEVER, ref_read = NEVER, ref_write = NEVER, other = NEVER, ... } => ELIMINATE
         | { call = NEVER, project = _, ref_read = NEVER, ref_write = NEVER, other = NEVER, labels, ... } =>
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
datatype simplify_result = VALUE of C.Value
                         | SIMPLE_EXP of C.SimpleExp
                         | NOT_SIMPLIFIED
(*: val simplifySimpleExp : CpsUsageAnalysis.usage_table * { exp : C.SimpleExp option, isDiscardableFunction : bool } TypedSyntax.VIdMap.map * C.SimpleExp -> simplify_result *)
fun simplifySimpleExp (usage, env, C.PrimOp { primOp, tyargs = _, args })
    = (case (primOp, args) of
           (F.PrimCall P.Ref_read, [C.Var v]) =>
           let val u = CpsUsageAnalysis.getValueUsage (usage, v)
           in case (#ref_write u, #other u) of
                  (CpsUsageAnalysis.NEVER, CpsUsageAnalysis.NEVER) =>
                  (case TypedSyntax.VIdMap.find (env, v) of
                       SOME { exp = SOME (C.PrimOp { primOp = F.PrimCall P.Ref_ref, tyargs = _, args = [initialValue] }), ... } =>
                       VALUE initialValue
                     | _ => NOT_SIMPLIFIED
                  )
                | _ => NOT_SIMPLIFIED
           end
         | _ => NOT_SIMPLIFIED
      )
  | simplifySimpleExp (_, _, _) = NOT_SIMPLIFIED
and simplifyDec (ctx : Context, appliedCont : C.CVar option) (dec, (env, cenv, subst, csubst, acc : C.Dec list))
    = case dec of
          C.ValDec { exp, results } =>
          let val exp = CpsSimplify.substSimpleExp (subst, csubst, exp)
              val results = List.map (fn result as SOME name => if CpsDeadCodeAnalysis.isUsed (#dead_code_analysis ctx, name) then
                                                                    result
                                                                else
                                                                    NONE
                                     | NONE => NONE
                                     ) results
          in case simplifySimpleExp (#usage ctx, env, exp) of
                 VALUE v => let val () = #simplificationOccurred (#base ctx) := true
                                val subst = case results of
                                                [SOME result] => TypedSyntax.VIdMap.insert (subst, result, v)
                                              | [NONE] => subst
                                              | _ => subst (* should not occur *)
                            in (env, cenv, subst, csubst, acc)
                            end
               | simplified =>
                 let val () = case simplified of
                                  SIMPLE_EXP _ => #simplificationOccurred (#base ctx) := true
                                | VALUE _ => #simplificationOccurred (#base ctx) := true (* shoud not occur *)
                                | NOT_SIMPLIFIED => ()
                     val exp = case simplified of
                                   SIMPLE_EXP exp => exp
                                 | _ => exp
                 in case (exp, results) of
                        (C.Abs { contParam, params, body, attr }, [SOME result]) =>
                        (case CpsUsageAnalysis.getValueUsage (#usage ctx, result) of
                             { call = NEVER, project = NEVER, ref_read = NEVER, ref_write = NEVER, other = NEVER, returnConts = _, labels = _ } => (env, cenv, subst, csubst, acc)
                           | { call = ONCE, project = NEVER, ref_read = NEVER, ref_write = NEVER, other = NEVER, returnConts = _, labels = _ } =>
                             let val body = simplifyCExp (ctx, env, cenv, subst, csubst, body)
                                 val env = TypedSyntax.VIdMap.insert (env, result, { exp = SOME (C.Abs { contParam = contParam, params = params, body = body, attr = attr }), isDiscardableFunction = CpsSimplify.isDiscardableExp (env, body) })
                             in (env, cenv, subst, csubst, acc)
                             end
                           | u => let val shouldTransformParams = case u of
                                                                      { call = _, project = NEVER, ref_read = NEVER, ref_write = NEVER, other = NEVER, ... } =>
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
                                             val body = simplifyCExp (ctx, env, cenv, subst, csubst, body)
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
                                                                                                      }
                                                                                       }
                                                                        , attr = { isWrapper = true }
                                                                        }
                                                               end
                                             val env = TypedSyntax.VIdMap.insert (env, result, { exp = SOME wrapperBody, isDiscardableFunction = CpsSimplify.isDiscardableExp (env, body) })
                                             val dec = C.ValDec { exp = exp
                                                                , results = [SOME result']
                                                                }
                                         in (env, cenv, subst, csubst, dec :: acc)
                                         end
                                       | NONE => let val body = simplifyCExp (ctx, env, cenv, subst, csubst, body)
                                                     val exp = C.Abs { contParam = contParam, params = params, body = body, attr = attr }
                                                     val env = TypedSyntax.VIdMap.insert (env, result, { exp = NONE, isDiscardableFunction = CpsSimplify.isDiscardableExp (env, body) })
                                                     val dec = C.ValDec { exp = exp
                                                                        , results = [SOME result]
                                                                        }
                                                 in (env, cenv, subst, csubst, dec :: acc)
                                                 end
                                  end
                        )
                      | _ => (case (C.isDiscardable exp, results) of
                                  (true, [NONE]) => (env, cenv, subst, csubst, acc)
                                | (_, [SOME result]) => let val dec = C.ValDec { exp = exp
                                                                               , results = [SOME result]
                                                                               }
                                                            val env = TypedSyntax.VIdMap.insert (env, result, { exp = SOME exp, isDiscardableFunction = false })
                                                        in (env, cenv, subst, csubst, dec :: acc)
                                                        end
                                | _ => let val dec = C.ValDec { exp = exp
                                                              , results = results
                                                              }
                                       in (env, cenv, subst, csubst, dec :: acc)
                                       end
                             )
                 end
          end
        | C.RecDec defs =>
          if List.exists (fn { name, ... } => CpsDeadCodeAnalysis.isUsed (#dead_code_analysis ctx, name)) defs then
              let fun transform ({ name, contParam, params, body, attr }, (env, acc))
                      = let val shouldTransformParams = case CpsUsageAnalysis.getValueUsage (#usage ctx, name) of
                                                            { call = _, project = NEVER, ref_read = NEVER, ref_write = NEVER, other = NEVER, ... } =>
                                                            (case CpsUsageAnalysis.getValueUsage (#rec_usage ctx, name) of
                                                                 { call = _, project = NEVER, ref_read = NEVER, ref_write = NEVER, other = NEVER, ... } =>
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
                                                 in C.Abs { contParam = k
                                                          , params = params'
                                                          , body = C.Let { decs = decs
                                                                         , cont = C.App { applied = C.Var name'
                                                                                        , cont = k
                                                                                        , args = args
                                                                                        }
                                                                         }
                                                          , attr = { isWrapper = true }
                                                          }
                                                 end
                                   val env = TypedSyntax.VIdMap.insert (env, name, { exp = SOME wrapper, isDiscardableFunction = false })
                               in (env, { name = name', contParam = contParam, params = params', body = body, attr = attr } :: acc)
                               end
                             | NONE => (env, { name = name, contParam = contParam, params = params, body = body, attr = attr } :: acc)
                        end
                  val (env, defs) = List.foldr transform (env, []) defs
                  val defs = List.map (fn { name, contParam, params, body, attr } => { name = name, contParam = contParam, params = params, body = simplifyCExp (ctx, env, cenv, subst, csubst, body), attr = attr }) defs
                  fun tryConvertToLoop (def as { name, contParam, params, body, attr })
                      = if C.CVarSet.member (#returnConts (CpsUsageAnalysis.getValueUsage (#rec_usage ctx, name)), contParam) then
                            let val loop = CpsSimplify.genContSym (#base ctx)
                                val params' = List.map (fn v => CpsSimplify.renewVId (#base ctx, v)) params
                                val body' = C.recurseCExp (fn call as C.App { applied = C.Var applied, cont, args } =>
                                                              if applied = name andalso cont = contParam then
                                                                  C.AppCont { applied = loop, args = args }
                                                              else
                                                                  call
                                                          | c => c
                                                          ) body
                                val body'' = C.Let { decs = [C.RecContDec [(loop, List.map SOME params, body')]]
                                                   , cont = C.AppCont { applied = loop, args = List.map C.Var params' }
                                                   }
                            in { name = name, contParam = contParam, params = params', body = body'', attr = attr }
                            end
                        else
                            def
                  val defs = List.map tryConvertToLoop defs
                  val defined = List.foldl (fn ({ name, ... }, set) => TypedSyntax.VIdSet.add (set, name)) TypedSyntax.VIdSet.empty defs
                  val map = List.foldl (fn (def as { name, body, ... }, map) =>
                                           TypedSyntax.VIdMap.insert (map, name, { def = def
                                                                                 , dests = TypedSyntax.VIdSet.intersection (C.freeVarsInExp (TypedSyntax.VIdSet.empty, body, TypedSyntax.VIdSet.empty), defined)
                                                                                 }
                                                                     )
                                       ) TypedSyntax.VIdMap.empty defs
                  val sccs = TypedSyntax.VIdSCC.components (#dests, map)
                  val decs = List.foldl (fn (scc, decs) =>
                                            let val dec = case TypedSyntax.VIdSet.listItems scc of
                                                              [vid] => let val { def as { name, contParam, params, body, attr }, dests } = TypedSyntax.VIdMap.lookup (map, vid)
                                                                       in if TypedSyntax.VIdSet.member (dests, vid) then
                                                                              C.RecDec [def]
                                                                          else
                                                                              C.ValDec { exp = C.Abs { contParam = contParam, params = params, body = body, attr = attr }, results = [SOME name] }
                                                                       end
                                                            | scc => C.RecDec (List.map (fn vid => #def (TypedSyntax.VIdMap.lookup (map, vid))) scc)
                                            in dec :: decs
                                            end
                                        ) acc sccs
              in (env, cenv, subst, csubst, decs)
              end
          else
              ( #simplificationOccurred (#base ctx) := true
              ; (env, cenv, subst, csubst, acc)
              )
        | C.ContDec { name, params, body } =>
          (case CpsUsageAnalysis.getContUsage (#cont_usage ctx, name) of
               { direct = NEVER, indirect = NEVER } =>
               ( #simplificationOccurred (#base ctx) := true
               ; (env, cenv, subst, csubst, acc)
               )
             | { direct = direct_usage, indirect = indirect_usage } =>
               if direct_usage = ONCE andalso indirect_usage = NEVER andalso ((case appliedCont of SOME c => c = name | NONE => false) orelse CpsSimplify.sizeOfCExp (body, 10) >= 0) then (* Inline small continuations *)
                   let val () = #simplificationOccurred (#base ctx) := true
                       val body = simplifyCExp (ctx, env, cenv, subst, csubst, body)
                       val cenv = C.CVarMap.insert (cenv, name, (params, SOME body))
                   in (env, cenv, subst, csubst, acc)
                   end
               else
                   let val shouldTransformParams = if indirect_usage = NEVER then
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
                              val body = simplifyCExp (ctx, env, cenv, subst, csubst, body)
                              val name' = CpsSimplify.renewCVar (#base ctx, name)
                              val wrapperBody = let val params' = List.map (Option.map (fn p => CpsSimplify.renewVId (#base ctx, p))) params
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
                              val cenv = C.CVarMap.insert (cenv, name, wrapperBody)
                              val dec = C.ContDec { name = name'
                                                  , params = List.map SOME params'
                                                  , body = body
                                                  }
                          in (env, cenv, subst, csubst, dec :: acc)
                          end
                        | NONE =>
                          let val body = simplifyCExp (ctx, env, cenv, subst, csubst, body)
                              val params = List.map (fn SOME p => (case CpsUsageAnalysis.getValueUsage (#usage ctx, p) of
                                                                       { call = NEVER, project = NEVER, ref_read = NEVER, ref_write = NEVER, other = NEVER, ... } => NONE
                                                                     | _ => SOME p
                                                                  )
                                                    | NONE => NONE
                                                    ) params
                              val cenv = C.CVarMap.insert (cenv, name, (params, NONE))
                              val dec = C.ContDec { name = name
                                                  , params = params
                                                  , body = body
                                                  }
                          in (env, cenv, subst, csubst, dec :: acc)
                          end
                   end
          )
        | C.RecContDec defs =>
          if List.all (fn (f, _, _) => CpsUsageAnalysis.getContUsage (#cont_usage ctx, f) = { direct = NEVER, indirect = NEVER }) defs then
              ( #simplificationOccurred (#base ctx) := true
              ; (env, cenv, subst, csubst, acc)
              )
          else
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
                               in { origName = name, body = body, newName = name', newParams = List.map SOME params', env = env, inline = wrapper }
                               end
                             | NONE => { origName = name, body = body, env = env, newName = name, newParams = params, inline = (params, NONE) }
                        end
                  val defs' = List.map transform defs
                  val cenv = List.foldl (fn ({ origName, inline, ... }, cenv) => C.CVarMap.insert (cenv, origName, inline)) cenv defs'
                  val dec = C.RecContDec (List.map (fn { newName, newParams, env, body, ... } => (newName, newParams, simplifyCExp (ctx, env, cenv, subst, csubst, body))) defs')
              in (env, cenv, subst, csubst, dec :: acc)
              end
        | C.ESImportDec { pure, specs, moduleName } =>
          let val specs = List.filter (fn (_, vid) => CpsDeadCodeAnalysis.isUsed (#dead_code_analysis ctx, vid)) specs
          in if pure andalso List.null specs then
                 (env, cenv, subst, csubst, acc)
             else
                 let val dec = C.ESImportDec { pure = pure, specs = specs, moduleName = moduleName }
                 in (env, cenv, subst, csubst, dec :: acc)
                 end
          end
and simplifyCExp (ctx : Context, env : CpsSimplify.value_info TypedSyntax.VIdMap.map, cenv : ((C.Var option) list * C.CExp option) C.CVarMap.map, subst : C.Value TypedSyntax.VIdMap.map, csubst : C.CVar C.CVarMap.map, e)
    = case e of
          C.Let { decs, cont } =>
          let val appliedCont = case cont of
                                    C.AppCont { applied, args = _ } => SOME applied
                                  | _ => NONE
              val (env, cenv, subst, csubst, revDecs) = List.foldl (simplifyDec (ctx, appliedCont)) (env, cenv, subst, csubst, []) decs
          in CpsTransform.prependRevDecs (revDecs, simplifyCExp (ctx, env, cenv, subst, csubst, cont))
          end
        | C.App { applied, cont, args } =>
          let val applied = CpsSimplify.substValue subst applied
              val cont = CpsSimplify.substCVar csubst cont
              val args = List.map (CpsSimplify.substValue subst) args
          in case applied of
                 C.Var applied =>
                 (case TypedSyntax.VIdMap.find (env, applied) of
                      SOME { exp = SOME (C.Abs { contParam, params, body, attr = _ }), ... } =>
                      let val () = #simplificationOccurred (#base ctx) := true
                          val subst = ListPair.foldlEq (fn (p, a, subst) => TypedSyntax.VIdMap.insert (subst, p, a)) subst (params, args)
                          val csubst = C.CVarMap.insert (csubst, contParam, cont)
                          val canOmitAlphaConversion = case CpsUsageAnalysis.getValueUsage (#usage ctx, applied) of
                                                           { call = ONCE, project = NEVER, ref_read = NEVER, ref_write = NEVER, other = NEVER, returnConts = _, labels = _ } =>
                                                           (case CpsUsageAnalysis.getValueUsage (#rec_usage ctx, applied) of
                                                                { call = NEVER, project = NEVER, ref_read = NEVER, ref_write = NEVER, other = NEVER, returnConts = _, labels = _ } => true
                                                              | _ => false
                                                           )
                                                         | _ => false
                      in if canOmitAlphaConversion then
                             CpsSimplify.substCExp (subst, csubst, body) (* no alpha conversion *)
                         else
                             CpsSimplify.alphaConvert (#base ctx, subst, csubst, body)
                      end
                    | SOME { exp = _, isDiscardableFunction = true } =>
                      (case C.CVarMap.find (cenv, cont) of
                           SOME (params, _) => if not (List.exists Option.isSome params) then
                                                   ( #simplificationOccurred (#base ctx) := true
                                                   ; C.AppCont { applied = cont, args = List.map (fn _ => C.Unit (* dummy *)) params }
                                                   )
                                               else
                                                   C.App { applied = C.Var applied, cont = cont, args = args }
                         | _ => C.App { applied = C.Var applied, cont = cont, args = args }
                      )
                    | _ => C.App { applied = C.Var applied, cont = cont, args = args }
                 )
               | _ => C.App { applied = applied, cont = cont, args = args } (* should not occur *)
          end
        | C.AppCont { applied, args } =>
          let val applied = CpsSimplify.substCVar csubst applied
              val args = List.map (CpsSimplify.substValue subst) args
          in case C.CVarMap.find (cenv, applied) of
                 SOME (params, SOME body) =>
                 let val () = #simplificationOccurred (#base ctx) := true
                     val subst = ListPair.foldlEq (fn (SOME p, a, subst) => TypedSyntax.VIdMap.insert (subst, p, a) | (NONE, _, subst) => subst) subst (params, args)
                     val canOmitAlphaConversion = case CpsUsageAnalysis.getContUsage (#cont_usage ctx, applied) of
                                                      { direct = ONCE, indirect = NEVER } => (case CpsUsageAnalysis.getContUsage (#cont_rec_usage ctx, applied) of
                                                                                                  { direct = NEVER, indirect = NEVER } => true
                                                                                                | _ => false
                                                                                             )
                                                    | _ => false
                 in if canOmitAlphaConversion then
                        CpsSimplify.substCExp (subst, csubst, body) (* no alpha conversion *)
                    else
                        CpsSimplify.alphaConvert (#base ctx, subst, csubst, body)
                 end
               | _ => C.AppCont { applied = applied, args = args }
          end
        | C.If { cond, thenCont, elseCont } =>
          (case CpsSimplify.substValue subst cond of
               C.BoolConst true => (#simplificationOccurred (#base ctx) := true; simplifyCExp (ctx, env, cenv, subst, csubst, thenCont))
             | C.BoolConst false => (#simplificationOccurred (#base ctx) := true; simplifyCExp (ctx, env, cenv, subst, csubst, elseCont))
             | cond => C.If { cond = cond
                            , thenCont = simplifyCExp (ctx, env, cenv, subst, csubst, thenCont)
                            , elseCont = simplifyCExp (ctx, env, cenv, subst, csubst, elseCont)
                            }
          )
        | C.Handle { body, handler = (e, h), successfulExitIn, successfulExitOut } =>
          C.Handle { body = simplifyCExp (ctx, env, C.CVarMap.empty (* do not inline across 'handle' *), subst, csubst, body)
                   , handler = (e, simplifyCExp (ctx, env, cenv, subst, csubst, h))
                   , successfulExitIn = successfulExitIn
                   , successfulExitOut = CpsSimplify.substCVar csubst successfulExitOut
                   }
        | C.Unreachable => e
fun goCExp (ctx : CpsSimplify.Context, exp)
    = let val usage = CpsUsageAnalysis.analyze exp
          val ctx' = { base = ctx
                     , usage = #usage usage
                     , rec_usage = #rec_usage usage
                     , cont_usage = #cont_usage usage
                     , cont_rec_usage = #cont_rec_usage usage
                     , dead_code_analysis = #dead_code_analysis usage
                     }
      in simplifyCExp (ctx', TypedSyntax.VIdMap.empty, C.CVarMap.empty, TypedSyntax.VIdMap.empty, C.CVarMap.empty, exp)
      end
end (* local *)
end; (* structure CpsDeadCodeElimination *)
