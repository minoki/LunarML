(*
 * Copyright (c) 2024 ARATA Mizuki
 * This file is part of LunarML.
 *)
(*
 * Unpack record parameters,
 *)
local
  structure CpsUsageAnalysis :>
  sig
    datatype frequency = NEVER | ONCE | MANY
    type usage =
      { call: frequency
      , project: frequency
      , other: frequency
      , labels: (string option) Syntax.LabelMap.map
      }
    type cont_usage = {indirect: frequency}
    val neverUsed: usage
    val neverUsedCont: cont_usage
    type usage_table
    type cont_usage_table
    val getValueUsage: usage_table * TypedSyntax.VId -> usage
    val getContUsage: cont_usage_table * CSyntax.CVar -> cont_usage
    val analyze:
      CSyntax.Stat
      -> { usage: usage_table
         , rec_usage: usage_table
         , cont_usage: cont_usage_table
         , cont_rec_usage: cont_usage_table
         }
  end =
  struct
    local structure C = CSyntax
    in
      datatype frequency = NEVER | ONCE | MANY
      fun oneMore NEVER = ONCE
        | oneMore ONCE = MANY
        | oneMore (many as MANY) = many
      type usage =
        { call: frequency
        , project: frequency
        , other: frequency
        , labels: (string option) Syntax.LabelMap.map
        }
      type cont_usage = {indirect: frequency}
      val neverUsed: usage =
        { call = NEVER
        , project = NEVER
        , other = NEVER
        , labels = Syntax.LabelMap.empty
        }
      val neverUsedCont: cont_usage = {indirect = NEVER}
      type usage_table = (usage ref) TypedSyntax.VIdTable.hash_table
      type cont_usage_table = (cont_usage ref) CSyntax.CVarTable.hash_table
      fun getValueUsage (table: usage_table, v) =
        case TypedSyntax.VIdTable.find table v of
          SOME r => !r
        | NONE =>
            { call = MANY
            , project = MANY
            , other = MANY
            , labels = Syntax.LabelMap.empty
            } (* unknown *)
      fun getContUsage (table: cont_usage_table, c) =
        case CSyntax.CVarTable.find table c of
          SOME r => !r
        | NONE => {indirect = MANY} (* unknown *)
      fun useValue env v =
        case C.extractVarFromValue v of
          NONE => ()
        | SOME var =>
            case TypedSyntax.VIdTable.find env var of
              SOME r =>
                let
                  val {call, project, other, labels} = !r
                in
                  r
                  :=
                  { call = call
                  , project = project
                  , other = oneMore other
                  , labels = labels
                  }
                end
            | NONE => ()
      fun useValueAsCallee (env, v) =
        case C.extractVarFromValue v of
          NONE => ()
        | SOME var =>
            case TypedSyntax.VIdTable.find env var of
              SOME r =>
                let
                  val {call, project, other, labels} = !r
                in
                  r
                  :=
                  { call = oneMore call
                  , project = project
                  , other = other
                  , labels = labels
                  }
                end
            | NONE => ()
      fun useValueAsRecord (env, label, result, v) =
        case C.extractVarFromValue v of
          NONE => ()
        | SOME var =>
            case TypedSyntax.VIdTable.find env var of
              SOME r =>
                let
                  val {call, project, other, labels} = !r
                  val result' =
                    case result of
                      SOME (TypedSyntax.MkVId (name, _)) => SOME name
                    | NONE => NONE
                  fun mergeOption (x as SOME _, _) = x
                    | mergeOption (NONE, y) = y
                in
                  r
                  :=
                  { call = call
                  , project = oneMore project
                  , other = other
                  , labels =
                      Syntax.LabelMap.insertWith mergeOption
                        (labels, label, result')
                  }
                end
            | NONE => ()
      fun useContVarIndirect cenv (v: C.CVar) =
        (case C.CVarTable.find cenv v of
           SOME r =>
             let val {indirect} = !r
             in r := {indirect = oneMore indirect}
             end
         | NONE => ())
      fun useContVarDirect _ (_: C.CVar) = ()
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
        fun goSimpleExp
              (env, _, _, _, _, C.PrimOp {primOp = _, tyargs = _, args}) =
              List.app (useValue env) args
          | goSimpleExp (env, _, _, _, _, C.Record fields) =
              Syntax.LabelMap.app (useValue env) fields
          | goSimpleExp (_, _, _, _, _, C.ExnTag {name = _, payloadTy = _}) = ()
          | goSimpleExp
              ( env
              , _
              , _
              , _
              , results
              , C.Projection {label, record, fieldTypes = _}
              ) =
              (case results of
                 [(result, _)] => useValueAsRecord (env, label, result, record)
               | _ => () (* should not occur *))
          | goSimpleExp
              ( env
              , renv
              , cenv
              , crenv
              , _
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
            ( goSimpleExp (env, renv, cenv, crenv, results, exp)
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
           | C.UnpackDec {tyVar = _, kind = _, vid, unpackedTy = _, package} =>
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
          | C.App {applied, cont, tyArgs = _, args, attr = _} =>
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
          }
        end
    end (* local *)
  end (* strucuture CpsUsageAnalysis *)
in
  structure CpsUnpackRecordParameter:
  sig
    val goStat: CpsSimplify.Context * CSyntax.Stat -> CSyntax.Stat
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
        }
      datatype param_transform =
        KEEP
      | ELIMINATE
      | UNPACK of (C.Var * Syntax.Label * FSyntax.Ty) list
      fun tryUnpackParam (ctx: Context, usage) (param, recordTy) =
        case CpsUsageAnalysis.getValueUsage (usage, param) of
          {call = NEVER, project = NEVER, other = NEVER, labels = _} =>
            ELIMINATE
        | {call = NEVER, project = _, other = NEVER, labels} =>
            UNPACK
              (Syntax.LabelMap.foldri
                 (fn (label, optName, acc) =>
                    let
                      val name =
                        case optName of
                          SOME name => name
                        | NONE =>
                            case label of
                              Syntax.IdentifierLabel name => name
                            | Syntax.NumericLabel n => "_" ^ Int.toString n
                      val fieldTy =
                        case FSyntax.weakNormalizeTy recordTy of
                          FSyntax.RecordType fieldTypes =>
                            (case Syntax.LabelMap.find (fieldTypes, label) of
                               SOME fieldTy => fieldTy
                             | NONE => raise Fail "missing field")
                        | anyTy as FSyntax.AnyType FSyntax.TypeKind => anyTy
                        | recordTy =>
                            raise Fail
                              ("invalid record type: "
                               ^ Printer.build (FPrinter.doTy 0 recordTy) ^ "; "
                               ^ TypedSyntax.print_VId param)
                    in
                      (CpsSimplify.newVId (#base ctx, name), label, fieldTy)
                      :: acc
                    end) [] labels)
        | _ => KEEP
      fun tryUnpackContParam (ctx: Context, usage) (SOME param, recordTy) =
            (case CpsUsageAnalysis.getValueUsage (usage, param) of
               {call = NEVER, project = NEVER, other = NEVER, labels = _} =>
                 ELIMINATE
             | {call = NEVER, project = _, other = NEVER, labels} =>
                 UNPACK
                   (Syntax.LabelMap.foldri
                      (fn (label, optName, acc) =>
                         let
                           val name =
                             case optName of
                               SOME name => name
                             | NONE =>
                                 case label of
                                   Syntax.IdentifierLabel name => name
                                 | Syntax.NumericLabel n => "_" ^ Int.toString n
                           val fieldTy =
                             case FSyntax.weakNormalizeTy recordTy of
                               FSyntax.RecordType fieldTypes =>
                                 (case Syntax.LabelMap.find (fieldTypes, label) of
                                    SOME fieldTy => fieldTy
                                  | NONE => raise Fail "missing field")
                             | anyTy as FSyntax.AnyType FSyntax.TypeKind =>
                                 anyTy
                             | recordTy =>
                                 raise Fail
                                   ("invalid record type: "
                                    ^ Printer.build (FPrinter.doTy 0 recordTy))
                         in
                           ( CpsSimplify.newVId (#base ctx, name)
                           , label
                           , fieldTy
                           ) :: acc
                         end) [] labels)
             | _ => KEEP)
        | tryUnpackContParam (_, _) (NONE, _) = ELIMINATE
      fun simplifyDec (ctx: Context) (dec, acc: C.Dec list) =
        case dec of
          C.ValDec
            { exp =
                C.Abs
                  { contParam
                  , tyParams
                  , params
                  , body
                  , resultTy
                  , attr as {typeOnly, ...}
                  }
            , results = [(SOME name, fnTy)]
            } =>
            let
              val shouldTransformParams =
                case CpsUsageAnalysis.getValueUsage (#usage ctx, name) of
                  {call = _, project = NEVER, other = NEVER, ...} =>
                    let
                      val t = List.map (tryUnpackParam (ctx, #usage ctx)) params
                    in
                      if
                        List.exists
                          (fn ELIMINATE => true
                            | UNPACK _ => true
                            | KEEP => false) t
                      then SOME t
                      else NONE
                    end
                | _ => NONE
            in
              case shouldTransformParams of
                SOME paramTransforms =>
                  let
                    val () = #simplificationOccurred (#base ctx) := true
                    val workerParams =
                      ListPair.foldrEq
                        (fn (p, KEEP, acc) => p :: acc
                          | (_, ELIMINATE, acc) => acc
                          | (_, UNPACK fields, acc) =>
                           List.map (fn (v, _, ty) => (v, ty)) fields @ acc) []
                        (params, paramTransforms)
                    local
                      val workerDecs =
                        ListPair.foldrEq
                          (fn ((p, _), UNPACK fields, decs) =>
                             let
                               val expMap =
                                 List.foldl
                                   (fn ((fieldVar, label, _), map) =>
                                      Syntax.LabelMap.insert
                                        (map, label, C.Var fieldVar))
                                   Syntax.LabelMap.empty fields
                               val tyMap =
                                 List.foldl
                                   (fn ((_, label, ty), map) =>
                                      Syntax.LabelMap.insert (map, label, ty))
                                   Syntax.LabelMap.empty fields
                             in
                               C.ValDec
                                 { exp = C.Record expMap
                                 , results =
                                     [(SOME p, FSyntax.RecordType tyMap)]
                                 } :: decs
                             end
                            | (_, KEEP, decs) => decs
                            | (_, ELIMINATE, decs) => decs) []
                          (params, paramTransforms)
                      val workerBody =
                        case workerDecs of
                          [] => body
                        | _ => C.Let {decs = workerDecs, cont = body}
                      val workerBody = simplifyStat (ctx, workerBody)
                    in
                      val worker = C.Abs
                        { contParam = contParam
                        , tyParams = tyParams
                        , params = workerParams
                        , body = workerBody
                        , resultTy = resultTy
                        , attr = attr
                        }
                    end
                    val workerName = CpsSimplify.renewVId (#base ctx, name)
                    val wrapper =
                      let
                        val k = CpsSimplify.genContSym (#base ctx)
                        val (tysubst, wrapperTyParams) =
                          List.foldr
                            (fn ((tv, kind), (tysubst, acc)) =>
                               let
                                 val tv' =
                                   CpsSimplify.renewTyVar (#base ctx, tv)
                               in
                                 ( TypedSyntax.TyVarMap.insert
                                     (tysubst, tv, FSyntax.TyVar tv')
                                 , (tv', kind) :: acc
                                 )
                               end) (TypedSyntax.TyVarMap.empty, []) tyParams
                        val substTy = #doTy (FSyntax.lazySubstTy tysubst)
                        val wrapperParams =
                          List.map
                            (fn (p, ty) =>
                               (CpsSimplify.renewVId (#base ctx, p), substTy ty))
                            params
                        val (decs, args) =
                          ListPair.foldrEq
                            (fn ((p, _), KEEP, (decs, args)) =>
                               (decs, C.Var p :: args)
                              | (_, ELIMINATE, acc) => acc
                              | ((p, _), UNPACK fields, (decs, args)) =>
                               List.foldr
                                 (fn ((v, label, ty), (decs, args)) =>
                                    let
                                      val v =
                                        CpsSimplify.renewVId (#base ctx, v)
                                      val dec = C.ValDec
                                        { exp =
                                            C.Projection
                                              { label = label
                                              , record = C.Var p
                                              , fieldTypes =
                                                  Syntax.LabelMap.empty (* dummy *)
                                              }
                                        , results = [(SOME v, substTy ty)]
                                        }
                                    in
                                      (dec :: decs, C.Var v :: args)
                                    end) (decs, args) fields) ([], [])
                            (wrapperParams, paramTransforms)
                      in
                        C.Abs
                          { contParam = k
                          , tyParams = wrapperTyParams
                          , params = wrapperParams
                          , body = C.Let
                              { decs = decs
                              , cont = C.App
                                  { applied = C.Var workerName
                                  , cont = k
                                  , tyArgs =
                                      List.map (fn (tv, _) => FSyntax.TyVar tv)
                                        wrapperTyParams
                                  , args = args
                                  , attr = {typeOnly = typeOnly}
                                  }
                              }
                          , resultTy = substTy resultTy
                          , attr = {alwaysInline = true, typeOnly = typeOnly}
                          }
                      end
                    val workerTy = FSyntax.MultiFnType
                      (List.map #2 workerParams, resultTy)
                    val workerTy =
                      List.foldr
                        (fn ((tv, kind), ty) =>
                           FSyntax.ForallType (tv, kind, ty)) workerTy tyParams
                    val workerDec = C.ValDec
                      {exp = worker, results = [(SOME workerName, workerTy)]}
                    val wrapperDec = C.ValDec
                      {exp = wrapper, results = [(SOME name, fnTy)]}
                  in
                    wrapperDec :: workerDec :: acc
                  end
              | NONE =>
                  let
                    val body = simplifyStat (ctx, body)
                    val exp = C.Abs
                      { contParam = contParam
                      , tyParams = tyParams
                      , params = params
                      , body = body
                      , resultTy = resultTy
                      , attr = attr
                      }
                    val dec = C.ValDec
                      {exp = exp, results = [(SOME name, fnTy)]}
                  in
                    dec :: acc
                  end
            end
        | C.ValDec _ => dec :: acc
        | C.RecDec defs =>
            let
              fun transform
                ( { name
                  , contParam
                  , tyParams
                  , params
                  , body
                  , resultTy
                  , attr as {typeOnly, ...}
                  }
                , (wrappers, acc)
                ) =
                let
                  val shouldTransformParams =
                    case CpsUsageAnalysis.getValueUsage (#usage ctx, name) of
                      {call = _, project = NEVER, other = NEVER, ...} =>
                        (case
                           CpsUsageAnalysis.getValueUsage (#rec_usage ctx, name)
                         of
                           {call = _, project = NEVER, other = NEVER, ...} =>
                             let
                               val t =
                                 List.map (tryUnpackParam (ctx, #usage ctx))
                                   params
                             in
                               if
                                 List.exists
                                   (fn ELIMINATE => true
                                     | UNPACK _ => true
                                     | KEEP => false) t
                               then SOME t
                               else NONE
                             end
                         | _ => NONE)
                    | _ => NONE
                in
                  case shouldTransformParams of
                    SOME paramTransforms =>
                      let
                        val () = #simplificationOccurred (#base ctx) := true
                        val workerParams =
                          ListPair.foldrEq
                            (fn (p, KEEP, acc) => p :: acc
                              | (_, ELIMINATE, acc) => acc
                              | (_, UNPACK fields, acc) =>
                               List.map (fn (v, _, ty) => (v, ty)) fields @ acc)
                            [] (params, paramTransforms)
                        val workerDecs =
                          ListPair.foldrEq
                            (fn ((p, _), UNPACK fields, decs) =>
                               let
                                 val expMap =
                                   List.foldl
                                     (fn ((fieldVar, label, _), map) =>
                                        Syntax.LabelMap.insert
                                          (map, label, C.Var fieldVar))
                                     Syntax.LabelMap.empty fields
                                 val tyMap =
                                   List.foldl
                                     (fn ((_, label, ty), map) =>
                                        Syntax.LabelMap.insert (map, label, ty))
                                     Syntax.LabelMap.empty fields
                               in
                                 C.ValDec
                                   { exp = C.Record expMap
                                   , results =
                                       [(SOME p, FSyntax.RecordType tyMap)]
                                   } :: decs
                               end
                              | (_, KEEP, decs) => decs
                              | (_, ELIMINATE, decs) => decs) []
                            (params, paramTransforms)
                        val workerBody =
                          case workerDecs of
                            [] => body
                          | _ => C.Let {decs = workerDecs, cont = body}
                        val workerName = CpsSimplify.renewVId (#base ctx, name)
                        val wrapper =
                          let
                            val k = CpsSimplify.genContSym (#base ctx)
                            val wrapperTyParams =
                              List.map
                                (fn (tv, kind) =>
                                   ( CpsSimplify.renewTyVar (#base ctx, tv)
                                   , kind
                                   )) tyParams
                            val wrapperParams =
                              List.map
                                (fn (p, ty) =>
                                   (CpsSimplify.renewVId (#base ctx, p), ty))
                                params
                            val (decs, args) =
                              ListPair.foldrEq
                                (fn ((p, _), KEEP, (decs, args)) =>
                                   (decs, C.Var p :: args)
                                  | (_, ELIMINATE, acc) => acc
                                  | ((p, _), UNPACK fields, (decs, args)) =>
                                   List.foldr
                                     (fn ((v, label, ty), (decs, args)) =>
                                        let
                                          val v =
                                            CpsSimplify.renewVId (#base ctx, v)
                                          val dec = C.ValDec
                                            { exp =
                                                C.Projection
                                                  { label = label
                                                  , record = C.Var p
                                                  , fieldTypes =
                                                      Syntax.LabelMap.empty (* dummy *)
                                                  }
                                            , results = [(SOME v, ty)]
                                            }
                                        in
                                          (dec :: decs, C.Var v :: args)
                                        end) (decs, args) fields) ([], [])
                                (wrapperParams, paramTransforms)
                          in
                            { contParam = k
                            , tyParams = wrapperTyParams
                            , params = wrapperParams
                            , body = C.Let
                                { decs = decs
                                , cont = C.App
                                    { applied = C.Var workerName
                                    , cont = k
                                    , tyArgs =
                                        List.map
                                          (fn (tv, _) => FSyntax.TyVar tv)
                                          wrapperTyParams
                                    , args = args
                                    , attr = {typeOnly = typeOnly}
                                    }
                                }
                            , resultTy = resultTy
                            , attr = {alwaysInline = true, typeOnly = typeOnly}
                            }
                          end
                        val wrappers =
                          TypedSyntax.VIdMap.insert (wrappers, name, wrapper)
                      in
                        ( wrappers
                        , { name = workerName
                          , contParam = contParam
                          , tyParams = tyParams
                          , params = workerParams
                          , body = workerBody
                          , resultTy = resultTy
                          , attr = attr
                          } :: acc
                        )
                      end
                  | NONE =>
                      ( wrappers
                      , { name = name
                        , contParam = contParam
                        , tyParams = tyParams
                        , params = params
                        , body = body
                        , resultTy = resultTy
                        , attr = attr
                        } :: acc
                      )
                end
              val (wrappers, defs) =
                List.foldr transform (TypedSyntax.VIdMap.empty, []) defs
              val defs =
                List.map
                  (fn {name, contParam, tyParams, params, body, resultTy, attr} =>
                     let
                       val body =
                         if TypedSyntax.VIdMap.isEmpty wrappers then
                           body
                         else
                           C.recurseStat
                             (fn e as
                                  C.App
                                    { applied = C.Var applied
                                    , cont
                                    , tyArgs
                                    , args
                                    , attr = _
                                    } =>
                                (case
                                   TypedSyntax.VIdMap.find (wrappers, applied)
                                 of
                                   NONE => e
                                 | SOME
                                     { contParam
                                     , tyParams
                                     , params
                                     , body
                                     , resultTy = _
                                     , attr = _
                                     } =>
                                     let
                                       val tysubst =
                                         ListPair.foldlEq
                                           (fn ((tv, _), a, tysubst) =>
                                              TypedSyntax.TyVarMap.insert
                                                (tysubst, tv, a))
                                           TypedSyntax.TyVarMap.empty
                                           (tyParams, tyArgs)
                                         handle ListPair.UnequalLengths =>
                                           raise Fail
                                             ("CpsUnpackRecordParameter: "
                                              ^ TypedSyntax.print_VId name
                                              ^ ": tyParams ("
                                              ^
                                              String.concatWith ", "
                                                (List.map
                                                   (fn (tv, _) =>
                                                      Printer.build
                                                        (FPrinter.doTyVar tv))
                                                   tyParams) ^ ") vs tyArgs ("
                                              ^
                                              String.concatWith ", "
                                                (List.map
                                                   (Printer.build
                                                    o FPrinter.doTy 0) tyArgs)
                                              ^ ")")
                                       val subst =
                                         ListPair.foldlEq
                                           (fn ((p, _), a, subst) =>
                                              TypedSyntax.VIdMap.insert
                                                (subst, p, a))
                                           TypedSyntax.VIdMap.empty
                                           (params, args)
                                         handle ListPair.UnequalLengths =>
                                           raise Fail
                                             ("CpsUnpackRecordParameter: "
                                              ^ TypedSyntax.print_VId name
                                              ^ ": params ("
                                              ^
                                              String.concatWith ", "
                                                (List.map
                                                   (fn (v, _) =>
                                                      TypedSyntax.print_VId v)
                                                   params) ^ ") vs args ("
                                              ^
                                              String.concatWith ", "
                                                (List.map CSyntax.valueToString
                                                   args) ^ ")")
                                       val csubst =
                                         C.CVarMap.singleton (contParam, cont)
                                     in
                                       CpsSimplify.alphaConvert
                                         ( #base ctx
                                         , tysubst
                                         , subst
                                         , csubst
                                         , body
                                         )
                                     end)
                               | e => e) body
                       val body = simplifyStat (ctx, body)
                     in
                       { name = name
                       , contParam = contParam
                       , tyParams = tyParams
                       , params = params
                       , body = body
                       , resultTy = resultTy
                       , attr = attr
                       }
                     end) defs
            in
              TypedSyntax.VIdMap.foldli
                (fn ( name
                    , {contParam, tyParams, params, body, resultTy, attr}
                    , acc
                    ) =>
                   let
                     val wrapperTy = FSyntax.MultiFnType
                       ( List.map #2 params
                       , resultTy
                       ) (* TODO: type parameters *)
                   in
                     C.ValDec
                       { exp = C.Abs
                           { contParam = contParam
                           , tyParams = tyParams
                           , params = params
                           , body = body
                           , resultTy = resultTy
                           , attr = attr
                           }
                       , results = [(SOME name, wrapperTy)]
                       } :: acc
                   end) (C.RecDec defs :: acc) wrappers
            end
        | C.UnpackDec _ => dec :: acc
        | C.ContDec {name, params, body, attr} =>
            let
              val shouldTransformParams =
                if
                  #indirect
                    (CpsUsageAnalysis.getContUsage (#cont_usage ctx, name))
                  = NEVER
                then
                  let
                    val t =
                      List.map (tryUnpackContParam (ctx, #usage ctx)) params
                  in
                    if
                      List.exists
                        (fn ELIMINATE => true | UNPACK _ => true | KEEP => false)
                        t
                    then SOME t
                    else NONE
                  end
                else
                  NONE
            in
              case shouldTransformParams of
                SOME paramTransforms =>
                  let
                    val () = #simplificationOccurred (#base ctx) := true
                    val params' =
                      ListPair.foldrEq
                        (fn ((SOME p, ty), KEEP, acc) => (p, ty) :: acc
                          | ((SOME _, _), ELIMINATE, acc) => acc
                          | ((SOME _, _), UNPACK fields, acc) =>
                           List.map (fn (v, _, ty) => (v, ty)) fields @ acc
                          | ((NONE, _), _, acc) => acc) []
                        (params, paramTransforms)
                    val decs =
                      ListPair.foldrEq
                        (fn ((p, _), UNPACK fields, decs) =>
                           let
                             val expMap =
                               List.foldl
                                 (fn ((fieldVar, label, _), map) =>
                                    Syntax.LabelMap.insert
                                      (map, label, C.Var fieldVar))
                                 Syntax.LabelMap.empty fields
                             val tyMap =
                               List.foldl
                                 (fn ((_, label, ty), map) =>
                                    Syntax.LabelMap.insert (map, label, ty))
                                 Syntax.LabelMap.empty fields
                           in
                             C.ValDec
                               { exp = C.Record expMap
                               , results = [(p, FSyntax.RecordType tyMap)]
                               } :: decs
                           end
                          | (_, KEEP, decs) => decs
                          | (_, ELIMINATE, decs) => decs) []
                        (params, paramTransforms)
                    val body =
                      case decs of
                        [] => body
                      | _ => C.Let {decs = decs, cont = body}
                    val body = simplifyStat (ctx, body)
                    val name' = CpsSimplify.renewCVar (#base ctx, name)
                    val wrapper =
                      let
                        val params' =
                          List.map
                            (fn (SOME p, ty) =>
                               (SOME (CpsSimplify.renewVId (#base ctx, p)), ty)
                              | a as (NONE, _) => a) params
                        val (decs, args) =
                          ListPair.foldrEq
                            (fn ((SOME p, _), KEEP, (decs, args)) =>
                               (decs, C.Var p :: args)
                              | ((SOME _, _), ELIMINATE, acc) => acc
                              | ((SOME p, _), UNPACK fields, (decs, args)) =>
                               List.foldr
                                 (fn ((v, label, ty), (decs, args)) =>
                                    let
                                      val v =
                                        CpsSimplify.renewVId (#base ctx, v)
                                      val dec = C.ValDec
                                        { exp =
                                            C.Projection
                                              { label = label
                                              , record = C.Var p
                                              , fieldTypes =
                                                  Syntax.LabelMap.empty (* dummy *)
                                              }
                                        , results = [(SOME v, ty)]
                                        }
                                    in
                                      (dec :: decs, C.Var v :: args)
                                    end) (decs, args) fields
                              | ((NONE, _), _, acc) => acc) ([], [])
                            (params', paramTransforms)
                      in
                        C.ContDec
                          { name = name
                          , params = params'
                          , body = C.Let
                              { decs = decs
                              , cont = C.AppCont {applied = name', args = args}
                              }
                          , attr = {alwaysInline = true}
                          }
                      end
                    val dec = C.ContDec
                      { name = name'
                      , params = List.map (fn (v, ty) => (SOME v, ty)) params'
                      , body = body
                      , attr = attr
                      }
                  in
                    wrapper :: dec :: acc
                  end
              | NONE =>
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
            end
        | C.RecContDec defs =>
            let
              fun transform (name, params, body) =
                let
                  val shouldTransformParams =
                    if
                      #indirect
                        (CpsUsageAnalysis.getContUsage (#cont_usage ctx, name))
                      = NEVER
                      andalso
                      #indirect
                        (CpsUsageAnalysis.getContUsage
                           (#cont_rec_usage ctx, name)) = NEVER
                    then
                      let
                        val t =
                          List.map (tryUnpackContParam (ctx, #usage ctx)) params
                      in
                        if
                          List.exists
                            (fn ELIMINATE => true
                              | UNPACK _ => true
                              | KEEP => false) t
                        then SOME t
                        else NONE
                      end
                    else
                      NONE
                in
                  case shouldTransformParams of
                    SOME paramTransforms =>
                      let
                        val () = #simplificationOccurred (#base ctx) := true
                        val params' =
                          ListPair.foldrEq
                            (fn ((SOME p, ty), KEEP, acc) => (p, ty) :: acc
                              | ((SOME _, _), ELIMINATE, acc) => acc
                              | ((SOME _, _), UNPACK fields, acc) =>
                               List.map (fn (v, _, ty) => (v, ty)) fields @ acc
                              | ((NONE, _), _, acc) => acc) []
                            (params, paramTransforms)
                        val decs =
                          ListPair.foldrEq
                            (fn ((p, _), UNPACK fields, decs) =>
                               let
                                 val expMap =
                                   List.foldl
                                     (fn ((fieldVar, label, _), map) =>
                                        Syntax.LabelMap.insert
                                          (map, label, C.Var fieldVar))
                                     Syntax.LabelMap.empty fields
                                 val tyMap =
                                   List.foldl
                                     (fn ((_, label, ty), map) =>
                                        Syntax.LabelMap.insert (map, label, ty))
                                     Syntax.LabelMap.empty fields
                               in
                                 C.ValDec
                                   { exp = C.Record expMap
                                   , results = [(p, FSyntax.RecordType tyMap)]
                                   } :: decs
                               end
                              | (_, KEEP, decs) => decs
                              | (_, ELIMINATE, decs) => decs) []
                            (params, paramTransforms)
                        val body =
                          case decs of
                            [] => body
                          | _ => C.Let {decs = decs, cont = body}
                        val name' = CpsSimplify.renewCVar (#base ctx, name)
                        val wrapper =
                          let
                            val params' =
                              List.map
                                (fn (SOME p, ty) =>
                                   ( SOME (CpsSimplify.renewVId (#base ctx, p))
                                   , ty
                                   )
                                  | a as (NONE, _) => a) params
                            val (decs, args) =
                              ListPair.foldrEq
                                (fn ((SOME p, _), KEEP, (decs, args)) =>
                                   (decs, C.Var p :: args)
                                  | ((SOME _, _), ELIMINATE, acc) => acc
                                  | ((SOME p, _), UNPACK fields, (decs, args)) =>
                                   List.foldr
                                     (fn ((v, label, ty), (decs, args)) =>
                                        let
                                          val v =
                                            CpsSimplify.renewVId (#base ctx, v)
                                          val dec = C.ValDec
                                            { exp =
                                                C.Projection
                                                  { label = label
                                                  , record = C.Var p
                                                  , fieldTypes =
                                                      Syntax.LabelMap.empty (* dummy *)
                                                  }
                                            , results = [(SOME v, ty)]
                                            }
                                        in
                                          (dec :: decs, C.Var v :: args)
                                        end) (decs, args) fields
                                  | ((NONE, _), _, acc) => acc) ([], [])
                                (params', paramTransforms)
                          in
                            ( params'
                            , SOME (C.Let
                                { decs = decs
                                , cont =
                                    C.AppCont {applied = name', args = args}
                                })
                            )
                          end
                      in
                        { origName = name
                        , body = body
                        , newName = name'
                        , newParams =
                            List.map (fn (p, ty) => (SOME p, ty)) params'
                        , inline = wrapper
                        }
                      end
                  | NONE =>
                      { origName = name
                      , body = body
                      , newName = name
                      , newParams = params
                      , inline = (params, NONE)
                      }
                end
              val defs' = List.map transform defs
              val dec = C.RecContDec
                (List.map
                   (fn {newName, newParams, body, ...} =>
                      let
                        val body =
                          C.recurseStat
                            (fn e as C.AppCont {applied, args} =>
                               (case
                                  List.find
                                    (fn {origName, ...} => origName = applied)
                                    defs'
                                of
                                  SOME
                                    {inline = (params, SOME wrapperBody), ...} =>
                                    let
                                      val subst =
                                        ListPair.foldlEq
                                          (fn ((SOME p, _), a, subst) =>
                                             TypedSyntax.VIdMap.insert
                                               (subst, p, a)
                                            | ((NONE, _), _, subst) => subst)
                                          TypedSyntax.VIdMap.empty
                                          (params, args)
                                    in
                                      CpsSimplify.alphaConvert
                                        ( #base ctx
                                        , TypedSyntax.TyVarMap.empty
                                        , subst
                                        , C.CVarMap.empty
                                        , wrapperBody
                                        )
                                    end
                                | _ => e)
                              | e => e) body
                      in
                        (newName, newParams, simplifyStat (ctx, body))
                      end) defs')
            in
              List.foldl
                (fn ({origName, inline = (params, SOME wrapperBody), ...}, acc) =>
                   C.ContDec
                     { name = origName
                     , params = params
                     , body = wrapperBody
                     , attr = {alwaysInline = true}
                     } :: acc
                  | (_, acc) => acc) (dec :: acc) defs'
            end
        | C.DatatypeDec _ => dec :: acc
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
        | C.Handle
            { body
            , handler = (e, h)
            , successfulExitIn
            , successfulExitOut
            , resultTy
            } =>
            C.Handle
              { body = simplifyStat (ctx, body)
              , handler = (e, simplifyStat (ctx, h))
              , successfulExitIn = successfulExitIn
              , successfulExitOut = successfulExitOut
              , resultTy = resultTy
              }
        | C.Raise _ => e
        | C.Unreachable => e
      fun goStat (ctx: CpsSimplify.Context, exp) =
        let
          val usage = CpsUsageAnalysis.analyze exp
          val ctx' =
            { base = ctx
            , usage = #usage usage
            , rec_usage = #rec_usage usage
            , cont_usage = #cont_usage usage
            , cont_rec_usage = #cont_rec_usage usage
            }
        in
          simplifyStat (ctx', exp)
        end
    end (* local *)
  end (* structure CpsUnpackRecordParameter *)
end; (* local *)
