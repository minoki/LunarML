(*
 * Copyright (c) 2024 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure NSyntax :>
sig
  type Var = CSyntax.Var
  type CVar = CSyntax.CVar
  datatype Exp =
    Value of CSyntax.Value
  | PrimOp of {primOp: FSyntax.PrimOp, tyargs: FSyntax.Ty list, args: Exp list}
  | Record of Exp Syntax.LabelMap.map
  | ExnTag of {name: string, payloadTy: FSyntax.Ty option}
  | Projection of
      { label: Syntax.Label
      , record: Exp
      , fieldTypes: FSyntax.Ty Syntax.LabelMap.map
      }
  | Abs of
      { contParam: CVar
      , params: Var list
      , body: Stat
      , attr: CSyntax.AbsAttr
      } (* non-recursive function *)
  | LogicalAnd of Exp * Exp
  | LogicalOr of Exp * Exp
  (* TODO: direct-style function application? *)
  and Dec =
    ValDec of {exp: Exp, results: (Var option) list}
  | RecDec of
      { name: Var
      , contParam: CVar
      , params: Var list
      , body: Stat
      , attr: CSyntax.AbsAttr
      } list (* recursive function *)
  | ContDec of {name: CVar, params: (Var option) list, body: Stat}
  | RecContDec of (CVar * (Var option) list * Stat) list
  | ESImportDec of
      {pure: bool, specs: (Syntax.ESImportName * Var) list, moduleName: string}
  and Stat =
    Let of {decs: Dec list, cont: Stat}
  | App of {applied: Exp, cont: CVar, args: Exp list, attr: CSyntax.AppAttr}
  | AppCont of {applied: CVar, args: Exp list}
  | If of {cond: Exp, thenCont: Stat, elseCont: Stat}
  | Handle of
      { body: Stat
      , handler: Var * Stat
      , successfulExitIn: CVar
      , successfulExitOut: CVar
      }
  | Raise of SourcePos.span * Exp
  | Unreachable
  val containsApp: Stat -> bool
  val fromCExp: CSyntax.CExp -> Stat
  val toNested: Backend.backend * Stat -> Stat
end =
struct
  type Var = CSyntax.Var
  type CVar = CSyntax.CVar
  datatype Exp =
    Value of CSyntax.Value
  | PrimOp of {primOp: FSyntax.PrimOp, tyargs: FSyntax.Ty list, args: Exp list}
  | Record of Exp Syntax.LabelMap.map
  | ExnTag of {name: string, payloadTy: FSyntax.Ty option}
  | Projection of
      { label: Syntax.Label
      , record: Exp
      , fieldTypes: FSyntax.Ty Syntax.LabelMap.map
      }
  | Abs of
      { contParam: CVar
      , params: Var list
      , body: Stat
      , attr: CSyntax.AbsAttr
      } (* non-recursive function *)
  | LogicalAnd of Exp * Exp
  | LogicalOr of Exp * Exp
  and Dec =
    ValDec of {exp: Exp, results: (Var option) list}
  | RecDec of
      { name: Var
      , contParam: CVar
      , params: Var list
      , body: Stat
      , attr: CSyntax.AbsAttr
      } list (* recursive function *)
  | ContDec of {name: CVar, params: (Var option) list, body: Stat}
  | RecContDec of (CVar * (Var option) list * Stat) list
  | ESImportDec of
      {pure: bool, specs: (Syntax.ESImportName * Var) list, moduleName: string}
  and Stat =
    Let of {decs: Dec list, cont: Stat}
  | App of {applied: Exp, cont: CVar, args: Exp list, attr: CSyntax.AppAttr}
  | AppCont of {applied: CVar, args: Exp list}
  | If of {cond: Exp, thenCont: Stat, elseCont: Stat}
  | Handle of
      { body: Stat
      , handler: Var * Stat
      , successfulExitIn: CVar
      , successfulExitOut: CVar
      }
  | Raise of SourcePos.span * Exp
  | Unreachable

  fun containsAppDec (ValDec _) = false
    | containsAppDec (RecDec _) = false
    | containsAppDec (ContDec {name = _, params = _, body}) = containsApp body
    | containsAppDec (RecContDec defs) =
        List.exists (fn (_, _, body) => containsApp body) defs
    | containsAppDec (ESImportDec _) = false
  and containsApp (Let {decs, cont}) =
        containsApp cont orelse List.exists containsAppDec decs
    | containsApp (App _) = true
    | containsApp (AppCont _) = false
    | containsApp (If {cond = _, thenCont, elseCont}) =
        containsApp thenCont orelse containsApp elseCont
    | containsApp (Handle {body, handler = (_, h), ...}) =
        containsApp body orelse containsApp h
    | containsApp (Raise _) = false
    | containsApp Unreachable = false

  local
    structure C = CSyntax
    fun goSimpleExp (C.PrimOp {primOp, tyargs, args}) =
          PrimOp {primOp = primOp, tyargs = tyargs, args = List.map Value args}
      | goSimpleExp (C.Record fields) =
          Record (Syntax.LabelMap.map Value fields)
      | goSimpleExp (C.ExnTag x) = ExnTag x
      | goSimpleExp (C.Projection {label, record, fieldTypes}) =
          Projection
            {label = label, record = Value record, fieldTypes = fieldTypes}
      | goSimpleExp (C.Abs {contParam, params, body, attr}) =
          Abs
            { contParam = contParam
            , params = params
            , body = goCExp body
            , attr = attr
            }
    and goDec (C.ValDec {exp, results}) =
          ValDec {exp = goSimpleExp exp, results = results}
      | goDec (C.RecDec decs) =
          RecDec
            (List.map
               (fn {name, contParam, params, body, attr} =>
                  { name = name
                  , contParam = contParam
                  , params = params
                  , body = goCExp body
                  , attr = attr
                  }) decs)
      | goDec (C.ContDec {name, params, body, attr = _}) =
          ContDec {name = name, params = params, body = goCExp body}
      | goDec (C.RecContDec decs) =
          RecContDec
            (List.map (fn (name, params, body) => (name, params, goCExp body))
               decs)
      | goDec (C.ESImportDec dec) = ESImportDec dec
    and goCExp (C.Let {decs, cont}) =
          Let {decs = List.map goDec decs, cont = goCExp cont}
      | goCExp (C.App {applied, cont, args, attr}) =
          App
            { applied = Value applied
            , cont = cont
            , args = List.map Value args
            , attr = attr
            }
      | goCExp (C.AppCont {applied, args}) =
          AppCont {applied = applied, args = List.map Value args}
      | goCExp (C.If {cond, thenCont, elseCont}) =
          If
            { cond = Value cond
            , thenCont = goCExp thenCont
            , elseCont = goCExp elseCont
            }
      | goCExp
          (C.Handle
             {body, handler = (e, h), successfulExitIn, successfulExitOut}) =
          Handle
            { body = goCExp body
            , handler = (e, goCExp h)
            , successfulExitIn = successfulExitIn
            , successfulExitOut = successfulExitOut
            }
      | goCExp (C.Raise (span, x)) =
          Raise (span, Value x)
      | goCExp C.Unreachable = Unreachable
  in val fromCExp = goCExp
  end (* local *)

  structure Analysis :>
  sig
    datatype frequency = NEVER | ONCE | MANY
    type usage_table
    val analyze: Stat -> usage_table
    val get: usage_table * Var -> frequency
  end =
  struct
    structure C = CSyntax
    datatype frequency = NEVER | ONCE | MANY
    type usage_table = (frequency ref) TypedSyntax.VIdTable.hash_table
    fun oneMore NEVER = ONCE
      | oneMore _ = MANY
    fun goStat usage =
      let
        fun use vid =
          (case TypedSyntax.VIdTable.find usage vid of
             SOME r => r := oneMore (!r)
           | NONE => TypedSyntax.VIdTable.insert usage (vid, ref ONCE))
        fun useValue (C.Var v) = use v
          | useValue _ = ()
        fun goExp (Value v) = useValue v
          | goExp (PrimOp {args, ...}) = List.app goExp args
          | goExp (Record fields) = Syntax.LabelMap.app goExp fields
          | goExp (ExnTag _) = ()
          | goExp (Projection {record, ...}) = goExp record
          | goExp (Abs {body, ...}) = goStat body
          | goExp (LogicalAnd (x, y)) =
              (goExp x; goExp y)
          | goExp (LogicalOr (x, y)) =
              (goExp x; goExp y)
        and goDec (ValDec {exp, ...}) = goExp exp
          | goDec (RecDec decs) =
              List.app (fn {body, ...} => goStat body) decs
          | goDec (ContDec {body, ...}) = goStat body
          | goDec (RecContDec decs) =
              List.app (fn (_, _, body) => goStat body) decs
          | goDec (ESImportDec _) = ()
        and goStat (Let {decs, cont}) =
              (List.app goDec decs; goStat cont)
          | goStat (App {applied, args, ...}) =
              (goExp applied; List.app goExp args)
          | goStat (AppCont {applied = _, args}) = List.app goExp args
          | goStat (If {cond, thenCont, elseCont}) =
              (goExp cond; goStat thenCont; goStat elseCont)
          | goStat (Handle {body, handler = (_, h), ...}) =
              (goStat body; goStat h)
          | goStat (Raise (_, x)) = goExp x
          | goStat Unreachable = ()
      in
        goStat
      end
    fun analyze program =
      let
        val usage =
          TypedSyntax.VIdTable.mkTable (1, Fail "usage table lookup failed")
      in
        goStat usage program;
        usage
      end
    fun get (table, vid) =
      case TypedSyntax.VIdTable.find table vid of
        SOME r => !r
      | NONE => NEVER
  end (* structure Analysis *)

  local
    structure C = CSyntax
    (*: val replaceList : ('a -> 'a option) -> 'a list -> ('a list) option *)
    fun replaceList tryReplace =
      let
        fun go ([], _) = NONE
          | go (x :: xs, revAcc) =
              case tryReplace x of
                NONE => go (xs, x :: revAcc)
              | SOME y => SOME (List.revAppend (revAcc, y :: xs))
      in
        fn xs => go (xs, [])
      end
    fun replaceOnce (var, replacement) =
      let
        fun goValue (C.Var v) =
              if v = var then SOME replacement else NONE
          | goValue _ = NONE
        fun goExp (Value v) = goValue v
          | goExp (PrimOp {primOp, tyargs, args}) =
              Option.map
                (fn args =>
                   PrimOp {primOp = primOp, tyargs = tyargs, args = args})
                (replaceList goExp args)
          | goExp (Record fields) =
              let
                fun go (label, exp, (true, acc)) =
                      (true, Syntax.LabelMap.insert (acc, label, exp))
                  | go (label, exp, (false, acc)) =
                      case goExp exp of
                        NONE =>
                          (false, Syntax.LabelMap.insert (acc, label, exp))
                      | SOME y => (true, Syntax.LabelMap.insert (acc, label, y))
              in
                case
                  Syntax.LabelMap.foldli go (false, Syntax.LabelMap.empty)
                    fields
                of
                  (true, fields') => SOME (Record fields')
                | (false, _) => NONE
              end
          | goExp (ExnTag _) = NONE
          | goExp (Projection {label, record, fieldTypes}) =
              Option.map
                (fn record =>
                   Projection
                     {label = label, record = record, fieldTypes = fieldTypes})
                (goExp record)
          | goExp (Abs _) = NONE
          | goExp (LogicalAnd (x, y)) =
              Option.map (fn x => LogicalAnd (x, y)) (goExp x)
          | goExp (LogicalOr (x, y)) =
              Option.map (fn x => LogicalOr (x, y)) (goExp x)
        fun goDecs [] = NONE
          | goDecs (ValDec {exp, results} :: decs) =
              Option.map
                (fn exp => ValDec {exp = exp, results = results} :: decs)
                (goExp exp)
          | goDecs ((dec as RecDec _) :: decs) =
              Option.map (fn decs => dec :: decs) (goDecs decs)
          | goDecs ((dec as ContDec _) :: decs) =
              Option.map (fn decs => dec :: decs) (goDecs decs)
          | goDecs ((dec as RecContDec _) :: decs) =
              Option.map (fn decs => dec :: decs) (goDecs decs)
          | goDecs (ESImportDec _ :: _) = NONE
        fun goStat (Let {decs, cont}) =
              Option.map (fn decs => Let {decs = decs, cont = cont})
                (goDecs decs)
          | goStat (App {applied, cont, args, attr}) =
              (case goExp applied of
                 SOME applied =>
                   SOME (App
                     {applied = applied, cont = cont, args = args, attr = attr})
               | NONE =>
                   case replaceList goExp args of
                     SOME args =>
                       SOME (App
                         { applied = applied
                         , cont = cont
                         , args = args
                         , attr = attr
                         })
                   | NONE => NONE)
          | goStat (AppCont {applied, args}) =
              Option.map (fn args => AppCont {applied = applied, args = args})
                (replaceList goExp args)
          | goStat (If {cond, thenCont, elseCont}) =
              Option.map
                (fn cond =>
                   If {cond = cond, thenCont = thenCont, elseCont = elseCont})
                (goExp cond)
          | goStat (Handle _) = NONE
          | goStat (Raise (span, x)) =
              Option.map (fn x => Raise (span, x)) (goExp x)
          | goStat Unreachable = NONE
      in
        {goValue = goValue, goExp = goExp, goDecs = goDecs, goStat = goStat}
      end
    fun canInlineLua _ = true
    fun canInlineJs (ExnTag _) = false
      | canInlineJs _ = true
    val DEPTH_LIMIT = 10
    fun toNestedImpl (backend, usage) =
      let
        val canInline =
          case backend of
            Backend.BACKEND_LUA _ => canInlineLua
          | Backend.BACKEND_LUAJIT => canInlineLua
          | Backend.BACKEND_JS _ => canInlineJs
        fun goExp (e as Value _) = e
          | goExp (PrimOp {primOp, tyargs, args}) =
              PrimOp
                {primOp = primOp, tyargs = tyargs, args = List.map goExp args}
          | goExp (Record fields) =
              Record (Syntax.LabelMap.map goExp fields)
          | goExp (e as ExnTag _) = e
          | goExp (Projection {label, record, fieldTypes}) =
              Projection
                {label = label, record = goExp record, fieldTypes = fieldTypes}
          | goExp (Abs {contParam, params, body, attr}) =
              Abs
                { contParam = contParam
                , params = params
                , body = goStat body
                , attr = attr
                }
          | goExp (LogicalAnd (x, y)) =
              LogicalAnd (goExp x, goExp y)
          | goExp (LogicalOr (x, y)) =
              LogicalOr (goExp x, goExp y)
        and goDecs (_, [], revAcc) = revAcc (* reversed *)
          | goDecs (i, ValDec {exp, results as [SOME v]} :: decs, revAcc) =
              let
                val exp = goExp exp
              in
                if
                  canInline exp andalso Analysis.get (usage, v) = Analysis.ONCE
                  andalso i <= DEPTH_LIMIT
                then
                  case #goDecs (replaceOnce (v, exp)) decs of
                    NONE =>
                      goDecs
                        ( 0
                        , decs
                        , ValDec {exp = exp, results = results} :: revAcc
                        )
                  | SOME decs => goDecs (i + 1, decs, revAcc)
                else
                  goDecs
                    (0, decs, ValDec {exp = exp, results = results} :: revAcc)
              end
          | goDecs (_, ValDec {exp, results} :: decs, revAcc) =
              goDecs
                (0, decs, ValDec {exp = goExp exp, results = results} :: revAcc)
          | goDecs (_, RecDec rdecs :: decs, revAcc) =
              goDecs
                ( 0
                , decs
                , RecDec
                    (List.map
                       (fn {name, contParam, params, body, attr} =>
                          { name = name
                          , contParam = contParam
                          , params = params
                          , body = goStat body
                          , attr = attr
                          }) rdecs) :: revAcc
                )
          | goDecs (_, ContDec {name, params, body} :: decs, revAcc) =
              goDecs
                ( 0
                , decs
                , ContDec {name = name, params = params, body = goStat body}
                  :: revAcc
                )
          | goDecs (_, RecContDec rdecs :: decs, revAcc) =
              goDecs
                ( 0
                , decs
                , RecContDec
                    (List.map
                       (fn (name, params, body) => (name, params, goStat body))
                       rdecs) :: revAcc
                )
          | goDecs (_, (dec as ESImportDec _) :: decs, revAcc) =
              goDecs (0, decs, dec :: revAcc)
        and goStat (Let {decs, cont}) =
              let
                fun searchFirstValDec
                      (acc, ValDec {exp, results = [SOME v]} :: revDecs) =
                      SOME (List.revAppend (acc, revDecs), exp, v)
                  | searchFirstValDec (_, ValDec _ :: _) = NONE
                  | searchFirstValDec (acc, (dec as RecDec _) :: revDecs) =
                      searchFirstValDec (dec :: acc, revDecs)
                  | searchFirstValDec (acc, (dec as ContDec _) :: revDecs) =
                      searchFirstValDec (dec :: acc, revDecs)
                  | searchFirstValDec (acc, (dec as RecContDec _) :: revDecs) =
                      searchFirstValDec (dec :: acc, revDecs)
                  | searchFirstValDec (_, ESImportDec _ :: _) = NONE
                  | searchFirstValDec (_, []) = NONE
                val revDecs0 = goDecs (0, decs, [])
              in
                case searchFirstValDec ([], revDecs0) of
                  SOME (revDecs1, exp, v) =>
                    if
                      canInline exp
                      andalso Analysis.get (usage, v) = Analysis.ONCE
                    then
                      case #goStat (replaceOnce (v, exp)) cont of
                        SOME cont =>
                          let
                            val cont = goStat cont
                          in
                            if List.null revDecs1 then
                              cont
                            else
                              case (revDecs1, cont) of
                                ( ContDec {name, params as [SOME p], body} ::
                                    revDecs2
                                , AppCont {applied, args = [a]}
                                ) =>
                                  if name = applied then
                                    if
                                      canInline a
                                      andalso
                                      Analysis.get (usage, p) = Analysis.ONCE
                                    then
                                      case #goStat (replaceOnce (p, a)) body of
                                        SOME body =>
                                          if List.null revDecs2 then
                                            body
                                          else
                                            Let
                                              { decs = List.rev revDecs2
                                              , cont = body
                                              }
                                      | NONE =>
                                          Let
                                            { decs = List.rev
                                                (ValDec
                                                   {exp = a, results = params}
                                                 :: revDecs2)
                                            , cont = body
                                            }
                                    else
                                      Let
                                        { decs = List.rev
                                            (ValDec {exp = a, results = params}
                                             :: revDecs2)
                                        , cont = body
                                        }
                                  else
                                    Let {decs = List.rev revDecs1, cont = cont}
                              | _ => Let {decs = List.rev revDecs1, cont = cont}
                          end
                      | NONE =>
                          Let {decs = List.rev revDecs0, cont = goStat cont}
                    else
                      Let {decs = List.rev revDecs0, cont = goStat cont}
                | NONE => Let {decs = List.rev revDecs0, cont = goStat cont}
              end
          | goStat (App {applied, cont, args, attr}) =
              App
                { applied = goExp applied
                , cont = cont
                , args = List.map goExp args
                , attr = attr
                }
          | goStat (AppCont {applied, args}) =
              AppCont {applied = applied, args = List.map goExp args}
          | goStat (If {cond, thenCont, elseCont}) =
              let
                val cond = goExp cond
                val thenCont = goStat thenCont
                val elseCont = goStat elseCont
                fun Not
                      (PrimOp
                         { primOp = FSyntax.PrimCall Primitives.Bool_not
                         , tyargs = _
                         , args = [x]
                         }) = x
                  | Not x =
                      PrimOp
                        { primOp = FSyntax.PrimCall Primitives.Bool_not
                        , tyargs = []
                        , args = [x]
                        }
                val toLogicalAndOr =
                  case (thenCont, elseCont) of
                    ( AppCont {applied, args = [x]}
                    , AppCont {applied = applied', args = [y]}
                    ) =>
                      if applied = applied' then
                        case (x, y) of
                          (Value (C.BoolConst true), Value (C.BoolConst false)) =>
                            SOME (applied, cond)
                        | (Value (C.BoolConst false), Value (C.BoolConst true)) =>
                            SOME (applied, Not cond)
                        | (_, Value (C.BoolConst true)) =>
                            SOME (applied, LogicalOr (Not cond, x))
                        | (_, Value (C.BoolConst false)) =>
                            SOME (applied, LogicalAnd (cond, x))
                        | (Value (C.BoolConst true), _) =>
                            SOME (applied, LogicalOr (cond, y))
                        | (Value (C.BoolConst false), _) =>
                            SOME (applied, LogicalAnd (Not cond, y))
                        | _ => NONE
                      else
                        NONE
                  | _ => NONE
              in
                case toLogicalAndOr of
                  SOME (applied, exp) =>
                    AppCont {applied = applied, args = [exp]}
                | NONE =>
                    If {cond = cond, thenCont = thenCont, elseCont = elseCont}
              end
          | goStat
              (Handle
                 {body, handler = (e, h), successfulExitIn, successfulExitOut}) =
              Handle
                { body = goStat body
                , handler = (e, goStat h)
                , successfulExitIn = successfulExitIn
                , successfulExitOut = successfulExitOut
                }
          | goStat (Raise (span, x)) =
              Raise (span, goExp x)
          | goStat Unreachable = Unreachable
      in
        goStat
      end
  in
    fun toNested (backend, program) =
      let val usage = Analysis.analyze program
      in toNestedImpl (backend, usage) program
      end
  end (* local *)
end; (* structure NSyntax *)
