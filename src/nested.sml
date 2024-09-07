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
  | Unreachable
  val containsApp: Stat -> bool
  val fromCExp: CSyntax.CExp -> Stat
  val toNested: Stat -> Stat
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
      | goDec (C.ContDec {name, params, body}) =
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
        fun goDec (ValDec {exp, results}) =
              Option.map (fn exp => ValDec {exp = exp, results = results})
                (goExp exp)
          | goDec (RecDec _) = NONE
          | goDec (ContDec _) = NONE
          | goDec (RecContDec _) = NONE
          | goDec (ESImportDec _) = NONE
        fun goStat (Let {decs = dec :: decs, cont}) =
              Option.map (fn dec => Let {decs = dec :: decs, cont = cont})
                (goDec dec)
          | goStat (Let {decs = [], cont}) = goStat cont
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
          | goStat Unreachable = NONE
      in
        {goValue = goValue, goExp = goExp, goDec = goDec, goStat = goStat}
      end
    fun canInline (PrimOp {primOp = FSyntax.RaiseOp _, ...}) = false
      | canInline _ = true
    val DEPTH_LIMIT = 10
    fun toNestedImpl usage =
      let
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
        and goDecs (_, [], revAcc) = revAcc (* reversed *)
          | goDecs
              ( i
              , ValDec {exp, results as [SOME v]} :: (decs1 as (dec2 :: decs2))
              , revAcc
              ) =
              let
                val exp = goExp exp
              in
                if
                  canInline exp andalso Analysis.get (usage, v) = Analysis.ONCE
                  andalso i <= DEPTH_LIMIT
                then
                  case #goDec (replaceOnce (v, exp)) dec2 of
                    NONE =>
                      goDecs
                        ( 0
                        , decs1
                        , ValDec {exp = exp, results = results} :: revAcc
                        )
                  | SOME dec2' => goDecs (i + 1, dec2' :: decs2, revAcc)
                else
                  goDecs
                    (0, decs1, ValDec {exp = exp, results = results} :: revAcc)
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
              (case goDecs (0, decs, []) of
                 revDecs0 as (ValDec {exp, results = [SOME v]} :: revDecs1) =>
                   if
                     canInline exp
                     andalso Analysis.get (usage, v) = Analysis.ONCE
                   then
                     case #goStat (replaceOnce (v, exp)) cont of
                       SOME cont =>
                         Let {decs = List.rev revDecs1, cont = goStat cont}
                     | NONE =>
                         Let {decs = List.rev revDecs0, cont = goStat cont}
                   else
                     Let {decs = List.rev revDecs0, cont = goStat cont}
               | revDecs => Let {decs = List.rev revDecs, cont = goStat cont})
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
              If
                { cond = goExp cond
                , thenCont = goStat thenCont
                , elseCont = goStat elseCont
                }
          | goStat
              (Handle
                 {body, handler = (e, h), successfulExitIn, successfulExitOut}) =
              Handle
                { body = goStat body
                , handler = (e, goStat h)
                , successfulExitIn = successfulExitIn
                , successfulExitOut = successfulExitOut
                }
          | goStat Unreachable = Unreachable
      in
        goStat
      end
  in
    fun toNested program =
      let val usage = Analysis.analyze program
      in toNestedImpl usage program
      end
  end (* local *)
end; (* structure NSyntax *)
