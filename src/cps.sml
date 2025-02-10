(*
 * Copyright (c) 2023 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure CSyntax :>
sig
  exception InvalidCode of string
  type Var = TypedSyntax.VId
  structure CVar:
  sig
    eqtype t
    type ord_key = t
    val compare: ord_key * ord_key -> order
    val fromInt: int -> t
    val toInt: t -> int
    val dummy: t
  end
  type CVar = CVar.t
  structure CVarSet: ORD_SET where type Key.ord_key = CVar.t
  structure CVarMap: ORD_MAP where type Key.ord_key = CVar.t
  structure CVarTable: MONO_HASH_TABLE where type Key.hash_key = CVar.t
  datatype Value =
    Var of Var
  | Unit (* : unit *)
  | Nil (* : 'a list *)
  | BoolConst of bool
  | IntConst of Primitives.int_width * IntInf.int
  | WordConst of Primitives.word_width * IntInf.int
  | CharConst of char
  | Char16Const of int
  | StringConst of string
  | String16Const of int vector
  type AbsAttr = {alwaysInline: bool}
  type ContAttr = {alwaysInline: bool}
  type AppAttr = {}
  datatype SimpleExp =
    PrimOp of
      {primOp: FSyntax.PrimOp, tyargs: FSyntax.Ty list, args: Value list}
  | Record of Value Syntax.LabelMap.map (* non-empty record *)
  | ExnTag of {name: string, payloadTy: FSyntax.Ty option}
  | Projection of
      { label: Syntax.Label
      , record: Value
      , fieldTypes: FSyntax.Ty Syntax.LabelMap.map
      }
  | Abs of
      { contParam: CVar
      , params: Var list
      , body: CExp
      , attr: AbsAttr
      } (* non-recursive function *)
  and Dec =
    ValDec of {exp: SimpleExp, results: (Var option) list}
  | RecDec of
      {name: Var, contParam: CVar, params: Var list, body: CExp, attr: AbsAttr} list (* recursive function *)
  | ContDec of
      {name: CVar, params: (Var option) list, body: CExp, attr: ContAttr}
  | RecContDec of (CVar * (Var option) list * CExp) list
  | ESImportDec of
      {pure: bool, specs: (Syntax.ESImportName * Var) list, moduleName: string}
  and CExp =
    Let of {decs: Dec list, cont: CExp}
  | App of
      { applied: Value
      , cont: CVar
      , args: Value list
      , attr: AppAttr
      } (* tail call *) (* return arity? *)
  | AppCont of {applied: CVar, args: Value list}
  | If of {cond: Value, thenCont: CExp, elseCont: CExp}
  | Handle of
      { body: CExp
      , handler: Var * CExp
      , successfulExitIn: CVar
      , successfulExitOut: CVar
      }
  | Raise of SourcePos.span * Value
  | Unreachable
  val isDiscardable: SimpleExp -> bool
  val containsApp: CExp -> bool
  val freeVarsInExp: TypedSyntax.VIdSet.set * CExp * TypedSyntax.VIdSet.set
                     -> TypedSyntax.VIdSet.set
  val recurseCExp: (CExp -> CExp) -> CExp -> CExp
end =
struct
  exception InvalidCode of string
  type Var = TypedSyntax.VId
  structure CVar :>
  sig
    eqtype t
    type ord_key = t
    val compare: ord_key * ord_key -> order
    val fromInt: int -> t
    val toInt: t -> int
    val dummy: t
  end =
  struct
    type t = int
    type ord_key = t
    val compare = Int.compare
    fun fromInt x = x
    fun toInt x = x
    val dummy = ~1
  end
  type CVar = CVar.t
  structure CVarSet = RedBlackSetFn(CVar)
  structure CVarMap = RedBlackMapFn(CVar)
  structure CVarTable =
    HashTableFn
      (struct
         type hash_key = CVar.t
         fun hashVal x =
           Word.fromInt (CVar.toInt x)
         fun sameKey (x: CVar.t, y: CVar.t) = x = y
       end)
  datatype Value =
    Var of Var
  | Unit (* : unit *)
  | Nil (* : 'a list *)
  | BoolConst of bool
  | IntConst of Primitives.int_width * IntInf.int
  | WordConst of Primitives.word_width * IntInf.int
  | CharConst of char
  | Char16Const of int
  | StringConst of string
  | String16Const of int vector
  (*
                 | RealConst of Numeric.float_notation
  *)
  type AbsAttr = {alwaysInline: bool}
  type ContAttr = {alwaysInline: bool}
  type AppAttr = {}
  datatype SimpleExp =
    PrimOp of
      {primOp: FSyntax.PrimOp, tyargs: FSyntax.Ty list, args: Value list}
  | Record of Value Syntax.LabelMap.map (* non-empty record *)
  | ExnTag of {name: string, payloadTy: FSyntax.Ty option}
  | Projection of
      { label: Syntax.Label
      , record: Value
      , fieldTypes: FSyntax.Ty Syntax.LabelMap.map
      }
  | Abs of
      { contParam: CVar
      , params: Var list
      , body: CExp
      , attr: AbsAttr
      } (* non-recursive function *)
  and Dec =
    ValDec of {exp: SimpleExp, results: (Var option) list}
  | RecDec of
      {name: Var, contParam: CVar, params: Var list, body: CExp, attr: AbsAttr} list (* recursive function *)
  | ContDec of
      {name: CVar, params: (Var option) list, body: CExp, attr: ContAttr}
  | RecContDec of (CVar * (Var option) list * CExp) list
  | ESImportDec of
      {pure: bool, specs: (Syntax.ESImportName * Var) list, moduleName: string}
  and CExp =
    Let of {decs: Dec list, cont: CExp}
  | App of
      { applied: Value
      , cont: CVar
      , args: Value list
      , attr: AppAttr
      } (* tail call *) (* return arity? *)
  | AppCont of {applied: CVar, args: Value list}
  | If of {cond: Value, thenCont: CExp, elseCont: CExp}
  | Handle of
      { body: CExp
      , handler: Var * CExp
      , successfulExitIn: CVar
      , successfulExitOut: CVar
      }
  | Raise of SourcePos.span * Value
  | Unreachable
  local structure F = FSyntax
  in
    fun isDiscardable (PrimOp {primOp = F.IntConstOp _, ...}) = true
      | isDiscardable (PrimOp {primOp = F.WordConstOp _, ...}) = true
      | isDiscardable (PrimOp {primOp = F.RealConstOp _, ...}) = true
      | isDiscardable (PrimOp {primOp = F.Char8ConstOp _, ...}) = true
      | isDiscardable (PrimOp {primOp = F.Char16ConstOp _, ...}) = true
      | isDiscardable (PrimOp {primOp = F.String8ConstOp _, ...}) = true
      | isDiscardable (PrimOp {primOp = F.String16ConstOp _, ...}) = true
      | isDiscardable (PrimOp {primOp = F.RaiseOp _, ...}) = false
      | isDiscardable (PrimOp {primOp = F.ListOp, ...}) = true
      | isDiscardable (PrimOp {primOp = F.VectorOp, ...}) = true
      | isDiscardable (PrimOp {primOp = F.DataTagAsStringOp _, ...}) = true
      | isDiscardable (PrimOp {primOp = F.DataTagAsString16Op _, ...}) = true
      | isDiscardable (PrimOp {primOp = F.DataPayloadOp _, ...}) = true
      | isDiscardable (PrimOp {primOp = F.ExnPayloadOp, ...}) = true
      | isDiscardable (PrimOp {primOp = F.ConstructValOp _, ...}) = true
      | isDiscardable (PrimOp {primOp = F.ConstructValWithPayloadOp _, ...}) =
          true
      | isDiscardable (PrimOp {primOp = F.ConstructExnOp, ...}) = true
      | isDiscardable (PrimOp {primOp = F.ConstructExnWithPayloadOp, ...}) =
          true
      | isDiscardable (PrimOp {primOp = F.PrimCall p, ...}) =
          Primitives.isDiscardable p
      | isDiscardable (PrimOp {primOp = F.JsCallOp, ...}) = false
      | isDiscardable (PrimOp {primOp = F.JsMethodOp, ...}) = false
      | isDiscardable (PrimOp {primOp = F.JsNewOp, ...}) = false
      | isDiscardable (PrimOp {primOp = F.LuaCallOp, ...}) = false
      | isDiscardable (PrimOp {primOp = F.LuaCall1Op, ...}) = false
      | isDiscardable (PrimOp {primOp = F.LuaCallNOp _, ...}) = false
      | isDiscardable (PrimOp {primOp = F.LuaMethodOp _, ...}) = false
      | isDiscardable (PrimOp {primOp = F.LuaMethod1Op _, ...}) = false
      | isDiscardable (PrimOp {primOp = F.LuaMethodNOp _, ...}) = false
      | isDiscardable (Record _) = true
      | isDiscardable (ExnTag _) = true
      | isDiscardable (Projection _) = true
      | isDiscardable (Abs _) = true
    (* fun mayRaise (PrimOp { primOp, ... }) = (case primOp of
                                                 F.IntConstOp _ => false
                                               | F.WordConstOp _ => false
                                               | F.RealConstOp _ => false
                                               | F.Char8ConstOp _ => false
                                               | F.Char16ConstOp _ => false
                                               | F.String8ConstOp _ => false
                                               | F.String16ConstOp _ => false
                                               | F.RaiseOp _ => true
                                               | F.ListOp => false
                                               | F.VectorOp => false
                                               | F.DataTagAsStringOp _ => false
                                               | F.DataTagAsString16Op _ => false
                                               | F.DataPayloadOp _ => false
                                               | F.ExnPayloadOp => false
                                               | F.ConstructValOp _ => false
                                               | F.ConstructValWithPayloadOp _ => false
                                               | F.ConstructExnOp => false
                                               | F.ConstructExnWithPayloadOp => false
                                               | F.PrimCall p => Primitives.mayRaise p
                                               | F.JsCallOp => true
                                               | F.JsMethodOp => true
                                               | F.JsNewOp => true
                                               | F.LuaCallOp => true
                                               | F.LuaCall1Op => true
                                               | F.LuaMethodOp _ => true
                                               | F.LuaMethod1Op _ => true
                                            )
      | mayRaise (Record _) = false
      | mayRaise (ExnTag _) = false
      | mayRaise (Projection _) = false
      | mayRaise (Abs _) = false *)

    fun containsAppDec (ValDec _) = false
      | containsAppDec (RecDec _) = false
      | containsAppDec (ContDec {name = _, params = _, body, attr = _}) =
          containsApp body
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

    fun freeVarsInValue bound (v, acc) =
      case v of
        Var var =>
          if TypedSyntax.VIdSet.member (bound, var) then acc
          else TypedSyntax.VIdSet.add (acc, var)
      | Unit => acc
      | Nil => acc
      | BoolConst _ => acc
      | IntConst _ => acc
      | WordConst _ => acc
      | CharConst _ => acc
      | Char16Const _ => acc
      | StringConst _ => acc
      | String16Const _ => acc
    fun VIdSet_addOpt (SOME x, set) = TypedSyntax.VIdSet.add (set, x)
      | VIdSet_addOpt (NONE, set) = set
    fun freeVarsInSimpleExp (bound, PrimOp {primOp = _, tyargs = _, args}, acc) =
          List.foldl (freeVarsInValue bound) acc args
      | freeVarsInSimpleExp (bound, Record fields, acc) =
          Syntax.LabelMap.foldl (freeVarsInValue bound) acc fields
      | freeVarsInSimpleExp (_, ExnTag {name = _, payloadTy = _}, acc) = acc
      | freeVarsInSimpleExp
          (bound, Projection {label = _, record, fieldTypes = _}, acc) =
          freeVarsInValue bound (record, acc)
      | freeVarsInSimpleExp
          (bound, Abs {contParam = _, params, body, attr = _}, acc) =
          let val bound = List.foldl TypedSyntax.VIdSet.add' bound params
          in freeVarsInExp (bound, body, acc)
          end
    and freeVarsInDec (ValDec {exp, results}, (bound, acc)) =
          let
            val acc = freeVarsInSimpleExp (bound, exp, acc)
            val bound' =
              List.foldl
                (fn (SOME r, bound) => TypedSyntax.VIdSet.add (bound, r)
                  | (NONE, bound) => bound) bound results
          in
            (bound', acc)
          end
      | freeVarsInDec (RecDec defs, (bound, acc)) =
          let
            val bound =
              List.foldl
                (fn ({name, ...}, bound) => TypedSyntax.VIdSet.add (bound, name))
                bound defs
            val acc =
              List.foldl
                (fn ({params, body, ...}, acc) =>
                   let
                     val bound = List.foldl TypedSyntax.VIdSet.add' bound params
                   in
                     freeVarsInExp (bound, body, acc)
                   end) acc defs
          in
            (bound, acc)
          end
      | freeVarsInDec (ContDec {name = _, params, body, attr = _}, (bound, acc)) =
          let val bound = List.foldl VIdSet_addOpt bound params
          in (bound, freeVarsInExp (bound, body, acc))
          end
      | freeVarsInDec (RecContDec defs, (bound, acc)) =
          let
            val acc =
              List.foldl
                (fn ((_, params, body), acc) =>
                   let val bound = List.foldl VIdSet_addOpt bound params
                   in freeVarsInExp (bound, body, acc)
                   end) acc defs
          in
            (bound, acc)
          end
      | freeVarsInDec
          (ESImportDec {pure = _, specs, moduleName = _}, (bound, acc)) =
          ( List.foldl (fn ((_, v), bound) => TypedSyntax.VIdSet.add (bound, v))
              bound specs
          , acc
          )
    and freeVarsInExp (bound, Let {decs, cont}, acc) =
          let val (bound, acc) = List.foldl freeVarsInDec (bound, acc) decs
          in freeVarsInExp (bound, cont, acc)
          end
      | freeVarsInExp (bound, App {applied, cont = _, args, attr = _}, acc) =
          List.foldl (freeVarsInValue bound)
            (freeVarsInValue bound (applied, acc)) args
      | freeVarsInExp (bound, AppCont {applied = _, args}, acc) =
          List.foldl (freeVarsInValue bound) acc args
      | freeVarsInExp (bound, If {cond, thenCont, elseCont}, acc) =
          freeVarsInExp (bound, elseCont, freeVarsInExp
            (bound, thenCont, freeVarsInValue bound (cond, acc)))
      | freeVarsInExp
          ( bound
          , Handle
              { body
              , handler = (e, h)
              , successfulExitIn = _
              , successfulExitOut = _
              }
          , acc
          ) =
          freeVarsInExp (bound, body, freeVarsInExp
            (TypedSyntax.VIdSet.add (bound, e), h, acc))
      | freeVarsInExp (bound, Raise (_, x), acc) =
          freeVarsInValue bound (x, acc)
      | freeVarsInExp (_, Unreachable, acc) = acc

    fun recurseCExp f =
      let
        fun goSimpleExp (e as PrimOp _) = e
          | goSimpleExp (e as Record _) = e
          | goSimpleExp (e as ExnTag _) = e
          | goSimpleExp (e as Projection _) = e
          | goSimpleExp (Abs {contParam, params, body, attr}) =
              Abs
                { contParam = contParam
                , params = params
                , body = goExp body
                , attr = attr
                }
        and goDec (ValDec {exp, results}) =
              ValDec {exp = goSimpleExp exp, results = results}
          | goDec (RecDec defs) =
              RecDec
                (List.map
                   (fn {name, contParam, params, body, attr} =>
                      { name = name
                      , contParam = contParam
                      , params = params
                      , body = goExp body
                      , attr = attr
                      }) defs)
          | goDec (ContDec {name, params, body, attr}) =
              ContDec
                {name = name, params = params, body = goExp body, attr = attr}
          | goDec (RecContDec defs) =
              RecContDec
                (List.map
                   (fn (name, params, body) => (name, params, goExp body)) defs)
          | goDec (dec as ESImportDec _) = dec
        and goExp e =
          f (case e of
               Let {decs, cont} =>
                 Let {decs = List.map goDec decs, cont = goExp cont}
             | App _ => e
             | AppCont _ => e
             | If {cond, thenCont, elseCont} =>
                 If
                   { cond = cond
                   , thenCont = goExp thenCont
                   , elseCont = goExp elseCont
                   }
             | Handle
                 {body, handler = (k, h), successfulExitIn, successfulExitOut} =>
                 Handle
                   { body = goExp body
                   , handler = (k, goExp h)
                   , successfulExitIn = successfulExitIn
                   , successfulExitOut = successfulExitOut
                   }
             | Raise _ => e
             | Unreachable => e)
      in
        goExp
      end
  end
end

structure CpsTransform :>
sig
  type Context =
    {targetInfo: TargetInfo.target_info, nextVId: int ref, exportAsRecord: bool}
  val initialEnv: CSyntax.Value TypedSyntax.VIdMap.map
  val prependRevDecs: CSyntax.Dec list * CSyntax.CExp -> CSyntax.CExp
  val transformT: Context * CSyntax.Value TypedSyntax.VIdMap.map
                  -> FSyntax.Exp
                  -> CSyntax.Dec list * CSyntax.CVar
                  -> CSyntax.CExp
end =
struct
  local
    structure F = FSyntax
    structure C = CSyntax
    val foldlCont = ListUtil.foldlCont
  in

    type Context =
      { targetInfo: TargetInfo.target_info
      , nextVId: int ref
      , exportAsRecord: bool
      }

    fun genContSym (ctx: Context) : CSyntax.CVar =
      let
        val n = !(#nextVId ctx)
        val _ = #nextVId ctx := n + 1
      in
        CSyntax.CVar.fromInt n
      end

    fun genSym (ctx: Context) =
      let
        val n = !(#nextVId ctx)
        val _ = #nextVId ctx := n + 1
      in
        TypedSyntax.MkVId ("tmp", n)
      end

    fun stripTyAbs (F.TyAbsExp (_, _, e)) = stripTyAbs e
      | stripTyAbs e = e

    (* 'a -> 'b ~~> (cont : 'b -> 'ans, param : 'a) -> 'ans *)
    (* continuation of 'a : (value : 'a) -> 'ans *)

    datatype cont =
      REIFIED of C.CVar
    | META of C.Var option * (C.Dec list * C.Value -> C.CExp)
    fun prependRevDecs ([], cont) = cont
      | prependRevDecs (revDecs, C.Let {decs, cont}) =
          C.Let {decs = List.revAppend (revDecs, decs), cont = cont}
      | prependRevDecs (revDecs, cont) =
          C.Let {decs = List.rev revDecs, cont = cont}
    fun reify (_, revDecs, REIFIED k) f =
          prependRevDecs (revDecs, f k)
      | reify (ctx, revDecs, META (hint, m)) f =
          let
            val k = genContSym ctx
            val x =
              case hint of
                NONE => genSym ctx
              | SOME x => x
          in
            prependRevDecs
              ( C.ContDec
                  { name = k
                  , params = [SOME x]
                  , body = m ([], C.Var x)
                  , attr = {alwaysInline = false}
                  } :: revDecs
              , f k
              )
          end
    fun apply revDecs (REIFIED k) arg =
          prependRevDecs (revDecs, C.AppCont {applied = k, args = [arg]})
      | apply revDecs (META (_, m)) arg = m (revDecs, arg)
    fun getResultHint (META (hint, _)) = hint
      | getResultHint (REIFIED _) = NONE
    val initialEnv =
      List.foldl TypedSyntax.VIdMap.insert' TypedSyntax.VIdMap.empty
        [ (InitialEnv.VId_false, C.BoolConst false)
        , (InitialEnv.VId_true, C.BoolConst true)
        , (InitialEnv.VId_nil, C.Nil)
        ]
    (*:
    val transform : Context * C.Value TypedSyntax.VIdMap.map -> F.Exp -> { revDecs : C.Dec list, resultHint : C.Var option } -> (C.Dec list * C.Value -> C.CExp) -> C.CExp
    and transformT : Context * C.Value TypedSyntax.VIdMap.map -> F.Exp -> C.Dec list * C.CVar -> C.CExp
    and transformX : Context * C.Value TypedSyntax.VIdMap.map -> F.Exp -> C.Dec list * cont -> C.CExp
     *)
    fun transform (ctx, env) exp {revDecs, resultHint} k =
      transformX (ctx, env) exp (revDecs, META (resultHint, k))
    and transformT (ctx, env) exp (revDecs, k) =
      transformX (ctx, env) exp (revDecs, REIFIED k)
    and transformX (ctx: Context, env) (exp: F.Exp)
      (revDecs: C.Dec list, k: cont) : C.CExp =
      case exp of
        F.PrimExp
          ( F.PrimCall Primitives.DelimCont_pushPrompt
          , _
          , [p (* 'a prompt_tag *), f (* unit -> 'a *)]
          ) =>
          transform (ctx, env) p {revDecs = revDecs, resultHint = NONE}
            (fn (revDecs, p) =>
               transform (ctx, env) f {revDecs = revDecs, resultHint = NONE}
                 (fn (revDecs, f) =>
                    reify (ctx, revDecs, k) (fn kk =>
                      C.App
                        { applied = C.Var InitialEnv.VId_DelimCont_pushPrompt
                        , cont = kk
                        , args = [p, f]
                        , attr = {}
                        })))
      | F.PrimExp
          ( F.PrimCall Primitives.DelimCont_withSubCont
          , _
          , [p (* 'b prompt_tag *), f (* ('a,'b) subcont -> 'b *)]
          ) =>
          transform (ctx, env) p {revDecs = revDecs, resultHint = NONE}
            (fn (revDecs, p) =>
               transform (ctx, env) f {revDecs = revDecs, resultHint = NONE}
                 (fn (revDecs, f) =>
                    reify (ctx, revDecs, k) (fn kk =>
                      C.App
                        { applied = C.Var InitialEnv.VId_DelimCont_withSubCont
                        , cont = kk
                        , args = [p, f]
                        , attr = {}
                        })))
      | F.PrimExp
          ( F.PrimCall Primitives.DelimCont_pushSubCont
          , _
          , [subcont (* ('a,'b) subcont *), f (* unit -> 'a *)]
          ) =>
          transform (ctx, env) subcont {revDecs = revDecs, resultHint = NONE}
            (fn (revDecs, subcont) =>
               transform (ctx, env) f {revDecs = revDecs, resultHint = NONE}
                 (fn (revDecs, f) =>
                    reify (ctx, revDecs, k) (fn kk =>
                      C.App
                        { applied = C.Var InitialEnv.VId_DelimCont_pushSubCont
                        , cont = kk
                        , args = [subcont, f]
                        , attr = {}
                        })))
      | F.PrimExp (F.PrimCall Primitives.Unsafe_cast, _, [arg]) =>
          transformX (ctx, env) arg (revDecs, k)
      | F.PrimExp (F.RaiseOp span, _, [arg]) =>
          transform (ctx, env) arg {revDecs = revDecs, resultHint = NONE}
            (fn (revDecs, arg) => prependRevDecs (revDecs, C.Raise (span, arg)))
      | F.PrimExp (F.PrimCall Primitives.unreachable, _, _) => C.Unreachable
      | F.PrimExp (primOp, tyargs, args) =>
          foldlCont
            (fn (e, (revDecs, acc), cont) =>
               transform (ctx, env) e {revDecs = revDecs, resultHint = NONE}
                 (fn (revDecs, v) => cont (revDecs, v :: acc))) (revDecs, [])
            args
            (fn (revDecs, revArgs) =>
               case primOp of
                 F.IntConstOp x =>
                   (case tyargs of
                      [F.TyVar tv] =>
                        if
                          TypedSyntax.eqUTyVar
                            (tv, F.tyNameToTyVar Typing.primTyName_int)
                        then
                          apply revDecs k (C.IntConst
                            (#defaultInt (#targetInfo ctx), x))
                        else if
                          TypedSyntax.eqUTyVar
                            (tv, F.tyNameToTyVar Typing.primTyName_int32)
                        then
                          apply revDecs k (C.IntConst (Primitives.I32, x))
                        else if
                          TypedSyntax.eqUTyVar
                            (tv, F.tyNameToTyVar Typing.primTyName_int54)
                        then
                          apply revDecs k (C.IntConst (Primitives.I54, x))
                        else if
                          TypedSyntax.eqUTyVar
                            (tv, F.tyNameToTyVar Typing.primTyName_int64)
                        then
                          apply revDecs k (C.IntConst (Primitives.I64, x))
                        else if
                          TypedSyntax.eqUTyVar
                            (tv, F.tyNameToTyVar Typing.primTyName_intInf)
                        then
                          apply revDecs k (C.IntConst (Primitives.INT_INF, x))
                        else
                          raise Fail "IntConstOp: invalid type"
                    | _ => raise Fail "IntConstOp: invalid type")
               | F.WordConstOp x =>
                   (case tyargs of
                      [F.TyVar tv] =>
                        if
                          TypedSyntax.eqUTyVar
                            (tv, F.tyNameToTyVar Typing.primTyName_word)
                        then
                          apply revDecs k (C.WordConst
                            (#defaultWord (#targetInfo ctx), x))
                        else if
                          TypedSyntax.eqUTyVar
                            (tv, F.tyNameToTyVar Typing.primTyName_word32)
                        then
                          apply revDecs k (C.WordConst (Primitives.W32, x))
                        else if
                          TypedSyntax.eqUTyVar
                            (tv, F.tyNameToTyVar Typing.primTyName_word64)
                        then
                          apply revDecs k (C.WordConst (Primitives.W64, x))
                        else
                          raise Fail "WordConstOp: invalid type"
                    | _ => raise Fail "WordConstOp: invalid type")
               | F.Char8ConstOp x =>
                   apply revDecs k
                     (C.CharConst x) (* assume the type is correct *)
               | F.Char16ConstOp x =>
                   apply revDecs k
                     (C.Char16Const x) (* assume the type is correct *)
               | F.String8ConstOp x =>
                   apply revDecs k
                     (C.StringConst x) (* assume the type is correct *)
               | F.String16ConstOp x =>
                   apply revDecs k
                     (C.String16Const x) (* assume the type is correct *)
               | _ =>
                   let
                     val args = List.rev revArgs
                     val returnArity =
                       case primOp of
                         F.PrimCall p => Primitives.returnArity p
                       | F.LuaCallNOp n => n
                       | F.LuaMethodNOp (_, n) => n
                       | _ => 1
                     val primOp =
                       case primOp of
                         F.PrimCall p =>
                           F.PrimCall
                             (Primitives.fixIntWord
                                { int = #defaultInt (#targetInfo ctx)
                                , word = #defaultWord (#targetInfo ctx)
                                } p)
                       | _ => primOp
                     val exp =
                       C.PrimOp {primOp = primOp, tyargs = tyargs, args = args}
                   in
                     case returnArity of
                       0 =>
                         apply (C.ValDec {exp = exp, results = []} :: revDecs) k
                           C.Unit
                     | 1 =>
                         let
                           val result =
                             case getResultHint k of
                               SOME r => r
                             | NONE => genSym ctx
                         in
                           apply
                             (C.ValDec {exp = exp, results = [SOME result]}
                              :: revDecs) k (C.Var result)
                         end
                     | _ =>
                         let
                           val results = List.tabulate (returnArity, fn _ =>
                             genSym ctx)
                           val tupleVar =
                             case getResultHint k of
                               SOME r => r
                             | NONE => genSym ctx
                           val (_, tuple) =
                             List.foldl
                               (fn (v, (i, acc)) =>
                                  ( i + 1
                                  , Syntax.LabelMap.insert
                                      (acc, Syntax.NumericLabel i, C.Var v)
                                  )) (1, Syntax.LabelMap.empty) results
                         in
                           apply
                             (C.ValDec
                                { exp = C.Record tuple
                                , results = [SOME tupleVar]
                                }
                              ::
                              C.ValDec
                                {exp = exp, results = List.map SOME results}
                              :: revDecs) k (C.Var tupleVar)
                         end
                   end)
      | F.VarExp vid =>
          (case TypedSyntax.VIdMap.find (env, vid) of
             SOME v => apply revDecs k v
           | NONE => apply revDecs k (C.Var vid))
      | F.RecordExp [] => apply revDecs k C.Unit
      | F.RecordExp fields =>
          foldlCont
            (fn ((label, exp), (revDecs, acc), cont) =>
               transform (ctx, env) exp {revDecs = revDecs, resultHint = NONE}
                 (fn (revDecs, v) => cont (revDecs, (label, v) :: acc)))
            (revDecs, []) fields
            (fn (revDecs, revFields) =>
               let
                 val result =
                   case getResultHint k of
                     SOME r => r
                   | NONE => genSym ctx
               in
                 apply
                   (C.ValDec
                      { exp = C.Record
                          (List.foldr Syntax.LabelMap.insert'
                             Syntax.LabelMap.empty revFields)
                      , results = [SOME result]
                      } :: revDecs) k (C.Var result)
               end)
      | F.LetExp (decs, finalExp) =>
          let
            fun doDecs (env, [], revDecs) =
                  transformX (ctx, env) finalExp (revDecs, k)
              | doDecs (env, F.ValDec (vid, _, exp) :: decs, revDecs) =
                  transform (ctx, env) exp
                    {revDecs = revDecs, resultHint = SOME vid}
                    (fn (revDecs, v) =>
                       doDecs
                         ( TypedSyntax.VIdMap.insert (env, vid, v)
                         , decs
                         , revDecs
                         ))
              | doDecs (env, F.RecValDec decs' :: decs, revDecs) =
                  let
                    val dec = C.RecDec
                      (List.map
                         (fn (vid, _, exp) =>
                            let
                              val contParam = genContSym ctx
                            in
                              case stripTyAbs exp of
                                F.FnExp (param, _, body) =>
                                  { name = vid
                                  , contParam = contParam
                                  , params = [param]
                                  , body =
                                      transformT (ctx, env) body ([], contParam)
                                  , attr = {alwaysInline = false}
                                  }
                              | _ => raise Fail "RecValDec"
                            end) decs')
                  in
                    doDecs (env, decs, dec :: revDecs)
                  end
              | doDecs (env, F.UnpackDec (_, _, vid, _, exp) :: decs, revDecs) =
                  transform (ctx, env) exp
                    {revDecs = revDecs, resultHint = SOME vid}
                    (fn (revDecs, v) =>
                       doDecs
                         ( TypedSyntax.VIdMap.insert (env, vid, v)
                         , decs
                         , revDecs
                         ))
              | doDecs (env, F.IgnoreDec exp :: decs, revDecs) =
                  transform (ctx, env) exp
                    {revDecs = revDecs, resultHint = NONE}
                    (fn (revDecs, _) => doDecs (env, decs, revDecs))
              | doDecs (env, F.DatatypeDec _ :: decs, revDecs) =
                  doDecs (env, decs, revDecs)
              | doDecs
                  ( env
                  , F.ExceptionDec {name, tagName, payloadTy} :: decs
                  , revDecs
                  ) =
                  let
                    val dec = C.ValDec
                      { exp = C.ExnTag {name = name, payloadTy = payloadTy}
                      , results = [SOME tagName]
                      }
                  in
                    doDecs (env, decs, dec :: revDecs)
                  end
              | doDecs
                  ( env
                  , F.ESImportDec {pure, specs, moduleName} :: decs
                  , revDecs
                  ) =
                  let
                    val dec = C.ESImportDec
                      { pure = pure
                      , specs =
                          List.map (fn (name, vid, _) => (name, vid)) specs
                      , moduleName = moduleName
                      }
                  in
                    doDecs (env, decs, dec :: revDecs)
                  end
          in
            doDecs (env, decs, revDecs)
          end
      | F.AppExp (applied, arg) =>
          transform (ctx, env) applied {revDecs = revDecs, resultHint = NONE}
            (fn (revDecs, f) =>
               transform (ctx, env) arg {revDecs = revDecs, resultHint = NONE}
                 (fn (revDecs, v) =>
                    reify (ctx, revDecs, k) (fn j =>
                      C.App {applied = f, cont = j, args = [v], attr = {}})))
      | F.HandleExp {body, exnName, handler} =>
          reify (ctx, revDecs, k) (fn j =>
            let
              val success = genContSym ctx
            in
              C.Handle
                { body = transformT (ctx, env) body ([], success)
                , handler = (exnName, transformT (ctx, env) handler ([], j))
                , successfulExitIn = success
                , successfulExitOut = j
                }
            end)
      | F.IfThenElseExp (e1, e2, e3) =>
          transform (ctx, env) e1 {revDecs = revDecs, resultHint = NONE}
            (fn (revDecs, e1) =>
               reify (ctx, revDecs, k) (fn j =>
                 C.If
                   { cond = e1
                   , thenCont = transformT (ctx, env) e2 ([], j)
                   , elseCont = transformT (ctx, env) e3 ([], j)
                   }))
      | F.CaseExp _ => raise Fail "CaseExp: not supported here"
      | F.FnExp (vid, _, body) =>
          let
            val f =
              case getResultHint k of
                SOME f => f
              | NONE => genSym ctx
            val kk = genContSym ctx
            val dec = C.ValDec
              { exp = C.Abs
                  { contParam = kk
                  , params = [vid]
                  , body = transformT (ctx, env) body ([], kk)
                  , attr = {alwaysInline = false}
                  }
              , results = [SOME f]
              }
          in
            apply (dec :: revDecs) k (C.Var f)
          end
      | F.ProjectionExp {label, record, fieldTypes} =>
          transform (ctx, env) record {revDecs = revDecs, resultHint = NONE}
            (fn (revDecs, record) =>
               let
                 val x =
                   case getResultHint k of
                     SOME x => x
                   | NONE => genSym ctx
                 val dec = C.ValDec
                   { exp =
                       C.Projection
                         { label = label
                         , record = record
                         , fieldTypes = fieldTypes
                         }
                   , results = [SOME x]
                   }
               in
                 apply (dec :: revDecs) k (C.Var x)
               end)
      | F.TyAbsExp (_, _, exp) => transformX (ctx, env) exp (revDecs, k)
      | F.TyAppExp (exp, _) => transformX (ctx, env) exp (revDecs, k)
      | F.PackExp {payloadTy = _, exp, packageTy = _} =>
          transformX (ctx, env) exp (revDecs, k)
      | F.BogusExp _ => raise Message.Abort
      | F.ExitProgram =>
          (case k of
             REIFIED k =>
               prependRevDecs (revDecs, C.AppCont {applied = k, args = []})
           | META _ => raise Fail "unexpected META")
      | F.ExportValue exp =>
          (case k of
             REIFIED k =>
               transform (ctx, env) exp {revDecs = revDecs, resultHint = NONE}
                 (fn (revDecs, v) =>
                    prependRevDecs
                      (revDecs, C.AppCont {applied = k, args = [v]}))
           | META _ => raise Fail "unexpected META")
      | F.ExportModule entities =>
          (case k of
             REIFIED k =>
               foldlCont
                 (fn ((name, exp), (revDecs, acc), cont) =>
                    transform (ctx, env) exp
                      {revDecs = revDecs, resultHint = NONE (* name? *)}
                      (fn (revDecs, v) => cont (revDecs, (name, v) :: acc)))
                 (revDecs, []) (Vector.foldr (op::) [] entities)
                 (fn (revDecs, items) =>
                    if #exportAsRecord ctx then
                      let
                        val result = genSym ctx (* "export"? *)
                        val dec = C.ValDec
                          { exp = C.Record
                              (List.foldl
                                 (fn ((name, v), m) =>
                                    Syntax.LabelMap.insert
                                      (m, Syntax.IdentifierLabel name, v))
                                 Syntax.LabelMap.empty items)
                          , results = [SOME result]
                          }
                      in
                        prependRevDecs (dec :: revDecs, C.AppCont
                          {applied = k, args = [C.Var result]})
                      end
                    else
                      prependRevDecs (revDecs, C.AppCont
                        { applied = k
                        , args =
                            List.foldl (fn ((_, v), acc) => v :: acc) [] items
                        }))
           | META _ => raise Fail "unexpected META")
  end
end;

structure CpsSimplify :>
sig
  type Context = {nextVId: int ref, simplificationOccurred: bool ref}
  type value_info = {exp: CSyntax.SimpleExp option, isDiscardableFunction: bool}
  val genContSym: Context -> CSyntax.CVar
  val newVId: Context * string -> TypedSyntax.VId
  val renewVId: Context * TypedSyntax.VId -> TypedSyntax.VId
  val renewCVar: Context * CSyntax.CVar -> CSyntax.CVar
  val substSimpleExp:
    CSyntax.Value TypedSyntax.VIdMap.map
    * CSyntax.CVar CSyntax.CVarMap.map
    * CSyntax.SimpleExp
    -> CSyntax.SimpleExp
  val sizeOfCExp: CSyntax.CExp * int -> int
  val substValue: CSyntax.Value TypedSyntax.VIdMap.map
                  -> CSyntax.Value
                  -> CSyntax.Value
  val substCVar: CSyntax.CVar CSyntax.CVarMap.map
                 -> CSyntax.CVar
                 -> CSyntax.CVar
  val substCExp:
    CSyntax.Value TypedSyntax.VIdMap.map
    * CSyntax.CVar CSyntax.CVarMap.map
    * CSyntax.CExp
    -> CSyntax.CExp
  val alphaConvert:
    Context
    * CSyntax.Value TypedSyntax.VIdMap.map
    * CSyntax.CVar CSyntax.CVarMap.map
    * CSyntax.CExp
    -> CSyntax.CExp
  val isDiscardableExp: value_info TypedSyntax.VIdMap.map * CSyntax.CExp -> bool
  val finalizeCExp: Context * CSyntax.CExp -> CSyntax.CExp
end =
struct
  local structure F = FSyntax structure C = CSyntax
  in
    type Context = {nextVId: int ref, simplificationOccurred: bool ref}
    fun genContSym (ctx: Context) : CSyntax.CVar =
      let
        val n = !(#nextVId ctx)
        val _ = #nextVId ctx := n + 1
      in
        CSyntax.CVar.fromInt n
      end
    fun newVId ({nextVId, ...}: Context, name) =
      let val n = !nextVId
      in TypedSyntax.MkVId (name, n) before (nextVId := n + 1)
      end
    fun renewVId ({nextVId, ...}: Context, TypedSyntax.MkVId (name, _)) =
      let val n = !nextVId
      in TypedSyntax.MkVId (name, n) before (nextVId := n + 1)
      end
    fun renewCVar ({nextVId, ...}: Context, _: C.CVar) =
      let val n = !nextVId
      in C.CVar.fromInt n before (nextVId := n + 1)
      end
    fun sizeOfSimpleExp (e, threshold) =
      if threshold < 0 then
        threshold
      else
        case e of
          C.PrimOp {primOp = _, tyargs = _, args} =>
            threshold - List.length args
        | C.Record fields => threshold - Syntax.LabelMap.numItems fields
        | C.ExnTag _ => threshold - 1
        | C.Projection _ => threshold - 1
        | C.Abs {contParam = _, params = _, body, attr = _} =>
            sizeOfCExp (body, threshold)
    and sizeOfDec (dec, threshold) =
      if threshold < 0 then
        threshold
      else
        case dec of
          C.ValDec {exp, results = _} => sizeOfSimpleExp (exp, threshold)
        | C.RecDec defs =>
            List.foldl (fn ({body, ...}, t) => sizeOfCExp (body, t)) threshold
              defs
        | C.ContDec {name = _, params = _, body, attr = _} =>
            sizeOfCExp (body, threshold)
        | C.RecContDec defs =>
            List.foldl (fn ((_, _, body), t) => sizeOfCExp (body, t)) threshold
              defs
        | C.ESImportDec _ => 0
    and sizeOfCExp (e, threshold) =
      if threshold < 0 then
        threshold
      else
        case e of
          C.Let {decs, cont} =>
            List.foldl sizeOfDec (sizeOfCExp (cont, threshold)) decs
        | C.App {applied = _, cont = _, args, attr = _} =>
            threshold - List.length args
        | C.AppCont {applied = _, args} => threshold - List.length args
        | C.If {cond = _, thenCont, elseCont} =>
            sizeOfCExp (elseCont, sizeOfCExp (thenCont, threshold - 1))
        | C.Handle
            { body
            , handler = (_, h)
            , successfulExitIn = _
            , successfulExitOut = _
            } => sizeOfCExp (body, sizeOfCExp (h, threshold - 1))
        | C.Raise _ => threshold
        | C.Unreachable => threshold
    fun substValue (subst: C.Value TypedSyntax.VIdMap.map) (x as C.Var v) =
          (case TypedSyntax.VIdMap.find (subst, v) of
             SOME w => w
           | NONE => x)
      | substValue _ v = v
    fun substCVar (csubst: C.CVar C.CVarMap.map) v =
      case C.CVarMap.find (csubst, v) of
        SOME w => w
      | NONE => v
    fun substSimpleExp (subst, _, C.PrimOp {primOp, tyargs, args}) =
          C.PrimOp
            { primOp = primOp
            , tyargs = tyargs
            , args = List.map (substValue subst) args
            }
      | substSimpleExp (subst, _, C.Record fields) =
          C.Record (Syntax.LabelMap.map (substValue subst) fields)
      | substSimpleExp (_, _, e as C.ExnTag _) = e
      | substSimpleExp (subst, _, C.Projection {label, record, fieldTypes}) =
          C.Projection
            { label = label
            , record = substValue subst record
            , fieldTypes = fieldTypes
            }
      | substSimpleExp (subst, csubst, C.Abs {contParam, params, body, attr}) =
          C.Abs
            { contParam = contParam
            , params = params
            , body = substCExp (subst, csubst, body)
            , attr = attr
            }
    and substDec (subst, csubst) =
      fn C.ValDec {exp, results} =>
        C.ValDec {exp = substSimpleExp (subst, csubst, exp), results = results}
       | C.RecDec defs =>
        C.RecDec
          (List.map
             (fn {name, contParam, params, body, attr} =>
                { name = name
                , contParam = contParam
                , params = params
                , body = substCExp (subst, csubst, body)
                , attr = attr
                }) defs)
       | C.ContDec {name, params, body, attr} =>
        C.ContDec
          { name = name
          , params = params
          , body = substCExp (subst, csubst, body)
          , attr = attr
          }
       | C.RecContDec defs =>
        C.RecContDec
          (List.map
             (fn (f, params, body) =>
                (f, params, substCExp (subst, csubst, body))) defs)
       | dec as C.ESImportDec _ => dec
    and substCExp
          ( subst: C.Value TypedSyntax.VIdMap.map
          , csubst: C.CVar C.CVarMap.map
          , C.Let {decs, cont}
          ) =
          C.Let
            { decs = List.map (substDec (subst, csubst)) decs
            , cont = substCExp (subst, csubst, cont)
            }
      | substCExp (subst, csubst, C.App {applied, cont, args, attr}) =
          C.App
            { applied = substValue subst applied
            , cont = substCVar csubst cont
            , args = List.map (substValue subst) args
            , attr = attr
            }
      | substCExp (subst, csubst, C.AppCont {applied, args}) =
          C.AppCont
            { applied = substCVar csubst applied
            , args = List.map (substValue subst) args
            }
      | substCExp (subst, csubst, C.If {cond, thenCont, elseCont}) =
          C.If
            { cond = substValue subst cond
            , thenCont = substCExp (subst, csubst, thenCont)
            , elseCont = substCExp (subst, csubst, elseCont)
            }
      | substCExp
          ( subst
          , csubst
          , C.Handle
              {body, handler = (e, h), successfulExitIn, successfulExitOut}
          ) =
          C.Handle
            { body = substCExp (subst, csubst, body)
            , handler = (e, substCExp (subst, csubst, h))
            , successfulExitIn = successfulExitIn
            , successfulExitOut = substCVar csubst successfulExitOut
            }
      | substCExp (subst, _, C.Raise (span, x)) =
          C.Raise (span, substValue subst x)
      | substCExp (_, _, e as C.Unreachable) = e
    val substCExp = fn (subst, csubst, e) =>
      if TypedSyntax.VIdMap.isEmpty subst andalso C.CVarMap.isEmpty csubst then
        e
      else
        substCExp (subst, csubst, e)
    fun alphaConvertSimpleExp
          (ctx, subst, csubst, C.Abs {contParam, params, body, attr}) =
          let
            val (params', subst') =
              List.foldr
                (fn (p, (params', subst)) =>
                   let
                     val p' = renewVId (ctx, p)
                   in
                     ( p' :: params'
                     , TypedSyntax.VIdMap.insert (subst, p, C.Var p')
                     )
                   end) ([], subst) params
            val contParam' = renewCVar (ctx, contParam)
            val csubst' = C.CVarMap.insert (csubst, contParam, contParam')
          in
            C.Abs
              { contParam = contParam'
              , params = params'
              , body = alphaConvert (ctx, subst', csubst', body)
              , attr = attr
              }
          end
      | alphaConvertSimpleExp (_, subst, csubst, e) =
          substSimpleExp (subst, csubst, e)
    and alphaConvertDec (ctx: Context) (dec, (subst, csubst, acc)) =
      case dec of
        C.ValDec {exp, results} =>
          let
            val (results', subst') =
              List.foldr
                (fn (SOME result, (acc, subst)) =>
                   let
                     val result' = renewVId (ctx, result)
                   in
                     ( SOME result' :: acc
                     , TypedSyntax.VIdMap.insert (subst, result, C.Var result')
                     )
                   end
                  | (NONE, (acc, subst)) => (NONE :: acc, subst)) ([], subst)
                results
            val dec' = C.ValDec
              { exp = alphaConvertSimpleExp (ctx, subst, csubst, exp)
              , results = results'
              }
          in
            (subst', csubst, dec' :: acc)
          end
      | C.RecDec defs =>
          let
            val (subst, nameMap) =
              List.foldl
                (fn ({name, ...}, (subst, nameMap)) =>
                   let
                     val name' = renewVId (ctx, name)
                   in
                     ( TypedSyntax.VIdMap.insert (subst, name, C.Var name')
                     , TypedSyntax.VIdMap.insert (nameMap, name, name')
                     )
                   end) (subst, TypedSyntax.VIdMap.empty) defs
            val dec' = C.RecDec
              (List.map
                 (fn {name, contParam, params, body, attr} =>
                    let
                      val name' = TypedSyntax.VIdMap.lookup (nameMap, name)
                      val contParam' = renewCVar (ctx, contParam)
                      val (params', subst) =
                        List.foldr
                          (fn (p, (params', subst)) =>
                             let
                               val p' = renewVId (ctx, p)
                             in
                               ( p' :: params'
                               , TypedSyntax.VIdMap.insert (subst, p, C.Var p')
                               )
                             end) ([], subst) params
                      val csubst =
                        C.CVarMap.insert (csubst, contParam, contParam')
                    in
                      { name = name'
                      , contParam = contParam'
                      , params = params'
                      , body = alphaConvert (ctx, subst, csubst, body)
                      , attr = attr
                      }
                    end) defs)
          in
            (subst, csubst, dec' :: acc)
          end
      | C.ContDec {name, params, body, attr} =>
          let
            val (params', subst') =
              List.foldr
                (fn (SOME p, (params', subst)) =>
                   let
                     val p' = renewVId (ctx, p)
                   in
                     ( SOME p' :: params'
                     , TypedSyntax.VIdMap.insert (subst, p, C.Var p')
                     )
                   end
                  | (NONE, (params', subst)) => (NONE :: params', subst))
                ([], subst) params
            val body = alphaConvert (ctx, subst', csubst, body)
            val name' = renewCVar (ctx, name)
            val csubst = C.CVarMap.insert (csubst, name, name')
            val dec' = C.ContDec
              {name = name', params = params', body = body, attr = attr}
          in
            (subst, csubst, dec' :: acc)
          end
      | C.RecContDec defs =>
          let
            val (csubst, nameMap) =
              List.foldl
                (fn ((f, _, _), (csubst, nameMap)) =>
                   let
                     val f' = renewCVar (ctx, f)
                   in
                     ( C.CVarMap.insert (csubst, f, f')
                     , C.CVarMap.insert (nameMap, f, f')
                     )
                   end) (csubst, C.CVarMap.empty) defs
            val dec' = C.RecContDec
              (List.map
                 (fn (f, params, body) =>
                    let
                      val f' = C.CVarMap.lookup (nameMap, f)
                      val (params', subst) =
                        List.foldr
                          (fn (SOME p, (params', subst)) =>
                             let
                               val p' = renewVId (ctx, p)
                             in
                               ( SOME p' :: params'
                               , TypedSyntax.VIdMap.insert (subst, p, C.Var p')
                               )
                             end
                            | (NONE, (params', subst)) => (params', subst))
                          ([], subst) params
                    in
                      (f', params', alphaConvert (ctx, subst, csubst, body))
                    end) defs)
          in
            (subst, csubst, dec' :: acc)
          end
      | C.ESImportDec {pure, specs, moduleName} =>
          let
            val specs' =
              List.map (fn (name, vid) => (name, vid, renewVId (ctx, vid)))
                specs
            val subst' =
              List.foldl
                (fn ((_, vid, vid'), subst) =>
                   TypedSyntax.VIdMap.insert (subst, vid, C.Var vid')) subst
                specs'
            val dec' = C.ESImportDec
              { pure = pure
              , specs = List.map (fn (name, _, vid) => (name, vid)) specs'
              , moduleName = moduleName
              }
          in
            (subst', csubst, dec' :: acc)
          end
    and alphaConvert
          ( ctx: Context
          , subst: C.Value TypedSyntax.VIdMap.map
          , csubst: C.CVar C.CVarMap.map
          , C.Let {decs, cont}
          ) =
          let
            val (subst', csubst', revDecs) =
              List.foldl (alphaConvertDec ctx) (subst, csubst, []) decs
          in
            C.Let
              { decs = List.rev revDecs
              , cont = alphaConvert (ctx, subst', csubst', cont)
              }
          end
      | alphaConvert (_, subst, csubst, C.App {applied, cont, args, attr}) =
          C.App
            { applied = substValue subst applied
            , cont = substCVar csubst cont
            , args = List.map (substValue subst) args
            , attr = attr
            }
      | alphaConvert (_, subst, csubst, C.AppCont {applied, args}) =
          C.AppCont
            { applied = substCVar csubst applied
            , args = List.map (substValue subst) args
            }
      | alphaConvert (ctx, subst, csubst, C.If {cond, thenCont, elseCont}) =
          C.If
            { cond = substValue subst cond
            , thenCont = alphaConvert (ctx, subst, csubst, thenCont)
            , elseCont = alphaConvert (ctx, subst, csubst, elseCont)
            }
      | alphaConvert
          ( ctx
          , subst
          , csubst
          , C.Handle
              {body, handler = (e, h), successfulExitIn, successfulExitOut}
          ) =
          let
            val successfulExitIn' = renewCVar (ctx, successfulExitIn)
            val csubst' =
              C.CVarMap.insert (csubst, successfulExitIn, successfulExitIn')
            val e' = renewVId (ctx, e)
            val subst' = TypedSyntax.VIdMap.insert (subst, e, C.Var e')
          in
            C.Handle
              { body = alphaConvert (ctx, subst, csubst', body)
              , handler = (e', alphaConvert (ctx, subst', csubst, h))
              , successfulExitIn = successfulExitIn'
              , successfulExitOut = substCVar csubst successfulExitOut
              }
          end
      | alphaConvert (_, subst, _, C.Raise (span, x)) =
          C.Raise (span, substValue subst x)
      | alphaConvert (_, _, _, e as C.Unreachable) = e
    type value_info = {exp: C.SimpleExp option, isDiscardableFunction: bool}
    fun isDiscardableDec (dec, env: value_info TypedSyntax.VIdMap.map) =
      case dec of
        C.ValDec {exp, results} =>
          (case exp of
             C.Abs {body, ...} =>
               let
                 val env =
                   case results of
                     [SOME result] =>
                       TypedSyntax.VIdMap.insert
                         ( env
                         , result
                         , { exp = NONE
                           , isDiscardableFunction =
                               isDiscardableExp (env, body)
                           }
                         )
                   | _ => env
               in
                 SOME env
               end
           | _ => if C.isDiscardable exp then SOME env else NONE)
      | C.RecDec _ => SOME env
      | C.ContDec {name = _, params = _, body, attr = _} =>
          if isDiscardableExp (env, body) then SOME env else NONE
      | C.RecContDec _ => NONE
      | C.ESImportDec {pure = _, specs = _, moduleName = _} => SOME env
    and isDiscardableExp
          (env: value_info TypedSyntax.VIdMap.map, C.Let {decs, cont}) =
          (case ListUtil.foldlOption isDiscardableDec env decs of
             SOME env => isDiscardableExp (env, cont)
           | NONE => false)
      | isDiscardableExp
          (env, C.App {applied = C.Var applied, cont = _, args = _, attr = _}) =
          (case TypedSyntax.VIdMap.find (env, applied) of
             SOME {isDiscardableFunction = true, ...} => true
           | _ => false)
      | isDiscardableExp (_, C.App _) = false (* should not occur *)
      | isDiscardableExp (_, C.AppCont _) = true
      | isDiscardableExp (env, C.If {cond = _, thenCont, elseCont}) =
          isDiscardableExp (env, thenCont)
          andalso isDiscardableExp (env, elseCont)
      | isDiscardableExp
          ( env
          , C.Handle
              { body
              , handler = (_, h)
              , successfulExitIn = _
              , successfulExitOut = _
              }
          ) =
          isDiscardableExp (env, body) andalso isDiscardableExp (env, h)
      | isDiscardableExp (_, C.Raise _) = false
      | isDiscardableExp (_, C.Unreachable) = false
    fun prependDecs ([], cont) = cont
      | prependDecs (decs, C.Let {decs = decs', cont}) =
          C.Let {decs = decs @ decs', cont = cont}
      | prependDecs (decs, cont) = C.Let {decs = decs, cont = cont}
    (* Eliminate assumeDiscardable *)
    (* More sophisticated analysis is wanted. *)
    fun finalizeDec ctx (dec, (decs, cont)) =
      case dec of
        C.ValDec
          { exp =
              C.PrimOp
                { primOp = F.PrimCall Primitives.assumeDiscardable
                , tyargs = _
                , args = [f, arg]
                }
          , results = [SOME result]
          } =>
          let
            val name = genContSym ctx
          in
            ( [C.ContDec
                 { name = name
                 , params = [SOME result]
                 , body = prependDecs (decs, cont)
                 , attr = {alwaysInline = false}
                 }]
            , C.App {applied = f, cont = name, args = [arg], attr = {}}
            )
          end
      | C.ValDec
          { exp =
              C.PrimOp
                { primOp = F.PrimCall Primitives.assumeDiscardable
                , tyargs = _
                , args = _
                }
          , results = _
          } => raise Fail "assumeDiscardable: invalid argument"
      | C.ValDec {exp = C.PrimOp _, results = _} => (dec :: decs, cont)
      | C.ValDec {exp = C.Record _, results = _} => (dec :: decs, cont)
      | C.ValDec {exp = C.ExnTag _, results = _} => (dec :: decs, cont)
      | C.ValDec {exp = C.Projection _, results = _} => (dec :: decs, cont)
      | C.ValDec {exp = C.Abs {contParam, params, body, attr}, results} =>
          let
            val dec = C.ValDec
              { exp = C.Abs
                  { contParam = contParam
                  , params = params
                  , body = finalizeCExp (ctx, body)
                  , attr = attr
                  }
              , results = results
              }
          in
            (dec :: decs, cont)
          end
      | C.RecDec defs =>
          let
            val dec = C.RecDec
              (List.map
                 (fn {name, contParam, params, body, attr} =>
                    { name = name
                    , contParam = contParam
                    , params = params
                    , body = finalizeCExp (ctx, body)
                    , attr = attr
                    }) defs)
          in
            (dec :: decs, cont)
          end
      | C.ContDec {name, params, body, attr} =>
          let
            val dec = C.ContDec
              { name = name
              , params = params
              , body = finalizeCExp (ctx, body)
              , attr = attr
              }
          in
            (dec :: decs, cont)
          end
      | C.RecContDec defs =>
          let
            val dec = C.RecContDec
              (List.map
                 (fn (name, params, body) =>
                    (name, params, finalizeCExp (ctx, body))) defs)
          in
            (dec :: decs, cont)
          end
      | C.ESImportDec _ => (dec :: decs, cont)
    and finalizeCExp (ctx, C.Let {decs, cont}) =
          prependDecs
            (List.foldr (finalizeDec ctx) ([], finalizeCExp (ctx, cont)) decs)
      | finalizeCExp (_, e as C.App _) = e
      | finalizeCExp (_, e as C.AppCont _) = e
      | finalizeCExp (ctx, C.If {cond, thenCont, elseCont}) =
          C.If
            { cond = cond
            , thenCont = finalizeCExp (ctx, thenCont)
            , elseCont = finalizeCExp (ctx, elseCont)
            }
      | finalizeCExp
          ( ctx
          , C.Handle
              {body, handler = (e, h), successfulExitIn, successfulExitOut}
          ) =
          C.Handle
            { body = finalizeCExp (ctx, body)
            , handler = (e, finalizeCExp (ctx, h))
            , successfulExitIn = successfulExitIn
            , successfulExitOut = successfulExitOut
            }
      | finalizeCExp (_, e as C.Raise _) = e
      | finalizeCExp (_, e as C.Unreachable) = e
  end
end;

structure CpsAnalyze :>
sig
  type cont_map
  val escapes: cont_map * CSyntax.CVar -> bool
  val escapesTransitively: cont_map * CSyntax.CVar -> bool
  val contEscape: CSyntax.CVar * CSyntax.CExp -> cont_map
end =
struct
  local structure C = CSyntax
  in
    type cont_map =
      {escapes: bool, escapesTransitively: bool} CSyntax.CVarTable.hash_table
    fun escapes (t: cont_map, v) =
      #escapes (C.CVarTable.lookup t v)
    fun escapesTransitively (t: cont_map, v) =
      #escapesTransitively (C.CVarTable.lookup t v)
    type table =
      { escapes: bool ref
      , escapesTransitively: bool ref
      , level: int
      , free: CSyntax.CVarSet.set
      } CSyntax.CVarTable.hash_table
    fun direct (table: table, level, k, acc) =
      let
        val {escapes = _, escapesTransitively = _, level = level', free = _} =
          C.CVarTable.lookup table k
      in
        if level' < level then C.CVarSet.add (acc, k) else acc
      end
    fun recEscape (table: table) k =
      let
        val {escapes = _, escapesTransitively, level = _, free} =
          C.CVarTable.lookup table k
      in
        if !escapesTransitively then ()
        else (escapesTransitively := true; C.CVarSet.app (recEscape table) free)
      end
    fun escape (table: table, level, k, acc) =
      let
        val {escapes, escapesTransitively = _, level = level', free = _} =
          C.CVarTable.lookup table k
      in
        escapes := true;
        if level' < level then C.CVarSet.add (acc, k) else acc
      end
    fun goDec (table, level) (dec, acc) =
      case dec of
        C.ValDec
          {exp = C.Abs {contParam, params = _, body, attr = _}, results = _} =>
          ( C.CVarTable.insert table
              ( contParam
              , { escapes = ref false
                , escapesTransitively = ref false
                , level = 0
                , free = C.CVarSet.empty
                }
              )
          ; go (table, 0, body, acc)
          )
      | C.ValDec {exp = _, results = _} => acc
      | C.RecDec defs =>
          List.foldl
            (fn ({contParam, body, ...}, acc) =>
               ( C.CVarTable.insert table
                   ( contParam
                   , { escapes = ref false
                     , escapesTransitively = ref false
                     , level = 0
                     , free = C.CVarSet.empty
                     }
                   )
               ; go (table, 0, body, acc)
               )) acc defs
      | C.ContDec {name, params = _, body, attr = _} =>
          let
            val free = go (table, level + 1, body, C.CVarSet.empty)
          in
            C.CVarTable.insert table
              ( name
              , { escapes = ref false
                , escapesTransitively = ref false
                , level = level
                , free = free
                }
              );
            C.CVarSet.union (acc, free)
          end
      | C.RecContDec defs =>
          ( List.app
              (fn (name, _, _) =>
                 C.CVarTable.insert table
                   ( name
                   , { escapes = ref false
                     , escapesTransitively = ref false
                     , level = level
                     , free = C.CVarSet.empty
                     }
                   )) defs
          ; List.foldl
              (fn ((name, _, body), acc) =>
                 let
                   val {escapes, escapesTransitively, level, free = _} =
                     C.CVarTable.lookup table name
                   val free = go (table, level + 1, body, C.CVarSet.empty)
                 in
                   C.CVarTable.insert table
                     ( name
                     , { escapes = escapes
                       , escapesTransitively = escapesTransitively
                       , level = level
                       , free = free
                       }
                     );
                   C.CVarSet.union (acc, free)
                 end) acc defs
          )
      | C.ESImportDec _ => acc
    and go (table, level, C.Let {decs, cont}, acc) =
          go (table, level, cont, List.foldl (goDec (table, level)) acc decs)
      | go (table, level, C.App {applied = _, cont, args = _, attr = _}, acc) =
          escape (table, level, cont, acc)
      | go (table, level, C.AppCont {applied, args = _}, acc) =
          direct (table, level, applied, acc)
      | go (table, level, C.If {cond = _, thenCont, elseCont}, acc) =
          go (table, level, elseCont, go (table, level, thenCont, acc))
      | go
          ( table
          , level
          , C.Handle
              {body, handler = (_, h), successfulExitIn, successfulExitOut}
          , acc
          ) =
          let
            val free = go (table, level + 1, h, C.CVarSet.empty)
          in
            C.CVarTable.insert table
              ( successfulExitIn
              , { escapes = ref false
                , escapesTransitively = ref false
                , level = level
                , free = C.CVarSet.singleton successfulExitOut
                }
              );
            C.CVarSet.app
              (fn k => ignore (escape (table, level + 1, k, C.CVarSet.empty)))
              free;
            go (table, level, body, C.CVarSet.union (acc, free))
          end
      | go (_, _, C.Raise _, acc) = acc
      | go (_, _, C.Unreachable, acc) = acc
    fun contEscape (cont, cexp) =
      let
        val table =
          C.CVarTable.mkTable (1, C.InvalidCode "unbound continuation")
      in
        C.CVarTable.insert table
          ( cont
          , { escapes = ref false
            , escapesTransitively = ref false
            , level = 0
            , free = C.CVarSet.empty
            }
          );
        ignore (go (table, 0, cexp, C.CVarSet.empty));
        C.CVarTable.appi
          (fn (k, {escapes = ref true, escapesTransitively = ref false, ...}) =>
             recEscape table k
            | _ => ()) table;
        C.CVarTable.map
          (fn {escapes, escapesTransitively, ...} =>
             {escapes = !escapes, escapesTransitively = !escapesTransitively})
          table
      end
  end (* local *)
end; (* structure CpsAnalyze *)
