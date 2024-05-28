(*
 * Copyright (c) 2023 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure CSyntax :> sig
              exception InvalidCode of string
              type Var = TypedSyntax.VId
              structure CVar : sig
                            eqtype t
                            type ord_key = t
                            val compare : ord_key * ord_key -> order
                            val fromInt : int -> t
                            val toInt : t -> int
                            val dummy : t
                        end
              type CVar = CVar.t
              structure CVarSet : ORD_SET where type Key.ord_key = CVar.t
              structure CVarMap : ORD_MAP where type Key.ord_key = CVar.t
              structure CVarTable : MONO_HASH_TABLE where type Key.hash_key = CVar.t
              datatype Value = Var of Var
                             | Unit (* : unit *)
                             | Nil (* : 'a list *)
                             | BoolConst of bool
                             | IntConst of Primitives.int_width * IntInf.int
                             | WordConst of Primitives.word_width * IntInf.int
                             | CharConst of char
                             | Char16Const of int
                             | StringConst of string
                             | String16Const of int vector
              type AbsAttr = { isWrapper : bool }
              datatype SimpleExp = PrimOp of { primOp : FSyntax.PrimOp, tyargs : FSyntax.Ty list, args : Value list }
                                 | Record of Value Syntax.LabelMap.map (* non-empty record *)
                                 | ExnTag of { name : string, payloadTy : FSyntax.Ty option }
                                 | Projection of { label : Syntax.Label, record : Value, fieldTypes : FSyntax.Ty Syntax.LabelMap.map }
                                 | Abs of { contParam : CVar, params : Var list, body : CExp, attr : AbsAttr } (* non-recursive function *)
                   and Dec = ValDec of { exp : SimpleExp, result : Var option }
                           | RecDec of { name : Var, contParam : CVar, params : Var list, body : CExp, attr : AbsAttr} list (* recursive function *)
                           | ContDec of { name : CVar, params : (Var option) list, body : CExp }
                           | RecContDec of (CVar * (Var option) list * CExp) list
                           | ESImportDec of { pure : bool, specs : (Syntax.ESImportName * Var) list, moduleName : string }
                   and CExp = Let of { decs : Dec vector, cont : CExp }
                            | App of { applied : Value, cont : CVar, args : Value list } (* tail call *) (* return arity? *)
                            | AppCont of { applied : CVar, args : Value list }
                            | If of { cond : Value
                                    , thenCont : CExp
                                    , elseCont : CExp
                                    }
                            | Handle of { body : CExp, handler : Var * CExp, successfulExitIn : CVar, successfulExitOut : CVar }
                            | Unreachable
              val isDiscardable : SimpleExp -> bool
              val containsApp : CExp -> bool
              val freeVarsInExp : TypedSyntax.VIdSet.set * CExp * TypedSyntax.VIdSet.set -> TypedSyntax.VIdSet.set
              val recurseCExp : (CExp -> CExp) -> CExp -> CExp
          end = struct
exception InvalidCode of string
type Var = TypedSyntax.VId
structure CVar :> sig
              eqtype t
              type ord_key = t
              val compare : ord_key * ord_key -> order
              val fromInt : int -> t
              val toInt : t -> int
              val dummy : t
          end = struct
type t = int
type ord_key = t
val compare = Int.compare
fun fromInt x = x
fun toInt x = x
val dummy = ~1
end
type CVar = CVar.t
structure CVarSet = RedBlackSetFn (CVar)
structure CVarMap = RedBlackMapFn (CVar)
structure CVarTable = HashTableFn (struct
                                   type hash_key = CVar.t
                                   fun hashVal x = Word.fromInt (CVar.toInt x)
                                   fun sameKey (x : CVar.t, y : CVar.t) = x = y
                                   end)
datatype Value = Var of Var
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
type AbsAttr = { isWrapper : bool }
datatype SimpleExp = PrimOp of { primOp : FSyntax.PrimOp, tyargs : FSyntax.Ty list, args : Value list }
                   | Record of Value Syntax.LabelMap.map (* non-empty record *)
                   | ExnTag of { name : string, payloadTy : FSyntax.Ty option }
                   | Projection of { label : Syntax.Label, record : Value, fieldTypes : FSyntax.Ty Syntax.LabelMap.map }
                   | Abs of { contParam : CVar, params : Var list, body : CExp, attr : AbsAttr } (* non-recursive function *)
     and Dec = ValDec of { exp : SimpleExp, result : Var option }
             | RecDec of { name : Var, contParam : CVar, params : Var list, body : CExp, attr : AbsAttr} list (* recursive function *)
             | ContDec of { name : CVar, params : (Var option) list, body : CExp }
             | RecContDec of (CVar * (Var option) list * CExp) list
             | ESImportDec of { pure : bool, specs : (Syntax.ESImportName * Var) list, moduleName : string }
     and CExp = Let of { decs : Dec vector, cont : CExp }
              | App of { applied : Value, cont : CVar, args : Value list } (* tail call *) (* return arity? *)
              | AppCont of { applied : CVar, args : Value list }
              | If of { cond : Value
                      , thenCont : CExp
                      , elseCont : CExp
                      }
              | Handle of { body : CExp, handler : Var * CExp, successfulExitIn : CVar, successfulExitOut : CVar }
              | Unreachable
local structure F = FSyntax in
fun isDiscardable (PrimOp { primOp = F.IntConstOp _, ... }) = true
  | isDiscardable (PrimOp { primOp = F.WordConstOp _, ... }) = true
  | isDiscardable (PrimOp { primOp = F.RealConstOp _, ... }) = true
  | isDiscardable (PrimOp { primOp = F.Char8ConstOp _, ... }) = true
  | isDiscardable (PrimOp { primOp = F.Char16ConstOp _, ... }) = true
  | isDiscardable (PrimOp { primOp = F.String8ConstOp _, ... }) = true
  | isDiscardable (PrimOp { primOp = F.String16ConstOp _, ... }) = true
  | isDiscardable (PrimOp { primOp = F.RaiseOp _, ... }) = false
  | isDiscardable (PrimOp { primOp = F.ListOp, ... }) = true
  | isDiscardable (PrimOp { primOp = F.VectorOp, ... }) = true
  | isDiscardable (PrimOp { primOp = F.DataTagAsStringOp _, ... }) = true
  | isDiscardable (PrimOp { primOp = F.DataTagAsString16Op _, ... }) = true
  | isDiscardable (PrimOp { primOp = F.DataPayloadOp _, ... }) = true
  | isDiscardable (PrimOp { primOp = F.ExnPayloadOp, ... }) = true
  | isDiscardable (PrimOp { primOp = F.ConstructValOp _, ... }) = true
  | isDiscardable (PrimOp { primOp = F.ConstructValWithPayloadOp _, ... }) = true
  | isDiscardable (PrimOp { primOp = F.ConstructExnOp, ... }) = true
  | isDiscardable (PrimOp { primOp = F.ConstructExnWithPayloadOp, ... }) = true
  | isDiscardable (PrimOp { primOp = F.PrimCall p, ... }) = Primitives.isDiscardable p
  | isDiscardable (PrimOp { primOp = F.JsCallOp, ... }) = false
  | isDiscardable (PrimOp { primOp = F.JsMethodOp, ... }) = false
  | isDiscardable (PrimOp { primOp = F.JsNewOp, ... }) = false
  | isDiscardable (PrimOp { primOp = F.LuaCallOp, ... }) = false
  | isDiscardable (PrimOp { primOp = F.LuaCall1Op, ... }) = false
  | isDiscardable (PrimOp { primOp = F.LuaCall2Op, ... }) = false
  | isDiscardable (PrimOp { primOp = F.LuaCall3Op, ... }) = false
  | isDiscardable (PrimOp { primOp = F.LuaMethodOp _, ... }) = false
  | isDiscardable (PrimOp { primOp = F.LuaMethod1Op _, ... }) = false
  | isDiscardable (PrimOp { primOp = F.LuaMethod2Op _, ... }) = false
  | isDiscardable (PrimOp { primOp = F.LuaMethod3Op _, ... }) = false
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
  | containsAppDec (ContDec { name = _, params = _, body }) = containsApp body
  | containsAppDec (RecContDec defs) = List.exists (fn (_, _, body) => containsApp body) defs
  | containsAppDec (ESImportDec _) = false
and containsApp (Let { decs, cont }) = containsApp cont orelse Vector.exists containsAppDec decs
  | containsApp (App _) = true
  | containsApp (AppCont _) = false
  | containsApp (If { cond = _, thenCont, elseCont }) = containsApp thenCont orelse containsApp elseCont
  | containsApp (Handle { body, handler = (_, h), ... }) = containsApp body orelse containsApp h
  | containsApp Unreachable = false

fun freeVarsInValue bound (v, acc)
    = case v of
          Var var => if TypedSyntax.VIdSet.member (bound, var) then
                         acc
                     else
                         TypedSyntax.VIdSet.add (acc, var)
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
fun freeVarsInSimpleExp (bound, PrimOp { primOp = _, tyargs = _, args }, acc) = List.foldl (freeVarsInValue bound) acc args
  | freeVarsInSimpleExp (bound, Record fields, acc) = Syntax.LabelMap.foldl (freeVarsInValue bound) acc fields
  | freeVarsInSimpleExp (_, ExnTag { name = _, payloadTy = _ }, acc) = acc
  | freeVarsInSimpleExp (bound, Projection { label = _, record, fieldTypes = _ }, acc) = freeVarsInValue bound (record, acc)
  | freeVarsInSimpleExp (bound, Abs { contParam = _, params, body, attr = _ }, acc)
    = let val bound = List.foldl TypedSyntax.VIdSet.add' bound params
      in freeVarsInExp (bound, body, acc)
      end
and freeVarsInDec (ValDec { exp, result }, (bound, acc))
    = let val acc = freeVarsInSimpleExp (bound, exp, acc)
      in case result of
             SOME r => (TypedSyntax.VIdSet.add (bound, r), acc)
           | NONE => (bound, acc)
      end
  | freeVarsInDec (RecDec defs, (bound, acc))
    = let val bound = List.foldl (fn ({ name, ... }, bound) => TypedSyntax.VIdSet.add (bound, name)) bound defs
          val acc = List.foldl (fn ({ params, body, ... }, acc) =>
                                   let val bound = List.foldl TypedSyntax.VIdSet.add' bound params
                                   in freeVarsInExp (bound, body, acc)
                                   end
                               ) acc defs
      in (bound, acc)
      end
  | freeVarsInDec (ContDec { name = _, params, body }, (bound, acc))
    = let val bound = List.foldl VIdSet_addOpt bound params
      in (bound, freeVarsInExp (bound, body, acc))
      end
  | freeVarsInDec (RecContDec defs, (bound, acc))
    = let val acc = List.foldl (fn ((_, params, body), acc) =>
                                   let val bound = List.foldl VIdSet_addOpt bound params
                                   in freeVarsInExp (bound, body, acc)
                                   end
                               ) acc defs
      in (bound, acc)
      end
  | freeVarsInDec (ESImportDec { pure = _, specs, moduleName = _ }, (bound, acc)) = (List.foldl (fn ((_, v), bound) => TypedSyntax.VIdSet.add (bound, v)) bound specs, acc)
and freeVarsInExp (bound, Let { decs, cont }, acc) = let val (bound, acc) = Vector.foldl freeVarsInDec (bound, acc) decs
                                                     in freeVarsInExp (bound, cont, acc)
                                                     end
  | freeVarsInExp (bound, App { applied, cont = _, args }, acc) = List.foldl (freeVarsInValue bound) (freeVarsInValue bound (applied, acc)) args
  | freeVarsInExp (bound, AppCont { applied = _, args }, acc) = List.foldl (freeVarsInValue bound) acc args
  | freeVarsInExp (bound, If { cond, thenCont, elseCont }, acc) = freeVarsInExp (bound, elseCont, freeVarsInExp (bound, thenCont, freeVarsInValue bound (cond, acc)))
  | freeVarsInExp (bound, Handle { body, handler = (e, h), successfulExitIn = _, successfulExitOut = _ }, acc) = freeVarsInExp (bound, body, freeVarsInExp (TypedSyntax.VIdSet.add (bound, e), h, acc))
  | freeVarsInExp (_, Unreachable, acc) = acc

fun recurseCExp f
    = let fun goSimpleExp (e as PrimOp _) = e
            | goSimpleExp (e as Record _) = e
            | goSimpleExp (e as ExnTag _) = e
            | goSimpleExp (e as Projection _) = e
            | goSimpleExp (Abs { contParam, params, body, attr }) = Abs { contParam = contParam, params = params, body = goExp body, attr = attr }
          and goDec (ValDec { exp, result }) = ValDec { exp = goSimpleExp exp, result = result }
            | goDec (RecDec defs) = RecDec (List.map (fn { name, contParam, params, body, attr } => { name = name, contParam = contParam, params = params, body = goExp body, attr = attr }) defs)
            | goDec (ContDec { name, params, body }) = ContDec { name = name, params = params, body = goExp body }
            | goDec (RecContDec defs) = RecContDec (List.map (fn (name, params, body) => (name, params, goExp body)) defs)
            | goDec (dec as ESImportDec _) = dec
          and goExp e = f (case e of
                               Let { decs, cont } => Let { decs = Vector.map goDec decs, cont = goExp cont }
                             | App _ => e
                             | AppCont _ => e
                             | If { cond, thenCont, elseCont } => If { cond = cond, thenCont = goExp thenCont, elseCont = goExp elseCont }
                             | Handle { body, handler = (k, h), successfulExitIn, successfulExitOut } => Handle { body = goExp body, handler = (k, goExp h), successfulExitIn = successfulExitIn, successfulExitOut = successfulExitOut }
                             | Unreachable => e
                          )
      in goExp
      end
end
end

structure CpsTransform :> sig
              type Context = { targetInfo : TargetInfo.target_info
                             , nextVId : int ref
                             , exportAsRecord : bool
                             }
              val initialEnv : CSyntax.Value TypedSyntax.VIdMap.map
              val prependRevDecs : CSyntax.Dec list * CSyntax.CExp -> CSyntax.CExp
              val transformT : Context * CSyntax.Value TypedSyntax.VIdMap.map -> FSyntax.Exp -> CSyntax.Dec list * CSyntax.CVar -> CSyntax.CExp
          end = struct
local structure F = FSyntax
      structure C = CSyntax
      val foldlCont = ListUtil.foldlCont
in

type Context = { targetInfo : TargetInfo.target_info
               , nextVId : int ref
               , exportAsRecord : bool
               }

fun genContSym (ctx : Context) : CSyntax.CVar
    = let val n = !(#nextVId ctx)
          val _ = #nextVId ctx := n + 1
      in CSyntax.CVar.fromInt n
      end

fun genSym (ctx : Context) = let val n = !(#nextVId ctx)
                                 val _ = #nextVId ctx := n + 1
                             in TypedSyntax.MkVId ("tmp", n)
                             end

fun stripTyAbs (F.TyAbsExp (_, _, e)) = stripTyAbs e
  | stripTyAbs e = e

(* 'a -> 'b ~~> (cont : 'b -> 'ans, param : 'a) -> 'ans *)
(* continuation of 'a : (value : 'a) -> 'ans *)

datatype cont = REIFIED of C.CVar
              | META of C.Var option * (C.Dec list * C.Value -> C.CExp)
fun prependRevDecs ([], cont) = cont
  | prependRevDecs (revDecs, C.Let { decs, cont }) = C.Let { decs = Vector.fromList (List.revAppend (revDecs, Vector.foldr (op ::) [] decs)), cont = cont }
  | prependRevDecs (revDecs, cont) = C.Let { decs = Vector.fromList (List.rev revDecs), cont = cont }
fun reify (_, revDecs, REIFIED k) f = prependRevDecs (revDecs, f k)
  | reify (ctx, revDecs, META (hint, m)) f = let val k = genContSym ctx
                                                 val x = case hint of
                                                             NONE => genSym ctx
                                                           | SOME x => x
                                             in prependRevDecs ( C.ContDec { name = k
                                                                           , params = [SOME x]
                                                                           , body = m ([], C.Var x)
                                                                           } :: revDecs
                                                               , f k
                                                               )
                                             end
fun apply revDecs (REIFIED k) arg = prependRevDecs (revDecs, C.AppCont { applied = k, args = [arg] })
  | apply revDecs (META (_, m)) arg = m (revDecs, arg)
fun getResultHint (META (hint, _)) = hint
  | getResultHint (REIFIED _) = NONE
val initialEnv = List.foldl TypedSyntax.VIdMap.insert' TypedSyntax.VIdMap.empty
                            [(InitialEnv.VId_false, C.BoolConst false)
                            ,(InitialEnv.VId_true, C.BoolConst true)
                            ,(InitialEnv.VId_nil, C.Nil)
                            ]
(*:
val transform : Context * C.Value TypedSyntax.VIdMap.map -> F.Exp -> { revDecs : C.Dec list, resultHint : C.Var option } -> (C.Dec list * C.Value -> C.CExp) -> C.CExp
and transformT : Context * C.Value TypedSyntax.VIdMap.map -> F.Exp -> C.Dec list * C.CVar -> C.CExp
and transformX : Context * C.Value TypedSyntax.VIdMap.map -> F.Exp -> C.Dec list * cont -> C.CExp
 *)
fun transform (ctx, env) exp { revDecs, resultHint } k = transformX (ctx, env) exp (revDecs, META (resultHint, k))
and transformT (ctx, env) exp (revDecs, k) = transformX (ctx, env) exp (revDecs, REIFIED k)
and transformX (ctx : Context, env) (exp : F.Exp) (revDecs : C.Dec list, k : cont) : C.CExp
    = case exp of
           F.PrimExp (F.PrimCall Primitives.DelimCont_pushPrompt, _, [p (* 'a prompt_tag *), f (* unit -> 'a *)]) =>
           transform (ctx, env) p { revDecs = revDecs, resultHint = NONE }
                     (fn (revDecs, p) =>
                         transform (ctx, env) f { revDecs = revDecs, resultHint = NONE }
                                   (fn (revDecs, f) =>
                                       reify (ctx, revDecs, k)
                                             (fn kk =>
                                                 C.App { applied = C.Var InitialEnv.VId_DelimCont_pushPrompt, cont = kk, args = [p, f] }
                                             )
                                   )
                     )
         | F.PrimExp (F.PrimCall Primitives.DelimCont_withSubCont, _, [p (* 'b prompt_tag *), f (* ('a,'b) subcont -> 'b *)]) =>
           transform (ctx, env) p { revDecs = revDecs, resultHint = NONE }
                     (fn (revDecs, p) =>
                         transform (ctx, env) f { revDecs = revDecs, resultHint = NONE }
                                   (fn (revDecs, f) =>
                                       reify (ctx, revDecs, k)
                                             (fn kk =>
                                                 C.App { applied = C.Var InitialEnv.VId_DelimCont_withSubCont, cont = kk, args = [p, f] }
                                             )
                                   )
                     )
         | F.PrimExp (F.PrimCall Primitives.DelimCont_pushSubCont, _, [subcont (* ('a,'b) subcont *), f (* unit -> 'a *)]) =>
           transform (ctx, env) subcont { revDecs = revDecs, resultHint = NONE }
                     (fn (revDecs, subcont) =>
                         transform (ctx, env) f { revDecs = revDecs, resultHint = NONE }
                                   (fn (revDecs, f) =>
                                       reify (ctx, revDecs, k)
                                             (fn kk =>
                                                 C.App { applied = C.Var InitialEnv.VId_DelimCont_pushSubCont, cont = kk, args = [subcont, f] }
                                             )
                                   )
                     )
         | F.PrimExp (F.PrimCall Primitives.Unsafe_cast, _, [arg]) =>
           transformX (ctx, env) arg (revDecs, k)
         | F.PrimExp (F.PrimCall Primitives.unreachable, _, _) =>
           C.Unreachable
         | F.PrimExp (primOp, tyargs, args) =>
           foldlCont (fn (e, (revDecs, acc), cont) => transform (ctx, env) e { revDecs = revDecs, resultHint = NONE } (fn (revDecs, v) => cont (revDecs, v :: acc)))
                     (revDecs, [])
                     args
                     (fn (revDecs, revArgs) =>
                         case primOp of
                             F.IntConstOp x => (case tyargs of
                                                    [F.TyVar tv] => if TypedSyntax.eqUTyVar (tv, F.tyNameToTyVar Typing.primTyName_int) then
                                                                        apply revDecs k (C.IntConst (#defaultInt (#targetInfo ctx), x))
                                                                    else if TypedSyntax.eqUTyVar (tv, F.tyNameToTyVar Typing.primTyName_int32) then
                                                                        apply revDecs k (C.IntConst (Primitives.I32, x))
                                                                    else if TypedSyntax.eqUTyVar (tv, F.tyNameToTyVar Typing.primTyName_int54) then
                                                                        apply revDecs k (C.IntConst (Primitives.I54, x))
                                                                    else if TypedSyntax.eqUTyVar (tv, F.tyNameToTyVar Typing.primTyName_int64) then
                                                                        apply revDecs k (C.IntConst (Primitives.I64, x))
                                                                    else if TypedSyntax.eqUTyVar (tv, F.tyNameToTyVar Typing.primTyName_intInf) then
                                                                        apply revDecs k (C.IntConst (Primitives.INT_INF, x))
                                                                    else
                                                                        raise Fail "IntConstOp: invalid type"
                                                  | _ => raise Fail "IntConstOp: invalid type"
                                               )
                           | F.WordConstOp x => (case tyargs of
                                                     [F.TyVar tv] => if TypedSyntax.eqUTyVar (tv, F.tyNameToTyVar Typing.primTyName_word) then
                                                                         apply revDecs k (C.WordConst (#defaultWord (#targetInfo ctx), x))
                                                                     else if TypedSyntax.eqUTyVar (tv, F.tyNameToTyVar Typing.primTyName_word32) then
                                                                         apply revDecs k (C.WordConst (Primitives.W32, x))
                                                                     else if TypedSyntax.eqUTyVar (tv, F.tyNameToTyVar Typing.primTyName_word64) then
                                                                         apply revDecs k (C.WordConst (Primitives.W64, x))
                                                                     else
                                                                         raise Fail "WordConstOp: invalid type"
                                                   | _ => raise Fail "WordConstOp: invalid type"
                                                )
                           | F.Char8ConstOp x => apply revDecs k (C.CharConst x) (* assume the type is correct *)
                           | F.Char16ConstOp x => apply revDecs k (C.Char16Const x) (* assume the type is correct *)
                           | F.String8ConstOp x => apply revDecs k (C.StringConst x) (* assume the type is correct *)
                           | F.String16ConstOp x => apply revDecs k (C.String16Const x) (* assume the type is correct *)
                           | _ => let val returnsUnit = case primOp of
                                                            F.PrimCall Primitives.Ref_set => true
                                                          | F.PrimCall (Primitives.Unsafe_Array_update _) => true
                                                          | F.PrimCall Primitives.Lua_set => true
                                                          | F.PrimCall Primitives.Lua_setGlobal => true
                                                          | F.PrimCall Primitives.JavaScript_set => true
                                                          | F.PrimCall Primitives.JavaScript_setGlobal => true
                                                          | _ => false
                                      val args = List.rev revArgs
                                      val primOp = case primOp of
                                                       F.PrimCall p => F.PrimCall (Primitives.fixIntWord { int = #defaultInt (#targetInfo ctx), word = #defaultWord (#targetInfo ctx) } p)
                                                     | _ => primOp
                                      val exp = C.PrimOp { primOp = primOp, tyargs = tyargs, args = args }
                                  in if returnsUnit then
                                         apply (C.ValDec { exp = exp, result = NONE } :: revDecs) k C.Unit
                                     else
                                         let val result = case getResultHint k of
                                                              SOME r => r
                                                            | NONE => genSym ctx
                                         in apply (C.ValDec { exp = exp, result = SOME result } :: revDecs) k (C.Var result)
                                         end
                                  end
                     )
         | F.VarExp vid => (case TypedSyntax.VIdMap.find (env, vid) of
                                SOME v => apply revDecs k v
                              | NONE => apply revDecs k (C.Var vid)
                           )
         | F.RecordExp [] => apply revDecs k C.Unit
         | F.RecordExp fields =>
           foldlCont (fn ((label, exp), (revDecs, acc), cont) => transform (ctx, env) exp { revDecs = revDecs, resultHint = NONE } (fn (revDecs, v) => cont (revDecs, (label, v) :: acc)))
                     (revDecs, [])
                     fields
                     (fn (revDecs, revFields) =>
                         let val result = case getResultHint k of
                                              SOME r => r
                                            | NONE => genSym ctx
                         in apply (C.ValDec { exp = C.Record (List.foldr Syntax.LabelMap.insert' Syntax.LabelMap.empty revFields)
                                            , result = SOME result
                                            }
                                   :: revDecs
                                  ) k (C.Var result)
                         end
                     )
         | F.LetExp (decs, finalExp) =>
           let fun doDecs (env, [], revDecs) = transformX (ctx, env) finalExp (revDecs, k)
                 | doDecs (env, F.ValDec (vid, _, exp) :: decs, revDecs)
                   = transform (ctx, env) exp { revDecs = revDecs, resultHint = SOME vid }
                               (fn (revDecs, v) => doDecs (TypedSyntax.VIdMap.insert (env, vid, v), decs, revDecs))
                 | doDecs (env, F.RecValDec decs' :: decs, revDecs)
                   = let val dec = C.RecDec (List.map (fn (vid, _, exp) =>
                                                          let val contParam = genContSym ctx
                                                          in case stripTyAbs exp of
                                                                 F.FnExp (param, _, body) => { name = vid, contParam = contParam, params = [param], body = transformT (ctx, env) body ([], contParam), attr = { isWrapper = false } }
                                                               | _ => raise Fail "RecValDec"
                                                          end
                                                      ) decs'
                                            )
                     in doDecs (env, decs, dec :: revDecs)
                     end
                 | doDecs (env, F.UnpackDec (_, _, vid, _, exp) :: decs, revDecs)
                   = transform (ctx, env) exp { revDecs = revDecs, resultHint = SOME vid }
                               (fn (revDecs, v) => doDecs (TypedSyntax.VIdMap.insert (env, vid, v), decs, revDecs))
                 | doDecs (env, F.IgnoreDec exp :: decs, revDecs)
                   = transform (ctx, env) exp { revDecs = revDecs, resultHint = NONE }
                               (fn (revDecs, _) => doDecs (env, decs, revDecs))
                 | doDecs (env, F.DatatypeDec _ :: decs, revDecs) = doDecs (env, decs, revDecs)
                 | doDecs (env, F.ExceptionDec { name, tagName, payloadTy } :: decs, revDecs)
                   = let val dec = C.ValDec { exp = C.ExnTag { name = name
                                                             , payloadTy = payloadTy
                                                             }
                                            , result = SOME tagName
                                            }
                     in doDecs (env, decs, dec :: revDecs)
                     end
                 | doDecs (env, F.ESImportDec { pure, specs, moduleName } :: decs, revDecs) = let val dec = C.ESImportDec { pure = pure, specs = List.map (fn (name, vid, _) => (name, vid)) specs, moduleName = moduleName }
                                                                                              in doDecs (env, decs, dec :: revDecs)
                                                                                              end
           in doDecs (env, decs, revDecs)
           end
         | F.AppExp (applied, arg) =>
           transform (ctx, env) applied { revDecs = revDecs, resultHint = NONE }
                     (fn (revDecs, f) =>
                         transform (ctx, env) arg { revDecs = revDecs, resultHint = NONE }
                                   (fn (revDecs, v) =>
                                       reify (ctx, revDecs, k)
                                             (fn j =>
                                                 C.App { applied = f, cont = j, args = [v] }
                                             )
                                   )
                     )
         | F.HandleExp { body, exnName, handler } =>
           reify (ctx, revDecs, k)
                 (fn j => let val success = genContSym ctx
                          in C.Handle { body = transformT (ctx, env) body ([], success)
                                      , handler = (exnName, transformT (ctx, env) handler ([], j))
                                      , successfulExitIn = success
                                      , successfulExitOut = j
                                      }
                          end
                 )
         | F.IfThenElseExp (e1, e2, e3) =>
           transform (ctx, env) e1 { revDecs = revDecs, resultHint = NONE }
                     (fn (revDecs, e1) =>
                         reify (ctx, revDecs, k)
                               (fn j => C.If { cond = e1
                                             , thenCont = transformT (ctx, env) e2 ([], j)
                                             , elseCont = transformT (ctx, env) e3 ([], j)
                                             }
                               )
                     )
         | F.CaseExp _ => raise Fail "CaseExp: not supported here"
         | F.FnExp (vid, _, body) => let val f = case getResultHint k of
                                                     SOME f => f
                                                   | NONE => genSym ctx
                                         val kk = genContSym ctx
                                         val dec = C.ValDec { exp = C.Abs { contParam = kk
                                                                          , params = [vid]
                                                                          , body = transformT (ctx, env) body ([], kk)
                                                                          , attr = { isWrapper = false }
                                                                          }
                                                            , result = SOME f
                                                            }
                                     in apply (dec :: revDecs) k (C.Var f)
                                     end
         | F.ProjectionExp { label, record, fieldTypes } =>
           transform (ctx, env) record { revDecs = revDecs, resultHint = NONE }
                     (fn (revDecs, record) =>
                         let val x = case getResultHint k of
                                         SOME x => x
                                       | NONE => genSym ctx
                             val dec = C.ValDec { exp = C.Projection { label = label
                                                                     , record = record
                                                                     , fieldTypes = fieldTypes
                                                                     }
                                                , result = SOME x
                                                }
                         in apply (dec :: revDecs) k (C.Var x)
                         end
                     )
         | F.TyAbsExp (_, _, exp) => transformX (ctx, env) exp (revDecs, k)
         | F.TyAppExp (exp, _) => transformX (ctx, env) exp (revDecs, k)
         | F.PackExp { payloadTy = _, exp, packageTy = _ } => transformX (ctx, env) exp (revDecs, k)
         | F.BogusExp _ => raise Message.Abort
         | F.ExitProgram => (case k of
                                 REIFIED k => prependRevDecs (revDecs, C.AppCont { applied = k, args = [] })
                               | META _ => raise Fail "unexpected META"
                            )
         | F.ExportValue exp => (case k of
                                     REIFIED k => transform (ctx, env) exp { revDecs = revDecs, resultHint = NONE }
                                                            (fn (revDecs, v) => prependRevDecs (revDecs, C.AppCont { applied = k, args = [v] }))
                                   | META _ => raise Fail "unexpected META"
                                )
         | F.ExportModule entities => (case k of
                                           REIFIED k => foldlCont (fn ((name, exp), (revDecs, acc), cont) => transform (ctx, env) exp { revDecs = revDecs, resultHint = NONE (* name? *) } (fn (revDecs, v) => cont (revDecs, (name, v) :: acc)))
                                                                  (revDecs, [])
                                                                  (Vector.foldr (op ::) [] entities)
                                                                  (fn (revDecs, items) =>
                                                                      if #exportAsRecord ctx then
                                                                          let val result = genSym ctx (* "export"? *)
                                                                              val dec = C.ValDec { exp = C.Record (List.foldl (fn ((name, v), m) => Syntax.LabelMap.insert (m, Syntax.IdentifierLabel name, v)) Syntax.LabelMap.empty items)
                                                                                                 , result = SOME result
                                                                                                 }
                                                                          in prependRevDecs (dec :: revDecs, C.AppCont { applied = k, args = [C.Var result] })
                                                                          end
                                                                      else
                                                                          prependRevDecs (revDecs, C.AppCont { applied = k, args = List.foldl (fn ((_, v), acc) => v :: acc) [] items })
                                                                  )
                                         | META _ => raise Fail "unexpected META"
                                      )
end
end;

structure CpsDeadCodeAnalysis :> sig
              type usage
              val emptyUsage : usage
              val analyze : CSyntax.CExp -> usage
              val isUsed : usage * TypedSyntax.VId -> bool
          end = struct
local structure C = CSyntax
in
type graph = TypedSyntax.VIdSet.set TypedSyntax.VIdTable.hash_table
type usage = bool TypedSyntax.VIdTable.hash_table
val emptyUsage = TypedSyntax.VIdTable.mkTable (0, Fail "")
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
fun goSimpleExp (_, C.PrimOp { primOp = _, tyargs = _, args }) = List.foldl addValue TypedSyntax.VIdSet.empty args
  | goSimpleExp (_, C.Record fields) = Syntax.LabelMap.foldl addValue TypedSyntax.VIdSet.empty fields
  | goSimpleExp (_, C.ExnTag { name = _, payloadTy = _ }) = TypedSyntax.VIdSet.empty
  | goSimpleExp (_, C.Projection { label = _, record, fieldTypes = _ }) = addValue (record, TypedSyntax.VIdSet.empty)
  | goSimpleExp (g, C.Abs { contParam = _, params = _, body, attr = _ }) = goExp (g, body, TypedSyntax.VIdSet.empty) (* What to do with params? *)
and goDec g (C.ValDec { exp, result }, acc) = let val s = goSimpleExp (g, exp)
                                              in case result of
                                                     SOME r => TypedSyntax.VIdTable.insert g (r, s)
                                                   | NONE => ()
                                               ; if C.isDiscardable exp then
                                                     acc
                                                 else
                                                     TypedSyntax.VIdSet.union (acc, s)
                                              end
  | goDec g (C.RecDec defs, acc) = let val s = List.foldl (fn ({ body, ... }, acc) => goExp (g, body, acc)) acc defs
                                   in List.app (fn { name, ... } => TypedSyntax.VIdTable.insert g (name, s)) defs
                                    ; acc
                                   end
  | goDec g (C.ContDec { name = _, params = _, body }, acc) = goExp (g, body, acc)
  | goDec g (C.RecContDec defs, acc) = List.foldl (fn ((_, _, body), acc) => goExp (g, body, acc)) acc defs
  | goDec g (C.ESImportDec { pure = _, specs, moduleName = _ }, acc) = ( List.app (fn (_, vid) => TypedSyntax.VIdTable.insert g (vid, TypedSyntax.VIdSet.empty)) specs
                                                                       ; acc
                                                                       )
and goExp (g, C.Let { decs, cont }, acc) = goExp (g, cont, Vector.foldl (goDec g) acc decs)
  | goExp (_, C.App { applied, cont = _, args }, acc) = List.foldl addValue (addValue (applied, acc)) args
  | goExp (_, C.AppCont { applied = _, args }, acc) = List.foldl addValue acc args
  | goExp (g, C.If { cond, thenCont, elseCont }, acc) = goExp (g, elseCont, goExp (g, thenCont, addValue (cond, acc)))
  | goExp (g, C.Handle { body, handler = (_, h), successfulExitIn = _, successfulExitOut = _ }, acc) = goExp (g, body, goExp (g, h, acc))
  | goExp (_, C.Unreachable, acc) = acc
(*: val makeGraph : CSyntax.CExp -> graph * TypedSyntax.VIdSet.set *)
fun makeGraph program = let val g = TypedSyntax.VIdTable.mkTable (1, Fail "dead code analysis table lookup failed")
                        in (g, goExp (g, program, TypedSyntax.VIdSet.empty))
                        end
fun analyze program = let val (g, root) = makeGraph program
                          val usage = TypedSyntax.VIdTable.mkTable (1, Fail "dead code analysis table lookup failed")
                          fun go vid = case TypedSyntax.VIdTable.find usage vid of
                                           SOME true => ()
                                         | _ => ( TypedSyntax.VIdTable.insert usage (vid, true)
                                                ; case TypedSyntax.VIdTable.find g vid of
                                                      SOME set => TypedSyntax.VIdSet.app go set
                                                    | NONE => ()
                                                )
                      in TypedSyntax.VIdSet.app go root
                       ; usage
                      end
fun isUsed (usage, vid) = case TypedSyntax.VIdTable.find usage vid of
                              SOME true => true
                            | _ => false
end (* local *)
end (* structure CpsDeadCodeAnalysis *)

structure CpsUsageAnalysis :> sig
              datatype frequency = NEVER | ONCE | MANY
              type usage = { call : frequency
                           , project : frequency
                           , ref_read : frequency
                           , ref_write : frequency
                           , other : frequency
                           , returnConts : CSyntax.CVarSet.set
                           , labels : (string option) Syntax.LabelMap.map
                           }
              type cont_usage = { direct : frequency, indirect : frequency }
              val neverUsed : usage
              val neverUsedCont : cont_usage
              type usage_table
              type cont_usage_table
              val emptyUsageTable : usage_table
              val emptyContUsageTable : cont_usage_table
              val getValueUsage : usage_table * TypedSyntax.VId -> usage
              val getContUsage : cont_usage_table * CSyntax.CVar -> cont_usage
              val analyze : CSyntax.CExp -> { usage : usage_table
                                            , rec_usage : usage_table
                                            , cont_usage : cont_usage_table
                                            , cont_rec_usage : cont_usage_table
                                            , dead_code_analysis : CpsDeadCodeAnalysis.usage
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
             , ref_read : frequency
             , ref_write : frequency
             , other : frequency
             , returnConts : CSyntax.CVarSet.set
             , labels : (string option) Syntax.LabelMap.map
             }
type cont_usage = { direct : frequency, indirect : frequency }
val neverUsed : usage = { call = NEVER
                        , project = NEVER
                        , ref_read = NEVER
                        , ref_write = NEVER
                        , other = NEVER
                        , returnConts = CSyntax.CVarSet.empty
                        , labels = Syntax.LabelMap.empty
                        }
val neverUsedCont : cont_usage = { direct = NEVER, indirect = NEVER }
type usage_table = (usage ref) TypedSyntax.VIdTable.hash_table
type cont_usage_table = (cont_usage ref) CSyntax.CVarTable.hash_table
val emptyUsageTable = TypedSyntax.VIdTable.mkTable (0, Fail "")
val emptyContUsageTable = CSyntax.CVarTable.mkTable (0, Fail "")
fun getValueUsage (table : usage_table, v)
    = case TypedSyntax.VIdTable.find table v of
          SOME r => !r
        | NONE => { call = MANY, project = MANY, ref_read = MANY, ref_write = MANY, other = MANY, returnConts = CSyntax.CVarSet.empty, labels = Syntax.LabelMap.empty } (* unknown *)
fun getContUsage (table : cont_usage_table, c)
    = case CSyntax.CVarTable.find table c of
          SOME r => !r
        | NONE => { direct = MANY, indirect = MANY } (* unknown *)
fun useValue env (C.Var v) = (case TypedSyntax.VIdTable.find env v of
                                  SOME r => let val { call, project, ref_read, ref_write, other, returnConts, labels } = !r
                                            in r := { call = call, project = project, ref_read = ref_read, ref_write = ref_write, other = oneMore other, returnConts = returnConts, labels = labels }
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
           SOME r => let val { call, project, ref_read, ref_write, other, returnConts, labels } = !r
                     in r := { call = oneMore call, project = project, ref_read = ref_read, ref_write = ref_write, other = other, returnConts = C.CVarSet.add (returnConts, cont), labels = labels }
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
           SOME r => let val { call, project, ref_read, ref_write, other, returnConts, labels } = !r
                         val result' = case result of
                                           SOME (TypedSyntax.MkVId (name, _)) => SOME name
                                         | NONE => NONE
                         fun mergeOption (x as SOME _, _) = x
                           | mergeOption (NONE, y) = y
                     in r := { call = call, project = oneMore project, ref_read = ref_read, ref_write = ref_write, other = other, returnConts = returnConts, labels = Syntax.LabelMap.insertWith mergeOption (labels, label, result') }
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
fun useValueAsRefRead (env, C.Var v)
    = (case TypedSyntax.VIdTable.find env v of
           SOME r => let val { call, project, ref_read, ref_write, other, returnConts, labels } = !r
                     in r := { call = call, project = project, ref_read = oneMore ref_read, ref_write = ref_write, other = other, returnConts = returnConts, labels = labels }
                     end
         | NONE => ()
      )
  | useValueAsRefRead (_, C.Unit) = ()
  | useValueAsRefRead (_, C.Nil) = ()
  | useValueAsRefRead (_, C.BoolConst _) = ()
  | useValueAsRefRead (_, C.IntConst _) = ()
  | useValueAsRefRead (_, C.WordConst _) = ()
  | useValueAsRefRead (_, C.CharConst _) = ()
  | useValueAsRefRead (_, C.Char16Const _) = ()
  | useValueAsRefRead (_, C.StringConst _) = ()
  | useValueAsRefRead (_, C.String16Const _) = ()
fun useValueAsRefWrite (env, C.Var v)
    = (case TypedSyntax.VIdTable.find env v of
           SOME r => let val { call, project, ref_read, ref_write, other, returnConts, labels } = !r
                     in r := { call = call, project = project, ref_read = ref_read, ref_write = oneMore ref_write, other = other, returnConts = returnConts, labels = labels }
                     end
         | NONE => ()
      )
  | useValueAsRefWrite (_, C.Unit) = ()
  | useValueAsRefWrite (_, C.Nil) = ()
  | useValueAsRefWrite (_, C.BoolConst _) = ()
  | useValueAsRefWrite (_, C.IntConst _) = ()
  | useValueAsRefWrite (_, C.WordConst _) = ()
  | useValueAsRefWrite (_, C.CharConst _) = ()
  | useValueAsRefWrite (_, C.Char16Const _) = ()
  | useValueAsRefWrite (_, C.StringConst _) = ()
  | useValueAsRefWrite (_, C.String16Const _) = ()
fun useContVarIndirect cenv (v : C.CVar) = (case C.CVarTable.find cenv v of
                                                SOME r => let val { direct, indirect } = !r
                                                          in r := { direct = direct, indirect = oneMore indirect }
                                                          end
                                              | NONE => ()
                                           )
fun useContVarDirect cenv (v : C.CVar) = (case C.CVarTable.find cenv v of
                                              SOME r => let val { direct, indirect } = !r
                                                        in r := { direct = oneMore direct, indirect = indirect }
                                                        end
                                            | NONE => ()
                                         )
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
fun goSimpleExp (env, _, _, _, _, C.PrimOp { primOp = FSyntax.PrimCall Primitives.Ref_set, tyargs = _, args = [r, v] }) = (useValueAsRefWrite (env, r); useValue env v)
  | goSimpleExp (env, _, _, _, _, C.PrimOp { primOp = FSyntax.PrimCall Primitives.Ref_read, tyargs = _, args = [r] }) = useValueAsRefRead (env, r)
  | goSimpleExp (env, _, _, _, _, C.PrimOp { primOp = _, tyargs = _, args }) = List.app (useValue env) args
  | goSimpleExp (env, _, _, _, _, C.Record fields) = Syntax.LabelMap.app (useValue env) fields
  | goSimpleExp (_, _, _, _, _, C.ExnTag { name = _, payloadTy = _ }) = ()
  | goSimpleExp (env, _, _, _, result, C.Projection { label, record, fieldTypes = _ }) = useValueAsRecord (env, label, result, record)
  | goSimpleExp (env, renv, cenv, crenv, _, C.Abs { contParam, params, body, attr = _ })
    = ( List.app (fn p => add (env, p)) params
      ; addC (cenv, contParam)
      ; goCExp (env, renv, cenv, crenv, body)
      )
and goDec (env, renv, cenv, crenv)
    = fn C.ValDec { exp, result } =>
         ( goSimpleExp (env, renv, cenv, crenv, result, exp)
         ; case result of
               SOME result => add (env, result)
             | NONE => ()
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
          ( Vector.app (goDec (env, renv, cenv, crenv)) decs
          ; goCExp (env, renv, cenv, crenv, cont)
          )
        | C.App { applied, cont, args } =>
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
fun analyze exp = let val dca = CpsDeadCodeAnalysis.analyze exp
                      val usage = TypedSyntax.VIdTable.mkTable (1, Fail "usage table lookup failed")
                      val rusage = TypedSyntax.VIdTable.mkTable (1, Fail "rusage table lookup failed")
                      val cusage = CSyntax.CVarTable.mkTable (1, Fail "cusage table lookup failed")
                      val crusage = CSyntax.CVarTable.mkTable (1, Fail "crusage table lookup failed")
                  in goCExp (usage, rusage, cusage, crusage, exp)
                   ; { usage = usage, rec_usage = rusage, cont_usage = cusage, cont_rec_usage = crusage, dead_code_analysis = dca }
                  end
end (* local *)
end (* strucuture CpsUsageAnalysis *)

structure CpsSimplify :> sig
              type Context = { nextVId : int ref
                             , simplificationOccurred : bool ref
                             }
              type value_info = { exp : CSyntax.SimpleExp option, isDiscardableFunction : bool }
              val genContSym : Context -> CSyntax.CVar
              val newVId : Context * string -> TypedSyntax.VId
              val renewVId : Context * TypedSyntax.VId -> TypedSyntax.VId
              val renewCVar : Context * CSyntax.CVar -> CSyntax.CVar
              val substSimpleExp : CSyntax.Value TypedSyntax.VIdMap.map * CSyntax.CVar CSyntax.CVarMap.map * CSyntax.SimpleExp -> CSyntax.SimpleExp
              val sizeOfCExp : CSyntax.CExp * int -> int
              val substValue : CSyntax.Value TypedSyntax.VIdMap.map -> CSyntax.Value -> CSyntax.Value
              val substCVar : CSyntax.CVar CSyntax.CVarMap.map -> CSyntax.CVar -> CSyntax.CVar
              val substCExp : CSyntax.Value TypedSyntax.VIdMap.map * CSyntax.CVar CSyntax.CVarMap.map * CSyntax.CExp -> CSyntax.CExp
              val alphaConvert : Context * CSyntax.Value TypedSyntax.VIdMap.map * CSyntax.CVar CSyntax.CVarMap.map * CSyntax.CExp -> CSyntax.CExp
              val isDiscardableExp : value_info TypedSyntax.VIdMap.map * CSyntax.CExp -> bool
              val finalizeCExp : Context * CSyntax.CExp -> CSyntax.CExp
          end = struct
local structure F = FSyntax
      structure C = CSyntax
      structure P = Primitives
      datatype frequency = datatype CpsUsageAnalysis.frequency
in
type Context = { nextVId : int ref
               , simplificationOccurred : bool ref
               }
fun genContSym (ctx : Context) : CSyntax.CVar
    = let val n = !(#nextVId ctx)
          val _ = #nextVId ctx := n + 1
      in CSyntax.CVar.fromInt n
      end
fun newVId ({ nextVId, ... } : Context, name)
    = let val n = !nextVId
      in TypedSyntax.MkVId (name, n) before (nextVId := n + 1)
      end
fun renewVId ({ nextVId, ... } : Context, TypedSyntax.MkVId (name, _))
    = let val n = !nextVId
      in TypedSyntax.MkVId (name, n) before (nextVId := n + 1)
      end
fun renewCVar ({ nextVId, ... } : Context, _ : C.CVar)
    = let val n = !nextVId
      in C.CVar.fromInt n before (nextVId := n + 1)
      end
fun sizeOfSimpleExp (e, threshold)
    = if threshold < 0 then
          threshold
      else
          case e of
              C.PrimOp { primOp = _, tyargs = _, args } => threshold - List.length args
            | C.Record fields => threshold - Syntax.LabelMap.numItems fields
            | C.ExnTag _ => threshold - 1
            | C.Projection _ => threshold - 1
            | C.Abs { contParam = _, params = _, body, attr = _ } => sizeOfCExp (body, threshold)
and sizeOfDec (dec, threshold)
    = if threshold < 0 then
          threshold
      else
          case dec of
              C.ValDec { exp, result = _ } => sizeOfSimpleExp (exp, threshold)
            | C.RecDec defs => List.foldl (fn ({ body, ... }, t) => sizeOfCExp (body, t)) threshold defs
            | C.ContDec { name = _, params = _, body } => sizeOfCExp (body, threshold)
            | C.RecContDec defs => List.foldl (fn ((_, _, body), t) => sizeOfCExp (body, t)) threshold defs
            | C.ESImportDec _ => 0
and sizeOfCExp (e, threshold)
    = if threshold < 0 then
          threshold
      else
          case e of
              C.Let { decs, cont } => Vector.foldl sizeOfDec (sizeOfCExp (cont, threshold)) decs
            | C.App { applied = _, cont = _, args } => threshold - List.length args
            | C.AppCont { applied = _, args } => threshold - List.length args
            | C.If { cond = _, thenCont, elseCont } => sizeOfCExp (elseCont, sizeOfCExp (thenCont, threshold - 1))
            | C.Handle { body, handler = (_, h), successfulExitIn = _, successfulExitOut = _ } => sizeOfCExp (body, sizeOfCExp (h, threshold - 1))
            | C.Unreachable => threshold
fun substValue (subst : C.Value TypedSyntax.VIdMap.map) (x as C.Var v) = (case TypedSyntax.VIdMap.find (subst, v) of
                                                                              SOME w => w
                                                                            | NONE => x
                                                                         )
  | substValue _ v = v
fun substCVar (csubst : C.CVar C.CVarMap.map) v = case C.CVarMap.find (csubst, v) of
                                                      SOME w => w
                                                    | NONE => v
fun substSimpleExp (subst, _, C.PrimOp { primOp, tyargs, args }) = C.PrimOp { primOp = primOp, tyargs = tyargs, args = List.map (substValue subst) args }
  | substSimpleExp (subst, _, C.Record fields) = C.Record (Syntax.LabelMap.map (substValue subst) fields)
  | substSimpleExp (_, _, e as C.ExnTag _) = e
  | substSimpleExp (subst, _, C.Projection { label, record, fieldTypes }) = C.Projection { label = label, record = substValue subst record, fieldTypes = fieldTypes }
  | substSimpleExp (subst, csubst, C.Abs { contParam, params, body, attr }) = C.Abs { contParam = contParam, params = params, body = substCExp (subst, csubst, body), attr = attr }
and substDec (subst, csubst) = fn C.ValDec { exp, result } => C.ValDec { exp = substSimpleExp (subst, csubst, exp), result = result }
                                | C.RecDec defs => C.RecDec (List.map (fn { name, contParam, params, body, attr } => { name = name, contParam = contParam, params = params, body = substCExp (subst, csubst, body), attr = attr }) defs)
                                | C.ContDec { name, params, body } => C.ContDec { name = name, params = params, body = substCExp (subst, csubst, body) }
                                | C.RecContDec defs => C.RecContDec (List.map (fn (f, params, body) => (f, params, substCExp (subst, csubst, body))) defs)
                                | dec as C.ESImportDec _ => dec
and substCExp (subst : C.Value TypedSyntax.VIdMap.map, csubst : C.CVar C.CVarMap.map, C.Let { decs, cont }) = C.Let { decs = Vector.map (substDec (subst, csubst)) decs, cont = substCExp (subst, csubst, cont) }
  | substCExp (subst, csubst, C.App { applied, cont, args }) = C.App { applied = substValue subst applied, cont = substCVar csubst cont, args = List.map (substValue subst) args }
  | substCExp (subst, csubst, C.AppCont { applied, args }) = C.AppCont { applied = substCVar csubst applied, args = List.map (substValue subst) args }
  | substCExp (subst, csubst, C.If { cond, thenCont, elseCont }) = C.If { cond = substValue subst cond, thenCont = substCExp (subst, csubst, thenCont), elseCont = substCExp (subst, csubst, elseCont) }
  | substCExp (subst, csubst, C.Handle { body, handler = (e, h), successfulExitIn, successfulExitOut }) = C.Handle { body = substCExp (subst, csubst, body), handler = (e, substCExp (subst, csubst, h)), successfulExitIn = successfulExitIn, successfulExitOut = substCVar csubst successfulExitOut }
  | substCExp (_, _, e as C.Unreachable) = e
val substCExp = fn (subst, csubst, e) => if TypedSyntax.VIdMap.isEmpty subst andalso C.CVarMap.isEmpty csubst then
                                             e
                                         else
                                             substCExp (subst, csubst, e)
fun alphaConvertSimpleExp (ctx, subst, csubst, C.Abs { contParam, params, body, attr })
    = let val (params', subst') = List.foldr (fn (p, (params', subst)) =>
                                                 let val p' = renewVId (ctx, p)
                                                 in (p' :: params', TypedSyntax.VIdMap.insert (subst, p, C.Var p'))
                                                 end
                                             ) ([], subst) params
          val contParam' = renewCVar (ctx, contParam)
          val csubst' = C.CVarMap.insert (csubst, contParam, contParam')
      in C.Abs { contParam = contParam'
               , params = params'
               , body = alphaConvert (ctx, subst', csubst', body)
               , attr = attr
               }
      end
  | alphaConvertSimpleExp (_, subst, csubst, e) = substSimpleExp (subst, csubst, e)
and alphaConvertDec (ctx : Context) (dec, (subst, csubst, acc))
    = case dec of
          C.ValDec { exp, result = SOME result } =>
          let val result' = renewVId (ctx, result)
              val subst' = TypedSyntax.VIdMap.insert (subst, result, C.Var result')
              val dec' = C.ValDec { exp = alphaConvertSimpleExp (ctx, subst, csubst, exp)
                                  , result = SOME result'
                                  }
          in (subst', csubst, dec' :: acc)
          end
        | C.ValDec { exp, result = NONE } =>
          let val dec' = C.ValDec { exp = alphaConvertSimpleExp (ctx, subst, csubst, exp)
                                  , result = NONE
                                  }
          in (subst, csubst, dec' :: acc)
          end
        | C.RecDec defs =>
          let val (subst, nameMap) = List.foldl (fn ({ name, ... }, (subst, nameMap)) =>
                                                    let val name' = renewVId (ctx, name)
                                                    in (TypedSyntax.VIdMap.insert (subst, name, C.Var name'), TypedSyntax.VIdMap.insert (nameMap, name, name'))
                                                    end
                                                ) (subst, TypedSyntax.VIdMap.empty) defs
              val dec' = C.RecDec (List.map (fn { name, contParam, params, body, attr } =>
                                                let val name' = TypedSyntax.VIdMap.lookup (nameMap, name)
                                                    val contParam' = renewCVar (ctx, contParam)
                                                    val (params', subst) = List.foldr (fn (p, (params', subst)) =>
                                                                                          let val p' = renewVId (ctx, p)
                                                                                          in (p' :: params', TypedSyntax.VIdMap.insert (subst, p, C.Var p'))
                                                                                          end
                                                                                      ) ([], subst) params
                                                    val csubst = C.CVarMap.insert (csubst, contParam, contParam')
                                                in { name = name', contParam = contParam', params = params', body = alphaConvert (ctx, subst, csubst, body), attr = attr }
                                                end
                                            ) defs
                                  )
          in (subst, csubst, dec' :: acc)
          end
        | C.ContDec { name, params, body } =>
          let val (params', subst') = List.foldr (fn (SOME p, (params', subst)) =>
                                                     let val p' = renewVId (ctx, p)
                                                     in (SOME p' :: params', TypedSyntax.VIdMap.insert (subst, p, C.Var p'))
                                                     end
                                                 | (NONE, (params', subst)) => (NONE :: params', subst)
                                                 ) ([], subst) params
              val body = alphaConvert (ctx, subst', csubst, body)
              val name' = renewCVar (ctx, name)
              val csubst = C.CVarMap.insert (csubst, name, name')
              val dec' = C.ContDec { name = name'
                                   , params = params'
                                   , body = body
                                   }
          in (subst, csubst, dec' :: acc)
          end
        | C.RecContDec defs =>
          let val (csubst, nameMap) = List.foldl (fn ((f, _, _), (csubst, nameMap)) =>
                                                     let val f' = renewCVar (ctx, f)
                                                     in (C.CVarMap.insert (csubst, f, f'), C.CVarMap.insert (nameMap, f, f'))
                                                     end
                                                 ) (csubst, C.CVarMap.empty) defs
              val dec' = C.RecContDec (List.map (fn (f, params, body) =>
                                                    let val f' = C.CVarMap.lookup (nameMap, f)
                                                        val (params', subst) = List.foldr (fn (SOME p, (params', subst)) =>
                                                                                              let val p' = renewVId (ctx, p)
                                                                                              in (SOME p' :: params', TypedSyntax.VIdMap.insert (subst, p, C.Var p'))
                                                                                              end
                                                                                          | (NONE, (params', subst)) => (params', subst)
                                                                                          ) ([], subst) params
                                                    in (f', params', alphaConvert (ctx, subst, csubst, body))
                                                    end
                                                ) defs
                                      )
          in (subst, csubst, dec' :: acc)
          end
        | C.ESImportDec { pure, specs, moduleName } =>
          let val specs' = List.map (fn (name, vid) => (name, vid, renewVId (ctx, vid))) specs
              val subst' = List.foldl (fn ((_, vid, vid'), subst) => TypedSyntax.VIdMap.insert (subst, vid, C.Var vid')) subst specs'
              val dec' = C.ESImportDec { pure = pure, specs = List.map (fn (name, _, vid) => (name, vid)) specs', moduleName = moduleName }
          in (subst', csubst, dec' :: acc)
          end
and alphaConvert (ctx : Context, subst : C.Value TypedSyntax.VIdMap.map, csubst : C.CVar C.CVarMap.map, C.Let { decs, cont })
    = let val (subst', csubst', revDecs) = Vector.foldl (alphaConvertDec ctx) (subst, csubst, []) decs
      in C.Let { decs = Vector.fromList (List.rev revDecs), cont = alphaConvert (ctx, subst', csubst', cont) }
      end
  | alphaConvert (_, subst, csubst, C.App { applied, cont, args })
    = C.App { applied = substValue subst applied
            , cont = substCVar csubst cont
            , args = List.map (substValue subst) args
            }
  | alphaConvert (_, subst, csubst, C.AppCont { applied, args })
    = C.AppCont { applied = substCVar csubst applied, args = List.map (substValue subst) args }
  | alphaConvert (ctx, subst, csubst, C.If { cond, thenCont, elseCont }) = C.If { cond = substValue subst cond, thenCont = alphaConvert (ctx, subst, csubst, thenCont), elseCont = alphaConvert (ctx, subst, csubst, elseCont) }
  | alphaConvert (ctx, subst, csubst, C.Handle { body, handler = (e, h), successfulExitIn, successfulExitOut })
    = let val successfulExitIn' = renewCVar (ctx, successfulExitIn)
          val csubst' = C.CVarMap.insert (csubst, successfulExitIn, successfulExitIn')
          val e' = renewVId (ctx, e)
          val subst' = TypedSyntax.VIdMap.insert (subst, e, C.Var e')
      in C.Handle { body = alphaConvert (ctx, subst, csubst', body)
                  , handler = (e', alphaConvert (ctx, subst', csubst, h))
                  , successfulExitIn = successfulExitIn'
                  , successfulExitOut = substCVar csubst successfulExitOut
                  }
      end
  | alphaConvert (_, _, _, e as C.Unreachable) = e
datatype simplify_result = VALUE of C.Value
                         | SIMPLE_EXP of C.SimpleExp
                         | NOT_SIMPLIFIED
type value_info = { exp : C.SimpleExp option, isDiscardableFunction : bool }
fun isDiscardableDec (dec, env : value_info TypedSyntax.VIdMap.map)
    = case dec of
          C.ValDec { exp, result } =>
          (case exp of
               C.Abs { body, ... } => let val env = case result of
                                                        SOME result => TypedSyntax.VIdMap.insert (env, result, { exp = NONE, isDiscardableFunction = isDiscardableExp (env, body) })
                                                      | NONE => env
                                      in SOME env
                                      end
             | _ => if C.isDiscardable exp then
                        SOME env
                    else
                        NONE
          )
        | C.RecDec _ => SOME env
        | C.ContDec { name = _, params = _, body } => if isDiscardableExp (env, body) then
                                                          SOME env
                                                      else
                                                          NONE
        | C.RecContDec _ => NONE
        | C.ESImportDec { pure = _, specs = _, moduleName = _ } => SOME env
and isDiscardableExp (env : value_info TypedSyntax.VIdMap.map, C.Let { decs, cont })
    = (case VectorUtil.foldlOption isDiscardableDec env decs of
           SOME env => isDiscardableExp (env, cont)
         | NONE => false
      )
  | isDiscardableExp (env, C.App { applied = C.Var applied, cont = _, args = _ })
    = (case TypedSyntax.VIdMap.find (env, applied) of
           SOME { isDiscardableFunction = true, ... } => true
         | _ => false
      )
  | isDiscardableExp (_, C.App _) = false (* should not occur *)
  | isDiscardableExp (_, C.AppCont _) = true
  | isDiscardableExp (env, C.If { cond = _, thenCont, elseCont }) = isDiscardableExp (env, thenCont) andalso isDiscardableExp (env, elseCont)
  | isDiscardableExp (env, C.Handle { body, handler = (_, h), successfulExitIn = _, successfulExitOut = _ }) = isDiscardableExp (env, body) andalso isDiscardableExp (env, h)
  | isDiscardableExp (_, C.Unreachable) = false
fun prependDecs ([], cont) = cont
  | prependDecs (decs, C.Let { decs = decs', cont }) = C.Let { decs = Vector.fromList (decs @ Vector.foldr (op ::) [] decs'), cont = cont }
  | prependDecs (decs, cont) = C.Let { decs = Vector.fromList decs, cont = cont }
(* Eliminate assumeDiscardable *)
(* More sophisticated analysis is wanted. *)
fun finalizeDec ctx (dec, (decs, cont))
    = case dec of
          C.ValDec { exp = C.PrimOp { primOp = F.PrimCall Primitives.assumeDiscardable, tyargs = _, args = [f, arg] }, result = SOME result } =>
          let val name = genContSym ctx
          in ([C.ContDec { name = name, params = [SOME result], body = prependDecs (decs, cont) }], C.App { applied = f, cont = name, args = [arg] })
          end
        | C.ValDec { exp = C.PrimOp { primOp = F.PrimCall Primitives.assumeDiscardable, tyargs = _, args = _ }, result = _ } =>
          raise Fail "assumeDiscardable: invalid argument"
        | C.ValDec { exp = C.PrimOp _, result = _ } => (dec :: decs, cont)
        | C.ValDec { exp = C.Record _, result = _ } => (dec :: decs, cont)
        | C.ValDec { exp = C.ExnTag _, result = _ } => (dec :: decs, cont)
        | C.ValDec { exp = C.Projection _, result = _ } => (dec :: decs, cont)
        | C.ValDec { exp = C.Abs { contParam, params, body, attr }, result } =>
          let val dec = C.ValDec { exp = C.Abs { contParam = contParam, params = params, body = finalizeCExp (ctx, body), attr = attr }, result = result }
          in (dec :: decs, cont)
          end
        | C.RecDec defs =>
          let val dec = C.RecDec (List.map (fn { name, contParam, params, body, attr } => { name = name, contParam = contParam, params = params, body = finalizeCExp (ctx, body), attr = attr }) defs)
          in (dec :: decs, cont)
          end
        | C.ContDec { name, params, body } =>
          let val dec = C.ContDec { name = name, params = params, body = finalizeCExp (ctx, body) }
          in (dec :: decs, cont)
          end
        | C.RecContDec defs =>
          let val dec = C.RecContDec (List.map (fn (name, params, body) => (name, params, finalizeCExp (ctx, body))) defs)
          in (dec :: decs, cont)
          end
        | C.ESImportDec _ => (dec :: decs, cont)
and finalizeCExp (ctx, C.Let { decs, cont })
    = prependDecs (Vector.foldr (finalizeDec ctx) ([], finalizeCExp (ctx, cont)) decs)
  | finalizeCExp (_, e as C.App _) = e
  | finalizeCExp (_, e as C.AppCont _) = e
  | finalizeCExp (ctx, C.If { cond, thenCont, elseCont }) = C.If { cond = cond, thenCont = finalizeCExp (ctx, thenCont), elseCont = finalizeCExp (ctx, elseCont) }
  | finalizeCExp (ctx, C.Handle { body, handler = (e, h), successfulExitIn, successfulExitOut }) = C.Handle { body = finalizeCExp (ctx, body), handler = (e, finalizeCExp (ctx, h)), successfulExitIn = successfulExitIn, successfulExitOut = successfulExitOut }
  | finalizeCExp (_, e as C.Unreachable) = e
end
end;

structure CpsAnalyze :> sig
              type cont_map
              val escapes : cont_map * CSyntax.CVar -> bool
              val escapesTransitively : cont_map * CSyntax.CVar -> bool
              val contEscape : CSyntax.CVar * CSyntax.CExp -> cont_map
              end = struct
local structure C = CSyntax
in
type cont_map = { escapes : bool, escapesTransitively : bool } CSyntax.CVarTable.hash_table
fun escapes (t : cont_map, v) = #escapes (C.CVarTable.lookup t v)
fun escapesTransitively (t : cont_map, v) = #escapesTransitively (C.CVarTable.lookup t v)
type table = { escapes : bool ref, escapesTransitively : bool ref, level : int, free : CSyntax.CVarSet.set } CSyntax.CVarTable.hash_table
fun direct (table : table, level, k, acc) = let val { escapes = _, escapesTransitively = _, level = level', free = _ } = C.CVarTable.lookup table k
                                            in if level' < level then
                                                   C.CVarSet.add (acc, k)
                                               else
                                                   acc
                                            end
fun recEscape (table : table) k = let val { escapes = _, escapesTransitively, level = _, free } = C.CVarTable.lookup table k
                                  in if !escapesTransitively then
                                         ()
                                     else
                                         ( escapesTransitively := true
                                         ; C.CVarSet.app (recEscape table) free
                                         )
                                  end
fun escape (table : table, level, k, acc) = let val { escapes, escapesTransitively = _, level = level', free = _ } = C.CVarTable.lookup table k
                                            in escapes := true
                                             ; if level' < level then
                                                   C.CVarSet.add (acc, k)
                                               else
                                                   acc
                                            end
fun goDec (table, level) (dec, acc)
    = case dec of
          C.ValDec { exp = C.Abs { contParam, params = _, body, attr = _ }, result = _ } =>
          ( C.CVarTable.insert table (contParam, { escapes = ref false, escapesTransitively = ref false, level = 0, free = C.CVarSet.empty })
          ; go (table, 0, body, acc)
          )
        | C.ValDec { exp = _, result = _ } => acc
        | C.RecDec defs => List.foldl (fn ({ contParam, body, ... }, acc) =>
                                          ( C.CVarTable.insert table (contParam, { escapes = ref false, escapesTransitively = ref false, level = 0, free = C.CVarSet.empty })
                                          ; go (table, 0, body, acc)
                                          )
                                      ) acc defs
        | C.ContDec { name, params = _, body } =>
          let val free = go (table, level + 1, body, C.CVarSet.empty)
          in C.CVarTable.insert table (name, { escapes = ref false, escapesTransitively = ref false, level = level, free = free })
           ; C.CVarSet.union (acc, free)
          end
        | C.RecContDec defs =>
          ( List.app (fn (name, _, _) => C.CVarTable.insert table (name, { escapes = ref false, escapesTransitively = ref false, level = level, free = C.CVarSet.empty })) defs
          ; List.foldl (fn ((name, _, body), acc) =>
                         let val { escapes, escapesTransitively, level, free = _ } = C.CVarTable.lookup table name
                             val free = go (table, level + 1, body, C.CVarSet.empty)
                         in C.CVarTable.insert table (name, { escapes = escapes, escapesTransitively = escapesTransitively, level = level, free = free })
                          ; C.CVarSet.union (acc, free)
                         end
                     ) acc defs
          )
        | C.ESImportDec _ => acc
and go (table, level, C.Let { decs, cont }, acc)
    = go (table, level, cont, Vector.foldl (goDec (table, level)) acc decs)
  | go (table, level, C.App { applied = _, cont, args = _ }, acc) = escape (table, level, cont, acc)
  | go (table, level, C.AppCont { applied, args = _ }, acc) = direct (table, level, applied, acc)
  | go (table, level, C.If { cond = _, thenCont, elseCont }, acc) = go (table, level, elseCont, go (table, level, thenCont, acc))
  | go (table, level, C.Handle { body, handler = (_, h), successfulExitIn, successfulExitOut }, acc)
    = let val free = go (table, level + 1, h, C.CVarSet.empty)
      in C.CVarTable.insert table (successfulExitIn, { escapes = ref false, escapesTransitively = ref false, level = level, free = C.CVarSet.singleton successfulExitOut })
       ; C.CVarSet.app (fn k => ignore (escape (table, level + 1, k, C.CVarSet.empty))) free
       ; go (table, level, body, C.CVarSet.union (acc, free))
      end
  | go (_, _, C.Unreachable, acc) = acc
fun contEscape (cont, cexp) = let val table = C.CVarTable.mkTable (1, C.InvalidCode "unbound continuation")
                              in C.CVarTable.insert table (cont, { escapes = ref false, escapesTransitively = ref false, level = 0, free = C.CVarSet.empty })
                               ; ignore (go (table, 0, cexp, C.CVarSet.empty))
                               ; C.CVarTable.appi (fn (k, { escapes = ref true, escapesTransitively = ref false, ... }) => recEscape table k
                                                  | _ => ()) table
                               ; C.CVarTable.map (fn { escapes, escapesTransitively, ... } => { escapes = !escapes, escapesTransitively = !escapesTransitively }) table
                              end
end (* local *)
end; (* structure CpsAnalyze *)
