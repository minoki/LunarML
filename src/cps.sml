(*
 * Copyright (c) 2023 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure CSyntax = struct
exception InvalidCode of string
type Var = TypedSyntax.VId
type Tag = string
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
datatype SimpleExp = PrimOp of { primOp : FSyntax.PrimOp, tyargs : FSyntax.Ty list, args : Value list }
                   | Record of Value Syntax.LabelMap.map (* non-empty record *)
                   | ExnTag of { name : string, payloadTy : FSyntax.Ty option }
                   | Projection of { label : Syntax.Label, record : Value, fieldTypes : FSyntax.Ty Syntax.LabelMap.map }
                   | Abs of { contParam : CVar, params : Var list, body : CExp } (* non-recursive function *)
     and Dec = ValDec of { exp : SimpleExp, result : Var option }
             | RecDec of (Var * CVar * Var list * CExp) list (* recursive function *)
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
  | isDiscardable (PrimOp { primOp = F.LuaMethodOp _, ... }) = false
  | isDiscardable (Record _) = true
  | isDiscardable (ExnTag _) = true
  | isDiscardable (Projection _) = true
  | isDiscardable (Abs _) = true
fun mayRaise (PrimOp { primOp, ... }) = (case primOp of
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
                                        )
  | mayRaise (Record _) = false
  | mayRaise (ExnTag _) = false
  | mayRaise (Projection _) = false
  | mayRaise (Abs _) = false

fun containsAppDec (ValDec _) = false
  | containsAppDec (RecDec _) = false
  | containsAppDec (ContDec { name, params, body }) = containsApp body
  | containsAppDec (RecContDec defs) = List.exists (fn (_, _, body) => containsApp body) defs
  | containsAppDec (ESImportDec _) = false
and containsApp (Let { decs, cont }) = containsApp cont orelse Vector.exists containsAppDec decs
  | containsApp (App _) = true
  | containsApp (AppCont _) = false
  | containsApp (If { cond, thenCont, elseCont }) = containsApp thenCont orelse containsApp elseCont
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
fun freeVarsInSimpleExp (bound, PrimOp { primOp, tyargs = _, args }, acc) = List.foldl (freeVarsInValue bound) acc args
  | freeVarsInSimpleExp (bound, Record fields, acc) = Syntax.LabelMap.foldl (freeVarsInValue bound) acc fields
  | freeVarsInSimpleExp (bound, ExnTag { name, payloadTy }, acc) = acc
  | freeVarsInSimpleExp (bound, Projection { label, record, fieldTypes }, acc) = freeVarsInValue bound (record, acc)
  | freeVarsInSimpleExp (bound, Abs { contParam, params, body }, acc) = let val bound = List.foldl TypedSyntax.VIdSet.add' bound params
                                                                        in freeVarsInExp (bound, body, acc)
                                                                        end
and freeVarsInDec (ValDec { exp, result }, (bound, acc))
    = let val acc = freeVarsInSimpleExp (bound, exp, acc)
      in case result of
             SOME r => (TypedSyntax.VIdSet.add (bound, r), acc)
           | NONE => (bound, acc)
      end
  | freeVarsInDec (RecDec defs, (bound, acc))
    = let val bound = List.foldl (fn ((name, _, _, _), bound) => TypedSyntax.VIdSet.add (bound, name)) bound defs
          val acc = List.foldl (fn ((_, _, params, body), acc) =>
                                   let val bound = List.foldl TypedSyntax.VIdSet.add' bound params
                                   in freeVarsInExp (bound, body, acc)
                                   end
                               ) acc defs
      in (bound, acc)
      end
  | freeVarsInDec (ContDec { name, params, body }, (bound, acc))
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
  | freeVarsInExp (bound, App { applied, cont, args }, acc) = List.foldl (freeVarsInValue bound) (freeVarsInValue bound (applied, acc)) args
  | freeVarsInExp (bound, AppCont { applied, args }, acc) = List.foldl (freeVarsInValue bound) acc args
  | freeVarsInExp (bound, If { cond, thenCont, elseCont }, acc) = freeVarsInExp (bound, elseCont, freeVarsInExp (bound, thenCont, freeVarsInValue bound (cond, acc)))
  | freeVarsInExp (bound, Handle { body, handler = (e, h), successfulExitIn, successfulExitOut }, acc) = freeVarsInExp (bound, body, freeVarsInExp (TypedSyntax.VIdSet.add (bound, e), h, acc))
  | freeVarsInExp (bound, Unreachable, acc) = acc

fun recurseCExp f
    = let fun goSimpleExp (e as PrimOp _) = e
            | goSimpleExp (e as Record _) = e
            | goSimpleExp (e as ExnTag _) = e
            | goSimpleExp (e as Projection _) = e
            | goSimpleExp (Abs { contParam, params, body }) = Abs { contParam = contParam, params = params, body = goExp body }
          and goDec (ValDec { exp, result }) = ValDec { exp = goSimpleExp exp, result = result }
            | goDec (RecDec defs) = RecDec (List.map (fn (g, k, params, body) => (g, k, params, goExp body)) defs)
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

structure CpsTransform : sig
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
fun reify (ctx, revDecs, REIFIED k) f = prependRevDecs (revDecs, f k)
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
(*!
val transform : Context * C.Value TypedSyntax.VIdMap.map -> F.Exp -> { revDecs : C.Dec list, resultHint : C.Var option } -> (C.Dec list * C.Value -> C.CExp) -> C.CExp
and transformT : Context * C.Value TypedSyntax.VIdMap.map -> F.Exp -> C.Dec list * C.CVar -> C.CExp
and transformX : Context * C.Value TypedSyntax.VIdMap.map -> F.Exp -> C.Dec list * cont -> C.CExp
 *)
fun transform (ctx, env) exp { revDecs, resultHint } k = transformX (ctx, env) exp (revDecs, META (resultHint, k))
and transformT (ctx, env) exp (revDecs, k) = transformX (ctx, env) exp (revDecs, REIFIED k)
and transformX (ctx : Context, env) (exp : F.Exp) (revDecs : C.Dec list, k : cont) : C.CExp
    = case exp of
           F.PrimExp (F.PrimCall Primitives.DelimCont_pushPrompt, tyargs, [p (* 'a prompt_tag *), f (* unit -> 'a *)]) =>
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
         | F.PrimExp (F.PrimCall Primitives.DelimCont_withSubCont, tyargs, [p (* 'b prompt_tag *), f (* ('a,'b) subcont -> 'b *)]) =>
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
         | F.PrimExp (F.PrimCall Primitives.DelimCont_pushSubCont, tyargs, [subcont (* ('a,'b) subcont *), f (* unit -> 'a *)]) =>
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
         | F.PrimExp (F.PrimCall Primitives.Unsafe_cast, tyargs, [arg]) =>
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
                                                                 F.FnExp (param, _, body) => (vid, contParam, [param], transformT (ctx, env) body ([], contParam))
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
         | F.PackExp { payloadTy, exp, packageTy } => transformX (ctx, env) exp (revDecs, k)
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
                                                                          prependRevDecs (revDecs, C.AppCont { applied = k, args = List.foldl (fn ((name, v), acc) => v :: acc) [] items })
                                                                  )
                                         | META _ => raise Fail "unexpected META"
                                      )
end
end;

structure CpsDeadCodeAnalysis :> sig
              type usage
              val analyze : CSyntax.CExp -> usage
              val isUsed : usage * TypedSyntax.VId -> bool
          end = struct
local structure F = FSyntax
      structure C = CSyntax
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
fun goSimpleExp (g, C.PrimOp { primOp, tyargs = _, args }) = List.foldl addValue TypedSyntax.VIdSet.empty args
  | goSimpleExp (g, C.Record fields) = Syntax.LabelMap.foldl addValue TypedSyntax.VIdSet.empty fields
  | goSimpleExp (g, C.ExnTag { name, payloadTy }) = TypedSyntax.VIdSet.empty
  | goSimpleExp (g, C.Projection { label, record, fieldTypes }) = addValue (record, TypedSyntax.VIdSet.empty)
  | goSimpleExp (g, C.Abs { contParam, params, body }) = goExp (g, body, TypedSyntax.VIdSet.empty) (* What to do with params? *)
and goDec g (C.ValDec { exp, result }, acc) = let val s = goSimpleExp (g, exp)
                                              in case result of
                                                     SOME r => TypedSyntax.VIdTable.insert g (r, s)
                                                   | NONE => ()
                                               ; if C.isDiscardable exp then
                                                     acc
                                                 else
                                                     TypedSyntax.VIdSet.union (acc, s)
                                              end
  | goDec g (C.RecDec defs, acc) = let val s = List.foldl (fn ((_, _, _, exp), acc) => goExp (g, exp, acc)) acc defs
                                   in List.app (fn (name, _, _, _) => TypedSyntax.VIdTable.insert g (name, s)) defs
                                    ; acc
                                   end
  | goDec g (C.ContDec { name, params, body }, acc) = goExp (g, body, acc)
  | goDec g (C.RecContDec defs, acc) = List.foldl (fn ((_, _, body), acc) => goExp (g, body, acc)) acc defs
  | goDec g (C.ESImportDec { pure, specs, moduleName }, acc) = ( List.app (fn (_, vid) => TypedSyntax.VIdTable.insert g (vid, TypedSyntax.VIdSet.empty)) specs
                                                               ; acc
                                                               )
and goExp (g, C.Let { decs, cont }, acc) = goExp (g, cont, Vector.foldl (goDec g) acc decs)
  | goExp (g, C.App { applied, cont, args }, acc) = List.foldl addValue (addValue (applied, acc)) args
  | goExp (g, C.AppCont { applied, args }, acc) = List.foldl addValue acc args
  | goExp (g, C.If { cond, thenCont, elseCont }, acc) = goExp (g, elseCont, goExp (g, thenCont, addValue (cond, acc)))
  | goExp (g, C.Handle { body, handler = (e, h), successfulExitIn, successfulExitOut }, acc) = goExp (g, body, goExp (g, h, acc))
  | goExp (g, C.Unreachable, acc) = acc
(*! val makeGraph : CSyntax.CExp -> graph * TypedSyntax.VIdSet.set *)
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
                           , other : frequency
                           , returnConts : CSyntax.CVarSet.set
                           , labels : (string option) Syntax.LabelMap.map
                           }
              type cont_usage = { direct : frequency, indirect : frequency }
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
                                            , dead_code_analysis : CpsDeadCodeAnalysis.usage
                                            }
          end = struct
local structure F = FSyntax
      structure C = CSyntax
in
datatype frequency = NEVER | ONCE | MANY
fun oneMore NEVER = ONCE
  | oneMore ONCE = MANY
  | oneMore (many as MANY) = many
type usage = { call : frequency
             , project : frequency
             , other : frequency
             , returnConts : CSyntax.CVarSet.set
             , labels : (string option) Syntax.LabelMap.map
             }
type cont_usage = { direct : frequency, indirect : frequency }
val neverUsed : usage = { call = NEVER, project = NEVER, other = NEVER, returnConts = CSyntax.CVarSet.empty, labels = Syntax.LabelMap.empty }
val neverUsedCont : cont_usage = { direct = NEVER, indirect = NEVER }
type usage_table = (usage ref) TypedSyntax.VIdTable.hash_table
type cont_usage_table = (cont_usage ref) CSyntax.CVarTable.hash_table
fun getValueUsage (table : usage_table, v)
    = case TypedSyntax.VIdTable.find table v of
          SOME r => !r
        | NONE => { call = MANY, project = MANY, other = MANY, returnConts = CSyntax.CVarSet.empty, labels = Syntax.LabelMap.empty } (* unknown *)
fun getContUsage (table : cont_usage_table, c)
    = case CSyntax.CVarTable.find table c of
          SOME r => !r
        | NONE => { direct = MANY, indirect = MANY } (* unknown *)
fun useValue env (C.Var v) = (case TypedSyntax.VIdTable.find env v of
                                  SOME r => let val { call, project, other, returnConts, labels } = !r
                                            in r := { call = call, project = project, other = oneMore other, returnConts = returnConts, labels = labels }
                                            end
                                | NONE => ()
                             )
  | useValue env C.Unit = ()
  | useValue env C.Nil = ()
  | useValue env (C.BoolConst _) = ()
  | useValue env (C.IntConst _) = ()
  | useValue env (C.WordConst _) = ()
  | useValue env (C.CharConst _) = ()
  | useValue env (C.Char16Const _) = ()
  | useValue env (C.StringConst _) = ()
  | useValue env (C.String16Const _) = ()
fun useValueAsCallee (env, cont, C.Var v)
    = (case TypedSyntax.VIdTable.find env v of
           SOME r => let val { call, project, other, returnConts, labels } = !r
                     in r := { call = oneMore call, project = project, other = other, returnConts = C.CVarSet.add (returnConts, cont), labels = labels }
                     end
         | NONE => ()
      )
  | useValueAsCallee (env, cont, C.Unit) = ()
  | useValueAsCallee (env, cont, C.Nil) = ()
  | useValueAsCallee (env, cont, C.BoolConst _) = ()
  | useValueAsCallee (env, cont, C.IntConst _) = ()
  | useValueAsCallee (env, cont, C.WordConst _) = ()
  | useValueAsCallee (env, cont, C.CharConst _) = ()
  | useValueAsCallee (env, cont, C.Char16Const _) = ()
  | useValueAsCallee (env, cont, C.StringConst _) = ()
  | useValueAsCallee (env, cont, C.String16Const _) = ()
fun useValueAsRecord (env, label, result, C.Var v)
    = (case TypedSyntax.VIdTable.find env v of
           SOME r => let val { call, project, other, returnConts, labels } = !r
                         val result' = case result of
                                           SOME (TypedSyntax.MkVId (name, _)) => SOME name
                                         | NONE => NONE
                         fun mergeOption (x as SOME _, _) = x
                           | mergeOption (NONE, y) = y
                     in r := { call = call, project = oneMore project, other = other, returnConts = returnConts, labels = Syntax.LabelMap.insertWith mergeOption (labels, label, result') }
                     end
         | NONE => ()
      )
  | useValueAsRecord (env, label, result, C.Unit) = ()
  | useValueAsRecord (env, label, result, C.Nil) = ()
  | useValueAsRecord (env, label, result, C.BoolConst _) = ()
  | useValueAsRecord (env, label, result, C.IntConst _) = ()
  | useValueAsRecord (env, label, result, C.WordConst _) = ()
  | useValueAsRecord (env, label, result, C.CharConst _) = ()
  | useValueAsRecord (env, label, result, C.Char16Const _) = ()
  | useValueAsRecord (env, label, result, C.StringConst _) = ()
  | useValueAsRecord (env, label, result, C.String16Const _) = ()
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
fun goSimpleExp (env, renv, cenv, crenv, _, C.PrimOp { primOp = _, tyargs = _, args }) = List.app (useValue env) args
  | goSimpleExp (env, renv, cenv, crenv, _, C.Record fields) = Syntax.LabelMap.app (useValue env) fields
  | goSimpleExp (env, renv, cenv, crenv, _, C.ExnTag { name = _, payloadTy = _ }) = ()
  | goSimpleExp (env, renv, cenv, crenv, result, C.Projection { label, record, fieldTypes = _ }) = useValueAsRecord (env, label, result, record)
  | goSimpleExp (env, renv, cenv, crenv, _, C.Abs { contParam, params, body })
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
         let val recursiveEnv = List.foldl (fn ((f, _, _, _), m) => TypedSyntax.VIdMap.insert (m, f, ref neverUsed)) TypedSyntax.VIdMap.empty defs
         in TypedSyntax.VIdMap.appi (fn (f, v) => TypedSyntax.VIdTable.insert env (f, v)) recursiveEnv
          ; List.app (fn (_, k, params, body) => ( addC (cenv, k)
                                                 ; List.app (fn p => add (env, p)) params
                                                 ; goCExp (env, renv, cenv, crenv, body)
                                                 )
                     ) defs
          ; TypedSyntax.VIdMap.appi (fn (f, v) => TypedSyntax.VIdTable.insert renv (f, v)) recursiveEnv
          ; List.app (fn (f, _, _, _) => TypedSyntax.VIdTable.insert env (f, ref neverUsed)) defs
         end
       | C.ContDec { name, params, body } =>
         ( List.app (Option.app (fn p => add (env, p))) params
         ; goCExp (env, renv, cenv, crenv, body)
         ; addC (cenv, name)
         )
       | C.RecContDec defs =>
         let val recursiveCEnv = List.foldl (fn ((f, _, _), m) => C.CVarMap.insert (m, f, ref neverUsedCont)) C.CVarMap.empty defs
         in C.CVarMap.appi (fn (f, v) => C.CVarTable.insert cenv (f, v)) recursiveCEnv
          ; List.app (fn (f, params, body) => ( List.app (Option.app (fn p => add (env, p))) params
                                              ; goCExp (env, renv, cenv, crenv, body)
                                              )
                     ) defs
          ; C.CVarMap.appi (fn (f, v) => C.CVarTable.insert crenv (f, v)) recursiveCEnv
          ; List.app (fn (f, _, _) => C.CVarTable.insert cenv (f, ref neverUsedCont)) defs
         end
       | C.ESImportDec { pure, specs, moduleName } => List.app (fn (_, vid) => add (env, vid)) specs
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
              type Context = { nextVId : int ref, simplificationOccurred : bool ref }
              type value_info
              val simplifyCExp : Context * value_info TypedSyntax.VIdMap.map * (CSyntax.Var option list * CSyntax.CExp option) CSyntax.CVarMap.map * CSyntax.Value TypedSyntax.VIdMap.map * CSyntax.CVar CSyntax.CVarMap.map * { usage : CpsUsageAnalysis.usage_table, rec_usage : CpsUsageAnalysis.usage_table, cont_usage : CpsUsageAnalysis.cont_usage_table, cont_rec_usage : CpsUsageAnalysis.cont_usage_table, dead_code_analysis : CpsDeadCodeAnalysis.usage } * CSyntax.CExp -> CSyntax.CExp
              val finalizeCExp : Context * CSyntax.CExp -> CSyntax.CExp
          end = struct
local structure F = FSyntax
      structure C = CSyntax
      structure P = Primitives
      datatype frequency = datatype CpsUsageAnalysis.frequency
in
type Context = { nextVId : int ref, simplificationOccurred : bool ref }
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
            | C.Abs { contParam, params, body } => sizeOfCExp (body, threshold)
and sizeOfDec (dec, threshold)
    = if threshold < 0 then
          threshold
      else
          case dec of
              C.ValDec { exp, result = _ } => sizeOfSimpleExp (exp, threshold)
            | C.RecDec defs => List.foldl (fn ((_, _, _, body), t) => sizeOfCExp (body, t)) threshold defs
            | C.ContDec { name, params, body } => sizeOfCExp (body, threshold)
            | C.RecContDec defs => List.foldl (fn ((_, _, body), t) => sizeOfCExp (body, t)) threshold defs
            | C.ESImportDec _ => 0
and sizeOfCExp (e, threshold)
    = if threshold < 0 then
          threshold
      else
          case e of
              C.Let { decs, cont } => Vector.foldl sizeOfDec (sizeOfCExp (cont, threshold)) decs
            | C.App { applied, cont, args } => threshold - List.length args
            | C.AppCont { applied, args } => threshold - List.length args
            | C.If { cond, thenCont, elseCont } => sizeOfCExp (elseCont, sizeOfCExp (thenCont, threshold - 1))
            | C.Handle { body, handler = (_, h), successfulExitIn, successfulExitOut } => sizeOfCExp (body, sizeOfCExp (h, threshold - 1))
            | C.Unreachable => threshold
fun substValue (subst : C.Value TypedSyntax.VIdMap.map) (x as C.Var v) = (case TypedSyntax.VIdMap.find (subst, v) of
                                                                              SOME w => w
                                                                            | NONE => x
                                                                         )
  | substValue subst v = v
fun substCVar (csubst : C.CVar C.CVarMap.map) v = case C.CVarMap.find (csubst, v) of
                                                      SOME w => w
                                                    | NONE => v
fun substSimpleExp (subst, csubst, C.PrimOp { primOp, tyargs, args }) = C.PrimOp { primOp = primOp, tyargs = tyargs, args = List.map (substValue subst) args }
  | substSimpleExp (subst, csubst, C.Record fields) = C.Record (Syntax.LabelMap.map (substValue subst) fields)
  | substSimpleExp (subst, csubst, e as C.ExnTag _) = e
  | substSimpleExp (subst, csubst, C.Projection { label, record, fieldTypes }) = C.Projection { label = label, record = substValue subst record, fieldTypes = fieldTypes }
  | substSimpleExp (subst, csubst, C.Abs { contParam, params, body }) = C.Abs { contParam = contParam, params = params, body = substCExp (subst, csubst, body) }
and substDec (subst, csubst) = fn C.ValDec { exp, result } => C.ValDec { exp = substSimpleExp (subst, csubst, exp), result = result }
                                | C.RecDec defs => C.RecDec (List.map (fn (f, k, params, body) => (f, k, params, substCExp (subst, csubst, body))) defs)
                                | C.ContDec { name, params, body } => C.ContDec { name = name, params = params, body = substCExp (subst, csubst, body) }
                                | C.RecContDec defs => C.RecContDec (List.map (fn (f, params, body) => (f, params, substCExp (subst, csubst, body))) defs)
                                | dec as C.ESImportDec { pure, specs, moduleName } => dec
and substCExp (subst : C.Value TypedSyntax.VIdMap.map, csubst : C.CVar C.CVarMap.map, C.Let { decs, cont }) = C.Let { decs = Vector.map (substDec (subst, csubst)) decs, cont = substCExp (subst, csubst, cont) }
  | substCExp (subst, csubst, C.App { applied, cont, args }) = C.App { applied = substValue subst applied, cont = substCVar csubst cont, args = List.map (substValue subst) args }
  | substCExp (subst, csubst, C.AppCont { applied, args }) = C.AppCont { applied = substCVar csubst applied, args = List.map (substValue subst) args }
  | substCExp (subst, csubst, C.If { cond, thenCont, elseCont }) = C.If { cond = substValue subst cond, thenCont = substCExp (subst, csubst, thenCont), elseCont = substCExp (subst, csubst, elseCont) }
  | substCExp (subst, csubst, C.Handle { body, handler = (e, h), successfulExitIn, successfulExitOut }) = C.Handle { body = substCExp (subst, csubst, body), handler = (e, substCExp (subst, csubst, h)), successfulExitIn = successfulExitIn, successfulExitOut = substCVar csubst successfulExitOut }
  | substCExp (subst, csubst, e as C.Unreachable) = e
val substCExp = fn (subst, csubst, e) => if TypedSyntax.VIdMap.isEmpty subst andalso C.CVarMap.isEmpty csubst then
                                             e
                                         else
                                             substCExp (subst, csubst, e)
fun alphaConvertSimpleExp (ctx, subst, csubst, C.Abs { contParam, params, body })
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
               }
      end
  | alphaConvertSimpleExp (ctx, subst, csubst, e) = substSimpleExp (subst, csubst, e)
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
          let val (subst, nameMap) = List.foldl (fn ((f, _, _, _), (subst, nameMap)) =>
                                                    let val f' = renewVId (ctx, f)
                                                    in (TypedSyntax.VIdMap.insert (subst, f, C.Var f'), TypedSyntax.VIdMap.insert (nameMap, f, f'))
                                                    end
                                                ) (subst, TypedSyntax.VIdMap.empty) defs
              val dec' = C.RecDec (List.map (fn (f, k, params, body) =>
                                                let val f' = TypedSyntax.VIdMap.lookup (nameMap, f)
                                                    val k' = renewCVar (ctx, k)
                                                    val (params', subst) = List.foldr (fn (p, (params', subst)) =>
                                                                                          let val p' = renewVId (ctx, p)
                                                                                          in (p' :: params', TypedSyntax.VIdMap.insert (subst, p, C.Var p'))
                                                                                          end
                                                                                      ) ([], subst) params
                                                    val csubst = C.CVarMap.insert (csubst, k, k')
                                                in (f', k', params', alphaConvert (ctx, subst, csubst, body))
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
  | alphaConvert (ctx, subst, csubst, C.App { applied, cont, args })
    = C.App { applied = substValue subst applied
            , cont = substCVar csubst cont
            , args = List.map (substValue subst) args
            }
  | alphaConvert (ctx, subst, csubst, C.AppCont { applied, args })
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
  | alphaConvert (ctx, subst, csubst, e as C.Unreachable) = e
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
        | C.RecDec defs => SOME env
        | C.ContDec { name, params, body } => if isDiscardableExp (env, body) then
                                                  SOME env
                                              else
                                                  NONE
        | C.RecContDec _ => NONE
        | C.ESImportDec { pure, specs, moduleName } => SOME env
and isDiscardableExp (env : value_info TypedSyntax.VIdMap.map, C.Let { decs, cont })
    = (case VectorUtil.foldlOption isDiscardableDec env decs of
           SOME env => isDiscardableExp (env, cont)
         | NONE => false
      )
  | isDiscardableExp (env, C.App { applied = C.Var applied, cont, args })
    = (case TypedSyntax.VIdMap.find (env, applied) of
           SOME { isDiscardableFunction = true, ... } => true
         | _ => false
      )
  | isDiscardableExp (env, C.App _) = false (* should not occur *)
  | isDiscardableExp (env, C.AppCont _) = true
  | isDiscardableExp (env, C.If { cond, thenCont, elseCont }) = isDiscardableExp (env, thenCont) andalso isDiscardableExp (env, elseCont)
  | isDiscardableExp (env, C.Handle { body, handler = (e, h), successfulExitIn, successfulExitOut }) = isDiscardableExp (env, body) andalso isDiscardableExp (env, h)
  | isDiscardableExp (env, C.Unreachable) = false
datatype param_transform = KEEP | ELIMINATE | UNPACK of (C.Var * Syntax.Label) list
fun tryUnpackParam (ctx, usage) param = case CpsUsageAnalysis.getValueUsage (usage, param) of
                                            { call = NEVER, project = NEVER, other = NEVER, ... } => ELIMINATE
                                          | { call = NEVER, project = _, other = NEVER, labels, ... } =>
                                            UNPACK (Syntax.LabelMap.foldri (fn (label, optName, acc) =>
                                                                               let val name = case optName of
                                                                                                  SOME name => name
                                                                                                | NONE => case label of
                                                                                                              Syntax.IdentifierLabel name => name
                                                                                                            | Syntax.NumericLabel n => "_" ^ Int.toString n
                                                                               in (newVId (ctx, name), label) :: acc
                                                                               end
                                                                           ) [] labels)
                                          | _ => KEEP
fun tryUnpackContParam (ctx, usage) (SOME param)
    = (case CpsUsageAnalysis.getValueUsage (usage, param) of
           { call = NEVER, project = NEVER, other = NEVER, ... } => ELIMINATE
         | { call = NEVER, project = _, other = NEVER, labels, ... } =>
           UNPACK (Syntax.LabelMap.foldri (fn (label, optName, acc) =>
                                              let val name = case optName of
                                                                 SOME name => name
                                                               | NONE => case label of
                                                                             Syntax.IdentifierLabel name => name
                                                                           | Syntax.NumericLabel n => "_" ^ Int.toString n
                                              in (newVId (ctx, name), label) :: acc
                                              end
                                          ) [] labels)
         | _ => KEEP
      )
  | tryUnpackContParam (ctx, usage) NONE = ELIMINATE
fun isInt32 (x : IntInf.int) = ~0x80000000 <= x andalso x <= 0x7fffffff
fun isInt54 (x : IntInf.int) = ~0x20000000000000 <= x andalso x <= 0x1fffffffffffff
fun isInt64 (x : IntInf.int) = ~0x8000000000000000 <= x andalso x <= 0x7fffffffffffffff
fun isInt (P.INT, x) = false
  | isInt (P.I32, x) = isInt32 x
  | isInt (P.I54, x) = isInt54 x
  | isInt (P.I64, x) = isInt64 x
  | isInt (P.INT_INF, x) = true
fun wraparound (P.INT, x : IntInf.int) = x
  | wraparound (P.I32, x) = (x + 0x80000000) mod 0x100000000 - 0x80000000
  | wraparound (P.I54, x) = (x + 0x20000000000000) mod 0x40000000000000 - 0x20000000000000
  | wraparound (P.I64, x) = (x + 0x8000000000000000) mod 0x10000000000000000 - 0x8000000000000000
  | wraparound (P.INT_INF, x) = x
fun min3 (x, y, z) = IntInf.min (x, IntInf.min (y, z))
fun max3 (x, y, z) = IntInf.max (x, IntInf.max (y, z))
fun simplifySimpleExp (env : value_info TypedSyntax.VIdMap.map, C.Record fields) = NOT_SIMPLIFIED
  | simplifySimpleExp (env, C.PrimOp { primOp, tyargs, args })
    = (case (primOp, args) of
           (F.ListOp, []) => VALUE C.Nil (* empty list *)
         | (F.PrimCall P.JavaScript_call, [f, C.Var args]) =>
           (case TypedSyntax.VIdMap.find (env, args) of
                SOME { exp = SOME (C.PrimOp { primOp = F.VectorOp, tyargs = _, args }), ... } => SIMPLE_EXP (C.PrimOp { primOp = F.JsCallOp, tyargs = [], args = f :: args })
              | _ => NOT_SIMPLIFIED
           )
         | (F.PrimCall P.JavaScript_method, [obj, name, C.Var args]) =>
           (case TypedSyntax.VIdMap.find (env, args) of
                SOME { exp = SOME (C.PrimOp { primOp = F.VectorOp, tyargs = _, args }), ... } => SIMPLE_EXP (C.PrimOp { primOp = F.JsMethodOp, tyargs = [], args = obj :: name :: args })
              | _ => NOT_SIMPLIFIED
           )
         | (F.PrimCall P.JavaScript_new, [ctor, C.Var args]) =>
           (case TypedSyntax.VIdMap.find (env, args) of
                SOME { exp = SOME (C.PrimOp { primOp = F.VectorOp, tyargs = _, args }), ... } => SIMPLE_EXP (C.PrimOp { primOp = F.JsNewOp, tyargs = [], args = ctor :: args })
              | _ => NOT_SIMPLIFIED
           )
         | (F.PrimCall P.Lua_call, [ctor, C.Var args]) =>
           (case TypedSyntax.VIdMap.find (env, args) of
                SOME { exp = SOME (C.PrimOp { primOp = F.VectorOp, tyargs = _, args }), ... } => SIMPLE_EXP (C.PrimOp { primOp = F.LuaCallOp, tyargs = [], args = ctor :: args })
              | _ => NOT_SIMPLIFIED
           )
         | (F.PrimCall P.Lua_call1, [ctor, C.Var args]) =>
           (case TypedSyntax.VIdMap.find (env, args) of
                SOME { exp = SOME (C.PrimOp { primOp = F.VectorOp, tyargs = _, args }), ... } => SIMPLE_EXP (C.PrimOp { primOp = F.LuaCall1Op, tyargs = [], args = ctor :: args })
              | _ => NOT_SIMPLIFIED
           )
         | (F.PrimCall P.Lua_method, [ctor, C.StringConst name, C.Var args]) =>
           if LuaWriter.isLuaIdentifier name then
               case TypedSyntax.VIdMap.find (env, args) of
                   SOME { exp = SOME (C.PrimOp { primOp = F.VectorOp, tyargs = _, args }), ... } => SIMPLE_EXP (C.PrimOp { primOp = F.LuaMethodOp name, tyargs = [], args = ctor :: args })
                 | _ => NOT_SIMPLIFIED
           else
               NOT_SIMPLIFIED
         | (F.DataTagAsStringOp _, [C.Var x]) =>
           (case TypedSyntax.VIdMap.find (env, x) of
                SOME { exp = SOME (C.PrimOp { primOp = F.ConstructValOp { tag, ... }, ... }), ... } => VALUE (C.StringConst tag)
              | SOME { exp = SOME (C.PrimOp { primOp = F.ConstructValWithPayloadOp { tag, ... }, ... }), ... } => VALUE (C.StringConst tag)
              | _ => NOT_SIMPLIFIED
           )
         | (F.DataTagAsString16Op _, [C.Var x]) =>
           (case TypedSyntax.VIdMap.find (env, x) of
                SOME { exp = SOME (C.PrimOp { primOp = F.ConstructValOp { tag, ... }, ... }), ... } => VALUE (C.String16Const (Vector.tabulate (String.size tag, fn i => ord (String.sub (tag, i))))) (* Assume tag is ASCII *)
              | SOME { exp = SOME (C.PrimOp { primOp = F.ConstructValWithPayloadOp { tag, ... }, ... }), ... } => VALUE (C.String16Const (Vector.tabulate (String.size tag, fn i => ord (String.sub (tag, i))))) (* Assume tag is ASCII *)
              | _ => NOT_SIMPLIFIED
           )
         | (F.PrimCall P.Bool_EQUAL, [x, C.BoolConst true]) => VALUE x
         | (F.PrimCall P.Bool_EQUAL, [C.BoolConst true, x]) => VALUE x
         | (F.PrimCall P.Bool_EQUAL, [x, C.BoolConst false]) => SIMPLE_EXP (C.PrimOp { primOp = F.PrimCall P.Bool_not, tyargs = [], args = [x] })
         | (F.PrimCall P.Bool_EQUAL, [C.BoolConst false, x]) => SIMPLE_EXP (C.PrimOp { primOp = F.PrimCall P.Bool_not, tyargs = [], args = [x] })
         | (F.PrimCall P.Bool_not, [C.BoolConst x]) => VALUE (C.BoolConst (not x))
         | (F.PrimCall P.Bool_not, [C.Var x]) =>
           (case TypedSyntax.VIdMap.find (env, x) of
                SOME { exp = SOME (C.PrimOp { primOp = F.PrimCall P.Bool_not, tyargs = _, args = [v] }), ... } => VALUE v
              | _ => NOT_SIMPLIFIED
           )
         | (F.PrimCall P.String_EQUAL, [C.StringConst x, C.StringConst y]) => VALUE (C.BoolConst (x = y))
         | (F.PrimCall P.String16_EQUAL, [C.String16Const x, C.String16Const y]) => VALUE (C.BoolConst (x = y))
         | (F.PrimCall (P.Int_EQUAL w), [C.IntConst (w', x), C.IntConst (w'', y)]) => if w = w' andalso w = w'' then
                                                                                          VALUE (C.BoolConst (x = y))
                                                                                      else
                                                                                          NOT_SIMPLIFIED
         | (F.PrimCall (P.Int_LT w), [C.IntConst (w', x), C.IntConst (w'', y)]) => if w = w' andalso w = w'' then
                                                                                       VALUE (C.BoolConst (x < y))
                                                                                   else
                                                                                       NOT_SIMPLIFIED
         | (F.PrimCall (P.Int_LE w), [C.IntConst (w', x), C.IntConst (w'', y)]) => if w = w' andalso w = w'' then
                                                                                       VALUE (C.BoolConst (x <= y))
                                                                                   else
                                                                                       NOT_SIMPLIFIED
         | (F.PrimCall (P.Int_GT w), [C.IntConst (w', x), C.IntConst (w'', y)]) => if w = w' andalso w = w'' then
                                                                                       VALUE (C.BoolConst (x > y))
                                                                                   else
                                                                                       NOT_SIMPLIFIED
         | (F.PrimCall (P.Int_GE w), [C.IntConst (w', x), C.IntConst (w'', y)]) => if w = w' andalso w = w'' then
                                                                                       VALUE (C.BoolConst (x >= y))
                                                                                   else
                                                                                       NOT_SIMPLIFIED
         | (F.PrimCall (P.Int_PLUS w), [C.IntConst (w', x), C.IntConst (w'', y)]) => if w = w' andalso w = w'' then
                                                                                         let val z = x + y
                                                                                         in if isInt (w, z) orelse (min3 (x, y, 0) <= z andalso z <= max3 (x, y, 0)) then
                                                                                                VALUE (C.IntConst (w, z))
                                                                                            else
                                                                                                NOT_SIMPLIFIED
                                                                                         end
                                                                                     else
                                                                                         NOT_SIMPLIFIED
         | (F.PrimCall (P.Int_PLUS w), [C.IntConst (w', 0), y]) => if w = w' then VALUE y else NOT_SIMPLIFIED
         | (F.PrimCall (P.Int_PLUS w), [x, C.IntConst (w', 0)]) => if w = w' then VALUE x else NOT_SIMPLIFIED
         | (F.PrimCall (P.Int_PLUS_wrapping w), [C.IntConst (w', x), C.IntConst (w'', y)]) => if w <> P.INT andalso w = w' andalso w = w'' then
                                                                                                  VALUE (C.IntConst (w, wraparound (w, x + y)))
                                                                                              else
                                                                                                  NOT_SIMPLIFIED
         | (F.PrimCall (P.Int_PLUS_wrapping w), [C.IntConst (w', 0), y]) => if w = w' then VALUE y else NOT_SIMPLIFIED
         | (F.PrimCall (P.Int_PLUS_wrapping w), [x, C.IntConst (w', 0)]) => if w = w' then VALUE x else NOT_SIMPLIFIED
         | (F.PrimCall (P.Int_MINUS w), [C.IntConst (w', x), C.IntConst (w'', y)]) => if w = w' andalso w = w'' then
                                                                                         let val z = x - y
                                                                                         in if isInt (w, z) orelse (min3 (x, y, 0) <= z andalso z <= max3 (x, y, 0)) then
                                                                                                VALUE (C.IntConst (w, z))
                                                                                            else
                                                                                                NOT_SIMPLIFIED
                                                                                         end
                                                                                      else
                                                                                          NOT_SIMPLIFIED
         | (F.PrimCall (P.Int_MINUS w), [C.IntConst (w', 0), y]) => if w = w' then
                                                                        SIMPLE_EXP (C.PrimOp { primOp = F.PrimCall (P.Int_TILDE w), tyargs = [], args = [y] })
                                                                    else
                                                                        NOT_SIMPLIFIED
         | (F.PrimCall (P.Int_MINUS w), [x, C.IntConst (w', 0)]) => if w = w' then VALUE x else NOT_SIMPLIFIED
         | (F.PrimCall (P.Int_MINUS_wrapping w), [C.IntConst (w', x), C.IntConst (w'', y)]) => if w = w' andalso w = w'' then
                                                                                                   VALUE (C.IntConst (w, wraparound (w, x - y)))
                                                                                               else
                                                                                                   NOT_SIMPLIFIED
         | (F.PrimCall (P.Int_MINUS_wrapping w), [C.IntConst (w', 0), y]) => if w = w' then
                                                                                 SIMPLE_EXP (C.PrimOp { primOp = F.PrimCall (P.Int_TILDE_wrapping w), tyargs = [], args = [y] })
                                                                             else
                                                                                 NOT_SIMPLIFIED
         | (F.PrimCall (P.Int_MINUS_wrapping w), [x, C.IntConst (w', 0)]) => if w = w' then VALUE x else NOT_SIMPLIFIED
         | (F.PrimCall (P.Int_TIMES w), [zero as C.IntConst (w', 0), _]) => if w = w' then VALUE zero else NOT_SIMPLIFIED
         | (F.PrimCall (P.Int_TIMES w), [_, zero as C.IntConst (w', 0)]) => if w = w' then VALUE zero else NOT_SIMPLIFIED
         | (F.PrimCall (P.Int_TIMES w), [C.IntConst (w', 1), y]) => if w = w' then VALUE y else NOT_SIMPLIFIED
         | (F.PrimCall (P.Int_TIMES w), [x, C.IntConst (w', 1)]) => if w = w' then VALUE x else NOT_SIMPLIFIED
         | (F.PrimCall (P.Int_TIMES w), [x as C.IntConst (w', x'), y as C.IntConst (w'', y')]) =>
           if w = w' andalso w = w'' then
               let val z = x' * y'
               in if isInt (w, z) orelse (min3 (x', y', 0) <= z andalso z <= max3 (x', y', 0)) then
                      VALUE (C.IntConst (w, z))
                  else
                      if x' = ~1 then
                          SIMPLE_EXP (C.PrimOp { primOp = F.PrimCall (P.Int_TILDE w), tyargs = [], args = [y] })
                      else if y' = ~1 then
                          SIMPLE_EXP (C.PrimOp { primOp = F.PrimCall (P.Int_TILDE w), tyargs = [], args = [x] })
                      else
                          NOT_SIMPLIFIED
               end
           else
               NOT_SIMPLIFIED
         | (F.PrimCall (P.Int_TIMES w), [C.IntConst (w', ~1), y]) => if w = w' then SIMPLE_EXP (C.PrimOp { primOp = F.PrimCall (P.Int_TILDE w), tyargs = [], args = [y] }) else NOT_SIMPLIFIED
         | (F.PrimCall (P.Int_TIMES w), [x, C.IntConst (w', ~1)]) => if w = w' then SIMPLE_EXP (C.PrimOp { primOp = F.PrimCall (P.Int_TILDE w), tyargs = [], args = [x] }) else NOT_SIMPLIFIED
         | (F.PrimCall (P.Int_TIMES_wrapping w), [zero as C.IntConst (w', 0), _]) => if w = w' then VALUE zero else NOT_SIMPLIFIED
         | (F.PrimCall (P.Int_TIMES_wrapping w), [_, zero as C.IntConst (w', 0)]) => if w = w' then VALUE zero else NOT_SIMPLIFIED
         | (F.PrimCall (P.Int_TIMES_wrapping w), [C.IntConst (w', 1), y]) => if w = w' then VALUE y else NOT_SIMPLIFIED
         | (F.PrimCall (P.Int_TIMES_wrapping w), [x, C.IntConst (w', 1)]) => if w = w' then VALUE x else NOT_SIMPLIFIED
         | (F.PrimCall (P.Int_TIMES_wrapping w), [x as C.IntConst (w', x'), y as C.IntConst (w'', y')]) =>
           if w = w' andalso w = w'' then
               VALUE (C.IntConst (w, wraparound (w, x' * y')))
           else
               NOT_SIMPLIFIED
         | (F.PrimCall (P.Int_TIMES_wrapping w), [C.IntConst (w', ~1), y]) => if w = w' then SIMPLE_EXP (C.PrimOp { primOp = F.PrimCall (P.Int_TILDE_wrapping w), tyargs = [], args = [y] }) else NOT_SIMPLIFIED
         | (F.PrimCall (P.Int_TIMES_wrapping w), [x, C.IntConst (w', ~1)]) => if w = w' then SIMPLE_EXP (C.PrimOp { primOp = F.PrimCall (P.Int_TILDE_wrapping w), tyargs = [], args = [x] }) else NOT_SIMPLIFIED
         | (F.PrimCall (P.Int_div w), [x, y as C.IntConst (w', y')]) =>
           if w = w' then
               if y' = 1 then
                   VALUE x
               else if y' = ~1 then
                   SIMPLE_EXP (C.PrimOp { primOp = F.PrimCall (P.Int_TILDE w), tyargs = [], args = [x] })
               else if y' <> 0 then
                   case x of
                       C.IntConst (w'', x') => if w = w'' then VALUE (C.IntConst (w, x' div y')) else NOT_SIMPLIFIED
                     | _ => SIMPLE_EXP (C.PrimOp { primOp = F.PrimCall (P.Int_div_unchecked w), tyargs = [], args = [x, y] })
               else
                   NOT_SIMPLIFIED
           else
               NOT_SIMPLIFIED
         | (F.PrimCall (P.Int_mod w), [x, y as C.IntConst (w', y')]) =>
           if w = w' then
               if y' = 1 orelse y' = ~1 then
                   VALUE (C.IntConst (w, 0))
               else if y' <> 0 then
                   case x of
                       C.IntConst (w'', x') => if w = w'' then VALUE (C.IntConst (w, x' mod y')) else NOT_SIMPLIFIED
                     | _ => SIMPLE_EXP (C.PrimOp { primOp = F.PrimCall (P.Int_mod_unchecked w), tyargs = [], args = [x, y] })
               else
                   NOT_SIMPLIFIED
           else
               NOT_SIMPLIFIED
         | (F.PrimCall (P.Int_quot w), [x, y as C.IntConst (w', y')]) =>
           if w = w' then
               if y' = 1 then
                   VALUE x
               else if y' = ~1 then
                   SIMPLE_EXP (C.PrimOp { primOp = F.PrimCall (P.Int_TILDE w), tyargs = [], args = [x] })
               else if y' <> 0 then
                   case x of
                       C.IntConst (w'', x') => if w = w'' then VALUE (C.IntConst (w, IntInf.quot (x', y'))) else NOT_SIMPLIFIED
                     | _ => SIMPLE_EXP (C.PrimOp { primOp = F.PrimCall (P.Int_quot_unchecked w), tyargs = [], args = [x, y] })
               else
                   NOT_SIMPLIFIED
           else
               NOT_SIMPLIFIED
         | (F.PrimCall (P.Int_rem w), [x, y as C.IntConst (w', y')]) =>
           if w = w' then
               if y' = 1 orelse y' = ~1 then
                   VALUE (C.IntConst (w, 0))
               else if y' <> 0 then
                   case x of
                       C.IntConst (w'', x') => if w = w'' then VALUE (C.IntConst (w, IntInf.rem (x', y'))) else NOT_SIMPLIFIED
                     | _ => SIMPLE_EXP (C.PrimOp { primOp = F.PrimCall (P.Int_rem_unchecked w), tyargs = [], args = [x, y] })
               else
                   NOT_SIMPLIFIED
           else
               NOT_SIMPLIFIED
         | (F.PrimCall (P.Int_TILDE_wrapping w), [C.IntConst (w', x)]) => if w = w' then
                                                                              VALUE (C.IntConst (w, wraparound (w, ~ x)))
                                                                          else
                                                                              NOT_SIMPLIFIED
         | (F.PrimCall (P.Int_toInt_unchecked (w1, w2)), [x as C.IntConst (w', x')]) => if w1 = w' then
                                                                                            if w1 = w2 then
                                                                                                VALUE x (* no op *)
                                                                                            else
                                                                                                if isInt (w2, x') then
                                                                                                    VALUE (C.IntConst (w2, x'))
                                                                                                else
                                                                                                    NOT_SIMPLIFIED
                                                                                        else
                                                                                            NOT_SIMPLIFIED
         | (F.PrimCall (P.Int_toInt_unchecked (w1, w2)), [x]) => if w1 = w2 then VALUE x else NOT_SIMPLIFIED
         | (F.PrimCall (P.Word_div w), [x, y as C.WordConst (w', y')]) =>
           if w = w' then
               if y' = 1 then
                   VALUE x
               else if y' <> 0 then
                   case x of
                       C.WordConst (w'', x') => if w = w'' then VALUE (C.WordConst (w, x' div y')) else NOT_SIMPLIFIED
                     | _ => SIMPLE_EXP (C.PrimOp { primOp = F.PrimCall (P.Word_div_unchecked w), tyargs = [], args = [x, y] })
               else
                   NOT_SIMPLIFIED
           else
               NOT_SIMPLIFIED
         | (F.PrimCall (P.Word_mod w), [x, y as C.WordConst (w', y')]) =>
           if w = w' then
               if y' = 1 then
                   VALUE (C.WordConst (w, 0))
               else if y' <> 0 then
                   case x of
                       C.WordConst (w'', x') => if w = w'' then VALUE (C.WordConst (w, x' mod y')) else NOT_SIMPLIFIED
                     | _ => SIMPLE_EXP (C.PrimOp { primOp = F.PrimCall (P.Word_mod_unchecked w), tyargs = [], args = [x, y] })
               else
                   NOT_SIMPLIFIED
           else
               NOT_SIMPLIFIED
         | (F.PrimCall (P.Char_ord w), [C.CharConst c]) => VALUE (C.IntConst (w, Int.toLarge (Char.ord c)))
         | (F.PrimCall (P.Char_chr_unchecked w), [C.IntConst (w', c)]) =>
           if w = w' andalso 0 <= c andalso c <= 255 then
               VALUE (C.CharConst (Char.chr (Int.fromLarge c)))
           else
               NOT_SIMPLIFIED
         | (F.PrimCall (P.Char16_ord w), [C.Char16Const c]) => VALUE (C.IntConst (w, Int.toLarge c))
         | (F.PrimCall (P.Char16_chr_unchecked w), [C.IntConst (w', c)]) =>
           if w = w' andalso 0 <= c andalso c <= 0xffff then
               VALUE (C.Char16Const (Int.fromLarge c))
           else
               NOT_SIMPLIFIED
         | _ => NOT_SIMPLIFIED
      )
  | simplifySimpleExp (env, C.ExnTag _) = NOT_SIMPLIFIED
  | simplifySimpleExp (env, C.Projection { label, record, fieldTypes })
    = (case record of
           C.Var v => (case TypedSyntax.VIdMap.find (env, v) of
                           SOME { exp = SOME (C.Record fields), ... } => (case Syntax.LabelMap.find (fields, label) of
                                                                              SOME w => VALUE w
                                                                            | NONE => NOT_SIMPLIFIED
                                                                         )
                         | _ => NOT_SIMPLIFIED
                      )
         | _ => NOT_SIMPLIFIED
      )
  | simplifySimpleExp (env, C.Abs { contParam, params, body }) = NOT_SIMPLIFIED (* TODO: Try eta conversion *)
and simplifyDec (ctx : Context, usage : { usage : CpsUsageAnalysis.usage_table, rec_usage : CpsUsageAnalysis.usage_table, cont_usage : CpsUsageAnalysis.cont_usage_table, cont_rec_usage : CpsUsageAnalysis.cont_usage_table, dead_code_analysis : CpsDeadCodeAnalysis.usage }, appliedCont : C.CVar option) (dec, (env, cenv, subst, csubst, acc : C.Dec list))
    = case dec of
          C.ValDec { exp, result } =>
          let val exp = substSimpleExp (subst, csubst, exp)
              val result = case result of
                               SOME name => if CpsDeadCodeAnalysis.isUsed (#dead_code_analysis usage, name) then
                                                result
                                            else
                                                NONE
                             | NONE => NONE
          in case simplifySimpleExp (env, exp) of
                 VALUE v => let val () = #simplificationOccurred ctx := true
                                val subst = case result of
                                                SOME result => TypedSyntax.VIdMap.insert (subst, result, v)
                                              | NONE => subst
                            in (env, cenv, subst, csubst, acc)
                            end
               | simplified =>
                 let val () = case simplified of
                                  SIMPLE_EXP _ => #simplificationOccurred ctx := true
                                | VALUE _ => #simplificationOccurred ctx := true (* shoud not occur *)
                                | NOT_SIMPLIFIED => ()
                     val exp = case simplified of
                                   SIMPLE_EXP exp => exp
                                 | _ => exp
                 in case (exp, result) of
                        (C.Abs { contParam, params, body }, SOME result) =>
                        (case CpsUsageAnalysis.getValueUsage (#usage usage, result) of
                             { call = NEVER, project = NEVER, other = NEVER, returnConts = _, labels = _ } => (env, cenv, subst, csubst, acc)
                           | { call = ONCE, project = NEVER, other = NEVER, returnConts = _, labels = _ } =>
                             let val body = simplifyCExp (ctx, env, cenv, subst, csubst, usage, body)
                                 val env = TypedSyntax.VIdMap.insert (env, result, { exp = SOME (C.Abs { contParam = contParam, params = params, body = body }), isDiscardableFunction = isDiscardableExp (env, body) })
                             in (env, cenv, subst, csubst, acc)
                             end
                           | u => let val isSmall = sizeOfCExp (body, 10) >= 0
                                      val shouldTransformParams = if isSmall then
                                                                      NONE
                                                                  else
                                                                      case u of
                                                                          { call = _, project = NEVER, other = NEVER, ... } =>
                                                                          let val t = List.map (tryUnpackParam (ctx, #usage usage)) params
                                                                          in if List.exists (fn ELIMINATE => true | UNPACK _ => true | KEEP => false) t then
                                                                                 SOME t
                                                                             else
                                                                                 NONE
                                                                          end
                                                                        | _ => NONE
                                  in case shouldTransformParams of
                                         SOME paramTransforms =>
                                         let val () = #simplificationOccurred ctx := true
                                             val params' = ListPair.foldrEq (fn (p, KEEP, acc) => p :: acc
                                                                            | (p, ELIMINATE, acc) => acc
                                                                            | (p, UNPACK fields, acc) => List.map #1 fields @ acc
                                                                            ) [] (params, paramTransforms)
                                             val env' = ListPair.foldlEq (fn (p, UNPACK fields, env) => TypedSyntax.VIdMap.insert (env, p, { exp = SOME (C.Record (List.foldl (fn ((fieldVar, label), map) => Syntax.LabelMap.insert (map, label, C.Var fieldVar)) Syntax.LabelMap.empty fields)), isDiscardableFunction = false })
                                                                         | (p, KEEP, env) => env
                                                                         | (p, ELIMINATE, env) => env
                                                                         ) env (params, paramTransforms)
                                             val body = simplifyCExp (ctx, env', cenv, subst, csubst, usage, body)
                                             val exp = C.Abs { contParam = contParam, params = params', body = body }
                                             val result' = renewVId (ctx, result)
                                             val wrapperBody = let val k = genContSym ctx
                                                                   val params' = List.map (fn p => renewVId (ctx, p)) params
                                                                   val (decs, args) = ListPair.foldrEq (fn (p, KEEP, (decs, args)) => (decs, C.Var p :: args)
                                                                                                       | (p, ELIMINATE, acc) => acc
                                                                                                       | (p, UNPACK fields, (decs, args)) =>
                                                                                                         List.foldr (fn ((v, label), (decs, args)) =>
                                                                                                                        let val dec = C.ValDec { exp = C.Projection { label = label, record = C.Var p, fieldTypes = Syntax.LabelMap.empty (* dummy *) }
                                                                                                                                               , result = SOME v
                                                                                                                                               }
                                                                                                                        in (dec :: decs, C.Var v :: args)
                                                                                                                        end
                                                                                                                    ) (decs, args) fields
                                                                                                       ) ([], []) (params', paramTransforms)
                                                               in C.Abs { contParam = k
                                                                        , params = params'
                                                                        , body = C.Let { decs = vector decs
                                                                                       , cont = C.App { applied = C.Var result'
                                                                                                      , cont = k
                                                                                                      , args = args
                                                                                                      }
                                                                                       }
                                                                        }
                                                               end
                                             val env = TypedSyntax.VIdMap.insert (env, result, { exp = SOME wrapperBody, isDiscardableFunction = isDiscardableExp (env, body) })
                                             val dec = C.ValDec { exp = exp
                                                                , result = SOME result'
                                                                }
                                         in (env, cenv, subst, csubst, dec :: acc)
                                         end
                                       | NONE => let val body = simplifyCExp (ctx, env, cenv, subst, csubst, usage, body)
                                                     val exp = C.Abs { contParam = contParam, params = params, body = body }
                                                     val env = if isSmall orelse sizeOfCExp (body, 10) >= 0 then (* Inline small functions *)
                                                                   TypedSyntax.VIdMap.insert (env, result, { exp = SOME exp, isDiscardableFunction = isDiscardableExp (env, body) })
                                                               else
                                                                   TypedSyntax.VIdMap.insert (env, result, { exp = NONE, isDiscardableFunction = isDiscardableExp (env, body) })
                                                     val dec = C.ValDec { exp = exp
                                                                        , result = SOME result
                                                                        }
                                                 in (env, cenv, subst, csubst, dec :: acc)
                                                 end
                                  end
                        )
                      | _ => (case (C.isDiscardable exp, result) of
                                  (true, NONE) => (env, cenv, subst, csubst, acc)
                                | (_, SOME result) => let val dec = C.ValDec { exp = exp
                                                                             , result = SOME result
                                                                             }
                                                          val env = TypedSyntax.VIdMap.insert (env, result, { exp = SOME exp, isDiscardableFunction = false })
                                                      in (env, cenv, subst, csubst, dec :: acc)
                                                      end
                                | (false, NONE) => let val dec = C.ValDec { exp = exp
                                                                          , result = NONE
                                                                          }
                                                   in (env, cenv, subst, csubst, dec :: acc)
                                                   end
                             )
                 end
          end
        | C.RecDec defs =>
          if List.exists (fn (f, _, _, _) => CpsDeadCodeAnalysis.isUsed (#dead_code_analysis usage, f)) defs then
              let fun transform ((f, k, params, body), (env, acc))
                      = let val shouldTransformParams = case CpsUsageAnalysis.getValueUsage (#usage usage, f) of
                                                            { call = _, project = NEVER, other = NEVER, ... } =>
                                                            (case CpsUsageAnalysis.getValueUsage (#rec_usage usage, f) of
                                                                 { call = _, project = NEVER, other = NEVER, ... } =>
                                                                 let val t = List.map (tryUnpackParam (ctx, #usage usage)) params
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
                               let val () = #simplificationOccurred ctx := true
                                   val params' = ListPair.foldrEq (fn (p, KEEP, acc) => p :: acc
                                                                  | (p, ELIMINATE, acc) => acc
                                                                  | (p, UNPACK fields, acc) => List.map #1 fields @ acc
                                                                  ) [] (params, paramTransforms)
                                   val env = ListPair.foldlEq (fn (p, UNPACK fields, env) => TypedSyntax.VIdMap.insert (env, p, { exp = SOME (C.Record (List.foldl (fn ((fieldVar, label), map) => Syntax.LabelMap.insert (map, label, C.Var fieldVar)) Syntax.LabelMap.empty fields)), isDiscardableFunction = false })
                                                              | (p, KEEP, env) => env
                                                              | (p, ELIMINATE, env) => env
                                                              ) env (params, paramTransforms)
                                   val name' = renewVId (ctx, f)
                                   val wrapper = let val k = genContSym ctx
                                                     val params' = List.map (fn p => renewVId (ctx, p)) params
                                                     val (decs, args) = ListPair.foldrEq (fn (p, KEEP, (decs, args)) => (decs, C.Var p :: args)
                                                                                         | (p, ELIMINATE, acc) => acc
                                                                                         | (p, UNPACK fields, (decs, args)) =>
                                                                                           List.foldr (fn ((v, label), (decs, args)) =>
                                                                                                          let val v = renewVId (ctx, v)
                                                                                                              val dec = C.ValDec { exp = C.Projection { label = label, record = C.Var p, fieldTypes = Syntax.LabelMap.empty (* dummy *) }
                                                                                                                                 , result = SOME v
                                                                                                                                 }
                                                                                                          in (dec :: decs, C.Var v :: args)
                                                                                                          end
                                                                                                      ) (decs, args) fields
                                                                                         ) ([], []) (params', paramTransforms)
                                                 in C.Abs { contParam = k
                                                          , params = params'
                                                          , body = C.Let { decs = vector decs
                                                                         , cont = C.App { applied = C.Var name'
                                                                                        , cont = k
                                                                                        , args = args
                                                                                        }
                                                                         }
                                                          }
                                                 end
                                   val env = TypedSyntax.VIdMap.insert (env, f, { exp = SOME wrapper, isDiscardableFunction = false })
                               in (env, (name', k, params', body) :: acc)
                               end
                             | NONE => (env, (f, k, params, body) :: acc)
                        end
                  val (env, defs) = List.foldr transform (env, []) defs
                  val defs = List.map (fn (f, k, params, body) => (f, k, params, simplifyCExp (ctx, env, cenv, subst, csubst, usage, body))) defs
                  fun tryConvertToLoop (def as (f, k, params, body))
                      = if C.CVarSet.member (#returnConts (CpsUsageAnalysis.getValueUsage (#rec_usage usage, f)), k) then
                            let val loop = genContSym ctx
                                val params' = List.map (fn v => renewVId (ctx, v)) params
                                val body' = C.recurseCExp (fn call as C.App { applied = C.Var applied, cont, args } =>
                                                              if applied = f andalso cont = k then
                                                                  C.AppCont { applied = loop, args = args }
                                                              else
                                                                  call
                                                          | c => c
                                                          ) body
                                val body'' = C.Let { decs = vector [C.RecContDec [(loop, List.map SOME params, body')]]
                                                   , cont = C.AppCont { applied = loop, args = List.map C.Var params' }
                                                   }
                            in (f, k, params', body'')
                            end
                        else
                            def
                  val defs = List.map tryConvertToLoop defs
                  val defined = List.foldl (fn ((f, _, _, _), set) => TypedSyntax.VIdSet.add (set, f)) TypedSyntax.VIdSet.empty defs
                  val map = List.foldl (fn (def as (f, _, _, body), map) =>
                                           TypedSyntax.VIdMap.insert (map, f, { def = def
                                                                              , dests = TypedSyntax.VIdSet.intersection (C.freeVarsInExp (TypedSyntax.VIdSet.empty, body, TypedSyntax.VIdSet.empty), defined)
                                                                              }
                                                                     )
                                       ) TypedSyntax.VIdMap.empty defs
                  val sccs = TypedSyntax.VIdSCC.components (#dests, map)
                  val decs = List.foldl (fn (scc, decs) =>
                                            let val dec = case TypedSyntax.VIdSet.listItems scc of
                                                              [vid] => let val { def as (f, k, params, body), dests } = TypedSyntax.VIdMap.lookup (map, vid)
                                                                       in if TypedSyntax.VIdSet.member (dests, vid) then
                                                                              C.RecDec [def]
                                                                          else
                                                                              C.ValDec { exp = C.Abs { contParam = k, params = params, body = body }, result = SOME f }
                                                                       end
                                                            | scc => C.RecDec (List.map (fn vid => #def (TypedSyntax.VIdMap.lookup (map, vid))) scc)
                                            in dec :: decs
                                            end
                                        ) acc sccs
              in (env, cenv, subst, csubst, decs)
              end
          else
              ( #simplificationOccurred ctx := true
              ; (env, cenv, subst, csubst, acc)
              )
        | C.ContDec { name, params, body } =>
          (case CpsUsageAnalysis.getContUsage (#cont_usage usage, name) of
               { direct = NEVER, indirect = NEVER } =>
               ( #simplificationOccurred ctx := true
               ; (env, cenv, subst, csubst, acc)
               )
             | { direct = direct_usage, indirect = indirect_usage } =>
               if direct_usage = ONCE andalso indirect_usage = NEVER andalso ((case appliedCont of SOME c => c = name | NONE => false) orelse sizeOfCExp (body, 10) >= 0) then (* Inline small continuations *)
                   let val () = #simplificationOccurred ctx := true
                       val body = simplifyCExp (ctx, env, cenv, subst, csubst, usage, body)
                       val cenv = C.CVarMap.insert (cenv, name, (params, SOME body))
                   in (env, cenv, subst, csubst, acc)
                   end
               else
                   let val isVerySmall = sizeOfCExp (body, 3) >= 0
                       val shouldTransformParams = if not isVerySmall andalso indirect_usage = NEVER then
                                                       let val t = List.map (tryUnpackContParam (ctx, #usage usage)) params
                                                       in if List.exists (fn ELIMINATE => true | UNPACK _ => true | KEEP => false) t then
                                                              SOME t
                                                          else
                                                              NONE
                                                       end
                                                   else
                                                       NONE
                   in case shouldTransformParams of
                          SOME paramTransforms =>
                          let val () = #simplificationOccurred ctx := true
                              val params' = ListPair.foldrEq (fn (SOME p, KEEP, acc) => p :: acc
                                                             | (SOME p, ELIMINATE, acc) => acc
                                                             | (SOME p, UNPACK fields, acc) => List.map #1 fields @ acc
                                                             | (NONE, _, acc) => acc
                                                             ) [] (params, paramTransforms)
                              val env' = ListPair.foldlEq (fn (SOME p, UNPACK fields, env) => TypedSyntax.VIdMap.insert (env, p, { exp = SOME (C.Record (List.foldl (fn ((fieldVar, label), map) => Syntax.LabelMap.insert (map, label, C.Var fieldVar)) Syntax.LabelMap.empty fields)), isDiscardableFunction = false })
                                                          | (SOME p, KEEP, env) => env
                                                          | (SOME p, ELIMINATE, env) => env
                                                          | (NONE, _, env) => env
                                                          ) env (params, paramTransforms)
                              val body = simplifyCExp (ctx, env', cenv, subst, csubst, usage, body)
                              val name' = renewCVar (ctx, name)
                              val wrapperBody = let val params' = List.map (Option.map (fn p => renewVId (ctx, p))) params
                                                    val (decs, args) = ListPair.foldrEq (fn (SOME p, KEEP, (decs, args)) => (decs, C.Var p :: args)
                                                                                        | (SOME p, ELIMINATE, acc) => acc
                                                                                        | (SOME p, UNPACK fields, (decs, args)) =>
                                                                                          List.foldr (fn ((v, label), (decs, args)) =>
                                                                                                         let val v = renewVId (ctx, v)
                                                                                                             val dec = C.ValDec { exp = C.Projection { label = label, record = C.Var p, fieldTypes = Syntax.LabelMap.empty (* dummy *) }
                                                                                                                                , result = SOME v
                                                                                                                                }
                                                                                                         in (dec :: decs, C.Var v :: args)
                                                                                                         end
                                                                                                     ) (decs, args) fields
                                                                                        | (NONE, _, acc) => acc
                                                                                        ) ([], []) (params', paramTransforms)
                                                in (params', SOME (C.Let { decs = vector decs
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
                          let val body = simplifyCExp (ctx, env, cenv, subst, csubst, usage, body)
                              val params = List.map (fn SOME p => (case CpsUsageAnalysis.getValueUsage (#usage usage, p) of
                                                                       { call = NEVER, project = NEVER, other = NEVER, ... } => NONE
                                                                     | _ => SOME p
                                                                  )
                                                    | NONE => NONE
                                                    ) params
                              val cenv = if isVerySmall then (* Inline very small continuations *)
                                             C.CVarMap.insert (cenv, name, (params, SOME body))
                                         else
                                             C.CVarMap.insert (cenv, name, (params, NONE))
                              val dec = C.ContDec { name = name
                                                  , params = params
                                                  , body = body
                                                  }
                          in (env, cenv, subst, csubst, dec :: acc)
                          end
                   end
          )
        | C.RecContDec defs =>
          if List.all (fn (f, _, _) => CpsUsageAnalysis.getContUsage (#cont_usage usage, f) = { direct = NEVER, indirect = NEVER }) defs then
              ( #simplificationOccurred ctx := true
              ; (env, cenv, subst, csubst, acc)
              )
          else
              let fun transform (name, params, body)
                      = let val shouldTransformParams = if #indirect (CpsUsageAnalysis.getContUsage (#cont_usage usage, name)) = NEVER then
                                                            let val t = List.map (tryUnpackContParam (ctx, #usage usage)) params
                                                            in if List.exists (fn ELIMINATE => true | UNPACK _ => true | KEEP => false) t then
                                                                   SOME t
                                                               else
                                                                   NONE
                                                            end
                                                        else
                                                            NONE
                        in case shouldTransformParams of
                               SOME paramTransforms =>
                               let val () = #simplificationOccurred ctx := true
                                   val params' = ListPair.foldrEq (fn (SOME p, KEEP, acc) => p :: acc
                                                                  | (SOME p, ELIMINATE, acc) => acc
                                                                  | (SOME p, UNPACK fields, acc) => List.map #1 fields @ acc
                                                                  | (NONE, _, acc) => acc
                                                                  ) [] (params, paramTransforms)
                                   val env' = ListPair.foldlEq (fn (SOME p, UNPACK fields, env) => TypedSyntax.VIdMap.insert (env, p, { exp = SOME (C.Record (List.foldl (fn ((fieldVar, label), map) => Syntax.LabelMap.insert (map, label, C.Var fieldVar)) Syntax.LabelMap.empty fields)), isDiscardableFunction = false })
                                                               | (SOME p, KEEP, env) => env
                                                               | (SOME p, ELIMINATE, env) => env
                                                               | (NONE, _, env) => env
                                                               ) env (params, paramTransforms)
                                   val name' = renewCVar (ctx, name)
                                   val wrapper = let val params' = List.map (Option.map (fn p => renewVId (ctx, p))) params
                                                     val (decs, args) = ListPair.foldrEq (fn (SOME p, KEEP, (decs, args)) => (decs, C.Var p :: args)
                                                                                         | (SOME p, ELIMINATE, acc) => acc
                                                                                         | (SOME p, UNPACK fields, (decs, args)) =>
                                                                                           List.foldr (fn ((v, label), (decs, args)) =>
                                                                                                          let val v = renewVId (ctx, v)
                                                                                                              val dec = C.ValDec { exp = C.Projection { label = label, record = C.Var p, fieldTypes = Syntax.LabelMap.empty (* dummy *) }
                                                                                                                                 , result = SOME v
                                                                                                                                 }
                                                                                                          in (dec :: decs, C.Var v :: args)
                                                                                                          end
                                                                                                      ) (decs, args) fields
                                                                                         | (NONE, _, acc) => acc
                                                                                         ) ([], []) (params', paramTransforms)
                                                 in (params', SOME (C.Let { decs = vector decs
                                                                          , cont = C.AppCont { applied = name'
                                                                                             , args = args
                                                                                             }
                                                                          }
                                                                   )
                                                    )
                                                 end
                               in { origName = name, body = body, newName = name', newParams = List.map SOME params', env = env', inline = wrapper }
                               end
                             | NONE => { origName = name, body = body, env = env, newName = name, newParams = params, inline = (params, NONE) }
                        end
                  val defs' = List.map transform defs
                  val cenv = List.foldl (fn ({ origName, inline, ... }, cenv) => C.CVarMap.insert (cenv, origName, inline)) cenv defs'
                  val dec = C.RecContDec (List.map (fn { newName, newParams, env, body, ... } => (newName, newParams, simplifyCExp (ctx, env, cenv, subst, csubst, usage, body))) defs')
              in (env, cenv, subst, csubst, dec :: acc)
              end
        | C.ESImportDec { pure, specs, moduleName } =>
          let val specs = List.filter (fn (_, vid) => CpsDeadCodeAnalysis.isUsed (#dead_code_analysis usage, vid)) specs
          in if pure andalso List.null specs then
                 (env, cenv, subst, csubst, acc)
             else
                 let val dec = C.ESImportDec { pure = pure, specs = specs, moduleName = moduleName }
                 in (env, cenv, subst, csubst, dec :: acc)
                 end
          end
and simplifyCExp (ctx : Context, env : value_info TypedSyntax.VIdMap.map, cenv : ((C.Var option) list * C.CExp option) C.CVarMap.map, subst : C.Value TypedSyntax.VIdMap.map, csubst : C.CVar C.CVarMap.map, usage, e)
    = case e of
          C.Let { decs, cont } =>
          let val appliedCont = case cont of
                                    C.AppCont { applied, args } => SOME applied
                                  | _ => NONE
              val (env, cenv, subst, csubst, revDecs) = Vector.foldl (simplifyDec (ctx, usage, appliedCont)) (env, cenv, subst, csubst, []) decs
          in CpsTransform.prependRevDecs (revDecs, simplifyCExp (ctx, env, cenv, subst, csubst, usage, cont))
          end
        | C.App { applied, cont, args } =>
          let val applied = substValue subst applied
              val cont = substCVar csubst cont
              val args = List.map (substValue subst) args
          in case applied of
                 C.Var applied =>
                 (case TypedSyntax.VIdMap.find (env, applied) of
                      SOME { exp = SOME (C.Abs { contParam, params, body }), ... } =>
                      let val () = #simplificationOccurred ctx := true
                          val subst = ListPair.foldlEq (fn (p, a, subst) => TypedSyntax.VIdMap.insert (subst, p, a)) subst (params, args)
                          val csubst = C.CVarMap.insert (csubst, contParam, cont)
                          val canOmitAlphaConversion = case CpsUsageAnalysis.getValueUsage (#usage usage, applied) of
                                                           { call = ONCE, project = NEVER, other = NEVER, returnConts = _, labels = _ } =>
                                                           (case CpsUsageAnalysis.getValueUsage (#rec_usage usage, applied) of
                                                                { call = NEVER, project = NEVER, other = NEVER, returnConts = _, labels = _ } => true
                                                              | _ => false
                                                           )
                                                         | _ => false
                      in if canOmitAlphaConversion then
                             substCExp (subst, csubst, body) (* no alpha conversion *)
                         else
                             alphaConvert (ctx, subst, csubst, body)
                      end
                    | SOME { exp, isDiscardableFunction = true } =>
                      (case C.CVarMap.find (cenv, cont) of
                           SOME (params, _) => if not (List.exists Option.isSome params) then
                                                   ( #simplificationOccurred ctx := true
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
          let val applied = substCVar csubst applied
              val args = List.map (substValue subst) args
          in case C.CVarMap.find (cenv, applied) of
                 SOME (params, SOME body) =>
                 let val () = #simplificationOccurred ctx := true
                     val subst = ListPair.foldlEq (fn (SOME p, a, subst) => TypedSyntax.VIdMap.insert (subst, p, a) | (NONE, _, subst) => subst) subst (params, args)
                     val canOmitAlphaConversion = case CpsUsageAnalysis.getContUsage (#cont_usage usage, applied) of
                                                      { direct = ONCE, indirect = NEVER } => (case CpsUsageAnalysis.getContUsage (#cont_rec_usage usage, applied) of
                                                                                                  { direct = NEVER, indirect = NEVER } => true
                                                                                                | _ => false
                                                                                             )
                                                    | _ => false
                 in if canOmitAlphaConversion then
                        substCExp (subst, csubst, body) (* no alpha conversion *)
                    else
                        alphaConvert (ctx, subst, csubst, body)
                 end
               | _ => C.AppCont { applied = applied, args = args }
          end
        | C.If { cond, thenCont, elseCont } =>
          (case substValue subst cond of
               C.BoolConst true => (#simplificationOccurred ctx := true; simplifyCExp (ctx, env, cenv, subst, csubst, usage, thenCont))
             | C.BoolConst false => (#simplificationOccurred ctx := true; simplifyCExp (ctx, env, cenv, subst, csubst, usage, elseCont))
             | cond => C.If { cond = cond
                            , thenCont = simplifyCExp (ctx, env, cenv, subst, csubst, usage, thenCont)
                            , elseCont = simplifyCExp (ctx, env, cenv, subst, csubst, usage, elseCont)
                            }
          )
        | C.Handle { body, handler = (e, h), successfulExitIn, successfulExitOut } =>
          C.Handle { body = simplifyCExp (ctx, env, C.CVarMap.empty (* do not inline across 'handle' *), subst, csubst, usage, body)
                   , handler = (e, simplifyCExp (ctx, env, cenv, subst, csubst, usage, h))
                   , successfulExitIn = successfulExitIn
                   , successfulExitOut = substCVar csubst successfulExitOut
                   }
        | C.Unreachable => e

fun prependDecs ([], cont) = cont
  | prependDecs (decs, C.Let { decs = decs', cont }) = C.Let { decs = Vector.fromList (decs @ Vector.foldr (op ::) [] decs'), cont = cont }
  | prependDecs (decs, cont) = C.Let { decs = Vector.fromList decs, cont = cont }
(* Eliminate assumeDiscardable *)
(* More sophisticated analysis is wanted. *)
fun finalizeDec ctx (dec, (decs, cont))
    = case dec of
          C.ValDec { exp = C.PrimOp { primOp = F.PrimCall Primitives.assumeDiscardable, tyargs, args = [f, arg] }, result = SOME result } =>
          let val name = genContSym ctx
          in ([C.ContDec { name = name, params = [SOME result], body = prependDecs (decs, cont) }], C.App { applied = f, cont = name, args = [arg] })
          end
        | C.ValDec { exp = C.PrimOp { primOp = F.PrimCall Primitives.assumeDiscardable, tyargs, args = _ }, result = _ } =>
          raise Fail "assumeDiscardable: invalid argument"
        | C.ValDec { exp = C.PrimOp _, result } => (dec :: decs, cont)
        | C.ValDec { exp = C.Record _, result } => (dec :: decs, cont)
        | C.ValDec { exp = C.ExnTag _, result } => (dec :: decs, cont)
        | C.ValDec { exp = C.Projection _, result } => (dec :: decs, cont)
        | C.ValDec { exp = C.Abs { contParam, params, body }, result } =>
          let val dec = C.ValDec { exp = C.Abs { contParam = contParam, params = params, body = finalizeCExp (ctx, body) }, result = result }
          in (dec :: decs, cont)
          end
        | C.RecDec defs =>
          let val dec = C.RecDec (List.map (fn (name, k, params, body) => (name, k, params, finalizeCExp (ctx, body))) defs)
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
  | finalizeCExp (ctx, e as C.App _) = e
  | finalizeCExp (ctx, e as C.AppCont _) = e
  | finalizeCExp (ctx, C.If { cond, thenCont, elseCont }) = C.If { cond = cond, thenCont = finalizeCExp (ctx, thenCont), elseCont = finalizeCExp (ctx, elseCont) }
  | finalizeCExp (ctx, C.Handle { body, handler = (e, h), successfulExitIn, successfulExitOut }) = C.Handle { body = finalizeCExp (ctx, body), handler = (e, finalizeCExp (ctx, h)), successfulExitIn = successfulExitIn, successfulExitOut = successfulExitOut }
  | finalizeCExp (ctx, e as C.Unreachable) = e
end
end;

structure CpsAnalyze :> sig
              type cont_map
              val escapes : cont_map * CSyntax.CVar -> bool
              val escapesTransitively : cont_map * CSyntax.CVar -> bool
              val contEscape : CSyntax.CVar * CSyntax.CExp -> cont_map
              end = struct
local structure F = FSyntax
      structure C = CSyntax
in
type cont_map = { escapes : bool, escapesTransitively : bool } CSyntax.CVarTable.hash_table
fun escapes (t : cont_map, v) = #escapes (C.CVarTable.lookup t v)
fun escapesTransitively (t : cont_map, v) = #escapesTransitively (C.CVarTable.lookup t v)
type table = { escapes : bool ref, escapesTransitively : bool ref, level : int, free : CSyntax.CVarSet.set } CSyntax.CVarTable.hash_table
fun direct (table : table, level, k, acc) = let val { escapes, escapesTransitively, level = level', free } = C.CVarTable.lookup table k
                                            in if level' < level then
                                                   C.CVarSet.add (acc, k)
                                               else
                                                   acc
                                            end
fun recEscape (table : table) k = let val { escapes, escapesTransitively, level, free } = C.CVarTable.lookup table k
                                  in if !escapesTransitively then
                                         ()
                                     else
                                         ( escapesTransitively := true
                                         ; C.CVarSet.app (recEscape table) free
                                         )
                                  end
fun escape (table : table, level, k, acc) = let val { escapes, escapesTransitively, level = level', free } = C.CVarTable.lookup table k
                                            in escapes := true
                                             ; if level' < level then
                                                   C.CVarSet.add (acc, k)
                                               else
                                                   acc
                                            end
fun goDec (table, level) (dec, acc)
    = case dec of
          C.ValDec { exp = C.Abs { contParam, params, body }, result } =>
          ( C.CVarTable.insert table (contParam, { escapes = ref false, escapesTransitively = ref false, level = 0, free = C.CVarSet.empty })
          ; go (table, 0, body, acc)
          )
        | C.ValDec { exp, result } => acc
        | C.RecDec defs => List.foldl (fn ((f, k, params, body), acc) =>
                                          ( C.CVarTable.insert table (k, { escapes = ref false, escapesTransitively = ref false, level = 0, free = C.CVarSet.empty })
                                          ; go (table, 0, body, acc)
                                          )
                                      ) acc defs
        | C.ContDec { name, params, body } =>
          let val free = go (table, level + 1, body, C.CVarSet.empty)
          in C.CVarTable.insert table (name, { escapes = ref false, escapesTransitively = ref false, level = level, free = free })
           ; C.CVarSet.union (acc, free)
          end
        | C.RecContDec defs =>
          ( List.app (fn (name, params, body) => C.CVarTable.insert table (name, { escapes = ref false, escapesTransitively = ref false, level = level, free = C.CVarSet.empty })) defs
          ; List.foldl (fn ((name, params, body), acc) =>
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
  | go (table, level, C.App { applied, cont, args }, acc) = escape (table, level, cont, acc)
  | go (table, level, C.AppCont { applied, args }, acc) = direct (table, level, applied, acc)
  | go (table, level, C.If { cond, thenCont, elseCont }, acc) = go (table, level, elseCont, go (table, level, thenCont, acc))
  | go (table, level, C.Handle { body, handler = (e, h), successfulExitIn, successfulExitOut }, acc)
    = let val free = go (table, level + 1, h, C.CVarSet.empty)
      in C.CVarTable.insert table (successfulExitIn, { escapes = ref false, escapesTransitively = ref false, level = level, free = C.CVarSet.singleton successfulExitOut })
       ; C.CVarSet.app (fn k => ignore (escape (table, level + 1, k, C.CVarSet.empty))) free
       ; go (table, level, body, C.CVarSet.union (acc, free))
      end
  | go (table, level, C.Unreachable, acc) = acc
fun contEscape (cont, cexp) = let val table = C.CVarTable.mkTable (1, C.InvalidCode "unbound continuation")
                              in C.CVarTable.insert table (cont, { escapes = ref false, escapesTransitively = ref false, level = 0, free = C.CVarSet.empty })
                               ; ignore (go (table, 0, cexp, C.CVarSet.empty))
                               ; C.CVarTable.appi (fn (k, { escapes = ref true, escapesTransitively = ref false, ... }) => recEscape table k
                                                  | _ => ()) table
                               ; C.CVarTable.map (fn { escapes, escapesTransitively, ... } => { escapes = !escapes, escapesTransitively = !escapesTransitively }) table
                              end
end (* local *)
end; (* structure CpsAnalyze *)
