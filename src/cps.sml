(*
 * Copyright (c) 2022 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure CSyntax = struct
type Var = TypedSyntax.VId
type Tag = string
structure CVar :> sig
              type t
              type ord_key = t
              val compare : ord_key * ord_key -> order
              val fromInt : int -> t
              val toInt : t -> int
          end = struct
type t = int
type ord_key = t
val compare = Int.compare
fun fromInt x = x
fun toInt x = x
end
type CVar = CVar.t
structure CVarSet = RedBlackSetFn (CVar)
structure CVarMap = RedBlackMapFn (CVar)
datatype Value = Var of Var
               | Unit
               | BoolConst of bool
               | Int32Const of Int32.int
               | IntInfConst of IntInf.int
               | Word32Const of Word32.word
               | CharConst of char
               | Char16Const of int
               | StringConst of int vector
               | String16Const of int vector
(*
               | RealConst of Numeric.float_notation
*)
datatype SimpleExp = PrimOp of { primOp : FSyntax.PrimOp, tyargs : FSyntax.Ty list, args : Value list }
                   | Record of Value Syntax.LabelMap.map (* non-empty record *)
                   | ExnTag of { name : string, payloadTy : FSyntax.Ty option }
                   | Projection of { label : Syntax.Label, record : Value, fieldTypes : FSyntax.Ty Syntax.LabelMap.map }
                   | Abs of { contParam : CVar, exnContParam : CVar, params : Var list, body : CExp } (* non-recursive function *)
     and CExp = Let of { exp : SimpleExp, result : Var option, cont : CExp, exnCont : CVar option }
              | App of { applied : Value, cont : CVar, exnCont : CVar, args : Value list } (* tail call *)
              | AppCont of { applied : CVar, args : Value list }
              | If of { cond : Value
                      , thenCont : CExp
                      , elseCont : CExp
                      }
              | LetRec of { defs : (Var * CVar * CVar * Var list * CExp) list, cont : CExp } (* recursive function *)
              | LetCont of { name : CVar, params : Var list, body : CExp, cont : CExp }
              | LetRecCont of { defs : (CVar * Var list * CExp) list, cont : CExp }
local structure F = FSyntax in
fun isDiscardable (PrimOp { primOp = F.IntConstOp _, ... }) = true
  | isDiscardable (PrimOp { primOp = F.WordConstOp _, ... }) = true
  | isDiscardable (PrimOp { primOp = F.RealConstOp _, ... }) = true
  | isDiscardable (PrimOp { primOp = F.StringConstOp _, ... }) = true
  | isDiscardable (PrimOp { primOp = F.CharConstOp _, ... }) = true
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
  | isDiscardable (PrimOp { primOp = F.PrimFnOp p, ... }) = Primitives.isDiscardable p
  | isDiscardable (PrimOp { primOp = F.JsCallOp, ... }) = false
  | isDiscardable (PrimOp { primOp = F.JsMethodOp, ... }) = false
  | isDiscardable (PrimOp { primOp = F.JsNewOp, ... }) = false
  | isDiscardable (Record _) = true
  | isDiscardable (ExnTag _) = true
  | isDiscardable (Projection _) = true
  | isDiscardable (Abs _) = true
end
end

structure CpsTransform = struct
local structure F = FSyntax
      structure C = CSyntax
in

type Context = { nextVId : int ref }

fun genContSym (ctx : Context) : CSyntax.CVar
    = let val n = !(#nextVId ctx)
          val _ = #nextVId ctx := n + 1
      in CSyntax.CVar.fromInt n
      end

fun genExnContSym (ctx : Context) : CSyntax.CVar
    = let val n = !(#nextVId ctx)
          val _ = #nextVId ctx := n + 1
      in CSyntax.CVar.fromInt n
      end

fun genSym (ctx : Context) = let val n = !(#nextVId ctx)
                                 val _ = #nextVId ctx := n + 1
                             in TypedSyntax.MkVId ("tmp", n)
                             end

(* mapCont : ('a * ('b -> 'r) -> 'r) -> 'a list -> ('b list -> 'r) -> 'r *)
fun mapCont f [] cont = cont []
  | mapCont f (x :: xs) cont = f (x, fn y => mapCont f xs (fn ys => cont (y :: ys)))

fun stripTyAbs (F.TyAbsExp (_, _, e)) = stripTyAbs e
  | stripTyAbs e = e

(* 'a -> 'b ~~> (cont : 'b -> 'ans, exh : exn -> 'ans, param : 'a) -> 'ans *)
(* continuation of 'a : (value : 'a) -> 'ans *)

datatype cont = REIFIED of C.CVar
              | META of C.Var option * (C.Value -> C.CExp)
fun reify (ctx, REIFIED k) f = f k
  | reify (ctx, META (hint, m)) f = let val k = genContSym ctx
                                        val x = case hint of
                                                    NONE => genSym ctx
                                                  | SOME x => x
                                    in C.LetCont { name = k
                                                 , params = [x]
                                                 , body = m (C.Var x)
                                                 , cont = f k
                                                 }
                                    end
fun apply (REIFIED k) arg = C.AppCont { applied = k, args = [arg] }
  | apply (META (_, m)) arg = m arg
(* transformX : Context * Value TypedSyntax.VIdMap.map -> F.Exp -> { exnCont : C.Var } -> cont -> C.Exp *)
fun transform (ctx, env) exp { exnCont, resultHint } k = transformX (ctx, env) exp { exnCont = exnCont } (META (resultHint, k))
and transformT (ctx, env) exp { exnCont } k = transformX (ctx, env) exp { exnCont = exnCont } (REIFIED k)
and transformX (ctx : Context, env) (exp : F.Exp) { exnCont : C.CVar } (k : cont) : C.CExp
    = case exp of
           F.PrimExp (F.PrimFnOp Primitives.DelimCont_pushPrompt, tyargs, [p (* 'a prompt_tag *), f (* unit -> 'a *)]) =>
           transform (ctx, env) p { exnCont = exnCont, resultHint = NONE }
                     (fn p =>
                         transform (ctx, env) f { exnCont = exnCont, resultHint = NONE }
                                   (fn f =>
                                       reify (ctx, k)
                                             (fn kk =>
                                                 C.App { applied = C.Var InitialEnv.VId_DelimCont_pushPrompt, cont = kk, exnCont = exnCont, args = [p, f] }
                                             )
                                   )
                     )
         | F.PrimExp (F.PrimFnOp Primitives.DelimCont_withSubCont, tyargs, [p (* 'b prompt_tag *), f (* ('a,'b) subcont -> 'b *)]) =>
           transform (ctx, env) p { exnCont = exnCont, resultHint = NONE }
                     (fn p =>
                         transform (ctx, env) f { exnCont = exnCont, resultHint = NONE }
                                   (fn f =>
                                       reify (ctx, k)
                                             (fn kk =>
                                                 C.App { applied = C.Var InitialEnv.VId_DelimCont_withSubCont, cont = kk, exnCont = exnCont, args = [p, f] }
                                             )
                               )
                 )
         | F.PrimExp (F.PrimFnOp Primitives.DelimCont_pushSubCont, tyargs, [subcont (* ('a,'b) subcont *), f (* unit -> 'a *)]) =>
           transform (ctx, env) subcont { exnCont = exnCont, resultHint = NONE }
                     (fn subcont =>
                         transform (ctx, env) f { exnCont = exnCont, resultHint = NONE }
                                   (fn f =>
                                       reify (ctx, k)
                                             (fn kk =>
                                                 C.App { applied = C.Var InitialEnv.VId_DelimCont_pushSubCont, cont = kk, exnCont = exnCont, args = [subcont, f] }
                                             )
                               )
                 )
         | F.PrimExp (F.PrimFnOp Primitives.Unsafe_cast, tyargs, [arg]) =>
           transformX (ctx, env) arg { exnCont = exnCont } k
         | F.PrimExp (primOp, tyargs, args) =>
           mapCont (fn (e, cont) => transform (ctx, env) e { exnCont = exnCont, resultHint = NONE } cont)
                   args
                   (fn args =>
                       case primOp of
                           F.IntConstOp x => (case tyargs of
                                                  [F.TyVar tv] => if TypedSyntax.eqUTyVar (tv, F.tyNameToTyVar Typing.primTyName_int) then
                                                                      apply k (C.Int32Const (Int32.fromLarge x))
                                                                  else if TypedSyntax.eqUTyVar (tv, F.tyNameToTyVar Typing.primTyName_intInf) then
                                                                      apply k (C.IntInfConst x)
                                                                  else
                                                                      raise Fail "IntConstOp: invalid type"
                                                | _ => raise Fail "IntConstOp: invalid type"
                                             )
                         | F.WordConstOp x => (case tyargs of
                                                   [F.TyVar tv] => if TypedSyntax.eqUTyVar (tv, F.tyNameToTyVar Typing.primTyName_word) then
                                                                       apply k (C.Word32Const (Word32.fromLargeInt x))
                                                                   else
                                                                       raise Fail "WordConstOp: invalid type"
                                                 | _ => raise Fail "WordConstOp: invalid type"
                                              )
                         | F.CharConstOp x => (case tyargs of
                                                   [F.TyVar tv] => if TypedSyntax.eqUTyVar (tv, F.tyNameToTyVar Typing.primTyName_char) then
                                                                       apply k (C.CharConst (Char.chr x))
                                                                   else if TypedSyntax.eqUTyVar (tv, F.tyNameToTyVar Typing.primTyName_char16) then
                                                                       apply k (C.Char16Const x)
                                                                   else
                                                                       raise Fail "CharConstOp: invalid type"
                                                 | _ => raise Fail "CharConstOp: invalid type"
                                              )
                         | F.StringConstOp x => (case tyargs of
                                                     [F.TyVar tv] => if TypedSyntax.eqUTyVar (tv, F.tyNameToTyVar Typing.primTyName_string) then
                                                                         apply k (C.StringConst x)
                                                                     else if TypedSyntax.eqUTyVar (tv, F.tyNameToTyVar Typing.primTyName_string16) then
                                                                         apply k (C.String16Const x)
                                                                     else
                                                                         raise Fail "StringConstOp: invalid type"
                                                   | _ => raise Fail "StringConstOp: invalid type"
                                                )
                         | _ => let val mayraise = case primOp of
                                                       F.IntConstOp _ => false
                                                     | F.WordConstOp _ => false
                                                     | F.RealConstOp _ => false
                                                     | F.StringConstOp _ => false
                                                     | F.CharConstOp _ => false
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
                                                     | F.PrimFnOp p => Primitives.mayRaise p
                                                     | F.JsCallOp => true
                                                     | F.JsMethodOp => true
                                                     | F.JsNewOp => true
                                    val returnsUnit = case primOp of
                                                          F.PrimFnOp Primitives.Ref_set => true
                                                        | F.PrimFnOp Primitives.Unsafe_Array_update => true
                                                        | F.PrimFnOp Primitives.Lua_set => true
                                                        | F.PrimFnOp Primitives.Lua_call0 => true
                                                        | F.PrimFnOp Primitives.JavaScript_set => true
                                                        | _ => false
                                    val exp = C.PrimOp { primOp = primOp, tyargs = tyargs, args = args }
                                in if returnsUnit then
                                       C.Let { exp = exp
                                             , result = NONE
                                             , cont = apply k C.Unit
                                             , exnCont = if mayraise then SOME exnCont else NONE
                                             }
                                   else
                                       let val result = case k of
                                                            META (SOME r, _) => r
                                                          | _ => genSym ctx
                                       in C.Let { exp = exp
                                                , result = SOME result
                                                , cont = apply k (C.Var result)
                                                , exnCont = if mayraise then SOME exnCont else NONE
                                                }
                                       end
                                end
                   )
         | F.VarExp vid => (case TypedSyntax.VIdMap.find (env, vid) of
                                SOME v => apply k v
                              | NONE => apply k (C.Var vid)
                           )
         | F.RecordExp [] => apply k C.Unit
         | F.RecordExp fields => mapCont (fn ((label, exp), cont) => transform (ctx, env) exp { exnCont = exnCont, resultHint = NONE } (fn v => cont (label, v)))
                                         fields
                                         (fn fields => let val result = case k of
                                                                            META (SOME r, _) => r
                                                                          | _ => genSym ctx
                                                       in C.Let { exp = C.Record (List.foldl Syntax.LabelMap.insert' Syntax.LabelMap.empty fields)
                                                                , result = SOME result
                                                                , cont = apply k (C.Var result)
                                                                , exnCont = NONE
                                                                }
                                                       end
                                         )
         | F.LetExp (F.ValDec (vid, _, exp1), exp2) =>
           transform (ctx, env) exp1 { exnCont = exnCont, resultHint = SOME vid }
                     (fn v =>
                         transformX (ctx, TypedSyntax.VIdMap.insert (env, vid, v)) exp2 { exnCont = exnCont } k
                     )
         | F.LetExp (F.RecValDec decs, exp2) =>
           C.LetRec { defs = List.map (fn (vid, _, exp1) =>
                                          let val contParam = genContSym ctx
                                              val exnContParam = genExnContSym ctx
                                          in case stripTyAbs exp1 of
                                                 F.FnExp (param, _, body) => (vid, contParam, exnContParam, [param], transformT (ctx, env) body { exnCont = exnContParam } contParam)
                                               | _ => raise Fail "RecValDec"
                                          end
                                      ) decs
                    , cont = transformX (ctx, env) exp2 { exnCont = exnCont } k
                    }
         | F.LetExp (F.UnpackDec (_, _, vid, _, exp1), exp2) =>
           transform (ctx, env) exp1 { exnCont = exnCont, resultHint = SOME vid }
                     (fn v =>
                         transformX (ctx, TypedSyntax.VIdMap.insert (env, vid, v)) exp2 { exnCont = exnCont } k
                     )
         | F.LetExp (F.IgnoreDec exp1, exp2) =>
           transform (ctx, env) exp1 { exnCont = exnCont, resultHint = NONE }
                     (fn _ =>
                         transformX (ctx, env) exp2 { exnCont = exnCont } k
                     )
         | F.LetExp (F.DatatypeDec _, exp) => transformX (ctx, env) exp { exnCont = exnCont } k
         | F.LetExp (F.ExceptionDec { name, tagName, payloadTy }, exp) =>
           C.Let { exp = C.ExnTag { name = name
                                  , payloadTy = payloadTy
                                  }
                 , result = SOME tagName
                 , cont = transformX (ctx, env) exp { exnCont = exnCont } k
                 , exnCont = NONE
                 }
         | F.LetExp (F.ExportValue _, _) => raise Fail "ExportValue in CPS: not supported"
         | F.LetExp (F.ExportModule _, _) => raise Fail "ExportModule in CPS: not supported"
         | F.LetExp (F.GroupDec (_, decs), exp) => transformX (ctx, env) (List.foldr F.LetExp exp decs) { exnCont = exnCont } k
         | F.AppExp (applied, arg) =>
           transform (ctx, env) applied { exnCont = exnCont, resultHint = NONE }
                     (fn f =>
                         transform (ctx, env) arg { exnCont = exnCont, resultHint = NONE }
                                   (fn v =>
                                       reify (ctx, k)
                                             (fn j =>
                                                 C.App { applied = f, cont = j, exnCont = exnCont, args = [v] }
                                             )
                                   )
                     )
         | F.HandleExp { body, exnName, handler } =>
           let val h' = genExnContSym ctx
           in reify (ctx, k)
                    (fn j => C.LetCont { name = h'
                                       , params = [exnName]
                                       , body = transformT (ctx, env) handler { exnCont = exnCont } j
                                       , cont = transformT (ctx, env) body { exnCont = h' } j
                                       }
                    )
           end
         | F.IfThenElseExp (e1, e2, e3) =>
           transform (ctx, env) e1 { exnCont = exnCont, resultHint = NONE }
                     (fn e1 =>
                         reify (ctx, k)
                               (fn j => C.If { cond = e1
                                             , thenCont = transformT (ctx, env) e2 { exnCont = exnCont } j
                                             , elseCont = transformT (ctx, env) e3 { exnCont = exnCont } j
                                             }
                                  )
                     )
         | F.CaseExp _ => raise Fail "CaseExp: not supported here"
         | F.FnExp (vid, _, body) => let val f = case k of
                                                     META (SOME f, _) => f
                                                   | _ => genSym ctx
                                         val kk = genContSym ctx
                                         val hh = genExnContSym ctx
                                     in C.Let { exp = C.Abs { contParam = kk
                                                            , exnContParam = hh
                                                            , params = [vid]
                                                            , body = transformT (ctx, env) body { exnCont = hh } kk
                                                            }
                                              , result = SOME f
                                              , cont = apply k (C.Var f)
                                              , exnCont = NONE
                                              }
                                     end
         | F.ProjectionExp { label, record, fieldTypes } =>
           transform (ctx, env) record { exnCont = exnCont, resultHint = NONE }
                     (fn record =>
                         let val x = case k of
                                         META (SOME x, _) => x
                                       | _ => genSym ctx
                         in C.Let { exp = C.Projection { label = label
                                                       , record = record
                                                       , fieldTypes = fieldTypes
                                                       }
                                  , result = SOME x
                                  , cont = apply k (C.Var x)
                                  , exnCont = NONE
                                  }
                         end
                     )
         | F.TyAbsExp (_, _, exp) => transformX (ctx, env) exp { exnCont = exnCont } k
         | F.TyAppExp (exp, _) => transformX (ctx, env) exp { exnCont = exnCont } k
         | F.PackExp { payloadTy, exp, packageTy } => transformX (ctx, env) exp { exnCont = exnCont } k
fun transformDecs (ctx : Context, env) ([] : F.Dec list) { exnCont : C.CVar } (k : C.CVar) : C.CExp
    = C.AppCont { applied = k, args = [] } (* apply continuation *)
  | transformDecs (ctx, env) (dec :: decs) { exnCont } k
    = (case dec of
           F.ValDec (vid, _, exp) => transform (ctx, env) exp { exnCont = exnCont, resultHint = SOME vid }
                                               (fn v => transformDecs (ctx, TypedSyntax.VIdMap.insert (env, vid, v)) decs { exnCont = exnCont } k)
         | F.RecValDec decs' => C.LetRec { defs = List.map (fn (vid, _, exp) =>
                                                               let val contParam = genContSym ctx
                                                                   val exnContParam = genExnContSym ctx
                                                               in case stripTyAbs exp of
                                                                      F.FnExp (param, _, body) => (vid, contParam, exnContParam, [param], transformT (ctx, env) body { exnCont = exnContParam } contParam)
                                                                    | _ => raise Fail "RecValDec"
                                                               end
                                                           ) decs'
                                         , cont = transformDecs (ctx, env) decs { exnCont = exnCont } k
                                         }
         | F.UnpackDec (_, _, vid, _, exp) => transform (ctx, env) exp { exnCont = exnCont, resultHint = SOME vid }
                                                        (fn v => transformDecs (ctx, TypedSyntax.VIdMap.insert (env, vid, v)) decs { exnCont = exnCont } k)
         | F.IgnoreDec exp => transform (ctx, env) exp { exnCont = exnCont, resultHint = NONE }
                                        (fn v => transformDecs (ctx, env) decs { exnCont = exnCont } k)
         | F.DatatypeDec _ => transformDecs (ctx, env) decs { exnCont = exnCont } k
         | F.ExceptionDec { name, tagName, payloadTy } => C.Let { exp = C.ExnTag { name = name
                                                                                 , payloadTy = payloadTy
                                                                                 }
                                                                , result = SOME tagName
                                                                , cont = transformDecs (ctx, env) decs { exnCont = exnCont } k
                                                                , exnCont = NONE
                                                                }
         | F.ExportValue _ => raise Fail "ExportValue in CPS: not supported"
         | F.ExportModule _ => raise Fail "ExportModule in CPS: not supported"
         | F.GroupDec (_, decs') => transformDecs (ctx, env) (decs' @ decs) { exnCont = exnCont } k
      )
end
end;

structure CpsSimplify = struct
local structure F = FSyntax
      structure C = CSyntax
in
type Context = { nextVId : int ref }
fun genContSym (ctx : Context) : CSyntax.CVar
    = let val n = !(#nextVId ctx)
          val _ = #nextVId ctx := n + 1
      in CSyntax.CVar.fromInt n
      end
fun renewVId ({ nextVId } : Context, TypedSyntax.MkVId (name, _))
    = let val n = !nextVId
      in TypedSyntax.MkVId (name, n) before (nextVId := n + 1)
      end
fun renewCVar ({ nextVId } : Context, _ : C.CVar)
    = let val n = !nextVId
      in C.CVar.fromInt n before (nextVId := n + 1)
      end
fun sizeOfSimpleExp (C.PrimOp { primOp = _, tyargs = _, args }) = List.length args
  | sizeOfSimpleExp (C.Record fields) = Syntax.LabelMap.numItems fields
  | sizeOfSimpleExp (C.ExnTag _) = 1
  | sizeOfSimpleExp (C.Projection _) = 1
  | sizeOfSimpleExp (C.Abs { contParam, exnContParam, params, body }) = sizeOfCExp body
and sizeOfCExp (C.Let { exp, result = _, cont, exnCont = _ }) = sizeOfSimpleExp exp + sizeOfCExp cont
  | sizeOfCExp (C.App { applied, cont, exnCont, args }) = List.length args
  | sizeOfCExp (C.AppCont { applied, args }) = List.length args
  | sizeOfCExp (C.If { cond, thenCont, elseCont }) = 1 + sizeOfCExp thenCont + sizeOfCExp elseCont
  | sizeOfCExp (C.LetRec { defs, cont }) = List.foldl (fn ((_, _, _, _, body), acc) => acc + sizeOfCExp body) (sizeOfCExp cont) defs
  | sizeOfCExp (C.LetCont { name, params, body, cont }) = sizeOfCExp body + sizeOfCExp cont
  | sizeOfCExp (C.LetRecCont { defs, cont }) = List.foldl (fn ((_, _, body), acc) => acc + sizeOfCExp body) (sizeOfCExp cont) defs
datatype usage = NEVER | ONCE_AS_CALLEE | ONCE | MANY
datatype cont_usage = C_NEVER | C_ONCE | C_ONCE_DIRECT | C_MANY_DIRECT | C_MANY
fun usageInValue env (C.Var v) = (case TypedSyntax.VIdMap.find (env, v) of
                                      SOME r => (case !r of
                                                     NEVER => r := ONCE
                                                   | ONCE => r := MANY
                                                   | ONCE_AS_CALLEE => r := MANY
                                                   | MANY => ()
                                                )
                                    | NONE => ()
                                 )
  | usageInValue env C.Unit = ()
  | usageInValue env (C.BoolConst _) = ()
  | usageInValue env (C.Int32Const _) = ()
  | usageInValue env (C.IntInfConst _) = ()
  | usageInValue env (C.Word32Const _) = ()
  | usageInValue env (C.CharConst _) = ()
  | usageInValue env (C.Char16Const _) = ()
  | usageInValue env (C.StringConst _) = ()
  | usageInValue env (C.String16Const _) = ()
fun usageInValueAsCallee env (C.Var v) = (case TypedSyntax.VIdMap.find (env, v) of
                                              SOME r => (case !r of
                                                             NEVER => r := ONCE_AS_CALLEE
                                                           | ONCE => r := MANY
                                                           | ONCE_AS_CALLEE => r := MANY
                                                           | MANY => ()
                                                        )
                                            | NONE => ()
                                         )
  | usageInValueAsCallee env C.Unit = ()
  | usageInValueAsCallee env (C.BoolConst _) = ()
  | usageInValueAsCallee env (C.Int32Const _) = ()
  | usageInValueAsCallee env (C.IntInfConst _) = ()
  | usageInValueAsCallee env (C.Word32Const _) = ()
  | usageInValueAsCallee env (C.CharConst _) = ()
  | usageInValueAsCallee env (C.Char16Const _) = ()
  | usageInValueAsCallee env (C.StringConst _) = ()
  | usageInValueAsCallee env (C.String16Const _) = ()
fun usageContVar cenv (v : C.CVar) = (case C.CVarMap.find (cenv, v) of
                                          SOME r => (case !r of
                                                         C_NEVER => r := C_ONCE
                                                       | C_ONCE => r := C_MANY
                                                       | C_ONCE_DIRECT => r := C_MANY
                                                       | C_MANY_DIRECT => r := C_MANY
                                                       | C_MANY => ()
                                                    )
                                        | NONE => ()
                                     )
fun usageContVarDirect cenv (v : C.CVar) = (case C.CVarMap.find (cenv, v) of
                                                SOME r => (case !r of
                                                               C_NEVER => r := C_ONCE_DIRECT
                                                             | C_ONCE => r := C_MANY
                                                             | C_ONCE_DIRECT => r := C_MANY_DIRECT
                                                             | C_MANY_DIRECT => ()
                                                             | C_MANY => ()
                                                          )
                                              | NONE => ()
                                           )
local
    fun add (env, v) = if TypedSyntax.VIdMap.inDomain (env, v) then
                           raise Fail ("usageInCExp: duplicate name in AST: " ^ TypedSyntax.print_VId v)
                       else
                           TypedSyntax.VIdMap.insert (env, v, ref NEVER)
    fun addC (cenv, v) = if C.CVarMap.inDomain (cenv, v) then
                             raise Fail ("usageInCExp: duplicate continuation name in AST: " ^ Int.toString (C.CVar.toInt v))
                         else
                             C.CVarMap.insert (cenv, v, ref C_NEVER)
in
fun usageInSimpleExp (env, renv, cenv, crenv, C.PrimOp { primOp = _, tyargs = _, args }) = List.app (usageInValue (!env)) args
  | usageInSimpleExp (env, renv, cenv, crenv, C.Record fields) = Syntax.LabelMap.app (usageInValue (!env)) fields
  | usageInSimpleExp (env, renv, cenv, crenv, C.ExnTag { name = _, payloadTy = _ }) = ()
  | usageInSimpleExp (env, renv, cenv, crenv, C.Projection { label = _, record, fieldTypes = _ }) = usageInValue (!env) record
  | usageInSimpleExp (env, renv, cenv, crenv, C.Abs { contParam, exnContParam, params, body })
    = ( env := List.foldl (fn (p, e) => add (e, p)) (!env) params
      ; cenv := addC (addC (!cenv, contParam), exnContParam)
      ; usageInCExp (env, renv, cenv, crenv, body)
      )
and usageInCExp (env : ((usage ref) TypedSyntax.VIdMap.map) ref, renv, cenv : ((cont_usage ref) C.CVarMap.map) ref, crenv, cexp)
    = case cexp of
          C.Let { exp, result, cont, exnCont } =>
          ( usageInSimpleExp (env, renv, cenv, crenv, exp)
          ; Option.app (usageContVar (!cenv)) exnCont
          ; case result of
                SOME result => env := add (!env, result)
              | NONE => ()
          ; usageInCExp (env, renv, cenv, crenv, cont)
          )
        | C.App { applied, cont, exnCont, args } =>
          ( usageInValueAsCallee (!env) applied
          ; usageContVar (!cenv) cont
          ; usageContVar (!cenv) exnCont
          ; List.app (usageInValue (!env)) args
          )
        | C.AppCont { applied, args } =>
          ( usageContVarDirect (!cenv) applied
          ; List.app (usageInValue (!env)) args
          )
        | C.If { cond, thenCont, elseCont } =>
          ( usageInValue (!env) cond
          ; usageInCExp (env, renv, cenv, crenv, thenCont)
          ; usageInCExp (env, renv, cenv, crenv, elseCont)
          )
        | C.LetRec { defs, cont } =>
          let val recursiveEnv = List.foldl (fn ((f, _, _, _, _), m) => TypedSyntax.VIdMap.insert (m, f, ref NEVER)) TypedSyntax.VIdMap.empty defs
              val innerEnv = List.foldl (fn ((_, _, _, params, _), e) =>
                                            List.foldl (fn (p, e) => add (e, p)) e params
                                        ) (TypedSyntax.VIdMap.unionWith #2 (!env, recursiveEnv)) defs
              val cenv' = List.foldl (fn ((_, k, h, _, _), ce) =>
                                         addC (addC (ce, k), h)
                                     ) (!cenv) defs
          in env := innerEnv
           ; cenv := cenv'
           ; List.app (fn (_, _, _, _, body) => usageInCExp (env, renv, cenv, crenv, body)) defs
           ; renv := TypedSyntax.VIdMap.foldli (fn (f, r, m) => TypedSyntax.VIdMap.insert (m, f, r)) (!renv) recursiveEnv
           ; env := List.foldl (fn ((f, _, _, _, _), m) => TypedSyntax.VIdMap.insert (m, f, ref NEVER)) (!env) defs
           ; usageInCExp (env, renv, cenv, crenv, cont)
          end
        | C.LetCont { name, params, body, cont } =>
          ( env := List.foldl (fn (p, e) => add (e, p)) (!env) params
          ; usageInCExp (env, renv, cenv, crenv, body)
          ; cenv := addC (!cenv, name)
          ; usageInCExp (env, renv, cenv, crenv, cont)
          )
        | C.LetRecCont { defs, cont } =>
          let val recursiveCEnv = List.foldl (fn ((f, _, _), m) => C.CVarMap.insert (m, f, ref C_NEVER)) C.CVarMap.empty defs
              val env' = List.foldl (fn ((f, params, _), e) =>
                                        List.foldl (fn (p, e) => add (e, p)) e params
                                    ) (!env) defs
              val innerCEnv = C.CVarMap.unionWith #2 (!cenv, recursiveCEnv)
          in env := env'
           ; cenv := innerCEnv
           ; List.app (fn (_, _, body) => usageInCExp (env, renv, cenv, crenv, body)) defs
           ; crenv := C.CVarMap.foldli (fn (f, r, m) => C.CVarMap.insert (m, f, r)) (!crenv) recursiveCEnv
           ; cenv := List.foldl (fn ((f, _, _), m) => C.CVarMap.insert (m, f, ref C_NEVER)) (!cenv) defs
           ; usageInCExp (env, renv, cenv, crenv, cont)
          end
end
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
  | substSimpleExp (subst, csubst, C.Abs { contParam, exnContParam, params, body }) = C.Abs { contParam = contParam, exnContParam = exnContParam, params = params, body = substCExp (subst, csubst, body) }
and substCExp (subst : C.Value TypedSyntax.VIdMap.map, csubst : C.CVar C.CVarMap.map, C.Let { exp, result, cont, exnCont }) = C.Let { exp = substSimpleExp (subst, csubst, exp), result = result, cont = substCExp (subst, csubst, cont), exnCont = Option.map (substCVar csubst) exnCont }
  | substCExp (subst, csubst, C.App { applied, cont, exnCont, args }) = C.App { applied = substValue subst applied, cont = substCVar csubst cont, exnCont = substCVar csubst exnCont, args = List.map (substValue subst) args }
  | substCExp (subst, csubst, C.AppCont { applied, args }) = C.AppCont { applied = substCVar csubst applied, args = List.map (substValue subst) args }
  | substCExp (subst, csubst, C.If { cond, thenCont, elseCont }) = C.If { cond = substValue subst cond, thenCont = substCExp (subst, csubst, thenCont), elseCont = substCExp (subst, csubst, elseCont) }
  | substCExp (subst, csubst, C.LetRec { defs, cont }) = C.LetRec { defs = List.map (fn (f, k, h, params, body) => (f, k, h, params, substCExp (subst, csubst, body))) defs, cont = substCExp (subst, csubst, cont) }
  | substCExp (subst, csubst, C.LetCont { name, params, body, cont }) = C.LetCont { name = name, params = params, body = substCExp (subst, csubst, body), cont = substCExp (subst, csubst, cont) }
  | substCExp (subst, csubst, C.LetRecCont { defs, cont }) = C.LetRecCont { defs = List.map (fn (f, params, body) => (f, params, substCExp (subst, csubst, body))) defs, cont = substCExp (subst, csubst, cont) }
val substCExp = fn (subst, csubst, e) => if TypedSyntax.VIdMap.isEmpty subst andalso C.CVarMap.isEmpty csubst then
                                             e
                                         else
                                             substCExp (subst, csubst, e)
fun alphaConvertSimpleExp (ctx, subst, csubst, C.Abs { contParam, exnContParam, params, body })
    = let val (params', subst') = List.foldr (fn (p, (params', subst)) =>
                                                 let val p' = renewVId (ctx, p)
                                                 in (p' :: params', TypedSyntax.VIdMap.insert (subst, p, C.Var p'))
                                                 end
                                             ) ([], subst) params
          val contParam' = renewCVar (ctx, contParam)
          val exnContParam' = renewCVar (ctx, exnContParam)
          val csubst' = C.CVarMap.insert (C.CVarMap.insert (csubst, contParam, contParam'), exnContParam, exnContParam')
      in C.Abs { contParam = contParam'
               , exnContParam = exnContParam'
               , params = params'
               , body = alphaConvert (ctx, subst', csubst', body)
               }
      end
  | alphaConvertSimpleExp (ctx, subst, csubst, e) = substSimpleExp (subst, csubst, e)
and alphaConvert (ctx : Context, subst : C.Value TypedSyntax.VIdMap.map, csubst : C.CVar C.CVarMap.map, C.Let { exp, result = SOME result, cont, exnCont })
    = let val result' = renewVId (ctx, result)
          val subst' = TypedSyntax.VIdMap.insert (subst, result, C.Var result')
      in C.Let { exp = alphaConvertSimpleExp (ctx, subst, csubst, exp)
               , result = SOME result'
               , exnCont = Option.map (substCVar csubst) exnCont
               , cont = alphaConvert (ctx, subst', csubst, cont)
               }
      end
  | alphaConvert (ctx, subst, csubst, C.Let { exp, result = NONE, cont, exnCont })
    = C.Let { exp = alphaConvertSimpleExp (ctx, subst, csubst, exp)
            , result = NONE
            , exnCont = Option.map (substCVar csubst) exnCont
            , cont = alphaConvert (ctx, subst, csubst, cont)
            }
  | alphaConvert (ctx, subst, csubst, C.App { applied, cont, exnCont, args })
    = C.App { applied = substValue subst applied
            , cont = substCVar csubst cont
            , exnCont = substCVar csubst exnCont
            , args = List.map (substValue subst) args
            }
  | alphaConvert (ctx, subst, csubst, C.AppCont { applied, args })
    = C.AppCont { applied = substCVar csubst applied, args = List.map (substValue subst) args }
  | alphaConvert (ctx, subst, csubst, C.If { cond, thenCont, elseCont }) = C.If { cond = substValue subst cond, thenCont = alphaConvert (ctx, subst, csubst, thenCont), elseCont = alphaConvert (ctx, subst, csubst, elseCont) }
  | alphaConvert (ctx, subst, csubst, C.LetRec { defs, cont })
    = let val (subst, nameMap) = List.foldl (fn ((f, _, _, _, _), (subst, nameMap)) =>
                                                let val f' = renewVId (ctx, f)
                                                in (TypedSyntax.VIdMap.insert (subst, f, C.Var f'), TypedSyntax.VIdMap.insert (nameMap, f, f'))
                                                end
                                            ) (subst, TypedSyntax.VIdMap.empty) defs
      in C.LetRec { defs = List.map (fn (f, k, h, params, body) =>
                                        let val f' = TypedSyntax.VIdMap.lookup (nameMap, f)
                                            val k' = renewCVar (ctx, k)
                                            val h' = renewCVar (ctx, h)
                                            val (params', subst) = List.foldr (fn (p, (params', subst)) =>
                                                                                  let val p' = renewVId (ctx, p)
                                                                                  in (p' :: params', TypedSyntax.VIdMap.insert (subst, p, C.Var p'))
                                                                                  end
                                                                              ) ([], subst) params
                                            val csubst = C.CVarMap.insert (C.CVarMap.insert (csubst, k, k'), h, h')
                                        in (f', k', h', params', alphaConvert (ctx, subst, csubst, body))
                                        end
                                    ) defs
                  , cont = alphaConvert (ctx, subst, csubst, cont)
                  }
      end
  | alphaConvert (ctx, subst, csubst, C.LetCont { name, params, body, cont })
    = let val (params', subst') = List.foldr (fn (p, (params', subst)) =>
                                                 let val p' = renewVId (ctx, p)
                                                 in (p' :: params', TypedSyntax.VIdMap.insert (subst, p, C.Var p'))
                                                 end
                                             ) ([], subst) params
          val body = alphaConvert (ctx, subst', csubst, body)
          val name' = renewCVar (ctx, name)
          val csubst = C.CVarMap.insert (csubst, name, name')
      in C.LetCont { name = name'
                   , params = params'
                   , body = body
                   , cont = alphaConvert (ctx, subst, csubst, cont)
                   }
      end
  | alphaConvert (ctx, subst, csubst, C.LetRecCont { defs, cont })
    = let val (csubst, nameMap) = List.foldl (fn ((f, _, _), (csubst, nameMap)) =>
                                                 let val f' = renewCVar (ctx, f)
                                                 in (C.CVarMap.insert (csubst, f, f'), C.CVarMap.insert (nameMap, f, f'))
                                                 end
                                             ) (csubst, C.CVarMap.empty) defs
      in C.LetRecCont { defs = List.map (fn (f, params, body) =>
                                            let val f' = C.CVarMap.lookup (nameMap, f)
                                                val (params', subst) = List.foldr (fn (p, (params', subst)) =>
                                                                                      let val p' = renewVId (ctx, p)
                                                                                      in (p' :: params', TypedSyntax.VIdMap.insert (subst, p, C.Var p'))
                                                                                      end
                                                                                  ) ([], subst) params
                                            in (f', params', alphaConvert (ctx, subst, csubst, body))
                                            end
                                        ) defs
                      , cont = alphaConvert (ctx, subst, csubst, cont)
                      }
      end
datatype simplify_result = VALUE of C.Value
                         | SIMPLE_EXP of C.SimpleExp
                         | NOT_SIMPLIFIED
type value_info = { exp : C.SimpleExp option, isDiscardableFunction : bool }
fun isDiscardableExp (env : value_info TypedSyntax.VIdMap.map, C.Let { exp, result, cont, exnCont })
    = (case exp of
           C.Abs { body, ... } => let val env = case result of
                                                    SOME result => TypedSyntax.VIdMap.insert (env, result, { exp = NONE, isDiscardableFunction = isDiscardableExp (env, body) })
                                                  | NONE => env
                                  in isDiscardableExp (env, cont)
                                  end
         | _ => C.isDiscardable exp andalso isDiscardableExp (env, cont)
      )
  | isDiscardableExp (env, C.App { applied = C.Var applied, cont, exnCont, args })
    = (case TypedSyntax.VIdMap.find (env, applied) of
           SOME { isDiscardableFunction = true, ... } => true
         | _ => false
      )
  | isDiscardableExp (env, C.App _) = false (* should not occur *)
  | isDiscardableExp (env, C.AppCont _) = true
  | isDiscardableExp (env, C.If { cond, thenCont, elseCont }) = isDiscardableExp (env, thenCont) andalso isDiscardableExp (env, elseCont)
  | isDiscardableExp (env, C.LetRec { defs, cont }) = isDiscardableExp (env, cont)
  | isDiscardableExp (env, C.LetCont { name, params, body, cont }) = isDiscardableExp (env, cont) andalso isDiscardableExp (env, body)
  | isDiscardableExp (env, C.LetRecCont _) = false
fun simplifySimpleExp (env : value_info TypedSyntax.VIdMap.map, C.Record fields) = NOT_SIMPLIFIED
  | simplifySimpleExp (env, C.PrimOp { primOp = F.PrimFnOp Primitives.JavaScript_call, tyargs, args = [f, C.Var args] })
    = (case TypedSyntax.VIdMap.find (env, args) of
           SOME { exp = SOME (C.PrimOp { primOp = F.VectorOp, tyargs = _, args }), ... } => SIMPLE_EXP (C.PrimOp { primOp = F.JsCallOp, tyargs = [], args = f :: args })
         | _ => NOT_SIMPLIFIED
      )
  | simplifySimpleExp (env, C.PrimOp { primOp = F.PrimFnOp Primitives.JavaScript_method, tyargs, args = [obj, name, C.Var args] })
    = (case TypedSyntax.VIdMap.find (env, args) of
           SOME { exp = SOME (C.PrimOp { primOp = F.VectorOp, tyargs = _, args }), ... } => SIMPLE_EXP (C.PrimOp { primOp = F.JsMethodOp, tyargs = [], args = obj :: name :: args })
         | _ => NOT_SIMPLIFIED
      )
  | simplifySimpleExp (env, C.PrimOp { primOp = F.PrimFnOp Primitives.JavaScript_new, tyargs, args = [ctor, C.Var args] })
    = (case TypedSyntax.VIdMap.find (env, args) of
           SOME { exp = SOME (C.PrimOp { primOp = F.VectorOp, tyargs = _, args }), ... } => SIMPLE_EXP (C.PrimOp { primOp = F.JsNewOp, tyargs = [], args = ctor :: args })
         | _ => NOT_SIMPLIFIED
      )
  | simplifySimpleExp (env, C.PrimOp { primOp, tyargs, args }) = NOT_SIMPLIFIED (* TODO: constant folding *)
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
  | simplifySimpleExp (env, C.Abs { contParam, exnContParam, params, body }) = NOT_SIMPLIFIED (* TODO: Try eta conversion *)
and simplifyCExp (ctx, env, cenv, subst, csubst, usage, rusage, cusage, crusage, e)
    = case e of
          C.Let { exp, result, cont, exnCont } =>
          let val exp = substSimpleExp (subst, csubst, exp)
              val exnCont = Option.map (substCVar csubst) exnCont
          in case simplifySimpleExp (env, exp) of
                 VALUE v => let val subst = case result of
                                                SOME result => TypedSyntax.VIdMap.insert (subst, result, v)
                                              | NONE => subst
                            in simplifyCExp (ctx, env, cenv, subst, csubst, usage, rusage, cusage, crusage, cont)
                            end
               | simplified =>
                 let val exp = case simplified of
                                   SIMPLE_EXP exp => exp
                                 | _ => exp
                 in case (exp, result) of
                        (C.Abs { contParam, exnContParam, params, body }, SOME result) =>
                        (case TypedSyntax.VIdMap.find (usage, result) of
                             SOME (ref NEVER) => simplifyCExp (ctx, env, cenv, subst, csubst, usage, rusage, cusage, crusage, cont)
                           | SOME (ref ONCE_AS_CALLEE) => let val body = simplifyCExp (ctx, env, cenv, subst, csubst, usage, rusage, cusage, crusage, body)
                                                              val env = TypedSyntax.VIdMap.insert (env, result, { exp = SOME (C.Abs { contParam = contParam, exnContParam = exnContParam, params = params, body = body }), isDiscardableFunction = isDiscardableExp (env, body) })
                                                          in simplifyCExp (ctx, env, cenv, subst, csubst, usage, rusage, cusage, crusage, cont)
                                                          end
                           | _ => let val body = simplifyCExp (ctx, env, cenv, subst, csubst, usage, rusage, cusage, crusage, body)
                                      val exp = C.Abs { contParam = contParam, exnContParam = exnContParam, params = params, body = body }
                                      val env = if sizeOfCExp body <= 10 then (* Inline small functions *)
                                                    TypedSyntax.VIdMap.insert (env, result, { exp = SOME exp, isDiscardableFunction = isDiscardableExp (env, body) })
                                                else
                                                    TypedSyntax.VIdMap.insert (env, result, { exp = NONE, isDiscardableFunction = isDiscardableExp (env, body) })
                                  in C.Let { exp = exp
                                           , result = SOME result
                                           , cont = simplifyCExp (ctx, env, cenv, subst, csubst, usage, rusage, cusage, crusage, cont)
                                           , exnCont = NONE
                                           }
                                  end
                        )
                      | _ => let val result = case result of
                                                  s as SOME result => (case TypedSyntax.VIdMap.find (usage, result) of
                                                                           SOME (ref NEVER) => NONE
                                                                         | _ => s
                                                                      )
                                                | NONE => NONE
                             in case (C.isDiscardable exp, result) of
                                    (true, NONE) => simplifyCExp (ctx, env, cenv, subst, csubst, usage, rusage, cusage, crusage, cont)
                                  | (_, SOME result) => C.Let { exp = exp
                                                              , result = SOME result
                                                              , cont = simplifyCExp (ctx, TypedSyntax.VIdMap.insert (env, result, { exp = SOME exp, isDiscardableFunction = false }), cenv, subst, csubst, usage, rusage, cusage, crusage, cont)
                                                              , exnCont = exnCont
                                                              }
                                  | (false, NONE) => C.Let { exp = exp
                                                           , result = NONE
                                                           , cont = simplifyCExp (ctx, env, cenv, subst, csubst, usage, rusage, cusage, crusage, cont)
                                                           , exnCont = exnCont
                                                           }
                             end
                 end
          end
        | C.App { applied, cont, exnCont, args } =>
          let val applied = substValue subst applied
              val cont = substCVar csubst cont
              val exnCont = substCVar csubst exnCont
              val args = List.map (substValue subst) args
          in case applied of
                 C.Var applied =>
                 (case TypedSyntax.VIdMap.find (env, applied) of
                      SOME { exp = SOME (C.Abs { contParam, exnContParam, params, body }), ... } =>
                      let val subst = ListPair.foldlEq (fn (p, a, subst) => TypedSyntax.VIdMap.insert (subst, p, a)) subst (params, args)
                          val csubst = C.CVarMap.insert (C.CVarMap.insert (csubst, contParam, cont), exnContParam, exnCont)
                      in case TypedSyntax.VIdMap.find (usage, applied) of
                             SOME (ref ONCE_AS_CALLEE) => substCExp (subst, csubst, body) (* no alpha conversion *)
                           | _ => alphaConvert (ctx, subst, csubst, body)
                      end
                    | SOME { exp, isDiscardableFunction = true } =>
                      (case C.CVarMap.find (cenv, cont) of
                           SOME (params, _) => if List.all (fn p => case TypedSyntax.VIdMap.find (usage, p) of SOME (ref NEVER) => true | _ => false) params then
                                                   C.AppCont { applied = cont, args = List.map (fn _ => C.Unit (* dummy *)) params }
                                               else
                                                   C.App { applied = C.Var applied, cont = cont, exnCont = exnCont, args = args }
                         | _ => C.App { applied = C.Var applied, cont = cont, exnCont = exnCont, args = args }
                      )
                    | _ => C.App { applied = C.Var applied, cont = cont, exnCont = exnCont, args = args }
                 )
               | _ => C.App { applied = applied, cont = cont, exnCont = exnCont, args = args } (* should not occur *)
          end
        | C.AppCont { applied, args } =>
          let val applied = substCVar csubst applied
              val args = List.map (substValue subst) args
          in case C.CVarMap.find (cenv, applied) of
                 SOME (params, SOME body) =>
                 let val subst = ListPair.foldlEq (fn (p, a, subst) => TypedSyntax.VIdMap.insert (subst, p, a)) subst (params, args)
                 in case C.CVarMap.find (cusage, applied) of
                        SOME (ref C_ONCE_DIRECT) => substCExp (subst, csubst, body) (* no alpha conversion *)
                      | _ => alphaConvert (ctx, subst, csubst, body)
                 end
               | _ => C.AppCont { applied = applied, args = args }
          end
        | C.If { cond, thenCont, elseCont } =>
          C.If { cond = substValue subst cond
               , thenCont = simplifyCExp (ctx, env, cenv, subst, csubst, usage, rusage, cusage, crusage, thenCont)
               , elseCont = simplifyCExp (ctx, env, cenv, subst, csubst, usage, rusage, cusage, crusage, elseCont)
               }
        | C.LetRec { defs, cont } =>
          if List.exists (fn (f, _, _, _, _) => case TypedSyntax.VIdMap.find (usage, f) of SOME (ref NEVER) => false | _ => true) defs then
              C.LetRec { defs = List.map (fn (f, k, h, params, body) => (f, k, h, params, simplifyCExp (ctx, env, cenv, subst, csubst, usage, rusage, cusage, crusage, body))) defs
                       , cont = simplifyCExp (ctx, env, cenv, subst, csubst, usage, rusage, cusage, crusage, cont)
                       }
          else
              simplifyCExp (ctx, env, cenv, subst, csubst, usage, rusage, cusage, crusage, cont)
        | C.LetCont { name, params, body, cont } =>
          (case C.CVarMap.find (cusage, name) of
               SOME (ref C_NEVER) => simplifyCExp (ctx, env, cenv, subst, csubst, usage, rusage, cusage, crusage, cont)
             | SOME (ref C_ONCE_DIRECT) => let val body' = simplifyCExp (ctx, env, cenv, subst, csubst, usage, rusage, cusage, crusage, body)
                                               val cenv' = C.CVarMap.insert (cenv, name, (params, SOME body'))
                                           in simplifyCExp (ctx, env, cenv', subst, csubst, usage, rusage, cusage, crusage, cont)
                                           end
             | _ => let val body = simplifyCExp (ctx, env, cenv, subst, csubst, usage, rusage, cusage, crusage, body)
                        val cenv = if sizeOfCExp body <= 3 then (* Inline small continuations *)
                                       C.CVarMap.insert (cenv, name, (params, SOME body))
                                   else
                                       C.CVarMap.insert (cenv, name, (params, NONE))
                    in C.LetCont { name = name
                                 , params = params
                                 , body = body
                                 , cont = simplifyCExp (ctx, env, cenv, subst, csubst, usage, rusage, cusage, crusage, cont)
                                 }
                    end
          )
        | C.LetRecCont { defs, cont } =>
          if List.exists (fn (f, _, _) => case C.CVarMap.find (cusage, f) of SOME (ref C_NEVER) => false | _ => true) defs then
              let val cenv = List.foldl (fn ((f, params, body), cenv) => C.CVarMap.insert (cenv, f, (params, NONE))) cenv defs
              in C.LetRecCont { defs = List.map (fn (f, params, body) => (f, params, simplifyCExp (ctx, env, cenv, subst, csubst, usage, rusage, cusage, crusage, body))) defs
                              , cont = simplifyCExp (ctx, env, cenv, subst, csubst, usage, rusage, cusage, crusage, cont)
                              }
              end
          else
              simplifyCExp (ctx, env, cenv, subst, csubst, usage, rusage, cusage, crusage, cont)

(* Eliminate assumeDiscardable *)
fun finalizeCExp (ctx, C.Let { exp = C.PrimOp { primOp = F.PrimFnOp Primitives.assumeDiscardable, tyargs, args = [f, arg] }, result = SOME result, cont, exnCont = SOME exnCont })
    = let val name = genContSym ctx
      in C.LetCont { name = name
                   , params = [result]
                   , body = finalizeCExp (ctx, cont)
                   , cont = C.App { applied = f, cont = name, exnCont = exnCont, args = [arg] }
                   }
      end
  | finalizeCExp (ctx, C.Let { exp = C.PrimOp { primOp = F.PrimFnOp Primitives.assumeDiscardable, tyargs, args = _ }, result = _, cont, exnCont = _ }) = raise Fail "assumeDiscardable: invalid argument"
  | finalizeCExp (ctx, C.Let { exp as C.PrimOp { primOp, tyargs, args }, result, cont, exnCont }) = C.Let { exp = exp, result = result, cont = finalizeCExp (ctx, cont), exnCont = exnCont }
  | finalizeCExp (ctx, C.Let { exp as C.Record _, result, cont, exnCont }) = C.Let { exp = exp, result = result, cont = finalizeCExp (ctx, cont), exnCont = exnCont }
  | finalizeCExp (ctx, C.Let { exp as C.ExnTag _, result, cont, exnCont }) = C.Let { exp = exp, result = result, cont = finalizeCExp (ctx, cont), exnCont = exnCont }
  | finalizeCExp (ctx, C.Let { exp as C.Projection _, result, cont, exnCont }) = C.Let { exp = exp, result = result, cont = finalizeCExp (ctx, cont), exnCont = exnCont }
  | finalizeCExp (ctx, C.Let { exp = C.Abs { contParam, exnContParam, params, body }, result, cont, exnCont })
    = C.Let { exp = C.Abs { contParam = contParam, exnContParam = exnContParam, params = params, body = finalizeCExp (ctx, body) }
            , result = result
            , cont = finalizeCExp (ctx, cont)
            , exnCont = exnCont
            }
  | finalizeCExp (ctx, e as C.App _) = e
  | finalizeCExp (ctx, e as C.AppCont _) = e
  | finalizeCExp (ctx, C.If { cond, thenCont, elseCont }) = C.If { cond = cond, thenCont = finalizeCExp (ctx, thenCont), elseCont = finalizeCExp (ctx, elseCont) }
  | finalizeCExp (ctx, C.LetRec { defs, cont }) = C.LetRec { defs = List.map (fn (name, k, h, params, body) => (name, k, h, params, finalizeCExp (ctx, body))) defs, cont = finalizeCExp (ctx, cont) }
  | finalizeCExp (ctx, C.LetCont { name, params, body, cont }) = C.LetCont { name = name, params = params, body = finalizeCExp (ctx, body), cont = finalizeCExp (ctx, cont) }
  | finalizeCExp (ctx, C.LetRecCont { defs, cont }) = C.LetRecCont { defs = List.map (fn (name, params, body) => (name, params, finalizeCExp (ctx, body))) defs, cont = finalizeCExp (ctx, cont) }
end
end;

structure CpsAnalyze = struct
local structure F = FSyntax
      structure C = CSyntax
in
type env = { escapes : bool ref, level : int, outerDestinations : C.CVarSet.set } C.CVarMap.map
fun direct (env : env ref, level, k, acc) = case C.CVarMap.find (!env, k) of
                                                SOME { escapes, level = level', outerDestinations } => if level' < level then
                                                                                                           C.CVarSet.add (acc, k)
                                                                                                       else
                                                                                                           acc
                                              | NONE => acc
fun recEscape (env : env) k = case C.CVarMap.find (env, k) of
                                  SOME { escapes, level, outerDestinations } => if !escapes then
                                                                                    ()
                                                                                else
                                                                                    ( escapes := true
                                                                                    ; C.CVarSet.app (recEscape env) outerDestinations
                                                                                    )
                                | NONE => ()
fun escape (env : env ref, level, k, acc) = case C.CVarMap.find (!env, k) of
                                                SOME { escapes, level = level', outerDestinations } => ( recEscape (!env) k
                                                                                                       ; if level' < level then
                                                                                                             C.CVarSet.add (acc, k)
                                                                                                         else
                                                                                                             acc
                                                                                                       )
                                              | NONE => acc
fun go (env, level, C.Let { exp = C.Abs { contParam, exnContParam, params, body}, result = SOME result, cont, exnCont }, acc)
    = let val acc = case exnCont of
                        SOME exnCont => direct (env, level, exnCont, acc)
                      | NONE => acc
          val acc = go (env, 0, body, acc)
      in go (env, level, cont, acc)
      end
  | go (env, level, C.Let { exp, result, cont, exnCont }, acc) = let val acc = case exnCont of
                                                                                   SOME exnCont => direct (env, level, exnCont, acc)
                                                                                 | NONE => acc
                                                                 in go (env, level, cont, acc)
                                                                 end
  | go (env, level, C.App { applied, cont, exnCont, args }, acc) = escape (env, level, cont, escape (env, level, exnCont, acc))
  | go (env, level, C.AppCont { applied, args }, acc) = direct (env, level, applied, acc)
  | go (env, level, C.If { cond, thenCont, elseCont }, acc) = go (env, level, elseCont, go (env, level, thenCont, acc))
  | go (env, level, C.LetRec { defs, cont }, acc) = let val acc = List.foldl (fn ((f, k, h, params, body), acc) =>
                                                                                 go (env, 0, body, acc)
                                                                             ) acc defs
                                                    in go (env, level, cont, acc)
                                                    end
  | go (env, level, C.LetCont { name, params, body, cont }, acc) = let val outerDestinations = go (env, level + 1, body, C.CVarSet.empty)
                                                                   in env := C.CVarMap.insert (!env, name, { escapes = ref false, level = level, outerDestinations = outerDestinations })
                                                                    ; go (env, level, cont, C.CVarSet.union (acc, outerDestinations))
                                                                   end
  | go (env, level, C.LetRecCont { defs, cont }, acc) = let val () = env := List.foldl (fn ((name, params, body), env) => C.CVarMap.insert (env, name, { escapes = ref false, level = level, outerDestinations = C.CVarSet.empty })) (!env) defs
                                                            val outerDestinations = List.foldl (fn ((name, params, body), acc) => go (env, level + 1, body, acc)) C.CVarSet.empty defs
                                                        in env := List.foldl (fn ((name, params, body), env) => C.CVarMap.insert (env, name, { escapes = #escapes (C.CVarMap.lookup (env, name)), level = level, outerDestinations = outerDestinations })) (!env) defs
                                                         ; if List.exists (fn (name, _, _) => !(#escapes (C.CVarMap.lookup (!env, name)))) defs then
                                                               List.app (fn (name, _, _) => ignore (escape (env, level + 1, name, C.CVarSet.empty))) defs
                                                           else
                                                               ()
                                                         ; go (env, level, cont, C.CVarSet.union (acc, outerDestinations))
                                                        end
fun contEscape cexp = let val env = ref C.CVarMap.empty
                          val _ = go (env, 0, cexp, C.CVarSet.empty)
                      in C.CVarMap.map (fn { escapes, ... } => !escapes) (!env)
                      end
end (* local *)
end; (* structure CpsAnalyze *)
