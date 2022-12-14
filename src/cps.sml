(*
 * Copyright (c) 2022 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure CSyntax = struct
type Var = TypedSyntax.VId
type Tag = string
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
datatype CExp = Let of { exp : SimpleExp, result : Var, cont : CExp, exnCont : Value option }
              | App of { applied : Value, args : Value list } (* tail call *)
              | If of { cond : Value
                      , thenCont : CExp
                      , elseCont : CExp
                      }
              | Fn of { function : Var * Var list * CExp, cont : CExp } (* non-recursive function *)
              | Fix of { functions : (Var * Var list * CExp) list, cont : CExp } (* recursive function *)
              | PushPrompt of { promptTag : Value, f : Value, cont : Value, exnCont : Value }
              | WithSubCont of { promptTag : Value, f : Value, cont : Value, exnCont : Value }
              | PushSubCont of { subCont : Value, f : Value, cont : Value, exnCont : Value }
local structure F = FSyntax in
fun isDiscardable (PrimOp { primOp = F.IntConstOp _, ... }) = true
  | isDiscardable (PrimOp { primOp = F.WordConstOp _, ... }) = true
  | isDiscardable (PrimOp { primOp = F.RealConstOp _, ... }) = true
  | isDiscardable (PrimOp { primOp = F.StringConstOp _, ... }) = true
  | isDiscardable (PrimOp { primOp = F.CharConstOp _, ... }) = true
  | isDiscardable (PrimOp { primOp = F.RaiseOp _, ... }) = false
  | isDiscardable (PrimOp { primOp = F.ListOp, ... }) = true
  | isDiscardable (PrimOp { primOp = F.VectorOp, ... }) = true
  | isDiscardable (PrimOp { primOp = F.DataTagOp _, ... }) = true
  | isDiscardable (PrimOp { primOp = F.DataPayloadOp _, ... }) = true
  | isDiscardable (PrimOp { primOp = F.ExnPayloadOp, ... }) = true
  | isDiscardable (PrimOp { primOp = F.ConstructValOp _, ... }) = true
  | isDiscardable (PrimOp { primOp = F.ConstructValWithPayloadOp _, ... }) = true
  | isDiscardable (PrimOp { primOp = F.ConstructExnOp, ... }) = true
  | isDiscardable (PrimOp { primOp = F.ConstructExnWithPayloadOp, ... }) = true
  | isDiscardable (PrimOp { primOp = F.PrimFnOp p, ... }) = false (* TODO *)
  | isDiscardable (Record _) = true
  | isDiscardable (ExnTag _) = true
  | isDiscardable (Projection _) = true
end
end

structure CpsTransform = struct
local structure F = FSyntax
      structure C = CSyntax
in

type Context = { nextVId : int ref }

fun genContSym (ctx : Context) = let val n = !(#nextVId ctx)
                                     val _ = #nextVId ctx := n + 1
                                 in TypedSyntax.MkVId ("cont", n)
                                 end

fun genExnContSym (ctx : Context) = let val n = !(#nextVId ctx)
                                        val _ = #nextVId ctx := n + 1
                                    in TypedSyntax.MkVId ("exh", n)
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

datatype cont = REIFIED of C.Var
              | META of C.Var option * (C.Value -> C.CExp)
fun reify (ctx, REIFIED k) f = f k
  | reify (ctx, META (hint, m)) f = let val k = genContSym ctx
                                        val x = case hint of
                                                    NONE => genSym ctx
                                                  | SOME x => x
                                    in C.Fn { function = (k, [x], m (C.Var x)) (* letcont *)
                                            , cont = f k
                                            }
                                    end
fun apply (REIFIED k) arg = C.App { applied = C.Var k, args = [arg] }
  | apply (META (_, m)) arg = m arg
(* transformX : Context * Value TypedSyntax.VIdMap.map -> F.Exp -> { exnCont : C.Var } -> cont -> C.Exp *)
fun transform (ctx, env) exp { exnCont, resultHint } k = transformX (ctx, env) exp { exnCont = exnCont } (META (resultHint, k))
and transformT (ctx, env) exp { exnCont } k = transformX (ctx, env) exp { exnCont = exnCont } (REIFIED k)
and transformX (ctx : Context, env) (exp : F.Exp) { exnCont : C.Var } (k : cont) : C.CExp
    = case exp of
           F.PrimExp (F.PrimFnOp Primitives.DelimCont_pushPrompt, tyargs, [p (* 'a prompt_tag *), f (* unit -> 'a *)]) =>
           reify (ctx, k)
                 (fn kk =>
                     transform (ctx, env) p { exnCont = exnCont, resultHint = NONE }
                               (fn p =>
                                   transform (ctx, env) f { exnCont = exnCont, resultHint = NONE }
                                             (fn f =>
                                                 C.PushPrompt { promptTag = p
                                                              , f = f
                                                              , cont = C.Var kk
                                                              , exnCont = C.Var exnCont
                                                              }
                                             )
                               )
                 )
         | F.PrimExp (F.PrimFnOp Primitives.DelimCont_withSubCont, tyargs, [p (* 'b prompt_tag *), f (* ('a,'b) subcont -> 'b *)]) =>
           reify (ctx, k)
                 (fn kk =>
                     transform (ctx, env) p { exnCont = exnCont, resultHint = NONE }
                               (fn p =>
                                   transform (ctx, env) f { exnCont = exnCont, resultHint = NONE }
                                             (fn f =>
                                                 C.WithSubCont { promptTag = p
                                                               , f = f
                                                               , cont = C.Var kk
                                                               , exnCont = C.Var exnCont
                                                               }
                                             )
                               )
                 )
         | F.PrimExp (F.PrimFnOp Primitives.DelimCont_pushSubCont, tyargs, [subcont (* ('a,'b) subcont *), f (* unit -> 'a *)]) =>
           reify (ctx, k)
                 (fn kk =>
                     transform (ctx, env) subcont { exnCont = exnCont, resultHint = NONE }
                               (fn subcont =>
                                   transform (ctx, env) f { exnCont = exnCont, resultHint = NONE }
                                             (fn f =>
                                                 C.PushSubCont { subCont = subcont
                                                               , f = f
                                                               , cont = C.Var kk
                                                               , exnCont = C.Var exnCont
                                                               }
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
                         | _ => let val result = case k of
                                                     META (SOME r, _) => r
                                                   | _ => genSym ctx
                                    val mayraise = case primOp of
                                                       F.IntConstOp _ => false
                                                     | F.WordConstOp _ => false
                                                     | F.RealConstOp _ => false
                                                     | F.StringConstOp _ => false
                                                     | F.CharConstOp _ => false
                                                     | F.RaiseOp _ => true
                                                     | F.ListOp => false
                                                     | F.VectorOp => false
                                                     | F.DataTagOp _ => false
                                                     | F.DataPayloadOp _ => false
                                                     | F.ExnPayloadOp => false
                                                     | F.ConstructValOp _ => false
                                                     | F.ConstructValWithPayloadOp _ => false
                                                     | F.ConstructExnOp => false
                                                     | F.ConstructExnWithPayloadOp => false
                                                     | F.PrimFnOp p => Primitives.mayRaise p
                                in C.Let { exp = C.PrimOp { primOp = primOp, tyargs = tyargs, args = args }
                                         , result = result
                                         , cont = apply k (C.Var result)
                                         , exnCont = if mayraise then SOME (C.Var exnCont) else NONE
                                         }
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
                                                                , result = result
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
           C.Fix { functions = List.map (fn (vid, _, exp1) =>
                                            let val contParam = genContSym ctx
                                                val exnContParam = genExnContSym ctx
                                            in case stripTyAbs exp1 of
                                                   F.FnExp (param, _, body) => (vid, [contParam, exnContParam, param], transformT (ctx, env) body { exnCont = exnContParam } contParam)
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
                 , result = tagName
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
                                                 C.App { applied = f, args = [C.Var j, C.Var exnCont, v] }
                                             )
                                   )
                     )
         | F.HandleExp { body, exnName, handler } =>
           let val h' = genExnContSym ctx
           in reify (ctx, k)
                    (fn j => C.Fix { functions = [(h', [exnName], transformT (ctx, env) handler { exnCont = exnCont } j)]
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
                                     in C.Fn { function = (f, [kk, hh, vid], transformT (ctx, env) body { exnCont = hh } kk)
                                             , cont = apply k (C.Var f)
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
                                  , result = x
                                  , cont = apply k (C.Var x)
                                  , exnCont = NONE
                                  }
                         end
                     )
         | F.TyAbsExp (_, _, exp) => transformX (ctx, env) exp { exnCont = exnCont } k
         | F.TyAppExp (exp, _) => transformX (ctx, env) exp { exnCont = exnCont } k
         | F.PackExp { payloadTy, exp, packageTy } => transformX (ctx, env) exp { exnCont = exnCont } k
fun transformDecs (ctx : Context, env) ([] : F.Dec list) { exnCont : C.Var } (k : C.Var) : C.CExp
    = C.App { applied = C.Var k, args = [] } (* apply continuation *)
  | transformDecs (ctx, env) (dec :: decs) { exnCont } k
    = (case dec of
           F.ValDec (vid, _, exp) => transform (ctx, env) exp { exnCont = exnCont, resultHint = SOME vid }
                                               (fn v => transformDecs (ctx, TypedSyntax.VIdMap.insert (env, vid, v)) decs { exnCont = exnCont } k)
         | F.RecValDec decs' => C.Fix { functions = List.map (fn (vid, _, exp) =>
                                                                 let val contParam = genContSym ctx
                                                                     val exnContParam = genExnContSym ctx
                                                                 in case stripTyAbs exp of
                                                                        F.FnExp (param, _, body) => (vid, [contParam, exnContParam, param], transformT (ctx, env) body { exnCont = exnContParam } contParam)
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
                                                                , result = tagName
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
fun renewVId ({ nextVId } : Context, TypedSyntax.MkVId (name, _))
    = let val n = !nextVId
      in TypedSyntax.MkVId (name, n) before (nextVId := n + 1)
      end
fun sizeOfSimpleExp (C.PrimOp { primOp = _, tyargs = _, args }) = List.length args
  | sizeOfSimpleExp (C.Record fields) = Syntax.LabelMap.numItems fields
  | sizeOfSimpleExp (C.ExnTag _) = 1
  | sizeOfSimpleExp (C.Projection _) = 1
fun sizeOfCExp (C.Let { exp, result = _, cont, exnCont = _ }) = sizeOfSimpleExp exp + sizeOfCExp cont
  | sizeOfCExp (C.App { applied, args }) = List.length args
  | sizeOfCExp (C.If { cond, thenCont, elseCont }) = 1 + sizeOfCExp thenCont + sizeOfCExp elseCont
  | sizeOfCExp (C.Fn { function = (f, params, body), cont }) = sizeOfCExp body + sizeOfCExp cont
  | sizeOfCExp (C.Fix { functions, cont }) = List.foldl (fn ((_, _, body), acc) => acc + sizeOfCExp body) (sizeOfCExp cont) functions
  | sizeOfCExp (C.PushPrompt _) = 1
  | sizeOfCExp (C.WithSubCont _) = 1
  | sizeOfCExp (C.PushSubCont _) = 1
fun freeVarsInValue bound (C.Var v, acc) = if TypedSyntax.VIdSet.member (bound, v) then
                                               acc
                                           else
                                               TypedSyntax.VIdSet.add (acc, v)
  | freeVarsInValue bound (C.Unit, acc) = acc
  | freeVarsInValue bound (C.BoolConst _, acc) = acc
  | freeVarsInValue bound (C.Int32Const _, acc) = acc
  | freeVarsInValue bound (C.IntInfConst _, acc) = acc
  | freeVarsInValue bound (C.Word32Const _, acc) = acc
  | freeVarsInValue bound (C.CharConst _, acc) = acc
  | freeVarsInValue bound (C.Char16Const _, acc) = acc
  | freeVarsInValue bound (C.StringConst _, acc) = acc
  | freeVarsInValue bound (C.String16Const _, acc) = acc
fun freeVarsInSimpleExp (bound, C.PrimOp { primOp = _, tyargs = _, args }, acc) = List.foldl (freeVarsInValue bound) acc args
  | freeVarsInSimpleExp (bound, C.Record fields, acc) = Syntax.LabelMap.foldl (freeVarsInValue bound) acc fields
  | freeVarsInSimpleExp (bound, C.ExnTag { name = _, payloadTy = _}, acc) = acc
  | freeVarsInSimpleExp (bound, C.Projection { label = _, record, fieldTypes = _}, acc) = freeVarsInValue bound (record, acc)
fun freeVarsInCExp (bound, C.Let { exp, result, cont, exnCont = SOME (C.Var exnCont) }, acc) = freeVarsInCExp (TypedSyntax.VIdSet.add (bound, result), cont, freeVarsInSimpleExp (bound, exp, TypedSyntax.VIdSet.add (acc, exnCont)))
  | freeVarsInCExp (bound, C.Let { exp, result, cont, exnCont = _ }, acc) = freeVarsInCExp (TypedSyntax.VIdSet.add (bound, result), cont, freeVarsInSimpleExp (bound, exp, acc))
  | freeVarsInCExp (bound, C.App { applied, args }, acc) = List.foldl (freeVarsInValue bound) (freeVarsInValue bound (applied, acc)) args
  | freeVarsInCExp (bound, C.If { cond, thenCont, elseCont }, acc) = freeVarsInCExp (bound, elseCont, freeVarsInCExp (bound, thenCont, freeVarsInValue bound (cond, acc)))
  | freeVarsInCExp (bound, C.Fn { function = (f, params, body), cont }, acc) = let val acc = freeVarsInCExp (List.foldl TypedSyntax.VIdSet.add' bound params, body, acc)
                                                                               in freeVarsInCExp (TypedSyntax.VIdSet.add (bound, f), cont, acc)
                                                                               end
  | freeVarsInCExp (bound, C.Fix { functions, cont }, acc) = let val bound' = List.foldl (fn ((f, params, body), bound) =>
                                                                                             TypedSyntax.VIdSet.add (bound, f)
                                                                                         ) bound functions
                                                                 val acc = List.foldl (fn ((f, params, body), acc) =>
                                                                                          let val bound'' = List.foldl TypedSyntax.VIdSet.add' bound' params
                                                                                          in freeVarsInCExp (bound'', body, acc)
                                                                                          end
                                                                                      ) acc functions
                                                             in freeVarsInCExp (bound', cont, acc)
                                                             end
  | freeVarsInCExp (bound, C.PushPrompt { promptTag, f, cont, exnCont }, acc) = List.foldl (freeVarsInValue bound) acc [promptTag, f, cont, exnCont]
  | freeVarsInCExp (bound, C.WithSubCont { promptTag, f, cont, exnCont }, acc) = List.foldl (freeVarsInValue bound) acc [promptTag, f, cont, exnCont]
  | freeVarsInCExp (bound, C.PushSubCont { subCont, f, cont, exnCont }, acc) = List.foldl (freeVarsInValue bound) acc [subCont, f, cont, exnCont]
datatype usage = NEVER | ONCE_AS_CALLEE | ONCE | MANY
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
fun usageInSimpleExp (env, C.PrimOp { primOp = _, tyargs = _, args }) = List.app (usageInValue env) args
  | usageInSimpleExp (env, C.Record fields) = Syntax.LabelMap.app (usageInValue env) fields
  | usageInSimpleExp (env, C.ExnTag { name = _, payloadTy = _ }) = ()
  | usageInSimpleExp (env, C.Projection { label = _, record, fieldTypes = _ }) = usageInValue env record
local
    fun add (env, v) = if TypedSyntax.VIdMap.inDomain (env, v) then
                           raise Fail ("usageInCExp: duplicate name in AST: " ^ TypedSyntax.print_VId v)
                       else
                           TypedSyntax.VIdMap.insert (env, v, ref NEVER)
in
fun usageInCExp (env : ((usage ref) TypedSyntax.VIdMap.map) ref, C.Let { exp, result, cont, exnCont })
    = ( usageInSimpleExp (!env, exp)
      ; Option.app (usageInValue (!env)) exnCont
      ; env := add (!env, result)
      ; usageInCExp (env, cont)
      )
  | usageInCExp (env, C.App { applied, args }) = ( usageInValueAsCallee (!env) applied
                                                 ; List.app (usageInValue (!env)) args
                                                 )
  | usageInCExp (env, C.If { cond, thenCont, elseCont }) = ( usageInValue (!env) cond
                                                           ; usageInCExp (env, thenCont)
                                                           ; usageInCExp (env, elseCont)
                                                           )
  | usageInCExp (env, C.Fn { function = (f, params, body), cont }) = ( env := List.foldl (fn (p, e) => add (e, p)) (!env) params
                                                                     ; usageInCExp (env, body)
                                                                     ; env := add (!env, f)
                                                                     ; usageInCExp (env, cont)
                                                                     )
  | usageInCExp (env, C.Fix { functions, cont }) = let val env' = List.foldl (fn ((f, params, body), e) =>
                                                                                 let val e = add (e, f)
                                                                                 in List.foldl (fn (p, e) => add (e, p)) e params
                                                                                 end
                                                                             ) (!env) functions
                                                   in env := env'
                                                    ; List.app (fn (f, params, body) => usageInCExp (env, body)) functions
                                                    ; usageInCExp (env, cont)
                                                   end
  | usageInCExp (env, C.PushPrompt { promptTag, f, cont, exnCont }) = ( usageInValue (!env) promptTag
                                                                      ; usageInValue (!env) f
                                                                      ; usageInValue (!env) cont
                                                                      ; usageInValue (!env) exnCont
                                                                      )
  | usageInCExp (env, C.WithSubCont { promptTag, f, cont, exnCont }) = ( usageInValue (!env) promptTag
                                                                       ; usageInValue (!env) f
                                                                       ; usageInValue (!env) cont
                                                                       ; usageInValue (!env) exnCont
                                                                       )
  | usageInCExp (env, C.PushSubCont { subCont, f, cont, exnCont }) = ( usageInValue (!env) subCont
                                                                     ; usageInValue (!env) f
                                                                     ; usageInValue (!env) cont
                                                                     ; usageInValue (!env) exnCont
                                                                     )
end
fun substValue (subst : C.Value TypedSyntax.VIdMap.map) (x as C.Var v) = (case TypedSyntax.VIdMap.find (subst, v) of
                                                                              SOME w => w
                                                                            | NONE => x
                                                                         )
  | substValue subst v = v
fun substSimpleExp (subst, C.PrimOp { primOp, tyargs, args }) = C.PrimOp { primOp = primOp, tyargs = tyargs, args = List.map (substValue subst) args }
  | substSimpleExp (subst, C.Record fields) = C.Record (Syntax.LabelMap.map (substValue subst) fields)
  | substSimpleExp (subst, e as C.ExnTag _) = e
  | substSimpleExp (subst, C.Projection { label, record, fieldTypes }) = C.Projection { label = label, record = substValue subst record, fieldTypes = fieldTypes }
fun substCExp (subst : C.Value TypedSyntax.VIdMap.map, C.Let { exp, result, cont, exnCont }) = C.Let { exp = substSimpleExp (subst, exp), result = result, cont = substCExp (subst, cont), exnCont = Option.map (substValue subst) exnCont }
  | substCExp (subst, C.App { applied, args }) = C.App { applied = substValue subst applied, args = List.map (substValue subst) args }
  | substCExp (subst, C.If { cond, thenCont, elseCont }) = C.If { cond = substValue subst cond, thenCont = substCExp (subst, thenCont), elseCont = substCExp (subst, elseCont) }
  | substCExp (subst, C.Fn { function = (f, params, body), cont }) = C.Fn { function = (f, params, substCExp (subst, body)), cont = substCExp (subst, cont) }
  | substCExp (subst, C.Fix { functions, cont }) = C.Fix { functions = List.map (fn (f, params, body) => (f, params, substCExp (subst, body))) functions, cont = substCExp (subst, cont) }
  | substCExp (subst, C.PushPrompt { promptTag, f, cont, exnCont }) = C.PushPrompt { promptTag = substValue subst promptTag, f = substValue subst f, cont = substValue subst cont, exnCont = substValue subst exnCont }
  | substCExp (subst, C.WithSubCont { promptTag, f, cont, exnCont }) = C.WithSubCont { promptTag = substValue subst promptTag, f = substValue subst f, cont = substValue subst cont, exnCont = substValue subst exnCont }
  | substCExp (subst, C.PushSubCont { subCont, f, cont, exnCont }) = C.PushSubCont { subCont = substValue subst subCont, f = substValue subst f, cont = substValue subst cont, exnCont = substValue subst exnCont }
val substCExp = fn (subst, e) => if TypedSyntax.VIdMap.isEmpty subst then
                                     e
                                 else
                                     substCExp (subst, e)
fun alphaConvert (ctx : Context, subst : C.Value TypedSyntax.VIdMap.map, C.Let { exp, result, cont, exnCont })
    = let val result' = renewVId (ctx, result)
          val subst' = TypedSyntax.VIdMap.insert (subst, result, C.Var result')
      in C.Let { exp = substSimpleExp (subst, exp)
               , result = result'
               , exnCont = Option.map (substValue subst) exnCont
               , cont = alphaConvert (ctx, subst', cont)
               }
      end
  | alphaConvert (ctx, subst, C.App { applied, args }) = C.App { applied = substValue subst applied, args = List.map (substValue subst) args }
  | alphaConvert (ctx, subst, C.If { cond, thenCont, elseCont }) = C.If { cond = substValue subst cond, thenCont = alphaConvert (ctx, subst, thenCont), elseCont = alphaConvert (ctx, subst, elseCont) }
  | alphaConvert (ctx, subst, C.Fn { function = (f, params, body), cont })
    = let val (params', subst') = List.foldr (fn (p, (params', subst)) =>
                                                 let val p' = renewVId (ctx, p)
                                                 in (p' :: params', TypedSyntax.VIdMap.insert (subst, p, C.Var p'))
                                                 end
                                             ) ([], subst) params
          val body = alphaConvert (ctx, subst', body)
          val f' = renewVId (ctx, f)
          val subst = TypedSyntax.VIdMap.insert (subst, f, C.Var f')
      in C.Fn { function = (f', params', body)
              , cont = alphaConvert (ctx, subst, cont)
              }
      end
  | alphaConvert (ctx, subst, C.Fix { functions, cont })
    = let val (subst, nameMap) = List.foldl (fn ((f, _, _), (subst, nameMap)) =>
                                                let val f' = renewVId (ctx, f)
                                                in (TypedSyntax.VIdMap.insert (subst, f, C.Var f'), TypedSyntax.VIdMap.insert (nameMap, f, f'))
                                                end
                                            ) (subst, TypedSyntax.VIdMap.empty) functions
      in C.Fix { functions = List.map (fn (f, params, body) =>
                                          let val f' = TypedSyntax.VIdMap.lookup (nameMap, f)
                                              val (params', subst) = List.foldr (fn (p, (params', subst)) =>
                                                                                    let val p' = renewVId (ctx, p)
                                                                                    in (p' :: params', TypedSyntax.VIdMap.insert (subst, p, C.Var p'))
                                                                                    end
                                                                                ) ([], subst) params
                                          in (f', params', alphaConvert (ctx, subst, body))
                                          end
                                      ) functions
               , cont = alphaConvert (ctx, subst, cont)
               }
      end
  | alphaConvert (ctx, subst, C.PushPrompt { promptTag, f, cont, exnCont }) = C.PushPrompt { promptTag = substValue subst promptTag, f = substValue subst f, cont = substValue subst cont, exnCont = substValue subst exnCont }
  | alphaConvert (ctx, subst, C.WithSubCont { promptTag, f, cont, exnCont }) = C.WithSubCont { promptTag = substValue subst promptTag, f = substValue subst f, cont = substValue subst cont, exnCont = substValue subst exnCont }
  | alphaConvert (ctx, subst, C.PushSubCont { subCont, f, cont, exnCont }) = C.PushSubCont { subCont = substValue subst subCont, f = substValue subst f, cont = substValue subst cont, exnCont = substValue subst exnCont }
fun simplifySimpleExp (env : C.SimpleExp TypedSyntax.VIdMap.map, usage, C.PrimOp { primOp, tyargs, args }) : C.Value option = NONE (* TODO: constant folding *)
  | simplifySimpleExp (env, usage, C.Record fields) = NONE
  | simplifySimpleExp (env, usage, C.ExnTag _) = NONE
  | simplifySimpleExp (env, usage, C.Projection { label, record, fieldTypes })
    = (case record of
           C.Var v => (case TypedSyntax.VIdMap.find (env, v) of
                           SOME (C.Record fields) => (case Syntax.LabelMap.find (fields, label) of
                                                          SOME w => SOME w
                                                        | NONE => NONE
                                                     )
                         | _ => NONE
                      )
         | _ => NONE
      )
fun simplifyCExp (ctx, env, fenv : (C.Var list * C.CExp) TypedSyntax.VIdMap.map, subst, usage, e)
    = case e of
          C.Let { exp, result, cont, exnCont } =>
          let val exp = substSimpleExp (subst, exp)
              val exnCont = Option.map (substValue subst) exnCont
          in case simplifySimpleExp (env, usage, exp) of
                 SOME v => simplifyCExp (ctx, env, fenv, TypedSyntax.VIdMap.insert (subst, result, v), usage, cont)
               | NONE => if C.isDiscardable exp then
                             case TypedSyntax.VIdMap.find (usage, result) of
                                 SOME (ref NEVER) => simplifyCExp (ctx, env, fenv, subst, usage, cont)
                               | _ => C.Let { exp = exp
                                            , result = result
                                            , cont = simplifyCExp (ctx, TypedSyntax.VIdMap.insert (env, result, exp), fenv, subst, usage, cont)
                                            , exnCont = exnCont
                                            }
                         else
                             C.Let { exp = exp
                                   , result = result
                                   , cont = simplifyCExp (ctx, TypedSyntax.VIdMap.insert (env, result, exp), fenv, subst, usage, cont)
                                   , exnCont = exnCont
                                   }
          end
        | C.App { applied, args } =>
          let val applied = substValue subst applied
              val args = List.map (substValue subst) args
          in case applied of
                 C.Var applied =>
                 (case TypedSyntax.VIdMap.find (fenv, applied) of
                      SOME (params, body) => let val subst = ListPair.foldlEq (fn (p, a, subst) => TypedSyntax.VIdMap.insert (subst, p, a)) TypedSyntax.VIdMap.empty (params, args)
                                             in case TypedSyntax.VIdMap.find (usage, applied) of
                                                    SOME (ref ONCE_AS_CALLEE) => substCExp (subst, body) (* no alpha conversion *)
                                                  | _ => alphaConvert (ctx, subst, body)
                                             end
                    | NONE => C.App { applied = C.Var applied, args = args }
                 )
               | _ => C.App { applied = applied, args = args }
          end
        | C.If { cond, thenCont, elseCont } =>
          C.If { cond = substValue subst cond
               , thenCont = simplifyCExp (ctx, env, fenv, subst, usage, thenCont)
               , elseCont = simplifyCExp (ctx, env, fenv, subst, usage, elseCont)
               }
        | C.Fn { function = (f, params, body), cont } =>
          (case TypedSyntax.VIdMap.find (usage, f) of
               SOME (ref NEVER) => simplifyCExp (ctx, env, fenv, subst, usage, cont)
             | SOME (ref ONCE_AS_CALLEE) => let val body' = simplifyCExp (ctx, env, fenv, subst, usage, body)
                                                val fenv' = TypedSyntax.VIdMap.insert (fenv, f, (params, body'))
                                            in simplifyCExp (ctx, env, fenv', subst, usage, cont)
                                            end
             | _ => let val body = simplifyCExp (ctx, env, fenv, subst, usage, body)
                        val fenv = if sizeOfCExp body <= 10 then (* Inline small functions *)
                                       TypedSyntax.VIdMap.insert (fenv, f, (params, body))
                                   else
                                       fenv
                    in C.Fn { function = (f, params, body)
                            , cont = simplifyCExp (ctx, env, fenv, subst, usage, cont)
                            }
                    end
          )
        | C.Fix { functions, cont } =>
          C.Fix { functions = List.map (fn (f, params, body) => (f, params, simplifyCExp (ctx, env, fenv, subst, usage, body))) functions
                , cont = simplifyCExp (ctx, env, fenv, subst, usage, cont)
                }
        | C.PushPrompt { promptTag, f, cont, exnCont } => C.PushPrompt { promptTag = substValue subst promptTag, f = substValue subst f, cont = substValue subst cont, exnCont = substValue subst exnCont }
        | C.WithSubCont { promptTag, f, cont, exnCont } => C.WithSubCont { promptTag = substValue subst promptTag, f = substValue subst f, cont = substValue subst cont, exnCont = substValue subst exnCont }
        | C.PushSubCont { subCont, f, cont, exnCont } => C.PushSubCont { subCont = substValue subst subCont, f = substValue subst f, cont = substValue subst cont, exnCont = substValue subst exnCont }
end
end;
