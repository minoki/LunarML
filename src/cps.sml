(*
 * Copyright (c) 2022 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure CSyntax = struct
type Var = TypedSyntax.VId
type Tag = string
structure CVar :> sig
              eqtype t
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
               | NativeIntConst of IntInf.int
               | Int32Const of Int32.int
               | Int54Const of Int64.int
               | Int64Const of Int64.int
               | IntInfConst of IntInf.int
               | NativeWordConst of IntInf.int
               | Word32Const of Word32.word
               | Word64Const of Word64.word
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
                   | Abs of { contParam : CVar, params : Var list, body : CExp } (* non-recursive function *)
     and CExp = Let of { exp : SimpleExp, result : Var option, cont : CExp }
              | App of { applied : Value, cont : CVar, args : Value list } (* tail call *) (* return arity? *)
              | AppCont of { applied : CVar, args : Value list }
              | If of { cond : Value
                      , thenCont : CExp
                      , elseCont : CExp
                      }
              | LetRec of { defs : (Var * CVar * Var list * CExp) list, cont : CExp } (* recursive function *)
              | LetCont of { name : CVar, params : Var list, body : CExp, cont : CExp }
              | LetRecCont of { defs : (CVar * Var list * CExp) list, cont : CExp }
              | Handle of { body : CExp, handler : Var * CExp, successfulExitIn : CVar, successfulExitOut : CVar }
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
                                           | F.LuaCallOp => true
                                           | F.LuaCall1Op => true
                                           | F.LuaMethodOp _ => true
                                        )
  | mayRaise (Record _) = false
  | mayRaise (ExnTag _) = false
  | mayRaise (Projection _) = false
  | mayRaise (Abs _) = false

fun containsApp (Let { cont, ... }) = containsApp cont
  | containsApp (App _) = true
  | containsApp (AppCont _) = false
  | containsApp (If { cond, thenCont, elseCont }) = containsApp thenCont orelse containsApp elseCont
  | containsApp (LetRec { defs, cont }) = containsApp cont
  | containsApp (LetCont { name, params, body, cont }) = containsApp body orelse containsApp cont
  | containsApp (LetRecCont { defs, cont }) = containsApp cont orelse List.exists (fn (_, _, body) => containsApp body) defs
  | containsApp (Handle { body, handler = (_, h), ... }) = containsApp body orelse containsApp h

(*
fun freeCVars (bound : CVarSet.set, Let { exp, result, cont }, acc : CVarSet.set) = acc
  | freeCVars (bound, App { applied, cont, args }, acc) = if CVarSet.member (bound, cont) then
                                                              acc
                                                          else
                                                              CVarSet.add (acc, cont)
  | freeCVars (bound, AppCont { applied, args }, acc) = if CVarSet.member (bound, applied) then
                                                            acc
                                                        else
                                                            CVarSet.add (acc, applied)
  | freeCVars (bound, If { cond, thenCont, elseCont }, acc) = freeCVars (bound, elseCont, freeCVars (bound, thenCont, acc))
  | freeCVars (bound, LetRec { defs, cont }, acc) = freeCVars (bound, cont, acc)
  | freeCVars (bound, LetCont { name, params, body, cont }, acc) = freeCVars (bound, body, freeCVars (CVarSet.add (bound, name), body, acc))
  | freeCVars (bound, LetRecCont { defs, cont }, acc) = let val bound' = List.foldl (fn ((name, _, _), b) => CVarSet.add (b, name)) bound defs
                                                        in freeCVars (bound', cont, List.foldl (fn ((_, _, body), acc) => freeCVars (bound', body, acc)) acc defs)
                                                        end
  | freeCVars (bound, Handle { body, handler = (e, h), successfulExitIn, successfulExitOut }, acc)
    = let val acc = if CVarSet.member (bound, successfulExitOut) then
                        acc
                    else
                        CVarSet.add (acc, successfulExitOut)
          val acc = freeCVars (CVarSet.add (bound, successfulExitIn), body, acc)
      in freeCVars (bound, h, acc)
      end
*)
end
end

structure CpsTransform = struct
local structure F = FSyntax
      structure C = CSyntax
in

type Context = { targetInfo : TargetInfo.target_info
               , nextVId : int ref
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
val initialEnv = List.foldl TypedSyntax.VIdMap.insert' TypedSyntax.VIdMap.empty
                            [(InitialEnv.VId_false, C.BoolConst false)
                            ,(InitialEnv.VId_true, C.BoolConst true)
                            ]
(* transformX : Context * Value TypedSyntax.VIdMap.map -> F.Exp -> cont -> C.Exp *)
fun transform (ctx, env) exp { resultHint } k = transformX (ctx, env) exp (META (resultHint, k))
and transformT (ctx, env) exp k = transformX (ctx, env) exp (REIFIED k)
and transformX (ctx : Context, env) (exp : F.Exp) (k : cont) : C.CExp
    = case exp of
           F.PrimExp (F.PrimFnOp Primitives.DelimCont_pushPrompt, tyargs, [p (* 'a prompt_tag *), f (* unit -> 'a *)]) =>
           transform (ctx, env) p { resultHint = NONE }
                     (fn p =>
                         transform (ctx, env) f { resultHint = NONE }
                                   (fn f =>
                                       reify (ctx, k)
                                             (fn kk =>
                                                 C.App { applied = C.Var InitialEnv.VId_DelimCont_pushPrompt, cont = kk, args = [p, f] }
                                             )
                                   )
                     )
         | F.PrimExp (F.PrimFnOp Primitives.DelimCont_withSubCont, tyargs, [p (* 'b prompt_tag *), f (* ('a,'b) subcont -> 'b *)]) =>
           transform (ctx, env) p { resultHint = NONE }
                     (fn p =>
                         transform (ctx, env) f { resultHint = NONE }
                                   (fn f =>
                                       reify (ctx, k)
                                             (fn kk =>
                                                 C.App { applied = C.Var InitialEnv.VId_DelimCont_withSubCont, cont = kk, args = [p, f] }
                                             )
                               )
                 )
         | F.PrimExp (F.PrimFnOp Primitives.DelimCont_pushSubCont, tyargs, [subcont (* ('a,'b) subcont *), f (* unit -> 'a *)]) =>
           transform (ctx, env) subcont { resultHint = NONE }
                     (fn subcont =>
                         transform (ctx, env) f { resultHint = NONE }
                                   (fn f =>
                                       reify (ctx, k)
                                             (fn kk =>
                                                 C.App { applied = C.Var InitialEnv.VId_DelimCont_pushSubCont, cont = kk, args = [subcont, f] }
                                             )
                               )
                 )
         | F.PrimExp (F.PrimFnOp Primitives.Unsafe_cast, tyargs, [arg]) =>
           transformX (ctx, env) arg k
         | F.PrimExp (primOp, tyargs, args) =>
           mapCont (fn (e, cont) => transform (ctx, env) e { resultHint = NONE } cont)
                   args
                   (fn args =>
                       case primOp of
                           F.IntConstOp x => (case tyargs of
                                                  [F.TyVar tv] => if TypedSyntax.eqUTyVar (tv, F.tyNameToTyVar Typing.primTyName_int) then
                                                                      case #defaultInt (#targetInfo ctx) of
                                                                          TargetInfo.NATIVE_INT => apply k (C.NativeIntConst x)
                                                                        | TargetInfo.INT32 => apply k (C.Int32Const (Int32.fromLarge x))
                                                                  else if TypedSyntax.eqUTyVar (tv, F.tyNameToTyVar Typing.primTyName_int54) then
                                                                      apply k (C.Int54Const (Int64.fromLarge x))
                                                                  else if TypedSyntax.eqUTyVar (tv, F.tyNameToTyVar Typing.primTyName_int64) then
                                                                      apply k (C.Int64Const (Int64.fromLarge x))
                                                                  else if TypedSyntax.eqUTyVar (tv, F.tyNameToTyVar Typing.primTyName_intInf) then
                                                                      apply k (C.IntInfConst x)
                                                                  else
                                                                      raise Fail "IntConstOp: invalid type"
                                                | _ => raise Fail "IntConstOp: invalid type"
                                             )
                         | F.WordConstOp x => (case tyargs of
                                                   [F.TyVar tv] => if TypedSyntax.eqUTyVar (tv, F.tyNameToTyVar Typing.primTyName_word) then
                                                                       case #defaultWord (#targetInfo ctx) of
                                                                           TargetInfo.NATIVE_WORD => apply k (C.NativeWordConst x)
                                                                         | TargetInfo.WORD32 => apply k (C.Word32Const (Word32.fromLargeInt x))
                                                                   else if TypedSyntax.eqUTyVar (tv, F.tyNameToTyVar Typing.primTyName_word64) then
                                                                       apply k (C.Word64Const (Word64.fromLargeInt x))
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
                         | _ => let val returnsUnit = case primOp of
                                                          F.PrimFnOp Primitives.Ref_set => true
                                                        | F.PrimFnOp Primitives.Unsafe_Array_update => true
                                                        | F.PrimFnOp Primitives.Lua_set => true
                                                        | F.PrimFnOp Primitives.JavaScript_set => true
                                                        | _ => false
                                    val exp = C.PrimOp { primOp = primOp, tyargs = tyargs, args = args }
                                in if returnsUnit then
                                       C.Let { exp = exp
                                             , result = NONE
                                             , cont = apply k C.Unit
                                             }
                                   else
                                       let val result = case k of
                                                            META (SOME r, _) => r
                                                          | _ => genSym ctx
                                       in C.Let { exp = exp
                                                , result = SOME result
                                                , cont = apply k (C.Var result)
                                                }
                                       end
                                end
                   )
         | F.VarExp vid => (case TypedSyntax.VIdMap.find (env, vid) of
                                SOME v => apply k v
                              | NONE => apply k (C.Var vid)
                           )
         | F.RecordExp [] => apply k C.Unit
         | F.RecordExp fields => mapCont (fn ((label, exp), cont) => transform (ctx, env) exp { resultHint = NONE } (fn v => cont (label, v)))
                                         fields
                                         (fn fields => let val result = case k of
                                                                            META (SOME r, _) => r
                                                                          | _ => genSym ctx
                                                       in C.Let { exp = C.Record (List.foldl Syntax.LabelMap.insert' Syntax.LabelMap.empty fields)
                                                                , result = SOME result
                                                                , cont = apply k (C.Var result)
                                                                }
                                                       end
                                         )
         | F.LetExp (F.ValDec (vid, _, exp1), exp2) =>
           transform (ctx, env) exp1 { resultHint = SOME vid }
                     (fn v =>
                         transformX (ctx, TypedSyntax.VIdMap.insert (env, vid, v)) exp2 k
                     )
         | F.LetExp (F.RecValDec decs, exp2) =>
           C.LetRec { defs = List.map (fn (vid, _, exp1) =>
                                          let val contParam = genContSym ctx
                                          in case stripTyAbs exp1 of
                                                 F.FnExp (param, _, body) => (vid, contParam, [param], transformT (ctx, env) body contParam)
                                               | _ => raise Fail "RecValDec"
                                          end
                                      ) decs
                    , cont = transformX (ctx, env) exp2 k
                    }
         | F.LetExp (F.UnpackDec (_, _, vid, _, exp1), exp2) =>
           transform (ctx, env) exp1 { resultHint = SOME vid }
                     (fn v =>
                         transformX (ctx, TypedSyntax.VIdMap.insert (env, vid, v)) exp2 k
                     )
         | F.LetExp (F.IgnoreDec exp1, exp2) =>
           transform (ctx, env) exp1 { resultHint = NONE }
                     (fn _ =>
                         transformX (ctx, env) exp2 k
                     )
         | F.LetExp (F.DatatypeDec _, exp) => transformX (ctx, env) exp k
         | F.LetExp (F.ExceptionDec { name, tagName, payloadTy }, exp) =>
           C.Let { exp = C.ExnTag { name = name
                                  , payloadTy = payloadTy
                                  }
                 , result = SOME tagName
                 , cont = transformX (ctx, env) exp k
                 }
         | F.LetExp (F.ExportValue _, _) => raise Fail "ExportValue in CPS: not supported"
         | F.LetExp (F.ExportModule _, _) => raise Fail "ExportModule in CPS: not supported"
         | F.LetExp (F.GroupDec (_, decs), exp) => transformX (ctx, env) (List.foldr F.LetExp exp decs) k
         | F.AppExp (applied, arg) =>
           transform (ctx, env) applied { resultHint = NONE }
                     (fn f =>
                         transform (ctx, env) arg { resultHint = NONE }
                                   (fn v =>
                                       reify (ctx, k)
                                             (fn j =>
                                                 C.App { applied = f, cont = j, args = [v] }
                                             )
                                   )
                     )
         | F.HandleExp { body, exnName, handler } =>
           reify (ctx, k)
                 (fn j => let val success = genContSym ctx
                          in C.Handle { body = transformT (ctx, env) body success
                                      , handler = (exnName, transformT (ctx, env) handler j)
                                      , successfulExitIn = success
                                      , successfulExitOut = j
                                      }
                          end
                 )
         | F.IfThenElseExp (e1, e2, e3) =>
           transform (ctx, env) e1 { resultHint = NONE }
                     (fn e1 =>
                         reify (ctx, k)
                               (fn j => C.If { cond = e1
                                             , thenCont = transformT (ctx, env) e2 j
                                             , elseCont = transformT (ctx, env) e3 j
                                             }
                                  )
                     )
         | F.CaseExp _ => raise Fail "CaseExp: not supported here"
         | F.FnExp (vid, _, body) => let val f = case k of
                                                     META (SOME f, _) => f
                                                   | _ => genSym ctx
                                         val kk = genContSym ctx
                                     in C.Let { exp = C.Abs { contParam = kk
                                                            , params = [vid]
                                                            , body = transformT (ctx, env) body kk
                                                            }
                                              , result = SOME f
                                              , cont = apply k (C.Var f)
                                              }
                                     end
         | F.ProjectionExp { label, record, fieldTypes } =>
           transform (ctx, env) record { resultHint = NONE }
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
                                  }
                         end
                     )
         | F.TyAbsExp (_, _, exp) => transformX (ctx, env) exp k
         | F.TyAppExp (exp, _) => transformX (ctx, env) exp k
         | F.PackExp { payloadTy, exp, packageTy } => transformX (ctx, env) exp k
fun transformDecs (ctx : Context, env) ([] : F.Dec list) (k : C.CVar) : C.CExp
    = C.AppCont { applied = k, args = [] } (* apply continuation *)
  | transformDecs (ctx, env) [F.ExportValue exp] k
    = transform (ctx, env) exp { resultHint = NONE (* "export"? *) }
                (fn v => C.AppCont { applied = k, args = [v] })
  | transformDecs (ctx, env) [F.ExportModule items] k
    = mapCont (fn ((name, exp), cont) => transform (ctx, env) exp { resultHint = NONE (* name? *) } (fn v => cont (name, v)))
              (Vector.foldr (op ::) [] items)
              (fn items => let val result = genSym ctx (* "export"? *)
                           in C.Let { exp = C.Record (List.foldl (fn ((name, v), m) => Syntax.LabelMap.insert (m, Syntax.IdentifierLabel name, v)) Syntax.LabelMap.empty items)
                                    , result = SOME result
                                    , cont = C.AppCont { applied = k, args = [C.Var result] }
                                    }
                           end
              )
  | transformDecs (ctx, env) (dec :: decs) k
    = (case dec of
           F.ValDec (vid, _, exp) => transform (ctx, env) exp { resultHint = SOME vid }
                                               (fn v => transformDecs (ctx, TypedSyntax.VIdMap.insert (env, vid, v)) decs k)
         | F.RecValDec decs' => C.LetRec { defs = List.map (fn (vid, _, exp) =>
                                                               let val contParam = genContSym ctx
                                                               in case stripTyAbs exp of
                                                                      F.FnExp (param, _, body) => (vid, contParam, [param], transformT (ctx, env) body contParam)
                                                                    | _ => raise Fail "RecValDec"
                                                               end
                                                           ) decs'
                                         , cont = transformDecs (ctx, env) decs k
                                         }
         | F.UnpackDec (_, _, vid, _, exp) => transform (ctx, env) exp { resultHint = SOME vid }
                                                        (fn v => transformDecs (ctx, TypedSyntax.VIdMap.insert (env, vid, v)) decs k)
         | F.IgnoreDec exp => transform (ctx, env) exp { resultHint = NONE }
                                        (fn v => transformDecs (ctx, env) decs k)
         | F.DatatypeDec _ => transformDecs (ctx, env) decs k
         | F.ExceptionDec { name, tagName, payloadTy } => C.Let { exp = C.ExnTag { name = name
                                                                                 , payloadTy = payloadTy
                                                                                 }
                                                                , result = SOME tagName
                                                                , cont = transformDecs (ctx, env) decs k
                                                                }
         | F.ExportValue exp => raise Fail "ExportValue must be the last declaration"
         | F.ExportModule _ => raise Fail "ExportModule must be the last declaration"
         | F.GroupDec (_, decs') => transformDecs (ctx, env) (decs' @ decs) k
      )
end
end;

structure CpsSimplify = struct
local structure F = FSyntax
      structure C = CSyntax
in
type Context = { nextVId : int ref, simplificationOccurred : bool ref }
fun genContSym (ctx : Context) : CSyntax.CVar
    = let val n = !(#nextVId ctx)
          val _ = #nextVId ctx := n + 1
      in CSyntax.CVar.fromInt n
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
and sizeOfCExp (e, threshold)
    = if threshold < 0 then
          threshold
      else
          case e of
              C.Let { exp, result = _, cont } => sizeOfCExp (cont, sizeOfSimpleExp (exp, threshold))
            | C.App { applied, cont, args } => threshold - List.length args
            | C.AppCont { applied, args } => threshold - List.length args
            | C.If { cond, thenCont, elseCont } => sizeOfCExp (elseCont, sizeOfCExp (thenCont, threshold - 1))
            | C.LetRec { defs, cont } => List.foldl (fn ((_, _, _, body), t) => sizeOfCExp (body, t)) (sizeOfCExp (cont, threshold)) defs
            | C.LetCont { name, params, body, cont } => sizeOfCExp (body, sizeOfCExp (cont, threshold))
            | C.LetRecCont { defs, cont } => List.foldl (fn ((_, _, body), t) => sizeOfCExp (body, t)) (sizeOfCExp (cont, threshold)) defs
            | C.Handle { body, handler = (_, h), successfulExitIn, successfulExitOut } => sizeOfCExp (body, sizeOfCExp (h, threshold - 1))
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
  | usageInValue env (C.NativeIntConst _) = ()
  | usageInValue env (C.Int32Const _) = ()
  | usageInValue env (C.Int54Const _) = ()
  | usageInValue env (C.Int64Const _) = ()
  | usageInValue env (C.IntInfConst _) = ()
  | usageInValue env (C.NativeWordConst _) = ()
  | usageInValue env (C.Word32Const _) = ()
  | usageInValue env (C.Word64Const _) = ()
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
  | usageInValueAsCallee env (C.NativeIntConst _) = ()
  | usageInValueAsCallee env (C.Int32Const _) = ()
  | usageInValueAsCallee env (C.Int54Const _) = ()
  | usageInValueAsCallee env (C.Int64Const _) = ()
  | usageInValueAsCallee env (C.IntInfConst _) = ()
  | usageInValueAsCallee env (C.NativeWordConst _) = ()
  | usageInValueAsCallee env (C.Word32Const _) = ()
  | usageInValueAsCallee env (C.Word64Const _) = ()
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
  | usageInSimpleExp (env, renv, cenv, crenv, C.Abs { contParam, params, body })
    = ( env := List.foldl (fn (p, e) => add (e, p)) (!env) params
      ; cenv := addC (!cenv, contParam)
      ; usageInCExp (env, renv, cenv, crenv, body)
      )
and usageInCExp (env : ((usage ref) TypedSyntax.VIdMap.map) ref, renv, cenv : ((cont_usage ref) C.CVarMap.map) ref, crenv, cexp)
    = case cexp of
          C.Let { exp, result, cont } =>
          ( usageInSimpleExp (env, renv, cenv, crenv, exp)
          ; case result of
                SOME result => env := add (!env, result)
              | NONE => ()
          ; usageInCExp (env, renv, cenv, crenv, cont)
          )
        | C.App { applied, cont, args } =>
          ( usageInValueAsCallee (!env) applied
          ; usageContVar (!cenv) cont
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
          let val recursiveEnv = List.foldl (fn ((f, _, _, _), m) => TypedSyntax.VIdMap.insert (m, f, ref NEVER)) TypedSyntax.VIdMap.empty defs
              val innerEnv = List.foldl (fn ((_, _, params, _), e) =>
                                            List.foldl (fn (p, e) => add (e, p)) e params
                                        ) (TypedSyntax.VIdMap.unionWith #2 (!env, recursiveEnv)) defs
              val cenv' = List.foldl (fn ((_, k, _, _), ce) => addC (ce, k)) (!cenv) defs
          in env := innerEnv
           ; cenv := cenv'
           ; List.app (fn (_, _, _, body) => usageInCExp (env, renv, cenv, crenv, body)) defs
           ; renv := TypedSyntax.VIdMap.foldli (fn (f, r, m) => TypedSyntax.VIdMap.insert (m, f, r)) (!renv) recursiveEnv
           ; env := List.foldl (fn ((f, _, _, _), m) => TypedSyntax.VIdMap.insert (m, f, ref NEVER)) (!env) defs
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
        | C.Handle { body, handler = (e, h), successfulExitIn, successfulExitOut } =>
          ( usageContVar (!cenv) successfulExitOut
          ; cenv := addC (!cenv, successfulExitIn)
          ; usageInCExp (env, renv, cenv, crenv, body)
          ; env := add (!env, e)
          ; usageInCExp (env, renv, cenv, crenv, h)
          )
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
  | substSimpleExp (subst, csubst, C.Abs { contParam, params, body }) = C.Abs { contParam = contParam, params = params, body = substCExp (subst, csubst, body) }
and substCExp (subst : C.Value TypedSyntax.VIdMap.map, csubst : C.CVar C.CVarMap.map, C.Let { exp, result, cont }) = C.Let { exp = substSimpleExp (subst, csubst, exp), result = result, cont = substCExp (subst, csubst, cont) }
  | substCExp (subst, csubst, C.App { applied, cont, args }) = C.App { applied = substValue subst applied, cont = substCVar csubst cont, args = List.map (substValue subst) args }
  | substCExp (subst, csubst, C.AppCont { applied, args }) = C.AppCont { applied = substCVar csubst applied, args = List.map (substValue subst) args }
  | substCExp (subst, csubst, C.If { cond, thenCont, elseCont }) = C.If { cond = substValue subst cond, thenCont = substCExp (subst, csubst, thenCont), elseCont = substCExp (subst, csubst, elseCont) }
  | substCExp (subst, csubst, C.LetRec { defs, cont }) = C.LetRec { defs = List.map (fn (f, k, params, body) => (f, k, params, substCExp (subst, csubst, body))) defs, cont = substCExp (subst, csubst, cont) }
  | substCExp (subst, csubst, C.LetCont { name, params, body, cont }) = C.LetCont { name = name, params = params, body = substCExp (subst, csubst, body), cont = substCExp (subst, csubst, cont) }
  | substCExp (subst, csubst, C.LetRecCont { defs, cont }) = C.LetRecCont { defs = List.map (fn (f, params, body) => (f, params, substCExp (subst, csubst, body))) defs, cont = substCExp (subst, csubst, cont) }
  | substCExp (subst, csubst, C.Handle { body, handler = (e, h), successfulExitIn, successfulExitOut }) = C.Handle { body = substCExp (subst, csubst, body), handler = (e, substCExp (subst, csubst, h)), successfulExitIn = successfulExitIn, successfulExitOut = substCVar csubst successfulExitOut }
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
and alphaConvert (ctx : Context, subst : C.Value TypedSyntax.VIdMap.map, csubst : C.CVar C.CVarMap.map, C.Let { exp, result = SOME result, cont })
    = let val result' = renewVId (ctx, result)
          val subst' = TypedSyntax.VIdMap.insert (subst, result, C.Var result')
      in C.Let { exp = alphaConvertSimpleExp (ctx, subst, csubst, exp)
               , result = SOME result'
               , cont = alphaConvert (ctx, subst', csubst, cont)
               }
      end
  | alphaConvert (ctx, subst, csubst, C.Let { exp, result = NONE, cont })
    = C.Let { exp = alphaConvertSimpleExp (ctx, subst, csubst, exp)
            , result = NONE
            , cont = alphaConvert (ctx, subst, csubst, cont)
            }
  | alphaConvert (ctx, subst, csubst, C.App { applied, cont, args })
    = C.App { applied = substValue subst applied
            , cont = substCVar csubst cont
            , args = List.map (substValue subst) args
            }
  | alphaConvert (ctx, subst, csubst, C.AppCont { applied, args })
    = C.AppCont { applied = substCVar csubst applied, args = List.map (substValue subst) args }
  | alphaConvert (ctx, subst, csubst, C.If { cond, thenCont, elseCont }) = C.If { cond = substValue subst cond, thenCont = alphaConvert (ctx, subst, csubst, thenCont), elseCont = alphaConvert (ctx, subst, csubst, elseCont) }
  | alphaConvert (ctx, subst, csubst, C.LetRec { defs, cont })
    = let val (subst, nameMap) = List.foldl (fn ((f, _, _, _), (subst, nameMap)) =>
                                                let val f' = renewVId (ctx, f)
                                                in (TypedSyntax.VIdMap.insert (subst, f, C.Var f'), TypedSyntax.VIdMap.insert (nameMap, f, f'))
                                                end
                                            ) (subst, TypedSyntax.VIdMap.empty) defs
      in C.LetRec { defs = List.map (fn (f, k, params, body) =>
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
datatype simplify_result = VALUE of C.Value
                         | SIMPLE_EXP of C.SimpleExp
                         | NOT_SIMPLIFIED
type value_info = { exp : C.SimpleExp option, isDiscardableFunction : bool }
fun isDiscardableExp (env : value_info TypedSyntax.VIdMap.map, C.Let { exp, result, cont })
    = (case exp of
           C.Abs { body, ... } => let val env = case result of
                                                    SOME result => TypedSyntax.VIdMap.insert (env, result, { exp = NONE, isDiscardableFunction = isDiscardableExp (env, body) })
                                                  | NONE => env
                                  in isDiscardableExp (env, cont)
                                  end
         | _ => C.isDiscardable exp andalso isDiscardableExp (env, cont)
      )
  | isDiscardableExp (env, C.App { applied = C.Var applied, cont, args })
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
  | isDiscardableExp (env, C.Handle { body, handler = (e, h), successfulExitIn, successfulExitOut }) = isDiscardableExp (env, body) andalso isDiscardableExp (env, h)
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
  | simplifySimpleExp (env, C.PrimOp { primOp = F.PrimFnOp Primitives.Lua_call, tyargs, args = [ctor, C.Var args] })
    = (case TypedSyntax.VIdMap.find (env, args) of
           SOME { exp = SOME (C.PrimOp { primOp = F.VectorOp, tyargs = _, args }), ... } => SIMPLE_EXP (C.PrimOp { primOp = F.LuaCallOp, tyargs = [], args = ctor :: args })
         | _ => NOT_SIMPLIFIED
      )
  | simplifySimpleExp (env, C.PrimOp { primOp = F.PrimFnOp Primitives.Lua_call1, tyargs, args = [ctor, C.Var args] })
    = (case TypedSyntax.VIdMap.find (env, args) of
           SOME { exp = SOME (C.PrimOp { primOp = F.VectorOp, tyargs = _, args }), ... } => SIMPLE_EXP (C.PrimOp { primOp = F.LuaCall1Op, tyargs = [], args = ctor :: args })
         | _ => NOT_SIMPLIFIED
      )
  | simplifySimpleExp (env, C.PrimOp { primOp = F.PrimFnOp Primitives.Lua_method, tyargs, args = [ctor, C.StringConst name, C.Var args] })
    = let val name = CharVector.tabulate (Vector.length name, fn i => Char.chr (Vector.sub (name, i)))
      in if LuaWriter.isLuaIdentifier name then
             case TypedSyntax.VIdMap.find (env, args) of
                 SOME { exp = SOME (C.PrimOp { primOp = F.VectorOp, tyargs = _, args }), ... } => SIMPLE_EXP (C.PrimOp { primOp = F.LuaMethodOp name, tyargs = [], args = ctor :: args })
               | _ => NOT_SIMPLIFIED
         else
             NOT_SIMPLIFIED
      end
  | simplifySimpleExp (env, C.PrimOp { primOp = F.DataTagAsStringOp _, tyargs, args = [C.Var x] })
    = (case TypedSyntax.VIdMap.find (env, x) of
           SOME { exp = SOME (C.PrimOp { primOp = F.ConstructValOp { tag, ... }, ... }), ... } => VALUE (C.StringConst (Vector.tabulate (String.size tag, fn i => ord (String.sub (tag, i)))))
         | SOME { exp = SOME (C.PrimOp { primOp = F.ConstructValWithPayloadOp { tag, ... }, ... }), ... } => VALUE (C.StringConst (Vector.tabulate (String.size tag, fn i => ord (String.sub (tag, i)))))
         | _ => NOT_SIMPLIFIED
      )
  | simplifySimpleExp (env, C.PrimOp { primOp = F.DataTagAsString16Op _, tyargs, args = [C.Var x] })
    = (case TypedSyntax.VIdMap.find (env, x) of
           SOME { exp = SOME (C.PrimOp { primOp = F.ConstructValOp { tag, ... }, ... }), ... } => VALUE (C.String16Const (Vector.tabulate (String.size tag, fn i => ord (String.sub (tag, i))))) (* Assume tag is ASCII *)
         | SOME { exp = SOME (C.PrimOp { primOp = F.ConstructValWithPayloadOp { tag, ... }, ... }), ... } => VALUE (C.String16Const (Vector.tabulate (String.size tag, fn i => ord (String.sub (tag, i))))) (* Assume tag is ASCII *)
         | _ => NOT_SIMPLIFIED
      )
  | simplifySimpleExp (env, C.PrimOp { primOp = F.PrimFnOp Primitives.Bool_EQUAL, tyargs, args = [x, C.BoolConst true] }) = VALUE x
  | simplifySimpleExp (env, C.PrimOp { primOp = F.PrimFnOp Primitives.Bool_EQUAL, tyargs, args = [C.BoolConst true, x] }) = VALUE x
  | simplifySimpleExp (env, C.PrimOp { primOp = F.PrimFnOp Primitives.Bool_EQUAL, tyargs, args = [x, C.BoolConst false] }) = SIMPLE_EXP (C.PrimOp { primOp = F.PrimFnOp Primitives.Bool_not, tyargs = [], args = [x] })
  | simplifySimpleExp (env, C.PrimOp { primOp = F.PrimFnOp Primitives.Bool_EQUAL, tyargs, args = [C.BoolConst false, x] }) = SIMPLE_EXP (C.PrimOp { primOp = F.PrimFnOp Primitives.Bool_not, tyargs = [], args = [x] })
  | simplifySimpleExp (env, C.PrimOp { primOp = F.PrimFnOp Primitives.Bool_not, tyargs, args = [C.BoolConst x] }) = VALUE (C.BoolConst (not x))
  | simplifySimpleExp (env, C.PrimOp { primOp = F.PrimFnOp Primitives.Bool_not, tyargs, args = [C.Var x] })
    = (case TypedSyntax.VIdMap.find (env, x) of
           SOME { exp = SOME (C.PrimOp { primOp = F.PrimFnOp Primitives.Bool_not, tyargs = _, args = [v] }), ... } => VALUE v
         | _ => NOT_SIMPLIFIED
      )
  | simplifySimpleExp (env, C.PrimOp { primOp = F.PrimFnOp Primitives.String_EQUAL, tyargs, args = [C.StringConst x, C.StringConst y] }) = VALUE (C.BoolConst (x = y))
  | simplifySimpleExp (env, C.PrimOp { primOp = F.PrimFnOp Primitives.String16_EQUAL, tyargs, args = [C.String16Const x, C.String16Const y] }) = VALUE (C.BoolConst (x = y))
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
  | simplifySimpleExp (env, C.Abs { contParam, params, body }) = NOT_SIMPLIFIED (* TODO: Try eta conversion *)
and simplifyCExp (ctx, env, cenv, subst, csubst, usage, rusage, cusage, crusage, e)
    = case e of
          C.Let { exp, result, cont } =>
          let val exp = substSimpleExp (subst, csubst, exp)
          in case simplifySimpleExp (env, exp) of
                 VALUE v => let val () = #simplificationOccurred ctx := true
                                val subst = case result of
                                                SOME result => TypedSyntax.VIdMap.insert (subst, result, v)
                                              | NONE => subst
                            in simplifyCExp (ctx, env, cenv, subst, csubst, usage, rusage, cusage, crusage, cont)
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
                        (case TypedSyntax.VIdMap.find (usage, result) of
                             SOME (ref NEVER) => simplifyCExp (ctx, env, cenv, subst, csubst, usage, rusage, cusage, crusage, cont)
                           | SOME (ref ONCE_AS_CALLEE) => let val body = simplifyCExp (ctx, env, cenv, subst, csubst, usage, rusage, cusage, crusage, body)
                                                              val env = TypedSyntax.VIdMap.insert (env, result, { exp = SOME (C.Abs { contParam = contParam, params = params, body = body }), isDiscardableFunction = isDiscardableExp (env, body) })
                                                          in simplifyCExp (ctx, env, cenv, subst, csubst, usage, rusage, cusage, crusage, cont)
                                                          end
                           | _ => let val body = simplifyCExp (ctx, env, cenv, subst, csubst, usage, rusage, cusage, crusage, body)
                                      val exp = C.Abs { contParam = contParam, params = params, body = body }
                                      val env = if sizeOfCExp (body, 10) >= 0 then (* Inline small functions *)
                                                    TypedSyntax.VIdMap.insert (env, result, { exp = SOME exp, isDiscardableFunction = isDiscardableExp (env, body) })
                                                else
                                                    TypedSyntax.VIdMap.insert (env, result, { exp = NONE, isDiscardableFunction = isDiscardableExp (env, body) })
                                  in C.Let { exp = exp
                                           , result = SOME result
                                           , cont = simplifyCExp (ctx, env, cenv, subst, csubst, usage, rusage, cusage, crusage, cont)
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
                                                              }
                                  | (false, NONE) => C.Let { exp = exp
                                                           , result = NONE
                                                           , cont = simplifyCExp (ctx, env, cenv, subst, csubst, usage, rusage, cusage, crusage, cont)
                                                           }
                             end
                 end
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
                      in case TypedSyntax.VIdMap.find (usage, applied) of
                             SOME (ref ONCE_AS_CALLEE) => substCExp (subst, csubst, body) (* no alpha conversion *)
                           | _ => alphaConvert (ctx, subst, csubst, body)
                      end
                    | SOME { exp, isDiscardableFunction = true } =>
                      (case C.CVarMap.find (cenv, cont) of
                           SOME (params, _) => if List.all (fn p => case TypedSyntax.VIdMap.find (usage, p) of SOME (ref NEVER) => true | _ => false) params then
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
                     val subst = ListPair.foldlEq (fn (p, a, subst) => TypedSyntax.VIdMap.insert (subst, p, a)) subst (params, args)
                 in case C.CVarMap.find (cusage, applied) of
                        SOME (ref C_ONCE_DIRECT) => substCExp (subst, csubst, body) (* no alpha conversion *)
                      | _ => alphaConvert (ctx, subst, csubst, body)
                 end
               | _ => C.AppCont { applied = applied, args = args }
          end
        | C.If { cond, thenCont, elseCont } =>
          (case substValue subst cond of
               C.BoolConst true => (#simplificationOccurred ctx := true; simplifyCExp (ctx, env, cenv, subst, csubst, usage, rusage, cusage, crusage, thenCont))
             | C.BoolConst false => (#simplificationOccurred ctx := true; simplifyCExp (ctx, env, cenv, subst, csubst, usage, rusage, cusage, crusage, elseCont))
             | cond => C.If { cond = cond
                            , thenCont = simplifyCExp (ctx, env, cenv, subst, csubst, usage, rusage, cusage, crusage, thenCont)
                            , elseCont = simplifyCExp (ctx, env, cenv, subst, csubst, usage, rusage, cusage, crusage, elseCont)
                            }
          )
        | C.LetRec { defs, cont } =>
          if List.exists (fn (f, _, _, _) => case TypedSyntax.VIdMap.find (usage, f) of SOME (ref NEVER) => false | _ => true) defs then
              C.LetRec { defs = List.map (fn (f, k, params, body) => (f, k, params, simplifyCExp (ctx, env, cenv, subst, csubst, usage, rusage, cusage, crusage, body))) defs
                       , cont = simplifyCExp (ctx, env, cenv, subst, csubst, usage, rusage, cusage, crusage, cont)
                       }
          else
              ( #simplificationOccurred ctx := true
              ; simplifyCExp (ctx, env, cenv, subst, csubst, usage, rusage, cusage, crusage, cont)
              )
        | C.LetCont { name, params, body, cont } =>
          (case C.CVarMap.find (cusage, name) of
               SOME (ref C_NEVER) => ( #simplificationOccurred ctx := true
                                     ; simplifyCExp (ctx, env, cenv, subst, csubst, usage, rusage, cusage, crusage, cont)
                                     )
             | SOME (ref C_ONCE_DIRECT) => let val () = #simplificationOccurred ctx := true
                                               val body' = simplifyCExp (ctx, env, cenv, subst, csubst, usage, rusage, cusage, crusage, body)
                                               val cenv' = C.CVarMap.insert (cenv, name, (params, SOME body'))
                                           in simplifyCExp (ctx, env, cenv', subst, csubst, usage, rusage, cusage, crusage, cont)
                                           end
             | _ => let val body = simplifyCExp (ctx, env, cenv, subst, csubst, usage, rusage, cusage, crusage, body)
                        val cenv = if sizeOfCExp (body, 3) >= 0 then (* Inline small continuations *)
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
              ( #simplificationOccurred ctx := true
              ; simplifyCExp (ctx, env, cenv, subst, csubst, usage, rusage, cusage, crusage, cont)
              )
        | C.Handle { body, handler = (e, h), successfulExitIn, successfulExitOut } =>
          C.Handle { body = simplifyCExp (ctx, env, C.CVarMap.empty (* do not inline across 'handle' *), subst, csubst, usage, rusage, cusage, crusage, body)
                   , handler = (e, simplifyCExp (ctx, env, cenv, subst, csubst, usage, rusage, cusage, crusage, h))
                   , successfulExitIn = successfulExitIn
                   , successfulExitOut = substCVar csubst successfulExitOut
                   }

(* Eliminate assumeDiscardable *)
fun finalizeCExp (ctx, C.Let { exp = C.PrimOp { primOp = F.PrimFnOp Primitives.assumeDiscardable, tyargs, args = [f, arg] }, result = SOME result, cont })
    = let val name = genContSym ctx
      in C.LetCont { name = name
                   , params = [result]
                   , body = finalizeCExp (ctx, cont)
                   , cont = C.App { applied = f, cont = name, args = [arg] }
                   }
      end
  | finalizeCExp (ctx, C.Let { exp = C.PrimOp { primOp = F.PrimFnOp Primitives.assumeDiscardable, tyargs, args = _ }, result = _, cont }) = raise Fail "assumeDiscardable: invalid argument"
  | finalizeCExp (ctx, C.Let { exp as C.PrimOp { primOp, tyargs, args }, result, cont }) = C.Let { exp = exp, result = result, cont = finalizeCExp (ctx, cont) }
  | finalizeCExp (ctx, C.Let { exp as C.Record _, result, cont }) = C.Let { exp = exp, result = result, cont = finalizeCExp (ctx, cont) }
  | finalizeCExp (ctx, C.Let { exp as C.ExnTag _, result, cont }) = C.Let { exp = exp, result = result, cont = finalizeCExp (ctx, cont) }
  | finalizeCExp (ctx, C.Let { exp as C.Projection _, result, cont }) = C.Let { exp = exp, result = result, cont = finalizeCExp (ctx, cont) }
  | finalizeCExp (ctx, C.Let { exp = C.Abs { contParam, params, body }, result, cont })
    = C.Let { exp = C.Abs { contParam = contParam, params = params, body = finalizeCExp (ctx, body) }
            , result = result
            , cont = finalizeCExp (ctx, cont)
            }
  | finalizeCExp (ctx, e as C.App _) = e
  | finalizeCExp (ctx, e as C.AppCont _) = e
  | finalizeCExp (ctx, C.If { cond, thenCont, elseCont }) = C.If { cond = cond, thenCont = finalizeCExp (ctx, thenCont), elseCont = finalizeCExp (ctx, elseCont) }
  | finalizeCExp (ctx, C.LetRec { defs, cont }) = C.LetRec { defs = List.map (fn (name, k, params, body) => (name, k, params, finalizeCExp (ctx, body))) defs, cont = finalizeCExp (ctx, cont) }
  | finalizeCExp (ctx, C.LetCont { name, params, body, cont }) = C.LetCont { name = name, params = params, body = finalizeCExp (ctx, body), cont = finalizeCExp (ctx, cont) }
  | finalizeCExp (ctx, C.LetRecCont { defs, cont }) = C.LetRecCont { defs = List.map (fn (name, params, body) => (name, params, finalizeCExp (ctx, body))) defs, cont = finalizeCExp (ctx, cont) }
  | finalizeCExp (ctx, C.Handle { body, handler = (e, h), successfulExitIn, successfulExitOut }) = C.Handle { body = finalizeCExp (ctx, body), handler = (e, finalizeCExp (ctx, h)), successfulExitIn = successfulExitIn, successfulExitOut = successfulExitOut }
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
fun go (env, level, C.Let { exp = C.Abs { contParam, params, body}, result = SOME result, cont }, acc)
    = let val acc = go (env, 0, body, acc)
      in go (env, level, cont, acc)
      end
  | go (env, level, C.Let { exp, result, cont }, acc) = go (env, level, cont, acc)
  | go (env, level, C.App { applied, cont, args }, acc) = escape (env, level, cont, acc)
  | go (env, level, C.AppCont { applied, args }, acc) = direct (env, level, applied, acc)
  | go (env, level, C.If { cond, thenCont, elseCont }, acc) = go (env, level, elseCont, go (env, level, thenCont, acc))
  | go (env, level, C.LetRec { defs, cont }, acc) = let val acc = List.foldl (fn ((f, k, params, body), acc) =>
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
  | go (env, level, C.Handle { body, handler = (e, h), successfulExitIn, successfulExitOut }, acc)
    = let val outerDestinations = (go (env, level + 1, h, C.CVarSet.empty))
      in env := C.CVarMap.insert (!env, successfulExitIn, { escapes = ref false, level = level, outerDestinations = C.CVarSet.singleton successfulExitOut })
       ; C.CVarSet.app (fn k => ignore (escape (env, level + 1, k, C.CVarSet.empty))) outerDestinations
       ; go (env, level, body, C.CVarSet.union (acc, outerDestinations))
      end
fun contEscape cexp = let val env = ref C.CVarMap.empty
                          val _ = go (env, 0, cexp, C.CVarSet.empty)
                      in C.CVarMap.map (fn { escapes, ... } => !escapes) (!env)
                      end
end (* local *)
end; (* structure CpsAnalyze *)
