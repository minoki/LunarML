(*
 * Copyright (c) 2022 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure CSyntax = struct
type Var = TypedSyntax.VId
type Tag = string
datatype Value = Var of Var
               | Unit
(*
               | BoolConst of bool
               | IntConst of IntInf.int
               | WordConst of IntInf.int
               | RealConst of Numeric.float_notation
               | CharConst of int
               | StringConst of int vector
*)
datatype CExp = PrimOp of { primOp : FSyntax.PrimOp, tyargs : FSyntax.Ty list, args : Value list, result : Var, cont : CExp, exnCont : Var }
              | Record of { fields : Value Syntax.LabelMap.map, result : Var, cont : CExp } (* non-empty record *)
              | ExnTag of { name : string, payloadTy : FSyntax.Ty option, result : Var, cont : CExp }
              | Projection of { label : Syntax.Label
                              , record : Value
                              , result : Var
                              , cont : CExp
                              }
              | App of { applied : Value, args : Value list } (* tail call *)
              | If of { cond : Value
                      , thenCont : CExp
                      , elseCont : CExp
                      }
              | Fix of { functions : (Var * Var list * CExp) list, cont : CExp } (* function *)
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

fun StructToRecord { valMap, strMap, exnTagMap } = let val entries = Syntax.VIdMap.foldri (fn (vid, path, xs) => (Syntax.IdentifierLabel (Syntax.getVIdName vid ^ ".tag"), F.PathToExp path) :: xs) [] exnTagMap
                                                       val entries = Syntax.StrIdMap.foldri (fn (Syntax.MkStrId name, path, xs) => (Syntax.IdentifierLabel ("_" ^ name), F.PathToExp path) :: xs) entries strMap
                                                       val entries = Syntax.VIdMap.foldri (fn (vid, path, xs) => (Syntax.IdentifierLabel (Syntax.getVIdName vid), F.PathToExp path) :: xs) entries valMap
                                                   in F.RecordExp entries
                                                   end
fun SLabelToLabel (F.ValueLabel vid) = Syntax.IdentifierLabel (Syntax.getVIdName vid)
  | SLabelToLabel (F.StructLabel (Syntax.MkStrId name)) = Syntax.IdentifierLabel ("_" ^ name)
  | SLabelToLabel (F.ExnTagLabel vid) = Syntax.IdentifierLabel (Syntax.getVIdName vid ^ ".tag")

(* transform : Context -> F.Exp -> (C.Value -> C.CExp) -> C.CExp *)
(* transformT : Context -> F.Exp -> (* continuation variable *) C.Var -> C.CExp *)
fun transform (ctx : Context) (exp : F.Exp) (h : C.Var (* exception handler *)) (k : C.Value -> C.CExp) : C.CExp
    = (case exp of
           F.PrimExp (primOp, tyargs, args) => mapCont (fn (e, cont) => transform ctx e h cont)
                                                       (Vector.foldr (op ::) [] args)
                                                       (fn args => let val result = genSym ctx
                                                                   in C.PrimOp { primOp = primOp, tyargs = Vector.foldr (op ::) [] tyargs, args = args, result = result, cont = k (C.Var result), exnCont = h }
                                                                   end
                                                       )
         | F.VarExp vid => k (C.Var vid)
         | F.RecordExp [] => k C.Unit
         | F.RecordExp fields => mapCont (fn ((label, exp), cont) => transform ctx exp h (fn v => cont (label, v)))
                                         fields
                                         (fn fields => let val result = genSym ctx
                                                       in C.Record { fields = List.foldl Syntax.LabelMap.insert' Syntax.LabelMap.empty fields, result = result, cont = k (C.Var result) }
                                                       end
                                         )
         | F.LetExp (F.ValDec (vid, _, exp1), exp2) => let val j = genContSym ctx
                                                       in C.Fix { functions = [(j, [vid], transform ctx exp2 h k)] (* letcont *)
                                                                , cont = transformT ctx exp1 h j
                                                                }
                                                       end
         | F.LetExp (F.RecValDec decs, exp2) => C.Fix { functions = List.map (fn (vid, _, exp1) =>
                                                                                 let val contParam = genContSym ctx
                                                                                     val exnContParam = genExnContSym ctx
                                                                                 in case stripTyAbs exp1 of
                                                                                        F.FnExp (param, _, body) => (vid, [contParam, exnContParam, param], transformT ctx body exnContParam contParam)
                                                                                      | _ => raise Fail "RecValDec"
                                                                                 end
                                                                             ) decs
                                                      , cont = transform ctx exp2 h k
                                                      }
         | F.LetExp (F.UnpackDec (_, _, vid, _, exp1), exp2) => let val j = genContSym ctx
                                                                in C.Fix { functions = [(j, [vid], transform ctx exp2 h k)] (* letcont *)
                                                                         , cont = transformT ctx exp1 h j
                                                                         }
                                                                end
         | F.LetExp (F.IgnoreDec exp1, exp2) => let val j = genContSym ctx
                                                    val ignored = genSym ctx
                                                in C.Fix { functions = [(j, [ignored], transform ctx exp2 h k)] (* letcont *)
                                                         , cont = transformT ctx exp1 h j
                                                         }
                                                end
         | F.LetExp (F.DatatypeDec _, exp) => transform ctx exp h k
         | F.LetExp (F.ExceptionDec { name, tagName, payloadTy }, exp) => C.ExnTag { name = name
                                                                                   , payloadTy = payloadTy
                                                                                   , result = tagName
                                                                                   , cont = transform ctx exp h k
                                                                                   }
         | F.LetExp (F.ExportValue _, _) => raise Fail "ExportValue in CPS: not supported"
         | F.LetExp (F.ExportModule _, _) => raise Fail "ExportModule in CPS: not supported"
         | F.LetExp (F.GroupDec (_, decs), exp) => transform ctx (List.foldr F.LetExp exp decs) h k
         | F.AppExp (applied, arg) => transform ctx applied h (fn f =>
                                                                  transform ctx arg h (fn v =>
                                                                                          let val j = genContSym ctx
                                                                                              val t = genSym ctx
                                                                                          in C.Fix { functions = [(j, [t], k (C.Var t))] (* letcont *)
                                                                                                   , cont = C.App { applied = f, args = [C.Var j, C.Var h, v] }
                                                                                                   }
                                                                                          end
                                                                                      )
                                                              )
         | F.HandleExp { body, exnName, handler } => let val j = genContSym ctx
                                                         val x = genSym ctx
                                                         val h' = genExnContSym ctx
                                                     in C.Fix { functions = [(j, [x], k (C.Var x))]
                                                              , cont = C.Fix { functions = [(h', [exnName], transformT ctx handler h j)]
                                                                             , cont = transformT ctx body h' j
                                                                             }
                                                              }
                                                     end
         | F.IfThenElseExp (e1, e2, e3) => transform ctx e1 h (fn e1 =>
                                                                  let val j = genContSym ctx
                                                                      val t = genSym ctx
                                                                  in C.Fix { functions = [(j, [t], k (C.Var t))] (* letcont *)
                                                                           , cont = C.If { cond = e1
                                                                                         , thenCont = transformT ctx e2 h j
                                                                                         , elseCont = transformT ctx e3 h j
                                                                                         }
                                                                           }
                                                                  end
                                                              )
         | F.CaseExp _ => raise Fail "CaseExp: not supported here"
         | F.FnExp (vid, _, body) => let val f = genSym ctx
                                         val kk = genContSym ctx
                                         val hh = genExnContSym ctx
                                     in C.Fix { functions = [(f, [kk, hh, vid], transformT ctx body hh kk)]
                                              , cont = k (C.Var f)
                                              }
                                     end
         | F.ProjectionExp { label, record } => transform ctx record h (fn record =>
                                                                           let val x = genSym ctx
                                                                           in C.Projection { label = label
                                                                                           , record = record
                                                                                           , result = x
                                                                                           , cont = k (C.Var x)
                                                                                           }
                                                                           end
                                                                       )
         | F.TyAbsExp (_, _, exp) => transform ctx exp h k
         | F.TyAppExp (exp, _) => transform ctx exp h k
         | F.StructExp maps => transform ctx (StructToRecord maps) h k
         | F.SProjectionExp (exp, label) => transform ctx exp h (fn exp =>
                                                                    let val x = genSym ctx
                                                                    in C.Projection { label = SLabelToLabel label
                                                                                    , record = exp
                                                                                    , result = x
                                                                                    , cont = k (C.Var x)
                                                                                    }
                                                                    end
                                                                )
         | F.PackExp { payloadTy, exp, packageTy } => transform ctx exp h k
      )
and transformT (ctx : Context) (exp : F.Exp) (h : C.Var (* exception handler *)) (k : C.Var (* continuation variable *)) : C.CExp
    = (case exp of
           F.PrimExp (primOp, tyargs, args) => mapCont (fn (e, cont) => transform ctx e h cont)
                                                       (Vector.foldr (op ::) [] args)
                                                       (fn args => let val result = genSym ctx
                                                                   in C.PrimOp { primOp = primOp
                                                                               , tyargs = Vector.foldr (op ::) [] tyargs
                                                                               , args = args
                                                                               , result = result
                                                                               , cont = C.App { applied = C.Var k, args = [C.Var result] } (* apply continuation *)
                                                                               , exnCont = h
                                                                               }
                                                                   end
                                                       )
         | F.VarExp vid => C.App { applied = C.Var k, args = [C.Var vid] } (* apply continuation *)
         | F.RecordExp [] => C.App { applied = C.Var k, args = [C.Unit] } (* apply continuation *)
         | F.RecordExp fields => mapCont (fn ((label, exp), cont) => transform ctx exp h (fn v => cont (label, v)))
                                         fields
                                         (fn fields => let val result = genSym ctx
                                                       in C.Record { fields = List.foldl Syntax.LabelMap.insert' Syntax.LabelMap.empty fields
                                                                   , result = result
                                                                   , cont = C.App { applied = C.Var k, args = [C.Var result] } (* apply continuation *)
                                                                   }
                                                       end
                                         )
         | F.LetExp (F.ValDec (vid, _, exp1), exp2) => let val j = genContSym ctx
                                                       in C.Fix { functions = [(j, [vid], transformT ctx exp2 h k)] (* letcont *)
                                                                , cont = transformT ctx exp1 h j
                                                                }
                                                       end
         | F.LetExp (F.RecValDec decs, exp2) => C.Fix { functions = List.map (fn (vid, _, exp1) =>
                                                                                 let val contParam = genContSym ctx
                                                                                     val exnContParam = genExnContSym ctx
                                                                                 in case stripTyAbs exp1 of
                                                                                        F.FnExp (param, _, body) => (vid, [contParam, exnContParam, param], transformT ctx body exnContParam contParam)
                                                                                      | _ => raise Fail "RecValDec"
                                                                                 end
                                                                             ) decs
                                                      , cont = transformT ctx exp2 h k
                                                      }
         | F.LetExp (F.UnpackDec (_, _, vid, _, exp1), exp2) => let val j = genContSym ctx
                                                                in C.Fix { functions = [(j, [vid], transformT ctx exp2 h k)] (* letcont *)
                                                                         , cont = transformT ctx exp1 h j
                                                                         }
                                                                end
         | F.LetExp (F.IgnoreDec exp1, exp2) => let val j = genContSym ctx
                                                    val ignored = genSym ctx
                                                in C.Fix { functions = [(j, [ignored], transformT ctx exp2 h k)] (* letcont *)
                                                         , cont = transformT ctx exp1 h j
                                                         }
                                                end
         | F.LetExp (F.DatatypeDec _, exp) => transformT ctx exp h k
         | F.LetExp (F.ExceptionDec { name, tagName, payloadTy }, exp) => C.ExnTag { name = name
                                                                                   , payloadTy = payloadTy
                                                                                   , result = tagName
                                                                                   , cont = transformT ctx exp h k
                                                                                   }
         | F.LetExp (F.ExportValue _, _) => raise Fail "ExportValue in CPS: not supported"
         | F.LetExp (F.ExportModule _, _) => raise Fail "ExportModule in CPS: not supported"
         | F.LetExp (F.GroupDec (_, decs), exp) => transformT ctx (List.foldr F.LetExp exp decs) h k
         | F.AppExp (applied, arg) => transform ctx applied h (fn f =>
                                                                  transform ctx arg h (fn v =>
                                                                                          C.App { applied = f, args = [C.Var k, C.Var h, v] }
                                                                                      )
                                                              )
         | F.HandleExp { body, exnName, handler } => let val h' = genExnContSym ctx
                                                     in C.Fix { functions = [(h', [exnName], transformT ctx handler h k)]
                                                              , cont = transformT ctx body h' k
                                                              }
                                                     end
         | F.IfThenElseExp (e1, e2, e3) => transform ctx e1 h (fn e1 =>
                                                                  C.If { cond = e1
                                                                       , thenCont = transformT ctx e2 h k
                                                                       , elseCont = transformT ctx e3 h k
                                                                       }
                                                              )
         | F.CaseExp _ => raise Fail "CaseExp: not supported here"
         | F.FnExp (vid, _, body) => let val f = genSym ctx
                                         val kk = genContSym ctx
                                         val hh = genExnContSym ctx
                                     in C.Fix { functions = [(f, [kk, hh, vid], transformT ctx body hh kk)]
                                              , cont = C.App { applied = C.Var k, args = [C.Var f] } (* apply continuation *)
                                              }
                                     end
         | F.ProjectionExp { label, record } => transform ctx record h (fn record =>
                                                                           let val x = genSym ctx
                                                                           in C.Projection { label = label
                                                                                           , record = record
                                                                                           , result = x
                                                                                           , cont = C.App { applied = C.Var k, args = [C.Var x] } (* apply continuation *)
                                                                                           }
                                                                           end
                                                                       )
         | F.TyAbsExp (_, _, exp) => transformT ctx exp h k
         | F.TyAppExp (exp, _) => transformT ctx exp h k
         | F.StructExp maps => transformT ctx (StructToRecord maps) h k
         | F.SProjectionExp (exp, label) => transform ctx exp h (fn exp =>
                                                                    let val x = genSym ctx
                                                                    in C.Projection { label = SLabelToLabel label
                                                                                    , record = exp
                                                                                    , result = x
                                                                                    , cont = C.App { applied = C.Var k, args = [C.Var x] } (* apply continuation *)
                                                                                    }
                                                                    end
                                                                )
         | F.PackExp { payloadTy, exp, packageTy } => transformT ctx exp h k
      )
fun transformDecs (ctx : Context) ([] : F.Dec list) (h : C.Var) (k : C.Var) : C.CExp
    = C.App { applied = C.Var k, args = [] } (* apply continuation *)
  | transformDecs ctx (dec :: decs) h k
    = (case dec of
           F.ValDec (vid, _, exp) => let val j = genContSym ctx
                                     in C.Fix { functions = [(j, [vid], transformDecs ctx decs h k)] (* letcont *)
                                              , cont = transformT ctx exp h j
                                              }
                                     end
         | F.RecValDec decs' => C.Fix { functions = List.map (fn (vid, _, exp) =>
                                                                 let val contParam = genContSym ctx
                                                                     val exnContParam = genExnContSym ctx
                                                                 in case stripTyAbs exp of
                                                                        F.FnExp (param, _, body) => (vid, [contParam, exnContParam, param], transformT ctx body exnContParam contParam)
                                                                      | _ => raise Fail "RecValDec"
                                                                 end
                                                             ) decs'
                                      , cont = transformDecs ctx decs h k
                                      }
         | F.UnpackDec (_, _, vid, _, exp) => let val j = genContSym ctx
                                              in C.Fix { functions = [(j, [vid], transformDecs ctx decs h k)] (* letcont *)
                                                       , cont = transformT ctx exp h j
                                                       }
                                              end
         | F.IgnoreDec exp => let val j = genContSym ctx
                                  val ignored = genSym ctx
                              in C.Fix { functions = [(j, [ignored], transformDecs ctx decs h k)] (* letcont *)
                                       , cont = transformT ctx exp h j
                                       }
                              end
         | F.DatatypeDec _ => transformDecs ctx decs h k
         | F.ExceptionDec { name, tagName, payloadTy } => C.ExnTag { name = name
                                                                   , payloadTy = payloadTy
                                                                   , result = tagName
                                                                   , cont = transformDecs ctx decs h k
                                                                   }
         | F.ExportValue _ => raise Fail "ExportValue in CPS: not supported"
         | F.ExportModule _ => raise Fail "ExportModule in CPS: not supported"
         | F.GroupDec (_, decs') => transformDecs ctx (decs' @ decs) h k
      )
end
end;
