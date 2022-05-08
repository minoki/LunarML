(*
 * Copyright (c) 2022 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure LuaTransform = struct
structure L = LuaSyntax
type Context = { nextId : int ref }
datatype Variable = Plain of TypedSyntax.VId
                  | Index of TypedSyntax.VId * int
type Env = { currentLocals : int
           , locals : (TypedSyntax.VId * int ref) option
           , valMap : Variable TypedSyntax.VIdMap.map
           }
val initialEnv = { currentLocals = 61 (* See mlinit.lua *)
                 , locals = NONE
                 , valMap = TypedSyntax.VIdMap.empty
                 }
fun genSym (ctx : Context) = let val n = !(#nextId ctx)
                                 val _ = #nextId ctx := n + 1
                             in TypedSyntax.MkVId ("LOCAL", n)
                             end
val LOCAL_LIMIT = 190
fun doExp (ctx : Context) (env : Env) (exp as L.ConstExp ct) = exp
  | doExp ctx env (exp as L.VarExp (L.PredefinedId _)) = exp
  | doExp ctx env (exp as L.VarExp (L.UserDefinedId vid)) = (case TypedSyntax.VIdMap.find (#valMap env, vid) of
                                                                 NONE => exp
                                                               | SOME (Plain vid) => L.VarExp (L.UserDefinedId vid)
                                                               | SOME (Index (locals, n)) => L.IndexExp (L.VarExp (L.UserDefinedId locals), L.ConstExp (L.Numeral (Int.toString n)))
                                                            )
  | doExp ctx env (L.TableExp fields) = L.TableExp (Vector.map (fn (key, value) => (key, doExp ctx env value)) fields)
  | doExp ctx env (L.CallExp (exp, args)) = L.CallExp (doExp ctx env exp, Vector.map (doExp ctx env) args)
  | doExp ctx env (L.MethodExp (self, method, args)) = L.MethodExp (doExp ctx env self, method, Vector.map (doExp ctx env) args)
  | doExp ctx env (L.FunctionExp (params, body))
    = let val innerEnv = { currentLocals = Vector.length params
                         , locals = NONE
                         , valMap = Vector.foldl (fn (L.PredefinedId param, valMap) => valMap
                                                 | (L.UserDefinedId vid, valMap) => TypedSyntax.VIdMap.insert (valMap, vid, Plain vid)
                                                 ) (#valMap env) params
                         }
      in L.FunctionExp (params, doBlock ctx innerEnv body)
      end
  | doExp ctx env (L.BinExp (binOp, a, b)) = L.BinExp (binOp, doExp ctx env a, doExp ctx env b)
  | doExp ctx env (L.UnaryExp (unOp, a)) = L.UnaryExp (unOp, doExp ctx env a)
  | doExp ctx env (L.IndexExp (a, b)) = L.IndexExp (doExp ctx env a, doExp ctx env b)
and doStat ctx env (L.LocalStat (vars, exps))
    = let val newLocals = #currentLocals env + Vector.length vars
      in if newLocals > LOCAL_LIMIT then
             let val (n, locals, r, dec) = case #locals env of
                                               SOME (locals, r) => (#currentLocals env, locals, r, [])
                                             | NONE => let val locals = genSym ctx
                                                       in (#currentLocals env + 1, locals, ref 1, [L.LocalStat (vector [locals], vector [L.TableExp (vector [])])])
                                                       end
                 val (vars, valMap) = Vector.foldl (fn (vid, (acc, valMap)) => let val i = !r
                                                                               in r := i + 1
                                                                                ; (L.IndexExp (L.VarExp (L.UserDefinedId locals), L.ConstExp (L.Numeral (Int.toString i))) :: acc, TypedSyntax.VIdMap.insert (valMap, vid, Index (locals, i)))
                                                                               end
                                                   ) ([], #valMap env) vars
                 val newEnv = { currentLocals = n
                              , locals = SOME (locals, r)
                              , valMap = valMap
                              }
             in if Vector.length exps > 0 then
                    (newEnv, dec @ [L.AssignStat (vector (List.rev vars), Vector.map (doExp ctx env) exps)])
                else
                    (newEnv, dec)
             end
         else
             let val newEnv = { currentLocals = newLocals, locals = #locals env, valMap = #valMap env }
             in (newEnv, [L.LocalStat (vars, Vector.map (doExp ctx env) exps)])
             end
      end
  | doStat ctx env (L.AssignStat (vars, exps)) = (env, [L.AssignStat (Vector.map (doExp ctx env) vars, Vector.map (doExp ctx env) exps)])
  | doStat ctx env (L.CallStat (exp, args)) = (env, [L.CallStat (doExp ctx env exp, Vector.map (doExp ctx env) args)])
  | doStat ctx env (L.MethodStat (self, method, args)) = (env, [L.MethodStat (doExp ctx env self, method, Vector.map (doExp ctx env) args)])
  | doStat ctx env (L.IfStat (cond, thenPart, elsePart)) = (env, [L.IfStat (doExp ctx env cond, doBlock ctx env thenPart, doBlock ctx env elsePart)])
  | doStat ctx env (L.LocalFunctionStat (name, params, body))
    = let val newLocals = #currentLocals env + 1
      in if newLocals > LOCAL_LIMIT then
             let val (n, locals, r, dec) = case #locals env of
                                               SOME (locals, r) => (#currentLocals env, locals, r, [])
                                             | NONE => let val locals = genSym ctx
                                                       in (#currentLocals env + 1, locals, ref 1, [L.LocalStat (vector [locals], vector [L.TableExp (vector [])])])
                                                       end
                 val (name, valMap) = let val i = !r
                                      in r := i + 1
                                       ; (L.IndexExp (L.VarExp (L.UserDefinedId locals), L.ConstExp (L.Numeral (Int.toString i))), TypedSyntax.VIdMap.insert (#valMap env, name, Index (locals, i)))
                                      end
                 val newEnv = { currentLocals = n
                              , locals = SOME (locals, r)
                              , valMap = valMap
                              }
                 val innerEnv = { currentLocals = Vector.length params
                                , locals = NONE
                                , valMap = Vector.foldl (fn (L.PredefinedId params, valMap) => valMap
                                                        | (L.UserDefinedId vid, valMap) => TypedSyntax.VIdMap.insert (valMap, vid, Plain vid)
                                                        ) valMap params
                                }
             in (newEnv, dec @ [L.AssignStat (vector [name], vector [L.FunctionExp (params, doBlock ctx innerEnv body)])])
             end
         else
             let val innerEnv = { currentLocals = Vector.length params
                                , locals = NONE
                                , valMap = Vector.foldl (fn (L.PredefinedId params, valMap) => valMap
                                                        | (L.UserDefinedId vid, valMap) => TypedSyntax.VIdMap.insert (valMap, vid, Plain vid)
                                                        ) (#valMap env) params
                                }
             in (env, [L.LocalFunctionStat (name, params, doBlock ctx innerEnv body)])
             end
      end
  | doStat ctx env (L.ReturnStat results) = (env, [L.ReturnStat (Vector.map (doExp ctx env) results)])
  | doStat ctx env (L.DoStat block) = (env, [L.DoStat (doBlock ctx env block)])
and doBlock ctx env stats = vector (List.concat (List.rev (#2 (Vector.foldl (fn (stat, (env, acc)) => let val (env, stat) = doStat ctx env stat
                                                                                                      in (env, stat :: acc)
                                                                                                      end
                                                                            ) (env, []) stats))))
end;
