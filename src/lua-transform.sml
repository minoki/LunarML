(*
 * Copyright (c) 2022 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure LuaTransform = struct
structure L = LuaSyntax

fun freeVarsExp (_, L.ConstExp _) acc = acc
  | freeVarsExp (bound, L.VarExp x) acc = if L.IdSet.member (bound, x) then
                                              acc
                                          else
                                              L.IdSet.add (acc, x)
  | freeVarsExp (bound, L.TableExp fields) acc = Vector.foldl (fn ((key, x), acc) => freeVarsExp (bound, x) acc) acc fields
  | freeVarsExp (bound, L.CallExp (x, ys)) acc = Vector.foldl (fn (y, acc) => freeVarsExp (bound, y) acc) (freeVarsExp (bound, x) acc) ys
  | freeVarsExp (bound, L.MethodExp (x, _, ys)) acc = Vector.foldl (fn (y, acc) => freeVarsExp (bound, y) acc) (freeVarsExp (bound, x) acc) ys
  | freeVarsExp (bound, L.FunctionExp (params, body)) acc = let val bound' = Vector.foldl (fn (id, bound) => L.IdSet.add (bound, id)) bound params
                                                            in freeVarsBlock (bound', body) acc
                                                            end
  | freeVarsExp (bound, L.BinExp (_, x, y)) acc = freeVarsExp (bound, x) (freeVarsExp (bound, y) acc)
  | freeVarsExp (bound, L.UnaryExp (_, x)) acc = freeVarsExp (bound, x) acc
  | freeVarsExp (bound, L.IndexExp (x, y)) acc = freeVarsExp (bound, x) (freeVarsExp (bound, y) acc)
and freeVarsStat (bound, L.LocalStat (vids, exps)) acc = let val acc = List.foldl (fn (x, acc) => freeVarsExp (bound, x) acc) acc exps
                                                         in (List.foldl (fn ((vid, _), bound) => L.IdSet.add (bound, L.UserDefinedId vid)) bound vids, acc)
                                                         end
  | freeVarsStat (bound, L.AssignStat (lhs, rhs)) acc = let val acc = List.foldl (fn (x, acc) => freeVarsExp (bound, x) acc) acc lhs
                                                            val acc = List.foldl (fn (x, acc) => freeVarsExp (bound, x) acc) acc rhs
                                                        in (bound, acc)
                                                        end
  | freeVarsStat (bound, L.CallStat (x, ys)) acc = let val acc = freeVarsExp (bound, x) acc
                                                       val acc = Vector.foldl (fn (x, acc) => freeVarsExp (bound, x) acc) acc ys
                                                   in (bound, acc)
                                                   end
  | freeVarsStat (bound, L.MethodStat (x, _, ys)) acc = let val acc = freeVarsExp (bound, x) acc
                                                            val acc = Vector.foldl (fn (x, acc) => freeVarsExp (bound, x) acc) acc ys
                                                        in (bound, acc)
                                                        end
  | freeVarsStat (bound, L.IfStat (x, then', else')) acc = let val acc = freeVarsExp (bound, x) acc
                                                               val acc = freeVarsBlock (bound, then') acc
                                                               val acc = freeVarsBlock (bound, else') acc
                                                           in (bound, acc)
                                                           end
  | freeVarsStat (bound, L.LocalFunctionStat (vid, params, body)) acc = let val bound' = L.IdSet.add (bound, L.UserDefinedId vid)
                                                                            val bound'' = Vector.foldl (fn (id, bound) => L.IdSet.add (bound, id)) bound' params
                                                                            val acc = freeVarsBlock (bound'', body) acc
                                                                        in (bound', acc)
                                                                        end
  | freeVarsStat (bound, L.ReturnStat xs) acc = (bound, Vector.foldl (fn (x, acc) => freeVarsExp (bound, x) acc) acc xs)
  | freeVarsStat (bound, L.DoStat block) acc = let val acc = freeVarsBlock (bound, block) acc
                                               in (bound, acc)
                                               end
and freeVarsBlock (bound, block) acc = let val (_, acc) = Vector.foldl (fn (stat, (bound, acc)) => freeVarsStat (bound, stat) acc) (bound, acc) block
                                       in acc
                                       end

fun substExp map (x as L.ConstExp _) = x
  | substExp map (x as L.VarExp id) = (case L.IdMap.find (map, id) of
                                           NONE => x
                                         | SOME y => y
                                      )
  | substExp map (L.TableExp fields) = L.TableExp (Vector.map (fn (key, x) => (key, substExp map x)) fields)
  | substExp map (L.CallExp (x, ys)) = L.CallExp (substExp map x, Vector.map (substExp map) ys)
  | substExp map (L.MethodExp (x, name, ys)) = L.MethodExp (substExp map x, name, Vector.map (substExp map) ys)
  | substExp map (L.FunctionExp (params, body)) = let val map' = Vector.foldl (fn (id, map) => if L.IdMap.inDomain (map, id) then
                                                                                                   #1 (L.IdMap.remove (map, id))
                                                                                               else
                                                                                                   map) map params
                                                  in L.FunctionExp (params, substBlock map' body)
                                                  end
  | substExp map (L.BinExp (binOp, x, y)) = L.BinExp (binOp, substExp map x, substExp map y)
  | substExp map (L.UnaryExp (unOp, x)) = L.UnaryExp (unOp, substExp map x)
  | substExp map (L.IndexExp (x, y)) = L.IndexExp (substExp map x, substExp map y)
and substStat map (L.LocalStat (lhs, rhs)) = let val rhs = List.map (substExp map) rhs
                                                 val map' = List.foldl (fn ((id, _), map) => let val id = L.UserDefinedId id
                                                                                             in if L.IdMap.inDomain (map, id) then
                                                                                                    #1 (L.IdMap.remove (map, id))
                                                                                                else
                                                                                                    map
                                                                                             end) map lhs
                                             in (map', L.LocalStat (lhs, rhs))
                                             end
  | substStat map (L.AssignStat (lhs, rhs)) = (map, L.AssignStat (List.map (substExp map) lhs, List.map (substExp map) rhs))
  | substStat map (L.CallStat (x, ys)) = (map, L.CallStat (substExp map x, Vector.map (substExp map) ys))
  | substStat map (L.MethodStat (x, name, ys)) = (map, L.MethodStat (substExp map x, name, Vector.map (substExp map) ys))
  | substStat map (L.IfStat (exp, then', else')) = (map, L.IfStat (substExp map exp, substBlock map then', substBlock map else'))
  | substStat map (L.LocalFunctionStat (vid, params, body)) = let val map' = let val vid = L.UserDefinedId vid
                                                                             in if L.IdMap.inDomain (map, vid) then
                                                                                    #1 (L.IdMap.remove (map, vid))
                                                                                else
                                                                                    map
                                                                             end
                                                                  val map'' = Vector.foldl (fn (id, map) => if L.IdMap.inDomain (map, id) then
                                                                                                                #1 (L.IdMap.remove (map, id))
                                                                                                            else
                                                                                                                map) map params
                                                              in (map', L.LocalFunctionStat (vid, params, substBlock map'' body))
                                                              end
  | substStat map (L.ReturnStat xs) = (map, L.ReturnStat (Vector.map (substExp map) xs))
  | substStat map (L.DoStat block) = (map, L.DoStat (substBlock map block))
and substBlock map block = let val (_, revStats) = Vector.foldl (fn (stat, (map, revStats)) =>
                                                                    let val (map, stat) = substStat map stat
                                                                    in (map, stat :: revStats)
                                                                    end
                                                                ) (map, []) block
                           in Vector.fromList (List.rev revStats)
                           end

type Context = { nextId : int ref, maxUpvalue : int }
datatype Variable = Plain of TypedSyntax.VId
                  | Index of TypedSyntax.VId * int
(* See mlinit.lua *)
val mlinit_lua = List.foldl (fn (name, m) => L.IdMap.insert (m, L.PredefinedId name, L.CONST)) L.IdMap.empty
                            ["_ENV","assert","error","getmetatable","pairs","pcall","setmetatable","math","math_abs","math_type","math_maxinteger","math_mininteger","math_ult","string","string_format","table","table_pack","table_unpack","_Unit_EQUAL","_Record_EQUAL","_id","_exn_meta","_Match_tag","_Match","_Bind_tag","_Bind","_Overflow_tag","_Overflow","_Div_tag","_Div","_Size_tag","_Size","_Subscript_tag","_Subscript","_Fail_tag","_Fail","_LuaError_tag","_LuaError","_handle","_exnName","__exn_instanceof","_raise","__Int_add","__Int_sub","__Int_mul","__Int_div","__Int_mod","_Int_negate","_Int_abs","__Word_div","__Word_mod","__Word_LT","_nil","_cons","_list","_ref","_Array_array","_VectorOrArray_fromList","_VectorOrArray_tabulate","_Vector_concat","_Lua_global","_Lua_call","_Lua_method","_Lua_newTable","_Lua_function"]
val mlinit_luajit = List.foldl (fn (name, m) => L.IdMap.insert (m, L.PredefinedId name, L.CONST)) L.IdMap.empty
                            ["assert","error","getmetatable","pairs","pcall","setmetatable","math","math_abs","math_floor","math_modf","string","string_format","table","select","table_pack","table_unpack","_Unit_EQUAL","_Record_EQUAL","_id","_exn_meta","_Match_tag","_Match","_Bind_tag","_Bind","_Overflow_tag","_Overflow","_Div_tag","_Div","_Size_tag","_Size","_Subscript_tag","_Subscript","_Fail_tag","_Fail","_LuaError_tag","_LuaError","_handle","_exnName","__exn_instanceof","_raise","MIN_INT32","MAX_INT32","__Int_add","__Int_sub","__Int_mul","__Int_div","__Int_quot","__Int_mod","_Int_negate","_Int_abs","__Word_add","__Word_sub","__Word_mul","__Word_div","__Word_mod","_Word_negate","__Word_LT","NEGATIVE_ZERO","__Real_mul","_nil","_cons","_list","_ref","_Array_array","_VectorOrArray_fromList","_VectorOrArray_tabulate","_Vector_concat","_Lua_global","_Lua_call","_Lua_method","_Lua_newTable","_Lua_function"]
fun genSym (ctx : Context, name) = let val n = !(#nextId ctx)
                                       val _ = #nextId ctx := n + 1
                                   in TypedSyntax.MkVId (name, n)
                                   end
structure ProcessUpvalue = struct
type Env = { valMap : Variable TypedSyntax.VIdMap.map
           , bound : L.VarAttr L.IdMap.map
           , dynamic : L.VarAttr L.IdMap.map
           }
val initialEnv : Env = { valMap = TypedSyntax.VIdMap.empty
                       , bound = mlinit_lua
                       , dynamic = mlinit_lua
                       }
val initialEnvForLuaJIT : Env = { valMap = TypedSyntax.VIdMap.empty
                                , bound = mlinit_luajit
                                , dynamic = mlinit_luajit
                                }
fun doExp (ctx : Context) (env : Env) (exp as L.ConstExp ct) = ([], exp)
  | doExp ctx env (exp as L.VarExp (L.PredefinedId _)) = ([], exp)
  | doExp ctx env (exp as L.VarExp (L.UserDefinedId vid)) = ([], case TypedSyntax.VIdMap.find (#valMap env, vid) of
                                                                     NONE => exp
                                                                   | SOME (Plain vid) => L.VarExp (L.UserDefinedId vid)
                                                                   | SOME (Index (locals, n)) => L.IndexExp (L.VarExp (L.UserDefinedId locals), L.ConstExp (L.Numeral (Int.toString n)))
                                                            )
  | doExp ctx env (L.TableExp fields) = let val (decs, fields) = Vector.foldr (fn ((key, value), (decs, fields)) => let val (decs', value) = doExp ctx env value
                                                                                                                    in (decs' @ decs, (key, value) :: fields)
                                                                                                                    end) ([], []) fields
                                        in (decs, L.TableExp (Vector.fromList fields))
                                        end
  | doExp ctx env (L.CallExp (exp, args)) = let val (decs, exp) = doExp ctx env exp
                                                val (decs', args) = Vector.foldr (fn (x, (decs, xs)) => let val (decs', x) = doExp ctx env x
                                                                                                        in (decs' @ decs, x :: xs)
                                                                                                        end) ([], []) args
                                            in (decs @ decs', L.CallExp (exp, vector args))
                                            end
  | doExp ctx env (L.MethodExp (self, method, args)) = let val (decs, self) = doExp ctx env self
                                                           val (decs', args) = Vector.foldr (fn (x, (decs, xs)) => let val (decs', x) = doExp ctx env x
                                                                                                                   in (decs' @ decs, x :: xs)
                                                                                                                   end) ([], []) args
                                                       in (decs @ decs', L.MethodExp (self, method, vector args))
                                                       end
  | doExp ctx env (L.FunctionExp (params, body))
    = let val innerEnv = { valMap = Vector.foldl (fn (L.PredefinedId param, valMap) => valMap
                                                 | (L.UserDefinedId vid, valMap) => TypedSyntax.VIdMap.insert (valMap, vid, Plain vid)
                                                 ) (#valMap env) params
                         , bound = Vector.foldl (fn (p, bound) => L.IdMap.insert (bound, p, L.CONST)) (#bound env) params
                         , dynamic = Vector.foldl (fn (p, dynamic) => L.IdMap.insert (dynamic, p, L.CONST)) (#dynamic env) params
                         }
          val paramSet = Vector.foldl (fn (p, bound) => L.IdSet.add (bound, p)) L.IdSet.empty params
          val fv = freeVarsBlock (paramSet, body) L.IdSet.empty
          val upvaluesMap = L.IdMap.filteri (fn (id, _) => L.IdSet.member (fv, id)) (#dynamic env)
      in if #maxUpvalue ctx < L.IdMap.numItems upvaluesMap then
             let val upvaluesList = L.IdMap.foldli (fn (id, attr, acc) => (id, attr) :: acc) [] upvaluesMap
                 val (constUpvalues, nonconstUpvalues) = List.partition (fn (id, L.CONST) => true | _ => false) upvaluesList
                 val n = List.length nonconstUpvalues
                 val escapeList = if #maxUpvalue ctx - 1 < n then
                                      constUpvalues (* Need a better algorithm *)
                                  else
                                      List.drop (constUpvalues, #maxUpvalue ctx - n - 1)
                 val vid = genSym (ctx, "UPVAL")
                 val dec = L.LocalStat ([(vid, L.CONST)], [L.TableExp (vector (List.rev (#2 (List.foldl (fn ((id, _), (i, acc)) => (i + 1, (L.IntKey i, L.VarExp id) :: acc)) (1, []) escapeList))))])
                 val (_, subst) = List.foldl (fn ((id, _), (i, map)) => (i + 1, L.IdMap.insert (map, id, L.IndexExp (L.VarExp (L.UserDefinedId vid), L.ConstExp (L.Numeral (Int.toString i)))))) (1, L.IdMap.empty) escapeList
                 val (_, body') = doBlock ctx innerEnv (substBlock subst body)
             in ([dec], L.FunctionExp (params, body'))
             end
         else
             let val (_, body') = doBlock ctx innerEnv body
             in ([], L.FunctionExp (params, body'))
             end
      end
  | doExp ctx env (L.BinExp (binOp, a, b)) = let val (decs, a) = doExp ctx env a
                                                 val (decs', b) = doExp ctx env b
                                             in (decs @ decs', L.BinExp (binOp, a, b))
                                             end
  | doExp ctx env (L.UnaryExp (unOp, a)) = let val (decs, a) = doExp ctx env a
                                           in (decs, L.UnaryExp (unOp, a))
                                           end
  | doExp ctx env (L.IndexExp (a, b)) = let val (decs, a) = doExp ctx env a
                                            val (decs', b) = doExp ctx env b
                                        in (decs @ decs', L.IndexExp (a, b))
                                        end
and doStat ctx env (L.LocalStat (vars, exps))
    = let val newEnv = { valMap = #valMap env
                       , bound = List.foldl (fn ((vid, attr), m) => L.IdMap.insert (m, L.UserDefinedId vid, attr)) (#bound env) vars
                       , dynamic = List.foldl (fn ((vid, attr), m) => L.IdMap.insert (m, L.UserDefinedId vid, attr)) (#dynamic env) vars
                       }
          val (decs, exps) = List.foldr (fn (x, (decs, xs)) => let val (decs', x) = doExp ctx env x
                                                               in (decs' @ decs, x :: xs)
                                                               end) ([], []) exps
      in case decs of
             [] => (newEnv, [L.LocalStat (vars, exps)])
           | _ :: _ => (newEnv, [L.LocalStat (List.map (fn (vid, L.CONST) => (vid, L.LATE_INIT) | x => x) vars, []), L.DoStat (vector (decs @ [L.AssignStat (List.map (fn (vid, _) => L.VarExp (L.UserDefinedId vid)) vars, exps)]))])
      end
  | doStat ctx env (L.AssignStat (vars, exps)) = let val (decs, vars) = List.foldr (fn (x, (decs, xs)) => let val (decs', x) = doExp ctx env x
                                                                                                          in (decs' @ decs, x :: xs)
                                                                                                          end) ([], []) vars
                                                     val (decs', exps) = List.foldr (fn (x, (decs, xs)) => let val (decs', x) = doExp ctx env x
                                                                                                           in (decs' @ decs, x :: xs)
                                                                                                           end) ([], []) exps
                                                     val newEnv = { valMap = #valMap env
                                                                  , bound = #bound env
                                                                  , dynamic = List.foldl (fn (L.VarExp id, dynamic) => (case L.IdMap.find (dynamic, id) of
                                                                                                                            NONE => dynamic
                                                                                                                          | SOME L.LATE_INIT => L.IdMap.insert (dynamic, id, L.CONST)
                                                                                                                          | SOME _ => dynamic
                                                                                                                       )
                                                                                         | (_, dynamic) => dynamic) (#dynamic env) vars
                                                                  }
                                                 in (newEnv, decs @ decs' @ [L.AssignStat (vars, exps)])
                                                 end
  | doStat ctx env (L.CallStat (exp, args)) = let val (decs, exp) = doExp ctx env exp
                                                  val (decs', args) = Vector.foldr (fn (x, (decs, xs)) => let val (decs', x) = doExp ctx env x
                                                                                                          in (decs' @ decs, x :: xs)
                                                                                                          end) ([], []) args
                                              in (env, decs @ decs' @ [L.CallStat (exp, vector args)])
                                              end
  | doStat ctx env (L.MethodStat (self, method, args)) = let val (decs, self) = doExp ctx env self
                                                             val (decs', args) = Vector.foldr (fn (x, (decs, xs)) => let val (decs', x) = doExp ctx env x
                                                                                                                     in (decs' @ decs, x :: xs)
                                                                                                                     end) ([], []) args
                                                         in (env, [L.MethodStat (self, method, vector args)])
                                                         end
  | doStat ctx env (L.IfStat (cond, thenPart, elsePart)) = let val (decs, cond) = doExp ctx env cond
                                                               val (dynamic1, thenPart) = doBlock ctx env thenPart
                                                               val (dynamic2, elsePart) = doBlock ctx env elsePart
                                                               val newEnv = { valMap = #valMap env
                                                                            , bound = #bound env
                                                                            , dynamic = L.IdMap.mapi (fn (id, attr as L.CONST) => attr
                                                                                                     | (id, attr as L.MUTABLE) => attr
                                                                                                     | (id, attr as L.LATE_INIT) => (case L.IdMap.find (dynamic1, id) of
                                                                                                                                         SOME L.CONST => (case L.IdMap.find (dynamic2, id) of
                                                                                                                                                              SOME L.CONST => L.CONST
                                                                                                                                                            | _ => attr
                                                                                                                                                         )
                                                                                                                                       | _ => attr
                                                                                                                                    )
                                                                                                     ) (#dynamic env)
                                                                            }
                                                           in (newEnv, decs @ [L.IfStat (cond, thenPart, elsePart)])
                                                           end
  | doStat ctx env (L.LocalFunctionStat (name, params, body))
    = let val (env', stat0) = doStat ctx env (L.LocalStat ([(name, L.LATE_INIT)], []))
          val (env'', stat1) = doStat ctx env' (L.AssignStat ([L.VarExp (L.UserDefinedId name)], [L.FunctionExp (params, body)]))
      in case (stat0, stat1) of
             ([L.LocalStat ([(name', L.LATE_INIT)], [])], [L.AssignStat ([L.VarExp (L.UserDefinedId name'')], [L.FunctionExp (params', body')])]) =>
             if name' = name'' then
                 (env'', [L.LocalFunctionStat (name', params', body')])
             else
                 (env'', stat0 @ stat1)
           | _ => (env'', stat0 @ stat1)
      end
  | doStat ctx env (L.ReturnStat results) = let val (decs, results) = Vector.foldr (fn (x, (decs, xs)) => let val (decs', x) = doExp ctx env x
                                                                                                          in (decs' @ decs, x :: xs)
                                                                                                          end) ([], []) results
                                            in (env, decs @ [L.ReturnStat (vector results)])
                                            end
  | doStat ctx env (L.DoStat block) = let val (dynamic, block) = doBlock ctx env block
                                          val newEnv = { valMap = #valMap env
                                                       , bound = #bound env
                                                       , dynamic = L.IdMap.mapi (fn (id, attr as L.CONST) => attr
                                                                                | (id, attr as L.MUTABLE) => attr
                                                                                | (id, attr as L.LATE_INIT) => (case L.IdMap.find (dynamic, id) of
                                                                                                                    SOME L.CONST => L.CONST
                                                                                                                  | _ => attr
                                                                                                               )
                                                                                ) (#dynamic env)
                                                       }
                                      in (newEnv, [L.DoStat block])
                                      end
and doStats ctx env [] acc = (env, List.concat (List.rev acc))
  | doStats ctx env (stats as (L.AssignStat ([L.VarExp _], [L.FunctionExp (_, _)]) :: _)) acc
    = let val (env', defs, stats') = takeFunctionAssignments ctx env stats []
          val (decs, inits, funcs) = List.foldr (fn ((v, f), (decs, inits, funcs)) => case doExp ctx env' f of
                                                                                          ([L.LocalStat ([(u, L.CONST)], init)], f') => ((u, L.LATE_INIT) :: decs, L.AssignStat ([L.VarExp (L.UserDefinedId u)], init) :: inits, L.AssignStat ([L.VarExp v], [f']) :: funcs)
                                                                                        | ([], f') => (decs, inits, L.AssignStat ([L.VarExp v], [f]) :: funcs)
                                                                                        | _ => raise Fail "ProcessUpvalue: unexpected transformation"
                                                ) ([], [], []) defs
      in case decs of
             [] => doStats ctx env' stats' ([funcs @ inits] @ acc)
           | _ :: _ => doStats ctx env' stats' ([L.DoStat (vector (L.LocalStat (decs, []) :: List.rev funcs @ List.rev inits))] :: acc)
      end
  | doStats ctx env (stat :: stats) acc = let val (newEnv, stat') = doStat ctx env stat
                                          in doStats ctx newEnv stats (stat' :: acc)
                                          end
and takeFunctionAssignments ctx env (stats as (L.AssignStat ([L.VarExp v], [f as L.FunctionExp (params, body)]) :: stats')) revAcc
    = (case L.IdMap.find (#dynamic env, v) of
           SOME L.LATE_INIT => let val newEnv = { valMap = #valMap env
                                                , bound = #bound env
                                                , dynamic = L.IdMap.insert (#dynamic env, v, L.CONST)
                                                }
                               in takeFunctionAssignments ctx newEnv stats' ((v, f) :: revAcc)
                               end
         | _ => (env, List.rev revAcc, stats)
      )
  | takeFunctionAssignments ctx env stats revAcc = (env, List.rev revAcc, stats)
and doBlock ctx env stats = let val (env', stats) = doStats ctx env (Vector.foldr (op ::) [] stats) []
                                val dynamic = #dynamic env' (* assumes no shadowing *)
                            in (dynamic, vector stats)
                            end
end
structure ProcessLocal = struct
type Env = { currentLocals : int
           , locals : (TypedSyntax.VId * int ref) option
           , valMap : Variable TypedSyntax.VIdMap.map
           }
val initialEnv : Env = { currentLocals = L.IdMap.numItems mlinit_lua
                       , locals = NONE
                       , valMap = TypedSyntax.VIdMap.empty
                       }
val initialEnvForLuaJIT : Env = { currentLocals = L.IdMap.numItems mlinit_luajit
                                , locals = NONE
                                , valMap = TypedSyntax.VIdMap.empty
                                }
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
    = let val newLocals = #currentLocals env + List.length vars
      in if newLocals > LOCAL_LIMIT then
             let val (n, locals, r, dec) = case #locals env of
                                               SOME (locals, r) => (#currentLocals env, locals, r, [])
                                             | NONE => let val locals = genSym (ctx, "LOCAL")
                                                       in (#currentLocals env + 1, locals, ref 1, [L.LocalStat ([(locals, L.CONST)], [L.TableExp (vector [])])])
                                                       end
                 val (vars, valMap) = List.foldl (fn ((vid, _), (acc, valMap)) => let val i = !r
                                                                                  in r := i + 1
                                                                                   ; (L.IndexExp (L.VarExp (L.UserDefinedId locals), L.ConstExp (L.Numeral (Int.toString i))) :: acc, TypedSyntax.VIdMap.insert (valMap, vid, Index (locals, i)))
                                                                                  end
                                                 ) ([], #valMap env) vars
                 val newEnv = { currentLocals = n
                              , locals = SOME (locals, r)
                              , valMap = valMap
                              }
             in if List.null exps then
                    (newEnv, dec)
                else
                    (newEnv, dec @ [L.AssignStat (List.rev vars, List.map (doExp ctx env) exps)])
             end
         else
             let val newEnv = { currentLocals = newLocals, locals = #locals env, valMap = #valMap env }
             in (newEnv, [L.LocalStat (vars, List.map (doExp ctx env) exps)])
             end
      end
  | doStat ctx env (L.AssignStat (vars, exps)) = (env, [L.AssignStat (List.map (doExp ctx env) vars, List.map (doExp ctx env) exps)])
  | doStat ctx env (L.CallStat (exp, args)) = (env, [L.CallStat (doExp ctx env exp, Vector.map (doExp ctx env) args)])
  | doStat ctx env (L.MethodStat (self, method, args)) = (env, [L.MethodStat (doExp ctx env self, method, Vector.map (doExp ctx env) args)])
  | doStat ctx env (L.IfStat (cond, thenPart, elsePart)) = (env, [L.IfStat (doExp ctx env cond, doBlock ctx env thenPart, doBlock ctx env elsePart)])
  | doStat ctx env (L.LocalFunctionStat (name, params, body))
    = let val (env', stat0) = doStat ctx env (L.LocalStat ([(name, L.LATE_INIT)], []))
          val (env'', stat1) = doStat ctx env' (L.AssignStat ([L.VarExp (L.UserDefinedId name)], [L.FunctionExp (params, body)]))
      in case (stat0, stat1) of
             ([L.LocalStat (vars as [(name', L.LATE_INIT)], exps as [])], [L.AssignStat (lhs as [L.VarExp (L.UserDefinedId name'')], rhs as [L.FunctionExp (params', body')])]) =>
             if name' = name'' then
                 (env'', [L.LocalFunctionStat (name', params', body')])
             else
                 (env'', stat0 @ stat1)
           | _ => (env'', stat0 @ stat1)
      end
  | doStat ctx env (L.ReturnStat results) = (env, [L.ReturnStat (Vector.map (doExp ctx env) results)])
  | doStat ctx env (L.DoStat block) = (env, [L.DoStat (doBlock ctx env block)])
and doBlock ctx env stats = vector (List.concat (List.rev (#2 (Vector.foldl (fn (stat, (env, acc)) => let val (env, stat) = doStat ctx env stat
                                                                                                      in (env, stat :: acc)
                                                                                                      end
                                                                            ) (env, []) stats))))
end
end;
