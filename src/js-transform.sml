structure JsTransform = struct
structure J = JsSyntax

fun collectVarsStat (J.VarStat vars) = Vector.foldl (fn ((vid, _), acc) => J.IdSet.add (acc, J.UserDefinedId vid)) J.IdSet.empty vars
  | collectVarsStat (J.ExpStat _) = J.IdSet.empty
  | collectVarsStat (J.IfStat (_, then', else')) = J.IdSet.union (collectVarsBlock then', collectVarsBlock else')
  | collectVarsStat (J.ReturnStat _) = J.IdSet.empty
  | collectVarsStat (J.TryCatchStat (try, vid, catch)) = J.IdSet.union (collectVarsBlock try, J.IdSet.subtract (collectVarsBlock catch, J.UserDefinedId vid))
  | collectVarsStat (J.ThrowStat _) = J.IdSet.empty
and collectVarsBlock stats = Vector.foldl (fn (stat, acc) => J.IdSet.union (collectVarsStat stat, acc)) J.IdSet.empty stats

fun freeVarsExp (_, J.ConstExp _) = J.IdSet.empty
  | freeVarsExp (_, J.ThisExp) = J.IdSet.empty
  | freeVarsExp (bound, J.VarExp x) = if J.IdSet.member (bound, x) then
                                          J.IdSet.empty
                                      else
                                          J.IdSet.singleton x
  | freeVarsExp (bound, J.ObjectExp fields) = Vector.foldl (fn ((key, exp), acc) => J.IdSet.union (freeVarsExp (bound, exp), acc)) J.IdSet.empty fields
  | freeVarsExp (bound, J.ArrayExp elems) = Vector.foldl (fn (exp, acc) => J.IdSet.union (freeVarsExp (bound, exp), acc)) J.IdSet.empty elems
  | freeVarsExp (bound, J.CallExp (x, ys)) = Vector.foldl (fn (exp, acc) => J.IdSet.union (freeVarsExp (bound, exp), acc)) (freeVarsExp (bound, x)) ys
  | freeVarsExp (bound, J.MethodExp (x, _, ys)) = Vector.foldl (fn (exp, acc) => J.IdSet.union (freeVarsExp (bound, exp), acc)) (freeVarsExp (bound, x)) ys
  | freeVarsExp (bound, J.NewExp (x, ys)) = Vector.foldl (fn (exp, acc) => J.IdSet.union (freeVarsExp (bound, exp), acc)) (freeVarsExp (bound, x)) ys
  | freeVarsExp (bound, J.FunctionExp (params, body)) = let val bound' = Vector.foldl (fn (id, bound) => J.IdSet.add (bound, id)) bound params
                                                            val bound'' = J.IdSet.union (collectVarsBlock body, bound')
                                                        in freeVarsBlock (bound'', body)
                                                        end
  | freeVarsExp (bound, J.BinExp (_, x, y)) = J.IdSet.union (freeVarsExp (bound, x), freeVarsExp (bound, y))
  | freeVarsExp (bound, J.UnaryExp (_, x)) = freeVarsExp (bound, x)
  | freeVarsExp (bound, J.IndexExp (x, y)) = J.IdSet.union (freeVarsExp (bound, x), freeVarsExp (bound, y))
  | freeVarsExp (bound, J.CondExp (x, y, z)) = J.IdSet.union (freeVarsExp (bound, x), J.IdSet.union (freeVarsExp (bound, y), freeVarsExp (bound, z)))
and freeVarsStat (bound, J.VarStat vars) = Vector.foldl (fn ((vid, NONE), acc) => acc
                                                        | ((vid, SOME exp), acc) => J.IdSet.union (freeVarsExp (bound, exp), acc)
                                                        ) J.IdSet.empty vars
  | freeVarsStat (bound, J.ExpStat exp) = freeVarsExp (bound, exp)
  | freeVarsStat (bound, J.IfStat (cond, then', else')) = J.IdSet.union (freeVarsExp (bound, cond), J.IdSet.union (freeVarsBlock (bound, then'), freeVarsBlock (bound, else')))
  | freeVarsStat (bound, J.ReturnStat NONE) = J.IdSet.empty
  | freeVarsStat (bound, J.ReturnStat (SOME exp)) = freeVarsExp (bound, exp)
  | freeVarsStat (bound, J.TryCatchStat (try, vid, catch)) = J.IdSet.union (freeVarsBlock (bound, try), freeVarsBlock (J.IdSet.add (bound, J.UserDefinedId vid), catch))
  | freeVarsStat (bound, J.ThrowStat exp) = freeVarsExp (bound, exp)
and freeVarsBlock (bound, stats) = Vector.foldl (fn (stat, acc) => J.IdSet.union (freeVarsStat (bound, stat), acc)) J.IdSet.empty stats

type Context = { nextVId : int ref }

fun freshVId (ctx : Context, name) = let val n = !(#nextVId ctx)
                                         val _ = #nextVId ctx := n + 1
                                     in TypedSyntax.MkVId (name, n)
                                     end

fun goExp (ctx, bound, depth, e as J.ConstExp _) = ([], e)
  | goExp (ctx, bound, depth, e as J.ThisExp) = ([], e)
  | goExp (ctx, bound, depth, e as J.VarExp _) = ([], e)
  | goExp (ctx, bound, depth, J.ObjectExp fields) = let val (decs, fields') = Vector.foldr (fn ((key, exp), (decs, fields)) =>
                                                                                               let val (decs', exp') = goExp (ctx, bound, depth, exp)
                                                                                               in (decs' @ decs, (key, exp') :: fields)
                                                                                               end
                                                                                           ) ([], []) fields
                                                    in (decs, J.ObjectExp (Vector.fromList fields'))
                                                    end
  | goExp (ctx, bound, depth, J.ArrayExp elems) = let val (decs, elems') = goExpVector (ctx, bound, depth, elems)
                                                  in (decs, J.ArrayExp elems')
                                                  end
  | goExp (ctx, bound, depth, J.CallExp (x, ys)) = let val (decs, x') = goExp (ctx, bound, depth, x)
                                                       val (decs', ys') = goExpVector (ctx, bound, depth, ys)
                                                   in (decs @ decs', J.CallExp (x', ys'))
                                                   end
  | goExp (ctx, bound, depth, J.MethodExp (x, name, ys)) = let val (decs, x') = goExp (ctx, bound, depth, x)
                                                               val (decs', ys') = goExpVector (ctx, bound, depth, ys)
                                                           in (decs @ decs', J.MethodExp (x', name, ys'))
                                                           end
  | goExp (ctx, bound, depth, J.NewExp (x, ys)) = let val (decs, x') = goExp (ctx, bound, depth, x)
                                                      val (decs', ys') = goExpVector (ctx, bound, depth, ys)
                                                  in (decs @ decs', J.NewExp (x', ys'))
                                                  end
  | goExp (ctx, bound, depth, f as J.FunctionExp (params, body))
    = if depth > 200 then
          let val fv = freeVarsExp (J.IdSet.empty, f)
              val captures = J.IdSet.toList (J.IdSet.intersection (bound, fv))
              val bound' = Vector.foldl (fn (id, bound) => J.IdSet.add (bound, id)) bound params
              val bound'' = J.IdSet.union (collectVarsBlock body, bound')
              val (decs, body') = goBlock (ctx, bound'', 0, body)
              val name = freshVId (ctx, "f")
              val params' = Vector.foldr (op ::) [] params
              val capturesAndParams = Vector.fromList (captures @ params')
              val newDec = J.VarStat (vector [(name, SOME (J.FunctionExp (capturesAndParams, body')))])
              val newExp = J.FunctionExp (params, vector [J.ReturnStat (SOME (J.CallExp (J.VarExp (J.UserDefinedId name), Vector.map J.VarExp capturesAndParams)))])
          in (decs @ [newDec], newExp)
          end
      else
          let val bound' = Vector.foldl (fn (id, bound) => J.IdSet.add (bound, id)) bound params
              val bound'' = J.IdSet.union (collectVarsBlock body, bound')
              val (decs, body') = goBlock (ctx, bound'', depth + 1, body)
          in (decs, J.FunctionExp (params, body'))
          end
  | goExp (ctx, bound, depth, J.BinExp (p, x, y)) = let val (decs, x') = goExp (ctx, bound, depth, x)
                                                        val (decs', y') = goExp (ctx, bound, depth, y)
                                                    in (decs @ decs', J.BinExp (p, x', y'))
                                                    end
  | goExp (ctx, bound, depth, J.UnaryExp (p, x)) = let val (decs, x') = goExp (ctx, bound, depth, x)
                                                   in (decs, J.UnaryExp (p, x'))
                                                   end
  | goExp (ctx, bound, depth, J.IndexExp (x, y)) = let val (decs, x') = goExp (ctx, bound, depth, x)
                                                       val (decs', y') = goExp (ctx, bound, depth, y)
                                                   in (decs @ decs', J.IndexExp (x', y'))
                                                   end
  | goExp (ctx, bound, depth, J.CondExp (x, y, z)) = let val (decs, x') = goExp (ctx, bound, depth, x)
                                                         val (decs', y') = goExp (ctx, bound, depth, y)
                                                         val (decs'', z') = goExp (ctx, bound, depth, z)
                                                     in (decs @ decs' @ decs'', J.CondExp (x', y', z'))
                                                     end
and goExpVector (ctx, bound, depth, xs) = let val (decs, ys) = Vector.foldr (fn (exp, (decs, ys)) =>
                                                                                let val (decs', exp') = goExp (ctx, bound, depth, exp)
                                                                                in (decs' @ decs, exp' :: ys)
                                                                                end
                                                                            ) ([], []) xs
                                          in (decs, Vector.fromList ys)
                                          end
and goStat (ctx, bound, depth, J.VarStat vars) = let val (decs, vars) = Vector.foldr (fn ((vid, NONE), (decs, vars)) => (decs, (vid, NONE) :: vars)
                                                                                     | ((vid, SOME exp), (decs, vars)) => let val (decs', exp) = goExp (ctx, bound, depth, exp)
                                                                                                                          in (decs' @ decs, (vid, SOME exp) :: vars)
                                                                                                                          end
                                                                                     ) ([], []) vars
                                                 in (decs, J.VarStat (Vector.fromList vars))
                                                 end
  | goStat (ctx, bound, depth, J.ExpStat exp) = let val (decs, exp) = goExp (ctx, bound, depth, exp)
                                                in (decs, J.ExpStat exp)
                                                end
  | goStat (ctx, bound, depth, J.IfStat (exp, then', else')) = let val (decs, exp) = goExp (ctx, bound, depth, exp)
                                                                   val (decs', then') = goBlock (ctx, bound, depth, then')
                                                                   val (decs'', else') = goBlock (ctx, bound, depth, else')
                                                               in (decs @ decs' @ decs'', J.IfStat (exp, then', else'))
                                                               end
  | goStat (ctx, bound, depth, s as J.ReturnStat NONE) = ([], s)
  | goStat (ctx, bound, depth, s as J.ReturnStat (SOME exp)) = let val (decs, exp) = goExp (ctx, bound, depth, exp)
                                                               in (decs, J.ReturnStat (SOME exp))
                                                               end
  | goStat (ctx, bound, depth, J.TryCatchStat (try, vid, catch)) = let val (decs, try) = goBlock (ctx, bound, depth, try)
                                                                       val (decs', catch) = goBlock (ctx, J.IdSet.add (bound, J.UserDefinedId vid), depth, catch)
                                                                   in (decs @ decs', J.TryCatchStat (try, vid, catch))
                                                                   end
  | goStat (ctx, bound, depth, J.ThrowStat exp) = let val (decs, exp) = goExp (ctx, bound, depth, exp)
                                                  in (decs, J.ThrowStat exp)
                                                  end
and goBlock (ctx, bound, depth, stats) = let val (decs, ys) = Vector.foldr (fn (stat, (decs, ys)) =>
                                                                               let val (decs', stat') = goStat (ctx, bound, depth, stat)
                                                                               in (decs' @ decs, stat' :: ys)
                                                                               end
                                                                           ) ([], []) stats
                                         in (decs, Vector.fromList ys)
                                         end

fun doProgram ctx block = let val (decs, block') = goBlock (ctx, J.IdSet.empty, 0, block)
                          in Vector.fromList (decs @ Vector.foldr (op ::) [] block')
                          end

end;
