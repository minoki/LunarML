(*
 * Copyright (c) 2023 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure JsTransform :>
sig
  type Context = {nextVId: int ref}
  val doProgram: Context -> JsSyntax.Block -> JsSyntax.Block
end =
struct
  structure J = JsSyntax

  fun collectLetConstStat (J.LetStat vars) acc =
        Vector.foldl
          (fn ((vid, _), acc) => J.IdSet.add (acc, J.UserDefinedId vid)) acc
          vars
    | collectLetConstStat (J.ConstStat vars) acc =
        Vector.foldl
          (fn ((vid, _), acc) => J.IdSet.add (acc, J.UserDefinedId vid)) acc
          vars
    | collectLetConstStat (J.ExpStat _) acc = acc
    | collectLetConstStat (J.IfStat _) acc = acc
    | collectLetConstStat (J.ReturnStat _) acc = acc
    | collectLetConstStat (J.TryCatchStat _) acc = acc
    | collectLetConstStat (J.ThrowStat _) acc = acc
    | collectLetConstStat (J.BlockStat _) acc = acc
    | collectLetConstStat (J.LoopStat _) acc = acc
    | collectLetConstStat (J.SwitchStat _) acc = acc
    | collectLetConstStat (J.BreakStat _) acc = acc
    | collectLetConstStat (J.ContinueStat _) acc = acc
    | collectLetConstStat (J.DefaultExportStat _) acc = acc
    | collectLetConstStat (J.NamedExportStat _) acc = acc
  and collectLetConstBlock stats acc =
    Vector.foldl (fn (stat, acc) => collectLetConstStat stat acc) acc stats

  fun freeVarsExp (_, J.ConstExp _) acc = acc
    | freeVarsExp (_, J.ThisExp) acc = acc
    | freeVarsExp (bound, J.VarExp x) acc =
        if J.IdSet.member (bound, x) then acc else J.IdSet.add (acc, x)
    | freeVarsExp (bound, J.ObjectExp fields) acc =
        Vector.foldl (fn ((_, exp), acc) => freeVarsExp (bound, exp) acc) acc
          fields
    | freeVarsExp (bound, J.ArrayExp elems) acc =
        Vector.foldl (fn (exp, acc) => freeVarsExp (bound, exp) acc) acc elems
    | freeVarsExp (bound, J.CallExp (x, ys)) acc =
        Vector.foldl (fn (exp, acc) => freeVarsExp (bound, exp) acc)
          (freeVarsExp (bound, x) acc) ys
    | freeVarsExp (bound, J.MethodExp (x, _, ys)) acc =
        Vector.foldl (fn (exp, acc) => freeVarsExp (bound, exp) acc)
          (freeVarsExp (bound, x) acc) ys
    | freeVarsExp (bound, J.NewExp (x, ys)) acc =
        Vector.foldl (fn (exp, acc) => freeVarsExp (bound, exp) acc)
          (freeVarsExp (bound, x) acc) ys
    | freeVarsExp (bound, J.FunctionExp (params, body)) acc =
        let
          val bound' =
            Vector.foldl (fn (id, bound) => J.IdSet.add (bound, id)) bound
              params
          val bound'' = collectLetConstBlock body bound'
        in
          freeVarsBlock (bound'', body) acc
        end
    | freeVarsExp (bound, J.BinExp (_, x, y)) acc =
        freeVarsExp (bound, x) (freeVarsExp (bound, y) acc)
    | freeVarsExp (bound, J.UnaryExp (_, x)) acc =
        freeVarsExp (bound, x) acc
    | freeVarsExp (bound, J.IndexExp (x, y)) acc =
        freeVarsExp (bound, x) (freeVarsExp (bound, y) acc)
    | freeVarsExp (bound, J.CondExp (x, y, z)) acc =
        freeVarsExp (bound, x) (freeVarsExp (bound, y)
          (freeVarsExp (bound, z) acc))
  and freeVarsStat (bound, J.LetStat vars) acc =
        Vector.foldl
          (fn ((_, NONE), acc) => acc
            | ((_, SOME exp), acc) => freeVarsExp (bound, exp) acc) acc vars
    | freeVarsStat (bound, J.ConstStat vars) acc =
        Vector.foldl (fn ((_, exp), acc) => freeVarsExp (bound, exp) acc) acc
          vars
    | freeVarsStat (bound, J.ExpStat exp) acc =
        freeVarsExp (bound, exp) acc
    | freeVarsStat (bound, J.IfStat (cond, then', else')) acc =
        freeVarsExp (bound, cond) (freeVarsBlock (bound, then')
          (freeVarsBlock (bound, else') acc))
    | freeVarsStat (_, J.ReturnStat NONE) acc = acc
    | freeVarsStat (bound, J.ReturnStat (SOME exp)) acc =
        freeVarsExp (bound, exp) acc
    | freeVarsStat (bound, J.TryCatchStat (try, vid, catch)) acc =
        freeVarsBlock (bound, try)
          (freeVarsBlock (J.IdSet.add (bound, J.UserDefinedId vid), catch) acc)
    | freeVarsStat (bound, J.ThrowStat exp) acc =
        freeVarsExp (bound, exp) acc
    | freeVarsStat (bound, J.BlockStat (_, block)) acc =
        freeVarsBlock (bound, block) acc
    | freeVarsStat (bound, J.LoopStat (_, block)) acc =
        freeVarsBlock (bound, block) acc
    | freeVarsStat (bound, J.SwitchStat (exp, cases)) acc =
        List.foldl (fn ((_, block), acc) => freeVarsBlock (bound, block) acc)
          (freeVarsExp (bound, exp) acc) cases
    | freeVarsStat (_, J.BreakStat _) acc = acc
    | freeVarsStat (_, J.ContinueStat _) acc = acc
    | freeVarsStat (bound, J.DefaultExportStat exp) acc =
        freeVarsExp (bound, exp) acc
    | freeVarsStat (bound, J.NamedExportStat entities) acc =
        Vector.foldl
          (fn ((x, _), acc) =>
             if J.IdSet.member (bound, x) then acc else J.IdSet.add (acc, x))
          acc entities
  and freeVarsBlock (bound, stats) acc =
    let
      val bound' = collectLetConstBlock stats bound
    in
      Vector.foldl (fn (stat, acc) => freeVarsStat (bound', stat) acc) acc stats
    end

  type Context = {nextVId: int ref}

  fun freshVId (ctx: Context, name) =
    let
      val n = !(#nextVId ctx)
      val _ = #nextVId ctx := n + 1
    in
      TypedSyntax.MkVId (name, n)
    end

  fun renewVId (ctx: Context, TypedSyntax.MkVId (name, _)) =
    let
      val n = !(#nextVId ctx)
      val _ = #nextVId ctx := n + 1
    in
      TypedSyntax.MkVId (name, n)
    end

  (*:
  val goExp : Context * J.IdSet.set * TypedSyntax.VId TypedSyntax.VIdMap.map * int * J.Exp -> J.Stat list * J.Exp
  val goExpVector : Context * J.IdSet.set * TypedSyntax.VId TypedSyntax.VIdMap.map * int * J.Exp vector -> J.Stat list * J.Exp vector
  val goStat : Context * J.IdSet.set * TypedSyntax.VId TypedSyntax.VIdMap.map * int * J.Stat -> J.Stat list * J.Stat
  val goBlock : Context * J.IdSet.set * TypedSyntax.VId TypedSyntax.VIdMap.map * int * J.Stat vector -> J.Stat list * J.Stat vector
   *)
  fun goExp (_, _, _, _, e as J.ConstExp _) = ([], e)
    | goExp (_, _, _, _, e as J.ThisExp) = ([], e)
    | goExp (_, _, renameMap, _, e as J.VarExp id) =
        (case id of
           J.PredefinedId _ => ([], e)
         | J.UserDefinedId vid =>
             case TypedSyntax.VIdMap.find (renameMap, vid) of
               SOME newVId => ([], J.VarExp (J.UserDefinedId newVId))
             | NONE => ([], e))
    | goExp (ctx, bound, renameMap, depth, J.ObjectExp fields) =
        let
          val (decs, fields') =
            Vector.foldr
              (fn ((key, exp), (decs, fields)) =>
                 let
                   val (decs', exp') = goExp (ctx, bound, renameMap, depth, exp)
                 in
                   (decs' @ decs, (key, exp') :: fields)
                 end) ([], []) fields
        in
          (decs, J.ObjectExp (Vector.fromList fields'))
        end
    | goExp (ctx, bound, renameMap, depth, J.ArrayExp elems) =
        let
          val (decs, elems') = goExpVector (ctx, bound, renameMap, depth, elems)
        in
          (decs, J.ArrayExp elems')
        end
    | goExp (ctx, bound, renameMap, depth, J.CallExp (x, ys)) =
        let
          val (decs, x') = goExp (ctx, bound, renameMap, depth, x)
          val (decs', ys') = goExpVector (ctx, bound, renameMap, depth, ys)
        in
          (decs @ decs', J.CallExp (x', ys'))
        end
    | goExp (ctx, bound, renameMap, depth, J.MethodExp (x, name, ys)) =
        let
          val (decs, x') = goExp (ctx, bound, renameMap, depth, x)
          val (decs', ys') = goExpVector (ctx, bound, renameMap, depth, ys)
        in
          (decs @ decs', J.MethodExp (x', name, ys'))
        end
    | goExp (ctx, bound, renameMap, depth, J.NewExp (x, ys)) =
        let
          val (decs, x') = goExp (ctx, bound, renameMap, depth, x)
          val (decs', ys') = goExpVector (ctx, bound, renameMap, depth, ys)
        in
          (decs @ decs', J.NewExp (x', ys'))
        end
    | goExp (ctx, bound, renameMap, depth, f as J.FunctionExp (params, body)) =
        if depth > 200 then
          let
            val fv = freeVarsExp (J.IdSet.empty, f) J.IdSet.empty
            val captures = J.IdSet.toList (J.IdSet.intersection (bound, fv))
            val bound' =
              Vector.foldl (fn (id, bound) => J.IdSet.add (bound, id)) bound
                params
            val name = freshVId (ctx, "f")
            val params' = Vector.foldr (op::) [] params
            val capturesAndParams = Vector.fromList (captures @ params')
            val (renameMap', capturesAndParams2) =
              Vector.foldr
                (fn (id as J.PredefinedId _, (m, acc)) => (m, id :: acc)
                  | (J.UserDefinedId vid, (m, acc)) =>
                   let
                     val vid' = renewVId (ctx, vid)
                   in
                     ( TypedSyntax.VIdMap.insert (m, vid, vid')
                     , J.UserDefinedId vid' :: acc
                     )
                   end) (renameMap, []) capturesAndParams
            val (decs, body') = goBlock (ctx, bound', renameMap', 0, body)
            val newDec = J.ConstStat (vector
              [(name, J.FunctionExp (Vector.fromList capturesAndParams2, body'))])
            fun renameVId vid =
              case TypedSyntax.VIdMap.find (renameMap, vid) of
                SOME vid' => vid'
              | NONE => vid
            fun renameId (id as J.PredefinedId _) = id
              | renameId (id as J.UserDefinedId vid) =
                  case TypedSyntax.VIdMap.find (renameMap, vid) of
                    SOME vid' => J.UserDefinedId vid'
                  | NONE => id
            val newExp = J.FunctionExp (params, vector
              [J.ReturnStat (SOME (J.CallExp
                 ( J.VarExp (J.UserDefinedId (renameVId name))
                 , Vector.map (J.VarExp o renameId) capturesAndParams
                 )))])
          in
            (decs @ [newDec], newExp)
          end
        else
          let
            val bound' =
              Vector.foldl (fn (id, bound) => J.IdSet.add (bound, id)) bound
                params
            val (decs, body') = goBlock
              (ctx, bound', renameMap, depth + 1, body)
          in
            (decs, J.FunctionExp (params, body'))
          end
    | goExp (ctx, bound, renameMap, depth, J.BinExp (p, x, y)) =
        let
          val (decs, x') = goExp (ctx, bound, renameMap, depth, x)
          val (decs', y') = goExp (ctx, bound, renameMap, depth, y)
        in
          (decs @ decs', J.BinExp (p, x', y'))
        end
    | goExp (ctx, bound, renameMap, depth, J.UnaryExp (p, x)) =
        let val (decs, x') = goExp (ctx, bound, renameMap, depth, x)
        in (decs, J.UnaryExp (p, x'))
        end
    | goExp (ctx, bound, renameMap, depth, J.IndexExp (x, y)) =
        let
          val (decs, x') = goExp (ctx, bound, renameMap, depth, x)
          val (decs', y') = goExp (ctx, bound, renameMap, depth, y)
        in
          (decs @ decs', J.IndexExp (x', y'))
        end
    | goExp (ctx, bound, renameMap, depth, J.CondExp (x, y, z)) =
        let
          val (decs, x') = goExp (ctx, bound, renameMap, depth, x)
          val (decs', y') = goExp (ctx, bound, renameMap, depth, y)
          val (decs'', z') = goExp (ctx, bound, renameMap, depth, z)
        in
          (decs @ decs' @ decs'', J.CondExp (x', y', z'))
        end
  and goExpVector (ctx, bound, renameMap, depth, xs) =
    let
      val (decs, ys) =
        Vector.foldr
          (fn (exp, (decs, ys)) =>
             let val (decs', exp') = goExp (ctx, bound, renameMap, depth, exp)
             in (decs' @ decs, exp' :: ys)
             end) ([], []) xs
    in
      (decs, Vector.fromList ys)
    end
  and goStat (ctx, bound, renameMap, depth, J.LetStat vars) =
        let
          val (decs, vars) =
            Vector.foldr
              (fn ((vid, NONE), (decs, vars)) => (decs, (vid, NONE) :: vars)
                | ((vid, SOME exp), (decs, vars)) =>
                 let
                   val (decs', exp) = goExp (ctx, bound, renameMap, depth, exp)
                 in
                   (decs' @ decs, (vid, SOME exp) :: vars)
                 end) ([], []) vars
        in
          (decs, J.LetStat (Vector.fromList vars))
        end
    | goStat (ctx, bound, renameMap, depth, J.ConstStat vars) =
        let
          val (decs, vars) =
            Vector.foldr
              (fn ((vid, exp), (decs, vars)) =>
                 let
                   val (decs', exp) = goExp (ctx, bound, renameMap, depth, exp)
                 in
                   (decs' @ decs, (vid, exp) :: vars)
                 end) ([], []) vars
        in
          (decs, J.ConstStat (Vector.fromList vars))
        end
    | goStat (ctx, bound, renameMap, depth, J.ExpStat exp) =
        let val (decs, exp) = goExp (ctx, bound, renameMap, depth, exp)
        in (decs, J.ExpStat exp)
        end
    | goStat (ctx, bound, renameMap, depth, J.IfStat (exp, then', else')) =
        let
          val (decs, exp) = goExp (ctx, bound, renameMap, depth, exp)
          val (decs', then') = goBlock (ctx, bound, renameMap, depth, then')
          val (decs'', else') = goBlock (ctx, bound, renameMap, depth, else')
        in
          (decs @ decs' @ decs'', J.IfStat (exp, then', else'))
        end
    | goStat (_, _, _, _, s as J.ReturnStat NONE) = ([], s)
    | goStat (ctx, bound, renameMap, depth, J.ReturnStat (SOME exp)) =
        let val (decs, exp) = goExp (ctx, bound, renameMap, depth, exp)
        in (decs, J.ReturnStat (SOME exp))
        end
    | goStat (ctx, bound, renameMap, depth, J.TryCatchStat (try, vid, catch)) =
        let
          val (decs, try) = goBlock (ctx, bound, renameMap, depth, try)
          val (decs', catch) = goBlock
            ( ctx
            , J.IdSet.add (bound, J.UserDefinedId vid)
            , renameMap
            , depth
            , catch
            )
        in
          (decs @ decs', J.TryCatchStat (try, vid, catch))
        end
    | goStat (ctx, bound, renameMap, depth, J.ThrowStat exp) =
        let val (decs, exp) = goExp (ctx, bound, renameMap, depth, exp)
        in (decs, J.ThrowStat exp)
        end
    | goStat (ctx, bound, renameMap, depth, J.BlockStat (optLabel, block)) =
        let val (decs, block) = goBlock (ctx, bound, renameMap, depth, block)
        in (decs, J.BlockStat (optLabel, block))
        end
    | goStat (ctx, bound, renameMap, depth, J.LoopStat (optLabel, block)) =
        let val (decs, block) = goBlock (ctx, bound, renameMap, depth, block)
        in (decs, J.LoopStat (optLabel, block))
        end
    | goStat (ctx, bound, renameMap, depth, J.SwitchStat (exp, cases)) =
        let
          val (decs, exp) = goExp (ctx, bound, renameMap, depth, exp)
          val (decs', cases) =
            List.foldr
              (fn ((c, block), (decs, cases)) =>
                 let
                   val (decs', block) = goBlock
                     (ctx, bound, renameMap, depth, block)
                 in
                   (decs' @ decs, (c, block) :: cases)
                 end) ([], []) cases
        in
          (decs @ decs', J.SwitchStat (exp, cases))
        end
    | goStat (_, _, _, _, s as J.BreakStat _) = ([], s)
    | goStat (_, _, _, _, s as J.ContinueStat _) = ([], s)
    | goStat (ctx, bound, renameMap, depth, J.DefaultExportStat exp) =
        let val (decs, exp) = goExp (ctx, bound, renameMap, depth, exp)
        in (decs, J.DefaultExportStat exp)
        end
    | goStat (_, _, _, _, s as J.NamedExportStat _) = ([], s)
  and goBlock (ctx, bound, renameMap, depth, stats) =
    let
      val bound' = collectLetConstBlock stats bound
      val (decs, ys) =
        Vector.foldr
          (fn (stat, (decs, ys)) =>
             let
               val (decs', stat') = goStat (ctx, bound', renameMap, depth, stat)
             in
               (decs' @ decs, stat' :: ys)
             end) ([], []) stats
    in
      (decs, Vector.fromList ys)
    end

  fun doProgram ctx block =
    let
      val (decs, block') = goBlock
        (ctx, J.IdSet.empty, TypedSyntax.VIdMap.empty, 0, block)
    in
      Vector.fromList (decs @ Vector.foldr (op::) [] block')
    end

end;
