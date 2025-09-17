(*
 * Copyright (c) 2025 ARATA Mizuki
 * This file is part of LunarML.
 *)
(* Remove redundant jumps and labels *)
structure JsSimplifyJumps :>
sig
  val doProgram: JsSyntax.Block -> JsSyntax.Block
end =
struct
  structure J = JsSyntax
  type Env =
    {currentBreakTarget: J.Id option, currentContinueTarget: J.Id option}
  val initialEnv: Env =
    {currentBreakTarget = NONE, currentContinueTarget = NONE}
  fun eliminateFinalBreakTo (label, block) =
    let
      val n = Vector.length block
    in
      if n = 0 then
        block
      else
        case Vector.sub (block, n - 1) of
          J.BreakStat (SOME label') =>
            if J.eqId (label, label') then
              VectorSlice.vector (VectorSlice.slice (block, 0, SOME (n - 1)))
            else
              block
        | J.IfStat (c, t, e) =>
            let
              val init = VectorSlice.slice (block, 0, SOME (n - 1))
            in
              VectorSlice.concat
                [ init
                , VectorSlice.full (Vector.fromList
                    [J.IfStat
                       ( c
                       , eliminateFinalBreakTo (label, t)
                       , eliminateFinalBreakTo (label, e)
                       )])
                ]
            end
        | _ => block
    end
  (*
  fun eliminateFinalBreakTo (_, []) = []
    | eliminateFinalBreakTo (label, original as ( :: rest)) = if label = label' then rest else original
    | eliminateFinalBreakTo (label, J.IfStat (c, t, e) :: rest) = J.IfStat (c, eliminateFinalBreakToInBlock (label, t), eliminateFinalBreakToInBlock (label, e)) :: rest
    | eliminateFinalBreakTo (_, revStats) = revStats
  and eliminateFinalBreakToInBlock (label, block) = Vector.fromList (List.rev (eliminateFinalBreakTo (label, Vector.foldl (op ::) [] block)))
  *)
  fun eliminateFinalContinue (optLabel, block) =
    let
      val n = Vector.length block
    in
      if n = 0 then
        block
      else
        case Vector.sub (block, n - 1) of
          J.ContinueStat optLabel' =>
            let
              val match =
                case (optLabel, optLabel') of
                  (SOME label, SOME label') => J.eqId (label, label')
                | (_, NONE) => true
                | (NONE, SOME _) => false
            in
              if match then
                VectorSlice.vector (VectorSlice.slice (block, 0, SOME (n - 1)))
              else
                block
            end
        | J.IfStat (c, t, e) =>
            let
              val init = VectorSlice.slice (block, 0, SOME (n - 1))
            in
              VectorSlice.concat
                [ init
                , VectorSlice.full (Vector.fromList
                    [J.IfStat
                       ( c
                       , eliminateFinalContinue (optLabel, t)
                       , eliminateFinalContinue (optLabel, e)
                       )])
                ]
            end
        | _ => block
    end
  (*
  fun eliminateFinalContinue (_, []) = []
    | eliminateFinalContinue (optLabel, original as (J.ContinueStat optLabel' :: rest)) =
        let val match = case (optLabel, optLabel') of
            (SOME label, SOME label') => label = label'
            | (_, NONE) => true
            | (NONE, SOME _) => false
        in if match then
             rest
           else original
        end
    | eliminateFinalContinue (optLabel, J.IfStat (c, t, e) :: rest) = J.IfStat (c, eliminateFinalContinueInBlock (optLabel, t), eliminateFinalContinueInBlock (optLabel, e)) :: rest
    | eliminateFinalContinue (_, revStats) = revStats
  and eliminateFinalContinueInBlock (label, block) = Vector.fromList (List.rev (eliminateFinalContinue (label, Vector.foldl (op ::) [] block)))
  *)
  fun isVariableDeclaration (J.LetStat _) = true
    | isVariableDeclaration (J.ConstStat _) = true
    | isVariableDeclaration _ = false
  fun hasVariableDeclaration block = Vector.exists isVariableDeclaration block
  fun isLabelUsedInStat label stat =
    case stat of
      J.LetStat _ => false
    | J.ConstStat _ => false
    | J.ExpStat _ => false
    | J.IfStat (_, t, e) => isLabelUsed (label, t) orelse isLabelUsed (label, e)
    | J.ReturnStat _ => false
    | J.TryCatchStat (t, _, c) =>
        isLabelUsed (label, t) orelse isLabelUsed (label, c)
    | J.ThrowStat _ => false
    | J.BlockStat (_, body) => isLabelUsed (label, body)
    | J.LoopStat (_, body) => isLabelUsed (label, body)
    | J.SwitchStat (_, clauses) =>
        List.exists (fn (_, block) => isLabelUsed (label, block)) clauses
    | J.BreakStat (SOME label') => J.eqId (label, label')
    | J.BreakStat NONE => false
    | J.ContinueStat (SOME label') => J.eqId (label, label')
    | J.ContinueStat NONE => false
    | J.DefaultExportStat _ => false
    | J.NamedExportStat _ => false
  and isLabelUsed (label, block) =
    Vector.exists (isLabelUsedInStat label) block
  fun doExp (exp as J.ConstExp _) = exp
    | doExp (exp as J.ThisExp) = exp
    | doExp (exp as J.VarExp _) = exp
    | doExp (J.ObjectExp fields) =
        J.ObjectExp (Vector.map (fn (k, x) => (k, doExp x)) fields)
    | doExp (J.ArrayExp elems) =
        J.ArrayExp (Vector.map doExp elems)
    | doExp (J.CallExp (f, args)) =
        J.CallExp (doExp f, Vector.map doExp args)
    | doExp (J.MethodExp (f, name, args)) =
        J.MethodExp (doExp f, name, Vector.map doExp args)
    | doExp (J.NewExp (f, args)) =
        J.NewExp (doExp f, Vector.map doExp args)
    | doExp (J.FunctionExp (params, body)) =
        J.FunctionExp (params, doBlock (initialEnv, body))
    | doExp (J.BinExp (p, x, y)) =
        J.BinExp (p, doExp x, doExp y)
    | doExp (J.UnaryExp (p, x)) =
        J.UnaryExp (p, doExp x)
    | doExp (J.IndexExp (x, y)) =
        J.IndexExp (doExp x, doExp y)
    | doExp (J.CondExp (x, y, z)) =
        J.CondExp (doExp x, doExp y, doExp z)
  and doStat (env: Env) (stat, acc) =
    (case stat of
       J.LetStat vars =>
         J.LetStat (Vector.map (fn (v, x) => (v, Option.map doExp x)) vars)
         :: acc
     | J.ConstStat vars =>
         J.ConstStat (Vector.map (fn (v, x) => (v, doExp x)) vars) :: acc
     | J.ExpStat x => J.ExpStat (doExp x) :: acc
     | J.IfStat (x, y, z) =>
         J.IfStat (doExp x, doBlock (env, y), doBlock (env, z)) :: acc
     | J.ReturnStat x => J.ReturnStat (Option.map doExp x) :: acc
     | J.TryCatchStat (body, e, catch) =>
         J.TryCatchStat (doBlock (env, body), e, doBlock (env, catch)) :: acc
     | J.ThrowStat x => J.ThrowStat (doExp x) :: acc
     | J.BlockStat (someLabel as SOME label, body) =>
         let
           val body = doBlock (env, body)
           val body = eliminateFinalBreakTo (label, body)
         in
           if isLabelUsed (label, body) then
             J.BlockStat (someLabel, body) :: acc
           else if hasVariableDeclaration body then
             J.BlockStat (NONE, body) :: acc
           else
             Vector.foldr (op::) acc body
         end
     | J.BlockStat (NONE, body) =>
         J.BlockStat (NONE, doBlock (env, body)) :: acc
     | J.LoopStat (someLabel as SOME label, body) =>
         let
           val body = doBlock
             ( { currentBreakTarget = someLabel
               , currentContinueTarget = someLabel
               }
             , body
             )
           val body = eliminateFinalContinue (someLabel, body)
         in
           if isLabelUsed (label, body) then J.LoopStat (someLabel, body) :: acc
           else J.LoopStat (NONE, body) :: acc
         end
     | J.LoopStat (NONE, body) =>
         let
           val body = doBlock
             ({currentBreakTarget = NONE, currentContinueTarget = NONE}, body)
           val body = eliminateFinalContinue (NONE, body)
         in
           J.LoopStat (NONE, body) :: acc
         end
     | J.SwitchStat (exp, clauses) =>
         J.SwitchStat
           ( doExp exp
           , List.map
               (fn (k, body) =>
                  ( k
                  , doBlock
                      ( { currentBreakTarget = NONE
                        , currentContinueTarget = #currentContinueTarget env
                        }
                      , body
                      )
                  )) clauses
           ) :: acc
     | J.BreakStat _ => stat :: acc
     | J.ContinueStat optLabel =>
         let
           val canEliminateLabel =
             case (optLabel, #currentContinueTarget env) of
               (SOME label, SOME target) => J.eqId (label, target)
             | _ => false
         in
           if canEliminateLabel then J.ContinueStat NONE :: acc else stat :: acc
         end
     | J.DefaultExportStat exp => J.DefaultExportStat (doExp exp) :: acc
     | J.NamedExportStat _ => stat :: acc)
  and doBlock (env: Env, block) =
    Vector.fromList (Vector.foldr (doStat env) [] block)
  fun doProgram block = doBlock (initialEnv, block)
end;
