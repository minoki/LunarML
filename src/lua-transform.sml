(*
 * Copyright (c) 2023 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure LuaTransform :>
sig
  type Context = {nextId: int ref, maxUpvalue: int}
  structure InsertDo:
  sig
    val doBlock: int * LuaSyntax.Block -> LuaSyntax.Block
  end
  structure LuaJITFixup:
  sig
    val doBlock: Context -> LuaSyntax.Block -> LuaSyntax.Block
  end
  structure ProcessUpvalue:
  sig
    type Env
    val initialEnv: Env
    val initialEnvForLuaJIT: Env
    val doBlock: Context
                 -> Env
                 -> LuaSyntax.Block
                 -> LuaSyntax.VarAttr LuaSyntax.IdMap.map * LuaSyntax.Block
  end
  structure ProcessLocal:
  sig
    type Env
    val initialEnv: Env
    val initialEnvForLuaJIT: Env
    val doBlock: Context -> Env -> LuaSyntax.Block -> LuaSyntax.Block
  end
  structure StripUnusedLabels:
  sig
    val doBlock: LuaSyntax.Block -> LuaSyntax.Block
  end
  structure Reassociate:
  sig
    val doBlock: LuaSyntax.Block -> LuaSyntax.Block
  end
end =
struct
  structure L = LuaSyntax

  fun hasInnerFunction (L.ConstExp _) = false
    | hasInnerFunction (L.VarExp _) = false
    | hasInnerFunction (L.TableExp fields) =
        Vector.exists (fn (_, value) => hasInnerFunction value) fields
    | hasInnerFunction (L.CallExp (x, ys)) =
        hasInnerFunction x orelse Vector.exists hasInnerFunction ys
    | hasInnerFunction (L.MethodExp (x, _, ys)) =
        hasInnerFunction x orelse Vector.exists hasInnerFunction ys
    | hasInnerFunction (L.FunctionExp _) = true
    | hasInnerFunction (L.BinExp (_, x, y)) =
        hasInnerFunction x orelse hasInnerFunction y
    | hasInnerFunction (L.UnaryExp (_, x)) = hasInnerFunction x
  fun hasInnerFunctionStat (L.LocalStat (_, xs)) =
        List.exists hasInnerFunction xs
    | hasInnerFunctionStat (L.AssignStat (xs, ys)) =
        List.exists hasInnerFunction xs orelse List.exists hasInnerFunction ys
    | hasInnerFunctionStat (L.CallStat (x, ys)) =
        hasInnerFunction x orelse Vector.exists hasInnerFunction ys
    | hasInnerFunctionStat (L.MethodStat (x, _, ys)) =
        hasInnerFunction x orelse Vector.exists hasInnerFunction ys
    | hasInnerFunctionStat (L.IfStat (x, then', else')) =
        hasInnerFunction x orelse hasInnerFunctionBlock then'
        orelse hasInnerFunctionBlock else'
    | hasInnerFunctionStat (L.ReturnStat xs) = Vector.exists hasInnerFunction xs
    | hasInnerFunctionStat (L.DoStat {loopLike = _, body}) =
        hasInnerFunctionBlock body
    | hasInnerFunctionStat (L.GotoStat _) = false
    | hasInnerFunctionStat (L.LabelStat _) = false
  and hasInnerFunctionBlock block = Vector.exists hasInnerFunctionStat block

  fun sizeOfStat (L.LocalStat (_, xs), acc) = acc + List.length xs
    | sizeOfStat (L.AssignStat (xs, ys), acc) =
        acc + List.length xs + List.length ys
    | sizeOfStat (L.CallStat _, acc) = acc + 1
    | sizeOfStat (L.MethodStat _, acc) = acc + 1
    | sizeOfStat (L.IfStat (_, then', else'), acc) =
        sizeOfBlock (then', sizeOfBlock (else', acc + 1))
    | sizeOfStat (L.ReturnStat xs, acc) = acc + Vector.length xs
    | sizeOfStat (L.DoStat {loopLike = _, body}, acc) = sizeOfBlock (body, acc)
    | sizeOfStat (L.GotoStat _, acc) = acc + 1
    | sizeOfStat (L.LabelStat _, acc) = acc
  and sizeOfBlock (xs, acc) =
    Vector.foldl sizeOfStat acc xs

  fun freeVarsExp (_, L.ConstExp _) acc = acc
    | freeVarsExp (bound, L.VarExp x) acc =
        if L.IdSet.member (bound, x) then acc else L.IdSet.add (acc, x)
    | freeVarsExp (bound, L.TableExp fields) acc =
        Vector.foldl (fn ((_, x), acc) => freeVarsExp (bound, x) acc) acc fields
    | freeVarsExp (bound, L.CallExp (x, ys)) acc =
        Vector.foldl (fn (y, acc) => freeVarsExp (bound, y) acc)
          (freeVarsExp (bound, x) acc) ys
    | freeVarsExp (bound, L.MethodExp (x, _, ys)) acc =
        Vector.foldl (fn (y, acc) => freeVarsExp (bound, y) acc)
          (freeVarsExp (bound, x) acc) ys
    | freeVarsExp (bound, L.FunctionExp (params, body)) acc =
        let
          val bound' =
            Vector.foldl (fn (id, bound) => L.IdSet.add (bound, id)) bound
              params
        in
          freeVarsBlock (bound', body) acc
        end
    | freeVarsExp (bound, L.BinExp (_, x, y)) acc =
        freeVarsExp (bound, x) (freeVarsExp (bound, y) acc)
    | freeVarsExp (bound, L.UnaryExp (_, x)) acc =
        freeVarsExp (bound, x) acc
  and freeVarsStat (bound, L.LocalStat (vids, exps)) acc =
        let
          val acc =
            List.foldl (fn (x, acc) => freeVarsExp (bound, x) acc) acc exps
        in
          ( List.foldl
              (fn ((vid, _), bound) => L.IdSet.add (bound, L.UserDefinedId vid))
              bound vids
          , acc
          )
        end
    | freeVarsStat (bound, L.AssignStat (lhs, rhs)) acc =
        let
          val acc =
            List.foldl (fn (x, acc) => freeVarsExp (bound, x) acc) acc lhs
          val acc =
            List.foldl (fn (x, acc) => freeVarsExp (bound, x) acc) acc rhs
        in
          (bound, acc)
        end
    | freeVarsStat (bound, L.CallStat (x, ys)) acc =
        let
          val acc = freeVarsExp (bound, x) acc
          val acc =
            Vector.foldl (fn (x, acc) => freeVarsExp (bound, x) acc) acc ys
        in
          (bound, acc)
        end
    | freeVarsStat (bound, L.MethodStat (x, _, ys)) acc =
        let
          val acc = freeVarsExp (bound, x) acc
          val acc =
            Vector.foldl (fn (x, acc) => freeVarsExp (bound, x) acc) acc ys
        in
          (bound, acc)
        end
    | freeVarsStat (bound, L.IfStat (x, then', else')) acc =
        let
          val acc = freeVarsExp (bound, x) acc
          val acc = freeVarsBlock (bound, then') acc
          val acc = freeVarsBlock (bound, else') acc
        in
          (bound, acc)
        end
    | freeVarsStat (bound, L.ReturnStat xs) acc =
        (bound, Vector.foldl (fn (x, acc) => freeVarsExp (bound, x) acc) acc xs)
    | freeVarsStat (bound, L.DoStat {loopLike = _, body}) acc =
        let val acc = freeVarsBlock (bound, body) acc
        in (bound, acc)
        end
    | freeVarsStat (bound, L.GotoStat _) acc = (bound, acc)
    | freeVarsStat (bound, L.LabelStat _) acc = (bound, acc)
  and freeVarsBlock (bound, block) acc =
    let
      val (_, acc) =
        Vector.foldl (fn (stat, (bound, acc)) => freeVarsStat (bound, stat) acc)
          (bound, acc) block
    in
      acc
    end

  fun substExp _ (x as L.ConstExp _) = x
    | substExp map (x as L.VarExp id) =
        (case L.IdMap.find (map, id) of
           NONE => x
         | SOME y => y)
    | substExp map (L.TableExp fields) =
        L.TableExp (Vector.map (fn (key, x) => (key, substExp map x)) fields)
    | substExp map (L.CallExp (x, ys)) =
        L.CallExp (substExp map x, Vector.map (substExp map) ys)
    | substExp map (L.MethodExp (x, name, ys)) =
        L.MethodExp (substExp map x, name, Vector.map (substExp map) ys)
    | substExp map (L.FunctionExp (params, body)) =
        let
          val map' =
            Vector.foldl
              (fn (id, map) =>
                 if L.IdMap.inDomain (map, id) then
                   #1 (L.IdMap.remove (map, id))
                 else
                   map) map params
        in
          L.FunctionExp (params, substBlock map' body)
        end
    | substExp map (L.BinExp (binOp, x, y)) =
        L.BinExp (binOp, substExp map x, substExp map y)
    | substExp map (L.UnaryExp (unOp, x)) =
        L.UnaryExp (unOp, substExp map x)
  and substStat map (L.LocalStat (lhs, rhs)) =
        let
          val rhs = List.map (substExp map) rhs
          val map' =
            List.foldl
              (fn ((id, _), map) =>
                 let
                   val id = L.UserDefinedId id
                 in
                   if L.IdMap.inDomain (map, id) then
                     #1 (L.IdMap.remove (map, id))
                   else
                     map
                 end) map lhs
        in
          (map', L.LocalStat (lhs, rhs))
        end
    | substStat map (L.AssignStat (lhs, rhs)) =
        ( map
        , L.AssignStat
            (List.map (substExp map) lhs, List.map (substExp map) rhs)
        )
    | substStat map (L.CallStat (x, ys)) =
        (map, L.CallStat (substExp map x, Vector.map (substExp map) ys))
    | substStat map (L.MethodStat (x, name, ys)) =
        (map, L.MethodStat (substExp map x, name, Vector.map (substExp map) ys))
    | substStat map (L.IfStat (exp, then', else')) =
        ( map
        , L.IfStat
            (substExp map exp, substBlock map then', substBlock map else')
        )
    | substStat map (L.ReturnStat xs) =
        (map, L.ReturnStat (Vector.map (substExp map) xs))
    | substStat map (L.DoStat {loopLike, body}) =
        (map, L.DoStat {loopLike = loopLike, body = substBlock map body})
    | substStat map (stat as L.GotoStat _) = (map, stat)
    | substStat map (stat as L.LabelStat _) = (map, stat)
  and substBlock map block =
    let
      val (_, revStats) =
        Vector.foldl
          (fn (stat, (map, revStats)) =>
             let val (map, stat) = substStat map stat
             in (map, stat :: revStats)
             end) (map, []) block
    in
      Vector.fromList (List.rev revStats)
    end

  type Context = {nextId: int ref, maxUpvalue: int}
  datatype Variable = Plain of TypedSyntax.VId | Index of TypedSyntax.VId * int
  (* See mlinit.lua *)
  val mlinit_lua =
    List.foldl
      (fn (name, m) => L.IdMap.insert (m, L.PredefinedId name, L.CONST))
      L.IdMap.empty
      [ "_ENV"
      , "assert"
      , "error"
      , "getmetatable"
      , "pairs"
      , "pcall"
      , "setmetatable"
      , "math"
      , "math_abs"
      , "math_type"
      , "math_maxinteger"
      , "math_mininteger"
      , "math_ult"
      , "string"
      , "string_char"
      , "string_format"
      , "table"
      , "table_pack"
      , "table_unpack"
      , "_id"
      , "_exn_meta"
      , "_Match_tag"
      , "_Match"
      , "_Bind_tag"
      , "_Bind"
      , "_Overflow_tag"
      , "_Overflow"
      , "_Div_tag"
      , "_Div"
      , "_Size_tag"
      , "_Size"
      , "_Subscript_tag"
      , "_Subscript"
      , "_Fail_tag"
      , "_Fail"
      , "_Error_tag"
      , "_Error"
      , "_handle"
      , "_exnName"
      , "__exn_instanceof"
      , "_raise"
      , "_Int_add"
      , "_Int_sub"
      , "_Int_mul"
      , "_Int_div"
      , "_Int_mod"
      , "_Int_negate"
      , "_Int_abs"
      , "_Word_div"
      , "_Word_mod"
      , "_list"
      , "_Array_array"
      , "_VectorOrArray_fromList"
      , "_Vector_unsafeFromListRevN"
      , "_VectorOrArray_tabulate"
      , "_Vector_concat"
      , "_Lua_function"
      ]
  val mlinit_luajit =
    List.foldl
      (fn (name, m) => L.IdMap.insert (m, L.PredefinedId name, L.CONST))
      L.IdMap.empty
      [ "_G"
      , "assert"
      , "error"
      , "getmetatable"
      , "pairs"
      , "pcall"
      , "setmetatable"
      , "math"
      , "math_abs"
      , "math_floor"
      , "math_fmod"
      , "math_modf"
      , "string"
      , "string_char"
      , "string_format"
      , "table"
      , "select"
      , "table_pack"
      , "table_unpack"
      , "tonumber"
      , "bit"
      , "bit_bnot"
      , "bit_band"
      , "bit_bor"
      , "bit_bxor"
      , "bit_lshift"
      , "bit_rshift"
      , "ffi"
      , "int64_t"
      , "uint64_t"
      , "_id"
      , "_exn_meta"
      , "_Match_tag"
      , "_Match"
      , "_Bind_tag"
      , "_Bind"
      , "_Overflow_tag"
      , "_Overflow"
      , "_Div_tag"
      , "_Div"
      , "_Size_tag"
      , "_Size"
      , "_Subscript_tag"
      , "_Subscript"
      , "_Fail_tag"
      , "_Fail"
      , "_Error_tag"
      , "_Error"
      , "_handle"
      , "_exnName"
      , "__exn_instanceof"
      , "_raise"
      , "MIN_INT32"
      , "MAX_INT32"
      , "_Int54_add"
      , "_Int54_sub"
      , "_Int54_mul"
      , "_Int54_div"
      , "_Int54_quot"
      , "_Int54_mod"
      , "_Int54_negate"
      , "_Int54_abs"
      , "_Word32_mul"
      , "NEGATIVE_ZERO"
      , "_Real_mul"
      , "_list"
      , "_Array_array"
      , "_VectorOrArray_fromList"
      , "_Vector_unsafeFromListRevN"
      , "_VectorOrArray_tabulate"
      , "_Vector_concat"
      , "_Lua_function"
      ]
  fun genSym (ctx: Context, name) =
    let
      val n = !(#nextId ctx)
      val _ = #nextId ctx := n + 1
    in
      TypedSyntax.MkVId (name, n)
    end
  structure InsertDo =
  struct
    fun annotateStat
      (stat: L.Stat, (live: L.IdSet.set, rest: (L.IdSet.set * L.Stat) list)) =
      case stat of
        L.LocalStat (vars, exps) =>
          let
            val live' =
              List.foldl
                (fn ((var, _), live) =>
                   L.IdSet.subtract (live, L.UserDefinedId var)) live vars
            val live'' =
              List.foldl
                (fn (exp, live) => freeVarsExp (L.IdSet.empty, exp) live) live'
                exps
          in
            (live'', (live'', stat) :: rest)
          end
      | _ =>
          let val (_, live') = freeVarsStat (L.IdSet.empty, stat) live
          in (live', (live', stat) :: rest)
          end
    fun hoist targets ((stat as L.LocalStat (vars, exps)), (hoistedAcc, acc)) =
          let
            val (hoisted, keep) =
              List.partition
                (fn (v, _) => L.IdSet.member (targets, L.UserDefinedId v)) vars
            val hoistedAcc' =
              List.foldl
                (fn ((v, L.CONST), vars) => (v, L.LATE_INIT) :: vars
                  | (va, vars) => va :: vars) hoistedAcc hoisted
          in
            case (hoisted, keep, exps) of
              ([], _, _) => (hoistedAcc, stat :: acc)
            | (_, [], []) => (hoistedAcc', acc)
            | (_, _, []) => (hoistedAcc', L.LocalStat (keep, exps) :: acc)
            | (_, [], _) =>
                ( hoistedAcc'
                , L.AssignStat
                    ( List.map (fn (v, _) => L.VarExp (L.UserDefinedId v)) vars
                    , exps
                    ) :: acc
                )
            | (_, _, _) =>
                let
                  val keep' =
                    List.map (fn (v, L.CONST) => (v, L.LATE_INIT) | va => va)
                      keep
                in
                  ( hoistedAcc'
                  , L.LocalStat (keep', [])
                    ::
                    L.AssignStat
                      ( List.map (fn (v, _) => L.VarExp (L.UserDefinedId v))
                          vars
                      , exps
                      ) :: acc
                  )
                end
          end
      | hoist _ (stat, (hoistedAcc, acc)) = (hoistedAcc, stat :: acc)
    fun doExp (e as L.ConstExp _) = e
      | doExp (e as L.VarExp _) = e
      | doExp (L.TableExp fields) =
          L.TableExp (Vector.map (fn (key, e) => (key, doExp e)) fields)
      | doExp (L.CallExp (f, args)) =
          L.CallExp (doExp f, Vector.map doExp args)
      | doExp (L.MethodExp (self, name, args)) =
          L.MethodExp (doExp self, name, Vector.map doExp args)
      | doExp (L.FunctionExp (params, block)) =
          L.FunctionExp (params, doBlock (Vector.length params, block))
      | doExp (L.BinExp (p, x, y)) =
          L.BinExp (p, doExp x, doExp y)
      | doExp (L.UnaryExp (p, x)) =
          L.UnaryExp (p, doExp x)
    and doBlock (numOuter, block) =
      let
        val (_, annotatedBlock) =
          Vector.foldr annotateStat (L.IdSet.empty, []) block
        fun insertDo (vars, live, stat, rest, declared, numOuter, revStats) =
          let
            val dead = L.IdSet.difference (declared, live)
            val numDead = L.IdSet.numItems dead
            val shouldInsertDo =
              numDead >= 10 andalso numOuter + L.IdSet.numItems declared <= 190
          in
            if shouldInsertDo then
              let
                val (hoistedVars, stats) =
                  List.foldl (hoist live) ([], []) revStats
                val dec =
                  if List.null hoistedVars then [] (* should not occur *)
                  else [L.LocalStat (hoistedVars, [])]
                val doStat = L.DoStat {loopLike = false, body = vector stats}
                val declared' =
                  List.foldl
                    (fn ((v, _), acc) => L.IdSet.add (acc, L.UserDefinedId v))
                    L.IdSet.empty vars
              in
                dec
                @
                doStat
                ::
                goForward
                  (rest, declared', numOuter + List.length hoistedVars, [stat])
              end
            else
              goForward
                ( rest
                , List.foldl
                    (fn ((v, _), acc) => L.IdSet.add (acc, L.UserDefinedId v))
                    declared vars
                , numOuter
                , stat :: revStats
                )
          end
        and goForward ([], _: L.IdSet.set, _: int, revStats) = List.rev revStats
          | goForward
              ( (live, L.LocalStat (vars, exps)) :: rest
              , declared
              , numOuter
              , revStats
              ) =
              insertDo
                ( vars
                , live
                , L.LocalStat (vars, List.map doExp exps)
                , rest
                , declared
                , numOuter
                , revStats
                )
          | goForward
              ( (_, L.AssignStat (lhs, rhs)) :: rest
              , declared
              , numOuter
              , revStats
              ) =
              let
                val stat = L.AssignStat (List.map doExp lhs, List.map doExp rhs)
              in
                goForward (rest, declared, numOuter, stat :: revStats)
              end
          | goForward
              ((_, L.CallStat (f, args)) :: rest, declared, numOuter, revStats) =
              let val stat = L.CallStat (doExp f, Vector.map doExp args)
              in goForward (rest, declared, numOuter, stat :: revStats)
              end
          | goForward
              ( (_, L.MethodStat (self, name, args)) :: rest
              , declared
              , numOuter
              , revStats
              ) =
              let
                val stat = L.MethodStat
                  (doExp self, name, Vector.map doExp args)
              in
                goForward (rest, declared, numOuter, stat :: revStats)
              end
          | goForward
              ( (live, L.IfStat (cond, thenBlock, elseBlock)) :: rest
              , declared
              , numOuter
              , revStats
              ) =
              let
                val n = numOuter + L.IdSet.numItems declared
              in
                insertDo
                  ( []
                  , live
                  , L.IfStat
                      ( doExp cond
                      , doBlock (n, thenBlock)
                      , doBlock (n, elseBlock)
                      )
                  , rest
                  , declared
                  , numOuter
                  , revStats
                  )
              end
          | goForward
              ((_, L.ReturnStat exps) :: rest, declared, numOuter, revStats) =
              let val stat = L.ReturnStat (Vector.map doExp exps)
              in goForward (rest, declared, numOuter, stat :: revStats)
              end
          | goForward
              ( (_, L.DoStat {loopLike, body}) :: rest
              , declared
              , numOuter
              , revStats
              ) =
              let
                val n = numOuter + L.IdSet.numItems declared
                val stat = L.DoStat
                  {loopLike = loopLike, body = doBlock (n, body)}
              in
                goForward (rest, declared, numOuter, stat :: revStats)
              end
          | goForward
              ((_, stat as L.GotoStat _) :: rest, declared, numOuter, revStats) =
              goForward (rest, declared, numOuter, stat :: revStats)
          | goForward
              ( (live, stat as L.LabelStat _) :: rest
              , declared
              , numOuter
              , revStats
              ) =
              let
                val dead = L.IdSet.difference (declared, live)
                val numDead = L.IdSet.numItems dead
                val shouldInsertDo =
                  numDead >= 1
                  andalso numOuter + L.IdSet.numItems declared <= 190
                val (stats', numOuter') =
                  if shouldInsertDo then
                    let
                      val (hoistedVars, stats) =
                        List.foldl (hoist live) ([], []) revStats
                      val dec =
                        if List.null hoistedVars then [] (* should not occur *)
                        else [L.LocalStat (hoistedVars, [])]
                      val doStat =
                        L.DoStat {loopLike = false, body = vector stats}
                    in
                      (dec @ [doStat], numOuter + List.length hoistedVars)
                    end
                  else
                    (List.rev revStats, numOuter + L.IdSet.numItems declared)
                val rest' = goForward (rest, L.IdSet.empty, numOuter', [])
              in
                stats' @ stat :: rest'
              end
      in
        vector (goForward (annotatedBlock, L.IdSet.empty, numOuter, []))
      end
  end
  structure LuaJITFixup =
  struct
    val BODY_SIZE_THRESHOLD = 500
    fun doExp _ (x as L.ConstExp _) = x
      | doExp _ (x as L.VarExp _) = x
      | doExp ctx (L.TableExp fields) =
          L.TableExp
            (Vector.map (fn (key, value) => (key, doExp ctx value)) fields)
      | doExp ctx (L.CallExp (x, ys)) =
          L.CallExp (doExp ctx x, Vector.map (doExp ctx) ys)
      | doExp ctx (L.MethodExp (x, name, ys)) =
          L.MethodExp (doExp ctx x, name, Vector.map (doExp ctx) ys)
      | doExp ctx (x as L.FunctionExp (params, body)) =
          if hasInnerFunctionBlock body then
            if sizeOfBlock (body, 0) >= BODY_SIZE_THRESHOLD then
              let
                val body = Vector.foldr (op::) [] (doBlock ctx body)
                val dummy = genSym (ctx, "DUMMY")
                val body =
                  L.IfStat (L.ConstExp L.True, vector [], vector
                    [L.LocalStat
                       ( [(dummy, L.CONST)]
                       , [L.FunctionExp (vector [], vector [])]
                       )]) :: body
              in
                L.FunctionExp (params, Vector.fromList body)
              end
            else
              L.FunctionExp (params, doBlock ctx body)
          else
            x
      | doExp ctx (L.BinExp (binOp, x, y)) =
          L.BinExp (binOp, doExp ctx x, doExp ctx y)
      | doExp ctx (L.UnaryExp (unOp, x)) =
          L.UnaryExp (unOp, doExp ctx x)
    and doStat ctx (L.LocalStat (vars, xs)) =
          L.LocalStat (vars, List.map (doExp ctx) xs)
      | doStat ctx (L.AssignStat (xs, ys)) =
          L.AssignStat (List.map (doExp ctx) xs, List.map (doExp ctx) ys)
      | doStat ctx (L.CallStat (x, ys)) =
          L.CallStat (doExp ctx x, Vector.map (doExp ctx) ys)
      | doStat ctx (L.MethodStat (x, name, ys)) =
          L.MethodStat (doExp ctx x, name, Vector.map (doExp ctx) ys)
      | doStat ctx (L.IfStat (x, then', else')) =
          L.IfStat (doExp ctx x, doBlock ctx then', doBlock ctx else')
      | doStat ctx (L.ReturnStat xs) =
          L.ReturnStat (Vector.map (doExp ctx) xs)
      | doStat ctx (L.DoStat {loopLike, body}) =
          L.DoStat {loopLike = loopLike, body = doBlock ctx body}
      | doStat _ (stat as L.GotoStat _) = stat
      | doStat _ (stat as L.LabelStat _) = stat
    and doBlock ctx block =
      Vector.map (doStat ctx) block
  end
  structure ProcessUpvalue =
  struct
    type Env = {bound: L.VarAttr L.IdMap.map, dynamic: L.VarAttr L.IdMap.map}
    val initialEnv: Env = {bound = mlinit_lua, dynamic = mlinit_lua}
    val initialEnvForLuaJIT: Env =
      {bound = mlinit_luajit, dynamic = mlinit_luajit}
    fun doExp (_: Context) (_: Env) (exp as L.ConstExp _) = ([], exp)
      | doExp _ _ (exp as L.VarExp _) = ([], exp)
      | doExp ctx env (L.TableExp fields) =
          let
            val (decs, fields) =
              Vector.foldr
                (fn ((key, value), (decs, fields)) =>
                   let val (decs', value) = doExp ctx env value
                   in (decs' @ decs, (key, value) :: fields)
                   end) ([], []) fields
          in
            (decs, L.TableExp (Vector.fromList fields))
          end
      | doExp ctx env (L.CallExp (exp, args)) =
          let
            val (decs, exp) = doExp ctx env exp
            val (decs', args) =
              Vector.foldr
                (fn (x, (decs, xs)) => let val (decs', x) = doExp ctx env x
                                       in (decs' @ decs, x :: xs)
                                       end) ([], []) args
          in
            (decs @ decs', L.CallExp (exp, vector args))
          end
      | doExp ctx env (L.MethodExp (self, method, args)) =
          let
            val (decs, self) = doExp ctx env self
            val (decs', args) =
              Vector.foldr
                (fn (x, (decs, xs)) => let val (decs', x) = doExp ctx env x
                                       in (decs' @ decs, x :: xs)
                                       end) ([], []) args
          in
            (decs @ decs', L.MethodExp (self, method, vector args))
          end
      | doExp ctx env (L.FunctionExp (params, body)) =
          let
            val innerEnv =
              { bound =
                  Vector.foldl
                    (fn (p, bound) => L.IdMap.insert (bound, p, L.CONST))
                    (#bound env) params
              , dynamic =
                  Vector.foldl
                    (fn (p, dynamic) => L.IdMap.insert (dynamic, p, L.CONST))
                    (#dynamic env) params
              }
            val paramSet =
              Vector.foldl (fn (p, bound) => L.IdSet.add (bound, p))
                L.IdSet.empty params
            val fv = freeVarsBlock (paramSet, body) L.IdSet.empty
            val upvaluesMap =
              L.IdMap.filteri (fn (id, _) => L.IdSet.member (fv, id))
                (#dynamic env)
          in
            if #maxUpvalue ctx < L.IdMap.numItems upvaluesMap then
              let
                val upvaluesList =
                  L.IdMap.foldli (fn (id, attr, acc) => (id, attr) :: acc) []
                    upvaluesMap
                val (constUpvalues, nonconstUpvalues) =
                  List.partition (fn (_, L.CONST) => true | _ => false)
                    upvaluesList
                val n = List.length nonconstUpvalues
                val escapeList =
                  if #maxUpvalue ctx - 1 < n then
                    constUpvalues (* Need a better algorithm *)
                  else
                    List.drop (constUpvalues, #maxUpvalue ctx - n - 1)
                val vid = genSym (ctx, "UPVAL")
                val dec = L.LocalStat
                  ( [(vid, L.CONST)]
                  , [L.TableExp (vector (List.rev (#2
                       (List.foldl
                          (fn ((id, _), (i, acc)) =>
                             (i + 1, (L.IntKey i, L.VarExp id) :: acc)) (1, [])
                          escapeList))))]
                  )
                val (_, subst) =
                  List.foldl
                    (fn ((id, _), (i, map)) =>
                       ( i + 1
                       , L.IdMap.insert
                           ( map
                           , id
                           , L.IndexExp
                               ( L.VarExp (L.UserDefinedId vid)
                               , L.ConstExp (L.Numeral (Int.toString i))
                               )
                           )
                       )) (1, L.IdMap.empty) escapeList
                val innerEnv' =
                  { bound =
                      L.IdMap.insert
                        (#bound innerEnv, L.UserDefinedId vid, L.CONST)
                  , dynamic =
                      L.IdMap.insert
                        (#dynamic innerEnv, L.UserDefinedId vid, L.CONST)
                  }
                val (_, body') = doBlock ctx innerEnv' (substBlock subst body)
              in
                ([dec], L.FunctionExp (params, body'))
              end
            else
              let val (_, body') = doBlock ctx innerEnv body
              in ([], L.FunctionExp (params, body'))
              end
          end
      | doExp ctx env (L.BinExp (binOp, a, b)) =
          let
            val (decs, a) = doExp ctx env a
            val (decs', b) = doExp ctx env b
          in
            (decs @ decs', L.BinExp (binOp, a, b))
          end
      | doExp ctx env (L.UnaryExp (unOp, a)) =
          let val (decs, a) = doExp ctx env a
          in (decs, L.UnaryExp (unOp, a))
          end
    and doStat ctx env (L.LocalStat (vars, exps)) =
          let
            val newEnv =
              { bound =
                  List.foldl
                    (fn ((vid, attr), m) =>
                       L.IdMap.insert (m, L.UserDefinedId vid, attr))
                    (#bound env) vars
              , dynamic =
                  List.foldl
                    (fn ((vid, attr), m) =>
                       L.IdMap.insert (m, L.UserDefinedId vid, attr))
                    (#dynamic env) vars
              }
            val (decs, exps) =
              List.foldr
                (fn (x, (decs, xs)) => let val (decs', x) = doExp ctx env x
                                       in (decs' @ decs, x :: xs)
                                       end) ([], []) exps
          in
            case decs of
              [] => (newEnv, [L.LocalStat (vars, exps)])
            | _ :: _ =>
                ( newEnv
                , [ L.LocalStat
                      ( List.map
                          (fn (vid, L.CONST) => (vid, L.LATE_INIT) | x => x)
                          vars
                      , []
                      )
                  , L.DoStat
                      { loopLike = false
                      , body = vector
                          (decs
                           @
                           [L.AssignStat
                              ( List.map
                                  (fn (vid, _) => L.VarExp (L.UserDefinedId vid))
                                  vars
                              , exps
                              )])
                      }
                  ]
                )
          end
      | doStat ctx env (L.AssignStat (vars, exps)) =
          let
            val (decs, vars) =
              List.foldr
                (fn (x, (decs, xs)) => let val (decs', x) = doExp ctx env x
                                       in (decs' @ decs, x :: xs)
                                       end) ([], []) vars
            val (decs', exps) =
              List.foldr
                (fn (x, (decs, xs)) => let val (decs', x) = doExp ctx env x
                                       in (decs' @ decs, x :: xs)
                                       end) ([], []) exps
            val newEnv =
              { bound = #bound env
              , dynamic =
                  List.foldl
                    (fn (L.VarExp id, dynamic) =>
                       (case L.IdMap.find (dynamic, id) of
                          NONE => dynamic
                        | SOME L.LATE_INIT =>
                            L.IdMap.insert (dynamic, id, L.CONST)
                        | SOME _ => dynamic)
                      | (_, dynamic) => dynamic) (#dynamic env) vars
              }
          in
            (newEnv, decs @ decs' @ [L.AssignStat (vars, exps)])
          end
      | doStat ctx env (L.CallStat (exp, args)) =
          let
            val (decs, exp) = doExp ctx env exp
            val (decs', args) =
              Vector.foldr
                (fn (x, (decs, xs)) => let val (decs', x) = doExp ctx env x
                                       in (decs' @ decs, x :: xs)
                                       end) ([], []) args
          in
            (env, decs @ decs' @ [L.CallStat (exp, vector args)])
          end
      | doStat ctx env (L.MethodStat (self, method, args)) =
          let
            val (decs, self) = doExp ctx env self
            val (decs', args) =
              Vector.foldr
                (fn (x, (decs, xs)) => let val (decs', x) = doExp ctx env x
                                       in (decs' @ decs, x :: xs)
                                       end) ([], []) args
          in
            (env, decs @ decs' @ [L.MethodStat (self, method, vector args)])
          end
      | doStat ctx env (L.IfStat (cond, thenPart, elsePart)) =
          let
            val (decs, cond) = doExp ctx env cond
            val (dynamic1, thenPart) = doBlock ctx env thenPart
            val (dynamic2, elsePart) = doBlock ctx env elsePart
            val newEnv =
              { bound = #bound env
              , dynamic =
                  L.IdMap.mapi
                    (fn (_, attr as L.CONST) => attr
                      | (_, attr as L.MUTABLE) => attr
                      | (id, attr as L.LATE_INIT) =>
                       (case L.IdMap.find (dynamic1, id) of
                          SOME L.CONST =>
                            (case L.IdMap.find (dynamic2, id) of
                               SOME L.CONST => L.CONST
                             | _ => attr)
                        | _ => attr)) (#dynamic env)
              }
          in
            (newEnv, decs @ [L.IfStat (cond, thenPart, elsePart)])
          end
      | doStat ctx env (L.ReturnStat results) =
          let
            val (decs, results) =
              Vector.foldr
                (fn (x, (decs, xs)) => let val (decs', x) = doExp ctx env x
                                       in (decs' @ decs, x :: xs)
                                       end) ([], []) results
          in
            (env, decs @ [L.ReturnStat (vector results)])
          end
      | doStat ctx env (L.DoStat {loopLike, body}) =
          let
            val (dynamic, body) = doBlock ctx env body
            val newEnv =
              { bound = #bound env
              , dynamic =
                  L.IdMap.mapi
                    (fn (_, attr as L.CONST) => attr
                      | (_, attr as L.MUTABLE) => attr
                      | (id, attr as L.LATE_INIT) =>
                       (case L.IdMap.find (dynamic, id) of
                          SOME L.CONST => L.CONST
                        | _ => attr)) (#dynamic env)
              }
          in
            (newEnv, [L.DoStat {loopLike = loopLike, body = body}])
          end
      | doStat _ env (stat as L.GotoStat _) = (env, [stat])
      | doStat _ env (stat as L.LabelStat _) = (env, [stat])
    and doStats _ env [] acc =
          (env, List.concat (List.rev acc))
      | doStats ctx env
          (stats as (L.AssignStat ([L.VarExp _], [L.FunctionExp (_, _)]) :: _))
          acc =
          let
            val (env', defs, stats') = takeFunctionAssignments ctx env stats []
            val (decs, inits, funcs) =
              List.foldr
                (fn ((v, f), (decs, inits, funcs)) =>
                   case doExp ctx env' f of
                     ([L.LocalStat ([(u, L.CONST)], init)], f') =>
                       ( (u, L.LATE_INIT) :: decs
                       , L.AssignStat ([L.VarExp (L.UserDefinedId u)], init)
                         :: inits
                       , L.AssignStat ([L.VarExp v], [f']) :: funcs
                       )
                   | ([], f') =>
                       (decs, inits, L.AssignStat ([L.VarExp v], [f']) :: funcs)
                   | _ => raise Fail "ProcessUpvalue: unexpected transformation")
                ([], [], []) defs
          in
            case decs of
              [] => doStats ctx env' stats' ([funcs @ inits] @ acc)
            | _ :: _ =>
                doStats ctx env' stats'
                  ([L.DoStat
                      { loopLike = false
                      , body = vector
                          (L.LocalStat (decs, [])
                           :: List.rev funcs @ List.rev inits)
                      }] :: acc)
          end
      | doStats ctx env (stat :: stats) acc =
          let val (newEnv, stat') = doStat ctx env stat
          in doStats ctx newEnv stats (stat' :: acc)
          end
    and takeFunctionAssignments ctx env
          (stats as
             (L.AssignStat ([L.VarExp v], [f as L.FunctionExp (_, _)]) :: stats'))
          revAcc =
          (case L.IdMap.find (#dynamic env, v) of
             SOME L.LATE_INIT =>
               let
                 val newEnv =
                   { bound = #bound env
                   , dynamic = L.IdMap.insert (#dynamic env, v, L.CONST)
                   }
               in
                 takeFunctionAssignments ctx newEnv stats' ((v, f) :: revAcc)
               end
           | _ => (env, List.rev revAcc, stats))
      | takeFunctionAssignments _ env stats revAcc =
          (env, List.rev revAcc, stats)
    and doBlock ctx env stats =
      let
        val (env', stats) = doStats ctx env (Vector.foldr (op::) [] stats) []
        val dynamic = #dynamic env' (* assumes no shadowing *)
      in
        (dynamic, vector stats)
      end
  end
  structure ProcessLocal =
  struct
    type Env =
      { currentLocals: int
      , locals: (TypedSyntax.VId * int ref) option
      , valMap: Variable TypedSyntax.VIdMap.map
      }
    val initialEnv: Env =
      { currentLocals = L.IdMap.numItems mlinit_lua
      , locals = NONE
      , valMap = TypedSyntax.VIdMap.empty
      }
    val initialEnvForLuaJIT: Env =
      { currentLocals = L.IdMap.numItems mlinit_luajit
      , locals = NONE
      , valMap = TypedSyntax.VIdMap.empty
      }
    val LOCAL_LIMIT = 190
    fun doExp (_: Context) (_: Env) (exp as L.ConstExp _) = exp
      | doExp _ _ (exp as L.VarExp (L.PredefinedId _)) = exp
      | doExp _ env (exp as L.VarExp (L.UserDefinedId vid)) =
          (case TypedSyntax.VIdMap.find (#valMap env, vid) of
             NONE => exp
           | SOME (Plain vid) => L.VarExp (L.UserDefinedId vid)
           | SOME (Index (locals, n)) =>
               L.IndexExp (L.VarExp (L.UserDefinedId locals), L.ConstExp
                 (L.Numeral (Int.toString n))))
      | doExp ctx env (L.TableExp fields) =
          L.TableExp
            (Vector.map (fn (key, value) => (key, doExp ctx env value)) fields)
      | doExp ctx env (L.CallExp (exp, args)) =
          L.CallExp (doExp ctx env exp, Vector.map (doExp ctx env) args)
      | doExp ctx env (L.MethodExp (self, method, args)) =
          L.MethodExp
            (doExp ctx env self, method, Vector.map (doExp ctx env) args)
      | doExp ctx env (L.FunctionExp (params, body)) =
          let
            val innerEnv =
              { currentLocals = Vector.length params
              , locals = NONE
              , valMap =
                  Vector.foldl
                    (fn (L.PredefinedId _, valMap) => valMap
                      | (L.UserDefinedId vid, valMap) =>
                       TypedSyntax.VIdMap.insert (valMap, vid, Plain vid))
                    (#valMap env) params
              }
          in
            L.FunctionExp (params, doBlock ctx innerEnv body)
          end
      | doExp ctx env (L.BinExp (binOp, a, b)) =
          L.BinExp (binOp, doExp ctx env a, doExp ctx env b)
      | doExp ctx env (L.UnaryExp (unOp, a)) =
          L.UnaryExp (unOp, doExp ctx env a)
    and doStat ctx env (L.LocalStat (vars, exps)) =
          let
            val newLocals = #currentLocals env + List.length vars
          in
            if newLocals > LOCAL_LIMIT then
              let
                val (n, locals, r, dec) =
                  case #locals env of
                    SOME (locals, r) => (#currentLocals env, locals, r, [])
                  | NONE =>
                      let
                        val locals = genSym (ctx, "_L")
                      in
                        ( #currentLocals env + 1
                        , locals
                        , ref 1
                        , [L.LocalStat
                             ([(locals, L.CONST)], [L.TableExp (vector [])])]
                        )
                      end
                val (vars, valMap) =
                  List.foldl
                    (fn ((vid, _), (acc, valMap)) =>
                       let
                         val i = !r
                       in
                         r := i + 1;
                         ( L.IndexExp
                             ( L.VarExp (L.UserDefinedId locals)
                             , L.ConstExp (L.Numeral (Int.toString i))
                             ) :: acc
                         , TypedSyntax.VIdMap.insert
                             (valMap, vid, Index (locals, i))
                         )
                       end) ([], #valMap env) vars
                val newEnv =
                  { currentLocals = n
                  , locals = SOME (locals, r)
                  , valMap = valMap
                  }
              in
                if List.null exps then
                  (newEnv, dec)
                else
                  ( newEnv
                  , dec
                    @
                    [L.AssignStat (List.rev vars, List.map (doExp ctx env) exps)]
                  )
              end
            else
              let
                val newEnv =
                  { currentLocals = newLocals
                  , locals = #locals env
                  , valMap = #valMap env
                  }
              in
                (newEnv, [L.LocalStat (vars, List.map (doExp ctx env) exps)])
              end
          end
      | doStat ctx env (L.AssignStat (vars, exps)) =
          ( env
          , [L.AssignStat
               (List.map (doExp ctx env) vars, List.map (doExp ctx env) exps)]
          )
      | doStat ctx env (L.CallStat (exp, args)) =
          ( env
          , [L.CallStat (doExp ctx env exp, Vector.map (doExp ctx env) args)]
          )
      | doStat ctx env (L.MethodStat (self, method, args)) =
          ( env
          , [L.MethodStat
               (doExp ctx env self, method, Vector.map (doExp ctx env) args)]
          )
      | doStat ctx env (L.IfStat (cond, thenPart, elsePart)) =
          ( env
          , [L.IfStat
               ( doExp ctx env cond
               , doBlock ctx env thenPart
               , doBlock ctx env elsePart
               )]
          )
      | doStat ctx env (L.ReturnStat results) =
          (env, [L.ReturnStat (Vector.map (doExp ctx env) results)])
      | doStat ctx env (L.DoStat {loopLike = true, body}) =
          (case #locals env of
             SOME _ =>
               let
                 val locals = genSym (ctx, "_L")
                 val env' =
                   { currentLocals = #currentLocals env + 1
                   , locals = SOME (locals, ref 1)
                   , valMap = #valMap env
                   }
                 val dec = L.LocalStat
                   ([(locals, L.CONST)], [L.TableExp (vector [])])
               in
                 ( env
                 , [L.DoStat
                      { loopLike = true
                      , body = vector
                          (dec :: Vector.foldr (op::) [] (doBlock ctx env' body))
                      }]
                 )
               end
           | NONE =>
               (env, [L.DoStat {loopLike = true, body = doBlock ctx env body}]))
      | doStat ctx env (L.DoStat {loopLike = false, body}) =
          (env, [L.DoStat {loopLike = false, body = doBlock ctx env body}])
      | doStat _ env (stat as L.GotoStat _) = (env, [stat])
      | doStat _ env (stat as L.LabelStat _) = (env, [stat])
    and doBlock ctx env stats =
      vector (List.concat (List.rev (#2
        (Vector.foldl
           (fn (stat, (env, acc)) => let val (env, stat) = doStat ctx env stat
                                     in (env, stat :: acc)
                                     end) (env, []) stats))))
  end
  structure StripUnusedLabels =
  struct
    structure LabelSet = TypedSyntax.VIdSet
    (*:
    val usedLabelsExp : L.Exp * LabelSet.set -> LabelSet.set
    and usedLabelsStat : L.Stat * LabelSet.set -> LabelSet.set
    and usedLabelsBlock : L.Block * LabelSet.set -> LabelSet.set
     *)
    fun usedLabelsExp (L.ConstExp _, acc) = acc
      | usedLabelsExp (L.VarExp _, acc) = acc
      | usedLabelsExp (L.TableExp elems, acc) =
          Vector.foldl (fn ((_, x), acc) => usedLabelsExp (x, acc)) acc elems
      | usedLabelsExp (L.CallExp (x, args), acc) =
          Vector.foldl usedLabelsExp (usedLabelsExp (x, acc)) args
      | usedLabelsExp (L.MethodExp (x, _, args), acc) =
          Vector.foldl usedLabelsExp (usedLabelsExp (x, acc)) args
      | usedLabelsExp (L.FunctionExp (_, body), acc) =
          usedLabelsBlock (body, acc)
      | usedLabelsExp (L.BinExp (_, x, y), acc) =
          usedLabelsExp (y, usedLabelsExp (x, acc))
      | usedLabelsExp (L.UnaryExp (_, x), acc) = usedLabelsExp (x, acc)
    and usedLabelsStat (L.LocalStat (_, rhs), acc) =
          List.foldl usedLabelsExp acc rhs
      | usedLabelsStat (L.AssignStat (lhs, rhs), acc) =
          List.foldl usedLabelsExp (List.foldl usedLabelsExp acc lhs) rhs
      | usedLabelsStat (L.CallStat (x, args), acc) =
          Vector.foldl usedLabelsExp (usedLabelsExp (x, acc)) args
      | usedLabelsStat (L.MethodStat (x, _, args), acc) =
          Vector.foldl usedLabelsExp (usedLabelsExp (x, acc)) args
      | usedLabelsStat (L.IfStat (x, t, e), acc) =
          usedLabelsBlock (e, usedLabelsBlock (t, usedLabelsExp (x, acc)))
      | usedLabelsStat (L.ReturnStat xs, acc) =
          Vector.foldl usedLabelsExp acc xs
      | usedLabelsStat (L.DoStat {loopLike = _, body}, acc) =
          usedLabelsBlock (body, acc)
      | usedLabelsStat (L.GotoStat label, acc) = LabelSet.add (acc, label)
      | usedLabelsStat (L.LabelStat _, acc) = acc
    and usedLabelsBlock (block, acc) =
      Vector.foldl usedLabelsStat acc block
    fun doBlock block =
      let
        val used = usedLabelsBlock (block, LabelSet.empty)
        fun goStat (stat as L.LabelStat label, acc) =
              if LabelSet.member (used, label) then stat :: acc else acc
          | goStat (stat, acc) = stat :: acc
        fun doBlock block =
          Vector.fromList (Vector.foldr goStat [] block)
      in
        L.recursePost {exp = fn x => x, stat = fn x => x, block = doBlock} block
      end
  end
  structure Reassociate =
  struct
    fun collectConcat (L.BinExp (L.CONCAT, x, y), acc) =
          collectConcat (x, collectConcat (y, acc))
      | collectConcat (x, acc) = x :: acc
    fun doExp (L.BinExp (L.CONCAT, L.BinExp (L.CONCAT, x, y), z)) =
          let val xs = collectConcat (x, collectConcat (y, []))
          in List.foldr (fn (a, acc) => L.BinExp (L.CONCAT, a, acc)) z xs
          end
      | doExp x = x
    fun doBlock block =
      L.recursePre {exp = doExp, stat = fn x => x, block = fn x => x} block
  end
end;
