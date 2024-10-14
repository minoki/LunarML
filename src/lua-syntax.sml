(*
 * Copyright (c) 2023 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure LuaSyntax :>
sig
  datatype TableKey = IntKey of int | StringKey of string
  datatype Id = PredefinedId of string | UserDefinedId of TypedSyntax.VId
  type Label = TypedSyntax.VId
  structure IdSet: ORD_SET where type Key.ord_key = Id
  structure IdMap: ORD_MAP where type Key.ord_key = Id
  datatype VarAttr = CONST | LATE_INIT | MUTABLE
  datatype LuaConst =
    Nil
  | False
  | True
  | Numeral of string (* integer, word (hexadecimal), floating-point *)
  | LiteralString of string
  datatype BinaryOp =
    PLUS
  | MINUS
  | TIMES
  | DIV
  | INTDIV
  | POW
  | MOD
  | BITAND
  | BITXOR
  | BITOR
  | RSHIFT
  | LSHIFT
  | CONCAT
  | LT
  | LE
  | GT
  | GE
  | EQUAL
  | NOTEQUAL
  | AND
  | OR
  | INDEX (* a[b] *)
  datatype UnaryOp =
    NEGATE (* -a *)
  | NOT (* not a *)
  | LENGTH (* #a *)
  | BITNOT (* ~a *)
  | SINGLE_VALUE (* (f(...)) *)
  datatype Exp =
    ConstExp of LuaConst
  | VarExp of Id
  | TableExp of (TableKey * Exp) vector
  | CallExp of Exp * Exp vector
  | MethodExp of Exp * string * Exp vector
  | FunctionExp of
      Id vector * Stat vector (* function parameters are implicitly const *)
  | BinExp of BinaryOp * Exp * Exp
  | UnaryExp of UnaryOp * Exp
  and Stat =
    LocalStat of
      (TypedSyntax.VId * VarAttr) list * Exp list (* vars must not be empty *)
  | AssignStat of
      Exp list
      * Exp list (* LHS must be non-empty prefixexps and RHS must not be empty*)
  | CallStat of Exp * Exp vector
  | MethodStat of
      Exp * string * Exp vector (* name must be a valid Lua identifier *)
  | IfStat of
      Exp
      * Stat vector
      * Stat vector (* 'elseif' will be synthesized by writer *)
  | ReturnStat of Exp vector (* must be the last statement in a block *)
  | DoStat of {loopLike: bool, body: Stat vector}
  | GotoStat of Label
  | LabelStat of Label
  type Block = Stat vector
  val IndexExp: Exp * Exp -> Exp
  val SingleValueExp: Exp -> Exp
  val makeDoStat: {loopLike: bool, body: Stat list} -> Stat list
  val MultiAssignStat: Id list * Exp list -> Stat list
  val predefinedIdsInBlock: Block * StringSet.set -> StringSet.set
  val recursePre: {exp: Exp -> Exp, stat: Stat -> Stat, block: Block -> Block}
                  -> Block
                  -> Block
  val recursePost: {exp: Exp -> Exp, stat: Stat -> Stat, block: Block -> Block}
                   -> Block
                   -> Block
end =
struct
  datatype TableKey = IntKey of int | StringKey of string
  datatype Id = PredefinedId of string | UserDefinedId of TypedSyntax.VId
  type Label = TypedSyntax.VId
  structure IdKey =
  struct
    type ord_key = Id
    fun compare (PredefinedId x, PredefinedId y) = String.compare (x, y)
      | compare (PredefinedId _, UserDefinedId _) = LESS
      | compare (UserDefinedId _, PredefinedId _) = GREATER
      | compare (UserDefinedId x, UserDefinedId y) =
          TypedSyntax.VIdKey.compare (x, y)
  end : ORD_KEY
  structure IdSet = RedBlackSetFn(IdKey)
  structure IdMap = RedBlackMapFn(IdKey)
  structure IdSCC =
    StronglyConnectedComponents
      (type t = Id structure Map = IdMap structure Set = IdSet)
  datatype VarAttr = CONST | LATE_INIT | MUTABLE
  datatype LuaConst =
    Nil
  | False
  | True
  | Numeral of string (* integer, word (hexadecimal), floating-point *)
  | LiteralString of string
  datatype BinaryOp =
    PLUS
  | MINUS
  | TIMES
  | DIV
  | INTDIV
  | POW
  | MOD
  | BITAND
  | BITXOR
  | BITOR
  | RSHIFT
  | LSHIFT
  | CONCAT
  | LT
  | LE
  | GT
  | GE
  | EQUAL
  | NOTEQUAL
  | AND
  | OR
  | INDEX (* a[b] *)
  datatype UnaryOp =
    NEGATE (* -a *)
  | NOT (* not a *)
  | LENGTH (* #a *)
  | BITNOT (* ~a *)
  | SINGLE_VALUE (* (f(...)) *)
  datatype Exp =
    ConstExp of LuaConst
  | VarExp of Id
  | TableExp of (TableKey * Exp) vector
  | CallExp of Exp * Exp vector
  | MethodExp of Exp * string * Exp vector
  | FunctionExp of
      Id vector * Block (* function parameters are implicitly const *)
  | BinExp of BinaryOp * Exp * Exp
  | UnaryExp of UnaryOp * Exp
  and Stat =
    LocalStat of
      (TypedSyntax.VId * VarAttr) list * Exp list (* vars must not be empty *)
  | AssignStat of
      Exp list
      * Exp list (* LHS must be non-empty prefixexps and RHS must not be empty*)
  | CallStat of Exp * Exp vector
  | MethodStat of
      Exp * string * Exp vector (* name must be a valid Lua identifier *)
  | IfStat of Exp * Block * Block (* 'elseif' will be synthesized by writer *)
  | ReturnStat of Exp vector (* must be the last statement in a block *)
  | DoStat of {loopLike: bool, body: Block}
  | GotoStat of Label
  | LabelStat of Label
  withtype Block = Stat vector

  fun IndexExp (x, y) = BinExp (INDEX, x, y)
  fun SingleValueExp x = UnaryExp (SINGLE_VALUE, x)

  fun makeDoStat {loopLike: bool, body: Stat list} =
    if
      List.exists
        (fn LocalStat _ => true
          | ReturnStat _ => true
          | LabelStat _ => true
          | _ => false) body
    then [DoStat {loopLike = loopLike, body = vector body}]
    else body

  fun freeVarsInExp (_: IdSet.set, ConstExp _, acc: IdSet.set) = acc
    | freeVarsInExp (bound, VarExp v, acc) =
        if IdSet.member (bound, v) then acc else IdSet.add (acc, v)
    | freeVarsInExp (bound, TableExp fields, acc) =
        Vector.foldl (fn ((_, x), acc) => freeVarsInExp (bound, x, acc)) acc
          fields
    | freeVarsInExp (bound, CallExp (f, args), acc) =
        Vector.foldl (fn (x, acc) => freeVarsInExp (bound, x, acc))
          (freeVarsInExp (bound, f, acc)) args
    | freeVarsInExp (bound, MethodExp (obj, _, args), acc) =
        Vector.foldl (fn (x, acc) => freeVarsInExp (bound, x, acc))
          (freeVarsInExp (bound, obj, acc)) args
    | freeVarsInExp (bound, FunctionExp (params, body), acc) =
        freeVarsInBlock (Vector.foldl IdSet.add' bound params, body, acc)
    | freeVarsInExp (bound, BinExp (_, x, y), acc) =
        freeVarsInExp (bound, y, freeVarsInExp (bound, x, acc))
    | freeVarsInExp (bound, UnaryExp (_, x), acc) =
        freeVarsInExp (bound, x, acc)
  and freeVarsInStat (LocalStat (lhs, rhs), (bound, acc)) =
        let
          val acc =
            List.foldl (fn (x, acc) => freeVarsInExp (bound, x, acc)) acc rhs
          val bound =
            List.foldl (fn ((v, _), b) => IdSet.add (b, UserDefinedId v)) bound
              lhs
        in
          (bound, acc)
        end
    | freeVarsInStat (AssignStat (lhs, rhs), (bound, acc)) =
        let
          val acc =
            List.foldl (fn (x, acc) => freeVarsInExp (bound, x, acc)) acc rhs
          val acc =
            List.foldl (fn (x, acc) => freeVarsInExp (bound, x, acc)) acc lhs
        in
          (bound, acc)
        end
    | freeVarsInStat (CallStat (f, args), (bound, acc)) =
        let
          val acc = freeVarsInExp (bound, f, acc)
          val acc =
            Vector.foldl (fn (x, acc) => freeVarsInExp (bound, x, acc)) acc args
        in
          (bound, acc)
        end
    | freeVarsInStat (MethodStat (obj, _, args), (bound, acc)) =
        let
          val acc = freeVarsInExp (bound, obj, acc)
          val acc =
            Vector.foldl (fn (x, acc) => freeVarsInExp (bound, x, acc)) acc args
        in
          (bound, acc)
        end
    | freeVarsInStat (IfStat (cond, then_, else_), (bound, acc)) =
        let
          val acc = freeVarsInExp (bound, cond, acc)
          val acc = freeVarsInBlock (bound, then_, acc)
          val acc = freeVarsInBlock (bound, else_, acc)
        in
          (bound, acc)
        end
    | freeVarsInStat (ReturnStat xs, (bound, acc)) =
        let
          val acc =
            Vector.foldl (fn (x, acc) => freeVarsInExp (bound, x, acc)) acc xs
        in
          (bound, acc)
        end
    | freeVarsInStat (DoStat {loopLike = _, body}, (bound, acc)) =
        let val acc = freeVarsInBlock (bound, body, acc)
        in (bound, acc)
        end
    | freeVarsInStat (GotoStat _, bound_acc) = bound_acc
    | freeVarsInStat (LabelStat _, bound_acc) = bound_acc
  and freeVarsInBlock (bound, block, acc) =
    #2 (Vector.foldl freeVarsInStat (bound, acc) block)
  fun MultiAssignStat (vars: Id list, values: Exp list) =
    let
      val idset = List.foldl IdSet.add' IdSet.empty vars
      val graph =
        ListPair.foldlEq
          (fn (v, x, map) =>
             let
               val free = IdSet.intersection
                 (idset, freeVarsInExp (IdSet.empty, x, IdSet.empty))
             in
               IdMap.insert (map, v, {rhsFree = free, rhs = x})
             end) IdMap.empty (vars, values)
      val sccs = List.rev (IdSCC.components (#rhsFree, graph))
    in
      List.map
        (fn set =>
           let
             val vars = IdSet.toList set
           in
             AssignStat
               ( List.map VarExp vars
               , List.map (fn v => #rhs (IdMap.lookup (graph, v))) vars
               )
           end) sccs
    end

  fun predefinedIdsInExp (ConstExp _, acc) = acc
    | predefinedIdsInExp (VarExp (PredefinedId i), acc) = StringSet.add (acc, i)
    | predefinedIdsInExp (VarExp (UserDefinedId _), acc) = acc
    | predefinedIdsInExp (TableExp fields, acc) =
        Vector.foldl (fn ((_, x), acc) => predefinedIdsInExp (x, acc)) acc
          fields
    | predefinedIdsInExp (CallExp (x, ys), acc) =
        Vector.foldl predefinedIdsInExp (predefinedIdsInExp (x, acc)) ys
    | predefinedIdsInExp (MethodExp (x, _, ys), acc) =
        Vector.foldl predefinedIdsInExp (predefinedIdsInExp (x, acc)) ys
    | predefinedIdsInExp (FunctionExp (_, body), acc) =
        predefinedIdsInBlock (body, acc)
    | predefinedIdsInExp (BinExp (_, x, y), acc) =
        predefinedIdsInExp (y, predefinedIdsInExp (x, acc))
    | predefinedIdsInExp (UnaryExp (_, x), acc) = predefinedIdsInExp (x, acc)
  and predefinedIdsInStat (LocalStat (_, xs), acc) =
        List.foldl predefinedIdsInExp acc xs
    | predefinedIdsInStat (AssignStat (xs, ys), acc) =
        List.foldl predefinedIdsInExp (List.foldl predefinedIdsInExp acc xs) ys
    | predefinedIdsInStat (CallStat (x, ys), acc) =
        Vector.foldl predefinedIdsInExp (predefinedIdsInExp (x, acc)) ys
    | predefinedIdsInStat (MethodStat (x, _, ys), acc) =
        Vector.foldl predefinedIdsInExp (predefinedIdsInExp (x, acc)) ys
    | predefinedIdsInStat (IfStat (x, t, e), acc) =
        predefinedIdsInBlock (e, predefinedIdsInBlock
          (t, predefinedIdsInExp (x, acc)))
    | predefinedIdsInStat (ReturnStat xs, acc) =
        Vector.foldl predefinedIdsInExp acc xs
    | predefinedIdsInStat (DoStat {loopLike = _, body}, acc) =
        predefinedIdsInBlock (body, acc)
    | predefinedIdsInStat (GotoStat _, acc) = acc
    | predefinedIdsInStat (LabelStat _, acc) = acc
  and predefinedIdsInBlock (block, acc) =
    Vector.foldl predefinedIdsInStat acc block
  (*: val recursePre : { exp : Exp -> Exp, stat : Stat -> Stat, block : Block -> Block } -> Block -> Block *)
  fun recursePre {exp = doExp, stat = doStat: Stat -> Stat, block = doBlock} =
    let
      fun goExp exp =
        (case doExp exp of
           x as ConstExp _ => x
         | x as VarExp _ => x
         | TableExp fields =>
             TableExp (Vector.map (fn (k, v) => (k, goExp v)) fields)
         | CallExp (x, args) => CallExp (goExp x, Vector.map goExp args)
         | MethodExp (x, name, args) =>
             MethodExp (goExp x, name, Vector.map goExp args)
         | FunctionExp (params, body) => FunctionExp (params, goBlock body)
         | BinExp (p, x, y) => BinExp (p, goExp x, goExp y)
         | UnaryExp (p, x) => UnaryExp (p, goExp x))
      and goStat stat =
        (case doStat stat of
           LocalStat (lhs, rhs) => LocalStat (lhs, List.map goExp rhs)
         | AssignStat (lhs, rhs) =>
             AssignStat (List.map goExp lhs, List.map goExp rhs)
         | CallStat (x, args) => CallStat (goExp x, Vector.map goExp args)
         | MethodStat (x, name, args) =>
             MethodStat (goExp x, name, Vector.map goExp args)
         | IfStat (x, y, z) => IfStat (goExp x, goBlock y, goBlock z)
         | ReturnStat xs => ReturnStat (Vector.map goExp xs)
         | DoStat {loopLike, body} =>
             DoStat {loopLike = loopLike, body = goBlock body}
         | stat as GotoStat _ => stat
         | stat as LabelStat _ => stat)
      and goBlock block =
        Vector.map goStat (doBlock block)
    in
      goBlock
    end
  (*: val recursePost : { exp : Exp -> Exp, stat : Stat -> Stat, block : Block -> Block } -> Block -> Block *)
  fun recursePost {exp = doExp, stat = doStat: Stat -> Stat, block = doBlock} =
    let
      fun goExp exp =
        doExp
          (case exp of
             x as ConstExp _ => x
           | x as VarExp _ => x
           | TableExp fields =>
               TableExp (Vector.map (fn (k, v) => (k, goExp v)) fields)
           | CallExp (x, args) => CallExp (goExp x, Vector.map goExp args)
           | MethodExp (x, name, args) =>
               MethodExp (goExp x, name, Vector.map goExp args)
           | FunctionExp (params, body) => FunctionExp (params, goBlock body)
           | BinExp (p, x, y) => BinExp (p, goExp x, goExp y)
           | UnaryExp (p, x) => UnaryExp (p, goExp x))
      and goStat stat =
        doStat
          (case stat of
             LocalStat (lhs, rhs) => LocalStat (lhs, List.map goExp rhs)
           | AssignStat (lhs, rhs) =>
               AssignStat (List.map goExp lhs, List.map goExp rhs)
           | CallStat (x, args) => CallStat (goExp x, Vector.map goExp args)
           | MethodStat (x, name, args) =>
               MethodStat (goExp x, name, Vector.map goExp args)
           | IfStat (x, y, z) => IfStat (goExp x, goBlock y, goBlock z)
           | ReturnStat xs => ReturnStat (Vector.map goExp xs)
           | DoStat {loopLike, body} =>
               DoStat {loopLike = loopLike, body = goBlock body}
           | stat as GotoStat _ => stat
           | stat as LabelStat _ => stat)
      and goBlock block =
        doBlock (Vector.map goStat block)
    in
      goBlock
    end
end;

structure LuaWriter :>
sig
  val isLuaIdentifier: string -> bool
  val doChunk: LuaSyntax.Stat vector -> string
end =
struct
  fun smlNameToLuaChar #"_" = "__"
    | smlNameToLuaChar #"'" = "_PRIME"
    | smlNameToLuaChar #"!" = "_EXCLAM"
    | smlNameToLuaChar #"%" = "_PERCENT"
    | smlNameToLuaChar #"&" = "_AMPER"
    | smlNameToLuaChar #"$" = "_DOLLAR"
    | smlNameToLuaChar #"#" = "_HASH"
    | smlNameToLuaChar #"+" = "_PLUS"
    | smlNameToLuaChar #"-" = "_MINUS"
    | smlNameToLuaChar #"/" = "_SLASH"
    | smlNameToLuaChar #":" = "_COLON"
    | smlNameToLuaChar #"<" = "_LT"
    | smlNameToLuaChar #"=" = "_EQ"
    | smlNameToLuaChar #">" = "_GT"
    | smlNameToLuaChar #"?" = "_QUESTION"
    | smlNameToLuaChar #"@" = "_AT"
    | smlNameToLuaChar #"\\" = "_BACKSLASH"
    | smlNameToLuaChar #"~" = "_TILDE"
    | smlNameToLuaChar #"`" = "_GRAVE"
    | smlNameToLuaChar #"^" = "_HAT"
    | smlNameToLuaChar #"|" = "_BAR"
    | smlNameToLuaChar #"*" = "_ASTER"
    | smlNameToLuaChar x =
        if Char.isAlphaNum x then String.str x
        else raise Fail "smlNameToLua: invalid character"
  fun smlNameToLua name = String.translate smlNameToLuaChar name
  val LuaKeywords = StringSet.fromList
    [ "and"
    , "break"
    , "do"
    , "else"
    , "elseif"
    , "end"
    , "false"
    , "for"
    , "function"
    , "goto"
    , "if"
    , "in"
    , "local"
    , "nil"
    , "not"
    , "or"
    , "repeat"
    , "return"
    , "then"
    , "true"
    , "until"
    , "while"
    ]
  val LuaReservedNames = StringSet.add (LuaKeywords, "_ENV")
  fun isLuaIdentifier name =
    case CharVectorSlice.getItem (CharVectorSlice.full name) of
      NONE => false
    | SOME (x0, xs) =>
        (Char.isAlpha x0 orelse x0 = #"_")
        andalso
        (CharVectorSlice.all (fn c => Char.isAlphaNum c orelse c = #"_") xs)
        andalso (not (StringSet.member (LuaKeywords, name)))

  local structure L = LuaSyntax
  in
    fun declare
      ( vid as TypedSyntax.MkVId (smlName, _)
      , acc as (unavailableNames, nameMap)
      ) =
      (case TypedSyntax.VIdMap.find (nameMap, vid) of
         SOME _ =>
           raise Fail ("name already declared: " ^ TypedSyntax.print_VId vid)
       | NONE =>
           let
             val baseName =
               if isLuaIdentifier smlName then smlName else smlNameToLua smlName
             val baseName_ =
               if
                 String.size baseName = 0
                 orelse
                 Char.isDigit (String.sub (baseName, String.size baseName - 1))
               then baseName ^ "_"
               else baseName
             fun isAvailable x =
               not (StringSet.member (unavailableNames, x))
             fun go i =
               let val name = baseName_ ^ Int.toString i
               in if isAvailable name then name else go (i + 1)
               end
             val name = if isAvailable baseName then baseName else go 1
           in
             ( StringSet.add (unavailableNames, name)
             , TypedSyntax.VIdMap.insert (nameMap, vid, name)
             )
           end)
    fun declareId (L.PredefinedId _, acc) = acc
      | declareId (L.UserDefinedId vid, acc) = declare (vid, acc)
    (*:
    val createNameMapForExp : StringSet.set -> LuaSyntax.Exp * string TypedSyntax.VIdMap.map -> string TypedSyntax.VIdMap.map
    val createNameMapForStat : LuaSyntax.Stat * (StringSet.set * string TypedSyntax.VIdMap.map) -> StringSet.set * string TypedSyntax.VIdMap.map
     *)
    fun createNameMapForExp _ (L.ConstExp _, nameMap) = nameMap
      | createNameMapForExp _ (L.VarExp _, nameMap) = nameMap
      | createNameMapForExp unavailableNames (L.TableExp elements, nameMap) =
          Vector.foldl
            (fn ((_, v), nameMap) =>
               createNameMapForExp unavailableNames (v, nameMap)) nameMap
            elements
      | createNameMapForExp unavailableNames (L.CallExp (f, args), nameMap) =
          Vector.foldl (createNameMapForExp unavailableNames)
            (createNameMapForExp unavailableNames (f, nameMap)) args
      | createNameMapForExp unavailableNames
          (L.MethodExp (obj, _, args), nameMap) =
          Vector.foldl (createNameMapForExp unavailableNames)
            (createNameMapForExp unavailableNames (obj, nameMap)) args
      | createNameMapForExp unavailableNames
          (L.FunctionExp (params, body), nameMap) =
          let
            val (unavailableNames, nameMap) =
              Vector.foldl declareId (unavailableNames, nameMap) params
          in
            #2
              (Vector.foldl createNameMapForStat (unavailableNames, nameMap)
                 body)
          end
      | createNameMapForExp unavailableNames (L.BinExp (_, x, y), nameMap) =
          createNameMapForExp unavailableNames
            (y, createNameMapForExp unavailableNames (x, nameMap))
      | createNameMapForExp unavailableNames (L.UnaryExp (_, x), nameMap) =
          createNameMapForExp unavailableNames (x, nameMap)
    and createNameMapForStat
          (L.LocalStat (vars, exps), unavailableNamesAndNameMap) =
          let
            val (unavailableNames, nameMap) =
              List.foldl (fn ((vid, _), acc) => declare (vid, acc))
                unavailableNamesAndNameMap vars
            val nameMap =
              List.foldl (createNameMapForExp unavailableNames) nameMap exps
          in
            (unavailableNames, nameMap)
          end
      | createNameMapForStat
          (L.AssignStat (lhs, rhs), (unavailableNames, nameMap)) =
          let
            val nameMap =
              List.foldl (createNameMapForExp unavailableNames) nameMap lhs
          in
            ( unavailableNames
            , List.foldl (createNameMapForExp unavailableNames) nameMap rhs
            )
          end
      | createNameMapForStat (L.CallStat (f, args), (unavailableNames, nameMap)) =
          let
            val nameMap = createNameMapForExp unavailableNames (f, nameMap)
          in
            ( unavailableNames
            , Vector.foldl (createNameMapForExp unavailableNames) nameMap args
            )
          end
      | createNameMapForStat
          (L.MethodStat (obj, _, args), (unavailableNames, nameMap)) =
          let
            val nameMap = createNameMapForExp unavailableNames (obj, nameMap)
          in
            ( unavailableNames
            , Vector.foldl (createNameMapForExp unavailableNames) nameMap args
            )
          end
      | createNameMapForStat
          (L.IfStat (cond, then_, else_), (unavailableNames, nameMap)) =
          let
            val nameMap = createNameMapForExp unavailableNames (cond, nameMap)
            val (_, nameMap) =
              Vector.foldl createNameMapForStat (unavailableNames, nameMap)
                then_
          in
            ( unavailableNames
            , #2
                (Vector.foldl createNameMapForStat (unavailableNames, nameMap)
                   else_)
            )
          end
      | createNameMapForStat (L.ReturnStat values, (unavailableNames, nameMap)) =
          ( unavailableNames
          , Vector.foldl (createNameMapForExp unavailableNames) nameMap values
          )
      | createNameMapForStat
          (L.DoStat {loopLike = _, body}, (unavailableNames, nameMap)) =
          ( unavailableNames
          , #2
              (Vector.foldl createNameMapForStat (unavailableNames, nameMap)
                 body)
          )
      | createNameMapForStat (L.GotoStat _, unavailableNamesAndNameMap) =
          unavailableNamesAndNameMap
      | createNameMapForStat (L.LabelStat _, unavailableNamesAndNameMap) =
          unavailableNamesAndNameMap
    (*:
    val createLabelMapForExp : StringSet.set -> LuaSyntax.Exp * string TypedSyntax.VIdMap.map -> string TypedSyntax.VIdMap.map
    val createLabelMapForStat : StringSet.set -> LuaSyntax.Stat * string TypedSyntax.VIdMap.map -> string TypedSyntax.VIdMap.map
     *)
    fun createLabelMapForExp _ (L.ConstExp _, labelMap) = labelMap
      | createLabelMapForExp _ (L.VarExp (L.PredefinedId _), labelMap) =
          labelMap
      | createLabelMapForExp _ (L.VarExp (L.UserDefinedId _), labelMap) =
          labelMap
      | createLabelMapForExp unavailableLabels (L.TableExp elements, labelMap) =
          Vector.foldl
            (fn ((_, v), labelMap) =>
               createLabelMapForExp unavailableLabels (v, labelMap)) labelMap
            elements
      | createLabelMapForExp unavailableLabels (L.CallExp (f, args), labelMap) =
          Vector.foldl (createLabelMapForExp unavailableLabels)
            (createLabelMapForExp unavailableLabels (f, labelMap)) args
      | createLabelMapForExp unavailableLabels
          (L.MethodExp (obj, _, args), labelMap) =
          Vector.foldl (createLabelMapForExp unavailableLabels)
            (createLabelMapForExp unavailableLabels (obj, labelMap)) args
      | createLabelMapForExp _ (L.FunctionExp (_, body), labelMap) =
          createLabelMapForBlock (LuaKeywords, labelMap, body)
      | createLabelMapForExp unavailableLabels (L.BinExp (_, x, y), labelMap) =
          createLabelMapForExp unavailableLabels
            (y, createLabelMapForExp unavailableLabels (x, labelMap))
      | createLabelMapForExp unavailableLabels (L.UnaryExp (_, x), labelMap) =
          createLabelMapForExp unavailableLabels (x, labelMap)
    and createLabelMapForStat unavailableLabels
          (L.LocalStat (_, exps), labelMap) =
          List.foldl (createLabelMapForExp unavailableLabels) labelMap exps
      | createLabelMapForStat unavailableLabels
          (L.AssignStat (lhs, rhs), labelMap) =
          let
            val labelMap =
              List.foldl (createLabelMapForExp unavailableLabels) labelMap lhs
          in
            List.foldl (createLabelMapForExp unavailableLabels) labelMap rhs
          end
      | createLabelMapForStat unavailableLabels (L.CallStat (f, args), labelMap) =
          let
            val labelMap = createLabelMapForExp unavailableLabels (f, labelMap)
          in
            Vector.foldl (createLabelMapForExp unavailableLabels) labelMap args
          end
      | createLabelMapForStat unavailableLabels
          (L.MethodStat (obj, _, args), labelMap) =
          let
            val labelMap =
              createLabelMapForExp unavailableLabels (obj, labelMap)
          in
            Vector.foldl (createLabelMapForExp unavailableLabels) labelMap args
          end
      | createLabelMapForStat unavailableLabels
          (L.IfStat (cond, then_, else_), labelMap) =
          let
            val labelMap =
              createLabelMapForExp unavailableLabels (cond, labelMap)
            val labelMap =
              createLabelMapForBlock (unavailableLabels, labelMap, then_)
          in
            createLabelMapForBlock (unavailableLabels, labelMap, else_)
          end
      | createLabelMapForStat unavailableLabels (L.ReturnStat values, labelMap) =
          Vector.foldl (createLabelMapForExp unavailableLabels) labelMap values
      | createLabelMapForStat unavailableLabels
          (L.DoStat {loopLike = _, body}, labelMap) =
          createLabelMapForBlock (unavailableLabels, labelMap, body)
      | createLabelMapForStat _ (L.GotoStat _, labelMap) =
          labelMap (* TypedSyntax.VIdMap.inDomain (labelMap, label) should be true *)
      | createLabelMapForStat _ (L.LabelStat _, labelMap) =
          labelMap (* TypedSyntax.VIdMap.inDomain (labelMap, label) should be true *)
    and createLabelMapForBlock (unavailableLabels, labelMap, block) =
      let
        fun go (L.LabelStat label, acc) = declare (label, acc)
          | go (_, acc) = acc
        val (unavailableLabels, labelMap) =
          (Vector.foldl go (unavailableLabels, labelMap) block)
      in
        Vector.foldl (createLabelMapForStat unavailableLabels) labelMap block
      end
  end

  fun IdToLua (_, LuaSyntax.PredefinedId name) = name
    | IdToLua (nameMap, LuaSyntax.UserDefinedId vid) =
        case TypedSyntax.VIdMap.find (nameMap, vid) of
          SOME x => x
        | NONE => raise Fail ("IdToLua " ^ TypedSyntax.print_VId vid)
  (* "UNDEFINED_" ^ smlNameToLua name ^ "_" ^ Int.toString i *)

  fun toLuaStringLit (s: string) =
    "\""
    ^
    String.translate
      (fn #"\\" => "\\\\"
        | #"\a" => "\\a"
        | #"\b" => "\\b"
        | #"\f" => "\\f"
        | #"\n" => "\\n"
        | #"\r" => "\\r"
        | #"\t" => "\\t"
        | #"\v" => "\\v"
        | #"\"" => "\\\""
        | c =>
         if Char.isAscii c andalso Char.isPrint c then
           String.str c
         else
           let
             val x = Char.ord c
           in
             if x > 255 then
               raise Fail
                 ("this string cannot be expressed in Lua: " ^ String.toString s)
             else if x < 0x10 then
               "\\x0" ^ Int.fmt StringCvt.HEX x
             else
               "\\x" ^ Int.fmt StringCvt.HEX x
           end) s ^ "\""

  (* statements: local, assignment, if-then-else-end *)
  (* exp: false, true, Numeral, LiteralString, functiondef, var, functioncall, parens, binop, unop *)
  (* precedence:
   12: or
   11: and
   10: < > <= >= ~= ==
   9: |
   8: ~
   7: &
   6: << >>
   5: .. (right assoc)
   4: + -
   3: * / // %
   2: unary operators (not # - ~)
   1: ^ (right assoc)
   0: literals
   ~1: prefixexp
   ~2: functioncall : allowed to appear as statements
   *)
  (* exp ::= nil | false | true | Numeral | LiteralString | '...' | functiondef |
             prefixexp | tableconstructor | exp binop exp | unop exp
     prefixexp ::= var | functioncall | '(' exp ')'
   *)

  datatype Fragment =
    Fragment of string
  | IncreaseIndent
  | DecreaseIndent
  | Indent
  | OptSemicolon
  | LineTerminator
  fun findNextFragment [] = NONE
    | findNextFragment (Fragment "" :: fragments) = findNextFragment fragments
    | findNextFragment (Fragment s :: _) = SOME s
    | findNextFragment (_ :: fragments) = findNextFragment fragments
  fun processIndent (revAcc, _, []) = List.rev revAcc
    | processIndent (revAcc, indent, Fragment s :: fragments) =
        processIndent (s :: revAcc, indent, fragments)
    | processIndent (revAcc, indent, IncreaseIndent :: fragments) =
        processIndent (revAcc, indent + 2, fragments)
    | processIndent (revAcc, indent, DecreaseIndent :: fragments) =
        processIndent (revAcc, indent - 2, fragments)
    | processIndent (revAcc, indent, Indent :: fragments) =
        processIndent
          ( CharVector.tabulate (indent, fn _ => #" ") :: revAcc
          , indent
          , fragments
          )
    | processIndent (revAcc, indent, OptSemicolon :: fragments) =
        (case findNextFragment fragments of
           NONE => processIndent ("\n" :: revAcc, indent, fragments)
         | SOME next =>
             if String.sub (next, 0) = #"(" then
               processIndent (";\n" :: revAcc, indent, fragments)
             else
               processIndent ("\n" :: revAcc, indent, fragments))
    | processIndent (revAcc, indent, LineTerminator :: fragments) =
        processIndent ("\n" :: revAcc, indent, fragments)
  fun buildProgram fragments =
    String.concat (processIndent ([], 0, fragments))

  fun idToFragment nameMap id =
    [Fragment (IdToLua (nameMap, id))]
  fun vidToFragment nameMap id =
    [Fragment (IdToLua (nameMap, LuaSyntax.UserDefinedId id))]

  type Exp = {prec: int, exp: Fragment list}

  fun paren allowed {prec, exp} =
    if allowed < prec then Fragment "(" :: exp @ [Fragment ")"] else exp

  datatype BinaryOp =
    InfixOp of (* prec *) int * string
  | InfixOpR of (* prec *) int * string
  | IndexOp

  fun binOpInfo LuaSyntax.PLUS = InfixOp (4, "+")
    | binOpInfo LuaSyntax.MINUS = InfixOp (4, "-")
    | binOpInfo LuaSyntax.TIMES = InfixOp (3, "*")
    | binOpInfo LuaSyntax.DIV = InfixOp (3, "/")
    | binOpInfo LuaSyntax.INTDIV = InfixOp (3, "//")
    | binOpInfo LuaSyntax.POW = InfixOpR (1, "^")
    | binOpInfo LuaSyntax.MOD = InfixOp (3, "%")
    | binOpInfo LuaSyntax.BITAND = InfixOp (7, "&")
    | binOpInfo LuaSyntax.BITXOR = InfixOp (8, "~")
    | binOpInfo LuaSyntax.BITOR = InfixOp (9, "|")
    | binOpInfo LuaSyntax.RSHIFT = InfixOp (6, ">>")
    | binOpInfo LuaSyntax.LSHIFT = InfixOp (6, "<<")
    | binOpInfo LuaSyntax.CONCAT = InfixOpR (5, "..")
    | binOpInfo LuaSyntax.LT = InfixOp (10, "<")
    | binOpInfo LuaSyntax.LE = InfixOp (10, "<=")
    | binOpInfo LuaSyntax.GT = InfixOp (10, ">")
    | binOpInfo LuaSyntax.GE = InfixOp (10, ">=")
    | binOpInfo LuaSyntax.EQUAL = InfixOp (10, "==")
    | binOpInfo LuaSyntax.NOTEQUAL = InfixOp (10, "~=")
    | binOpInfo LuaSyntax.AND = InfixOp (11, "and")
    | binOpInfo LuaSyntax.OR = InfixOp (12, "or")
    | binOpInfo LuaSyntax.INDEX = IndexOp

  fun commaSep ([]: (Fragment list) list) : Fragment list = []
    | commaSep (x :: xs) = x @ commaSep1 xs
  and commaSep1 [] = []
    | commaSep1 (x :: xs) =
        Fragment ", " :: x @ commaSep1 xs
  fun commaSepV (v: (Fragment list) vector) : Fragment list =
    (case VectorSlice.getItem (VectorSlice.full v) of
       NONE => []
     | SOME (x, xs) => x @ commaSepV1 xs)
  and commaSepV1 xs =
    (case VectorSlice.getItem xs of
       NONE => []
     | SOME (x, xss) => Fragment ", " :: x @ commaSepV1 xss)

  fun mkWriter (nameMap, labelMap) =
    let
      fun doExp (LuaSyntax.ConstExp ct) : Exp =
            (case ct of
               LuaSyntax.Nil => {prec = 0, exp = [Fragment "nil"]}
             | LuaSyntax.False => {prec = 0, exp = [Fragment "false"]}
             | LuaSyntax.True => {prec = 0, exp = [Fragment "true"]}
             | LuaSyntax.Numeral s =>
                 { prec = 0
                 , exp = [Fragment s]
                 } (* s must not contain negative sign *)
             | LuaSyntax.LiteralString s =>
                 {prec = 0, exp = [Fragment (toLuaStringLit s)]})
        | doExp (LuaSyntax.VarExp id) =
            {prec = ~1, exp = idToFragment nameMap id}
        | doExp (LuaSyntax.TableExp fields) =
            let
              fun doFields (i, slice) =
                case VectorSlice.getItem slice of
                  NONE => []
                | SOME ((LuaSyntax.IntKey n, value), slice') =>
                    if n = i then
                      #exp (doExp value) :: doFields (i + 1, slice')
                    else
                      (Fragment ("[" ^ Int.toString n ^ "] = ")
                       :: #exp (doExp value))
                      :: doFields (i, slice') (* TODO: negative index *)
                | SOME ((LuaSyntax.StringKey key, value), slice') =>
                    if isLuaIdentifier key then
                      (Fragment (key ^ " = ") :: #exp (doExp value))
                      :: doFields (i, slice')
                    else
                      (Fragment ("[" ^ toLuaStringLit key ^ "] = ")
                       :: #exp (doExp value)) :: doFields (i, slice')
            in
              { prec = 0
              , exp =
                  Fragment "{"
                  ::
                  commaSep (doFields (1, VectorSlice.full fields))
                  @ [Fragment "}"]
              }
            end
        | doExp (LuaSyntax.CallExp (fnExp, args)) =
            { prec = ~2
            , exp =
                paren ~1 (doExp fnExp)
                @
                Fragment "("
                :: commaSepV (Vector.map (#exp o doExp) args) @ [Fragment ")"]
            }
        | doExp (LuaSyntax.MethodExp (self, name, args)) =
            { prec = ~2
            , exp =
                paren ~1 (doExp self)
                @
                Fragment (":" ^ name ^ "(")
                :: commaSepV (Vector.map (#exp o doExp) args) @ [Fragment ")"]
            }
        | doExp (LuaSyntax.FunctionExp (args, body)) =
            { prec = 0
            , exp =
                Fragment "function("
                ::
                commaSepV (Vector.map (idToFragment nameMap) args)
                @
                Fragment ")" :: LineTerminator :: IncreaseIndent
                :: doBlock body @ [DecreaseIndent, Indent, Fragment "end"]
            }
        | doExp (LuaSyntax.BinExp (binOp, exp1, exp2raw)) =
            let
              val exp1 = doExp exp1
              val exp2 = doExp exp2raw
            in
              case binOpInfo binOp of
                InfixOp (prec, luaop) =>
                  { prec = prec
                  , exp =
                      paren prec exp1
                      @ Fragment (" " ^ luaop ^ " ") :: paren (prec - 1) exp2
                  }
              | InfixOpR (prec, luaop) =>
                  { prec = prec
                  , exp =
                      paren (prec - 1) exp1
                      @ Fragment (" " ^ luaop ^ " ") :: paren prec exp2
                  }
              | IndexOp =>
                  (case exp2raw of
                     LuaSyntax.ConstExp (LuaSyntax.LiteralString key) =>
                       if isLuaIdentifier key then
                         { prec = ~1
                         , exp = paren ~1 exp1 @ [Fragment ("." ^ key)]
                         }
                       else
                         { prec = ~1
                         , exp =
                             paren ~1 exp1
                             @ Fragment "[" :: #exp exp2 @ [Fragment "]"]
                         }
                   | _ =>
                       { prec = ~1
                       , exp =
                           paren ~1 exp1
                           @ Fragment "[" :: #exp exp2 @ [Fragment "]"]
                       })
            end
        | doExp
            (LuaSyntax.UnaryExp
               (LuaSyntax.NOT, LuaSyntax.BinExp (LuaSyntax.EQUAL, exp1, exp2))) =
            let
              val exp1 = doExp exp1
              val exp2 = doExp exp2
              val prec = 10
            in
              { prec = prec
              , exp = paren prec exp1 @ Fragment " ~= " :: paren (prec - 1) exp2
              }
            end
        | doExp
            (LuaSyntax.UnaryExp
               ( LuaSyntax.NOT
               , LuaSyntax.BinExp (LuaSyntax.NOTEQUAL, exp1, exp2)
               )) =
            let
              val exp1 = doExp exp1
              val exp2 = doExp exp2
              val prec = 10
            in
              { prec = prec
              , exp = paren prec exp1 @ Fragment " == " :: paren (prec - 1) exp2
              }
            end
        | doExp (LuaSyntax.UnaryExp (unOp, exp)) =
            let
              val exp' = doExp exp
              val unOp =
                case unOp of
                  LuaSyntax.NEGATE =>
                    (case exp of
                       LuaSyntax.ConstExp (LuaSyntax.Numeral _) => SOME "-"
                     | _ => SOME "- ")
                | LuaSyntax.NOT => SOME "not "
                | LuaSyntax.LENGTH => SOME "#"
                | LuaSyntax.BITNOT => SOME "~ "
                | LuaSyntax.SINGLE_VALUE => NONE
            in
              case unOp of
                SOME unOp => {prec = 2, exp = Fragment unOp :: paren 2 exp'}
              | NONE =>
                  {prec = ~1, exp = Fragment "(" :: #exp exp' @ [Fragment ")"]}
            end
      and doStat ([], acc) = acc
        | doStat
            ( LuaSyntax.AssignStat
                ( vars as [LuaSyntax.VarExp (LuaSyntax.UserDefinedId name)]
                , exps as [LuaSyntax.FunctionExp (params, body)]
                ) :: (rest' as (LuaSyntax.LocalStat ([(name', _)], []) :: rest))
            , acc
            ) =
            if name = name' then
              (* local f; f = function(...) ... end -> local function f(...) ... end *)
              doStat
                ( rest
                , Indent :: Fragment "local function "
                  ::
                  vidToFragment nameMap name
                  @
                  Fragment "("
                  ::
                  commaSepV (Vector.map (idToFragment nameMap) params)
                  @
                  Fragment ")" :: LineTerminator :: IncreaseIndent
                  ::
                  doBlock body
                  @
                  DecreaseIndent :: Indent :: Fragment "end" :: LineTerminator
                  :: acc
                )
            else
              doStat
                ( rest'
                , Indent
                  ::
                  commaSep (List.map (#exp o doExp) vars)
                  @
                  Fragment " = "
                  ::
                  commaSep (List.map (#exp o doExp) exps) @ OptSemicolon :: acc
                )
        | doStat (LuaSyntax.LocalStat (vars, []) :: rest, acc) =
            doStat
              ( rest
              , Indent :: Fragment "local "
                ::
                commaSep (List.map (vidToFragment nameMap o #1) vars)
                @ LineTerminator :: acc
              )
        | doStat (LuaSyntax.LocalStat (vars, exps) :: rest, acc) =
            doStat
              ( rest
              , Indent :: Fragment "local "
                ::
                commaSep (List.map (vidToFragment nameMap o #1) vars)
                @
                Fragment " = "
                :: commaSep (List.map (#exp o doExp) exps) @ OptSemicolon :: acc
              )
        | doStat (LuaSyntax.AssignStat (vars, exps) :: rest, acc) =
            doStat
              ( rest
              , Indent
                ::
                commaSep (List.map (#exp o doExp) vars)
                @
                Fragment " = "
                :: commaSep (List.map (#exp o doExp) exps) @ OptSemicolon :: acc
              )
        | doStat (LuaSyntax.CallStat (fnExp, args) :: rest, acc) =
            doStat
              ( rest
              , Indent
                ::
                paren ~1 (doExp fnExp)
                @
                Fragment "("
                ::
                commaSepV (Vector.map (#exp o doExp) args)
                @ Fragment ")" :: OptSemicolon :: acc
              )
        | doStat (LuaSyntax.MethodStat (self, name, args) :: rest, acc) =
            doStat
              ( rest
              , Indent
                ::
                paren ~1 (doExp self)
                @
                Fragment (":" ^ name ^ "(")
                ::
                commaSepV (Vector.map (#exp o doExp) args)
                @ Fragment ")" :: OptSemicolon :: acc
              )
        | doStat (LuaSyntax.IfStat (cond, thenPart, elsePart) :: rest, acc) =
            let
              val thenPart' =
                Indent :: Fragment "if "
                ::
                #exp (doExp cond)
                @
                Fragment " then" :: LineTerminator :: IncreaseIndent
                :: doBlock thenPart @ [DecreaseIndent]
              fun doElse elsePart =
                if Vector.length elsePart = 0 then
                  []
                else
                  let
                    val tryElseIf =
                      if Vector.length elsePart = 1 then
                        case Vector.sub (elsePart, 0) of
                          LuaSyntax.IfStat (cond, thenPart, elsePart) =>
                            SOME
                              (Indent :: Fragment "elseif "
                               ::
                               #exp (doExp cond)
                               @
                               Fragment " then" :: LineTerminator
                               :: IncreaseIndent
                               ::
                               doBlock thenPart
                               @ DecreaseIndent :: doElse elsePart)
                        | _ => NONE
                      else
                        NONE
                  in
                    case tryElseIf of
                      SOME elseIf => elseIf
                    | NONE =>
                        Indent :: Fragment "else" :: LineTerminator
                        :: IncreaseIndent :: doBlock elsePart @ [DecreaseIndent]
                  end
            in
              doStat
                ( rest
                , thenPart' @ doElse elsePart
                  @ Indent :: Fragment "end" :: LineTerminator :: acc
                )
            end
        | doStat (LuaSyntax.ReturnStat exps :: rest, acc) =
            if Vector.length exps = 0 then
              doStat
                (rest, Indent :: Fragment "return" :: LineTerminator :: acc)
            else
              doStat
                ( rest
                , Indent :: Fragment "return "
                  ::
                  commaSepV (Vector.map (#exp o doExp) exps)
                  @ OptSemicolon :: acc
                )
        | doStat (LuaSyntax.DoStat {loopLike = _, body} :: rest, acc) =
            doStat
              ( rest
              , Indent :: Fragment "do" :: LineTerminator :: IncreaseIndent
                ::
                doBlock body
                @
                DecreaseIndent :: Indent :: Fragment "end" :: LineTerminator
                :: acc
              )
        | doStat (LuaSyntax.GotoStat label :: rest, acc) =
            doStat
              ( rest
              , Indent :: Fragment "goto "
                :: vidToFragment labelMap label @ LineTerminator :: acc
              )
        | doStat (LuaSyntax.LabelStat label :: rest, acc) =
            doStat
              ( rest
              , Indent :: Fragment "::"
                ::
                vidToFragment labelMap label
                @ Fragment "::" :: LineTerminator :: acc
              )
      and doBlock stats =
        let
          val revStats = Vector.foldl (op::) [] stats
        in
          doStat
            ( revStats
            , []
            ) (* Vector.foldr (fn (stat, xs) => doStat stat @ xs) [] stats *)
        end
    in
      {doBlock = doBlock}
    end

  fun doChunk chunk =
    let
      val unavailableNames =
        LuaSyntax.predefinedIdsInBlock (chunk, LuaReservedNames)
      val (_, nameMap) =
        Vector.foldl createNameMapForStat
          (unavailableNames, TypedSyntax.VIdMap.empty) chunk
      val labelMap =
        createLabelMapForBlock (LuaKeywords, TypedSyntax.VIdMap.empty, chunk)
    in
      buildProgram (#doBlock (mkWriter (nameMap, labelMap)) chunk)
    end
end;
