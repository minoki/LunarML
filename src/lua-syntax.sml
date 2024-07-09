(*
 * Copyright (c) 2023 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure LuaSyntax :>
sig
  datatype TableKey = IntKey of int | StringKey of string
  datatype Id = PredefinedId of string | UserDefinedId of TypedSyntax.VId
  type Label = Id
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
  datatype UnaryOp = NEGATE | NOT | LENGTH | BITNOT
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
  | IndexExp of Exp * Exp
  | SingleValueExp of Exp (* (f(...)) *)
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
  val makeDoStat: {loopLike: bool, body: Stat list} -> Stat list
  val MultiAssignStat: Id list * Exp list -> Stat list
  val predefinedIdsInBlock: Block * StringSet.set -> StringSet.set
end =
struct
  datatype TableKey = IntKey of int | StringKey of string
  datatype Id = PredefinedId of string | UserDefinedId of TypedSyntax.VId
  type Label = Id
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
  datatype UnaryOp = NEGATE | NOT | LENGTH | BITNOT
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
  | IndexExp of Exp * Exp
  | SingleValueExp of Exp (* (f(...)) *)
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
    | freeVarsInExp (bound, IndexExp (x, y), acc) =
        freeVarsInExp (bound, y, freeVarsInExp (bound, x, acc))
    | freeVarsInExp (bound, SingleValueExp x, acc) =
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
    | predefinedIdsInExp (IndexExp (x, y), acc) =
        predefinedIdsInExp (y, predefinedIdsInExp (x, acc))
    | predefinedIdsInExp (SingleValueExp x, acc) = predefinedIdsInExp (x, acc)
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
  fun smlNameToLua (name) = String.translate smlNameToLuaChar name
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
  fun isLuaIdentifier name =
    case CharVectorSlice.getItem (CharVectorSlice.full name) of
      NONE => false
    | SOME (x0, xs) =>
        (Char.isAlpha x0 orelse x0 = #"_")
        andalso
        (CharVectorSlice.all (fn c => Char.isAlphaNum c orelse c = #"_") xs)
        andalso (not (StringSet.member (LuaKeywords, name)))

  fun IdToLua (LuaSyntax.PredefinedId name) = name
    | IdToLua (LuaSyntax.UserDefinedId (TypedSyntax.MkVId (name, n))) =
        smlNameToLua name ^ "_" ^ Int.toString n

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

  fun idToFragment id =
    [Fragment (IdToLua id)]
  fun vidToFragment id =
    [Fragment (IdToLua (LuaSyntax.UserDefinedId id))]

  type Exp = {prec: int, exp: Fragment list}

  fun paren allowed {prec, exp} =
    if allowed < prec then [Fragment "("] @ exp @ [Fragment ")"] else exp

  datatype BinaryOp =
    InfixOp of (* prec *) int * string
  | InfixOpR of (* prec *) int * string

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
    | doExp (LuaSyntax.VarExp id) = {prec = ~1, exp = idToFragment id}
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
              commaSep (doFields (1, VectorSlice.full fields)) @ [Fragment "}"]
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
            commaSepV (Vector.map idToFragment args)
            @
            Fragment ")" :: LineTerminator :: IncreaseIndent
            :: doBlock body @ [DecreaseIndent, Indent, Fragment "end"]
        }
    | doExp (LuaSyntax.BinExp (binOp, exp1, exp2)) =
        let
          val exp1 = doExp exp1
          val exp2 = doExp exp2
        in
          case binOpInfo binOp of
            InfixOp (prec, luaop) =>
              { prec = prec
              , exp =
                  paren prec exp1
                  @ Fragment (" " ^ luaop ^ " ") :: paren (prec + 1) exp2
              }
          | InfixOpR (prec, luaop) =>
              { prec = prec
              , exp =
                  paren (prec + 1) exp1
                  @ Fragment (" " ^ luaop ^ " ") :: paren prec exp2
              }
        end
    | doExp (LuaSyntax.UnaryExp (unOp, exp)) =
        let
          val unOp =
            case unOp of
              LuaSyntax.NEGATE =>
                (case exp of
                   LuaSyntax.ConstExp (LuaSyntax.Numeral _) => "-"
                 | _ => "- ")
            | LuaSyntax.NOT => "not "
            | LuaSyntax.LENGTH => "#"
            | LuaSyntax.BITNOT => "~ "
        in
          {prec = 2, exp = Fragment unOp :: paren 2 (doExp exp)}
        end
    | doExp (LuaSyntax.IndexExp (exp1, exp2)) =
        (case exp2 of
           LuaSyntax.ConstExp (LuaSyntax.LiteralString key) =>
             if isLuaIdentifier key then
               {prec = ~1, exp = paren ~1 (doExp exp1) @ [Fragment ("." ^ key)]}
             else
               { prec = ~1
               , exp =
                   paren ~1 (doExp exp1)
                   @ Fragment "[" :: #exp (doExp exp2) @ [Fragment "]"]
               }
         | _ =>
             { prec = ~1
             , exp =
                 paren ~1 (doExp exp1)
                 @ Fragment "[" :: #exp (doExp exp2) @ [Fragment "]"]
             })
    | doExp (LuaSyntax.SingleValueExp exp) =
        {prec = ~1, exp = Fragment "(" :: #exp (doExp exp) @ [Fragment ")"]}
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
              vidToFragment name
              @
              Fragment "("
              ::
              commaSepV (Vector.map idToFragment params)
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
              :: commaSep (List.map (#exp o doExp) exps) @ OptSemicolon :: acc
            )
    | doStat (LuaSyntax.LocalStat (vars, []) :: rest, acc) =
        doStat
          ( rest
          , Indent :: Fragment "local "
            ::
            commaSep (List.map (vidToFragment o #1) vars)
            @ LineTerminator :: acc
          )
    | doStat (LuaSyntax.LocalStat (vars, exps) :: rest, acc) =
        doStat
          ( rest
          , Indent :: Fragment "local "
            ::
            commaSep (List.map (vidToFragment o #1) vars)
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
                           Fragment " then" :: LineTerminator :: IncreaseIndent
                           ::
                           doBlock thenPart @ DecreaseIndent :: doElse elsePart)
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
          doStat (rest, Indent :: Fragment "return" :: LineTerminator :: acc)
        else
          doStat
            ( rest
            , Indent :: Fragment "return "
              ::
              commaSepV (Vector.map (#exp o doExp) exps) @ OptSemicolon :: acc
            )
    | doStat (LuaSyntax.DoStat {loopLike = _, body} :: rest, acc) =
        doStat
          ( rest
          , Indent :: Fragment "do" :: LineTerminator :: IncreaseIndent
            ::
            doBlock body
            @
            DecreaseIndent :: Indent :: Fragment "end" :: LineTerminator :: acc
          )
    | doStat (LuaSyntax.GotoStat label :: rest, acc) =
        doStat
          ( rest
          , Indent :: Fragment "goto "
            :: idToFragment label @ LineTerminator :: acc
          )
    | doStat (LuaSyntax.LabelStat label :: rest, acc) =
        doStat
          ( rest
          , Indent :: Fragment "::"
            :: idToFragment label @ Fragment "::" :: LineTerminator :: acc
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

  fun doChunk chunk =
    buildProgram (doBlock chunk)
end;
