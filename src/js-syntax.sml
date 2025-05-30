(*
 * Copyright (c) 2023 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure JsSyntax :>
sig
  datatype ObjectKey = IntKey of int | StringKey of string
  datatype Id = PredefinedId of string | UserDefinedId of TypedSyntax.VId
  structure IdSet: ORD_SET where type Key.ord_key = Id
  datatype JsConst =
    Null
  | False
  | True
  | Numeral of string (* integer *)
  | WideString of int vector
  val asciiStringAsIntVector: string -> int vector
  val asciiStringAsWide: string -> JsConst
  datatype BinaryOp =
    PLUS
  | MINUS
  | TIMES
  | DIV
  | MOD
  | BITAND
  | BITXOR
  | BITOR
  | RSHIFT (* >> *)
  | LSHIFT (* << *)
  | URSHIFT (* >>> *)
  | LT
  | LE
  | GT
  | GE
  | EQUAL (* === *)
  | NOTEQUAL (* !== *)
  | LAXEQUAL (* == *)
  | NOTLAXEQUAL (* != *)
  | AND (* && *)
  | OR (* || *)
  | ASSIGN
  | INSTANCEOF
  | IN
  | EXP (* ** *)
  datatype UnaryOp =
    VOID
  | TYPEOF
  | TONUMBER (* unary + *)
  | NEGATE
  | BITNOT
  | NOT
  datatype Exp =
    ConstExp of JsConst
  | ThisExp
  | VarExp of Id
  | ObjectExp of (ObjectKey * Exp) vector
  | ArrayExp of Exp vector
  | CallExp of Exp * Exp vector
  | MethodExp of
      Exp * string * Exp vector (* method name must be ASCII Identifier *)
  | NewExp of Exp * Exp vector
  | FunctionExp of Id vector * Stat vector
  | BinExp of BinaryOp * Exp * Exp
  | UnaryExp of UnaryOp * Exp
  | IndexExp of Exp * Exp
  | CondExp of Exp * Exp * Exp (* exp1 ? exp2 : exp3 *)
  and Stat =
    LetStat of (TypedSyntax.VId * Exp option) vector (* must not be empty *)
  | ConstStat of (TypedSyntax.VId * Exp) vector (* must not be empty *)
  | ExpStat of Exp
  | IfStat of Exp * Stat vector * Stat vector
  | ReturnStat of Exp option
  | TryCatchStat of Stat vector * TypedSyntax.VId * Stat vector
  | ThrowStat of Exp
  | BlockStat of Id option * Stat vector
  | LoopStat of Id option * Stat vector (* label: for(;;) { body } *)
  | SwitchStat of
      Exp
      * (JsConst * Stat vector) list (* switch (e) { case c0: { ... } case c1: { ... } } *)
  | BreakStat of Id option
  | ContinueStat of Id option
  | DefaultExportStat of Exp (* export default <expression> *)
  | NamedExportStat of
      (Id * string) vector (* export { x1 as name1, x2 as name2, ... } *)
  type Block = Stat vector
  val UndefinedExp: Exp
  val ToInt32Exp: Exp -> Exp
  val ToUint32Exp: Exp -> Exp
  val AssignStat: Exp * Exp -> Stat
  val MultiAssignStat: Exp list * Exp list -> Stat list
  val predefinedIdsInBlock: Block * StringSet.set -> StringSet.set
end =
struct
  datatype ObjectKey = IntKey of int | StringKey of string
  datatype Id = PredefinedId of string | UserDefinedId of TypedSyntax.VId
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
  (* structure IdMap = RedBlackMapFn (IdKey) *)
  datatype JsConst =
    Null
  | False
  | True
  | Numeral of string (* integer *)
  | WideString of int vector
  fun asciiStringAsIntVector s =
    Vector.map Char.ord (Vector.fromList (String.explode s))
  fun asciiStringAsWide s =
    WideString (asciiStringAsIntVector s)
  datatype BinaryOp =
    PLUS
  | MINUS
  | TIMES
  | DIV
  | MOD
  | BITAND
  | BITXOR
  | BITOR
  | RSHIFT (* >> *)
  | LSHIFT (* << *)
  | URSHIFT (* >>> *)
  | LT
  | LE
  | GT
  | GE
  | EQUAL (* === *)
  | NOTEQUAL (* !== *)
  | LAXEQUAL (* == *)
  | NOTLAXEQUAL (* != *)
  | AND (* && *)
  | OR (* || *)
  | ASSIGN
  | INSTANCEOF
  | IN
  | EXP (* ** *)
  datatype UnaryOp =
    VOID
  | TYPEOF
  | TONUMBER (* unary + *)
  | NEGATE
  | BITNOT
  | NOT
  datatype Exp =
    ConstExp of JsConst
  | ThisExp
  | VarExp of Id
  | ObjectExp of (ObjectKey * Exp) vector
  | ArrayExp of Exp vector
  | CallExp of Exp * Exp vector
  | MethodExp of
      Exp * string * Exp vector (* method name must be ASCII Identifier *)
  | NewExp of Exp * Exp vector
  | FunctionExp of Id vector * Block
  | BinExp of BinaryOp * Exp * Exp
  | UnaryExp of UnaryOp * Exp
  | IndexExp of Exp * Exp
  | CondExp of Exp * Exp * Exp (* exp1 ? exp2 : exp3 *)
  and Stat =
    LetStat of (TypedSyntax.VId * Exp option) vector (* must not be empty *)
  | ConstStat of (TypedSyntax.VId * Exp) vector (* must not be empty *)
  | ExpStat of Exp
  | IfStat of Exp * Block * Block
  | ReturnStat of Exp option
  | TryCatchStat of Block * TypedSyntax.VId * Block
  | ThrowStat of Exp
  | BlockStat of Id option * Block
  | LoopStat of Id option * Block (* label: for(;;) { body } *)
  | SwitchStat of
      Exp
      * (JsConst * Block) list (* switch (e) { case c0: { ... } case c1: { ... } } *)
  | BreakStat of Id option
  | ContinueStat of Id option
  | DefaultExportStat of Exp (* export default <expression> *)
  | NamedExportStat of
      (Id * string) vector (* export { x1 as name1, x2 as name2, ... } *)
  withtype Block = Stat vector

  val UndefinedExp = VarExp (PredefinedId "undefined")
  fun ToInt32Exp exp =
    BinExp (BITOR, exp, ConstExp (Numeral "0"))
  fun ToUint32Exp exp =
    BinExp (URSHIFT, exp, ConstExp (Numeral "0"))
  fun AssignStat (lhs, rhs) =
    ExpStat (BinExp (ASSIGN, lhs, rhs))
  fun MultiAssignStat ([], []) = []
    | MultiAssignStat ([lhs], [rhs]) =
        [AssignStat (lhs, rhs)]
    | MultiAssignStat (lhs, rhs) =
        [ExpStat (BinExp (ASSIGN, ArrayExp (vector lhs), ArrayExp (vector rhs)))]

  fun predefinedIdsInExp (ConstExp _, acc) = acc
    | predefinedIdsInExp (ThisExp, acc) = acc
    | predefinedIdsInExp (VarExp (PredefinedId i), acc) = StringSet.add (acc, i)
    | predefinedIdsInExp (VarExp (UserDefinedId _), acc) = acc
    | predefinedIdsInExp (ObjectExp fields, acc) =
        Vector.foldl (fn ((_, x), acc) => predefinedIdsInExp (x, acc)) acc
          fields
    | predefinedIdsInExp (ArrayExp elems, acc) =
        Vector.foldl predefinedIdsInExp acc elems
    | predefinedIdsInExp (CallExp (x, ys), acc) =
        Vector.foldl predefinedIdsInExp (predefinedIdsInExp (x, acc)) ys
    | predefinedIdsInExp (MethodExp (x, _, ys), acc) =
        Vector.foldl predefinedIdsInExp (predefinedIdsInExp (x, acc)) ys
    | predefinedIdsInExp (NewExp (x, ys), acc) =
        Vector.foldl predefinedIdsInExp (predefinedIdsInExp (x, acc)) ys
    | predefinedIdsInExp (FunctionExp (_, body), acc) =
        predefinedIdsInBlock (body, acc)
    | predefinedIdsInExp (BinExp (_, x, y), acc) =
        predefinedIdsInExp (y, predefinedIdsInExp (x, acc))
    | predefinedIdsInExp (UnaryExp (_, x), acc) = predefinedIdsInExp (x, acc)
    | predefinedIdsInExp (IndexExp (x, y), acc) =
        predefinedIdsInExp (y, predefinedIdsInExp (x, acc))
    | predefinedIdsInExp (CondExp (x, y, z), acc) =
        predefinedIdsInExp (z, predefinedIdsInExp
          (y, predefinedIdsInExp (x, acc)))
  and predefinedIdsInStat (LetStat xs, acc) =
        Vector.foldl
          (fn ((_, SOME x), acc) => predefinedIdsInExp (x, acc)
            | ((_, NONE), acc) => acc) acc xs
    | predefinedIdsInStat (ConstStat xs, acc) =
        Vector.foldl (fn ((_, x), acc) => predefinedIdsInExp (x, acc)) acc xs
    | predefinedIdsInStat (ExpStat x, acc) = predefinedIdsInExp (x, acc)
    | predefinedIdsInStat (IfStat (x, t, e), acc) =
        predefinedIdsInBlock (e, predefinedIdsInBlock
          (t, predefinedIdsInExp (x, acc)))
    | predefinedIdsInStat (ReturnStat NONE, acc) = acc
    | predefinedIdsInStat (ReturnStat (SOME x), acc) =
        predefinedIdsInExp (x, acc)
    | predefinedIdsInStat (TryCatchStat (body, _, catch), acc) =
        predefinedIdsInBlock (catch, predefinedIdsInBlock (body, acc))
    | predefinedIdsInStat (ThrowStat x, acc) = predefinedIdsInExp (x, acc)
    | predefinedIdsInStat (BlockStat (_, body), acc) =
        predefinedIdsInBlock (body, acc)
    | predefinedIdsInStat (LoopStat (_, body), acc) =
        predefinedIdsInBlock (body, acc)
    | predefinedIdsInStat (SwitchStat (x, clauses), acc) =
        List.foldl (fn ((_, block), acc) => predefinedIdsInBlock (block, acc))
          (predefinedIdsInExp (x, acc)) clauses
    | predefinedIdsInStat (BreakStat _, acc) = acc
    | predefinedIdsInStat (ContinueStat _, acc) = acc
    | predefinedIdsInStat (DefaultExportStat x, acc) =
        predefinedIdsInExp (x, acc)
    | predefinedIdsInStat (NamedExportStat xs, acc) =
        Vector.foldl
          (fn ((PredefinedId i, _), acc) => StringSet.add (acc, i)
            | ((UserDefinedId _, _), acc) => acc) acc xs
  and predefinedIdsInBlock (block, acc) =
    Vector.foldl predefinedIdsInStat acc block
end;

structure JsWriter :>
sig
  val JsUnavailableNames: StringSet.set
  val createNameMapForImports:
    StringSet.set
    * string TypedSyntax.VIdMap.map
    * {specs: (string * JsSyntax.Id) list, moduleName: string} list
    -> StringSet.set * string TypedSyntax.VIdMap.map
  val createNameMapForBlock:
    StringSet.set * string TypedSyntax.VIdMap.map * JsSyntax.Stat vector
    -> string TypedSyntax.VIdMap.map
  val createLabelMapForBlock:
    StringSet.set * string TypedSyntax.VIdMap.map * JsSyntax.Stat vector
    -> string TypedSyntax.VIdMap.map
  (* val doProgram: JsSyntax.Block -> string *)
  val mkWriter: string TypedSyntax.VIdMap.map * string TypedSyntax.VIdMap.map
                -> {doProgram: JsSyntax.Block -> string}
  val doImports:
    string TypedSyntax.VIdMap.map
    * {specs: (string * JsSyntax.Id) list, moduleName: string} list
    -> string
end =
struct
  structure S = JsSyntax

  fun smlNameToJsChar #"_" = "_"
    | smlNameToJsChar #"'" = "$PRIME"
    | smlNameToJsChar #"!" = "$EXCLAM"
    | smlNameToJsChar #"%" = "$PERCENT"
    | smlNameToJsChar #"&" = "$AMPER"
    | smlNameToJsChar #"$" = "$DOLLAR"
    | smlNameToJsChar #"#" = "$HASH"
    | smlNameToJsChar #"+" = "$PLUS"
    | smlNameToJsChar #"-" = "$MINUS"
    | smlNameToJsChar #"/" = "$SLASH"
    | smlNameToJsChar #":" = "$COLON"
    | smlNameToJsChar #"<" = "$LT"
    | smlNameToJsChar #"=" = "$EQ"
    | smlNameToJsChar #">" = "$GT"
    | smlNameToJsChar #"?" = "$QUESTION"
    | smlNameToJsChar #"@" = "$AT"
    | smlNameToJsChar #"\\" = "$BACKSLASH"
    | smlNameToJsChar #"~" = "$TILDE"
    | smlNameToJsChar #"`" = "$GRAVE"
    | smlNameToJsChar #"^" = "$HAT"
    | smlNameToJsChar #"|" = "$BAR"
    | smlNameToJsChar #"*" = "$ASTER"
    | smlNameToJsChar x =
        if Char.isAlphaNum x then String.str x
        else raise Fail "smlNameToJs: invalid character"
  fun smlNameToJs name = String.translate smlNameToJsChar name
  val JsReservedWords = StringSet.fromList
    [ "await"
    , "break"
    , "case"
    , "catch"
    , "class"
    , "const"
    , "continue"
    , "debugger"
    , "default"
    , "delete"
    , "do"
    , "else"
    , "enum"
    , "export"
    , "extends"
    , "false"
    , "finally"
    , "for"
    , "function"
    , "if"
    , "import"
    , "in"
    , "instanceof"
    , "new"
    , "null"
    , "return"
    , "super"
    , "switch"
    , "this"
    , "throw"
    , "true"
    , "try"
    , "typeof"
    , "var"
    , "void"
    , "while"
    , "with"
    , "yield"
    , (* disallowed in strict mode *) "let"
    , "static"
    , "implements"
    , "interface"
    , "package"
    , "private"
    , "protected"
    , "public"
    (* forbidden in certain positions *)
    (* "as", "async", "from", "get", "of", "set", "target" *)
    ]
  val JsUnavailableNames =
    List.foldl StringSet.add' JsReservedWords ["arguments", "eval"]

  fun isIdentifierName name =
    case CharVectorSlice.getItem (CharVectorSlice.full name) of
      NONE => false
    | SOME (x0, xs) =>
        (Char.isAlpha x0 orelse x0 = #"_" orelse x0 = #"$")
        andalso
        CharVectorSlice.all
          (fn c => Char.isAlphaNum c orelse c = #"_" orelse c = #"$") xs
  (* non-ASCII characters are not handled *)
  fun isIdentifier name =
    isIdentifierName name andalso not (StringSet.member (JsReservedWords, name))

  fun declare
    (vid as TypedSyntax.MkVId (smlName, _), acc as (unavailableNames, nameMap)) =
    (case TypedSyntax.VIdMap.find (nameMap, vid) of
       SOME _ =>
         raise Fail ("name already declared: " ^ TypedSyntax.print_VId vid)
     | NONE =>
         let
           val baseName =
             if isIdentifierName smlName then smlName else smlNameToJs smlName
           val baseName_ =
             if
               String.size baseName = 0
               orelse
               Char.isDigit (String.sub (baseName, String.size baseName - 1))
             then baseName ^ "$"
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
  fun declareId (S.PredefinedId _, acc) = acc
    | declareId (S.UserDefinedId vid, acc) = declare (vid, acc)
  (*:
  val createNameMapForImports : StringSet.set * string TypedSyntax.VIdMap.map * {specs: (string * JsSyntax.Id) list, moduleName: string} list -> StringSet.set * string TypedSyntax.VIdMap.map
   *)
  fun createNameMapForImports (unavailableNames, nameMap, imports) =
    let
      fun go ({specs, moduleName = _: string}, acc) =
        List.foldl (fn ((_: string, id), acc) => declareId (id, acc)) acc specs
    in
      List.foldl go (unavailableNames, nameMap) imports
    end
  (*:
  val createNameMapForExp : StringSet.set -> JsSyntax.Exp * string TypedSyntax.VIdMap.map -> string TypedSyntax.VIdMap.map
  val createNameMapForStat : StringSet.set -> JsSyntax.Stat * string TypedSyntax.VIdMap.map -> string TypedSyntax.VIdMap.map
  val createNameMapForBlock : StringSet.set * string TypedSyntax.VIdMap.map * JsSyntax.Stat vector -> string TypedSyntax.VIdMap.map
   *)
  fun createNameMapForExp _ (S.ConstExp _, nameMap) = nameMap
    | createNameMapForExp _ (S.ThisExp, nameMap) = nameMap
    | createNameMapForExp _ (S.VarExp _, nameMap) = nameMap
    | createNameMapForExp unavailableNames (S.ObjectExp fields, nameMap) =
        Vector.foldl
          (fn ((_, v), nameMap) =>
             createNameMapForExp unavailableNames (v, nameMap)) nameMap fields
    | createNameMapForExp unavailableNames (S.ArrayExp elements, nameMap) =
        Vector.foldl (createNameMapForExp unavailableNames) nameMap elements
    | createNameMapForExp unavailableNames (S.CallExp (f, args), nameMap) =
        Vector.foldl (createNameMapForExp unavailableNames)
          (createNameMapForExp unavailableNames (f, nameMap)) args
    | createNameMapForExp unavailableNames (S.MethodExp (obj, _, args), nameMap) =
        Vector.foldl (createNameMapForExp unavailableNames)
          (createNameMapForExp unavailableNames (obj, nameMap)) args
    | createNameMapForExp unavailableNames (S.NewExp (f, args), nameMap) =
        Vector.foldl (createNameMapForExp unavailableNames)
          (createNameMapForExp unavailableNames (f, nameMap)) args
    | createNameMapForExp unavailableNames
        (S.FunctionExp (params, body), nameMap) =
        let
          val (unavailableNames, nameMap) =
            Vector.foldl declareId (unavailableNames, nameMap) params
        in
          createNameMapForBlock (unavailableNames, nameMap, body)
        end
    | createNameMapForExp unavailableNames (S.BinExp (_, x, y), nameMap) =
        createNameMapForExp unavailableNames
          (y, createNameMapForExp unavailableNames (x, nameMap))
    | createNameMapForExp unavailableNames (S.UnaryExp (_, x), nameMap) =
        createNameMapForExp unavailableNames (x, nameMap)
    | createNameMapForExp unavailableNames (S.IndexExp (x, y), nameMap) =
        createNameMapForExp unavailableNames
          (y, createNameMapForExp unavailableNames (x, nameMap))
    | createNameMapForExp unavailableNames (S.CondExp (x, y, z), nameMap) =
        createNameMapForExp unavailableNames
          ( z
          , createNameMapForExp unavailableNames
              (y, createNameMapForExp unavailableNames (x, nameMap))
          )
  and createNameMapForStat unavailableNames (S.LetStat decs, nameMap) =
        Vector.foldl
          (fn ((vid, NONE), acc) => acc
            | ((_, SOME exp), nameMap) =>
             createNameMapForExp unavailableNames (exp, nameMap)) nameMap decs
    | createNameMapForStat unavailableNames (S.ConstStat decs, nameMap) =
        Vector.foldl
          (fn ((_, exp), nameMap) =>
             createNameMapForExp unavailableNames (exp, nameMap)) nameMap decs
    | createNameMapForStat unavailableNames (S.ExpStat exp, nameMap) =
        createNameMapForExp unavailableNames (exp, nameMap)
    | createNameMapForStat unavailableNames
        (S.IfStat (cond, then_, else_), nameMap) =
        let
          val nameMap = createNameMapForExp unavailableNames (cond, nameMap)
          val nameMap = createNameMapForBlock (unavailableNames, nameMap, then_)
        in
          createNameMapForBlock (unavailableNames, nameMap, else_)
        end
    | createNameMapForStat unavailableNames (S.ReturnStat exp, nameMap) =
        (case exp of
           SOME exp => createNameMapForExp unavailableNames (exp, nameMap)
         | NONE => nameMap)
    | createNameMapForStat unavailableNames
        (S.TryCatchStat (try, vid, catch), nameMap) =
        let
          val nameMap = createNameMapForBlock (unavailableNames, nameMap, try)
          val (unavailableNames', nameMap) =
            declare (vid, (unavailableNames, nameMap))
          val nameMap =
            createNameMapForBlock (unavailableNames', nameMap, catch)
        in
          nameMap
        end
    | createNameMapForStat unavailableNames (S.ThrowStat exp, nameMap) =
        createNameMapForExp unavailableNames (exp, nameMap)
    | createNameMapForStat unavailableNames (S.BlockStat (_, body), nameMap) =
        createNameMapForBlock (unavailableNames, nameMap, body)
    | createNameMapForStat unavailableNames (S.LoopStat (_, body), nameMap) =
        createNameMapForBlock (unavailableNames, nameMap, body)
    | createNameMapForStat unavailableNames (S.SwitchStat (exp, cases), nameMap) =
        let
          val nameMap = createNameMapForExp unavailableNames (exp, nameMap)
          fun go ((_, block), nameMap) =
            createNameMapForBlock (unavailableNames, nameMap, block)
          val nameMap = List.foldl go nameMap cases
        in
          nameMap
        end
    | createNameMapForStat _ (S.BreakStat _, nameMap) = nameMap
    | createNameMapForStat _ (S.ContinueStat _, nameMap) = nameMap
    | createNameMapForStat unavailableNames (S.DefaultExportStat exp, nameMap) =
        createNameMapForExp unavailableNames (exp, nameMap)
    | createNameMapForStat _ (S.NamedExportStat _, nameMap) = nameMap
  and createNameMapForBlock (unavailableNames, nameMap, block) =
    let
      fun go (S.LetStat decs, acc) =
            Vector.foldl (fn ((vid, _), acc) => declare (vid, acc)) acc decs
        | go (S.ConstStat decs, acc) =
            Vector.foldl (fn ((vid, _), acc) => declare (vid, acc)) acc decs
        | go (_, acc) = acc
      val (unavailableNames, nameMap) =
        Vector.foldl go (unavailableNames, nameMap) block
    in
      Vector.foldl (createNameMapForStat unavailableNames) nameMap block
    end
  (*:
  val createLabelMapForExp : JsSyntax.Exp * string TypedSyntax.VIdMap.map -> string TypedSyntax.VIdMap.map
  val createLabelMapForStat : StringSet.set -> JsSyntax.Stat * string TypedSyntax.VIdMap.map -> string TypedSyntax.VIdMap.map
  val createLabelMapForBlock : StringSet.set * string TypedSyntax.VIdMap.map * JsSyntax.Stat vector -> string TypedSyntax.VIdMap.map
   *)
  fun createLabelMapForExp (S.ConstExp _, labelMap) = labelMap
    | createLabelMapForExp (S.ThisExp, labelMap) = labelMap
    | createLabelMapForExp (S.VarExp _, labelMap) = labelMap
    | createLabelMapForExp (S.ObjectExp fields, labelMap) =
        Vector.foldl
          (fn ((_, v), labelMap) => createLabelMapForExp (v, labelMap)) labelMap
          fields
    | createLabelMapForExp (S.ArrayExp elements, labelMap) =
        Vector.foldl (createLabelMapForExp) labelMap elements
    | createLabelMapForExp (S.CallExp (f, args), labelMap) =
        Vector.foldl (createLabelMapForExp) (createLabelMapForExp (f, labelMap))
          args
    | createLabelMapForExp (S.MethodExp (obj, _, args), labelMap) =
        Vector.foldl (createLabelMapForExp)
          (createLabelMapForExp (obj, labelMap)) args
    | createLabelMapForExp (S.NewExp (f, args), labelMap) =
        Vector.foldl (createLabelMapForExp) (createLabelMapForExp (f, labelMap))
          args
    | createLabelMapForExp (S.FunctionExp (_, body), labelMap) =
        createLabelMapForBlock (JsUnavailableNames, labelMap, body)
    | createLabelMapForExp (S.BinExp (_, x, y), labelMap) =
        createLabelMapForExp (y, createLabelMapForExp (x, labelMap))
    | createLabelMapForExp (S.UnaryExp (_, x), labelMap) =
        createLabelMapForExp (x, labelMap)
    | createLabelMapForExp (S.IndexExp (x, y), labelMap) =
        createLabelMapForExp (y, createLabelMapForExp (x, labelMap))
    | createLabelMapForExp (S.CondExp (x, y, z), labelMap) =
        createLabelMapForExp (z, createLabelMapForExp
          (y, createLabelMapForExp (x, labelMap)))
  and createLabelMapForStat _ (S.LetStat decs, labelMap) =
        Vector.foldl
          (fn ((vid, NONE), acc) => acc
            | ((_, SOME exp), labelMap) => createLabelMapForExp (exp, labelMap))
          labelMap decs
    | createLabelMapForStat _ (S.ConstStat decs, labelMap) =
        Vector.foldl
          (fn ((_, exp), labelMap) => createLabelMapForExp (exp, labelMap))
          labelMap decs
    | createLabelMapForStat _ (S.ExpStat exp, labelMap) =
        createLabelMapForExp (exp, labelMap)
    | createLabelMapForStat unavailableLabels
        (S.IfStat (cond, then_, else_), labelMap) =
        let
          val labelMap = createLabelMapForExp (cond, labelMap)
          val labelMap =
            createLabelMapForBlock (unavailableLabels, labelMap, then_)
        in
          createLabelMapForBlock (unavailableLabels, labelMap, else_)
        end
    | createLabelMapForStat _ (S.ReturnStat exp, labelMap) =
        (case exp of
           SOME exp => createLabelMapForExp (exp, labelMap)
         | NONE => labelMap)
    | createLabelMapForStat unavailableLabels
        (S.TryCatchStat (try, _, catch), labelMap) =
        let
          val labelMap =
            createLabelMapForBlock (unavailableLabels, labelMap, try)
          val labelMap =
            createLabelMapForBlock (unavailableLabels, labelMap, catch)
        in
          labelMap
        end
    | createLabelMapForStat _ (S.ThrowStat exp, labelMap) =
        createLabelMapForExp (exp, labelMap)
    | createLabelMapForStat unavailableLabels
        (S.BlockStat (label, body), labelMap) =
        (case label of
           SOME label =>
             let
               val (unavailableLabels, labelMap) =
                 declareId (label, (unavailableLabels, labelMap))
             in
               createLabelMapForBlock (unavailableLabels, labelMap, body)
             end
         | NONE => createLabelMapForBlock (unavailableLabels, labelMap, body))
    | createLabelMapForStat unavailableLabels
        (S.LoopStat (label, body), labelMap) =
        (case label of
           SOME label =>
             let
               val (unavailableLabels, labelMap) =
                 declareId (label, (unavailableLabels, labelMap))
             in
               createLabelMapForBlock (unavailableLabels, labelMap, body)
             end
         | NONE => createLabelMapForBlock (unavailableLabels, labelMap, body))
    | createLabelMapForStat unavailableLabels
        (S.SwitchStat (exp, cases), labelMap) =
        let
          val labelMap = createLabelMapForExp (exp, labelMap)
          fun go ((_, block), labelMap) =
            createLabelMapForBlock (unavailableLabels, labelMap, block)
          val labelMap = List.foldl go labelMap cases
        in
          labelMap
        end
    | createLabelMapForStat _ (S.BreakStat _, labelMap) = labelMap
    | createLabelMapForStat _ (S.ContinueStat _, labelMap) = labelMap
    | createLabelMapForStat _ (S.DefaultExportStat exp, labelMap) =
        createLabelMapForExp (exp, labelMap)
    | createLabelMapForStat _ (S.NamedExportStat _, labelMap) = labelMap
  and createLabelMapForBlock (unavailableLabels, labelMap, block) =
    Vector.foldl (createLabelMapForStat unavailableLabels) labelMap block

  (*
  fun idToJs (JsSyntax.PredefinedId name) = name
    | idToJs (JsSyntax.UserDefinedId (TypedSyntax.MkVId (name, n))) =
        smlNameToJs name ^ "$"
        ^ Int.toString n (* the number must be non-negative *)
  *)
  fun idToJs (_, JsSyntax.PredefinedId name) = name
    | idToJs (nameMap, JsSyntax.UserDefinedId vid) =
        case TypedSyntax.VIdMap.find (nameMap, vid) of
          SOME x => x
        | NONE => raise Fail ("idToJs " ^ TypedSyntax.print_VId vid)

  fun toStringLit (s: string) =
    "\""
    ^
    String.translate
      (fn #"\\" => "\\\\"
        | #"\b" => "\\b" (* U+0008 *)
        | #"\f" => "\\f" (* U+000C *)
        | #"\n" => "\\n" (* U+000A *)
        | #"\r" => "\\r" (* U+000D *)
        | #"\t" => "\\t" (* U+0009 *)
        | #"\v" => "\\v" (* U+000B *)
        | #"\"" => "\\\""
        | c =>
         if Char.isAscii c andalso Char.isPrint c then
           String.str c
         else
           let
             val x = Char.ord c
             val t = Int.fmt StringCvt.HEX x
           in
             if x < 0x10 then "\\x0" ^ t else "\\x" ^ t
           end) s ^ "\""
  local
    fun doChar 0x5C = "\\\\"
      | doChar 0x08 = "\\b" (* U+0008 *)
      | doChar 0x0C = "\\f" (* U+000C *)
      | doChar 0x0A = "\\n" (* U+000A *)
      | doChar 0x0D = "\\r" (* U+000D *)
      | doChar 0x09 = "\\t" (* U+0009 *)
      | doChar 0x0B = "\\v" (* U+000B *)
      | doChar 0x22 = "\\\""
      | doChar i =
          if i < 0 orelse 0x10000 <= i then
            raise Fail "this string cannot be expressed in JavaScript"
          else if i < 128 andalso Char.isPrint (Char.chr i) then
            String.str (Char.chr i)
          else
            let
              val t = Int.fmt StringCvt.HEX i
            in
              if i < 0x10 then "\\x0" ^ t
              else if i < 0x100 then "\\x" ^ t
              else if i < 0x1000 then "\\u0" ^ t
              else "\\u" ^ t
            end
  in
    fun toWideStringLit (s: int vector) =
      "\"" ^ Vector.foldr (fn (x, acc) => doChar x ^ acc) "\"" s
  end

  (* precedence:
   * 18: Expression (comma; left-assoc)
   * 17: AssignmentExpression (= *= /= %= += -= <<= >>= >>>= &= ^= |=, right-assoc)
   * 16: ConditionalExpression
   * 15: LogicalORExpression (||; left-assoc)
   * 14: LogicalANDExpression (&&; left-assoc)
   * 13: BitwiseORExpression (|; left-assoc)
   * 12: BitwiseXORExpression (^; left-assoc)
   * 11: BitwiseANDExpression (&; left-assoc)
   * 10: EqualityExpression (== != === !==; left-assoc)
   * 9: RelationalExpression (< > <= >= instanceof in; left-assoc)
   * 8: ShiftExpression (<< >> >>>; left-assoc)
   * 7: AdditiveExpression ('+', '-'; left-assoc)
   * 6: MultiplicativeExpression ('*', '/', '%'; left-assoc)
   * 5: ExponentiationExpression ('**'; right-assoc)
   * 4: UnaryExpression
   * 3: PostfixExpression / UpdateExpression
   * 2: CallExpression (LeftHandSideExpression)
   * 1: MemberExpression
   * 0: PrimaryExpression: this | Identifier | Literal | ArrayLiteral | ObjectLiteral | FunctionExpression | '(' Expression ')'
   *)

  structure Precedence =
  struct
    val Expression = 18
    val AssignmentExpression = 17
    val ConditionalExpression = 16
    val LogicalORExpression = 15
    val UnaryExpression = 4
    val CallExpression = 2
    val MemberExpression = 1
  (* val PrimaryExpression = 0 *)
  end

  datatype Fragment =
    Fragment of string
  | IncreaseIndent
  | DecreaseIndent
  | Indent
  | LineTerminator
  fun findNextFragment [] = NONE
    | findNextFragment (Fragment "" :: fragments) = findNextFragment fragments
    | findNextFragment (Fragment s :: _) = SOME s
    | findNextFragment (_ :: fragments) = findNextFragment fragments
  fun processIndent (revAcc, _, []) = List.rev revAcc
    | processIndent (revAcc, indent, Fragment s :: fragments) =
        processIndent (s :: revAcc, indent, fragments)
    | processIndent (revAcc, indent, IncreaseIndent :: fragments) =
        processIndent (revAcc, indent + 1, fragments)
    | processIndent (revAcc, indent, DecreaseIndent :: fragments) =
        processIndent (revAcc, indent - 1, fragments)
    | processIndent (revAcc, indent, Indent :: fragments) =
        processIndent
          ( CharVector.tabulate (indent mod 8, fn _ => #" ")
            :: CharVector.tabulate (indent div 8, fn _ => #"\t") :: revAcc
          , indent
          , fragments
          )
    | processIndent (revAcc, indent, LineTerminator :: fragments) =
        processIndent ("\n" :: revAcc, indent, fragments)
  fun buildProgram fragments =
    String.concat (processIndent ([], 0, fragments))

  fun paren true exp rest =
        Fragment "(" :: exp (Fragment ")" :: rest)
    | paren false exp rest = exp rest

  fun intToString n =
    if n >= 0 then Int.toString n
    else "-" ^ String.extract (Int.toString n, 1, NONE)

  fun doKey (S.IntKey n) =
        toWideStringLit (JsSyntax.asciiStringAsIntVector (intToString n))
    | doKey (S.StringKey s) =
        if isIdentifier s then s
        else toWideStringLit (JsSyntax.asciiStringAsIntVector s)
  fun commaSep ([]: (Fragment list -> Fragment list) list) rest : Fragment list =
        rest
    | commaSep (x :: xs) rest =
        x (commaSep1 xs rest)
  and commaSep1 [] rest = rest
    | commaSep1 (x :: xs) rest =
        Fragment ", " :: x (commaSep1 xs rest)
  fun commaSepV (v: (Fragment list -> Fragment list) vector) rest :
    Fragment list =
    (case VectorSlice.getItem (VectorSlice.full v) of
       NONE => rest
     | SOME (x, xs) => x (commaSepV1 xs rest))
  and commaSepV1 xs rest =
    (case VectorSlice.getItem xs of
       NONE => rest
     | SOME (x, xss) => Fragment ", " :: x (commaSepV1 xss rest))

  datatype BinaryOp =
    InfixOp of (* prec *) int * string
  | InfixOpR of (* prec *) int * string
  | ExponentiationOp
  fun binOpInfo S.PLUS = InfixOp (7, "+")
    | binOpInfo S.MINUS = InfixOp (7, "-")
    | binOpInfo S.TIMES = InfixOp (6, "*")
    | binOpInfo S.DIV = InfixOp (6, "/")
    | binOpInfo S.MOD = InfixOp (6, "%")
    | binOpInfo S.BITAND = InfixOp (11, "&")
    | binOpInfo S.BITXOR = InfixOp (12, "^")
    | binOpInfo S.BITOR = InfixOp (13, "|")
    | binOpInfo S.RSHIFT = InfixOp (8, ">>")
    | binOpInfo S.LSHIFT = InfixOp (8, "<<")
    | binOpInfo S.URSHIFT = InfixOp (8, ">>>")
    | binOpInfo S.LT = InfixOp (9, "<")
    | binOpInfo S.LE = InfixOp (9, "<=")
    | binOpInfo S.GT = InfixOp (9, ">")
    | binOpInfo S.GE = InfixOp (9, ">=")
    | binOpInfo S.EQUAL = InfixOp (10, "===")
    | binOpInfo S.NOTEQUAL = InfixOp (10, "!==")
    | binOpInfo S.LAXEQUAL = InfixOp (10, "==")
    | binOpInfo S.NOTLAXEQUAL = InfixOp (10, "!=")
    | binOpInfo S.AND = InfixOp (14, "&&")
    | binOpInfo S.OR = InfixOp (15, "||")
    | binOpInfo S.ASSIGN = InfixOp (17, "=")
    | binOpInfo S.INSTANCEOF = InfixOp (9, "instanceof")
    | binOpInfo S.IN = InfixOp (9, "in")
    | binOpInfo S.EXP = ExponentiationOp

  fun doConst S.Null =
        (fn rest => Fragment "null" :: rest)
    | doConst S.False =
        (fn rest => Fragment "false" :: rest)
    | doConst S.True =
        (fn rest => Fragment "true" :: rest)
    | doConst (S.Numeral s) =
        (fn rest => Fragment s :: rest)
    | doConst (S.WideString s) =
        (fn rest => Fragment (toWideStringLit s) :: rest)
  fun mkWriter (nameMap, labelMap) =
    let
      fun doExp (_, S.ConstExp ct) : Fragment list -> Fragment list = doConst ct
        | doExp (_, S.ThisExp) =
            (fn rest => Fragment "this" :: rest)
        | doExp (_, S.VarExp id) =
            (fn rest => Fragment (idToJs (nameMap, id)) :: rest)
        | doExp (_, S.ObjectExp fields) =
            (fn rest =>
               Fragment "{"
               ::
               commaSepV
                 (Vector.map
                    (fn (key, value) =>
                       fn rest =>
                         Fragment (doKey key) :: Fragment ": "
                         :: doExp (Precedence.AssignmentExpression, value) rest)
                    fields) (Fragment "}" :: rest))
        | doExp (_, S.ArrayExp elements) =
            (fn rest =>
               Fragment "[" :: doCommaSepExp elements (Fragment "]" :: rest))
        | doExp (prec, S.CallExp (fnExp, arguments)) =
            paren (prec < Precedence.CallExpression) (fn rest =>
              doExp (Precedence.CallExpression, fnExp)
                (Fragment "(" :: doCommaSepExp arguments (Fragment ")" :: rest)))
        | doExp (prec, S.MethodExp (objectExp, methodName, arguments)) =
            paren (prec < Precedence.CallExpression) (fn rest =>
              doExp (Precedence.MemberExpression, objectExp)
                (Fragment "." :: Fragment methodName :: Fragment "("
                 :: doCommaSepExp arguments (Fragment ")" :: rest)))
        | doExp (prec, S.NewExp (constructorExp, arguments)) =
            paren (prec < Precedence.MemberExpression) (fn rest =>
              Fragment "new "
              ::
              doExp (Precedence.MemberExpression, constructorExp)
                (Fragment "(" :: doCommaSepExp arguments (Fragment ")" :: rest)))
        | doExp (_, S.FunctionExp (parameters, body)) =
            (fn rest =>
               Fragment "function" :: Fragment "("
               ::
               commaSepV
                 (Vector.map
                    (fn id => fn rest => Fragment (idToJs (nameMap, id)) :: rest)
                    parameters)
                 (Fragment ") {" :: IncreaseIndent :: LineTerminator
                  ::
                  doBlock body
                    (DecreaseIndent :: Indent :: Fragment "}" :: rest)))
        | doExp (prec, S.BinExp (binOp, x, y)) =
            (case binOpInfo binOp of
               InfixOp (prec', symbol) =>
                 paren (prec < prec') (fn rest =>
                   doExp (prec', x)
                     (Fragment " " :: Fragment symbol :: Fragment " "
                      :: doExp (prec' - 1, y) rest))
             | InfixOpR (prec', symbol) =>
                 paren (prec < prec') (fn rest =>
                   doExp (prec' - 1, x)
                     (Fragment " " :: Fragment symbol :: Fragment " "
                      :: doExp (prec', y) rest))
             | ExponentiationOp =>
                 paren (prec < 5) (fn rest =>
                   doExp (3, x) (Fragment " ** " :: doExp (5, y) rest)))
        | doExp (prec, S.UnaryExp (unOp, x)) =
            let
              val symbol =
                case unOp of
                  S.VOID => "void"
                | S.TYPEOF => "typeof"
                | S.TONUMBER => "+"
                | S.NEGATE => "-"
                | S.BITNOT => "~"
                | S.NOT => "!"
            in
              paren (prec < Precedence.UnaryExpression) (fn rest =>
                Fragment symbol :: Fragment " "
                :: doExp (Precedence.UnaryExpression, x) rest)
            end
        | doExp (prec, S.IndexExp (objectExp, indexExp)) =
            let
              val tryIdentifierName =
                case indexExp of
                  S.ConstExp (S.WideString name) =>
                    let
                      val name =
                        Vector.foldr
                          (fn (_, NONE) => NONE
                            | (c, SOME xs) =>
                             if c < 128 then SOME (chr c :: xs) else NONE)
                          (SOME []) name
                    in
                      case name of
                        SOME name =>
                          let
                            val name = CharVector.fromList name
                          in
                            if isIdentifierName name (* ES5 or later *) then
                              SOME name
                            else
                              NONE
                          end
                      | NONE => NONE
                    end
                | _ => NONE
              val indexPart =
                case tryIdentifierName of
                  SOME name =>
                    let
                      val isIntegerLiteral =
                        case objectExp of
                          S.ConstExp (S.Numeral s) =>
                            CharVector.all
                              (fn #"." => false
                                | #"e" => false
                                | #"E" => false
                                | _ => true) s
                        | _ => false
                    in
                      if isIntegerLiteral then
                        fn rest => Fragment " ." :: Fragment name :: rest
                      else
                        fn rest => Fragment "." :: Fragment name :: rest
                    end
                | _ =>
                    (fn rest =>
                       Fragment "["
                       ::
                       doExp (Precedence.Expression, indexExp)
                         (Fragment "]" :: rest))
            in
              paren (prec < Precedence.MemberExpression)
                (doExp (Precedence.MemberExpression, objectExp) o indexPart)
            end
        | doExp (prec, S.CondExp (exp1, exp2, exp3)) =
            paren (prec < Precedence.ConditionalExpression) (fn rest =>
              doExp (Precedence.LogicalORExpression, exp1)
                (Fragment " ? "
                 ::
                 doExp (Precedence.AssignmentExpression, exp2)
                   (Fragment " : "
                    :: doExp (Precedence.AssignmentExpression, exp3) rest)))
      and doCommaSepExp elements =
        commaSepV
          (Vector.map
             (fn value => doExp (Precedence.AssignmentExpression, value))
             elements)
      and doStat (S.LetStat variables) =
            (fn rest =>
               Indent :: Fragment "let "
               ::
               commaSepV
                 (Vector.map
                    (fn (id, NONE) =>
                       (fn rest =>
                          Fragment (idToJs (nameMap, S.UserDefinedId id))
                          :: rest)
                      | (id, SOME init) =>
                       (fn rest =>
                          Fragment (idToJs (nameMap, S.UserDefinedId id))
                          :: Fragment " = "
                          :: doExp (Precedence.AssignmentExpression, init) rest))
                    variables) (Fragment ";" :: LineTerminator :: rest))
        | doStat (S.ConstStat variables) =
            (fn rest =>
               Indent :: Fragment "const "
               ::
               commaSepV
                 (Vector.map
                    (fn (id, init) =>
                       (fn rest =>
                          Fragment (idToJs (nameMap, S.UserDefinedId id))
                          :: Fragment " = "
                          :: doExp (Precedence.AssignmentExpression, init) rest))
                    variables) (Fragment ";" :: LineTerminator :: rest))
        | doStat (S.ExpStat exp) =
            (fn rest =>
               let
                 val rest' = Fragment ";" :: LineTerminator :: rest
                 val fragments = doExp (Precedence.Expression, exp) rest'
                 val needParen =
                   case fragments of
                     Fragment "{" :: _ => true
                   | Fragment "function" :: _ => true
                   | _ => false
               in
                 Indent
                 ::
                 (if needParen then
                    paren true (doExp (Precedence.Expression, exp)) rest'
                  else
                    fragments)
               end)
        | doStat (S.IfStat (cond, thenBlock, elseBlock)) =
            (fn rest =>
               let
                 fun processElseIfs (elseIfsRev, elseBlock) =
                   if Vector.length elseBlock = 1 then
                     case Vector.sub (elseBlock, 0) of
                       S.IfStat (cond', thenBlock, elseBlock) =>
                         processElseIfs
                           ((cond', thenBlock) :: elseIfsRev, elseBlock)
                     | _ => (elseIfsRev, elseBlock)
                   else
                     (elseIfsRev, elseBlock)
                 val (elseIfsRev, elseBlock) = processElseIfs ([], elseBlock)
                 val elsePart =
                   if Vector.length elseBlock = 0 then
                     DecreaseIndent :: Indent :: Fragment "}" :: LineTerminator
                     :: rest
                   else
                     DecreaseIndent :: Indent :: Fragment "} else {"
                     :: IncreaseIndent :: LineTerminator
                     ::
                     doBlock elseBlock
                       (DecreaseIndent :: Indent :: Fragment "}"
                        :: LineTerminator :: rest)
                 val elseIfsAndElsePart =
                   List.foldl
                     (fn ((cond, elseIfBlock), acc) =>
                        DecreaseIndent :: Indent :: Fragment "} else if ("
                        ::
                        doExp (Precedence.Expression, cond)
                          (Fragment ") {" :: IncreaseIndent :: LineTerminator
                           :: doBlock elseIfBlock acc)) elsePart elseIfsRev
               in
                 Indent :: Fragment "if ("
                 ::
                 doExp (Precedence.Expression, cond)
                   (Fragment ") {" :: IncreaseIndent :: LineTerminator
                    :: doBlock thenBlock elseIfsAndElsePart)
               end)
        | doStat (S.ReturnStat NONE) =
            (fn rest => Indent :: Fragment "return;" :: LineTerminator :: rest)
        | doStat (S.ReturnStat (SOME exp)) =
            (fn rest =>
               Indent :: Fragment "return "
               ::
               doExp (Precedence.Expression, exp)
                 (Fragment ";" :: LineTerminator :: rest))
        | doStat (S.TryCatchStat (body, exnName, catch)) =
            (fn rest =>
               Indent :: Fragment "try {" :: IncreaseIndent :: LineTerminator
               ::
               doBlock body
                 (DecreaseIndent :: Indent :: Fragment "} catch ("
                  :: Fragment (idToJs (nameMap, S.UserDefinedId exnName))
                  :: Fragment ") {" :: IncreaseIndent :: LineTerminator
                  ::
                  doBlock catch
                    (DecreaseIndent :: Indent :: Fragment "}" :: LineTerminator
                     :: rest)))
        | doStat (S.ThrowStat exp) =
            (fn rest =>
               Indent :: Fragment "throw "
               ::
               doExp (Precedence.Expression, exp)
                 (Fragment ";" :: LineTerminator :: rest))
        | doStat (S.BlockStat (NONE, block)) =
            (fn rest =>
               Indent :: Fragment "{" :: IncreaseIndent :: LineTerminator
               ::
               doBlock block
                 (DecreaseIndent :: Indent :: Fragment "}" :: LineTerminator
                  :: rest))
        | doStat (S.BlockStat (SOME label, block)) =
            (fn rest =>
               Indent :: Fragment (idToJs (labelMap, label)) :: Fragment ": {"
               :: IncreaseIndent :: LineTerminator
               ::
               doBlock block
                 (DecreaseIndent :: Indent :: Fragment "}" :: LineTerminator
                  :: rest))
        | doStat (S.LoopStat (NONE, block)) =
            (fn rest =>
               Indent :: Fragment "for (;;) {" :: IncreaseIndent
               :: LineTerminator
               ::
               doBlock block
                 (DecreaseIndent :: Indent :: Fragment "}" :: LineTerminator
                  :: rest))
        | doStat (S.LoopStat (SOME label, block)) =
            (fn rest =>
               Indent :: Fragment (idToJs (labelMap, label))
               :: Fragment ": for (;;) {" :: IncreaseIndent :: LineTerminator
               ::
               doBlock block
                 (DecreaseIndent :: Indent :: Fragment "}" :: LineTerminator
                  :: rest))
        | doStat (S.SwitchStat (exp, cases)) =
            (fn rest =>
               Indent :: Fragment "switch ("
               ::
               doExp (Precedence.Expression, exp)
                 (Fragment ") {" :: LineTerminator
                  ::
                  List.foldr
                    (fn ((c, block), rest) =>
                       Indent :: Fragment "case "
                       ::
                       doConst c
                         (Fragment ":" :: IncreaseIndent :: LineTerminator
                          :: Indent :: Fragment "{" :: IncreaseIndent
                          :: LineTerminator
                          ::
                          doBlock block
                            (DecreaseIndent :: Indent :: Fragment "}"
                             :: LineTerminator :: rest)))
                    (DecreaseIndent :: Indent :: Fragment "}" :: LineTerminator
                     :: rest) cases))
        | doStat (S.BreakStat NONE) =
            (fn rest => Indent :: Fragment "break;" :: LineTerminator :: rest)
        | doStat (S.BreakStat (SOME label)) =
            (fn rest =>
               Indent :: Fragment "break "
               :: Fragment (idToJs (labelMap, label)) :: Fragment ";"
               :: LineTerminator :: rest)
        | doStat (S.ContinueStat NONE) =
            (fn rest => Indent :: Fragment "continue;" :: LineTerminator :: rest)
        | doStat (S.ContinueStat (SOME label)) =
            (fn rest =>
               Indent :: Fragment "continue "
               :: Fragment (idToJs (labelMap, label)) :: Fragment ";"
               :: LineTerminator :: rest)
        | doStat (S.DefaultExportStat exp) =
            (fn rest =>
               let
                 val rest' = Fragment ";" :: LineTerminator :: rest
                 val fragments =
                   doExp (Precedence.AssignmentExpression, exp) rest'
                 val needParen =
                   case fragments of
                     Fragment "function" :: _ => true
                   | Fragment "async function" :: _ => true
                   | Fragment "class" :: _ => true
                   | _ => false
               in
                 Indent :: Fragment "export default "
                 ::
                 (if needParen then
                    paren true (doExp (Precedence.AssignmentExpression, exp))
                      rest'
                  else
                    fragments)
               end)
        | doStat (S.NamedExportStat entities) =
            (fn rest =>
               Indent :: Fragment "export {"
               ::
               commaSepV
                 (Vector.map
                    (fn (v, name) =>
                       fn rest =>
                         let
                           val jsId = idToJs (nameMap, v)
                         in
                           if name = jsId then
                             Fragment name :: rest
                           else
                             Fragment jsId :: Fragment " as " :: Fragment name
                             :: rest
                         end) entities)
                 (Fragment "};" :: LineTerminator :: rest))
      and doBlock stats =
        (fn rest => Vector.foldr (fn (stat, acc) => doStat stat acc) rest stats)
      fun doProgram stats =
        buildProgram (doBlock stats [])
    in
      {doProgram = doProgram}
    end


  (*: val doImports : string TypedSyntax.VIdMap.map * { specs : (string * JsSyntax.Id) list, moduleName : string } list -> string *)
  fun doImports (nameMap, imports) =
    let
      fun importOne ({specs, moduleName}, rest) =
        let
          val (default, named) =
            List.partition (fn (name, _) => name = "default") specs
        (* length default must be <= 1 *)
        in
          case (default, named) of
            ([], []) =>
              Fragment "import " :: Fragment (toStringLit moduleName)
              :: Fragment ";" :: LineTerminator :: rest
          | ((_, defaultId) :: _, []) =>
              Fragment "import " :: Fragment (idToJs (nameMap, defaultId))
              :: Fragment " from " :: Fragment (toStringLit moduleName)
              :: Fragment ";" :: LineTerminator :: rest
          | ([], _ :: _) =>
              Fragment "import {"
              ::
              commaSep
                (List.map
                   (fn (name, vid) =>
                      fn rest =>
                        let
                          val jsId = idToJs (nameMap, vid)
                        in
                          if name = jsId then
                            Fragment name :: rest
                          else
                            (if isIdentifierName name then Fragment name
                             else Fragment (toStringLit name))
                            :: Fragment " as " :: Fragment jsId :: rest
                        end) named)
                (Fragment "} from " :: Fragment (toStringLit moduleName)
                 :: Fragment ";" :: LineTerminator :: rest)
          | ((_, defaultId) :: _, _ :: _) =>
              Fragment "import " :: Fragment (idToJs (nameMap, defaultId))
              :: Fragment ", {"
              ::
              commaSep
                (List.map
                   (fn (name, vid) =>
                      fn rest =>
                        (if isIdentifierName name then Fragment name
                         else Fragment (toStringLit name)) :: Fragment " as "
                        :: Fragment (idToJs (nameMap, vid)) :: rest) named)
                (Fragment "} from " :: Fragment (toStringLit moduleName)
                 :: Fragment ";" :: LineTerminator :: rest)
        end
    in
      buildProgram (List.foldr importOne [] imports)
    end
end;
