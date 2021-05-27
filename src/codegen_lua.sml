(*
 * Copyright (c) 2021 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure CodeGenLua = struct
(* Mapping of types:
 * SML -> Lua
 * int -> integer
 * word -> integer
 * real -> number
 * string -> string
 * char -> single-character string
 * exn -> ???
 * bool -> boolean
 * ref -> {tag = "ref", payload = <mutable> ...}
 * list -> {tag = "nil"} | {tag = "::", payload = ...}
 * record { label1 = ..., label2 = ... } -> table
 * function -> function
 *)
(* Mapping of local variable names:
 * SML -> Lua
 * MkVId(name, n) -> name ^ "_" ^ Int.toString n
 * _ -> __
 * ' -> _PRIME
 * ! -> _EXCLAM
 * % -> _PERCENT
 * & -> _AMPER
 * $ -> _DOLLAR
 * # -> _HASH
 * + -> _PLUS
 * - -> _MINUS
 * / -> _SLASH
 * : -> _COLON
 * < -> _LT
 * = -> _EQ
 * > -> _GT
 * ? -> _QUESTION
 * @ -> _AT
 * \ -> _BACKSLASH
 * ~ -> _TILDE
 * ` -> _GRAVE
 * ^ -> _HAT
 * | -> _BAR
 * * -> _ASTER
 *)
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
  | smlNameToLuaChar x = if Char.isAlphaNum x then String.str x else raise Fail "smlNameToLua: invalid character"
fun smlNameToLua(name) = String.translate smlNameToLuaChar name
val builtins
    = let open InitialEnv
      in List.foldl USyntax.VIdMap.insert' USyntax.VIdMap.empty
                    [(* ref *)
                     (VId_ref, "_ref")
                    ,(VId_COLONEQUAL, "_set")
                    ,(VId_EXCLAM, "_read")
                    (* boolean *)
                    ,(VId_true, "true") (* boolean literal *)
                    ,(VId_false, "false") (* boolean literal *)
                    ,(VId_Bool_not, "_not") (* Lua not *)
                    (* list *)
                    ,(VId_nil, "_nil")
                    ,(VId_DCOLON, "_cons")
                    (* exn *)
                    ,(VId_Match, "_Match")
                    ,(VId_Bind, "_Bind")
                    ,(VId_Div, "_Div")
                    ,(VId_Overflow, "_Overflow")
                    ,(VId_Size, "_Size")
                    ,(VId_Subscript, "_Subscript")
                    ,(VId_Fail, "_Fail")
                    ,(VId_Match_tag, "_Match_tag")
                    ,(VId_Bind_tag, "_Bind_tag")
                    ,(VId_Div_tag, "_Div_tag")
                    ,(VId_Overflow_tag, "_Overflow_tag")
                    ,(VId_Size_tag, "_Size_tag")
                    ,(VId_Subscript_tag, "_Subscript_tag")
                    ,(VId_Fail_tag, "_Fail_tag")
	            (* VId_EQUAL *)
                    (* Overloaded: VId_abs, VId_TILDE, VId_div, VId_mod, VId_TIMES, VId_DIVIDE, VId_PLUS, VId_MINUS, VId_LT, VId_GT, VId_LE, VId_GE *)
                    ,(VId_EQUAL_bool, "_EQUAL") (* Lua == *)
                    ,(VId_EQUAL_int, "_EQUAL") (* Lua == *)
                    ,(VId_EQUAL_word, "_EQUAL") (* Lua == *)
                    ,(VId_EQUAL_string, "_EQUAL") (* Lua == *)
                    ,(VId_EQUAL_char, "_EQUAL") (* Lua == *)
                    ,(VId_EQUAL_list, "_List_EQUAL")
                    ,(VId_EQUAL_ref, "_EQUAL") (* Lua == *)
                    ,(VId_EQUAL_array, "_EQUAL") (* Lua == *)
                    ,(VId_EQUAL_vector, "_Vector_EQUAL")
                    ,(VId_EQUAL_exntag, "_EQUAL_exntag") (* Lua == *)
                    (* int *)
                    ,(VId_Int_PLUS, "_Int_add") (* may raise Overflow *)
                    ,(VId_Int_MINUS, "_Int_sub") (* may raise Overflow *)
                    ,(VId_Int_TIMES, "_Int_mul") (* may raise Overflow *)
                    ,(VId_Int_abs, "_Int_abs") (* may raise Overflow *)
                    ,(VId_Int_TILDE, "_Int_negate") (* may raise Overflow *)
                    ,(VId_Int_div, "_Int_div") (* may raise Overflow/Div *)
                    ,(VId_Int_mod, "_Int_mod") (* may raise Div *)
                    ,(VId_Int_LT, "_LT")
                    ,(VId_Int_GT, "_GT")
                    ,(VId_Int_LE, "_LE")
                    ,(VId_Int_GE, "_GE")
                    (* word *)
                    ,(VId_Word_PLUS, "_PLUS") (* Lua +; does not raise Overflow *)
                    ,(VId_Word_MINUS, "_MINUS") (* Lua - (binary); does not raise Overflow *)
                    ,(VId_Word_TIMES, "_TIMES") (* Lua *; does not raise Overflow *)
                    ,(VId_Word_div, "_Word_div") (* may raise Div *)
                    ,(VId_Word_mod, "_Word_mod") (* may raise Div *)
                    ,(VId_Word_TILDE, "_unm") (* Lua - (unary) *)
                    ,(VId_Word_LT, "_Word_LT")
                    ,(VId_Word_GT, "_Word_GT")
                    ,(VId_Word_LE, "_Word_LE")
                    ,(VId_Word_GE, "_Word_GE")
                    (* real *)
                    ,(VId_Real_PLUS, "_PLUS") (* Lua + *)
                    ,(VId_Real_MINUS, "_MINUS") (* Lua - (binary) *)
                    ,(VId_Real_TIMES, "_TIMES") (* Lua * *)
                    ,(VId_Real_DIVIDE, "_DIVIDE") (* Lua / *)
                    ,(VId_Real_abs, "_Real_abs") (* Lua math.abs *)
                    ,(VId_Real_TILDE, "_unm") (* Lua - (unary) *)
                    ,(VId_Real_LT, "_LT")
                    ,(VId_Real_GT, "_GT")
                    ,(VId_Real_LE, "_LE")
                    ,(VId_Real_GE, "_GE")
                    (* String *)
                    ,(VId_String_HAT, "_concat")
                    ,(VId_String_size, "_length")
                    ,(VId_String_str, "_id") (* no-op *)
                    ,(VId_String_LT, "_LT")
                    ,(VId_String_GT, "_GT")
                    ,(VId_String_LE, "_LE")
                    ,(VId_String_GE, "_GE")
                    (* Char *)
                    ,(VId_Char_LT, "_LT")
                    ,(VId_Char_GT, "_GT")
                    ,(VId_Char_LE, "_LE")
                    ,(VId_Char_GE, "_GE")
                    (* Array and Vector *)
                    ,(VId_Array_array, "_Array_array")
                    ,(VId_Array_fromList, "_VectorOrArray_fromList")
                    ,(VId_Array_tabulate, "_VectorOrArray_tabulate")
                    ,(VId_Array_length, "_VectorOrArray_length")
                    ,(VId_Array_sub, "_VectorOrArray_sub")
                    ,(VId_Array_update, "_Array_update")
                    ,(VId_Vector_fromList, "_VectorOrArray_fromList")
                    ,(VId_Vector_tabulate, "_VectorOrArray_tabulate")
                    ,(VId_Vector_length, "_VectorOrArray_length")
                    ,(VId_Vector_sub, "_VectorOrArray_sub")
                    (* Lua interface *)
                    ,(VId_Lua_sub, "_Lua_sub")
                    ,(VId_Lua_set, "_Lua_set")
                    ,(VId_Lua_global, "_Lua_global")
                    ,(VId_Lua_call, "_Lua_call")
                    ,(VId_Lua_method, "_Lua_method")
                    ,(VId_Lua_NIL, "nil") (* literal *)
                    ,(VId_Lua_isNil, "_Lua_isNil")
                    ,(VId_Lua_isFalsy, "_not")
                    ,(VId_Lua_unsafeToValue, "_id") (* no-op *)
                    ,(VId_Lua_unsafeFromValue, "_id") (* no-op *)
                    ,(VId_Lua_newTable, "_Lua_newTable")
                    ,(VId_Lua_function, "_Lua_function")
                    ,(VId_Lua_PLUS, "_PLUS")
                    ,(VId_Lua_MINUS, "_MINUS")
                    ,(VId_Lua_TIMES, "_TIMES")
                    ,(VId_Lua_DIVIDE, "_DIVIDE")
                    ,(VId_Lua_INTDIV, "_INTDIV")
                    ,(VId_Lua_MOD, "_MOD")
                    ,(VId_Lua_pow, "_pow")
                    ,(VId_Lua_unm, "_unm")
                    ,(VId_Lua_andb, "_andb")
                    ,(VId_Lua_orb, "_orb")
                    ,(VId_Lua_xorb, "_xorb")
                    ,(VId_Lua_notb, "_notb")
                    ,(VId_Lua_LSHIFT, "_LSHIFT")
                    ,(VId_Lua_RSHIFT, "_RSHIFT")
                    ,(VId_Lua_concat, "_concat")
                    ,(VId_Lua_length, "_length")
                    (* extra *)
                    ,(VId_assumePure, "_id") (* no-op *)
                    ,(VId_assumeDiscardable, "_id") (* no-op *)
                    ]
      end
datatype BinaryOp = InfixOp of (* prec *) int * string
                  | InfixOpR of (* prec *) int * string
                  | NamedBinaryFn of string
                  | WordCompare of { flip : bool, negate : bool }
val builtinBinaryOps : (BinaryOp * (* pure? *) bool) USyntax.VIdMap.map
    = let open InitialEnv
      in List.foldl USyntax.VIdMap.insert' USyntax.VIdMap.empty
                    [(VId_EQUAL_bool,   (InfixOp (10, "=="), true))
                    ,(VId_EQUAL_int,    (InfixOp (10, "=="), true))
                    ,(VId_EQUAL_word,   (InfixOp (10, "=="), true))
                    ,(VId_EQUAL_string, (InfixOp (10, "=="), true))
                    ,(VId_EQUAL_char,   (InfixOp (10, "=="), true))
                    ,(VId_EQUAL_exntag, (InfixOp (10, "=="), true))
                    ,(VId_Int_PLUS,     (NamedBinaryFn "__Int_add", false))
                    ,(VId_Int_MINUS,    (NamedBinaryFn "__Int_sub", false))
                    ,(VId_Int_TIMES,    (NamedBinaryFn "__Int_mul", false))
                    ,(VId_Int_div,      (NamedBinaryFn "__Int_div", false))
                    ,(VId_Int_mod,      (NamedBinaryFn "__Int_mod", false))
                    ,(VId_Int_LT,       (InfixOp (10, "<"), true))
                    ,(VId_Int_LE,       (InfixOp (10, "<="), true))
                    ,(VId_Int_GT,       (InfixOp (10, ">"), true))
                    ,(VId_Int_GE,       (InfixOp (10, ">="), true))
                    ,(VId_Word_PLUS,    (InfixOp (4, "+"), true))
                    ,(VId_Word_MINUS,   (InfixOp (4, "-"), true))
                    ,(VId_Word_TIMES,   (InfixOp (3, "*"), true))
                    ,(VId_Word_div,     (NamedBinaryFn "__Word_div", false))
                    ,(VId_Word_mod,     (NamedBinaryFn "__Word_mod", false))
                    ,(VId_Word_LT,      (WordCompare { flip = false, negate = false }, true))
                    ,(VId_Word_LE,      (WordCompare { flip = true, negate = true }, true))
                    ,(VId_Word_GT,      (WordCompare { flip = true, negate = false }, true))
                    ,(VId_Word_GE,      (WordCompare { flip = false, negate = true }, true))
                    ,(VId_Real_PLUS,    (InfixOp (4, "+"), true))
                    ,(VId_Real_MINUS,   (InfixOp (4, "-"), true))
                    ,(VId_Real_TIMES,   (InfixOp (3, "*"), true))
                    ,(VId_Real_DIVIDE,  (InfixOp (3, "/"), true))
                    ,(VId_Real_LT,      (InfixOp (10, "<"), true))
                    ,(VId_Real_LE,      (InfixOp (10, "<="), true))
                    ,(VId_Real_GT,      (InfixOp (10, ">"), true))
                    ,(VId_Real_GE,      (InfixOp (10, ">="), true))
                    ,(VId_Char_LT,      (InfixOp (10, "<"), true))
                    ,(VId_Char_LE,      (InfixOp (10, "<="), true))
                    ,(VId_Char_GT,      (InfixOp (10, ">"), true))
                    ,(VId_Char_GE,      (InfixOp (10, ">="), true))
                    ,(VId_String_LT,    (InfixOp (10, "<"), true))
                    ,(VId_String_LE,    (InfixOp (10, "<="), true))
                    ,(VId_String_GT,    (InfixOp (10, ">"), true))
                    ,(VId_String_GE,    (InfixOp (10, ">="), true))
                    ,(VId_String_HAT,   (InfixOpR (5, ".."), true))
                    ,(VId_Lua_PLUS,     (InfixOp (4, "+"), false))
                    ,(VId_Lua_MINUS,    (InfixOp (4, "-"), false))
                    ,(VId_Lua_TIMES,    (InfixOp (3, "*"), false))
                    ,(VId_Lua_DIVIDE,   (InfixOp (3, "/"), false))
                    ,(VId_Lua_INTDIV,   (InfixOp (3, "//"), false))
                    ,(VId_Lua_MOD,      (InfixOp (3, "%"), false))
                    ,(VId_Lua_pow,      (InfixOpR (1, "^"), false))
                    ,(VId_Lua_andb,     (InfixOp (7, "&"), false))
                    ,(VId_Lua_orb,      (InfixOp (9, "|"), false))
                    ,(VId_Lua_xorb,     (InfixOp (8, "~"), false))
                    ,(VId_Lua_LSHIFT,   (InfixOp (6, "<<"), false))
                    ,(VId_Lua_RSHIFT,   (InfixOp (6, ">>"), false))
                    ,(VId_Lua_EQUAL,    (InfixOp (10, "=="), false))
                    ,(VId_Lua_NOTEQUAL, (InfixOp (10, "~="), false))
                    ,(VId_Lua_LT,       (InfixOp (10, "<"), false))
                    ,(VId_Lua_GT,       (InfixOp (10, ">"), false))
                    ,(VId_Lua_LE,       (InfixOp (10, "<="), false))
                    ,(VId_Lua_GE,       (InfixOp (10, ">="), false))
                    ,(VId_Lua_concat,   (InfixOpR (5, ".."), false))
                    ]
      end
fun VIdToLua(vid as USyntax.MkVId(name, n)) = if n < 0 then
                                                  case USyntax.VIdMap.find (builtins, vid) of
                                                      NONE => raise Fail ("Unknown built-in symbol: " ^ name ^ "@" ^ Int.toString n)
                                                    | SOME luaExpr => luaExpr
                                              else
                                                  smlNameToLua name ^ "_" ^ Int.toString n

structure StringSet = RedBlackSetFn(struct open String; type ord_key = string end)
val LuaKeywords = StringSet.fromList
                      ["and", "break", "do", "else", "elseif", "end",
                       "false", "for", "function", "goto", "if", "in",
                       "local", "nil", "not", "or", "repeat", "return",
                       "then", "true", "until", "while"]
fun isLuaIdentifier name = case String.explode name of
                               [] => false
                             | x0 :: xs => (Char.isAlpha x0 orelse x0 = #"_")
                                           andalso (List.all (fn c => Char.isAlphaNum c orelse c = #"_") xs)
                                           andalso (not (StringSet.member(LuaKeywords, name)))
fun LabelToLua(Syntax.NumericLabel(n)) = Int.toString n
  | LabelToLua(Syntax.IdentifierLabel(s)) = "\"" ^ String.toString s ^ "\"" (* TODO: pretty-printing? *)

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

datatype Fragment = Fragment of string
                  | IncreaseIndent
                  | DecreaseIndent
                  | Indent
                  | OptSemicolon
                  | LineTerminator
fun findNextFragment [] = NONE
  | findNextFragment (Fragment "" :: fragments) = findNextFragment fragments
  | findNextFragment (Fragment s :: _) = SOME s
  | findNextFragment (_ :: fragments) = findNextFragment fragments
fun processIndent (indent, []) = []
  | processIndent (indent, Fragment s :: fragments) = s :: processIndent (indent, fragments)
  | processIndent (indent, IncreaseIndent :: fragments) = processIndent (indent + 2, fragments)
  | processIndent (indent, DecreaseIndent :: fragments) = processIndent (indent - 2, fragments)
  | processIndent (indent, Indent :: fragments) = CharVector.tabulate(indent, fn _ => #" ") :: processIndent (indent, fragments)
  | processIndent (indent, OptSemicolon :: fragments) = (case findNextFragment fragments of
                                                            NONE => "\n" :: processIndent (indent, fragments)
                                                          | SOME next => if String.sub (next, 0) = #"(" then
                                                                             ";\n" :: processIndent (indent, fragments)
                                                                         else
                                                                             "\n" :: processIndent (indent, fragments)
                                                        )
  | processIndent (indent, LineTerminator :: fragments) = "\n" :: processIndent (indent, fragments)
fun buildProgram fragments = String.concat (processIndent (0, fragments))

type Exp = { prec : int
           , exp : Fragment list
           }

fun paren allowed { prec, exp } = if allowed < prec then
                                      [ Fragment "(" ] @ exp @ [ Fragment ")" ] 
                                  else
                                      exp

type Context = { nextLuaId : int ref }
type Env = {}
val initialEnv : Env = {}

fun genSym (ctx: Context) = let val n = !(#nextLuaId ctx)
                                val _ = #nextLuaId ctx := n + 1
                            in "tmp" ^ Int.toString n
                            end

structure F = FSyntax

fun toLuaStringLit (s : string) = "\"" ^ String.translate (fn #"\\" => "\\\\"
                                                          | #"\a" => "\\a"
                                                          | #"\b" => "\\b"
                                                          | #"\f" => "\\f"
                                                          | #"\n" => "\\n"
                                                          | #"\r" => "\\r"
                                                          | #"\t" => "\\t"
                                                          | #"\v" => "\\v"
                                                          | #"\"" => "\\\""
                                                          | c => if Char.isAscii c andalso Char.isPrint c then
                                                                     String.str c
                                                                 else
                                                                     let val x = Char.ord c
                                                                     in
                                                                         if x > 255 then
                                                                             raise Fail ("this string cannot be expressed in Lua: " ^ String.toString s)
                                                                         else if x < 0x10 then
                                                                             "\\x0" ^ Int.fmt StringCvt.HEX x
                                                                         else
                                                                             "\\x" ^ Int.fmt StringCvt.HEX x
                                                                     end
                                                          ) s ^ "\""

fun doLiteral (Syntax.IntegerConstant x) = if x < 0 then { prec = 2, exp = [ Fragment ("-" ^ Int.toString (~ x)) ] } else {prec = 0, exp = [ Fragment (Int.toString x) ] }
  | doLiteral (Syntax.WordConstant x) = { prec = 0, exp = [ Fragment ("0x" ^ Word.toString x) ] }
  | doLiteral (Syntax.RealConstant x) = (if String.sub (x, 0) = #"~" then
                                             { prec = 2, exp = [ Fragment (String.map (fn #"~" => #"-" | c => c) x) ] }
                                         else
                                             { prec = 0, exp = [ Fragment (String.map (fn #"~" => #"-" | c => c) x) ] }
                                        )
  | doLiteral (Syntax.StringConstant x) = { prec = 0, exp = [ Fragment (toLuaStringLit x) ] }
  | doLiteral (Syntax.CharacterConstant x) = { prec = 0, exp = [ Fragment (toLuaStringLit x) ] }

datatype Destination = Return
                     | AssignTo of string
                     | UnpackingAssignTo of string list
                     | Discard
                     | Continue of (* statements *) Fragment list * (* pure expression *) Exp -> Fragment list (* the continuation should be called exactly once, and the expression should be used only once *)

(* mapCont : ('a * ('b -> 'r) -> 'r) -> 'a list -> ('b list -> 'r) -> 'r *)
fun mapCont f [] cont = cont []
  | mapCont f (x :: xs) cont = f (x, fn y => mapCont f xs (fn ys => cont (y :: ys)))

fun commaSep ([] : (Fragment list) list) : Fragment list = []
  | commaSep (x :: xs) = x @ commaSep1 xs
and commaSep1 [] = []
  | commaSep1 (x :: xs) = Fragment ", " :: x @ commaSep1 xs

(* doExpTo : Context -> Env -> F.Exp -> Destination -> Line list *)
fun putPureTo ctx env Return (stmts, exp : Exp) = stmts @ [ Indent, Fragment "return " ] @ #exp exp @ [ OptSemicolon ]
  | putPureTo ctx env (AssignTo v) (stmts, exp) = stmts @ [ Indent, Fragment (v ^ " = ") ] @ #exp exp @ [ OptSemicolon ]
  | putPureTo ctx env (UnpackingAssignTo v) (stmts, exp) = stmts @ [ Indent, Fragment (String.concatWith ", " v ^ " = table.unpack(") ] @ #exp exp @ [ Fragment (", 1, " ^ Int.toString (List.length v) ^ ")"), OptSemicolon ]
  | putPureTo ctx env Discard (stmts, exp) = stmts
  | putPureTo ctx env (Continue cont) (stmts, exp) = cont (stmts, exp)
and putImpureTo ctx env Return (stmts, exp : Exp) = stmts @ [ Indent, Fragment "return " ] @ #exp exp @ [ OptSemicolon ]
  | putImpureTo ctx env (AssignTo v) (stmts, exp) = stmts @ [ Indent, Fragment (v ^ " = ") ] @ #exp exp @ [ OptSemicolon ]
  | putImpureTo ctx env (UnpackingAssignTo v) (stmts, exp) = stmts @ [ Indent, Fragment (String.concatWith ", " v ^ " = table.unpack(") ] @ #exp exp @ [ Fragment (", 1, " ^ Int.toString (List.length v) ^ ")"), OptSemicolon ]
  | putImpureTo ctx env Discard (stmts, exp) = stmts @ (if #prec exp = ~2 then
                                                            Indent :: #exp exp @ [ OptSemicolon ]
                                                        else
                                                            [ Indent, Fragment "_id(" ] @ #exp exp @ [ Fragment ")", OptSemicolon ]
                                                       )
  | putImpureTo ctx env (Continue cont) (stmts, exp) = let val dest = genSym ctx
                                                       in cont (stmts @ [ Indent, Fragment ("local " ^ dest ^ " = ") ] @ #exp exp @ [ OptSemicolon ], { prec = ~1, exp = [ Fragment dest ] })
                                                       end
and doExpCont ctx env exp cont = doExpTo ctx env exp (Continue cont)
and doExpTo ctx env (F.SConExp scon) dest : Fragment list = putPureTo ctx env dest ([], doLiteral scon)
  | doExpTo ctx env (F.VarExp (Syntax.MkQualified (_, vid))) dest = putPureTo ctx env dest ([], { prec = ~1, exp = [ Fragment (VIdToLua vid) ] })
  | doExpTo ctx env (F.RecordExp []) dest = putPureTo ctx env dest ([], { prec = ~1, exp = [ Fragment "nil" ] })
  | doExpTo ctx env (F.RecordExp fields) Discard = List.concat (List.map (fn (_, exp) => doExpTo ctx env exp Discard) fields)
  | doExpTo ctx env (F.RecordExp fields) dest
    = mapCont (fn ((label, exp), cont) => doExpCont ctx env exp (fn (stmts, e) => cont (stmts, (label, e))))
              fields
              (fn ys => let val (stmts, fields') = ListPair.unzip ys
                        in putPureTo ctx env dest (List.concat stmts
                                                  , case Syntax.extractTuple(1, fields) of
                                                        SOME _ => { prec = 0, exp = [ Fragment "{" ] @ commaSep (List.map (#exp o #2) fields') @ [ Fragment "}" ] }
                                                      | NONE => { prec = 0, exp = [ Fragment "{" ] @ commaSep (List.map (fn (label, e) => [ Fragment ("[" ^ LabelToLua label ^ "] = ") ] @ #exp e) fields') @ [ Fragment "}" ] }
                                                  )
                        end
              )
  | doExpTo ctx env (F.LetExp (dec, exp)) (Continue cont)
    = let val dec' = doDec ctx env dec
      in doExpCont ctx env exp (fn (stmts, exp) => cont (dec' @ stmts, exp))
      end
  | doExpTo ctx env (F.LetExp (dec, exp)) dest
    = let val dec' = doDec ctx env dec
      in dec' @ doExpTo ctx env exp dest
      end
  | doExpTo ctx env (F.AppExp (exp1, exp2)) dest
    = let val doProjection = case exp1 of
                                 F.ProjectionExp { label, ... } =>
                                 SOME (fn () => doExpCont ctx env exp2 (fn (stmts, exp2') => putPureTo ctx env dest (stmts, { prec = ~1, exp = paren ~1 exp2' @ [ Fragment ("[" ^ LabelToLua label ^ "]") ] })))
                               | _ => NONE
          val doBinary = case (exp1, exp2) of
                             (F.VarExp (Syntax.MkQualified([], vid)), F.RecordExp [(Syntax.NumericLabel 1, e1), (Syntax.NumericLabel 2, e2)]) =>
                             let fun wrap f = SOME (fn () => doExpCont ctx env e1 (fn (stmts1, e1') => doExpCont ctx env e2 (fn (stmts2, e2') => f (stmts1 @ stmts2, e1', e2'))))
                             in case USyntax.VIdMap.find(builtinBinaryOps, vid) of
                                    SOME (binop, pure) => wrap (fn (stmts, e1', e2') =>
                                                                   let val e = case binop of
                                                                                   InfixOp (prec, luaop) => { prec = prec, exp = paren prec e1' @ Fragment (" " ^ luaop ^ " ") :: paren (prec + 1) e2' }
                                                                                 | InfixOpR (prec, luaop) => { prec = prec, exp = paren (prec + 1) e1' @ Fragment (" " ^ luaop ^ " ") :: paren prec e2' }
                                                                                 | NamedBinaryFn luafn => { prec = ~2, exp = Fragment (luafn ^ "(") :: #exp e1' @ Fragment ", " :: #exp e2' @ [ Fragment ")" ] }
                                                                                 | WordCompare { flip, negate } => let val (x, y) = if flip then
                                                                                                                                        (e2', e1')
                                                                                                                                    else
                                                                                                                                        (e1', e2')
                                                                                                                   in if negate then
                                                                                                                          { prec = 2, exp = Fragment "not __Word_LT(" :: #exp x @ Fragment ", " :: #exp y @ [ Fragment ")" ] }
                                                                                                                      else
                                                                                                                          { prec = ~2, exp = Fragment "__Word_LT(" :: #exp x @ Fragment ", " :: #exp y @ [ Fragment ")" ] }
                                                                                                                   end
                                                                   in if pure then
                                                                          putPureTo ctx env dest (stmts, e)
                                                                      else
                                                                          putImpureTo ctx env dest (stmts, e)
                                                                   end
                                                               )
                                  | NONE => if USyntax.eqVId(vid, InitialEnv.VId_Lua_sub) then
                                                wrap (fn (stmts, e1', e2') =>
                                                         let val e = { prec = ~1, exp = paren ~1 e1' @ Fragment "[" :: #exp e2' @ [ Fragment "]" ] }
                                                         in putImpureTo ctx env dest (stmts, e)
                                                         end
                                                     )
                                            else
                                                NONE
                             end
                           | _ => NONE
          val doUnary = case exp1 of
                            F.VarExp(Syntax.MkQualified([], vid)) =>
                            let fun wrap f = SOME (fn () => doExpCont ctx env exp2 f)
                                open InitialEnv
                            in if USyntax.eqVId(vid, VId_Real_TILDE) orelse USyntax.eqVId(vid, VId_Word_TILDE) orelse USyntax.eqVId(vid, VId_Lua_unm) then
                                   wrap (fn (stmts, e2') => putPureTo ctx env dest (stmts, { prec = 2, exp = Fragment "- " :: paren 2 e2' }))
                               else if USyntax.eqVId(vid, VId_Bool_not) orelse USyntax.eqVId(vid, VId_Lua_isFalsy) then
                                   wrap (fn (stmts, e2') => putPureTo ctx env dest (stmts, { prec = 2, exp = Fragment "not " :: paren 2 e2' }))
                               else if USyntax.eqVId(vid, VId_EXCLAM) then
                                   wrap (fn (stmts, e2') => putImpureTo ctx env dest (stmts, { prec = ~1, exp = paren ~1 e2' @ [ Fragment ".payload" ] }))
                               else if USyntax.eqVId(vid, VId_String_size) orelse USyntax.eqVId(vid, VId_Lua_length) then
                                   wrap (fn (stmts, e2') => putPureTo ctx env dest (stmts, { prec = 2, exp = Fragment "#" :: paren 2 e2' }))
                               else if USyntax.eqVId(vid, VId_Lua_isNil) then
                                   wrap (fn (stmts, e2') => putPureTo ctx env dest (stmts, { prec = 10, exp = paren 10 e2' @ [ Fragment " == nil" ] }))
                               else if USyntax.eqVId(vid, VId_Lua_notb) then
                                   wrap (fn (stmts, e2') => putPureTo ctx env dest (stmts, { prec = 2, exp = Fragment "~ " :: paren 2 e2' }))
                               else
                                   NONE
                            end
                          | _ => NONE
          val doLuaCall = case (exp1, exp2) of
                              (F.AppExp(F.VarExp(Syntax.MkQualified([], vid_luacall)), f), F.VectorExp(xs, _)) =>
                              if USyntax.eqVId(vid_luacall, InitialEnv.VId_Lua_call) then
                                  SOME (fn () => doExpCont ctx env f
                                                           (fn (stmts1, f) =>
                                                               mapCont (fn (e, cont) => doExpCont ctx env e cont)
                                                                       (Vector.foldr (op ::) [] xs)
                                                                       (fn ys => let val stmts2 = List.foldr (fn ((x, _), acc) => x @ acc) [] ys
                                                                                     val zs = List.map (#exp o #2) ys
                                                                                 in case dest of
                                                                                        Discard => putImpureTo ctx env dest (stmts1 @ stmts2, { prec = ~2, exp = paren ~1 f @ Fragment "(" :: commaSep zs @ [ Fragment ")" ] })
                                                                                      | _ => putImpureTo ctx env dest (stmts1 @ stmts2, { prec = ~2, exp = Fragment "table.pack(" :: paren ~1 f @ Fragment "(" :: commaSep zs @ [ Fragment "))" ] })
                                                                                 end
                                                                       )
                                                           )
                                       )
                              else
                                  NONE
                            | _ => NONE
          val doLuaMethod = case (exp1, exp2) of
                                (F.AppExp(F.VarExp(Syntax.MkQualified([], vid_luamethod)), F.RecordExp [(Syntax.NumericLabel 1, self), (Syntax.NumericLabel 2, F.SConExp(Syntax.StringConstant method))]), F.VectorExp(xs, _)) =>
                                if USyntax.eqVId(vid_luamethod, InitialEnv.VId_Lua_method) andalso isLuaIdentifier method then
                                    SOME (fn () => doExpCont ctx env self
                                                             (fn (stmts1, self) =>
                                                                 mapCont (fn (e, cont) => doExpCont ctx env e cont)
                                                                         (Vector.foldr (op ::) [] xs)
                                                                         (fn ys => let val stmts2 = List.foldr (fn ((x, _), acc) => x @ acc) [] ys
                                                                                       val zs = List.map (#exp o #2) ys
                                                                                   in case dest of
                                                                                          Discard => putImpureTo ctx env dest (stmts1 @ stmts2, { prec = ~2, exp = paren ~1 self @ Fragment (":" ^ method ^ "(") :: commaSep zs @ [ Fragment ")" ] })
                                                                                        | _ => putImpureTo ctx env dest (stmts1 @ stmts2, { prec = ~2, exp = Fragment "table.pack(" :: paren ~1 self @ Fragment (":" ^ method ^ "(") :: commaSep zs @ [ Fragment "))" ] })
                                                                                   end
                                                                         )
                                                             )
                                         )
                                else
                                    NONE
                              | _ => NONE
          val isNoop = case exp1 of
                           F.VarExp(Syntax.MkQualified([], vid)) => USyntax.eqVId(vid, InitialEnv.VId_String_str)
                         | F.TyAppExp(F.VarExp(Syntax.MkQualified([], vid)), _) => USyntax.eqVId(vid, InitialEnv.VId_Lua_unsafeToValue) orelse USyntax.eqVId(vid, InitialEnv.VId_Lua_unsafeFromValue) orelse USyntax.eqVId(vid, InitialEnv.VId_assumePure) orelse USyntax.eqVId(vid, InitialEnv.VId_assumeDiscardable)
                         | _ => false
      in case List.mapPartial (fn x => x) [doProjection, doBinary, doUnary, doLuaCall, doLuaMethod] of
             f :: _ => f ()
           | [] => if isNoop then
                       doExpTo ctx env exp2 dest
                   else
                       doExpCont ctx env exp1
                                 (fn (stmts1, e1') =>
                                     doExpCont ctx env exp2
                                               (fn (stmts2, e2') =>
                                                   putImpureTo ctx env dest (stmts1 @ stmts2, { prec = ~2, exp = paren ~1 e1' @ Fragment "(" :: #exp e2' @ [ Fragment ")" ] })
                                               )
                                 )
      end
  | doExpTo ctx env (F.HandleExp { body, exnName, handler }) dest
    = let val status = genSym ctx
          val result = genSym ctx
          val stmts = [ Indent, Fragment ("local " ^ status ^ ", " ^ result ^ " = pcall(function()"), LineTerminator, IncreaseIndent ]
                      @ doExpTo ctx env body Return
                      @ [ DecreaseIndent, Indent, Fragment "end)", OptSemicolon
                        , Indent, Fragment ("if not " ^ status ^ " then"), LineTerminator, IncreaseIndent
                        , Indent, Fragment ("local " ^ VIdToLua exnName ^ " = " ^ result), OptSemicolon
                        ]
                      @ doExpTo ctx env handler (AssignTo result) (* TODO: tail call *)
                      @ [ DecreaseIndent, Indent, Fragment "end", LineTerminator
                        ]
      in putPureTo ctx env dest (stmts, { prec = ~1, exp = [ Fragment result ] })
      end
  | doExpTo ctx env (F.RaiseExp (span as { start = { file, line, column }, ... }, exp)) dest
    = doExpCont ctx env exp
                (fn (stmts, exp') =>
                    case dest of
                        Continue cont => cont (stmts @ [Indent, Fragment "_raise(" ] @ #exp exp' @ [ Fragment (", " ^ toLuaStringLit (OS.Path.file file) ^ ", " ^ Int.toString line ^ ", " ^ Int.toString column ^ ")"), OptSemicolon ], { prec = 0, exp = [ Fragment "nil" ] })
                      | _ => stmts @ [Indent, Fragment "_raise(" ] @ #exp exp' @ [ Fragment (", " ^ toLuaStringLit (OS.Path.file file) ^ ", " ^ Int.toString line ^ ", " ^ Int.toString column ^ ")"), OptSemicolon ]
                )
  | doExpTo ctx env (F.IfThenElseExp (exp1, exp2, exp3)) dest
    = doExpCont ctx env exp1
                (fn (stmts1, exp1') =>
                    let fun doElseIf env (F.IfThenElseExp(e1, e2, e3)) dest'
                            = doExpCont ctx env e1
                                        (fn (s1, e1') =>
                                            if List.null s1 then
                                                [ Indent, Fragment "elseif " ] @ #exp e1' @ [ Fragment " then", LineTerminator, IncreaseIndent ]
                                                @ doExpTo ctx env e2 dest'
                                                @ [ DecreaseIndent ]
                                                @ doElseIf env e3 dest'
                                            else
                                                [ Indent, Fragment "else", LineTerminator, IncreaseIndent ]
                                                @ s1
                                                @ [ Indent, Fragment "if " ] @ #exp e1' @ [ Fragment " then", LineTerminator, IncreaseIndent ]
                                                @ doExpTo ctx env e2 dest'
                                                @ [ DecreaseIndent ]
                                                @ doElseIf env e3 dest'
                                                @ [ Indent, Fragment "end", LineTerminator, DecreaseIndent ]
                                        )
                          | doElseIf env e dest' = [ Indent, Fragment "else", LineTerminator, IncreaseIndent ]
                                                   @ doExpTo ctx env e dest'
                                                   @ [ DecreaseIndent ]
                    in case dest of
                           Continue cont => let val result = genSym ctx
                                            in cont (stmts1
                                                     @ [ Indent, Fragment ("local " ^ result), LineTerminator
                                                       , Indent, Fragment "if " ] @ #exp exp1' @ [ Fragment " then", LineTerminator, IncreaseIndent ]
                                                     @ doExpTo ctx env exp2 (AssignTo result)
                                                     @ [ DecreaseIndent ]
                                                     @ doElseIf env exp3 (AssignTo result)
                                                     @ [ Indent, Fragment "end", LineTerminator ]
                                                    , { prec = ~1, exp = [ Fragment result ] })
                                            end
                         | _ => stmts1
                                @ [ Indent, Fragment "if " ] @ #exp exp1' @ [ Fragment " then", LineTerminator, IncreaseIndent ]
                                @ doExpTo ctx env exp2 dest
                                @ [ DecreaseIndent ]
                                @ doElseIf env exp3 dest
                                @ [ Indent, Fragment "end", LineTerminator ]
                    end
              )
  | doExpTo ctx env (F.CaseExp _) dest = raise Fail "Lua codegen: CaseExp should have been desugared earlier"
  | doExpTo ctx env (F.FnExp (vid, _, exp)) dest = putPureTo ctx env dest ([], { prec = 0, exp = [ Fragment ("function(" ^ VIdToLua vid ^ ")"), LineTerminator, IncreaseIndent ] @ doExpTo ctx env exp Return @ [ DecreaseIndent, Indent, Fragment "end" ] }) (* TODO: update environment *)
  | doExpTo ctx env (F.ProjectionExp { label, ... }) dest = putPureTo ctx env dest ([], { prec = 0, exp = [ Fragment ("function(x) return x[" ^ LabelToLua label ^ "] end") ] })
  | doExpTo ctx env (F.ListExp (xs, _)) dest
    = if Vector.length xs = 0 then
          putPureTo ctx env dest ([], { prec = ~1, exp = [ Fragment "_nil" ] })
      else
          mapCont (fn (e, cont) => doExpCont ctx env e cont)
                  (Vector.foldr (op ::) [] xs)
                  (fn ys => let val stmts = List.foldr (fn ((x, _), acc) => x @ acc) [] ys
                            in putPureTo ctx env dest (stmts, { prec = ~2, exp = Fragment ("_list{ n = " ^ Int.toString (Vector.length xs)) :: List.foldr (fn ((_, y), acc) => Fragment ", " :: #exp y @ acc) [ Fragment " }" ] ys })
                            end
                  )
  | doExpTo ctx env (F.VectorExp (xs, _)) dest
    = mapCont (fn (e, cont) => doExpCont ctx env e cont)
              (Vector.foldr (op ::) [] xs)
              (fn ys => let val stmts = List.foldr (fn ((x, _), acc) => x @ acc) [] ys
                        in putPureTo ctx env dest (stmts, { prec = ~1, exp = Fragment ("{ n = " ^ Int.toString (Vector.length xs)) :: List.foldr (fn ((_, y), acc) => Fragment ", " :: #exp y @ acc) [ Fragment " }" ] ys })
                        end
              )
  | doExpTo ctx env (F.TyAbsExp (_, exp)) dest = doExpTo ctx env exp dest
  | doExpTo ctx env (F.TyAppExp (exp, _)) dest = doExpTo ctx env exp dest
  | doExpTo ctx env (F.RecordEqualityExp fields) dest
    = mapCont (fn ((label, exp), cont) => doExpCont ctx env exp (fn (stmts, exp') => cont (stmts, (label, exp'))))
              fields
              (fn ys => let val (stmts, fields') = ListPair.unzip ys
                        in putPureTo ctx env dest (List.concat stmts
                                                  , case Syntax.extractTuple(1, fields) of
                                                        SOME xs => { prec = ~2, exp = Fragment "_Record_EQUAL({" :: commaSep (List.map (#exp o #2) fields') @ [ Fragment "})" ] }
                                                      | NONE => { prec = ~2, exp = Fragment "_Record_EQUAL({" :: commaSep (List.map (fn (label, v) => Fragment ("[" ^ LabelToLua label ^ "] = ") :: #exp v) fields') @ [ Fragment "})" ] }
                                                  )
                        end
              )
  | doExpTo ctx env (F.DataTagExp exp) dest = doExpCont ctx env exp (fn (stmts, exp') => putPureTo ctx env dest (stmts, { prec = ~1, exp = paren ~1 exp' @ [ Fragment ".tag" ] }))
  | doExpTo ctx env (F.DataPayloadExp exp) dest = doExpCont ctx env exp (fn (stmts, exp') => putPureTo ctx env dest (stmts, { prec = ~1, exp = paren ~1 exp' @ [ Fragment ".payload" ] }))

(* doDec : Context -> Env -> F.Dec -> string *)
and doDec ctx env (F.ValDec (F.SimpleBind(v, _, exp)))
    = let val luavid = VIdToLua v
      in [ Indent, Fragment ("local " ^ luavid), LineTerminator ] @ doExpTo ctx env exp (AssignTo luavid)
      end
  | doDec ctx env (F.ValDec (F.TupleBind([], exp)))
    = doExpTo ctx env exp Discard
  | doDec ctx env (F.ValDec (F.TupleBind(vars, exp)))
    = let val vars' = List.map (fn (v,_) => VIdToLua v) vars
          val decs = [ Indent, Fragment ("local " ^ String.concatWith ", " vars'), LineTerminator ]
      in decs @ doExpTo ctx env exp (UnpackingAssignTo vars')
      end
  | doDec ctx env (F.RecValDec valbinds)
    = let val (decs, assignments) = ListPair.unzip (List.map (fn F.SimpleBind (v,_,exp) => let val v' = VIdToLua v
                                                                                           in ([ Indent, Fragment ("local " ^ v'), LineTerminator ], doExpTo ctx env exp (AssignTo v'))
                                                                                           end
                                                             | F.TupleBind ([], exp) => ([], doExpTo ctx env exp Discard)
                                                             | F.TupleBind (vars, exp) => let val vars' = List.map (fn (v,_) => VIdToLua v) vars
                                                                                          in ([ Indent, Fragment ("local " ^ String.concatWith ", " vars'), LineTerminator ], doExpTo ctx env exp (UnpackingAssignTo vars'))
                                                                                          end
                                                             ) valbinds)
      in List.foldr (op @) (List.concat assignments) decs
      end
  | doDec ctx env (F.IgnoreDec exp) = doExpTo ctx env exp Discard
  | doDec ctx env (F.DatatypeDec datbinds) = List.concat (List.map (doDatBind ctx env) datbinds)
  | doDec ctx env (F.ExceptionDec { conName as USyntax.MkVId(name, _), tagName, payloadTy })
    = let val conName' = VIdToLua conName
          val tagName' = VIdToLua tagName
      in [ Indent, Fragment ("local " ^ tagName' ^ " = { " ^ toLuaStringLit name ^ " }"), LineTerminator ]
         @ (case payloadTy of
                NONE => [ Indent, Fragment ("local " ^ conName' ^ " = { tag = " ^ tagName' ^ " }"), LineTerminator ]
              | SOME _ => [ Indent, Fragment ("local function " ^ conName' ^ "(payload)"), LineTerminator, IncreaseIndent
                          , Indent, Fragment ("return { tag = " ^ tagName' ^ ", payload = payload }"), LineTerminator
                          , DecreaseIndent, Indent, Fragment "end", LineTerminator
                          ]
           )
      end
  | doDec ctx env (F.ExportValue _) = raise Fail "internal error: ExportValue must be the last statement"
  | doDec ctx env (F.ExportModule _) = raise Fail "internal error: ExportModule must be the last statement"
and doDatBind ctx env (F.DatBind (tyvars, tycon, conbinds)) = List.concat (List.map (doConBind ctx env) conbinds) (* TODO: equality *)
and doConBind ctx env (F.ConBind (vid as USyntax.MkVId(name,_), NONE)) = [ Indent, Fragment ("local " ^ VIdToLua vid ^ " = { tag = " ^ toLuaStringLit name ^ " }"), LineTerminator ]
  | doConBind ctx env (F.ConBind (vid as USyntax.MkVId(name,_), SOME ty)) = [ Indent, Fragment ("local function " ^ VIdToLua vid ^ "(x)"), LineTerminator, IncreaseIndent
                                                                            , Indent, Fragment ("return { tag = " ^ toLuaStringLit name ^ ", payload = x }"), LineTerminator
                                                                            , DecreaseIndent, Indent, Fragment "end", LineTerminator
                                                                            ]

fun doDecs ctx env [F.ExportValue exp] = doExpTo ctx env exp Return
  | doDecs ctx env [F.ExportModule fields] = mapCont (fn ((label, exp), cont) => doExpCont ctx env exp (fn (stmts, e) => cont (stmts, (label, e))))
                                                     (Vector.foldr (op ::) [] fields)
                                                     (fn ys => let val (stmts, fields') = ListPair.unzip ys
                                                               in putPureTo ctx env Return ( List.concat stmts
                                                                                           , { prec = 0, exp = [ Fragment "{" ] @ commaSep (List.map (fn (label, e) => [ Fragment (if isLuaIdentifier label then
                                                                                                                                                                                       label ^ " = "
                                                                                                                                                                                   else
                                                                                                                                                                                       "[" ^ toLuaStringLit label ^ "] = "
                                                                                                                                                                       ) ] @ #exp e) fields') @ [ Fragment "}" ] }
                                                                                           )
                                                               end
                                                     )
  | doDecs ctx env (dec :: decs) = doDec ctx env dec @ doDecs ctx env decs
  | doDecs ctx env [] = []

fun doProgram ctx env decs = buildProgram (doDecs ctx env decs)

end (* structure CodeGenLua *)
