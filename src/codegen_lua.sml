(*
 * Copyright (c) 2021 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure CodeGenLua = struct
exception CodeGenError of string
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
          val map = List.foldl USyntax.LongVIdMap.insert' USyntax.LongVIdMap.empty
                               [(USyntax.MkShortVId (FSyntax.strIdToVId StrId_General), NONE)
                               ,(USyntax.MkShortVId (FSyntax.strIdToVId StrId_Bool), NONE)
                               ,(USyntax.MkShortVId (FSyntax.strIdToVId StrId_Int), NONE)
                               ,(USyntax.MkShortVId (FSyntax.strIdToVId StrId_Word), NONE)
                               ,(USyntax.MkShortVId (FSyntax.strIdToVId StrId_Real), NONE)
                               ,(USyntax.MkShortVId (FSyntax.strIdToVId StrId_String), NONE)
                               ,(USyntax.MkShortVId (FSyntax.strIdToVId StrId_Char), NONE)
                               ,(USyntax.MkShortVId (FSyntax.strIdToVId StrId_Vector), NONE)
                               ,(USyntax.MkShortVId (FSyntax.strIdToVId StrId_Array), NONE)
                               ,(USyntax.MkShortVId (FSyntax.strIdToVId StrId_Lua), NONE)
                               ,(USyntax.MkShortVId (FSyntax.strIdToVId StrId_LunarML), NONE)
                               ]
      in List.foldl (fn ((vid, name), map) => USyntax.LongVIdMap.insert (map, vid, SOME name)) map
                    [(* ref *)
                     (LongVId_ref, "_ref")
                    ,(VId_COLONEQUAL, "_set")
                    ,(VId_EXCLAM, "_read")
                    (* boolean *)
                    ,(LongVId_true, "true") (* boolean literal *)
                    ,(LongVId_false, "false") (* boolean literal *)
                    ,(VId_Bool_not, "_not") (* Lua not *)
                    (* list *)
                    ,(LongVId_nil, "_nil")
                    ,(LongVId_DCOLON, "_cons")
                    (* exn *)
                    ,(LongVId_Match, "_Match")
                    ,(LongVId_Bind, "_Bind")
                    ,(LongVId_Div, "_Div")
                    ,(LongVId_Overflow, "_Overflow")
                    ,(LongVId_Size, "_Size")
                    ,(LongVId_Subscript, "_Subscript")
                    ,(LongVId_Fail, "_Fail")
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
                    (* int *)
                    ,(VId_Int_abs, "_Int_abs") (* may raise Overflow *)
                    ,(VId_Int_TILDE, "_Int_negate") (* may raise Overflow *)
                    ,(VId_Int_LT, "_LT")
                    ,(VId_Int_GT, "_GT")
                    ,(VId_Int_LE, "_LE")
                    ,(VId_Int_GE, "_GE")
                    ,(USyntax.MkShortVId VId_Int_add_bin, "__Int_add")
                    ,(USyntax.MkShortVId VId_Int_sub_bin, "__Int_sub")
                    ,(USyntax.MkShortVId VId_Int_mul_bin, "__Int_mul")
                    ,(USyntax.MkShortVId VId_Int_div_bin, "__Int_div")
                    ,(USyntax.MkShortVId VId_Int_mod_bin, "__Int_mod")
                    (* word *)
                    ,(VId_Word_PLUS, "_PLUS") (* Lua +; does not raise Overflow *)
                    ,(VId_Word_MINUS, "_MINUS") (* Lua - (binary); does not raise Overflow *)
                    ,(VId_Word_TIMES, "_TIMES") (* Lua *; does not raise Overflow *)
                    ,(VId_Word_TILDE, "_unm") (* Lua - (unary) *)
                    ,(USyntax.MkShortVId VId_Word_div_bin, "__Word_div")
                    ,(USyntax.MkShortVId VId_Word_mod_bin, "__Word_mod")
                    ,(USyntax.MkShortVId VId_Word_LT_bin, "__Word_LT")
                    (* real *)
                    ,(VId_Real_PLUS, "_PLUS") (* Lua + *)
                    ,(VId_Real_MINUS, "_MINUS") (* Lua - (binary) *)
                    ,(VId_Real_TIMES, "_TIMES") (* Lua * *)
                    ,(VId_Real_DIVIDE, "_DIVIDE") (* Lua / *)
                    ,(VId_Real_abs, "math_abs") (* Lua math.abs *)
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
                    ,(VId_Vector_tabulate, "_VectorOrArray_tabulate")
                    ,(VId_Vector_length, "_VectorOrArray_length")
                    ,(VId_Vector_sub, "_VectorOrArray_sub")
                    ,(VId_Vector_concat, "_Vector_concat")
                    ,(USyntax.MkShortVId VId_Vector_fromList, "_VectorOrArray_fromList")
                    (* Lua interface *)
                    ,(VId_Lua_global, "_Lua_global")
                    ,(VId_Lua_call, "_Lua_call")
                    ,(VId_Lua_method, "_Lua_method")
                    ,(VId_Lua_NIL, "nil") (* literal *)
                    ,(VId_Lua_unsafeToValue, "_id") (* no-op *)
                    ,(VId_Lua_unsafeFromValue, "_id") (* no-op *)
                    ,(VId_Lua_newTable, "_Lua_newTable")
                    ,(VId_Lua_function, "_Lua_function")
                    (* extra *)
                    ,(VId_assumePure, "_id") (* no-op *)
                    ,(VId_assumeDiscardable, "_id") (* no-op *)
                    ,(VId_Lua_Lib_assert, "assert")
                    ,(VId_Lua_Lib_error, "error")
                    ,(VId_Lua_Lib_getmetatable, "getmetatable")
                    ,(VId_Lua_Lib_pairs, "pairs")
                    ,(VId_Lua_Lib_pcall, "pcall")
                    ,(VId_Lua_Lib_setmetatable, "setmetatable")
                    ,(VId_Lua_Lib_math, "math")
                    ,(VId_Lua_Lib_math_abs, "math_abs")
                    ,(VId_Lua_Lib_math_type, "math_type")
                    ,(VId_Lua_Lib_math_maxinteger, "math_maxinteger")
                    ,(VId_Lua_Lib_math_mininteger, "math_mininteger")
                    ,(VId_Lua_Lib_string, "string")
                    ,(VId_Lua_Lib_string_format, "string_format")
                    ,(VId_Lua_Lib_table, "table")
                    ,(VId_Lua_Lib_table_pack, "table_pack")
                    ,(VId_Lua_Lib_table_unpack, "table_unpack")
                    ]
      end
datatype BinaryOp = InfixOp of (* prec *) int * string
                  | InfixOpR of (* prec *) int * string
val builtinBinaryOps : (BinaryOp * (* pure? *) bool) USyntax.LongVIdMap.map
    = let open InitialEnv
      in List.foldl USyntax.LongVIdMap.insert' USyntax.LongVIdMap.empty
                    [(VId_EQUAL_bool,   (InfixOp (10, "=="), true))
                    ,(VId_EQUAL_int,    (InfixOp (10, "=="), true))
                    ,(VId_EQUAL_word,   (InfixOp (10, "=="), true))
                    ,(VId_EQUAL_string, (InfixOp (10, "=="), true))
                    ,(VId_EQUAL_char,   (InfixOp (10, "=="), true))
                    ,(VId_Int_LT,       (InfixOp (10, "<"), true))
                    ,(VId_Int_LE,       (InfixOp (10, "<="), true))
                    ,(VId_Int_GT,       (InfixOp (10, ">"), true))
                    ,(VId_Int_GE,       (InfixOp (10, ">="), true))
                    ,(VId_Word_PLUS,    (InfixOp (4, "+"), true))
                    ,(VId_Word_MINUS,   (InfixOp (4, "-"), true))
                    ,(VId_Word_TIMES,   (InfixOp (3, "*"), true))
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
                    ]
      end
fun VIdToLua(vid as USyntax.MkVId(name, n)) = if n < 0 then
                                                  case USyntax.LongVIdMap.find (builtins, USyntax.MkShortVId vid) of
                                                      NONE => raise Fail ("Unknown built-in symbol: " ^ name ^ "@" ^ Int.toString n)
                                                    | SOME (SOME luaExpr) => luaExpr
                                                    | SOME NONE => raise CodeGenError ("the built-in identifier " ^ USyntax.print_VId vid ^ " has no runtime counterpart")
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
type Env = { boundSymbols : StringSet.set, hoistedSymbols : StringSet.set }
val initialEnv : Env = { boundSymbols = StringSet.fromList
                                            [ "_Unit_EQUAL"
                                            , "_Record_EQUAL"
                                            , "_EQUAL"
                                            , "_LT"
                                            , "_GT"
                                            , "_LE"
                                            , "_GE"
                                            , "_PLUS"
                                            , "_MINUS"
                                            , "_TIMES"
                                            , "_DIVIDE"
                                            , "_pow"
                                            , "_unm"
                                            , "_andb"
                                            , "_orb"
                                            , "_xorb"
                                            , "_notb"
                                            , "_LSHIFT"
                                            , "_RSHIFT"
                                            , "_concat"
                                            , "_length"
                                            , "_not"
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
                                            , "_raise"
                                            , "__Int_add"
                                            , "__Int_sub"
                                            , "__Int_mul"
                                            , "__Int_div"
                                            , "__Int_mod"
                                            , "_Int_negate"
                                            , "_Int_abs"
                                            , "__Word_div"
                                            , "__Word_mod"
                                            , "__Word_LT"
                                            , "_nil"
                                            , "_cons"
                                            , "_List_EQUAL"
                                            , "_list"
                                            , "_ref"
                                            , "_set"
                                            , "_read"
                                            , "_Array_array"
                                            , "_VectorOrArray_fromList"
                                            , "_VectorOrArray_length"
                                            , "_VectorOrArray_sub"
                                            , "_Vector_Array"
                                            , "_EQUAL_vector"
                                            , "_Lua_global"
                                            , "_Lua_call"
                                            , "_Lua_method"
                                            , "_Lua_newTable"
                                            , "_Lua_function"
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
                                            , "string"
                                            , "string_format"
                                            , "table"
                                            , "table_pack"
                                            , "table_unpack"
                                            ]
                       , hoistedSymbols = StringSet.empty
                       }
fun addSymbol ({ boundSymbols, hoistedSymbols } : Env, s)
    = { boundSymbols = StringSet.add (boundSymbols, s)
      , hoistedSymbols = hoistedSymbols
      }
fun addHoistedSymbol ({ boundSymbols, hoistedSymbols } : Env, s)
    = { boundSymbols = StringSet.add (boundSymbols, s)
      , hoistedSymbols = StringSet.add (hoistedSymbols, s)
      }
fun isHoisted ({ hoistedSymbols, ... } : Env, s)
    = StringSet.member (hoistedSymbols, s)
fun declareIfNotHoisted (env : Env, vars)
    = let val (env, vars) = List.foldr (fn (v, (env, xs)) => if isHoisted (env, v) then
                                                                 (env, xs)
                                                             else
                                                                 (addHoistedSymbol (env, v), v :: xs)) (env, []) vars
      in (env, case vars of
                   [] => []
                 | _ => [ Indent, Fragment ("local " ^ String.concatWith ", " vars), LineTerminator ]
         )
      end

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
                     | Continue of (* statements *) Fragment list * Env * (* pure expression *) Exp -> Fragment list (* the continuation should be called exactly once, and the expression should be used only once *)

(* mapCont : ('a * ('b -> 'r) -> 'r) -> 'a list -> ('b list -> 'r) -> 'r *)
fun mapCont f [] cont = cont []
  | mapCont f (x :: xs) cont = f (x, fn y => mapCont f xs (fn ys => cont (y :: ys)))

fun commaSep ([] : (Fragment list) list) : Fragment list = []
  | commaSep (x :: xs) = x @ commaSep1 xs
and commaSep1 [] = []
  | commaSep1 (x :: xs) = Fragment ", " :: x @ commaSep1 xs

local
fun extractStrId(F.VarExp(USyntax.MkVId(name, n))) = SOME (USyntax.MkStrId(name, n), [])
  | extractStrId(F.SProjectionExp(exp, F.StructLabel strid)) = (case extractStrId exp of
                                                              SOME (strid0, revStrids) => SOME (strid0, strid :: revStrids)
                                                            | NONE => NONE
                                                         )
  | extractStrId _ = NONE
in
fun extractLongVId(F.VarExp(vid)) = SOME (USyntax.MkShortVId vid)
  | extractLongVId(F.SProjectionExp(exp, F.ValueLabel vid)) = Option.map (fn (strid0, revStrids) => USyntax.MkLongVId(strid0, List.rev revStrids, vid)) (extractStrId exp)
  | extractLongVId _ = NONE
end

(* doExpTo : Context -> Env -> F.Exp -> Destination -> Line list *)
fun putPureTo ctx env Return (stmts, exp : Exp) = stmts @ [ Indent, Fragment "return " ] @ #exp exp @ [ OptSemicolon ]
  | putPureTo ctx env (AssignTo v) (stmts, exp) = stmts @ [ Indent, Fragment (v ^ " = ") ] @ #exp exp @ [ OptSemicolon ]
  | putPureTo ctx env (UnpackingAssignTo v) (stmts, exp) = stmts @ [ Indent, Fragment (String.concatWith ", " v ^ " = table_unpack(") ] @ #exp exp @ [ Fragment (", 1, " ^ Int.toString (List.length v) ^ ")"), OptSemicolon ]
  | putPureTo ctx env Discard (stmts, exp) = stmts
  | putPureTo ctx env (Continue cont) (stmts, exp) = cont (stmts, env, exp)
and putImpureTo ctx env Return (stmts, exp : Exp) = stmts @ [ Indent, Fragment "return " ] @ #exp exp @ [ OptSemicolon ]
  | putImpureTo ctx env (AssignTo v) (stmts, exp) = stmts @ [ Indent, Fragment (v ^ " = ") ] @ #exp exp @ [ OptSemicolon ]
  | putImpureTo ctx env (UnpackingAssignTo v) (stmts, exp) = stmts @ [ Indent, Fragment (String.concatWith ", " v ^ " = table_unpack(") ] @ #exp exp @ [ Fragment (", 1, " ^ Int.toString (List.length v) ^ ")"), OptSemicolon ]
  | putImpureTo ctx env Discard (stmts, exp) = stmts @ (if #prec exp = ~2 then
                                                            Indent :: #exp exp @ [ OptSemicolon ]
                                                        else
                                                            [ Indent, Fragment "_id(" ] @ #exp exp @ [ Fragment ")", OptSemicolon ]
                                                       )
  | putImpureTo ctx env (Continue cont) (stmts, exp) = let val dest = genSym ctx
                                                           val env = addSymbol (env, dest)
                                                       in cont (stmts @ [ Indent, Fragment ("local " ^ dest ^ " = ") ] @ #exp exp @ [ OptSemicolon ], env, { prec = ~1, exp = [ Fragment dest ] })
                                                       end
and doExpCont ctx env exp (cont : Fragment list * Env * Exp -> Fragment list)  = doExpTo ctx env exp (Continue cont)
and doExpTo ctx env (F.PrimExp (F.SConOp scon, _, xs)) dest : Fragment list = if Vector.length xs = 0 then
                                                                                  putPureTo ctx env dest ([], doLiteral scon)
                                                                              else
                                                                                  raise CodeGenError "PrimExp.SConOp: non-empty argument"
  | doExpTo ctx env (F.VarExp vid) dest = putPureTo ctx env dest ([], { prec = ~1, exp = [ Fragment (VIdToLua vid) ] }) (* TODO: prec for true, false, nil *)
  | doExpTo ctx env (F.RecordExp []) dest = putPureTo ctx env dest ([], { prec = ~1, exp = [ Fragment "nil" ] })
  | doExpTo ctx env (F.RecordExp fields) Discard = List.concat (List.map (fn (_, exp) => doExpTo ctx env exp Discard) fields)
  | doExpTo ctx env (F.RecordExp fields) dest
    = mapCont (fn ((label, exp), cont) => doExpCont ctx env exp (fn (stmts, env, e) => cont (stmts, (label, e))))
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
      in doExpCont ctx env exp (fn (stmts, env, exp) => cont (dec' @ stmts, env, exp)) (* TODO: modify environment *)
      end
  | doExpTo ctx env (F.LetExp (dec, exp)) dest
    = let val dec' = doDec ctx env dec
      in dec' @ doExpTo ctx env exp dest
      end
  | doExpTo ctx env (F.AppExp (exp1, exp2)) dest
    = let val doBinary = case (extractLongVId exp1, exp2) of
                             (SOME longvid, F.RecordExp [(Syntax.NumericLabel 1, e1), (Syntax.NumericLabel 2, e2)]) =>
                             let fun wrap f = SOME (fn () => doExpCont ctx env e1 (fn (stmts1, env, e1') => doExpCont ctx env e2 (fn (stmts2, env, e2') => f (stmts1 @ stmts2, env, e1', e2'))))
                             in case USyntax.LongVIdMap.find(builtinBinaryOps, longvid) of
                                    SOME (binop, pure) => wrap (fn (stmts, env, e1', e2') =>
                                                                   let val e = case binop of
                                                                                   InfixOp (prec, luaop) => { prec = prec, exp = paren prec e1' @ Fragment (" " ^ luaop ^ " ") :: paren (prec + 1) e2' }
                                                                                 | InfixOpR (prec, luaop) => { prec = prec, exp = paren (prec + 1) e1' @ Fragment (" " ^ luaop ^ " ") :: paren prec e2' }
                                                                   in if pure then
                                                                          putPureTo ctx env dest (stmts, e)
                                                                      else
                                                                          putImpureTo ctx env dest (stmts, e)
                                                                   end
                                                               )
                                  | NONE => NONE
                             end
                           | _ => NONE
          val doUnary = case extractLongVId exp1 of
                            SOME vid =>
                            let fun wrap f = SOME (fn () => doExpCont ctx env exp2 f)
                                open InitialEnv
                            in if USyntax.eqULongVId(vid, VId_Real_TILDE) orelse USyntax.eqULongVId(vid, VId_Word_TILDE) then
                                   wrap (fn (stmts, env, e2') => putPureTo ctx env dest (stmts, { prec = 2, exp = Fragment "- " :: paren 2 e2' }))
                               else if USyntax.eqULongVId(vid, VId_Bool_not) then
                                   wrap (fn (stmts, env, e2') => putPureTo ctx env dest (stmts, { prec = 2, exp = Fragment "not " :: paren 2 e2' }))
                               else if USyntax.eqULongVId(vid, VId_String_size) then
                                   wrap (fn (stmts, env, e2') => putPureTo ctx env dest (stmts, { prec = 2, exp = Fragment "#" :: paren 2 e2' }))
                               else
                                   NONE
                            end
                          | _ => NONE
          val doPolymorphicUnary = case exp1 of
                                       F.TyAppExp (exp1', _) =>
                                       (case extractLongVId exp1' of
                                            SOME vid =>
                                            let fun wrap f = SOME (fn () => doExpCont ctx env exp2 f)
                                                open InitialEnv
                                            in if USyntax.eqULongVId(vid, VId_EXCLAM) then
                                                   wrap (fn (stmts, env, e2') => putImpureTo ctx env dest (stmts, { prec = ~1, exp = paren ~1 e2' @ [ Fragment ".payload" ] }))
                                               else if USyntax.eqULongVId(vid, VId_Vector_length) orelse USyntax.eqULongVId(vid, VId_Array_length) then
                                                   wrap (fn (stmts, env, e2') => putPureTo ctx env dest (stmts, { prec = 2, exp = paren ~1 e2' @ [ Fragment ".n" ] }))
                                               else
                                                   NONE
                                            end
                                          | NONE => NONE
                                       )
                                     | _ => NONE
          val doLuaCall = case (exp1, exp2) of
                              (F.AppExp(vid_luacall, f), F.PrimExp(F.VectorOp, _, xs)) =>
                              if F.isLongVId(vid_luacall, InitialEnv.VId_Lua_call) then
                                  SOME (fn () => doExpCont ctx env f
                                                           (fn (stmts1, env, f) =>
                                                               mapCont (fn (e, cont) => doExpCont ctx env e (fn (x, _, e) => cont (x, e)))
                                                                       (Vector.foldr (op ::) [] xs)
                                                                       (fn ys => let val stmts2 = List.foldr (fn ((x, _), acc) => x @ acc) [] ys
                                                                                     val zs = List.map (#exp o #2) ys
                                                                                 in case dest of
                                                                                        Discard => putImpureTo ctx env dest (stmts1 @ stmts2, { prec = ~2, exp = paren ~1 f @ Fragment "(" :: commaSep zs @ [ Fragment ")" ] })
                                                                                      | _ => putImpureTo ctx env dest (stmts1 @ stmts2, { prec = ~2, exp = Fragment "table_pack(" :: paren ~1 f @ Fragment "(" :: commaSep zs @ [ Fragment "))" ] })
                                                                                 end
                                                                       )
                                                           )
                                       )
                              else
                                  NONE
                            | _ => NONE
          val doLuaMethod = case (exp1, exp2) of
                                (F.AppExp(vid_luamethod, F.RecordExp [(Syntax.NumericLabel 1, self), (Syntax.NumericLabel 2, F.PrimExp (F.SConOp(Syntax.StringConstant method), _, _))]), F.PrimExp(F.VectorOp, _, xs)) =>
                                if F.isLongVId(vid_luamethod, InitialEnv.VId_Lua_method) andalso isLuaIdentifier method then
                                    SOME (fn () => doExpCont ctx env self
                                                             (fn (stmts1, env, self) =>
                                                                 mapCont (fn (e, cont) => doExpCont ctx env e (fn (x, _, e) => cont (x, e)))
                                                                         (Vector.foldr (op ::) [] xs)
                                                                         (fn ys => let val stmts2 = List.foldr (fn ((x, _), acc) => x @ acc) [] ys
                                                                                       val zs = List.map (#exp o #2) ys
                                                                                   in case dest of
                                                                                          Discard => putImpureTo ctx env dest (stmts1 @ stmts2, { prec = ~2, exp = paren ~1 self @ Fragment (":" ^ method ^ "(") :: commaSep zs @ [ Fragment ")" ] })
                                                                                        | _ => putImpureTo ctx env dest (stmts1 @ stmts2, { prec = ~2, exp = Fragment "table_pack(" :: paren ~1 self @ Fragment (":" ^ method ^ "(") :: commaSep zs @ [ Fragment "))" ] })
                                                                                   end
                                                                         )
                                                             )
                                         )
                                else
                                    NONE
                              | _ => NONE
          (* doTernary = if USyntax.eqVId(vid, VId_Lua_set) then ... *)
          (* doLuaGlobal: VId_Lua_global *)
          val isNoop = case exp1 of
                           F.TyAppExp(vid, _) => F.isLongVId(vid, InitialEnv.VId_Lua_unsafeToValue) orelse F.isLongVId(vid, InitialEnv.VId_Lua_unsafeFromValue) orelse F.isLongVId(vid, InitialEnv.VId_assumePure) orelse F.isLongVId(vid, InitialEnv.VId_assumeDiscardable)
                         | exp1 => F.isLongVId(exp1, InitialEnv.VId_String_str)
      in case List.mapPartial (fn x => x) [doBinary, doUnary, doPolymorphicUnary, doLuaCall, doLuaMethod] of
             f :: _ => f ()
           | [] => if isNoop then
                       doExpTo ctx env exp2 dest
                   else
                       doExpCont ctx env exp1
                                 (fn (stmts1, env, e1') =>
                                     doExpCont ctx env exp2
                                               (fn (stmts2, env, e2') =>
                                                   putImpureTo ctx env dest (stmts1 @ stmts2, { prec = ~2, exp = paren ~1 e1' @ Fragment "(" :: #exp e2' @ [ Fragment ")" ] })
                                               )
                                 )
      end
  | doExpTo ctx env (F.HandleExp { body, exnName, handler }) dest
    = let val status = genSym ctx
          val result = genSym ctx
          val exnName = VIdToLua exnName
          val env' = addSymbol (addSymbol (env, status), result)
          val env'' = addSymbol (env', exnName)
          val stmts = [ Indent, Fragment ("local " ^ status ^ ", " ^ result ^ " = pcall(function()"), LineTerminator, IncreaseIndent ]
                      @ doExpTo ctx env' body Return
                      @ [ DecreaseIndent, Indent, Fragment "end)", OptSemicolon
                        , Indent, Fragment ("if not " ^ status ^ " then"), LineTerminator, IncreaseIndent
                        , Indent, Fragment ("local " ^ exnName ^ " = " ^ result), OptSemicolon
                        ]
                      @ doExpTo ctx env'' handler (AssignTo result) (* TODO: tail call *)
                      @ [ DecreaseIndent, Indent, Fragment "end", LineTerminator
                        ]
      in putPureTo ctx env dest (stmts, { prec = ~1, exp = [ Fragment result ] })
      end
  | doExpTo ctx env (F.PrimExp (F.RaiseOp (span as { start = { file, line, column }, ... }), _, xs)) dest
    = if Vector.length xs = 1 then
          let val exp = Vector.sub (xs, 0)
          in doExpCont ctx env exp
                       (fn (stmts, env, exp') =>
                           case dest of
                               Continue cont => cont (stmts @ [Indent, Fragment "_raise(" ] @ #exp exp' @ [ Fragment (", " ^ toLuaStringLit (OS.Path.file file ^ ":" ^ Int.toString line ^ ":" ^ Int.toString column) ^ ")"), OptSemicolon ], env, { prec = 0, exp = [ Fragment "nil" ] })
                             | _ => stmts @ [Indent, Fragment "_raise(" ] @ #exp exp' @ [ Fragment (", " ^ toLuaStringLit (OS.Path.file file ^ ":" ^ Int.toString line ^ ":" ^ Int.toString column) ^ ")"), OptSemicolon ]
                       )
          end
      else
          raise CodeGenError "PrimExp.RaiseOp: invalid number of arguments"
  | doExpTo ctx env (F.IfThenElseExp (exp1, exp2, exp3)) dest
    = doExpCont ctx env exp1
                (fn (stmts1, env, exp1') =>
                    let fun doElseIf env (F.IfThenElseExp(e1, e2, e3)) dest'
                            = doExpCont ctx env e1
                                        (fn (s1, env, e1') =>
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
                          | doElseIf env e dest' = case doExpTo ctx env e dest' of
                                                       [] => []
                                                     | else' => [ Indent, Fragment "else", LineTerminator, IncreaseIndent ]
                                                                @ else'
                                                                @ [ DecreaseIndent ]
                    in case dest of
                           Continue cont => let val result = genSym ctx
                                                val env' = addSymbol (env, result)
                                            in cont (stmts1
                                                     @ [ Indent, Fragment ("local " ^ result), LineTerminator
                                                       , Indent, Fragment "if " ] @ #exp exp1' @ [ Fragment " then", LineTerminator, IncreaseIndent ]
                                                     @ doExpTo ctx env' exp2 (AssignTo result)
                                                     @ [ DecreaseIndent ]
                                                     @ doElseIf env' exp3 (AssignTo result)
                                                     @ [ Indent, Fragment "end", LineTerminator ]
                                                    , env', { prec = ~1, exp = [ Fragment result ] })
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
  | doExpTo ctx env (F.ProjectionExp { label, record }) dest = doExpCont ctx env record (fn (stmts, env, record') => putPureTo ctx env dest (stmts, { prec = ~1, exp = paren ~1 record' @ [ Fragment ("[" ^ LabelToLua label ^ "]") ] }))
  | doExpTo ctx env (F.PrimExp (F.ListOp, _, xs)) dest
    = if Vector.length xs = 0 then
          putPureTo ctx env dest ([], { prec = ~1, exp = [ Fragment "_nil" ] })
      else
          mapCont (fn (e, cont) => doExpCont ctx env e (fn (x, _, e) => cont (x, e)))
                  (Vector.foldr (op ::) [] xs)
                  (fn ys => let val stmts = List.foldr (fn ((x, _), acc) => x @ acc) [] ys
                            in putPureTo ctx env dest (stmts, { prec = ~2, exp = Fragment ("_list{ n = " ^ Int.toString (Vector.length xs)) :: List.foldr (fn ((_, y), acc) => Fragment ", " :: #exp y @ acc) [ Fragment " }" ] ys })
                            end
                  )
  | doExpTo ctx env (F.PrimExp (F.VectorOp, _, xs)) dest
    = mapCont (fn (e, cont) => doExpCont ctx env e (fn (x, _, e) => cont (x, e)))
              (Vector.foldr (op ::) [] xs)
              (fn ys => let val stmts = List.foldr (fn ((x, _), acc) => x @ acc) [] ys
                        in putPureTo ctx env dest (stmts, { prec = ~1, exp = Fragment ("{ n = " ^ Int.toString (Vector.length xs)) :: List.foldr (fn ((_, y), acc) => Fragment ", " :: #exp y @ acc) [ Fragment " }" ] ys })
                        end
              )
  | doExpTo ctx env (F.TyAbsExp (_, _, exp)) dest = doExpTo ctx env exp dest
  | doExpTo ctx env (F.TyAppExp (exp, _)) dest = doExpTo ctx env exp dest
  | doExpTo ctx env (F.PrimExp (F.RecordEqualityOp, _, xs)) dest
    = if Vector.length xs = 1 then
          let val exp = Vector.sub (xs, 0)
          in case exp of
                 F.RecordExp [] => putPureTo ctx env dest ([], { prec = ~1, exp = [ Fragment "_Unit_EQUAL" ] })
               | _ => doExpCont ctx env exp
                                (fn (stmts, env, e') =>
                                    putPureTo ctx env dest (stmts, { prec = ~2, exp = Fragment "_Record_EQUAL(" :: #exp e' @ [ Fragment ")" ] })
                                )
          end
      else
          raise CodeGenError "PrimExp.RecordEqualityOp: invalid number of arguments"
  | doExpTo ctx env (F.PrimExp (F.DataTagOp, _, xs)) dest
    = if Vector.length xs = 1 then
          let val exp = Vector.sub (xs, 0)
          in doExpCont ctx env exp (fn (stmts, env, exp') => putPureTo ctx env dest (stmts, { prec = ~1, exp = paren ~1 exp' @ [ Fragment ".tag" ] }))
          end
      else
          raise CodeGenError "PrimExp.DataTagOp: invalid number of arguments"
  | doExpTo ctx env (F.PrimExp (F.DataPayloadOp, _, xs)) dest
    = if Vector.length xs = 1 then
          let val exp = Vector.sub (xs, 0)
          in doExpCont ctx env exp (fn (stmts, env, exp') => putPureTo ctx env dest (stmts, { prec = ~1, exp = paren ~1 exp' @ [ Fragment ".payload" ] }))
          end
      else
          raise CodeGenError "PrimExp.DataPayloadOp: invalid number of arguments"
  | doExpTo ctx env (F.StructExp { valMap, strMap, exnTagMap }) dest
    = let val valMap' = Syntax.VIdMap.listItemsi valMap
          val strMap' = Syntax.StrIdMap.listItemsi strMap
          val exnTagMap' = Syntax.VIdMap.listItemsi exnTagMap
      in mapCont (fn ((label, path), cont) => doExpCont ctx env (F.PathToExp path) (fn (stmts, env, e) => cont (stmts, (label, e))))
                 valMap'
                 (fn valMap' =>
                     let val (stmts, valFields) = ListPair.unzip valMap'
                     in mapCont (fn ((label, path), cont) => doExpCont ctx env (F.PathToExp path) (fn (stmts, env, e) => cont (stmts, (label, e))))
                                strMap'
                                (fn strMap' =>
                                    let val (stmts', strFields) = ListPair.unzip strMap'
                                    in mapCont (fn ((label, path), cont) => doExpCont ctx env (F.PathToExp path) (fn (stmts, env, e) => cont (stmts, (label, e))))
                                               exnTagMap'
                                               (fn exnTagMap' =>
                                                   let val (stmts'', exnTagFields) = ListPair.unzip exnTagMap'
                                                       val valFields = List.map (fn (vid, e) => [ Fragment ("[" ^ toLuaStringLit (Syntax.getVIdName vid) ^ "] = ") ] @ #exp e) valFields
                                                       val strFields = List.map (fn (Syntax.MkStrId name, e) => [ Fragment ("[" ^ toLuaStringLit ("_" ^ name) ^ "] = ") ] @ #exp e) strFields
                                                       val exnTagFields = List.map (fn (vid, e) => [ Fragment ("[" ^ toLuaStringLit (Syntax.getVIdName vid ^ ".tag") ^ "] = ") ] @ #exp e) exnTagFields
                                                   in putPureTo ctx env dest ( List.concat stmts @ List.concat stmts' @ List.concat stmts''
                                                                             , { prec = 0, exp = [ Fragment "{" ] @ commaSep (valFields @ strFields @ exnTagFields) @ [ Fragment "}" ] }
                                                                             )
                                                   end
                                               )
                                    end
                                )
                     end
                 )
      end
  | doExpTo ctx env (exp as F.SProjectionExp (exp', F.ValueLabel vid)) dest = let val builtin = case extractLongVId exp of
                                                                                           SOME longvid => (case USyntax.LongVIdMap.find (builtins, longvid) of
                                                                                                                SOME (SOME luaExpr) => SOME luaExpr
                                                                                                              | SOME NONE => raise CodeGenError ("the built-in identifier " ^ USyntax.print_LongVId longvid ^ " has no runtime counterpart")
                                                                                                              | NONE => NONE
                                                                                                           )
                                                                                         | NONE => NONE
                                                                     in case builtin of
                                                                            SOME luaExpr => putPureTo ctx env dest ([], { prec = ~1, exp = [ Fragment luaExpr ] }) (* TODO: prec for true, false, nil *)
                                                                          | NONE => doExpCont ctx env exp' (fn (stmts, env, exp') => putPureTo ctx env dest (stmts, { prec = ~1, exp = paren ~1 exp' @ [ Fragment ("[" ^ toLuaStringLit (Syntax.getVIdName vid) ^ "]") ] }))
                                                                     end
  | doExpTo ctx env (exp as F.SProjectionExp (exp', label)) dest = let val field = case label of
                                                                                       F.ValueLabel vid => Syntax.getVIdName vid
                                                                                     | F.StructLabel (Syntax.MkStrId name) => "_" ^ name
                                                                                     | F.ExnTagLabel vid => Syntax.getVIdName vid ^ ".tag"
                                                                   in doExpCont ctx env exp' (fn (stmts, env, exp') => putPureTo ctx env dest (stmts, { prec = ~1, exp = paren ~1 exp' @ [ Fragment ("[" ^ toLuaStringLit field ^ "]") ] }))
                                                                   end
  | doExpTo ctx env (F.PackExp { payloadTy, exp, packageTy }) dest = doExpTo ctx env exp dest
  | doExpTo ctx env (F.PrimExp (F.PrimFnOp primOp, _, args)) dest
    = let fun doUnary cont = if Vector.length args = 1 then
                                 let val a = Vector.sub (args, 0)
                                 in doExpCont ctx env a cont
                                 end
                             else
                                 raise CodeGenError ("PrimExp." ^ Syntax.primOpToString primOp ^ ": invalid number of arguments")
          fun doBinary cont = if Vector.length args = 2 then
                                  let val a = Vector.sub (args, 0)
                                      val b = Vector.sub (args, 1)
                                  in doExpCont ctx env a (fn (stmts0, env, a) =>
                                                             doExpCont ctx env b (fn (stmts1, env, b) =>
                                                                                     cont (stmts0 @ stmts1, env, (a, b))
                                                                                 )
                                                         )
                                  end
                              else
                                  raise CodeGenError ("PrimExp." ^ Syntax.primOpToString primOp ^ ": invalid number of arguments")
          fun doBinaryOp (binop, pure) = doBinary (fn (stmts, env, (a, b)) =>
                                                      let val e = case binop of
                                                                      InfixOp (prec, luaop) => { prec = prec, exp = paren prec a @ Fragment (" " ^ luaop ^ " ") :: paren (prec + 1) b }
                                                                    | InfixOpR (prec, luaop) => { prec = prec, exp = paren (prec + 1) a @ Fragment (" " ^ luaop ^ " ") :: paren prec b }
                                                      in if pure then
                                                             putPureTo ctx env dest (stmts, e)
                                                         else
                                                             putImpureTo ctx env dest (stmts, e)
                                                      end
                                                  )
          fun doTernary cont = if Vector.length args = 3 then
                                  let val a = Vector.sub (args, 0)
                                      val b = Vector.sub (args, 1)
                                      val c = Vector.sub (args, 2)
                                  in doExpCont ctx env a (fn (stmts0, env, a) =>
                                                             doExpCont ctx env b (fn (stmts1, env, b) =>
                                                                                     doExpCont ctx env c (fn (stmts2, env, c) =>
                                                                                                             cont (stmts0 @ stmts1 @ stmts2, env, (a, b, c))
                                                                                                         )
                                                                                 )
                                                         )
                                  end
                              else
                                  raise CodeGenError ("PrimExp." ^ Syntax.primOpToString primOp ^ ": invalid number of arguments")
      in case primOp of
             Syntax.PrimOp_call2 => doTernary (fn (stmts, env, (f, a0, a1)) =>
                                                               putImpureTo ctx env dest (stmts, { prec = ~2, exp = paren ~1 f @ Fragment "(" :: #exp a0 @ Fragment ", " :: #exp a1 @ [ Fragment ")" ] })
                                              )
           | Syntax.PrimOp_call3 => if Vector.length args = 4 then
                                        let val f = Vector.sub (args, 0)
                                            val a0 = Vector.sub (args, 1)
                                            val a1 = Vector.sub (args, 2)
                                            val a2 = Vector.sub (args, 3)
                                        in doExpCont ctx env f (fn (stmts0, env, f) =>
                                                                   doExpCont ctx env a0 (fn (stmts1, env, a0) =>
                                                                                            doExpCont ctx env a1 (fn (stmts2, env, a1) =>
                                                                                                                     doExpCont ctx env a2 (fn (stmts3, env, a2) =>
                                                                                                                                              putImpureTo ctx env dest (stmts0 @ stmts1 @ stmts2 @ stmts3, { prec = ~2, exp = paren ~1 f @ Fragment "(" :: #exp a0 @ Fragment ", " :: #exp a1 @ Fragment ", " :: #exp a2 @ [ Fragment ")" ] })
                                                                                                                                          )
                                                                                                                 )
                                                                                        )
                                                               )
                                        end
                                    else
                                        raise CodeGenError "PrimExp.call3: invalid number of arguments"
           | Syntax.PrimOp_Lua_sub => doBinary (fn (stmts, env, (a, b)) =>
                                                   putImpureTo ctx env dest (stmts, { prec = ~1, exp = paren ~1 a @ Fragment "[" :: #exp b @ [ Fragment "]" ] })
                                               )
           | Syntax.PrimOp_Lua_set => doTernary (fn (stmts, env, (a, b, c)) =>
                                                    let val stmts = stmts @ Indent :: paren ~1 a @ Fragment "[" :: #exp b @ Fragment "] = " :: #exp c @ [ OptSemicolon ]
                                                    in putPureTo ctx env dest (stmts, { prec = 0, exp = [ Fragment "nil" ] })
                                                    end
                                                )
           | Syntax.PrimOp_Lua_isNil => doUnary (fn (stmts, env, a) =>
                                                    putPureTo ctx env dest (stmts, { prec = 10, exp = paren 10 a @ [ Fragment " == nil" ] })
                                                )
           | Syntax.PrimOp_Lua_EQUAL => doBinaryOp (InfixOp (10, "=="), false)
           | Syntax.PrimOp_Lua_NOTEQUAL => doBinaryOp (InfixOp (10, "~="), false)
           | Syntax.PrimOp_Lua_LT => doBinaryOp (InfixOp (10, "<"), false)
           | Syntax.PrimOp_Lua_GT => doBinaryOp (InfixOp (10, ">"), false)
           | Syntax.PrimOp_Lua_LE => doBinaryOp (InfixOp (10, "<="), false)
           | Syntax.PrimOp_Lua_GE => doBinaryOp (InfixOp (10, ">="), false)
           | Syntax.PrimOp_Lua_PLUS => doBinaryOp (InfixOp (4, "+"), false)
           | Syntax.PrimOp_Lua_MINUS => doBinaryOp (InfixOp (4, "-"), false)
           | Syntax.PrimOp_Lua_TIMES => doBinaryOp (InfixOp (3, "*"), false)
           | Syntax.PrimOp_Lua_DIVIDE => doBinaryOp (InfixOp (3, "/"), false)
           | Syntax.PrimOp_Lua_INTDIV => doBinaryOp (InfixOp (3, "//"), false)
           | Syntax.PrimOp_Lua_MOD => doBinaryOp (InfixOp (3, "%"), false)
           | Syntax.PrimOp_Lua_pow => doBinaryOp (InfixOpR (1, "^"), false)
           | Syntax.PrimOp_Lua_unm => doUnary (fn (stmts, env, a) =>
                                                  putImpureTo ctx env dest (stmts, { prec = 2, exp = Fragment "- " :: paren 2 a })
                                              )
           | Syntax.PrimOp_Lua_andb => doBinaryOp (InfixOp (7, "&"), false)
           | Syntax.PrimOp_Lua_orb => doBinaryOp (InfixOp (9, "|"), false)
           | Syntax.PrimOp_Lua_xorb => doBinaryOp (InfixOp (8, "~"), false)
           | Syntax.PrimOp_Lua_notb => doUnary (fn (stmts, env, a) =>
                                                   putImpureTo ctx env dest (stmts, { prec = 2, exp = Fragment "~ " :: paren 2 a })
                                               )
           | Syntax.PrimOp_Lua_LSHIFT => doBinaryOp (InfixOp (6, "<<"), false)
           | Syntax.PrimOp_Lua_RSHIFT => doBinaryOp (InfixOp (6, ">>"), false)
           | Syntax.PrimOp_Lua_concat => doBinaryOp (InfixOpR (5, ".."), false)
           | Syntax.PrimOp_Lua_length => doUnary (fn (stmts, env, a) =>
                                                     putImpureTo ctx env dest (stmts, { prec = 2, exp = Fragment "#" :: paren 2 a })
                                                 )
           | Syntax.PrimOp_Lua_isFalsy => doUnary (fn (stmts, env, a) =>
                                                      putPureTo ctx env dest (stmts, { prec = 2, exp = Fragment "not " :: paren 2 a })
                                                  )
      end
  | doExpTo ctx env (F.PrimExp (F.ExnInstanceofOp, _, args)) dest
    = if Vector.length args = 2 then
          let val a0 = Vector.sub (args, 0)
              val a1 = Vector.sub (args, 1)
          in doExpCont ctx env a0 (fn (stmts0, env, a0') =>
                                      doExpCont ctx env a1 (fn (stmts1, env, a1') =>
                                                               putPureTo ctx env dest (stmts0 @ stmts1, { prec = ~2, exp = Fragment "__exn_instanceof(" :: #exp a0' @ [ Fragment "," ] @ #exp a1' @ [ Fragment ")" ] })
                                                           )
                                  )
          end
      else
          raise CodeGenError "PrimExp.ExnInstanceofOp: invalid number of arguments"
(* doDec : Context -> Env -> F.Dec -> string *)
and doDec ctx env (F.ValDec (F.SimpleBind(v, _, exp)))
    = let val luavid = VIdToLua v
          val (env', dec) = declareIfNotHoisted (env, [luavid])
      in dec @ doExpTo ctx env' exp (AssignTo luavid)
      end
  | doDec ctx env (F.ValDec (F.TupleBind([], exp)))
    = doExpTo ctx env exp Discard
  | doDec ctx env (F.ValDec (F.TupleBind(vars, exp)))
    = let val vars = List.map (VIdToLua o #1) vars
          val (env', decs) = declareIfNotHoisted (env, vars)
      in decs @ doExpTo ctx env' exp (UnpackingAssignTo vars)
      end
  | doDec ctx env (F.RecValDec valbinds)
    = let val (decs, assignments) = List.foldr (fn ((v,_,exp), (decs, assignments)) => let val v' = VIdToLua v
                                                                                           val (env, dec) = declareIfNotHoisted (env, [v'])
                                                                                       in (dec @ decs, doExpTo ctx env exp (AssignTo v') @ assignments)
                                                                                       end
                                               ) ([], []) valbinds
      in decs @ assignments
      end
  | doDec ctx env (F.UnpackDec (tv, kind, vid, ty, exp))
    = let val luavid = VIdToLua vid
          val (env', dec) = declareIfNotHoisted (env, [luavid])
      in dec @ doExpTo ctx env' exp (AssignTo luavid)
      end
  | doDec ctx env (F.IgnoreDec exp) = doExpTo ctx env exp Discard
  | doDec ctx env (F.DatatypeDec datbinds) = List.concat (List.map (doDatBind ctx env) datbinds)
  | doDec ctx env (F.ExceptionDec { conName as USyntax.MkVId(name, _), tagName, payloadTy })
    = let val conName' = VIdToLua conName
          val tagName' = VIdToLua tagName
      in [ Indent, Fragment ((if isHoisted (env, tagName') then "" else "local ") ^ tagName' ^ " = { " ^ toLuaStringLit name ^ " }"), LineTerminator ]
         @ (case payloadTy of
                NONE => [ Indent, Fragment ((if isHoisted (env, conName') then "" else "local ") ^ conName' ^ " = setmetatable({ tag = " ^ tagName' ^ " }, _exn_meta)"), LineTerminator ]
              | SOME _ => [ Indent, Fragment ((if isHoisted (env, conName') then "function " else "local function ") ^ conName' ^ "(payload)"), LineTerminator, IncreaseIndent
                          , Indent, Fragment ("return setmetatable({ tag = " ^ tagName' ^ ", payload = payload }, _exn_meta)"), LineTerminator
                          , DecreaseIndent, Indent, Fragment "end", LineTerminator
                          ]
           )
      end
  | doDec ctx env (F.ExceptionRepDec _) = raise Fail "internal error: ExceptionRepDec should have been desugared earlier"
  | doDec ctx env (F.ExportValue _) = raise Fail "internal error: ExportValue must be the last statement"
  | doDec ctx env (F.ExportModule _) = raise Fail "internal error: ExportModule must be the last statement"
  | doDec ctx env (F.GroupDec (SOME hoist, decs)) = let val (env, dec) = declareIfNotHoisted (env, List.map VIdToLua (USyntax.VIdSet.toList hoist))
                                                    in dec
                                                       @ [ Indent, Fragment "do", LineTerminator, IncreaseIndent ]
                                                       @ doDecs ctx env decs
                                                       @ [ DecreaseIndent, Indent, Fragment "end", LineTerminator ]
                                                    end
  | doDec ctx env (F.GroupDec (NONE, decs)) = doDecs ctx env decs (* should be an error? *)
and doDatBind ctx env (F.DatBind (tyvars, tycon, conbinds)) = List.concat (List.map (doConBind ctx env) conbinds) (* TODO: equality *)
and doConBind ctx env (F.ConBind (vid as USyntax.MkVId(name,_), NONE)) = [ Indent, Fragment ((if isHoisted (env, VIdToLua vid) then "" else "local ") ^ VIdToLua vid ^ " = { tag = " ^ toLuaStringLit name ^ " }"), LineTerminator ]
  | doConBind ctx env (F.ConBind (vid as USyntax.MkVId(name,_), SOME ty)) = [ Indent, Fragment ((if isHoisted (env, VIdToLua vid) then "function " else "local function ") ^ VIdToLua vid ^ "(x)"), LineTerminator, IncreaseIndent
                                                                            , Indent, Fragment ("return { tag = " ^ toLuaStringLit name ^ ", payload = x }"), LineTerminator
                                                                            , DecreaseIndent, Indent, Fragment "end", LineTerminator
                                                                            ]

and doDecs ctx env [F.ExportValue exp] = doExpTo ctx env exp Return
  | doDecs ctx env [F.ExportModule fields] = mapCont (fn ((label, exp), cont) => doExpCont ctx env exp (fn (stmts, env, e) => cont (stmts, (label, e))))
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
