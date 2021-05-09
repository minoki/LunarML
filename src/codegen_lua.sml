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
                    ,(VId_EQUAL_bool, "_EQUAL_prim") (* Lua == *)
                    ,(VId_EQUAL_int, "_EQUAL_prim") (* Lua == *)
                    ,(VId_EQUAL_word, "_EQUAL_prim") (* Lua == *)
                    ,(VId_EQUAL_string, "_EQUAL_prim") (* Lua == *)
                    ,(VId_EQUAL_char, "_EQUAL_prim") (* Lua == *)
                    ,(VId_EQUAL_list, "_EQUAL_list")
                    ,(VId_EQUAL_ref, "_EQUAL_prim") (* Lua == *)
                    ,(VId_EQUAL_array, "_EQUAL_prim") (* Lua == *)
                    ,(VId_EQUAL_vector, "_EQUAL_vector")
                    ,(VId_EQUAL_exntag, "_EQUAL_exntag") (* Lua == *)
                         (* int *)
                    ,(VId_PLUS_int, "_add_int") (* may raise Overflow *)
                    ,(VId_MINUS_int, "_sub_int") (* may raise Overflow *)
                    ,(VId_TIMES_int, "_mul_int") (* may raise Overflow *)
                    ,(VId_abs_int, "_abs_int") (* may raise Overflow *)
                    ,(VId_TILDE_int, "_negate_int") (* may raise Overflow *)
                    ,(VId_div_int, "_div_int") (* may raise Overflow/Div *)
                    ,(VId_mod_int, "_mod_int") (* may raise Div *)
                    (* word *)
                    ,(VId_PLUS_word, "_add_word") (* Lua +; does not raise Overflow *)
                    ,(VId_MINUS_word, "_sub_word") (* Lua - (binary); does not raise Overflow *)
                    ,(VId_TIMES_word, "_mul_word") (* Lua *; does not raise Overflow *)
                    ,(VId_div_word, "_div_word") (* may raise Div *)
                    ,(VId_mod_word, "_mod_word") (* may raise Div *)
                    (* real *)
                    ,(VId_PLUS_real, "_add_real") (* Lua + *)
                    ,(VId_MINUS_real, "_sub_real") (* Lua - (binary) *)
                    ,(VId_TIMES_real, "_mul_real") (* Lua * *)
                    ,(VId_DIVIDE_real, "_divide_real") (* Lua / *)
                    ,(VId_abs_real, "_abs_real") (* Lua math.abs *)
                    ,(VId_TILDE_real, "_negate_real") (* Lua - (unary) *)
                    ,(VId_LT_int, "_LT_prim")
                    ,(VId_LT_word, "_LT_word")
                    ,(VId_LT_real, "_LT_prim")
                    ,(VId_LT_string, "_LT_prim")
                    ,(VId_LT_char, "_LT_prim")
                    ,(VId_GT_int, "_GT_prim")
                    ,(VId_GT_word, "_GT_word")
                    ,(VId_GT_real, "_GT_prim")
                    ,(VId_GT_string, "_GT_prim")
                    ,(VId_GT_char, "_GT_prim")
                    ,(VId_LE_int, "_LE_prim")
                    ,(VId_LE_word, "_LE_word")
                    ,(VId_LE_real, "_LE_prim")
                    ,(VId_LE_string, "_LE_prim")
                    ,(VId_LE_char, "_LE_prim")
                    ,(VId_GE_int, "_GE_prim")
                    ,(VId_GE_word, "_GE_word")
                    ,(VId_GE_real, "_GE_prim")
                    ,(VId_GE_string, "_GE_prim")
                    ,(VId_GE_char, "_GE_prim")
                    ,(VId_TextIO_print, "_print")
                    ,(VId_Int_toString, "_Int_toString")
                    ,(VId_String_HAT, "_string_append")
                    ,(VId_Bool_not, "_not") (* Lua not *)
                    ,(VId_Array_array, "_Array_array")
                    ,(VId_Array_fromList, "_Array_fromList")
                    ,(VId_Array_tabulate, "_Array_tabulate")
                    ,(VId_Array_length, "_Array_length")
                    ,(VId_Array_sub, "_Array_sub")
                    ,(VId_Array_update, "_Array_update")
                    ,(VId_Vector_fromList, "_Vector_fromList")
                    ,(VId_Vector_tabulate, "_Vector_tabulate")
                    ,(VId_Vector_length, "_Vector_length")
                    ,(VId_Vector_sub, "_Vector_sub")
                    ]
      end
datatype BinaryOp = InfixOp of string | NamedBinaryFn of string
val builtinBinaryOps : (BinaryOp * (* pure? *) bool) USyntax.VIdMap.map
    = let open InitialEnv
      in List.foldl USyntax.VIdMap.insert' USyntax.VIdMap.empty
                    [(VId_EQUAL_bool,   (InfixOp "==", true))
                    ,(VId_EQUAL_int,    (InfixOp "==", true))
                    ,(VId_EQUAL_word,   (InfixOp "==", true))
                    ,(VId_EQUAL_string, (InfixOp "==", true))
                    ,(VId_EQUAL_char,   (InfixOp "==", true))
                    ,(VId_EQUAL_exntag, (InfixOp "==", true))
                    ,(VId_PLUS_int,     (NamedBinaryFn "__add_int", false))
                    ,(VId_PLUS_word,    (InfixOp "+", true))
                    ,(VId_PLUS_real,    (InfixOp "+", true))
                    ,(VId_MINUS_int,    (NamedBinaryFn "__sub_int", false))
                    ,(VId_MINUS_word,   (InfixOp "-", true))
                    ,(VId_MINUS_real,   (InfixOp "-", true))
                    ,(VId_TIMES_int,    (NamedBinaryFn "__mul_int", false))
                    ,(VId_TIMES_word,   (InfixOp "*", true))
                    ,(VId_TIMES_real,   (InfixOp "*", true))
                    ,(VId_DIVIDE_real,  (InfixOp "/", true))
                    ,(VId_div_int,      (NamedBinaryFn "__div_int", false))
                    ,(VId_div_word,     (NamedBinaryFn "__div_word", false))
                    ,(VId_mod_int,      (NamedBinaryFn "__mod_int", false))
                    ,(VId_mod_word,     (NamedBinaryFn "__mod_word", false))
                    ,(VId_LT_int,       (InfixOp "<", true))
                    ,(VId_LT_real,      (InfixOp "<", true))
                    ,(VId_LT_string,    (InfixOp "<", true))
                    ,(VId_LT_char,      (InfixOp "<", true))
                    ,(VId_LT_word,      (NamedBinaryFn "__LT_word", true))
                    ,(VId_LE_int,       (InfixOp "<=", true))
                    ,(VId_LE_real,      (InfixOp "<=", true))
                    ,(VId_LE_string,    (InfixOp "<=", true))
                    ,(VId_LE_char,      (InfixOp "<=", true))
                    ,(VId_LE_word,      (NamedBinaryFn "__LE_word", true))
                    ,(VId_GT_int,       (InfixOp ">", true))
                    ,(VId_GT_real,      (InfixOp ">", true))
                    ,(VId_GT_string,    (InfixOp ">", true))
                    ,(VId_GT_char,      (InfixOp ">", true))
                    ,(VId_GT_word,      (NamedBinaryFn "__GT_word", true))
                    ,(VId_GE_int,       (InfixOp ">=", true))
                    ,(VId_GE_real,      (InfixOp ">=", true))
                    ,(VId_GE_string,    (InfixOp ">=", true))
                    ,(VId_GE_char,      (InfixOp ">=", true))
                    ,(VId_GE_word,      (NamedBinaryFn "__GE_word", true))
                    ,(VId_String_HAT,   (InfixOp "..", true))
                    ]
      end
fun VIdToLua(vid as USyntax.MkVId(name, n)) = if n < 0 then
                                                  case USyntax.VIdMap.find (builtins, vid) of
                                                      NONE => raise Fail ("Unknown built-in symbol: " ^ name ^ "@" ^ Int.toString n)
                                                    | SOME luaExpr => luaExpr
                                              else
                                                  smlNameToLua name ^ "_" ^ Int.toString n

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
 0: function call
 *)

type Context = { nextLuaId : int ref }
type Env = { indent : int }
val initialEnv : Env = { indent = 0 }

fun nextIndentLevel ({ indent } : Env) = { indent = indent + 2 }
fun indent ({ indent } : Env) = CharVector.tabulate(indent, fn _ => #" ")

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
                                                                         else
                                                                             "\\x" ^ Int.fmt StringCvt.HEX x
                                                                     end
                                                          ) s ^ "\""

fun doLiteral (Syntax.IntegerConstant x) = if x < 0 then "(-" ^ Int.toString (~ x) ^ ")" else Int.toString x
  | doLiteral (Syntax.WordConstant x) = Word.toString x
  | doLiteral (Syntax.RealConstant x) = raise Fail "CodeGenLua: real constant not implemented yet"
  | doLiteral (Syntax.StringConstant x) = toLuaStringLit x
  | doLiteral (Syntax.CharacterConstant x) = toLuaStringLit x

datatype Destination = Return | AssignTo of string | UnpackingAssignTo of string list | Discard

(* doExp : Context -> Env -> F.Exp -> string list * (* pure expression *) string *)
fun doExp ctx env (F.SConExp scon): string list * string = ([], doLiteral scon)
  | doExp ctx env (F.VarExp (Syntax.MkQualified(_, vid))) = ([], VIdToLua vid) (* TODO: qualified identifier? *)
  | doExp ctx env (F.RecordExp []) = ([], "_unit")
  | doExp ctx env (F.RecordExp fields) = (case Syntax.extractTuple(1, fields) of
                                                    SOME xs => let val (stmts, ys) = ListPair.unzip (List.map (doExp ctx env) xs)
                                                               in (List.concat stmts, "{" ^ String.concatWith ", " ys ^ "}")
                                                               end
                                                  | NONE => let val (stmts, ys) = ListPair.unzip (List.map (fn (label, exp) => case doExp ctx env exp of (stmt, e) => (stmt, "[" ^ LabelToLua label ^ "] = " ^ e)) fields)
                                                            in (List.concat stmts, "{" ^ String.concatWith ", " ys ^ "}")
                                                            end
                                         )
  | doExp ctx env (F.LetExp (dec, exp))
    = let val dec' = doDec ctx env dec
          val (stmt', exp') = doExp ctx env exp
      in (dec' :: stmt', exp')
      end
  | doExp ctx env (F.AppExp(F.ProjectionExp { label = label, ... }, exp2))
    = let val (stmts, exp2') = doExp ctx env exp2
      in (stmts, "(" ^ exp2' ^ ")[" ^ LabelToLua(label) ^ "]")
      end
  | doExp ctx env (F.AppExp (exp1 as F.VarExp (Syntax.MkQualified([], vid)), exp2 as F.RecordExp [(Syntax.NumericLabel 1, e1), (Syntax.NumericLabel 2, e2)]))
    (* built-in operator? *)
    (* TODO: evaluation order *)
    (* TODO: check for overflow *)
    = (case USyntax.VIdMap.find(builtinBinaryOps, vid) of
           SOME (InfixOp luaop, true) => let val (stmts1, e1') = doExp ctx env e1
                                             val (stmts2, e2') = doExp ctx env e2
                                         in (stmts1 @ stmts2, "(" ^ e1' ^ ") " ^ luaop ^ " (" ^ e2' ^ ")")
                                         end
         | SOME (NamedBinaryFn luafn, true) => let val (stmts1, e1') = doExp ctx env e1
                                                   val (stmts2, e2') = doExp ctx env e2
                                               in (stmts1 @ stmts2, luafn ^ "(" ^ e1' ^ ", " ^ e2' ^ ")")
                                               end
         | _ => let val (stmts1, exp1') = doExp ctx env exp1
                    val (stmts2, exp2') = doExp ctx env exp2
                    val dest = genSym ctx
                in (stmts1 @ stmts2 @ [indent env ^ "local " ^ dest ^ " = (" ^ exp1' ^ ")(" ^ exp2' ^ ")\n"], dest)
                end
      )
  | doExp ctx env (F.AppExp (F.VarExp (Syntax.MkQualified([], vid)), exp2))
    (* built-in operator? *)
    (* TODO: check for overflow *)
    = let open InitialEnv
      in if USyntax.eqVId(vid, VId_TILDE_real) then
             let val (stmts, exp2') = doExp ctx env exp2
             in (stmts, "- (" ^ exp2' ^ ")")
             end
         else if USyntax.eqVId(vid, VId_Bool_not) then
             let val (stmts, exp2') = doExp ctx env exp2
             in (stmts, "not (" ^ exp2' ^ ")")
             end
         else if USyntax.eqVId(vid, VId_EXCLAM) then
             let val (stmts, exp2') = doExp ctx env exp2
                 val dest = genSym ctx
             in (stmts @ [indent env ^ "local " ^ dest ^ " = (" ^ exp2' ^ ").payload"], dest)
             end
         else
             let val (stmts, exp2') = doExp ctx env exp2
                 val dest = genSym ctx
             in (stmts @ [indent env ^ "local " ^ dest ^ " = " ^ VIdToLua vid ^ "(" ^ exp2' ^ ")\n"], dest)
             end
      end
  | doExp ctx env (F.AppExp (exp1, exp2))
    = let val (stmts1, exp1') = doExp ctx env exp1
          val (stmts2, exp2') = doExp ctx env exp2
          val dest = genSym ctx
      in (stmts1 @ stmts2 @ [indent env ^ "local " ^ dest ^ " = (" ^ exp1' ^ ")(" ^ exp2' ^ ")\n"], dest)
      end
  | doExp ctx env (F.HandleExp { body, exnName, handler } )
    = let val body' = doExpTo ctx (nextIndentLevel env) Return body
          val status = genSym ctx
          val result = genSym ctx
          val handler' = doExpTo ctx (nextIndentLevel env) (AssignTo result) handler
      in ( [indent env ^ "local " ^ status ^ ", " ^ result ^ " = pcall(function()\n" ^ body' ^ indent env ^ "end)\n"
           ^ indent env ^ "if not " ^ status ^ " then\n"
           ^ indent (nextIndentLevel env) ^ "local " ^ VIdToLua exnName ^ " = " ^ result ^ "\n"
           ^ handler'
           ^ indent env ^ "end\n"]
         , result
         )
      end
  | doExp ctx env (F.RaiseExp (span, exp))
    = let val (stmts, exp') = doExp ctx env exp
          val { start = { file, line, column }, ...} = span
      in (stmts @ [indent env ^ "_raise(" ^ exp' ^ ", " ^ toLuaStringLit file ^ ", " ^ Int.toString line ^ ", " ^ Int.toString column ^ ")"], "nil")
      end
  | doExp ctx env (exp as F.IfThenElseExp (exp1, exp2, exp3))
    = let val result = genSym ctx
          fun doElseIf env (F.IfThenElseExp(e1, e2, e3)) = let val (s1, e1') = doExp ctx (nextIndentLevel env) e1
                                                           in if List.null s1 then
                                                                  [ indent env ^ "elseif " ^ e1' ^ " then\n"
                                                                  , doExpTo ctx (nextIndentLevel env) (AssignTo result) e2 ]
                                                                  @ doElseIf env e3
                                                              else
                                                                  [ indent env ^ "else\n" ]
                                                                  @ s1
                                                                  @ [ indent (nextIndentLevel env) ^ "if " ^ e1' ^ " then\n"
                                                                    , doExpTo ctx (nextIndentLevel (nextIndentLevel env)) (AssignTo result) e2 ]
                                                                  @ doElseIf (nextIndentLevel env) e3
                                                                  @ [ indent (nextIndentLevel env) ^ "end\n" ]
                                                       end
            | doElseIf env e = [ indent env ^ "else\n"
                               , doExpTo ctx (nextIndentLevel env) (AssignTo result) e ]
          val (stmts1, exp1') = doExp ctx env exp1
      in ( [ indent env ^ "local " ^ result ^ "\n" ]
           @ stmts1
           @ [ indent env ^ "if " ^ exp1' ^ " then\n", doExpTo ctx (nextIndentLevel env) (AssignTo result) exp2]
           @ doElseIf env exp3
           @ [ indent env ^ "end\n" ]
         , result
         ) (* TODO: Use 'and' / 'or' if we can *)
      end
  | doExp ctx env (F.CaseExp _) = raise Fail "CodeGenLua: CaseExp should have been desugared earlier"
  | doExp ctx env (F.FnExp (vid, _, exp)) = ([], "function(" ^ VIdToLua(vid) ^ ")\n"
                                                 ^ doExpTo ctx env Return exp (* TODO: indent level *)
                                                 ^ "end\n"
                                            )
  | doExp ctx env (F.ProjectionExp { label = label, ... }) = ([], "function(x) return x[" ^ LabelToLua(label) ^ "] end")
  | doExp ctx env (F.TyAbsExp (_, exp)) = doExp ctx env exp
  | doExp ctx env (F.TyAppExp (exp, _)) = doExp ctx env exp
  | doExp ctx env (F.RecordEqualityExp fields) = (case Syntax.extractTuple(1, fields) of
                                                      SOME xs => let val (stmts, ys) = ListPair.unzip (List.map (doExp ctx env) xs)
                                                                 in (List.concat stmts, "_recordEqual({" ^ String.concatWith ", " ys ^ "})")
                                                                 end
                                                    | NONE => let val (stmts, ys) = ListPair.unzip (List.map (fn (label, exp) => case doExp ctx env exp of (stmt, e) => (stmt, "[" ^ LabelToLua label ^ "] = " ^ e)) fields)
                                                              in (List.concat stmts, "_recordEqual({" ^ String.concatWith ", " ys ^ "})")
                                                              end
                                                 )
  | doExp ctx env (F.DataTagExp exp) = let val (stmts, exp') = doExp ctx env exp
                                       in (stmts, "(" ^ exp' ^ ").tag")
                                       end
  | doExp ctx env (F.DataPayloadExp exp) = let val (stmts, exp') = doExp ctx env exp
                                           in (stmts, "(" ^ exp' ^ ").payload") (* 'ref' should have been desugared to '!' *)
                                           end
and putPureTo ctx env Return (exp : string) = indent env ^ "return " ^ exp ^ "\n"
  | putPureTo ctx env (AssignTo v) exp = indent env ^ v ^ " = " ^ exp ^ "\n"
  | putPureTo ctx env (UnpackingAssignTo v) exp = indent env ^ String.concatWith ", " v ^ " = table.unpack(" ^ exp ^ ")\n"
  | putPureTo ctx env Discard exp = ""
and putImpureTo ctx env Return (exp : string) = indent env ^ "return " ^ exp ^ "\n"
  | putImpureTo ctx env (AssignTo v) exp = indent env ^ v ^ " = " ^ exp ^ "\n"
  | putImpureTo ctx env (UnpackingAssignTo v) exp = indent env ^ String.concatWith ", " v ^ " = table.unpack(" ^ exp ^ ")\n"
  | putImpureTo ctx env Discard exp = indent env ^ "local _ = " ^ exp ^ "\n"
and doExpTo ctx env dest (F.SConExp scon) : string = putPureTo ctx env dest (doLiteral scon)
  | doExpTo ctx env dest (F.VarExp (Syntax.MkQualified (_, vid))) = putPureTo ctx env dest (VIdToLua vid)
  | doExpTo ctx env dest (F.RecordExp []) = putPureTo ctx env dest "_unit"
  | doExpTo ctx env Discard (F.RecordExp fields) = String.concat (List.map (fn (_, exp) => doExpTo ctx env Discard exp) fields)
  | doExpTo ctx env dest (F.RecordExp fields)
    = let val (stmts, fields') = ListPair.unzip (List.map (fn (label, exp) => let val v = genSym ctx
                                                                              in (doExpTo ctx env (AssignTo v) exp, (label, v))
                                                                              end
                                                          ) fields)
          val decl = indent env ^ "local " ^ String.concatWith ", " (List.map #2 fields') ^ "\n"
      in decl
         ^ String.concat stmts
         ^ putPureTo ctx env dest (case Syntax.extractTuple(1, fields) of
                                       SOME _ => "{" ^ String.concatWith ", " (List.map #2 fields') ^ "}"
                                     | NONE => "{" ^ String.concatWith ", " (List.map (fn (label, v) => "[" ^ LabelToLua label ^ "] = " ^ v) fields') ^ "}"
                                  )
      end
  | doExpTo ctx env dest (F.LetExp (dec, exp))
    = let val dec' = doDec ctx env dec
          val stmts = doExpTo ctx env dest exp
      in dec' ^ stmts
      end
  | doExpTo ctx env dest (F.AppExp (F.ProjectionExp { label, ...}, exp2))
    = let val v = genSym ctx
      in indent env ^ "local " ^ v ^ "\n"
         ^ doExpTo ctx env (AssignTo v) exp2
         ^ putPureTo ctx env dest (v ^ "[" ^ LabelToLua label ^ "]")
      end
  | doExpTo ctx env dest (F.AppExp (exp1 as F.VarExp (Syntax.MkQualified([], vid)), F.RecordExp [(Syntax.NumericLabel 1, e1), (Syntax.NumericLabel 2, e2)]))
    = let val (stmts1, e1') = doExp ctx env e1
          val (stmts2, e2') = doExp ctx env e2
      in case USyntax.VIdMap.find(builtinBinaryOps, vid) of
             SOME (InfixOp luaop, pure) => let val e = "(" ^ e1' ^ ") " ^ luaop ^ " (" ^ e2' ^ ")"
                                           in String.concat stmts1
                                              ^ String.concat stmts2
                                              ^ (if pure then
                                                     putPureTo ctx env dest e
                                                 else
                                                     putImpureTo ctx env dest e
                                                )
                                           end
         | SOME (NamedBinaryFn luafn, pure) => let val e = luafn ^ "(" ^ e1' ^ ", " ^ e2' ^ ")"
                                               in String.concat stmts1
                                                  ^ String.concat stmts2
                                                  ^ (if pure then
                                                         putPureTo ctx env dest e
                                                     else
                                                         putImpureTo ctx env dest e
                                                    )
                                               end
         | NONE => String.concat stmts1
                   ^ String.concat stmts2
                   ^ putImpureTo ctx env dest (VIdToLua vid ^ "({" ^ e1' ^ ", " ^ e2' ^ "})")
      end
  | doExpTo ctx env dest (F.AppExp (exp1 as F.VarExp (Syntax.MkQualified([], vid)), exp2))
    = let open InitialEnv
          val (stmts, e2') = doExp ctx env exp2
          val v1 = genSym ctx
      in String.concat stmts
         ^ (if USyntax.eqVId(vid, VId_TILDE_real) then
                putPureTo ctx env dest ("- " ^ e2')
            else if USyntax.eqVId(vid, VId_Bool_not) then
                putPureTo ctx env dest ("not " ^ e2')
            else if USyntax.eqVId(vid, VId_EXCLAM) then
                putImpureTo ctx env dest ("(" ^ e2' ^ ").payload")
            else
                putImpureTo ctx env dest (VIdToLua vid ^ "(" ^ e2' ^ ")")
           )
      end
  | doExpTo ctx env dest (F.AppExp (exp1, exp2))
    = let val (stmts1, e1') = doExp ctx env exp1
          val (stmts2, e2') = doExp ctx env exp2
      in String.concat stmts1
         ^ String.concat stmts2
         ^ putImpureTo ctx env dest ("(" ^ e1' ^ ")(" ^ e2' ^ ")")
      end
  | doExpTo ctx env dest (F.HandleExp { body, exnName, handler })
    = let val status = genSym ctx
          val result = genSym ctx
      in indent env ^ "local " ^ status ^ ", " ^ result ^ " = pcall(function()\n"
         ^ doExpTo ctx (nextIndentLevel env) Return body ^ indent env ^ "end)\n"
         ^ indent env ^ "if not " ^ status ^ " then\n"
         ^ indent (nextIndentLevel env) ^ "local " ^ VIdToLua exnName ^ " = " ^ result ^ "\n"
         ^ doExpTo ctx (nextIndentLevel env) (AssignTo result) handler ^ "\n"
         ^ indent env ^ "end\n"
      end
  | doExpTo ctx env dest (F.RaiseExp (span as { start = { file, line, column }, ... }, exp))
    = let val (stmts, exp') = doExp ctx env exp
      in String.concat stmts
         ^ indent env ^ "_raise(" ^ exp' ^ ", " ^ toLuaStringLit file ^ ", " ^ Int.toString line ^ ", " ^ Int.toString column ^ ")\n"
      end
  | doExpTo ctx env dest (F.IfThenElseExp (exp1, exp2, exp3))
    = let val (stmtsc, expc) = doExp ctx env exp1
      in String.concat stmtsc
         ^ indent env ^ "if " ^ expc ^ " then\n"
         ^ doExpTo ctx (nextIndentLevel env) dest exp2
         ^ indent env ^ "else\n" (* TODO: Use elseif if we can *)
         ^ doExpTo ctx (nextIndentLevel env) dest exp3
         ^ indent env ^ "end\n"
      end
  | doExpTo ctx env dest (F.CaseExp _) = raise Fail "Lua codegen: CaseExp should have been desugared earlier"
  | doExpTo ctx env dest (F.FnExp (vid, _, exp)) = putPureTo ctx env dest ("function(" ^ VIdToLua vid ^ ")\n" ^ doExpTo ctx (nextIndentLevel env) Return exp ^ indent env ^ "end\n") (* TODO: update environment *)
  | doExpTo ctx env dest (F.ProjectionExp { label, ... }) = putPureTo ctx env dest ("function(x) return x[" ^ LabelToLua label ^ "] end\n")
  | doExpTo ctx env dest (F.TyAbsExp (_, exp)) = doExpTo ctx env dest exp
  | doExpTo ctx env dest (F.TyAppExp (exp, _)) = doExpTo ctx env dest exp
  | doExpTo ctx env dest (F.RecordEqualityExp fields)
    = let val (stmts, fields') = ListPair.unzip (List.map (fn (label, exp) => let val (stmts, exp') = doExp ctx env exp
                                                                              in (String.concat stmts, (label, exp'))
                                                                              end
                                                          ) fields)
      in String.concat stmts
         ^ putPureTo ctx env dest (case Syntax.extractTuple(1, fields) of
                                       SOME xs => "_recordEqual({" ^ String.concatWith ", " (List.map #2 fields') ^ "})"
                                     | NONE => "_recordEqual({" ^ String.concatWith ", " (List.map (fn (label, v) => "[" ^ LabelToLua label ^ "] = " ^ v) fields') ^ "})"
                                  )
      end
  | doExpTo ctx env dest (F.DataTagExp exp) = let val (stmts, exp') = doExp ctx env exp
                                              in String.concat stmts
                                                 ^ putPureTo ctx env dest (exp' ^ ".tag")
                                              end
  | doExpTo ctx env dest (F.DataPayloadExp exp) = let val (stmts, exp') = doExp ctx env exp
                                                  in String.concat stmts
                                                     ^ putPureTo ctx env dest (exp' ^ ".payload")
                                                  end

(* doDec : Context -> Env -> F.Dec -> string *)
and doDec ctx env (F.ValDec (F.SimpleBind(v, _, exp)))
    = let val luavid = VIdToLua v
      in indent env ^ "local " ^ luavid ^ "\n" ^ doExpTo ctx env (AssignTo luavid) exp
      end
  | doDec ctx env (F.ValDec (F.TupleBind([], exp)))
    = doExpTo ctx env Discard exp
  | doDec ctx env (F.ValDec (F.TupleBind(vars, exp)))
    = let val vars' = List.map (fn (v,_) => VIdToLua v) vars
          val decl = indent env ^ "local " ^ String.concatWith ", " vars' ^ "\n"
      in decl ^ doExpTo ctx env (UnpackingAssignTo vars') exp
      end
  | doDec ctx env (F.RecValDec valbinds)
    = let val (decls, assignments) = ListPair.unzip (List.map (fn F.SimpleBind (v,_,exp) => let val v' = VIdToLua v
                                                                                            in (indent env ^ "local " ^ v' ^ "\n", doExpTo ctx env (AssignTo v') exp)
                                                                                            end
                                                              | F.TupleBind ([], exp) => ("", doExpTo ctx env Discard exp)
                                                              | F.TupleBind (vars, exp) => let val vars' = List.map (fn (v,_) => VIdToLua v) vars
                                                                                           in (indent env ^ "local " ^ String.concatWith ", " vars' ^ "\n", doExpTo ctx env (UnpackingAssignTo vars') exp)
                                                                                           end
                                                              ) valbinds)
      in String.concat decls ^ String.concat assignments
      end
  | doDec ctx env (F.DatatypeDec datbinds) = String.concat (List.map (doDatBind ctx env) datbinds)
  | doDec ctx env (F.ExceptionDec exbind) = "" (* not implemented yet *)
and doDatBind ctx env (F.DatBind (tyvars, tycon, conbinds)) = String.concat (List.map (doConBind ctx env) conbinds) (* TODO: equality *)
and doConBind ctx env (F.ConBind (vid as USyntax.MkVId(name,_), NONE)) = "local " ^ VIdToLua vid ^ " = { tag = " ^ toLuaStringLit name ^ " }\n"
  | doConBind ctx env (F.ConBind (vid as USyntax.MkVId(name,_), SOME ty)) = "local function " ^ VIdToLua vid ^ "(x)\n  return { tag = " ^ toLuaStringLit name ^ ", payload = x }\nend\n"

fun doDecs ctx env decs = String.concat (List.map (doDec ctx env) decs)

end (* structure CodeGenLua *)
