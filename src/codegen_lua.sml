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
val builtins = let open InitialEnv
      in List.foldl USyntax.VIdMap.insert' USyntax.VIdMap.empty
                    [(* ref *)
                     (VId_ref, "_ref")
                    ,(VId_COLONEQUAL, "_set")
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
                    ,(VId_print, "_print")
                    ,(VId_Int_toString, "_Int_toString")
                    ,(VId_HAT, "_string_append")
                    ,(VId_not, "_not") (* Lua not *)
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
datatype Env = MkEnv

fun genSym (ctx: Context) = let val n = !(#nextLuaId ctx)
                                val _ = #nextLuaId ctx := n + 1
                            in "tmp" ^ Int.toString n
                            end

structure F = FSyntax

fun doLiteral (Syntax.IntegerConstant x) = if x < 0 then "(-" ^ Int.toString (~ x) ^ ")" else Int.toString x
  | doLiteral (Syntax.WordConstant x) = Word.toString x
  | doLiteral (Syntax.RealConstant x) = raise Fail "CodeGenLua: real constant not implemented yet"
  | doLiteral (Syntax.StringConstant x) = "\"" ^ String.toString x ^ "\"" (* TODO *)
  | doLiteral (Syntax.CharacterConstant x) = "\"" ^ String.toString x ^ "\"" (* TODO *)

fun doExp ctx env (F.SConExp scon): string list * string = ([], doLiteral scon)
  | doExp ctx env (F.VarExp (Syntax.MkQualified(_, vid))) = ([], VIdToLua vid) (* TODO: qualified identifier? *)
  | doExp ctx env (F.RecordExp []) = ([], "{}")
  | doExp ctx env (F.RecordExp fields) = (case Syntax.extractTuple(1, fields) of
                                              SOME xs => let val (stmts, ys) = ListPair.unzip (List.map (doExp ctx env) xs)
                                                         in (List.concat stmts, "{" ^ String.concatWith ", " ys ^ "}") (* TODO: evaluation order *)
                                                         end
                                            | NONE => let val (stmts, ys) = ListPair.unzip (List.map (fn (label, exp) => case doExp ctx env exp of (stmt, e) => (stmt, "[" ^ LabelToLua label ^ "] = " ^ e)) fields)
                                                      in (List.concat stmts, "{" ^ String.concatWith ", " ys ^ "}") (* TODO: evaluation order *)
                                                      end
                                         )
  | doExp ctx env (F.LetExp (F.SimpleBind (v, _, exp1), exp2))
    = let val (stmt1, exp1') = doExp ctx env exp1
          val (stmt2, exp2') = doExp ctx env exp2
      in (stmt1 @ ("local " ^ VIdToLua v ^ " = " ^ exp1' ^ "\n") :: stmt2, exp2')
      end
  | doExp ctx env (F.LetExp (F.TupleBind (vars, exp1), exp2))
    = let val (stmt1, exp1') = doExp ctx env exp1
          val (stmt2, exp2') = doExp ctx env exp2
          val stmt1' = case vars of
                           [] => "local _ = " ^ exp1' ^ "\n"
                         | _ => "local " ^ String.concatWith ", " (List.map (fn (v,_) => VIdToLua v) vars) ^ " = table.unpack(" ^ exp1' ^ ")\n"
      in (stmt1 @ stmt1' :: stmt2, exp2')
      end
  | doExp ctx env (F.LetRecExp (valbinds, exp2))
    = let val decls = List.map (fn valbind => case valbind of
                                                  F.SimpleBind (v,_,_) => "local " ^ VIdToLua v ^ "\n"
                                                | F.TupleBind ([], _) => ""
                                                | F.TupleBind (vars, _) => "local " ^ String.concatWith ", " (List.map (fn (v,_) => VIdToLua v) vars) ^ "\n"
                               ) valbinds
          val assignments = List.map (fn valbind =>
                                         case valbind of
                                             F.SimpleBind (v, _, exp1) => let val (stmts1, exp1') = doExp ctx env exp1
                                                                          in String.concat stmts1 ^ VIdToLua v ^ " = " ^ exp1'
                                                                          end
                                           | F.TupleBind ([], exp1) => let val (stmts1, exp1') = doExp ctx env exp1
                                                                       in String.concat stmts1 ^ "local _ = " ^ exp1' ^ "\n"
                                                                       end
                                           | F.TupleBind (vars, exp1) => let val (stmts1, exp1') = doExp ctx env exp1
                                                                         in String.concat stmts1 ^ String.concatWith ", " (List.map (fn (v,_) => VIdToLua v) vars) ^ " = table.unpack(" ^ exp1' ^ ")\n"
                                                                         end
                                ) valbinds
          val (stmts2, exp2') = doExp ctx env exp2
      in (decls @ assignments @ stmts2, exp2')
      end
  | doExp ctx env (F.AppExp(F.ProjectionExp { label = label, ... }, exp2))
    = let val (stmts, exp2') = doExp ctx env exp2
      in (stmts, "(" ^ exp2' ^ ")[" ^ LabelToLua(label) ^ "]")
      end
  | doExp ctx env (F.AppExp (exp1 as F.VarExp (Syntax.MkQualified([], vid)), exp2 as F.RecordExp [(Syntax.NumericLabel 1, e1), (Syntax.NumericLabel 2, e2)]))
    (* built-in operator? *)
    (* TODO: evaluation order *)
    (* TODO: check for overflow *)
    = let open InitialEnv
          fun doBinaryOp ope = let val (stmts1, e1') = doExp ctx env e1
                                 val (stmts2, e2') = doExp ctx env e2
                             in (stmts1 @ stmts2, "(" ^ e1' ^ ") " ^ ope ^ " (" ^ e2' ^ ")") (* TODO: evaluation order *)
                             end
          fun doBinaryFn name = let val (stmts1, e1') = doExp ctx env e1
                                    val (stmts2, e2') = doExp ctx env e2
                                in (stmts1 @ stmts2, name ^ "(" ^ e1' ^ ", " ^ e2' ^ ")") (* TODO: evaluation order *)
                                end
      in if USyntax.eqVId(vid, VId_EQUAL_bool)
            orelse USyntax.eqVId(vid, VId_EQUAL_int)
            orelse USyntax.eqVId(vid, VId_EQUAL_word)
            orelse USyntax.eqVId(vid, VId_EQUAL_string)
            orelse USyntax.eqVId(vid, VId_EQUAL_char)
            orelse USyntax.eqVId(vid, VId_EQUAL_exntag) then
             doBinaryOp "=="
         else if USyntax.eqVId(vid, VId_PLUS_int) then
             doBinaryFn "__add_int"
         else if USyntax.eqVId(vid, VId_PLUS_word)
                 orelse USyntax.eqVId(vid, VId_PLUS_real) then
             doBinaryOp "+"
         else if USyntax.eqVId(vid, VId_MINUS_int) then
             doBinaryFn "__sub_int"
         else if USyntax.eqVId(vid, VId_MINUS_word)
                 orelse USyntax.eqVId(vid, VId_MINUS_real) then
             doBinaryOp "-"
         else if USyntax.eqVId(vid, VId_TIMES_int) then
             doBinaryFn "__mul_int"
         else if USyntax.eqVId(vid, VId_TIMES_word)
                 orelse USyntax.eqVId(vid, VId_TIMES_real) then
             doBinaryOp "*"
         else if USyntax.eqVId(vid, VId_DIVIDE_real) then
             doBinaryOp "/"
         else if USyntax.eqVId(vid, VId_div_int) then
             doBinaryFn "__div_int"
         else if USyntax.eqVId(vid, VId_div_word) then
             doBinaryFn "__div_word"
         else if USyntax.eqVId(vid, VId_mod_int) then
             doBinaryFn "__mod_int"
         else if USyntax.eqVId(vid, VId_mod_word) then
             doBinaryFn "__mod_word"
         else if USyntax.eqVId(vid, VId_LT_int)
                 orelse USyntax.eqVId(vid, VId_LT_real)
                 orelse USyntax.eqVId(vid, VId_LT_string)
                 orelse USyntax.eqVId(vid, VId_LT_char) then
             doBinaryOp "<"
         else if USyntax.eqVId(vid, VId_LT_word) then
             doBinaryFn "__LT_word"
         else if USyntax.eqVId(vid, VId_GT_int)
                 orelse USyntax.eqVId(vid, VId_GT_real)
                 orelse USyntax.eqVId(vid, VId_GT_string)
                 orelse USyntax.eqVId(vid, VId_GT_char) then
             doBinaryOp ">"
         else if USyntax.eqVId(vid, VId_GT_word) then
             doBinaryFn "__GT_word"
         else if USyntax.eqVId(vid, VId_LE_int)
                 orelse USyntax.eqVId(vid, VId_LE_real)
                 orelse USyntax.eqVId(vid, VId_LE_string)
                 orelse USyntax.eqVId(vid, VId_LE_char) then
             doBinaryOp "<="
         else if USyntax.eqVId(vid, VId_LE_word) then
             doBinaryFn "__LE_word"
         else if USyntax.eqVId(vid, VId_GE_int)
                 orelse USyntax.eqVId(vid, VId_GE_real)
                 orelse USyntax.eqVId(vid, VId_GE_string)
                 orelse USyntax.eqVId(vid, VId_GE_char) then
             doBinaryOp ">="
         else if USyntax.eqVId(vid, VId_GE_word) then
             doBinaryFn "__GE_word"
	 else if USyntax.eqVId(vid, VId_HAT) then
	     doBinaryOp ".."
         else
             let val (stmts1, exp1') = doExp ctx env exp1
                 val (stmts2, exp2') = doExp ctx env exp2
             in (stmts1 @ stmts2, "(" ^ exp1' ^ ")(" ^ exp2' ^ ")") (* TODO: evaluation order *)
             end
      end
  | doExp ctx env (F.AppExp (exp1 as F.VarExp (Syntax.MkQualified([], vid)), exp2))
    (* built-in operator? *)
    (* TODO: check for overflow *)
    = let open InitialEnv
      in if USyntax.eqVId(vid, VId_TILDE_real) then
             let val (stmts, exp2') = doExp ctx env exp2
             in (stmts, "- (" ^ exp2' ^ ")")
             end
         else if USyntax.eqVId(vid, VId_not) then
             let val (stmts, exp2') = doExp ctx env exp2
             in (stmts, "not (" ^ exp2' ^ ")")
             end
         else
             let val (stmts1, exp1') = doExp ctx env exp1
                 val (stmts2, exp2') = doExp ctx env exp2
             in (stmts1 @ stmts2, "(" ^ exp1' ^ ")(" ^ exp2' ^ ")") (* TODO: evaluation order *)
             end
      end
  | doExp ctx env (F.AppExp (exp1, exp2))
    = let val (stmts1, exp1') = doExp ctx env exp1
          val (stmts2, exp2') = doExp ctx env exp2
      in (stmts1 @ stmts2, "(" ^ exp1' ^ ")(" ^ exp2' ^ ")") (* TODO: evaluation order *)
      end
  | doExp ctx env (F.HandleExp { body, exnName, handler } )
    = let val (stmts, body') = doExp ctx env body
          val status = genSym ctx
          val result = genSym ctx
          val (handlerstmts, handler') = doExp ctx env handler
      in ( ["local " ^ status ^ ", " ^ result ^ " = pcall(function()\n"] @ stmts @ ["return " ^ body' ^ "\nend)\n\
           \if not " ^ status ^ " then\n\
           \local " ^ VIdToLua exnName ^ " = " ^ result ^ "\n\
           \"] @ handlerstmts @ [result ^ " = " ^ handler' ^ "\n\
           \end\n"]
         , result
         )
      end
  | doExp ctx env (F.RaiseExp (span, exp))
    = let val (stmts, exp') = doExp ctx env exp
          val { start = { file, line, column }, ...} = span
      in (stmts, "_raise(" ^ exp' ^ ", \"" ^ String.toString file ^ "\", " ^ Int.toString line ^ ", " ^ Int.toString column ^ ")")
      end
  | doExp ctx env (exp as F.IfThenElseExp (exp1, exp2, exp3))
    = let fun doElseIf (F.IfThenElseExp(e1, e2, e3)) = let val (s1, e1') = doExp ctx env e1
                                                       in if List.null s1 then
                                                              "elseif " ^ e1' ^ " then\n"
                                                              ^ let val (s2, e2') = doExp ctx env e2
                                                                in String.concat s2 ^ "return " ^ e2' ^ "\n"
                                                                end
                                                              ^ doElseIf e3
                                                          else
                                                              "else\n"
                                                              ^ String.concat s1
                                                              ^ "if " ^ e1' ^ " then\n"
                                                              ^ let val (s2, e2') = doExp ctx env e2
                                                                in String.concat s2 ^ "return " ^ e2' ^ "\n"
                                                                end
                                                              ^ doElseIf e3
                                                              ^ "end\n"
                                                       end
            | doElseIf e = "else\n"
                           ^ let val (s, e') = doExp ctx env e
                             in String.concat s ^ "return " ^ e' ^ "\n"
                             end
      in ([], "(function()\n"
              ^ let val (stmts1, exp1') = doExp ctx env exp1
                in String.concat stmts1 ^ "if " ^ exp1' ^ " then\n"
                end
              ^ let val (stmts2, exp2') = doExp ctx env exp2
                in String.concat stmts2 ^ "return " ^ exp2' ^ "\n"
                end
              ^ doElseIf exp3
              ^ "end\n"
              ^ "end)()" (* TODO: Use 'and' / 'or' if we can *)
               )
      end
  | doExp ctx env (F.CaseExp _) = raise Fail "CodeGenLua: CaseExp should have been desugared earlier"
  | doExp ctx env (F.FnExp (vid, _, exp)) = ([], "function(" ^ VIdToLua(vid) ^ ")\n"
                                                 ^ let val (stmts, exp') = doExp ctx env exp
                                                   in String.concat stmts ^ "return " ^ exp' ^ "\n" (* TODO: update environment? *)
                                                   end
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
                                           in (stmts, "(" ^ exp' ^ ").payload")
                                           end

fun doDec ctx env (F.ValDec (F.SimpleBind(v, _, exp)))
    = let val (stmts, exp') = doExp ctx env exp
      in String.concat stmts ^ "local " ^ VIdToLua v ^ " = " ^ exp' ^ "\n"
      end
  | doDec ctx env (F.ValDec (F.TupleBind(vars, exp)))
    = let val (stmts, exp') = doExp ctx env exp
      in String.concat stmts ^ (case vars of
                                    [] => "local _ = " ^ exp' ^ "\n"
                                  | _ => "local " ^ String.concatWith ", " (List.map (fn (v,_) => VIdToLua v) vars) ^ " = table.unpack(" ^ exp' ^ ")\n"
                               )
      end
  | doDec ctx env (F.RecValDec valbinds)
    = String.concat (List.map (fn valbind => case valbind of
                                                 F.SimpleBind (v, _, _) => "local " ^ VIdToLua v ^ "\n"
                                               | F.TupleBind ([], _) => ""
                                               | F.TupleBind (vars, _) => "local " ^ String.concatWith ", " (List.map (fn (v,_) => VIdToLua v) vars) ^ "\n"
                              ) valbinds)
      ^ String.concat (List.map (fn valbind =>
                                    case valbind of
                                        F.SimpleBind (v, _, exp1) => let val (stmts1, exp1') = doExp ctx env exp1
                                                                     in String.concat stmts1 ^ VIdToLua v ^ " = " ^ exp1' ^ "\n"
                                                                     end
                                      | F.TupleBind ([], exp1) => let val (stmts1, exp1') = doExp ctx env exp1
                                                                  in String.concat stmts1 ^ "local _ = " ^ exp1' ^ "\n"
                                                                  end
                                      | F.TupleBind (vars, exp1) => let val (stmts1, exp1') = doExp ctx env exp1
                                                                    in String.concat stmts1 ^ String.concatWith ", " (List.map (fn (v,_) => VIdToLua v) vars) ^ " = table.unpack(" ^ exp1' ^ ")\n"
                                                                    end
                                ) valbinds)

fun doDecs ctx env decs = String.concat (List.map (doDec ctx env) decs)

fun doTopDecs ctx env decs = String.concat (List.map (doTopDec ctx env) decs)
and doTopDec ctx env (USyntax.TypeDec (span, _)) = ""
  | doTopDec ctx env (USyntax.DatatypeDec (span, datbinds)) = String.concat (List.map (doDatBind ctx env) datbinds)
  | doTopDec ctx env (USyntax.DatatypeRepDec (span, _, _)) = ""
  | doTopDec ctx env (USyntax.AbstypeDec (span, _, _)) = ""
  | doTopDec ctx env (USyntax.ExceptionDec (span, _)) = ""
and doDatBind ctx env (USyntax.DatBind (span, tyvars, tycon, conbinds)) = String.concat (List.map (doConBind ctx env) conbinds) (* TODO: equality *)
and doConBind ctx env (USyntax.ConBind (span, vid as USyntax.MkVId(name,_), NONE)) = "local " ^ VIdToLua vid ^ " = { tag = \"" ^ String.toString name ^ "\" }\n" (* TODO *)
  | doConBind ctx env (USyntax.ConBind (span, vid as USyntax.MkVId(name,_), SOME ty)) = "local function " ^ VIdToLua vid ^ "(x)\n  return { tag = \"" ^ String.toString name ^ "\", payload = x }\nend\n" (* TODO *)

end (* structure CodeGenLua *)
