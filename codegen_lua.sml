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
fun VIdToLua(vid as USyntax.MkVId(name, n)) = if vid = InitialEnv.VId_true then
                                                  "true"
                                              else if vid = InitialEnv.VId_false then
                                                  "false"
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

fun doExp ctx env (F.SConExp (Syntax.IntegerConstant x)) = if x < 0 then "(-" ^ Int.toString (~ x) ^ ")" else Int.toString x
  | doExp ctx env (F.SConExp (Syntax.WordConstant x)) = Word.toString x
  | doExp ctx env (F.SConExp (Syntax.RealConstant x)) = raise Fail "CodeGenLua: real constant not implemented yet"
  | doExp ctx env (F.SConExp (Syntax.StringConstant x)) = "\"" ^ String.toString x ^ "\"" (* TODO *)
  | doExp ctx env (F.SConExp (Syntax.CharacterConstant x)) = "\"" ^ String.toString x ^ "\"" (* TODO *)
  | doExp ctx env (F.VarExp (Syntax.MkQualified([], vid))) = VIdToLua vid
  | doExp ctx env (F.VarExp (Syntax.MkQualified(_ :: _, _))) = raise Fail "CodeGenLua: qualified identifiers are not implemented yet"
  | doExp ctx env (F.RecordExp []) = "{}"
  | doExp ctx env (F.RecordExp fields) = (case Syntax.extractTuple(1, fields) of
                                              SOME xs => "{" ^ String.concatWith ", " (List.map (doExp ctx env) xs) ^ "}" (* TODO: evaluation order *)
                                            | NONE => "{" ^ String.concatWith ", " (List.map (fn (label, exp) => "[" ^ LabelToLua label ^ "] = " ^ doExp ctx env exp) fields) ^ "}" (* TODO: evaluation order *)
                                         )
  | doExp ctx env (F.LetExp (F.SimpleBind (v, _, exp1), exp2))
    = "(function()\n"
      ^ "local " ^ VIdToLua v ^ " = " ^ doExp ctx env exp1 ^ "\n"
      ^ "return " ^ doExp ctx env exp2 (* TODO: update environment? *)
      ^ "\nend)()"
  | doExp ctx env (F.LetExp (F.TupleBind (vars, exp1), exp2))
    = "(function()\n"
      ^ (case vars of
             [] => "local _ = " ^ doExp ctx env exp1 ^ "\n"
           | _ => "local " ^ String.concatWith ", " (List.map (fn (v,_) => VIdToLua v) vars) ^ " = table.unpack(" ^ doExp ctx env exp1 ^ ")\n"
        )
      ^ "return " ^ doExp ctx env exp2 (* TODO: update environment? *)
      ^ "\nend)()"
  | doExp ctx env (F.LetRecExp (valbinds, exp2))
    = "(function()\n"
      ^ String.concat (List.map (fn valbind => case valbind of
                                                   F.SimpleBind (v,_,_) => "local " ^ VIdToLua v ^ "\n"
                                                 | F.TupleBind ([], _) => ""
                                                 | F.TupleBind (vars, _) => "local " ^ String.concatWith ", " (List.map (fn (v,_) => VIdToLua v) vars) ^ "\n"
                                ) valbinds)
      ^ String.concat (List.map (fn valbind => case valbind of
                                                   F.SimpleBind (v,_,exp1) => VIdToLua v ^ " = " ^ doExp ctx env exp1 ^ "\n"
                                                 | F.TupleBind ([], exp1) => "local _ = " ^ doExp ctx env exp1 ^ "\n"
                                                 | F.TupleBind (vars, exp1) => String.concatWith ", " (List.map (fn (v,_) => VIdToLua v) vars) ^ " = table.unpack(" ^ doExp ctx env exp1 ^ ")\n"
                    ) valbinds)
      ^ "return " ^ doExp ctx env exp2 (* TODO: update environment? *)
      ^ "\nend)()"
  | doExp ctx env (F.AppExp(F.ProjectionExp { label = label, ... }, exp2)) = "(" ^ doExp ctx env exp2 ^ ")[" ^ LabelToLua(label) ^ "]"
  | doExp ctx env (F.AppExp (exp1 as F.VarExp (Syntax.MkQualified([], vid)), exp2 as F.RecordExp [(Syntax.NumericLabel 1, e1), (Syntax.NumericLabel 2, e2)]))
    (* built-in operator? *)
    (* TODO: evaluation order *)
    (* TODO: check for overflow *)
    = let open InitialEnv
      in if USyntax.eqVId(vid, VId_EQUAL_bool)
            orelse USyntax.eqVId(vid, VId_EQUAL_int)
            orelse USyntax.eqVId(vid, VId_EQUAL_word)
            orelse USyntax.eqVId(vid, VId_EQUAL_string)
            orelse USyntax.eqVId(vid, VId_EQUAL_char) then
             "(" ^ doExp ctx env e1 ^ ") == (" ^ doExp ctx env e2 ^ ")"
         else if USyntax.eqVId(vid, VId_PLUS_int)
                 orelse USyntax.eqVId(vid, VId_PLUS_word)
                 orelse USyntax.eqVId(vid, VId_PLUS_real) then
             "(" ^ doExp ctx env e1 ^ ") + (" ^ doExp ctx env e2 ^ ")"
         else if USyntax.eqVId(vid, VId_MINUS_int)
                 orelse USyntax.eqVId(vid, VId_MINUS_word)
                 orelse USyntax.eqVId(vid, VId_MINUS_real) then
             "(" ^ doExp ctx env e1 ^ ") - (" ^ doExp ctx env e2 ^ ")"
         else if USyntax.eqVId(vid, VId_TIMES_int)
                 orelse USyntax.eqVId(vid, VId_TIMES_word)
                 orelse USyntax.eqVId(vid, VId_TIMES_real) then
             "(" ^ doExp ctx env e1 ^ ") * (" ^ doExp ctx env e2 ^ ")"
         else if USyntax.eqVId(vid, VId_DIVIDE_real) then
             "(" ^ doExp ctx env e1 ^ ") / (" ^ doExp ctx env e2 ^ ")"
         else if USyntax.eqVId(vid, VId_div_int) then
             "(" ^ doExp ctx env e1 ^ ") // (" ^ doExp ctx env e2 ^ ")" (* flooring division *)
         else if USyntax.eqVId(vid, VId_mod_int) then
             "(" ^ doExp ctx env e1 ^ ") % (" ^ doExp ctx env e2 ^ ")" (* modulo w.r.t. flooring division *)
         else if USyntax.eqVId(vid, VId_LT_int)
                 orelse USyntax.eqVId(vid, VId_LT_word) (* TODO: should use math.ult (Lua 5.3) *)
                 orelse USyntax.eqVId(vid, VId_LT_real)
                 orelse USyntax.eqVId(vid, VId_LT_string)
                 orelse USyntax.eqVId(vid, VId_LT_char) then
             "(" ^ doExp ctx env e1 ^ ") < (" ^ doExp ctx env e2 ^ ")"
         else if USyntax.eqVId(vid, VId_GT_int)
                 orelse USyntax.eqVId(vid, VId_GT_word) (* TODO: should use math.ult (Lua 5.3) *)
                 orelse USyntax.eqVId(vid, VId_GT_real)
                 orelse USyntax.eqVId(vid, VId_GT_string)
                 orelse USyntax.eqVId(vid, VId_GT_char) then
             "(" ^ doExp ctx env e1 ^ ") > (" ^ doExp ctx env e2 ^ ")"
         else if USyntax.eqVId(vid, VId_LE_int)
                 orelse USyntax.eqVId(vid, VId_LE_word) (* TODO: should use math.ult (Lua 5.3) *)
                 orelse USyntax.eqVId(vid, VId_LE_real)
                 orelse USyntax.eqVId(vid, VId_LE_string)
                 orelse USyntax.eqVId(vid, VId_LE_char) then
             "(" ^ doExp ctx env e1 ^ ") <= (" ^ doExp ctx env e2 ^ ")"
         else if USyntax.eqVId(vid, VId_GE_int)
                 orelse USyntax.eqVId(vid, VId_GE_word) (* TODO: should use math.ult (Lua 5.3) *)
                 orelse USyntax.eqVId(vid, VId_GE_real)
                 orelse USyntax.eqVId(vid, VId_GE_string)
                 orelse USyntax.eqVId(vid, VId_GE_char) then
             "(" ^ doExp ctx env e1 ^ ") >= (" ^ doExp ctx env e2 ^ ")"
         else
             (* TODO: div, mod -> //, %*)
             "(" ^ doExp ctx env exp1 ^ ")(" ^ doExp ctx env exp2 ^ ")"
      end
  | doExp ctx env (F.AppExp (exp1 as F.VarExp (Syntax.MkQualified([], vid)), exp2))
    (* built-in operator? *)
    (* TODO: check for overflow *)
    = let open InitialEnv
      in if USyntax.eqVId(vid, VId_abs_int)
            orelse USyntax.eqVId(vid, VId_abs_real) then
             "math.abs(" ^ doExp ctx env exp2 ^ ")"
         else if USyntax.eqVId(vid, VId_TILDE_int)
                 orelse USyntax.eqVId(vid, VId_TILDE_real) then
             "- (" ^ doExp ctx env exp2 ^ ")"
         else
             "(" ^ doExp ctx env exp1 ^ ")(" ^ doExp ctx env exp2 ^ ")" (* TODO: evaluation order *)
      end
  | doExp ctx env (F.AppExp (exp1, exp2)) = "(" ^ doExp ctx env exp1 ^ ")(" ^ doExp ctx env exp2 ^ ")" (* TODO: evaluation order *)
  | doExp ctx env (F.IfThenElseExp (exp1, exp2, exp3))
    = let fun doElseIf (F.IfThenElseExp(e1, e2, e3)) = "elseif " ^ doExp ctx env e1 ^ " then\n"
                                                       ^ "return " ^ doExp ctx env e2 ^ "\n"
                                                       ^ doElseIf e3
            | doElseIf e = "else\n"
                           ^ "return " ^ doExp ctx env e ^ "\n"
      in "(function()\n"
         ^ "if " ^ doExp ctx env exp1 ^ " then\n"
         ^ "return " ^ doExp ctx env exp2 ^ "\n"
         ^ doElseIf exp3
         ^ "end\n"
         ^ "end)()" (* TODO: Use and / or if we can *)
      end
  | doExp ctx env (F.CaseExp _) = raise Fail "CodeGenLua: CaseExp should have been desugared earlier"
  | doExp ctx env (F.FnExp (vid, _, exp)) = "function(" ^ VIdToLua(vid) ^ ")\n"
                                            ^ "return " ^ doExp ctx env exp ^ "\n" (* TODO: update environment? *)
                                            ^ "end"
  | doExp ctx env (F.ProjectionExp { label = label, ... }) = "function(x) return x[" ^ LabelToLua(label) ^ "] end"
  | doExp ctx env (F.TyAbsExp (_, exp)) = doExp ctx env exp
  | doExp ctx env (F.TyAppExp (exp, _)) = doExp ctx env exp
  | doExp ctx env (F.RecordEqualityExp fields) = (case Syntax.extractTuple(1, fields) of
                                                      SOME xs => "_recordEqual({" ^ String.concatWith ", " (List.map (doExp ctx env) xs) ^ "})"
                                                    | NONE => "_recordEqual({" ^ String.concatWith ", " (List.map (fn (label, exp) => "[" ^ LabelToLua label ^ "] = " ^ doExp ctx env exp) fields) ^ "})"
                                                 )
  | doExp ctx env (F.DataTagExp exp) = "(" ^ doExp ctx env exp ^ ").tag"
  | doExp ctx env (F.DataPayloadExp exp) = "(" ^ doExp ctx env exp ^ ").payload"

fun doDec ctx env (F.ValDec (F.SimpleBind(v, _, exp)))
    = "local " ^ VIdToLua v ^ " = " ^ doExp ctx env exp ^ "\n"
  | doDec ctx env (F.ValDec (F.TupleBind([], exp)))
    = "local _ = " ^ doExp ctx env exp ^ "\n"
  | doDec ctx env (F.ValDec (F.TupleBind(vars, exp)))
    = "local " ^ String.concatWith ", " (List.map (fn (v,_) => VIdToLua v) vars) ^ " = table.unpack(" ^ doExp ctx env exp ^ ")\n"
  | doDec ctx env (F.RecValDec valbinds)
    = String.concat (List.map (fn valbind => case valbind of
                                                 F.SimpleBind (v, _, _) => "local " ^ VIdToLua v
                                               | F.TupleBind ([], _) => ""
                                               | F.TupleBind (vars, _) => "local " ^ String.concatWith ", " (List.map (fn (v,_) => VIdToLua v) vars) ^ "\n"
                              ) valbinds)
      ^ String.concat (List.map (fn valbind => case valbind of
                                                   F.SimpleBind (v, _, exp1) => VIdToLua v ^ " = " ^ doExp ctx env exp1
                                                 | F.TupleBind ([], exp1) => "local _ = " ^ doExp ctx env exp1 ^ "\n"
                                                 | F.TupleBind (vars, exp1) => String.concatWith ", " (List.map (fn (v,_) => VIdToLua v) vars) ^ " = table.unpack(" ^ doExp ctx env exp1 ^ ")\n"
                                ) valbinds)

fun doDecs ctx env decs = String.concat (List.map (doDec ctx env) decs)

end (* structure CodeGenLua *)
