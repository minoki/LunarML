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
  | LabelToLua(Syntax.IdentifierLabel(s)) = String.toString s (* TODO: pretty-printing? *)

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
  | doExp ctx env (F.SConExp (Syntax.StringConstant x)) = String.toString x (* TODO *)
  | doExp ctx env (F.SConExp (Syntax.CharacterConstant x)) = String.toString x (* TODO *)
  | doExp ctx env (F.VarExp (Syntax.MkQualified([], vid))) = VIdToLua vid
  | doExp ctx env (F.VarExp (Syntax.MkQualified(_ :: _, _))) = raise Fail "CodeGenLua: qualified identifiers are not implemented yet"
  | doExp ctx env (F.RecordExp []) = "{}"
  | doExp ctx env (F.RecordExp fields) = "{" ^ String.concatWith ", " (List.map (fn (label, exp) => "[" ^ LabelToLua label ^ "] = " ^ doExp ctx env exp) fields) ^ "}" (* TODO: evaluation order *)
  | doExp ctx env (F.LetExp (F.TupleBind (vars, exp1), exp2))
    = "(function()\n"
      ^ (case vars of
             [(v,_)] => "local " ^ VIdToLua v ^ " = table.unpack(" ^ doExp ctx env exp1 ^ ")\n"
           | _ => "local " ^ String.concatWith ", " (List.map (fn (v,_) => VIdToLua v) vars) ^ " = table.unpack(" ^ doExp ctx env exp1 ^ ")\n"
        )
      ^ "return " ^ doExp ctx env exp2 (* TODO: update environment? *)
      ^ "\nend)()"
  | doExp ctx env (F.LetRecExp (valbinds, exp2))
    = "(function()\n"
      ^ String.concat (List.map (fn (F.TupleBind (vars, _)) => "local " ^ String.concatWith ", " (List.map (fn (v,_) => VIdToLua v) vars) ^ "\n") valbinds)
      ^ String.concat (List.map (fn (F.TupleBind (vars, exp1)) => case vars of
                                                                      [(v,_)] => VIdToLua v ^ " = table.unpack(" ^ doExp ctx env exp1 ^ ")\n"
                                                                    | _ => String.concatWith ", " (List.map (fn (v,_) => VIdToLua v) vars) ^ " = table.unpack(" ^ doExp ctx env exp1 ^ ")\n"
                    ) valbinds)
      ^ "return " ^ doExp ctx env exp2 (* TODO: update environment? *)
      ^ "\nend)()"
  | doExp ctx env (F.AppExp(F.ProjectionExp { label = label, ... }, exp2)) = "(" ^ doExp ctx env exp2 ^ ")[" ^ LabelToLua(label) ^ "]"
  | doExp ctx env (F.AppExp (exp1, exp2)) = "(" ^ doExp ctx env exp1 ^ ")(" ^ doExp ctx env exp2 ^ ")" (* TODO: evaluation order *)
                                                                                                       (* TODO: built-in functions *)
  | doExp ctx env (F.IfThenElseExp (exp1, exp2, exp3))
    = "(function()\n"
      ^ "if " ^ doExp ctx env exp1 ^ " then\n"
      ^ "return " ^ doExp ctx env exp2 ^ "\n"
      ^ "else\n"
      ^ "return " ^ doExp ctx env exp3 ^ "\n"
      ^ "end\n"
      ^ "end)()" (* TODO: Use and / or if we can *) (* TODO: Turn nested if-then-else'es into if-then-elseif-end *)
  | doExp ctx env (F.CaseExp _) = raise Fail "CodeGenLua: CaseExp should have been desugared earlier"
  | doExp ctx env (F.FnExp (vid, _, exp)) = "function(" ^ VIdToLua(vid) ^ ")\n"
                                            ^ "return " ^ doExp ctx env exp ^ "\n" (* TODO: update environment? *)
                                            ^ "end"
  | doExp ctx env (F.ProjectionExp { label = label, ... }) = "function(x) return x[" ^ LabelToLua(label) ^ "] end"
  | doExp ctx env (F.TyAbsExp (_, exp)) = doExp ctx env exp
  | doExp ctx env (F.TyAppExp (exp, _)) = doExp ctx env exp
  | doExp ctx env (F.RecordEqualityExp fields) = "_recordEqual({" ^ String.concatWith ", " (List.map (fn (label, exp) => "[" ^ LabelToLua label ^ "] = " ^ doExp ctx env exp) fields) ^ "})"
  | doExp ctx env (F.DataTagExp exp) = "(" ^ doExp ctx env exp ^ ").tag"
  | doExp ctx env (F.DataPayloadExp exp) = "(" ^ doExp ctx env exp ^ ").payload"

fun doDec ctx env (F.ValDec (F.TupleBind([(v,_)], exp)))
    = "local " ^ VIdToLua v ^ " = table.unpack(" ^ doExp ctx env exp ^ ")\n"
  | doDec ctx env (F.ValDec (F.TupleBind(vars, exp)))
    = "local " ^ String.concatWith ", " (List.map (fn (v,_) => VIdToLua v) vars) ^ " = table.unpack(" ^ doExp ctx env exp ^ ")\n"
  | doDec ctx env (F.RecValDec valbinds)
    = String.concat (List.map (fn (F.TupleBind (vars, _)) => "local " ^ String.concatWith ", " (List.map (fn (v,_) => VIdToLua v) vars) ^ "\n") valbinds)
      ^ String.concat (List.map (fn (F.TupleBind (vars, exp1)) => case vars of
                                                                      [(v,_)] => VIdToLua v ^ " = table.unpack(" ^ doExp ctx env exp1 ^ ")\n"
                                                                    | _ => String.concatWith ", " (List.map (fn (v,_) => VIdToLua v) vars) ^ " = table.unpack(" ^ doExp ctx env exp1 ^ ")\n"
                                ) valbinds)

fun doDecs ctx env decs = String.concat (List.map (doDec ctx env) decs)

end (* structure CodeGenLua *)
