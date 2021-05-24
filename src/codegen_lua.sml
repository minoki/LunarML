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
                    ,(VId_Int_PLUS, "_add_int") (* may raise Overflow *)
                    ,(VId_Int_MINUS, "_sub_int") (* may raise Overflow *)
                    ,(VId_Int_TIMES, "_mul_int") (* may raise Overflow *)
                    ,(VId_Int_abs, "_abs_int") (* may raise Overflow *)
                    ,(VId_Int_TILDE, "_negate_int") (* may raise Overflow *)
                    ,(VId_Int_div, "_div_int") (* may raise Overflow/Div *)
                    ,(VId_Int_mod, "_mod_int") (* may raise Div *)
                    ,(VId_Int_LT, "_LT")
                    ,(VId_Int_GT, "_GT")
                    ,(VId_Int_LE, "_LE")
                    ,(VId_Int_GE, "_GE")
                    (* word *)
                    ,(VId_Word_PLUS, "_PLUS") (* Lua +; does not raise Overflow *)
                    ,(VId_Word_MINUS, "_MINUS") (* Lua - (binary); does not raise Overflow *)
                    ,(VId_Word_TIMES, "_TIMES") (* Lua *; does not raise Overflow *)
                    ,(VId_Word_div, "_div_word") (* may raise Div *)
                    ,(VId_Word_mod, "_mod_word") (* may raise Div *)
                    ,(VId_Word_TILDE, "_unm") (* Lua - (unary) *)
                    ,(VId_Word_LT, "_LT_word")
                    ,(VId_Word_GT, "_GT_word")
                    ,(VId_Word_LE, "_LE_word")
                    ,(VId_Word_GE, "_GE_word")
                    (* real *)
                    ,(VId_Real_PLUS, "_PLUS") (* Lua + *)
                    ,(VId_Real_MINUS, "_MINUS") (* Lua - (binary) *)
                    ,(VId_Real_TIMES, "_TIMES") (* Lua * *)
                    ,(VId_Real_DIVIDE, "_DIVIDE") (* Lua / *)
                    ,(VId_Real_abs, "_abs_real") (* Lua math.abs *)
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
                    ,(VId_Lua_sub, "_lua_sub")
                    ,(VId_Lua_set, "_lua_set")
                    ,(VId_Lua_global, "_lua_global")
                    ,(VId_Lua_call, "_lua_call")
                    ,(VId_Lua_method, "_lua_method")
                    ,(VId_Lua_NIL, "nil") (* literal *)
                    ,(VId_Lua_isNil, "_lua_isNil")
                    ,(VId_Lua_isFalsy, "_not")
                    ,(VId_Lua_unsafeToValue, "_id") (* no-op *)
                    ,(VId_Lua_unsafeFromValue, "_id") (* no-op *)
                    ,(VId_Lua_newTable, "_lua_newTable")
                    ,(VId_Lua_function, "_lua_function")
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
                    ,(VId_Int_PLUS,     (NamedBinaryFn "__add_int", false))
                    ,(VId_Word_PLUS,    (InfixOp "+", true))
                    ,(VId_Real_PLUS,    (InfixOp "+", true))
                    ,(VId_Int_MINUS,    (NamedBinaryFn "__sub_int", false))
                    ,(VId_Word_MINUS,   (InfixOp "-", true))
                    ,(VId_Real_MINUS,   (InfixOp "-", true))
                    ,(VId_Int_TIMES,    (NamedBinaryFn "__mul_int", false))
                    ,(VId_Word_TIMES,   (InfixOp "*", true))
                    ,(VId_Real_TIMES,   (InfixOp "*", true))
                    ,(VId_Real_DIVIDE,  (InfixOp "/", true))
                    ,(VId_Int_div,      (NamedBinaryFn "__div_int", false))
                    ,(VId_Word_div,     (NamedBinaryFn "__div_word", false))
                    ,(VId_Int_mod,      (NamedBinaryFn "__mod_int", false))
                    ,(VId_Word_mod,     (NamedBinaryFn "__mod_word", false))
                    ,(VId_Int_LT,       (InfixOp "<", true))
                    ,(VId_Real_LT,      (InfixOp "<", true))
                    ,(VId_String_LT,    (InfixOp "<", true))
                    ,(VId_Char_LT,      (InfixOp "<", true))
                    ,(VId_Word_LT,      (NamedBinaryFn "__LT_word", true))
                    ,(VId_Int_LE,       (InfixOp "<=", true))
                    ,(VId_Real_LE,      (InfixOp "<=", true))
                    ,(VId_String_LE,    (InfixOp "<=", true))
                    ,(VId_Char_LE,      (InfixOp "<=", true))
                    ,(VId_Word_LE,      (NamedBinaryFn "__LE_word", true))
                    ,(VId_Int_GT,       (InfixOp ">", true))
                    ,(VId_Real_GT,      (InfixOp ">", true))
                    ,(VId_String_GT,    (InfixOp ">", true))
                    ,(VId_Char_GT,      (InfixOp ">", true))
                    ,(VId_Word_GT,      (NamedBinaryFn "__GT_word", true))
                    ,(VId_Int_GE,       (InfixOp ">=", true))
                    ,(VId_Real_GE,      (InfixOp ">=", true))
                    ,(VId_String_GE,    (InfixOp ">=", true))
                    ,(VId_Char_GE,      (InfixOp ">=", true))
                    ,(VId_Word_GE,      (NamedBinaryFn "__GE_word", true))
                    ,(VId_String_HAT,   (InfixOp "..", true))
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
  | doLiteral (Syntax.WordConstant x) = "0x" ^ Word.toString x
  | doLiteral (Syntax.RealConstant x) = (if String.sub (x, 0) = #"~" then
                                             "(" ^ String.map (fn #"~" => #"-" | c => c) x ^ ")"
                                         else
                                             String.map (fn #"~" => #"-" | c => c) x
                                        )
  | doLiteral (Syntax.StringConstant x) = toLuaStringLit x
  | doLiteral (Syntax.CharacterConstant x) = toLuaStringLit x

datatype Destination = Return
                     | AssignTo of string
                     | UnpackingAssignTo of string list
                     | Discard
                     | Continue of (* statements *) string list * (* pure expression *) string -> string (* the continuation should be called exactly once, and the expression should be used only once *)

(* mapCont : ('a * ('b -> 'r) -> 'r) -> 'a list -> ('b list -> 'r) -> 'r *)
fun mapCont f [] cont = cont []
  | mapCont f (x :: xs) cont = f (x, fn y => mapCont f xs (fn ys => cont (y :: ys)))

(* doExpTo : Context -> Env -> F.Exp -> Destination -> string *)
fun putPureTo ctx env Return (stmts, exp : string) = String.concat stmts ^ indent env ^ "return " ^ exp ^ "\n"
  | putPureTo ctx env (AssignTo v) (stmts, exp) = String.concat stmts ^ indent env ^ v ^ " = " ^ exp ^ "\n"
  | putPureTo ctx env (UnpackingAssignTo v) (stmts, exp) = String.concat stmts ^ indent env ^ String.concatWith ", " v ^ " = table.unpack(" ^ exp ^ ", 1, " ^ Int.toString (List.length v) ^ ")\n"
  | putPureTo ctx env Discard (stmts, exp) = String.concat stmts
  | putPureTo ctx env (Continue cont) (stmts, exp) = cont (stmts, exp)
and putImpureTo ctx env Return (stmts, exp : string) = String.concat stmts ^ indent env ^ "return " ^ exp ^ "\n"
  | putImpureTo ctx env (AssignTo v) (stmts, exp) = String.concat stmts ^ indent env ^ v ^ " = " ^ exp ^ "\n"
  | putImpureTo ctx env (UnpackingAssignTo v) (stmts, exp) = String.concat stmts ^ indent env ^ String.concatWith ", " v ^ " = table.unpack(" ^ exp ^ ", 1, " ^ Int.toString (List.length v) ^ ")\n"
  | putImpureTo ctx env Discard (stmts, exp) = String.concat stmts ^ indent env ^ "local _ = " ^ exp ^ "\n"
  | putImpureTo ctx env (Continue cont) (stmts, exp) = let val dest = genSym ctx
                                                       in cont (stmts @ [indent env ^ "local " ^ dest ^ " = " ^ exp ^ "\n"], dest)
                                                       end
and doExpCont ctx env exp cont = doExpTo ctx env exp (Continue cont)
and doExpTo ctx env (F.SConExp scon) dest : string = putPureTo ctx env dest ([], doLiteral scon)
  | doExpTo ctx env (F.VarExp (Syntax.MkQualified (_, vid))) dest = putPureTo ctx env dest ([], VIdToLua vid)
  | doExpTo ctx env (F.RecordExp []) dest = putPureTo ctx env dest ([], "_unit")
  | doExpTo ctx env (F.RecordExp fields) Discard = String.concat (List.map (fn (_, exp) => doExpTo ctx env exp Discard) fields)
  | doExpTo ctx env (F.RecordExp fields) dest
    = mapCont (fn ((label, exp), cont) => doExpCont ctx env exp (fn (stmts, e) => cont (stmts, (label, e))))
              fields
              (fn ys => let val (stmts, fields') = ListPair.unzip ys
                        in putPureTo ctx env dest (List.concat stmts
                                                  , case Syntax.extractTuple(1, fields) of
                                                        SOME _ => "{" ^ String.concatWith ", " (List.map #2 fields') ^ "}"
                                                      | NONE => "{" ^ String.concatWith ", " (List.map (fn (label, e) => "[" ^ LabelToLua label ^ "] = " ^ e) fields') ^ "}"
                                                  )
                        end
              )
  | doExpTo ctx env (F.LetExp (dec, exp)) (Continue cont)
    = let val dec' = doDec ctx env dec
      in doExpCont ctx env exp (fn (stmts, exp) => cont (dec' :: stmts, exp))
      end
  | doExpTo ctx env (F.LetExp (dec, exp)) dest
    = let val dec' = doDec ctx env dec
      in dec' ^ doExpTo ctx env exp dest
      end
  | doExpTo ctx env (F.AppExp (exp1, exp2)) dest
    = let val doProjection = case exp1 of
                                 F.ProjectionExp { label, ... } =>
                                 SOME (fn () => doExpCont ctx env exp2 (fn (stmts, exp2') => putPureTo ctx env dest (stmts, "(" ^ exp2' ^ ")[" ^ LabelToLua label ^ "]")))
                               | _ => NONE
          val doBinary = case (exp1, exp2) of
                             (F.VarExp (Syntax.MkQualified([], vid)), F.RecordExp [(Syntax.NumericLabel 1, e1), (Syntax.NumericLabel 2, e2)]) =>
                             let fun wrap f = SOME (fn () => doExpCont ctx env e1 (fn (stmts1, e1') => doExpCont ctx env e2 (fn (stmts2, e2') => f (stmts1 @ stmts2, e1', e2'))))
                             in case USyntax.VIdMap.find(builtinBinaryOps, vid) of
                                    SOME (binop, pure) => wrap (fn (stmts, e1', e2') =>
                                                                   let val e = case binop of
                                                                                   InfixOp luaop => "(" ^ e1' ^ ") " ^ luaop ^ " (" ^ e2' ^ ")"
                                                                                 | NamedBinaryFn luafn => luafn ^ "(" ^ e1' ^ ", " ^ e2' ^ ")"
                                                                   in if pure then
                                                                          putPureTo ctx env dest (stmts, e)
                                                                      else
                                                                          putImpureTo ctx env dest (stmts, e)
                                                                   end
                                                               )
                                  | NONE => if USyntax.eqVId(vid, InitialEnv.VId_Lua_sub) then
                                                wrap (fn (stmts, e1', e2') =>
                                                         let val e = "(" ^ e1' ^ ")[" ^ e2' ^ "]"
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
                            in if USyntax.eqVId(vid, VId_Real_TILDE) orelse USyntax.eqVId(vid, VId_Word_TILDE) then
                                   wrap (fn (stmts, e2') => putPureTo ctx env dest (stmts, "- (" ^ e2' ^ ")"))
                               else if USyntax.eqVId(vid, VId_Bool_not) orelse USyntax.eqVId(vid, VId_Lua_isFalsy) then
                                   wrap (fn (stmts, e2') => putPureTo ctx env dest (stmts, "not (" ^ e2' ^ ")"))
                               else if USyntax.eqVId(vid, VId_EXCLAM) then
                                   wrap (fn (stmts, e2') => putImpureTo ctx env dest (stmts, "(" ^ e2' ^ ").payload"))
                               else if USyntax.eqVId(vid, VId_String_size) then
                                   wrap (fn (stmts, e2') => putPureTo ctx env dest (stmts, "#(" ^ e2' ^ ")"))
                               else if USyntax.eqVId(vid, VId_Lua_isNil) then
                                   wrap (fn (stmts, e2') => putPureTo ctx env dest (stmts, "(" ^ e2' ^ ") == nil"))
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
                                                                                     val zs = List.map #2 ys
                                                                                 in case dest of
                                                                                        Discard => putImpureTo ctx env dest (stmts1 @ stmts2, "(" ^ f ^ ")(" ^ String.concatWith ", " zs ^ ")")
                                                                                      | _ => putImpureTo ctx env dest (stmts1 @ stmts2, "table.pack((" ^ f ^ ")(" ^ String.concatWith ", " zs ^ "))")
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
                                                                                       val zs = List.map #2 ys
                                                                                   in case dest of
                                                                                          Discard => putImpureTo ctx env dest (stmts1 @ stmts2, "(" ^ self ^ "):" ^ method ^ "(" ^ String.concatWith ", " zs ^ ")")
                                                                                        | _ => putImpureTo ctx env dest (stmts1 @ stmts2, "table.pack((" ^ self ^ "):" ^ method ^ "(" ^ String.concatWith ", " zs ^ "))")
                                                                                   end
                                                                         )
                                                             )
                                         )
                                else
                                    NONE
                              | _ => NONE
          val isNoop = case exp1 of
                           F.VarExp(Syntax.MkQualified([], vid)) => USyntax.eqVId(vid, InitialEnv.VId_String_str)
                         | F.TyAppExp(F.VarExp(Syntax.MkQualified([], vid)), _) => USyntax.eqVId(vid, InitialEnv.VId_Lua_unsafeToValue) orelse USyntax.eqVId(vid, InitialEnv.VId_Lua_unsafeFromValue) 
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
                                                   putImpureTo ctx env dest (stmts1 @ stmts2, "(" ^ e1' ^ ")(" ^ e2' ^ ")")
                                               )
                                 )
      end
  | doExpTo ctx env (F.HandleExp { body, exnName, handler }) dest
    = let val status = genSym ctx
          val result = genSym ctx
          val stmts = [indent env ^ "local " ^ status ^ ", " ^ result ^ " = pcall(function()\n"
                      ,doExpTo ctx (nextIndentLevel env) body Return ^ indent env ^ "end)\n"
                      ,indent env ^ "if not " ^ status ^ " then\n"
                      ,indent (nextIndentLevel env) ^ "local " ^ VIdToLua exnName ^ " = " ^ result ^ "\n"
                      ,doExpTo ctx (nextIndentLevel env) handler (AssignTo result) ^ "\n" (* TODO: tail call *)
                      ,indent env ^ "end\n"
                      ]
      in putPureTo ctx env dest (stmts, result)
      end
  | doExpTo ctx env (F.RaiseExp (span as { start = { file, line, column }, ... }, exp)) dest
    = doExpCont ctx env exp
                (fn (stmts, exp') =>
                    case dest of
                        Continue cont => cont (stmts @ [indent env ^ "_raise(" ^ exp' ^ ", " ^ toLuaStringLit (OS.Path.file file) ^ ", " ^ Int.toString line ^ ", " ^ Int.toString column ^ ")\n"], "nil")
                      | _ => String.concat stmts ^ indent env ^ "_raise(" ^ exp' ^ ", " ^ toLuaStringLit (OS.Path.file file) ^ ", " ^ Int.toString line ^ ", " ^ Int.toString column ^ ")\n"
                )
  | doExpTo ctx env (F.IfThenElseExp (exp1, exp2, exp3)) dest
    = doExpCont ctx env exp1
                (fn (stmts1, exp1') =>
                    let fun doElseIf env (F.IfThenElseExp(e1, e2, e3)) dest'
                            = doExpCont ctx (nextIndentLevel env) e1
                                        (fn (s1, e1') =>
                                            String.concat
                                                (if List.null s1 then
                                                     [ indent env ^ "elseif " ^ e1' ^ " then\n"
                                                     , doExpTo ctx (nextIndentLevel env) e2 dest'
                                                     , doElseIf env e3 dest'
                                                     ]
                                                 else
                                                     [ indent env ^ "else\n" ]
                                                     @ s1
                                                     @ [ indent (nextIndentLevel env) ^ "if " ^ e1' ^ " then\n"
                                                       , doExpTo ctx (nextIndentLevel (nextIndentLevel env)) e2 dest'
                                                       , doElseIf (nextIndentLevel env) e3 dest'
                                                       , indent (nextIndentLevel env) ^ "end\n"
                                                       ]
                                                )
                                        )
                          | doElseIf env e dest' = String.concat [ indent env ^ "else\n"
                                                                 , doExpTo ctx (nextIndentLevel env) e dest'
                                                                 ]
                    in case dest of
                           Continue cont => let val result = genSym ctx
                                            in cont (stmts1
                                                     @ [ indent env ^ "local " ^ result ^ "\n"
                                                       , indent env ^ "if " ^ exp1' ^ " then\n"
                                                       , doExpTo ctx (nextIndentLevel env) exp2 (AssignTo result)
                                                       , doElseIf env exp3 (AssignTo result)
                                                       , indent env ^ "end\n"
                                                       ]
                                                    , result)
                                            end
                         | _ => String.concat stmts1
                                ^ indent env ^ "if " ^ exp1' ^ " then\n"
                                ^ doExpTo ctx (nextIndentLevel env) exp2 dest
                                ^ doElseIf env exp3 dest
                                ^ indent env ^ "end\n"
                    end
              )
  | doExpTo ctx env (F.CaseExp _) dest = raise Fail "Lua codegen: CaseExp should have been desugared earlier"
  | doExpTo ctx env (F.FnExp (vid, _, exp)) dest = putPureTo ctx env dest ([], "function(" ^ VIdToLua vid ^ ")\n" ^ doExpTo ctx (nextIndentLevel env) exp Return ^ indent env ^ "end\n") (* TODO: update environment *)
  | doExpTo ctx env (F.ProjectionExp { label, ... }) dest = putPureTo ctx env dest ([], "function(x) return x[" ^ LabelToLua label ^ "] end\n")
  | doExpTo ctx env (F.ListExp (xs, _)) dest
    = if Vector.length xs = 0 then
          putPureTo ctx env dest ([], "_nil")
      else
          mapCont (fn (e, cont) => doExpCont ctx env e cont)
                  (Vector.foldr (op ::) [] xs)
                  (fn ys => let val stmts = List.foldr (fn ((x, _), acc) => x @ acc) [] ys
                            in putPureTo ctx env dest (stmts, "_list{ n = " ^ Int.toString (Vector.length xs) ^ List.foldr (fn ((_, y), acc) => ", " ^ y ^ acc) " }" ys)
                            end
                  )
  | doExpTo ctx env (F.VectorExp (xs, _)) dest
    = mapCont (fn (e, cont) => doExpCont ctx env e cont)
              (Vector.foldr (op ::) [] xs)
              (fn ys => let val stmts = List.foldr (fn ((x, _), acc) => x @ acc) [] ys
                        in putPureTo ctx env dest (stmts, "{ n = " ^ Int.toString (Vector.length xs) ^ List.foldr (fn ((_, y), acc) => ", " ^ y ^ acc) " }" ys)
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
                                                        SOME xs => "_recordEqual({" ^ String.concatWith ", " (List.map #2 fields') ^ "})"
                                                      | NONE => "_recordEqual({" ^ String.concatWith ", " (List.map (fn (label, v) => "[" ^ LabelToLua label ^ "] = " ^ v) fields') ^ "})"
                                                  )
                        end
              )
  | doExpTo ctx env (F.DataTagExp exp) dest = doExpCont ctx env exp (fn (stmts, exp') => putPureTo ctx env dest (stmts, exp' ^ ".tag"))
  | doExpTo ctx env (F.DataPayloadExp exp) dest = doExpCont ctx env exp (fn (stmts, exp') => putPureTo ctx env dest (stmts, exp' ^ ".payload"))

(* doDec : Context -> Env -> F.Dec -> string *)
and doDec ctx env (F.ValDec (F.SimpleBind(v, _, exp)))
    = let val luavid = VIdToLua v
      in indent env ^ "local " ^ luavid ^ "\n" ^ doExpTo ctx env exp (AssignTo luavid)
      end
  | doDec ctx env (F.ValDec (F.TupleBind([], exp)))
    = doExpTo ctx env exp Discard
  | doDec ctx env (F.ValDec (F.TupleBind(vars, exp)))
    = let val vars' = List.map (fn (v,_) => VIdToLua v) vars
          val decl = indent env ^ "local " ^ String.concatWith ", " vars' ^ "\n"
      in decl ^ doExpTo ctx env exp (UnpackingAssignTo vars')
      end
  | doDec ctx env (F.RecValDec valbinds)
    = let val (decls, assignments) = ListPair.unzip (List.map (fn F.SimpleBind (v,_,exp) => let val v' = VIdToLua v
                                                                                            in (indent env ^ "local " ^ v' ^ "\n", doExpTo ctx env exp (AssignTo v'))
                                                                                            end
                                                              | F.TupleBind ([], exp) => ("", doExpTo ctx env exp Discard)
                                                              | F.TupleBind (vars, exp) => let val vars' = List.map (fn (v,_) => VIdToLua v) vars
                                                                                           in (indent env ^ "local " ^ String.concatWith ", " vars' ^ "\n", doExpTo ctx env exp (UnpackingAssignTo vars'))
                                                                                           end
                                                              ) valbinds)
      in String.concat decls ^ String.concat assignments
      end
  | doDec ctx env (F.IgnoreDec exp) = doExpTo ctx env exp Discard
  | doDec ctx env (F.DatatypeDec datbinds) = String.concat (List.map (doDatBind ctx env) datbinds)
  | doDec ctx env (F.ExceptionDec { conName as USyntax.MkVId(name, _), tagName, payloadTy })
    = let val conName' = VIdToLua conName
          val tagName' = VIdToLua tagName
      in indent env ^ "local " ^ tagName' ^ " = { " ^ toLuaStringLit name ^ " }\n"
         ^ (case payloadTy of
                NONE => indent env ^ "local " ^ conName' ^ " = { tag = " ^ tagName' ^ " }\n"
              | SOME _ => indent env ^ "local function " ^ conName' ^ "(payload)\n"
                          ^ indent (nextIndentLevel env) ^ "return { tag = " ^ tagName' ^ ", payload = payload }\n"
                          ^ indent env ^ "end\n"
           )
      end
and doDatBind ctx env (F.DatBind (tyvars, tycon, conbinds)) = String.concat (List.map (doConBind ctx env) conbinds) (* TODO: equality *)
and doConBind ctx env (F.ConBind (vid as USyntax.MkVId(name,_), NONE)) = "local " ^ VIdToLua vid ^ " = { tag = " ^ toLuaStringLit name ^ " }\n"
  | doConBind ctx env (F.ConBind (vid as USyntax.MkVId(name,_), SOME ty)) = "local function " ^ VIdToLua vid ^ "(x)\n  return { tag = " ^ toLuaStringLit name ^ ", payload = x }\nend\n"

fun doDecs ctx env decs = String.concat (List.map (doDec ctx env) decs)

end (* structure CodeGenLua *)
