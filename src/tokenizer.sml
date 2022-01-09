(*
 * Copyright (c) 2021 ARATA Mizuki
 * This file is part of LunarML.
 *)
functor LunarMLLexFun(structure Tokens: LunarML_TOKENS) = struct
        datatype TokError = TokError of SourcePos.pos * string
                          | TokWarning of SourcePos.pos * string
        structure UserDeclarations = struct
        type pos = SourcePos.pos (* line, column; both 1-based *)
        type svalue = Tokens.svalue
        type ('a,'b) token = ('a,'b) Tokens.token
        type result = (svalue,pos) token
        type arg = string * (TokError list) ref (* (filename, errorsAndWarnings) *)
        end
        datatype NumericLitType = NLTUnsigned
                                | NLTNegative
                                | NLTWord
        (* read a token *)
        fun tokenizeAllString(name,s) = let
            fun pos(l,c) = { file = name, line = l, column = c }
            val errorsAndWarnings = ref [] : (TokError list) ref
            fun emitWarning(l,c,message) = let val e = !errorsAndWarnings
                                           in errorsAndWarnings := TokWarning ({ file = name, line = l, column = c }, message) :: e
                                           end
            fun emitError(l,c,message) = let val e = !errorsAndWarnings
                                         in errorsAndWarnings := TokError ({ file = name, line = l, column = c }, message) :: e
                                         end
            fun tokenizeAll (l, c, xs) = case tokenizeOne (l, c, xs) of
                                             NONE => nil
                                           | SOME (t, l', c', rest) => t :: tokenizeAll (l', c', rest)
            and tokenizeOne (l, c, nil) = NONE (* end of input *)
              | tokenizeOne (l, c, #"(" :: #"*" :: xs) = skipComment (l, c, l, c+2, 0, xs) (* beginning of comment *)
              | tokenizeOne (l, c, #"(" :: xs) = SOME (Tokens.LPAREN (pos(l,c),pos(l,c)), l, c+1, xs)
              | tokenizeOne (l, c, #")" :: xs) = SOME (Tokens.RPAREN (pos(l,c),pos(l,c)), l, c+1, xs)
              | tokenizeOne (l, c, #"[" :: xs) = SOME (Tokens.LBRACK (pos(l,c),pos(l,c)), l, c+1, xs)
              | tokenizeOne (l, c, #"]" :: xs) = SOME (Tokens.RBRACK (pos(l,c),pos(l,c)), l, c+1, xs)
              | tokenizeOne (l, c, #"{" :: xs) = SOME (Tokens.LBRACE (pos(l,c),pos(l,c)), l, c+1, xs)
              | tokenizeOne (l, c, #"}" :: xs) = SOME (Tokens.RBRACE (pos(l,c),pos(l,c)), l, c+1, xs)
              | tokenizeOne (l, c, #"," :: xs) = SOME (Tokens.COMMA (pos(l,c),pos(l,c)), l, c+1, xs)
              | tokenizeOne (l, c, #";" :: xs) = SOME (Tokens.SEMICOLON (pos(l,c),pos(l,c)), l, c+1, xs)
              | tokenizeOne (l, c, #"." :: #"." :: #"." :: xs) = SOME (Tokens.ELLIPSIS (pos(l,c),pos(l,c+2)), l, c+3, xs)
              | tokenizeOne (l, c, #"." :: xs) = SOME (Tokens.DOT (pos(l,c),pos(l,c)), l, c+1, xs)
              | tokenizeOne (l, c, #"#" :: #"\"" :: xs) = let val (l', c', str, rest) = readStringLit (l, c, l, c+2, nil, xs)
                                                          in SOME (Tokens.CharacterConst (implode str,pos(l,c),pos(l',c'-1)), l', c', rest)
                                                          end
              | tokenizeOne (l, c, #"#" :: #"[" :: xs) = SOME (Tokens.HASHLBRACK (pos(l,c),pos(l,c+1)), l, c+2, xs)
              | tokenizeOne (l, c, #"\"" :: xs) = let val (l', c', str, rest) = readStringLit (l, c, l, c+1, nil, xs)
                                                  in SOME (Tokens.StringConst (implode str,pos(l,c),pos(l',c'-1)), l', c', rest)
                                                  end
              | tokenizeOne (l, c, #"~" :: #"0" :: (zr as #"x" :: x :: xs)) = if Char.isHexDigit x then
                                                                                  readHexadecimalConstant (l, c, c+4, NLTNegative, hexDigitToLargeInt x, xs)
                                                                              else
                                                                                  ( emitWarning (l, c+2, "there should be a space between a numeric literal and an identifier")
                                                                                  ; SOME (Tokens.ZNIntConst (0,pos(l,c),pos(l,c+1)), l, c+2, zr)
                                                                                  )
              | tokenizeOne (l, c, #"~" :: (rest0 as x :: xs)) = if Char.isDigit x then
                                                                     readDecimalConstant (l, c, c+2, NLTNegative, digitToLargeInt x, xs)
                                                                 else
                                                                     readSymbolicIdentifier (l, c, c, [], [#"~"], rest0)
              | tokenizeOne (l, c, #"0" :: (rest0 as #"w" :: #"x" :: x :: xs)) = if Char.isHexDigit x then
                                                                                     readHexadecimalConstant (l, c, c+3, NLTWord, hexDigitToLargeInt x, xs)
                                                                                 else
                                                                                     ( emitWarning (l, c+1, "there should be a space between a numeric literal and an identifier")
                                                                                     ; SOME (Tokens.ZNIntConst (0,pos(l,c),pos(l,c)), l, c+1, rest0)
                                                                                     )
              | tokenizeOne (l, c, #"0" :: (rest0 as #"w" :: x :: xs)) = if Char.isDigit x then
                                                                             readDecimalConstant (l, c, c+3, NLTWord, digitToLargeInt x, xs)
                                                                         else
                                                                             ( emitWarning (l, c+1, "there should be a space between a numeric literal and an identifier")
                                                                             ; SOME (Tokens.ZNIntConst (0,pos(l,c),pos(l,c)), l, c+1, rest0)
                                                                             )
              | tokenizeOne (l, c, #"0" :: (rest0 as #"x" :: x :: xs)) = if Char.isHexDigit x then
                                                                             readHexadecimalConstant (l, c, c+3, NLTUnsigned, hexDigitToLargeInt x, xs)
                                                                         else
                                                                             ( emitWarning (l, c+1, "there should be a space between a numeric literal and an identifier ")
                                                                             ; SOME (Tokens.ZNIntConst (0,pos(l,c),pos(l,c)), l, c+1, rest0)
                                                                             )
              (* TODO: binary constant *)
              | tokenizeOne (l, c, #"\n" :: xs) = tokenizeOne (l+1, 1, xs)
              | tokenizeOne (l, c, x :: xs) = if Char.isAlpha x orelse x = #"_" orelse x = #"'" then
                                                  readIdentifierOrKeyword (l, c, c, [], [x], xs)
                                              else if Char.isDigit x then
                                                  (* integer in decimal notation, or real constant *)
                                                  readDecimalConstant (l, c, c+1, NLTUnsigned, digitToLargeInt x, xs)
                                              else if isSymbolChar x then
                                                  readSymbolicIdentifier (l, c, c, [], [x], xs)
                                              else if Char.isSpace x then
                                                  tokenizeOne (l, c+1, xs)
                                              else
                                                  ( emitError (l, c, "invalid character '" ^ Char.toString x ^ "'")
                                                  ; tokenizeOne (l, c+1, xs) (* continue *)
                                                  )
            (* isAscii, isAlpha, isAlphaNum, isDigit, isSpace *)
            and skipComment (l0, c0, l, c, n, #"*" :: #")" :: xs) = if n = 0 then
                                                                        tokenizeOne (l, c+2, xs)
                                                                    else
                                                                        skipComment (l0, c0, l, c+2, n - 1, xs)
              | skipComment (l0, c0, l, c, n, #"(" :: #"*" :: xs) = skipComment (l0, c0, l, c+2, n + 1, xs)
              | skipComment (l0, c0, l, c, n, #"\n" :: xs) = skipComment (l0, c0, l+1, 1, n, xs)
              | skipComment (l0, c0, l, c, n, _ :: xs) = skipComment (l0, c0, l, c+1, n, xs)
              | skipComment (l0, c0, _, _, _, nil) = ( emitError (l0, c0, "unterminated comment")
                                                     ; NONE
                                                     )
            and readIdentifierOrKeyword (l, c0, c1, rstrids, accum, nil) = let val (tok, ident) = recognizeKeyword (l, c1, String.implode (rev accum))
                                                                           in if List.null rstrids then
                                                                                  SOME (tok, l, c1 + length accum, nil)
                                                                              else
                                                                                  case ident of
                                                                                      SOME (name, p2) => SOME (Tokens.QualifiedAlnumIdent ((List.rev rstrids, name), pos (l, c0), p2), l, c1 + length accum, nil)
                                                                                    | NONE => ( emitError (l, c1, "invalid qualified name")
                                                                                              ; SOME (tok, l, c1 + length accum, nil)
                                                                                              )
                                                                           end
              | readIdentifierOrKeyword (l, c0, c1, rstrids, accum, input as x :: xs) = if Char.isAlphaNum x orelse x = #"_" orelse x = #"'" then
                                                                                            readIdentifierOrKeyword (l, c0, c1, rstrids, x :: accum, xs)
                                                                                        else
                                                                                            let val (tok, ident) = recognizeKeyword (l, c1, String.implode (rev accum))
                                                                                            in if List.null rstrids then
                                                                                                   case ident of
                                                                                                       SOME (name, p2) =>
                                                                                                       if x = #"." then
                                                                                                           case xs of
                                                                                                               x' :: xs' => if Char.isAlpha x' then
                                                                                                                                readIdentifierOrKeyword (l, c0, c1 + 2, name :: rstrids, [x'], xs')
                                                                                                                            else if isSymbolChar x' then
                                                                                                                                readSymbolicIdentifier (l, c0, c1 + 2, name :: rstrids, [x'], xs')
                                                                                                                            else
                                                                                                                                SOME (tok, l, c1 + length accum, input)
                                                                                                             | [] => SOME (tok, l, c1 + length accum, input)
                                                                                                       else
                                                                                                           SOME (tok, l, c1 + length accum, input)
                                                                                                     | NONE => SOME (tok, l, c1 + length accum, input)
                                                                                               else
                                                                                                   case ident of
                                                                                                       SOME (name, p2) =>
                                                                                                       if x = #"." then
                                                                                                           case xs of
                                                                                                               x' :: xs' => if Char.isAlpha x' then
                                                                                                                                readIdentifierOrKeyword (l, c0, c1 + 2, name :: rstrids, [x'], xs')
                                                                                                                            else if isSymbolChar x' then
                                                                                                                                readSymbolicIdentifier (l, c0, c1 + 2, name :: rstrids, [x'], xs')
                                                                                                                            else
                                                                                                                                SOME (tok, l, c1 + length accum, input)
                                                                                                             | [] => SOME (tok, l, c1 + length accum, input)
                                                                                                       else
                                                                                                           SOME (Tokens.QualifiedAlnumIdent ((List.rev rstrids, name), pos (l, c0), p2), l, c1 + length accum, input)
                                                                                                     | NONE => ( emitError (l, c1, "invalid qualified name")
                                                                                                               ; SOME (tok, l, c1 + length accum, input)
                                                                                                               )
                                                                                            end
            and recognizeKeyword (l, c, name) = let val (tok, ident) = case name of
                                                                           "_" => (Tokens.UNDERSCORE, NONE)
                                                                         | "_primType" => (Tokens.PRIMTYPE, NONE) (* extension *)
                                                                         | "_primVal" => (Tokens.PRIMVAL, NONE) (* extension *)
                                                                         | "_primCall" => (Tokens.PRIMCALL, NONE) (* extension *)
                                                                         | "_overload" => (Tokens.OVERLOAD, NONE) (* extension *)
                                                                         | "abstype" => (Tokens.ABSTYPE, NONE)
                                                                         | "and" => (Tokens.AND, NONE)
                                                                         | "andalso" => (Tokens.ANDALSO, NONE)
                                                                         | "as" => (Tokens.AS, NONE)
                                                                         | "case" => (Tokens.CASE, NONE)
                                                                         | "datatype" => (Tokens.DATATYPE, NONE)
                                                                         | "do" => (Tokens.DO, NONE)
                                                                         | "else" => (Tokens.ELSE, NONE)
                                                                         | "end" => (Tokens.END, NONE)
                                                                         | "eqtype" => (Tokens.EQTYPE, NONE)
                                                                         | "exception" => (Tokens.EXCEPTION, NONE)
                                                                         | "fn" => (Tokens.FN, NONE)
                                                                         | "fun" => (Tokens.FUN, NONE)
                                                                         | "functor" => (Tokens.FUNCTOR, NONE)
                                                                         | "handle" => (Tokens.HANDLE, NONE)
                                                                         | "if" => (Tokens.IF, NONE)
                                                                         | "in" => (Tokens.IN, NONE)
                                                                         | "include" => (Tokens.INCLUDE, NONE)
                                                                         | "infix" => (Tokens.INFIX, NONE)
                                                                         | "infixr" => (Tokens.INFIXR, NONE)
                                                                         | "let" => (Tokens.LET, NONE)
                                                                         | "local" => (Tokens.LOCAL, NONE)
                                                                         | "nonfix" => (Tokens.NONFIX, NONE)
                                                                         | "of" => (Tokens.OF, NONE)
                                                                         | "op" => (Tokens.OP, NONE)
                                                                         | "open" => (Tokens.OPEN, NONE)
                                                                         | "orelse" => (Tokens.ORELSE, NONE)
                                                                         | "raise" => (Tokens.RAISE, NONE)
                                                                         | "rec" => (Tokens.REC, NONE)
                                                                         | "sharing" => (Tokens.SHARING, NONE)
                                                                         | "sig" => (Tokens.SIG, NONE)
                                                                         | "signature" => (Tokens.SIGNATURE, NONE)
                                                                         | "struct" => (Tokens.STRUCT, NONE)
                                                                         | "structure" => (Tokens.STRUCTURE, NONE)
                                                                         | "then" => (Tokens.THEN, NONE)
                                                                         | "type" => (Tokens.TYPE, NONE)
                                                                         | "val" => (Tokens.VAL, NONE)
                                                                         | "with" => (Tokens.WITH, NONE)
                                                                         | "withtype" => (Tokens.WITHTYPE, NONE)
                                                                         | "where" => (Tokens.WHERE, NONE)
                                                                         | "while" => (Tokens.WHILE, NONE)
                                                                         | _ => case String.sub(name,0) of
                                                                                    #"'" => (fn (p1,p2) => Tokens.PrimeIdent (name, p1, p2), NONE)
                                                                                  | #"_" => ( emitError (l, c, "an identifier cannot begin with an underscore")
                                                                                            ; (Tokens.UNDERSCORE, NONE)
                                                                                            )
                                                                                  | _ => (fn (p1,p2) => Tokens.AlnumIdent (name, p1, p2), SOME name)
                                                    val p2 = pos(l,c + String.size name - 1)
                                                in (tok (pos(l, c), p2), Option.map (fn name => (name, p2)) ident)
                                                end
            and readSymbolicIdentifier (l, c0, c1, rstrids, accum, nil) = SOME (#1 (recognizeSymbolic (l, c1, String.implode (rev accum))), l, c1 + length accum, nil)
              | readSymbolicIdentifier (l, c0, c1, rstrids, accum, input as x :: xs) = if isSymbolChar x then
                                                                                           readSymbolicIdentifier (l, c0, c1, rstrids, x :: accum, xs)
                                                                                       else
                                                                                           let val (tok, ident) = recognizeSymbolic (l, c1, String.implode (rev accum))
                                                                                           in if List.null rstrids then
                                                                                                  SOME (tok, l, c1 + length accum, input)
                                                                                              else
                                                                                                  case ident of
                                                                                                      SOME (name, p2) => SOME (Tokens.QualifiedSymbolicIdent ((List.rev rstrids, name), pos (l, c0), p2), l, c1 + length accum, input)
                                                                                                    | NONE => ( emitError (l, c1, "invalid qualified name")
                                                                                                              ; SOME (tok, l, c1 + length accum, input)
                                                                                                              )
                                                                                           end
            and recognizeSymbolic (l, c, name) = let val (tok, ident) = case name of
                                                                            ":" => (Tokens.COLON, NONE)
                                                                          | "|" => (Tokens.BAR, NONE)
                                                                          | "=" => (Tokens.EQUALS, NONE)
                                                                          | "=>" => (Tokens.DARROW, NONE)
                                                                          | "->" => (Tokens.ARROW, NONE)
                                                                          | "#" => (Tokens.HASH, NONE)
                                                                          | ":>" => (Tokens.COLONGT, NONE)
                                                                          | "*" => (Tokens.ASTERISK, SOME "*")
                                                                          | _ => (fn (p1,p2) => Tokens.SymbolicIdent (name,p1,p2), SOME name)
                                                     val p2 = pos(l, c + String.size name - 1)
                                                 in (tok (pos(l, c), p2), Option.map (fn name => (name, p2)) ident)
                                                 end
            and isSymbolChar #"!" = true
              | isSymbolChar #"%" = true
              | isSymbolChar #"&" = true
              | isSymbolChar #"$" = true
              | isSymbolChar #"#" = true
              | isSymbolChar #"+" = true
              | isSymbolChar #"-" = true
              | isSymbolChar #"/" = true
              | isSymbolChar #":" = true
              | isSymbolChar #"<" = true
              | isSymbolChar #"=" = true
              | isSymbolChar #">" = true
              | isSymbolChar #"?" = true
              | isSymbolChar #"@" = true
              | isSymbolChar #"\\" = true
              | isSymbolChar #"~" = true
              | isSymbolChar #"`" = true
              | isSymbolChar #"^" = true
              | isSymbolChar #"|" = true
              | isSymbolChar #"*" = true
              | isSymbolChar _ = false
            and readDecimalConstant (l1, c1, c', numericLitType : NumericLitType, x0 : IntInf.int, xs : char list)
                (* x0 is a decimal digit *)
                = let fun mkIntConst (p2, a) = if numericLitType = NLTWord then
                                                   Tokens.WordConst (a,pos(l1,c1),p2)
                                               else if numericLitType = NLTNegative then
                                                   Tokens.ZNIntConst (~a,pos(l1,c1),p2)
                                               else if x0 <> 0 andalso numericLitType = NLTUnsigned then
                                                   Tokens.PosInt (a,pos(l1,c1),p2)
                                               else
                                                   Tokens.ZNIntConst (a,pos(l1,c1),p2)
                      fun mkRealConst (p2, intPart : IntInf.int, fracPart, expPart)
                          = let val s = if fracPart = "" then
                                            IntInf.toString intPart ^ "e" ^ Int.toString expPart
                                        else
                                            IntInf.toString intPart ^ "." ^ fracPart ^ "e" ^ Int.toString expPart
                            in Tokens.RealConst (s,pos(l1,c1),p2)
                            end
                      fun parseIntPart (l, c, a : IntInf.int, rest0 as #"." :: x2 :: rest1)
                          = if numericLitType <> NLTWord andalso Char.isDigit x2 then
                                parseFracPart (l, c+2, a, String.str x2, rest1)
                            else
                                ( emitWarning (l, c, "there should be a space between a numeric literal and a dot")
                                ; SOME (mkIntConst (pos(l,c-1), a), l, c, rest0)
                                )
                        | parseIntPart (l, c, a, rest0 as x1 :: #"~" :: x2 :: rest1)
                          = if numericLitType <> NLTWord andalso (x1 = #"e" orelse x1 = #"E") andalso Char.isDigit x2 then
                                parseExpPart (l, c+3, a, "", ~1, digitToInt x2, rest1)
                            else
                                parseMoreIntPart (l, c, a, rest0)
                        | parseIntPart (l, c, a, rest0 as x1 :: x2 :: rest1)
                          = if numericLitType <> NLTWord andalso (x1 = #"e" orelse x1 = #"E") andalso Char.isDigit x2 then
                                parseExpPart (l, c+2, a, "", 1, digitToInt x2, rest1)
                            else
                                parseMoreIntPart (l, c, a, rest0)
                        | parseIntPart (l, c, a, rest) = parseMoreIntPart (l, c, a, rest)
                      and parseMoreIntPart (l, c, a, rest0 as x1 :: rest1) = if Char.isDigit x1 then
                                                                                 parseIntPart (l, c+1, a * 10 + digitToLargeInt x1, rest1)
                                                                             else
                                                                                 ( if Char.isAlpha x1 then
                                                                                       emitWarning (l, c, "there should be a space between a numeric literal and an identifier")
                                                                                   else
                                                                                       ()
                                                                                 ; SOME (mkIntConst(pos(l,c-1),a), l, c, rest0)
                                                                                 )
                        | parseMoreIntPart (l, c, a, rest0) = SOME (mkIntConst(pos(l,c-1),a), l, c, rest0)
                      and parseFracPart (l, c, intPart : IntInf.int, fracPart : string, rest0 as x :: rest1)
                          = if Char.isDigit x then
                                parseFracPart (l, c+1, intPart, fracPart ^ String.str x, rest1) (* TODO: a better impl? *)
                            else if x = #"e" orelse x = #"E" then
                                case rest1 of
                                    #"~" :: y :: ys => if Char.isDigit y then
                                                           parseExpPart (l, c+3, intPart, fracPart, ~1, digitToInt y, ys)
                                                       else
                                                           SOME (mkRealConst (pos(l,c-1), intPart, fracPart, 0), l, c, rest0)
                                  | y :: ys => if Char.isDigit y then
                                                   parseExpPart (l, c+2, intPart, fracPart, 1, digitToInt y, ys)
                                               else
                                                   SOME (mkRealConst (pos(l,c-1), intPart, fracPart, 0), l, c, rest0)
                                  | _ => SOME (mkRealConst (pos(l,c-1), intPart, fracPart, 0), l, c, rest0)
                            else
                                SOME (mkRealConst (pos(l,c-1), intPart, fracPart, 0), l, c, rest0)
                        | parseFracPart (l, c, intPart, fracPart, rest0 as nil) = SOME (mkRealConst (pos(l,c-1), intPart, fracPart, 0), l, c, rest0)
                      and parseExpPart (l, c, intPart : IntInf.int, fracPart : string, expSign : int, expPart : int, rest0 as x :: rest1)
                          = if Char.isDigit x then
                                parseExpPart (l, c+1, intPart, fracPart, expSign, expPart * 10 + digitToInt x, rest1)
                            else
                                SOME (mkRealConst (pos(l,c-1), intPart, fracPart, expSign * expPart), l, c, rest0)
                        | parseExpPart (l, c, intPart, fracPart, expSign, expPart, rest0)
                          = SOME (mkRealConst (pos(l,c-1), intPart, fracPart, expSign * expPart), l, c, rest0)
                  in parseIntPart (l1, c', x0, xs)
                  end
            and readHexadecimalConstant (l1, c1, c', numericLitType : NumericLitType, x : IntInf.int, xs : char list)
                (* x is a hexadecimal digit *)
                = let fun mkIntConst (p2, a) = if numericLitType = NLTWord then
                                                   Tokens.WordConst (a,pos(l1,c1),p2)
                                               else if numericLitType = NLTNegative then
                                                   Tokens.ZNIntConst (~a,pos(l1,c1),p2)
                                               else
                                                   Tokens.ZNIntConst (a,pos(l1,c1),p2)
                      fun parseIntPart (l, c, a, rest0 as x1 :: rest1) = if Char.isHexDigit x1 then
                                                                             parseIntPart (l, c+1, a * 16 + hexDigitToLargeInt x1, rest1)
                                                                         else
                                                                             SOME (mkIntConst(pos(l,c-1), a), l, c, rest0)
                        | parseIntPart (l, c, a, rest0) = SOME (mkIntConst(pos(l,c-1),a), l, c, rest0)
                  in parseIntPart (l1, c', x, xs)
                  end
            and readStringLit (l0, c0, l, c, accum, nil) = ( emitError (l0, c0, "unterminated string literal")
                                                           ; (l, c, rev accum, nil)
                                                           )
              | readStringLit (l0, c0, l, c, accum, #"\"" :: xs) = (l, c+1, rev accum, xs)
              | readStringLit (l0, c0, l, c, accum, #"\\" :: #"a" :: xs) = readStringLit (l0, c0, l, c+2, #"\a" :: accum, xs) (* bell *)
              | readStringLit (l0, c0, l, c, accum, #"\\" :: #"b" :: xs) = readStringLit (l0, c0, l, c+2, #"\b" :: accum, xs) (* backspace *)
              | readStringLit (l0, c0, l, c, accum, #"\\" :: #"t" :: xs) = readStringLit (l0, c0, l, c+2, #"\t" :: accum, xs) (* horizontal tab *)
              | readStringLit (l0, c0, l, c, accum, #"\\" :: #"n" :: xs) = readStringLit (l0, c0, l, c+2, #"\n" :: accum, xs) (* line feed *)
              | readStringLit (l0, c0, l, c, accum, #"\\" :: #"v" :: xs) = readStringLit (l0, c0, l, c+2, #"\v" :: accum, xs) (* vertical tab *)
              | readStringLit (l0, c0, l, c, accum, #"\\" :: #"f" :: xs) = readStringLit (l0, c0, l, c+2, #"\f" :: accum, xs) (* form feed *)
              | readStringLit (l0, c0, l, c, accum, #"\\" :: #"r" :: xs) = readStringLit (l0, c0, l, c+2, #"\r" :: accum, xs) (* carriage return *)
              | readStringLit (l0, c0, l, c, accum, #"\\" :: #"^" :: x :: xs) (* control character *)
                = if 64 <= ord x andalso ord x <= 95 then
                      readStringLit (l0, c0, l, c+3, chr (ord x - 64) :: accum, xs)
                  else
                      ( emitError (l, c, "invalid control character")
                      ; readStringLit (l0, c0, l, c+3, accum, xs)
                      )
              | readStringLit (l0, c0, l, c, accum, #"\\" :: #"u" :: (xs' as (x0 :: x1 :: x2 :: x3 :: xs)))
                = if Char.isHexDigit x0 andalso Char.isHexDigit x1 andalso Char.isHexDigit x2 andalso Char.isHexDigit x3 then
                      let val charOrd = ((hexDigitToInt x0 * 16 + hexDigitToInt x1) * 16 + hexDigitToInt x3) * 16 + hexDigitToInt x3;
                      in if charOrd > Char.maxOrd then
                             ( emitError (l, c, "char ordinal too large")
                             ; readStringLit (l0, c0, l, c+6, accum, xs)
                             )
                         else
                             readStringLit (l0, c0, l, c+6, chr charOrd :: accum, xs)
                      end
                  else
                      ( emitError (l, c, "invalid \\u escape sequence")
                      ; readStringLit (l0, c0, l, c+2, accum, xs')
                      )
              | readStringLit (l0, c0, l, c, accum, #"\\" :: #"\"" :: xs) = readStringLit (l0, c0, l, c+2, #"\"" :: accum, xs)
              | readStringLit (l0, c0, l, c, accum, #"\\" :: #"\\" :: xs) = readStringLit (l0, c0, l, c+2, #"\\" :: accum, xs)
              | readStringLit (l0, c0, l, c, accum, #"\\" :: x :: xs)
                = if Char.isDigit x then
                      case xs of
                          d1 :: d2 :: xs' => if Char.isDigit d1 andalso Char.isDigit d2 then
                                                 let val charOrd = digitToInt x * 100 + digitToInt d1 * 10 + digitToInt d2;
                                                 in if charOrd > Char.maxOrd then
                                                        ( emitError (l, c, "char ordinal too large")
                                                        ; readStringLit (l0, c0, l, c+4, accum, xs')
                                                        )
                                                    else
                                                        readStringLit (l0, c0, l, c+4, chr charOrd :: accum, xs')
                                                 end
                                             else
                                                 ( emitError (l, c, "invalid \\<digits> escape sequence")
                                                 ; readStringLit (l0, c0, l, c+2, accum, xs)
                                                 )
                        | _ => ( emitError (l, c, "invalid \\<digits> escape sequence")
                               ; readStringLit (l0, c0, l, c+2, accum, xs)
                               )
                  else if x = #"\n" then
                      skipFormattingCharacters (l0, c0, l+1, 1, accum, xs)
                  else if Char.isSpace x then
                      skipFormattingCharacters (l0, c0, l, c+1, accum, xs)
                  else
                      ( emitError (l, c, "unknown escape sequence")
                      ; readStringLit (l0, c0, l, c+2, accum, xs)
                      )
              | readStringLit (l0, c0, l, c, accum, x :: xs) = readStringLit (l0, c0, l, c+1, x :: accum, xs)
            and digitToInt x = ord x - ord #"0"
            and digitToLargeInt x = Int.toLarge (digitToInt x)
            and hexDigitToInt x = if ord #"A" <= ord x andalso ord x <= ord #"F" then
                                      ord x - ord #"A" + 10
                                  else if ord #"a" <= ord x andalso ord x <= ord #"f" then
                                      ord x - ord #"a" + 10
                                  else
                                      ord x - ord #"0"
            and hexDigitToLargeInt x = Int.toLarge (hexDigitToInt x)
            and skipFormattingCharacters (l0, c0, l, c, accum, nil) = ( emitError (l0, c0, "unterminated string literal")
                                                                      ; (l, c, rev accum, nil)
                                                                      )
              | skipFormattingCharacters (l0, c0, l, c, accum, #"\\" :: xs) = readStringLit (l0, c0, l, c+1, accum, xs)
              | skipFormattingCharacters (l0, c0, l, c, accum, #"\n" :: xs) = skipFormattingCharacters(l0, c0, l+1, 1, accum, xs)
              | skipFormattingCharacters (l0, c0, l, c, accum, x :: xs) = if Char.isSpace x then
                                                                              skipFormattingCharacters (l0, c0, l, c+1, accum, xs)
                                                                          else
                                                                              ( emitError (l, c, "invalid formatting character in string literal")
                                                                              ; skipFormattingCharacters (l0, c0, l, c+1, accum, xs)
                                                                              )
            val result = tokenizeAll (1, 1, String.explode s)
        in (result, rev (!errorsAndWarnings))
        end
        fun readAll input = let val x = input 1024
                            in if x = "" then
                                   ""
                               else
                                   x ^ readAll input
                            end
        fun makeLexer input (name, ewRef) = let val (tokens, ew) = tokenizeAllString (name, readAll input)
                                                val tokensRef = ref tokens
                                            in ewRef := ew
                                              ; fn _ => case !tokensRef of
                                                            nil => Tokens.EOF ({file=name,line=0,column=0},{file=name,line=0,column=0})
                                                          | x :: xs => (tokensRef := xs; x)
                                            end
        fun makeInputFromString str = let val i = ref false
                                      in fn _ => if !i then
                                                     ""
                                                 else
                                                     (i := true; str)
                                      end
        end
