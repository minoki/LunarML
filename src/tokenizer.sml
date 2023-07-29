(*
 * Copyright (c) 2021 ARATA Mizuki
 * This file is part of LunarML.
 *)
functor LunarMLLexFun (structure Tokens: LunarML_TOKENS) = struct
        structure UserDeclarations = struct
        type pos = SourcePos.pos (* line, column; both 1-based *)
        type svalue = Tokens.svalue
        type ('a,'b) token = ('a,'b) Tokens.token
        type result = (svalue,pos) token
        type arg = (* filename *) string * LanguageOptions.options * Message.handler
        end
        datatype NumericLitType = NLTUnsigned
                                | NLTNegative
                                | NLTWord
        datatype state = NORMAL | SPECIAL_COMMENT
        (* read a token *)
        fun tokenizeAllString (messageHandler : Message.handler, opts : LanguageOptions.options, name, s) = let
            fun pos (l, c) = { file = name, line = l, column = c }
            fun emitWarning (l, c, message) = let val pos = { file = name, line = l, column = c }
                                                  val span = { start = pos, end_ = pos }
                                              in Message.warning (messageHandler, [span], "lexical", message)
                                              end
            fun emitError (l, c, message) = let val pos = { file = name, line = l, column = c }
                                                val span = { start = pos, end_ = pos }
                                            in Message.error (messageHandler, [span], "lexical", message)
                                            end
            fun isBinDigit c = c = #"0" orelse c = #"1"
            fun tokenizeOne (state, l, c, input)
                = (case input of
                      nil => NONE (* end of input *)
                    | #"(" :: #"*" :: xs => if state = NORMAL andalso #valDescInComments opts <> LanguageOptions.IGNORE then
                                                case xs of
                                                    #"!" :: xss => SOME (Tokens.START_VAL_DESC_COMMENT (pos (l, c), pos (l, c + 2)), SPECIAL_COMMENT, l, c + 3, xss)
                                                  | _ => skipComment (state, l, c, l, c + 2, 0, xs) (* beginning of comment *)
                                            else
                                                skipComment (state, l, c, l, c + 2, 0, xs) (* beginning of comment *)
                    | #"(" :: xs => SOME (Tokens.LPAREN (pos (l, c), pos (l, c)), state, l, c + 1, xs)
                    | #")" :: xs => SOME (Tokens.RPAREN (pos (l, c), pos (l, c)), state, l, c + 1, xs)
                    | #"[" :: xs => SOME (Tokens.LBRACK (pos (l, c), pos (l, c)), state, l, c + 1, xs)
                    | #"]" :: xs => SOME (Tokens.RBRACK (pos (l, c), pos (l, c)), state, l, c + 1, xs)
                    | #"{" :: xs => SOME (Tokens.LBRACE (pos (l, c), pos (l, c)), state, l, c + 1, xs)
                    | #"}" :: xs => SOME (Tokens.RBRACE (pos (l, c), pos (l, c)), state, l, c + 1, xs)
                    | #"," :: xs => SOME (Tokens.COMMA (pos (l, c), pos (l, c)), state, l, c + 1, xs)
                    | #";" :: xs => SOME (Tokens.SEMICOLON (pos (l, c), pos (l, c)), state, l, c + 1, xs)
                    | #"." :: #"." :: #"." :: xs => SOME (Tokens.ELLIPSIS (pos (l, c), pos (l, c + 2)), state, l, c + 3, xs)
                    | #"." :: x :: xs => if Char.isAlpha x then
                                             readIdentifierOrKeyword (state, l, c, c + 1, true, [], [x], xs)
                                         else if isSymbolChar x then
                                             readSymbolicIdentifier (state, l, c, c + 1, true, [], [x], xs)
                                         else
                                             SOME (Tokens.DOT (pos (l, c), pos (l, c)), state, l, c + 1, xs)
                    | #"." :: (xs as nil) => SOME (Tokens.DOT (pos (l, c), pos (l, c)), state, l, c + 1, xs)
                    | #"#" :: #"\"" :: xs => let val (l', c', str, rest) = readStringLit (l, c, l, c + 2, nil, xs)
                                                 val value = case str of
                                                                 [value] => value
                                                               | _ => ( emitError (l, c, "invalid character constant")
                                                                      ; StringElement.CODEUNIT 0
                                                                      )
                                             in SOME (Tokens.CharacterConst (value, pos (l, c), pos (l', c' - 1)), state, l', c', rest)
                                             end
                    | #"#" :: #"[" :: xs => SOME (Tokens.HASHLBRACK (pos (l, c), pos (l, c + 1)), state, l, c + 2, xs)
                    | #"\"" :: xs => let val (l', c', str, rest) = readStringLit (l, c, l, c + 1, nil, xs)
                                         val str = vector str
                                     in case StringElement.toAsciiString str of
                                            NONE => SOME (Tokens.StringConst (str, pos (l, c), pos (l', c' - 1)), state, l', c', rest)
                                          | SOME ascii => SOME (Tokens.AsciiStringConst ((ascii, str), pos (l, c), pos (l', c' - 1)), state, l', c', rest)
                                     end
                    | #"~" :: #"0" :: (zr as #"x" :: x :: xs) => if Char.isHexDigit x then
                                                                     readHexadecimalConstant (state, l, c, c + 4, NLTNegative, hexDigitToLargeInt x, xs)
                                                                 else
                                                                     ( emitWarning (l, c + 2, "there should be a space between a numeric literal and an identifier")
                                                                     ; SOME (Tokens.ZNIntConst (0, pos (l, c), pos (l, c + 1)), state, l, c + 2, zr)
                                                                     )
                    | #"~" :: #"0" :: (zr as #"b" :: x :: xs) => if isBinDigit x then
                                                                     readBinaryConstant (state, l, c, c + 4, NLTNegative, digitToLargeInt x, xs) (* [Successor ML] extended literal syntax (binary constant) *)
                                                                 else
                                                                     ( emitWarning (l, c + 2, "there should be a space between a numeric literal and an identifier")
                                                                     ; SOME (Tokens.ZNIntConst (0, pos (l, c), pos (l, c + 1)), state, l, c + 2, zr)
                                                                     )
                    | #"~" :: (rest0 as x :: xs) => if Char.isDigit x then
                                                        readDecimalConstant (state, l, c, c + 2, NLTNegative, digitToLargeInt x, xs)
                                                    else
                                                        readSymbolicIdentifier (state, l, c, c, false, [], [#"~"], rest0)
                    | #"0" :: (rest0 as #"w" :: #"x" :: x :: xs) => if Char.isHexDigit x then
                                                                        readHexadecimalConstant (state, l, c, c + 3, NLTWord, hexDigitToLargeInt x, xs)
                                                                    else
                                                                        ( emitWarning (l, c + 1, "there should be a space between a numeric literal and an identifier")
                                                                        ; SOME (Tokens.ZNIntConst (0, pos (l, c), pos (l, c)), state, l, c + 1, rest0)
                                                                        )
                    | #"0" :: (rest0 as #"w" :: #"b" :: x :: xs) => if isBinDigit x then
                                                                        readBinaryConstant (state, l, c, c + 3, NLTWord, digitToLargeInt x, xs) (* [Successor ML] extended literal syntax (binary constant) *)
                                                                    else
                                                                        ( emitWarning (l, c + 1, "there should be a space between a numeric literal and an identifier")
                                                                        ; SOME (Tokens.ZNIntConst (0, pos (l, c), pos (l, c)), state, l, c + 1, rest0)
                                                                        )
                    | #"0" :: (rest0 as #"w" :: x :: xs) => if Char.isDigit x then
                                                                readDecimalConstant (state, l, c, c + 3, NLTWord, digitToLargeInt x, xs)
                                                            else
                                                                ( emitWarning (l, c + 1, "there should be a space between a numeric literal and an identifier")
                                                                ; SOME (Tokens.ZNIntConst (0, pos (l, c), pos (l, c)), state, l, c + 1, rest0)
                                                                )
                    | #"0" :: (rest0 as #"x" :: x :: xs) => if Char.isHexDigit x then
                                                                readHexadecimalConstant (state, l, c, c + 3, NLTUnsigned, hexDigitToLargeInt x, xs)
                                                            else
                                                                ( emitWarning (l, c + 1, "there should be a space between a numeric literal and an identifier ")
                                                                ; SOME (Tokens.ZNIntConst (0, pos (l, c), pos (l, c)), state, l, c + 1, rest0)
                                                                )
                    | #"0" :: (rest0 as #"b" :: x :: xs) => if isBinDigit x then
                                                                readBinaryConstant (state, l, c, c + 3, NLTUnsigned, digitToLargeInt x, xs) (* [Successor ML] extended literal syntax (binary constant) *)
                                                            else
                                                                ( emitWarning (l, c + 1, "there should be a space between a numeric literal and an identifier ")
                                                                ; SOME (Tokens.ZNIntConst (0, pos (l, c), pos (l, c)), state, l, c + 1, rest0)
                                                                )
                    | #"\n" :: xs => tokenizeOne (state, l + 1, 1, xs)
                    | x :: xs => if Char.isAlpha x orelse x = #"_" orelse x = #"'" then
                                     readIdentifierOrKeyword (state, l, c, c, false, [], [x], xs)
                                 else if Char.isDigit x then
                                     (* integer in decimal notation, or real constant *)
                                     readDecimalConstant (state, l, c, c + 1, NLTUnsigned, digitToLargeInt x, xs)
                                 else if isSymbolChar x then
                                     if state = SPECIAL_COMMENT andalso x = #"*" then
                                         case xs of
                                             #")" :: xss => SOME (Tokens.END_SPECIAL_COMMENT (pos (l, c), pos (l, c + 1)), NORMAL, l, c + 2, xss)
                                           | _ => readSymbolicIdentifier (state, l, c, c, false, [], [x], xs)
                                     else
                                         readSymbolicIdentifier (state, l, c, c, false, [], [x], xs)
                                 else if Char.isSpace x then
                                     tokenizeOne (state, l, c + 1, xs)
                                 else
                                     ( emitError (l, c, "invalid character '" ^ Char.toString x ^ "'")
                                     ; tokenizeOne (state, l, c + 1, xs) (* continue *)
                                     )
                  )
            and skipComment (state, l0, c0, l, c, n, #"*" :: #")" :: xs) = if n = 0 then
                                                                               tokenizeOne (state, l, c + 2, xs)
                                                                           else
                                                                               skipComment (state, l0, c0, l, c + 2, n - 1, xs)
              | skipComment (state, l0, c0, l, c, n, #"(" :: #"*" :: xs) = skipComment (state, l0, c0, l, c + 2, n + 1, xs)
              | skipComment (state, l0, c0, l, c, n, #"\n" :: xs) = skipComment (state, l0, c0, l + 1, 1, n, xs)
              | skipComment (state, l0, c0, l, c, n, _ :: xs) = skipComment (state, l0, c0, l, c + 1, n, xs)
              | skipComment (state, l0, c0, _, _, _, nil) = ( emitError (l0, c0, "unterminated comment")
                                                            ; NONE
                                                            )
            and readIdentifierOrKeyword (state, l, c0, c1, startingDot, rstrids, accum, nil)
                = let val name = String.implode (List.rev accum)
                      val (tok, ident) = recognizeKeyword (l, c1, name)
                  in if name = "_Prim" then
                         emitError (l, c1, "_Prim not allowed here")
                     else
                         ()
                   ; case (rstrids, startingDot, ident) of
                         ([], false, _) => SOME (tok, state, l, c1 + String.size name, nil)
                       | ([], true, SOME (name, p2)) => let val p1 = pos (l, c0)
                                                        in SOME (Tokens.DotAlnumIdent (name, p1, p2), state, l, c1 + String.size name, nil)
                                                        end
                       | ([], true, NONE) => ( emitError (l, c0, "stray dot")
                                             ; SOME (tok, state, l, c1 + String.size name, nil)
                                             )
                       | (_, _, SOME (name, p2)) =>
                         ( if startingDot then
                               emitError (l, c0, "stray dot")
                           else
                               ()
                         ; case List.rev rstrids of
                               strids as "_Prim" :: _ => SOME (Tokens.PrimIdent (String.concatWith "." (strids @ [name]), pos (l, c0), p2), state, l, c1 + String.size name, nil)
                             | strids => SOME (Tokens.QualifiedAlnumIdent ((strids, name), pos (l, c0), p2), state, l, c1 + String.size name, nil)
                         )
                       | (_, _, NONE) => ( if startingDot then
                                               emitError (l, c0, "stray dot")
                                           else
                                               ()
                                         ; emitError (l, c1, "invalid qualified name")
                                         ; SOME (tok, state, l, c1 + String.size name, nil)
                                         )
                  end
              | readIdentifierOrKeyword (state, l, c0, c1, startingDot, rstrids, accum, input as x :: xs)
                = if Char.isAlphaNum x orelse x = #"_" orelse x = #"'" then
                      readIdentifierOrKeyword (state, l, c0, c1, startingDot, rstrids, x :: accum, xs)
                  else
                      let val name = String.implode (List.rev accum)
                          val (tok, ident) = recognizeKeyword (l, c1, name)
                      in if List.null rstrids then
                             case ident of
                                 SOME (name, p2) =>
                                 if x = #"." then
                                     let fun finalize () = if startingDot then
                                                               let val p1 = pos (l, c0)
                                                                   val p2 = pos (l, c1 + String.size name)
                                                               in if #allowInfixingDot opts then
                                                                      ()
                                                                  else
                                                                      emitError (l, c0, "stray dot; set \"allowInfixingDot true\" to enable infix identifiers")
                                                                ; SOME (Tokens.InfixIdent (([], name), p1, p2), state, l, c1 + String.size name + 1, xs)
                                                               end
                                                           else
                                                               SOME (tok, state, l, c1 + String.size name, input)
                                     in case xs of
                                            x' :: xs' => if Char.isAlpha x' then
                                                             readIdentifierOrKeyword (state, l, c0, c1 + String.size name + 1, startingDot, name :: rstrids, [x'], xs')
                                                         else if isSymbolChar x' then
                                                             readSymbolicIdentifier (state, l, c0, c1 + String.size name + 1, startingDot, name :: rstrids, [x'], xs')
                                                         else
                                                             finalize ()
                                          | [] => finalize ()
                                     end
                                 else
                                     ( if startingDot then
                                           emitError (l, c0, "stray dot")
                                       else
                                           ()
                                     ; if name = "_Prim" then
                                           emitError (l, c1, "_Prim not allowed here")
                                       else
                                           ()
                                     ; SOME (tok, state, l, c1 + String.size name, input)
                                     )
                               | NONE => ( if startingDot then
                                               emitError (l, c0, "stray dot")
                                           else
                                               ()
                                         ; SOME (tok, state, l, c1 + String.size name, input)
                                         )
                         else
                             ( if name = "_Prim" then
                                   emitError (l, c1, "_Prim not allowed here")
                               else
                                   ()
                             ; case ident of
                                   SOME (name, p2) =>
                                   if x = #"." then
                                       let fun finalize () = if startingDot then
                                                                 ( if #allowInfixingDot opts then
                                                                       ()
                                                                   else
                                                                       emitError (l, c0, "stray dot; set \"allowInfixingDot true\" to enable infix identifiers")
                                                                 ; case List.rev rstrids of
                                                                       strids as "_Prim" :: _ => SOME (Tokens.InfixIdent (([], String.concatWith "." (strids @ [name])), pos (l, c0), pos (l, c1 + String.size name)), state, l, c1 + String.size name + 1, xs)
                                                                     | strids => SOME (Tokens.InfixIdent ((strids, name), pos (l, c0), pos (l, c1 + String.size name)), state, l, c1 + String.size name + 1, xs)
                                                                 )
                                                             else
                                                                 SOME (tok, state, l, c1 + String.size name, input)
                                       in case xs of
                                              x' :: xs' => if Char.isAlpha x' then
                                                               readIdentifierOrKeyword (state, l, c0, c1 + String.size name + 1, startingDot, name :: rstrids, [x'], xs')
                                                           else if isSymbolChar x' then
                                                               readSymbolicIdentifier (state, l, c0, c1 + String.size name + 1, startingDot, name :: rstrids, [x'], xs')
                                                           else
                                                               finalize ()
                                            | [] => finalize ()
                                       end
                                   else
                                       ( if startingDot then
                                             emitError (l, c0, "stray dot")
                                         else
                                             ()
                                       ; case List.rev rstrids of
                                             strids as "_Prim" :: _ => SOME (Tokens.PrimIdent (String.concatWith "." (strids @ [name]), pos (l, c0), p2), state, l, c1 + String.size name, input)
                                           | strids => SOME (Tokens.QualifiedAlnumIdent ((strids, name), pos (l, c0), p2), state, l, c1 + String.size name, input)
                                       )
                                 | NONE => ( if startingDot then
                                                 emitError (l, c0, "stray dot")
                                             else
                                                 ()
                                           ; emitError (l, c1, "invalid qualified name")
                                           ; SOME (tok, state, l, c1 + String.size name, input)
                                           )
                             )
                      end
            and recognizeKeyword (l, c, name) = let val (tok, ident) = case name of
                                                                           "_" => (Tokens.UNDERSCORE, NONE)
                                                                         | "_Prim" => ( if not (#allowPrim opts) then
                                                                                            emitError (l, c, "_Prim is not allowed in user code")
                                                                                        else
                                                                                            ()
                                                                                      ; (Tokens.PRIMNS, SOME "_Prim") (* extension *)
                                                                                      )
                                                                         | "_primCall" => ( if not (#allowPrim opts) then
                                                                                                emitError (l, c, "_primCall is not allowed in user code")
                                                                                            else
                                                                                                ()
                                                                                          ; (Tokens.PRIMCALL, NONE) (* extension *)
                                                                                          )
                                                                         | "_overload" => ( if not (#allowOverload opts) then
                                                                                                emitError (l, c, "_overload is not allowed in user code")
                                                                                            else
                                                                                                ()
                                                                                          ; (Tokens.OVERLOAD, NONE) (* extension *)
                                                                                          )
                                                                         | "_equality" => ( if not (#allowOverload opts) then
                                                                                                emitError (l, c, "_equality is not allowed in user code")
                                                                                            else
                                                                                                ()
                                                                                          ; (Tokens.EQUALITY, NONE) (* extension *)
                                                                                          )
                                                                         | "_esImport" => (Tokens.ESIMPORT, NONE) (* extension *)
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
                                                                         | "from" => (Tokens.FROM, SOME "from") (* contextual keyword *)
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
                                                                         | "pure" => (Tokens.PURE, SOME "pure") (* contextual keyword *)
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
            and readSymbolicIdentifier (state, l, c0, c1, startingDot, rstrids, accum, nil) = let val (tok, ident) = recognizeSymbolic (l, c1, String.implode (rev accum))
                                                                                              in if startingDot then
                                                                                                     case ident of
                                                                                                         SOME (name, p2) => SOME (Tokens.DotSymbolicIdent (name, pos (l, c0), p2), state, l, c1 + length accum, nil)
                                                                                                       | _ => ( emitError (l, c0, "stray dot"); SOME (tok, state, l, c1 + length accum, nil) )
                                                                                                 else
                                                                                                     SOME (tok, state, l, c1 + length accum, nil)
                                                                                              end
              | readSymbolicIdentifier (state, l, c0, c1, startingDot, rstrids, accum, input as x :: xs)
                = if isSymbolChar x then
                      readSymbolicIdentifier (state, l, c0, c1, startingDot, rstrids, x :: accum, xs)
                  else if startingDot andalso x = #"." then
                      let val (tok, ident) = recognizeSymbolic (l, c1, String.implode (rev accum))
                      in if List.null rstrids then
                             case ident of
                                 SOME (name, p2) => ( if #allowInfixingDot opts then
                                                          ()
                                                      else
                                                          emitError (l, c0, "stray dot; set \"allowInfixingDot true\" to enable infix identifiers")
                                                    ; SOME (Tokens.InfixIdent (([], name), pos (l, c0), pos (l, c1 + String.size name)), state, l, c1 + length accum, xs)
                                                    )
                               | _ => ( emitError (l, c0, "stray dot"); SOME (tok, state, l, c1 + length accum, input) )
                         else
                             case ident of
                                 SOME (name, p2) => ( if #allowInfixingDot opts then
                                                          ()
                                                      else
                                                          emitError (l, c0, "stray dot; set \"allowInfixingDot true\" to enable infix identifiers")
                                                    ; case List.rev rstrids of
                                                          strids as "_Prim" :: _ => SOME (Tokens.InfixIdent (([], String.concatWith "." (strids @ [name])), pos (l, c0), pos (l, c1 + String.size name)), state, l, c1 + length accum, xs)
                                                        | strids => SOME (Tokens.InfixIdent ((strids, name), pos (l, c0), pos (l, c1 + String.size name)), state, l, c1 + length accum, xs)
                                                    )
                               | NONE => ( emitError (l, c0, "stray dot")
                                         ; emitError (l, c1, "invalid qualified name")
                                         ; SOME (tok, state, l, c1 + length accum, input)
                                         )
                      end
                  else
                      let val (tok, ident) = recognizeSymbolic (l, c1, String.implode (rev accum))
                      in if List.null rstrids then
                             if startingDot then
                                 case ident of
                                     SOME (name, p2) => SOME (Tokens.DotSymbolicIdent (name, pos (l, c0), p2), state, l, c1 + length accum, input)
                                   | _ => ( emitError (l, c0, "stray dot"); SOME (tok, state, l, c1 + length accum, input) )
                             else
                                 SOME (tok, state, l, c1 + length accum, input)
                         else
                             ( if startingDot then
                                   emitError (l, c0, "stray dot")
                               else
                                   ()
                             ; case ident of
                                   SOME (name, p2) => (case List.rev rstrids of
                                                           strids as "_Prim" :: _ => SOME (Tokens.PrimIdent (String.concatWith "." (strids @ [name]), pos (l, c0), p2), state, l, c1 + length accum, input)
                                                         | strids => SOME (Tokens.QualifiedSymbolicIdent ((strids, name), pos (l, c0), p2), state, l, c1 + length accum, input)
                                                      )
                                 | NONE => ( emitError (l, c1, "invalid qualified name")
                                           ; SOME (tok, state, l, c1 + length accum, input)
                                           )
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
            and skipUnderscoresAndReadDigit (_, c, #"_" :: xs) = skipUnderscoresAndReadDigit (true, c + 1, xs) (* [Successor ML] extended literal syntax (underscore) *)
              | skipUnderscoresAndReadDigit (anyUnderscores, c, x :: xs) = if Char.isDigit x then
                                                                               SOME (anyUnderscores, c + 1, x, xs)
                                                                           else
                                                                               NONE
              | skipUnderscoresAndReadDigit (_, c, []) = NONE
            and skipUnderscoresAndReadHexDigit (c, #"_" :: xs) = skipUnderscoresAndReadHexDigit (c + 1, xs) (* [Successor ML] extended literal syntax (underscore) *)
              | skipUnderscoresAndReadHexDigit (c, x :: xs) = if Char.isHexDigit x then
                                                                  SOME (c + 1, x, xs)
                                                              else
                                                                  NONE
              | skipUnderscoresAndReadHexDigit (c, []) = NONE
            and skipUnderscoresAndReadBinaryDigit (c, #"_" :: xs) = skipUnderscoresAndReadBinaryDigit (c + 1, xs) (* [Successor ML] extended literal syntax (underscore) *)
              | skipUnderscoresAndReadBinaryDigit (c, x :: xs) = if isBinDigit x then
                                                                     SOME (c + 1, x, xs)
                                                                 else
                                                                     NONE
              | skipUnderscoresAndReadBinaryDigit (c, []) = NONE
            and readDecimalConstant (state, l1, c1, c', numericLitType : NumericLitType, x0 : IntInf.int, xs : char list)
                (* x0 is a decimal digit *)
                = let fun mkIntConst (anyUnderscores, p2, a) = if numericLitType = NLTWord then
                                                                   Tokens.WordConst (a, pos (l1, c1), p2)
                                                               else if numericLitType = NLTNegative then
                                                                   Tokens.ZNIntConst (~a, pos (l1, c1), p2)
                                                               else if x0 <> 0 andalso numericLitType = NLTUnsigned andalso not anyUnderscores then
                                                                   Tokens.PosInt (a, pos (l1, c1), p2)
                                                               else
                                                                   Tokens.ZNIntConst (a, pos (l1, c1), p2)
                      fun mkRealConst (p2, intPart : IntInf.int, fracPart : int vector, expPart : int)
                          = let val x = Numeric.DecimalNotation { sign = numericLitType = NLTNegative
                                                                , intPart = intPart
                                                                , fracPart = fracPart
                                                                , exponent = expPart
                                                                }
                            in Tokens.RealConst (x, pos (l1, c1), p2)
                            end
                      fun parseIntPart (anyUnderscores, l, c, a : IntInf.int, rest0 as #"." :: x2 :: rest1)
                          = if numericLitType <> NLTWord andalso Char.isDigit x2 then
                                parseFracPart (l, c+2, a, [digitToInt x2], rest1)
                            else
                                ( emitWarning (l, c, "there should be a space between a numeric literal and a dot")
                                ; SOME (mkIntConst (anyUnderscores, pos (l, c - 1), a), state, l, c, rest0)
                                )
                        | parseIntPart (anyUnderscores, l, c, a, rest0 as x1 :: #"~" :: x2 :: rest1)
                          = if numericLitType <> NLTWord andalso (x1 = #"e" orelse x1 = #"E") andalso Char.isDigit x2 then
                                parseExpPart (l, c+3, a, vector [], ~1, digitToInt x2, rest1)
                            else
                                parseMoreIntPart (anyUnderscores, l, c, a, rest0)
                        | parseIntPart (anyUnderscores, l, c, a, rest0 as x1 :: x2 :: rest1)
                          = if numericLitType <> NLTWord andalso (x1 = #"e" orelse x1 = #"E") andalso Char.isDigit x2 then
                                parseExpPart (l, c+2, a, vector [], 1, digitToInt x2, rest1)
                            else
                                ( if numericLitType <> NLTWord andalso (x1 = #"e" orelse x1 = #"E") andalso x2 = #"-" andalso (case rest1 of x3 :: _ => Char.isDigit x3 | _ => false) then
                                      emitWarning (l, c + 1, "use tilde (~) for negative sign")
                                  else
                                      ()
                                ; parseMoreIntPart (anyUnderscores, l, c, a, rest0)
                                )
                        | parseIntPart (anyUnderscores, l, c, a, rest) = parseMoreIntPart (anyUnderscores, l, c, a, rest)
                      and parseMoreIntPart (anyUnderscores, l, c, a, rest0)
                          = (case skipUnderscoresAndReadDigit (anyUnderscores, c, rest0) of
                                 SOME (anyUnderscores, c', x1, rest1) => parseIntPart (anyUnderscores, l, c', a * 10 + digitToLargeInt x1, rest1)
                               | NONE => ( if (case rest0 of x1 :: _ => Char.isAlpha x1 | _ => false) then
                                               emitWarning (l, c, "there should be a space between a numeric literal and an identifier")
                                           else
                                               ()
                                         ; SOME (mkIntConst (anyUnderscores, pos (l, c - 1), a), state, l, c, rest0)
                                         )
                            )
                      and parseFracPart (l, c, intPart : IntInf.int, revFracPart : int list, rest0)
                          = (case skipUnderscoresAndReadDigit (false, c, rest0) of
                                 SOME (_, c', x, rest1) => parseFracPart (l, c', intPart, digitToInt x :: revFracPart, rest1) (* TODO: a better impl? *)
                               | NONE => let val fracPart = Vector.fromList (List.rev revFracPart)
                                         in case rest0 of
                                                x :: rest1 => if x = #"e" orelse x = #"E" then
                                                                  case rest1 of
                                                                      #"~" :: y :: ys => if Char.isDigit y then
                                                                                             parseExpPart (l, c + 3, intPart, fracPart, ~1, digitToInt y, ys)
                                                                                         else
                                                                                             ( emitWarning (l, c, "there should be a space between a numeric literal and an identifier")
                                                                                             ; SOME (mkRealConst (pos (l, c - 1), intPart, fracPart, 0), state, l, c, rest0)
                                                                                             )
                                                                    | #"-" :: y :: _ => ( if Char.isDigit y then
                                                                                              emitWarning (l, c + 1, "use tilde (~) for negative sign")
                                                                                          else
                                                                                              ()
                                                                                        ; emitWarning (l, c, "there should be a space between a numeric literal and an identifier")
                                                                                        ; SOME (mkRealConst (pos (l, c - 1), intPart, fracPart, 0), state, l, c, rest0)
                                                                                        )
                                                                    | y :: ys => if Char.isDigit y then
                                                                                     parseExpPart (l, c+2, intPart, fracPart, 1, digitToInt y, ys)
                                                                                 else
                                                                                     ( emitWarning (l, c, "there should be a space between a numeric literal and an identifier")
                                                                                     ; SOME (mkRealConst (pos (l, c - 1), intPart, fracPart, 0), state, l, c, rest0)
                                                                                     )
                                                                    | [] => ( emitWarning (l, c, "there should be a space between a numeric literal and an identifier")
                                                                            ; SOME (mkRealConst (pos (l, c - 1), intPart, fracPart, 0), state, l, c, rest0)
                                                                            )
                                                              else
                                                                  ( if Char.isAlpha x then
                                                                        emitWarning (l, c, "there should be a space between a numeric literal and an identifier")
                                                                    else
                                                                        ()
                                                                  ; SOME (mkRealConst (pos (l, c - 1), intPart, fracPart, 0), state, l, c, rest0)
                                                                  )
                                              | [] => SOME (mkRealConst (pos (l, c - 1), intPart, fracPart, 0), state, l, c, rest0)
                                         end
                            )
                      and parseExpPart (l, c, intPart : IntInf.int, fracPart : int vector, expSign : int, expPart : int, rest0)
                          = (case skipUnderscoresAndReadDigit (false, c, rest0) of
                                 SOME (_, c', x, rest1) => parseExpPart (l, c', intPart, fracPart, expSign, expPart * 10 + digitToInt x, rest1)
                               | NONE => SOME (mkRealConst (pos (l, c - 1), intPart, fracPart, expSign * expPart), state, l, c, rest0)
                            )
                  in parseIntPart (false, l1, c', x0, xs)
                  end
            and readHexadecimalConstant (state, l1, c1, c', numericLitType : NumericLitType, x : IntInf.int, xs : char list)
                (* x is a hexadecimal digit *)
                (*
                 * <hexadecimal-integer-constant> ::= '~'? '0' 'w'? 'x' <hexadecimal-digit-sequence>
                 * <hexadecimal-floating-point-constant> ::= '~'? '0x' <hexadecimal-digit-sequence> (<binary-exponent-part> | '.' <hexadecimal-digit-sequence> <binary-exponent-part>?)
                 * <hexadecimal-digit-sequence> ::= <hexadecimal-digit> ('_'* <hexadecimal-digit>)*
                 * <binary-exponent-part> ::= [pP] '~'? <digit> ('_'* <digit>)?
                 *)
                = let fun mkIntConst (p2, a) = if numericLitType = NLTWord then
                                                   Tokens.WordConst (a,pos(l1,c1),p2)
                                               else if numericLitType = NLTNegative then
                                                   Tokens.ZNIntConst (~a,pos(l1,c1),p2)
                                               else
                                                   Tokens.ZNIntConst (a,pos(l1,c1),p2)
                      fun mkRealConst (p2, intPart : IntInf.int, fracPart : int vector, expPart : int)
                          = let val x = Numeric.HexadecimalNotation { sign = numericLitType = NLTNegative
                                                                    , intPart = intPart
                                                                    , fracPart = fracPart
                                                                    , exponent = expPart
                                                                    }
                            in Tokens.RealConst (x, pos (l1, c1), p2)
                            end
                      fun parseIntPart (l, c, a : IntInf.int, rest0)
                          = (case skipUnderscoresAndReadHexDigit (c, rest0) of
                                 SOME (c', x1, rest1) => parseIntPart (l, c', a * 16 + hexDigitToLargeInt x1, rest1)
                               | NONE => let val (c', optFracPart, rest1) = if numericLitType = NLTWord orelse not (#allowHexFloatConsts opts) then
                                                                                (c, NONE, rest0)
                                                                            else
                                                                                parseFracPart (l, c, rest0)
                                             val (c'', optExpPart, rest2) = if numericLitType = NLTWord orelse not (#allowHexFloatConsts opts) then
                                                                                (c, NONE, rest0)
                                                                            else
                                                                                parseExpPart (c', rest1)
                                         in case rest2 of
                                                #"_" :: _ => emitWarning (l, c'', "extra underscore")
                                              | x :: xs => ( if not (Option.isSome optExpPart) andalso (x = #"p" orelse x = #"P") andalso (case xs of #"-" :: y :: _ => Char.isDigit y | _ => false) then
                                                                 emitWarning (l, c'' + 1, "use tilde (~) for negative sign")
                                                             else
                                                                 ()
                                                           ; if Char.isAlpha x then
                                                                 emitWarning (l, c'', "there should be a space between a numeric literal and an identifier")
                                                             else
                                                                 ()
                                                           )
                                              | _ => ()
                                          ; case (optFracPart, optExpPart) of
                                                (NONE, NONE) => SOME (mkIntConst (pos (l, c'' - 1), a), state, l, c'', rest2)
                                              | (_, _) => SOME (mkRealConst (pos (l, c'' - 1), a, Option.getOpt (optFracPart, vector []), Option.getOpt (optExpPart, 0)), state, l, c'', rest2) (* [extension] hexadecimal floating-point constant *)
                                         end
                            )
                      and parseFracPart (l, c, #"." :: (xs as (x :: xss)))
                          = if Char.isHexDigit x then
                                parseMoreFracPart (c + 2, [hexDigitToInt x], xss)
                            else
                                ( emitError (l, c, "malformed number: fractional part must not be empty")
                                ; (c + 1, SOME (vector []), xs)
                                )
                        | parseFracPart (l, c, xs) = (c, NONE, xs)
                      and parseMoreFracPart (c, revAcc, xs)
                          = (case skipUnderscoresAndReadHexDigit (c, xs) of
                                 SOME (c', x, xss) => parseMoreFracPart (c', hexDigitToInt x :: revAcc, xss)
                               | NONE => (c, SOME (vector (List.rev revAcc)), xs)
                            )
                      and parseExpPart (c, xs as (p :: #"~" :: x :: xss)) = if (p = #"p" orelse p = #"P") andalso Char.isDigit x then
                                                                                parseMoreExpPart (c + 2, ~1, digitToInt x, xss)
                                                                            else
                                                                                (c, NONE, xs)
                        | parseExpPart (c, xs as (p :: x :: xss)) = if (p = #"p" orelse p = #"P") andalso Char.isDigit x then
                                                                        parseMoreExpPart (c + 1, 1, digitToInt x, xss)
                                                                    else
                                                                        (c, NONE, xs)
                        | parseExpPart (c, xs) = (c, NONE, xs)
                      and parseMoreExpPart (c, sign, acc, xs) = (case skipUnderscoresAndReadDigit (false, c, xs) of
                                                                     SOME (_, c', x, xss) => parseMoreExpPart (c', sign, acc * 10 + digitToInt x, xss)
                                                                   | NONE => (c', SOME (sign * acc), xs)
                                                                )
                  in parseIntPart (l1, c', x, xs)
                  end
            and readBinaryConstant (state, l1, c1, c', numericLitType : NumericLitType, x : IntInf.int, xs : char list)
                (* x is a binary digit *)
                = let fun mkIntConst (p2, a) = if numericLitType = NLTWord then
                                                   Tokens.WordConst (a, pos (l1, c1), p2)
                                               else if numericLitType = NLTNegative then
                                                   Tokens.ZNIntConst (~a, pos (l1, c1), p2)
                                               else
                                                   Tokens.ZNIntConst (a, pos (l1, c1), p2)
                      fun parseIntPart (l, c, a, rest0) = (case skipUnderscoresAndReadBinaryDigit (c, rest0) of
                                                               SOME (c', x1, rest1) => parseIntPart (l, c', a * 2 + digitToLargeInt x1, rest1)
                                                             | NONE => ( if (case rest0 of x :: _ => Char.isDigit x | _ => false) then
                                                                             emitWarning (l, c, "there should be a space between numeric literals")
                                                                         else if (case rest0 of x :: _ => Char.isAlpha x | _ => false) then
                                                                             emitWarning (l, c, "there should be a space between a numeric literal and an identifier")
                                                                         else
                                                                             ()
                                                                       ; SOME (mkIntConst (pos (l, c - 1), a), state, l, c, rest0)
                                                                       )
                                                          )
                  in parseIntPart (l1, c', x, xs)
                  end
            and readStringLit (l0, c0, l, c, accum, nil) = ( emitError (l0, c0, "unterminated string literal")
                                                           ; (l, c, rev accum, nil)
                                                           )
              | readStringLit (l0, c0, l, c, accum, #"\"" :: xs) = (l, c+1, rev accum, xs)
              | readStringLit (l0, c0, l, c, accum, #"\\" :: #"a" :: xs) = readStringLit (l0, c0, l, c + 2, StringElement.CODEUNIT (ord #"\a") :: accum, xs) (* bell *)
              | readStringLit (l0, c0, l, c, accum, #"\\" :: #"b" :: xs) = readStringLit (l0, c0, l, c + 2, StringElement.CODEUNIT (ord #"\b") :: accum, xs) (* backspace *)
              | readStringLit (l0, c0, l, c, accum, #"\\" :: #"t" :: xs) = readStringLit (l0, c0, l, c + 2, StringElement.CODEUNIT (ord #"\t") :: accum, xs) (* horizontal tab *)
              | readStringLit (l0, c0, l, c, accum, #"\\" :: #"n" :: xs) = readStringLit (l0, c0, l, c + 2, StringElement.CODEUNIT (ord #"\n") :: accum, xs) (* line feed *)
              | readStringLit (l0, c0, l, c, accum, #"\\" :: #"v" :: xs) = readStringLit (l0, c0, l, c + 2, StringElement.CODEUNIT (ord #"\v") :: accum, xs) (* vertical tab *)
              | readStringLit (l0, c0, l, c, accum, #"\\" :: #"f" :: xs) = readStringLit (l0, c0, l, c + 2, StringElement.CODEUNIT (ord #"\f") :: accum, xs) (* form feed *)
              | readStringLit (l0, c0, l, c, accum, #"\\" :: #"r" :: xs) = readStringLit (l0, c0, l, c + 2, StringElement.CODEUNIT (ord #"\r") :: accum, xs) (* carriage return *)
              | readStringLit (l0, c0, l, c, accum, #"\\" :: #"^" :: x :: xs) (* control character *)
                = if 64 <= ord x andalso ord x <= 95 then
                      readStringLit (l0, c0, l, c+3, StringElement.CODEUNIT (ord x - 64) :: accum, xs)
                  else
                      ( emitError (l, c, "invalid control character")
                      ; readStringLit (l0, c0, l, c+3, accum, xs)
                      )
              | readStringLit (l0, c0, l, c1, accum, #"\\" :: #"u" :: #"{" :: x0 :: xs)
                = let fun go (c, scalar, #"}" :: ys) = if scalar > 0x10FFFF then
                                                           ( emitError (l, c1, "invalid \\u{} escape sequence: ordinal too large")
                                                           ; readStringLit (l0, c0, l, c, accum, ys)
                                                           )
                                                       else if 0xD800 <= scalar andalso scalar <= 0xDFFF then
                                                           ( emitError (l, c1, "invalid \\u{} escape sequence: surrogate code point not allowed")
                                                           ; readStringLit (l0, c0, l, c, accum, ys)
                                                           )
                                                       else
                                                           ( if not (#allowUtfEscapeSequences opts) then
                                                                 emitError (l, c1, "\\u{} escape sequence is not allowed; enable allowUtfEscapeSequences to use it")
                                                             else
                                                                 ()
                                                           ; readStringLit (l0, c0, l, c + 1, StringElement.UNICODE_SCALAR scalar :: accum, ys)
                                                           )
                        | go (c, scalar, y :: ys) = if Char.isHexDigit y then
                                                        go (c + 1, scalar * 16 + hexDigitToInt y, ys)
                                                    else
                                                        ( emitError (l, c, "invalid \\u{} escape sequence")
                                                        ; readStringLit (l0, c0, l, c, accum, y :: ys)
                                                        )
                        | go (c, scalar, ys as []) = readStringLit (l0, c0, l, c, accum, ys) (* unterminated string literal *)
                  in if Char.isHexDigit x0 then
                         go (c1 + 4, hexDigitToInt x0, xs)
                     else
                         ( emitError (l, c1, "invalid \\u{} escape sequence")
                         ; readStringLit (l0, c0, l, c1 + 3, accum, x0 :: xs)
                         )
                  end
              | readStringLit (l0, c0, l, c, accum, #"\\" :: #"u" :: (xs' as (x0 :: x1 :: x2 :: x3 :: xs)))
                = if Char.isHexDigit x0 andalso Char.isHexDigit x1 andalso Char.isHexDigit x2 andalso Char.isHexDigit x3 then
                      let val charOrd = ((hexDigitToInt x0 * 16 + hexDigitToInt x1) * 16 + hexDigitToInt x2) * 16 + hexDigitToInt x3;
                      in readStringLit (l0, c0, l, c + 6, StringElement.CODEUNIT charOrd :: accum, xs)
                      end
                  else
                      ( emitError (l, c, "invalid \\u escape sequence")
                      ; readStringLit (l0, c0, l, c+2, accum, xs')
                      )
              | readStringLit (l0, c0, l, c, accum, #"\\" :: #"U" :: (xs' as (x0 :: x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: xs)))
                = if Char.isHexDigit x0 andalso Char.isHexDigit x1 andalso Char.isHexDigit x2 andalso Char.isHexDigit x3 andalso Char.isHexDigit x4 andalso Char.isHexDigit x5 andalso Char.isHexDigit x6 andalso Char.isHexDigit x7 then
                      let val charOrd = ((((((hexDigitToInt x0 * 16 + hexDigitToInt x1) * 16 + hexDigitToInt x2) * 16 + hexDigitToInt x3) * 16 + hexDigitToInt x4) * 16 + hexDigitToInt x5) * 16 + hexDigitToInt x6) * 16 + hexDigitToInt x7;
                          val () = if not (#allowExtendedTextConsts opts) then
                                       emitError (l, c, "\\U escape sequence is not allowed; enable allowExtendedTextConsts to use it")
                                   else
                                       ()
                      in readStringLit (l0, c0, l, c + 6, StringElement.CODEUNIT charOrd :: accum, xs)
                      end
                  else
                      ( emitError (l, c, "invalid \\U escape sequence")
                      ; readStringLit (l0, c0, l, c + 2, accum, xs')
                      )
              | readStringLit (l0, c0, l, c, accum, #"\\" :: #"\"" :: xs) = readStringLit (l0, c0, l, c + 2, StringElement.CODEUNIT (ord #"\"") :: accum, xs)
              | readStringLit (l0, c0, l, c, accum, #"\\" :: #"\\" :: xs) = readStringLit (l0, c0, l, c + 2, StringElement.CODEUNIT (ord #"\\") :: accum, xs)
              | readStringLit (l0, c0, l, c, accum, #"\\" :: x :: xs)
                = if Char.isDigit x then
                      case xs of
                          d1 :: d2 :: xs' => if Char.isDigit d1 andalso Char.isDigit d2 then
                                                 let val charOrd = digitToInt x * 100 + digitToInt d1 * 10 + digitToInt d2;
                                                 in readStringLit (l0, c0, l, c + 4, StringElement.CODEUNIT charOrd :: accum, xs')
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
              | readStringLit (l0, c0, l, c, accum, x :: xs) = readStringLit (l0, c0, l, c+1, StringElement.CODEUNIT (ord x) :: accum, xs) (* TODO: Check if the character is printable *)
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
            fun tokenizeAll (state, l, c, xs, revAcc) = case tokenizeOne (state, l, c, xs) of
                                                     NONE => List.rev revAcc
                                                   | SOME (t, state', l', c', rest) => tokenizeAll (state', l', c', rest, t :: revAcc)
        in tokenizeAll (NORMAL, 1, 1, String.explode s, [])
        end
        fun readAll input = let val x = input 1024
                            in if x = "" then
                                   ""
                               else
                                   x ^ readAll input
                            end
        fun makeLexer input (name, opts, messageHandler)
            = let val tokens = tokenizeAllString (messageHandler, opts, name, readAll input)
                  val tokensRef = ref tokens
              in fn _ => case !tokensRef of
                             nil => Tokens.EOF ({ file = name, line = 0, column = 0 }, { file = name, line = 0, column = 0 })
                           | x :: xs => (tokensRef := xs; x)
              end
        fun makeInputFromString str = let val i = ref false
                                      in fn _ => if !i then
                                                     ""
                                                 else
                                                     (i := true; str)
                                      end
end; (* functor LunarMLLexFun *)
