functor DamepoMLLexFun(structure Tokens: DamepoML_TOKENS) = struct
        structure UserDeclarations = struct
        type pos = int
        type svalue = Tokens.svalue
        type ('a,'b) token = ('a,'b) Tokens.token
        type result = (svalue,pos) token
        end
        exception TokError of string;
        (* read a token *)
        fun tokenizeAllString s = tokenizeAll (String.explode s)
        and tokenizeAll xs = case tokenizeOne xs of
                                 NONE => nil
                               | SOME (t, rest) => t :: tokenizeAll rest
        and tokenizeOne nil = NONE (* end of input *)
          | tokenizeOne (#"(" :: #"*" :: xs) = skipComment (0, xs) (* beginning of comment *)
          | tokenizeOne (#"(" :: xs) = SOME (Tokens.LPAREN (0,0), xs)
          | tokenizeOne (#")" :: xs) = SOME (Tokens.RPAREN (0,0), xs)
          | tokenizeOne (#"[" :: xs) = SOME (Tokens.LBRACK (0,0), xs)
          | tokenizeOne (#"]" :: xs) = SOME (Tokens.RBRACK (0,0), xs)
          | tokenizeOne (#"{" :: xs) = SOME (Tokens.LBRACE (0,0), xs)
          | tokenizeOne (#"}" :: xs) = SOME (Tokens.RBRACE (0,0), xs)
          | tokenizeOne (#"," :: xs) = SOME (Tokens.COMMA (0,0), xs)
          | tokenizeOne (#";" :: xs) = SOME (Tokens.SEMICOLON (0,0), xs)
          | tokenizeOne (#"." :: #"." :: #"." :: xs) = SOME (Tokens.ELLIPSIS (0,0), xs)
          | tokenizeOne (#"." :: xs) = SOME (Tokens.DOT (0,0), xs)
          | tokenizeOne (#"#" :: #"\"" :: xs) = let val (str, rest) = readStringLit (nil, xs)
                                                in SOME (Tokens.CharacterConst (implode str,0,0), rest)
                                                end
          | tokenizeOne (#"\"" :: xs) = let val (str, rest) = readStringLit (nil, xs)
                                        in SOME (Tokens.StringConst (implode str,0,0), rest)
                                        end
          | tokenizeOne (#"~" :: (xs as x :: xss)) = if Char.isDigit x then
                                                         readNumericConstant (#"~" :: xs)
                                                     else
                                                         readSymbolicIdentifier ([#"~"], xs)
          | tokenizeOne (#"0" :: #"w" :: #"x" :: xs) = raise TokError "Word constant: not implemented yet"
          | tokenizeOne (#"0" :: #"w" :: xs) = raise TokError "Word constant: not implemented yet"
          | tokenizeOne (#"0" :: #"x" :: xs) = raise TokError "Hexadecimal constant: not implemented yet"
          | tokenizeOne (x :: xs) = if Char.isAlpha x orelse x = #"_" orelse x = #"'" then
                                        readIdentifierOrKeyword ([x], xs)
                                    else if Char.isDigit x then
                                        raise Fail "not impl"
                                    else if isSymbolChar x then
                                        readSymbolicIdentifier ([x], xs)
                                    else if Char.isSpace x then
                                        tokenizeOne xs
                                    else
                                        raise Fail "not impl" (* error? *)
        (* isAscii, isAlpha, isAlphaNum, isDigit, isSpace *)
        and skipComment (n, #"*" :: #")" :: xs) = if n = 0 then
                                              tokenizeOne xs
                                                  else
                                                      skipComment (n - 1, xs)
          | skipComment (n, #"(" :: #"*" :: xs) = skipComment (n + 1, xs)
          | skipComment (n, _ :: xs) = skipComment (n, xs)
          | skipComment (_, nil) = raise TokError "Unended comment"
        and readIdentifierOrKeyword (accum, nil) = SOME (recognizeKeyword (String.implode (rev accum)), nil)
          | readIdentifierOrKeyword (accum, input as x :: xs) = if Char.isAlphaNum x orelse x = #"_" orelse x = #"'" then
                                                                    readIdentifierOrKeyword (x :: accum, xs)
                                                                else
                                                                    SOME (recognizeKeyword (String.implode (rev accum)), x :: xs)
        and recognizeKeyword name = case name of
                                        "_" => Tokens.UNDERSCORE (0,0)
                                      | "abstype" => Tokens.ABSTYPE (0,0)
                                      | "and" => Tokens.AND (0,0)
                                      | "andalso" => Tokens.ANDALSO (0,0)
                                      | "as" => Tokens.AS (0,0)
                                      | "case" => Tokens.CASE (0,0)
                                      | "datatype" => Tokens.DATATYPE (0,0)
                                      | "do" => Tokens.DO (0,0)
                                      | "else" => Tokens.ELSE (0,0)
                                      | "end" => Tokens.END (0,0)
                                      | "eqtype" => Tokens.EQTYPE (0,0)
                                      | "exception" => Tokens.EXCEPTION (0,0)
                                      | "fn" => Tokens.FN (0,0)
                                      | "fun" => Tokens.FUN (0,0)
                                      | "functor" => Tokens.FUNCTOR (0,0)
                                      | "handle" => Tokens.HANDLE (0,0)
                                      | "if" => Tokens.IF (0,0)
                                      | "in" => Tokens.IN (0,0)
                                      | "include" => Tokens.INCLUDE (0,0)
                                      | "infix" => Tokens.INFIX (0,0)
                                      | "infixr" => Tokens.INFIXR (0,0)
                                      | "let" => Tokens.LET (0,0)
                                      | "local" => Tokens.LOCAL (0,0)
                                      | "nonfix" => Tokens.NONFIX (0,0)
                                      | "of" => Tokens.OF (0,0)
                                      | "op" => Tokens.OP (0,0)
                                      | "open" => Tokens.OPEN (0,0)
                                      | "orelse" => Tokens.ORELSE (0,0)
                                      | "raise" => Tokens.RAISE (0,0)
                                      | "rec" => Tokens.REC (0,0)
                                      | "sharing" => Tokens.SHARING (0,0)
                                      | "sig" => Tokens.SIG (0,0)
                                      | "signature" => Tokens.SIGNATURE (0,0)
                                      | "struct" => Tokens.STRUCT (0,0)
                                      | "structure" => Tokens.STRUCTURE (0,0)
                                      | "then" => Tokens.THEN (0,0)
                                      | "type" => Tokens.TYPE (0,0)
                                      | "val" => Tokens.VAL (0,0)
                                      | "with" => Tokens.WITH (0,0)
                                      | "withtype" => Tokens.WITHTYPE (0,0)
                                      | "where" => Tokens.WHERE (0,0)
                                      | "while" => Tokens.WHILE (0,0)
                                      | _ => if String.sub(name,0) = #"'" then
                                                 Tokens.PrimeIdent (name,0,0)
                                             else
                                                 Tokens.AlnumIdent (name,0,0)
        and readSymbolicIdentifier (accum, nil) = SOME (recognizeSymbolic (String.implode (rev accum)), nil)
          | readSymbolicIdentifier (accum, input as x :: xs) = if isSymbolChar x then
                                                                   readSymbolicIdentifier (x :: accum, xs)
                                                               else
                                                                   SOME (recognizeSymbolic (String.implode (rev accum)), x :: xs)
        and recognizeSymbolic name = case name of
                                         ":" => Tokens.COLON (0,0)
                                       | "|" => Tokens.BAR (0,0)
                                       | "=" => Tokens.EQUAL (0,0)
                                       | "=>" => Tokens.DARROW (0,0)
                                       | "->" => Tokens.ARROW (0,0)
                                       | "#" => Tokens.HASH (0,0)
                                       | ":>" => Tokens.COLONGT (0,0)
                                       | "*" => Tokens.ASTERISK (0,0)
                                       | _ => Tokens.SymbolicIdent (name,0,0)
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
        and readNumericConstant xs = raise TokError "Notimpl"
        and readStringLit (_, nil) = raise TokError "Unterminated string literal"
          | readStringLit (accum, #"\"" :: xs) = (rev accum, xs)
          | readStringLit (accum, #"\\" :: #"a" :: xs) = readStringLit (#"\a" :: accum, xs) (* bell *)
          | readStringLit (accum, #"\\" :: #"b" :: xs) = readStringLit (#"\b" :: accum, xs) (* backspace *)
          | readStringLit (accum, #"\\" :: #"t" :: xs) = readStringLit (#"\t" :: accum, xs) (* horizontal tab *)
          | readStringLit (accum, #"\\" :: #"n" :: xs) = readStringLit (#"\n" :: accum, xs) (* line feed *)
          | readStringLit (accum, #"\\" :: #"v" :: xs) = readStringLit (#"\v" :: accum, xs) (* vertical tab *)
          | readStringLit (accum, #"\\" :: #"f" :: xs) = readStringLit (#"\f" :: accum, xs) (* form feed *)
          | readStringLit (accum, #"\\" :: #"r" :: xs) = readStringLit (#"\r" :: accum, xs) (* carriage return *)
          | readStringLit (accum, #"\\" :: #"^" :: x :: xs) (* control character *)
            = if 64 <= ord x andalso ord x <= 95 then
                  readStringLit (chr (ord x - 64) :: accum, xs)
              else
                  raise TokError "Invalid control character"
          | readStringLit (accum, #"\\" :: #"u" :: x0 :: x1 :: x2 :: x3 :: xs)
            = if Char.isHexDigit x0 andalso Char.isHexDigit x1 andalso Char.isHexDigit x2 andalso Char.isHexDigit x3 then
                  let val charOrd = ((hexDigitToInt x0 * 16 + hexDigitToInt x1) * 16 + hexDigitToInt x3) + hexDigitToInt x3;
                  in if charOrd > Char.maxOrd then
                         raise TokError "Char ordinal too large"
                     else
                         readStringLit (chr charOrd :: accum, xs)
                  end
              else
                  raise TokError "Invalid \\u escape sequence"
          | readStringLit (accum, #"\\" :: #"\"" :: xs) = readStringLit (#"\"" :: accum, xs)
          | readStringLit (accum, #"\\" :: #"\\" :: xs) = readStringLit (#"\\" :: accum, xs)
          | readStringLit (accum, #"\\" :: x :: xs)
            = if Char.isDigit x then
                  case xs of
                      d1 :: d2 :: xs' => if Char.isDigit d1 andalso Char.isDigit d2 then
                                             let val charOrd = digitToInt x * 100 + digitToInt d1 * 10 + digitToInt d2;
                                             in if charOrd > Char.maxOrd then
                                                    raise TokError "Char ordinal too large"
                                                else
                                                    readStringLit (chr charOrd :: accum, xs)
                                             end
                                         else
                                             raise TokError "Invalid \\<digits> escape sequence"
                    | _ => raise TokError "Invalid \\<digits> escape sequence"
              else if Char.isSpace x then
                  skipFormattingCharacters (accum, xs)
              else
                  raise TokError "Unknown escape sequence"
          | readStringLit (accum, x :: xs) = readStringLit (x :: accum, xs)
        and digitToInt x = ord x - ord #"0"
        and hexDigitToInt x = if ord #"A" <= ord x andalso ord x <= ord #"F" then
                                  ord x - ord #"A"
                              else if ord #"a" <= ord x andalso ord x <= ord #"f" then
                                  ord x - ord #"a"
                              else
                                  ord x - ord #"0"
        and skipFormattingCharacters (_, nil) = raise TokError "Unended string literal"
          | skipFormattingCharacters (accum, #"\\" :: xs) = readStringLit (accum, xs)
          | skipFormattingCharacters (accum, x :: xs) = if Char.isSpace x then
                                                            skipFormattingCharacters (accum, xs)
                                                        else
                                                            raise TokError "Invalid formatting character in string literal"
        fun readAll input = let val x = input 1024
                            in if x = "" then
                                   ""
                               else
                                   x ^ readAll input
                            end
        fun makeLexer input = let val tokens = ref (tokenizeAllString (readAll input))
                              in fn _ => case !tokens of
                                             nil => Tokens.EOF (0,0)
                                           | x :: xs => (tokens := xs; x)
                              end
        fun makeInputFromString str = let val i = ref false
                                      in fn _ => if !i then
                                                     ""
                                                 else
                                                     (i := true; str)
                                      end
        end
