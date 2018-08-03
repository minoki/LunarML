datatype Token = Ident of string
               | Symbolic of string
               | IntLit of string
               | WordLit of string
               | RealLit of string
               | StringLit of string
               | CharLit of string
               (* Reserved Words *)
               | Kabstype | Kand | Kandalso | Kas | Kcase | Kdatatype | Kdo
               | Kelse | Kend | Keqtype | Kexception | Kfn | Kfun | Kfunctor
               | Khandle | Kif | Kin | Kinclude | Kinfix | Kinfixr | Klet
               | Klocal | Knonfix | Kof | Kop | Kopen | Korelse | Kraise | Krec
               | Ksharing | Ksig | Ksignature | Kstruct | Kstructure | Kthen
               | Ktype | Kval | Kwith | Kwithtype | Kwhere | Kwhile
               | LPAREN | RPAREN | LBRACK | RBRACK | LBRACE | RBRACE | COMMA
               | COLON | SEMICOLON | DOT | ELLIPSIS | UNDERSCORE | BAR | EQUAL
               | DARROW | ARROW | HASH | COLONGT;
exception TokError of string;
(* read a token *)
fun tokenizeAll xs = case tokenizeOne xs of
                         NONE => nil
                       | SOME (t, rest) => t :: tokenizeAll rest
and tokenizeOne nil = NONE (* end of input *)
  | tokenizeOne (#"(" :: #"*" :: xs) = skipComment (0, xs) (* beginning of comment *)
  | tokenizeOne (#"(" :: xs) = SOME (LPAREN, xs)
  | tokenizeOne (#")" :: xs) = SOME (RPAREN, xs)
  | tokenizeOne (#"[" :: xs) = SOME (LBRACK, xs)
  | tokenizeOne (#"]" :: xs) = SOME (RBRACK, xs)
  | tokenizeOne (#"{" :: xs) = SOME (LBRACE, xs)
  | tokenizeOne (#"}" :: xs) = SOME (RBRACE, xs)
  | tokenizeOne (#"," :: xs) = SOME (COMMA, xs)
  | tokenizeOne (#";" :: xs) = SOME (SEMICOLON, xs)
  | tokenizeOne (#"." :: #"." :: #"." :: xs) = SOME (ELLIPSIS, xs)
  | tokenizeOne (#"." :: xs) = SOME (DOT, xs)
  | tokenizeOne (#"#" :: #"\"" :: xs) = let val (str, rest) = readStringLit (nil, xs)
                                        in SOME (CharLit (implode str), rest)
                                        end
  | tokenizeOne (#"\"" :: xs) = let val (str, rest) = readStringLit (nil, xs)
                                in SOME (StringLit (implode str), rest)
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
                                "_" => UNDERSCORE
                              | "abstype" => Kabstype
                              | "and" => Kand
                              | "andalso" => Kandalso
                              | "as" => Kas
                              | "case" => Kcase
                              | "datatype" => Kdatatype
                              | "do" => Kdo
                              | "else" => Kelse
                              | "end" => Kend
                              | "exception" => Kexception
                              | "fn" => Kfn
                              | "fun" => Kfun
                              | "handle" => Khandle
                              | "if" => Kif
                              | "in" => Kin
                              | "infix" => Kinfix
                              | "infixr" => Kinfixr
                              | _ => Ident name
and readSymbolicIdentifier (accum, nil) = SOME (recognizeSymbolic (String.implode (rev accum)), nil)
  | readSymbolicIdentifier (accum, input as x :: xs) = if isSymbolChar x then
                                                           readSymbolicIdentifier (x :: accum, xs)
                                                       else
                                                           SOME (recognizeSymbolic (String.implode (rev accum)), x :: xs)
and recognizeSymbolic name = case name of
                                 ":" => COLON
                               | "|" => BAR
                               | "=" => EQUAL
                               | "=>" => DARROW
                               | "->" => ARROW
                               | "#" => HASH
                               | _ => Symbolic name
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
