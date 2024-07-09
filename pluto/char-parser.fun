functor CharParser(P: PARSER_COMBINATOR where type Stream.token = char):
  CHAR_PARSER
  where type string = string
  where type char = char
  where type 'a parser = 'a P.parser =
struct
  type string = string
  type char = char
  type 'a parser = 'a P.parser
  infix 4 <$> <$ <*>
  infix 1 >> >>=
  infixr 1 <|>
  infix 0 <?>
  open P.Operators
  fun char c =
    P.token (fn c' => if c = c' then SOME c else NONE)
  fun oneOf xs =
    P.choice (List.map char xs)
  fun noneOf xs =
    P.token (fn c => if List.exists (fn c' => c = c') xs then NONE else SOME c)
  fun satisfy pred =
    P.token (fn c => if pred c then SOME c else NONE)
  val space = satisfy Char.isSpace <?> "space"
  val spaces = (P.many space <?> "white space") >>= (fn _ => P.pure ())
  val newline = char #"\n" <?> "lf new-line"
  val crlf = char #"\r" >> char #"\n" <?> "crlf new-line"
  val endOfLine = newline <|> crlf <?> "new-line"
  val tab = char #"\t" <?> "tab"
  val upper = satisfy Char.isUpper <?> "uppercase letter"
  val lower = satisfy Char.isLower <?> "lowercase letter"
  val alphaNum = satisfy Char.isAlphaNum <?> "letter or digit"
  val letter = satisfy Char.isAlpha <?> "letter"
  val digit = satisfy Char.isDigit <?> "digit"
  val hexDigit = satisfy Char.isHexDigit <?> "hexadecimal digit"
  val octDigit = satisfy (fn c => #"0" <= c andalso c <= #"7") <?> "octal digit"
  fun digitToInt c =
    if #"0" <= c andalso c <= #"9" then ord c - ord #"0"
    else if #"A" <= c andalso c <= #"F" then ord c - ord #"A" + 10
    else ord c - ord #"a" + 10
  val digitValue = digitToInt <$> digit
  val hexDigitValue = digitToInt <$> hexDigit
  val octDigitValue = digitToInt <$> octDigit
  val anyChar = P.anyToken
  fun string s =
    let
      val n = String.size s
      fun go i =
        if i >= n then P.pure s else char (String.sub (s, i)) >> go (i + 1)
    in
      go 0
    end
end
