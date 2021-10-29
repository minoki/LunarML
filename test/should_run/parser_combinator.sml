signature TOKEN_STREAM = sig
    type token
    type pos
    type stream
    val next : stream -> (token * pos * stream) option
    val initialPos : string -> pos
end
signature STRING_STREAM = sig
    include TOKEN_STREAM where type token = char
                         where type pos = { file : string, line : int, column : int } (* both 1-based *)
    val fromString : { file : string, content : string } -> stream (* (file, content) *)
end
structure StringStream : STRING_STREAM = struct
type token = char
type pos = { file : string, line : int, column : int }
type stream = Substring.substring * pos
fun fromString { file, content } = (Substring.full content, { file = file, line = 1, column = 1 })
fun next (content, { file, line, column }) = case Substring.getc content of
                                                 NONE => NONE
                                               | SOME (c, content) => let val newpos = if c = #"\n" then
                                                                                           { file = file, line = line + 1, column = 1 }
                                                                                       else
                                                                                           { file = file, line = line, column = column + 1 }
                                                                      in SOME (c, newpos, (content, newpos))
                                                                      end
fun initialPos file = { file = file, line = 1, column = 1 }
end
signature PARSER_COMBINATOR = sig
    structure Stream : TOKEN_STREAM
    type state
    type 'a parser
    datatype 'a result = ParseError of string
                       | Ok of 'a * state
    val runParser : 'a parser -> state -> string -> Stream.stream -> 'a result
    val delay : (unit -> 'a parser) -> 'a parser
    val fix : ('a parser -> 'a parser) -> 'a parser
    val <$> : ('a -> 'b) * 'a parser -> 'b parser (* infix 4 *)
    val <$ : 'a * 'b parser -> 'a parser (* infix 4 *)
    val pure : 'a -> 'a parser
    val <*> : ('a -> 'b) parser * 'a parser -> 'b parser (* infix 4 *)
    val <* : 'a parser * 'b parser -> 'a parser (* infix 4 *)
    val >> : 'a parser * 'b parser -> 'b parser (* infix 1 *)
    val >>= : 'a parser * ('a -> 'b parser) -> 'b parser (* infix 1 *)
    val <|> : 'a parser * 'a parser -> 'a parser (* infixr 1 *)
    val fail : string -> 'a parser
    val <?> : 'a parser * string -> 'a parser (* infix 0 *)
    val label : 'a parser * string -> 'a parser
    val try : 'a parser -> 'a parser
    val getState : state parser
    val setState : state -> unit parser
    val modifyState : (state -> state) -> unit parser
    val token : (Stream.token -> 'a option) -> 'a parser
    val choice : 'a parser list -> 'a parser
    val many : 'a parser -> 'a list parser
    val many1 : 'a parser -> 'a list parser
    val skipMany : 'a parser -> unit parser
    val skipMany1 : 'a parser -> unit parser
    val sepBy : 'a parser * 'sep parser -> 'a list parser
    val sepBy1 : 'a parser * 'sep parser -> 'a list parser
    val anyToken : Stream.token parser
    val optional : 'a parser -> 'a option parser
    val optional_ : 'a parser -> unit parser
    val notFollowedBy : 'a parser -> unit parser
    val eof : unit parser
    val between : 'open parser * 'close parser -> 'a parser -> 'a parser
    val currentPos : Stream.pos parser
    val withPos : 'a parser -> ('a * Stream.pos * Stream.pos) parser
    structure Operators : sig
                  val <$> : ('a -> 'b) * 'a parser -> 'b parser (* infix 4 *)
                  val <$ : 'a * 'b parser -> 'a parser (* infix 4 *)
                  val <*> : ('a -> 'b) parser * 'a parser -> 'b parser (* infix 4 *)
                  val <* : 'a parser * 'b parser -> 'a parser (* infix 4 *)
                  val >> : 'a parser * 'b parser -> 'b parser (* infix 1 *)
                  val >>= : 'a parser * ('a -> 'b parser) -> 'b parser (* infix 1 *)
                  val <|> : 'a parser * 'a parser -> 'a parser (* infixr 1 *)
                  val <?> : 'a parser * string -> 'a parser (* infix 0 *)
              end
end
functor ParserCombinator (S : sig
                              structure Stream : TOKEN_STREAM
                              val showToken : Stream.token -> string
                              type state
                          end) :> PARSER_COMBINATOR where type Stream.token = S.Stream.token
                                                    where type Stream.pos = S.Stream.pos
                                                    where type Stream.stream = S.Stream.stream
                                                    where type state = S.state = struct
structure Stream = S.Stream
type state = S.state
type internalState = { stream : Stream.stream, pos : Stream.pos, user : S.state }
type parseError = string
datatype 'a parserResult = Ok' of 'a * bool * internalState
                         | Err of bool * parseError
type 'a parser = internalState -> 'a parserResult
datatype 'a result = ParseError of string
                   | Ok of 'a * state
fun runParser p state0 name stream = case p { stream = stream, pos = Stream.initialPos name, user = state0 } of
                                         Ok' (result, consumed, { stream, pos, user }) => Ok (result, user)
                                       | Err (consumed, e) => ParseError e
fun delay p = fn s => p () s
fun fix f = let fun p s = f p s
            in p
            end
(*
fun fix (f : 'a parser -> 'a parser) = let val r = ref NONE : 'a parser option ref
                                           fun p s = case !r of
                                                         NONE => let val q = f p
                                                                 in r := SOME q
                                                                  ; q s
                                                                 end
                                                       | SOME q => q s
                                       in p
                                       end
*)
infix 4 <$> <$ <*> <*
fun f <$> p = fn s => case p s of
                          Ok' (result, consumed, s) => Ok' (f result, consumed, s)
                        | Err e => Err e
fun x <$ p = fn s => case p s of
                         Ok' (_, consumed, s) => Ok' (x, consumed, s)
                       | Err e => Err e
fun pure x = fn s => Ok' (x, false, s)
fun f <*> p = fn s => case f s of
                          Ok' (f', consumed, s) => (case p s of
                                                        Ok' (x, consumed', s) => Ok' (f' x, consumed orelse consumed', s)
                                                      | Err (consumed', e) => Err (consumed orelse consumed', e)
                                                   )
                        | Err e => Err e
fun p <* q = fn s => case p s of
                         Ok' (x, consumed, s) => (case q s of
                                                      Ok' (_, consumed', s) => Ok' (x, consumed orelse consumed', s)
                                                    | Err (consumed', e) => Err (consumed orelse consumed', e)
                                                 )
                       | Err e => Err e
infix 1 >> >>=
fun p >> q = fn s => case p s of
                         Ok' (_, consumed, s) => (case q s of
                                                      Ok' (y, consumed', s) => Ok' (y, consumed orelse consumed', s)
                                                    | Err (consumed', e) => Err (consumed orelse consumed', e)
                                                 )
                       | Err e => Err e
fun p >>= f = fn s => case p s of
                          Ok' (x, consumed, s) => (case f x s of
                                                       Ok' (y, consumed', s) => Ok' (y, consumed orelse consumed', s)
                                                     | Err (consumed', e) => Err (consumed orelse consumed', e)
                                                  )
                        | Err e => Err e
infixr 1 <|>
fun p <|> q = fn s => case p s of
                          Ok' r => Ok' r
                        | Err (false, _) => q s (* TODO: merge error? *)
                        | Err (true, e) => Err (true, e)
fun fail message = fn s => Err (false, message)
infix 0 <?>
fun p <?> msg = fn s => case p s of
                            Ok' r => Ok' r
                          | Err (false, _) => Err (false, "expected " ^ msg)
                          | Err e => Err e
val label = op <?>
fun try p = fn s => case p s of
                        Ok' r => Ok' r
                      | Err (_, e) => Err (false, e)
val getState = fn s => Ok' (#user s, false, s)
fun setState u = fn (s : internalState) => Ok' ((), false, { stream = #stream s, pos = #pos s, user = u })
fun modifyState f = fn (s : internalState) => Ok' ((), false, { stream = #stream s, pos = #pos s, user = f (#user s) })
fun token tokValue = fn (s : internalState) => case S.Stream.next (#stream s) of
                                                   NONE => Err (false, "unexpected EOF")
                                                 | SOME (tok, pos, stream) => (case tokValue tok of
                                                                                   NONE => Err (false, "unexpected " ^ S.showToken tok)
                                                                                 | SOME value => Ok' (value, true, { stream = stream, pos = pos, user = #user s })
                                                                              )
fun choice [] = fail "unknown error"
  | choice (p :: ps) = p <|> choice ps
fun many p = fn s => case p s of
                         Ok' (x, true, s) => (case many p s of
                                                  Ok' (xs, _, s) => Ok' (x :: xs, true, s)
                                                | Err (_, e) => Err (true, e)
                                             )
                       | Ok' (x, false, s) => Err (false, "many: combinator 'many' is applied to a parser that accepts an empty string")
                       | Err (false, _) => Ok' ([], false, s)
                       | Err (true, e) => Err (true, e)
(* fun many p = (p >>= (fn x => many p >>= (fn xs => pure (x :: xs)))) <|> pure [] *)
fun many1 p = p >>= (fn x => many p >>= (fn xs => pure (x :: xs)))
fun skipMany p = fix (fn skipMany_p => (p >> skipMany_p) <|> pure ())
fun skipMany1 p = p >> skipMany p
fun sepBy1 (p, sep) = p >>= (fn x => many (sep >> p) >>= (fn xs => pure (x :: xs)))
fun sepBy (p, sep) = sepBy1 (p, sep) <|> pure []
val anyToken = token SOME
fun optional p = fn s => case p s of
                             Ok' (x, consumed, s) => Ok' (SOME x, consumed, s)
                           | Err (false, _) => Ok' (NONE, false, s)
                           | Err (true, e) => Err (true, e)
fun optional_ p = fn s => case p s of
                              Ok' (_, consumed, s) => Ok' ((), consumed, s)
                            | Err (false, _) => Ok' ((), false, s)
                            | Err (true, e) => Err (true, e)
fun notFollowedBy p = fn s => case p s of
                                  Ok' (x, _, _) => Err (false, "unknown") (* unexpected 'x' *)
                                | Err (_, _) => Ok' ((), false, s)
val eof = notFollowedBy anyToken <?> "end of input"
fun between (open_, close) p = (open_ >> p) <* close
val currentPos : Stream.pos parser = fn s => Ok' (#pos s, false, s)
fun withPos (p : 'a parser) = fn s => let val pos0 = #pos s
                                      in case p s of
                                             Ok' (x, consumed, s) => Ok' ((x, pos0, #pos s), consumed, s)
                                           | Err e => Err e
                                      end
structure Operators = struct
val op <$> = op <$>
val op <$ = op <$
val op <*> = op <*>
val op <* = op <*
val op >> = op >>
val op >>= = op >>=
val op <|> = op <|>
val op <?> = op <?>
end
end
signature CHAR_PARSER = sig
    eqtype string
    eqtype char
    type 'a parser
    val oneOf : char list -> char parser
    val noneOf : char list -> char parser
    val spaces : unit parser
    val space : char parser
    val newline : char parser
    val crlf : char parser
    val endOfLine : char parser
    val tab : char parser
    val upper : char parser
    val lower : char parser
    val alphaNum : char parser
    val letter : char parser
    val digit : char parser
    val hexDigit : char parser
    val octDigit : char parser
    val digitValue : int parser
    val hexDigitValue : int parser
    val octDigitValue : int parser
    val char : char -> char parser
    val anyChar : char parser
    val satisfy : (char -> bool) -> char parser
    val string : string -> string parser
end
functor CharParser (P : PARSER_COMBINATOR where type Stream.token = char)
        : CHAR_PARSER
          where type string = string
          where type char = char
          where type 'a parser = 'a P.parser = struct
type string = string
type char = char
type 'a parser = 'a P.parser
infix 4 <$> <$ <*>
infix 1 >> >>=
infixr 1 <|>
infix 0 <?>
open P.Operators
fun char c = P.token (fn c' => if c = c' then SOME c else NONE)
fun oneOf xs = P.choice (List.map char xs)
fun noneOf xs = P.token (fn c => if List.exists (fn c' => c = c') xs then NONE else SOME c)
fun satisfy pred = P.token (fn c => if pred c then SOME c else NONE)
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
fun digitToInt c = if #"0" <= c andalso c <= #"9" then
                       ord c - ord #"0"
                   else if #"A" <= c andalso c <= #"F" then
                       ord c - ord #"A" + 10
                   else
                       ord c - ord #"a" + 10
val digitValue = digitToInt <$> digit
val hexDigitValue = digitToInt <$> hexDigit
val octDigitValue = digitToInt <$> octDigit
val anyChar = P.anyToken
fun string s = let val n = String.size s
                   fun go i = if i >= n then
                                  P.pure s
                              else
                                  char (String.sub (s, i)) >> go (i + 1)
               in go 0
               end
end
structure P = ParserCombinator (structure Stream = StringStream
                                fun showToken c = Char.toString c
                                type state = unit
                               )
structure CP = CharParser (P)
open P.Operators
infix 4 <$> <*>
infix 1 >> >>=
infixr 1 <|>
infix 0 <?>
val int : int P.parser = List.foldl (fn (x,y) => 10 * y + (ord x - ord #"0")) 0 <$> P.many1 CP.digit
val expr = P.fix (fn expr => let val factor : int P.parser = int <|> (CP.char #"(" >> expr >>= (fn x => CP.char #")" >> P.pure x))
                                 fun term1 x = (CP.char #"*" >> factor >>= (fn y => term1 (x * y)))
                                               <|> (CP.char #"/" >> factor >>= (fn y => term1 (x div y)))
                                               <|> P.pure x
                                 val term = factor >>= term1
                                 fun expr1 x = (CP.char #"+" >> term >>= (fn y => expr1 (x + y)))
                                               <|> (CP.char #"-" >> term >>= (fn y => expr1 (x - y)))
                                               <|> P.pure x
                             in term >>= expr1
                             end
                 )
fun eval s = case P.runParser expr () "input" (StringStream.fromString { file = "input", content = s }) of
                 P.Ok (x, _) => print ("OK: " ^ Int.toString x ^ "\n")
               | P.ParseError msg => print ("Fail: " ^ msg ^ "\n");
eval "42";
eval "1+1";
eval "2*3";
eval "(1+1)*(2+3)";
eval "3-1-2";
eval "12/2/3";
