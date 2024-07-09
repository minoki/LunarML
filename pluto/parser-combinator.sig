signature PARSER_COMBINATOR =
sig
  structure Stream: TOKEN_STREAM
  type state
  type 'a parser
  datatype 'a result = ParseError of string | Ok of 'a * state
  val runParser: 'a parser -> state -> string -> Stream.stream -> 'a result
  val delay: (unit -> 'a parser) -> 'a parser
  val fix: ('a parser -> 'a parser) -> 'a parser
  val <$> : ('a -> 'b) * 'a parser -> 'b parser (* infix 4 *)
  val <$ : 'a * 'b parser -> 'a parser (* infix 4 *)
  val pure: 'a -> 'a parser
  val <*> : ('a -> 'b) parser * 'a parser -> 'b parser (* infix 4 *)
  val <* : 'a parser * 'b parser -> 'a parser (* infix 4 *)
  val >> : 'a parser * 'b parser -> 'b parser (* infix 1 *)
  val >>= : 'a parser * ('a -> 'b parser) -> 'b parser (* infix 1 *)
  val <|> : 'a parser * 'a parser -> 'a parser (* infixr 1 *)
  val fail: string -> 'a parser
  val <?> : 'a parser * string -> 'a parser (* infix 0 *)
  val label: 'a parser * string -> 'a parser
  val try: 'a parser -> 'a parser
  val getState: state parser
  val setState: state -> unit parser
  val modifyState: (state -> state) -> unit parser
  val token: (Stream.token -> 'a option) -> 'a parser
  val choice: 'a parser list -> 'a parser
  val many: 'a parser -> 'a list parser
  val many1: 'a parser -> 'a list parser
  val skipMany: 'a parser -> unit parser
  val skipMany1: 'a parser -> unit parser
  val sepBy: 'a parser * 'sep parser -> 'a list parser
  val sepBy1: 'a parser * 'sep parser -> 'a list parser
  val anyToken: Stream.token parser
  val optional: 'a parser -> 'a option parser
  val optional_: 'a parser -> unit parser
  val notFollowedBy: 'a parser -> unit parser
  val eof: unit parser
  val between: 'open parser * 'close parser -> 'a parser -> 'a parser
  val currentPos: Stream.pos parser
  val withPos: 'a parser -> ('a * Stream.pos * Stream.pos) parser
  structure Operators:
  sig
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
