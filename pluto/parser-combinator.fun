functor ParserCombinator
  (S:
   sig
     structure Stream: TOKEN_STREAM
     val showToken: Stream.token -> string
     val showPos: Stream.pos -> string
     val comparePos: Stream.pos * Stream.pos -> order
     type state
   end) :> PARSER_COMBINATOR
           where type Stream.token = S.Stream.token
           where type Stream.pos = S.Stream.pos
           where type Stream.stream = S.Stream.stream
           where type state = S.state =
struct
  structure Stream = S.Stream
  type state = S.state
  type internalState =
    {stream: S.Stream.stream, pos: S.Stream.pos, user: S.state}
  datatype message =
    Message of string
  | Expected of string
  | Unexpected of string
  type parseError = {pos: S.Stream.pos, messages: message list}
  fun mergeError (e1: parseError, e2: parseError) =
    case S.comparePos (#pos e1, #pos e2) of
      EQUAL => {pos = #pos e1, messages = #messages e1 @ #messages e2}
    | LESS => e2
    | GREATER => e1
  fun showError ({pos, messages}: parseError) =
    S.showPos pos ^ ": "
    ^
    String.concatWith ", "
      (List.map
         (fn Message s => s
           | Expected s => "expected " ^ s
           | Unexpected s => "unexpected " ^ s) messages)
  datatype 'a parserResult =
    Ok' of 'a * (* consumed? *) bool * internalState
  | Err of (* consumed? *) bool * parseError
  type 'a parser = internalState -> 'a parserResult
  datatype 'a result = ParseError of string | Ok of 'a * state
  fun runParser p state0 name stream =
    case p {stream = stream, pos = Stream.initialPos name, user = state0} of
      Ok' (result, _, {stream = _, pos = _, user}) => Ok (result, user)
    | Err (_, e) => ParseError (showError e)
  fun delay p =
    fn s => p () s
  fun fix f =
    let
      fun p s = f p s
    in
      p
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
  fun f <$> p =
    fn s =>
      case p s of
        Ok' (result, consumed, s) => Ok' (f result, consumed, s)
      | Err e => Err e
  fun x <$ p =
    fn s =>
      case p s of
        Ok' (_, consumed, s) => Ok' (x, consumed, s)
      | Err e => Err e
  fun pure x =
    fn s => Ok' (x, false, s)
  fun f <*> p =
    fn s =>
      case f s of
        Ok' (f', consumed, s) =>
          (case p s of
             Ok' (x, consumed', s) => Ok' (f' x, consumed orelse consumed', s)
           | Err (consumed', e) => Err (consumed orelse consumed', e))
      | Err e => Err e
  fun p <* q =
    fn s =>
      case p s of
        Ok' (x, consumed, s) =>
          (case q s of
             Ok' (_, consumed', s) => Ok' (x, consumed orelse consumed', s)
           | Err (consumed', e) => Err (consumed orelse consumed', e))
      | Err e => Err e
  infix 1 >> >>=
  fun p >> q =
    fn s =>
      case p s of
        Ok' (_, consumed, s) =>
          (case q s of
             Ok' (y, consumed', s) => Ok' (y, consumed orelse consumed', s)
           | Err (consumed', e) => Err (consumed orelse consumed', e))
      | Err e => Err e
  fun p >>= f =
    fn s =>
      case p s of
        Ok' (x, consumed, s) =>
          (case f x s of
             Ok' (y, consumed', s) => Ok' (y, consumed orelse consumed', s)
           | Err (consumed', e) => Err (consumed orelse consumed', e))
      | Err e => Err e
  infixr 1 <|>
  fun p <|> q =
    fn s =>
      case p s of
        Ok' r => Ok' r
      | Err (false, e1) =>
          (case q s of
             Ok' r => Ok' r
           | Err (true, e2) => Err (true, e2)
           | Err (false, e2) => Err (false, mergeError (e1, e2)))
      | Err (true, e) => Err (true, e)
  fun fail message =
    fn (s: internalState) =>
      Err (false, {pos = #pos s, messages = [Message message]})
  infix 0 <?>
  fun p <?> msg =
    fn (s: internalState) =>
      case p s of
        Ok' r => Ok' r
      | Err (false, _) => Err (false, {pos = #pos s, messages = [Expected msg]})
      | Err e => Err e
  val label = op<?>
  fun try p =
    fn s =>
      case p s of
        Ok' r => Ok' r
      | Err (_, e) => Err (false, e)
  val getState = fn s => Ok' (#user s, false, s)
  fun setState u =
    fn (s: internalState) =>
      Ok' ((), false, {stream = #stream s, pos = #pos s, user = u})
  fun modifyState f =
    fn (s: internalState) =>
      Ok' ((), false, {stream = #stream s, pos = #pos s, user = f (#user s)})
  fun token tokValue =
    fn (s: internalState) =>
      case S.Stream.next (#stream s) of
        NONE =>
          Err (false, {pos = #pos s, messages = [Unexpected "end of input"]})
      | SOME (tok, pos, stream) =>
          (case tokValue tok of
             NONE =>
               Err
                 (false, {pos = pos, messages = [Unexpected (S.showToken tok)]})
           | SOME value =>
               Ok' (value, true, {stream = stream, pos = pos, user = #user s}))
  fun choice [] = fail ""
    | choice (p :: ps) = p <|> choice ps
  fun many p =
    fn s =>
      case p s of
        Ok' (x, true, s) =>
          (case many p s of
             Ok' (xs, _, s) => Ok' (x :: xs, true, s)
           | Err (_, e) => Err (true, e))
      | Ok' (_, false, s) =>
          Err
            ( false
            , { pos = #pos s
              , messages =
                  [Message
                     "many: combinator 'many' is applied to a parser that accepts an empty string"]
              }
            )
      | Err (false, _) => Ok' ([], false, s)
      | Err (true, e) => Err (true, e)
  (* fun many p = (p >>= (fn x => many p >>= (fn xs => pure (x :: xs)))) <|> pure [] *)
  fun many1 p =
    p >>= (fn x => many p >>= (fn xs => pure (x :: xs)))
  fun skipMany p =
    fix (fn skipMany_p => (p >> skipMany_p) <|> pure ())
  fun skipMany1 p = p >> skipMany p
  fun sepBy1 (p, sep) =
    p >>= (fn x => many (sep >> p) >>= (fn xs => pure (x :: xs)))
  fun sepBy (p, sep) =
    sepBy1 (p, sep) <|> pure []
  val anyToken = token SOME
  fun optional p =
    fn s =>
      case p s of
        Ok' (x, consumed, s) => Ok' (SOME x, consumed, s)
      | Err (false, _) => Ok' (NONE, false, s)
      | Err (true, e) => Err (true, e)
  fun optional_ p =
    fn s =>
      case p s of
        Ok' (_, consumed, s) => Ok' ((), consumed, s)
      | Err (false, _) => Ok' ((), false, s)
      | Err (true, e) => Err (true, e)
  fun notFollowedBy p =
    fn s =>
      case p s of
        Ok' (_, _, _) =>
          Err
            ( false
            , {pos = #pos s, messages = [Unexpected "token"]}
            ) (* unexpected 'x' *)
      | Err (_, _) => Ok' ((), false, s)
  val eof = notFollowedBy anyToken <?> "end of input"
  fun between (open_, close) p =
    (open_ >> p) <* close
  val currentPos: Stream.pos parser = fn s => Ok' (#pos s, false, s)
  fun withPos (p: 'a parser) =
    fn s =>
      let
        val pos0 = #pos s
      in
        case p s of
          Ok' (x, consumed, s) => Ok' ((x, pos0, #pos s), consumed, s)
        | Err e => Err e
      end
  structure Operators =
  struct
    val op<$> = op<$>
    val op<$ = op<$
    val op<*> = op<*>
    val op<* = op<*
    val op>> = op>>
    val op>>= = op>>=
    val op<|> = op<|>
    val op<?> = op<?>
  end
end
