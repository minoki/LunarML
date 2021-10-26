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
                                    (*
fun factor () : int P.parser = int <|> (CP.char #"(" >> P.delay expr >>= (fn x => CP.char #")" >> P.pure x))
and term () = factor () >>= term1
and term1 x = (CP.char #"*" >> factor () >>= (fn y => term1 (x * y)))
                  <|> (CP.char #"/" >> factor () >>= (fn y => term1 (x div y)))
                  <|> P.pure x
and expr () = term () >>= expr1
and expr1 x = (CP.char #"+" >> term () >>= (fn y => expr1 (x + y)))
                  <|> (CP.char #"-" >> term() >>= (fn y => expr1 (x - y)))
                  <|> P.pure x
                                    *)
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
