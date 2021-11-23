structure MLBParser = struct
structure P = ParserCombinator (structure Stream = StringStream
                                fun showToken c = Char.toString c
                                fun showPos { file, line, column } = file ^ ":" ^ Int.toString line ^ ":" ^ Int.toString column
                                fun comparePos (p : Stream.pos, q : Stream.pos)
                                    = case String.compare (#file p, #file q) of
                                          LESS => LESS
                                        | GREATER => GREATER
                                        | EQUAL => case Int.compare (#line p, #line q) of
                                                       LESS => LESS
                                                     | GREATER => GREATER
                                                     | EQUAL => Int.compare (#column p, #column q)
                                type state = unit
                               )
structure CP = CharParser (P)
structure StringSet = RedBlackSetFn (open String
                                     type ord_key = string
                                    )
open P.Operators
infix 4 <$> <$ <*> <*
infix 1 >> >>=
infixr 1 <|>
infix 0 <?>
val comment = let val commentRest = P.fix (fn commentRest => (P.try (CP.string "(*") >> commentRest >> commentRest)
                                                                    <|> (CP.char #"*" >> (() <$ CP.char #")" <|> commentRest))
                                                                    <|> (CP.anyChar >> commentRest)
                                          )
              in P.try (CP.string "(*") >> commentRest
              end
val whiteSpace = P.skipMany (comment <|> (() <$ CP.space) <?> "")
fun lexeme p = p <* whiteSpace
val pathchar = CP.alphaNum <|> CP.oneOf [#"_",#"-",#".",#"/",#"$"]
val reservedNames = StringSet.fromList ["and","ann","bas","basis","end","functor","in","let","local","open","signature","structure"]
fun reserved s = lexeme (P.try (CP.string s >> P.notFollowedBy pathchar))
val identifier : string P.parser = lexeme (CP.letter >>= (fn c0 => P.many (CP.alphaNum <|> CP.oneOf [#"'",#"_"]) >>= (fn cs => P.notFollowedBy pathchar >> P.pure (String.implode (c0 :: cs)))) <?> "identifier") (* TODO: check the name is not reserved *)
val pathvar : string P.parser = CP.string "$(" >> (CP.upper <|> CP.char #"_") >>= (fn c0 => P.many (CP.upper <|> CP.digit <|> CP.char #"_") >>= (fn cs => CP.char #")" >> P.pure ("$(" ^ String.implode (c0 :: cs) ^ ")")))
val filename : string P.parser = (pathvar <|> String.str <$> (CP.alphaNum <|> CP.oneOf [#"_",#"."])) >>= (fn x => P.many (pathvar <|> String.str <$> (CP.alphaNum <|> CP.oneOf [#"_",#".",#"-"])) >>= (fn xs => P.pure (String.concat (x :: xs))))
val arc = filename (* {pathvar}|{filename}|"."|".." *)
val relpath = String.concat <$> P.many (P.try (arc >>= (fn a => (a ^ "/") <$ CP.char #"/")))
val abspath = CP.char #"/" >> (fn s => "/" ^ s) <$> relpath
val path : string P.parser = relpath <|> abspath
val file : string P.parser = lexeme (P.try (path >>= (fn p => filename >>= (fn f => P.notFollowedBy (CP.char #"'") >> P.pure (p ^ f))) >>= (fn file => if StringSet.member (reservedNames, file) then P.fail "reserved name" else P.pure file)) <?> "file")
val threeDigits : int P.parser = CP.digitValue >>= (fn d1 => CP.digitValue >>= (fn d2 => CP.digitValue >>= (fn d3 => P.pure ((d1 * 10 + d2) * 10 + d3))))
val fourHexDigits : int P.parser = CP.hexDigitValue >>= (fn x1 => CP.hexDigitValue >>= (fn x2 => CP.hexDigitValue >>= (fn x3 => CP.hexDigitValue >>= (fn x4 => P.pure (((x1 * 16 + x2) * 16 + x3) * 16 + x4)))))
val stringLiteral = let val elem = (CP.char #"\\" >> (String.str <$> P.choice [ CP.char #"\""
                                                                              , CP.char #"\\"
                                                                              , #"\a" <$ CP.char #"a"
                                                                              , #"\b" <$ CP.char #"b"
                                                                              , #"\t" <$ CP.char #"t"
                                                                              , #"\n" <$ CP.char #"n"
                                                                              , #"\v" <$ CP.char #"v"
                                                                              , #"\f" <$ CP.char #"f"
                                                                              , #"\r" <$ CP.char #"r"
                                                                              , CP.char #"^" >> CP.satisfy (fn c => #"\064" <= c andalso c <= #"\095") >>= (fn c => P.pure (Char.chr (Char.ord c - 64)))
                                                                              , threeDigits >>= (fn x => if x <= 255 then
                                                                                                             P.pure (Char.chr x)
                                                                                                         else
                                                                                                             P.fail "escape sequence too large"
                                                                                                )
                                                                              , CP.char #"u" >> fourHexDigits >>= (fn x => if x <= 255 then
                                                                                                                               P.pure (Char.chr x)
                                                                                                                           else
                                                                                                                               P.fail "escape sequence too large"
                                                                                                                  )
                                                                              ]
                                                                 <|> (P.skipMany1 (CP.oneOf [#" ",#"\t",#"\n",#"\r",#"\f"]) >> CP.char #"\\" >> P.pure "")
                                                     )
                                   )
                                       <|> (String.str <$> CP.satisfy (fn c => c <> #"\""))
                    in lexeme (P.between (CP.char #"\"", CP.char #"\"") (String.concat <$> P.many elem) <?> "string literal")
                    end
val equal = lexeme (CP.char #"=")
val basdec = P.fix (fn basdec =>
                       let val basexp : MLBSyntax.BasExp P.parser
                               = P.fix (fn basexp =>
                                           P.choice [ reserved "bas" >> MLBSyntax.BasisExp <$> basdec <* reserved "end"
                                                    , reserved "let" >> basdec <* reserved "in" >>= (fn decs => (fn exp => MLBSyntax.LetExp (decs, exp)) <$> basexp <* reserved "end")
                                                    , MLBSyntax.BasIdExp <$> identifier
                                                    ]
                                       )
                           val singlebasdec : MLBSyntax.BasDec P.parser
                               = P.choice [ reserved "basis" >> MLBSyntax.BasisDec <$> P.sepBy1 (identifier <* equal >>= (fn basid => (fn exp => (basid, exp)) <$> basexp), reserved "and")
                                          , reserved "open" >> MLBSyntax.OpenDec <$> P.many1 identifier
                                          , reserved "local" >> (basdec <* reserved "in" >>= (fn decs1 => (fn decs2 => MLBSyntax.LocalDec (decs1, decs2)) <$> basdec)) <* reserved "end"
                                          , reserved "structure" >> MLBSyntax.StructureDec <$> P.sepBy1 (Syntax.MkStrId <$> identifier >>= (fn id1 => (equal >> (fn id2 => (id1, Syntax.MkStrId id2)) <$> identifier)
                                                                                                                                                          <|> P.pure (id1, id1)
                                                                                                                                           ), reserved "and")
                                          , reserved "signature" >> MLBSyntax.SignatureDec <$> P.sepBy1 (Syntax.MkSigId <$> identifier >>= (fn id1 => (equal >> (fn id2 => (id1, Syntax.MkSigId id2)) <$> identifier)
                                                                                                                                                          <|> P.pure (id1, id1)
                                                                                                                                           ), reserved "and")
                                          , reserved "functor" >> MLBSyntax.FunctorDec <$> P.sepBy1 (Syntax.MkFunId <$> identifier >>= (fn id1 => (equal >> (fn id2 => (id1, Syntax.MkFunId id2)) <$> identifier)
                                                                                                                                                      <|> P.pure (id1, id1)
                                                                                                                                       ), reserved "and")
                                          , MLBSyntax.PrimDec <$ reserved "_prim"
                                          , reserved "ann" >> P.many1 stringLiteral <* reserved "in" >>= (fn annotations => (fn dec => MLBSyntax.AnnotationDec (annotations, dec)) <$> basdec <* reserved "end")
                                          , MLBSyntax.PathDec <$> file (* .sml, .sig, .fun, .mlb *)
                                          , MLBSyntax.PathDec <$> stringLiteral
                                          ]
                       in (singlebasdec >>= (fn dec => ((lexeme (CP.char #";") >> (fn decs => dec :: decs) <$> basdec)
                                                          <|> ((fn decs => dec :: decs) <$> basdec))))
                              <|> P.pure []
                       end
                   )
val basfile = whiteSpace >> basdec <* P.eof
end
