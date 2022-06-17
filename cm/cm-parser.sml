structure CMParser = struct
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
val whiteSpace = P.skipMany (comment <|> (() <$ CP.oneOf [#" ",#"\t"] (* no newline *)) <?> "")
fun lexeme p = p <* whiteSpace
val identLetter = CP.alphaNum <|> CP.oneOf [#"_",#"'"]
fun reserved name = lexeme (P.try (CP.string name >> P.notFollowedBy identLetter))
val opLetter = CP.oneOf [#"=", #">", #"&", #"|"]
fun reservedOp name = lexeme (P.try (CP.string name >> P.notFollowedBy opLetter))
val reservedNames = StringSet.fromList ["and","ann","bas","basis","end","functor","in","let","local","open","signature","structure"]
val pathchar = CP.alphaNum <|> CP.oneOf [#"_",#"-",#".",#"/",#"$"]
val mlIdentifier : CMSyntax.mlid P.parser = lexeme (P.try (CP.letter >>= (fn c0 => P.many (CP.alphaNum <|> CP.oneOf [#"'",#"_"]) >>= (fn cs => P.notFollowedBy pathchar >> let val name = String.implode (c0 :: cs) in if StringSet.member (reservedNames, name) then P.fail "reserved name" else P.pure name end)))) <?> "identifier"
val ns : CMSyntax.mlns P.parser = (CMSyntax.STRUCTURE <$ reserved "structure") <|> (CMSyntax.SIGNATURE <$ reserved "signature") <|> (CMSyntax.FUNCTOR <$ reserved "functor")
val mlSymbol : (CMSyntax.mlns * CMSyntax.mlid) P.parser = ns >>= (fn n => (fn id => (n, id)) <$> mlIdentifier)
val reservedNames = StringSet.fromList ["defined","div","mod","andalso","orelse","not","structure","signature","functor","funsig"]
val cmIdentifier : CMSyntax.cmid P.parser = lexeme (P.try (CP.letter >>= (fn c0 => P.many (CP.alphaNum <|> CP.oneOf [#"'",#"_"]) >>= (fn cs => P.notFollowedBy pathchar >> let val name = String.implode (c0 :: cs) in if StringSet.member (reservedNames, name) then P.fail "reserved name" else P.pure name end)))) <?> "identifier"
val number : int P.parser = let fun goDigits x = (CP.digitValue >>= (fn d => goDigits (x * 10 + d))) <|> P.pure x
                            in lexeme (CP.digitValue >>= goDigits) <?> "number"
                            end
val libkw : unit P.parser = reserved "library" <|> reserved "Library" <|> reserved "LIBRARY"
val groupkw : unit P.parser = reserved "group" <|> reserved "Group" <|> reserved "GROUP"
val sourcekw : unit P.parser = reserved "source" <|> reserved "Source" <|> reserved "SOURCE"
val pathnamechar : char P.parser = CP.alphaNum <|> CP.oneOf [#"'",#"_",#".",#";",#",",#"!",#"%",#"&",#"$",#"+",#"/",#"<",#"=",#">",#"?",#"@",#"~",#"|",#"#",#"*",#"-",#"^"]
val cmKeywords = StringSet.fromList ["structure","signature","functor","funsig","group","Group","GROUP","library","Library","LIBRARY","source","Source","SOURCE","is","IS","*","-"]
val stdpn = lexeme (P.many1 pathnamechar >>= (fn xs => let val s = String.implode xs
                                                       in if StringSet.member (cmKeywords, s) then
                                                              P.fail "keyword"
                                                          else
                                                              P.pure s
                                                       end
                                             ))
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
val colon = lexeme (CP.char #":")
fun paren p = lexeme (CP.char #"(") >> p <* lexeme (CP.char #")")
val pparith = P.fix (fn pparith =>
                        let val aatom = P.fix (fn aatom =>
                                                  (CMSyntax.ANumber <$> number)
                                                      <|> (CMSyntax.ACMId <$> cmIdentifier)
                                                      <|> paren pparith
                                                      <|> (lexeme (CP.char #"~") >> CMSyntax.ANegate <$> aatom)
                                                      <|> (lexeme (CP.char #"-") >> CMSyntax.ANegate <$> aatom)
                                              )
                            fun doProd x = (((lexeme (CP.char #"*") >> (fn y => CMSyntax.ABinary (CMSyntax.MUL, x, y)) <$> aatom)
                                                    <|> (reserved "div" >> (fn y => CMSyntax.ABinary (CMSyntax.DIV, x, y)) <$> aatom)
                                                    <|> (reserved "mod" >> (fn y => CMSyntax.ABinary (CMSyntax.MOD, x, y)) <$> aatom)
                                            ) >>= doProd
                                           ) <|> P.pure x
                            val aprod = aatom >>= doProd
                            fun doSum x = (((lexeme (CP.char #"+") >> (fn y => CMSyntax.ABinary (CMSyntax.PLUS, x, y)) <$> aatom)
                                                   <|> (lexeme (CP.char #"-") >> (fn y => CMSyntax.ABinary (CMSyntax.MINUS, x, y)) <$> aatom)
                                           ) >>= doSum
                                          ) <|> P.pure x
                        in aprod >>= doSum
                        end
                    )
val query = reserved "defined" >> paren (CMSyntax.PPDefinedML <$> mlSymbol <|> CMSyntax.PPDefinedCM <$> cmIdentifier)
val cmpop = (CMSyntax.LT <$ reservedOp "<")
                <|> (CMSyntax.LE <$ reservedOp "<=")
                <|> (CMSyntax.GT <$ reservedOp ">")
                <|> (CMSyntax.GE <$ reservedOp ">=")
                <|> (CMSyntax.A_EQ <$ reservedOp "=")
                <|> (CMSyntax.A_EQ <$ reservedOp "==") (* obsolete *)
                <|> (CMSyntax.A_NE <$ reservedOp "<>")
                <|> (CMSyntax.A_NE <$ reservedOp "!=") (* obsolete *)
val ppbool = P.fix (fn ppbool =>
                       let val batom = P.fix (fn batom =>
                                                 query
                                                     <|> (pparith >>= (fn x => cmpop >>= (fn cmp => (fn y => CMSyntax.PPCompare (cmp, x, y)) <$> pparith)))
                                                     <|> (reserved "not" >> CMSyntax.PPNot <$> batom)
                                                     <|> (reservedOp "!" >> CMSyntax.PPNot <$> batom) (* obsolete *)
                                                     <|> paren ppbool
                                             )
                           val bcmp = batom >>= (fn x =>
                                                    ((reservedOp "=" <|> reservedOp "==" (* obsolete *)) >> (fn y => CMSyntax.PPBoolBinary (CMSyntax.B_EQ, x, y)) <$> batom)
                                                        <|> ((reservedOp "<>" <|> reservedOp "!=" (* obsolete *)) >> (fn y => CMSyntax.PPBoolBinary (CMSyntax.B_NE, x, y)) <$> batom)
                                                        <|> P.pure x
                                                )
                           fun doConj x = (((reserved "andalso" <|> reservedOp "&&" (* obsolete *)) >> (fn y => CMSyntax.PPBoolBinary (CMSyntax.ANDALSO, x, y)) <$> bcmp) >>= doConj)
                                              <|> P.pure x
                           val bconj = bcmp >>= doConj
                           fun doDisj x = (((reserved "orelse" <|> reservedOp "||" (* obsolete *)) >> (fn y => CMSyntax.PPBoolBinary (CMSyntax.ORELSE, x, y)) <$> bcmp) >>= doDisj)
                                              <|> P.pure x
                       in bconj >>= doDisj
                       end
                   )
val pathname = stdpn <|> stringLiteral
fun ppkeyword kwd = CP.char #"#" >> P.many (CP.oneOf [#" ",#"\t"]) >> reserved kwd
val ppif = ppkeyword "if"
val ppelif = ppkeyword "elif"
val ppelse = ppkeyword "else"
val ppendif = ppkeyword "endif"
val pperror = ppkeyword "error"
(* val ppline = ppkeyword "line" *)
fun pplist p = let val line = P.notFollowedBy (CP.char #"#") >> whiteSpace >> P.many p <* P.optional_ CP.endOfLine
               in P.fix (fn entries =>
                            let val rest = P.fix (fn rest =>
                                                     (ppendif >> P.optional_ CP.endOfLine >> P.pure [])
                                                         <|> ((ppelif >> ppbool <* CP.endOfLine) >>= (fn cond => entries >>= (fn then' => (fn else' => [CMSyntax.PPConditional (cond, then', else')]) <$> rest)))
                                                         <|> (ppelse >> CP.endOfLine >> entries <* (ppendif >> CP.endOfLine))
                                                 )
                            in ((ppif >> ppbool <* CP.endOfLine) >>= (fn cond => entries >>= (fn then' => (fn else' => [CMSyntax.PPConditional (cond, then', else')]) <$> rest)))
                                   <|> (fn xs => [CMSyntax.PPError (String.implode xs)]) <$> (pperror >> P.many (CP.noneOf [#"\r",#"\n"]) <* CP.endOfLine)
                                   <|> (List.map CMSyntax.PPJust) <$> line
                                   <|> (CP.endOfLine >> entries)
                                   <|> P.pure []
                            end
                        )
               end
val elst = pplist (CMSyntax.MLSymbol <$> mlSymbol) (* TODO: difference, intersection, union, implicitset *)
val members = pplist ((fn pn => CMSyntax.Member { pathname = pn }) <$> pathname) (* TODO: class, toolopts *)
val is = reserved "is" <|> reserved "IS"
val library = libkw >> elst >>= (fn exports => is >> members >>= (fn members => P.pure (exports, members)))
end;
