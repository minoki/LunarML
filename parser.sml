structure DamepoMLLrVals = DamepoMLLrValsFun(structure Token = LrParser.Token)
structure DamepoMLLex = DamepoMLLexFun(structure Tokens = DamepoMLLrVals.Tokens)
structure DamepoMLParser = Join(structure Lex = DamepoMLLex
                                structure ParserData = DamepoMLLrVals.ParserData
                                structure LrParser = LrParser)
