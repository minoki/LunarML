structure DamepoMLLrVals = DamepoMLLrValsFun(structure Token = LrParser.Token)
structure DamepoMLLex = DamepoMLLexFun(structure Tokens = DamepoMLLrVals.Tokens)
structure DamepoMLParser = JoinWithArg(structure Lex = DamepoMLLex
                                       structure ParserData = DamepoMLLrVals.ParserData
                                       structure LrParser = LrParser)
