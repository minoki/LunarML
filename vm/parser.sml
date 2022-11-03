structure IRLrVals = IRLrValsFun (structure Token = LrParser.Token)
structure IRLex = IRLexFun (structure Tokens = IRLrVals.Tokens)
structure IRParser = Join (structure Lex = IRLex
                           structure ParserData = IRLrVals.ParserData
                           structure LrParser = LrParser);
