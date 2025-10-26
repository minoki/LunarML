(*
 * Copyright (c) 2025 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure AlbireoLrVals = AlbireoLrValsFun (structure Token = LrParser.Token)
structure AlbireoLex = AlbireoLexFun (structure Tokens = AlbireoLrVals.Tokens)
structure AlbireoParser =
  JoinWithArg
    (structure Lex = AlbireoLex
     structure ParserData = AlbireoLrVals.ParserData
     structure LrParser = LrParser)
