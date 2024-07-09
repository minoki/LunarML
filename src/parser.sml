(*
 * Copyright (c) 2021 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure LunarMLLrVals = LunarMLLrValsFun (structure Token = LrParser.Token)
structure LunarMLLex = LunarMLLexFun (structure Tokens = LunarMLLrVals.Tokens)
structure LunarMLParser =
  JoinWithArg
    (structure Lex = LunarMLLex
     structure ParserData = LunarMLLrVals.ParserData
     structure LrParser = LrParser)
