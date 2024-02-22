(*
 * Copyright (c) 2024 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure InitFile : sig
              datatype language = LUA | JS
              type chunk = { provides : string list
                           , requires : string list
                           , body : string
                           }
              val readFile : language * string -> chunk list
              val eliminateUnusedChunks : chunk list * string list -> chunk list
              val output : TextIO.outstream * chunk list -> unit
          end = struct
datatype language = LUA | JS
type chunk = { provides : string list
             , requires : string list
             , body : string
             }
datatype token = BEGIN of { provides : string list, requires : string list } | END | OTHER
fun readFile (lang, path)
    = let val ins = TextIO.openIn path
          fun parseLuaLine "--END\n" = END
            | parseLuaLine line = if String.isPrefix "--BEGIN " line then
                                      let val rest = Substring.extract (line, 8, NONE)
                                      in case Substring.fields (fn c => c = #":") rest of
                                             [p] => BEGIN { provides = List.map Substring.string (Substring.tokens Char.isSpace p), requires = [] }
                                           | [p, r] => BEGIN { provides = List.map Substring.string (Substring.tokens Char.isSpace p)
                                                             , requires = List.map Substring.string (Substring.tokens Char.isSpace r)
                                                             }
                                           | _ => raise Fail ("invalid initialization file: " ^ path)
                                      end
                                  else
                                      OTHER
          fun parseJsLine "//END\n" = END
            | parseJsLine line = if String.isPrefix "//BEGIN " line then
                                     let val rest = Substring.extract (line, 8, NONE)
                                     in case Substring.fields (fn c => c = #":") rest of
                                            [p] => BEGIN { provides = List.map Substring.string (Substring.tokens Char.isSpace p), requires = [] }
                                          | [p, r] => BEGIN { provides = List.map Substring.string (Substring.tokens Char.isSpace p)
                                                            , requires = List.map Substring.string (Substring.tokens Char.isSpace r)
                                                            }
                                          | _ => raise Fail ("invalid initialization file: " ^ path)
                                     end
                                 else
                                     OTHER
          val parseLine = case lang of
                              LUA => parseLuaLine
                            | JS => parseJsLine
          fun go chunks = case TextIO.inputLine ins of
                              SOME line => (case parseLine line of
                                                BEGIN { provides, requires } => goChunk (chunks, provides, requires, [])
                                              | OTHER => go chunks (* ignore *)
                                              | END => go chunks (* ignore *)
                                           )
                            | NONE => List.rev chunks
          and goChunk (chunks, provides, requires, acc)
              = case TextIO.inputLine ins of
                    SOME line => (case parseLine line of
                                      BEGIN { provides = provides', requires = requires' } =>
                                      goChunk ({ provides = provides, requires = requires, body = String.concat (List.rev acc) } :: chunks, provides', requires', [])
                                    | OTHER => goChunk (chunks, provides, requires, line :: acc)
                                    | END => go ({ provides = provides, requires = requires, body = String.concat (List.rev acc) } :: chunks)
                                 )
                  | NONE => go ({ provides = provides, requires = requires, body = String.concat (List.rev acc) } :: chunks)
      in go [] before TextIO.closeIn ins
      end
fun doEliminate (chunk as { provides, requires, body = _ }, (acc, used))
    = if List.exists (fn p => StringSet.member (used, p)) provides then
          (chunk :: acc, List.foldl StringSet.add' used requires)
      else
          (acc, used)
fun eliminateUnusedChunks (chunks, used) = let val usedSet = List.foldl StringSet.add' StringSet.empty used
                                           in #1 (List.foldr doEliminate ([], usedSet) chunks)
                                           end
fun output (outs, chunks) = let fun outputOne { provides = _, requires = _, body } = TextIO.output (outs, body)
                            in List.app outputOne chunks
                            end
end;
