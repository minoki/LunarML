structure ScanNumUtils = struct
fun skipInitialWhitespace (getc, strm) = case getc strm of
                                             NONE => strm
                                           | SOME (c, strm') => if Char.isSpace c then
                                                                    skipInitialWhitespace (getc, strm')
                                                                else
                                                                    strm
fun isBinDigit c = c = #"0" orelse c = #"1"
fun isOctDigit c = #"0" <= c andalso c <= #"7"
fun digitToInt c = if #"0" <= c andalso c <= #"9" then
                       Char.ord c - Char.ord #"0"
                   else if #"a" <= c andalso c <= #"f" then
                       Char.ord c - Char.ord #"a" + 10
                   else
                       Char.ord c - Char.ord #"A" + 10
(* scanSign: true if negative *)
fun scanSign (getc, strm) = case getc strm of
                                SOME (#"+", strm) => (false, strm)
                              | SOME (#"~", strm) => (true, strm)
                              | SOME (#"-", strm) => (true, strm)
                              | _ => (false, strm)
fun skip0w (isDigit, getc, strm) = case getc strm of
                                       NONE => strm
                                     | SOME (#"0", strm') => (case getc strm' of
                                                                  SOME (#"w", strm'') => (case getc strm'' of
                                                                                              SOME (c, _) => if isDigit c then
                                                                                                                 strm''
                                                                                                             else
                                                                                                                 strm
                                                                                            | NONE => strm
                                                                                         )
                                                                | _ => strm
                                                             )
                                     | _ => strm
fun skip0wx (getc, strm) = case getc strm of
                               NONE => strm
                             | SOME (#"0", strm') =>
                               (case getc strm' of
                                    SOME (#"w", strm'') =>
                                    (case getc strm'' of
                                         SOME (x, strm''') =>
                                         if x = #"x" orelse x = #"X" then
                                             case getc strm''' of
                                                 SOME (c, _) => if Char.isHexDigit c then
                                                                    strm'''
                                                                else
                                                                    strm
                                               | NONE => strm
                                         else
                                             strm
                                    )
                                  | SOME (#"x", strm'') =>
                                    (case getc strm'' of
                                         SOME (c, _) => if Char.isHexDigit c then
                                                            strm''
                                                        else
                                                            strm
                                       | NONE => strm
                                    )
                                  | SOME (#"X", strm'') =>
                                    (case getc strm'' of
                                         SOME (c, _) => if Char.isHexDigit c then
                                                            strm''
                                                        else
                                                            strm
                                       | NONE => strm
                                    )
                                  | _ => strm
                               )
                             | _ => strm
end;
