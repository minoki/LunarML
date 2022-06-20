signature BOOL = sig
    datatype bool = datatype bool
    val not : bool -> bool
    val toString : bool -> string
    val scan : (char, 'a) StringCvt.reader -> (bool, 'a) StringCvt.reader
    val fromString : string -> bool option
end

structure Bool :> BOOL = struct
fun scan getc strm = let val strm = StringCvt.skipWS getc strm
                     in case getc strm of
                            NONE => NONE
                          | SOME (c, strm') => if c = #"t" orelse c = #"T" then
                                                   case getc strm' of
                                                       NONE => NONE
                                                     | SOME (c, strm'') => if c = #"r" orelse c = #"R" then
                                                                               case getc strm'' of
                                                                                   NONE => NONE
                                                                                 | SOME (c, strm''') => if c = #"u" orelse c = #"U" then
                                                                                                            case getc strm''' of
                                                                                                                NONE => NONE
                                                                                                              | SOME (c, strm'''') => if c = #"e" orelse c = #"E" then
                                                                                                                                          SOME (true, strm'''')
                                                                                                                                      else
                                                                                                                                          NONE
                                                                                                        else
                                                                                                            NONE
                                                                           else
                                                                               NONE
                                               else if c = #"f" orelse c = #"F" then
                                                   case getc strm' of
                                                       NONE => NONE
                                                     | SOME (c, strm'') => if c = #"a" orelse c = #"A" then
                                                                               case getc strm'' of
                                                                                   NONE => NONE
                                                                                 | SOME (c, strm''') => if c = #"l" orelse c = #"L" then
                                                                                                            case getc strm''' of
                                                                                                                NONE => NONE
                                                                                                              | SOME (c, strm'''') => if c = #"s" orelse c = #"S" then
                                                                                                                                          case getc strm'''' of
                                                                                                                                              NONE => NONE
                                                                                                                                            | SOME (c, strm''''') => if c = #"e" orelse c = #"E" then
                                                                                                                                                                         SOME (false, strm''''')
                                                                                                                                                                     else
                                                                                                                                                                         NONE
                                                                                                                                      else
                                                                                                                                          NONE
                                                                                                        else
                                                                                                            NONE
                                                                           else
                                                                               NONE
                                               else
                                                   NONE
                     end
fun fromString s = StringCvt.scanString scan s
open Bool
end;
