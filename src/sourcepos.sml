structure SourcePos = struct
type pos = { file : string
           , line : int (* 1-based *)
           , column : int (* 1-based *)
           }
type span = { start : pos
            , end_ : pos (* inclusive *)
            }
val nullPos : pos = { file = "", line = 0, column = 0 }
val nullSpan : span = { start = nullPos, end_ = nullPos }
fun compare(p1 : pos, p2 : pos) = if #file p1 = #file p2 then
                                      case Int.compare (#line p1, #line p2) of
                                          EQUAL => Int.compare (#column p1, #column p2)
                                        | x => x
                                  else
                                      LESS (* fallback *)
fun first(p1 : pos, p2 : pos) = case compare (p1, p2) of
                                    LESS => p1
                                  | EQUAL => p1
                                  | GREATER => p2
fun last(p1 : pos, p2 : pos) = case compare (p1, p2) of
                                   LESS => p2
                                 | EQUAL => p2
                                 | GREATER => p1
fun mergeSpan(s1 : span, s2 : span) = { start = first(#start s1, #start s2)
                                      , end_ = last(#end_ s1, #end_ s2)
                                      }
end
