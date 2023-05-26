fun printOptionReal NONE = print "NONE\n"
  | printOptionReal (SOME x) = print ("SOME " ^ Real.toString x ^ "\n")
val () = List.app (fn s => (print ("Real.fromString \"" ^ String.toString s ^ "\" = "); printOptionReal (Real.fromString s)))
                  [ ""
                  , "0"
                  , "1.0"
                  , "inf"
                  , "+inf"
                  , " ~INF"
                  , "\t-Inf"
                  , "-NaN"
                  , "1z"
                  , "~.5e0"
                  , "~5e~1"
                  , "~5E-1"
                  , "+5E"
                  , "5E1000"
                  ];
