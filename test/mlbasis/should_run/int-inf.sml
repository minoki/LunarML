datatype 'a result = VALUE of 'a
                   | OVERFLOW
                   | DOMAIN
fun resultToString (VALUE x) = Int.toString x
  | resultToString OVERFLOW = "Overflow"
  | resultToString DOMAIN = "DOMAIN"
val () = List.app (fn (s, x, expected) =>
                      let val actual = VALUE (IntInf.log2 x)
                                       handle Overflow => OVERFLOW
                                            | Domain => DOMAIN
                      in if expected = actual then
                             print ("log2 " ^ s ^ ": OK\n")
                         else
                             print ("log2 " ^ s ^ ": mismatch (expected " ^ resultToString expected ^ ", but got " ^ resultToString actual ^ ")\n")
                      end
                  )
                  [("0", 0, DOMAIN)
                  ,("~1", ~1, DOMAIN)
                  ,("1", 1, VALUE 0)
                  ,("2", 2, VALUE 1)
                  ,("3", 3, VALUE 1)
                  ,("4", 4, VALUE 2)
                  ,("5", 5, VALUE 2)
                  ,("6", 6, VALUE 2)
                  ,("7", 7, VALUE 2)
                  ,("8", 8, VALUE 3)
                  ,("9", 9, VALUE 3)
                  ,("10", 10, VALUE 3)
                  ,("2^16-1", IntInf.<< (1, 0w16) - 1, VALUE 15)
                  ,("2^16", IntInf.<< (1, 0w16), VALUE 16)
                  ,("2^16+1", IntInf.<< (1, 0w16) + 1, VALUE 16)
                  ,("2^64-1", IntInf.<< (1, 0w64) - 1, VALUE 63)
                  ,("2^64", IntInf.<< (1, 0w64), VALUE 64)
                  ,("2^64+1", IntInf.<< (1, 0w64) + 1, VALUE 64)
                  ,("2^100-1", IntInf.<< (1, 0w100) - 1, VALUE 99)
                  ,("2^100", IntInf.<< (1, 0w100), VALUE 100)
                  ,("2^100+1", IntInf.<< (1, 0w100) + 1, VALUE 100)
                  ,("2^1000-1", IntInf.<< (1, 0w1000) - 1, VALUE 999)
                  ,("2^1000", IntInf.<< (1, 0w1000), VALUE 1000)
                  ,("2^1000+1", IntInf.<< (1, 0w1000) + 1, VALUE 1000)
                  ];
