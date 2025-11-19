fun go i = if i > 127 then
               ()
           else
               let val c = Char32.chr i
                   val s = Char32.toString c
               in List.app (fn (name, pred) => print (name ^ " #\"" ^ s ^ "\" = " ^ Bool.toString (pred c) ^ "\n"))
                           [("isAscii", Char32.isAscii)
                           ,("isAlpha", Char32.isAlpha)
                           ,("isAlphaNum", Char32.isAlphaNum)
                           ,("isCntrl", Char32.isCntrl)
                           ,("isDigit", Char32.isDigit)
                           ,("isGraph", Char32.isGraph)
                           ,("isHexDigit", Char32.isHexDigit)
                           ,("isLower", Char32.isLower)
                           ,("isPrint", Char32.isPrint)
                           ,("isSpace", Char32.isSpace)
                           ,("isPunct", Char32.isPunct)
                           ,("isUpper", Char32.isUpper)
                           ]
                ; print ("toLower #\"" ^ s ^ "\" = #\"" ^ Char32.toString (Char32.toLower c) ^ "\"\n")
                ; print ("toUpper #\"" ^ s ^ "\" = #\"" ^ Char32.toString (Char32.toUpper c) ^ "\"\n")
                ; print ("toCString #\"" ^ s ^ "\" = #\"" ^ Char32.toCString c ^ "\"\n")
                ; go (i + 1)
               end;
val _ = go 0;
List.app (fn s => case Char32.fromString s of
                      NONE => print ("Char32.fromString \"" ^ String.toString s ^ "\" = NONE\n")
                    | SOME c => print ("Char32.fromString \"" ^ String.toString s ^ "\" = SOME #\"" ^ Char32.toString c ^ "\"\n")
         )
         ["\\q"
         ,"a"
         ,""
         ,"\\  \\z"
         ,"\\  a"
         ,"\\a"
         ,"\\^H"
         ,"\\0"
         ,"\\123"
         ,"\\1234"
         ,"\\u00Ac"
         ,"\\uFEFF"
         ,"\\U0010FFFF"
         ];
