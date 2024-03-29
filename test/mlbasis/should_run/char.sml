fun go i = if i > 127 then
               ()
           else
               let val c = Char.chr i
                   val s = Char.toString c
               in List.app (fn (name, pred) => print (name ^ " #\"" ^ s ^ "\" = " ^ Bool.toString (pred c) ^ "\n"))
                           [("isAscii", Char.isAscii)
                           ,("isAlpha", Char.isAlpha)
                           ,("isAlphaNum", Char.isAlphaNum)
                           ,("isCntrl", Char.isCntrl)
                           ,("isDigit", Char.isDigit)
                           ,("isGraph", Char.isGraph)
                           ,("isHexDigit", Char.isHexDigit)
                           ,("isLower", Char.isLower)
                           ,("isPrint", Char.isPrint)
                           ,("isSpace", Char.isSpace)
                           ,("isPunct", Char.isPunct)
                           ,("isUpper", Char.isUpper)
                           ]
                ; print ("toLower #\"" ^ s ^ "\" = #\"" ^ Char.toString (Char.toLower c) ^ "\"\n")
                ; print ("toUpper #\"" ^ s ^ "\" = #\"" ^ Char.toString (Char.toUpper c) ^ "\"\n")
                ; print ("toCString #\"" ^ s ^ "\" = #\"" ^ Char.toCString c ^ "\"\n")
                ; go (i + 1)
               end;
val _ = go 0;
List.app (fn s => case Char.fromString s of
                      NONE => print ("Char.fromString \"" ^ String.toString s ^ "\" = NONE\n")
                    | SOME c => print ("Char.fromString \"" ^ String.toString s ^ "\" = SOME #\"" ^ Char.toString c ^ "\"\n")
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
         ];
