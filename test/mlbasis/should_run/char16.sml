fun go i = if i > 127 then
               ()
           else
               let val c = Char16.chr i
                   val s = Char16.toString c
               in List.app (fn (name, pred) => print (name ^ " #\"" ^ s ^ "\" = " ^ Bool.toString (pred c) ^ "\n"))
                           [("isAscii", Char16.isAscii)
                           ,("isAlpha", Char16.isAlpha)
                           ,("isAlphaNum", Char16.isAlphaNum)
                           ,("isCntrl", Char16.isCntrl)
                           ,("isDigit", Char16.isDigit)
                           ,("isGraph", Char16.isGraph)
                           ,("isHexDigit", Char16.isHexDigit)
                           ,("isLower", Char16.isLower)
                           ,("isPrint", Char16.isPrint)
                           ,("isSpace", Char16.isSpace)
                           ,("isPunct", Char16.isPunct)
                           ,("isUpper", Char16.isUpper)
                           ]
                ; print ("toLower #\"" ^ s ^ "\" = #\"" ^ Char16.toString (Char16.toLower c) ^ "\"\n")
                ; print ("toUpper #\"" ^ s ^ "\" = #\"" ^ Char16.toString (Char16.toUpper c) ^ "\"\n")
                ; print ("toCString #\"" ^ s ^ "\" = #\"" ^ Char16.toCString c ^ "\"\n")
                ; go (i + 1)
               end;
val _ = go 0;
List.app (fn s => case Char16.fromString s of
                      NONE => print ("Char16.fromString \"" ^ String.toString s ^ "\" = NONE\n")
                    | SOME c => print ("Char16.fromString \"" ^ String.toString s ^ "\" = SOME #\"" ^ Char16.toString c ^ "\"\n")
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
