structure export = struct
val main = JavaScript.callback (fn jsargs =>
                                   let val progName = JavaScript.encodeUtf8 (JavaScript.unsafeFromValue (Vector.sub (jsargs, 0)) : WideString.string)
                                       val args = Vector.map JavaScript.encodeUtf8 (JavaScript.unsafeFromValue (Vector.sub (jsargs, 1)) : WideString.string vector)
                                   in Main.main (progName, Vector.foldr (op ::) [] args)
                                   end
                               )
end;
