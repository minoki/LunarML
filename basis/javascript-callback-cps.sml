structure JavaScript = struct
open JavaScript
fun callback (f : value vector -> General.unit) : value
    = function (fn args =>
                   ( LunarML.DelimCont.pushPrompt (LunarML.DelimCont.topLevel, fn () => f args)
                   ; JavaScript.undefined
                   )
               )
end;
