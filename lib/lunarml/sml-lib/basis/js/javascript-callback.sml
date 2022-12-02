structure JavaScript = struct
open JavaScript
fun callback (f : value vector -> General.unit) : value
    = function (fn args =>
                   ( f args
                   ; JavaScript.undefined
                   )
               )
end;
