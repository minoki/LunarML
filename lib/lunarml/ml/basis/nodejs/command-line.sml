structure CommandLine : sig
              val name : unit -> string
              val arguments : unit -> string list
          end = struct
local _esImport [pure] { argv } from "node:process";
in
fun name () = JavaScript.encodeUtf8 (JavaScript.unsafeFromValue (JavaScript.sub (argv, JavaScript.fromInt 1)))
fun arguments () = let val n = JavaScript.unsafeFromValue (JavaScript.field (argv, "length"))
                       fun go i = if i < n then
                                      let val x = JavaScript.unsafeFromValue (JavaScript.sub (argv, JavaScript.fromInt i)) : WideString.string
                                      in JavaScript.encodeUtf8 x :: go (i + 1)
                                      end
                                  else
                                      []
                   in go 2
                   end
end
end;
