(*
SML/NJ style main:
structure DamepoML = struct
fun main (progName : string, args : string list) = (print "Hello world!\n" ; OS.Process.success)
end
*)
val progName = CommandLine.name ();
val args = CommandLine.arguments ();
case args of
    [] => (print ("Usage:\n" ^ progName ^ " file.sml\n"); OS.Process.exit OS.Process.failure)
  | [fileName] => let val baseLen = String.size fileName - 4
                      val () = if baseLen < 0 then (print "Input filename must end with '.sml'\n"; OS.Process.exit OS.Process.failure) else ()
                      val ext = String.substring (fileName, baseLen, 4)
                      val () = if ext <> ".sml" then (print "Input filename must end with '.sml'\n"; OS.Process.exit OS.Process.failure) else ()
                      val base = String.substring (fileName, 0, baseLen)
                      val ins = TextIO.openIn fileName (* may raise Io *)
                      val source = TextIO.inputAll ins
                      val () = TextIO.closeIn ins
                      val (_, _, _, _, _, _, lua) = Driver.compile source
                      val outs = TextIO.openOut (base ^ ".lua") (* may raise Io *)
                      val () = TextIO.output (outs, lua)
                      val () = TextIO.closeOut outs
                  in ()
                  end
  | _ => (print "Too many arguments\n"; OS.Process.exit OS.Process.failure);