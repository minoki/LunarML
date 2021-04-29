(*
SML/NJ style main:
structure LunarML = struct
fun main (progName : string, args : string list) = (print "Hello world!\n" ; OS.Process.success)
end
*)
val progName = CommandLine.name ();
val progDir = OS.Path.dir progName;
val mlinit_lua = OS.Path.joinDirFile { dir = progDir, file = "mlinit.lua" }
val args = CommandLine.arguments ();
case args of
    [] => (print ("Usage:\n" ^ progName ^ " file.sml\n"); OS.Process.exit OS.Process.failure)
  | [fileName] => (let val baseLen = String.size fileName - 4
                       val () = if baseLen < 0 then (print "Input filename must end with '.sml'\n"; OS.Process.exit OS.Process.failure) else ()
                       val ext = String.substring (fileName, baseLen, 4)
                       val () = if ext <> ".sml" then (print "Input filename must end with '.sml'\n"; OS.Process.exit OS.Process.failure) else ()
                       val base = String.substring (fileName, 0, baseLen)
                       val mlinit = let val ins = TextIO.openIn mlinit_lua
                                        val source = TextIO.inputAll ins
                                        val () = TextIO.closeIn ins
                                    in source
                                    end
                       val ins = TextIO.openIn fileName (* may raise Io *)
                       val source = TextIO.inputAll ins
                       val () = TextIO.closeIn ins
                       val (_, _, _, _, _, _, lua) = Driver.compile(fileName, source)
                       val outs = TextIO.openOut (base ^ ".lua") (* may raise Io *)
		       val () = TextIO.output (outs, mlinit)
                       val () = TextIO.output (outs, lua)
                       val () = TextIO.closeOut outs
                   in ()
                   end handle Driver.Abort => OS.Process.exit OS.Process.failure
		  )
  | _ => (print "Too many arguments\n"; OS.Process.exit OS.Process.failure);
