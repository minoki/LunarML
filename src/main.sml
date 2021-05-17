(*
 * Copyright (c) 2021 ARATA Mizuki
 * This file is part of LunarML.
 *)
(*
SML/NJ style main:
structure LunarML = struct
fun main (progName : string, args : string list) = (print "Hello world!\n" ; OS.Process.success)
end
*)
val progName = CommandLine.name ();
val progDir = OS.Path.dir progName;
val mlinit_lua = OS.Path.joinDirFile { dir = progDir, file = "mlinit.lua" }
val mlbasis_sml = OS.Path.joinDirFile { dir = progDir, file = "mlbasis.sml" }
val args = CommandLine.arguments ();
fun readFile filename = let val ins = TextIO.openIn filename (* may raise Io *)
                            val content = TextIO.inputAll ins
                            val () = TextIO.closeIn ins
                        in content
                        end;
case args of
    [] => (print ("Usage:\n" ^ progName ^ " file.sml\n"); OS.Process.exit OS.Process.failure)
  | [fileName] => (let val baseLen = String.size fileName - 4
                       val () = if baseLen < 0 then (print "Input filename must end with '.sml'\n"; OS.Process.exit OS.Process.failure) else ()
                       val ext = String.substring (fileName, baseLen, 4)
                       val () = if ext <> ".sml" then (print "Input filename must end with '.sml'\n"; OS.Process.exit OS.Process.failure) else ()
                       val base = String.substring (fileName, 0, baseLen)
                       val mlinit = readFile mlinit_lua
                       val mlbasis = readFile mlbasis_sml
                       val source = readFile fileName
                       val ctx = Driver.newContext()
                       val (env, basisdecs) = Driver.compile(ctx, Driver.initialEnv, mlbasis_sml, mlbasis)
                       val (env', programdecs) = Driver.compile(ctx, env, fileName, source)
                       val decs = Driver.wholeProgramOptimization (basisdecs @ programdecs)
                       val lua = CodeGenLua.doDecs { nextLuaId = ref 0 } CodeGenLua.initialEnv decs
                       val outs = TextIO.openOut (base ^ ".lua") (* may raise Io *)
                       val () = TextIO.output (outs, mlinit)
                       val () = TextIO.output (outs, lua)
                       val () = TextIO.closeOut outs
                   in ()
                   end handle Driver.Abort => OS.Process.exit OS.Process.failure
		  )
  | _ => (print "Too many arguments\n"; OS.Process.exit OS.Process.failure);
