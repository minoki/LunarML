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
fun showVersion () = TextIO.output (TextIO.stdErr, "LunarML <unreleased>\n")
fun showHelp () = TextIO.output (TextIO.stdErr, "Usage:\n\
                                                \  " ^ progName ^ " [options] file.sml\n\
                                                \Options:\n\
                                                \  -o,--output=file.lua   File name to output.\n\
                                                \  -mexe                  Produce a standalone script.\n\
                                                \  -mlib                  Produce a Lua module.\n\
                                                \  -h,--help              Show this message.\n\
                                                \  -v,--version           Show version information.\n\
                                                \")
type Options = { output : string option
               , outputMode : Driver.OutputMode option
               }
fun showMessageAndFail message = ( TextIO.output (TextIO.stdErr, message)
                                 ; OS.Process.exit OS.Process.failure
                                 )
fun readFile filename = let val ins = TextIO.openIn filename (* may raise Io *)
                            val content = TextIO.inputAll ins
                            val () = TextIO.closeIn ins
                        in content
                        end;
fun parseArgs opts [] = showMessageAndFail "No input given. Try --help.\n"
  | parseArgs opts (arg :: args)
    = let fun doOutput (outname, args) = case #output opts of
                                             NONE => parseArgs { output = SOME outname, outputMode = #outputMode opts } args
                                           | SOME _ => showMessageAndFail "--output was given multiple times.\n"
      in if arg = "-o" orelse arg = "--output" then
             case args of
                 outname :: args => doOutput (outname, args)
               | [] => showMessageAndFail ("Argument for " ^ arg ^ " is missing.\n")
         else if String.isPrefix "-o" arg then
             doOutput (String.extract (arg, 2, NONE), args)
         else if String.isPrefix "--output=" arg then
             doOutput (String.extract (arg, 9, NONE), args)
         else if arg = "-mexe" then
             case #outputMode opts of
                 NONE => parseArgs { output = #output opts, outputMode = SOME Driver.ExecutableMode } args
               | SOME Driver.ExecutableMode => parseArgs opts args
               | SOME _ => showMessageAndFail "-mexe or -mlib was given multiple times.\n"
         else if arg = "-mlib" then
             case #outputMode opts of
                 NONE => parseArgs { output = #output opts, outputMode = SOME Driver.LibraryMode } args
               | SOME Driver.LibraryMode => parseArgs opts args
               | SOME _ => showMessageAndFail "-mexe or -mlib was given multiple times.\n"
         else if arg = "-h" orelse arg = "--help" then
             ( showHelp (); OS.Process.exit OS.Process.success )
         else if arg = "-v" orelse arg = "--version" then
             ( showVersion (); OS.Process.exit OS.Process.success )
         else if arg = "--" then
             handleInputFile opts args
         else if String.isPrefix "-" arg then
             showMessageAndFail ("Unrecognized option: " ^ arg ^ ".\n")
         else
             handleInputFile opts (arg :: args)
      end
and handleInputFile opts [file] = if String.isSuffix ".sml" file then
                                      doCompile opts file
                                  else
                                      showMessageAndFail "Input filename must end with '.sml'\n"
  | handleInputFile opts [] = showMessageAndFail "No input given.\n"
  | handleInputFile opts _ = showMessageAndFail "Multiple input is not supported.\n"
and doCompile opts fileName = let val progDir = OS.Path.dir progName;
                                  val mlinit_lua = OS.Path.joinDirFile { dir = progDir, file = "mlinit.lua" }
                                  val mlbasis_sml = OS.Path.joinDirFile { dir = progDir, file = "mlbasis.sml" }
                                  val baseLen = String.size fileName - 4
                                  val base = String.substring (fileName, 0, baseLen)
                                  val mlinit = readFile mlinit_lua
                                  val mlbasis = readFile mlbasis_sml
                                  val source = readFile fileName
                                  val ctx = Driver.newContext()
                                  val (env, basisdecs) = Driver.compile(ctx, Driver.ExecutableMode, Driver.initialEnv, mlbasis_sml, mlbasis)
                                  val (env', programdecs) = Driver.compile(ctx, Option.getOpt (#outputMode opts, Driver.ExecutableMode), env, fileName, source)
                                  val decs = Driver.wholeProgramOptimization (basisdecs @ programdecs)
                                  val luactx = { nextLuaId = ref 0 }
                                  val lua = CodeGenLua.doProgram luactx CodeGenLua.initialEnv decs
                                  val outs = TextIO.openOut (Option.getOpt (#output opts, base ^ ".lua")) (* may raise Io *)
                                  val () = TextIO.output (outs, mlinit)
                                  val () = TextIO.output (outs, lua)
                                  val () = TextIO.closeOut outs
                              in ()
                              end handle Driver.Abort => OS.Process.exit OS.Process.failure;
parseArgs { output = NONE, outputMode = NONE } (CommandLine.arguments ());
