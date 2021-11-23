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
structure M = MLBSyntax;
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
                                                \  --dump                 Dump intermediate code.\n\
                                                \")
type Options = { output : string option
               , outputMode : Driver.OutputMode option
               , dump : bool
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
                                             NONE => parseArgs { output = SOME outname, outputMode = #outputMode opts, dump = #dump opts } args
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
                 NONE => parseArgs { output = #output opts, outputMode = SOME Driver.ExecutableMode, dump = #dump opts } args
               | SOME Driver.ExecutableMode => parseArgs opts args
               | SOME _ => showMessageAndFail "-mexe or -mlib was given multiple times.\n"
         else if arg = "-mlib" then
             case #outputMode opts of
                 NONE => parseArgs { output = #output opts, outputMode = SOME Driver.LibraryMode, dump = #dump opts } args
               | SOME Driver.LibraryMode => parseArgs opts args
               | SOME _ => showMessageAndFail "-mexe or -mlib was given multiple times.\n"
         else if arg = "-h" orelse arg = "--help" then
             ( showHelp (); OS.Process.exit OS.Process.success )
         else if arg = "-v" orelse arg = "--version" then
             ( showVersion (); OS.Process.exit OS.Process.success )
         else if arg = "--" then
             handleInputFile opts args
         else if arg = "--dump" then
             parseArgs { output = #output opts, outputMode = #outputMode opts, dump = true } args
         else if String.isPrefix "-" arg then
             showMessageAndFail ("Unrecognized option: " ^ arg ^ ".\n")
         else
             handleInputFile opts (arg :: args)
      end
and handleInputFile opts [file] = if String.isSuffix ".sml" file then
                                      doCompile opts file
                                  else if String.isSuffix ".mlb" file then
                                      doMLB opts file
                                  else
                                      showMessageAndFail "Input filename must end with '.sml'\n"
  | handleInputFile opts [] = showMessageAndFail "No input given.\n"
  | handleInputFile opts _ = showMessageAndFail "Multiple input is not supported.\n"
and doCompile opts fileName
    = let val progDir = OS.Path.dir progName
          val pathMap = List.foldl MLBSyntax.StringMap.insert' MLBSyntax.StringMap.empty
                                   [("SML_LIB", progDir)]
          val ctx = { driverContext = Driver.newContext ()
                    , baseDir = OS.FileSys.getDir ()
                    , pathMap = pathMap
                    }
          val mlbdecs = [MLBSyntax.PathDec "$(SML_LIB)/basis/basis.mlb"
                        ,MLBSyntax.PathDec fileName
                        ]
          val (env, { tynameset, toFEnv, fdecs, cache }) = MLBEval.doDecs ctx MLBEval.emptyEnv mlbdecs MLBEval.initialCode
          val fdecs = case Option.getOpt (#outputMode opts, Driver.ExecutableMode) of
                          Driver.ExecutableMode => fdecs
                        | Driver.LibraryMode => ToFSyntax.addExport (#toFContext (#driverContext ctx), #typing env, fdecs)
          val () = if #dump opts then
                       print (Printer.build (FPrinter.doDecs fdecs) ^ "\n")
                   else
                       ()
          val (_, fdecs) = FTransform.doDecs (#toFContext (#driverContext ctx)) FTransform.initialEnv fdecs
          val fdecs = Driver.wholeProgramOptimization fdecs
      in emitLua opts fileName fdecs
      end handle Driver.Abort => OS.Process.exit OS.Process.failure
               | CodeGenLua.CodeGenError message => ( print (message ^ "\n") ; OS.Process.exit OS.Process.failure )
and doMLB opts mlbfilename
    = let val progDir = OS.Path.dir progName
          val pathMap = List.foldl MLBSyntax.StringMap.insert' MLBSyntax.StringMap.empty
                                   [("SML_LIB", OS.Path.mkAbsolute { path = progDir, relativeTo = OS.FileSys.getDir () })]
          val ctx = { driverContext = Driver.newContext ()
                    , baseDir = OS.FileSys.getDir ()
                    , pathMap = pathMap
                    }
          val (env, { tynameset, toFEnv, fdecs, cache }) = MLBEval.doMlbSource ctx MLBEval.emptyEnv mlbfilename MLBEval.initialCode
          val fdecs = case Option.getOpt (#outputMode opts, Driver.ExecutableMode) of
                          Driver.ExecutableMode => fdecs
                        | Driver.LibraryMode => ToFSyntax.addExport (#toFContext (#driverContext ctx), #typing env, fdecs)
          val () = if #dump opts then
                       print (Printer.build (FPrinter.doDecs fdecs) ^ "\n")
                   else
                       ()
          val (_, fdecs) = FTransform.doDecs (#toFContext (#driverContext ctx)) FTransform.initialEnv fdecs
          val fdecs = Driver.wholeProgramOptimization fdecs
      in emitLua opts mlbfilename fdecs
      end handle Driver.Abort => OS.Process.exit OS.Process.failure
               | CodeGenLua.CodeGenError message => ( print (message ^ "\n") ; OS.Process.exit OS.Process.failure )
and emitLua opts fileName decs
    = let val progDir = OS.Path.dir progName
          val base = OS.Path.base fileName
          val mlinit_lua = OS.Path.joinDirFile { dir = progDir, file = "mlinit.lua" }
          val mlinit = readFile mlinit_lua
          val luactx = { nextLuaId = ref 0 }
          val lua = CodeGenLua.doProgram luactx CodeGenLua.initialEnv decs
          val outs = TextIO.openOut (Option.getOpt (#output opts, base ^ ".lua")) (* may raise Io *)
          val () = TextIO.output (outs, mlinit)
          val () = TextIO.output (outs, lua)
          val () = TextIO.closeOut outs
      in ()
      end
val _ = parseArgs { output = NONE, outputMode = NONE, dump = false } (CommandLine.arguments ());
