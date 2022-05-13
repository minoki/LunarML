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
                                                \  -o,--output=file.ext   File name to output.\n\
                                                \  -mexe                  Produce a standalone script.\n\
                                                \  -mlib                  Produce a Lua module.\n\
                                                \  --js                   Produce JavaScript code.\n\
                                                \  --js-cps               Produce JavaScript code (CPS mode).\n\
                                                \  -h,--help              Show this message.\n\
                                                \  -v,--version           Show version information.\n\
                                                \  --dump                 Dump intermediate code.\n\
                                                \  --dump-final           Dump final intermediate code.\n\
                                                \  -O,--optimize          Try to optimize hard.\n\
                                                \")
datatype DumpMode = NO_DUMP | DUMP_INITIAL | DUMP_FINAL
datatype Backend = BACKEND_LUA | BACKEND_JS | BACKEND_JS_CPS
type Options = { output : string option
               , outputMode : Driver.OutputMode option
               , dump : DumpMode
               , optimizationLevel : int
               , backend : Backend
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
                                             NONE => parseArgs { output = SOME outname, outputMode = #outputMode opts, dump = #dump opts, optimizationLevel = #optimizationLevel opts, backend = #backend opts } args
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
                 NONE => parseArgs { output = #output opts, outputMode = SOME Driver.ExecutableMode, dump = #dump opts, optimizationLevel = #optimizationLevel opts, backend = #backend opts } args
               | SOME Driver.ExecutableMode => parseArgs opts args
               | SOME _ => showMessageAndFail "-mexe or -mlib was given multiple times.\n"
         else if arg = "-mlib" then
             case #outputMode opts of
                 NONE => parseArgs { output = #output opts, outputMode = SOME Driver.LibraryMode, dump = #dump opts, optimizationLevel = #optimizationLevel opts, backend = #backend opts } args
               | SOME Driver.LibraryMode => parseArgs opts args
               | SOME _ => showMessageAndFail "-mexe or -mlib was given multiple times.\n"
         else if arg = "--js" then
             parseArgs { output = #output opts, outputMode = #outputMode opts, dump = #dump opts, optimizationLevel = #optimizationLevel opts, backend = BACKEND_JS } args
         else if arg = "--js-cps" then
             parseArgs { output = #output opts, outputMode = #outputMode opts, dump = #dump opts, optimizationLevel = #optimizationLevel opts, backend = BACKEND_JS_CPS } args
         else if arg = "-h" orelse arg = "--help" then
             ( showHelp (); OS.Process.exit OS.Process.success )
         else if arg = "-v" orelse arg = "--version" then
             ( showVersion (); OS.Process.exit OS.Process.success )
         else if arg = "--" then
             handleInputFile opts args
         else if arg = "--dump" then
             parseArgs { output = #output opts, outputMode = #outputMode opts, dump = DUMP_INITIAL, optimizationLevel = #optimizationLevel opts, backend = #backend opts } args
         else if arg = "--dump-final" then
             parseArgs { output = #output opts, outputMode = #outputMode opts, dump = DUMP_FINAL, optimizationLevel = #optimizationLevel opts, backend = #backend opts } args
         else if arg = "--optimize" orelse arg = "-O" then
             parseArgs { output = #output opts, outputMode = #outputMode opts, dump = #dump opts, optimizationLevel = #optimizationLevel opts + 1, backend = #backend opts } args
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
and getTargetInfo opts = (case #backend opts of
                              BACKEND_LUA => { wideChar = TargetInfo.CHAR8
                                             , nativeString = TargetInfo.NARROW_STRING
                                             }
                            | BACKEND_JS => { wideChar = TargetInfo.CHAR16
                                            , nativeString = TargetInfo.WIDE_STRING
                                            }
                            | BACKEND_JS_CPS => { wideChar = TargetInfo.CHAR16
                                                , nativeString = TargetInfo.WIDE_STRING
                                                }
                         )
and optimize ctx fdecs 0 = fdecs
  | optimize ctx fdecs n = let val ctx' = { nextVId = #nextVId (#toFContext (#driverContext ctx))
                                          , nextTyVar = #nextTyVar (#toFContext (#driverContext ctx))
                                          , targetInfo = #targetInfo ctx
                                          }
                           in optimize ctx (#2 (FTransform.doDecs ctx' FTransform.initialEnv fdecs)) (n - 1)
                           end
and doCompile opts fileName
    = let val progDir = OS.Path.dir progName
          val pathMap = List.foldl MLBSyntax.StringMap.insert' MLBSyntax.StringMap.empty
                                   [("SML_LIB", progDir)
                                   ,("TARGET_LANG", case #backend opts of BACKEND_LUA => "lua" | BACKEND_JS => "js" | BACKEND_JS_CPS => "js-cps")
                                   ]
          val ctx = { driverContext = Driver.newContext ()
                    , baseDir = OS.FileSys.getDir ()
                    , pathMap = pathMap
                    , targetInfo = getTargetInfo opts
                    }
          val mlbdecs = [MLBSyntax.PathDec "$(SML_LIB)/basis/basis.mlb"
                        ,MLBSyntax.PathDec fileName
                        ]
          val (env, { tynameset, toFEnv, fdecs, cache }) = MLBEval.doDecs ctx LanguageOptions.default MLBEval.emptyEnv mlbdecs MLBEval.initialCode
          val fdecs = case Option.getOpt (#outputMode opts, Driver.ExecutableMode) of
                          Driver.ExecutableMode => fdecs
                        | Driver.LibraryMode => ToFSyntax.addExport (#toFContext (#driverContext ctx), #typing env, fdecs)
          val () = if #dump opts = DUMP_INITIAL then
                       print (Printer.build (FPrinter.doDecs fdecs) ^ "\n")
                   else
                       ()
          val fdecs = optimize ctx fdecs (2 * (#optimizationLevel opts + 1))
          val fdecs = Driver.wholeProgramOptimization fdecs
          val () = if #dump opts = DUMP_FINAL then
                       print (Printer.build (FPrinter.doDecs fdecs) ^ "\n")
                   else
                       ()
      in emit opts fileName (#nextVId (#toFContext (#driverContext ctx))) fdecs
      end handle Driver.Abort => OS.Process.exit OS.Process.failure
               | DesugarPatternMatches.DesugarError ([], message) =>
                 ( print ("internal error: " ^ message ^ "\n")
                 ; OS.Process.exit OS.Process.failure
                 )
               | DesugarPatternMatches.DesugarError (spans as ({start=p1 as {file=f1,line=l1,column=c1},end_=p2 as {file=f2,line=l2,column=c2}} :: _), message) =>
                 ( if f1 = f2 then
                       if p1 = p2 then
                           print (f1 ^ ":" ^ Int.toString l1 ^ ":" ^ Int.toString c1 ^ ": " ^ message ^ "\n")
                       else
                           print (f1 ^ ":" ^ Int.toString l1 ^ ":" ^ Int.toString c1 ^ "-" ^ Int.toString l2 ^ ":" ^ Int.toString c2 ^ ": " ^ message ^ "\n")
                   else
                       print (f1 ^ ":" ^ Int.toString l1 ^ ":" ^ Int.toString c1 ^ "-" ^ f2 ^ ":" ^ Int.toString l2 ^ ":" ^ Int.toString c2 ^ ": " ^ message ^ "\n")
                 ; OS.Process.exit OS.Process.failure
                 )
               | CodeGenLua.CodeGenError message => ( print (message ^ "\n") ; OS.Process.exit OS.Process.failure )
               | CodeGenJs.CodeGenError message => ( print (message ^ "\n") ; OS.Process.exit OS.Process.failure )
               | CodeGenJsCps.CodeGenError message => ( print (message ^ "\n") ; OS.Process.exit OS.Process.failure )
and doMLB opts mlbfilename
    = let val progDir = OS.Path.dir progName
          val pathMap = List.foldl MLBSyntax.StringMap.insert' MLBSyntax.StringMap.empty
                                   [("SML_LIB", OS.Path.mkAbsolute { path = progDir, relativeTo = OS.FileSys.getDir () })
                                   ,("TARGET_LANG", case #backend opts of BACKEND_LUA => "lua" | BACKEND_JS => "js" | BACKEND_JS_CPS => "js-cps")
                                   ]
          val ctx = { driverContext = Driver.newContext ()
                    , baseDir = OS.FileSys.getDir ()
                    , pathMap = pathMap
                    , targetInfo = getTargetInfo opts
                    }
          val (env, { tynameset, toFEnv, fdecs, cache }) = MLBEval.doMlbSource ctx MLBEval.emptyEnv mlbfilename MLBEval.initialCode
          val fdecs = case Option.getOpt (#outputMode opts, Driver.ExecutableMode) of
                          Driver.ExecutableMode => fdecs
                        | Driver.LibraryMode => ToFSyntax.addExport (#toFContext (#driverContext ctx), #typing env, fdecs)
          val () = if #dump opts = DUMP_INITIAL then
                       print (Printer.build (FPrinter.doDecs fdecs) ^ "\n")
                   else
                       ()
          val fdecs = optimize ctx fdecs (2 * (#optimizationLevel opts + 1))
          val fdecs = Driver.wholeProgramOptimization fdecs
          val () = if #dump opts = DUMP_FINAL then
                       print (Printer.build (FPrinter.doDecs fdecs) ^ "\n")
                   else
                       ()
      in emit opts mlbfilename (#nextVId (#toFContext (#driverContext ctx))) fdecs
      end handle Driver.Abort => OS.Process.exit OS.Process.failure
                | DesugarPatternMatches.DesugarError ([], message) =>
                  ( print ("internal error: " ^ message ^ "\n")
                  ; OS.Process.exit OS.Process.failure
                  )
                | DesugarPatternMatches.DesugarError (spans as ({start=p1 as {file=f1,line=l1,column=c1},end_=p2 as {file=f2,line=l2,column=c2}} :: _), message) =>
                  ( if f1 = f2 then
                        if p1 = p2 then
                            print (f1 ^ ":" ^ Int.toString l1 ^ ":" ^ Int.toString c1 ^ ": " ^ message ^ "\n")
                        else
                            print (f1 ^ ":" ^ Int.toString l1 ^ ":" ^ Int.toString c1 ^ "-" ^ Int.toString l2 ^ ":" ^ Int.toString c2 ^ ": " ^ message ^ "\n")
                    else
                        print (f1 ^ ":" ^ Int.toString l1 ^ ":" ^ Int.toString c1 ^ "-" ^ f2 ^ ":" ^ Int.toString l2 ^ ":" ^ Int.toString c2 ^ ": " ^ message ^ "\n")
                  ; OS.Process.exit OS.Process.failure
                  )
               | CodeGenLua.CodeGenError message => ( print (message ^ "\n") ; OS.Process.exit OS.Process.failure )
               | CodeGenJs.CodeGenError message => ( print (message ^ "\n") ; OS.Process.exit OS.Process.failure )
               | CodeGenJsCps.CodeGenError message => ( print (message ^ "\n") ; OS.Process.exit OS.Process.failure )
and emit (opts as { backend = BACKEND_LUA, ... }) fileName nextId decs
    = let val progDir = OS.Path.dir progName
          val base = OS.Path.base fileName
          val mlinit_lua = OS.Path.joinDirFile { dir = progDir, file = "mlinit.lua" }
          val mlinit = readFile mlinit_lua
          val luactx = { nextLuaId = ref 0 }
          val lua = CodeGenLua.doProgram luactx CodeGenLua.initialEnv decs
          val lua = LuaTransform.doBlock { nextId = nextId } LuaTransform.initialEnv lua
          val lua = LuaWriter.doChunk lua
          val outs = TextIO.openOut (Option.getOpt (#output opts, base ^ ".lua")) (* may raise Io *)
          val () = TextIO.output (outs, mlinit)
          val () = TextIO.output (outs, lua)
          val () = TextIO.closeOut outs
      in ()
      end
  | emit (opts as { backend = BACKEND_JS, ... }) fileName nextId decs
    = let val progDir = OS.Path.dir progName
          val base = OS.Path.base fileName
          val mlinit_js = OS.Path.joinDirFile { dir = progDir, file = "mlinit.js" }
          val mlinit = readFile mlinit_js
          val jsctx = { nextJsId = ref 0 }
          val js = CodeGenJs.doProgram jsctx CodeGenJs.initialEnv decs
          val js = JsWriter.doProgram js
          val outs = TextIO.openOut (Option.getOpt (#output opts, base ^ ".js")) (* may raise Io *)
          val () = TextIO.output (outs, mlinit)
          val () = TextIO.output (outs, js)
          val () = TextIO.closeOut outs
      in ()
      end
  | emit (opts as { backend = BACKEND_JS_CPS, ... }) fileName nextId decs
    = let val cont = let val n = !nextId
                         val _ = nextId := n + 1
                     in TypedSyntax.MkVId ("cont", n)
                     end
          val exnCont = let val n = !nextId
                            val _ = nextId := n + 1
                        in TypedSyntax.MkVId ("exh", n)
                        end
          val cexp = CpsTransform.transformDecs { nextVId = nextId } decs exnCont cont
          val progDir = OS.Path.dir progName
          val base = OS.Path.base fileName
          val mlinit_js = OS.Path.joinDirFile { dir = progDir, file = "mlinit-cps.js" }
          val mlinit = readFile mlinit_js
          val jsctx = { nextJsId = ref 0 }
          val js = CodeGenJsCps.doProgram jsctx cont exnCont cexp
          val js = JsWriter.doProgram js
          val outs = TextIO.openOut (Option.getOpt (#output opts, base ^ ".js")) (* may raise Io *)
          val () = TextIO.output (outs, mlinit)
          val () = TextIO.output (outs, js)
          val () = TextIO.closeOut outs
      in ()
      end
val _ = parseArgs { output = NONE, outputMode = NONE, dump = NO_DUMP, optimizationLevel = 0, backend = BACKEND_LUA } (CommandLine.arguments ());
