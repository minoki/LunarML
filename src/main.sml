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
structure Main = struct
structure M = MLBSyntax;
structure S = CommandLineSettings;
val progName = CommandLine.name ();
fun showVersion () = TextIO.output (TextIO.stdErr, "LunarML <unreleased>\n")
fun showHelp () = TextIO.output (TextIO.stdErr, "Usage:\n\
                                                \  " ^ progName ^ " [options] file.sml\n\
                                                \Options:\n\
                                                \  -o,--output=file.ext  File name to output.\n\
                                                \  -mexe                 Produce a standalone script.\n\
                                                \  -mlib                 Produce a Lua module.\n\
                                                \  --lua                 Produce Lua code (targets Lua 5.3+).\n\
                                                \  --lua-continuations   Produce Lua code (targets Lua 5.3+ / supports one-shot delimited continuations).\n\
                                                \  --luajit              Produce Lua code (targets LuaJIT).\n\
                                                \  --js                  Produce JavaScript code.\n\
                                                \  --js-cps              Produce JavaScript code (CPS mode).\n\
                                                \  -h,--help             Show this message.\n\
                                                \  -v,--version          Show version information.\n\
                                                \  --dump                Dump intermediate code.\n\
                                                \  --dump-final          Dump final intermediate code.\n\
                                                \  -O,--optimize         Try to optimize hard.\n\
                                                \")
datatype dump_mode = NO_DUMP | DUMP_INITIAL | DUMP_FINAL
datatype lua_runtime = LUA_PLAIN | LUA_CONTINUATIONS
datatype backend = BACKEND_LUA of lua_runtime
                 | BACKEND_LUAJIT
                 | BACKEND_JS
                 | BACKEND_JS_CPS
type options = { output : string option
               , outputMode : Driver.OutputMode option
               , dump : dump_mode
               , optimizationLevel : int
               , backend : backend
               }
fun showMessageAndFail message = ( TextIO.output (TextIO.stdErr, message)
                                 ; OS.Process.exit OS.Process.failure
                                 )
fun readFile filename = let val ins = TextIO.openIn filename (* may raise Io *)
                        in TextIO.inputAll ins before TextIO.closeIn ins
                        end
fun getTargetInfo (opts : options) = (case #backend opts of
                                          BACKEND_LUA _ => { wideChar = TargetInfo.CHAR8
                                                           , nativeString = TargetInfo.NARROW_STRING
                                                           }
                                        | BACKEND_LUAJIT => { wideChar = TargetInfo.CHAR8
                                                            , nativeString = TargetInfo.NARROW_STRING
                                                            }
                                        | BACKEND_JS => { wideChar = TargetInfo.CHAR16
                                                        , nativeString = TargetInfo.WIDE_STRING
                                                        }
                                        | BACKEND_JS_CPS => { wideChar = TargetInfo.CHAR16
                                                            , nativeString = TargetInfo.WIDE_STRING
                                                            }
                                     )
type context = { driverContext : Driver.Context
               , baseDir : string
               , pathMap : string MLBSyntax.StringMap.map
               , targetInfo : TargetInfo.target_info
               }
fun optimize (ctx : context) fdecs 0 = fdecs
  | optimize ctx fdecs n = let val ctx' = { nextVId = #nextVId (#toFContext (#driverContext ctx))
                                          , nextTyVar = #nextTyVar (#toFContext (#driverContext ctx))
                                          , targetInfo = #targetInfo ctx
                                          }
                           in optimize ctx (#2 (FTransform.doDecs ctx' FTransform.initialEnv fdecs)) (n - 1)
                           end
fun emit (opts as { backend = BACKEND_LUA runtime, ... } : options) fileName nextId decs
    = let val progDir = OS.Path.dir progName
          val base = OS.Path.base fileName
          val mlinit_lua = OS.Path.joinDirFile { dir = progDir
                                               , file = case runtime of
                                                            LUA_PLAIN => "mlinit.lua"
                                                          | LUA_CONTINUATIONS => "mlinit-continuations.lua"
                                               }
          val mlinit = readFile mlinit_lua
          val luactx = { nextLuaId = nextId, targetLuaVersion = CodeGenLua.LUA5_3, hasDelimitedContinuations = runtime = LUA_CONTINUATIONS }
          val lua = case runtime of
                        LUA_PLAIN => CodeGenLua.doProgram luactx CodeGenLua.initialEnv decs
                      | LUA_CONTINUATIONS => CodeGenLua.doProgramWithContinuations luactx CodeGenLua.initialEnv decs
          val lua = #2 (LuaTransform.ProcessUpvalue.doBlock { nextId = nextId, maxUpvalue = 255 } LuaTransform.ProcessUpvalue.initialEnv lua)
          val lua = LuaTransform.ProcessLocal.doBlock { nextId = nextId, maxUpvalue = 255 } LuaTransform.ProcessLocal.initialEnv lua
          val lua = LuaWriter.doChunk lua
          val outs = TextIO.openOut (Option.getOpt (#output opts, base ^ ".lua")) (* may raise Io *)
          val () = TextIO.output (outs, mlinit)
          val () = TextIO.output (outs, lua)
          val () = TextIO.closeOut outs
      in ()
      end
  | emit (opts as { backend = BACKEND_LUAJIT, ... }) fileName nextId decs
    = let val progDir = OS.Path.dir progName
          val base = OS.Path.base fileName
          val mlinit_lua = OS.Path.joinDirFile { dir = progDir, file = "mlinit-luajit.lua" }
          val mlinit = readFile mlinit_lua
          val luactx = { nextLuaId = nextId, targetLuaVersion = CodeGenLua.LUAJIT, hasDelimitedContinuations = false }
          val lua = CodeGenLua.doProgram luactx CodeGenLua.initialEnv decs
          val lua = LuaTransform.LuaJITFixup.doBlock { nextId = nextId, maxUpvalue = 60 } lua
          val lua = #2 (LuaTransform.ProcessUpvalue.doBlock { nextId = nextId, maxUpvalue = 60 } LuaTransform.ProcessUpvalue.initialEnvForLuaJIT lua)
          val lua = LuaTransform.ProcessLocal.doBlock { nextId = nextId, maxUpvalue = 60 } LuaTransform.ProcessLocal.initialEnvForLuaJIT lua
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
          val jsctx = { nextJsId = nextId }
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
          val cexp = CpsTransform.transformDecs { nextVId = nextId } decs { exnCont = exnCont } cont
          val progDir = OS.Path.dir progName
          val base = OS.Path.base fileName
          val mlinit_js = OS.Path.joinDirFile { dir = progDir, file = "mlinit-cps.js" }
          val mlinit = readFile mlinit_js
          val jsctx = { nextJsId = nextId }
          val js = CodeGenJsCps.doProgram jsctx cont exnCont cexp
          val js = JsTransform.doProgram { nextVId = nextId } js
          val js = JsWriter.doProgram js
          val outs = TextIO.openOut (Option.getOpt (#output opts, base ^ ".js")) (* may raise Io *)
          val () = TextIO.output (outs, mlinit)
          val () = TextIO.output (outs, js)
          val () = TextIO.closeOut outs
      in ()
      end
fun doCompile (opts : options) fileName (f : context -> MLBEval.Env * MLBEval.Code)
    = let val progDir = OS.Path.dir progName
          val pathMap = List.foldl MLBSyntax.StringMap.insert' MLBSyntax.StringMap.empty
                                   [("SML_LIB", OS.Path.mkAbsolute { path = OS.Path.joinDirFile { dir = progDir, file = "sml-lib" }, relativeTo = OS.FileSys.getDir () })
                                   ,("TARGET_LANG", case #backend opts of BACKEND_LUA _ => "lua" | BACKEND_LUAJIT => "luajit" | BACKEND_JS => "js" | BACKEND_JS_CPS => "js-cps")
                                   ,("DELIMITED_CONTINUATIONS", case #backend opts of BACKEND_LUA LUA_CONTINUATIONS => "oneshot" | BACKEND_JS_CPS => "multishot" | _ => "none")
                                   ]
          val ctx = { driverContext = Driver.newContext ()
                    , baseDir = OS.FileSys.getDir ()
                    , pathMap = pathMap
                    , targetInfo = getTargetInfo opts
                    }
          val (env, { tynameset, toFEnv, fdecs, cache }) = f ctx
          val fdecs = case Option.getOpt (#outputMode opts, Driver.ExecutableMode) of
                          Driver.ExecutableMode => fdecs
                        | Driver.LibraryMode => ToFSyntax.addExport (#toFContext (#driverContext ctx), #typing env, toFEnv, fdecs)
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
fun handleInputFile opts [file] = if String.isSuffix ".sml" file then
                                      doCompile opts file (fn ctx =>
                                                              let val mlbdecs = [MLBSyntax.PathDec "$(SML_LIB)/basis/basis.mlb"
                                                                                ,MLBSyntax.PathDec file
                                                                                ]
                                                              in MLBEval.doDecs ctx LanguageOptions.default MLBEval.emptyEnv mlbdecs MLBEval.initialCode
                                                              end
                                                          )
                                  else if String.isSuffix ".mlb" file then
                                      doCompile opts file (fn ctx => MLBEval.doMlbSource ctx MLBEval.emptyEnv file MLBEval.initialCode)
                                  else
                                      showMessageAndFail "Input filename must end with '.sml'\n"
  | handleInputFile opts [] = showMessageAndFail "No input given.\n"
  | handleInputFile opts _ = showMessageAndFail "Multiple input is not supported.\n"
fun parseArgs (opts : options) [] = showMessageAndFail "No input given. Try --help.\n"
  | parseArgs opts (arg :: args)
    = let fun doOutput (outname, args) = case #output opts of
                                             NONE => parseArgs (S.set.output (SOME outname) opts) args
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
                 NONE => parseArgs (S.set.outputMode (SOME Driver.ExecutableMode) opts) args
               | SOME Driver.ExecutableMode => parseArgs opts args
               | SOME _ => showMessageAndFail "-mexe or -mlib was given multiple times.\n"
         else if arg = "-mlib" then
             case #outputMode opts of
                 NONE => parseArgs (S.set.outputMode (SOME Driver.LibraryMode) opts) args
               | SOME Driver.LibraryMode => parseArgs opts args
               | SOME _ => showMessageAndFail "-mexe or -mlib was given multiple times.\n"
         else if arg = "--lua" then
             parseArgs (S.set.backend (BACKEND_LUA LUA_PLAIN) opts) args
         else if arg = "--lua-continuations" then
             parseArgs (S.set.backend (BACKEND_LUA LUA_CONTINUATIONS) opts) args
         else if arg = "--luajit" then
             parseArgs (S.set.backend BACKEND_LUAJIT opts) args
         else if arg = "--js" then
             parseArgs (S.set.backend BACKEND_JS opts) args
         else if arg = "--js-cps" then
             parseArgs (S.set.backend BACKEND_JS_CPS opts) args
         else if arg = "-h" orelse arg = "--help" then
             ( showHelp (); OS.Process.exit OS.Process.success )
         else if arg = "-v" orelse arg = "--version" then
             ( showVersion (); OS.Process.exit OS.Process.success )
         else if arg = "--" then
             handleInputFile opts args
         else if arg = "--dump" then
             parseArgs (S.set.dump DUMP_INITIAL opts) args
         else if arg = "--dump-final" then
             parseArgs (S.set.dump DUMP_FINAL opts) args
         else if arg = "--optimize" orelse arg = "-O" then
             parseArgs (S.update.optimizationLevel (fn level => level + 1) opts) args
         else if String.isPrefix "-" arg then
             showMessageAndFail ("Unrecognized option: " ^ arg ^ ".\n")
         else
             handleInputFile opts (arg :: args)
      end
fun main () = let val args = CommandLine.arguments ()
                  val initialSettings = { output = NONE
                                        , outputMode = NONE
                                        , dump = NO_DUMP
                                        , optimizationLevel = 0
                                        , backend = BACKEND_LUA LUA_PLAIN
                                        }
              in parseArgs initialSettings args
                 handle Fail msg => (TextIO.output (TextIO.stdErr, "unhandled error: " ^ msg ^ "\n"); OS.Process.exit OS.Process.failure)
                      | IO.Io { name, function, cause } => (TextIO.output (TextIO.stdErr, "io error: " ^ name ^ ", " ^ function ^ ", " ^ (case cause of Fail msg => msg | _ => exnName cause) ^ "\n"); OS.Process.exit OS.Process.failure)
              end
end;
val () = Main.main ();
