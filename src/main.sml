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
                                                \  " ^ progName ^ " [subcommand] [options] file.sml\n\
                                                \Subcommand:\n\
                                                \  compile               Compile a program.\n\
                                                \  help                  Show this message.\n\
                                                \  version               Show version information.\n\
                                                \Options:\n\
                                                \  -o,--output=file.ext  File name to output.\n\
                                                \  --exe                 Produce a standalone script.\n\
                                                \  --lib                 Produce a Lua module.\n\
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
                                                \  -B<libdir>            Library directory (default: <bindir>/../lib/lunarml).\n\
                                                \")
datatype dump_mode = NO_DUMP | DUMP_INITIAL | DUMP_FINAL
datatype lua_runtime = LUA_PLAIN | LUA_CONTINUATIONS
datatype backend = BACKEND_LUA of lua_runtime
                 | BACKEND_LUAJIT
                 | BACKEND_JS
                 | BACKEND_JS_CPS
datatype subcommand = SUBCOMMAND_COMPILE
type options = { subcommand : subcommand option
               , output : string option
               , outputMode : Driver.OutputMode option
               , dump : dump_mode
               , optimizationLevel : int
               , backend : backend
               , libDir : string
               }
fun showMessageAndFail message = ( TextIO.output (TextIO.stdErr, message)
                                 ; OS.Process.exit OS.Process.failure
                                 )
fun readFile filename = let val ins = TextIO.openIn filename (* may raise Io *)
                        in TextIO.inputAll ins before TextIO.closeIn ins
                        end
fun getTargetInfo (opts : options) = (case #backend opts of
                                          BACKEND_LUA _ => { wideChar = TargetInfo.CHAR8
                                                           , datatypeTag = TargetInfo.STRING8
                                                           }
                                        | BACKEND_LUAJIT => { wideChar = TargetInfo.CHAR8
                                                            , datatypeTag = TargetInfo.STRING8
                                                            }
                                        | BACKEND_JS => { wideChar = TargetInfo.CHAR16
                                                        , datatypeTag = TargetInfo.STRING16
                                                        }
                                        | BACKEND_JS_CPS => { wideChar = TargetInfo.CHAR16
                                                            , datatypeTag = TargetInfo.STRING16
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
fun optimizeCps (ctx : { nextVId : int ref }) cexp 0 = cexp
  | optimizeCps ctx cexp n = let val usage = ref TypedSyntax.VIdMap.empty
                                 val () = CpsSimplify.usageInCExp (usage, cexp)
                             in optimizeCps ctx (CpsSimplify.simplifyCExp (ctx, TypedSyntax.VIdMap.empty, TypedSyntax.VIdMap.empty, TypedSyntax.VIdMap.empty, !usage, cexp)) (n - 1)
                             end
fun emit (opts as { backend = BACKEND_LUA runtime, ... } : options) fileName nextId decs
    = let val base = OS.Path.base fileName
          val mlinit_lua = OS.Path.joinDirFile { dir = #libDir opts
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
    = let val base = OS.Path.base fileName
          val mlinit_lua = OS.Path.joinDirFile { dir = #libDir opts, file = "mlinit-luajit.lua" }
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
    = let val base = OS.Path.base fileName
          val mlinit_js = OS.Path.joinDirFile { dir = #libDir opts, file = "mlinit.js" }
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
          val cexp = CpsTransform.transformDecs ({ nextVId = nextId }, TypedSyntax.VIdMap.empty) decs { exnCont = exnCont } cont
          val cexp = optimizeCps { nextVId = nextId } cexp (5 * (#optimizationLevel opts + 4))
          val base = OS.Path.base fileName
          val mlinit_js = OS.Path.joinDirFile { dir = #libDir opts, file = "mlinit-cps.js" }
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
    = let val pathMap = List.foldl MLBSyntax.StringMap.insert' MLBSyntax.StringMap.empty
                                   [("SML_LIB", OS.Path.mkAbsolute { path = OS.Path.joinDirFile { dir = #libDir opts, file = "sml-lib" }, relativeTo = OS.FileSys.getDir () })
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
datatype 'a option_action = SIMPLE of 'a
                          | WITH_ARG of string -> 'a
datatype option_desc = SHORT of string
                     | LONG of string
fun testOption (_, []) = NONE
  | testOption ((SHORT s, SIMPLE v), arg :: args) = if arg = s then
                                                        SOME (v, args)
                                                    else
                                                        NONE
  | testOption ((SHORT s, WITH_ARG f), arg :: args) = if arg = s then
                                                          case args of
                                                              [] => raise Fail ("Missing argument after" ^ s)
                                                            | arg' :: args' => SOME (f arg', args')
                                                      else if String.isPrefix s arg then
                                                          let val arg' = String.extract (arg, String.size s, NONE)
                                                          in SOME (f arg', args)
                                                          end
                                                      else
                                                          NONE
  | testOption ((LONG s, SIMPLE v), arg :: args) = if arg = s then
                                                       SOME (v, args)
                                                   else
                                                       NONE
  | testOption ((LONG s, WITH_ARG f), arg :: args) = if arg = s then
                                                         case args of
                                                             [] => raise Fail ("Missing argument after" ^ s)
                                                           | arg' :: args' => SOME (f arg', args')
                                                     else if String.isPrefix (s ^ "=") arg then
                                                         let val arg' = String.extract (arg, String.size s + 1, NONE)
                                                         in SOME (f arg', args)
                                                         end
                                                     else
                                                         NONE
fun parseOption (descs, []) = NONE
  | parseOption (descs, args) = let fun go [] = NONE
                                      | go (desc :: descs) = case testOption (desc, args) of
                                                                 SOME r => SOME r
                                                               | NONE => go descs
                                in go descs
                                end
datatype option = OPT_OUTPUT of string (* -o,--output *)
                | OPT_EXE (* --exe *)
                | OPT_LIB (* --lib *)
                | OPT_TARGET_LUA (* --lua *)
                | OPT_TARGET_LUA_CONTINUATIONS (* --lua-continuations *)
                | OPT_TARGET_LUAJIT (* --luajit *)
                | OPT_TARGET_JS (* --js *)
                | OPT_TARGET_JS_CPS (* --js-cps *)
                | OPT_HELP (* -h,--help *)
                | OPT_VERSION (* -v,--version *)
                | OPT_STOP (* -- *)
                | OPT_DUMP (* --dump *)
                | OPT_DUMP_FINAL (* --dump-final *)
                | OPT_OPTIMIZE (* -O,--optimize *)
                | OPT_LIB_DIR of string (* -B *)
val optionDescs = [(SHORT "-o", WITH_ARG OPT_OUTPUT)
                  ,(LONG "--output", WITH_ARG OPT_OUTPUT)
                  ,(LONG "--exe", SIMPLE OPT_EXE)
                  ,(LONG "--lib", SIMPLE OPT_LIB)
                  ,(LONG "--lua", SIMPLE OPT_TARGET_LUA)
                  ,(LONG "--lua-continuations", SIMPLE OPT_TARGET_LUA_CONTINUATIONS)
                  ,(LONG "--luajit", SIMPLE OPT_TARGET_LUAJIT)
                  ,(LONG "--js", SIMPLE OPT_TARGET_JS)
                  ,(LONG "--js-cps", SIMPLE OPT_TARGET_JS_CPS)
                  ,(SHORT "-h", SIMPLE OPT_HELP)
                  ,(LONG "--help", SIMPLE OPT_HELP)
                  ,(SHORT "-v", SIMPLE OPT_VERSION)
                  ,(LONG "--version", SIMPLE OPT_VERSION)
                  ,(LONG "--", SIMPLE OPT_STOP)
                  ,(LONG "--dump", SIMPLE OPT_DUMP)
                  ,(LONG "--dump-final", SIMPLE OPT_DUMP_FINAL)
                  ,(SHORT "-O", SIMPLE OPT_OPTIMIZE)
                  ,(LONG "--optimize", SIMPLE OPT_OPTIMIZE)
                  ,(SHORT "-B", WITH_ARG OPT_LIB_DIR)
                  ]
fun parseArgs (opts : options) args
    = case parseOption (optionDescs, args) of
          SOME (OPT_OUTPUT outname, args) => (case #output opts of
                                                  NONE => parseArgs (S.set.output (SOME outname) opts) args
                                                | SOME _ => showMessageAndFail "--output was given multiple times.\n"
                                             )
        | SOME (OPT_EXE, args) => (case #outputMode opts of
                                       NONE => parseArgs (S.set.outputMode (SOME Driver.ExecutableMode) opts) args
                                     | SOME Driver.ExecutableMode => parseArgs opts args
                                     | SOME _ => showMessageAndFail "--exe or --lib was given multiple times.\n"
                                  )
        | SOME (OPT_LIB, args) => (case #outputMode opts of
                                       NONE => parseArgs (S.set.outputMode (SOME Driver.LibraryMode) opts) args
                                     | SOME Driver.LibraryMode => parseArgs opts args
                                     | SOME _ => showMessageAndFail "--exe or --lib was given multiple times.\n"
                                  )
        | SOME (OPT_TARGET_LUA, args) => parseArgs (S.set.backend (BACKEND_LUA LUA_PLAIN) opts) args
        | SOME (OPT_TARGET_LUA_CONTINUATIONS, args) => parseArgs (S.set.backend (BACKEND_LUA LUA_CONTINUATIONS) opts) args
        | SOME (OPT_TARGET_LUAJIT, args) => parseArgs (S.set.backend BACKEND_LUAJIT opts) args
        | SOME (OPT_TARGET_JS, args) => parseArgs (S.set.backend BACKEND_JS opts) args
        | SOME (OPT_TARGET_JS_CPS, args) => parseArgs (S.set.backend BACKEND_JS_CPS opts) args
        | SOME (OPT_HELP, args) => ( showHelp (); OS.Process.exit OS.Process.success )
        | SOME (OPT_VERSION, args) => ( showVersion (); OS.Process.exit OS.Process.success )
        | SOME (OPT_STOP, args) => handleInputFile opts args
        | SOME (OPT_DUMP, args) => parseArgs (S.set.dump DUMP_INITIAL opts) args
        | SOME (OPT_DUMP_FINAL, args) => parseArgs (S.set.dump DUMP_FINAL opts) args
        | SOME (OPT_OPTIMIZE, args) => parseArgs (S.update.optimizationLevel (fn level => level + 1) opts) args
        | SOME (OPT_LIB_DIR libDir, args) => parseArgs (S.set.libDir libDir opts) args
        | NONE => (case args of
                       arg :: args' =>
                       if String.isPrefix "-" arg then
                           showMessageAndFail ("Unrecognized option: " ^ arg ^ ".\n")
                       else
                           (case #subcommand opts of
                                NONE => (case arg of
                                             "compile" => parseArgs (S.set.subcommand (SOME SUBCOMMAND_COMPILE) opts) args'
                                           | "help" => ( showHelp (); OS.Process.exit OS.Process.success )
                                           | "version" => ( showVersion (); OS.Process.exit OS.Process.success )
                                           | "run" => showMessageAndFail "'run': not implemented yet\n"
                                           | "repl" => showMessageAndFail "'repl': not implemented yet\n"
                                           | _ => showMessageAndFail ("Unrecognized subcommand: " ^ arg ^ ".\n")
                                        )
                              | SOME _ => handleInputFile opts args
                           )
                     | [] => showMessageAndFail "No input given. Try --help.\n"
                  )
fun main () = let val args = CommandLine.arguments ()
                  val progDir = OS.Path.dir progName
                  val initialSettings = { subcommand = NONE
                                        , output = NONE
                                        , outputMode = NONE
                                        , dump = NO_DUMP
                                        , optimizationLevel = 0
                                        , backend = BACKEND_LUA LUA_PLAIN
                                        , libDir = List.foldl (fn (arc, dir) => OS.Path.joinDirFile { dir = dir, file = arc }) progDir [OS.Path.parentArc, "lib", "lunarml"]
                                        }
              in parseArgs initialSettings args
                 handle Fail msg => (TextIO.output (TextIO.stdErr, "unhandled error: " ^ msg ^ "\n"); OS.Process.exit OS.Process.failure)
                      | IO.Io { name, function, cause } => (TextIO.output (TextIO.stdErr, "io error: " ^ name ^ ", " ^ function ^ ", " ^ (case cause of Fail msg => msg | _ => exnName cause) ^ "\n"); OS.Process.exit OS.Process.failure)
              end
end;
val () = Main.main ();
