(*
 * Copyright (c) 2023 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure Main:
sig
  val main: string * string list -> unit
end =
struct
  structure S = CommandLineSettings;

  datatype OutputMode = ExecutableMode | LibraryMode

  val progName = CommandLine.name ();
  fun showVersion () =
    TextIO.output
      (TextIO.stdErr, "LunarML version " ^ LunarMLVersion.version ^ "\n")
  fun showHelp () =
    TextIO.output
      ( TextIO.stdErr
      , "Usage:\n\
        \  " ^ progName
        ^
        " [subcommand] [options] file.sml\n\
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
        \  --nodejs              Produce JavaScript code for Node.js.\n\
        \  --nodejs-cps          Produce JavaScript code for Node.js (CPS mode).\n\
        \  -h,--help             Show this message.\n\
        \  -v,--version          Show version information.\n\
        \  --dump                Dump intermediate code.\n\
        \  --dump-final          Dump final intermediate code.\n\
        \  -O,--optimize         Try to optimize hard.\n\
        \  --mlb-path-map=<file> Specify MLB path map.\n\
        \  --mlb-path-var=<var>=<path>  Specify MLB path variable.\n\
        \  --default-ann <annotation>   Specify MLB annotations.\n\
        \  --print-timings       Print compilation times.\n\
        \  -B<libdir>            Library directory (default: <bindir>/../lib/lunarml).\n\
        \"
      )
  datatype dump_mode = NO_DUMP | DUMP_INITIAL | DUMP_FINAL
  datatype lua_runtime = LUA_PLAIN | LUA_CONTINUATIONS
  datatype backend =
    BACKEND_LUA of lua_runtime
  | BACKEND_LUAJIT
  | BACKEND_JS of CodeGenJs.code_style
  datatype subcommand = SUBCOMMAND_COMPILE
  type options =
    { subcommand: subcommand option
    , output: string option
    , outputMode: OutputMode option
    , dump: dump_mode
    , optimizationLevel: int
    , backend: backend
    , libDir: string
    , printTimings: bool
    , mlbPathSettings: MLBEval.path_setting list (* --mlb-path-map=file1 ... --mlb-path-map=fileN -> [PATH_MAP fileN, ..., PATH_MAP file1] *)
    , defaultAnnotations: string list (* --default-ann ann1 ... --default-ann annN -> [annN, ..., ann1] *)
    }
  fun showMessageAndFail message =
    (TextIO.output (TextIO.stdErr, message); OS.Process.exit OS.Process.failure)
  fun getTargetInfo (opts: options) : TargetInfo.target_info =
    (case #backend opts of
       BACKEND_LUA _ =>
         { defaultInt = Primitives.INT
         , defaultWord = Primitives.WORD
         , datatypeTag = TargetInfo.STRING8
         , minInt = SOME TargetInfo.minInt64
         , maxInt = SOME TargetInfo.maxInt64
         , wordSize = 64
         }
     | BACKEND_LUAJIT =>
         { defaultInt = Primitives.I54
         , defaultWord = Primitives.W32
         , datatypeTag = TargetInfo.STRING8
         , minInt = SOME TargetInfo.minInt54
         , maxInt = SOME TargetInfo.maxInt54
         , wordSize = 32
         }
     | BACKEND_JS _ =>
         { defaultInt = Primitives.I54
         , defaultWord = Primitives.W32
         , datatypeTag = TargetInfo.STRING16
         , minInt = SOME TargetInfo.minInt54
         , maxInt = SOME TargetInfo.maxInt54
         , wordSize = 32
         })
  fun optimizeCps (_: {nextVId: int ref, printTimings: bool}) cexp 0 = cexp
    | optimizeCps ctx cexp n =
        let
          val () =
            if #printTimings ctx then
              print ("[TIME] optimizeCps " ^ Int.toString n ^ "...")
            else
              ()
          val timer = Timer.startCPUTimer ()
          val ctx' =
            {nextVId = #nextVId ctx, simplificationOccurred = ref false}
          val cexp = CpsDeadCodeElimination.goCExp (ctx', cexp)
          val cexp = CpsUncurry.goCExp (ctx', cexp)
          val cexp = CpsUnpackRecordParameter.goCExp (ctx', cexp)
          val cexp = CpsLoopOptimization.goCExp (ctx', cexp)
          val cexp = CpsDecomposeRecursive.goCExp (ctx', cexp)
          val cexp = CpsConstantRefCell.goCExp (ctx', cexp)
          val cexp = CpsInline.goCExp (ctx', cexp)
        in
          if #printTimings ctx then
            print
              (" "
               ^
               LargeInt.toString (Time.toMicroseconds
                 (#usr (Timer.checkCPUTimer timer))) ^ " us\n")
          else
            ();
          if !(#simplificationOccurred ctx') then optimizeCps ctx cexp (n - 1)
          else cexp
        end
  fun emit (opts as {backend = BACKEND_LUA runtime, ...}: options)
        (_ (* targetInfo *)) fileName cont nextId cexp _ =
        let
          val timer = Timer.startCPUTimer ()
          val base = OS.Path.base fileName
          val mlinit_lua = OS.Path.joinDirFile
            { dir = #libDir opts
            , file =
                case runtime of
                  LUA_PLAIN => "mlinit.lua"
                | LUA_CONTINUATIONS => "mlinit-continuations.lua"
            }
          val mlinit = InitFile.readFile (InitFile.LUA, mlinit_lua)
          val luactx =
            { nextLuaId = nextId
            , targetLuaVersion = CodeGenLua.LUA5_3
            , hasDelimitedContinuations = runtime = LUA_CONTINUATIONS
            }
          val nested = NSyntax.toNested (NSyntax.fromCExp cexp)
          val lua =
            case runtime of
              LUA_PLAIN => CodeGenLua.doProgram luactx cont nested
            | LUA_CONTINUATIONS =>
                CodeGenLua.doProgramWithContinuations luactx cont nested
          val codegenTime = Time.toMicroseconds
            (#usr (Timer.checkCPUTimer timer))
          val lua =
            LuaTransform.InsertDo.doBlock
              (0, lua) (* TODO: pre-declared locals *)
          val lua = #2
            (LuaTransform.ProcessUpvalue.doBlock
               {nextId = nextId, maxUpvalue = 255}
               LuaTransform.ProcessUpvalue.initialEnv lua)
          val lua =
            LuaTransform.ProcessLocal.doBlock
              {nextId = nextId, maxUpvalue = 255}
              LuaTransform.ProcessLocal.initialEnv lua
          val codetransTime = Time.toMicroseconds
            (#usr (Timer.checkCPUTimer timer))
          val usedLib = StringSet.toList
            (LuaSyntax.predefinedIdsInBlock (lua, StringSet.empty))
          val lua = LuaWriter.doChunk lua
          val writeTime = Time.toMicroseconds (#usr (Timer.checkCPUTimer timer))
          val mlinit = InitFile.eliminateUnusedChunks (mlinit, usedLib)
          val outs = TextIO.openOut
            (Option.getOpt (#output opts, base ^ ".lua")) (* may raise Io *)
          val () = InitFile.output (outs, mlinit)
          val () = TextIO.output (outs, lua)
          val () = TextIO.closeOut outs
          val () =
            if #printTimings opts then
              print
                ("[TIME] Lua codegen: " ^ LargeInt.toString codegenTime
                 ^
                 " us\n\
                 \[TIME] Lua transformation: "
                 ^ LargeInt.toString (codetransTime - codegenTime)
                 ^
                 " us\n\
                 \[TIME] Lua writer: "
                 ^ LargeInt.toString (writeTime - codetransTime) ^ " us\n")
            else
              ()
        in
          ()
        end
    | emit (opts as {backend = BACKEND_LUAJIT, ...}) _ fileName cont nextId cexp
        _ =
        let
          val timer = Timer.startCPUTimer ()
          val base = OS.Path.base fileName
          val mlinit_lua =
            OS.Path.joinDirFile {dir = #libDir opts, file = "mlinit-luajit.lua"}
          val mlinit = InitFile.readFile (InitFile.LUA, mlinit_lua)
          val luactx =
            { nextLuaId = nextId
            , targetLuaVersion = CodeGenLua.LUAJIT
            , hasDelimitedContinuations = false
            }
          val nested = NSyntax.toNested (NSyntax.fromCExp cexp)
          val lua = CodeGenLua.doProgram luactx cont nested
          val codegenTime = Time.toMicroseconds
            (#usr (Timer.checkCPUTimer timer))
          val lua =
            LuaTransform.LuaJITFixup.doBlock {nextId = nextId, maxUpvalue = 60}
              lua
          val lua =
            LuaTransform.InsertDo.doBlock
              (0, lua) (* TODO: pre-declared locals *)
          val lua = #2
            (LuaTransform.ProcessUpvalue.doBlock
               {nextId = nextId, maxUpvalue = 60}
               LuaTransform.ProcessUpvalue.initialEnvForLuaJIT lua)
          val lua =
            LuaTransform.ProcessLocal.doBlock {nextId = nextId, maxUpvalue = 60}
              LuaTransform.ProcessLocal.initialEnvForLuaJIT lua
          val codetransTime = Time.toMicroseconds
            (#usr (Timer.checkCPUTimer timer))
          val usedLib = StringSet.toList
            (LuaSyntax.predefinedIdsInBlock (lua, StringSet.empty))
          val lua = LuaWriter.doChunk lua
          val writeTime = Time.toMicroseconds (#usr (Timer.checkCPUTimer timer))
          val mlinit = InitFile.eliminateUnusedChunks (mlinit, usedLib)
          val outs = TextIO.openOut
            (Option.getOpt (#output opts, base ^ ".lua")) (* may raise Io *)
          val () = InitFile.output (outs, mlinit)
          val () = TextIO.output (outs, lua)
          val () = TextIO.closeOut outs
          val () =
            if #printTimings opts then
              print
                ("[TIME] Lua codegen: " ^ LargeInt.toString codegenTime
                 ^
                 " us\n\
                 \[TIME] Lua transformation: "
                 ^ LargeInt.toString (codetransTime - codegenTime)
                 ^
                 " us\n\
                 \[TIME] Lua writer: "
                 ^ LargeInt.toString (writeTime - codetransTime) ^ " us\n")
            else
              ()
        in
          ()
        end
    | emit (opts as {backend = BACKEND_JS style, ...}) _ fileName cont nextId
        cexp export =
        let
          val timer = Timer.startCPUTimer ()
          val contEscapeMap = CpsAnalyze.contEscape (cont, cexp)
          val base = OS.Path.base fileName
          val mlinit_js =
            case style of
              CodeGenJs.DIRECT_STYLE =>
                OS.Path.joinDirFile {dir = #libDir opts, file = "mlinit.js"}
            | CodeGenJs.CPS =>
                OS.Path.joinDirFile {dir = #libDir opts, file = "mlinit-cps.js"}
          val mlinit = InitFile.readFile (InitFile.JS, mlinit_js)
          val jsctx =
            { nextJsId = nextId
            , contEscapeMap = contEscapeMap
            , style = style
            , imports = ref []
            }
          val js =
            case (style, export) of
              (CodeGenJs.DIRECT_STYLE, ToFSyntax.NO_EXPORT) =>
                CodeGenJs.doProgramDirect jsctx cont cexp
            | (CodeGenJs.DIRECT_STYLE, ToFSyntax.EXPORT_VALUE) =>
                CodeGenJs.doProgramDirectDefaultExport jsctx cont cexp
            | (CodeGenJs.DIRECT_STYLE, ToFSyntax.EXPORT_NAMED names) =>
                CodeGenJs.doProgramDirectNamedExport jsctx cont cexp names
            | (CodeGenJs.CPS, ToFSyntax.NO_EXPORT) =>
                CodeGenJs.doProgramCPS jsctx cont cexp
            | (CodeGenJs.CPS, ToFSyntax.EXPORT_VALUE) =>
                CodeGenJs.doProgramCPSDefaultExport jsctx cont cexp
            | (CodeGenJs.CPS, ToFSyntax.EXPORT_NAMED names) =>
                CodeGenJs.doProgramCPSNamedExport jsctx cont cexp names
          val codegenTime = Time.toMicroseconds
            (#usr (Timer.checkCPUTimer timer))
          val js = JsTransform.doProgram {nextVId = nextId} js
          val codetransTime = Time.toMicroseconds
            (#usr (Timer.checkCPUTimer timer))
          val hasImports = not (List.null (!(#imports jsctx)))
          val imports = JsWriter.doImports (!(#imports jsctx))
          val usedLib = StringSet.toList
            (JsSyntax.predefinedIdsInBlock (js, StringSet.empty))
          val js = JsWriter.doProgram js
          val writeTime = Time.toMicroseconds (#usr (Timer.checkCPUTimer timer))
          val mlinit = InitFile.eliminateUnusedChunks (mlinit, usedLib)
          val outs = TextIO.openOut
            (Option.getOpt (#output opts, base ^ ".mjs")) (* may raise Io *)
          val () = TextIO.output (outs, imports)
          val () =
            if hasImports then () else TextIO.output (outs, "\"use strict\";\n")
          val () = InitFile.output (outs, mlinit)
          val () = TextIO.output (outs, js)
          val () = TextIO.closeOut outs
          val () =
            if #printTimings opts then
              print
                ("[TIME] JavaScript codegen: " ^ LargeInt.toString codegenTime
                 ^
                 " us\n\
                 \[TIME] JavaScript transformation: "
                 ^ LargeInt.toString (codetransTime - codegenTime)
                 ^
                 " us\n\
                 \[TIME] JavaScript writer: "
                 ^ LargeInt.toString (writeTime - codetransTime) ^ " us\n")
            else
              ()
        in
          ()
        end
  fun doCompile (opts: options) fileName
    (f: MLBEval.Context -> MLBEval.Env * MLBEval.Code) =
    let
      val pathMap = List.foldl StringMap.insert' StringMap.empty
        [ ( "SML_LIB"
          , OS.Path.mkAbsolute
              { path = OS.Path.joinDirFile {dir = #libDir opts, file = "ml"}
              , relativeTo = OS.FileSys.getDir ()
              }
          )
        , ( "TARGET_LANG"
          , case #backend opts of
              BACKEND_LUA _ => "lua"
            | BACKEND_LUAJIT => "luajit"
            | BACKEND_JS CodeGenJs.DIRECT_STYLE => "js"
            | BACKEND_JS CodeGenJs.CPS => "js-cps"
          )
        , ( "DELIMITED_CONTINUATIONS"
          , case #backend opts of
              BACKEND_LUA LUA_CONTINUATIONS => "oneshot"
            | BACKEND_JS CodeGenJs.CPS => "multishot"
            | _ => "none"
          )
        ]
      val targetInfo = getTargetInfo opts
      val errorCounter = Message.newCounter {errorTolerance = 10}
      fun printMessage {spans, domain = _, message, type_} =
        let
          val t =
            case type_ of
              Message.WARNING => "warning: "
            | Message.ERROR => "error: "
        in
          case spans of
            [] => TextIO.output (TextIO.stdErr, t ^ message ^ "\n")
          | { start = p1 as {file = f1, line = l1, column = c1}
            , end_ = p2 as {file = f2, line = l2, column = c2}
            } :: _ =>
              (if f1 = f2 then
                 if p1 = p2 then
                   TextIO.output
                     ( TextIO.stdErr
                     , f1 ^ ":" ^ Int.toString l1 ^ ":" ^ Int.toString c1 ^ ": "
                       ^ t ^ message ^ "\n"
                     )
                 else
                   TextIO.output
                     ( TextIO.stdErr
                     , f1 ^ ":" ^ Int.toString l1 ^ ":" ^ Int.toString c1 ^ "-"
                       ^ Int.toString l2 ^ ":" ^ Int.toString c2 ^ ": " ^ t
                       ^ message ^ "\n"
                     )
               else
                 TextIO.output
                   ( TextIO.stdErr
                   , f1 ^ ":" ^ Int.toString l1 ^ ":" ^ Int.toString c1 ^ "-"
                     ^ f2 ^ ":" ^ Int.toString l2 ^ ":" ^ Int.toString c2 ^ ": "
                     ^ t ^ message ^ "\n"
                   ))
        end
      val messageHandler = Message.newHandler (errorCounter, printMessage)
      val pathMap =
        List.foldr (MLBEval.loadPathVar messageHandler) pathMap
          (#mlbPathSettings opts)
      val defaultLanguageOptions =
        List.foldr (MLBEval.applyAnnotation messageHandler)
          LanguageOptions.default (#defaultAnnotations opts)
      val ctx: MLBEval.Context =
        { driverContext = Driver.newContext (targetInfo, errorCounter)
        , baseDir = OS.FileSys.getDir ()
        , pathMap = pathMap
        , targetInfo = targetInfo
        , defaultLanguageOptions = defaultLanguageOptions
        , messageHandler = messageHandler
        }
      val timer = Timer.startCPUTimer ()
      val (env, {tynameset = _, toFEnv, fdecs, cache = _}) = f ctx
      val toFContext =
        let
          fun printMessage {spans, domain = _, message, type_} =
            let
              val t =
                case type_ of
                  Message.WARNING => "warning: "
                | Message.ERROR => "error: "
            in
              case spans of
                [] => TextIO.output (TextIO.stdErr, t ^ message ^ "\n")
              | { start = p1 as {file = f1, line = l1, column = c1}
                , end_ = p2 as {file = f2, line = l2, column = c2}
                } :: _ =>
                  (if f1 = f2 then
                     if p1 = p2 then
                       TextIO.output
                         ( TextIO.stdErr
                         , f1 ^ ":" ^ Int.toString l1 ^ ":" ^ Int.toString c1
                           ^ ": " ^ message ^ "\n"
                         )
                     else
                       TextIO.output
                         ( TextIO.stdErr
                         , f1 ^ ":" ^ Int.toString l1 ^ ":" ^ Int.toString c1
                           ^ "-" ^ Int.toString l2 ^ ":" ^ Int.toString c2
                           ^ ": " ^ message ^ "\n"
                         )
                   else
                     TextIO.output
                       ( TextIO.stdErr
                       , f1 ^ ":" ^ Int.toString l1 ^ ":" ^ Int.toString c1
                         ^ "-" ^ f2 ^ ":" ^ Int.toString l2 ^ ":"
                         ^ Int.toString c2 ^ ": " ^ message ^ "\n"
                       ))
            end
          val messageHandler = Message.newHandler (errorCounter, printMessage)
        in
          { nextVId = #nextVId (#driverContext ctx)
          , nextTyVar = #nextTyVar (#driverContext ctx)
          , targetInfo = targetInfo
          , messageHandler = messageHandler
          }
        end
      val (fexp, export) =
        case Option.getOpt (#outputMode opts, ExecutableMode) of
          ExecutableMode =>
            (FSyntax.LetExp (fdecs, FSyntax.ExitProgram), ToFSyntax.NO_EXPORT)
        | LibraryMode =>
            ToFSyntax.addExport (toFContext, #typing env, toFEnv, fdecs)
      val frontTime = Time.toMicroseconds (#usr (Timer.checkCPUTimer timer))
      val () =
        if #dump opts = DUMP_INITIAL then
          print (Printer.build (FPrinter.doExp 0 fexp) ^ "\n")
        else
          ()
      val fexp =
        #doExp (DesugarPatternMatches.desugarPatternMatches toFContext) fexp
      val fexp = DecomposeValRec.doExp fexp
      val (_, fexp) = DeadCodeElimination.doExp fexp TypedSyntax.VIdSet.empty
      val optTime = Time.toMicroseconds (#usr (Timer.checkCPUTimer timer))
      val () =
        if #dump opts = DUMP_FINAL then
          print (Printer.build (FPrinter.doExp 0 fexp) ^ "\n")
        else
          ()
      val () =
        if #printTimings opts then
          print
            ("[TIME] frontend: " ^ LargeInt.toString frontTime
             ^
             " us\n\
             \[TIME] optimization (F): "
             ^ LargeInt.toString (optTime - frontTime) ^ " us\n")
        else
          ()
      val nextId = #nextVId (#driverContext ctx)
      val cont =
        let
          val n = !nextId
          val _ = nextId := n + 1
        in
          CSyntax.CVar.fromInt n
        end
      val exportAsRecord =
        case #backend opts of
          BACKEND_LUA _ => true
        | BACKEND_LUAJIT => true
        | BACKEND_JS CodeGenJs.DIRECT_STYLE => false
        | BACKEND_JS CodeGenJs.CPS => true
      val cexp =
        CpsTransform.transformT
          ( { targetInfo = targetInfo
            , nextVId = nextId
            , exportAsRecord = exportAsRecord
            }
          , CpsTransform.initialEnv
          ) fexp ([], cont)
      val cpsTime = Time.toMicroseconds (#usr (Timer.checkCPUTimer timer))
      val () =
        if #printTimings opts then
          print
            ("[TIME] CPS: " ^ LargeInt.toString (cpsTime - optTime) ^ " us\n")
        else
          ()
      val cexp =
        optimizeCps {nextVId = nextId, printTimings = #printTimings opts} cexp
          (3 * (#optimizationLevel opts + 3))
      val cexp =
        let val context = {nextVId = nextId, simplificationOccurred = ref false}
        in CpsSimplify.finalizeCExp (context, cexp)
        end
      val cexp =
        optimizeCps {nextVId = nextId, printTimings = #printTimings opts} cexp
          (3 * (#optimizationLevel opts + 3))
      val optTime = Time.toMicroseconds (#usr (Timer.checkCPUTimer timer))
      val () =
        if #printTimings opts then
          print
            ("[TIME] CPS optimization (total): "
             ^ LargeInt.toString (optTime - cpsTime) ^ " us\n")
        else
          ()
      val () = if Message.anyError errorCounter then raise Message.Abort else ()
    in
      emit opts targetInfo fileName cont nextId cexp export
    end
    handle
      Message.Abort => OS.Process.exit OS.Process.failure
    | DesugarPatternMatches.DesugarError ([], message) =>
        ( print ("internal error: " ^ message ^ "\n")
        ; OS.Process.exit OS.Process.failure
        )
    | DesugarPatternMatches.DesugarError
        ( { start = p1 as {file = f1, line = l1, column = c1}
          , end_ = p2 as {file = f2, line = l2, column = c2}
          } :: _
        , message
        ) =>
        ( if f1 = f2 then
            if p1 = p2 then
              print
                (f1 ^ ":" ^ Int.toString l1 ^ ":" ^ Int.toString c1 ^ ": "
                 ^ message ^ "\n")
            else
              print
                (f1 ^ ":" ^ Int.toString l1 ^ ":" ^ Int.toString c1 ^ "-"
                 ^ Int.toString l2 ^ ":" ^ Int.toString c2 ^ ": " ^ message
                 ^ "\n")
          else
            print
              (f1 ^ ":" ^ Int.toString l1 ^ ":" ^ Int.toString c1 ^ "-" ^ f2
               ^ ":" ^ Int.toString l2 ^ ":" ^ Int.toString c2 ^ ": " ^ message
               ^ "\n")
        ; OS.Process.exit OS.Process.failure
        )
    | CSyntax.InvalidCode message =>
        (print (message ^ "\n"); OS.Process.exit OS.Process.failure)
    | CodeGenLua.CodeGenError message =>
        (print (message ^ "\n"); OS.Process.exit OS.Process.failure)
    | CodeGenJs.CodeGenError message =>
        (print (message ^ "\n"); OS.Process.exit OS.Process.failure)
  fun handleInputFile opts [file] =
        if String.isSuffix ".sml" file then
          doCompile opts file (fn ctx =>
            let
              val mlbdecs =
                [ MLBSyntax.PathDec "$(SML_LIB)/basis/basis.mlb"
                , MLBSyntax.PathDec file
                ]
            in
              MLBEval.doDecs ctx (#defaultLanguageOptions ctx) MLBEval.emptyEnv
                mlbdecs MLBEval.initialCode
            end)
        else if String.isSuffix ".mlb" file then
          doCompile opts file (fn ctx =>
            MLBEval.doMlbSource ctx MLBEval.emptyEnv file MLBEval.initialCode)
        else
          showMessageAndFail "Input filename must end with '.sml'\n"
    | handleInputFile _ [] = showMessageAndFail "No input given.\n"
    | handleInputFile _ _ =
        showMessageAndFail "Multiple input is not supported.\n"
  datatype 'a option_action = SIMPLE of 'a | WITH_ARG of string -> 'a
  datatype option_desc = SHORT of string | LONG of string
  fun testOption (_, []) = NONE
    | testOption ((SHORT s, SIMPLE v), arg :: args) =
        if arg = s then SOME (v, args) else NONE
    | testOption ((SHORT s, WITH_ARG f), arg :: args) =
        if arg = s then
          case args of
            [] => raise Fail ("Missing argument after" ^ s)
          | arg' :: args' => SOME (f arg', args')
        else if String.isPrefix s arg then
          let val arg' = String.extract (arg, String.size s, NONE)
          in SOME (f arg', args)
          end
        else
          NONE
    | testOption ((LONG s, SIMPLE v), arg :: args) =
        if arg = s then SOME (v, args) else NONE
    | testOption ((LONG s, WITH_ARG f), arg :: args) =
        if arg = s then
          case args of
            [] => raise Fail ("Missing argument after" ^ s)
          | arg' :: args' => SOME (f arg', args')
        else if String.isPrefix (s ^ "=") arg then
          let val arg' = String.extract (arg, String.size s + 1, NONE)
          in SOME (f arg', args)
          end
        else
          NONE
  fun parseOption (_, []) = NONE
    | parseOption (descs, args) =
        let
          fun go [] = NONE
            | go (desc :: descs) =
                case testOption (desc, args) of
                  SOME r => SOME r
                | NONE => go descs
        in
          go descs
        end
  datatype option =
    OPT_OUTPUT of string (* -o,--output *)
  | OPT_EXE (* --exe *)
  | OPT_LIB (* --lib *)
  | OPT_TARGET_LUA (* --lua *)
  | OPT_TARGET_LUA_CONTINUATIONS (* --lua-continuations *)
  | OPT_TARGET_LUAJIT (* --luajit *)
  | OPT_TARGET_NODEJS (* --nodejs *)
  | OPT_TARGET_NODEJS_CPS (* --nodejs-cps *)
  | OPT_HELP (* -h,--help *)
  | OPT_VERSION (* -v,--version *)
  | OPT_STOP (* -- *)
  | OPT_DUMP (* --dump *)
  | OPT_DUMP_FINAL (* --dump-final *)
  | OPT_OPTIMIZE (* -O,--optimize *)
  | OPT_MLB_PATH_MAP of string (* --mlb-path-map, -mlb-path-map *)
  | OPT_MLB_PATH_VAR of string (* --mlb-path-var *)
  | OPT_DEFAULT_ANN of string (* --default-ann, -default-ann *)
  | OPT_LIB_DIR of string (* -B *)
  | OPT_PRINT_TIMINGS
  val optionDescs =
    [ (SHORT "-o", WITH_ARG OPT_OUTPUT)
    , (LONG "--output", WITH_ARG OPT_OUTPUT)
    , (LONG "--exe", SIMPLE OPT_EXE)
    , (LONG "--lib", SIMPLE OPT_LIB)
    , (LONG "--lua", SIMPLE OPT_TARGET_LUA)
    , (LONG "--lua-continuations", SIMPLE OPT_TARGET_LUA_CONTINUATIONS)
    , (LONG "--luajit", SIMPLE OPT_TARGET_LUAJIT)
    , (LONG "--nodejs", SIMPLE OPT_TARGET_NODEJS)
    , (LONG "--nodejs-cps", SIMPLE OPT_TARGET_NODEJS_CPS)
    , (SHORT "-h", SIMPLE OPT_HELP)
    , (LONG "--help", SIMPLE OPT_HELP)
    , (SHORT "-v", SIMPLE OPT_VERSION)
    , (LONG "--version", SIMPLE OPT_VERSION)
    , (LONG "--", SIMPLE OPT_STOP)
    , (LONG "--dump", SIMPLE OPT_DUMP)
    , (LONG "--dump-final", SIMPLE OPT_DUMP_FINAL)
    , (SHORT "-O", SIMPLE OPT_OPTIMIZE)
    , (LONG "--optimize", SIMPLE OPT_OPTIMIZE)
    , (LONG "--mlb-path-map", WITH_ARG OPT_MLB_PATH_MAP)
    , (LONG "-mlb-path-map", WITH_ARG OPT_MLB_PATH_MAP)
    , (LONG "--mlb-path-var", WITH_ARG OPT_MLB_PATH_VAR)
    , (LONG "--default-ann", WITH_ARG OPT_DEFAULT_ANN)
    , (LONG "-default-ann", WITH_ARG OPT_DEFAULT_ANN)
    , (SHORT "-B", WITH_ARG OPT_LIB_DIR)
    , (LONG "--print-timings", SIMPLE OPT_PRINT_TIMINGS)
    ]
  fun parseArgs (opts: options) args =
    case parseOption (optionDescs, args) of
      SOME (OPT_OUTPUT outname, args) =>
        (case #output opts of
           NONE => parseArgs (S.set.output (SOME outname) opts) args
         | SOME _ => showMessageAndFail "--output was given multiple times.\n")
    | SOME (OPT_EXE, args) =>
        (case #outputMode opts of
           NONE => parseArgs (S.set.outputMode (SOME ExecutableMode) opts) args
         | SOME ExecutableMode => parseArgs opts args
         | SOME _ =>
             showMessageAndFail "--exe or --lib was given multiple times.\n")
    | SOME (OPT_LIB, args) =>
        (case #outputMode opts of
           NONE => parseArgs (S.set.outputMode (SOME LibraryMode) opts) args
         | SOME LibraryMode => parseArgs opts args
         | SOME _ =>
             showMessageAndFail "--exe or --lib was given multiple times.\n")
    | SOME (OPT_TARGET_LUA, args) =>
        parseArgs (S.set.backend (BACKEND_LUA LUA_PLAIN) opts) args
    | SOME (OPT_TARGET_LUA_CONTINUATIONS, args) =>
        parseArgs (S.set.backend (BACKEND_LUA LUA_CONTINUATIONS) opts) args
    | SOME (OPT_TARGET_LUAJIT, args) =>
        parseArgs (S.set.backend BACKEND_LUAJIT opts) args
    | SOME (OPT_TARGET_NODEJS, args) =>
        parseArgs (S.set.backend (BACKEND_JS CodeGenJs.DIRECT_STYLE) opts) args
    | SOME (OPT_TARGET_NODEJS_CPS, args) =>
        parseArgs (S.set.backend (BACKEND_JS CodeGenJs.CPS) opts) args
    | SOME (OPT_HELP, _) => (showHelp (); OS.Process.exit OS.Process.success)
    | SOME (OPT_VERSION, _) =>
        (showVersion (); OS.Process.exit OS.Process.success)
    | SOME (OPT_STOP, args) => handleInputFile opts args
    | SOME (OPT_DUMP, args) => parseArgs (S.set.dump DUMP_INITIAL opts) args
    | SOME (OPT_DUMP_FINAL, args) => parseArgs (S.set.dump DUMP_FINAL opts) args
    | SOME (OPT_OPTIMIZE, args) =>
        parseArgs (S.update.optimizationLevel (fn level => level + 1) opts) args
    | SOME (OPT_MLB_PATH_MAP file, args) =>
        parseArgs
          (S.update.mlbPathSettings (fn xs => MLBEval.PATH_MAP file :: xs) opts)
          args
    | SOME (OPT_MLB_PATH_VAR v, args) =>
        parseArgs
          (S.update.mlbPathSettings (fn xs => MLBEval.PATH_VAR v :: xs) opts)
          args
    | SOME (OPT_DEFAULT_ANN ann, args) =>
        parseArgs (S.update.defaultAnnotations (fn xs => ann :: xs) opts) args
    | SOME (OPT_LIB_DIR libDir, args) =>
        parseArgs (S.set.libDir libDir opts) args
    | SOME (OPT_PRINT_TIMINGS, args) =>
        parseArgs (S.set.printTimings true opts) args
    | NONE =>
        (case args of
           arg :: args' =>
             if String.isPrefix "-" arg then
               showMessageAndFail ("Unrecognized option: " ^ arg ^ ".\n")
             else
               (case #subcommand opts of
                  NONE =>
                    (case arg of
                       "compile" =>
                         parseArgs
                           (S.set.subcommand (SOME SUBCOMMAND_COMPILE) opts)
                           args'
                     | "help" =>
                         (showHelp (); OS.Process.exit OS.Process.success)
                     | "version" =>
                         (showVersion (); OS.Process.exit OS.Process.success)
                     | "run" =>
                         showMessageAndFail "'run': not implemented yet\n"
                     | "repl" =>
                         showMessageAndFail "'repl': not implemented yet\n"
                     | _ =>
                         showMessageAndFail
                           ("Unrecognized subcommand: " ^ arg ^ ".\n"))
                | SOME _ => handleInputFile opts args)
         | [] => showMessageAndFail "No input given. Try --help.\n")
  fun main (progName, args) =
    let
      val progDir = OS.Path.dir progName
      val initialSettings =
        { subcommand = NONE
        , output = NONE
        , outputMode = NONE
        , dump = NO_DUMP
        , optimizationLevel = 0
        , backend = BACKEND_LUA LUA_PLAIN
        , libDir =
            List.foldl
              (fn (arc, dir) => OS.Path.joinDirFile {dir = dir, file = arc})
              progDir [OS.Path.parentArc, "lib", "lunarml"]
        , printTimings = false
        , mlbPathSettings = []
        , defaultAnnotations = []
        }
    in
      parseArgs initialSettings args
      handle
        Fail msg =>
          ( TextIO.output (TextIO.stdErr, "unhandled error: " ^ msg ^ "\n")
          ; OS.Process.exit OS.Process.failure
          )
      | IO.Io {name, function, cause} =>
          ( TextIO.output
              ( TextIO.stdErr
              , "io error: " ^ name ^ ", " ^ function ^ ", "
                ^
                (case cause of
                   Fail msg => msg
                 | _ => exnName cause) ^ "\n"
              )
          ; OS.Process.exit OS.Process.failure
          )
    end
end;
