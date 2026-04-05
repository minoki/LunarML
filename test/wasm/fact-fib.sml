(*
 * Build a Wasm module with factorial and fibonacci functions,
 * then write it out as a WAT file.
 *)
local
  open WasmSyntax
in

(* Type 0: (i32) -> i32 *)
val funcType : rectype =
  [SubType {final = true, supertypes = [], body = FuncType {params = [i32], results = [i32]}}]

(*
 * Function 0: factorial(n)
 *   if n == 0 then 1
 *   else n * factorial(n - 1)
 *)
val factFunc : func =
  { typeidx = 0
  , locals = []
  , body =
      [ LOCAL_GET 0
      , I32_TESTOP EQZ
      , IF (BlockTypeVal i32,
            [I32_CONST 1],
            [ LOCAL_GET 0
            , LOCAL_GET 0
            , I32_CONST 1
            , I32_BINOP SUB
            , CALL 0
            , I32_BINOP MUL
            ])
      ]
  }

(*
 * Function 1: fib(n)
 *   Uses a loop with two accumulators.
 *   local 1: a (current value)
 *   local 2: b (next value)
 *   local 3: temp
 *)
val fibFunc : func =
  { typeidx = 0
  , locals = [i32, i32, i32]
  , body =
      [ (* a = 0, b = 1 *)
        I32_CONST 0
      , LOCAL_SET 1
      , I32_CONST 1
      , LOCAL_SET 2
      , BLOCK (BlockTypeNone,
          [ LOOP (BlockTypeNone,
              [ (* if n == 0 then break *)
                LOCAL_GET 0
                , I32_TESTOP EQZ
                , BR_IF 1
                (* temp = b *)
              , LOCAL_GET 2
              , LOCAL_SET 3
                (* b = a + b *)
              , LOCAL_GET 1
              , LOCAL_GET 2
              , I32_BINOP ADD
              , LOCAL_SET 2
                (* a = temp *)
              , LOCAL_GET 3
              , LOCAL_SET 1
                (* n = n - 1 *)
              , LOCAL_GET 0
              , I32_CONST 1
              , I32_BINOP SUB
              , LOCAL_SET 0
                (* continue *)
              , BR 0
              ])
          ])
      , LOCAL_GET 1
      ]
  }

val theModule : module =
  { types = [funcType]
  , funcs = [factFunc, fibFunc]
  , tables = []
  , mems = []
  , globals = []
  , elems = []
  , datas = []
  , start = NONE
  , imports = []
  , exports =
      [ {name = "factorial", desc = ExportFunc 0}
      , {name = "fib", desc = ExportFunc 1}
      ]
  }

val () =
  let
    val args = CommandLine.arguments ()
    val binary = List.exists (fn s => s = "--binary") args
  in
    if binary then
      let val out = BinIO.openOut "fact-fib.wasm"
      in  WasmWriter.writeModule (out, theModule)
        ; BinIO.closeOut out
        ; print "Wrote fact-fib.wasm\n"
      end
    else
      let val out = TextIO.openOut "fact-fib.wat"
      in  WatWriter.writeModule (out, theModule)
        ; TextIO.closeOut out
        ; print "Wrote fact-fib.wat\n"
      end
  end

end
