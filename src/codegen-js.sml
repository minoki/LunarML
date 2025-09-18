(*
 * Copyright (c) 2023 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure CodeGenJs :>
sig
  exception CodeGenError of string
  type Context =
    { nextJsId: int ref
    , style: Backend.code_style
    , contEscapeMap: CpsAnalyze.cont_map
    , imports: ({specs: (string * JsSyntax.Id) list, moduleName: string} list) ref
    }
  val doProgramDirect: Context -> CSyntax.CVar -> NSyntax.Stat -> JsSyntax.Block
  val doProgramDirectDefaultExport: Context
                                    -> CSyntax.CVar
                                    -> NSyntax.Stat
                                    -> JsSyntax.Block
  val doProgramDirectNamedExport: Context
                                  -> CSyntax.CVar
                                  -> NSyntax.Stat
                                  -> string vector
                                  -> JsSyntax.Block
  val doProgramCPS: Context -> CSyntax.CVar -> NSyntax.Stat -> JsSyntax.Block
  val doProgramCPSDefaultExport: Context
                                 -> CSyntax.CVar
                                 -> NSyntax.Stat
                                 -> JsSyntax.Block
  val doProgramCPSNamedExport: Context
                               -> CSyntax.CVar
                               -> NSyntax.Stat
                               -> string vector
                               -> JsSyntax.Block
end =
struct
  exception CodeGenError of string
  (* Mapping of types:
   * int -> 54-bit signed integer, as a subset of 64-bit floating-point number
   * word -> 32-bit unsigned integer, as a subset of 64-bit floating-point number (excluding negative zero)
   * real -> number (64-bit floating-point number)
   * string -> immutable Uint8Array
   * char -> 8-bit unsigned integer, as a subset of 64-bit floating-point number (excluding negative zero)
   * String16.string (WideString.string) -> 16-bit string
   * Char16.char (WideChar.char) -> 16-bit unsigned integer, as a subset of 64-bit floating-point number (excluding negative zero)
   * exn -> object
   * bool -> boolean
   * ref -> [ <mutable> ]
   * list -> null | [<head>, <tail>]
   * tuple -> immutable Array
   * non-tuple record -> immutable object, with integer index starting with 0
   * vector -> immutable Array
   * array -> mutable Array
   * function -> function with optional _MLTAIL_ field (direct style)
   *)
  structure F = FSyntax
  structure C = CSyntax
  structure N = NSyntax
  structure J = JsSyntax

  fun ConstStat (vid, exp) =
    J.ConstStat (vector [(vid, exp)])

  val builtinsDirect =
    let
      open InitialEnv
    in
      List.foldl
        (fn ((vid, name), map) => TypedSyntax.VIdMap.insert (map, vid, name))
        TypedSyntax.VIdMap.empty
        [ (* exn *) (VId_Match, "_Match")
        , (VId_Bind, "_Bind")
        , (VId_Div, "_Div")
        , (VId_Overflow, "_Overflow")
        , (VId_Size, "_Size")
        , (VId_Subscript, "_Subscript")
        , (VId_Fail, "_Fail")
        , (VId_Match_tag, "_Match_tag")
        , (VId_Bind_tag, "_Bind_tag")
        , (VId_Div_tag, "_Div_tag")
        , (VId_Overflow_tag, "_Overflow_tag")
        , (VId_Size_tag, "_Size_tag")
        , (VId_Subscript_tag, "_Subscript_tag")
        , (VId_Fail_tag, "_Fail_tag")
        , (VId_Match_predicate, "_isMatch")
        , (VId_Bind_predicate, "_isBind")
        , (VId_Div_predicate, "_isDiv")
        , (VId_Overflow_predicate, "_isOverflow")
        , (VId_Size_predicate, "_isSize")
        , (VId_Subscript_predicate, "_isSubscript")
        , (VId_Fail_predicate, "_isFail")
        , (VId_Fail_payload, "_Fail_payload")
        , (VId_exnName, "_exnName")
        (* string *)
        , (VId_String_concat, "_String_concat")
        , (VId_String_concatWith, "_String_concatWith")
        , (VId_String_implode, "_String_implode")
        , (VId_String_translate, "_String_translate")
        (* real *)
        , (VId_Real_abs, "Math_abs") (* JS Math.abs *)
        (* Vector and Array *)
        , (VId_Vector_tabulate, "_VectorOrArray_tabulate")
        , (VId_Vector_concat, "_Vector_concat")
        , (VId_Vector_fromList, "_VectorOrArray_fromList")
        , (VId_Array_fromList, "_VectorOrArray_fromList")
        , (VId_Array_tabulate, "_VectorOrArray_tabulate")
        (* JS interface *)
        , (VId_JavaScript_undefined, "undefined")
        , (VId_JavaScript_null, "null")
        , (VId_JavaScript_function, "_function")
        , (VId_JavaScript_encodeUtf8, "_encodeUtf8")
        , (VId_JavaScript_decodeUtf8, "_decodeUtf8")
        , (VId_JavaScript_Error, "_id")
        , (VId_JavaScript_Error_predicate, "_isError")
        , (VId_JavaScript_Error_payload, "_id")
        , (VId_JavaScript_wrapThenable, "_wrapThenable")
        , (VId_JavaScript_unwrapThenable, "_unwrapThenable")
        ]
    end
  val builtinsCPS =
    let
      open InitialEnv
    in
      List.foldl
        (fn ((vid, name), map) => TypedSyntax.VIdMap.insert (map, vid, name))
        TypedSyntax.VIdMap.empty
        [ (* exn *) (VId_Match, "_Match")
        , (VId_Bind, "_Bind")
        , (VId_Div, "_Div")
        , (VId_Overflow, "_Overflow")
        , (VId_Size, "_Size")
        , (VId_Subscript, "_Subscript")
        , (VId_Fail, "_Fail")
        , (VId_Match_tag, "_Match_tag")
        , (VId_Bind_tag, "_Bind_tag")
        , (VId_Div_tag, "_Div_tag")
        , (VId_Overflow_tag, "_Overflow_tag")
        , (VId_Size_tag, "_Size_tag")
        , (VId_Subscript_tag, "_Subscript_tag")
        , (VId_Fail_tag, "_Fail_tag")
        , (VId_Match_predicate, "_isMatch")
        , (VId_Bind_predicate, "_isBind")
        , (VId_Div_predicate, "_isDiv")
        , (VId_Overflow_predicate, "_isOverflow")
        , (VId_Size_predicate, "_isSize")
        , (VId_Subscript_predicate, "_isSubscript")
        , (VId_Fail_predicate, "_isFail")
        , (VId_Fail_payload, "_Fail_payload")
        (* string *)
        , (VId_String_concatWith, "_String_concatWith")
        (* JS interface *)
        , (VId_JavaScript_undefined, "undefined")
        , (VId_JavaScript_null, "null")
        , (VId_JavaScript_Error, "_id")
        , (VId_JavaScript_Error_predicate, "_isError")
        , (VId_JavaScript_Error_payload, "_id")
        , (VId_JavaScript_wrapThenable, "_wrapThenable")
        , (VId_JavaScript_unwrapThenable, "_unwrapThenable")
        (* extra *)
        , (VId_DelimCont_pushPrompt, "_pushPrompt")
        , (VId_DelimCont_withSubCont, "_withSubCont")
        , (VId_DelimCont_pushSubCont, "_pushSubCont")
        , (VId_DelimCont_topLevel, "_topLevel")
        ]
    end

  type Context =
    { nextJsId: int ref
    , style: Backend.code_style
    , contEscapeMap: CpsAnalyze.cont_map
    , imports: ({specs: (string * J.Id) list, moduleName: string} list) ref
    }

  fun VIdToJs (ctx: Context) (vid as TypedSyntax.MkVId (name, n)) =
    if n < 0 then
      let
        val builtins =
          case #style ctx of
            Backend.DIRECT_STYLE => builtinsDirect
          | Backend.CPS => builtinsCPS
      in
        case TypedSyntax.VIdMap.find (builtins, vid) of
          NONE =>
            raise Fail
              ("the built-in symbol "
               ^ Syntax.SourceName.getStringWithDefault (name, "?") ^ "@"
               ^ Int.toString n ^ " is not supported by JavaScript backend")
        | SOME jsName => JsSyntax.PredefinedId jsName
      end
    else
      JsSyntax.UserDefinedId vid

  datatype cont_type =
    BREAK_TO of
      { label: J.Id
      , which: (J.Id * J.JsConst) option
      , params: (J.Id option) list
      }
  | CONTINUE_TO of
      { label: J.Id
      , which: (J.Id * J.JsConst) option
      , params: (J.Id option) list
      }
  | TAILCALL of C.CVar (* continuation passing style *)
  | RETURN_TRAMPOLINE (* direct style *)
  | RETURN_SIMPLE (* direct style *)
  type Env =
    { continuations: cont_type C.CVarMap.map
    , subst: J.Exp TypedSyntax.VIdMap.map
    }

  fun doValue (ctx, env: Env) (C.Var vid) =
        (case TypedSyntax.VIdMap.find (#subst env, vid) of
           SOME e => e
         | NONE =>
             case VIdToJs ctx vid of
               J.PredefinedId "null" => J.ConstExp J.Null
             | J.PredefinedId "false" => J.ConstExp J.False
             | J.PredefinedId "true" => J.ConstExp J.True
             | id => J.VarExp id)
    | doValue _ C.Unit = J.UndefinedExp
    | doValue _ C.Nil = J.ConstExp J.Null (* empty list *)
    | doValue _ (C.TypedNil _) = J.ConstExp J.Null (* empty list *)
    | doValue _ (C.BoolConst false) = J.ConstExp J.False
    | doValue _ (C.BoolConst true) = J.ConstExp J.True
    | doValue _ (C.IntConst (Primitives.INT, _)) =
        raise Fail "NativeIntConst is not supported by JavaScript backend"
    | doValue _ (C.IntConst (Primitives.I32, x)) =
        if x < 0 then
          J.UnaryExp (J.NEGATE, J.ConstExp (J.Numeral (LargeInt.toString (~x))))
        else
          J.ConstExp (J.Numeral (LargeInt.toString x))
    | doValue _ (C.IntConst (Primitives.I54, x)) =
        if x < 0 then
          J.UnaryExp (J.NEGATE, J.ConstExp (J.Numeral (LargeInt.toString (~x))))
        else
          J.ConstExp (J.Numeral (LargeInt.toString x))
    | doValue _ (C.IntConst (Primitives.I64, x)) =
        if x < 0 then
          J.UnaryExp (J.NEGATE, J.ConstExp (J.Numeral
            (LargeInt.toString (~x) ^ "n")))
        else
          J.ConstExp (J.Numeral (LargeInt.toString x ^ "n"))
    | doValue _ (C.IntConst (Primitives.INT_INF, x)) =
        if x < 0 then
          J.UnaryExp (J.NEGATE, J.ConstExp (J.Numeral
            (LargeInt.toString (~x) ^ "n")))
        else
          J.ConstExp (J.Numeral (LargeInt.toString x ^ "n"))
    | doValue _ (C.WordConst (Primitives.WORD, _)) =
        raise Fail "NativeWordConst is not supported by JavaScript backend"
    | doValue _ (C.WordConst (Primitives.W32, x)) =
        J.ConstExp (J.Numeral ("0x" ^ LargeInt.fmt StringCvt.HEX x))
    | doValue _ (C.WordConst (Primitives.W64, x)) =
        J.ConstExp (J.Numeral ("0x" ^ LargeInt.fmt StringCvt.HEX x ^ "n"))
    | doValue _ (C.CharConst x) =
        J.ConstExp (J.Numeral (Int.toString (ord x)))
    | doValue _ (C.Char16Const x) =
        J.ConstExp (J.Numeral (Int.toString x))
    | doValue _ (C.StringConst x) =
        J.MethodExp
          ( J.VarExp (J.PredefinedId "Uint8Array")
          , "of"
          , Vector.map (J.ConstExp o J.Numeral o Int.toString o Char.ord)
              (Vector.fromList (String.explode x))
          )
    | doValue _ (C.String16Const x) =
        J.ConstExp (J.WideString x)
    | doValue _ (C.PrimEffect _) = J.ConstExp J.Null
    | doValue ctx (C.Cast {value, ...}) = doValue ctx value
    | doValue ctx (C.Pack {value, ...}) = doValue ctx value

  fun CVarToId v =
    TypedSyntax.MkVId (Syntax.SourceName.fromString "cont", C.CVar.toInt v)
  fun CVarToJs v =
    J.UserDefinedId (CVarToId v)
  fun doCVar v =
    J.VarExp (CVarToJs v)

  fun LabelToObjectKey (Syntax.NumericLabel n) =
        JsSyntax.IntKey (n - 1)
    | LabelToObjectKey (Syntax.IdentifierLabel s) = JsSyntax.StringKey s

  datatype prim_effect = datatype Primitives.prim_effect

  fun genSymNamed (ctx: Context, name) =
    let
      val n = !(#nextJsId ctx)
      val _ = #nextJsId ctx := n + 1
    in
      TypedSyntax.MkVId (Syntax.SourceName.fromString name, n)
    end
  fun genSym ctx = genSymNamed (ctx, "tmp")
  (*
  fun genExnContSym (ctx : Context) = let val n = !(#nextJsId ctx)
                                          val _ = #nextJsId ctx := n + 1
                                      in CSyntax.CVar.fromInt n
                                      end
  *)

  fun applyCont (_: Context, env: Env, cont, args) =
    case C.CVarMap.find (#continuations env, cont) of
      SOME (BREAK_TO {label, which, params}) =>
        let
          val (params', args') =
            ListPair.foldrEq
              (fn (SOME p, a, (pp, aa)) => (p :: pp, a :: aa)
                | (NONE, _, acc) => acc) ([], []) (params, args)
          val setWhich =
            case which of
              NONE => []
            | SOME (whichVar, whichVal) =>
                [J.AssignStat (J.VarExp whichVar, J.ConstExp whichVal)]
        in
          J.MultiAssignStat (List.map J.VarExp params', args') @ setWhich
          @ [J.BreakStat (SOME label)]
        end
    | SOME (CONTINUE_TO {label, which, params}) =>
        let
          val (params', args') =
            ListPair.foldrEq
              (fn (SOME p, a, (pp, aa)) => (p :: pp, a :: aa)
                | (NONE, _, acc) => acc) ([], []) (params, args)
          val setWhich =
            case which of
              NONE => []
            | SOME (whichVar, whichVal) =>
                [J.AssignStat (J.VarExp whichVar, J.ConstExp whichVal)]
        in
          J.MultiAssignStat (List.map J.VarExp params', args') @ setWhich
          @ [J.ContinueStat (SOME label)]
        end
    | SOME (TAILCALL k) =>
        [J.ReturnStat (SOME (J.ArrayExp (vector
           [J.ConstExp J.False, doCVar k, J.ArrayExp (vector args)])))] (* continuation passing style *)
    | SOME RETURN_TRAMPOLINE =>
        [J.ReturnStat (SOME (J.ArrayExp (vector (J.ConstExp J.True :: args))))] (* direct style *)
    | SOME RETURN_SIMPLE =>
        (case args of
           [v] => [J.ReturnStat (SOME v)] (* direct style *)
         | _ => raise CodeGenError "invalid return arity")
    | NONE => raise CodeGenError "undefined continuation"
  (*:
  val doExp : Context * Env * N.Exp -> J.Exp
  val doDecs : Context * Env * N.Dec list * N.Stat * J.Stat list -> J.Stat list
  val doStat : Context -> Env -> N.Stat -> J.Stat list
   *)
  fun doExp (ctx, env, N.Value v) =
        doValue (ctx, env) v
    | doExp (ctx, env, N.PrimOp {primOp, tyargs = _, args}) =
        (case (primOp, args) of
           (F.RealConstOp x, _) =>
             let
               val y =
                 Numeric.toDecimal
                   { nominal_format = Numeric.binary64
                   , target_format = Numeric.binary64
                   } x
             (* JavaScript does not support hexadecimal floating-point literals *)
             in
               case y of
                 SOME z =>
                   if Numeric.Notation.isNegative z then
                     J.UnaryExp (J.NEGATE, J.ConstExp (J.Numeral
                       (Numeric.Notation.toString "-" (Numeric.Notation.abs z))))
                   else
                     J.ConstExp (J.Numeral (Numeric.Notation.toString "-" z))
               | NONE =>
                   raise CodeGenError
                     "the hexadecimal floating-point value cannot be represented as a 64-bit floating-point number"
             end
         | (F.ListOp, []) => J.ConstExp J.Null
         | (F.ListOp, xs) =>
             J.CallExp (J.VarExp (J.PredefinedId "_list"), vector
               [J.ArrayExp
                  (Vector.map (fn x => doExp (ctx, env, x)) (vector xs))])
         | (F.VectorOp, xs) =>
             J.ArrayExp (Vector.map (fn x => doExp (ctx, env, x)) (vector xs))
         | (F.DataTagAsString16Op info, [exp]) =>
             (case #representation info of
                Syntax.REP_BOXED =>
                  J.IndexExp
                    ( doExp (ctx, env, exp)
                    , J.ConstExp (J.asciiStringAsWide "tag")
                    )
              | Syntax.REP_ENUM => doExp (ctx, env, exp)
              | _ =>
                  raise CodeGenError
                    "unexpected datatype representation for DataTagAsString16Op")
         | (F.DataPayloadOp info, [exp]) =>
             (case #representation info of
                Syntax.REP_BOXED =>
                  J.IndexExp
                    ( doExp (ctx, env, exp)
                    , J.ConstExp (J.asciiStringAsWide "payload")
                    )
              | Syntax.REP_ALIAS => doExp (ctx, env, exp)
              | _ =>
                  raise CodeGenError
                    "unexpected datatype representation for DataPayloadOp")
         | (F.ExnPayloadOp, [exp]) =>
             J.IndexExp
               ( doExp (ctx, env, exp)
               , J.ConstExp (J.asciiStringAsWide "payload")
               )
         | (F.ConstructValOp info, []) =>
             let
               val tag = #tag info
             in
               case #representation info of
                 Syntax.REP_BOXED =>
                   J.ObjectExp (vector
                     [(J.StringKey "tag", J.ConstExp (J.asciiStringAsWide tag))])
               | Syntax.REP_ENUM => J.ConstExp (J.asciiStringAsWide tag)
               | Syntax.REP_UNIT => J.ConstExp J.Null
               | _ =>
                   raise CodeGenError
                     "unexpected datatype representation for ConstructValOp"
             end
         | (F.ConstructValWithPayloadOp info, [payload]) =>
             let
               val tag = #tag info
               val payload = doExp (ctx, env, payload)
             in
               case #representation info of
                 Syntax.REP_BOXED =>
                   J.ObjectExp (vector
                     [ (J.StringKey "tag", J.ConstExp (J.asciiStringAsWide tag))
                     , (J.StringKey "payload", payload)
                     ])
               | Syntax.REP_ALIAS => payload
               | _ =>
                   raise CodeGenError
                     "unexpected datatype representation for ConstructValWithPayloadOp"
             end
         | (F.ConstructExnOp, [tag]) =>
             let val tag = doExp (ctx, env, tag)
             in J.NewExp (tag, vector [])
             end
         | (F.ConstructExnWithPayloadOp, [tag, payload]) =>
             let
               val tag = doExp (ctx, env, tag)
               val payload = doExp (ctx, env, payload)
             in
               J.NewExp (tag, vector [payload])
             end
         | ( F.RaiseOp _ (* span as { start as { file, line, column }, ... } *)
           , _
           ) => raise CodeGenError "unexpected RaiseOp"
         | (F.PrimCall prim, args) =>
             let
               (*
               fun action ([], stmt) =
                     doDecs (ctx, env, decs, finalExp, stmt :: revStats)
                 | action (_ :: _, stmt) =
                     raise CodeGenError
                       ("primop " ^ Primitives.toString prim
                        ^ ": unexpected number of results for an action")
                        *)
               fun doNullary f =
                 case args of
                   [] => f ()
                 | _ =>
                     raise CodeGenError
                       ("primop " ^ Primitives.toString prim
                        ^ ": invalid number of arguments")
               fun doNullaryExp (f, _ (* purity *)) = doNullary f
               fun doUnary f =
                 case args of
                   [a] => f (doExp (ctx, env, a))
                 | _ =>
                     raise CodeGenError
                       ("primop " ^ Primitives.toString prim
                        ^ ": invalid number of arguments")
               fun doUnaryExp (f, _ (* purity *)) = doUnary f
               fun doBinary f =
                 case args of
                   [a, b] => f (doExp (ctx, env, a), doExp (ctx, env, b))
                 | _ =>
                     raise CodeGenError
                       ("primop " ^ Primitives.toString prim
                        ^ ": invalid number of arguments")
               fun doBinaryExp (f, _ (* purity *)) = doBinary f
               fun doBinaryOp (binop, pure) =
                 doBinaryExp (fn (a, b) => J.BinExp (binop, a, b), pure)
               fun doTernary f =
                 case args of
                   [a, b, c] =>
                     f ( doExp (ctx, env, a)
                       , doExp (ctx, env, b)
                       , doExp (ctx, env, c)
                       )
                 | _ =>
                     raise CodeGenError
                       ("primop " ^ Primitives.toString prim
                        ^ ": invalid number of arguments")
               fun doUnaryExpE f =
                 case args of
                   [a, _] => f (doExp (ctx, env, a))
                 | _ =>
                     raise CodeGenError
                       ("primop " ^ Primitives.toString prim
                        ^ ": invalid number of arguments")
               fun doBinaryExpE f =
                 case args of
                   [a, b, _] => f (doExp (ctx, env, a), doExp (ctx, env, b))
                 | _ =>
                     raise CodeGenError
                       ("primop " ^ Primitives.toString prim
                        ^ ": invalid number of arguments")
               fun doBinaryOpE binop =
                 doBinaryExpE (fn (a, b) => J.BinExp (binop, a, b))
             in
               case prim of
                 Primitives.mkFn2 =>
                   doUnary (fn f =>
                     J.CallExp (J.VarExp (J.PredefinedId "_mkFn2"), vector [f]))
               | Primitives.mkFn3 =>
                   doUnary (fn f =>
                     J.CallExp (J.VarExp (J.PredefinedId "_mkFn3"), vector [f]))
               | Primitives.List_cons =>
                   doBinaryExp (fn (x, xs) => J.ArrayExp (vector [x, xs]), PURE)
               | Primitives.List_null =>
                   doUnaryExp
                     (fn a => J.BinExp (J.EQUAL, a, J.ConstExp J.Null), PURE)
               | Primitives.List_unsafeHead =>
                   doUnaryExp
                     ( fn xs => J.IndexExp (xs, J.ConstExp (J.Numeral "0"))
                     , PURE
                     )
               | Primitives.List_unsafeTail =>
                   doUnaryExp
                     ( fn xs => J.IndexExp (xs, J.ConstExp (J.Numeral "1"))
                     , PURE
                     )
               | Primitives.General_exnName =>
                   doUnaryExp
                     ( fn a =>
                         J.CallExp
                           (J.VarExp (J.PredefinedId "_exnName"), vector [a])
                     , PURE
                     )
               | Primitives.Ref_ref =>
                   doUnaryExp
                     ( fn x =>
                         (* REPRESENTATION_OF_REF *)
                         J.ArrayExp (vector [x])
                     , DISCARDABLE
                     )
               | Primitives.Ref_EQUAL => doBinaryOp (J.EQUAL, PURE)
               | Primitives.Ref_set => raise CodeGenError "unexpected Ref.set"
               | Primitives.Ref_read =>
                   doUnaryExp
                     ( fn a => J.IndexExp (a, J.ConstExp (J.Numeral "0"))
                     , DISCARDABLE
                     ) (* REPRESENTATION_OF_REF *)
               | Primitives.Bool_EQUAL => doBinaryOp (J.EQUAL, PURE)
               | Primitives.Bool_not =>
                   doUnaryExp (fn a => J.UnaryExp (J.NOT, a), PURE)
               | Primitives.Int_EQUAL _ =>
                   doBinaryOp (J.EQUAL, PURE) (* Int32, Int54, IntInf *)
               | Primitives.Int_PLUS Primitives.I32 =>
                   doBinaryExp
                     ( fn (a, b) =>
                         J.CallExp
                           ( J.VarExp (J.PredefinedId "_Int32_add")
                           , vector [a, b]
                           )
                     , IMPURE
                     )
               | Primitives.Int_PLUS Primitives.I54 =>
                   doBinaryExp
                     ( fn (a, b) =>
                         J.CallExp
                           ( J.VarExp (J.PredefinedId "_Int54_add")
                           , vector [a, b]
                           )
                     , IMPURE
                     )
               | Primitives.Int_MINUS Primitives.I32 =>
                   doBinaryExp
                     ( fn (a, b) =>
                         J.CallExp
                           ( J.VarExp (J.PredefinedId "_Int32_sub")
                           , vector [a, b]
                           )
                     , IMPURE
                     )
               | Primitives.Int_MINUS Primitives.I54 =>
                   doBinaryExp
                     ( fn (a, b) =>
                         J.CallExp
                           ( J.VarExp (J.PredefinedId "_Int54_sub")
                           , vector [a, b]
                           )
                     , IMPURE
                     )
               | Primitives.Int_TIMES Primitives.I32 =>
                   doBinaryExp
                     ( fn (a, b) =>
                         J.CallExp
                           ( J.VarExp (J.PredefinedId "_Int32_mul")
                           , vector [a, b]
                           )
                     , IMPURE
                     )
               | Primitives.Int_TIMES Primitives.I54 =>
                   doBinaryExp
                     ( fn (a, b) =>
                         J.CallExp
                           ( J.VarExp (J.PredefinedId "_Int54_mul")
                           , vector [a, b]
                           )
                     , IMPURE
                     )
               | Primitives.Int_div Primitives.I32 =>
                   doBinaryExp
                     ( fn (a, b) =>
                         J.CallExp
                           ( J.VarExp (J.PredefinedId "_Int32_div")
                           , vector [a, b]
                           )
                     , IMPURE
                     )
               | Primitives.Int_div Primitives.I54 =>
                   doBinaryExp
                     ( fn (a, b) =>
                         J.CallExp
                           ( J.VarExp (J.PredefinedId "_Int54_div")
                           , vector [a, b]
                           )
                     , IMPURE
                     )
               | Primitives.Int_div_unchecked Primitives.I32 =>
                   doBinaryExp
                     ( fn (a, b) =>
                         J.ToInt32Exp
                           (J.MethodExp
                              ( J.VarExp (J.PredefinedId "Math")
                              , "floor"
                              , vector [J.BinExp (J.DIV, a, b)]
                              ))
                     , PURE
                     )
               | Primitives.Int_div_unchecked Primitives.I54 =>
                   doBinaryExp
                     ( fn (a, b) =>
                         J.BinExp
                           ( J.PLUS
                           , J.ConstExp (J.Numeral "0")
                           , J.MethodExp
                               ( J.VarExp (J.PredefinedId "Math")
                               , "floor"
                               , vector [J.BinExp (J.DIV, a, b)]
                               )
                           )
                     , PURE
                     )
               | Primitives.Int_mod Primitives.I32 =>
                   doBinaryExp
                     ( fn (a, b) =>
                         J.CallExp
                           ( J.VarExp (J.PredefinedId "_Int32_mod")
                           , vector [a, b]
                           )
                     , IMPURE
                     )
               | Primitives.Int_mod Primitives.I54 =>
                   doBinaryExp
                     ( fn (a, b) =>
                         J.CallExp
                           ( J.VarExp (J.PredefinedId "_Int54_mod")
                           , vector [a, b]
                           )
                     , IMPURE
                     )
               | Primitives.Int_mod_unchecked Primitives.I32 =>
                   doBinaryExp
                     ( fn (a, b) =>
                         J.CallExp
                           ( J.VarExp (J.PredefinedId "_Int32_mod")
                           , vector [a, b]
                           )
                     , PURE
                     )
               | Primitives.Int_mod_unchecked Primitives.I54 =>
                   doBinaryExp
                     ( fn (a, b) =>
                         J.CallExp
                           ( J.VarExp (J.PredefinedId "_Int54_mod")
                           , vector [a, b]
                           )
                     , PURE
                     )
               | Primitives.Int_quot Primitives.I32 =>
                   doBinaryExp
                     ( fn (a, b) =>
                         J.CallExp
                           ( J.VarExp (J.PredefinedId "_Int32_quot")
                           , vector [a, b]
                           )
                     , IMPURE
                     )
               | Primitives.Int_quot Primitives.I54 =>
                   doBinaryExp
                     ( fn (a, b) =>
                         J.CallExp
                           ( J.VarExp (J.PredefinedId "_Int54_quot")
                           , vector [a, b]
                           )
                     , IMPURE
                     )
               | Primitives.Int_quot_unchecked Primitives.I32 =>
                   doBinaryExp
                     (fn (a, b) => J.ToInt32Exp (J.BinExp (J.DIV, a, b)), PURE)
               | Primitives.Int_quot_unchecked Primitives.I54 =>
                   doBinaryExp
                     ( fn (a, b) =>
                         J.BinExp
                           ( J.PLUS
                           , J.ConstExp (J.Numeral "0")
                           , J.MethodExp
                               ( J.VarExp (J.PredefinedId "Math")
                               , "trunc"
                               , vector [J.BinExp (J.DIV, a, b)]
                               )
                           )
                     , PURE
                     )
               | Primitives.Int_rem_unchecked Primitives.I32 =>
                   doBinaryExp
                     (fn (a, b) => J.ToInt32Exp (J.BinExp (J.MOD, a, b)), PURE)
               | Primitives.Int_rem_unchecked Primitives.I54 =>
                   doBinaryExp
                     ( fn (a, b) =>
                         J.BinExp
                           ( J.PLUS
                           , J.ConstExp (J.Numeral "0")
                           , J.BinExp (J.MOD, a, b)
                           )
                     , PURE
                     )
               | Primitives.Int_TILDE Primitives.I32 =>
                   doUnaryExp
                     ( fn a =>
                         J.CallExp
                           ( J.VarExp (J.PredefinedId "_Int32_negate")
                           , vector [a]
                           )
                     , IMPURE
                     )
               | Primitives.Int_TILDE Primitives.I54 =>
                   doUnaryExp
                     ( fn a =>
                         J.CallExp
                           ( J.VarExp (J.PredefinedId "_Int54_negate")
                           , vector [a]
                           )
                     , IMPURE
                     )
               | Primitives.Int_TILDE_unchecked Primitives.I32 =>
                   doUnaryExp
                     (fn a => J.ToInt32Exp (J.UnaryExp (J.NEGATE, a)), PURE)
               | Primitives.Int_TILDE_unchecked Primitives.I54 =>
                   doUnaryExp
                     ( fn a => J.BinExp (J.MINUS, J.ConstExp (J.Numeral "0"), a)
                     , PURE
                     )
               | Primitives.Int_abs Primitives.I32 =>
                   doUnaryExp
                     ( fn a =>
                         J.CallExp
                           (J.VarExp (J.PredefinedId "_Int32_abs"), vector [a])
                     , IMPURE
                     )
               | Primitives.Int_abs Primitives.I54 =>
                   doUnaryExp
                     ( fn a =>
                         J.CallExp
                           (J.VarExp (J.PredefinedId "_Int54_abs"), vector [a])
                     , IMPURE
                     )
               | Primitives.Int_LT _ =>
                   doBinaryOp (J.LT, PURE) (* Int32, Int54, IntInf *)
               | Primitives.Int_GT _ =>
                   doBinaryOp (J.GT, PURE) (* Int32, Int54, IntInf *)
               | Primitives.Int_LE _ =>
                   doBinaryOp (J.LE, PURE) (* Int32, Int54, IntInf *)
               | Primitives.Int_GE _ =>
                   doBinaryOp (J.GE, PURE) (* Int32, Int54, IntInf *)
               | Primitives.Int_toInt_unchecked (Primitives.I32, Primitives.I32) =>
                   doUnaryExp (fn x => x, PURE) (* no-op *)
               | Primitives.Int_toInt_unchecked (Primitives.I32, Primitives.I54) =>
                   doUnaryExp (fn x => x, PURE) (* no-op *)
               | Primitives.Int_toInt_unchecked
                   (Primitives.I32, Primitives.INT_INF) =>
                   doUnaryExp
                     ( fn x =>
                         J.CallExp
                           (J.VarExp (J.PredefinedId "BigInt"), vector [x])
                     , PURE
                     )
               | Primitives.Int_toInt_unchecked
                   (Primitives.I54, Primitives.INT_INF) =>
                   doUnaryExp
                     ( fn x =>
                         J.CallExp
                           (J.VarExp (J.PredefinedId "BigInt"), vector [x])
                     , PURE
                     )
               | Primitives.Int_toInt_unchecked (Primitives.I54, Primitives.I32) =>
                   doUnaryExp (fn x => x, PURE) (* no-op *)
               | Primitives.Int_toInt_unchecked (Primitives.I54, Primitives.I54) =>
                   doUnaryExp (fn x => x, PURE) (* no-op *)
               | Primitives.Int_toInt_unchecked
                   (Primitives.INT_INF, Primitives.I32) =>
                   doUnaryExp
                     ( fn x =>
                         J.CallExp
                           (J.VarExp (J.PredefinedId "Number"), vector [x])
                     , PURE
                     )
               | Primitives.Int_toInt_unchecked
                   (Primitives.INT_INF, Primitives.I54) =>
                   doUnaryExp
                     ( fn x =>
                         J.CallExp
                           (J.VarExp (J.PredefinedId "Number"), vector [x])
                     , PURE
                     )
               | Primitives.Word_EQUAL _ => doBinaryOp (J.EQUAL, PURE)
               | Primitives.Word_PLUS Primitives.W32 =>
                   doBinaryExp
                     ( fn (a, b) => J.ToUint32Exp (J.BinExp (J.PLUS, a, b))
                     , PURE
                     )
               | Primitives.Word_MINUS Primitives.W32 =>
                   doBinaryExp
                     ( fn (a, b) => J.ToUint32Exp (J.BinExp (J.MINUS, a, b))
                     , PURE
                     )
               | Primitives.Word_TIMES Primitives.W32 =>
                   doBinaryExp
                     ( fn (a, b) =>
                         J.ToUint32Exp (J.CallExp
                           ( J.VarExp (J.PredefinedId "Math_imul")
                           , vector [a, b]
                           ))
                     , PURE
                     )
               | Primitives.Word_TILDE Primitives.W32 =>
                   doUnaryExp
                     (fn a => J.ToUint32Exp (J.UnaryExp (J.NEGATE, a)), PURE)
               | Primitives.Word_div_unchecked Primitives.W32 =>
                   doBinaryExp
                     (fn (a, b) => J.ToUint32Exp (J.BinExp (J.DIV, a, b)), PURE)
               | Primitives.Word_mod_unchecked Primitives.W32 =>
                   doBinaryExp (fn (a, b) => J.BinExp (J.MOD, a, b), PURE)
               | Primitives.Word_LT _ => doBinaryOp (J.LT, PURE)
               | Primitives.Word_GT _ => doBinaryOp (J.GT, PURE)
               | Primitives.Word_LE _ => doBinaryOp (J.LE, PURE)
               | Primitives.Word_GE _ => doBinaryOp (J.GE, PURE)
               | Primitives.Word_notb Primitives.W32 =>
                   doUnaryExp
                     (fn a => J.ToUint32Exp (J.UnaryExp (J.BITNOT, a)), PURE)
               | Primitives.Word_andb Primitives.W32 =>
                   doBinaryExp
                     ( fn (a, b) => J.ToUint32Exp (J.BinExp (J.BITAND, a, b))
                     , PURE
                     )
               | Primitives.Word_orb Primitives.W32 =>
                   doBinaryExp
                     ( fn (a, b) => J.ToUint32Exp (J.BinExp (J.BITOR, a, b))
                     , PURE
                     )
               | Primitives.Word_xorb Primitives.W32 =>
                   doBinaryExp
                     ( fn (a, b) => J.ToUint32Exp (J.BinExp (J.BITXOR, a, b))
                     , PURE
                     )
               | Primitives.Word_LSHIFT_unchecked
                   (Primitives.W32, Primitives.W32) =>
                   doBinaryExp
                     ( fn (a, b) => J.ToUint32Exp (J.BinExp (J.LSHIFT, a, b))
                     , PURE
                     )
               | Primitives.Word_RSHIFT_unchecked
                   (Primitives.W32, Primitives.W32) =>
                   doBinaryOp (J.URSHIFT, PURE)
               | Primitives.Real_PLUS => doBinaryOp (J.PLUS, PURE)
               | Primitives.Real_MINUS => doBinaryOp (J.MINUS, PURE)
               | Primitives.Real_TIMES => doBinaryOp (J.TIMES, PURE)
               | Primitives.Real_DIVIDE => doBinaryOp (J.DIV, PURE)
               | Primitives.Real_TILDE =>
                   doUnaryExp (fn a => J.UnaryExp (J.NEGATE, a), PURE)
               | Primitives.Real_abs =>
                   doUnaryExp
                     ( fn a =>
                         J.CallExp
                           (J.VarExp (J.PredefinedId "Math_abs"), vector [a])
                     , PURE
                     )
               | Primitives.Real_LT => doBinaryOp (J.LT, PURE)
               | Primitives.Real_GT => doBinaryOp (J.GT, PURE)
               | Primitives.Real_LE => doBinaryOp (J.LE, PURE)
               | Primitives.Real_GE => doBinaryOp (J.GE, PURE)
               | Primitives.Char_EQUAL => doBinaryOp (J.EQUAL, PURE)
               | Primitives.Char_LT => doBinaryOp (J.LT, PURE)
               | Primitives.Char_GT => doBinaryOp (J.GT, PURE)
               | Primitives.Char_LE => doBinaryOp (J.LE, PURE)
               | Primitives.Char_GE => doBinaryOp (J.GE, PURE)
               | Primitives.Char_ord Primitives.I32 =>
                   doUnaryExp (fn a => a, PURE) (* no-op *)
               | Primitives.Char_ord Primitives.I54 =>
                   doUnaryExp (fn a => a, PURE) (* no-op *)
               | Primitives.Char_chr_unchecked Primitives.I32 =>
                   doUnaryExp (fn a => a, PURE) (* no-op *)
               | Primitives.Char_chr_unchecked Primitives.I54 =>
                   doUnaryExp (fn a => a, PURE) (* no-op *)
               | Primitives.Char16_EQUAL => doBinaryOp (J.EQUAL, PURE)
               | Primitives.Char16_LT => doBinaryOp (J.LT, PURE)
               | Primitives.Char16_GT => doBinaryOp (J.GT, PURE)
               | Primitives.Char16_LE => doBinaryOp (J.LE, PURE)
               | Primitives.Char16_GE => doBinaryOp (J.GE, PURE)
               | Primitives.Char16_ord Primitives.I32 =>
                   doUnaryExp (fn a => a, PURE) (* no-op *)
               | Primitives.Char16_ord Primitives.I54 =>
                   doUnaryExp (fn a => a, PURE) (* no-op *)
               | Primitives.Char16_chr_unchecked Primitives.I54 =>
                   doUnaryExp (fn a => a, PURE) (* no-op *)
               | Primitives.Char16_chr_unchecked Primitives.I32 =>
                   doUnaryExp (fn a => a, PURE) (* no-op *)
               | Primitives.String_EQUAL =>
                   doBinaryExp
                     ( fn (a, b) =>
                         J.CallExp
                           ( J.VarExp (J.PredefinedId "_String_EQUAL")
                           , vector [a, b]
                           )
                     , PURE
                     )
               | Primitives.String_LT =>
                   doBinaryExp
                     ( fn (a, b) =>
                         J.CallExp
                           ( J.VarExp (J.PredefinedId "_String_LT")
                           , vector [a, b]
                           )
                     , PURE
                     )
               | Primitives.String_HAT =>
                   doBinaryExp
                     ( fn (a, b) =>
                         J.CallExp
                           ( J.VarExp (J.PredefinedId "_String_append")
                           , vector [a, b]
                           )
                     , PURE
                     )
               | Primitives.String_size Primitives.I54 =>
                   doUnaryExp
                     ( fn a =>
                         J.IndexExp
                           (a, J.ConstExp (J.asciiStringAsWide "length"))
                     , PURE
                     )
               | Primitives.String_str =>
                   doUnaryExp
                     ( fn a =>
                         J.MethodExp
                           ( J.VarExp (J.PredefinedId "Uint8Array")
                           , "of"
                           , vector [a]
                           )
                     , PURE
                     )
               | Primitives.String_concat =>
                   doUnaryExp
                     ( fn a =>
                         J.CallExp
                           ( J.VarExp (J.PredefinedId "_String_concat")
                           , vector [a]
                           )
                     , PURE
                     )
               | Primitives.String_implode =>
                   doUnaryExp
                     ( fn a =>
                         J.CallExp
                           ( J.VarExp (J.PredefinedId "_String_implode")
                           , vector [a]
                           )
                     , PURE
                     )
               | Primitives.String16_EQUAL => doBinaryOp (J.EQUAL, PURE)
               | Primitives.String16_LT => doBinaryOp (J.LT, PURE)
               | Primitives.String16_GT => doBinaryOp (J.GT, PURE)
               | Primitives.String16_LE => doBinaryOp (J.LE, PURE)
               | Primitives.String16_GE => doBinaryOp (J.GE, PURE)
               | Primitives.String16_HAT => doBinaryOp (J.PLUS, PURE)
               | Primitives.String16_size Primitives.I54 =>
                   doUnaryExp
                     ( fn a =>
                         J.IndexExp
                           (a, J.ConstExp (J.asciiStringAsWide "length"))
                     , PURE
                     )
               | Primitives.String16_str =>
                   doUnaryExp
                     ( fn a =>
                         J.MethodExp
                           ( J.VarExp (J.PredefinedId "String")
                           , "fromCharCode"
                           , vector [a]
                           )
                     , PURE
                     )
               | Primitives.Int_PLUS Primitives.INT_INF =>
                   doBinaryOp (J.PLUS, PURE)
               | Primitives.Int_MINUS Primitives.INT_INF =>
                   doBinaryOp (J.MINUS, PURE)
               | Primitives.Int_TIMES Primitives.INT_INF =>
                   doBinaryOp (J.TIMES, PURE)
               | Primitives.Int_TILDE Primitives.INT_INF =>
                   doUnaryExp (fn a => J.UnaryExp (J.NEGATE, a), PURE)
               | Primitives.IntInf_andb => doBinaryOp (J.BITAND, PURE)
               | Primitives.IntInf_orb => doBinaryOp (J.BITOR, PURE)
               | Primitives.IntInf_xorb => doBinaryOp (J.BITXOR, PURE)
               | Primitives.IntInf_notb =>
                   doUnaryExp (fn a => J.UnaryExp (J.BITNOT, a), PURE)
               | Primitives.Int_quot_unchecked Primitives.INT_INF =>
                   doBinaryOp (J.DIV, PURE)
               | Primitives.Int_rem_unchecked Primitives.INT_INF =>
                   doBinaryOp (J.MOD, PURE)
               | Primitives.Vector_length Primitives.I54 =>
                   doUnaryExp
                     ( fn a =>
                         J.IndexExp
                           (a, J.ConstExp (J.asciiStringAsWide "length"))
                     , PURE
                     )
               | Primitives.Vector_fromList =>
                   doUnaryExp
                     ( fn xs =>
                         J.CallExp
                           ( J.VarExp (J.PredefinedId "_VectorOrArray_fromList")
                           , vector [xs]
                           )
                     , PURE
                     )
               | Primitives.Vector_concat =>
                   doUnaryExp
                     ( fn a =>
                         J.CallExp
                           ( J.VarExp (J.PredefinedId "_Vector_concat")
                           , vector [a]
                           )
                     , IMPURE
                     )
               | Primitives.Vector_unsafeFromListRevN Primitives.I54 =>
                   doBinaryExp
                     ( fn (n, xs) =>
                         J.CallExp
                           ( J.VarExp
                               (J.PredefinedId "_Vector_unsafeFromListRevN")
                           , vector [n, xs]
                           )
                     , PURE
                     )
               | Primitives.Array_EQUAL => doBinaryOp (J.EQUAL, PURE)
               | Primitives.Array_length Primitives.I54 =>
                   doUnaryExp
                     ( fn a =>
                         J.IndexExp
                           (a, J.ConstExp (J.asciiStringAsWide "length"))
                     , PURE
                     )
               | Primitives.Array_fromList =>
                   doUnaryExp
                     ( fn xs =>
                         J.CallExp
                           ( J.VarExp (J.PredefinedId "_VectorOrArray_fromList")
                           , vector [xs]
                           )
                     , PURE
                     )
               | Primitives.Array_array Primitives.I54 =>
                   doBinaryExp
                     ( fn (n, init) =>
                         J.CallExp
                           ( J.VarExp (J.PredefinedId "_Array_array")
                           , vector [n, init]
                           )
                     , IMPURE
                     )
               | Primitives.Unsafe_cast => doUnaryExp (fn a => a, PURE)
               | Primitives.Unsafe_Vector_sub Primitives.I54 =>
                   doBinaryExp (fn (vec, i) => J.IndexExp (vec, i), PURE)
               | Primitives.Unsafe_Array_sub Primitives.I54 =>
                   doBinaryExp (fn (arr, i) => J.IndexExp (arr, i), DISCARDABLE)
               | Primitives.Unsafe_Array_update Primitives.I54 =>
                   raise CodeGenError "unexpected Unsafe.Array.update"
               | Primitives.Exception_instanceof =>
                   doBinaryExp
                     (fn (e, tag) => J.BinExp (J.INSTANCEOF, e, tag), PURE)
               | Primitives.JavaScript_sub =>
                   doBinaryExpE (fn (a, b) => J.IndexExp (a, b))
               | Primitives.JavaScript_set =>
                   raise CodeGenError "unexpected JavaScript.set"
               | Primitives.JavaScript_EQUAL => doBinaryOp (J.EQUAL, PURE)
               | Primitives.JavaScript_NOTEQUAL => doBinaryOp (J.NOTEQUAL, PURE)
               | Primitives.JavaScript_LT => doBinaryOpE J.LT
               | Primitives.JavaScript_GT => doBinaryOpE J.GT
               | Primitives.JavaScript_LE => doBinaryOpE J.LE
               | Primitives.JavaScript_GE => doBinaryOpE J.GE
               | Primitives.JavaScript_PLUS => doBinaryOpE J.PLUS
               | Primitives.JavaScript_MINUS => doBinaryOpE J.MINUS
               | Primitives.JavaScript_TIMES => doBinaryOpE J.TIMES
               | Primitives.JavaScript_DIVIDE => doBinaryOpE J.DIV
               | Primitives.JavaScript_MOD => doBinaryOpE J.MOD
               | Primitives.JavaScript_negate =>
                   doUnaryExpE (fn a => J.UnaryExp (J.NEGATE, a))
               | Primitives.JavaScript_andb => doBinaryOpE J.BITAND
               | Primitives.JavaScript_orb => doBinaryOpE J.BITOR
               | Primitives.JavaScript_xorb => doBinaryOpE J.BITXOR
               | Primitives.JavaScript_notb =>
                   doUnaryExpE (fn a => J.UnaryExp (J.BITNOT, a))
               | Primitives.JavaScript_LSHIFT => doBinaryOpE J.LSHIFT
               | Primitives.JavaScript_RSHIFT => doBinaryOpE J.RSHIFT
               | Primitives.JavaScript_URSHIFT => doBinaryOpE J.URSHIFT
               | Primitives.JavaScript_EXP => doBinaryOpE J.EXP
               | Primitives.JavaScript_isFalsy =>
                   doUnaryExp (fn a => J.UnaryExp (J.NOT, a), DISCARDABLE)
               | Primitives.JavaScript_isNullOrUndefined =>
                   doUnaryExp
                     (fn a => J.BinExp (J.LAXEQUAL, a, J.ConstExp J.Null), PURE)
               | Primitives.JavaScript_typeof =>
                   doUnaryExp (fn a => J.UnaryExp (J.TYPEOF, a), PURE)
               | Primitives.JavaScript_global =>
                   doUnaryExp
                     ( fn a =>
                         J.IndexExp (J.VarExp (J.PredefinedId "globalThis"), a)
                     , DISCARDABLE
                     )
               | Primitives.JavaScript_setGlobal =>
                   raise CodeGenError "unexpected JavaScript.setGlobal"
               | Primitives.JavaScript_call =>
                   doTernary (fn (f, args, _) =>
                     J.MethodExp (f, "apply", vector [J.UndefinedExp, args]))
               | Primitives.JavaScript_method =>
                   doTernary (fn (obj, method, args) =>
                     J.MethodExp
                       (J.IndexExp (obj, method), "apply", vector [obj, args]))
               | Primitives.JavaScript_new =>
                   doTernary (fn (ctor, args, _) =>
                     J.MethodExp
                       ( J.VarExp (J.PredefinedId "Reflect")
                       , "construct"
                       , vector [ctor, args]
                       ))
               | Primitives.JavaScript_function =>
                   doUnary (fn f =>
                     J.CallExp
                       (J.VarExp (J.PredefinedId "_function"), vector [f]))
               | Primitives.JavaScript_encodeUtf8 =>
                   doUnary (fn f =>
                     J.CallExp
                       (J.VarExp (J.PredefinedId "_encodeUtf8"), vector [f]))
               | Primitives.JavaScript_decodeUtf8 =>
                   doUnary (fn f =>
                     J.CallExp
                       (J.VarExp (J.PredefinedId "_decodeUtf8"), vector [f]))
               | Primitives.DelimCont_newPromptTag =>
                   doNullaryExp
                     ( fn () =>
                         J.NewExp
                           (J.VarExp (J.PredefinedId "_PromptTag"), vector [])
                     , DISCARDABLE
                     )
               | _ =>
                   raise CodeGenError
                     ("primop " ^ Primitives.toString prim
                      ^ " is not supported on JavaScript backend")
             end
         | (F.JsCallOp, _ :: f :: args) =>
             J.CallExp
               ( doExp (ctx, env, f)
               , Vector.map (fn x => doExp (ctx, env, x)) (vector args)
               )
         | (F.JsMethodOp, _ :: obj :: name :: args) =>
             J.CallExp
               ( J.IndexExp (doExp (ctx, env, obj), doExp (ctx, env, name))
               , Vector.map (fn x => doExp (ctx, env, x)) (vector args)
               )
         | (F.JsNewOp, _ :: ctor :: args) =>
             J.NewExp
               ( doExp (ctx, env, ctor)
               , Vector.map (fn x => doExp (ctx, env, x)) (vector args)
               )
         | _ =>
             raise CodeGenError
               ("primop " ^ Printer.build (FPrinter.doPrimOp primOp)
                ^ " is not supported on JavaScript backend"))
    | doExp (ctx, env, N.Record fields) =
        let
          val fields =
            Syntax.LabelMap.foldri
              (fn (label, x, acc) => (label, doExp (ctx, env, x)) :: acc) []
              fields
          fun isTuple (_, []) = true
            | isTuple (i, (Syntax.NumericLabel n, _) :: xs) =
                i = n andalso isTuple (i + 1, xs)
            | isTuple _ = false
        in
          if isTuple (1, fields) then
            J.ArrayExp (vector (List.map #2 fields))
          else
            J.ObjectExp (vector
              (List.map (fn (label, exp) => (LabelToObjectKey label, exp))
                 fields))
        end
    | doExp (ctx, env, N.ExnTag {name, payloadTy}) =
        raise CodeGenError "unexpected ExnTag"
    (*
               (case result of
                  SOME result =>
                    let
                      val stmts =
                        [ let
                            val value =
                              case payloadTy of
                                NONE => J.FunctionExp (vector [], vector [])
                              | SOME _ =>
                                  J.FunctionExp
                                    ( vector [J.PredefinedId "payload"]
                                    , vector
                                        [J.AssignStat
                                           ( J.IndexExp
                                               ( J.ThisExp
                                               , J.ConstExp
                                                   (J.asciiStringAsWide
                                                      "payload")
                                               )
                                           , J.VarExp (J.PredefinedId "payload")
                                           )]
                                    )
                          in
                            ConstStat (result, value)
                          end
                        , J.AssignStat
                            ( J.IndexExp
                                ( J.IndexExp
                                    ( J.VarExp (J.UserDefinedId result)
                                    , J.ConstExp
                                        (J.asciiStringAsWide "prototype")
                                    )
                                , J.ConstExp (J.asciiStringAsWide "__isMLExn")
                                )
                            , J.ConstExp J.True
                            )
                        , J.AssignStat
                            ( J.IndexExp
                                ( J.IndexExp
                                    ( J.VarExp (J.UserDefinedId result)
                                    , J.ConstExp
                                        (J.asciiStringAsWide "prototype")
                                    )
                                , J.ConstExp (J.asciiStringAsWide "name")
                                )
                            , J.ConstExp (J.asciiStringAsWide name)
                            )
                        ]
                    in
                      doDecs
                        ( ctx
                        , env
                        , decs
                        , finalExp
                        , List.revAppend (stmts, revStats)
                        )
                    end
                | NONE => doDecs (ctx, env, decs, finalExp, revStats))
                *)
    | doExp (ctx, env, N.Projection {label, record, fieldTypes = _}) =
        let
          val label =
            case label of
              Syntax.NumericLabel n =>
                J.ConstExp (J.Numeral (Int.toString (n - 1))) (* non-negative *)
            | Syntax.IdentifierLabel s => J.ConstExp (J.asciiStringAsWide s)
        in
          J.IndexExp (doExp (ctx, env, record), label)
        end
    | doExp (ctx, env, N.Abs {contParam, params, body, attr = _}) =
        (case #style ctx of
           Backend.DIRECT_STYLE =>
             if CpsAnalyze.escapes (#contEscapeMap ctx, contParam) then
               let
                 val env' =
                   { continuations =
                       C.CVarMap.singleton (contParam, RETURN_TRAMPOLINE)
                   , subst = #subst env
                   }
               in
                 J.CallExp (J.VarExp (J.PredefinedId "_wrap"), vector
                   [J.FunctionExp
                      ( Vector.map (VIdToJs ctx) (vector params)
                      , vector (doStat ctx env' body)
                      )])
               end
             else
               let
                 val env' =
                   { continuations =
                       C.CVarMap.singleton (contParam, RETURN_SIMPLE)
                   , subst = #subst env
                   }
               in
                 J.FunctionExp (Vector.map (VIdToJs ctx) (vector params), vector
                   (doStat ctx env' body))
               end
         | Backend.CPS =>
             let
               val env' =
                 { continuations =
                     C.CVarMap.singleton (contParam, TAILCALL contParam)
                 , subst = #subst env
                 }
             in
               J.FunctionExp
                 ( vector (CVarToJs contParam :: List.map (VIdToJs ctx) params)
                 , vector (doStat ctx env' body)
                 )
             end)
    | doExp (ctx, env, N.LogicalAnd (x, y)) =
        J.BinExp (J.AND, doExp (ctx, env, x), doExp (ctx, env, y))
    | doExp (ctx, env, N.LogicalOr (x, y)) =
        J.BinExp (J.OR, doExp (ctx, env, x), doExp (ctx, env, y))
  and doDecs (ctx, env, decs, finalExp, revStats) =
    (case decs of
       [] => List.revAppend (revStats, doStat ctx env finalExp)
     | dec :: decs =>
         let
           fun pure (NONE, _) =
                 doDecs (ctx, env, decs, finalExp, revStats)
             | pure (SOME result, exp) =
                 doDecs
                   ( ctx
                   , env
                   , decs
                   , finalExp
                   , ConstStat (result, exp) :: revStats
                   )
           fun discardable (NONE, _) =
                 doDecs (ctx, env, decs, finalExp, revStats)
             | discardable (SOME result, exp) =
                 doDecs
                   ( ctx
                   , env
                   , decs
                   , finalExp
                   , ConstStat (result, exp) :: revStats
                   )
           fun impure (NONE, exp) =
                 doDecs (ctx, env, decs, finalExp, J.ExpStat exp :: revStats)
             | impure (SOME result, exp) =
                 doDecs
                   ( ctx
                   , env
                   , decs
                   , finalExp
                   , ConstStat (result, exp) :: revStats
                   )
         in
           case dec of
             N.ValDec
               { exp = N.PrimOp {primOp as F.PrimCall prim, tyargs, args}
               , results
               } =>
               let
                 fun action ([], stmt) =
                       doDecs (ctx, env, decs, finalExp, stmt :: revStats)
                   | action (_ :: _, stmt) =
                       raise CodeGenError
                         ("primop " ^ Primitives.toString prim
                          ^ ": unexpected number of results for an action")
                 fun doNullary f =
                   case args of
                     [] => f ()
                   | _ =>
                       raise CodeGenError
                         ("primop " ^ Primitives.toString prim
                          ^ ": invalid number of arguments")
                 fun doNullaryExp (f, purity) =
                   doNullary (fn () =>
                     case results of
                       [result] =>
                         (case purity of
                            PURE => pure (result, f ())
                          | DISCARDABLE => discardable (result, f ())
                          | IMPURE => impure (result, f ()))
                     | _ => raise CodeGenError "unexpected number of results")
                 fun doUnary f =
                   case args of
                     [a] => f (doExp (ctx, env, a))
                   | _ =>
                       raise CodeGenError
                         ("primop " ^ Primitives.toString prim
                          ^ ": invalid number of arguments")
                 fun doUnaryExp (f, purity) =
                   doUnary (fn a =>
                     case results of
                       [result] =>
                         (case purity of
                            PURE => pure (result, f a)
                          | DISCARDABLE => discardable (result, f a)
                          | IMPURE => impure (result, f a))
                     | _ => raise CodeGenError "unexpected number of results")
                 fun doBinary f =
                   case args of
                     [a, b] => f (doExp (ctx, env, a), doExp (ctx, env, b))
                   | _ =>
                       raise CodeGenError
                         ("primop " ^ Primitives.toString prim
                          ^ ": invalid number of arguments")
                 fun doBinaryExp (f, purity) =
                   doBinary (fn (a, b) =>
                     case results of
                       [result] =>
                         (case purity of
                            PURE => pure (result, f (a, b))
                          | DISCARDABLE => discardable (result, f (a, b))
                          | IMPURE => impure (result, f (a, b)))
                     | _ => raise CodeGenError "unexpected number of results")
                 fun doBinaryOp (binop, pure) =
                   doBinaryExp (fn (a, b) => J.BinExp (binop, a, b), pure)
                 fun doTernary f =
                   case args of
                     [a, b, c] =>
                       f ( doExp (ctx, env, a)
                         , doExp (ctx, env, b)
                         , doExp (ctx, env, c)
                         )
                   | _ =>
                       raise CodeGenError
                         ("primop " ^ Primitives.toString prim
                          ^ ": invalid number of arguments")
               in
                 case prim of
                   Primitives.Ref_set =>
                     doBinary (fn (a, b) =>
                       action (results, J.AssignStat
                         (J.IndexExp (a, J.ConstExp (J.Numeral "0")), b))) (* REPRESENTATION_OF_REF *)
                 | Primitives.Unsafe_Array_update Primitives.I54 =>
                     doTernary (fn (arr, i, v) =>
                       action (results, J.AssignStat (J.IndexExp (arr, i), v)))
                 | Primitives.JavaScript_set =>
                     doTernary (fn (a, b, c) =>
                       action (results, J.AssignStat (J.IndexExp (a, b), c)))
                 | Primitives.JavaScript_setGlobal =>
                     doBinary (fn (name, value) =>
                       action (results, J.AssignStat
                         ( J.IndexExp
                             (J.VarExp (J.PredefinedId "globalThis"), name)
                         , value
                         )))
                 | _ =>
                     (case results of
                        [result] =>
                          impure (result, doExp
                            ( ctx
                            , env
                            , N.PrimOp
                                {primOp = primOp, tyargs = tyargs, args = args}
                            ))
                      | _ => raise CodeGenError "unexpected number of results")
               end
           | N.ValDec {exp = N.ExnTag {name, payloadTy}, results = [result]} =>
               (case result of
                  SOME result =>
                    let
                      val stmts =
                        [ let
                            val value =
                              case payloadTy of
                                NONE => J.FunctionExp (vector [], vector [])
                              | SOME _ =>
                                  J.FunctionExp
                                    ( vector [J.PredefinedId "payload"]
                                    , vector
                                        [J.AssignStat
                                           ( J.IndexExp
                                               ( J.ThisExp
                                               , J.ConstExp
                                                   (J.asciiStringAsWide
                                                      "payload")
                                               )
                                           , J.VarExp (J.PredefinedId "payload")
                                           )]
                                    )
                          in
                            ConstStat (result, value)
                          end
                        , J.AssignStat
                            ( J.IndexExp
                                ( J.IndexExp
                                    ( J.VarExp (J.UserDefinedId result)
                                    , J.ConstExp
                                        (J.asciiStringAsWide "prototype")
                                    )
                                , J.ConstExp (J.asciiStringAsWide "__isMLExn")
                                )
                            , J.ConstExp J.True
                            )
                        , J.AssignStat
                            ( J.IndexExp
                                ( J.IndexExp
                                    ( J.VarExp (J.UserDefinedId result)
                                    , J.ConstExp
                                        (J.asciiStringAsWide "prototype")
                                    )
                                , J.ConstExp (J.asciiStringAsWide "name")
                                )
                            , J.ConstExp (J.asciiStringAsWide name)
                            )
                        ]
                    in
                      doDecs
                        ( ctx
                        , env
                        , decs
                        , finalExp
                        , List.revAppend (stmts, revStats)
                        )
                    end
                | NONE => doDecs (ctx, env, decs, finalExp, revStats))
           | N.ValDec {exp, results} =>
               (case results of
                  [result] => impure (result, doExp (ctx, env, exp))
                | _ => raise CodeGenError "unexpected number of results")
           | N.RecDec defs =>
               (case #style ctx of
                  Backend.DIRECT_STYLE =>
                    let
                      val (decs', assignments) =
                        List.foldr
                          (fn ( {name, contParam, params, body, attr = _}
                              , (decs, assignments)
                              ) =>
                             if
                               CpsAnalyze.escapes
                                 (#contEscapeMap ctx, contParam)
                             then
                               let
                                 val env' =
                                   { continuations =
                                       C.CVarMap.singleton
                                         (contParam, RETURN_TRAMPOLINE)
                                   , subst = #subst env
                                   }
                               in
                                 ( J.LetStat (vector [(name, NONE)]) :: decs
                                 , J.AssignStat
                                     ( J.VarExp (J.UserDefinedId name)
                                     , J.CallExp
                                         ( J.VarExp (J.PredefinedId "_wrap")
                                         , vector
                                             [J.FunctionExp
                                                ( Vector.map (VIdToJs ctx)
                                                    (vector params)
                                                , vector (doStat ctx env' body)
                                                )]
                                         )
                                     ) :: assignments
                                 )
                               end
                             else
                               let
                                 val env' =
                                   { continuations =
                                       C.CVarMap.singleton
                                         (contParam, RETURN_SIMPLE)
                                   , subst = #subst env
                                   }
                               in
                                 ( J.LetStat (vector [(name, NONE)]) :: decs
                                 , J.AssignStat
                                     ( J.VarExp (J.UserDefinedId name)
                                     , J.FunctionExp
                                         ( Vector.map (VIdToJs ctx)
                                             (vector params)
                                         , vector (doStat ctx env' body)
                                         )
                                     ) :: assignments
                                 )
                               end) ([], []) defs
                    in
                      doDecs
                        ( ctx
                        , env
                        , decs
                        , finalExp
                        , List.revAppend (decs' @ assignments, revStats)
                        )
                    end
                | Backend.CPS =>
                    let
                      val (decs', assignments) =
                        List.foldr
                          (fn ( {name, contParam, params, body, attr = _}
                              , (decs, assignments)
                              ) =>
                             let
                               val env' =
                                 { continuations =
                                     C.CVarMap.singleton
                                       (contParam, TAILCALL contParam)
                                 , subst = #subst env
                                 }
                             in
                               ( J.LetStat (vector [(name, NONE)]) :: decs
                               , J.AssignStat
                                   ( J.VarExp (J.UserDefinedId name)
                                   , J.FunctionExp
                                       ( vector
                                           (CVarToJs contParam
                                            :: List.map (VIdToJs ctx) params)
                                       , vector (doStat ctx env' body)
                                       )
                                   ) :: assignments
                               ) (* in fact, ConstStat can be used *)
                             end) ([], []) defs
                    in
                      doDecs
                        ( ctx
                        , env
                        , decs
                        , finalExp
                        , List.revAppend (decs' @ assignments, revStats)
                        )
                    end)
           | N.ContDec {name, params, body} =>
               let
                 val escape =
                   case #style ctx of
                     Backend.DIRECT_STYLE => false
                   | Backend.CPS =>
                       CpsAnalyze.escapesTransitively (#contEscapeMap ctx, name)
               in
                 if escape then
                   let
                     val dec =
                       ConstStat
                         ( CVarToId name
                         , J.FunctionExp
                             ( vector
                                 (List.map
                                    (fn SOME p => VIdToJs ctx p
                                      | NONE => VIdToJs ctx (genSym ctx)) params)
                             , vector (doStat ctx env body)
                             )
                         )
                     val env' =
                       { continuations =
                           C.CVarMap.insert
                             (#continuations env, name, TAILCALL name)
                       , subst = #subst env
                       }
                   in
                     doDecs (ctx, env', decs, finalExp, dec :: revStats)
                   end
                 else
                   case (decs, finalExp, params) of
                     ([], N.App {applied, cont, args, attr = _}, [SOME result]) =>
                       if cont = name then
                         List.revAppend
                           ( revStats
                           , ConstStat (result, J.CallExp
                               ( doExp (ctx, env, applied)
                               , Vector.map (fn x => doExp (ctx, env, x))
                                   (vector args)
                               )) :: doStat ctx env body
                           )
                       else
                         List.revAppend
                           ( revStats
                           , doStat ctx env finalExp
                           ) (* dead continuation elimination *)
                   | _ =>
                       let
                         val dec =
                           let
                             val params' =
                               List.mapPartial (Option.map (fn p => (p, NONE)))
                                 params
                           in
                             if List.null params' then []
                             else [J.LetStat (vector params')]
                           end
                         val newEnv =
                           { continuations =
                               C.CVarMap.insert
                                 ( #continuations env
                                 , name
                                 , BREAK_TO
                                     { label = CVarToJs name
                                     , which = NONE
                                     , params =
                                         List.map (Option.map (VIdToJs ctx))
                                           params
                                     }
                                 )
                           , subst = #subst env
                           }
                       in
                         List.revAppend
                           ( revStats
                           , dec
                             @
                             J.BlockStat (SOME (CVarToJs name), vector (doDecs
                               (ctx, newEnv, decs, finalExp, [])))
                             :: doStat ctx env body
                           )
                       end
               end
           | N.RecContDec defs =>
               let
                 val escape =
                   case #style ctx of
                     Backend.DIRECT_STYLE => false
                   | Backend.CPS =>
                       List.exists
                         (fn (name, _, _) =>
                            CpsAnalyze.escapesTransitively
                              (#contEscapeMap ctx, name)) defs
               in
                 if escape then
                   let
                     val env' =
                       { continuations =
                           List.foldl
                             (fn ((name, _, _), e) =>
                                C.CVarMap.insert (e, name, TAILCALL name))
                             (#continuations env) defs
                       , subst = #subst env
                       }
                     val (decs', assignments) =
                       List.foldr
                         (fn ((name, params, body), (decs, assignments)) =>
                            ( J.LetStat (vector [(CVarToId name, NONE)]) :: decs
                            , J.AssignStat
                                ( doCVar name
                                , J.FunctionExp
                                    ( vector
                                        (List.map
                                           (fn SOME p => VIdToJs ctx p
                                             | NONE => VIdToJs ctx (genSym ctx))
                                           params)
                                    , vector (doStat ctx env' body)
                                    )
                                ) :: assignments
                            ) (* in fact, ConstStat can be used *)) ([], [])
                         defs
                   in
                     doDecs
                       ( ctx
                       , env'
                       , decs
                       , finalExp
                       , List.revAppend (decs' @ assignments, revStats)
                       )
                   end
                 else
                   let
                     datatype init =
                       INIT_WITH_VALUES of
                         int * (C.Var option list) * N.Exp list
                     | NO_INIT
                     val init =
                       case (decs, finalExp) of
                         ([], N.AppCont {applied, args}) =>
                           let
                             fun find (_, []) = NO_INIT
                               | find (i, (name, params, _) :: xs) =
                                   if name = applied then
                                     INIT_WITH_VALUES (i, params, args)
                                   else
                                     find (i + 1, xs)
                           in
                             find (0, defs)
                           end
                       | _ => NO_INIT
                     val loopLabel = J.UserDefinedId (genSymNamed (ctx, "loop"))
                     datatype needs_which =
                       NEED_WHICH of J.Id
                     | NO_WHICH of C.CVar * (C.Var option) list * N.Stat
                     val maxargs =
                       List.foldl
                         (fn ((_, params, _), n) =>
                            Int.max (n, List.length
                              (List.filter Option.isSome params))) 0 defs
                     val commonParams = List.tabulate (maxargs, fn _ =>
                       genSym ctx)
                     fun mapCommonParams params =
                       List.rev (#2
                         (List.foldl
                            (fn (SOME _, (c :: rest, acc)) =>
                               (rest, SOME c :: acc)
                              | (_, (rest, acc)) => (rest, NONE :: acc))
                            (commonParams, []) params))
                     val (optWhich, vars) =
                       case defs of
                         [def] => (NO_WHICH def, commonParams)
                       | _ =>
                           let
                             val which = genSym ctx
                             val which' = J.UserDefinedId which
                           in
                             (NEED_WHICH which', which :: commonParams)
                           end
                     val (n, recEnvC) =
                       List.foldl
                         (fn ((name, params, _), (i, e)) =>
                            let
                              val cont = CONTINUE_TO
                                { label = loopLabel
                                , which =
                                    case optWhich of
                                      NO_WHICH _ => NONE
                                    | NEED_WHICH which' =>
                                        SOME
                                          (which', J.Numeral (Int.toString i))
                                , params =
                                    List.map (Option.map (VIdToJs ctx))
                                      (mapCommonParams params)
                                }
                            in
                              (i + 1, C.CVarMap.insert (e, name, cont))
                            end) (0, #continuations env) defs
                     val recEnv = {continuations = recEnvC, subst = #subst env}
                     val initAndRest =
                       case init of
                         INIT_WITH_VALUES (initialWhich, params, args) =>
                           let
                             val args =
                               ListPair.foldrEq
                                 (fn (SOME _, a, acc) => a :: acc
                                   | (NONE, _, acc) => acc) [] (params, args)
                             val args' =
                               case optWhich of
                                 NO_WHICH _ =>
                                   List.map (fn x => doExp (ctx, env, x)) args
                               | NEED_WHICH _ =>
                                   J.ConstExp
                                     (J.Numeral (Int.toString initialWhich))
                                   ::
                                   List.map (fn x => doExp (ctx, env, x)) args
                           in
                             if List.null args' then
                               []
                             else
                               [J.LetStat (vector
                                  (ListPair.map (fn (p, a) => (p, SOME a))
                                     (vars, args')))]
                           end
                       | NO_INIT =>
                           let
                             val blockLabel = J.UserDefinedId (genSym ctx)
                             val (_, contEnvC) =
                               List.foldl
                                 (fn ((name, params, _), (i, e)) =>
                                    let
                                      val cont = BREAK_TO
                                        { label = blockLabel
                                        , which =
                                            case optWhich of
                                              NO_WHICH _ => NONE
                                            | NEED_WHICH which' =>
                                                SOME
                                                  ( which'
                                                  , J.Numeral (Int.toString i)
                                                  )
                                        , params =
                                            List.map (Option.map (VIdToJs ctx))
                                              (mapCommonParams params)
                                        }
                                    in
                                      (i + 1, C.CVarMap.insert (e, name, cont))
                                    end) (0, #continuations env) defs
                             val contEnv =
                               {continuations = contEnvC, subst = #subst env}
                           in
                             [ J.LetStat (vector
                                 (List.map (fn p => (p, NONE)) vars))
                             , J.BlockStat (SOME blockLabel, vector (doDecs
                                 (ctx, contEnv, decs, finalExp, [])))
                             ]
                           end
                   in
                     List.revAppend
                       ( revStats
                       , initAndRest
                         @
                         [J.LoopStat
                            ( SOME loopLabel
                            , case optWhich of
                                NO_WHICH (_, params, body) =>
                                  let
                                    val dec =
                                      let
                                        val defs =
                                          ListPair.map
                                            (fn (p, c) =>
                                               (p, J.VarExp (J.UserDefinedId c)))
                                            ( List.mapPartial (fn x => x) params
                                            , commonParams
                                            )
                                      in
                                        if List.null defs then []
                                        else [J.ConstStat (vector defs)]
                                      end
                                  in
                                    vector (dec @ doStat ctx recEnv body)
                                  end
                              | NEED_WHICH which' =>
                                  vector
                                    [J.SwitchStat (J.VarExp which', #2
                                       (List.foldr
                                          (fn ((_, params, body), (i, cases)) =>
                                             let
                                               val i = i - 1
                                               val dec =
                                                 let
                                                   val defs =
                                                     ListPair.map
                                                       (fn (p, c) =>
                                                          ( p
                                                          , J.VarExp
                                                              (J.UserDefinedId c)
                                                          ))
                                                       ( List.mapPartial
                                                           (fn x => x) params
                                                       , commonParams
                                                       )
                                                 in
                                                   if List.null defs then
                                                     []
                                                   else
                                                     [J.ConstStat (vector defs)]
                                                 end
                                             in
                                               ( i
                                               , ( J.Numeral (Int.toString i)
                                                 , vector
                                                     (dec
                                                      @ doStat ctx recEnv body)
                                                 ) :: cases
                                               )
                                             end) (n, []) defs))]
                            )]
                       )
                   end
               end
           | N.ESImportDec {pure = _, specs, moduleName} =>
               let
                 val imports = !(#imports ctx)
                 fun go ([], env, acc) =
                       ( env
                       , List.revAppend
                           ( acc
                           , [{ specs =
                                  List.map
                                    (fn (name, vid) => (name, VIdToJs ctx vid))
                                    specs
                              , moduleName = moduleName
                              }]
                           )
                       )
                   | go
                       ( (i as {specs = specs', moduleName = moduleName'}) ::
                           imports'
                       , env
                       , acc
                       ) =
                       if moduleName' = moduleName then
                         let
                           fun loop ([], env, acc) = (env, acc)
                             | loop ((name, vid) :: ss, env, acc) =
                                 case
                                   List.find (fn (name', _) => name = name')
                                     specs'
                                 of
                                   SOME (_, vid') =>
                                     loop
                                       ( ss
                                       , TypedSyntax.VIdMap.insert
                                           (env, vid, J.VarExp vid')
                                       , acc
                                       )
                                 | NONE =>
                                     loop
                                       (ss, env, (name, VIdToJs ctx vid) :: acc)
                           val (env', specs'') = loop (specs, env, [])
                           val i' =
                             { specs = specs' @ specs''
                             , moduleName = moduleName'
                             }
                         in
                           (env', List.revAppend (acc, i' :: imports'))
                         end
                       else
                         go (imports', env, i :: acc)
                 val (subst', imports') = go (imports, #subst env, [])
                 val () = #imports ctx := imports'
                 val env' = {continuations = #continuations env, subst = subst'}
               in
                 doDecs (ctx, env', decs, finalExp, revStats)
               end
         end)
  and doStat (ctx: Context) (env: Env) (N.Let {decs, cont}) : J.Stat list =
        doDecs (ctx, env, decs, cont, [])
    | doStat ctx env (N.App {applied, cont, args, attr = _}) =
        (case C.CVarMap.find (#continuations env, cont) of
           SOME (TAILCALL k) =>
             [J.ReturnStat (SOME (J.ArrayExp (vector
                [ J.ConstExp J.False
                , doExp (ctx, env, applied)
                , J.ArrayExp (vector
                    (doCVar k :: List.map (fn x => doExp (ctx, env, x)) args))
                ])))] (* continuation passing style *)
         | SOME (BREAK_TO {label, which, params = [p]}) =>
             let
               val setWhich =
                 case which of
                   NONE => []
                 | SOME (whichVar, whichVal) =>
                     [J.AssignStat (J.VarExp whichVar, J.ConstExp whichVal)]
               val call =
                 case p of
                   SOME p =>
                     J.AssignStat (J.VarExp p, J.CallExp
                       ( doExp (ctx, env, applied)
                       , Vector.map (fn x => doExp (ctx, env, x)) (vector args)
                       ))
                 | NONE =>
                     J.ExpStat (J.CallExp
                       ( doExp (ctx, env, applied)
                       , Vector.map (fn x => doExp (ctx, env, x)) (vector args)
                       ))
             in
               call :: setWhich @ [J.BreakStat (SOME label)] (* direct style *)
             end
         | SOME (CONTINUE_TO {label, which, params = [p]}) =>
             let
               val setWhich =
                 case which of
                   NONE => []
                 | SOME (whichVar, whichVal) =>
                     [J.AssignStat (J.VarExp whichVar, J.ConstExp whichVal)]
               val call =
                 case p of
                   SOME p =>
                     J.AssignStat (J.VarExp p, J.CallExp
                       ( doExp (ctx, env, applied)
                       , Vector.map (fn x => doExp (ctx, env, x)) (vector args)
                       ))
                 | NONE =>
                     J.ExpStat (J.CallExp
                       ( doExp (ctx, env, applied)
                       , Vector.map (fn x => doExp (ctx, env, x)) (vector args)
                       ))
             in
               call
               :: setWhich @ [J.ContinueStat (SOME label)] (* direct style *)
             end
         | SOME RETURN_TRAMPOLINE =>
             [J.ReturnStat (SOME (J.ArrayExp (vector
                [ J.ConstExp J.False
                , doExp (ctx, env, applied)
                , J.ArrayExp
                    (Vector.map (fn x => doExp (ctx, env, x)) (vector args))
                ])))] (* direct style, tail call *)
         | SOME RETURN_SIMPLE =>
             raise CodeGenError "invalid RETURN_SIMPLE continuation"
         | _ => raise CodeGenError "invalid continuation")
    | doStat ctx env (N.AppCont {applied, args}) =
        applyCont
          (ctx, env, applied, List.map (fn x => doExp (ctx, env, x)) args)
    | doStat ctx env (N.If {cond, thenCont, elseCont}) =
        [J.IfStat
           ( doExp (ctx, env, cond)
           , vector (doStat ctx env thenCont)
           , vector (doStat ctx env elseCont)
           )]
    | doStat ctx env
        (N.Handle {body, handler = (e, h), successfulExitIn, successfulExitOut}) =
        (case #style ctx of
           Backend.DIRECT_STYLE =>
             let
               val env' =
                 { continuations = C.CVarMap.singleton
                     ( successfulExitIn
                     , C.CVarMap.lookup (#continuations env, successfulExitOut)
                     )
                 , subst = #subst env
                 }
             in
               [J.TryCatchStat (vector (doStat ctx env' body), e, vector
                  (doStat ctx env h))]
             end
         | Backend.CPS =>
             let
               val escape =
                 CpsAnalyze.escapesTransitively
                   (#contEscapeMap ctx, successfulExitIn)
               val oldExh = genSym ctx
               val save = ConstStat (oldExh, J.VarExp (J.PredefinedId "_exh"))
               val restore = J.AssignStat
                 ( J.VarExp (J.PredefinedId "_exh")
                 , J.VarExp (J.UserDefinedId oldExh)
                 )
               val handler = J.FunctionExp (vector [VIdToJs ctx e], vector
                 (restore :: doStat ctx env h))
               val install = J.AssignStat
                 (J.VarExp (J.PredefinedId "_exh"), handler)
             in
               if escape then
                 let
                   val env' =
                     { continuations =
                         C.CVarMap.singleton
                           (successfulExitIn, TAILCALL successfulExitIn)
                     , subst = #subst env
                     }
                   val result = genSym ctx
                   val dec =
                     ConstStat
                       ( CVarToId successfulExitIn
                       , J.FunctionExp (vector [VIdToJs ctx result], vector
                           (restore
                            ::
                            applyCont
                              ( ctx
                              , env
                              , successfulExitOut
                              , [J.VarExp (VIdToJs ctx result)]
                              )))
                       )
                 in
                   save :: install :: dec :: doStat ctx env' body
                 end
               else
                 let
                   val result = genSym ctx
                   val dec = J.LetStat (vector [(result, NONE)])
                   val env' =
                     { continuations =
                         C.CVarMap.singleton (successfulExitIn, BREAK_TO
                           { label = CVarToJs successfulExitIn
                           , which = NONE
                           , params = [SOME (VIdToJs ctx result)]
                           })
                     , subst = #subst env
                     }
                 in
                   save :: install :: dec
                   ::
                   J.BlockStat (SOME (CVarToJs successfulExitIn), vector
                     (doStat ctx env' body)) :: restore
                   ::
                   applyCont
                     ( ctx
                     , env
                     , successfulExitOut
                     , [J.VarExp (VIdToJs ctx result)]
                     )
                 end
             end)
    | doStat ctx env
        (N.Raise (_ (* span as {start as {file, line, column}} *), exp)) =
        [J.ThrowStat (doExp (ctx, env, exp))] (* TODO: location information *)
    | doStat _ _ N.Unreachable = []

  fun doProgramDirect ctx cont program =
    let
      val label = CVarToJs cont
      val env =
        { continuations = C.CVarMap.singleton
            (cont, BREAK_TO {label = label, which = NONE, params = []})
        , subst = TypedSyntax.VIdMap.empty
        }
    in
      vector [J.BlockStat (SOME label, vector (doStat ctx env program))]
    end
  fun doProgramDirectDefaultExport ctx cont program =
    let
      val label = CVarToJs cont
      val varName = genSymNamed (ctx, "export")
      val item = J.UserDefinedId varName
      val env =
        { continuations = C.CVarMap.singleton (cont, BREAK_TO
            {label = label, which = NONE, params = [SOME item]})
        , subst = TypedSyntax.VIdMap.empty
        }
    in
      vector
        [ J.LetStat (vector [(varName, NONE)])
        , J.BlockStat (SOME label, vector (doStat ctx env program))
        , J.DefaultExportStat (J.VarExp item)
        ]
    end
  fun doProgramDirectNamedExport ctx cont program entities =
    let
      val label = CVarToJs cont
      val entities' =
        Vector.map (fn name => (genSymNamed (ctx, name), name)) entities
      val letStat =
        if Vector.length entities' = 0 then []
        else [J.LetStat (Vector.map (fn (v, _) => (v, NONE)) entities')]
      val env =
        { continuations = C.CVarMap.singleton (cont, BREAK_TO
            { label = label
            , which = NONE
            , params = Vector.foldr (op::) []
                (Vector.map (fn (v, _) => SOME (J.UserDefinedId v)) entities')
            })
        , subst = TypedSyntax.VIdMap.empty
        }
    in
      vector
        (letStat
         @
         [ J.BlockStat (SOME label, vector (doStat ctx env program))
         , J.NamedExportStat
             (Vector.map (fn (v, name) => (J.UserDefinedId v, name)) entities')
         ])
    end
  fun doProgramCPS' ctx cont retVar program =
    if CpsAnalyze.escapesTransitively (#contEscapeMap ctx, cont) then
      let
        val env =
          { continuations = C.CVarMap.singleton (cont, TAILCALL cont)
          , subst = TypedSyntax.VIdMap.empty
          }
        val callExp = J.CallExp (J.VarExp (J.PredefinedId "_run"), vector
          [ J.FunctionExp (vector [CVarToJs cont], vector
              (doStat ctx env program))
          , J.ConstExp J.True
          ])
      in
        case retVar of
          NONE => [J.ExpStat callExp]
        | SOME v => [J.ConstStat (vector [(v, callExp)])]
      end
    else
      let
        val dec =
          case retVar of
            NONE => []
          | SOME v => [J.LetStat (vector [(v, NONE)])]
        val env =
          { continuations = C.CVarMap.singleton (cont, BREAK_TO
              { label = CVarToJs cont
              , which = NONE
              , params =
                  case retVar of
                    NONE => []
                  | SOME v => [SOME (VIdToJs ctx v)]
              })
          , subst = TypedSyntax.VIdMap.empty
          }
      in
        dec
        @ [J.BlockStat (SOME (CVarToJs cont), vector (doStat ctx env program))]
      end
  fun doProgramCPS ctx cont program =
    vector (doProgramCPS' ctx cont NONE program)
  fun doProgramCPSDefaultExport ctx cont program =
    let
      val varName = genSymNamed (ctx, "export")
    in
      vector
        (doProgramCPS' ctx cont (SOME varName) program
         @ [J.DefaultExportStat (J.VarExp (J.UserDefinedId varName))])
    end
  fun doProgramCPSNamedExport ctx cont program entities =
    let
      val varName = genSymNamed (ctx, "export")
      val entities' =
        Vector.map (fn name => (genSymNamed (ctx, name), name)) entities
      val unpack =
        if Vector.length entities' > 0 then
          [J.ConstStat
             (Vector.map
                (fn (v, name) =>
                   ( v
                   , J.IndexExp
                       ( J.VarExp (J.UserDefinedId varName)
                       , J.ConstExp (J.asciiStringAsWide name)
                       )
                   )) entities')]
        else
          []
    in
      vector
        (doProgramCPS' ctx cont (SOME varName) program @ unpack
         @
         [J.NamedExportStat
            (Vector.map (fn (v, name) => (J.UserDefinedId v, name)) entities')])
    end
end;
