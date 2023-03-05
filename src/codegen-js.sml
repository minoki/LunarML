(*
 * Copyright (c) 2023 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure CodeGenJs = struct
exception CodeGenError of string
(* Mapping of types:
 * int -> 32-bit signed integer, as a subset of 64-bit floating-point number (excluding negative zero)
 * word -> 32-bit unsigned integer, as a subset of 64-bit floating-point number (excluding negative zero)
 * real -> number (64-bit floating-point number)
 * string -> immutable Uint8Array
 * char -> 8-bit unsigned integer, as a subset of 64-bit floating-point number (excluding negative zero)
 * String16.string (WideString.string) -> 16-bit string
 * Char16.char (WideChar.char) -> 16-bit unsigned integer, as a subset of 64-bit floating-point number (excluding negative zero)
 * exn -> object
 * bool -> boolean
 * ref -> { tag: "ref", payload: <mutable> }
 * list -> null | [<head>, <tail>]
 * tuple -> immutable Array
 * non-tuple record -> immutable object, with integer index starting with 0
 * vector -> immutable Array
 * array -> mutable Array
 * function -> function with arity 1, with optional _MLTAIL_ field (direct style)
 *)
structure F = FSyntax
structure C = CSyntax
structure J = JsSyntax

fun LetStat (vid, exp) = J.LetStat (vector [(vid, SOME exp)])
fun ConstStat (vid, exp) = J.ConstStat (vector [(vid, exp)])

val builtinsDirect
    = let open InitialEnv
      in List.foldl (fn ((vid, name), map) => TypedSyntax.VIdMap.insert (map, vid, name)) TypedSyntax.VIdMap.empty
                    [(* exn *)
                     (VId_Match, "_Match")
                    ,(VId_Bind, "_Bind")
                    ,(VId_Div, "_Div")
                    ,(VId_Overflow, "_Overflow")
                    ,(VId_Size, "_Size")
                    ,(VId_Subscript, "_Subscript")
                    ,(VId_Fail, "_Fail")
                    ,(VId_Match_tag, "_Match_tag")
                    ,(VId_Bind_tag, "_Bind_tag")
                    ,(VId_Div_tag, "_Div_tag")
                    ,(VId_Overflow_tag, "_Overflow_tag")
                    ,(VId_Size_tag, "_Size_tag")
                    ,(VId_Subscript_tag, "_Subscript_tag")
                    ,(VId_Fail_tag, "_Fail_tag")
                    ,(VId_exnName, "_exnName")
                    (* int *)
                    ,(VId_Int_abs, "_Int32_abs") (* may raise Overflow *)
                    ,(VId_Int_TILDE, "_Int32_negate") (* may raise Overflow *)
                    ,(VId_Int_add_bin, "_Int32_add")
                    ,(VId_Int_sub_bin, "_Int32_sub")
                    ,(VId_Int_mul_bin, "_Int32_mul")
                    ,(VId_Int_div_bin, "_Int32_div")
                    ,(VId_Int_mod_bin, "_Int32_mod")
                    ,(VId_Int_quot_bin, "_Int32_quot")
                    ,(VId_Int_rem_bin, "_Int32_rem")
                    (* word *)
                    ,(VId_Word_div_bin, "_Word32_div")
                    ,(VId_Word_mod_bin, "_Word32_mod")
                    (* string *)
                    ,(VId_String_concat, "_String_concat")
                    ,(VId_String_concatWith, "_String_concatWith")
                    ,(VId_String_implode, "_String_implode")
                    ,(VId_String_translate, "_String_translate")
                    (* real *)
                    ,(VId_Real_abs, "Math_abs") (* JS Math.abs *)
                    (* Vector and Array *)
                    ,(VId_Vector_tabulate, "_VectorOrArray_tabulate")
                    ,(VId_Vector_concat, "_Vector_concat")
                    ,(VId_Vector_fromList, "_VectorOrArray_fromList")
                    ,(VId_Array_array, "_Array_array")
                    ,(VId_Array_fromList, "_VectorOrArray_fromList")
                    ,(VId_Array_tabulate, "_VectorOrArray_tabulate")
                    (* JS interface *)
                    ,(VId_JavaScript_undefined, "undefined")
                    ,(VId_JavaScript_null, "null")
                    ,(VId_JavaScript_function, "_function")
                    ,(VId_JavaScript_encodeUtf8, "_encodeUtf8")
                    ,(VId_JavaScript_decodeUtf8, "_decodeUtf8")
                    ,(VId_JavaScript_require, "require")
                    ]
      end
val builtinsCPS
    = let open InitialEnv
      in List.foldl (fn ((vid, name), map) => TypedSyntax.VIdMap.insert (map, vid, name)) TypedSyntax.VIdMap.empty
                    [(* exn *)
                     (VId_Match, "_Match")
                    ,(VId_Bind, "_Bind")
                    ,(VId_Div, "_Div")
                    ,(VId_Overflow, "_Overflow")
                    ,(VId_Size, "_Size")
                    ,(VId_Subscript, "_Subscript")
                    ,(VId_Fail, "_Fail")
                    ,(VId_Match_tag, "_Match_tag")
                    ,(VId_Bind_tag, "_Bind_tag")
                    ,(VId_Div_tag, "_Div_tag")
                    ,(VId_Overflow_tag, "_Overflow_tag")
                    ,(VId_Size_tag, "_Size_tag")
                    ,(VId_Subscript_tag, "_Subscript_tag")
                    ,(VId_Fail_tag, "_Fail_tag")
                    ,(VId_exnName, "_exnName")
                    (* int *)
                    ,(VId_Int_abs, "_Int32_abs") (* may raise Overflow *)
                    ,(VId_Int_TILDE, "_Int32_negate") (* may raise Overflow *)
                    ,(VId_Int_add_bin, "_Int32_add")
                    ,(VId_Int_sub_bin, "_Int32_sub")
                    ,(VId_Int_mul_bin, "_Int32_mul")
                    ,(VId_Int_div_bin, "_Int32_div")
                    ,(VId_Int_mod_bin, "_Int32_mod")
                    ,(VId_Int_quot_bin, "_Int32_quot")
                    ,(VId_Int_rem_bin, "_Int32_rem")
                    (* word *)
                    ,(VId_Word_div_bin, "_Word32_div")
                    ,(VId_Word_mod_bin, "_Word32_mod")
                    (* string *)
                    ,(VId_String_concat, "_String_concat")
                    ,(VId_String_concatWith, "_String_concatWith")
                    ,(VId_String_implode, "_String_implode")
                    (* real *)
                    ,(VId_Real_abs, "_Real_abs")
                    (* Vector and Array *)
                    ,(VId_Vector_concat, "_Vector_concat")
                    ,(VId_Vector_fromList, "_VectorOrArray_fromList")
                    ,(VId_Array_array, "_Array_array")
                    ,(VId_Array_fromList, "_VectorOrArray_fromList")
                    (* JS interface *)
                    ,(VId_JavaScript_undefined, "undefined")
                    ,(VId_JavaScript_null, "null")
                    ,(VId_JavaScript_function, "_function")
                    ,(VId_JavaScript_encodeUtf8, "_encodeUtf8")
                    ,(VId_JavaScript_decodeUtf8, "_decodeUtf8")
                    ,(VId_JavaScript_require, "require")
                    (* extra *)
                    ,(VId_DelimCont_pushPrompt, "_pushPrompt")
                    ,(VId_DelimCont_withSubCont, "_withSubCont")
                    ,(VId_DelimCont_pushSubCont, "_pushSubCont")
                    ,(VId_DelimCont_topLevel, "_topLevel")
                    ]
      end

datatype code_style = DIRECT_STYLE | CPS
type Context = { nextJsId : int ref
               , style : code_style
               , contEscapeMap : bool C.CVarMap.map
               }

fun VIdToJs (ctx : Context) (vid as TypedSyntax.MkVId (name, n))
    = if n < 0 then
          let val builtins = case #style ctx of
                                 DIRECT_STYLE => builtinsDirect
                               | CPS => builtinsCPS
          in case TypedSyntax.VIdMap.find (builtins, vid) of
                 NONE => raise Fail ("the built-in symbol " ^ name ^ "@" ^ Int.toString n ^ " is not supported by JavaScript backend")
               | SOME jsName => JsSyntax.PredefinedId jsName
          end
      else
          JsSyntax.UserDefinedId vid

fun doValue ctx (C.Var vid) = (case VIdToJs ctx vid of
                                   J.PredefinedId "null" => J.ConstExp J.Null
                                 | J.PredefinedId "false" => J.ConstExp J.False
                                 | J.PredefinedId "true" => J.ConstExp J.True
                                 | id => J.VarExp id
                              )
  | doValue _ C.Unit = J.UndefinedExp
  | doValue _ C.Nil = J.ConstExp J.Null (* empty list *)
  | doValue _ (C.BoolConst false) = J.ConstExp J.False
  | doValue _ (C.BoolConst true) = J.ConstExp J.True
  | doValue _ (C.NativeIntConst x) = raise Fail "NativeIntConst is not supported by JavaScript backend"
  | doValue _ (C.Int32Const x) = if x < 0 then
                                     J.UnaryExp (J.NEGATE, J.ConstExp (J.Numeral (LargeInt.toString (~ (Int32.toLarge x)))))
                                 else
                                     J.ConstExp (J.Numeral (Int32.toString x))
  | doValue _ (C.Int54Const x) = if x < 0 then
                                     J.UnaryExp (J.NEGATE, J.ConstExp (J.Numeral (Int64.toString (~ x))))
                                 else
                                     J.ConstExp (J.Numeral (Int64.toString x))
  | doValue _ (C.Int64Const x) = if x < 0 then
                                     J.UnaryExp (J.NEGATE, J.ConstExp (J.Numeral (LargeInt.toString (~ (Int64.toLarge x)) ^ "n")))
                                 else
                                     J.ConstExp (J.Numeral (Int64.toString x ^ "n"))
  | doValue _ (C.IntInfConst x) = if x < 0 then
                                      J.UnaryExp (J.NEGATE, J.ConstExp (J.Numeral (LargeInt.toString (~ x) ^ "n")))
                                  else
                                      J.ConstExp (J.Numeral (LargeInt.toString x ^ "n"))
  | doValue _ (C.NativeWordConst x) = raise Fail "NativeWordConst is not supported by JavaScript backend"
  | doValue _ (C.Word32Const x) = J.ConstExp (J.Numeral ("0x" ^ Word32.fmt StringCvt.HEX x))
  | doValue _ (C.Word64Const x) = J.ConstExp (J.Numeral ("0x" ^ Word64.fmt StringCvt.HEX x ^ "n"))
  | doValue _ (C.CharConst x) = J.ConstExp (J.Numeral (Int.toString (ord x)))
  | doValue _ (C.Char16Const x) = J.ConstExp (J.Numeral (Int.toString x))
  | doValue _ (C.StringConst x) = J.MethodExp (J.VarExp (J.PredefinedId "Uint8Array"), "of", Vector.map (J.ConstExp o J.Numeral o Int.toString) x)
  | doValue _ (C.String16Const x) = J.ConstExp (J.WideString x)

fun CVarToId v = TypedSyntax.MkVId ("cont", C.CVar.toInt v)
fun CVarToJs v = J.UserDefinedId (CVarToId v)
fun doCVar v = J.VarExp (CVarToJs v)

fun LabelToObjectKey (Syntax.NumericLabel n) = JsSyntax.IntKey (n - 1)
  | LabelToObjectKey (Syntax.IdentifierLabel s) = JsSyntax.StringKey s

datatype cont_type = BREAK_TO of { label : J.Id, which : (J.Id * J.JsConst) option, params : J.Id list }
                   | CONTINUE_TO of { label : J.Id, which : (J.Id * J.JsConst) option, params : J.Id list }
                   | TAILCALL of C.CVar (* continuation passing style *)
                   | RETURN (* direct style *)
type Env = { continuations : cont_type C.CVarMap.map
           }

datatype purity = PURE | DISCARDABLE | IMPURE

fun genSym (ctx : Context) = let val n = !(#nextJsId ctx)
                                 val _ = #nextJsId ctx := n + 1
                             in TypedSyntax.MkVId ("tmp", n)
                             end
fun genExnContSym (ctx : Context) = let val n = !(#nextJsId ctx)
                                        val _ = #nextJsId ctx := n + 1
                                    in CSyntax.CVar.fromInt n
                                    end

fun applyCont (ctx : Context, env : Env, cont, args)
    = case C.CVarMap.find (#continuations env, cont) of
          SOME (BREAK_TO { label, which = NONE, params }) => J.MultiAssignStat (List.map J.VarExp params, args) @ [ J.BreakStat (SOME label) ]
        | SOME (BREAK_TO { label, which = SOME (whichVar, whichVal), params }) => J.MultiAssignStat (List.map J.VarExp params, args) @ [ J.AssignStat (J.VarExp whichVar, J.ConstExp whichVal), J.BreakStat (SOME label) ]
        | SOME (CONTINUE_TO { label, which = NONE, params }) => J.MultiAssignStat (List.map J.VarExp params, args) @ [ J.ContinueStat (SOME label) ]
        | SOME (CONTINUE_TO { label, which = SOME (whichVar, whichVal), params }) => J.MultiAssignStat (List.map J.VarExp params, args) @ [ J.AssignStat (J.VarExp whichVar, J.ConstExp whichVal), J.ContinueStat (SOME label) ]
        | SOME (TAILCALL k) => [ J.ReturnStat (SOME (J.ArrayExp (vector [J.ConstExp J.False, doCVar k, J.ArrayExp (vector args)]))) ] (* continuation passing style *)
        | SOME RETURN => [ J.ReturnStat (SOME (J.ArrayExp (vector (J.ConstExp J.True :: args)))) ] (* direct style *)
        | NONE => raise CodeGenError "undefined continuation"
(* doDecs : Context * Env * C.Dec VectorSlice.slice * C.CExp * L.Stat list -> L.Stat list *)
fun doDecs (ctx, env, decs, finalExp, revStats)
    = (case VectorSlice.getItem decs of
           NONE => List.revAppend (revStats, doCExp ctx env finalExp)
         | SOME (dec, decs) =>
           let fun pure (NONE, exp) = doDecs (ctx, env, decs, finalExp, revStats)
                 | pure (SOME result, exp) = doDecs (ctx, env, decs, finalExp, ConstStat (result, exp) :: revStats)
               fun discardable (NONE, exp) = doDecs (ctx, env, decs, finalExp, revStats)
                 | discardable (SOME result, exp) = doDecs (ctx, env, decs, finalExp, ConstStat (result, exp) :: revStats)
               fun impure (NONE, exp) = doDecs (ctx, env, decs, finalExp, J.ExpStat exp :: revStats)
                 | impure (SOME result, exp) = doDecs (ctx, env, decs, finalExp, ConstStat (result, exp) :: revStats)
               fun action (NONE, stmt) = doDecs (ctx, env, decs, finalExp, stmt :: revStats)
                 | action (SOME result, stmt) = doDecs (ctx, env, decs, finalExp, ConstStat (result, J.UndefinedExp) :: stmt :: revStats)
           in case dec of
                  C.ValDec { exp = C.PrimOp { primOp = F.RealConstOp x, tyargs = _, args = _ }, result } =>
                  let val exp = let val y = Numeric.toDecimal { nominal_format = Numeric.binary64, target_format = Numeric.binary64 } x
                                    (* JavaScript does not support hexadecimal floating-point literals *)
                                in case y of
                                       SOME z => if Numeric.Notation.isNegative z then
                                                     J.UnaryExp (J.NEGATE, J.ConstExp (J.Numeral (Numeric.Notation.toString "-" (Numeric.Notation.abs z))))
                                                 else
                                                     J.ConstExp (J.Numeral (Numeric.Notation.toString "-" z))
                                     | NONE => raise CodeGenError "the hexadecimal floating-point value cannot be represented as a 64-bit floating-point number"
                                end
                  in pure (result, exp)
                  end
                | C.ValDec { exp = C.PrimOp { primOp = F.ListOp, tyargs = _, args = [] }, result } =>
                  pure (result, J.ConstExp J.Null)
                | C.ValDec { exp = C.PrimOp { primOp = F.ListOp, tyargs = _, args = xs }, result } =>
                  pure (result, J.CallExp (J.VarExp (J.PredefinedId "_list"), vector [J.ArrayExp (Vector.map (doValue ctx) (vector xs))]))
                | C.ValDec { exp = C.PrimOp { primOp = F.VectorOp, tyargs = _, args = xs }, result } =>
                  pure (result, J.ArrayExp (Vector.map (doValue ctx) (vector xs)))
                | C.ValDec { exp = C.PrimOp { primOp = F.DataTagAsString16Op info, tyargs = _, args = [exp] }, result } =>
                  pure (result, J.IndexExp (doValue ctx exp, J.ConstExp (J.asciiStringAsWide "tag")))
                | C.ValDec { exp = C.PrimOp { primOp = F.DataPayloadOp info, tyargs = _, args = [exp] }, result } =>
                  pure (result, J.IndexExp (doValue ctx exp, J.ConstExp (J.asciiStringAsWide "payload")))
                | C.ValDec { exp = C.PrimOp { primOp = F.ExnPayloadOp, tyargs = _, args = [exp] }, result } =>
                  pure (result, J.IndexExp (doValue ctx exp, J.ConstExp (J.asciiStringAsWide "payload")))
                | C.ValDec { exp = C.PrimOp { primOp = F.ConstructValOp info, tyargs = _, args = [] }, result } =>
                  let val tag = #tag info
                  in pure (result, J.ObjectExp (vector [(J.StringKey "tag", J.ConstExp (J.asciiStringAsWide tag))]))
                  end
                | C.ValDec { exp = C.PrimOp { primOp = F.ConstructValWithPayloadOp info, tyargs = _, args = [payload] }, result } =>
                  let val tag = #tag info
                      val payload = doValue ctx payload
                  in pure (result, J.ObjectExp (vector [(J.StringKey "tag", J.ConstExp (J.asciiStringAsWide tag)), (J.StringKey "payload", payload)]))
                  end
                | C.ValDec { exp = C.PrimOp { primOp = F.ConstructExnOp, tyargs = _, args = [tag] }, result } =>
                  let val tag = doValue ctx tag
                  in pure (result, J.NewExp (tag, vector []))
                  end
                | C.ValDec { exp = C.PrimOp { primOp = F.ConstructExnWithPayloadOp, tyargs = _, args = [tag, payload] }, result } =>
                  let val tag = doValue ctx tag
                      val payload = doValue ctx payload
                  in pure (result, J.NewExp (tag, vector [payload]))
                  end
                | C.ValDec { exp = C.PrimOp { primOp = F.RaiseOp (span as { start as { file, line, column }, ... }), tyargs = _, args = [exp] }, result } =>
                  List.rev (J.ThrowStat (doValue ctx exp) :: revStats) (* TODO: location information *)
                | C.ValDec { exp = C.PrimOp { primOp = F.PrimFnOp prim, tyargs, args }, result } =>
                  let fun ConstStatOrExpStat e = case result of
                                                     SOME result => ConstStat (result, e)
                                                   | NONE => J.ExpStat e
                      fun doNullary f = case args of
                                            [] => f ()
                                          | _ => raise CodeGenError ("primop " ^ Primitives.toString prim ^ ": invalid number of arguments")
                      fun doNullaryExp (f, purity) = doNullary (fn () =>
                                                                   case purity of
                                                                       PURE => pure (result, f ())
                                                                     | DISCARDABLE => discardable (result, f ())
                                                                     | IMPURE => impure (result, f ())
                                                               )
                      fun doUnary f = case args of
                                          [a] => f (doValue ctx a)
                                        | _ => raise CodeGenError ("primop " ^ Primitives.toString prim ^ ": invalid number of arguments")
                      fun doUnaryExp (f, purity) = doUnary (fn a =>
                                                               case purity of
                                                                   PURE => pure (result, f a)
                                                                 | DISCARDABLE => discardable (result, f a)
                                                                 | IMPURE => impure (result, f a)
                                                           )
                      fun doBinary f = case args of
                                           [a, b] => f (doValue ctx a, doValue ctx b)
                                         | _ => raise CodeGenError ("primop " ^ Primitives.toString prim ^ ": invalid number of arguments")
                      fun doBinaryExp (f, purity) = doBinary (fn (a, b) =>
                                                                 case purity of
                                                                     PURE => pure (result, f (a, b))
                                                                   | DISCARDABLE => discardable (result, f (a, b))
                                                                   | IMPURE => impure (result, f (a, b))
                                                             )
                      fun doBinaryOp (binop, pure) = doBinaryExp (fn (a, b) => J.BinExp (binop, a, b), pure)
                      fun doTernary f = case args of
                                            [a, b, c] => f (doValue ctx a, doValue ctx b, doValue ctx c)
                                          | _ => raise CodeGenError ("primop " ^ Primitives.toString prim ^ ": invalid number of arguments")
                  in case prim of
                         Primitives.call2 => doTernary (fn (f, a0, a1) =>
                                                           impure (result, J.CallExp (f, vector [a0, a1]))
                                                       )
                       | Primitives.List_cons => doBinaryExp (fn (x, xs) => J.ArrayExp (vector [x, xs]), PURE)
                       | Primitives.List_null => doUnaryExp (fn a => J.BinExp (J.EQUAL, a, J.ConstExp J.Null), PURE)
                       | Primitives.List_unsafeHead => doUnaryExp (fn xs => J.IndexExp (xs, J.ConstExp (J.Numeral "0")), PURE)
                       | Primitives.List_unsafeTail => doUnaryExp (fn xs => J.IndexExp (xs, J.ConstExp (J.Numeral "1")), PURE)
                       | Primitives.Ref_ref => doUnaryExp ( fn x =>
                                                               (* REPRESENTATION_OF_REF *)
                                                               J.ObjectExp (vector [(J.StringKey "tag", J.ConstExp (J.asciiStringAsWide "ref"))
                                                                                   ,(J.StringKey "payload", x)
                                                                                   ]
                                                                           )
                                                          , DISCARDABLE
                                                          )
                       | Primitives.Ref_EQUAL => doBinaryOp (J.EQUAL, PURE)
                       | Primitives.Ref_set => doBinary (fn (a, b) =>
                                                            action (result, J.AssignStat (J.IndexExp (a, J.ConstExp (J.asciiStringAsWide "payload")), b))
                                                        ) (* REPRESENTATION_OF_REF *)
                       | Primitives.Ref_read => doUnaryExp (fn a => J.IndexExp (a, J.ConstExp (J.asciiStringAsWide "payload")), DISCARDABLE) (* REPRESENTATION_OF_REF *)
                       | Primitives.Bool_EQUAL => doBinaryOp (J.EQUAL, PURE)
                       | Primitives.Bool_not => doUnaryExp (fn a => J.UnaryExp (J.NOT, a), PURE)
                       | Primitives.Int_EQUAL => doBinaryOp (J.EQUAL, PURE)
                       | Primitives.Int_LT => doBinaryOp (J.LT, PURE)
                       | Primitives.Int_GT => doBinaryOp (J.GT, PURE)
                       | Primitives.Int_LE => doBinaryOp (J.LE, PURE)
                       | Primitives.Int_GE => doBinaryOp (J.GE, PURE)
                       | Primitives.Word_EQUAL => doBinaryOp (J.EQUAL, PURE)
                       | Primitives.Word_PLUS => doBinaryExp (fn (a, b) => J.ToUint32Exp (J.BinExp (J.PLUS, a, b)), PURE)
                       | Primitives.Word_MINUS => doBinaryExp (fn (a, b) => J.ToUint32Exp (J.BinExp (J.MINUS, a, b)), PURE)
                       | Primitives.Word_TIMES => doBinaryExp (fn (a, b) => J.ToUint32Exp (J.CallExp (J.VarExp (J.PredefinedId "Math_imul"), vector [a, b])), PURE)
                       | Primitives.Word_TILDE => doUnaryExp (fn a => J.ToUint32Exp (J.UnaryExp (J.NEGATE, a)), PURE)
                       | Primitives.Word_div_unchecked => doBinaryExp (fn (a, b) => J.ToUint32Exp (J.BinExp (J.DIV, a, b)), PURE)
                       | Primitives.Word_mod_unchecked => doBinaryExp (fn (a, b) => J.BinExp (J.MOD, a, b), PURE)
                       | Primitives.Word_LT => doBinaryOp (J.LT, PURE)
                       | Primitives.Word_GT => doBinaryOp (J.GT, PURE)
                       | Primitives.Word_LE => doBinaryOp (J.LE, PURE)
                       | Primitives.Word_GE => doBinaryOp (J.GE, PURE)
                       | Primitives.Word_notb => doUnaryExp (fn a => J.ToUint32Exp (J.UnaryExp (J.BITNOT, a)), PURE)
                       | Primitives.Word_andb => doBinaryExp (fn (a, b) => J.ToUint32Exp (J.BinExp (J.BITAND, a, b)), PURE)
                       | Primitives.Word_orb => doBinaryExp (fn (a, b) => J.ToUint32Exp (J.BinExp (J.BITOR, a, b)), PURE)
                       | Primitives.Word_xorb => doBinaryExp (fn (a, b) => J.ToUint32Exp (J.BinExp (J.BITXOR, a, b)), PURE)
                       | Primitives.Word_LSHIFT_unchecked => doBinaryExp (fn (a, b) => J.ToUint32Exp (J.BinExp (J.LSHIFT, a, b)), PURE)
                       | Primitives.Word_RSHIFT_unchecked => doBinaryOp (J.URSHIFT, PURE)
                       | Primitives.Real_PLUS => doBinaryOp (J.PLUS, PURE)
                       | Primitives.Real_MINUS => doBinaryOp (J.MINUS, PURE)
                       | Primitives.Real_TIMES => doBinaryOp (J.TIMES, PURE)
                       | Primitives.Real_DIVIDE => doBinaryOp (J.DIV, PURE)
                       | Primitives.Real_TILDE => doUnaryExp (fn a => J.UnaryExp (J.NEGATE, a), PURE)
                       | Primitives.Real_LT => doBinaryOp (J.LT, PURE)
                       | Primitives.Real_GT => doBinaryOp (J.GT, PURE)
                       | Primitives.Real_LE => doBinaryOp (J.LE, PURE)
                       | Primitives.Real_GE => doBinaryOp (J.GE, PURE)
                       | Primitives.Char_EQUAL => doBinaryOp (J.EQUAL, PURE)
                       | Primitives.Char_LT => doBinaryOp (J.LT, PURE)
                       | Primitives.Char_GT => doBinaryOp (J.GT, PURE)
                       | Primitives.Char_LE => doBinaryOp (J.LE, PURE)
                       | Primitives.Char_GE => doBinaryOp (J.GE, PURE)
                       | Primitives.Char16_EQUAL => doBinaryOp (J.EQUAL, PURE)
                       | Primitives.Char16_LT => doBinaryOp (J.LT, PURE)
                       | Primitives.Char16_GT => doBinaryOp (J.GT, PURE)
                       | Primitives.Char16_LE => doBinaryOp (J.LE, PURE)
                       | Primitives.Char16_GE => doBinaryOp (J.GE, PURE)
                       | Primitives.String_EQUAL => doBinaryExp (fn (a, b) =>  J.CallExp (J.VarExp (J.PredefinedId "_String_EQUAL"), vector [a, b]), PURE)
                       | Primitives.String_LT => doBinaryExp (fn (a, b) => J.CallExp (J.VarExp (J.PredefinedId "_String_LT"), vector [a, b]), PURE)
                       | Primitives.String_HAT => doBinaryExp (fn (a, b) => J.CallExp (J.VarExp (J.PredefinedId "_String_append"), vector [a, b]), PURE)
                       | Primitives.String_size => doUnaryExp (fn a => J.IndexExp (a, J.ConstExp (J.asciiStringAsWide "length")), PURE)
                       | Primitives.String_str => doUnaryExp (fn a => J.MethodExp (J.VarExp (J.PredefinedId "Uint8Array"), "of", vector [a]), PURE)
                       | Primitives.String16_EQUAL => doBinaryOp (J.EQUAL, PURE)
                       | Primitives.String16_LT => doBinaryOp (J.LT, PURE)
                       | Primitives.String16_GT => doBinaryOp (J.GT, PURE)
                       | Primitives.String16_LE => doBinaryOp (J.LE, PURE)
                       | Primitives.String16_GE => doBinaryOp (J.GE, PURE)
                       | Primitives.String16_HAT => doBinaryOp (J.PLUS, PURE)
                       | Primitives.String16_size => doUnaryExp (fn a => J.IndexExp (a, J.ConstExp (J.asciiStringAsWide "length")), PURE)
                       | Primitives.String16_str => doUnaryExp (fn a => J.MethodExp (J.VarExp (J.PredefinedId "String"), "fromCharCode", vector [a]), PURE)
                       | Primitives.IntInf_EQUAL => doBinaryOp (J.EQUAL, PURE)
                       | Primitives.IntInf_PLUS => doBinaryOp (J.PLUS, PURE)
                       | Primitives.IntInf_MINUS => doBinaryOp (J.MINUS, PURE)
                       | Primitives.IntInf_TIMES => doBinaryOp (J.TIMES, PURE)
                       | Primitives.IntInf_TILDE => doUnaryExp (fn a => J.UnaryExp (J.NEGATE, a), PURE)
                       | Primitives.IntInf_LT => doBinaryOp (J.LT, PURE)
                       | Primitives.IntInf_LE => doBinaryOp (J.LE, PURE)
                       | Primitives.IntInf_GT => doBinaryOp (J.GT, PURE)
                       | Primitives.IntInf_GE => doBinaryOp (J.GE, PURE)
                       | Primitives.IntInf_andb => doBinaryOp (J.BITAND, PURE)
                       | Primitives.IntInf_orb => doBinaryOp (J.BITOR, PURE)
                       | Primitives.IntInf_xorb => doBinaryOp (J.BITXOR, PURE)
                       | Primitives.IntInf_notb => doUnaryExp (fn a => J.UnaryExp (J.BITNOT, a), PURE)
                       | Primitives.IntInf_quot_unchecked => doBinaryOp (J.DIV, PURE)
                       | Primitives.IntInf_rem_unchecked => doBinaryOp (J.MOD, PURE)
                       | Primitives.Vector_length => doUnaryExp (fn a => J.IndexExp (a, J.ConstExp (J.asciiStringAsWide "length")), PURE)
                       | Primitives.Vector_unsafeFromListRevN => doBinaryExp (fn (n, xs) => J.CallExp (J.VarExp (J.PredefinedId "_Vector_unsafeFromListRevN"), vector [n, xs]), PURE)
                       | Primitives.Array_EQUAL => doBinaryOp (J.EQUAL, PURE)
                       | Primitives.Array_length => doUnaryExp (fn a => J.IndexExp (a, J.ConstExp (J.asciiStringAsWide "length")), PURE)
                       | Primitives.Unsafe_cast => doUnaryExp (fn a => a, PURE)
                       | Primitives.Unsafe_Vector_sub => doBinaryExp (fn (vec, i) => J.IndexExp (vec, i), PURE)
                       | Primitives.Unsafe_Array_sub => doBinaryExp (fn (arr, i) => J.IndexExp (arr, i), DISCARDABLE)
                       | Primitives.Unsafe_Array_update => doTernary (fn (arr, i, v) =>
                                                                         action (result, J.AssignStat (J.IndexExp (arr, i), v))
                                                                     )
                       | Primitives.Exception_instanceof => doBinaryExp (fn (e, tag) => J.BinExp (J.INSTANCEOF, e, tag), PURE)
                       | Primitives.JavaScript_sub => doBinaryExp (fn (a, b) => J.IndexExp (a, b), IMPURE)
                       | Primitives.JavaScript_set => doTernary (fn (a, b, c) =>
                                                                    action (result, J.AssignStat (J.IndexExp (a, b), c))
                                                                )
                       | Primitives.JavaScript_EQUAL => doBinaryOp (J.EQUAL, PURE)
                       | Primitives.JavaScript_NOTEQUAL => doBinaryOp (J.NOTEQUAL, PURE)
                       | Primitives.JavaScript_LT => doBinaryOp (J.LT, IMPURE)
                       | Primitives.JavaScript_GT => doBinaryOp (J.GT, IMPURE)
                       | Primitives.JavaScript_LE => doBinaryOp (J.LE, IMPURE)
                       | Primitives.JavaScript_GE => doBinaryOp (J.GE, IMPURE)
                       | Primitives.JavaScript_PLUS => doBinaryOp (J.PLUS, IMPURE)
                       | Primitives.JavaScript_MINUS => doBinaryOp (J.MINUS, IMPURE)
                       | Primitives.JavaScript_TIMES => doBinaryOp (J.TIMES, IMPURE)
                       | Primitives.JavaScript_DIVIDE => doBinaryOp (J.DIV, IMPURE)
                       | Primitives.JavaScript_MOD => doBinaryOp (J.MOD, IMPURE)
                       | Primitives.JavaScript_negate => doUnaryExp (fn a => J.UnaryExp (J.NEGATE, a), IMPURE)
                       | Primitives.JavaScript_andb => doBinaryOp (J.BITAND, IMPURE)
                       | Primitives.JavaScript_orb => doBinaryOp (J.BITOR, IMPURE)
                       | Primitives.JavaScript_xorb => doBinaryOp (J.BITXOR, IMPURE)
                       | Primitives.JavaScript_notb => doUnaryExp (fn a => J.UnaryExp (J.BITNOT, a), IMPURE)
                       | Primitives.JavaScript_LSHIFT => doBinaryOp (J.LSHIFT, IMPURE)
                       | Primitives.JavaScript_RSHIFT => doBinaryOp (J.RSHIFT, IMPURE)
                       | Primitives.JavaScript_URSHIFT => doBinaryOp (J.URSHIFT, IMPURE)
                       | Primitives.JavaScript_EXP => doBinaryOp (J.EXP, PURE)
                       | Primitives.JavaScript_isFalsy => doUnaryExp (fn a => J.UnaryExp (J.NOT, a), DISCARDABLE)
                       | Primitives.JavaScript_typeof => doUnaryExp (fn a => J.UnaryExp (J.TYPEOF, a), PURE)
                       | Primitives.JavaScript_global => doUnaryExp (fn a => J.IndexExp (J.VarExp (J.PredefinedId "globalThis"), a), DISCARDABLE)
                       | Primitives.JavaScript_setGlobal => doBinary (fn (name, value) =>
                                                                         action (result, J.AssignStat (J.IndexExp (J.VarExp (J.PredefinedId "globalThis"), name), value))
                                                                     )
                       | Primitives.JavaScript_call => doBinary (fn (f, args) =>
                                                                    impure (result, J.MethodExp (f, "apply", vector [J.UndefinedExp, args]))
                                                                )
                       | Primitives.JavaScript_method => doTernary (fn (obj, method, args) =>
                                                                       impure (result, J.MethodExp (J.IndexExp (obj, method), "apply", vector [obj, args]))
                                                                   )
                       | Primitives.JavaScript_new => doBinary (fn (ctor, args) =>
                                                                   impure (result, J.MethodExp (J.VarExp (J.PredefinedId "Reflect"), "construct", vector [ctor, args]))
                                                               )
                       | Primitives.DelimCont_newPromptTag => doNullaryExp (fn () => J.CallExp (J.VarExp (J.PredefinedId "_newPromptTag"), vector []), DISCARDABLE)
                       | _ => raise CodeGenError ("primop " ^ Primitives.toString prim ^ " is not supported on JavaScript backend")
                  end
                | C.ValDec { exp = C.PrimOp { primOp = F.JsCallOp, tyargs = _, args = f :: args }, result } =>
                  impure (result, J.CallExp (doValue ctx f, Vector.map (doValue ctx) (vector args)))
                | C.ValDec { exp = C.PrimOp { primOp = F.JsMethodOp, tyargs = _, args = obj :: name :: args }, result } =>
                  impure (result, J.CallExp (J.IndexExp (doValue ctx obj, doValue ctx name), Vector.map (doValue ctx) (vector args)))
                | C.ValDec { exp = C.PrimOp { primOp = F.JsNewOp, tyargs = _, args = ctor :: args }, result } =>
                  impure (result, J.NewExp (doValue ctx ctor, Vector.map (doValue ctx) (vector args)))
                | C.ValDec { exp = C.PrimOp { primOp, tyargs = _, args = _ }, result } =>
                  raise CodeGenError ("primop " ^ Printer.build (FPrinter.doPrimOp primOp) ^ " is not supported on JavaScript backend")
                | C.ValDec { exp = C.Record fields, result } =>
                  let val fields = Syntax.LabelMap.foldri (fn (label, v, acc) => (label, doValue ctx v) :: acc) [] fields
                      fun isTuple (_, []) = true
                        | isTuple (i, (Syntax.NumericLabel n, x) :: xs) = i = n andalso isTuple (i + 1, xs)
                        | isTuple _ = false
                      val exp = if isTuple (1, fields) then
                                    J.ArrayExp (vector (List.map #2 fields))
                                else
                                    J.ObjectExp (vector (List.map (fn (label, exp) => (LabelToObjectKey label, exp)) fields))
                  in pure (result, exp)
                  end
                | C.ValDec { exp = C.ExnTag { name, payloadTy }, result } =>
                  (case result of
                       SOME result =>
                       let val stmts = [ let val value = case payloadTy of
                                                             NONE => J.FunctionExp (vector [], vector [])
                                                           | SOME _ => J.FunctionExp (vector [J.PredefinedId "payload"], vector [J.AssignStat (J.IndexExp (J.ThisExp, J.ConstExp (J.asciiStringAsWide "payload")), J.VarExp (J.PredefinedId "payload"))])
                                         in ConstStat (result, value)
                                         end
                                       , J.AssignStat (J.IndexExp (J.IndexExp (J.VarExp (J.UserDefinedId result), J.ConstExp (J.asciiStringAsWide "prototype")), J.ConstExp (J.asciiStringAsWide "name")), J.ConstExp (J.asciiStringAsWide name))
                                       ]
                       in doDecs (ctx, env, decs, finalExp, List.revAppend (stmts, revStats))
                       end
                     | NONE => doDecs (ctx, env, decs, finalExp, revStats)
                  )
                | C.ValDec { exp = C.Projection { label, record, fieldTypes }, result } =>
                  let val label = case label of
                                      Syntax.NumericLabel n => J.ConstExp (J.Numeral (Int.toString (n - 1))) (* non-negative *)
                                    | Syntax.IdentifierLabel s => J.ConstExp (J.asciiStringAsWide s)
                  in pure (result, J.IndexExp (doValue ctx record, label))
                  end
                | C.ValDec { exp = C.Abs { contParam, params, body }, result } =>
                  (case #style ctx of
                       DIRECT_STYLE => let val env' = { continuations = C.CVarMap.singleton (contParam, RETURN) }
                                       in pure (result, J.CallExp (J.VarExp (J.PredefinedId "_wrap"), vector [J.FunctionExp (Vector.map (VIdToJs ctx) (vector params), vector (doCExp ctx env' body))]))
                                       end
                     | CPS => let val env' = { continuations = C.CVarMap.singleton (contParam, TAILCALL contParam)
                                             }
                              in pure (result, J.FunctionExp (vector (CVarToJs contParam :: List.map (VIdToJs ctx) params), vector (doCExp ctx env' body)))
                              end
                  )
                | C.RecDec defs =>
                  (case #style ctx of
                       DIRECT_STYLE =>
                       let val (decs', assignments) = List.foldr (fn ((vid, k, params, body), (decs, assignments)) =>
                                                                     let val env' = { continuations = C.CVarMap.singleton (k, RETURN) }
                                                                     in (J.LetStat (vector [(vid, NONE)]) :: decs, J.AssignStat (J.VarExp (J.UserDefinedId vid), J.CallExp (J.VarExp (J.PredefinedId "_wrap"), vector [J.FunctionExp (Vector.map (VIdToJs ctx) (vector params), vector (doCExp ctx env' body))])) :: assignments)
                                                                     end
                                                                 ) ([], []) defs
                       in doDecs (ctx, env, decs, finalExp, List.revAppend (decs' @ assignments, revStats))
                       end
                     | CPS =>
                       let val (decs', assignments) = List.foldr (fn ((vid, k, params, body), (decs, assignments)) =>
                                                                     let val env' = { continuations = C.CVarMap.singleton (k, TAILCALL k)
                                                                                    }
                                                                     in (J.LetStat (vector [(vid, NONE)]) :: decs, J.AssignStat (J.VarExp (J.UserDefinedId vid), J.FunctionExp (vector (CVarToJs k :: List.map (VIdToJs ctx) params), vector (doCExp ctx env' body))) :: assignments) (* in fact, ConstStat can be used *)
                                                                     end
                                                                 ) ([], []) defs
                       in doDecs (ctx, env, decs, finalExp, List.revAppend (decs' @ assignments, revStats))
                       end
                  )
                | C.ContDec { name, params, body } =>
                  let val escape = case #style ctx of
                                       DIRECT_STYLE => false
                                     | CPS => case C.CVarMap.find (#contEscapeMap ctx, name) of
                                                  SOME false => false
                                                | _ => true
                  in if escape then
                         let val dec = ConstStat (CVarToId name, J.FunctionExp (vector (List.map (VIdToJs ctx) params), vector (doCExp ctx env body)))
                             val env' = { continuations = C.CVarMap.insert (#continuations env, name, TAILCALL name)
                                        }
                         in doDecs (ctx, env', decs, finalExp, dec :: revStats)
                         end
                     else
                         case (VectorSlice.isEmpty decs, finalExp, params) of
                             (true, C.App { applied, cont, args }, [result]) =>
                             if cont = name then
                                 List.revAppend (revStats, ConstStat (result, J.CallExp (doValue ctx applied, Vector.map (doValue ctx) (vector args))) :: doCExp ctx env body)
                             else
                                 List.revAppend (revStats, doCExp ctx env finalExp) (* dead continuation elimination *)
                           | _ => let val dec = if List.null params then
                                                    []
                                                else
                                                    [J.LetStat (vector (List.map (fn p => (p, NONE)) params))]
                                      val newEnv = { continuations = C.CVarMap.insert (#continuations env, name, BREAK_TO { label = CVarToJs name, which = NONE, params = List.map (VIdToJs ctx) params })
                                                   }
                                  in List.revAppend (revStats, dec @ J.BlockStat (SOME (CVarToJs name), vector (doDecs (ctx, newEnv, decs, finalExp, [])))
                                                               :: doCExp ctx env body)
                                  end
                  end
                | C.RecContDec defs =>
                  let val escape = case #style ctx of
                                       DIRECT_STYLE => false
                                     | CPS => List.exists (fn (name, _, _) => case C.CVarMap.find (#contEscapeMap ctx, name) of SOME false => false | _ => true) defs
                  in if escape then
                         let val env' = { continuations = List.foldl (fn ((name, params, _), e) => C.CVarMap.insert (e, name, TAILCALL name)) (#continuations env) defs
                                        }
                             val (decs', assignments) = List.foldr (fn ((name, params, body), (decs, assignments)) =>
                                                                       (J.LetStat (vector [(CVarToId name, NONE)]) :: decs, J.AssignStat (doCVar name, J.FunctionExp (vector (List.map (VIdToJs ctx) params), vector (doCExp ctx env' body))) :: assignments) (* in fact, ConstStat can be used *)
                                                                   ) ([], []) defs
                         in doDecs (ctx, env', decs, finalExp, List.revAppend (decs' @ assignments, revStats))
                         end
                     else
                         let val blockLabel = J.UserDefinedId (genSym ctx)
                             val loopLabel = J.UserDefinedId (genSym ctx)
                             val which = genSym ctx
                             val which' = J.UserDefinedId which
                             val maxargs = List.foldl (fn ((_, params, _), n) => Int.max (n, List.length params)) 0 defs
                             val commonParams = List.tabulate (maxargs, fn _ => genSym ctx)
                             val vars = which :: commonParams
                             val (_, contEnvC) = List.foldl (fn ((name, params, _), (i, e)) => (i + 1, C.CVarMap.insert (e, name, BREAK_TO { label = blockLabel, which = SOME (which', J.Numeral (Int.toString i)), params = List.map (VIdToJs ctx) (List.take (commonParams, List.length params)) }))) (0, #continuations env) defs
                             val contEnv = { continuations = contEnvC }
                             val (n, recEnvC) = List.foldl (fn ((name, params, _), (i, e)) => (i + 1, C.CVarMap.insert (e, name, CONTINUE_TO { label = loopLabel, which = SOME (which', J.Numeral (Int.toString i)), params = List.map (VIdToJs ctx) (List.take (commonParams, List.length params)) }))) (0, #continuations env) defs
                             val recEnv = { continuations = recEnvC }
                         in List.revAppend ( revStats
                                           , J.LetStat (vector (List.map (fn p => (p, NONE)) vars))
                                             :: J.BlockStat (SOME blockLabel, vector (doDecs (ctx, contEnv, decs, finalExp, [])))
                                             :: [ J.LoopStat ( SOME loopLabel
                                                             , vector [ J.SwitchStat ( J.VarExp which'
                                                                                     , #2 (List.foldr (fn ((name, params, body), (i, cases)) =>
                                                                                                          let val i = i - 1
                                                                                                              val dec = if List.null params then
                                                                                                                            []
                                                                                                                        else
                                                                                                                            [J.ConstStat (vector (ListPair.map (fn (p, v) => (p, J.VarExp (J.UserDefinedId v))) (params, commonParams)))]
                                                                                                          in (i, (J.Numeral (Int.toString i), vector (dec @ doCExp ctx recEnv body)) :: cases)
                                                                                                          end
                                                                                                      ) (n, []) defs)
                                                                                     )
                                                                      ]
                                                             )
                                                ]
                                           )
                         end
                  end
           end
      )
and doCExp (ctx : Context) (env : Env) (C.Let { decs, cont }) : J.Stat list
    = doDecs (ctx, env, VectorSlice.full decs, cont, [])
  | doCExp ctx env (C.App { applied, cont, args })
    = (case C.CVarMap.find (#continuations env, cont) of
           SOME (TAILCALL k) => [ J.ReturnStat (SOME (J.ArrayExp (vector [J.ConstExp J.False, doValue ctx applied, J.ArrayExp (vector (doCVar k :: List.map (doValue ctx) args))]))) ] (* continuation passing style *)
         | SOME (BREAK_TO { label, which = NONE, params = [p] }) => [ J.AssignStat (J.VarExp p, J.CallExp (doValue ctx applied, Vector.map (doValue ctx) (vector args))), J.BreakStat (SOME label) ] (* direct style *)
         | SOME (BREAK_TO { label, which = SOME (whichVar, whichVal), params = [p] }) => [ J.AssignStat (J.VarExp p, J.CallExp (doValue ctx applied, Vector.map (doValue ctx) (vector args))), J.AssignStat (J.VarExp whichVar, J.ConstExp whichVal), J.BreakStat (SOME label) ] (* direct style *)
         | SOME (CONTINUE_TO { label, which = NONE, params = [p] }) => [ J.AssignStat (J.VarExp p, J.CallExp (doValue ctx applied, Vector.map (doValue ctx) (vector args))), J.ContinueStat (SOME label) ] (* direct style *)
         | SOME (CONTINUE_TO { label, which = SOME (whichVar, whichVal), params = [p] }) => [ J.AssignStat (J.VarExp p, J.CallExp (doValue ctx applied, Vector.map (doValue ctx) (vector args))), J.AssignStat (J.VarExp whichVar, J.ConstExp whichVal), J.ContinueStat (SOME label) ] (* direct style *)
         | SOME RETURN => (case args of
                               [arg] => [ J.ReturnStat (SOME (J.ArrayExp (vector [J.ConstExp J.False, doValue ctx applied, doValue ctx arg]))) ] (* direct style, tail call *)
                             | _ => raise CodeGenError "unsupported number of arguments"
                          )
         | _ => raise CodeGenError "invalid continuation"
      )
  | doCExp ctx env (C.AppCont { applied, args })
    = applyCont (ctx, env, applied, List.map (doValue ctx) args)
  | doCExp ctx env (C.If { cond, thenCont, elseCont })
    = [ J.IfStat (doValue ctx cond, vector (doCExp ctx env thenCont), vector (doCExp ctx env elseCont)) ]
  | doCExp ctx env (C.Handle { body, handler = (e, h), successfulExitIn, successfulExitOut })
    = (case #style ctx of
           DIRECT_STYLE =>
           let val env' = { continuations = C.CVarMap.singleton (successfulExitIn, C.CVarMap.lookup (#continuations env, successfulExitOut))
                          }
           in [J.TryCatchStat (vector (doCExp ctx env' body), e, vector (doCExp ctx env h))]
           end
         | CPS =>
           let val escape = case C.CVarMap.find (#contEscapeMap ctx, successfulExitIn) of
                                SOME false => false
                              | _ => true
               val oldExh = genSym ctx
               val save = ConstStat (oldExh, J.VarExp (J.PredefinedId "_exh"))
               val restore = J.AssignStat (J.VarExp (J.PredefinedId "_exh"), J.VarExp (J.UserDefinedId oldExh))
               val handler = J.FunctionExp (vector [VIdToJs ctx e], vector (restore :: doCExp ctx env h))
               val install = J.AssignStat (J.VarExp (J.PredefinedId "_exh"), handler)
           in if escape then
                  let val env' = { continuations = C.CVarMap.singleton (successfulExitIn, TAILCALL successfulExitIn) }
                      val result = genSym ctx
                      val dec = ConstStat (CVarToId successfulExitIn, J.FunctionExp (vector [VIdToJs ctx result], vector (restore :: applyCont (ctx, env, successfulExitOut, [J.VarExp (VIdToJs ctx result)]))))
                  in save :: install :: dec :: doCExp ctx env' body
                  end
              else
                  let val result = genSym ctx
                      val dec = J.LetStat (vector [(result, NONE)])
                      val env' = { continuations = C.CVarMap.singleton (successfulExitIn, BREAK_TO { label = CVarToJs successfulExitIn, which = NONE, params = [VIdToJs ctx result] }) }
                  in save :: install :: dec
                     :: J.BlockStat (SOME (CVarToJs successfulExitIn), vector (doCExp ctx env' body))
                     :: restore
                     :: applyCont (ctx, env, successfulExitOut, [J.VarExp (VIdToJs ctx result)])
                  end
           end
      )

fun doProgramDirect ctx cont cexp
    = let val label = CVarToJs cont
          val env = { continuations = C.CVarMap.singleton (cont, BREAK_TO { label = label, which = NONE, params = [] })
                    }
      in vector [J.BlockStat (SOME label, vector (doCExp ctx env cexp))]
      end
fun doProgramCPS ctx cont cexp
    = let val env = { continuations = C.CVarMap.singleton (cont, TAILCALL cont)
                    }
      in vector [J.ExpStat (J.CallExp (J.VarExp (J.PredefinedId "_run"), vector [J.FunctionExp (vector [CVarToJs cont], vector (doCExp ctx env cexp))]))]
      end
end;
