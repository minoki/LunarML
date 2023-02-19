(*
 * Copyright (c) 2023 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure CodeGenJsCps = struct
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
                    [(* boolean *)
                     (VId_true, "true") (* boolean literal *)
                    ,(VId_false, "false") (* boolean literal *)
                    (* list *)
                    ,(VId_nil, "null")
                    (* exn *)
                    ,(VId_Match, "_Match")
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
                    [(* boolean *)
                     (VId_true, "true") (* boolean literal *)
                    ,(VId_false, "false") (* boolean literal *)
                    (* list *)
                    ,(VId_nil, "null")
                    (* exn *)
                    ,(VId_Match, "_Match")
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
           , exnCont : C.CVar (* used by CPS *)
           }
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
fun doCExp (ctx : Context) (env : Env) (C.Let { exp = C.PrimOp { primOp = F.RealConstOp x, tyargs = _, args = _ }, result, cont }) : J.Stat list
    = let val exp = let val y = Numeric.toDecimal { nominal_format = Numeric.binary64, target_format = Numeric.binary64 } x
                        (* JavaScript does not support hexadecimal floating-point literals *)
                    in case y of
                           SOME z => if Numeric.Notation.isNegative z then
                                         J.UnaryExp (J.NEGATE, J.ConstExp (J.Numeral (Numeric.Notation.toString "-" (Numeric.Notation.abs z))))
                                     else
                                         J.ConstExp (J.Numeral (Numeric.Notation.toString "-" z))
                         | NONE => raise CodeGenError "the hexadecimal floating-point value cannot be represented as a 64-bit floating-point number"
                    end
      in case result of
             SOME result => ConstStat (result, exp) :: doCExp ctx env cont
           | NONE => doCExp ctx env cont
      end
  | doCExp ctx env (C.Let { exp = C.PrimOp { primOp = F.ListOp, tyargs = _, args = [] }, result, cont })
    = (case result of
           SOME result => ConstStat (result, J.ConstExp J.Null) :: doCExp ctx env cont
         | NONE => doCExp ctx env cont
      )
  | doCExp ctx env (C.Let { exp = C.PrimOp { primOp = F.ListOp, tyargs = _, args = xs }, result, cont })
    = (case result of
           SOME result => ConstStat (result, J.CallExp (J.VarExp (J.PredefinedId "_list"), vector [J.ArrayExp (Vector.map (doValue ctx) (vector xs))])) :: doCExp ctx env cont
         | NONE => doCExp ctx env cont
      )
  | doCExp ctx env (C.Let { exp = C.PrimOp { primOp = F.VectorOp, tyargs = _, args = xs }, result, cont })
    = (case result of
           SOME result => ConstStat (result, J.ArrayExp (Vector.map (doValue ctx) (vector xs))) :: doCExp ctx env cont
         | NONE => doCExp ctx env cont
      )
  | doCExp ctx env (C.Let { exp = C.PrimOp { primOp = F.DataTagAsString16Op info, tyargs = _, args = [exp] }, result, cont })
    = (case result of
           SOME result => ConstStat (result, J.IndexExp (doValue ctx exp, J.ConstExp (J.asciiStringAsWide "tag"))) :: doCExp ctx env cont
         | NONE => doCExp ctx env cont
      )
  | doCExp ctx env (C.Let { exp = C.PrimOp { primOp = F.DataPayloadOp info, tyargs = _, args = [exp] }, result, cont })
    = (case result of
           SOME result => ConstStat (result, J.IndexExp (doValue ctx exp, J.ConstExp (J.asciiStringAsWide "payload"))) :: doCExp ctx env cont
         | NONE => doCExp ctx env cont
      )
  | doCExp ctx env (C.Let { exp = C.PrimOp { primOp = F.ExnPayloadOp, tyargs = _, args = [exp] }, result, cont })
    = (case result of
           SOME result => ConstStat (result, J.IndexExp (doValue ctx exp, J.ConstExp (J.asciiStringAsWide "payload"))) :: doCExp ctx env cont
         | NONE => doCExp ctx env cont
      )
  | doCExp ctx env (C.Let { exp = C.PrimOp { primOp = F.ConstructValOp info, tyargs = _, args = [] }, result, cont })
    = let val tag = #tag info
      in case result of
             SOME result => ConstStat (result, J.ObjectExp (vector [(J.StringKey "tag", J.ConstExp (J.asciiStringAsWide tag))])) :: doCExp ctx env cont
           | NONE => doCExp ctx env cont
      end
  | doCExp ctx env (C.Let { exp = C.PrimOp { primOp = F.ConstructValWithPayloadOp info, tyargs = _, args = [payload] }, result, cont })
    = let val tag = #tag info
          val payload = doValue ctx payload
      in case result of
             SOME result => ConstStat (result, J.ObjectExp (vector [(J.StringKey "tag", J.ConstExp (J.asciiStringAsWide tag)), (J.StringKey "payload", payload)])) :: doCExp ctx env cont
           | NONE => doCExp ctx env cont
      end
  | doCExp ctx env (C.Let { exp = C.PrimOp { primOp = F.ConstructExnOp, tyargs = _, args = [tag] }, result, cont })
    = let val tag = doValue ctx tag
      in case result of
             SOME result => ConstStat (result, J.NewExp (tag, vector [])) :: doCExp ctx env cont
           | NONE => doCExp ctx env cont
      end
  | doCExp ctx env (C.Let { exp = C.PrimOp { primOp = F.ConstructExnWithPayloadOp, tyargs = _, args = [tag, payload] }, result, cont })
    = let val tag = doValue ctx tag
          val payload = doValue ctx payload
      in case result of
             SOME result => ConstStat (result, J.NewExp (tag, vector [payload])) :: doCExp ctx env cont
           | NONE => doCExp ctx env cont
      end
  | doCExp ctx env (C.Let { exp = C.PrimOp { primOp = F.RaiseOp (span as { start as { file, line, column }, ... }), tyargs = _, args = [exp] }, result = _, cont = _ })
    = (case #style ctx of
           DIRECT_STYLE => [J.ThrowStat (doValue ctx exp)]
         | CPS => applyCont (ctx, env, #exnCont env, [doValue ctx exp]) (* TODO: location information *)
      )
  | doCExp ctx env (C.Let { exp = C.PrimOp { primOp = F.PrimFnOp prim, tyargs, args }, result, cont })
    = let fun ConstStatOrExpStat e = case result of
                                       SOME result => ConstStat (result, e)
                                     | NONE => J.ExpStat e
          fun doNullary f = case args of
                                [] => f ()
                              | _ => raise CodeGenError ("primop " ^ Primitives.toString prim ^ ": invalid number of arguments")
          fun doNullaryExp (f, pure) = doNullary (fn () => ConstStatOrExpStat (f ()) :: doCExp ctx env cont)
          fun doUnary f = case args of
                              [a] => f (doValue ctx a)
                            | _ => raise CodeGenError ("primop " ^ Primitives.toString prim ^ ": invalid number of arguments")
          fun doUnaryExp (f, pure) = doUnary (fn a => ConstStatOrExpStat (f a) :: doCExp ctx env cont)
          fun doBinary f = case args of
                               [a, b] => f (doValue ctx a, doValue ctx b)
                             | _ => raise CodeGenError ("primop " ^ Primitives.toString prim ^ ": invalid number of arguments")
          fun doBinaryExp (f, pure : bool) = doBinary (fn (a, b) => ConstStatOrExpStat (f (a, b)) :: doCExp ctx env cont)
          fun doBinaryOp (binop, pure) = doBinaryExp (fn (a, b) => J.BinExp (binop, a, b), pure)
          fun doTernary f = case args of
                                [a, b, c] => f (doValue ctx a, doValue ctx b, doValue ctx c)
                              | _ => raise CodeGenError ("primop " ^ Primitives.toString prim ^ ": invalid number of arguments")
          fun doTernaryExp (f, pure : bool) = doTernary (fn (a, b, c) => ConstStatOrExpStat (f (a, b, c)) :: doCExp ctx env cont)
      in case prim of
             Primitives.call2 => doTernary (fn (f, a0, a1) =>
                                               case #style ctx of
                                                   DIRECT_STYLE => (case result of
                                                                        SOME result => ConstStat (result, J.CallExp (f, vector [a0, a1])) :: doCExp ctx env cont
                                                                      | NONE => J.ExpStat (J.CallExp (f, vector [a0, a1])) :: doCExp ctx env cont
                                                                   )
                                                 | CPS =>
                                                   let val exnName = genSym ctx
                                                   in case result of
                                                          SOME result => J.LetStat (vector [(result, NONE)])
                                                                         :: J.TryCatchStat ( vector [ J.AssignStat (J.VarExp (J.UserDefinedId result), J.CallExp (f, vector [a0, a1])) ]
                                                                                           , exnName
                                                                                           , vector (applyCont (ctx, env, #exnCont env, [J.VarExp (J.UserDefinedId exnName)]))
                                                                                           )
                                                                         :: doCExp ctx env cont
                                                        | NONE => J.TryCatchStat ( vector [ J.ExpStat (J.CallExp (f, vector [a0, a1])) ]
                                                                                 , exnName
                                                                                 , vector (applyCont (ctx, env, #exnCont env, [J.VarExp (J.UserDefinedId exnName)]))
                                                                                 )
                                                                  :: doCExp ctx env cont
                                                   end
                                           )
           | Primitives.List_cons => doBinaryExp (fn (x, xs) => J.ArrayExp (vector [x, xs]), true)
           | Primitives.List_null => doUnaryExp (fn a => J.BinExp (J.EQUAL, a, J.ConstExp J.Null), true)
           | Primitives.List_unsafeHead => doUnaryExp (fn xs => J.IndexExp (xs, J.ConstExp (J.Numeral "0")), true)
           | Primitives.List_unsafeTail => doUnaryExp (fn xs => J.IndexExp (xs, J.ConstExp (J.Numeral "1")), true)
           | Primitives.Ref_ref => doUnary (fn x =>
                                               (* REPRESENTATION_OF_REF *)
                                               case result of
                                                   SOME result => ConstStat (result, J.ObjectExp (vector [(J.StringKey "tag", J.ConstExp (J.asciiStringAsWide "ref"))
                                                                                                         ,(J.StringKey "payload", x)
                                                                                                         ]
                                                                                                 )
                                                                            ) :: doCExp ctx env cont
                                                 | NONE => doCExp ctx env cont
                                           )
           | Primitives.Ref_EQUAL => doBinaryOp (J.EQUAL, true)
           | Primitives.Ref_set => doBinary (fn (a, b) => J.AssignStat (J.IndexExp (a, J.ConstExp (J.asciiStringAsWide "payload")), b) :: (case result of
                                                                                                                                               SOME result => ConstStat (result, J.UndefinedExp) :: doCExp ctx env cont
                                                                                                                                             | NONE => doCExp ctx env cont
                                                                                                                                          )
                                            ) (* REPRESENTATION_OF_REF *)
           | Primitives.Ref_read => doUnaryExp (fn a => J.IndexExp (a, J.ConstExp (J.asciiStringAsWide "payload")), false) (* REPRESENTATION_OF_REF *)
           | Primitives.Bool_EQUAL => doBinaryOp (J.EQUAL, true)
           | Primitives.Bool_not => doUnaryExp (fn a => J.UnaryExp (J.NOT, a), true)
           | Primitives.Int_EQUAL => doBinaryOp (J.EQUAL, true)
           | Primitives.Int_LT => doBinaryOp (J.LT, true)
           | Primitives.Int_GT => doBinaryOp (J.GT, true)
           | Primitives.Int_LE => doBinaryOp (J.LE, true)
           | Primitives.Int_GE => doBinaryOp (J.GE, true)
           | Primitives.Word_EQUAL => doBinaryOp (J.EQUAL, true)
           | Primitives.Word_PLUS => doBinaryExp (fn (a, b) => J.ToUint32Exp (J.BinExp (J.PLUS, a, b)), true)
           | Primitives.Word_MINUS => doBinaryExp (fn (a, b) => J.ToUint32Exp (J.BinExp (J.MINUS, a, b)), true)
           | Primitives.Word_TIMES => doBinaryExp (fn (a, b) => J.ToUint32Exp (J.CallExp (J.VarExp (J.PredefinedId "Math_imul"), vector [a, b])), true)
           | Primitives.Word_TILDE => doUnaryExp (fn a => J.ToUint32Exp (J.UnaryExp (J.NEGATE, a)), true)
           | Primitives.Word_LT => doBinaryOp (J.LT, true)
           | Primitives.Word_GT => doBinaryOp (J.GT, true)
           | Primitives.Word_LE => doBinaryOp (J.LE, true)
           | Primitives.Word_GE => doBinaryOp (J.GE, true)
           | Primitives.Word_LSHIFT_unchecked => doBinaryOp (J.LSHIFT, true)
           | Primitives.Word_RSHIFT_unchecked => doBinaryOp (J.URSHIFT, true)
           | Primitives.Real_PLUS => doBinaryOp (J.PLUS, true)
           | Primitives.Real_MINUS => doBinaryOp (J.MINUS, true)
           | Primitives.Real_TIMES => doBinaryOp (J.TIMES, true)
           | Primitives.Real_DIVIDE => doBinaryOp (J.DIV, true)
           | Primitives.Real_TILDE => doUnaryExp (fn a => J.UnaryExp (J.NEGATE, a), true)
           | Primitives.Real_LT => doBinaryOp (J.LT, true)
           | Primitives.Real_GT => doBinaryOp (J.GT, true)
           | Primitives.Real_LE => doBinaryOp (J.LE, true)
           | Primitives.Real_GE => doBinaryOp (J.GE, true)
           | Primitives.Char_EQUAL => doBinaryOp (J.EQUAL, true)
           | Primitives.Char_LT => doBinaryOp (J.LT, true)
           | Primitives.Char_GT => doBinaryOp (J.GT, true)
           | Primitives.Char_LE => doBinaryOp (J.LE, true)
           | Primitives.Char_GE => doBinaryOp (J.GE, true)
           | Primitives.Char16_EQUAL => doBinaryOp (J.EQUAL, true)
           | Primitives.Char16_LT => doBinaryOp (J.LT, true)
           | Primitives.Char16_GT => doBinaryOp (J.GT, true)
           | Primitives.Char16_LE => doBinaryOp (J.LE, true)
           | Primitives.Char16_GE => doBinaryOp (J.GE, true)
           | Primitives.String_EQUAL => doBinaryExp (fn (a, b) =>  J.CallExp (J.VarExp (J.PredefinedId "_String_EQUAL"), vector [a, b]), true)
           | Primitives.String_LT => doBinaryExp (fn (a, b) => J.CallExp (J.VarExp (J.PredefinedId "_String_LT"), vector [a, b]), true)
           | Primitives.String_HAT => doBinaryExp (fn (a, b) => J.CallExp (J.VarExp (J.PredefinedId "_String_append"), vector [a, b]), true)
           | Primitives.String_size => doUnaryExp (fn a => J.IndexExp (a, J.ConstExp (J.asciiStringAsWide "length")), true)
           | Primitives.String_str => doUnaryExp (fn a => J.MethodExp (J.VarExp (J.PredefinedId "Uint8Array"), "of", vector [a]), true)
           | Primitives.String16_EQUAL => doBinaryOp (J.EQUAL, true)
           | Primitives.String16_LT => doBinaryOp (J.LT, true)
           | Primitives.String16_GT => doBinaryOp (J.GT, true)
           | Primitives.String16_LE => doBinaryOp (J.LE, true)
           | Primitives.String16_GE => doBinaryOp (J.GE, true)
           | Primitives.String16_HAT => doBinaryOp (J.PLUS, true)
           | Primitives.String16_size => doUnaryExp (fn a => J.IndexExp (a, J.ConstExp (J.asciiStringAsWide "length")), true)
           | Primitives.String16_str => doUnaryExp (fn a => J.MethodExp (J.VarExp (J.PredefinedId "String"), "fromCharCode", vector [a]), true)
           | Primitives.IntInf_EQUAL => doBinaryOp (J.EQUAL, true)
           | Primitives.IntInf_PLUS => doBinaryOp (J.PLUS, true)
           | Primitives.IntInf_MINUS => doBinaryOp (J.MINUS, true)
           | Primitives.IntInf_TIMES => doBinaryOp (J.TIMES, true)
           | Primitives.IntInf_TILDE => doUnaryExp (fn a => J.UnaryExp (J.NEGATE, a), true)
           | Primitives.IntInf_LT => doBinaryOp (J.LT, true)
           | Primitives.IntInf_LE => doBinaryOp (J.LE, true)
           | Primitives.IntInf_GT => doBinaryOp (J.GT, true)
           | Primitives.IntInf_GE => doBinaryOp (J.GE, true)
           | Primitives.IntInf_andb => doBinaryOp (J.BITAND, true)
           | Primitives.IntInf_orb => doBinaryOp (J.BITOR, true)
           | Primitives.IntInf_xorb => doBinaryOp (J.BITXOR, true)
           | Primitives.IntInf_notb => doUnaryExp (fn a => J.UnaryExp (J.BITNOT, a), true)
           | Primitives.IntInf_quot_unchecked => doBinaryOp (J.DIV, true)
           | Primitives.IntInf_rem_unchecked => doBinaryOp (J.MOD, true)
           | Primitives.Vector_length => doUnaryExp (fn a => J.IndexExp (a, J.ConstExp (J.asciiStringAsWide "length")), true)
           | Primitives.Vector_unsafeFromListRevN => doBinaryExp (fn (n, xs) => J.CallExp (J.VarExp (J.PredefinedId "_Vector_unsafeFromListRevN"), vector [n, xs]), true)
           | Primitives.Array_EQUAL => doBinaryOp (J.EQUAL, true)
           | Primitives.Array_length => doUnaryExp (fn a => J.IndexExp (a, J.ConstExp (J.asciiStringAsWide "length")), true)
           | Primitives.Unsafe_cast => doUnaryExp (fn a => a, true)
           | Primitives.Unsafe_Vector_sub => doBinaryExp (fn (vec, i) => J.IndexExp (vec, i), true)
           | Primitives.Unsafe_Array_sub => doBinaryExp (fn (arr, i) => J.IndexExp (arr, i), false)
           | Primitives.Unsafe_Array_update => doTernary (fn (arr, i, v) => J.AssignStat (J.IndexExp (arr, i), v) :: (case result of
                                                                                                                          SOME result => ConstStat (result, J.UndefinedExp) :: doCExp ctx env cont
                                                                                                                        | NONE => doCExp ctx env cont
                                                                                                                     )
                                                         )
           | Primitives.Exception_instanceof => doBinaryExp (fn (e, tag) => J.BinExp (J.INSTANCEOF, e, tag), true)
           | Primitives.JavaScript_sub => doBinaryExp (fn (a, b) => J.IndexExp (a, b), false)
           | Primitives.JavaScript_set => doTernary (fn (a, b, c) => J.AssignStat (J.IndexExp (a, b), c) :: (case result of
                                                                                                                 SOME result => ConstStat (result, J.UndefinedExp) :: doCExp ctx env cont
                                                                                                               | NONE => doCExp ctx env cont
                                                                                                            )
                                                    )
           | Primitives.JavaScript_EQUAL => doBinaryOp (J.EQUAL, true)
           | Primitives.JavaScript_NOTEQUAL => doBinaryOp (J.NOTEQUAL, true)
           | Primitives.JavaScript_LT => doBinaryOp (J.LT, false)
           | Primitives.JavaScript_GT => doBinaryOp (J.GT, false)
           | Primitives.JavaScript_LE => doBinaryOp (J.LE, false)
           | Primitives.JavaScript_GE => doBinaryOp (J.GE, false)
           | Primitives.JavaScript_PLUS => doBinaryOp (J.PLUS, false)
           | Primitives.JavaScript_MINUS => doBinaryOp (J.MINUS, false)
           | Primitives.JavaScript_TIMES => doBinaryOp (J.TIMES, false)
           | Primitives.JavaScript_DIVIDE => doBinaryOp (J.DIV, false)
           | Primitives.JavaScript_MOD => doBinaryOp (J.MOD, false)
           | Primitives.JavaScript_negate => doUnaryExp (fn a => J.UnaryExp (J.NEGATE, a), false)
           | Primitives.JavaScript_andb => doBinaryOp (J.BITAND, false)
           | Primitives.JavaScript_orb => doBinaryOp (J.BITOR, false)
           | Primitives.JavaScript_xorb => doBinaryOp (J.BITXOR, false)
           | Primitives.JavaScript_notb => doUnaryExp (fn a => J.UnaryExp (J.BITNOT, a), false)
           | Primitives.JavaScript_LSHIFT => doBinaryOp (J.LSHIFT, false)
           | Primitives.JavaScript_RSHIFT => doBinaryOp (J.RSHIFT, false)
           | Primitives.JavaScript_URSHIFT => doBinaryOp (J.URSHIFT, false)
           | Primitives.JavaScript_EXP => doBinaryOp (J.EXP, true)
           | Primitives.JavaScript_isFalsy => doUnaryExp (fn a => J.UnaryExp (J.NOT, a), false)
           | Primitives.JavaScript_typeof => doUnaryExp (fn a => J.UnaryExp (J.TYPEOF, a), true)
           | Primitives.JavaScript_global => doUnaryExp (fn a => J.IndexExp (J.VarExp (J.PredefinedId "globalThis"), a), false)
           | Primitives.JavaScript_setGlobal => doBinary (fn (name, value) =>
                                                             J.AssignStat (J.IndexExp (J.VarExp (J.PredefinedId "globalThis"), name), value)
                                                             :: (case result of
                                                                     SOME result => ConstStat (result, J.UndefinedExp) :: doCExp ctx env cont
                                                                   | NONE => doCExp ctx env cont
                                                                )
                                                         )
           | Primitives.JavaScript_call => doBinary (fn (f, args) =>
                                                        case #style ctx of
                                                            DIRECT_STYLE => (case result of
                                                                                 SOME result => ConstStat (result, J.MethodExp (f, "apply", vector [J.UndefinedExp, args])) :: doCExp ctx env cont
                                                                               | NONE => J.ExpStat (J.MethodExp (f, "apply", vector [J.UndefinedExp, args])) :: doCExp ctx env cont
                                                                            )
                                                          | CPS =>
                                                            let val exnName = genSym ctx
                                                            in case result of
                                                                   SOME result => J.LetStat (vector [(result, NONE)])
                                                                                  :: J.TryCatchStat ( vector [ J.AssignStat (J.VarExp (J.UserDefinedId result), J.MethodExp (f, "apply", vector [J.UndefinedExp, args])) ]
                                                                                                    , exnName
                                                                                                    , vector (applyCont (ctx, env, #exnCont env, [J.VarExp (J.UserDefinedId exnName)]))
                                                                                                    )
                                                                                  :: doCExp ctx env cont
                                                                 | NONE => J.TryCatchStat ( vector [ J.ExpStat (J.MethodExp (f, "apply", vector [J.UndefinedExp, args])) ]
                                                                                          , exnName
                                                                                          , vector (applyCont (ctx, env, #exnCont env, [J.VarExp (J.UserDefinedId exnName)]))
                                                                                          )
                                                                           :: doCExp ctx env cont
                                                            end
                                                    )
           | Primitives.JavaScript_method => doTernary (fn (obj, method, args) =>
                                                           case #style ctx of
                                                               DIRECT_STYLE => (case result of
                                                                                    SOME result => ConstStat (result, J.MethodExp (J.IndexExp (obj, method), "apply", vector [obj, args])) :: doCExp ctx env cont
                                                                                  | NONE => J.ExpStat (J.MethodExp (J.IndexExp (obj, method), "apply", vector [obj, args])) :: doCExp ctx env cont
                                                                               )
                                                             | CPS =>
                                                               let val exnName = genSym ctx
                                                               in case result of
                                                                      SOME result => J.LetStat (vector [(result, NONE)])
                                                                                     :: J.TryCatchStat ( vector [ J.AssignStat (J.VarExp (J.UserDefinedId result), J.MethodExp (J.IndexExp (obj, method), "apply", vector [obj, args])) ]
                                                                                                       , exnName
                                                                                                       , vector (applyCont (ctx, env, #exnCont env, [J.VarExp (J.UserDefinedId exnName)]))
                                                                                                       )
                                                                                     :: doCExp ctx env cont
                                                                    | NONE => J.TryCatchStat ( vector [ J.ExpStat (J.MethodExp (J.IndexExp (obj, method), "apply", vector [obj, args])) ]
                                                                                             , exnName
                                                                                             , vector (applyCont (ctx, env, #exnCont env, [J.VarExp (J.UserDefinedId exnName)]))
                                                                                             )
                                                                              :: doCExp ctx env cont
                                                               end
                                                       )
           | Primitives.JavaScript_new => doBinary (fn (ctor, args) =>
                                                       case #style ctx of
                                                           DIRECT_STYLE => (case result of
                                                                                SOME result => ConstStat (result, J.MethodExp (J.VarExp (J.PredefinedId "Reflect"), "construct", vector [ctor, args])) :: doCExp ctx env cont
                                                                              | NONE => J.ExpStat (J.MethodExp (J.VarExp (J.PredefinedId "Reflect"), "construct", vector [ctor, args])) :: doCExp ctx env cont
                                                                           )
                                                         | CPS =>
                                                           let val exnName = genSym ctx
                                                           in case result of
                                                                  SOME result => J.LetStat (vector [(result, NONE)])
                                                                                 :: J.TryCatchStat ( vector [ J.AssignStat (J.VarExp (J.UserDefinedId result), J.MethodExp (J.VarExp (J.PredefinedId "Reflect"), "construct", vector [ctor, args])) ]
                                                                                                   , exnName
                                                                                                   , vector (applyCont (ctx, env, #exnCont env, [J.VarExp (J.UserDefinedId exnName)]))
                                                                                                   )
                                                                                 :: doCExp ctx env cont
                                                                | NONE => J.TryCatchStat ( vector [ J.ExpStat (J.MethodExp (J.VarExp (J.PredefinedId "Reflect"), "construct", vector [ctor, args])) ]
                                                                                         , exnName
                                                                                         , vector (applyCont (ctx, env, #exnCont env, [J.VarExp (J.UserDefinedId exnName)]))
                                                                                         )
                                                                          :: doCExp ctx env cont
                                                           end
                                                   )
           | Primitives.DelimCont_newPromptTag => doNullaryExp (fn () => J.CallExp (J.VarExp (J.PredefinedId "_newPromptTag"), vector []), false)
           | _ => raise CodeGenError ("primop " ^ Primitives.toString prim ^ " is not supported on JavaScript-CPS backend")
      end
  | doCExp ctx env (C.Let { exp = C.PrimOp { primOp = F.JsCallOp, tyargs = _, args = f :: args }, result, cont })
    = (case #style ctx of
           DIRECT_STYLE => (case result of
                                SOME result => ConstStat (result, J.CallExp (doValue ctx f, Vector.map (doValue ctx) (vector args))) :: doCExp ctx env cont
                              | NONE => J.ExpStat (J.CallExp (doValue ctx f, Vector.map (doValue ctx) (vector args))) :: doCExp ctx env cont
                           )
         | CPS =>
           let val exnName = genSym ctx
           in case result of
                  SOME result => J.LetStat (vector [(result, NONE)])
                                 :: J.TryCatchStat ( vector [ J.AssignStat (J.VarExp (J.UserDefinedId result), J.CallExp (doValue ctx f, Vector.map (doValue ctx) (vector args))) ]
                                                   , exnName
                                                   , vector (applyCont (ctx, env, #exnCont env, [J.VarExp (J.UserDefinedId exnName)]))
                                                   )
                                 :: doCExp ctx env cont
                | NONE => J.TryCatchStat ( vector [ J.ExpStat (J.CallExp (doValue ctx f, Vector.map (doValue ctx) (vector args))) ]
                                         , exnName
                                         , vector (applyCont (ctx, env, #exnCont env, [J.VarExp (J.UserDefinedId exnName)]))
                                         )
                          :: doCExp ctx env cont
           end
      )
  | doCExp ctx env (C.Let { exp = C.PrimOp { primOp = F.JsMethodOp, tyargs = _, args = obj :: name :: args }, result, cont })
    = (case #style ctx of
           DIRECT_STYLE => (case result of
                                SOME result => ConstStat (result, J.CallExp (J.IndexExp (doValue ctx obj, doValue ctx name), Vector.map (doValue ctx) (vector args))) :: doCExp ctx env cont
                              | NONE => J.ExpStat (J.CallExp (J.IndexExp (doValue ctx obj, doValue ctx name), Vector.map (doValue ctx) (vector args))) :: doCExp ctx env cont
                           )
         | CPS =>
           let val exnName = genSym ctx
           in case result of
                  SOME result => J.LetStat (vector [(result, NONE)])
                                 :: J.TryCatchStat ( vector [ J.AssignStat (J.VarExp (J.UserDefinedId result), J.CallExp (J.IndexExp (doValue ctx obj, doValue ctx name), Vector.map (doValue ctx) (vector args))) ]
                                                   , exnName
                                                   , vector (applyCont (ctx, env, #exnCont env, [J.VarExp (J.UserDefinedId exnName)]))
                                                   )
                                 :: doCExp ctx env cont
                | NONE => J.TryCatchStat ( vector [ J.ExpStat (J.CallExp (J.IndexExp (doValue ctx obj, doValue ctx name), Vector.map (doValue ctx) (vector args))) ]
                                         , exnName
                                         , vector (applyCont (ctx, env, #exnCont env, [J.VarExp (J.UserDefinedId exnName)]))
                                         )
                          :: doCExp ctx env cont
           end
      )
  | doCExp ctx env (C.Let { exp = C.PrimOp { primOp = F.JsNewOp, tyargs = _, args = ctor :: args }, result, cont })
    = (case #style ctx of
           DIRECT_STYLE => (case result of
                                SOME result => ConstStat (result, J.NewExp (doValue ctx ctor, Vector.map (doValue ctx) (vector args))) :: doCExp ctx env cont
                              | NONE => J.ExpStat (J.NewExp (doValue ctx ctor, Vector.map (doValue ctx) (vector args))) :: doCExp ctx env cont
                           )
         | CPS =>
           let val exnName = genSym ctx
           in case result of
                  SOME result => J.LetStat (vector [(result, NONE)])
                                 :: J.TryCatchStat ( vector [ J.AssignStat (J.VarExp (J.UserDefinedId result), J.NewExp (doValue ctx ctor, Vector.map (doValue ctx) (vector args))) ]
                                                   , exnName
                                                   , vector (applyCont (ctx, env, #exnCont env, [J.VarExp (J.UserDefinedId exnName)]))
                                                   )
                                 :: doCExp ctx env cont
                | NONE => J.TryCatchStat ( vector [ J.ExpStat (J.NewExp (doValue ctx ctor, Vector.map (doValue ctx) (vector args))) ]
                                         , exnName
                                         , vector (applyCont (ctx, env, #exnCont env, [J.VarExp (J.UserDefinedId exnName)]))
                                         )
                          :: doCExp ctx env cont
           end
      )
  | doCExp ctx env (C.Let { exp = C.PrimOp { primOp, tyargs = _, args = _ }, result, cont })
    = raise CodeGenError ("primop " ^ Printer.build (FPrinter.doPrimOp primOp) ^ " not implemented yet")
  | doCExp ctx env (C.Let { exp = C.Record fields, result, cont })
    = let val fields = Syntax.LabelMap.foldri (fn (label, v, acc) => (label, doValue ctx v) :: acc) [] fields
          fun isTuple (_, []) = true
            | isTuple (i, (Syntax.NumericLabel n, x) :: xs) = i = n andalso isTuple (i + 1, xs)
            | isTuple _ = false
          val exp = if isTuple (1, fields) then
                        J.ArrayExp (vector (List.map #2 fields))
                    else
                        J.ObjectExp (vector (List.map (fn (label, exp) => (LabelToObjectKey label, exp)) fields))
      in case result of
             SOME result => ConstStat (result, exp) :: doCExp ctx env cont
           | NONE => doCExp ctx env cont
      end
  | doCExp ctx env (C.Let { exp = C.ExnTag { name, payloadTy }, result, cont })
    = (case result of
           SOME result => [ let val value = case payloadTy of
                                                NONE => J.FunctionExp (vector [], vector [])
                                              | SOME _ => J.FunctionExp (vector [J.PredefinedId "payload"], vector [J.AssignStat (J.IndexExp (J.ThisExp, J.ConstExp (J.asciiStringAsWide "payload")), J.VarExp (J.PredefinedId "payload"))])
                            in ConstStat (result, value)
                            end
                          , J.AssignStat (J.IndexExp (J.IndexExp (J.VarExp (J.UserDefinedId result), J.ConstExp (J.asciiStringAsWide "prototype")), J.ConstExp (J.asciiStringAsWide "name")), J.ConstExp (J.asciiStringAsWide name))
                          ] @ doCExp ctx env cont
         | NONE => doCExp ctx env cont
      )
  | doCExp ctx env (C.Let { exp = C.Projection { label, record, fieldTypes }, result, cont })
    = let val label = case label of
                          Syntax.NumericLabel n => J.ConstExp (J.Numeral (Int.toString (n - 1))) (* non-negative *)
                        | Syntax.IdentifierLabel s => J.ConstExp (J.asciiStringAsWide s)
      in case result of
             SOME result => ConstStat (result, J.IndexExp (doValue ctx record, label)) :: doCExp ctx env cont
           | NONE => doCExp ctx env cont
      end
  | doCExp ctx env (C.App { applied, cont, args })
    = (case C.CVarMap.find (#continuations env, cont) of
           SOME (TAILCALL k) => [ J.ReturnStat (SOME (J.ArrayExp (vector [J.ConstExp J.False, doValue ctx applied, J.ArrayExp (vector (doCVar k :: doCVar (#exnCont env) :: List.map (doValue ctx) args))]))) ] (* continuation passing style *)
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
  | doCExp ctx env (C.Let { exp = C.Abs { contParam, params, body }, result, cont })
    = (case result of
           SOME result =>
           (case #style ctx of
                DIRECT_STYLE => let val env' = { continuations = C.CVarMap.singleton (contParam, RETURN), exnCont = C.CVar.dummy }
                                in ConstStat (result, J.CallExp (J.VarExp (J.PredefinedId "_wrap"), vector [J.FunctionExp (Vector.map (VIdToJs ctx) (vector params), vector (doCExp ctx env' body))]))
                                   :: doCExp ctx env cont
                                end
              | CPS => let val exnContParam = genExnContSym ctx
                           val env' = { continuations = C.CVarMap.insert (C.CVarMap.singleton (contParam, TAILCALL contParam), exnContParam, TAILCALL exnContParam)
                                      , exnCont = exnContParam
                                      }
                       in ConstStat (result, J.FunctionExp (vector (CVarToJs contParam :: CVarToJs exnContParam :: List.map (VIdToJs ctx) params), vector (doCExp ctx env' body)))
                          :: doCExp ctx env cont
                       end
           )
         | NONE => doCExp ctx env cont
      )
  | doCExp ctx env (C.LetRec { defs, cont })
    = (case #style ctx of
           DIRECT_STYLE =>
           let val (decs, assignments) = List.foldr (fn ((vid, k, params, body), (decs, assignments)) =>
                                                        let val env' = { continuations = C.CVarMap.singleton (k, RETURN), exnCont = C.CVar.dummy }
                                                        in (J.LetStat (vector [(vid, NONE)]) :: decs, J.AssignStat (J.VarExp (J.UserDefinedId vid), J.CallExp (J.VarExp (J.PredefinedId "_wrap"), vector [J.FunctionExp (Vector.map (VIdToJs ctx) (vector params), vector (doCExp ctx env' body))])) :: assignments)
                                                        end
                                                    ) ([], []) defs
           in decs @ assignments @ doCExp ctx env cont
           end
         | CPS =>
           let val (decs, assignments) = List.foldr (fn ((vid, k, params, body), (decs, assignments)) =>
                                                        let val exnContParam = genExnContSym ctx
                                                            val env' = { continuations = C.CVarMap.insert (C.CVarMap.singleton (k, TAILCALL k), exnContParam, TAILCALL exnContParam)
                                                                       , exnCont = exnContParam
                                                                       }
                                                        in (J.LetStat (vector [(vid, NONE)]) :: decs, J.AssignStat (J.VarExp (J.UserDefinedId vid), J.FunctionExp (vector (CVarToJs k :: CVarToJs exnContParam :: List.map (VIdToJs ctx) params), vector (doCExp ctx env' body))) :: assignments) (* in fact, ConstStat can be used *)
                                                        end
                                                    ) ([], []) defs
           in decs @ assignments @ doCExp ctx env cont
           end
      )
  | doCExp ctx env (C.LetCont { name, params, body, cont })
    = let val escape = case #style ctx of
                           DIRECT_STYLE => false
                         | CPS => case C.CVarMap.find (#contEscapeMap ctx, name) of
                                      SOME false => false
                                    | _ => true
      in if escape then
             let val dec = ConstStat (CVarToId name, J.FunctionExp (vector (List.map (VIdToJs ctx) params), vector (doCExp ctx env body)))
                 val env' = { continuations = C.CVarMap.insert (#continuations env, name, TAILCALL name)
                            , exnCont = #exnCont env
                            }
             in dec :: doCExp ctx env' cont
             end
         else
             case (cont, params) of
                 (app as C.App { applied, cont, args }, [result]) =>
                 if cont = name then
                     ConstStat (result, J.CallExp (doValue ctx applied, Vector.map (doValue ctx) (vector args))) :: doCExp ctx env body
                 else
                     doCExp ctx env body (* dead continuation elimination *)
               | _ => let val dec = if List.null params then
                                        []
                                    else
                                        [J.LetStat (vector (List.map (fn p => (p, NONE)) params))]
                          val newEnv = { continuations = C.CVarMap.insert (#continuations env, name, BREAK_TO { label = CVarToJs name, which = NONE, params = List.map (VIdToJs ctx) params })
                                       , exnCont = #exnCont env
                                       }
                      in dec @ J.BlockStat (SOME (CVarToJs name), vector (doCExp ctx newEnv cont))
                         :: doCExp ctx env body
                      end
      end
  | doCExp ctx env (C.LetRecCont { defs, cont })
    = let val escape = case #style ctx of
                           DIRECT_STYLE => false
                         | CPS => List.exists (fn (name, _, _) => case C.CVarMap.find (#contEscapeMap ctx, name) of SOME false => false | _ => true) defs
      in if escape then
             let val env' = { continuations = List.foldl (fn ((name, params, _), e) => C.CVarMap.insert (e, name, TAILCALL name)) (#continuations env) defs
                            , exnCont = #exnCont env
                            }
                 val (decs, assignments) = List.foldr (fn ((name, params, body), (decs, assignments)) =>
                                                          (J.LetStat (vector [(CVarToId name, NONE)]) :: decs, J.AssignStat (doCVar name, J.FunctionExp (vector (List.map (VIdToJs ctx) params), vector (doCExp ctx env' body))) :: assignments) (* in fact, ConstStat can be used *)
                                                      ) ([], []) defs
             in decs @ assignments @ doCExp ctx env' cont
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
                 val contEnv = { continuations = contEnvC, exnCont = #exnCont env }
                 val (n, recEnvC) = List.foldl (fn ((name, params, _), (i, e)) => (i + 1, C.CVarMap.insert (e, name, CONTINUE_TO { label = loopLabel, which = SOME (which', J.Numeral (Int.toString i)), params = List.map (VIdToJs ctx) (List.take (commonParams, List.length params)) }))) (0, #continuations env) defs
                 val recEnv = { continuations = recEnvC, exnCont = #exnCont env }
             in J.LetStat (vector (List.map (fn p => (p, NONE)) vars))
                :: J.BlockStat (SOME blockLabel, vector (doCExp ctx contEnv cont))
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
             end
      end
  | doCExp ctx env (C.Handle { body, handler = (e, h), successfulExitIn, successfulExitOut })
    = (case #style ctx of
           DIRECT_STYLE =>
           let val env' = { continuations = C.CVarMap.singleton (successfulExitIn, C.CVarMap.lookup (#continuations env, successfulExitOut))
                          , exnCont = C.CVar.dummy
                          }
           in [J.TryCatchStat (vector (doCExp ctx env' body), e, vector (doCExp ctx env h))]
           end
         | CPS =>
           let val handlerName = genExnContSym ctx
               val dec = ConstStat (CVarToId handlerName, J.FunctionExp (vector [VIdToJs ctx e], vector (doCExp ctx env h)))
               val env' = { continuations = C.CVarMap.insert (C.CVarMap.insert (#continuations env, handlerName, TAILCALL handlerName), successfulExitIn, C.CVarMap.lookup (#continuations env, successfulExitOut))
                          , exnCont = handlerName
                          }
           in dec :: doCExp ctx env' body
           end
      )

fun doProgramDirect ctx cont cexp
    = let val label = CVarToJs cont
          val env = { continuations = C.CVarMap.singleton (cont, BREAK_TO { label = label, which = NONE, params = [] })
                    , exnCont = C.CVar.dummy
                    }
      in vector [J.BlockStat (SOME label, vector (doCExp ctx env cexp))]
      end
fun doProgramCPS ctx cont cexp
    = let val exnCont = genExnContSym ctx
          val env = { continuations = C.CVarMap.insert (C.CVarMap.singleton (cont, TAILCALL cont), exnCont, TAILCALL exnCont)
                    , exnCont = exnCont
                    }
      in vector [J.ExpStat (J.CallExp (J.VarExp (J.PredefinedId "_run"), vector [J.FunctionExp (vector [CVarToJs cont, CVarToJs exnCont], vector (doCExp ctx env cexp))]))]
      end
end;
