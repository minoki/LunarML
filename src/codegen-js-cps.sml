(*
 * Copyright (c) 2022 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure CodeGenJsCps = struct
exception CodeGenError of string
structure F = FSyntax
structure C = CSyntax
structure J = JsSyntax

fun VarStat (vid, exp) = J.VarStat (vector [(vid, SOME exp)])

val builtins
    = let open InitialEnv
      in List.foldl (fn ((vid, name), map) => TypedSyntax.VIdMap.insert (map, vid, name)) TypedSyntax.VIdMap.empty
                    [(* ref *)
                     (VId_ref, "_ref")
                    (* boolean *)
                    ,(VId_true, "true") (* boolean literal *)
                    ,(VId_false, "false") (* boolean literal *)
                    (* list *)
                    ,(VId_nil, "_nil")
                    ,(VId_DCOLON, "_cons")
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
                    (* Overloaded: VId_abs, VId_TILDE, VId_div, VId_mod, VId_TIMES, VId_DIVIDE, VId_PLUS, VId_MINUS, VId_LT, VId_GT, VId_LE, VId_GE *)
                    (* int *)
                    ,(VId_Int_abs, "_Int_abs") (* may raise Overflow *)
                    ,(VId_Int_TILDE, "_Int_negate") (* may raise Overflow *)
                    ,(VId_Int_add_bin, "__Int_add")
                    ,(VId_Int_sub_bin, "__Int_sub")
                    ,(VId_Int_mul_bin, "__Int_mul")
                    ,(VId_Int_div_bin, "__Int_div")
                    ,(VId_Int_mod_bin, "__Int_mod")
                    ,(VId_Int_quot_bin, "__Int_quot")
                    ,(VId_Int_rem_bin, "__Int_rem")
                    (* word *)
                    ,(VId_Word_div_bin, "__Word_div")
                    ,(VId_Word_mod_bin, "__Word_mod")
                    ,(VId_Word_LT_bin, "__Word_LT")
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
                    ,(VId_JavaScript_call, "_call")
                    ,(VId_JavaScript_new, "_new")
                    ,(VId_JavaScript_method, "_method")
                    ,(VId_JavaScript_function, "_function")
                    ,(VId_JavaScript_encodeUtf8, "_encodeUtf8")
                    ,(VId_JavaScript_decodeUtf8, "_decodeUtf8")
                    ,(VId_JavaScript_require, "require")
                    (* extra *)
                    ,(VId_assumePure, "_id") (* no-op *)
                    ,(VId_assumeDiscardable, "_id") (* no-op *)
                    ,(VId_DelimCont_topLevel, "_topLevel")
                    ]
      end

fun VIdToJs (vid as TypedSyntax.MkVId (name, n)) = if n < 0 then
                                                       case TypedSyntax.VIdMap.find (builtins, vid) of
                                                           NONE => raise Fail ("the built-in symbol " ^ name ^ "@" ^ Int.toString n ^ " is not supported by JavaScript backend")
                                                         | SOME jsName => JsSyntax.PredefinedId jsName
                                                   else
                                                       JsSyntax.UserDefinedId vid

fun doValue (C.Var vid) = (case VIdToJs vid of
                               J.PredefinedId "null" => J.ConstExp J.Null
                             | J.PredefinedId "false" => J.ConstExp J.False
                             | J.PredefinedId "true" => J.ConstExp J.True
                             | id => J.VarExp id
                          )
  | doValue C.Unit = J.UndefinedExp
  | doValue (C.BoolConst false) = J.ConstExp J.False
  | doValue (C.BoolConst true) = J.ConstExp J.True
  | doValue (C.Int32Const x) = if x < 0 then
                                   J.UnaryExp (J.NEGATE, J.ConstExp (J.Numeral (LargeInt.toString (~ (Int32.toLarge x)))))
                               else
                                   J.ConstExp (J.Numeral (Int32.toString x))
  | doValue (C.IntInfConst x) = if x < 0 then
                                    J.UnaryExp (J.NEGATE, J.ConstExp (J.Numeral (LargeInt.toString (~ x) ^ "n")))
                                else
                                    J.ConstExp (J.Numeral (LargeInt.toString x ^ "n"))
  | doValue (C.Word32Const x) = J.ConstExp (J.Numeral ("0x" ^ Word32.fmt StringCvt.HEX x))
  | doValue (C.CharConst x) = J.ConstExp (J.Numeral (Int.toString (ord x)))
  | doValue (C.Char16Const x) = J.ConstExp (J.WideString (vector [x]))
  | doValue (C.StringConst x) = J.MethodExp (J.VarExp (J.PredefinedId "Uint8Array"), "of", Vector.map (J.ConstExp o J.Numeral o Int.toString) x)
  | doValue (C.String16Const x) = J.ConstExp (J.WideString x)

fun CVarToId v = TypedSyntax.MkVId ("cont", C.CVar.toInt v)
fun CVarToJs v = J.UserDefinedId (CVarToId v)
fun doCVar v = J.VarExp (CVarToJs v)

fun LabelToObjectKey (Syntax.NumericLabel n) = JsSyntax.IntKey (n - 1)
  | LabelToObjectKey (Syntax.IdentifierLabel s) = JsSyntax.StringKey s

type Context = { nextJsId : int ref }
fun genSym (ctx : Context) = let val n = !(#nextJsId ctx)
                                 val _ = #nextJsId ctx := n + 1
                             in TypedSyntax.MkVId ("tmp", n)
                             end

fun doCExp (ctx : Context) (C.Let { exp = C.PrimOp { primOp = F.RealConstOp x, tyargs = _, args = _ }, result, cont, exnCont }) : J.Stat list
    = let val exp = let val y = Numeric.toDecimal { nominal_format = Numeric.binary64, target_format = Numeric.binary64 } x
                        (* JavaScript does not support hexadecimal floating-point literals *)
                    in case y of
                           SOME z => if Numeric.Notation.isNegative z then
                                         J.UnaryExp (J.NEGATE, J.ConstExp (J.Numeral (Numeric.Notation.toString "-" (Numeric.Notation.abs z))))
                                     else
                                         J.ConstExp (J.Numeral (Numeric.Notation.toString "-" z))
                         | NONE => raise CodeGenError "the hexadecimal floating-point value cannot be represented as a 64-bit floating-point number"
                    end
      in VarStat (result, exp) :: doCExp ctx cont
      end
  | doCExp ctx (C.Let { exp = C.PrimOp { primOp = F.ListOp, tyargs = _, args = [] }, result, cont, exnCont })
    = VarStat (result, J.VarExp (J.PredefinedId "_nil")) :: doCExp ctx cont
  | doCExp ctx (C.Let { exp = C.PrimOp { primOp = F.ListOp, tyargs = _, args = xs }, result, cont, exnCont })
    = VarStat (result, J.CallExp (J.VarExp (J.PredefinedId "_list"), vector [J.ArrayExp (Vector.map doValue (vector xs))])) :: doCExp ctx cont
  | doCExp ctx (C.Let { exp = C.PrimOp { primOp = F.VectorOp, tyargs = _, args = xs }, result, cont, exnCont })
    = VarStat (result, J.ArrayExp (Vector.map doValue (vector xs))) :: doCExp ctx cont
  | doCExp ctx (C.Let { exp = C.PrimOp { primOp = F.DataTagOp info, tyargs = _, args = [exp] }, result, cont, exnCont })
    = VarStat (result, J.IndexExp (doValue exp, J.ConstExp (J.asciiStringAsWide "tag"))) :: doCExp ctx cont
  | doCExp ctx (C.Let { exp = C.PrimOp { primOp = F.DataPayloadOp info, tyargs = _, args = [exp] }, result, cont, exnCont })
    = VarStat (result, J.IndexExp (doValue exp, J.ConstExp (J.asciiStringAsWide "payload"))) :: doCExp ctx cont
  | doCExp ctx (C.Let { exp = C.PrimOp { primOp = F.ExnPayloadOp, tyargs = _, args = [exp] }, result, cont, exnCont })
    = VarStat (result, J.IndexExp (doValue exp, J.ConstExp (J.asciiStringAsWide "payload"))) :: doCExp ctx cont
  | doCExp ctx (C.Let { exp = C.PrimOp { primOp = F.ConstructValOp info, tyargs = _, args = [] }, result, cont, exnCont })
    = let val tag = #tag info
      in VarStat (result, J.ObjectExp (vector [(J.StringKey "tag", J.ConstExp (J.asciiStringAsWide tag))])) :: doCExp ctx cont
      end
  | doCExp ctx (C.Let { exp = C.PrimOp { primOp = F.ConstructValWithPayloadOp info, tyargs = _, args = [payload] }, result, cont, exnCont })
    = let val tag = #tag info
          val payload = doValue payload
      in VarStat (result, J.ObjectExp (vector [(J.StringKey "tag", J.ConstExp (J.asciiStringAsWide tag)), (J.StringKey "payload", payload)])) :: doCExp ctx cont
      end
  | doCExp ctx (C.Let { exp = C.PrimOp { primOp = F.ConstructExnOp, tyargs = _, args = [tag] }, result, cont, exnCont })
    = let val tag = doValue tag
      in VarStat (result, J.NewExp (tag, vector [])) :: doCExp ctx cont
      end
  | doCExp ctx (C.Let { exp = C.PrimOp { primOp = F.ConstructExnWithPayloadOp, tyargs = _, args = [tag, payload] }, result, cont, exnCont })
    = let val tag = doValue tag
          val payload = doValue payload
      in VarStat (result, J.NewExp (tag, vector [payload])) :: doCExp ctx cont
      end
  | doCExp ctx (C.Let { exp = C.PrimOp { primOp = F.RaiseOp (span as { start as { file, line, column }, ... }), tyargs = _, args = [exp] }, result, cont, exnCont = SOME exnCont })
    = [ J.ReturnStat (SOME (J.ArrayExp (vector [J.ConstExp J.False, doCVar exnCont, J.ArrayExp (vector [doValue exp]) ]))) ] (* TODO: location information *)
  | doCExp ctx (C.Let { exp = C.PrimOp { primOp = F.PrimFnOp prim, tyargs, args }, result, cont, exnCont })
    = let fun doNullary f = case args of
                                [] => f ()
                              | _ => raise CodeGenError ("primop " ^ Primitives.toString prim ^ ": invalid number of arguments")
          fun doNullaryExp (f, pure) = doNullary (fn () => VarStat (result, f ()) :: doCExp ctx cont)
          fun doUnary f = case args of
                              [a] => f (doValue a)
                            | _ => raise CodeGenError ("primop " ^ Primitives.toString prim ^ ": invalid number of arguments")
          fun doUnaryExp (f, pure) = doUnary (fn a => VarStat (result, f a) :: doCExp ctx cont)
          fun doBinary f = case args of
                               [a, b] => f (doValue a, doValue b)
                             | _ => raise CodeGenError ("primop " ^ Primitives.toString prim ^ ": invalid number of arguments")
          fun doBinaryExp (f, pure : bool) = doBinary (fn (a, b) => VarStat (result, f (a, b)) :: doCExp ctx cont)
          fun doBinaryOp (binop, pure) = doBinaryExp (fn (a, b) => J.BinExp (binop, a, b), pure)
          fun doTernary f = case args of
                                [a, b, c] => f (doValue a, doValue b, doValue c)
                              | _ => raise CodeGenError ("primop " ^ Primitives.toString prim ^ ": invalid number of arguments")
          fun doTernaryExp (f, pure : bool) = doTernary (fn (a, b, c) => VarStat (result, f (a, b, c)) :: doCExp ctx cont)
      in case prim of
             Primitives.call2 => doTernary (fn (f, a0, a1) =>
                                               case exnCont of
                                                   SOME exnCont =>
                                                   let val exnName = genSym ctx
                                                   in J.TryCatchStat ( vector [VarStat (result, J.CallExp (f, vector [a0, a1]))]
                                                                     , exnName
                                                                     , vector [ J.ReturnStat (SOME (J.ArrayExp (vector [J.ConstExp J.False, doCVar exnCont, J.ArrayExp (vector [J.VarExp (J.UserDefinedId exnName)]) ]))) ]
                                                                     )
                                                      :: doCExp ctx cont
                                                   end
                                                 | _ => raise CodeGenError "No exnCont for Primitives.call2"
                                           )
           | Primitives.Ref_EQUAL => doBinaryOp (J.EQUAL, true)
           | Primitives.Ref_set => doBinary (fn (a, b) => J.AssignStat (J.IndexExp (a, J.ConstExp (J.asciiStringAsWide "payload")), b) :: VarStat (result, J.UndefinedExp) (* ? *) :: doCExp ctx cont) (* REPRESENTATION_OF_REF *)
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
           | Primitives.String16_str => doUnaryExp (fn a => a, true)
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
           | Primitives.Unsafe_Array_update => doTernary (fn (arr, i, v) => J.AssignStat (J.IndexExp (arr, i), v) :: VarStat (result, J.UndefinedExp) (* ? *) :: doCExp ctx cont)
           | Primitives.Exception_instanceof => doBinaryExp (fn (e, tag) => J.BinExp (J.INSTANCEOF, e, tag), true)
           | Primitives.JavaScript_sub => doBinaryExp (fn (a, b) => J.IndexExp (a, b), false)
           | Primitives.JavaScript_set => doTernary (fn (a, b, c) => J.AssignStat (J.IndexExp (a, b), c) :: VarStat (result, J.UndefinedExp) (* ? *) :: doCExp ctx cont)
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
           | Primitives.DelimCont_newPromptTag => doNullaryExp (fn () => J.CallExp (J.VarExp (J.PredefinedId "_newPromptTag"), vector []), false)
           | _ => raise CodeGenError ("primop " ^ Primitives.toString prim ^ " is not supported on JavaScript-CPS backend")
      end
  | doCExp ctx (C.Let { exp = C.PrimOp { primOp, tyargs = _, args = _ }, result, cont, exnCont })
    = raise CodeGenError ("primop " ^ Printer.build (FPrinter.doPrimOp primOp) ^ " not implemented yet")
  | doCExp ctx (C.Let { exp = C.Record fields, result, cont, exnCont = _ })
    = let val fields = Syntax.LabelMap.foldri (fn (label, v, acc) => (label, doValue v) :: acc) [] fields
          fun isTuple (_, []) = true
            | isTuple (i, (Syntax.NumericLabel n, x) :: xs) = i = n andalso isTuple (i + 1, xs)
            | isTuple _ = false
          val exp = if isTuple (1, fields) then
                        J.ArrayExp (vector (List.map #2 fields))
                    else
                        J.ObjectExp (vector (List.map (fn (label, exp) => (LabelToObjectKey label, exp)) fields))
      in VarStat (result, exp) :: doCExp ctx cont
      end
  | doCExp ctx (C.Let { exp = C.ExnTag { name, payloadTy }, result, cont, exnCont = _ })
    = [ let val value = case payloadTy of
                            NONE => J.FunctionExp (vector [], vector [])
                          | SOME _ => J.FunctionExp (vector [J.PredefinedId "payload"], vector [J.AssignStat (J.IndexExp (J.ThisExp, J.ConstExp (J.asciiStringAsWide "payload")), J.VarExp (J.PredefinedId "payload"))])
        in J.VarStat (vector [(result, SOME value)])
        end
      , J.AssignStat (J.IndexExp (J.IndexExp (J.VarExp (J.UserDefinedId result), J.ConstExp (J.asciiStringAsWide "prototype")), J.ConstExp (J.asciiStringAsWide "name")), J.ConstExp (J.asciiStringAsWide name))
      ] @ doCExp ctx cont
  | doCExp ctx (C.Let { exp = C.Projection { label, record, fieldTypes }, result, cont, exnCont = _ })
    = let val label = case label of
                          Syntax.NumericLabel n => J.ConstExp (J.Numeral (Int.toString (n - 1))) (* non-negative *)
                        | Syntax.IdentifierLabel s => J.ConstExp (J.asciiStringAsWide s)
      in VarStat (result, J.IndexExp (doValue record, label)) :: doCExp ctx cont
      end
  | doCExp ctx (C.App { applied, cont, exnCont, args })
    = [ J.ReturnStat (SOME (J.ArrayExp (vector [J.ConstExp J.False, doValue applied, J.ArrayExp (vector (doCVar cont :: doCVar exnCont :: List.map doValue args))]))) ]
  | doCExp ctx (C.AppCont { applied, args })
    = [ J.ReturnStat (SOME (J.ArrayExp (vector [J.ConstExp J.False, doCVar applied, J.ArrayExp (vector (List.map doValue args))]))) ]
  | doCExp ctx (C.If { cond, thenCont, elseCont })
    = [ J.IfStat (doValue cond, vector (doCExp ctx thenCont), vector (doCExp ctx elseCont)) ]
  | doCExp ctx (C.LetFun { name, contParam, exnContParam, params, body, cont })
    = let val dec = J.VarStat (vector [(name, SOME (J.FunctionExp (vector (CVarToJs contParam :: CVarToJs exnContParam :: List.map VIdToJs params), vector (doCExp ctx body))))])
      in dec :: doCExp ctx cont
      end
  | doCExp ctx (C.LetRec { defs, cont })
    = let val (decs, assignments) = List.foldr (fn ((vid, k, h, params, body), (decs, assignments)) =>
                                                   (J.VarStat (vector [(vid, NONE)]) :: decs, J.AssignStat (J.VarExp (J.UserDefinedId vid), J.FunctionExp (vector (CVarToJs k :: CVarToJs h :: List.map VIdToJs params), vector (doCExp ctx body))) :: assignments)
                                               ) ([], []) defs
      in decs @ assignments @ doCExp ctx cont
      end
  | doCExp ctx (C.LetCont { name, params, body, cont })
    = let val dec = J.VarStat (vector [(CVarToId name, SOME (J.FunctionExp (vector (List.map VIdToJs params), vector (doCExp ctx body))))])
      in dec :: doCExp ctx cont
      end
  | doCExp ctx (C.LetRecCont { defs, cont })
    = let val (decs, assignments) = List.foldr (fn ((name, params, body), (decs, assignments)) =>
                                                   (J.VarStat (vector [(CVarToId name, NONE)]) :: decs, J.AssignStat (doCVar name, J.FunctionExp (vector (List.map VIdToJs params), vector (doCExp ctx body))) :: assignments)
                                               ) ([], []) defs
      in decs @ assignments @ doCExp ctx cont
      end
  | doCExp ctx (C.PushPrompt { promptTag, f, cont, exnCont })
    = [ J.ReturnStat (SOME (J.CallExp (J.VarExp (J.PredefinedId "_pushPrompt"), vector [doValue promptTag, doValue f, doCVar cont, doCVar exnCont]))) ]
  | doCExp ctx (C.WithSubCont { promptTag, f, cont, exnCont })
    = [ J.ReturnStat (SOME (J.CallExp (J.VarExp (J.PredefinedId "_withSubCont"), vector [doValue promptTag, doValue f, doCVar cont, doCVar exnCont]))) ]
  | doCExp ctx (C.PushSubCont { subCont, f, cont, exnCont })
    = [ J.ReturnStat (SOME (J.CallExp (J.VarExp (J.PredefinedId "_pushSubCont"), vector [doValue subCont, doValue f, doCVar cont, doCVar exnCont]))) ]

fun doProgram ctx cont exnCont cexp = vector [J.ExpStat (J.CallExp (J.VarExp (J.PredefinedId "_run"), vector [J.FunctionExp (vector [CVarToJs cont, CVarToJs exnCont], vector (doCExp ctx cexp))]))]
end;
