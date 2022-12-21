(*
 * Copyright (c) 2022 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure CodeGenJsCps = struct
exception CodeGenError of string
structure F = FSyntax
structure C = CSyntax
structure J = JsSyntax

fun LetStat (vid, exp) = J.LetStat (vector [(vid, SOME exp)])
fun ConstStat (vid, exp) = J.ConstStat (vector [(vid, exp)])

val builtins
    = let open InitialEnv
      in List.foldl (fn ((vid, name), map) => TypedSyntax.VIdMap.insert (map, vid, name)) TypedSyntax.VIdMap.empty
                    [(* boolean *)
                     (VId_true, "true") (* boolean literal *)
                    ,(VId_false, "false") (* boolean literal *)
                    (* list *)
                    ,(VId_nil, "_nil")
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

type Context = { nextJsId : int ref, contEscapeMap : bool C.CVarMap.map }
datatype cont_type = BREAK_TO of { label : J.Id, which : (J.Id * J.JsConst) option, params : J.Id list }
                   | CONTINUE_TO of { label : J.Id, which : (J.Id * J.JsConst) option, params : J.Id list }
                   | TAILCALL
type Env = cont_type C.CVarMap.map
fun genSym (ctx : Context) = let val n = !(#nextJsId ctx)
                                 val _ = #nextJsId ctx := n + 1
                             in TypedSyntax.MkVId ("tmp", n)
                             end

fun applyCont (ctx : Context, env : Env, cont, args)
    = case C.CVarMap.find (env, cont) of
          SOME (BREAK_TO { label, which = NONE, params }) => J.MultiAssignStat (List.map J.VarExp params, args) @ [ J.BreakStat (SOME label) ]
        | SOME (BREAK_TO { label, which = SOME (whichVar, whichVal), params }) => J.MultiAssignStat (List.map J.VarExp params, args) @ [ J.AssignStat (J.VarExp whichVar, J.ConstExp whichVal), J.BreakStat (SOME label) ]
        | SOME (CONTINUE_TO { label, which = NONE, params }) => J.MultiAssignStat (List.map J.VarExp params, args) @ [ J.ContinueStat (SOME label) ]
        | SOME (CONTINUE_TO { label, which = SOME (whichVar, whichVal), params }) => J.MultiAssignStat (List.map J.VarExp params, args) @ [ J.AssignStat (J.VarExp whichVar, J.ConstExp whichVal), J.ContinueStat (SOME label) ]
        | SOME TAILCALL => [ J.ReturnStat (SOME (J.ArrayExp (vector [J.ConstExp J.False, doCVar cont, J.ArrayExp (vector args)]))) ]
        | NONE => raise CodeGenError "undefined continuation"
fun doCExp (ctx : Context) (env : Env) (C.Let { exp = C.PrimOp { primOp = F.RealConstOp x, tyargs = _, args = _ }, result, cont, exnCont }) : J.Stat list
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
  | doCExp ctx env (C.Let { exp = C.PrimOp { primOp = F.ListOp, tyargs = _, args = [] }, result, cont, exnCont })
    = (case result of
           SOME result => ConstStat (result, J.VarExp (J.PredefinedId "_nil")) :: doCExp ctx env cont
         | NONE => doCExp ctx env cont
      )
  | doCExp ctx env (C.Let { exp = C.PrimOp { primOp = F.ListOp, tyargs = _, args = xs }, result, cont, exnCont })
    = (case result of
           SOME result => ConstStat (result, J.CallExp (J.VarExp (J.PredefinedId "_list"), vector [J.ArrayExp (Vector.map doValue (vector xs))])) :: doCExp ctx env cont
         | NONE => doCExp ctx env cont
      )
  | doCExp ctx env (C.Let { exp = C.PrimOp { primOp = F.VectorOp, tyargs = _, args = xs }, result, cont, exnCont })
    = (case result of
           SOME result => ConstStat (result, J.ArrayExp (Vector.map doValue (vector xs))) :: doCExp ctx env cont
         | NONE => doCExp ctx env cont
      )
  | doCExp ctx env (C.Let { exp = C.PrimOp { primOp = F.DataTagAsString16Op info, tyargs = _, args = [exp] }, result, cont, exnCont })
    = (case result of
           SOME result => ConstStat (result, J.IndexExp (doValue exp, J.ConstExp (J.asciiStringAsWide "tag"))) :: doCExp ctx env cont
         | NONE => doCExp ctx env cont
      )
  | doCExp ctx env (C.Let { exp = C.PrimOp { primOp = F.DataPayloadOp info, tyargs = _, args = [exp] }, result, cont, exnCont })
    = (case result of
           SOME result => ConstStat (result, J.IndexExp (doValue exp, J.ConstExp (J.asciiStringAsWide "payload"))) :: doCExp ctx env cont
         | NONE => doCExp ctx env cont
      )
  | doCExp ctx env (C.Let { exp = C.PrimOp { primOp = F.ExnPayloadOp, tyargs = _, args = [exp] }, result, cont, exnCont })
    = (case result of
           SOME result => ConstStat (result, J.IndexExp (doValue exp, J.ConstExp (J.asciiStringAsWide "payload"))) :: doCExp ctx env cont
         | NONE => doCExp ctx env cont
      )
  | doCExp ctx env (C.Let { exp = C.PrimOp { primOp = F.ConstructValOp info, tyargs = _, args = [] }, result, cont, exnCont })
    = let val tag = #tag info
      in case result of
             SOME result => ConstStat (result, J.ObjectExp (vector [(J.StringKey "tag", J.ConstExp (J.asciiStringAsWide tag))])) :: doCExp ctx env cont
           | NONE => doCExp ctx env cont
      end
  | doCExp ctx env (C.Let { exp = C.PrimOp { primOp = F.ConstructValWithPayloadOp info, tyargs = _, args = [payload] }, result, cont, exnCont })
    = let val tag = #tag info
          val payload = doValue payload
      in case result of
             SOME result => ConstStat (result, J.ObjectExp (vector [(J.StringKey "tag", J.ConstExp (J.asciiStringAsWide tag)), (J.StringKey "payload", payload)])) :: doCExp ctx env cont
           | NONE => doCExp ctx env cont
      end
  | doCExp ctx env (C.Let { exp = C.PrimOp { primOp = F.ConstructExnOp, tyargs = _, args = [tag] }, result, cont, exnCont })
    = let val tag = doValue tag
      in case result of
             SOME result => ConstStat (result, J.NewExp (tag, vector [])) :: doCExp ctx env cont
           | NONE => doCExp ctx env cont
      end
  | doCExp ctx env (C.Let { exp = C.PrimOp { primOp = F.ConstructExnWithPayloadOp, tyargs = _, args = [tag, payload] }, result, cont, exnCont })
    = let val tag = doValue tag
          val payload = doValue payload
      in case result of
             SOME result => ConstStat (result, J.NewExp (tag, vector [payload])) :: doCExp ctx env cont
           | NONE => doCExp ctx env cont
      end
  | doCExp ctx env (C.Let { exp = C.PrimOp { primOp = F.RaiseOp (span as { start as { file, line, column }, ... }), tyargs = _, args = [exp] }, result = _, cont = _, exnCont = SOME exnCont })
    = applyCont (ctx, env, exnCont, [doValue exp]) (* TODO: location information *)
  | doCExp ctx env (C.Let { exp = C.PrimOp { primOp = F.PrimFnOp prim, tyargs, args }, result, cont, exnCont })
    = let fun ConstStatOrExpStat e = case result of
                                       SOME result => ConstStat (result, e)
                                     | NONE => J.ExpStat e
          fun doNullary f = case args of
                                [] => f ()
                              | _ => raise CodeGenError ("primop " ^ Primitives.toString prim ^ ": invalid number of arguments")
          fun doNullaryExp (f, pure) = doNullary (fn () => ConstStatOrExpStat (f ()) :: doCExp ctx env cont)
          fun doUnary f = case args of
                              [a] => f (doValue a)
                            | _ => raise CodeGenError ("primop " ^ Primitives.toString prim ^ ": invalid number of arguments")
          fun doUnaryExp (f, pure) = doUnary (fn a => ConstStatOrExpStat (f a) :: doCExp ctx env cont)
          fun doBinary f = case args of
                               [a, b] => f (doValue a, doValue b)
                             | _ => raise CodeGenError ("primop " ^ Primitives.toString prim ^ ": invalid number of arguments")
          fun doBinaryExp (f, pure : bool) = doBinary (fn (a, b) => ConstStatOrExpStat (f (a, b)) :: doCExp ctx env cont)
          fun doBinaryOp (binop, pure) = doBinaryExp (fn (a, b) => J.BinExp (binop, a, b), pure)
          fun doTernary f = case args of
                                [a, b, c] => f (doValue a, doValue b, doValue c)
                              | _ => raise CodeGenError ("primop " ^ Primitives.toString prim ^ ": invalid number of arguments")
          fun doTernaryExp (f, pure : bool) = doTernary (fn (a, b, c) => ConstStatOrExpStat (f (a, b, c)) :: doCExp ctx env cont)
      in case prim of
             Primitives.call2 => doTernary (fn (f, a0, a1) =>
                                               case exnCont of
                                                   SOME exnCont =>
                                                   let val exnName = genSym ctx
                                                   in case result of
                                                          SOME result => J.LetStat (vector [(result, NONE)])
                                                                         :: J.TryCatchStat ( vector [ J.AssignStat (J.VarExp (J.UserDefinedId result), J.CallExp (f, vector [a0, a1])) ]
                                                                                           , exnName
                                                                                           , vector (applyCont (ctx, env, exnCont, [J.VarExp (J.UserDefinedId exnName)]))
                                                                                           )
                                                                         :: doCExp ctx env cont
                                                        | NONE => J.TryCatchStat ( vector [ J.ExpStat (J.CallExp (f, vector [a0, a1])) ]
                                                                                 , exnName
                                                                                 , vector (applyCont (ctx, env, exnCont, [J.VarExp (J.UserDefinedId exnName)]))
                                                                                 )
                                                                  :: doCExp ctx env cont
                                                   end
                                                 | _ => raise CodeGenError "No exnCont for Primitives.call2"
                                           )
           | Primitives.List_cons => doBinary (fn (x, xs) =>
                                                  case result of
                                                      SOME result => ConstStat (result, J.ObjectExp (vector [(J.StringKey "tag", J.ConstExp (J.asciiStringAsWide "::"))
                                                                                                            ,(J.StringKey "payload", J.ArrayExp (vector [x, xs]))
                                                                                                            ]
                                                                                                    )
                                                                               ) :: doCExp ctx env cont
                                                    | NONE => doCExp ctx env cont
                                              )
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
           | Primitives.JavaScript_call => doBinary (fn (f, args) =>
                                                        case exnCont of
                                                            SOME exnCont =>
                                                            let val exnName = genSym ctx
                                                            in case result of
                                                                   SOME result => J.LetStat (vector [(result, NONE)])
                                                                                  :: J.TryCatchStat ( vector [ J.AssignStat (J.VarExp (J.UserDefinedId result), J.MethodExp (f, "apply", vector [J.UndefinedExp, args])) ]
                                                                                                    , exnName
                                                                                                    , vector (applyCont (ctx, env, exnCont, [J.VarExp (J.UserDefinedId exnName)]))
                                                                                                    )
                                                                                  :: doCExp ctx env cont
                                                                 | NONE => J.TryCatchStat ( vector [ J.ExpStat (J.MethodExp (f, "apply", vector [J.UndefinedExp, args])) ]
                                                                                          , exnName
                                                                                          , vector (applyCont (ctx, env, exnCont, [J.VarExp (J.UserDefinedId exnName)]))
                                                                                          )
                                                                           :: doCExp ctx env cont
                                                            end
                                                          | NONE => raise CodeGenError "No exnCont for JavaScript.call"
                                                    )
           | Primitives.JavaScript_method => doTernary (fn (obj, method, args) =>
                                                           case exnCont of
                                                               SOME exnCont =>
                                                               let val exnName = genSym ctx
                                                               in case result of
                                                                      SOME result => J.LetStat (vector [(result, NONE)])
                                                                                     :: J.TryCatchStat ( vector [ J.AssignStat (J.VarExp (J.UserDefinedId result), J.MethodExp (J.IndexExp (obj, method), "apply", vector [obj, args])) ]
                                                                                                       , exnName
                                                                                                       , vector (applyCont (ctx, env, exnCont, [J.VarExp (J.UserDefinedId exnName)]))
                                                                                                       )
                                                                                     :: doCExp ctx env cont
                                                                    | NONE => J.TryCatchStat ( vector [ J.ExpStat (J.MethodExp (J.IndexExp (obj, method), "apply", vector [obj, args])) ]
                                                                                             , exnName
                                                                                             , vector (applyCont (ctx, env, exnCont, [J.VarExp (J.UserDefinedId exnName)]))
                                                                                             )
                                                                              :: doCExp ctx env cont
                                                               end
                                                             | NONE => raise CodeGenError "No exnCont for JavaScript.method"
                                                       )
           | Primitives.JavaScript_new => doBinary (fn (ctor, args) =>
                                                       case exnCont of
                                                           SOME exnCont =>
                                                           let val exnName = genSym ctx
                                                           in case result of
                                                                  SOME result => J.LetStat (vector [(result, NONE)])
                                                                                 :: J.TryCatchStat ( vector [ J.AssignStat (J.VarExp (J.UserDefinedId result), J.MethodExp (J.VarExp (J.PredefinedId "Reflect"), "construct", vector [ctor, args])) ]
                                                                                                   , exnName
                                                                                                   , vector (applyCont (ctx, env, exnCont, [J.VarExp (J.UserDefinedId exnName)]))
                                                                                                   )
                                                                                 :: doCExp ctx env cont
                                                                | NONE => J.TryCatchStat ( vector [ J.ExpStat (J.MethodExp (J.VarExp (J.PredefinedId "Reflect"), "construct", vector [ctor, args])) ]
                                                                                         , exnName
                                                                                         , vector (applyCont (ctx, env, exnCont, [J.VarExp (J.UserDefinedId exnName)]))
                                                                                         )
                                                                          :: doCExp ctx env cont
                                                           end
                                                         | NONE => raise CodeGenError "No exnCont for JavaScript.new"
                                                   )
           | Primitives.DelimCont_newPromptTag => doNullaryExp (fn () => J.CallExp (J.VarExp (J.PredefinedId "_newPromptTag"), vector []), false)
           | _ => raise CodeGenError ("primop " ^ Primitives.toString prim ^ " is not supported on JavaScript-CPS backend")
      end
  | doCExp ctx env (C.Let { exp = C.PrimOp { primOp = F.JsCallOp, tyargs = _, args = f :: args }, result, cont, exnCont = SOME exnCont })
    = let val exnName = genSym ctx
      in case result of
             SOME result => J.LetStat (vector [(result, NONE)])
                            :: J.TryCatchStat ( vector [ J.AssignStat (J.VarExp (J.UserDefinedId result), J.CallExp (doValue f, Vector.map doValue (vector args))) ]
                                              , exnName
                                              , vector (applyCont (ctx, env, exnCont, [J.VarExp (J.UserDefinedId exnName)]))
                                              )
                            :: doCExp ctx env cont
           | NONE => J.TryCatchStat ( vector [ J.ExpStat (J.CallExp (doValue f, Vector.map doValue (vector args))) ]
                                    , exnName
                                    , vector (applyCont (ctx, env, exnCont, [J.VarExp (J.UserDefinedId exnName)]))
                                    )
                     :: doCExp ctx env cont
      end
  | doCExp ctx env (C.Let { exp = C.PrimOp { primOp = F.JsMethodOp, tyargs = _, args = obj :: name :: args }, result, cont, exnCont = SOME exnCont })
    = let val exnName = genSym ctx
      in case result of
             SOME result => J.LetStat (vector [(result, NONE)])
                            :: J.TryCatchStat ( vector [ J.AssignStat (J.VarExp (J.UserDefinedId result), J.CallExp (J.IndexExp (doValue obj, doValue name), Vector.map doValue (vector args))) ]
                                              , exnName
                                              , vector (applyCont (ctx, env, exnCont, [J.VarExp (J.UserDefinedId exnName)]))
                                              )
                            :: doCExp ctx env cont
           | NONE => J.TryCatchStat ( vector [ J.ExpStat (J.CallExp (J.IndexExp (doValue obj, doValue name), Vector.map doValue (vector args))) ]
                                    , exnName
                                    , vector (applyCont (ctx, env, exnCont, [J.VarExp (J.UserDefinedId exnName)]))
                                    )
                     :: doCExp ctx env cont
      end
  | doCExp ctx env (C.Let { exp = C.PrimOp { primOp = F.JsNewOp, tyargs = _, args = ctor :: args }, result, cont, exnCont = SOME exnCont })
    = let val exnName = genSym ctx
      in case result of
             SOME result => J.LetStat (vector [(result, NONE)])
                            :: J.TryCatchStat ( vector [ J.AssignStat (J.VarExp (J.UserDefinedId result), J.NewExp (doValue ctor, Vector.map doValue (vector args))) ]
                                              , exnName
                                              , vector (applyCont (ctx, env, exnCont, [J.VarExp (J.UserDefinedId exnName)]))
                                              )
                            :: doCExp ctx env cont
           | NONE => J.TryCatchStat ( vector [ J.ExpStat (J.NewExp (doValue ctor, Vector.map doValue (vector args))) ]
                                    , exnName
                                    , vector (applyCont (ctx, env, exnCont, [J.VarExp (J.UserDefinedId exnName)]))
                                    )
                     :: doCExp ctx env cont
      end
  | doCExp ctx env (C.Let { exp = C.PrimOp { primOp, tyargs = _, args = _ }, result, cont, exnCont })
    = raise CodeGenError ("primop " ^ Printer.build (FPrinter.doPrimOp primOp) ^ " not implemented yet")
  | doCExp ctx env (C.Let { exp = C.Record fields, result, cont, exnCont = _ })
    = let val fields = Syntax.LabelMap.foldri (fn (label, v, acc) => (label, doValue v) :: acc) [] fields
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
  | doCExp ctx env (C.Let { exp = C.ExnTag { name, payloadTy }, result, cont, exnCont = _ })
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
  | doCExp ctx env (C.Let { exp = C.Projection { label, record, fieldTypes }, result, cont, exnCont = _ })
    = let val label = case label of
                          Syntax.NumericLabel n => J.ConstExp (J.Numeral (Int.toString (n - 1))) (* non-negative *)
                        | Syntax.IdentifierLabel s => J.ConstExp (J.asciiStringAsWide s)
      in case result of
             SOME result => ConstStat (result, J.IndexExp (doValue record, label)) :: doCExp ctx env cont
           | NONE => doCExp ctx env cont
      end
  | doCExp ctx env (C.App { applied, cont, exnCont, args })
    = [ J.ReturnStat (SOME (J.ArrayExp (vector [J.ConstExp J.False, doValue applied, J.ArrayExp (vector (doCVar cont :: doCVar exnCont :: List.map doValue args))]))) ]
  | doCExp ctx env (C.AppCont { applied, args })
    = applyCont (ctx, env, applied, List.map doValue args)
  | doCExp ctx env (C.If { cond, thenCont, elseCont })
    = [ J.IfStat (doValue cond, vector (doCExp ctx env thenCont), vector (doCExp ctx env elseCont)) ]
  | doCExp ctx env (C.Let { exp = C.Abs { contParam, exnContParam, params, body }, result, cont, exnCont })
    = (case result of
           SOME result => let val env' = C.CVarMap.insert (C.CVarMap.insert (env, contParam, TAILCALL), exnContParam, TAILCALL)
                              val dec = ConstStat (result, J.FunctionExp (vector (CVarToJs contParam :: CVarToJs exnContParam :: List.map VIdToJs params), vector (doCExp ctx env' body)))
                          in dec :: doCExp ctx env cont
                          end
         | NONE => doCExp ctx env cont
      )
  | doCExp ctx env (C.LetRec { defs, cont })
    = let val (decs, assignments) = List.foldr (fn ((vid, k, h, params, body), (decs, assignments)) =>
                                                   let val env' = C.CVarMap.insert (C.CVarMap.insert (env, k, TAILCALL), h, TAILCALL)
                                                   in (J.LetStat (vector [(vid, NONE)]) :: decs, J.AssignStat (J.VarExp (J.UserDefinedId vid), J.FunctionExp (vector (CVarToJs k :: CVarToJs h :: List.map VIdToJs params), vector (doCExp ctx env' body))) :: assignments) (* in fact, ConstStat can be used *)
                                                   end
                                               ) ([], []) defs
      in decs @ assignments @ doCExp ctx env cont
      end
  | doCExp ctx env (C.LetCont { name, params, body, cont })
    = (case C.CVarMap.find (#contEscapeMap ctx, name) of
           SOME false => let val dec = if List.null params then
                                           []
                                       else
                                           [J.LetStat (vector (List.map (fn p => (p, NONE)) params))]
                             val newEnv = C.CVarMap.insert (env, name, BREAK_TO { label = CVarToJs name, which = NONE, params = List.map VIdToJs params })
                         in dec @ J.BlockStat (SOME (CVarToJs name), vector (doCExp ctx newEnv cont))
                            :: doCExp ctx env body
                         end
         | _ => let val dec = ConstStat (CVarToId name, J.FunctionExp (vector (List.map VIdToJs params), vector (doCExp ctx env body)))
                    val env' = C.CVarMap.insert (env, name, TAILCALL)
                in dec :: doCExp ctx env' cont
                end
      )
  | doCExp ctx env (C.LetRecCont { defs, cont })
    = if List.all (fn (name, _, _) => case C.CVarMap.find (#contEscapeMap ctx, name) of SOME false => true | _ => false) defs then
          let val blockLabel = J.UserDefinedId (genSym ctx)
              val loopLabel = J.UserDefinedId (genSym ctx)
              val which = genSym ctx
              val which' = J.UserDefinedId which
              val maxargs = List.foldl (fn ((_, params, _), n) => Int.max (n, List.length params)) 0 defs
              val commonParams = List.tabulate (maxargs, fn _ => genSym ctx)
              val vars = which :: commonParams
              val (_, contEnv) = List.foldl (fn ((name, params, _), (i, e)) => (i + 1, C.CVarMap.insert (e, name, BREAK_TO { label = blockLabel, which = SOME (which', J.Numeral (Int.toString i)), params = List.map VIdToJs (List.take (commonParams, List.length params)) }))) (0, env) defs
              val (n, recEnv) = List.foldl (fn ((name, params, _), (i, e)) => (i + 1, C.CVarMap.insert (e, name, CONTINUE_TO { label = loopLabel, which = SOME (which', J.Numeral (Int.toString i)), params = List.map VIdToJs (List.take (commonParams, List.length params)) }))) (0, env) defs
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
      else
          let val env' = List.foldl (fn ((name, params, _), e) => C.CVarMap.insert (e, name, TAILCALL)) env defs
              val (decs, assignments) = List.foldr (fn ((name, params, body), (decs, assignments)) =>
                                                       (J.LetStat (vector [(CVarToId name, NONE)]) :: decs, J.AssignStat (doCVar name, J.FunctionExp (vector (List.map VIdToJs params), vector (doCExp ctx env' body))) :: assignments) (* in fact, ConstStat can be used *)
                                                   ) ([], []) defs
          in decs @ assignments @ doCExp ctx env' cont
          end

fun doProgram ctx env cont exnCont cexp = let val env' = C.CVarMap.insert (C.CVarMap.insert (env, cont, TAILCALL), exnCont, TAILCALL)
                                          in vector [J.ExpStat (J.CallExp (J.VarExp (J.PredefinedId "_run"), vector [J.FunctionExp (vector [CVarToJs cont, CVarToJs exnCont], vector (doCExp ctx env' cexp))]))]
                                          end
end;
