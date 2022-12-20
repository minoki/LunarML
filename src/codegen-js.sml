(*
 * Copyright (c) 2022 ARATA Mizuki
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
 * exn -> object
 * bool -> boolean
 * ref -> { tag: "ref", payload: <mutable> }
 * list -> { tag: "nil" } | { tag: "::", payload: ... }
 * tuple -> immutable Array
 * non-tuple record -> immutable object, with integer index starting with 0
 * vector -> immutable Array
 * array -> mutable Array
 * function -> function with arity 1, with optional _MLTAIL_ field
 *)
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
fun VIdToJs (vid as TypedSyntax.MkVId (name, n)) = if n < 0 then
                                                       case TypedSyntax.VIdMap.find (builtins, vid) of
                                                           NONE => raise Fail ("the built-in symbol " ^ name ^ "@" ^ Int.toString n ^ " is not supported by JavaScript backend")
                                                         | SOME jsName => JsSyntax.PredefinedId jsName
                                                   else
                                                       JsSyntax.UserDefinedId vid

fun LabelToObjectKey (Syntax.NumericLabel n) = JsSyntax.IntKey (n - 1)
  | LabelToObjectKey (Syntax.IdentifierLabel s) = JsSyntax.StringKey s

type Context = { nextJsId : int ref }
type Env = { hoistedSymbols : TypedSyntax.VIdSet.set
           , level : int
           }
val initialEnv : Env = { hoistedSymbols = TypedSyntax.VIdSet.empty
                       , level = 0
                       }
fun addSymbol ({ hoistedSymbols, level } : Env, s)
    = { hoistedSymbols = hoistedSymbols
      , level = level
      }
fun addHoistedSymbol ({ hoistedSymbols, level } : Env, s : TypedSyntax.VId)
    = { hoistedSymbols = TypedSyntax.VIdSet.add (hoistedSymbols, s)
      , level = level
      }
fun isHoisted ({ hoistedSymbols, ... } : Env, s : TypedSyntax.VId)
    = TypedSyntax.VIdSet.member (hoistedSymbols, s)
fun declareIfNotHoisted (env : Env, vars : TypedSyntax.VId list) : Env * JsSyntax.Stat list
    = let val (env, vars) = List.foldr (fn (v, (env, xs)) => if isHoisted (env, v) then
                                                                 (env, xs)
                                                             else
                                                                 (addHoistedSymbol (env, v), (v, NONE) :: xs)) (env, []) vars
      in (env, case vars of
                   [] => []
                 | _ => [ JsSyntax.LetStat (vector vars) ]
         )
      end
fun increaseLevel ({ hoistedSymbols, level } : Env) = { hoistedSymbols = hoistedSymbols, level = level + 1 }

fun genSym (ctx: Context) = let val n = !(#nextJsId ctx)
                                val _ = #nextJsId ctx := n + 1
                            in TypedSyntax.MkVId ("tmp", n)
                            end

structure F = FSyntax
structure J = JsSyntax

datatype Destination = Return
                     | AssignTo of TypedSyntax.VId
                     | DeclareAndAssignTo of { level : int, destination : TypedSyntax.VId }
                     | Discard
                     | Continue of (* statements *) J.Stat list * Env * (* pure expression *) J.Exp -> J.Stat list (* the continuation should be called exactly once, and the expression should be used only once *)

(* mapCont : ('a * ('b -> 'r) -> 'r) -> 'a list -> ('b list -> 'r) -> 'r *)
fun mapCont f [] cont = cont []
  | mapCont f (x :: xs) cont = f (x, fn y => mapCont f xs (fn ys => cont (y :: ys)))

(* doExpTo : Context -> Env -> F.Exp -> Destination -> J.Stat list *)
fun putPureTo ctx env Return (stmts, exp : J.Exp) = stmts @ [ J.ReturnStat (SOME (J.ArrayExp (vector [J.ConstExp J.True, exp]))) ]
  | putPureTo ctx env (AssignTo v) (stmts, exp) = stmts @ [ J.ExpStat (J.BinExp (J.ASSIGN, J.VarExp (J.UserDefinedId v), exp)) ]
  | putPureTo ctx env (DeclareAndAssignTo { level, destination }) (stmts, exp) = if #level env = level then
                                                                                     stmts @ [ J.ConstStat (vector [(destination, exp)]) ]
                                                                                 else
                                                                                     raise CodeGenError "invalid DeclareAndAssignTo"
  | putPureTo ctx env Discard (stmts, exp) = stmts
  | putPureTo ctx env (Continue cont) (stmts, exp) = cont (stmts, env, exp)
and putImpureTo ctx env Return (stmts, exp : J.Exp) = stmts @ [ J.ReturnStat (SOME (J.ArrayExp (vector [J.ConstExp J.True, exp]))) ]
  | putImpureTo ctx env (AssignTo v) (stmts, exp) = stmts @ [ J.ExpStat (J.BinExp (J.ASSIGN, J.VarExp (J.UserDefinedId v), exp)) ]
  | putImpureTo ctx env (DeclareAndAssignTo { level, destination }) (stmts, exp) = if #level env = level then
                                                                                       stmts @ [ J.ConstStat (vector [(destination, exp)]) ]
                                                                                   else
                                                                                       raise CodeGenError "invalid DeclareAndAssignTo"
  | putImpureTo ctx env Discard (stmts, exp) = stmts @ [ J.ExpStat exp ]
  | putImpureTo ctx env (Continue cont) (stmts, exp) = let val dest = genSym ctx
                                                           val env = addSymbol (env, dest)
                                                       in cont (stmts @ [ J.ConstStat (vector [(dest, exp)]) ], env, J.VarExp (J.UserDefinedId dest))
                                                       end
and doExpCont ctx env exp (cont : J.Stat list * Env * J.Exp -> J.Stat list) = doExpTo ctx env exp (Continue cont)
and doExpTo ctx env (F.PrimExp (F.IntConstOp x, tys, [])) dest : J.Stat list
    = (case tys of
           [F.TyVar tv] => let val suffix = if TypedSyntax.eqUTyVar (tv, F.tyNameToTyVar Typing.primTyName_int) then
                                                ""
                                            else if TypedSyntax.eqUTyVar (tv, F.tyNameToTyVar Typing.primTyName_intInf) then
                                                "n"
                                            else
                                                raise CodeGenError "PrimExp.IntConstOp: invalid type"
                               val exp = if x < 0 then
                                             J.UnaryExp (J.NEGATE, J.ConstExp (J.Numeral (LargeInt.toString (~ x) ^ suffix)))
                                         else
                                             J.ConstExp (J.Numeral (LargeInt.toString x ^ suffix))
                           in putPureTo ctx env dest ([], exp)
                           end
         | _ => raise CodeGenError "PrimExp.IntConstOp: invalid type"
      )
  | doExpTo ctx env (F.PrimExp (F.IntConstOp x, _, _)) dest = raise CodeGenError "PrimExp.IntConstOp: non-empty argument"
  | doExpTo ctx env (F.PrimExp (F.WordConstOp x, _, [])) dest
    = let val exp = J.ConstExp (J.Numeral ("0x" ^ LargeInt.fmt StringCvt.HEX x))
      in putPureTo ctx env dest ([], exp)
      end
  | doExpTo ctx env (F.PrimExp (F.WordConstOp x, _, _)) dest = raise CodeGenError "PrimExp.WordConstOp: non-empty argument"
  | doExpTo ctx env (F.PrimExp (F.RealConstOp x, _, [])) dest
    = let val exp = let val y = Numeric.toDecimal { nominal_format = Numeric.binary64, target_format = Numeric.binary64 } x
                        (* JavaScript does not support hexadecimal floating-point literals *)
                    in case y of
                           SOME z => if Numeric.Notation.isNegative z then
                                         J.UnaryExp (J.NEGATE, J.ConstExp (J.Numeral (Numeric.Notation.toString "-" (Numeric.Notation.abs z))))
                                     else
                                         J.ConstExp (J.Numeral (Numeric.Notation.toString "-" z))
                         | NONE => raise CodeGenError "the hexadecimal floating-point value cannot be represented as a 64-bit floating-point number"
                    end
      in putPureTo ctx env dest ([], exp)
      end
  | doExpTo ctx env (F.PrimExp (F.RealConstOp x, _, _)) dest = raise CodeGenError "PrimExp.RealConstOp: non-empty argument"
  | doExpTo ctx env (F.PrimExp (F.StringConstOp x, tys, [])) dest
    = let val exp = case tys of
                        [F.TyVar tv] => if tv = F.tyNameToTyVar Typing.primTyName_string then
                                            J.MethodExp (J.VarExp (J.PredefinedId "Uint8Array"), "of", Vector.map (J.ConstExp o J.Numeral o Int.toString) x)
                                        else if tv = F.tyNameToTyVar Typing.primTyName_string16 then
                                            J.ConstExp (J.WideString x)
                                        else
                                            raise CodeGenError "PrimExp.StringConstOp: invalid type"
                      | _ => raise CodeGenError "PrimExp.StringConstOp: invalid type"
      in putPureTo ctx env dest ([], exp)
      end
  | doExpTo ctx env (F.PrimExp (F.StringConstOp x, _, _)) dest = raise CodeGenError "PrimExp.StringConstOp: invalid argument"
  | doExpTo ctx env (F.PrimExp (F.CharConstOp x, tys, [])) dest
    = let val exp = case tys of
                        [F.TyVar tv] => if tv = F.tyNameToTyVar Typing.primTyName_char then
                                            J.ConstExp (J.Numeral (Int.toString x))
                                        else if tv = F.tyNameToTyVar Typing.primTyName_char16 then
                                            J.ConstExp (J.WideString (vector [x]))
                                        else
                                            raise CodeGenError "PrimExp.CharConstOp: invalid type"
                      | _ => raise CodeGenError "PrimExp.CharConstOp: invalid type"
      in putPureTo ctx env dest ([], exp)
      end
  | doExpTo ctx env (F.PrimExp (F.CharConstOp x, tys, xs)) dest = raise CodeGenError "PrimExp.StringConstOp: invalid argument"
  | doExpTo ctx env (F.VarExp vid) dest = putPureTo ctx env dest ([], case VIdToJs vid of
                                                                          J.PredefinedId "null" => J.ConstExp J.Null
                                                                        | J.PredefinedId "false" => J.ConstExp J.False
                                                                        | J.PredefinedId "true" => J.ConstExp J.True
                                                                        | id => J.VarExp id
                                                                 )
  | doExpTo ctx env (F.RecordExp []) dest = putPureTo ctx env dest ([], J.VarExp (J.PredefinedId "undefined"))
  | doExpTo ctx env (F.RecordExp fields) Discard = List.concat (List.map (fn (_, exp) => doExpTo ctx env exp Discard) fields)
  | doExpTo ctx env (F.RecordExp fields) dest
    = mapCont (fn ((label, exp), cont) => doExpCont ctx env exp (fn (stmts, env, e) => cont (stmts, (label, e))))
              fields
              (fn ys => let val (stmts, fields') = ListPair.unzip ys
                            fun isTuple (_, []) = true
                              | isTuple (i, (Syntax.NumericLabel n, x) :: xs) = i = n andalso isTuple (i + 1, xs)
                              | isTuple _ = false
                        in putPureTo ctx env dest ( List.concat stmts
                                                  , if isTuple (1, fields') then
                                                        J.ArrayExp (vector (List.map #2 fields'))
                                                    else
                                                        J.ObjectExp (vector (List.map (fn (label, exp) => (LabelToObjectKey label, exp)) fields'))
                                                  )
                        end
              )
  | doExpTo ctx env (F.LetExp (dec, exp)) (Continue cont)
    = let val dec' = doDec ctx env dec
      in doExpCont ctx env exp (fn (stmts, env, exp) => cont (dec' @ stmts, env, exp)) (* TODO: modify environment *)
      end
  | doExpTo ctx env (F.LetExp (dec, exp)) dest
    = let val dec' = doDec ctx env dec
      in dec' @ doExpTo ctx env exp dest
      end
  | doExpTo ctx env (F.AppExp (exp1, exp2)) dest
    = doExpCont ctx env exp1
                (fn (stmts1, env, e1') =>
                    doExpCont ctx env exp2
                              (fn (stmts2, env, e2') =>
                                  case dest of
                                      Return => stmts1 @ stmts2 @ [ J.ReturnStat (SOME (J.ArrayExp (vector [J.ConstExp J.False, e1', e2']))) ]
                                    | _ => putImpureTo ctx env dest (stmts1 @ stmts2, J.CallExp (e1', vector [e2']))
                              )
                )
  | doExpTo ctx env (F.HandleExp { body, exnName, handler }) dest
    = (case dest of
           Continue cont => let val result = genSym ctx
                                val env' = addSymbol (env, result)
                                val env'' = addSymbol (env', exnName)
                            in cont ( [ J.LetStat (vector [(result, NONE)])
                                      , J.TryCatchStat (vector (doExpTo ctx (increaseLevel env') body (AssignTo result)), exnName, vector (doExpTo ctx (increaseLevel env'') handler (AssignTo result)))
                                      ]
                                    , env'
                                    , J.VarExp (J.UserDefinedId result)
                                    )
                            end
         | DeclareAndAssignTo { level, destination } => let val env' = addSymbol (env, destination)
                                                            val env'' = addSymbol (env', exnName)
                                                        in [ J.LetStat (vector [(destination, NONE)])
                                                           , J.TryCatchStat (vector (doExpTo ctx (increaseLevel env') body (AssignTo destination)), exnName, vector (doExpTo ctx (increaseLevel env'') handler (AssignTo destination)))
                                                           ]
                                                        end
         (* Return, AssignTo, Discard *)
         | _ => let val env' = addSymbol (env, exnName)
                in [ J.TryCatchStat (vector (doExpTo ctx (increaseLevel env) body dest), exnName, vector (doExpTo ctx (increaseLevel env') handler dest)) ]
                end
      )
  | doExpTo ctx env (F.PrimExp (F.RaiseOp (span as { start as { file, line, column }, ... }), _, [exp])) dest
    = doExpCont ctx env exp
                (fn (stmts, env, exp) =>
                    stmts @ [ J.ThrowStat exp ] (* TODO: location information *)
                )
  | doExpTo ctx env (F.PrimExp (F.RaiseOp (span as { start as { file, line, column }, ... }), _, xs)) dest = raise CodeGenError "PrimExp.RaiseOp: invalid number of arguments"
  | doExpTo ctx env (F.IfThenElseExp (exp1, exp2, exp3)) dest
    = doExpCont ctx env exp1
                (fn (stmts1, env, exp1') =>
                    let fun makeCondExp (exp1', J.ConstExp J.True, J.ConstExp J.False) = exp1'
                          | makeCondExp (exp1', J.ConstExp J.False, J.ConstExp J.True) = J.UnaryExp (J.NOT, exp1')
                          | makeCondExp (exp1', J.ConstExp J.True, exp3'') = J.BinExp (J.OR, exp1', exp3'')
                          | makeCondExp (exp1', J.ConstExp J.False, exp3'') = J.BinExp (J.AND, J.UnaryExp (J.NOT, exp1'), exp3'')
                          | makeCondExp (exp1', exp2'', J.ConstExp J.True) = J.BinExp (J.OR, J.UnaryExp (J.NOT, exp1'), exp2'')
                          | makeCondExp (exp1', exp2'', J.ConstExp J.False) = J.BinExp (J.AND, exp1', exp2'')
                          | makeCondExp (exp1', exp2'', exp3'') = J.CondExp (exp1', exp2'', exp3'')
                        fun tryAssignToCondExp (destination, exp2', exp3') = case (exp2', exp3') of
                                                                                 ([J.ExpStat (J.BinExp (J.ASSIGN, J.VarExp (J.UserDefinedId id2), exp2''))], [J.ExpStat (J.BinExp (J.ASSIGN, J.VarExp (J.UserDefinedId id3), exp3''))]) =>
                                                                                 if id2 = destination andalso id3 = destination then
                                                                                     SOME (makeCondExp (exp1', exp2'', exp3''))
                                                                                 else
                                                                                     NONE
                                                                               | _ => NONE
                        fun tryReturnToCondExp (exp2', exp3') = case (exp2', exp3') of
                                                                    ([J.ReturnStat (SOME exp2'')], [J.ReturnStat (SOME exp3'')]) =>
                                                                    SOME (makeCondExp (exp1', exp2'', exp3''))
                                                                  | _ => NONE
                    in case dest of
                           Continue cont => let val result = genSym ctx
                                                val env' = addSymbol (env, result)
                                                val exp2' = doExpTo ctx (increaseLevel env') exp2 (AssignTo result)
                                                val exp3' = doExpTo ctx (increaseLevel env') exp3 (AssignTo result)
                                            in cont ( stmts1
                                                      @ (case tryAssignToCondExp (result, exp2', exp3') of
                                                             SOME condExp => [ J.ConstStat (vector [(result, condExp)]) ]
                                                           | NONE => [ J.LetStat (vector [(result, NONE)])
                                                                     , J.IfStat (exp1', vector exp2', vector exp3')
                                                                     ]
                                                        )
                                                    , env'
                                                    , J.VarExp (J.UserDefinedId result)
                                                    )
                                            end
                         | DeclareAndAssignTo { level, destination } => let val exp2' = doExpTo ctx (increaseLevel env) exp2 (AssignTo destination)
                                                                            val exp3' = doExpTo ctx (increaseLevel env) exp3 (AssignTo destination)
                                                                        in stmts1
                                                                           @ (case tryAssignToCondExp (destination, exp2', exp3') of
                                                                                  SOME condExp => [ J.ConstStat (vector [(destination, condExp)]) ]
                                                                                | NONE => [ J.LetStat (vector [(destination, NONE)])
                                                                                          , J.IfStat (exp1', vector exp2', vector exp3')
                                                                                          ]
                                                                             )
                                                                        end
                         | dest as AssignTo destination => let val exp2' = doExpTo ctx (increaseLevel env) exp2 dest
                                                               val exp3' = doExpTo ctx (increaseLevel env) exp3 dest
                                                           in stmts1
                                                              @ (case tryAssignToCondExp (destination, exp2', exp3') of
                                                                     SOME condExp => [ J.AssignStat (J.VarExp (J.UserDefinedId destination), condExp) ]
                                                                   | NONE => [ J.IfStat (exp1', vector exp2', vector exp3') ]
                                                                )
                                                           end
                         | Return => let val exp2' = doExpTo ctx (increaseLevel env) exp2 dest
                                         val exp3' = doExpTo ctx (increaseLevel env) exp3 dest
                                     in stmts1
                                        @ (case tryReturnToCondExp (exp2', exp3') of
                                               SOME condExp => [ J.ReturnStat (SOME condExp) ]
                                             | NONE => [ J.IfStat (exp1', vector exp2', vector exp3') ]
                                          )
                                     end
                         | Discard => stmts1
                                      @ [ J.IfStat ( exp1'
                                                   , vector (doExpTo ctx (increaseLevel env) exp2 dest)
                                                   , vector (doExpTo ctx (increaseLevel env) exp3 dest)
                                                   )
                                        ]
                    end
                )
  | doExpTo ctx env (F.CaseExp _) dest = raise Fail "Lua codegen: CaseExp should have been desugared earlier"
  | doExpTo ctx env (F.FnExp (vid, _, exp)) dest = putPureTo ctx env dest ([], J.CallExp (J.VarExp (J.PredefinedId "_wrap"), vector [J.FunctionExp (vector [VIdToJs vid], vector (doExpTo ctx (increaseLevel (addSymbol (env, vid))) exp Return))]))
  | doExpTo ctx env (F.ProjectionExp { label, record, fieldTypes }) dest = doExpCont ctx env record (fn (stmts, env, record') =>
                                                                                                        let val label = case label of
                                                                                                                            Syntax.NumericLabel n => J.ConstExp (J.Numeral (Int.toString (n - 1))) (* non-negative *)
                                                                                                                          | Syntax.IdentifierLabel s => J.ConstExp (J.asciiStringAsWide s)
                                                                                                        in putPureTo ctx env dest (stmts, J.IndexExp (record', label))
                                                                                                        end
                                                                                                    )
  | doExpTo ctx env (F.PrimExp (F.ListOp, _, [])) dest
    = putPureTo ctx env dest ([], J.VarExp (J.PredefinedId "_nil"))
  | doExpTo ctx env (F.PrimExp (F.ListOp, _, xs)) dest
    = mapCont (fn (e, cont) => doExpCont ctx env e (fn (x, _, e) => cont (x, e)))
              xs
              (fn ys => let val stmts = List.foldr (fn ((x, _), acc) => x @ acc) [] ys
                        in putPureTo ctx env dest (stmts, J.CallExp (J.VarExp (J.PredefinedId "_list"), vector [J.ArrayExp (Vector.map #2 (vector ys))]))
                        end
              )
  | doExpTo ctx env (F.PrimExp (F.VectorOp, _, xs)) dest
    = mapCont (fn (e, cont) => doExpCont ctx env e (fn (x, _, e) => cont (x, e)))
              xs
              (fn ys => let val stmts = List.foldr (fn ((x, _), acc) => x @ acc) [] ys
                        in putPureTo ctx env dest (stmts, J.ArrayExp (Vector.map #2 (vector ys)))
                        end
              )
  | doExpTo ctx env (F.TyAbsExp (_, _, exp)) dest = doExpTo ctx env exp dest
  | doExpTo ctx env (F.TyAppExp (exp, _)) dest = doExpTo ctx env exp dest
  | doExpTo ctx env (F.PrimExp (F.DataTagOp info, _, [exp])) dest
    = doExpCont ctx env exp (fn (stmts, env, exp') => putPureTo ctx env dest (stmts, J.IndexExp (exp', J.ConstExp (J.asciiStringAsWide "tag"))))
  | doExpTo ctx env (F.PrimExp (F.DataTagOp info, _, _)) dest = raise CodeGenError "PrimExp.DataTagOp: invalid number of arguments"
  | doExpTo ctx env (F.PrimExp (F.DataPayloadOp info, _, [exp])) dest
    = doExpCont ctx env exp (fn (stmts, env, exp') => putPureTo ctx env dest (stmts, J.IndexExp (exp', J.ConstExp (J.asciiStringAsWide "payload"))))
  | doExpTo ctx env (F.PrimExp (F.DataPayloadOp info, _, _)) dest = raise CodeGenError "PrimExp.DataPayloadOp: invalid number of arguments"
  | doExpTo ctx env (F.PrimExp (F.ExnPayloadOp, _, [exp])) dest
    = doExpCont ctx env exp (fn (stmts, env, exp') => putPureTo ctx env dest (stmts, J.IndexExp (exp', J.ConstExp (J.asciiStringAsWide "payload"))))
  | doExpTo ctx env (F.PrimExp (F.ExnPayloadOp, _, _)) dest = raise CodeGenError "PrimExp.ExnPayloadOp: invalid number of arguments"
  | doExpTo ctx env (F.PackExp { payloadTy, exp, packageTy }) dest = doExpTo ctx env exp dest
  | doExpTo ctx env (F.PrimExp (F.PrimFnOp primOp, _, args)) dest
    = let fun doUnary cont = case args of
                                 [a] => doExpCont ctx env a cont
                               | _ => raise CodeGenError ("primop " ^ Primitives.toString primOp ^ ": invalid number of arguments")
          fun doUnaryExp (f, pure) = doUnary (fn (stmts, env, a) =>
                                                 if pure then
                                                     putPureTo ctx env dest (stmts, f a)
                                                 else
                                                     putImpureTo ctx env dest (stmts, f a)
                                             )
          fun doBinary cont = case args of
                                  [a, b] => doExpCont ctx env a
                                                      (fn (stmts0, env, a) =>
                                                          doExpCont ctx env b
                                                                    (fn (stmts1, env, b) =>
                                                                        cont (stmts0 @ stmts1, env, (a, b))
                                                                    )
                                                      )
                                | _ => raise CodeGenError ("primop " ^ Primitives.toString primOp ^ ": invalid number of arguments")
          fun doBinaryExp (f, pure) = doBinary (fn (stmts, env, (a, b)) =>
                                                   if pure then
                                                       putPureTo ctx env dest (stmts, f (a, b))
                                                   else
                                                       putImpureTo ctx env dest (stmts, f (a, b))
                                               )
          fun doBinaryOp (binop, pure) = doBinaryExp (fn (a, b) => J.BinExp (binop, a, b), pure)
          fun doTernary cont = case args of
                                   [a, b, c] => doExpCont ctx env a
                                                          (fn (stmts0, env, a) =>
                                                              doExpCont ctx env b
                                                                        (fn (stmts1, env, b) =>
                                                                            doExpCont ctx env c
                                                                                      (fn (stmts2, env, c) =>
                                                                                          cont (stmts0 @ stmts1 @ stmts2, env, (a, b, c))
                                                                                      )
                                                                        )
                                                          )
                                 | _ => raise CodeGenError ("primop " ^ Primitives.toString primOp ^ ": invalid number of arguments")
          fun doTernaryExp (f, pure) = doTernary (fn (stmts, env, (a, b, c)) =>
                                                     if pure then
                                                         putPureTo ctx env dest (stmts, f (a, b, c))
                                                     else
                                                         putImpureTo ctx env dest (stmts, f (a, b, c))
                                                 )
      in case primOp of
             Primitives.call2 => doTernaryExp (fn (f, a0, a1) => J.CallExp (f, vector [a0, a1]), false)
           | Primitives.call3 => (case args of
                                      [f, a0, a1, a2] => doExpCont ctx env f
                                                                   (fn (stmts0, env, f) =>
                                                                       doExpCont ctx env a0
                                                                                 (fn (stmts1, env, a0) =>
                                                                                     doExpCont ctx env a1
                                                                                               (fn (stmts2, env, a1) =>
                                                                                                   doExpCont ctx env a2
                                                                                                             (fn (stmts3, env, a2) =>
                                                                                                                 putImpureTo ctx env dest (stmts0 @ stmts1 @ stmts2 @ stmts3, J.CallExp (f, vector [a0, a1, a2]))
                                                                                                             )
                                                                                               )
                                                                                 )
                                                                   )
                                    | _ => raise CodeGenError "primop call3: invalid number of arguments"
                                 )
           | Primitives.List_cons => doBinary (fn (stmts, env, (x, xs)) =>
                                                  putPureTo ctx env dest (stmts, J.ObjectExp (vector [(J.StringKey "tag", J.ConstExp (J.asciiStringAsWide "::"))
                                                                                                     ,(J.StringKey "payload", J.ArrayExp (vector [x, xs]))
                                                                                                     ]
                                                                                             )
                                                                         )
                                              )
           | Primitives.Ref_ref => doUnary (fn (stmts, env, x) =>
                                               (* REPRESENTATION_OF_REF *)
                                               putImpureTo ctx env dest (stmts, J.ObjectExp (vector [(J.StringKey "tag", J.ConstExp (J.asciiStringAsWide "ref"))
                                                                                                    ,(J.StringKey "payload", x)
                                                                                                    ]
                                                                                            )
                                                                        )
                                           )
           | Primitives.Ref_EQUAL => doBinaryOp (J.EQUAL, true)
           | Primitives.Ref_set => doBinary (fn (stmts, env, (a, b)) =>
                                                (* REPRESENTATION_OF_REF *)
                                                let val stmts = stmts @ [ J.ExpStat (J.BinExp (J.ASSIGN, J.IndexExp (a, J.ConstExp (J.asciiStringAsWide "payload")), b)) ]
                                                in putPureTo ctx env dest (stmts, J.UndefinedExp)
                                                end
                                            )
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
           | Primitives.String_HAT => doBinaryExp (fn (a, b) =>  J.CallExp (J.VarExp (J.PredefinedId "_String_append"), vector [a, b]), true)
           | Primitives.String_size => doUnaryExp (fn a => J.IndexExp (a, J.ConstExp (J.asciiStringAsWide "length")), true)
           | Primitives.String_str => doUnaryExp (fn a => J.MethodExp (J.VarExp (J.PredefinedId "Uint8Array"), "of", vector [a]), true)
           | Primitives.String16_EQUAL => doBinaryOp (J.EQUAL, true)
           | Primitives.String16_LT => doBinaryOp (J.LT, true)
           | Primitives.String16_GT => doBinaryOp (J.GT, true)
           | Primitives.String16_LE => doBinaryOp (J.LE, true)
           | Primitives.String16_GE => doBinaryOp (J.GE, true)
           | Primitives.String16_HAT => doBinaryOp (J.PLUS, true)
           | Primitives.String16_size => doUnaryExp (fn a => J.IndexExp (a, J.ConstExp (J.asciiStringAsWide "length")), true)
           | Primitives.String16_str => (case args of
                                             [a] => doExpTo ctx env a dest
                                           | _ => raise CodeGenError ("primop " ^ Primitives.toString primOp ^ ": invalid number of arguments")
                                        )
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
           | Primitives.Unsafe_cast => (case args of
                                            [a] => doExpTo ctx env a dest
                                          | _ => raise CodeGenError ("primop " ^ Primitives.toString primOp ^ ": invalid number of arguments")
                                       )
           | Primitives.Unsafe_Vector_sub => doBinaryExp (fn (vec, i) => J.IndexExp (vec, i), true)
           | Primitives.Unsafe_Array_sub => doBinaryExp (fn (arr, i) => J.IndexExp (arr, i), false)
           | Primitives.Unsafe_Array_update => doTernary (fn (stmts, env, (arr, i, v)) =>
                                                             let val stmts = stmts @ [ J.AssignStat (J.IndexExp (arr, i), v) ]
                                                             in putPureTo ctx env dest (stmts, J.UndefinedExp)
                                                             end
                                                         )
           | Primitives.Exception_instanceof => doBinaryExp (fn (e, tag) => J.BinExp (J.INSTANCEOF, e, tag), true)
           | Primitives.assumeDiscardable => doBinary (fn (stmts, env, (f, arg)) =>
                                                          putImpureTo ctx env dest (stmts, J.CallExp (f, vector [arg]))
                                                      )
           | Primitives.JavaScript_sub => doBinaryExp (fn (a, b) => J.IndexExp (a, b), false)
           | Primitives.JavaScript_set => doTernary (fn (stmts, env, (a, b, c)) =>
                                                        let val stmts = stmts @ [ J.AssignStat (J.IndexExp (a, b), c) ]
                                                        in putPureTo ctx env dest (stmts, J.UndefinedExp)
                                                        end
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
           | Primitives.JavaScript_call => (case args of
                                                [f, F.PrimExp (F.VectorOp, _, args)] =>
                                                doExpCont ctx env f
                                                          (fn (stmts1, env, f) =>
                                                              mapCont (fn (e, cont) => doExpCont ctx env e (fn (x, _, e) => cont (x, e)))
                                                                      args
                                                                      (fn args => let val stmts2 = List.foldr (fn ((x, _), acc) => x @ acc) [] args
                                                                                      val args = Vector.map #2 (vector args)
                                                                                  in putImpureTo ctx env dest (stmts1 @ stmts2, J.CallExp (f, args))
                                                                                  end
                                                                      )
                                                          )
                                              | [f, args] =>
                                                doExpCont ctx env f
                                                          (fn (stmts0, env, f) =>
                                                              doExpCont ctx env args
                                                                        (fn (stmts1, env, args) =>
                                                                            putImpureTo ctx env dest (stmts0 @ stmts1, J.MethodExp (f, "apply", vector [J.UndefinedExp, args]))
                                                                        )
                                                          )
                                              | _ => raise CodeGenError ("primop " ^ Primitives.toString primOp ^ ": invalid number of arguments")
                                           )
           | Primitives.JavaScript_method => (case args of
                                                  [obj, name, F.PrimExp (F.VectorOp, _, args)] =>
                                                  doExpCont ctx env obj
                                                            (fn (stmts1, env, obj) =>
                                                                doExpCont ctx env name
                                                                          (fn (stmts2, env, name) =>
                                                                              mapCont (fn (e, cont) => doExpCont ctx env e (fn (x, _, e) => cont (x, e)))
                                                                                      args
                                                                                      (fn args => let val stmts3 = List.foldr (fn ((x, _), acc) => x @ acc) [] args
                                                                                                      val args = Vector.map #2 (vector args)
                                                                                                  in putImpureTo ctx env dest (stmts1 @ stmts2 @ stmts3, J.CallExp (J.IndexExp (obj, name), args))
                                                                                                  end
                                                                                      )
                                                                          )
                                                            )
                                                | [obj, name, args] =>
                                                  doExpCont ctx env obj
                                                            (fn (stmts0, env, obj) =>
                                                                doExpCont ctx env name
                                                                          (fn (stmts0, env, name) =>
                                                                              doExpCont ctx env args
                                                                                        (fn (stmts1, env, args) =>
                                                                                            putImpureTo ctx env dest (stmts0 @ stmts1, J.MethodExp (J.IndexExp (obj, name), "apply", vector [obj, args]))
                                                                                        )
                                                                          )
                                                            )
                                                | _ => raise CodeGenError ("primop " ^ Primitives.toString primOp ^ ": invalid number of arguments")
                                             )
           | Primitives.JavaScript_new => (case args of
                                               [ctor, F.PrimExp (F.VectorOp, _, args)] =>
                                               doExpCont ctx env ctor
                                                         (fn (stmts1, env, ctor) =>
                                                             mapCont (fn (e, cont) => doExpCont ctx env e (fn (x, _, e) => cont (x, e)))
                                                                     args
                                                                     (fn args => let val stmts2 = List.foldr (fn ((x, _), acc) => x @ acc) [] args
                                                                                     val args = Vector.map #2 (vector args)
                                                                                 in putImpureTo ctx env dest (stmts1 @ stmts2, J.NewExp (ctor, args))
                                                                                 end
                                                                     )
                                                         )
                                             | [ctor, args] =>
                                               doExpCont ctx env ctor
                                                         (fn (stmts0, env, ctor) =>
                                                             doExpCont ctx env args
                                                                       (fn (stmts1, env, args) =>
                                                                           putImpureTo ctx env dest (stmts0 @ stmts1, J.MethodExp (J.VarExp (J.PredefinedId "Reflect"), "construct", vector [ctor, args]))
                                                                       )
                                                         )
                                             | _ => raise CodeGenError ("primop " ^ Primitives.toString primOp ^ ": invalid number of arguments")
                                          )
           | _ => raise CodeGenError ("primop " ^ Primitives.toString primOp ^ " is not supported on JavaScript backend")
      end
  | doExpTo ctx env (F.PrimExp (F.ConstructValOp info, _, _)) dest
    = let val tag = #tag info
      in putPureTo ctx env dest ([], J.ObjectExp (vector [(J.StringKey "tag", J.ConstExp (J.asciiStringAsWide tag))]))
      end
  | doExpTo ctx env (F.PrimExp (F.ConstructValWithPayloadOp info, _, [payload])) dest
    = let val tag = #tag info
      in doExpCont ctx env payload (fn (stmts, env, payload) =>
                                       putPureTo ctx env dest (stmts, J.ObjectExp (vector [(J.StringKey "tag", J.ConstExp (J.asciiStringAsWide tag)), (J.StringKey "payload", payload)]))
                                   )
      end
  | doExpTo ctx env (F.PrimExp (F.ConstructValWithPayloadOp info, _, _)) dest = raise CodeGenError "ConstructValWithPayloadOp: invalid number of arguments"
  | doExpTo ctx env (F.PrimExp (F.ConstructExnOp, _, [tag])) dest
    = doExpCont ctx env tag (fn (stmts, env, tag) =>
                                putPureTo ctx env dest (stmts, J.NewExp (tag, vector []))
                            )
  | doExpTo ctx env (F.PrimExp (F.ConstructExnOp, _, _)) dest = raise CodeGenError "ConstructExnOp: invalid number of arguments"
  | doExpTo ctx env (F.PrimExp (F.ConstructExnWithPayloadOp, _, [tag, payload])) dest
    = doExpCont ctx env tag (fn (stmts0, env, tag) =>
                                doExpCont ctx env payload (fn (stmts1, env, payload) =>
                                                              putPureTo ctx env dest (stmts0 @ stmts1, J.NewExp (tag, vector [payload]))
                                                          )
                            )
  | doExpTo ctx env (F.PrimExp (F.ConstructExnWithPayloadOp, _, _)) dest = raise CodeGenError "ConstructExnWithPayloadOp: invalid number of arguments"
  | doExpTo ctx env (F.PrimExp (F.JsCallOp, _, f :: args)) dest
    = doExpCont ctx env f (fn (stmts0, env, f) =>
                              mapCont (fn (a, cont) => doExpCont ctx env a (fn (stmts, env, a) => cont (stmts, a)))
                                      args
                                      (fn args => let val (stmts1, args) = ListPair.unzip args
                                                  in putImpureTo ctx env dest (stmts0 @ List.concat stmts1, J.CallExp (f, vector args))
                                                  end
                                      )
                          )
  | doExpTo ctx env (F.PrimExp (F.JsCallOp, _, [])) dest = raise CodeGenError "JsCallOp: invalid number of arguments"
  | doExpTo ctx env (F.PrimExp (F.JsMethodOp, _, obj :: name :: args)) dest
    = doExpCont ctx env obj
                (fn (stmts0, env, obj) =>
                    doExpCont ctx env name
                              (fn (stmts1, env, name) =>
                                  mapCont (fn (a, cont) => doExpCont ctx env a (fn (stmts, env, a) => cont (stmts, a)))
                                          args
                                          (fn args => let val (stmts2, args) = ListPair.unzip args
                                                      in putImpureTo ctx env dest (stmts0 @ stmts1 @ List.concat stmts2, J.CallExp (J.IndexExp (obj, name), vector args))
                                                      end
                                          )
                              )
                )
  | doExpTo ctx env (F.PrimExp (F.JsMethodOp, _, _)) dest = raise CodeGenError "JsMethodOp: invalid number of arguments"
  | doExpTo ctx env (F.PrimExp (F.JsNewOp, _, ctor :: args)) dest
    = doExpCont ctx env ctor (fn (stmts0, env, ctor) =>
                                 mapCont (fn (a, cont) => doExpCont ctx env a (fn (stmts, env, a) => cont (stmts, a)))
                                         args
                                         (fn args => let val (stmts1, args) = ListPair.unzip args
                                                     in putImpureTo ctx env dest (stmts0 @ List.concat stmts1, J.NewExp (ctor, vector args))
                                                     end
                                         )
                             )
  | doExpTo ctx env (F.PrimExp (F.JsNewOp, _, [])) dest = raise CodeGenError "JsNewOp: invalid number of arguments"
(* doDec : Context -> Env -> F.Dec -> J.Stat list *)
and doDec ctx env (F.ValDec (vid, _, exp)) : J.Stat list
    = if isHoisted (env, vid) then
          doExpTo ctx env exp (AssignTo vid)
      else
          doExpTo ctx env exp (DeclareAndAssignTo { level = #level env, destination = vid })
  | doDec ctx env (F.RecValDec valbinds)
    = let val (decs, assignments) = List.foldr (fn ((vid, _, exp), (decs, assignments)) => let val (env, dec) = declareIfNotHoisted (env, [vid])
                                                                                           in (dec @ decs, doExpTo ctx env exp (AssignTo vid) @ assignments)
                                                                                           end
                                               ) ([], []) valbinds
      in decs @ assignments
      end
  | doDec ctx env (F.UnpackDec (tv, kind, vid, ty, exp))
    = if isHoisted (env, vid) then
          doExpTo ctx env exp (AssignTo vid)
      else
          doExpTo ctx env exp (DeclareAndAssignTo { level = #level env, destination = vid })
  | doDec ctx env (F.IgnoreDec exp) = doExpTo ctx env exp Discard
  | doDec ctx env (F.DatatypeDec datbinds) = [] (* no runtime counterpart *)
  | doDec ctx env (F.ExceptionDec { name, tagName, payloadTy })
    = [ let val value = case payloadTy of
                            NONE => J.FunctionExp (vector [], vector [])
                          | SOME _ => J.FunctionExp (vector [J.PredefinedId "payload"], vector [J.AssignStat (J.IndexExp (J.ThisExp, J.ConstExp (J.asciiStringAsWide "payload")), J.VarExp (J.PredefinedId "payload"))])
        in if isHoisted (env, tagName) then
               J.AssignStat (J.VarExp (J.UserDefinedId tagName), value)
           else
               J.ConstStat (vector [(tagName, value)])
        end
      , J.AssignStat (J.IndexExp (J.IndexExp (J.VarExp (J.UserDefinedId tagName), J.ConstExp (J.asciiStringAsWide "prototype")), J.ConstExp (J.asciiStringAsWide "name")), J.ConstExp (J.asciiStringAsWide name))
      ]
  | doDec ctx env (F.ExportValue _) = raise CodeGenError "internal error: ExportValue must be the last statement"
  | doDec ctx env (F.ExportModule _) = raise CodeGenError "internal error: ExportModule must be the last statement"
  | doDec ctx env (F.GroupDec (_, decs)) = doDecs ctx env decs
and doDecs ctx env [F.ExportValue exp] = raise CodeGenError "ExportValue is not supported yet"
  | doDecs ctx env [F.ExportModule fields] = raise CodeGenError "ExportModule is not supported yet"
  | doDecs ctx env (dec :: decs) = doDec ctx env dec @ doDecs ctx env decs
  | doDecs ctx env [] = []

fun doProgram ctx env decs = vector (doDecs ctx env decs)

end;
