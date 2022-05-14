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
                    ,(VId_JavaScript_call, "_call")
                    ,(VId_JavaScript_new, "_new")
                    ,(VId_JavaScript_method, "_method")
                    ,(VId_JavaScript_encodeUtf8, "_encodeUtf8")
                    ,(VId_JavaScript_decodeUtf8, "_decodeUtf8")
                    ,(VId_JavaScript_require, "require")
                    (* extra *)
                    ,(VId_assumePure, "_id") (* no-op *)
                    ,(VId_assumeDiscardable, "_id") (* no-op *)
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
                 | _ => [ JsSyntax.VarStat (vector vars) ]
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
                                                                                     stmts @ [ J.VarStat (vector [(destination, SOME exp)]) ]
                                                                                 else
                                                                                     raise CodeGenError "invalid DeclareAndAssignTo"
  | putPureTo ctx env Discard (stmts, exp) = stmts
  | putPureTo ctx env (Continue cont) (stmts, exp) = cont (stmts, env, exp)
and putImpureTo ctx env Return (stmts, exp : J.Exp) = stmts @ [ J.ReturnStat (SOME (J.ArrayExp (vector [J.ConstExp J.True, exp]))) ]
  | putImpureTo ctx env (AssignTo v) (stmts, exp) = stmts @ [ J.ExpStat (J.BinExp (J.ASSIGN, J.VarExp (J.UserDefinedId v), exp)) ]
  | putImpureTo ctx env (DeclareAndAssignTo { level, destination }) (stmts, exp) = if #level env = level then
                                                                                       stmts @ [ J.VarStat (vector [(destination, SOME exp)]) ]
                                                                                   else
                                                                                       raise CodeGenError "invalid DeclareAndAssignTo"
  | putImpureTo ctx env Discard (stmts, exp) = stmts @ [ J.ExpStat exp ]
  | putImpureTo ctx env (Continue cont) (stmts, exp) = let val dest = genSym ctx
                                                           val env = addSymbol (env, dest)
                                                       in cont (stmts @ [ J.VarStat (vector [(dest, SOME exp)]) ], env, J.VarExp (J.UserDefinedId dest))
                                                       end
and doExpCont ctx env exp (cont : J.Stat list * Env * J.Exp -> J.Stat list) = doExpTo ctx env exp (Continue cont)
and doExpTo ctx env (F.PrimExp (F.IntConstOp x, tys, xs)) dest : J.Stat list
    = if Vector.length xs = 0 andalso Vector.length tys = 1 then
          case Vector.sub (tys, 0) of
              F.TyVar tv => let val suffix = if TypedSyntax.eqUTyVar (tv, F.tyNameToTyVar Typing.primTyName_int) then
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
      else
          raise CodeGenError "PrimExp.IntConstOp: non-empty argument"
  | doExpTo ctx env (F.PrimExp (F.WordConstOp x, _, xs)) dest
    = if Vector.length xs = 0 then
          let val exp = J.ConstExp (J.Numeral ("0x" ^ LargeInt.fmt StringCvt.HEX x))
          in putPureTo ctx env dest ([], exp)
          end
      else
          raise CodeGenError "PrimExp.WordConstOp: non-empty argument"
  | doExpTo ctx env (F.PrimExp (F.RealConstOp x, _, xs)) dest
    = if Vector.length xs = 0 then
          let val exp = let val y = Numeric.toDecimal { nominal_format = Numeric.binary64, target_format = Numeric.binary64 } x
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
      else
          raise CodeGenError "PrimExp.RealConstOp: non-empty argument"
  | doExpTo ctx env (F.PrimExp (F.StringConstOp x, tys, xs)) dest
    = if Vector.length xs = 0 andalso Vector.length tys = 1 then
          let val ty = Vector.sub (tys, 0)
              val exp = case ty of
                            F.TyVar tv => if tv = F.tyNameToTyVar Typing.primTyName_string then
                                              J.MethodExp (J.VarExp (J.PredefinedId "Uint8Array"), "of", Vector.map (J.ConstExp o J.Numeral o Int.toString) x)
                                          else if tv = F.tyNameToTyVar Typing.primTyName_wideString then
                                              J.ConstExp (J.WideString x)
                                          else
                                              raise CodeGenError "PrimExp.StringConstOp: invalid type"
                          | _ => raise CodeGenError "PrimExp.StringConstOp: invalid type"
          in putPureTo ctx env dest ([], exp)
          end
      else
          raise CodeGenError "PrimExp.StringConstOp: invalid argument"
  | doExpTo ctx env (F.PrimExp (F.CharConstOp x, tys, xs)) dest
    = if Vector.length xs = 0 andalso Vector.length tys = 1 then
          let val ty = Vector.sub (tys, 0)
              val exp = case ty of
                            F.TyVar tv => if tv = F.tyNameToTyVar Typing.primTyName_char then
                                              J.ConstExp (J.Numeral (Int.toString x))
                                          else if tv = F.tyNameToTyVar Typing.primTyName_wideChar then
                                              J.ConstExp (J.WideString (vector [x]))
                                          else
                                              raise CodeGenError "PrimExp.CharConstOp: invalid type"
                          | _ => raise CodeGenError "PrimExp.CharConstOp: invalid type"
          in putPureTo ctx env dest ([], exp)
          end
      else
          raise CodeGenError "PrimExp.StringConstOp: invalid argument"
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
    = let val doJsCall = case (exp1, exp2) of
                              (F.AppExp (F.VarExp vid_jscall, f), F.PrimExp (F.VectorOp, _, xs)) =>
                              if TypedSyntax.eqVId (vid_jscall, InitialEnv.VId_JavaScript_call) then
                                  SOME (fn () => doExpCont ctx env f
                                                           (fn (stmts1, env, f) =>
                                                               mapCont (fn (e, cont) => doExpCont ctx env e (fn (x, _, e) => cont (x, e)))
                                                                       (Vector.foldr (op ::) [] xs)
                                                                       (fn ys => let val stmts2 = List.foldr (fn ((x, _), acc) => x @ acc) [] ys
                                                                                     val zs = Vector.map #2 (vector ys)
                                                                                 in putImpureTo ctx env dest (stmts1 @ stmts2, J.CallExp (f, zs))
                                                                                 end
                                                                       )
                                                           )
                                       )
                              else
                                  NONE
                            | _ => NONE
          val doJsMethod = case (exp1, exp2) of
                               (F.AppExp (F.VarExp vid_jsmethod, F.RecordExp [(Syntax.NumericLabel 1, self), (Syntax.NumericLabel 2, F.PrimExp (F.StringConstOp method, _, _))]), F.PrimExp(F.VectorOp, _, xs)) =>
                                (case SOME (CharVector.tabulate (Vector.length method, fn i => Char.chr (Vector.sub (method, i)))) handle Chr => NONE of
                                     SOME method =>
                                     if TypedSyntax.eqVId (vid_jsmethod, InitialEnv.VId_JavaScript_method) andalso JsWriter.isIdentifier method then
                                         SOME (fn () => doExpCont ctx env self
                                                                  (fn (stmts1, env, self) =>
                                                                      mapCont (fn (e, cont) => doExpCont ctx env e (fn (x, _, e) => cont (x, e)))
                                                                              (Vector.foldr (op ::) [] xs)
                                                                              (fn ys => let val stmts2 = List.foldr (fn ((x, _), acc) => x @ acc) [] ys
                                                                                            val zs = Vector.map #2 (vector ys)
                                                                                        in putImpureTo ctx env dest (stmts1 @ stmts2, J.MethodExp (self, method, zs))
                                                                                        end
                                                                              )
                                                                  )
                                              )
                                     else
                                         NONE
                                   | NONE => NONE
                                )
                              | _ => NONE
          val isNoop = case exp1 of
                           F.TyAppExp (F.VarExp vid, _) => TypedSyntax.eqVId (vid, InitialEnv.VId_assumePure) orelse TypedSyntax.eqVId (vid, InitialEnv.VId_assumeDiscardable)
                         | _ => false
      in case List.mapPartial (fn x => x) [doJsCall, doJsMethod] of
             f :: _ => f ()
           | [] => if isNoop then
                       doExpTo ctx env exp2 dest
                   else
                       doExpCont ctx env exp1
                                 (fn (stmts1, env, e1') =>
                                     doExpCont ctx env exp2
                                               (fn (stmts2, env, e2') =>
                                                   case dest of
                                                       Return => stmts1 @ stmts2 @ [ J.ReturnStat (SOME (J.ArrayExp (vector [J.ConstExp J.False, e1', e2']))) ]
                                                     | _ => putImpureTo ctx env dest (stmts1 @ stmts2, J.CallExp (e1', vector [e2']))
                                               )
                                 )
      end
  | doExpTo ctx env (F.HandleExp { body, exnName, handler }) dest
    = (case dest of
           Continue cont => let val result = genSym ctx
                                val env' = addSymbol (env, result)
                                val env'' = addSymbol (env', exnName)
                            in cont ( [ J.VarStat (vector [(result, NONE)])
                                      , J.TryCatchStat (vector (doExpTo ctx (increaseLevel env') body (AssignTo result)), exnName, vector (doExpTo ctx (increaseLevel env'') handler (AssignTo result)))
                                      ]
                                    , env'
                                    , J.VarExp (J.UserDefinedId result)
                                    )
                            end
         | DeclareAndAssignTo { level, destination } => let val env' = addSymbol (env, destination)
                                                            val env'' = addSymbol (env', exnName)
                                                        in [ J.VarStat (vector [(destination, NONE)])
                                                           , J.TryCatchStat (vector (doExpTo ctx (increaseLevel env') body (AssignTo destination)), exnName, vector (doExpTo ctx (increaseLevel env'') handler (AssignTo destination)))
                                                           ]
                                                        end
         (* Return, AssignTo, Discard *)
         | _ => let val env' = addSymbol (env, exnName)
                in [ J.TryCatchStat (vector (doExpTo ctx (increaseLevel env) body dest), exnName, vector (doExpTo ctx (increaseLevel env') handler dest)) ]
                end
      )
  | doExpTo ctx env (F.PrimExp (F.RaiseOp (span as { start as { file, line, column }, ... }), _, xs)) dest
    = if Vector.length xs = 1 then
          let val exp = Vector.sub (xs, 0)
          in doExpCont ctx env exp
                       (fn (stmts, env, exp) =>
                           stmts @ [ J.ThrowStat exp ] (* TODO: location information *)
                       )
          end
      else
          raise CodeGenError "PrimExp.RaiseOp: invalid number of arguments"
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
                                                             SOME condExp => [ J.VarStat (vector [(result, SOME condExp)]) ]
                                                           | NONE => [ J.VarStat (vector [(result, NONE)])
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
                                                                                  SOME condExp => [ J.VarStat (vector [(destination, SOME condExp)]) ]
                                                                                | NONE => [ J.VarStat (vector [(destination, NONE)])
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
  | doExpTo ctx env (F.ProjectionExp { label, record }) dest = doExpCont ctx env record (fn (stmts, env, record') =>
                                                                                            let val label = case label of
                                                                                                                Syntax.NumericLabel n => J.ConstExp (J.Numeral (Int.toString (n - 1))) (* non-negative *)
                                                                                                              | Syntax.IdentifierLabel s => J.ConstExp (J.asciiStringAsWide s)
                                                                                            in putPureTo ctx env dest (stmts, J.IndexExp (record', label))
                                                                                            end
                                                                                        )
  | doExpTo ctx env (F.PrimExp (F.ListOp, _, xs)) dest
    = if Vector.length xs = 0 then
          putPureTo ctx env dest ([], J.VarExp (J.PredefinedId "_nil"))
      else
          mapCont (fn (e, cont) => doExpCont ctx env e (fn (x, _, e) => cont (x, e)))
                  (Vector.foldr (op ::) [] xs)
                  (fn ys => let val stmts = List.foldr (fn ((x, _), acc) => x @ acc) [] ys
                            in putPureTo ctx env dest (stmts, J.CallExp (J.VarExp (J.PredefinedId "_list"), vector [J.ArrayExp (Vector.map #2 (vector ys))]))
                            end
                  )
  | doExpTo ctx env (F.PrimExp (F.VectorOp, _, xs)) dest
    = mapCont (fn (e, cont) => doExpCont ctx env e (fn (x, _, e) => cont (x, e)))
              (Vector.foldr (op ::) [] xs)
              (fn ys => let val stmts = List.foldr (fn ((x, _), acc) => x @ acc) [] ys
                        in putPureTo ctx env dest (stmts, J.ArrayExp (Vector.map #2 (vector ys)))
                        end
              )
  | doExpTo ctx env (F.TyAbsExp (_, _, exp)) dest = doExpTo ctx env exp dest
  | doExpTo ctx env (F.TyAppExp (exp, _)) dest = doExpTo ctx env exp dest
  | doExpTo ctx env (F.PrimExp (F.RecordEqualityOp, _, xs)) dest
    = if Vector.length xs = 1 then
          let val exp = Vector.sub (xs, 0)
          in case exp of
                 F.RecordExp [] => putPureTo ctx env dest ([], J.VarExp (J.PredefinedId "_Unit_EQUAL"))
               | _ => doExpCont ctx env exp
                                (fn (stmts, env, e') =>
                                    putPureTo ctx env dest (stmts, J.CallExp (J.VarExp (J.PredefinedId "_Record_EQUAL"), vector [e']))
                                )
          end
      else
          raise CodeGenError "PrimExp.RecordEqualityOp: invalid number of arguments"
  | doExpTo ctx env (F.PrimExp (F.DataTagOp info, _, xs)) dest
    = if Vector.length xs = 1 then
          let val exp = Vector.sub (xs, 0)
          in doExpCont ctx env exp (fn (stmts, env, exp') => putPureTo ctx env dest (stmts, J.IndexExp (exp', J.ConstExp (J.asciiStringAsWide "tag"))))
          end
      else
          raise CodeGenError "PrimExp.DataTagOp: invalid number of arguments"
  | doExpTo ctx env (F.PrimExp (F.DataPayloadOp info, _, xs)) dest
    = if Vector.length xs = 1 then
          let val exp = Vector.sub (xs, 0)
          in doExpCont ctx env exp (fn (stmts, env, exp') => putPureTo ctx env dest (stmts, J.IndexExp (exp', J.ConstExp (J.asciiStringAsWide "payload"))))
          end
      else
          raise CodeGenError "PrimExp.DataPayloadOp: invalid number of arguments"
  | doExpTo ctx env (F.PrimExp (F.ExnPayloadOp, _, xs)) dest
    = if Vector.length xs = 1 then
          let val exp = Vector.sub (xs, 0)
          in doExpCont ctx env exp (fn (stmts, env, exp') => putPureTo ctx env dest (stmts, J.IndexExp (exp', J.ConstExp (J.asciiStringAsWide "payload"))))
          end
      else
          raise CodeGenError "PrimExp.ExnPayloadOp: invalid number of arguments"
  | doExpTo ctx env (F.StructExp { valMap, strMap, exnTagMap }) dest
    = let val valMap' = Syntax.VIdMap.listItemsi valMap
          val strMap' = Syntax.StrIdMap.listItemsi strMap
          val exnTagMap' = Syntax.VIdMap.listItemsi exnTagMap
      in mapCont (fn ((label, path), cont) => doExpCont ctx env (F.PathToExp path) (fn (stmts, env, e) => cont (stmts, (label, e))))
                 valMap'
                 (fn valMap' =>
                     let val (stmts, valFields) = ListPair.unzip valMap'
                     in mapCont (fn ((label, path), cont) => doExpCont ctx env (F.PathToExp path) (fn (stmts, env, e) => cont (stmts, (label, e))))
                                strMap'
                                (fn strMap' =>
                                    let val (stmts', strFields) = ListPair.unzip strMap'
                                    in mapCont (fn ((label, path), cont) => doExpCont ctx env (F.PathToExp path) (fn (stmts, env, e) => cont (stmts, (label, e))))
                                               exnTagMap'
                                               (fn exnTagMap' =>
                                                   let val (stmts'', exnTagFields) = ListPair.unzip exnTagMap'
                                                       val valFields = List.map (fn (vid, e) => (J.StringKey (Syntax.getVIdName vid), e)) valFields
                                                       val strFields = List.map (fn (Syntax.MkStrId name, e) => (J.StringKey ("_" ^ name), e)) strFields
                                                       val exnTagFields = List.map (fn (vid, e) => (J.StringKey (Syntax.getVIdName vid ^ ".tag"), e)) exnTagFields
                                                   in putPureTo ctx env dest ( List.concat stmts @ List.concat stmts' @ List.concat stmts''
                                                                             , J.ObjectExp (vector (valFields @ strFields @ exnTagFields))
                                                                             )
                                                   end
                                               )
                                    end
                                )
                     end
                 )
      end
  | doExpTo ctx env (exp as F.SProjectionExp (exp', label)) dest = let val field = case label of
                                                                                       F.ValueLabel vid => Syntax.getVIdName vid
                                                                                     | F.StructLabel (Syntax.MkStrId name) => "_" ^ name
                                                                                     | F.ExnTagLabel vid => Syntax.getVIdName vid ^ ".tag"
                                                                   in doExpCont ctx env exp' (fn (stmts, env, exp') => putPureTo ctx env dest (stmts, J.IndexExp (exp', J.ConstExp (J.asciiStringAsWide field))))
                                                                   end
  | doExpTo ctx env (F.PackExp { payloadTy, exp, packageTy }) dest = doExpTo ctx env exp dest
  | doExpTo ctx env (F.PrimExp (F.PrimFnOp primOp, _, args)) dest
    = let fun doUnary cont = if Vector.length args = 1 then
                                 let val a = Vector.sub (args, 0)
                                 in doExpCont ctx env a cont
                                 end
                             else
                                 raise CodeGenError ("primop " ^ Primitives.toString primOp ^ ": invalid number of arguments")
          fun doUnaryExp (f, pure) = doUnary (fn (stmts, env, a) =>
                                                 if pure then
                                                     putPureTo ctx env dest (stmts, f a)
                                                 else
                                                     putImpureTo ctx env dest (stmts, f a)
                                             )
          fun doBinary cont = if Vector.length args = 2 then
                                  let val a = Vector.sub (args, 0)
                                      val b = Vector.sub (args, 1)
                                  in doExpCont ctx env a (fn (stmts0, env, a) =>
                                                             doExpCont ctx env b (fn (stmts1, env, b) =>
                                                                                     cont (stmts0 @ stmts1, env, (a, b))
                                                                                 )
                                                         )
                                  end
                              else
                                  raise CodeGenError ("primop " ^ Primitives.toString primOp ^ ": invalid number of arguments")
          fun doBinaryExp (f, pure) = doBinary (fn (stmts, env, (a, b)) =>
                                                   if pure then
                                                       putPureTo ctx env dest (stmts, f (a, b))
                                                   else
                                                       putImpureTo ctx env dest (stmts, f (a, b))
                                               )
          fun doBinaryOp (binop, pure) = doBinaryExp (fn (a, b) => J.BinExp (binop, a, b), pure)
          fun doTernary cont = if Vector.length args = 3 then
                                  let val a = Vector.sub (args, 0)
                                      val b = Vector.sub (args, 1)
                                      val c = Vector.sub (args, 2)
                                  in doExpCont ctx env a (fn (stmts0, env, a) =>
                                                             doExpCont ctx env b (fn (stmts1, env, b) =>
                                                                                     doExpCont ctx env c (fn (stmts2, env, c) =>
                                                                                                             cont (stmts0 @ stmts1 @ stmts2, env, (a, b, c))
                                                                                                         )
                                                                                 )
                                                         )
                                  end
                              else
                                  raise CodeGenError ("primop " ^ Primitives.toString primOp ^ ": invalid number of arguments")
          fun doTernaryExp (f, pure) = doTernary (fn (stmts, env, (a, b, c)) =>
                                                     if pure then
                                                         putPureTo ctx env dest (stmts, f (a, b, c))
                                                     else
                                                         putImpureTo ctx env dest (stmts, f (a, b, c))
                                                 )
      in case primOp of
             Primitives.call2 => doTernaryExp (fn (f, a0, a1) => J.CallExp (f, vector [a0, a1]), false)
           | Primitives.call3 => if Vector.length args = 4 then
                                     let val f = Vector.sub (args, 0)
                                         val a0 = Vector.sub (args, 1)
                                         val a1 = Vector.sub (args, 2)
                                         val a2 = Vector.sub (args, 3)
                                     in doExpCont ctx env f (fn (stmts0, env, f) =>
                                                                doExpCont ctx env a0 (fn (stmts1, env, a0) =>
                                                                                         doExpCont ctx env a1 (fn (stmts2, env, a1) =>
                                                                                                                  doExpCont ctx env a2 (fn (stmts3, env, a2) =>
                                                                                                                                           putImpureTo ctx env dest (stmts0 @ stmts1 @ stmts2 @ stmts3, J.CallExp (f, vector [a0, a1, a2]))
                                                                                                                                       )
                                                                                                              )
                                                                                     )
                                                            )
                                     end
                                 else
                                     raise CodeGenError "primop call3: invalid number of arguments"
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
           | Primitives.WideChar_EQUAL => doBinaryOp (J.EQUAL, true)
           | Primitives.WideChar_LT => doBinaryOp (J.LT, true)
           | Primitives.WideChar_GT => doBinaryOp (J.GT, true)
           | Primitives.WideChar_LE => doBinaryOp (J.LE, true)
           | Primitives.WideChar_GE => doBinaryOp (J.GE, true)
           | Primitives.String_EQUAL => doBinaryExp (fn (a, b) =>  J.CallExp (J.VarExp (J.PredefinedId "_String_EQUAL"), vector [a, b]), true)
           | Primitives.String_LT => doBinaryExp (fn (a, b) => J.CallExp (J.VarExp (J.PredefinedId "_String_LT"), vector [a, b]), true)
           | Primitives.String_HAT => doBinaryExp (fn (a, b) =>  J.CallExp (J.VarExp (J.PredefinedId "_String_append"), vector [a, b]), true)
           | Primitives.String_size => doUnaryExp (fn a => J.IndexExp (a, J.ConstExp (J.asciiStringAsWide "length")), true)
           | Primitives.String_str => doUnaryExp (fn a => J.MethodExp (J.VarExp (J.PredefinedId "Uint8Array"), "of", vector [a]), true)
           | Primitives.WideString_EQUAL => doBinaryOp (J.EQUAL, true)
           | Primitives.WideString_LT => doBinaryOp (J.LT, true)
           | Primitives.WideString_GT => doBinaryOp (J.GT, true)
           | Primitives.WideString_LE => doBinaryOp (J.LE, true)
           | Primitives.WideString_GE => doBinaryOp (J.GE, true)
           | Primitives.WideString_HAT => doBinaryOp (J.PLUS, true)
           | Primitives.WideString_size => doUnaryExp (fn a => J.IndexExp (a, J.ConstExp (J.asciiStringAsWide "length")), true)
           | Primitives.WideString_str => if Vector.length args = 1 then
                                              doExpTo ctx env (Vector.sub (args, 0)) dest
                                          else
                                              raise CodeGenError ("primop " ^ Primitives.toString primOp ^ ": invalid number of arguments")
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
           | Primitives.Array_EQUAL => doBinaryOp (J.EQUAL, true)
           | Primitives.Array_length => doUnaryExp (fn a => J.IndexExp (a, J.ConstExp (J.asciiStringAsWide "length")), true)
           | Primitives.Unsafe_cast => if Vector.length args = 1 then
                                           doExpTo ctx env (Vector.sub (args, 0)) dest
                                       else
                                           raise CodeGenError ("primop " ^ Primitives.toString primOp ^ ": invalid number of arguments")
           | Primitives.Unsafe_Vector_sub => doBinaryExp (fn (vec, i) => J.IndexExp (vec, i), true)
           | Primitives.Unsafe_Array_sub => doBinaryExp (fn (arr, i) => J.IndexExp (arr, i), false)
           | Primitives.Unsafe_Array_update => doTernary (fn (stmts, env, (arr, i, v)) =>
                                                             let val stmts = stmts @ [ J.AssignStat (J.IndexExp (arr, i), v) ]
                                                             in putPureTo ctx env dest (stmts, J.UndefinedExp)
                                                             end
                                                         )
           | Primitives.Exception_instanceof => doBinaryExp (fn (e, tag) => J.BinExp (J.INSTANCEOF, e, tag), true)
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
           | _ => raise CodeGenError ("primop " ^ Primitives.toString primOp ^ " is not supported on JavaScript backend")
      end
  | doExpTo ctx env (F.PrimExp (F.ConstructValOp info, _, _)) dest
    = let val tag = #tag info
      in putPureTo ctx env dest ([], J.ObjectExp (vector [(J.StringKey "tag", J.ConstExp (J.asciiStringAsWide tag))]))
      end
  | doExpTo ctx env (F.PrimExp (F.ConstructValWithPayloadOp info, _, args)) dest
    = if Vector.length args = 1 then
          let val tag = #tag info
              val payload = Vector.sub (args, 0)
          in doExpCont ctx env payload (fn (stmts, env, payload) =>
                                           putPureTo ctx env dest (stmts, J.ObjectExp (vector [(J.StringKey "tag", J.ConstExp (J.asciiStringAsWide tag)), (J.StringKey "payload", payload)]))
                                       )
          end
      else
          raise CodeGenError "ConstructValWithPayloadOp: invalid number of arguments"
  | doExpTo ctx env (F.PrimExp (F.ConstructExnOp, _, args)) dest
    = if Vector.length args = 1 then
          let val tag = Vector.sub (args, 0)
          in doExpCont ctx env tag (fn (stmts, env, tag) =>
                                       putPureTo ctx env dest (stmts, J.NewExp (tag, vector []))
                                   )
          end
      else
          raise CodeGenError "ConstructExnOp: invalid number of arguments"
  | doExpTo ctx env (F.PrimExp (F.ConstructExnWithPayloadOp, _, args)) dest
    = if Vector.length args = 2 then
          let val tag = Vector.sub (args, 0)
              val payload = Vector.sub (args, 1)
          in doExpCont ctx env tag (fn (stmts0, env, tag) =>
                                       doExpCont ctx env payload (fn (stmts1, env, payload) =>
                                                                     putPureTo ctx env dest (stmts0 @ stmts1, J.NewExp (tag, vector [payload]))
                                                                 )
                                   )
          end
      else
          raise CodeGenError "ConstructExnWithPayloadOp: invalid number of arguments"
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
               J.VarStat (vector [(tagName, SOME value)])
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
