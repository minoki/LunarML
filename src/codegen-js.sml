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
 * function -> function with arity 1 (TODO: trampolines)
 *)
val builtins
    = let open InitialEnv
          val map = List.foldl USyntax.LongVIdMap.insert' USyntax.LongVIdMap.empty
                               [(USyntax.MkShortVId (FSyntax.strIdToVId StrId_Int), NONE)
                               ,(USyntax.MkShortVId (FSyntax.strIdToVId StrId_Real), NONE)
                               ,(USyntax.MkShortVId (FSyntax.strIdToVId StrId_String), NONE)
                               ,(USyntax.MkShortVId (FSyntax.strIdToVId StrId_Vector), NONE)
                               ,(USyntax.MkShortVId (FSyntax.strIdToVId StrId_Array), NONE)
                               ,(USyntax.MkShortVId (FSyntax.strIdToVId StrId_Lua), NONE)
                               ,(USyntax.MkShortVId (FSyntax.strIdToVId StrId_LunarML), NONE)
                               ]
      in List.foldl (fn ((vid, name), map) => USyntax.LongVIdMap.insert (map, vid, SOME name)) map
                    [(* ref *)
                     (LongVId_ref, "_ref")
                    (* boolean *)
                    ,(LongVId_true, "true") (* boolean literal *)
                    ,(LongVId_false, "false") (* boolean literal *)
                    (* list *)
                    ,(LongVId_nil, "_nil")
                    ,(LongVId_DCOLON, "_cons")
                    (* exn *)
                    ,(LongVId_Match, "_Match")
                    ,(LongVId_Bind, "_Bind")
                    ,(LongVId_Div, "_Div")
                    ,(LongVId_Overflow, "_Overflow")
                    ,(LongVId_Size, "_Size")
                    ,(LongVId_Subscript, "_Subscript")
                    ,(LongVId_Fail, "_Fail")
                    ,(VId_Match_tag, "_Match_tag")
                    ,(VId_Bind_tag, "_Bind_tag")
                    ,(VId_Div_tag, "_Div_tag")
                    ,(VId_Overflow_tag, "_Overflow_tag")
                    ,(VId_Size_tag, "_Size_tag")
                    ,(VId_Subscript_tag, "_Subscript_tag")
                    ,(VId_Fail_tag, "_Fail_tag")
                    ,(USyntax.MkShortVId VId_exnName, "_exnName")
                    (* Overloaded: VId_abs, VId_TILDE, VId_div, VId_mod, VId_TIMES, VId_DIVIDE, VId_PLUS, VId_MINUS, VId_LT, VId_GT, VId_LE, VId_GE *)
                    ,(VId_EQUAL_bool, "_EQUAL") (* JS === *)
                    ,(VId_EQUAL_int, "_EQUAL") (* JS === *)
                    ,(VId_EQUAL_word, "_EQUAL") (* JS === *)
                    (* ,(VId_EQUAL_string, "_EQUAL") (* JS === *) *)
                    ,(VId_EQUAL_char, "_EQUAL") (* JS === *)
                    ,(VId_EQUAL_list, "_List_EQUAL")
                    ,(VId_EQUAL_ref, "_EQUAL") (* JS === *)
                    ,(VId_EQUAL_array, "_EQUAL") (* JS === *)
                    ,(VId_EQUAL_vector, "_Vector_EQUAL")
                    (* int *)
                    ,(VId_Int_abs, "_Int_abs") (* may raise Overflow *)
                    ,(VId_Int_TILDE, "_Int_negate") (* may raise Overflow *)
                    ,(USyntax.MkShortVId VId_Int_add_bin, "__Int_add")
                    ,(USyntax.MkShortVId VId_Int_sub_bin, "__Int_sub")
                    ,(USyntax.MkShortVId VId_Int_mul_bin, "__Int_mul")
                    ,(USyntax.MkShortVId VId_Int_div_bin, "__Int_div")
                    ,(USyntax.MkShortVId VId_Int_mod_bin, "__Int_mod")
                    (* word *)
                    ,(USyntax.MkShortVId VId_Word_div_bin, "__Word_div")
                    ,(USyntax.MkShortVId VId_Word_mod_bin, "__Word_mod")
                    ,(USyntax.MkShortVId VId_Word_LT_bin, "__Word_LT")
                    (* real *)
                    ,(VId_Real_abs, "Math_abs") (* JS Math.abs *)
                    (* Array and Vector *)
                    ,(VId_Array_array, "_Array_array")
                    ,(VId_Array_fromList, "_VectorOrArray_fromList")
                    ,(VId_Array_tabulate, "_VectorOrArray_tabulate")
                    ,(VId_Vector_tabulate, "_VectorOrArray_tabulate")
                    ,(VId_Vector_concat, "_Vector_concat")
                    ,(USyntax.MkShortVId VId_Vector_fromList, "_VectorOrArray_fromList")
                    (* JS interface *)
                    ,(VId_Js_global, "_global")
                    ,(VId_Js_call, "_call")
                    (* extra *)
                    ,(VId_assumePure, "_id") (* no-op *)
                    ,(VId_assumeDiscardable, "_id") (* no-op *)
                    ]
      end
val builtinBinaryOps : (JsSyntax.BinaryOp * (* pure? *) bool) USyntax.LongVIdMap.map
    = let open InitialEnv
      in List.foldl USyntax.LongVIdMap.insert' USyntax.LongVIdMap.empty
                    [(VId_EQUAL_bool,   (JsSyntax.EQUAL, true))
                    ,(VId_EQUAL_int,    (JsSyntax.EQUAL, true))
                    ,(VId_EQUAL_word,   (JsSyntax.EQUAL, true))
                    ,(VId_EQUAL_string, (JsSyntax.EQUAL, true))
                    ,(VId_EQUAL_char,   (JsSyntax.EQUAL, true))
                    ]
      end
fun VIdToJs (vid as USyntax.MkVId (name, n)) = if n < 0 then
                                                   case USyntax.LongVIdMap.find (builtins, USyntax.MkShortVId vid) of
                                                       NONE => raise Fail ("Unknown built-in symbol: " ^ name ^ "@" ^ Int.toString n)
                                                     | SOME (SOME jsName) => JsSyntax.PredefinedId jsName
                                                     | SOME NONE => raise CodeGenError ("the built-in identifier " ^ USyntax.print_VId vid ^ " has no runtime counterpart")
                                               else
                                                   JsSyntax.UserDefinedId vid

fun LabelToObjectKey (Syntax.NumericLabel n) = JsSyntax.IntKey (n - 1)
  | LabelToObjectKey (Syntax.IdentifierLabel s) = JsSyntax.StringKey s

type Context = { nextJsId : int ref }
type Env = { hoistedSymbols : USyntax.VIdSet.set
           , level : int
           }
val initialEnv : Env = { hoistedSymbols = USyntax.VIdSet.empty
                       , level = 0
                       }
fun addSymbol ({ hoistedSymbols, level } : Env, s)
    = { hoistedSymbols = hoistedSymbols
      , level = level
      }
fun addHoistedSymbol ({ hoistedSymbols, level } : Env, s : USyntax.VId)
    = { hoistedSymbols = USyntax.VIdSet.add (hoistedSymbols, s)
      , level = level
      }
fun isHoisted ({ hoistedSymbols, ... } : Env, s : USyntax.VId)
    = USyntax.VIdSet.member (hoistedSymbols, s)
fun declareIfNotHoisted (env : Env, vars : USyntax.VId list) : Env * JsSyntax.Stat list
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
                            in USyntax.MkVId ("tmp", n)
                            end

structure F = FSyntax
structure J = JsSyntax

fun doLiteral (Syntax.IntegerConstant x) = if x < 0 then
                                               J.UnaryExp (J.NEGATE, J.ConstExp (J.Numeral (LargeInt.toString (~ x))))
                                           else
                                               J.ConstExp (J.Numeral (LargeInt.toString x))
  | doLiteral (Syntax.WordConstant x) = J.ConstExp (J.Numeral ("0x" ^ LargeInt.fmt StringCvt.HEX x))
  | doLiteral (Syntax.RealConstant x) = let val y = Numeric.toDecimal { nominal_format = Numeric.binary64, target_format = Numeric.binary64 } x
                                            (* JavaScript does not support hexadecimal floating-point literals *)
                                        in case y of
                                               SOME z => if Numeric.Notation.isNegative z then
                                                             J.UnaryExp (J.NEGATE, J.ConstExp (J.Numeral (Numeric.Notation.toString "-" (Numeric.Notation.abs z))))
                                                         else
                                                             J.ConstExp (J.Numeral (Numeric.Notation.toString "-" z))
                                             | NONE => raise CodeGenError "the hexadecimal floating-point value cannot be represented as a 64-bit floating-point number"
                                        end
  | doLiteral (Syntax.StringConstant x) = J.MethodExp (J.VarExp (J.PredefinedId "Uint8Array"), "of", Vector.map (J.ConstExp o J.Numeral o Int.toString o Char.ord) (Vector.fromList (String.explode x)))
  | doLiteral (Syntax.CharacterConstant x) = let val y = Char.ord (String.sub (x, 0))
                                             in J.ConstExp (J.Numeral (Int.toString y))
                                             end

datatype Destination = Return
                     | AssignTo of USyntax.VId
                     | DeclareAndAssignTo of { level : int, destination : USyntax.VId }
                     | Discard
                     | Continue of (* statements *) J.Stat list * Env * (* pure expression *) J.Exp -> J.Stat list (* the continuation should be called exactly once, and the expression should be used only once *)

(* mapCont : ('a * ('b -> 'r) -> 'r) -> 'a list -> ('b list -> 'r) -> 'r *)
fun mapCont f [] cont = cont []
  | mapCont f (x :: xs) cont = f (x, fn y => mapCont f xs (fn ys => cont (y :: ys)))

local
fun extractStrId (F.VarExp (USyntax.MkVId (name, n))) = SOME (USyntax.MkStrId (name, n), [])
  | extractStrId (F.SProjectionExp (exp, F.StructLabel strid)) = (case extractStrId exp of
                                                                      SOME (strid0, revStrids) => SOME (strid0, strid :: revStrids)
                                                                    | NONE => NONE
                                                                 )
  | extractStrId _ = NONE
in
fun extractLongVId (F.VarExp vid) = SOME (USyntax.MkShortVId vid)
  | extractLongVId (F.SProjectionExp (exp, F.ValueLabel vid)) = Option.map (fn (strid0, revStrids) => USyntax.MkLongVId (strid0, List.rev revStrids, vid)) (extractStrId exp)
  | extractLongVId _ = NONE
end

(* doExpTo : Context -> Env -> F.Exp -> Destination -> J.Stat list *)
fun putPureTo ctx env Return (stmts, exp : J.Exp) = stmts @ [ J.ReturnStat (SOME exp) ]
  | putPureTo ctx env (AssignTo v) (stmts, exp) = stmts @ [ J.ExpStat (J.BinExp (J.ASSIGN, J.VarExp (J.UserDefinedId v), exp)) ]
  | putPureTo ctx env (DeclareAndAssignTo { level, destination }) (stmts, exp) = if #level env = level then
                                                                                     stmts @ [ J.VarStat (vector [(destination, SOME exp)]) ]
                                                                                 else
                                                                                     raise CodeGenError "invalid DeclareAndAssignTo"
  | putPureTo ctx env Discard (stmts, exp) = stmts
  | putPureTo ctx env (Continue cont) (stmts, exp) = cont (stmts, env, exp)
and putImpureTo ctx env Return (stmts, exp : J.Exp) = stmts @ [ J.ReturnStat (SOME exp) ]
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
and doExpTo ctx env (F.PrimExp (F.SConOp scon, _, xs)) dest : J.Stat list = if Vector.length xs = 0 then
                                                                                putPureTo ctx env dest ([], doLiteral scon)
                                                                            else
                                                                                raise CodeGenError "PrimExp.SConOp: non-empty argument"
  | doExpTo ctx env (F.VarExp vid) dest = putPureTo ctx env dest ([], case VIdToJs vid of
                                                                          J.PredefinedId "null" => J.ConstExp J.Null
                                                                        | J.PredefinedId "false" => J.ConstExp J.False
                                                                        | J.PredefinedId "true" => J.ConstExp J.True
                                                                        | id => J.VarExp id
                                                                 )
  | doExpTo ctx env (F.RecordExp []) dest = putPureTo ctx env dest ([], J.VarExp (J.PredefinedId "undefined"))
  | doExpTo ctx env (F.RecordExp fields) Discard = List.concat (List.map (fn (_, exp) => doExpTo ctx env exp Discard) fields)
  | doExpTo ctx env (F.RecordExp fields) dest (* TODO: Use array for tuple *)
    = mapCont (fn ((label, exp), cont) => doExpCont ctx env exp (fn (stmts, env, e) => cont (stmts, (label, e))))
              fields
              (fn ys => let val (stmts, fields') = ListPair.unzip ys
                        in putPureTo ctx env dest ( List.concat stmts
                                                  , J.ObjectExp (vector (List.map (fn (label, exp) => (LabelToObjectKey label, exp)) fields'))
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
    = let val doBinary = case (extractLongVId exp1, exp2) of
                             (SOME longvid, F.RecordExp [(Syntax.NumericLabel 1, e1), (Syntax.NumericLabel 2, e2)]) =>
                             let fun wrap f = SOME (fn () => doExpCont ctx env e1 (fn (stmts1, env, e1') => doExpCont ctx env e2 (fn (stmts2, env, e2') => f (stmts1 @ stmts2, env, e1', e2'))))
                             in case USyntax.LongVIdMap.find (builtinBinaryOps, longvid) of
                                    SOME (binop, pure) => wrap (fn (stmts, env, e1', e2') =>
                                                                   if pure then
                                                                       putPureTo ctx env dest (stmts, J.BinExp (binop, e1', e2'))
                                                                   else
                                                                       putImpureTo ctx env dest (stmts, J.BinExp (binop, e1', e2'))
                                                               )
                                  | NONE => NONE
                             end
                           | _ => NONE
          val doJsCall = case (exp1, exp2) of
                              (F.AppExp (vid_jscall, f), F.PrimExp (F.VectorOp, _, xs)) =>
                              if F.isLongVId (vid_jscall, InitialEnv.VId_Js_call) then
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
          val isNoop = case exp1 of
                           F.TyAppExp(vid, _) => F.isLongVId(vid, InitialEnv.VId_assumePure) orelse F.isLongVId(vid, InitialEnv.VId_assumeDiscardable)
                         | _ => false
      in case List.mapPartial (fn x => x) [doBinary, doJsCall] of
             f :: _ => f ()
           | [] => if isNoop then
                       doExpTo ctx env exp2 dest
                   else
                       doExpCont ctx env exp1
                                 (fn (stmts1, env, e1') =>
                                     doExpCont ctx env exp2
                                               (fn (stmts2, env, e2') =>
                                                   putImpureTo ctx env dest (stmts1 @ stmts2, J.CallExp (e1', vector [e2']))
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
                    case dest of
                        Continue cont => let val result = genSym ctx
                                             val env' = addSymbol (env, result)
                                         in cont ( stmts1
                                                   @ [ J.VarStat (vector [(result, NONE)])
                                                     , J.IfStat ( exp1'
                                                                , vector (doExpTo ctx (increaseLevel env') exp2 (AssignTo result))
                                                                , vector (doExpTo ctx (increaseLevel env') exp3 (AssignTo result))
                                                                )
                                                     ]
                                                 , env'
                                                 , J.VarExp (J.UserDefinedId result)
                                                 )
                                         end
                      | DeclareAndAssignTo { level, destination } => stmts1
                                                                     @ [ J.VarStat (vector [(destination, NONE)])
                                                                       , J.IfStat ( exp1'
                                                                                  , vector (doExpTo ctx (increaseLevel env) exp2 (AssignTo destination))
                                                                                  , vector (doExpTo ctx (increaseLevel env) exp3 (AssignTo destination))
                                                                                  )
                                                                       ]
                      (* Return, AssignTo, Discard *)
                      | _ => stmts1
                             @ [ J.IfStat ( exp1'
                                          , vector (doExpTo ctx (increaseLevel env) exp2 dest)
                                          , vector (doExpTo ctx (increaseLevel env) exp3 dest)
                                          )
                               ]
                )
  | doExpTo ctx env (F.CaseExp _) dest = raise Fail "Lua codegen: CaseExp should have been desugared earlier"
  | doExpTo ctx env (F.FnExp (vid, _, exp)) dest = putPureTo ctx env dest ([], J.FunctionExp (vector [VIdToJs vid], vector (doExpTo ctx (increaseLevel (addSymbol (env, vid))) exp Return)))
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
  | doExpTo ctx env (F.PrimExp (F.DataTagOp, _, xs)) dest
    = if Vector.length xs = 1 then
          let val exp = Vector.sub (xs, 0)
          in doExpCont ctx env exp (fn (stmts, env, exp') => putPureTo ctx env dest (stmts, J.IndexExp (exp', J.ConstExp (J.asciiStringAsWide "tag"))))
          end
      else
          raise CodeGenError "PrimExp.DataTagOp: invalid number of arguments"
  | doExpTo ctx env (F.PrimExp (F.DataPayloadOp, _, xs)) dest
    = if Vector.length xs = 1 then
          let val exp = Vector.sub (xs, 0)
          in doExpCont ctx env exp (fn (stmts, env, exp') => putPureTo ctx env dest (stmts, J.IndexExp (exp', J.ConstExp (J.asciiStringAsWide "payload"))))
          end
      else
          raise CodeGenError "PrimExp.DataPayloadOp: invalid number of arguments"
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
  | doExpTo ctx env (exp as F.SProjectionExp (exp', F.ValueLabel vid)) dest
    = let val builtin = case extractLongVId exp of
                            SOME longvid => (case USyntax.LongVIdMap.find (builtins, longvid) of
                                                 SOME (SOME "null") => SOME (J.ConstExp J.Null)
                                               | SOME (SOME "true") => SOME (J.ConstExp J.True)
                                               | SOME (SOME "false") => SOME (J.ConstExp J.False)
                                               | SOME (SOME jsName) => SOME (J.VarExp (J.PredefinedId jsName))
                                               | SOME NONE => raise CodeGenError ("the built-in identifier " ^ USyntax.print_LongVId longvid ^ " has no runtime counterpart")
                                               | NONE => NONE
                                            )
                          | NONE => NONE
      in case builtin of
             SOME jsExpr => putPureTo ctx env dest ([], jsExpr)
           | NONE => doExpCont ctx env exp' (fn (stmts, env, exp') => putPureTo ctx env dest (stmts, J.IndexExp (exp', J.ConstExp (J.asciiStringAsWide (Syntax.getVIdName vid)))))
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
                                 raise CodeGenError ("PrimExp." ^ Syntax.primOpToString primOp ^ ": invalid number of arguments")
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
                                  raise CodeGenError ("PrimExp." ^ Syntax.primOpToString primOp ^ ": invalid number of arguments")
          fun doBinaryOp (binop, pure) = doBinary (fn (stmts, env, (a, b)) =>
                                                      if pure then
                                                          putPureTo ctx env dest (stmts, J.BinExp (binop, a, b))
                                                      else
                                                          putImpureTo ctx env dest (stmts, J.BinExp (binop, a, b))
                                                  )
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
                                  raise CodeGenError ("PrimExp." ^ Syntax.primOpToString primOp ^ ": invalid number of arguments")
      in case primOp of
             Syntax.PrimOp_call2 => doTernary (fn (stmts, env, (f, a0, a1)) =>
                                                  putImpureTo ctx env dest (stmts, J.CallExp (f, vector [a0, a1]))
                                              )
           | Syntax.PrimOp_call3 => if Vector.length args = 4 then
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
                                        raise CodeGenError "PrimExp.call3: invalid number of arguments"
           | Syntax.PrimOp_Ref_set => doBinary (fn (stmts, env, (a, b)) =>
                                                   (* REPRESENTATION_OF_REF *)
                                                   let val stmts = stmts @ [ J.ExpStat (J.BinExp (J.ASSIGN, J.IndexExp (a, J.ConstExp (J.asciiStringAsWide "payload")), b)) ]
                                                   in putPureTo ctx env dest (stmts, J.UndefinedExp)
                                                   end
                                               )
           | Syntax.PrimOp_Ref_read => doUnary (fn (stmts, env, a) =>
                                               (* REPRESENTATION_OF_REF *)
                                                   putImpureTo ctx env dest (stmts, J.IndexExp (a, J.ConstExp (J.asciiStringAsWide "payload")))
                                               )
           | Syntax.PrimOp_Bool_not => doUnary (fn (stmts, env, a) =>
                                                   putPureTo ctx env dest (stmts, J.UnaryExp (J.NOT, a))
                                               )
           | Syntax.PrimOp_Int_LT => doBinaryOp (J.LT, true)
           | Syntax.PrimOp_Int_GT => doBinaryOp (J.GT, true)
           | Syntax.PrimOp_Int_LE => doBinaryOp (J.LE, true)
           | Syntax.PrimOp_Int_GE => doBinaryOp (J.GE, true)
           | Syntax.PrimOp_Word_PLUS => doBinary (fn (stmts, env, (a, b)) =>
                                                     putPureTo ctx env dest (stmts, J.ToUint32Exp (J.BinExp (J.PLUS, a, b)))
                                                 )
           | Syntax.PrimOp_Word_MINUS => doBinary (fn (stmts, env, (a, b)) =>
                                                      putPureTo ctx env dest (stmts, J.ToUint32Exp (J.BinExp (J.MINUS, a, b)))
                                                  )
           | Syntax.PrimOp_Word_TIMES => doBinary (fn (stmts, env, (a, b)) =>
                                                      putPureTo ctx env dest (stmts, J.ToUint32Exp (J.CallExp (J.VarExp (J.PredefinedId "Math_imul"), vector [a, b])))
                                                  )
           | Syntax.PrimOp_Word_TILDE => doUnary (fn (stmts, env, a) =>
                                                     putPureTo ctx env dest (stmts, J.ToUint32Exp (J.UnaryExp (J.NEGATE, a)))
                                                 )
           | Syntax.PrimOp_Real_PLUS => doBinaryOp (J.PLUS, true)
           | Syntax.PrimOp_Real_MINUS => doBinaryOp (J.MINUS, true)
           | Syntax.PrimOp_Real_TIMES => doBinaryOp (J.TIMES, true)
           | Syntax.PrimOp_Real_DIVIDE => doBinaryOp (J.DIV, true)
           | Syntax.PrimOp_Real_TILDE => doUnary (fn (stmts, env, a) =>
                                                     putPureTo ctx env dest (stmts, J.UnaryExp (J.NEGATE, a))
                                                 )
           | Syntax.PrimOp_Real_LT => doBinaryOp (J.LT, true)
           | Syntax.PrimOp_Real_GT => doBinaryOp (J.GT, true)
           | Syntax.PrimOp_Real_LE => doBinaryOp (J.LE, true)
           | Syntax.PrimOp_Real_GE => doBinaryOp (J.GE, true)
           | Syntax.PrimOp_Char_LT => doBinaryOp (J.LT, true)
           | Syntax.PrimOp_Char_GT => doBinaryOp (J.GT, true)
           | Syntax.PrimOp_Char_LE => doBinaryOp (J.LE, true)
           | Syntax.PrimOp_Char_GE => doBinaryOp (J.GE, true)
           | Syntax.PrimOp_String_LT => raise CodeGenError "PrimOp_String_LT not supported on JavaScript backend"
           | Syntax.PrimOp_String_GT => raise CodeGenError "PrimOp_String_GT not supported on JavaScript backend"
           | Syntax.PrimOp_String_LE => raise CodeGenError "PrimOp_String_LE not supported on JavaScript backend"
           | Syntax.PrimOp_String_GE => raise CodeGenError "PrimOp_String_GE not supported on JavaScript backend"
           | Syntax.PrimOp_String_HAT => raise CodeGenError "PrimOp_String_HAT not supported on JavaScript backend"
           | Syntax.PrimOp_String_size => doUnary (fn (stmts, env, a) =>
                                                      putPureTo ctx env dest (stmts, J.IndexExp (a, J.ConstExp (J.asciiStringAsWide "length")))
                                                  )
           | Syntax.PrimOp_Vector_length => doUnary (fn (stmts, env, a) =>
                                                        putPureTo ctx env dest (stmts, J.IndexExp (a, J.ConstExp (J.asciiStringAsWide "length")))
                                                    )
           | Syntax.PrimOp_Array_length => doUnary (fn (stmts, env, a) =>
                                                       putPureTo ctx env dest (stmts, J.IndexExp (a, J.ConstExp (J.asciiStringAsWide "length")))
                                                   )
           | Syntax.PrimOp_Unsafe_cast => if Vector.length args = 1 then
                                              doExpTo ctx env (Vector.sub (args, 0)) dest
                                          else
                                              raise CodeGenError ("PrimExp." ^ Syntax.primOpToString primOp ^ ": invalid number of arguments")
           | Syntax.PrimOp_Unsafe_Vector_sub => doBinary (fn (stmts, env, (vec, i)) =>
                                                             putPureTo ctx env dest (stmts, J.IndexExp (vec, i))
                                                         )
           | Syntax.PrimOp_Unsafe_Array_sub => doBinary (fn (stmts, env, (arr, i)) =>
                                                             putImpureTo ctx env dest (stmts, J.IndexExp (arr, i))
                                                         )
           | Syntax.PrimOp_Unsafe_Array_update => doTernary (fn (stmts, env, (arr, i, v)) =>
                                                                let val stmts = stmts @ [ J.ExpStat (J.AssignExp (J.IndexExp (arr, i), v)) ]
                                                                in putPureTo ctx env dest (stmts, J.UndefinedExp)
                                                                end
                                                            )
           | Syntax.PrimOp_Lua_sub => raise CodeGenError "PrimOp_Lua_sub not supported on JavaScript backend"
           | Syntax.PrimOp_Lua_set => raise CodeGenError "PrimOp_Lua_set not supported on JavaScript backend"
           | Syntax.PrimOp_Lua_isNil => raise CodeGenError "PrimOp_Lua_isNil not supported on JavaScript backend"
           | Syntax.PrimOp_Lua_EQUAL => raise CodeGenError "PrimOp_Lua_EQUAL not supported on JavaScript backend"
           | Syntax.PrimOp_Lua_NOTEQUAL => raise CodeGenError "PrimOp_Lua_NOTEQUAL not supported on JavaScript backend"
           | Syntax.PrimOp_Lua_LT => raise CodeGenError "PrimOp_Lua_LT not supported on JavaScript backend"
           | Syntax.PrimOp_Lua_GT => raise CodeGenError "PrimOp_Lua_GT not supported on JavaScript backend"
           | Syntax.PrimOp_Lua_LE => raise CodeGenError "PrimOp_Lua_LE not supported on JavaScript backend"
           | Syntax.PrimOp_Lua_GE => raise CodeGenError "PrimOp_Lua_GE not supported on JavaScript backend"
           | Syntax.PrimOp_Lua_PLUS => raise CodeGenError "PrimOp_Lua_PLUS not supported on JavaScript backend"
           | Syntax.PrimOp_Lua_MINUS => raise CodeGenError "PrimOp_Lua_MINUS not supported on JavaScript backend"
           | Syntax.PrimOp_Lua_TIMES => raise CodeGenError "PrimOp_Lua_TIMES not supported on JavaScript backend"
           | Syntax.PrimOp_Lua_DIVIDE => raise CodeGenError "PrimOp_Lua_DIVIDE not supported on JavaScript backend"
           | Syntax.PrimOp_Lua_INTDIV => raise CodeGenError "PrimOp_Lua_INTDIV not supported on JavaScript backend"
           | Syntax.PrimOp_Lua_MOD => raise CodeGenError "PrimOp_Lua_MOD not supported on JavaScript backend"
           | Syntax.PrimOp_Lua_pow => raise CodeGenError "PrimOp_Lua_pow not supported on JavaScript backend"
           | Syntax.PrimOp_Lua_unm => raise CodeGenError "PrimOp_Lua_unm not supported on JavaScript backend"
           | Syntax.PrimOp_Lua_andb => raise CodeGenError "PrimOp_Lua_andb not supported on JavaScript backend"
           | Syntax.PrimOp_Lua_orb => raise CodeGenError "PrimOp_Lua_orb not supported on JavaScript backend"
           | Syntax.PrimOp_Lua_xorb => raise CodeGenError "PrimOp_Lua_xorb not supported on JavaScript backend"
           | Syntax.PrimOp_Lua_notb => raise CodeGenError "PrimOp_Lua_notb not supported on JavaScript backend"
           | Syntax.PrimOp_Lua_LSHIFT => raise CodeGenError "PrimOp_Lua_LSHIFT not supported on JavaScript backend"
           | Syntax.PrimOp_Lua_RSHIFT => raise CodeGenError "PrimOp_Lua_RSHIFT not supported on JavaScript backend"
           | Syntax.PrimOp_Lua_concat => raise CodeGenError "PrimOp_Lua_concat not supported on JavaScript backend"
           | Syntax.PrimOp_Lua_length => raise CodeGenError "PrimOp_Lua_length not supported on JavaScript backend"
           | Syntax.PrimOp_Lua_isFalsy => raise CodeGenError "PrimOp_Lua_isFalsy not supported on JavaScript backend"
      end
  | doExpTo ctx env (F.PrimExp (F.ExnInstanceofOp, _, args)) dest
    = if Vector.length args = 2 then
          let val a0 = Vector.sub (args, 0)
              val a1 = Vector.sub (args, 1)
          in doExpCont ctx env a0 (fn (stmts0, env, a0') =>
                                      doExpCont ctx env a1 (fn (stmts1, env, a1') =>
                                                               putPureTo ctx env dest (stmts0 @ stmts1, J.BinExp (J.INSTANCEOF, a0', a1'))
                                                           )
                                  )
          end
      else
          raise CodeGenError "PrimExp.ExnInstanceofOp: invalid number of arguments"
(* doDec : Context -> Env -> F.Dec -> string *)
and doDec ctx env (F.ValDec (vid, _, exp))
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
  | doDec ctx env (F.DatatypeDec datbinds) = List.concat (List.map (doDatBind ctx env) datbinds)
  | doDec ctx env (F.ExceptionDec { conName as USyntax.MkVId(name, _), tagName, payloadTy })
    = [ let val value = case payloadTy of
                            NONE => J.FunctionExp (vector [], vector [])
                          | SOME _ => J.FunctionExp (vector [J.PredefinedId "payload"], vector [J.AssignStat (J.IndexExp (J.ThisExp, J.ConstExp (J.asciiStringAsWide "payload")), J.VarExp (J.PredefinedId "payload"))])
        in if isHoisted (env, tagName) then
               J.AssignStat (J.VarExp (J.UserDefinedId tagName), value)
           else
               J.VarStat (vector [(tagName, SOME value)])
        end
      , J.AssignStat (J.IndexExp (J.VarExp (J.UserDefinedId tagName), J.ConstExp (J.asciiStringAsWide "ML_name")), J.ConstExp (J.asciiStringAsWide name))
      , case payloadTy of
            NONE => let val value = J.NewExp (J.VarExp (J.UserDefinedId tagName), vector [])
                    in if isHoisted (env, conName) then
                           J.AssignStat (J.VarExp (J.UserDefinedId conName), value)
                       else
                           J.VarStat (vector [(conName, SOME value)])
                    end
          | SOME _ => let val value = J.FunctionExp (vector [J.PredefinedId "payload"], vector [J.ReturnStat (SOME (J.NewExp (J.VarExp (J.UserDefinedId tagName), vector [J.VarExp (J.PredefinedId "payload")])))])
                      in if isHoisted (env, conName) then
                             J.AssignStat (J.VarExp (J.UserDefinedId conName), value)
                         else
                             J.VarStat (vector [(conName, SOME value)])
                      end
      ]
  | doDec ctx env (F.ExceptionRepDec _) = raise CodeGenError "internal error: ExceptionRepDec should have been desugared earlier"
  | doDec ctx env (F.ExportValue _) = raise CodeGenError "internal error: ExportValue must be the last statement"
  | doDec ctx env (F.ExportModule _) = raise CodeGenError "internal error: ExportModule must be the last statement"
  | doDec ctx env (F.GroupDec (_, decs)) = doDecs ctx env decs
and doDatBind ctx env (F.DatBind (tyvars, tycon, conbinds)) = List.map (doConBind ctx env) conbinds
and doConBind ctx env (F.ConBind (vid as USyntax.MkVId (name, _), NONE)) = let val value = J.ObjectExp (vector [(J.StringKey "tag", J.ConstExp (J.asciiStringAsWide name))])
                                                                           in if isHoisted (env, vid) then
                                                                                  J.AssignStat (J.VarExp (J.UserDefinedId vid), value)
                                                                              else
                                                                                  J.VarStat (vector [(vid, SOME value)])
                                                                           end
  | doConBind ctx env (F.ConBind (vid as USyntax.MkVId (name, _), SOME ty)) = let val value = J.FunctionExp (vector [J.PredefinedId "payload"], vector [J.ReturnStat (SOME (J.ObjectExp (vector [(J.StringKey "tag", J.ConstExp (J.asciiStringAsWide name)), (J.StringKey "payload", J.VarExp (J.PredefinedId "payload"))])))])
                                                                              in if isHoisted (env, vid) then
                                                                                     J.AssignStat (J.VarExp (J.UserDefinedId vid), value)
                                                                                 else
                                                                                     J.VarStat (vector [(vid, SOME value)])
                                                                              end
and doDecs ctx env [F.ExportValue exp] = raise CodeGenError "ExportValue is not supported yet"
  | doDecs ctx env [F.ExportModule fields] = raise CodeGenError "ExportModule is not supported yet"
  | doDecs ctx env (dec :: decs) = doDec ctx env dec @ doDecs ctx env decs
  | doDecs ctx env [] = []

fun doProgram ctx env decs = vector (doDecs ctx env decs)

end;
