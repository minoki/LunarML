(*
 * Copyright (c) 2022 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure CodeGenLua = struct
exception CodeGenError of string
(* Mapping of types:
 * SML -> Lua
 * int -> integer
 * word -> integer
 * real -> number
 * string -> string
 * char -> single-character string
 * exn -> ???
 * bool -> boolean
 * ref -> {tag = "ref", payload = <mutable> ...}
 * list -> {tag = "nil"} | {tag = "::", payload = ...}
 * record { label1 = ..., label2 = ... } -> table
 * function -> function
 *)

datatype target_lua_version = LUA5_3 | LUAJIT
type Context = { nextLuaId : int ref, targetLuaVersion : target_lua_version }

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
                    (* word *)
                    ,(VId_Word_div_bin, "__Word_div")
                    ,(VId_Word_mod_bin, "__Word_mod")
                    ,(VId_Word_LT_bin, "__Word_LT")
                    (* real *)
                    ,(VId_Real_abs, "math_abs") (* Lua math.abs *)
                    (* Vector and Array *)
                    ,(VId_Vector_tabulate, "_VectorOrArray_tabulate")
                    ,(VId_Vector_concat, "_Vector_concat")
                    ,(VId_Vector_fromList, "_VectorOrArray_fromList")
                    ,(VId_Array_array, "_Array_array")
                    ,(VId_Array_fromList, "_VectorOrArray_fromList")
                    ,(VId_Array_tabulate, "_VectorOrArray_tabulate")
                    (* Lua interface *)
                    ,(VId_Lua_LuaError, "_LuaError")
                    ,(VId_Lua_LuaError_tag, "_LuaError_tag")
                    ,(VId_Lua_global, "_Lua_global")
                    ,(VId_Lua_call, "_Lua_call")
                    ,(VId_Lua_method, "_Lua_method")
                    ,(VId_Lua_NIL, "nil") (* literal *)
                    ,(VId_Lua_newTable, "_Lua_newTable")
                    ,(VId_Lua_function, "_Lua_function")
                    ,(VId_Lua_Lib_assert, "assert")
                    ,(VId_Lua_Lib_error, "error")
                    ,(VId_Lua_Lib_getmetatable, "getmetatable")
                    ,(VId_Lua_Lib_pairs, "pairs")
                    ,(VId_Lua_Lib_pcall, "pcall")
                    ,(VId_Lua_Lib_setmetatable, "setmetatable")
                    ,(VId_Lua_Lib_math, "math")
                    ,(VId_Lua_Lib_math_abs, "math_abs")
                    ,(VId_Lua_Lib_math_type, "math_type")
                    ,(VId_Lua_Lib_math_maxinteger, "math_maxinteger")
                    ,(VId_Lua_Lib_math_mininteger, "math_mininteger")
                    ,(VId_Lua_Lib_string, "string")
                    ,(VId_Lua_Lib_string_format, "string_format")
                    ,(VId_Lua_Lib_table, "table")
                    ,(VId_Lua_Lib_table_pack, "table_pack")
                    ,(VId_Lua_Lib_table_unpack, "table_unpack")
                    (* extra *)
                    ,(VId_assumePure, "_id") (* no-op *)
                    ,(VId_assumeDiscardable, "_id") (* no-op *)
                    ]
      end
val builtinsLuaJIT
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
                    ,(VId_Int_quot_bin, "__Int_div")
                    (* word *)
                    ,(VId_Word_add_bin, "__Word_add")
                    ,(VId_Word_sub_bin, "__Word_sub")
                    ,(VId_Word_mul_bin, "__Word_mul")
                    ,(VId_Word_div_bin, "__Word_div")
                    ,(VId_Word_mod_bin, "__Word_mod")
                    ,(VId_Word_TILDE, "_Word_negate")
                    ,(VId_Word_LT_bin, "__Word_LT")
                    (* real *)
                    ,(VId_Real_abs, "math_abs") (* Lua math.abs *)
                    (* Vector and Array *)
                    ,(VId_Vector_tabulate, "_VectorOrArray_tabulate")
                    ,(VId_Vector_concat, "_Vector_concat")
                    ,(VId_Vector_fromList, "_VectorOrArray_fromList")
                    ,(VId_Array_array, "_Array_array")
                    ,(VId_Array_fromList, "_VectorOrArray_fromList")
                    ,(VId_Array_tabulate, "_VectorOrArray_tabulate")
                    (* Lua interface *)
                    ,(VId_Lua_LuaError, "_LuaError")
                    ,(VId_Lua_LuaError_tag, "_LuaError_tag")
                    ,(VId_Lua_global, "_Lua_global")
                    ,(VId_Lua_call, "_Lua_call")
                    ,(VId_Lua_method, "_Lua_method")
                    ,(VId_Lua_NIL, "nil") (* literal *)
                    ,(VId_Lua_newTable, "_Lua_newTable")
                    ,(VId_Lua_function, "_Lua_function")
                    ,(VId_Lua_Lib_assert, "assert")
                    ,(VId_Lua_Lib_error, "error")
                    ,(VId_Lua_Lib_getmetatable, "getmetatable")
                    ,(VId_Lua_Lib_pairs, "pairs")
                    ,(VId_Lua_Lib_pcall, "pcall")
                    ,(VId_Lua_Lib_setmetatable, "setmetatable")
                    ,(VId_Lua_Lib_math, "math")
                    ,(VId_Lua_Lib_math_abs, "math_abs")
                    ,(VId_Lua_Lib_string, "string")
                    ,(VId_Lua_Lib_string_format, "string_format")
                    ,(VId_Lua_Lib_table, "table")
                    ,(VId_Lua_Lib_table_pack, "table_pack")
                    ,(VId_Lua_Lib_table_unpack, "table_unpack")
                    (* extra *)
                    ,(VId_assumePure, "_id") (* no-op *)
                    ,(VId_assumeDiscardable, "_id") (* no-op *)
                    ]
      end
fun VIdToLua (ctx : Context, vid as TypedSyntax.MkVId (name, n))
    = if n < 0 then
          case #targetLuaVersion ctx of
              LUA5_3 => (case TypedSyntax.VIdMap.find (builtins, vid) of
                             NONE => raise Fail ("the built-in symbol " ^ name ^ "@" ^ Int.toString n ^ " is not supported by Lua backend")
                           | SOME luaName => LuaSyntax.PredefinedId luaName
                        )
            | LUAJIT => (case TypedSyntax.VIdMap.find (builtinsLuaJIT, vid) of
                             NONE => raise Fail ("the built-in symbol " ^ name ^ "@" ^ Int.toString n ^ " is not supported by LuaJIT backend")
                           | SOME luaName => LuaSyntax.PredefinedId luaName
                        )
      else
          LuaSyntax.UserDefinedId vid

structure StringSet = RedBlackSetFn(struct open String; type ord_key = string end)
val LuaKeywords = StringSet.fromList
                      ["and", "break", "do", "else", "elseif", "end",
                       "false", "for", "function", "goto", "if", "in",
                       "local", "nil", "not", "or", "repeat", "return",
                       "then", "true", "until", "while"]
fun isLuaIdentifier name = case String.explode name of
                               [] => false
                             | x0 :: xs => (Char.isAlpha x0 orelse x0 = #"_")
                                           andalso (List.all (fn c => Char.isAlphaNum c orelse c = #"_") xs)
                                           andalso (not (StringSet.member(LuaKeywords, name)))
fun LabelToTableKey (Syntax.NumericLabel n) = LuaSyntax.IntKey n
  | LabelToTableKey (Syntax.IdentifierLabel s) = LuaSyntax.StringKey s

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
fun declareIfNotHoisted (env : Env, vars : TypedSyntax.VId list) : Env * LuaSyntax.Stat list
    = let val (env, vars) = List.foldr (fn (v, (env, xs)) => if isHoisted (env, v) then
                                                                 (env, xs)
                                                             else
                                                                 (addHoistedSymbol (env, v), (v, LuaSyntax.LATE_INIT) :: xs)) (env, []) vars
      in (env, case vars of
                   [] => []
                 | _ => [ LuaSyntax.LocalStat (vars, []) ]
         )
      end
fun increaseLevel ({ hoistedSymbols, level } : Env) = { hoistedSymbols = hoistedSymbols, level = level + 1 }

fun genSym (ctx: Context) = let val n = !(#nextLuaId ctx)
                                val _ = #nextLuaId ctx := n + 1
                            in TypedSyntax.MkVId ("tmp", n)
                            end

structure F = FSyntax
structure L = LuaSyntax

datatype Destination = Return
                     | AssignTo of TypedSyntax.VId
                     | DeclareAndAssignTo of { level : int, destination : TypedSyntax.VId }
                     | Discard
                     | Continue of (* statements *) L.Stat list * Env * (* pure expression *) L.Exp -> L.Stat list (* the continuation should be called exactly once, and the expression should be used only once *)

(* mapCont : ('a * ('b -> 'r) -> 'r) -> 'a list -> ('b list -> 'r) -> 'r *)
fun mapCont f [] cont = cont []
  | mapCont f (x :: xs) cont = f (x, fn y => mapCont f xs (fn ys => cont (y :: ys)))

(* doExpTo : Context -> Env -> F.Exp -> Destination -> L.Stat list *)
fun putPureTo ctx env Return (stmts, exp : L.Exp) = stmts @ [ L.ReturnStat (vector [exp]) ]
  | putPureTo ctx env (AssignTo v) (stmts, exp) = stmts @ [ L.AssignStat ([L.VarExp (L.UserDefinedId v)], [exp]) ]
  | putPureTo ctx env (DeclareAndAssignTo { level, destination }) (stmts, exp) = if #level env = level then
                                                                                     stmts @ [ L.LocalStat ([(destination, L.CONST)], [exp]) ]
                                                                                 else
                                                                                     raise CodeGenError "invalid DeclareAndAssignTo"
  | putPureTo ctx env Discard (stmts, exp) = stmts
  | putPureTo ctx env (Continue cont) (stmts, exp) = cont (stmts, env, exp)
and putImpureTo ctx env Return (stmts, exp : L.Exp) = stmts @ [ L.ReturnStat (vector [exp]) ]
  | putImpureTo ctx env (AssignTo v) (stmts, exp) = stmts @ [ L.AssignStat ([L.VarExp (L.UserDefinedId v)], [exp]) ]
  | putImpureTo ctx env (DeclareAndAssignTo { level, destination }) (stmts, exp) = if #level env = level then
                                                                                       stmts @ [ L.LocalStat ([(destination, L.CONST)], [exp]) ]
                                                                                   else
                                                                                       raise CodeGenError "invalid DeclareAndAssignTo"
  | putImpureTo ctx env Discard (stmts, exp) = stmts @ [ case exp of
                                                             L.CallExp (f, args) => L.CallStat (f, args)
                                                           | L.MethodExp (self, name, args) => L.MethodStat (self, name, args)
                                                           | _ => L.CallStat (L.VarExp (L.PredefinedId "_id"), vector [exp])
                                                       ]
  | putImpureTo ctx env (Continue cont) (stmts, exp) = let val dest = genSym ctx
                                                           val env = addSymbol (env, dest)
                                                       in cont (stmts @ [ L.LocalStat ([(dest, L.CONST)], [exp]) ], env, L.VarExp (L.UserDefinedId dest))
                                                       end
and doExpCont ctx env exp (cont : L.Stat list * Env * L.Exp -> L.Stat list) = doExpTo ctx env exp (Continue cont)
and doExpTo ctx env (F.PrimExp (F.IntConstOp x, _, xs)) dest : L.Stat list
    = if Vector.length xs = 0 then
          let val exp = if x < 0 then
                            if x = ~0x800000000000 then
                                L.BinExp (L.MINUS, L.UnaryExp (L.NEGATE, L.ConstExp (L.Numeral (LargeInt.toString (~ (x + 1))))), L.ConstExp (L.Numeral "1"))
                            else
                                L.UnaryExp (L.NEGATE, L.ConstExp (L.Numeral (LargeInt.toString (~ x))))
                        else
                            L.ConstExp (L.Numeral (LargeInt.toString x))
          in putPureTo ctx env dest ([], exp)
          end
      else
          raise CodeGenError "PrimExp.IntConstOp: non-empty argument"
  | doExpTo ctx env (F.PrimExp (F.WordConstOp x, _, xs)) dest
    = if Vector.length xs = 0 then
          let val exp = L.ConstExp (L.Numeral ("0x" ^ LargeInt.fmt StringCvt.HEX x))
          in putPureTo ctx env dest ([], exp)
          end
      else
          raise CodeGenError "PrimExp.WordConstOp: non-empty argument"
  | doExpTo ctx env (F.PrimExp (F.RealConstOp x, _, xs)) dest
    = if Vector.length xs = 0 then
          let val exp = if Numeric.Notation.isNegative x then
                            case (#targetLuaVersion ctx, Numeric.Notation.isNegativeZero x) of
                                (LUAJIT, true) => L.VarExp (L.PredefinedId "NEGATIVE_ZERO")
                              | _ => L.UnaryExp (L.NEGATE, L.ConstExp (L.Numeral (Numeric.Notation.toString "-" (Numeric.Notation.abs x))))
                        else
                            L.ConstExp (L.Numeral (Numeric.Notation.toString "-" x))
          in putPureTo ctx env dest ([], exp)
          end
      else
          raise CodeGenError "PrimExp.RealConstOp: non-empty argument"
  | doExpTo ctx env (F.PrimExp (F.StringConstOp x, _, xs)) dest
    = if Vector.length xs = 0 then
          let val exp = L.ConstExp (L.LiteralString (CharVector.tabulate (Vector.length x, fn i => Char.chr (Vector.sub (x, i)))))
                        handle Chr => raise CodeGenError "character ordinal too large"
          in putPureTo ctx env dest ([], exp)
          end
      else
          raise CodeGenError "PrimExp.StringConstOp: non-empty argument"
  | doExpTo ctx env (F.PrimExp (F.CharConstOp x, _, xs)) dest
    = if Vector.length xs = 0 then
          let val exp = L.ConstExp (L.LiteralString (String.str (Char.chr x)))
                        handle Chr => raise CodeGenError "character ordinal too large"
          in putPureTo ctx env dest ([], exp)
          end
      else
          raise CodeGenError "PrimExp.CharConstOp: non-empty argument"
  | doExpTo ctx env (F.VarExp vid) dest = putPureTo ctx env dest ([], case VIdToLua (ctx, vid) of
                                                                          L.PredefinedId "nil" => L.ConstExp L.Nil
                                                                        | L.PredefinedId "false" => L.ConstExp L.False
                                                                        | L.PredefinedId "true" => L.ConstExp L.True
                                                                        | id => L.VarExp id
                                                                 )
  | doExpTo ctx env (F.RecordExp []) dest = putPureTo ctx env dest ([], L.ConstExp L.Nil)
  | doExpTo ctx env (F.RecordExp fields) Discard = List.concat (List.map (fn (_, exp) => doExpTo ctx env exp Discard) fields)
  | doExpTo ctx env (F.RecordExp fields) dest
    = mapCont (fn ((label, exp), cont) => doExpCont ctx env exp (fn (stmts, env, e) => cont (stmts, (label, e))))
              fields
              (fn ys => let val (stmts, fields') = ListPair.unzip ys
                        in putPureTo ctx env dest ( List.concat stmts
                                                  , L.TableExp (vector (List.map (fn (label, exp) => (LabelToTableKey label, exp)) fields'))
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
    = let val doLuaCall = case (exp1, exp2) of
                              (F.AppExp (F.VarExp vid_luacall, f), F.PrimExp (F.VectorOp, _, xs)) =>
                              if TypedSyntax.eqVId (vid_luacall, InitialEnv.VId_Lua_call) then
                                  SOME (fn () => doExpCont ctx env f
                                                           (fn (stmts1, env, f) =>
                                                               mapCont (fn (e, cont) => doExpCont ctx env e (fn (x, _, e) => cont (x, e)))
                                                                       (Vector.foldr (op ::) [] xs)
                                                                       (fn ys => let val stmts2 = List.foldr (fn ((x, _), acc) => x @ acc) [] ys
                                                                                     val zs = Vector.map #2 (vector ys)
                                                                                 in case dest of
                                                                                        Discard => putImpureTo ctx env dest (stmts1 @ stmts2, L.CallExp (f, zs))
                                                                                      | _ => putImpureTo ctx env dest (stmts1 @ stmts2, L.CallExp (L.VarExp (L.PredefinedId "table_pack"), vector [L.CallExp (f, zs)]))
                                                                                 end
                                                                       )
                                                           )
                                       )
                              else
                                  NONE
                            | _ => NONE
          val doLuaMethod = case (exp1, exp2) of
                                (F.AppExp (F.VarExp vid_luamethod, F.RecordExp [(Syntax.NumericLabel 1, self), (Syntax.NumericLabel 2, F.PrimExp (F.StringConstOp method, _, _))]), F.PrimExp(F.VectorOp, _, xs)) =>
                                (case SOME (CharVector.tabulate (Vector.length method, fn i => Char.chr (Vector.sub (method, i)))) handle Chr => NONE of
                                     SOME method =>
                                     if TypedSyntax.eqVId (vid_luamethod, InitialEnv.VId_Lua_method) andalso isLuaIdentifier method then
                                         SOME (fn () => doExpCont ctx env self
                                                                  (fn (stmts1, env, self) =>
                                                                      mapCont (fn (e, cont) => doExpCont ctx env e (fn (x, _, e) => cont (x, e)))
                                                                              (Vector.foldr (op ::) [] xs)
                                                                              (fn ys => let val stmts2 = List.foldr (fn ((x, _), acc) => x @ acc) [] ys
                                                                                            val zs = Vector.map #2 (vector ys)
                                                                                        in case dest of
                                                                                               Discard => putImpureTo ctx env dest (stmts1 @ stmts2, L.MethodExp (self, method, zs))
                                                                                             | _ => putImpureTo ctx env dest (stmts1 @ stmts2, L.CallExp (L.VarExp (L.PredefinedId "table_pack"), vector [L.MethodExp (self, method, zs)]))
                                                                                        end
                                                                              )
                                                                  )
                                              )
                                     else
                                         NONE
                                   | NONE => NONE
                                )
                              | _ => NONE
          (* doLuaGlobal: VId_Lua_global *)
          val isNoop = case exp1 of
                           F.TyAppExp (F.VarExp vid, _) => TypedSyntax.eqVId (vid, InitialEnv.VId_assumePure) orelse TypedSyntax.eqVId (vid, InitialEnv.VId_assumeDiscardable)
                         | _ => false
      in case List.mapPartial (fn x => x) [doLuaCall, doLuaMethod] of
             f :: _ => f ()
           | [] => if isNoop then
                       doExpTo ctx env exp2 dest
                   else
                       doExpCont ctx env exp1
                                 (fn (stmts1, env, e1') =>
                                     doExpCont ctx env exp2
                                               (fn (stmts2, env, e2') =>
                                                   putImpureTo ctx env dest (stmts1 @ stmts2, L.CallExp (e1', vector [e2']))
                                               )
                                 )
      end
  | doExpTo ctx env (F.HandleExp { body, exnName, handler }) dest
    = let val status = genSym ctx
          val result = genSym ctx
          val exnName = exnName
          val env' = addSymbol (addSymbol (env, status), result)
          val env'' = addSymbol (env', exnName)
          val stmts = [ L.LocalStat ([(status, L.CONST), (result, L.CONST)], [L.CallExp (L.VarExp (L.PredefinedId "_handle"), vector [L.FunctionExp (vector [], vector (doExpTo ctx (increaseLevel env') body Return))])])
                      , L.IfStat ( L.UnaryExp (L.NOT, L.VarExp (L.UserDefinedId status))
                                 , vector ( L.LocalStat ([(exnName, L.CONST)], [L.VarExp (L.UserDefinedId result)])
                                            :: doExpTo ctx (increaseLevel env'') handler (AssignTo result) (* TODO: tail call *)
                                          )
                                 , vector []
                                 )
                      ]
      in putPureTo ctx env dest (stmts, L.VarExp (L.UserDefinedId result))
      end
  | doExpTo ctx env (F.PrimExp (F.RaiseOp (span as { start as { file, line, column }, ... }), _, xs)) dest
    = if Vector.length xs = 1 then
          let val exp = Vector.sub (xs, 0)
          in doExpCont ctx env exp
                       (fn (stmts, env, exp') =>
                           let val locationInfo = if start = SourcePos.nullPos then
                                                      L.ConstExp L.Nil
                                                  else
                                                      L.ConstExp (L.LiteralString (OS.Path.file file ^ ":" ^ Int.toString line ^ ":" ^ Int.toString column))
                           in case dest of
                                  Continue cont => cont (stmts @ [ L.CallStat (L.VarExp (L.PredefinedId "_raise"), vector [exp', locationInfo]) ], env, L.ConstExp L.Nil)
                                | _ => stmts @ [ L.CallStat (L.VarExp (L.PredefinedId "_raise"), vector [exp', locationInfo]) ]
                           end
                       )
          end
      else
          raise CodeGenError "PrimExp.RaiseOp: invalid number of arguments"
  | doExpTo ctx env (F.IfThenElseExp (exp1, exp2, exp3)) dest
    = doExpCont ctx env exp1
                (fn (stmts1, env, exp1') =>
                    let fun tryCondExp (exp1', L.ConstExp L.True, L.ConstExp L.False) = SOME exp1'
                          | tryCondExp (exp1', L.ConstExp L.False, L.ConstExp L.True) = SOME (L.UnaryExp (L.NOT, exp1'))
                          | tryCondExp (exp1', L.ConstExp L.True, exp3'') = SOME (L.BinExp (L.OR, exp1', exp3''))
                          | tryCondExp (exp1', L.ConstExp L.False, exp3'') = SOME (L.BinExp (L.AND, L.UnaryExp (L.NOT, exp1'), exp3''))
                          | tryCondExp (exp1', exp2'', L.ConstExp L.True) = SOME (L.BinExp (L.OR, L.UnaryExp (L.NOT, exp1'), exp2''))
                          | tryCondExp (exp1', exp2'', L.ConstExp L.False) = SOME (L.BinExp (L.AND, exp1', exp2''))
                          | tryCondExp (exp1', exp2'', exp3'') = NONE
                        fun tryAssignToCondExp (destination, [L.AssignStat ([L.VarExp (L.UserDefinedId id2)], [exp2])], [L.AssignStat ([L.VarExp (L.UserDefinedId id3)], [exp3])])
                            = if id2 = destination andalso id3 = destination then
                                  tryCondExp (exp1', exp2, exp3)
                              else
                                  NONE
                          | tryAssignToCondExp _ = NONE
                    in case dest of
                           Continue cont => let val result = genSym ctx
                                                val env' = addSymbol (env, result)
                                                val exp2' = doExpTo ctx (increaseLevel env') exp2 (AssignTo result)
                                                val exp3' = doExpTo ctx (increaseLevel env') exp3 (AssignTo result)
                                            in cont ( stmts1
                                                      @ (case tryAssignToCondExp (result, exp2', exp3') of
                                                             SOME condExp => [ L.LocalStat ([(result, L.CONST)], [condExp]) ]
                                                           | NONE => [ L.LocalStat ([(result, L.LATE_INIT)], [])
                                                                     , L.IfStat (exp1', vector exp2', vector exp3')
                                                                     ]
                                                        )
                                                    , env'
                                                    , L.VarExp (L.UserDefinedId result)
                                                    )
                                            end
                         | DeclareAndAssignTo { level, destination } => let val exp2' = doExpTo ctx (increaseLevel env) exp2 (AssignTo destination)
                                                                            val exp3' = doExpTo ctx (increaseLevel env) exp3 (AssignTo destination)
                                                                        in stmts1
                                                                           @ (case tryAssignToCondExp (destination, exp2', exp3') of
                                                                                  SOME condExp => [ L.LocalStat ([(destination, L.CONST)], [condExp]) ]
                                                                                | NONE => [ L.LocalStat ([(destination, L.LATE_INIT)], [])
                                                                                          , L.IfStat (exp1', vector exp2', vector exp3')
                                                                                          ]
                                                                             )
                                                                        end
                         | dest as AssignTo destination => let val exp2' = doExpTo ctx (increaseLevel env) exp2 dest
                                                               val exp3' = doExpTo ctx (increaseLevel env) exp3 dest
                                                           in stmts1
                                                              @ (case tryAssignToCondExp (destination, exp2', exp3') of
                                                                     SOME condExp => [ L.AssignStat ([L.VarExp (L.UserDefinedId destination)], [condExp]) ]
                                                                   | NONE => [ L.IfStat (exp1', vector exp2', vector exp3') ]
                                                                )
                                                           end
                         | Return => let fun containsLocalStat (L.LocalStat _ :: _) = true
                                           | containsLocalStat (_ :: xs) = containsLocalStat xs
                                           | containsLocalStat [] = false
                                     in if containsLocalStat stmts1 then
                                            L.DoStat (vector (stmts1
                                                              @ [ L.IfStat ( exp1'
                                                                           , vector (doExpTo ctx (increaseLevel env) exp2 Return)
                                                                           , vector []
                                                                           )
                                                                ]
                                                             )
                                                     ) :: doExpTo ctx (increaseLevel env) exp3 dest
                                        else
                                            stmts1
                                            @ [ L.IfStat ( exp1'
                                                         , vector (doExpTo ctx (increaseLevel env) exp2 Return)
                                                         , vector []
                                                         )
                                              ] @ doExpTo ctx (increaseLevel env) exp3 dest
                                     end
                         | Discard => stmts1
                                      @ [ L.IfStat ( exp1'
                                                   , vector (doExpTo ctx (increaseLevel env) exp2 dest)
                                                   , vector (doExpTo ctx (increaseLevel env) exp3 dest)
                                                   )
                                        ]
                    end
                )
  | doExpTo ctx env (F.CaseExp _) dest = raise Fail "Lua codegen: CaseExp should have been desugared earlier"
  | doExpTo ctx env (F.FnExp (vid, _, exp)) dest = putPureTo ctx env dest ([], L.FunctionExp (vector [VIdToLua (ctx, vid)], vector (doExpTo ctx (increaseLevel env) exp Return))) (* TODO: update environment *)
  | doExpTo ctx env (F.ProjectionExp { label, record }) dest = doExpCont ctx env record (fn (stmts, env, record') =>
                                                                                            let val label = case label of
                                                                                                                Syntax.NumericLabel n => L.ConstExp (L.Numeral (Int.toString n))
                                                                                                              | Syntax.IdentifierLabel s => L.ConstExp (L.LiteralString s)
                                                                                            in putPureTo ctx env dest (stmts, L.IndexExp (record', label))
                                                                                            end
                                                                                        )
  | doExpTo ctx env (F.PrimExp (F.ListOp, _, xs)) dest
    = if Vector.length xs = 0 then
          putPureTo ctx env dest ([], L.VarExp (L.PredefinedId "_nil"))
      else
          mapCont (fn (e, cont) => doExpCont ctx env e (fn (x, _, e) => cont (x, e)))
                  (Vector.foldr (op ::) [] xs)
                  (fn ys => let val stmts = List.foldr (fn ((x, _), acc) => x @ acc) [] ys
                                fun doFields (i, []) = []
                                  | doFields (i, (_, y) :: ys) = (L.IntKey i, y) :: doFields (i + 1, ys)
                            in putPureTo ctx env dest (stmts, L.CallExp (L.VarExp (L.PredefinedId "_list"), vector [L.TableExp (vector ((L.StringKey "n", L.ConstExp (L.Numeral (Int.toString (Vector.length xs)))) :: doFields (1, ys)))]))
                            end
                  )
  | doExpTo ctx env (F.PrimExp (F.VectorOp, _, xs)) dest
    = mapCont (fn (e, cont) => doExpCont ctx env e (fn (x, _, e) => cont (x, e)))
              (Vector.foldr (op ::) [] xs)
              (fn ys => let val stmts = List.foldr (fn ((x, _), acc) => x @ acc) [] ys
                                fun doFields (i, []) = []
                                  | doFields (i, (_, y) :: ys) = (L.IntKey i, y) :: doFields (i + 1, ys)
                        in putPureTo ctx env dest (stmts, L.TableExp (vector ((L.StringKey "n", L.ConstExp (L.Numeral (Int.toString (Vector.length xs)))) :: doFields (1, ys))))
                        end
              )
  | doExpTo ctx env (F.TyAbsExp (_, _, exp)) dest = doExpTo ctx env exp dest
  | doExpTo ctx env (F.TyAppExp (exp, _)) dest = doExpTo ctx env exp dest
  | doExpTo ctx env (F.PrimExp (F.RecordEqualityOp, _, xs)) dest
    = if Vector.length xs = 1 then
          let val exp = Vector.sub (xs, 0)
          in case exp of
                 F.RecordExp [] => putPureTo ctx env dest ([], L.VarExp (L.PredefinedId "_Unit_EQUAL"))
               | _ => doExpCont ctx env exp
                                (fn (stmts, env, e') =>
                                    putPureTo ctx env dest (stmts, L.CallExp (L.VarExp (L.PredefinedId "_Record_EQUAL"), vector [e']))
                                )
          end
      else
          raise CodeGenError "PrimExp.RecordEqualityOp: invalid number of arguments"
  | doExpTo ctx env (F.PrimExp (F.DataTagOp info, _, xs)) dest
    = if Vector.length xs = 1 then
          let val exp = Vector.sub (xs, 0)
          in doExpCont ctx env exp (fn (stmts, env, exp') => putPureTo ctx env dest (stmts, L.IndexExp (exp', L.ConstExp (L.LiteralString "tag"))))
          end
      else
          raise CodeGenError "PrimExp.DataTagOp: invalid number of arguments"
  | doExpTo ctx env (F.PrimExp (F.DataPayloadOp info, _, xs)) dest
    = if Vector.length xs = 1 then
          let val exp = Vector.sub (xs, 0)
          in doExpCont ctx env exp (fn (stmts, env, exp') => putPureTo ctx env dest (stmts, L.IndexExp (exp', L.ConstExp (L.LiteralString "payload"))))
          end
      else
          raise CodeGenError "PrimExp.DataPayloadOp: invalid number of arguments"
  | doExpTo ctx env (F.PrimExp (F.ExnPayloadOp, _, xs)) dest
    = if Vector.length xs = 1 then
          let val exp = Vector.sub (xs, 0)
          in doExpCont ctx env exp (fn (stmts, env, exp') => putPureTo ctx env dest (stmts, L.IndexExp (exp', L.ConstExp (L.LiteralString "payload"))))
          end
      else
          raise CodeGenError "PrimExp.ExnPayloadOp: invalid number of arguments"
  | doExpTo ctx env (F.StructExp { valMap, strMap, exnTagMap }) dest
    = let val valMap' = Syntax.VIdMap.listItemsi valMap
          val strMap' = Syntax.StrIdMap.listItemsi strMap
          val exnTagMap' = Syntax.VIdMap.listItemsi exnTagMap
      in mapCont (fn ((label, exp), cont) => doExpCont ctx env exp (fn (stmts, env, e) => cont (stmts, (label, e))))
                 valMap'
                 (fn valMap' =>
                     let val (stmts, valFields) = ListPair.unzip valMap'
                     in mapCont (fn ((label, exp), cont) => doExpCont ctx env exp (fn (stmts, env, e) => cont (stmts, (label, e))))
                                strMap'
                                (fn strMap' =>
                                    let val (stmts', strFields) = ListPair.unzip strMap'
                                    in mapCont (fn ((label, exp), cont) => doExpCont ctx env exp (fn (stmts, env, e) => cont (stmts, (label, e))))
                                               exnTagMap'
                                               (fn exnTagMap' =>
                                                   let val (stmts'', exnTagFields) = ListPair.unzip exnTagMap'
                                                       val valFields = List.map (fn (vid, e) => (L.StringKey (Syntax.getVIdName vid), e)) valFields
                                                       val strFields = List.map (fn (Syntax.MkStrId name, e) => (L.StringKey ("_" ^ name), e)) strFields
                                                       val exnTagFields = List.map (fn (vid, e) => (L.StringKey (Syntax.getVIdName vid ^ ".tag"), e)) exnTagFields
                                                   in putPureTo ctx env dest ( List.concat stmts @ List.concat stmts' @ List.concat stmts''
                                                                             , L.TableExp (vector (valFields @ strFields @ exnTagFields))
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
                                                                   in doExpCont ctx env exp' (fn (stmts, env, exp') => putPureTo ctx env dest (stmts, L.IndexExp (exp', L.ConstExp (L.LiteralString field))))
                                                                   end
  | doExpTo ctx env (F.PackExp { payloadTy, exp, packageTy }) dest = doExpTo ctx env exp dest
  | doExpTo ctx env (F.PrimExp (F.PrimFnOp primOp, _, args)) dest
    = let fun doUnary cont = if Vector.length args = 1 then
                                 let val a = Vector.sub (args, 0)
                                 in doExpCont ctx env a cont
                                 end
                             else
                                 raise CodeGenError ("primop " ^ Primitives.toString primOp ^ ": invalid number of arguments")
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
          fun doBinaryOp (binop, pure) = doBinary (fn (stmts, env, (a, b)) =>
                                                      if pure then
                                                          putPureTo ctx env dest (stmts, L.BinExp (binop, a, b))
                                                      else
                                                          putImpureTo ctx env dest (stmts, L.BinExp (binop, a, b))
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
                                  raise CodeGenError ("primop " ^ Primitives.toString primOp ^ ": invalid number of arguments")
      in case primOp of
             Primitives.call2 => doTernary (fn (stmts, env, (f, a0, a1)) =>
                                               putImpureTo ctx env dest (stmts, L.CallExp (f, vector [a0, a1]))
                                           )
           | Primitives.call3 => if Vector.length args = 4 then
                                     let val f = Vector.sub (args, 0)
                                         val a0 = Vector.sub (args, 1)
                                         val a1 = Vector.sub (args, 2)
                                         val a2 = Vector.sub (args, 3)
                                     in doExpCont ctx env f (fn (stmts0, env, f) =>
                                                                doExpCont ctx env a0 (fn (stmts1, env, a0) =>
                                                                                         doExpCont ctx env a1 (fn (stmts2, env, a1) =>
                                                                                                                  doExpCont ctx env a2 (fn (stmts3, env, a2) =>
                                                                                                                                           putImpureTo ctx env dest (stmts0 @ stmts1 @ stmts2 @ stmts3, L.CallExp (f, vector [a0, a1, a2]))
                                                                                                                                       )
                                                                                                              )
                                                                                     )
                                                            )
                                     end
                                 else
                                     raise CodeGenError "primop call3: invalid number of arguments"
           | Primitives.Ref_EQUAL => doBinaryOp (L.EQUAL, true)
           | Primitives.Ref_set => doBinary (fn (stmts, env, (a, b)) =>
                                                (* REPRESENTATION_OF_REF *)
                                                let val stmts = stmts @ [ L.AssignStat ([L.IndexExp (a, L.ConstExp (L.LiteralString "payload"))], [b]) ]
                                                in putPureTo ctx env dest (stmts, L.ConstExp L.Nil)
                                                end
                                            )
           | Primitives.Ref_read => doUnary (fn (stmts, env, a) =>
                                                (* REPRESENTATION_OF_REF *)
                                                putImpureTo ctx env dest (stmts, L.IndexExp (a, L.ConstExp (L.LiteralString "payload")))
                                            )
           | Primitives.Bool_EQUAL => doBinaryOp (L.EQUAL, true)
           | Primitives.Bool_not => doUnary (fn (stmts, env, a) =>
                                                putPureTo ctx env dest (stmts, L.UnaryExp (L.NOT, a))
                                            )
           | Primitives.Int_EQUAL => doBinaryOp (L.EQUAL, true)
           | Primitives.Int_LT => doBinaryOp (L.LT, true)
           | Primitives.Int_GT => doBinaryOp (L.GT, true)
           | Primitives.Int_LE => doBinaryOp (L.LE, true)
           | Primitives.Int_GE => doBinaryOp (L.GE, true)
           | Primitives.Word_EQUAL => doBinaryOp (L.EQUAL, true)
           | Primitives.Word_PLUS => doBinaryOp (L.PLUS, true)
           | Primitives.Word_MINUS => doBinaryOp (L.MINUS, true)
           | Primitives.Word_TIMES => doBinaryOp (L.TIMES, true)
           | Primitives.Word_TILDE => doUnary (fn (stmts, env, a) =>
                                                  putPureTo ctx env dest (stmts, L.UnaryExp (L.NEGATE, a))
                                              )
           | Primitives.Real_PLUS => doBinaryOp (L.PLUS, true)
           | Primitives.Real_MINUS => doBinaryOp (L.MINUS, true)
           | Primitives.Real_TIMES => (case #targetLuaVersion ctx of
                                           LUA5_3 => doBinaryOp (L.TIMES, true)
                                         | LUAJIT => doBinary (fn (stmts, env, (a, b)) =>
                                                                  putPureTo ctx env dest (stmts, L.CallExp (L.VarExp (L.PredefinedId "__Real_mul"), vector [a, b]))
                                                              )
                                      )
           | Primitives.Real_DIVIDE => doBinaryOp (L.DIV, true)
           | Primitives.Real_TILDE => doUnary (fn (stmts, env, a) =>
                                                  case #targetLuaVersion ctx of
                                                      LUA5_3 => putPureTo ctx env dest (stmts, L.UnaryExp (L.NEGATE, a))
                                                    | LUAJIT => putPureTo ctx env dest (stmts, L.BinExp (L.MINUS, L.VarExp (L.PredefinedId "NEGATIVE_ZERO"),  a))
                                              )
           | Primitives.Real_LT => doBinaryOp (L.LT, true)
           | Primitives.Real_GT => doBinaryOp (L.GT, true)
           | Primitives.Real_LE => doBinaryOp (L.LE, true)
           | Primitives.Real_GE => doBinaryOp (L.GE, true)
           | Primitives.Char_EQUAL => doBinaryOp (L.EQUAL, true)
           | Primitives.Char_LT => doBinaryOp (L.LT, true)
           | Primitives.Char_GT => doBinaryOp (L.GT, true)
           | Primitives.Char_LE => doBinaryOp (L.LE, true)
           | Primitives.Char_GE => doBinaryOp (L.GE, true)
           | Primitives.String_EQUAL => doBinaryOp (L.EQUAL, true)
           | Primitives.String_LT => doBinaryOp (L.LT, true)
           | Primitives.String_GT => doBinaryOp (L.GT, true)
           | Primitives.String_LE => doBinaryOp (L.LE, true)
           | Primitives.String_GE => doBinaryOp (L.GE, true)
           | Primitives.String_HAT => doBinaryOp (L.CONCAT, true)
           | Primitives.String_size => doUnary (fn (stmts, env, a) =>
                                                   putPureTo ctx env dest (stmts, L.UnaryExp (L.LENGTH, a))
                                               )
           | Primitives.String_str => if Vector.length args = 1 then
                                          doExpTo ctx env (Vector.sub (args, 0)) dest
                                      else
                                          raise CodeGenError "primop String.str: invalid number of arguments"
           | Primitives.Vector_length => doUnary (fn (stmts, env, a) =>
                                                     putPureTo ctx env dest (stmts, L.IndexExp (a, L.ConstExp (L.LiteralString "n")))
                                                 )
           | Primitives.Vector_unsafeFromListRevN => doBinary (fn (stmts, env, (n, xs)) =>
                                                                  putPureTo ctx env dest (stmts, L.CallExp (L.VarExp (L.PredefinedId "_Vector_unsafeFromListRevN"), vector [n, xs]))
                                                              )
           | Primitives.Array_EQUAL => doBinaryOp (L.EQUAL, true)
           | Primitives.Array_length => doUnary (fn (stmts, env, a) =>
                                                    putPureTo ctx env dest (stmts, L.IndexExp (a, L.ConstExp (L.LiteralString "n")))
                                                )
           | Primitives.Unsafe_cast => if Vector.length args = 1 then
                                           doExpTo ctx env (Vector.sub (args, 0)) dest
                                       else
                                           raise CodeGenError ("primop " ^ Primitives.toString primOp ^ ": invalid number of arguments")
           | Primitives.Unsafe_Vector_sub => doBinary (fn (stmts, env, (vec, i)) =>
                                                          putPureTo ctx env dest (stmts, L.IndexExp (vec, L.BinExp (L.PLUS, i, L.ConstExp (L.Numeral "1"))))
                                                      )
           | Primitives.Unsafe_Array_sub => doBinary (fn (stmts, env, (arr, i)) =>
                                                         putImpureTo ctx env dest (stmts, L.IndexExp (arr, L.BinExp (L.PLUS, i, L.ConstExp (L.Numeral "1"))))
                                                     )
           | Primitives.Unsafe_Array_update => doTernary (fn (stmts, env, (arr, i, v)) =>
                                                             let val stmts = stmts @ [ L.AssignStat ([L.IndexExp (arr, L.BinExp (L.PLUS, i, L.ConstExp (L.Numeral "1")))], [v]) ]
                                                             in putPureTo ctx env dest (stmts, L.ConstExp L.Nil)
                                                             end
                                                         )
           | Primitives.Exception_instanceof => doBinary (fn (stmts, env, (e, tag)) =>
                                                             putPureTo ctx env dest (stmts, L.CallExp (L.VarExp (L.PredefinedId "__exn_instanceof"), vector [e, tag]))
                                                         )
           | Primitives.Lua_sub => doBinary (fn (stmts, env, (a, b)) =>
                                                putImpureTo ctx env dest (stmts, L.IndexExp (a, b))
                                            )
           | Primitives.Lua_set => doTernary (fn (stmts, env, (a, b, c)) =>
                                                 let val stmts = stmts @ [ L.AssignStat ([L.IndexExp (a, b)], [c]) ]
                                                 in putPureTo ctx env dest (stmts, L.ConstExp L.Nil)
                                                 end
                                             )
           | Primitives.Lua_isNil => doUnary (fn (stmts, env, a) =>
                                                 putPureTo ctx env dest (stmts, L.BinExp (L.EQUAL, a, L.ConstExp L.Nil))
                                             )
           | Primitives.Lua_EQUAL => doBinaryOp (L.EQUAL, false)
           | Primitives.Lua_NOTEQUAL => doBinaryOp (L.NOTEQUAL, false)
           | Primitives.Lua_LT => doBinaryOp (L.LT, false)
           | Primitives.Lua_GT => doBinaryOp (L.GT, false)
           | Primitives.Lua_LE => doBinaryOp (L.LE, false)
           | Primitives.Lua_GE => doBinaryOp (L.GE, false)
           | Primitives.Lua_PLUS => doBinaryOp (L.PLUS, false)
           | Primitives.Lua_MINUS => doBinaryOp (L.MINUS, false)
           | Primitives.Lua_TIMES => doBinaryOp (L.TIMES, false)
           | Primitives.Lua_DIVIDE => doBinaryOp (L.DIV, false)
           | Primitives.Lua_INTDIV => doBinaryOp (L.INTDIV, false)
           | Primitives.Lua_MOD => doBinaryOp (L.MOD, false)
           | Primitives.Lua_pow => doBinaryOp (L.POW, false)
           | Primitives.Lua_unm => doUnary (fn (stmts, env, a) =>
                                               putImpureTo ctx env dest (stmts, L.UnaryExp (L.NEGATE, a))
                                           )
           | Primitives.Lua_andb => doBinaryOp (L.BITAND, false)
           | Primitives.Lua_orb => doBinaryOp (L.BITOR, false)
           | Primitives.Lua_xorb => doBinaryOp (L.BITXOR, false)
           | Primitives.Lua_notb => doUnary (fn (stmts, env, a) =>
                                                putImpureTo ctx env dest (stmts, L.UnaryExp (L.BITNOT, a))
                                            )
           | Primitives.Lua_LSHIFT => doBinaryOp (L.LSHIFT, false)
           | Primitives.Lua_RSHIFT => doBinaryOp (L.RSHIFT, false)
           | Primitives.Lua_concat => doBinaryOp (L.CONCAT, false)
           | Primitives.Lua_length => doUnary (fn (stmts, env, a) =>
                                                  putImpureTo ctx env dest (stmts, L.UnaryExp (L.LENGTH, a))
                                              )
           | Primitives.Lua_isFalsy => doUnary (fn (stmts, env, a) =>
                                                   putPureTo ctx env dest (stmts, L.UnaryExp (L.NOT, a))
                                               )
           | Primitives.Lua_call0 => doBinary (fn (stmts, env, (f, args)) =>
                                                  let val (v, stmts') = case args of
                                                                            L.VarExp _ => (args, stmts)
                                                                          | _ => let val v = genSym ctx
                                                                                 in (L.VarExp (L.UserDefinedId v), stmts @ [L.LocalStat ([(v, L.CONST)], [args])])
                                                                                 end
                                                      val stmts'' = stmts'
                                                                    @ [ L.CallStat (f, vector [L.CallExp (L.VarExp (L.PredefinedId "table_unpack"), vector [v, L.ConstExp (L.Numeral "1"), L.IndexExp (v, L.ConstExp (L.LiteralString "n"))])]) ]
                                                  in putPureTo ctx env dest (stmts'', L.ConstExp L.Nil)
                                                  end
                                              )
           | Primitives.Lua_call1 => doBinary (fn (stmts, env, (f, args)) =>
                                                  let val (v, stmts') = case args of
                                                                            L.VarExp _ => (args, stmts)
                                                                          | _ => let val v = genSym ctx
                                                                                 in (L.VarExp (L.UserDefinedId v), stmts @ [L.LocalStat ([(v, L.CONST)], [args])])
                                                                                 end
                                                  in putImpureTo ctx env dest (stmts', L.SingleValueExp (L.CallExp (f, vector [L.CallExp (L.VarExp (L.PredefinedId "table_unpack"), vector [v, L.ConstExp (L.Numeral "1"), L.IndexExp (v, L.ConstExp (L.LiteralString "n"))])])))
                                                  end
                                              )
           | Primitives.Lua_call2 => doBinary (fn (stmts, env, (f, args)) =>
                                                  let val (v, stmts') = case args of
                                                                            L.VarExp _ => (args, stmts)
                                                                          | _ => let val v = genSym ctx
                                                                                 in (L.VarExp (L.UserDefinedId v), stmts @ [L.LocalStat ([(v, L.CONST)], [args])])
                                                                                 end
                                                      val r0 = genSym ctx
                                                      val r1 = genSym ctx
                                                      val stmts'' = stmts'
                                                                    @ [ L.LocalStat ([(r0, L.CONST), (r1, L.CONST)], [L.CallExp (f, vector [L.CallExp (L.VarExp (L.PredefinedId "table_unpack"), vector [v, L.ConstExp (L.Numeral "1"), L.IndexExp (v, L.ConstExp (L.LiteralString "n"))])])]) ]
                                                  in putPureTo ctx env dest (stmts'', L.TableExp (vector [(L.IntKey 1, L.VarExp (L.UserDefinedId r0)), (L.IntKey 2, L.VarExp (L.UserDefinedId r1))]))
                                                  end
                                              )
           | Primitives.Lua_call3 => doBinary (fn (stmts, env, (f, args)) =>
                                                  let val (v, stmts') = case args of
                                                                            L.VarExp _ => (args, stmts)
                                                                          | _ => let val v = genSym ctx
                                                                                 in (L.VarExp (L.UserDefinedId v), stmts @ [L.LocalStat ([(v, L.CONST)], [args])])
                                                                                 end
                                                      val r0 = genSym ctx
                                                      val r1 = genSym ctx
                                                      val r2 = genSym ctx
                                                      val stmts'' = stmts'
                                                                    @ [ L.LocalStat ([(r0, L.CONST), (r1, L.CONST), (r2, L.CONST)], [L.CallExp (f, vector [L.CallExp (L.VarExp (L.PredefinedId "table_unpack"), vector [v, L.ConstExp (L.Numeral "1"), L.IndexExp (v, L.ConstExp (L.LiteralString "n"))])])]) ]
                                                  in putPureTo ctx env dest (stmts'', L.TableExp (vector [(L.IntKey 1, L.VarExp (L.UserDefinedId r0)), (L.IntKey 2, L.VarExp (L.UserDefinedId r1)), (L.IntKey 3, L.VarExp (L.UserDefinedId r2))]))
                                                  end
                                              )
           | _ => raise CodeGenError ("primop " ^ Primitives.toString primOp ^ " is not supported on Lua backend")
      end
  | doExpTo ctx env (F.PrimExp (F.ConstructValOp info, _, _)) dest
    = let val tag = #tag info
      in putPureTo ctx env dest ([], L.TableExp (vector [(L.StringKey "tag", L.ConstExp (L.LiteralString tag))]))
      end
  | doExpTo ctx env (F.PrimExp (F.ConstructValWithPayloadOp info, _, args)) dest
    = if Vector.length args = 1 then
          let val tag = #tag info
              val payload = Vector.sub (args, 0)
          in doExpCont ctx env payload (fn (stmts, env, payload) =>
                                           putPureTo ctx env dest (stmts, L.TableExp (vector [(L.StringKey "tag", L.ConstExp (L.LiteralString tag)), (L.StringKey "payload", payload)]))
                                       )
          end
      else
          raise CodeGenError "ConstructValWithPayloadOp: invalid number of arguments"
  | doExpTo ctx env (F.PrimExp (F.ConstructExnOp, _, args)) dest
    = if Vector.length args = 1 then
          let val tag = Vector.sub (args, 0)
          in doExpCont ctx env tag (fn (stmts, env, tag) =>
                                       putPureTo ctx env dest (stmts, L.CallExp (L.VarExp (L.PredefinedId "setmetatable"), vector [L.TableExp (vector [(L.StringKey "tag", tag)]), L.VarExp (L.PredefinedId "_exn_meta")]))
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
                                                                     putPureTo ctx env dest (stmts0 @ stmts1, L.CallExp (L.VarExp (L.PredefinedId "setmetatable"), vector [L.TableExp (vector [(L.StringKey "tag", tag), (L.StringKey "payload", payload)]), L.VarExp (L.PredefinedId "_exn_meta")]))
                                                                 )
                                   )
          end
      else
          raise CodeGenError "ConstructExnWithPayloadOp: invalid number of arguments"
(* doDec : Context -> Env -> F.Dec -> L.Stat list *)
and doDec ctx env (F.ValDec (vid, _, exp)) : L.Stat list
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
    = [ if isHoisted (env, tagName) then
            L.AssignStat ([L.VarExp (L.UserDefinedId tagName)], [L.TableExp (vector [(L.IntKey 1, L.ConstExp (L.LiteralString name))])])
        else
            L.LocalStat ([(tagName, L.CONST)], [L.TableExp (vector [(L.IntKey 1, L.ConstExp (L.LiteralString name))])])
      ]
  | doDec ctx env (F.ExportValue _) = raise Fail "internal error: ExportValue must be the last statement"
  | doDec ctx env (F.ExportModule _) = raise Fail "internal error: ExportModule must be the last statement"
  | doDec ctx env (F.GroupDec (SOME hoist, decs)) = let val (env, dec) = declareIfNotHoisted (env, TypedSyntax.VIdSet.toList hoist)
                                                    in dec
                                                       @ [ L.DoStat (vector (doDecs ctx (increaseLevel env) decs)) ]
                                                    end
  | doDec ctx env (F.GroupDec (NONE, decs)) = doDecs ctx env decs (* should be an error? *)
and doDecs ctx env [F.ExportValue exp] = doExpTo ctx env exp Return
  | doDecs ctx env [F.ExportModule fields] = mapCont (fn ((label, exp), cont) => doExpCont ctx env exp (fn (stmts, env, e) => cont (stmts, (label, e))))
                                                     (Vector.foldr (op ::) [] fields)
                                                     (fn ys => let val (stmts, fields') = ListPair.unzip ys
                                                               in putPureTo ctx env Return ( List.concat stmts
                                                                                           , L.TableExp (vector (List.map (fn (label, e) => (L.StringKey label, e)) fields'))
                                                                                           )
                                                               end
                                                     )
  | doDecs ctx env (dec :: decs) = doDec ctx env dec @ doDecs ctx env decs
  | doDecs ctx env [] = []

fun doProgram ctx env decs = vector (doDecs ctx env decs)
fun doProgramWithStacklessHandle ctx env decs = let val func = L.FunctionExp (vector [], vector (doDecs ctx env decs))
                                                in vector [L.ReturnStat (vector [L.CallExp (L.VarExp (L.PredefinedId "_run"), vector [func])])]
                                                end

end (* structure CodeGenLua *)
