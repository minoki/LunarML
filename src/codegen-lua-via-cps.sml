(*
 * Copyright (c) 2023 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure CodeGenLuaViaCps = struct
exception CodeGenError of string

datatype target_lua_version = LUA5_3 | LUAJIT
type Context = { nextLuaId : int ref
               , targetLuaVersion : target_lua_version
               , hasDelimitedContinuations : bool
               }

val builtins
    = let open InitialEnv
      in List.foldl (fn ((vid, name), map) => TypedSyntax.VIdMap.insert (map, vid, name)) TypedSyntax.VIdMap.empty
                    [(* list *)
                     (VId_nil, "nil")
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
                    ,(VId_Lua_Lib_string_char, "string_char")
                    ,(VId_Lua_Lib_string_format, "string_format")
                    ,(VId_Lua_Lib_table, "table")
                    ,(VId_Lua_Lib_table_pack, "table_pack")
                    ,(VId_Lua_Lib_table_unpack, "table_unpack")
                    ]
      end
val builtinsLuaJIT
    = let open InitialEnv
      in List.foldl (fn ((vid, name), map) => TypedSyntax.VIdMap.insert (map, vid, name)) TypedSyntax.VIdMap.empty
                    [(* list *)
                     (VId_nil, "nil")
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
                    ,(VId_Lua_Lib_string_char, "string_char")
                    ,(VId_Lua_Lib_string_format, "string_format")
                    ,(VId_Lua_Lib_table, "table")
                    ,(VId_Lua_Lib_table_pack, "table_pack")
                    ,(VId_Lua_Lib_table_unpack, "table_unpack")
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

fun LabelToTableKey (Syntax.NumericLabel n) = LuaSyntax.IntKey n
  | LabelToTableKey (Syntax.IdentifierLabel s) = LuaSyntax.StringKey s

fun genSym (ctx: Context) = let val n = !(#nextLuaId ctx)
                                val _ = #nextLuaId ctx := n + 1
                            in TypedSyntax.MkVId ("tmp", n)
                            end
fun genSymWithName (ctx : Context, name : string)
    = let val n = !(#nextLuaId ctx)
          val _ = #nextLuaId ctx := n + 1
      in TypedSyntax.MkVId (name, n)
      end

structure F = FSyntax
structure C = CSyntax
structure L = struct
fun ConstStat (vid : TypedSyntax.VId, e : LuaSyntax.Exp) = LuaSyntax.LocalStat ([(vid, LuaSyntax.CONST)], [e])
open LuaSyntax
end

datatype cont_type = GOTO of { label : L.Id, params : L.Id list }
                   | RETURN
type Env = { continuations : cont_type C.CVarMap.map
           }

datatype purity = PURE | DISCARDABLE | IMPURE

fun applyCont (ctx : Context, env : Env, cont : C.CVar, args : L.Exp list)
    = case C.CVarMap.find (#continuations env, cont) of
          SOME (GOTO { label, params = [] }) => [L.GotoStat label]
        | SOME (GOTO { label, params }) => [L.AssignStat (List.map L.VarExp params, args), L.GotoStat label]
        | SOME RETURN => [L.ReturnStat (vector args)]
        | NONE => raise CodeGenError "undefined continuation"

fun doLabel cname = L.UserDefinedId (TypedSyntax.MkVId ("cont", C.CVar.toInt cname))

fun doValue ctx (C.Var vid) = (case VIdToLua (ctx, vid) of
                                   L.PredefinedId "nil" => L.ConstExp L.Nil
                                 | L.PredefinedId "false" => L.ConstExp L.False
                                 | L.PredefinedId "true" => L.ConstExp L.True
                                 | id => L.VarExp id
                              )
  | doValue ctx C.Unit = L.ConstExp L.Nil
  | doValue ctx (C.BoolConst false) = L.ConstExp L.False
  | doValue ctx (C.BoolConst true) = L.ConstExp L.True
  | doValue ctx (C.NativeIntConst x) = if x < 0 then
                                           if x = ~0x800000000000 then
                                               L.BinExp (L.MINUS, L.UnaryExp (L.NEGATE, L.ConstExp (L.Numeral (LargeInt.toString (~ (x + 1))))), L.ConstExp (L.Numeral "1"))
                                           else
                                               L.UnaryExp (L.NEGATE, L.ConstExp (L.Numeral (LargeInt.toString (~ x))))
                                       else
                                           L.ConstExp (L.Numeral (LargeInt.toString x))
  | doValue ctx (C.Int32Const x) = if x < 0 then
                                       L.UnaryExp (L.NEGATE, L.ConstExp (L.Numeral (LargeInt.toString (~ (Int32.toLarge x)))))
                                   else
                                       L.ConstExp (L.Numeral (Int32.toString x))
  | doValue ctx (C.IntInfConst x) = raise CodeGenError "IntInfConst is not supported by Lua backend"
  | doValue ctx (C.NativeWordConst x) = L.ConstExp (L.Numeral ("0x" ^ LargeInt.fmt StringCvt.HEX x))
  | doValue ctx (C.Word32Const x) = L.ConstExp (L.Numeral ("0x" ^ Word32.toString x))
  | doValue ctx (C.CharConst c) = L.ConstExp (L.Numeral (Int.toString (Char.ord c)))
  | doValue ctx (C.Char16Const _) = raise CodeGenError "Char16Const is not supported by Lua backend"
  | doValue ctx (C.StringConst s) = L.ConstExp (L.LiteralString (CharVector.tabulate (Vector.length s, fn i => Char.chr (Vector.sub (s, i)))))
  | doValue ctx (C.String16Const _) = raise CodeGenError "String16Const is not supported by Lua backend"

fun doCExp (ctx : Context) (env : Env) (C.Let { exp = C.PrimOp { primOp = F.RealConstOp x, tyargs = _, args = _ }, result, cont })
    = let val exp = if Numeric.Notation.isNegative x then
                        case (#targetLuaVersion ctx, Numeric.Notation.isNegativeZero x) of
                            (LUAJIT, true) => L.VarExp (L.PredefinedId "NEGATIVE_ZERO")
                          | _ => L.UnaryExp (L.NEGATE, L.ConstExp (L.Numeral (Numeric.Notation.toString "-" (Numeric.Notation.abs x))))
                    else
                        L.ConstExp (L.Numeral (Numeric.Notation.toString "-" x))
      in case result of
             SOME result => L.ConstStat (result, exp) :: doCExp ctx env cont
           | NONE => doCExp ctx env cont
      end
  | doCExp ctx env (C.Let { exp = C.PrimOp { primOp = F.ListOp, tyargs = _, args = [] }, result, cont })
    = (case result of
           SOME result => L.ConstStat (result, L.ConstExp L.Nil) :: doCExp ctx env cont
         | NONE => doCExp ctx env cont
      )
  | doCExp ctx env (C.Let { exp = C.PrimOp { primOp = F.ListOp, tyargs = _, args = xs }, result, cont })
    = (case result of
           SOME result => let fun doFields (i, []) = []
                                | doFields (i, y :: ys) = (L.IntKey i, doValue ctx y) :: doFields (i + 1, ys)
                          in L.ConstStat (result, L.CallExp (L.VarExp (L.PredefinedId "_list"), vector [L.TableExp (vector ((L.StringKey "n", L.ConstExp (L.Numeral (Int.toString (List.length xs)))) :: doFields (1, xs)))])) :: doCExp ctx env cont
                          end
         | NONE => doCExp ctx env cont
      )
  | doCExp ctx env (C.Let { exp = C.PrimOp { primOp = F.VectorOp, tyargs = _, args = xs }, result, cont })
    = (case result of
           SOME result => let fun doFields (i, []) = []
                                | doFields (i, y :: ys) = (L.IntKey i, doValue ctx y) :: doFields (i + 1, ys)
                          in L.ConstStat (result, L.TableExp (vector ((L.StringKey "n", L.ConstExp (L.Numeral (Int.toString (List.length xs)))) :: doFields (1, xs)))) :: doCExp ctx env cont
                          end
         | NONE => doCExp ctx env cont
      )
  | doCExp ctx env (C.Let { exp = C.PrimOp { primOp = F.DataTagAsStringOp info, tyargs = _, args = [exp] }, result, cont })
    = (case result of
           SOME result => L.ConstStat (result, L.IndexExp (doValue ctx exp, L.ConstExp (L.LiteralString "tag"))) :: doCExp ctx env cont
         | NONE => doCExp ctx env cont
      )
  | doCExp ctx env (C.Let { exp = C.PrimOp { primOp = F.DataPayloadOp info, tyargs = _, args = [exp] }, result, cont })
    = (case result of
           SOME result => L.ConstStat (result, L.IndexExp (doValue ctx exp, L.ConstExp (L.LiteralString "payload"))) :: doCExp ctx env cont
         | NONE => doCExp ctx env cont
      )
  | doCExp ctx env (C.Let { exp = C.PrimOp { primOp = F.ExnPayloadOp, tyargs = _, args = [exp] }, result, cont })
    = (case result of
           SOME result => L.ConstStat (result, L.IndexExp (doValue ctx exp, L.ConstExp (L.LiteralString "payload"))) :: doCExp ctx env cont
         | NONE => doCExp ctx env cont
      )
  | doCExp ctx env (C.Let { exp = C.PrimOp { primOp = F.ConstructValOp info, tyargs = _, args = [] }, result, cont })
    = let val tag = #tag info
      in case result of
             SOME result => L.ConstStat (result, L.TableExp (vector [(L.StringKey "tag", L.ConstExp (L.LiteralString tag))])) :: doCExp ctx env cont
           | NONE => doCExp ctx env cont
      end
  | doCExp ctx env (C.Let { exp = C.PrimOp { primOp = F.ConstructValWithPayloadOp info, tyargs = _, args = [payload] }, result, cont })
    = let val tag = #tag info
          val payload = doValue ctx payload
      in case result of
             SOME result => L.ConstStat (result, L.TableExp (vector [(L.StringKey "tag", L.ConstExp (L.LiteralString tag)), (L.StringKey "payload", payload)])) :: doCExp ctx env cont
           | NONE => doCExp ctx env cont
      end
  | doCExp ctx env (C.Let { exp = C.PrimOp { primOp = F.ConstructExnOp, tyargs = _, args = [tag] }, result, cont })
    = let val tag = doValue ctx tag
      in case result of
             SOME result => L.ConstStat (result, L.TableExp (vector [(L.StringKey "tag", tag)])) :: doCExp ctx env cont
           | NONE => doCExp ctx env cont
      end
  | doCExp ctx env (C.Let { exp = C.PrimOp { primOp = F.ConstructExnWithPayloadOp, tyargs = _, args = [tag, payload] }, result, cont })
    = let val tag = doValue ctx tag
          val payload = doValue ctx payload
      in case result of
             SOME result => L.ConstStat (result, L.TableExp (vector [(L.StringKey "tag", tag), (L.StringKey "payload", payload)])) :: doCExp ctx env cont
           | NONE => doCExp ctx env cont
      end
  | doCExp ctx env (C.Let { exp = C.PrimOp { primOp = F.RaiseOp (span as { start as { file, line, column }, ... }), tyargs = _, args = [exp] }, result, cont })
    = let val exp = doValue ctx exp
          val locationInfo = if start = SourcePos.nullPos then
                                 L.ConstExp L.Nil
                             else
                                 L.ConstExp (L.LiteralString (OS.Path.file file ^ ":" ^ Int.toString line ^ ":" ^ Int.toString column))
      in [L.CallStat (L.VarExp (L.PredefinedId "_raise"), vector [exp, locationInfo])]
      end
  | doCExp ctx env (C.Let { exp = C.PrimOp { primOp = F.PrimFnOp prim, tyargs, args }, result, cont })
    = let fun ExpStat (L.CallExp (f, args)) = [L.CallStat (f, args)]
            | ExpStat (L.MethodExp (self, name, args)) = [L.MethodStat (self, name, args)]
            | ExpStat (L.ConstExp _) = []
            | ExpStat e = [L.CallStat (L.VarExp (L.PredefinedId "_id"), vector [e])]
          fun ConstStatOrExpStat e = case result of
                                         SOME result => [L.ConstStat (result, e)]
                                       | NONE => ExpStat e
          fun doUnary f = case args of
                              [a] => f (doValue ctx a)
                            | _ => raise CodeGenError ("primop " ^ Primitives.toString prim ^ ": invalid number of arguments")
          fun doUnaryExp (f, purity) = doUnary (fn a =>
                                                   let val stmt = case result of
                                                                      SOME result => [L.ConstStat (result, f a)]
                                                                    | NONE => case purity of
                                                                                  PURE => []
                                                                                | DISCARDABLE => []
                                                                                | IMPURE => ExpStat (f a)
                                                   in stmt @ doCExp ctx env cont
                                                   end
                                               )
          fun doBinary f = case args of
                               [a, b] => f (doValue ctx a, doValue ctx b)
                             | _ => raise CodeGenError ("primop " ^ Primitives.toString prim ^ ": invalid number of arguments")
          fun doBinaryExp (f, purity) = doBinary (fn (a, b) =>
                                                     let val stmt = case result of
                                                                        SOME result => [L.ConstStat (result, f (a, b))]
                                                                      | NONE => case purity of
                                                                                    PURE => []
                                                                                  | DISCARDABLE => []
                                                                                  | IMPURE => ExpStat (f (a, b))
                                                     in stmt @ doCExp ctx env cont
                                                     end
                                                 )
          fun doBinaryOp (binop, purity) = doBinaryExp (fn (a, b) => L.BinExp (binop, a, b), purity)
          fun doTernary f = case args of
                                [a, b, c] => f (doValue ctx a, doValue ctx b, doValue ctx c)
                              | _ => raise CodeGenError ("primop " ^ Primitives.toString prim ^ ": invalid number of arguments")
      in case prim of
             Primitives.call2 => doTernary (fn (f, a0, a1) =>
                                               ConstStatOrExpStat (L.CallExp (f, vector [a0, a1])) @ doCExp ctx env cont
                                           )
           | Primitives.List_cons => doBinaryExp (fn (x, xs) => L.TableExp (vector [(L.IntKey 1, x), (L.IntKey 2, xs)]), PURE)
           | Primitives.List_null => doUnaryExp (fn a => L.BinExp (L.EQUAL, a, L.ConstExp L.Nil), PURE)
           | Primitives.List_unsafeHead => doUnaryExp (fn xs => L.IndexExp (xs, L.ConstExp (L.Numeral "1")), PURE)
           | Primitives.List_unsafeTail => doUnaryExp (fn xs => L.IndexExp (xs, L.ConstExp (L.Numeral "2")), PURE)
           | Primitives.Ref_ref => doUnaryExp ( fn x =>
                                                   (* REPRESENTATION_OF_REF *)
                                                   L.TableExp (vector [(L.StringKey "tag", L.ConstExp (L.LiteralString "ref"))
                                                                      ,(L.StringKey "payload", x)
                                                                      ]
                                                              )
                                              , DISCARDABLE
                                              )
           | Primitives.Ref_EQUAL => doBinaryOp (L.EQUAL, PURE)
           | Primitives.Ref_set => doBinary (fn (a, b) =>
                                                (* REPRESENTATION_OF_REF *)
                                                let val stmt = L.AssignStat ([L.IndexExp (a, L.ConstExp (L.LiteralString "payload"))], [b])
                                                in stmt :: ConstStatOrExpStat (L.ConstExp L.Nil) @ doCExp ctx env cont
                                                end
                                            )
           | Primitives.Ref_read => doUnaryExp ( fn a =>
                                                    (* REPRESENTATION_OF_REF *)
                                                    L.IndexExp (a, L.ConstExp (L.LiteralString "payload"))
                                               , DISCARDABLE
                                               )
           | Primitives.Bool_EQUAL => doBinaryOp (L.EQUAL, PURE)
           | Primitives.Bool_not => doUnaryExp (fn a => L.UnaryExp (L.NOT, a), PURE)
           | Primitives.Int_EQUAL => doBinaryOp (L.EQUAL, PURE)
           | Primitives.Int_LT => doBinaryOp (L.LT, PURE)
           | Primitives.Int_GT => doBinaryOp (L.GT, PURE)
           | Primitives.Int_LE => doBinaryOp (L.LE, PURE)
           | Primitives.Int_GE => doBinaryOp (L.GE, PURE)
           | Primitives.Word_EQUAL => doBinaryOp (L.EQUAL, PURE)
           | Primitives.Word_PLUS => doBinaryOp (L.PLUS, PURE) (* not used on LuaJIT *)
           | Primitives.Word_MINUS => doBinaryOp (L.MINUS, PURE) (* not used on LuaJIT *)
           | Primitives.Word_TIMES => doBinaryOp (L.TIMES, PURE) (* not used on LuaJIT *)
           | Primitives.Word_TILDE => doUnaryExp (fn a => L.UnaryExp (L.NEGATE, a), PURE) (* not used on LuaJIT *)
           | Primitives.Real_PLUS => doBinaryOp (L.PLUS, PURE)
           | Primitives.Real_MINUS => doBinaryOp (L.MINUS, PURE)
           | Primitives.Real_TIMES => (case #targetLuaVersion ctx of
                                           LUA5_3 => doBinaryOp (L.TIMES, PURE)
                                         | LUAJIT => doBinaryExp (fn (a, b) => L.CallExp (L.VarExp (L.PredefinedId "__Real_mul"), vector [a, b]), PURE)
                                      )
           | Primitives.Real_DIVIDE => doBinaryOp (L.DIV, PURE)
           | Primitives.Real_TILDE => doUnaryExp ( fn a => case #targetLuaVersion ctx of
                                                               LUA5_3 => L.UnaryExp (L.NEGATE, a)
                                                             | LUAJIT => L.BinExp (L.MINUS, L.VarExp (L.PredefinedId "NEGATIVE_ZERO"), a)
                                                 , PURE
                                                 )
           | Primitives.Real_LT => doBinaryOp (L.LT, PURE)
           | Primitives.Real_GT => doBinaryOp (L.GT, PURE)
           | Primitives.Real_LE => doBinaryOp (L.LE, PURE)
           | Primitives.Real_GE => doBinaryOp (L.GE, PURE)
           | Primitives.Char_EQUAL => doBinaryOp (L.EQUAL, PURE)
           | Primitives.Char_LT => doBinaryOp (L.LT, PURE)
           | Primitives.Char_GT => doBinaryOp (L.GT, PURE)
           | Primitives.Char_LE => doBinaryOp (L.LE, PURE)
           | Primitives.Char_GE => doBinaryOp (L.GE, PURE)
           | Primitives.String_EQUAL => doBinaryOp (L.EQUAL, PURE)
           | Primitives.String_LT => doBinaryOp (L.LT, PURE)
           | Primitives.String_GT => doBinaryOp (L.GT, PURE)
           | Primitives.String_LE => doBinaryOp (L.LE, PURE)
           | Primitives.String_GE => doBinaryOp (L.GE, PURE)
           | Primitives.String_HAT => doBinaryOp (L.CONCAT, PURE)
           | Primitives.String_size => doUnaryExp (fn a => L.UnaryExp (L.LENGTH, a), PURE)
           | Primitives.String_str => doUnaryExp (fn a => L.CallExp (L.VarExp (L.PredefinedId "string_char"), vector [a]), PURE)
           | Primitives.Vector_length => doUnaryExp (fn a => L.IndexExp (a, L.ConstExp (L.LiteralString "n")), PURE)
           | Primitives.Vector_unsafeFromListRevN => doBinaryExp (fn (n, xs) => L.CallExp (L.VarExp (L.PredefinedId "_Vector_unsafeFromListRevN"), vector [n, xs]), PURE)
           | Primitives.Array_EQUAL => doBinaryOp (L.EQUAL, PURE)
           | Primitives.Array_length => doUnaryExp (fn a => L.IndexExp (a, L.ConstExp (L.LiteralString "n")), PURE)
           | Primitives.Unsafe_Vector_sub => doBinaryExp (fn (vec, i) => L.IndexExp (vec, L.BinExp (L.PLUS, i, L.ConstExp (L.Numeral "1"))), PURE)
           | Primitives.Unsafe_Array_sub => doBinaryExp (fn (arr, i) => L.IndexExp (arr, L.BinExp (L.PLUS, i, L.ConstExp (L.Numeral "1"))), IMPURE)
           | Primitives.Unsafe_Array_update => doTernary (fn (arr, i, v) =>
                                                             let val stmt = L.AssignStat ([L.IndexExp (arr, L.BinExp (L.PLUS, i, L.ConstExp (L.Numeral "1")))], [v])
                                                             in stmt :: ConstStatOrExpStat (L.ConstExp L.Nil) @ doCExp ctx env cont
                                                             end
                                                         )
           | Primitives.Exception_instanceof => doBinaryExp (fn (e, tag) => L.CallExp (L.VarExp (L.PredefinedId "__exn_instanceof"), vector [e, tag]), PURE)
           | Primitives.Lua_sub => doBinaryExp (fn (a, b) => L.IndexExp (a, b), IMPURE)
           | Primitives.Lua_set => doTernary (fn (a, b, c) =>
                                                 let val stmt = L.AssignStat ([L.IndexExp (a, b)], [c])
                                                 in stmt :: ConstStatOrExpStat (L.ConstExp L.Nil) @ doCExp ctx env cont
                                                 end
                                             )
           | Primitives.Lua_isNil => doUnaryExp (fn a => L.BinExp (L.EQUAL, a, L.ConstExp L.Nil), PURE)
           | Primitives.Lua_EQUAL => doBinaryOp (L.EQUAL, IMPURE)
           | Primitives.Lua_NOTEQUAL => doBinaryOp (L.NOTEQUAL, IMPURE)
           | Primitives.Lua_LT => doBinaryOp (L.LT, IMPURE)
           | Primitives.Lua_GT => doBinaryOp (L.GT, IMPURE)
           | Primitives.Lua_LE => doBinaryOp (L.LE, IMPURE)
           | Primitives.Lua_GE => doBinaryOp (L.GE, IMPURE)
           | Primitives.Lua_PLUS => doBinaryOp (L.PLUS, IMPURE)
           | Primitives.Lua_MINUS => doBinaryOp (L.MINUS, IMPURE)
           | Primitives.Lua_TIMES => doBinaryOp (L.TIMES, IMPURE)
           | Primitives.Lua_DIVIDE => doBinaryOp (L.DIV, IMPURE)
           | Primitives.Lua_INTDIV => doBinaryOp (L.INTDIV, IMPURE)
           | Primitives.Lua_MOD => doBinaryOp (L.MOD, IMPURE)
           | Primitives.Lua_pow => doBinaryOp (L.POW, IMPURE)
           | Primitives.Lua_unm => doUnaryExp (fn a => L.UnaryExp (L.NEGATE, a), IMPURE)
           | Primitives.Lua_andb => doBinaryOp (L.BITAND, IMPURE) (* not used on LuaJIT *)
           | Primitives.Lua_orb => doBinaryOp (L.BITOR, IMPURE) (* not used on LuaJIT *)
           | Primitives.Lua_xorb => doBinaryOp (L.BITXOR, IMPURE) (* not used on LuaJIT *)
           | Primitives.Lua_notb => doUnaryExp (fn a => L.UnaryExp (L.BITNOT, a), IMPURE) (* not used on LuaJIT *)
           | Primitives.Lua_LSHIFT => doBinaryOp (L.LSHIFT, IMPURE) (* not used on LuaJIT *)
           | Primitives.Lua_RSHIFT => doBinaryOp (L.RSHIFT, IMPURE) (* not used on LuaJIT *)
           | Primitives.Lua_concat => doBinaryOp (L.CONCAT, IMPURE)
           | Primitives.Lua_length => doUnaryExp (fn a => L.UnaryExp (L.LENGTH, a), IMPURE)
           | Primitives.Lua_isFalsy => doUnaryExp (fn a => L.UnaryExp (L.NOT, a), PURE)
           | Primitives.Lua_call0 => doBinary (fn (f, args) =>
                                                  let val stmt = L.CallStat (f, vector [L.CallExp (L.VarExp (L.PredefinedId "table_unpack"), vector [args, L.ConstExp (L.Numeral "1"), L.IndexExp (args, L.ConstExp (L.LiteralString "n"))])])
                                                  in stmt :: ConstStatOrExpStat (L.ConstExp L.Nil) @ doCExp ctx env cont
                                                  end
                                              )
           | Primitives.Lua_call1 => doBinary (fn (f, args) =>
                                                  let val arg = vector [L.CallExp (L.VarExp (L.PredefinedId "table_unpack"), vector [args, L.ConstExp (L.Numeral "1"), L.IndexExp (args, L.ConstExp (L.LiteralString "n"))])]
                                                      val stmt = case result of
                                                                     NONE => L.CallStat (f, arg)
                                                                   | SOME result => L.ConstStat (result, L.CallExp (f, arg))
                                                  in stmt :: doCExp ctx env cont
                                                  end
                                              )
           | Primitives.Lua_call2 => doBinary (fn (f, args) =>
                                                  let val arg = vector [L.CallExp (L.VarExp (L.PredefinedId "table_unpack"), vector [args, L.ConstExp (L.Numeral "1"), L.IndexExp (args, L.ConstExp (L.LiteralString "n"))])]
                                                      val stmts = case result of
                                                                     NONE => [L.CallStat (f, arg)]
                                                                   | SOME result => let val r0 = genSym ctx
                                                                                        val r1 = genSym ctx
                                                                                    in [ L.LocalStat ([(r0, L.CONST), (r1, L.CONST)], [L.CallExp (f, arg)])
                                                                                       , L.ConstStat (result, L.TableExp (vector [(L.IntKey 1, L.VarExp (L.UserDefinedId r0)), (L.IntKey 2, L.VarExp (L.UserDefinedId r1))]))
                                                                                       ]
                                                                                    end
                                                  in stmts @ doCExp ctx env cont
                                                  end
                                              )
           | Primitives.Lua_call3 => doBinary (fn (f, args) =>
                                                  let val arg = vector [L.CallExp (L.VarExp (L.PredefinedId "table_unpack"), vector [args, L.ConstExp (L.Numeral "1"), L.IndexExp (args, L.ConstExp (L.LiteralString "n"))])]
                                                      val stmts = case result of
                                                                     NONE => [L.CallStat (f, arg)]
                                                                   | SOME result => let val r0 = genSym ctx
                                                                                        val r1 = genSym ctx
                                                                                        val r2 = genSym ctx
                                                                                    in [ L.LocalStat ([(r0, L.CONST), (r1, L.CONST), (r2, L.CONST)], [L.CallExp (f, arg)])
                                                                                       , L.ConstStat (result, L.TableExp (vector [(L.IntKey 1, L.VarExp (L.UserDefinedId r0)), (L.IntKey 2, L.VarExp (L.UserDefinedId r1)), (L.IntKey 3, L.VarExp (L.UserDefinedId r2))]))
                                                                                       ]
                                                                                    end
                                                  in stmts @ doCExp ctx env cont
                                                  end
                                              )
           | Primitives.DelimCont_newPromptTag => ConstStatOrExpStat (L.TableExp (vector [])) @ doCExp ctx env cont
           | Primitives.DelimCont_pushPrompt => if #hasDelimitedContinuations ctx then
                                                    doBinaryExp (fn (promptTag, action) => L.CallExp (L.VarExp (L.PredefinedId "_pushPrompt"), vector [promptTag, action]), IMPURE)
                                                else
                                                    raise CodeGenError ("primop " ^ Primitives.toString prim ^ " is not supported on Lua backend")
           | Primitives.DelimCont_withSubCont => if #hasDelimitedContinuations ctx then
                                                     doBinaryExp (fn (promptTag, action) => L.CallExp (L.VarExp (L.PredefinedId "_withSubCont"), vector [promptTag, action]), IMPURE)
                                                 else
                                                     raise CodeGenError ("primop " ^ Primitives.toString prim ^ " is not supported on Lua backend")
           | Primitives.DelimCont_pushSubCont => if #hasDelimitedContinuations ctx then
                                                     doBinaryExp (fn (subcont, action) => L.CallExp (L.VarExp (L.PredefinedId "_pushSubCont"), vector [subcont, action]), IMPURE)
                                                 else
                                                     raise CodeGenError ("primop " ^ Primitives.toString prim ^ " is not supported on Lua backend")
           | Primitives.assumeDiscardable => doBinaryExp (fn (f, arg) => L.CallExp (f, vector [arg]), IMPURE)
           | _ => raise CodeGenError ("primop " ^ Primitives.toString prim  ^ " is not supported on Lua backend")
      end
  | doCExp ctx env (C.Let { exp = C.PrimOp { primOp = F.JsCallOp, tyargs = _, args = _ }, result, cont })
    = raise CodeGenError "JsCallOp is not supported on Lua backend"
  | doCExp ctx env (C.Let { exp = C.PrimOp { primOp = F.JsMethodOp, tyargs = _, args = _ }, result, cont })
    = raise CodeGenError "JsMethodOp is not supported on Lua backend"
  | doCExp ctx env (C.Let { exp = C.PrimOp { primOp = F.JsNewOp, tyargs = _, args = _ }, result, cont })
    = raise CodeGenError "JsNewOp is not supported on Lua backend"
  | doCExp ctx env (C.Let { exp = C.PrimOp { primOp, tyargs = _, args = _ }, result, cont })
    = raise CodeGenError ("primop " ^ Printer.build (FPrinter.doPrimOp primOp) ^ " not implemented yet")
  | doCExp ctx env (C.Let { exp = C.Record fields, result, cont }) (* non-empty record *)
    = let val fields = Syntax.LabelMap.foldri (fn (label, v, acc) => (LabelToTableKey label, doValue ctx v) :: acc) [] fields
          val exp = L.TableExp (vector fields)
      in case result of
             SOME result => L.ConstStat (result, exp) :: doCExp ctx env cont
           | NONE => doCExp ctx env cont
      end
  | doCExp ctx env (C.Let { exp = C.ExnTag { name, payloadTy }, result, cont })
    = (case result of
           SOME result => L.ConstStat (result, L.TableExp (vector [(L.IntKey 1, L.ConstExp (L.LiteralString name))])) :: doCExp ctx env cont
         | NONE => doCExp ctx env cont
      )
  | doCExp ctx env (C.Let { exp = C.Projection { label, record, fieldTypes }, result, cont })
    = (case result of
           SOME result => let val label = case label of
                                              Syntax.NumericLabel n => L.ConstExp (L.Numeral (Int.toString n))
                                            | Syntax.IdentifierLabel s => L.ConstExp (L.LiteralString s)
                          in L.ConstStat (result, L.IndexExp (doValue ctx record, label)) :: doCExp ctx env cont
                          end
         | NONE => doCExp ctx env cont
      )
  | doCExp ctx env (C.App { applied, cont, args })
    = (case C.CVarMap.find (#continuations env, cont) of
           SOME (GOTO { label, params }) =>
           let val callAndAssign = case params of
                                       [] => L.CallStat (doValue ctx applied, Vector.map (doValue ctx) (vector args))
                                     | _ => L.AssignStat (List.map L.VarExp params, [L.CallExp (doValue ctx applied, Vector.map (doValue ctx) (vector args))])
           in [callAndAssign, L.GotoStat label]
           end
         | SOME RETURN => [L.ReturnStat (vector [L.CallExp (doValue ctx applied, Vector.map (doValue ctx) (vector args))])] (* tail call *)
         | NONE => raise CodeGenError "undefined continuation"
      )
  | doCExp ctx env (C.AppCont { applied, args }) = applyCont (ctx, env, applied, List.map (doValue ctx) args)
  | doCExp ctx env (C.If { cond, thenCont, elseCont }) = L.IfStat (doValue ctx cond, vector (doCExp ctx env thenCont), vector []) :: doCExp ctx env elseCont (* ad hoc *)
  | doCExp ctx env (C.Let { exp = C.Abs { contParam, params, body }, result, cont })
    = (case result of
           SOME result => let val env' = { continuations = C.CVarMap.singleton (contParam, RETURN) }
                          in L.ConstStat (result, L.FunctionExp (Vector.map (fn vid => VIdToLua (ctx, vid)) (vector params), vector (doCExp ctx env' body))) :: doCExp ctx env cont
                          end
         | NONE => doCExp ctx env cont
      )
  | doCExp ctx env (C.LetRec { defs, cont })
    = let val (decs, assignments) = List.foldr (fn ((name, contParam, params, body), (decs, assignments)) =>
                                                   let val env' = { continuations = C.CVarMap.singleton (contParam, RETURN) }
                                                       val dec = (name, L.LATE_INIT)
                                                       val assignment = L.AssignStat ([L.VarExp (VIdToLua (ctx, name))], [L.FunctionExp (Vector.map (fn vid => VIdToLua (ctx, vid)) (vector params), vector (doCExp ctx env' body))])
                                                   in (dec :: decs, assignment :: assignments)
                                                   end
                                               ) ([], []) defs
      in L.LocalStat (decs, []) :: assignments @ doCExp ctx env cont
      end
  | doCExp ctx env (C.LetCont { name, params, body, cont })
    = let val label = doLabel name
          val env' = { continuations = C.CVarMap.insert (#continuations env, name, GOTO { label = label, params = List.map (fn p => VIdToLua (ctx, p)) params })
                     }
          val decs = if List.null params then
                         []
                     else
                         [L.LocalStat (List.map (fn p => (p, L.LATE_INIT)) params, [])]
      in decs @ L.makeDoStat (doCExp ctx env' cont) @ L.LabelStat label :: doCExp ctx env body (* enclose with do statement? *)
      end
  | doCExp ctx env (C.LetRecCont { defs, cont })
    = let val maxParams = List.foldl (fn ((_, params, _), n) => Int.max (n, List.length params)) 0 defs
          val commonParams = List.tabulate (maxParams, fn _ => genSym ctx)
          val decs = if maxParams > 0 then
                         [L.LocalStat (List.map (fn v => (v, L.MUTABLE)) commonParams, [])]
                     else
                         []
          val env' = { continuations = List.foldl (fn ((name, params, body), m) => C.CVarMap.insert (m, name, GOTO { label = doLabel name, params = List.map L.UserDefinedId (List.take (commonParams, List.length params)) })) (#continuations env) defs
                     }
          val conts = List.map (fn (name, params, body) =>
                                   let val dec = if List.null params then
                                                     []
                                                 else
                                                     [L.LocalStat (List.map (fn v => (v, L.CONST)) params, List.map (fn v => L.VarExp (L.UserDefinedId v)) (List.take (commonParams, List.length params)))]
                                   in L.LabelStat (doLabel name) :: L.makeDoStat (dec @ doCExp ctx env' body)
                                   end
                               ) defs
      in decs @ L.makeDoStat (doCExp ctx env' cont) @ List.concat conts
      end
  | doCExp ctx env (C.Handle { body, handler = (e, h), successfulExitIn, successfulExitOut })
    = let val env' = { continuations = C.CVarMap.singleton (successfulExitIn, RETURN) }
          val status = genSymWithName (ctx, "status")
          val resultOrError = e
          val functionExp = L.FunctionExp (vector [], vector (doCExp ctx env' body))
      in [ L.LocalStat ([(status, L.CONST), (resultOrError, L.CONST)], [L.CallExp (L.VarExp (L.PredefinedId "_handle"), vector [functionExp])])
         , L.IfStat ( L.UnaryExp (L.NOT, L.VarExp (L.UserDefinedId status))
                    , vector (doCExp ctx env h)
                    , vector (applyCont (ctx, env, successfulExitOut, [L.VarExp (L.UserDefinedId e)]))
                    )
         ]
      end

fun doProgram ctx cont cexp
    = let val env = { continuations = C.CVarMap.singleton (cont, RETURN) }
      in vector (doCExp ctx env cexp)
      end
fun doProgramWithContinuations ctx cont cexp
    = let val env = { continuations = C.CVarMap.singleton (cont, RETURN) }
          val func = L.FunctionExp (vector [], vector (doCExp ctx env cexp))
      in vector [L.ReturnStat (vector [L.CallExp (L.VarExp (L.PredefinedId "_run"), vector [func])])]
      end
end;
