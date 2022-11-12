(*
 * Copyright (c) 2021 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure InitialEnv = struct
val initialFixityEnv : Fixity.Env = let fun mkValConMap xs = List.foldl (fn (n, m) => Syntax.VIdMap.insert (m, Syntax.MkVId n, Syntax.ValueConstructor ())) Syntax.VIdMap.empty xs
                                        fun mkExConMap xs = List.foldl (fn (n, m) => Syntax.VIdMap.insert(m, Syntax.MkVId n, Syntax.ExceptionConstructor)) Syntax.VIdMap.empty xs
                                        fun mkTyConMap xs = List.foldl (fn ((n, y), m) => Syntax.TyConMap.insert(m, Syntax.MkTyCon n, y)) Syntax.TyConMap.empty xs
                                        val boolConMap = mkValConMap ["true", "false"]
                                        val refConMap = mkValConMap ["ref"]
                                        val listConMap = mkValConMap ["nil", "::"]
                                    in { fixityMap = Syntax.VIdMap.empty
                                       , idStatusMap = { valMap = List.foldl (Syntax.VIdMap.unionWith #2) (mkExConMap ["Match", "Bind", "Div", "Overflow", "Size", "Subscript", "Fail", "_Prim.Lua.LuaError"]) [boolConMap, refConMap, listConMap]
                                                       , tyConMap = mkTyConMap [("bool", boolConMap)
                                                                               ,("ref", refConMap)
                                                                               ,("list", listConMap)
                                                                               ]
                                                       , strMap = Syntax.StrIdMap.empty
                                                       }
                                       , sigMap = Syntax.SigIdMap.empty
                                       , funMap = Syntax.FunIdMap.empty
                                       }
                                    end

val vidCounter = ref ~3
fun newVId name = let val n = !vidCounter
                  in vidCounter := n - 1
                   ; TypedSyntax.MkVId (name, n)
                  end

(* Ref *)
val VId_ref = newVId "ref"

(* Bool *)
val VId_true = newVId "true"
val VId_false = newVId "false"

(* List *)
val VId_nil = newVId "nil"
val VId_DCOLON = newVId "::"

(* Exception *)
val VId_Match = newVId "Match"
val VId_Bind = Typing.VId_Bind (* TypedSyntax.MkVId ("Bind", ~1) *)
val VId_Div = newVId "Div"
val VId_Overflow = newVId "Overflow"
val VId_Size = newVId "Size"
val VId_Subscript = newVId "Subscript"
val VId_Fail = newVId "Fail"
val VId_Match_tag = newVId "Match"
val VId_Bind_tag = newVId "Bind"
val VId_Div_tag = newVId "Div"
val VId_Overflow_tag = newVId "Overflow"
val VId_Size_tag = newVId "Size"
val VId_Subscript_tag = newVId "Subscript"
val VId_Fail_tag = newVId "Fail"
val VId_exnName = newVId "exnName"

(* Overloaded *)
val VId_abs = newVId "abs"
val VId_TILDE = newVId "~"
val VId_div = newVId "div"
val VId_mod = newVId "mod"
val VId_TIMES = newVId "*"
val VId_DIVIDE = newVId "/"
val VId_PLUS = newVId "+"
val VId_MINUS = newVId "-"
val VId_LT = newVId "<"
val VId_GT = newVId ">"
val VId_LE = newVId "<="
val VId_GE = newVId ">="

(* Int *)
val VId_Int_TILDE = newVId "_Prim.Int.~"
val VId_Int_abs = newVId "_Prim.Int.abs"

(* Word *)
val VId_Word_TILDE = newVId "_Prim.Word.~"

(* Real *)
val VId_Real_abs = newVId "_Prim.Real.abs"

(* Vector *)
val VId_Vector_tabulate = newVId "_Prim.Vector.tabulate"
val VId_Vector_concat = newVId "_Prim.Vector.concat"

(* Array *)
val VId_Array_array = newVId "_Prim.Array.array"
val VId_Array_fromList = newVId "_Prim.Array.fromList"
val VId_Array_tabulate = newVId "_Prim.Array.tabulate"

(* Lua interface *)
val VId_Lua_global = newVId "_Prim.Lua.global"
val VId_Lua_call = newVId "_Prim.Lua.call"
val VId_Lua_method = newVId "_Prim.Lua.method"
val VId_Lua_NIL = newVId "_Prim.Lua.NIL"
val VId_Lua_newTable = newVId "_Prim.Lua.newTable"
val VId_Lua_function = newVId "_Prim.Lua.function"
val VId_Lua_Lib_assert = newVId "_Prim.Lua.Lib.assert"
val VId_Lua_Lib_error = newVId "_Prim.Lua.Lib.error"
val VId_Lua_Lib_getmetatable = newVId "_Prim.Lua.Lib.getmetatable"
val VId_Lua_Lib_pairs = newVId "_Prim.Lua.Lib.pairs"
val VId_Lua_Lib_pcall = newVId "_Prim.Lua.Lib.pcall"
val VId_Lua_Lib_setmetatable = newVId "_Prim.Lua.Lib.setmetatable"
val VId_Lua_Lib_math = newVId "_Prim.Lua.Lib.math"
val VId_Lua_Lib_math_abs = newVId "_Prim.Lua.Lib.math.abs"
val VId_Lua_Lib_math_type = newVId "_Prim.Lua.Lib.math.type'"
val VId_Lua_Lib_math_maxinteger = newVId "_Prim.Lua.Lib.math.maxinteger"
val VId_Lua_Lib_math_mininteger = newVId "_Prim.Lua.Lib.math.mininteger"
val VId_Lua_Lib_string = newVId "_Prim.Lua.Lib.string"
val VId_Lua_Lib_string_format = newVId "_Prim.Lua.Lib.string.format"
val VId_Lua_Lib_table = newVId "_Prim.Lua.Lib.table"
val VId_Lua_Lib_table_pack = newVId "_Prim.Lua.Lib.table.pack"
val VId_Lua_Lib_table_unpack = newVId "_Prim.Lua.Lib.table.unpack"
val VId_Lua_LuaError = newVId "_Prim.Lua.LuaError"
val VId_Lua_LuaError_tag = newVId "_Prim.Lua.LuaError.tag"

(* JavaScript interface *)
val VId_JavaScript_undefined = newVId "_Prim.JavaScript.undefined"
val VId_JavaScript_null = newVId "_Prim.JavaScript.null"
val VId_JavaScript_call = newVId "_Prim.JavaScript.call"
val VId_JavaScript_new = newVId "_Prim.JavaScript.new"
val VId_JavaScript_method = newVId "_Prim.JavaScript.method"
val VId_JavaScript_function = newVId "_Prim.JavaScript.function"
val VId_JavaScript_encodeUtf8 = newVId "_Prim.JavaScript.encodeUtf8"
val VId_JavaScript_decodeUtf8 = newVId "_Prim.JavaScript.decodeUtf8"
val VId_JavaScript_require = newVId "_Prim.JavaScript.require" (* Node.js *)

(* Other primitives *)
val VId_assumePure = newVId "_Prim.assumePure"
val VId_assumeDiscardable = newVId "_Prim.assumeDiscardable"
val VId_Vector_fromList = newVId "_Prim.Vector.fromList"
val VId_Int_add_bin = newVId "_Prim.Int.+"
val VId_Int_sub_bin = newVId "_Prim.Int.-"
val VId_Int_mul_bin = newVId "_Prim.Int.*"
val VId_Int_div_bin = newVId "_Prim.Int.div"
val VId_Int_mod_bin = newVId "_Prim.Int.mod"
val VId_Int_quot_bin = newVId "_Prim.Int.quot"
val VId_Int_rem_bin = newVId "_Prim.Int.rem"
val VId_Word_add_bin = newVId "_Prim.Word.+"
val VId_Word_sub_bin = newVId "_Prim.Word.-"
val VId_Word_mul_bin = newVId "_Prim.Word.*"
val VId_Word_div_bin = newVId "_Prim.Word.div"
val VId_Word_mod_bin = newVId "_Prim.Word.mod"
val VId_Word_LT_bin = newVId "_Prim.Word.<"

val VId_String_concat = newVId "_Prim.String.concat"
val VId_String_concatWith = newVId "_Prim.String.concatWith"
val VId_String_implode = newVId "_Prim.String.implode"
val VId_String_translate = newVId "_Prim.String.translate"

val VId_DelimCont_topLevel = newVId "_Prim.DelimCont.topLevel"

val initialEnv : Typing.Env
    = let open Typing
          val mkTyMap = List.foldl Syntax.TyConMap.insert' Syntax.TyConMap.empty
          val mkValMap = List.foldl (fn ((vid, tysc), m) => Syntax.VIdMap.insert(m, Syntax.MkVId vid, (tysc, Syntax.ValueVariable))) Syntax.VIdMap.empty
          fun mkValConMap (cons, rep) = let val allConstructors = List.foldl (fn ((vid, _), set) => Syntax.VIdSet.add (set, Syntax.MkVId vid)) Syntax.VIdSet.empty cons
                                        in List.foldl (fn ((vid, tysc), m) => let val idstatus = Syntax.ValueConstructor { tag = vid, allConstructors = allConstructors, representation = rep }
                                                                              in Syntax.VIdMap.insert (m, Syntax.MkVId vid, (tysc, idstatus))
                                                                              end
                                                      ) Syntax.VIdMap.empty cons
                                        end
          fun mkTopValConMap (cons, rep) = let val allConstructors = List.foldl (fn ((vid, _, _), set) => Syntax.VIdSet.add (set, Syntax.MkVId vid)) Syntax.VIdSet.empty cons
                                           in List.foldl (fn ((vid, conid, tysc), m) => let val idstatus = Syntax.ValueConstructor { tag = vid, allConstructors = allConstructors, representation = rep }
                                                                                        in Syntax.VIdMap.insert (m, Syntax.MkVId vid, (tysc, idstatus, TypedSyntax.MkShortVId conid))
                                                                                        end
                                                         ) Syntax.VIdMap.empty cons
                                           end
          val tyVarA = TypedSyntax.MkTyVar ("'a", 0)
          val tyVarB = TypedSyntax.MkTyVar ("'b", 1)
          val tyVarC = TypedSyntax.MkTyVar ("'c", 2)
          val tyVarD = TypedSyntax.MkTyVar ("'d", 3)
          val TypeFunction = TypedSyntax.TypeFunction
          val TypeScheme = TypedSyntax.TypeScheme
          val emptyValEnv = TypedSyntax.emptyValEnv
          fun mkTyVar tv = TypedSyntax.TyVar (SourcePos.nullSpan, tv)
          val tyA = mkTyVar tyVarA
          val tyB = mkTyVar tyVarB
          val tyC = mkTyVar tyVarC
          val tyD = mkTyVar tyVarD
          infixr -->
          fun mkFnType (a, b) = TypedSyntax.FnType (SourcePos.nullSpan, a, b)
          val op --> = mkFnType
          fun mkPairType (a, b) = TypedSyntax.PairType (SourcePos.nullSpan, a, b)
          fun mkTupleType xs = TypedSyntax.TupleType (SourcePos.nullSpan, xs)
          fun mkTyCon (a, b) = TypedSyntax.TyCon (SourcePos.nullSpan, a, b)
          fun refOf(t) = mkTyCon([t], primTyName_ref)
          fun listOf(t) = mkTyCon([t], primTyName_list)
          fun arrayOf(t) = mkTyCon([t], primTyName_array)
          fun vectorOf(t) = mkTyCon([t], primTyName_vector)
          fun function2(resultTy, arg1Ty, arg2Ty) = mkTyCon([resultTy, arg1Ty, arg2Ty], primTyName_function2)
          fun function3(resultTy, arg1Ty, arg2Ty, arg3Ty) = mkTyCon([resultTy, arg1Ty, arg2Ty, arg3Ty], primTyName_function3)
          val tyStr_bool = { typeFunction = TypeFunction([], primTy_bool)
                           , valEnv = mkValConMap ([("true", TypeScheme ([], primTy_bool))
                                                   ,("false", TypeScheme ([], primTy_bool))
                                                   ], Syntax.REP_BOOL)
                           }
          val tyStr_int = { typeFunction = TypeFunction([], primTy_int)
                          , valEnv = emptyValEnv
                          }
          val tyStr_word = { typeFunction = TypeFunction([], primTy_word)
                           , valEnv = emptyValEnv
                           }
          val tyStr_real = { typeFunction = TypeFunction([], primTy_real)
                           , valEnv = emptyValEnv
                           }
          val tyStr_char = { typeFunction = TypeFunction([], primTy_char)
                           , valEnv = emptyValEnv
                           }
          val tyStr_wideChar = { typeFunction = TypeFunction([], primTy_wideChar)
                               , valEnv = emptyValEnv
                               }
          val tyStr_string = { typeFunction = TypeFunction([], primTy_string)
                             , valEnv = emptyValEnv
                             }
          val tyStr_wideString = { typeFunction = TypeFunction([], primTy_wideString)
                                 , valEnv = emptyValEnv
                                 }
          val tyStr_intInf = { typeFunction = TypeFunction ([], primTy_intInf)
                             , valEnv = emptyValEnv
                             }
          val tyStr_list = { typeFunction = TypeFunction([tyVarA], listOf tyA)
                           , valEnv = mkValConMap ([("nil", TypeScheme ([(tyVarA, [])], listOf tyA))
                                                   ,("::", TypeScheme ([(tyVarA, [])], mkPairType (tyA, listOf tyA) --> listOf tyA))
                                                   ], Syntax.REP_LIST)
                           }
          val tyStr_ref = { typeFunction = TypeFunction([tyVarA], refOf tyA)
                          , valEnv = mkValConMap ([("ref", TypeScheme ([(tyVarA, [])], tyA --> refOf tyA))
                                                  ], Syntax.REP_REF)
                          }
          val tyStr_exn = { typeFunction = TypeFunction([], primTy_exn)
                          , valEnv = emptyValEnv
                          }
          val tyStr_array = { typeFunction = TypeFunction([tyVarA], arrayOf tyA)
                            , valEnv = emptyValEnv
                            }
          val tyStr_vector = { typeFunction = TypeFunction([tyVarA], vectorOf tyA)
                             , valEnv = emptyValEnv
                             }
          val tyStr_Lua_value = { typeFunction = TypeFunction([], primTy_Lua_value)
                                , valEnv = emptyValEnv
                                }
          val tyStr_JavaScript_value = { typeFunction = TypeFunction([], primTy_JavaScript_value)
                                       , valEnv = emptyValEnv
                                       }
          val tyStr_function2 = { typeFunction = TypeFunction([tyVarA, tyVarB, tyVarC], function2 (tyA, tyB, tyC))
                                , valEnv = emptyValEnv
                                }
          val tyStr_function3 = { typeFunction = TypeFunction([tyVarA, tyVarB, tyVarC, tyVarD], function3 (tyA, tyB, tyC, tyD))
                                , valEnv = emptyValEnv
                                }
          val tyStr_prompt = { typeFunction = TypeFunction ([tyVarA], mkTyCon ([tyA], primTyName_prompt))
                             , valEnv = emptyValEnv
                             }
          val tyStr_subcont = { typeFunction = TypeFunction ([tyVarA, tyVarB], mkTyCon ([tyA, tyB], primTyName_subcont))
                              , valEnv = emptyValEnv
                              }
      in { valMap = List.foldl (Syntax.VIdMap.unionWith #2)
                               Syntax.VIdMap.empty
                               [mkTopValConMap ([("ref", VId_ref, TypeScheme ([(tyVarA, [])], tyA --> refOf tyA)) (* forall 'a. 'a -> 'a ref *)
                                                ], Syntax.REP_REF)
                               ,mkTopValConMap ([("true", VId_true, TypeScheme ([], primTy_bool))
                                                ,("false", VId_false, TypeScheme ([], primTy_bool))
                                                ], Syntax.REP_BOOL)
                               ,mkTopValConMap ([("nil", VId_nil, TypeScheme ([(tyVarA, [])], listOf tyA)) (* forall 'a. 'a list *)
                                                ,("::", VId_DCOLON, TypeScheme ([(tyVarA, [])], mkPairType (tyA, listOf tyA) --> listOf tyA)) (* forall 'a. 'a * 'a list -> 'a list *)
                                                ], Syntax.REP_LIST)
                               ,List.foldl (fn ((name, vid, tysc), m) => Syntax.VIdMap.insert(m, Syntax.MkVId name, (tysc, Syntax.ExceptionConstructor, TypedSyntax.MkShortVId vid)))
                                           Syntax.VIdMap.empty
                                           [("Match", VId_Match, TypeScheme ([], primTy_exn))
                                           ,("Bind", VId_Bind, TypeScheme ([], primTy_exn))
                                           ,("Div", VId_Div, TypeScheme ([], primTy_exn))
                                           ,("Overflow", VId_Overflow, TypeScheme ([], primTy_exn))
                                           ,("Size", VId_Size, TypeScheme ([], primTy_exn))
                                           ,("Subscript", VId_Subscript, TypeScheme ([], primTy_exn))
                                           ,("Fail", VId_Fail, TypeScheme ([], primTy_string --> primTy_exn))
                                           ,("_Prim.Lua.LuaError", VId_Lua_LuaError, TypeScheme ([], primTy_Lua_value --> primTy_exn))
                                           ]
                               ,List.foldl (fn ((name, vid, tysc), m) => Syntax.VIdMap.insert(m, Syntax.MkVId name, (tysc, Syntax.ValueVariable, TypedSyntax.MkShortVId vid)))
                                           Syntax.VIdMap.empty
                                           [("exnName", VId_exnName, TypeScheme ([], primTy_exn --> primTy_string))
                                           ,("_Prim.Vector.fromList", VId_Vector_fromList, TypeScheme ([(tyVarA, [])], listOf tyA --> vectorOf tyA))
                                           ,("_Prim.Int.~", VId_Int_TILDE, TypeScheme ([], primTy_int --> primTy_int))
                                           ,("_Prim.Int.abs", VId_Int_abs, TypeScheme ([], primTy_int --> primTy_int))
                                           ,("_Prim.Int.+", VId_Int_add_bin, TypeScheme ([], function2 (primTy_int, primTy_int, primTy_int)))
                                           ,("_Prim.Int.-", VId_Int_sub_bin, TypeScheme ([], function2 (primTy_int, primTy_int, primTy_int)))
                                           ,("_Prim.Int.*", VId_Int_mul_bin, TypeScheme ([], function2 (primTy_int, primTy_int, primTy_int)))
                                           ,("_Prim.Int.div", VId_Int_div_bin, TypeScheme ([], function2 (primTy_int, primTy_int, primTy_int)))
                                           ,("_Prim.Int.mod", VId_Int_mod_bin, TypeScheme ([], function2 (primTy_int, primTy_int, primTy_int)))
                                           ,("_Prim.Int.quot", VId_Int_quot_bin, TypeScheme ([], function2 (primTy_int, primTy_int, primTy_int)))
                                           ,("_Prim.Int.rem", VId_Int_rem_bin, TypeScheme ([], function2 (primTy_int, primTy_int, primTy_int)))
                                           ,("_Prim.Word.+", VId_Word_add_bin, TypeScheme ([], function2 (primTy_word, primTy_word, primTy_word)))
                                           ,("_Prim.Word.-", VId_Word_sub_bin, TypeScheme ([], function2 (primTy_word, primTy_word, primTy_word)))
                                           ,("_Prim.Word.*", VId_Word_mul_bin, TypeScheme ([], function2 (primTy_word, primTy_word, primTy_word)))
                                           ,("_Prim.Word.div", VId_Word_div_bin, TypeScheme ([], function2 (primTy_word, primTy_word, primTy_word)))
                                           ,("_Prim.Word.mod", VId_Word_mod_bin, TypeScheme ([], function2 (primTy_word, primTy_word, primTy_word)))
                                           ,("_Prim.Word.~", VId_Word_TILDE, TypeScheme ([], primTy_word --> primTy_word))
                                           ,("_Prim.Word.<", VId_Word_LT_bin, TypeScheme ([], function2 (primTy_bool, primTy_word, primTy_word)))
                                           ,("_Prim.Real.abs", VId_Real_abs, TypeScheme ([], primTy_real --> primTy_real))
                                           ,("_Prim.String.concat", VId_String_concat, TypeScheme ([], listOf primTy_string --> primTy_string))
                                           ,("_Prim.String.concatWith", VId_String_concatWith, TypeScheme ([], function2 (primTy_string, primTy_string, listOf primTy_string)))
                                           ,("_Prim.String.implode", VId_String_implode, TypeScheme ([], listOf primTy_char --> primTy_string))
                                           ,("_Prim.String.translate", VId_String_translate, TypeScheme ([], function2 (primTy_string, primTy_char --> primTy_string, primTy_string)))
                                           ,("_Prim.Vector.tabulate", VId_Vector_tabulate, TypeScheme ([(tyVarA, [])], mkPairType (primTy_int, primTy_int --> tyA) --> vectorOf tyA))
                                           ,("_Prim.Vector.concat", VId_Vector_concat, TypeScheme ([(tyVarA, [])], listOf (vectorOf tyA) --> vectorOf tyA))
                                           ,("_Prim.Array.array", VId_Array_array, TypeScheme ([(tyVarA, [])], mkPairType (primTy_int, tyA) --> arrayOf tyA))
                                           ,("_Prim.Array.fromList", VId_Array_fromList, TypeScheme ([(tyVarA, [])], listOf tyA --> arrayOf tyA))
                                           ,("_Prim.Array.tabulate", VId_Array_tabulate, TypeScheme ([(tyVarA, [])], mkPairType (primTy_int, primTy_int --> tyA) --> arrayOf tyA))
                                           ,("_Prim.Lua.global", VId_Lua_global, TypeScheme ([], primTy_string --> primTy_Lua_value))
                                           ,("_Prim.Lua.call", VId_Lua_call, TypeScheme ([], primTy_Lua_value --> vectorOf primTy_Lua_value --> vectorOf primTy_Lua_value))
                                           ,("_Prim.Lua.method", VId_Lua_method, TypeScheme ([], mkPairType (primTy_Lua_value, primTy_string) --> vectorOf primTy_Lua_value --> vectorOf primTy_Lua_value))
                                           ,("_Prim.Lua.NIL", VId_Lua_NIL, TypeScheme ([], primTy_Lua_value))
                                           ,("_Prim.Lua.newTable", VId_Lua_newTable, TypeScheme ([], primTy_unit --> primTy_Lua_value))
                                           ,("_Prim.Lua.function", VId_Lua_function, TypeScheme ([], (vectorOf primTy_Lua_value --> vectorOf primTy_Lua_value) --> primTy_Lua_value))
                                           ,("_Prim.Lua.Lib.assert", VId_Lua_Lib_assert, TypeScheme ([], primTy_Lua_value))
                                           ,("_Prim.Lua.Lib.error", VId_Lua_Lib_error, TypeScheme ([], primTy_Lua_value))
                                           ,("_Prim.Lua.Lib.getmetatable", VId_Lua_Lib_getmetatable, TypeScheme ([], primTy_Lua_value))
                                           ,("_Prim.Lua.Lib.pairs", VId_Lua_Lib_pairs, TypeScheme ([], primTy_Lua_value))
                                           ,("_Prim.Lua.Lib.pcall", VId_Lua_Lib_pcall, TypeScheme ([], primTy_Lua_value))
                                           ,("_Prim.Lua.Lib.setmetatable", VId_Lua_Lib_setmetatable, TypeScheme ([], primTy_Lua_value))
                                           ,("_Prim.Lua.Lib.math", VId_Lua_Lib_math, TypeScheme ([], primTy_Lua_value))
                                           ,("_Prim.Lua.Lib.string", VId_Lua_Lib_string, TypeScheme ([], primTy_Lua_value))
                                           ,("_Prim.Lua.Lib.table", VId_Lua_Lib_table, TypeScheme ([], primTy_Lua_value))
                                           ,("_Prim.Lua.Lib.math.abs", VId_Lua_Lib_math_abs, TypeScheme ([], primTy_Lua_value))
                                           ,("_Prim.Lua.Lib.math.type'", VId_Lua_Lib_math_type, TypeScheme ([], primTy_Lua_value))
                                           ,("_Prim.Lua.Lib.math.maxinteger", VId_Lua_Lib_math_maxinteger, TypeScheme ([], primTy_Lua_value))
                                           ,("_Prim.Lua.Lib.math.mininteger", VId_Lua_Lib_math_mininteger, TypeScheme ([], primTy_Lua_value))
                                           ,("_Prim.Lua.Lib.string.format", VId_Lua_Lib_string_format, TypeScheme ([], primTy_Lua_value))
                                           ,("_Prim.Lua.Lib.table.pack", VId_Lua_Lib_table_pack, TypeScheme ([], primTy_Lua_value))
                                           ,("_Prim.Lua.Lib.table.unpack", VId_Lua_Lib_table_unpack, TypeScheme ([], primTy_Lua_value))
                                           ,("_Prim.JavaScript.undefined", VId_JavaScript_undefined, TypeScheme ([], primTy_JavaScript_value))
                                           ,("_Prim.JavaScript.null", VId_JavaScript_null, TypeScheme ([], primTy_JavaScript_value))
                                           ,("_Prim.JavaScript.call", VId_JavaScript_call, TypeScheme ([], primTy_JavaScript_value --> vectorOf primTy_JavaScript_value --> primTy_JavaScript_value))
                                           ,("_Prim.JavaScript.new", VId_JavaScript_new, TypeScheme ([], primTy_JavaScript_value --> vectorOf primTy_JavaScript_value --> primTy_JavaScript_value))
                                           ,("_Prim.JavaScript.method", VId_JavaScript_method, TypeScheme ([], mkPairType(primTy_JavaScript_value, primTy_wideString) --> vectorOf primTy_JavaScript_value --> primTy_JavaScript_value))
                                           ,("_Prim.JavaScript.function", VId_JavaScript_function, TypeScheme ([], (vectorOf primTy_JavaScript_value --> primTy_JavaScript_value) --> primTy_JavaScript_value))
                                           ,("_Prim.JavaScript.encodeUtf8", VId_JavaScript_encodeUtf8, TypeScheme ([], primTy_wideString --> primTy_string))
                                           ,("_Prim.JavaScript.decodeUtf8", VId_JavaScript_decodeUtf8, TypeScheme ([], primTy_string --> primTy_wideString))
                                           ,("_Prim.JavaScript.require", VId_JavaScript_require, TypeScheme ([], primTy_JavaScript_value))
                                           ,("_Prim.assumePure", VId_assumePure, TypeScheme ([(tyVarA, [])], tyA --> tyA))
                                           ,("_Prim.assumeDiscardable", VId_assumeDiscardable, TypeScheme ([(tyVarA, [])], tyA --> tyA))
                                           ,("_Prim.DelimCont.topLevel", VId_DelimCont_topLevel, TypeScheme ([], mkTyCon ([primTy_unit], primTyName_prompt)))
                                           ]
                          ]
         , tyConMap = List.foldl (fn ((name, tystr), m) => Syntax.TyConMap.insert(m, Syntax.MkTyCon name, tystr))
                                 Syntax.TyConMap.empty
                                 [("bool", tyStr_bool)
                                 ,("int", tyStr_int)
                                 ,("word", tyStr_word)
                                 ,("real", tyStr_real)
                                 ,("string", tyStr_string)
                                 ,("char", tyStr_char)
                                 ,("list", tyStr_list)
                                 ,("ref", tyStr_ref)
                                 ,("exn", tyStr_exn)
                                 ,("array", tyStr_array)
                                 ,("vector", tyStr_vector)
                                 ,("_Prim.WideChar.char", tyStr_wideChar)
                                 ,("_Prim.WideString.string", tyStr_wideString)
                                 ,("_Prim.IntInf.int", tyStr_intInf)
                                 ,("_Prim.Function2.function2", tyStr_function2)
                                 ,("_Prim.Function3.function3", tyStr_function3)
                                 ,("_Prim.Lua.value", tyStr_Lua_value)
                                 ,("_Prim.JavaScript.value", tyStr_JavaScript_value)
                                 ,("_Prim.DelimCont.prompt", tyStr_prompt)
                                 ,("_Prim.DelimCont.subcont", tyStr_subcont)
                                 ]
         , tyNameMap = List.foldl TypedSyntax.TyNameMap.insert'
                                  TypedSyntax.TyNameMap.empty
                                  [(primTyName_bool, { arity = 0, admitsEquality = false (* true *), overloadClass = NONE })
                                  ,(primTyName_int, { arity = 0, admitsEquality = false (* true *), overloadClass = NONE (* SOME Syntax.CLASS_INT *) })
                                  ,(primTyName_word, { arity = 0, admitsEquality = false (* true *), overloadClass = NONE (* SOME Syntax.CLASS_WORD *) })
                                  ,(primTyName_real, { arity = 0, admitsEquality = false, overloadClass = NONE (* SOME Syntax.CLASS_REAL *) })
                                  ,(primTyName_char, { arity = 0, admitsEquality = false (* true *), overloadClass = NONE (* SOME Syntax.CLASS_CHAR *) })
                                  ,(primTyName_wideChar, { arity = 0, admitsEquality = false (* true *), overloadClass = NONE (* SOME Syntax.CLASS_CHAR *) })
                                  ,(primTyName_string, { arity = 0, admitsEquality = false (* true *), overloadClass = NONE (* SOME Syntax.CLASS_STRING *) })
                                  ,(primTyName_wideString, { arity = 0, admitsEquality = false (* true *), overloadClass = NONE (* SOME Syntax.CLASS_STRING *) })
                                  ,(primTyName_intInf, { arity = 0, admitsEquality = false (* true *), overloadClass = NONE (* SOME Syntax.CLASS_INT *) })
                                  ,(primTyName_list, { arity = 1, admitsEquality = false (* true *), overloadClass = NONE })
                                  ,(primTyName_ref, { arity = 1, admitsEquality = false (* must be handled specially *), overloadClass = NONE })
                                  ,(primTyName_exn, { arity = 0, admitsEquality = false, overloadClass = NONE })
                                  ,(primTyName_array, { arity = 1, admitsEquality = false (* must be handled specially *), overloadClass = NONE })
                                  ,(primTyName_vector, { arity = 1, admitsEquality = false (* true *), overloadClass = NONE })
                                  ,(primTyName_Lua_value, { arity = 0, admitsEquality = false, overloadClass = NONE })
                                  ,(primTyName_JavaScript_value, { arity = 0, admitsEquality = false, overloadClass = NONE })
                                  ,(primTyName_function2, { arity = 3, admitsEquality = false, overloadClass = NONE })
                                  ,(primTyName_function3, { arity = 4, admitsEquality = false, overloadClass = NONE })
                                  ,(primTyName_prompt, { arity = 1, admitsEquality = false, overloadClass = NONE })
                                  ,(primTyName_subcont, { arity = 2, admitsEquality = false, overloadClass = NONE })
                                  ]
         , strMap = Syntax.StrIdMap.empty
         , sigMap = Syntax.SigIdMap.empty
         , funMap = Syntax.FunIdMap.empty
         , boundTyVars = Syntax.TyVarMap.empty
         }
      end

val primOverloadEnv : Typing.Env
    = let open Typing
          val TypeScheme = TypedSyntax.TypeScheme
          fun mkTyVar tv = TypedSyntax.TyVar (SourcePos.nullSpan, tv)
          val tyVarA = TypedSyntax.MkTyVar ("'a", 0)
          val tyVarEqA = TypedSyntax.MkTyVar ("''a", 0)
          val tyA = mkTyVar tyVarA
          val tyEqA = mkTyVar tyVarEqA
          infixr -->
          fun a --> b = TypedSyntax.FnType (SourcePos.nullSpan, a, b)
          fun mkPairType (a, b) = TypedSyntax.PairType (SourcePos.nullSpan, a, b)
      in { valMap = List.foldl (fn ((name, vid, tysc), m) => Syntax.VIdMap.insert (m, Syntax.MkVId name, (tysc, Syntax.ValueVariable, TypedSyntax.MkShortVId vid)))
                               Syntax.VIdMap.empty
                               [("abs", VId_abs,    TypeScheme ([(tyVarA, [TypedSyntax.IsSignedReal])], tyA --> tyA))                               (* forall 'a:realint. 'a -> 'a,        default: int -> int *)
                               ,("~",   VId_TILDE,  TypeScheme ([(tyVarA, [TypedSyntax.IsRing])],       tyA --> tyA))                               (* forall 'a:num.     'a -> 'a,        default: int -> int *)
                               ,("div", VId_div,    TypeScheme ([(tyVarA, [TypedSyntax.IsIntegral])],   mkPairType (tyA, tyA) --> tyA))             (* forall 'a:wordint. 'a * 'a -> 'a,   default: int * int -> int *)
                               ,("mod", VId_mod,    TypeScheme ([(tyVarA, [TypedSyntax.IsIntegral])],   mkPairType (tyA, tyA) --> tyA))             (* forall 'a:wordint. 'a * 'a -> 'a,   default: int * int -> int *)
                               ,("*",   VId_TIMES,  TypeScheme ([(tyVarA, [TypedSyntax.IsRing])],       mkPairType (tyA, tyA) --> tyA))             (* forall 'a:num.     'a * 'a -> 'a,   default: int * int -> int *)
                               ,("/",   VId_DIVIDE, TypeScheme ([(tyVarA, [TypedSyntax.IsField])],      mkPairType (tyA, tyA) --> tyA))             (* forall 'a:Real.    'a * 'a -> 'a,   default: real * real -> real *)
                               ,("+",   VId_PLUS,   TypeScheme ([(tyVarA, [TypedSyntax.IsRing])],       mkPairType (tyA, tyA) --> tyA))             (* forall 'a:num.     'a * 'a -> 'a,   default: int * int -> int *)
                               ,("-",   VId_MINUS,  TypeScheme ([(tyVarA, [TypedSyntax.IsRing])],       mkPairType (tyA, tyA) --> tyA))             (* forall 'a:num.     'a * 'a -> 'a,   default: int * int -> int *)
                               ,("<",   VId_LT,     TypeScheme ([(tyVarA, [TypedSyntax.IsOrdered])],    mkPairType (tyA, tyA) --> primTy_bool))     (* forall 'a:numtxt.  'a * 'a -> bool, default: int * int -> bool *)
                               ,(">",   VId_GT,     TypeScheme ([(tyVarA, [TypedSyntax.IsOrdered])],    mkPairType (tyA, tyA) --> primTy_bool))     (* forall 'a:numtxt.  'a * 'a -> bool, default: int * int -> bool *)
                               ,("<=",  VId_LE,     TypeScheme ([(tyVarA, [TypedSyntax.IsOrdered])],    mkPairType (tyA, tyA) --> primTy_bool))     (* forall 'a:numtxt.  'a * 'a -> bool, default: int * int -> bool *)
                               ,(">=",  VId_GE,     TypeScheme ([(tyVarA, [TypedSyntax.IsOrdered])],    mkPairType (tyA, tyA) --> primTy_bool))     (* forall 'a:numtxt.  'a * 'a -> bool, default: int * int -> bool *)
                               ]
         , tyConMap = Syntax.TyConMap.empty
         , tyNameMap = TypedSyntax.TyNameMap.empty
         , strMap = Syntax.StrIdMap.empty
         , sigMap = Syntax.SigIdMap.empty
         , funMap = Syntax.FunIdMap.empty
         , boundTyVars = Syntax.TyVarMap.empty
         }
      end

val initialTyNameSet = let open Typing
                       in TypedSyntax.TyNameSet.fromList
                              [primTyName_int
                              ,primTyName_word
                              ,primTyName_real
                              ,primTyName_char
                              ,primTyName_wideChar
                              ,primTyName_string
                              ,primTyName_wideString
                              ,primTyName_intInf
                              ,primTyName_exn
                              ,primTyName_bool
                              ,primTyName_ref
                              ,primTyName_list
                              ,primTyName_array
                              ,primTyName_vector
                              ,primTyName_Lua_value
                              ,primTyName_JavaScript_value
                              ,primTyName_function2
                              ,primTyName_function3
                              ,primTyName_prompt
                              ,primTyName_subcont
                              ]
                       end
end
