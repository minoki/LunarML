(*
 * Copyright (c) 2021 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure InitialEnv = struct
val initialFixityEnv : Fixity.Env = let fun mkValConMap xs = List.foldl (fn ((n, isSoleConstructor), m) => Syntax.VIdMap.insert(m, Syntax.MkVId n, Syntax.ValueConstructor isSoleConstructor)) Syntax.VIdMap.empty xs
                                        fun mkExConMap xs = List.foldl (fn (n, m) => Syntax.VIdMap.insert(m, Syntax.MkVId n, Syntax.ExceptionConstructor)) Syntax.VIdMap.empty xs
                                        fun mkTyConMap xs = List.foldl (fn ((n, y), m) => Syntax.TyConMap.insert(m, Syntax.MkTyCon n, y)) Syntax.TyConMap.empty xs
                                        fun mkStrMap xs = List.foldl (fn ((n, y), m) => Syntax.StrIdMap.insert(m, Syntax.MkStrId n, Fixity.MkIdStatusMap y)) Syntax.StrIdMap.empty xs
                                        fun mkSubstrMap xs = { valMap = Syntax.VIdMap.empty
                                                             , tyConMap = Syntax.TyConMap.empty
                                                             , strMap = mkStrMap xs
                                                             }
                                    in { fixityMap = Syntax.VIdMap.empty
                                       , idStatusMap = { valMap = Syntax.VIdMap.unionWith #2 (mkValConMap [("ref", true), ("true", false), ("false", false), ("nil", false), ("::", false)]
                                                                                             ,mkExConMap ["Match", "Bind", "Div", "Overflow", "Size", "Subscript", "Fail"]
                                                                                             )
                                                       , tyConMap = mkTyConMap [("bool", mkValConMap [("true", false), ("false", false)])
                                                                               ,("ref", mkValConMap [("ref", true)])
                                                                               ,("list", mkValConMap [("nil", false), ("::", false)])
                                                                               ]
                                                       , strMap = mkStrMap [("General", mkSubstrMap [])
                                                                           ,("Bool", mkSubstrMap [])
                                                                           ,("Int", mkSubstrMap [])
                                                                           ,("Word", mkSubstrMap [])
                                                                           ,("Real", mkSubstrMap [])
                                                                           ,("String", mkSubstrMap [])
                                                                           ,("Char", mkSubstrMap [])
                                                                           ,("Array", mkSubstrMap [])
                                                                           ,("Vector", mkSubstrMap [])
                                                                           ,("Lua", mkSubstrMap
                                                                                        [("Lib", mkSubstrMap
                                                                                                     [("math", mkSubstrMap [])
                                                                                                     ,("table", mkSubstrMap [])
                                                                                                     ,("string", mkSubstrMap [])
                                                                                                     ]
                                                                                         )
                                                                                        ]
                                                                            )
                                                                           ,("LunarML", mkSubstrMap [])
                                                                           ]
                                                       }
                                       , sigMap = Syntax.SigIdMap.empty
                                       , funMap = Syntax.FunIdMap.empty
                                       }
                                    end

val vidCounter = ref ~3
fun newVId name = let val n = !vidCounter
                  in vidCounter := n - 1
                   ; USyntax.MkVId(name, n)
                  end
fun newShortVId name = let val n = !vidCounter
                       in vidCounter := n - 1
                        ; USyntax.MkShortVId(USyntax.MkVId(name, n))
                       end
fun newStrId name = let val n = !vidCounter
                    in vidCounter := n - 1
                     ; USyntax.MkStrId(name, n)
                    end
fun newLongVId (strid0, strids) name = USyntax.MkLongVId(strid0, List.map Syntax.MkStrId strids, Syntax.MkVId name)

val StrId_Int = newStrId "Int"
val StrId_Real = newStrId "Real"
val StrId_String = newStrId "String"
val StrId_Vector = newStrId "Vector"
val StrId_Array = newStrId "Array"
val StrId_Lua = newStrId "Lua"
val StrId_LunarML = newStrId "LunarML"

(* Ref *)
val VId_ref = newVId "ref"
val LongVId_ref = USyntax.MkShortVId VId_ref

(* Bool *)
val VId_true = newVId "true"
val VId_false = newVId "false"
val LongVId_true = USyntax.MkShortVId VId_true
val LongVId_false = USyntax.MkShortVId VId_false

(* List *)
val VId_nil = newVId "nil"
val VId_DCOLON = newVId "::"
val LongVId_nil = USyntax.MkShortVId VId_nil
val LongVId_DCOLON = USyntax.MkShortVId VId_DCOLON

(* Exception *)
val VId_Match = newVId "Match"
val VId_Bind = Typing.VId_Bind (* USyntax.MkVId("Bind", ~1) *)
val VId_Div = newVId "Div"
val VId_Overflow = newVId "Overflow"
val VId_Size = newVId "Size"
val VId_Subscript = newVId "Subscript"
val VId_Fail = newVId "Fail"
val LongVId_Match = USyntax.MkShortVId VId_Match
val LongVId_Bind = Typing.LongVId_Bind
val LongVId_Div = USyntax.MkShortVId VId_Div
val LongVId_Overflow = USyntax.MkShortVId VId_Overflow
val LongVId_Size = USyntax.MkShortVId VId_Size
val LongVId_Subscript = USyntax.MkShortVId VId_Subscript
val LongVId_Fail = USyntax.MkShortVId VId_Fail
val VId_Match_tag = newShortVId "Match"
val VId_Bind_tag = newShortVId "Bind"
val VId_Div_tag = newShortVId "Div"
val VId_Overflow_tag = newShortVId "Overflow"
val VId_Size_tag = newShortVId "Size"
val VId_Subscript_tag = newShortVId "Subscript"
val VId_Fail_tag = newShortVId "Fail"
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

(* Equality *)
val VId_EQUAL = newVId "="
val VId_EQUAL_bool = newShortVId "=@bool"
val VId_EQUAL_int = newShortVId "=@int"
val VId_EQUAL_word = newShortVId "=@word"
val VId_EQUAL_string = newShortVId "=@string"
val VId_EQUAL_char = newShortVId "=@char"
val VId_EQUAL_list = newShortVId "=@list"
val VId_EQUAL_ref = newShortVId "=@ref"
val VId_EQUAL_array  = newShortVId "=@array"
val VId_EQUAL_vector = newShortVId "=@vector"

(* Int *)
local val newVId = newLongVId (StrId_Int, [])
in
val VId_Int_TILDE = newVId "~"
val VId_Int_abs = newVId "abs"
end

(* Real *)
local val newVId = newLongVId (StrId_Real, [])
in
val VId_Real_abs = newVId "abs"
end

(* Vector *)
local val newVId = newLongVId (StrId_Vector, [])
in
val VId_Vector_tabulate = newVId "tabulate"
val VId_Vector_concat = newVId "concat"
end

(* Array *)
local val newVId = newLongVId (StrId_Array, [])
in
val VId_Array_array = newVId "array"
val VId_Array_fromList = newVId "fromList"
val VId_Array_tabulate = newVId "tabulate"
end

(* Lua interface *)
local val newVId = newLongVId (StrId_Lua, [])
in
val VId_Lua_global = newVId "global"
val VId_Lua_call = newVId "call"
val VId_Lua_method = newVId "method"
val VId_Lua_NIL = newVId "NIL"
val VId_Lua_newTable = newVId "newTable"
val VId_Lua_function = newVId "function"
local val newVId = newLongVId (StrId_Lua, ["Lib"])
in
val VId_Lua_Lib_assert = newVId "assert"
val VId_Lua_Lib_error = newVId "error"
val VId_Lua_Lib_getmetatable = newVId "getmetatable"
val VId_Lua_Lib_pairs = newVId "pairs"
val VId_Lua_Lib_pcall = newVId "pcall"
val VId_Lua_Lib_setmetatable = newVId "setmetatable"
val VId_Lua_Lib_math = newVId "math"
local val newVId = newLongVId (StrId_Lua, ["Lib", "math"])
in
val VId_Lua_Lib_math_abs = newVId "abs"
val VId_Lua_Lib_math_type = newVId "type'"
val VId_Lua_Lib_math_maxinteger = newVId "maxinteger"
val VId_Lua_Lib_math_mininteger = newVId "mininteger"
end
val VId_Lua_Lib_string = newVId "string"
local val newVId = newLongVId (StrId_Lua, ["Lib", "string"])
in
val VId_Lua_Lib_string_format = newVId "format"
end
val VId_Lua_Lib_table = newVId "table"
local val newVId = newLongVId (StrId_Lua, ["Lib", "table"])
in
val VId_Lua_Lib_table_pack = newVId "pack"
val VId_Lua_Lib_table_unpack = newVId "unpack"
end
end
end

(* Other primitives *)
local val newVId = newLongVId (StrId_LunarML, [])
in
val VId_assumePure = newVId "assumePure"
val VId_assumeDiscardable = newVId "assumeDiscardable"
end
val VId_Vector_fromList = newVId "Vector.fromList"
val LongVId_Vector_fromList = USyntax.MkShortVId VId_Vector_fromList
val VId_Int_add_bin = newVId "Int.+"
val VId_Int_sub_bin = newVId "Int.-"
val VId_Int_mul_bin = newVId "Int.*"
val VId_Int_div_bin = newVId "Int.div"
val VId_Int_mod_bin = newVId "Int.mod"
val VId_Word_div_bin = newVId "Word.div"
val VId_Word_mod_bin = newVId "Word.mod"
val VId_Word_LT_bin = newVId "Word.<"

val initialEnv : Typing.Env
    = let open Typing
          val mkTyMap = List.foldl Syntax.TyConMap.insert' Syntax.TyConMap.empty
          val mkValMap = List.foldl (fn ((vid, tysc), m) => Syntax.VIdMap.insert(m, Syntax.MkVId vid, (tysc, Syntax.ValueVariable))) Syntax.VIdMap.empty
          val mkValConMap = fn cons => let val isSoleConstructor = case cons of
                                                                       [_] => true
                                                                     | _ => false
                                           val idstatus = Syntax.ValueConstructor isSoleConstructor
                                       in List.foldl (fn ((vid, tysc), m) => Syntax.VIdMap.insert(m, Syntax.MkVId vid, (tysc, idstatus))) Syntax.VIdMap.empty cons
                                       end
          val mkExConMap = List.foldl (fn ((vid, tysc), m) => Syntax.VIdMap.insert(m, Syntax.MkVId vid, (tysc, Syntax.ExceptionConstructor))) Syntax.VIdMap.empty
          val mkStrMap = List.foldl (fn ((name, str), m) => Syntax.StrIdMap.insert(m, Syntax.MkStrId name, USyntax.MkSignature str)) Syntax.StrIdMap.empty
          val tyVarA = USyntax.NamedTyVar ("'a", false, 0)
          val tyVarB = USyntax.NamedTyVar ("'b", false, 0)
          val tyVarC = USyntax.NamedTyVar ("'c", false, 0)
          val tyVarD = USyntax.NamedTyVar ("'d", false, 0)
          val TypeFunction = USyntax.TypeFunction
          val TypeScheme = USyntax.TypeScheme
          val emptyValEnv = USyntax.emptyValEnv
          fun mkTyVar tv = USyntax.TyVar(SourcePos.nullSpan, tv)
          val tyA = mkTyVar tyVarA
          val tyB = mkTyVar tyVarB
          val tyC = mkTyVar tyVarC
          val tyD = mkTyVar tyVarD
          infixr -->
          fun mkFnType(a, b) = USyntax.FnType(SourcePos.nullSpan, a, b)
          val op --> = mkFnType
          fun mkPairType(a, b) = USyntax.PairType(SourcePos.nullSpan, a, b)
          fun mkTupleType(xs) = USyntax.TupleType(SourcePos.nullSpan, xs)
          fun mkTyCon(a, b) = USyntax.TyCon(SourcePos.nullSpan, a, b)
          fun refOf(t) = mkTyCon([t], primTyName_ref)
          fun listOf(t) = mkTyCon([t], primTyName_list)
          fun arrayOf(t) = mkTyCon([t], primTyName_array)
          fun vectorOf(t) = mkTyCon([t], primTyName_vector)
          fun function2(resultTy, arg1Ty, arg2Ty) = mkTyCon([resultTy, arg1Ty, arg2Ty], primTyName_function2)
          fun function3(resultTy, arg1Ty, arg2Ty, arg3Ty) = mkTyCon([resultTy, arg1Ty, arg2Ty, arg3Ty], primTyName_function3)
          val tyStr_bool = { typeFunction = TypeFunction([], primTy_bool)
                           , valEnv = mkValConMap [("true", TypeScheme ([], primTy_bool))
                                                  ,("false", TypeScheme ([], primTy_bool))
                                                  ]
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
          val tyStr_string = { typeFunction = TypeFunction([], primTy_string)
                             , valEnv = emptyValEnv
                             }
          val tyStr_char = { typeFunction = TypeFunction([], primTy_char)
                           , valEnv = emptyValEnv
                           }
          val tyStr_list = { typeFunction = TypeFunction([tyVarA], listOf tyA)
                           , valEnv = mkValConMap [("nil", TypeScheme ([(tyVarA, [])], listOf tyA))
                                                  ,("::", TypeScheme ([(tyVarA, [])], mkPairType(tyA, listOf tyA) --> listOf tyA))
                                                  ]
                           }
          val tyStr_ref = { typeFunction = TypeFunction([tyVarA], refOf tyA)
                          , valEnv = mkValConMap [("ref", TypeScheme ([(tyVarA, [])], tyA --> refOf tyA))
                                                 ]
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
          val tyStr_function2 = { typeFunction = TypeFunction([tyVarA, tyVarB, tyVarC], function2 (tyA, tyB, tyC))
                                , valEnv = emptyValEnv
                                }
          val tyStr_function3 = { typeFunction = TypeFunction([tyVarA, tyVarB, tyVarC, tyVarD], function3 (tyA, tyB, tyC, tyD))
                                , valEnv = emptyValEnv
                                }
          val sig_Int = { tyConMap = mkTyMap []
                        , valMap = mkValMap
                                       [("~", TypeScheme ([], primTy_int --> primTy_int))
                                       ,("abs", TypeScheme ([], primTy_int --> primTy_int))
                                       ]
                        , strMap = mkStrMap []
                        }
          val sig_Real = { tyConMap = mkTyMap []
                         , valMap = mkValMap
                                        [("abs", TypeScheme ([], primTy_real --> primTy_real))
                                        ]
                         , strMap = mkStrMap []
                         }
          val sig_Array = { tyConMap = mkTyMap []
                          , valMap = mkValMap
                                         [("array", TypeScheme ([(tyVarA, [])], mkPairType(primTy_int, tyA) --> arrayOf tyA))
                                         ,("fromList", TypeScheme ([(tyVarA, [])], listOf tyA --> arrayOf tyA))
                                         ,("tabulate", TypeScheme ([(tyVarA, [])], mkPairType(primTy_int, primTy_int --> tyA) --> arrayOf tyA))
                                         ]
                          , strMap = mkStrMap []
                          }
          val sig_Vector = { tyConMap = mkTyMap []
                           , valMap = mkValMap
                                          [("tabulate", TypeScheme ([(tyVarA, [])], mkPairType(primTy_int, primTy_int --> tyA) --> vectorOf tyA))
                                          ,("concat", TypeScheme ([(tyVarA, [])], listOf (vectorOf tyA) --> vectorOf tyA))
                                          ]
                           , strMap = mkStrMap []
                           }
          val sig_Lua_Lib = { tyConMap = mkTyMap []
                            , valMap = mkValMap
                                           [("assert", TypeScheme ([], primTy_Lua_value))
                                           ,("error", TypeScheme ([], primTy_Lua_value))
                                           ,("getmetatable", TypeScheme ([], primTy_Lua_value))
                                           ,("pairs", TypeScheme ([], primTy_Lua_value))
                                           ,("pcall", TypeScheme ([], primTy_Lua_value))
                                           ,("setmetatable", TypeScheme ([], primTy_Lua_value))
                                           ,("math", TypeScheme ([], primTy_Lua_value))
                                           ,("string", TypeScheme ([], primTy_Lua_value))
                                           ,("table", TypeScheme ([], primTy_Lua_value))
                                           ]
                            , strMap = mkStrMap
                                           [("math", { tyConMap = mkTyMap []
                                                     , valMap = mkValMap
                                                                    [("abs", TypeScheme ([], primTy_Lua_value))
                                                                    ,("type'", TypeScheme ([], primTy_Lua_value))
                                                                    ,("maxinteger", TypeScheme ([], primTy_Lua_value))
                                                                    ,("mininteger", TypeScheme ([], primTy_Lua_value))
                                                                    ]
                                                     , strMap = mkStrMap []
                                                     }
                                            )
                                           ,("string", { tyConMap = mkTyMap []
                                                       , valMap = mkValMap
                                                                      [("format", TypeScheme ([], primTy_Lua_value))
                                                                      ]
                                                       , strMap = mkStrMap []
                                                       }
                                            )
                                           ,("table", { tyConMap = mkTyMap []
                                                       , valMap = mkValMap
                                                                      [("pack", TypeScheme ([], primTy_Lua_value))
                                                                      ,("unpack", TypeScheme ([], primTy_Lua_value))
                                                                      ]
                                                       , strMap = mkStrMap []
                                                       }
                                            )
                                           ]
                            }
          val sig_Lua = { tyConMap = mkTyMap [(Syntax.MkTyCon "value", tyStr_Lua_value)]
                        , valMap = mkValMap
                                       [("global", TypeScheme ([], primTy_string --> primTy_Lua_value))
                                       ,("call", TypeScheme ([], primTy_Lua_value --> vectorOf primTy_Lua_value --> vectorOf primTy_Lua_value))
                                       ,("method", TypeScheme ([], mkPairType(primTy_Lua_value, primTy_string) --> vectorOf primTy_Lua_value --> vectorOf primTy_Lua_value))
                                       ,("NIL", TypeScheme ([], primTy_Lua_value))
                                       ,("newTable", TypeScheme ([], primTy_unit --> primTy_Lua_value))
                                       ,("function", TypeScheme ([], (vectorOf primTy_Lua_value --> vectorOf primTy_Lua_value) --> primTy_Lua_value))
                                       ]
                        , strMap = mkStrMap [("Lib", sig_Lua_Lib)]
                        }
          val sig_LunarML = { tyConMap = mkTyMap []
                            , valMap = mkValMap
                                           [("assumePure", TypeScheme ([(tyVarA, [])], tyA --> tyA))
                                           ,("assumeDiscardable", TypeScheme ([(tyVarA, [])], tyA --> tyA))
                                           ]
                            , strMap = mkStrMap []
                            }
      in { valMap = List.foldl (Syntax.VIdMap.unionWith #2)
                               Syntax.VIdMap.empty
                               [List.foldl (fn ((name, isSoleConstructor, vid, tysc), m) => Syntax.VIdMap.insert(m, Syntax.MkVId name, (tysc, Syntax.ValueConstructor isSoleConstructor, vid)))
                                           Syntax.VIdMap.empty
                                           [("ref", true, LongVId_ref, TypeScheme ([(tyVarA, [])], tyA --> refOf tyA)) (* forall 'a. 'a -> 'a ref *)
                                           ,("true", false, LongVId_true, TypeScheme ([], primTy_bool))
                                           ,("false", false, LongVId_false, TypeScheme ([], primTy_bool))
                                           ,("nil", false, LongVId_nil, TypeScheme ([(tyVarA, [])], listOf tyA)) (* forall 'a. 'a list *)
                                           ,("::", false, LongVId_DCOLON, TypeScheme ([(tyVarA, [])], mkPairType(tyA, listOf tyA) --> listOf tyA)) (* forall 'a. 'a * 'a list -> 'a list *)
                                           ]
                               ,List.foldl (fn ((name, vid, tysc), m) => Syntax.VIdMap.insert(m, Syntax.MkVId name, (tysc, Syntax.ExceptionConstructor, vid)))
                                           Syntax.VIdMap.empty
                                           [("Match", LongVId_Match, TypeScheme ([], primTy_exn))
                                           ,("Bind", LongVId_Bind, TypeScheme ([], primTy_exn))
                                           ,("Div", LongVId_Div, TypeScheme ([], primTy_exn))
                                           ,("Overflow", LongVId_Overflow, TypeScheme ([], primTy_exn))
                                           ,("Size", LongVId_Size, TypeScheme ([], primTy_exn))
                                           ,("Subscript", LongVId_Subscript, TypeScheme ([], primTy_exn))
                                           ,("Fail", LongVId_Fail, TypeScheme ([], primTy_string --> primTy_exn))
                                           ]
                               ,List.foldl (fn ((name, vid, tysc), m) => Syntax.VIdMap.insert(m, Syntax.MkVId name, (tysc, Syntax.ValueVariable, vid)))
                                           Syntax.VIdMap.empty
                                           [("Vector.fromList", USyntax.MkShortVId VId_Vector_fromList, TypeScheme([(tyVarA, [])], listOf tyA --> vectorOf tyA))
                                           ,("Int.+", USyntax.MkShortVId VId_Int_add_bin, TypeScheme([], function2 (primTy_int, primTy_int, primTy_int)))
                                           ,("Int.-", USyntax.MkShortVId VId_Int_sub_bin, TypeScheme([], function2 (primTy_int, primTy_int, primTy_int)))
                                           ,("Int.*", USyntax.MkShortVId VId_Int_mul_bin, TypeScheme([], function2 (primTy_int, primTy_int, primTy_int)))
                                           ,("Int.div", USyntax.MkShortVId VId_Int_div_bin, TypeScheme([], function2 (primTy_int, primTy_int, primTy_int)))
                                           ,("Int.mod", USyntax.MkShortVId VId_Int_mod_bin, TypeScheme([], function2 (primTy_int, primTy_int, primTy_int)))
                                           ,("Word.div", USyntax.MkShortVId VId_Word_div_bin, TypeScheme([], function2 (primTy_word, primTy_word, primTy_word)))
                                           ,("Word.mod", USyntax.MkShortVId VId_Word_mod_bin, TypeScheme([], function2 (primTy_word, primTy_word, primTy_word)))
                                           ,("Word.<", USyntax.MkShortVId VId_Word_LT_bin, TypeScheme([], function2 (primTy_bool, primTy_word, primTy_word)))
                                           ,("exnName", USyntax.MkShortVId VId_exnName, TypeScheme ([], primTy_exn --> primTy_string))
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
                                 ,("Function2.function2", tyStr_function2)
                                 ,("Function3.function3", tyStr_function3)
                                 ]
         , tyNameMap = List.foldl USyntax.TyNameMap.insert'
                                  USyntax.TyNameMap.empty
                                  [(primTyName_bool, { arity = 0, admitsEquality = true, overloadClass = NONE })
                                  ,(primTyName_int, { arity = 0, admitsEquality = true, overloadClass = NONE (* SOME Syntax.CLASS_INT *) })
                                  ,(primTyName_word, { arity = 0, admitsEquality = true, overloadClass = NONE (* SOME Syntax.CLASS_WORD *) })
                                  ,(primTyName_real, { arity = 0, admitsEquality = false, overloadClass = NONE (* SOME Syntax.CLASS_REAL *) })
                                  ,(primTyName_string, { arity = 0, admitsEquality = true, overloadClass = NONE (* SOME Syntax.CLASS_STRING *) })
                                  ,(primTyName_char, { arity = 0, admitsEquality = true, overloadClass = NONE (* SOME Syntax.CLASS_CHAR *) })
                                  ,(primTyName_list, { arity = 1, admitsEquality = true, overloadClass = NONE })
                                  ,(primTyName_ref, { arity = 1, admitsEquality = false (* must be handled specially *), overloadClass = NONE })
                                  ,(primTyName_exn, { arity = 0, admitsEquality = false, overloadClass = NONE })
                                  ,(primTyName_array, { arity = 1, admitsEquality = false (* must be handled specially *), overloadClass = NONE })
                                  ,(primTyName_vector, { arity = 1, admitsEquality = true, overloadClass = NONE })
                                  ,(primTyName_Lua_value, { arity = 0, admitsEquality = false, overloadClass = NONE })
                                  ,(primTyName_function2, { arity = 3, admitsEquality = false, overloadClass = NONE })
                                  ,(primTyName_function3, { arity = 4, admitsEquality = false, overloadClass = NONE })
                                  ]
         , strMap = List.foldl (fn ((name, strid, s), m) => Syntax.StrIdMap.insert(m, Syntax.MkStrId name, (s, USyntax.MkLongStrId(strid, []))))
                               Syntax.StrIdMap.empty
                               [("Int", StrId_Int, sig_Int)
                               ,("Real", StrId_Real, sig_Real)
                               ,("Array", StrId_Array, sig_Array)
                               ,("Vector", StrId_Vector, sig_Vector)
                               ,("Lua", StrId_Lua, sig_Lua)
                               ,("LunarML", StrId_LunarML, sig_LunarML)
                               ]
         , sigMap = Syntax.SigIdMap.empty
         , funMap = Syntax.FunIdMap.empty
         , boundTyVars = Syntax.TyVarMap.empty
         }
      end

val primOverloadEnv : Typing.Env
    = let open Typing
          val TypeScheme = USyntax.TypeScheme
          val IsEqType = USyntax.IsEqType
          val IsIntegral = USyntax.IsIntegral
          val IsSignedReal = USyntax.IsSignedReal
          val IsRing = USyntax.IsRing
          val IsField = USyntax.IsField
          val IsSigned = USyntax.IsSigned
          val IsOrdered = USyntax.IsOrdered
          fun mkTyVar tv = USyntax.TyVar (SourcePos.nullSpan, tv)
          val tyVarA = USyntax.NamedTyVar ("'a", false, 0)
          val tyVarEqA = USyntax.NamedTyVar ("''a", true, 0)
          val tyA = mkTyVar tyVarA
          val tyEqA = mkTyVar tyVarEqA
          infixr -->
          fun a --> b = USyntax.FnType (SourcePos.nullSpan, a, b)
          fun mkPairType (a, b) = USyntax.PairType (SourcePos.nullSpan, a, b)
      in { valMap = List.foldl (fn ((name, vid, tysc), m) => Syntax.VIdMap.insert(m, Syntax.MkVId name, (tysc, Syntax.ValueVariable, USyntax.MkShortVId vid)))
                               Syntax.VIdMap.empty
                               [("=",   VId_EQUAL,  TypeScheme ([(tyVarEqA, [IsEqType SourcePos.nullSpan])],   mkPairType (tyEqA, tyEqA) --> primTy_bool)) (* forall ''a.        ''a * ''a -> bool *)
                               ,("abs", VId_abs,    TypeScheme ([(tyVarA, [IsSignedReal SourcePos.nullSpan])], tyA --> tyA))                               (* forall 'a:realint. 'a -> 'a,        default: int -> int *)
                               ,("~",   VId_TILDE,  TypeScheme ([(tyVarA, [IsRing SourcePos.nullSpan])],       tyA --> tyA))                               (* forall 'a:num.     'a -> 'a,        default: int -> int *)
                               ,("div", VId_div,    TypeScheme ([(tyVarA, [IsIntegral SourcePos.nullSpan])],   mkPairType (tyA, tyA) --> tyA))             (* forall 'a:wordint. 'a * 'a -> 'a,   default: int * int -> int *)
                               ,("mod", VId_mod,    TypeScheme ([(tyVarA, [IsIntegral SourcePos.nullSpan])],   mkPairType (tyA, tyA) --> tyA))             (* forall 'a:wordint. 'a * 'a -> 'a,   default: int * int -> int *)
                               ,("*",   VId_TIMES,  TypeScheme ([(tyVarA, [IsRing SourcePos.nullSpan])],       mkPairType (tyA, tyA) --> tyA))             (* forall 'a:num.     'a * 'a -> 'a,   default: int * int -> int *)
                               ,("/",   VId_DIVIDE, TypeScheme ([(tyVarA, [IsField SourcePos.nullSpan])],      mkPairType (tyA, tyA) --> tyA))             (* forall 'a:Real.    'a * 'a -> 'a,   default: real * real -> real *)
                               ,("+",   VId_PLUS,   TypeScheme ([(tyVarA, [IsRing SourcePos.nullSpan])],       mkPairType (tyA, tyA) --> tyA))             (* forall 'a:num.     'a * 'a -> 'a,   default: int * int -> int *)
                               ,("-",   VId_MINUS,  TypeScheme ([(tyVarA, [IsRing SourcePos.nullSpan])],       mkPairType (tyA, tyA) --> tyA))             (* forall 'a:num.     'a * 'a -> 'a,   default: int * int -> int *)
                               ,("<",   VId_LT,     TypeScheme ([(tyVarA, [IsOrdered SourcePos.nullSpan])],    mkPairType (tyA, tyA) --> primTy_bool))     (* forall 'a:numtxt.  'a * 'a -> bool, default: int * int -> bool *)
                               ,(">",   VId_GT,     TypeScheme ([(tyVarA, [IsOrdered SourcePos.nullSpan])],    mkPairType (tyA, tyA) --> primTy_bool))     (* forall 'a:numtxt.  'a * 'a -> bool, default: int * int -> bool *)
                               ,("<=",  VId_LE,     TypeScheme ([(tyVarA, [IsOrdered SourcePos.nullSpan])],    mkPairType (tyA, tyA) --> primTy_bool))     (* forall 'a:numtxt.  'a * 'a -> bool, default: int * int -> bool *)
                               ,(">=",  VId_GE,     TypeScheme ([(tyVarA, [IsOrdered SourcePos.nullSpan])],    mkPairType (tyA, tyA) --> primTy_bool))     (* forall 'a:numtxt.  'a * 'a -> bool, default: int * int -> bool *)
                               ]
         , tyConMap = Syntax.TyConMap.empty
         , tyNameMap = USyntax.TyNameMap.empty
         , strMap = Syntax.StrIdMap.empty
         , sigMap = Syntax.SigIdMap.empty
         , funMap = Syntax.FunIdMap.empty
         , boundTyVars = Syntax.TyVarMap.empty
         }
      end

val initialTyNameSet = let open Typing
                       in USyntax.TyNameSet.fromList
                              [primTyName_int
                              ,primTyName_word
                              ,primTyName_real
                              ,primTyName_string
                              ,primTyName_char
                              ,primTyName_exn
                              ,primTyName_bool
                              ,primTyName_ref
                              ,primTyName_list
                              ,primTyName_array
                              ,primTyName_vector
                              ,primTyName_Lua_value
                              ,primTyName_function2
                              ,primTyName_function3
                              ]
                       end
end
