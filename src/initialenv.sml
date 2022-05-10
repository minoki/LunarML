(*
 * Copyright (c) 2021 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure InitialEnv = struct
val initialFixityEnv : Fixity.Env = let fun mkValConMap xs = List.foldl (fn (n, m) => Syntax.VIdMap.insert (m, Syntax.MkVId n, Syntax.ValueConstructor ())) Syntax.VIdMap.empty xs
                                        fun mkExConMap xs = List.foldl (fn (n, m) => Syntax.VIdMap.insert(m, Syntax.MkVId n, Syntax.ExceptionConstructor)) Syntax.VIdMap.empty xs
                                        fun mkTyConMap xs = List.foldl (fn ((n, y), m) => Syntax.TyConMap.insert(m, Syntax.MkTyCon n, y)) Syntax.TyConMap.empty xs
                                        fun mkStrMap xs = List.foldl (fn ((n, y), m) => Syntax.StrIdMap.insert(m, Syntax.MkStrId n, Fixity.MkIdStatusMap y)) Syntax.StrIdMap.empty xs
                                        fun mkSubstrMap xs = { valMap = Syntax.VIdMap.empty
                                                             , tyConMap = Syntax.TyConMap.empty
                                                             , strMap = mkStrMap xs
                                                             }
                                        val boolConMap = mkValConMap ["true", "false"]
                                        val refConMap = mkValConMap ["ref"]
                                        val listConMap = mkValConMap ["nil", "::"]
                                    in { fixityMap = Syntax.VIdMap.empty
                                       , idStatusMap = { valMap = List.foldl (Syntax.VIdMap.unionWith #2) (mkExConMap ["Match", "Bind", "Div", "Overflow", "Size", "Subscript", "Fail"]) [boolConMap, refConMap, listConMap]
                                                       , tyConMap = mkTyConMap [("bool", boolConMap)
                                                                               ,("ref", refConMap)
                                                                               ,("list", listConMap)
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
                                                                           ,("Lua", { valMap = mkExConMap []
                                                                                    , tyConMap = Syntax.TyConMap.empty
                                                                                    , strMap = mkStrMap
                                                                                                   [("Lib", mkSubstrMap
                                                                                                                [("math", mkSubstrMap [])
                                                                                                                ,("table", mkSubstrMap [])
                                                                                                                ,("string", mkSubstrMap [])
                                                                                                                ]
                                                                                                    )
                                                                                                   ]
                                                                                    }
                                                                            )
                                                                           ,("JavaScript", mkSubstrMap [])
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
                   ; TypedSyntax.MkVId (name, n)
                  end
fun newShortVId name = let val n = !vidCounter
                       in vidCounter := n - 1
                        ; TypedSyntax.MkShortVId (TypedSyntax.MkVId (name, n))
                       end
fun newStrId name = let val n = !vidCounter
                    in vidCounter := n - 1
                     ; TypedSyntax.MkStrId (name, n)
                    end
fun newLongVId (strid0, strids) name = TypedSyntax.MkLongVId (strid0, List.map Syntax.MkStrId strids, Syntax.MkVId name)

val StrId_Int = newStrId "Int"
val StrId_Real = newStrId "Real"
val StrId_String = newStrId "String"
val StrId_Vector = newStrId "Vector"
val StrId_Array = newStrId "Array"
val StrId_Lua = newStrId "Lua"
val StrId_JavaScript = newStrId "JavaScript"
val StrId_LunarML = newStrId "LunarML"

(* Ref *)
val VId_ref = newVId "ref"
val LongVId_ref = TypedSyntax.MkShortVId VId_ref

(* Bool *)
val VId_true = newVId "true"
val VId_false = newVId "false"
val LongVId_true = TypedSyntax.MkShortVId VId_true
val LongVId_false = TypedSyntax.MkShortVId VId_false

(* List *)
val VId_nil = newVId "nil"
val VId_DCOLON = newVId "::"
val LongVId_nil = TypedSyntax.MkShortVId VId_nil
val LongVId_DCOLON = TypedSyntax.MkShortVId VId_DCOLON

(* Exception *)
val VId_Match = newVId "Match"
val VId_Bind = Typing.VId_Bind (* TypedSyntax.MkVId ("Bind", ~1) *)
val VId_Div = newVId "Div"
val VId_Overflow = newVId "Overflow"
val VId_Size = newVId "Size"
val VId_Subscript = newVId "Subscript"
val VId_Fail = newVId "Fail"
val LongVId_Match = TypedSyntax.MkShortVId VId_Match
val LongVId_Bind = Typing.LongVId_Bind
val LongVId_Div = TypedSyntax.MkShortVId VId_Div
val LongVId_Overflow = TypedSyntax.MkShortVId VId_Overflow
val LongVId_Size = TypedSyntax.MkShortVId VId_Size
val LongVId_Subscript = TypedSyntax.MkShortVId VId_Subscript
val LongVId_Fail = TypedSyntax.MkShortVId VId_Fail
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
val VId_Lua_LuaError = newVId "Lua.LuaError"
val LongVId_Lua_LuaError = TypedSyntax.MkShortVId VId_Lua_LuaError
val VId_Lua_LuaError_tag = newShortVId "Lua.LuaError.tag"

(* JavaScript interface *)
local val newVId = newLongVId (StrId_JavaScript, [])
in
val VId_JavaScript_call = newVId "call"
val VId_JavaScript_new = newVId "new"
val VId_JavaScript_method = newVId "method"
val VId_JavaScript_encodeUtf8 = newVId "encodeUtf8"
val VId_JavaScript_decodeUtf8 = newVId "decodeUtf8"
val VId_JavaScript_require = newVId "require" (* Node.js *)
end

(* Other primitives *)
local val newVId = newLongVId (StrId_LunarML, [])
in
val VId_assumePure = newVId "assumePure"
val VId_assumeDiscardable = newVId "assumeDiscardable"
end
val VId_Vector_fromList = newVId "Vector.fromList"
val LongVId_Vector_fromList = TypedSyntax.MkShortVId VId_Vector_fromList
val VId_Int_add_bin = newVId "Int.+"
val VId_Int_sub_bin = newVId "Int.-"
val VId_Int_mul_bin = newVId "Int.*"
val VId_Int_div_bin = newVId "Int.div"
val VId_Int_mod_bin = newVId "Int.mod"
val VId_Int_quot_bin = newVId "Int.quot"
val VId_Int_rem_bin = newVId "Int.rem"
val VId_Word_div_bin = newVId "Word.div"
val VId_Word_mod_bin = newVId "Word.mod"
val VId_Word_LT_bin = newVId "Word.<"

val VId_String_concat = newVId "String.concat"
val VId_String_concatWith = newVId "String.concatWith"
val VId_String_implode = newVId "String.implode"
val VId_String_translate = newVId "String.translate"

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
                                           in List.foldl (fn ((vid, longvid, tysc), m) => let val idstatus = Syntax.ValueConstructor { tag = vid, allConstructors = allConstructors, representation = rep }
                                                                                          in Syntax.VIdMap.insert (m, Syntax.MkVId vid, (tysc, idstatus, longvid))
                                                                                          end
                                                         ) Syntax.VIdMap.empty cons
                                           end
          val mkExConMap = List.foldl (fn ((vid, tysc), m) => Syntax.VIdMap.insert(m, Syntax.MkVId vid, (tysc, Syntax.ExceptionConstructor))) Syntax.VIdMap.empty
          val mkStrMap = List.foldl (fn ((name, str), m) => Syntax.StrIdMap.insert (m, Syntax.MkStrId name, TypedSyntax.MkSignature str)) Syntax.StrIdMap.empty
          val tyVarA = TypedSyntax.NamedTyVar ("'a", 0)
          val tyVarB = TypedSyntax.NamedTyVar ("'b", 0)
          val tyVarC = TypedSyntax.NamedTyVar ("'c", 0)
          val tyVarD = TypedSyntax.NamedTyVar ("'d", 0)
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
                                       ,("method", TypeScheme ([], mkPairType (primTy_Lua_value, primTy_string) --> vectorOf primTy_Lua_value --> vectorOf primTy_Lua_value))
                                       ,("NIL", TypeScheme ([], primTy_Lua_value))
                                       ,("newTable", TypeScheme ([], primTy_unit --> primTy_Lua_value))
                                       ,("function", TypeScheme ([], (vectorOf primTy_Lua_value --> vectorOf primTy_Lua_value) --> primTy_Lua_value))
                                       ]
                        , strMap = mkStrMap [("Lib", sig_Lua_Lib)]
                        }
          val sig_JavaScript = { tyConMap = mkTyMap [(Syntax.MkTyCon "value", tyStr_JavaScript_value)]
                               , valMap = mkValMap
                                              [("call", TypeScheme ([], primTy_JavaScript_value --> vectorOf primTy_JavaScript_value --> primTy_JavaScript_value))
                                              ,("new", TypeScheme ([], primTy_JavaScript_value --> vectorOf primTy_JavaScript_value --> primTy_JavaScript_value))
                                              ,("method", TypeScheme ([], mkPairType(primTy_JavaScript_value, primTy_wideString) --> vectorOf primTy_JavaScript_value --> primTy_JavaScript_value))
                                              ,("encodeUtf8", TypeScheme ([], primTy_wideString --> primTy_string))
                                              ,("decodeUtf8", TypeScheme ([], primTy_string --> primTy_wideString))
                                              ,("require", TypeScheme ([], primTy_JavaScript_value))
                                              ]
                               , strMap = mkStrMap []
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
                               [mkTopValConMap ([("ref", LongVId_ref, TypeScheme ([(tyVarA, [])], tyA --> refOf tyA)) (* forall 'a. 'a -> 'a ref *)
                                                ], Syntax.REP_REF)
                               ,mkTopValConMap ([("true", LongVId_true, TypeScheme ([], primTy_bool))
                                                ,("false", LongVId_false, TypeScheme ([], primTy_bool))
                                                ], Syntax.REP_BOOL)
                               ,mkTopValConMap ([("nil", LongVId_nil, TypeScheme ([(tyVarA, [])], listOf tyA)) (* forall 'a. 'a list *)
                                                ,("::", LongVId_DCOLON, TypeScheme ([(tyVarA, [])], mkPairType (tyA, listOf tyA) --> listOf tyA)) (* forall 'a. 'a * 'a list -> 'a list *)
                                                ], Syntax.REP_LIST)
                               ,List.foldl (fn ((name, vid, tysc), m) => Syntax.VIdMap.insert(m, Syntax.MkVId name, (tysc, Syntax.ExceptionConstructor, vid)))
                                           Syntax.VIdMap.empty
                                           [("Match", LongVId_Match, TypeScheme ([], primTy_exn))
                                           ,("Bind", LongVId_Bind, TypeScheme ([], primTy_exn))
                                           ,("Div", LongVId_Div, TypeScheme ([], primTy_exn))
                                           ,("Overflow", LongVId_Overflow, TypeScheme ([], primTy_exn))
                                           ,("Size", LongVId_Size, TypeScheme ([], primTy_exn))
                                           ,("Subscript", LongVId_Subscript, TypeScheme ([], primTy_exn))
                                           ,("Fail", LongVId_Fail, TypeScheme ([], primTy_string --> primTy_exn))
                                           ,("Lua.LuaError", LongVId_Lua_LuaError, TypeScheme ([], primTy_Lua_value --> primTy_exn))
                                           ]
                               ,List.foldl (fn ((name, vid, tysc), m) => Syntax.VIdMap.insert(m, Syntax.MkVId name, (tysc, Syntax.ValueVariable, vid)))
                                           Syntax.VIdMap.empty
                                           [("Vector.fromList", TypedSyntax.MkShortVId VId_Vector_fromList, TypeScheme ([(tyVarA, [])], listOf tyA --> vectorOf tyA))
                                           ,("Int.+", TypedSyntax.MkShortVId VId_Int_add_bin, TypeScheme ([], function2 (primTy_int, primTy_int, primTy_int)))
                                           ,("Int.-", TypedSyntax.MkShortVId VId_Int_sub_bin, TypeScheme ([], function2 (primTy_int, primTy_int, primTy_int)))
                                           ,("Int.*", TypedSyntax.MkShortVId VId_Int_mul_bin, TypeScheme ([], function2 (primTy_int, primTy_int, primTy_int)))
                                           ,("Int.div", TypedSyntax.MkShortVId VId_Int_div_bin, TypeScheme ([], function2 (primTy_int, primTy_int, primTy_int)))
                                           ,("Int.mod", TypedSyntax.MkShortVId VId_Int_mod_bin, TypeScheme ([], function2 (primTy_int, primTy_int, primTy_int)))
                                           ,("Int.quot", TypedSyntax.MkShortVId VId_Int_div_bin, TypeScheme ([], function2 (primTy_int, primTy_int, primTy_int)))
                                           ,("Int.rem", TypedSyntax.MkShortVId VId_Int_mod_bin, TypeScheme ([], function2 (primTy_int, primTy_int, primTy_int)))
                                           ,("Word.div", TypedSyntax.MkShortVId VId_Word_div_bin, TypeScheme ([], function2 (primTy_word, primTy_word, primTy_word)))
                                           ,("Word.mod", TypedSyntax.MkShortVId VId_Word_mod_bin, TypeScheme ([], function2 (primTy_word, primTy_word, primTy_word)))
                                           ,("Word.<", TypedSyntax.MkShortVId VId_Word_LT_bin, TypeScheme ([], function2 (primTy_bool, primTy_word, primTy_word)))
                                           ,("exnName", TypedSyntax.MkShortVId VId_exnName, TypeScheme ([], primTy_exn --> primTy_string))
                                           ,("String.concat", TypedSyntax.MkShortVId VId_String_concat, TypeScheme ([], listOf primTy_string --> primTy_string))
                                           ,("String.concatWith", TypedSyntax.MkShortVId VId_String_concatWith, TypeScheme ([], function2 (primTy_string, primTy_string, listOf primTy_string)))
                                           ,("String.implode", TypedSyntax.MkShortVId VId_String_implode, TypeScheme ([], listOf primTy_char --> primTy_string))
                                           ,("String.translate", TypedSyntax.MkShortVId VId_String_translate, TypeScheme ([], function2 (primTy_string, primTy_char --> primTy_string, primTy_string)))
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
                                 ,("WideChar.char", tyStr_wideChar)
                                 ,("WideString.string", tyStr_wideString)
                                 ,("IntInf.int", tyStr_intInf)
                                 ,("Function2.function2", tyStr_function2)
                                 ,("Function3.function3", tyStr_function3)
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
                                  ]
         , strMap = List.foldl (fn ((name, strid, s), m) => Syntax.StrIdMap.insert (m, Syntax.MkStrId name, (s, TypedSyntax.MkLongStrId (strid, []))))
                               Syntax.StrIdMap.empty
                               [("Int", StrId_Int, sig_Int)
                               ,("Real", StrId_Real, sig_Real)
                               ,("Array", StrId_Array, sig_Array)
                               ,("Vector", StrId_Vector, sig_Vector)
                               ,("Lua", StrId_Lua, sig_Lua)
                               ,("JavaScript", StrId_JavaScript, sig_JavaScript)
                               ,("LunarML", StrId_LunarML, sig_LunarML)
                               ]
         , sigMap = Syntax.SigIdMap.empty
         , funMap = Syntax.FunIdMap.empty
         , boundTyVars = Syntax.TyVarMap.empty
         }
      end

val primOverloadEnv : Typing.Env
    = let open Typing
          val TypeScheme = TypedSyntax.TypeScheme
          fun mkTyVar tv = TypedSyntax.TyVar (SourcePos.nullSpan, tv)
          val tyVarA = TypedSyntax.NamedTyVar ("'a", 0)
          val tyVarEqA = TypedSyntax.NamedTyVar ("''a", 0)
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
                              ]
                       end
end
