(*
 * Copyright (c) 2021 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure InitialEnv = struct
(*
Top-level environment:
infix  7  * / div mod
infix  6  + - ^
infixr 5  :: @
infix  4  = <> > >= < <=
infix  3  := o
infix  0  before
*)
val initialFixity = let open Syntax
                        fun InfixL p = Syntax.Infix (Syntax.LeftAssoc p)
                        fun InfixR p = Syntax.Infix (Syntax.RightAssoc p)
                    in List.foldl VIdMap.insert' VIdMap.empty
                                  [(MkVId "*",  InfixL 7)
                                  ,(MkVId "/",  InfixL 7)
                                  ,(MkVId "div",InfixL 7)
                                  ,(MkVId "mod",InfixL 7)
                                  ,(MkVId "+",  InfixL 6)
                                  ,(MkVId "-",  InfixL 6)
                                  ,(MkVId "^",  InfixL 6) (* string concatenation *)
                                  ,(MkVId "::", InfixR 5)
                                  ,(MkVId "@",  InfixR 5) (* list concatenation *)
                                  ,(MkVId "=",  InfixL 4)
                                  ,(MkVId "<>", InfixL 4)
                                  ,(MkVId ">",  InfixL 4)
                                  ,(MkVId ">=", InfixL 4)
                                  ,(MkVId "<",  InfixL 4)
                                  ,(MkVId "<=", InfixL 4)
                                  ,(MkVId ":=", InfixL 3)
                                  ,(MkVId "o",  InfixL 3)
                                  ,(MkVId "before",InfixL 3)
                                  ]
                    end
val initialFixityEnv : Fixity.Env = let fun mkValConMap xs = List.foldl (fn (n, m) => Syntax.VIdMap.insert(m, Syntax.MkVId n, Syntax.ValueConstructor)) Syntax.VIdMap.empty xs
                                        fun mkExConMap xs = List.foldl (fn (n, m) => Syntax.VIdMap.insert(m, Syntax.MkVId n, Syntax.ExceptionConstructor)) Syntax.VIdMap.empty xs
                                        fun mkTyConMap xs = List.foldl (fn ((n, y), m) => Syntax.TyConMap.insert(m, Syntax.MkTyCon n, y)) Syntax.TyConMap.empty xs
                                        fun mkStrMap xs = List.foldl (fn ((n, y), m) => Syntax.StrIdMap.insert(m, Syntax.MkStrId n, Fixity.MkIdStatusMap y)) Syntax.StrIdMap.empty xs
                                        fun mkSubstrMap xs = { valMap = Syntax.VIdMap.empty
                                                             , tyConMap = Syntax.TyConMap.empty
                                                             , strMap = mkStrMap xs
                                                             }
                                    in { fixityMap = initialFixity
                                       , idStatusMap = { valMap = Syntax.VIdMap.unionWith #2 (mkValConMap ["ref", "true", "false", "nil", "::"]
                                                                                             ,mkExConMap ["Match", "Bind", "Div", "Overflow", "Size", "Subscript", "Fail"]
                                                                                             )
                                                       , tyConMap = mkTyConMap [("bool", mkValConMap ["true", "false"])
                                                                               ,("ref", mkValConMap ["ref"])
                                                                               ,("list", mkValConMap ["nil", "::"])
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

val StrId_General = newStrId "General"
val StrId_Bool = newStrId "Bool"
val StrId_Int = newStrId "Int"
val StrId_Word = newStrId "Word"
val StrId_Real = newStrId "Real"
val StrId_String = newStrId "String"
val StrId_Char = newStrId "Char"
val StrId_Vector = newStrId "Vector"
val StrId_Array = newStrId "Array"
val StrId_Lua = newStrId "Lua"
val StrId_LunarML = newStrId "LunarML"

(* Ref *)
val VId_ref = newVId "ref"
val LongVId_ref = USyntax.MkShortVId VId_ref
val VId_COLONEQUAL = newLongVId (StrId_General, []) ":="
val VId_EXCLAM = newLongVId (StrId_General, []) "!"

(* Bool *)
val VId_true = newVId "true"
val VId_false = newVId "false"
val LongVId_true = USyntax.MkShortVId VId_true
val LongVId_false = USyntax.MkShortVId VId_false
val VId_Bool_not = newLongVId (StrId_Bool, []) "not"

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
val VId_Int_PLUS = newVId "+"
val VId_Int_MINUS = newVId "-"
val VId_Int_TIMES = newVId "*"
val VId_Int_div = newVId "div"
val VId_Int_mod = newVId "mod"
val VId_Int_TILDE = newVId "~"
val VId_Int_abs = newVId "abs"
val VId_Int_LT = newVId "<"
val VId_Int_LE = newVId "<="
val VId_Int_GT = newVId ">"
val VId_Int_GE = newVId ">="
end

(* Word *)
local val newVId = newLongVId (StrId_Word, [])
in
val VId_Word_PLUS = newVId "+"
val VId_Word_MINUS = newVId "-"
val VId_Word_TIMES = newVId "*"
val VId_Word_div = newVId "div"
val VId_Word_mod = newVId "mod"
val VId_Word_TILDE = newVId "~"
val VId_Word_LT = newVId "<"
val VId_Word_LE = newVId "<="
val VId_Word_GT = newVId ">"
val VId_Word_GE = newVId ">="
end

(* Real *)
local val newVId = newLongVId (StrId_Real, [])
in
val VId_Real_PLUS = newVId "+"
val VId_Real_MINUS = newVId "-"
val VId_Real_TIMES = newVId "*"
val VId_Real_DIVIDE = newVId "/"
val VId_Real_TILDE = newVId "~"
val VId_Real_abs = newVId "abs"
val VId_Real_LT = newVId "<"
val VId_Real_LE = newVId "<="
val VId_Real_GT = newVId ">"
val VId_Real_GE = newVId ">="
end

(* String *)
local val newVId = newLongVId (StrId_String, [])
in
val VId_String_size = newVId "size"
val VId_String_HAT = newVId "^"
val VId_String_str = newVId "str"
val VId_String_LT = newVId "<"
val VId_String_LE = newVId "<="
val VId_String_GT = newVId ">"
val VId_String_GE = newVId ">="
end

(* Char *)
local val newVId = newLongVId (StrId_Char, [])
in
val VId_Char_LT = newVId "<"
val VId_Char_GT = newVId ">"
val VId_Char_LE = newVId "<="
val VId_Char_GE = newVId ">="
end

(* Vector *)
local val newVId = newLongVId (StrId_Vector, [])
in
val VId_Vector_tabulate = newVId "tabulate"
val VId_Vector_length = newVId "length"
val VId_Vector_sub = newVId "sub"
val VId_Vector_concat = newVId "concat"
end

(* Array *)
local val newVId = newLongVId (StrId_Array, [])
in
val VId_Array_array = newVId "array"
val VId_Array_fromList = newVId "fromList"
val VId_Array_tabulate = newVId "tabulate"
val VId_Array_length = newVId "length"
val VId_Array_sub = newVId "sub"
val VId_Array_update = newVId "update"
end

(* Lua interface *)
local val newVId = newLongVId (StrId_Lua, [])
in
val primTyName_Lua_value = USyntax.MkTyName("Lua.value", 12)
val primTy_Lua_value = USyntax.TyCon(SourcePos.nullSpan, [], primTyName_Lua_value)
val VId_Lua_sub = newVId "sub"
val VId_Lua_set = newVId "set"
val VId_Lua_global = newVId "global"
val VId_Lua_call = newVId "call"
val VId_Lua_method = newVId "method"
val VId_Lua_NIL = newVId "NIL"
val VId_Lua_isNil = newVId "isNil"
val VId_Lua_isFalsy = newVId "isFalsy"
val VId_Lua_unsafeToValue = newVId "unsafeToValue"
val VId_Lua_unsafeFromValue = newVId "unsafeFromValue"
val VId_Lua_newTable = newVId "newTable"
val VId_Lua_function = newVId "function"
val VId_Lua_PLUS = newVId "+"
val VId_Lua_MINUS = newVId "-"
val VId_Lua_TIMES = newVId "*"
val VId_Lua_DIVIDE = newVId "/"
val VId_Lua_INTDIV = newVId "//"
val VId_Lua_MOD = newVId "%"
val VId_Lua_pow = newVId "pow" (* ^ *)
val VId_Lua_unm = newVId "unm" (* unary minus *)
val VId_Lua_andb = newVId "andb" (* & *)
val VId_Lua_orb = newVId "orb" (* | *)
val VId_Lua_xorb = newVId "xorb" (* binary ~ *)
val VId_Lua_notb = newVId "notb" (* unary ~ *)
val VId_Lua_LSHIFT = newVId "<<"
val VId_Lua_RSHIFT = newVId ">>"
val VId_Lua_EQUAL = newVId "=="
val VId_Lua_NOTEQUAL = newVId "~="
val VId_Lua_LT = newVId "<"
val VId_Lua_GT = newVId ">"
val VId_Lua_LE = newVId "<="
val VId_Lua_GE = newVId ">="
val VId_Lua_concat = newVId "concat" (* .. *)
val VId_Lua_length = newVId "length" (* # *)
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

val initialEnv : Typing.Env
    = let open Typing
          val mkTyMap = List.foldl Syntax.TyConMap.insert' Syntax.TyConMap.empty
          val mkValMap = List.foldl (fn ((vid, tysc), m) => Syntax.VIdMap.insert(m, Syntax.MkVId vid, (tysc, Syntax.ValueVariable))) Syntax.VIdMap.empty
          val mkValConMap = List.foldl (fn ((vid, tysc), m) => Syntax.VIdMap.insert(m, Syntax.MkVId vid, (tysc, Syntax.ValueConstructor))) Syntax.VIdMap.empty
          val mkExConMap = List.foldl (fn ((vid, tysc), m) => Syntax.VIdMap.insert(m, Syntax.MkVId vid, (tysc, Syntax.ExceptionConstructor))) Syntax.VIdMap.empty
          val mkStrMap = List.foldl (fn ((name, str), m) => Syntax.StrIdMap.insert(m, Syntax.MkStrId name, USyntax.MkSignature str)) Syntax.StrIdMap.empty
          val tyVarA = USyntax.AnonymousTyVar(0)
          val TypeFunction = USyntax.TypeFunction
          val TypeScheme = USyntax.TypeScheme
          val IsEqType = USyntax.IsEqType
          val IsIntegral = USyntax.IsIntegral
          val IsSignedReal = USyntax.IsSignedReal
          val IsRing = USyntax.IsRing
          val IsField = USyntax.IsField
          val IsSigned = USyntax.IsSigned
          val IsOrdered = USyntax.IsOrdered
          val emptyValEnv = USyntax.emptyValEnv
          fun mkTyVar tv = USyntax.TyVar(SourcePos.nullSpan, tv)
          val tyA = mkTyVar tyVarA
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
          val sig_General = { tyConMap = mkTyMap []
                            , valMap = mkValMap
                                           [(":=", TypeScheme ([(tyVarA, [])], mkPairType(refOf tyA, tyA) --> primTy_unit)) (* forall 'a. 'a ref * 'a -> {} *)
                                           ,("!", TypeScheme ([(tyVarA, [])], refOf tyA --> tyA)) (* forall 'a. 'a ref -> 'a *)
                                           ]
                            , strMap = mkStrMap []
                            }
          val sig_Bool = { tyConMap = mkTyMap []
                         , valMap = mkValMap
                                        [("not", TypeScheme ([], primTy_bool --> primTy_bool))
                                        ]
                         , strMap = mkStrMap []
                         }
          val sig_Int = { tyConMap = mkTyMap []
                        , valMap = mkValMap
                                       [("+", TypeScheme ([], mkPairType(primTy_int, primTy_int) --> primTy_int))
                                       ,("-", TypeScheme ([], mkPairType(primTy_int, primTy_int) --> primTy_int))
                                       ,("*", TypeScheme ([], mkPairType(primTy_int, primTy_int) --> primTy_int))
                                       ,("div", TypeScheme ([], mkPairType(primTy_int, primTy_int) --> primTy_int))
                                       ,("mod", TypeScheme ([], mkPairType(primTy_int, primTy_int) --> primTy_int))
                                       ,("<", TypeScheme ([], mkPairType(primTy_int, primTy_int) --> primTy_bool))
                                       ,("<=", TypeScheme ([], mkPairType(primTy_int, primTy_int) --> primTy_bool))
                                       ,(">", TypeScheme ([], mkPairType(primTy_int, primTy_int) --> primTy_bool))
                                       ,(">=", TypeScheme ([], mkPairType(primTy_int, primTy_int) --> primTy_bool))
                                       ,("~", TypeScheme ([], primTy_int --> primTy_int))
                                       ,("abs", TypeScheme ([], primTy_int --> primTy_int))
                                       ]
                        , strMap = mkStrMap []
                        }
          val sig_Word = { tyConMap = mkTyMap []
                         , valMap = mkValMap
                                        [("+", TypeScheme ([], mkPairType(primTy_word, primTy_word) --> primTy_word))
                                        ,("-", TypeScheme ([], mkPairType(primTy_word, primTy_word) --> primTy_word))
                                        ,("*", TypeScheme ([], mkPairType(primTy_word, primTy_word) --> primTy_word))
                                        ,("div", TypeScheme ([], mkPairType(primTy_word, primTy_word) --> primTy_word))
                                        ,("mod", TypeScheme ([], mkPairType(primTy_word, primTy_word) --> primTy_word))
                                        ,("~", TypeScheme ([], primTy_word --> primTy_word))
                                        ,("<", TypeScheme ([], mkPairType(primTy_word, primTy_word) --> primTy_bool))
                                        ,("<=", TypeScheme ([], mkPairType(primTy_word, primTy_word) --> primTy_bool))
                                        ,(">", TypeScheme ([], mkPairType(primTy_word, primTy_word) --> primTy_bool))
                                        ,(">=", TypeScheme ([], mkPairType(primTy_word, primTy_word) --> primTy_bool))
                                        ]
                         , strMap = mkStrMap []
                         }
          val sig_Real = { tyConMap = mkTyMap []
                         , valMap = mkValMap
                                        [("+", TypeScheme ([], mkPairType(primTy_real, primTy_real) --> primTy_real))
                                        ,("-", TypeScheme ([], mkPairType(primTy_real, primTy_real) --> primTy_real))
                                        ,("*", TypeScheme ([], mkPairType(primTy_real, primTy_real) --> primTy_real))
                                        ,("/", TypeScheme ([], mkPairType(primTy_real, primTy_real) --> primTy_real))
                                        ,("<", TypeScheme ([], mkPairType(primTy_real, primTy_real) --> primTy_bool))
                                        ,("<=", TypeScheme ([], mkPairType(primTy_real, primTy_real) --> primTy_bool))
                                        ,(">", TypeScheme ([], mkPairType(primTy_real, primTy_real) --> primTy_bool))
                                        ,(">=", TypeScheme ([], mkPairType(primTy_real, primTy_real) --> primTy_bool))
                                        ,("~", TypeScheme ([], primTy_real --> primTy_real))
                                        ,("abs", TypeScheme ([], primTy_real --> primTy_real))
                                        ]
                         , strMap = mkStrMap []
                         }
          val sig_String = { tyConMap = mkTyMap []
                           , valMap = mkValMap
                                          [("size", TypeScheme ([], primTy_string --> primTy_int))
                                          ,("^", TypeScheme ([], mkPairType(primTy_string, primTy_string) --> primTy_string))
                                          ,("str", TypeScheme ([], primTy_char --> primTy_string))
                                          ,("<", TypeScheme ([], mkPairType(primTy_string, primTy_string) --> primTy_bool))
                                          ,("<=", TypeScheme ([], mkPairType(primTy_string, primTy_string) --> primTy_bool))
                                          ,(">", TypeScheme ([], mkPairType(primTy_string, primTy_string) --> primTy_bool))
                                          ,(">=", TypeScheme ([], mkPairType(primTy_string, primTy_string) --> primTy_bool))
                                          ]
                           , strMap = mkStrMap []
                           }
          val sig_Char = { tyConMap = mkTyMap []
                         , valMap = mkValMap
                                        [("<", TypeScheme ([], mkPairType(primTy_char, primTy_char) --> primTy_bool))
                                        ,("<=", TypeScheme ([], mkPairType(primTy_char, primTy_char) --> primTy_bool))
                                        ,(">", TypeScheme ([], mkPairType(primTy_char, primTy_char) --> primTy_bool))
                                        ,(">=", TypeScheme ([], mkPairType(primTy_char, primTy_char) --> primTy_bool))
                                        ]
                         , strMap = mkStrMap []
                         }
          val sig_Array = { tyConMap = mkTyMap []
                          , valMap = mkValMap
                                         [("array", TypeScheme ([(tyVarA, [])], mkPairType(primTy_int, tyA) --> arrayOf tyA))
                                         ,("fromList", TypeScheme ([(tyVarA, [])], listOf tyA --> arrayOf tyA))
                                         ,("tabulate", TypeScheme ([(tyVarA, [])], mkPairType(primTy_int, primTy_int --> tyA) --> arrayOf tyA))
                                         ,("length", TypeScheme ([(tyVarA, [])], arrayOf tyA --> primTy_int))
                                         ,("sub", TypeScheme ([(tyVarA, [])], mkPairType(arrayOf tyA, primTy_int) --> tyA))
                                         ,("update", TypeScheme ([(tyVarA, [])], USyntax.TupleType(SourcePos.nullSpan, [arrayOf tyA, primTy_int, tyA]) --> primTy_unit))
                                         ]
                          , strMap = mkStrMap []
                          }
          val sig_Vector = { tyConMap = mkTyMap []
                           , valMap = mkValMap
                                          [("tabulate", TypeScheme ([(tyVarA, [])], mkPairType(primTy_int, primTy_int --> tyA) --> vectorOf tyA))
                                          ,("length", TypeScheme ([(tyVarA, [])], vectorOf tyA --> primTy_int))
                                          ,("sub", TypeScheme ([(tyVarA, [])], mkPairType(vectorOf tyA, primTy_int) --> tyA))
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
                                       [("sub", TypeScheme ([], mkPairType(primTy_Lua_value, primTy_Lua_value) --> primTy_Lua_value))
                                       ,("set", TypeScheme ([], mkTupleType[primTy_Lua_value, primTy_Lua_value, primTy_Lua_value] --> primTy_unit))
                                       ,("global", TypeScheme ([], primTy_string --> primTy_Lua_value))
                                       ,("call", TypeScheme ([], primTy_Lua_value --> vectorOf primTy_Lua_value --> vectorOf primTy_Lua_value))
                                       ,("method", TypeScheme ([], mkPairType(primTy_Lua_value, primTy_string) --> vectorOf primTy_Lua_value --> vectorOf primTy_Lua_value))
                                       ,("NIL", TypeScheme ([], primTy_Lua_value))
                                       ,("isNil", TypeScheme ([], primTy_Lua_value --> primTy_bool))
                                       ,("isFalsy", TypeScheme ([], primTy_Lua_value --> primTy_bool))
                                       ,("unsafeToValue", TypeScheme ([(tyVarA, [])], tyA --> primTy_Lua_value))
                                       ,("unsafeFromValue", TypeScheme ([(tyVarA, [])], primTy_Lua_value --> tyA))
                                       ,("newTable", TypeScheme ([], primTy_unit --> primTy_Lua_value))
                                       ,("function", TypeScheme ([], (vectorOf primTy_Lua_value --> vectorOf primTy_Lua_value) --> primTy_Lua_value))
                                       ,("+", TypeScheme ([], mkPairType(primTy_Lua_value, primTy_Lua_value) --> primTy_Lua_value))
                                       ,("-", TypeScheme ([], mkPairType(primTy_Lua_value, primTy_Lua_value) --> primTy_Lua_value))
                                       ,("*", TypeScheme ([], mkPairType(primTy_Lua_value, primTy_Lua_value) --> primTy_Lua_value))
                                       ,("/", TypeScheme ([], mkPairType(primTy_Lua_value, primTy_Lua_value) --> primTy_Lua_value))
                                       ,("//", TypeScheme ([], mkPairType(primTy_Lua_value, primTy_Lua_value) --> primTy_Lua_value))
                                       ,("%", TypeScheme ([], mkPairType(primTy_Lua_value, primTy_Lua_value) --> primTy_Lua_value))
                                       ,("pow", TypeScheme ([], mkPairType(primTy_Lua_value, primTy_Lua_value) --> primTy_Lua_value))
                                       ,("unm", TypeScheme ([], primTy_Lua_value --> primTy_Lua_value))
                                       ,("andb", TypeScheme ([], mkPairType(primTy_Lua_value, primTy_Lua_value) --> primTy_Lua_value))
                                       ,("orb", TypeScheme ([], mkPairType(primTy_Lua_value, primTy_Lua_value) --> primTy_Lua_value))
                                       ,("xorb", TypeScheme ([], mkPairType(primTy_Lua_value, primTy_Lua_value) --> primTy_Lua_value))
                                       ,("notb", TypeScheme ([], primTy_Lua_value --> primTy_Lua_value))
                                       ,("<<", TypeScheme ([], mkPairType(primTy_Lua_value, primTy_Lua_value) --> primTy_Lua_value))
                                       ,(">>", TypeScheme ([], mkPairType(primTy_Lua_value, primTy_Lua_value) --> primTy_Lua_value))
                                       ,("==", TypeScheme ([], mkPairType(primTy_Lua_value, primTy_Lua_value) --> primTy_bool))
                                       ,("~=", TypeScheme ([], mkPairType(primTy_Lua_value, primTy_Lua_value) --> primTy_bool))
                                       ,("<", TypeScheme ([], mkPairType(primTy_Lua_value, primTy_Lua_value) --> primTy_bool))
                                       ,("<=", TypeScheme ([], mkPairType(primTy_Lua_value, primTy_Lua_value) --> primTy_bool))
                                       ,(">", TypeScheme ([], mkPairType(primTy_Lua_value, primTy_Lua_value) --> primTy_bool))
                                       ,(">=", TypeScheme ([], mkPairType(primTy_Lua_value, primTy_Lua_value) --> primTy_bool))
                                       ,("concat", TypeScheme ([], mkPairType(primTy_Lua_value, primTy_Lua_value) --> primTy_Lua_value))
                                       ,("length", TypeScheme ([], primTy_Lua_value --> primTy_Lua_value))
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
                               [List.foldl (fn ((name, vid, tysc), m) => Syntax.VIdMap.insert(m, Syntax.MkVId name, (tysc, Syntax.ValueConstructor, vid)))
                                           Syntax.VIdMap.empty
                                           [("ref", LongVId_ref, TypeScheme ([(tyVarA, [])], tyA --> refOf tyA)) (* forall 'a. 'a -> 'a ref *)
                                           ,("true", LongVId_true, TypeScheme ([], primTy_bool))
                                           ,("false", LongVId_false, TypeScheme ([], primTy_bool))
                                           ,("nil", LongVId_nil, TypeScheme ([(tyVarA, [])], listOf tyA)) (* forall 'a. 'a list *)
                                           ,("::", LongVId_DCOLON, TypeScheme ([(tyVarA, [])], mkPairType(tyA, listOf tyA) --> listOf tyA)) (* forall 'a. 'a * 'a list -> 'a list *)
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
                                           [("=", USyntax.MkShortVId VId_EQUAL, TypeScheme ([(tyVarA, [IsEqType SourcePos.nullSpan])], mkPairType(tyA, tyA) --> primTy_bool)) (* forall ''a. ''a * ''a -> bool *)
                                           ,("abs", USyntax.MkShortVId VId_abs, TypeScheme([(tyVarA, [IsSignedReal SourcePos.nullSpan])], tyA --> tyA)) (* realint -> realint, default: int -> int *)
                                           ,("~", USyntax.MkShortVId VId_TILDE, TypeScheme([(tyVarA, [IsRing SourcePos.nullSpan])], tyA --> tyA)) (* num -> num, default: int -> int *)
                                           ,("div", USyntax.MkShortVId VId_div, TypeScheme([(tyVarA, [IsIntegral SourcePos.nullSpan])], mkPairType(tyA, tyA) --> tyA)) (* wordint * wordint -> wordint, default: int * int -> int *)
                                           ,("mod", USyntax.MkShortVId VId_mod, TypeScheme([(tyVarA, [IsIntegral SourcePos.nullSpan])], mkPairType(tyA, tyA) --> tyA)) (* wordint * wordint -> wordint, default: int * int -> int *)
                                           ,("*", USyntax.MkShortVId VId_TIMES, TypeScheme([(tyVarA, [IsRing SourcePos.nullSpan])], mkPairType(tyA, tyA) --> tyA)) (* num * num -> num, default: int * int -> int *)
                                           ,("/", USyntax.MkShortVId VId_DIVIDE, TypeScheme([(tyVarA, [IsField SourcePos.nullSpan])], mkPairType(tyA, tyA) --> tyA)) (* Real * Real -> Real, default: real * real -> real *)
                                           ,("+", USyntax.MkShortVId VId_PLUS, TypeScheme([(tyVarA, [IsRing SourcePos.nullSpan])], mkPairType(tyA, tyA) --> tyA)) (* num * num -> num, default: int * int -> int *)
                                           ,("-", USyntax.MkShortVId VId_MINUS, TypeScheme([(tyVarA, [IsRing SourcePos.nullSpan])], mkPairType(tyA, tyA) --> tyA)) (* num * num -> num, default: int * int -> int *)
                                           ,("<", USyntax.MkShortVId VId_LT, TypeScheme([(tyVarA, [IsOrdered SourcePos.nullSpan])], mkPairType(tyA, tyA) --> primTy_bool)) (* numtxt * numtxt -> bool, default: int * int -> bool *)
                                           ,(">", USyntax.MkShortVId VId_GT, TypeScheme([(tyVarA, [IsOrdered SourcePos.nullSpan])], mkPairType(tyA, tyA) --> primTy_bool)) (* numtxt * numtxt -> bool, default: int * int -> bool *)
                                           ,("<=", USyntax.MkShortVId VId_LE, TypeScheme([(tyVarA, [IsOrdered SourcePos.nullSpan])], mkPairType(tyA, tyA) --> primTy_bool)) (* numtxt * numtxt -> bool, default: int * int -> bool *)
                                           ,(">=", USyntax.MkShortVId VId_GE, TypeScheme([(tyVarA, [IsOrdered SourcePos.nullSpan])], mkPairType(tyA, tyA) --> primTy_bool)) (* numtxt * numtxt -> bool, default: int * int -> bool *)
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
                                 ]
         , tyNameMap = List.foldl USyntax.TyNameMap.insert'
                                  USyntax.TyNameMap.empty
                                  [(primTyName_bool, { valEnv = #valEnv tyStr_bool, admitsEquality = true })
                                  ,(primTyName_int, { valEnv = emptyValEnv, admitsEquality = true })
                                  ,(primTyName_word, { valEnv = emptyValEnv, admitsEquality = true })
                                  ,(primTyName_real, { valEnv = emptyValEnv, admitsEquality = false })
                                  ,(primTyName_string, { valEnv = emptyValEnv, admitsEquality = true })
                                  ,(primTyName_char, { valEnv = emptyValEnv, admitsEquality = true })
                                  ,(primTyName_list, { valEnv = #valEnv tyStr_list, admitsEquality = true })
                                  ,(primTyName_ref, { valEnv = emptyValEnv, admitsEquality = false (* must be handled specially *) })
                                  ,(primTyName_exn, { valEnv = emptyValEnv, admitsEquality = false })
                                  ,(primTyName_array, { valEnv = emptyValEnv, admitsEquality = false (* must be handled specially *) })
                                  ,(primTyName_vector, { valEnv = emptyValEnv, admitsEquality = true })
                                  ,(primTyName_Lua_value, { valEnv = emptyValEnv, admitsEquality = false })
                                  ]
         , strMap = List.foldl (fn ((name, strid, s), m) => Syntax.StrIdMap.insert(m, Syntax.MkStrId name, (s, USyntax.MkLongStrId(strid, []))))
                               Syntax.StrIdMap.empty
                               [("General", StrId_General, sig_General)
                               ,("Bool", StrId_Bool, sig_Bool)
                               ,("Int", StrId_Int, sig_Int)
                               ,("Word", StrId_Word, sig_Word)
                               ,("Real", StrId_Real, sig_Real)
                               ,("String", StrId_String, sig_String)
                               ,("Char", StrId_Char, sig_Char)
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
                              ]
                       end
end
