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

val vidCounter = ref ~3
fun newVId name = let val n = !vidCounter
                  in vidCounter := n - 1
                   ; USyntax.MkVId(name, n)
                  end

(* Ref *)
val VId_ref        = Typing.VId_ref (* USyntax.MkVId("ref", ~2) *)
val VId_COLONEQUAL = newVId "General.:="
val VId_EXCLAM     = newVId "General.!"

(* Bool *)
val VId_true  = newVId "true"
val VId_false = newVId "false"
val VId_Bool_not = newVId "Bool.not"

(* List *)
val VId_nil    = newVId "nil"
val VId_DCOLON = newVId "::"

(* Exception *)
val VId_Match     = newVId "Match"
val VId_Bind      = Typing.VId_Bind (* USyntax.MkVId("Bind", ~1) *)
val VId_Div       = newVId "Div"
val VId_Overflow  = newVId "Overflow"
val VId_Size      = newVId "Size"
val VId_Subscript = newVId "Subscript"
val VId_Fail      = newVId "Fail"
val VId_Match_tag     = newVId "Match"
val VId_Bind_tag      = newVId "Bind"
val VId_Div_tag       = newVId "Div"
val VId_Overflow_tag  = newVId "Overflow"
val VId_Size_tag      = newVId "Size"
val VId_Subscript_tag = newVId "Subscript"
val VId_Fail_tag      = newVId "Fail"

(* Overloaded *)
val VId_abs    = newVId "abs"
val VId_TILDE  = newVId "~"
val VId_div    = newVId "div"
val VId_mod    = newVId "mod"
val VId_TIMES  = newVId "*"
val VId_DIVIDE = newVId "/"
val VId_PLUS   = newVId "+"
val VId_MINUS  = newVId "-"
val VId_LT     = newVId "<"
val VId_GT     = newVId ">"
val VId_LE     = newVId "<="
val VId_GE     = newVId ">="

(* Equality *)
val VId_EQUAL        = newVId "="
val VId_EQUAL_bool   = newVId "=@bool"
val VId_EQUAL_int    = newVId "=@int"
val VId_EQUAL_word   = newVId "=@word"
val VId_EQUAL_string = newVId "=@string"
val VId_EQUAL_char   = newVId "=@char"
val VId_EQUAL_list   = newVId "=@list"
val VId_EQUAL_ref    = newVId "=@ref"
val VId_EQUAL_array  = newVId "=@array"
val VId_EQUAL_vector = newVId "=@vector"
val VId_EQUAL_exntag = newVId "=@exntag"

(* Int *)
val VId_Int_PLUS     = newVId "Int.+"
val VId_Int_MINUS    = newVId "Int.-"
val VId_Int_TIMES    = newVId "Int.*"
val VId_Int_div      = newVId "Int.div"
val VId_Int_mod      = newVId "Int.mod"
val VId_Int_TILDE    = newVId "Int.~"
val VId_Int_abs      = newVId "Int.abs"
val VId_Int_LT       = newVId "Int.<"
val VId_Int_LE       = newVId "Int.<="
val VId_Int_GT       = newVId "Int.>"
val VId_Int_GE       = newVId "Int.>="
val VId_Int_toString = newVId "Int.toString"

(* Word *)
val VId_Word_PLUS  = newVId "Word.+"
val VId_Word_MINUS = newVId "Word.-"
val VId_Word_TIMES = newVId "Word.*"
val VId_Word_div   = newVId "Word.div"
val VId_Word_mod   = newVId "Word.mod"
val VId_Word_TILDE = newVId "Word.~"
val VId_Word_LT    = newVId "Word.<"
val VId_Word_LE    = newVId "Word.<="
val VId_Word_GT    = newVId "Word.>"
val VId_Word_GE    = newVId "Word.>="

(* Real *)
val VId_Real_PLUS   = newVId "Real.+"
val VId_Real_MINUS  = newVId "Real.-"
val VId_Real_TIMES  = newVId "Real.*"
val VId_Real_DIVIDE = newVId "Real./"
val VId_Real_TILDE  = newVId "Real.~"
val VId_Real_abs    = newVId "Real.abs"
val VId_Real_LT     = newVId "Real.<"
val VId_Real_LE     = newVId "Real.<="
val VId_Real_GT     = newVId "Real.>"
val VId_Real_GE     = newVId "Real.>="

(* String *)
val VId_String_size = newVId "String.size"
val VId_String_HAT = newVId "String.^"
val VId_String_str = newVId "String.str"
val VId_String_LT = newVId "String.<"
val VId_String_LE = newVId "String.<="
val VId_String_GT = newVId "String.>"
val VId_String_GE = newVId "String.>="

(* Char *)
val VId_Char_LT = newVId "Char.<"
val VId_Char_GT = newVId "Char.>"
val VId_Char_LE = newVId "Char.<="
val VId_Char_GE = newVId "Char.>="

(* Vector *)
val VId_Vector_fromList = newVId "Vector.fromList"
val VId_Vector_tabulate = newVId "Vector.tabulate"
val VId_Vector_length   = newVId "Vector.length"
val VId_Vector_sub      = newVId "Vector.sub"

(* Array *)
val VId_Array_array    = newVId "Array.array"
val VId_Array_fromList = newVId "Array.fromList"
val VId_Array_tabulate = newVId "Array.tabulate"
val VId_Array_length   = newVId "Array.length"
val VId_Array_sub      = newVId "Array.sub"
val VId_Array_update   = newVId "Array.update"

(* Lua interface *)
val primTyCon_Lua_value = USyntax.MkTyCon("Lua.value", 11)
val primTy_Lua_value = USyntax.TyCon(SourcePos.nullSpan, [], primTyCon_Lua_value)
val VId_Lua_sub = newVId "Lua.sub"
val VId_Lua_set = newVId "Lua.set"
val VId_Lua_global = newVId "Lua.global"
val VId_Lua_call = newVId "Lua.call"
val VId_Lua_method = newVId "Lua.method"
val VId_Lua_NIL = newVId "Lua.NIL"
val VId_Lua_isNil = newVId "Lua.isNil"
val VId_Lua_isFalsy = newVId "Lua.isFalsy"
val VId_Lua_unsafeToValue = newVId "Lua.unsafeToValue"
val VId_Lua_unsafeFromValue = newVId "Lua.unsafeFromValue"
val VId_Lua_newTable = newVId "Lua.newTable"
val VId_Lua_function = newVId "Lua.function"
val VId_Lua_PLUS = newVId "Lua.+"
val VId_Lua_MINUS = newVId "Lua.-"
val VId_Lua_TIMES = newVId "Lua.*"
val VId_Lua_DIVIDE = newVId "Lua./"
val VId_Lua_INTDIV = newVId "Lua.//"
val VId_Lua_MOD = newVId "Lua.%"
val VId_Lua_pow = newVId "Lua.pow" (* ^ *)
val VId_Lua_unm = newVId "Lua.unm" (* unary minus *)
val VId_Lua_andb = newVId "Lua.andb" (* & *)
val VId_Lua_orb = newVId "Lua.orb" (* | *)
val VId_Lua_xorb = newVId "Lua.xorb" (* binary ~ *)
val VId_Lua_notb = newVId "Lua.notb" (* unary ~ *)
val VId_Lua_LSHIFT = newVId "Lua.<<"
val VId_Lua_RSHIFT = newVId "Lua.>>"
val VId_Lua_EQUAL = newVId "Lua.=="
val VId_Lua_NOTEQUAL = newVId "Lua.~="
val VId_Lua_LT = newVId "Lua.<"
val VId_Lua_GT = newVId "Lua.>"
val VId_Lua_LE = newVId "Lua.<="
val VId_Lua_GE = newVId "Lua.>="
val VId_Lua_concat = newVId "Lua.concat" (* .. *)
val VId_Lua_length = newVId "Lua.length" (* # *)

val initialEnv_ToTypedSyntax : ToTypedSyntax.Env
    = let val ValueConstructor = Syntax.ValueConstructor
          val ExceptionConstructor = Syntax.ExceptionConstructor
          val ValueVariable = Syntax.ValueVariable
          val mkTyConMap = List.foldl Syntax.TyConMap.insert' Syntax.TyConMap.empty 
          val mkValMap = List.foldl (fn ((name, vid), m) => Syntax.VIdMap.insert(m, Syntax.MkVId name, (vid, Syntax.ValueVariable))) Syntax.VIdMap.empty
          val mkValConMap = List.foldl (fn ((name, vid), m) => Syntax.VIdMap.insert(m, Syntax.MkVId name, (vid, Syntax.ValueConstructor))) Syntax.VIdMap.empty
          val mkExConMap = List.foldl (fn ((name, vid), m) => Syntax.VIdMap.insert(m, Syntax.MkVId name, (vid, Syntax.ExceptionConstructor))) Syntax.VIdMap.empty
          val mkStrMap = List.foldl (fn ((name, str), m) => Syntax.StrIdMap.insert(m, Syntax.MkStrId name, ToTypedSyntax.MkEnv str)) Syntax.StrIdMap.empty
          val union = List.foldl (Syntax.VIdMap.unionWith (fn (_, _) => raise Fail "InitialEnv: name conflict")) Syntax.VIdMap.empty
          fun OpaqueBTyCon tycon = ToTypedSyntax.BTyCon { tyCon = tycon, valConMap = Syntax.VIdMap.empty }
          val module_Int = { valMap = mkValMap
                                          [(* toLarge, fromLarge, toInt, fromInt, precision, minInt, maxInt *)
                                           ("+", VId_Int_PLUS)
                                          ,("-", VId_Int_MINUS)
                                          ,("*", VId_Int_TIMES)
                                          ,("div", VId_Int_div)
                                          ,("mod", VId_Int_mod)
                                           (* quot, rem, compare *)
                                          ,("~", VId_Int_TILDE)
                                          ,("abs", VId_Int_abs)
                                          ,("<", VId_Int_LT)
                                          ,("<=", VId_Int_LE)
                                          ,(">", VId_Int_GT)
                                          ,(">=", VId_Int_GE)
                                           (* min, max, sign, sameSign, fmt *)
                                          ,("toString", VId_Int_toString)
                                           (* scan, fromString *)
                                          ]
                           , tyConMap = mkTyConMap []
                           , strMap = Syntax.StrIdMap.empty
                           }
          val module_Word = { valMap = mkValMap
                                           [(* andb, orb, xorb, notb, <<. >>, ~>> *)
                                            ("+", VId_Word_PLUS)
                                           ,("-", VId_Word_MINUS)
                                           ,("*", VId_Word_TIMES)
                                           ,("div", VId_Word_div)
                                           ,("mod", VId_Word_mod)
                                           ,("~", VId_Word_TILDE)
                                           ,("<", VId_Word_LT)
                                           ,("<=", VId_Word_LE)
                                           ,(">", VId_Word_GT)
                                           ,(">=", VId_Word_GE)
                                           ]
                            , tyConMap = mkTyConMap []
                            , strMap = Syntax.StrIdMap.empty
                            }
          val module_Real = { valMap = mkValMap
                                           [("+", VId_Real_PLUS)
                                           ,("-", VId_Real_MINUS)
                                           ,("*", VId_Real_TIMES)
                                           ,("/", VId_Real_DIVIDE)
                                           ,("~", VId_Real_TILDE)
                                           ,("abs", VId_Real_abs)
                                           ,("<", VId_Real_LT)
                                           ,("<=", VId_Real_LE)
                                           ,(">", VId_Real_GT)
                                           ,(">=", VId_Real_GE)
                                           (* ,("==", VId_Real_EQUAL) *)
                                           ]
                            , tyConMap = mkTyConMap []
                            , strMap = Syntax.StrIdMap.empty
                            }
          val module_String = { valMap = mkValMap
                                             [(* maxSize *)
                                              ("size", VId_String_size)
                                             ,("^", VId_String_HAT)
                                             ,("str", VId_String_str)
                                             ,("<", VId_String_LT)
                                             ,("<=", VId_String_LE)
                                             ,(">", VId_String_GT)
                                             ,(">=", VId_String_GE)
                                             ]
                              , tyConMap = mkTyConMap []
                              , strMap = Syntax.StrIdMap.empty
                              }
          val module_Char = { valMap = mkValMap
                                           [(* minChar, maxChar, maxOrd, ord, chr, succ, pred, compare *)
                                            ("<", VId_Char_LT)
                                           ,("<=", VId_Char_LE)
                                           ,(">", VId_Char_GT)
                                           ,(">=", VId_Char_GE)
                                           ]
                            , tyConMap = mkTyConMap []
                            , strMap = Syntax.StrIdMap.empty
                            }
          val module_Array = { valMap = mkValMap
                                            [(* maxLen *)
                                             ("array", VId_Array_array)
                                            ,("fromList", VId_Array_fromList)
                                            ,("tabulate", VId_Array_tabulate)
                                            ,("length", VId_Array_length)
                                            ,("sub", VId_Array_sub)
                                            ,("update", VId_Array_update)
                                             (* vector, copy, copyVec, appi, app, modifyi, modify, foldli, foldri, foldl, foldr, findi, find, exists, all, collate *)
                                            ]
                             , tyConMap = mkTyConMap []
                             , strMap = Syntax.StrIdMap.empty
                             }
          val module_Vector = { valMap = mkValMap
                                             [(* maxLen *)
                                              ("fromList", VId_Vector_fromList)
                                             ,("tabulate", VId_Vector_tabulate)
                                             ,("length", VId_Vector_length)
                                             ,("sub", VId_Vector_sub)
                                              (* update, concat, appi, app, mapi, map, foldli, foldri, foldl, foldr, findi, find, exists, all, collate *)
                                             ]
                              , tyConMap = mkTyConMap []
                              , strMap = Syntax.StrIdMap.empty
                              }
          val module_Lua = { valMap = mkValMap
                                          [("sub", VId_Lua_sub)
                                          ,("set", VId_Lua_set)
                                          ,("global", VId_Lua_global)
                                          ,("call", VId_Lua_call)
                                          ,("method", VId_Lua_method)
                                          ,("NIL", VId_Lua_NIL)
                                          ,("isNil", VId_Lua_isNil)
                                          ,("isFalsy", VId_Lua_isFalsy)
                                          ,("unsafeToValue", VId_Lua_unsafeToValue)
                                          ,("unsafeFromValue", VId_Lua_unsafeFromValue)
                                          ,("newTable", VId_Lua_newTable)
                                          ,("function", VId_Lua_function)
                                          ,("+", VId_Lua_PLUS)
                                          ,("-", VId_Lua_MINUS)
                                          ,("*", VId_Lua_TIMES)
                                          ,("/", VId_Lua_DIVIDE)
                                          ,("//", VId_Lua_INTDIV)
                                          ,("%", VId_Lua_MOD)
                                          ,("pow", VId_Lua_pow)
                                          ,("unm", VId_Lua_unm)
                                          ,("andb", VId_Lua_andb)
                                          ,("orb", VId_Lua_orb)
                                          ,("xorb", VId_Lua_xorb)
                                          ,("notb", VId_Lua_notb)
                                          ,("<<", VId_Lua_LSHIFT)
                                          ,(">>", VId_Lua_RSHIFT)
                                          ,("==", VId_Lua_EQUAL)
                                          ,("~=", VId_Lua_NOTEQUAL)
                                          ,("<", VId_Lua_LT)
                                          ,(">", VId_Lua_GT)
                                          ,("<=", VId_Lua_LE)
                                          ,(">=", VId_Lua_GE)
                                          ,("concat", VId_Lua_concat)
                                          ,("length", VId_Lua_length)
                                          ]
                           , tyConMap = mkTyConMap [(Syntax.MkTyCon "value", OpaqueBTyCon primTyCon_Lua_value)]
                           , strMap = Syntax.StrIdMap.empty
                           }
          val module_LunarML = { valMap = mkValMap []
                               , tyConMap = mkTyConMap []
                               , strMap = mkStrMap
                                              [("Int", module_Int)
                                              ,("Word", module_Word)
                                              ,("Real", module_Real)
                                              ,("String", module_String)
                                              ,("Char", module_Char)
                                              ,("Array", module_Array)
                                              ,("Vector", module_Vector)
                                              ,("Lua", module_Lua)
                                              ]
                               }
      in { valMap = union [mkValConMap [("ref", VId_ref)
                                       ,("true", VId_true)
                                       ,("false", VId_false)
                                       ,("nil", VId_nil)
                                       ,("::", VId_DCOLON)
                                       ]
                          ,mkExConMap [("Match", VId_Match)
                                      ,("Bind", VId_Bind)
                                      ,("Div", VId_Div)
                                      ,("Overflow", VId_Overflow)
                                      ,("Size", VId_Size)
                                      ,("Subscript", VId_Subscript)
                                      ,("Fail", VId_Fail)
                                      ]
                          ,mkValMap [("=", VId_EQUAL)
                                    ,("!", VId_EXCLAM)
                                    ,(":=", VId_COLONEQUAL)
                                    ,("not", VId_Bool_not)
                                     (* Overloaded identifiers *)
                                    ,("abs", VId_abs)
                                    ,("~", VId_TILDE)
                                    ,("div", VId_div)
                                    ,("mod", VId_mod)
                                    ,("*", VId_TIMES)
                                    ,("/", VId_DIVIDE)
                                    ,("+", VId_PLUS)
                                    ,("-", VId_MINUS)
                                    ,("<", VId_LT)
                                    ,(">", VId_GT)
                                    ,("<=", VId_LE)
                                    ,(">=", VId_GE)
                                    ]
                          ]
         , tyConMap = List.foldl Syntax.TyConMap.insert' Syntax.TyConMap.empty
                                 [(Syntax.MkTyCon "unit", ToTypedSyntax.BTyAlias ([], Typing.primTy_unit))
                                 ,(Syntax.MkTyCon "int", OpaqueBTyCon Typing.primTyCon_int)
                                 ,(Syntax.MkTyCon "word", OpaqueBTyCon Typing.primTyCon_word)
                                 ,(Syntax.MkTyCon "real", OpaqueBTyCon Typing.primTyCon_real)
                                 ,(Syntax.MkTyCon "string", OpaqueBTyCon Typing.primTyCon_string)
                                 ,(Syntax.MkTyCon "char", OpaqueBTyCon Typing.primTyCon_char)
                                 ,(Syntax.MkTyCon "exn", OpaqueBTyCon Typing.primTyCon_exn)
                                 ,(Syntax.MkTyCon "bool", ToTypedSyntax.BTyCon { tyCon = Typing.primTyCon_bool
                                                                               , valConMap = mkValConMap [("true", VId_true)
                                                                                                         ,("false", VId_false)
                                                                                                         ]
                                                                               }
                                  )
                                 ,(Syntax.MkTyCon "ref", ToTypedSyntax.BTyCon { tyCon = Typing.primTyCon_ref
                                                                              , valConMap = mkValConMap [("ref", VId_ref)]
                                                                              }
                                  )
                                 ,(Syntax.MkTyCon "list", ToTypedSyntax.BTyCon { tyCon = Typing.primTyCon_list
                                                                               , valConMap = mkValConMap [("nil", VId_nil)
                                                                                                         ,("::", VId_DCOLON)
                                                                                                         ]
                                                                               }
                                  )
                                 ,(Syntax.MkTyCon "array", OpaqueBTyCon Typing.primTyCon_array)
                                 ,(Syntax.MkTyCon "vector", OpaqueBTyCon Typing.primTyCon_vector)
                                 ]
         , strMap = mkStrMap
                        [("LunarML", module_LunarML)
                        ]
         }
      end

val initialEnv : Typing.Env
    = let open Typing
          val mkTyMap = List.foldl USyntax.TyConMap.insert' USyntax.TyConMap.empty
          val mkValMap = List.foldl (fn ((vid, tysc), m) => USyntax.VIdMap.insert(m, vid, (tysc, Syntax.ValueVariable))) USyntax.VIdMap.empty
          val mkValConMap = List.foldl (fn ((vid, tysc), m) => USyntax.VIdMap.insert(m, vid, (tysc, Syntax.ValueConstructor))) USyntax.VIdMap.empty
          val mkExConMap = List.foldl (fn ((vid, tysc), m) => USyntax.VIdMap.insert(m, vid, (tysc, Syntax.ExceptionConstructor))) USyntax.VIdMap.empty
          val mkStrMap = List.foldl (fn ((name, str), m) => Syntax.StrIdMap.insert(m, Syntax.MkStrId name, Typing.MkEnv str)) Syntax.StrIdMap.empty
          val union = List.foldl (USyntax.VIdMap.unionWith (fn (_, _) => raise Fail "InitialEnv: name conflict")) USyntax.VIdMap.empty
          val tyVarA = USyntax.AnonymousTyVar(0)
          val TypeFcn = USyntax.TypeFcn
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
          fun refOf(t) = mkTyCon([t], primTyCon_ref)
          fun listOf(t) = mkTyCon([t], primTyCon_list)
          fun arrayOf(t) = mkTyCon([t], primTyCon_array)
          fun vectorOf(t) = mkTyCon([t], primTyCon_vector)
          val module_Int = { tyMap = mkTyMap []
                           , valMap = mkValMap
                                          [(VId_Int_PLUS, TypeScheme ([], mkPairType(primTy_int, primTy_int) --> primTy_int))
                                          ,(VId_Int_MINUS, TypeScheme ([], mkPairType(primTy_int, primTy_int) --> primTy_int))
                                          ,(VId_Int_TIMES, TypeScheme ([], mkPairType(primTy_int, primTy_int) --> primTy_int))
                                          ,(VId_Int_div, TypeScheme ([], mkPairType(primTy_int, primTy_int) --> primTy_int))
                                          ,(VId_Int_mod, TypeScheme ([], mkPairType(primTy_int, primTy_int) --> primTy_int))
                                          ,(VId_Int_LT, TypeScheme ([], mkPairType(primTy_int, primTy_int) --> primTy_bool))
                                          ,(VId_Int_LE, TypeScheme ([], mkPairType(primTy_int, primTy_int) --> primTy_bool))
                                          ,(VId_Int_GT, TypeScheme ([], mkPairType(primTy_int, primTy_int) --> primTy_bool))
                                          ,(VId_Int_GE, TypeScheme ([], mkPairType(primTy_int, primTy_int) --> primTy_bool))
                                          ,(VId_Int_TILDE, TypeScheme ([], primTy_int --> primTy_int))
                                          ,(VId_Int_abs, TypeScheme ([], primTy_int --> primTy_int))
                                          ,(VId_Int_toString, TypeScheme ([], primTy_int --> primTy_string))
                                          (* ,(VId_Int_fromString, TypeScheme ([], primTy_string --> optionOf primTy_int)) *)
                                          ]
                           , strMap = mkStrMap []
                           , boundTyVars = USyntax.TyVarSet.empty
                           }
          val module_Word = { tyMap = mkTyMap []
                            , valMap = mkValMap
                                          [(VId_Word_PLUS, TypeScheme ([], mkPairType(primTy_word, primTy_word) --> primTy_word))
                                          ,(VId_Word_MINUS, TypeScheme ([], mkPairType(primTy_word, primTy_word) --> primTy_word))
                                          ,(VId_Word_TIMES, TypeScheme ([], mkPairType(primTy_word, primTy_word) --> primTy_word))
                                          ,(VId_Word_div, TypeScheme ([], mkPairType(primTy_word, primTy_word) --> primTy_word))
                                          ,(VId_Word_mod, TypeScheme ([], mkPairType(primTy_word, primTy_word) --> primTy_word))
                                          ,(VId_Word_TILDE, TypeScheme ([], primTy_word --> primTy_word))
                                          ,(VId_Word_LT, TypeScheme ([], mkPairType(primTy_word, primTy_word) --> primTy_bool))
                                          ,(VId_Word_LE, TypeScheme ([], mkPairType(primTy_word, primTy_word) --> primTy_bool))
                                          ,(VId_Word_GT, TypeScheme ([], mkPairType(primTy_word, primTy_word) --> primTy_bool))
                                          ,(VId_Word_GE, TypeScheme ([], mkPairType(primTy_word, primTy_word) --> primTy_bool))
                                          ]
                            , strMap = mkStrMap []
                            , boundTyVars = USyntax.TyVarSet.empty
                            }
          val module_Real = { tyMap = mkTyMap []
                            , valMap = mkValMap
                                          [(VId_Real_PLUS, TypeScheme ([], mkPairType(primTy_real, primTy_real) --> primTy_real))
                                          ,(VId_Real_MINUS, TypeScheme ([], mkPairType(primTy_real, primTy_real) --> primTy_real))
                                          ,(VId_Real_TIMES, TypeScheme ([], mkPairType(primTy_real, primTy_real) --> primTy_real))
                                          ,(VId_Real_DIVIDE, TypeScheme ([], mkPairType(primTy_real, primTy_real) --> primTy_real))
                                          ,(VId_Real_LT, TypeScheme ([], mkPairType(primTy_real, primTy_real) --> primTy_bool))
                                          ,(VId_Real_LE, TypeScheme ([], mkPairType(primTy_real, primTy_real) --> primTy_bool))
                                          ,(VId_Real_GT, TypeScheme ([], mkPairType(primTy_real, primTy_real) --> primTy_bool))
                                          ,(VId_Real_GE, TypeScheme ([], mkPairType(primTy_real, primTy_real) --> primTy_bool))
                                          ,(VId_Real_TILDE, TypeScheme ([], primTy_real --> primTy_real))
                                          ,(VId_Real_abs, TypeScheme ([], primTy_real --> primTy_real))
                                          ]
                            , strMap = mkStrMap []
                            , boundTyVars = USyntax.TyVarSet.empty
                            }
          val module_String = { tyMap = mkTyMap []
                              , valMap = mkValMap
                                             [(* size, sub, extract, substring, ^, concat, concatWith, str, implode, explode, map, translate, tokens, fields, isPrefix, isSubstring, isSuffix, compare, collate *)
                                              (VId_String_size, TypeScheme ([], primTy_string --> primTy_int))
                                             ,(VId_String_HAT, TypeScheme ([], mkPairType(primTy_string, primTy_string) --> primTy_string))
                                             ,(VId_String_str, TypeScheme ([], primTy_char --> primTy_string))
                                             ,(VId_String_LT, TypeScheme ([], mkPairType(primTy_string, primTy_string) --> primTy_bool))
                                             ,(VId_String_LE, TypeScheme ([], mkPairType(primTy_string, primTy_string) --> primTy_bool))
                                             ,(VId_String_GT, TypeScheme ([], mkPairType(primTy_string, primTy_string) --> primTy_bool))
                                             ,(VId_String_GE, TypeScheme ([], mkPairType(primTy_string, primTy_string) --> primTy_bool))
                                              (* toString, scan, fromString, toCString, fromCString *)
                                             ]
                              , strMap = mkStrMap []
                              , boundTyVars = USyntax.TyVarSet.empty
                              }
          val module_Char = { tyMap = mkTyMap []
                            , valMap = mkValMap
                                           [(* size, sub, extract, substring, ^, concat, concatWith, str, implode, explode, map, translate, tokens, fields, isPrefix, isSubstring, isSuffix, compare, collate *)
                                            (VId_Char_LT, TypeScheme ([], mkPairType(primTy_char, primTy_char) --> primTy_bool))
                                           ,(VId_Char_LE, TypeScheme ([], mkPairType(primTy_char, primTy_char) --> primTy_bool))
                                           ,(VId_Char_GT, TypeScheme ([], mkPairType(primTy_char, primTy_char) --> primTy_bool))
                                           ,(VId_Char_GE, TypeScheme ([], mkPairType(primTy_char, primTy_char) --> primTy_bool))
                                           ]
                            , strMap = mkStrMap []
                            , boundTyVars = USyntax.TyVarSet.empty
                            }
          val module_Array = { tyMap = mkTyMap []
                             , valMap = mkValMap
                                            [(VId_Array_array, TypeScheme ([(tyVarA, [])], mkPairType(primTy_int, tyA) --> arrayOf tyA))
                                            ,(VId_Array_fromList, TypeScheme ([(tyVarA, [])], listOf tyA --> arrayOf tyA))
                                            ,(VId_Array_tabulate, TypeScheme ([(tyVarA, [])], mkPairType(primTy_int, primTy_int --> tyA) --> arrayOf tyA))
                                            ,(VId_Array_length, TypeScheme ([(tyVarA, [])], arrayOf tyA --> primTy_int))
                                            ,(VId_Array_sub, TypeScheme ([(tyVarA, [])], mkPairType(arrayOf tyA, primTy_int) --> tyA))
                                            ,(VId_Array_update, TypeScheme ([(tyVarA, [])], USyntax.TupleType(SourcePos.nullSpan, [arrayOf tyA, primTy_int, tyA]) --> primTy_unit))
                                            ]
                             , strMap = mkStrMap []
                             , boundTyVars = USyntax.TyVarSet.empty
                             }
          val module_Vector = { tyMap = mkTyMap []
                              , valMap = mkValMap
                                             [(VId_Vector_fromList, TypeScheme ([(tyVarA, [])], listOf tyA --> vectorOf tyA))
                                             ,(VId_Vector_tabulate, TypeScheme ([(tyVarA, [])], mkPairType(primTy_int, primTy_int --> tyA) --> vectorOf tyA))
                                             ,(VId_Vector_length, TypeScheme ([(tyVarA, [])], vectorOf tyA --> primTy_int))
                                             ,(VId_Vector_sub, TypeScheme ([(tyVarA, [])], mkPairType(vectorOf tyA, primTy_int) --> tyA))
                                             ]
                              , strMap = mkStrMap []
                              , boundTyVars = USyntax.TyVarSet.empty
                              }
          val module_Lua = { tyMap = mkTyMap [(primTyCon_Lua_value, TyStr(TypeFcn([], primTy_Lua_value), emptyValEnv))]
                           , valMap = mkValMap
                                          [(VId_Lua_sub, TypeScheme ([], mkPairType(primTy_Lua_value, primTy_Lua_value) --> primTy_Lua_value))
                                          ,(VId_Lua_set, TypeScheme ([], mkTupleType[primTy_Lua_value, primTy_Lua_value, primTy_Lua_value] --> primTy_unit))
                                          ,(VId_Lua_global, TypeScheme ([], primTy_string --> primTy_Lua_value))
                                          ,(VId_Lua_call, TypeScheme ([], primTy_Lua_value --> vectorOf primTy_Lua_value --> vectorOf primTy_Lua_value))
                                          ,(VId_Lua_method, TypeScheme ([], mkPairType(primTy_Lua_value, primTy_string) --> vectorOf primTy_Lua_value --> vectorOf primTy_Lua_value))
                                          ,(VId_Lua_NIL, TypeScheme ([], primTy_Lua_value))
                                          ,(VId_Lua_isNil, TypeScheme ([], primTy_Lua_value --> primTy_bool))
                                          ,(VId_Lua_isFalsy, TypeScheme ([], primTy_Lua_value --> primTy_bool))
                                          ,(VId_Lua_unsafeToValue, TypeScheme ([(tyVarA, [])], tyA --> primTy_Lua_value))
                                          ,(VId_Lua_unsafeFromValue, TypeScheme ([(tyVarA, [])], primTy_Lua_value --> tyA))
                                          ,(VId_Lua_newTable, TypeScheme ([], primTy_unit --> primTy_Lua_value))
                                          ,(VId_Lua_function, TypeScheme ([], (vectorOf primTy_Lua_value --> vectorOf primTy_Lua_value) --> primTy_Lua_value))
                                          ,(VId_Lua_PLUS, TypeScheme ([], mkPairType(primTy_Lua_value, primTy_Lua_value) --> primTy_Lua_value))
                                          ,(VId_Lua_MINUS, TypeScheme ([], mkPairType(primTy_Lua_value, primTy_Lua_value) --> primTy_Lua_value))
                                          ,(VId_Lua_TIMES, TypeScheme ([], mkPairType(primTy_Lua_value, primTy_Lua_value) --> primTy_Lua_value))
                                          ,(VId_Lua_DIVIDE, TypeScheme ([], mkPairType(primTy_Lua_value, primTy_Lua_value) --> primTy_Lua_value))
                                          ,(VId_Lua_INTDIV, TypeScheme ([], mkPairType(primTy_Lua_value, primTy_Lua_value) --> primTy_Lua_value))
                                          ,(VId_Lua_MOD, TypeScheme ([], mkPairType(primTy_Lua_value, primTy_Lua_value) --> primTy_Lua_value))
                                          ,(VId_Lua_pow, TypeScheme ([], mkPairType(primTy_Lua_value, primTy_Lua_value) --> primTy_Lua_value))
                                          ,(VId_Lua_unm, TypeScheme ([], primTy_Lua_value --> primTy_Lua_value))
                                          ,(VId_Lua_andb, TypeScheme ([], mkPairType(primTy_Lua_value, primTy_Lua_value) --> primTy_Lua_value))
                                          ,(VId_Lua_orb, TypeScheme ([], mkPairType(primTy_Lua_value, primTy_Lua_value) --> primTy_Lua_value))
                                          ,(VId_Lua_xorb, TypeScheme ([], mkPairType(primTy_Lua_value, primTy_Lua_value) --> primTy_Lua_value))
                                          ,(VId_Lua_notb, TypeScheme ([], primTy_Lua_value --> primTy_Lua_value))
                                          ,(VId_Lua_LSHIFT, TypeScheme ([], mkPairType(primTy_Lua_value, primTy_Lua_value) --> primTy_Lua_value))
                                          ,(VId_Lua_RSHIFT, TypeScheme ([], mkPairType(primTy_Lua_value, primTy_Lua_value) --> primTy_Lua_value))
                                          ,(VId_Lua_EQUAL, TypeScheme ([], mkPairType(primTy_Lua_value, primTy_Lua_value) --> primTy_bool))
                                          ,(VId_Lua_NOTEQUAL, TypeScheme ([], mkPairType(primTy_Lua_value, primTy_Lua_value) --> primTy_bool))
                                          ,(VId_Lua_LT, TypeScheme ([], mkPairType(primTy_Lua_value, primTy_Lua_value) --> primTy_bool))
                                          ,(VId_Lua_GT, TypeScheme ([], mkPairType(primTy_Lua_value, primTy_Lua_value) --> primTy_bool))
                                          ,(VId_Lua_LE, TypeScheme ([], mkPairType(primTy_Lua_value, primTy_Lua_value) --> primTy_bool))
                                          ,(VId_Lua_GE, TypeScheme ([], mkPairType(primTy_Lua_value, primTy_Lua_value) --> primTy_bool))
                                          ,(VId_Lua_concat, TypeScheme ([], mkPairType(primTy_Lua_value, primTy_Lua_value) --> primTy_Lua_value))
                                          ,(VId_Lua_length, TypeScheme ([], primTy_Lua_value --> primTy_Lua_value))
                                          ]
                           , strMap = mkStrMap []
                           , boundTyVars = USyntax.TyVarSet.empty
                           }
      in List.foldl Typing.mergeEnv
                    { tyMap = mkTyMap
                                  [(USyntax.MkTyCon("unit", 9), TyStr(TypeFcn([], primTy_unit), emptyValEnv))
                                  ,(USyntax.MkTyCon("bool", 6), TyStr(TypeFcn([], primTy_bool)
                                                                     , mkValConMap [(VId_true, TypeScheme ([], primTy_bool))
                                                                                   ,(VId_false, TypeScheme ([], primTy_bool))
                                                                                   ]
                                                                     )
                                   )
                                  ,(USyntax.MkTyCon("int", 0), TyStr(TypeFcn([], primTy_int), emptyValEnv))
                                  ,(USyntax.MkTyCon("word", 1), TyStr(TypeFcn([], primTy_word), emptyValEnv))
                                  ,(USyntax.MkTyCon("real", 2), TyStr(TypeFcn([], primTy_real), emptyValEnv))
                                  ,(USyntax.MkTyCon("string", 3), TyStr(TypeFcn([], primTy_string), emptyValEnv))
                                  ,(USyntax.MkTyCon("char", 4), TyStr(TypeFcn([], primTy_char), emptyValEnv))
                                  ,(USyntax.MkTyCon("list", 5), TyStr(TypeFcn([tyVarA], mkTyCon([mkTyVar(tyVarA)], primTyCon_list))
                                                                     , mkValConMap [(VId_nil, TypeScheme ([(tyVarA, [])], listOf tyA))
                                                                                   ,(VId_DCOLON, TypeScheme ([(tyVarA, [])], mkPairType(tyA, listOf tyA) --> listOf tyA))
                                                                                   ]
                                                                     )
                                   )
                                  ,(USyntax.MkTyCon("ref", 7), TyStr(TypeFcn([tyVarA], mkTyCon([mkTyVar(tyVarA)], primTyCon_ref))
                                                                    , mkValConMap [(VId_ref, TypeScheme ([(tyVarA, [])], tyA --> refOf tyA))
                                                                                  ]
                                                                    )
                                   )
                                  ,(USyntax.MkTyCon("exn", 5), TyStr(TypeFcn([], primTy_exn), emptyValEnv))
                                  ,(USyntax.MkTyCon("array", 9), TyStr(TypeFcn([tyVarA], arrayOf tyA), emptyValEnv))
                                  ,(USyntax.MkTyCon("vector", 9), TyStr(TypeFcn([tyVarA], vectorOf tyA), emptyValEnv))
                                  ]
                    , valMap = union [mkValConMap [(VId_ref, TypeScheme ([(tyVarA, [])], tyA --> refOf tyA)) (* forall 'a. 'a -> 'a ref *)
                                                  ,(VId_true, TypeScheme ([], primTy_bool))
                                                  ,(VId_false, TypeScheme ([], primTy_bool))
                                                  ,(VId_nil, TypeScheme ([(tyVarA, [])], listOf tyA)) (* forall 'a. 'a list *)
                                                  ,(VId_DCOLON, TypeScheme ([(tyVarA, [])], mkPairType(tyA, listOf tyA) --> listOf tyA)) (* forall 'a. 'a * 'a list -> 'a list *)
                                                  ]
                                     ,mkExConMap [(VId_Match, TypeScheme ([], primTy_exn))
                                                 ,(VId_Bind, TypeScheme ([], primTy_exn))
                                                 ,(VId_Div, TypeScheme ([], primTy_exn))
                                                 ,(VId_Overflow, TypeScheme ([], primTy_exn))
                                                 ,(VId_Size, TypeScheme ([], primTy_exn))
                                                 ,(VId_Subscript, TypeScheme ([], primTy_exn))
                                                 ,(VId_Fail, TypeScheme ([], primTy_string --> primTy_exn))
                                                 ]
                                     ,mkValMap [(VId_EQUAL, TypeScheme ([(tyVarA, [IsEqType])], mkPairType(tyA, tyA) --> primTy_bool)) (* forall ''a. ''a * ''a -> bool *)
                                               ,(VId_COLONEQUAL, TypeScheme ([(tyVarA, [])], mkPairType(refOf tyA, tyA) --> primTy_unit)) (* forall 'a. 'a ref * 'a -> {} *)
                                               ,(VId_EXCLAM, TypeScheme ([(tyVarA, [])], refOf tyA --> tyA)) (* forall 'a. 'a ref -> 'a *)
                                               (* Overloaded identifiers *)
                                               ,(VId_abs, TypeScheme([(tyVarA, [IsSignedReal])], tyA --> tyA)) (* realint -> realint, default: int -> int *)
                                               ,(VId_TILDE, TypeScheme([(tyVarA, [IsSigned])], tyA --> tyA)) (* realint -> realint, default: int -> int *)
                                               ,(VId_div, TypeScheme([(tyVarA, [IsIntegral])], mkPairType(tyA, tyA) --> tyA)) (* wordint * wordint -> wordint, default: int * int -> int *)
                                               ,(VId_mod, TypeScheme([(tyVarA, [IsIntegral])], mkPairType(tyA, tyA) --> tyA)) (* wordint * wordint -> wordint, default: int * int -> int *)
                                               ,(VId_TIMES, TypeScheme([(tyVarA, [IsRing])], mkPairType(tyA, tyA) --> tyA)) (* num * num -> num, default: int * int -> int *)
                                               ,(VId_DIVIDE, TypeScheme([(tyVarA, [IsField])], mkPairType(tyA, tyA) --> tyA)) (* Real * Real -> Real, default: real * real -> real *)
                                               ,(VId_PLUS, TypeScheme([(tyVarA, [IsRing])], mkPairType(tyA, tyA) --> tyA)) (* num * num -> num, default: int * int -> int *)
                                               ,(VId_MINUS, TypeScheme([(tyVarA, [IsRing])], mkPairType(tyA, tyA) --> tyA)) (* num * num -> num, default: int * int -> int *)
                                               ,(VId_LT, TypeScheme([(tyVarA, [IsOrdered])], mkPairType(tyA, tyA) --> primTy_bool)) (* numtxt * numtxt -> bool, default: int * int -> bool *)
                                               ,(VId_GT, TypeScheme([(tyVarA, [IsOrdered])], mkPairType(tyA, tyA) --> primTy_bool)) (* numtxt * numtxt -> bool, default: int * int -> bool *)
                                               ,(VId_LE, TypeScheme([(tyVarA, [IsOrdered])], mkPairType(tyA, tyA) --> primTy_bool)) (* numtxt * numtxt -> bool, default: int * int -> bool *)
                                               ,(VId_GE, TypeScheme([(tyVarA, [IsOrdered])], mkPairType(tyA, tyA) --> primTy_bool)) (* numtxt * numtxt -> bool, default: int * int -> bool *)
                                               (* Non-overloaded identifiers *)
                                               ,(VId_Bool_not, TypeScheme ([], primTy_bool --> primTy_bool))
                                               ]
                                     ]
                    , strMap = mkStrMap []
                    , boundTyVars = USyntax.TyVarSet.empty
                    }
                    [module_Int
                    ,module_Word
                    ,module_Real
                    ,module_String
                    ,module_Char
                    ,module_Array
                    ,module_Vector
                    ,module_Lua
                    ]
      end

val initialTyConSet = let open Typing
                      in USyntax.TyConSet.fromList
                             [primTyCon_int
                             ,primTyCon_word
                             ,primTyCon_real
                             ,primTyCon_string
                             ,primTyCon_char
                             ,primTyCon_exn
                             ,primTyCon_bool
                             ,primTyCon_ref
                             ,primTyCon_list
                             ,primTyCon_array
                             ,primTyCon_vector
                             ,primTyCon_Lua_value
                             ]
                      end
end
