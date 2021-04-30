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
(* ! : 'a ref -> 'a (General.!)
   := : 'a ref * 'a -> unit (General.:=)
   ^ : string * string -> string (String.^)
   not : bool -> bool (Bool.not)
   print : string -> unit (TextIO.print)
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

val vidCounter = ref ~2
fun newVId name = let val n = !vidCounter
                  in vidCounter := n - 1
                   ; USyntax.MkVId(name, n)
                  end

(* Ref *)
val VId_ref        = newVId "ref"
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
val VId_PLUS_int     = newVId "Int.+"
val VId_MINUS_int    = newVId "Int.-"
val VId_TIMES_int    = newVId "Int.*"
val VId_div_int      = newVId "Int.div"
val VId_mod_int      = newVId "Int.mod"
val VId_LT_int       = newVId "Int.<"
val VId_LE_int       = newVId "Int.<="
val VId_GT_int       = newVId "Int.>"
val VId_GE_int       = newVId "Int.>="
val VId_TILDE_int    = newVId "Int.~"
val VId_abs_int      = newVId "Int.abs"
val VId_Int_toString = newVId "Int.toString"

(* Word *)
val VId_PLUS_word  = newVId "Word.+"
val VId_MINUS_word = newVId "Word.-"
val VId_TIMES_word = newVId "Word.*"
val VId_div_word   = newVId "Word.div"
val VId_mod_word   = newVId "Word.mod"
val VId_LT_word    = newVId "Word.<"
val VId_LE_word    = newVId "Word.<="
val VId_GT_word    = newVId "Word.>"
val VId_GE_word    = newVId "Word.>="

(* Real *)
val VId_PLUS_real   = newVId "Real.+"
val VId_MINUS_real  = newVId "Real.-"
val VId_TIMES_real  = newVId "Real.*"
val VId_DIVIDE_real = newVId "Real./"
val VId_TILDE_real  = newVId "Real.~"
val VId_abs_real    = newVId "Real.abs"
val VId_LT_real     = newVId "Real.<"
val VId_LE_real     = newVId "Real.<="
val VId_GT_real     = newVId "Real.>"
val VId_GE_real     = newVId "Real.>="

(* String *)
val VId_String_HAT = newVId "String.^"
val VId_LT_string = newVId "String.<"
val VId_LE_string = newVId "String.<="
val VId_GT_string = newVId "String.>"
val VId_GE_string = newVId "String.>="

(* Char *)
val VId_LT_char = newVId "Char.<"
val VId_GT_char = newVId "Char.>"
val VId_LE_char = newVId "Char.<="
val VId_GE_char = newVId "Char.>="

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

(* TextIO *)
val VId_TextIO_print = newVId "TextIO.print"

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
          val module_Int = { valMap = mkValMap
                                          [(* toLarge, fromLarge, toInt, fromInt, precision, minInt, maxInt *)
                                           ("+", VId_PLUS_int)
                                          ,("-", VId_MINUS_int)
                                          ,("*", VId_TIMES_int)
                                          ,("div", VId_div_int)
                                          ,("mod", VId_mod_int)
                                           (* quot, rem, compare *)
                                          ,("<", VId_LT_int)
                                          ,("<=", VId_LE_int)
                                          ,(">", VId_GT_int)
                                          ,(">=", VId_GE_int)
                                          ,("~", VId_TILDE_int)
                                          ,("abs", VId_abs_int)
                                           (* min, max, sign, sameSign, fmt *)
                                          ,("toString", VId_Int_toString)
                                           (* scan, fromString *)
                                          ]
                           , tyConMap = mkTyConMap
                                            [(Syntax.MkTyCon "int", ToTypedSyntax.BTyCon Typing.primTyCon_int)
                                            ]
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
                             , tyConMap = mkTyConMap
                                              [(Syntax.MkTyCon "array", ToTypedSyntax.BTyCon Typing.primTyCon_array)
                                              ,(Syntax.MkTyCon "vector", ToTypedSyntax.BTyCon Typing.primTyCon_vector)
                                              ]
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
                              , tyConMap = mkTyConMap
                                               [(Syntax.MkTyCon "vector", ToTypedSyntax.BTyCon Typing.primTyCon_vector)
                                               ]
                              , strMap = Syntax.StrIdMap.empty
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
                                    ,("^", VId_String_HAT)
                                    ,("not", VId_Bool_not)
                                    ,("print", VId_TextIO_print)
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
                                 ,(Syntax.MkTyCon "int", ToTypedSyntax.BTyCon Typing.primTyCon_int)
                                 ,(Syntax.MkTyCon "word", ToTypedSyntax.BTyCon Typing.primTyCon_word)
                                 ,(Syntax.MkTyCon "real", ToTypedSyntax.BTyCon Typing.primTyCon_real)
                                 ,(Syntax.MkTyCon "string", ToTypedSyntax.BTyCon Typing.primTyCon_string)
                                 ,(Syntax.MkTyCon "char", ToTypedSyntax.BTyCon Typing.primTyCon_char)
                                 ,(Syntax.MkTyCon "exn", ToTypedSyntax.BTyCon Typing.primTyCon_exn)
                                 ,(Syntax.MkTyCon "bool", ToTypedSyntax.BTyCon Typing.primTyCon_bool)
                                 ,(Syntax.MkTyCon "ref", ToTypedSyntax.BTyCon Typing.primTyCon_ref)
                                 ,(Syntax.MkTyCon "list", ToTypedSyntax.BTyCon Typing.primTyCon_list)
                                 ,(Syntax.MkTyCon "array", ToTypedSyntax.BTyCon Typing.primTyCon_array)
                                 ,(Syntax.MkTyCon "vector", ToTypedSyntax.BTyCon Typing.primTyCon_vector)
                                 ]
         , strMap = mkStrMap
                        [("Int", module_Int)
                        ,("Array", module_Array)
                        ,("Vector", module_Vector)
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
          fun mkTyCon(a, b) = USyntax.TyCon(SourcePos.nullSpan, a, b)
          fun refOf(t) = mkTyCon([t], primTyCon_ref)
          fun listOf(t) = mkTyCon([t], primTyCon_list)
          fun arrayOf(t) = mkTyCon([t], primTyCon_array)
          fun vectorOf(t) = mkTyCon([t], primTyCon_vector)
          val module_Int = { tyMap = mkTyMap
                                         [(USyntax.MkTyCon("int", 0), TyStr(TypeFcn([], primTy_int), emptyValEnv)) (* ??? *)
                                         ]
                           , valMap = mkValMap
                                          [(VId_PLUS_int, TypeScheme ([], mkPairType(primTy_int, primTy_int) --> primTy_int))
                                          ,(VId_MINUS_int, TypeScheme ([], mkPairType(primTy_int, primTy_int) --> primTy_int))
                                          ,(VId_TIMES_int, TypeScheme ([], mkPairType(primTy_int, primTy_int) --> primTy_int))
                                          ,(VId_div_int, TypeScheme ([], mkPairType(primTy_int, primTy_int) --> primTy_int))
                                          ,(VId_mod_int, TypeScheme ([], mkPairType(primTy_int, primTy_int) --> primTy_int))
                                          ,(VId_LT_int, TypeScheme ([], mkPairType(primTy_int, primTy_int) --> primTy_bool))
                                          ,(VId_LE_int, TypeScheme ([], mkPairType(primTy_int, primTy_int) --> primTy_bool))
                                          ,(VId_GT_int, TypeScheme ([], mkPairType(primTy_int, primTy_int) --> primTy_bool))
                                          ,(VId_GE_int, TypeScheme ([], mkPairType(primTy_int, primTy_int) --> primTy_bool))
                                          ,(VId_TILDE_int, TypeScheme ([], primTy_int --> primTy_int))
                                          ,(VId_abs_int, TypeScheme ([], primTy_int --> primTy_int))
                                          ,(VId_Int_toString, TypeScheme ([], primTy_int --> primTy_string))
                                          ]
                           , strMap = mkStrMap []
                           , boundTyVars = USyntax.TyVarSet.empty
                           }
          val module_Array = { tyMap = mkTyMap
                                           [(USyntax.MkTyCon("array", 9), TyStr(TypeFcn([tyVarA], arrayOf tyA), emptyValEnv))
                                           ,(USyntax.MkTyCon("vector", 9), TyStr(TypeFcn([tyVarA], vectorOf tyA), emptyValEnv))
                                           ]
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
          val module_Vector = { tyMap = mkTyMap
                                            [(USyntax.MkTyCon("vector", 9), TyStr(TypeFcn([tyVarA], vectorOf tyA), emptyValEnv))
                                            ]
                              , valMap = mkValMap
                                             [(VId_Vector_fromList, TypeScheme ([(tyVarA, [])], listOf tyA --> vectorOf tyA))
                                             ,(VId_Vector_tabulate, TypeScheme ([(tyVarA, [])], mkPairType(primTy_int, primTy_int --> tyA) --> vectorOf tyA))
                                             ,(VId_Vector_length, TypeScheme ([(tyVarA, [])], vectorOf tyA --> primTy_int))
                                             ,(VId_Vector_sub, TypeScheme ([(tyVarA, [])], mkPairType(vectorOf tyA, primTy_int) --> tyA))
                                             ]
                              , strMap = mkStrMap []
                              , boundTyVars = USyntax.TyVarSet.empty
                              }
      in { tyMap = mkTyMap
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
                                    ,(VId_TextIO_print, TypeScheme ([], primTy_string --> primTy_unit))
                                    ,(VId_String_HAT, TypeScheme ([], mkPairType(primTy_string, primTy_string) --> primTy_string))
                                    ,(VId_Bool_not, TypeScheme ([], primTy_bool --> primTy_bool))
                                    ]
                          ]
         , strMap = mkStrMap
                        [("Int", module_Int)
                        ,("Array", module_Array)
                        ,("Vector", module_Vector)
                        ]
         , boundTyVars = USyntax.TyVarSet.empty
         }
      end
end
