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

val VId_ref = USyntax.MkVId("ref", 0)
val VId_nil = USyntax.MkVId("nil", 1)
val VId_true = USyntax.MkVId("true", 2)
val VId_false = USyntax.MkVId("false", 3)
val VId_Match = USyntax.MkVId("Match", 4)
val VId_Bind = USyntax.MkVId("Bind", 5)
val VId_DCOLON = USyntax.MkVId("::", 6)
val VId_EQUAL = USyntax.MkVId("=", 7)
val VId_COLONEQUAL = USyntax.MkVId(":=", 8)
val VId_abs = USyntax.MkVId("abs", 9)
val VId_TILDE = USyntax.MkVId("~", 10)
val VId_div = USyntax.MkVId("div", 11)
val VId_mod = USyntax.MkVId("mod", 12)
val VId_TIMES = USyntax.MkVId("*", 13)
val VId_DIVIDE = USyntax.MkVId("/", 14)
val VId_PLUS = USyntax.MkVId("+", 15)
val VId_MINUS = USyntax.MkVId("-", 16)
val VId_LT = USyntax.MkVId("<", 17)
val VId_GT = USyntax.MkVId(">", 18)
val VId_LE = USyntax.MkVId("<=", 19)
val VId_GE = USyntax.MkVId(">=", 20)
(* Monomorphic operators: *)
val VId_EQUAL_bool   = USyntax.MkVId("=@bool", 22)
val VId_EQUAL_int    = USyntax.MkVId("=@int", 23)
val VId_EQUAL_word   = USyntax.MkVId("=@word", 24)
val VId_EQUAL_string = USyntax.MkVId("=@string", 25)
val VId_EQUAL_char   = USyntax.MkVId("=@char", 26)
val VId_EQUAL_list   = USyntax.MkVId("=@list", 27) (* parametrized by element type *)
val VId_EQUAL_ref    = USyntax.MkVId("=@ref", 28)
val VId_abs_int      = USyntax.MkVId("abs@int", 29)
val VId_abs_real     = USyntax.MkVId("abs@real", 30)
val VId_TILDE_int    = USyntax.MkVId("~@int", 31)
val VId_TILDE_real   = USyntax.MkVId("~@real", 32)
val VId_div_int      = USyntax.MkVId("div@int", 33)
val VId_div_word     = USyntax.MkVId("div@word", 34)
val VId_mod_int      = USyntax.MkVId("mod@int", 35)
val VId_mod_word     = USyntax.MkVId("mod@word", 36)
val VId_TIMES_int    = USyntax.MkVId("*@int", 37)
val VId_TIMES_word   = USyntax.MkVId("*@word", 38)
val VId_TIMES_real   = USyntax.MkVId("*@real", 39)
val VId_DIVIDE_real  = USyntax.MkVId("/@real", 40)
val VId_PLUS_int     = USyntax.MkVId("+@int", 41)
val VId_PLUS_word    = USyntax.MkVId("+@word", 42)
val VId_PLUS_real    = USyntax.MkVId("+@real", 43)
val VId_MINUS_int    = USyntax.MkVId("-@int", 44)
val VId_MINUS_word   = USyntax.MkVId("-@word", 45)
val VId_MINUS_real   = USyntax.MkVId("-@real", 46)
val VId_LT_int       = USyntax.MkVId("<@int", 47)
val VId_LT_word      = USyntax.MkVId("<@word", 48)
val VId_LT_real      = USyntax.MkVId("<@real", 49)
val VId_LT_string    = USyntax.MkVId("<@string", 50)
val VId_LT_char      = USyntax.MkVId("<@char", 51)
val VId_GT_int       = USyntax.MkVId(">@int", 52)
val VId_GT_word      = USyntax.MkVId(">@word", 53)
val VId_GT_real      = USyntax.MkVId(">@real", 54)
val VId_GT_string    = USyntax.MkVId(">@string", 55)
val VId_GT_char      = USyntax.MkVId(">@char", 56)
val VId_LE_int       = USyntax.MkVId("<=@int", 57)
val VId_LE_word      = USyntax.MkVId("<=@word", 58)
val VId_LE_real      = USyntax.MkVId("<=@real", 59)
val VId_LE_string    = USyntax.MkVId("<=@string", 60)
val VId_LE_char      = USyntax.MkVId("<=@char", 61)
val VId_GE_int       = USyntax.MkVId(">=@int", 62)
val VId_GE_word      = USyntax.MkVId(">=@word", 63)
val VId_GE_real      = USyntax.MkVId(">=@real", 64)
val VId_GE_string    = USyntax.MkVId(">=@string", 65)
val VId_GE_char      = USyntax.MkVId(">=@char", 66)
(* Misc *)
val VId_print = USyntax.MkVId("print", 67)
val VId_Int_toString = USyntax.MkVId("Int.toString", 68)
val VId_HAT = USyntax.MkVId("^", 69) (* String.^ *)
val VId_raise = USyntax.MkVId("raise", 70)
val VId_not = USyntax.MkVId("not", 71)
val VId_EQUAL_array = USyntax.MkVId("=@array", 72)
val VId_EQUAL_vector = USyntax.MkVId("=@vector", 73)
val VId_Array_array = USyntax.MkVId("Array.array", 74)
val VId_Array_fromList = USyntax.MkVId("Array.fromList", 75)
val VId_Array_tabulate = USyntax.MkVId("Array.tabulate", 76)
val VId_Array_length = USyntax.MkVId("Array.length", 77)
val VId_Array_sub = USyntax.MkVId("Array.sub", 78)
val VId_Array_update = USyntax.MkVId("Array.update", 79)
val VId_Vector_fromList = USyntax.MkVId("Vector.fromList", 80)
val VId_Vector_tabulate = USyntax.MkVId("Vector.tabulate", 81)
val VId_Vector_length = USyntax.MkVId("Vector.length", 82)
val VId_Vector_sub = USyntax.MkVId("Vector.sub", 83)

val initialEnv_ToTypedSyntax
    = let val ValueConstructor = Syntax.ValueConstructor
          val ExceptionConstructor = Syntax.ExceptionConstructor
          val ValueVariable = Syntax.ValueVariable
          val mkTyConMap = List.foldl Syntax.TyConMap.insert' Syntax.TyConMap.empty
          val mkValMap = List.foldl Syntax.VIdMap.insert' Syntax.VIdMap.empty
          val mkStrMap = List.foldl Syntax.StrIdMap.insert' Syntax.StrIdMap.empty
          fun getTyConIndex(Syntax.MkQualified(_, USyntax.MkTyCon(_, n))) = n
          val module_Int = ToTypedSyntax.MkEnv { valMap = mkValMap
                                                              [(* toLarge, fromLarge, toInt, fromInt, precision, minInt, maxInt *)
                                                               (Syntax.MkVId "+", (VId_PLUS_int, ValueVariable))
                                                              ,(Syntax.MkVId "-", (VId_MINUS_int, ValueVariable))
                                                              ,(Syntax.MkVId "*", (VId_TIMES_int, ValueVariable))
                                                              ,(Syntax.MkVId "div", (VId_div_int, ValueVariable))
                                                              ,(Syntax.MkVId "mod", (VId_mod_int, ValueVariable))
                                                               (* quot, rem, compare *)
                                                              ,(Syntax.MkVId "<", (VId_LT_int, ValueVariable))
                                                              ,(Syntax.MkVId "<=", (VId_LE_int, ValueVariable))
                                                              ,(Syntax.MkVId ">", (VId_GT_int, ValueVariable))
                                                              ,(Syntax.MkVId ">=", (VId_GE_int, ValueVariable))
                                                              ,(Syntax.MkVId "~", (VId_TILDE_int, ValueVariable))
                                                              ,(Syntax.MkVId "abs", (VId_abs_int, ValueVariable))
                                                               (* min, max, sign, sameSign, fmt *)
                                                              ,(Syntax.MkVId "toString", (VId_Int_toString, ValueVariable))
                                                               (* scan, fromString *)
                                                              ]
                                               , tyConMap = mkTyConMap
                                                                [(Syntax.MkTyCon "int", ToTypedSyntax.BTyCon (getTyConIndex Typing.primTyCon_int))
                                                                ]
                                               , strMap = Syntax.StrIdMap.empty
                                               }
          val module_Array = ToTypedSyntax.MkEnv { valMap = mkValMap
                                                                [(* maxLen *)
                                                                 (Syntax.MkVId "array", (VId_Array_array, ValueVariable))
                                                                ,(Syntax.MkVId "fromList", (VId_Array_fromList, ValueVariable))
                                                                ,(Syntax.MkVId "tabulate", (VId_Array_tabulate, ValueVariable))
                                                                ,(Syntax.MkVId "length", (VId_Array_length, ValueVariable))
                                                                ,(Syntax.MkVId "sub", (VId_Array_sub, ValueVariable))
                                                                ,(Syntax.MkVId "update", (VId_Array_update, ValueVariable))
                                                                 (* vector, copy, copyVec, appi, app, modifyi, modify, foldli, foldri, foldl, foldr, findi, find, exists, all, collate *)
                                                                 ]
                                                  , tyConMap = mkTyConMap
                                                                   [(Syntax.MkTyCon "array", ToTypedSyntax.BTyCon (getTyConIndex Typing.primTyCon_array))
                                                                   ,(Syntax.MkTyCon "vector", ToTypedSyntax.BTyCon (getTyConIndex Typing.primTyCon_vector))
                                                                   ]
                                                  , strMap = Syntax.StrIdMap.empty
                                                  }
          val module_Vector = ToTypedSyntax.MkEnv { valMap = mkValMap
                                                                 [(* maxLen *)
                                                                  (Syntax.MkVId "fromList", (VId_Vector_fromList, ValueVariable))
                                                                 ,(Syntax.MkVId "tabulate", (VId_Vector_tabulate, ValueVariable))
                                                                 ,(Syntax.MkVId "length", (VId_Vector_length, ValueVariable))
                                                                 ,(Syntax.MkVId "sub", (VId_Vector_sub, ValueVariable))
                                                                  (* update, concat, appi, app, mapi, map, foldli, foldri, foldl, foldr, findi, find, exists, all, collate *)
                                                                 ]
                                                  , tyConMap = mkTyConMap
                                                                   [(Syntax.MkTyCon "vector", ToTypedSyntax.BTyCon (getTyConIndex Typing.primTyCon_vector))
                                                                   ]
                                                  , strMap = Syntax.StrIdMap.empty
                                                  }
      in ToTypedSyntax.MkEnv { valMap = List.foldl Syntax.VIdMap.insert' Syntax.VIdMap.empty
                                                   [(Syntax.MkVId "ref", (VId_ref, ValueConstructor))
                                                   ,(Syntax.MkVId "nil", (VId_nil, ValueConstructor))
                                                   ,(Syntax.MkVId "true", (VId_true, ValueConstructor))
                                                   ,(Syntax.MkVId "false", (VId_false, ValueConstructor))
                                                   ,(Syntax.MkVId "Match", (VId_Match, ExceptionConstructor))
                                                   ,(Syntax.MkVId "Bind", (VId_Bind, ExceptionConstructor))
                                                   ,(Syntax.MkVId "::", (VId_DCOLON, ValueConstructor))
                                                   ,(Syntax.MkVId "=", (VId_EQUAL, ValueVariable))
                                                   ,(Syntax.MkVId ":=", (VId_COLONEQUAL, ValueVariable))
                                                   ,(Syntax.MkVId "abs", (VId_abs, ValueVariable))
                                                   ,(Syntax.MkVId "~", (VId_TILDE, ValueVariable))
                                                   ,(Syntax.MkVId "div", (VId_div, ValueVariable))
                                                   ,(Syntax.MkVId "mod", (VId_mod, ValueVariable))
                                                   ,(Syntax.MkVId "*", (VId_TIMES, ValueVariable))
                                                   ,(Syntax.MkVId "/", (VId_DIVIDE, ValueVariable))
                                                   ,(Syntax.MkVId "+", (VId_PLUS, ValueVariable))
                                                   ,(Syntax.MkVId "-", (VId_MINUS, ValueVariable))
                                                   ,(Syntax.MkVId "<", (VId_LT, ValueVariable))
                                                   ,(Syntax.MkVId ">", (VId_GT, ValueVariable))
                                                   ,(Syntax.MkVId "<=", (VId_LE, ValueVariable))
                                                   ,(Syntax.MkVId ">=", (VId_GE, ValueVariable))
                                                   ,(Syntax.MkVId "print", (VId_print, ValueVariable))
                                                   ,(Syntax.MkVId "^", (VId_HAT, ValueVariable))
                                                   ,(Syntax.MkVId "not", (VId_not, ValueVariable))
                                                   ]
                             , tyConMap = List.foldl Syntax.TyConMap.insert' Syntax.TyConMap.empty
                                                     [(Syntax.MkTyCon "unit", ToTypedSyntax.BTyAlias ([], Typing.primTy_unit))
                                                     ,(Syntax.MkTyCon "int", ToTypedSyntax.BTyCon (getTyConIndex Typing.primTyCon_int))
                                                     ,(Syntax.MkTyCon "word", ToTypedSyntax.BTyCon (getTyConIndex Typing.primTyCon_word))
                                                     ,(Syntax.MkTyCon "real", ToTypedSyntax.BTyCon (getTyConIndex Typing.primTyCon_real))
                                                     ,(Syntax.MkTyCon "string", ToTypedSyntax.BTyCon (getTyConIndex Typing.primTyCon_string))
                                                     ,(Syntax.MkTyCon "char", ToTypedSyntax.BTyCon (getTyConIndex Typing.primTyCon_char))
                                                     ,(Syntax.MkTyCon "exn", ToTypedSyntax.BTyCon (getTyConIndex Typing.primTyCon_exn))
                                                     ,(Syntax.MkTyCon "bool", ToTypedSyntax.BTyCon (getTyConIndex Typing.primTyCon_bool))
                                                     ,(Syntax.MkTyCon "ref", ToTypedSyntax.BTyCon (getTyConIndex Typing.primTyCon_ref))
                                                     ,(Syntax.MkTyCon "list", ToTypedSyntax.BTyCon (getTyConIndex Typing.primTyCon_list))
                                                     ,(Syntax.MkTyCon "array", ToTypedSyntax.BTyCon (getTyConIndex Typing.primTyCon_array))
                                                     ,(Syntax.MkTyCon "vector", ToTypedSyntax.BTyCon (getTyConIndex Typing.primTyCon_vector))
                                                     ]
                             , strMap = List.foldl Syntax.StrIdMap.insert' Syntax.StrIdMap.empty
                                                   [(Syntax.MkStrId "Int", module_Int)
                                                   ,(Syntax.MkStrId "Array", module_Array)
                                                   ,(Syntax.MkStrId "Vector", module_Vector)
                                                   ]
                             }
      end

val initialEnv : Typing.Env
    = let open Syntax
          open Typing
          val mkTyMap = List.foldl USyntax.TyConMap.insert' USyntax.TyConMap.empty
          val mkValMap = List.foldl USyntax.VIdMap.insert' USyntax.VIdMap.empty
          val mkStrMap = List.foldl Syntax.StrIdMap.insert' Syntax.StrIdMap.empty
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
          fun listOf(t) = mkTyCon([t], primTyCon_list)
          fun arrayOf(t) = mkTyCon([t], primTyCon_array)
          fun vectorOf(t) = mkTyCon([t], primTyCon_vector)
          val module_Int = MkEnv { tyMap = mkTyMap
                                               [(USyntax.MkTyCon("int", 0), TyStr(TypeFcn([], primTy_int), emptyValEnv)) (* ??? *)
                                               ]
                                 , valMap = mkValMap
                                                [(VId_PLUS_int, (TypeScheme ([], mkPairType(primTy_int, primTy_int) --> primTy_int), ValueVariable))
                                                ,(VId_MINUS_int, (TypeScheme ([], mkPairType(primTy_int, primTy_int) --> primTy_int), ValueVariable))
                                                ,(VId_TIMES_int, (TypeScheme ([], mkPairType(primTy_int, primTy_int) --> primTy_int), ValueVariable))
                                                ,(VId_div_int, (TypeScheme ([], mkPairType(primTy_int, primTy_int) --> primTy_int), ValueVariable))
                                                ,(VId_mod_int, (TypeScheme ([], mkPairType(primTy_int, primTy_int) --> primTy_int), ValueVariable))
                                                ,(VId_LT_int, (TypeScheme ([], mkPairType(primTy_int, primTy_int) --> primTy_bool), ValueVariable))
                                                ,(VId_LE_int, (TypeScheme ([], mkPairType(primTy_int, primTy_int) --> primTy_bool), ValueVariable))
                                                ,(VId_GT_int, (TypeScheme ([], mkPairType(primTy_int, primTy_int) --> primTy_bool), ValueVariable))
                                                ,(VId_GE_int, (TypeScheme ([], mkPairType(primTy_int, primTy_int) --> primTy_bool), ValueVariable))
                                                ,(VId_TILDE_int, (TypeScheme ([], primTy_int --> primTy_bool), ValueVariable))
                                                ,(VId_abs_int, (TypeScheme ([], primTy_int --> primTy_bool), ValueVariable))
                                                ,(VId_Int_toString, (TypeScheme ([], primTy_int --> primTy_string), ValueVariable))
                                                ]
                                 , strMap = mkStrMap []
                                 }
          val module_Array = MkEnv { tyMap = mkTyMap
                                                 [(USyntax.MkTyCon("array", 9), TyStr(TypeFcn([tyVarA], arrayOf tyA), emptyValEnv))
                                                 ,(USyntax.MkTyCon("vector", 9), TyStr(TypeFcn([tyVarA], vectorOf tyA), emptyValEnv))
                                                 ]
                                   , valMap = mkValMap
                                                  [(VId_Array_array, (TypeScheme ([(tyVarA, [])], mkPairType(primTy_int, tyA) --> arrayOf tyA), ValueVariable))
                                                  ,(VId_Array_fromList, (TypeScheme ([(tyVarA, [])], listOf tyA --> arrayOf tyA), ValueVariable))
                                                  ,(VId_Array_tabulate, (TypeScheme ([(tyVarA, [])], mkPairType(primTy_int, primTy_int --> tyA) --> arrayOf tyA), ValueVariable))
                                                  ,(VId_Array_length, (TypeScheme ([(tyVarA, [])], arrayOf tyA --> primTy_int), ValueVariable))
                                                  ,(VId_Array_sub, (TypeScheme ([(tyVarA, [])], mkPairType(arrayOf tyA, primTy_int) --> tyA), ValueVariable))
                                                  ,(VId_Array_update, (TypeScheme ([(tyVarA, [])], USyntax.TupleType(SourcePos.nullSpan, [arrayOf tyA, primTy_int, tyA]) --> primTy_unit), ValueVariable))
                                                  ]
                                   , strMap = mkStrMap []
                                   }
          val module_Vector = MkEnv { tyMap = mkTyMap
                                                 [(USyntax.MkTyCon("vector", 9), TyStr(TypeFcn([tyVarA], vectorOf tyA), emptyValEnv))
                                                 ]
                                   , valMap = mkValMap
                                                  [(VId_Vector_fromList, (TypeScheme ([(tyVarA, [])], listOf tyA --> vectorOf tyA), ValueVariable))
                                                  ,(VId_Vector_tabulate, (TypeScheme ([(tyVarA, [])], mkPairType(primTy_int, primTy_int --> tyA) --> vectorOf tyA), ValueVariable))
                                                  ,(VId_Vector_length, (TypeScheme ([(tyVarA, [])], vectorOf tyA --> primTy_int), ValueVariable))
                                                  ,(VId_Vector_sub, (TypeScheme ([(tyVarA, [])], mkPairType(vectorOf tyA, primTy_int) --> tyA), ValueVariable))
                                                  ]
                                   , strMap = mkStrMap []
                                   }
      in MkEnv { tyMap = mkTyMap
                             [(USyntax.MkTyCon("unit", 9), TyStr(TypeFcn([], primTy_unit), emptyValEnv))
                             ,(USyntax.MkTyCon("bool", 6), TyStr(TypeFcn([], primTy_bool)
                                                                , mkValMap [(VId_true, (TypeScheme ([], primTy_bool), ValueConstructor))
                                                                           ,(VId_false, (TypeScheme ([], primTy_bool), ValueConstructor))
                              ]))
                             ,(USyntax.MkTyCon("int", 0), TyStr(TypeFcn([], primTy_int), emptyValEnv))
                             ,(USyntax.MkTyCon("word", 1), TyStr(TypeFcn([], primTy_word), emptyValEnv))
                             ,(USyntax.MkTyCon("real", 2), TyStr(TypeFcn([], primTy_real), emptyValEnv))
                             ,(USyntax.MkTyCon("string", 3), TyStr(TypeFcn([], primTy_string), emptyValEnv))
                             ,(USyntax.MkTyCon("char", 4), TyStr(TypeFcn([], primTy_char), emptyValEnv))
                             ,(USyntax.MkTyCon("list", 5), TyStr(TypeFcn([tyVarA], mkTyCon([mkTyVar(tyVarA)], primTyCon_list))
                                                                , mkValMap [(VId_nil, (TypeScheme ([(tyVarA, [])], mkTyCon([mkTyVar(tyVarA)], primTyCon_list)), ValueConstructor))
                                                                           ,(VId_DCOLON, (TypeScheme ([(tyVarA, [])], mkPairType(mkTyVar(tyVarA), mkTyCon([mkTyVar(tyVarA)], primTyCon_list)) --> mkTyCon([mkTyVar(tyVarA)], primTyCon_list)), ValueConstructor))
                              ]))
                             ,(USyntax.MkTyCon("ref", 7), TyStr(TypeFcn([tyVarA], mkTyCon([mkTyVar(tyVarA)], primTyCon_ref))
                                                           , mkValMap [(VId_ref, (TypeScheme ([(tyVarA, [])], mkTyVar(tyVarA) --> mkTyCon([mkTyVar(tyVarA)], primTyCon_ref)), ValueConstructor))
                              ]))
                             ,(USyntax.MkTyCon("exn", 5), TyStr(TypeFcn([], primTy_exn), emptyValEnv))
                             ,(USyntax.MkTyCon("array", 9), TyStr(TypeFcn([tyVarA], mkTyCon([mkTyVar(tyVarA)], primTyCon_array)), emptyValEnv))
                             ,(USyntax.MkTyCon("vector", 9), TyStr(TypeFcn([tyVarA], mkTyCon([mkTyVar(tyVarA)], primTyCon_vector)), emptyValEnv))
                             ]
               , valMap = mkValMap
                              (* C Appendix: The Initial Static Basis *)
                              [(VId_ref, (TypeScheme ([(tyVarA, [])], mkTyVar(tyVarA) --> mkTyCon([mkTyVar(tyVarA)], primTyCon_ref)), ValueConstructor)) (* forall 'a. 'a -> 'a ref *)
                              ,(VId_nil, (TypeScheme ([(tyVarA, [])], mkTyCon([mkTyVar(tyVarA)], primTyCon_list)), ValueConstructor)) (* forall 'a. 'a list *)
                              ,(VId_true, (TypeScheme ([], primTy_bool), ValueConstructor))
                              ,(VId_false, (TypeScheme ([], primTy_bool), ValueConstructor))
                              ,(VId_Match, (TypeScheme ([], primTy_exn), ExceptionConstructor))
                              ,(VId_Bind, (TypeScheme ([], primTy_exn), ExceptionConstructor))
                              ,(VId_DCOLON, (TypeScheme ([(tyVarA, [])], mkPairType(mkTyVar(tyVarA), mkTyCon([mkTyVar(tyVarA)], primTyCon_list)) --> mkTyCon([mkTyVar(tyVarA)], primTyCon_list)), ValueConstructor)) (* forall 'a. 'a * 'a list -> 'a list *)
                              ,(VId_EQUAL, (TypeScheme ([(tyVarA, [IsEqType])], mkPairType(mkTyVar(tyVarA), mkTyVar(tyVarA)) --> primTy_bool), ValueVariable)) (* forall ''a. ''a * ''a -> bool *)
                              ,(VId_COLONEQUAL, (TypeScheme ([(tyVarA, [])], mkPairType(mkTyCon([mkTyVar(tyVarA)], primTyCon_ref), mkTyVar(tyVarA)) --> primTy_unit), ValueVariable)) (* forall 'a. 'a ref * 'a -> {} *)
                              (* Overloaded identifiers *)
                              ,(VId_abs, (TypeScheme([(tyVarA, [IsSignedReal])], mkTyVar(tyVarA) --> mkTyVar(tyVarA)), ValueVariable)) (* realint -> realint, default: int -> int *)
                              ,(VId_TILDE, (TypeScheme([(tyVarA, [IsSigned])], mkTyVar(tyVarA) --> mkTyVar(tyVarA)), ValueVariable)) (* realint -> realint, default: int -> int *)
                              ,(VId_div, (TypeScheme([(tyVarA, [IsIntegral])], mkPairType(mkTyVar(tyVarA), mkTyVar(tyVarA)) --> mkTyVar(tyVarA)), ValueVariable)) (* wordint * wordint -> wordint, default: int * int -> int *)
                              ,(VId_mod, (TypeScheme([(tyVarA, [IsIntegral])], mkPairType(mkTyVar(tyVarA), mkTyVar(tyVarA)) --> mkTyVar(tyVarA)), ValueVariable)) (* wordint * wordint -> wordint, default: int * int -> int *)
                              ,(VId_TIMES, (TypeScheme([(tyVarA, [IsRing])], mkPairType(mkTyVar(tyVarA), mkTyVar(tyVarA)) --> mkTyVar(tyVarA)), ValueVariable)) (* num * num -> num, default: int * int -> int *)
                              ,(VId_DIVIDE, (TypeScheme([(tyVarA, [IsField])], mkPairType(mkTyVar(tyVarA), mkTyVar(tyVarA)) --> mkTyVar(tyVarA)), ValueVariable)) (* Real * Real -> Real, default: real * real -> real *)
                              ,(VId_PLUS, (TypeScheme([(tyVarA, [IsRing])], mkPairType(mkTyVar(tyVarA), mkTyVar(tyVarA)) --> mkTyVar(tyVarA)), ValueVariable)) (* num * num -> num, default: int * int -> int *)
                              ,(VId_MINUS, (TypeScheme([(tyVarA, [IsRing])], mkPairType(mkTyVar(tyVarA), mkTyVar(tyVarA)) --> mkTyVar(tyVarA)), ValueVariable)) (* num * num -> num, default: int * int -> int *)
                              ,(VId_LT, (TypeScheme([(tyVarA, [IsOrdered])], mkPairType(mkTyVar(tyVarA), mkTyVar(tyVarA)) --> primTy_bool), ValueVariable)) (* numtxt * numtxt -> bool, default: int * int -> bool *)
                              ,(VId_GT, (TypeScheme([(tyVarA, [IsOrdered])], mkPairType(mkTyVar(tyVarA), mkTyVar(tyVarA)) --> primTy_bool), ValueVariable)) (* numtxt * numtxt -> bool, default: int * int -> bool *)
                              ,(VId_LE, (TypeScheme([(tyVarA, [IsOrdered])], mkPairType(mkTyVar(tyVarA), mkTyVar(tyVarA)) --> primTy_bool), ValueVariable)) (* numtxt * numtxt -> bool, default: int * int -> bool *)
                              ,(VId_GE, (TypeScheme([(tyVarA, [IsOrdered])], mkPairType(mkTyVar(tyVarA), mkTyVar(tyVarA)) --> primTy_bool), ValueVariable)) (* numtxt * numtxt -> bool, default: int * int -> bool *)
                                   (* Non-overloaded identifiers *)
                              ,(VId_print, (TypeScheme ([], primTy_string --> primTy_unit), ValueVariable))
                              ,(VId_HAT, (TypeScheme ([], mkPairType(primTy_string, primTy_string) --> primTy_string), ValueVariable))
                              ,(VId_not, (TypeScheme ([], primTy_bool --> primTy_bool), ValueVariable))
                              ]
               , strMap = mkStrMap
                              [(Syntax.MkStrId "Int", module_Int)
                              ,(Syntax.MkStrId "Array", module_Array)
                              ,(Syntax.MkStrId "Vector", module_Vector)
                              ]
               }
      end
end
