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

val vidCounter = ref ~1
fun newVId name = let val n = !vidCounter
                  in vidCounter := n - 1
                   ; USyntax.MkVId(name, n)
                  end

(* Ref *)
val VId_ref        = newVId "ref"
val VId_COLONEQUAL = newVId "General.:="

(* Bool *)
val VId_true  = newVId "true"
val VId_false = newVId "false"
val VId_not   = newVId "Bool.not"

(* List *)
val VId_nil    = newVId "nil"
val VId_DCOLON = newVId "::"

(* Exception *)
val VId_Match = newVId "Match"
val VId_Bind  = newVId "Bind"
val VId_raise = newVId "raise"

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
val VId_HAT       = newVId "String.^"
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
val VId_print = newVId "TextIO.print"

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
