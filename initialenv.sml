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

val initialEnv_ToTypedSyntax
    = let val ValueConstructor = Syntax.ValueConstructor
          val ExceptionConstructor = Syntax.ExceptionConstructor
          val ValueVariable = Syntax.ValueVariable
          fun getTyConIndex(Syntax.MkQualified(_, USyntax.MkTyCon(_, n))) = n
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
                                                     ]
                             , strMap = Syntax.StrIdMap.empty
                             }
      end

val initialEnv : Typing.Env
    = let open Syntax
          open Typing
          val mkTyMap = List.foldl Syntax.TyConMap.insert' Syntax.TyConMap.empty
          val mkValMap = List.foldl USyntax.VIdMap.insert' USyntax.VIdMap.empty
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
      in MkEnv { tyMap = mkTyMap
                             [(MkTyCon "unit", TyStr(TypeFcn([], primTy_unit), emptyValEnv))
                             ,(MkTyCon "bool", TyStr(TypeFcn([], primTy_bool)
                                                    , mkValMap [(VId_true, (TypeScheme ([], primTy_bool), ValueConstructor))
                                                               ,(VId_false, (TypeScheme ([], primTy_bool), ValueConstructor))
                                                               ]))
                             ,(MkTyCon "int", TyStr(TypeFcn([], primTy_int), emptyValEnv))
                             ,(MkTyCon "word", TyStr(TypeFcn([], primTy_word), emptyValEnv))
                             ,(MkTyCon "real", TyStr(TypeFcn([], primTy_real), emptyValEnv))
                             ,(MkTyCon "string", TyStr(TypeFcn([], primTy_string), emptyValEnv))
                             ,(MkTyCon "char", TyStr(TypeFcn([], primTy_char), emptyValEnv))
                             ,(MkTyCon "list", TyStr(TypeFcn([tyVarA], USyntax.TyCon([USyntax.TyVar(tyVarA)], primTyCon_list))
                                                    , mkValMap [(VId_nil, (TypeScheme ([(tyVarA, [])], USyntax.TyCon([USyntax.TyVar(tyVarA)], primTyCon_list)), ValueConstructor))
                                                               ,(VId_DCOLON, (TypeScheme ([(tyVarA, [])], USyntax.FnType(USyntax.PairType(USyntax.TyVar(tyVarA), USyntax.TyCon([USyntax.TyVar(tyVarA)], primTyCon_list)), USyntax.TyCon([USyntax.TyVar(tyVarA)], primTyCon_list))), ValueConstructor))
                                                               ]))
                             ,(MkTyCon "ref", TyStr(TypeFcn([tyVarA], USyntax.TyCon([USyntax.TyVar(tyVarA)], primTyCon_ref))
                                                   , mkValMap [(VId_ref, (TypeScheme ([(tyVarA, [])], USyntax.FnType(USyntax.TyVar(tyVarA), USyntax.TyCon([USyntax.TyVar(tyVarA)], primTyCon_ref))), ValueConstructor))
                                                              ]))
                             ,(MkTyCon "exn", TyStr(TypeFcn([], primTy_exn), emptyValEnv))
                             ]
               , valMap = mkValMap
                              (* C Appendix: The Initial Static Basis *)
                              [(VId_ref, (TypeScheme ([(tyVarA, [])], USyntax.FnType(USyntax.TyVar(tyVarA), USyntax.TyCon([USyntax.TyVar(tyVarA)], primTyCon_ref))), ValueConstructor)) (* forall 'a. 'a -> 'a ref *)
                              ,(VId_nil, (TypeScheme ([(tyVarA, [])], USyntax.TyCon([USyntax.TyVar(tyVarA)], primTyCon_list)), ValueConstructor)) (* forall 'a. 'a list *)
                              ,(VId_true, (TypeScheme ([], primTy_bool), ValueConstructor))
                              ,(VId_false, (TypeScheme ([], primTy_bool), ValueConstructor))
                              ,(VId_Match, (TypeScheme ([], primTy_exn), ExceptionConstructor))
                              ,(VId_Bind, (TypeScheme ([], primTy_exn), ExceptionConstructor))
                              ,(VId_DCOLON, (TypeScheme ([(tyVarA, [])], USyntax.FnType(USyntax.PairType(USyntax.TyVar(tyVarA), USyntax.TyCon([USyntax.TyVar(tyVarA)], primTyCon_list)), USyntax.TyCon([USyntax.TyVar(tyVarA)], primTyCon_list))), ValueConstructor)) (* forall 'a. 'a * 'a list -> 'a list *)
                              ,(VId_EQUAL, (TypeScheme ([(tyVarA, [IsEqType])], USyntax.FnType(USyntax.PairType(USyntax.TyVar(tyVarA), USyntax.TyVar(tyVarA)), primTy_bool)), ValueVariable)) (* forall ''a. ''a * ''a -> bool *)
                              ,(VId_COLONEQUAL, (TypeScheme ([(tyVarA, [])], USyntax.FnType(USyntax.PairType(USyntax.TyCon([USyntax.TyVar(tyVarA)], primTyCon_ref), USyntax.TyVar(tyVarA)), primTy_unit)), ValueVariable)) (* forall 'a. 'a ref * 'a -> {} *)
                              (* Overloaded identifiers *)
                              ,(VId_abs, (TypeScheme([(tyVarA, [IsSignedReal])], USyntax.FnType(USyntax.TyVar(tyVarA), USyntax.TyVar(tyVarA))), ValueVariable)) (* realint -> realint, default: int -> int *)
                              ,(VId_TILDE, (TypeScheme([(tyVarA, [IsSigned])], USyntax.FnType(USyntax.TyVar(tyVarA), USyntax.TyVar(tyVarA))), ValueVariable)) (* realint -> realint, default: int -> int *)
                              ,(VId_div, (TypeScheme([(tyVarA, [IsIntegral])], USyntax.FnType(USyntax.PairType(USyntax.TyVar(tyVarA), USyntax.TyVar(tyVarA)), USyntax.TyVar(tyVarA))), ValueVariable)) (* wordint * wordint -> wordint, default: int * int -> int *)
                              ,(VId_mod, (TypeScheme([(tyVarA, [IsIntegral])], USyntax.FnType(USyntax.PairType(USyntax.TyVar(tyVarA), USyntax.TyVar(tyVarA)), USyntax.TyVar(tyVarA))), ValueVariable)) (* wordint * wordint -> wordint, default: int * int -> int *)
                              ,(VId_TIMES, (TypeScheme([(tyVarA, [IsRing])], USyntax.FnType(USyntax.PairType(USyntax.TyVar(tyVarA), USyntax.TyVar(tyVarA)), USyntax.TyVar(tyVarA))), ValueVariable)) (* num * num -> num, default: int * int -> int *)
                              ,(VId_DIVIDE, (TypeScheme([(tyVarA, [IsField])], USyntax.FnType(USyntax.PairType(USyntax.TyVar(tyVarA), USyntax.TyVar(tyVarA)), USyntax.TyVar(tyVarA))), ValueVariable)) (* Real * Real -> Real, default: real * real -> real *)
                              ,(VId_PLUS, (TypeScheme([(tyVarA, [IsRing])], USyntax.FnType(USyntax.PairType(USyntax.TyVar(tyVarA), USyntax.TyVar(tyVarA)), USyntax.TyVar(tyVarA))), ValueVariable)) (* num * num -> num, default: int * int -> int *)
                              ,(VId_MINUS, (TypeScheme([(tyVarA, [IsRing])], USyntax.FnType(USyntax.PairType(USyntax.TyVar(tyVarA), USyntax.TyVar(tyVarA)), USyntax.TyVar(tyVarA))), ValueVariable)) (* num * num -> num, default: int * int -> int *)
                              ,(VId_LT, (TypeScheme([(tyVarA, [IsOrdered])], USyntax.FnType(USyntax.PairType(USyntax.TyVar(tyVarA), USyntax.TyVar(tyVarA)), primTy_bool)), ValueVariable)) (* numtxt * numtxt -> bool, default: int * int -> bool *)
                              ,(VId_GT, (TypeScheme([(tyVarA, [IsOrdered])], USyntax.FnType(USyntax.PairType(USyntax.TyVar(tyVarA), USyntax.TyVar(tyVarA)), primTy_bool)), ValueVariable)) (* numtxt * numtxt -> bool, default: int * int -> bool *)
                              ,(VId_LE, (TypeScheme([(tyVarA, [IsOrdered])], USyntax.FnType(USyntax.PairType(USyntax.TyVar(tyVarA), USyntax.TyVar(tyVarA)), primTy_bool)), ValueVariable)) (* numtxt * numtxt -> bool, default: int * int -> bool *)
                              ,(VId_GE, (TypeScheme([(tyVarA, [IsOrdered])], USyntax.FnType(USyntax.PairType(USyntax.TyVar(tyVarA), USyntax.TyVar(tyVarA)), primTy_bool)), ValueVariable)) (* numtxt * numtxt -> bool, default: int * int -> bool *)
                                   (* Non-overloaded identifiers *)
                              ,(VId_print, (TypeScheme ([], USyntax.FnType(primTy_string, primTy_unit)), ValueVariable))
                              ]
               , strMap = Syntax.StrIdMap.empty
               }
      end
end
