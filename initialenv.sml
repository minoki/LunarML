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

val initialEnv : Typing.Env
    = let open Syntax
          open Typing
          val mkTyMap = List.foldl Syntax.TyConMap.insert' Syntax.TyConMap.empty
          val mkValMap = List.foldl Syntax.VIdMap.insert' Syntax.VIdMap.empty
          val tyVarA = USyntax.MkTyVar("a", 0)
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
                                                    , mkValMap [(MkVId "true", (TypeScheme ([], primTy_bool), ValueConstructor))
                                                               ,(MkVId "false", (TypeScheme ([], primTy_bool), ValueConstructor))
                                                               ]))
                             ,(MkTyCon "int", TyStr(TypeFcn([], primTy_int), emptyValEnv))
                             ,(MkTyCon "word", TyStr(TypeFcn([], primTy_word), emptyValEnv))
                             ,(MkTyCon "real", TyStr(TypeFcn([], primTy_real), emptyValEnv))
                             ,(MkTyCon "string", TyStr(TypeFcn([], primTy_string), emptyValEnv))
                             ,(MkTyCon "char", TyStr(TypeFcn([], primTy_char), emptyValEnv))
                             ,(MkTyCon "list", TyStr(TypeFcn([tyVarA], USyntax.TyCon([USyntax.TyVar(tyVarA)], primTyCon_list))
                                                    , mkValMap [(MkVId "nil", (TypeScheme ([(tyVarA, [])], USyntax.TyCon([USyntax.TyVar(tyVarA)], primTyCon_list)), ValueConstructor))
                                                               ,(MkVId "::", (TypeScheme ([(tyVarA, [])], USyntax.FnType(USyntax.PairType(USyntax.TyVar(tyVarA), USyntax.TyCon([USyntax.TyVar(tyVarA)], primTyCon_list)), USyntax.TyCon([USyntax.TyVar(tyVarA)], primTyCon_list))), ValueConstructor))
                                                               ]))
                             ,(MkTyCon "ref", TyStr(TypeFcn([tyVarA], USyntax.TyCon([USyntax.TyVar(tyVarA)], primTyCon_ref))
                                                   , mkValMap [(MkVId "ref", (TypeScheme ([(tyVarA, [])], USyntax.FnType(USyntax.TyVar(tyVarA), USyntax.TyCon([USyntax.TyVar(tyVarA)], primTyCon_ref))), ValueConstructor))
                                                              ]))
                             ,(MkTyCon "exn", TyStr(TypeFcn([], primTy_exn), emptyValEnv))
                             ]
               , valMap = mkValMap
                              (* C Appendix: The Initial Static Basis *)
                              [(MkVId "ref", (TypeScheme ([(tyVarA, [])], USyntax.FnType(USyntax.TyVar(tyVarA), USyntax.TyCon([USyntax.TyVar(tyVarA)], primTyCon_ref))), ValueConstructor)) (* forall 'a. 'a -> 'a ref *)
                              ,(MkVId "nil", (TypeScheme ([(tyVarA, [])], USyntax.TyCon([USyntax.TyVar(tyVarA)], primTyCon_list)), ValueConstructor)) (* forall 'a. 'a list *)
                              ,(MkVId "true", (TypeScheme ([], primTy_bool), ValueConstructor))
                              ,(MkVId "false", (TypeScheme ([], primTy_bool), ValueConstructor))
                              ,(MkVId "Match", (TypeScheme ([], primTy_exn), ExceptionConstructor))
                              ,(MkVId "Bind", (TypeScheme ([], primTy_exn), ExceptionConstructor))
                              ,(MkVId "::", (TypeScheme ([(tyVarA, [])], USyntax.FnType(USyntax.PairType(USyntax.TyVar(tyVarA), USyntax.TyCon([USyntax.TyVar(tyVarA)], primTyCon_list)), USyntax.TyCon([USyntax.TyVar(tyVarA)], primTyCon_list))), ValueConstructor)) (* forall 'a. 'a * 'a list -> 'a list *)
                              ,(MkVId "=", (TypeScheme ([(tyVarA, [IsEqType])], USyntax.FnType(USyntax.PairType(USyntax.TyVar(tyVarA), USyntax.TyVar(tyVarA)), primTy_bool)), ValueVariable)) (* forall ''a. ''a * ''a -> bool *)
                              ,(MkVId ":=", (TypeScheme ([(tyVarA, [])], USyntax.FnType(USyntax.PairType(USyntax.TyCon([USyntax.TyVar(tyVarA)], primTyCon_ref), USyntax.TyVar(tyVarA)), primTy_unit)), ValueVariable)) (* forall 'a. 'a ref * 'a -> {} *)
                              (* Overloaded identifiers *)
                              ,(MkVId "abs", (TypeScheme([(tyVarA, [IsSignedReal])], USyntax.FnType(USyntax.TyVar(tyVarA), USyntax.TyVar(tyVarA))), ValueVariable)) (* realint -> realint, default: int -> int *)
                              ,(MkVId "~", (TypeScheme([(tyVarA, [IsSigned])], USyntax.FnType(USyntax.TyVar(tyVarA), USyntax.TyVar(tyVarA))), ValueVariable)) (* realint -> realint, default: int -> int *)
                              ,(MkVId "div", (TypeScheme([(tyVarA, [IsIntegral])], USyntax.FnType(USyntax.PairType(USyntax.TyVar(tyVarA), USyntax.TyVar(tyVarA)), USyntax.TyVar(tyVarA))), ValueVariable)) (* wordint * wordint -> wordint, default: int * int -> int *)
                              ,(MkVId "mod", (TypeScheme([(tyVarA, [IsIntegral])], USyntax.FnType(USyntax.PairType(USyntax.TyVar(tyVarA), USyntax.TyVar(tyVarA)), USyntax.TyVar(tyVarA))), ValueVariable)) (* wordint * wordint -> wordint, default: int * int -> int *)
                              ,(MkVId "*", (TypeScheme([(tyVarA, [IsRing])], USyntax.FnType(USyntax.PairType(USyntax.TyVar(tyVarA), USyntax.TyVar(tyVarA)), USyntax.TyVar(tyVarA))), ValueVariable)) (* num * num -> num, default: int * int -> int *)
                              ,(MkVId "/", (TypeScheme([(tyVarA, [IsField])], USyntax.FnType(USyntax.PairType(USyntax.TyVar(tyVarA), USyntax.TyVar(tyVarA)), USyntax.TyVar(tyVarA))), ValueVariable)) (* Real * Real -> Real, default: real * real -> real *)
                              ,(MkVId "+", (TypeScheme([(tyVarA, [IsRing])], USyntax.FnType(USyntax.PairType(USyntax.TyVar(tyVarA), USyntax.TyVar(tyVarA)), USyntax.TyVar(tyVarA))), ValueVariable)) (* num * num -> num, default: int * int -> int *)
                              ,(MkVId "-", (TypeScheme([(tyVarA, [IsRing])], USyntax.FnType(USyntax.PairType(USyntax.TyVar(tyVarA), USyntax.TyVar(tyVarA)), USyntax.TyVar(tyVarA))), ValueVariable)) (* num * num -> num, default: int * int -> int *)
                              ,(MkVId "<", (TypeScheme([(tyVarA, [IsOrdered])], USyntax.FnType(USyntax.PairType(USyntax.TyVar(tyVarA), USyntax.TyVar(tyVarA)), primTy_bool)), ValueVariable)) (* numtxt * numtxt -> bool, default: int * int -> bool *)
                              ,(MkVId ">", (TypeScheme([(tyVarA, [IsOrdered])], USyntax.FnType(USyntax.PairType(USyntax.TyVar(tyVarA), USyntax.TyVar(tyVarA)), primTy_bool)), ValueVariable)) (* numtxt * numtxt -> bool, default: int * int -> bool *)
                              ,(MkVId "<=", (TypeScheme([(tyVarA, [IsOrdered])], USyntax.FnType(USyntax.PairType(USyntax.TyVar(tyVarA), USyntax.TyVar(tyVarA)), primTy_bool)), ValueVariable)) (* numtxt * numtxt -> bool, default: int * int -> bool *)
                              ,(MkVId ">=", (TypeScheme([(tyVarA, [IsOrdered])], USyntax.FnType(USyntax.PairType(USyntax.TyVar(tyVarA), USyntax.TyVar(tyVarA)), primTy_bool)), ValueVariable)) (* numtxt * numtxt -> bool, default: int * int -> bool *)
                              ]
               , strMap = Syntax.StrIdMap.empty
               }
      end
end
