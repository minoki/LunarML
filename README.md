# DamepoML

A would-be SML compiler.

```sml
$ sml
- CM.make "sources.cm";
...
- val t = SimpleParser.parse "let val x = let val id: 'a -> 'a = fn z=>z in id id end in x end";
val t = LetInExp ([ValDec (#,#)],VarExp (MkLongVId (#,#)))
  : Syntax.SyntaxTree.Exp
- print(Syntax.print_Exp t);
LetInExp([ValDec([],[PatBind(ConOrVarPat(MkVId "x"),LetInExp([ValDec([],[PatBind(TypedPat(ConOrVarPat(MkVId "id"),FnType(TyVar(MkTyVar "'a"),TyVar(MkTyVar "'a"))),FnExp([(ConOrVarPat(MkVId "z"),SimpleVarExp(MkVId "z"))]))])],AppExp(SimpleVarExp(MkVId "id"),SimpleVarExp(MkVId "id"))))])],SimpleVarExp(MkVId "x"))val it = () : unit
- print(Syntax.print_Exp(PostParsing.scopeTyVarsInExp(Syntax.TyVarSet.empty, t)));
LetInExp([ValDec([],[PatBind(ConOrVarPat(MkVId "x"),LetInExp([ValDec([MkTyVar "'a"],[PatBind(TypedPat(ConOrVarPat(MkVId "id"),FnType(TyVar(MkTyVar "'a"),TyVar(MkTyVar "'a"))),FnExp([(ConOrVarPat(MkVId "z"),SimpleVarExp(MkVId "z"))]))])],AppExp(SimpleVarExp(MkVId "id"),SimpleVarExp(MkVId "id"))))])],SimpleVarExp(MkVId "x"))val it = () : unit
- val t = SimpleParser.parse "let val x = case (let val id: 'a -> 'a = fn z=>z in id id end) of _ => fn z => z : 'a  in x end";
val t = LetInExp ([ValDec (#,#)],VarExp (MkLongVId (#,#)))
  : Syntax.SyntaxTree.Exp
- print(Syntax.print_Exp(PostParsing.scopeTyVarsInExp(Syntax.TyVarSet.empty, t)));
LetInExp([ValDec([MkTyVar "'a"],[PatBind(ConOrVarPat(MkVId "x"),CaseExp(LetInExp([ValDec([],[PatBind(TypedPat(ConOrVarPat(MkVId "id"),FnType(TyVar(MkTyVar "'a"),TyVar(MkTyVar "'a"))),FnExp([(ConOrVarPat(MkVId "z"),SimpleVarExp(MkVId "z"))]))])],AppExp(SimpleVarExp(MkVId "id"),SimpleVarExp(MkVId "id"))),[(WildcardPat,FnExp([(ConOrVarPat(MkVId "z"),TypedExp(SimpleVarExp(MkVId "z"),TyVar(MkTyVar "'a")))]))]))])],SimpleVarExp(MkVId "x"))val it = () : unit
- SimpleParser.parse "1 + 2 * (4 + 5) / 3";
val it = AppExp (VarExp (MkLongVId (#,#)),RecordExp [(#,#),(#,#)])
  : Syntax.SyntaxTree.Exp
- print(Syntax.print_Exp it);
AppExp(SimpleVarExp(MkVId "+"),TupleExp [SConExp(IntegerConstant 1),AppExp(SimpleVarExp(MkVId "/"),TupleExp [AppExp(SimpleVarExp(MkVId "*"),TupleExp [SConExp(IntegerConstant 2),AppExp(SimpleVarExp(MkVId "+"),TupleExp [SConExp(IntegerConstant 4),SConExp(IntegerConstant 5)])]),SConExp(IntegerConstant 3)])])val it = () : unit
- SimpleParser.parse "let infixr - in 1 - 2 - 3 end";
val it = LetInExp ([FixityDec (#,#)],AppExp (VarExp #,RecordExp #))
  : Syntax.SyntaxTree.Exp
- print(Syntax.print_Exp it);
LetInExp([<Dec>],AppExp(SimpleVarExp(MkVId "-"),TupleExp [SConExp(IntegerConstant 1),AppExp(SimpleVarExp(MkVId "-"),TupleExp [SConExp(IntegerConstant 2),SConExp(IntegerConstant 3)])]))val it = () : unit
```

```
- val ast1 = SimpleParser.parse "let val x = let val id: 'a -> 'a = fn z=>z in id id end in x end";
val ast1 = LetInExp ([ValDec (#,#)],VarExp (MkLongVId (#,#)))
  : Syntax.SyntaxTree.Exp
- print (Syntax.print_Exp ast1);
LetInExp([ValDec([],[PatBind(ConOrVarPat(MkVId "x"),LetInExp([ValDec([],[PatBind(TypedPat(ConOrVarPat(MkVId "id"),FnType(TyVar(MkTyVar "'a"),TyVar(MkTyVar "'a"))),FnExp([(ConOrVarPat(MkVId "z"),SimpleVarExp(MkVId "z"))]))])],AppExp(SimpleVarExp(MkVId "id"),SimpleVarExp(MkVId "id"))))])],SimpleVarExp(MkVId "x"))val it = () : unit
- val ctx = Typing.newContext();
val ctx =
  {constraints=ref [],nextTyVar=ref 100,tyVarConstraints=ref [],
   tyVarSubst=ref E} : Typing.Context
- val ast2 = ToTypedSyntax.toUExp(ctx, ToTypedSyntax.emptyEnv, PostParsing.scopeTyVarsInExp(Syntax.TyVarSet.empty, ast1));
val ast2 = LetInExp ([ValDec (#,#)],VarExp (MkLongVId #,ValueVariable))
  : USyntax.Exp
- print (USyntax.print_Exp ast2);
LetInExp([ValDec([],[PatBind(VarPat(MkVId "x",TyVar(UTyVar(MkTyVar "_",100))),LetInExp([ValDec([UTyVar(MkTyVar "'a",101)],[PatBind(VarPat(MkVId "id",FnType(TyVar(UTyVar(MkTyVar "'a",102)),TyVar(UTyVar(MkTyVar "'a",103)))),FnExp([(VarPat(MkVId "z",TyVar(UTyVar(MkTyVar "_",104))),SimpleVarExp(MkVId "z",ValueVariable))]))])],AppExp(SimpleVarExp(MkVId "id",ValueVariable),SimpleVarExp(MkVId "id",ValueVariable))))])],SimpleVarExp(MkVId "x",ValueVariable))val it = () : unit
- val (a, b, c, d) = Typing.typeCheckExp(ctx, Typing.initialEnv, ast2);
val a =
  T
    {cnt=5,key=UTyVar (MkTyVar #,100),
     left=T {cnt=2,key=UTyVar #,left=T #,right=E,value=TyVar #},
     right=T {cnt=2,key=UTyVar #,left=E,right=T #,value=FnType #},
     value=FnType (TyVar #,TyVar #)} : Typing.Subst
val b = [] : (USyntax.TyVar * Typing.TyVarConstraint) list
val c = FnType (TyVar (UTyVar (#,#)),TyVar (UTyVar (#,#))) : USyntax.Ty
val d = LetInExp ([ValDec (#,#)],VarExp (MkLongVId #,ValueVariable))
  : USyntax.Exp
- print (USyntax.print_Exp d);
LetInExp([ValDec([],[PatBind(VarPat(MkVId "x",FnType(TyVar(UTyVar(MkTyVar "_",106)),TyVar(UTyVar(MkTyVar "_",106)))),LetInExp([ValDec([UTyVar(MkTyVar "'a",101)],[PatBind(VarPat(MkVId "id",FnType(TyVar(UTyVar(MkTyVar "_",104)),TyVar(UTyVar(MkTyVar "_",104)))),FnExp([(VarPat(MkVId "z",TyVar(UTyVar(MkTyVar "_",104))),SimpleVarExp(MkVId "z",ValueVariable))]))])],AppExp(SimpleVarExp(MkVId "id",ValueVariable),SimpleVarExp(MkVId "id",ValueVariable))))])],SimpleVarExp(MkVId "x",ValueVariable))val it = () : unit
```

```
- val ast1 = SimpleParser.parse "let val a = 123 in (fn z => z) a end";
val ast1 = LetInExp ([ValDec (#,#)],AppExp (FnExp #,VarExp #)) : Syntax.Exp
- val ctx = Typing.newContext();
val ctx =
  {constraints=ref [],nextTyVar=ref 100,tyVarConstraints=ref [],
   tyVarSubst=ref E} : Typing.Context
- val ast2 = ToTypedSyntax.toUExp(ctx, ToTypedSyntax.emptyEnv, PostParsing.scopeTyVarsInExp(Syntax.TyVarSet.empty, ast1));
val ast2 = LetInExp ([ValDec (#,#)],AppExp (FnExp #,VarExp #)) : USyntax.Exp
- val (a, b, c, d) = Typing.typeCheckExp(ctx, Typing.initialEnv, ast2);
val a =
  T
    {cnt=3,key=UTyVar (MkTyVar #,101),
     left=T {cnt=1,key=UTyVar #,left=E,right=E,value=TyCon #},
     right=T {cnt=1,key=UTyVar #,left=E,right=E,value=TyCon #},
     value=TyCon ([],ULongTyCon #)} : Typing.Subst
val b = [] : (USyntax.TyVar * Typing.TyVarConstraint) list
val c = TyCon ([],ULongTyCon (MkLongTyCon #,0)) : USyntax.Ty
val d = LetInExp ([ValDec (#,#)],AppExp (FnExp #,VarExp #)) : USyntax.Exp
- print (USyntax.print_Exp(d));
LetInExp([ValDec([],[PatBind(VarPat(MkVId "a",TyCon([],ULongTyCon(MkLongTyCon([],MkTyCon "int"),0))),SConExp(IntegerConstant 123))])],AppExp(FnExp([(VarPat(MkVId "z",TyCon([],ULongTyCon(MkLongTyCon([],MkTyCon "int"),0))),SimpleVarExp(MkVId "z",ValueVariable))]),SimpleVarExp(MkVId "a",ValueVariable)))val it = () : unit
```

Let polymorphism:

```
- val ast1 = SimpleParser.parse "let val id = fn z => z in (id \"foo\", id 123, id id) end";
val ast1 = LetInExp ([ValDec (#,#)],RecordExp [(#,#),(#,#),(#,#)])
  : Syntax.Exp
- val ctx = Typing.newContext();
val ctx =
  {constraints=ref [],nextTyVar=ref 100,tyVarConstraints=ref [],
   tyVarSubst=ref E} : Typing.Context
- val ast2 = ToTypedSyntax.toUExp(ctx, ToTypedSyntax.emptyEnv, PostParsing.scopeTyVarsInExp(Syntax.TyVarSet.empty, ast1));
val ast2 = LetInExp ([ValDec (#,#)],RecordExp [(#,#),(#,#),(#,#)])
  : USyntax.Exp
- val (a, b, c, d) = Typing.typeCheckExp(ctx, Typing.initialEnv, ast2);
val a =
  T
    {cnt=7,key=UTyVar (MkTyVar #,105),
     left=T {cnt=4,key=UTyVar #,left=T #,right=T #,value=TyCon #},
     right=T {cnt=2,key=UTyVar #,left=E,right=T #,value=FnType #},
     value=TyCon ([],ULongTyCon #)} : Typing.Subst
val b = [] : (USyntax.TyVar * Typing.TyVarConstraint) list
val c =
  RecordType
    [(NumericLabel 1,TyCon (#,#)),(NumericLabel 2,TyCon (#,#)),
     (NumericLabel 3,FnType (#,#))] : USyntax.Ty
val d = LetInExp ([ValDec (#,#)],RecordExp [(#,#),(#,#),(#,#)]) : USyntax.Exp
- print (USyntax.print_Ty c);
RecordType [(NumericLabel 1,TyCon([],ULongTyCon(MkLongTyCon([],MkTyCon "string"),3))),(NumericLabel 2,TyCon([],ULongTyCon(MkLongTyCon([],MkTyCon "int"),0))),(NumericLabel 3,FnType(TyVar(UTyVar(MkTyVar "_",107)),TyVar(UTyVar(MkTyVar "_",107))))]val it = () : unit
- print (USyntax.print_Exp d);
LetInExp([ValDec([],[PatBind(VarPat(MkVId "id",FnType(TyVar(UTyVar(MkTyVar "_",101)),TyVar(UTyVar(MkTyVar "_",101)))),FnExp([(VarPat(MkVId "z",TyVar(UTyVar(MkTyVar "_",101))),SimpleVarExp(MkVId "z",ValueVariable))]))])],TupleExp [AppExp(SimpleVarExp(MkVId "id",ValueVariable),SConExp(StringConstant "foo")),AppExp(SimpleVarExp(MkVId "id",ValueVariable),SConExp(IntegerConstant 123)),AppExp(SimpleVarExp(MkVId "id",ValueVariable),SimpleVarExp(MkVId "id",ValueVariable))])val it = () : unit
```

The type is not generalized when reference cell involves:

```
- val ast1 = SimpleParser.parse "let val ! = fn ref x => x ; val id = !(ref (fn z => z)) in (id \"foo\", id 123, id id) end";
val ast1 =
  LetInExp ([ValDec (#,#),ValDec (#,#)],RecordExp [(#,#),(#,#),(#,#)])
  : Syntax.Exp
- val ctx = Typing.newContext();
val ctx =
  {constraints=ref [],nextTyVar=ref 100,tyVarConstraints=ref [],
   tyVarSubst=ref E} : Typing.Context
- val ast2 = ToTypedSyntax.toUExp(ctx, ToTypedSyntax.emptyEnv, PostParsing.scopeTyVarsInExp(Syntax.TyVarSet.empty, ast1));
val ast2 =
  LetInExp ([ValDec (#,#),ValDec (#,#)],RecordExp [(#,#),(#,#),(#,#)])
  : USyntax.Exp
- val (a, b, c, d) = Typing.typeCheckExp(ctx, Typing.initialEnv, ast2);

uncaught exception TypeError
  raised at: typing.sml:334.17-334.130
```
