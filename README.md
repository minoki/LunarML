# DamepoML

A would-be SML compiler.

```sml
$ sml
- CM.make "sources.cm";
...
- val t = SimpleParser.parse "let val id: 'a -> 'a = fn z=>z in id id end;";
val t = [ValDec ([],[PatBind #])] : Syntax.Dec list
- print(Syntax.print_list Syntax.print_Dec t);
[ValDec([],[PatBind(ConOrVarPat(MkVId "it"),LetInExp([ValDec([],[PatBind(TypedPat(ConOrVarPat(MkVId "id"),FnType(TyVar(MkTyVar "'a"),TyVar(MkTyVar "'a"))),FnExp([(ConOrVarPat(MkVId "z"),SimpleVarExp(MkVId "z"))]))])],AppExp(SimpleVarExp(MkVId "id"),SimpleVarExp(MkVId "id"))))])]val it = () : unit
- print(Syntax.print_list Syntax.print_Dec (PostParsing.scopeTyVarsInDecs(Syntax.TyVarSet.empty, t)));
[ValDec([],[PatBind(ConOrVarPat(MkVId "it"),LetInExp([ValDec([MkTyVar "'a"],[PatBind(TypedPat(ConOrVarPat(MkVId "id"),FnType(TyVar(MkTyVar "'a"),TyVar(MkTyVar "'a"))),FnExp([(ConOrVarPat(MkVId "z"),SimpleVarExp(MkVId "z"))]))])],AppExp(SimpleVarExp(MkVId "id"),SimpleVarExp(MkVId "id"))))])]val it = () : unit
- val t = SimpleParser.parse "case (let val id: 'a -> 'a = fn z=>z in id id end) of _ => fn z => z : 'a;";
val t = [ValDec ([],[PatBind #])] : Syntax.Dec list
- print(Syntax.print_list Syntax.print_Dec (PostParsing.scopeTyVarsInDecs(Syntax.TyVarSet.empty, t)));
[ValDec([MkTyVar "'a"],[PatBind(ConOrVarPat(MkVId "it"),CaseExp(LetInExp([ValDec([],[PatBind(TypedPat(ConOrVarPat(MkVId "id"),FnType(TyVar(MkTyVar "'a"),TyVar(MkTyVar "'a"))),FnExp([(ConOrVarPat(MkVId "z"),SimpleVarExp(MkVId "z"))]))])],AppExp(SimpleVarExp(MkVId "id"),SimpleVarExp(MkVId "id"))),[(WildcardPat,FnExp([(ConOrVarPat(MkVId "z"),TypedExp(SimpleVarExp(MkVId "z"),TyVar(MkTyVar "'a")))]))]))])]val it = () : unit
- SimpleParser.parse "1 + 2 * (4 + 5) / 3;";
val it = [ValDec ([],[PatBind #])] : Syntax.Dec list
- print(Syntax.print_list Syntax.print_Dec it);
[ValDec([],[PatBind(ConOrVarPat(MkVId "it"),AppExp(SimpleVarExp(MkVId "+"),TupleExp [SConExp(IntegerConstant 1),AppExp(SimpleVarExp(MkVId "/"),TupleExp [AppExp(SimpleVarExp(MkVId "*"),TupleExp [SConExp(IntegerConstant 2),AppExp(SimpleVarExp(MkVId "+"),TupleExp [SConExp(IntegerConstant 4),SConExp(IntegerConstant 5)])]),SConExp(IntegerConstant 3)])]))])]val it = () : unit
- SimpleParser.parse "let infixr - in 1 - 2 - 3 end;";
val it = [ValDec ([],[PatBind #])] : Syntax.Dec list
- print(Syntax.print_list Syntax.print_Dec it);
[ValDec([],[PatBind(ConOrVarPat(MkVId "it"),LetInExp([<Dec>],AppExp(SimpleVarExp(MkVId "-"),TupleExp [SConExp(IntegerConstant 1),AppExp(SimpleVarExp(MkVId "-"),TupleExp [SConExp(IntegerConstant 2),SConExp(IntegerConstant 3)])])))])]val it = () : unit
```

```
- val ast1 = SimpleParser.parse "let val id: 'a -> 'a = fn z=>z in id id end;";
val ast1 = [ValDec ([],[PatBind #])] : Syntax.Dec list
- print (Syntax.print_list Syntax.print_Dec ast1);
[ValDec([],[PatBind(ConOrVarPat(MkVId "it"),LetInExp([ValDec([],[PatBind(TypedPat(ConOrVarPat(MkVId "id"),FnType(TyVar(MkTyVar "'a"),TyVar(MkTyVar "'a"))),FnExp([(ConOrVarPat(MkVId "z"),SimpleVarExp(MkVId "z"))]))])],AppExp(SimpleVarExp(MkVId "id"),SimpleVarExp(MkVId "id"))))])]val it = () : unit
- val ctx = Typing.newContext();
val ctx = {nextTyVar=ref 100,tyVarConstraints=ref [],tyVarSubst=ref E}
  : Typing.Context
- val (env, ast2) = ToTypedSyntax.toUDecs(ctx, ToTypedSyntax.emptyEnv, PostParsing.scopeTyVarsInDecs(Syntax.TyVarSet.empty, ast1));
val env = MkEnv {strMap=E,tyConMap=E,valMap=E} : ToTypedSyntax.Env
val ast2 = [ValDec ([],[PatBind #])] : USyntax.Dec list
- print (Syntax.print_list USyntax.print_Dec ast2);
[ValDec([],[PatBind(VarPat(MkVId "it",TyVar(MkTyVar("_",100))),LetInExp([ValDec([MkTyVar("'a",101)],[PatBind(VarPat(MkVId "id",FnType(TyVar(MkTyVar("'a",102)),TyVar(MkTyVar("'a",103)))),FnExp([(VarPat(MkVId "z",TyVar(MkTyVar("_",104))),SimpleVarExp(MkVId "z",ValueVariable))]))])],AppExp(SimpleVarExp(MkVId "id",ValueVariable),SimpleVarExp(MkVId "id",ValueVariable))))])]val it = () : unit
- val (a, b, c) = Typing.typeCheckProgram(ctx, Typing.initialEnv, ast2);
val a =
  T
    {cnt=5,key=MkTyVar ("_",100),
     left=T {cnt=2,key=MkTyVar #,left=T #,right=E,value=TyVar #},
     right=T {cnt=2,key=MkTyVar #,left=E,right=T #,value=FnType #},
     value=FnType (TyVar #,TyVar #)} : Typing.Subst
val b = [] : (USyntax.TyVar * Typing.TyVarConstraint) list
val c = [ValDec ([],[PatBind #])] : USyntax.Dec list
- print (Syntax.print_list USyntax.print_Dec c);
[ValDec([],[PatBind(VarPat(MkVId "it",FnType(TyVar(MkTyVar("_",106)),TyVar(MkTyVar("_",106)))),LetInExp([ValDec([MkTyVar("'a",101)],[PatBind(VarPat(MkVId "id",FnType(TyVar(MkTyVar("_",104)),TyVar(MkTyVar("_",104)))),FnExp([(VarPat(MkVId "z",TyVar(MkTyVar("_",104))),SimpleVarExp(MkVId "z",ValueVariable))]))])],AppExp(SimpleVarExp(MkVId "id",ValueVariable),SimpleVarExp(MkVId "id",ValueVariable))))])]val it = () : unit
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
- val (a, b, c, d) = Typing.typeCheckExp_(ctx, Typing.initialEnv, ast2);
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
- val ast1 = SimpleParser.parse "let val id = fn z => z in (id \"foo\", id 123, id id) end;";
val ast1 = [ValDec ([],[PatBind #])] : Syntax.Dec list
- val ctx = Typing.newContext();
val ctx = {nextTyVar=ref 100,tyVarConstraints=ref [],tyVarSubst=ref E}
  : Typing.Context
- val (env, ast2) = ToTypedSyntax.toUDecs(ctx, ToTypedSyntax.emptyEnv, PostParsing.scopeTyVarsInDecs(Syntax.TyVarSet.empty, ast1));
val env = MkEnv {strMap=E,tyConMap=E,valMap=E} : ToTypedSyntax.Env
val ast2 = [ValDec ([],[PatBind #])] : USyntax.Dec list
- val (a, b, c) = Typing.typeCheckProgram(ctx, Typing.initialEnv, ast2);
val a =
  T
    {cnt=8,key=MkTyVar ("_",104),
     left=T {cnt=3,key=MkTyVar #,left=T #,right=T #,value=FnType #},
     right=T {cnt=4,key=MkTyVar #,left=T #,right=T #,value=TyCon #},
     value=TyCon ([],MkLongTyCon #)} : Typing.Subst
val b = [] : (USyntax.TyVar * Typing.TyVarConstraint) list
val c = [ValDec ([],[PatBind #])] : USyntax.Dec list
- print (Syntax.print_list USyntax.print_Dec c);
[ValDec([],[PatBind(VarPat(MkVId "it",RecordType [(NumericLabel 1,TyCon([],MkLongTyCon(MkLongTyCon([],MkTyCon "string"),3))),(NumericLabel 2,TyCon([],MkLongTyCon(MkLongTyCon([],MkTyCon "int"),0))),(NumericLabel 3,FnType(TyVar(MkTyVar("_",108)),TyVar(MkTyVar("_",108))))]),LetInExp([ValDec([],[PatBind(VarPat(MkVId "id",FnType(TyVar(MkTyVar("_",102)),TyVar(MkTyVar("_",102)))),FnExp([(VarPat(MkVId "z",TyVar(MkTyVar("_",102))),SimpleVarExp(MkVId "z",ValueVariable))]))])],TupleExp [AppExp(SimpleVarExp(MkVId "id",ValueVariable),SConExp(StringConstant "foo")),AppExp(SimpleVarExp(MkVId "id",ValueVariable),SConExp(IntegerConstant 123)),AppExp(SimpleVarExp(MkVId "id",ValueVariable),SimpleVarExp(MkVId "id",ValueVariable))]))])]val it = () : unit
```

The type is not generalized when reference cell involves:

```
- val ast1 = SimpleParser.parse "let val ! = fn ref x => x ; val id = !(ref (fn z => z)) in (id \"foo\", id 123, id id) end;";
val ast1 = [ValDec ([],[PatBind #])] : Syntax.Dec list
- val ctx = Typing.newContext();
val ctx = {nextTyVar=ref 100,tyVarConstraints=ref [],tyVarSubst=ref E}
  : Typing.Context
- val (_, ast2) = ToTypedSyntax.toUDecs(ctx, ToTypedSyntax.emptyEnv, PostParsing.scopeTyVarsInDecs(Syntax.TyVarSet.empty, ast1));
val ast2 = [ValDec ([],[PatBind #])] : USyntax.Dec list
- val (a, b, c) = Typing.typeCheckProgram(ctx, Typing.initialEnv, ast2);

uncaught exception TypeError
  raised at: typing.sml:290.22-290.80
```
