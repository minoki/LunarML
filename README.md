# DamepoML

A would-be SML compiler.

```sml
$ sml
- CM.make "sources.cm";
...
- val t = SimpleParser.parse "let val id: 'a -> 'a = fn z=>z in id id end;";
val t = [ValDec ([],[PatBind #])] : Syntax.Dec list
- print(Syntax.print_Decs t);
[ValDec([],[PatBind(ConOrVarPat(MkVId "it"),LetInExp([ValDec([],[PatBind(TypedPat(ConOrVarPat(MkVId "id"),FnType(TyVar(MkTyVar "'a"),TyVar(MkTyVar "'a"))),FnExp([(ConOrVarPat(MkVId "z"),SimpleVarExp(MkVId "z"))]))])],AppExp(SimpleVarExp(MkVId "id"),SimpleVarExp(MkVId "id"))))])]val it = () : unit
- print(Syntax.print_Decs (PostParsing.scopeTyVarsInDecs(Syntax.TyVarSet.empty, t)));
[ValDec([],[PatBind(ConOrVarPat(MkVId "it"),LetInExp([ValDec([MkTyVar "'a"],[PatBind(TypedPat(ConOrVarPat(MkVId "id"),FnType(TyVar(MkTyVar "'a"),TyVar(MkTyVar "'a"))),FnExp([(ConOrVarPat(MkVId "z"),SimpleVarExp(MkVId "z"))]))])],AppExp(SimpleVarExp(MkVId "id"),SimpleVarExp(MkVId "id"))))])]val it = () : unit
- val t = SimpleParser.parse "case (let val id: 'a -> 'a = fn z=>z in id id end) of _ => fn z => z : 'a;";
val t = [ValDec ([],[PatBind #])] : Syntax.Dec list
- print(Syntax.print_Decs (PostParsing.scopeTyVarsInDecs(Syntax.TyVarSet.empty, t)));
[ValDec([MkTyVar "'a"],[PatBind(ConOrVarPat(MkVId "it"),CaseExp(LetInExp([ValDec([],[PatBind(TypedPat(ConOrVarPat(MkVId "id"),FnType(TyVar(MkTyVar "'a"),TyVar(MkTyVar "'a"))),FnExp([(ConOrVarPat(MkVId "z"),SimpleVarExp(MkVId "z"))]))])],AppExp(SimpleVarExp(MkVId "id"),SimpleVarExp(MkVId "id"))),[(WildcardPat,FnExp([(ConOrVarPat(MkVId "z"),TypedExp(SimpleVarExp(MkVId "z"),TyVar(MkTyVar "'a")))]))]))])]val it = () : unit
- SimpleParser.parse "1 + 2 * (4 + 5) / 3;";
val it = [ValDec ([],[PatBind #])] : Syntax.Dec list
- print(Syntax.print_Decs it);
[ValDec([],[PatBind(ConOrVarPat(MkVId "it"),AppExp(SimpleVarExp(MkVId "+"),TupleExp [SConExp(IntegerConstant 1),AppExp(SimpleVarExp(MkVId "/"),TupleExp [AppExp(SimpleVarExp(MkVId "*"),TupleExp [SConExp(IntegerConstant 2),AppExp(SimpleVarExp(MkVId "+"),TupleExp [SConExp(IntegerConstant 4),SConExp(IntegerConstant 5)])]),SConExp(IntegerConstant 3)])]))])]val it = () : unit
- SimpleParser.parse "let infixr - in 1 - 2 - 3 end;";
val it = [ValDec ([],[PatBind #])] : Syntax.Dec list
- print(Syntax.print_Decs it);
[ValDec([],[PatBind(ConOrVarPat(MkVId "it"),LetInExp([<Dec>],AppExp(SimpleVarExp(MkVId "-"),TupleExp [SConExp(IntegerConstant 1),AppExp(SimpleVarExp(MkVId "-"),TupleExp [SConExp(IntegerConstant 2),SConExp(IntegerConstant 3)])])))])]val it = () : unit
```

```
- val ast1 = SimpleParser.parse "let val id: 'a -> 'a = fn z=>z in id id end;";
val ast1 = [ValDec ([],[PatBind #])] : Syntax.Dec list
- print (Syntax.print_Decs ast1);
[ValDec([],[PatBind(ConOrVarPat(MkVId "it"),LetInExp([ValDec([],[PatBind(TypedPat(ConOrVarPat(MkVId "id"),FnType(TyVar(MkTyVar "'a"),TyVar(MkTyVar "'a"))),FnExp([(ConOrVarPat(MkVId "z"),SimpleVarExp(MkVId "z"))]))])],AppExp(SimpleVarExp(MkVId "id"),SimpleVarExp(MkVId "id"))))])]val it = () : unit
- val ctx = Typing.newContext();
val ctx = {nextTyVar=ref 100,tyVarConstraints=ref E,tyVarSubst=ref E}
  : Typing.Context
- val (_, ast2) = ToTypedSyntax.toUDecs(ctx, ToTypedSyntax.emptyEnv, PostParsing.scopeTyVarsInDecs(Syntax.TyVarSet.empty, ast1));
val ast2 = [ValDec ([],[PatBind #])] : USyntax.Dec list
- print (USyntax.print_Decs ast2);
[ValDec([],[PatBind(VarPat(MkVId "it",TyVar(MkTyVar("_",100))),LetInExp([ValDec([MkTyVar("'a",101)],[PatBind(VarPat(MkVId "id",FnType(TyVar(MkTyVar("'a",102)),TyVar(MkTyVar("'a",103)))),FnExp([(VarPat(MkVId "z",TyVar(MkTyVar("_",104))),SimpleVarExp(MkVId "z",ValueVariable))]))])],AppExp(SimpleVarExp(MkVId "id",ValueVariable),SimpleVarExp(MkVId "id",ValueVariable))))])]val it = () : unit
- val (a, b, c) = Typing.typeCheckProgram(ctx, Typing.initialEnv, ast2);
val a =
  MkEnv
    {strMap=E,tyMap=E,valMap=T {cnt=1,key=MkVId #,left=E,right=E,value=(#,#)}}
  : Typing.Env
val b = E : Typing.UnaryConstraint list USyntax.TyVarMap.map
val c = [ValDec ([],[PatBind #])] : USyntax.Dec list
- print (USyntax.print_Decs c);
[ValDec([],[PatBind(VarPat(MkVId "it",FnType(TyVar(MkTyVar("_",106)),TyVar(MkTyVar("_",106)))),LetInExp([ValDec([MkTyVar("'a",101)],[PatBind(VarPat(MkVId "id",FnType(TyVar(MkTyVar("_",104)),TyVar(MkTyVar("_",104)))),FnExp([(VarPat(MkVId "z",TyVar(MkTyVar("_",104))),SimpleVarExp(MkVId "z",ValueVariable))]))])],AppExp(SimpleVarExp(MkVId "id",ValueVariable),SimpleVarExp(MkVId "id",ValueVariable))))])]val it = () : unit
```

```
- val ast1 = SimpleParser.parse "let val a = 123 in (fn z => z) a end;";
val ast1 = [ValDec ([],[PatBind #])] : Syntax.Dec list
- val ctx = Typing.newContext();
val ctx = {constraints=ref [],nextTyVar=ref 100,tyVarConstraints=ref E,tyVarSubst=ref E}
  : Typing.Context
- val (_, ast2) = ToTypedSyntax.toUDecs(ctx, ToTypedSyntax.emptyEnv, PostParsing.scopeTyVarsInDecs(Syntax.TyVarSet.empty, ast1));
val ast2 = [ValDec ([],[PatBind #])] : USyntax.Dec list
- val (a, b, c) = Typing.typeCheckProgram(ctx, Typing.initialEnv, ast2);
val a =
  MkEnv
    {strMap=E,tyMap=E,valMap=T {cnt=1,key=MkVId #,left=E,right=E,value=(#,#)}}
  : Typing.Env
val b = E : Typing.UnaryConstraint list USyntax.TyVarMap.map
val c = [ValDec ([],[PatBind #])] : USyntax.Dec list
- print (USyntax.print_Decs c);
[ValDec([],[PatBind(VarPat(MkVId "it",TyCon([],MkLongTyCon(MkLongTyCon([],MkTyCon "int"),0))),LetInExp([ValDec([],[PatBind(VarPat(MkVId "a",TyCon([],MkLongTyCon(MkLongTyCon([],MkTyCon "int"),0))),SConExp(IntegerConstant 123))])],AppExp(FnExp([(VarPat(MkVId "z",TyCon([],MkLongTyCon(MkLongTyCon([],MkTyCon "int"),0))),SimpleVarExp(MkVId "z",ValueVariable))]),SimpleVarExp(MkVId "a",ValueVariable))))])]val it = () : unit
```

Let polymorphism:

```
- val ast1 = SimpleParser.parse "val id = fn z => z; (id \"foo\", id 123, id id);";
val ast1 = [ValDec ([],[PatBind #]),ValDec ([],[PatBind #])] : Syntax.Dec list
- val ctx = Typing.newContext();
val ctx = {nextTyVar=ref 100,tyVarConstraints=ref E,tyVarSubst=ref E}
  : Typing.Context
- val (_, ast2) = ToTypedSyntax.toUDecs(ctx, ToTypedSyntax.emptyEnv, PostParsing.scopeTyVarsInDecs(Syntax.TyVarSet.empty, ast1));
val ast2 = [ValDec ([],[PatBind #]),ValDec ([],[PatBind #])]
  : USyntax.Dec list
- val (a, b, c) = Typing.typeCheckProgram(ctx, Typing.initialEnv, ast2);
val a =
  MkEnv
    {strMap=E,tyMap=E,
     valMap=T {cnt=2,key=MkVId #,left=T #,right=E,value=(#,#)}} : Typing.Env
val b = E : Typing.UnaryConstraint list USyntax.TyVarMap.map
val c = [ValDec ([],[PatBind #]),ValDec ([],[PatBind #])] : USyntax.Dec list
- print (USyntax.print_Decs c);
[ValDec([],[PatBind(VarPat(MkVId "it",RecordType [(NumericLabel 1,TyCon([],MkLongTyCon(MkLongTyCon([],MkTyCon "string"),3))),(NumericLabel 2,TyCon([],MkLongTyCon(MkLongTyCon([],MkTyCon "int"),0))),(NumericLabel 3,FnType(TyVar(MkTyVar("_",108)),TyVar(MkTyVar("_",108))))]),LetInExp([ValDec([],[PatBind(VarPat(MkVId "id",FnType(TyVar(MkTyVar("_",102)),TyVar(MkTyVar("_",102)))),FnExp([(VarPat(MkVId "z",TyVar(MkTyVar("_",102))),SimpleVarExp(MkVId "z",ValueVariable))]))])],TupleExp [AppExp(SimpleVarExp(MkVId "id",ValueVariable),SConExp(StringConstant "foo")),AppExp(SimpleVarExp(MkVId "id",ValueVariable),SConExp(IntegerConstant 123)),AppExp(SimpleVarExp(MkVId "id",ValueVariable),SimpleVarExp(MkVId "id",ValueVariable))]))])]val it = () : unit
- print (Typing.print_Env a);
MkEnv{tyMap=[],valMap=[(MkVId "id",(TypeScheme([(MkTyVar("_",101),[])],FnType(TyVar(MkTyVar("_",101)),TyVar(MkTyVar("_",101)))),ValueVariable)),(MkVId "it",(TypeScheme([],TyVar(MkTyVar("_",102))),ValueVariable))],strMap=[]}val it = () : unit
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
  raised at: typing.sml:303.22-303.80
```
