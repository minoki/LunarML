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
- val ast2 = PostParsing.toUExp(PostParsing.newContext(), PostParsing.emptyEnv, PostParsing.scopeTyVarsInExp(Syntax.TyVarSet.empty, ast1));
val ast2 = LetInExp ([ValDec (#,#)],VarExp (MkLongVId #,ValueVariable))
  : USyntax.Exp
- print (USyntax.print_Exp ast2);
LetInExp([ValDec([],[PatBind(VarPat(MkVId "x",TyVar(UTyVar(MkTyVar "_",100))),LetInExp([ValDec([UTyVar(MkTyVar "'a",101)],[PatBind(VarPat(MkVId "id",FnType(TyVar(UTyVar(MkTyVar "'a",102)),TyVar(UTyVar(MkTyVar "'a",103)))),FnExp([(VarPat(MkVId "z",TyVar(UTyVar(MkTyVar "_",104))),SimpleVarExp(MkVId "z",ValueVariable))]))])],AppExp(SimpleVarExp(MkVId "id",ValueVariable),SimpleVarExp(MkVId "id",ValueVariable))))])],SimpleVarExp(MkVId "x",ValueVariable))val it = () : unit
- val ctx = Typing.newContext();
val ctx = {nextTyVar=ref 100} : Typing.Context
- val (a, b, c, d) = Typing.typeCheckExp(ctx, Typing.emptyEnv, ast2);
val a = [] : Typing.Subst
val b = [] : (USyntax.TyVar * Typing.TyVarConstraint) list
val c = TyVar (UTyVar (MkTyVar "_",100)) : USyntax.Ty
val d = LetInExp ([ValDec (#,#)],VarExp (MkLongVId #,ValueVariable))
  : USyntax.Exp
- USyntax.print_Exp d;
val it =
  "LetInExp([ValDec([],[PatBind(VarPat(MkVId \"x\",TyVar(UTyVar(MkTyVar \"_\"#"
  : string
- print (USyntax.print_Exp d);
LetInExp([ValDec([],[PatBind(VarPat(MkVId "x",TyVar(UTyVar(MkTyVar "_",100))),LetInExp([ValDec([UTyVar(MkTyVar "'a",101)],[PatBind(VarPat(MkVId "id",FnType(TyVar(UTyVar(MkTyVar "'a",102)),TyVar(UTyVar(MkTyVar "'a",103)))),FnExp([(VarPat(MkVId "z",TyVar(UTyVar(MkTyVar "_",104))),SimpleVarExp(MkVId "z",ValueVariable))]))])],AppExp(SimpleVarExp(MkVId "id",ValueVariable),SimpleVarExp(MkVId "id",ValueVariable))))])],SimpleVarExp(MkVId "x",ValueVariable))val it = () : unit
```

```
- SimpleParser.parse "let val a = 123 in (fn z => z) a end";
val it = LetInExp ([ValDec (#,#)],AppExp (FnExp #,VarExp #))
  : Syntax.SyntaxTree.Exp
- val ast2 = PostParsing.toUExp(PostParsing.newContext(), PostParsing.emptyEnv, PostParsing.scopeTyVarsInExp(Syntax.TyVarSet.empty, it));
val ast2 = LetInExp ([ValDec (#,#)],AppExp (FnExp #,VarExp #)) : USyntax.Exp
- val ctx = Typing.newContext();
val ctx = {nextTyVar=ref 100} : Typing.Context
- val (a, b, c, d) = Typing.typeCheckExp(ctx, Typing.emptyEnv, ast2);
val a =
  [(UTyVar (MkTyVar #,100),TyCon ([],ULongTyCon #)),
   (UTyVar (MkTyVar #,101),TyCon ([],ULongTyCon #))] : Typing.Subst
val b = [] : (USyntax.TyVar * Typing.TyVarConstraint) list
val c = TyCon ([],ULongTyCon (MkLongTyCon #,0)) : USyntax.Ty
val d = LetInExp ([ValDec (#,#)],AppExp (FnExp #,VarExp #)) : USyntax.Exp
- USyntax.print_Exp(d);
val it =
  "LetInExp([ValDec([],[PatBind(VarPat(MkVId \"a\",TyCon([],ULongTyCon(MkLo#"
  : string
- print (USyntax.print_Exp(d));
LetInExp([ValDec([],[PatBind(VarPat(MkVId "a",TyCon([],ULongTyCon(MkLongTyCon([],MkTyCon "int"),0))),SConExp(IntegerConstant 123))])],AppExp(FnExp([(VarPat(MkVId "z",TyCon([],ULongTyCon(MkLongTyCon([],MkTyCon "int"),0))),SimpleVarExp(MkVId "z",ValueVariable))]),SimpleVarExp(MkVId "a",ValueVariable)))val it = () : unit
```
