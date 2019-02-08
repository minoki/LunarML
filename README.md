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
LetInExp([ValDec([],PatBind(SimpleVIdPat(MkVId "x"),LetInExp([ValDec([],PatBind(TypedPat(SimpleVIdPat(MkVId "id"),FnType(TyVar(MkTyVar "'a"),TyVar(MkTyVar "'a"))),FnExp([(SimpleVIdPat(MkVId "z"),SimpleVarExp(MkVId "z"))]),NONE))],AppExp(SimpleVarExp(MkVId "id"),SimpleVarExp(MkVId "id"))),NONE))],SimpleVarExp(MkVId "x"))val it = () : unit
- print(Syntax.print_Exp(PostParsing.scopeTyVarsInExp(Syntax.TyVarSet.empty, t)));
LetInExp([ValDec([],PatBind(SimpleVIdPat(MkVId "x"),LetInExp([ValDec([MkTyVar "'a"],PatBind(TypedPat(SimpleVIdPat(MkVId "id"),FnType(TyVar(MkTyVar "'a"),TyVar(MkTyVar "'a"))),FnExp([(SimpleVIdPat(MkVId "z"),SimpleVarExp(MkVId "z"))]),NONE))],AppExp(SimpleVarExp(MkVId "id"),SimpleVarExp(MkVId "id"))),NONE))],SimpleVarExp(MkVId "x"))val it = () : unit
- val t = SimpleParser.parse "let val x = case (let val id: 'a -> 'a = fn z=>z in id id end) of _ => fn z => z : 'a  in x end";
val t = LetInExp ([ValDec (#,#)],VarExp (MkLongVId (#,#)))
  : Syntax.SyntaxTree.Exp
- print(Syntax.print_Exp(PostParsing.scopeTyVarsInExp(Syntax.TyVarSet.empty, t)));
LetInExp([ValDec([MkTyVar "'a"],PatBind(SimpleVIdPat(MkVId "x"),CaseExp(LetInExp([ValDec([],PatBind(TypedPat(SimpleVIdPat(MkVId "id"),FnType(TyVar(MkTyVar "'a"),TyVar(MkTyVar "'a"))),FnExp([(SimpleVIdPat(MkVId "z"),SimpleVarExp(MkVId "z"))]),NONE))],AppExp(SimpleVarExp(MkVId "id"),SimpleVarExp(MkVId "id"))),[(WildcardPat,FnExp([(SimpleVIdPat(MkVId "z"),TypedExp(SimpleVarExp(MkVId "z"),TyVar(MkTyVar "'a")))]))]),NONE))],SimpleVarExp(MkVId "x"))val it = () : unit
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
  : DamepoMLParser.result
- print (Syntax.print_Exp ast1);
LetInExp([ValDec([],PatBind(SimpleVIdPat(MkVId "x"),LetInExp([ValDec([],PatBind(TypedPat(SimpleVIdPat(MkVId "id"),FnType(TyVar(MkTyVar "'a"),TyVar(MkTyVar "'a"))),FnExp([(SimpleVIdPat(MkVId "z"),SimpleVarExp(MkVId "z"))]),NONE))],AppExp(SimpleVarExp(MkVId "id"),SimpleVarExp(MkVId "id"))),NONE))],SimpleVarExp(MkVId "x"))val it = () : unit
- val ast2 = PostParsing.toUExp(PostParsing.newContext(PostParsing.emptyConEnv), PostParsing.scopeTyVarsInExp(Syntax.TyVarSet.empty, ast1));
val ast2 = LetInExp ([ValDec (#,#)],VarExp (MkLongVId (#,#)))
  : USyntax.SyntaxTree.Exp
- print (USyntax.print_Exp ast2);
LetInExp([ValDec([],PatBind(TypedPat(SimpleVIdPat(MkVId "x"),TyVar(UTyVar(MkTyVar "_",100))),LetInExp([ValDec([UTyVar(MkTyVar "'a",101)],PatBind(TypedPat(SimpleVIdPat(MkVId "id"),FnType(TyVar(UTyVar(MkTyVar "'a",102)),TyVar(UTyVar(MkTyVar "'a",103)))),FnExp([(TypedPat(SimpleVIdPat(MkVId "z"),TyVar(UTyVar(MkTyVar "_",104))),SimpleVarExp(MkVId "z"))]),NONE))],AppExp(SimpleVarExp(MkVId "id"),SimpleVarExp(MkVId "id"))),NONE))],SimpleVarExp(MkVId "x"))val it = () : unit
- val ctx = Typing.newContext(Typing.emptyEnv);
val ctx = {env=MkEnv {strMap=E,tyConMap=E,valMap=E},nextTyVar=ref 100}
  : Typing.Context
- val (a, b, c, d) = Typing.typeCheckExp(ctx, ast2);

uncaught exception Fail [Fail: let-in not implemented yet]
  raised at: typing.sml:274.13-274.46
```
