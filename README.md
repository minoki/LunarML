# DamepoML

A would-be SML compiler.

```sml
$ sml
- CM.make "sources.cm";
...
- fun print_error (s,p1,p2) = print s;
val print_error = fn : string * 'a * 'b -> unit
- DamepoMLParser.parse(0,DamepoMLParser.makeLexer(DamepoMLLex.makeInputFromString "let val x = let val id: 'a -> 'a = fn z=>z in id id end in x end"),print_error,());
val it = (LetInExp ([ValDec #],VarExp (MkLongVId #)),-)
  : DamepoMLParser.result * 
    (DamepoMLParser.svalue,DamepoMLParser.pos) ?.LrParser.Token.token 
      ?.LrParser.stream
- val tree = #1 it;
val tree = LetInExp ([ValDec (#,#)],VarExp (MkLongVId (#,#)))
  : DamepoMLParser.result
- print(Syntax.print_Exp(PostParsing.scopeTyVarsInExp(Syntax.TyVarSet.empty, tree)));
LetInExp([ValDec([],PatBind(SimpleVIdPat(MkVId "x"),LetInExp([ValDec([MkTyVar "'a"],PatBind(TypedPat(SimpleVIdPat(MkVId "id"),FnType(TyVar(MkTyVar "'a"),TyVar(MkTyVar "'a"))),FnExp([(SimpleVIdPat(MkVId "z"),SimpleVarExp(MkVId "z"))]),NONE))],AppExp(SimpleVarExp(MkVId "id"),SimpleVarExp(MkVId "id"))),NONE))],SimpleVarExp(MkVId "x"))val it = () : unit
- DamepoMLParser.parse(0,DamepoMLParser.makeLexer(DamepoMLLex.makeInputFromString "let val x = case (let val id: 'a -> 'a = fn z=>z in id id end)of _ => fn z => z : 'a in x end"),print_error,());
val it = (LetInExp ([ValDec #],VarExp (MkLongVId #)),-)
  : DamepoMLParser.result * 
    (DamepoMLParser.svalue,DamepoMLParser.pos) ?.LrParser.Token.token 
      ?.LrParser.stream
- val tree2 = #1 it;
val tree2 = LetInExp ([ValDec (#,#)],VarExp (MkLongVId (#,#)))
  : DamepoMLParser.result
- print(Syntax.print_Exp(PostParsing.scopeTyVarsInExp(Syntax.TyVarSet.empty, tree2)));
LetInExp([ValDec([MkTyVar "'a"],PatBind(SimpleVIdPat(MkVId "x"),CaseExp(LetInExp([ValDec([],PatBind(TypedPat(SimpleVIdPat(MkVId "id"),FnType(TyVar(MkTyVar "'a"),TyVar(MkTyVar "'a"))),FnExp([(SimpleVIdPat(MkVId "z"),SimpleVarExp(MkVId "z"))]),NONE))],AppExp(SimpleVarExp(MkVId "id"),SimpleVarExp(MkVId "id"))),[(WildcardPat,FnExp([(SimpleVIdPat(MkVId "z"),TypedExp(SimpleVarExp(MkVId "z"),TyVar(MkTyVar "'a")))]))]),NONE))],SimpleVarExp(MkVId "x"))val it = () : unit
```
