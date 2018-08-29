structure Typing = struct
datatype TypeScheme = TypeScheme
datatype IdStatus = ValueVariable
                  | ValueConstructor
                  | ExceptionConstructor
datatype VEnv = MkVEnv of (TypeScheme * IdStatus) Syntax.VIdMap.map
datatype TyEnv = MkTyEnv of (unit) Syntax.TyConMap.map
(* infer : Env * Syntax.Exp -> unit *)
local open Syntax
in
fun infer (env,SConExp(IntegerConstant x))
    = () (* IsInteger 'a, 'a *)
  | infer (env,SConExp(WordConstant x))
    = ()
  | infer (env,SConExp(RealConstant x))
    = ()
  | infer (env,SConExp(StringConstant x))
    = ()
  | infer (env,SConExp(CharacterConstant x))
    = ()
  | infer (env,VarExp(lvid))
    = () (* lookup the variable *)
  | infer (env,RecordExp(fields))
    = () (* record variable *)
  | infer (env,LetInExp(decls,inner))
    = ()
  | infer (env,AppExp(f,x))
    = () (* apply *)
  | infer (env,TypedExp(e,t))
    = () (* unify *)
  | infer (env,HandleExp(e,handlers))
    = ()
  | infer (env,RaiseExp(e))
    = ()
  | infer (env,IfThenElseExp(cond,thenPart,elsePart))
    = ()
  | infer (env,CaseExp(exp,matches))
    = ()
  | infer (env,FnExp(matches))
    = ()
end
end
