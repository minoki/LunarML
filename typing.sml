structure Typing = struct
datatype TypeScheme = TypeScheme of Syntax.TyVar list * Syntax.Ty
datatype IdStatus = ValueVariable
                  | ValueConstructor
                  | ExceptionConstructor
type ValEnv = (TypeScheme * IdStatus) Syntax.VIdMap.map
type TyEnv = (unit) Syntax.TyConMap.map
datatype TyStr = TyStr (* of TypeFunction * ValEnv *)
datatype Env = Env of { typeEnv : TyEnv
                      , valEnv : ValEnv
                      , strEnv : StrEnv
                      , nextTyVarId : int ref
                      }
     and StrEnv = StrEnv of Env Syntax.StrIdMap.map
exception TypeError of string

fun newEmptyEnv() : Env
    = Env { typeEnv = Syntax.TyConMap.empty
          , valEnv = Syntax.VIdMap.empty
          , strEnv = StrEnv Syntax.StrIdMap.empty
          , nextTyVarId = ref 0
          }

fun freshTyVar(Env env) : Syntax.TyVar
    = let val nextTyVarId = #nextTyVarId env
          val i = !nextTyVarId
      in nextTyVarId := i + 1
       ; Syntax.MkTyVar("@" ^ Int.toString i)
      end

datatype ('tv, 'tycon) GConstraint
  = EqConstr of ('tv, 'tycon) Syntax.GTy * ('tv, 'tycon) Syntax.GTy (* ty1 = ty2 *)
  | FieldConstr of Syntax.Label * ('tv, 'tycon) Syntax.GTy * ('tv, 'tycon) Syntax.GTy (* ty1 = {label: ty2, ...} *)
(* | Is(Int|Word|Real|String|Char) of ('tv, 'tycon) GTy *)
(* IsWordInt|IsRealInt|IsNum|IsNumTxt *)

local open Syntax
in
(* unify : Env * Syntax.Ty * Syntax.Ty -> unit *)
(* constraints : Env * Syntax.Exp -> Constraint * Syntax.Ty *)
fun constraints(env, SConExp(IntegerConstant x))
    = ([], Syntax.TyCon([], MkLongTyCon([], MkTyCon "int"))) (* TODO: What if the global "int" is overridden? *) (* TODO: overloaded literals *)
  | constraints(env, SConExp(WordConstant x))
    = ([], Syntax.TyCon([], MkLongTyCon([], MkTyCon "word"))) (* TODO: What if the global "word" is overridden? *) (* TODO: overloaded literals *)
  | constraints(env, SConExp(RealConstant x))
    = ([], Syntax.TyCon([], MkLongTyCon([], MkTyCon "real"))) (* TODO: What if the global "real" is overridden? *) (* TODO: overloaded literals *)
  | constraints(env, SConExp(StringConstant x))
    = ([], Syntax.TyCon([], MkLongTyCon([], MkTyCon "string"))) (* TODO: What if the global "string" is overridden? *) (* TODO: overloaded literals *)
  | constraints(env, SConExp(CharacterConstant x))
    = ([], Syntax.TyCon([], MkLongTyCon([], MkTyCon "char"))) (* TODO: What if the global "char" is overridden? *) (* TODO: overloaded literals *)
  | constraints(env as Env {valEnv = valEnv, ...},
                VarExp(MkLongVId([], vid as MkVId vidstr)))
    = (case Syntax.VIdMap.find(valEnv, vid) of (* lookup the variable *)
          NONE => raise TypeError("variable not found: " ^ vidstr)
        | SOME (TypeScheme([], ty), ids) => ([], ty)
        | _ => raise TypeError("not impl")
      )
  | constraints(env, VarExp(longvid))
    = raise TypeError "longvid not implemented yet"
  | constraints(env, RecordExp(fields))
    = raise TypeError "record not implemented yet" (* record *)
  | constraints(env, LetInExp(decls, inner))
    = raise TypeError "let-in not implemented yet"
  | constraints(env, AppExp(f, x))
          (* f: s -> t, x: s *)
    = let val (ct1, funcTy) = constraints(env, f)
          val (ct2, argTy) = constraints(env, x)
          val retTy = TyVar(freshTyVar(env))
          (* funcTy = (argTy -> retTy) *)
          val ct = EqConstr(funcTy, FnType(argTy, retTy))
      in (ct :: (ct1 @ ct2), retTy)
      end
  | constraints(env, TypedExp(exp, ty))
    = let val (ct1, expTy) = constraints(env, exp)
          val ct = EqConstr(expTy, ty) (* ety = ty *)
      in (ct :: ct1, ty)
      end
  | constraints(env, HandleExp(e, handlers))
          (* e: t, handlers: exn -> t *)
    = raise TypeError "handle expression not implemented yet"
  | constraints(env, RaiseExp(exp))
    = let val (ct1, expTy) = constraints(env, exp)
          (* expTy = exn *)
          val ct = EqConstr(expTy, Syntax.TyCon([], MkLongTyCon([], MkTyCon "exn")))
          val retTy = TyVar(freshTyVar(env))
      in (ct :: ct1, retTy)
      end
  | constraints(env, IfThenElseExp(cond, thenPart, elsePart))
    = let val (ct1, condTy) = constraints(env, cond)
          val (ct2, thenTy) = constraints(env, thenPart)
          val (ct3, elseTy) = constraints(env, elsePart)
          (* condTy = bool *)
          val ect1 = EqConstr(condTy, Syntax.TyCon([], MkLongTyCon([], MkTyCon "bool")))
          (* thenTy = elseTy *)
          val ect2 = EqConstr(thenTy, elseTy)
      in (ect1 :: ect2 :: (ct1 @ ct2 @ ct3), thenTy)
      end
  | constraints(env, CaseExp(exp, matches))
    = raise TypeError "case expression not implemented yet"
  | constraints(env, FnExp([(VIdPat(MkLongVId([], vid)), body)]))
    = let val argTy = TyVar(freshTyVar(env))
          val (ct1, retTy) = constraints(env, body) (* TODO: Add vid to the environment *)
      in (ct1, Syntax.FnType(argTy, retTy))
      end
  | constraints(env, FnExp(matches))
    = raise TypeError "function expression not implemented yet"
end
end
