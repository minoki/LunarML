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
type Constraint = (Syntax.TyVar, Syntax.LongTyCon) GConstraint

local open Syntax
      val PrimTy_int = Syntax.TyCon([], MkLongTyCon([], MkTyCon "int"))
      val PrimTy_word = Syntax.TyCon([], MkLongTyCon([], MkTyCon "word"))
      val PrimTy_real = Syntax.TyCon([], MkLongTyCon([], MkTyCon "real"))
      val PrimTy_string = Syntax.TyCon([], MkLongTyCon([], MkTyCon "string"))
      val PrimTy_char = Syntax.TyCon([], MkLongTyCon([], MkTyCon "char"))
      val PrimTy_exn = Syntax.TyCon([], MkLongTyCon([], MkTyCon "exn"))
      val PrimTy_bool = Syntax.TyCon([], MkLongTyCon([], MkTyCon "bool"))
in
(* constraints : Env * Syntax.Exp -> Constraint list * Syntax.Ty *)
fun constraints(env, SConExp(IntegerConstant x))   = ([], PrimTy_int) (* TODO: overloaded literals *)
  | constraints(env, SConExp(WordConstant x))      = ([], PrimTy_word) (* TODO: overloaded literals *)
  | constraints(env, SConExp(RealConstant x))      = ([], PrimTy_real) (* TODO: overloaded literals *)
  | constraints(env, SConExp(StringConstant x))    = ([], PrimTy_string) (* TODO: overloaded literals *)
  | constraints(env, SConExp(CharacterConstant x)) = ([], PrimTy_char) (* TODO: overloaded literals *)
  | constraints(env as Env {valEnv = valEnv, ...},
                VarExp(MkLongVId([], vid as MkVId vidstr)))
    = (case Syntax.VIdMap.find(valEnv, vid) of (* lookup the variable *)
          NONE => raise TypeError("variable not found: " ^ vidstr)
        | SOME (TypeScheme([], ty), ids) => ([], ty)
        | _ => raise TypeError("not impl")
      )
  | constraints(env, VarExp(longvid))
    = raise TypeError "longvid not implemented yet"
  | constraints(env, RecordExp(row))
    = let val (ct, row') = constraintsFromRow(env, row)
      in (ct, RecordType(row'))
      end
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
          val ct = EqConstr(expTy, PrimTy_exn)
          val retTy = TyVar(freshTyVar(env))
      in (ct :: ct1, retTy)
      end
  | constraints(env, IfThenElseExp(cond, thenPart, elsePart))
    = let val (ct1, condTy) = constraints(env, cond)
          val (ct2, thenTy) = constraints(env, thenPart)
          val (ct3, elseTy) = constraints(env, elsePart)
          (* condTy = bool *)
          val ect1 = EqConstr(condTy, PrimTy_bool)
          (* thenTy = elseTy *)
          val ect2 = EqConstr(thenTy, elseTy)
      in (ect1 :: ect2 :: (ct1 @ ct2 @ ct3), thenTy)
      end
  | constraints(env, CaseExp(exp, matches))
    = let val (ct1, expTy) = constraints(env, exp)
          val (ct2, patTy, retTy) = constraintsFromMatch(env, matches)
      in (EqConstr(expTy, patTy) :: (ct1 @ ct2), retTy)
      end
  | constraints(env, FnExp([(VIdPat(MkLongVId([], vid)), body)]))
    = let val argTy = TyVar(freshTyVar(env))
          val (ct1, retTy) = constraints(env, body) (* TODO: Add vid to the environment *)
      in (ct1, Syntax.FnType(argTy, retTy))
      end
  | constraints(env, FnExp(matches))
    = let val (ct, argTy, retTy) = constraintsFromMatch(env, matches)
      in (ct, Syntax.FnType(argTy, retTy))
      end
 (* constraintsFromRow : Env * (Label * Exp) list -> Constraint list * (Label * Syntax.Ty) list *)
and constraintsFromRow(env, xs)
    = let fun oneField(label, exp) = let val (ct, ty) = constraints(env, exp)
                                     in (ct, (label, ty))
                                     end
          val (cts, row) = ListPair.unzip (List.map oneField xs)
      in (List.concat cts, row)
      end
 (* constraintsFromMatch : Env * (Pat * Exp) list -> Constraint list * Syntax.Ty * Syntax.Ty *)
and constraintsFromMatch(env, (pat0, exp0) :: rest)
    = let val (ct0p, patTy) = constraintsFromPat(env, pat0)
          val (ct0e, expTy) = constraints(env, exp0)
          fun oneBranch(pat, exp) = let val (ctp, patTy) = constraintsFromPat(env, pat)
                                        val (cte, expTy) = constraints(env, exp)
                                    in ctp @ cte
                                    end
          val cts = List.map oneBranch rest
      in (ct0p @ ct0e @ List.concat cts, patTy, expTy)
      end
  | constraintsFromMatch(env, nil) = raise TypeError "invalid syntax tree: match is empty"
 (* constraintsFromPat : Env * Pat -> Constraint list * Syntax.Ty *)
and constraintsFromPat(env, WildcardPat)
    = let val ty = TyVar(freshTyVar(env))
      in ([], ty)
      end
  | constraintsFromPat(env, SConPat(IntegerConstant(_)))   = ([], PrimTy_int)
  | constraintsFromPat(env, SConPat(WordConstant(_)))      = ([], PrimTy_word)
  | constraintsFromPat(env, SConPat(RealConstant(_)))      = ([], PrimTy_real)
  | constraintsFromPat(env, SConPat(StringConstant(_)))    = ([], PrimTy_string)
  | constraintsFromPat(env, SConPat(CharacterConstant(_))) = ([], PrimTy_char)
  | constraintsFromPat(env, VIdPat(longvid)) = raise TypeError "VIdPat: not implemented yet"
  | constraintsFromPat(env, RecordPat(row, wildcard))
    = let val (ct, row') = constraintsFromPatRow(env, row)
      in if wildcard then
             let val recordTy = TyVar(freshTyVar(env))
                 fun oneField(label, ty) = FieldConstr(label, recordTy, ty)
                 val fieldCts = List.map oneField row'
             in (fieldCts @ ct, recordTy)
             end
         else
             (ct, RecordType(row'))
      end
  | constraintsFromPat(env, ConPat(_, _)) = raise TypeError "ConPat"
  | constraintsFromPat(env, TypedPat(pat, ty))
    = let val (ct, inferredTy) = constraintsFromPat(env, pat)
      in (EqConstr(ty, inferredTy) :: ct, ty)
      end
  | constraintsFromPat(env, LayeredPat(_, SOME ty, pat))
    = let val (ct, inferredTy) = constraintsFromPat(env, pat)
      in (EqConstr(ty, inferredTy) :: ct, ty)
      end
  | constraintsFromPat(env, LayeredPat(_, NONE, pat)) = constraintsFromPat(env, pat)
 (* constraintsFromPatRow : Env * (Label * Pat) list -> Constraint list * (Label * Syntax.Ty) list *)
and constraintsFromPatRow(env, xs)
    = let fun oneField(label, pat) = let val (ct, ty) = constraintsFromPat(env, pat)
                                     in (ct, (label, ty))
                                     end
          val (cts, row) = ListPair.unzip (List.map oneField xs)
      in (List.concat cts, row)
      end
end
end
