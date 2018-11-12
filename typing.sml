structure Typing = struct
datatype TypeScheme = TypeScheme of Syntax.TyVar list * Syntax.Ty
datatype IdStatus = ValueVariable
                  | ValueConstructor
                  | ExceptionConstructor
type ValEnv = (TypeScheme * IdStatus) Syntax.VIdMap.map
type TyConEnv = (int) Syntax.TyConMap.map
datatype TyStr = TyStr (* of TypeFunction * ValEnv *)
datatype Env = MkEnv of { tyConMap : TyConEnv (* Syntax.TyCon -> (tycon id) *)
                        , valMap : ValEnv (* Syntax.VId -> (type scheme * id status) *)
                        , strMap : Env Syntax.StrIdMap.map
                        }
type Context = { nextTyVar : int ref
               , env : Env
               }
exception TypeError of string
exception NameError of string

fun lookupStr(env, nil) = env
  | lookupStr(env as MkEnv { strMap = strMap, ... }, (str0 as Syntax.MkStrId name) :: str1)
    = (case Syntax.StrIdMap.find(strMap, str0) of
           NONE => raise NameError("unknown structure name " ^ name)
         | SOME innerEnv => lookupStr(innerEnv, str1)
      )
fun lookupTyConInEnv(MkEnv env, tycon as Syntax.MkTyCon name)
    = (case Syntax.TyConMap.find(#tyConMap env, tycon) of
           NONE => raise NameError("unknown type constructor " ^ name)
         | SOME x => x
      )
fun lookupValInEnv(MkEnv env, vid as Syntax.MkVId name)
    = (case Syntax.VIdMap.find(#valMap env, vid) of
           NONE => raise NameError("unknown value name " ^ name)
         | SOME x => x
      )

fun emptyEnv() : Env
    = MkEnv { tyConMap = Syntax.TyConMap.empty
            , valMap = Syntax.VIdMap.empty
            , strMap = Syntax.StrIdMap.empty
            }
fun newContext(env : Env) : Context
    = { env = env
      , nextTyVar = ref 100
      }

fun freshTyVar(ctx : Context) : USyntax.TyVar
    = let val nextTyVar = #nextTyVar ctx
          val i = !nextTyVar
      in nextTyVar := i + 1
       ; USyntax.UTyVar(Syntax.MkTyVar "_", i)
      end

datatype Constraint
  = EqConstr of USyntax.Ty * USyntax.Ty (* ty1 = ty2 *)
  | FieldConstr of { label : Syntax.Label
                   , recordTy : USyntax.Ty
                   , fieldTy : USyntax.Ty
                   } (* recordTy = {label: fieldTy, ...} *)
  | IsEqType of USyntax.Ty
(* | Is(Int|Word|Real|String|Char) of USyntax.Ty *)
(* IsWordInt|IsRealInt|IsNum|IsNumTxt *)

local open USyntax
      val PrimTy_int    = USyntax.TyCon([], USyntax.ULongTyCon(Syntax.MkLongTyCon([], Syntax.MkTyCon "int"), 0))
      val PrimTy_word   = USyntax.TyCon([], USyntax.ULongTyCon(Syntax.MkLongTyCon([], Syntax.MkTyCon "word"), 0))
      val PrimTy_real   = USyntax.TyCon([], USyntax.ULongTyCon(Syntax.MkLongTyCon([], Syntax.MkTyCon "real"), 0))
      val PrimTy_string = USyntax.TyCon([], USyntax.ULongTyCon(Syntax.MkLongTyCon([], Syntax.MkTyCon "string"), 0))
      val PrimTy_char   = USyntax.TyCon([], USyntax.ULongTyCon(Syntax.MkLongTyCon([], Syntax.MkTyCon "char"), 0))
      val PrimTy_exn    = USyntax.TyCon([], USyntax.ULongTyCon(Syntax.MkLongTyCon([], Syntax.MkTyCon "exn"), 0))
      val PrimTy_bool   = USyntax.TyCon([], USyntax.ULongTyCon(Syntax.MkLongTyCon([], Syntax.MkTyCon "bool"), 0))
      val PrimTyCon_ref   = USyntax.ULongTyCon(Syntax.MkLongTyCon([], Syntax.MkTyCon "ref"), 0)
      fun lookupVariable(env) = raise TypeError "not implemented yet"
in
(* constraints : Context * USyntax.Exp -> Constraint list * USyntax.Ty *)
fun constraints(ctx, SConExp(Syntax.IntegerConstant x))   = ([], PrimTy_int) (* TODO: overloaded literals *)
  | constraints(ctx, SConExp(Syntax.WordConstant x))      = ([], PrimTy_word) (* TODO: overloaded literals *)
  | constraints(ctx, SConExp(Syntax.RealConstant x))      = ([], PrimTy_real) (* TODO: overloaded literals *)
  | constraints(ctx, SConExp(Syntax.StringConstant x))    = ([], PrimTy_string) (* TODO: overloaded literals *)
  | constraints(ctx, SConExp(Syntax.CharacterConstant x)) = ([], PrimTy_char) (* TODO: overloaded literals *)
  | constraints(ctx, VarExp(Syntax.MkLongVId(str, vid as Syntax.MkVId name)))
    = (case lookupValInEnv(lookupStr(#env ctx, str), vid) of
          (TypeScheme([], ty), ids) => raise TypeError("not impl")
        | _ => raise TypeError("not impl")
      )
  | constraints(ctx, RecordExp(row))
    = let val (ct, row') = constraintsFromRow(ctx, row)
      in (ct, RecordType(row'))
      end
  | constraints(ctx, LetInExp(decls, inner))
    = raise TypeError "let-in not implemented yet"
  | constraints(ctx, AppExp(f, x))
          (* f: s -> t, x: s *)
    = let val (ct1, funcTy) = constraints(ctx, f)
          val (ct2, argTy) = constraints(ctx, x)
          val retTy = TyVar(freshTyVar(ctx))
          (* funcTy = (argTy -> retTy) *)
          val ct = EqConstr(funcTy, FnType(argTy, retTy))
      in (ct :: (ct1 @ ct2), retTy)
      end
  | constraints(ctx, TypedExp(exp, ty))
    = let val (ct1, expTy) = constraints(ctx, exp)
          val ct = EqConstr(expTy, ty) (* ety = ty *)
      in (ct :: ct1, ty)
      end
  | constraints(ctx, HandleExp(e, handlers))
          (* e: t, handlers: exn -> t *)
    = raise TypeError "handle expression not implemented yet"
  | constraints(ctx, RaiseExp(exp))
    = let val (ct1, expTy) = constraints(ctx, exp)
          (* expTy = exn *)
          val ct = EqConstr(expTy, PrimTy_exn)
          val retTy = TyVar(freshTyVar(ctx))
      in (ct :: ct1, retTy)
      end
  | constraints(ctx, IfThenElseExp(cond, thenPart, elsePart))
    = let val (ct1, condTy) = constraints(ctx, cond)
          val (ct2, thenTy) = constraints(ctx, thenPart)
          val (ct3, elseTy) = constraints(ctx, elsePart)
          (* condTy = bool *)
          val ect1 = EqConstr(condTy, PrimTy_bool)
          (* thenTy = elseTy *)
          val ect2 = EqConstr(thenTy, elseTy)
      in (ect1 :: ect2 :: (ct1 @ ct2 @ ct3), thenTy)
      end
  | constraints(ctx, CaseExp(exp, matches))
    = let val (ct1, expTy) = constraints(ctx, exp)
          val (ct2, patTy, retTy) = constraintsFromMatch(ctx, matches)
      in (EqConstr(expTy, patTy) :: (ct1 @ ct2), retTy)
      end
  | constraints(ctx, FnExp([(VIdPat(Syntax.MkLongVId([], vid)), body)]))
    = let val argTy = TyVar(freshTyVar(ctx))
          val (ct1, retTy) = constraints(ctx, body) (* TODO: Add vid to the ctx *)
      in (ct1, USyntax.FnType(argTy, retTy))
      end
  | constraints(ctx, FnExp(matches))
    = let val (ct, argTy, retTy) = constraintsFromMatch(ctx, matches)
      in (ct, USyntax.FnType(argTy, retTy))
      end
 (* constraintsFromRow : Ctx * (Label * Exp) list -> Constraint list * (Label * Syntax.Ty) list *)
and constraintsFromRow(ctx, xs)
    = let fun oneField(label, exp) = let val (ct, ty) = constraints(ctx, exp)
                                     in (ct, (label, ty))
                                     end
          val (cts, row) = ListPair.unzip (List.map oneField xs)
      in (List.concat cts, row)
      end
 (* constraintsFromMatch : Ctx * (Pat * Exp) list -> Constraint list * Syntax.Ty * Syntax.Ty *)
and constraintsFromMatch(ctx, (pat0, exp0) :: rest)
    = let val (ct0p, patTy) = constraintsFromPat(ctx, pat0)
          val (ct0e, expTy) = constraints(ctx, exp0)
          fun oneBranch(pat, exp) = let val (ctp, patTy) = constraintsFromPat(ctx, pat)
                                        val (cte, expTy) = constraints(ctx, exp)
                                    in ctp @ cte
                                    end
          val cts = List.map oneBranch rest
      in (ct0p @ ct0e @ List.concat cts, patTy, expTy)
      end
  | constraintsFromMatch(ctx, nil) = raise TypeError "invalid syntax tree: match is empty"
 (* constraintsFromPat : Ctx * Pat -> Constraint list * Syntax.Ty *)
and constraintsFromPat(ctx, WildcardPat)
    = let val ty = TyVar(freshTyVar(ctx))
      in ([], ty)
      end
  | constraintsFromPat(ctx, SConPat(Syntax.IntegerConstant(_)))   = ([], PrimTy_int)
  | constraintsFromPat(ctx, SConPat(Syntax.WordConstant(_)))      = ([], PrimTy_word)
  | constraintsFromPat(ctx, SConPat(Syntax.RealConstant(_)))      = ([], PrimTy_real)
  | constraintsFromPat(ctx, SConPat(Syntax.StringConstant(_)))    = ([], PrimTy_string)
  | constraintsFromPat(ctx, SConPat(Syntax.CharacterConstant(_))) = ([], PrimTy_char)
  | constraintsFromPat(ctx, VIdPat(longvid)) = raise TypeError "VIdPat: not implemented yet"
  | constraintsFromPat(ctx, RecordPat(row, wildcard))
    = let val (ct, row') = constraintsFromPatRow(ctx, row)
      in if wildcard then
             let val recordTy = TyVar(freshTyVar(ctx))
                 fun oneField(label, ty) = FieldConstr { label = label, recordTy = recordTy, fieldTy = ty }
                 val fieldCts = List.map oneField row'
             in (fieldCts @ ct, recordTy)
             end
         else
             (ct, RecordType(row'))
      end
  | constraintsFromPat(ctx, ConPat(_, _)) = raise TypeError "ConPat"
  | constraintsFromPat(ctx, TypedPat(pat, ty))
    = let val (ct, inferredTy) = constraintsFromPat(ctx, pat)
      in (EqConstr(ty, inferredTy) :: ct, ty)
      end
  | constraintsFromPat(ctx, LayeredPat(_, SOME ty, pat))
    = let val (ct, inferredTy) = constraintsFromPat(ctx, pat)
      in (EqConstr(ty, inferredTy) :: ct, ty)
      end
  | constraintsFromPat(ctx, LayeredPat(_, NONE, pat)) = constraintsFromPat(ctx, pat)
 (* constraintsFromPatRow : Ctx * (Label * Pat) list -> Constraint list * (Label * Syntax.Ty) list *)
and constraintsFromPatRow(ctx, xs)
    = let fun oneField(label, pat) = let val (ct, ty) = constraintsFromPat(ctx, pat)
                                     in (ct, (label, ty))
                                     end
          val (cts, row) = ListPair.unzip (List.map oneField xs)
      in (List.concat cts, row)
      end

local (* occurCheck : TyVar -> Ty -> bool; returns true if the type variable occurs in the type *)
      fun occurCheck tv = let fun check (TyVar tv') = tv = tv'
                                | check (RecordType xs) = List.exists (fn (label, ty) => check ty) xs
                                | check (TyCon(tyargs, longtycon)) = List.exists check tyargs
                                | check (FnType(ty1, ty2)) = check ty1 orelse check ty2
                          in check
                          end
      (* substituteTy : TyVar * Ty -> Ty -> Ty *)
      fun substituteTy (tv, replacement) =
          let fun substTy (ty as TyVar tv') = if tv = tv' then
                                                  replacement
                                              else
                                                  ty
                | substTy (RecordType fields) = RecordType (List.map (fn (label, ty) => (label, substTy ty)) fields)
                | substTy (TyCon(tyargs, longtycon)) = TyCon(List.map substTy tyargs, longtycon)
                | substTy (FnType(ty1, ty2)) = FnType(substTy ty1, substTy ty2)
          in substTy
          end
      (* substituteConstraint : TyVar * Ty -> Constraint -> Constraint *)
      fun substituteConstraint (tv, replacement) =
          let val substTy = substituteTy (tv, replacement)
          in fn EqConstr(ty1, ty2) => EqConstr(substTy ty1, substTy ty2)
              | FieldConstr{label = label, recordTy = recordTy, fieldTy = fieldTy } => FieldConstr{label = label, recordTy = substTy recordTy, fieldTy = substTy fieldTy}
              | IsEqType ty => IsEqType(substTy ty)
          end
in
type Subst = (TyVar * Ty) list

 (* unify : Context * Constraint list -> Subst *)
fun unify(ctx, EqConstr(TyVar(tv), ty) :: ctrs)
    = if occurCheck tv ty then
          raise TypeError("unification failed: occurrence check")
      else
          (tv, ty) :: unify(ctx, List.map (substituteConstraint (tv, ty)) ctrs)
  | unify(ctx, EqConstr(ty, TyVar(tv)) :: ctrs)
    = if occurCheck tv ty then
          raise TypeError("unification failed: occurrence check")
      else
          (tv, ty) :: unify(ctx, List.map (substituteConstraint (tv, ty)) ctrs)
  | unify(ctx, EqConstr(FnType(s0, s1), FnType(t0, t1)) :: ctrs) = unify(ctx, EqConstr(s0, t0) :: EqConstr(s1, t1) :: ctrs)
  | unify(ctx, EqConstr(RecordType(fields), RecordType(fields')) :: ctrs)
    = if List.length fields <> List.length fields then
          raise TypeError("unification failed: incompatible record types (different number of fields)")
      else
          unify(ctx, List.foldl (fn ((label, ty), acc) => case List.find (fn (label', _) => label = label') fields' of
                                                              NONE => raise TypeError("unification failed: incompatible record types")
                                                            | SOME(_,ty') => EqConstr(ty, ty') :: acc) ctrs fields)
  | unify(ctx, EqConstr(TyCon(tyarg, con), TyCon(tyarg', con')) :: ctrs)
    = if con = con' then
          unify(ctx, ListPair.mapEq EqConstr (tyarg, tyarg') @ ctrs)
          handle ListPair.UnequalLengths => raise TypeError("unification failed: the number of type arguments differ")
      else
          raise TypeError("unification failed: type constructor mismatch")
  | unify(ctx, EqConstr(_, _) :: ctrs) = raise TypeError("unification failed: not match")
  | unify(ctx, FieldConstr{label = label, recordTy = RecordType(fields), fieldTy = fieldTy} :: ctrs)
    = (case List.find (fn (label', _) => label = label') fields of
           NONE => raise TypeError("unification failed: no field")
         | SOME(_, ty') => unify(ctx, EqConstr(fieldTy, ty') :: ctrs)
      )
  | unify(ctx, FieldConstr{label = label, recordTy = TyCon(_, _), fieldTy = fieldTy} :: ctrs) = raise TypeError("record field for a non-record type")
  | unify(ctx, FieldConstr{label = label, recordTy = FnType(_, _), fieldTy = fieldTy} :: ctrs) = raise TypeError("record field for a function type")
  | unify(ctx, FieldConstr{label = label, recordTy = TyVar tv, fieldTy = fieldTy} :: ctrs) = raise TypeError("not impl")
  | unify(ctx, IsEqType(RecordType fields) :: ctrs) = unify(ctx, List.map (fn (label, ty) => IsEqType ty) fields @ ctrs)
  | unify(ctx, IsEqType(FnType _) :: ctrs) = raise TypeError("function type does not admit equality")
  | unify(ctx, IsEqType(TyCon(tyargs, longtycon)) :: ctrs)
    = if longtycon = PrimTyCon_ref then
          unify(ctx, ctrs)
      else
          (* (longtycon???) : List.map IsEqType tyargs @ ctrs *)
          raise TypeError("not impl")
  | unify(ctx, IsEqType(TyVar(_)) :: ctrs) = raise TypeError("not impl")
  | unify(ctx, nil) = nil
end
end
end

