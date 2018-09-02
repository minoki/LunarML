structure PostParsing = struct
(* structure TyVarSet = Syntax.TyVarSet *)
local
    open Syntax
in
(* freeTyVarsInTy : TyVarSet * Ty -> TyVarSet *)
fun freeTyVarsInTy(bound, TyVar tv) = if TyVarSet.member(bound, tv) then
                                          TyVarSet.empty
                                      else
                                          TyVarSet.singleton tv
  | freeTyVarsInTy(bound, RecordType xs) = List.foldl (fn ((_, ty), set) => TyVarSet.union(freeTyVarsInTy(bound, ty), set)) TyVarSet.empty xs
  | freeTyVarsInTy(bound, TyCon(xs,_)) = List.foldl (fn (ty,set) => TyVarSet.union(freeTyVarsInTy(bound, ty), set)) TyVarSet.empty xs
  | freeTyVarsInTy(bound, FnType(s,t)) = TyVarSet.union(freeTyVarsInTy(bound, s), freeTyVarsInTy(bound, t))

(* freeTyVarsInPat : TyVarSet * Pat -> TyVarSet *)
fun freeTyVarsInPat(_, WildcardPat) = TyVarSet.empty
  | freeTyVarsInPat(_, SConPat _) = TyVarSet.empty
  | freeTyVarsInPat(_, VIdPat _) = TyVarSet.empty
  | freeTyVarsInPat(bound, RecordPat(xs, _)) = List.foldl (fn ((_, pat), set) => TyVarSet.union(freeTyVarsInPat(bound, pat), set)) TyVarSet.empty xs
  | freeTyVarsInPat(bound, ConPat(_, pat)) = freeTyVarsInPat(bound, pat)
  | freeTyVarsInPat(bound, TypedPat(pat, ty)) = TyVarSet.union(freeTyVarsInPat(bound, pat), freeTyVarsInTy(bound, ty))
  | freeTyVarsInPat(bound, LayeredPat(_, SOME ty, pat)) = TyVarSet.union(freeTyVarsInTy(bound, ty), freeTyVarsInPat(bound, pat))
  | freeTyVarsInPat(bound, LayeredPat(_, NONE, pat)) = freeTyVarsInPat(bound, pat)

(* unguardedTyVarsInValBind : TyVarSet.set * ValBind -> TyVarSet.set *)
local
    fun union3(x, y, z) = TyVarSet.union(x, TyVarSet.union(y, z))
    fun collectExp(_, SConExp _) = TyVarSet.empty
      | collectExp(_, VarExp _) = TyVarSet.empty
      | collectExp(bound, RecordExp xs) = List.foldl (fn ((_, e), set) => TyVarSet.union(collectExp(bound, e), set)) TyVarSet.empty xs
      | collectExp(bound, LetInExp(_, e)) = collectExp(bound, e) (* declarations are not examined *)
      | collectExp(bound, AppExp(x, y)) = TyVarSet.union(collectExp(bound, x), collectExp(bound,y))
      | collectExp(bound, TypedExp(x, ty)) = TyVarSet.union(collectExp(bound, x), TyVarSet.difference(freeTyVarsInTy(bound, ty), bound))
      | collectExp(bound, HandleExp(x, match)) = TyVarSet.union(collectExp(bound, x), collectMatch(bound, match))
      | collectExp(bound, RaiseExp x) = collectExp(bound, x)
      | collectExp(bound, IfThenElseExp(x, y, z)) = union3(collectExp(bound, x), collectExp(bound, y), collectExp(bound, z))
      | collectExp(bound, CaseExp(x, match)) = TyVarSet.union(collectExp(bound, x), collectMatch(bound, match))
      | collectExp(bound, FnExp match) = collectMatch(bound, match)
    and collectMatch(bound, xs) = List.foldl (fn ((pat, e), set) => TyVarSet.union(freeTyVarsInPat(bound, pat), TyVarSet.union(collectExp(bound, e), set))) TyVarSet.empty xs
    and collectValBind(bound, PatBind(pat, e, SOME rest)) = union3(freeTyVarsInPat(bound, pat), collectExp(bound, e), collectValBind(bound, rest))
      | collectValBind(bound, PatBind(pat, e, NONE)) = TyVarSet.union(freeTyVarsInPat(bound, pat), collectExp(bound, e))
      | collectValBind(bound, RecValBind(valbind)) = collectValBind(bound, valbind)
in
val unguardedTyVarsInExp : TyVarSet.set * Exp -> TyVarSet.set = collectExp
val unguardedTyVarsInValBind : TyVarSet.set * ValBind -> TyVarSet.set = collectValBind
end

(* The Definition 4.6 *)
(* scopeTyVarsInDec: TyVarSet.set * Dec -> Dec *)
local
    fun doDec(bound, ValDec(expbound, valbind)) = let val bound' = TyVarSet.addList(bound, expbound)
                                                      val unguarded = unguardedTyVarsInValBind(bound', valbind)
                                                      val expbound' = TyVarSet.listItems(unguarded)
                                                      val bound'' = TyVarSet.union(bound', unguarded)
                                                  in ValDec(expbound', doValBind(bound'', valbind))
                                                  end
      | doDec(bound, dec as TypeDec _) = dec
      | doDec(bound, dec as DatatypeDec _) = dec
      | doDec(bound, dec as DatatypeRepDec _) = dec
      | doDec(bound, dec as AbstypeDec _) = dec
      | doDec(bound, dec as ExceptionDec _) = dec
      | doDec(bound, LocalDec(xs, ys)) = LocalDec(doDecList(bound, xs), doDecList(bound, ys))
      | doDec(bound, dec as OpenDec _) = dec
      | doDec(bound, dec as InfixDec _) = dec
      | doDec(bound, dec as InfixrDec _) = dec
      | doDec(bound, dec as NonfixDec _) = dec
    and doDecList(bound, decls) = List.map (fn x => doDec(bound, x)) decls
    and doValBind(bound, PatBind(pat, e, SOME valbind)) = PatBind(pat, doExp(bound, e), SOME(doValBind(bound, valbind)))
      | doValBind(bound, PatBind(pat, e, NONE)) = PatBind(pat, doExp(bound, e), NONE)
      | doValBind(bound, RecValBind(valbind)) = RecValBind(doValBind(bound, valbind))
    and doExp(bound, exp as SConExp _) = exp
      | doExp(bound, exp as VarExp _) = exp
      | doExp(bound, exp as RecordExp _) = exp
      | doExp(bound, LetInExp(decls, exp)) = LetInExp(doDecList(bound, decls), doExp(bound, exp))
      | doExp(bound, AppExp(x, y)) = AppExp(doExp(bound, x), doExp(bound, y))
      | doExp(bound, TypedExp(x, ty)) = TypedExp(doExp(bound, x), ty)
      | doExp(bound, HandleExp(x, match)) = HandleExp(doExp(bound, x), doMatch(bound, match))
      | doExp(bound, RaiseExp(x)) = RaiseExp(doExp(bound, x))
      | doExp(bound, IfThenElseExp(x, y, z)) = IfThenElseExp(doExp(bound, x), doExp(bound, y), doExp(bound, z))
      | doExp(bound, CaseExp(x, match)) = CaseExp(doExp(bound, x), doMatch(bound, match))
      | doExp(bound, FnExp(match)) = FnExp(doMatch(bound, match))
    and doMatch(bound, xs) = List.map (fn (pat, exp) => (pat, doExp(bound, exp))) xs
in
val scopeTyVarsInDec: TyVarSet.set * Dec -> Dec = doDec
val scopeTyVarsInExp: TyVarSet.set * Exp -> Exp = doExp
end
end
end
