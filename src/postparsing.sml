structure Fixity = struct
type FixityStatusMap = Syntax.FixityStatus Syntax.VIdMap.map

fun getFixityStatus (map, vid) = case Syntax.VIdMap.find (map, vid) of
                                     SOME a => a
                                   | NONE => Syntax.Nonfix

datatype ('op, 'a) InfixList = Leaf of 'a
                             | Tree of 'a * Syntax.InfixAssociativity * 'op * ('op, 'a) InfixList
fun maxPrec(p, Leaf _) = p
  | maxPrec(p, Tree(_, Syntax.LeftAssoc q, _, rest)) = maxPrec(Int.max(p, q), rest)
  | maxPrec(p, Tree(_, Syntax.RightAssoc q, _, rest)) = maxPrec(Int.max(p, q), rest)
(* resolveFixity : ('a * 'op * 'a -> 'a) -> ('op, 'a) InfixList -> 'a *)
fun resolveFixity f
    = let fun go(Leaf x) = x
            | go(t as Tree(_, assoc, _, rest)) = let val p0 = case assoc of
                                                                  Syntax.LeftAssoc p0 => p0
                                                                | Syntax.RightAssoc p0 => p0
                                                     val prec = maxPrec(p0, rest)
                                                 in go(goPrec(prec, t)) end
          and goPrec(p, Leaf x) = Leaf x
            | goPrec(p, Tree(x, assoc as Syntax.LeftAssoc q, op_, rest))
              = if p = q then
                    goLeftAssoc(p, x, op_, rest)
                else (* p > q *)
                    Tree(x, assoc, op_, goPrec(p, rest))
            | goPrec(p, Tree(x, assoc as Syntax.RightAssoc q, op_, rest))
              = if p = q then
                    goRightAssoc(p, fn y => f(x, op_, y), rest)
                else (* p > q *)
                    Tree(x, assoc, op_, goPrec(p, rest))
          and goLeftAssoc(p, x, op_, Leaf y) = Leaf(f(x, op_, y))
            | goLeftAssoc(p, x, op_, Tree(y, assoc as Syntax.LeftAssoc q, op', rest))
              = if p = q then
                    goLeftAssoc(p, f(x, op_, y), op', rest)
                else (* p > q *)
                    Tree(f(x, op_, y), assoc, op', goPrec(p, rest))
            | goLeftAssoc(p, x, op_, Tree(y, assoc as Syntax.RightAssoc q, op', rest))
              = if p = q then
                    raise Fail "You cannot mix left-associative operators and right-associative operators of same precedence"
                else (* p > q *)
                    Tree(f(x, op_, y), assoc, op', goPrec(p, rest))
          and goRightAssoc(p, g, Leaf y) = Leaf(g y)
            | goRightAssoc(p, g, Tree(y, assoc as Syntax.LeftAssoc q, op', rest))
              = if p = q then
                    raise Fail "You cannot mix left-associative operators and right-associative operators of same precedence"
                else (* p > q *)
                    Tree(g y, assoc, op', goPrec(p, rest))
            | goRightAssoc(p, g, Tree(y, assoc as Syntax.RightAssoc q, op', rest))
              = if p = q then
                    goRightAssoc(p, fn z => g(f(y, op', z)), rest)
                else (* p > q *)
                    Tree(g y, assoc, op', goPrec(p, rest))
      in go
      end
(* let open Fixity in resolveFixity (fn (a,f,b) => f(a,b)) (Tree(3,Syntax.LeftAssoc 5,op +,Tree(2,Syntax.LeftAssoc 6,op *,Leaf 7))) end; should yield 17 *)

(* doPat : FixityStatusMap * UnfixedSyntax.Pat -> Syntax.Pat *)
(* doExp : FixityStatusMap * UnfixedSyntax.Exp -> Syntax.Exp *)
(* doDec : FixityStatusMap * UnfixedSyntax.Dec -> FixityStatusMap * Syntax.Dec *)
(* doDecs : FixityStatusMap * UnfixedSyntax.Dec list -> FixityStatusMap * Syntax.Dec list *)
(* doValBind : FixityStatusMap * UnfixedSyntax.ValBind -> Syntax.ValBind *)
fun doPat(env, UnfixedSyntax.WildcardPat) = Syntax.WildcardPat
  | doPat(env, UnfixedSyntax.SConPat scon) = Syntax.SConPat scon
  | doPat(env, UnfixedSyntax.InfixOrVIdPat(vid)) = (case getFixityStatus(env, vid) of
                                                        Syntax.Nonfix => Syntax.ConOrVarPat vid
                                                      | _ => raise Fail "infix operator used in non-infix position"
                                                   )
  | doPat(env, UnfixedSyntax.NonInfixVIdPat(Syntax.MkQualified([], vid))) = Syntax.ConOrVarPat vid
  | doPat(env, UnfixedSyntax.NonInfixVIdPat(longvid)) = Syntax.ConPat(longvid, NONE)
  | doPat(env, UnfixedSyntax.RecordPat(fields, r)) = Syntax.RecordPat(List.map (fn (label, pat) => (label, doPat(env, pat))) fields, r)
  | doPat(env, UnfixedSyntax.JuxtapositionPat patterns) (* constructed pattern or infix constructed pattern *)
    = let fun doPrefix(UnfixedSyntax.InfixOrVIdPat(vid) :: UnfixedSyntax.InfixOrVIdPat(vid') :: pats)
              = (case getFixityStatus(env, vid) of
                     Syntax.Nonfix => (case getFixityStatus(env, vid') of
                                           Syntax.Nonfix => doInfix(Syntax.ConPat(Syntax.MkLongVId([], vid), SOME(Syntax.ConOrVarPat(vid'))), pats)
                                         | Syntax.Infix assoc => Tree(Syntax.ConOrVarPat(vid), assoc, vid', doPrefix(pats))
                                      )
                   | Syntax.Infix assoc => raise Fail "infix operator used in prefix position"
                )
            | doPrefix(UnfixedSyntax.InfixOrVIdPat(vid) :: atpat :: pats)
              = (case getFixityStatus(env, vid) of
                     Syntax.Nonfix => doInfix(Syntax.ConPat(Syntax.MkLongVId([], vid), SOME(doPat(env, atpat))), pats)
                   | Syntax.Infix _ => raise Fail "infix operator used in prefix position"
                )
            | doPrefix(UnfixedSyntax.NonInfixVIdPat(longvid) :: UnfixedSyntax.InfixOrVIdPat(vid') :: pats)
              = (case getFixityStatus(env, vid') of
                     Syntax.Nonfix => doInfix(Syntax.ConPat(longvid, SOME(Syntax.ConOrVarPat(vid'))), pats)
                   | Syntax.Infix assoc => case longvid of
                                               Syntax.MkQualified([], vid) => Tree(Syntax.ConOrVarPat(vid), assoc, vid', doPrefix(pats))
                                             | _ => Tree(Syntax.ConPat(longvid, NONE), assoc, vid', doPrefix(pats))
                )
            | doPrefix(UnfixedSyntax.NonInfixVIdPat(longvid) :: atpat :: pats)
              = doInfix(Syntax.ConPat(longvid, SOME(doPat(env, atpat))), pats)
            | doPrefix(atpat :: UnfixedSyntax.InfixOrVIdPat(vid') :: pats)
              = (case getFixityStatus(env, vid') of
                     Syntax.Nonfix => raise Fail "invalid pattern"
                   | Syntax.Infix assoc => Tree(doPat(env, atpat), assoc, vid', doPrefix(pats))
                )
            | doPrefix(pat :: nil) = Leaf(doPat(env, pat))
            | doPrefix _ = raise Fail "invalid pattern"
          and doInfix(lhs : Syntax.Pat, UnfixedSyntax.InfixOrVIdPat(vid) :: pats)
              = (case getFixityStatus(env, vid) of
                     Syntax.Nonfix => raise Fail "invalid pattern"
                   | Syntax.Infix assoc => Tree(lhs, assoc, vid, doPrefix(pats))
                )
            | doInfix(lhs, nil) = Leaf lhs
            | doInfix(lhs, _) = raise Fail "invalid pattern"
      in resolveFixity Syntax.MkInfixConPat (doPrefix patterns)
      end
  | doPat(env, UnfixedSyntax.ConPat(longvid, pat)) = Syntax.ConPat(longvid, SOME(doPat(env, pat)))
  | doPat(env, UnfixedSyntax.TypedPat(pat, ty)) = Syntax.TypedPat(doPat(env, pat), ty)
  | doPat(env, UnfixedSyntax.LayeredPat(vid, ty, pat)) = Syntax.LayeredPat(vid, ty, doPat(env, pat))
  | doPat(env, UnfixedSyntax.ConjunctivePat(pat1, pat2)) = raise Fail "conjunctive: not implemented yet"
fun doExp(env, UnfixedSyntax.SConExp scon) = Syntax.SConExp scon
  | doExp(env, UnfixedSyntax.InfixOrVIdExp(vid)) = (case getFixityStatus(env, vid) of
                                                        Syntax.Nonfix => Syntax.VarExp(Syntax.MkLongVId([], vid))
                                                      | _ => raise Fail "infix operaor used in non-infix position"
                                                   )
  | doExp(env, UnfixedSyntax.NonInfixVIdExp(longvid)) = Syntax.VarExp longvid
  | doExp(env, UnfixedSyntax.RecordExp fields) = Syntax.RecordExp (List.map (fn (label, exp) => (label, doExp(env, exp))) fields)
  | doExp(env, UnfixedSyntax.LetInExp(decls, exp)) = let val (env', decls') = doDecs(env, decls)
                                                     in Syntax.LetInExp(decls', doExp(Syntax.VIdMap.unionWith #2 (env, env'), exp))
                                                     end
  | doExp(env, UnfixedSyntax.JuxtapositionExp expressions) (* application or infix application *)
    = let fun doPrefix(exp1 :: rest) = doInfix(doExp(env, exp1), rest)
            | doPrefix _ = raise Fail "invalid pattern"
          and doInfix(lhs : Syntax.Exp, UnfixedSyntax.InfixOrVIdExp(vid) :: rest)
              = (case getFixityStatus(env, vid) of
                     Syntax.Nonfix => doInfix(Syntax.AppExp(lhs, Syntax.VarExp(Syntax.MkLongVId([], vid))), rest)
                   | Syntax.Infix assoc => Tree(lhs, assoc, vid, doPrefix(rest))
                )
            | doInfix(lhs, x :: rest) = doInfix(Syntax.AppExp(lhs, doExp(env, x)), rest)
            | doInfix(lhs, nil) = Leaf lhs
      in resolveFixity Syntax.MkInfixExp (doPrefix expressions)
      end
  | doExp(env, UnfixedSyntax.AppExp(exp1, exp2)) = Syntax.AppExp(doExp(env, exp1), doExp(env, exp2))
  | doExp(env, UnfixedSyntax.TypedExp(exp, ty)) = Syntax.TypedExp(doExp(env, exp), ty)
  | doExp(env, UnfixedSyntax.HandleExp(exp, matches)) = Syntax.HandleExp(doExp(env, exp), List.map (fn (pat, exp') => (doPat(env, pat), doExp(env, exp'))) matches)
  | doExp(env, UnfixedSyntax.RaiseExp exp) = Syntax.RaiseExp(doExp(env, exp))
  | doExp(env, UnfixedSyntax.IfThenElseExp(e1, e2, e3)) = Syntax.IfThenElseExp(doExp(env, e1), doExp(env, e2), doExp(env, e3))
  | doExp(env, UnfixedSyntax.WhileDoExp(e1, e2)) = Syntax.WhileDoExp(doExp(env, e1), doExp(env, e2))
  | doExp(env, UnfixedSyntax.CaseExp(exp, matches)) = Syntax.CaseExp(doExp(env, exp), List.map (fn (pat, exp') => (doPat(env, pat), doExp(env, exp'))) matches)
  | doExp(env, UnfixedSyntax.FnExp matches) = Syntax.FnExp (List.map (fn (pat, exp) => (doPat(env, pat), doExp(env, exp))) matches)
  | doExp(env, UnfixedSyntax.ProjectionExp lab) = Syntax.ProjectionExp lab
and doDecs(env, nil) = (Syntax.VIdMap.empty, nil)
  | doDecs(env, dec :: decs) = let val (env', dec') = doDec(env, dec)
                                   val (env'', decs') = doDecs(Syntax.VIdMap.unionWith #2 (env, env'), decs)
                               in (Syntax.VIdMap.unionWith #2 (env', env''), dec' :: decs')
                               end
and doDec(env, UnfixedSyntax.ValDec(tyvars, valbind)) = (Syntax.VIdMap.empty, Syntax.ValDec(tyvars, List.map (fn vb => doValBind(env, vb)) valbind))
  | doDec(env, UnfixedSyntax.RecValDec(tyvars, valbind)) = (Syntax.VIdMap.empty, Syntax.RecValDec(tyvars, List.map (fn vb => doValBind(env, vb)) valbind))
  | doDec(env, UnfixedSyntax.FValDec(tyvars, fvalbind)) = (Syntax.VIdMap.empty, Syntax.FunDec(tyvars, List.map (fn fvb => doFValBind(env, fvb)) fvalbind))
  | doDec(env, UnfixedSyntax.TypeDec(typbinds)) = (Syntax.VIdMap.empty, Syntax.TypeDec(typbinds))
  | doDec(env, UnfixedSyntax.DatatypeDec(datbinds)) = (Syntax.VIdMap.empty, Syntax.DatatypeDec(datbinds))
  | doDec(env, UnfixedSyntax.DatatypeRepDec(tycon, longtycon)) = (Syntax.VIdMap.empty, Syntax.DatatypeRepDec(tycon, longtycon))
  | doDec(env, UnfixedSyntax.AbstypeDec(datbinds, decs)) = let val (_, decs') = doDecs(env, decs)
                                                           in (Syntax.VIdMap.empty, Syntax.AbstypeDec(datbinds, decs'))
                                                           end
  | doDec(env, UnfixedSyntax.ExceptionDec(exbinds)) = (Syntax.VIdMap.empty, Syntax.ExceptionDec(exbinds))
  | doDec(env, UnfixedSyntax.LocalDec(decs1, decs2)) = let val (env', decs1') = doDecs(env, decs1)
                                                           val (env'', decs2') = doDecs(Syntax.VIdMap.unionWith #2 (env, env'), decs2)
                                                       in (env'', Syntax.LocalDec(decs1', decs2'))
                                                       end
  | doDec(env, UnfixedSyntax.OpenDec strid) = (Syntax.VIdMap.empty, Syntax.OpenDec strid)
  | doDec(env, UnfixedSyntax.FixityDec(fixity, vids)) = (List.foldl (fn (vid, m) => Syntax.VIdMap.insert(m, vid, fixity)) Syntax.VIdMap.empty vids, Syntax.FixityDec(fixity, vids))
and doValBind(env, UnfixedSyntax.PatBind(pat, exp)) = Syntax.PatBind(doPat(env, pat), doExp(env, exp))
and doFValBind(env, UnfixedSyntax.FValBind(rules)) = let val rules' = List.map (fn rule => doFMRule (env, rule)) rules
                                                         fun getVIdAndArity (((vid, pats), _, _) :: xs) = checkVIdAndArity(vid, length pats, xs)
                                                           | getVIdAndArity [] = raise Fail "internal error: empty 'fun' rule"
                                                         and checkVIdAndArity(vid, arity, []) = (vid, arity)
                                                           | checkVIdAndArity(vid, arity, ((vid', pats), _, _) :: xs) = if vid = vid' then
                                                                                                                          if arity = length pats then
                                                                                                                              checkVIdAndArity(vid, arity, xs)
                                                                                                                          else
                                                                                                                              raise Syntax.SyntaxError "invalid 'fun' declaration: arity mismatch"
                                                                                                                      else
                                                                                                                          raise Syntax.SyntaxError "invalid 'fun' declaration: name mismatch"
                                                         val (vid, arity) = getVIdAndArity rules'
                                                     in Syntax.FValBind { vid = vid, arity = arity, rules = List.map (fn ((_, pats), optTy, exp) => (pats, optTy, exp)) rules' }
                                                     end
and doFMRule(env, UnfixedSyntax.FMRule(fpat, optTy, exp)) = (doFPat(env, fpat), optTy, doExp(env, exp))
and doFPat(env, UnfixedSyntax.FPat [UnfixedSyntax.JuxtapositionPat [pat1, UnfixedSyntax.InfixOrVIdPat vid, pat3]])
    = (case getFixityStatus(env, vid) of
           Syntax.Nonfix => raise Syntax.SyntaxError "invalid function declaration"
         | Syntax.Infix _ => doInfixFPat(env, vid, pat1, pat3, [])
      )
  | doFPat(env, UnfixedSyntax.FPat [pat1 as UnfixedSyntax.JuxtapositionPat [pat11, UnfixedSyntax.InfixOrVIdPat vid1, pat13], pat2 as UnfixedSyntax.InfixOrVIdPat vid2, pat3])
    = (case (getFixityStatus(env, vid1), getFixityStatus(env, vid2)) of
           (Syntax.Nonfix, Syntax.Nonfix) => raise Syntax.SyntaxError "invalid function declaration"
         | (Syntax.Infix _, Syntax.Nonfix) => doInfixFPat(env, vid1, pat11, pat13, [pat2, pat3])
         | (_, Syntax.Infix _) => doInfixFPat(env, vid2, pat1, pat3, [])
      )
  | doFPat(env, UnfixedSyntax.FPat (UnfixedSyntax.JuxtapositionPat [pat11, UnfixedSyntax.InfixOrVIdPat vid, pat13] :: pats))
    = (case getFixityStatus(env, vid) of
           Syntax.Nonfix => raise Syntax.SyntaxError "invalid function declaration"
         | Syntax.Infix _ => doInfixFPat(env, vid, pat11, pat13, pats)
      )
  | doFPat(env, UnfixedSyntax.FPat [pat1, pat2 as UnfixedSyntax.InfixOrVIdPat vid, pat3])
    = (case getFixityStatus(env, vid) of
           Syntax.Nonfix => (case pat1 of
                                 UnfixedSyntax.NonInfixVIdPat(Syntax.MkQualified([], vid')) => doPrefixFPat(env, vid', [pat2, pat3])
                               | UnfixedSyntax.InfixOrVIdPat(vid') => (case getFixityStatus(env, vid') of
                                                                           Syntax.Nonfix => doPrefixFPat(env, vid', [pat2, pat3])
                                                                         | Syntax.Infix _ => raise Syntax.SyntaxError "invalid function declaration"
                                                                      )
                               | _ => raise Syntax.SyntaxError "invalid function declaration"
                            )
         | Syntax.Infix _ => doInfixFPat(env, vid, pat1, pat3, [])
      )
  | doFPat(env, UnfixedSyntax.FPat (UnfixedSyntax.NonInfixVIdPat(Syntax.MkQualified([], vid)) :: pats)) = doPrefixFPat(env, vid, pats)
  | doFPat(env, UnfixedSyntax.FPat (UnfixedSyntax.InfixOrVIdPat(vid) :: pats))
    = (case getFixityStatus(env, vid) of
           Syntax.Nonfix => doPrefixFPat(env, vid, pats)
         | Syntax.Infix _ => raise Syntax.SyntaxError "invalid function declaration"
      )
  | doFPat(env, _)
    = raise Syntax.SyntaxError "invalid function declaration"
and doInfixFPat(env, vid, patL, patR, pats) = (vid, Syntax.TuplePat [doPat(env, patL), doPat(env, patR)] :: List.map (fn p => doPat(env, p)) pats)
and doPrefixFPat(env, vid, pats) = (vid, List.map (fn p => doPat(env, p)) pats)
end (* structure Fixity *)

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
  | freeTyVarsInPat(_, ConOrVarPat _) = TyVarSet.empty
  | freeTyVarsInPat(_, VarPat _) = TyVarSet.empty
  | freeTyVarsInPat(bound, RecordPat(xs, _)) = List.foldl (fn ((_, pat), set) => TyVarSet.union(freeTyVarsInPat(bound, pat), set)) TyVarSet.empty xs
  | freeTyVarsInPat(_, ConPat(_, NONE)) = TyVarSet.empty
  | freeTyVarsInPat(bound, ConPat(_, SOME pat)) = freeTyVarsInPat(bound, pat)
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
      | collectExp(bound, WhileDoExp(x, y)) = TyVarSet.union(collectExp(bound, x), collectExp(bound, y))
      | collectExp(bound, CaseExp(x, match)) = TyVarSet.union(collectExp(bound, x), collectMatch(bound, match))
      | collectExp(bound, FnExp match) = collectMatch(bound, match)
      | collectExp(bound, ProjectionExp lab) = TyVarSet.empty
    and collectMatch(bound, xs) = List.foldl (fn ((pat, e), set) => TyVarSet.union(freeTyVarsInPat(bound, pat), TyVarSet.union(collectExp(bound, e), set))) TyVarSet.empty xs
    and collectValBind(bound, PatBind(pat, e)) = TyVarSet.union(freeTyVarsInPat(bound, pat), collectExp(bound, e))
    and collectFValBind(bound, FValBind { rules = rules, ... }) = List.foldl (fn (rule, set) => TyVarSet.union(set, collectFRule(bound, rule))) TyVarSet.empty rules
    and collectFRule(bound, (pats, optTy, exp)) = let val tyVarsInPats = List.foldl TyVarSet.union TyVarSet.empty (List.map (fn pat => freeTyVarsInPat(bound, pat)) pats)
                                                      val tyVarsInOptTy = case optTy of
                                                                              NONE => TyVarSet.empty
                                                                            | SOME expTy => TyVarSet.difference(freeTyVarsInTy(bound, expTy), bound)
                                                  in union3(tyVarsInPats, tyVarsInOptTy, collectExp(bound, exp))
                                                  end
in
val unguardedTyVarsInExp : TyVarSet.set * Exp -> TyVarSet.set = collectExp
val unguardedTyVarsInValBind : TyVarSet.set * ValBind list -> TyVarSet.set = fn (bound, valbinds) => List.foldl (fn (valbind, set) => TyVarSet.union(set, collectValBind(bound, valbind))) TyVarSet.empty valbinds
val unguardedTyVarsInFValBind : TyVarSet.set * FValBind list -> TyVarSet.set = fn (bound, fvalbinds) => List.foldl (fn (fvalbind, set) => TyVarSet.union(set, collectFValBind(bound, fvalbind))) TyVarSet.empty fvalbinds
end (* local *)

(* The Definition 4.6 *)
(* scopeTyVarsInDec: TyVarSet.set * Dec -> Dec *)
local
    fun doDec(bound, ValDec(expbound, valbind)) = let val bound' = TyVarSet.addList(bound, expbound)
                                                      val unguarded = unguardedTyVarsInValBind(bound', valbind)
                                                      val expbound' = TyVarSet.listItems(unguarded)
                                                      val bound'' = TyVarSet.union(bound', unguarded)
                                                  in ValDec(expbound', List.map (fn vb => doValBind(bound'', vb)) valbind)
                                                  end
      | doDec(bound, RecValDec(expbound, valbind)) = let val bound' = TyVarSet.addList(bound, expbound)
                                                         val unguarded = unguardedTyVarsInValBind(bound', valbind)
                                                         val expbound' = TyVarSet.listItems(unguarded)
                                                         val bound'' = TyVarSet.union(bound', unguarded)
                                                     in RecValDec(expbound', List.map (fn vb => doValBind(bound'', vb)) valbind)
                                                     end
      | doDec(bound, FunDec(expbound, fvalbind)) = let val bound' = TyVarSet.addList(bound, expbound)
                                                       val unguarded = unguardedTyVarsInFValBind(bound', fvalbind)
                                                       val expbound' = TyVarSet.listItems(unguarded)
                                                       val bound'' = TyVarSet.union(bound', unguarded)
                                                   in FunDec(expbound', List.map (fn fvb => doFValBind(bound'', fvb)) fvalbind)
                                                   end
      | doDec(bound, dec as TypeDec _) = dec
      | doDec(bound, dec as DatatypeDec _) = dec
      | doDec(bound, dec as DatatypeRepDec _) = dec
      | doDec(bound, dec as AbstypeDec _) = dec
      | doDec(bound, dec as ExceptionDec _) = dec
      | doDec(bound, LocalDec(xs, ys)) = LocalDec(doDecList(bound, xs), doDecList(bound, ys))
      | doDec(bound, dec as OpenDec _) = dec
      | doDec(bound, dec as FixityDec _) = dec
    and doDecList(bound, decls) = List.map (fn x => doDec(bound, x)) decls
    and doValBind(bound, PatBind(pat, e)) = PatBind(pat, doExp(bound, e))
    and doFValBind(bound, FValBind { vid = vid, arity = arity, rules = rules }) = let fun doFRule(pats, optTy, exp) = (pats, optTy, doExp(bound, exp))
                                                                                  in FValBind { vid = vid, arity = arity, rules = List.map doFRule rules }
                                                                                  end
    and doExp(bound, exp as SConExp _) = exp
      | doExp(bound, exp as VarExp _) = exp
      | doExp(bound, exp as RecordExp _) = exp
      | doExp(bound, LetInExp(decls, exp)) = LetInExp(doDecList(bound, decls), doExp(bound, exp))
      | doExp(bound, AppExp(x, y)) = AppExp(doExp(bound, x), doExp(bound, y))
      | doExp(bound, TypedExp(x, ty)) = TypedExp(doExp(bound, x), ty)
      | doExp(bound, HandleExp(x, match)) = HandleExp(doExp(bound, x), doMatch(bound, match))
      | doExp(bound, RaiseExp(x)) = RaiseExp(doExp(bound, x))
      | doExp(bound, IfThenElseExp(x, y, z)) = IfThenElseExp(doExp(bound, x), doExp(bound, y), doExp(bound, z))
      | doExp(bound, WhileDoExp(x, y)) = WhileDoExp(doExp(bound, x), doExp(bound, y))
      | doExp(bound, CaseExp(x, match)) = CaseExp(doExp(bound, x), doMatch(bound, match))
      | doExp(bound, FnExp(match)) = FnExp(doMatch(bound, match))
      | doExp(bound, exp as ProjectionExp _) = exp
    and doMatch(bound, xs) = List.map (fn (pat, exp) => (pat, doExp(bound, exp))) xs
in
val scopeTyVarsInDecs: TyVarSet.set * Dec list -> Dec list = doDecList
val scopeTyVarsInDec: TyVarSet.set * Dec -> Dec = doDec
val scopeTyVarsInExp: TyVarSet.set * Exp -> Exp = doExp
end (* local *)
end (* local *)

(* Check syntactic restrictions (The Definition 2.9) *)
local
    structure S = Syntax

    (* checkRow : (Label * 'a) list -> bool, returns true if the same label is bound twice *)
    fun checkRow(row: (S.Label * 'a) list) = doCheckRow(S.LabelSet.empty, row)
    and doCheckRow(seen, []) = false
      | doCheckRow(seen, (label, _) :: xs) = if S.LabelSet.member(seen, label) then
                                                 true
                                             else
                                                 doCheckRow(S.LabelSet.add(seen, label), xs)

    (* checkTyVarSeq : TyVar list -> bool, returns true if the same TyVar is bound twice *)
    fun checkTyVarSeq(xs: S.TyVar list) = doCheckTyVarSeq(S.TyVarSet.empty, xs)
    and doCheckTyVarSeq(seen, []) = false
      | doCheckTyVarSeq(seen, tv :: xs) = if S.TyVarSet.member(seen, tv) then
                                              true
                                          else
                                              doCheckTyVarSeq(S.TyVarSet.add(seen, tv), xs)


    (* doTy : S.Ty -> unit *)
    fun doTy(S.TyVar _) = ()
      | doTy(S.RecordType row) = if checkRow row then
                                     raise S.SyntaxError "No type-expression row may bind the same label twice"
                                 else
                                     ()
      | doTy(S.TyCon(arg, longtycon)) = ()
      | doTy(S.FnType(s, t)) = (doTy s ; doTy t)

    (* doPat : S.Pat -> unit *)
    fun doPat(S.WildcardPat) = ()
      | doPat(S.SConPat(S.RealConstant _)) = raise S.SyntaxError "No real constant may occur in a pattern"
      | doPat(S.SConPat _) = ()
      | doPat(S.ConOrVarPat _) = ()
      | doPat(S.VarPat _) = ()
      | doPat(S.RecordPat(row, ellip)) = if checkRow row then
                                             raise S.SyntaxError "No pattern row may bind the same label twice"
                                         else
                                             List.app (fn (_, pat) => doPat pat) row
      | doPat(S.ConPat(_, NONE)) = ()
      | doPat(S.ConPat(longvid, SOME pat)) = doPat pat
      | doPat(S.TypedPat(pat, ty)) = (doPat pat ; doTy ty)
      | doPat(S.LayeredPat(vid, NONE, pat)) = (doPat pat)
      | doPat(S.LayeredPat(vid, SOME ty, pat)) = (doTy ty ; doPat pat)

    fun doTypBind(S.TypBind(tyvarseq, tycon, ty))
        = if checkTyVarSeq tyvarseq then
              raise S.SyntaxError "No tyvarseq may contain the same tyvar twice"
          else
              doTy ty

    val invalidBoundNames = List.foldl S.VIdSet.add' S.VIdSet.empty [S.MkVId "true", S.MkVId "false", S.MkVId "nil", S.MkVId "::", S.MkVId "ref"]

    (* doExBind : S.DatBind -> unit *)
    fun doDatBind(S.DatBind(tyvarseq, tycon, conbinds))
        = if checkTyVarSeq tyvarseq then
              raise S.SyntaxError "No tyvarseq may contain the same tyvar twice"
          else
              doConBinds(S.VIdSet.empty, conbinds)
    and doConBinds(seen, []) = ()
      | doConBinds(seen, S.ConBind(vid, tyopt) :: conbinds) = if S.VIdSet.member(seen, vid) then
                                                                  raise S.SyntaxError "No datbind may bind the same identifier twice"
                                                              else
                                                                  if vid = S.MkVId "it" orelse S.VIdSet.member(invalidBoundNames, vid) then
                                                                      raise S.SyntaxError "Invalid bound name"
                                                                  else
                                                                      doConBinds(S.VIdSet.add(seen, vid), conbinds)

    (* doExBind : S.ExBind -> unit *)
    fun doExBind(_: S.ExBind): unit = raise Fail "not implemented yet"

    (* doExp : S.TyVarSet * S.Exp -> unit *)
    (* doDec : S.TyVarSet * S.Dec -> unit *)
    (* doValBind : S.TyVarSet * S.ValBind -> unit *)
    fun doExp(tyvarenv : S.TyVarSet.set, S.SConExp _) = ()
      | doExp(tyvarenv, S.VarExp _) = ()
      | doExp(tyvarenv, S.RecordExp row) = if checkRow row then
                                               raise S.SyntaxError "No expression row may bind the same label twice"
                                           else
                                               ()
      | doExp(tyvarenv, S.LetInExp(decls, exp)) = (List.app (fn dec => doDec(tyvarenv, dec)) decls ; doExp(tyvarenv, exp))
      | doExp(tyvarenv, S.AppExp(e1, e2)) = (doExp(tyvarenv, e1) ; doExp(tyvarenv, e2))
      | doExp(tyvarenv, S.TypedExp(exp, ty)) = (doExp(tyvarenv, exp) ; doTy ty)
      | doExp _ = raise Fail "not implemented yet"
    and doDec(tyvarenv : S.TyVarSet.set, S.ValDec(tyvarseq, valbind)) = raise Fail "not implemented yet"
      | doDec(tyvarenv : S.TyVarSet.set, S.RecValDec(tyvarseq, valbind)) = raise Fail "not implemented yet"
      | doDec _ = raise Fail "not implemented yet"
    and doValBind(tyvarenv, S.PatBind(pat, exp)) = (doPat pat ; doExp(tyvarenv, exp))
in
val checkExp = doExp
val checkDec = doDec
end (* local *)
end (* structure PostParsing *)
