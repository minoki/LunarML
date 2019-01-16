structure USyntax = struct
datatype UTyVar = UTyVar of Syntax.TyVar * int
datatype UTyCon = UTyCon of Syntax.TyCon * int
datatype ULongTyCon = ULongTyCon of Syntax.LongTyCon * int
structure SyntaxTree = Syntax.GenericSyntaxTree(type TyVar = UTyVar
                                                type TyCon = UTyCon
                                                type LongTyCon = ULongTyCon
                                                fun print_TyVar(UTyVar(tv, n)) = "UTyVar(" ^ Syntax.print_TyVar tv ^ "," ^ Int.toString n ^ ")"
                                                fun print_TyCon(UTyCon(tycon, n)) = "UTyCon(" ^ Syntax.print_TyCon tycon ^ "," ^ Int.toString n ^ ")"
                                                fun print_LongTyCon(ULongTyCon(longtycon, n)) = "ULongTyCon(" ^ Syntax.print_LongTyCon longtycon ^ "," ^ Int.toString n ^ ")"
                                               )
type TyVar = UTyVar
type TyCon = UTyCon
type LongTyCon = ULongTyCon
open SyntaxTree

exception NotImpl of string

(* mapTyInExp : (Ty -> Ty) -> Exp -> Exp *)
fun mapTyInExp doTy =
    (* assumes that doTy only acts on type variables *)
    let fun doExp(e as SConExp _) = e
          | doExp(e as VarExp _) = e
          | doExp(RecordExp fields) = RecordExp(List.map (fn (label, exp) => (label, doExp exp)) fields)
          | doExp(LetInExp(decls, e)) = LetInExp(List.map doDec decls, doExp e)
          | doExp(AppExp(e1, e2)) = AppExp(doExp e1, doExp e2)
          | doExp(TypedExp(e, ty)) = TypedExp(doExp e, doTy ty)
          | doExp(HandleExp(e, matches)) = HandleExp(doExp e, List.map doMatch matches)
          | doExp(RaiseExp e) = RaiseExp(doExp e)
          | doExp(IfThenElseExp(e1, e2, e3)) = IfThenElseExp(doExp e1, doExp e2, doExp e3)
          | doExp(CaseExp(e, matches)) = CaseExp(doExp e, List.map doMatch matches)
          | doExp(FnExp matches) = FnExp(List.map doMatch matches)
        and doDec(ValDec(tyvars, valbind)) = ValDec(tyvars, doValBind valbind)
          | doDec(TypeDec _) = raise NotImpl "doDec(TypeDec) not implemented yet"
          | doDec(DatatypeDec _) = raise NotImpl "doDec(DatatypeDec) not implemented yet"
          | doDec(DatatypeRepDec _) = raise NotImpl "doDec(DatatypeRepDec) not implemented yet"
          | doDec(AbstypeDec _) = raise NotImpl "doDec(AbstypeDec) not implemented yet"
          | doDec(ExceptionDec _) = raise NotImpl "doDec(ExceptionDec) not implemented yet"
          | doDec(LocalDec _) = raise NotImpl "doDec(LocalDec) not implemented yet"
          | doDec(OpenDec _) = raise NotImpl "doDec(OpenDec) not implemented yet"
          | doDec(dec as FixityDec _) = dec
        and doValBind(PatBind(pat, exp, opt)) = PatBind(doPat pat, doExp exp, Option.map doValBind opt)
          | doValBind(RecValBind valbind) = RecValBind(doValBind valbind)
        and doMatch(pat, exp) = (doPat pat, doExp exp)
        and doPat WildcardPat = WildcardPat
          | doPat(s as SConPat _) = s
          | doPat(s as VIdPat _) = s
          | doPat(RecordPat(xs, xt)) = RecordPat(List.map (fn (label, pat) => (label, doPat pat)) xs, xt)
          | doPat(ConPat(ct, pat)) = ConPat(ct, doPat pat)
          | doPat(TypedPat(pat, ty)) = TypedPat(doPat pat, doTy ty)
          | doPat(LayeredPat(vid, tyopt, pat)) = LayeredPat(vid, Option.map doTy tyopt, doPat pat)
    in doExp
    end
end (* structure USyntax *)

structure Fixity = struct
type FixityStatusMap = Syntax.FixityStatus Syntax.VIdMap.map
(*
Top-level environment:
infix  7  * / div mod
infix  6  + - ^
infixr 5  :: @
infix  4  = <> > >= < <=
infix  3  := o
infix  0  before
*)
val initialFixity = let open Syntax
                        fun InfixL p = Syntax.Infix (Syntax.LeftAssoc p)
                        fun InfixR p = Syntax.Infix (Syntax.RightAssoc p)
                    in List.foldl VIdMap.insert' VIdMap.empty
                                  [(MkVId "*",  InfixL 7)
                                  ,(MkVId "/",  InfixL 7)
                                  ,(MkVId "div",InfixL 7)
                                  ,(MkVId "mod",InfixL 7)
                                  ,(MkVId "+",  InfixL 6)
                                  ,(MkVId "-",  InfixL 6)
                                  ,(MkVId "^",  InfixL 6) (* string concatenation *)
                                  ,(MkVId "::", InfixR 5)
                                  ,(MkVId "@",  InfixR 5) (* list concatenation *)
                                  ,(MkVId "=",  InfixL 4)
                                  ,(MkVId "<>", InfixL 4)
                                  ,(MkVId ">",  InfixL 4)
                                  ,(MkVId ">=", InfixL 4)
                                  ,(MkVId "<",  InfixL 4)
                                  ,(MkVId "<=", InfixL 4)
                                  ,(MkVId ":=", InfixL 3)
                                  ,(MkVId "o",  InfixL 3)
                                  ,(MkVId "before",InfixL 3)
                                  ]
                    end
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
                                                        Syntax.Nonfix => Syntax.VIdPat(Syntax.MkLongVId([], vid))
                                                      | _ => raise Fail "infix operator used in non-infix position"
                                                   )
  | doPat(env, UnfixedSyntax.NonInfixVIdPat(longvid)) = Syntax.VIdPat longvid
  | doPat(env, UnfixedSyntax.RecordPat(fields, r)) = Syntax.RecordPat(List.map (fn (label, pat) => (label, doPat(env, pat))) fields, r)
  | doPat(env, UnfixedSyntax.JuxtapositionPat patterns) (* constructed pattern or infix constructed pattern *)
    = let fun doPrefix(UnfixedSyntax.InfixOrVIdPat(vid) :: UnfixedSyntax.InfixOrVIdPat(vid') :: pats)
              = (case getFixityStatus(env, vid) of
                     Syntax.Nonfix => (case getFixityStatus(env, vid') of
                                           Syntax.Nonfix => doInfix(Syntax.ConPat(Syntax.MkLongVId([], vid), Syntax.VIdPat(Syntax.MkLongVId([], vid'))), pats)
                                         | Syntax.Infix assoc => Tree(Syntax.VIdPat(Syntax.MkLongVId([], vid)), assoc, vid', doPrefix(pats))
                                      )
                   | Syntax.Infix assoc => raise Fail "infix operator used in prefix position"
                )
            | doPrefix(UnfixedSyntax.InfixOrVIdPat(vid) :: atpat :: pats)
              = (case getFixityStatus(env, vid) of
                     Syntax.Nonfix => doInfix(Syntax.ConPat(Syntax.MkLongVId([], vid), doPat(env, atpat)), pats)
                   | Syntax.Infix _ => raise Fail "infix operator used in prefix position"
                )
            | doPrefix(UnfixedSyntax.NonInfixVIdPat(longvid) :: UnfixedSyntax.InfixOrVIdPat(vid') :: pats)
              = (case getFixityStatus(env, vid') of
                     Syntax.Nonfix => doInfix(Syntax.ConPat(longvid, Syntax.VIdPat(Syntax.MkLongVId([], vid'))), pats)
                   | Syntax.Infix assoc => Tree(Syntax.VIdPat(longvid), assoc, vid', doPrefix(pats))
                )
            | doPrefix(UnfixedSyntax.NonInfixVIdPat(longvid) :: atpat :: pats)
              = doInfix(Syntax.ConPat(longvid, doPat(env, atpat)), pats)
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
  (* | doPat(env, UnfixedSyntax.ParenthesizedPat pat) = doPat(env, pat) *)
  | doPat(env, UnfixedSyntax.TypedPat(pat, ty)) = Syntax.TypedPat(doPat(env, pat), ty)
  | doPat(env, UnfixedSyntax.LayeredPat(vid, ty, pat)) = Syntax.LayeredPat(vid, ty, doPat(env, pat))
fun doExp(env, UnfixedSyntax.SConExp scon) = Syntax.SConExp scon
  | doExp(env, UnfixedSyntax.InfixOrVIdExp(vid)) = (case getFixityStatus(env, vid) of
                                                        Syntax.Nonfix => Syntax.VarExp(Syntax.MkLongVId([], vid))
                                                      | _ => raise Fail "infix operaor used in non-infix position"
                                                   )
  | doExp(env, UnfixedSyntax.NonInfixVIdExp(longvid)) = Syntax.VarExp longvid
  | doExp(env, UnfixedSyntax.RecordExp fields) = Syntax.RecordExp (List.map (fn (label, exp) => (label, doExp(env, exp))) fields)
  | doExp(env, UnfixedSyntax.LetInExp(decls, exp)) = let val (env', decls') = doDecs(env, decls)
                                                     in Syntax.LetInExp(decls', doExp(env', exp))
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
  (* | doExp(env, UnfixedSyntax.ParenthesizedExp exp) = doExp(env, exp) *)
  | doExp(env, UnfixedSyntax.TypedExp(exp, ty)) = Syntax.TypedExp(doExp(env, exp), ty)
  | doExp(env, UnfixedSyntax.HandleExp(exp, matches)) = Syntax.HandleExp(doExp(env, exp), List.map (fn (pat, exp') => (doPat(env, pat), doExp(env, exp'))) matches)
  | doExp(env, UnfixedSyntax.RaiseExp exp) = Syntax.RaiseExp(doExp(env, exp))
  | doExp(env, UnfixedSyntax.IfThenElseExp(e1, e2, e3)) = Syntax.IfThenElseExp(doExp(env, e1), doExp(env, e2), doExp(env, e3))
  | doExp(env, UnfixedSyntax.CaseExp(exp, matches)) = Syntax.CaseExp(doExp(env, exp), List.map (fn (pat, exp') => (doPat(env, pat), doExp(env, exp'))) matches)
  | doExp(env, UnfixedSyntax.FnExp matches) = Syntax.FnExp (List.map (fn (pat, exp) => (doPat(env, pat), doExp(env, exp))) matches)
and doDecs(env, nil) = (Syntax.VIdMap.empty, nil)
  | doDecs(env, dec :: decs) = let val (env', dec') = doDec(env, dec)
                                   val (env'', decs') = doDecs(Syntax.VIdMap.unionWith #2 (env, env'), decs)
                               in (Syntax.VIdMap.unionWith #2 (env', env''), dec' :: decs')
                               end
and doDec(env, UnfixedSyntax.ValDec(tyvars, valbind)) = (Syntax.VIdMap.empty, Syntax.ValDec(tyvars, doValBind(env, valbind)))
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
and doValBind(env, UnfixedSyntax.PatBind(pat, exp, SOME valbind)) = Syntax.PatBind(doPat(env, pat), doExp(env, exp), SOME (doValBind(env, valbind)))
  | doValBind(env, UnfixedSyntax.PatBind(pat, exp, NONE)) = Syntax.PatBind(doPat(env, pat), doExp(env, exp), NONE)
  | doValBind(env, UnfixedSyntax.RecValBind valbind) = Syntax.RecValBind(doValBind(env, valbind))
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
end (* local *)

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
      | doDec(bound, dec as FixityDec _) = dec
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
end (* local *)
end (* local *)

exception NameError of string

datatype BoundTyCon = BTyAlias of USyntax.TyVar list * USyntax.Ty
                    | BTyCon of int
datatype ConEnv = MkConEnv of { tyConMap : BoundTyCon Syntax.TyConMap.map
                              , strMap : ConEnv Syntax.StrIdMap.map
                              }
type Context = { nextTyVar : int ref
               , nextTyCon : int ref
               , tyVarMap : int Syntax.TyVarMap.map
               , conEnv : ConEnv
               }

val emptyConEnv : ConEnv
    = MkConEnv { tyConMap = Syntax.TyConMap.empty
               , strMap = Syntax.StrIdMap.empty
               }
fun newContext(conEnv : ConEnv) : Context
    = { nextTyVar = ref 100
      , nextTyCon = ref 100
      , tyVarMap = Syntax.TyVarMap.empty
      , conEnv = conEnv
      }

local structure S = Syntax
      structure U = USyntax

      fun genTyVarId(ctx : Context)
          = let val id = !(#nextTyVar ctx)
            in #nextTyVar ctx := id + 1 ; id end
      fun genTyVar(ctx, tv) = USyntax.UTyVar(tv, genTyVarId(ctx))
      fun freshTyVar(ctx : Context) = genTyVar(ctx, Syntax.MkTyVar "_")

      fun genTyConId(ctx : Context)
          = let val id = !(#nextTyCon ctx)
            in #nextTyCon ctx := id + 1 ; id end
      fun genTyCon(ctx, tycon) = USyntax.UTyCon(tycon, genTyConId(ctx))

      fun lookupTyVar(ctx : Context, tv as Syntax.MkTyVar name)
          = case Syntax.TyVarMap.find(#tyVarMap ctx, tv) of
                NONE => raise NameError("unknown type variable " ^ name)
              | SOME id => USyntax.UTyVar(tv, id)

      fun lookupTyConInConEnv(MkConEnv env, tycon as Syntax.MkTyCon name)
          = case Syntax.TyConMap.find(#tyConMap env, tycon) of
                NONE => raise NameError("unknown type constructor " ^ name)
              | SOME b => b
      fun lookupTyCon(ctx : Context, tycon) = lookupTyConInConEnv(#conEnv ctx, tycon)

      fun lookupStr(env, nil) = env
        | lookupStr(env as MkConEnv { strMap = strMap, ... }, (str0 as S.MkStrId name) :: str1)
          = case Syntax.StrIdMap.find(strMap, str0) of
                NONE => raise NameError("unknown structure name " ^ name)
              | SOME innerEnv => lookupStr(innerEnv, str1)

      fun lookupLongTyConInConEnv(env, Syntax.MkLongTyCon(strpath, tycon)) = lookupTyConInConEnv(lookupStr(env, strpath), tycon)
      fun lookupLongTyCon(ctx : Context, longtycon) = lookupLongTyConInConEnv(#conEnv ctx, longtycon)
in
(* toUTy : Context * Syntax.Ty -> USyntax.Ty *)
(* toUTyRow : Context * (Label * Syntax.Ty) list -> (Label * USyntax.Ty) list *)
(* toUPat : Context * Syntax.Pat -> USyntax.Pat *)
(* toUPatRow : Context * (Label * USyntax.Pat) list -> (Label * USyntax.Pat) list *)
(* toUTypBind : Context * Syntax.TypBind -> USyntax.TypBind *)
(* toUConBind : Context * Syntax.ConBind -> USyntax.ConBind *)
(* toUDatBind : Context * Syntax.DatBind -> USyntax.DatBind *)
(* toUExBind : Context * Syntax.ExBind -> USyntax.ExBind *)
(* toUExp : Context * Syntax.Exp -> USyntax.Exp *)
(* toUMatch : Context * (Syntax.Pat * Syntax.Exp) list -> (USyntax.Pat * USyntax.Exp) list *)
(* toUDec : Context * Syntax.Dec -> USyntax.Dec *)
fun toUTy(ctx, S.TyVar tv) = U.TyVar(genTyVar(ctx, tv))
  | toUTy(ctx, S.RecordType row) = U.RecordType(toUTyRow(ctx, row))
  | toUTy(ctx, S.TyCon(args, tycon)) = (case lookupLongTyCon(ctx, tycon) of
                                            BTyCon id => U.TyCon(List.map (fn ty => toUTy(ctx, ty)) args, U.ULongTyCon(tycon, id))
                                          | BTyAlias _ => raise NameError "type alias not supported yet"
                                       )
  | toUTy(ctx, S.FnType(ty1, ty2)) = U.FnType(toUTy(ctx, ty1), toUTy(ctx, ty2))
and toUTyRow(ctx, row) = let fun oneField(label, ty) = (label, toUTy(ctx, ty))
                         in List.map oneField row
                         end
fun toUPat(ctx, S.WildcardPat) = U.WildcardPat (* TODO: should generate a type id? *)
  | toUPat(ctx, S.SConPat sc) = U.SConPat sc
  | toUPat(ctx, S.VIdPat longvid) = U.TypedPat(U.VIdPat longvid, USyntax.TyVar(freshTyVar(ctx))) (* TODO: identifier status *)
  | toUPat(ctx, S.RecordPat(row, wildcard)) = U.RecordPat(toUPatRow(ctx, row), wildcard)
  | toUPat(ctx, S.ConPat(longvid, pat)) = U.ConPat(longvid, toUPat(ctx, pat))
  | toUPat(ctx, S.TypedPat(S.VIdPat longvid, ty)) = U.TypedPat(U.VIdPat longvid, toUTy(ctx, ty))
  | toUPat(ctx, S.TypedPat(pat, ty)) = U.TypedPat(toUPat(ctx, pat), toUTy(ctx, ty))
  | toUPat(ctx, S.LayeredPat(vid, SOME ty, pat)) = U.LayeredPat(vid, SOME(toUTy(ctx, ty)), toUPat(ctx, pat))
  | toUPat(ctx, S.LayeredPat(vid, NONE, pat)) = U.LayeredPat(vid, NONE, toUPat(ctx, pat))
and toUPatRow(ctx, _) = raise NameError("not implemented yet")
fun toUTypBind(ctx, S.TypBind(params, tycon, ty)) = (* let val params' = List.map (fn ty => genTyVar(ctx, ty)) params
                                                        val tycon' = 
                                                  in U.TypBind(params', tycon *) raise NameError("not implemented yet")
fun toUConBind(ctx, S.ConBind(vid, opt_ty)) = raise NameError("not implemented yet")
fun toUDatBind(ctx, S.DatBind(params, tycon, conbinds)) = raise NameError("not implemented yet") (* genTyCon *)
fun toUExBind(ctx, _ : S.ExBind) = raise NameError("not implemented yet")
fun toUExp(ctx, S.SConExp(scon)) = U.SConExp(scon)
  | toUExp(ctx, S.VarExp(longvid)) = U.VarExp(longvid)
  | toUExp(ctx, S.RecordExp(row)) = raise NameError("not implemented yet")
  | toUExp(ctx, S.LetInExp(decls, exp))
    = let fun doDecl(ctx, nil, acc) = (ctx, List.rev acc)
            | doDecl(ctx, decl :: decls, acc)
              = (case decl of
                     S.ValDec(_) => doDecl(ctx, decls, toUDec(ctx, decl) :: acc)
                   | S.TypeDec(typbinds) => doDecl((* TODO: add type ctor *) ctx, decls, toUDec(ctx, decl) :: acc)
                   | S.DatatypeDec(datbinds) => doDecl((* TODO: add type ctor *) ctx, decls, toUDec(ctx, decl) :: acc)
                   | S.DatatypeRepDec(tycon, longtycon) => doDecl((* TODO: add type ctor *) ctx, decls, toUDec(ctx, decl) :: acc)
                   | S.AbstypeDec(datbinds, dec) => doDecl((* TODO: add type ctor *) ctx, decls, toUDec(ctx, decl) :: acc)
                   | S.ExceptionDec(_) => doDecl(ctx, decls, toUDec(ctx, decl) :: acc)
                   | S.LocalDec(_) => doDecl((* TODO: add type ctor *) ctx, decls, toUDec(ctx, decl) :: acc)
                   | S.OpenDec(_) => doDecl((* TODO: add type ctor *) ctx, decls, toUDec(ctx, decl) :: acc)
                   | S.FixityDec(_) => doDecl(ctx, decls, toUDec(ctx, decl) :: acc)
                )
          val (ctx', decls') = doDecl(ctx, decls, nil)
      in U.LetInExp(decls', toUExp(ctx', exp))
      end
  | toUExp(ctx, S.AppExp(exp1, exp2)) = U.AppExp(toUExp(ctx, exp1), toUExp(ctx, exp2))
  | toUExp(ctx, S.TypedExp(exp, ty)) = U.TypedExp(toUExp(ctx, exp), toUTy(ctx, ty))
  | toUExp(ctx, S.HandleExp(exp, ty)) = raise NameError("not implemented yet")
  | toUExp(ctx, S.RaiseExp(exp)) = U.RaiseExp(toUExp(ctx, exp))
  | toUExp(ctx, S.IfThenElseExp(exp1, exp2, exp3)) = U.IfThenElseExp(toUExp(ctx, exp1), toUExp(ctx, exp2), toUExp(ctx, exp3))
  | toUExp(ctx, S.CaseExp(exp, match)) = U.CaseExp(toUExp(ctx, exp), toUMatch(ctx, match))
  | toUExp(ctx, S.FnExp(match)) = U.FnExp(toUMatch(ctx, match))
and toUMatch(ctx, matches : (S.Pat * S.Exp) list) = List.map (fn (pat, exp) => (toUPat(ctx, pat), toUExp(ctx, exp))) matches
and toUDec(ctx, S.ValDec(tyvars, valbind)) = U.ValDec(List.map (fn tv => genTyVar(ctx, tv)) tyvars, toUValBind(ctx, valbind))
  | toUDec(ctx, S.TypeDec(typbinds)) = U.TypeDec(List.map (fn typbind => toUTypBind(ctx, typbind)) typbinds)
  | toUDec(ctx, S.DatatypeDec _) = raise NameError("not implemented yet")
  | toUDec(ctx, _) = raise NameError("not implemented yet")
and toUValBind(ctx, S.PatBind(pat, exp, valbind)) = U.PatBind(toUPat(ctx, pat), toUExp(ctx, exp), Option.map (fn x => toUValBind(ctx, x)) valbind)
  | toUValBind(ctx, S.RecValBind(valbind)) = U.RecValBind(toUValBind(ctx, valbind))
and toUTypBind(ctx, _) = raise NameError("not implemented yet")
end (* local *)
end (* structure PostParsing *)
