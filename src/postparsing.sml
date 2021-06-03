(*
 * Copyright (c) 2021 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure Fixity = struct

type Context = {}

fun emitError(ctx : Context, spans, message) = raise Syntax.SyntaxError (spans, message)

type FixityStatusMap = Syntax.FixityStatus Syntax.VIdMap.map

datatype IdStatusMap' = MkIdStatusMap of IdStatusMap
withtype IdStatusMap = { valMap : Syntax.IdStatus Syntax.VIdMap.map
                       , tyConMap : (Syntax.IdStatus Syntax.VIdMap.map) Syntax.TyConMap.map
                       , strMap : IdStatusMap' Syntax.StrIdMap.map
                       }
type Env = { fixityMap : FixityStatusMap
           , idStatusMap : IdStatusMap
           , sigMap : IdStatusMap Syntax.SigIdMap.map
           }

val emptyIdStatusMap : IdStatusMap = { valMap = Syntax.VIdMap.empty
                                     , tyConMap = Syntax.TyConMap.empty
                                     , strMap = Syntax.StrIdMap.empty
                                     }

val emptyEnv : Env = { fixityMap = Syntax.VIdMap.empty
                     , idStatusMap = emptyIdStatusMap
                     , sigMap = Syntax.SigIdMap.empty
                     }

(* used by fixity declaration *)
fun envWithFixityMap fixityMap : Env = { fixityMap = fixityMap
                                       , idStatusMap = emptyIdStatusMap
                                       , sigMap = Syntax.SigIdMap.empty
                                       }

fun envWithIdStatusMap idStatusMap : Env = { fixityMap = Syntax.VIdMap.empty
                                           , idStatusMap = idStatusMap
                                           , sigMap = Syntax.SigIdMap.empty
                                           }

(* used by signature binding *)
fun envWithSigMap sigMap : Env = { fixityMap = Syntax.VIdMap.empty
                                 , idStatusMap = emptyIdStatusMap
                                 , sigMap = sigMap
                                 }

fun mergeIdStatusMap(env1 : IdStatusMap, env2 : IdStatusMap) : IdStatusMap
    = { valMap = Syntax.VIdMap.unionWith #2 (#valMap env1, #valMap env2)
      , tyConMap = Syntax.TyConMap.unionWith #2 (#tyConMap env1, #tyConMap env2)
      , strMap = Syntax.StrIdMap.unionWith #2 (#strMap env1, #strMap env2)
      }

fun mergeEnv(env1 : Env, env2 : Env) : Env
    = { fixityMap = Syntax.VIdMap.unionWith #2 (#fixityMap env1, #fixityMap env2)
      , idStatusMap = mergeIdStatusMap(#idStatusMap env1, #idStatusMap env2)
      , sigMap = Syntax.SigIdMap.unionWith #2 (#sigMap env1, #sigMap env2)
      }

fun getFixityStatus ({ fixityMap, ... } : Env, vid)
    = case Syntax.VIdMap.find (fixityMap, vid) of
          SOME a => a
        | NONE => Syntax.Nonfix

fun isConstructor ({ idStatusMap = { valMap, ... }, ... } : Env, vid)
    = case Syntax.VIdMap.find (valMap, vid) of
          SOME Syntax.ValueConstructor => true
        | SOME Syntax.ExceptionConstructor => true
        | _ => false

fun ConOrVarPat(env, span, vid) = if isConstructor(env, vid) then
                                      Syntax.ConPat(span, Syntax.MkQualified([], vid), NONE)
                                  else
                                      Syntax.VarPat(span, vid)

(* used by datatype replication *)
fun lookupLongTyConInIdStatusMap(env : IdStatusMap, [], tycon)
    = Syntax.TyConMap.find(#tyConMap env, tycon)
  | lookupLongTyConInIdStatusMap(env, strid :: strids, tycon)
    = case Syntax.StrIdMap.find(#strMap env, strid) of
          SOME (MkIdStatusMap env) => lookupLongTyConInIdStatusMap(env, strids, tycon)
        | NONE => NONE
fun lookupLongTyCon(env : Env, Syntax.MkQualified(strids, tycon)) : (Syntax.IdStatus Syntax.VIdMap.map) option
    = lookupLongTyConInIdStatusMap(#idStatusMap env, strids, tycon)

(* used by open declaration *)
fun lookupLongStrIdInIdStatusMap(env : IdStatusMap, []) = SOME env
  | lookupLongStrIdInIdStatusMap(env, strid :: strids)
    = case Syntax.StrIdMap.find(#strMap env, strid) of
          SOME (MkIdStatusMap env) => lookupLongStrIdInIdStatusMap(env, strids)
        | NONE => NONE
fun lookupLongStrId({ idStatusMap, ... } : Env, Syntax.MkQualified(strids, strid)) : IdStatusMap option
    = lookupLongStrIdInIdStatusMap(idStatusMap, strids @ [ strid ])

datatype ('op, 'a) InfixList = Leaf of 'a
                             | Tree of 'a * Syntax.InfixAssociativity * SourcePos.span * 'op * ('op, 'a) InfixList
fun maxPrec(p, Leaf _) = p
  | maxPrec(p, Tree(_, Syntax.LeftAssoc q, _, _, rest)) = maxPrec(Int.max(p, q), rest)
  | maxPrec(p, Tree(_, Syntax.RightAssoc q, _, _, rest)) = maxPrec(Int.max(p, q), rest)
(* resolveFixity : Context * ('a * SourceSpan.span * 'op * 'a -> 'a) -> ('op, 'a) InfixList -> 'a *)
fun resolveFixity (ctx, f)
    = let fun go(Leaf x) = x
            | go(t as Tree(_, assoc, span, _, rest)) = let val p0 = case assoc of
                                                                        Syntax.LeftAssoc p0 => p0
                                                                      | Syntax.RightAssoc p0 => p0
                                                           val prec = maxPrec(p0, rest)
                                                       in go(goPrec(prec, t)) end
          and goPrec(p, Leaf x) = Leaf x
            | goPrec(p, Tree(x, assoc as Syntax.LeftAssoc q, span, op_, rest))
              = if p = q then
                    goLeftAssoc(p, x, span, op_, rest)
                else (* p > q *)
                    Tree(x, assoc, span, op_, goPrec(p, rest))
            | goPrec(p, Tree(x, assoc as Syntax.RightAssoc q, span, op_, rest))
              = if p = q then
                    goRightAssoc(p, span, fn y => f(x, span, op_, y), rest)
                else (* p > q *)
                    Tree(x, assoc, span, op_, goPrec(p, rest))
          and goLeftAssoc(p, x, span, op_, Leaf y) = Leaf(f(x, span, op_, y))
            | goLeftAssoc(p, x, span1, op_, Tree(y, assoc as Syntax.LeftAssoc q, span2, op', rest))
              = if p = q then
                    goLeftAssoc(p, f(x, span1, op_, y), span2, op', rest)
                else (* p > q *)
                    Tree(f(x, span1, op_, y), assoc, span2, op', goPrec(p, rest))
            | goLeftAssoc(p, x, span1, op_, Tree(y, assoc as Syntax.RightAssoc q, span2, op', rest))
              = if p = q then
                    emitError (ctx, [span1, span2], "you cannot mix left-associative operators and right-associative operators of same precedence")
                else (* p > q *)
                    Tree(f(x, span1, op_, y), assoc, span2, op', goPrec(p, rest))
          and goRightAssoc(p, _, g, Leaf y) = Leaf(g y)
            | goRightAssoc(p, span1, g, Tree(y, assoc as Syntax.LeftAssoc q, span2, op', rest))
              = if p = q then
                    emitError (ctx, [span1, span2], "you cannot mix left-associative operators and right-associative operators of same precedence")
                else (* p > q *)
                    Tree(g y, assoc, span2, op', goPrec(p, rest))
            | goRightAssoc(p, _, g, Tree(y, assoc as Syntax.RightAssoc q, span, op', rest))
              = if p = q then
                    goRightAssoc(p, span, fn z => g(f(y, span, op', z)), rest)
                else (* p > q *)
                    Tree(g y, assoc, span, op', goPrec(p, rest))
      in go
      end
(* let open Fixity in resolveFixity (fn (a,f,b) => f(a,b)) (Tree(3,Syntax.LeftAssoc 5,op +,Tree(2,Syntax.LeftAssoc 6,op *,Leaf 7))) end; should yield 17 *)

(* doPat : Context * Env * UnfixedSyntax.Pat -> Syntax.Pat *)
(* doExp : Context * Env * UnfixedSyntax.Exp -> Syntax.Exp *)
(* doDec : Context * Env * UnfixedSyntax.Dec -> Env * Syntax.Dec list *)
(* doDecs : Context * Env * UnfixedSyntax.Dec list -> Env * Syntax.Dec list *)
(* doValBind : Context * Env * UnfixedSyntax.ValBind -> Syntax.ValBind *)
fun doPat(ctx, env : Env, UnfixedSyntax.WildcardPat span) = Syntax.WildcardPat span
  | doPat(ctx, env, UnfixedSyntax.SConPat(span, scon)) = Syntax.SConPat(span, scon)
  | doPat(ctx, env, UnfixedSyntax.InfixOrVIdPat(span, vid)) = (case getFixityStatus(env, vid) of
                                                                   Syntax.Nonfix => ConOrVarPat(env, span, vid)
                                                                 | _ => emitError(ctx, [span], "infix operator used in non-infix position")
                                                              )
  | doPat(ctx, env, UnfixedSyntax.NonInfixVIdPat(span, Syntax.MkQualified([], vid))) = ConOrVarPat(env, span, vid)
  | doPat(ctx, env, UnfixedSyntax.NonInfixVIdPat(span, longvid)) = Syntax.ConPat(span, longvid, NONE) (* TODO: Check idstatus? *)
  | doPat(ctx, env, UnfixedSyntax.RecordPat{sourceSpan, fields, wildcard}) = Syntax.RecordPat { sourceSpan = sourceSpan, fields = List.map (fn (label, pat) => (label, doPat(ctx, env, pat))) fields, wildcard = wildcard }
  | doPat(ctx, env, UnfixedSyntax.JuxtapositionPat(jspan, patterns)) (* constructed pattern or infix constructed pattern *)
    = let fun doPrefix(UnfixedSyntax.InfixOrVIdPat(span1, vid) :: UnfixedSyntax.InfixOrVIdPat(span2, vid') :: pats)
              = (case getFixityStatus(env, vid) of
                     Syntax.Nonfix => (case getFixityStatus(env, vid') of
                                           Syntax.Nonfix => doInfix(Syntax.ConPat(SourcePos.mergeSpan(span1, span2), Syntax.MkLongVId([], vid), SOME(ConOrVarPat(env, span2, vid'))), pats)
                                         | Syntax.Infix assoc => Tree(ConOrVarPat(env, span1, vid), assoc, span2, vid', doPrefix(pats))
                                      )
                   | Syntax.Infix assoc => emitError(ctx, [span1], "infix operator used in prefix position")
                )
            | doPrefix(UnfixedSyntax.InfixOrVIdPat(span1, vid) :: atpat :: pats)
              = (case getFixityStatus(env, vid) of
                     Syntax.Nonfix => doInfix(Syntax.ConPat(SourcePos.mergeSpan(span1, UnfixedSyntax.getSourceSpanOfPat atpat), Syntax.MkLongVId([], vid), SOME(doPat(ctx, env, atpat))), pats)
                   | Syntax.Infix _ => emitError(ctx, [span1], "infix operator used in prefix position")
                )
            | doPrefix(UnfixedSyntax.NonInfixVIdPat(span1, longvid) :: UnfixedSyntax.InfixOrVIdPat(span2, vid') :: pats)
              = (case getFixityStatus(env, vid') of
                     Syntax.Nonfix => doInfix(Syntax.ConPat(SourcePos.mergeSpan(span1, span2), longvid, SOME(ConOrVarPat(env, span2, vid'))), pats)
                   | Syntax.Infix assoc => case longvid of
                                               Syntax.MkQualified([], vid) => Tree(ConOrVarPat(env, span1, vid), assoc, span2, vid', doPrefix(pats))
                                             | _ => Tree(Syntax.ConPat(SourcePos.mergeSpan(span1, span2), longvid, NONE), assoc, span2, vid', doPrefix(pats))
                )
            | doPrefix(UnfixedSyntax.NonInfixVIdPat(span1, longvid) :: atpat :: pats)
              = doInfix(Syntax.ConPat(SourcePos.mergeSpan(span1, UnfixedSyntax.getSourceSpanOfPat atpat), longvid, SOME(doPat(ctx, env, atpat))), pats)
            | doPrefix(atpat :: UnfixedSyntax.InfixOrVIdPat(span2, vid') :: pats)
              = (case getFixityStatus(env, vid') of
                     Syntax.Nonfix => emitError(ctx, [SourcePos.mergeSpan(UnfixedSyntax.getSourceSpanOfPat atpat, span2)], "invalid pattern")
                   | Syntax.Infix assoc => Tree(doPat(ctx, env, atpat), assoc, span2, vid', doPrefix(pats))
                )
            | doPrefix(pat :: nil) = Leaf(doPat(ctx, env, pat))
            | doPrefix _ = emitError(ctx, [jspan], "invalid pattern")
          and doInfix(lhs : Syntax.Pat, UnfixedSyntax.InfixOrVIdPat(span2, vid) :: pats)
              = (case getFixityStatus(env, vid) of
                     Syntax.Nonfix => emitError(ctx, [SourcePos.mergeSpan(Syntax.getSourceSpanOfPat lhs, span2)], "invalid pattern")
                   | Syntax.Infix assoc => Tree(lhs, assoc, span2, vid, doPrefix(pats))
                )
            | doInfix(lhs, nil) = Leaf lhs
            | doInfix(lhs, pat :: _) = emitError(ctx, [SourcePos.mergeSpan(Syntax.getSourceSpanOfPat lhs, UnfixedSyntax.getSourceSpanOfPat pat)], "invalid pattern")
      in resolveFixity (ctx, Syntax.MkInfixConPat) (doPrefix patterns)
      end
  | doPat(ctx, env, UnfixedSyntax.ConPat(span, longvid, pat)) = Syntax.ConPat(span, longvid, SOME(doPat(ctx, env, pat)))
  | doPat(ctx, env, UnfixedSyntax.TypedPat(span, pat, ty)) = Syntax.TypedPat(span, doPat(ctx, env, pat), ty)
  | doPat(ctx, env, UnfixedSyntax.ConjunctivePat(span, pat1, pat2))
    = (case pat1 of
           UnfixedSyntax.TypedPat (_, UnfixedSyntax.JuxtapositionPat(_, [UnfixedSyntax.InfixOrVIdPat(span, vid)]), ty) => (case getFixityStatus(env, vid) of
                                                                                                                               Syntax.Nonfix => Syntax.LayeredPat (span, vid, SOME ty, doPat(ctx, env, pat2))
                                                                                                                             | Syntax.Infix _ => emitError(ctx, [span], "invalid infix identifier in layered pattern")
                                                                                                                          )
         | UnfixedSyntax.TypedPat (_, UnfixedSyntax.JuxtapositionPat(_, [UnfixedSyntax.NonInfixVIdPat(_, Syntax.MkQualified([], vid))]), ty) => Syntax.LayeredPat (span, vid, SOME ty, doPat(ctx, env, pat2))
         | UnfixedSyntax.JuxtapositionPat(_, [UnfixedSyntax.InfixOrVIdPat(span, vid)]) => (case getFixityStatus(env, vid) of
                                                                                               Syntax.Nonfix => Syntax.LayeredPat (span, vid, NONE, doPat(ctx, env, pat2)) (* TODO: Check infix status *)
                                                                                             | Syntax.Infix _ => emitError(ctx, [span], "invalid infix identifier in layered pattern")
                                                                                          )
         | UnfixedSyntax.JuxtapositionPat(_, [UnfixedSyntax.NonInfixVIdPat(_, Syntax.MkQualified([], vid))]) => Syntax.LayeredPat (span, vid, NONE, doPat(ctx, env, pat2))
         | _ => emitError(ctx, [span], "conjunctive: not implemented yet") (* Successor ML *)
      )
fun doExp(ctx, env, UnfixedSyntax.SConExp(span, scon)) = Syntax.SConExp(span, scon)
  | doExp(ctx, env, UnfixedSyntax.InfixOrVIdExp(span, vid)) = (case getFixityStatus(env, vid) of
                                                                   Syntax.Nonfix => Syntax.VarExp(span, Syntax.MkLongVId([], vid))
                                                                 | _ => emitError(ctx, [span], "infix operaor used in non-infix position")
                                                              )
  | doExp(ctx, env, UnfixedSyntax.NonInfixVIdExp(span, longvid)) = Syntax.VarExp(span, longvid)
  | doExp(ctx, env, UnfixedSyntax.RecordExp(span, fields)) = Syntax.RecordExp (span, List.map (fn (label, exp) => (label, doExp(ctx, env, exp))) fields)
  | doExp(ctx, env, UnfixedSyntax.LetInExp(span, decls, exp)) = let val (env', decls') = doDecs(ctx, env, decls)
                                                                in Syntax.LetInExp(span, decls', doExp(ctx, mergeEnv(env, env'), exp))
                                                                end
  | doExp(ctx, env, UnfixedSyntax.JuxtapositionExp(span, expressions)) (* application or infix application *)
    = let fun doPrefix(exp1 :: rest) = doInfix(doExp(ctx, env, exp1), rest)
            | doPrefix _ = emitError(ctx, [span], "invalid expression")
          and doInfix(lhs : Syntax.Exp, UnfixedSyntax.InfixOrVIdExp(span2, vid) :: rest)
              = (case getFixityStatus(env, vid) of
                     Syntax.Nonfix => doInfix(Syntax.AppExp(SourcePos.mergeSpan(Syntax.getSourceSpanOfExp lhs, span2), lhs, Syntax.VarExp(span2, Syntax.MkLongVId([], vid))), rest)
                   | Syntax.Infix assoc => Tree(lhs, assoc, span2, vid, doPrefix(rest))
                )
            | doInfix(lhs, x :: rest) = let val x' = doExp(ctx, env, x)
                                        in doInfix(Syntax.AppExp(SourcePos.mergeSpan(Syntax.getSourceSpanOfExp lhs, Syntax.getSourceSpanOfExp x'), lhs, x'), rest)
                                        end
            | doInfix(lhs, nil) = Leaf lhs
      in resolveFixity (ctx, Syntax.MkInfixExp) (doPrefix expressions)
      end
  | doExp(ctx, env, UnfixedSyntax.AppExp(span, exp1, exp2)) = Syntax.AppExp(span, doExp(ctx, env, exp1), doExp(ctx, env, exp2))
  | doExp(ctx, env, UnfixedSyntax.TypedExp(span, exp, ty)) = Syntax.TypedExp(span, doExp(ctx, env, exp), ty)
  | doExp(ctx, env, UnfixedSyntax.HandleExp(span, exp, matches)) = Syntax.HandleExp(span, doExp(ctx, env, exp), List.map (fn (pat, exp') => (doPat(ctx, env, pat), doExp(ctx, env, exp'))) matches)
  | doExp(ctx, env, UnfixedSyntax.RaiseExp(span, exp)) = Syntax.RaiseExp(span, doExp(ctx, env, exp))
  | doExp(ctx, env, UnfixedSyntax.IfThenElseExp(span, e1, e2, e3)) = Syntax.IfThenElseExp(span, doExp(ctx, env, e1), doExp(ctx, env, e2), doExp(ctx, env, e3))
  | doExp(ctx, env, UnfixedSyntax.WhileDoExp(span, e1, e2)) = Syntax.WhileDoExp(span, doExp(ctx, env, e1), doExp(ctx, env, e2))
  | doExp(ctx, env, UnfixedSyntax.CaseExp(span, exp, matches)) = Syntax.CaseExp(span, doExp(ctx, env, exp), List.map (fn (pat, exp') => (doPat(ctx, env, pat), doExp(ctx, env, exp'))) matches)
  | doExp(ctx, env, UnfixedSyntax.FnExp(span, matches)) = Syntax.FnExp(span, List.map (fn (pat, exp) => (doPat(ctx, env, pat), doExp(ctx, env, exp))) matches)
  | doExp(ctx, env, UnfixedSyntax.ProjectionExp(span, lab)) = Syntax.ProjectionExp(span, lab)
  | doExp(ctx, env, UnfixedSyntax.ListExp(span, xs)) = Syntax.ListExp(span, Vector.map (fn e => doExp(ctx, env, e)) xs)
and doDecs(ctx, env, nil) = (emptyEnv, nil)
  | doDecs(ctx, env, dec :: decs) = let val (env', dec') = doDec(ctx, env, dec)
                                        val (env'', decs') = doDecs(ctx, mergeEnv(env, env'), decs)
                                    in (mergeEnv(env', env''), dec' @ decs')
                                    end
and doDec(ctx, env, UnfixedSyntax.ValDec(span, tyvars, valbind)) = (emptyEnv, [Syntax.ValDec(span, tyvars, List.map (fn vb => doValBind(ctx, env, vb)) valbind)])
  | doDec(ctx, env, UnfixedSyntax.RecValDec(span, tyvars, valbind)) = (emptyEnv, [Syntax.RecValDec(span, tyvars, List.map (fn vb => doValBind(ctx, env, vb)) valbind)])
  | doDec(ctx, env, UnfixedSyntax.FValDec(span, tyvars, fvalbind)) = (emptyEnv, [Syntax.FunDec(span, tyvars, List.map (fn fvb => doFValBind(ctx, env, fvb)) fvalbind)])
  | doDec(ctx, env, UnfixedSyntax.TypeDec(span, typbinds)) = (emptyEnv, [Syntax.TypeDec(span, typbinds)])
  | doDec(ctx, env, UnfixedSyntax.DatatypeDec(span, datbinds)) = let val doConBinds : Syntax.ConBind list -> Syntax.IdStatus Syntax.VIdMap.map
                                                                         = List.foldl (fn (Syntax.ConBind(_, vid, _), map) =>
                                                                                          (* Syntactic Restriction: vid must not be one of "true", "false", "nil", "::" or "ref". *)
                                                                                          Syntax.VIdMap.insert(map, vid, Syntax.ValueConstructor)) Syntax.VIdMap.empty
                                                                     val tyConMap = List.foldl (fn (Syntax.DatBind(span, tyvars, tycon, conbinds), map) => Syntax.TyConMap.insert(map, tycon, doConBinds conbinds)) Syntax.TyConMap.empty datbinds
                                                                     val valMap = Syntax.TyConMap.foldl (Syntax.VIdMap.unionWith #2 (* should be disjoint *)) Syntax.VIdMap.empty tyConMap
                                                                     val idStatusMap = { valMap = valMap
                                                                                       , tyConMap = tyConMap
                                                                                       , strMap = Syntax.StrIdMap.empty
                                                                                       }
                                                                 in (envWithIdStatusMap idStatusMap, [Syntax.DatatypeDec(span, datbinds)])
                                                                 end
  | doDec(ctx, env, UnfixedSyntax.DatatypeRepDec(span, tycon, longtycon)) = let val valMap = case lookupLongTyCon(env, longtycon) of
                                                                                                 SOME valMap => valMap
                                                                                               | NONE => Syntax.VIdMap.empty
                                                                                val idStatusMap = { valMap = valMap
                                                                                                  , tyConMap = Syntax.TyConMap.singleton(tycon, valMap)
                                                                                                  , strMap = Syntax.StrIdMap.empty
                                                                                                  }
                                                                            in (envWithIdStatusMap idStatusMap, [Syntax.DatatypeRepDec(span, tycon, longtycon)]) (* copy tyConMap's ValEnv into valMap *)
                                                                            end
  | doDec(ctx, env, UnfixedSyntax.AbstypeDec(span, datbinds, decs)) = let val (_, decs') = doDecs(ctx, env, decs) (* not really implemented yet *)
                                                                      in (emptyEnv, [Syntax.AbstypeDec(span, datbinds, decs')])
                                                                      end
  | doDec(ctx, env, UnfixedSyntax.ExceptionDec(span, exbinds)) = let val valMap = List.foldl (fn (Syntax.ExBind(span, vid, _), valMap) => Syntax.VIdMap.insert(valMap, vid, Syntax.ExceptionConstructor)
                                                                                             | (Syntax.ExReplication(span, vid, _), valMap) => Syntax.VIdMap.insert(valMap, vid, Syntax.ExceptionConstructor) (* RHS should be an exception constructor *)
                                                                                             ) Syntax.VIdMap.empty exbinds
                                                                     val idStatusMap = { valMap = valMap
                                                                                       , tyConMap = Syntax.TyConMap.empty
                                                                                       , strMap = Syntax.StrIdMap.empty
                                                                                       }
                                                                 in (envWithIdStatusMap idStatusMap, [Syntax.ExceptionDec(span, exbinds)])
                                                                 end
  | doDec(ctx, env, UnfixedSyntax.LocalDec(span, decs1, decs2)) = let val (env', decs1') = doDecs(ctx, env, decs1)
                                                                      val (env'', decs2') = doDecs(ctx, mergeEnv(env, env'), decs2)
                                                                  in (env'', [Syntax.LocalDec(span, decs1', decs2')])
                                                                  end
  | doDec(ctx, env, UnfixedSyntax.OpenDec(span, strids)) = let val idStatusMap = List.foldl (fn (strid, m) => case lookupLongStrId(env, strid) of
                                                                                                                    SOME m' => mergeIdStatusMap(m, m')
                                                                                                                  | NONE => emitError(ctx, [span], "structure not found: " ^ Syntax.print_LongStrId strid)
                                                                                            ) emptyIdStatusMap strids
                                                           in (envWithIdStatusMap idStatusMap, [Syntax.OpenDec(span, strids)])
                                                           end
  | doDec(ctx, env, UnfixedSyntax.FixityDec(span, fixity, vids)) = let val fixityMap = List.foldl (fn (vid, m) => Syntax.VIdMap.insert(m, vid, fixity)) Syntax.VIdMap.empty vids
                                                                   in (envWithFixityMap fixityMap, [])
                                                                   end
and doValBind(ctx, env, UnfixedSyntax.PatBind(span, pat, exp)) = Syntax.PatBind(span, doPat(ctx, env, pat), doExp(ctx, env, exp))
and doFValBind(ctx, env, UnfixedSyntax.FValBind(span, rules)) = let fun doFMRule (UnfixedSyntax.FMRule(_, fpat, optTy, exp)) = (doFPat(ctx, env, fpat), optTy, doExp(ctx, env, exp))
                                                                    val rules' = List.map doFMRule rules
                                                                    fun getVIdAndArity (((span, vid, []), _, _) :: xs) = emitError(ctx, [span], "function declaration with no arguments")
                                                                      | getVIdAndArity (((span, vid, pats), _, _) :: xs) = checkVIdAndArity(span, vid, length pats, xs)
                                                                      | getVIdAndArity [] = emitError(ctx, [span], "internal error: empty 'fun' rule")
                                                                    and checkVIdAndArity(span, vid, arity, []) = (vid, arity)
                                                                      | checkVIdAndArity(span, vid, arity, ((span', vid', pats), _, _) :: xs)
                                                                        = if vid = vid' then
                                                                              if arity = length pats then
                                                                                  checkVIdAndArity(span, vid, arity, xs)
                                                                              else
                                                                                  emitError(ctx, [span], "invalid function declaration: arity mismatch")
                                                                          else
                                                                              emitError(ctx, [span, span'], "invalid function declaration: name mismatch")
                                                                    val (vid, arity) = getVIdAndArity rules'
                                                                in Syntax.FValBind { sourceSpan = span, vid = vid, arity = arity, rules = List.map (fn ((_, _, pats), optTy, exp) => (pats, optTy, exp)) rules' }
                                                                end
  (* (<pat1> <vid> <pat3>) -> <vid> is infix function name *)
and doFPat(ctx, env, UnfixedSyntax.FPat(span1, [UnfixedSyntax.JuxtapositionPat(span2, [pat1, UnfixedSyntax.InfixOrVIdPat(span3, vid), pat3])]))
    = (case getFixityStatus(env, vid) of
           Syntax.Nonfix => emitError(ctx, [span3], "invalid function declaration: '" ^ Syntax.getVIdName vid ^ "' must be an infix identifier")
         | Syntax.Infix _ => doInfixFPat(ctx, env, span1, span3, vid, pat1, pat3, [])
      )
  (* (<pat11> <vid1> <pat13>) <vid2> <pat3> -> <vid1> or <vid2> is infix function name *)
  | doFPat(ctx, env, UnfixedSyntax.FPat(span1, [pat1 as UnfixedSyntax.JuxtapositionPat(span2, [pat11, UnfixedSyntax.InfixOrVIdPat(span3, vid1), pat13]), pat2 as UnfixedSyntax.InfixOrVIdPat(span4, vid2), pat3]))
    = (case (getFixityStatus(env, vid1), getFixityStatus(env, vid2)) of
           (Syntax.Nonfix, Syntax.Nonfix) => emitError(ctx, [span3, span4], "invalid function declaration: '" ^ Syntax.getVIdName vid1 ^ "' or '" ^ Syntax.getVIdName vid2 ^ "' must be an infix identifier")
         | (Syntax.Infix _, Syntax.Nonfix) => doInfixFPat(ctx, env, span2, span3, vid1, pat11, pat13, [pat2, pat3])
         | (_, Syntax.Infix _) => doInfixFPat(ctx, env, span1, span4, vid2, pat1, pat3, [])
      )
  (* (<pat11> <vid> <pat13>) <pat2> ... -> <vid> is infix function name *)
  | doFPat(ctx, env, UnfixedSyntax.FPat(span1, UnfixedSyntax.JuxtapositionPat(span2, [pat11, UnfixedSyntax.InfixOrVIdPat(span3, vid), pat13]) :: pats))
    = (case getFixityStatus(env, vid) of
           Syntax.Nonfix => emitError(ctx, [span3], "invalid function declaration: '" ^ Syntax.getVIdName vid ^ "' must be an infix identifier")
         | Syntax.Infix _ => doInfixFPat(ctx, env, span1, span3, vid, pat11, pat13, pats)
      )
  (* <vid1> <vid2> <pat3> -> <vid2> is infix function name or <vid1> is nonfix function name
     <pat1> <vid2> <pat3> -> <vid2> is infix function name *)
  | doFPat(ctx, env, UnfixedSyntax.FPat(span, [pat1, pat2 as UnfixedSyntax.InfixOrVIdPat(span2, vid2), pat3]))
    = (case getFixityStatus(env, vid2) of
           Syntax.Nonfix => (case pat1 of
                                 UnfixedSyntax.NonInfixVIdPat(span1, Syntax.MkQualified([], vid1)) => doPrefixFPat(ctx, env, span1, vid1, [pat2, pat3])
                               | UnfixedSyntax.InfixOrVIdPat(span1, vid1) => (case getFixityStatus(env, vid1) of
                                                                                  Syntax.Nonfix => doPrefixFPat(ctx, env, span1, vid1, [pat2, pat3])
                                                                                | Syntax.Infix _ => emitError(ctx, [span1], "invalid function declaration: '" ^ Syntax.getVIdName vid1 ^ "' must be prefixed by an 'op'")
                                                                             )
                               | _ => emitError(ctx, [span2], "invalid function declaration: '" ^ Syntax.getVIdName vid2 ^ "' must be an infix identifier")
                            )
         | Syntax.Infix _ => doInfixFPat(ctx, env, span, span2, vid2, pat1, pat3, [])
      )
  (* (op) <vid> <pat> ... -> <vid> is function name *)
  | doFPat(ctx, env, UnfixedSyntax.FPat (_, UnfixedSyntax.NonInfixVIdPat(span, Syntax.MkQualified([], vid)) :: pats)) = doPrefixFPat(ctx, env, span, vid, pats)
  (* <vid> <pat> ... -> <vid> is nonfix function name  *)
  | doFPat(ctx, env, UnfixedSyntax.FPat (_, UnfixedSyntax.InfixOrVIdPat(span, vid) :: pats))
    = (case getFixityStatus(env, vid) of
           Syntax.Nonfix => doPrefixFPat(ctx, env, span, vid, pats)
         | Syntax.Infix _ => emitError(ctx, [span], "invalid function declaration: '" ^ Syntax.getVIdName vid ^ "' must be prefixed by an 'op'")
      )
  | doFPat(ctx, env, UnfixedSyntax.FPat (span, _))
    = emitError(ctx, [span], "invalid function declaration")
and doInfixFPat(ctx, env, span, vidspan, vid, patL, patR, pats) = (vidspan, vid, Syntax.TuplePat(span, [doPat(ctx, env, patL), doPat(ctx, env, patR)]) :: List.map (fn p => doPat(ctx, env, p)) pats)
and doPrefixFPat(ctx, env, span, vid, pats) = (span, vid, List.map (fn p => doPat(ctx, env, p)) pats)
(* doStrExp : Context * Env * UnfixedSyntax.Dec Syntax.StrExp -> IdStatusMap * Syntax.Dec Syntax.StrExp *)
(* doStrDec : Context * Env * UnfixedSyntax.Dec Syntax.StrDec -> Env * Syntax.Dec Syntax.StrDec *)
(* doStrDecs : Context * Env * (UnfixedSyntax.Dec Syntax.StrDec) list -> Env * (Syntax.Dec Syntax.StrDec) list *)
fun doStrExp(ctx, env, Syntax.StructExp(span, strdecs)) = let val (env', strdecs) = doStrDecs(ctx, env, strdecs)
                                                          in (#idStatusMap env', Syntax.StructExp(span, strdecs))
                                                          end
  | doStrExp(ctx, env, Syntax.StrIdExp(span, longstrid)) = let val env' = case lookupLongStrId(env, longstrid) of
                                                                              SOME env => env
                                                                            | NONE => emitError(ctx, [span], "structure not found: " ^ Syntax.print_LongStrId longstrid)
                                                           in (env', Syntax.StrIdExp(span, longstrid))
                                                           end
  | doStrExp(ctx, env, Syntax.TransparentConstraintExp(span, strexp, sigexp)) = let val env' = emptyIdStatusMap (* TODO: signature *)
                                                                                in (env', Syntax.TransparentConstraintExp(span, #2 (doStrExp(ctx, env, strexp)), sigexp)) 
                                                                                end
  | doStrExp(ctx, env, Syntax.OpaqueConstraintExp(span, strexp, sigexp)) = let val env' = emptyIdStatusMap (* TODO: signature *)
                                                                           in (env', Syntax.OpaqueConstraintExp(span, #2 (doStrExp(ctx, env, strexp)), sigexp))
                                                                           end
  | doStrExp(ctx, env, Syntax.LetInStrExp(span, strdecs, strexp)) = let val (env', strdecs) = doStrDecs(ctx, env, strdecs)
                                                                        val (env'', strexp) = doStrExp(ctx, mergeEnv(env, env'), strexp)
                                                                    in (env'', Syntax.LetInStrExp(span, strdecs, strexp))
                                                                    end
and doStrDec(ctx, env, Syntax.CoreDec(span, dec)) = let val (env', decs) = doDec(ctx, env, dec)
                                                    in (env', List.map (fn dec => Syntax.CoreDec(span, dec)) decs)
                                                    end
  | doStrDec(ctx, env, Syntax.StrBindDec(span, binds)) = let val (strMap, binds) = List.foldr (fn ((strid, strexp), (m, xs)) => let val (inner, strexp) = doStrExp(ctx, env, strexp)
                                                                                                                                in (Syntax.StrIdMap.insert(m, strid, MkIdStatusMap inner), (strid, strexp) :: xs)
                                                                                                                                end
                                                                                              ) (Syntax.StrIdMap.empty, []) binds
                                                             val idStatusMap = { valMap = Syntax.VIdMap.empty
                                                                               , tyConMap = Syntax.TyConMap.empty
                                                                               , strMap = strMap
                                                                               }
                                                         in (envWithIdStatusMap idStatusMap, [Syntax.StrBindDec(span, binds)])
                                                         end
  | doStrDec(ctx, env, Syntax.LocalStrDec(span, decs, decs')) = let val (env', decs) = doStrDecs(ctx, env, decs)
                                                                    val (env'', decs') = doStrDecs(ctx, mergeEnv(env, env'), decs')
                                                                in (env'', [Syntax.LocalStrDec(span, decs, decs')])
                                                                end
and doStrDecs(ctx, env, []) = (emptyEnv, [])
  | doStrDecs(ctx, env, dec :: decs) = let val (env', dec) = doStrDec(ctx, env, dec)
                                           val (env'', decs) = doStrDecs(ctx, mergeEnv(env, env'), decs)
                                       in (mergeEnv(env', env''), dec @ decs)
                                       end
fun doSigExp(ctx, env, Syntax.BasicSigExp(span, specs)) : IdStatusMap = List.foldl (fn (spec, m) => mergeIdStatusMap(m, doSpec(ctx, mergeEnv(env, envWithIdStatusMap m), spec))) emptyIdStatusMap specs
  | doSigExp(ctx, env, Syntax.SigIdExp(span, sigid)) = (case Syntax.SigIdMap.find(#sigMap env, sigid) of
                                                            SOME m => m
                                                          | NONE => emitError(ctx, [span], "signature not found: " ^ Syntax.print_SigId sigid)
                                                       )
  | doSigExp(ctx, env, Syntax.TypeRealisationExp(span, sigexp, tyvars, longtycon, ty)) = doSigExp(ctx, env, sigexp) (* does not affect idstatus *)
and doSpec(ctx, env, Syntax.ValDesc(span, descs)) = emptyIdStatusMap
  | doSpec(ctx, env, Syntax.TypeDesc(span, descs)) = emptyIdStatusMap
  | doSpec(ctx, env, Syntax.EqtypeDesc(span, descs)) = emptyIdStatusMap
  | doSpec(ctx, env, Syntax.DatDesc(span, descs)) = List.foldl (fn ((tyvars, tycon, condescs), { valMap, tyConMap, strMap }) =>
                                                                   let val valMap' = List.foldl (fn (Syntax.ConBind(_, vid, _), m) => Syntax.VIdMap.insert(m, vid, Syntax.ValueConstructor)) Syntax.VIdMap.empty condescs
                                                                   in { valMap = Syntax.VIdMap.unionWith #2 (valMap, valMap')
                                                                      , tyConMap = Syntax.TyConMap.insert(tyConMap, tycon, valMap')
                                                                      , strMap = strMap }
                                                                   end
                                                               ) emptyIdStatusMap descs
  | doSpec(ctx, env, Syntax.DatatypeRepSpec(span, tycon, longtycon)) = let val valMap = case lookupLongTyCon(env, longtycon) of
                                                                            SOME m => m
                                                                          | NONE => Syntax.VIdMap.empty
                                                                       in { valMap = valMap
                                                                          , tyConMap = Syntax.TyConMap.singleton(tycon, valMap)
                                                                          , strMap = Syntax.StrIdMap.empty
                                                                          }
                                                                       end
  | doSpec(ctx, env, Syntax.ExDesc(span, descs)) = { valMap = List.foldl (fn ((vid, _), m) => Syntax.VIdMap.insert(m, vid, Syntax.ExceptionConstructor)) Syntax.VIdMap.empty descs
                                                   , tyConMap = Syntax.TyConMap.empty
                                                   , strMap = Syntax.StrIdMap.empty
                                                   }
  | doSpec(ctx, env, Syntax.StrDesc(span, descs)) = let val strMap = List.foldl (fn ((strid, sigexp), strMap) => Syntax.StrIdMap.insert(strMap, strid, MkIdStatusMap (doSigExp(ctx, env, sigexp)))) Syntax.StrIdMap.empty descs
                                                    in { valMap = Syntax.VIdMap.empty
                                                       , tyConMap = Syntax.TyConMap.empty
                                                       , strMap = strMap
                                                       }
                                                    end
  | doSpec(ctx, env, Syntax.Include(span, sigexp)) = doSigExp(ctx, env, sigexp)
fun doTopDec(ctx, env, Syntax.StrDec(strdec)) = let val (env, strdecs) = doStrDec(ctx, env, strdec)
                                                in (env, List.map Syntax.StrDec strdecs)
                                                end
  | doTopDec(ctx, env, Syntax.SigDec(sigbinds)) = let val sigMap = List.foldl (fn (Syntax.SigBind (sigid, sigexp), m) => Syntax.SigIdMap.insert(m, sigid, doSigExp(ctx, env, sigexp))) Syntax.SigIdMap.empty sigbinds
                                                  in (envWithSigMap sigMap, [Syntax.SigDec(sigbinds)])
                                                  end
fun doTopDecs(ctx, env, []) : Env * (Syntax.Dec Syntax.TopDec) list = (emptyEnv, [])
  | doTopDecs(ctx, env, dec :: decs) = let val (env', dec) = doTopDec(ctx, env, dec)
                                           val (env'', decs) = doTopDecs(ctx, mergeEnv(env, env'), decs)
                                       in (mergeEnv(env', env''), dec @ decs)
                                       end
fun doProgram(ctx, env, []) : Env * ((Syntax.Dec Syntax.TopDec) list) list = (emptyEnv, [])
  | doProgram(ctx, env, dec :: decs) = let val (env', dec) = doTopDecs(ctx, env, dec)
                                           val (env'', decs) = doProgram(ctx, mergeEnv(env, env'), decs)
                                       in (mergeEnv(env', env''), dec :: decs)
                                       end
end (* structure Fixity *)

structure PostParsing = struct
(* structure TyVarSet = Syntax.TyVarSet *)
local
    open Syntax
in
(* freeTyVarsInTy : TyVarSet * Ty -> TyVarSet *)
fun freeTyVarsInTy(bound, TyVar(_, tv)) = if TyVarSet.member(bound, tv) then
                                              TyVarSet.empty
                                          else
                                              TyVarSet.singleton tv
  | freeTyVarsInTy(bound, RecordType(_, xs)) = List.foldl (fn ((_, ty), set) => TyVarSet.union(freeTyVarsInTy(bound, ty), set)) TyVarSet.empty xs
  | freeTyVarsInTy(bound, TyCon(_, xs, _)) = List.foldl (fn (ty,set) => TyVarSet.union(freeTyVarsInTy(bound, ty), set)) TyVarSet.empty xs
  | freeTyVarsInTy(bound, FnType(_, s, t)) = TyVarSet.union(freeTyVarsInTy(bound, s), freeTyVarsInTy(bound, t))

(* freeTyVarsInPat : TyVarSet * Pat -> TyVarSet *)
fun freeTyVarsInPat(_, WildcardPat _) = TyVarSet.empty
  | freeTyVarsInPat(_, SConPat _) = TyVarSet.empty
  | freeTyVarsInPat(_, VarPat _) = TyVarSet.empty
  | freeTyVarsInPat(bound, RecordPat{fields=xs, ...}) = List.foldl (fn ((_, pat), set) => TyVarSet.union(freeTyVarsInPat(bound, pat), set)) TyVarSet.empty xs
  | freeTyVarsInPat(_, ConPat(_, _, NONE)) = TyVarSet.empty
  | freeTyVarsInPat(bound, ConPat(_, _, SOME pat)) = freeTyVarsInPat(bound, pat)
  | freeTyVarsInPat(bound, TypedPat(_, pat, ty)) = TyVarSet.union(freeTyVarsInPat(bound, pat), freeTyVarsInTy(bound, ty))
  | freeTyVarsInPat(bound, LayeredPat(_, _, SOME ty, pat)) = TyVarSet.union(freeTyVarsInTy(bound, ty), freeTyVarsInPat(bound, pat))
  | freeTyVarsInPat(bound, LayeredPat(_, _, NONE, pat)) = freeTyVarsInPat(bound, pat)

(* unguardedTyVarsInValBind : TyVarSet.set * ValBind -> TyVarSet.set *)
local
    fun union3(x, y, z) = TyVarSet.union(x, TyVarSet.union(y, z))
    fun collectExp(_, SConExp _) = TyVarSet.empty
      | collectExp(_, VarExp _) = TyVarSet.empty
      | collectExp(bound, RecordExp(_, xs)) = List.foldl (fn ((_, e), set) => TyVarSet.union(collectExp(bound, e), set)) TyVarSet.empty xs
      | collectExp(bound, LetInExp(_, decs, e)) = List.foldl (fn (dec, acc) => TyVarSet.union(collectDec(bound, dec), acc)) (collectExp(bound, e)) decs
      | collectExp(bound, AppExp(_, x, y)) = TyVarSet.union(collectExp(bound, x), collectExp(bound,y))
      | collectExp(bound, TypedExp(_ ,x, ty)) = TyVarSet.union(collectExp(bound, x), TyVarSet.difference(freeTyVarsInTy(bound, ty), bound))
      | collectExp(bound, HandleExp(_, x, match)) = TyVarSet.union(collectExp(bound, x), collectMatch(bound, match))
      | collectExp(bound, RaiseExp(_, x)) = collectExp(bound, x)
      | collectExp(bound, IfThenElseExp(_, x, y, z)) = union3(collectExp(bound, x), collectExp(bound, y), collectExp(bound, z))
      | collectExp(bound, WhileDoExp(_, x, y)) = TyVarSet.union(collectExp(bound, x), collectExp(bound, y))
      | collectExp(bound, CaseExp(_, x, match)) = TyVarSet.union(collectExp(bound, x), collectMatch(bound, match))
      | collectExp(bound, FnExp(_, match)) = collectMatch(bound, match)
      | collectExp(bound, ProjectionExp(_, lab)) = TyVarSet.empty
      | collectExp(bound, ListExp(_, xs)) = Vector.foldl (fn (e, set) => TyVarSet.union(collectExp(bound, e), set)) TyVarSet.empty xs
    and collectMatch(bound, xs) = List.foldl (fn ((pat, e), set) => TyVarSet.union(freeTyVarsInPat(bound, pat), TyVarSet.union(collectExp(bound, e), set))) TyVarSet.empty xs
    and collectValBind(bound, PatBind(_, pat, e)) = TyVarSet.union(freeTyVarsInPat(bound, pat), collectExp(bound, e))
    and collectFValBind(bound, FValBind { rules = rules, ... }) = List.foldl (fn (rule, set) => TyVarSet.union(set, collectFRule(bound, rule))) TyVarSet.empty rules
    and collectFRule(bound, (pats, optTy, exp)) = let val tyVarsInPats = List.foldl TyVarSet.union TyVarSet.empty (List.map (fn pat => freeTyVarsInPat(bound, pat)) pats)
                                                      val tyVarsInOptTy = case optTy of
                                                                              NONE => TyVarSet.empty
                                                                            | SOME expTy => TyVarSet.difference(freeTyVarsInTy(bound, expTy), bound)
                                                  in union3(tyVarsInPats, tyVarsInOptTy, collectExp(bound, exp))
                                                  end
    and collectDec(bound, ValDec _) = TyVarSet.empty
      | collectDec(bound, RecValDec _) = TyVarSet.empty
      | collectDec(bound, FunDec _) = TyVarSet.empty
      | collectDec(bound, TypeDec(_, typbinds)) = List.foldl (fn (TypBind (_, tyvars, _, ty), acc) => TyVarSet.union(freeTyVarsInTy(TyVarSet.addList(bound, tyvars), ty), acc)) TyVarSet.empty typbinds
      | collectDec(bound, DatatypeDec(_, datbinds)) = List.foldl (fn (datbind, acc) => TyVarSet.union(collectDatBind(bound, datbind), acc)) TyVarSet.empty datbinds
      | collectDec(bound, DatatypeRepDec(_, _, _)) = TyVarSet.empty
      | collectDec(bound, AbstypeDec(_, _, _)) = TyVarSet.empty (* not implemeted yet *)
      | collectDec(bound, ExceptionDec(span, exbinds)) = List.foldl (fn (ExBind(span, vid, SOME ty), acc) => TyVarSet.union(freeTyVarsInTy(bound, ty), acc)
                                                                    | (ExBind(span, vid, NONE), acc) => acc
                                                                    | (ExReplication(_, _, _), acc) => acc) TyVarSet.empty exbinds
      | collectDec(bound, LocalDec(_, decs1, decs2)) = List.foldl (fn (dec, acc) => TyVarSet.union(collectDec(bound, dec), acc)) (List.foldl (fn (dec, acc) => TyVarSet.union(collectDec(bound, dec), acc)) TyVarSet.empty decs1) decs2
      | collectDec(bound, OpenDec _) = TyVarSet.empty
    and collectDatBind(bound, DatBind (_, tyvars, _, conbinds)) = let val bound = TyVarSet.addList(bound, tyvars)
                                                                      fun doConBind(ConBind(_, _, NONE)) = TyVarSet.empty
                                                                        | doConBind(ConBind(_, _, SOME ty)) = freeTyVarsInTy(bound, ty)
                                                                  in List.foldl (fn (conbind, acc) => TyVarSet.union(doConBind conbind, acc)) TyVarSet.empty conbinds
                                                                  end
in
val unguardedTyVarsInExp : TyVarSet.set * Exp -> TyVarSet.set = collectExp
val unguardedTyVarsInValBind : TyVarSet.set * ValBind list -> TyVarSet.set = fn (bound, valbinds) => List.foldl (fn (valbind, set) => TyVarSet.union(set, collectValBind(bound, valbind))) TyVarSet.empty valbinds
val unguardedTyVarsInFValBind : TyVarSet.set * FValBind list -> TyVarSet.set = fn (bound, fvalbinds) => List.foldl (fn (fvalbind, set) => TyVarSet.union(set, collectFValBind(bound, fvalbind))) TyVarSet.empty fvalbinds
end (* local *)

(* The Definition 4.6 *)
(* scopeTyVarsInDec: TyVarSet.set * Dec -> Dec *)
local
    fun doDec(bound, ValDec(span, expbound, valbind)) = let val bound' = TyVarSet.addList(bound, expbound)
                                                            val unguarded = unguardedTyVarsInValBind(bound', valbind)
                                                            val expbound' = expbound @ TyVarSet.listItems(unguarded)
                                                            val bound'' = TyVarSet.union(bound', unguarded)
                                                        in ValDec(span, expbound', List.map (fn vb => doValBind(bound'', vb)) valbind)
                                                        end
      | doDec(bound, RecValDec(span, expbound, valbind)) = let val bound' = TyVarSet.addList(bound, expbound)
                                                               val unguarded = unguardedTyVarsInValBind(bound', valbind)
                                                               val expbound' = expbound @ TyVarSet.listItems(unguarded)
                                                               val bound'' = TyVarSet.union(bound', unguarded)
                                                           in RecValDec(span, expbound', List.map (fn vb => doValBind(bound'', vb)) valbind)
                                                           end
      | doDec(bound, FunDec(span, expbound, fvalbind)) = let val bound' = TyVarSet.addList(bound, expbound)
                                                             val unguarded = unguardedTyVarsInFValBind(bound', fvalbind)
                                                             val expbound' = expbound @ TyVarSet.listItems(unguarded)
                                                             val bound'' = TyVarSet.union(bound', unguarded)
                                                         in FunDec(span, expbound', List.map (fn fvb => doFValBind(bound'', fvb)) fvalbind)
                                                         end
      | doDec(bound, dec as TypeDec _) = dec
      | doDec(bound, dec as DatatypeDec _) = dec
      | doDec(bound, dec as DatatypeRepDec _) = dec
      | doDec(bound, dec as AbstypeDec _) = dec
      | doDec(bound, dec as ExceptionDec _) = dec
      | doDec(bound, LocalDec(span, xs, ys)) = LocalDec(span, doDecList(bound, xs), doDecList(bound, ys))
      | doDec(bound, dec as OpenDec _) = dec
    and doDecList(bound, decls) = List.map (fn x => doDec(bound, x)) decls
    and doValBind(bound, PatBind(span, pat, e)) = PatBind(span, pat, doExp(bound, e))
    and doFValBind(bound, FValBind { sourceSpan, vid, arity, rules }) = let fun doFRule(pats, optTy, exp) = (pats, optTy, doExp(bound, exp))
                                                                        in FValBind { sourceSpan = sourceSpan, vid = vid, arity = arity, rules = List.map doFRule rules }
                                                                        end
    and doExp(bound, exp as SConExp _) = exp
      | doExp(bound, exp as VarExp _) = exp
      | doExp(bound, exp as RecordExp _) = exp
      | doExp(bound, LetInExp(span, decls, exp)) = LetInExp(span, doDecList(bound, decls), doExp(bound, exp))
      | doExp(bound, AppExp(span, x, y)) = AppExp(span, doExp(bound, x), doExp(bound, y))
      | doExp(bound, TypedExp(span, x, ty)) = TypedExp(span, doExp(bound, x), ty)
      | doExp(bound, HandleExp(span, x, match)) = HandleExp(span, doExp(bound, x), doMatch(bound, match))
      | doExp(bound, RaiseExp(span, x)) = RaiseExp(span, doExp(bound, x))
      | doExp(bound, IfThenElseExp(span, x, y, z)) = IfThenElseExp(span, doExp(bound, x), doExp(bound, y), doExp(bound, z))
      | doExp(bound, WhileDoExp(span, x, y)) = WhileDoExp(span, doExp(bound, x), doExp(bound, y))
      | doExp(bound, CaseExp(span, x, match)) = CaseExp(span, doExp(bound, x), doMatch(bound, match))
      | doExp(bound, FnExp(span, match)) = FnExp(span, doMatch(bound, match))
      | doExp(bound, exp as ProjectionExp _) = exp
      | doExp(bound, ListExp(span, xs)) = ListExp(span, Vector.map (fn x => doExp(bound, x)) xs)
    and doMatch(bound, xs) = List.map (fn (pat, exp) => (pat, doExp(bound, exp))) xs
    fun doStrExp(StructExp(span, strdecs)) = StructExp(span, List.map doStrDec strdecs)
      | doStrExp(StrIdExp(span, longstrid)) = StrIdExp(span, longstrid)
      | doStrExp(TransparentConstraintExp(span, strexp, sigexp)) = TransparentConstraintExp(span, doStrExp strexp, sigexp)
      | doStrExp(OpaqueConstraintExp(span, strexp, sigexp)) = OpaqueConstraintExp(span, doStrExp strexp, sigexp)
      | doStrExp(LetInStrExp(span, strdecs, strexp)) = LetInStrExp(span, doStrDecs strdecs, doStrExp strexp)
    and doStrDec(CoreDec(span, dec)) = CoreDec(span, doDec(TyVarSet.empty, dec))
      | doStrDec(StrBindDec(span, binds)) = StrBindDec(span, List.map (fn (strid, strexp) => (strid, doStrExp strexp)) binds)
      | doStrDec(LocalStrDec(span, decs1, decs2)) = LocalStrDec(span, doStrDecs decs1, List.map doStrDec decs2)
    and doStrDecs(strdecs) = List.map doStrDec strdecs
in
val scopeTyVarsInDecs: TyVarSet.set * Dec list -> Dec list = doDecList
val scopeTyVarsInDec: TyVarSet.set * Dec -> Dec = doDec
val scopeTyVarsInExp: TyVarSet.set * Exp -> Exp = doExp
val scopeTyVarsInStrDec: Dec StrDec -> Dec StrDec = doStrDec
val scopeTyVarsInTopDecs = List.map (fn StrDec(strdec) => StrDec(scopeTyVarsInStrDec(strdec))
                                    | topdec as SigDec _ => topdec)
val scopeTyVarsInProgram = List.map scopeTyVarsInTopDecs
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
      | doTy(S.RecordType(_, row)) = if checkRow row then
                                         raise S.SyntaxError ([], "No type-expression row may bind the same label twice")
                                     else
                                         ()
      | doTy(S.TyCon(_, arg, longtycon)) = ()
      | doTy(S.FnType(_, s, t)) = (doTy s ; doTy t)

    (* doPat : S.Pat -> unit *)
    fun doPat(S.WildcardPat _) = ()
      | doPat(S.SConPat(_, S.RealConstant _)) = raise S.SyntaxError ([], "No real constant may occur in a pattern")
      | doPat(S.SConPat _) = ()
      | doPat(S.VarPat _) = ()
      | doPat(S.RecordPat{fields = row, wildcard = ellip, ...}) = if checkRow row then
                                                                      raise S.SyntaxError ([], "No pattern row may bind the same label twice")
                                                                  else
                                                                      List.app (fn (_, pat) => doPat pat) row
      | doPat(S.ConPat(_, _, NONE)) = ()
      | doPat(S.ConPat(_, longvid, SOME pat)) = doPat pat
      | doPat(S.TypedPat(_, pat, ty)) = (doPat pat ; doTy ty)
      | doPat(S.LayeredPat(_, vid, NONE, pat)) = (doPat pat)
      | doPat(S.LayeredPat(_, vid, SOME ty, pat)) = (doTy ty ; doPat pat)

    fun doTypBind(S.TypBind(_, tyvarseq, tycon, ty))
        = if checkTyVarSeq tyvarseq then
              raise S.SyntaxError ([], "No tyvarseq may contain the same tyvar twice")
          else
              doTy ty

    val invalidBoundNames = List.foldl S.VIdSet.add' S.VIdSet.empty [S.MkVId "true", S.MkVId "false", S.MkVId "nil", S.MkVId "::", S.MkVId "ref"]

    (* doExBind : S.DatBind -> unit *)
    fun doDatBind(S.DatBind(_, tyvarseq, tycon, conbinds))
        = if checkTyVarSeq tyvarseq then
              raise S.SyntaxError ([], "No tyvarseq may contain the same tyvar twice")
          else
              doConBinds(S.VIdSet.empty, conbinds)
    and doConBinds(seen, []) = ()
      | doConBinds(seen, S.ConBind(_, vid, tyopt) :: conbinds) = if S.VIdSet.member(seen, vid) then
                                                                     raise S.SyntaxError ([], "No datbind may bind the same identifier twice")
                                                                 else
                                                                     if vid = S.MkVId "it" orelse S.VIdSet.member(invalidBoundNames, vid) then
                                                                         raise S.SyntaxError ([], "Invalid bound name")
                                                                     else
                                                                         doConBinds(S.VIdSet.add(seen, vid), conbinds)

    (* doExBind : S.ExBind -> unit *)
    fun doExBind(_: S.ExBind): unit = raise Fail "not implemented yet"

    (* doExp : S.TyVarSet * S.Exp -> unit *)
    (* doDec : S.TyVarSet * S.Dec -> unit *)
    (* doValBind : S.TyVarSet * S.ValBind -> unit *)
    fun doExp(tyvarenv : S.TyVarSet.set, S.SConExp _) = ()
      | doExp(tyvarenv, S.VarExp _) = ()
      | doExp(tyvarenv, S.RecordExp(_, row)) = if checkRow row then
                                               raise S.SyntaxError ([], "No expression row may bind the same label twice")
                                               else
                                                   ()
      | doExp(tyvarenv, S.LetInExp(_, decls, exp)) = (List.app (fn dec => doDec(tyvarenv, dec)) decls ; doExp(tyvarenv, exp))
      | doExp(tyvarenv, S.AppExp(_, e1, e2)) = (doExp(tyvarenv, e1) ; doExp(tyvarenv, e2))
      | doExp(tyvarenv, S.TypedExp(_, exp, ty)) = (doExp(tyvarenv, exp) ; doTy ty)
      | doExp _ = raise Fail "not implemented yet"
    and doDec(tyvarenv : S.TyVarSet.set, S.ValDec(_, tyvarseq, valbind)) = raise Fail "not implemented yet"
      | doDec(tyvarenv : S.TyVarSet.set, S.RecValDec(_, tyvarseq, valbind)) = raise Fail "not implemented yet"
      | doDec _ = raise Fail "not implemented yet"
    and doValBind(tyvarenv, S.PatBind(_, pat, exp)) = (doPat pat ; doExp(tyvarenv, exp))
in
val checkExp = doExp
val checkDec = doDec
end (* local *)
end (* structure PostParsing *)
