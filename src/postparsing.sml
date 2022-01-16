(*
 * Copyright (c) 2021 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure Fixity = struct

type Context = { nextVId : int ref
               }

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
           , funMap : IdStatusMap Syntax.FunIdMap.map
           }

val emptyIdStatusMap : IdStatusMap = { valMap = Syntax.VIdMap.empty
                                     , tyConMap = Syntax.TyConMap.empty
                                     , strMap = Syntax.StrIdMap.empty
                                     }

val emptyEnv : Env = { fixityMap = Syntax.VIdMap.empty
                     , idStatusMap = emptyIdStatusMap
                     , sigMap = Syntax.SigIdMap.empty
                     , funMap = Syntax.FunIdMap.empty
                     }

(* used by fixity declaration *)
fun envWithFixityMap fixityMap : Env = { fixityMap = fixityMap
                                       , idStatusMap = emptyIdStatusMap
                                       , sigMap = Syntax.SigIdMap.empty
                                       , funMap = Syntax.FunIdMap.empty
                                       }

fun envWithIdStatusMap idStatusMap : Env = { fixityMap = Syntax.VIdMap.empty
                                           , idStatusMap = idStatusMap
                                           , sigMap = Syntax.SigIdMap.empty
                                           , funMap = Syntax.FunIdMap.empty
                                           }

(* used by signature binding *)
fun envWithSigMap sigMap : Env = { fixityMap = Syntax.VIdMap.empty
                                 , idStatusMap = emptyIdStatusMap
                                 , sigMap = sigMap
                                 , funMap = Syntax.FunIdMap.empty
                                 }

fun envWithFunMap funMap : Env = { fixityMap = Syntax.VIdMap.empty
                                 , idStatusMap = emptyIdStatusMap
                                 , sigMap = Syntax.SigIdMap.empty
                                 , funMap = funMap
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
      , funMap = Syntax.FunIdMap.unionWith #2 (#funMap env1, #funMap env2)
      }

fun getFixityStatus ({ fixityMap, ... } : Env, vid)
    = case Syntax.VIdMap.find (fixityMap, vid) of
          SOME a => a
        | NONE => Syntax.Nonfix

fun isConstructor ({ idStatusMap = { valMap, ... }, ... } : Env, vid)
    = case Syntax.VIdMap.find (valMap, vid) of
          SOME (Syntax.ValueConstructor _) => true
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

fun freshVId(ctx : Context, name) = let val n = !(#nextVId ctx)
                                    in #nextVId ctx := n + 1
                                     ; Syntax.GeneratedVId(name, n)
                                    end

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
  | doPat(ctx, env, UnfixedSyntax.RecordPat{sourceSpan, fields, ellipsis}) = Syntax.RecordPat { sourceSpan = sourceSpan, fields = List.map (fn (label, pat) => (label, doPat(ctx, env, pat))) fields, ellipsis = Option.map (fn pat => doPat(ctx, env, pat)) ellipsis }
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
  | doPat(ctx, env, UnfixedSyntax.VectorPat(span, pats, ellipsis)) = Syntax.VectorPat(span, Vector.map (fn pat => doPat(ctx, env, pat)) pats, ellipsis)
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
  | doExp(ctx, env, UnfixedSyntax.WhileDoExp(span, e1, e2))
    = let val fnName = freshVId(ctx, "loop")
          val fnCall = Syntax.AppExp(span, Syntax.VarExp(span, Syntax.MkQualified([], fnName)), Syntax.RecordExp(span, []))
      in Syntax.LetInExp( span
                        , [ Syntax.RecValDec( span
                                            , [] (* tyvars *)
                                            , [Syntax.PatBind( span
                                                             , Syntax.VarPat(span, fnName)
                                                             , Syntax.FnExp( span
                                                                           , [ ( Syntax.RecordPat { sourceSpan = span, fields = [], ellipsis = NONE }
                                                                               , Syntax.IfThenElseExp( span
                                                                                                     , doExp(ctx, env, e1)
                                                                                                     , Syntax.LetInExp( span
                                                                                                                      , [ Syntax.ValDec( span
                                                                                                                                       , [] (* tyvars *)
                                                                                                                                       , [ Syntax.PatBind(span, Syntax.WildcardPat span, doExp(ctx, env, e2)) ]
                                                                                                                                       )
                                                                                                                        ]
                                                                                                                      , fnCall
                                                                                                                      )
                                                                                                     , Syntax.RecordExp(span, [])
                                                                                                     )
                                                                               )
                                                                             ]
                                                                           )
                                                             )
                                              ]
                                            )
                          ]
                        , fnCall
                        )
      end
  | doExp(ctx, env, UnfixedSyntax.CaseExp(span, exp, matches)) = Syntax.CaseExp(span, doExp(ctx, env, exp), List.map (fn (pat, exp') => (doPat(ctx, env, pat), doExp(ctx, env, exp'))) matches)
  | doExp(ctx, env, UnfixedSyntax.FnExp(span, matches)) = Syntax.FnExp(span, List.map (fn (pat, exp) => (doPat(ctx, env, pat), doExp(ctx, env, exp))) matches)
  | doExp(ctx, env, UnfixedSyntax.ProjectionExp(span, lab)) = Syntax.ProjectionExp(span, lab)
  | doExp(ctx, env, UnfixedSyntax.ListExp(span, xs)) = Syntax.ListExp(span, Vector.map (fn e => doExp(ctx, env, e)) xs)
  | doExp(ctx, env, UnfixedSyntax.VectorExp(span, xs)) = Syntax.VectorExp(span, Vector.map (fn e => doExp(ctx, env, e)) xs)
  | doExp(ctx, env, UnfixedSyntax.PrimValExp(span, name)) = Syntax.VarExp(span, Syntax.MkQualified([], Syntax.MkVId name))
  | doExp(ctx, env, UnfixedSyntax.PrimExp(span, name, tyargs, args))
    = let val primOp = case name of
                           (* PRIMITIVES *)
                           "call2" => Syntax.PrimOp_call2
                         | "call3" => Syntax.PrimOp_call3
                         | "Ref.:=" => Syntax.PrimOp_Ref_set
                         | "Ref.!" => Syntax.PrimOp_Ref_read
                         | "Bool.not" => Syntax.PrimOp_Bool_not
                         | "Int.<" => Syntax.PrimOp_Int_LT
                         | "Int.<=" => Syntax.PrimOp_Int_LE
                         | "Int.>" => Syntax.PrimOp_Int_GT
                         | "Int.>=" => Syntax.PrimOp_Int_GE
                         | "Word.+" => Syntax.PrimOp_Word_PLUS
                         | "Word.-" => Syntax.PrimOp_Word_MINUS
                         | "Word.*" => Syntax.PrimOp_Word_TIMES
                         | "Word.~" => Syntax.PrimOp_Word_TILDE
                         | "Real.+" => Syntax.PrimOp_Real_PLUS
                         | "Real.-" => Syntax.PrimOp_Real_MINUS
                         | "Real.*" => Syntax.PrimOp_Real_TIMES
                         | "Real./" => Syntax.PrimOp_Real_DIVIDE
                         | "Real.~" => Syntax.PrimOp_Real_TILDE
                         | "Real.<" => Syntax.PrimOp_Real_LT
                         | "Real.<=" => Syntax.PrimOp_Real_LE
                         | "Real.>" => Syntax.PrimOp_Real_GT
                         | "Real.>=" => Syntax.PrimOp_Real_GE
                         | "Char.<" => Syntax.PrimOp_Char_LT
                         | "Char.<=" => Syntax.PrimOp_Char_LE
                         | "Char.>" => Syntax.PrimOp_Char_GT
                         | "Char.>=" => Syntax.PrimOp_Char_GE
                         | "String.<" => Syntax.PrimOp_String_LT
                         | "String.<=" => Syntax.PrimOp_String_LE
                         | "String.>" => Syntax.PrimOp_String_GT
                         | "String.>=" => Syntax.PrimOp_String_GE
                         | "String.^" => Syntax.PrimOp_String_HAT
                         | "String.size" => Syntax.PrimOp_String_size
                         | "Vector.length" => Syntax.PrimOp_Vector_length
                         | "Array.length" => Syntax.PrimOp_Array_length
                         | "Unsafe.cast" => Syntax.PrimOp_Unsafe_cast
                         | "Unsafe.Vector.sub" => Syntax.PrimOp_Unsafe_Vector_sub
                         | "Unsafe.Array.sub" => Syntax.PrimOp_Unsafe_Array_sub
                         | "Unsafe.Array.update" => Syntax.PrimOp_Unsafe_Array_update
                         | "Lua.sub" => Syntax.PrimOp_Lua_sub
                         | "Lua.set" => Syntax.PrimOp_Lua_set
                         | "Lua.isNil" => Syntax.PrimOp_Lua_isNil
                         | "Lua.==" => Syntax.PrimOp_Lua_EQUAL
                         | "Lua.~=" => Syntax.PrimOp_Lua_NOTEQUAL
                         | "Lua.<" => Syntax.PrimOp_Lua_LT
                         | "Lua.>" => Syntax.PrimOp_Lua_GT
                         | "Lua.<=" => Syntax.PrimOp_Lua_LE
                         | "Lua.>=" => Syntax.PrimOp_Lua_GE
                         | "Lua.+" => Syntax.PrimOp_Lua_PLUS
                         | "Lua.-" => Syntax.PrimOp_Lua_MINUS
                         | "Lua.*" => Syntax.PrimOp_Lua_TIMES
                         | "Lua./" => Syntax.PrimOp_Lua_DIVIDE
                         | "Lua.//" => Syntax.PrimOp_Lua_INTDIV
                         | "Lua.%" => Syntax.PrimOp_Lua_MOD
                         | "Lua.pow" => Syntax.PrimOp_Lua_pow
                         | "Lua.unm" => Syntax.PrimOp_Lua_unm
                         | "Lua.andb" => Syntax.PrimOp_Lua_andb
                         | "Lua.orb" => Syntax.PrimOp_Lua_orb
                         | "Lua.xorb" => Syntax.PrimOp_Lua_xorb
                         | "Lua.notb" => Syntax.PrimOp_Lua_notb
                         | "Lua.<<" => Syntax.PrimOp_Lua_LSHIFT
                         | "Lua.>>" => Syntax.PrimOp_Lua_RSHIFT
                         | "Lua.concat" => Syntax.PrimOp_Lua_concat
                         | "Lua.length" => Syntax.PrimOp_Lua_length
                         | "Lua.isFalsy" => Syntax.PrimOp_Lua_isFalsy
                         | _ => emitError(ctx, [span], "unknown primop: " ^ String.toString name)
          val args = Vector.map (fn e => doExp(ctx, env, e)) args
      in Syntax.PrimExp(span, primOp, tyargs, args)
      end
and doDecs(ctx, env, nil) = (emptyEnv, nil)
  | doDecs(ctx, env, dec :: decs) = let val (env', dec') = doDec(ctx, env, dec)
                                        val (env'', decs') = doDecs(ctx, mergeEnv(env, env'), decs)
                                    in (mergeEnv(env', env''), dec' @ decs')
                                    end
and doDec(ctx, env, UnfixedSyntax.ValDec(span, tyvars, valbind)) = (emptyEnv, [Syntax.ValDec(span, tyvars, List.map (fn vb => doValBind(ctx, env, vb)) valbind)])
  | doDec(ctx, env, UnfixedSyntax.RecValDec(span, tyvars, valbind)) = (emptyEnv, [Syntax.RecValDec(span, tyvars, List.map (fn vb => doValBind(ctx, env, vb)) valbind)])
  | doDec(ctx, env, UnfixedSyntax.FValDec(span, tyvars, fvalbind)) = (emptyEnv, [Syntax.RecValDec(span, tyvars, List.map (fn fvb => doFValBind(ctx, env, fvb)) fvalbind)])
  | doDec(ctx, env, UnfixedSyntax.TypeDec(span, typbinds)) = (emptyEnv, [Syntax.TypeDec(span, typbinds)])
  | doDec(ctx, env, UnfixedSyntax.DatatypeDec(span, datbinds, typbinds))
    = let val doConBinds : Syntax.ConBind list -> Syntax.IdStatus Syntax.VIdMap.map
              = fn conbinds =>
                   let val isSoleConstructor = case conbinds of
                                                   [_] => true
                                                 | _ => false
                       val idstatus = Syntax.ValueConstructor isSoleConstructor
                   in List.foldl (fn (Syntax.ConBind(_, vid, _), map) =>
                                     (* Syntactic Restriction: vid must not be one of "true", "false", "nil", "::" or "ref". *)
                                     Syntax.VIdMap.insert(map, vid, idstatus)) Syntax.VIdMap.empty conbinds
                   end
          val tyConMap = List.foldl (fn (Syntax.DatBind(span, tyvars, tycon, conbinds), map) => Syntax.TyConMap.insert(map, tycon, doConBinds conbinds)) Syntax.TyConMap.empty datbinds
          val valMap = Syntax.TyConMap.foldl (Syntax.VIdMap.unionWith #2 (* should be disjoint *)) Syntax.VIdMap.empty tyConMap
          val idStatusMap = { valMap = valMap
                            , tyConMap = tyConMap
                            , strMap = Syntax.StrIdMap.empty
                            }
      in (envWithIdStatusMap idStatusMap, [Syntax.DatatypeDec(span, datbinds, typbinds)])
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
  | doDec(ctx, env, UnfixedSyntax.AbstypeDec(span, datbinds, typbinds, decs))
    = let val doConBinds : Syntax.ConBind list -> Syntax.IdStatus Syntax.VIdMap.map
              = fn conbinds =>
                   let val isSoleConstructor = case conbinds of
                                                   [_] => true
                                                 | _ => false
                       val idstatus = Syntax.ValueConstructor isSoleConstructor
                   in List.foldl (fn (Syntax.ConBind(_, vid, _), map) =>
                                     (* Syntactic Restriction: vid must not be one of "true", "false", "nil", "::" or "ref". *)
                                     Syntax.VIdMap.insert(map, vid, idstatus)) Syntax.VIdMap.empty conbinds
                   end
          val tyConMap = List.foldl (fn (Syntax.DatBind(span, tyvars, tycon, conbinds), map) => Syntax.TyConMap.insert(map, tycon, doConBinds conbinds)) Syntax.TyConMap.empty datbinds
          val valMap = Syntax.TyConMap.foldl (Syntax.VIdMap.unionWith #2 (* should be disjoint *)) Syntax.VIdMap.empty tyConMap
          val idStatusMap = { valMap = valMap
                            , tyConMap = tyConMap
                            , strMap = Syntax.StrIdMap.empty
                            }
          val innerEnv = mergeEnv(env, envWithIdStatusMap idStatusMap)
          val (env, decs') = doDecs(ctx, innerEnv, decs) (* not really implemented yet *)
      in (env, [Syntax.AbstypeDec(span, datbinds, typbinds, decs')])
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
  | doDec(ctx, env, UnfixedSyntax.OverloadDec(span, class, longtycon, map))
    = let val class = case class of
                          "Int" => Syntax.CLASS_INT
                        | "Word" => Syntax.CLASS_WORD
                        | "Real" => Syntax.CLASS_REAL
                        | "Char" => Syntax.CLASS_CHAR
                        | "String" => Syntax.CLASS_STRING
                        | _ => emitError(ctx, [span], "unknown overload class: " ^ class)
          val keys = case class of
                         Syntax.CLASS_INT => [("+", Syntax.OVERLOAD_PLUS)
                                             ,("-", Syntax.OVERLOAD_MINUS)
                                             ,("*", Syntax.OVERLOAD_TIMES)
                                             ,("div", Syntax.OVERLOAD_div)
                                             ,("mod", Syntax.OVERLOAD_mod)
                                             ,("abs", Syntax.OVERLOAD_abs)
                                             ,("~", Syntax.OVERLOAD_TILDE)
                                             ,("<", Syntax.OVERLOAD_LT)
                                             ,("<=", Syntax.OVERLOAD_LE)
                                             ,(">", Syntax.OVERLOAD_GT)
                                             ,(">=", Syntax.OVERLOAD_GE)
                                             ,("fromInt", Syntax.OVERLOAD_fromInt)
                                             ]
                       | Syntax.CLASS_WORD => [("+", Syntax.OVERLOAD_PLUS)
                                              ,("-", Syntax.OVERLOAD_MINUS)
                                              ,("*", Syntax.OVERLOAD_TIMES)
                                              ,("div", Syntax.OVERLOAD_div)
                                              ,("mod", Syntax.OVERLOAD_mod)
                                              ,("~", Syntax.OVERLOAD_TILDE)
                                              ,("<", Syntax.OVERLOAD_LT)
                                              ,("<=", Syntax.OVERLOAD_LE)
                                              ,(">", Syntax.OVERLOAD_GT)
                                              ,(">=", Syntax.OVERLOAD_GE)
                                              ,("fromWord", Syntax.OVERLOAD_fromWord)
                                              ]
                       | Syntax.CLASS_REAL => [("+", Syntax.OVERLOAD_PLUS)
                                              ,("-", Syntax.OVERLOAD_MINUS)
                                              ,("*", Syntax.OVERLOAD_TIMES)
                                              ,("/", Syntax.OVERLOAD_DIVIDE)
                                              ,("abs", Syntax.OVERLOAD_abs)
                                              ,("~", Syntax.OVERLOAD_TILDE)
                                              ,("<", Syntax.OVERLOAD_LT)
                                              ,("<=", Syntax.OVERLOAD_LE)
                                              ,(">", Syntax.OVERLOAD_GT)
                                              ,(">=", Syntax.OVERLOAD_GE)
                                              ]
                       | Syntax.CLASS_CHAR => [("<", Syntax.OVERLOAD_LT)
                                              ,("<=", Syntax.OVERLOAD_LE)
                                              ,(">", Syntax.OVERLOAD_GT)
                                              ,(">=", Syntax.OVERLOAD_GE)
                                              ]
                       | Syntax.CLASS_STRING => [("<", Syntax.OVERLOAD_LT)
                                                ,("<=", Syntax.OVERLOAD_LE)
                                                ,(">", Syntax.OVERLOAD_GT)
                                                ,(">=", Syntax.OVERLOAD_GE)
                                                ]
          fun isSimpleExp (Syntax.VarExp _) = true
            | isSimpleExp (Syntax.TypedExp (_, exp, _)) = isSimpleExp exp
            | isSimpleExp _ = false
          val (keys, map) = List.foldl (fn ((name, exp), (keys, map)) =>
                                           let val exp = doExp(ctx, env, exp)
                                               val () = if isSimpleExp exp then
                                                            ()
                                                        else
                                                            emitError(ctx, [span], "the RHS of an overload declaration must be a simple expression")
                                           in case List.find (fn (name', _) => name = name') keys of
                                                  NONE => emitError(ctx, [span], "invalid overload declaration")
                                                | SOME (_, key) => (List.filter (fn (name', _) => name <> name') keys, Syntax.OverloadKeyMap.insert (map, key, exp))
                                           end
                                       ) (keys, Syntax.OverloadKeyMap.empty) map
          val () = case keys of
                       [] => ()
                     | (name, _) :: _ => emitError(ctx, [span], "missing key: " ^ name)
      in (emptyEnv, [Syntax.OverloadDec(span, class, longtycon, map)])
      end
and doValBind(ctx, env, UnfixedSyntax.PatBind(span, pat, exp)) = Syntax.PatBind(span, doPat(ctx, env, pat), doExp(ctx, env, exp))
and doFValBind(ctx, env, UnfixedSyntax.FValBind(span, rules)) : Syntax.ValBind
    = let fun doFMRule (UnfixedSyntax.FMRule(_, fpat, optTy, exp)) = (doFPat(ctx, env, fpat), optTy, doExp(ctx, env, exp))
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
          fun buildExp(0, [paramId]) = let fun doRule ((_, _, [pat]), optTy, exp) = let val exp = case optTy of
                                                                                                      NONE => exp 
                                                                                                    | SOME expTy => Syntax.TypedExp(Syntax.getSourceSpanOfExp exp, exp, expTy)
                                                                                    in (pat, exp)
                                                                                    end
                                             | doRule _ = emitError(ctx, [span], "invalid function declaration")
                                       in Syntax.CaseExp(span, Syntax.VarExp(span, Syntax.MkQualified([], paramId)), List.map doRule rules')
                                       end
            | buildExp(0, revParams) = let val params = List.rev revParams
                                           val paramTuple = Syntax.TupleExp(span, List.map (fn vid => Syntax.VarExp(span, Syntax.MkQualified([], vid))) params)
                                           fun doRule ((_, _, pats), optTy, exp) = let val pat = Syntax.TuplePat(span, pats)
                                                                                       val exp = case optTy of
                                                                                                     NONE => exp 
                                                                                                   | SOME expTy => Syntax.TypedExp(Syntax.getSourceSpanOfExp exp, exp, expTy)
                                                                                   in (pat, exp)
                                                                                   end
                                       in Syntax.CaseExp(span, paramTuple, List.map doRule rules')
                                       end
            | buildExp(n, revParams) = let val paramId = freshVId(ctx, "a")
                                       in Syntax.FnExp(span, [(Syntax.VarPat(span, paramId), buildExp(n - 1, paramId :: revParams))])
                                       end
      in Syntax.PatBind(span, Syntax.VarPat(span, vid), buildExp(arity, []))
      end
(* (<pat1> <vid> <pat3>) -> <vid> is infix function name *)
and doFPat(ctx, env, UnfixedSyntax.FPat(span1, [UnfixedSyntax.JuxtapositionPat(span2, [pat1, UnfixedSyntax.InfixOrVIdPat(span3, vid), pat3])])) : SourcePos.span * Syntax.VId * Syntax.Pat list
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
fun doSigExp(ctx, env, Syntax.BasicSigExp(span, specs)) : IdStatusMap = doSpecs(ctx, env, specs)
  | doSigExp(ctx, env, Syntax.SigIdExp(span, sigid)) = (case Syntax.SigIdMap.find(#sigMap env, sigid) of
                                                            SOME m => m
                                                          | NONE => emitError(ctx, [span], "signature not found: " ^ Syntax.print_SigId sigid)
                                                       )
  | doSigExp(ctx, env, Syntax.TypeRealisationExp(span, sigexp, tyvars, longtycon, ty)) = doSigExp(ctx, env, sigexp) (* does not affect idstatus *)
and doSpecs(ctx, env, specs) = List.foldl (fn (spec, m) => mergeIdStatusMap(m, doSpec(ctx, mergeEnv(env, envWithIdStatusMap m), spec))) emptyIdStatusMap specs
and doSpec(ctx, env, Syntax.ValDesc(span, descs)) = emptyIdStatusMap
  | doSpec(ctx, env, Syntax.TypeDesc(span, descs)) = emptyIdStatusMap
  | doSpec(ctx, env, Syntax.EqtypeDesc(span, descs)) = emptyIdStatusMap
  | doSpec(ctx, env, Syntax.DatDesc(span, descs, typbinds)) = List.foldl (fn ((tyvars, tycon, condescs), { valMap, tyConMap, strMap }) =>
                                                                             let val isSoleConstructor = case condescs of
                                                                                                             [_] => true
                                                                                                           | _ => false
                                                                                 val idstatus = Syntax.ValueConstructor isSoleConstructor
                                                                                 val valMap' = List.foldl (fn (Syntax.ConBind(_, vid, _), m) => Syntax.VIdMap.insert(m, vid, idstatus)) Syntax.VIdMap.empty condescs
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
  | doSpec(ctx, env, Syntax.Sharing(span, specs, longtycons)) = doSpecs(ctx, env, specs)
  | doSpec(ctx, env, Syntax.SharingStructure(span, specs, longstrids)) = doSpecs(ctx, env, specs)
  | doSpec(ctx, env, Syntax.TypeAliasDesc(span, descs)) = emptyIdStatusMap
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
  | doStrExp(ctx, env, Syntax.TransparentConstraintExp(span, strexp, sigexp)) = let val env' = doSigExp(ctx, env, sigexp)
                                                                                in (env', Syntax.TransparentConstraintExp(span, #2 (doStrExp(ctx, env, strexp)), sigexp))
                                                                                end
  | doStrExp(ctx, env, Syntax.OpaqueConstraintExp(span, strexp, sigexp)) = let val env' = doSigExp(ctx, env, sigexp)
                                                                           in (env', Syntax.OpaqueConstraintExp(span, #2 (doStrExp(ctx, env, strexp)), sigexp))
                                                                           end
  | doStrExp(ctx, env, Syntax.FunctorAppExp(span, funid, strexp)) = let val (_, strexp') = doStrExp(ctx, env, strexp)
                                                                        val env' = case Syntax.FunIdMap.find(#funMap env, funid) of
                                                                                       SOME m => m
                                                                                     | NONE => emitError(ctx, [span], "functor not found: " ^ Syntax.print_FunId funid)
                                                                    in (env', Syntax.FunctorAppExp(span, funid, strexp'))
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
fun doTopDec(ctx, env, Syntax.StrDec(strdec)) = let val (env, strdecs) = doStrDec(ctx, env, strdec)
                                                in (env, List.map Syntax.StrDec strdecs)
                                                end
  | doTopDec(ctx, env, Syntax.SigDec(sigbinds)) = let val sigMap = List.foldl (fn ((sigid, sigexp), m) => Syntax.SigIdMap.insert(m, sigid, doSigExp(ctx, env, sigexp))) Syntax.SigIdMap.empty sigbinds
                                                  in (envWithSigMap sigMap, [Syntax.SigDec(sigbinds)])
                                                  end
  | doTopDec(ctx, env, Syntax.FunDec funbinds)
    = let val funbinds' : (SourcePos.span * Syntax.FunId * IdStatusMap * Syntax.Dec Syntax.FunExp) list
              = List.map (fn (span, funid, Syntax.NamedFunExp (strid, sigexp, strexp)) =>
                             let val sigexp' : IdStatusMap = doSigExp(ctx, env, sigexp)
                                 val (ids, strexp') = doStrExp(ctx, mergeEnv(env, envWithIdStatusMap { valMap = Syntax.VIdMap.empty
                                                                                                     , tyConMap = Syntax.TyConMap.empty
                                                                                                     , strMap = Syntax.StrIdMap.singleton (strid, MkIdStatusMap sigexp')
                                                                                                     }
                                                                            ), strexp)
                             in (span, funid, ids, Syntax.NamedFunExp (strid, sigexp, strexp'))
                             end
                         | (span, funid, Syntax.AnonymousFunExp (sigexp, strexp)) =>
                           let val sigexp' : IdStatusMap = doSigExp(ctx, env, sigexp)
                               val (ids, strexp') = doStrExp(ctx, mergeEnv(env, envWithIdStatusMap sigexp'), strexp)
                           in (span, funid, ids, Syntax.AnonymousFunExp (sigexp, strexp'))
                           end
                         ) funbinds
         val env' = envWithFunMap (List.foldl (fn ((_, funid, ids, _), m) => Syntax.FunIdMap.insert(m, funid, ids)) Syntax.FunIdMap.empty funbinds')
      in (env', [Syntax.FunDec (List.map (fn (span, funid, _, exp) => (span, funid, exp)) funbinds')])
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

structure PostParsing : sig
              val freeTyVarsInTy : Syntax.TyVarSet.set * Syntax.Ty -> Syntax.TyVarSet.set
              val freeTyVarsInPat : Syntax.TyVarSet.set * Syntax.Pat -> Syntax.TyVarSet.set
              val scopeTyVarsInDecs : Syntax.TyVarSet.set * Syntax.Dec list -> Syntax.Dec list
              val scopeTyVarsInDec : Syntax.TyVarSet.set * Syntax.Dec -> Syntax.Dec
              val scopeTyVarsInExp : Syntax.TyVarSet.set * Syntax.Exp -> Syntax.Exp
              val scopeTyVarsInStrDec : Syntax.Dec Syntax.StrDec -> Syntax.Dec Syntax.StrDec
              val scopeTyVarsInTopDecs : Syntax.Dec Syntax.TopDec list -> Syntax.Dec Syntax.TopDec list
              val scopeTyVarsInProgram : Syntax.Program -> Syntax.Program
              end = struct
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
  | freeTyVarsInPat(bound, VectorPat(_, pats, _)) = Vector.foldl (fn (pat, set) => TyVarSet.union(freeTyVarsInPat(bound, pat), set)) TyVarSet.empty pats

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
      | collectExp(bound, CaseExp(_, x, match)) = TyVarSet.union(collectExp(bound, x), collectMatch(bound, match))
      | collectExp(bound, FnExp(_, match)) = collectMatch(bound, match)
      | collectExp(bound, ProjectionExp(_, lab)) = TyVarSet.empty
      | collectExp(bound, ListExp(_, xs)) = Vector.foldl (fn (e, set) => TyVarSet.union(collectExp(bound, e), set)) TyVarSet.empty xs
      | collectExp(bound, VectorExp(_, xs)) = Vector.foldl (fn (e, set) => TyVarSet.union(collectExp(bound, e), set)) TyVarSet.empty xs
      | collectExp(bound, PrimExp(_, _, tyargs, args)) = let val acc = Vector.foldl (fn (ty, set) => TyVarSet.union(set, TyVarSet.difference(freeTyVarsInTy(bound, ty), bound))) TyVarSet.empty tyargs
                                                         in Vector.foldl (fn (e, set) => TyVarSet.union(collectExp(bound, e), set)) acc args
                                                         end
    and collectMatch(bound, xs) = List.foldl (fn ((pat, e), set) => TyVarSet.union(freeTyVarsInPat(bound, pat), TyVarSet.union(collectExp(bound, e), set))) TyVarSet.empty xs
    and collectValBind(bound, PatBind(_, pat, e)) = TyVarSet.union(freeTyVarsInPat(bound, pat), collectExp(bound, e))
    and collectFRule(bound, (pats, optTy, exp)) = let val tyVarsInPats = List.foldl TyVarSet.union TyVarSet.empty (List.map (fn pat => freeTyVarsInPat(bound, pat)) pats)
                                                      val tyVarsInOptTy = case optTy of
                                                                              NONE => TyVarSet.empty
                                                                            | SOME expTy => TyVarSet.difference(freeTyVarsInTy(bound, expTy), bound)
                                                  in union3(tyVarsInPats, tyVarsInOptTy, collectExp(bound, exp))
                                                  end
    and collectDec(bound, ValDec _) = TyVarSet.empty
      | collectDec(bound, RecValDec _) = TyVarSet.empty
      | collectDec(bound, TypeDec(_, typbinds)) = List.foldl (fn (TypBind (_, tyvars, _, ty), acc) => TyVarSet.union(freeTyVarsInTy(TyVarSet.addList(bound, tyvars), ty), acc)) TyVarSet.empty typbinds
      | collectDec(bound, DatatypeDec(_, datbinds, typbinds)) = List.foldl (fn (datbind, acc) => TyVarSet.union(collectDatBind(bound, datbind), acc)) (List.foldl (fn (TypBind (_, tyvars, _, ty), acc) => TyVarSet.union(freeTyVarsInTy(TyVarSet.addList(bound, tyvars), ty), acc)) TyVarSet.empty typbinds) datbinds
      | collectDec(bound, DatatypeRepDec(_, _, _)) = TyVarSet.empty
      | collectDec(bound, AbstypeDec(_, datbinds, typbinds, decs)) = let val acc = List.foldl (fn (TypBind (_, tyvars, _, ty), acc) => TyVarSet.union(freeTyVarsInTy(TyVarSet.addList(bound, tyvars), ty), acc)) TyVarSet.empty typbinds
                                                                         val acc = List.foldl (fn (datbind, acc) => TyVarSet.union(collectDatBind(bound, datbind), acc)) acc datbinds
                                                                     in List.foldl (fn (dec, acc) => TyVarSet.union(collectDec(bound, dec), acc)) acc decs
                                                                     end
      | collectDec(bound, ExceptionDec(span, exbinds)) = List.foldl (fn (ExBind(span, vid, SOME ty), acc) => TyVarSet.union(freeTyVarsInTy(bound, ty), acc)
                                                                    | (ExBind(span, vid, NONE), acc) => acc
                                                                    | (ExReplication(_, _, _), acc) => acc) TyVarSet.empty exbinds
      | collectDec(bound, LocalDec(_, decs1, decs2)) = List.foldl (fn (dec, acc) => TyVarSet.union(collectDec(bound, dec), acc)) (List.foldl (fn (dec, acc) => TyVarSet.union(collectDec(bound, dec), acc)) TyVarSet.empty decs1) decs2
      | collectDec(bound, OpenDec _) = TyVarSet.empty
      | collectDec(bound, OverloadDec(_, _, _, map)) = OverloadKeyMap.foldl (fn (exp, acc) => TyVarSet.union(acc, collectExp(bound, exp))) TyVarSet.empty map
    and collectDatBind(bound, DatBind (_, tyvars, _, conbinds)) = let val bound = TyVarSet.addList(bound, tyvars)
                                                                      fun doConBind(ConBind(_, _, NONE)) = TyVarSet.empty
                                                                        | doConBind(ConBind(_, _, SOME ty)) = freeTyVarsInTy(bound, ty)
                                                                  in List.foldl (fn (conbind, acc) => TyVarSet.union(doConBind conbind, acc)) TyVarSet.empty conbinds
                                                                  end
in
val unguardedTyVarsInExp : TyVarSet.set * Exp -> TyVarSet.set = collectExp
val unguardedTyVarsInValBind : TyVarSet.set * ValBind list -> TyVarSet.set = fn (bound, valbinds) => List.foldl (fn (valbind, set) => TyVarSet.union(set, collectValBind(bound, valbind))) TyVarSet.empty valbinds
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
      | doDec(bound, dec as TypeDec _) = dec
      | doDec(bound, dec as DatatypeDec _) = dec
      | doDec(bound, dec as DatatypeRepDec _) = dec
      | doDec(bound, AbstypeDec(span, datbinds, typbinds, decs)) = AbstypeDec(span, datbinds, typbinds, doDecList(bound, decs))
      | doDec(bound, dec as ExceptionDec _) = dec
      | doDec(bound, LocalDec(span, xs, ys)) = LocalDec(span, doDecList(bound, xs), doDecList(bound, ys))
      | doDec(bound, dec as OpenDec _) = dec
      | doDec(bound, dec as OverloadDec _) = dec
    and doDecList(bound, decls) = List.map (fn x => doDec(bound, x)) decls
    and doValBind(bound, PatBind(span, pat, e)) = PatBind(span, pat, doExp(bound, e))
    and doExp(bound, exp as SConExp _) = exp
      | doExp(bound, exp as VarExp _) = exp
      | doExp(bound, exp as RecordExp _) = exp
      | doExp(bound, LetInExp(span, decls, exp)) = LetInExp(span, doDecList(bound, decls), doExp(bound, exp))
      | doExp(bound, AppExp(span, x, y)) = AppExp(span, doExp(bound, x), doExp(bound, y))
      | doExp(bound, TypedExp(span, x, ty)) = TypedExp(span, doExp(bound, x), ty)
      | doExp(bound, HandleExp(span, x, match)) = HandleExp(span, doExp(bound, x), doMatch(bound, match))
      | doExp(bound, RaiseExp(span, x)) = RaiseExp(span, doExp(bound, x))
      | doExp(bound, IfThenElseExp(span, x, y, z)) = IfThenElseExp(span, doExp(bound, x), doExp(bound, y), doExp(bound, z))
      | doExp(bound, CaseExp(span, x, match)) = CaseExp(span, doExp(bound, x), doMatch(bound, match))
      | doExp(bound, FnExp(span, match)) = FnExp(span, doMatch(bound, match))
      | doExp(bound, exp as ProjectionExp _) = exp
      | doExp(bound, ListExp(span, xs)) = ListExp(span, Vector.map (fn x => doExp(bound, x)) xs)
      | doExp(bound, VectorExp(span, xs)) = VectorExp(span, Vector.map (fn x => doExp(bound, x)) xs)
      | doExp(bound, PrimExp(span, name, tyargs, args)) = PrimExp(span, name, tyargs, Vector.map (fn x => doExp(bound, x)) args)
    and doMatch(bound, xs) = List.map (fn (pat, exp) => (pat, doExp(bound, exp))) xs
    fun doStrExp(StructExp(span, strdecs)) = StructExp(span, List.map doStrDec strdecs)
      | doStrExp(StrIdExp(span, longstrid)) = StrIdExp(span, longstrid)
      | doStrExp(TransparentConstraintExp(span, strexp, sigexp)) = TransparentConstraintExp(span, doStrExp strexp, sigexp)
      | doStrExp(OpaqueConstraintExp(span, strexp, sigexp)) = OpaqueConstraintExp(span, doStrExp strexp, sigexp)
      | doStrExp(FunctorAppExp(span, funid, strexp)) = FunctorAppExp(span, funid, doStrExp strexp)
      | doStrExp(LetInStrExp(span, strdecs, strexp)) = LetInStrExp(span, doStrDecs strdecs, doStrExp strexp)
    and doStrDec(CoreDec(span, dec)) = CoreDec(span, doDec(TyVarSet.empty, dec))
      | doStrDec(StrBindDec(span, binds)) = StrBindDec(span, List.map (fn (strid, strexp) => (strid, doStrExp strexp)) binds)
      | doStrDec(LocalStrDec(span, decs1, decs2)) = LocalStrDec(span, doStrDecs decs1, List.map doStrDec decs2)
    and doStrDecs(strdecs) = List.map doStrDec strdecs
    fun doFunExp(NamedFunExp(strid, sigexp, strexp)) = NamedFunExp(strid, sigexp, doStrExp strexp)
      | doFunExp(AnonymousFunExp(sigexp, strexp)) = AnonymousFunExp(sigexp, doStrExp strexp)
in
val scopeTyVarsInDecs: TyVarSet.set * Dec list -> Dec list = doDecList
val scopeTyVarsInDec: TyVarSet.set * Dec -> Dec = doDec
val scopeTyVarsInExp: TyVarSet.set * Exp -> Exp = doExp
val scopeTyVarsInStrDec: Dec StrDec -> Dec StrDec = doStrDec
val scopeTyVarsInTopDecs = List.map (fn StrDec(strdec) => StrDec(scopeTyVarsInStrDec(strdec))
                                    | topdec as SigDec _ => topdec
                                    | FunDec funbinds => FunDec (List.map (fn (span, funid, funexp) => (span, funid, doFunExp funexp)) funbinds)
                                    )
val scopeTyVarsInProgram = List.map scopeTyVarsInTopDecs
end (* local *)
end (* local *)
end (* structure PostParsing *)

structure CheckSyntacticRestrictions : sig
              val checkProgram : Syntax.Program -> unit
          end = struct
(* Check syntactic restrictions (The Definition 2.9) *)
structure S = Syntax

(* checkRow : (Label * 'a) list -> bool, returns true if the same label is bound twice *)
fun checkRow (row: (S.Label * 'a) list) = doCheckRow (S.LabelSet.empty, row)
and doCheckRow (seen, []) = false
  | doCheckRow (seen, (label, _) :: xs) = if S.LabelSet.member (seen, label) then
                                              true
                                          else
                                              doCheckRow (S.LabelSet.add (seen, label), xs)

(* checkTyVarSeq : SourcePos.span * TyVar list -> unit *)
fun checkTyVarSeq (span, xs: S.TyVar list) = doCheckTyVarSeq (span, S.TyVarSet.empty, xs)
and doCheckTyVarSeq (span, seen, []) = ()
  | doCheckTyVarSeq (span, seen, tv :: xs) = if S.TyVarSet.member (seen, tv) then
                                                 raise S.SyntaxError ([span], "no tyvarseq may contain the same tyvar twice")
                                             else
                                                 doCheckTyVarSeq (span, S.TyVarSet.add (seen, tv), xs)

(* doTy : S.Ty -> unit *)
fun doTy (S.TyVar span) = ()
  | doTy (S.RecordType (span, fields)) = if checkRow fields then
                                             raise S.SyntaxError ([span], "no type-expression row may bind the same label twice")
                                         else
                                             ()
  | doTy (S.TyCon (span, tyargs, longtycon)) = List.app doTy tyargs
  | doTy (S.FnType (span, s, t)) = ( doTy s ; doTy t )

(* doPat : S.Pat -> unit *)
fun doPat (S.WildcardPat _) = ()
  | doPat (S.SConPat (span, S.RealConstant _)) = raise S.SyntaxError ([span], "no real constant may occur in a pattern")
  | doPat (S.SConPat _) = ()
  | doPat (S.VarPat (_, vid)) = ()
  | doPat (S.RecordPat { sourceSpan, fields, ellipsis }) = if checkRow fields then
                                                               raise S.SyntaxError ([sourceSpan], "no pattern row may bind the same label twice")
                                                           else
                                                               Option.app doPat ellipsis
  | doPat (S.ConPat (_, _, NONE)) = ()
  | doPat (S.ConPat (_, longvid, SOME pat)) = doPat pat
  | doPat (S.TypedPat (_, pat, ty)) = ( doTy ty ; doPat pat )
  | doPat (S.LayeredPat (span, vid, optTy, pat)) = ( Option.app doTy optTy ; doPat pat )
  | doPat (S.VectorPat (span, pats, _)) = Vector.app doPat pats

fun doTypBind (S.TypBind (span, tyvarseq, tycon, ty))
    = ( checkTyVarSeq (span, tyvarseq)
      ; doTy ty
      )

val invalidBoundNames = List.foldl S.VIdSet.add' S.VIdSet.empty [S.MkVId "true", S.MkVId "false", S.MkVId "nil", S.MkVId "::", S.MkVId "ref"]

(* doDatBind : S.DatBind -> unit *)
fun doDatBind (S.DatBind (span, tyvarseq, tycon, conbinds))
    = ( checkTyVarSeq (span, tyvarseq)
      ; List.foldl (fn (S.ConBind (span, vid, optTy), seen) =>
                       if S.VIdSet.member(seen, vid) then
                           raise S.SyntaxError ([span], "no datbind may bind the same identifier twice")
                       else
                           if vid = S.MkVId "it" orelse S.VIdSet.member(invalidBoundNames, vid) then
                               raise S.SyntaxError ([span], "invalid bound name")
                           else
                               S.VIdSet.add(seen, vid)
                   ) S.VIdSet.empty conbinds
      )

(* doExp : S.TyVarSet * S.Exp -> unit *)
(* doDec : S.TyVarSet * S.Dec -> unit *)
(* doValBind : S.TyVarSet * S.ValBind -> unit *)
fun doExp (env : S.TyVarSet.set, S.SConExp span) = ()
  | doExp (env, S.VarExp span) = ()
  | doExp (env, S.RecordExp (span, fields)) = if checkRow fields then
                                                  raise S.SyntaxError ([span], "no expression row may bind the same label twice")
                                              else
                                                  List.app (fn (label, exp) => doExp (env, exp)) fields
  | doExp (env, S.LetInExp (span, decls, exp)) = ( List.app (fn dec => doDec (env, dec)) decls ; doExp (env, exp) )
  | doExp (env, S.AppExp (span, e1, e2)) = ( doExp (env, e1) ; doExp (env, e2) )
  | doExp (env, S.TypedExp (span, exp, ty)) = ( doExp (env, exp) ; doTy ty )
  | doExp (env, S.HandleExp (span, exp, matches)) = ( doExp (env, exp) ; doMatches (env, matches) )
  | doExp (env, S.RaiseExp (span, exp)) = doExp (env, exp)
  | doExp (env, S.IfThenElseExp (span, exp1, exp2, exp3)) = ( doExp (env, exp1) ; doExp (env, exp2) ; doExp (env, exp3) )
  | doExp (env, S.CaseExp (span, exp, matches)) = ( doExp (env, exp) ; doMatches (env, matches) )
  | doExp (env, S.FnExp (span, matches)) = doMatches (env, matches)
  | doExp (env, S.ProjectionExp (span, label)) = ()
  | doExp (env, S.ListExp (span, exps)) = Vector.app (fn exp => doExp (env, exp)) exps
  | doExp (env, S.VectorExp (span, exps)) = Vector.app (fn exp => doExp (env, exp)) exps
  | doExp (env, S.PrimExp (span, primOp, tyargs, args)) = ( Vector.app doTy tyargs ; Vector.app (fn exp => doExp (env, exp)) args )
and doMatches (env, matches) = List.app (fn (pat, exp) => ( doPat pat ; doExp (env, exp) )) matches
and doDec (env : S.TyVarSet.set, S.ValDec (span, tyvarseq, valbinds)) = let val tyvars = S.TyVarSet.fromList tyvarseq
                                                                        in if S.TyVarSet.disjoint (env, S.TyVarSet.fromList tyvarseq) then
                                                                               doValBinds (S.TyVarSet.union (env, tyvars), valbinds)
                                                                           else
                                                                               raise S.SyntaxError ([span], "in a nested value bindings, type variables must be disjoint")
                                                                        end
  | doDec (env : S.TyVarSet.set, S.RecValDec (span, tyvarseq, valbinds)) = let val tyvars = S.TyVarSet.fromList tyvarseq
                                                                           in if S.TyVarSet.disjoint (env, S.TyVarSet.fromList tyvarseq) then
                                                                                  doValBinds (S.TyVarSet.union (env, tyvars), valbinds)
                                                                              else
                                                                                  raise S.SyntaxError ([span], "in a nested value bindings, type variables must be disjoint")
                                                                           end
  | doDec (env, S.TypeDec (span, typbinds)) = ignore (List.foldl (fn (S.TypBind (span, tyvarseq, tycon, ty), set) =>
                                                                     ( checkTyVarSeq (span, tyvarseq)
                                                                     ; doTy ty
                                                                     ; if S.TyConSet.member (set, tycon) then
                                                                           raise S.SyntaxError ([span], "duplicate type constructor in type declaration")
                                                                       else
                                                                           S.TyConSet.add (set, tycon)
                                                                     )
                                                                 ) S.TyConSet.empty typbinds)
  | doDec (env, S.DatatypeDec (span, datbinds, withtypebinds))
    = let val set = #ty (List.foldl (fn (S.DatBind (span, tyvarseq, tycon, conbinds), { v, ty }) =>
                                        ( checkTyVarSeq (span, tyvarseq)
                                        ; { v = List.foldl (fn (S.ConBind (span, vid, optTy), set) =>
                                                               ( Option.app doTy optTy
                                                               ; if Syntax.VIdSet.member (set, vid) then
                                                                     raise S.SyntaxError ([span], "duplicate value identifier in datatype declaration")
                                                                 else
                                                                     Syntax.VIdSet.add (set, vid)
                                                               )
                                                           ) v conbinds
                                          , ty = if Syntax.TyConSet.member (ty, tycon) then
                                                     raise S.SyntaxError ([span], "duplicate type constructor in datatype declaration")
                                                 else
                                                     Syntax.TyConSet.add (ty, tycon)
                                          }
                                        )
                                    ) { v = Syntax.VIdSet.empty, ty = Syntax.TyConSet.empty } datbinds)
      in ignore (List.foldl (fn (S.TypBind (span, tyvarseq, tycon, ty), set) =>
                                ( doTy ty
                                ; checkTyVarSeq (span, tyvarseq)
                                ; if Syntax.TyConSet.member (set, tycon) then
                                      raise S.SyntaxError ([span], "duplicate type constructor in withtype declaration")
                                  else
                                      Syntax.TyConSet.add (set, tycon)
                                )
                            ) set withtypebinds)
      end
  | doDec (env, S.DatatypeRepDec (span, tycon, longtycon)) = ()
  | doDec (env, S.AbstypeDec (span, datbinds, withtypebinds, decs))
    = let val set = #ty (List.foldl (fn (S.DatBind (span, tyvarseq, tycon, conbinds), { v, ty }) =>
                                        ( checkTyVarSeq (span, tyvarseq)
                                        ; { v = List.foldl (fn (S.ConBind (span, vid, optTy), set) =>
                                                               ( Option.app doTy optTy
                                                               ; if Syntax.VIdSet.member (set, vid) then
                                                                     raise S.SyntaxError ([span], "duplicate value identifier in datatype declaration")
                                                                 else
                                                                     Syntax.VIdSet.add (set, vid)
                                                               )
                                                           ) v conbinds
                                          , ty = if Syntax.TyConSet.member (ty, tycon) then
                                                     raise S.SyntaxError ([span], "duplicate type constructor in datatype declaration")
                                                 else
                                                     Syntax.TyConSet.add (ty, tycon)
                                          }
                                        )
                                    ) { v = Syntax.VIdSet.empty, ty = Syntax.TyConSet.empty } datbinds)
          val _ = List.foldl (fn (S.TypBind (span, tyvarseq, tycon, ty), set) =>
                                 ( doTy ty
                                 ; checkTyVarSeq (span, tyvarseq)
                                 ; if Syntax.TyConSet.member (set, tycon) then
                                       raise S.SyntaxError ([span], "duplicate type constructor in withtype declaration")
                                   else
                                       Syntax.TyConSet.add (set, tycon)
                                 )
                             ) set withtypebinds
      in List.app (fn dec => doDec (env, dec)) decs
      end
  | doDec (env, S.ExceptionDec (span, exbinds)) = ignore (List.foldl (fn (S.ExBind (span, vid, optTy), set) =>
                                                                         ( Option.app doTy optTy
                                                                         ; if Syntax.VIdSet.member (set, vid) then
                                                                               raise S.SyntaxError ([span], "duplicate exception constructor")
                                                                           else
                                                                               Syntax.VIdSet.add (set, vid)
                                                                         )
                                                                     | (S.ExReplication (span, vid, longvid), set) =>
                                                                       if Syntax.VIdSet.member (set, vid) then
                                                                           raise S.SyntaxError ([span], "duplicate exception constructor")
                                                                       else
                                                                           Syntax.VIdSet.add (set, vid)
                                                                     ) Syntax.VIdSet.empty exbinds)
  | doDec (env, S.LocalDec (span, decs1, decs2)) = ( List.app (fn dec => doDec (env, dec)) decs1
                                                   ; List.app (fn dec => doDec (env, dec)) decs2
                                                   )
  | doDec (env, S.OpenDec (span, longstrids)) = ()
  | doDec (env, S.OverloadDec _) = ()
and doValBinds (env, valbinds) = List.app (fn (S.PatBind (_, pat, exp)) => ( doPat pat ; doExp (env, exp) )) valbinds (* duplicate identifiers are not checked here *)

(* doSpec : Syntax.Spec -> unit *)
fun doSpec (S.ValDesc (span, descs)) = ignore (List.foldl (fn ((vid, ty), set) =>
                                                              ( doTy ty
                                                              ; if Syntax.VIdSet.member (set, vid) then
                                                                    raise S.SyntaxError ([span], "duplicate identifier in value description")
                                                                else
                                                                    Syntax.VIdSet.add (set, vid)
                                                              )
                                                          ) Syntax.VIdSet.empty descs)
  | doSpec (S.TypeDesc (span, descs)) = ignore (List.foldl (fn ((tyvarseq, tycon), set) =>
                                                               ( checkTyVarSeq (span, tyvarseq)
                                                               ; if Syntax.TyConSet.member (set, tycon) then
                                                                     raise S.SyntaxError ([span], "duplicate type constructor in type description")
                                                                 else
                                                                     Syntax.TyConSet.add (set, tycon)
                                                               )
                                                           ) Syntax.TyConSet.empty descs)
  | doSpec (S.EqtypeDesc (span, descs)) = ignore (List.foldl (fn ((tyvarseq, tycon), set) =>
                                                                 ( checkTyVarSeq (span, tyvarseq)
                                                                 ; if Syntax.TyConSet.member (set, tycon) then
                                                                       raise S.SyntaxError ([span], "duplicate type constructor in type description")
                                                                   else
                                                                       Syntax.TyConSet.add (set, tycon)
                                                                 )
                                                             ) Syntax.TyConSet.empty descs)
  | doSpec (S.DatDesc (span, descs, withtypedescs))
    = let val set = #ty (List.foldl (fn ((tyvarseq, tycon, condescs), { v, ty }) =>
                                        ( checkTyVarSeq (span, tyvarseq)
                                        ; if Syntax.TyConSet.member (ty, tycon) then
                                              raise S.SyntaxError ([span], "duplicate type constructor in datatype description")
                                          else
                                              { v = List.foldl (fn (S.ConBind (span, vid, optTy), set) =>
                                                                   ( Option.app doTy optTy
                                                                   ; if Syntax.VIdSet.member (set, vid) then
                                                                         raise S.SyntaxError ([span], "duplicate value identifier in datatype description")
                                                                     else
                                                                         Syntax.VIdSet.add (set, vid)
                                                                   )
                                                               ) v condescs
                                              , ty = Syntax.TyConSet.add (ty, tycon)
                                              }
                                        )
                                    ) { v = Syntax.VIdSet.empty, ty = Syntax.TyConSet.empty } descs)
      in ignore (List.foldl (fn (S.TypBind (span, tyvarseq, tycon, ty), set) =>
                                ( doTy ty
                                ; checkTyVarSeq (span, tyvarseq)
                                ; if Syntax.TyConSet.member (set, tycon) then
                                      raise S.SyntaxError ([span], "duplicate type constructor in withtype description")
                                  else
                                      Syntax.TyConSet.add (set, tycon)
                                )
                            ) set withtypedescs)
      end
  | doSpec (S.DatatypeRepSpec (span, tycon, longtycon)) = ()
  | doSpec (S.ExDesc (span, descs)) = ignore (List.foldl (fn ((vid, optTy), set) =>
                                                             ( Option.app doTy optTy
                                                             ; if Syntax.VIdSet.member (set, vid) then
                                                                   raise S.SyntaxError ([span], "duplicate exception description")
                                                               else
                                                                   Syntax.VIdSet.add (set, vid)
                                                             )
                                                         ) Syntax.VIdSet.empty descs)
  | doSpec (S.StrDesc (span, strdescs)) = ignore (List.foldl (fn ((strid, sigexp), set) =>
                                                                 ( doSigExp sigexp
                                                                 ; if Syntax.StrIdSet.member (set, strid) then
                                                                       raise S.SyntaxError ([span], "duplicate structure identifier")
                                                                   else
                                                                       Syntax.StrIdSet.add (set, strid)
                                                                 )
                                                             ) Syntax.StrIdSet.empty strdescs)
  | doSpec (S.Include (span, sigexp)) = doSigExp sigexp
  | doSpec (S.Sharing (span, specs, longtycons)) = doSpecs (span, specs)
  | doSpec (S.SharingStructure (span, specs, longstrids)) = doSpecs (span, specs)
  | doSpec (S.TypeAliasDesc (span, descs)) = ignore (List.foldl (fn ((tyvarseq, tycon, ty), set) =>
                                                                    ( checkTyVarSeq (span, tyvarseq)
                                                                    ; doTy ty
                                                                    ; if Syntax.TyConSet.member (set, tycon) then
                                                                          raise S.SyntaxError ([span], "duplicate type constructor in type description")
                                                                      else
                                                                          Syntax.TyConSet.add (set, tycon)
                                                                    )
                                                                ) Syntax.TyConSet.empty descs)
and doSpecs (span, specs) = List.app doSpec specs
and doSigExp (S.BasicSigExp (span, specs)) = doSpecs (span, specs)
  | doSigExp (S.SigIdExp (_, _)) = ()
  | doSigExp (S.TypeRealisationExp (span, sigexp, tyvarseq, longtycon, ty)) = ( checkTyVarSeq (span, tyvarseq)
                                                                              ; doTy ty
                                                                              )

fun doStrExp (S.StructExp (span, strdecs)) = List.app doStrDec strdecs
  | doStrExp (S.StrIdExp (span, longstrid)) = ()
  | doStrExp (S.TransparentConstraintExp (span, strexp, sigexp)) = ( doSigExp sigexp ; doStrExp strexp )
  | doStrExp (S.OpaqueConstraintExp (span, strexp, sigexp)) = ( doSigExp sigexp ; doStrExp strexp )
  | doStrExp (S.FunctorAppExp (span, funid, strexp)) = doStrExp strexp
  | doStrExp (S.LetInStrExp (span, strdecs, strexp)) = ( List.app doStrDec strdecs ; doStrExp strexp )
and doStrDec (S.CoreDec (span, dec)) = doDec (S.TyVarSet.empty, dec)
  | doStrDec (S.StrBindDec (span, strbinds)) = ignore (List.foldl (fn ((strid, strexp), set) =>
                                                                      ( doStrExp strexp
                                                                      ; if Syntax.StrIdSet.member (set, strid) then
                                                                            raise S.SyntaxError ([span], "duplicate structure binding")
                                                                        else
                                                                            Syntax.StrIdSet.add (set, strid)
                                                                      )
                                                                  ) Syntax.StrIdSet.empty strbinds)
  | doStrDec (S.LocalStrDec (span, strdecs1, strdecs2)) = ( List.app doStrDec strdecs1 ; List.app doStrDec strdecs2 )

fun doFunExp (S.NamedFunExp (strid, sigexp, strexp)) = ( doSigExp sigexp ; doStrExp strexp )
  | doFunExp (S.AnonymousFunExp (sigexp, strexp)) = ( doSigExp sigexp ; doStrExp strexp )

fun doTopDec (S.StrDec strdec) = doStrDec strdec
  | doTopDec (S.SigDec sigbinds) = ignore (List.foldl (fn ((sigid, sigexp), set) =>
                                                          ( doSigExp sigexp
                                                          ; if Syntax.SigIdSet.member (set, sigid) then
                                                                raise S.SyntaxError ([], "duplicate signature binding") (* TODO: location info *)
                                                            else
                                                                Syntax.SigIdSet.add (set, sigid)
                                                          )
                                                      ) Syntax.SigIdSet.empty sigbinds)
  | doTopDec (S.FunDec funbinds) = ignore (List.foldl (fn ((span, funid, funexp), set) =>
                                                          ( doFunExp funexp
                                                          ; if Syntax.FunIdSet.member (set, funid) then
                                                                raise S.SyntaxError ([], "duplicate functor binding") (* TODO: location info *)
                                                            else
                                                                Syntax.FunIdSet.add (set, funid)
                                                          )
                                                      ) Syntax.FunIdSet.empty funbinds)
val checkProgram = List.app (List.app doTopDec)
end (* structure CheckSyntacticRestrictions *)
