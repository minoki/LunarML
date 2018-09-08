structure USyntax = struct
datatype UTyVar = UTyVar of Syntax.TyVar * int
datatype UTyCon = UTyCon of Syntax.TyCon * int
datatype ULongTyCon = ULongTyCon of Syntax.LongTyCon * int
structure SyntaxTree = Syntax.GenericSyntaxTree(type TyVar = UTyVar
                                                type TyCon = UTyCon
                                                type LongTyCon = ULongTyCon
                                               )
type TyVar = UTyVar
type TyCon = UTyCon
type LongTyCon = ULongTyCon
open SyntaxTree
end
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

exception NameError of string

local structure S = Syntax
      structure U = USyntax
      datatype BoundTyCon = BTyAlias of U.TyVar list * U.Ty
                          | BTyCon of int
      datatype ConEnv = MkConEnv of { tyConMap : BoundTyCon Syntax.TyConMap.map
                                    , strMap : ConEnv Syntax.StrIdMap.map
                                    }
      type Context = { nextTyVar : int ref
                     , nextTyCon : int ref
                     , tyVarMap : int Syntax.TyVarMap.map
                     , conEnv : ConEnv
                     }

      fun genTyVarId(ctx : Context)
          = let val id = !(#nextTyVar ctx)
            in #nextTyVar ctx := id + 1 ; id end
      fun genTyVar(ctx, tv) = USyntax.UTyVar(tv, genTyVarId(ctx))

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
fun toUPat(ctx, S.WildcardPat) = U.WildcardPat
  | toUPat(ctx, S.SConPat sc) = U.SConPat sc
  | toUPat(ctx, S.VIdPat longvid) = U.VIdPat longvid
  | toUPat(ctx, S.RecordPat(row, wildcard)) = U.RecordPat(toUPatRow(ctx, row), wildcard)
  | toUPat(ctx, S.ConPat(longvid, pat)) = U.ConPat(longvid, toUPat(ctx, pat))
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
  | toUExp(ctx, S.LetInExp(decls, exp)) = raise NameError("not impl")
  | toUExp(ctx, S.AppExp(exp1, exp2)) = U.AppExp(toUExp(ctx, exp1), toUExp(ctx, exp2))
  | toUExp(ctx, S.TypedExp(exp, ty)) = U.TypedExp(toUExp(ctx, exp), toUTy(ctx, ty))
  | toUExp(ctx, S.HandleExp(exp, ty)) = raise NameError("not implemented yet")
  | toUExp(ctx, S.RaiseExp(exp)) = U.RaiseExp(toUExp(ctx, exp))
  | toUExp(ctx, S.IfThenElseExp(exp1, exp2, exp3)) = U.IfThenElseExp(toUExp(ctx, exp1), toUExp(ctx, exp2), toUExp(ctx, exp3))
  | toUExp(ctx, S.CaseExp(exp, match)) = U.CaseExp(toUExp(ctx, exp), toUMatch(ctx, match))
  | toUExp(ctx, S.FnExp(match)) = U.FnExp(toUMatch(ctx, match))
and toUMatch(ctx, _ : (S.Pat * S.Exp) list) = raise NameError("not implemented yet")
and toUDec(ctx, _) = raise NameError("not implemented yet")
end
end

