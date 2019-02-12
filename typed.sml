structure USyntax = struct
datatype UTyVar = UTyVar of Syntax.TyVar * int
datatype UTyCon = UTyCon of Syntax.TyCon * int
datatype ULongTyCon = ULongTyCon of Syntax.LongTyCon * int
fun eqUTyVar(UTyVar(_,a),UTyVar(_,b)) = a = b
fun eqUTyCon(UTyCon(_,a),UTyCon(_,b)) = a = b
fun eqULongTyCon(ULongTyCon(_,a),ULongTyCon(_,b)) = a = b

type TyVar = UTyVar
type TyCon = UTyCon
type LongTyCon = ULongTyCon

datatype Ty = TyVar of TyVar (* type variable *)
            | RecordType of (Syntax.Label * Ty) list (* record type expression *)
            | TyCon of Ty list * LongTyCon (* type construction *)
            | FnType of Ty * Ty (* function type expression *)

fun PairType(a, b) = RecordType [(Syntax.NumericLabel 1, a), (Syntax.NumericLabel 2, b)]

datatype Pat = WildcardPat
             | SConPat of Syntax.SCon (* special constant *)
             | VarPat of Syntax.VId * Ty (* variable *)
             | NulConPat of Syntax.LongVId (* nullary constructor, like 'true', 'false', or 'nil' *)
             | RecordPat of (Syntax.Label * Pat) list * bool
             | ConPat of Syntax.LongVId * Pat (* constructed pattern *)
             | TypedPat of Pat * Ty (* typed *)
             | LayeredPat of Syntax.VId * Ty * Pat (* layered *)

datatype TypBind = TypBind of TyVar list * TyCon * Ty
datatype ConBind = ConBind of Syntax.VId * Ty option
datatype DatBind = DatBind of TyVar list * TyCon * ConBind list
datatype ExBind = ExBind1 of Syntax.VId * Ty option (* <op> vid <of ty> *)
                | ExBind2 of Syntax.VId * Syntax.LongVId (* <op> vid = <op> longvid *)

datatype Exp = SConExp of Syntax.SCon (* special constant *)
             | VarExp of Syntax.LongVId * Syntax.IdStatus (* value identifier *)
             | RecordExp of (Syntax.Label * Exp) list (* record *)
             | LetInExp of Dec list * Exp (* local declaration *)
             | AppExp of Exp * Exp (* function, argument *)
             | TypedExp of Exp * Ty
             | HandleExp of Exp * (Pat * Exp) list
             | RaiseExp of Exp
             | IfThenElseExp of Exp * Exp * Exp
             | CaseExp of Exp * (Pat * Exp) list
             | FnExp of (Pat * Exp) list
     and Dec = ValDec of TyVar list * ValBind list (* non-recursive *)
             | RecValDec of TyVar list * ValBind list (* recursive (val rec) *)
             | TypeDec of TypBind list
             | DatatypeDec of DatBind list
             | DatatypeRepDec of TyCon * LongTyCon
             | AbstypeDec of DatBind list * Dec list
             | ExceptionDec of ExBind list
             | LocalDec of Dec list * Dec list
             | OpenDec of Syntax.LongStrId list
     and ValBind = PatBind of Pat * Exp
type Program = Dec list

structure TyVarKey = struct
type ord_key = TyVar
fun compare (UTyVar(Syntax.MkTyVar x, a), UTyVar(Syntax.MkTyVar y, b)) = case String.compare (x,y) of
                                                                             EQUAL => Int.compare(a, b)
                                                                           | x => x
end : ORD_KEY
structure TyVarSet = BinarySetFn(TyVarKey)
structure TyVarMap = BinaryMapFn(TyVarKey)

(* pretty printing *)
structure PrettyPrint = struct
fun print_TyVar(UTyVar(tv, n)) = "UTyVar(" ^ Syntax.print_TyVar tv ^ "," ^ Int.toString n ^ ")"
fun print_TyCon(UTyCon(tycon, n)) = "UTyCon(" ^ Syntax.print_TyCon tycon ^ "," ^ Int.toString n ^ ")"
fun print_LongTyCon(ULongTyCon(longtycon, n)) = "ULongTyCon(" ^ Syntax.print_LongTyCon longtycon ^ "," ^ Int.toString n ^ ")"
fun print_Ty (TyVar x) = "TyVar(" ^ print_TyVar x ^ ")"
  | print_Ty (RecordType xs) = "RecordType " ^ Syntax.print_list (Syntax.print_pair (Syntax.print_Label,print_Ty)) xs
  | print_Ty (TyCon(x,y)) = "TyCon(" ^ Syntax.print_list print_Ty x ^ "," ^ print_LongTyCon y ^ ")"
  | print_Ty (FnType(x,y)) = "FnType(" ^ print_Ty x ^ "," ^ print_Ty y ^ ")"
fun print_Pat WildcardPat = "WildcardPat"
  | print_Pat (SConPat x) = "SConPat(" ^ Syntax.print_SCon x ^ ")"
  | print_Pat (VarPat(vid, ty)) = "VarPat(" ^ Syntax.print_VId vid ^ "," ^ print_Ty ty ^ ")"
  | print_Pat (NulConPat longvid) = "NulConPat(" ^ Syntax.print_LongVId longvid ^ ")"
  | print_Pat (TypedPat (pat, ty)) = "TypedPat(" ^ print_Pat pat ^ "," ^ print_Ty ty ^ ")"
  | print_Pat (LayeredPat (vid, ty, pat)) = "TypedPat(" ^ Syntax.print_VId vid ^ "," ^ print_Ty ty ^ "," ^ print_Pat pat ^ ")"
  | print_Pat (ConPat(longvid, pat)) = "ConPat(" ^ Syntax.print_LongVId longvid ^ "," ^ print_Pat pat ^ ")"
  | print_Pat (RecordPat(x, false)) = (case Syntax.extractTuple (1, x) of
                                           NONE => "RecordPat(" ^ Syntax.print_list (Syntax.print_pair (Syntax.print_Label, print_Pat)) x ^ ",false)"
                                         | SOME ys => "TuplePat " ^ Syntax.print_list print_Pat ys
                                      )
  | print_Pat (RecordPat(x, true)) = "RecordPat(" ^ Syntax.print_list (Syntax.print_pair (Syntax.print_Label, print_Pat)) x ^ ",true)"
(* | print_Pat _ = "<Pat>" *)
fun print_Exp (SConExp x) = "SConExp(" ^ Syntax.print_SCon x ^ ")"
  | print_Exp (VarExp(Syntax.MkLongVId([], vid), idstatus)) = "SimpleVarExp(" ^ Syntax.print_VId vid ^ "," ^ Syntax.print_IdStatus idstatus ^ ")"
  | print_Exp (VarExp(x, idstatus)) = "VarExp(" ^ Syntax.print_LongVId x ^ "," ^ Syntax.print_IdStatus idstatus ^ ")"
  | print_Exp (RecordExp x) = (case Syntax.extractTuple (1, x) of
                                   NONE => "RecordExp " ^ Syntax.print_list (Syntax.print_pair (Syntax.print_Label, print_Exp)) x
                                 | SOME ys => "TupleExp " ^ Syntax.print_list print_Exp ys
                              )
  | print_Exp (LetInExp(decls,x)) = "LetInExp(" ^ Syntax.print_list print_Dec decls ^ "," ^ print_Exp x ^ ")"
  | print_Exp (AppExp(x,y)) = "AppExp(" ^ print_Exp x ^ "," ^ print_Exp y ^ ")"
  | print_Exp (TypedExp(x,y)) = "TypedExp(" ^ print_Exp x ^ "," ^ print_Ty y ^ ")"
  | print_Exp (HandleExp(x,y)) = "HandleExp(" ^ print_Exp x ^ "," ^ Syntax.print_list (Syntax.print_pair (print_Pat, print_Exp)) y ^ ")"
  | print_Exp (RaiseExp x) = "RaiseExp(" ^ print_Exp x ^ ")"
  | print_Exp (IfThenElseExp(x,y,z)) = "IfThenElseExp(" ^ print_Exp x ^ "," ^ print_Exp y ^ "," ^ print_Exp z ^ ")"
  | print_Exp (CaseExp(x,y)) = "CaseExp(" ^ print_Exp x ^ "," ^ Syntax.print_list (Syntax.print_pair (print_Pat,print_Exp)) y ^ ")"
  | print_Exp (FnExp x) = "FnExp(" ^ Syntax.print_list (Syntax.print_pair (print_Pat,print_Exp)) x ^ ")"
and print_Dec (ValDec (bound,valbind)) = "ValDec(" ^ Syntax.print_list print_TyVar bound ^ "," ^ Syntax.print_list print_ValBind valbind  ^ ")"
  | print_Dec (RecValDec (bound,valbind)) = "RecValDec(" ^ Syntax.print_list print_TyVar bound ^ "," ^ Syntax.print_list print_ValBind valbind  ^ ")"
  | print_Dec _ = "<Dec>"
and print_ValBind (PatBind (pat, exp)) = "PatBind(" ^ print_Pat pat ^ "," ^ print_Exp exp ^ ")"
end
open PrettyPrint

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
        and doDec(ValDec(tyvars, valbind)) = ValDec(tyvars, List.map doValBind valbind)
          | doDec(RecValDec(tyvars, valbind)) = RecValDec(tyvars, List.map doValBind valbind)
          | doDec(TypeDec _) = raise NotImpl "doDec(TypeDec) not implemented yet"
          | doDec(DatatypeDec _) = raise NotImpl "doDec(DatatypeDec) not implemented yet"
          | doDec(DatatypeRepDec _) = raise NotImpl "doDec(DatatypeRepDec) not implemented yet"
          | doDec(AbstypeDec _) = raise NotImpl "doDec(AbstypeDec) not implemented yet"
          | doDec(ExceptionDec _) = raise NotImpl "doDec(ExceptionDec) not implemented yet"
          | doDec(LocalDec _) = raise NotImpl "doDec(LocalDec) not implemented yet"
          | doDec(OpenDec _) = raise NotImpl "doDec(OpenDec) not implemented yet"
        and doValBind(PatBind(pat, exp)) = PatBind(doPat pat, doExp exp)
        and doMatch(pat, exp) = (doPat pat, doExp exp)
        and doPat WildcardPat = WildcardPat
          | doPat(s as SConPat _) = s
          | doPat(VarPat(vid, ty)) = VarPat(vid, doTy ty)
          | doPat(s as NulConPat _) = s
          | doPat(RecordPat(xs, xt)) = RecordPat(List.map (fn (label, pat) => (label, doPat pat)) xs, xt)
          | doPat(ConPat(ct, pat)) = ConPat(ct, doPat pat)
          | doPat(TypedPat(pat, ty)) = TypedPat(doPat pat, doTy ty)
          | doPat(LayeredPat(vid, ty, pat)) = LayeredPat(vid, doTy ty, doPat pat)
    in doExp
    end

(* freeTyVarsInTy : TyVarSet * Ty -> TyVarSet *)
fun freeTyVarsInTy(bound, ty)
    = (case ty of
           TyVar tv => if TyVarSet.member(bound, tv) then
                           TyVarSet.empty
                       else
                           TyVarSet.singleton tv
         | RecordType xs => List.foldl (fn ((_, ty), set) => TyVarSet.union(freeTyVarsInTy(bound, ty), set)) TyVarSet.empty xs
         | TyCon(xs,_) => List.foldl (fn (ty, set) => TyVarSet.union(freeTyVarsInTy(bound, ty), set)) TyVarSet.empty xs
         | FnType(s,t) => TyVarSet.union(freeTyVarsInTy(bound, s), freeTyVarsInTy(bound, t))
      )

(* freeTyVarsInPat : TyVarSet * Pat -> TyVarSet *)
fun freeTyVarsInPat(bound, pat)
    = (case pat of
           WildcardPat => TyVarSet.empty
         | SConPat _ => TyVarSet.empty
         | VarPat _ => TyVarSet.empty
         | NulConPat _ => TyVarSet.empty
         | RecordPat(xs, _) => List.foldl (fn ((_, pat), set) => TyVarSet.union(freeTyVarsInPat(bound, pat), set)) TyVarSet.empty xs
         | ConPat(_, pat) => freeTyVarsInPat(bound, pat)
         | TypedPat(pat, ty) => TyVarSet.union(freeTyVarsInPat(bound, pat), freeTyVarsInTy(bound, ty))
         | LayeredPat(_, ty, pat) => TyVarSet.union(freeTyVarsInTy(bound, ty), freeTyVarsInPat(bound, pat))
      )

(* freeTyVarsInExp : TyVarSet * Exp -> TyVarSet *)
fun freeTyVarsInExp(bound, exp)
    = (case exp of
           SConExp _ => TyVarSet.empty
         | VarExp(_, _) => TyVarSet.empty
         | RecordExp(xs) => List.foldl (fn ((_, exp), set) => TyVarSet.union(freeTyVarsInExp(bound, exp), set)) TyVarSet.empty xs
         | LetInExp(decls, exp) => raise Fail "not impl"
         | AppExp(exp1, exp2) => TyVarSet.union(freeTyVarsInExp(bound, exp1), freeTyVarsInExp(bound, exp2))
         | TypedExp(exp, ty) => TyVarSet.union(freeTyVarsInExp(bound, exp), freeTyVarsInTy(bound, ty))
         | HandleExp(exp, matches) => TyVarSet.union(freeTyVarsInExp(bound, exp), freeTyVarsInMatches(bound, matches, TyVarSet.empty))
         | RaiseExp(exp) => freeTyVarsInExp(bound, exp)
         | IfThenElseExp(exp1, exp2, exp3) => TyVarSet.union(freeTyVarsInExp(bound, exp1), TyVarSet.union(freeTyVarsInExp(bound, exp2), freeTyVarsInExp(bound, exp3)))
         | CaseExp(exp, matches) => TyVarSet.union(freeTyVarsInExp(bound, exp), freeTyVarsInMatches(bound, matches, TyVarSet.empty))
         | FnExp(matches) => freeTyVarsInMatches(bound, matches, TyVarSet.empty)
      )
and freeTyVarsInMatches(bound, nil, acc) = acc
  | freeTyVarsInMatches(bound, (pat, exp) :: rest, acc) = freeTyVarsInMatches(bound, rest, TyVarSet.union(acc, TyVarSet.union(freeTyVarsInPat(bound, pat), freeTyVarsInExp(bound, exp))))
and freeTyVarsInDecs(bound, decls) = List.foldl (fn (dec, set) => TyVarSet.union(set, freeTyVarsInDec(bound, dec))) TyVarSet.empty decls
and freeTyVarsInDec(bound, dec)
    = (case dec of
           ValDec(tyvarseq, valbinds) => freeTyVarsInValBinds(TyVarSet.addList(bound, tyvarseq), valbinds, TyVarSet.empty)
         | RecValDec(tyvarseq, valbinds) => freeTyVarsInValBinds(TyVarSet.addList(bound, tyvarseq), valbinds, TyVarSet.empty)
         | TypeDec(_) => TyVarSet.empty (* ??? *)
         | DatatypeDec(_) => TyVarSet.empty
         | DatatypeRepDec(_, _) => TyVarSet.empty
         | AbstypeDec(_, _) => TyVarSet.empty
         | ExceptionDec(_) => TyVarSet.empty
         | LocalDec(dec1, dec2) => TyVarSet.union(freeTyVarsInDecs(bound, dec1), freeTyVarsInDecs(bound, dec2))
         | OpenDec(_) => TyVarSet.empty
      )
and freeTyVarsInValBinds(bound, nil, acc) = acc
  | freeTyVarsInValBinds(bound, PatBind(pat, exp) :: rest, acc) = freeTyVarsInValBinds(bound, rest, TyVarSet.union(acc, TyVarSet.union(freeTyVarsInPat(bound, pat), freeTyVarsInExp(bound, exp))))

end (* structure USyntax *)

structure ToTypedSyntax = struct
exception NameError of string

type ('a,'b,'c) Context = { nextTyVar : int ref
                                            (* , nextTyCon : int ref *)
                          , constraints : 'a
                          , tyVarConstraints : 'b
                          , tyVarSubst : 'c
                          }

(*
fun newContext() : Context
    = { nextTyVar = ref 100
      (* , nextTyCon = ref 100 *)
      }
*)

datatype BoundTyCon = BTyAlias of USyntax.TyVar list * USyntax.Ty
                    | BTyCon of int
datatype Env = MkEnv of { valMap : Syntax.IdStatus Syntax.VIdMap.map
                        , tyConMap : BoundTyCon Syntax.TyConMap.map
                        , strMap : Env Syntax.StrIdMap.map
                        }
val emptyEnv = MkEnv { valMap = Syntax.VIdMap.empty
                     , tyConMap = Syntax.TyConMap.empty
                     , strMap = Syntax.StrIdMap.empty
                     }
val initialEnv = MkEnv { valMap = let open Syntax
                                  in List.foldl Syntax.VIdMap.insert' Syntax.VIdMap.empty
                                                [(MkVId "ref", ValueConstructor)
                                                ,(MkVId "nil", ValueConstructor)
                                                ,(MkVId "true", ValueConstructor)
                                                ,(MkVId "false", ValueConstructor)
                                                ,(MkVId "Match", ExceptionConstructor)
                                                ,(MkVId "Bind", ExceptionConstructor)
                                                ,(MkVId "::", ValueConstructor)
                                                ]
                                  end
                       , tyConMap = Syntax.TyConMap.empty (* TODO *)
                       , strMap = Syntax.StrIdMap.empty
                       }

local structure S = Syntax
      structure U = USyntax

      fun genTyVarId(ctx : ('a,'b,'c) Context)
          = let val id = !(#nextTyVar ctx)
            in #nextTyVar ctx := id + 1 ; id end
      fun genTyVar(ctx, tv) = USyntax.UTyVar(tv, genTyVarId(ctx))
      fun freshTyVar(ctx : ('a,'b,'c) Context) = genTyVar(ctx, Syntax.MkTyVar "_")

(*
      fun genTyConId(ctx : Context)
          = let val id = !(#nextTyCon ctx)
            in #nextTyCon ctx := id + 1 ; id end
      fun genTyCon(ctx, tycon) = USyntax.UTyCon(tycon, genTyConId(ctx))
*)

      fun lookupStr(env, nil) = env
        | lookupStr(MkEnv { strMap = strMap, ... }, (str0 as S.MkStrId name) :: str1)
          = case Syntax.StrIdMap.find(strMap, str0) of
                NONE => raise NameError("unknown structure name " ^ name)
              | SOME innerEnv => lookupStr(innerEnv, str1)

      fun lookupTyCon(MkEnv env, tycon as Syntax.MkTyCon name)
          = case Syntax.TyConMap.find(#tyConMap env, tycon) of
                NONE => raise NameError("unknown type constructor " ^ name)
              | SOME b => b
      fun lookupLongTyCon(env : Env, Syntax.MkLongTyCon(strpath, tycon)) = lookupTyCon(lookupStr(env, strpath), tycon)

      fun lookupVId(MkEnv env, vid) = Syntax.VIdMap.find(#valMap env, vid)
      fun lookupLongVId(env : Env, Syntax.MkLongVId(strpath, vid)) = lookupVId(lookupStr(env, strpath), vid)
in
(* toUTy : Context * Env * Syntax.Ty -> USyntax.Ty *)
(* toUTyRow : Context * Env * (Label * Syntax.Ty) list -> (Label * USyntax.Ty) list *)
(* toUPat : Context * Env * Syntax.Pat -> USyntax.Pat *)
(* toUPatRow : Context * Env * (Label * USyntax.Pat) list -> (Label * USyntax.Pat) list *)
(* toUTypBind : Context * Syntax.TypBind -> USyntax.TypBind *)
(* toUConBind : Context * Syntax.ConBind -> USyntax.ConBind *)
(* toUDatBind : Context * Syntax.DatBind -> USyntax.DatBind *)
(* toUExBind : Context * Syntax.ExBind -> USyntax.ExBind *)
(* toUExp : Context * Env * Syntax.Exp -> USyntax.Exp *)
(* toUMatch : Context * Env * (Syntax.Pat * Syntax.Exp) list -> (USyntax.Pat * USyntax.Exp) list *)
(* toUDec : Context * Env * Syntax.Dec -> USyntax.Dec *)
fun toUTy(ctx : ('a,'b,'c) Context, env : Env, S.TyVar tv) = U.TyVar(genTyVar(ctx, tv))
  | toUTy(ctx, env, S.RecordType row) = U.RecordType(toUTyRow(ctx, env, row))
  | toUTy(ctx, env, S.TyCon(args, tycon)) = (case lookupLongTyCon(env, tycon) of
                                                 BTyCon id => U.TyCon(List.map (fn ty => toUTy(ctx, env, ty)) args, U.ULongTyCon(tycon, id))
                                               | BTyAlias _ => raise Fail "type alias not supported yet"
                                            )
  | toUTy(ctx, env, S.FnType(ty1, ty2)) = U.FnType(toUTy(ctx, env, ty1), toUTy(ctx, env, ty2))
and toUTyRow(ctx, env, row) = let fun oneField(label, ty) = (label, toUTy(ctx, env, ty))
                              in List.map oneField row
                              end
fun toUPat(ctx : ('a,'b,'c) Context, env : Env, S.WildcardPat) = U.WildcardPat (* TODO: should generate a type id? *)
  | toUPat(ctx, env, S.SConPat(Syntax.RealConstant _)) = raise Syntax.SyntaxError "No real constant may occur in a pattern"
  | toUPat(ctx, env, S.SConPat sc) = U.SConPat sc
  | toUPat(ctx, env, S.ConOrVarPat vid)
    = (case lookupVId(env, vid) of
           SOME Syntax.ValueConstructor => U.NulConPat(Syntax.MkLongVId([], vid))
         | SOME Syntax.ExceptionConstructor => U.NulConPat(Syntax.MkLongVId([], vid))
         | _ => U.VarPat(vid, USyntax.TyVar(freshTyVar(ctx)))
      )
  | toUPat(ctx, env, S.VarPat vid) = U.VarPat(vid, USyntax.TyVar(freshTyVar(ctx))) (* add extra type annotation *)
  | toUPat(ctx, env, S.NulConPat longvid) = U.NulConPat longvid
  | toUPat(ctx, env, S.RecordPat(row, wildcard)) = U.RecordPat(toUPatRow(ctx, env, row), wildcard)
  | toUPat(ctx, env, S.ConPat(longvid, pat)) = U.ConPat(longvid, toUPat(ctx, env, pat))
  | toUPat(ctx, env, S.TypedPat(S.ConOrVarPat vid, ty))
    = (case lookupVId(env, vid) of
           SOME Syntax.ValueConstructor => U.TypedPat(U.NulConPat(Syntax.MkLongVId([], vid)), toUTy(ctx, env, ty))
         | SOME Syntax.ExceptionConstructor => U.TypedPat(U.NulConPat(Syntax.MkLongVId([], vid)), toUTy(ctx, env, ty))
         | _ => U.VarPat(vid, toUTy(ctx, env, ty))
      )
  | toUPat(ctx, env, S.TypedPat(S.VarPat vid, ty)) = U.VarPat(vid, toUTy(ctx, env, ty))
  | toUPat(ctx, env, S.TypedPat(pat, ty)) = U.TypedPat(toUPat(ctx, env, pat), toUTy(ctx, env, ty))
  | toUPat(ctx, env, S.LayeredPat(vid, SOME ty, pat)) = U.LayeredPat(vid, toUTy(ctx, env, ty), toUPat(ctx, env, pat))
  | toUPat(ctx, env, S.LayeredPat(vid, NONE, pat)) = U.LayeredPat(vid, USyntax.TyVar(freshTyVar(ctx)), toUPat(ctx, env, pat))
and toUPatRow(ctx, env, row : (Syntax.Label * Syntax.Pat) list) = List.map (fn (label, pat) => (label, toUPat(ctx, env, pat))) row
fun toUTypBind(ctx, S.TypBind(params, tycon, ty)) = (* let val params' = List.map (fn ty => genTyVar(ctx, ty)) params
                                                        val tycon' = 
                                                  in U.TypBind(params', tycon *) raise Fail "toUTypBind: not implemented yet"
fun toUConBind(ctx, S.ConBind(vid, opt_ty)) = raise Fail "toUConBind: not implemented yet"
fun toUDatBind(ctx, S.DatBind(params, tycon, conbinds)) = raise Fail "toUDatBind: not implemented yet" (* genTyCon *)
fun toUExBind(ctx, _ : S.ExBind) = raise Fail "not implemented yet"
fun toUExp(ctx : ('a,'b,'c) Context, env : Env, S.SConExp(scon)) = U.SConExp(scon)
  | toUExp(ctx, env, S.VarExp(longvid))
    = (case lookupLongVId(env, longvid) of
           SOME idstatus => U.VarExp(longvid, idstatus)
         | NONE => U.VarExp(longvid, Syntax.ValueVariable)
      )
  | toUExp(ctx, env, S.RecordExp(row)) = U.RecordExp(List.map (fn (label, exp) => (label, toUExp(ctx, env, exp))) row)
  | toUExp(ctx, env, S.LetInExp(decls, exp))
    = let fun doDecl(env, nil, acc) = (env, List.rev acc)
            | doDecl(env, decl :: decls, acc)
              = (case decl of
                     S.ValDec(_) => doDecl(env, decls, toUDec(ctx, env, decl) :: acc)
                   | S.RecValDec(_) => doDecl(env, decls, toUDec(ctx, env, decl) :: acc)
                   | S.TypeDec(typbinds) => doDecl((* TODO: add type ctor *) env, decls, toUDec(ctx, env, decl) :: acc)
                   | S.DatatypeDec(datbinds) => doDecl((* TODO: add type ctor, data ctor *) env, decls, toUDec(ctx, env, decl) :: acc)
                   | S.DatatypeRepDec(tycon, longtycon) => doDecl((* TODO: add type ctor *) env, decls, toUDec(ctx, env, decl) :: acc)
                   | S.AbstypeDec(datbinds, dec) => doDecl((* TODO: add type ctor *) env, decls, toUDec(ctx, env, decl) :: acc)
                   | S.ExceptionDec(_) => doDecl((* TODO: add exception ctor *) env, decls, toUDec(ctx, env, decl) :: acc)
                   | S.LocalDec(_) => doDecl((* TODO: add type ctor *) env, decls, toUDec(ctx, env, decl) :: acc)
                   | S.OpenDec(_) => doDecl((* TODO: add type ctor *) env, decls, toUDec(ctx, env, decl) :: acc)
                   | S.FixityDec(_) => doDecl(env, decls, acc) (* ignore *)
                )
          val (env', decls') = doDecl(env, decls, nil)
      in U.LetInExp(decls', toUExp(ctx, env', exp))
      end
  | toUExp(ctx, env, S.AppExp(exp1, exp2)) = U.AppExp(toUExp(ctx, env, exp1), toUExp(ctx, env, exp2))
  | toUExp(ctx, env, S.TypedExp(exp, ty)) = U.TypedExp(toUExp(ctx, env, exp), toUTy(ctx, env, ty))
  | toUExp(ctx, env, S.HandleExp(exp, ty)) = raise NameError("not implemented yet")
  | toUExp(ctx, env, S.RaiseExp(exp)) = U.RaiseExp(toUExp(ctx, env, exp))
  | toUExp(ctx, env, S.IfThenElseExp(exp1, exp2, exp3)) = U.IfThenElseExp(toUExp(ctx, env, exp1), toUExp(ctx, env, exp2), toUExp(ctx, env, exp3))
  | toUExp(ctx, env, S.CaseExp(exp, match)) = U.CaseExp(toUExp(ctx, env, exp), toUMatch(ctx, env, match))
  | toUExp(ctx, env, S.FnExp(match)) = U.FnExp(toUMatch(ctx, env, match))
and toUMatch(ctx, env, matches : (S.Pat * S.Exp) list) = List.map (fn (pat, exp) => (toUPat(ctx, env, pat), toUExp(ctx, env, exp))) matches
and toUDec(ctx, env, S.ValDec(tyvars, valbind)) = U.ValDec(List.map (fn tv => genTyVar(ctx, tv)) tyvars, List.map (fn vb => toUValBind(ctx, env, vb)) valbind)
  | toUDec(ctx, env, S.RecValDec(tyvars, valbind)) = U.RecValDec(List.map (fn tv => genTyVar(ctx, tv)) tyvars, List.map (fn vb => toUValBind(ctx, env, vb)) valbind)
  | toUDec(ctx, env, S.TypeDec(typbinds)) = U.TypeDec(List.map (fn typbind => toUTypBind(ctx, env, typbind)) typbinds)
  | toUDec(ctx, env, S.DatatypeDec _) = raise Fail "not implemented yet"
  | toUDec(ctx, env, _) = raise Fail "not implemented yet"
and toUValBind(ctx, env, S.PatBind(pat, exp)) = U.PatBind(toUPat(ctx, env, pat), toUExp(ctx, env, exp))
and toUTypBind(ctx, env, _) = raise Fail "not implemented yet"
end (* local *)
end (* structure ToTypedSyntax *)
