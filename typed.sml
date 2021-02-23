structure USyntax = struct
datatype VId = MkVId of string * int
type LongVId = VId Syntax.Qualified
fun MkLongVId(strids, vid: VId) = Syntax.MkQualified(strids, vid)
datatype TyVar = NamedTyVar of string * bool * int
               | AnonymousTyVar of int
datatype TyCon = MkTyCon of string * int
type LongTyCon = TyCon Syntax.Qualified
fun MkLongTyCon(Syntax.MkQualified(strids, Syntax.MkTyCon(tycon)), n) = Syntax.MkQualified(strids, MkTyCon(tycon, n))
fun eqUTyVar(NamedTyVar(name,eq,a),NamedTyVar(name',eq',b)) = name = name' andalso eq = eq' andalso a = b
  | eqUTyVar(AnonymousTyVar a, AnonymousTyVar b) = a = b
  | eqUTyVar(_, _) = false
fun eqUTyCon(MkTyCon(_,a),MkTyCon(_,b)) = a = b
fun eqULongTyCon(Syntax.MkQualified(_,a),Syntax.MkQualified(_,b)) = eqUTyCon(a, b)
fun eqVId(a, b : VId) = a = b
fun eqULongVId(Syntax.MkQualified(_,a),Syntax.MkQualified(_,b)) = a = b

datatype Ty = TyVar of TyVar (* type variable *)
            | RecordType of (Syntax.Label * Ty) list (* record type expression *)
            | TyCon of Ty list * LongTyCon (* type construction *)
            | FnType of Ty * Ty (* function type expression *)

fun PairType(a, b) = RecordType [(Syntax.NumericLabel 1, a), (Syntax.NumericLabel 2, b)]
fun TupleType xs = let fun doFields i nil = nil
                         | doFields i (x :: xs) = (Syntax.NumericLabel i, x) :: doFields (i + 1) xs
                   in RecordType (doFields 1 xs)
                   end

structure VIdKey = struct
type ord_key = VId
fun compare(MkVId(x,a), MkVId(y,b)) = case String.compare (x,y) of
                                          EQUAL => Int.compare(a,b)
                                        | ord => ord
end : ORD_KEY
structure VIdSet = BinarySetFn(VIdKey)
structure VIdMap = BinaryMapFn(VIdKey)

structure TyConKey = struct
type ord_key = TyCon
fun compare(MkTyCon(x,a), MkTyCon(y,b)) = case String.compare (x,y) of
                                              EQUAL => Int.compare(a,b)
                                            | ord => ord
end : ORD_KEY
structure TyConSet = BinarySetFn(TyConKey)
structure TyConMap = BinaryMapFn(TyConKey)

structure TyVarKey = struct
type ord_key = TyVar
fun compare(NamedTyVar(x,_,a), NamedTyVar(y,_,b)) = (case String.compare (x,y) of
                                                         EQUAL => Int.compare(a,b)
                                                       | ord => ord
                                                    )
  | compare(AnonymousTyVar(a), AnonymousTyVar(b)) = Int.compare(a,b)
  | compare(NamedTyVar _, AnonymousTyVar _) = LESS
  | compare(AnonymousTyVar _, NamedTyVar _) = GREATER
end : ORD_KEY
structure TyVarMap = BinaryMapFn(TyVarKey)

datatype UnaryConstraint
  = HasField of { label : Syntax.Label
                , fieldTy : Ty
                }
  | IsEqType
  | IsIntegral (* Int, Word; div, mod; defaults to int *)
  | IsSignedReal (* Int, Real; abs; defaults to int *)
  | IsRing (* Int, Word, Real; *, +, -; defaults to int *)
  | IsField (* Real; /; defaults to real *)
  | IsSigned (* Int, Real; ~; defaults to int *)
  | IsOrdered (* NumTxt; <, >, <=, >=; defaults to int *)

datatype Constraint
  = EqConstr of Ty * Ty (* ty1 = ty2 *)
  | UnaryConstraint of Ty * UnaryConstraint

datatype TypeFcn = TypeFcn of TyVar list * Ty
datatype TypeScheme = TypeScheme of (TyVar * UnaryConstraint list) list * Ty
type ValEnv = (TypeScheme * Syntax.IdStatus) VIdMap.map
val emptyValEnv = VIdMap.empty

datatype Pat = WildcardPat
             | SConPat of Syntax.SCon (* special constant *)
             | VarPat of VId * Ty (* variable *)
             | RecordPat of (Syntax.Label * Pat) list * bool
             | ConPat of LongVId * Pat option (* constructed pattern *)
             | InstantiatedConPat of LongVId * Pat option * Ty list
             | TypedPat of Pat * Ty (* typed *)
             | LayeredPat of VId * Ty * Pat (* layered *)

datatype TypBind = TypBind of TyVar list * TyCon * Ty
datatype ConBind = ConBind of VId * Ty option
datatype DatBind = DatBind of TyVar list * TyCon * ConBind list
datatype ExBind = ExBind1 of VId * Ty option (* <op> vid <of ty> *)
                | ExBind2 of VId * LongVId (* <op> vid = <op> longvid *)

datatype Exp = SConExp of Syntax.SCon (* special constant *)
             | VarExp of LongVId * Syntax.IdStatus (* value identifier; IdStatus is used by isNonexpansive *)
             | InstantiatedVarExp of LongVId * Syntax.IdStatus * Ty list (* identifiers with type arguments; produced during type-checking *)
             | RecordExp of (Syntax.Label * Exp) list (* record *)
             | LetInExp of Dec list * Exp (* local declaration *)
             | AppExp of Exp * Exp (* function, argument *)
             | TypedExp of Exp * Ty
             | HandleExp of Exp * (Pat * Exp) list
             | RaiseExp of Exp
             | IfThenElseExp of Exp * Exp * Exp
             | CaseExp of Exp * Ty * (Pat * Exp) list
             | FnExp of VId * Ty * Exp (* parameter name, parameter type, body *)
             | ProjectionExp of { label : Syntax.Label, recordTy : Ty, fieldTy : Ty }
     and Dec = ValDec of TyVar list * ValBind list * ValEnv (* non-recursive *)
             | RecValDec of TyVar list * ValBind list * ValEnv (* recursive (val rec) *)
     and TopDec = TypeDec of TypBind list
                | DatatypeDec of DatBind list
                | DatatypeRepDec of TyCon * LongTyCon
                | AbstypeDec of DatBind list * Dec list
                | ExceptionDec of ExBind list
     and ValBind = PatBind of Pat * Exp
                 | TupleBind of (VId * Ty) list * Exp (* monomorphic binding; produced during type-check *)
                 | PolyVarBind of VId * TypeScheme * Exp (* polymorphic binding; produced during type-check *)
type Program = TopDec list * Dec list

fun TupleExp xs = let fun doFields i nil = nil
                        | doFields i (x :: xs) = (Syntax.NumericLabel i, x) :: doFields (i + 1) xs
                  in RecordExp (doFields 1 xs)
                  end

structure TyVarKey = struct
type ord_key = TyVar
fun compare (NamedTyVar(name, eq, a), NamedTyVar(name', eq', b)) =
    (case String.compare (name, name') of
         EQUAL => Int.compare(a, b)
       | ord => ord
    )
  | compare (AnonymousTyVar(a), AnonymousTyVar(b)) = Int.compare(a, b)
  | compare (NamedTyVar _, AnonymousTyVar _) = LESS
  | compare (AnonymousTyVar _, NamedTyVar _) = GREATER
end : ORD_KEY
structure TyVarSet = BinarySetFn(TyVarKey)
structure TyVarMap = BinaryMapFn(TyVarKey)

(* pretty printing *)
structure PrettyPrint = struct
fun print_VId(MkVId(name, n)) = "MkVId(\"" ^ String.toString name ^ "\"," ^ Int.toString n ^ ")"
fun print_LongVId(Syntax.MkQualified(strids, vid)) = "MkLongVId(" ^ Syntax.print_list Syntax.print_StrId strids ^ "," ^ print_VId vid ^ ")"
fun print_TyVar(NamedTyVar(tvname, eq, n)) = "NamedTyVar(\"" ^ String.toString tvname ^ "\"," ^ Bool.toString eq ^ "," ^ Int.toString n ^ ")"
  | print_TyVar(AnonymousTyVar(n)) = "AnonymousTyVar(" ^ Int.toString n ^ ")"
fun print_TyCon(MkTyCon(tyconname, n)) = "MkTyCon(\"" ^ String.toString tyconname ^ "\"," ^ Int.toString n ^ ")"
fun print_LongTyCon(Syntax.MkQualified([], tycon)) = (case tycon of
                                                          MkTyCon ("int", 0) => "primTyCon_int"
                                                        | MkTyCon ("word", 1) => "primTyCon_word"
                                                        | MkTyCon ("real", 2) => "primTyCon_real"
                                                        | MkTyCon ("string", 3) => "primTyCon_string"
                                                        | MkTyCon ("char", 4) => "primTyCon_char"
                                                        | MkTyCon ("exn", 5) => "primTyCon_exn"
                                                        | MkTyCon ("bool", 6) => "primTyCon_bool"
                                                        | MkTyCon ("ref", 7) => "primTyCon_ref"
                                                        | MkTyCon ("list", 8) => "primTyCon_list"
                                                        | _ => "MkQualified([]," ^ print_TyCon tycon ^ ")"
                                                     )
  | print_LongTyCon(Syntax.MkQualified(strids, tycon)) = "MkQualified(" ^ Syntax.print_list Syntax.print_StrId strids ^ "," ^ print_TyCon tycon ^ ")"
fun print_Ty (TyVar x) = "TyVar(" ^ print_TyVar x ^ ")"
  | print_Ty (RecordType xs) = (case Syntax.extractTuple (1, xs) of
                                    NONE => "RecordType " ^ Syntax.print_list (Syntax.print_pair (Syntax.print_Label,print_Ty)) xs
                                  | SOME ys => "TupleType " ^ Syntax.print_list print_Ty ys
                               )
  | print_Ty (TyCon([],Syntax.MkQualified([],MkTyCon("int", 0)))) = "primTy_int"
  | print_Ty (TyCon([],Syntax.MkQualified([],MkTyCon("word", 1)))) = "primTy_word"
  | print_Ty (TyCon([],Syntax.MkQualified([],MkTyCon("real", 2)))) = "primTy_real"
  | print_Ty (TyCon([],Syntax.MkQualified([],MkTyCon("string", 3)))) = "primTy_string"
  | print_Ty (TyCon([],Syntax.MkQualified([],MkTyCon("char", 4)))) = "primTy_char"
  | print_Ty (TyCon([],Syntax.MkQualified([],MkTyCon("exn", 5)))) = "primTy_exn"
  | print_Ty (TyCon([],Syntax.MkQualified([],MkTyCon("bool", 6)))) = "primTy_bool"
  | print_Ty (TyCon(x,y)) = "TyCon(" ^ Syntax.print_list print_Ty x ^ "," ^ print_LongTyCon y ^ ")"
  | print_Ty (FnType(x,y)) = "FnType(" ^ print_Ty x ^ "," ^ print_Ty y ^ ")"
fun print_Pat WildcardPat = "WildcardPat"
  | print_Pat (SConPat x) = "SConPat(" ^ Syntax.print_SCon x ^ ")"
  | print_Pat (VarPat(vid, ty)) = "VarPat(" ^ print_VId vid ^ "," ^ print_Ty ty ^ ")"
  | print_Pat (TypedPat (pat, ty)) = "TypedPat(" ^ print_Pat pat ^ "," ^ print_Ty ty ^ ")"
  | print_Pat (LayeredPat (vid, ty, pat)) = "TypedPat(" ^ print_VId vid ^ "," ^ print_Ty ty ^ "," ^ print_Pat pat ^ ")"
  | print_Pat (ConPat(longvid, pat)) = "ConPat(" ^ print_LongVId longvid ^ "," ^ Syntax.print_option print_Pat pat ^ ")"
  | print_Pat (InstantiatedConPat(longvid, pat, tyargs)) = "InstantiatedConPat(" ^ print_LongVId longvid ^ "," ^ Syntax.print_option print_Pat pat ^ "," ^ Syntax.print_list print_Ty tyargs ^ ")"
  | print_Pat (RecordPat(x, false)) = (case Syntax.extractTuple (1, x) of
                                           NONE => "RecordPat(" ^ Syntax.print_list (Syntax.print_pair (Syntax.print_Label, print_Pat)) x ^ ",false)"
                                         | SOME ys => "TuplePat " ^ Syntax.print_list print_Pat ys
                                      )
  | print_Pat (RecordPat(x, true)) = "RecordPat(" ^ Syntax.print_list (Syntax.print_pair (Syntax.print_Label, print_Pat)) x ^ ",true)"
(* | print_Pat _ = "<Pat>" *)
fun print_Exp (SConExp x) = "SConExp(" ^ Syntax.print_SCon x ^ ")"
  | print_Exp (VarExp(Syntax.MkQualified([], vid), idstatus)) = "SimpleVarExp(" ^ print_VId vid ^ "," ^ Syntax.print_IdStatus idstatus ^ ")"
  | print_Exp (VarExp(x, idstatus)) = "VarExp(" ^ print_LongVId x ^ "," ^ Syntax.print_IdStatus idstatus ^ ")"
  | print_Exp (InstantiatedVarExp(x, idstatus, tyargs)) = "InstantiatedVarExp(" ^ print_LongVId x ^ "," ^ Syntax.print_IdStatus idstatus ^ "," ^ Syntax.print_list print_Ty tyargs ^ ")"
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
  | print_Exp (CaseExp(x,ty,y)) = "CaseExp(" ^ print_Exp x ^ "," ^ print_Ty ty ^ "," ^ Syntax.print_list (Syntax.print_pair (print_Pat,print_Exp)) y ^ ")"
  | print_Exp (FnExp(pname,pty,body)) = "FnExp(" ^ print_VId pname ^ "," ^ print_Ty pty ^ "," ^ print_Exp body ^ ")"
  | print_Exp (ProjectionExp { label = label, recordTy = recordTy, fieldTy = fieldTy }) = "ProjectionExp{label=" ^ Syntax.print_Label label ^ ",recordTy=" ^ print_Ty recordTy ^ ",fieldTy=" ^ print_Ty fieldTy ^ "}"
and print_Dec (ValDec (bound,valbind,valenv)) = "ValDec(" ^ Syntax.print_list print_TyVar bound ^ "," ^ Syntax.print_list print_ValBind valbind ^ "," ^ print_ValEnv valenv ^ ")"
  | print_Dec (RecValDec (bound,valbind,valenv)) = "RecValDec(" ^ Syntax.print_list print_TyVar bound ^ "," ^ Syntax.print_list print_ValBind valbind ^ "," ^ print_ValEnv valenv ^ ")"
  (* | print_Dec _ = "<Dec>"*)
and print_ValBind (PatBind (pat, exp)) = "PatBind(" ^ print_Pat pat ^ "," ^ print_Exp exp ^ ")"
  | print_ValBind (TupleBind (xs, exp)) = "TupleBind(" ^ Syntax.print_list (Syntax.print_pair (print_VId, print_Ty)) xs ^ "," ^ print_Exp exp ^ ")"
  | print_ValBind (PolyVarBind (name, tysc, exp)) = "PolyVarBind(" ^ print_VId name ^ "," ^ print_TypeScheme tysc ^ "," ^ print_Exp exp ^ ")"
and print_TyVarMap print_elem x = Syntax.print_list (Syntax.print_pair (print_TyVar,print_elem)) (TyVarMap.foldri (fn (k,x,ys) => (k,x) :: ys) [] x)
and print_VIdMap print_elem x = Syntax.print_list (Syntax.print_pair (print_VId,print_elem)) (VIdMap.foldri (fn (k,x,ys) => (k,x) :: ys) [] x)
and print_UnaryConstraint (HasField { label = label, fieldTy = fieldTy }) = "HasField{label=" ^ Syntax.print_Label label ^ ",fieldTy=" ^ print_Ty fieldTy ^ "}"
  | print_UnaryConstraint IsEqType = "IsEqType"
  | print_UnaryConstraint IsIntegral = "IsIntegral"
  | print_UnaryConstraint IsSignedReal = "IsSignedReal"
  | print_UnaryConstraint IsRing = "IsRing"
  | print_UnaryConstraint IsField = "IsField"
  | print_UnaryConstraint IsSigned = "IsSigned"
  | print_UnaryConstraint IsOrdered = "IsOrdered"
and print_TypeScheme (TypeScheme(tyvars, ty)) = "TypeScheme(" ^ Syntax.print_list (Syntax.print_pair (print_TyVar, Syntax.print_list print_UnaryConstraint)) tyvars ^ "," ^ print_Ty ty ^ ")"
and print_ValEnv env = print_VIdMap (Syntax.print_pair (print_TypeScheme,Syntax.print_IdStatus)) env
fun print_TyVarSet x = Syntax.print_list print_TyVar (TyVarSet.foldr (fn (x,ys) => x :: ys) [] x)
val print_Decs = Syntax.print_list print_Dec
end (* structure PrettyPrint *)
open PrettyPrint

exception NotImpl of string

(* mapTy : (Ty -> Ty) -> { doExp : Exp -> Exp, doDec : Dec -> Dec } *)
fun mapTy doTy =
    (* assumes that doTy only acts on type variables *)
    let fun doExp(e as SConExp _) = e
          | doExp(e as VarExp _) = e
          | doExp(InstantiatedVarExp(longvid, idstatus, tyargs)) = InstantiatedVarExp(longvid, idstatus, List.map doTy tyargs)
          | doExp(RecordExp fields) = RecordExp(Syntax.mapRecordRow doExp fields)
          | doExp(LetInExp(decls, e)) = LetInExp(List.map doDec decls, doExp e)
          | doExp(AppExp(e1, e2)) = AppExp(doExp e1, doExp e2)
          | doExp(TypedExp(e, ty)) = TypedExp(doExp e, doTy ty)
          | doExp(HandleExp(e, matches)) = HandleExp(doExp e, List.map doMatch matches)
          | doExp(RaiseExp e) = RaiseExp(doExp e)
          | doExp(IfThenElseExp(e1, e2, e3)) = IfThenElseExp(doExp e1, doExp e2, doExp e3)
          | doExp(CaseExp(e, ty, matches)) = CaseExp(doExp e, doTy ty, List.map doMatch matches)
          | doExp(FnExp(vid,ty,body)) = FnExp(vid,doTy ty,doExp body)
          | doExp(ProjectionExp { label = label, recordTy = recordTy, fieldTy = fieldTy }) = ProjectionExp { label = label, recordTy = doTy recordTy, fieldTy = doTy fieldTy }
        and doDec(ValDec(tyvars, valbind, valenv)) = ValDec(tyvars, List.map doValBind valbind, valenv)
          | doDec(RecValDec(tyvars, valbind, valenv)) = RecValDec(tyvars, List.map doValBind valbind, valenv)
        and doValBind(PatBind(pat, exp)) = PatBind(doPat pat, doExp exp)
          | doValBind(TupleBind(xs, exp)) = TupleBind(xs, doExp exp) (* TODO *)
          | doValBind(PolyVarBind(vid, tysc, exp)) = PolyVarBind(vid, tysc, doExp exp) (* TODO *)
        and doMatch(pat, exp) = (doPat pat, doExp exp)
        and doPat WildcardPat = WildcardPat
          | doPat(s as SConPat _) = s
          | doPat(VarPat(vid, ty)) = VarPat(vid, doTy ty)
          | doPat(RecordPat(xs, xt)) = RecordPat(Syntax.mapRecordRow doPat xs, xt)
          | doPat(ConPat(ct, pat)) = ConPat(ct, Option.map doPat pat)
          | doPat(InstantiatedConPat(ct, pat, tyargs)) = InstantiatedConPat(ct, Option.map doPat pat, List.map doTy tyargs)
          | doPat(TypedPat(pat, ty)) = TypedPat(doPat pat, doTy ty)
          | doPat(LayeredPat(vid, ty, pat)) = LayeredPat(vid, doTy ty, doPat pat)
    in { doExp = doExp, doDec = doDec }
    end
(* mapTyInExp : (Ty -> Ty) -> Exp -> Exp *)
(* mapTyInDec : (Ty -> Ty) -> Dec -> Dec *)
fun mapTyInExp doTy = #doExp (mapTy doTy)
fun mapTyInDec doTy = #doDec (mapTy doTy)

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
         | VarPat(_, ty) => freeTyVarsInTy(bound, ty)
         | RecordPat(xs, _) => List.foldl (fn ((_, pat), set) => TyVarSet.union(freeTyVarsInPat(bound, pat), set)) TyVarSet.empty xs
         | ConPat(_, NONE) => TyVarSet.empty
         | ConPat(_, SOME pat) => freeTyVarsInPat(bound, pat)
         | InstantiatedConPat(_, NONE, tyargs) => List.foldl (fn (ty, set) => TyVarSet.union(freeTyVarsInTy(bound, ty), set)) TyVarSet.empty tyargs
         | InstantiatedConPat(_, SOME pat, tyargs) => List.foldl (fn (ty, set) => TyVarSet.union(freeTyVarsInTy(bound, ty), set)) (freeTyVarsInPat(bound, pat)) tyargs
         | TypedPat(pat, ty) => TyVarSet.union(freeTyVarsInPat(bound, pat), freeTyVarsInTy(bound, ty))
         | LayeredPat(_, ty, pat) => TyVarSet.union(freeTyVarsInTy(bound, ty), freeTyVarsInPat(bound, pat))
      )

(* freeTyVarsInExp : TyVarSet * Exp -> TyVarSet *)
fun freeTyVarsInExp(bound, exp)
    = (case exp of
           SConExp _ => TyVarSet.empty
         | VarExp(_, _) => TyVarSet.empty
         | InstantiatedVarExp(_, _, tyargs) => List.foldl (fn (ty, set) => TyVarSet.union(freeTyVarsInTy(bound, ty), set)) TyVarSet.empty tyargs
         | RecordExp(xs) => List.foldl (fn ((_, exp), set) => TyVarSet.union(freeTyVarsInExp(bound, exp), set)) TyVarSet.empty xs
         | LetInExp(decls, exp) => TyVarSet.union(freeTyVarsInDecs(bound, decls), freeTyVarsInExp(bound, exp))
         | AppExp(exp1, exp2) => TyVarSet.union(freeTyVarsInExp(bound, exp1), freeTyVarsInExp(bound, exp2))
         | TypedExp(exp, ty) => TyVarSet.union(freeTyVarsInExp(bound, exp), freeTyVarsInTy(bound, ty))
         | HandleExp(exp, matches) => TyVarSet.union(freeTyVarsInExp(bound, exp), freeTyVarsInMatches(bound, matches, TyVarSet.empty))
         | RaiseExp(exp) => freeTyVarsInExp(bound, exp)
         | IfThenElseExp(exp1, exp2, exp3) => TyVarSet.union(freeTyVarsInExp(bound, exp1), TyVarSet.union(freeTyVarsInExp(bound, exp2), freeTyVarsInExp(bound, exp3)))
         | CaseExp(exp, ty, matches) => TyVarSet.union(freeTyVarsInExp(bound, exp), TyVarSet.union(freeTyVarsInTy(bound, ty), freeTyVarsInMatches(bound, matches, TyVarSet.empty)))
         | FnExp(vid, ty, body) => TyVarSet.union(freeTyVarsInTy(bound, ty), freeTyVarsInExp(bound, body))
         | ProjectionExp { label = _, recordTy = recordTy, fieldTy = fieldTy } => TyVarSet.union(freeTyVarsInTy(bound, recordTy), freeTyVarsInTy(bound, fieldTy))
      )
and freeTyVarsInMatches(bound, nil, acc) = acc
  | freeTyVarsInMatches(bound, (pat, exp) :: rest, acc) = freeTyVarsInMatches(bound, rest, TyVarSet.union(acc, TyVarSet.union(freeTyVarsInPat(bound, pat), freeTyVarsInExp(bound, exp))))
and freeTyVarsInDecs(bound, decls) = List.foldl (fn (dec, set) => TyVarSet.union(set, freeTyVarsInDec(bound, dec))) TyVarSet.empty decls
and freeTyVarsInDec(bound, dec)
    = (case dec of
           ValDec(tyvarseq, valbinds, valenv) => freeTyVarsInValBinds(TyVarSet.addList(bound, tyvarseq), valbinds, TyVarSet.empty)
         | RecValDec(tyvarseq, valbinds, valenv) => freeTyVarsInValBinds(TyVarSet.addList(bound, tyvarseq), valbinds, TyVarSet.empty)
      )
and freeTyVarsInValBinds(bound, nil, acc) = acc
  | freeTyVarsInValBinds(bound, PatBind(pat, exp) :: rest, acc) = freeTyVarsInValBinds(bound, rest, TyVarSet.union(acc, TyVarSet.union(freeTyVarsInPat(bound, pat), freeTyVarsInExp(bound, exp))))
  | freeTyVarsInValBinds(bound, TupleBind(xs, exp) :: rest, acc) = freeTyVarsInValBinds(bound, rest, TyVarSet.union(acc, freeTyVarsInExp(bound, exp))) (* TODO *)
  | freeTyVarsInValBinds(bound, PolyVarBind(vid, TypeScheme(tyvars, _), exp) :: rest, acc) = freeTyVarsInValBinds(bound, rest, TyVarSet.union(acc, freeTyVarsInExp(TyVarSet.addList(bound, List.map #1 tyvars), exp))) (* TODO *)

(* filterVarsInPat : (VId -> bool) -> Pat -> Pat *)
fun filterVarsInPat pred =
    let fun doPat pat = case pat of
                            WildcardPat => pat
                          | SConPat _ => pat
                          | VarPat(vid, ty) => if pred vid then pat else WildcardPat
                          | RecordPat(row, x) => RecordPat(Syntax.mapRecordRow doPat row, x)
                          | ConPat(_, NONE) => pat
                          | ConPat(longvid, SOME innerPat) => ConPat(longvid, SOME (doPat innerPat))
                          | InstantiatedConPat(_, NONE, _) => pat
                          | InstantiatedConPat(longvid, SOME innerPat, tyargs) => InstantiatedConPat(longvid, SOME (doPat innerPat), tyargs)
                          | TypedPat(innerPat, ty) => TypedPat(doPat innerPat, ty)
                          | LayeredPat(vid, ty, innerPat) => if pred vid then LayeredPat(vid, ty, doPat innerPat) else TypedPat(doPat innerPat, ty)
    in doPat
    end

(* filterVarsInPatAsList : (VId -> bool) -> Pat -> (VId * Ty) list *)
(*fun filterVarsInPatAsList pred pat =
    let fun doPat(xs, pat) = case pat of
                                 WildcardPat => xs
                               | SConPat _ => xs
                               | VarPat(vid, ty) => if pred vid then (vid, ty) :: xs else xs
                               | RecordPat(row, _) => List.foldl (fn ((label,innerPat),ys) => doPat(ys,innerPat)) xs row
                               | ConPat(_, NONE) => xs
                               | ConPat(longvid, SOME innerPat) => doPat(xs, innerPat)
                               | InstantiatedConPat(_, NONE, _) => xs
                               | InstantiatedConPat(_longvid, SOME innerPat, _tyargs) => doPat(xs, innerPat)
                               | TypedPat(innerPat, _ty) => doPat(xs, innerPat)
                               | LayeredPat(vid, ty, innerPat) => if pred vid then
                                                                      doPat((vid, ty) :: xs, innerPat)
                                                                  else
                                                                      doPat(xs, innerPat)
    in List.rev (doPat([], pat))
    end
*)
end (* structure USyntax *)

structure ToTypedSyntax = struct
exception NameError of string

type ('a,'b) Context = { nextTyVar : int ref
                       , nextVId : int ref
                       , nextTyCon : int ref
                       , tyVarConstraints : 'a
                       , tyVarSubst : 'b
                       , topDecs : (USyntax.TopDec list) ref
                       }

datatype BoundTyCon = BTyAlias of USyntax.TyVar list * USyntax.Ty
                    | BTyCon of int (* and data constructors *)
                    (* BDuplicatedTyCon of USyntax.TyVar list * USyntax.Ty *)
datatype Env = MkEnv of { valMap : (USyntax.VId * Syntax.IdStatus) Syntax.VIdMap.map
                        , tyConMap : BoundTyCon Syntax.TyConMap.map
                        , strMap : Env Syntax.StrIdMap.map
                        }
val emptyEnv = MkEnv { valMap = Syntax.VIdMap.empty
                     , tyConMap = Syntax.TyConMap.empty
                     , strMap = Syntax.StrIdMap.empty
                     }

fun mergeEnv(MkEnv env1 : Env, MkEnv env2 : Env)
    = MkEnv { valMap = Syntax.VIdMap.unionWith #2 (#valMap env1, #valMap env2)
            , tyConMap = Syntax.TyConMap.unionWith #2 (#tyConMap env1, #tyConMap env2)
            , strMap = Syntax.StrIdMap.unionWith #2 (#strMap env1, #strMap env2)
            }

fun envWithValEnv valEnv = MkEnv { valMap = valEnv
                                 , tyConMap = Syntax.TyConMap.empty
                                 , strMap = Syntax.StrIdMap.empty
                                 }
fun envWithTyConEnv tyConEnv = MkEnv { valMap = Syntax.VIdMap.empty
                                     , tyConMap = tyConEnv
                                     , strMap = Syntax.StrIdMap.empty
                                     }

type TVEnv = USyntax.TyVar Syntax.TyVarMap.map

local structure S = Syntax
      structure U = USyntax

      fun genTyVarId(ctx : ('a,'b) Context)
          = let val id = !(#nextTyVar ctx)
            in #nextTyVar ctx := id + 1 ; id end
      fun genTyVar(ctx, Syntax.MkTyVar tvname) = if String.isPrefix "''" tvname then
                                                     USyntax.NamedTyVar(tvname, true, genTyVarId(ctx))
                                                 else
                                                     USyntax.NamedTyVar(tvname, false, genTyVarId(ctx))
      fun freshTyVar(ctx : ('a,'b) Context) = USyntax.AnonymousTyVar(genTyVarId(ctx))

      fun newVId(ctx : ('a,'b) Context, Syntax.MkVId name) = let val n = !(#nextVId ctx)
                                                             in #nextVId ctx := n + 1
                                                              ; USyntax.MkVId(name, n)
                                                             end

      fun genTyConId(ctx : ('a,'b) Context)
          = let val id = !(#nextTyCon ctx)
            in #nextTyCon ctx := id + 1 ; id end
      fun newTyCon(ctx, Syntax.MkTyCon name) = USyntax.MkTyCon(name, genTyConId(ctx))

      fun lookupStr(env, nil) = env
        | lookupStr(MkEnv { strMap = strMap, ... }, (str0 as S.MkStrId name) :: str1)
          = case Syntax.StrIdMap.find(strMap, str0) of
                NONE => raise NameError("unknown structure name " ^ name)
              | SOME innerEnv => lookupStr(innerEnv, str1)

      fun lookupTyCon(MkEnv env, tycon as Syntax.MkTyCon name)
          = case Syntax.TyConMap.find(#tyConMap env, tycon) of
                NONE => raise NameError("unknown type constructor " ^ name)
              | SOME b => b
      fun lookupLongTyCon(env : Env, Syntax.MkQualified(strpath, tycon)) = lookupTyCon(lookupStr(env, strpath), tycon)

      fun lookupVId(MkEnv env, vid) = Syntax.VIdMap.find(#valMap env, vid)
      fun lookupLongVId(env : Env, Syntax.MkQualified(strpath, vid))
          = (case lookupVId(lookupStr(env, strpath), vid) of
                 NONE => NONE
               | SOME (vid', x) => SOME (USyntax.MkLongVId(strpath, vid'), x)
            )

      fun addTopDec(ctx : ('a,'b) Context, dec) = let val xs = !(#topDecs ctx)
                                                  in #topDecs ctx := dec :: xs
                                                  end
in
(* toUTy : Context * TVEnv * Env * Syntax.Ty -> USyntax.Ty *)
(* toUPat : Context * TVEnv * Env * Syntax.Pat -> USyntax.VId Syntax.VIdMap.map * USyntax.Pat *)
(* toUExp : Context * TVEnv * Env * Syntax.Exp -> USyntax.Exp *)
(* toUMatch : Context * TVEnv * Env * (Syntax.Pat * Syntax.Exp) list -> (USyntax.Pat * USyntax.Exp) list *)
(* toUDecs : Context * TVEnv * Env * Syntax.Dec list -> Env * USyntax.Dec list *)
fun toUTy(ctx : ('a,'b) Context, tvenv : TVEnv, env : Env, S.TyVar tv)
    = (case Syntax.TyVarMap.find(tvenv, tv) of
           NONE => raise NameError("unknown type variable " ^ Syntax.print_TyVar tv)
         | SOME tv => U.TyVar(tv)
      )
  | toUTy(ctx, tvenv, env, S.RecordType row) = U.RecordType(Syntax.mapRecordRow (fn ty => toUTy(ctx, tvenv, env, ty)) row)
  | toUTy(ctx, tvenv, env, S.TyCon(args, tycon))
    = (case lookupLongTyCon(env, tycon) of
           BTyCon id => U.TyCon(List.map (fn ty => toUTy(ctx, tvenv, env, ty)) args, U.MkLongTyCon(tycon, id))
         | BTyAlias _ => raise Fail "type alias not supported yet"
      )
  | toUTy(ctx, tvenv, env, S.FnType(ty1, ty2)) = U.FnType(toUTy(ctx, tvenv, env, ty1), toUTy(ctx, tvenv, env, ty2))
fun toUPat(ctx : ('a,'b) Context, tvenv : TVEnv, env : Env, S.WildcardPat) = (Syntax.VIdMap.empty, U.WildcardPat) (* TODO: should generate a type id? *)
  | toUPat(ctx, tvenv, env, S.SConPat(Syntax.RealConstant _)) = raise Syntax.SyntaxError "No real constant may occur in a pattern"
  | toUPat(ctx, tvenv, env, S.SConPat sc) = (Syntax.VIdMap.empty, U.SConPat sc)
  | toUPat(ctx, tvenv, env, S.ConOrVarPat vid)
    = (case lookupVId(env, vid) of
           SOME (vid', Syntax.ValueConstructor) => (Syntax.VIdMap.empty, U.ConPat(USyntax.MkLongVId([], vid'), NONE))
         | SOME (vid', Syntax.ExceptionConstructor) => (Syntax.VIdMap.empty, U.ConPat(USyntax.MkLongVId([], vid'), NONE))
         | _ => let val vid' = newVId(ctx, vid)
                in (Syntax.VIdMap.insert(Syntax.VIdMap.empty, vid, vid'), U.VarPat(vid', USyntax.TyVar(freshTyVar(ctx))))
                end
      )
  | toUPat(ctx, tvenv, env, S.VarPat vid) = let val vid' = newVId(ctx, vid)
                                            in (Syntax.VIdMap.insert(Syntax.VIdMap.empty, vid, vid'), U.VarPat(vid', USyntax.TyVar(freshTyVar(ctx)))) (* add extra type annotation *)
                                            end
  | toUPat(ctx, tvenv, env, S.RecordPat(row, wildcard))
    = let val (vidmap, row') = List.foldr (fn ((label, pat), (vidmap, row')) => let val (vidmap', pat') = toUPat(ctx, tvenv, env, pat)
                                                                                    (* TODO: error if vid conflict *)
                                                                                in (Syntax.VIdMap.unionWith #2 (vidmap, vidmap'), (label, pat') :: row')
                                                                                end
                                          ) (Syntax.VIdMap.empty, []) row
      in (vidmap, U.RecordPat(row', wildcard))
      end
  | toUPat(ctx, tvenv, env, S.ConPat(longvid, NONE))
    = (case lookupLongVId(env, longvid) of
           SOME (longvid', _) => (Syntax.VIdMap.empty, U.ConPat(longvid', NONE))
         | NONE => raise Syntax.SyntaxError "unbound identifier"
      )
  | toUPat(ctx, tvenv, env, S.ConPat(longvid, SOME pat))
    = let val (vidmap, pat') = toUPat(ctx, tvenv, env, pat)
      in case lookupLongVId(env, longvid) of
             SOME (longvid', _) => (vidmap, U.ConPat(longvid', SOME pat'))
           | NONE => raise Syntax.SyntaxError "unbound identifier"
      end
  | toUPat(ctx, tvenv, env, S.TypedPat(S.ConOrVarPat vid, ty))
    = (case lookupVId(env, vid) of
           SOME (vid', Syntax.ValueConstructor) => (Syntax.VIdMap.empty, U.TypedPat(U.ConPat(USyntax.MkLongVId([], vid'), NONE), toUTy(ctx, tvenv, env, ty)))
         | SOME (vid', Syntax.ExceptionConstructor) => (Syntax.VIdMap.empty, U.TypedPat(U.ConPat(USyntax.MkLongVId([], vid'), NONE), toUTy(ctx, tvenv, env, ty)))
         | _ => let val vid' = newVId(ctx, vid)
                in (Syntax.VIdMap.insert(Syntax.VIdMap.empty, vid, vid'), U.VarPat(vid', toUTy(ctx, tvenv, env, ty)))
                end
      )
  | toUPat(ctx, tvenv, env, S.TypedPat(S.VarPat vid, ty))
    = let val vid' = newVId(ctx, vid)
      in (Syntax.VIdMap.insert(Syntax.VIdMap.empty, vid, vid'), U.VarPat(vid', toUTy(ctx, tvenv, env, ty)))
      end
  | toUPat(ctx, tvenv, env, S.TypedPat(pat, ty))
    = let val (vidmap, pat') = toUPat(ctx, tvenv, env, pat)
      in (vidmap, U.TypedPat(pat', toUTy(ctx, tvenv, env, ty)))
      end
  | toUPat(ctx, tvenv, env, S.LayeredPat(vid, SOME ty, pat))
    = let val vid' = newVId(ctx, vid)
          val (vidmap, pat') = toUPat(ctx, tvenv, env, pat)
      in (Syntax.VIdMap.insert(vidmap, vid, vid'), U.LayeredPat(vid', toUTy(ctx, tvenv, env, ty), pat'))
      end
  | toUPat(ctx, tvenv, env, S.LayeredPat(vid, NONE, pat))
    = let val vid' = newVId(ctx, vid)
          val (vidmap, pat' ) = toUPat(ctx, tvenv, env, pat)
      in (Syntax.VIdMap.insert(vidmap, vid, vid'), U.LayeredPat(vid', USyntax.TyVar(freshTyVar(ctx)), pat'))
      end
fun toUExp(ctx : ('a,'b) Context, tvenv : TVEnv, env : Env, S.SConExp(scon)) = U.SConExp(scon)
  | toUExp(ctx, tvenv, env, S.VarExp(longvid))
    = (case lookupLongVId(env, longvid) of
           SOME (longvid', idstatus) => U.VarExp(longvid', idstatus)
         | NONE => raise Syntax.SyntaxError ("unbound identifier: " ^ Syntax.print_LongVId longvid)
      )
  | toUExp(ctx, tvenv, env, S.RecordExp(row)) = U.RecordExp(Syntax.mapRecordRow (fn exp => toUExp(ctx, tvenv, env, exp)) row)
  | toUExp(ctx, tvenv, env, S.LetInExp(decls, exp))
    = let val (env', decls') = toUDecs(ctx, tvenv, env, decls)
      in U.LetInExp(decls', toUExp(ctx, tvenv, mergeEnv(env, env'), exp))
      end
  | toUExp(ctx, tvenv, env, S.AppExp(exp1, exp2)) = U.AppExp(toUExp(ctx, tvenv, env, exp1), toUExp(ctx, tvenv, env, exp2))
  | toUExp(ctx, tvenv, env, S.TypedExp(exp, ty)) = U.TypedExp(toUExp(ctx, tvenv, env, exp), toUTy(ctx, tvenv, env, ty))
  | toUExp(ctx, tvenv, env, S.HandleExp(exp, ty)) = raise NameError("not implemented yet")
  | toUExp(ctx, tvenv, env, S.RaiseExp(exp)) = U.RaiseExp(toUExp(ctx, tvenv, env, exp))
  | toUExp(ctx, tvenv, env, S.IfThenElseExp(exp1, exp2, exp3)) = U.IfThenElseExp(toUExp(ctx, tvenv, env, exp1), toUExp(ctx, tvenv, env, exp2), toUExp(ctx, tvenv, env, exp3))
  | toUExp(ctx, tvenv, env, S.CaseExp(exp, match)) = U.CaseExp(toUExp(ctx, tvenv, env, exp), USyntax.TyVar(freshTyVar(ctx)), toUMatch(ctx, tvenv, env, match))
  | toUExp(ctx, tvenv, env, S.FnExp([(pat, body)]))
    = let fun determineConOrVar vid
              = (case lookupVId(env, vid) of
                     SOME (vid', Syntax.ValueConstructor) => S.ConPat(S.MkLongVId([], vid), NONE)
                   | SOME (vid', Syntax.ExceptionConstructor) => S.ConPat(S.MkLongVId([], vid), NONE)
                   | _ => S.VarPat vid
                )
          val pat' = case pat of
                         S.ConOrVarPat vid => determineConOrVar vid
                       | S.TypedPat(S.ConOrVarPat vid, ty) => S.TypedPat(determineConOrVar vid, ty)
                       | _ => pat
      in case pat' of
             S.VarPat vid => let val vid' = newVId(ctx, vid)
                                 val valEnv = Syntax.VIdMap.insert(Syntax.VIdMap.empty, vid, (vid', Syntax.ValueVariable))
                                 val env' = mergeEnv(env, envWithValEnv valEnv)
                             in U.FnExp(vid', USyntax.TyVar(freshTyVar(ctx)), toUExp(ctx, tvenv, env', body))
                             end
           | S.TypedPat(S.VarPat vid, ty) => let val vid' = newVId(ctx, vid)
                                                 val valEnv = Syntax.VIdMap.insert(Syntax.VIdMap.empty, vid, (vid', Syntax.ValueVariable))
                                                 val env' = mergeEnv(env, envWithValEnv valEnv)
                                             in U.FnExp(vid', toUTy(ctx, tvenv, env, ty), toUExp(ctx, tvenv, env', body))
                                             end
           | _ => let val vid' = newVId(ctx, Syntax.MkVId("a"))
                      val ty = USyntax.TyVar(freshTyVar(ctx))
                  in U.FnExp(vid', ty, U.CaseExp(U.VarExp(U.MkLongVId([], vid'), Syntax.ValueVariable), ty, toUMatch(ctx, tvenv, env, [(pat', body)])))
                  end
      end
  | toUExp(ctx, tvenv, env, S.FnExp(match))
    = let val vid' = newVId(ctx, Syntax.MkVId("a"))
          val ty = USyntax.TyVar(freshTyVar(ctx))
      in U.FnExp(vid', ty, U.CaseExp(U.VarExp(U.MkLongVId([], vid'), Syntax.ValueVariable), ty, toUMatch(ctx, tvenv, env, match)))
      end
  | toUExp(ctx, tvenv, env, S.ProjectionExp label) = U.ProjectionExp { label = label, recordTy = USyntax.TyVar(freshTyVar(ctx)), fieldTy = USyntax.TyVar(freshTyVar(ctx)) }
and toUMatch(ctx, tvenv, env, matches : (S.Pat * S.Exp) list)
    = List.map (fn (pat, exp) => let val (vidmap, pat') = toUPat(ctx, tvenv, env, pat)
                                     val valEnv = Syntax.VIdMap.map (fn vid => (vid, Syntax.ValueVariable)) vidmap
                                     val env' = mergeEnv(env, envWithValEnv valEnv)
                                 in (pat', toUExp(ctx, tvenv, env', exp))
                                 end
               ) matches
and toUDecs(ctx, tvenv, env, nil) = (emptyEnv, nil)
  | toUDecs(ctx, tvenv, env, decl :: decls)
    = (case decl of
           S.ValDec(tyvars, valbind) =>
           let val tyvars' = List.map (fn tv => (tv, genTyVar(ctx, tv))) tyvars
               val tvenv' = List.foldl Syntax.TyVarMap.insert' tvenv tyvars'
               val valbind' = List.map (fn S.PatBind(pat, exp) => let val (vidmap, pat') = toUPat(ctx, tvenv', env, pat)
                                                                  in (vidmap, U.PatBind(pat', toUExp(ctx, tvenv', env, exp)))
                                                                  end
                                       ) valbind
               val decl' = U.ValDec(List.map #2 tyvars', List.map #2 valbind', USyntax.VIdMap.empty)
               val vidmap = List.foldl (Syntax.VIdMap.unionWith #2) Syntax.VIdMap.empty (List.map #1 valbind')
               val venv = envWithValEnv (Syntax.VIdMap.map (fn vid => (vid, Syntax.ValueVariable)) vidmap)
               val (env', decls') = toUDecs(ctx, tvenv, mergeEnv(env, venv), decls)
           in (mergeEnv(venv, env'), decl' :: decls')
           end
         | S.RecValDec(tyvars, valbind) =>
           let val tyvars' = List.map (fn tv => (tv, genTyVar(ctx, tv))) tyvars
               val tvenv' = List.foldl Syntax.TyVarMap.insert' tvenv tyvars'
               val valbind' = List.map (fn S.PatBind(pat, exp) => (toUPat(ctx, tvenv', env, pat), exp)) valbind
               val vidmap = List.foldl (Syntax.VIdMap.unionWith #2) Syntax.VIdMap.empty (List.map (fn x => #1 (#1 x)) valbind')
               val venv = envWithValEnv (Syntax.VIdMap.map (fn vid => (vid, Syntax.ValueVariable)) vidmap)
               val env' = mergeEnv(env, venv)
               val valbind'' = List.map (fn ((_, pat'), exp) => U.PatBind(pat', toUExp(ctx, tvenv', env', exp))) valbind'
               val decl' = U.RecValDec(List.map #2 tyvars', valbind'', U.VIdMap.empty)
               val (env'', decls') = toUDecs(ctx, tvenv, env', decls)
           in (mergeEnv(venv, env''), decl' :: decls')
           end
         | S.TypeDec(typbinds) =>
           let fun doTypBind (S.TypBind(tyvars, tycon, ty), (tyConEnv, typbinds))
                   = let val tyvars' = List.map (fn tv => (tv, genTyVar(ctx, tv))) tyvars
                         val tvenv = List.foldl Syntax.TyVarMap.insert' Syntax.TyVarMap.empty tyvars'
                         val tyvars'' = List.map #2 tyvars'
                         val ty' = toUTy(ctx, tvenv, env, ty)
                         val tycon' = newTyCon(ctx, tycon)
                     in (Syntax.TyConMap.insert(tyConEnv, tycon, BTyAlias(tyvars'', ty')), U.TypBind(tyvars'', tycon', ty') :: typbinds)
                     end
               val (tyConEnv, typbinds') = List.foldr doTypBind (Syntax.TyConMap.empty, []) typbinds
               val tenv = envWithTyConEnv tyConEnv
               val (env', decls') = toUDecs(ctx, tvenv, mergeEnv(env, tenv), decls)
               val _ = addTopDec(ctx, U.TypeDec(typbinds'))
           in (mergeEnv(tenv, env'), decls')
           end
         | S.DatatypeDec(datbinds) =>
           let fun doDatBind1 (S.DatBind(tyvars, tycon, conbinds), (tyConEnv, datbinds))
                   = let val tyvars' = List.map (fn tv => (tv, genTyVar(ctx, tv))) tyvars
                         val tvenv = List.foldl Syntax.TyVarMap.insert' Syntax.TyVarMap.empty tyvars'
                         val tycon' = newTyCon(ctx, tycon)
                     in (Syntax.TyConMap.insert(tyConEnv, tycon, tycon'), (List.map #2 tyvars', tvenv, tycon', conbinds) :: datbinds)
                     end
               val (tyConEnv, datbinds') = List.foldr doDatBind1 (Syntax.TyConMap.empty, []) datbinds
               val tyConEnv' = envWithTyConEnv(Syntax.TyConMap.map (fn USyntax.MkTyCon(_,n) => BTyCon n) tyConEnv)
               val env' = mergeEnv(env, tyConEnv')
               fun doDatBind2 ((tyvars, tvenv, tycon, conbinds), (valEnv, datbinds))
                   = let fun doConBind (S.ConBind(vid, optPayloadTy), (valEnv, conbinds))
                             = let val vid' = newVId(ctx, vid)
                                   val optPayloadTy' = case optPayloadTy of
                                                           NONE => NONE
                                                         | SOME payloadTy => SOME (toUTy(ctx, tvenv, env', payloadTy))
                               in (Syntax.VIdMap.insert(valEnv, vid, (vid', Syntax.ValueConstructor)), U.ConBind(vid', optPayloadTy') :: conbinds)
                               end
                         val (valEnv', conbinds') = List.foldr doConBind (Syntax.VIdMap.empty, []) conbinds
                     in (Syntax.VIdMap.unionWith #2 (valEnv', valEnv), USyntax.DatBind(tyvars, tycon, conbinds') :: datbinds)
                     end
               val (valEnv, datbinds'') = List.foldr doDatBind2 (Syntax.VIdMap.empty, []) datbinds'
               val datbindEnv = mergeEnv(tyConEnv', envWithValEnv valEnv)
               val (env', decls') = toUDecs(ctx, tvenv, mergeEnv(env, datbindEnv), decls)
               val _ = addTopDec(ctx, U.DatatypeDec(datbinds''))
           in (mergeEnv(datbindEnv, env'), decls')
           end
         | S.DatatypeRepDec(tycon, longtycon) =>
           let val _ = lookupLongTyCon(env, longtycon)
               val (env', decls') = toUDecs(ctx, tvenv, (* TODO: add type ctor *) env, decls)
           in raise Fail "datatype replication: not implemented yet" (* ((* TODO: add type ctor *) env', decl' :: decls') *)
           end
         | S.AbstypeDec(datbinds, dec) =>
           let val decl' = raise Fail "not implemented yet"
               val (env', decls') = toUDecs(ctx, tvenv, (* TODO: add type ctor *) env, decls)
           in ((* TODO: add type ctor *) env', decl' :: decls')
           end
         | S.ExceptionDec(_) =>
           let val decl' = raise Fail "not implemented yet"
               val (env', decls') = toUDecs(ctx, tvenv, (* TODO: add exception ctor *) env, decls)
           in ((* TODO: add exception ctor *) env', decl' :: decls')
           end
         | S.LocalDec(decls1, decls2) =>
           let val (env1, decls1') = toUDecs(ctx, tvenv, env, decls1)
               val (env2, decls2') = toUDecs(ctx, tvenv, mergeEnv(env, env1), decls2)
               val (env', decls') = toUDecs(ctx, tvenv, mergeEnv(env, env2), decls)
           in (mergeEnv(env2, env'), decls1' @ decls2' @ decls') end
         | S.OpenDec(_) =>
           let val decl' = raise Fail "not implemented yet"
               val (env', decls') = toUDecs(ctx, tvenv, (* TODO: add type ctor *) env, decls)
           in ((* TODO: add type ctor *) env', decl' :: decls')
           end
         | S.FixityDec(_) => toUDecs(ctx, tvenv, env, decls) (* ignore *)
      )
end (* local *)
end (* structure ToTypedSyntax *)
