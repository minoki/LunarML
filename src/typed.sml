(*
 * Copyright (c) 2021 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure USyntax = struct
datatype VId = MkVId of string * int
type LongVId = VId Syntax.Qualified
fun MkLongVId(strids, vid: VId) = Syntax.MkQualified(strids, vid)
datatype TyVar = NamedTyVar of string * bool * int
               | AnonymousTyVar of int
datatype TyCon = MkTyCon of string * int
fun eqUTyVar(NamedTyVar(name,eq,a),NamedTyVar(name',eq',b)) = name = name' andalso eq = eq' andalso a = b
  | eqUTyVar(AnonymousTyVar a, AnonymousTyVar b) = a = b
  | eqUTyVar(_, _) = false
fun eqUTyCon(MkTyCon(_,a),MkTyCon(_,b)) = a = b
fun eqVId(a, b : VId) = a = b
fun eqULongVId(Syntax.MkQualified(_,a),Syntax.MkQualified(_,b)) = a = b

datatype Ty = TyVar of SourcePos.span * TyVar (* type variable *)
            | RecordType of SourcePos.span * (Syntax.Label * Ty) list (* record type expression *)
            | TyCon of SourcePos.span * Ty list * TyCon (* type construction *)
            | FnType of SourcePos.span * Ty * Ty (* function type expression *)

fun PairType(span, a, b) = RecordType(span, [(Syntax.NumericLabel 1, a), (Syntax.NumericLabel 2, b)])
fun TupleType(span, xs) = let fun doFields i nil = nil
                                | doFields i (x :: xs) = (Syntax.NumericLabel i, x) :: doFields (i + 1) xs
                          in RecordType (span, doFields 1 xs)
                          end

structure VIdKey = struct
type ord_key = VId
fun compare(MkVId(x,a), MkVId(y,b)) = case String.compare (x,y) of
                                          EQUAL => Int.compare(a,b)
                                        | ord => ord
end : ORD_KEY
structure VIdSet = RedBlackSetFn(VIdKey)
structure VIdMap = RedBlackMapFn(VIdKey)

structure TyConKey = struct
type ord_key = TyCon
fun compare(MkTyCon(x,a), MkTyCon(y,b)) = case String.compare (x,y) of
                                              EQUAL => Int.compare(a,b)
                                            | ord => ord
end : ORD_KEY
structure TyConSet = RedBlackSetFn(TyConKey)
structure TyConMap = RedBlackMapFn(TyConKey)

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
structure TyVarSet = RedBlackSetFn(TyVarKey)
structure TyVarMap = RedBlackMapFn(TyVarKey)

datatype UnaryConstraint
  = HasField of { sourceSpan : SourcePos.span
                , label : Syntax.Label
                , fieldTy : Ty
                }
  | IsEqType of SourcePos.span
  | IsIntegral of SourcePos.span (* Int, Word; div, mod; defaults to int *)
  | IsSignedReal of SourcePos.span (* Int, Real; abs; defaults to int *)
  | IsRing of SourcePos.span (* Int, Word, Real; *, +, -, ~; defaults to int *)
  | IsField of SourcePos.span (* Real; /; defaults to real *)
  | IsSigned of SourcePos.span (* Int, Real; defaults to int *)
  | IsOrdered of SourcePos.span (* NumTxt; <, >, <=, >=; defaults to int *)

datatype Constraint
  = EqConstr of SourcePos.span * Ty * Ty (* ty1 = ty2 *)
  | UnaryConstraint of SourcePos.span * Ty * UnaryConstraint

datatype TypeFcn = TypeFcn of TyVar list * Ty
datatype TypeScheme = TypeScheme of (TyVar * UnaryConstraint list) list * Ty
type ValEnv = (TypeScheme * Syntax.IdStatus) VIdMap.map
val emptyValEnv = VIdMap.empty

datatype Pat = WildcardPat of SourcePos.span
             | SConPat of SourcePos.span * Syntax.SCon (* special constant *)
             | VarPat of SourcePos.span * VId * Ty (* variable *)
             | RecordPat of { sourceSpan : SourcePos.span, fields : (Syntax.Label * Pat) list, wildcard : bool }
             | ConPat of SourcePos.span * LongVId * Pat option (* constructed pattern *)
             | InstantiatedConPat of SourcePos.span * LongVId * Pat option * Ty list
             | TypedPat of SourcePos.span * Pat * Ty (* typed *)
             | LayeredPat of SourcePos.span * VId * Ty * Pat (* layered *)

datatype TypBind = TypBind of SourcePos.span * TyVar list * TyCon * Ty
datatype ConBind = ConBind of SourcePos.span * VId * Ty option
datatype DatBind = DatBind of SourcePos.span * TyVar list * TyCon * ConBind list * (* admits equality? (after type check) *) bool
datatype ExBind = ExBind of SourcePos.span * VId * Ty option (* <op> vid <of ty> *)

datatype Exp = SConExp of SourcePos.span * Syntax.SCon (* special constant *)
             | VarExp of SourcePos.span * LongVId * Syntax.IdStatus (* value identifier; IdStatus is used by isNonexpansive *)
             | InstantiatedVarExp of SourcePos.span * LongVId * Syntax.IdStatus * (Ty * UnaryConstraint list) list (* identifiers with type arguments; produced during type-checking *)
             | RecordExp of SourcePos.span * (Syntax.Label * Exp) list (* record *)
             | LetInExp of SourcePos.span * Dec list * Exp (* local declaration *)
             | AppExp of SourcePos.span * Exp * Exp (* function, argument *)
             | TypedExp of SourcePos.span * Exp * Ty
             | HandleExp of SourcePos.span * Exp * (Pat * Exp) list
             | RaiseExp of SourcePos.span * Exp
             | IfThenElseExp of SourcePos.span * Exp * Exp * Exp
             | CaseExp of SourcePos.span * Exp * Ty * (Pat * Exp) list
             | FnExp of SourcePos.span * VId * Ty * Exp (* parameter name, parameter type, body *)
             | ProjectionExp of { sourceSpan : SourcePos.span, label : Syntax.Label, recordTy : Ty, fieldTy : Ty }
             | ListExp of SourcePos.span * Exp vector * Ty
     and Dec = ValDec of SourcePos.span * TyVar list * ValBind list (* non-recursive *)
             | RecValDec of SourcePos.span * TyVar list * ValBind list (* recursive (val rec) *)
             | ValDec' of SourcePos.span * ValBind' list (* non-recursive; produced during type-check *)
             | RecValDec' of SourcePos.span * ValBind' list (* recursive (val rec); produced during type-check *)
             | TypeDec of SourcePos.span * TypBind list (* not used by the type checker *)
             | DatatypeDec of SourcePos.span * DatBind list
             | ExceptionDec of SourcePos.span * ExBind list
     and ValBind = PatBind of SourcePos.span * Pat * Exp
     and ValBind' = TupleBind of SourcePos.span * (VId * Ty) list * Exp (* monomorphic binding; produced during type-check *)
                  | PolyVarBind of SourcePos.span * VId * TypeScheme * Exp (* polymorphic binding; produced during type-check *)
datatype TopDec = StrDec of Dec list
type Program = TopDec list

local
    fun doFields i nil = nil
      | doFields i (x :: xs) = (Syntax.NumericLabel i, x) :: doFields (i + 1) xs
in 
fun TuplePat(span, xs) = RecordPat { sourceSpan = span, fields = doFields 1 xs, wildcard = false }
fun TupleExp(span, xs) = RecordExp (span, doFields 1 xs)
end

fun getSourceSpanOfTy(TyVar(span, _)) = span
  | getSourceSpanOfTy(RecordType(span, _)) = span
  | getSourceSpanOfTy(TyCon(span, _, _)) = span
  | getSourceSpanOfTy(FnType(span, _, _)) = span

fun getSourceSpanOfExp(SConExp(span, _)) = span
  | getSourceSpanOfExp(VarExp(span, _, _)) = span
  | getSourceSpanOfExp(InstantiatedVarExp(span, _, _, _)) = span
  | getSourceSpanOfExp(RecordExp(span, _)) = span
  | getSourceSpanOfExp(LetInExp(span, _, _)) = span
  | getSourceSpanOfExp(AppExp(span, _, _)) = span
  | getSourceSpanOfExp(TypedExp(span, _, _)) = span
  | getSourceSpanOfExp(HandleExp(span, _, _)) = span
  | getSourceSpanOfExp(RaiseExp(span, _)) = span
  | getSourceSpanOfExp(IfThenElseExp(span, _, _, _)) = span
  | getSourceSpanOfExp(CaseExp(span, _, _, _)) = span
  | getSourceSpanOfExp(FnExp(span, _, _, _)) = span
  | getSourceSpanOfExp(ProjectionExp{sourceSpan, ...}) = sourceSpan
  | getSourceSpanOfExp(ListExp(span, _, _)) = span

(* pretty printing *)
structure PrettyPrint = struct
fun print_VId(MkVId(name, n)) = "MkVId(\"" ^ String.toString name ^ "\"," ^ Int.toString n ^ ")"
fun print_LongVId(Syntax.MkQualified(strids, vid)) = "MkLongVId(" ^ Syntax.print_list Syntax.print_StrId strids ^ "," ^ print_VId vid ^ ")"
fun print_TyVar(NamedTyVar(tvname, eq, n)) = "NamedTyVar(\"" ^ String.toString tvname ^ "\"," ^ Bool.toString eq ^ "," ^ Int.toString n ^ ")"
  | print_TyVar(AnonymousTyVar(n)) = "AnonymousTyVar(" ^ Int.toString n ^ ")"
fun print_TyCon (MkTyCon ("int", 0)) = "primTyCon_int"
  | print_TyCon (MkTyCon ("word", 1)) = "primTyCon_word"
  | print_TyCon (MkTyCon ("real", 2)) = "primTyCon_real"
  | print_TyCon (MkTyCon ("string", 3)) = "primTyCon_string"
  | print_TyCon (MkTyCon ("char", 4)) = "primTyCon_char"
  | print_TyCon (MkTyCon ("exn", 5)) = "primTyCon_exn"
  | print_TyCon (MkTyCon ("bool", 6)) = "primTyCon_bool"
  | print_TyCon (MkTyCon ("ref", 7)) = "primTyCon_ref"
  | print_TyCon (MkTyCon ("list", 8)) = "primTyCon_list"
  | print_TyCon (MkTyCon(tyconname, n)) = "MkTyCon(\"" ^ String.toString tyconname ^ "\"," ^ Int.toString n ^ ")"
fun print_Ty (TyVar(_,x)) = "TyVar(" ^ print_TyVar x ^ ")"
  | print_Ty (RecordType(_,xs)) = (case Syntax.extractTuple (1, xs) of
                                       NONE => "RecordType " ^ Syntax.print_list (Syntax.print_pair (Syntax.print_Label,print_Ty)) xs
                                     | SOME ys => "TupleType " ^ Syntax.print_list print_Ty ys
                                  )
  | print_Ty (TyCon(_,[],MkTyCon("int", 0))) = "primTy_int"
  | print_Ty (TyCon(_,[],MkTyCon("word", 1))) = "primTy_word"
  | print_Ty (TyCon(_,[],MkTyCon("real", 2))) = "primTy_real"
  | print_Ty (TyCon(_,[],MkTyCon("string", 3))) = "primTy_string"
  | print_Ty (TyCon(_,[],MkTyCon("char", 4))) = "primTy_char"
  | print_Ty (TyCon(_,[],MkTyCon("exn", 5))) = "primTy_exn"
  | print_Ty (TyCon(_,[],MkTyCon("bool", 6))) = "primTy_bool"
  | print_Ty (TyCon(_,x,y)) = "TyCon(" ^ Syntax.print_list print_Ty x ^ "," ^ print_TyCon y ^ ")"
  | print_Ty (FnType(_,x,y)) = "FnType(" ^ print_Ty x ^ "," ^ print_Ty y ^ ")"
fun print_Pat (WildcardPat _) = "WildcardPat"
  | print_Pat (SConPat(_, x)) = "SConPat(" ^ Syntax.print_SCon x ^ ")"
  | print_Pat (VarPat(_, vid, ty)) = "VarPat(" ^ print_VId vid ^ "," ^ print_Ty ty ^ ")"
  | print_Pat (TypedPat (_, pat, ty)) = "TypedPat(" ^ print_Pat pat ^ "," ^ print_Ty ty ^ ")"
  | print_Pat (LayeredPat (_, vid, ty, pat)) = "TypedPat(" ^ print_VId vid ^ "," ^ print_Ty ty ^ "," ^ print_Pat pat ^ ")"
  | print_Pat (ConPat(_, longvid, pat)) = "ConPat(" ^ print_LongVId longvid ^ "," ^ Syntax.print_option print_Pat pat ^ ")"
  | print_Pat (InstantiatedConPat(_, longvid, pat, tyargs)) = "InstantiatedConPat(" ^ print_LongVId longvid ^ "," ^ Syntax.print_option print_Pat pat ^ "," ^ Syntax.print_list print_Ty tyargs ^ ")"
  | print_Pat (RecordPat{fields = x, wildcard = false, ...}) = (case Syntax.extractTuple (1, x) of
                                                               NONE => "RecordPat(" ^ Syntax.print_list (Syntax.print_pair (Syntax.print_Label, print_Pat)) x ^ ",false)"
                                                             | SOME ys => "TuplePat " ^ Syntax.print_list print_Pat ys
                                                          )
  | print_Pat (RecordPat{fields = x, wildcard = true, ...}) = "RecordPat(" ^ Syntax.print_list (Syntax.print_pair (Syntax.print_Label, print_Pat)) x ^ ",true)"
(* | print_Pat _ = "<Pat>" *)
fun print_Exp (SConExp(_, x)) = "SConExp(" ^ Syntax.print_SCon x ^ ")"
  | print_Exp (VarExp(_, Syntax.MkQualified([], vid), idstatus)) = "SimpleVarExp(" ^ print_VId vid ^ "," ^ Syntax.print_IdStatus idstatus ^ ")"
  | print_Exp (VarExp(_, x, idstatus)) = "VarExp(" ^ print_LongVId x ^ "," ^ Syntax.print_IdStatus idstatus ^ ")"
  | print_Exp (InstantiatedVarExp(_, x, idstatus, tyargs)) = "InstantiatedVarExp(" ^ print_LongVId x ^ "," ^ Syntax.print_IdStatus idstatus ^ "," ^ Syntax.print_list (Syntax.print_pair (print_Ty, Syntax.print_list print_UnaryConstraint)) tyargs ^ ")"
  | print_Exp (RecordExp(_, x)) = (case Syntax.extractTuple (1, x) of
                                       NONE => "RecordExp " ^ Syntax.print_list (Syntax.print_pair (Syntax.print_Label, print_Exp)) x
                                     | SOME ys => "TupleExp " ^ Syntax.print_list print_Exp ys
                                  )
  | print_Exp (LetInExp(_,decls,x)) = "LetInExp(" ^ Syntax.print_list print_Dec decls ^ "," ^ print_Exp x ^ ")"
  | print_Exp (AppExp(_,x,y)) = "AppExp(" ^ print_Exp x ^ "," ^ print_Exp y ^ ")"
  | print_Exp (TypedExp(_,x,y)) = "TypedExp(" ^ print_Exp x ^ "," ^ print_Ty y ^ ")"
  | print_Exp (HandleExp(_,x,y)) = "HandleExp(" ^ print_Exp x ^ "," ^ Syntax.print_list (Syntax.print_pair (print_Pat, print_Exp)) y ^ ")"
  | print_Exp (RaiseExp(_,x)) = "RaiseExp(" ^ print_Exp x ^ ")"
  | print_Exp (IfThenElseExp(_,x,y,z)) = "IfThenElseExp(" ^ print_Exp x ^ "," ^ print_Exp y ^ "," ^ print_Exp z ^ ")"
  | print_Exp (CaseExp(_,x,ty,y)) = "CaseExp(" ^ print_Exp x ^ "," ^ print_Ty ty ^ "," ^ Syntax.print_list (Syntax.print_pair (print_Pat,print_Exp)) y ^ ")"
  | print_Exp (FnExp(_,pname,pty,body)) = "FnExp(" ^ print_VId pname ^ "," ^ print_Ty pty ^ "," ^ print_Exp body ^ ")"
  | print_Exp (ProjectionExp { label = label, recordTy = recordTy, fieldTy = fieldTy, ... }) = "ProjectionExp{label=" ^ Syntax.print_Label label ^ ",recordTy=" ^ print_Ty recordTy ^ ",fieldTy=" ^ print_Ty fieldTy ^ "}"
  | print_Exp (ListExp _) = "ListExp"
and print_Dec (ValDec (_,bound,valbind)) = "ValDec(" ^ Syntax.print_list print_TyVar bound ^ "," ^ Syntax.print_list print_ValBind valbind ^ ")"
  | print_Dec (RecValDec (_,bound,valbind)) = "RecValDec(" ^ Syntax.print_list print_TyVar bound ^ "," ^ Syntax.print_list print_ValBind valbind ^ ")"
  | print_Dec (ValDec'(_,valbinds)) = "ValDec'(" ^ Syntax.print_list print_ValBind' valbinds ^ ")"
  | print_Dec (RecValDec'(_,valbinds)) = "RecValDec'(" ^ Syntax.print_list print_ValBind' valbinds ^ ")"
  | print_Dec (TypeDec(_, typbinds)) = "TypeDec(" ^ Syntax.print_list print_TypBind typbinds ^ ")"
  | print_Dec (DatatypeDec(_, datbinds)) = "DatatypeDec(" ^ Syntax.print_list print_DatBind datbinds ^ ")"
  | print_Dec (ExceptionDec(_, exbinds)) = "ExceptionDec"
and print_TypBind (TypBind(_, tyvars, tycon, ty)) = "TypBind(" ^ Syntax.print_list print_TyVar tyvars ^ "," ^ print_TyCon tycon ^ "," ^ print_Ty ty ^ ")"
and print_DatBind (DatBind(_, tyvars, tycon, conbinds, _)) = "DatBind(" ^ Syntax.print_list print_TyVar tyvars ^ "," ^ print_TyCon tycon ^ "," ^ Syntax.print_list print_ConBind conbinds ^ ")"
and print_ConBind (ConBind(_, vid, NONE)) = "ConBind(" ^ print_VId vid ^ ",NONE)"
  | print_ConBind (ConBind(_, vid, SOME ty)) = "ConBind(" ^ print_VId vid ^ ",SOME " ^ print_Ty ty ^ ")"
and print_ValBind (PatBind (_, pat, exp)) = "PatBind(" ^ print_Pat pat ^ "," ^ print_Exp exp ^ ")"
and print_ValBind' (TupleBind (_, xs, exp)) = "TupleBind(" ^ Syntax.print_list (Syntax.print_pair (print_VId, print_Ty)) xs ^ "," ^ print_Exp exp ^ ")"
  | print_ValBind' (PolyVarBind (_, name, tysc, exp)) = "PolyVarBind(" ^ print_VId name ^ "," ^ print_TypeScheme tysc ^ "," ^ print_Exp exp ^ ")"
and print_TyVarMap print_elem x = Syntax.print_list (Syntax.print_pair (print_TyVar,print_elem)) (TyVarMap.foldri (fn (k,x,ys) => (k,x) :: ys) [] x)
and print_VIdMap print_elem x = Syntax.print_list (Syntax.print_pair (print_VId,print_elem)) (VIdMap.foldri (fn (k,x,ys) => (k,x) :: ys) [] x)
and print_UnaryConstraint (HasField { sourceSpan, label, fieldTy }) = "HasField{label=" ^ Syntax.print_Label label ^ ",fieldTy=" ^ print_Ty fieldTy ^ "}"
  | print_UnaryConstraint (IsEqType _) = "IsEqType"
  | print_UnaryConstraint (IsIntegral _) = "IsIntegral"
  | print_UnaryConstraint (IsSignedReal _) = "IsSignedReal"
  | print_UnaryConstraint (IsRing _) = "IsRing"
  | print_UnaryConstraint (IsField _) = "IsField"
  | print_UnaryConstraint (IsSigned _) = "IsSigned"
  | print_UnaryConstraint (IsOrdered _) = "IsOrdered"
and print_TypeScheme (TypeScheme(tyvars, ty)) = "TypeScheme(" ^ Syntax.print_list (Syntax.print_pair (print_TyVar, Syntax.print_list print_UnaryConstraint)) tyvars ^ "," ^ print_Ty ty ^ ")"
and print_ValEnv env = print_VIdMap (Syntax.print_pair (print_TypeScheme,Syntax.print_IdStatus)) env
fun print_TyVarSet x = Syntax.print_list print_TyVar (TyVarSet.foldr (fn (x,ys) => x :: ys) [] x)
fun print_TyConMap print_elem x = Syntax.print_list (Syntax.print_pair (print_TyCon,print_elem)) (TyConMap.foldri (fn (k,x,ys) => (k,x) :: ys) [] x)
val print_Decs = Syntax.print_list print_Dec
fun print_TopDec (StrDec decs) = "StrDec(" ^ Syntax.print_list print_Dec decs ^ ")"
val print_Program = Syntax.print_list print_TopDec
end (* structure PrettyPrint *)
open PrettyPrint

(* freeTyVarsInTy : TyVarSet * Ty -> TyVarSet *)
fun freeTyVarsInTy(bound, ty)
    = (case ty of
           TyVar(_,tv) => if TyVarSet.member(bound, tv) then
                              TyVarSet.empty
                          else
                              TyVarSet.singleton tv
         | RecordType(_,xs) => List.foldl (fn ((_, ty), set) => TyVarSet.union(freeTyVarsInTy(bound, ty), set)) TyVarSet.empty xs
         | TyCon(_,xs,_) => List.foldl (fn (ty, set) => TyVarSet.union(freeTyVarsInTy(bound, ty), set)) TyVarSet.empty xs
         | FnType(_,s,t) => TyVarSet.union(freeTyVarsInTy(bound, s), freeTyVarsInTy(bound, t))
      )

(* applySubstTy : Ty TyVarMap.map -> Ty -> Ty *)
fun applySubstTy subst =
    let fun substTy (ty as TyVar(_, tv'))
            = (case TyVarMap.find(subst, tv') of
                   NONE => ty
                 | SOME replacement => replacement
              )
          | substTy (RecordType(span, fields)) = RecordType (span, Syntax.mapRecordRow substTy fields)
          | substTy (TyCon(span, tyargs, tycon)) = TyCon(span, List.map substTy tyargs, tycon)
          | substTy (FnType(span, ty1, ty2)) = FnType(span, substTy ty1, substTy ty2)
    in substTy
    end

(* mapTy : Context * Ty TyVarMap.map * bool -> { doExp : Exp -> Exp, doDec : Dec -> Dec, doDecs : Dec list -> Dec list, doValBind : ValBind -> ValBind } *)
fun mapTy (ctx : { nextTyVar : int ref, nextVId : 'a, nextTyCon : 'b, tyVarConstraints : 'c, tyVarSubst : 'd }, subst, avoidCollision) =
    let val doTy = applySubstTy subst
        val range = TyVarMap.foldl (fn (ty, tyvarset) => TyVarSet.union(freeTyVarsInTy(TyVarSet.empty, ty), tyvarset)) TyVarSet.empty subst
        fun genFreshTyVars(subst, tyvars) = List.foldr (fn (tv, (subst, tyvars)) => if avoidCollision andalso TyVarSet.member (range, tv) then
                                                                                        let val nextTyVar = #nextTyVar ctx
                                                                                            val x = !nextTyVar
                                                                                            val () = nextTyVar := x + 1
                                                                                            val tv' = case tv of
                                                                                                          NamedTyVar(name, eq, _) => NamedTyVar(name, eq, x)
                                                                                                        | AnonymousTyVar _ => AnonymousTyVar x
                                                                                        in (TyVarMap.insert (subst, tv, TyVar(SourcePos.nullSpan, tv')), tv' :: tyvars)
                                                                                        end
                                                                                    else
                                                                                        (subst, tv :: tyvars))
                                                       (subst, []) tyvars
        fun doUnaryConstraint(HasField{sourceSpan, label, fieldTy}) = HasField{sourceSpan=sourceSpan, label=label, fieldTy=doTy fieldTy}
          | doUnaryConstraint ct = ct
        fun doTypeScheme(TypeScheme (tyvarsWithConstraints, ty)) = let val (subst, tyvars) = genFreshTyVars(subst, List.map #1 tyvarsWithConstraints)
                                                                       val constraints = List.map (fn (_, cts) => List.map doUnaryConstraint cts) tyvarsWithConstraints
                                                                   in TypeScheme (ListPair.zip (tyvars, constraints), applySubstTy subst ty)
                                                                   end
        val doValEnv = VIdMap.map (fn (tysc, idstatus) => (doTypeScheme tysc, idstatus))
        fun doExp(e as SConExp _) = e
          | doExp(e as VarExp _) = e
          | doExp(InstantiatedVarExp(span, longvid, idstatus, tyargs)) = InstantiatedVarExp(span, longvid, idstatus, List.map (fn (ty, cts) => (doTy ty, List.map doUnaryConstraint cts)) tyargs)
          | doExp(RecordExp(span, fields)) = RecordExp(span, Syntax.mapRecordRow doExp fields)
          | doExp(LetInExp(span, decls, e)) = LetInExp(span, List.map doDec decls, doExp e)
          | doExp(AppExp(span, e1, e2)) = AppExp(span, doExp e1, doExp e2)
          | doExp(TypedExp(span, e, ty)) = TypedExp(span, doExp e, doTy ty)
          | doExp(HandleExp(span, e, matches)) = HandleExp(span, doExp e, List.map doMatch matches)
          | doExp(RaiseExp(span, e)) = RaiseExp(span, doExp e)
          | doExp(IfThenElseExp(span, e1, e2, e3)) = IfThenElseExp(span, doExp e1, doExp e2, doExp e3)
          | doExp(CaseExp(span, e, ty, matches)) = CaseExp(span, doExp e, doTy ty, List.map doMatch matches)
          | doExp(FnExp(span, vid, ty, body)) = FnExp(span, vid, doTy ty, doExp body)
          | doExp(ProjectionExp { sourceSpan, label, recordTy, fieldTy }) = ProjectionExp { sourceSpan = sourceSpan, label = label, recordTy = doTy recordTy, fieldTy = doTy fieldTy }
          | doExp(ListExp(span, xs, ty)) = ListExp(span, Vector.map doExp xs, doTy ty)
        and doDec(ValDec(span, tyvars, valbind)) = let val (subst, tyvars) = genFreshTyVars(subst, tyvars)
                                                   in ValDec(span, tyvars, List.map (#doValBind (mapTy (ctx, subst, avoidCollision))) valbind)
                                                   end
          | doDec(RecValDec(span, tyvars, valbind)) = let val (subst, tyvars) = genFreshTyVars(subst, tyvars)
                                                      in RecValDec(span, tyvars, List.map (#doValBind (mapTy (ctx, subst, avoidCollision))) valbind)
                                                      end
          | doDec(ValDec'(span, valbind)) = ValDec'(span, List.map doValBind' valbind)
          | doDec(RecValDec'(span, valbind)) = RecValDec'(span, List.map doValBind' valbind)
          | doDec(TypeDec(span, typbinds)) = TypeDec(span, List.map doTypBind typbinds)
          | doDec(DatatypeDec(span, datbinds)) = DatatypeDec(span, List.map doDatBind datbinds)
          | doDec(ExceptionDec(span, exbinds)) = ExceptionDec(span, List.map doExBind exbinds)
        and doValBind(PatBind(span, pat, exp)) = PatBind(span, doPat pat, doExp exp)
        and doValBind'(TupleBind(span, xs, exp)) = TupleBind(span, List.map (fn (vid, ty) => (vid, doTy ty)) xs, doExp exp)
          | doValBind'(PolyVarBind(span, vid, tysc as TypeScheme (tyvarsWithConstraints, ty), exp)) = let val (subst, tyvars) = genFreshTyVars(subst, List.map #1 tyvarsWithConstraints)
                                                                                                          val constraints = List.map (fn (_, cts) => List.map doUnaryConstraint cts) tyvarsWithConstraints
                                                                                                      in PolyVarBind(span, vid, TypeScheme (ListPair.zip (tyvars, constraints), applySubstTy subst ty), #doExp (mapTy (ctx, subst, avoidCollision)) exp)
                                                                                                      end
        and doMatch(pat, exp) = (doPat pat, doExp exp)
        and doPat(pat as WildcardPat _) = pat
          | doPat(s as SConPat _) = s
          | doPat(VarPat(span, vid, ty)) = VarPat(span, vid, doTy ty)
          | doPat(RecordPat{sourceSpan, fields, wildcard}) = RecordPat{sourceSpan=sourceSpan, fields=Syntax.mapRecordRow doPat fields, wildcard=wildcard}
          | doPat(ConPat(span, ct, pat)) = ConPat(span, ct, Option.map doPat pat)
          | doPat(InstantiatedConPat(span, ct, pat, tyargs)) = InstantiatedConPat(span, ct, Option.map doPat pat, List.map doTy tyargs)
          | doPat(TypedPat(span, pat, ty)) = TypedPat(span, doPat pat, doTy ty)
          | doPat(LayeredPat(span, vid, ty, pat)) = LayeredPat(span, vid, doTy ty, doPat pat)
        and doTypBind(TypBind(span, tyvars, tycon, ty)) = let val (subst, tyvars) = genFreshTyVars(subst, tyvars)
                                                          in TypBind(span, tyvars, tycon, applySubstTy subst ty)
                                                          end
        and doDatBind(DatBind(span, tyvars, tycon, conbinds, eq)) = let val (subst, tyvars) = genFreshTyVars(subst, tyvars)
                                                                        fun doConBind(ConBind(span, vid, optTy)) = ConBind(span, vid, Option.map (applySubstTy subst) optTy)
                                                                    in DatBind(span, tyvars, tycon, List.map doConBind conbinds, eq)
                                                                    end
        and doExBind(ExBind(span, vid, optTy)) = ExBind(span, vid, Option.map doTy optTy)
    in { doExp = doExp
       , doDec = doDec
       , doDecs = List.map doDec
       , doValBind = doValBind
       , doUnaryConstraint = doUnaryConstraint
       }
    end

(* freeTyVarsInPat : TyVarSet * Pat -> TyVarSet *)
fun freeTyVarsInPat(bound, pat)
    = (case pat of
           WildcardPat _ => TyVarSet.empty
         | SConPat _ => TyVarSet.empty
         | VarPat(_, _, ty) => freeTyVarsInTy(bound, ty)
         | RecordPat{ fields = xs, ... } => List.foldl (fn ((_, pat), set) => TyVarSet.union(freeTyVarsInPat(bound, pat), set)) TyVarSet.empty xs
         | ConPat(_, _, NONE) => TyVarSet.empty
         | ConPat(_, _, SOME pat) => freeTyVarsInPat(bound, pat)
         | InstantiatedConPat(_, _, NONE, tyargs) => List.foldl (fn (ty, set) => TyVarSet.union(freeTyVarsInTy(bound, ty), set)) TyVarSet.empty tyargs
         | InstantiatedConPat(_, _, SOME pat, tyargs) => List.foldl (fn (ty, set) => TyVarSet.union(freeTyVarsInTy(bound, ty), set)) (freeTyVarsInPat(bound, pat)) tyargs
         | TypedPat(_, pat, ty) => TyVarSet.union(freeTyVarsInPat(bound, pat), freeTyVarsInTy(bound, ty))
         | LayeredPat(_, _, ty, pat) => TyVarSet.union(freeTyVarsInTy(bound, ty), freeTyVarsInPat(bound, pat))
      )

(* freeTyVarsInExp : TyVarSet * Exp -> TyVarSet *)
fun freeTyVarsInExp(bound, exp)
    = (case exp of
           SConExp _ => TyVarSet.empty
         | VarExp(_, _, _) => TyVarSet.empty
         | InstantiatedVarExp(_, _, _, tyargs) => List.foldl (fn ((ty,cts), set) => TyVarSet.union(freeTyVarsInTy(bound, ty), set)) TyVarSet.empty tyargs
         | RecordExp(_, xs) => List.foldl (fn ((_, exp), set) => TyVarSet.union(freeTyVarsInExp(bound, exp), set)) TyVarSet.empty xs
         | LetInExp(_, decls, exp) => TyVarSet.union(freeTyVarsInDecs(bound, decls), freeTyVarsInExp(bound, exp))
         | AppExp(_, exp1, exp2) => TyVarSet.union(freeTyVarsInExp(bound, exp1), freeTyVarsInExp(bound, exp2))
         | TypedExp(_, exp, ty) => TyVarSet.union(freeTyVarsInExp(bound, exp), freeTyVarsInTy(bound, ty))
         | HandleExp(_, exp, matches) => TyVarSet.union(freeTyVarsInExp(bound, exp), freeTyVarsInMatches(bound, matches, TyVarSet.empty))
         | RaiseExp(_, exp) => freeTyVarsInExp(bound, exp)
         | IfThenElseExp(_, exp1, exp2, exp3) => TyVarSet.union(freeTyVarsInExp(bound, exp1), TyVarSet.union(freeTyVarsInExp(bound, exp2), freeTyVarsInExp(bound, exp3)))
         | CaseExp(_, exp, ty, matches) => TyVarSet.union(freeTyVarsInExp(bound, exp), TyVarSet.union(freeTyVarsInTy(bound, ty), freeTyVarsInMatches(bound, matches, TyVarSet.empty)))
         | FnExp(_, vid, ty, body) => TyVarSet.union(freeTyVarsInTy(bound, ty), freeTyVarsInExp(bound, body))
         | ProjectionExp { recordTy = recordTy, fieldTy = fieldTy, ... } => TyVarSet.union(freeTyVarsInTy(bound, recordTy), freeTyVarsInTy(bound, fieldTy))
         | ListExp(_, xs, ty) => Vector.foldl (fn (x, set) => TyVarSet.union(freeTyVarsInExp(bound, x), set)) (freeTyVarsInTy(bound, ty)) xs
      )
and freeTyVarsInMatches(bound, nil, acc) = acc
  | freeTyVarsInMatches(bound, (pat, exp) :: rest, acc) = freeTyVarsInMatches(bound, rest, TyVarSet.union(acc, TyVarSet.union(freeTyVarsInPat(bound, pat), freeTyVarsInExp(bound, exp))))
and freeTyVarsInDecs(bound, decls) = List.foldl (fn (dec, set) => TyVarSet.union(set, freeTyVarsInDec(bound, dec))) TyVarSet.empty decls
and freeTyVarsInDec(bound, dec)
    = (case dec of
           ValDec(_, tyvarseq, valbinds) => freeTyVarsInValBinds(TyVarSet.addList(bound, tyvarseq), valbinds, TyVarSet.empty)
         | RecValDec(_, tyvarseq, valbinds) => freeTyVarsInValBinds(TyVarSet.addList(bound, tyvarseq), valbinds, TyVarSet.empty)
         | ValDec'(_, valbinds) => List.foldl (fn (valbind, acc) => TyVarSet.union(acc, freeTyVarsInValBind'(bound, valbind))) TyVarSet.empty valbinds
         | RecValDec'(_, valbinds) => List.foldl (fn (valbind, acc) => TyVarSet.union(acc, freeTyVarsInValBind'(bound, valbind))) TyVarSet.empty valbinds
         | TypeDec(_, typbinds) => List.foldl (fn (typbind, acc) => TyVarSet.union(acc, freeTyVarsInTypBind(bound, typbind))) TyVarSet.empty typbinds
         | DatatypeDec(_, datbinds) => List.foldl (fn (datbind, acc) => TyVarSet.union(acc, freeTyVarsInDatBind(bound, datbind))) TyVarSet.empty datbinds
         | ExceptionDec(_, exbinds) => List.foldl (fn (exbind, acc) => TyVarSet.union(acc, freeTyVarsInExBind(bound, exbind))) TyVarSet.empty exbinds
      )
and freeTyVarsInValBinds(bound, valbinds, acc) = List.foldl (fn (valbind, acc) => TyVarSet.union(acc, freeTyVarsInValBind(bound, valbind))) acc valbinds
and freeTyVarsInValBind(bound, PatBind(_, pat, exp)) = TyVarSet.union(freeTyVarsInPat(bound, pat), freeTyVarsInExp(bound, exp))
and freeTyVarsInValBind'(bound, TupleBind(_, xs, exp)) = List.foldl (fn ((_, ty), acc) => TyVarSet.union(acc, freeTyVarsInTy(bound, ty))) (freeTyVarsInExp(bound, exp)) xs
  | freeTyVarsInValBind'(bound, PolyVarBind(_, vid, TypeScheme(tyvars, ty), exp)) = let val bound' = TyVarSet.addList(bound, List.map #1 tyvars)
                                                                                    in TyVarSet.union(freeTyVarsInTy(bound', ty), freeTyVarsInExp(bound', exp))
                                                                                    end
and freeTyVarsInTypBind(bound, TypBind(_, tyvars, tycon, ty)) = freeTyVarsInTy(TyVarSet.addList(bound, tyvars), ty)
and freeTyVarsInDatBind(bound, DatBind(_, tyvars, tycon, conbinds, _)) = let val bound' = TyVarSet.addList(bound, tyvars)
                                                                         in List.foldl (fn (conbind, acc) => TyVarSet.union(acc, freeTyVarsInConBind(bound', conbind))) TyVarSet.empty conbinds
                                                                         end
and freeTyVarsInConBind(bound, ConBind(_, vid, NONE)) = TyVarSet.empty
  | freeTyVarsInConBind(bound, ConBind(_, vid, SOME ty)) = freeTyVarsInTy(bound, ty)
and freeTyVarsInExBind(bound, ExBind(_, vid, NONE)) = TyVarSet.empty
  | freeTyVarsInExBind(bound, ExBind(_, vid, SOME ty)) = freeTyVarsInTy(bound, ty)
and freeTyVarsInUnaryConstraint(bound, unaryConstraint)
    = (case unaryConstraint of
           HasField{fieldTy = fieldTy, ...} => freeTyVarsInTy(bound, fieldTy)
         | IsEqType _    => TyVarSet.empty
         | IsIntegral _   => TyVarSet.empty
         | IsSignedReal _ => TyVarSet.empty
         | IsRing _       => TyVarSet.empty
         | IsField _      => TyVarSet.empty
         | IsSigned _     => TyVarSet.empty
         | IsOrdered _    => TyVarSet.empty
      )

(* filterVarsInPat : (VId -> bool) -> Pat -> Pat *)
fun filterVarsInPat pred =
    let fun doPat pat = case pat of
                            WildcardPat _ => pat
                          | SConPat _ => pat
                          | VarPat(span, vid, ty) => if pred vid then pat else WildcardPat span
                          | RecordPat{sourceSpan, fields, wildcard} => RecordPat{ sourceSpan = sourceSpan, fields = Syntax.mapRecordRow doPat fields, wildcard = wildcard }
                          | ConPat(_, _, NONE) => pat
                          | ConPat(span, longvid, SOME innerPat) => ConPat(span, longvid, SOME (doPat innerPat))
                          | InstantiatedConPat(_, _, NONE, _) => pat
                          | InstantiatedConPat(span, longvid, SOME innerPat, tyargs) => InstantiatedConPat(span, longvid, SOME (doPat innerPat), tyargs)
                          | TypedPat(span, innerPat, ty) => TypedPat(span, doPat innerPat, ty)
                          | LayeredPat(span, vid, ty, innerPat) => if pred vid then LayeredPat(span, vid, ty, doPat innerPat) else TypedPat(span, doPat innerPat, ty)
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

(* renameVarsInPat : VId VIdMap.map -> Pat -> Pat *)
fun renameVarsInPat m =
    let fun doPat (pat as WildcardPat _) = pat
          | doPat (pat as SConPat _) = pat
          | doPat (pat as VarPat(span, vid, ty)) = (case VIdMap.find(m, vid) of
                                                        NONE => pat
                                                      | SOME repl => VarPat(span, repl, ty)
                                                   )
          | doPat (RecordPat { sourceSpan, fields, wildcard }) = RecordPat { sourceSpan = sourceSpan
                                                                           , fields = List.map (fn (label, pat) => (label, doPat pat)) fields
                                                                           , wildcard = wildcard
                                                                           }
          | doPat (ConPat(span, longvid, optPat)) = ConPat(span, longvid, Option.map doPat optPat)
          | doPat (InstantiatedConPat(span, longvid, optPat, tyargs)) = InstantiatedConPat(span, longvid, Option.map doPat optPat, tyargs)
          | doPat (TypedPat(span, pat, ty)) = TypedPat(span, doPat pat, ty)
          | doPat (LayeredPat(span, vid, ty, pat)) = LayeredPat(span, case VIdMap.find(m, vid) of
                                                                          NONE => vid
                                                                        | SOME repl => repl
                                                                , ty, doPat pat)
    in doPat
    end
end (* structure USyntax *)

structure ToTypedSyntax = struct

type ('a,'b) Context = { nextTyVar : int ref
                       , nextVId : int ref
                       , nextTyCon : int ref
                       , tyVarConstraints : 'a
                       , tyVarSubst : 'b
                       }

fun emitError(ctx : ('a,'b) Context, spans, message) = raise Syntax.SyntaxError (spans, message)

datatype BoundTyCon = BTyAlias of USyntax.TyVar list * USyntax.Ty
                    | BTyCon of { tyCon : USyntax.TyCon
                                , valConMap : (USyntax.VId * Syntax.IdStatus) Syntax.VIdMap.map
                                }

datatype Env' = MkEnv of Env
withtype Env = { valMap : (USyntax.VId * Syntax.IdStatus) Syntax.VIdMap.map
               , tyConMap : BoundTyCon Syntax.TyConMap.map
               , strMap : Env' Syntax.StrIdMap.map
               }

val emptyEnv = { valMap = Syntax.VIdMap.empty
               , tyConMap = Syntax.TyConMap.empty
               , strMap = Syntax.StrIdMap.empty
               }

fun mergeEnv(env1 : Env, env2 : Env)
    = { valMap = Syntax.VIdMap.unionWith #2 (#valMap env1, #valMap env2)
      , tyConMap = Syntax.TyConMap.unionWith #2 (#tyConMap env1, #tyConMap env2)
      , strMap = Syntax.StrIdMap.unionWith #2 (#strMap env1, #strMap env2)
      }

fun envWithValEnv valEnv = { valMap = valEnv
                           , tyConMap = Syntax.TyConMap.empty
                           , strMap = Syntax.StrIdMap.empty
                           }
fun envWithTyConEnv tyConEnv = { valMap = Syntax.VIdMap.empty
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

      fun lookupStr(ctx, env, span, nil) = env
        | lookupStr(ctx, env, span, (str0 as S.MkStrId name) :: str1)
          = case Syntax.StrIdMap.find(#strMap env, str0) of
                NONE => emitError(ctx, [span], "unknown structure name '" ^ name ^ "'")
              | SOME (MkEnv innerEnv) => lookupStr(ctx, innerEnv, span, str1)

      fun lookupTyCon(ctx, env, span, tycon as Syntax.MkTyCon name)
          = case Syntax.TyConMap.find(#tyConMap env, tycon) of
                NONE => emitError(ctx, [span], "unknown type constructor '" ^ name ^ "'")
              | SOME b => b
      fun lookupLongTyCon(ctx, env : Env, span, Syntax.MkQualified(strpath, tycon)) = lookupTyCon(ctx, lookupStr(ctx, env, span, strpath), span, tycon)

      fun lookupVId(env, vid) = Syntax.VIdMap.find(#valMap env, vid)
      fun lookupLongVId(ctx, env : Env, span, Syntax.MkQualified(strpath, vid))
          = (case lookupVId(lookupStr(ctx, env, span, strpath), vid) of
                 NONE => NONE
               | SOME (vid', x) => SOME (Syntax.MkQualified([], vid'), x)
            )
in
(* toUTy : Context * TVEnv * Env * Syntax.Ty -> USyntax.Ty *)
(* toUPat : Context * TVEnv * Env * Syntax.Pat -> USyntax.VId Syntax.VIdMap.map * USyntax.Pat *)
(* toUExp : Context * TVEnv * Env * Syntax.Exp -> USyntax.Exp *)
(* toUMatch : Context * TVEnv * Env * (Syntax.Pat * Syntax.Exp) list -> (USyntax.Pat * USyntax.Exp) list *)
(* toUDecs : Context * TVEnv * Env * Syntax.Dec list -> (* created environment *) Env * USyntax.Dec list *)
fun toUTy(ctx : ('a,'b) Context, tvenv : TVEnv, env : Env, S.TyVar(span, tv))
    = (case Syntax.TyVarMap.find(tvenv, tv) of
           NONE => emitError(ctx, [span], "unknown type variable `" ^ Syntax.print_TyVar tv ^ "`")
         | SOME tv => U.TyVar(span, tv)
      )
  | toUTy(ctx, tvenv, env, S.RecordType(span, row)) = U.RecordType(span, Syntax.mapRecordRow (fn ty => toUTy(ctx, tvenv, env, ty)) row)
  | toUTy(ctx, tvenv, env, S.TyCon(span, args, tycon))
    = (case lookupLongTyCon(ctx, env, span, tycon) of
           BTyCon { tyCon, ... } => U.TyCon(span, List.map (fn ty => toUTy(ctx, tvenv, env, ty)) args, tyCon)
         | BTyAlias (tyvars, ty) => let val subst = ListPair.foldlEq (fn (tv, arg, m) => USyntax.TyVarMap.insert (m, tv, toUTy(ctx, tvenv, env, arg))) USyntax.TyVarMap.empty (tyvars, args)
                                    in USyntax.applySubstTy subst ty
                                    end
      )
  | toUTy(ctx, tvenv, env, S.FnType(span, ty1, ty2)) = U.FnType(span, toUTy(ctx, tvenv, env, ty1), toUTy(ctx, tvenv, env, ty2))
fun toUPat(ctx : ('a,'b) Context, tvenv : TVEnv, env : Env, S.WildcardPat span) = (Syntax.VIdMap.empty, U.WildcardPat span) (* TODO: should generate a type id? *)
  | toUPat(ctx, tvenv, env, S.SConPat(span, Syntax.RealConstant _)) = emitError(ctx, [span], "no real constant may occur in a pattern")
  | toUPat(ctx, tvenv, env, S.SConPat(span, sc)) = (Syntax.VIdMap.empty, U.SConPat(span, sc))
  | toUPat(ctx, tvenv, env, S.ConOrVarPat(span, vid))
    = (case lookupVId(env, vid) of
           SOME (vid', Syntax.ValueConstructor) => (Syntax.VIdMap.empty, U.ConPat(span, USyntax.MkLongVId([], vid'), NONE))
         | SOME (vid', Syntax.ExceptionConstructor) => (Syntax.VIdMap.empty, U.ConPat(span, USyntax.MkLongVId([], vid'), NONE))
         | _ => let val vid' = newVId(ctx, vid)
                in (Syntax.VIdMap.insert(Syntax.VIdMap.empty, vid, vid'), U.VarPat(span, vid', USyntax.TyVar(span, freshTyVar(ctx))))
                end
      )
  | toUPat(ctx, tvenv, env, S.VarPat(span, vid)) = let val vid' = newVId(ctx, vid)
                                                   in (Syntax.VIdMap.insert(Syntax.VIdMap.empty, vid, vid'), U.VarPat(span, vid', USyntax.TyVar(span, freshTyVar(ctx)))) (* add extra type annotation *)
                                                   end
  | toUPat(ctx, tvenv, env, S.RecordPat{sourceSpan, fields = row, wildcard})
    = let val (vidmap, row') = List.foldr (fn ((label, pat), (vidmap, row')) => let val (vidmap', pat') = toUPat(ctx, tvenv, env, pat)
                                                                                    (* TODO: error if vid conflict *)
                                                                                in (Syntax.VIdMap.unionWith #2 (vidmap, vidmap'), (label, pat') :: row')
                                                                                end
                                          ) (Syntax.VIdMap.empty, []) row
      in (vidmap, U.RecordPat{sourceSpan=sourceSpan, fields=row', wildcard=wildcard})
      end
  | toUPat(ctx, tvenv, env, S.ConPat(span, longvid, NONE))
    = (case lookupLongVId(ctx, env, span, longvid) of
           SOME (longvid', _) => (Syntax.VIdMap.empty, U.ConPat(span, longvid', NONE))
         | NONE => emitError(ctx, [span], "unbound identifier")
      )
  | toUPat(ctx, tvenv, env, S.ConPat(span, longvid, SOME pat))
    = let val (vidmap, pat') = toUPat(ctx, tvenv, env, pat)
      in case lookupLongVId(ctx, env, span, longvid) of
             SOME (longvid', _) => (vidmap, U.ConPat(span, longvid', SOME pat'))
           | NONE => emitError(ctx, [span], "unbound identifier")
      end
  | toUPat(ctx, tvenv, env, S.TypedPat(span1, S.ConOrVarPat(span2, vid), ty))
    = (case lookupVId(env, vid) of
           SOME (vid', Syntax.ValueConstructor) => (Syntax.VIdMap.empty, U.TypedPat(span1, U.ConPat(span2, USyntax.MkLongVId([], vid'), NONE), toUTy(ctx, tvenv, env, ty)))
         | SOME (vid', Syntax.ExceptionConstructor) => (Syntax.VIdMap.empty, U.TypedPat(span1, U.ConPat(span2, USyntax.MkLongVId([], vid'), NONE), toUTy(ctx, tvenv, env, ty)))
         | _ => let val vid' = newVId(ctx, vid)
                in (Syntax.VIdMap.insert(Syntax.VIdMap.empty, vid, vid'), U.VarPat(span1, vid', toUTy(ctx, tvenv, env, ty)))
                end
      )
  | toUPat(ctx, tvenv, env, S.TypedPat(span1, S.VarPat(span2, vid), ty))
    = let val vid' = newVId(ctx, vid)
      in (Syntax.VIdMap.insert(Syntax.VIdMap.empty, vid, vid'), U.VarPat(span1, vid', toUTy(ctx, tvenv, env, ty)))
      end
  | toUPat(ctx, tvenv, env, S.TypedPat(span, pat, ty))
    = let val (vidmap, pat') = toUPat(ctx, tvenv, env, pat)
      in (vidmap, U.TypedPat(span, pat', toUTy(ctx, tvenv, env, ty)))
      end
  | toUPat(ctx, tvenv, env, S.LayeredPat(span, vid, SOME ty, pat))
    = let val vid' = newVId(ctx, vid)
          val (vidmap, pat') = toUPat(ctx, tvenv, env, pat)
      in (Syntax.VIdMap.insert(vidmap, vid, vid'), U.LayeredPat(span, vid', toUTy(ctx, tvenv, env, ty), pat'))
      end
  | toUPat(ctx, tvenv, env, S.LayeredPat(span, vid, NONE, pat))
    = let val vid' = newVId(ctx, vid)
          val (vidmap, pat' ) = toUPat(ctx, tvenv, env, pat)
      in (Syntax.VIdMap.insert(vidmap, vid, vid'), U.LayeredPat(span, vid', USyntax.TyVar(span, freshTyVar(ctx)), pat'))
      end
fun toUExp(ctx : ('a,'b) Context, tvenv : TVEnv, env : Env, S.SConExp(span, scon)) = U.SConExp(span, scon)
  | toUExp(ctx, tvenv, env, S.VarExp(span, longvid))
    = (case lookupLongVId(ctx, env, span, longvid) of
           SOME (longvid', idstatus) => U.VarExp(span, longvid', idstatus)
         | NONE => emitError(ctx, [span], "unbound identifier: " ^ Syntax.print_LongVId longvid)
      )
  | toUExp(ctx, tvenv, env, S.RecordExp(span, row)) = U.RecordExp(span, Syntax.mapRecordRow (fn exp => toUExp(ctx, tvenv, env, exp)) row)
  | toUExp(ctx, tvenv, env, S.LetInExp(span, decls, exp))
    = let val (env', decls') = toUDecs(ctx, tvenv, env, decls)
      in U.LetInExp(span, decls', toUExp(ctx, tvenv, mergeEnv(env, env'), exp))
      end
  | toUExp(ctx, tvenv, env, S.AppExp(span, exp1, exp2)) = U.AppExp(span, toUExp(ctx, tvenv, env, exp1), toUExp(ctx, tvenv, env, exp2))
  | toUExp(ctx, tvenv, env, S.TypedExp(span, exp, ty)) = U.TypedExp(span, toUExp(ctx, tvenv, env, exp), toUTy(ctx, tvenv, env, ty))
  | toUExp(ctx, tvenv, env, S.HandleExp(span, exp, match)) = U.HandleExp(span, toUExp(ctx, tvenv, env, exp), toUMatch(ctx, tvenv, env, match))
  | toUExp(ctx, tvenv, env, S.RaiseExp(span, exp)) = U.RaiseExp(span, toUExp(ctx, tvenv, env, exp))
  | toUExp(ctx, tvenv, env, S.IfThenElseExp(span, exp1, exp2, exp3)) = U.IfThenElseExp(span, toUExp(ctx, tvenv, env, exp1), toUExp(ctx, tvenv, env, exp2), toUExp(ctx, tvenv, env, exp3))
  | toUExp(ctx, tvenv, env, S.WhileDoExp(span, exp1, exp2))
    = let val fnName = newVId(ctx, S.MkVId "loop")
          val fnCall = U.AppExp(span, U.VarExp(span, Syntax.MkQualified([], fnName), Syntax.ValueVariable), U.RecordExp(span, [])) (* loop () *)
          val unitTy = U.RecordType(span, [])
      in U.LetInExp(span,
                    [U.RecValDec(span,
                                 [(* TODO: tyvar *)],
                                 [U.PatBind(span,
                                            U.VarPat(span,
                                                     fnName,
                                                     U.FnType(span, unitTy, unitTy)
                                                    ),
                                            U.FnExp(span,
                                                    newVId(ctx, S.MkVId "a"),
                                                    unitTy,
                                                    U.IfThenElseExp(span,
                                                                    toUExp(ctx, tvenv, env, exp1),
                                                                    U.LetInExp(span,
                                                                               [U.ValDec(span,
                                                                                         [(* TODO: tyvar *)],
                                                                                         [U.PatBind(span, U.WildcardPat span, toUExp(ctx, tvenv, env, exp2))]
                                                                                        )
                                                                               ],
                                                                               fnCall
                                                                              ),
                                                                    U.RecordExp(span, [])
                                                                   )
                                                   )
                                           )
                                 ]
                                )
                    ],
                    fnCall
                   )
      end
  | toUExp(ctx, tvenv, env, S.CaseExp(span, exp, match)) = U.CaseExp(span, toUExp(ctx, tvenv, env, exp), USyntax.TyVar(span, freshTyVar(ctx)), toUMatch(ctx, tvenv, env, match))
  | toUExp(ctx, tvenv, env, S.FnExp(span, [(pat, body)]))
    = let fun determineConOrVar(span', vid)
              = (case lookupVId(env, vid) of
                     SOME (vid', Syntax.ValueConstructor) => S.ConPat(span', S.MkLongVId([], vid), NONE)
                   | SOME (vid', Syntax.ExceptionConstructor) => S.ConPat(span', S.MkLongVId([], vid), NONE)
                   | _ => S.VarPat(span', vid)
                )
          val pat' = case pat of
                         S.ConOrVarPat(span, vid) => determineConOrVar(span, vid)
                       | S.TypedPat(span1, S.ConOrVarPat(span2, vid), ty) => S.TypedPat(span1, determineConOrVar(span2, vid), ty)
                       | _ => pat
      in case pat' of
             S.VarPat(span2, vid) => let val vid' = newVId(ctx, vid)
                                         val valEnv = Syntax.VIdMap.insert(Syntax.VIdMap.empty, vid, (vid', Syntax.ValueVariable))
                                         val env' = mergeEnv(env, envWithValEnv valEnv)
                                     in U.FnExp(span, vid', USyntax.TyVar(span2, freshTyVar(ctx)), toUExp(ctx, tvenv, env', body))
                                     end
           | S.TypedPat(_, S.VarPat(_, vid), ty) => let val vid' = newVId(ctx, vid)
                                                        val valEnv = Syntax.VIdMap.insert(Syntax.VIdMap.empty, vid, (vid', Syntax.ValueVariable))
                                                        val env' = mergeEnv(env, envWithValEnv valEnv)
                                                    in U.FnExp(span, vid', toUTy(ctx, tvenv, env, ty), toUExp(ctx, tvenv, env', body))
                                                    end
           | _ => let val vid' = newVId(ctx, Syntax.MkVId("a"))
                      val ty = USyntax.TyVar(span, freshTyVar(ctx))
                  in U.FnExp(span, vid', ty, U.CaseExp(span, U.VarExp(span, U.MkLongVId([], vid'), Syntax.ValueVariable), ty, toUMatch(ctx, tvenv, env, [(pat', body)])))
                  end
      end
  | toUExp(ctx, tvenv, env, S.FnExp(span, match))
    = let val vid' = newVId(ctx, Syntax.MkVId("a"))
          val ty = USyntax.TyVar(span, freshTyVar(ctx))
      in U.FnExp(span, vid', ty, U.CaseExp(span, U.VarExp(span, U.MkLongVId([], vid'), Syntax.ValueVariable), ty, toUMatch(ctx, tvenv, env, match)))
      end
  | toUExp(ctx, tvenv, env, S.ProjectionExp(span, label)) = U.ProjectionExp { sourceSpan = span, label = label, recordTy = USyntax.TyVar(span, freshTyVar(ctx)), fieldTy = USyntax.TyVar(span, freshTyVar(ctx)) }
  | toUExp(ctx, tvenv, env, S.ListExp(span, xs)) = U.ListExp(span, Vector.map (fn x => toUExp(ctx, tvenv, env, x)) xs, USyntax.TyVar(span, freshTyVar(ctx)))
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
           S.ValDec(span, tyvars, valbind) =>
           let val tyvars' = List.map (fn tv => (tv, genTyVar(ctx, tv))) tyvars
               val tvenv' = List.foldl Syntax.TyVarMap.insert' tvenv tyvars'
               val valbind' = List.map (fn S.PatBind(span, pat, exp) => let val (vidmap, pat') = toUPat(ctx, tvenv', env, pat)
                                                                        in (vidmap, U.PatBind(span, pat', toUExp(ctx, tvenv', env, exp)))
                                                                        end
                                       ) valbind
               val decl' = U.ValDec(span, List.map #2 tyvars', List.map #2 valbind')
               val vidmap = List.foldl (Syntax.VIdMap.unionWith #2) Syntax.VIdMap.empty (List.map #1 valbind')
               val venv = envWithValEnv (Syntax.VIdMap.map (fn vid => (vid, Syntax.ValueVariable)) vidmap)
               val (env', decls') = toUDecs(ctx, tvenv, mergeEnv(env, venv), decls)
           in (mergeEnv(venv, env'), decl' :: decls')
           end
         | S.RecValDec(span, tyvars, valbind) =>
           let val tyvars' = List.map (fn tv => (tv, genTyVar(ctx, tv))) tyvars
               val tvenv' = List.foldl Syntax.TyVarMap.insert' tvenv tyvars'
               val valbind' = List.map (fn S.PatBind(span, pat, exp) => (span, toUPat(ctx, tvenv', env, pat), exp)) valbind
               val vidmap = List.foldl (Syntax.VIdMap.unionWith #2) Syntax.VIdMap.empty (List.map (fn x => #1 (#2 x)) valbind')
               val venv = envWithValEnv (Syntax.VIdMap.map (fn vid => (vid, Syntax.ValueVariable)) vidmap)
               val env' = mergeEnv(env, venv)
               val valbind'' = List.map (fn (span, (_, pat'), exp) => U.PatBind(span, pat', toUExp(ctx, tvenv', env', exp))) valbind'
               val decl' = U.RecValDec(span, List.map #2 tyvars', valbind'')
               val (env'', decls') = toUDecs(ctx, tvenv, env', decls)
           in (mergeEnv(venv, env''), decl' :: decls')
           end
         | S.FunDec(span, tyvars, fvalbind) =>
           let val tyvars' = List.map (fn tv => (tv, genTyVar(ctx, tv))) tyvars
               val tvenv' = List.foldl Syntax.TyVarMap.insert' tvenv tyvars'
               (* TODO: Check id status *)
               val vidmap = List.foldl Syntax.VIdMap.insert' Syntax.VIdMap.empty (List.map (fn S.FValBind { vid = vid, ... } => (vid, newVId(ctx, vid))) fvalbind)
               val venv = envWithValEnv (Syntax.VIdMap.map (fn vid => (vid, Syntax.ValueVariable)) vidmap)
               val env' = mergeEnv(env, venv)
               fun doFValBind (S.FValBind { sourceSpan, vid, arity, rules })
                   = let fun buildExp(0, revParams) = let val params = List.rev revParams
                                                          val paramTuple = U.TupleExp (sourceSpan, List.map (fn (vid, _) => U.VarExp(sourceSpan, Syntax.MkQualified([], vid), Syntax.ValueVariable)) params)
                                                          val paramTupleTy = U.TupleType (sourceSpan, List.map #2 params)
                                                          fun doRule (pats, optTy, exp) = let val (vidmap', pat) = toUPat(ctx, tvenv', env', S.TuplePat(SourcePos.nullSpan (* TODO *), pats))
                                                                                              val venv' = envWithValEnv (Syntax.VIdMap.map (fn vid => (vid, Syntax.ValueVariable)) vidmap')
                                                                                          in (pat, toUExp(ctx, tvenv', mergeEnv(env', venv'), case optTy of
                                                                                                                                                  NONE => exp 
                                                                                                                                                | SOME expTy => S.TypedExp(Syntax.getSourceSpanOfExp exp, exp, expTy)))
                                                                                          end
                                                      in U.CaseExp(sourceSpan, paramTuple, paramTupleTy, List.map doRule rules)
                                                      end
                           | buildExp(n, revParams) = let val paramId = newVId(ctx, Syntax.MkVId "a")
                                                          val paramTy = USyntax.TyVar(sourceSpan, freshTyVar(ctx))
                                                      in U.FnExp(sourceSpan, paramId, paramTy, buildExp(n - 1, (paramId, paramTy) :: revParams))
                                                      end
                         val vid' = case Syntax.VIdMap.find(vidmap, vid) of
                                        SOME v => v
                                      | NONE => emitError(ctx, [sourceSpan], "internal error")
                     in U.PatBind(sourceSpan, U.VarPat(sourceSpan, vid', USyntax.TyVar(sourceSpan, freshTyVar(ctx))), buildExp(arity, []))
                     end
               val valbind'' = List.map doFValBind fvalbind
               val decl' = U.RecValDec(span, List.map #2 tyvars', valbind'')
               val (env'', decls') = toUDecs(ctx, tvenv, env', decls)
           in (mergeEnv(venv, env''), decl' :: decls')
           end
         | S.TypeDec(span, typbinds) =>
           let fun doTypBind (S.TypBind(span, tyvars, tycon, ty), (tyConEnv, typbinds))
                   = let val tyvars' = List.map (fn tv => (tv, genTyVar(ctx, tv))) tyvars
                         val tvenv = List.foldl Syntax.TyVarMap.insert' tvenv tyvars'
                         val tyvars'' = List.map #2 tyvars'
                         val ty' = toUTy(ctx, tvenv, env, ty)
                         val tycon' = newTyCon(ctx, tycon)
                     in (Syntax.TyConMap.insert(tyConEnv, tycon, BTyAlias(tyvars'', ty')), U.TypBind(span, tyvars'', tycon', ty') :: typbinds)
                     end
               val (tyConEnv, typbinds') = List.foldr doTypBind (Syntax.TyConMap.empty, []) typbinds
               val tenv = envWithTyConEnv tyConEnv
               val (env', decls') = toUDecs(ctx, tvenv, mergeEnv(env, tenv), decls)
           in (mergeEnv(tenv, env'), U.TypeDec(span, typbinds') :: decls')
           end
         | S.DatatypeDec(span, datbinds) =>
           let fun doDatBind1 (S.DatBind(span, tyvars, tycon, conbinds), (tyConEnv, datbinds))
                   = let val tyvars' = List.map (fn tv => (tv, genTyVar(ctx, tv))) tyvars
                         val tvenv = List.foldl Syntax.TyVarMap.insert' tvenv tyvars'
                         val tycon' = newTyCon(ctx, tycon)
                         fun doConBind (conbind as S.ConBind(span, vid, _), (valConMap, conbinds))
                             = let val vid' = newVId(ctx, vid)
                               in (Syntax.VIdMap.insert(valConMap, vid, (vid', Syntax.ValueConstructor)), (conbind, vid') :: conbinds)
                               end
                         val (valConMap, conbinds') = List.foldl doConBind (Syntax.VIdMap.empty, []) conbinds
                     in (Syntax.TyConMap.insert(tyConEnv, tycon, { tyCon = tycon', valConMap = valConMap }), (span, List.map #2 tyvars', tvenv, tycon', List.rev conbinds', valConMap) :: datbinds)
                     end
               val (tyConEnv, datbinds') = List.foldr doDatBind1 (Syntax.TyConMap.empty, []) datbinds
               val tyConEnv' = envWithTyConEnv(Syntax.TyConMap.map BTyCon tyConEnv)
               val env' = mergeEnv(env, tyConEnv')
               fun doDatBind2 ((span, tyvars, tvenv, tycon, conbinds, valConMap), (valEnv, datbinds))
                   = let fun doConBind (S.ConBind(span, vid, optPayloadTy), vid')
                             = let val optPayloadTy' = case optPayloadTy of
                                                           NONE => NONE
                                                         | SOME payloadTy => SOME (toUTy(ctx, tvenv, env', payloadTy))
                               in U.ConBind(span, vid', optPayloadTy')
                               end
                         val conbinds' = List.map doConBind conbinds
                     in (Syntax.VIdMap.unionWith #2 (valConMap, valEnv), USyntax.DatBind(span, tyvars, tycon, conbinds', false) :: datbinds)
                     end
               val (valEnv, datbinds'') = List.foldr doDatBind2 (Syntax.VIdMap.empty, []) datbinds'
               val datbindEnv = mergeEnv(tyConEnv', envWithValEnv valEnv)
               val (env', decls') = toUDecs(ctx, tvenv, mergeEnv(env, datbindEnv), decls)
           in (mergeEnv(datbindEnv, env'), U.DatatypeDec(span, datbinds'') :: decls')
           end
         | S.DatatypeRepDec(span, tycon, longtycon) =>
           let val btycon = lookupLongTyCon(ctx, env, span, longtycon)
               val valConMap = case btycon of
                                   BTyAlias _ => Syntax.VIdMap.empty (* TODO: emit warning? *)
                                 | BTyCon { valConMap, ... } => valConMap
               val replicatedEnv = { valMap = valConMap
                                   , tyConMap = Syntax.TyConMap.insert (Syntax.TyConMap.empty, tycon, btycon)
                                   , strMap = Syntax.StrIdMap.empty
                                   }
               val (env', decls') = toUDecs(ctx, tvenv, mergeEnv(env, replicatedEnv), decls)
           in (mergeEnv(replicatedEnv, env'), decls')
           end
         | S.AbstypeDec(span, datbinds, dec) =>
           let val decl' = emitError(ctx, [span], "abstype: not implemented yet")
               val (env', decls') = toUDecs(ctx, tvenv, (* TODO: add type ctor *) env, decls)
           in ((* TODO: add type ctor *) env', decl' :: decls')
           end
         | S.ExceptionDec(span, exbinds) =>
           let fun doExBind(S.ExBind(span, vid, optTy), (exconmap, exbinds')) =
                   if Syntax.VIdMap.inDomain(exconmap, vid) then
                       emitError(ctx, [span], "exception: the same identifier is bound twice")
                   else
                       let val vid' = newVId(ctx, vid)
                           val optTy' = case optTy of
                                            NONE => NONE
                                          | SOME ty => SOME (toUTy(ctx, tvenv, env, ty))
                       in (Syntax.VIdMap.insert(exconmap, vid, (vid', Syntax.ExceptionConstructor)), U.ExBind(span, vid', optTy') :: exbinds')
                       end
                 | doExBind(S.ExReplication(span, vid, longvid), (exconmap, exbinds')) =
                   if Syntax.VIdMap.inDomain(exconmap, vid) then
                       emitError(ctx, [span], "exception: the same identifier is bound twice")
                   else
                       (case lookupLongVId(ctx, env, span, longvid) of
                            NONE => emitError(ctx, [span], "exception constructor not found")
                          | SOME (Syntax.MkQualified(_, vid'), idstatus) =>
                            if idstatus = Syntax.ExceptionConstructor then
                                (Syntax.VIdMap.insert(exconmap, vid, (vid', idstatus)), exbinds')
                            else
                                emitError(ctx, [span], "exception: RHS is not an exception constructor")
                       )
               val (exconmap, exbinds') = List.foldr doExBind (Syntax.VIdMap.empty, []) exbinds
               val exenv = { valMap = exconmap, tyConMap = Syntax.TyConMap.empty, strMap = Syntax.StrIdMap.empty }
               val (env', decls') = toUDecs(ctx, tvenv, mergeEnv(env, exenv), decls)
           in (mergeEnv(exenv, env'), if List.null exbinds' then
                                          decls'
                                      else
                                          U.ExceptionDec(span, exbinds') :: decls')
           end
         | S.LocalDec(span, decls1, decls2) =>
           let val (env1, decls1') = toUDecs(ctx, tvenv, env, decls1)
               val (env2, decls2') = toUDecs(ctx, tvenv, mergeEnv(env, env1), decls2)
               val (env', decls') = toUDecs(ctx, tvenv, mergeEnv(env, env2), decls)
           in (mergeEnv(env2, env'), decls1' @ decls2' @ decls') end
         | S.OpenDec(span, longstrids) =>
           let val env' = List.foldl (fn (Syntax.MkQualified (strids, strid as Syntax.MkStrId name), accEnv) =>
                                         case Syntax.StrIdMap.find(#strMap (lookupStr(ctx, env, span, strids)), strid) of
                                             NONE => emitError(ctx, [span], "unknown structure name '" ^ name ^ "'")
                                           | SOME (MkEnv strEnv) => mergeEnv(accEnv, strEnv))
                                     emptyEnv
                                     longstrids
               val (env'', decls') = toUDecs(ctx, tvenv, mergeEnv(env, env'), decls)
           in (mergeEnv(env', env''), decls')
           end
         | S.FixityDec(_) => toUDecs(ctx, tvenv, env, decls) (* ignore *)
      )
(* doUStrExp : Context * Env * Syntax.Dec Syntax.StrExp -> (* structure *) Env * USyntax.Dec list *)
(* doUStrDecs : Context * Env * (Syntax.Dec Syntax.StrDec) list -> (* created environment *) Env * USyntax.Dec list *)
fun doUStrExp(ctx, env, Syntax.StructExp(span, strdecs)) = let val (env', decs') = doUStrDecs(ctx, env, strdecs)
                                                           in (env', decs')
                                                           end
  | doUStrExp(ctx, env, Syntax.StrIdExp(span, Syntax.MkQualified(strids, strid as Syntax.MkStrId name)))
    = let val env = lookupStr(ctx, env, span, strids)
      in case Syntax.StrIdMap.find(#strMap env, strid) of
             NONE => emitError(ctx, [span], "unknown structure name '" ^ name ^ "'") (* TODO: print full name *)
           | SOME (MkEnv innerEnv) => (innerEnv, [])
      end
  | doUStrExp(ctx, env, Syntax.LetInStrExp(span, strdecs, strexp)) = let val (env', strdecs') = doUStrDecs(ctx, env, strdecs)
                                                                         val (env'', decs') = doUStrExp(ctx, mergeEnv(env, env'), strexp)
                                                                     in (env'', strdecs' @ decs')
                                                                     end
and doUStrDecs(ctx, env, [] : (Syntax.Dec Syntax.StrDec) list) : Env * USyntax.Dec list = (emptyEnv, [])
  | doUStrDecs(ctx, env, Syntax.CoreDec(span, dec) :: strdecs) = let val (env', dec') = toUDecs(ctx, Syntax.TyVarMap.empty, env, [dec])
                                                                     val (env'', strdecs') = doUStrDecs(ctx, mergeEnv(env, env'), strdecs)
                                                                 in (mergeEnv(env', env''), dec' @ strdecs')
                                                                 end
  | doUStrDecs(ctx, env, Syntax.StrBindDec(span, strbinds) :: strdecs) = let val (strMap, decs) = List.foldl (doStrBind (ctx, env)) (Syntax.StrIdMap.empty, []) strbinds
                                                                             val env' = { valMap = #valMap emptyEnv
                                                                                        , tyConMap = #tyConMap emptyEnv
                                                                                        , strMap = strMap
                                                                                        }
                                                                             val (env'', strdecs') = doUStrDecs(ctx, mergeEnv(env, env'), strdecs)
                                                                         in (mergeEnv(env', env''), decs @ strdecs')
                                                                         end
  | doUStrDecs(ctx, env, Syntax.LocalStrDec(span, strdecs1, strdecs2) :: strdecs3) = let val (env', strdecs1') = doUStrDecs(ctx, env, strdecs1)
                                                                                         val (env'', strdecs2') = doUStrDecs(ctx, mergeEnv(env, env'), strdecs2)
                                                                                         val (env''', strdecs3') = doUStrDecs(ctx, mergeEnv(env, env''), strdecs3)
                                                                                     in (mergeEnv(env'', env'''), strdecs1' @ strdecs2' @ strdecs3')
                                                                                     end
and doStrBind (ctx, env) ((strid, strexp), (acc, decs0)) = let val (strenv, decs) = doUStrExp(ctx, env, strexp)
                                                           in (Syntax.StrIdMap.insert (acc, strid, MkEnv strenv), decs0 @ decs)
                                                           end
(* toUTopDec : Context * Env * Syntax.Dec Syntax.StrDec -> Env * USyntax.TopDec *)
fun toUTopDec(ctx, env, strdec : Syntax.Dec Syntax.StrDec) = let val (env', decs) = doUStrDecs(ctx, env, [strdec])
                                                             in (env', USyntax.StrDec decs)
                                                             end
(* toUProgram : Context * Env * (Syntax.Dec Syntax.StrDec) list -> Env * USyntax.TopDec list *)
fun toUProgram(ctx, env, [] : (Syntax.Dec Syntax.StrDec) list) : Env * USyntax.TopDec list = (emptyEnv, [])
  | toUProgram(ctx, env, strdec :: strdecs) = let val (env', topdec) = toUTopDec(ctx, env, strdec)
                                                  val (env'', topdecs) = toUProgram(ctx, mergeEnv(env, env'), strdecs)
                                              in (mergeEnv(env', env''), topdec :: topdecs)
                                              end
end (* local *)
end (* structure ToTypedSyntax *)
