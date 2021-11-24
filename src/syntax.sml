(*
 * Copyright (c) 2021 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure Syntax = struct
datatype SCon = IntegerConstant of int (* decimal / hexadecimal *)
              | WordConstant of word (* decimal / hexadecimal *)
              | RealConstant of string (* decimal *)
              | StringConstant of string
              | CharacterConstant of string
datatype VId = MkVId of string
             | GeneratedVId of string * int
datatype TyVar = MkTyVar of string
datatype TyCon = MkTyCon of string
datatype Label = NumericLabel of int
               | IdentifierLabel of string
datatype StrId = MkStrId of string
datatype SigId = MkSigId of string
datatype FunId = MkFunId of string
datatype 'a Qualified = MkQualified of StrId list * 'a
type LongVId = VId Qualified
type LongTyCon = TyCon Qualified
type LongStrId = StrId Qualified
fun MkLongVId(strids, vid: VId) = MkQualified(strids, vid)
fun MkLongTyCon(strids, tycon: TyCon) = MkQualified(strids, tycon)
fun MkLongStrId(strids, strid: StrId) = MkQualified(strids, strid)

fun getVIdName(MkVId name) = name
  | getVIdName(GeneratedVId (name, i)) = name ^ "@" ^ Int.toString i

datatype InfixAssociativity = LeftAssoc of int
                            | RightAssoc of int
datatype FixityStatus = Nonfix
                      | Infix of InfixAssociativity

datatype IdStatus = ValueVariable
                  | ValueConstructor of bool (* is it the sole constructor? *)
                  | ExceptionConstructor
fun isValueConstructor ValueVariable = false
  | isValueConstructor (ValueConstructor _) = true
  | isValueConstructor ExceptionConstructor = false

(* RedBlackMapFn, RedBlackSetFn: from smlnj-lib *)
structure VIdKey = struct
type ord_key = VId
fun compare (MkVId x, MkVId y) = String.compare (x,y)
  | compare (MkVId _, GeneratedVId _) = LESS
  | compare (GeneratedVId _, MkVId _) = GREATER
  | compare (GeneratedVId (x, i), GeneratedVId (y, j)) = (case Int.compare (i, j) of
                                                              EQUAL => String.compare (x, y)
                                                            | t => t
                                                         )
end : ORD_KEY
structure VIdSet = RedBlackSetFn(VIdKey)
structure VIdMap = RedBlackMapFn(VIdKey)
structure StrIdKey = struct
type ord_key = StrId
fun compare (MkStrId x, MkStrId y) = String.compare (x,y)
end : ORD_KEY
structure StrIdSet = RedBlackSetFn(StrIdKey)
structure StrIdMap = RedBlackMapFn(StrIdKey)
structure SigIdKey = struct
type ord_key = SigId
fun compare (MkSigId x, MkSigId y) = String.compare (x,y)
end
structure SigIdSet = RedBlackSetFn(SigIdKey)
structure SigIdMap = RedBlackMapFn(SigIdKey)
structure FunIdKey = struct
type ord_key = FunId
fun compare (MkFunId x, MkFunId y) = String.compare (x,y)
end
structure FunIdSet = RedBlackSetFn(FunIdKey)
structure FunIdMap = RedBlackMapFn(FunIdKey)
structure TyVarKey = struct
type ord_key = TyVar
fun compare (MkTyVar x, MkTyVar y) = String.compare (x,y)
end : ORD_KEY
structure TyVarMap = RedBlackMapFn(TyVarKey)
structure TyVarSet = RedBlackSetFn(TyVarKey)
structure TyVarSet = struct
(* compatibility with older smlnj-lib *)
open TyVarSet
val toList = foldr (op ::) []
fun disjoint (x, y) = isEmpty (intersection (x, y))
open TyVarSet
end
structure LabelKey = struct
type ord_key = Label
(* NumericLabel _ < IdentifierLabel _ *)
fun compare (NumericLabel x, NumericLabel y) = Int.compare (x,y)
  | compare (NumericLabel _, IdentifierLabel _) = LESS
  | compare (IdentifierLabel _, NumericLabel _) = GREATER
  | compare (IdentifierLabel x, IdentifierLabel y) = String.compare (x,y)
end
structure LabelSet = RedBlackSetFn(LabelKey)
structure LabelMap = RedBlackMapFn(LabelKey)
fun LabelMapFromList (xs : (Label * 'a) list) : 'a LabelMap.map = List.foldl LabelMap.insert' LabelMap.empty xs

structure TyConKey = struct
type ord_key = TyCon
fun compare (MkTyCon x, MkTyCon y) = String.compare (x, y)
end : ORD_KEY
structure TyConSet = RedBlackSetFn(TyConKey)
structure TyConMap = RedBlackMapFn(TyConKey)

structure LongTyCon : sig
              type t
              type ord_key = t
              val compare : t * t -> order
              val min : t * t -> t
          end = struct
type t = LongTyCon
type ord_key = t
fun compare (MkQualified ([], tycon), MkQualified ([], tycon')) = TyConKey.compare (tycon, tycon')
  | compare (MkQualified ([], _), MkQualified (_ :: _, _)) = LESS
  | compare (MkQualified (_ :: _, _), MkQualified ([], _)) = GREATER
  | compare (MkQualified (s :: ss, tycon), MkQualified (s' :: ss', tycon')) = case StrIdKey.compare (s, s') of
                                                                                  EQUAL => compare (MkQualified (ss, tycon), MkQualified (ss', tycon'))
                                                                                | x => x
fun min (x, y) = case compare (x, y) of
                     LESS => x
                   | EQUAL => x
                   | GREATER => y
end
structure LongTyConSet = RedBlackSetFn(LongTyCon)

datatype Ty = TyVar of SourcePos.span * TyVar (* type variable *)
            | RecordType of SourcePos.span * (Label * Ty) list (* record type expression *)
            | TyCon of SourcePos.span * Ty list * LongTyCon (* type construction *)
            | FnType of SourcePos.span * Ty * Ty (* function type expression *)

datatype Pat = WildcardPat of SourcePos.span
             | SConPat of SourcePos.span * SCon (* special constant *)
             | VarPat of SourcePos.span * VId (* variable *)
             | RecordPat of { sourceSpan : SourcePos.span
                            , fields : (Label * Pat) list
                            , wildcard : bool
                            }
             | ConPat of SourcePos.span * LongVId * Pat option (* constructed pattern *)
             | TypedPat of SourcePos.span * Pat * Ty (* typed *)
             | LayeredPat of SourcePos.span * VId * Ty option * Pat (* layered *)
             | VectorPat of SourcePos.span * Pat vector * bool (* [extension] vector pattern *)

datatype TypBind = TypBind of SourcePos.span * TyVar list * TyCon * Ty
datatype ConBind = ConBind of SourcePos.span * VId * Ty option
datatype DatBind = DatBind of SourcePos.span * TyVar list * TyCon * ConBind list
datatype ExBind = ExBind of SourcePos.span * VId * Ty option (* <op> vid <of ty> *)
                | ExReplication of SourcePos.span * VId * LongVId (* <op> vid = <op> longvid *)

datatype PrimOp = PrimOp_call2 (* # of type arguments: 3 (optional), # of arguments: 3 *)

datatype Exp = SConExp of SourcePos.span * SCon (* special constant *)
             | VarExp of SourcePos.span * LongVId (* value identifier, with or without 'op'  *)
             | RecordExp of SourcePos.span * (Label * Exp) list (* record *)
             | LetInExp of SourcePos.span * Dec list * Exp (* local declaration *)
             | AppExp of SourcePos.span * Exp * Exp (* function, argument *)
             | TypedExp of SourcePos.span * Exp * Ty
             | HandleExp of SourcePos.span * Exp * (Pat * Exp) list
             | RaiseExp of SourcePos.span * Exp
             | IfThenElseExp of SourcePos.span * Exp * Exp * Exp
             | CaseExp of SourcePos.span * Exp * (Pat * Exp) list
             | FnExp of SourcePos.span * (Pat * Exp) list
             | ProjectionExp of SourcePos.span * Label
             | ListExp of SourcePos.span * Exp vector
             | VectorExp of SourcePos.span * Exp vector
             | PrimExp of SourcePos.span * PrimOp * Ty vector * Exp vector
     and Dec = ValDec of SourcePos.span * TyVar list * ValBind list (* non-recursive *)
             | RecValDec of SourcePos.span * TyVar list * ValBind list (* recursive (val rec) *)
             | TypeDec of SourcePos.span * TypBind list
             | DatatypeDec of SourcePos.span * DatBind list * TypBind list
             | DatatypeRepDec of SourcePos.span * TyCon * LongTyCon
             | AbstypeDec of SourcePos.span * DatBind list * TypBind list * Dec list
             | ExceptionDec of SourcePos.span * ExBind list
             | LocalDec of SourcePos.span * Dec list * Dec list
             | OpenDec of SourcePos.span * LongStrId list
     and ValBind = PatBind of SourcePos.span * Pat * Exp

datatype Spec = ValDesc of SourcePos.span * (VId * Ty) list
              | TypeDesc of SourcePos.span * (TyVar list * TyCon) list
              | EqtypeDesc of SourcePos.span * (TyVar list * TyCon) list
              | DatDesc of SourcePos.span * (TyVar list * TyCon * ConBind list) list * TypBind list
              | DatatypeRepSpec of SourcePos.span * TyCon * LongTyCon
              | ExDesc of SourcePos.span * (VId * Ty option) list
              | StrDesc of SourcePos.span * (StrId * SigExp) list
              | Include of SourcePos.span * SigExp
              | Sharing of SourcePos.span * Spec list * LongTyCon list
              | SharingStructure of SourcePos.span * Spec list * LongStrId list (* derived form *)
              | TypeAliasDesc of SourcePos.span * (TyVar list * TyCon * Ty) list (* derived form *)
     and SigExp = BasicSigExp of SourcePos.span * Spec list
                | SigIdExp of SourcePos.span * SigId
                | TypeRealisationExp of SourcePos.span * SigExp * TyVar list * LongTyCon * Ty

datatype 'coreDec StrExp = StructExp of SourcePos.span * ('coreDec StrDec) list
                         | StrIdExp of SourcePos.span * LongStrId
                         | TransparentConstraintExp of SourcePos.span * 'coreDec StrExp * SigExp
                         | OpaqueConstraintExp of SourcePos.span * 'coreDec StrExp * SigExp
                         | FunctorAppExp of SourcePos.span * FunId * 'coreDec StrExp
                         | LetInStrExp of SourcePos.span * ('coreDec StrDec) list * 'coreDec StrExp
     and 'coreDec StrDec = CoreDec of SourcePos.span * 'coreDec
                         | StrBindDec of SourcePos.span * (StrId * 'coreDec StrExp) list
                         | LocalStrDec of SourcePos.span * ('coreDec StrDec) list * ('coreDec StrDec) list

datatype 'coreDec FunExp = NamedFunExp of StrId * SigExp * 'coreDec StrExp
                         | AnonymousFunExp of SigExp * 'coreDec StrExp

datatype 'coreDec TopDec = StrDec of 'coreDec StrDec
                         | SigDec of (SigId * SigExp) list
                         | FunDec of (SourcePos.span * FunId * 'coreDec FunExp) list

type Program = ((Dec TopDec) list) list

fun SimpleVarExp(span, vid) = VarExp (span, MkLongVId ([], vid))
local
    fun doFields i nil = nil
      | doFields i (x :: xs) = (NumericLabel i, x) :: doFields (i + 1) xs
in
fun TupleExp(span, xs) = RecordExp (span, doFields 1 xs)
fun TuplePat(span, xs) = RecordPat { sourceSpan = span, fields = doFields 1 xs, wildcard = false }
end

fun getSourceSpanOfPat(WildcardPat span) = span
  | getSourceSpanOfPat(SConPat(span, _)) = span
  | getSourceSpanOfPat(VarPat(span, _)) = span
  | getSourceSpanOfPat(RecordPat{sourceSpan, ...}) = sourceSpan
  | getSourceSpanOfPat(ConPat(span, _, _)) = span
  | getSourceSpanOfPat(TypedPat(span, _, _)) = span
  | getSourceSpanOfPat(LayeredPat(span, _, _, _)) = span
  | getSourceSpanOfPat(VectorPat(span, _, _)) = span

fun getSourceSpanOfExp(SConExp(span, _)) = span
  | getSourceSpanOfExp(VarExp(span, _)) = span
  | getSourceSpanOfExp(RecordExp(span, _)) = span
  | getSourceSpanOfExp(LetInExp(span, _, _)) = span
  | getSourceSpanOfExp(AppExp(span, _, _)) = span
  | getSourceSpanOfExp(TypedExp(span, _, _)) = span
  | getSourceSpanOfExp(HandleExp(span, _, _)) = span
  | getSourceSpanOfExp(RaiseExp(span, _)) = span
  | getSourceSpanOfExp(IfThenElseExp(span, _, _, _)) = span
  | getSourceSpanOfExp(CaseExp(span, _, _)) = span
  | getSourceSpanOfExp(FnExp(span, _)) = span
  | getSourceSpanOfExp(ProjectionExp(span, _)) = span
  | getSourceSpanOfExp(ListExp(span, _)) = span
  | getSourceSpanOfExp(VectorExp(span, _)) = span
  | getSourceSpanOfExp(PrimExp(span, _, _, _)) = span

fun MkInfixConPat(pat1, _, vid, pat2) = let val span = SourcePos.mergeSpan(getSourceSpanOfPat pat1, getSourceSpanOfPat pat2)
                                        in ConPat(span, MkLongVId([], vid), SOME(RecordPat { sourceSpan = span, fields = [(NumericLabel 1, pat1), (NumericLabel 2, pat2)], wildcard = false }))
                                        end
fun MkInfixExp(exp1, vspan, vid, exp2) = let val span = SourcePos.mergeSpan(getSourceSpanOfExp exp1, getSourceSpanOfExp exp2)
                                        in AppExp(span, VarExp(vspan, MkLongVId([], vid)), RecordExp(span, [(NumericLabel 1, exp1), (NumericLabel 2, exp2)]))
                                        end

(* extractTuple : int * (Label * 'a) list -> ('a list) option *)
fun extractTuple (i, nil) = SOME nil
  | extractTuple (i, (NumericLabel j,e) :: xs) = if i = j then
                                                     case extractTuple (i + 1, xs) of
                                                         NONE => NONE
                                                       | SOME ys => SOME (e :: ys)
                                                 else
                                                     NONE
  | extractTuple _ = NONE

 (* mapRecordRow : ('a -> 'b) -> (Label * 'a) list -> (Label * 'b) list *)
fun ('a,'b) mapRecordRow (f : 'a -> 'b) (row : (Label * 'a) list) = List.map (fn (label, x) => (label, f x)) row

(* pretty printing *)
structure PrettyPrint = struct
fun print_list p xs = "[" ^ String.concatWith "," (map p xs) ^ "]"
fun print_option p (SOME x) = "SOME(" ^ p x ^ ")"
  | print_option p NONE = "NONE"
fun print_pair (f,g) (x,y) = "(" ^ f x ^ "," ^ g y ^ ")"

fun print_SCon (IntegerConstant x) = "IntegerConstant " ^ Int.toString x
  | print_SCon (WordConstant x) = "WordConstant " ^ Word.toString x
  | print_SCon (RealConstant x) = "RealConstant " ^ x
  | print_SCon (StringConstant x) = "StringConstant \"" ^ String.toString x ^ "\""
  | print_SCon (CharacterConstant x) = "CharacterConstant \"" ^ String.toString x ^ "\""
fun print_VId (MkVId x) = "MkVId \"" ^ String.toString x ^ "\""
  | print_VId (GeneratedVId (x,i)) = "GeneratedVId(\"" ^ String.toString x ^ "\", " ^ Int.toString i ^ ")"
fun print_TyVar (MkTyVar x) = "MkTyVar \"" ^ String.toString x ^ "\""
fun print_TyCon (MkTyCon x) = "MkTyCon \"" ^ String.toString x ^ "\""
fun print_Label (NumericLabel x) = "NumericLabel " ^ Int.toString x
  | print_Label (IdentifierLabel x) = "IdentifierLabel \"" ^ String.toString x ^ "\""
fun print_StrId (MkStrId x) = "MkStrId \"" ^ String.toString x ^ "\""
fun print_SigId (MkSigId x) = "MkSigId \"" ^ String.toString x ^ "\""
fun print_FunId (MkFunId x) = "MkFunId \"" ^ String.toString x ^ "\""
fun print_LongVId (MkQualified(x,y)) = "MkLongVId(" ^ print_list print_StrId x ^ "," ^ print_VId y ^ ")"
fun print_LongTyCon (MkQualified(x,y)) = "MkLongTyCon(" ^ print_list print_StrId x ^ "," ^ print_TyCon y ^ ")"
fun print_LongStrId (MkQualified(x,y)) = "MkLongStrId(" ^ print_list print_StrId x ^ "," ^ print_StrId y ^ ")"
fun print_IdStatus ValueVariable = "ValueVariable"
  | print_IdStatus (ValueConstructor sole) = "ValueConstructor " ^ Bool.toString sole
  | print_IdStatus ExceptionConstructor = "ExceptionConstructor"

(* pretty printing *)
fun print_Ty (TyVar(_,x)) = "TyVar(" ^ print_TyVar x ^ ")"
  | print_Ty (RecordType(_,xs)) = "RecordType " ^ print_list (print_pair (print_Label,print_Ty)) xs
  | print_Ty (TyCon(_,x,y)) = "TyCon(" ^ print_list print_Ty x ^ "," ^ print_LongTyCon y ^ ")"
  | print_Ty (FnType(_,x,y)) = "FnType(" ^ print_Ty x ^ "," ^ print_Ty y ^ ")"
fun print_Pat (WildcardPat _) = "WildcardPat"
  | print_Pat (SConPat(_, x)) = "SConPat(" ^ print_SCon x ^ ")"
  | print_Pat (VarPat(_, vid)) = "VarPat(" ^ print_VId vid ^ ")"
  | print_Pat (TypedPat (_, pat, ty)) = "TypedPat(" ^ print_Pat pat ^ "," ^ print_Ty ty ^ ")"
  | print_Pat (LayeredPat (_, vid, oty, pat)) = "TypedPat(" ^ print_VId vid ^ "," ^ print_option print_Ty oty ^ "," ^ print_Pat pat ^ ")"
  | print_Pat (ConPat(_, longvid, pat)) = "ConPat(" ^ print_LongVId longvid ^ "," ^ print_option print_Pat pat ^ ")"
  | print_Pat (RecordPat { fields = x, wildcard = false, ... }) = (case extractTuple (1, x) of
                                                                       NONE => "RecordPat(" ^ print_list (print_pair (print_Label, print_Pat)) x ^ ",false)"
                                                                     | SOME ys => "TuplePat " ^ print_list print_Pat ys
                                                                  )
  | print_Pat (RecordPat { fields = x, wildcard = true, ... }) = "RecordPat(" ^ print_list (print_pair (print_Label, print_Pat)) x ^ ",true)"
  | print_Pat (VectorPat _) = "VectorPat"
(* | print_Pat _ = "<Pat>" *)
fun print_Exp (SConExp(_,x)) = "SConExp(" ^ print_SCon x ^ ")"
  | print_Exp (VarExp(_,MkQualified([], vid))) = "SimpleVarExp(" ^ print_VId vid ^ ")"
  | print_Exp (VarExp(_,x)) = "VarExp(" ^ print_LongVId x ^ ")"
  | print_Exp (RecordExp(_,x)) = (case extractTuple (1, x) of
                                      NONE => "RecordExp " ^ print_list (print_pair (print_Label, print_Exp)) x
                                    | SOME ys => "TupleExp " ^ print_list print_Exp ys
                                 )
  | print_Exp (LetInExp(_,decls,x)) = "LetInExp(" ^ print_list print_Dec decls ^ "," ^ print_Exp x ^ ")"
  | print_Exp (AppExp(_,x,y)) = "AppExp(" ^ print_Exp x ^ "," ^ print_Exp y ^ ")"
  | print_Exp (TypedExp(_,x,y)) = "TypedExp(" ^ print_Exp x ^ "," ^ print_Ty y ^ ")"
  | print_Exp (HandleExp(_,x,y)) = "HandleExp(" ^ print_Exp x ^ "," ^ print_list (print_pair (print_Pat, print_Exp)) y ^ ")"
  | print_Exp (RaiseExp(_,x)) = "RaiseExp(" ^ print_Exp x ^ ")"
  | print_Exp (IfThenElseExp(_,x,y,z)) = "IfThenElseExp(" ^ print_Exp x ^ "," ^ print_Exp y ^ "," ^ print_Exp z ^ ")"
  | print_Exp (CaseExp(_,x,y)) = "CaseExp(" ^ print_Exp x ^ "," ^ print_list (print_pair (print_Pat,print_Exp)) y ^ ")"
  | print_Exp (FnExp(_,x)) = "FnExp(" ^ print_list (print_pair (print_Pat,print_Exp)) x ^ ")"
  | print_Exp (ProjectionExp(_,label)) = "ProjectionExp(" ^ print_Label label ^ ")"
  | print_Exp (ListExp _) = "ListExp"
  | print_Exp (VectorExp _) = "VectorExp"
  | print_Exp (PrimExp _) = "PrimExp"
and print_Dec (ValDec (_,bound,valbind)) = "ValDec(" ^ print_list print_TyVar bound ^ "," ^ print_list print_ValBind valbind  ^ ")"
  | print_Dec (RecValDec (_,bound,valbind)) = "RecValDec(" ^ print_list print_TyVar bound ^ "," ^ print_list print_ValBind valbind  ^ ")"
  | print_Dec _ = "<Dec>"
and print_ValBind (PatBind (_,pat, exp)) = "PatBind(" ^ print_Pat pat ^ "," ^ print_Exp exp ^ ")"
val print_Decs = print_list print_Dec
fun print_VIdMap print_elem x = print_list (print_pair (print_VId,print_elem)) (VIdMap.foldri (fn (k,x,ys) => (k,x) :: ys) [] x)
fun print_TyConMap print_elem x = print_list (print_pair (print_TyCon,print_elem)) (TyConMap.foldri (fn (k,x,ys) => (k,x) :: ys) [] x)
fun print_StrIdMap print_elem x = print_list (print_pair (print_StrId,print_elem)) (StrIdMap.foldri (fn (k,x,ys) => (k,x) :: ys) [] x)
end
open PrettyPrint

exception SyntaxError of SourcePos.span list * string

end (* structure Syntax *)

structure UnfixedSyntax = struct
datatype Pat
  = WildcardPat of SourcePos.span
  | SConPat of SourcePos.span * Syntax.SCon (* special constant *)
  | NonInfixVIdPat of SourcePos.span * Syntax.LongVId (* value identifier, with 'op' or structure identifiers *)
  | InfixOrVIdPat of SourcePos.span * Syntax.VId (* value identifier, without 'op' or structure identifers *)
  | JuxtapositionPat of SourcePos.span * Pat list (* constructed pattern, maybe with binary operator  *)
  | ConPat of SourcePos.span * Syntax.LongVId * Pat (* constructed pattern, used by desugaring of list patttern *)
  | RecordPat of { sourceSpan : SourcePos.span, fields : (Syntax.Label * Pat) list, wildcard : bool }
  | TypedPat of SourcePos.span * Pat * Syntax.Ty (* typed *)
  | ConjunctivePat of SourcePos.span * Pat * Pat (* layered or [Successor ML] conjunctive *)
  | VectorPat of SourcePos.span * Pat vector * bool (* [extension] vector pattern *)

datatype Exp = SConExp of SourcePos.span * Syntax.SCon (* special constant *)
             | NonInfixVIdExp of SourcePos.span * Syntax.LongVId (* value identifier, with or without 'op'  *)
             | InfixOrVIdExp of SourcePos.span * Syntax.VId (* value identifier, without 'op' or structure identifiers *)
             | RecordExp of SourcePos.span * (Syntax.Label * Exp) list (* record *)
             | LetInExp of SourcePos.span * Dec list * Exp (* local declaration *)
             | JuxtapositionExp of SourcePos.span * Exp list (* application, or binary operator *)
             | AppExp of SourcePos.span * Exp * Exp (* application, used by desugaring of list expression *)
             | TypedExp of SourcePos.span * Exp * Syntax.Ty
             | HandleExp of SourcePos.span * Exp * (Pat * Exp) list
             | RaiseExp of SourcePos.span * Exp
             | IfThenElseExp of SourcePos.span * Exp * Exp * Exp
             | WhileDoExp of SourcePos.span * Exp * Exp
             | CaseExp of SourcePos.span * Exp * (Pat * Exp) list
             | FnExp of SourcePos.span * (Pat * Exp) list
             | ProjectionExp of SourcePos.span * Syntax.Label
             | ListExp of SourcePos.span * Exp vector
             | VectorExp of SourcePos.span * Exp vector
             | PrimValExp of SourcePos.span * string
             | PrimExp of SourcePos.span * string * Syntax.Ty vector * Exp vector
     and Dec = ValDec of SourcePos.span * Syntax.TyVar list * ValBind list
             | RecValDec of SourcePos.span * Syntax.TyVar list * ValBind list
             | FValDec of SourcePos.span * Syntax.TyVar list * FValBind list
             | TypeDec of SourcePos.span * Syntax.TypBind list
             | DatatypeDec of SourcePos.span * Syntax.DatBind list * Syntax.TypBind list
             | DatatypeRepDec of SourcePos.span * Syntax.TyCon * Syntax.LongTyCon
             | AbstypeDec of SourcePos.span * Syntax.DatBind list * Syntax.TypBind list * Dec list
             | ExceptionDec of SourcePos.span * Syntax.ExBind list
             | LocalDec of SourcePos.span * Dec list * Dec list
             | OpenDec of SourcePos.span * Syntax.LongStrId list
             | FixityDec of SourcePos.span * Syntax.FixityStatus * Syntax.VId list
     and ValBind = PatBind of SourcePos.span * Pat * Exp
     and FValBind = FValBind of SourcePos.span * FMRule list
     and FMRule = FMRule of SourcePos.span * FPat * Syntax.Ty option * Exp
     and FPat = FPat of SourcePos.span * Pat list
type Program = (Dec Syntax.StrDec) list

local
    fun doFields i nil = nil
      | doFields i (x :: xs) = (Syntax.NumericLabel i, x) :: doFields (i + 1) xs
in
fun TupleExp(span, xs) = RecordExp (span, doFields 1 xs)
fun TuplePat(span, xs) = RecordPat { sourceSpan = span, fields = doFields 1 xs, wildcard = false }
end

fun getSourceSpanOfPat(WildcardPat span) = span
  | getSourceSpanOfPat(SConPat(span, _)) = span
  | getSourceSpanOfPat(NonInfixVIdPat(span, _)) = span
  | getSourceSpanOfPat(InfixOrVIdPat(span, _)) = span
  | getSourceSpanOfPat(JuxtapositionPat(span, _)) = span
  | getSourceSpanOfPat(ConPat(span, _, _)) = span
  | getSourceSpanOfPat(RecordPat{sourceSpan, ...}) = sourceSpan
  | getSourceSpanOfPat(TypedPat(span, _, _)) = span
  | getSourceSpanOfPat(ConjunctivePat(span, _, _)) = span
  | getSourceSpanOfPat(VectorPat(span, _, _)) = span

end (* structure UnfixedSyntax *)
