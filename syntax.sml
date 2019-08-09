structure Syntax = struct
datatype SCon = IntegerConstant of int (* decimal / hexadecimal *)
              | WordConstant of word (* decimal / hexadecimal *)
              | RealConstant of real (* decimal *)
              | StringConstant of string
              | CharacterConstant of string
datatype VId = MkVId of string
datatype TyVar = MkTyVar of string
datatype TyCon = MkTyCon of string
datatype Label = NumericLabel of int
               | IdentifierLabel of string
datatype StrId = MkStrId of string
datatype LongVId = MkLongVId of StrId list * VId
datatype LongTyCon = MkLongTyCon of StrId list * TyCon
datatype LongStrId = MkLongStrId of StrId list * StrId

datatype InfixAssociativity = LeftAssoc of int
                            | RightAssoc of int
datatype FixityStatus = Nonfix
                      | Infix of InfixAssociativity

datatype IdStatus = ValueVariable
                  | ValueConstructor
                  | ExceptionConstructor

(* BinaryMapFn, BinarySetFn: from smlnj-lib *)
structure VIdKey = struct
type ord_key = VId
fun compare (MkVId x, MkVId y) = String.compare (x,y)
end : ORD_KEY
structure VIdSet = BinarySetFn(VIdKey)
structure VIdMap = BinaryMapFn(VIdKey)
structure TyConMap = BinaryMapFn(struct
                                  type ord_key = TyCon
                                  fun compare (MkTyCon x, MkTyCon y) = String.compare (x,y)
                                  end)
structure StrIdMap = BinaryMapFn(struct
                                  type ord_key = StrId
                                  fun compare (MkStrId x, MkStrId y) = String.compare (x,y)
                                  end)
structure TyVarKey = struct
type ord_key = TyVar
fun compare (MkTyVar x, MkTyVar y) = String.compare (x,y)
end : ORD_KEY
structure TyVarMap = BinaryMapFn(TyVarKey)
structure TyVarSet = BinarySetFn(TyVarKey)
structure LabelKey = struct
type ord_key = Label
(* NumericLabel _ < IdentifierLabel _ *)
fun compare (NumericLabel x, NumericLabel y) = Int.compare (x,y)
  | compare (NumericLabel _, IdentifierLabel _) = LESS
  | compare (IdentifierLabel _, NumericLabel _) = GREATER
  | compare (IdentifierLabel x, IdentifierLabel y) = String.compare (x,y)
end
structure LabelSet = BinarySetFn(LabelKey)

datatype Ty = TyVar of TyVar (* type variable *)
            | RecordType of (Label * Ty) list (* record type expression *)
            | TyCon of Ty list * LongTyCon (* type construction *)
            | FnType of Ty * Ty (* function type expression *)

datatype Pat = WildcardPat
             | SConPat of SCon (* special constant *)
             | ConOrVarPat of VId (* constructor or variable *)
             | VarPat of VId (* variable *)
             | RecordPat of (Label * Pat) list * bool
             | ConPat of LongVId * Pat option (* constructed pattern *)
             | TypedPat of Pat * Ty (* typed *)
             | LayeredPat of VId * Ty option * Pat (* layered *)

datatype TypBind = TypBind of TyVar list * TyCon * Ty
datatype ConBind = ConBind of VId * Ty option
datatype DatBind = DatBind of TyVar list * TyCon * ConBind list
datatype ExBind = ExBind1 of VId * Ty option (* <op> vid <of ty> *)
                | ExBind2 of VId * LongVId (* <op> vid = <op> longvid *)

datatype Exp = SConExp of SCon (* special constant *)
             | VarExp of LongVId (* value identifier, with or without 'op'  *)
             | RecordExp of (Label * Exp) list (* record *)
             | LetInExp of Dec list * Exp (* local declaration *)
             | AppExp of Exp * Exp (* function, argument *)
             | TypedExp of Exp * Ty
             | HandleExp of Exp * (Pat * Exp) list
             | RaiseExp of Exp
             | IfThenElseExp of Exp * Exp * Exp
             | CaseExp of Exp * (Pat * Exp) list
             | FnExp of (Pat * Exp) list
             | ProjectionExp of Label
     and Dec = ValDec of TyVar list * ValBind list (* non-recursive *)
             | RecValDec of TyVar list * ValBind list (* recursive (val rec) *)
             | TypeDec of TypBind list
             | DatatypeDec of DatBind list
             | DatatypeRepDec of TyCon * LongTyCon
             | AbstypeDec of DatBind list * Dec list
             | ExceptionDec of ExBind list
             | LocalDec of Dec list * Dec list
             | OpenDec of LongStrId list
             | FixityDec of FixityStatus * VId list
     and ValBind = PatBind of Pat * Exp
type Program = Dec list

fun SimpleVarExp vid = VarExp (MkLongVId ([], vid))
fun TupleExp xs = let fun doFields i nil = nil
                        | doFields i (x :: xs) = (NumericLabel i, x) :: doFields (i + 1) xs
                  in RecordExp (doFields 1 xs)
                  end

fun MkInfixConPat(pat1, vid, pat2) = ConPat(MkLongVId([], vid), SOME(RecordPat([(NumericLabel 1, pat1), (NumericLabel 2, pat2)], false)))
fun MkInfixExp(exp1, vid, exp2) = AppExp(VarExp(MkLongVId([], vid)), RecordExp([(NumericLabel 1, exp1), (NumericLabel 2, exp2)]))

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
  | print_SCon (RealConstant x) = "RealConstant " ^ Real.toString x
  | print_SCon (StringConstant x) = "StringConstant \"" ^ String.toString x ^ "\""
  | print_SCon (CharacterConstant x) = "CharacterConstant \"" ^ String.toString x ^ "\""
fun print_VId (MkVId x) = "MkVId \"" ^ String.toString x ^ "\""
fun print_TyVar (MkTyVar x) = "MkTyVar \"" ^ String.toString x ^ "\""
fun print_TyCon (MkTyCon x) = "MkTyCon \"" ^ String.toString x ^ "\""
fun print_Label (NumericLabel x) = "NumericLabel " ^ Int.toString x
  | print_Label (IdentifierLabel x) = "IdentifierLabel \"" ^ String.toString x ^ "\""
fun print_StrId (MkStrId x) = "MkStrId \"" ^ String.toString x ^ "\""
fun print_LongVId (MkLongVId(x,y)) = "MkLongVId(" ^ print_list print_StrId x ^ "," ^ print_VId y ^ ")"
fun print_LongTyCon (MkLongTyCon(x,y)) = "MkLongTyCon(" ^ print_list print_StrId x ^ "," ^ print_TyCon y ^ ")"
fun print_LongStrId (MkLongStrId(x,y)) = "MkLongStrId(" ^ print_list print_StrId x ^ "," ^ print_StrId y ^ ")"
fun print_IdStatus ValueVariable = "ValueVariable"
  | print_IdStatus ValueConstructor = "ValueConstructor"
  | print_IdStatus ExceptionConstructor = "ExceptionConstructor"

(* pretty printing *)
fun print_Ty (TyVar x) = "TyVar(" ^ print_TyVar x ^ ")"
  | print_Ty (RecordType xs) = "RecordType " ^ print_list (print_pair (print_Label,print_Ty)) xs
  | print_Ty (TyCon(x,y)) = "TyCon(" ^ print_list print_Ty x ^ "," ^ print_LongTyCon y ^ ")"
  | print_Ty (FnType(x,y)) = "FnType(" ^ print_Ty x ^ "," ^ print_Ty y ^ ")"
fun print_Pat WildcardPat = "WildcardPat"
  | print_Pat (SConPat x) = "SConPat(" ^ print_SCon x ^ ")"
  | print_Pat (ConOrVarPat vid) = "ConOrVarPat(" ^ print_VId vid ^ ")"
  | print_Pat (VarPat vid) = "VarPat(" ^ print_VId vid ^ ")"
  | print_Pat (TypedPat (pat, ty)) = "TypedPat(" ^ print_Pat pat ^ "," ^ print_Ty ty ^ ")"
  | print_Pat (LayeredPat (vid, oty, pat)) = "TypedPat(" ^ print_VId vid ^ "," ^ print_option print_Ty oty ^ "," ^ print_Pat pat ^ ")"
  | print_Pat (ConPat(longvid, pat)) = "ConPat(" ^ print_LongVId longvid ^ "," ^ print_option print_Pat pat ^ ")"
  | print_Pat (RecordPat(x, false)) = (case extractTuple (1, x) of
                                           NONE => "RecordPat(" ^ print_list (print_pair (print_Label, print_Pat)) x ^ ",false)"
                                         | SOME ys => "TuplePat " ^ print_list print_Pat ys
                                      )
  | print_Pat (RecordPat(x, true)) = "RecordPat(" ^ print_list (print_pair (print_Label, print_Pat)) x ^ ",true)"
(* | print_Pat _ = "<Pat>" *)
fun print_Exp (SConExp x) = "SConExp(" ^ print_SCon x ^ ")"
  | print_Exp (VarExp(MkLongVId([], vid))) = "SimpleVarExp(" ^ print_VId vid ^ ")"
  | print_Exp (VarExp x) = "VarExp(" ^ print_LongVId x ^ ")"
  | print_Exp (RecordExp x) = (case extractTuple (1, x) of
                                   NONE => "RecordExp " ^ print_list (print_pair (print_Label, print_Exp)) x
                                 | SOME ys => "TupleExp " ^ print_list print_Exp ys
                              )
  | print_Exp (LetInExp(decls,x)) = "LetInExp(" ^ print_list print_Dec decls ^ "," ^ print_Exp x ^ ")"
  | print_Exp (AppExp(x,y)) = "AppExp(" ^ print_Exp x ^ "," ^ print_Exp y ^ ")"
  | print_Exp (TypedExp(x,y)) = "TypedExp(" ^ print_Exp x ^ "," ^ print_Ty y ^ ")"
  | print_Exp (HandleExp(x,y)) = "HandleExp(" ^ print_Exp x ^ "," ^ print_list (print_pair (print_Pat, print_Exp)) y ^ ")"
  | print_Exp (RaiseExp x) = "RaiseExp(" ^ print_Exp x ^ ")"
  | print_Exp (IfThenElseExp(x,y,z)) = "IfThenElseExp(" ^ print_Exp x ^ "," ^ print_Exp y ^ "," ^ print_Exp z ^ ")"
  | print_Exp (CaseExp(x,y)) = "CaseExp(" ^ print_Exp x ^ "," ^ print_list (print_pair (print_Pat,print_Exp)) y ^ ")"
  | print_Exp (FnExp x) = "FnExp(" ^ print_list (print_pair (print_Pat,print_Exp)) x ^ ")"
  | print_Exp (ProjectionExp label) = "ProjectionExp(" ^ print_Label label ^ ")"
and print_Dec (ValDec (bound,valbind)) = "ValDec(" ^ print_list print_TyVar bound ^ "," ^ print_list print_ValBind valbind  ^ ")"
  | print_Dec (RecValDec (bound,valbind)) = "RecValDec(" ^ print_list print_TyVar bound ^ "," ^ print_list print_ValBind valbind  ^ ")"
  | print_Dec _ = "<Dec>"
and print_ValBind (PatBind (pat, exp)) = "PatBind(" ^ print_Pat pat ^ "," ^ print_Exp exp ^ ")"
val print_Decs = print_list print_Dec
fun print_VIdMap print_elem x = print_list (print_pair (print_VId,print_elem)) (VIdMap.foldri (fn (k,x,ys) => (k,x) :: ys) [] x)
fun print_TyConMap print_elem x = print_list (print_pair (print_TyCon,print_elem)) (TyConMap.foldri (fn (k,x,ys) => (k,x) :: ys) [] x)
fun print_StrIdMap print_elem x = print_list (print_pair (print_StrId,print_elem)) (StrIdMap.foldri (fn (k,x,ys) => (k,x) :: ys) [] x)
end
open PrettyPrint

exception SyntaxError of string

end (* structure Syntax *)

structure UnfixedSyntax = struct
datatype Pat
  = WildcardPat
  | SConPat of Syntax.SCon (* special constant *)
  | NonInfixVIdPat of Syntax.LongVId (* value identifier, with 'op' or structure identifiers *)
  | InfixOrVIdPat of Syntax.VId (* value identifier, without 'op' or structure identifers *)
  | JuxtapositionPat of Pat list (* constructed pattern, maybe with binary operator  *)
  | ConPat of Syntax.LongVId * Pat (* constructed pattern, used by desugaring of list patttern *)
  | RecordPat of (Syntax.Label * Pat) list * bool
  | TypedPat of Pat * Syntax.Ty (* typed *)
  | LayeredPat of Syntax.VId * Syntax.Ty option * Pat (* layered *)

datatype Exp = SConExp of Syntax.SCon (* special constant *)
             | NonInfixVIdExp of Syntax.LongVId (* value identifier, with or without 'op'  *)
             | InfixOrVIdExp of Syntax.VId (* value identifier, without 'op' or structure identifiers *)
             | RecordExp of (Syntax.Label * Exp) list (* record *)
             | LetInExp of Dec list * Exp (* local declaration *)
             | JuxtapositionExp of Exp list (* application, or binary operator *)
             | AppExp of Exp * Exp (* application, used by desugaring of list expression *)
             | TypedExp of Exp * Syntax.Ty
             | HandleExp of Exp * (Pat * Exp) list
             | RaiseExp of Exp
             | IfThenElseExp of Exp * Exp * Exp
             | CaseExp of Exp * (Pat * Exp) list
             | FnExp of (Pat * Exp) list
             | ProjectionExp of Syntax.Label
     and Dec = ValDec of Syntax.TyVar list * ValBind list
             | RecValDec of Syntax.TyVar list * ValBind list
             | TypeDec of Syntax.TypBind list
             | DatatypeDec of Syntax.DatBind list
             | DatatypeRepDec of Syntax.TyCon * Syntax.LongTyCon
             | AbstypeDec of Syntax.DatBind list * Dec list
             | ExceptionDec of Syntax.ExBind list
             | LocalDec of Dec list * Dec list
             | OpenDec of Syntax.LongStrId list
             | FixityDec of Syntax.FixityStatus * Syntax.VId list
     and ValBind = PatBind of Pat * Exp
type Program = Dec list

fun TupleExp xs = let fun doFields i nil = nil
                        | doFields i (x :: xs) = (Syntax.NumericLabel i, x) :: doFields (i + 1) xs
                  in RecordExp (doFields 1 xs)
                  end
fun TuplePat xs = let fun doFields i nil = nil
                        | doFields i (x :: xs) = (Syntax.NumericLabel i, x) :: doFields (i + 1) xs
                  in RecordPat (doFields 1 xs, false)
                  end

end (* structure UnfixedSyntax *)
