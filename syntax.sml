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

datatype Ty = TyVar of TyVar (* type variable *)
            | RecordType of (Label * Ty) list (* record type expression *)
            | TyCon of Ty list * LongTyCon (* type construction *)
            | FnType of Ty * Ty (* function type expression *)

datatype Pat = WildcardPat
             | SConPat of SCon (* special constant *)
             | VIdPat of LongVId
             | RecordPat of (Label * Pat) list * bool
             | ConPat of LongVId * Pat (* constructed pattern *)
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
     and Dec = ValDec of TyVar list * ValBind
             | TypeDec of TypBind list
             | DatatypeDec of DatBind list
             | DatatypeRepDec of TyCon * LongTyCon
             | AbstypeDec of DatBind list * Dec list
             | ExceptionDec of ExBind list
             | LocalDec of Dec list * Dec list
             | OpenDec of LongStrId list
             | InfixDec of int option * VId list
             | InfixrDec of int option * VId list
             | NonfixDec of VId list
     and ValBind = PatBind of Pat * Exp * ValBind option
                 | RecValBind of ValBind
type Program = Dec list

fun TupleExp xs = let fun doFields i nil = nil
                        | doFields i (x :: xs) = (NumericLabel i, x) :: doFields (i + 1) xs
                  in RecordExp (doFields 1 xs)
                  end

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
fun print_list p xs = "[" ^ String.concatWith "," (map p xs) ^ "]"
fun print_pair (f,g) (x,y) = "(" ^ f x ^ "," ^ g y ^ ")"
fun print_LongVId (MkLongVId(x,y)) = "MkLongVId(" ^ print_list print_StrId x ^ "," ^ print_VId y ^ ")"
fun print_LongTyCon (MkLongTyCon(x,y)) = "MkLongTyCon(" ^ print_list print_StrId x ^ "," ^ print_TyCon y ^ ")"
fun print_LongStrId (MkLongStrId(x,y)) = "MkLongStrId(" ^ print_list print_StrId x ^ "," ^ print_StrId y ^ ")"
fun print_Ty (TyVar x) = "TyVar(" ^ print_TyVar x ^ ")"
  | print_Ty (RecordType xs) = "RecordType " ^ print_list (print_pair (print_Label,print_Ty)) xs
  | print_Ty (TyCon(x,y)) = "TyCon(" ^ print_list print_Ty x ^ "," ^ print_LongTyCon y ^ ")"
  | print_Ty (FnType(x,y)) = "FnType(" ^ print_Ty x ^ "," ^ print_Ty y ^ ")"
fun print_Pat WildcardPat = "WildcardPat"
  | print_Pat (SConPat x) = "SConPat(" ^ print_SCon x ^ ")"
  | print_Pat (VIdPat x) = "VIdPat(" ^ print_LongVId x ^ ")"
  | print_Pat _ = "<Pat>"
fun print_Exp (SConExp x) = "SConExp(" ^ print_SCon x ^ ")"
  | print_Exp (VarExp(MkLongVId([],MkVId x))) = "SimpleVarExp(\"" ^ String.toString x ^ "\")"
  | print_Exp (VarExp x) = "VarExp(" ^ print_LongVId x ^ ")"
  | print_Exp (RecordExp x) = let fun extractTuple (i, nil) = SOME nil
                                    | extractTuple (i, (NumericLabel j,e) :: xs) = if i = j then
                                                                                      case extractTuple (i + 1, xs) of
                                                                                          NONE => NONE
                                                                                        | SOME ys => SOME (e :: ys)
                                                                                  else
                                                                                      NONE
                                    | extractTuple _ = NONE
                              in case extractTuple (1, x) of
                                     NONE => "RecordExp " ^ print_list (print_pair (print_Label, print_Exp)) x
                                   | SOME ys => "TupleExp " ^ print_list print_Exp ys
                              end
  | print_Exp (AppExp(x,y)) = "AppExp(" ^ print_Exp x ^ "," ^ print_Exp y ^ ")"
  | print_Exp (TypedExp(x,y)) = "TypedExp(" ^ print_Exp x ^ "," ^ print_Ty y ^ ")"
  | print_Exp (HandleExp(x,y)) = "HandleExp(" ^ print_Exp x ^ "," ^ print_list (print_pair (print_Pat, print_Exp)) y ^ ")"
  | print_Exp (RaiseExp x) = "RaiseExp(" ^ print_Exp x ^ ")"
  | print_Exp (IfThenElseExp(x,y,z)) = "IfThenElseExp(" ^ print_Exp x ^ "," ^ print_Exp y ^ "," ^ print_Exp z ^ ")"
  | print_Exp (CaseExp(x,y)) = "CaseExp(" ^ print_Exp x ^ "," ^ print_list (print_pair (print_Pat,print_Exp)) y ^ ")"
  | print_Exp (FnExp x) = "FnExp(" ^ print_list (print_pair (print_Pat,print_Exp)) x ^ ")"
  | print_Exp _ = "<Exp>"
and print_Dec _ = "<Dec>"
and print_ValBind _ = "<ValBind>"
end
