structure Syntax = struct
datatype SCon = IntegerConstant of int (* decimal / hexadecimal *)
              | WordConstant of word (* decimal / hexadecimal *)
              | RealConstant (* decimal *)
              | StringConstant of string
              | CharacterConstant of string
datatype VId = VId of string
datatype TyVar = TyVar of string
datatype TyCon = TyCon of string
datatype Label = NumericLabel of int
               | IdentifierLabel of string
datatype StrId = StrId of string
datatype LongVId = LongVId of StrId list * VId
datatype LongTyCon = LongTyCon of StrId list * TyCon
datatype LongStrId = LongStrId of StrId list * StrId

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
             | OpAppExp of Exp * Exp * Exp (* operator, lhs, rhs *)
             | TypedExp of Exp * Ty
             | HandleExp of Exp * (Pat * Exp) list
             | RaiseExp of Exp
             | FnExp of (Pat * Exp) list
     and Dec = ValDec of TyVar list * ValBind
             | TypeDec of TypBind list
             | DatatypeDec of DatBind list
             | DatatypeRepDec of TyCon * LongTyCon
             | AbstypeDec of DatBind * Dec list
             | ExceptionDec of ExBind
             | LocalDec of Dec list * Dec list
             | OpenDec of LongStrId list
             | InfixDec of int option * VId list
             | InfixrDec of int option * VId list
             | NonfixDec of VId list
     and ValBind = PatBind of Pat * Exp * ValBind option
                 | RecValBind of ValBind
type Program = Dec list
end

