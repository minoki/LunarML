(*
 * Copyright (c) 2022 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure TypedSyntax = struct
datatype VId = MkVId of string * int
datatype TyVar = MkTyVar of string * int
datatype TyName = MkTyName of string * int
datatype StrId = MkStrId of string * int
datatype FunId = MkFunId of string * int
datatype LongVId = MkShortVId of VId
                 | MkLongVId of StrId * Syntax.StrId list * Syntax.VId
datatype LongStrId = MkLongStrId of StrId * Syntax.StrId list
fun eqUTyVar (MkTyVar (name, a), MkTyVar (name', b)) = name = name' andalso a = b
fun eqTyName(MkTyName(_,a),MkTyName(_,b)) = a = b
fun eqVId(a, b : VId) = a = b
fun eqULongVId(MkShortVId a, MkShortVId b) = eqVId(a, b)
  | eqULongVId(MkLongVId(s, t, u), MkLongVId(s', t', u')) = s = s' andalso t = t' andalso u = u'
  | eqULongVId(_, _) = false

fun tyVarAdmitsEquality (MkTyVar (name, _)) = String.isPrefix "''" name

structure TyVarKey = struct
type ord_key = TyVar
fun compare (MkTyVar (x, a), MkTyVar (y, b)) = (case Int.compare (a, b) of
                                                    EQUAL => String.compare (x, y)
                                                  | ord => ord
                                               )
end : ORD_KEY
structure TyVarSet = RedBlackSetFn(TyVarKey)
structure TyVarMap = RedBlackMapFn(TyVarKey)

structure VIdKey = struct
type ord_key = VId
fun compare (MkVId (x, a), MkVId (y, b)) = case Int.compare (a, b) of
                                               EQUAL => String.compare (x, y)
                                             | ord => ord
end : ORD_KEY
structure VIdSet = RedBlackSetFn(VIdKey)
structure VIdSet = struct
(* compatibility with older smlnj-lib *)
open VIdSet
val toList = foldr (op ::) []
fun disjoint (x, y) = isEmpty (intersection (x, y))
open VIdSet
end
structure VIdMap = RedBlackMapFn(VIdKey)
structure VIdTable = HashTableFn (struct
                                   type hash_key = VId
                                   fun hashVal (MkVId (_, i)) = Word.fromInt i
                                   val sameKey = eqVId
                                   end)
structure VIdSCC = StronglyConnectedComponents (type t = VId
                                                structure Map = VIdMap
                                                structure Set = VIdSet
                                               )

structure TyNameKey = struct
type ord_key = TyName
fun compare(MkTyName(x,a), MkTyName(y,b)) = case String.compare (x,y) of
                                                EQUAL => Int.compare(a,b)
                                              | ord => ord
end : ORD_KEY
structure TyNameSet = RedBlackSetFn(TyNameKey)
structure TyNameMap = RedBlackMapFn(TyNameKey)

structure StrIdKey = struct
type ord_key = StrId
fun compare(MkStrId(x,a), MkStrId(y,b)) = case String.compare (x,y) of
                                              EQUAL => Int.compare(a,b)
                                            | ord => ord
end : ORD_KEY
structure StrIdSet = RedBlackSetFn(StrIdKey)
structure StrIdMap = RedBlackMapFn(StrIdKey)

structure LongVIdKey = struct
type ord_key = LongVId
fun compare(MkShortVId(vid), MkShortVId(vid')) = VIdKey.compare(vid, vid')
  | compare(MkShortVId _, MkLongVId _) = LESS
  | compare(MkLongVId _, MkShortVId _) = GREATER
  | compare(MkLongVId(strid0, strids, vid), MkLongVId(strid0', strids', vid')) = case StrIdKey.compare(strid0, strid0') of
                                                                                     EQUAL => (case Syntax.VIdKey.compare(vid, vid') of
                                                                                                  EQUAL => List.collate Syntax.StrIdKey.compare (strids, strids')
                                                                                                | x => x
                                                                                              )
                                                                                   | x => x
end : ORD_KEY
structure LongVIdSet = RedBlackSetFn(LongVIdKey)
structure LongVIdMap = RedBlackMapFn(LongVIdKey)

type level = int

datatype UnaryConstraint
  = NoField of Syntax.Label
  | IsRecord
  | IsEqType
  | IsIntegral (* Int, Word; div, mod; defaults to int *)
  | IsSignedReal (* Int, Real; abs; defaults to int *)
  | IsRing (* Int, Word, Real; *, +, -, ~; defaults to int *)
  | IsField (* Real; /; defaults to real *)
  | IsSigned (* Int, Real; defaults to int *)
  | IsOrdered (* NumTxt; <, >, <=, >=; defaults to int *)
  | IsInt (* Int; defaults to int *)
  | IsWord (* Word; defaults to word *)
  | IsReal (* Real; defaults to real *)
  | IsChar (* Char; defaults to char *)
  | IsString (* String; defaults to string *)

datatype Ty = TyVar of SourcePos.span * TyVar (* named type variable *)
            | AnonymousTyVar of SourcePos.span * TyVarData ref (* anonymous type variable; only used during type checking *)
            | RecordType of SourcePos.span * Ty Syntax.LabelMap.map (* record type expression *)
            | TyCon of SourcePos.span * Ty list * TyName (* type construction *)
            | FnType of SourcePos.span * Ty * Ty (* function type expression *)
            | RecordExtType of SourcePos.span * Ty Syntax.LabelMap.map * Ty (* only used during type checking *)
     and TyVarData = Unbound of (SourcePos.span * UnaryConstraint) list * level
                   | Link of Ty

type AnonymousTyVar = TyVarData ref

fun PairType (span, a, b) = RecordType (span, Syntax.LabelMapFromList [(Syntax.NumericLabel 1, a), (Syntax.NumericLabel 2, b)])
fun TupleType (span, xs) = let fun doFields (i, nil, m) = m
                                 | doFields (i, x :: xs, m) = doFields (i + 1, xs, Syntax.LabelMap.insert (m, Syntax.NumericLabel i, x))
                           in RecordType (span, doFields (1, xs, Syntax.LabelMap.empty))
                           end

datatype Constraint
  = EqConstr of SourcePos.span * Ty * Ty (* ty1 = ty2 *)
  | UnaryConstraint of SourcePos.span * Ty * UnaryConstraint

datatype TypeFunction = TypeFunction of TyVar list * Ty
datatype TypeScheme = TypeScheme of (TyVar * UnaryConstraint list) list * Ty
type ValEnv = (TypeScheme * Syntax.ValueConstructorInfo Syntax.IdStatus) Syntax.VIdMap.map
val emptyValEnv : ValEnv = Syntax.VIdMap.empty

type TypeStructure = { typeFunction : TypeFunction
                     , valEnv : ValEnv
                     }

datatype Signature' = MkSignature of Signature
withtype Signature = { valMap : (TypeScheme * Syntax.ValueConstructorInfo Syntax.IdStatus) Syntax.VIdMap.map
                     , tyConMap : TypeStructure Syntax.TyConMap.map
                     , strMap : Signature' Syntax.StrIdMap.map
                     }
type QSignature = { s : Signature
                  , bound : { arity : int, admitsEquality : bool, longtycon : Syntax.LongTyCon } TyNameMap.map
                  }
type PackedSignature = { s : Signature
                       , bound : { tyname : TyName, arity : int, admitsEquality : bool } list
                       }
type FunSig = { bound : { tyname : TyName, arity : int, admitsEquality : bool, longtycon : Syntax.LongTyCon } list
              , paramSig : Signature
              , resultSig : PackedSignature
              }

datatype Pat = WildcardPat of SourcePos.span
             | SConPat of SourcePos.span * Syntax.SCon * Ty (* special constant *)
             | VarPat of SourcePos.span * VId * Ty (* variable *)
             | RecordPat of { sourceSpan : SourcePos.span, fields : (Syntax.Label * Pat) list, ellipsis : Pat option, wholeRecordType : Ty }
             | ConPat of { sourceSpan : SourcePos.span
                         , longvid : LongVId
                         , payload : (Ty * Pat) option
                         , tyargs : Ty list
                         , valueConstructorInfo : Syntax.ValueConstructorInfo option
                         }
             | TypedPat of SourcePos.span * Pat * Ty (* typed *)
             | LayeredPat of SourcePos.span * VId * Ty * Pat (* layered *)
             | VectorPat of SourcePos.span * Pat vector * bool * Ty (* [extension] vector pattern *)

datatype TypBind = TypBind of SourcePos.span * TyVar list * Syntax.TyCon * Ty
datatype ConBind = ConBind of SourcePos.span * VId * Ty option * Syntax.ValueConstructorInfo
datatype DatBind = DatBind of SourcePos.span * TyVar list * TyName * ConBind list * (* admits equality? *) bool
datatype ExBind = ExBind of SourcePos.span * VId * Ty option (* <op> vid <of ty> *)
                | ExReplication of SourcePos.span * VId * LongVId * Ty option

datatype match_type = CASE | VAL | HANDLE
datatype valdesc_origin = VALDESC_COMMENT | VALDESC_SEQUENCE

datatype Exp = SConExp of SourcePos.span * Syntax.SCon * Ty (* special constant *)
             | VarExp of SourcePos.span * LongVId * Syntax.ValueConstructorInfo Syntax.IdStatus * (Ty * UnaryConstraint list) list (* identifiers with type arguments *)
             | RecordExp of SourcePos.span * (Syntax.Label * Exp) list (* record *)
             | RecordExtExp of { sourceSpan : SourcePos.span
                               , fields : (Syntax.Label * Exp) list
                               , baseExp : Exp
                               , baseTy : Ty
                               } (* record extension *)
             | LetInExp of SourcePos.span * Dec list * Exp (* local declaration *)
             | AppExp of SourcePos.span * Exp * Exp (* function, argument *)
             | TypedExp of SourcePos.span * Exp * Ty
             | HandleExp of SourcePos.span * Exp * (Pat * Exp) list * Ty
             | RaiseExp of SourcePos.span * Ty * Exp (* result type, exception *)
             | IfThenElseExp of SourcePos.span * Exp * Exp * Exp
             | CaseExp of { sourceSpan : SourcePos.span
                          , subjectExp : Exp
                          , subjectTy : Ty
                          , matches : (Pat * Exp) list
                          , matchType : match_type
                          , resultTy : Ty
                          }
             | FnExp of SourcePos.span * VId * Ty * Exp (* parameter name, parameter type, body *)
             | ProjectionExp of { sourceSpan : SourcePos.span, label : Syntax.Label, recordTy : Ty, fieldTy : Ty }
             | ListExp of SourcePos.span * Exp vector * Ty
             | VectorExp of SourcePos.span * Exp vector * Ty
             | PrimExp of SourcePos.span * Primitives.PrimOp * Ty vector * Exp vector
     and Dec = ValDec of SourcePos.span * ValBind list (* non-recursive *)
             | RecValDec of SourcePos.span * ValBind list (* recursive (val rec) *)
             | IgnoreDec of SourcePos.span * Exp * Ty (* val _ = ... *)
             | TypeDec of SourcePos.span * TypBind list (* not used by the type checker *)
             | DatatypeDec of SourcePos.span * DatBind list
             | ExceptionDec of SourcePos.span * ExBind list
             | GroupDec of SourcePos.span * Dec list
             | OverloadDec of SourcePos.span * Syntax.OverloadClass * TyName * Exp Syntax.OverloadKeyMap.map
             | EqualityDec of SourcePos.span * TyVar list * TyName * Exp
             | ValDescDec of { sourceSpan : SourcePos.span, expected : TypeScheme, actual : TypeScheme, origin : valdesc_origin }
     and ValBind = TupleBind of SourcePos.span * (VId * Ty) list * Exp (* monomorphic binding; produced during type-check *)
                 | PolyVarBind of SourcePos.span * VId * TypeScheme * Exp (* polymorphic binding; produced during type-check *)

datatype StrExp = StructExp of { sourceSpan : SourcePos.span
                               , valMap : (LongVId * Syntax.ValueConstructorInfo Syntax.IdStatus) Syntax.VIdMap.map
                               , tyConMap : TypeStructure Syntax.TyConMap.map
                               , strMap : LongStrId Syntax.StrIdMap.map
                               }
                | StrIdExp of SourcePos.span * LongStrId
                | PackedStrExp of { sourceSpan : SourcePos.span, strExp : StrExp, payloadTypes : TypeFunction list, packageSig : PackedSignature }
                | FunctorAppExp of { sourceSpan : SourcePos.span, funId : FunId, argumentTypes : { typeFunction : TypeFunction, admitsEquality : bool } list, argumentStr : StrExp, packageSig : PackedSignature }
                | LetInStrExp of SourcePos.span * StrDec list * StrExp
     and StrDec = CoreDec of SourcePos.span * Dec
                | StrBindDec of SourcePos.span * StrId * StrExp * PackedSignature
                | GroupStrDec of SourcePos.span * StrDec list
type FunExp = { tyname : TyName, arity : int, admitsEquality : bool } list * StrId * Signature * StrExp

datatype TopDec = StrDec of StrDec
                | FunDec of FunId * FunExp
type Program = (TopDec list) list

fun TupleType(span, xs) = RecordType (span, #2 (List.foldl (fn (ty, (i, m)) => (i+1, Syntax.LabelMap.insert (m, Syntax.NumericLabel i, ty))) (1, Syntax.LabelMap.empty) xs))
local
    fun doFields i nil = nil
      | doFields i (x :: xs) = (Syntax.NumericLabel i, x) :: doFields (i + 1) xs
in
fun TupleExp(span, xs) = RecordExp (span, doFields 1 xs)
end

fun getSourceSpanOfTy(TyVar(span, _)) = span
  | getSourceSpanOfTy (AnonymousTyVar (span, _)) = span
  | getSourceSpanOfTy(RecordType(span, _)) = span
  | getSourceSpanOfTy(TyCon(span, _, _)) = span
  | getSourceSpanOfTy(FnType(span, _, _)) = span
  | getSourceSpanOfTy (RecordExtType (span, _, _)) = span

fun getSourceSpanOfExp(SConExp(span, _, _)) = span
  | getSourceSpanOfExp(VarExp(span, _, _, _)) = span
  | getSourceSpanOfExp(RecordExp(span, _)) = span
  | getSourceSpanOfExp(RecordExtExp { sourceSpan, ... }) = sourceSpan
  | getSourceSpanOfExp(LetInExp(span, _, _)) = span
  | getSourceSpanOfExp(AppExp(span, _, _)) = span
  | getSourceSpanOfExp(TypedExp(span, _, _)) = span
  | getSourceSpanOfExp (HandleExp (span, _, _, _)) = span
  | getSourceSpanOfExp(RaiseExp(span, _, _)) = span
  | getSourceSpanOfExp(IfThenElseExp(span, _, _, _)) = span
  | getSourceSpanOfExp (CaseExp { sourceSpan, ... }) = sourceSpan
  | getSourceSpanOfExp(FnExp(span, _, _, _)) = span
  | getSourceSpanOfExp(ProjectionExp{sourceSpan, ...}) = sourceSpan
  | getSourceSpanOfExp(ListExp(span, _, _)) = span
  | getSourceSpanOfExp(VectorExp(span, _, _)) = span
  | getSourceSpanOfExp(PrimExp(span, _, _, _)) = span

(* pretty printing *)
structure PrettyPrint = struct
fun print_VId(MkVId(name, n)) = name ^ "@" ^ Int.toString n
fun print_StrId(MkStrId(name,n)) = name ^ "@" ^ Int.toString n
fun print_FunId(MkFunId(name,n)) = name ^ "@" ^ Int.toString n
fun print_LongVId(MkShortVId(vid)) = print_VId vid
  | print_LongVId(MkLongVId(strid, strids, vid)) = "MkLongVId(" ^ print_StrId strid ^ "," ^ Syntax.print_list Syntax.print_StrId strids ^ "," ^ Syntax.print_VId vid ^ ")"
fun print_LongStrId(MkLongStrId(strid, strids)) = String.concatWith "." (print_StrId strid :: List.map (fn Syntax.MkStrId name => name) strids)
fun print_TyVar (MkTyVar (tvname, n)) = "MkTyVar(\"" ^ String.toString tvname ^ "\"," ^ Int.toString n ^ ")"
fun print_AnonymousTyVar _ = "<AnonymousTyVar>" (* (MkAnonymousTyVar n) = "AnonymousTyVar(" ^ Int.toString n ^ ")" *)
fun print_TyName (MkTyName ("int", 0)) = "primTyName_int"
  | print_TyName (MkTyName ("word", 1)) = "primTyName_word"
  | print_TyName (MkTyName ("real", 2)) = "primTyName_real"
  | print_TyName (MkTyName ("string", 3)) = "primTyName_string"
  | print_TyName (MkTyName ("char", 4)) = "primTyName_char"
  | print_TyName (MkTyName ("exn", 5)) = "primTyName_exn"
  | print_TyName (MkTyName ("bool", 6)) = "primTyName_bool"
  | print_TyName (MkTyName ("ref", 7)) = "primTyName_ref"
  | print_TyName (MkTyName ("list", 8)) = "primTyName_list"
  | print_TyName (MkTyName(tyconname, n)) = "MkTyName(\"" ^ String.toString tyconname ^ "\"," ^ Int.toString n ^ ")"
fun print_Ty (TyVar(_,x)) = "TyVar(" ^ print_TyVar x ^ ")"
  | print_Ty (AnonymousTyVar (_, ref (Unbound (_, level)))) = "AnonymousTyVar(" ^ Int.toString level ^ ")"
  | print_Ty (AnonymousTyVar (_, ref (Link ty))) = "*" ^ print_Ty ty
  | print_Ty (RecordType(_,xs)) = let val xs = Syntax.LabelMap.listItemsi xs
                                  in case Syntax.extractTuple (1, xs) of
                                         NONE => "RecordType " ^ Syntax.print_list (Syntax.print_pair (Syntax.print_Label,print_Ty)) xs
                                       | SOME ys => "TupleType " ^ Syntax.print_list print_Ty ys
                                  end
  | print_Ty (RecordExtType (_, xs, baseTy)) = let val xs = Syntax.LabelMap.listItemsi xs
                                               in "RecordExtType(" ^ Syntax.print_list (Syntax.print_pair (Syntax.print_Label,print_Ty)) xs ^ "," ^ print_Ty baseTy ^ ")"
                                               end
  | print_Ty (TyCon(_,[],MkTyName("int", 0))) = "primTy_int"
  | print_Ty (TyCon(_,[],MkTyName("word", 1))) = "primTy_word"
  | print_Ty (TyCon(_,[],MkTyName("real", 2))) = "primTy_real"
  | print_Ty (TyCon(_,[],MkTyName("string", 3))) = "primTy_string"
  | print_Ty (TyCon(_,[],MkTyName("char", 4))) = "primTy_char"
  | print_Ty (TyCon(_,[],MkTyName("exn", 5))) = "primTy_exn"
  | print_Ty (TyCon(_,[],MkTyName("bool", 6))) = "primTy_bool"
  | print_Ty (TyCon(_,x,y)) = "TyCon(" ^ Syntax.print_list print_Ty x ^ "," ^ print_TyName y ^ ")"
  | print_Ty (FnType(_,x,y)) = "FnType(" ^ print_Ty x ^ "," ^ print_Ty y ^ ")"
fun print_Pat (WildcardPat _) = "WildcardPat"
  | print_Pat (SConPat(_, x, _)) = "SConPat(" ^ Syntax.print_SCon x ^ ")"
  | print_Pat (VarPat(_, vid, ty)) = "VarPat(" ^ print_VId vid ^ "," ^ print_Ty ty ^ ")"
  | print_Pat (TypedPat (_, pat, ty)) = "TypedPat(" ^ print_Pat pat ^ "," ^ print_Ty ty ^ ")"
  | print_Pat (LayeredPat (_, vid, ty, pat)) = "TypedPat(" ^ print_VId vid ^ "," ^ print_Ty ty ^ "," ^ print_Pat pat ^ ")"
  | print_Pat (ConPat { longvid, payload, tyargs, ...}) = "ConPat(" ^ print_LongVId longvid ^ "," ^ Syntax.print_option (Syntax.print_pair (print_Ty, print_Pat)) payload ^ "," ^ Syntax.print_list print_Ty tyargs ^ ")"
  | print_Pat (RecordPat{fields = x, ellipsis = NONE, ...}) = (case Syntax.extractTuple (1, x) of
                                                               NONE => "RecordPat(" ^ Syntax.print_list (Syntax.print_pair (Syntax.print_Label, print_Pat)) x ^ ",NONE)"
                                                             | SOME ys => "TuplePat " ^ Syntax.print_list print_Pat ys
                                                          )
  | print_Pat (RecordPat{fields = x, ellipsis = SOME basePat, ...}) = "RecordPat(" ^ Syntax.print_list (Syntax.print_pair (Syntax.print_Label, print_Pat)) x ^ ",SOME(" ^ print_Pat basePat ^ "))"
  | print_Pat (VectorPat _) = "VectorPat"
(* | print_Pat _ = "<Pat>" *)
fun print_Exp (SConExp(_, x, ty)) = "SConExp(" ^ Syntax.print_SCon x ^ ")"
  | print_Exp (VarExp(_, x, idstatus, tyargs)) = "VarExp(" ^ print_LongVId x ^ "," ^ Syntax.print_IdStatus idstatus ^ "," ^ Syntax.print_list (Syntax.print_pair (print_Ty, Syntax.print_list print_UnaryConstraint)) tyargs ^ ")"
  | print_Exp (RecordExp(_, x)) = (case Syntax.extractTuple (1, x) of
                                       NONE => "RecordExp " ^ Syntax.print_list (Syntax.print_pair (Syntax.print_Label, print_Exp)) x
                                     | SOME ys => "TupleExp " ^ Syntax.print_list print_Exp ys
                                  )
  | print_Exp (RecordExtExp { sourceSpan = _, fields, baseExp, baseTy }) = "RecordExtExp(" ^ Syntax.print_list (Syntax.print_pair (Syntax.print_Label, print_Exp)) fields ^ "," ^ print_Exp baseExp ^ ")"
  | print_Exp (LetInExp(_,decls,x)) = "LetInExp(" ^ Syntax.print_list print_Dec decls ^ "," ^ print_Exp x ^ ")"
  | print_Exp (AppExp(_,x,y)) = "AppExp(" ^ print_Exp x ^ "," ^ print_Exp y ^ ")"
  | print_Exp (TypedExp(_,x,y)) = "TypedExp(" ^ print_Exp x ^ "," ^ print_Ty y ^ ")"
  | print_Exp (HandleExp (_, x, y, _)) = "HandleExp(" ^ print_Exp x ^ "," ^ Syntax.print_list (Syntax.print_pair (print_Pat, print_Exp)) y ^ ")"
  | print_Exp (RaiseExp(_,ty,x)) = "RaiseExp(" ^ print_Ty ty ^ "," ^ print_Exp x ^ ")"
  | print_Exp (IfThenElseExp(_,x,y,z)) = "IfThenElseExp(" ^ print_Exp x ^ "," ^ print_Exp y ^ "," ^ print_Exp z ^ ")"
  | print_Exp (CaseExp { subjectExp, subjectTy, matches, ... }) = "CaseExp(" ^ print_Exp subjectExp ^ "," ^ print_Ty subjectTy ^ "," ^ Syntax.print_list (Syntax.print_pair (print_Pat, print_Exp)) matches ^ ")"
  | print_Exp (FnExp(_,pname,pty,body)) = "FnExp(" ^ print_VId pname ^ "," ^ print_Ty pty ^ "," ^ print_Exp body ^ ")"
  | print_Exp (ProjectionExp { label = label, recordTy = recordTy, fieldTy = fieldTy, ... }) = "ProjectionExp{label=" ^ Syntax.print_Label label ^ ",recordTy=" ^ print_Ty recordTy ^ ",fieldTy=" ^ print_Ty fieldTy ^ "}"
  | print_Exp (ListExp _) = "ListExp"
  | print_Exp (VectorExp _) = "VectorExp"
  | print_Exp (PrimExp _) = "PrimExp"
and print_Dec (ValDec(_,valbinds)) = "ValDec(" ^ Syntax.print_list print_ValBind valbinds ^ ")"
  | print_Dec (RecValDec(_,valbinds)) = "RecValDec(" ^ Syntax.print_list print_ValBind valbinds ^ ")"
  | print_Dec (IgnoreDec _) = "IgnoreDec"
  | print_Dec (TypeDec(_, typbinds)) = "TypeDec(" ^ Syntax.print_list print_TypBind typbinds ^ ")"
  | print_Dec (DatatypeDec(_, datbinds)) = "DatatypeDec(" ^ Syntax.print_list print_DatBind datbinds ^ ")"
  | print_Dec (ExceptionDec(_, exbinds)) = "ExceptionDec"
  | print_Dec (GroupDec _) = "GroupDec"
  | print_Dec (OverloadDec _) = "OverloadDec"
  | print_Dec (EqualityDec _) = "EqualityDec"
  | print_Dec (ValDescDec _) = "ValDescDec"
and print_TypBind (TypBind(_, tyvars, tycon, ty)) = "TypBind(" ^ Syntax.print_list print_TyVar tyvars ^ "," ^ Syntax.print_TyCon tycon ^ "," ^ print_Ty ty ^ ")"
and print_DatBind (DatBind(_, tyvars, tycon, conbinds, _)) = "DatBind(" ^ Syntax.print_list print_TyVar tyvars ^ "," ^ print_TyName tycon ^ "," ^ Syntax.print_list print_ConBind conbinds ^ ")"
and print_ConBind (ConBind(_, vid, NONE, _)) = "ConBind(" ^ print_VId vid ^ ",NONE)"
  | print_ConBind (ConBind(_, vid, SOME ty, _)) = "ConBind(" ^ print_VId vid ^ ",SOME " ^ print_Ty ty ^ ")"
and print_ValBind (TupleBind (_, xs, exp)) = "TupleBind(" ^ Syntax.print_list (Syntax.print_pair (print_VId, print_Ty)) xs ^ "," ^ print_Exp exp ^ ")"
  | print_ValBind (PolyVarBind (_, name, tysc, exp)) = "PolyVarBind(" ^ print_VId name ^ "," ^ print_TypeScheme tysc ^ "," ^ print_Exp exp ^ ")"
and print_TyVarMap print_elem x = Syntax.print_list (Syntax.print_pair (print_TyVar,print_elem)) (TyVarMap.foldri (fn (k,x,ys) => (k,x) :: ys) [] x)
and print_VIdMap print_elem x = Syntax.print_list (Syntax.print_pair (print_VId,print_elem)) (VIdMap.foldri (fn (k,x,ys) => (k,x) :: ys) [] x)
and print_UnaryConstraint (NoField label) = "NoField(" ^ Syntax.print_Label label ^ ")"
  | print_UnaryConstraint IsRecord = "IsEqType"
  | print_UnaryConstraint IsEqType = "IsEqType"
  | print_UnaryConstraint IsIntegral = "IsIntegral"
  | print_UnaryConstraint IsSignedReal = "IsSignedReal"
  | print_UnaryConstraint IsRing = "IsRing"
  | print_UnaryConstraint IsField = "IsField"
  | print_UnaryConstraint IsSigned = "IsSigned"
  | print_UnaryConstraint IsOrdered = "IsOrdered"
  | print_UnaryConstraint IsInt = "IsInt"
  | print_UnaryConstraint IsWord = "IsWord"
  | print_UnaryConstraint IsReal = "IsReal"
  | print_UnaryConstraint IsChar = "IsChar"
  | print_UnaryConstraint IsString = "IsString"
and print_TypeScheme (TypeScheme(tyvars, ty)) = "TypeScheme(" ^ Syntax.print_list (Syntax.print_pair (print_TyVar, Syntax.print_list print_UnaryConstraint)) tyvars ^ "," ^ print_Ty ty ^ ")"
and print_ValEnv env = print_VIdMap (Syntax.print_pair (print_TypeScheme,Syntax.print_IdStatus)) env
fun print_TyVarSet x = Syntax.print_list print_TyVar (TyVarSet.foldr (fn (x,ys) => x :: ys) [] x)
fun print_TyNameMap print_elem x = Syntax.print_list (Syntax.print_pair (print_TyName,print_elem)) (TyNameMap.foldri (fn (k,x,ys) => (k,x) :: ys) [] x)
fun print_TypeFunction (TypeFunction (tyvars, ty)) = "TypeFunction(" ^ Syntax.print_list print_TyVar tyvars ^ "," ^ print_Ty ty ^ ")"
val print_Decs = Syntax.print_list print_Dec
fun print_Constraint(EqConstr(span,ty1,ty2)) = "EqConstr(" ^ print_Ty ty1 ^ "," ^ print_Ty ty2 ^ ")"
  | print_Constraint(UnaryConstraint(span,ty,ct)) = "Unary(" ^ print_Ty ty ^ "," ^ print_UnaryConstraint ct ^ ")"
fun print_Signature { valMap, tyConMap, strMap } = "{valMap=" ^ Syntax.print_list (Syntax.print_pair (Syntax.print_VId, Syntax.print_pair (print_TypeScheme, Syntax.print_IdStatus))) (Syntax.VIdMap.listItemsi valMap) ^ ",tyConMap=..." ^ ",strMap=..." ^ "}"
fun print_PackedSignature { s, bound } = "{s=" ^ print_Signature s ^ ",bound=" ^ Syntax.print_list (fn { tyname, arity, admitsEquality } => "(" ^ print_TyName tyname ^ "," ^ Int.toString arity ^ "," ^ Bool.toString admitsEquality ^ ")") bound ^ "}"
fun print_StrExp (StructExp { sourceSpan, valMap, tyConMap, strMap }) = "StructExp"
  | print_StrExp (StrIdExp (span, longstrid)) = "StrIdExp(" ^ print_LongStrId longstrid ^ ")"
  | print_StrExp (PackedStrExp { sourceSpan, strExp, payloadTypes, packageSig }) = "PackedStrExp(" ^ print_StrExp strExp ^ "," ^ Syntax.print_list print_TypeFunction payloadTypes ^ "," ^ print_PackedSignature packageSig ^ ")"
  | print_StrExp (FunctorAppExp { sourceSpan, funId, argumentTypes, argumentStr, packageSig }) = "FunctorAppExp(" ^ print_FunId funId ^ "," ^ Syntax.print_list (fn { typeFunction, admitsEquality } => "(" ^ print_TypeFunction typeFunction ^ "," ^ Bool.toString admitsEquality ^ ")") argumentTypes ^ "," ^ print_StrExp argumentStr ^ "," ^ print_PackedSignature packageSig ^ ")"
  | print_StrExp (LetInStrExp (span, strdecs, strexp)) = "LetInStrExp(" ^ Syntax.print_list print_StrDec strdecs ^ "," ^ print_StrExp strexp ^ ")"
and print_StrDec (CoreDec (span, dec)) = print_Dec dec
  | print_StrDec (StrBindDec (span, strid, strexp, ps)) = "StrBindDec(" ^ print_StrId strid ^ "," ^ print_StrExp strexp ^ ")"
  | print_StrDec (GroupStrDec (span, strdecs)) = "GroupStrDec" ^ Syntax.print_list print_StrDec strdecs
fun print_TopDec (StrDec strdec) = print_StrDec strdec
  | print_TopDec (FunDec (funid, (typarams, strid, s, strexp))) = "FunDec(" ^ print_FunId funid ^ ",(" ^ Syntax.print_list (fn { tyname, arity, admitsEquality } => "(" ^ print_TyName tyname ^ "," ^ Int.toString arity ^ "," ^ Bool.toString admitsEquality ^ ")") typarams ^ "," ^ print_Signature s ^ "," ^ print_StrExp strexp ^ "))"
end (* structure PrettyPrint *)
open PrettyPrint

(*! val freeTyVarsInTy : TyVarSet.set * Ty -> TyVarSet.set *)
fun freeTyVarsInTy(bound, ty)
    = (case ty of
           TyVar(_,tv) => if TyVarSet.member(bound, tv) then
                              TyVarSet.empty
                          else
                              TyVarSet.singleton tv
         | AnonymousTyVar (_, ref (Link ty)) => freeTyVarsInTy (bound, ty)
         | AnonymousTyVar (_, ref (Unbound _)) => TyVarSet.empty
         | RecordType(_,xs) => Syntax.LabelMap.foldl (fn (ty, set) => TyVarSet.union(freeTyVarsInTy(bound, ty), set)) TyVarSet.empty xs
         | TyCon(_,xs,_) => List.foldl (fn (ty, set) => TyVarSet.union(freeTyVarsInTy(bound, ty), set)) TyVarSet.empty xs
         | FnType(_,s,t) => TyVarSet.union(freeTyVarsInTy(bound, s), freeTyVarsInTy(bound, t))
         | RecordExtType (_, xs, baseTy) => Syntax.LabelMap.foldl (fn (ty, set) => TyVarSet.union (freeTyVarsInTy (bound, ty), set)) (freeTyVarsInTy (bound, baseTy)) xs
      )

(*! val freeAnonymousTyVarsInTy : Ty -> AnonymousTyVar list *)
fun freeAnonymousTyVarsInTy ty
    = (case ty of
           AnonymousTyVar (_, tv as ref (Unbound _)) => [tv]
         | AnonymousTyVar (_, ref (Link ty)) => freeAnonymousTyVarsInTy ty
         | TyVar _ => []
         | RecordType (_, xs) => Syntax.LabelMap.foldl (fn (ty, set) => freeAnonymousTyVarsInTy ty @ set) [] xs
         | TyCon (_, xs, _) => List.foldl (fn (ty, set) => freeAnonymousTyVarsInTy ty @ set) [] xs
         | FnType (_, s, t) => freeAnonymousTyVarsInTy s @ freeAnonymousTyVarsInTy t
         | RecordExtType (_, xs, baseTy) => Syntax.LabelMap.foldl (fn (ty, set) => freeAnonymousTyVarsInTy ty @ set) (freeAnonymousTyVarsInTy baseTy) xs
      )

(*! val applySubstTy : Ty TyVarMap.map -> Ty -> Ty *)
fun applySubstTy subst
    = let fun substTy (ty as TyVar(_, tv'))
              = (case TyVarMap.find(subst, tv') of
                     NONE => ty
                   | SOME replacement => replacement
                )
            | substTy (AnonymousTyVar (_, ref (Link ty))) = substTy ty
            | substTy (ty as AnonymousTyVar (_, ref (Unbound _))) = ty
            | substTy (RecordType(span, fields)) = RecordType (span, Syntax.LabelMap.map substTy fields)
            | substTy (TyCon(span, tyargs, tycon)) = TyCon(span, List.map substTy tyargs, tycon)
            | substTy (FnType(span, ty1, ty2)) = FnType(span, substTy ty1, substTy ty2)
            | substTy (RecordExtType (span, fields, baseTy)) = RecordExtType (span, Syntax.LabelMap.map substTy fields, substTy baseTy)
      in substTy
      end

fun applySubstTyInPat subst
    = let val doTy = applySubstTy subst
          fun doPat (pat as WildcardPat _) = pat
            | doPat (SConPat (span, scon, ty)) = SConPat (span, scon, doTy ty)
            | doPat (VarPat (span, vid, ty)) = VarPat (span, vid, doTy ty)
            | doPat (RecordPat { sourceSpan, fields, ellipsis, wholeRecordType }) = RecordPat { sourceSpan = sourceSpan, fields = List.map (fn (label, pat) => (label, doPat pat)) fields, ellipsis = Option.map doPat ellipsis, wholeRecordType = doTy wholeRecordType }
            | doPat (ConPat { sourceSpan, longvid, payload, tyargs, valueConstructorInfo }) = ConPat { sourceSpan = sourceSpan, longvid = longvid, payload = Option.map (fn (ty, pat) => (doTy ty, doPat pat)) payload, tyargs = List.map doTy tyargs, valueConstructorInfo = valueConstructorInfo }
            | doPat (TypedPat (span, pat, ty)) = TypedPat (span, doPat pat, doTy ty)
            | doPat (LayeredPat (span, vid, ty, pat)) = LayeredPat (span, vid, doTy ty, doPat pat)
            | doPat (VectorPat (span, pats, ellipsis, elemTy)) = VectorPat (span, Vector.map doPat pats, ellipsis, doTy elemTy)
      in doPat
      end

fun applySubstTyInExpOrDec subst
    = let val doTy = applySubstTy subst
          val doPat = applySubstTyInPat subst
          fun doExp (SConExp (span, scon, ty)) = SConExp (span, scon, doTy ty)
            | doExp (VarExp (span, longvid, idstatus, tyargs)) = VarExp (span, longvid, idstatus, List.map (fn (ty, c) => (doTy ty, c)) tyargs)
            | doExp (RecordExp (span, fields)) = RecordExp (span, List.map (fn (label, exp) => (label, doExp exp)) fields)
            | doExp (RecordExtExp { sourceSpan, fields, baseExp, baseTy }) = RecordExtExp { sourceSpan = sourceSpan, fields = List.map (fn (label, exp) => (label, doExp exp)) fields, baseExp = doExp baseExp, baseTy = doTy baseTy }
            | doExp (LetInExp (span, decs, exp)) = LetInExp (span, List.map doDec decs, doExp exp)
            | doExp (AppExp (span, exp1, exp2)) = AppExp (span, doExp exp1, doExp exp2)
            | doExp (TypedExp (span, exp, ty)) = TypedExp (span, doExp exp, doTy ty)
            | doExp (HandleExp (span, exp, matches, resultTy)) = HandleExp (span, doExp exp, List.map (fn (pat, exp) => (doPat pat, doExp exp)) matches, doTy resultTy)
            | doExp (RaiseExp (span, ty, exp)) = RaiseExp (span, doTy ty, doExp exp)
            | doExp (IfThenElseExp (span, exp1, exp2, exp3)) = IfThenElseExp (span, doExp exp1, doExp exp2, doExp exp3)
            | doExp (CaseExp { sourceSpan, subjectExp, subjectTy, matches, matchType, resultTy }) = CaseExp { sourceSpan = sourceSpan, subjectExp = doExp subjectExp, subjectTy = doTy subjectTy, matches = List.map (fn (pat, exp) => (doPat pat, doExp exp)) matches, matchType = matchType, resultTy = doTy resultTy }
            | doExp (FnExp (span, vid, ty, exp)) = FnExp (span, vid, doTy ty, doExp exp)
            | doExp (ProjectionExp { sourceSpan, label, recordTy, fieldTy }) = ProjectionExp { sourceSpan = sourceSpan, label = label, recordTy = doTy recordTy, fieldTy = doTy fieldTy }
            | doExp (ListExp (span, elems, elemTy)) = ListExp (span, Vector.map doExp elems, doTy elemTy)
            | doExp (VectorExp (span, elems, elemTy)) = VectorExp (span, Vector.map doExp elems, doTy elemTy)
            | doExp (PrimExp (span, primOp, tyargs, args)) = PrimExp (span, primOp, Vector.map doTy tyargs, Vector.map doExp args)
          and doDec (ValDec (span, valbinds)) = ValDec (span, List.map doValBind valbinds)
            | doDec (RecValDec (span, valbinds)) = RecValDec (span, List.map doValBind valbinds)
            | doDec (IgnoreDec (span, exp, ty)) = IgnoreDec (span, doExp exp, doTy ty)
            | doDec (TypeDec (span, typbinds)) = let fun doTypBind (TypBind (span, tyvars, tycon, ty)) = let val subst' = List.foldl (fn (tv, s) => if TyVarMap.inDomain (s, tv) then #1 (TyVarMap.remove (s, tv)) else s) subst tyvars
                                                                                                         in TypBind (span, tyvars, tycon, applySubstTy subst' ty)
                                                                                                         end
                                                 in TypeDec (span, List.map doTypBind typbinds)
                                                 end
            | doDec (DatatypeDec (span, datbinds)) = let fun doDatBind (DatBind (span, tyvars, tyname, conbinds, admitsEquality))
                                                             = let val subst' = List.foldl (fn (tv, s) => if TyVarMap.inDomain (s, tv) then #1 (TyVarMap.remove (s, tv)) else s) subst tyvars
                                                                   val doTy' = applySubstTy subst'
                                                               in DatBind (span, tyvars, tyname, List.map (fn ConBind (span, vid, optTy, info) => ConBind (span, vid, Option.map doTy' optTy, info)) conbinds, admitsEquality)
                                                               end
                                                     in DatatypeDec (span, List.map doDatBind datbinds)
                                                     end
            | doDec (ExceptionDec (span, exbinds)) = let fun doExBind (ExBind (span, vid, optTy)) = ExBind (span, vid, Option.map doTy optTy)
                                                           | doExBind (ExReplication (span, vid, longvid, optTy)) = ExReplication (span, vid, longvid, Option.map doTy optTy)
                                                     in ExceptionDec (span, List.map doExBind exbinds)
                                                     end
            | doDec (GroupDec (span, decs)) = GroupDec (span, List.map doDec decs)
            | doDec (OverloadDec (span, class, tyname, map)) = OverloadDec (span, class, tyname, Syntax.OverloadKeyMap.map doExp map)
            | doDec (EqualityDec (span, tyvars, tyname, exp)) = let val subst' = List.foldl (fn (tv, s) => if TyVarMap.inDomain (s, tv) then #1 (TyVarMap.remove (s, tv)) else s) subst tyvars
                                                                in EqualityDec (span, tyvars, tyname, #doExp (applySubstTyInExpOrDec subst') exp)
                                                                end
            | doDec (ValDescDec { sourceSpan, expected = TypeScheme (tyvars, ty), actual = TypeScheme (tyvars', ty'), origin })
              = let val subst' = List.foldl (fn ((tv, _), s) => if TyVarMap.inDomain (s, tv) then #1 (TyVarMap.remove (s, tv)) else s) subst tyvars
                    val subst'' = List.foldl (fn ((tv, _), s) => if TyVarMap.inDomain (s, tv) then #1 (TyVarMap.remove (s, tv)) else s) subst tyvars'
                in ValDescDec { sourceSpan = sourceSpan, expected = TypeScheme (tyvars, applySubstTy subst' ty), actual = TypeScheme (tyvars, applySubstTy subst'' ty'), origin = origin }
                end
          and doValBind (TupleBind (span, binds, exp)) = TupleBind (span, List.map (fn (vid, ty) => (vid, doTy ty)) binds, doExp exp)
            | doValBind (PolyVarBind (span, vid, TypeScheme (tyvars, ty), exp)) = let val subst' = List.foldl (fn ((tv, _), s) => if TyVarMap.inDomain (s, tv) then #1 (TyVarMap.remove (s, tv)) else s) subst tyvars
                                                                                  in PolyVarBind (span, vid, TypeScheme (tyvars, applySubstTy subst' ty), #doExp (applySubstTyInExpOrDec subst') exp)
                                                                                  end
      in { doExp = doExp, doDec = doDec }
      end

fun boundVIdsInPat (WildcardPat _) = VIdSet.empty
  | boundVIdsInPat (SConPat _) = VIdSet.empty
  | boundVIdsInPat (VarPat (span, vid, ty)) = VIdSet.singleton vid
  | boundVIdsInPat (RecordPat { sourceSpan, fields, ellipsis = NONE, wholeRecordType }) = List.foldl (fn ((label, pat), acc) => VIdSet.union (boundVIdsInPat pat, acc)) VIdSet.empty fields
  | boundVIdsInPat (RecordPat { sourceSpan, fields, ellipsis = SOME base, wholeRecordType }) = List.foldl (fn ((label, pat), acc) => VIdSet.union (boundVIdsInPat pat, acc)) (boundVIdsInPat base) fields
  | boundVIdsInPat (ConPat { sourceSpan, longvid, payload = NONE, tyargs, valueConstructorInfo }) = VIdSet.empty
  | boundVIdsInPat (ConPat { sourceSpan, longvid, payload = SOME (ty, pat), tyargs, valueConstructorInfo }) = boundVIdsInPat pat
  | boundVIdsInPat (TypedPat (span, pat, ty)) = boundVIdsInPat pat
  | boundVIdsInPat (LayeredPat (span, vid, ty, pat)) = VIdSet.add (boundVIdsInPat pat, vid)
  | boundVIdsInPat (VectorPat (span, pats, ellipsis, ty)) = Vector.foldl (fn (pat, acc) => VIdSet.union (boundVIdsInPat pat, acc)) VIdSet.empty pats

fun substVId (subst : (SourcePos.span * Syntax.ValueConstructorInfo Syntax.IdStatus * (Ty * UnaryConstraint list) list -> Exp) VIdMap.map)
    = let fun remove' (map, key) = if VIdMap.inDomain (map, key) then
                                       #1 (VIdMap.remove (map, key))
                                   else
                                       map
          fun removeKeys (map, keys) = VIdSet.foldl (fn (key, map) => remove' (map, key)) map keys
          fun boundVIdsInValBinds valbinds = List.foldl (fn (TupleBind (span, vids, exp), acc) => List.foldl (fn ((vid, ty), acc) => VIdSet.add (acc, vid)) acc vids
                                                        | (PolyVarBind (span, vid, tysc, exp), acc) => VIdSet.add (acc, vid)) VIdSet.empty valbinds
          fun doExp (e as SConExp _) = e
            | doExp (e as VarExp (span, MkShortVId vid, idstatus, tyargs)) = (case VIdMap.find (subst, vid) of
                                                                                  NONE => e
                                                                                | SOME s => s (span, idstatus, tyargs)
                                                                             )
            | doExp (e as VarExp (_, MkLongVId _, _, _)) = e
            | doExp (RecordExp (span, fields)) = RecordExp (span, List.map (fn (label, exp) => (label, doExp exp)) fields)
            | doExp (RecordExtExp { sourceSpan, fields, baseExp, baseTy }) = RecordExtExp { sourceSpan = sourceSpan, fields = List.map (fn (label, exp) => (label, doExp exp)) fields, baseExp = doExp baseExp, baseTy = baseTy }
            | doExp (LetInExp (span, decs, exp)) = let val (env, decs) = doDecs decs
                                                       val subst' = removeKeys (subst, env)
                                                   in LetInExp (span, decs, #doExp (substVId subst') exp)
                                                   end
            | doExp (AppExp (span, exp1, exp2)) = AppExp (span, doExp exp1, doExp exp2)
            | doExp (TypedExp (span, exp, ty)) = TypedExp (span, doExp exp, ty)
            | doExp (HandleExp (span, exp, matches, resultTy)) = HandleExp (span, doExp exp, doMatches matches, resultTy)
            | doExp (RaiseExp (span, ty, exp)) = RaiseExp (span, ty, doExp exp)
            | doExp (IfThenElseExp (span, exp1, exp2, exp3)) = IfThenElseExp (span, doExp exp1, doExp exp2, doExp exp3)
            | doExp (CaseExp { sourceSpan, subjectExp, subjectTy, matches, matchType, resultTy }) = CaseExp { sourceSpan = sourceSpan, subjectExp = doExp subjectExp, subjectTy = subjectTy, matches = doMatches matches, matchType = matchType, resultTy = resultTy }
            | doExp (FnExp (span, vid, ty, exp)) = let val subst' = remove' (subst, vid)
                                                   in FnExp (span, vid, ty, #doExp (substVId subst') exp)
                                                   end
            | doExp (e as ProjectionExp { sourceSpan, label, recordTy, fieldTy }) = e
            | doExp (ListExp (span, elems, elemTy)) = ListExp (span, Vector.map doExp elems, elemTy)
            | doExp (VectorExp (span, elems, elemTy)) = VectorExp (span, Vector.map doExp elems, elemTy)
            | doExp (PrimExp (span, primOp, tyargs, args)) = PrimExp (span, primOp, tyargs, Vector.map doExp args)
          and doMatches matches = List.map (fn (pat, exp) => let val subst' = removeKeys (subst, boundVIdsInPat pat)
                                                             in (pat, #doExp (substVId subst') exp)
                                                             end) matches
          and doDec (ValDec (span, valbinds)) = (boundVIdsInValBinds valbinds, ValDec (span, List.map (fn TupleBind (span, vids, exp) => TupleBind (span, vids, doExp exp)
                                                                                                      | PolyVarBind (span, vid, tysc, exp) => PolyVarBind (span, vid, tysc, doExp exp)) valbinds))
            | doDec (RecValDec (span, valbinds)) = let val bound = boundVIdsInValBinds valbinds
                                                       val subst' = removeKeys (subst, bound)
                                                       val doExp' = #doExp (substVId subst')
                                                       val valbinds' = List.map (fn TupleBind (span, vids, exp) => TupleBind (span, vids, doExp' exp)
                                                                                | PolyVarBind (span, vid, tysc, exp) => PolyVarBind (span, vid, tysc, doExp' exp)
                                                                                ) valbinds
                                                   in (bound, RecValDec (span, valbinds'))
                                                   end
            | doDec (IgnoreDec (span, exp, ty)) = (VIdSet.empty, IgnoreDec (span, doExp exp, ty))
            | doDec (d as TypeDec (span, typbinds)) = (VIdSet.empty, d)
            | doDec (d as DatatypeDec (span, datbinds)) = (List.foldl (fn (DatBind (_, _, _, conbinds, admitsEquality), acc) => List.foldl (fn (ConBind (_, vid, _, _), acc) => VIdSet.add (acc, vid)) acc conbinds) VIdSet.empty datbinds, d)
            | doDec (d as ExceptionDec (span, exbinds)) = (List.foldl (fn (ExBind (span, vid, optTy), acc) => VIdSet.add (acc, vid)
                                                                      | (ExReplication (span, vid, longvid, optTy), acc) => VIdSet.add (acc, vid) (* longvid? *)
                                                                      ) VIdSet.empty exbinds, d)
            | doDec (GroupDec (span, decs)) = let val (env, decs) = doDecs decs
                                              in (env, GroupDec (span, decs))
                                              end
            | doDec (OverloadDec (span, class, tyname, map)) = (VIdSet.empty, OverloadDec (span, class, tyname, Syntax.OverloadKeyMap.map doExp map))
            | doDec (EqualityDec (span, tyvars, tyname, exp)) = (VIdSet.empty, EqualityDec (span, tyvars, tyname, doExp exp))
            | doDec (e as ValDescDec _) = (VIdSet.empty, e)
          and doDecs decs = let val (env, decs) = List.foldl (fn (dec, (env, decs)) => let val subst' = removeKeys (subst, env)
                                                                                           val (env', dec) = #doDec (substVId subst') dec
                                                                                       in (VIdSet.union (env', env), dec :: decs)
                                                                                       end) (VIdSet.empty, []) decs
                            in (env, List.rev decs)
                            end
      in { doExp = doExp, doDec = doDec, doDecs = doDecs }
      end

fun forceTy (ty as TyVar _) = ty
  | forceTy (ty as AnonymousTyVar (span, tv)) = (case !tv of
                                                     Unbound _ => ty
                                                   | Link value => forceTy value
                                                )
  | forceTy (RecordType (span, fields)) = RecordType (span, Syntax.LabelMap.map forceTy fields)
  | forceTy (TyCon (span, tyargs, tyname)) = TyCon (span, List.map forceTy tyargs, tyname)
  | forceTy (FnType (span, ty1, ty2)) = FnType (span, forceTy ty1, forceTy ty2)
  | forceTy (RecordExtType (span, fields, baseTy)) = let val fields = Syntax.LabelMap.map forceTy fields
                                                     in case forceTy baseTy of
                                                            RecordType (span', fields') => RecordType (span, Syntax.LabelMap.unionWith #2 (fields', fields)) (* duplication should be an error *)
                                                          | RecordExtType (span', fields', baseTy') => RecordExtType (span, Syntax.LabelMap.unionWith #2 (fields', fields), baseTy') (* duplication should be an error *)
                                                          | baseTy' => RecordExtType (span, fields, baseTy') (* ill-kinded *)
                                                     end

(*! val forceTyIn : { nextTyVar : int ref, nextVId : 'a, matchContext : 'b, messageHandler : 'c, languageOptions : 'd } -> { doExp : Exp -> Exp, doDec : Dec -> Dec, doDecs : Dec list -> Dec list, doTopDec : TopDec -> TopDec, doTopDecs : TopDec list -> TopDec list } *)
fun forceTyIn (ctx : { nextTyVar : int ref, nextVId : 'a, matchContext : 'b, messageHandler : 'c, languageOptions : 'd })
    = let val doTy = forceTy
          fun doTypeScheme (TypeScheme (tyvarsWithConstraints, ty)) = TypeScheme (tyvarsWithConstraints, doTy ty)
          val doValEnv = VIdMap.map (fn (tysc, idstatus) => (doTypeScheme tysc, idstatus))
          fun doExp(SConExp(span, scon, ty)) = SConExp(span, scon, doTy ty)
            | doExp (VarExp (span, longvid, idstatus, tyargs)) = VarExp (span, longvid, idstatus, List.map (fn (ty, cts) => (doTy ty, cts)) tyargs)
            | doExp(RecordExp(span, fields)) = RecordExp(span, Syntax.mapRecordRow doExp fields)
            | doExp(RecordExtExp { sourceSpan, fields, baseExp, baseTy }) = RecordExtExp { sourceSpan = sourceSpan, fields = Syntax.mapRecordRow doExp fields, baseExp = doExp baseExp, baseTy = doTy baseTy }
            | doExp(LetInExp(span, decls, e)) = LetInExp(span, List.map doDec decls, doExp e)
            | doExp(AppExp(span, e1, e2)) = AppExp(span, doExp e1, doExp e2)
            | doExp(TypedExp(span, e, ty)) = TypedExp(span, doExp e, doTy ty)
            | doExp (HandleExp (span, e, matches, resultTy)) = HandleExp (span, doExp e, List.map doMatch matches, resultTy)
            | doExp(RaiseExp(span, ty, e)) = RaiseExp(span, doTy ty, doExp e)
            | doExp(IfThenElseExp(span, e1, e2, e3)) = IfThenElseExp(span, doExp e1, doExp e2, doExp e3)
            | doExp (CaseExp { sourceSpan, subjectExp, subjectTy, matches, matchType, resultTy }) = CaseExp { sourceSpan = sourceSpan, subjectExp = doExp subjectExp, subjectTy = doTy subjectTy, matches = List.map doMatch matches, matchType = matchType, resultTy = doTy resultTy }
            | doExp(FnExp(span, vid, ty, body)) = FnExp(span, vid, doTy ty, doExp body)
            | doExp(ProjectionExp { sourceSpan, label, recordTy, fieldTy }) = ProjectionExp { sourceSpan = sourceSpan, label = label, recordTy = doTy recordTy, fieldTy = doTy fieldTy }
            | doExp(ListExp(span, xs, ty)) = ListExp(span, Vector.map doExp xs, doTy ty)
            | doExp(VectorExp(span, xs, ty)) = VectorExp(span, Vector.map doExp xs, doTy ty)
            | doExp(PrimExp(span, primOp, tyargs, args)) = PrimExp(span, primOp, Vector.map doTy tyargs, Vector.map doExp args)
          and doDec(ValDec(span, valbind)) = ValDec(span, List.map doValBind valbind)
            | doDec(RecValDec(span, valbind)) = RecValDec(span, List.map doValBind valbind)
            | doDec (IgnoreDec (span, exp, ty)) = IgnoreDec (span, doExp exp, doTy ty)
            | doDec(TypeDec(span, typbinds)) = TypeDec(span, List.map doTypBind typbinds)
            | doDec(DatatypeDec(span, datbinds)) = DatatypeDec(span, List.map doDatBind datbinds)
            | doDec(ExceptionDec(span, exbinds)) = ExceptionDec(span, List.map doExBind exbinds)
            | doDec(GroupDec(span, decs)) = GroupDec(span, List.map doDec decs)
            | doDec(OverloadDec(span, class, tyname, map)) = OverloadDec(span, class, tyname, Syntax.OverloadKeyMap.map doExp map)
            | doDec (EqualityDec (span, tyvars, tyname, exp)) = EqualityDec (span, tyvars, tyname, doExp exp)
            | doDec (ValDescDec { sourceSpan, expected = TypeScheme (tyvars, ty), actual = TypeScheme (tyvars', ty'), origin }) = ValDescDec { sourceSpan = sourceSpan, expected = TypeScheme (tyvars, doTy ty) (* should not be needed *), actual = TypeScheme (tyvars', doTy ty'), origin = origin }
          and doValBind(TupleBind(span, xs, exp)) = TupleBind(span, List.map (fn (vid, ty) => (vid, doTy ty)) xs, doExp exp)
            | doValBind (PolyVarBind (span, vid, tysc as TypeScheme (tyvarsWithConstraints, ty), exp)) = PolyVarBind (span, vid, TypeScheme (tyvarsWithConstraints, doTy ty), doExp exp)
          and doMatch(pat, exp) = (doPat pat, doExp exp)
          and doPat(pat as WildcardPat _) = pat
            | doPat(SConPat(span, scon, ty)) = SConPat(span, scon, doTy ty)
            | doPat(VarPat(span, vid, ty)) = VarPat(span, vid, doTy ty)
            | doPat (RecordPat { sourceSpan, fields, ellipsis, wholeRecordType }) = RecordPat { sourceSpan = sourceSpan, fields = Syntax.mapRecordRow doPat fields, ellipsis = Option.map doPat ellipsis, wholeRecordType = doTy wholeRecordType }
            | doPat (ConPat { sourceSpan, longvid, payload, tyargs, valueConstructorInfo }) = ConPat { sourceSpan = sourceSpan, longvid = longvid, payload = Option.map (fn (ty, pat) => (doTy ty, doPat pat)) payload, tyargs = List.map doTy tyargs, valueConstructorInfo = valueConstructorInfo }
            | doPat(TypedPat(span, pat, ty)) = TypedPat(span, doPat pat, doTy ty)
            | doPat(LayeredPat(span, vid, ty, pat)) = LayeredPat(span, vid, doTy ty, doPat pat)
            | doPat(VectorPat(span, pats, ellipsis, elemTy)) = VectorPat(span, Vector.map doPat pats, ellipsis, doTy elemTy)
          and doTypBind (TypBind (span, tyvars, tycon, ty)) = TypBind (span, tyvars, tycon, forceTy ty)
          and doDatBind (DatBind (span, tyvars, tycon, conbinds, eq)) = let fun doConBind (ConBind (span, vid, optTy, info)) = ConBind (span, vid, Option.map doTy optTy, info)
                                                                        in DatBind (span, tyvars, tycon, List.map doConBind conbinds, eq)
                                                                        end
          and doExBind(ExBind(span, vid, optTy)) = ExBind(span, vid, Option.map doTy optTy)
            | doExBind(ExReplication(span, vid, longvid, optTy)) = ExReplication(span, vid, longvid, Option.map doTy optTy)
          fun doTypeStructure { typeFunction = TypeFunction(tyvars, ty), valEnv }
              = { typeFunction = TypeFunction (tyvars, doTy ty)
                , valEnv = Syntax.VIdMap.map (fn (tysc, ids) => (doTypeScheme tysc, ids)) valEnv
                }
          fun doSignature({ valMap, tyConMap, strMap } : Signature) = { valMap = Syntax.VIdMap.map (fn (tysc, ids) => (doTypeScheme tysc, ids)) valMap
                                                                      , tyConMap = Syntax.TyConMap.map doTypeStructure tyConMap
                                                                      , strMap = Syntax.StrIdMap.map (fn MkSignature s => MkSignature (doSignature s)) strMap
                                                                      }
          fun doStrExp(StructExp { sourceSpan, valMap, tyConMap, strMap }) = StructExp { sourceSpan = sourceSpan, valMap = valMap, tyConMap = Syntax.TyConMap.map doTypeStructure tyConMap, strMap = strMap }
            | doStrExp(exp as StrIdExp _) = exp
            | doStrExp(PackedStrExp { sourceSpan, strExp, payloadTypes, packageSig }) = PackedStrExp { sourceSpan = sourceSpan
                                                                                                     , strExp = doStrExp strExp
                                                                                                     , payloadTypes = List.map (fn TypeFunction (tyvars, ty) => TypeFunction (tyvars, doTy ty)) payloadTypes
                                                                                                     , packageSig = { s = doSignature (#s packageSig)
                                                                                                                    , bound = #bound packageSig
                                                                                                                    }
                                                                                                     }
            | doStrExp(FunctorAppExp { sourceSpan, funId, argumentTypes, argumentStr, packageSig })
              = FunctorAppExp { sourceSpan = sourceSpan
                              , funId = funId
                              , argumentTypes = List.map (fn { typeFunction = TypeFunction (tyvars, ty), admitsEquality } => { typeFunction = TypeFunction (tyvars, doTy ty), admitsEquality = admitsEquality }) argumentTypes
                              , argumentStr = doStrExp argumentStr
                              , packageSig = { s = doSignature (#s packageSig)
                                             , bound = #bound packageSig
                                             }
                              }
            | doStrExp(LetInStrExp(span, strdecs, strexp)) = LetInStrExp(span, List.map doStrDec strdecs, doStrExp strexp)
          and doStrDec(CoreDec(span, dec)) = CoreDec(span, doDec dec)
            | doStrDec(StrBindDec(span, strid, strexp, { s, bound })) = StrBindDec(span, strid, doStrExp strexp, { s = doSignature s, bound = bound })
            | doStrDec(GroupStrDec(span, decs)) = GroupStrDec(span, List.map doStrDec decs)
          fun doFunExp(bound, strid, s, strexp) = (bound, strid, doSignature s, doStrExp strexp)
          fun doTopDec(StrDec strdec) = StrDec(doStrDec strdec)
            | doTopDec(FunDec(funid, funexp)) = FunDec(funid, doFunExp funexp)
      in { doExp = doExp
         , doDec = doDec
         , doDecs = List.map doDec
         , doTopDec = doTopDec
         , doTopDecs = List.map doTopDec
         }
      end

(*! val freeTyVarsInPat : 'a * Pat -> AnonymousTyVar list *)
fun freeTyVarsInPat (bound, pat)
    = (case pat of
           WildcardPat _ => []
         | SConPat (_, _, ty) => freeAnonymousTyVarsInTy ty
         | VarPat (_, _, ty) => freeAnonymousTyVarsInTy ty
         | RecordPat { fields = xs, wholeRecordType, ... } => List.foldl (fn ((_, pat), set) => freeTyVarsInPat (bound, pat) @ set) (freeAnonymousTyVarsInTy wholeRecordType) xs
         | ConPat { payload = NONE, tyargs, ... } => List.foldl (fn (ty, set) => freeAnonymousTyVarsInTy ty @ set) [] tyargs
         | ConPat { payload = SOME (ty, pat), tyargs, ... } => List.foldl (fn (ty, set) => freeAnonymousTyVarsInTy ty @ set) (freeAnonymousTyVarsInTy ty @ freeTyVarsInPat (bound, pat)) tyargs
         | TypedPat (_, pat, ty) => freeTyVarsInPat (bound, pat) @ freeAnonymousTyVarsInTy ty
         | LayeredPat (_, _, ty, pat) => freeAnonymousTyVarsInTy ty @ freeTyVarsInPat (bound, pat)
         | VectorPat (_, pats, _, elemTy) => Vector.foldl (fn (pat, set) => freeTyVarsInPat (bound, pat) @ set) (freeAnonymousTyVarsInTy elemTy) pats
      )

(*! val freeTyVarsInExp : 'a * Exp -> AnonymousTyVar list *)
fun freeTyVarsInExp (bound, exp)
    = (case exp of
           SConExp (_, _, ty) => freeAnonymousTyVarsInTy ty
         | VarExp (_, _, _, tyargs) => List.foldl (fn ((ty, cts), set) => freeAnonymousTyVarsInTy ty @ set) [] tyargs
         | RecordExp (_, xs) => List.foldl (fn ((_, exp), set) => freeTyVarsInExp (bound, exp) @ set) [] xs
         | RecordExtExp { sourceSpan = _, fields, baseExp, baseTy } => List.foldl (fn ((_, exp), set) => freeTyVarsInExp (bound, exp) @ set) (freeTyVarsInExp (bound, baseExp) @ freeAnonymousTyVarsInTy baseTy) fields
         | LetInExp (_, decls, exp) => freeTyVarsInDecs (bound, decls) @ freeTyVarsInExp (bound, exp)
         | AppExp (_, exp1, exp2) => freeTyVarsInExp (bound, exp1) @ freeTyVarsInExp (bound, exp2)
         | TypedExp (_, exp, ty) => freeTyVarsInExp (bound, exp) @ freeAnonymousTyVarsInTy ty
         | HandleExp (_, exp, matches, resultTy) => freeTyVarsInExp (bound, exp) @ freeAnonymousTyVarsInTy resultTy @ freeTyVarsInMatches (bound, matches, [])
         | RaiseExp (_, ty, exp) => freeAnonymousTyVarsInTy ty @ freeTyVarsInExp (bound, exp)
         | IfThenElseExp (_, exp1, exp2, exp3) => freeTyVarsInExp (bound, exp1) @ freeTyVarsInExp (bound, exp2) @ freeTyVarsInExp (bound, exp3)
         | CaseExp { sourceSpan = _, subjectExp, subjectTy, matches, matchType = _, resultTy } => freeTyVarsInExp (bound, subjectExp) @ freeAnonymousTyVarsInTy subjectTy @ freeAnonymousTyVarsInTy resultTy @ freeTyVarsInMatches (bound, matches, [])
         | FnExp (_, vid, ty, body) => freeAnonymousTyVarsInTy ty @ freeTyVarsInExp (bound, body)
         | ProjectionExp { recordTy = recordTy, fieldTy = fieldTy, ... } => freeAnonymousTyVarsInTy recordTy @ freeAnonymousTyVarsInTy fieldTy
         | ListExp (_, xs, ty) => Vector.foldl (fn (x, set) => freeTyVarsInExp (bound, x) @ set) (freeAnonymousTyVarsInTy ty) xs
         | VectorExp (_, xs, ty) => Vector.foldl (fn (x, set) => freeTyVarsInExp (bound, x) @ set) (freeAnonymousTyVarsInTy ty) xs
         | PrimExp (_, _, tyargs, args) => let val set = Vector.foldl (fn (ty, set) => set @ freeAnonymousTyVarsInTy ty) [] tyargs
                                           in Vector.foldl (fn (x, set) => set @ freeTyVarsInExp (bound, x)) set args
                                           end
      )
and freeTyVarsInMatches (bound, nil, acc) = acc
  | freeTyVarsInMatches (bound, (pat, exp) :: rest, acc) = freeTyVarsInMatches (bound, rest, acc @ freeTyVarsInPat (bound, pat) @ freeTyVarsInExp (bound, exp))
and freeTyVarsInDecs (bound, decls) = List.foldl (fn (dec, set) => set @ freeTyVarsInDec (bound, dec)) [] decls
and freeTyVarsInDec (bound, dec)
    = (case dec of
           ValDec (_, valbinds) => List.foldl (fn (valbind, acc) => freeTyVarsInValBind (bound, valbind) @ acc) [] valbinds
         | RecValDec (_, valbinds) => List.foldl (fn (valbind, acc) => acc @ freeTyVarsInValBind (bound, valbind)) [] valbinds
         | IgnoreDec (_, exp, ty) => freeTyVarsInExp (bound, exp) @ freeAnonymousTyVarsInTy ty
         | TypeDec (_, typbinds) => List.foldl (fn (typbind, acc) => acc @ freeAnonymousTyVarsInTypBind (bound, typbind)) [] typbinds
         | DatatypeDec (_, datbinds) => List.foldl (fn (datbind, acc) => acc @ freeTyVarsInDatBind (bound, datbind)) [] datbinds
         | ExceptionDec (_, exbinds) => List.foldl (fn (exbind, acc) => acc @ freeTyVarsInExBind (bound, exbind)) [] exbinds
         | GroupDec (_, decs) => freeTyVarsInDecs (bound, decs)
         | OverloadDec (_, class, tyname, map) => Syntax.OverloadKeyMap.foldl (fn (exp, acc) => acc @ freeTyVarsInExp (bound, exp)) [] map
         | EqualityDec (_, typarams, tyname, exp) => freeTyVarsInExp (bound, exp)
         | ValDescDec { sourceSpan, expected = TypeScheme (tyvars, ty), actual = TypeScheme (tyvars', ty'), origin } => freeAnonymousTyVarsInTy ty (* should be empty *) @ freeAnonymousTyVarsInTy ty'
      )
and freeTyVarsInValBind (bound, TupleBind(_, xs, exp)) = List.foldl (fn ((_, ty), acc) => acc @ freeAnonymousTyVarsInTy ty) (freeTyVarsInExp (bound, exp)) xs
  | freeTyVarsInValBind (bound, PolyVarBind(_, vid, TypeScheme(tyvars, ty), exp)) = freeAnonymousTyVarsInTy ty @ freeTyVarsInExp (bound, exp)
and freeAnonymousTyVarsInTypBind (bound, TypBind (_, tyvars, tycon, ty)) = freeAnonymousTyVarsInTy ty
and freeTyVarsInDatBind (bound, DatBind (_, tyvars, tycon, conbinds, _)) = List.foldl (fn (conbind, acc) => acc @ freeTyVarsInConBind (bound, conbind)) [] conbinds
and freeTyVarsInConBind (bound, ConBind (_, vid, NONE, info)) = []
  | freeTyVarsInConBind (bound, ConBind (_, vid, SOME ty, info)) = freeAnonymousTyVarsInTy ty
and freeTyVarsInExBind (bound, ExBind (_, vid, NONE)) = []
  | freeTyVarsInExBind (bound, ExBind (_, vid, SOME ty)) = freeAnonymousTyVarsInTy ty
  | freeTyVarsInExBind (bound, ExReplication (_, _, _, NONE)) = []
  | freeTyVarsInExBind (bound, ExReplication (_, _, _, SOME ty)) = freeAnonymousTyVarsInTy ty
and freeTyVarsInUnaryConstraint (bound, unaryConstraint) = []

(*! val filterVarsInPat : (VId -> bool) -> Pat -> Pat *)
fun filterVarsInPat pred =
    let fun doPat pat = case pat of
                            WildcardPat _ => pat
                          | SConPat _ => pat
                          | VarPat(span, vid, ty) => if pred vid then pat else WildcardPat span
                          | RecordPat { sourceSpan, fields, ellipsis, wholeRecordType } => RecordPat { sourceSpan = sourceSpan, fields = Syntax.mapRecordRow doPat fields, ellipsis = Option.map doPat ellipsis, wholeRecordType = wholeRecordType }
                          | ConPat { payload = NONE, ... } => pat
                          | ConPat { sourceSpan, longvid, payload = SOME (innerTy, innerPat), tyargs, valueConstructorInfo } => ConPat { sourceSpan = sourceSpan, longvid = longvid, payload = SOME (innerTy, doPat innerPat), tyargs = tyargs, valueConstructorInfo = valueConstructorInfo }
                          | TypedPat(span, innerPat, ty) => TypedPat(span, doPat innerPat, ty)
                          | LayeredPat(span, vid, ty, innerPat) => if pred vid then LayeredPat(span, vid, ty, doPat innerPat) else TypedPat(span, doPat innerPat, ty)
                          | VectorPat(span, pats, ellipsis, elemTy) => VectorPat(span, Vector.map doPat pats, ellipsis, elemTy)
    in doPat
    end

(*! val renameVarsInPat : VId VIdMap.map -> Pat -> Pat *)
fun renameVarsInPat m =
    let fun doPat (pat as WildcardPat _) = pat
          | doPat (pat as SConPat _) = pat
          | doPat (pat as VarPat(span, vid, ty)) = (case VIdMap.find(m, vid) of
                                                        NONE => pat
                                                      | SOME repl => VarPat(span, repl, ty)
                                                   )
          | doPat (RecordPat { sourceSpan, fields, ellipsis, wholeRecordType })
            = RecordPat { sourceSpan = sourceSpan
                        , fields = List.map (fn (label, pat) => (label, doPat pat)) fields
                        , ellipsis = Option.map doPat ellipsis
                        , wholeRecordType = wholeRecordType
                        }
          | doPat (ConPat { sourceSpan, longvid, payload, tyargs, valueConstructorInfo }) = ConPat { sourceSpan = sourceSpan, longvid = longvid, payload = Option.map (fn (ty, pat) => (ty, doPat pat)) payload, tyargs = tyargs, valueConstructorInfo = valueConstructorInfo }
          | doPat (TypedPat(span, pat, ty)) = TypedPat(span, doPat pat, ty)
          | doPat (LayeredPat(span, vid, ty, pat)) = LayeredPat(span, case VIdMap.find(m, vid) of
                                                                          NONE => vid
                                                                        | SOME repl => repl
                                                                , ty, doPat pat)
          | doPat (VectorPat(span, pats, ellipsis, elemTy)) = VectorPat(span, Vector.map doPat pats, ellipsis, elemTy)
    in doPat
    end
end; (* structure TypedSyntax *)
