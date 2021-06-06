(*
 * Copyright (c) 2021 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure FSyntax = struct
type TyVar = USyntax.TyVar
type TyCon = USyntax.TyCon
datatype Ty = TyVar of TyVar
            | RecordType of (Syntax.Label * Ty) list
            | TyCon of Ty list * TyCon
            | FnType of Ty * Ty
            | ForallType of TyVar * Ty
            (* TypeFn : (TyVar * Kind) list * Ty *)
            (* ExistsType of (TyVar * Kind) * Ty *)
            | SigType of { valMap : Ty Syntax.VIdMap.map (* id status? *)
                         , strMap : Ty Syntax.StrIdMap.map
                                       (* equality? *)
                                       (* value constructor? *)
                         , exnTags : Syntax.VIdSet.set
                         }
datatype Pat = WildcardPat
             | SConPat of Syntax.SCon
             | VarPat of USyntax.VId * Ty
             | RecordPat of (Syntax.Label * Pat) list * bool
             | ConPat of USyntax.LongVId * Pat option * Ty list
             | LayeredPat of USyntax.VId * Ty * Pat
datatype ConBind = ConBind of USyntax.VId * Ty option
datatype DatBind = DatBind of TyVar list * TyCon * ConBind list
datatype Exp = SConExp of Syntax.SCon
             | VarExp of USyntax.VId
             | RecordExp of (Syntax.Label * Exp) list
             | LetExp of Dec * Exp
             | AppExp of Exp * Exp
             | HandleExp of { body : Exp
                            , exnName : USyntax.VId
                            , handler : Exp
                            }
             | RaiseExp of SourcePos.span * Exp
             | IfThenElseExp of Exp * Exp * Exp
             | CaseExp of SourcePos.span * Exp * Ty * (Pat * Exp) list
             | FnExp of USyntax.VId * Ty * Exp
             | ProjectionExp of { label : Syntax.Label, recordTy : Ty, fieldTy : Ty }
             | ListExp of Exp vector * Ty
             | VectorExp of Exp vector * Ty
             | TyAbsExp of TyVar * Exp
             | TyAppExp of Exp * Ty
             | RecordEqualityExp of (Syntax.Label * Exp) list
             | DataTagExp of Exp (* * TyCon *)
             | DataPayloadExp of Exp (* * USyntax.LongVId * TyCon *)
             | StructExp of { valMap : Exp Syntax.VIdMap.map
                            , strMap : Exp Syntax.StrIdMap.map
                            , exnTagMap : Exp Syntax.VIdMap.map
                            }
             | ValueProjectionExp of Exp * Syntax.VId
             | StructureProjectionExp of Exp * Syntax.StrId
             | ExTagProjectionExp of Exp * Syntax.VId
     (* PackExp *)
     and ValBind = SimpleBind of USyntax.VId * Ty * Exp
                 | TupleBind of (USyntax.VId * Ty) list * Exp
                 (* UnpackingBind *)
     and Dec = ValDec of ValBind
             | RecValDec of ValBind list
             | IgnoreDec of Exp (* val _ = ... *)
             | DatatypeDec of DatBind list
             | ExceptionDec of { conName : USyntax.VId, tagName : USyntax.VId, payloadTy : Ty option }
             | ExportValue of Exp
             | ExportModule of (string * Exp) vector
fun PairType(a, b) = RecordType [(Syntax.NumericLabel 1, a), (Syntax.NumericLabel 2, b)]
fun TuplePat xs = let fun doFields i nil = nil
                        | doFields i (x :: xs) = (Syntax.NumericLabel i, x) :: doFields (i + 1) xs
                  in RecordPat (doFields 1 xs, false)
                  end
fun TupleExp xs = let fun doFields i nil = nil
                        | doFields i (x :: xs) = (Syntax.NumericLabel i, x) :: doFields (i + 1) xs
                  in RecordExp (doFields 1 xs)
                  end
fun strIdToVId(USyntax.MkStrId(name, n)) = USyntax.MkVId(name, n)
fun LongVarExp(USyntax.MkShortVId vid) = VarExp vid
  | LongVarExp(USyntax.MkLongVId(strid0, strids, vid)) = ValueProjectionExp (List.foldl (fn (label, x) => StructureProjectionExp (x, label)) (VarExp (strIdToVId strid0)) strids, vid)
fun AndalsoExp(a, b) = IfThenElseExp(a, b, VarExp(InitialEnv.VId_false))
fun SimplifyingAndalsoExp(a as VarExp(vid), b) = if USyntax.eqVId(vid, InitialEnv.VId_true) then
                                                     b
                                                 else if USyntax.eqVId(vid, InitialEnv.VId_false) then
                                                     a
                                                 else
                                                     AndalsoExp(a, b)
  | SimplifyingAndalsoExp(a, b as VarExp(vid)) = if USyntax.eqVId(vid, InitialEnv.VId_true) then
                                                     a
                                                 else
                                                     AndalsoExp(a, b)
  | SimplifyingAndalsoExp(a, b) = AndalsoExp(a, b)

(* occurCheck : TyVar -> Ty -> bool *)
fun occurCheck tv =
    let fun check (TyVar tv') = USyntax.eqUTyVar(tv, tv')
          | check (RecordType xs) = List.exists (fn (label, ty) => check ty) xs
          | check (TyCon(tyargs, longtycon)) = List.exists check tyargs
          | check (FnType(ty1, ty2)) = check ty1 orelse check ty2
          | check (ForallType(tv', ty)) = if USyntax.eqUTyVar(tv, tv') then
                                              false
                                          else
                                              check ty
          | check (SigType { valMap, strMap, exnTags }) = Syntax.VIdMap.exists check valMap orelse Syntax.StrIdMap.exists check strMap
    in check
    end

(* substituteTy : TyVar * Ty -> Ty -> Ty *)
fun substituteTy (tv, replacement) =
    let fun go (ty as TyVar tv') = if USyntax.eqUTyVar(tv, tv') then
                                       replacement
                                   else
                                       ty
          | go (RecordType fields) = RecordType (Syntax.mapRecordRow go fields)
          | go (TyCon(tyargs, longtycon)) = TyCon(List.map go tyargs, longtycon)
          | go (FnType(ty1, ty2)) = FnType(go ty1, go ty2)
          | go (ty as ForallType(tv', ty')) = if USyntax.eqUTyVar(tv, tv') then
                                                  ty
                                              else if occurCheck tv' replacement then
                                                  (* TODO: generate fresh type variable *)
                                                  let val tv'' = raise Fail "FSyntax.substituteTy: not implemented yet"
                                                  in ForallType(tv'', go (substituteTy (tv', TyVar tv'') ty'))
                                                  end
                                              else
                                                  ForallType(tv', go ty')
          | go (SigType { valMap, strMap, exnTags }) = SigType { valMap = Syntax.VIdMap.map go valMap, strMap = Syntax.StrIdMap.map go strMap, exnTags = exnTags }
    in go
    end

(* substTy : Ty TyVarMap.map -> { doTy : Ty -> Ty, doConBind : ConBind -> ConBind, doPat : Pat -> Pat, doExp : Exp -> Exp, doDec : Dec -> Dec, doDecs : Decs -> Decs } *)
fun substTy (subst : Ty USyntax.TyVarMap.map) =
    let fun doTy (ty as TyVar tv) = (case USyntax.TyVarMap.find(subst, tv) of
                                         NONE => ty
                                       | SOME replacement => replacement
                                    )
          | doTy (RecordType fields) = RecordType (List.map (fn (label, ty) => (label, doTy ty)) fields)
          | doTy (TyCon (tyargs, tycon)) = TyCon (List.map doTy tyargs, tycon)
          | doTy (FnType (ty1, ty2)) = FnType (doTy ty1, doTy ty2)
          | doTy (ForallType (tv, ty)) = if USyntax.TyVarMap.inDomain (subst, tv) then (* TODO: use fresh tyvar if necessary *)
                                             ForallType (tv, #doTy (substTy (#1 (USyntax.TyVarMap.remove (subst, tv)))) ty)
                                         else
                                             ForallType (tv, doTy ty)
          | doTy (SigType { valMap, strMap, exnTags }) = SigType { valMap = Syntax.VIdMap.map doTy valMap, strMap = Syntax.StrIdMap.map doTy strMap, exnTags = exnTags }
        fun doPat (pat as WildcardPat) = pat
          | doPat (pat as SConPat _) = pat
          | doPat (VarPat (vid, ty)) = VarPat (vid, doTy ty)
          | doPat (RecordPat (fields, wildcard)) = RecordPat (List.map (fn (label, pat) => (label, doPat pat)) fields, wildcard)
          | doPat (ConPat (longvid, optPat, tyargs)) = ConPat (longvid, Option.map doPat optPat, List.map doTy tyargs)
          | doPat (LayeredPat (vid, ty, pat)) = LayeredPat (vid, doTy ty, doPat pat)
        fun doConBind (ConBind (vid, optTy)) = ConBind (vid, Option.map doTy optTy)
        fun doDatBind (DatBind (tyvars, tycon, conbinds)) = let val subst' = List.foldl (fn (tv, subst) => if USyntax.TyVarMap.inDomain (subst, tv) then #1 (USyntax.TyVarMap.remove (subst, tv)) else subst) subst tyvars (* TODO: use fresh tyvar if necessary *)
                                                            in DatBind (tyvars, tycon, List.map (#doConBind (substTy subst')) conbinds)
                                                            end
        fun doExp (exp as SConExp _) = exp
          | doExp (exp as VarExp _) = exp
          | doExp (RecordExp fields) = RecordExp (List.map (fn (label, exp) => (label, doExp exp)) fields)
          | doExp (LetExp (dec, exp)) = LetExp (doDec dec, doExp exp)
          | doExp (AppExp (exp1, exp2)) = AppExp (doExp exp1, doExp exp2)
          | doExp (HandleExp { body, exnName, handler }) = HandleExp { body = doExp body, exnName = exnName, handler = doExp handler }
          | doExp (RaiseExp (span, exp)) = RaiseExp (span, doExp exp)
          | doExp (IfThenElseExp (exp1, exp2, exp3)) = IfThenElseExp (doExp exp1, doExp exp2, doExp exp3)
          | doExp (CaseExp (span, exp, ty, matches)) = CaseExp (span, doExp exp, doTy ty, List.map (fn (pat, exp) => (doPat pat, doExp exp)) matches)
          | doExp (FnExp (vid, ty, exp)) = FnExp (vid, doTy ty, doExp exp)
          | doExp (ProjectionExp { label, recordTy, fieldTy }) = ProjectionExp { label = label, recordTy = doTy recordTy, fieldTy = doTy fieldTy }
          | doExp (ListExp (xs, ty)) = ListExp (Vector.map doExp xs, doTy ty)
          | doExp (VectorExp (xs, ty)) = VectorExp (Vector.map doExp xs, doTy ty)
          | doExp (TyAbsExp (tv, exp)) = if USyntax.TyVarMap.inDomain (subst, tv) then (* TODO: use fresh tyvar if necessary *)
                                             TyAbsExp (tv, #doExp (substTy (#1 (USyntax.TyVarMap.remove (subst, tv)))) exp)
                                         else
                                             TyAbsExp (tv, doExp exp)
          | doExp (TyAppExp (exp, ty)) = TyAppExp (doExp exp, doTy ty)
          | doExp (RecordEqualityExp fields) = RecordEqualityExp (List.map (fn (label, exp) => (label, doExp exp)) fields)
          | doExp (DataTagExp exp) = DataTagExp (doExp exp)
          | doExp (DataPayloadExp exp) = DataPayloadExp (doExp exp)
          | doExp (StructExp { valMap, strMap, exnTagMap }) = StructExp { valMap = Syntax.VIdMap.map doExp valMap, strMap = Syntax.StrIdMap.map doExp strMap, exnTagMap = Syntax.VIdMap.map doExp exnTagMap }
          | doExp (ValueProjectionExp (exp, label)) = ValueProjectionExp (doExp exp, label)
          | doExp (StructureProjectionExp (exp, label)) = StructureProjectionExp (doExp exp, label)
          | doExp (ExTagProjectionExp (exp, label)) = ExTagProjectionExp (doExp exp, label)
        and doDec (ValDec valbind) = ValDec (doValBind valbind)
          | doDec (RecValDec valbinds) = RecValDec (List.map doValBind valbinds)
          | doDec (IgnoreDec exp) = IgnoreDec (doExp exp)
          | doDec (DatatypeDec datbinds) = DatatypeDec (List.map doDatBind datbinds)
          | doDec (ExceptionDec { conName, tagName, payloadTy }) = ExceptionDec { conName = conName, tagName = tagName, payloadTy = Option.map doTy payloadTy }
          | doDec (ExportValue exp) = ExportValue (doExp exp)
          | doDec (ExportModule fields) = ExportModule (Vector.map (fn (label, exp) => (label, doExp exp)) fields)
        and doValBind (SimpleBind (vid, ty, exp)) = SimpleBind (vid, doTy ty, doExp exp)
          | doValBind (TupleBind (binds, exp)) = TupleBind (List.map (fn (vid, ty) => (vid, doTy ty)) binds, doExp exp)
    in { doTy = doTy
       , doConBind = doConBind
       , doPat = doPat
       , doExp = doExp
       , doDec = doDec
       , doDecs = List.map doDec
       }
    end

local
    fun isLongStrId(VarExp(USyntax.MkVId(name, n)), USyntax.MkStrId(name', n'), []) = n = n' andalso name = name'
      | isLongStrId(StructureProjectionExp(exp, strid), strid0, stridLast :: strids) = strid = stridLast andalso isLongStrId(exp, strid0, strids)
      | isLongStrId(_, _, _) = false
in
    fun isLongVId(VarExp vid, USyntax.MkShortVId vid') = USyntax.eqVId(vid, vid')
      | isLongVId(ValueProjectionExp(exp, vid), USyntax.MkLongVId(strid0, strids, vid')) = vid = vid' andalso isLongStrId(exp, strid0, List.rev strids)
      | isLongVId(_, _) = false
end

structure PrettyPrint = struct
val print_TyVar = USyntax.print_TyVar
val print_VId = USyntax.print_VId
val print_LongVId = USyntax.print_LongVId
val print_TyCon = USyntax.print_TyCon
fun print_Ty (TyVar x) = "TyVar(" ^ print_TyVar x ^ ")"
  | print_Ty (RecordType xs) = (case Syntax.extractTuple (1, xs) of
                                    NONE => "RecordType " ^ Syntax.print_list (Syntax.print_pair (Syntax.print_Label,print_Ty)) xs
                                  | SOME ys => "TupleType " ^ Syntax.print_list print_Ty ys
                               )
  | print_Ty (TyCon([],USyntax.MkTyCon("int", 0))) = "primTy_int"
  | print_Ty (TyCon([],USyntax.MkTyCon("word", 1))) = "primTy_word"
  | print_Ty (TyCon([],USyntax.MkTyCon("real", 2))) = "primTy_real"
  | print_Ty (TyCon([],USyntax.MkTyCon("string", 3))) = "primTy_string"
  | print_Ty (TyCon([],USyntax.MkTyCon("char", 4))) = "primTy_char"
  | print_Ty (TyCon([],USyntax.MkTyCon("exn", 5))) = "primTy_exn"
  | print_Ty (TyCon([],USyntax.MkTyCon("bool", 6))) = "primTy_bool"
  | print_Ty (TyCon(x,y)) = "TyCon(" ^ Syntax.print_list print_Ty x ^ "," ^ print_TyCon y ^ ")"
  | print_Ty (FnType(x,y)) = "FnType(" ^ print_Ty x ^ "," ^ print_Ty y ^ ")"
  | print_Ty (ForallType(tv,x)) = "ForallType(" ^ print_TyVar tv ^ "," ^ print_Ty x ^ ")"
  | print_Ty (SigType _) = "SigType"
fun print_Pat WildcardPat = "WildcardPat"
  | print_Pat (SConPat x) = "SConPat(" ^ Syntax.print_SCon x ^ ")"
  | print_Pat (VarPat(vid, ty)) = "VarPat(" ^ print_VId vid ^ "," ^ print_Ty ty ^ ")"
  | print_Pat (LayeredPat (vid, ty, pat)) = "TypedPat(" ^ print_VId vid ^ "," ^ print_Ty ty ^ "," ^ print_Pat pat ^ ")"
  | print_Pat (ConPat(longvid, pat, tyargs)) = "ConPat(" ^ print_LongVId longvid ^ "," ^ Syntax.print_option print_Pat pat ^ "," ^ Syntax.print_list print_Ty tyargs ^ ")"
  | print_Pat (RecordPat(x, false)) = (case Syntax.extractTuple (1, x) of
                                           NONE => "RecordPat(" ^ Syntax.print_list (Syntax.print_pair (Syntax.print_Label, print_Pat)) x ^ ",false)"
                                         | SOME ys => "TuplePat " ^ Syntax.print_list print_Pat ys
                                      )
  | print_Pat (RecordPat(x, true)) = "RecordPat(" ^ Syntax.print_list (Syntax.print_pair (Syntax.print_Label, print_Pat)) x ^ ",true)"
fun print_Exp (SConExp x) = "SConExp(" ^ Syntax.print_SCon x ^ ")"
  | print_Exp (VarExp(x)) = "VarExp(" ^ print_VId x ^ ")"
  | print_Exp (RecordExp x) = (case Syntax.extractTuple (1, x) of
                                   NONE => "RecordExp " ^ Syntax.print_list (Syntax.print_pair (Syntax.print_Label, print_Exp)) x
                                 | SOME ys => "TupleExp " ^ Syntax.print_list print_Exp ys
                              )
  | print_Exp (LetExp(dec,x)) = "LetExp(" ^ print_Dec dec ^ "," ^ print_Exp x ^ ")"
  | print_Exp (AppExp(x,y)) = "AppExp(" ^ print_Exp x ^ "," ^ print_Exp y ^ ")"
  | print_Exp (HandleExp{body,exnName,handler}) = "HandleExp{body=" ^ print_Exp body ^ ",exnName=" ^ USyntax.print_VId exnName ^ ",handler=" ^ print_Exp handler ^ ")"
  | print_Exp (RaiseExp(span,x)) = "RaiseExp(" ^ print_Exp x ^ ")"
  | print_Exp (IfThenElseExp(x,y,z)) = "IfThenElseExp(" ^ print_Exp x ^ "," ^ print_Exp y ^ "," ^ print_Exp z ^ ")"
  | print_Exp (CaseExp(_,x,ty,y)) = "CaseExp(" ^ print_Exp x ^ "," ^ print_Ty ty ^ "," ^ Syntax.print_list (Syntax.print_pair (print_Pat,print_Exp)) y ^ ")"
  | print_Exp (FnExp(pname,pty,body)) = "FnExp(" ^ print_VId pname ^ "," ^ print_Ty pty ^ "," ^ print_Exp body ^ ")"
  | print_Exp (ProjectionExp { label = label, recordTy = recordTy, fieldTy = fieldTy }) = "ProjectionExp{label=" ^ Syntax.print_Label label ^ ",recordTy=" ^ print_Ty recordTy ^ ",fieldTy=" ^ print_Ty fieldTy ^ "}"
  | print_Exp (ListExp _) = "ListExp"
  | print_Exp (VectorExp _) = "VectorExp"
  | print_Exp (TyAbsExp(tv, exp)) = "TyAbsExp(" ^ print_TyVar tv ^ "," ^ print_Exp exp ^ ")"
  | print_Exp (TyAppExp(exp, ty)) = "TyAppExp(" ^ print_Exp exp ^ "," ^ print_Ty ty ^ ")"
  | print_Exp (RecordEqualityExp(fields)) = "RecordEqualityExp(" ^ Syntax.print_list (Syntax.print_pair (Syntax.print_Label, print_Exp)) fields ^ ")"
  | print_Exp (DataTagExp exp) = "DataTagExp(" ^ print_Exp exp ^ ")"
  | print_Exp (DataPayloadExp exp) = "DataPayloadExp(" ^ print_Exp exp ^ ")"
  | print_Exp (StructExp _) = "StructExp"
  | print_Exp (ValueProjectionExp _) = "ValueProjectionExp"
  | print_Exp (StructureProjectionExp _) = "StructureProjectionExp"
  | print_Exp (ExTagProjectionExp _) = "ExTagProjectionExp"
and print_ValBind (SimpleBind (v, ty, exp)) = "SimpleBind(" ^ print_VId v ^ "," ^ print_Ty ty ^ "," ^ print_Exp exp ^ ")"
  | print_ValBind (TupleBind (xs, exp)) = "TupleBind(" ^ Syntax.print_list (Syntax.print_pair (print_VId, print_Ty)) xs ^ "," ^ print_Exp exp ^ ")"
and print_Dec (ValDec (valbind)) = "ValDec(" ^ print_ValBind valbind ^ ")"
  | print_Dec (RecValDec (valbinds)) = "RecValDec(" ^ Syntax.print_list print_ValBind valbinds ^ ")"
  | print_Dec (IgnoreDec exp) = "IgnoreDec(" ^ print_Exp exp ^ ")"
  | print_Dec (DatatypeDec datbinds) = "DatatypeDec"
  | print_Dec (ExceptionDec _) = "ExceptionDec"
  | print_Dec (ExportValue _) = "ExportValue"
  | print_Dec (ExportModule _) = "ExportModule"
val print_Decs = Syntax.print_list print_Dec
end (* structure PrettyPrint *)
end (* structure FSyntax *)

structure ToFSyntax = struct
type Context = { nextVId : int ref
               }
type Env = { equalityForTyVarMap : USyntax.VId USyntax.TyVarMap.map
           , equalityForTyConMap : USyntax.LongVId USyntax.TyConMap.map
           , exnTagMap : FSyntax.Exp USyntax.LongVIdMap.map
           }
val initialEnv : Env = { equalityForTyVarMap = USyntax.TyVarMap.empty
                       , equalityForTyConMap = let open Typing InitialEnv
                                               in List.foldl USyntax.TyConMap.insert' USyntax.TyConMap.empty
                                                             [(primTyCon_int, VId_EQUAL_int)
                                                             ,(primTyCon_word, VId_EQUAL_word)
                                                             ,(primTyCon_string, VId_EQUAL_string)
                                                             ,(primTyCon_char, VId_EQUAL_char)
                                                             ,(primTyCon_bool, VId_EQUAL_bool)
                                                             ,(primTyCon_list, VId_EQUAL_list)
                                                             ,(primTyCon_vector, VId_EQUAL_vector)
                                                             ]
                                               end
                       , exnTagMap = let open InitialEnv
                                     in List.foldl USyntax.LongVIdMap.insert' USyntax.LongVIdMap.empty
                                                   [(LongVId_Match, FSyntax.LongVarExp VId_Match_tag)
                                                   ,(LongVId_Bind, FSyntax.LongVarExp VId_Bind_tag)
                                                   ,(LongVId_Div, FSyntax.LongVarExp VId_Div_tag)
                                                   ,(LongVId_Overflow, FSyntax.LongVarExp VId_Overflow_tag)
                                                   ,(LongVId_Size, FSyntax.LongVarExp VId_Size_tag)
                                                   ,(LongVId_Subscript, FSyntax.LongVarExp VId_Subscript_tag)
                                                   ,(LongVId_Fail, FSyntax.LongVarExp VId_Fail_tag)
                                                   ]
                                     end
                       }
fun mergeEnv(env1 : Env, env2 : Env)
    = { equalityForTyVarMap = USyntax.TyVarMap.unionWith #2 (#equalityForTyVarMap env1, #equalityForTyVarMap env2)
      , equalityForTyConMap = USyntax.TyConMap.unionWith #2 (#equalityForTyConMap env1, #equalityForTyConMap env2)
      , exnTagMap = USyntax.LongVIdMap.unionWith #2 (#exnTagMap env1, #exnTagMap env2)
      }

fun updateEqualityForTyVarMap(f, env : Env) = { equalityForTyVarMap = f (#equalityForTyVarMap env)
                                              , equalityForTyConMap = #equalityForTyConMap env
                                              , exnTagMap = #exnTagMap env
                                              }

fun freshVId(ctx : Context, name: string) = let val n = !(#nextVId ctx)
                                            in #nextVId ctx := n + 1
                                             ; USyntax.MkVId(name, n)
                                            end

fun LongStrIdExp(USyntax.MkLongStrId(strid0, strids)) = List.foldl (fn (label, x) => FSyntax.StructureProjectionExp (x, label)) (FSyntax.VarExp (FSyntax.strIdToVId strid0)) strids

local structure U = USyntax
      structure F = FSyntax
      (* toFTy : Context * Env * USyntax.Ty -> FSyntax.Ty *)
      (* toFPat : Context * Env * USyntax.Pat -> unit USyntax.VIdMap.map * FSyntax.Pat *)
      (* toFExp : Context * Env * USyntax.Exp -> FSyntax.Exp *)
      (* toFDecs : Context * Env * USyntax.Dec list -> Env * FSyntax.Dec list *)
      (* getEquality : Context * Env * USyntax.Ty -> FSyntax.Exp *)
      val overloads = let open Typing InitialEnv
                      in List.foldl (fn ((vid, xs), m) => USyntax.VIdMap.insert (m, vid, List.foldl USyntax.TyConMap.insert' USyntax.TyConMap.empty xs)) USyntax.VIdMap.empty
                                    [(VId_abs, [(primTyCon_int, VId_Int_abs)
                                               ,(primTyCon_real, VId_Real_abs)
                                               ]
                                     )
                                    ,(VId_TILDE, [(primTyCon_int, VId_Int_TILDE)
                                                 ,(primTyCon_word, VId_Word_TILDE)
                                                 ,(primTyCon_real, VId_Real_TILDE)
                                                 ]
                                     )
                                    ,(VId_div, [(primTyCon_int, VId_Int_div)
                                               ,(primTyCon_word, VId_Word_div)
                                               ]
                                     )
                                    ,(VId_mod, [(primTyCon_int, VId_Int_mod)
                                               ,(primTyCon_word, VId_Word_mod)
                                               ]
                                     )
                                    ,(VId_TIMES, [(primTyCon_int, VId_Int_TIMES)
                                                 ,(primTyCon_word, VId_Word_TIMES)
                                                 ,(primTyCon_real, VId_Real_TIMES)
                                                 ]
                                     )
                                    ,(VId_DIVIDE, [(primTyCon_real, VId_Real_DIVIDE)
                                                  ]
                                     )
                                    ,(VId_PLUS, [(primTyCon_int, VId_Int_PLUS)
                                                ,(primTyCon_word, VId_Word_PLUS)
                                                ,(primTyCon_real, VId_Real_PLUS)
                                                ]
                                     )
                                    ,(VId_MINUS, [(primTyCon_int, VId_Int_MINUS)
                                                 ,(primTyCon_word, VId_Word_MINUS)
                                                 ,(primTyCon_real, VId_Real_MINUS)
                                                 ]
                                     )
                                    ,(VId_LT, [(primTyCon_int, VId_Int_LT)
                                              ,(primTyCon_word, VId_Word_LT)
                                              ,(primTyCon_real, VId_Real_LT)
                                              ,(primTyCon_string, VId_String_LT)
                                              ,(primTyCon_char, VId_Char_LT)
                                              ]
                                     )
                                    ,(VId_LE, [(primTyCon_int, VId_Int_LE)
                                              ,(primTyCon_word, VId_Word_LE)
                                              ,(primTyCon_real, VId_Real_LE)
                                              ,(primTyCon_string, VId_String_LE)
                                              ,(primTyCon_char, VId_Char_LE)
                                              ]
                                     )
                                    ,(VId_GT, [(primTyCon_int, VId_Int_GT)
                                              ,(primTyCon_word, VId_Word_GT)
                                              ,(primTyCon_real, VId_Real_GT)
                                              ,(primTyCon_string, VId_String_GT)
                                              ,(primTyCon_char, VId_Char_GT)
                                              ]
                                     )
                                    ,(VId_GE, [(primTyCon_int, VId_Int_GE)
                                              ,(primTyCon_word, VId_Word_GE)
                                              ,(primTyCon_real, VId_Real_GE)
                                              ,(primTyCon_string, VId_String_GE)
                                              ,(primTyCon_char, VId_Char_GE)
                                              ]
                                     )
                                    ]
                      end
in
fun toFTy(ctx : Context, env : Env, U.TyVar(span, tv)) = F.TyVar tv
  | toFTy(ctx, env, U.RecordType(span, fields)) = let fun doField(label, ty) = (label, toFTy(ctx, env, ty))
                                                  in F.RecordType (List.map doField fields)
                                                  end
  | toFTy(ctx, env, U.TyCon(span, tyargs, longtycon)) = let fun doTy ty = toFTy(ctx, env, ty)
                                                        in F.TyCon(List.map doTy tyargs, longtycon)
                                                        end
  | toFTy(ctx, env, U.FnType(span, paramTy, resultTy)) = let fun doTy ty = toFTy(ctx, env, ty)
                                                         in F.FnType(doTy paramTy, doTy resultTy)
                                                         end
and toFPat(ctx, env, U.WildcardPat span) = (USyntax.VIdMap.empty, F.WildcardPat)
  | toFPat(ctx, env, U.SConPat(span, scon)) = (USyntax.VIdMap.empty, F.SConPat(scon))
  | toFPat(ctx, env, U.VarPat(span, vid, ty)) = (USyntax.VIdMap.empty, F.VarPat(vid, toFTy(ctx, env, ty))) (* TODO *)
  | toFPat(ctx, env, U.RecordPat{sourceSpan=span, fields, wildcard}) = let fun doField(label, pat) = let val (_, pat') = toFPat(ctx, env, pat)
                                                                                                     in (label, pat')
                                                                                                     end
                                                                       in (USyntax.VIdMap.empty, F.RecordPat(List.map doField fields, wildcard)) (* TODO *)
                                                                       end
  | toFPat(ctx, env, U.InstantiatedConPat { sourceSpan = span, longvid, payload = NONE, tyargs, isSoleConstructor })
    = (USyntax.VIdMap.empty, F.ConPat(longvid, NONE, List.map (fn ty => toFTy(ctx, env, ty)) tyargs))
  | toFPat(ctx, env, U.InstantiatedConPat { sourceSpan = span, longvid, payload = SOME payloadPat, tyargs, isSoleConstructor })
    = let val (m, payloadPat') = toFPat(ctx, env, payloadPat)
      in (USyntax.VIdMap.empty, F.ConPat(longvid, SOME payloadPat', List.map (fn ty => toFTy(ctx, env, ty)) tyargs))
      end
  | toFPat(ctx, env, U.TypedPat(_, pat, _)) = toFPat(ctx, env, pat)
  | toFPat(ctx, env, U.LayeredPat(span, vid, ty, innerPat)) = let val (m, innerPat') = toFPat(ctx, env, innerPat)
                                                              in (USyntax.VIdMap.empty, F.LayeredPat(vid, toFTy(ctx, env, ty), innerPat')) (* TODO *)
                                                              end
and toFExp(ctx, env, U.SConExp(span, scon)) = F.SConExp(scon)
  | toFExp(ctx, env, U.InstantiatedVarExp(span, longvid as USyntax.MkShortVId vid, _, [(tyarg, cts)]))
    = if U.eqVId(vid, InitialEnv.VId_EQUAL) then
          getEquality(ctx, env, tyarg)
      else
          (case USyntax.VIdMap.find(overloads, vid) of
               SOME ov => (case tyarg of
                               U.TyCon(_, [], tycon) => (case USyntax.TyConMap.find (ov, tycon) of
                                                             SOME vid' => F.LongVarExp(vid')
                                                           | NONE => raise Fail ("invalid use of " ^ USyntax.print_VId vid)
                                                        )
                             | _ => raise Fail ("invalid use of " ^ USyntax.print_VId vid)
                          )
             | NONE => if List.exists (fn USyntax.IsEqType _ => true | _ => false) cts then
                           F.AppExp(F.TyAppExp(F.LongVarExp(longvid), toFTy(ctx, env, tyarg)), getEquality(ctx, env, tyarg))
                       else
                           F.TyAppExp(F.LongVarExp(longvid), toFTy(ctx, env, tyarg))
          )
  | toFExp(ctx, env, U.InstantiatedVarExp(span, longvid, _, tyargs))
    = List.foldl (fn ((ty, cts), e) =>
                     if List.exists (fn USyntax.IsEqType _ => true | _ => false) cts then
                         F.AppExp(F.TyAppExp(e, toFTy(ctx, env, ty)), getEquality(ctx, env, ty))
                     else
                         F.TyAppExp(e, toFTy(ctx, env, ty))
                 ) (F.LongVarExp(longvid)) tyargs
  | toFExp(ctx, env, U.RecordExp(span, fields)) = let fun doField (label, e) = (label, toFExp(ctx, env, e))
                                                  in F.RecordExp (List.map doField fields)
                                                  end
  | toFExp(ctx, env, U.LetInExp(span, decs, e))
    = let val (env, decs) = toFDecs(ctx, env, decs)
      in List.foldr F.LetExp (toFExp(ctx, env, e)) decs
      end
  | toFExp(ctx, env, U.AppExp(span, e1, e2)) = F.AppExp(toFExp(ctx, env, e1), toFExp(ctx, env, e2))
  | toFExp(ctx, env, U.TypedExp(span, exp, _)) = toFExp(ctx, env, exp)
  | toFExp(ctx, env, U.IfThenElseExp(span, e1, e2, e3)) = F.IfThenElseExp(toFExp(ctx, env, e1), toFExp(ctx, env, e2), toFExp(ctx, env, e3))
  | toFExp(ctx, env, U.CaseExp(span, e, ty, matches))
    = let fun doMatch(pat, exp) = let val (_, pat') = toFPat(ctx, env, pat)
                                  in (pat', toFExp(ctx, env, exp)) (* TODO: environment *)
                                  end
      in F.CaseExp(span, toFExp(ctx, env, e), toFTy(ctx, env, ty), List.map doMatch matches)
      end
  | toFExp(ctx, env, U.FnExp(span, vid, ty, body))
    = let val env' = env (* TODO *)
      in F.FnExp(vid, toFTy(ctx, env, ty), toFExp(ctx, env', body))
      end
  | toFExp(ctx, env, U.ProjectionExp { sourceSpan = span, label = label, recordTy = recordTy, fieldTy = fieldTy })
    = F.ProjectionExp { label = label, recordTy = toFTy(ctx, env, recordTy), fieldTy = toFTy(ctx, env, fieldTy) }
  | toFExp(ctx, env, U.HandleExp(span, exp, matches))
    = let val exnName = freshVId(ctx, "exn")
          val exnTy = F.TyCon([], Typing.primTyCon_exn)
          fun doMatch(pat, exp) = let val (_, pat') = toFPat(ctx, env, pat)
                                  in (pat', toFExp(ctx, env, exp)) (* TODO: environment *)
                                  end
          fun isExhaustive F.WildcardPat = true
            | isExhaustive (F.SConPat _) = false
            | isExhaustive (F.VarPat _) = true
            | isExhaustive (F.RecordPat _) = false (* exn is not a record *)
            | isExhaustive (F.ConPat _) = false (* exn is open *)
            | isExhaustive (F.LayeredPat (_, _, pat)) = isExhaustive pat
          val matches' = List.map doMatch matches
          val matches'' = if List.exists (fn (pat, _) => isExhaustive pat) matches' then
                              matches'
                          else
                              matches' @ [(F.WildcardPat, F.RaiseExp(SourcePos.nullSpan, F.VarExp(exnName)))]
      in F.HandleExp { body = toFExp(ctx, env, exp)
                     , exnName = exnName
                     , handler = F.CaseExp(SourcePos.nullSpan, F.VarExp(exnName), exnTy, matches'')
                     }
      end
  | toFExp(ctx, env, U.RaiseExp(span, exp)) = F.RaiseExp(span, toFExp(ctx, env, exp))
  | toFExp(ctx, env, U.ListExp(span, xs, ty)) = F.ListExp(Vector.map (fn x => toFExp(ctx, env, x)) xs, toFTy(ctx, env, ty))
and doValBind ctx env (U.TupleBind (span, vars, exp)) = F.TupleBind (List.map (fn (vid,ty) => (vid, toFTy(ctx, env, ty))) vars, toFExp(ctx, env, exp))
  | doValBind ctx env (U.PolyVarBind (span, vid, U.TypeScheme(tvs, ty), exp))
    = let val ty0 = toFTy (ctx, env, ty)
          val ty' = List.foldr (fn ((tv,cts),ty1) =>
                                   case cts of
                                       [] => F.ForallType (tv, ty1)
                                     | [U.IsEqType _] => F.ForallType (tv, F.FnType (F.FnType (F.PairType (F.TyVar tv, F.TyVar tv), F.TyCon([], Typing.primTyCon_bool)), ty1))
                                     | _ => raise Fail "invalid type constraint"
                               ) ty0 tvs
          fun doExp (env', [])
              = toFExp(ctx, env', exp)
            | doExp (env', (tv,cts) :: rest)
              = (case cts of
                     [] => F.TyAbsExp (tv, doExp (env', rest))
                   | [U.IsEqType _] => let val vid = freshVId(ctx, "eq")
                                           val eqTy = F.FnType (F.PairType (F.TyVar tv, F.TyVar tv), F.TyCon([], Typing.primTyCon_bool))
                                           val env'' = updateEqualityForTyVarMap(fn m => USyntax.TyVarMap.insert(m, tv, vid), env')
                                       in F.TyAbsExp (tv, F.FnExp(vid, eqTy, doExp(env'', rest)))
                                       end
                   | _ => raise Fail "invalid type constraint"
                )
      in F.SimpleBind (vid, ty', doExp(env, tvs))
      end
and typeSchemeToTy(ctx, env, USyntax.TypeScheme(vars, ty))
    = let fun go env [] = toFTy(ctx, env, ty)
            | go env ((tv, []) :: xs) = let val env' = env (* TODO *)
                                        in F.ForallType(tv, go env' xs)
                                        end
            | go env ((tv, [U.IsEqType _]) :: xs) = let val env' = env (* TODO *)
                                                        val eqTy = F.FnType(F.PairType(F.TyVar tv, F.TyVar tv), F.TyCon([], Typing.primTyCon_bool))
                                                    in F.ForallType(tv, F.FnType(eqTy, go env' xs))
                                                    end
            | go env ((tv, _) :: xs) = raise Fail "invalid type constraint"
      in go env vars
      end
and getEquality(ctx, env, U.TyCon(span, [tyarg], tycon))
    = if U.eqUTyCon(tycon, Typing.primTyCon_ref) then
          F.TyAppExp(F.LongVarExp(InitialEnv.VId_EQUAL_ref), toFTy(ctx, env, tyarg))
      else if U.eqUTyCon(tycon, Typing.primTyCon_array) then
          F.TyAppExp(F.LongVarExp(InitialEnv.VId_EQUAL_array), toFTy(ctx, env, tyarg))
      else
          (case USyntax.TyConMap.find(#equalityForTyConMap env, tycon) of
               NONE => raise Fail (USyntax.PrettyPrint.print_TyCon tycon ^ " does not admit equality")
             | SOME longvid => F.AppExp(F.TyAppExp(F.LongVarExp(longvid), toFTy(ctx, env, tyarg)), getEquality(ctx, env, tyarg))
          )
  | getEquality (ctx, env, U.TyCon(span, tyargs, tycon)) = (case USyntax.TyConMap.find(#equalityForTyConMap env, tycon) of
                                                                NONE => raise Fail (USyntax.PrettyPrint.print_TyCon tycon ^ " does not admit equality")
                                                              | SOME longvid => let val typesApplied = List.foldl (fn (tyarg, exp) => F.TyAppExp(exp, toFTy(ctx, env, tyarg))) (F.LongVarExp(longvid)) tyargs
                                                                                in List.foldl (fn (tyarg, exp) => F.AppExp(exp, getEquality(ctx, env, tyarg))) typesApplied tyargs
                                                                                end
                                                           )
  | getEquality (ctx, env, U.TyVar(span, tv)) = (case USyntax.TyVarMap.find(#equalityForTyVarMap env, tv) of
                                                     NONE => raise Fail ("equality for the type variable not found: " ^ USyntax.PrettyPrint.print_TyVar tv)
                                                   | SOME vid => F.VarExp(vid)
                                                )
  | getEquality (ctx, env, U.RecordType(span, fields)) = let fun doField (label, ty) = (label, getEquality(ctx, env, ty))
                                                         in F.RecordEqualityExp (List.map doField fields)
                                                         end
  | getEquality (ctx, env, U.FnType _) = raise Fail "functions are not equatable; this should have been a type error"
and toFDecs(ctx, env, []) = (env, [])
  | toFDecs(ctx, env, U.ValDec'(span, valbinds) :: decs)
    = let val dec = List.map (fn valbind => F.ValDec (doValBind ctx env valbind)) valbinds
          val (env, decs) = toFDecs (ctx, env, decs)
      in (env, dec @ decs)
      end
  | toFDecs(ctx, env, U.RecValDec'(span, valbinds) :: decs)
    = let val dec = F.RecValDec (List.map (doValBind ctx env) valbinds)
          val (env, decs) = toFDecs (ctx, env, decs)
      in (env, dec :: decs)
      end
  | toFDecs(ctx, env, U.TypeDec(span, typbinds) :: decs) = toFDecs(ctx, env, decs)
  | toFDecs(ctx, env, U.DatatypeDec(span, datbinds) :: decs)
    = let val dec = F.DatatypeDec (List.map (fn datbind => doDatBind(ctx, env, datbind)) datbinds)
          val (env, valbinds) = genEqualitiesForDatatypes(ctx, env, datbinds)
          val (env, decs) = toFDecs(ctx, env, decs)
      in (env, dec :: (if List.null valbinds then decs else F.RecValDec valbinds :: decs))
      end
  | toFDecs(ctx, env as { exnTagMap, ... }, U.ExceptionDec(span, exbinds) :: decs)
    = let val (exnTagMap, exbinds) = List.foldr (fn (U.ExBind(span, vid as USyntax.MkVId(name, _), optTy), (exnTagMap, xs)) =>
                                                    let val tag = freshVId(ctx, name)
                                                    in ( USyntax.LongVIdMap.insert(exnTagMap, U.MkShortVId(vid), F.VarExp(tag))
                                                       , F.ExceptionDec { conName = vid
                                                                        , tagName = tag
                                                                        , payloadTy = case optTy of
                                                                                          NONE => NONE 
                                                                                        | SOME ty => SOME (toFTy(ctx, env, ty))
                                                                        } :: xs
                                                       )
                                                    end
                                                | (U.ExReplication(span, vid, longvid), (exnTagMap, xs)) =>
                                                  (case USyntax.LongVIdMap.find(#exnTagMap env, longvid) of
                                                       SOME exp => (USyntax.LongVIdMap.insert(exnTagMap, U.MkShortVId(vid), exp), F.ValDec(F.SimpleBind(vid, (* TODO *) F.TyCon([], Typing.primTyCon_exn), F.LongVarExp longvid)) ::xs)
                                                     | NONE => raise Fail ("exception not found: " ^ USyntax.print_LongVId longvid)
                                                  )
                                                ) (exnTagMap, []) exbinds
          val env = { equalityForTyVarMap = #equalityForTyVarMap env
                    , equalityForTyConMap = #equalityForTyConMap env
                    , exnTagMap = exnTagMap
                    }
          val (env, decs) = toFDecs(ctx, env, decs)
      in (env, exbinds @ decs)
      end
and doDatBind(ctx, env, U.DatBind(span, tyvars, tycon, conbinds, _)) = F.DatBind(tyvars, tycon, List.map (fn conbind => doConBind(ctx, env, conbind)) conbinds)
and doConBind(ctx, env, U.ConBind(span, vid, NONE)) = F.ConBind(vid, NONE)
  | doConBind(ctx, env, U.ConBind(span, vid, SOME ty)) = F.ConBind(vid, SOME (toFTy(ctx, env, ty)))
and genEqualitiesForDatatypes(ctx, env, datbinds) : Env * F.ValBind list
    = let val nameMap = List.foldl (fn (U.DatBind(span, tyvars, tycon as USyntax.MkTyCon(name, _), conbinds, true), map) => USyntax.TyConMap.insert(map, tycon, freshVId(ctx, "EQUAL" ^ name))
                                   | (_, map) => map) USyntax.TyConMap.empty datbinds
          val env' = { equalityForTyVarMap = #equalityForTyVarMap env
                     , equalityForTyConMap = USyntax.TyConMap.unionWith #2 (#equalityForTyConMap env, USyntax.TyConMap.map U.MkShortVId nameMap)
                     , exnTagMap = #exnTagMap env
                     }
          fun doDatBind(U.DatBind(span, tyvars, tycon, conbinds, true), valbinds)
              = let val vid = USyntax.TyConMap.lookup(nameMap, tycon)
                    fun eqTy t = F.FnType (F.PairType (t, t), F.TyCon([], Typing.primTyCon_bool))
                    val tyvars'' = List.map F.TyVar tyvars
                    val ty = List.foldr (fn (tv, ty) => F.FnType(eqTy (F.TyVar tv), ty)) (eqTy (F.TyCon(tyvars'', tycon))) tyvars
                    val ty = List.foldr F.ForallType ty tyvars
                    val tyvars' = List.map (fn tv => (tv, freshVId(ctx, "eq"))) tyvars
                    val eqForTyVars = List.foldl USyntax.TyVarMap.insert' USyntax.TyVarMap.empty tyvars'
                    val env'' = { equalityForTyVarMap = USyntax.TyVarMap.unionWith #2 (#equalityForTyVarMap env', eqForTyVars)
                                , equalityForTyConMap = #equalityForTyConMap env'
                                , exnTagMap = #exnTagMap env'
                                }
                    val body = let val param = freshVId(ctx, "p")
                                   val paramTy = let val ty = F.TyCon(tyvars'', tycon)
                                                 in F.PairType(ty, ty)
                                                 end
                               in F.FnExp ( param
                                          , paramTy
                                          , F.CaseExp ( SourcePos.nullSpan
                                                      , F.VarExp(param)
                                                      , paramTy
                                                      , List.foldr (fn (U.ConBind (span, conName, NONE), rest) =>
                                                                       let val conPat = F.ConPat(U.MkShortVId(conName), NONE, tyvars'')
                                                                       in ( F.TuplePat [conPat, conPat]
                                                                          , F.VarExp(InitialEnv.VId_true)
                                                                          ) :: rest
                                                                       end
                                                                   | (U.ConBind (span, conName, SOME payloadTy), rest) =>
                                                                     let val payload1 = freshVId(ctx, "a")
                                                                         val payload2 = freshVId(ctx, "b")
                                                                         val payloadEq = getEquality(ctx, env'', payloadTy)
                                                                         val payloadTy = toFTy(ctx, env, payloadTy)
                                                                     in ( F.TuplePat [F.ConPat(U.MkShortVId(conName), SOME (F.VarPat(payload1, payloadTy)), tyvars''), F.ConPat(U.MkShortVId(conName), SOME (F.VarPat(payload2, payloadTy)), tyvars'')]
                                                                        , F.AppExp(payloadEq, F.TupleExp [F.VarExp(payload1), F.VarExp(payload2)])
                                                                        ) :: rest
                                                                     end
                                                                   )
                                                                   [(F.WildcardPat, F.VarExp(InitialEnv.VId_false))]
                                                                   conbinds
                                                      )
                                          )
                               end
                    val body = List.foldr (fn ((tv, eqParam), ty) => let val paramTy = eqTy (F.TyVar tv)
                                                                     in F.FnExp ( eqParam
                                                                                , paramTy
                                                                                , body
                                                                                )
                                                                     end) body tyvars'
                    val body = List.foldr F.TyAbsExp body tyvars
                in F.SimpleBind (vid, ty, body) :: valbinds
                end
            | doDatBind(_, valbinds) = valbinds
      in (env', List.foldr doDatBind [] datbinds)
      end
fun signatureToTy(ctx, env, { valMap, tyConMap, strMap, variables } : U.Signature)
    = let val exnTags = Syntax.VIdMap.foldli (fn (vid, (tysc, ids), set) => if ids = Syntax.ExceptionConstructor then
                                                                                Syntax.VIdSet.add(set, vid)
                                                                            else
                                                                                set
                                             ) Syntax.VIdSet.empty valMap
      in F.SigType { valMap = Syntax.VIdMap.map (fn (tysc, ids) => typeSchemeToTy(ctx, env, tysc)) valMap
                   , strMap = Syntax.StrIdMap.map (fn U.MkSignature s => signatureToTy(ctx, env, s)) strMap
                   , exnTags = exnTags
                   } (* TODO: pack existentials *)
      end
fun strExpToFExp(ctx, env : Env, U.StructExp { sourceSpan, valMap, tyConMap, strMap })
    = F.StructExp { valMap = Syntax.VIdMap.map (fn (longvid, _) => F.LongVarExp(longvid)) valMap
                  , strMap = Syntax.StrIdMap.map LongStrIdExp strMap
                  , exnTagMap = Syntax.VIdMap.mapPartial (fn (longvid, ids) => if ids = Syntax.ExceptionConstructor then
                                                                                   case USyntax.LongVIdMap.find (#exnTagMap env, longvid) of
                                                                                       SOME exp => SOME exp
                                                                                     | NONE => raise Fail ("exception tag not found for " ^ USyntax.print_LongVId longvid ^ "\n" ^ String.concatWith ", " (List.map (fn (k,v) => USyntax.print_LongVId k ^ ": " ^ F.PrettyPrint.print_Exp v) (USyntax.LongVIdMap.listItemsi (#exnTagMap env))))
                                                                               else
                                                                                   NONE
                                                         ) valMap
                  }
  | strExpToFExp(ctx, env, U.StrIdExp(span, longstrid)) = LongStrIdExp longstrid
  | strExpToFExp(ctx, env, U.TransparentConstraintExp _) = raise Fail "transparent constraint: not implemented yet"
  | strExpToFExp(ctx, env, U.OpaqueConstraintExp _) = raise Fail "opaque constraint: not implemented yet"
  | strExpToFExp(ctx, env, U.LetInStrExp(span, strdecs, strexp)) = let val (env', decs) = strDecsToFDecs(ctx, env, strdecs)
                                                                   in List.foldr F.LetExp (strExpToFExp(ctx, env', strexp)) decs
                                                                   end
and strDecToFDecs(ctx, env : Env, U.CoreDec(span, dec)) = toFDecs(ctx, env, [dec])
  | strDecToFDecs(ctx, env, U.StrBindDec(span, strid, strexp, s))
    = let val vid = F.strIdToVId strid
          val ty = signatureToTy(ctx, env, s)
          fun updateExnTagMap (strids, F.SigType { valMap = _, strMap, exnTags }, e, exnTagMap)
              = let val exnTagMap = Syntax.VIdSet.foldl (fn (vid, m) => USyntax.LongVIdMap.insert(m, U.MkLongVId(strid, strids, vid), F.ExTagProjectionExp(e, vid))) exnTagMap exnTags
                in Syntax.StrIdMap.foldli (fn (strid, ty, m) => updateExnTagMap(strids @ [strid], ty, F.StructureProjectionExp(e, strid), m)) exnTagMap strMap
                end
            | updateExnTagMap (strids, _, _, exnTagMap) = exnTagMap
          val env' = { equalityForTyVarMap = #equalityForTyVarMap env
                     , equalityForTyConMap = #equalityForTyConMap env
                     , exnTagMap = updateExnTagMap ([], ty, F.VarExp vid, #exnTagMap env)
                     }
      in (env', [F.ValDec(F.SimpleBind(vid, ty, strExpToFExp(ctx, env, strexp)))]) (* TODO: unpack existentials *)
      end
and strDecsToFDecs(ctx, env : Env, []) = (env, [])
  | strDecsToFDecs(ctx, env, dec :: decs) = let val (env, dec) = strDecToFDecs(ctx, env, dec)
                                                val (env, decs) = strDecsToFDecs(ctx, env, decs)
                                            in (env, dec @ decs)
                                            end
fun programToFDecs(ctx, env : Env, []) = (env, [])
  | programToFDecs(ctx, env, USyntax.StrDec dec :: topdecs) = let val (env, decs) = strDecToFDecs(ctx, env, dec)
                                                                  val (env, decs') = programToFDecs(ctx, env, topdecs)
                                                              in (env, decs @ decs')
                                                              end
fun isAlphaNumName name = List.all (fn c => Char.isAlphaNum c orelse c = #"_") (String.explode name)
fun libraryToFDecs(ctx, tenv: Typing.Env, env, decs)
    = case (Syntax.VIdMap.find (#valMap tenv, Syntax.MkVId "export"), Syntax.StrIdMap.find (#strMap tenv, Syntax.MkStrId "export")) of
          (NONE, NONE) => raise Fail "No value to export was found."
        | (SOME (longvid, _, _), NONE) => let val (env, decs) = programToFDecs(ctx, env, decs)
                                   in (env, decs @ [ F.ExportValue (F.LongVarExp longvid) ])
                                   end
        | (NONE, SOME (U.MkLongStrId(strid0, strids), { valMap, ... })) =>
          let val fields = Syntax.VIdMap.listItems (Syntax.VIdMap.mapPartiali (fn (vid, _) => let val name = Syntax.getVIdName vid
                                                                                              in if isAlphaNumName name then
                                                                                                     SOME (name, F.LongVarExp(U.MkLongVId(strid0, strids, vid)))
                                                                                                 else if String.isSuffix "'" name then
                                                                                                     let val name' = String.substring (name, 0, String.size name - 1)
                                                                                                     in if isAlphaNumName name' andalso not (Syntax.VIdMap.inDomain (valMap, Syntax.MkVId name')) then
                                                                                                            SOME (name', F.LongVarExp(U.MkLongVId(strid0, strids, vid)))
                                                                                                        else
                                                                                                            NONE
                                                                                                     end
                                                                                                 else
                                                                                                     NONE
                                                                                              end
                                                                              ) valMap)
              val (env, decs) = programToFDecs(ctx, env, decs)
          in (env, decs @ [ F.ExportModule (Vector.fromList fields) ])
          end
        | (SOME _, SOME _) => raise Fail "The value to export is ambiguous."
end (* local *)
end (* structure ToFSyntax *)
