(*
 * Copyright (c) 2022 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure FSyntax = struct
type TyVar = TypedSyntax.TyVar
datatype Kind = TypeKind
              | ArrowKind of Kind * Kind
datatype Ty = TyVar of TyVar
            | RecordType of Ty Syntax.LabelMap.map
            | AppType of { applied : Ty, arg : Ty }
            | FnType of Ty * Ty
            | ForallType of TyVar * Kind * Ty
            | ExistsType of TyVar * Kind * Ty
            | TypeFn of TyVar * Kind * Ty (* type-level function *)
datatype ConBind = ConBind of TypedSyntax.VId * Ty option
datatype DatBind = DatBind of TyVar list * TyVar * ConBind list
datatype PrimOp = IntConstOp of IntInf.int (* 1 type argument *)
                | WordConstOp of IntInf.int (* 1 type argument *)
                | RealConstOp of Numeric.float_notation (* 1 type argument *)
                | Char8ConstOp of char (* 1 type argument *)
                | Char16ConstOp of int (* 1 type argument *)
                | String8ConstOp of string (* 1 type argument *)
                | String16ConstOp of int vector (* 1 type argument *)
                | RaiseOp of SourcePos.span (* type argument: result type, value argument: the exception *)
                | ListOp (* type argument: element type, value arguments: the elements *)
                | VectorOp (* type argument: element type, value arguments: the elements *)
                | DataTagAsStringOp of Syntax.ValueConstructorInfo (* value argument: the data *)
                | DataTagAsString16Op of Syntax.ValueConstructorInfo (* value argument: the data *)
                | DataPayloadOp of Syntax.ValueConstructorInfo (* type argument: payload, value argument: the data *)
                | ExnPayloadOp (* type argument: payload, value argument: the data *)
                | ConstructValOp of Syntax.ValueConstructorInfo (* type argument: data type *)
                | ConstructValWithPayloadOp of Syntax.ValueConstructorInfo (* type arguments: data type, payload, value argument: payload *)
                | ConstructExnOp (* value argument: exception tag *)
                | ConstructExnWithPayloadOp (* type argument: payload, value argument: exception tag, value argument: payload *)
                | PrimFnOp of Primitives.PrimOp
                | JsCallOp (* value argument: function, arguments *)
                | JsMethodOp (* value argument: object, name, arguments *)
                | JsNewOp (* value argument: constructor, arguments *)
                | LuaCallOp (* value argument: function, arguments *)
                | LuaCall1Op (* value argument: function, arguments *)
                | LuaMethodOp of string (* value argument: object, arguments *)
datatype PatternSCon = IntegerConstant of IntInf.int
                     | WordConstant of IntInf.int
                     | CharConstant of char
                     | Char16Constant of int
                     | StringConstant of string
                     | String16Constant of int vector
datatype Pat = WildcardPat of SourcePos.span
             | SConPat of { sourceSpan : SourcePos.span
                          , scon : PatternSCon
                          , equality : Exp
                          , cookedValue : Exp
                          }
             | VarPat of SourcePos.span * TypedSyntax.VId * Ty
             | RecordPat of { sourceSpan : SourcePos.span
                            , fields : (Syntax.Label * Pat) list
                            , ellipsis : Pat option
                            , allFields : Syntax.LabelSet.set
                            }
             | ValConPat of { sourceSpan : SourcePos.span, info : Syntax.ValueConstructorInfo, payload : (Ty * Pat) option }
             | ExnConPat of { sourceSpan : SourcePos.span, tagPath : Exp, payload : (Ty * Pat) option }
             | LayeredPat of SourcePos.span * TypedSyntax.VId * Ty * Pat
             | VectorPat of SourcePos.span * Pat vector * bool * Ty
     and Exp = PrimExp of PrimOp * Ty list * Exp list
             | VarExp of TypedSyntax.VId
             | RecordExp of (Syntax.Label * Exp) list
             | LetExp of Dec * Exp
             | AppExp of Exp * Exp
             | HandleExp of { body : Exp
                            , exnName : TypedSyntax.VId
                            , handler : Exp
                            }
             | IfThenElseExp of Exp * Exp * Exp
             | CaseExp of { sourceSpan : SourcePos.span, subjectExp : Exp, subjectTy : Ty, matches : (Pat * Exp) list, matchType : TypedSyntax.match_type, resultTy : Ty }
             | FnExp of TypedSyntax.VId * Ty * Exp
             | ProjectionExp of { label : Syntax.Label, record : Exp, fieldTypes : Ty Syntax.LabelMap.map }
             | TyAbsExp of TyVar * Kind * Exp
             | TyAppExp of Exp * Ty
             | PackExp of { payloadTy : Ty, exp : Exp, packageTy : Ty } (* packageTy must be ExistsType *)
     and Dec = ValDec of TypedSyntax.VId * Ty option * Exp
             | RecValDec of (TypedSyntax.VId * Ty * Exp) list
             | UnpackDec of TyVar * Kind * TypedSyntax.VId * (* the type of the new identifier *) Ty * Exp
             | IgnoreDec of Exp (* val _ = ... *)
             | DatatypeDec of DatBind list (* does not define value-level constructors *)
             | ExceptionDec of { name : string, tagName : TypedSyntax.VId, payloadTy : Ty option } (* does not define value-level constructors *)
             | ExportValue of Exp
             | ExportModule of (string * Exp) vector
             | GroupDec of TypedSyntax.VIdSet.set option * Dec list
fun ValueLabel vid = Syntax.IdentifierLabel (Syntax.getVIdName vid)
fun StructLabel (Syntax.MkStrId name) = Syntax.IdentifierLabel ("_" ^ name)
fun ExnTagLabel vid = Syntax.IdentifierLabel (Syntax.getVIdName vid ^ ".tag")
fun IntConstExp (value, ty) = PrimExp (IntConstOp value, [ty], [])
fun WordConstExp (value, ty) = PrimExp (WordConstOp value, [ty], [])
fun RaiseExp (span, ty, exp) = PrimExp (RaiseOp span, [ty], [exp])
fun ListExp (exps, elemTy) = PrimExp (ListOp, [elemTy], Vector.foldr (op ::) [] exps)
fun VectorExp (exps, elemTy) = PrimExp (VectorOp, [elemTy], Vector.foldr (op ::) [] exps)
fun TupleType xs = let fun doFields i [] acc = acc
                         | doFields i (x :: xs) acc = doFields (i + 1) xs (Syntax.LabelMap.insert (acc, Syntax.NumericLabel i, x))
                   in RecordType (doFields 1 xs Syntax.LabelMap.empty)
                   end
fun PairType(a, b) = RecordType (Syntax.LabelMapFromList [(Syntax.NumericLabel 1, a), (Syntax.NumericLabel 2, b)])
fun TuplePat (span, xs) = let fun doFields i nil = nil
                                | doFields i (x :: xs) = (Syntax.NumericLabel i, x) :: doFields (i + 1) xs
                              val allFields = #2 (List.foldl (fn (_, (i, set)) => (i + 1, Syntax.LabelSet.add (set, Syntax.NumericLabel i))) (1, Syntax.LabelSet.empty) xs)
                          in RecordPat { sourceSpan = span, fields = doFields 1 xs, ellipsis = NONE, allFields = allFields }
                          end
fun TupleExp xs = let fun doFields i nil = nil
                        | doFields i (x :: xs) = (Syntax.NumericLabel i, x) :: doFields (i + 1) xs
                  in RecordExp (doFields 1 xs)
                  end
fun tyNameToTyVar (TypedSyntax.MkTyName (name, n)) = TypedSyntax.MkTyVar (name, n)
fun TyCon(tyargs, tyname) = List.foldl (fn (arg, applied) => AppType { applied = applied, arg = arg }) (TyVar (tyNameToTyVar tyname)) tyargs
fun AsciiStringAsDatatypeTag (targetInfo : TargetInfo.target_info, s : string)
    = (case #datatypeTag targetInfo of
           TargetInfo.STRING8 => PrimExp (String8ConstOp s, [TyCon ([], Typing.primTyName_string)], [])
         | TargetInfo.STRING16 => PrimExp (String16ConstOp (StringElement.encodeAscii s), [TyCon ([], Typing.primTyName_string16)], [])
      )
fun strIdToVId (TypedSyntax.MkStrId (name, n)) = TypedSyntax.MkVId (name, n)
fun AndalsoExp(a, b) = IfThenElseExp(a, b, VarExp(InitialEnv.VId_false))
fun SimplifyingAndalsoExp (a as VarExp vid, b) = if TypedSyntax.eqVId (vid, InitialEnv.VId_true) then
                                                     b
                                                 else if TypedSyntax.eqVId (vid, InitialEnv.VId_false) then
                                                     a
                                                 else
                                                     AndalsoExp(a, b)
  | SimplifyingAndalsoExp (a, b as VarExp vid) = if TypedSyntax.eqVId (vid, InitialEnv.VId_true) then
                                                     a
                                                 else
                                                     AndalsoExp(a, b)
  | SimplifyingAndalsoExp(a, b) = AndalsoExp(a, b)
fun EqualityType t = FnType (PairType (t, t), TyVar (tyNameToTyVar (Typing.primTyName_bool))) (* t * t -> bool *)
fun arityToKind 0 = TypeKind
  | arityToKind n = ArrowKind (TypeKind, arityToKind (n - 1))

(* occurCheck : TyVar -> Ty -> bool *)
fun occurCheck tv =
    let fun check (TyVar tv') = TypedSyntax.eqUTyVar (tv, tv')
          | check (RecordType xs) = Syntax.LabelMap.exists check xs
          | check (AppType { applied, arg }) = check applied orelse check arg
          | check (FnType(ty1, ty2)) = check ty1 orelse check ty2
          | check (ForallType (tv', kind, ty)) = if TypedSyntax.eqUTyVar (tv, tv') then
                                                     false
                                                 else
                                                     check ty
          | check (ExistsType (tv', kind, ty)) = if TypedSyntax.eqUTyVar (tv, tv') then
                                                     false
                                                 else
                                                     check ty
          | check (TypeFn (tv', kind, ty)) = if TypedSyntax.eqUTyVar (tv, tv') then
                                                 false
                                             else
                                                 check ty
    in check
    end

(* substituteTy : TyVar * Ty -> Ty -> Ty *)
fun substituteTy (tv, replacement) =
    let fun go (ty as TyVar tv') = if TypedSyntax.eqUTyVar (tv, tv') then
                                       replacement
                                   else
                                       ty
          | go (RecordType fields) = RecordType (Syntax.LabelMap.map go fields)
          | go (AppType { applied, arg }) = AppType { applied = go applied, arg = go arg }
          | go (FnType(ty1, ty2)) = FnType(go ty1, go ty2)
          | go (ty as ForallType (tv', kind, ty')) = if TypedSyntax.eqUTyVar (tv, tv') then
                                                         ty
                                                     else if occurCheck tv' replacement then
                                                         (* TODO: generate fresh type variable *)
                                                         let val tv'' = raise Fail "FSyntax.substituteTy: not implemented yet"
                                                         in ForallType (tv'', kind, go (substituteTy (tv', TyVar tv'') ty'))
                                                         end
                                                     else
                                                         ForallType (tv', kind, go ty')
          | go (ty as ExistsType (tv', kind, ty')) = if TypedSyntax.eqUTyVar (tv, tv') then
                                                         ty
                                                     else if occurCheck tv' replacement then
                                                         (* TODO: generate fresh type variable *)
                                                         let val tv'' = raise Fail "FSyntax.substituteTy: not implemented yet"
                                                         in ExistsType (tv'', kind, go (substituteTy (tv', TyVar tv'') ty'))
                                                         end
                                                     else
                                                         ExistsType (tv', kind, go ty')
          | go (ty as TypeFn (tv', kind, ty')) = if TypedSyntax.eqUTyVar (tv, tv') then
                                                     ty
                                                 else if occurCheck tv' replacement then
                                                     (* TODO: generate fresh type variable *)
                                                     let val tv'' = raise Fail "FSyntax.substituteTy: not implemented yet"
                                                     in TypeFn (tv'', kind, go (substituteTy (tv', TyVar tv'') ty'))
                                                     end
                                                 else
                                                     TypeFn (tv', kind, go ty')
    in go
    end

(* substTy : Ty TyVarMap.map -> { doTy : Ty -> Ty, doConBind : ConBind -> ConBind, doPat : Pat -> Pat, doExp : Exp -> Exp, doDec : Dec -> Dec, doDecs : Decs -> Decs } *)
fun substTy (subst : Ty TypedSyntax.TyVarMap.map) =
    let fun doTy (ty as TyVar tv) = (case TypedSyntax.TyVarMap.find (subst, tv) of
                                         NONE => ty
                                       | SOME replacement => replacement
                                    )
          | doTy (RecordType fields) = RecordType (Syntax.LabelMap.map doTy fields)
          | doTy (AppType { applied, arg }) = AppType { applied = doTy applied, arg = doTy arg }
          | doTy (FnType (ty1, ty2)) = FnType (doTy ty1, doTy ty2)
          | doTy (ForallType (tv, kind, ty)) = if TypedSyntax.TyVarMap.inDomain (subst, tv) then (* TODO: use fresh tyvar if necessary *)
                                                   ForallType (tv, kind, #doTy (substTy (#1 (TypedSyntax.TyVarMap.remove (subst, tv)))) ty)
                                               else
                                                   ForallType (tv, kind, doTy ty)
          | doTy (ExistsType (tv, kind, ty)) = if TypedSyntax.TyVarMap.inDomain (subst, tv) then (* TODO: use fresh tyvar if necessary *)
                                                   ExistsType (tv, kind, #doTy (substTy (#1 (TypedSyntax.TyVarMap.remove (subst, tv)))) ty)
                                               else
                                                   ExistsType (tv, kind, doTy ty)
          | doTy (TypeFn (tv, kind, ty)) = if TypedSyntax.TyVarMap.inDomain (subst, tv) then (* TODO: use fresh tyvar if necessary *)
                                               TypeFn (tv, kind, #doTy (substTy (#1 (TypedSyntax.TyVarMap.remove (subst, tv)))) ty)
                                           else
                                               TypeFn (tv, kind, doTy ty)
        fun doConBind (ConBind (vid, optTy)) = ConBind (vid, Option.map doTy optTy)
        fun doDatBind (DatBind (tyvars, tyname, conbinds)) = let val subst' = List.foldl (fn (tv, subst) => if TypedSyntax.TyVarMap.inDomain (subst, tv) then #1 (TypedSyntax.TyVarMap.remove (subst, tv)) else subst) subst tyvars (* TODO: use fresh tyvar if necessary *)
                                                             in DatBind (tyvars, tyname, List.map (#doConBind (substTy subst')) conbinds)
                                                             end
        fun doPat (pat as WildcardPat _) = pat
          | doPat (SConPat { sourceSpan, scon, equality, cookedValue }) = SConPat { sourceSpan = sourceSpan, scon = scon, equality = doExp equality, cookedValue = doExp cookedValue }
          | doPat (VarPat (span, vid, ty)) = VarPat (span, vid, doTy ty)
          | doPat (RecordPat { sourceSpan, fields, ellipsis, allFields }) = RecordPat { sourceSpan = sourceSpan, fields = List.map (fn (label, pat) => (label, doPat pat)) fields, ellipsis = Option.map doPat ellipsis, allFields = allFields }
          | doPat (ValConPat { sourceSpan, info, payload }) = ValConPat { sourceSpan = sourceSpan, info = info, payload = Option.map (fn (ty, pat) => (doTy ty, doPat pat)) payload }
          | doPat (ExnConPat { sourceSpan, tagPath, payload }) = ExnConPat { sourceSpan = sourceSpan, tagPath = doExp tagPath, payload = Option.map (fn (ty, pat) => (doTy ty, doPat pat)) payload }
          | doPat (LayeredPat (span, vid, ty, pat)) = LayeredPat (span, vid, doTy ty, doPat pat)
          | doPat (VectorPat (span, pats, ellipsis, elemTy)) = VectorPat (span, Vector.map doPat pats, ellipsis, doTy elemTy)
        and doExp (PrimExp (primOp, tyargs, args)) = PrimExp (primOp, List.map doTy tyargs, List.map doExp args)
          | doExp (exp as VarExp _) = exp
          | doExp (RecordExp fields) = RecordExp (List.map (fn (label, exp) => (label, doExp exp)) fields)
          | doExp (LetExp (dec, exp)) = LetExp (doDec dec, doExp exp)
          | doExp (AppExp (exp1, exp2)) = AppExp (doExp exp1, doExp exp2)
          | doExp (HandleExp { body, exnName, handler }) = HandleExp { body = doExp body, exnName = exnName, handler = doExp handler }
          | doExp (IfThenElseExp (exp1, exp2, exp3)) = IfThenElseExp (doExp exp1, doExp exp2, doExp exp3)
          | doExp (CaseExp { sourceSpan, subjectExp, subjectTy, matches, matchType, resultTy }) = CaseExp { sourceSpan = sourceSpan, subjectExp = doExp subjectExp, subjectTy = doTy subjectTy, matches = List.map (fn (pat, exp) => (doPat pat, doExp exp)) matches, matchType = matchType, resultTy = doTy resultTy }
          | doExp (FnExp (vid, ty, exp)) = FnExp (vid, doTy ty, doExp exp)
          | doExp (ProjectionExp { label, record, fieldTypes }) = ProjectionExp { label = label, record = doExp record, fieldTypes = Syntax.LabelMap.map doTy fieldTypes }
          | doExp (TyAbsExp (tv, kind, exp)) = if TypedSyntax.TyVarMap.inDomain (subst, tv) then (* TODO: use fresh tyvar if necessary *)
                                                   TyAbsExp (tv, kind, #doExp (substTy (#1 (TypedSyntax.TyVarMap.remove (subst, tv)))) exp)
                                               else
                                                   TyAbsExp (tv, kind, doExp exp)
          | doExp (PackExp { payloadTy, exp, packageTy }) = PackExp { payloadTy = doTy payloadTy, exp = doExp exp, packageTy = doTy packageTy }
          | doExp (TyAppExp (exp, ty)) = TyAppExp (doExp exp, doTy ty)
        and doDec (ValDec (vid, optTy, exp)) = ValDec (vid, Option.map doTy optTy, doExp exp)
          | doDec (RecValDec valbinds) = RecValDec (List.map (fn (vid, ty, exp) => (vid, doTy ty, doExp exp)) valbinds)
          | doDec (UnpackDec (tv, kind, vid, ty, exp)) = UnpackDec (tv, kind, vid, if TypedSyntax.TyVarMap.inDomain (subst, tv) then (* TODO: use fresh tyvar if necessary *)
                                                                                       #doTy (substTy (#1 (TypedSyntax.TyVarMap.remove (subst, tv)))) ty
                                                                                   else
                                                                                       doTy ty, doExp exp)
          | doDec (IgnoreDec exp) = IgnoreDec (doExp exp)
          | doDec (DatatypeDec datbinds) = DatatypeDec (List.map doDatBind datbinds)
          | doDec (ExceptionDec { name, tagName, payloadTy }) = ExceptionDec { name = name, tagName = tagName, payloadTy = Option.map doTy payloadTy }
          | doDec (ExportValue exp) = ExportValue (doExp exp)
          | doDec (ExportModule fields) = ExportModule (Vector.map (fn (label, exp) => (label, doExp exp)) fields)
          | doDec (GroupDec (vars, decs)) = GroupDec (vars, List.map doDec decs)
    in { doTy = doTy
       , doConBind = doConBind
       , doPat = doPat
       , doExp = doExp
       , doDec = doDec
       , doDecs = List.map doDec
       }
    end

fun freeTyVarsInTy (bound : TypedSyntax.TyVarSet.set, TyVar tv) acc = if TypedSyntax.TyVarSet.member (bound, tv) then
                                                                          acc
                                                                      else
                                                                          TypedSyntax.TyVarSet.add (acc, tv)
  | freeTyVarsInTy (bound, RecordType fields) acc = Syntax.LabelMap.foldl (fn (ty, acc) => freeTyVarsInTy (bound, ty) acc) acc fields
  | freeTyVarsInTy (bound, AppType { applied, arg }) acc = freeTyVarsInTy (bound, applied) (freeTyVarsInTy (bound, arg) acc)
  | freeTyVarsInTy (bound, FnType (ty1, ty2)) acc = freeTyVarsInTy (bound, ty1) (freeTyVarsInTy (bound, ty2) acc)
  | freeTyVarsInTy (bound, ForallType (tv, kind, ty)) acc = freeTyVarsInTy (TypedSyntax.TyVarSet.add (bound, tv), ty) acc
  | freeTyVarsInTy (bound, ExistsType (tv, kind, ty)) acc = freeTyVarsInTy (TypedSyntax.TyVarSet.add (bound, tv), ty) acc
  | freeTyVarsInTy (bound, TypeFn (tv, kind, ty)) acc = freeTyVarsInTy (TypedSyntax.TyVarSet.add (bound, tv), ty) acc
fun freeTyVarsInPat (bound, WildcardPat _) acc = acc
  | freeTyVarsInPat (bound, SConPat { sourceSpan = _, scon = _, equality, cookedValue }) acc = freeTyVarsInExp (bound, equality) (freeTyVarsInExp (bound, cookedValue) acc)
  | freeTyVarsInPat (bound, VarPat (_, vid, ty)) acc = freeTyVarsInTy (bound, ty) acc
  | freeTyVarsInPat (bound, RecordPat { sourceSpan, fields, ellipsis = NONE, allFields }) acc = List.foldl (fn ((label, pat), acc) => freeTyVarsInPat (bound, pat) acc) acc fields
  | freeTyVarsInPat (bound, RecordPat { sourceSpan, fields, ellipsis = SOME basePat, allFields }) acc = List.foldl (fn ((label, pat), acc) => freeTyVarsInPat (bound, pat) acc) (freeTyVarsInPat (bound, basePat) acc) fields
  | freeTyVarsInPat (bound, ValConPat { sourceSpan = _, info, payload = NONE }) acc = acc
  | freeTyVarsInPat (bound, ValConPat { sourceSpan = _, info, payload = SOME (payloadTy, payloadPat) }) acc = freeTyVarsInTy (bound, payloadTy) (freeTyVarsInPat (bound, payloadPat) acc)
  | freeTyVarsInPat (bound, ExnConPat { sourceSpan = _, tagPath, payload = NONE }) acc = freeTyVarsInExp (bound, tagPath) acc
  | freeTyVarsInPat (bound, ExnConPat { sourceSpan = _, tagPath, payload = SOME (payloadTy, payloadPat) }) acc = freeTyVarsInTy (bound, payloadTy) (freeTyVarsInPat (bound, payloadPat) (freeTyVarsInExp (bound, tagPath) acc))
  | freeTyVarsInPat (bound, LayeredPat (_, _, ty, innerPat)) acc = freeTyVarsInTy (bound, ty) (freeTyVarsInPat (bound, innerPat) acc)
  | freeTyVarsInPat (bound, VectorPat (_, pats, ellipsis, elemTy)) acc = Vector.foldr (fn (pat, acc) => freeTyVarsInPat (bound, pat) acc) (freeTyVarsInTy (bound, elemTy) acc) pats
and freeTyVarsInExp (bound : TypedSyntax.TyVarSet.set, PrimExp (primOp, tyargs, args)) acc = let val acc = List.foldl (fn (ty, acc) => freeTyVarsInTy (bound, ty) acc) acc tyargs
                                                                                             in List.foldl (fn (exp, acc) => freeTyVarsInExp (bound, exp) acc) acc args
                                                                                             end
  | freeTyVarsInExp (bound, VarExp _) acc = acc
  | freeTyVarsInExp (bound, RecordExp fields) acc = List.foldl (fn ((label, exp), acc) => freeTyVarsInExp (bound, exp) acc) acc fields
  | freeTyVarsInExp (bound, LetExp (dec, exp)) acc = let val (bound, acc) = freeTyVarsInDec (bound, dec) acc
                                                     in freeTyVarsInExp (bound, exp) acc
                                                     end
  | freeTyVarsInExp (bound, AppExp (exp1, exp2)) acc = freeTyVarsInExp (bound, exp1) (freeTyVarsInExp (bound, exp2) acc)
  | freeTyVarsInExp (bound, HandleExp { body, exnName, handler }) acc = freeTyVarsInExp (bound, body) (freeTyVarsInExp (bound, handler) acc)
  | freeTyVarsInExp (bound, IfThenElseExp (exp1, exp2, exp3)) acc = freeTyVarsInExp (bound, exp1) (freeTyVarsInExp (bound, exp2) (freeTyVarsInExp (bound, exp3) acc))
  | freeTyVarsInExp (bound, CaseExp { sourceSpan, subjectExp, subjectTy, matches, matchType, resultTy }) acc
    = let val acc = freeTyVarsInExp (bound, subjectExp) acc
          val acc = freeTyVarsInTy (bound, subjectTy) acc
          val acc = freeTyVarsInTy (bound, resultTy) acc
      in List.foldl (fn ((pat, exp), acc) => freeTyVarsInPat (bound, pat) (freeTyVarsInExp (bound, exp) acc)) acc matches
      end
  | freeTyVarsInExp (bound, FnExp (vid, ty, exp)) acc = freeTyVarsInTy (bound, ty) (freeTyVarsInExp (bound, exp) acc)
  | freeTyVarsInExp (bound, ProjectionExp { label, record, fieldTypes }) acc = freeTyVarsInExp (bound, record) (Syntax.LabelMap.foldl (fn (ty, acc) => freeTyVarsInTy (bound, ty) acc) acc fieldTypes)
  | freeTyVarsInExp (bound, TyAbsExp (tv, kind, exp)) acc = freeTyVarsInExp (TypedSyntax.TyVarSet.add (bound, tv), exp) acc
  | freeTyVarsInExp (bound, TyAppExp (exp, ty)) acc = freeTyVarsInExp (bound, exp) (freeTyVarsInTy (bound, ty) acc)
  | freeTyVarsInExp (bound, PackExp { payloadTy, exp, packageTy }) acc = freeTyVarsInTy (bound, payloadTy) (freeTyVarsInTy (bound, packageTy) (freeTyVarsInExp (bound, exp) acc))
and freeTyVarsInDec (bound, ValDec (vid, optTy, exp)) acc = (bound, (case optTy of
                                                                         NONE => freeTyVarsInExp (bound, exp) acc
                                                                       | SOME ty => freeTyVarsInTy (bound, ty) (freeTyVarsInExp (bound, exp) acc)
                                                                    )
                                                            )
  | freeTyVarsInDec (bound, RecValDec valbinds) acc = (bound, List.foldl (fn ((vid, ty, exp), acc) => freeTyVarsInTy (bound, ty) (freeTyVarsInExp (bound, exp) acc)) acc valbinds)
  | freeTyVarsInDec (bound, UnpackDec (tv, kind, vid, ty, exp)) acc = let val acc = freeTyVarsInExp (bound, exp) acc
                                                                          val bound = TypedSyntax.TyVarSet.add (bound, tv)
                                                                      in (bound, freeTyVarsInTy (bound, ty) acc)
                                                                      end
  | freeTyVarsInDec (bound, IgnoreDec exp) acc = (bound, freeTyVarsInExp (bound, exp) acc)
  | freeTyVarsInDec (bound, DatatypeDec datbinds) acc = let val bound = List.foldl (fn (DatBind (tyvars, tyname, conbinds), bound) => TypedSyntax.TyVarSet.add (bound, tyname)) bound datbinds
                                                        in (bound, List.foldl (fn (DatBind (tyvars, tyname, conbinds), acc) =>
                                                                                  let val bound = TypedSyntax.TyVarSet.addList (bound, tyvars)
                                                                                  in List.foldl (fn (ConBind (vid, NONE), acc) => acc
                                                                                                | (ConBind (vid, SOME ty), acc) => freeTyVarsInTy (bound, ty) acc
                                                                                                ) acc conbinds
                                                                                  end
                                                                              ) acc datbinds)
                                                        end
  | freeTyVarsInDec (bound, ExceptionDec { name, tagName, payloadTy }) acc = (bound, case payloadTy of
                                                                                         NONE => acc
                                                                                       | SOME payloadTy => freeTyVarsInTy (bound, payloadTy) acc
                                                                             )
  | freeTyVarsInDec (bound, ExportValue exp) acc = (bound, freeTyVarsInExp (bound, exp) acc)
  | freeTyVarsInDec (bound, ExportModule exports) acc = (bound, Vector.foldl (fn ((name, exp), acc) => freeTyVarsInExp (bound, exp) acc) acc exports)
  | freeTyVarsInDec (bound, GroupDec (v, decs)) acc = freeTyVarsInDecs (bound, decs) acc
and freeTyVarsInDecs (bound, decs) acc = List.foldl (fn (dec, (bound, acc)) => freeTyVarsInDec (bound, dec) acc) (bound, acc) decs

fun varsInPat (WildcardPat _) acc = acc
  | varsInPat (SConPat _) acc = acc
  | varsInPat (VarPat (_, vid, ty)) acc = TypedSyntax.VIdSet.add (acc, vid)
  | varsInPat (RecordPat { sourceSpan, fields, ellipsis = NONE, allFields }) acc = List.foldl (fn ((label, pat), acc) => varsInPat pat acc) acc fields
  | varsInPat (RecordPat { sourceSpan, fields, ellipsis = SOME basePat, allFields }) acc = List.foldl (fn ((label, pat), acc) => varsInPat pat acc) (varsInPat basePat acc) fields
  | varsInPat (ValConPat { sourceSpan = _, info = _, payload = SOME (_, payloadPat) }) acc = varsInPat payloadPat acc
  | varsInPat (ValConPat { sourceSpan = _, info = _, payload = NONE }) acc = acc
  | varsInPat (ExnConPat { sourceSpan = _, tagPath = _, payload = SOME (_, payloadPat) }) acc = varsInPat payloadPat acc
  | varsInPat (ExnConPat { sourceSpan = _, tagPath = _, payload = NONE }) acc = acc
  | varsInPat (LayeredPat (_, vid, ty, innerPat)) acc = varsInPat innerPat (TypedSyntax.VIdSet.add (acc, vid))
  | varsInPat (VectorPat (_, pats, wildcard, ty)) acc = Vector.foldl (fn (pat, acc) => varsInPat pat acc) acc pats

fun freeVarsInPat (bound : TypedSyntax.VIdSet.set, WildcardPat _) acc = acc
  | freeVarsInPat (bound, SConPat { sourceSpan = _, scon = _, equality, cookedValue }) acc = freeVarsInExp (bound, equality) (freeVarsInExp (bound, cookedValue) acc)
  | freeVarsInPat (bound, VarPat _) acc = acc
  | freeVarsInPat (bound, RecordPat { sourceSpan = _, fields, ellipsis = NONE, allFields }) acc = List.foldl (fn ((_, pat), acc) => freeVarsInPat (bound, pat) acc) acc fields
  | freeVarsInPat (bound, RecordPat { sourceSpan = _, fields, ellipsis = SOME basePat, allFields }) acc = List.foldl (fn ((_, pat), acc) => freeVarsInPat (bound, pat) acc) (freeVarsInPat (bound, basePat) acc) fields
  | freeVarsInPat (bound, ValConPat { sourceSpan = _, info = _, payload = NONE }) acc = acc
  | freeVarsInPat (bound, ValConPat { sourceSpan = _, info = _, payload = SOME (_, payloadPat) }) acc = freeVarsInPat (bound, payloadPat) acc
  | freeVarsInPat (bound, ExnConPat { sourceSpan = _, tagPath, payload = NONE }) acc = freeVarsInExp (bound, tagPath) acc
  | freeVarsInPat (bound, ExnConPat { sourceSpan = _, tagPath, payload = SOME (_, payloadPat) }) acc = freeVarsInExp (bound, tagPath) (freeVarsInPat (bound, payloadPat) acc)
  | freeVarsInPat (bound, LayeredPat (_, _, _, innerPat)) acc = freeVarsInPat (bound, innerPat) acc
  | freeVarsInPat (bound, VectorPat (_, pats, _, _)) acc = Vector.foldl (fn (pat, acc) => freeVarsInPat (bound, pat) acc) acc pats
and freeVarsInExp (bound : TypedSyntax.VIdSet.set, PrimExp (primOp, tyargs, args)) acc = List.foldl (fn (exp, acc) => freeVarsInExp (bound, exp) acc) acc args
  | freeVarsInExp (bound, VarExp vid) acc = if TypedSyntax.VIdSet.member (bound, vid) then
                                                acc
                                            else
                                                TypedSyntax.VIdSet.add (acc, vid)
  | freeVarsInExp (bound, RecordExp fields) acc = List.foldl (fn ((label, exp), acc) => freeVarsInExp (bound, exp) acc) acc fields
  | freeVarsInExp (bound, LetExp (dec, exp)) acc = let val (bound, acc) = freeVarsInDec (bound, dec) acc
                                                   in freeVarsInExp (bound, exp) acc
                                                   end
  | freeVarsInExp (bound, AppExp (exp1, exp2)) acc = freeVarsInExp (bound, exp1) (freeVarsInExp (bound, exp2) acc)
  | freeVarsInExp (bound, HandleExp { body, exnName, handler }) acc = freeVarsInExp (bound, body) (freeVarsInExp (TypedSyntax.VIdSet.add (bound, exnName), handler) acc)
  | freeVarsInExp (bound, IfThenElseExp (exp1, exp2, exp3)) acc = freeVarsInExp (bound, exp1) (freeVarsInExp (bound, exp2) (freeVarsInExp (bound, exp3) acc))
  | freeVarsInExp (bound, CaseExp { sourceSpan = _, subjectExp, subjectTy = _, matches, matchType = _, resultTy = _ }) acc = List.foldl (fn ((pat, exp), acc) => freeVarsInExp (varsInPat pat bound, exp) (freeVarsInPat (bound, pat) acc)) (freeVarsInExp (bound, subjectExp) acc) matches
  | freeVarsInExp (bound, FnExp (vid, ty, exp)) acc = freeVarsInExp (TypedSyntax.VIdSet.add (bound, vid), exp) acc
  | freeVarsInExp (bound, ProjectionExp { label, record, fieldTypes }) acc = freeVarsInExp (bound, record) acc
  | freeVarsInExp (bound, TyAbsExp (tv, kind, exp)) acc = freeVarsInExp (bound, exp) acc
  | freeVarsInExp (bound, TyAppExp (exp, ty)) acc = freeVarsInExp (bound, exp) acc
  | freeVarsInExp (bound, PackExp { payloadTy, exp, packageTy }) acc = freeVarsInExp (bound, exp) acc
and freeVarsInDec (bound, ValDec (vid, ty, exp)) acc = (TypedSyntax.VIdSet.add (bound, vid), freeVarsInExp (bound, exp) acc)
  | freeVarsInDec (bound, RecValDec valbinds) acc = let val bound = List.foldl (fn ((vid, _, _), bound) => TypedSyntax.VIdSet.add (bound, vid)) bound valbinds
                                                    in (bound, List.foldl (fn ((_, _, exp), acc) => freeVarsInExp (bound, exp) acc) acc valbinds)
                                                    end
  | freeVarsInDec (bound, UnpackDec (tv, kind, vid, ty, exp)) acc = (TypedSyntax.VIdSet.add (bound, vid), freeVarsInExp (bound, exp) acc)
  | freeVarsInDec (bound, IgnoreDec exp) acc = (bound, freeVarsInExp (bound, exp) acc)
  | freeVarsInDec (bound, DatatypeDec datbinds) acc = (List.foldl (fn (DatBind (tyvars, tyname, conbinds), bound) => List.foldl (fn (ConBind (vid, optTy), bound) => TypedSyntax.VIdSet.add (bound, vid)) bound conbinds) bound datbinds, acc)
  | freeVarsInDec (bound, ExceptionDec { name, tagName, payloadTy }) acc = (TypedSyntax.VIdSet.add (bound, tagName), acc)
  | freeVarsInDec (bound, ExportValue exp) acc = (bound, freeVarsInExp (bound, exp) acc)
  | freeVarsInDec (bound, ExportModule exps) acc = (bound, Vector.foldl (fn ((name, exp), acc) => freeVarsInExp (bound, exp) acc) acc exps)
  | freeVarsInDec (bound, GroupDec (_, decs)) acc = List.foldl (fn (dec, (bound, acc)) => freeVarsInDec (bound, dec) acc) (bound, acc) decs

fun getSourceSpanOfPat (WildcardPat span) = span
  | getSourceSpanOfPat (SConPat { sourceSpan, ... }) = sourceSpan
  | getSourceSpanOfPat (VarPat (span, _, _)) = span
  | getSourceSpanOfPat (RecordPat { sourceSpan, ... }) = sourceSpan
  | getSourceSpanOfPat (ValConPat { sourceSpan, ... }) = sourceSpan
  | getSourceSpanOfPat (ExnConPat { sourceSpan, ... }) = sourceSpan
  | getSourceSpanOfPat (LayeredPat (span, _, _, _)) = span
  | getSourceSpanOfPat (VectorPat (span, _, _, _)) = span

structure PrettyPrint = struct
val print_TyVar = TypedSyntax.print_TyVar
val print_VId = TypedSyntax.print_VId
val print_LongVId = TypedSyntax.print_LongVId
fun print_Ty (TyVar x) = "TyVar(" ^ print_TyVar x ^ ")"
  | print_Ty (RecordType xs) = let val xs = Syntax.LabelMap.listItemsi xs
                               in case Syntax.extractTuple (1, xs) of
                                      NONE => "RecordType " ^ Syntax.print_list (Syntax.print_pair (Syntax.print_Label,print_Ty)) xs
                                    | SOME ys => "TupleType " ^ Syntax.print_list print_Ty ys
                               end
  | print_Ty (AppType { applied, arg }) = "AppType{applied=" ^ print_Ty applied ^ ",arg=" ^ print_Ty arg ^ "}"
  | print_Ty (FnType(x,y)) = "FnType(" ^ print_Ty x ^ "," ^ print_Ty y ^ ")"
  | print_Ty (ForallType(tv,kind,x)) = "ForallType(" ^ print_TyVar tv ^ "," ^ print_Ty x ^ ")"
  | print_Ty (ExistsType(tv,kind,x)) = "ExistsType(" ^ print_TyVar tv ^ "," ^ print_Ty x ^ ")"
  | print_Ty (TypeFn(tv,kind,x)) = "TypeFn(" ^ print_TyVar tv ^ "," ^ print_Ty x ^ ")"
fun print_PrimOp (IntConstOp x) = "IntConstOp " ^ IntInf.toString x
  | print_PrimOp (WordConstOp x) = "WordConstOp " ^ IntInf.toString x
  | print_PrimOp (RealConstOp x) = "RealConstOp " ^ Numeric.Notation.toString "~" x
  | print_PrimOp (Char8ConstOp x) = "Char8ConstOp \"" ^ Char.toString x ^ "\""
  | print_PrimOp (Char16ConstOp x) = "Char16ConstOp \"" ^ StringElement.charToString (StringElement.CODEUNIT x) ^ "\""
  | print_PrimOp (String8ConstOp x) = "String8ConstOp \"" ^ String.toString x ^ "\""
  | print_PrimOp (String16ConstOp x) = "String16ConstOp \"" ^ Vector.foldr (fn (c, acc) => StringElement.charToString (StringElement.CODEUNIT c) ^ acc) "\"" x
  | print_PrimOp (RaiseOp span) = "RaiseOp"
  | print_PrimOp ListOp = "ListOp"
  | print_PrimOp VectorOp = "VectorOp"
  | print_PrimOp (DataTagAsStringOp _) = "DataTagAsStringOp"
  | print_PrimOp (DataTagAsString16Op _) = "DataTagAsString16Op"
  | print_PrimOp (DataPayloadOp _) = "DataPayloadOp"
  | print_PrimOp ExnPayloadOp = "ExnPayloadOp"
  | print_PrimOp (ConstructValOp _) = "ConstructValOp"
  | print_PrimOp (ConstructValWithPayloadOp _) = "ConstructValWithPayloadOp"
  | print_PrimOp ConstructExnOp = "ConstructExnOp"
  | print_PrimOp ConstructExnWithPayloadOp = "ConstructExnWithPayloadOp"
  | print_PrimOp (PrimFnOp x) = Primitives.toString x
  | print_PrimOp JsCallOp = "JsCallOp"
  | print_PrimOp JsMethodOp = "JsMethodOp"
  | print_PrimOp JsNewOp = "JsNewOp"
  | print_PrimOp LuaCallOp = "LuaCallOp"
  | print_PrimOp LuaCall1Op = "LuaCall1Op"
  | print_PrimOp (LuaMethodOp _) = "LuaMethodOp"
fun print_Pat (WildcardPat _) = "WildcardPat"
  | print_Pat (SConPat { sourceSpan, scon = IntegerConstant x, equality, cookedValue }) = "SConPat(IntegerConstant " ^ IntInf.toString x ^ ")"
  | print_Pat (SConPat { sourceSpan, scon = WordConstant x, equality, cookedValue }) = "SConPat(WordConstant " ^ IntInf.toString x ^ ")"
  | print_Pat (SConPat { sourceSpan, scon = CharConstant x, equality, cookedValue }) = "SConPat(CharConstant #\"" ^ Char.toString x ^ "\")"
  | print_Pat (SConPat { sourceSpan, scon = Char16Constant x, equality, cookedValue }) = "SConPat(Char16Constant " ^ Int.toString x ^ ")"
  | print_Pat (SConPat { sourceSpan, scon = StringConstant x, equality, cookedValue }) = "SConPat(StringConstant \"" ^ String.toString x ^ "\")"
  | print_Pat (SConPat { sourceSpan, scon = String16Constant x, equality, cookedValue }) = "SConPat(String16Constant)"
  | print_Pat (VarPat(_, vid, ty)) = "VarPat(" ^ print_VId vid ^ "," ^ print_Ty ty ^ ")"
  | print_Pat (LayeredPat (_, vid, ty, pat)) = "TypedPat(" ^ print_VId vid ^ "," ^ print_Ty ty ^ "," ^ print_Pat pat ^ ")"
  | print_Pat (ValConPat { sourceSpan = _, info, payload }) = "ValConPat(" ^ #tag info ^ "," ^ Syntax.print_option (Syntax.print_pair (print_Ty, print_Pat)) payload ^ ")"
  | print_Pat (ExnConPat { sourceSpan = _, tagPath, payload }) = "ExnConPat(" ^ print_Exp tagPath ^ "," ^ Syntax.print_option (Syntax.print_pair (print_Ty, print_Pat)) payload ^ ")"
  | print_Pat (RecordPat { sourceSpan, fields, ellipsis = NONE, allFields })
    = (case Syntax.extractTuple (1, fields) of
           NONE => "RecordPat(" ^ Syntax.print_list (Syntax.print_pair (Syntax.print_Label, print_Pat)) fields ^ ",NONE)"
         | SOME ys => "TuplePat " ^ Syntax.print_list print_Pat ys
      )
  | print_Pat (RecordPat { sourceSpan, fields, ellipsis = SOME basePat, allFields }) = "RecordPat(" ^ Syntax.print_list (Syntax.print_pair (Syntax.print_Label, print_Pat)) fields ^ ",SOME(" ^ print_Pat basePat ^ "))"
  | print_Pat (VectorPat _) = "VectorPat"
and print_Exp (PrimExp (primOp, tyargs, args)) = "PrimExp(" ^ print_PrimOp primOp ^ "," ^ String.concatWith "," (List.map print_Ty tyargs) ^ "," ^ String.concatWith "," (List.map print_Exp args) ^ ")"
  | print_Exp (VarExp(x)) = "VarExp(" ^ print_VId x ^ ")"
  | print_Exp (RecordExp x) = (case Syntax.extractTuple (1, x) of
                                   NONE => "RecordExp " ^ Syntax.print_list (Syntax.print_pair (Syntax.print_Label, print_Exp)) x
                                 | SOME ys => "TupleExp " ^ Syntax.print_list print_Exp ys
                              )
  | print_Exp (LetExp(dec,x)) = "LetExp(" ^ print_Dec dec ^ "," ^ print_Exp x ^ ")"
  | print_Exp (AppExp(x,y)) = "AppExp(" ^ print_Exp x ^ "," ^ print_Exp y ^ ")"
  | print_Exp (HandleExp { body, exnName, handler }) = "HandleExp{body=" ^ print_Exp body ^ ",exnName=" ^ TypedSyntax.print_VId exnName ^ ",handler=" ^ print_Exp handler ^ ")"
  | print_Exp (IfThenElseExp(x,y,z)) = "IfThenElseExp(" ^ print_Exp x ^ "," ^ print_Exp y ^ "," ^ print_Exp z ^ ")"
  | print_Exp (CaseExp { subjectExp, subjectTy, matches, ... }) = "CaseExp(" ^ print_Exp subjectExp ^ "," ^ print_Ty subjectTy ^ "," ^ Syntax.print_list (Syntax.print_pair (print_Pat,print_Exp)) matches ^ ")"
  | print_Exp (FnExp(pname,pty,body)) = "FnExp(" ^ print_VId pname ^ "," ^ print_Ty pty ^ "," ^ print_Exp body ^ ")"
  | print_Exp (ProjectionExp { label, record, fieldTypes }) = "ProjectionExp{label=" ^ Syntax.print_Label label ^ ",record=" ^ print_Exp record ^ "}"
  | print_Exp (TyAbsExp(tv, kind, exp)) = "TyAbsExp(" ^ print_TyVar tv ^ "," ^ print_Exp exp ^ ")"
  | print_Exp (TyAppExp(exp, ty)) = "TyAppExp(" ^ print_Exp exp ^ "," ^ print_Ty ty ^ ")"
  | print_Exp (PackExp { payloadTy, exp, packageTy }) = "PackExp{payloadTy=" ^ print_Ty payloadTy ^ ",exp=" ^ print_Exp exp ^ ",packageTy=" ^ print_Ty packageTy ^ "}"
and print_Dec (ValDec (vid, optTy, exp)) = (case optTy of
                                                SOME ty => "ValDec(" ^ print_VId vid ^ ",SOME " ^ print_Ty ty ^ "," ^ print_Exp exp ^ ")"
                                              | NONE => "ValDec(" ^ print_VId vid ^ ",NONE," ^ print_Exp exp ^ ")"
                                           )
  | print_Dec (RecValDec valbinds) = "RecValDec(" ^ Syntax.print_list (fn (vid, ty, exp) => "(" ^ print_VId vid ^ "," ^ print_Ty ty ^ "," ^ print_Exp exp ^ ")") valbinds ^ ")"
  | print_Dec (UnpackDec (tv, kind, vid, ty, exp)) = "UnpackDec(" ^ TypedSyntax.print_TyVar tv ^ "," ^ print_VId vid ^ "," ^ print_Ty ty ^ "," ^ print_Exp exp ^ ")"
  | print_Dec (IgnoreDec exp) = "IgnoreDec(" ^ print_Exp exp ^ ")"
  | print_Dec (DatatypeDec datbinds) = "DatatypeDec"
  | print_Dec (ExceptionDec _) = "ExceptionDec"
  | print_Dec (ExportValue _) = "ExportValue"
  | print_Dec (ExportModule _) = "ExportModule"
  | print_Dec (GroupDec (vids, decs)) = "GroupDec(" ^ Syntax.print_list print_Dec decs ^ ")"
val print_Decs = Syntax.print_list print_Dec
end (* structure PrettyPrint *)
end (* structure FSyntax *)

structure ToFSyntax = struct
type Context = { nextVId : int ref
               , nextTyVar : int ref
               , targetInfo : TargetInfo.target_info
               , messageHandler : Message.handler
               }
fun emitError (ctx : Context, spans, message) = Message.error (#messageHandler ctx, spans, "code generation", message)
fun emitFatalError (ctx : Context, spans, message) = Message.fatalError (#messageHandler ctx, spans, "code generation", message)
type Env = { equalityForTyVarMap : TypedSyntax.VId TypedSyntax.TyVarMap.map
           , equalityForTyNameMap : TypedSyntax.LongVId TypedSyntax.TyNameMap.map
           , exnTagMap : FSyntax.Exp TypedSyntax.VIdMap.map
           , overloadMap : (FSyntax.Exp Syntax.OverloadKeyMap.map) TypedSyntax.TyNameMap.map
           , valMap : FSyntax.Ty TypedSyntax.VIdMap.map
           }

fun LongVarExp (ctx, env : Env, spans, TypedSyntax.MkShortVId vid) = (case TypedSyntax.VIdMap.find (#valMap env, vid) of
                                                                          SOME ty => (FSyntax.VarExp vid, ty)
                                                                        | NONE => emitFatalError (ctx, spans, "vid not found (" ^ TypedSyntax.print_VId vid ^ ")")
                                                                     )
  | LongVarExp (ctx, env, spans, TypedSyntax.MkLongVId (strid0, strids, vid))
    = let val strid0 = FSyntax.strIdToVId strid0
          val ty0 = case TypedSyntax.VIdMap.find (#valMap env, strid0) of
                        SOME ty => ty
                      | NONE => emitFatalError (ctx, spans, "strid not found (longvid / " ^ TypedSyntax.print_VId strid0 ^ ")")
          fun go ([], exp, ty) = let val label = FSyntax.ValueLabel vid
                                 in case ty of
                                        FSyntax.RecordType fieldTypes => (case Syntax.LabelMap.find (fieldTypes, label) of
                                                                              SOME ty' => (FSyntax.ProjectionExp { label = label, record = exp, fieldTypes = fieldTypes }, ty')
                                                                            | NONE => emitFatalError (ctx, spans, "non-existent value")
                                                                         )
                                      | _ => emitFatalError (ctx, spans, "not a record")
                                 end
            | go (strid :: strids, exp, ty) = let val label = FSyntax.StructLabel strid
                                              in case ty of
                                                     FSyntax.RecordType fieldTypes => (case Syntax.LabelMap.find (fieldTypes, label) of
                                                                                           SOME ty' => go (strids, FSyntax.ProjectionExp { label = label, record = exp, fieldTypes = fieldTypes }, ty')
                                                                                         | NONE => emitFatalError (ctx, spans, "non-existent substructure")
                                                                                      )
                                                   | _ => emitFatalError (ctx, spans, "not a record")
                                              end
      in go (strids, FSyntax.VarExp strid0, ty0)
      end
fun LongStrIdExp (ctx, env : Env, spans, TypedSyntax.MkLongStrId (strid0, strids)) : FSyntax.Exp * FSyntax.Ty
    = let val strid0 = FSyntax.strIdToVId strid0
          val ty0 = case TypedSyntax.VIdMap.find (#valMap env, strid0) of
                        SOME ty => ty
                      | NONE => emitFatalError (ctx, spans, "strid not found (longstrid / " ^ TypedSyntax.print_VId strid0 ^ ")")
          fun go ([], exp, ty) = (exp, ty)
            | go (strid :: strids, exp, ty) = let val label = FSyntax.StructLabel strid
                                              in case ty of
                                                     FSyntax.RecordType fieldTypes => (case Syntax.LabelMap.find (fieldTypes, label) of
                                                                                           SOME ty' => go (strids, FSyntax.ProjectionExp { label = label, record = exp, fieldTypes = fieldTypes}, ty')
                                                                                         | NONE => emitFatalError (ctx, spans, "non-existent substructure")
                                                                                      )
                                                   | _ => emitFatalError (ctx, spans, "not a record")
                                              end
      in go (strids, FSyntax.VarExp strid0, ty0)
      end
fun LongVIdToExnTagExp (ctx, env : Env, spans, TypedSyntax.MkShortVId vid) = TypedSyntax.VIdMap.find (#exnTagMap env, vid)
  | LongVIdToExnTagExp (ctx, env, spans, TypedSyntax.MkLongVId (strid0, strids, vid))
    = let val strid0 = FSyntax.strIdToVId strid0
          val ty0 = case TypedSyntax.VIdMap.find (#valMap env, strid0) of
                        SOME ty => ty
                      | NONE => emitFatalError (ctx, spans, "strid not found (exn / " ^ TypedSyntax.print_VId strid0 ^ ")")
          fun go ([], exp, ty) = let val label = FSyntax.ExnTagLabel vid
                                 in case ty of
                                        FSyntax.RecordType fieldTypes => SOME (FSyntax.ProjectionExp { label = label, record = exp, fieldTypes = fieldTypes })
                                      | _ => emitFatalError (ctx, spans, "not a record")
                                 end
            | go (strid :: strids, exp, ty) = let val label = FSyntax.StructLabel strid
                                              in case ty of
                                                     FSyntax.RecordType fieldTypes => (case Syntax.LabelMap.find (fieldTypes, label) of
                                                                                           SOME ty' => go (strids, FSyntax.ProjectionExp { label = label, record = exp, fieldTypes = fieldTypes }, ty')
                                                                                         | NONE => emitFatalError (ctx, spans, "non-existent substructure")
                                                                                      )
                                                   | _ => emitFatalError (ctx, spans, "not a record")
                                              end
      in go (strids, FSyntax.VarExp strid0, ty0)
      end

fun updateEqualityForTyVarMap(f, env : Env) : Env = { equalityForTyVarMap = f (#equalityForTyVarMap env)
                                                    , equalityForTyNameMap = #equalityForTyNameMap env
                                                    , exnTagMap = #exnTagMap env
                                                    , overloadMap = #overloadMap env
                                                    , valMap = #valMap env
                                                    }

fun updateEqualityForTyNameMap(f, env : Env) : Env = { equalityForTyVarMap = #equalityForTyVarMap env
                                                     , equalityForTyNameMap = f (#equalityForTyNameMap env)
                                                     , exnTagMap = #exnTagMap env
                                                     , overloadMap = #overloadMap env
                                                     , valMap = #valMap env
                                                     }

fun updateExnTagMap(f, env : Env) : Env = { equalityForTyVarMap = #equalityForTyVarMap env
                                          , equalityForTyNameMap = #equalityForTyNameMap env
                                          , exnTagMap = f (#exnTagMap env)
                                          , overloadMap = #overloadMap env
                                          , valMap = #valMap env
                                          }

fun updateOverloadMap(f, env : Env) : Env = { equalityForTyVarMap = #equalityForTyVarMap env
                                            , equalityForTyNameMap = #equalityForTyNameMap env
                                            , exnTagMap = #exnTagMap env
                                            , overloadMap = f (#overloadMap env)
                                            , valMap = #valMap env
                                            }

fun updateValMap (f, env : Env) : Env = { equalityForTyVarMap = #equalityForTyVarMap env
                                        , equalityForTyNameMap = #equalityForTyNameMap env
                                        , exnTagMap = #exnTagMap env
                                        , overloadMap = #overloadMap env
                                        , valMap = f (#valMap env)
                                        }

fun freshTyVar(ctx : Context) = let val n = !(#nextTyVar ctx)
                                in #nextTyVar ctx := n + 1
                                 ; TypedSyntax.MkTyVar ("'?", n)
                                end
fun freshVId(ctx : Context, name: string) = let val n = !(#nextVId ctx)
                                            in #nextVId ctx := n + 1
                                             ; TypedSyntax.MkVId (name, n)
                                            end

local structure T = TypedSyntax
      structure F = FSyntax
      (* toFTy : Context * Env * TypedSyntax.Ty -> FSyntax.Ty *)
      (* toFPat : Context * Env * TypedSyntax.Pat -> unit TypedSyntax.VIdMap.map * FSyntax.Pat *)
      (* toFExp : Context * Env * TypedSyntax.Exp -> FSyntax.Exp *)
      (* toFDecs : Context * Env * TypedSyntax.Dec list -> Env * FSyntax.Dec list *)
      (* getEquality : Context * Env * TypedSyntax.Ty -> FSyntax.Exp *)
      val overloads = let open InitialEnv Syntax
                      in List.foldl TypedSyntax.VIdMap.insert' TypedSyntax.VIdMap.empty
                                    [(VId_abs, OVERLOAD_abs)
                                    ,(VId_TILDE, OVERLOAD_TILDE)
                                    ,(VId_div, OVERLOAD_div)
                                    ,(VId_mod, OVERLOAD_mod)
                                    ,(VId_TIMES, OVERLOAD_TIMES)
                                    ,(VId_DIVIDE, OVERLOAD_DIVIDE)
                                    ,(VId_PLUS, OVERLOAD_PLUS)
                                    ,(VId_MINUS, OVERLOAD_MINUS)
                                    ,(VId_LT, OVERLOAD_LT)
                                    ,(VId_LE, OVERLOAD_LE)
                                    ,(VId_GT, OVERLOAD_GT)
                                    ,(VId_GE, OVERLOAD_GE)
                                    ]
                      end
in
fun toFTy (ctx, env : 'dummy, T.TyVar (span, tv)) = F.TyVar tv
  | toFTy (ctx, env, T.AnonymousTyVar (span, ref (T.Link ty))) = toFTy (ctx, env, ty)
  | toFTy (ctx, env, T.AnonymousTyVar (span, ref (T.Unbound _))) = emitFatalError (ctx, [span], "unexpected anonymous type variable")
  | toFTy (ctx, env, T.RecordType (span, fields)) = F.RecordType (Syntax.LabelMap.map (fn ty => toFTy (ctx, env, ty)) fields)
  | toFTy (ctx, env, T.RecordExtType (span, fields, baseTy)) = emitFatalError (ctx, [span], "unexpected record extension")
  | toFTy (ctx, env, T.TyCon (span, tyargs, tyname)) = F.TyCon (List.map (fn arg => toFTy (ctx, env, arg)) tyargs, tyname)
  | toFTy (ctx, env, T.FnType (span, paramTy, resultTy)) = let fun doTy ty = toFTy (ctx, env, ty)
                                                           in F.FnType (doTy paramTy, doTy resultTy)
                                                           end
fun cookIntegerConstant (ctx : Context, env : Env, span, value : IntInf.int, ty)
    = (case ty of
           T.TyCon (_, [], tycon) => if T.eqTyName (tycon, Typing.primTyName_int) then
                                         let val { minInt, maxInt, ... } = #targetInfo ctx
                                             val lower = case minInt of
                                                             NONE => true
                                                           | SOME m => m <= value
                                             val upper = case maxInt of
                                                             NONE => true
                                                           | SOME m => value <= m
                                         in if lower andalso upper then
                                                F.IntConstExp (value, toFTy (ctx, env, ty))
                                            else
                                                ( emitError (ctx, [span], "integer constant out of range")
                                                ; F.IntConstExp (0, toFTy (ctx, env, ty))
                                                )
                                         end
                                     else if T.eqTyName (tycon, Typing.primTyName_int32) then
                                         let val lower = ~0x80000000 <= value
                                             val upper = value <= 0x7fffffff
                                         in if lower andalso upper then
                                                F.IntConstExp (value, toFTy (ctx, env, ty))
                                            else
                                                ( emitError (ctx, [span], "integer constant out of range")
                                                ; F.IntConstExp (0, toFTy (ctx, env, ty))
                                                )
                                         end
                                     else if T.eqTyName (tycon, Typing.primTyName_int54) then
                                         let val lower = ~0x20000000000000 <= value
                                             val upper = value <= 0x1fffffffffffff
                                         in if lower andalso upper then
                                                F.IntConstExp (value, toFTy (ctx, env, ty))
                                            else
                                                ( emitError (ctx, [span], "integer constant out of range")
                                                ; F.IntConstExp (0, toFTy (ctx, env, ty))
                                                )
                                         end
                                     else if T.eqTyName (tycon, Typing.primTyName_int64) then
                                         let val lower = ~0x8000000000000000 <= value
                                             val upper = value <= 0x7fffffffffffffff
                                         in if lower andalso upper then
                                                F.IntConstExp (value, toFTy (ctx, env, ty))
                                            else
                                                ( emitError (ctx, [span], "integer constant out of range")
                                                ; F.IntConstExp (0, toFTy (ctx, env, ty))
                                                )
                                         end
                                     else if T.eqTyName (tycon, Typing.primTyName_intInf) then
                                         F.IntConstExp (value, toFTy (ctx, env, ty))
                                     else
                                         let val overloadMap = case TypedSyntax.TyNameMap.find (#overloadMap env, tycon) of
                                                                   SOME m => m
                                                                 | NONE => emitFatalError (ctx, [span], "invalid integer constant")
                                             val fromInt = case Syntax.OverloadKeyMap.find (overloadMap, Syntax.OVERLOAD_fromInt) of
                                                               SOME x => x
                                                             | NONE => emitFatalError (ctx, [span], "invalid integer constant")
                                             val PLUS = case Syntax.OverloadKeyMap.find (overloadMap, Syntax.OVERLOAD_PLUS) of
                                                            SOME x => x
                                                          | NONE => emitFatalError (ctx, [span], "invalid integer constant")
                                             val TIMES = case Syntax.OverloadKeyMap.find (overloadMap, Syntax.OVERLOAD_TIMES) of
                                                             SOME x => x
                                                           | NONE => emitFatalError (ctx, [span], "invalid integer constant")
                                             val TILDE = case Syntax.OverloadKeyMap.find (overloadMap, Syntax.OVERLOAD_TILDE) of
                                                             SOME x => x
                                                           | NONE => emitFatalError (ctx, [span], "invalid integer constant")
                                             val minInt = case Syntax.OverloadKeyMap.find (overloadMap, Syntax.OVERLOAD_minInt) of
                                                              SOME (F.PrimExp (F.IntConstOp m, _, _)) => SOME m
                                                            | SOME _ => (emitError (ctx, [span], "invalid integer constant"); NONE)
                                                            | NONE => NONE
                                             val maxInt = case Syntax.OverloadKeyMap.find (overloadMap, Syntax.OVERLOAD_maxInt) of
                                                              SOME (F.PrimExp (F.IntConstOp m, _, _)) => SOME m
                                                            | SOME _ => (emitError (ctx, [span], "invalid integer constant"); NONE)
                                                            | NONE => NONE
                                             val lower = case minInt of
                                                             NONE => true
                                                           | SOME m => m <= value
                                             val upper = case maxInt of
                                                             NONE => true
                                                           | SOME m => value <= m
                                             val intTy = toFTy (ctx, env, Typing.primTy_int)
                                             fun decompose x = if ~0x80000000 <= x andalso x <= 0x7fffffff then
                                                                   F.AppExp (fromInt, F.IntConstExp (x, intTy))
                                                               else
                                                                   let val (q, r) = IntInf.quotRem (x, ~0x80000000)
                                                                       val y = case q of
                                                                                   1 => F.AppExp (fromInt, F.IntConstExp (~0x80000000, intTy))
                                                                                 | ~1 => F.AppExp (TILDE, F.AppExp (fromInt, F.IntConstExp (~0x80000000, intTy)))
                                                                                 | _ => F.AppExp (TIMES, F.TupleExp [decompose q, F.AppExp (fromInt, F.IntConstExp (~0x80000000, intTy))])
                                                                   in if r = 0 then
                                                                          y
                                                                      else
                                                                          F.AppExp (PLUS, F.TupleExp [y, F.AppExp (fromInt, F.IntConstExp (r, intTy))])
                                                                   end
                                         in if lower andalso upper then
                                                decompose value
                                            else
                                                ( emitError (ctx, [span], "integer constant out of range")
                                                ; F.IntConstExp (0, toFTy (ctx, env, ty))
                                                )
                                         end
         | _ => emitFatalError (ctx, [span], "invalid integer constant")
      )
fun cookWordConstant (ctx : Context, env : Env, span, value : IntInf.int, ty)
    = (case ty of
           T.TyCon (_, [], tycon) => if T.eqTyName (tycon, Typing.primTyName_word) then
                                         let val { wordSize, ... } = #targetInfo ctx
                                         in if IntInf.~>> (value, Word.fromInt wordSize) = 0 then
                                                F.WordConstExp (value, toFTy (ctx, env, ty))
                                            else
                                                ( emitError (ctx, [span], "word constant out of range")
                                                ; F.WordConstExp (0, toFTy (ctx, env, ty))
                                                )
                                         end
                                     else if T.eqTyName (tycon, Typing.primTyName_word32) then
                                         if IntInf.~>> (value, 0w32) = 0 then
                                             F.WordConstExp (value, toFTy (ctx, env, ty))
                                         else
                                             ( emitError (ctx, [span], "word constant out of range")
                                             ; F.WordConstExp (0, toFTy (ctx, env, ty))
                                             )
                                     else if T.eqTyName (tycon, Typing.primTyName_word64) then
                                         if IntInf.~>> (value, 0w64) = 0 then
                                             F.WordConstExp (value, toFTy (ctx, env, ty))
                                         else
                                             ( emitError (ctx, [span], "word constant out of range")
                                             ; F.WordConstExp (0, toFTy (ctx, env, ty))
                                             )
                                     else
                                         let val overloadMap = case TypedSyntax.TyNameMap.find (#overloadMap env, tycon) of
                                                                   SOME m => m
                                                                 | NONE => emitFatalError (ctx, [span], "invalid word constant for " ^ TypedSyntax.print_TyName tycon)
                                             val fromWord = case Syntax.OverloadKeyMap.find (overloadMap, Syntax.OVERLOAD_fromWord) of
                                                                SOME x => x
                                                              | NONE => emitFatalError (ctx, [span], "invalid word constant: fromWord is not defined")
                                             val PLUS = case Syntax.OverloadKeyMap.find (overloadMap, Syntax.OVERLOAD_PLUS) of
                                                            SOME x => x
                                                          | NONE => emitFatalError (ctx, [span], "invalid word constant: + is not defined")
                                             val TIMES = case Syntax.OverloadKeyMap.find (overloadMap, Syntax.OVERLOAD_TIMES) of
                                                             SOME x => x
                                                           | NONE => emitFatalError (ctx, [span], "invalid word constant: * is not defined")
                                             val wordSize = case Syntax.OverloadKeyMap.find (overloadMap, Syntax.OVERLOAD_wordSize) of
                                                                SOME (F.PrimExp (F.IntConstOp x, _, _)) => x
                                                              | _ => emitFatalError (ctx, [span], "invalid word constant: wordSize is not defined")
                                             val wordTy = toFTy (ctx, env, Typing.primTy_word)
                                             fun decompose x = if x <= 0xffffffff then
                                                                   F.AppExp (fromWord, F.WordConstExp (x, wordTy))
                                                               else
                                                                   let val (q, r) = IntInf.quotRem (x, 0xffffffff)
                                                                       val y = case q of
                                                                                   1 => F.AppExp (fromWord, F.WordConstExp (0xffffffff, wordTy))
                                                                                 | _ => F.AppExp (TIMES, F.TupleExp [decompose q, F.AppExp (fromWord, F.WordConstExp (0xffffffff, wordTy))])
                                                                   in if r = 0 then
                                                                          y
                                                                      else
                                                                          F.AppExp (PLUS, F.TupleExp [y, F.AppExp (fromWord, F.WordConstExp (r, wordTy))])
                                                                   end
                                         in if IntInf.~>> (value, Word.fromLargeInt wordSize) = 0 then
                                                decompose value
                                            else
                                                ( emitError (ctx, [span], "word constant out of range")
                                                ; F.WordConstExp (0, toFTy (ctx, env, ty))
                                                )
                                         end
         | _ => emitFatalError (ctx, [span], "invalid word constant: invalid type")
      )
fun cookRealConstant (ctx : Context, env : Env, span, value : Numeric.float_notation, ty)
    = (case ty of
           T.TyCon (_, [], tycon) => if T.eqTyName (tycon, Typing.primTyName_real) then
                                         ( if not (Numeric.checkExactness Numeric.binary64 value) then
                                               emitError (ctx, [span], "the hexadecimal floating-point value cannot be represented as a 64-bit floating-point number")
                                           else
                                               ()
                                         ; F.PrimExp (F.RealConstOp value, [toFTy (ctx, env, ty)], [])
                                         )
                                     else
                                         emitFatalError (ctx, [span], "invalid real constant: type")
         | _ => emitFatalError (ctx, [span], "invalid real constant: type")
      )
datatype char_width = C8 | C16
fun cookCharacterConstant (ctx : Context, env : Env, span, value : StringElement.char, ty)
    = (case ty of
           T.TyCon (_, [], tycon) =>
           let val w = if T.eqTyName (tycon, Typing.primTyName_char) then
                           C8
                       else if T.eqTyName (tycon, Typing.primTyName_char16) then
                           C16
                       else
                           let val overloadMap = case TypedSyntax.TyNameMap.find (#overloadMap env, tycon) of
                                                     SOME m => m
                                                   | NONE => emitFatalError (ctx, [span], "invalid character constant: type")
                               val maxOrd = case Syntax.OverloadKeyMap.find (overloadMap, Syntax.OVERLOAD_maxOrd) of
                                                SOME (F.PrimExp (F.IntConstOp x, _, _)) => x
                                              | _ => (emitError (ctx, [span], "invalid character constant: type"); 65535)
                           in case maxOrd of
                                  255 => C8
                                | 65535 => C16
                                | _ => (emitError (ctx, [span], "invalid character constant: type"); C16)
                           end
           in case w of
                  C8 => let val x = case value of
                                        StringElement.CODEUNIT x => if 0 <= x andalso x <= 255 then
                                                                        x
                                                                    else
                                                                        (emitError (ctx, [span], "invalid character constant: out of range"); 0)
                                      | StringElement.UNICODE_SCALAR x => if 0 <= x andalso x <= 127 then
                                                                              x
                                                                          else
                                                                              (emitError (ctx, [span], "invalid character constant: out of range"); 0)
                            val c = Char.chr x
                        in (F.CharConstant c, F.PrimExp (F.Char8ConstOp c, [toFTy (ctx, env, ty)], []))
                        end
                | C16 => let val x = case value of
                                         StringElement.CODEUNIT x => if 0 <= x andalso x <= 0xffff then
                                                                         x
                                                                     else
                                                                         (emitError (ctx, [span], "invalid character constant: out of range"); 0)
                                       | StringElement.UNICODE_SCALAR x => if 0 <= x andalso x <= 0xffff then
                                                                               x
                                                                           else
                                                                               (emitError (ctx, [span], "invalid character constant: out of range"); 0)
                         in (F.Char16Constant x, F.PrimExp (F.Char16ConstOp x, [toFTy (ctx, env, ty)], []))
                         end
           end
         | _ => emitFatalError (ctx, [span], "invalid character constant: type")
      )
fun cookStringConstant (ctx : Context, env : Env, span, value, ty)
    = (case ty of
           T.TyCon (_, [], tycon) =>
           let val w = if T.eqTyName (tycon, Typing.primTyName_string) then
                           C8
                       else if T.eqTyName (tycon, Typing.primTyName_string16) then
                           C16
                       else
                           let val overloadMap = case TypedSyntax.TyNameMap.find (#overloadMap env, tycon) of
                                                     SOME m => m
                                                   | NONE => emitFatalError (ctx, [span], "invalid string constant: type")
                               val maxOrd = case Syntax.OverloadKeyMap.find (overloadMap, Syntax.OVERLOAD_maxOrd) of
                                                SOME (F.PrimExp (F.IntConstOp x, _, _)) => x
                                              | _ => (emitError (ctx, [span], "invalid string constant: type"); 65535)
                           in case maxOrd of
                                  255 => C8
                                | 65535 => C16
                                | _ => (emitError (ctx, [span], "invalid string constant: type"); C16)
                           end
           in case w of
                  C8 => let val cooked = StringElement.encode8bit value
                                         handle Chr => (emitError (ctx, [span], "invalid string constant: out of range"); "")
                        in (F.StringConstant cooked, F.PrimExp (F.String8ConstOp cooked, [toFTy (ctx, env, ty)], []))
                        end
                | C16 => let val cooked = StringElement.encode16bit value
                                          handle Chr => (emitError (ctx, [span], "invalid string constant: out of range"); vector [])
                         in (F.String16Constant cooked, F.PrimExp (F.String16ConstOp cooked, [toFTy (ctx, env, ty)], []))
                         end
           end
         | _ => emitFatalError (ctx, [span], "invalid string constant: type")
      )
fun toFPat (ctx : Context, env : Env, T.WildcardPat span) = (TypedSyntax.VIdMap.empty, F.WildcardPat span)
  | toFPat (ctx, env, T.SConPat (span, Syntax.IntegerConstant value, ty))
    = (TypedSyntax.VIdMap.empty, F.SConPat { sourceSpan = span
                                           , scon = F.IntegerConstant value
                                           , equality = getEquality (ctx, env, ty)
                                           , cookedValue = cookIntegerConstant (ctx, env, span, value, ty)
                                           }
      )
  | toFPat (ctx, env, T.SConPat (span, Syntax.WordConstant value, ty))
    = (TypedSyntax.VIdMap.empty, F.SConPat { sourceSpan = span
                                           , scon = F.WordConstant value
                                           , equality = getEquality (ctx, env, ty)
                                           , cookedValue = cookWordConstant (ctx, env, span, value, ty)
                                           }
      )
  | toFPat (ctx, env, T.SConPat (span, Syntax.RealConstant value, ty)) = (emitError (ctx, [span], "invalid real constant in pattern"); (TypedSyntax.VIdMap.empty, F.WildcardPat span))
  | toFPat (ctx, env, T.SConPat (span, Syntax.CharacterConstant value, ty))
    = let val (scon, cookedValue) = cookCharacterConstant (ctx, env, span, value, ty)
      in (TypedSyntax.VIdMap.empty, F.SConPat { sourceSpan = span
                                              , scon = scon
                                              , equality = getEquality (ctx, env, ty)
                                              , cookedValue = cookedValue
                                              }
         )
      end
  | toFPat (ctx, env, T.SConPat (span, Syntax.StringConstant value, ty))
    = let val (scon, cookedValue) = cookStringConstant (ctx, env, span, value, ty)
      in (TypedSyntax.VIdMap.empty, F.SConPat { sourceSpan = span
                                              , scon = scon
                                              , equality = getEquality (ctx, env, ty)
                                              , cookedValue = cookedValue
                                              }
         )
      end
  | toFPat (ctx, env, T.VarPat (span, vid, ty)) = let val ty = toFTy (ctx, env, ty)
                                                  in (TypedSyntax.VIdMap.singleton (vid, ty), F.VarPat (span, vid, ty))
                                                  end
  | toFPat (ctx, env, T.RecordPat { sourceSpan, fields, ellipsis, wholeRecordType })
    = let val (newEnv, ellipsis) = case ellipsis of
                                       NONE => (TypedSyntax.VIdMap.empty, NONE)
                                     | SOME pat => let val (m, pat) = toFPat (ctx, env, pat)
                                                   in (m, SOME pat)
                                                   end
          val (newEnv, fields) = List.foldr (fn ((label, pat), (newEnv, fields)) =>
                                                let val (m, pat) = toFPat (ctx, env, pat)
                                                in (TypedSyntax.VIdMap.unionWith #2 (newEnv, m), (label, pat) :: fields)
                                                end) (newEnv, []) fields
          val allFields = case wholeRecordType of
                              T.RecordType (_, fieldTypes) => Syntax.LabelMap.foldli (fn (label, _, acc) => Syntax.LabelSet.add (acc, label)) Syntax.LabelSet.empty fieldTypes
                            | _ => emitFatalError (ctx, [sourceSpan], "invalid record pattern")
      in (newEnv, F.RecordPat { sourceSpan = sourceSpan, fields = fields, ellipsis = ellipsis, allFields = allFields })
      end
  | toFPat (ctx, env, T.ConPat { sourceSpan = span, longvid, payload, tyargs, valueConstructorInfo })
    = let val (m, payload) = case payload of
                                 NONE => (TypedSyntax.VIdMap.empty, NONE)
                               | SOME (payloadTy, payloadPat) => let val payloadTy = toFTy (ctx, env, payloadTy)
                                                                     val (m, payloadPat) = toFPat (ctx, env, payloadPat)
                                                                 in (m, SOME (payloadTy, payloadPat))
                                                                 end
          val tyargs = List.map (fn ty => toFTy (ctx, env, ty)) tyargs
      in (m, case valueConstructorInfo of
                 SOME info => F.ValConPat { sourceSpan = span, info = info, payload = payload }
               | NONE => (case LongVIdToExnTagExp (ctx, env, [span], longvid) of
                              SOME tagExp => F.ExnConPat { sourceSpan = span, tagPath = tagExp, payload = payload }
                            | NONE => (emitError (ctx, [span], "invalid constructor pattern"); F.WildcardPat span)
                         )
         )
      end
  | toFPat (ctx, env, T.TypedPat (_, pat, _)) = toFPat (ctx, env, pat)
  | toFPat (ctx, env, T.LayeredPat (span, vid, ty, innerPat)) = let val (m, innerPat') = toFPat (ctx, env, innerPat)
                                                                    val ty = toFTy (ctx, env, ty)
                                                                in (TypedSyntax.VIdMap.insert (m, vid, ty), F.LayeredPat (span, vid, ty, innerPat'))
                                                                end
  | toFPat (ctx, env, T.VectorPat (span, pats, ellipsis, elemTy))
    = let val (m, pats) = Vector.foldr (fn (pat, (m, xs)) => let val (m', pat) = toFPat (ctx, env, pat)
                                                             in (TypedSyntax.VIdMap.unionWith #2 (m, m'), pat :: xs)
                                                             end) (TypedSyntax.VIdMap.empty, []) pats
      in (m, F.VectorPat (span, Vector.fromList pats, ellipsis, toFTy (ctx, env, elemTy)))
      end
and toFExp (ctx : Context, env : Env, T.SConExp (span, Syntax.IntegerConstant value, ty)) = cookIntegerConstant (ctx, env, span, value, ty)
  | toFExp (ctx, env, T.SConExp (span, Syntax.WordConstant value, ty)) = cookWordConstant (ctx, env, span, value, ty)
  | toFExp (ctx, env, T.SConExp (span, Syntax.RealConstant value, ty)) = cookRealConstant (ctx, env, span, value, ty)
  | toFExp (ctx, env, T.SConExp (span, Syntax.StringConstant value, ty)) = #2 (cookStringConstant (ctx, env, span, value, ty))
  | toFExp (ctx, env, T.SConExp (span, Syntax.CharacterConstant value, ty)) = #2 (cookCharacterConstant (ctx, env, span, value, ty))
  | toFExp (ctx, env, T.VarExp (span, longvid as TypedSyntax.MkShortVId vid, _, [(tyarg, cts)]))
    = (case TypedSyntax.VIdMap.find (overloads, vid) of
           SOME key => (case tyarg of
                            T.TyCon (_, [], tycon) => (case TypedSyntax.TyNameMap.find (#overloadMap env, tycon) of
                                                           SOME m => (case Syntax.OverloadKeyMap.find (m, key) of
                                                                          SOME exp => exp
                                                                        | NONE => emitFatalError (ctx, [span], "invalid use of " ^ TypedSyntax.print_VId vid)
                                                                     )
                                                         | NONE => emitFatalError (ctx, [span], "invalid use of " ^ TypedSyntax.print_VId vid)
                                                      )
                          | _ => emitFatalError (ctx, [span], "invalid use of " ^ TypedSyntax.print_VId vid)
                       )
         | NONE => if List.exists (fn TypedSyntax.IsEqType => true | _ => false) cts then
                       F.AppExp (F.TyAppExp (#1 (LongVarExp (ctx, env, [span], longvid)), toFTy (ctx, env, tyarg)), getEquality (ctx, env, tyarg))
                   else
                       F.TyAppExp (#1 (LongVarExp (ctx, env, [span], longvid)), toFTy (ctx, env, tyarg))
      )
  | toFExp (ctx, env, T.VarExp (span, longvid, _, tyargs))
    = List.foldl (fn ((ty, cts), e) =>
                     if List.exists (fn TypedSyntax.IsEqType => true | _ => false) cts then
                         F.AppExp(F.TyAppExp(e, toFTy(ctx, env, ty)), getEquality(ctx, env, ty))
                     else
                         F.TyAppExp(e, toFTy(ctx, env, ty))
                 ) (#1 (LongVarExp (ctx, env, [span], longvid))) tyargs
  | toFExp (ctx, env, T.RecordExp (span, fields)) = let fun doField (label, e) = (label, toFExp (ctx, env, e))
                                                    in F.RecordExp (List.map doField fields)
                                                    end
  | toFExp (ctx, env, T.RecordExtExp { sourceSpan, fields, baseExp, baseTy as T.RecordType (_, baseFields) })
    = let fun vidForLabel (Syntax.IdentifierLabel x) = x
            | vidForLabel (Syntax.NumericLabel n) = "field" ^ Int.toString n
          fun doField ((label, e), (decs, fields))
              = let val vid = freshVId (ctx, vidForLabel label)
                    val e = toFExp (ctx, env, e)
                    val dec = F.ValDec (vid, NONE, e)
                in (dec :: decs, (label, F.VarExp vid) :: fields)
                end
          val (decs, fields) = List.foldr doField ([], []) fields
          val baseVId = freshVId (ctx, "base")
          val baseTy = toFTy (ctx, env, baseTy)
          val baseFieldTypes = case baseTy of
                                   F.RecordType fieldTypes => fieldTypes
                                 | _ => emitFatalError (ctx, [sourceSpan], "invalid record type")
          val baseDec = F.ValDec (baseVId, SOME baseTy, toFExp (ctx, env, baseExp))
          val baseExp = F.VarExp baseVId
          val baseFields = Syntax.LabelMap.foldri (fn (label, _, fields) =>
                                                      (label, F.ProjectionExp { label = label, record = baseExp, fieldTypes = baseFieldTypes }) :: fields
                                                  ) [] baseFields
      in List.foldr F.LetExp (F.LetExp (baseDec, F.RecordExp (fields @ baseFields))) decs
      end
  | toFExp (ctx, env, T.RecordExtExp { sourceSpan, fields, baseExp, baseTy }) = emitFatalError (ctx, [sourceSpan], "record extension of non-record type: " ^ T.print_Ty baseTy)
  | toFExp (ctx, env, T.LetInExp (span, decs, e))
    = let val (env, decs) = toFDecs(ctx, env, decs)
      in List.foldr F.LetExp (toFExp(ctx, env, e)) decs
      end
  | toFExp (ctx, env, T.AppExp (span, T.ProjectionExp { label, recordTy, ... }, e2))
    = let val recordTy = toFTy (ctx, env, recordTy)
          val fieldTypes = case recordTy of
                               F.RecordType fieldTypes => fieldTypes
                             | _ => emitFatalError (ctx, [span], "invalid record type")
      in F.ProjectionExp { label = label, record = toFExp (ctx, env, e2), fieldTypes = fieldTypes }
      end
  | toFExp (ctx, env, T.AppExp (span, e1, e2)) = F.AppExp (toFExp (ctx, env, e1), toFExp (ctx, env, e2))
  | toFExp (ctx, env, T.TypedExp (span, exp, _)) = toFExp (ctx, env, exp)
  | toFExp (ctx, env, T.IfThenElseExp (span, e1, e2, e3)) = F.IfThenElseExp (toFExp (ctx, env, e1), toFExp (ctx, env, e2), toFExp (ctx, env, e3))
  | toFExp (ctx, env, T.CaseExp { sourceSpan, subjectExp, subjectTy, matches, matchType, resultTy })
    = let fun doMatch (pat, exp) = let val (valMap, pat') = toFPat (ctx, env, pat)
                                       val env' = updateValMap (fn m => T.VIdMap.unionWith #2 (m, valMap), env)
                                   in (pat', toFExp (ctx, env', exp))
                                   end
      in F.CaseExp { sourceSpan = sourceSpan
                   , subjectExp = toFExp (ctx, env, subjectExp)
                   , subjectTy = toFTy (ctx, env, subjectTy)
                   , matches = List.map doMatch matches
                   , matchType = matchType
                   , resultTy = toFTy (ctx, env, resultTy)
                   }
      end
  | toFExp (ctx, env, T.FnExp (span, vid, ty, body))
    = let val ty = toFTy(ctx, env, ty)
          val env' = updateValMap (fn m => T.VIdMap.insert (m, vid, ty), env)
      in F.FnExp (vid, ty, toFExp (ctx, env', body))
      end
  | toFExp (ctx, env, T.ProjectionExp { sourceSpan = span, label, recordTy, fieldTy = _ })
    = let val vid = freshVId (ctx, "tmp")
          val recordTy = toFTy (ctx, env, recordTy)
          val fieldTypes = case recordTy of
                               F.RecordType fieldTypes => fieldTypes
                             | _ => emitFatalError (ctx, [span], "invalid record type")
      in F.FnExp (vid, recordTy, F.ProjectionExp { label = label, record = F.VarExp vid, fieldTypes = fieldTypes })
      end
  | toFExp (ctx, env, T.HandleExp (span, exp, matches, resultTy))
    = let val exnName = freshVId(ctx, "exn")
          val exnTy = F.TyVar(F.tyNameToTyVar(Typing.primTyName_exn))
          fun doMatch (pat, exp) = let val (valMap, pat') = toFPat (ctx, env, pat)
                                       val env' = updateValMap (fn m => T.VIdMap.unionWith #2 (m, valMap), env)
                                   in (pat', toFExp (ctx, env', exp))
                                   end
          val matches' = List.map doMatch matches
      in F.HandleExp { body = toFExp(ctx, env, exp)
                     , exnName = exnName
                     , handler = F.CaseExp { sourceSpan = span, subjectExp = F.VarExp exnName, subjectTy = exnTy, matches = matches', matchType = T.HANDLE, resultTy = toFTy (ctx, env, resultTy) }
                     }
      end
  | toFExp (ctx, env, T.RaiseExp (span, ty, exp)) = F.RaiseExp (span, toFTy (ctx, env, ty), toFExp (ctx, env, exp))
  | toFExp (ctx, env, T.ListExp (span, xs, ty)) = F.ListExp (Vector.map (fn x => toFExp (ctx, env, x)) xs, toFTy (ctx, env, ty))
  | toFExp (ctx, env, T.VectorExp (span, xs, ty)) = F.VectorExp (Vector.map (fn x => toFExp (ctx, env, x)) xs, toFTy (ctx, env, ty))
  | toFExp (ctx, env, T.PrimExp (span, Primitives.EQUAL, tyargs, args)) = if Vector.length tyargs = 1 andalso Vector.length args = 2 then
                                                                              let val tyarg = Vector.sub (tyargs, 0)
                                                                                  val x = toFExp (ctx, env, Vector.sub (args, 0))
                                                                                  val y = toFExp (ctx, env, Vector.sub (args, 1))
                                                                              in F.AppExp (getEquality (ctx, env, tyarg), F.TupleExp [x, y])
                                                                              end
                                                                          else
                                                                              emitFatalError (ctx, [span], "invalid arguments to primop '=' (" ^ Int.toString (Vector.length tyargs) ^ ", " ^ Int.toString (Vector.length args) ^ ")")
  | toFExp (ctx, env, T.PrimExp (span, primOp, tyargs, args)) = F.PrimExp (F.PrimFnOp primOp, Vector.foldr (fn (ty, xs) => toFTy (ctx, env, ty) :: xs) [] tyargs, Vector.foldr (fn (x, xs) => toFExp (ctx, env, x) :: xs) [] args)
and doValBind ctx env (T.TupleBind (span, vars, exp))
    = let val tupleVId = freshVId (ctx, "tmp")
          val exp = toFExp (ctx, env, exp)
          val vars = List.map (fn (vid, ty) => (vid, toFTy (ctx, env, ty))) vars
          val tupleTy = F.TupleType (List.map #2 vars)
          val tupleFieldTypes = case tupleTy of
                                    F.RecordType fieldTypes => fieldTypes
                                  | _ => emitFatalError (ctx, [span], "invalid tuple")
          val decs = let fun go (i, []) = []
                           | go (i, (vid, ty) :: xs) = F.ValDec (vid, SOME ty, F.ProjectionExp { label = Syntax.NumericLabel i, record = F.VarExp tupleVId, fieldTypes = tupleFieldTypes }) :: go (i + 1, xs)
                     in go (1, vars)
                     end
          val env' = updateValMap (fn m => List.foldl (fn ((vid, ty), m) => T.VIdMap.insert (m, vid, ty)) (T.VIdMap.insert (m, tupleVId, tupleTy)) vars, env)
      in (env', F.ValDec (tupleVId, SOME tupleTy, exp) :: decs)
      end
  | doValBind ctx env (T.PolyVarBind (span, vid, T.TypeScheme (tvs, ty), exp))
    = let val ty0 = toFTy (ctx, env, ty)
          val ty' = List.foldr (fn ((tv,cts),ty1) =>
                                   case cts of
                                       [] => F.ForallType (tv, F.TypeKind, ty1)
                                     | [T.IsEqType] => F.ForallType (tv, F.TypeKind, F.FnType (F.EqualityType (F.TyVar tv), ty1))
                                     | _ => emitFatalError (ctx, [span], "invalid type constraint")
                               ) ty0 tvs
          fun doExp (env', [])
              = toFExp(ctx, env', exp)
            | doExp (env', (tv,cts) :: rest)
              = (case cts of
                     [] => F.TyAbsExp (tv, F.TypeKind, doExp (env', rest))
                   | [T.IsEqType] => let val vid = freshVId (ctx, "eq")
                                         val env'' = updateEqualityForTyVarMap (fn m => TypedSyntax.TyVarMap.insert (m, tv, vid), env')
                                     in F.TyAbsExp (tv, F.TypeKind, F.FnExp (vid, F.EqualityType (F.TyVar tv), doExp (env'', rest)))
                                     end
                   | _ => emitFatalError (ctx, [span], "invalid type constraint")
                )
          val env' = updateValMap (fn m => T.VIdMap.insert (m, vid, ty'), env)
      in (env', [F.ValDec (vid, SOME ty', doExp (env, tvs))])
      end
and typeSchemeToTy (ctx, env : 'dummy2, TypedSyntax.TypeScheme (vars, ty))
    = let fun go env [] = toFTy(ctx, env, ty)
            | go env ((tv, []) :: xs) = let val env' = env (* TODO *)
                                        in F.ForallType(tv, F.TypeKind, go env' xs)
                                        end
            | go env ((tv, [T.IsEqType]) :: xs) = let val env' = env (* TODO *)
                                                  in F.ForallType (tv, F.TypeKind, F.FnType (F.EqualityType (F.TyVar tv), go env' xs))
                                                  end
            | go env ((tv, _) :: xs) = emitFatalError (ctx, [T.getSourceSpanOfTy ty], "invalid type constraint")
      in go env vars
      end
and getEquality (ctx, env, T.TyCon (span, tyargs, tyname))
    = (case TypedSyntax.TyNameMap.find (#equalityForTyNameMap env, tyname) of
           NONE => emitFatalError (ctx, [span], TypedSyntax.PrettyPrint.print_TyName tyname ^ " does not admit equality")
         | SOME longvid => let val typesApplied = List.foldl (fn (tyarg, exp) => F.TyAppExp (exp, toFTy (ctx, env, tyarg))) (#1 (LongVarExp (ctx, env, [span], longvid))) tyargs
                           in if Typing.isRefOrArray tyname then
                                  typesApplied
                              else
                                  List.foldl (fn (tyarg, exp) => F.AppExp (exp, getEquality (ctx, env, tyarg))) typesApplied tyargs
                           end
      )
  | getEquality (ctx, env, T.TyVar (span, tv)) = (case TypedSyntax.TyVarMap.find (#equalityForTyVarMap env, tv) of
                                                      NONE => emitFatalError (ctx, [span], "equality for the type variable not found: " ^ TypedSyntax.PrettyPrint.print_TyVar tv)
                                                    | SOME vid => F.VarExp vid
                                                 )
  | getEquality (ctx, env, T.AnonymousTyVar (span, ref (T.Link ty))) = getEquality (ctx, env, ty)
  | getEquality (ctx, env, T.AnonymousTyVar (span, ref (T.Unbound _))) = emitFatalError (ctx, [span], "unexpected anonymous type variable")
  | getEquality (ctx, env, recordTy as T.RecordType (span, fields))
    = let val param = freshVId (ctx, "a")
      in if Syntax.LabelMap.isEmpty fields then
             F.VarExp Typing.VId_unit_equal
         else
             let val recordTy = toFTy (ctx, env, recordTy)
                 val fieldTypes = case recordTy of
                                      F.RecordType fieldTypes => fieldTypes
                                    | _ => raise Fail "invalid record type"
                 val pairTy = F.PairType (recordTy, recordTy)
                 val lhs = freshVId (ctx, "x")
                 val rhs = freshVId (ctx, "y")
                 val body = F.LetExp ( F.ValDec (lhs, SOME recordTy, F.ProjectionExp { label = Syntax.NumericLabel 1, record = F.VarExp param, fieldTypes = case pairTy of F.RecordType fieldTypes => fieldTypes | _ => emitFatalError (ctx, [span], "invalid record type") })
                                     , F.LetExp ( F.ValDec (rhs, SOME recordTy, F.ProjectionExp { label = Syntax.NumericLabel 2, record = F.VarExp param, fieldTypes = case pairTy of F.RecordType fieldTypes => fieldTypes | _ => emitFatalError (ctx, [span], "invalid record type") })
                                                , Syntax.LabelMap.foldli (fn (label, ty, rest) =>
                                                                             F.SimplifyingAndalsoExp ( F.AppExp ( getEquality (ctx, env, ty)
                                                                                                                , F.TupleExp [ F.ProjectionExp { label = label, record = F.VarExp lhs, fieldTypes = fieldTypes }
                                                                                                                             , F.ProjectionExp { label = label, record = F.VarExp rhs, fieldTypes = fieldTypes }
                                                                                                                             ]
                                                                                                                )
                                                                                                     , rest
                                                                                                     )
                                                                         ) (F.VarExp InitialEnv.VId_true) fields
                                                )
                                     )
             in F.FnExp (param, pairTy, body)
             end
      end
  | getEquality (ctx, env, T.RecordExtType (span, fields, baseTy)) = emitFatalError (ctx, [span], "unexpected record extension")
  | getEquality (ctx, env, T.FnType (span, _, _)) = emitFatalError (ctx, [span], "functions are not equatable; this should have been a type error")
and toFDecs (ctx, env, []) = (env, [])
  | toFDecs (ctx, env, T.ValDec (span, valbinds) :: decs)
    = let val (env, dec) = List.foldl (fn (valbind, (env, decs)) => let val (env, decs') = doValBind ctx env valbind in (env, decs @ decs') end) (env, []) valbinds
          val (env, decs) = toFDecs (ctx, env, decs)
      in (env, dec @ decs)
      end
  | toFDecs (ctx, env, T.RecValDec (span, valbinds) :: decs)
    = let val valbinds' = List.map (fn T.TupleBind (span, vars, exp) => emitFatalError (ctx, [span], "unexpected TupleBind in RecValDec")
                                   | T.PolyVarBind (span, vid, T.TypeScheme (tvs, ty), exp) =>
                                     let val ty0 = toFTy (ctx, env, ty)
                                         val ty' = List.foldr (fn ((tv, cts), ty1) =>
                                                                  case cts of
                                                                      [] => F.ForallType (tv, F.TypeKind, ty1)
                                                                    | [T.IsEqType] => F.ForallType (tv, F.TypeKind, F.FnType (F.EqualityType (F.TyVar tv), ty1))
                                                                    | _ => emitFatalError (ctx, [span], "invalid type constraint")
                                                              ) ty0 tvs
                                     in (span, vid, ty', tvs, ty, exp)
                                     end
                                   ) valbinds
          val valMap = List.foldl (fn ((_, vid, ty, _, _, _), m) => T.VIdMap.insert (m, vid, ty)) (#valMap env) valbinds'
          val env = updateValMap (fn _ => valMap, env)
          val valbinds' = List.map (fn (span, vid, ty', tvs, ty, exp) =>
                                       let fun doExp (env', []) = toFExp (ctx, env', exp)
                                             | doExp (env', (tv, cts) :: rest)
                                               = (case cts of
                                                      [] => F.TyAbsExp (tv, F.TypeKind, doExp (env', rest))
                                                    | [T.IsEqType] => let val vid = freshVId (ctx, "eq")
                                                                          val eqTy = F.EqualityType (F.TyVar tv)
                                                                          val env'' = updateEqualityForTyVarMap (fn m => TypedSyntax.TyVarMap.insert (m, tv, vid), env')
                                                                          val env'' = updateValMap (fn m => T.VIdMap.insert (m, vid, eqTy), env'')
                                                                      in F.TyAbsExp (tv, F.TypeKind, F.FnExp (vid, eqTy, doExp (env'', rest)))
                                                                      end
                                                    | _ => emitFatalError (ctx, [span], "invalid type constraint")
                                                 )
                                       in (vid, ty', doExp (env, tvs))
                                       end) valbinds'
          val (env, decs) = toFDecs (ctx, env, decs)
      in (env, F.RecValDec valbinds' :: decs)
      end
  | toFDecs (ctx, env, T.TypeDec (span, typbinds) :: decs) = toFDecs (ctx, env, decs)
  | toFDecs (ctx, env, T.DatatypeDec (span, datbinds) :: decs)
    = let val dec = F.DatatypeDec (List.map (fn T.DatBind (span, tyvars, tycon, conbinds, _) =>
                                                let val conbinds = List.map (fn T.ConBind (span, vid, NONE, info) => F.ConBind (vid, NONE)
                                                                            | T.ConBind (span, vid, SOME ty, info) => F.ConBind (vid, SOME (toFTy (ctx, env, ty)))
                                                                            ) conbinds
                                                in F.DatBind (tyvars, F.tyNameToTyVar tycon, conbinds)
                                                end
                                            ) datbinds)
          val constructors = List.foldr (fn (T.DatBind (span, tyvars, tycon, conbinds, _), acc) =>
                                            let val baseTy = F.TyCon (List.map F.TyVar tyvars, tycon)
                                            in List.foldr (fn (T.ConBind (span, vid, optPayload, info), acc) =>
                                                              let val (ty, exp) = case optPayload of
                                                                                      NONE => (baseTy, F.PrimExp (F.ConstructValOp info, [baseTy], []))
                                                                                    | SOME payloadTy => let val payloadId = freshVId (ctx, "payload")
                                                                                                            val payloadTy = toFTy (ctx, env, payloadTy)
                                                                                                            val ty = F.FnType (payloadTy, baseTy)
                                                                                                        in (ty, F.FnExp (payloadId, payloadTy, F.PrimExp (F.ConstructValWithPayloadOp info, [baseTy, payloadTy], [F.VarExp payloadId])))
                                                                                                        end
                                                                  val ty = List.foldr (fn (tv, ty) => F.ForallType (tv, F.TypeKind, ty)) ty tyvars
                                                                  val exp = List.foldr (fn (tv, exp) => F.TyAbsExp (tv, F.TypeKind, exp)) exp tyvars
                                                              in (vid, ty, exp) :: acc
                                                              end
                                                          ) acc conbinds
                                            end
                                        ) [] datbinds
          val env = updateValMap (fn m => List.foldl (fn ((vid, ty, _), m) => T.VIdMap.insert (m, vid, ty)) m constructors, env)
          val (env, valbinds) = genEqualitiesForDatatypes(ctx, env, datbinds)
          val (env, decs) = toFDecs(ctx, env, decs)
      in (env, dec :: List.map (fn (vid, ty, exp) => F.ValDec (vid, SOME ty, exp)) constructors @ (if List.null valbinds then decs else F.RecValDec valbinds :: decs))
      end
  | toFDecs (ctx, env as { exnTagMap, ... }, T.ExceptionDec (span, exbinds) :: decs)
    = let val exnTy = FSyntax.TyCon ([], Typing.primTyName_exn)
          val (env, exnTagMap, revExbinds) = List.foldl (fn (T.ExBind (span, vid as TypedSyntax.MkVId (name, _), optPayloadTy), (env, exnTagMap, revExbinds)) =>
                                                            let val tag = freshVId (ctx, name ^ "_tag")
                                                                val optPayloadTy = Option.map (fn ty => toFTy (ctx, env, ty)) optPayloadTy
                                                                val (ty, exp) = case optPayloadTy of
                                                                                    NONE => (exnTy, F.PrimExp (F.ConstructExnOp, [], [F.VarExp tag]))
                                                                                  | SOME payloadTy => let val payloadId = freshVId (ctx, "payload")
                                                                                                          val ty = F.FnType (payloadTy, exnTy)
                                                                                                      in (ty, F.FnExp (payloadId, payloadTy, F.PrimExp (F.ConstructExnWithPayloadOp, [payloadTy], [F.VarExp tag, F.VarExp payloadId])))
                                                                                                      end
                                                                val env = updateValMap (fn m => T.VIdMap.insert (m, vid, ty), env)
                                                                val conDec = F.ValDec (vid, SOME ty, exp)
                                                            in ( env
                                                               , TypedSyntax.VIdMap.insert (exnTagMap, vid, F.VarExp tag)
                                                               , conDec :: F.ExceptionDec { name = name
                                                                                          , tagName = tag
                                                                                          , payloadTy = optPayloadTy
                                                                                          } :: revExbinds
                                                               )
                                                            end
                                                        | (T.ExReplication (span, vid, longvid, optTy), (env, exnTagMap, revExbinds)) =>
                                                          (case LongVIdToExnTagExp (ctx, env, [span], longvid) of
                                                               SOME tagExp => let val conTy = case optTy of
                                                                                                  SOME payloadTy => F.FnType (toFTy (ctx, env, payloadTy), exnTy)
                                                                                                | NONE => exnTy
                                                                                  val env = updateValMap (fn m => T.VIdMap.insert (m, vid, conTy), env)
                                                                                  val dec = F.ValDec (vid, SOME conTy, #1 (LongVarExp (ctx, env, [span], longvid)))
                                                                              in (env, TypedSyntax.VIdMap.insert (exnTagMap, vid, tagExp), dec :: revExbinds)
                                                                              end
                                                             | NONE => emitFatalError (ctx, [span], "exception not found: " ^ TypedSyntax.print_LongVId longvid)
                                                          )
                                                        ) (env, exnTagMap, []) exbinds
          val env = updateExnTagMap (fn _ => exnTagMap, env)
          val (env, decs) = toFDecs(ctx, env, decs)
      in (env, List.rev revExbinds @ decs)
      end
  | toFDecs (ctx, env, T.GroupDec (span, decs) :: decs') = let val (env, decs) = toFDecs (ctx, env, decs)
                                                               val (env, decs') = toFDecs (ctx, env, decs')
                                                           in (env, case decs of
                                                                        [] => decs'
                                                                      | [dec] => dec :: decs'
                                                                      | _ => F.GroupDec (NONE, decs) :: decs'
                                                              )
                                                           end
  | toFDecs (ctx, env, T.OverloadDec (span, class, tyname, map) :: decs) = let val map = Syntax.OverloadKeyMap.map (fn exp => toFExp (ctx, env, exp)) map
                                                                               val env = updateOverloadMap (fn m => TypedSyntax.TyNameMap.insert (m, tyname, map), env)
                                                                               val (env, decs) = toFDecs (ctx, env, decs)
                                                                           in (env, decs)
                                                                           end
  | toFDecs (ctx, env, T.EqualityDec (span, tyvars, tyname, exp) :: decs) = let val vid = freshVId (ctx, "eq")
                                                                                val tyvarEqualities = if Typing.isRefOrArray tyname then
                                                                                                          []
                                                                                                      else
                                                                                                          List.map (fn tv => (tv, freshVId (ctx, "eq"))) tyvars
                                                                                val ty = F.EqualityType (F.TyVar (F.tyNameToTyVar tyname))
                                                                                val ty = List.foldr (fn ((tv, _), ty) => F.FnType (F.EqualityType (F.TyVar tv), ty)) ty tyvarEqualities
                                                                                val ty = List.foldr (fn (tv, ty) => F.ForallType (tv, F.TypeKind, ty)) ty tyvars
                                                                                val env = updateEqualityForTyNameMap (fn m => TypedSyntax.TyNameMap.insert (m, tyname, TypedSyntax.MkShortVId vid), env)
                                                                                val env = updateValMap (fn m => T.VIdMap.insert (m, vid, ty), env)
                                                                                val innerEnv = updateEqualityForTyVarMap (fn m => List.foldl TypedSyntax.TyVarMap.insert' m tyvarEqualities, env)
                                                                                val exp = toFExp (ctx, innerEnv, exp)
                                                                                val exp = List.foldr (fn ((tv, eqParam), exp) => F.FnExp (eqParam, F.EqualityType (F.TyVar tv), exp)) exp tyvarEqualities
                                                                                val exp = List.foldr (fn (tv, exp) => F.TyAbsExp (tv, F.TypeKind, exp)) exp tyvars
                                                                                val dec = F.RecValDec [(vid, ty, exp)]
                                                                                val (env, decs) = toFDecs (ctx, env, decs)
                                                                            in (env, dec :: decs)
                                                                            end
  | toFDecs (ctx, env, T.ValDescDec _ :: decs) = toFDecs (ctx, env, decs)
and genEqualitiesForDatatypes (ctx, env, datbinds) : Env * (TypedSyntax.VId * F.Ty * F.Exp) list
    = let val nameMap = List.foldl (fn (T.DatBind (span, tyvars, tycon as TypedSyntax.MkTyName (name, _), conbinds, true), map) => TypedSyntax.TyNameMap.insert (map, tycon, freshVId (ctx, "EQUAL" ^ name))
                                   | (_, map) => map) TypedSyntax.TyNameMap.empty datbinds
          val env' = updateEqualityForTyNameMap (fn m => TypedSyntax.TyNameMap.unionWith #2 (#equalityForTyNameMap env, TypedSyntax.TyNameMap.map T.MkShortVId nameMap), env)
          fun updateEnv (T.DatBind (span, tyvars, tyname, conbinds, true), env)
              = let val vid = TypedSyntax.TyNameMap.lookup (nameMap, tyname)
                    val tyvars'' = List.map F.TyVar tyvars
                    val ty = List.foldr (fn (tv, ty) => F.FnType (F.EqualityType (F.TyVar tv), ty)) (F.EqualityType (F.TyCon (tyvars'', tyname))) tyvars
                    val ty = List.foldr (fn (tv, ty) => F.ForallType (tv, F.TypeKind, ty)) ty tyvars
                in updateValMap (fn m => T.VIdMap.insert (m, vid, ty), env)
                end
            | updateEnv (_, env) = env
          val env' = List.foldl updateEnv env' datbinds
          fun doDatBind (T.DatBind (span, tyvars, tyname, conbinds, true), valbinds)
              = let val vid = TypedSyntax.TyNameMap.lookup (nameMap, tyname)
                    val tyvars'' = List.map F.TyVar tyvars
                    val ty = List.foldr (fn (tv, ty) => F.FnType(F.EqualityType (F.TyVar tv), ty)) (F.EqualityType (F.TyCon(tyvars'', tyname))) tyvars
                    val ty = List.foldr (fn (tv, ty) => F.ForallType(tv, F.TypeKind, ty)) ty tyvars
                    val tyvars' = List.map (fn tv => (tv, freshVId(ctx, "eq"))) tyvars
                    val eqForTyVars = List.foldl TypedSyntax.TyVarMap.insert' TypedSyntax.TyVarMap.empty tyvars'
                    val env'' = updateEqualityForTyVarMap (fn m => TypedSyntax.TyVarMap.unionWith #2 (m, eqForTyVars), env')
                    val env'' = updateValMap (fn m => List.foldl (fn ((tv, vid), m) => T.VIdMap.insert (m, vid, F.EqualityType (F.TyVar tv))) m tyvars', env'')
                    val body = let val param = freshVId(ctx, "p")
                                   val paramTy = let val ty = F.TyCon(tyvars'', tyname)
                                                 in F.PairType(ty, ty)
                                                 end
                                   val hasMultipleConstructors = case conbinds of
                                                                     [] => true (* ? *)
                                                                   | [_] => false
                                                                   | _ => true
                               in F.FnExp ( param
                                          , paramTy
                                          , F.CaseExp { sourceSpan = span
                                                      , subjectExp = F.VarExp param
                                                      , subjectTy = paramTy
                                                      , matches = List.foldr (fn (T.ConBind (span, conName, NONE, info), rest) =>
                                                                                 let val conPat = F.ValConPat { sourceSpan = span, info = info, payload = NONE }
                                                                                 in ( F.TuplePat (span, [conPat, conPat])
                                                                                    , F.VarExp(InitialEnv.VId_true)
                                                                                    ) :: rest
                                                                                 end
                                                                             | (T.ConBind (span, conName, SOME payloadTy, info), rest) =>
                                                                               let val payload1 = freshVId(ctx, "a")
                                                                                   val payload2 = freshVId(ctx, "b")
                                                                                   val payloadEq = getEquality(ctx, env'', payloadTy)
                                                                                   val payloadTy = toFTy(ctx, env, payloadTy)
                                                                               in ( F.TuplePat (span, [ F.ValConPat { sourceSpan = span, info = info, payload = SOME (payloadTy, F.VarPat (span, payload1, payloadTy)) }
                                                                                                      , F.ValConPat { sourceSpan = span, info = info, payload = SOME (payloadTy, F.VarPat (span, payload2, payloadTy)) }
                                                                                                      ]
                                                                                               )
                                                                                  , F.AppExp(payloadEq, F.TupleExp [F.VarExp(payload1), F.VarExp(payload2)])
                                                                                  ) :: rest
                                                                               end
                                                                             )
                                                                             (if hasMultipleConstructors then
                                                                                  [(F.WildcardPat span, F.VarExp InitialEnv.VId_false)]
                                                                              else
                                                                                  []
                                                                             )
                                                                             conbinds
                                                      , matchType = T.CASE
                                                      , resultTy = F.TyVar (F.tyNameToTyVar Typing.primTyName_bool)
                                                      }
                                          )
                               end
                    val body = List.foldr (fn ((tv, eqParam), body) => F.FnExp ( eqParam
                                                                               , F.EqualityType (F.TyVar tv)
                                                                               , body
                                                                               )
                                          ) body tyvars'
                    val body = List.foldr (fn (tv, body) => F.TyAbsExp(tv, F.TypeKind, body)) body tyvars
                in (vid, ty, body) :: valbinds
                end
            | doDatBind(_, valbinds) = valbinds
          val valbinds = List.foldr doDatBind [] datbinds
      in (env', valbinds)
      end
fun signatureToTy (ctx, env, { valMap, tyConMap, strMap } : T.Signature)
    = let val fields = Syntax.VIdMap.foldli (fn (vid, (tysc, Syntax.ExceptionConstructor), fields) => Syntax.LabelMap.insert (fields, F.ExnTagLabel vid, toFTy (ctx, env, Typing.primTy_exntag))
                                            | (vid, (tysc, _), fields) => fields
                                            ) Syntax.LabelMap.empty valMap
          val fields = Syntax.VIdMap.foldli (fn (vid, (tysc, ids), fields) => Syntax.LabelMap.insert (fields, F.ValueLabel vid, typeSchemeToTy (ctx, env, tysc))) fields valMap
          val fields = Syntax.StrIdMap.foldli (fn (strid, T.MkSignature s, fields) => Syntax.LabelMap.insert (fields, F.StructLabel strid, signatureToTy (ctx, env, s))) fields strMap
      in F.RecordType fields
      end
fun getEqualityForTypeFunction (ctx, env, T.TypeFunction (tyvars, ty))
    = let val tyvars' = List.map (fn tv => (tv, freshVId (ctx, "eq"))) tyvars
          val equalityEnv = updateEqualityForTyVarMap (fn m => List.foldl TypedSyntax.TyVarMap.insert' m tyvars', env)
          val equalityEnv = updateValMap (fn m => List.foldl (fn ((tv, name), m) => T.VIdMap.insert (m, name, F.EqualityType (F.TyVar tv))) m tyvars', equalityEnv)
          val equality = getEquality (ctx, equalityEnv, ty)
          val equality = List.foldr (fn ((tv, eqParam), body) => F.FnExp (eqParam, F.EqualityType (F.TyVar tv), body)) equality tyvars'
          val equality = List.foldr (fn (tv, body) => F.TyAbsExp (tv, F.TypeKind, body)) equality tyvars
      in equality
      end
fun strExpToFExp (ctx, env : Env, T.StructExp { sourceSpan, valMap, tyConMap, strMap }) : Env * F.Dec list * F.Exp * F.Ty
    = let val acc = Syntax.VIdMap.foldri (fn (vid, (longvid, Syntax.ExceptionConstructor), (fieldTypes, fields)) =>
                                                              let val label = F.ExnTagLabel vid
                                                              in ( Syntax.LabelMap.insert (fieldTypes, label, F.TyCon ([], Typing.primTyName_exntag))
                                                                 , ( label
                                                                   , case LongVIdToExnTagExp (ctx, env, [sourceSpan], longvid) of
                                                                         SOME exp => exp
                                                                       | NONE => emitFatalError (ctx, [sourceSpan], "exception tag not found for " ^ TypedSyntax.print_LongVId longvid)
                                                                   ) :: fields
                                                                 )
                                                              end
                                                          | (vid, (longvid, _), acc) => acc) (Syntax.LabelMap.empty, []) valMap
          val acc = Syntax.StrIdMap.foldri (fn (strid, longstrid, (fieldTypes, fields)) =>
                                               let val label = F.StructLabel strid
                                                   val (exp, ty) = LongStrIdExp (ctx, env, [sourceSpan], longstrid)
                                               in (Syntax.LabelMap.insert (fieldTypes, label, ty), (label, exp) :: fields)
                                               end
                                           ) acc strMap
          val (fieldTypes, fields) = Syntax.VIdMap.foldri (fn (vid, (longvid, ids), (fieldTypes, fields)) =>
                                                              let val label = F.ValueLabel vid
                                                                  val (exp, ty) = LongVarExp (ctx, env, [sourceSpan], longvid)
                                                              in (Syntax.LabelMap.insert (fieldTypes, label, ty), (label, exp) :: fields)
                                                              end
                                                          ) acc valMap
      in (env, [], F.RecordExp fields, F.RecordType fieldTypes)
      end
  | strExpToFExp (ctx, env, T.StrIdExp (span, longstrid)) = let val (exp, ty) = LongStrIdExp (ctx, env, [span], longstrid)
                                                            in (env, [], exp, ty)
                                                            end
  | strExpToFExp (ctx, env, T.PackedStrExp { sourceSpan, strExp, payloadTypes, packageSig })
    = let val (env', decs, exp, _) = strExpToFExp (ctx, env, strExp)
          val packageTy = signatureToTy (ctx, env, #s packageSig)
          fun EqualityTyForArity 0 xs t = List.foldl F.FnType (F.EqualityType t) xs
            | EqualityTyForArity n xs t = let val tv = freshTyVar ctx
                                          in F.ForallType (tv, F.TypeKind, EqualityTyForArity (n - 1) (F.EqualityType (F.TyVar tv) :: xs) (F.AppType { applied = t, arg = F.TyVar tv }))
                                          end
          val (exp, packageTy) = ListPair.foldrEq (fn (typeFunction as T.TypeFunction (tyvars, payloadTy), { tyname, arity, admitsEquality }, (exp, packageTy)) =>
                                                      let val kind = F.arityToKind arity
                                                          val payloadTy' = List.foldr (fn (tv, ty) => F.TypeFn (tv, F.TypeKind, ty)) (toFTy (ctx, env', payloadTy)) tyvars
                                                      in if admitsEquality then
                                                             let val packageTy = F.ExistsType (F.tyNameToTyVar tyname, kind, F.PairType(EqualityTyForArity arity [] (F.TyVar (F.tyNameToTyVar tyname)), packageTy)) (* exists 'a. ('a * 'a -> bool) * packageTy / exists t. (forall 'a. forall 'b. ... ('a * 'a -> bool) -> ('b * 'b -> bool) -> ... -> (t 'a 'b ... * t 'a 'b ... -> bool)) * packageTy *)
                                                                 val equality = getEqualityForTypeFunction (ctx, env', typeFunction)
                                                             in (F.PackExp { payloadTy = payloadTy', exp = F.TupleExp [equality, exp], packageTy = packageTy }, packageTy)
                                                             end
                                                         else
                                                             let val packageTy = F.ExistsType (F.tyNameToTyVar tyname, kind, packageTy)
                                                             in (F.PackExp { payloadTy = payloadTy', exp = exp, packageTy = packageTy }, packageTy)
                                                             end
                                                      end
                                                  ) (exp, packageTy) (payloadTypes, #bound packageSig)
      in (env', decs, exp, packageTy)
      end
  | strExpToFExp (ctx, env, T.FunctorAppExp { sourceSpan, funId, argumentTypes, argumentStr, packageSig })
    = let val (env', decs, argumentStr, _) = strExpToFExp (ctx, env, argumentStr)
          (* val packageTy = signatureToTy (ctx, env, #s packageSig) *)
          (* <funid> <argument type>... <argument type's equality>... <structure> *)
          val funId' = case funId of T.MkFunId (name, n) => T.MkVId (name, n) (* the functor id *)
          val exp = F.VarExp funId'
          val ty = case T.VIdMap.find (#valMap env, funId') of
                       SOME ty => ty
                     | NONE => emitFatalError (ctx, [sourceSpan], "undefined functor")
          val exp = List.foldl (fn ({ typeFunction = T.TypeFunction (tyvars, ty), admitsEquality = _ }, exp) =>
                                   let val ty = List.foldr (fn (tv, ty) => F.TypeFn (tv, F.TypeKind, ty)) (toFTy (ctx, env', ty)) tyvars
                                   in F.TyAppExp (exp, ty)
                                   end
                               ) exp argumentTypes (* apply the types *)
          val ty = List.foldl (fn (_, F.ForallType (_, _, ty)) => ty
                              | (_, _) => emitFatalError (ctx, [sourceSpan], "invalid functor type")
                              ) ty argumentTypes (* apply the types *)
          val exp = List.foldl (fn ({ typeFunction, admitsEquality = true }, exp) => F.AppExp (exp, getEqualityForTypeFunction (ctx, env, typeFunction))
                               | ({ typeFunction = _, admitsEquality = false }, exp) => exp
                               ) exp argumentTypes (* apply the equalities *)
          val ty = List.foldl (fn ({ typeFunction, admitsEquality = true }, F.FnType (_, ty)) => ty
                              | ({ typeFunction, admitsEquality = true }, _) => emitFatalError (ctx, [sourceSpan], "invalid functor type")
                              | ({ typeFunction = _, admitsEquality = false }, ty) => ty
                              ) ty argumentTypes (* apply the equalities *)
          val exp = F.AppExp (exp, argumentStr) (* apply the structure *)
          val ty = case ty of
                       F.FnType (_, ty) => ty
                     | _ => emitFatalError (ctx, [sourceSpan], "invalid functor type")
      in (env (* What to do? *), decs, exp, ty)
      end
  | strExpToFExp (ctx, env, T.LetInStrExp (span, strdecs, strexp)) = let val (env', decs) = strDecsToFDecs (ctx, env, strdecs)
                                                                         val (env', decs', exp, ty) = strExpToFExp (ctx, env', strexp)
                                                                     in (env', decs @ decs', exp, ty)
                                                                     end
and strDecToFDecs (ctx, env : Env, T.CoreDec (span, dec)) = toFDecs (ctx, env, [dec])
  | strDecToFDecs (ctx, env, T.StrBindDec (span, strid, strexp, { s, bound }))
    = let val vid = F.strIdToVId strid
          val ty = signatureToTy (ctx, env, s)
          val (env', decs0, exp, packageTy) = strExpToFExp (ctx, env, strexp)
          val env'' = updateEqualityForTyNameMap (fn m => TypedSyntax.TyNameMap.unionWith #2 (m, #equalityForTyNameMap env'), env)
          val env'' = updateValMap (fn m => T.VIdMap.insert (m, vid, ty), env'')
          val (revDecs, exp, ty', env) = List.foldl (fn ({ tyname, arity, admitsEquality }, (revDecs, exp, packageTy, env)) =>
                                                        case packageTy of
                                                            F.ExistsType (tv, _, payloadTy) =>
                                                            if admitsEquality then
                                                                case payloadTy of
                                                                    F.RecordType fieldTypes =>
                                                                    let val packageVId = freshVId (ctx, case vid of T.MkVId (name, _) => name)
                                                                        val equalityVId = freshVId (ctx, "eq")
                                                                        val equalityTy = case Syntax.LabelMap.find (fieldTypes, Syntax.NumericLabel 1) of
                                                                                             SOME ty => ty
                                                                                           | NONE => emitFatalError (ctx, [span], "invalid record")
                                                                        val strVId = freshVId (ctx, case vid of T.MkVId (name,_) => name)
                                                                        val strTy = case Syntax.LabelMap.find (fieldTypes, Syntax.NumericLabel 2) of
                                                                                             SOME ty => ty
                                                                                           | NONE => emitFatalError (ctx, [span], "invalid record")
                                                                        val env = updateEqualityForTyNameMap (fn m => T.TyNameMap.insert (m, tyname, T.MkShortVId equalityVId), env)
                                                                        val env = updateValMap (fn m => T.VIdMap.insert (T.VIdMap.insert (T.VIdMap.insert (m, packageVId, payloadTy), equalityVId, equalityTy), strVId, strTy), env)
                                                                    in ( F.ValDec (equalityVId, SOME equalityTy, F.ProjectionExp { label = Syntax.NumericLabel 1, record = F.VarExp packageVId, fieldTypes = fieldTypes })
                                                                         :: F.ValDec (strVId, SOME strTy, F.ProjectionExp { label = Syntax.NumericLabel 2, record = F.VarExp packageVId, fieldTypes = fieldTypes })
                                                                         :: F.UnpackDec (F.tyNameToTyVar tyname, F.arityToKind arity, packageVId, payloadTy, exp)
                                                                         :: revDecs
                                                                       , F.VarExp strVId
                                                                       , strTy
                                                                       , env
                                                                       )
                                                                    end
                                                                  | _ => emitFatalError (ctx, [span], "expected RecordType")
                                                            else
                                                                let val vid = freshVId (ctx, case vid of T.MkVId (name, _) => name)
                                                                    val env = updateValMap (fn m => T.VIdMap.insert (m, vid, payloadTy), env)
                                                                in (F.UnpackDec (F.tyNameToTyVar tyname, F.arityToKind arity, vid, payloadTy, exp) :: revDecs, F.VarExp vid, payloadTy, env)
                                                                end
                                                          | _ => emitFatalError (ctx, [span], "expected ExistsType, but got " ^ F.PrettyPrint.print_Ty packageTy)
                                                    ) ([], exp, packageTy, env'') bound
          (* ty and ty' should be the same *)
      in (env, [F.GroupDec(NONE, decs0 @ List.rev (F.ValDec (vid, SOME ty, exp) :: revDecs))])
      end
  | strDecToFDecs (ctx, env, T.GroupStrDec (span, decs)) = let val (env, decs) = strDecsToFDecs (ctx, env, decs)
                                                           in (env, case decs of
                                                                        [] => decs
                                                                      | [_] => decs
                                                                      | _ => [F.GroupDec (NONE, decs)]
                                                              )
                                                         end
and strDecsToFDecs(ctx, env : Env, []) = (env, [])
  | strDecsToFDecs(ctx, env, dec :: decs) = let val (env, dec) = strDecToFDecs(ctx, env, dec)
                                                val (env, decs) = strDecsToFDecs(ctx, env, decs)
                                            in (env, dec @ decs)
                                            end
fun funDecToFDec(ctx, env, (funid, (types, paramStrId, paramSig, bodyStr))) : Env * F.Dec
    = let val funid = case funid of T.MkFunId (name, n) => T.MkVId (name, n)
          val paramId = case paramStrId of T.MkStrId (name, n) => T.MkVId (name, n)
          val (equalityForTyNameMap, valMap, equalityVars)
              = List.foldr (fn ({ tyname, arity, admitsEquality = true }, (m, valMap, xs)) =>
                               let val vid = freshVId (ctx, "eq")
                                   val tyvars = List.tabulate (arity, fn _ => freshTyVar ctx)
                                   val ty = F.EqualityType (F.TyVar (F.tyNameToTyVar tyname))
                                   val ty = List.foldr (fn (tv, ty) => F.FnType (F.EqualityType (F.TyVar tv), ty)) ty tyvars
                                   val ty = List.foldr (fn (tv, ty) => F.ForallType (tv, F.TypeKind, ty)) ty tyvars
                               in (T.TyNameMap.insert (m, tyname, T.MkShortVId vid), T.VIdMap.insert (valMap, vid, ty), (tyname, arity, vid) :: xs)
                               end
                           | ({ admitsEquality = false, ... }, acc) => acc
                           ) (#equalityForTyNameMap env, #valMap env, []) types
          val env' = updateEqualityForTyNameMap (fn _ => equalityForTyNameMap, env)
          val env' = updateValMap (fn _ => valMap, env')
          val paramSigTy = signatureToTy (ctx, env, paramSig)
          val env' = updateValMap (fn m => T.VIdMap.insert (m, paramId, paramSigTy), env')
          val (_, bodyDecs, bodyExp, bodyTy) = strExpToFExp (ctx, env', bodyStr)
          val funexp = F.FnExp (paramId, paramSigTy, List.foldr F.LetExp bodyExp bodyDecs)
          val funTy = F.FnType (paramSigTy, bodyTy)
          val funexp = List.foldr (fn ((tyname, arity, vid), funexp) => F.FnExp (vid, F.EqualityType (F.TyVar (F.tyNameToTyVar tyname)), funexp)) funexp equalityVars (* equalities *)
          val funTy = List.foldr (fn ((tyname, arity, _), funTy) => F.FnType (F.EqualityType (F.TyVar (F.tyNameToTyVar tyname)), funTy)) funTy equalityVars (* equalities *)
          val funexp = List.foldr (fn ({ tyname, arity, admitsEquality = _ }, funexp) => F.TyAbsExp (F.tyNameToTyVar tyname, F.arityToKind arity, funexp)) funexp types (* type parameters *)
          val funTy = List.foldr (fn ({ tyname, arity, admitsEquality = _ }, funTy) => F.ForallType (F.tyNameToTyVar tyname, F.arityToKind arity, funTy)) funTy types (* type parameters *)
          val env = updateValMap (fn m => T.VIdMap.insert (m, funid, funTy), env)
      in (env, F.ValDec (funid, SOME funTy, funexp))
      end
fun programToFDecs(ctx, env : Env, []) = (env, [])
  | programToFDecs (ctx, env, TypedSyntax.StrDec dec :: topdecs) = let val (env, decs) = strDecToFDecs (ctx, env, dec)
                                                                       val (env, decs') = programToFDecs (ctx, env, topdecs)
                                                                   in (env, decs @ decs')
                                                                   end
  | programToFDecs (ctx, env, TypedSyntax.FunDec dec :: topdecs) = let val (env, dec) = funDecToFDec (ctx, env, dec)
                                                                       val (env, decs) = programToFDecs (ctx, env, topdecs)
                                                                   in (env, dec :: decs)
                                                                   end
fun isAlphaNumName name = List.all (fn c => Char.isAlphaNum c orelse c = #"_") (String.explode name)
fun addExport (ctx, tenv : Typing.Env, toFEnv : Env, decs)
    = case (Syntax.VIdMap.find (#valMap tenv, Syntax.MkVId "export"), Syntax.StrIdMap.find (#strMap tenv, Syntax.MkStrId "export")) of
          (NONE, NONE) => (emitError (ctx, [], "No value to export was found."); decs)
        | (SOME (_, _, longvid), NONE) => decs @ [ F.ExportValue (#1 (LongVarExp (ctx, toFEnv, [], longvid))) ]
        | (NONE, SOME ({ valMap, ... }, T.MkLongStrId (strid0, strids))) =>
          let val fields = Syntax.VIdMap.listItems (Syntax.VIdMap.mapPartiali (fn (vid, _) => let val name = Syntax.getVIdName vid
                                                                                              in if isAlphaNumName name then
                                                                                                     SOME (name, #1 (LongVarExp (ctx, toFEnv, [], T.MkLongVId (strid0, strids, vid))))
                                                                                                 else if String.isSuffix "'" name then
                                                                                                     let val name' = String.substring (name, 0, String.size name - 1)
                                                                                                     in if isAlphaNumName name' andalso not (Syntax.VIdMap.inDomain (valMap, Syntax.MkVId name')) then
                                                                                                            SOME (name', #1 (LongVarExp (ctx, toFEnv, [], T.MkLongVId (strid0, strids, vid))))
                                                                                                        else
                                                                                                            NONE
                                                                                                     end
                                                                                                 else
                                                                                                     NONE
                                                                                              end
                                                                              ) valMap)
          in decs @ [ F.ExportModule (Vector.fromList fields) ]
          end
        | (SOME _, SOME _) => (emitError (ctx, [], "The value to export is ambiguous."); decs)

val initialEnv : Env = { equalityForTyVarMap = TypedSyntax.TyVarMap.empty
                       , equalityForTyNameMap = TypedSyntax.TyNameMap.empty
                       , exnTagMap = let open InitialEnv
                                     in List.foldl (fn ((con, tag), m) => TypedSyntax.VIdMap.insert (m, con, FSyntax.VarExp tag)) TypedSyntax.VIdMap.empty
                                                   [(VId_Match, VId_Match_tag)
                                                   ,(VId_Bind, VId_Bind_tag)
                                                   ,(VId_Div, VId_Div_tag)
                                                   ,(VId_Overflow, VId_Overflow_tag)
                                                   ,(VId_Size, VId_Size_tag)
                                                   ,(VId_Subscript, VId_Subscript_tag)
                                                   ,(VId_Fail, VId_Fail_tag)
                                                   ,(VId_Lua_LuaError, VId_Lua_LuaError_tag)
                                                   ]
                                     end
                       , overloadMap = TypedSyntax.TyNameMap.empty
                       , valMap = let open InitialEnv
                                      val initialValMap = #valMap initialEnv
                                      fun toFTy (T.TyVar (span, tv)) = F.TyVar tv
                                        | toFTy (T.AnonymousTyVar (span, ref (T.Link ty))) = toFTy ty
                                        | toFTy (T.AnonymousTyVar (span, ref (T.Unbound _))) = raise Fail "unexpected anonymous type variable"
                                        | toFTy (T.RecordType (span, fields)) = F.RecordType (Syntax.LabelMap.map toFTy fields)
                                        | toFTy (T.RecordExtType (span, fields, baseTy)) = raise Fail "unexpected record extension"
                                        | toFTy (T.TyCon (span, tyargs, tyname)) = F.TyCon (List.map toFTy tyargs, tyname)
                                        | toFTy (T.FnType (span, paramTy, resultTy)) = F.FnType (toFTy paramTy, toFTy resultTy)
                                      fun typeSchemeToTy (TypedSyntax.TypeScheme (vars, ty))
                                          = let fun go [] = toFTy ty
                                                  | go ((tv, []) :: xs) = F.ForallType (tv, F.TypeKind, go xs)
                                                  | go ((tv, [T.IsEqType]) :: xs) = F.ForallType (tv, F.TypeKind, F.FnType (F.EqualityType (F.TyVar tv), go xs))
                                                  | go ((tv, _) :: xs) = raise Fail "invalid type scheme"
                                            in go vars
                                            end
                                      val initialValMap = Syntax.VIdMap.foldl (fn ((tysc, ids, vid), m) => case vid of
                                                                                                               TypedSyntax.MkShortVId vid => TypedSyntax.VIdMap.insert (m, vid, typeSchemeToTy tysc)
                                                                                                             | TypedSyntax.MkLongVId _ => raise Fail "unexpected longvid") TypedSyntax.VIdMap.empty initialValMap
                                  in List.foldl TypedSyntax.VIdMap.insert' initialValMap
                                                [(VId_Match_tag, FSyntax.TyCon ([], Typing.primTyName_exntag))
                                                ,(VId_Bind_tag, FSyntax.TyCon ([], Typing.primTyName_exntag))
                                                ,(VId_Div_tag, FSyntax.TyCon ([], Typing.primTyName_exntag))
                                                ,(VId_Overflow_tag, FSyntax.TyCon ([], Typing.primTyName_exntag))
                                                ,(VId_Size_tag, FSyntax.TyCon ([], Typing.primTyName_exntag))
                                                ,(VId_Subscript_tag, FSyntax.TyCon ([], Typing.primTyName_exntag))
                                                ,(VId_Fail_tag, FSyntax.TyCon ([], Typing.primTyName_exntag))
                                                ,(VId_Lua_LuaError_tag, FSyntax.TyCon ([], Typing.primTyName_exntag))
                                                ]
                                  end
                       }
end (* local *)
end (* structure ToFSyntax *)
