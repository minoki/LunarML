(*
 * Copyright (c) 2023 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure CSyntax :>
sig
  exception InvalidCode of string
  type Var = TypedSyntax.VId
  structure CVar:
  sig
    eqtype t
    type ord_key = t
    val compare: ord_key * ord_key -> order
    val fromInt: int -> t
    val toInt: t -> int
    val dummy: t
  end
  type CVar = CVar.t
  structure CVarSet: ORD_SET where type Key.ord_key = CVar.t
  structure CVarMap: ORD_MAP where type Key.ord_key = CVar.t
  structure CVarTable: MONO_HASH_TABLE where type Key.hash_key = CVar.t
  type Ty = FSyntax.Ty
  datatype Value =
    Var of Var
  | Unit (* : unit *)
  | Nil (* : forall 'a. 'a list *)
  | TypedNil of Ty (* : 'a list *)
  | BoolConst of bool
  | IntConst of Primitives.int_width * IntInf.int
  | WordConst of Primitives.word_width * IntInf.int
  | CharConst of char
  | Char7Const of char
  | Char16Const of int
  | Char32Const of int
  | UCharConst of int
  | StringConst of string
  | String7Const of string
  | String16Const of int vector
  | String32Const of int vector
  | UStringConst of int vector
  | PrimEffect of Primitives.prim_effect
  | Cast of {value: Value, from: Ty, to: Ty}
  | Pack of {value: Value, payloadTy: Ty, packageTy: Ty}
  type AbsAttr = {alwaysInline: bool, typeOnly: bool}
  type ContAttr = {alwaysInline: bool}
  type AppAttr = {typeOnly: bool}
  datatype SimpleExp =
    PrimOp of {primOp: FSyntax.PrimOp, tyargs: Ty list, args: Value list}
  | Record of Value Syntax.LabelMap.map (* non-empty record *)
  | ExnTag of {name: string, payloadTy: Ty option}
  | Projection of
      {label: Syntax.Label, record: Value, fieldTypes: Ty Syntax.LabelMap.map}
  | Abs of
      { contParam: CVar
      , tyParams: (FSyntax.TyVar * FSyntax.Kind) list
      , params: (Var * Ty) list
      , body: Stat
      , resultTy: Ty
      , attr: AbsAttr
      } (* non-recursive function *)
  and Dec =
    ValDec of {exp: SimpleExp, results: (Var option * Ty) list}
  | RecDec of
      { name: Var
      , contParam: CVar
      , tyParams: (FSyntax.TyVar * FSyntax.Kind) list
      , params: (Var * Ty) list
      , body: Stat
      , resultTy: Ty
      , attr: AbsAttr
      } list (* recursive function *)
  | UnpackDec of
      { tyVar: FSyntax.TyVar
      , kind: FSyntax.Kind
      , vid: Var
      , unpackedTy: Ty
      , package: Value
      }
  | ContDec of
      {name: CVar, params: (Var option * Ty) list, body: Stat, attr: ContAttr}
  | RecContDec of (CVar * (Var option * Ty) list * Stat) list
  | DatatypeDec of FSyntax.TyVar * FSyntax.Kind
  | ESImportDec of
      { pure: bool
      , specs: (Syntax.ESImportName * Var * Ty) list
      , moduleName: string
      }
  and Stat =
    Let of {decs: Dec list, cont: Stat}
  | App of
      { applied: Value
      , cont: CVar
      , tyArgs: Ty list
      , args: Value list
      , attr: AppAttr
      } (* tail call *) (* return arity? *)
  | AppCont of {applied: CVar, args: Value list}
  | If of {cond: Value, thenCont: Stat, elseCont: Stat}
  | Handle of
      { body: Stat
      , handler: Var * Stat
      , successfulExitIn: CVar
      , successfulExitOut: CVar
      , resultTy: Ty
      }
  | Raise of SourcePos.span * Value
  | Unreachable
  val extractVarFromValue: Value -> Var option
  val isDiscardable: SimpleExp -> bool
  val containsApp: Stat -> bool
  val freeVarsInExp: TypedSyntax.VIdSet.set * Stat * TypedSyntax.VIdSet.set
                     -> TypedSyntax.VIdSet.set
  val recurseStat: (Stat -> Stat) -> Stat -> Stat
  val valueToString: Value -> string
  val simpleExpToString: SimpleExp -> string
end =
struct
  exception InvalidCode of string
  type Var = TypedSyntax.VId
  structure CVar :>
  sig
    eqtype t
    type ord_key = t
    val compare: ord_key * ord_key -> order
    val fromInt: int -> t
    val toInt: t -> int
    val dummy: t
  end =
  struct
    type t = int
    type ord_key = t
    val compare = Int.compare
    fun fromInt x = x
    fun toInt x = x
    val dummy = ~1
  end
  type CVar = CVar.t
  structure CVarSet = RedBlackSetFn(CVar)
  structure CVarMap = RedBlackMapFn(CVar)
  structure CVarTable =
    HashTableFn
      (struct
         type hash_key = CVar.t
         fun hashVal x =
           Word.fromInt (CVar.toInt x)
         fun sameKey (x: CVar.t, y: CVar.t) = x = y
       end)
  type Ty = FSyntax.Ty
  datatype Value =
    Var of Var
  | Unit (* : unit *)
  | Nil (* : forall 'a. 'a list *)
  | TypedNil of Ty (* : 'a list *)
  | BoolConst of bool
  | IntConst of Primitives.int_width * IntInf.int
  | WordConst of Primitives.word_width * IntInf.int
  | CharConst of char
  | Char7Const of char
  | Char16Const of int
  | Char32Const of int
  | UCharConst of int
  | StringConst of string
  | String7Const of string
  | String16Const of int vector
  | String32Const of int vector
  | UStringConst of int vector
  (*
  | RealConst of Numeric.float_notation
  *)
  | PrimEffect of Primitives.prim_effect
  | Cast of {value: Value, from: Ty, to: Ty}
  | Pack of {value: Value, payloadTy: Ty, packageTy: Ty}
  type AbsAttr = {alwaysInline: bool, typeOnly: bool}
  type ContAttr = {alwaysInline: bool}
  type AppAttr = {typeOnly: bool}
  datatype SimpleExp =
    PrimOp of {primOp: FSyntax.PrimOp, tyargs: Ty list, args: Value list}
  | Record of Value Syntax.LabelMap.map (* non-empty record *)
  | ExnTag of {name: string, payloadTy: Ty option}
  | Projection of
      {label: Syntax.Label, record: Value, fieldTypes: Ty Syntax.LabelMap.map}
  | Abs of
      { contParam: CVar
      , tyParams: (FSyntax.TyVar * FSyntax.Kind) list
      , params: (Var * Ty) list
      , body: Stat
      , resultTy: Ty
      , attr: AbsAttr
      } (* non-recursive function *)
  and Dec =
    ValDec of {exp: SimpleExp, results: (Var option * Ty) list}
  | RecDec of
      { name: Var
      , contParam: CVar
      , tyParams: (FSyntax.TyVar * FSyntax.Kind) list
      , params: (Var * Ty) list
      , body: Stat
      , resultTy: Ty
      , attr: AbsAttr
      } list (* recursive function *)
  | UnpackDec of
      { tyVar: FSyntax.TyVar
      , kind: FSyntax.Kind
      , vid: Var
      , unpackedTy: Ty (* type of the variable *)
      , package: Value
      }
  | ContDec of
      {name: CVar, params: (Var option * Ty) list, body: Stat, attr: ContAttr}
  | RecContDec of (CVar * (Var option * Ty) list * Stat) list
  | DatatypeDec of FSyntax.TyVar * FSyntax.Kind
  | ESImportDec of
      { pure: bool
      , specs: (Syntax.ESImportName * Var * Ty) list
      , moduleName: string
      }
  and Stat =
    Let of {decs: Dec list, cont: Stat}
  | App of
      { applied: Value
      , cont: CVar
      , tyArgs: Ty list
      , args: Value list
      , attr: AppAttr
      } (* tail call *) (* return arity? *)
  | AppCont of {applied: CVar, args: Value list}
  | If of {cond: Value, thenCont: Stat, elseCont: Stat}
  | Handle of
      { body: Stat
      , handler: Var * Stat
      , successfulExitIn: CVar
      , successfulExitOut: CVar
      , resultTy: Ty
      }
  | Raise of SourcePos.span * Value
  | Unreachable
  fun extractVarFromValue (Var v) = SOME v
    | extractVarFromValue Unit = NONE
    | extractVarFromValue Nil = NONE
    | extractVarFromValue (TypedNil _) = NONE
    | extractVarFromValue (BoolConst _) = NONE
    | extractVarFromValue (IntConst _) = NONE
    | extractVarFromValue (WordConst _) = NONE
    | extractVarFromValue (CharConst _) = NONE
    | extractVarFromValue (Char7Const _) = NONE
    | extractVarFromValue (Char16Const _) = NONE
    | extractVarFromValue (Char32Const _) = NONE
    | extractVarFromValue (UCharConst _) = NONE
    | extractVarFromValue (StringConst _) = NONE
    | extractVarFromValue (String7Const _) = NONE
    | extractVarFromValue (String16Const _) = NONE
    | extractVarFromValue (String32Const _) = NONE
    | extractVarFromValue (UStringConst _) = NONE
    | extractVarFromValue (PrimEffect _) = NONE
    | extractVarFromValue (Cast {value, ...}) = extractVarFromValue value
    | extractVarFromValue (Pack {value, ...}) = extractVarFromValue value
  local structure F = FSyntax
  in
    fun isDiscardablePrimEffect (PrimEffect e) =
          (case e of
             Primitives.PURE => true
           | Primitives.DISCARDABLE => true
           | Primitives.IMPURE => false)
      | isDiscardablePrimEffect _ = false
    fun getPrimEffect (PrimEffect e) = e
      | getPrimEffect _ = Primitives.IMPURE
    fun isDiscardable (PrimOp {primOp = F.IntConstOp _, ...}) = true
      | isDiscardable (PrimOp {primOp = F.WordConstOp _, ...}) = true
      | isDiscardable (PrimOp {primOp = F.RealConstOp _, ...}) = true
      | isDiscardable (PrimOp {primOp = F.Char7ConstOp _, ...}) = true
      | isDiscardable (PrimOp {primOp = F.Char8ConstOp _, ...}) = true
      | isDiscardable (PrimOp {primOp = F.Char16ConstOp _, ...}) = true
      | isDiscardable (PrimOp {primOp = F.Char32ConstOp _, ...}) = true
      | isDiscardable (PrimOp {primOp = F.UCharConstOp _, ...}) = true
      | isDiscardable (PrimOp {primOp = F.String7ConstOp _, ...}) = true
      | isDiscardable (PrimOp {primOp = F.String8ConstOp _, ...}) = true
      | isDiscardable (PrimOp {primOp = F.String16ConstOp _, ...}) = true
      | isDiscardable (PrimOp {primOp = F.String32ConstOp _, ...}) = true
      | isDiscardable (PrimOp {primOp = F.UStringConstOp _, ...}) = true
      | isDiscardable (PrimOp {primOp = F.RaiseOp _, ...}) = false
      | isDiscardable (PrimOp {primOp = F.ListOp, ...}) = true
      | isDiscardable (PrimOp {primOp = F.VectorOp, ...}) = true
      | isDiscardable (PrimOp {primOp = F.DataTagAsStringOp _, ...}) = true
      | isDiscardable (PrimOp {primOp = F.DataTagAsString16Op _, ...}) = true
      | isDiscardable (PrimOp {primOp = F.DataPayloadOp _, ...}) = true
      | isDiscardable (PrimOp {primOp = F.ExnPayloadOp, ...}) = true
      | isDiscardable (PrimOp {primOp = F.ConstructValOp _, ...}) = true
      | isDiscardable (PrimOp {primOp = F.ConstructValWithPayloadOp _, ...}) =
          true
      | isDiscardable (PrimOp {primOp = F.ConstructExnOp, ...}) = true
      | isDiscardable (PrimOp {primOp = F.ConstructExnWithPayloadOp, ...}) =
          true
      | isDiscardable
          (PrimOp
             { primOp = F.PrimCall (Primitives.Array_array _)
             , args = [IntConst (_, n), _]
             , ...
             }) = 0 <= n andalso n <= 0xffffffff
      | isDiscardable (PrimOp {primOp = F.PrimCall p, args, ...}) =
          Primitives.isDiscardableWithArgs (p, List.map getPrimEffect args)
      | isDiscardable (PrimOp {primOp = F.JsCallOp, args = e :: _, ...}) =
          isDiscardablePrimEffect e
      | isDiscardable (PrimOp {primOp = F.JsCallOp, args = [], ...}) =
          false (* should not occur *)
      | isDiscardable (PrimOp {primOp = F.JsMethodOp, args = e :: _, ...}) =
          isDiscardablePrimEffect e
      | isDiscardable (PrimOp {primOp = F.JsMethodOp, args = [], ...}) =
          false (* should not occur *)
      | isDiscardable (PrimOp {primOp = F.JsNewOp, args = e :: _, ...}) =
          isDiscardablePrimEffect e
      | isDiscardable (PrimOp {primOp = F.JsNewOp, args = [], ...}) =
          false (* should not occur *)
      | isDiscardable (PrimOp {primOp = F.LuaCallOp, args = e :: _, ...}) =
          isDiscardablePrimEffect e
      | isDiscardable (PrimOp {primOp = F.LuaCallOp, args = [], ...}) =
          false (* should not occur *)
      | isDiscardable (PrimOp {primOp = F.LuaCall1Op, args = e :: _, ...}) =
          isDiscardablePrimEffect e
      | isDiscardable (PrimOp {primOp = F.LuaCall1Op, args = [], ...}) =
          false (* should not occur *)
      | isDiscardable (PrimOp {primOp = F.LuaCallNOp _, args = e :: _, ...}) =
          isDiscardablePrimEffect e
      | isDiscardable (PrimOp {primOp = F.LuaCallNOp _, args = [], ...}) =
          false (* should not occur *)
      | isDiscardable (PrimOp {primOp = F.LuaMethodOp _, args = e :: _, ...}) =
          isDiscardablePrimEffect e
      | isDiscardable (PrimOp {primOp = F.LuaMethodOp _, args = [], ...}) =
          false (* should not occur *)
      | isDiscardable (PrimOp {primOp = F.LuaMethod1Op _, args = e :: _, ...}) =
          isDiscardablePrimEffect e
      | isDiscardable (PrimOp {primOp = F.LuaMethod1Op _, args = [], ...}) =
          false (* should not occur *)
      | isDiscardable (PrimOp {primOp = F.LuaMethodNOp _, args = e :: _, ...}) =
          isDiscardablePrimEffect e
      | isDiscardable (PrimOp {primOp = F.LuaMethodNOp _, args = [], ...}) =
          false (* should not occur *)
      | isDiscardable (Record _) = true
      | isDiscardable (ExnTag _) = true
      | isDiscardable (Projection _) = true
      | isDiscardable (Abs _) = true
    (* fun mayRaise (PrimOp { primOp, ... }) = (case primOp of
                                                 F.IntConstOp _ => false
                                               | F.WordConstOp _ => false
                                               | F.RealConstOp _ => false
                                               | F.Char8ConstOp _ => false
                                               | F.Char16ConstOp _ => false
                                               | F.String8ConstOp _ => false
                                               | F.String16ConstOp _ => false
                                               | F.RaiseOp _ => true
                                               | F.ListOp => false
                                               | F.VectorOp => false
                                               | F.DataTagAsStringOp _ => false
                                               | F.DataTagAsString16Op _ => false
                                               | F.DataPayloadOp _ => false
                                               | F.ExnPayloadOp => false
                                               | F.ConstructValOp _ => false
                                               | F.ConstructValWithPayloadOp _ => false
                                               | F.ConstructExnOp => false
                                               | F.ConstructExnWithPayloadOp => false
                                               | F.PrimCall p => Primitives.mayRaise p
                                               | F.JsCallOp => true
                                               | F.JsMethodOp => true
                                               | F.JsNewOp => true
                                               | F.LuaCallOp => true
                                               | F.LuaCall1Op => true
                                               | F.LuaMethodOp _ => true
                                               | F.LuaMethod1Op _ => true
                                            )
      | mayRaise (Record _) = false
      | mayRaise (ExnTag _) = false
      | mayRaise (Projection _) = false
      | mayRaise (Abs _) = false *)

    fun containsAppDec (ValDec _) = false
      | containsAppDec (RecDec _) = false
      | containsAppDec (UnpackDec _) = false
      | containsAppDec (ContDec {name = _, params = _, body, attr = _}) =
          containsApp body
      | containsAppDec (RecContDec defs) =
          List.exists (fn (_, _, body) => containsApp body) defs
      | containsAppDec (DatatypeDec _) = false
      | containsAppDec (ESImportDec _) = false
    and containsApp (Let {decs, cont}) =
          containsApp cont orelse List.exists containsAppDec decs
      | containsApp (App _) = true
      | containsApp (AppCont _) = false
      | containsApp (If {cond = _, thenCont, elseCont}) =
          containsApp thenCont orelse containsApp elseCont
      | containsApp (Handle {body, handler = (_, h), ...}) =
          containsApp body orelse containsApp h
      | containsApp (Raise _) = false
      | containsApp Unreachable = false

    fun freeVarsInValue bound (v, acc) =
      case extractVarFromValue v of
        SOME var =>
          if TypedSyntax.VIdSet.member (bound, var) then acc
          else TypedSyntax.VIdSet.add (acc, var)
      | NONE => acc
    fun VIdSet_addOpt ((SOME x, _), set) = TypedSyntax.VIdSet.add (set, x)
      | VIdSet_addOpt ((NONE, _), set) = set
    fun freeVarsInSimpleExp (bound, PrimOp {primOp = _, tyargs = _, args}, acc) =
          List.foldl (freeVarsInValue bound) acc args
      | freeVarsInSimpleExp (bound, Record fields, acc) =
          Syntax.LabelMap.foldl (freeVarsInValue bound) acc fields
      | freeVarsInSimpleExp (_, ExnTag {name = _, payloadTy = _}, acc) = acc
      | freeVarsInSimpleExp
          (bound, Projection {label = _, record, fieldTypes = _}, acc) =
          freeVarsInValue bound (record, acc)
      | freeVarsInSimpleExp
          ( bound
          , Abs
              { contParam = _
              , tyParams = _
              , params
              , body
              , resultTy = _
              , attr = _
              }
          , acc
          ) =
          let
            val bound =
              List.foldl
                (fn ((vid, _), bound) => TypedSyntax.VIdSet.add (bound, vid))
                bound params
          in
            freeVarsInExp (bound, body, acc)
          end
    and freeVarsInDec (ValDec {exp, results}, (bound, acc)) =
          let
            val acc = freeVarsInSimpleExp (bound, exp, acc)
            val bound' = List.foldl VIdSet_addOpt bound results
          in
            (bound', acc)
          end
      | freeVarsInDec (RecDec defs, (bound, acc)) =
          let
            val bound =
              List.foldl
                (fn ({name, ...}, bound) => TypedSyntax.VIdSet.add (bound, name))
                bound defs
            val acc =
              List.foldl
                (fn ({params, body, ...}, acc) =>
                   let
                     val bound =
                       List.foldl
                         (fn ((vid, _), bound) =>
                            TypedSyntax.VIdSet.add (bound, vid)) bound params
                   in
                     freeVarsInExp (bound, body, acc)
                   end) acc defs
          in
            (bound, acc)
          end
      | freeVarsInDec (UnpackDec {vid, package, ...}, (bound, acc)) =
          ( TypedSyntax.VIdSet.add (bound, vid)
          , freeVarsInValue bound (package, acc)
          )
      | freeVarsInDec (ContDec {name = _, params, body, attr = _}, (bound, acc)) =
          let val bound = List.foldl VIdSet_addOpt bound params
          in (bound, freeVarsInExp (bound, body, acc))
          end
      | freeVarsInDec (RecContDec defs, (bound, acc)) =
          let
            val acc =
              List.foldl
                (fn ((_, params, body), acc) =>
                   let val bound = List.foldl VIdSet_addOpt bound params
                   in freeVarsInExp (bound, body, acc)
                   end) acc defs
          in
            (bound, acc)
          end
      | freeVarsInDec (DatatypeDec _, (bound, acc)) = (bound, acc)
      | freeVarsInDec
          (ESImportDec {pure = _, specs, moduleName = _}, (bound, acc)) =
          ( List.foldl
              (fn ((_, v, _), bound) => TypedSyntax.VIdSet.add (bound, v)) bound
              specs
          , acc
          )
    and freeVarsInExp (bound, Let {decs, cont}, acc) =
          let val (bound, acc) = List.foldl freeVarsInDec (bound, acc) decs
          in freeVarsInExp (bound, cont, acc)
          end
      | freeVarsInExp
          (bound, App {applied, cont = _, tyArgs = _, args, attr = _}, acc) =
          List.foldl (freeVarsInValue bound)
            (freeVarsInValue bound (applied, acc)) args
      | freeVarsInExp (bound, AppCont {applied = _, args}, acc) =
          List.foldl (freeVarsInValue bound) acc args
      | freeVarsInExp (bound, If {cond, thenCont, elseCont}, acc) =
          freeVarsInExp (bound, elseCont, freeVarsInExp
            (bound, thenCont, freeVarsInValue bound (cond, acc)))
      | freeVarsInExp
          ( bound
          , Handle
              { body
              , handler = (e, h)
              , successfulExitIn = _
              , successfulExitOut = _
              , resultTy = _
              }
          , acc
          ) =
          freeVarsInExp (bound, body, freeVarsInExp
            (TypedSyntax.VIdSet.add (bound, e), h, acc))
      | freeVarsInExp (bound, Raise (_, x), acc) =
          freeVarsInValue bound (x, acc)
      | freeVarsInExp (_, Unreachable, acc) = acc

    fun recurseStat f =
      let
        fun goSimpleExp (e as PrimOp _) = e
          | goSimpleExp (e as Record _) = e
          | goSimpleExp (e as ExnTag _) = e
          | goSimpleExp (e as Projection _) = e
          | goSimpleExp
              (Abs {contParam, tyParams, params, body, resultTy, attr}) =
              Abs
                { contParam = contParam
                , tyParams = tyParams
                , params = params
                , body = goExp body
                , resultTy = resultTy
                , attr = attr
                }
        and goDec (ValDec {exp, results (*, tybinds *)}) =
              ValDec
                { exp = goSimpleExp exp
                , results = results (*, tybinds = tybinds *)
                }
          | goDec (RecDec defs) =
              RecDec
                (List.map
                   (fn {name, contParam, tyParams, params, body, resultTy, attr} =>
                      { name = name
                      , contParam = contParam
                      , tyParams = tyParams
                      , params = params
                      , body = goExp body
                      , resultTy = resultTy
                      , attr = attr
                      }) defs)
          | goDec (dec as UnpackDec _) = dec
          | goDec (ContDec {name, params, body, attr}) =
              ContDec
                {name = name, params = params, body = goExp body, attr = attr}
          | goDec (RecContDec defs) =
              RecContDec
                (List.map
                   (fn (name, params, body) => (name, params, goExp body)) defs)
          | goDec (dec as DatatypeDec _) = dec
          | goDec (dec as ESImportDec _) = dec
        and goExp e =
          f (case e of
               Let {decs, cont} =>
                 Let {decs = List.map goDec decs, cont = goExp cont}
             | App _ => e
             | AppCont _ => e
             | If {cond, thenCont, elseCont} =>
                 If
                   { cond = cond
                   , thenCont = goExp thenCont
                   , elseCont = goExp elseCont
                   }
             | Handle
                 { body
                 , handler = (k, h)
                 , successfulExitIn
                 , successfulExitOut
                 , resultTy
                 } =>
                 Handle
                   { body = goExp body
                   , handler = (k, goExp h)
                   , successfulExitIn = successfulExitIn
                   , successfulExitOut = successfulExitOut
                   , resultTy = resultTy
                   }
             | Raise _ => e
             | Unreachable => e)
      in
        goExp
      end
    fun valueToString (Var v) =
          "Var(" ^ TypedSyntax.print_VId v ^ ")"
      | valueToString Unit = "Unit"
      | valueToString Nil = "Nil"
      | valueToString (TypedNil _) = "TypedNil"
      | valueToString (BoolConst x) =
          "Bool(" ^ Bool.toString x ^ ")"
      | valueToString (IntConst (Primitives.INT, x)) =
          "Int(" ^ IntInf.toString x ^ ")"
      | valueToString (IntConst (Primitives.I32, x)) =
          "Int32(" ^ IntInf.toString x ^ ")"
      | valueToString (IntConst (Primitives.I54, x)) =
          "Int54(" ^ IntInf.toString x ^ ")"
      | valueToString (IntConst (Primitives.I64, x)) =
          "Int64(" ^ IntInf.toString x ^ ")"
      | valueToString (IntConst (Primitives.INT_INF, x)) =
          "IntInf(" ^ IntInf.toString x ^ ")"
      | valueToString (WordConst (Primitives.WORD, x)) =
          "Word(" ^ IntInf.toString x ^ ")"
      | valueToString (WordConst (Primitives.W32, x)) =
          "Word32(" ^ IntInf.toString x ^ ")"
      | valueToString (WordConst (Primitives.W64, x)) =
          "Word64(" ^ IntInf.toString x ^ ")"
      | valueToString (CharConst x) =
          "Char(" ^ Char.toString x ^ ")"
      | valueToString (Char7Const x) =
          "Char7(" ^ Char.toString x ^ ")"
      | valueToString (Char16Const x) =
          "Char16(" ^ Int.toString x ^ ")"
      | valueToString (Char32Const x) =
          "Char32(" ^ Int.toString x ^ ")"
      | valueToString (UCharConst x) =
          "UChar(" ^ Int.toString x ^ ")"
      | valueToString (StringConst x) =
          "String(\"" ^ String.toString x ^ "\")"
      | valueToString (String7Const x) =
          "String7(\"" ^ String.toString x ^ "\")"
      | valueToString (String16Const x) =
          "String16(["
          ^
          String.concatWith ","
            (Vector.foldr (fn (c, acc) => Int.toString c :: acc) [] x) ^ "])"
      | valueToString (String32Const x) =
          "String32(["
          ^
          String.concatWith ","
            (Vector.foldr (fn (c, acc) => Int.toString c :: acc) [] x) ^ "])"
      | valueToString (UStringConst x) =
          "UString(["
          ^
          String.concatWith ","
            (Vector.foldr (fn (c, acc) => Int.toString c :: acc) [] x) ^ "])"
      | valueToString (PrimEffect Primitives.PURE) = "PrimEffect(pure)"
      | valueToString (PrimEffect Primitives.DISCARDABLE) =
          "PrimEffect(discardable)"
      | valueToString (PrimEffect Primitives.IMPURE) = "PrimEffect(impure)"
      | valueToString (Cast {value, ...}) =
          "Cast(" ^ valueToString value ^ ")"
      | valueToString (Pack {value, ...}) =
          "Pack(" ^ valueToString value ^ ")"
    fun simpleExpToString (PrimOp {primOp, ...}) =
          (case primOp of
             F.PrimCall p => "PrimOp(" ^ Primitives.toString p ^ ")"
           | F.IntConstOp _ => "PrimOp(IntConstOp)"
           | F.WordConstOp _ => "PrimOp(WordConstOp)"
           | F.RealConstOp _ => "PrimOp(RealConstOp)"
           | F.Char7ConstOp _ => "PrimOp(Char7ConstOp)"
           | F.Char8ConstOp _ => "PrimOp(Char8ConstOp)"
           | F.Char16ConstOp _ => "PrimOp(Char16ConstOp)"
           | F.Char32ConstOp _ => "PrimOp(Char32ConstOp)"
           | F.UCharConstOp _ => "PrimOp(UCharConstOp)"
           | F.String7ConstOp _ => "PrimOp(String7ConstOp)"
           | F.String8ConstOp _ => "PrimOp(String8ConstOp)"
           | F.String16ConstOp _ => "PrimOp(String16ConstOp)"
           | F.String32ConstOp _ => "PrimOp(String32ConstOp)"
           | F.UStringConstOp _ => "PrimOp(UStringConstOp)"
           | F.RaiseOp _ => "PrimOp(RaiseOp)"
           | F.ListOp => "PrimOp(ListOp)"
           | F.VectorOp => "PrimOp(VectorOp)"
           | F.DataTagAsStringOp _ => "PrimOp(DataTagAsStringOp)"
           | F.DataTagAsString16Op _ => "PrimOp(DataTagAsString16Op)"
           | F.DataPayloadOp _ => "PrimOp(DataPayloadOp)"
           | F.ExnPayloadOp => "PrimOp(ExnPayloadOp)"
           | F.ConstructValOp _ => "PrimOp(ConstructValOp)"
           | F.ConstructValWithPayloadOp _ =>
               "PrimOp(ConstructValWithPayloadOp)"
           | F.ConstructExnOp => "PrimOp(ConstructExnOp)"
           | F.ConstructExnWithPayloadOp => "PrimOp(ConstructExnWithPayloadOp)"
           | F.JsCallOp => "PrimOp(JsCallOp)"
           | F.JsMethodOp => "PrimOp(JsMethodOp)"
           | F.JsNewOp => "PrimOp(JsNewOp)"
           | F.LuaCallOp => "PrimOp(LuaCallOp)"
           | F.LuaCall1Op => "PrimOp(LuaCall1Op)"
           | F.LuaCallNOp _ => "PrimOp(LuaCallNOp)"
           | F.LuaMethodOp _ => "PrimOp(LuaMethodOp)"
           | F.LuaMethod1Op _ => "PrimOp(LuaMethod1Op)"
           | F.LuaMethodNOp _ => "PrimOp(LuaMethodNOp)")
      | simpleExpToString (Record _) = "Record"
      | simpleExpToString (ExnTag _) = "ExnTag"
      | simpleExpToString (Projection _) = "Projection"
      | simpleExpToString (Abs _) = "Abs"
  end
end

structure CpsTransform :>
sig
  type Context =
    {targetInfo: TargetInfo.target_info, nextVId: int ref, exportAsRecord: bool}
  val fixIntWordTy: TargetInfo.target_info -> FSyntax.Ty -> FSyntax.Ty
  val initialEnv: (CSyntax.Value option * FSyntax.Ty) TypedSyntax.VIdMap.map
  val prependRevDecs: CSyntax.Dec list * CSyntax.Stat -> CSyntax.Stat
  val prependRevDecsTy: CSyntax.Dec list * (FSyntax.Ty * CSyntax.Stat)
                        -> FSyntax.Ty * CSyntax.Stat
  val transformBlock:
    Context * (CSyntax.Value option * FSyntax.Ty) TypedSyntax.VIdMap.map
    -> FSyntax.Exp
    -> CSyntax.CVar
    -> FSyntax.Ty * CSyntax.Stat
end =
struct
  local
    structure F = FSyntax
    structure C = CSyntax
    val foldlCont = ListUtil.foldlCont
  in

    type Context =
      { targetInfo: TargetInfo.target_info
      , nextVId: int ref
      , exportAsRecord: bool
      }

    fun fixIntWordTy
          ({defaultInt = Primitives.INT, defaultWord = Primitives.WORD, ...}:
             TargetInfo.target_info) = (fn ty => ty)
      | fixIntWordTy {defaultInt, defaultWord, ...} =
          let
            val intTy =
              case defaultInt of
                Primitives.INT => F.Types.int
              | Primitives.I32 => F.Types.int32
              | Primitives.I54 => F.Types.int54
              | Primitives.I64 => F.Types.int64
              | Primitives.INT_INF => F.Types.intInf
            val wordTy =
              case defaultWord of
                Primitives.WORD => F.Types.word
              | Primitives.W32 => F.Types.word32
              | Primitives.W64 => F.Types.word64
            fun goTy (ty as F.TyVar tv) =
                  if TypedSyntax.eqTyVar (tv, PrimTypes.Names.int) then
                    intTy
                  else if TypedSyntax.eqTyVar (tv, PrimTypes.Names.word) then
                    wordTy
                  else
                    ty
              | goTy (F.RecordType fields) =
                  F.RecordType (Syntax.LabelMap.map goTy fields)
              | goTy (F.AppType {applied, arg}) =
                  F.AppType {applied = goTy applied, arg = goTy arg}
              | goTy (F.MultiFnType (params, result)) =
                  F.MultiFnType (List.map goTy params, goTy result)
              | goTy (F.ForallType (tv, kind, ty)) =
                  F.ForallType (tv, kind, goTy ty)
              | goTy (F.ExistsType (tv, kind, ty)) =
                  F.ExistsType (tv, kind, goTy ty)
              | goTy (F.TypeFn (tv, kind, ty)) =
                  F.TypeFn (tv, kind, goTy ty)
              | goTy (ty as F.AnyType _) = ty
              | goTy (ty as F.DelayedSubst _) =
                  goTy (F.forceTy ty)
          in
            goTy
          end

    fun genContSym (ctx: Context) : CSyntax.CVar =
      let
        val n = !(#nextVId ctx)
        val _ = #nextVId ctx := n + 1
      in
        CSyntax.CVar.fromInt n
      end

    fun genSym (ctx: Context) =
      let
        val n = !(#nextVId ctx)
        val _ = #nextVId ctx := n + 1
      in
        TypedSyntax.MkVId (Syntax.SourceName.absent, n)
      end

    fun genSymWithHint (ctx: Context, NONE) = genSym ctx
      | genSymWithHint (ctx, SOME name) =
          let
            val n = !(#nextVId ctx)
            val _ = #nextVId ctx := n + 1
          in
            TypedSyntax.MkVId (name, n)
          end

    fun renewSym (ctx: Context, TypedSyntax.MkVId (name, _)) =
      let
        val n = !(#nextVId ctx)
        val _ = #nextVId ctx := n + 1
      in
        TypedSyntax.MkVId (name, n)
      end

    type Ty = FSyntax.Ty

    datatype value_transform =
      TCast of {from: Ty, to: Ty}
    | TPack of {payloadTy: Ty, packageTy: Ty}

    (* applyValueTransforms ([t1,...,tN], (v, ty)) = applyValueTransform ([tN], ...applyValueTransform ([t1], (v, ty))...) *)
    fun applyValueTransform (TCast {from, to}, (v, _)) =
          (C.Cast {value = v, from = from, to = to}, to)
      | applyValueTransform (TPack {payloadTy, packageTy}, (v, _)) =
          ( C.Pack {value = v, payloadTy = payloadTy, packageTy = packageTy}
          , packageTy
          )
    fun applyValueTransforms (ts, v_ty) =
      List.foldl applyValueTransform v_ty ts

    fun applyValueTransformToVal (TCast {from, to}, v) =
          C.Cast {value = v, from = from, to = to}
      | applyValueTransformToVal (TPack {payloadTy, packageTy}, v) =
          C.Pack {value = v, payloadTy = payloadTy, packageTy = packageTy}
    fun applyValueTransformsToVal (ts, v) =
      List.foldl applyValueTransformToVal v ts

    fun applyValueTransformToTy (TCast {from = _, to}, _) = to
      | applyValueTransformToTy (TPack {payloadTy = _, packageTy}, _) =
          packageTy
    fun applyValueTransformsToTy (ts, ty) =
      List.foldl applyValueTransformToTy ty ts

    (* 'a -> 'b ~~> (cont : 'b -> 'ans, param : 'a) -> 'ans *)
    (* continuation of 'a : (value : 'a) -> 'ans *)

    datatype cont =
      REIFIED of C.CVar
    | META of
        C.Var option (* * Ty *) * (C.Dec list * C.Value * Ty -> Ty * C.Stat)
    fun prependRevDecs ([], cont) = cont
      | prependRevDecs (revDecs, C.Let {decs, cont}) =
          C.Let {decs = List.revAppend (revDecs, decs), cont = cont}
      | prependRevDecs (revDecs, cont) =
          C.Let {decs = List.rev revDecs, cont = cont}
    fun prependRevDecsTy (revDecs, (ty: Ty, cont)) =
      (ty, prependRevDecs (revDecs, cont))
    (*: val reify : Context * C.Dec list * cont -> value_transform list -> (C.CVar -> Ty * C.Stat) -> Ty * C.Stat *)
    fun reify (ctx, revDecs, REIFIED k) valueTransforms f =
          if List.null valueTransforms then
            prependRevDecsTy (revDecs, f k)
          else
            let
              val kk = genContSym ctx
              val v = genSym ctx
              val (ty, stat) = f kk
              val ty' = applyValueTransformsToTy (valueTransforms, ty)
              val dec = C.ContDec
                { name = kk
                , params = [(SOME v, ty)]
                , body = C.AppCont
                    { applied = k
                    , args =
                        [applyValueTransformsToVal (valueTransforms, C.Var v)]
                    }
                , attr = {alwaysInline = false}
                }
            in
              (ty', prependRevDecs (dec :: revDecs, stat))
            end
      | reify (ctx, revDecs, META (hint, m)) valueTransforms f =
          let
            val k = genContSym ctx
            val x =
              case hint of
                NONE => genSym ctx
              | SOME x => x
            val (ty, stat) = f k
            val ty' = applyValueTransformsToTy (valueTransforms, ty)
            val v = applyValueTransformsToVal (valueTransforms, C.Var x)
            val (ty'', body') = m ([], v, ty)
          in
            ( ty''
            , prependRevDecs
                ( C.ContDec
                    { name = k
                    , params = [(SOME x, ty)]
                    , body = body'
                    , attr = {alwaysInline = false}
                    } :: revDecs
                , stat
                )
            )
          end
    (*: val apply : C.Dec list -> cont -> value_transform list -> C.Value * Ty -> Ty * C.Stat *)
    fun apply revDecs (REIFIED k) valueTransforms (arg, ty) =
          ( applyValueTransformsToTy (valueTransforms, ty)
          , prependRevDecs (revDecs, C.AppCont
              { applied = k
              , args = [applyValueTransformsToVal (valueTransforms, arg)]
              })
          )
      | apply revDecs (META (_, m)) valueTransforms (arg, ty) =
          let val ty' = applyValueTransformsToTy (valueTransforms, ty)
          in m (revDecs, applyValueTransformsToVal (valueTransforms, arg), ty')
          end
    fun getResultHint (META (hint, _)) = hint
      | getResultHint (REIFIED _) = NONE
    fun getResultHintString (META (hint, _)) =
          Option.map TypedSyntax.getVIdName hint
      | getResultHintString (REIFIED _) = NONE
    val initialEnv: (C.Value option * Ty) TypedSyntax.VIdMap.map =
      let
        val tv = TypedSyntax.MkTyVar ("a", 0)
        val initialEnv0 =
          TypedSyntax.VIdMap.map (fn ty => (NONE, ty))
            (#valMap ToFSyntax.initialEnv)
      in
        List.foldl TypedSyntax.VIdMap.insert' initialEnv0
          [ ( InitialEnv.VId_false
            , (SOME (C.BoolConst false), FSyntax.Types.bool)
            )
          , (InitialEnv.VId_true, (SOME (C.BoolConst true), FSyntax.Types.bool))
          , ( InitialEnv.VId_nil
            , ( SOME C.Nil
              , F.ForallType (tv, F.TypeKind, FSyntax.Types.list (F.TyVar tv))
              )
            )
          , ( InitialEnv.VId_PrimEffect_pure
            , (SOME (C.PrimEffect Primitives.PURE), FSyntax.Types.prim_effect)
            )
          , ( InitialEnv.VId_PrimEffect_discardable
            , ( SOME (C.PrimEffect Primitives.DISCARDABLE)
              , FSyntax.Types.prim_effect
              )
            )
          , ( InitialEnv.VId_PrimEffect_impure
            , (SOME (C.PrimEffect Primitives.IMPURE), FSyntax.Types.prim_effect)
            )
          ]
      end
    (*:
    val transform : Context * (C.Value option * Ty) TypedSyntax.VIdMap.map -> F.Exp -> { revDecs : C.Dec list, resultHint : C.Var option (*, resultTy : Ty *) } -> (C.Dec list * C.Value * Ty -> Ty * C.Stat) -> Ty * C.Stat
    and transformBlock : Context * (C.Value option * Ty) TypedSyntax.VIdMap.map -> F.Exp -> C.CVar -> Ty * C.Stat
    and transformX : Context * (C.Value option * Ty) TypedSyntax.VIdMap.map -> F.Exp -> value_transform list -> C.Dec list * cont -> Ty * C.Stat
     *)
    fun transform (ctx, env) exp {revDecs, resultHint (*, resultTy *)} k =
      transformX (ctx, env) exp [] (revDecs, META (resultHint, k))
    and transformBlock (ctx, env) exp k =
      transformX (ctx, env) exp [] ([], REIFIED k)
    and transformX (ctx: Context, env) (exp: F.Exp)
      (valueTransforms: value_transform list) (revDecs: C.Dec list, k: cont) :
      Ty * C.Stat =
      case exp of
        F.PrimExp (F.RaiseOp span, [ty], [arg]) =>
          transform (ctx, env) arg {revDecs = revDecs, resultHint = NONE}
            (fn (revDecs, arg, _) =>
               ( applyValueTransformsToTy (valueTransforms, ty)
               , prependRevDecs (revDecs, C.Raise (span, arg))
               ))
      | F.PrimExp (primOp, tyargs, args) =>
          let
            val primOp =
              case primOp of
                F.PrimCall p =>
                  F.PrimCall
                    (Primitives.fixIntWord
                       { int = #defaultInt (#targetInfo ctx)
                       , word = #defaultWord (#targetInfo ctx)
                       } p)
              | _ => primOp
            val (_ (* argTypes *), resultTypes) =
              case (primOp, tyargs) of
                (F.PrimCall p, tyargs) =>
                  let
                    val {vars, args, results} = CheckF.TypeOfPrimitives.typeOf p
                    val m =
                      ListPair.foldl
                        (fn ((v, _), t, acc) =>
                           TypedSyntax.TyVarMap.insert (acc, v, t))
                        TypedSyntax.TyVarMap.empty (vars, tyargs)
                    val goTy = #doTy (F.lazySubstTy m)
                    val results' = List.map goTy results
                  in
                    ( Vector.foldr (fn (ty, acc) => goTy ty :: acc) [] args
                    , List.map goTy results
                    )
                  end
              | (F.IntConstOp _, [ty]) => ([], [ty])
              | (F.WordConstOp _, [ty]) => ([], [ty])
              | (F.RealConstOp _, [ty]) => ([], [ty])
              | (F.Char7ConstOp _, [ty]) => ([], [ty])
              | (F.Char8ConstOp _, [ty]) => ([], [ty])
              | (F.Char16ConstOp _, [ty]) => ([], [ty])
              | (F.Char32ConstOp _, [ty]) => ([], [ty])
              | (F.UCharConstOp _, [ty]) => ([], [ty])
              | (F.String7ConstOp _, [ty]) => ([], [ty])
              | (F.String8ConstOp _, [ty]) => ([], [ty])
              | (F.String16ConstOp _, [ty]) => ([], [ty])
              | (F.String32ConstOp _, [ty]) => ([], [ty])
              | (F.UStringConstOp _, [ty]) => ([], [ty])
              | (F.RaiseOp _, [ty]) => ([FSyntax.Types.exn], [ty])
              | (F.ListOp, [elemTy]) =>
                  (List.map (fn _ => elemTy) args, [FSyntax.Types.list elemTy])
              | (F.VectorOp, [elemTy]) =>
                  ( List.map (fn _ => elemTy) args
                  , [FSyntax.Types.vector elemTy]
                  )
              | (F.DataTagAsStringOp _, [dataTy]) =>
                  ([dataTy], [FSyntax.Types.string])
              | (F.DataTagAsString16Op _, [dataTy]) =>
                  ([dataTy], [FSyntax.Types.string16])
              | (F.DataPayloadOp _, [dataTy, payloadTy]) =>
                  ([dataTy], [payloadTy])
              | (F.ExnPayloadOp, [payloadTy]) =>
                  ([FSyntax.Types.exn], [payloadTy])
              | (F.ConstructValOp _, [dataTy]) => ([], [dataTy])
              | (F.ConstructValWithPayloadOp _, [dataTy, payloadTy]) =>
                  ([payloadTy], [dataTy])
              | (F.ConstructExnOp, []) =>
                  ([FSyntax.Types.exntag], [FSyntax.Types.exn])
              | (F.ConstructExnWithPayloadOp, [payloadTy]) =>
                  ([FSyntax.Types.exntag, payloadTy], [FSyntax.Types.exn])
              | (F.JsCallOp, []) =>
                  ( List.map (fn _ => FSyntax.Types.js_value) args
                  , [FSyntax.Types.js_value]
                  )
              | (F.JsMethodOp, []) =>
                  ( List.map (fn _ => FSyntax.Types.js_value) args
                  , [FSyntax.Types.js_value]
                  )
              | (F.JsNewOp, []) =>
                  ( List.map (fn _ => FSyntax.Types.js_value) args
                  , [FSyntax.Types.js_value]
                  )
              | (F.LuaCallOp, []) =>
                  ( List.map (fn _ => FSyntax.Types.lua_value) args
                  , [FSyntax.Types.vector FSyntax.Types.lua_value]
                  )
              | (F.LuaCall1Op, []) =>
                  ( List.map (fn _ => FSyntax.Types.lua_value) args
                  , [FSyntax.Types.lua_value]
                  )
              | (F.LuaCallNOp n, []) =>
                  ( List.map (fn _ => FSyntax.Types.lua_value) args
                  , List.tabulate (n, fn _ => FSyntax.Types.lua_value)
                  )
              | (F.LuaMethodOp _, []) =>
                  ( List.map (fn _ => FSyntax.Types.lua_value) args
                  , [FSyntax.Types.vector FSyntax.Types.lua_value]
                  )
              | (F.LuaMethod1Op _, []) =>
                  ( List.map (fn _ => FSyntax.Types.lua_value) args
                  , [FSyntax.Types.lua_value]
                  )
              | (F.LuaMethodNOp (_, n), []) =>
                  ( List.map (fn _ => FSyntax.Types.lua_value) args
                  , List.tabulate (n, fn _ => FSyntax.Types.lua_value)
                  )
              | _ =>
                  raise CSyntax.InvalidCode
                    ("unknown primop or invalid type arguments to primop "
                     ^ FSyntax.PrettyPrint.print_PrimOp primOp)
          in
            case (primOp, args, resultTypes) of
              (F.PrimCall Primitives.Unsafe_cast, [arg], _) =>
                (case tyargs of
                   [from, to] =>
                     transformX (ctx, env) arg
                       (TCast {from = from, to = to} :: valueTransforms)
                       (revDecs, k)
                 | _ => raise Fail "invalid use of Unsafe.cast")
            | (F.PrimCall Primitives.unreachable, _, [ty]) =>
                (ty, C.Unreachable)
            | _ =>
                foldlCont
                  (fn (e, (revDecs, acc), cont) =>
                     transform (ctx, env) e
                       {revDecs = revDecs, resultHint = NONE}
                       (fn (revDecs, v, _) => cont (revDecs, v :: acc)))
                  (revDecs, []) args
                  (fn (revDecs, revArgs) =>
                     case primOp of
                       F.IntConstOp x =>
                         (case tyargs of
                            [ty as F.TyVar tv] =>
                              let
                                val v =
                                  if
                                    TypedSyntax.eqTyVar
                                      (tv, PrimTypes.Names.int)
                                  then
                                    C.IntConst
                                      (#defaultInt (#targetInfo ctx), x)
                                  else if
                                    TypedSyntax.eqTyVar
                                      (tv, PrimTypes.Names.int32)
                                  then
                                    C.IntConst (Primitives.I32, x)
                                  else if
                                    TypedSyntax.eqTyVar
                                      (tv, PrimTypes.Names.int54)
                                  then
                                    C.IntConst (Primitives.I54, x)
                                  else if
                                    TypedSyntax.eqTyVar
                                      (tv, PrimTypes.Names.int64)
                                  then
                                    C.IntConst (Primitives.I64, x)
                                  else if
                                    TypedSyntax.eqTyVar
                                      (tv, PrimTypes.Names.intInf)
                                  then
                                    C.IntConst (Primitives.INT_INF, x)
                                  else
                                    raise Fail "IntConstOp: invalid type"
                              in
                                apply revDecs k valueTransforms (v, ty)
                              end
                          | _ => raise Fail "IntConstOp: invalid type")
                     | F.WordConstOp x =>
                         (case tyargs of
                            [ty as F.TyVar tv] =>
                              let
                                val v =
                                  if
                                    TypedSyntax.eqTyVar
                                      (tv, PrimTypes.Names.word)
                                  then
                                    C.WordConst
                                      (#defaultWord (#targetInfo ctx), x)
                                  else if
                                    TypedSyntax.eqTyVar
                                      (tv, PrimTypes.Names.word32)
                                  then
                                    C.WordConst (Primitives.W32, x)
                                  else if
                                    TypedSyntax.eqTyVar
                                      (tv, PrimTypes.Names.word64)
                                  then
                                    C.WordConst (Primitives.W64, x)
                                  else
                                    raise Fail "WordConstOp: invalid type"
                              in
                                apply revDecs k valueTransforms (v, ty)
                              end
                          | _ => raise Fail "WordConstOp: invalid type")
                     | F.Char7ConstOp x =>
                         apply revDecs k valueTransforms
                           ( C.Char7Const x
                           , FSyntax.Types.char7
                           ) (* assume the type is correct *)
                     | F.Char8ConstOp x =>
                         (case tyargs of
                            [ty as F.TyVar tv] =>
                              let
                                val t =
                                  if
                                    TypedSyntax.eqTyVar
                                      (tv, PrimTypes.Names.char)
                                  then
                                    []
                                  else
                                    [TCast {from = FSyntax.Types.char, to = ty}]
                              in
                                apply revDecs k (t @ valueTransforms)
                                  ( C.CharConst x
                                  , FSyntax.Types.char
                                  ) (* assume the type is correct *)
                              end
                          | _ => raise Fail "Char8ConstOp: invalid type")
                     | F.Char16ConstOp x =>
                         apply revDecs k valueTransforms
                           ( C.Char16Const x
                           , FSyntax.Types.char16
                           ) (* assume the type is correct *)
                     | F.Char32ConstOp x =>
                         apply revDecs k valueTransforms
                           ( C.Char32Const x
                           , FSyntax.Types.char32
                           ) (* assume the type is correct *)
                     | F.UCharConstOp x =>
                         apply revDecs k valueTransforms
                           ( C.UCharConst x
                           , FSyntax.Types.uchar
                           ) (* assume the type is correct *)
                     | F.String7ConstOp x =>
                         apply revDecs k valueTransforms
                           ( C.String7Const x
                           , FSyntax.Types.string7
                           ) (* assume the type is correct *)
                     | F.String8ConstOp x =>
                         (case tyargs of
                            [ty as F.TyVar tv] =>
                              let
                                val t =
                                  if
                                    TypedSyntax.eqTyVar
                                      (tv, PrimTypes.Names.string)
                                  then
                                    []
                                  else
                                    [TCast
                                       {from = FSyntax.Types.string, to = ty}]
                              in
                                apply revDecs k (t @ valueTransforms)
                                  ( C.StringConst x
                                  , FSyntax.Types.string
                                  ) (* assume the type is correct *)
                              end
                          | _ => raise Fail "String8ConstOp: invalid type")
                     | F.String16ConstOp x =>
                         apply revDecs k valueTransforms
                           ( C.String16Const x
                           , FSyntax.Types.string16
                           ) (* assume the type is correct *)
                     | F.String32ConstOp x =>
                         apply revDecs k valueTransforms
                           ( C.String32Const x
                           , FSyntax.Types.string32
                           ) (* assume the type is correct *)
                     | F.UStringConstOp x =>
                         apply revDecs k valueTransforms
                           ( C.UStringConst x
                           , FSyntax.Types.ustring
                           ) (* assume the type is correct *)
                     | _ =>
                         let
                           val args = List.rev revArgs
                           val returnArity =
                             case primOp of
                               F.PrimCall p => Primitives.returnArity p
                             | F.LuaCallNOp n => n
                             | F.LuaMethodNOp (_, n) => n
                             | _ => 1
                           val exp =
                             C.PrimOp
                               {primOp = primOp, tyargs = tyargs, args = args}
                         in
                           case (returnArity, resultTypes) of
                             (0, []) =>
                               apply
                                 (C.ValDec {exp = exp, results = []} :: revDecs)
                                 k valueTransforms (C.Unit, FSyntax.Types.unit)
                           | (1, [resultTy]) =>
                               let
                                 val result =
                                   case getResultHint k of
                                     SOME r => r
                                   | NONE => genSym ctx
                               in
                                 apply
                                   (C.ValDec
                                      { exp = exp
                                      , results = [(SOME result, resultTy)]
                                      } :: revDecs) k valueTransforms
                                   (C.Var result, resultTy)
                               end
                           | _ =>
                               let
                                 val results =
                                   List.tabulate (returnArity, fn _ =>
                                     genSym ctx)
                                 val tupleVar =
                                   case getResultHint k of
                                     SOME r => r
                                   | NONE => genSym ctx
                                 val tupleTy = F.TupleType resultTypes
                                 val (_, tuple) =
                                   List.foldl
                                     (fn (v, (i, acc)) =>
                                        ( i + 1
                                        , Syntax.LabelMap.insert
                                            ( acc
                                            , Syntax.NumericLabel i
                                            , C.Var v
                                            )
                                        )) (1, Syntax.LabelMap.empty) results
                               in
                                 apply
                                   (C.ValDec
                                      { exp = C.Record tuple
                                      , results = [(SOME tupleVar, tupleTy)]
                                      }
                                    ::
                                    C.ValDec
                                      { exp = exp
                                      , results =
                                          ListPair.mapEq
                                            (fn (v, ty) => (SOME v, ty))
                                            (results, resultTypes)
                                      } :: revDecs) k valueTransforms
                                   (C.Var tupleVar, tupleTy)
                               end
                         end)
          end
      | F.VarExp vid =>
          (case TypedSyntax.VIdMap.find (env, vid) of
             SOME (SOME replacement, ty) =>
               apply revDecs k valueTransforms (replacement, ty)
           | SOME (NONE, ty) => apply revDecs k valueTransforms (C.Var vid, ty)
           | NONE =>
               raise Fail
                 ("CpsTransform: undefined variable: "
                  ^ TypedSyntax.print_VId vid))
      | F.RecordExp [] =>
          apply revDecs k valueTransforms (C.Unit, FSyntax.Types.unit)
      | F.RecordExp fields =>
          foldlCont
            (fn ((label, exp), (revDecs, acc), cont) =>
               transform (ctx, env) exp {revDecs = revDecs, resultHint = NONE}
                 (fn (revDecs, v, ty) => cont (revDecs, (label, v, ty) :: acc)))
            (revDecs, []) fields
            (fn (revDecs, revFields) =>
               let
                 val result =
                   case getResultHint k of
                     SOME r => r
                   | NONE => genSym ctx
                 val recordTy = F.RecordType
                   (List.foldl
                      (fn ((label, _, ty), m) =>
                         Syntax.LabelMap.insert (m, label, ty))
                      Syntax.LabelMap.empty revFields)
               in
                 apply
                   (C.ValDec
                      { exp = C.Record
                          (List.foldr
                             (fn ((label, v, _), m) =>
                                Syntax.LabelMap.insert (m, label, v))
                             Syntax.LabelMap.empty revFields)
                      , results = [(SOME result, recordTy)]
                      } :: revDecs) k valueTransforms (C.Var result, recordTy)
               end)
      | F.LetExp (decs, finalExp) =>
          let
            fun doDecs (env, [], revDecs) =
                  transformX (ctx, env) finalExp valueTransforms (revDecs, k)
              | doDecs (env, F.ValDec (vid, optTy, exp) :: decs, revDecs) =
                  transform (ctx, env) exp
                    {revDecs = revDecs, resultHint = SOME vid}
                    (fn (revDecs, v, ty) =>
                       let
                       (*
                       val () =
                         case optTy of
                           NONE => ()
                         | SOME expectedTy =>
                             CheckF.checkSame
                               ( TypedSyntax.TyVarMap.empty
                               , fn () =>
                                   "ValDec " ^ TypedSyntax.print_VId vid
                               , expectedTy
                               ) ty
                       *)
                       in
                         doDecs
                           ( TypedSyntax.VIdMap.insert (env, vid, (SOME v, ty))
                           , decs
                           , revDecs
                           )
                       end)
              | doDecs (env, F.RecValDec decs' :: decs, revDecs) =
                  let
                    val env =
                      List.foldl
                        (fn ((vid, ty, _), env) =>
                           TypedSyntax.VIdMap.insert (env, vid, (NONE, ty))) env
                        decs'
                    val dec = C.RecDec
                      (List.map
                         (fn (vid, _, exp) =>
                            let
                              val contParam = genContSym ctx
                            in
                              case exp of
                                F.MultiFnExp (_, params, body) =>
                                  let
                                    val env =
                                      List.foldl
                                        (fn ((vid, ty), env) =>
                                           TypedSyntax.VIdMap.insert
                                             (env, vid, (NONE, ty))) env params
                                    val (resultTy, body) =
                                      transformBlock (ctx, env) body contParam
                                  in
                                    { name = vid
                                    , contParam = contParam
                                    , tyParams = []
                                    , params = params
                                    , body = body
                                    , resultTy = resultTy
                                    , attr =
                                        {alwaysInline = false, typeOnly = false}
                                    }
                                  end
                              | F.TyAbsExp (tv, kind, body) =>
                                  let
                                    val (resultTy, body) =
                                      transformBlock (ctx, env) body contParam
                                  in
                                    { name = vid
                                    , contParam = contParam
                                    , tyParams = [(tv, kind)]
                                    , params = []
                                    , body = body
                                    , resultTy = resultTy
                                    , attr =
                                        {alwaysInline = false, typeOnly = true}
                                    }
                                  end
                              | _ => raise Fail "RecValDec"
                            end) decs')
                  in
                    doDecs (env, decs, dec :: revDecs)
                  end
              | doDecs
                  (env, F.UnpackDec (tv, kind, vid, ty, exp) :: decs, revDecs) =
                  transform (ctx, env) exp
                    {revDecs = revDecs, resultHint = SOME (renewSym (ctx, vid))}
                    (fn (revDecs, package, _ (* packageTy *)) =>
                       let
                         val dec = C.UnpackDec
                           { tyVar = tv
                           , kind = kind
                           , vid = vid
                           , unpackedTy = ty
                           , package = package
                           }
                       in
                         doDecs
                           ( TypedSyntax.VIdMap.insert (env, vid, (NONE, ty))
                           , decs
                           , dec :: revDecs
                           )
                       end)
              | doDecs (env, F.IgnoreDec exp :: decs, revDecs) =
                  transform (ctx, env) exp
                    {revDecs = revDecs, resultHint = NONE}
                    (fn (revDecs, _, _) => doDecs (env, decs, revDecs))
              | doDecs (env, F.DatatypeDec datbinds :: decs, revDecs) =
                  doDecs (env, decs, List.revAppend
                    ( List.map
                        (fn F.DatBind (params, tv, _) =>
                           C.DatatypeDec
                             (tv, F.arityToKind (List.length params))) datbinds
                    , revDecs
                    ))
              | doDecs
                  ( env
                  , F.ExceptionDec {name, tagName, payloadTy} :: decs
                  , revDecs
                  ) =
                  let
                    val dec = C.ValDec
                      { exp = C.ExnTag {name = name, payloadTy = payloadTy}
                      , results = [(SOME tagName, FSyntax.Types.exntag)]
                      }
                    val env =
                      TypedSyntax.VIdMap.insert
                        (env, tagName, (NONE, FSyntax.Types.exntag))
                  in
                    doDecs (env, decs, dec :: revDecs)
                  end
              | doDecs
                  ( env
                  , F.ESImportDec {pure, specs, moduleName} :: decs
                  , revDecs
                  ) =
                  let
                    val dec =
                      C.ESImportDec
                        {pure = pure, specs = specs, moduleName = moduleName}
                    val env =
                      List.foldl
                        (fn ((_, vid, ty), env) =>
                           TypedSyntax.VIdMap.insert (env, vid, (NONE, ty))) env
                        specs
                  in
                    doDecs (env, decs, dec :: revDecs)
                  end
          in
            doDecs (env, decs, revDecs)
          end
      | F.MultiAppExp (applied, args) =>
          transform (ctx, env) applied {revDecs = revDecs, resultHint = NONE}
            (fn (revDecs, f, tyF) =>
               let
                 val resultTy =
                   case F.weakNormalizeTy tyF of
                     F.MultiFnType (_, resultTy) => resultTy
                   | _ =>
                       raise Fail
                         ("MultiAppExp: expected MultiFnType, but got "
                          ^ Printer.build (FPrinter.doTy 0 tyF) ^ "\n"
                          ^ Printer.build (FPrinter.doExp 0 applied))
               in
                 foldlCont
                   (fn (arg, (revDecs, acc), cont) =>
                      transform (ctx, env) arg
                        {revDecs = revDecs, resultHint = NONE}
                        (fn (revDecs, v, _) => cont (revDecs, v :: acc)))
                   (revDecs, []) args
                   (fn (revDecs, revArgs) =>
                      reify (ctx, revDecs, k) valueTransforms (fn j =>
                        ( resultTy
                        , C.App
                            { applied = f
                            , cont = j
                            , tyArgs = []
                            , args = List.rev revArgs
                            , attr = {typeOnly = false}
                            }
                        )))
               end)
      | F.HandleExp {body, exnName, handler, resultTy} =>
          reify (ctx, revDecs, k) valueTransforms (fn j =>
            let
              val success = genContSym ctx
              val handlerEnv =
                TypedSyntax.VIdMap.insert
                  (env, exnName, (NONE, FSyntax.Types.exn))
            in
              ( resultTy
              , C.Handle
                  { body = #2 (transformBlock (ctx, env) body success)
                  , handler = (exnName, #2
                      (transformBlock (ctx, handlerEnv) handler j))
                  , successfulExitIn = success
                  , successfulExitOut = j
                  , resultTy = resultTy
                  }
              )
            end)
      | F.IfThenElseExp (e1, e2, e3) =>
          transform (ctx, env) e1 {revDecs = revDecs, resultHint = NONE}
            (fn (revDecs, e1, _) =>
               reify (ctx, revDecs, k) valueTransforms (fn j =>
                 let
                   val (resultTy, thenCont) = transformBlock (ctx, env) e2 j
                   val (_, elseCont) = transformBlock (ctx, env) e3 j
                 in
                   ( resultTy
                   , C.If {cond = e1, thenCont = thenCont, elseCont = elseCont}
                   )
                 end))
      | F.CaseExp _ => raise Fail "CaseExp: not supported here"
      | F.MultiFnExp (nameHint, params, body) =>
          let
            val f =
              case getResultHint k of
                SOME f => f
              | NONE => genSymWithHint (ctx, nameHint)
            val kk = genContSym ctx
            val innerEnv =
              List.foldl
                (fn ((p, ty), env) =>
                   TypedSyntax.VIdMap.insert (env, p, (NONE, ty))) env params
            val (resultTy, body) = transformBlock (ctx, innerEnv) body kk
            val fnTy = F.MultiFnType (List.map #2 params, resultTy)
            val dec = C.ValDec
              { exp = C.Abs
                  { contParam = kk
                  , tyParams = []
                  , params = params
                  , body = body
                  , resultTy = resultTy
                  , attr = {alwaysInline = false, typeOnly = false}
                  }
              , results = [(SOME f, fnTy)]
              }
          in
            apply (dec :: revDecs) k valueTransforms (C.Var f, fnTy)
          end
      | F.ProjectionExp {label, record, fieldTypes} =>
          transform (ctx, env) record {revDecs = revDecs, resultHint = NONE}
            (fn (revDecs, record, recordTy) =>
               let
                 val x =
                   case getResultHint k of
                     SOME x => x
                   | NONE => genSym ctx
                 val fieldTy =
                   case F.weakNormalizeTy recordTy of
                     F.RecordType fieldTypes' =>
                       (case Syntax.LabelMap.find (fieldTypes', label) of
                          SOME ty => ty
                        | NONE => raise Fail "ProjectionExp: field not found")
                   | _ =>
                       raise Fail
                         ("ProjectionExp: expected a record, but got "
                          ^ Printer.build (FPrinter.doTy 0 recordTy))
                 val dec = C.ValDec
                   { exp =
                       C.Projection
                         { label = label
                         , record = record
                         , fieldTypes = fieldTypes
                         }
                   , results = [(SOME x, fieldTy)]
                   }
               in
                 apply (dec :: revDecs) k valueTransforms (C.Var x, fieldTy)
               end)
      | F.TyAbsExp (tv, kind, body) =>
          let
            val f = genSymWithHint (ctx, getResultHintString k)
            val kk = genContSym ctx
            val (resultTy, body) = transformBlock (ctx, env) body kk
            val fnTy = F.ForallType (tv, kind, resultTy)
            val dec = C.ValDec
              { exp = C.Abs
                  { contParam = kk
                  , tyParams = [(tv, kind)]
                  , params = []
                  , body = body
                  , resultTy = resultTy
                  , attr = {alwaysInline = false, typeOnly = true}
                  }
              , results = [(SOME f, fnTy)]
              }
          in
            apply (dec :: revDecs) k valueTransforms (C.Var f, fnTy)
          end
      | F.TyAppExp (exp, ty) =>
          transform (ctx, env) exp {revDecs = revDecs, resultHint = NONE}
            (fn (revDecs, f, tyF) =>
               let
                 val resultTy =
                   case tyF of
                     F.ForallType (tv, _, resultTy) =>
                       F.substituteTy (tv, ty) resultTy
                   | _ =>
                       raise Fail
                         ("TyAppExp: expected ForallType, but got "
                          ^ Printer.build (FPrinter.doTy 0 tyF))
               in
                 reify (ctx, revDecs, k) valueTransforms (fn j =>
                   ( resultTy
                   , C.App
                       { applied = f
                       , cont = j
                       , tyArgs = [ty]
                       , args = []
                       , attr = {typeOnly = true}
                       }
                   ))
               end)
      | F.PackExp {payloadTy, exp, packageTy} =>
          transformX (ctx, env) exp
            (TPack {payloadTy = payloadTy, packageTy = packageTy}
             :: valueTransforms) (revDecs, k)
      | F.BogusExp _ => raise Message.Abort
      | F.ExitProgram =>
          (case k of
             REIFIED k =>
               let
                 val dummyTy = F.RecordType Syntax.LabelMap.empty
               in
                 ( dummyTy
                 , prependRevDecs (revDecs, C.AppCont {applied = k, args = []})
                 )
               end
           | META _ => raise Fail "unexpected META")
      | F.ExportValue exp =>
          (case k of
             REIFIED k =>
               transform (ctx, env) exp {revDecs = revDecs, resultHint = NONE}
                 (fn (revDecs, v, ty) =>
                    ( ty
                    , prependRevDecs
                        (revDecs, C.AppCont {applied = k, args = [v]})
                    ))
           | META _ => raise Fail "unexpected META")
      | F.ExportModule entities =>
          (case k of
             REIFIED k =>
               foldlCont
                 (fn ((name, exp), (revDecs, acc), cont) =>
                    transform (ctx, env) exp
                      {revDecs = revDecs, resultHint = NONE (* name? *)}
                      (fn (revDecs, v, ty) =>
                         cont (revDecs, (name, v, ty) :: acc))) (revDecs, [])
                 (Vector.foldr (op::) [] entities)
                 (fn (revDecs, items) =>
                    if #exportAsRecord ctx then
                      let
                        val result = genSym ctx (* "export"? *)
                        val recordTy = F.RecordType
                          (List.foldl
                             (fn ((name, _, ty), acc) =>
                                Syntax.LabelMap.insert
                                  (acc, Syntax.IdentifierLabel name, ty))
                             Syntax.LabelMap.empty items)
                        val dec = C.ValDec
                          { exp = C.Record
                              (List.foldl
                                 (fn ((name, v, _), m) =>
                                    Syntax.LabelMap.insert
                                      (m, Syntax.IdentifierLabel name, v))
                                 Syntax.LabelMap.empty items)
                          , results = [(SOME result, recordTy)]
                          }
                      in
                        ( recordTy
                        , prependRevDecs (dec :: revDecs, C.AppCont
                            {applied = k, args = [C.Var result]})
                        )
                      end
                    else
                      let
                        val dummyTy = F.RecordType Syntax.LabelMap.empty
                      in
                        ( dummyTy
                        , prependRevDecs (revDecs, C.AppCont
                            { applied = k
                            , args =
                                List.foldl (fn ((_, v, _), acc) => v :: acc) []
                                  items
                            })
                        )
                      end)
           | META _ => raise Fail "unexpected META")
  end
end;

structure CpsSimplify :>
sig
  type Context =
    {nextTyVar: int ref, nextVId: int ref, simplificationOccurred: bool ref}
  type value_info = {exp: CSyntax.SimpleExp option, isDiscardableFunction: bool}
  val genContSym: Context -> CSyntax.CVar
  val newVIdWithName: Context * Syntax.SourceName.name -> TypedSyntax.VId
  val newVId: Context * string -> TypedSyntax.VId
  val renewTyVar: Context * TypedSyntax.TyVar -> TypedSyntax.TyVar
  val renewVId: Context * TypedSyntax.VId -> TypedSyntax.VId
  val renewCVar: Context * CSyntax.CVar -> CSyntax.CVar
  val substSimpleExp:
    FSyntax.Ty TypedSyntax.TyVarMap.map
    * CSyntax.Value TypedSyntax.VIdMap.map
    * CSyntax.CVar CSyntax.CVarMap.map
    * CSyntax.SimpleExp
    -> CSyntax.SimpleExp
  val sizeOfStat: CSyntax.Stat * int -> int
  val substTy: FSyntax.Ty TypedSyntax.TyVarMap.map -> FSyntax.Ty -> FSyntax.Ty
  val substValue:
    FSyntax.Ty TypedSyntax.TyVarMap.map * CSyntax.Value TypedSyntax.VIdMap.map
    -> CSyntax.Value
    -> CSyntax.Value
  val substCVar: CSyntax.CVar CSyntax.CVarMap.map
                 -> CSyntax.CVar
                 -> CSyntax.CVar
  val substStat:
    FSyntax.Ty TypedSyntax.TyVarMap.map
    * CSyntax.Value TypedSyntax.VIdMap.map
    * CSyntax.CVar CSyntax.CVarMap.map
    * CSyntax.Stat
    -> CSyntax.Stat
  val alphaConvert:
    Context
    * FSyntax.Ty TypedSyntax.TyVarMap.map
    * CSyntax.Value TypedSyntax.VIdMap.map
    * CSyntax.CVar CSyntax.CVarMap.map
    * CSyntax.Stat
    -> CSyntax.Stat
  val alphaConvertWithNameOverride:
    Context
    * Syntax.SourceName.name TypedSyntax.VIdMap.map
    * FSyntax.Ty TypedSyntax.TyVarMap.map
    * CSyntax.Value TypedSyntax.VIdMap.map
    * CSyntax.CVar CSyntax.CVarMap.map
    * CSyntax.Stat
    -> CSyntax.Stat
  val isDiscardableExp: value_info TypedSyntax.VIdMap.map * CSyntax.Stat -> bool
  val finalizeStat: Context * CSyntax.Stat -> CSyntax.Stat
end =
struct
  local structure F = FSyntax structure C = CSyntax
  in
    type Context =
      {nextTyVar: int ref, nextVId: int ref, simplificationOccurred: bool ref}
    fun genContSym (ctx: Context) : CSyntax.CVar =
      let
        val n = !(#nextVId ctx)
        val _ = #nextVId ctx := n + 1
      in
        CSyntax.CVar.fromInt n
      end
    fun newVIdWithName ({nextVId, ...}: Context, name) =
      let val n = !nextVId
      in nextVId := n + 1; TypedSyntax.MkVId (name, n)
      end
    fun newVId (ctx, name) =
      newVIdWithName (ctx, Syntax.SourceName.fromString name)
    fun renewTyVar ({nextTyVar, ...}: Context, TypedSyntax.MkTyVar (name, _)) =
      let val n = !nextTyVar
      in TypedSyntax.MkTyVar (name, n) before (nextTyVar := n + 1)
      end
    fun renewVId ({nextVId, ...}: Context, TypedSyntax.MkVId (name, _)) =
      let val n = !nextVId
      in TypedSyntax.MkVId (name, n) before (nextVId := n + 1)
      end
    fun renewCVar ({nextVId, ...}: Context, _: C.CVar) =
      let val n = !nextVId
      in C.CVar.fromInt n before (nextVId := n + 1)
      end
    fun sizeOfSimpleExp (e, threshold) =
      if threshold < 0 then
        threshold
      else
        case e of
          C.PrimOp {primOp = _, tyargs = _, args} =>
            threshold - List.length args
        | C.Record fields => threshold - Syntax.LabelMap.numItems fields
        | C.ExnTag _ => threshold - 1
        | C.Projection _ => threshold - 1
        | C.Abs
            { contParam = _
            , tyParams = _
            , params = _
            , body
            , resultTy = _
            , attr = _
            } => sizeOfStat (body, threshold)
    and sizeOfDec (dec, threshold) =
      if threshold < 0 then
        threshold
      else
        case dec of
          C.ValDec {exp, results = _} => sizeOfSimpleExp (exp, threshold)
        | C.RecDec defs =>
            List.foldl (fn ({body, ...}, t) => sizeOfStat (body, t)) threshold
              defs
        | C.UnpackDec _ => threshold
        | C.ContDec {name = _, params = _, body, attr = _} =>
            sizeOfStat (body, threshold)
        | C.RecContDec defs =>
            List.foldl (fn ((_, _, body), t) => sizeOfStat (body, t)) threshold
              defs
        | C.DatatypeDec _ => 0
        | C.ESImportDec _ => 0
    and sizeOfStat (e, threshold) =
      if threshold < 0 then
        threshold
      else
        case e of
          C.Let {decs, cont} =>
            List.foldl sizeOfDec (sizeOfStat (cont, threshold)) decs
        | C.App {applied = _, cont = _, tyArgs = _, args, attr = _} =>
            threshold - List.length args
        | C.AppCont {applied = _, args} => threshold - List.length args
        | C.If {cond = _, thenCont, elseCont} =>
            sizeOfStat (elseCont, sizeOfStat (thenCont, threshold - 1))
        | C.Handle
            { body
            , handler = (_, h)
            , successfulExitIn = _
            , successfulExitOut = _
            , resultTy = _
            } => sizeOfStat (body, sizeOfStat (h, threshold - 1))
        | C.Raise _ => threshold
        | C.Unreachable => threshold
    fun substTy (tysubst: FSyntax.Ty TypedSyntax.TyVarMap.map) =
      #doTy (FSyntax.lazySubstTy tysubst)
    fun substValue (tysubst, subst: C.Value TypedSyntax.VIdMap.map) =
      let
        val goTy = substTy tysubst
        fun goVal (x as C.Var v) =
              (case TypedSyntax.VIdMap.find (subst, v) of
                 SOME w => w
               | NONE => x)
          | goVal (C.Cast {value, from, to}) =
              C.Cast {value = goVal value, from = goTy from, to = goTy to}
          | goVal (C.Pack {value, payloadTy, packageTy}) =
              C.Pack
                { value = goVal value
                , payloadTy = goTy payloadTy
                , packageTy = goTy packageTy
                }
          | goVal (v as C.Unit) = v
          | goVal (v as C.Nil) = v
          | goVal (C.TypedNil ty) =
              C.TypedNil (goTy ty)
          | goVal (v as C.BoolConst _) = v
          | goVal (v as C.IntConst _) = v
          | goVal (v as C.WordConst _) = v
          | goVal (v as C.CharConst _) = v
          | goVal (v as C.Char7Const _) = v
          | goVal (v as C.Char16Const _) = v
          | goVal (v as C.Char32Const _) = v
          | goVal (v as C.UCharConst _) = v
          | goVal (v as C.StringConst _) = v
          | goVal (v as C.String7Const _) = v
          | goVal (v as C.String16Const _) = v
          | goVal (v as C.String32Const _) = v
          | goVal (v as C.UStringConst _) = v
          | goVal (v as C.PrimEffect _) = v
      in
        goVal
      end
    fun substCVar (csubst: C.CVar C.CVarMap.map) v =
      case C.CVarMap.find (csubst, v) of
        SOME w => w
      | NONE => v
    fun substSimpleExp (tysubst, subst, _, C.PrimOp {primOp, tyargs, args}) =
          C.PrimOp
            { primOp = primOp
            , tyargs = List.map (substTy tysubst) tyargs
            , args = List.map (substValue (tysubst, subst)) args
            }
      | substSimpleExp (tysubst, subst, _, C.Record fields) =
          C.Record (Syntax.LabelMap.map (substValue (tysubst, subst)) fields)
      | substSimpleExp (tysubst, _, _, C.ExnTag {name, payloadTy}) =
          C.ExnTag
            {name = name, payloadTy = Option.map (substTy tysubst) payloadTy}
      | substSimpleExp
          (tysubst, subst, _, C.Projection {label, record, fieldTypes}) =
          C.Projection
            { label = label
            , record = substValue (tysubst, subst) record
            , fieldTypes = Syntax.LabelMap.map (substTy tysubst) fieldTypes
            }
      | substSimpleExp
          ( tysubst
          , subst
          , csubst
          , C.Abs {contParam, tyParams, params, body, resultTy, attr}
          ) =
          let
            val sTy = substTy tysubst
          in
            C.Abs
              { contParam = contParam
              , tyParams = tyParams
              , params = List.map (fn (v, ty) => (v, sTy ty)) params
              , body = substStat (tysubst, subst, csubst, body)
              , resultTy = sTy resultTy
              , attr = attr
              }
          end
    and substDec (tysubst, subst, csubst) =
      fn C.ValDec {exp, results} =>
        let
          val sTy = substTy tysubst
        in
          C.ValDec
            { exp = substSimpleExp (tysubst, subst, csubst, exp)
            , results = List.map (fn (v, ty) => (v, sTy ty)) results
            }
        end
       | C.RecDec defs =>
        let
          val sTy = substTy tysubst
        in
          C.RecDec
            (List.map
               (fn {name, contParam, tyParams, params, body, resultTy, attr} =>
                  { name = name
                  , contParam = contParam
                  , tyParams = tyParams
                  , params = List.map (fn (v, ty) => (v, sTy ty)) params
                  , body = substStat (tysubst, subst, csubst, body)
                  , resultTy = sTy resultTy
                  , attr = attr
                  }) defs)
        end
       | C.UnpackDec {tyVar, kind, vid, unpackedTy, package} =>
        C.UnpackDec
          { tyVar = tyVar
          , kind = kind
          , vid = vid
          , unpackedTy = substTy tysubst unpackedTy
          , package = substValue (tysubst, subst) package
          }
       | C.ContDec {name, params, body, attr} =>
        let
          val sTy = substTy tysubst
        in
          C.ContDec
            { name = name
            , params = List.map (fn (v, ty) => (v, sTy ty)) params
            , body = substStat (tysubst, subst, csubst, body)
            , attr = attr
            }
        end
       | C.RecContDec defs =>
        let
          val sTy = substTy tysubst
        in
          C.RecContDec
            (List.map
               (fn (f, params, body) =>
                  ( f
                  , List.map (fn (v, ty) => (v, sTy ty)) params
                  , substStat (tysubst, subst, csubst, body)
                  )) defs)
        end
       | dec as C.DatatypeDec _ => dec
       | C.ESImportDec {pure, specs, moduleName} =>
        let
          val sTy = substTy tysubst
        in
          C.ESImportDec
            { pure = pure
            , specs = List.map (fn (name, v, ty) => (name, v, sTy ty)) specs
            , moduleName = moduleName
            }
        end
    and substStat
          ( tysubst: FSyntax.Ty TypedSyntax.TyVarMap.map
          , subst: C.Value TypedSyntax.VIdMap.map
          , csubst: C.CVar C.CVarMap.map
          , C.Let {decs, cont}
          ) =
          C.Let
            { decs = List.map (substDec (tysubst, subst, csubst)) decs
            , cont = substStat (tysubst, subst, csubst, cont)
            }
      | substStat
          (tysubst, subst, csubst, C.App {applied, cont, tyArgs, args, attr}) =
          C.App
            { applied = substValue (tysubst, subst) applied
            , cont = substCVar csubst cont
            , tyArgs = List.map (substTy tysubst) tyArgs
            , args = List.map (substValue (tysubst, subst)) args
            , attr = attr
            }
      | substStat (tysubst, subst, csubst, C.AppCont {applied, args}) =
          C.AppCont
            { applied = substCVar csubst applied
            , args = List.map (substValue (tysubst, subst)) args
            }
      | substStat (tysubst, subst, csubst, C.If {cond, thenCont, elseCont}) =
          C.If
            { cond = substValue (tysubst, subst) cond
            , thenCont = substStat (tysubst, subst, csubst, thenCont)
            , elseCont = substStat (tysubst, subst, csubst, elseCont)
            }
      | substStat
          ( tysubst
          , subst
          , csubst
          , C.Handle
              { body
              , handler = (e, h)
              , successfulExitIn
              , successfulExitOut
              , resultTy
              }
          ) =
          C.Handle
            { body = substStat (tysubst, subst, csubst, body)
            , handler = (e, substStat (tysubst, subst, csubst, h))
            , successfulExitIn = successfulExitIn
            , successfulExitOut = substCVar csubst successfulExitOut
            , resultTy = substTy tysubst resultTy
            }
      | substStat (tysubst, subst, _, C.Raise (span, x)) =
          C.Raise (span, substValue (tysubst, subst) x)
      | substStat (_, _, _, e as C.Unreachable) = e
    val substStat = fn (tysubst, subst, csubst, e) =>
      if
        TypedSyntax.TyVarMap.isEmpty tysubst
        andalso TypedSyntax.VIdMap.isEmpty subst
        andalso C.CVarMap.isEmpty csubst
      then e
      else substStat (tysubst, subst, csubst, e)
    fun alphaConvertSimpleExp
          ( ctx
          , tysubst
          , subst
          , csubst
          , C.Abs {contParam, tyParams, params, body, resultTy, attr}
          ) =
          let
            val (tyParams', tysubst') =
              List.foldr
                (fn ((tv, kind), (tyParams', tysubst)) =>
                   let
                     val tv' = renewTyVar (ctx, tv)
                   (* val () = print
                     ("alphaConvert: Abs: " ^ TypedSyntax.print_TyVar tv
                      ^ " -> " ^ TypedSyntax.print_TyVar tv' ^ "\n") *)
                   in
                     ( (tv', kind) :: tyParams'
                     , TypedSyntax.TyVarMap.insert
                         (tysubst, tv, FSyntax.TyVar tv')
                     )
                   end) ([], tysubst) tyParams
            val sTy = substTy tysubst'
            val (params', subst') =
              List.foldr
                (fn ((p, ty), (params', subst)) =>
                   let
                     val p' = renewVId (ctx, p)
                   in
                     ( (p', sTy ty) :: params'
                     , TypedSyntax.VIdMap.insert (subst, p, C.Var p')
                     )
                   end) ([], subst) params
            val contParam' = renewCVar (ctx, contParam)
            val csubst' = C.CVarMap.insert (csubst, contParam, contParam')
          in
            C.Abs
              { contParam = contParam'
              , tyParams = tyParams'
              , params = params'
              , body = alphaConvert (ctx, tysubst', subst', csubst', body)
              , resultTy = sTy resultTy
              , attr = attr
              }
          end
      | alphaConvertSimpleExp (_, tysubst, subst, csubst, e) =
          substSimpleExp (tysubst, subst, csubst, e)
    and alphaConvertDec (ctx: Context) (dec, (tysubst, subst, csubst, acc)) =
      case dec of
        C.ValDec {exp, results} =>
          let
            val sTy = substTy tysubst
            val (results', subst') =
              List.foldr
                (fn ((SOME result, ty), (acc, subst)) =>
                   let
                     val result' = renewVId (ctx, result)
                   in
                     ( (SOME result', sTy ty) :: acc
                     , TypedSyntax.VIdMap.insert (subst, result, C.Var result')
                     )
                   end
                  | ((NONE, ty), (acc, subst)) => ((NONE, sTy ty) :: acc, subst))
                ([], subst) results
            val dec' = C.ValDec
              { exp = alphaConvertSimpleExp (ctx, tysubst, subst, csubst, exp)
              , results = results'
              }
          in
            (tysubst, subst', csubst, dec' :: acc)
          end
      | C.RecDec defs =>
          let
            val (subst, nameMap) =
              List.foldl
                (fn ({name, ...}, (subst, nameMap)) =>
                   let
                     val name' = renewVId (ctx, name)
                   in
                     ( TypedSyntax.VIdMap.insert (subst, name, C.Var name')
                     , TypedSyntax.VIdMap.insert (nameMap, name, name')
                     )
                   end) (subst, TypedSyntax.VIdMap.empty) defs
            val dec' = C.RecDec
              (List.map
                 (fn {name, contParam, tyParams, params, body, resultTy, attr} =>
                    let
                      val name' = TypedSyntax.VIdMap.lookup (nameMap, name)
                      val contParam' = renewCVar (ctx, contParam)
                      val (tyParams', tysubst') =
                        List.foldr
                          (fn ((tv, kind), (tyParams', tysubst)) =>
                             let
                               val tv' = renewTyVar (ctx, tv)
                             (* val () = print
                               ("alphaConvert: Rec: "
                                ^ TypedSyntax.print_TyVar tv ^ " -> "
                                ^ TypedSyntax.print_TyVar tv' ^ "\n") *)
                             in
                               ( (tv', kind) :: tyParams'
                               , TypedSyntax.TyVarMap.insert
                                   (tysubst, tv, FSyntax.TyVar tv')
                               )
                             end) ([], tysubst) tyParams
                      val sTy = substTy tysubst'
                      val (params', subst) =
                        List.foldr
                          (fn ((p, ty), (params', subst)) =>
                             let
                               val p' = renewVId (ctx, p)
                             in
                               ( (p', sTy ty) :: params'
                               , TypedSyntax.VIdMap.insert (subst, p, C.Var p')
                               )
                             end) ([], subst) params
                      val csubst =
                        C.CVarMap.insert (csubst, contParam, contParam')
                    in
                      { name = name'
                      , contParam = contParam'
                      , tyParams = tyParams'
                      , params = params'
                      , body = alphaConvert (ctx, tysubst', subst, csubst, body)
                      , resultTy = sTy resultTy
                      , attr = attr
                      }
                    end) defs)
          in
            (tysubst, subst, csubst, dec' :: acc)
          end
      | C.UnpackDec {tyVar, kind, vid, unpackedTy, package} =>
          let
            val package' = substValue (tysubst, subst) package
            val vid' = renewVId (ctx, vid)
            val tyVar' = renewTyVar (ctx, tyVar)
            (* val () = print
              ("alphaConvert: Unpack: " ^ TypedSyntax.print_TyVar tyVar ^ " -> "
               ^ TypedSyntax.print_TyVar tyVar' ^ "\n") *)
            val subst' = TypedSyntax.VIdMap.insert (subst, vid, C.Var vid')
            val tysubst' =
              TypedSyntax.TyVarMap.insert (tysubst, tyVar, FSyntax.TyVar tyVar')
            val unpackedTy' = substTy tysubst' unpackedTy
            val dec = C.UnpackDec
              { tyVar = tyVar'
              , kind = kind
              , vid = vid'
              , unpackedTy = unpackedTy'
              , package = package'
              }
          in
            (tysubst', subst', csubst, dec :: acc)
          end
      | C.ContDec {name, params, body, attr} =>
          let
            val sTy = substTy tysubst
            val (params', subst') =
              List.foldr
                (fn ((SOME p, ty), (params', subst)) =>
                   let
                     val p' = renewVId (ctx, p)
                   in
                     ( (SOME p', sTy ty) :: params'
                     , TypedSyntax.VIdMap.insert (subst, p, C.Var p')
                     )
                   end
                  | ((NONE, ty), (params', subst)) =>
                   ((NONE, sTy ty) :: params', subst)) ([], subst) params
            val body = alphaConvert (ctx, tysubst, subst', csubst, body)
            val name' = renewCVar (ctx, name)
            val csubst = C.CVarMap.insert (csubst, name, name')
            val dec' = C.ContDec
              {name = name', params = params', body = body, attr = attr}
          in
            (tysubst, subst, csubst, dec' :: acc)
          end
      | C.RecContDec defs =>
          let
            val (csubst, nameMap) =
              List.foldl
                (fn ((f, _, _), (csubst, nameMap)) =>
                   let
                     val f' = renewCVar (ctx, f)
                   in
                     ( C.CVarMap.insert (csubst, f, f')
                     , C.CVarMap.insert (nameMap, f, f')
                     )
                   end) (csubst, C.CVarMap.empty) defs
            val sTy = substTy tysubst
            val dec' = C.RecContDec
              (List.map
                 (fn (f, params, body) =>
                    let
                      val f' = C.CVarMap.lookup (nameMap, f)
                      val (params', subst) =
                        List.foldr
                          (fn ((SOME p, ty), (params', subst)) =>
                             let
                               val p' = renewVId (ctx, p)
                             in
                               ( (SOME p', sTy ty) :: params'
                               , TypedSyntax.VIdMap.insert (subst, p, C.Var p')
                               )
                             end
                            | ((NONE, ty), (params', subst)) =>
                             ((NONE, sTy ty) :: params', subst)) ([], subst)
                          params
                    in
                      ( f'
                      , params'
                      , alphaConvert (ctx, tysubst, subst, csubst, body)
                      )
                    end) defs)
          in
            (tysubst, subst, csubst, dec' :: acc)
          end
      | C.DatatypeDec (tyVar, kind) =>
          let
            val tyVar' = renewTyVar (ctx, tyVar)
            val tysubst' =
              TypedSyntax.TyVarMap.insert (tysubst, tyVar, FSyntax.TyVar tyVar')
          in
            (tysubst', subst, csubst, C.DatatypeDec (tyVar', kind) :: acc)
          end
      | C.ESImportDec {pure, specs, moduleName} =>
          let
            val sTy = substTy tysubst
            val specs' =
              List.map
                (fn (name, vid, ty) => (name, vid, renewVId (ctx, vid), sTy ty))
                specs
            val subst' =
              List.foldl
                (fn ((_, vid, vid', _), subst) =>
                   TypedSyntax.VIdMap.insert (subst, vid, C.Var vid')) subst
                specs'
            val dec' = C.ESImportDec
              { pure = pure
              , specs =
                  List.map (fn (name, _, vid, ty) => (name, vid, ty)) specs'
              , moduleName = moduleName
              }
          in
            (tysubst, subst', csubst, dec' :: acc)
          end
    and alphaConvert
          ( ctx: Context
          , tysubst: FSyntax.Ty TypedSyntax.TyVarMap.map
          , subst: C.Value TypedSyntax.VIdMap.map
          , csubst: C.CVar C.CVarMap.map
          , C.Let {decs, cont}
          ) =
          let
            val (tysubst', subst', csubst', revDecs) =
              List.foldl (alphaConvertDec ctx) (tysubst, subst, csubst, []) decs
          in
            C.Let
              { decs = List.rev revDecs
              , cont = alphaConvert (ctx, tysubst', subst', csubst', cont)
              }
          end
      | alphaConvert
          (_, tysubst, subst, csubst, C.App {applied, cont, tyArgs, args, attr}) =
          C.App
            { applied = substValue (tysubst, subst) applied
            , cont = substCVar csubst cont
            , tyArgs = List.map (substTy tysubst) tyArgs
            , args = List.map (substValue (tysubst, subst)) args
            , attr = attr
            }
      | alphaConvert (_, tysubst, subst, csubst, C.AppCont {applied, args}) =
          C.AppCont
            { applied = substCVar csubst applied
            , args = List.map (substValue (tysubst, subst)) args
            }
      | alphaConvert
          (ctx, tysubst, subst, csubst, C.If {cond, thenCont, elseCont}) =
          C.If
            { cond = substValue (tysubst, subst) cond
            , thenCont = alphaConvert (ctx, tysubst, subst, csubst, thenCont)
            , elseCont = alphaConvert (ctx, tysubst, subst, csubst, elseCont)
            }
      | alphaConvert
          ( ctx
          , tysubst
          , subst
          , csubst
          , C.Handle
              { body
              , handler = (e, h)
              , successfulExitIn
              , successfulExitOut
              , resultTy
              }
          ) =
          let
            val successfulExitIn' = renewCVar (ctx, successfulExitIn)
            val csubst' =
              C.CVarMap.insert (csubst, successfulExitIn, successfulExitIn')
            val e' = renewVId (ctx, e)
            val subst' = TypedSyntax.VIdMap.insert (subst, e, C.Var e')
          in
            C.Handle
              { body = alphaConvert (ctx, tysubst, subst, csubst', body)
              , handler = (e', alphaConvert (ctx, tysubst, subst', csubst, h))
              , successfulExitIn = successfulExitIn'
              , successfulExitOut = substCVar csubst successfulExitOut
              , resultTy = substTy tysubst resultTy
              }
          end
      | alphaConvert (_, tysubst, subst, _, C.Raise (span, x)) =
          C.Raise (span, substValue (tysubst, subst) x)
      | alphaConvert (_, _, _, _, e as C.Unreachable) = e
    fun renewVIdWithOverride ({nextVId, ...}: Context, _, SOME name) =
          let val n = !nextVId
          in TypedSyntax.MkVId (name, n) before (nextVId := n + 1)
          end
      | renewVIdWithOverride
          ({nextVId, ...}: Context, TypedSyntax.MkVId (name, _), NONE) =
          let val n = !nextVId
          in TypedSyntax.MkVId (name, n) before (nextVId := n + 1)
          end
    fun alphaConvertWithNameOverrideDec (ctx: Context, nameOverride)
      (dec, (tysubst, subst, csubst, acc)) =
      case dec of
        C.ValDec {exp, results} =>
          let
            val sTy = substTy tysubst
            val (results', subst') =
              List.foldr
                (fn ((SOME result, ty), (acc, subst)) =>
                   let
                     val result' = renewVIdWithOverride
                       ( ctx
                       , result
                       , TypedSyntax.VIdMap.find (nameOverride, result)
                       )
                   in
                     ( (SOME result', sTy ty) :: acc
                     , TypedSyntax.VIdMap.insert (subst, result, C.Var result')
                     )
                   end
                  | ((NONE, ty), (acc, subst)) => ((NONE, sTy ty) :: acc, subst))
                ([], subst) results
            val dec' = C.ValDec
              { exp = alphaConvertSimpleExp (ctx, tysubst, subst, csubst, exp)
              , results = results'
              }
          in
            (tysubst, subst', csubst, dec' :: acc)
          end
      | C.RecDec defs =>
          let
            val (subst, nameMap) =
              List.foldl
                (fn ({name, ...}, (subst, nameMap)) =>
                   let
                     val name' = renewVIdWithOverride
                       (ctx, name, TypedSyntax.VIdMap.find (nameOverride, name))
                   in
                     ( TypedSyntax.VIdMap.insert (subst, name, C.Var name')
                     , TypedSyntax.VIdMap.insert (nameMap, name, name')
                     )
                   end) (subst, TypedSyntax.VIdMap.empty) defs
            val dec' = C.RecDec
              (List.map
                 (fn {name, contParam, tyParams, params, body, resultTy, attr} =>
                    let
                      val name' = TypedSyntax.VIdMap.lookup (nameMap, name)
                      val contParam' = renewCVar (ctx, contParam)
                      val (tyParams', tysubst') =
                        List.foldr
                          (fn ((tv, kind), (tyParams', tysubst)) =>
                             let
                               val tv' = renewTyVar (ctx, tv)
                             in
                               ( (tv', kind) :: tyParams'
                               , TypedSyntax.TyVarMap.insert
                                   (tysubst, tv, FSyntax.TyVar tv')
                               )
                             end) ([], tysubst) tyParams
                      val sTy = substTy tysubst'
                      val (params', subst) =
                        List.foldr
                          (fn ((p, ty), (params', subst)) =>
                             let
                               val p' = renewVId (ctx, p)
                             in
                               ( (p', sTy ty) :: params'
                               , TypedSyntax.VIdMap.insert (subst, p, C.Var p')
                               )
                             end) ([], subst) params
                      val csubst =
                        C.CVarMap.insert (csubst, contParam, contParam')
                    in
                      { name = name'
                      , contParam = contParam'
                      , tyParams = tyParams'
                      , params = params'
                      , body = alphaConvert (ctx, tysubst', subst, csubst, body)
                      , resultTy = sTy resultTy
                      , attr = attr
                      }
                    end) defs)
          in
            (tysubst, subst, csubst, dec' :: acc)
          end
      | C.UnpackDec {tyVar, kind, vid, unpackedTy, package} =>
          let
            val package' = substValue (tysubst, subst) package
            val vid' = renewVIdWithOverride
              (ctx, vid, TypedSyntax.VIdMap.find (nameOverride, vid))
            val tyVar' = renewTyVar (ctx, tyVar)
            val subst' = TypedSyntax.VIdMap.insert (subst, vid, C.Var vid')
            val tysubst' =
              TypedSyntax.TyVarMap.insert (tysubst, tyVar, FSyntax.TyVar tyVar')
            val unpackedTy' = substTy tysubst' unpackedTy
            val dec = C.UnpackDec
              { tyVar = tyVar'
              , kind = kind
              , vid = vid'
              , unpackedTy = unpackedTy'
              , package = package'
              }
          in
            (tysubst', subst', csubst, dec :: acc)
          end
      | C.ContDec {name, params, body, attr} =>
          let
            val sTy = substTy tysubst
            val (params', subst') =
              List.foldr
                (fn ((SOME p, ty), (params', subst)) =>
                   let
                     val p' = renewVIdWithOverride
                       (ctx, p, TypedSyntax.VIdMap.find (nameOverride, p))
                   in
                     ( (SOME p', sTy ty) :: params'
                     , TypedSyntax.VIdMap.insert (subst, p, C.Var p')
                     )
                   end
                  | ((NONE, ty), (params', subst)) =>
                   ((NONE, sTy ty) :: params', subst)) ([], subst) params
            val body = alphaConvertWithNameOverride
              (ctx, nameOverride, tysubst, subst', csubst, body)
            val name' = renewCVar (ctx, name)
            val csubst = C.CVarMap.insert (csubst, name, name')
            val dec' = C.ContDec
              {name = name', params = params', body = body, attr = attr}
          in
            (tysubst, subst, csubst, dec' :: acc)
          end
      | C.RecContDec defs =>
          let
            val (csubst, nameMap) =
              List.foldl
                (fn ((f, _, _), (csubst, nameMap)) =>
                   let
                     val f' = renewCVar (ctx, f)
                   in
                     ( C.CVarMap.insert (csubst, f, f')
                     , C.CVarMap.insert (nameMap, f, f')
                     )
                   end) (csubst, C.CVarMap.empty) defs
            val sTy = substTy tysubst
            val dec' = C.RecContDec
              (List.map
                 (fn (f, params, body) =>
                    let
                      val f' = C.CVarMap.lookup (nameMap, f)
                      val (params', subst) =
                        List.foldr
                          (fn ((SOME p, ty), (params', subst)) =>
                             let
                               val p' = renewVIdWithOverride
                                 ( ctx
                                 , p
                                 , TypedSyntax.VIdMap.find (nameOverride, p)
                                 )
                             in
                               ( (SOME p', sTy ty) :: params'
                               , TypedSyntax.VIdMap.insert (subst, p, C.Var p')
                               )
                             end
                            | ((NONE, ty), (params', subst)) =>
                             ((NONE, sTy ty) :: params', subst)) ([], subst)
                          params
                    in
                      ( f'
                      , params'
                      , alphaConvertWithNameOverride
                          (ctx, nameOverride, tysubst, subst, csubst, body)
                      )
                    end) defs)
          in
            (tysubst, subst, csubst, dec' :: acc)
          end
      | C.DatatypeDec (tyVar, kind) =>
          let
            val tyVar' = renewTyVar (ctx, tyVar)
            val tysubst' =
              TypedSyntax.TyVarMap.insert (tysubst, tyVar, FSyntax.TyVar tyVar')
          in
            (tysubst', subst, csubst, C.DatatypeDec (tyVar', kind) :: acc)
          end
      | C.ESImportDec {pure, specs, moduleName} =>
          let
            val sTy = substTy tysubst
            val specs' =
              List.map
                (fn (name, vid, ty) =>
                   ( name
                   , vid
                   , renewVIdWithOverride
                       (ctx, vid, TypedSyntax.VIdMap.find (nameOverride, vid))
                   , sTy ty
                   )) specs
            val subst' =
              List.foldl
                (fn ((_, vid, vid', _), subst) =>
                   TypedSyntax.VIdMap.insert (subst, vid, C.Var vid')) subst
                specs'
            val dec' = C.ESImportDec
              { pure = pure
              , specs =
                  List.map (fn (name, _, vid, ty) => (name, vid, ty)) specs'
              , moduleName = moduleName
              }
          in
            (tysubst, subst', csubst, dec' :: acc)
          end
    and alphaConvertWithNameOverride
          ( ctx: Context
          , nameOverride: Syntax.SourceName.name TypedSyntax.VIdMap.map
          , tysubst: FSyntax.Ty TypedSyntax.TyVarMap.map
          , subst: C.Value TypedSyntax.VIdMap.map
          , csubst: C.CVar C.CVarMap.map
          , C.Let {decs, cont}
          ) =
          let
            val (tysubst', subst', csubst', revDecs) =
              List.foldl (alphaConvertWithNameOverrideDec (ctx, nameOverride))
                (tysubst, subst, csubst, []) decs
          in
            C.Let
              { decs = List.rev revDecs
              , cont = alphaConvertWithNameOverride
                  (ctx, nameOverride, tysubst', subst', csubst', cont)
              }
          end
      | alphaConvertWithNameOverride
          ( _
          , _
          , tysubst
          , subst
          , csubst
          , C.App {applied, cont, tyArgs, args, attr}
          ) =
          C.App
            { applied = substValue (tysubst, subst) applied
            , cont = substCVar csubst cont
            , tyArgs = List.map (substTy tysubst) tyArgs
            , args = List.map (substValue (tysubst, subst)) args
            , attr = attr
            }
      | alphaConvertWithNameOverride
          (_, _, tysubst, subst, csubst, C.AppCont {applied, args}) =
          C.AppCont
            { applied = substCVar csubst applied
            , args = List.map (substValue (tysubst, subst)) args
            }
      | alphaConvertWithNameOverride
          (ctx, no, tysubst, subst, csubst, C.If {cond, thenCont, elseCont}) =
          C.If
            { cond = substValue (tysubst, subst) cond
            , thenCont = alphaConvertWithNameOverride
                (ctx, no, tysubst, subst, csubst, thenCont)
            , elseCont = alphaConvertWithNameOverride
                (ctx, no, tysubst, subst, csubst, elseCont)
            }
      | alphaConvertWithNameOverride
          ( ctx
          , no
          , tysubst
          , subst
          , csubst
          , C.Handle
              { body
              , handler = (e, h)
              , successfulExitIn
              , successfulExitOut
              , resultTy
              }
          ) =
          let
            val successfulExitIn' = renewCVar (ctx, successfulExitIn)
            val csubst' =
              C.CVarMap.insert (csubst, successfulExitIn, successfulExitIn')
            val e' = renewVIdWithOverride
              (ctx, e, TypedSyntax.VIdMap.find (no, e))
            val subst' = TypedSyntax.VIdMap.insert (subst, e, C.Var e')
          in
            C.Handle
              { body = alphaConvert (ctx, tysubst, subst, csubst', body)
              , handler = (e', alphaConvertWithNameOverride
                  (ctx, no, tysubst, subst', csubst, h))
              , successfulExitIn = successfulExitIn'
              , successfulExitOut = substCVar csubst successfulExitOut
              , resultTy = substTy tysubst resultTy
              }
          end
      | alphaConvertWithNameOverride
          (_, _, tysubst, subst, _, C.Raise (span, x)) =
          C.Raise (span, substValue (tysubst, subst) x)
      | alphaConvertWithNameOverride (_, _, _, _, _, e as C.Unreachable) = e
    type value_info = {exp: C.SimpleExp option, isDiscardableFunction: bool}
    fun isDiscardableDec (dec, env: value_info TypedSyntax.VIdMap.map) =
      case dec of
        C.ValDec {exp, results} =>
          (case exp of
             C.Abs {body, ...} =>
               let
                 val env =
                   case results of
                     [(SOME result, _)] =>
                       TypedSyntax.VIdMap.insert
                         ( env
                         , result
                         , { exp = NONE
                           , isDiscardableFunction =
                               isDiscardableExp (env, body)
                           }
                         )
                   | _ => env
               in
                 SOME env
               end
           | _ => if C.isDiscardable exp then SOME env else NONE)
      | C.RecDec _ => SOME env
      | C.UnpackDec _ => SOME env
      | C.ContDec {name = _, params = _, body, attr = _} =>
          if isDiscardableExp (env, body) then SOME env else NONE
      | C.RecContDec _ => NONE
      | C.DatatypeDec _ => SOME env
      | C.ESImportDec {pure = _, specs = _, moduleName = _} => SOME env
    and isDiscardableExp
          (env: value_info TypedSyntax.VIdMap.map, C.Let {decs, cont}) =
          (case ListUtil.foldlOption isDiscardableDec env decs of
             SOME env => isDiscardableExp (env, cont)
           | NONE => false)
      | isDiscardableExp
          ( env
          , C.App
              { applied = C.Var applied
              , cont = _
              , tyArgs = _
              , args = _
              , attr = _
              }
          ) =
          (case TypedSyntax.VIdMap.find (env, applied) of
             SOME {isDiscardableFunction = true, ...} => true
           | _ => false)
      | isDiscardableExp (_, C.App _) = false (* should not occur *)
      | isDiscardableExp (_, C.AppCont _) = true
      | isDiscardableExp (env, C.If {cond = _, thenCont, elseCont}) =
          isDiscardableExp (env, thenCont)
          andalso isDiscardableExp (env, elseCont)
      | isDiscardableExp
          ( env
          , C.Handle
              { body
              , handler = (_, h)
              , successfulExitIn = _
              , successfulExitOut = _
              , resultTy = _
              }
          ) =
          isDiscardableExp (env, body) andalso isDiscardableExp (env, h)
      | isDiscardableExp (_, C.Raise _) = false
      | isDiscardableExp (_, C.Unreachable) = false
    fun prependDecs ([], cont) = cont
      | prependDecs (decs, C.Let {decs = decs', cont}) =
          C.Let {decs = decs @ decs', cont = cont}
      | prependDecs (decs, cont) = C.Let {decs = decs, cont = cont}
    (* Eliminate assumeDiscardable *)
    (* More sophisticated analysis is wanted. *)
    fun finalizeDec ctx (dec, (decs, cont)) =
      case dec of
        C.ValDec
          { exp =
              C.PrimOp
                { primOp = F.PrimCall Primitives.assumeDiscardable
                , tyargs = _
                , args = [f, arg]
                }
          , results = [(SOME result, ty)]
          } =>
          let
            val name = genContSym ctx
          in
            ( [C.ContDec
                 { name = name
                 , params = [(SOME result, ty)]
                 , body = prependDecs (decs, cont)
                 , attr = {alwaysInline = false}
                 }]
            , C.App
                { applied = f
                , cont = name
                , tyArgs = []
                , args = [arg]
                , attr = {typeOnly = false}
                }
            )
          end
      | C.ValDec
          { exp =
              C.PrimOp
                { primOp = F.PrimCall Primitives.assumeDiscardable
                , tyargs = _
                , args = _
                }
          , results = _
          } => raise Fail "assumeDiscardable: invalid argument"
      | C.ValDec {exp = C.PrimOp _, results = _} => (dec :: decs, cont)
      | C.ValDec {exp = C.Record _, results = _} => (dec :: decs, cont)
      | C.ValDec {exp = C.ExnTag _, results = _} => (dec :: decs, cont)
      | C.ValDec {exp = C.Projection _, results = _} => (dec :: decs, cont)
      | C.ValDec
          { exp = C.Abs {contParam, tyParams, params, body, resultTy, attr}
          , results
          } =>
          let
            val dec = C.ValDec
              { exp = C.Abs
                  { contParam = contParam
                  , tyParams = tyParams
                  , params = params
                  , body = finalizeStat (ctx, body)
                  , resultTy = resultTy
                  , attr = attr
                  }
              , results = results
              }
          in
            (dec :: decs, cont)
          end
      | C.RecDec defs =>
          let
            val dec = C.RecDec
              (List.map
                 (fn {name, contParam, tyParams, params, body, resultTy, attr} =>
                    { name = name
                    , contParam = contParam
                    , tyParams = tyParams
                    , params = params
                    , body = finalizeStat (ctx, body)
                    , resultTy = resultTy
                    , attr = attr
                    }) defs)
          in
            (dec :: decs, cont)
          end
      | C.UnpackDec _ => (dec :: decs, cont)
      | C.ContDec {name, params, body, attr} =>
          let
            val dec = C.ContDec
              { name = name
              , params = params
              , body = finalizeStat (ctx, body)
              , attr = attr
              }
          in
            (dec :: decs, cont)
          end
      | C.RecContDec defs =>
          let
            val dec = C.RecContDec
              (List.map
                 (fn (name, params, body) =>
                    (name, params, finalizeStat (ctx, body))) defs)
          in
            (dec :: decs, cont)
          end
      | C.DatatypeDec _ => (dec :: decs, cont)
      | C.ESImportDec _ => (dec :: decs, cont)
    and finalizeStat (ctx, C.Let {decs, cont}) =
          prependDecs
            (List.foldr (finalizeDec ctx) ([], finalizeStat (ctx, cont)) decs)
      | finalizeStat (_, e as C.App _) = e
      | finalizeStat (_, e as C.AppCont _) = e
      | finalizeStat (ctx, C.If {cond, thenCont, elseCont}) =
          C.If
            { cond = cond
            , thenCont = finalizeStat (ctx, thenCont)
            , elseCont = finalizeStat (ctx, elseCont)
            }
      | finalizeStat
          ( ctx
          , C.Handle
              { body
              , handler = (e, h)
              , successfulExitIn
              , successfulExitOut
              , resultTy
              }
          ) =
          C.Handle
            { body = finalizeStat (ctx, body)
            , handler = (e, finalizeStat (ctx, h))
            , successfulExitIn = successfulExitIn
            , successfulExitOut = successfulExitOut
            , resultTy = resultTy
            }
      | finalizeStat (_, e as C.Raise _) = e
      | finalizeStat (_, e as C.Unreachable) = e
  end
end;

structure CpsAnalyze :>
sig
  type cont_map

  (*
   * The JS-DS backend must determine whether a function calls another
   * function in tail position (i.e., whether the continuation is passed to
   * another function).  If so, the function needs to be wrapped to use
   * a trampoline.  This function determines whether the given continuation
   * is passed to another function.
   *)
  val escapes: cont_map * CSyntax.CVar -> bool

  (*
   * The JS-CPS backend must decide whether to compile a continuation as
   * a label/goto or as a function.  Continuations used for function calls,
   * as well as those invoked from such continuations, must be compiled as
   * functions.  This function determines whether the given continuation needs
   * to be compiled as a function.
   *)
  val escapesTransitively: cont_map * CSyntax.CVar -> bool

  val contEscape: CSyntax.CVar * CSyntax.Stat -> cont_map
end =
struct
  local structure C = CSyntax
  in
    (* Public interface to access the result *)
    type cont_map =
      {escapes: bool, escapesTransitively: bool} CSyntax.CVarTable.hash_table
    fun escapes (t: cont_map, v) =
      #escapes (C.CVarTable.lookup t v)
    fun escapesTransitively (t: cont_map, v) =
      #escapesTransitively (C.CVarTable.lookup t v)

    (* The implementation *)
    type table =
      { escapes: bool ref
      , escapesTransitively: bool ref
      , free: CSyntax.CVarSet.set
      } CSyntax.CVarTable.hash_table
    fun direct (_: table, k, acc) = C.CVarSet.add (acc, k)
    fun recEscape (table: table) k =
      let
        val {escapes = _, escapesTransitively, free} =
          C.CVarTable.lookup table k
      in
        if !escapesTransitively then ()
        else (escapesTransitively := true; C.CVarSet.app (recEscape table) free)
      end
    fun escape (table: table, k, acc) =
      let
        val {escapes, escapesTransitively = _, free = _} =
          C.CVarTable.lookup table k
      in
        escapes := true;
        C.CVarSet.add (acc, k)
      end
    fun goDec table (dec, acc) =
      case dec of
        C.ValDec
          { exp =
              C.Abs
                { contParam
                , tyParams = _
                , params = _
                , body
                , resultTy = _
                , attr = _
                }
          , results = _
          } =>
          ( C.CVarTable.insert table
              ( contParam
              , { escapes = ref false
                , escapesTransitively = ref false
                , free = C.CVarSet.empty
                }
              )
          ; ignore (go (table, body, C.CVarSet.empty))
          ; acc
          )
      | C.ValDec {exp = _, results = _} => acc
      | C.RecDec defs =>
          ( List.app
              (fn {contParam, body, ...} =>
                 ( C.CVarTable.insert table
                     ( contParam
                     , { escapes = ref false
                       , escapesTransitively = ref false
                       , free = C.CVarSet.empty
                       }
                     )
                 ; ignore (go (table, body, C.CVarSet.empty))
                 )) defs
          ; acc
          )
      | C.UnpackDec _ => acc
      | C.ContDec {name, params = _, body, attr = _} =>
          let
            val free = go (table, body, C.CVarSet.empty)
          in
            C.CVarTable.insert table
              ( name
              , { escapes = ref false
                , escapesTransitively = ref false
                , free = free
                }
              );
            C.CVarSet.union (acc, free)
          end
      | C.RecContDec defs =>
          ( List.app
              (fn (name, _, _) =>
                 C.CVarTable.insert table
                   ( name
                   , { escapes = ref false
                     , escapesTransitively = ref false
                     , free = C.CVarSet.empty
                     }
                   )) defs
          ; List.foldl
              (fn ((name, _, body), acc) =>
                 let
                   val {escapes, escapesTransitively, free = _} =
                     C.CVarTable.lookup table name
                   val free = go (table, body, C.CVarSet.empty)
                 in
                   C.CVarTable.insert table
                     ( name
                     , { escapes = escapes
                       , escapesTransitively = escapesTransitively
                       , free = free
                       }
                     );
                   C.CVarSet.union (acc, free)
                 end) acc defs
          )
      | C.DatatypeDec _ => acc
      | C.ESImportDec _ => acc
    (* Returns free continuations for a statement *)
    and go (table, C.Let {decs, cont}, acc) =
          go (table, cont, List.foldl (goDec table) acc decs)
      | go
          ( table
          , C.App {applied = _, cont, tyArgs = _, args = _, attr = _}
          , acc
          ) = escape (table, cont, acc)
      | go (table, C.AppCont {applied, args = _}, acc) =
          direct (table, applied, acc)
      | go (table, C.If {cond = _, thenCont, elseCont}, acc) =
          go (table, elseCont, go (table, thenCont, acc))
      | go
          ( table
          , C.Handle
              { body
              , handler = (_, h)
              , successfulExitIn
              , successfulExitOut
              , resultTy = _
              }
          , acc
          ) =
          let
            val free = go (table, h, C.CVarSet.empty)
          in
            C.CVarTable.insert table
              ( successfulExitIn
              , { escapes = ref false
                , escapesTransitively = ref false
                , free = C.CVarSet.singleton successfulExitOut
                }
              );
            C.CVarSet.app (fn k => ignore (escape (table, k, C.CVarSet.empty)))
              free;
            ignore (go (table, body, C.CVarSet.empty));
            C.CVarSet.union (acc, free)
          end
      | go (_, C.Raise _, acc) = acc
      | go (_, C.Unreachable, acc) = acc
    fun contEscape (cont, stat) =
      let
        val table =
          C.CVarTable.mkTable (1, C.InvalidCode "unbound continuation")
      in
        C.CVarTable.insert table
          ( cont
          , { escapes = ref false
            , escapesTransitively = ref false
            , free = C.CVarSet.empty
            }
          );
        ignore (go (table, stat, C.CVarSet.empty));
        C.CVarTable.appi
          (fn (k, {escapes = ref true, escapesTransitively = ref false, ...}) =>
             recEscape table k
            | _ => ()) table;
        C.CVarTable.map
          (fn {escapes, escapesTransitively, ...} =>
             {escapes = !escapes, escapesTransitively = !escapesTransitively})
          table
      end
  end (* local *)
end; (* structure CpsAnalyze *)
