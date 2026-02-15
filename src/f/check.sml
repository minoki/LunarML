(*
 * Copyright (c) 2025 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure CheckF:
sig
  exception TypeError of string
  type ValEnv = FSyntax.Ty TypedSyntax.VIdMap.map
  val emptyValEnv: ValEnv
  type Env =
    { valEnv: ValEnv
    , tyVarEnv: FSyntax.Kind TypedSyntax.TyVarMap.map
    , aliasEnv: FSyntax.Ty TypedSyntax.TyVarMap.map
    , valConEnv:
        ({tyParams: FSyntax.TyVar list, payload: FSyntax.Ty option} StringMap.map) TypedSyntax.TyVarMap.map
    }
  val checkKind: FSyntax.Kind TypedSyntax.TyVarMap.map * FSyntax.Kind
                 -> FSyntax.Ty
                 -> unit
  val sameType: FSyntax.Ty * FSyntax.Ty -> bool
  val checkSame:
    FSyntax.Ty TypedSyntax.TyVarMap.map * (unit -> string) * FSyntax.Ty
    -> FSyntax.Ty
    -> unit
  structure TypeOfPrimitives:
  sig
    val typeOf:
      Primitives.PrimOp
      -> { vars: (FSyntax.TyVar * unit) list
         , args: FSyntax.Ty vector
         , results: FSyntax.Ty list
         }
  end
  val inferExp: Env * FSyntax.Exp -> FSyntax.Ty
  val checkExp: Env * (* expectedTy *) FSyntax.Ty * FSyntax.Exp -> unit
end =
struct
  exception TypeError of string
  local structure F = FSyntax
  in
    (*:
    val kindOf : F.Kind TypedSyntax.TyVarMap.map -> F.Ty -> F.Kind
    val checkKind : F.Kind TypedSyntax.TyVarMap.map * F.Kind -> F.Ty -> unit
     *)
    fun kindOf (env: F.Kind TypedSyntax.TyVarMap.map) (F.TyVar tv) =
          (case TypedSyntax.TyVarMap.find (env, tv) of
             SOME kind => kind
           | NONE =>
               raise TypeError
                 ("undefined type variable " ^ TypedSyntax.print_TyVar tv))
      | kindOf env (F.RecordType fields) =
          (Syntax.LabelMap.app (checkKind (env, F.TypeKind)) fields; F.TypeKind)
      | kindOf env (F.AppType {applied, arg}) =
          (case kindOf env applied of
             F.ArrowKind (paramKind, resultKind) =>
               (checkKind (env, paramKind) arg; resultKind)
           | F.TypeKind =>
               raise TypeError
                 (Printer.build (FPrinter.doTy 0 applied)
                  ^ ": expected kind _ -> _, but has Type"))
      | kindOf env (F.MultiFnType (params, result)) =
          ( List.app (checkKind (env, F.TypeKind)) params
          ; checkKind (env, F.TypeKind) result
          ; F.TypeKind
          )
      | kindOf env (F.ForallType (tv, kind, ty)) =
          ( checkKind (TypedSyntax.TyVarMap.insert (env, tv, kind), F.TypeKind)
              ty
          ; F.TypeKind
          )
      | kindOf env (F.ExistsType (tv, kind, ty)) =
          ( checkKind (TypedSyntax.TyVarMap.insert (env, tv, kind), F.TypeKind)
              ty
          ; F.TypeKind
          )
      | kindOf env (F.TypeFn (tv, kind, ty)) =
          F.ArrowKind
            (kind, kindOf (TypedSyntax.TyVarMap.insert (env, tv, kind)) ty)
      | kindOf _ (F.AnyType kind) = kind
      | kindOf _ F.BoxedType = F.TypeKind
      | kindOf env (ty as F.DelayedSubst _) =
          kindOf env (F.forceTy ty)
    and checkKind (env, expectedKind) ty =
      let
        val kind = kindOf env ty
      in
        if kind = expectedKind then
          ()
        else
          raise TypeError
            (Printer.build (FPrinter.doTy 0 ty) ^ ": expected kind "
             ^ Printer.build (FPrinter.doKind 0 expectedKind) ^ ", but has "
             ^ Printer.build (FPrinter.doKind 0 kind))
      end

    (* the argument must be well-kinded *)
    fun normalizeType (env: F.Ty TypedSyntax.TyVarMap.map) (ty as F.TyVar tv) =
          (case TypedSyntax.TyVarMap.find (env, tv) of
             SOME ty' => normalizeType env ty'
           | NONE => ty)
      | normalizeType env (F.RecordType fields) =
          F.RecordType (Syntax.LabelMap.map (normalizeType env) fields)
      | normalizeType env (F.AppType {applied, arg}) =
          let
            val applied = normalizeType env applied
            val arg = normalizeType env arg
          in
            case applied of
              F.TypeFn (tv, kind, body) =>
                (* TODO: checkKind (env, kind) applied *)
                if
                  case arg of
                    F.TyVar tv' => tv = tv'
                  | _ => false
                then
                  normalizeType env body
                else if
                  F.occurCheck tv arg
                then
                  raise TypeError
                    ("occur check failed: " ^ TypedSyntax.print_TyVar tv
                     ^ " in " ^ Printer.build (FPrinter.doTy 0 arg))
                else
                  normalizeType (TypedSyntax.TyVarMap.insert (env, tv, arg))
                    body
            | F.AnyType (F.ArrowKind (_, kind)) => F.AnyType kind
            | _ => F.AppType {applied = applied, arg = arg}
          end
      | normalizeType env (F.MultiFnType (params, result)) =
          F.MultiFnType
            (List.map (normalizeType env) params, normalizeType env result)
      | normalizeType env (F.ForallType (tv, kind, ty)) =
          F.ForallType (tv, kind, normalizeType env ty)
      | normalizeType env (F.ExistsType (tv, kind, ty)) =
          F.ExistsType (tv, kind, normalizeType env ty)
      | normalizeType env (F.TypeFn (tv, kind, ty)) =
          F.TypeFn (tv, kind, normalizeType env ty)
      | normalizeType _ (ty as F.AnyType _) = ty
      | normalizeType _ (ty as F.BoxedType) = ty
      | normalizeType env (ty as F.DelayedSubst _) =
          normalizeType env (F.forceTy ty)

    (*: val sameType' : int * int TypedSyntax.TyVarMap.map * int TypedSyntax.TyVarMap.map -> F.Ty * F.Ty -> bool *)
    fun sameType' env (ty, ty') =
      sameType'' env (F.weakNormalizeTy ty, F.weakNormalizeTy ty')
    and sameType'' (_: int, tvmap, tvmap') (F.TyVar tv, F.TyVar tv') =
          (case
             ( TypedSyntax.TyVarMap.find (tvmap, tv)
             , TypedSyntax.TyVarMap.find (tvmap', tv')
             )
           of
             (SOME x, SOME x') => x = x'
           | (NONE, NONE) => TypedSyntax.eqTyVar (tv, tv')
           | _ => false)
      | sameType'' env (F.RecordType fields, F.RecordType fields') =
          Syntax.LabelMap.numItems fields = Syntax.LabelMap.numItems fields'
          andalso
          Syntax.LabelMap.alli
            (fn (label, ty) =>
               case Syntax.LabelMap.find (fields', label) of
                 SOME ty' => sameType' env (ty, ty')
               | NONE => false) fields
      | sameType'' env
          (F.AppType {applied, arg}, F.AppType {applied = applied', arg = arg'}) =
          sameType' env (applied, applied') andalso sameType' env (arg, arg')
      | sameType'' env
          (F.MultiFnType (params, result), F.MultiFnType (params', result')) =
          ListPair.allEq (sameType' env) (params, params')
          andalso sameType' env (result, result')
      | sameType'' (i, tvmap, tvmap')
          (F.ForallType (tv, kind, ty), F.ForallType (tv', kind', ty')) =
          kind = kind'
          andalso
          let
            val env' =
              ( i + 1
              , TypedSyntax.TyVarMap.insert (tvmap, tv, i)
              , TypedSyntax.TyVarMap.insert (tvmap', tv', i)
              )
          in
            sameType' env' (ty, ty')
          end
      | sameType'' (i, tvmap, tvmap')
          (F.ExistsType (tv, kind, ty), F.ExistsType (tv', kind', ty')) =
          kind = kind'
          andalso
          let
            val env' =
              ( i + 1
              , TypedSyntax.TyVarMap.insert (tvmap, tv, i)
              , TypedSyntax.TyVarMap.insert (tvmap', tv', i)
              )
          in
            sameType' env' (ty, ty')
          end
      | sameType'' (i, tvmap, tvmap')
          (F.TypeFn (tv, kind, ty), F.TypeFn (tv', kind', ty')) =
          kind = kind'
          andalso
          let
            val env' =
              ( i + 1
              , TypedSyntax.TyVarMap.insert (tvmap, tv, i)
              , TypedSyntax.TyVarMap.insert (tvmap', tv', i)
              )
          in
            sameType' env' (ty, ty')
          end
      | sameType'' _ (F.AnyType _, _) = true
      | sameType'' _ (_, F.AnyType _) = true
      | sameType'' _ (F.BoxedType, F.BoxedType) = true
      | sameType'' _ (F.BoxedType, ty) =
          not (F.isUnboxedTy ty)
      | sameType'' _ (ty, F.BoxedType) =
          not (F.isUnboxedTy ty)
      | sameType'' _ _ = false
    (*: val sameType : F.Ty * F.Ty -> bool *)
    val sameType =
      sameType' (0, TypedSyntax.TyVarMap.empty, TypedSyntax.TyVarMap.empty)
    (*: val checkSame : F.Ty TypedSyntax.TyVarMap.map * (unit -> string) * F.Ty -> F.Ty -> unit *)
    fun checkSame (env, comment, expectedTy) actualTy =
      let
        val substTy = #doTy (F.substTy env)
      in
        if sameType (substTy expectedTy, substTy actualTy) then
          ()
        else
          raise TypeError
            ("type mismatch: expected "
             ^ Printer.build (FPrinter.doTy 0 expectedTy) ^ ", but got "
             ^ Printer.build (FPrinter.doTy 0 actualTy) ^ " (" ^ comment ()
             ^ ")")
      end

    type ValEnv = (F.Ty (* * Syntax.IdStatus *)) TypedSyntax.VIdMap.map
    val emptyValEnv: ValEnv = TypedSyntax.VIdMap.empty
    type Env =
      { valEnv: ValEnv
      , tyVarEnv: F.Kind TypedSyntax.TyVarMap.map
      , aliasEnv: F.Ty TypedSyntax.TyVarMap.map
      , valConEnv:
          ({tyParams: F.TyVar list, payload: F.Ty option} StringMap.map) TypedSyntax.TyVarMap.map
      }
    fun modifyValEnv (f, {valEnv, tyVarEnv, aliasEnv, valConEnv}: Env) : Env =
      { valEnv = f valEnv
      , tyVarEnv = tyVarEnv
      , aliasEnv = aliasEnv
      , valConEnv = valConEnv
      }
    structure TypeOfPrimitives =
      TypeOfPrimitives
        (type ty = F.Ty
         type tv = F.TyVar
         type constraint = unit
         val tyVarA = TypedSyntax.MkTyVar ("'a", 0)
         val tyVarB = TypedSyntax.MkTyVar ("'b", 1)
         val tyVarC = TypedSyntax.MkTyVar ("'c", 2)
         val tyVarD = TypedSyntax.MkTyVar ("'d", 3)
         val tyVarEqA = TypedSyntax.MkTyVar ("''a", 0)
         val tyA = F.TyVar tyVarA
         val tyB = F.TyVar tyVarB
         val tyC = F.TyVar tyVarC
         val tyD = F.TyVar tyVarD
         val tyEqA = F.TyVar tyVarEqA
         val unit = F.RecordType Syntax.LabelMap.empty
         val bool = F.TyVar PrimTypes.Names.bool
         val int = F.TyVar PrimTypes.Names.int
         val word = F.TyVar PrimTypes.Names.word
         val real = F.TyVar PrimTypes.Names.real
         val char = F.TyVar PrimTypes.Names.char
         val char7 = F.TyVar PrimTypes.Names.char7
         val char16 = F.TyVar PrimTypes.Names.char16
         val char32 = F.TyVar PrimTypes.Names.char32
         val uchar = F.TyVar PrimTypes.Names.uchar
         val string = F.TyVar PrimTypes.Names.string
         val string7 = F.TyVar PrimTypes.Names.string7
         val string16 = F.TyVar PrimTypes.Names.string16
         val string32 = F.TyVar PrimTypes.Names.string32
         val intInf = F.TyVar PrimTypes.Names.intInf
         val int32 = F.TyVar PrimTypes.Names.int32
         val int54 = F.TyVar PrimTypes.Names.int54
         val int64 = F.TyVar PrimTypes.Names.int64
         val word32 = F.TyVar PrimTypes.Names.word32
         val word64 = F.TyVar PrimTypes.Names.word64
         val exn = F.TyVar PrimTypes.Names.exn
         val exntag = F.TyVar PrimTypes.Names.exntag
         val LuaValue = F.TyVar PrimTypes.Names.lua_value
         val JavaScriptValue = F.TyVar PrimTypes.Names.js_value
         val prim_effect = F.TyVar PrimTypes.Names.prim_effect
         fun refOf ty =
           F.AppType {applied = F.TyVar PrimTypes.Names.ref_, arg = ty}
         fun listOf ty =
           F.AppType {applied = F.TyVar PrimTypes.Names.list, arg = ty}
         fun vectorOf ty =
           F.AppType {applied = F.TyVar PrimTypes.Names.vector, arg = ty}
         fun arrayOf ty =
           F.AppType {applied = F.TyVar PrimTypes.Names.array, arg = ty}
         val pairOf = F.PairType
         val tupleOf = F.TupleType
         fun function1Of (a, result) = F.FnType (a, result)
         fun function2Of (a, b, result) =
           F.MultiFnType ([a, b], result)
         fun function3Of (a, b, c, result) =
           F.MultiFnType ([a, b, c], result)
         fun promptTagOf ty =
           F.AppType {applied = F.TyVar PrimTypes.Names.prompt_tag, arg = ty}
         fun subcontOf (a, b) =
           F.AppType
             { applied =
                 F.AppType {applied = F.TyVar PrimTypes.Names.subcont, arg = a}
             , arg = b
             }
         val Unconstrained = ()
         val IsEqType = ()) :
        sig
          val typeOf:
            Primitives.PrimOp
            -> { vars: (F.TyVar * unit) list
               , args: F.Ty vector
               , results: F.Ty list
               }
        end
    (*:
    val checkPat : Env * (* expectedTy *) F.Ty * F.Pat -> ValEnv (* expectedTy must be normalized *)
    val inferExp : Env * F.Exp -> F.Ty
    val checkExp : Env * (* expectedTy *) F.Ty * F.Exp -> unit
    val inferDecs : Env * F.Dec list -> Env
    val inferDec : F.Dec * Env -> Env
     *)
    fun checkPat (_: Env, _: F.Ty, F.WildcardPat _) = emptyValEnv
      | checkPat
          ( env
          , expectedTy
          , F.SConPat {sourceSpan = _, scon = _, equality, cookedValue}
          ) =
          ( checkExp (env, F.EqualityType expectedTy, equality)
          ; checkExp (env, expectedTy, cookedValue)
          ; emptyValEnv
          )
      | checkPat (env, expectedTy, F.VarPat (_, vid, ty)) =
          ( checkKind (#tyVarEnv env, F.TypeKind) ty
          ; checkSame (#aliasEnv env, fn () => "VarPat", expectedTy) ty
          ; TypedSyntax.VIdMap.singleton (vid, ty)
          )
      | checkPat
          ( env
          , F.RecordType fieldTypes
          , F.RecordPat
              { sourceSpan = _
              , fields = fieldPats
              , ellipsis = NONE
              , allFields = _
              }
          ) =
          if Syntax.LabelMap.numItems fieldTypes = List.length fieldPats then
            List.foldl
              (fn ((label, pat), map) =>
                 case Syntax.LabelMap.find (fieldTypes, label) of
                   SOME ty =>
                     TypedSyntax.VIdMap.unionWith
                       (fn _ => raise TypeError "RecordPat: duplicate binding")
                       (map, checkPat (env, ty, pat))
                 | NONE => raise TypeError "RecordPat: field mismatch")
              TypedSyntax.VIdMap.empty fieldPats
          else
            raise TypeError "RecordPat: number of fields"
      | checkPat
          ( env
          , F.RecordType fieldTypes
          , F.RecordPat
              { sourceSpan = _
              , fields = fieldPats
              , ellipsis = SOME restPat
              , allFields = _
              }
          ) =
          let
            val restEnv = checkPat
              ( env
              , F.RecordType
                  (List.foldl
                     (fn ((label, _), fields) =>
                        #1 (Syntax.LabelMap.remove (fields, label))) fieldTypes
                     fieldPats)
              , restPat
              )
          in
            List.foldl
              (fn ((label, pat), map) =>
                 case Syntax.LabelMap.find (fieldTypes, label) of
                   SOME ty =>
                     TypedSyntax.VIdMap.unionWith
                       (fn _ => raise TypeError "RecordPat: duplicate binding")
                       (map, checkPat (env, ty, pat))
                 | NONE => raise TypeError "RecordPat: field mismatch") restEnv
              fieldPats
          end
      | checkPat (env, _, F.RecordPat _) =
          raise TypeError "RecordPat: non-record type"
      | checkPat
          (env, expectedTy, F.ValConPat {sourceSpan = _, info = _, payload}) =
          (* TODO: check if the constructor has a payload *)
          (case payload of
             SOME (payloadTy, payloadPat) =>
               checkPat (env, payloadTy, payloadPat)
           | NONE => emptyValEnv)
      | checkPat
          (env, expectedTy, F.ExnConPat {sourceSpan = _, predicate, payload}) =
          ( checkSame (#aliasEnv env, fn () => "ExnConPat", expectedTy)
              (F.Types.exn)
          ; checkExp (env, F.FnType (F.Types.exn, F.Types.bool), predicate)
          (* TODO: check if the constructor has a payload *)
          ; case payload of
              SOME (payloadTy, getPayloadExp, payloadPat) =>
                ( checkExp
                    (env, F.FnType (F.Types.exn, payloadTy), getPayloadExp)
                ; checkPat (env, payloadTy, payloadPat)
                )
            | NONE => emptyValEnv
          )
      | checkPat (env, expectedTy, F.LayeredPat (_, vid, ty, pat)) =
          let
            val ty = normalizeType (#aliasEnv env) ty
            val () =
              checkSame (#aliasEnv env, fn () => "LayeredPat", expectedTy) ty
            val newEnv = checkPat (env, expectedTy, pat)
          in
            TypedSyntax.VIdMap.insert (newEnv, vid, ty)
          end
      | checkPat (env, expectedTy, F.VectorPat (_, pats, _, elemTy)) =
          let
            val elemTy = normalizeType (#aliasEnv env) elemTy
            val () =
              checkSame (#aliasEnv env, fn () => "VectorPat", expectedTy)
                (F.Types.vector elemTy)
          in
            Vector.foldl
              (fn (pat, newEnv) =>
                 TypedSyntax.VIdMap.unionWith
                   (fn _ => raise TypeError "VectorPat: duplicate binding")
                   (newEnv, checkPat (env, elemTy, pat)))
              TypedSyntax.VIdMap.empty pats
          end
      | checkPat (env, expectedTy, F.BogusPat (_, ty, pats)) =
          let
            val () =
              checkSame (#aliasEnv env, fn () => "BogusPat", expectedTy) ty
          in
            List.foldl
              (fn ((ty, pat), newEnv) =>
                 TypedSyntax.VIdMap.unionWith
                   (fn _ => raise TypeError "BogusPat: duplicate binding")
                   (newEnv, checkPat (env, ty, pat))) TypedSyntax.VIdMap.empty
              pats
          end
    and inferExp (env: Env, F.PrimExp (F.PrimCall primOp, tyargs, valargs)) :
      F.Ty =
          let
            val {vars, args, results} = TypeOfPrimitives.typeOf primOp
            val subst =
              ListPair.foldlEq
                (fn ((tv, ()), ty, acc) =>
                   TypedSyntax.TyVarMap.insert (acc, tv, ty))
                TypedSyntax.TyVarMap.empty (vars, tyargs)
            val args = Vector.map (#doTy (F.substTy subst)) args
            val results = List.map (#doTy (F.substTy subst)) results
            val resultTy =
              case results of
                [ty] => ty
              | _ => F.TupleType results
          in
            ListPair.appEq
              (fn (expectedTy, exp) => checkExp (env, expectedTy, exp))
              (Vector.foldr (op::) [] args, valargs);
            resultTy
          end
      | inferExp (_, F.PrimExp (F.IntConstOp _, [ty], [])) = ty
      | inferExp (_, F.PrimExp (F.WordConstOp _, [ty], [])) = ty
      | inferExp (_, F.PrimExp (F.RealConstOp _, [ty], [])) = ty
      | inferExp (_, F.PrimExp (F.Char8ConstOp _, [ty], [])) = ty
      | inferExp (_, F.PrimExp (F.Char16ConstOp _, [ty], [])) = ty
      | inferExp (_, F.PrimExp (F.Char32ConstOp _, [ty], [])) = ty
      | inferExp (_, F.PrimExp (F.UCharConstOp _, [ty], [])) = ty
      | inferExp (_, F.PrimExp (F.String8ConstOp _, [ty], [])) = ty
      | inferExp (_, F.PrimExp (F.String16ConstOp _, [ty], [])) = ty
      | inferExp (_, F.PrimExp (F.String32ConstOp _, [ty], [])) = ty
      | inferExp (env, F.PrimExp (F.RaiseOp _, [ty], [e])) =
          (checkExp (env, F.Types.exn, e); ty)
      | inferExp (env, F.PrimExp (F.ListOp, [elemTy], elements)) =
          ( List.app (fn elem => checkExp (env, elemTy, elem)) elements
          ; F.Types.list elemTy
          )
      | inferExp (env, F.PrimExp (F.VectorOp, [elemTy], elements)) =
          ( List.app (fn elem => checkExp (env, elemTy, elem)) elements
          ; F.Types.vector elemTy
          )
      | inferExp
          ( env
          , F.PrimExp (F.DataTagAsStringOp _, [dataTy], [data])
          ) (* TODO: Check constructor info *) =
          (checkExp (env, dataTy, data); F.Types.string)
      | inferExp
          ( env
          , F.PrimExp (F.DataTagAsString16Op _, [dataTy], [data])
          ) (* TODO: Check constructor info *) =
          (checkExp (env, dataTy, data); F.Types.string16)
      | inferExp
          ( env
          , F.PrimExp (F.DataPayloadOp _, [dataTy, payloadTy], [data])
          ) (* TODO: Check constructor info *) =
          (checkExp (env, dataTy, data); payloadTy)
      | inferExp
          ( env
          , F.PrimExp (F.ExnPayloadOp, [payloadTy], [data])
          ) (* TODO: Check constructor info *) =
          (checkExp (env, F.Types.exn, data); payloadTy)
      | inferExp
          ( env
          , F.PrimExp (F.ConstructValOp _, [dataTy], [])
          ) (* TODO: Check constructor info *) = dataTy
      | inferExp
          ( env
          , F.PrimExp
              (F.ConstructValWithPayloadOp _, [dataTy, payloadTy], [payload])
          ) =
          (checkExp (env, payloadTy, payload); dataTy)
      | inferExp (env, F.PrimExp (F.ConstructExnOp, [], [tag])) =
          (checkExp (env, F.Types.exntag, tag); F.Types.exn)
      | inferExp
          ( env
          , F.PrimExp (F.ConstructExnWithPayloadOp, [payloadTy], [tag, payload])
          ) =
          ( checkExp (env, F.Types.exntag, tag)
          ; checkExp (env, payloadTy, payload)
          ; F.Types.exn
          )
      | inferExp (env, F.PrimExp (F.JsCallOp, [], e :: f :: args)) =
          ( checkExp (env, F.Types.prim_effect, e)
          ; checkExp (env, F.Types.js_value, f)
          ; List.app (fn a => checkExp (env, F.Types.js_value, a)) args
          ; F.Types.js_value
          )
      | inferExp (env, F.PrimExp (F.JsMethodOp, [], e :: f :: name :: args)) =
          ( checkExp (env, F.Types.prim_effect, e)
          ; checkExp (env, F.Types.js_value, f)
          ; checkExp (env, F.Types.js_value, name)
          ; List.app (fn a => checkExp (env, F.Types.js_value, a)) args
          ; F.Types.js_value
          )
      | inferExp (env, F.PrimExp (F.JsNewOp, [], e :: f :: args)) =
          ( checkExp (env, F.Types.prim_effect, e)
          ; checkExp (env, F.Types.js_value, f)
          ; List.app (fn a => checkExp (env, F.Types.js_value, a)) args
          ; F.Types.js_value
          )
      | inferExp (env, F.PrimExp (F.LuaCallOp, [], e :: f :: args)) =
          ( checkExp (env, F.Types.prim_effect, e)
          ; checkExp (env, F.Types.lua_value, f)
          ; List.app (fn a => checkExp (env, F.Types.lua_value, a)) args
          ; F.Types.vector F.Types.lua_value
          )
      | inferExp (env, F.PrimExp (F.LuaCall1Op, [], e :: f :: args)) =
          ( checkExp (env, F.Types.prim_effect, e)
          ; checkExp (env, F.Types.lua_value, f)
          ; List.app (fn a => checkExp (env, F.Types.lua_value, a)) args
          ; F.Types.lua_value
          )
      | inferExp (env, F.PrimExp (F.LuaCallNOp n, [], e :: f :: args)) =
          let
            val valueTy = F.Types.lua_value
            fun loop (0, acc) = F.RecordType acc
              | loop (i, acc) =
                  loop
                    ( i - 1
                    , Syntax.LabelMap.insert
                        (acc, Syntax.NumericLabel i, valueTy)
                    )
          in
            checkExp (env, F.Types.prim_effect, e);
            checkExp (env, F.Types.lua_value, f);
            List.app (fn a => checkExp (env, F.Types.lua_value, a)) args;
            loop (n, Syntax.LabelMap.empty)
          end
      | inferExp (env, F.PrimExp (F.LuaMethodOp _, [], e :: obj :: args)) =
          ( checkExp (env, F.Types.prim_effect, e)
          ; checkExp (env, F.Types.lua_value, obj)
          ; List.app (fn a => checkExp (env, F.Types.lua_value, a)) args
          ; F.Types.vector F.Types.lua_value
          )
      | inferExp (env, F.PrimExp (F.LuaMethod1Op _, [], e :: obj :: args)) =
          ( checkExp (env, F.Types.prim_effect, e)
          ; checkExp (env, F.Types.lua_value, obj)
          ; List.app (fn a => checkExp (env, F.Types.lua_value, a)) args
          ; F.Types.lua_value
          )
      | inferExp (env, F.PrimExp (F.LuaMethodNOp (_, n), [], e :: obj :: args)) =
          let
            val valueTy = F.Types.lua_value
            fun loop (0, acc) = F.RecordType acc
              | loop (i, acc) =
                  loop
                    ( i - 1
                    , Syntax.LabelMap.insert
                        (acc, Syntax.NumericLabel i, valueTy)
                    )
          in
            checkExp (env, F.Types.prim_effect, e);
            checkExp (env, F.Types.lua_value, obj);
            List.app (fn a => checkExp (env, F.Types.lua_value, a)) args;
            loop (n, Syntax.LabelMap.empty)
          end
      | inferExp (env, F.PrimExp (F.BoxOp fsp, [], [e])) =
          (checkExp (env, F.unboxedTyToTy fsp, e); F.BoxedType)
      | inferExp (env, F.PrimExp (F.UnboxOp fsp, [], [e])) =
          (checkExp (env, F.BoxedType, e); F.unboxedTyToTy fsp)
      | inferExp (_, F.PrimExp (p, _, _)) =
          raise TypeError
            ("PrimOp with invalid arguments: "
             ^ Printer.build (FPrinter.doPrimOp p))
      | inferExp (env, F.VarExp vid) =
          (case TypedSyntax.VIdMap.find (#valEnv env, vid) of
             NONE =>
               raise TypeError ("Unbound variable " ^ TypedSyntax.print_VId vid)
           | SOME ty => ty)
      | inferExp (env, F.RecordExp fields) =
          F.RecordType
            (List.foldl
               (fn ((label, exp), m) =>
                  let
                    val ty = inferExp (env, exp)
                  in
                    Syntax.LabelMap.insertWith
                      (fn _ => raise TypeError "duplicate label") (m, label, ty)
                  end) Syntax.LabelMap.empty fields)
      | inferExp (env, F.LetExp (decs, exp)) =
          let val env' = inferDecs (env, decs)
          in inferExp (env', exp)
          end
      | inferExp (env, F.MultiAppExp (f, args)) =
          let
            val fTy = inferExp (env, f)
          in
            case fTy of
              F.MultiFnType (params, resultTy) =>
                ( ListPair.appEq (fn (ty, arg) => checkExp (env, ty, arg))
                    (params, args)
                  handle ListPair.UnequalLengths =>
                    raise TypeError
                      "invalid function application (arity mismatch)"
                ; resultTy
                )
            | _ => raise TypeError "invalid function application"
          end
      | inferExp (env, F.HandleExp {body, exnName, handler, resultTy}) =
          let
            val handlerEnv = modifyValEnv
              (fn m => TypedSyntax.VIdMap.insert (m, exnName, F.Types.exn), env)
          in
            checkExp (env, resultTy, body);
            checkExp (handlerEnv, resultTy, handler);
            resultTy
          end
      | inferExp (env, F.IfThenElseExp (cond, then', else')) =
          let
            val () = checkExp (env, F.Types.bool, cond)
            val ty = inferExp (env, then')
          in
            checkExp (env, ty, else');
            ty
          end
      | inferExp
          ( env
          , F.CaseExp
              { sourceSpan = _
              , subjectExp
              , subjectTy
              , matches
              , matchType = _
              , resultTy
              }
          ) =
          let
            val () = checkExp (env, subjectTy, subjectExp)
            fun checkMatch (pat, exp) =
              let
                val ve = checkPat (env, subjectTy, pat)
                val env' = modifyValEnv
                  (fn m => TypedSyntax.VIdMap.unionWithSecond (m, ve), env)
              in
                checkExp (env', resultTy, exp)
              end
          in
            List.app checkMatch matches;
            resultTy
          end
      | inferExp (env, F.MultiFnExp (_, params, exp)) =
          let
            val env' = modifyValEnv
              (fn m => List.foldl TypedSyntax.VIdMap.insert' m params, env)
          in
            F.MultiFnType (List.map #2 params, inferExp (env', exp))
          end
      | inferExp (env, F.ProjectionExp {label, record, fieldTypes}) =
          let
            val recordTy = F.RecordType fieldTypes
          in
            checkExp (env, recordTy, record);
            case Syntax.LabelMap.find (fieldTypes, label) of
              SOME ty => ty
            | NONE => raise TypeError "invalid projection"
          end
      | inferExp (env, F.TyAbsExp (tv, k, exp)) =
          let
            val env' =
              { valEnv = #valEnv env
              , tyVarEnv = TypedSyntax.TyVarMap.insert (#tyVarEnv env, tv, k)
              , aliasEnv = #aliasEnv env
              , valConEnv = #valConEnv env
              }
          in
            F.ForallType (tv, k, inferExp (env', exp))
          end
      | inferExp (env, F.TyAppExp (exp, ty)) =
          (case normalizeType (#aliasEnv env) (inferExp (env, exp)) of
             F.ForallType (tv, k, ty') =>
               ( checkKind (#tyVarEnv env, k) ty
               ; #doTy (F.substTy (TypedSyntax.TyVarMap.singleton (tv, ty))) ty'
               )
           | actualTy =>
               raise TypeError
                 ("invalid type application: actual type was "
                  ^ Printer.build (FPrinter.doTy 0 actualTy)))
      | inferExp (env, F.PackExp {payloadTy, exp, packageTy}) =
          (case packageTy of
             F.ExistsType (tv, k, ty) =>
               let
                 val () =
                   if F.occurCheck tv payloadTy then
                     raise TypeError
                       ("occur check failed: " ^ TypedSyntax.print_TyVar tv
                        ^ " in " ^ Printer.build (FPrinter.doTy 0 payloadTy)
                        ^ " / " ^ Printer.build (FPrinter.doExp 0 exp))
                   else
                     ()
                 val aliasEnv' =
                   TypedSyntax.TyVarMap.insert (#aliasEnv env, tv, payloadTy)
                 val env' =
                   { valEnv = #valEnv env
                   , tyVarEnv = #tyVarEnv env
                   , aliasEnv = aliasEnv'
                   , valConEnv = #valConEnv env
                   }
               in
                 checkKind (#tyVarEnv env, k) payloadTy;
                 checkExp (env', #doTy (F.substTy aliasEnv') ty, exp);
                 packageTy
               end
           | _ => raise TypeError "invalid package")
      | inferExp (_, F.BogusExp ty) = ty
      | inferExp (_, F.ExitProgram) =
          raise TypeError "ExitProgram without expected type"
      | inferExp (env, F.ExportValue exp) =
          ( ignore (inferExp (env, exp))
          ; raise TypeError "ExportValue without expected type"
          )
      | inferExp (env, F.ExportModule bindings) =
          ( Vector.app (fn (_, exp) => ignore (inferExp (env, exp))) bindings
          ; raise TypeError "ExportModule without expected type"
          )
    and checkExp (env, expectedTy, F.RecordExp fields) =
          (case expectedTy of
             F.RecordType fieldTypes =>
               if Syntax.LabelMap.numItems fieldTypes = List.length fields then
                 List.app
                   (fn (label, field) =>
                      case Syntax.LabelMap.find (fieldTypes, label) of
                        SOME fieldTy => checkExp (env, fieldTy, field)
                      | NONE => raise TypeError "field mismatch") fields
               else
                 raise TypeError "field mismatch"
           | _ => raise TypeError "actual: record")
      | checkExp (env, expectedTy, F.LetExp (decs, exp)) =
          let val env' = inferDecs (env, decs)
          in checkExp (env', expectedTy, exp)
          end
      | checkExp (env, expectedTy, F.IfThenElseExp (cond, then', else')) =
          ( checkExp (env, F.Types.bool, cond)
          ; checkExp (env, expectedTy, then')
          ; checkExp (env, expectedTy, else')
          )
      | checkExp
          (env, expectedTy, fnexp as F.MultiFnExp (nameHint, params, exp)) =
          (case normalizeType (#aliasEnv env) expectedTy of
             F.MultiFnType (paramTypes, b) =>
               let
                 val env' = modifyValEnv
                   (fn m => List.foldl TypedSyntax.VIdMap.insert' m params, env)
               in
                 ListPair.appEq
                   (fn (e, (_, a)) =>
                      checkSame
                        ( #aliasEnv env
                        , fn () =>
                            "FnExp "
                            ^
                            Option.getOpt
                              ( case nameHint of
                                  NONE => NONE
                                | SOME h => Syntax.SourceName.getString h
                              , "(no name)"
                              ) ^ ": " ^ Printer.build (FPrinter.doExp 0 fnexp)
                        , e
                        ) a) (paramTypes, params)
                 handle ListPair.UnequalLengths =>
                   raise TypeError "MultiFnExp arity mismatch";
                 checkExp (env', b, exp)
               end
           | _ => raise TypeError "invalid function expression")
      | checkExp (env, expectedTy, exp0 as F.TyAbsExp (tv, kind, exp)) =
          (case normalizeType (#aliasEnv env) expectedTy of
             F.ForallType (tv', k, ty') =>
               let
                 val ty'' =
                   if tv = tv' then
                     ty'
                   else
                     #doTy
                       (F.substTy
                          (TypedSyntax.TyVarMap.singleton (tv', F.TyVar tv)))
                       ty'
                 val env' =
                   { valEnv = #valEnv env
                   , tyVarEnv =
                       TypedSyntax.TyVarMap.insert (#tyVarEnv env, tv, kind)
                   , aliasEnv = #aliasEnv env
                   , valConEnv = #valConEnv env
                   }
               in
                 if k = kind then () else raise TypeError "kind mismatch";
                 checkExp (env', ty'', exp)
               end
           | expectedTy =>
               raise TypeError
                 ("invalid type abstraction: expected type was "
                  ^ Printer.build (FPrinter.doTy 0 expectedTy)
                  ^ ", expression was " ^ Printer.build (FPrinter.doExp 0 exp0)))
      | checkExp (_, _, F.ExitProgram) = ()
      | checkExp (env, _, F.ExportValue exp) =
          ignore (inferExp (env, exp))
      | checkExp (env, _, F.ExportModule bindings) =
          Vector.app (fn (_, exp) => ignore (inferExp (env, exp))) bindings
      | checkExp (env, expectedTy, exp) =
          checkSame
            ( #aliasEnv env
            , fn () => "checkExp: " ^ Printer.build (FPrinter.doExp 0 exp)
            , expectedTy
            ) (inferExp (env, exp))
    and inferDecs (env, decs) =
      List.foldl inferDec env decs
    and inferDec (F.ValDec (vid, NONE, exp), env: Env) : Env =
          let
            val ty = inferExp (env, exp)
          in
            modifyValEnv
              (fn valEnv => TypedSyntax.VIdMap.insert (valEnv, vid, ty), env)
          end
      | inferDec (F.ValDec (vid, SOME ty, exp), env) =
          ( checkExp (env, ty, exp)
          ; modifyValEnv
              (fn valEnv => TypedSyntax.VIdMap.insert (valEnv, vid, ty), env)
          )
      | inferDec (F.RecValDec defs, env) =
          let
            val env' = modifyValEnv
              ( fn valEnv =>
                  List.foldl
                    (fn ((vid, ty, _), acc) =>
                       TypedSyntax.VIdMap.insert (acc, vid, ty)) valEnv defs
              , env
              )
          in
            List.app (fn (_, ty, exp) => checkExp (env', ty, exp)) defs;
            env'
          end
      | inferDec (F.UnpackDec (tv, kind, vid, ty, exp), env) =
          ( checkExp (env, F.ExistsType (tv, kind, ty), exp)
          ; { valEnv = TypedSyntax.VIdMap.insert (#valEnv env, vid, ty)
            , tyVarEnv = TypedSyntax.TyVarMap.insert (#tyVarEnv env, tv, kind)
            , aliasEnv = #aliasEnv env
            , valConEnv = #valConEnv env
            }
          )
      | inferDec (F.IgnoreDec exp, env) =
          let val _ = inferExp (env, exp)
          in env
          end
      | inferDec (F.DatatypeDec datbinds, env) =
          let
            fun doTyName (F.DatBind (tyvars, tyname, _), tyVarEnv) =
              TypedSyntax.TyVarMap.insert
                ( tyVarEnv
                , tyname
                , List.foldl (fn (_, k) => F.ArrowKind (F.TypeKind, k))
                    F.TypeKind tyvars
                )
            fun doDatBind (F.DatBind (tyvars, tyname, conbinds), valConEnv) =
              let
                fun doConBind
                  (F.ConBind (TypedSyntax.MkVId (name, _), optPayloadTy), m) =
                  StringMap.insert
                    ( m
                    , Syntax.SourceName.getStringWithDefault (name, "")
                    , {tyParams = tyvars, payload = optPayloadTy}
                    )
              in
                TypedSyntax.TyVarMap.insert
                  ( valConEnv
                  , tyname
                  , List.foldl doConBind StringMap.empty conbinds
                  )
              end
          in
            { valEnv = #valEnv env
            , tyVarEnv = List.foldl doTyName (#tyVarEnv env) datbinds
            , aliasEnv = #aliasEnv env
            , valConEnv = List.foldl doDatBind (#valConEnv env) datbinds
            }
          end
      | inferDec (F.ExceptionDec {name = _, tagName, payloadTy = _}, env) =
          modifyValEnv
            ( fn valEnv =>
                TypedSyntax.VIdMap.insert (valEnv, tagName, F.Types.exntag)
            , env
            )
      | inferDec (F.ESImportDec {pure = _, specs, moduleName = _}, env) =
          modifyValEnv
            ( fn valEnv =>
                List.foldl
                  (fn ((_, vid, ty), acc) =>
                     TypedSyntax.VIdMap.insert (acc, vid, ty)) valEnv specs
            , env
            )
  end (* local *)
end (* structure CheckF *)
