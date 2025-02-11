(*
 * Copyright (c) 2025 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure TypeCheckF =
struct
  exception TypeError of string
  local structure F = FSyntax
  in
    (*
      fun findField (label, fields) =
        List.find (fn (label', x) => if label = label' then SOME x else NONE)
          fields*)

    (*:
    val kindOf : F.Kind TypedSyntax.TyVarMap.map -> F.Ty -> F.Kind
    val checkKind : F.Kind TypedSyntax.TyVarMap.map * F.Kind -> F.Ty -> unit
     *)
    fun kindOf (env: F.Kind TypedSyntax.TyVarMap.map) (F.TyVar tv) =
          (case TypedSyntax.TyVarMap.find (env, tv) of
             SOME kind => kind
           | NONE => raise TypeError "undefined type variable")
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
      | kindOf env (F.FnType (argTy, resultTy)) =
          ( checkKind (env, F.TypeKind) argTy
          ; checkKind (env, F.TypeKind) resultTy
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
    (*
    | kindOf env (F.SigType {valMap, strMap, exnTags = _}) =
    ( Syntax.VIdMap.app (checkKind (env, F.TypeKind)) valMap
    ; Syntax.VIdMap.app (checkKind (env, F.TypeKind)) strMap
    ; F.TypeKind
    )
    *)
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
                normalizeType (TypedSyntax.TyVarMap.insert (env, tv, arg)) body
            | F.TyVar _ => F.AppType {applied = applied, arg = arg}
            | _ => raise TypeError "invalid kind"
          end
      | normalizeType env (F.FnType (param, result)) =
          F.FnType (normalizeType env param, normalizeType env result)
      | normalizeType env (F.ForallType (tv, kind, ty)) =
          F.ForallType (tv, kind, normalizeType env ty)
      | normalizeType env (F.ExistsType (tv, kind, ty)) =
          F.ExistsType (tv, kind, normalizeType env ty)
      | normalizeType env (F.TypeFn (tv, kind, ty)) =
          F.TypeFn (tv, kind, normalizeType env ty)
    (*
    | normalizeType env (F.SigType {valMap, strMap, exnTags}) =
    F.SigType
    { valMap = Syntax.VIdMap.map (normalizeType env) valMap
    , strMap = Syntax.VIdMap.map (normalizeType env) strMap
    , exnTags = exnTags
    }
    *)

    (*: val sameType' : int * int TypedSyntax.TyVarMap.map * int TypedSyntax.TyVarMap.map -> F.Ty * F.Ty -> bool *)
    fun sameType' (_: int, tvmap, tvmap') (F.TyVar tv, F.TyVar tv') =
          (case
             ( TypedSyntax.TyVarMap.find (tvmap, tv)
             , TypedSyntax.TyVarMap.find (tvmap', tv')
             )
           of
             (SOME x, SOME x') => x = x'
           | (NONE, NONE) => TypedSyntax.eqTyVar (tv, tv')
           | _ => false)
      | sameType' env (F.RecordType fields, F.RecordType fields') =
          Syntax.LabelMap.numItems fields = Syntax.LabelMap.numItems fields'
          andalso
          Syntax.LabelMap.alli
            (fn (label, ty) =>
               case Syntax.LabelMap.find (fields', label) of
                 SOME ty' => sameType' env (ty, ty')
               | NONE => false) fields
      | sameType' env
          (F.AppType {applied, arg}, F.AppType {applied = applied', arg = arg'}) =
          sameType' env (applied, applied') andalso sameType' env (arg, arg')
      | sameType' env
          (F.FnType (paramTy, resultTy), F.FnType (paramTy', resultTy')) =
          sameType' env (paramTy, paramTy')
          andalso sameType' env (resultTy, resultTy')
      | sameType' (i, tvmap, tvmap')
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
      | sameType' (i, tvmap, tvmap')
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
      | sameType' (i, tvmap, tvmap')
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
      (*
      | sameType' env
      ( F.SigType {valMap, strMap, exnTags}
      , F.SigType {valMap = valMap', strMap = strMap', exnTags = exnTags'}
      ) =
      Syntax.VIdMap.numItems valMap = Syntax.VIdMap.numItems valMap'
      andalso
      Syntax.VIdMap.alli
        (fn (vid, ty) =>
           case Syntax.VIdMap.find (valMap', vid) of
             SOME ty' => sameType' env (ty, ty')
           | NONE => false) valMap
      andalso
      Syntax.StrIdMap.numItems strMap = Syntax.StrIdMap.numItems strMap'
      andalso
      Syntax.StrIdMap.alli
        (fn (strid, ty) =>
           case Syntax.StrIdMap.find (strMap', strid) of
             SOME ty' => sameType' env (ty, ty')
           | NONE => false) strMap
      andalso
      Syntax.VIdSet.numItems exnTags = Syntax.VIdSet.numItems exnTags'
      andalso
      Syntax.VIdSet.all (fn vid => Syntax.VIdSet.member (exnTags', vid))
        exnTags
        *)
      | sameType' env _ = false
    (*: val sameType : (F.Ty option) TypedSyntax.TyVarMap.map -> F.Ty * F.Ty -> bool *)
    fun sameType env (ty, ty') =
      sameType' (0, TypedSyntax.TyVarMap.empty, TypedSyntax.TyVarMap.empty)
        (normalizeType env ty, normalizeType env ty')
    (*: val checkType : (F.Ty option) TypedSyntax.TyVarMap.map * F.Ty -> F.Ty -> unit *)
    fun checkType (env, expectedTy) actualTy =
      if sameType env (expectedTy, actualTy) then
        ()
      else
        raise TypeError
          ("type mismatch: expected "
           ^ Printer.build (FPrinter.doTy 0 expectedTy) ^ ", but got "
           ^ Printer.build (FPrinter.doTy 0 actualTy))

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
         val bool = F.TyVar Typing.primTyName_bool
         val int = F.TyVar Typing.primTyName_int
         val word = F.TyVar Typing.primTyName_word
         val real = F.TyVar Typing.primTyName_real
         val char = F.TyVar Typing.primTyName_char
         val char16 = F.TyVar Typing.primTyName_char16
         val string = F.TyVar Typing.primTyName_string
         val string16 = F.TyVar Typing.primTyName_string16
         val intInf = F.TyVar Typing.primTyName_intInf
         val int32 = F.TyVar Typing.primTyName_int32
         val int54 = F.TyVar Typing.primTyName_int54
         val int64 = F.TyVar Typing.primTyName_int64
         val word32 = F.TyVar Typing.primTyName_word32
         val word64 = F.TyVar Typing.primTyName_word64
         val exn = F.TyVar Typing.primTyName_exn
         val exntag = F.TyVar Typing.primTyName_exntag
         val LuaValue = F.TyVar Typing.primTyName_Lua_value
         val JavaScriptValue = F.TyVar Typing.primTyName_JavaScript_value
         fun refOf ty =
           F.AppType {applied = F.TyVar Typing.primTyName_ref, arg = ty}
         fun listOf ty =
           F.AppType {applied = F.TyVar Typing.primTyName_list, arg = ty}
         fun vectorOf ty =
           F.AppType {applied = F.TyVar Typing.primTyName_vector, arg = ty}
         fun arrayOf ty =
           F.AppType {applied = F.TyVar Typing.primTyName_array, arg = ty}
         val pairOf = F.PairType
         val tupleOf = F.TupleType
         fun function1Of (a, b) = F.FnType (b, a)
         fun function2Of (a, b, c) =
           F.AppType
             { applied = F.AppType
                 { applied =
                     F.AppType
                       {applied = F.TyVar Typing.primTyName_function2, arg = a}
                 , arg = b
                 }
             , arg = c
             }
         fun function3Of (a, b, c, d) =
           F.AppType
             { applied = F.AppType
                 { applied = F.AppType
                     { applied =
                         F.AppType
                           { applied = F.TyVar Typing.primTyName_function2
                           , arg = a
                           }
                     , arg = b
                     }
                 , arg = c
                 }
             , arg = d
             }
         fun promptTagOf ty =
           F.AppType {applied = F.TyVar Typing.primTyName_prompt_tag, arg = ty}
         fun subcontOf (a, b) =
           F.AppType
             { applied =
                 F.AppType
                   {applied = F.TyVar Typing.primTyName_subcont, arg = a}
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
    val typeCheckPat : Env * (* expectedTy *) F.Ty * F.Pat -> ValEnv (* expectedTy must be normalized *)
    val typeCheckExp : Env * (* expectedTy *) F.Ty * F.Exp -> unit
     *)
    fun typeCheckPat (env: Env, expectedTy: F.Ty, F.WildcardPat _) = emptyValEnv
      | typeCheckPat
          ( env
          , expectedTy
          , F.SConPat {sourceSpan = _, scon = _, equality, cookedValue}
          ) =
          ( typeCheckExp (env, F.EqualityType expectedTy, equality)
          ; typeCheckExp (env, expectedTy, cookedValue)
          ; emptyValEnv
          )
      | typeCheckPat (env, expectedTy, F.VarPat (_, vid, ty)) =
          ( checkKind (#tyVarEnv env, F.TypeKind) ty
          ; checkType (#aliasEnv env, expectedTy) ty
          ; TypedSyntax.VIdMap.singleton (vid, ty)
          )
      | typeCheckPat
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
                       (map, typeCheckPat (env, ty, pat))
                 | NONE => raise TypeError "RecordPat: field mismatch")
              TypedSyntax.VIdMap.empty fieldPats
          else
            raise TypeError "RecordPat: number of fields"
      | typeCheckPat
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
            val restEnv = typeCheckPat
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
                       (map, typeCheckPat (env, ty, pat))
                 | NONE => raise TypeError "RecordPat: field mismatch") restEnv
              fieldPats
          end
      | typeCheckPat (env, _, F.RecordPat _) =
          raise TypeError "RecordPat: non-record type"
      | typeCheckPat
          ( env
          , expectedTy
          , F.ValConPat {sourceSpan = _, info as {tag, ...}, payload}
          ) =
          let
            fun getDatatype (args, ty) =
              case ty of
                F.TyVar tv => (List.rev args, tv)
              | F.AppType {applied, arg} => getDatatype (arg :: args, applied)
              | _ => raise TypeError "unexpected datatype"
            val (expectedArgs, tyCon) = getDatatype ([], expectedTy)
            val payloadTy =
              case TypedSyntax.TyVarMap.find (#valConEnv env, tyCon) of
                NONE => raise TypeError "unknown datatype"
              | SOME conMap =>
                  (case StringMap.find (conMap, tag) of
                     NONE => raise TypeError "unknown value constructor"
                   | SOME {tyParams = _, payload = NONE} => NONE
                   | SOME {tyParams, payload = SOME payloadTy} =>
                       let
                         val subst =
                           ListPair.foldlEq
                             (fn (tv, ty, acc) =>
                                TypedSyntax.TyVarMap.insert (acc, tv, ty))
                             TypedSyntax.TyVarMap.empty (tyParams, expectedArgs)
                       in
                         SOME (#doTy (F.substTy subst) payloadTy)
                       end)
          in
            case (payloadTy, payload) of
              (SOME expectedPayloadTy, SOME (actualPayloadTy, payloadPat)) =>
                ( checkType (#aliasEnv env, expectedPayloadTy) actualPayloadTy
                ; typeCheckPat (env, actualPayloadTy, payloadPat)
                )
            | (NONE, NONE) => emptyValEnv
            | _ => raise TypeError "payload mismatch"
          end
      | typeCheckPat
          (env, expectedTy, F.ExnConPat {sourceSpan = _, tagPath, payload}) =
          ( checkType (#aliasEnv env, expectedTy)
              (F.TyVar Typing.primTyName_exn)
          ; typeCheckExp (env, F.TyVar Typing.primTyName_exntag, tagPath)
          (* TODO: check if the constructor has a payload *)
          ; case payload of
              SOME (payloadTy, payloadPat) =>
                typeCheckPat (env, payloadTy, payloadPat)
            | NONE => emptyValEnv
          )
      | typeCheckPat (env, expectedTy, F.LayeredPat (_, vid, ty, pat)) =
          let
            val ty = normalizeType (#aliasEnv env) ty
            val () = checkType (#aliasEnv env, expectedTy) ty
            val newEnv = typeCheckPat (env, expectedTy, pat)
          in
            TypedSyntax.VIdMap.insert (newEnv, vid, ty)
          end
      | typeCheckPat (env, expectedTy, F.VectorPat (_, pats, ellipsis, elemTy)) =
          let
            val elemTy = normalizeType (#aliasEnv env) elemTy
            val () = checkType (#aliasEnv env, expectedTy)
              (F.AppType
                 {applied = F.TyVar Typing.primTyName_vector, arg = elemTy})
          in
            Vector.foldl
              (fn (pat, newEnv) =>
                 TypedSyntax.VIdMap.unionWith
                   (fn _ => raise TypeError "VectorPat: duplicate binding")
                   (newEnv, typeCheckPat (env, elemTy, pat)))
              TypedSyntax.VIdMap.empty pats
          end
    and inferExp (env: Env, exp: F.Exp) : F.Ty =
      raise Fail "not implemented yet"
    and typeCheckExp
          ( env: Env
          , expectedTy: F.Ty
          , F.PrimExp (F.PrimCall primOp, tyargs, valargs)
          ) =
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
              (fn (expectedTy, exp) => typeCheckExp (env, expectedTy, exp))
              (Vector.foldr (op::) [] args, valargs);
            checkType (#aliasEnv env, expectedTy) resultTy
          end
      | typeCheckExp (env, expectedTy, F.PrimExp (F.IntConstOp _, [ty], [])) =
          checkType (#aliasEnv env, expectedTy) ty
      | typeCheckExp (env, expectedTy, F.PrimExp (F.WordConstOp _, [ty], [])) =
          checkType (#aliasEnv env, expectedTy) ty
      | typeCheckExp (env, expectedTy, F.PrimExp (F.RealConstOp _, [ty], [])) =
          checkType (#aliasEnv env, expectedTy) ty
      | typeCheckExp (env, expectedTy, F.PrimExp (F.Char8ConstOp _, [ty], [])) =
          checkType (#aliasEnv env, expectedTy) ty
      | typeCheckExp (env, expectedTy, F.PrimExp (F.Char16ConstOp _, [ty], [])) =
          checkType (#aliasEnv env, expectedTy) ty
      | typeCheckExp (env, expectedTy, F.PrimExp (F.String8ConstOp _, [ty], [])) =
          checkType (#aliasEnv env, expectedTy) ty
      | typeCheckExp
          (env, expectedTy, F.PrimExp (F.String16ConstOp _, [ty], [])) =
          checkType (#aliasEnv env, expectedTy) ty
      | typeCheckExp (env, expectedTy, F.PrimExp (F.RaiseOp _, [ty], [e])) =
          ( typeCheckExp (env, F.TyVar Typing.primTyName_exn, e)
          ; checkType (#aliasEnv env, expectedTy) ty
          )
      | typeCheckExp (env, expectedTy, F.PrimExp (F.ListOp, [elemTy], elements)) =
          ( List.app (fn elem => typeCheckExp (env, elemTy, elem)) elements
          ; checkType (#aliasEnv env, expectedTy)
              (F.AppType
                 {applied = F.TyVar Typing.primTyName_list, arg = elemTy})
          )
      | typeCheckExp
          (env, expectedTy, F.PrimExp (F.VectorOp, [elemTy], elements)) =
          ( List.app (fn elem => typeCheckExp (env, elemTy, elem)) elements
          ; checkType (#aliasEnv env, expectedTy)
              (F.AppType
                 {applied = F.TyVar Typing.primTyName_vector, arg = elemTy})
          )
      | typeCheckExp
          ( env
          , expectedTy
          , F.PrimExp (F.DataTagAsStringOp _, [dataTy], [data])
          ) (* TODO: Check constructor info *) =
          ( typeCheckExp (env, dataTy, data)
          ; checkType (#aliasEnv env, expectedTy)
              (F.TyVar Typing.primTyName_string)
          )
      | typeCheckExp
          ( env
          , expectedTy
          , F.PrimExp (F.DataTagAsString16Op _, [dataTy], [data])
          ) (* TODO: Check constructor info *) =
          ( typeCheckExp (env, dataTy, data)
          ; checkType (#aliasEnv env, expectedTy)
              (F.TyVar Typing.primTyName_string16)
          )
      | typeCheckExp
          ( env
          , expectedTy
          , F.PrimExp (F.DataPayloadOp _, [dataTy, payloadTy], [data])
          ) (* TODO: Check constructor info *) =
          ( typeCheckExp (env, dataTy, data)
          ; checkType (#aliasEnv env, expectedTy) payloadTy
          )
      | typeCheckExp
          ( env
          , expectedTy
          , F.PrimExp (F.ExnPayloadOp, [payloadTy], [data])
          ) (* TODO: Check constructor info *) =
          ( typeCheckExp (env, F.TyVar Typing.primTyName_exn, data)
          ; checkType (#aliasEnv env, expectedTy) payloadTy
          )
      | typeCheckExp
          ( env
          , expectedTy
          , F.PrimExp (F.ConstructValOp _, [dataTy], [])
          ) (* TODO: Check constructor info *) =
          checkType (#aliasEnv env, expectedTy) dataTy
      | typeCheckExp
          ( env
          , expectedTy
          , F.PrimExp
              (F.ConstructValWithPayloadOp _, [dataTy, payloadTy], [payload])
          ) =
          ( typeCheckExp (env, payloadTy, payload)
          ; checkType (#aliasEnv env, expectedTy) dataTy
          )
      | typeCheckExp (env, expectedTy, F.PrimExp (F.ConstructExnOp, [], [tag])) =
          ( typeCheckExp (env, F.TyVar Typing.primTyName_exntag, tag)
          ; checkType (#aliasEnv env, expectedTy)
              (F.TyVar Typing.primTyName_exn)
          )
      | typeCheckExp
          ( env
          , expectedTy
          , F.PrimExp (F.ConstructExnWithPayloadOp, [payloadTy], [tag, payload])
          ) =
          ( typeCheckExp (env, F.TyVar Typing.primTyName_exntag, tag)
          ; typeCheckExp (env, payloadTy, payload)
          ; checkType (#aliasEnv env, expectedTy)
              (F.TyVar Typing.primTyName_exn)
          )
      | typeCheckExp (env, expectedTy, F.PrimExp (F.JsCallOp, [], f :: args)) =
          ( typeCheckExp (env, F.TyVar Typing.primTyName_JavaScript_value, f)
          ; List.app
              (fn a =>
                 typeCheckExp
                   (env, F.TyVar Typing.primTyName_JavaScript_value, a)) args
          ; checkType (#aliasEnv env, expectedTy) (F.TyVar
              Typing.primTyName_JavaScript_value)
          )
      | typeCheckExp
          (env, expectedTy, F.PrimExp (F.JsMethodOp, [], f :: name :: args)) =
          ( typeCheckExp (env, F.TyVar Typing.primTyName_JavaScript_value, f)
          ; typeCheckExp (env, F.TyVar Typing.primTyName_JavaScript_value, name)
          ; List.app
              (fn a =>
                 typeCheckExp
                   (env, F.TyVar Typing.primTyName_JavaScript_value, a)) args
          ; checkType (#aliasEnv env, expectedTy) (F.TyVar
              Typing.primTyName_JavaScript_value)
          )
      | typeCheckExp (env, expectedTy, F.PrimExp (F.JsNewOp, [], f :: args)) =
          ( typeCheckExp (env, F.TyVar Typing.primTyName_JavaScript_value, f)
          ; List.app
              (fn a =>
                 typeCheckExp
                   (env, F.TyVar Typing.primTyName_JavaScript_value, a)) args
          ; checkType (#aliasEnv env, expectedTy) (F.TyVar
              Typing.primTyName_JavaScript_value)
          )
      | typeCheckExp (env, expectedTy, F.PrimExp (F.LuaCallOp, [], f :: args)) =
          ( typeCheckExp (env, F.TyVar Typing.primTyName_Lua_value, f)
          ; List.app
              (fn a =>
                 typeCheckExp (env, F.TyVar Typing.primTyName_Lua_value, a))
              args
          ; checkType (#aliasEnv env, expectedTy)
              (F.AppType
                 { applied = F.TyVar Typing.primTyName_vector
                 , arg = F.TyVar Typing.primTyName_Lua_value
                 })
          )
      | typeCheckExp (env, expectedTy, F.PrimExp (F.LuaCall1Op, [], f :: args)) =
          ( typeCheckExp (env, F.TyVar Typing.primTyName_Lua_value, f)
          ; List.app
              (fn a =>
                 typeCheckExp (env, F.TyVar Typing.primTyName_Lua_value, a))
              args
          ; checkType (#aliasEnv env, expectedTy)
              (F.TyVar Typing.primTyName_Lua_value)
          )
      | typeCheckExp
          (env, expectedTy, F.PrimExp (F.LuaCallNOp n, [], f :: args)) =
          let
            val valueTy = F.TyVar Typing.primTyName_Lua_value
            fun loop (0, acc) = F.RecordType acc
              | loop (i, acc) =
                  loop
                    ( i - 1
                    , Syntax.LabelMap.insert
                        (acc, Syntax.NumericLabel i, valueTy)
                    )
          in
            typeCheckExp (env, F.TyVar Typing.primTyName_Lua_value, f);
            List.app
              (fn a =>
                 typeCheckExp (env, F.TyVar Typing.primTyName_Lua_value, a))
              args;
            checkType (#aliasEnv env, expectedTy)
              (loop (n, Syntax.LabelMap.empty))
          end
      | typeCheckExp
          (env, expectedTy, F.PrimExp (F.LuaMethodOp _, [], obj :: args)) =
          ( typeCheckExp (env, F.TyVar Typing.primTyName_Lua_value, obj)
          ; List.app
              (fn a =>
                 typeCheckExp (env, F.TyVar Typing.primTyName_Lua_value, a))
              args
          ; checkType (#aliasEnv env, expectedTy)
              (F.AppType
                 { applied = F.TyVar Typing.primTyName_vector
                 , arg = F.TyVar Typing.primTyName_Lua_value
                 })
          )
      | typeCheckExp
          (env, expectedTy, F.PrimExp (F.LuaMethod1Op _, [], obj :: args)) =
          ( typeCheckExp (env, F.TyVar Typing.primTyName_Lua_value, obj)
          ; List.app
              (fn a =>
                 typeCheckExp (env, F.TyVar Typing.primTyName_Lua_value, a))
              args
          ; checkType (#aliasEnv env, expectedTy)
              (F.TyVar Typing.primTyName_Lua_value)
          )
      | typeCheckExp
          (env, expectedTy, F.PrimExp (F.LuaMethodNOp (_, n), [], obj :: args)) =
          let
            val valueTy = F.TyVar Typing.primTyName_Lua_value
            fun loop (0, acc) = F.RecordType acc
              | loop (i, acc) =
                  loop
                    ( i - 1
                    , Syntax.LabelMap.insert
                        (acc, Syntax.NumericLabel i, valueTy)
                    )
          in
            typeCheckExp (env, F.TyVar Typing.primTyName_Lua_value, obj);
            List.app
              (fn a =>
                 typeCheckExp (env, F.TyVar Typing.primTyName_Lua_value, a))
              args;
            checkType (#aliasEnv env, expectedTy)
              (loop (n, Syntax.LabelMap.empty))
          end
      | typeCheckExp (_, _, F.PrimExp (p, _, _)) =
          raise TypeError
            ("PrimOp with invalid arguments: "
             ^ Printer.build (FPrinter.doPrimOp p))
      | typeCheckExp (env, expectedTy, F.VarExp vid) =
          (case TypedSyntax.VIdMap.find (#valEnv env, vid) of
             NONE =>
               raise TypeError ("Unbound variable " ^ TypedSyntax.print_VId vid)
           | SOME ty => checkType (#aliasEnv env, expectedTy) ty)
      | typeCheckExp (env, expectedTy, F.RecordExp fields) =
          (case expectedTy of
             F.RecordType fieldTypes =>
               if Syntax.LabelMap.numItems fieldTypes = List.length fields then
                 List.app
                   (fn (label, field) =>
                      case Syntax.LabelMap.find (fieldTypes, label) of
                        SOME fieldTy => typeCheckExp (env, fieldTy, field)
                      | NONE => raise TypeError "field mismatch") fields
               else
                 raise TypeError "field mismatch"
           | _ => raise TypeError "actual: record")
      | typeCheckExp (env, expectedTy, F.LetExp (decs, exp)) =
          let val env' = typeCheckDecs (env, decs)
          in typeCheckExp (env', expectedTy, exp)
          end
      | typeCheckExp (env, expectedTy, F.AppExp (f, arg)) =
          let
            val fTy = inferExp (env, f)
          in
            case fTy of
              F.FnType (paramTy, resultTy) =>
                ( typeCheckExp (env, paramTy, arg)
                ; checkType (#aliasEnv env, expectedTy) resultTy
                )
            | _ => raise TypeError "invalid function application"
          end
      (* | typeCheckExp (env, expectedTy, F.HandleExp {body, exnName, handler}) *)
      (* | typeCheckExp (env, expectedTy, F.IfThenElseExp (cond, t, e)) *)
      (* | typeCheckExp (env, expectedTy, F.CaseExp { sourceSpan = _, subjectExp, subjectTy, matches, matchType, resultTy }) *)
      (* | typeCheckExp (env, expectedTy, F.FnExp (vid, ty, exp)) *)
      (* | typeCheckExp (env, expectedTy, F.ProjectionExp {label, record, fieldTypes}) *)
      (* | typeCheckExp (env, expectedTy, F.TyAbsExp (tv, kind, exp)) *)
      (* | typeCheckExp (env, expectedTy, F.TyAppExp (exp, ty)) *)
      (* | typeCheckExp (env, expectedTy, F.PackExp { payloadTy, exp, packageTy }) *)
      (* | typeCheckExp (env, expectedTy, F.BogusExp ty) *)
      | typeCheckExp (env, expectedTy, F.ExitProgram) = ()
      (* | typeCheckExp (env, expectedTy, F.ExportValue _) = () *)
      (* | typeCheckExp (env, expectedTy, F.ExportModule _) = () *)
      | typeCheckExp _ = raise Fail "not implemented yet"
    and typeCheckDecs (env, decs) =
      List.foldl typeCheckDec env decs
    and typeCheckDec (F.ValDec (vid, NONE, exp), env: Env) : Env =
          let
            val ty = inferExp (env, exp)
          in
            modifyValEnv
              (fn valEnv => TypedSyntax.VIdMap.insert (valEnv, vid, ty), env)
          end
      | typeCheckDec (F.ValDec (vid, SOME ty, exp), env) =
          ( typeCheckExp (env, ty, exp)
          ; modifyValEnv
              (fn valEnv => TypedSyntax.VIdMap.insert (valEnv, vid, ty), env)
          )
      | typeCheckDec (F.RecValDec defs, env) =
          let
            val env' = modifyValEnv
              ( fn valEnv =>
                  List.foldl
                    (fn ((vid, ty, _), acc) =>
                       TypedSyntax.VIdMap.insert (acc, vid, ty)) valEnv defs
              , env
              )
          in
            List.app (fn (_, ty, exp) => typeCheckExp (env', ty, exp)) defs;
            env'
          end
      | typeCheckDec (F.UnpackDec (tv, kind, vid, ty, exp), env) =
          (case inferExp (env, exp) of
             F.ExistsType (tv', kind', ty') =>
               ( if kind <> kind' then raise TypeError "kind mismatch" else ()
               ; checkType (#aliasEnv env, ty)
                   (#doTy
                      (F.substTy
                         (TypedSyntax.TyVarMap.singleton (tv', F.TyVar tv))) ty')
               ; { valEnv = TypedSyntax.VIdMap.insert (#valEnv env, vid, ty)
                 , tyVarEnv =
                     TypedSyntax.TyVarMap.insert (#tyVarEnv env, tv, kind)
                 , aliasEnv = #aliasEnv env
                 , valConEnv = #valConEnv env
                 }
               )
           | _ => raise TypeError "expected ExistsType")
      | typeCheckDec (F.IgnoreDec exp, env) =
          let val _ = inferExp (env, exp)
          in env
          end
      | typeCheckDec (F.DatatypeDec datbinds, env) =
          let
            fun doDatBind (F.DatBind (tyvars, tyname, conbinds), valConEnv) =
              let
                fun doConBind
                  (F.ConBind (TypedSyntax.MkVId (name, _), optPayloadTy), m) =
                  StringMap.insert
                    (m, name, {tyParams = tyvars, payload = optPayloadTy})
              in
                TypedSyntax.TyVarMap.insert
                  ( valConEnv
                  , tyname
                  , List.foldl doConBind StringMap.empty conbinds
                  )
              end
          in
            { valEnv = #valEnv env
            , tyVarEnv = #tyVarEnv env
            , aliasEnv = #aliasEnv env
            , valConEnv = List.foldl doDatBind (#valConEnv env) datbinds
            }
          end
      | typeCheckDec (F.ExceptionDec {name = _, tagName, payloadTy = _}, env) =
          modifyValEnv
            ( fn valEnv =>
                TypedSyntax.VIdMap.insert
                  (valEnv, tagName, F.TyVar Typing.primTyName_exntag)
            , env
            )
      | typeCheckDec (F.ESImportDec {pure = _, specs, moduleName = _}, env) =
          modifyValEnv
            ( fn valEnv =>
                List.foldl
                  (fn ((_, vid, ty), acc) =>
                     TypedSyntax.VIdMap.insert (acc, vid, ty)) valEnv specs
            , env
            )
  end (* local *)
end (* structure TypeCheckF *)
