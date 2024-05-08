(*
 * Copyright (c) 2023 ARATA Mizuki
 * This file is part of LunarML.
 *)
(* Compile pattern match into chain of if-else expressions *)
structure DesugarPatternMatches :> sig
              exception DesugarError of SourcePos.span list * string
              type Context = { nextVId : int ref
                             , nextTyVar : int ref
                             , targetInfo : TargetInfo.target_info
                             , messageHandler : Message.handler
                             }
              val desugarPatternMatches : Context -> { doExp : FSyntax.Exp -> FSyntax.Exp, doDec : FSyntax.Dec -> FSyntax.Dec, doDecs : FSyntax.Dec list -> FSyntax.Dec list }
          end = struct
exception DesugarError of SourcePos.span list * string
structure F = FSyntax
type Context = { nextVId : int ref
               , nextTyVar : int ref
               , targetInfo : TargetInfo.target_info
               , messageHandler : Message.handler
               }
fun freshVId(ctx : Context, name: string) = let val n = !(#nextVId ctx)
                                            in #nextVId ctx := n + 1
                                             ; TypedSyntax.MkVId (name, n)
                                            end
(* Check if the pattern is exhaustive and binds no variable *)
fun isWildcardPat (F.WildcardPat _) = true
  | isWildcardPat (F.SConPat _) = false
  | isWildcardPat (F.VarPat _) = false
  | isWildcardPat (F.RecordPat { sourceSpan = _, fields, ellipsis = NONE, allFields = _ }) = List.all (fn (_, pat) => isWildcardPat pat) fields
  | isWildcardPat (F.RecordPat { sourceSpan = _, fields, ellipsis = SOME basePat, allFields = _ }) = isWildcardPat basePat andalso List.all (fn (_, pat) => isWildcardPat pat) fields
  | isWildcardPat (F.ValConPat _) = false (* TODO *)
  | isWildcardPat (F.ExnConPat _) = false
  | isWildcardPat (F.LayeredPat _) = false
  | isWildcardPat (F.VectorPat (_, pats, ellipsis, _)) = ellipsis andalso Vector.length pats = 0
fun desugarPatternMatches (ctx: Context) : { doExp : F.Exp -> F.Exp, doDec : F.Dec -> F.Dec, doDecs : F.Dec list -> F.Dec list }
    = let fun doExp exp0
              = (case exp0 of
                     F.PrimExp (primOp, tyargs, args) => F.PrimExp (primOp, tyargs, List.map doExp args)
                   | F.VarExp _ => exp0
                   | F.RecordExp fields => F.RecordExp (List.map (fn (label, e) => (label, doExp e)) fields)
                   | F.LetExp (decs, exp) => F.LetExp (List.map doDec decs, doExp exp)
                   | F.AppExp (exp1, exp2) => F.AppExp (doExp exp1, doExp exp2)
                   | F.HandleExp { body, exnName, handler } => F.HandleExp { body = doExp body, exnName = exnName, handler = doExp handler }
                   | F.IfThenElseExp (exp1, exp2, exp3) => F.IfThenElseExp (doExp exp1, doExp exp2, doExp exp3)
                   | F.FnExp (vid, ty, exp) => F.FnExp (vid, ty, doExp exp)
                   | F.ProjectionExp { label, record, fieldTypes } => F.ProjectionExp { label = label, record = doExp record, fieldTypes = fieldTypes }
                   | F.TyAbsExp (tv, kind, exp) => F.TyAbsExp (tv, kind, doExp exp)
                   | F.TyAppExp (exp, ty) => F.TyAppExp (doExp exp, ty)
                   | F.PackExp { payloadTy, exp, packageTy } => F.PackExp { payloadTy = payloadTy, exp = doExp exp, packageTy = packageTy }
                   | F.CaseExp { sourceSpan = _, subjectExp, subjectTy = _, matches = [(F.VarPat (_, vid, ty'), exp2 as F.VarExp vid')], matchType = _, resultTy = _ } =>
                     if TypedSyntax.eqVId (vid, vid') then
                         doExp subjectExp
                     else
                         F.LetExp ([F.ValDec (vid, SOME ty', doExp subjectExp)], exp2)
                   | F.CaseExp { sourceSpan, subjectExp, subjectTy, matches, matchType, resultTy } =>
                     let val canIgnore = case matches of
                                             [(pat, innerExp)] => if isWildcardPat pat then SOME innerExp else NONE
                                           | _ => NONE
                     in case canIgnore of
                            SOME innerExp => F.LetExp ([F.IgnoreDec (doExp subjectExp)], doExp innerExp)
                          | NONE => let val examinedVId = freshVId(ctx, "exp")
                                        val examinedExp = F.VarExp(examinedVId)
                                        fun go [] = (case matchType of
                                                         TypedSyntax.CASE => F.RaiseExp (sourceSpan, resultTy, F.VarExp InitialEnv.VId_Match)
                                                       | TypedSyntax.VAL => F.RaiseExp (sourceSpan, resultTy, F.VarExp InitialEnv.VId_Bind)
                                                       | TypedSyntax.HANDLE => F.RaiseExp (SourcePos.nullSpan, resultTy, examinedExp)
                                                    )
                                          | go ((pat, innerExp) :: rest)
                                            = let val binders = genBinders examinedExp subjectTy pat
                                                  val matcher = genMatcher examinedExp subjectTy pat
                                              in if isExhaustive pat then
                                                     List.foldr (fn (valbind, exp) => F.LetExp ([F.ValDec valbind], exp)) (doExp innerExp) binders
                                                 else
                                                     F.IfThenElseExp (matcher, List.foldr (fn (valbind, exp) => F.LetExp ([F.ValDec valbind], exp)) (doExp innerExp) binders, go rest)
                                              end
                                    in F.LetExp ([F.ValDec (examinedVId, SOME subjectTy, doExp subjectExp)], go matches)
                                    end
                     end
                   | F.BogusExp _ => exp0
                   | F.ExitProgram => exp0
                   | F.ExportValue exp => F.ExportValue (doExp exp)
                   | F.ExportModule entities => F.ExportModule (Vector.map (fn (name, exp) => (name, doExp exp)) entities)
                )
          and doDec (F.ValDec (vid, optTy, exp)) = F.ValDec (vid, optTy, doExp exp)
            | doDec (F.RecValDec valbinds) = F.RecValDec (List.map (fn (v, ty, exp) => (v, ty, doExp exp)) valbinds)
            | doDec (F.UnpackDec (tv, kind, vid, ty, exp)) = F.UnpackDec (tv, kind, vid, ty, doExp exp)
            | doDec (F.IgnoreDec exp) = F.IgnoreDec (doExp exp)
            | doDec (dec as F.DatatypeDec _) = dec
            | doDec (dec as F.ExceptionDec _) = dec
            | doDec (dec as F.ESImportDec _) = dec
          and genMatcher _ _ (F.WildcardPat _) : F.Exp = F.VarExp InitialEnv.VId_true (* always match *)
            | genMatcher exp _ (F.SConPat { sourceSpan = _, scon = _, equality, cookedValue }) = F.AppExp (equality, F.TupleExp [exp, cookedValue])
            | genMatcher _ _ (F.VarPat (_, _, _)) = F.VarExp InitialEnv.VId_true (* always match *)
            | genMatcher exp (recordTy as F.RecordType fieldTypes) (F.RecordPat { sourceSpan, fields, ellipsis = NONE, allFields = _ })
              = List.foldr (fn ((label, pat), e) =>
                               case Syntax.LabelMap.find (fieldTypes, label) of
                                   SOME fieldTy => let val exp = genMatcher (F.ProjectionExp { label = label, record = exp, fieldTypes = fieldTypes }) fieldTy pat
                                                   in F.SimplifyingAndalsoExp (exp, e)
                                                   end
                                 | NONE => raise DesugarError ([sourceSpan], "internal error: record field not found (fieldTypes=" ^ FSyntax.PrettyPrint.print_Ty recordTy ^ ", " ^ Syntax.PrettyPrint.print_Label label ^ ")")
                           )
                           (F.VarExp InitialEnv.VId_true)
                           fields
            | genMatcher exp (recordTy as F.RecordType fieldTypes) (F.RecordPat { sourceSpan, fields, ellipsis = SOME basePat, allFields = _ })
              = let val restTypes = List.foldl (fn ((label, _), fieldTypes) => #1 (Syntax.LabelMap.remove (fieldTypes, label))) fieldTypes fields
                    val restExp = F.RecordExp (Syntax.LabelMap.foldri (fn (label, _, xs) => (label, F.ProjectionExp { label = label, record = exp, fieldTypes = fieldTypes }) :: xs) [] restTypes)
                    val init = genMatcher restExp (F.RecordType restTypes) basePat
                in List.foldr (fn ((label, pat), e) =>
                                  case Syntax.LabelMap.find (fieldTypes, label) of
                                      SOME fieldTy => let val exp = genMatcher (F.ProjectionExp { label = label, record = exp, fieldTypes = fieldTypes }) fieldTy pat
                                                      in F.SimplifyingAndalsoExp (exp, e)
                                                      end
                                    | NONE => raise DesugarError ([sourceSpan], "internal error: record field not found (fieldTypes=" ^ FSyntax.PrettyPrint.print_Ty recordTy ^ ", " ^ Syntax.PrettyPrint.print_Label label ^ ")")
                              )
                              init
                              fields
                end
            | genMatcher _ _ (F.RecordPat { sourceSpan, fields = _, ellipsis = _, allFields = _ }) = raise DesugarError ([sourceSpan], "internal error: record pattern against non-record type")
            | genMatcher exp ty (F.ValConPat { sourceSpan, info, payload = SOME (payloadTy, payloadPat) })
              = (case info of
                     { representation = Syntax.REP_LIST, tag = "::", ... } =>
                     let val elemTy = case ty of
                                          F.AppType { applied = _, arg } => arg
                                        | _ => raise DesugarError ([sourceSpan], "internal error: nil pattern with invalid type")
                         val hdExp = F.PrimExp (F.PrimCall Primitives.List_unsafeHead, [elemTy], [exp])
                         val tlExp = F.PrimExp (F.PrimCall Primitives.List_unsafeTail, [elemTy], [exp])
                         val payload = genMatcher (F.TupleExp [hdExp, tlExp]) payloadTy payloadPat
                     in F.SimplifyingAndalsoExp (F.PrimExp (F.PrimCall Primitives.Bool_not, [], [F.PrimExp (F.PrimCall Primitives.List_null, [elemTy], [exp])]), payload)
                     end
                   | { representation = Syntax.REP_REF, tag = "ref", ... } =>
                     genMatcher (F.PrimExp (F.PrimCall Primitives.Ref_read, [payloadTy], [exp])) payloadTy payloadPat
                   | { representation = Syntax.REP_ALIAS, ... } =>
                     genMatcher (F.PrimExp (F.DataPayloadOp info, [payloadTy], [exp])) payloadTy payloadPat
                   | { tag, ... } => (* REP_BOXED *)
                     let val payload = genMatcher (F.PrimExp (F.DataPayloadOp info, [payloadTy], [exp])) payloadTy payloadPat
                         val (dataTagOp, equalTag) = case #datatypeTag (#targetInfo ctx) of
                                                         TargetInfo.STRING8 => (F.DataTagAsStringOp, Primitives.String_EQUAL)
                                                       | TargetInfo.STRING16 => (F.DataTagAsString16Op, Primitives.String16_EQUAL)
                     in F.SimplifyingAndalsoExp (F.PrimExp (F.PrimCall equalTag, [], [F.PrimExp (dataTagOp info, [], [exp]), F.AsciiStringAsDatatypeTag (#targetInfo ctx, tag)]), payload)
                     end
                )
            | genMatcher exp ty (F.ValConPat { sourceSpan, info, payload = NONE })
              = (case info of
                     { representation = Syntax.REP_BOOL, tag = "true", ... } => exp
                   | { representation = Syntax.REP_BOOL, tag = "false", ... } => F.PrimExp (F.PrimCall Primitives.Bool_not, [], [exp])
                   | { representation = Syntax.REP_LIST, tag = "nil", ... } =>
                     let val elemTy = case ty of
                                          F.AppType { applied = _, arg } => arg
                                        | _ => raise DesugarError ([sourceSpan], "internal error: nil pattern with invalid type")
                     in F.PrimExp (F.PrimCall Primitives.List_null, [elemTy], [exp])
                     end
                   | { representation = Syntax.REP_UNIT, ... } => F.VarExp InitialEnv.VId_true
                   | { representation = _, tag, ... } => (* REP_BOXED or REP_ENUM *)
                     let val (dataTagOp, equalTag) = case #datatypeTag (#targetInfo ctx) of
                                                         TargetInfo.STRING8 => (F.DataTagAsStringOp, Primitives.String_EQUAL)
                                                       | TargetInfo.STRING16 => (F.DataTagAsString16Op, Primitives.String16_EQUAL)
                     in F.PrimExp (F.PrimCall equalTag, [], [F.PrimExp (dataTagOp info, [], [exp]), F.AsciiStringAsDatatypeTag (#targetInfo ctx, tag)])
                     end
                )
            | genMatcher exp _ (F.ExnConPat { sourceSpan = _, tagPath = tag, payload = SOME (payloadTy, payloadPat) })
              = let val payload = genMatcher (F.PrimExp (F.ExnPayloadOp, [payloadTy], [exp])) payloadTy payloadPat
                in F.SimplifyingAndalsoExp (F.PrimExp (F.PrimCall Primitives.Exception_instanceof, [], [exp, tag]), payload)
                end
            | genMatcher exp _ (F.ExnConPat { sourceSpan = _, tagPath = tag, payload = NONE })
              = F.PrimExp (F.PrimCall Primitives.Exception_instanceof, [], [exp, tag])
            | genMatcher exp ty0 (F.LayeredPat (_, _, _ (* ty1 *), innerPat)) = genMatcher exp ty0 innerPat
            | genMatcher exp _ (F.VectorPat (_, pats, ellipsis, elemTy))
              = let val vectorLengthExp = F.PrimExp (F.PrimCall (Primitives.Vector_length Primitives.INT), [elemTy], [exp])
                    val intTy = F.TyCon ([], Typing.primTyName_int)
                    val expectedLengthExp = F.IntConstExp (Int.toLarge (Vector.length pats), intTy)
                    val e0 = if ellipsis then
                                 F.PrimExp (F.PrimCall (Primitives.Int_GE Primitives.INT), [], [vectorLengthExp, expectedLengthExp])
                             else
                                 F.PrimExp (F.PrimCall (Primitives.Int_EQUAL Primitives.INT), [], [vectorLengthExp, expectedLengthExp])
                in Vector.foldri (fn (i, pat, e) => let val exp = genMatcher (F.PrimExp (F.PrimCall (Primitives.Unsafe_Vector_sub Primitives.INT), [elemTy], [exp, F.IntConstExp (Int.toLarge i, intTy)])) elemTy pat
                                                    in F.SimplifyingAndalsoExp (e, exp)
                                                    end
                                 ) e0 pats
                end
          and genBinders _ _ (F.WildcardPat _) = []
            | genBinders _ _ (F.SConPat _) = []
            | genBinders exp _ (F.VarPat (_, vid, ty)) = [(vid, SOME ty, exp)]
            | genBinders exp (F.RecordType fieldTypes) (F.RecordPat { sourceSpan = _, fields, ellipsis = NONE, allFields = _ }) = List.concat (List.map (fn (label, innerPat) => genBinders (F.ProjectionExp { label = label, record = exp, fieldTypes = fieldTypes }) (Syntax.LabelMap.lookup (fieldTypes, label)) innerPat) fields)
            | genBinders exp (F.RecordType fieldTypes) (F.RecordPat { sourceSpan = _, fields, ellipsis = SOME basePat, allFields = _ })
              = let val restTypes = List.foldl (fn ((label, _), fieldTypes) => #1 (Syntax.LabelMap.remove (fieldTypes, label))) fieldTypes fields
                    val restExp = F.RecordExp (Syntax.LabelMap.foldri (fn (label, _, xs) => (label, F.ProjectionExp { label = label, record = exp, fieldTypes = fieldTypes }) :: xs) [] restTypes)
                in genBinders restExp (F.RecordType restTypes) basePat @ List.concat (List.map (fn (label, innerPat) => genBinders (F.ProjectionExp { label = label, record = exp, fieldTypes = fieldTypes }) (Syntax.LabelMap.lookup (fieldTypes, label)) innerPat) fields)
                end
            | genBinders _ _ (F.RecordPat { sourceSpan, fields = _, ellipsis = _, allFields = _ }) = raise DesugarError ([sourceSpan], "internal error: record pattern against non-record type")
            | genBinders exp ty (F.ValConPat { sourceSpan, info, payload = SOME (payloadTy, payloadPat) })
              = (case info of
                     { representation = Syntax.REP_REF, tag = "ref", ... } => genBinders (F.PrimExp (F.PrimCall Primitives.Ref_read, [payloadTy], [exp])) payloadTy payloadPat
                   | { representation = Syntax.REP_LIST, tag = "::", ... } =>
                     let val elemTy = case ty of
                                          F.AppType { applied = _, arg } => arg
                                        | _ => raise DesugarError ([sourceSpan], "internal error: nil pattern with invalid type")
                         val hdExp = F.PrimExp (F.PrimCall Primitives.List_unsafeHead, [elemTy], [exp])
                         val tlExp = F.PrimExp (F.PrimCall Primitives.List_unsafeTail, [elemTy], [exp])
                     in genBinders (F.TupleExp [hdExp, tlExp]) payloadTy payloadPat
                     end
                   | _ => genBinders (F.PrimExp (F.DataPayloadOp info, [payloadTy], [exp])) payloadTy payloadPat
                )
            | genBinders _ _ (F.ValConPat { sourceSpan = _, info = _, payload = NONE }) = []
            | genBinders exp _ (F.ExnConPat { sourceSpan = _, tagPath = _, payload = SOME (payloadTy, payloadPat) }) = genBinders (F.PrimExp (F.ExnPayloadOp, [payloadTy], [exp])) payloadTy payloadPat
            | genBinders _ _ (F.ExnConPat { sourceSpan = _, tagPath = _, payload = NONE }) = []
            | genBinders exp _ (F.LayeredPat (_, vid, ty, pat)) = (vid, SOME ty, exp) :: genBinders exp ty pat
            | genBinders exp _ (F.VectorPat (_, pats, _, elemTy)) = let val intTy = F.TyCon ([], Typing.primTyName_int)
                                                                    in Vector.foldri (fn (i, pat, acc) => genBinders (F.PrimExp (F.PrimCall (Primitives.Unsafe_Vector_sub Primitives.INT), [elemTy], [exp, F.IntConstExp (Int.toLarge i, intTy)])) elemTy pat @ acc) [] pats
                                                                    end
          and isExhaustive (F.WildcardPat _) = true
            | isExhaustive (F.SConPat _) = false
            | isExhaustive (F.VarPat _) = true
            | isExhaustive (F.RecordPat { sourceSpan = _, fields, ellipsis = NONE, allFields = _ }) = List.all (fn (_, e) => isExhaustive e) fields
            | isExhaustive (F.RecordPat { sourceSpan = _, fields, ellipsis = SOME basePat, allFields = _ }) = isExhaustive basePat andalso List.all (fn (_, e) => isExhaustive e) fields
            | isExhaustive (F.ValConPat _) = false (* TODO *)
            | isExhaustive (F.ExnConPat _) = false
            | isExhaustive (F.LayeredPat (_, _, _, innerPat)) = isExhaustive innerPat
            | isExhaustive (F.VectorPat (_, pats, ellipsis, _)) = ellipsis andalso Vector.length pats = 0
          fun doDecs decs = List.map doDec decs
      in { doExp = doExp
         , doDec = doDec
         , doDecs = doDecs
         }
      end
end (* structure DesugarPatternMatches *)

structure DecomposeValRec :> sig
              val doExp : FSyntax.Exp -> FSyntax.Exp
          end = struct
structure F = FSyntax
fun doExp (F.PrimExp (primOp, tyargs, args)) = F.PrimExp (primOp, tyargs, List.map doExp args)
  | doExp (exp as F.VarExp _) = exp
  | doExp (F.RecordExp fields) = F.RecordExp (List.map (fn (label, exp) => (label, doExp exp)) fields)
  | doExp (F.LetExp (decs, exp)) = F.LetExp (List.concat (List.map doDec decs), doExp exp)
  | doExp (F.AppExp (exp1, exp2)) = F.AppExp (doExp exp1, doExp exp2)
  | doExp (F.HandleExp { body, exnName, handler }) = F.HandleExp { body = doExp body
                                                                 , exnName = exnName
                                                                 , handler = doExp handler
                                                                 }
  | doExp (F.IfThenElseExp (exp1, exp2, exp3)) = F.IfThenElseExp (doExp exp1, doExp exp2, doExp exp3)
  | doExp (F.CaseExp { sourceSpan, subjectExp, subjectTy, matches, matchType, resultTy }) = F.CaseExp { sourceSpan = sourceSpan, subjectExp = doExp subjectExp, subjectTy = subjectTy, matches = List.map (fn (pat, exp) => (pat, doExp exp)) matches, matchType = matchType, resultTy = resultTy }
  | doExp (F.FnExp (vid, ty, exp)) = F.FnExp (vid, ty, doExp exp)
  | doExp (F.ProjectionExp { label, record, fieldTypes }) = F.ProjectionExp { label = label, record = doExp record, fieldTypes = fieldTypes }
  | doExp (F.TyAbsExp (tv, kind, exp)) = F.TyAbsExp (tv, kind, doExp exp)
  | doExp (F.TyAppExp (exp, ty)) = F.TyAppExp (doExp exp, ty)
  | doExp (F.PackExp { payloadTy, exp, packageTy }) = F.PackExp { payloadTy = payloadTy, exp = doExp exp, packageTy = packageTy }
  | doExp (e as F.BogusExp _) = e
  | doExp (e as F.ExitProgram) = e
  | doExp (F.ExportValue exp) = F.ExportValue (doExp exp)
  | doExp (F.ExportModule entities) = F.ExportModule (Vector.map (fn (name, exp) => (name, doExp exp)) entities)
and doDec (F.ValDec (vid, optTy, exp)) = [F.ValDec (vid, optTy, doExp exp)]
  | doDec (F.RecValDec valbinds)
    = let val bound = List.foldl (fn ((vid, _, _), set) => TypedSyntax.VIdSet.add (set, vid)) TypedSyntax.VIdSet.empty valbinds
          val map = List.foldl (fn (def as (vid, _, exp), map) =>
                                   TypedSyntax.VIdMap.insert (map, vid, { def = def
                                                                        , dests = TypedSyntax.VIdSet.intersection (F.freeVarsInExp (TypedSyntax.VIdSet.empty, exp) TypedSyntax.VIdSet.empty, bound)
                                                                        }
                                                             )
                               ) TypedSyntax.VIdMap.empty valbinds
          val sccs = TypedSyntax.VIdSCC.components (#dests, map)
      in List.foldr (fn (scc, decs) => let val dec = case TypedSyntax.VIdSet.listItems scc of
                                                         [vid] => let val { def as (vid, ty, exp), dests } = TypedSyntax.VIdMap.lookup (map, vid)
                                                                  in if TypedSyntax.VIdSet.member (dests, vid) then
                                                                         F.RecValDec [def]
                                                                     else
                                                                         F.ValDec (vid, SOME ty, exp)
                                                                  end
                                                       | scc => F.RecValDec (List.map (fn vid => #def (TypedSyntax.VIdMap.lookup (map, vid))) scc)
                                       in dec :: decs
                                       end
                    ) [] sccs
      end
  | doDec (F.UnpackDec (tv, kind, vid, ty, exp)) = [F.UnpackDec (tv, kind, vid, ty, doExp exp)]
  | doDec (F.IgnoreDec exp) = [F.IgnoreDec (doExp exp)]
  | doDec (F.DatatypeDec datbinds) = [F.DatatypeDec datbinds]
  | doDec (F.ExceptionDec names) = [F.ExceptionDec names]
  | doDec (dec as F.ESImportDec _) = [dec]
(* and doDecs decs = List.foldr (fn (dec, rest) => doDec dec @ rest) [] decs *)
end

structure DeadCodeElimination :> sig
              val doExp : FSyntax.Exp -> TypedSyntax.VIdSet.set -> TypedSyntax.VIdSet.set * FSyntax.Exp
          end = struct
structure F = FSyntax
fun isDiscardablePrimOp (F.IntConstOp _) = true
  | isDiscardablePrimOp (F.WordConstOp _) = true
  | isDiscardablePrimOp (F.RealConstOp _) = true
  | isDiscardablePrimOp (F.Char8ConstOp _) = true
  | isDiscardablePrimOp (F.Char16ConstOp _) = true
  | isDiscardablePrimOp (F.String8ConstOp _) = true
  | isDiscardablePrimOp (F.String16ConstOp _) = true
  | isDiscardablePrimOp (F.RaiseOp _) = false
  | isDiscardablePrimOp F.ListOp = true
  | isDiscardablePrimOp F.VectorOp = true
  | isDiscardablePrimOp (F.DataTagAsStringOp _) = true
  | isDiscardablePrimOp (F.DataTagAsString16Op _) = true
  | isDiscardablePrimOp (F.DataPayloadOp _) = true
  | isDiscardablePrimOp F.ExnPayloadOp = true
  | isDiscardablePrimOp (F.ConstructValOp _) = true
  | isDiscardablePrimOp (F.ConstructValWithPayloadOp _) = true
  | isDiscardablePrimOp F.ConstructExnOp = true
  | isDiscardablePrimOp F.ConstructExnWithPayloadOp = true
  | isDiscardablePrimOp (F.PrimCall p) = Primitives.isDiscardable p
  | isDiscardablePrimOp F.JsCallOp = false
  | isDiscardablePrimOp F.JsMethodOp = false
  | isDiscardablePrimOp F.JsNewOp = false
  | isDiscardablePrimOp F.LuaCallOp = false
  | isDiscardablePrimOp F.LuaCall1Op = false
  | isDiscardablePrimOp (F.LuaMethodOp _) = false
  | isDiscardablePrimOp (F.LuaMethod1Op _) = false
fun isDiscardable (F.PrimExp (primOp, _, args)) = isDiscardablePrimOp primOp andalso List.all isDiscardable args
  | isDiscardable (F.VarExp _) = true
  | isDiscardable (F.RecordExp fields) = List.all (fn (_, exp) => isDiscardable exp) fields
  | isDiscardable (F.LetExp (_, _)) = false (* TODO *)
  | isDiscardable (F.AppExp (_, _)) = false (* TODO *)
  | isDiscardable (F.HandleExp { body = _, exnName = _, handler = _ }) = false (* TODO *)
  | isDiscardable (F.IfThenElseExp (exp1, exp2, exp3)) = isDiscardable exp1 andalso isDiscardable exp2 andalso isDiscardable exp3
  | isDiscardable (F.CaseExp { sourceSpan = _, subjectExp = _, subjectTy = _, matches = _, matchType = _, resultTy = _ }) = false (* TODO *)
  | isDiscardable (F.FnExp (_, _, _)) = true
  | isDiscardable (F.ProjectionExp { label = _, record, fieldTypes = _ }) = isDiscardable record
  | isDiscardable (F.TyAbsExp (_, _, exp)) = isDiscardable exp
  | isDiscardable (F.TyAppExp (exp, _)) = isDiscardable exp
  | isDiscardable (F.PackExp { payloadTy = _, exp, packageTy = _ }) = isDiscardable exp
  | isDiscardable (F.BogusExp _) = false
  | isDiscardable F.ExitProgram = false
  | isDiscardable (F.ExportValue _) = false
  | isDiscardable (F.ExportModule _) = false
(*:
val doPat : F.Pat -> TypedSyntax.VIdSet.set -> (* constructors used *) TypedSyntax.VIdSet.set
and doExp : F.Exp -> TypedSyntax.VIdSet.set -> TypedSyntax.VIdSet.set * F.Exp
and doIgnoredExp : F.Exp -> TypedSyntax.VIdSet.set -> TypedSyntax.VIdSet.set * F.Exp list
and doDec : TypedSyntax.VIdSet.set * F.Dec -> TypedSyntax.VIdSet.set * F.Dec list
(* and doDecs : TypedSyntax.VIdSet.set * F.Dec list -> TypedSyntax.VIdSet.set * F.Dec list *)
 *)
fun doPat (F.WildcardPat _) acc = acc
  | doPat (F.SConPat { sourceSpan = _, scon = _, equality, cookedValue }) acc = #1 (doExp equality (#1 (doExp cookedValue acc)))
  | doPat (F.VarPat _) acc = acc
  | doPat (F.RecordPat { sourceSpan = _, fields, ellipsis, allFields = _ }) acc = List.foldl (fn ((_, pat), acc) => doPat pat acc) (case ellipsis of NONE => acc | SOME basePat => doPat basePat acc) fields
  | doPat (F.ValConPat { sourceSpan = _, info = _, payload = NONE }) acc = acc
  | doPat (F.ValConPat { sourceSpan = _, info = _, payload = SOME (_, payloadPat) }) acc = doPat payloadPat acc
  | doPat (F.ExnConPat { sourceSpan = _, tagPath, payload = NONE }) acc = #1 (doExp tagPath acc)
  | doPat (F.ExnConPat { sourceSpan = _, tagPath, payload = SOME (_, payloadPat) }) acc = doPat payloadPat (#1 (doExp tagPath acc))
  | doPat (F.LayeredPat (_, _, _, innerPat)) acc = doPat innerPat acc
  | doPat (F.VectorPat (_, pats, _, _)) acc = Vector.foldl (fn (pat, acc) => doPat pat acc) acc pats
and doExp (F.PrimExp (primOp, tyargs, args) : F.Exp) acc : TypedSyntax.VIdSet.set * F.Exp
    = let val (acc, args') = List.foldr (fn (x, (acc, xs)) => let val (acc, x) = doExp x acc in (acc, x :: xs) end) (acc, []) args
      in (acc, F.PrimExp (primOp, tyargs, args'))
      end
  | doExp (exp as F.VarExp vid) acc = (TypedSyntax.VIdSet.add (acc, vid), exp)
  | doExp (F.RecordExp fields) acc = let val (acc, fields) = List.foldr (fn ((label, exp), (acc, xs)) => let val (acc, exp) = doExp exp acc in (acc, (label, exp) :: xs) end) (acc, []) fields
                                     in (acc, F.RecordExp fields)
                                     end
  | doExp (F.LetExp (decs, exp)) acc = let val (used, exp) = doExp exp TypedSyntax.VIdSet.empty
                                           val (used', decs) = List.foldr (fn (dec, (used, decs)) => let val (used', decs') = doDec (used, dec)
                                                                                                     in (used', decs' @ decs)
                                                                                                     end) (used, []) decs
                                       in (TypedSyntax.VIdSet.union (acc, used'), F.LetExp (decs, exp))
                                       end
  | doExp (F.AppExp (exp1, exp2)) acc = let val (used, exp1) = doExp exp1 acc
                                            val (used', exp2) = doExp exp2 used
                                        in (used', F.AppExp (exp1, exp2))
                                        end
  | doExp (F.HandleExp { body, exnName, handler }) acc = let val (used, handler) = doExp handler TypedSyntax.VIdSet.empty
                                                             val used = TypedSyntax.VIdSet.subtract (used, exnName)
                                                             val (used', body) = doExp body (TypedSyntax.VIdSet.union (acc, used))
                                                         in (used', F.HandleExp { body = body, exnName = exnName, handler = handler })
                                                         end
  | doExp (F.IfThenElseExp (exp1, exp2, exp3)) acc = let val (used1, exp1) = doExp exp1 acc
                                                         val (used2, exp2) = doExp exp2 used1
                                                         val (used3, exp3) = doExp exp3 used2
                                                     in (used3, F.IfThenElseExp (exp1, exp2, exp3))
                                                     end
  | doExp (F.CaseExp { sourceSpan, subjectExp, subjectTy, matches, matchType, resultTy }) acc
    = let val (used, subjectExp) = doExp subjectExp acc
          val (_, matches) = List.foldr (fn ((pat, exp), (used, matches)) => let val (used', exp) = doExp exp used
                                                                             in (doPat pat used', (pat, exp) :: matches)
                                                                             end)
                                        (used, []) matches
      in (used, F.CaseExp { sourceSpan = sourceSpan, subjectExp = subjectExp, subjectTy = subjectTy, matches = matches, matchType = matchType, resultTy = resultTy })
      end
  | doExp (F.FnExp (vid, ty, exp)) acc = let val (used, exp) = doExp exp acc
                                         in (used, F.FnExp (vid, ty, exp))
                                         end
  | doExp (F.ProjectionExp { label, record, fieldTypes }) acc = let val (used, exp) = doExp record acc
                                                                in (used, F.ProjectionExp { label = label, record = exp, fieldTypes = fieldTypes })
                                                                end
  | doExp (F.TyAbsExp (tyvar, kind, exp)) acc = let val (used, exp) = doExp exp acc
                                                in (used, F.TyAbsExp (tyvar, kind, exp))
                                                end
  | doExp (F.TyAppExp (exp, ty)) acc = let val (used, exp) = doExp exp acc
                                       in (used, F.TyAppExp (exp, ty))
                                       end
  | doExp (F.PackExp { payloadTy, exp, packageTy }) acc = let val (used, exp) = doExp exp acc
                                                          in (used, F.PackExp { payloadTy = payloadTy, exp = exp, packageTy = packageTy })
                                                          end
  | doExp (exp as F.BogusExp _) acc = (acc, exp)
  | doExp (exp as F.ExitProgram) acc = (acc, exp)
  | doExp (F.ExportValue exp) acc = let val (used, exp) = doExp exp acc
                                    in (used, F.ExportValue exp)
                                    end
  | doExp (F.ExportModule entities) acc = let val (acc, entities') = Vector.foldr (fn ((name, exp), (acc, xs)) => let val (acc, exp) = doExp exp acc in (acc, (name, exp) :: xs) end) (acc, []) entities
                                          in (acc, F.ExportModule (Vector.fromList entities'))
                                          end
and doIgnoredExpAsExp exp acc = let val (used, exps) = doIgnoredExp exp acc
                                in (used, F.LetExp (List.map F.IgnoreDec exps, F.RecordExp []))
                                end
and doIgnoredExp (exp as F.PrimExp (primOp, _, args)) acc
    = if isDiscardablePrimOp primOp then
          List.foldr (fn (x, (acc, xs)) => let val (acc, ys) = doIgnoredExp x acc in (acc, ys @ xs) end) (acc, []) args
      else
          let val (used, exp) = doExp exp acc
          in (used, [exp])
          end
  | doIgnoredExp (F.VarExp _) acc = (acc, [])
  | doIgnoredExp (F.RecordExp fields) acc = List.foldr (fn ((_, exp), (acc, xs)) => let val (acc, ys) = doIgnoredExp exp acc in (acc, ys @ xs) end) (acc, []) fields
  | doIgnoredExp (F.LetExp (decs, exp)) acc = let val (used, exp) = doIgnoredExpAsExp exp TypedSyntax.VIdSet.empty
                                                  val (used, decs) = List.foldr (fn (dec, (used, decs)) => let val (used, decs') = doDec (used, dec)
                                                                                                           in (used, decs' @ decs)
                                                                                                           end) (used, []) decs
                                              in case (decs, exp) of
                                                     ([], F.RecordExp []) => (TypedSyntax.VIdSet.union (acc, used), [])
                                                   | _ => (TypedSyntax.VIdSet.union (acc, used), [F.LetExp (decs, exp)])
                                              end
  | doIgnoredExp (F.AppExp (exp1, exp2)) acc = let val (used1, exp1) = doExp exp1 acc
                                                   val (used2, exp2) = doExp exp2 used1
                                               in (used2, [F.AppExp (exp1, exp2)])
                                               end
  | doIgnoredExp (F.HandleExp { body, exnName, handler }) acc = let val (used2, handler) = doIgnoredExpAsExp handler TypedSyntax.VIdSet.empty
                                                                    val used2 = TypedSyntax.VIdSet.subtract (used2, exnName)
                                                                    val (used1, body) = doIgnoredExpAsExp body (TypedSyntax.VIdSet.union (acc, used2))
                                                                in case body of
                                                                       F.RecordExp [] => (used1, [])
                                                                     | _ => (used1, [F.HandleExp { body = body, exnName = exnName, handler = handler }])
                                                                end
  | doIgnoredExp (F.IfThenElseExp (exp1, exp2, exp3)) acc = let val (used2, exp2) = doIgnoredExpAsExp exp2 acc
                                                                val (used3, exp3) = doIgnoredExpAsExp exp3 used2
                                                            in case (exp2, exp3) of
                                                                   (F.RecordExp [], F.RecordExp []) => doIgnoredExp exp1 used3
                                                                 | (exp2, exp3) => let val (used1, exp1) = doExp exp1 used3
                                                                                   in (used1, [F.IfThenElseExp (exp1, exp2, exp3)])
                                                                                   end
                                                            end
  | doIgnoredExp (F.CaseExp { sourceSpan, subjectExp, subjectTy, matches, matchType, resultTy }) acc
    = let val (used, subjectExp) = doExp subjectExp acc
          val (used, matches) = List.foldr (fn ((pat, exp), (used, matches)) => let val (used', exp) = doIgnoredExpAsExp exp used
                                                                                    val used'' = doPat pat used'
                                                                                in (used'', (pat, exp) :: matches)
                                                                                end)
                                           (used, []) matches
      in (used, [F.CaseExp { sourceSpan = sourceSpan, subjectExp = subjectExp, subjectTy = subjectTy, matches = matches, matchType = matchType, resultTy = resultTy }])
      end
  | doIgnoredExp (F.FnExp _) acc = (acc, [])
  | doIgnoredExp (F.ProjectionExp { label = _, record, fieldTypes = _ }) acc = doIgnoredExp record acc
  | doIgnoredExp (F.TyAbsExp (tyvar, kind, exp)) acc = let val (used, exp) = doIgnoredExpAsExp exp acc (* should be pure *)
                                                       in case exp of
                                                              F.RecordExp [] => (used, [])
                                                            | exp => (used, [F.TyAbsExp (tyvar, kind, exp)])
                                                       end
  | doIgnoredExp (F.TyAppExp (exp, ty)) acc = let val (used, exp) = doIgnoredExpAsExp exp acc
                                              in case exp of
                                                     F.RecordExp [] => (used, [])
                                                   | exp => (used, [F.TyAppExp (exp, ty)])
                                              end
  | doIgnoredExp (F.PackExp { payloadTy, exp, packageTy }) acc = let val (used, exp) = doIgnoredExpAsExp exp acc
                                                                 in case exp of
                                                                        F.RecordExp [] => (used, [])
                                                                      | exp => (used, [F.PackExp { payloadTy = payloadTy, exp = exp, packageTy = packageTy }])
                                                                 end
  | doIgnoredExp (exp as F.BogusExp _) acc = (acc, [exp])
  | doIgnoredExp (exp as F.ExitProgram) acc = (acc, [exp])
  | doIgnoredExp (F.ExportValue exp) acc = let val (used, exp) = doExp exp acc
                                           in (used, [F.ExportValue exp])
                                           end
  | doIgnoredExp (F.ExportModule entities) acc = let val (acc, entities') = Vector.foldr (fn ((name, exp), (acc, xs)) => let val (acc, exp) = doExp exp acc in (acc, (name, exp) :: xs) end) (acc, []) entities
                                                 in (acc, [F.ExportModule (Vector.fromList entities')])
                                                 end
and doDec (used : TypedSyntax.VIdSet.set, F.ValDec (vid, optTy, exp)) : TypedSyntax.VIdSet.set * F.Dec list
    = if not (TypedSyntax.VIdSet.member (used, vid)) then
          if isDiscardable exp then
              (used, [])
          else
              let val (used', exps) = doIgnoredExp exp used
              in (used', List.map F.IgnoreDec exps)
              end
      else
          let val (used', exp') = doExp exp used
          in (used', [F.ValDec (vid, optTy, exp')])
          end
  | doDec (used, F.RecValDec valbinds)
    = let val bound = List.foldl (fn ((vid, _, _), acc) => TypedSyntax.VIdSet.add (acc, vid)) TypedSyntax.VIdSet.empty valbinds
      in if TypedSyntax.VIdSet.disjoint (used, bound) then
             (used, []) (* RHS should be fn _ => _, and therefore discardable *)
         else
             let val (used, valbinds) = List.foldr (fn ((vid, ty, exp), (used, valbinds)) => let val (used', exp) = doExp exp used
                                                                                             in (used', (vid, ty, exp) :: valbinds)
                                                                                             end
                                                   ) (used, []) valbinds
             in (used, [F.RecValDec valbinds])
             end
      end
  | doDec (used, F.UnpackDec (tv, kind, vid, ty, exp))
    = if not (TypedSyntax.VIdSet.member (used, vid)) then
          if isDiscardable exp then
              (used, [])
          else
              let val (used', exps) = doIgnoredExp exp used
              in (used', List.map F.IgnoreDec exps)
              end
      else
          let val (used', exp') = doExp exp used
          in (used', [F.UnpackDec (tv, kind, vid, ty, exp')])
          end
  | doDec (used, F.IgnoreDec exp) = let val (used', exps) = doIgnoredExp exp used
                                    in (used', List.map F.IgnoreDec exps)
                                    end
  | doDec (used, dec as F.DatatypeDec _) = (used, [dec]) (* TODO *)
  | doDec (used, dec as F.ExceptionDec { name = _, tagName, payloadTy = _ }) = if TypedSyntax.VIdSet.member (used, tagName) then
                                                                                   (used, [dec])
                                                                               else
                                                                                   (used, [])
  | doDec (used, F.ESImportDec { pure, specs, moduleName })
    = let val specs = List.filter (fn (_, vid, _) => TypedSyntax.VIdSet.member (used, vid)) specs
      in (used, if pure andalso List.null specs then [] else [F.ESImportDec { pure = pure, specs = specs, moduleName = moduleName }])
      end
(* and doDecs (used, decs) = List.foldr (fn (dec, (used, decs)) => let val (used, dec) = doDec (used, dec)
                                                                in (used, dec @ decs)
                                                                end) (used, []) decs *)
(* and definedInDecs decs acc = List.foldl (fn (dec, s) => definedInDec dec s) acc decs *)
(* and definedInDec (F.ValDec (vid, _, _)) acc = TypedSyntax.VIdSet.add (acc, vid)
  | definedInDec (F.RecValDec valbinds) acc = List.foldl (fn ((vid, _, _), s) => TypedSyntax.VIdSet.add (s, vid)) acc valbinds
  | definedInDec (F.UnpackDec (_, _, vid, _, _)) acc = TypedSyntax.VIdSet.add (acc, vid)
  | definedInDec (F.IgnoreDec _) acc = acc
  | definedInDec (F.DatatypeDec datbinds) acc = List.foldl (fn (F.DatBind (_, _, conbinds), s) => List.foldl (fn (F.ConBind (vid, _), s) => TypedSyntax.VIdSet.add (s, vid)) s conbinds) acc datbinds
  | definedInDec (F.ExceptionDec { name = _, tagName, payloadTy = _ }) acc = TypedSyntax.VIdSet.add (acc, tagName)
  | definedInDec (F.ESImportDec { pure = _, specs, moduleName = _ }) acc = List.foldl (fn ((_, vid, _), acc) => TypedSyntax.VIdSet.add (acc, vid)) acc specs *)
end; (* structure DeadCodeElimination *)
