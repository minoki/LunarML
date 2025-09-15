(*
 * Copyright (c) 2025 ARATA Mizuki
 * This file is part of LunarML.
 *)
(* This module implements type checking for the CPS IR. *)
structure CpsCheck :>
sig
  exception TypeError of string
  type Env =
    { valEnv: FSyntax.Ty TypedSyntax.VIdMap.map
    , tyEnv: FSyntax.Kind TypedSyntax.TyVarMap.map
    , contEnv: (FSyntax.Ty list) CSyntax.CVarMap.map
    }
  val checkCompatibility: (unit -> string) * FSyntax.Ty -> FSyntax.Ty -> unit
  val inferValue: Env -> CSyntax.Value -> FSyntax.Ty
  val inferSimpleExp: Env * CSyntax.SimpleExp -> FSyntax.Ty list
  val checkDec: CSyntax.Dec * Env -> Env
  val checkStat: Env * CSyntax.Stat -> unit
end =
struct
  structure F = FSyntax
  structure C = CSyntax
  exception TypeError of string
  type Env =
    { valEnv: FSyntax.Ty TypedSyntax.VIdMap.map
    , tyEnv: FSyntax.Kind TypedSyntax.TyVarMap.map
    , contEnv: (FSyntax.Ty list) CSyntax.CVarMap.map
    }
  fun checkCompatibility (comment, expectedTy) =
    CheckF.checkSame (TypedSyntax.TyVarMap.empty, comment, expectedTy)
  val checkKind = CheckF.checkKind
  val tv = TypedSyntax.MkTyVar ("a", 0)
  fun inferValue (env as {valEnv, tyEnv, contEnv = _}) =
    fn C.Var v =>
      (case TypedSyntax.VIdMap.find (valEnv, v) of
         NONE =>
           raise TypeError ("undefined variable: " ^ TypedSyntax.print_VId v)
       | SOME ty => ty)
     | C.Unit => FSyntax.Types.unit
     | C.Nil =>
      FSyntax.ForallType
        (tv, FSyntax.TypeKind, FSyntax.Types.list (FSyntax.TyVar tv))
     | C.TypedNil ty => FSyntax.Types.list ty
     | C.BoolConst _ => FSyntax.Types.bool
     | C.IntConst (Primitives.INT, _) => FSyntax.Types.int
     | C.IntConst (Primitives.I32, _) => FSyntax.Types.int32
     | C.IntConst (Primitives.I54, _) => FSyntax.Types.int54
     | C.IntConst (Primitives.I64, _) => FSyntax.Types.int64
     | C.IntConst (Primitives.INT_INF, _) => FSyntax.Types.intInf
     | C.WordConst (Primitives.WORD, _) => FSyntax.Types.word
     | C.WordConst (Primitives.W32, _) => FSyntax.Types.word32
     | C.WordConst (Primitives.W64, _) => FSyntax.Types.word64
     | C.CharConst _ => FSyntax.Types.char
     | C.Char16Const _ => FSyntax.Types.char16
     | C.StringConst _ => FSyntax.Types.string
     | C.String16Const _ => FSyntax.Types.string16
     | C.PrimEffect _ => FSyntax.Types.prim_effect
     | C.Cast {value, from, to} =>
      (checkValue (env, fn () => "Cast", from) value; to)
     | C.Pack {value, payloadTy, packageTy} =>
      let
        val packageTy = FSyntax.weakNormalizeTy packageTy
        val expectedTy =
          case packageTy of
            F.ExistsType (tv, kind, ty) =>
              ( checkKind (tyEnv, kind) payloadTy
              ; FSyntax.substituteTy (tv, payloadTy) ty
              )
          | _ =>
              raise TypeError
                ("invalid Pack: packageTy = "
                 ^ Printer.build (FPrinter.doTy 0 packageTy))
      in
        checkValue (env, fn () => "Pack", expectedTy) value;
        packageTy
      end
  and checkValue (env, s, expectedTy) v =
    checkCompatibility (s, expectedTy) (inferValue env v)
  fun inferSimpleExp (env, C.PrimOp {primOp, tyargs, args}) =
        (case primOp of
           F.PrimCall primOp =>
             let
               val {vars, args = argTypes, results} =
                 CheckF.TypeOfPrimitives.typeOf primOp
               val subst =
                 ListPair.foldlEq
                   (fn ((tv, ()), ty, acc) =>
                      TypedSyntax.TyVarMap.insert (acc, tv, ty))
                   TypedSyntax.TyVarMap.empty (vars, tyargs)
                 handle ListPair.UnequalLengths =>
                   raise TypeError
                     ("number of type variables differ: "
                      ^ Primitives.toString primOp ^ " ["
                      ^
                      String.concatWith ", "
                        (List.map (fn ty => Printer.build (FPrinter.doTy 0 ty))
                           tyargs) ^ "]")
             in
               ListPair.appEq
                 (fn (e, a) =>
                    checkValue (env, fn () => Primitives.toString primOp, e) a)
                 ( Vector.foldr
                     (fn (a, acc) => #doTy (F.lazySubstTy subst) a :: acc) []
                     argTypes
                 , args
                 )
               handle ListPair.UnequalLengths =>
                 raise TypeError
                   ("number of arguments mismatch: "
                    ^ Primitives.toString primOp);
               List.map (#doTy (F.lazySubstTy subst)) results
             end
         | F.IntConstOp _ => raise TypeError "unexpected IntConstOp"
         | F.WordConstOp _ => raise TypeError "unexpected WordConstOp"
         | F.Char8ConstOp _ => raise TypeError "unexpected Char8ConstOp"
         | F.Char16ConstOp _ => raise TypeError "unexpected Char16ConstOp"
         | F.String8ConstOp _ => raise TypeError "unexpected String8ConstOp"
         | F.String16ConstOp _ => raise TypeError "unexpected String16ConstOp"
         | F.RealConstOp _ => [F.Types.real]
         | F.RaiseOp _ => raise TypeError "unexpected RaiseOp"
         | F.ListOp =>
             (case tyargs of
                [elemTy] =>
                  ( List.app (checkValue (env, fn () => "list element", elemTy))
                      args
                  ; [F.Types.list elemTy]
                  )
              | _ => raise TypeError "invalid element type for ListOp")
         | F.VectorOp =>
             (case tyargs of
                [elemTy] =>
                  ( List.app
                      (checkValue (env, fn () => "vector element", elemTy)) args
                  ; [F.Types.vector elemTy]
                  )
              | _ => raise TypeError "invalid element type for VectorOp")
         | F.DataTagAsStringOp _ =>
             (case (tyargs, args) of
                ([dataTy], [data]) =>
                  ( checkValue (env, fn () => "DataTagAsStringOp", dataTy) data
                  ; [F.Types.string]
                  )
              | _ => raise TypeError "invalid DataTagAsString")
         | F.DataTagAsString16Op _ =>
             (case (tyargs, args) of
                ([dataTy], [data]) =>
                  ( checkValue (env, fn () => "DataTagAsString16Op", dataTy)
                      data
                  ; [F.Types.string16]
                  )
              | _ => raise TypeError "invalid DataTagAsString16")
         | F.DataPayloadOp _ =>
             (case (tyargs, args) of
                ([dataTy, payloadTy], [data]) =>
                  ( checkValue (env, fn () => "DataPayloadOp", dataTy) data
                  ; [payloadTy]
                  )
              | _ => raise TypeError "invalid DataPayloadOp")
         | F.ExnPayloadOp =>
             (case (tyargs, args) of
                ([payloadTy], [data]) =>
                  ( checkValue (env, fn () => "ExnPayloadOp", F.Types.exn) data
                  ; [payloadTy]
                  )
              | _ => raise TypeError "invalid ExnPayloadOp")
         | F.ConstructValOp _ =>
             (case (tyargs, args) of
                ([dataTy], []) => [dataTy]
              | _ => raise TypeError "invalid ConstructValOp")
         | F.ConstructValWithPayloadOp _ =>
             (case (tyargs, args) of
                ([dataTy, payloadTy], [payload]) =>
                  ( checkValue
                      (env, fn () => "ConstructValWithPayloadOp", payloadTy)
                      payload
                  ; [dataTy]
                  )
              | _ => raise TypeError "invalid ConstructValWithPayloadOp")
         | F.ConstructExnOp =>
             (case (tyargs, args) of
                ([], [tag]) =>
                  ( checkValue (env, fn () => "ConstructExnOp", F.Types.exntag)
                      tag
                  ; [F.Types.exn]
                  )
              | _ => raise TypeError "invalid ConstructExnOp")
         | F.ConstructExnWithPayloadOp =>
             (case (tyargs, args) of
                ([payloadTy], [tag, payload]) =>
                  ( checkValue (env, fn () => "ConstructExnOp", F.Types.exntag)
                      tag
                  ; checkValue
                      (env, fn () => "ConstructValWithPayloadOp", payloadTy)
                      payload
                  ; [F.Types.exn]
                  )
              | _ => raise TypeError "invalid ConstructExnWithPayloadOp")
         | F.JsCallOp =>
             (case (tyargs, args) of
                ([], e :: f :: args) =>
                  ( checkValue (env, fn () => "JsCallOp", F.Types.prim_effect) e
                  ; checkValue (env, fn () => "JsCallOp", F.Types.js_value) f
                  ; List.app
                      (checkValue (env, fn () => "JsCallOp", F.Types.js_value))
                      args
                  ; [F.Types.js_value]
                  )
              | _ => raise TypeError "invalid JsCallOp")
         | F.JsMethodOp =>
             (case (tyargs, args) of
                ([], e :: f :: name :: args) =>
                  ( checkValue (env, fn () => "JsMethodOp", F.Types.prim_effect)
                      e
                  ; checkValue (env, fn () => "JsMethodOp", F.Types.js_value) f
                  ; checkValue (env, fn () => "JsMethodOp", F.Types.string16)
                      name
                  ; List.app
                      (checkValue (env, fn () => "JsMethodOp", F.Types.js_value))
                      args
                  ; [F.Types.js_value]
                  )
              | _ => raise TypeError "invalid JsMethodOp")
         | F.JsNewOp =>
             (case (tyargs, args) of
                ([], e :: f :: args) =>
                  ( checkValue (env, fn () => "JsNewOp", F.Types.prim_effect) e
                  ; checkValue (env, fn () => "JsNewOp", F.Types.js_value) f
                  ; List.app
                      (checkValue (env, fn () => "JsNewOp", F.Types.js_value))
                      args
                  ; [F.Types.js_value]
                  )
              | _ => raise TypeError "invalid JsNewOp")
         | F.LuaCallOp =>
             (case (tyargs, args) of
                ([], e :: f :: args) =>
                  ( checkValue (env, fn () => "LuaCallOp", F.Types.prim_effect)
                      e
                  ; checkValue (env, fn () => "LuaCallOp", F.Types.lua_value) f
                  ; List.app
                      (checkValue (env, fn () => "LuaCallOp", F.Types.lua_value))
                      args
                  ; [F.Types.vector F.Types.lua_value]
                  )
              | _ => raise TypeError "invalid LuaCallOp")
         | F.LuaCall1Op =>
             (case (tyargs, args) of
                ([], e :: f :: args) =>
                  ( checkValue (env, fn () => "LuaCall1Op", F.Types.prim_effect)
                      e
                  ; checkValue (env, fn () => "LuaCall1Op", F.Types.lua_value) f
                  ; List.app
                      (checkValue
                         (env, fn () => "LuaCall1Op", F.Types.lua_value)) args
                  ; [F.Types.lua_value]
                  )
              | _ => raise TypeError "invalid LuaCall1Op")
         | F.LuaCallNOp n =>
             (case (tyargs, args) of
                ([], e :: f :: args) =>
                  ( checkValue (env, fn () => "LuaCallNOp", F.Types.prim_effect)
                      e
                  ; checkValue (env, fn () => "LuaCallNOp", F.Types.lua_value) f
                  ; List.app
                      (checkValue
                         (env, fn () => "LuaCallNOp", F.Types.lua_value)) args
                  ; List.tabulate (n, fn _ => F.Types.lua_value)
                  )
              | _ => raise TypeError "invalid LuaCallNOp")
         | F.LuaMethodOp _ =>
             (case (tyargs, args) of
                ([], e :: f :: args) =>
                  ( checkValue
                      (env, fn () => "LuaMethodOp", F.Types.prim_effect) e
                  ; checkValue (env, fn () => "LuaMethodOp", F.Types.lua_value)
                      f
                  ; List.app
                      (checkValue
                         (env, fn () => "LuaMethodOp", F.Types.lua_value)) args
                  ; [F.Types.vector F.Types.lua_value]
                  )
              | _ => raise TypeError "invalid LuaMethodOp")
         | F.LuaMethod1Op _ =>
             (case (tyargs, args) of
                ([], e :: f :: args) =>
                  ( checkValue
                      (env, fn () => "LuaMethod1Op", F.Types.prim_effect) e
                  ; checkValue (env, fn () => "LuaMethod1Op", F.Types.lua_value)
                      f
                  ; List.app
                      (checkValue
                         (env, fn () => "LuaMethod1Op", F.Types.lua_value)) args
                  ; [F.Types.lua_value]
                  )
              | _ => raise TypeError "invalid LuaMethod1Op")
         | F.LuaMethodNOp (_, n) =>
             (case (tyargs, args) of
                ([], e :: f :: args) =>
                  ( checkValue
                      (env, fn () => "LuaMethodNOp", F.Types.prim_effect) e
                  ; checkValue (env, fn () => "LuaMethodNOp", F.Types.lua_value)
                      f
                  ; List.app
                      (checkValue
                         (env, fn () => "LuaMethodNOp", F.Types.lua_value)) args
                  ; List.tabulate (n, fn _ => F.Types.lua_value)
                  )
              | _ => raise TypeError "invalid LuaMethodNOp"))
    | inferSimpleExp (env, C.Record fields) =
        [F.RecordType (Syntax.LabelMap.map (inferValue env) fields)]
    | inferSimpleExp (env as {tyEnv, ...}, C.ExnTag {name = _, payloadTy}) =
        (Option.app (checkKind (tyEnv, F.TypeKind)) payloadTy; [F.Types.exntag])
    | inferSimpleExp (env, C.Projection {label, record, fieldTypes}) =
        (case F.weakNormalizeTy (inferValue env record) of (* TODO: check fieldTypes *)
           recordTy as F.RecordType fields =>
             (case Syntax.LabelMap.find (fields, label) of
                SOME ty => [ty]
              | NONE =>
                  raise Fail
                    ("record label not found: record="
                     ^ Printer.build (FPrinter.doTy 0 recordTy) ^ ", label="
                     ^ Syntax.print_Label label))
         | anyTy as F.AnyType F.TypeKind => [anyTy]
         | recordTy =>
             raise Fail
               ("invalid record type for projection: "
                ^ Printer.build (FPrinter.doTy 0 recordTy) ^ ", label="
                ^ Syntax.print_Label label))
    | inferSimpleExp
        ( {valEnv, tyEnv, contEnv}
        , C.Abs
            { contParam
            , tyParams
            , params
            , body
            , resultTy
            , attr = {typeOnly, ...}
            }
        ) =
        let
          val tyEnv =
            List.foldl
              (fn ((tv, kind), e) => TypedSyntax.TyVarMap.insert (e, tv, kind))
              tyEnv tyParams
          val contEnv = C.CVarMap.insert (contEnv, contParam, [resultTy])
          val valEnv =
            List.foldl
              (fn ((v, ty), ve) => TypedSyntax.VIdMap.insert (ve, v, ty)) valEnv
              params
          val fnTy =
            case (params, typeOnly) of
              ([], true) => resultTy
            | (_ :: _, true) => raise TypeError "invalid type abstraction"
            | (_, false) => F.MultiFnType (List.map #2 params, resultTy)
          val absTy =
            List.foldr (fn ((tv, kind), ty) => F.ForallType (tv, kind, ty)) fnTy
              tyParams
        in
          (* print ("Abs: " ^ Printer.build (FPrinter.doTy 0 absTy) ^ "\n"); *)
          checkStat ({valEnv = valEnv, tyEnv = tyEnv, contEnv = contEnv}, body);
          [absTy]
        end
  and checkDec (dec, env as {valEnv, tyEnv, contEnv}) =
    (case dec of
       C.ValDec {exp, results} =>
         let
           val actualTy = inferSimpleExp (env, exp)
         in
           ListPair.appEq
             (fn ((_, e), a) =>
                checkCompatibility
                  (fn () => "ValDec " ^ C.simpleExpToString exp, e) a)
             (results, actualTy);
           { valEnv =
               List.foldl
                 (fn ((SOME v, ty), e) => TypedSyntax.VIdMap.insert (e, v, ty)
                   | ((NONE, _), e) => e) valEnv results
           , tyEnv = tyEnv
           , contEnv = contEnv
           }
         end
     | C.RecDec defs =>
         let
           fun add
             ( {name, tyParams, params, resultTy, attr = {typeOnly, ...}, ...}
             , ve
             ) =
             let
               val fnTy =
                 case (params, typeOnly) of
                   ([], true) => resultTy
                 | (_ :: _, true) => raise TypeError "invalid type abstraction"
                 | (_, false) => F.MultiFnType (List.map #2 params, resultTy)
             in
               TypedSyntax.VIdMap.insert
                 ( ve
                 , name
                 , List.foldr
                     (fn ((tv, kind), ty) => F.ForallType (tv, kind, ty)) fnTy
                     tyParams
                 )
             end
           val valEnv = List.foldl add valEnv defs
           fun check
             {name = _, contParam, tyParams, params, body, resultTy, attr = _} =
             let
               val contEnv = C.CVarMap.insert (contEnv, contParam, [resultTy])
               val tyEnv =
                 List.foldl
                   (fn ((tv, kind), te) =>
                      TypedSyntax.TyVarMap.insert (te, tv, kind)) tyEnv tyParams
               val innerValEnv =
                 List.foldl
                   (fn ((v, ty), ve) => TypedSyntax.VIdMap.insert (ve, v, ty))
                   valEnv params
             in
               checkStat
                 ( {valEnv = innerValEnv, tyEnv = tyEnv, contEnv = contEnv}
                 , body
                 )
             end
         in
           List.app check defs;
           {valEnv = valEnv, tyEnv = tyEnv, contEnv = contEnv}
         end
     | C.UnpackDec {tyVar, kind, vid, unpackedTy, package} =>
         let
           (* val () = print ("Unpack: " ^ TypedSyntax.print_VId vid ^ "\n") *)
           val packageTy = FSyntax.weakNormalizeTy (inferValue env package)
           val actualTy =
             case packageTy of
               F.ExistsType (tv, kind, ty) =>
                 FSyntax.substituteTy (tv, F.TyVar tyVar) ty
             | _ =>
                 raise TypeError
                   ("invalid Pack: packageTy = "
                    ^ Printer.build (FPrinter.doTy 0 packageTy))
         in
           checkCompatibility (fn () => "Pack", unpackedTy) actualTy;
           { valEnv = TypedSyntax.VIdMap.insert (valEnv, vid, unpackedTy)
           , tyEnv = TypedSyntax.TyVarMap.insert (tyEnv, tyVar, kind)
           , contEnv = contEnv
           }
         end
     | C.ContDec {name, params, body, attr = _} =>
         let
           val innerValEnv =
             List.foldl
               (fn ((SOME v, ty), ve) => TypedSyntax.VIdMap.insert (ve, v, ty)
                 | ((NONE, _), ve) => ve) valEnv params
         in
           checkStat
             ({valEnv = innerValEnv, tyEnv = tyEnv, contEnv = contEnv}, body);
           { valEnv = valEnv
           , tyEnv = tyEnv
           , contEnv = C.CVarMap.insert (contEnv, name, List.map #2 params)
           }
         end
     | C.RecContDec defs =>
         let
           fun add ((name, params, _), ce) =
             C.CVarMap.insert (ce, name, List.map #2 params)
           val contEnv = List.foldl add contEnv defs
           fun check (_, params, body) =
             let
               val innerValEnv =
                 List.foldl
                   (fn ((SOME v, ty), ve) =>
                      TypedSyntax.VIdMap.insert (ve, v, ty)
                     | ((NONE, _), ve) => ve) valEnv params
             in
               checkStat
                 ( {valEnv = innerValEnv, tyEnv = tyEnv, contEnv = contEnv}
                 , body
                 )
             end
         in
           List.app check defs;
           {valEnv = valEnv, tyEnv = tyEnv, contEnv = contEnv}
         end
     | C.DatatypeDec (tv, kind) =>
         { valEnv = valEnv
         , tyEnv = TypedSyntax.TyVarMap.insert (tyEnv, tv, kind)
         , contEnv = contEnv
         }
     | C.ESImportDec {pure = _, specs, moduleName = _} =>
         let
           val valEnv =
             List.foldl
               (fn ((_, v, ty), ve) => TypedSyntax.VIdMap.insert (ve, v, ty))
               valEnv specs
         in
           {valEnv = valEnv, tyEnv = tyEnv, contEnv = contEnv}
         end)
  and checkStat (env as {valEnv, tyEnv, contEnv}, stat) =
    (case stat of
       C.Let {decs, cont} =>
         let val env = List.foldl checkDec env decs
         in checkStat (env, cont)
         end
     | C.App {applied, cont, tyArgs, args, attr = {typeOnly, ...}} =>
         let
           val appliedTy = F.weakNormalizeTy (inferValue env applied)
           val contTy =
             case C.CVarMap.find (contEnv, cont) of
               NONE =>
                 raise TypeError
                   ("App: undefined continuation: "
                    ^ Int.toString (C.CVar.toInt cont))
             | SOME [ty] => ty
             | SOME _ => raise TypeError "App: multi-value return not supported"
           fun applyTy (tyArg, ty) =
             case F.weakNormalizeTy ty of
               F.ForallType (tv, kind, ty) => F.substituteTy (tv, tyArg) ty
             | ty =>
                 raise TypeError
                   ("App: invalid type application to "
                    ^ Printer.build (FPrinter.doTy 0 ty))
           val rawAppliedTy = appliedTy
           val appliedTy = List.foldl applyTy appliedTy tyArgs
         in
           case (args, typeOnly) of
             ([], true) =>
               checkCompatibility
                 ( fn () =>
                     "App (type only) " ^ C.valueToString applied ^ " "
                     ^ Printer.build (FPrinter.doTy 0 rawAppliedTy)
                 , appliedTy
                 ) contTy
           | (_ :: _, true) => raise TypeError "App: invalid type application"
           | (_, false) =>
               let
                 val argTypes = List.map (inferValue env) args
               in
                 checkCompatibility
                   ( fn () =>
                       "App " ^ C.valueToString applied ^ " "
                       ^ Printer.build (FPrinter.doTy 0 rawAppliedTy) ^ "["
                       ^
                       String.concatWith ","
                         (List.map (fn ta => Printer.build (FPrinter.doTy 0 ta))
                            tyArgs) ^ "]"
                   , appliedTy
                   ) (F.MultiFnType (argTypes, contTy))
               end
         end
     | C.AppCont {applied, args} =>
         let
           val appliedTy =
             case C.CVarMap.find (contEnv, applied) of
               NONE =>
                 raise TypeError
                   ("App: undefined continuation: "
                    ^ Int.toString (C.CVar.toInt applied))
             | SOME types => types
         in
           ListPair.appEq
             (fn (e, a) => checkValue (env, fn () => "AppCont", e) a)
             (appliedTy, args)
           handle ListPair.UnequalLengths =>
             raise TypeError "AppCont: arity mismatch"
         end
     | C.If {cond, thenCont, elseCont} =>
         ( checkValue (env, fn () => "If", F.Types.bool) cond
         ; checkStat (env, thenCont)
         ; checkStat (env, elseCont)
         )
     | C.Handle
         {body, handler = (e, h), successfulExitIn, successfulExitOut, resultTy} =>
         let
           val bodyEnv =
             { valEnv = valEnv
             , tyEnv = tyEnv
             , contEnv = C.CVarMap.singleton (successfulExitIn, [resultTy])
             }
           val handlerEnv =
             { valEnv = TypedSyntax.VIdMap.insert (valEnv, e, F.Types.exn)
             , tyEnv = tyEnv
             , contEnv = contEnv
             }
         in
           checkStat (bodyEnv, body);
           checkStat (handlerEnv, h)
         end
     | C.Raise (_, e) => checkValue (env, fn () => "Raise", F.Types.exn) e
     | C.Unreachable => ())
end;
