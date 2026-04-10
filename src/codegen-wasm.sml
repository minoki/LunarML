(*
 * Copyright (c) 2026 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure CodeGenWasm :>
sig
  exception CodeGenError of string

  type Context =
    { nextTypeIdx: int ref
    , nextFuncIdx: int ref
    , revTypes: WasmSyntax.rectype list ref (* reversed *)
    , revFuncs: WasmSyntax.func list ref (* reversed *)
    , revGlobals: WasmSyntax.global list ref (* reversed *)
    , revImports: WasmSyntax.import list ref (* reversed *)
    , revExports: WasmSyntax.export list ref (* reversed *)
    , revDatas: WasmSyntax.data list ref (* reversed *)
    , revTags: WasmSyntax.tagtype list ref (* reversed, module-defined tags *)
    , tupleTyIdxMap: int IntRedBlackMap.map ref (* field count -> typeidx *)
    , funcTypeMap:
        (WasmSyntax.valtype list * WasmSyntax.valtype list * int) list ref
    , closureBaseTypeIdx: WasmSyntax.typeidx
    , boxedI32TypeIdx: WasmSyntax.typeidx
    , boxedI64TypeIdx: WasmSyntax.typeidx
    , boxedF64TypeIdx: WasmSyntax.typeidx
    , exnTagTypeIdx: WasmSyntax.typeidx
    , smlExnTypeIdx: WasmSyntax.typeidx
    , smlExnTagIdx: int (* tagidx of $sml_exn tag *)
    }

  type FuncContext =
    { ctx: Context
    , nextLocalIdx: int ref
    , revLocalTypes: WasmSyntax.valtype list ref (* reversed *)
    }

  val labelToFieldIndex: Syntax.Label * FSyntax.Ty Syntax.LabelMap.map -> int

  val getTupleTypeIdx: Context -> int -> WasmSyntax.typeidx

  val emitBox: FSyntax.UnboxedTy * Context -> WasmSyntax.instr list
  val emitUnbox: FSyntax.UnboxedTy * Context -> WasmSyntax.instr list

  (* Entry point *)
  val initContext: unit -> Context
  val doProgram: Context
                 -> CSyntax.CVar
                 -> NSyntax.Stat
                 -> ToFSyntax.export_entity
                 -> WasmSyntax.module
end =
struct
  exception CodeGenError of string

  structure F = FSyntax
  structure C = CSyntax
  structure N = NSyntax
  structure W = WasmSyntax

  type Context =
    { nextTypeIdx: int ref
    , nextFuncIdx: int ref
    , revTypes: W.rectype list ref
    , revFuncs: W.func list ref
    , revGlobals: W.global list ref
    , revImports: W.import list ref
    , revExports: W.export list ref
    , revDatas: W.data list ref
    , revTags: W.tagtype list ref
    , tupleTyIdxMap: int IntRedBlackMap.map ref
    , funcTypeMap: (W.valtype list * W.valtype list * int) list ref
    , closureBaseTypeIdx: W.typeidx
    , boxedI32TypeIdx: W.typeidx
    , boxedI64TypeIdx: W.typeidx
    , boxedF64TypeIdx: W.typeidx
    , exnTagTypeIdx: W.typeidx
    , smlExnTypeIdx: W.typeidx
    , smlExnTagIdx: int
    }

  type FuncContext =
    {ctx: Context, nextLocalIdx: int ref, revLocalTypes: W.valtype list ref}

  val eqRefType: W.reftype = {nullable = true, heaptype = W.AbsHeapType W.EQ}
  val eqref: W.valtype = W.RefType eqRefType
  val eqref_nn: W.valtype =
    W.RefType {nullable = false, heaptype = W.AbsHeapType W.EQ}

  (* ==================== Type Mapping ==================== *)

  (* Map FSyntax.Ty to unboxed type tag, if applicable *)
  fun tyToUnboxedTy (F.TyVar tv) =
        if tv = PrimTypes.Names.int32 then SOME F.UBTyInt32
        else if tv = PrimTypes.Names.word32 then SOME F.UBTyWord32
        else if tv = PrimTypes.Names.bool then SOME F.UBTyBool
        else if tv = PrimTypes.Names.char then SOME F.UBTyChar
        else if tv = PrimTypes.Names.char16 then SOME F.UBTyChar16
        else if tv = PrimTypes.Names.char32 then SOME F.UBTyChar32
        else if tv = PrimTypes.Names.int64 then SOME F.UBTyInt64
        else if tv = PrimTypes.Names.word64 then SOME F.UBTyWord64
        else if tv = PrimTypes.Names.real then SOME F.UBTyReal
        else NONE
    | tyToUnboxedTy _ = NONE

  (* Convert FSyntax.Ty to Wasm valtype for local variable allocation.
     After CpsBoxing, unboxed types appear in certain positions;
     boxed types and polymorphic types use eqref. *)
  fun tyToWasmType (F.TyVar tv) =
        if
          tv = PrimTypes.Names.int32 orelse tv = PrimTypes.Names.word32
          orelse tv = PrimTypes.Names.bool orelse tv = PrimTypes.Names.char
          orelse tv = PrimTypes.Names.char7 orelse tv = PrimTypes.Names.char16
          orelse tv = PrimTypes.Names.char32 orelse tv = PrimTypes.Names.uchar
        then W.NumType W.I32
        else if
          tv = PrimTypes.Names.int64 orelse tv = PrimTypes.Names.word64
        then W.NumType W.I64
        else if
          tv = PrimTypes.Names.real
        then W.NumType W.F64
        else eqref
    | tyToWasmType (F.RecordType _) = eqref
    | tyToWasmType (F.MultiFnType _) = eqref
    | tyToWasmType (F.BoxedType) = eqref
    | tyToWasmType (F.AnyType _) = eqref
    | tyToWasmType _ = eqref (* other types default to eqref *)

  (* Compute the 0-based field index of a label within a LabelMap *)
  fun labelToFieldIndex (label, fieldTypes) =
    let
      (* TODO: a more functional implementation? *)
      val index = ref 0
      val found = ref false
    in
      Syntax.LabelMap.appi
        (fn (l, _) =>
           if l = label then found := true
           else if not (!found) then index := !index + 1
           else ()) fieldTypes;
      if !found then !index
      else raise CodeGenError "labelToFieldIndex: label not found"
    end

  (* ==================== Record/Tuple ==================== *)

  (* Allocate a new type index *)
  fun allocTypeIdx (ctx: Context) =
    let val idx = !(#nextTypeIdx ctx)
    in #nextTypeIdx ctx := idx + 1; idx
    end

  (* Allocate a new function index *)
  fun allocFuncIdx (ctx: Context) =
    let val idx = !(#nextFuncIdx ctx)
    in #nextFuncIdx ctx := idx + 1; idx
    end

  (* Get or create a function type index with deduplication *)
  fun getFuncTypeIdx (ctx: Context)
    (params: W.valtype list, results: W.valtype list) =
    let
      fun eq (p1, r1, _) = p1 = params andalso r1 = results
    in
      case List.find eq (!(#funcTypeMap ctx)) of
        SOME (_, _, idx) => idx
      | NONE =>
          let
            val idx = allocTypeIdx ctx
            val subtype = W.SubType
              { final = true
              , supertypes = []
              , body = W.FuncType {params = params, results = results}
              }
            val () = #revTypes ctx := [subtype] :: !(#revTypes ctx)
            val () =
              #funcTypeMap ctx := (params, results, idx) :: !(#funcTypeMap ctx)
          in
            idx
          end
    end

  (* Get or create a tuple struct type for the given field count.
   * All fields are (ref eq), and the struct is a subtype of eq. *)
  fun getTupleTypeIdx (ctx: Context) (n: int) =
    case IntRedBlackMap.find (!(#tupleTyIdxMap ctx), n) of
      SOME idx => idx
    | NONE =>
        let
          val idx = allocTypeIdx ctx
          val fields = List.tabulate (n, fn _ =>
            {mut = W.CONST, storagetype = W.ValStorageType eqref})
          val subtype =
            W.SubType
              {final = false, supertypes = [], body = W.StructType fields}
          val () = #revTypes ctx := [subtype] :: !(#revTypes ctx)
          val () =
            #tupleTyIdxMap ctx
            := IntRedBlackMap.insert (!(#tupleTyIdxMap ctx), n, idx)
        in
          idx
        end

  (* ==================== Box/Unbox ==================== *)

  (* Emit instructions to box an unboxed value on the stack to (ref eq) *)
  fun emitBox (ubt, ctx: Context) =
    case ubt of
      F.UBTyInt32 => [W.STRUCT_NEW (#boxedI32TypeIdx ctx)]
    | F.UBTyWord32 => [W.STRUCT_NEW (#boxedI32TypeIdx ctx)]
    | F.UBTyBool => [W.REF_I31]
    | F.UBTyChar => [W.REF_I31]
    | F.UBTyChar16 => [W.REF_I31]
    | F.UBTyChar32 => [W.REF_I31]
    | F.UBTyInt64 => [W.STRUCT_NEW (#boxedI64TypeIdx ctx)]
    | F.UBTyWord64 => [W.STRUCT_NEW (#boxedI64TypeIdx ctx)]
    | F.UBTyReal => [W.STRUCT_NEW (#boxedF64TypeIdx ctx)]

  (* Emit instructions to unbox a (ref eq) value on the stack *)
  fun emitUnbox (ubt, ctx: Context) =
    let
      val i31ref_nn: W.reftype =
        {nullable = false, heaptype = W.AbsHeapType W.I31}
    in
      case ubt of
        F.UBTyInt32 =>
          let
            val rt: W.reftype =
              {nullable = false, heaptype = W.TypeIdx (#boxedI32TypeIdx ctx)}
          in
            [W.REF_CAST rt, W.STRUCT_GET (#boxedI32TypeIdx ctx, 0)]
          end
      | F.UBTyWord32 =>
          let
            val rt: W.reftype =
              {nullable = false, heaptype = W.TypeIdx (#boxedI32TypeIdx ctx)}
          in
            [W.REF_CAST rt, W.STRUCT_GET (#boxedI32TypeIdx ctx, 0)]
          end
      | F.UBTyBool => [W.REF_CAST i31ref_nn, W.I31_GET W.U]
      | F.UBTyChar => [W.REF_CAST i31ref_nn, W.I31_GET W.U]
      | F.UBTyChar16 => [W.REF_CAST i31ref_nn, W.I31_GET W.U]
      | F.UBTyChar32 => [W.REF_CAST i31ref_nn, W.I31_GET W.U]
      | F.UBTyInt64 =>
          let
            val rt: W.reftype =
              {nullable = false, heaptype = W.TypeIdx (#boxedI64TypeIdx ctx)}
          in
            [W.REF_CAST rt, W.STRUCT_GET (#boxedI64TypeIdx ctx, 0)]
          end
      | F.UBTyWord64 =>
          let
            val rt: W.reftype =
              {nullable = false, heaptype = W.TypeIdx (#boxedI64TypeIdx ctx)}
          in
            [W.REF_CAST rt, W.STRUCT_GET (#boxedI64TypeIdx ctx, 0)]
          end
      | F.UBTyReal =>
          let
            val rt: W.reftype =
              {nullable = false, heaptype = W.TypeIdx (#boxedF64TypeIdx ctx)}
          in
            [W.REF_CAST rt, W.STRUCT_GET (#boxedF64TypeIdx ctx, 0)]
          end
    end

  (* ==================== Environment ==================== *)

  (* Representation of a continuation in the environment *)
  datatype cont_repr =
    RETURN (* return continuation: use RETURN instruction *)
  | BREAK_TO of
      { label: int (* block label index (depth from innermost) *)
      , params: (W.localidx option) list
      }
  | CONTINUE_TO of
      { label: int (* loop label index *)
      , which: (W.localidx * int) option (* (which_var, index) for RecContDec *)
      , params: (W.localidx option) list
      }

  type Env =
    { vars: W.localidx TypedSyntax.VIdMap.map
    , continuations: cont_repr C.CVarMap.map
    }

  val emptyEnv: Env =
    {vars = TypedSyntax.VIdMap.empty, continuations = C.CVarMap.empty}

  fun envWithVar (env: Env, v, idx) : Env =
    { vars = TypedSyntax.VIdMap.insert (#vars env, v, idx)
    , continuations = #continuations env
    }

  fun envWithCont (env: Env, k, repr) : Env =
    { vars = #vars env
    , continuations = C.CVarMap.insert (#continuations env, k, repr)
    }

  (* Bump all label indices in a cont_repr by n (for extra block nesting) *)
  fun bumpContRepr _ RETURN = RETURN
    | bumpContRepr n (BREAK_TO {label, params}) =
        BREAK_TO {label = label + n, params = params}
    | bumpContRepr n (CONTINUE_TO {label, which, params}) =
        CONTINUE_TO {label = label + n, which = which, params = params}

  (* Bump all continuation label indices in an Env by n.
     Use this when entering a structured control instruction that adds a label
     (e.g., if/block/loop/try_table) so that existing br targets stay correct. *)
  fun bumpEnvConts n (env: Env) : Env =
    { vars = #vars env
    , continuations = C.CVarMap.map (bumpContRepr n) (#continuations env)
    }

  (* Allocate a new local variable with a specific type *)
  fun allocLocal (fctx: FuncContext) (ty: W.valtype) =
    let
      val idx = !(#nextLocalIdx fctx)
    in
      #nextLocalIdx fctx := idx + 1;
      #revLocalTypes fctx := ty :: !(#revLocalTypes fctx);
      idx
    end

  (* Create a new context for compiling a nested function body.
   * Shares module-level state (types, funcs, etc.) but has a fresh local counter. *)
  fun newFuncContext (ctx: Context) : FuncContext =
    {ctx = ctx, nextLocalIdx = ref 0, revLocalTypes = ref []}

  (* ==================== Closure types ==================== *)

  (* Get the standard closure function type: (ref $ClosureBase, ref eq, ...) -> (ref eq) *)
  fun getClosureFuncTypeIdx (ctx: Context) (nParams: int) =
    let
      val closureRef = W.RefType
        {nullable = false, heaptype = W.TypeIdx (#closureBaseTypeIdx ctx)}
      val params = closureRef :: List.tabulate (nParams, fn _ => eqref)
      val results = [eqref]
    in
      getFuncTypeIdx ctx (params, results)
    end

  (* Create a closure struct type: sub $ClosureBase (struct (field $code (ref $FuncType)) (field (ref eq))...) *)
  fun getClosureTypeIdx (ctx: Context) (nFreeVars: int) (funcTypeIdx: W.typeidx) =
    let
      val idx = allocTypeIdx ctx
      val codeField =
        { mut = W.CONST
        , storagetype = W.ValStorageType
            (W.RefType {nullable = false, heaptype = W.TypeIdx funcTypeIdx})
        }
      val freeVarFields = List.tabulate (nFreeVars, fn _ =>
        {mut = W.CONST, storagetype = W.ValStorageType eqref})
      val subtype = W.SubType
        { final = false
        , supertypes = [#closureBaseTypeIdx ctx]
        , body = W.StructType (codeField :: freeVarFields)
        }
      val () = #revTypes ctx := [subtype] :: !(#revTypes ctx)
    in
      idx
    end

  (* Create a mutable closure struct type (for recursive closures that need backpatching) *)
  fun getMutClosureTypeIdx (ctx: Context) (nFreeVars: int)
    (funcTypeIdx: W.typeidx) =
    let
      val idx = allocTypeIdx ctx
      val codeField =
        { mut = W.CONST
        , storagetype = W.ValStorageType
            (W.RefType {nullable = false, heaptype = W.TypeIdx funcTypeIdx})
        }
      val freeVarFields = List.tabulate (nFreeVars, fn _ =>
        {mut = W.VAR, storagetype = W.ValStorageType eqref})
      val subtype = W.SubType
        { final = false
        , supertypes = [#closureBaseTypeIdx ctx]
        , body = W.StructType (codeField :: freeVarFields)
        }
      val () = #revTypes ctx := [subtype] :: !(#revTypes ctx)
    in
      idx
    end

  (* ==================== Free variable collection ==================== *)

  fun freeVarsValue (C.Var v, acc) = TypedSyntax.VIdSet.add (acc, v)
    | freeVarsValue (C.Cast {value, ...}, acc) = freeVarsValue (value, acc)
    | freeVarsValue (C.Pack {value, ...}, acc) = freeVarsValue (value, acc)
    | freeVarsValue (_, acc) = acc

  fun freeVarsExp (N.Value v, acc) = freeVarsValue (v, acc)
    | freeVarsExp (N.PrimOp {args, ...}, acc) =
        List.foldl (fn (a, s) => freeVarsExp (a, s)) acc args
    | freeVarsExp (N.Record fields, acc) =
        Syntax.LabelMap.foldl (fn (e, s) => freeVarsExp (e, s)) acc fields
    | freeVarsExp (N.ExnTag _, acc) = acc
    | freeVarsExp (N.Projection {record, ...}, acc) = freeVarsExp (record, acc)
    | freeVarsExp (N.Abs {params, body, ...}, acc) =
        let
          val bodyFV = freeVarsStat (body, TypedSyntax.VIdSet.empty)
          val paramSet =
            List.foldl (fn ((v, _), s) => TypedSyntax.VIdSet.add (s, v))
              TypedSyntax.VIdSet.empty params
          val fv = TypedSyntax.VIdSet.difference (bodyFV, paramSet)
        in
          TypedSyntax.VIdSet.union (acc, fv)
        end
    | freeVarsExp (N.LogicalAnd (a, b), acc) =
        freeVarsExp (b, freeVarsExp (a, acc))
    | freeVarsExp (N.LogicalOr (a, b), acc) =
        freeVarsExp (b, freeVarsExp (a, acc))
  and freeVarsDec (N.ValDec {exp, results}, acc) =
        let
          val fv = freeVarsExp (exp, acc)
          val bound =
            List.foldl
              (fn ((SOME v, _), s) => TypedSyntax.VIdSet.add (s, v)
                | ((NONE, _), s) => s) TypedSyntax.VIdSet.empty results
        in
          TypedSyntax.VIdSet.difference (fv, bound)
        end
    | freeVarsDec (N.RecDec decs, acc) =
        let
          val names =
            List.foldl (fn ({name, ...}, s) => TypedSyntax.VIdSet.add (s, name))
              TypedSyntax.VIdSet.empty decs
          val fv =
            List.foldl
              (fn ({params, body, ...}, s) =>
                 let
                   val bodyFV = freeVarsStat (body, TypedSyntax.VIdSet.empty)
                   val paramSet =
                     List.foldl
                       (fn ((v, _), s') => TypedSyntax.VIdSet.add (s', v))
                       TypedSyntax.VIdSet.empty params
                 in
                   TypedSyntax.VIdSet.union
                     (s, TypedSyntax.VIdSet.difference (bodyFV, paramSet))
                 end) acc decs
        in
          TypedSyntax.VIdSet.difference (fv, names)
        end
    | freeVarsDec (N.ContDec {body, params, ...}, acc) =
        let
          val bodyFV = freeVarsStat (body, TypedSyntax.VIdSet.empty)
          val paramSet =
            List.foldl
              (fn ((SOME v, _), s) => TypedSyntax.VIdSet.add (s, v)
                | ((NONE, _), s) => s) TypedSyntax.VIdSet.empty params
        in
          TypedSyntax.VIdSet.union
            (acc, TypedSyntax.VIdSet.difference (bodyFV, paramSet))
        end
    | freeVarsDec (N.RecContDec defs, acc) =
        List.foldl
          (fn ((_, params, body), s) =>
             let
               val bodyFV = freeVarsStat (body, TypedSyntax.VIdSet.empty)
               val paramSet =
                 List.foldl
                   (fn ((SOME v, _), s') => TypedSyntax.VIdSet.add (s', v)
                     | ((NONE, _), s') => s') TypedSyntax.VIdSet.empty params
             in
               TypedSyntax.VIdSet.union
                 (s, TypedSyntax.VIdSet.difference (bodyFV, paramSet))
             end) acc defs
    | freeVarsDec (N.ESImportDec _, acc) = acc
  and freeVarsStat (N.Let {decs, cont}, acc) =
        let
          val fv = freeVarsStat (cont, acc)
          val fv = List.foldr (fn (d, s) => freeVarsDec (d, s)) fv decs
        in
          fv
        end
    | freeVarsStat (N.App {applied, args, ...}, acc) =
        List.foldl (fn (a, s) => freeVarsExp (a, s))
          (freeVarsExp (applied, acc)) args
    | freeVarsStat (N.AppCont {args, ...}, acc) =
        List.foldl (fn (a, s) => freeVarsExp (a, s)) acc args
    | freeVarsStat (N.If {cond, thenCont, elseCont}, acc) =
        freeVarsExp (cond, freeVarsStat
          (elseCont, freeVarsStat (thenCont, acc)))
    | freeVarsStat (N.Handle {body, handler = (e, h), ...}, acc) =
        let
          val bodyFV = freeVarsStat (body, acc)
          val handlerFV = freeVarsStat (h, TypedSyntax.VIdSet.empty)
          val handlerFV =
            if TypedSyntax.VIdSet.member (handlerFV, e) then
              TypedSyntax.VIdSet.delete (handlerFV, e)
            else
              handlerFV
        in
          TypedSyntax.VIdSet.union (bodyFV, handlerFV)
        end
    | freeVarsStat (N.Raise (_, exp), acc) = freeVarsExp (exp, acc)
    | freeVarsStat (N.Unreachable, acc) = acc

  (* ==================== doValue ==================== *)
  (* All do* functions take a reverse accumulator and return a reverse accumulator.
   * Use List.rev on the final result to get forward-order instructions. *)

  fun doValue (fctx: FuncContext) (env: Env) (v: C.Value, acc: W.instr list) :
    W.instr list =
    case v of
      C.Var vid =>
        (case TypedSyntax.VIdMap.find (#vars env, vid) of
           SOME idx => W.LOCAL_GET idx :: acc
         | NONE =>
             raise CodeGenError
               ("doValue: unbound variable " ^ TypedSyntax.print_VId vid))
    | C.Unit => W.REF_NULL (W.AbsHeapType W.HEAP_NONE) :: acc
    | C.Nil => W.REF_NULL (W.AbsHeapType W.HEAP_NONE) :: acc
    | C.TypedNil _ => W.REF_NULL (W.AbsHeapType W.HEAP_NONE) :: acc
    | C.BoolConst b => W.I32_CONST (if b then 1 else 0) :: acc
    | C.IntConst (Primitives.I32, n) => W.I32_CONST (Int32.fromLarge n) :: acc
    | C.IntConst (Primitives.I64, n) => W.I64_CONST (Int64.fromLarge n) :: acc
    | C.IntConst (Primitives.I54, n) => W.I32_CONST (Int32.fromLarge n) :: acc
    | C.IntConst (Primitives.INT, n) => W.I32_CONST (Int32.fromLarge n) :: acc
    | C.IntConst (Primitives.INT_INF, _) =>
        raise CodeGenError "doValue: INT_INF not supported in Wasm"
    | C.WordConst (Primitives.W32, n) => W.I32_CONST (Int32.fromLarge n) :: acc
    | C.WordConst (Primitives.W64, n) => W.I64_CONST (Int64.fromLarge n) :: acc
    | C.WordConst (Primitives.WORD, n) => W.I32_CONST (Int32.fromLarge n) :: acc
    | C.CharConst (_, c) => W.I32_CONST (Int32.fromInt c) :: acc
    | C.StringConst s => doStringConst (fctx, s, acc)
    | C.String7Const s => doStringConst (fctx, s, acc)
    | C.String16Const _ =>
        raise CodeGenError "doValue: String16Const not yet implemented"
    | C.String32Const _ =>
        raise CodeGenError "doValue: String32Const not yet implemented"
    | C.PrimEffect _ => W.REF_NULL (W.AbsHeapType W.HEAP_NONE) :: acc
    | C.Cast {value, ...} => doValue fctx env (value, acc)
    | C.Pack {value, ...} => doValue fctx env (value, acc)

  (* String constants: store in data segment and create array *)
  and doStringConst ((_: FuncContext), s: string, acc: W.instr list) =
    if String.size s = 0 then
      W.REF_NULL (W.AbsHeapType W.HEAP_NONE) :: acc
    else
      (* TODO: proper string representation *)
      W.REF_NULL (W.AbsHeapType W.HEAP_NONE) :: acc

  (* ==================== doExp ==================== *)

  and doExp (fctx: FuncContext) (env: Env) (exp: N.Exp, acc: W.instr list) :
    W.instr list =
    let
      val ctx = #ctx fctx
    in
      case exp of
        N.Value v => doValue fctx env (v, acc)
      | N.PrimOp {primOp, tyargs, args} =>
          doPrimOp fctx env (primOp, tyargs, args, acc)
      | N.Record fields =>
          let
            val n = Syntax.LabelMap.numItems fields
          in
            if n = 0 then
              W.REF_NULL (W.AbsHeapType W.HEAP_NONE) :: acc
            else
              let
                val tupleIdx = getTupleTypeIdx ctx n
                val acc' =
                  Syntax.LabelMap.foldl (fn (e, a) => doExp fctx env (e, a)) acc
                    fields
              in
                W.STRUCT_NEW tupleIdx :: acc'
              end
          end
      | N.ExnTag _ =>
          (* Create a fresh ExnTagType struct instance for unique identity via ref.eq *)
          W.STRUCT_NEW (#exnTagTypeIdx ctx) :: acc
      | N.Projection {label, record, fieldTypes} =>
          let
            val n = Syntax.LabelMap.numItems fieldTypes
            val tupleIdx = getTupleTypeIdx ctx n
            val fieldIdx = labelToFieldIndex (label, fieldTypes)
            (* Tuple struct fields are always eqref, so struct.get returns eqref.
               If the field's original type is unboxable (e.g., int32), we need to
               unbox the result since the CPS type system still expects the
               unboxed type. *)
            val fieldTy =
              case Syntax.LabelMap.find (fieldTypes, label) of
                SOME ty => ty
              | NONE => F.BoxedType
            val unboxInstrs =
              case tyToUnboxedTy fieldTy of
                SOME ubt => emitUnbox (ubt, ctx)
              | NONE => []
          in
            List.revAppend
              ( unboxInstrs
              , W.STRUCT_GET (tupleIdx, fieldIdx)
                :: W.REF_CAST {nullable = false, heaptype = W.TypeIdx tupleIdx}
                :: doExp fctx env (record, acc)
              )
          end
      | N.Abs {contParam, params, body, resultTy = _, attr = _} =>
          doAbs fctx env (contParam, params, body, acc)
      | N.LogicalAnd (e1, e2) =>
          W.IF
            ( W.BlockTypeVal (W.NumType W.I32)
            , List.rev (doExp fctx env (e2, []))
            , [W.I32_CONST 0]
            ) :: doExp fctx env (e1, acc)
      | N.LogicalOr (e1, e2) =>
          W.IF (W.BlockTypeVal (W.NumType W.I32), [W.I32_CONST 1], List.rev
            (doExp fctx env (e2, []))) :: doExp fctx env (e1, acc)
    end

  (* ==================== Closure generation (Abs) ==================== *)

  and doAbs (fctx: FuncContext) (env: Env)
    ( contParam: C.CVar
    , params: (C.Var * F.Ty) list
    , body: N.Stat
    , acc: W.instr list
    ) : W.instr list =
    let
      val ctx = #ctx fctx
      (* Collect free variables *)
      val bodyFV = freeVarsStat (body, TypedSyntax.VIdSet.empty)
      val paramSet =
        List.foldl (fn ((v, _), s) => TypedSyntax.VIdSet.add (s, v))
          TypedSyntax.VIdSet.empty params
      val freeVarSet = TypedSyntax.VIdSet.difference (bodyFV, paramSet)
      val freeVars = TypedSyntax.VIdSet.listItems freeVarSet

      val nParams = List.length params
      val nFreeVars = List.length freeVars

      (* Create function type *)
      val funcTypeIdx = getClosureFuncTypeIdx ctx nParams

      (* Create closure struct type *)
      val closureTypeIdx = getClosureTypeIdx ctx nFreeVars funcTypeIdx

      (* Create a new context for the inner function *)
      val innerFctx = newFuncContext ctx

      (* Allocate locals: first is the closure self param *)
      val selfLocal = allocLocal innerFctx eqref (* param 0: closure ref *)
      val paramLocals =
        List.map (fn _ => allocLocal innerFctx eqref) params (* params *)

      (* Build environment for function body *)
      val innerEnv = emptyEnv
      (* Add return continuation *)
      val innerEnv = envWithCont (innerEnv, contParam, RETURN)
      (* Add params *)
      val innerEnv =
        ListPair.foldl (fn ((v, _), localIdx, e) => envWithVar (e, v, localIdx))
          innerEnv (params, paramLocals)

      (* Add free variables: extract from closure struct.
       * Build preamble as reverse accumulator. *)
      val (revPreamble, innerEnv) =
        let
          fun go ([], _, revAcc, e) = (revAcc, e)
            | go (fv :: rest, fieldIdx, revAcc, e) =
                let
                  val localIdx = allocLocal innerFctx eqref
                  val e' = envWithVar (e, fv, localIdx)
                in
                  go
                    ( rest
                    , fieldIdx + 1
                    , W.LOCAL_SET localIdx
                      :: W.STRUCT_GET (closureTypeIdx, fieldIdx)
                      ::
                      W.REF_CAST
                        {nullable = false, heaptype = W.TypeIdx closureTypeIdx}
                      :: W.LOCAL_GET selfLocal :: revAcc
                    , e'
                    )
                end
        in
          go (freeVars, 1, [], innerEnv) (* field 0 is the code pointer *)
        end

      (* Generate body with preamble as initial accumulator *)
      val bodyInstrs = List.rev (doStat innerFctx innerEnv (body, revPreamble))

      val totalLocals = !(#nextLocalIdx innerFctx)
      (* Params don't count as locals in Wasm func - they are separate *)
      val nWasmParams = 1 + nParams (* closure + user params *)
      val extraLocals = totalLocals - nWasmParams
      val allLocalTypes = List.rev (!(#revLocalTypes innerFctx))
      val localTypes = List.drop (allLocalTypes, nWasmParams)

      (* Register the function *)
      val funcIdx = allocFuncIdx ctx
      val func: W.func =
        {typeidx = funcTypeIdx, locals = localTypes, body = bodyInstrs}
      val () = #revFuncs ctx := func :: !(#revFuncs ctx)

      (* At call site: create the closure struct *)
      val acc = W.REF_FUNC funcIdx :: acc
      val acc =
        List.foldl
          (fn (fv, a) =>
             case TypedSyntax.VIdMap.find (#vars env, fv) of
               SOME idx => W.LOCAL_GET idx :: a
             | NONE =>
                 raise CodeGenError
                   ("doAbs: free var not in scope: " ^ TypedSyntax.print_VId fv))
          acc freeVars
    in
      W.STRUCT_NEW closureTypeIdx :: acc
    end

  (* ==================== doStat ==================== *)

  and doStat (fctx: FuncContext) (env: Env) (stat: N.Stat, acc: W.instr list) :
    W.instr list =
    case stat of
      N.Let {decs, cont} => doLetDecs fctx env (decs, cont, acc)
    | N.App {applied, cont, args, attr = _} =>
        doApp fctx env (applied, cont, args, acc)
    | N.AppCont {applied, args} => doAppCont fctx env (applied, args, acc)
    | N.If {cond, thenCont, elseCont} =>
        (* The `if` instruction adds a label (level 0 inside the body).
           Bump all continuation labels by 1 so existing `br` targets remain correct. *)
        let
          val bumpedEnv = bumpEnvConts 1 env
        in
          W.UNREACHABLE
          ::
          W.IF
            ( W.BlockTypeNone
            , List.rev (doStat fctx bumpedEnv (thenCont, []))
            , List.rev (doStat fctx bumpedEnv (elseCont, []))
            ) :: doExp fctx env (cond, acc)
        end
    | N.Handle
        { body
        , handler = (e, h)
        , successfulExitIn
        , successfulExitOut
        , resultTy = _
        } =>
        let
          val ctx = #ctx fctx
          (* Look up successfulExitOut continuation representation *)
          val outRepr =
            case C.CVarMap.find (#continuations env, successfulExitOut) of
              SOME r => r
            | NONE =>
                raise CodeGenError
                  "doStat: Handle: successfulExitOut not in env"
          (* Inside try_table body we are 2 levels deeper:
             - outer BLOCK (level +1)
             - try_table itself (level +1)
             So bump all break/continue labels by 2. *)
          val innerOutRepr = bumpContRepr 2 outRepr
          (* Allocate a local to hold the caught exception value *)
          val eLocal = allocLocal fctx eqref
          (* Env for body: add successfulExitIn → innerOutRepr *)
          val bodyEnv = envWithCont (env, successfulExitIn, innerOutRepr)
          (* Env for handler: bind exception variable e to eLocal *)
          val handlerEnv = envWithVar (env, e, eLocal)
          (* Generate body and handler code *)
          val bodyCode = List.rev (doStat fctx bodyEnv (body, []))
          val handlerCode = List.rev (doStat fctx handlerEnv (h, []))
          val smlExnTagIdx = #smlExnTagIdx ctx
          (* Structure:
             block (result eqref)       ;; outerBlock: catch target (label 0 from try_table's outer context)
               try_table (catch $sml_exn 0)  ;; 0 = outerBlock (immediately enclosing, counted from outside try_table)
                 <body>                 ;; body uses bumpContRepr 2 labels (try_table=1, outerBlock=1)
                 unreachable            ;; body always transfers control
               end
               unreachable              ;; never reached
             end                        ;; exception (eqref) falls through here
             local.set eLocal           ;; bind caught exception
             <handlerCode>              ;; handler runs in original env *)
          val outerBlock = W.BLOCK
            ( W.BlockTypeVal eqref
            , [ W.TRY_TABLE
                  ( W.BlockTypeNone
                  , [W.CATCH (smlExnTagIdx, 0)]
                  , bodyCode @ [W.UNREACHABLE]
                  )
              , W.UNREACHABLE
              ]
            )
        in
          List.revAppend (handlerCode, W.LOCAL_SET eLocal :: outerBlock :: acc)
        end
    | N.Raise (_, exp) =>
        (* Pop exception value (eqref = $SmlExn) from stack and throw *)
        W.THROW (#smlExnTagIdx (#ctx fctx)) :: doExp fctx env (exp, acc)
    | N.Unreachable => W.UNREACHABLE :: acc

  (* ==================== Function application ==================== *)

  and doApp (fctx: FuncContext) (env: Env)
    (applied: N.Exp, cont: C.CVar, args: N.Exp list, acc: W.instr list) :
    W.instr list =
    let
      val ctx = #ctx fctx
      val contRepr = C.CVarMap.find (#continuations env, cont)
      val nArgs = List.length args
      (* Helper: emit closure call instructions.
         Pushes (ref $ClosureBase), args, funcref onto the stack.
         Caller must follow with CALL_REF or RETURN_CALL_REF. *)
      fun emitClosureCall acc' applied' args' funcTypeIdx' closureLocal' =
        let
          val acc' = doExp fctx env (applied', acc')
          val acc' = W.LOCAL_SET closureLocal' :: acc'
          val acc' = W.LOCAL_GET closureLocal' :: acc'
          val acc' =
            W.REF_CAST
              {nullable = false, heaptype = W.TypeIdx (#closureBaseTypeIdx ctx)}
            :: acc'
          val acc' =
            List.foldl (fn (arg', a) => doExp fctx env (arg', a)) acc' args'
          val acc' = W.LOCAL_GET closureLocal' :: acc'
          val acc' =
            W.REF_CAST
              {nullable = false, heaptype = W.TypeIdx (#closureBaseTypeIdx ctx)}
            :: acc'
          val acc' = W.STRUCT_GET (#closureBaseTypeIdx ctx, 0) :: acc'
          val acc' =
            W.REF_CAST {nullable = false, heaptype = W.TypeIdx funcTypeIdx'}
            :: acc'
        in
          acc'
        end
    in
      case contRepr of
        SOME RETURN =>
          let
            val funcTypeIdx = getClosureFuncTypeIdx ctx nArgs
            val closureLocal = allocLocal fctx eqref
            val acc = emitClosureCall acc applied args funcTypeIdx closureLocal
          in
            W.RETURN_CALL_REF funcTypeIdx :: acc
          end
      | SOME (BREAK_TO {label, params}) =>
          let
            val funcTypeIdx = getClosureFuncTypeIdx ctx nArgs
            val closureLocal = allocLocal fctx eqref
            val acc = emitClosureCall acc applied args funcTypeIdx closureLocal
            val acc = W.CALL_REF funcTypeIdx :: acc
            val acc =
              case params of
                [SOME p] => W.LOCAL_SET p :: acc
              | [NONE] => W.DROP :: acc
              | _ =>
                  raise CodeGenError
                    "doApp: multi-value continuation not supported"
          in
            W.BR label :: acc
          end
      | SOME (CONTINUE_TO {label, which, params}) =>
          let
            val funcTypeIdx = getClosureFuncTypeIdx ctx nArgs
            val closureLocal = allocLocal fctx eqref
            val acc = emitClosureCall acc applied args funcTypeIdx closureLocal
            val acc = W.CALL_REF funcTypeIdx :: acc
            val acc =
              case params of
                [SOME p] => W.LOCAL_SET p :: acc
              | [NONE] => W.DROP :: acc
              | _ =>
                  raise CodeGenError
                    "doApp: multi-value continuation not supported"
            val acc =
              case which of
                NONE => acc
              | SOME (whichVar, whichVal) =>
                  W.LOCAL_SET whichVar :: W.I32_CONST (Int32.fromInt whichVal)
                  :: acc
          in
            W.BR label :: acc
          end
      | NONE => raise CodeGenError "doApp: unknown continuation"
    end

  (* ==================== Continuation application ==================== *)

  and doAppCont (fctx: FuncContext) (env: Env)
    (applied: C.CVar, args: N.Exp list, acc: W.instr list) : W.instr list =
    let
      val contRepr = C.CVarMap.find (#continuations env, applied)
    in
      case contRepr of
        SOME RETURN =>
          (case args of
             [arg] => W.RETURN :: doExp fctx env (arg, acc)
           | [] => W.RETURN :: W.REF_NULL (W.AbsHeapType W.HEAP_NONE) :: acc
           | _ => raise CodeGenError "doAppCont: RETURN with multiple args")
      | SOME (BREAK_TO {label, params}) =>
          let
            val acc =
              ListPair.foldl
                (fn (SOME p, arg, a) => W.LOCAL_SET p :: doExp fctx env (arg, a)
                  | (NONE, arg, a) => W.DROP :: doExp fctx env (arg, a)) acc
                (params, args)
          in
            W.BR label :: acc
          end
      | SOME (CONTINUE_TO {label, which, params}) =>
          let
            val acc =
              ListPair.foldl
                (fn (SOME p, arg, a) => W.LOCAL_SET p :: doExp fctx env (arg, a)
                  | (NONE, arg, a) => W.DROP :: doExp fctx env (arg, a)) acc
                (params, args)
            val acc =
              case which of
                NONE => acc
              | SOME (whichVar, whichVal) =>
                  W.LOCAL_SET whichVar :: W.I32_CONST (Int32.fromInt whichVal)
                  :: acc
          in
            W.BR label :: acc
          end
      | NONE =>
          raise CodeGenError
            ("doAppCont: unknown continuation "
             ^ Int.toString (C.CVar.toInt applied))
    end

  (* ==================== Declarations ==================== *)

  (* Process a Let's dec list, handling ContDec specially by wrapping in block/br.
     For ContDec: generates block around the rest, then continuation body after. *)
  and doLetDecs (fctx: FuncContext) (env: Env)
    (decs: N.Dec list, finalCont: N.Stat, acc: W.instr list) : W.instr list =
    case decs of
      [] => doStat fctx env (finalCont, acc)
    | N.ContDec {name, params, body} :: restDecs =>
        (* ContDec: wrap remaining code in a block, run continuation body after.
           When AppCont jumps to this continuation, it sets param locals and does br
           to exit the block. Then the continuation body runs after the block end. *)
        let
          val paramLocals =
            List.map
              (fn (SOME _, ty) => SOME (allocLocal fctx (tyToWasmType ty))
                | (NONE, _) => NONE) params
          val env' = envWithCont
            (env, name, BREAK_TO {label = 0, params = paramLocals})
          (* Generate the code inside the block (rest of decs + final cont) *)
          val innerCode = List.rev
            (doLetDecs fctx env' (restDecs, finalCont, []))
          (* Build env for continuation body with param locals *)
          val bodyEnv =
            ListPair.foldl
              (fn ((SOME v, _), SOME localIdx, e) => envWithVar (e, v, localIdx)
                | (_, _, e) => e) env (params, paramLocals)
          val contBodyCode = List.rev (doStat fctx bodyEnv (body, []))
        in
          (* block ... end ; contBody *)
          List.revAppend
            (contBodyCode, W.BLOCK (W.BlockTypeNone, innerCode) :: acc)
        end
    | N.RecContDec defs :: restDecs =>
        let val (env', acc') = doRecContDec fctx env (defs, acc)
        in doLetDecs fctx env' (restDecs, finalCont, acc')
        end
    | dec :: restDecs =>
        let val (env', acc') = doDec fctx env (dec, acc)
        in doLetDecs fctx env' (restDecs, finalCont, acc')
        end

  and doDec (fctx: FuncContext) (env: Env) (dec: N.Dec, acc: W.instr list) :
    Env * W.instr list =
    case dec of
      N.ValDec {exp, results} => doValDec fctx env (exp, results, acc)
    | N.RecDec decs => doRecDec fctx env (decs, acc)
    | N.ContDec {name, params, body} =>
        (* This path is for ContDec not handled by doLetDecs (shouldn't normally happen) *)
        raise CodeGenError "doDec: ContDec should be handled by doLetDecs"
    | N.RecContDec defs => doRecContDec fctx env (defs, acc)
    | N.ESImportDec _ =>
        raise CodeGenError "doDec: ESImportDec not supported in Wasm"

  (* ==================== ValDec ==================== *)

  and doValDec (fctx: FuncContext) (env: Env)
    (exp: N.Exp, results: (C.Var option * F.Ty) list, acc: W.instr list) :
    Env * W.instr list =
    case results of
      [(SOME v, ty)] =>
        let
          val localIdx = allocLocal fctx (tyToWasmType ty)
          val acc = W.LOCAL_SET localIdx :: doExp fctx env (exp, acc)
          val env' = envWithVar (env, v, localIdx)
        in
          (env', acc)
        end
    | [(NONE, _)] => (env, W.DROP :: doExp fctx env (exp, acc))
    | [] => (env, W.DROP :: doExp fctx env (exp, acc))
    | _ =>
        (* Multiple results: evaluate exp (should be a tuple), project each *)
        raise CodeGenError "doValDec: multiple results not yet implemented"

  (* ==================== RecDec (recursive functions) ==================== *)

  and doRecDec (fctx: FuncContext) (env: Env)
    ( decs:
        { name: C.Var
        , contParam: C.CVar
        , params: (C.Var * F.Ty) list
        , body: N.Stat
        , resultTy: F.Ty
        , attr: C.AbsAttr
        } list
    , acc: W.instr list
    ) : Env * W.instr list =
    let
      val ctx = #ctx fctx
    in
      case decs of
        [single] =>
          (* Single recursive function: can use mutable closure with backpatching *)
          let
            val {name, contParam, params, body, resultTy = _, attr = _} = single

            (* Collect free variables (excluding self) *)
            val bodyFV = freeVarsStat (body, TypedSyntax.VIdSet.empty)
            val paramSet =
              List.foldl (fn ((v, _), s) => TypedSyntax.VIdSet.add (s, v))
                TypedSyntax.VIdSet.empty params
            val allFreeVars = TypedSyntax.VIdSet.difference (bodyFV, paramSet)
            val freeVarSet =
              if TypedSyntax.VIdSet.member (allFreeVars, name) then
                TypedSyntax.VIdSet.delete (allFreeVars, name)
              else
                allFreeVars
            val freeVars = TypedSyntax.VIdSet.listItems freeVarSet
            val selfIsUsed = TypedSyntax.VIdSet.member (allFreeVars, name)

            val nParams = List.length params
            val nFreeVars = List.length freeVars + (if selfIsUsed then 1 else 0)

            val funcTypeIdx = getClosureFuncTypeIdx ctx nParams
            val closureTypeIdx = getMutClosureTypeIdx ctx nFreeVars funcTypeIdx

            (* Create a new context for the inner function *)
            val innerFctx = newFuncContext ctx

            val selfLocal = allocLocal innerFctx eqref
            val paramLocals =
              List.map (fn _ => allocLocal innerFctx eqref) params

            val innerEnv = emptyEnv
            val innerEnv = envWithCont (innerEnv, contParam, RETURN)
            val innerEnv =
              ListPair.foldl
                (fn ((v, _), localIdx, e) => envWithVar (e, v, localIdx))
                innerEnv (params, paramLocals)

            (* Extract free vars from closure into reverse preamble *)
            val (revPreamble, innerEnv, _) =
              List.foldl
                (fn (fv, (revAcc, e, fi)) =>
                   let
                     val localIdx = allocLocal innerFctx eqref
                   in
                     ( W.LOCAL_SET localIdx :: W.STRUCT_GET (closureTypeIdx, fi)
                       ::
                       W.REF_CAST
                         {nullable = false, heaptype = W.TypeIdx closureTypeIdx}
                       :: W.LOCAL_GET selfLocal :: revAcc
                     , envWithVar (e, fv, localIdx)
                     , fi + 1
                     )
                   end) ([], innerEnv, 1) freeVars

            (* If self is used as free var, extract it too *)
            val (revPreamble, innerEnv) =
              if selfIsUsed then
                let
                  val localIdx = allocLocal innerFctx eqref
                  val fi = 1 + List.length freeVars
                in
                  ( W.LOCAL_SET localIdx :: W.STRUCT_GET (closureTypeIdx, fi)
                    ::
                    W.REF_CAST
                      {nullable = false, heaptype = W.TypeIdx closureTypeIdx}
                    :: W.LOCAL_GET selfLocal :: revPreamble
                  , envWithVar (innerEnv, name, localIdx)
                  )
                end
              else
                (revPreamble, innerEnv)

            val bodyInstrs = List.rev
              (doStat innerFctx innerEnv (body, revPreamble))

            val totalLocals = !(#nextLocalIdx innerFctx)
            val nWasmParams = 1 + nParams
            val extraLocals = totalLocals - nWasmParams
            val allLocalTypes = List.rev (!(#revLocalTypes innerFctx))
            val localTypes = List.drop (allLocalTypes, nWasmParams)

            val funcIdx = allocFuncIdx ctx
            val func: W.func =
              {typeidx = funcTypeIdx, locals = localTypes, body = bodyInstrs}
            val () = #revFuncs ctx := func :: !(#revFuncs ctx)

            (* Create closure at call site *)
            val closureLocal = allocLocal fctx eqref
            val acc = W.REF_FUNC funcIdx :: acc
            val acc =
              List.foldl
                (fn (fv, a) =>
                   case TypedSyntax.VIdMap.find (#vars env, fv) of
                     SOME idx => W.LOCAL_GET idx :: a
                   | NONE =>
                       raise CodeGenError "doRecDec: free var not in scope") acc
                freeVars
            val acc =
              if selfIsUsed then W.REF_NULL (W.AbsHeapType W.HEAP_NONE) :: acc
              else acc
            val acc = W.STRUCT_NEW closureTypeIdx :: acc
            val acc = W.LOCAL_SET closureLocal :: acc

            (* Backpatch self reference *)
            val acc =
              if selfIsUsed then
                W.STRUCT_SET (closureTypeIdx, 1 + List.length freeVars)
                :: W.LOCAL_GET closureLocal
                ::
                W.REF_CAST
                  {nullable = false, heaptype = W.TypeIdx closureTypeIdx}
                :: W.LOCAL_GET closureLocal :: acc
              else
                acc

            val env' = envWithVar (env, name, closureLocal)
          in
            (env', acc)
          end
      | _ =>
          (* Multiple recursive functions: more complex, use mutable closures *)
          raise CodeGenError "doRecDec: mutual recursion not yet implemented"
    end

  (* ==================== ContDec (continuation) ==================== *)

  and doContDec (fctx: FuncContext) (env: Env)
    ( name: C.CVar
    , params: (C.Var option * F.Ty) list
    , body: N.Stat
    , acc: W.instr list
    ) : Env * W.instr list =
    let
      (* Allocate locals for params *)
      val paramLocals =
        List.map
          (fn (SOME _, ty) => SOME (allocLocal fctx (tyToWasmType ty))
            | (NONE, _) => NONE) params
    in
      (* Store cont info in env; the actual block wrapping happens in doStat for Let *)
      (envWithCont (env, name, BREAK_TO {label = 0, params = paramLocals}), acc)
    end

  (* ==================== RecContDec (recursive continuations) ==================== *)

  and doRecContDec (fctx: FuncContext) (env: Env)
    ( defs: (C.CVar * (C.Var option * F.Ty) list * N.Stat) list
    , acc: W.instr list
    ) : Env * W.instr list =
    let
      (* Allocate a "which" local to track which continuation to jump to *)
      val whichLocal = allocLocal fctx (W.NumType W.I32)

      (* Allocate locals for each continuation's params *)
      val defsWithLocals =
        let
          fun go (_, []) = []
            | go (i, (name, params, body) :: rest) =
                let
                  val paramLocals =
                    List.map
                      (fn (SOME _, ty) =>
                         SOME (allocLocal fctx (tyToWasmType ty))
                        | (NONE, _) => NONE) params
                in
                  (name, params, body, paramLocals, i) :: go (i + 1, rest)
                end
        in
          go (0, defs)
        end

      (* Register continuations in env *)
      val env' =
        List.foldl
          (fn ((name, _, _, paramLocals, i), e) =>
             envWithCont (e, name, CONTINUE_TO
               {label = 0, which = SOME (whichLocal, i), params = paramLocals}))
          env defsWithLocals
    in
      (env', acc)
    end

  (* ==================== PrimOp ==================== *)

  and doPrimOp (fctx: FuncContext) (env: Env)
    (primOp: F.PrimOp, tyargs: F.Ty list, args: N.Exp list, acc: W.instr list) :
    W.instr list =
    let
      val ctx = #ctx fctx
    in
      case (primOp, tyargs, args) of
      (* ---- Constant generation ---- *)
        (F.IntConstOp n, _, _) => W.I32_CONST (Int32.fromLarge n) :: acc
      | (F.WordConstOp n, _, _) => W.I32_CONST (Int32.fromLarge n) :: acc
      | (F.RealConstOp x, _, _) =>
          let
            val y =
              Numeric.toDecimal
                { nominal_format = Numeric.binary64
                , target_format = Numeric.binary64
                } x
          in
            case y of
              SOME z =>
                let
                  val s = Numeric.Notation.toString "~" z
                in
                  case Real.fromString s of
                    SOME r => W.F64_CONST r :: acc
                  | NONE =>
                      raise CodeGenError "doPrimOp: cannot parse real constant"
                end
            | NONE =>
                raise CodeGenError "doPrimOp: real constant not representable"
          end
      | (F.Char7ConstOp c, _, _) =>
          W.I32_CONST (Int32.fromInt (Char.ord c)) :: acc
      | (F.Char8ConstOp c, _, _) =>
          W.I32_CONST (Int32.fromInt (Char.ord c)) :: acc
      | (F.Char16ConstOp c, _, _) => W.I32_CONST (Int32.fromInt c) :: acc
      | (F.Char32ConstOp c, _, _) => W.I32_CONST (Int32.fromInt c) :: acc
      | (F.UCharConstOp c, _, _) => W.I32_CONST (Int32.fromInt c) :: acc
      | (F.String7ConstOp _, _, _) =>
          W.REF_NULL (W.AbsHeapType W.HEAP_NONE) :: acc (* TODO *)
      | (F.String8ConstOp _, _, _) =>
          W.REF_NULL (W.AbsHeapType W.HEAP_NONE) :: acc (* TODO *)
      | (F.String16ConstOp _, _, _) =>
          raise CodeGenError "doPrimOp: String16ConstOp not yet implemented"
      | (F.String32ConstOp _, _, _) =>
          raise CodeGenError "doPrimOp: String32ConstOp not yet implemented"

      (* ---- Box/Unbox ---- *)
      | (F.BoxOp ubt, _, [arg]) =>
          List.revAppend (emitBox (ubt, ctx), doExp fctx env (arg, acc))
      | (F.UnboxOp ubt, _, [arg]) =>
          List.revAppend (emitUnbox (ubt, ctx), doExp fctx env (arg, acc))

      (* ---- Data type operations ---- *)
      | (F.ConstructValOp info, _, []) =>
          (case #representation info of
             Syntax.REP_BOXED =>
               W.REF_I31 :: W.I32_CONST 0
               :: acc (* placeholder: use tag string hash or index *)
           | Syntax.REP_ENUM =>
               W.REF_I31 :: W.I32_CONST 0 :: acc (* TODO: proper tag encoding *)
           | Syntax.REP_UNIT => W.REF_NULL (W.AbsHeapType W.HEAP_NONE) :: acc
           | Syntax.REP_BOOL =>
               W.I32_CONST 0 :: acc (* Should not happen normally *)
           | _ =>
               raise CodeGenError
                 "doPrimOp: unexpected representation for ConstructValOp")
      | (F.ConstructValWithPayloadOp info, _, [payload]) =>
          (case #representation info of
             Syntax.REP_BOXED =>
               let
                 val tupleIdx = getTupleTypeIdx ctx 2
                 val acc = W.I32_CONST 0 :: acc
                 val acc = W.REF_I31 :: acc (* tag as i31ref *)
                 val acc = doExp fctx env (payload, acc)
               in
                 W.STRUCT_NEW tupleIdx :: acc
               end
           | Syntax.REP_ALIAS => doExp fctx env (payload, acc)
           | Syntax.REP_LIST =>
               doExp fctx env
                 (payload, acc) (* payload is already a cons cell from the IR *)
           | _ =>
               raise CodeGenError
                 "doPrimOp: unexpected representation for ConstructValWithPayloadOp")
      | (F.DataPayloadOp info, _, [arg]) =>
          (case #representation info of
             Syntax.REP_BOXED =>
               let val tupleIdx = getTupleTypeIdx ctx 2
               in W.STRUCT_GET (tupleIdx, 1) :: doExp fctx env (arg, acc)
               end
           | Syntax.REP_ALIAS => doExp fctx env (arg, acc)
           | _ =>
               raise CodeGenError
                 "doPrimOp: unexpected representation for DataPayloadOp")
      | (F.DataTagAsStringOp _, _, [_]) =>
          W.REF_NULL (W.AbsHeapType W.HEAP_NONE) :: acc (* TODO *)
      | (F.DataTagAsString16Op _, _, [_]) =>
          W.REF_NULL (W.AbsHeapType W.HEAP_NONE) :: acc (* TODO *)
      | (F.ExnPayloadOp, [payloadTy], [arg]) =>
          (* Extract payload (field 1) from $SmlExn struct.
             The payload is stored as eqref; if the caller expects an unboxed
             type, emit unbox instructions after extracting.
             NOTE: acc is a reversed accumulator; instructions are prepended in
             reverse execution order. The desired execution order is:
               arg → ref.cast($SmlExn) → struct.get(5,1) → [unbox] *)
          let
            val smlExnTypeIdx = #smlExnTypeIdx ctx
            (* Base: push arg, cast to $SmlExn, get payload field *)
            val baseAcc =
              W.STRUCT_GET (smlExnTypeIdx, 1)
              ::
              W.REF_CAST {nullable = false, heaptype = W.TypeIdx smlExnTypeIdx}
              :: doExp fctx env (arg, acc)
          in
            case tyToUnboxedTy payloadTy of
              SOME ubt => List.revAppend (emitUnbox (ubt, ctx), baseAcc)
            | NONE => baseAcc
          end
      | (F.ConstructExnOp, _, [tag]) =>
          (* Build $SmlExn struct with tag and null payload *)
          let
            val smlExnTypeIdx = #smlExnTypeIdx ctx
            val acc = doExp fctx env (tag, acc)
            val acc = W.REF_NULL (W.AbsHeapType W.HEAP_NONE) :: acc
          in
            W.STRUCT_NEW smlExnTypeIdx :: acc
          end
      | (F.ConstructExnWithPayloadOp, [payloadTy], [tag, payload]) =>
          (* Build $SmlExn struct with tag and payload (eqref).
             If the payload is an unboxed type (e.g., int), box it first. *)
          let
            val smlExnTypeIdx = #smlExnTypeIdx ctx
            val acc = doExp fctx env (tag, acc)
            val acc = doExp fctx env (payload, acc)
            val acc =
              case tyToUnboxedTy payloadTy of
                SOME ubt => List.revAppend (emitBox (ubt, ctx), acc)
              | NONE => acc
          in
            W.STRUCT_NEW smlExnTypeIdx :: acc
          end
      | (F.RaiseOp _, _, _) =>
          raise CodeGenError "doPrimOp: RaiseOp should not appear in NSyntax"

      (* ---- List operations ---- *)
      | (F.ListOp, _, []) => W.REF_NULL (W.AbsHeapType W.HEAP_NONE) :: acc
      | (F.ListOp, _, _) =>
          raise CodeGenError
            "doPrimOp: ListOp with elements not yet implemented"

      (* ---- Vector operations ---- *)
      | (F.VectorOp, _, _) =>
          raise CodeGenError "doPrimOp: VectorOp not yet implemented"

      (* ---- PrimCall ---- *)
      | (F.PrimCall prim, _, args) => doPrimCall fctx env (prim, args, acc)

      (* ---- Lua/JS specific ops ---- *)
      | (F.JsCallOp, _, _) =>
          raise CodeGenError "JsCallOp not supported in Wasm"
      | (F.JsMethodOp, _, _) =>
          raise CodeGenError "JsMethodOp not supported in Wasm"
      | (F.JsNewOp, _, _) => raise CodeGenError "JsNewOp not supported in Wasm"
      | (F.LuaCallOp, _, _) =>
          raise CodeGenError "LuaCallOp not supported in Wasm"
      | (F.LuaCall1Op, _, _) =>
          raise CodeGenError "LuaCall1Op not supported in Wasm"
      | (F.LuaCallNOp _, _, _) =>
          raise CodeGenError "LuaCallNOp not supported in Wasm"
      | (F.LuaMethodOp _, _, _) =>
          raise CodeGenError "LuaMethodOp not supported in Wasm"
      | (F.LuaMethod1Op _, _, _) =>
          raise CodeGenError "LuaMethod1Op not supported in Wasm"
      | (F.LuaMethodNOp _, _, _) =>
          raise CodeGenError "LuaMethodNOp not supported in Wasm"

      | _ => raise CodeGenError "doPrimOp: unhandled primOp"
    end

  (* ==================== PrimCall ==================== *)

  and doPrimCall (fctx: FuncContext) (env: Env)
    (prim: Primitives.PrimOp, args: N.Exp list, acc: W.instr list) :
    W.instr list =
    let
      val ctx = #ctx fctx
      (* doUnary: emit arg, then suffix instructions *)
      fun doUnary f [arg] =
            List.revAppend (f, doExp fctx env (arg, acc))
        | doUnary _ _ = raise CodeGenError "doPrimCall: expected 1 arg"
      (* doBinary: emit arg1, arg2, then suffix instructions *)
      fun doBinary f [a, b] =
            List.revAppend (f, doExp fctx env (b, doExp fctx env (a, acc)))
        | doBinary _ _ = raise CodeGenError "doPrimCall: expected 2 args"
    in
      case prim of
      (* ---- Boolean ---- *)
        Primitives.Bool_not => doUnary [W.I32_CONST 1, W.I32_BINOP W.XOR] args
      | Primitives.Bool_EQUAL => doBinary [W.I32_RELOP W.IEQ] args

      (* ---- Int32 arithmetic ---- *)
      | Primitives.Int_PLUS Primitives.I32 => doBinary [W.I32_BINOP W.ADD] args
      | Primitives.Int_PLUS_wrapping Primitives.I32 =>
          doBinary [W.I32_BINOP W.ADD] args
      | Primitives.Int_MINUS Primitives.I32 => doBinary [W.I32_BINOP W.SUB] args
      | Primitives.Int_MINUS_wrapping Primitives.I32 =>
          doBinary [W.I32_BINOP W.SUB] args
      | Primitives.Int_TIMES Primitives.I32 => doBinary [W.I32_BINOP W.MUL] args
      | Primitives.Int_TIMES_wrapping Primitives.I32 =>
          doBinary [W.I32_BINOP W.MUL] args
      | Primitives.Int_div Primitives.I32 => doBinary [W.I32_BINOP W.DIV_S] args
      | Primitives.Int_div_unchecked Primitives.I32 =>
          doBinary [W.I32_BINOP W.DIV_S] args
      | Primitives.Int_mod Primitives.I32 => doBinary [W.I32_BINOP W.REM_S] args
      | Primitives.Int_mod_unchecked Primitives.I32 =>
          doBinary [W.I32_BINOP W.REM_S] args
      | Primitives.Int_quot Primitives.I32 =>
          doBinary [W.I32_BINOP W.DIV_S] args
      | Primitives.Int_quot_unchecked Primitives.I32 =>
          doBinary [W.I32_BINOP W.DIV_S] args
      | Primitives.Int_rem Primitives.I32 => doBinary [W.I32_BINOP W.REM_S] args
      | Primitives.Int_rem_unchecked Primitives.I32 =>
          doBinary [W.I32_BINOP W.REM_S] args
      | Primitives.Int_TILDE Primitives.I32 =>
          (* 0 - x *)
          (case args of
             [arg] =>
               W.I32_BINOP W.SUB :: doExp fctx env (arg, W.I32_CONST 0 :: acc)
           | _ => raise CodeGenError "Int_TILDE: expected 1 arg")
      | Primitives.Int_TILDE_unchecked Primitives.I32 =>
          (case args of
             [arg] =>
               W.I32_BINOP W.SUB :: doExp fctx env (arg, W.I32_CONST 0 :: acc)
           | _ => raise CodeGenError "Int_TILDE_unchecked: expected 1 arg")
      | Primitives.Int_TILDE_wrapping Primitives.I32 =>
          (case args of
             [arg] =>
               W.I32_BINOP W.SUB :: doExp fctx env (arg, W.I32_CONST 0 :: acc)
           | _ => raise CodeGenError "Int_TILDE_wrapping: expected 1 arg")
      | Primitives.Int_abs Primitives.I32 =>
          (* abs(x) = if x < 0 then -x else x *)
          (case args of
             [arg] =>
               let
                 val localIdx = allocLocal fctx (W.NumType W.I32)
                 val acc = doExp fctx env (arg, acc)
                 val acc = W.LOCAL_TEE localIdx :: acc
                 val acc = W.I32_CONST 0 :: acc
                 val acc = W.I32_RELOP W.LT_S :: acc
               in
                 W.IF
                   ( W.BlockTypeVal (W.NumType W.I32)
                   , [W.I32_CONST 0, W.LOCAL_GET localIdx, W.I32_BINOP W.SUB]
                   , [W.LOCAL_GET localIdx]
                   ) :: acc
               end
           | _ => raise CodeGenError "Int_abs: expected 1 arg")

      (* ---- Int32 comparison ---- *)
      | Primitives.Int_EQUAL Primitives.I32 => doBinary [W.I32_RELOP W.IEQ] args
      | Primitives.Int_LT Primitives.I32 => doBinary [W.I32_RELOP W.LT_S] args
      | Primitives.Int_LE Primitives.I32 => doBinary [W.I32_RELOP W.LE_S] args
      | Primitives.Int_GT Primitives.I32 => doBinary [W.I32_RELOP W.GT_S] args
      | Primitives.Int_GE Primitives.I32 => doBinary [W.I32_RELOP W.GE_S] args

      (* ---- Int64 arithmetic ---- *)
      | Primitives.Int_PLUS Primitives.I64 => doBinary [W.I64_BINOP W.ADD] args
      | Primitives.Int_PLUS_wrapping Primitives.I64 =>
          doBinary [W.I64_BINOP W.ADD] args
      | Primitives.Int_MINUS Primitives.I64 => doBinary [W.I64_BINOP W.SUB] args
      | Primitives.Int_MINUS_wrapping Primitives.I64 =>
          doBinary [W.I64_BINOP W.SUB] args
      | Primitives.Int_TIMES Primitives.I64 => doBinary [W.I64_BINOP W.MUL] args
      | Primitives.Int_TIMES_wrapping Primitives.I64 =>
          doBinary [W.I64_BINOP W.MUL] args
      | Primitives.Int_div Primitives.I64 => doBinary [W.I64_BINOP W.DIV_S] args
      | Primitives.Int_div_unchecked Primitives.I64 =>
          doBinary [W.I64_BINOP W.DIV_S] args
      | Primitives.Int_mod Primitives.I64 => doBinary [W.I64_BINOP W.REM_S] args
      | Primitives.Int_mod_unchecked Primitives.I64 =>
          doBinary [W.I64_BINOP W.REM_S] args
      | Primitives.Int_quot Primitives.I64 =>
          doBinary [W.I64_BINOP W.DIV_S] args
      | Primitives.Int_quot_unchecked Primitives.I64 =>
          doBinary [W.I64_BINOP W.DIV_S] args
      | Primitives.Int_rem Primitives.I64 => doBinary [W.I64_BINOP W.REM_S] args
      | Primitives.Int_rem_unchecked Primitives.I64 =>
          doBinary [W.I64_BINOP W.REM_S] args
      | Primitives.Int_TILDE Primitives.I64 =>
          (case args of
             [arg] =>
               W.I64_BINOP W.SUB :: doExp fctx env (arg, W.I64_CONST 0 :: acc)
           | _ => raise CodeGenError "Int_TILDE I64: expected 1 arg")
      | Primitives.Int_TILDE_unchecked Primitives.I64 =>
          (case args of
             [arg] =>
               W.I64_BINOP W.SUB :: doExp fctx env (arg, W.I64_CONST 0 :: acc)
           | _ => raise CodeGenError "Int_TILDE_unchecked I64: expected 1 arg")
      | Primitives.Int_TILDE_wrapping Primitives.I64 =>
          (case args of
             [arg] =>
               W.I64_BINOP W.SUB :: doExp fctx env (arg, W.I64_CONST 0 :: acc)
           | _ => raise CodeGenError "Int_TILDE_wrapping I64: expected 1 arg")

      (* ---- Int64 comparison ---- *)
      | Primitives.Int_EQUAL Primitives.I64 => doBinary [W.I64_RELOP W.IEQ] args
      | Primitives.Int_LT Primitives.I64 => doBinary [W.I64_RELOP W.LT_S] args
      | Primitives.Int_LE Primitives.I64 => doBinary [W.I64_RELOP W.LE_S] args
      | Primitives.Int_GT Primitives.I64 => doBinary [W.I64_RELOP W.GT_S] args
      | Primitives.Int_GE Primitives.I64 => doBinary [W.I64_RELOP W.GE_S] args

      (* ---- Int width conversion ---- *)
      | Primitives.Int_toInt_unchecked (Primitives.I32, Primitives.I64) =>
          doUnary [W.I64_EXTEND_I32 W.S] args
      | Primitives.Int_toInt_unchecked (Primitives.I64, Primitives.I32) =>
          doUnary [W.I32_WRAP_I64] args

      (* ---- Word32 arithmetic ---- *)
      | Primitives.Word_PLUS Primitives.W32 => doBinary [W.I32_BINOP W.ADD] args
      | Primitives.Word_MINUS Primitives.W32 =>
          doBinary [W.I32_BINOP W.SUB] args
      | Primitives.Word_TIMES Primitives.W32 =>
          doBinary [W.I32_BINOP W.MUL] args
      | Primitives.Word_div Primitives.W32 =>
          doBinary [W.I32_BINOP W.DIV_U] args
      | Primitives.Word_div_unchecked Primitives.W32 =>
          doBinary [W.I32_BINOP W.DIV_U] args
      | Primitives.Word_mod Primitives.W32 =>
          doBinary [W.I32_BINOP W.REM_U] args
      | Primitives.Word_mod_unchecked Primitives.W32 =>
          doBinary [W.I32_BINOP W.REM_U] args
      | Primitives.Word_TILDE Primitives.W32 =>
          (case args of
             [arg] =>
               W.I32_BINOP W.SUB :: doExp fctx env (arg, W.I32_CONST 0 :: acc)
           | _ => raise CodeGenError "Word_TILDE: expected 1 arg")

      (* ---- Word32 comparison ---- *)
      | Primitives.Word_EQUAL Primitives.W32 =>
          doBinary [W.I32_RELOP W.IEQ] args
      | Primitives.Word_LT Primitives.W32 => doBinary [W.I32_RELOP W.LT_U] args
      | Primitives.Word_LE Primitives.W32 => doBinary [W.I32_RELOP W.LE_U] args
      | Primitives.Word_GT Primitives.W32 => doBinary [W.I32_RELOP W.GT_U] args
      | Primitives.Word_GE Primitives.W32 => doBinary [W.I32_RELOP W.GE_U] args

      (* ---- Word32 bitwise ---- *)
      | Primitives.Word_andb Primitives.W32 => doBinary [W.I32_BINOP W.AND] args
      | Primitives.Word_orb Primitives.W32 => doBinary [W.I32_BINOP W.OR] args
      | Primitives.Word_xorb Primitives.W32 => doBinary [W.I32_BINOP W.XOR] args
      | Primitives.Word_notb Primitives.W32 =>
          doUnary [W.I32_CONST ~1, W.I32_BINOP W.XOR] args

      (* ---- Word32 shift ---- *)
      | Primitives.Word_LSHIFT_unchecked (Primitives.W32, _) =>
          doBinary [W.I32_BINOP W.SHL] args
      | Primitives.Word_RSHIFT_unchecked (Primitives.W32, _) =>
          doBinary [W.I32_BINOP W.SHR_U] args

      (* ---- Word64 arithmetic ---- *)
      | Primitives.Word_PLUS Primitives.W64 => doBinary [W.I64_BINOP W.ADD] args
      | Primitives.Word_MINUS Primitives.W64 =>
          doBinary [W.I64_BINOP W.SUB] args
      | Primitives.Word_TIMES Primitives.W64 =>
          doBinary [W.I64_BINOP W.MUL] args
      | Primitives.Word_div Primitives.W64 =>
          doBinary [W.I64_BINOP W.DIV_U] args
      | Primitives.Word_div_unchecked Primitives.W64 =>
          doBinary [W.I64_BINOP W.DIV_U] args
      | Primitives.Word_mod Primitives.W64 =>
          doBinary [W.I64_BINOP W.REM_U] args
      | Primitives.Word_mod_unchecked Primitives.W64 =>
          doBinary [W.I64_BINOP W.REM_U] args

      (* ---- Word64 comparison ---- *)
      | Primitives.Word_EQUAL Primitives.W64 =>
          doBinary [W.I64_RELOP W.IEQ] args
      | Primitives.Word_LT Primitives.W64 => doBinary [W.I64_RELOP W.LT_U] args
      | Primitives.Word_LE Primitives.W64 => doBinary [W.I64_RELOP W.LE_U] args
      | Primitives.Word_GT Primitives.W64 => doBinary [W.I64_RELOP W.GT_U] args
      | Primitives.Word_GE Primitives.W64 => doBinary [W.I64_RELOP W.GE_U] args

      (* ---- Word64 bitwise ---- *)
      | Primitives.Word_andb Primitives.W64 => doBinary [W.I64_BINOP W.AND] args
      | Primitives.Word_orb Primitives.W64 => doBinary [W.I64_BINOP W.OR] args
      | Primitives.Word_xorb Primitives.W64 => doBinary [W.I64_BINOP W.XOR] args
      | Primitives.Word_notb Primitives.W64 =>
          doUnary [W.I64_CONST ~1, W.I64_BINOP W.XOR] args

      (* ---- Word64 shift ---- *)
      | Primitives.Word_LSHIFT_unchecked (Primitives.W64, _) =>
          doBinary [W.I64_BINOP W.SHL] args
      | Primitives.Word_RSHIFT_unchecked (Primitives.W64, _) =>
          doBinary [W.I64_BINOP W.SHR_U] args

      (* ---- Real (F64) arithmetic ---- *)
      | Primitives.Real_PLUS => doBinary [W.F64_BINOP W.FADD] args
      | Primitives.Real_MINUS => doBinary [W.F64_BINOP W.FSUB] args
      | Primitives.Real_TIMES => doBinary [W.F64_BINOP W.FMUL] args
      | Primitives.Real_DIVIDE => doBinary [W.F64_BINOP W.FDIV] args
      | Primitives.Real_TILDE => doUnary [W.F64_UNOP W.NEG] args
      | Primitives.Real_abs => doUnary [W.F64_UNOP W.ABS] args

      (* ---- Real comparison ---- *)
      | Primitives.Real_LT => doBinary [W.F64_RELOP W.FLT] args
      | Primitives.Real_LE => doBinary [W.F64_RELOP W.FLE] args
      | Primitives.Real_GT => doBinary [W.F64_RELOP W.FGT] args
      | Primitives.Real_GE => doBinary [W.F64_RELOP W.FGE] args

      (* ---- Char comparisons ---- *)
      | Primitives.Char_EQUAL => doBinary [W.I32_RELOP W.IEQ] args
      | Primitives.Char_LT => doBinary [W.I32_RELOP W.LT_U] args
      | Primitives.Char_LE => doBinary [W.I32_RELOP W.LE_U] args
      | Primitives.Char_GT => doBinary [W.I32_RELOP W.GT_U] args
      | Primitives.Char_GE => doBinary [W.I32_RELOP W.GE_U] args
      | Primitives.Char_ord _ => doUnary [] args (* char is already i32 *)
      | Primitives.Char_chr_unchecked _ =>
          doUnary [] args (* int is already i32 *)
      | Primitives.Char_fromChar7 => doUnary [] args

      | Primitives.Char7_EQUAL => doBinary [W.I32_RELOP W.IEQ] args
      | Primitives.Char7_LT => doBinary [W.I32_RELOP W.LT_U] args
      | Primitives.Char7_LE => doBinary [W.I32_RELOP W.LE_U] args
      | Primitives.Char7_GT => doBinary [W.I32_RELOP W.GT_U] args
      | Primitives.Char7_GE => doBinary [W.I32_RELOP W.GE_U] args
      | Primitives.Char7_ord _ => doUnary [] args
      | Primitives.Char7_chr_unchecked _ => doUnary [] args

      | Primitives.Char16_EQUAL => doBinary [W.I32_RELOP W.IEQ] args
      | Primitives.Char16_LT => doBinary [W.I32_RELOP W.LT_U] args
      | Primitives.Char16_LE => doBinary [W.I32_RELOP W.LE_U] args
      | Primitives.Char16_GT => doBinary [W.I32_RELOP W.GT_U] args
      | Primitives.Char16_GE => doBinary [W.I32_RELOP W.GE_U] args
      | Primitives.Char16_ord _ => doUnary [] args
      | Primitives.Char16_chr_unchecked _ => doUnary [] args
      | Primitives.Char16_fromChar7 => doUnary [] args

      | Primitives.Char32_EQUAL => doBinary [W.I32_RELOP W.IEQ] args
      | Primitives.Char32_LT => doBinary [W.I32_RELOP W.LT_U] args
      | Primitives.Char32_LE => doBinary [W.I32_RELOP W.LE_U] args
      | Primitives.Char32_GT => doBinary [W.I32_RELOP W.GT_U] args
      | Primitives.Char32_GE => doBinary [W.I32_RELOP W.GE_U] args
      | Primitives.Char32_ord _ => doUnary [] args
      | Primitives.Char32_chr_unchecked _ => doUnary [] args

      (* ---- Ref cells ---- *)
      | Primitives.Ref_ref =>
          (* Create a 1-field mutable struct *)
          (case args of
             [arg] =>
               let
                 val refTypeIdx = allocTypeIdx ctx
                 val subtype = W.SubType
                   { final = false
                   , supertypes = []
                   , body = W.StructType
                       [{mut = W.VAR, storagetype = W.ValStorageType eqref}]
                   }
                 val () = #revTypes ctx := [subtype] :: !(#revTypes ctx)
               in
                 W.STRUCT_NEW refTypeIdx :: doExp fctx env (arg, acc)
               end
           | _ => raise CodeGenError "Ref_ref: expected 1 arg")
      | Primitives.Ref_EQUAL => doBinary [W.REF_EQ] args
      | Primitives.Ref_set =>
          raise CodeGenError
            "Ref_set: not yet implemented (needs known ref type)"
      | Primitives.Ref_read =>
          raise CodeGenError
            "Ref_read: not yet implemented (needs known ref type)"

      (* ---- List operations ---- *)
      | Primitives.List_cons =>
          (case args of
             [hd, tl] =>
               let
                 val tupleIdx = getTupleTypeIdx ctx 2
                 val acc = doExp fctx env (hd, acc)
                 val acc = doExp fctx env (tl, acc)
               in
                 W.STRUCT_NEW tupleIdx :: acc
               end
           | _ => raise CodeGenError "List_cons: expected 2 args")
      | Primitives.List_null => doUnary [W.REF_IS_NULL] args
      | Primitives.List_unsafeHead =>
          (case args of
             [lst] =>
               let val tupleIdx = getTupleTypeIdx ctx 2
               in W.STRUCT_GET (tupleIdx, 0) :: doExp fctx env (lst, acc)
               end
           | _ => raise CodeGenError "List_unsafeHead: expected 1 arg")
      | Primitives.List_unsafeTail =>
          (case args of
             [lst] =>
               let val tupleIdx = getTupleTypeIdx ctx 2
               in W.STRUCT_GET (tupleIdx, 1) :: doExp fctx env (lst, acc)
               end
           | _ => raise CodeGenError "List_unsafeTail: expected 1 arg")

      (* ---- Array operations ---- *)
      | Primitives.Array_length _ =>
          raise CodeGenError "Array_length: not yet implemented"

      (* ---- Conversion ---- *)
      | Primitives.Int_toInt_unchecked (from, to) =>
          (case (from, to) of
             (Primitives.I32, Primitives.I64) =>
               doUnary [W.I64_EXTEND_I32 W.S] args
           | (Primitives.I64, Primitives.I32) => doUnary [W.I32_WRAP_I64] args
           | (Primitives.I32, Primitives.I32) => doUnary [] args
           | (Primitives.I64, Primitives.I64) => doUnary [] args
           | _ =>
               raise CodeGenError "Int_toInt_unchecked: unsupported width pair")

      (* ---- Default int/word (mapped to I32/W32 for Wasm) ---- *)
      | Primitives.Int_PLUS Primitives.INT => doBinary [W.I32_BINOP W.ADD] args
      | Primitives.Int_MINUS Primitives.INT => doBinary [W.I32_BINOP W.SUB] args
      | Primitives.Int_TIMES Primitives.INT => doBinary [W.I32_BINOP W.MUL] args
      | Primitives.Int_div Primitives.INT => doBinary [W.I32_BINOP W.DIV_S] args
      | Primitives.Int_mod Primitives.INT => doBinary [W.I32_BINOP W.REM_S] args
      | Primitives.Int_EQUAL Primitives.INT => doBinary [W.I32_RELOP W.IEQ] args
      | Primitives.Int_LT Primitives.INT => doBinary [W.I32_RELOP W.LT_S] args
      | Primitives.Int_LE Primitives.INT => doBinary [W.I32_RELOP W.LE_S] args
      | Primitives.Int_GT Primitives.INT => doBinary [W.I32_RELOP W.GT_S] args
      | Primitives.Int_GE Primitives.INT => doBinary [W.I32_RELOP W.GE_S] args
      | Primitives.Word_PLUS Primitives.WORD =>
          doBinary [W.I32_BINOP W.ADD] args
      | Primitives.Word_MINUS Primitives.WORD =>
          doBinary [W.I32_BINOP W.SUB] args
      | Primitives.Word_TIMES Primitives.WORD =>
          doBinary [W.I32_BINOP W.MUL] args
      | Primitives.Word_EQUAL Primitives.WORD =>
          doBinary [W.I32_RELOP W.IEQ] args

      (* ---- Exception operations ---- *)
      | Primitives.Exception_instanceof =>
          (* Check if exception e matches tag: cast e to $SmlExn, get tag field, ref.eq with tag arg *)
          (case args of
             [e, tag] =>
               let
                 val smlExnTypeIdx = #smlExnTypeIdx ctx
               in
                 W.REF_EQ :: W.STRUCT_GET (smlExnTypeIdx, 0)
                 ::
                 W.REF_CAST
                   {nullable = false, heaptype = W.TypeIdx smlExnTypeIdx}
                 :: doExp fctx env (e, doExp fctx env (tag, acc))
               end
           | _ => raise CodeGenError "Exception_instanceof: expected 2 args")

      (* ---- General exn name ---- *)
      | Primitives.General_exnName =>
          W.REF_NULL (W.AbsHeapType W.HEAP_NONE) :: acc (* TODO *)

      (* ---- mkFn / call helpers ---- *)
      | Primitives.mkFn2 => raise CodeGenError "mkFn2: not yet implemented"
      | Primitives.mkFn3 => raise CodeGenError "mkFn3: not yet implemented"
      | Primitives.call2 => raise CodeGenError "call2: not yet implemented"
      | Primitives.call3 => raise CodeGenError "call3: not yet implemented"

      | _ =>
          raise CodeGenError
            ("doPrimCall: unhandled primitive: " ^ Primitives.toString prim)
    end

  (* ==================== Context initialization ==================== *)

  fun initContext () =
    let
      val boxedI32TypeIdx = 0
      val boxedI64TypeIdx = 1
      val boxedF64TypeIdx = 2
      val closureBaseTypeIdx = 3
      val exnTagTypeIdx = 4
      val smlExnTypeIdx = 5
      val smlExnFuncTypeIdx = 6
      val smlExnTagIdx = 0 (* first module-defined tag, no imported tags *)
      fun mkBoxedStruct numty =
        W.SubType
          { final = false
          , supertypes = []
          , body = W.StructType
              [{mut = W.CONST, storagetype = W.ValStorageType (W.NumType numty)}]
          }
      val boxedI32 = mkBoxedStruct W.I32
      val boxedI64 = mkBoxedStruct W.I64
      val boxedF64 = mkBoxedStruct W.F64
      (* ClosureBase: (sub eq (struct (field $code (ref func)))) *)
      val closureBase = W.SubType
        { final = false
        , supertypes = []
        , body = W.StructType
            [{ mut = W.CONST
             , storagetype = W.ValStorageType
                 (W.RefType {nullable = false, heaptype = W.AbsHeapType W.FUNC})
             }]
        }
      (* ExnTagType: empty struct, used for exception tag identity via ref.eq *)
      val exnTagType =
        W.SubType {final = false, supertypes = [], body = W.StructType []}
      (* SmlExnType: struct (field tag eqref) (field payload eqref) *)
      val smlExnType = W.SubType
        { final = false
        , supertypes = []
        , body = W.StructType
            [ {mut = W.CONST, storagetype = W.ValStorageType eqref}
            , {mut = W.CONST, storagetype = W.ValStorageType eqref}
            ]
        }
      (* SmlExnFuncType: functype for the $sml_exn exception tag: (func (param eqref)) *)
      val smlExnFuncType = W.SubType
        { final = true
        , supertypes = []
        , body = W.FuncType {params = [eqref], results = []}
        }
      (* Module-defined tag for SML exceptions *)
      val smlExnTag: W.tagtype = {functype = smlExnFuncTypeIdx}
    in
      { nextTypeIdx = ref 7
      , nextFuncIdx = ref 0
      , revTypes = ref
          [ [smlExnFuncType]
          , [smlExnType]
          , [exnTagType]
          , [closureBase]
          , [boxedF64]
          , [boxedI64]
          , [boxedI32]
          ]
      , revFuncs = ref []
      , revGlobals = ref []
      , revImports = ref []
      , revExports = ref []
      , revDatas = ref []
      , revTags = ref [smlExnTag]
      , tupleTyIdxMap = ref IntRedBlackMap.empty
      , funcTypeMap = ref []
      , closureBaseTypeIdx = closureBaseTypeIdx
      , boxedI32TypeIdx = boxedI32TypeIdx
      , boxedI64TypeIdx = boxedI64TypeIdx
      , boxedF64TypeIdx = boxedF64TypeIdx
      , exnTagTypeIdx = exnTagTypeIdx
      , smlExnTypeIdx = smlExnTypeIdx
      , smlExnTagIdx = smlExnTagIdx
      }
    end

  (* ==================== doProgram ==================== *)

  (* Sort export names to match LabelMap ordering (IdentifierLabel is sorted lexicographically) *)
  fun sortedExportNames (names: (string * ToFSyntax.export_sig) vector) :
    (int * string * ToFSyntax.export_sig) list =
    let
      val indexed =
        Vector.foldri (fn (i, (name, sig_), acc) => (i, name, sig_) :: acc) []
          names
      fun insert ((i, n, t), []) = [(i, n, t)]
        | insert ((i, n, t), (j, m, u) :: rest) =
            if String.compare (n, m) = LESS orelse String.compare (n, m) = EQUAL then
              (i, n, t) :: (j, m, u) :: rest
            else
              (j, m, u) :: insert ((i, n, t), rest)
      val sorted = List.foldl (fn (x, acc) => insert (x, acc)) [] indexed
    in
      sorted
    end

  (* Generate a wrapper function for an exported int->int function.
     The wrapper takes i32 args and returns i32, converting via ref.i31/i31.get_s.
     For call_ref, the stack order is: [param1, param2, ..., funcref] (funcref on top).
     closureInstrs: instructions to put the closure (ref eq) on the stack
     nParams: number of i32 params for the wrapper *)
  fun genExportWrapper (ctx: Context)
    { name: string
    , closureInstrs:
        W.instr list (* instructions to put the closure on the stack *)
    , nParams: int (* number of i32 params for the wrapper *)
    } =
    let
      val funcTypeIdx = getClosureFuncTypeIdx ctx 1
      (* The wrapper needs 2 locals: one for closure (eqref), one for funcref *)
      val closureLocalTy = eqref
      val funcrefTy =
        W.RefType {nullable = false, heaptype = W.AbsHeapType W.FUNC}
      val nWrapperParams = nParams (* i32 params from the wrapper signature *)
      val closureLocalIdx = nWrapperParams (* first local after params *)
      val funcrefLocalIdx = nWrapperParams + 1 (* second local after params *)

      (* Instructions to save closure to local *)
      val setupInstrs = closureInstrs @ [W.LOCAL_SET closureLocalIdx]

      (* Box i32 → (ref eq) using the internal convention (BoxedI32 struct) *)
      val boxI32Instrs = emitBox (F.UBTyInt32, ctx)
      (* Unbox (ref eq) → i32 using the internal convention *)
      val unboxI32Instrs = emitUnbox (F.UBTyInt32, ctx)

      val argInstrs =
        if nParams = 0 then
          [W.REF_NULL (W.AbsHeapType W.HEAP_NONE)]
        else if nParams = 1 then
          [W.LOCAL_GET 0] @ boxI32Instrs
        else
          let
            val tupleTypeIdx = getTupleTypeIdx ctx nParams
            val boxArgs = List.tabulate (nParams, fn i =>
              [W.LOCAL_GET i] @ boxI32Instrs)
          in
            List.concat boxArgs @ [W.STRUCT_NEW tupleTypeIdx]
          end

      (* Stack order for call_ref: (ref $ClosureBase), arg, funcref *)
      val body =
        setupInstrs
        (* First param: closure cast to (ref $ClosureBase) *)
        @
        [ W.LOCAL_GET closureLocalIdx
        , W.REF_CAST
            {nullable = false, heaptype = W.TypeIdx (#closureBaseTypeIdx ctx)}
        ] @ argInstrs (* second param: arg (boxed) *)
        (* Push funcref last: extract from closure, cast to specific func type *)
        @
        [ W.LOCAL_GET closureLocalIdx
        , W.REF_CAST
            {nullable = false, heaptype = W.TypeIdx (#closureBaseTypeIdx ctx)}
        , W.STRUCT_GET (#closureBaseTypeIdx ctx, 0)
        , W.REF_CAST {nullable = false, heaptype = W.TypeIdx funcTypeIdx}
        , W.CALL_REF funcTypeIdx
        ] @ unboxI32Instrs

      val wrapperFuncTypeIdx = getFuncTypeIdx ctx
        (List.tabulate (nParams, fn _ => W.NumType W.I32), [W.NumType W.I32])
      val wrapperFuncIdx = allocFuncIdx ctx
      val wrapperFunc: W.func =
        {typeidx = wrapperFuncTypeIdx, locals = [closureLocalTy], body = body}
      val () = #revFuncs ctx := wrapperFunc :: !(#revFuncs ctx)
      val () =
        #revExports ctx
        :=
        {name = name, desc = W.ExportFunc wrapperFuncIdx} :: !(#revExports ctx)
    in
      ()
    end

  fun doProgram (ctx: Context) (returnCont: C.CVar) (program: N.Stat)
    (export: ToFSyntax.export_entity) : W.module =
    let
      (* Create a fresh context for the _start function *)
      val startCtx = newFuncContext ctx

      (* Set up environment with return continuation *)
      val env = envWithCont (emptyEnv, returnCont, RETURN)

      (* Generate body instructions with return suffix *)
      val revBody = doStat startCtx env (program, [])
      val revBody = W.REF_NULL (W.AbsHeapType W.HEAP_NONE) :: revBody
      val revBody = W.RETURN :: revBody
      val bodyInstrs = List.rev revBody

      val localTypes = List.rev (!(#revLocalTypes startCtx))

      (* Create the _start function type: () -> (ref eq) *)
      val startFuncTypeIdx = getFuncTypeIdx ctx ([], [eqref])

      (* Register _start function *)
      val startFuncIdx = allocFuncIdx ctx
      val startFunc: W.func =
        {typeidx = startFuncTypeIdx, locals = localTypes, body = bodyInstrs}
      val () = #revFuncs ctx := startFunc :: !(#revFuncs ctx)

      (* Export _start *)
      val () =
        #revExports ctx
        :=
        {name = "_start", desc = W.ExportFunc startFuncIdx}
        :: !(#revExports ctx)

      (* Generate export wrappers for EXPORT_NAMED *)
      val () =
        case export of
          ToFSyntax.EXPORT_NAMED names =>
            let
              val nFields = Vector.length names
              val tupleTypeIdx = getTupleTypeIdx ctx nFields
              (* Add a mutable global to cache the export record *)
              val globalIdx = List.length (!(#revGlobals ctx))
              val () =
                #revGlobals ctx
                :=
                { globaltype = {mut = W.VAR, valtype = eqref}
                , init = [W.REF_NULL (W.AbsHeapType W.HEAP_NONE)]
                } :: !(#revGlobals ctx)

              (* Generate an init function that calls _start and stores result *)
              val initFuncTypeIdx = getFuncTypeIdx ctx ([], [])
              val initFuncIdx = allocFuncIdx ctx
              val initBody =
                [ W.GLOBAL_GET globalIdx
                , W.REF_IS_NULL
                , W.IF
                    ( W.BlockTypeNone
                    , [W.CALL startFuncIdx, W.GLOBAL_SET globalIdx]
                    , []
                    )
                ]
              val initFunc: W.func =
                {typeidx = initFuncTypeIdx, locals = [], body = initBody}
              val () = #revFuncs ctx := initFunc :: !(#revFuncs ctx)

              (* Sort names to match LabelMap field ordering *)
              val sorted = sortedExportNames names
              (* sorted: (originalIndex, name) list, sorted by name *)
              (* The field index in the struct corresponds to position in the sorted list *)
              val closureInstrs = [W.CALL initFuncIdx, W.GLOBAL_GET globalIdx]
              fun genWrappers _ [] = ()
                | genWrappers fieldIdx ((_, name, {nParams}) :: rest) =
                    ( genExportWrapper ctx
                        { name = name
                        , closureInstrs =
                            closureInstrs
                            @
                            [ W.REF_CAST
                                { nullable = false
                                , heaptype = W.TypeIdx tupleTypeIdx
                                }
                            , W.STRUCT_GET (tupleTypeIdx, fieldIdx)
                            ]
                        , nParams = nParams
                        }
                    ; genWrappers (fieldIdx + 1) rest
                    )
            in
              genWrappers 0 sorted
            end
        | _ => ()

      (* Collect all elem declarations needed for REF_FUNC *)
      val allFuncIdxs = List.tabulate (!(#nextFuncIdx ctx), fn i => i)
      val elems =
        if null allFuncIdxs then
          []
        else
          [{ elemtype = {nullable = false, heaptype = W.AbsHeapType W.FUNC}
           , init = List.map (fn i => [W.REF_FUNC i]) allFuncIdxs
           , mode = W.ElemDeclarative
           }]
    in
      { types = List.rev (!(#revTypes ctx))
      , funcs = List.rev (!(#revFuncs ctx))
      , tables = []
      , mems = []
      , tags = List.rev (!(#revTags ctx))
      , globals = List.rev (!(#revGlobals ctx))
      , elems = elems
      , datas = List.rev (!(#revDatas ctx))
      , start = NONE
      , imports = List.rev (!(#revImports ctx))
      , exports = List.rev (!(#revExports ctx))
      }
    end
end
