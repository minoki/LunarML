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
    , nextLocalIdx: int ref
    , types: WasmSyntax.rectype list ref
    , funcs: WasmSyntax.func list ref
    , globals: WasmSyntax.global list ref
    , imports: WasmSyntax.import list ref
    , exports: WasmSyntax.export list ref
    , datas: WasmSyntax.data list ref
    , tupleTyIdxMap: int IntRedBlackMap.map ref (* field count -> typeidx *)
    , boxedI32TypeIdx: WasmSyntax.typeidx
    , boxedI64TypeIdx: WasmSyntax.typeidx
    , boxedF64TypeIdx: WasmSyntax.typeidx
    }

  val labelToFieldIndex: Syntax.Label * FSyntax.Ty Syntax.LabelMap.map -> int

  val getTupleTypeIdx: Context -> int -> WasmSyntax.typeidx

  val emitBox: FSyntax.UnboxedTy * Context -> WasmSyntax.instr list
  val emitUnbox: FSyntax.UnboxedTy * Context -> WasmSyntax.instr list

  (* Entry point *)
  val doProgram: Context -> CSyntax.CVar -> NSyntax.Stat -> WasmSyntax.module
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
    , nextLocalIdx: int ref
    , types: W.rectype list ref
    , funcs: W.func list ref
    , globals: W.global list ref
    , imports: W.import list ref
    , exports: W.export list ref
    , datas: W.data list ref
    , tupleTyIdxMap: int IntRedBlackMap.map ref
    , boxedI32TypeIdx: W.typeidx
    , boxedI64TypeIdx: W.typeidx
    , boxedF64TypeIdx: W.typeidx
    }

  val eqRefType: W.reftype = {nullable = true, heaptype = W.AbsHeapType W.EQ}
  val eqref: W.valtype = W.RefType eqRefType
  val eqref_nn: W.valtype =
    W.RefType {nullable = false, heaptype = W.AbsHeapType W.EQ}

  (* ==================== Type Mapping ==================== *)

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
          val () = #types ctx := !(#types ctx) @ [[subtype]]
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

  (* ==================== Context initialization ==================== *)

  (* Create runtime boxing types ($BoxedI32, $BoxedI64, $BoxedF64) at the start of the type section *)
  fun initContext () =
    let
      val boxedI32TypeIdx = 0
      val boxedI64TypeIdx = 1
      val boxedF64TypeIdx = 2
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
    in
      { nextTypeIdx = ref 3
      , nextFuncIdx = ref 0
      , nextLocalIdx = ref 0
      , types = ref [[boxedI32], [boxedI64], [boxedF64]]
      , funcs = ref []
      , globals = ref []
      , imports = ref []
      , exports = ref []
      , datas = ref []
      , tupleTyIdxMap = ref IntRedBlackMap.empty
      , boxedI32TypeIdx = boxedI32TypeIdx
      , boxedI64TypeIdx = boxedI64TypeIdx
      , boxedF64TypeIdx = boxedF64TypeIdx
      }
    end

  (* ==================== Code generation (stub) ==================== *)

  fun doProgram _ _ _ : W.module =
    raise CodeGenError "CodeGenWasm.doProgram: not yet implemented"
end
