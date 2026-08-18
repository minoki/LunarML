(*
 * Copyright (c) 2026 ARATA Mizuki
 * This file is part of LunarML.
 *)
(* Machine-oriented representation types for the Wasm backend.
 *
 * This is the type intended to annotate NSyntax for the Wasm backend
 * ('ty NSyntax.stat with 'ty = WasmRepType.Ty): produced by
 * NSyntaxFromCpsWasm and consumed by CodeGenWasm in a later step.
 * Unlike FSyntax.Ty, it only expresses what matters for Wasm code generation:
 * unboxed scalars, opaque boxed values, record structure, and function types.
 * Unboxed scalars are kept at UnboxedTy granularity (not collapsed to
 * i32/i64/f64) because the boxed representation differs within one Wasm type,
 * e.g. bool/char use i31 while int32 uses a dedicated struct.
 *
 * The annotation describes the machine representation, not the SML type: a
 * WasmGC struct field and a closure parameter/result are always anyref, so
 * record fields and function parameters/results are Boxed regardless of the
 * FSyntax type they come from.  Whenever an unboxed value flows into or out of
 * such a slot, NSyntaxFromCpsWasm inserts an explicit BoxOp/UnboxOp, so the
 * code generator never has to infer boxing from the annotations.
 *)
structure WasmRepType :>
sig
  datatype Ty =
    Unboxed of FSyntax.UnboxedTy (* raw Wasm scalar: i32/i64/f64 *)
  | Boxed (* opaque boxed value: anyref / (ref eq) at the Wasm level *)
  | Record of
      Ty Syntax.LabelMap.map (* WasmGC struct; fields in LabelMap order *)
  | Function of {params: Ty list, result: Ty} (* closure *)
  val fromTy: FSyntax.Ty -> Ty
  val same: Ty * Ty -> bool
  val toString: Ty -> string
end =
struct
  local structure F = FSyntax
  in
    datatype Ty =
      Unboxed of F.UnboxedTy
    | Boxed
    | Record of Ty Syntax.LabelMap.map
    | Function of {params: Ty list, result: Ty}
    fun tyVarToUnboxedTy tv =
      if TypedSyntax.eqTyVar (tv, PrimTypes.Names.int32) then
        SOME F.UBTyInt32
      else if TypedSyntax.eqTyVar (tv, PrimTypes.Names.int64) then
        SOME F.UBTyInt64
      else if TypedSyntax.eqTyVar (tv, PrimTypes.Names.word32) then
        SOME F.UBTyWord32
      else if TypedSyntax.eqTyVar (tv, PrimTypes.Names.word64) then
        SOME F.UBTyWord64
      else if TypedSyntax.eqTyVar (tv, PrimTypes.Names.real) then
        SOME F.UBTyReal
      else if TypedSyntax.eqTyVar (tv, PrimTypes.Names.char) then
        SOME F.UBTyChar
      else if TypedSyntax.eqTyVar (tv, PrimTypes.Names.char16) then
        SOME F.UBTyChar16
      else if TypedSyntax.eqTyVar (tv, PrimTypes.Names.char32) then
        SOME F.UBTyChar32
      else if TypedSyntax.eqTyVar (tv, PrimTypes.Names.bool) then
        SOME F.UBTyBool
      else if TypedSyntax.eqTyVar (tv, PrimTypes.Names.wasm_ptr) then
        SOME F.UBTyWasmPtr
      else
        NONE
    fun fromTy (F.TyVar tv) =
          (case tyVarToUnboxedTy tv of
             SOME ubt => Unboxed ubt
           | NONE => Boxed)
      | fromTy (F.RecordType fields) =
          (* struct fields are anyref *)
          Record (Syntax.LabelMap.map (fn _ => Boxed) fields)
      | fromTy (F.MultiFnType (params, result)) =
          (* the closure calling convention is uniformly anyref *)
          Function {params = List.map (fn _ => Boxed) params, result = Boxed}
      | fromTy (ty as F.DelayedSubst _) =
          fromTy (F.forceTy ty)
      | fromTy (F.AppType _) = Boxed
      | fromTy (F.ForallType _) = Boxed
      | fromTy (F.ExistsType _) = Boxed
      | fromTy (F.TypeFn _) = Boxed
      | fromTy (F.AnyType _) = Boxed
      | fromTy F.BoxedType = Boxed
    fun same (Unboxed a, Unboxed b) = a = b
      | same (Boxed, Boxed) = true
      | same (Record a, Record b) =
          ListPair.allEq
            (fn ((l1, t1), (l2, t2)) => l1 = l2 andalso same (t1, t2))
            (Syntax.LabelMap.listItemsi a, Syntax.LabelMap.listItemsi b)
      | same
          ( Function {params = p1, result = r1}
          , Function {params = p2, result = r2}
          ) =
          ListPair.allEq same (p1, p2) andalso same (r1, r2)
      | same (_, _) = false
    fun unboxedTyToString F.UBTyInt32 = "int32"
      | unboxedTyToString F.UBTyInt64 = "int64"
      | unboxedTyToString F.UBTyWord32 = "word32"
      | unboxedTyToString F.UBTyWord64 = "word64"
      | unboxedTyToString F.UBTyReal = "real"
      | unboxedTyToString F.UBTyChar = "char"
      | unboxedTyToString F.UBTyChar16 = "char16"
      | unboxedTyToString F.UBTyChar32 = "char32"
      | unboxedTyToString F.UBTyBool = "bool"
      | unboxedTyToString F.UBTyWasmPtr = "wasm_ptr"
    fun labelToString (Syntax.NumericLabel n) = Int.toString n
      | labelToString (Syntax.IdentifierLabel name) = name
    fun toString (Unboxed ubt) = unboxedTyToString ubt
      | toString Boxed = "boxed"
      | toString (Record fields) =
          "{"
          ^
          String.concatWith ", "
            (List.map
               (fn (label, ty) => labelToString label ^ ": " ^ toString ty)
               (Syntax.LabelMap.listItemsi fields)) ^ "}"
      | toString (Function {params, result}) =
          "(" ^ String.concatWith ", " (List.map toString params) ^ ") -> "
          ^ toString result
  end
end
