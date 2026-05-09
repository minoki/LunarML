(*
 * Copyright (c) 2026 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure WasmWriter :>
sig
  val writeModule: BinIO.outstream * WasmSyntax.module -> unit
end =
struct
  open WasmSyntax

  (* --- Low-level binary output helpers --- *)

  fun outputByte (out, b: Word8.word) =
    BinIO.output1 (out, b)

  fun outputBytes (out, v: Word8Vector.vector) =
    BinIO.output (out, v)

  fun outputWord8 (out, n: int) =
    outputByte (out, Word8.fromInt n)

  (* Encode unsigned integer as LEB128 *)
  fun encodeULEB128 (n: int) : Word8Vector.vector =
    let
      fun loop (n, acc) =
        let
          val byte = Word8.fromInt (n mod 128)
          val n' = n div 128
        in
          if n' = 0 then
            Word8Vector.fromList (List.rev (byte :: acc))
          else
            loop (n', Word8.orb (byte, 0w128) :: acc)
        end
    in
      if n < 0 then raise Fail "encodeULEB128: negative"
      else loop (n, [])
    end

  (* Encode signed integer as LEB128 using Word32 for bit operations *)
  fun encodeSLEB128_32 (n: Int32.int) : Word8Vector.vector =
    let
      fun loop (w, acc) =
        let
          val byte = Word8.fromLargeWord (Word32.toLargeWord (Word32.andb (w, 0wx7F)))
          val w' = Word32.~>> (w, 0w7)
          val signBit = Word8.andb (byte, 0w64) <> 0w0
        in
          if (w' = 0w0 andalso not signBit) orelse (w' = 0wxFFFFFFFF andalso signBit) then
            Word8Vector.fromList (List.rev (byte :: acc))
          else
            loop (w', Word8.orb (byte, 0w128) :: acc)
        end
    in
      loop (Word32.fromLargeInt (Int32.toLarge n), [])
    end

  fun encodeSLEB128_64 (n: Int64.int) : Word8Vector.vector =
    let
      fun loop (w, acc) =
        let
          val byte = Word8.fromLargeWord (Word64.toLargeWord (Word64.andb (w, 0wx7F)))
          val w' = Word64.~>> (w, 0w7)
          val signBit = Word8.andb (byte, 0w64) <> 0w0
        in
          if (w' = 0w0 andalso not signBit) orelse (w' = 0wxFFFFFFFFFFFFFFFF andalso signBit) then
            Word8Vector.fromList (List.rev (byte :: acc))
          else
            loop (w', Word8.orb (byte, 0w128) :: acc)
        end
    in
      loop (Word64.fromLargeInt (Int64.toLarge n), [])
    end

  fun outputULEB128 (out, n) = outputBytes (out, encodeULEB128 n)
  fun outputSLEB128_32 (out, n) = outputBytes (out, encodeSLEB128_32 n)
  fun outputSLEB128_64 (out, n) = outputBytes (out, encodeSLEB128_64 n)

  (* Buffer: accumulate byte vectors in a list, concat at the end *)
  type buffer = Word8Vector.vector list ref

  fun newBuffer () : buffer = ref []

  fun bufferOutputByte (buf: buffer, b: Word8.word) =
    buf := Word8Vector.fromList [b] :: !buf

  fun bufferOutputBytes (buf: buffer, v: Word8Vector.vector) =
    if Word8Vector.length v > 0 then buf := v :: !buf else ()

  fun bufferOutputULEB128 (buf, n) = bufferOutputBytes (buf, encodeULEB128 n)
  fun bufferOutputSLEB128_32 (buf, n) = bufferOutputBytes (buf, encodeSLEB128_32 n)
  fun bufferOutputSLEB128_64 (buf, n) = bufferOutputBytes (buf, encodeSLEB128_64 n)

  fun bufferOutputU32LE (buf, w: Word32.word) =
    bufferOutputBytes (buf, Word8Vector.fromList
      [ Word8.fromLargeWord (Word32.toLargeWord w)
      , Word8.fromLargeWord (Word32.toLargeWord (Word32.>> (w, 0w8)))
      , Word8.fromLargeWord (Word32.toLargeWord (Word32.>> (w, 0w16)))
      , Word8.fromLargeWord (Word32.toLargeWord (Word32.>> (w, 0w24)))
      ])

  fun bufferOutputF64 (buf, r: real) =
    let
      val arr = Word8Array.array (8, 0w0)
      val () = PackRealLittle.update (arr, 0, r)
    in
      bufferOutputBytes (buf, Word8Array.vector arr)
    end

  fun bufferOutputName (buf, s: string) =
    let val bytes = Byte.stringToBytes s
    in bufferOutputULEB128 (buf, Word8Vector.length bytes)
     ; bufferOutputBytes (buf, bytes)
    end

  fun bufferToVector (buf: buffer) : Word8Vector.vector =
    Word8Vector.concat (List.rev (!buf))

  fun bufferSize (buf: buffer) : int =
    List.foldl (fn (v, acc) => acc + Word8Vector.length v) 0 (!buf)

  (* Build section content into a vector *)
  fun buildSection (f: buffer -> unit) : Word8Vector.vector =
    let val buf = newBuffer ()
    in f buf; bufferToVector buf
    end

  (* Write a section: section id, then length-prefixed content *)
  fun outputSection (out, sectionId: int, content: Word8Vector.vector) =
    if Word8Vector.length content = 0 then ()
    else
      ( outputWord8 (out, sectionId)
      ; outputULEB128 (out, Word8Vector.length content)
      ; outputBytes (out, content)
      )

  (* --- Wasm type encodings --- *)

  fun encodeNumtype I32 = 0x7F
    | encodeNumtype I64 = 0x7E
    | encodeNumtype F32 = 0x7D
    | encodeNumtype F64 = 0x7C

  (* Abstract heap types are encoded as negative SLEB128 values in Wasm binary format.
     The unsigned byte values (0x70, 0x6F, etc.) map to signed values via 7-bit sign extension. *)
  fun encodeAbsHeapType FUNC = ~16      (* 0x70 *)
    | encodeAbsHeapType EXTERN = ~17    (* 0x6F *)
    | encodeAbsHeapType ANY = ~18       (* 0x6E *)
    | encodeAbsHeapType EQ = ~19        (* 0x6D *)
    | encodeAbsHeapType I31 = ~20       (* 0x6C *)
    | encodeAbsHeapType STRUCT = ~21    (* 0x6B *)
    | encodeAbsHeapType ARRAY = ~22     (* 0x6A *)
    | encodeAbsHeapType HEAP_NONE = ~15 (* 0x71 *)
    | encodeAbsHeapType NOFUNC = ~13    (* 0x73 *)
    | encodeAbsHeapType NOEXTERN = ~14  (* 0x72 *)

  (* Encode heaptype *)
  fun bufferOutputHeapType (buf, AbsHeapType aht) =
        bufferOutputSLEB128_32 (buf, Int32.fromInt (encodeAbsHeapType aht))
    | bufferOutputHeapType (buf, TypeIdx idx) =
        bufferOutputSLEB128_32 (buf, Int32.fromInt idx)

  (* Encode reftype *)
  fun bufferOutputRefType (buf, {nullable, heaptype}: reftype) =
    case (nullable, heaptype) of
      (true, AbsHeapType aht) =>
        (* Shorthand encoding for nullable abstract heap types *)
        bufferOutputSLEB128_32 (buf, Int32.fromInt (encodeAbsHeapType aht))
    | (nullable, ht) =>
        ( bufferOutputByte (buf, if nullable then 0wx63 else 0wx64)
        ; bufferOutputHeapType (buf, ht)
        )

  (* Encode valtype *)
  fun bufferOutputValType (buf, NumType nt) =
        bufferOutputByte (buf, Word8.fromInt (encodeNumtype nt))
    | bufferOutputValType (buf, RefType rt) =
        bufferOutputRefType (buf, rt)

  (* Encode storagetype *)
  fun bufferOutputStorageType (buf, ValStorageType vt) =
        bufferOutputValType (buf, vt)
    | bufferOutputStorageType (buf, PackedStorageType I8) =
        bufferOutputByte (buf, 0wx78)
    | bufferOutputStorageType (buf, PackedStorageType I16) =
        bufferOutputByte (buf, 0wx77)

  (* Encode fieldtype *)
  fun bufferOutputFieldType (buf, {mut = m, storagetype = st}: fieldtype) =
    ( bufferOutputStorageType (buf, st)
    ; bufferOutputByte (buf, case m of CONST => 0w0 | VAR => 0w1)
    )

  (* Encode structtype *)
  fun bufferOutputStructType (buf, fields: structtype) =
    ( bufferOutputULEB128 (buf, List.length fields)
    ; List.app (fn ft => bufferOutputFieldType (buf, ft)) fields
    )

  (* Encode arraytype *)
  fun bufferOutputArrayType (buf, ft: arraytype) =
    bufferOutputFieldType (buf, ft)

  (* Encode functype *)
  fun bufferOutputFuncType (buf, {params, results}: functype) =
    ( bufferOutputULEB128 (buf, List.length params)
    ; List.app (fn vt => bufferOutputValType (buf, vt)) params
    ; bufferOutputULEB128 (buf, List.length results)
    ; List.app (fn vt => bufferOutputValType (buf, vt)) results
    )

  (* Encode comptype *)
  fun bufferOutputCompType (buf, StructType st) =
        (bufferOutputByte (buf, 0wx5F); bufferOutputStructType (buf, st))
    | bufferOutputCompType (buf, ArrayType at) =
        (bufferOutputByte (buf, 0wx5E); bufferOutputArrayType (buf, at))
    | bufferOutputCompType (buf, FuncType ft) =
        (bufferOutputByte (buf, 0wx60); bufferOutputFuncType (buf, ft))

  (* Encode subtype *)
  fun bufferOutputSubType (buf, SubType {final, supertypes, body}) =
    case (final, supertypes) of
      (true, []) =>
        (* No sub declaration needed, just the comptype *)
        bufferOutputCompType (buf, body)
    | (final, sups) =>
        ( bufferOutputByte (buf, if final then 0wx4F else 0wx50)
        ; bufferOutputULEB128 (buf, List.length sups)
        ; List.app (fn idx => bufferOutputULEB128 (buf, idx)) sups
        ; bufferOutputCompType (buf, body)
        )

  (* Encode rectype *)
  fun bufferOutputRecType (buf, [st]) =
        (* Single type: no rec group wrapper *)
        bufferOutputSubType (buf, st)
    | bufferOutputRecType (buf, sts) =
        ( bufferOutputByte (buf, 0wx4E)
        ; bufferOutputULEB128 (buf, List.length sts)
        ; List.app (fn st => bufferOutputSubType (buf, st)) sts
        )

  (* Encode limits *)
  fun bufferOutputLimits (buf, {min, max = NONE}: limits) =
        (bufferOutputByte (buf, 0w0); bufferOutputULEB128 (buf, min))
    | bufferOutputLimits (buf, {min, max = SOME max}) =
        (bufferOutputByte (buf, 0w1); bufferOutputULEB128 (buf, min); bufferOutputULEB128 (buf, max))

  (* Encode tabletype *)
  fun bufferOutputTableType (buf, {limits, elemtype}: tabletype) =
    (bufferOutputRefType (buf, elemtype); bufferOutputLimits (buf, limits))

  (* Encode memtype *)
  fun bufferOutputMemType (buf, {limits}: memtype) =
    bufferOutputLimits (buf, limits)

  (* Encode globaltype *)
  fun bufferOutputGlobalType (buf, {mut = m, valtype = vt}: globaltype) =
    ( bufferOutputValType (buf, vt)
    ; bufferOutputByte (buf, case m of CONST => 0w0 | VAR => 0w1)
    )

  (* Encode blocktype *)
  fun bufferOutputBlockType (buf, BlockTypeNone) =
        bufferOutputByte (buf, 0wx40)
    | bufferOutputBlockType (buf, BlockTypeVal vt) =
        bufferOutputValType (buf, vt)
    | bufferOutputBlockType (buf, BlockTypeIdx idx) =
        bufferOutputSLEB128_32 (buf, Int32.fromInt idx)

  (* --- Instruction encoding --- *)

  fun bufferOutputInstrs (buf, instrs) =
    List.app (fn i => bufferOutputInstr (buf, i)) instrs

  and bufferOutputInstr (buf, instr) =
    case instr of
    (* Control instructions *)
      UNREACHABLE => bufferOutputByte (buf, 0wx00)
    | NOP => bufferOutputByte (buf, 0wx01)
    | BLOCK (bt, body) =>
        ( bufferOutputByte (buf, 0wx02)
        ; bufferOutputBlockType (buf, bt)
        ; bufferOutputInstrs (buf, body)
        ; bufferOutputByte (buf, 0wx0B) (* end *)
        )
    | LOOP (bt, body) =>
        ( bufferOutputByte (buf, 0wx03)
        ; bufferOutputBlockType (buf, bt)
        ; bufferOutputInstrs (buf, body)
        ; bufferOutputByte (buf, 0wx0B) (* end *)
        )
    | IF (bt, thenBody, elseBody) =>
        ( bufferOutputByte (buf, 0wx04)
        ; bufferOutputBlockType (buf, bt)
        ; bufferOutputInstrs (buf, thenBody)
        ; (case elseBody of
             [] => ()
           | _ => (bufferOutputByte (buf, 0wx05); bufferOutputInstrs (buf, elseBody)))
        ; bufferOutputByte (buf, 0wx0B) (* end *)
        )
    | BR idx =>
        (bufferOutputByte (buf, 0wx0C); bufferOutputULEB128 (buf, idx))
    | BR_IF idx =>
        (bufferOutputByte (buf, 0wx0D); bufferOutputULEB128 (buf, idx))
    | BR_TABLE (labels, default) =>
        ( bufferOutputByte (buf, 0wx0E)
        ; bufferOutputULEB128 (buf, List.length labels)
        ; List.app (fn l => bufferOutputULEB128 (buf, l)) labels
        ; bufferOutputULEB128 (buf, default)
        )
    | RETURN => bufferOutputByte (buf, 0wx0F)
    | CALL idx =>
        (bufferOutputByte (buf, 0wx10); bufferOutputULEB128 (buf, idx))
    | CALL_INDIRECT (tableidx, typeidx) =>
        ( bufferOutputByte (buf, 0wx11)
        ; bufferOutputULEB128 (buf, typeidx)
        ; bufferOutputULEB128 (buf, tableidx)
        )
    | CALL_REF typeidx =>
        (bufferOutputByte (buf, 0wx14); bufferOutputULEB128 (buf, typeidx))
    | RETURN_CALL idx =>
        (bufferOutputByte (buf, 0wx12); bufferOutputULEB128 (buf, idx))
    | RETURN_CALL_REF typeidx =>
        (bufferOutputByte (buf, 0wx15); bufferOutputULEB128 (buf, typeidx))
    (* Exception handling: try_table *)
    | TRY_TABLE (bt, catches, body) =>
        ( bufferOutputByte (buf, 0wx1F)
        ; bufferOutputBlockType (buf, bt)
        ; bufferOutputULEB128 (buf, List.length catches)
        ; List.app (fn c => bufferOutputCatchClause (buf, c)) catches
        ; bufferOutputInstrs (buf, body)
        ; bufferOutputByte (buf, 0wx0B) (* end *)
        )
    | THROW tagidx =>
        (bufferOutputByte (buf, 0wx08); bufferOutputULEB128 (buf, tagidx))
    | THROW_REF =>
        bufferOutputByte (buf, 0wx0A)
    (* Variable instructions *)
    | LOCAL_GET idx =>
        (bufferOutputByte (buf, 0wx20); bufferOutputULEB128 (buf, idx))
    | LOCAL_SET idx =>
        (bufferOutputByte (buf, 0wx21); bufferOutputULEB128 (buf, idx))
    | LOCAL_TEE idx =>
        (bufferOutputByte (buf, 0wx22); bufferOutputULEB128 (buf, idx))
    | GLOBAL_GET idx =>
        (bufferOutputByte (buf, 0wx23); bufferOutputULEB128 (buf, idx))
    | GLOBAL_SET idx =>
        (bufferOutputByte (buf, 0wx24); bufferOutputULEB128 (buf, idx))
    (* Numeric instructions *)
    | I32_CONST n =>
        (bufferOutputByte (buf, 0wx41); bufferOutputSLEB128_32 (buf, n))
    | I64_CONST n =>
        (bufferOutputByte (buf, 0wx42); bufferOutputSLEB128_64 (buf, n))
    | F32_CONST w =>
        (bufferOutputByte (buf, 0wx43); bufferOutputU32LE (buf, w))
    | F64_CONST r =>
        (bufferOutputByte (buf, 0wx44); bufferOutputF64 (buf, r))
    (* i32 unary *)
    | I32_UNOP CLZ => bufferOutputByte (buf, 0wx67)
    | I32_UNOP CTZ => bufferOutputByte (buf, 0wx68)
    | I32_UNOP POPCNT => bufferOutputByte (buf, 0wx69)
    (* i32 binary *)
    | I32_BINOP ADD => bufferOutputByte (buf, 0wx6A)
    | I32_BINOP SUB => bufferOutputByte (buf, 0wx6B)
    | I32_BINOP MUL => bufferOutputByte (buf, 0wx6C)
    | I32_BINOP DIV_S => bufferOutputByte (buf, 0wx6D)
    | I32_BINOP DIV_U => bufferOutputByte (buf, 0wx6E)
    | I32_BINOP REM_S => bufferOutputByte (buf, 0wx6F)
    | I32_BINOP REM_U => bufferOutputByte (buf, 0wx70)
    | I32_BINOP AND => bufferOutputByte (buf, 0wx71)
    | I32_BINOP OR => bufferOutputByte (buf, 0wx72)
    | I32_BINOP XOR => bufferOutputByte (buf, 0wx73)
    | I32_BINOP SHL => bufferOutputByte (buf, 0wx74)
    | I32_BINOP SHR_S => bufferOutputByte (buf, 0wx75)
    | I32_BINOP SHR_U => bufferOutputByte (buf, 0wx76)
    | I32_BINOP ROTL => bufferOutputByte (buf, 0wx77)
    | I32_BINOP ROTR => bufferOutputByte (buf, 0wx78)
    (* i64 unary *)
    | I64_UNOP CLZ => bufferOutputByte (buf, 0wx79)
    | I64_UNOP CTZ => bufferOutputByte (buf, 0wx7A)
    | I64_UNOP POPCNT => bufferOutputByte (buf, 0wx7B)
    (* i64 binary *)
    | I64_BINOP ADD => bufferOutputByte (buf, 0wx7C)
    | I64_BINOP SUB => bufferOutputByte (buf, 0wx7D)
    | I64_BINOP MUL => bufferOutputByte (buf, 0wx7E)
    | I64_BINOP DIV_S => bufferOutputByte (buf, 0wx7F)
    | I64_BINOP DIV_U => bufferOutputByte (buf, 0wx80)
    | I64_BINOP REM_S => bufferOutputByte (buf, 0wx81)
    | I64_BINOP REM_U => bufferOutputByte (buf, 0wx82)
    | I64_BINOP AND => bufferOutputByte (buf, 0wx83)
    | I64_BINOP OR => bufferOutputByte (buf, 0wx84)
    | I64_BINOP XOR => bufferOutputByte (buf, 0wx85)
    | I64_BINOP SHL => bufferOutputByte (buf, 0wx86)
    | I64_BINOP SHR_S => bufferOutputByte (buf, 0wx87)
    | I64_BINOP SHR_U => bufferOutputByte (buf, 0wx88)
    | I64_BINOP ROTL => bufferOutputByte (buf, 0wx89)
    | I64_BINOP ROTR => bufferOutputByte (buf, 0wx8A)
    (* f32 unary *)
    | F32_UNOP ABS => bufferOutputByte (buf, 0wx8B)
    | F32_UNOP NEG => bufferOutputByte (buf, 0wx8C)
    | F32_UNOP CEIL => bufferOutputByte (buf, 0wx8D)
    | F32_UNOP FLOOR => bufferOutputByte (buf, 0wx8E)
    | F32_UNOP TRUNC => bufferOutputByte (buf, 0wx8F)
    | F32_UNOP NEAREST => bufferOutputByte (buf, 0wx90)
    | F32_UNOP SQRT => bufferOutputByte (buf, 0wx91)
    (* f32 binary *)
    | F32_BINOP FADD => bufferOutputByte (buf, 0wx92)
    | F32_BINOP FSUB => bufferOutputByte (buf, 0wx93)
    | F32_BINOP FMUL => bufferOutputByte (buf, 0wx94)
    | F32_BINOP FDIV => bufferOutputByte (buf, 0wx95)
    | F32_BINOP FMIN => bufferOutputByte (buf, 0wx96)
    | F32_BINOP FMAX => bufferOutputByte (buf, 0wx97)
    | F32_BINOP FCOPYSIGN => bufferOutputByte (buf, 0wx98)
    (* f64 unary *)
    | F64_UNOP ABS => bufferOutputByte (buf, 0wx99)
    | F64_UNOP NEG => bufferOutputByte (buf, 0wx9A)
    | F64_UNOP CEIL => bufferOutputByte (buf, 0wx9B)
    | F64_UNOP FLOOR => bufferOutputByte (buf, 0wx9C)
    | F64_UNOP TRUNC => bufferOutputByte (buf, 0wx9D)
    | F64_UNOP NEAREST => bufferOutputByte (buf, 0wx9E)
    | F64_UNOP SQRT => bufferOutputByte (buf, 0wx9F)
    (* f64 binary *)
    | F64_BINOP FADD => bufferOutputByte (buf, 0wxA0)
    | F64_BINOP FSUB => bufferOutputByte (buf, 0wxA1)
    | F64_BINOP FMUL => bufferOutputByte (buf, 0wxA2)
    | F64_BINOP FDIV => bufferOutputByte (buf, 0wxA3)
    | F64_BINOP FMIN => bufferOutputByte (buf, 0wxA4)
    | F64_BINOP FMAX => bufferOutputByte (buf, 0wxA5)
    | F64_BINOP FCOPYSIGN => bufferOutputByte (buf, 0wxA6)
    (* i32 test *)
    | I32_TESTOP EQZ => bufferOutputByte (buf, 0wx45)
    (* i32 relop *)
    | I32_RELOP IEQ => bufferOutputByte (buf, 0wx46)
    | I32_RELOP INE => bufferOutputByte (buf, 0wx47)
    | I32_RELOP LT_S => bufferOutputByte (buf, 0wx48)
    | I32_RELOP LT_U => bufferOutputByte (buf, 0wx49)
    | I32_RELOP GT_S => bufferOutputByte (buf, 0wx4A)
    | I32_RELOP GT_U => bufferOutputByte (buf, 0wx4B)
    | I32_RELOP LE_S => bufferOutputByte (buf, 0wx4C)
    | I32_RELOP LE_U => bufferOutputByte (buf, 0wx4D)
    | I32_RELOP GE_S => bufferOutputByte (buf, 0wx4E)
    | I32_RELOP GE_U => bufferOutputByte (buf, 0wx4F)
    (* i64 test *)
    | I64_TESTOP EQZ => bufferOutputByte (buf, 0wx50)
    (* i64 relop *)
    | I64_RELOP IEQ => bufferOutputByte (buf, 0wx51)
    | I64_RELOP INE => bufferOutputByte (buf, 0wx52)
    | I64_RELOP LT_S => bufferOutputByte (buf, 0wx53)
    | I64_RELOP LT_U => bufferOutputByte (buf, 0wx54)
    | I64_RELOP GT_S => bufferOutputByte (buf, 0wx55)
    | I64_RELOP GT_U => bufferOutputByte (buf, 0wx56)
    | I64_RELOP LE_S => bufferOutputByte (buf, 0wx57)
    | I64_RELOP LE_U => bufferOutputByte (buf, 0wx58)
    | I64_RELOP GE_S => bufferOutputByte (buf, 0wx59)
    | I64_RELOP GE_U => bufferOutputByte (buf, 0wx5A)
    (* f32 relop *)
    | F32_RELOP FEQ => bufferOutputByte (buf, 0wx5B)
    | F32_RELOP FNE => bufferOutputByte (buf, 0wx5C)
    | F32_RELOP FLT => bufferOutputByte (buf, 0wx5D)
    | F32_RELOP FGT => bufferOutputByte (buf, 0wx5E)
    | F32_RELOP FLE => bufferOutputByte (buf, 0wx5F)
    | F32_RELOP FGE => bufferOutputByte (buf, 0wx60)
    (* f64 relop *)
    | F64_RELOP FEQ => bufferOutputByte (buf, 0wx61)
    | F64_RELOP FNE => bufferOutputByte (buf, 0wx62)
    | F64_RELOP FLT => bufferOutputByte (buf, 0wx63)
    | F64_RELOP FGT => bufferOutputByte (buf, 0wx64)
    | F64_RELOP FLE => bufferOutputByte (buf, 0wx65)
    | F64_RELOP FGE => bufferOutputByte (buf, 0wx66)
    (* Conversion instructions *)
    | I32_WRAP_I64 => bufferOutputByte (buf, 0wxA7)
    | I64_EXTEND_I32 S => bufferOutputByte (buf, 0wxAC)
    | I64_EXTEND_I32 U => bufferOutputByte (buf, 0wxAD)
    | I32_TRUNC_F32 S => bufferOutputByte (buf, 0wxA8)
    | I32_TRUNC_F32 U => bufferOutputByte (buf, 0wxA9)
    | I32_TRUNC_F64 S => bufferOutputByte (buf, 0wxAA)
    | I32_TRUNC_F64 U => bufferOutputByte (buf, 0wxAB)
    | I64_TRUNC_F32 S => bufferOutputByte (buf, 0wxAE)
    | I64_TRUNC_F32 U => bufferOutputByte (buf, 0wxAF)
    | I64_TRUNC_F64 S => bufferOutputByte (buf, 0wxB0)
    | I64_TRUNC_F64 U => bufferOutputByte (buf, 0wxB1)
    | F32_CONVERT_I32 S => bufferOutputByte (buf, 0wxB2)
    | F32_CONVERT_I32 U => bufferOutputByte (buf, 0wxB3)
    | F32_CONVERT_I64 S => bufferOutputByte (buf, 0wxB4)
    | F32_CONVERT_I64 U => bufferOutputByte (buf, 0wxB5)
    | F64_CONVERT_I32 S => bufferOutputByte (buf, 0wxB6)
    | F64_CONVERT_I32 U => bufferOutputByte (buf, 0wxB7)
    | F64_CONVERT_I64 S => bufferOutputByte (buf, 0wxB8)
    | F64_CONVERT_I64 U => bufferOutputByte (buf, 0wxB9)
    | F32_DEMOTE_F64 => bufferOutputByte (buf, 0wxBA)
    | F64_PROMOTE_F32 => bufferOutputByte (buf, 0wxBB)
    | I32_REINTERPRET_F32 => bufferOutputByte (buf, 0wxBC)
    | I64_REINTERPRET_F64 => bufferOutputByte (buf, 0wxBD)
    | F32_REINTERPRET_I32 => bufferOutputByte (buf, 0wxBE)
    | F64_REINTERPRET_I64 => bufferOutputByte (buf, 0wxBF)
    (* Sign extension *)
    | I32_EXTEND8_S => bufferOutputByte (buf, 0wxC0)
    | I32_EXTEND16_S => bufferOutputByte (buf, 0wxC1)
    | I64_EXTEND8_S => bufferOutputByte (buf, 0wxC2)
    | I64_EXTEND16_S => bufferOutputByte (buf, 0wxC3)
    | I64_EXTEND32_S => bufferOutputByte (buf, 0wxC4)
    (* Memory instructions *)
    | MEMORY_SIZE =>
        (bufferOutputByte (buf, 0wx3F); bufferOutputByte (buf, 0wx00))
    | MEMORY_GROW =>
        (bufferOutputByte (buf, 0wx40); bufferOutputByte (buf, 0wx00))
    | I32_LOAD {align, offset} =>
        ( bufferOutputByte (buf, 0wx28)
        ; bufferOutputULEB128 (buf, align)
        ; bufferOutputULEB128 (buf, offset)
        )
    | I32_LOAD8_U {align, offset} =>
        ( bufferOutputByte (buf, 0wx2C)
        ; bufferOutputULEB128 (buf, align)
        ; bufferOutputULEB128 (buf, offset)
        )
    | I32_STORE {align, offset} =>
        ( bufferOutputByte (buf, 0wx36)
        ; bufferOutputULEB128 (buf, align)
        ; bufferOutputULEB128 (buf, offset)
        )
    | I32_STORE8 {align, offset} =>
        ( bufferOutputByte (buf, 0wx3A)
        ; bufferOutputULEB128 (buf, align)
        ; bufferOutputULEB128 (buf, offset)
        )
    (* Reference instructions *)
    | REF_NULL ht =>
        (bufferOutputByte (buf, 0wxD0); bufferOutputHeapType (buf, ht))
    | REF_IS_NULL => bufferOutputByte (buf, 0wxD1)
    | REF_FUNC idx =>
        (bufferOutputByte (buf, 0wxD2); bufferOutputULEB128 (buf, idx))
    | REF_EQ => bufferOutputByte (buf, 0wxD3)
    | REF_AS_NON_NULL => bufferOutputByte (buf, 0wxD4)
    | REF_CAST rt =>
        ( bufferOutputBytes (buf, Word8Vector.fromList [0wxFB])
        ; bufferOutputByte (buf, if #nullable rt then 0wx17 else 0wx16)
        ; bufferOutputHeapType (buf, #heaptype rt)
        )
    | REF_TEST rt =>
        ( bufferOutputBytes (buf, Word8Vector.fromList [0wxFB])
        ; bufferOutputByte (buf, if #nullable rt then 0wx15 else 0wx14)
        ; bufferOutputHeapType (buf, #heaptype rt)
        )
    (* i31 instructions *)
    | REF_I31 =>
        (bufferOutputByte (buf, 0wxFB); bufferOutputByte (buf, 0wx1C))
    | I31_GET S =>
        (bufferOutputByte (buf, 0wxFB); bufferOutputByte (buf, 0wx1D))
    | I31_GET U =>
        (bufferOutputByte (buf, 0wxFB); bufferOutputByte (buf, 0wx1E))
    (* Struct instructions *)
    | STRUCT_NEW idx =>
        (bufferOutputByte (buf, 0wxFB); bufferOutputULEB128 (buf, 0); bufferOutputULEB128 (buf, idx))
    | STRUCT_NEW_DEFAULT idx =>
        (bufferOutputByte (buf, 0wxFB); bufferOutputULEB128 (buf, 1); bufferOutputULEB128 (buf, idx))
    | STRUCT_GET (tidx, fidx) =>
        (bufferOutputByte (buf, 0wxFB); bufferOutputULEB128 (buf, 2); bufferOutputULEB128 (buf, tidx); bufferOutputULEB128 (buf, fidx))
    | STRUCT_GET_S (tidx, fidx) =>
        (bufferOutputByte (buf, 0wxFB); bufferOutputULEB128 (buf, 3); bufferOutputULEB128 (buf, tidx); bufferOutputULEB128 (buf, fidx))
    | STRUCT_GET_U (tidx, fidx) =>
        (bufferOutputByte (buf, 0wxFB); bufferOutputULEB128 (buf, 4); bufferOutputULEB128 (buf, tidx); bufferOutputULEB128 (buf, fidx))
    | STRUCT_SET (tidx, fidx) =>
        (bufferOutputByte (buf, 0wxFB); bufferOutputULEB128 (buf, 5); bufferOutputULEB128 (buf, tidx); bufferOutputULEB128 (buf, fidx))
    (* Array instructions *)
    | ARRAY_NEW idx =>
        (bufferOutputByte (buf, 0wxFB); bufferOutputULEB128 (buf, 6); bufferOutputULEB128 (buf, idx))
    | ARRAY_NEW_DEFAULT idx =>
        (bufferOutputByte (buf, 0wxFB); bufferOutputULEB128 (buf, 7); bufferOutputULEB128 (buf, idx))
    | ARRAY_NEW_FIXED (idx, n) =>
        (bufferOutputByte (buf, 0wxFB); bufferOutputULEB128 (buf, 8); bufferOutputULEB128 (buf, idx); bufferOutputULEB128 (buf, n))
    | ARRAY_NEW_DATA (tidx, didx) =>
        (bufferOutputByte (buf, 0wxFB); bufferOutputULEB128 (buf, 9); bufferOutputULEB128 (buf, tidx); bufferOutputULEB128 (buf, didx))
    | ARRAY_GET idx =>
        (bufferOutputByte (buf, 0wxFB); bufferOutputULEB128 (buf, 11); bufferOutputULEB128 (buf, idx))
    | ARRAY_GET_S idx =>
        (bufferOutputByte (buf, 0wxFB); bufferOutputULEB128 (buf, 12); bufferOutputULEB128 (buf, idx))
    | ARRAY_GET_U idx =>
        (bufferOutputByte (buf, 0wxFB); bufferOutputULEB128 (buf, 13); bufferOutputULEB128 (buf, idx))
    | ARRAY_SET idx =>
        (bufferOutputByte (buf, 0wxFB); bufferOutputULEB128 (buf, 14); bufferOutputULEB128 (buf, idx))
    | ARRAY_LEN =>
        (bufferOutputByte (buf, 0wxFB); bufferOutputULEB128 (buf, 15))
    | ARRAY_COPY (dst, src) =>
        (bufferOutputByte (buf, 0wxFB); bufferOutputULEB128 (buf, 17); bufferOutputULEB128 (buf, dst); bufferOutputULEB128 (buf, src))
    (* Extern conversions *)
    | EXTERN_INTERNALIZE =>
        (bufferOutputByte (buf, 0wxFB); bufferOutputULEB128 (buf, 26))
    | EXTERN_EXTERNALIZE =>
        (bufferOutputByte (buf, 0wxFB); bufferOutputULEB128 (buf, 27))
    (* Select *)
    | SELECT NONE => bufferOutputByte (buf, 0wx1B)
    | SELECT (SOME vts) =>
        ( bufferOutputByte (buf, 0wx1C)
        ; bufferOutputULEB128 (buf, List.length vts)
        ; List.app (fn vt => bufferOutputValType (buf, vt)) vts
        )
    (* Drop *)
    | DROP => bufferOutputByte (buf, 0wx1A)

  and bufferOutputCatchClause (buf, clause) =
    case clause of
      CATCH (tagidx, labelidx) =>
        (bufferOutputByte (buf, 0w0); bufferOutputULEB128 (buf, tagidx); bufferOutputULEB128 (buf, labelidx))
    | CATCH_REF (tagidx, labelidx) =>
        (bufferOutputByte (buf, 0w1); bufferOutputULEB128 (buf, tagidx); bufferOutputULEB128 (buf, labelidx))
    | CATCH_ALL labelidx =>
        (bufferOutputByte (buf, 0w2); bufferOutputULEB128 (buf, labelidx))
    | CATCH_ALL_REF labelidx =>
        (bufferOutputByte (buf, 0w3); bufferOutputULEB128 (buf, labelidx))

  (* Encode an expression (instruction sequence terminated by 0x0B) *)
  fun bufferOutputExpr (buf, instrs) =
    (bufferOutputInstrs (buf, instrs); bufferOutputByte (buf, 0wx0B))

  (* --- Section writers --- *)

  (* Section 1: Type section *)
  fun writeTypeSection (out, types: rectype list) =
    if List.null types then ()
    else
      let
        val content = buildSection (fn buf =>
          ( bufferOutputULEB128 (buf, List.length types)
          ; List.app (fn rt => bufferOutputRecType (buf, rt)) types
          ))
      in
        outputSection (out, 1, content)
      end

  (* Section 2: Import section *)
  fun writeImportSection (out, imports: import list) =
    if List.null imports then ()
    else
      let
        val content = buildSection (fn buf =>
          ( bufferOutputULEB128 (buf, List.length imports)
          ; List.app (fn {module_name, name, desc} =>
              ( bufferOutputName (buf, module_name)
              ; bufferOutputName (buf, name)
              ; (case desc of
                   ImportFunc typeidx =>
                     (bufferOutputByte (buf, 0wx00); bufferOutputULEB128 (buf, typeidx))
                 | ImportTable tt =>
                     (bufferOutputByte (buf, 0wx01); bufferOutputTableType (buf, tt))
                 | ImportMemory mt =>
                     (bufferOutputByte (buf, 0wx02); bufferOutputMemType (buf, mt))
                 | ImportGlobal gt =>
                     (bufferOutputByte (buf, 0wx03); bufferOutputGlobalType (buf, gt))
                 | ImportTag {functype} =>
                     (bufferOutputByte (buf, 0wx04); bufferOutputByte (buf, 0wx00); bufferOutputULEB128 (buf, functype)))
              )) imports
          ))
      in
        outputSection (out, 2, content)
      end

  (* Section 3: Function section (just type indices) *)
  fun writeFunctionSection (out, funcs: func list) =
    if List.null funcs then ()
    else
      let
        val content = buildSection (fn buf =>
          ( bufferOutputULEB128 (buf, List.length funcs)
          ; List.app (fn {typeidx, ...} => bufferOutputULEB128 (buf, typeidx)) funcs
          ))
      in
        outputSection (out, 3, content)
      end

  (* Section 4: Table section *)
  fun writeTableSection (out, tables: table list) =
    if List.null tables then ()
    else
      let
        val content = buildSection (fn buf =>
          ( bufferOutputULEB128 (buf, List.length tables)
          ; List.app (fn {tabletype, init} =>
              let
                val hasInit = not (List.null init)
              in
                if hasInit then
                  ( bufferOutputByte (buf, 0wx40)
                  ; bufferOutputByte (buf, 0wx00)
                  ; bufferOutputTableType (buf, tabletype)
                  ; bufferOutputExpr (buf, init)
                  )
                else
                  bufferOutputTableType (buf, tabletype)
              end) tables
          ))
      in
        outputSection (out, 4, content)
      end

  (* Section 5: Memory section *)
  fun writeMemorySection (out, mems: memtype list) =
    if List.null mems then ()
    else
      let
        val content = buildSection (fn buf =>
          ( bufferOutputULEB128 (buf, List.length mems)
          ; List.app (fn mt => bufferOutputMemType (buf, mt)) mems
          ))
      in
        outputSection (out, 5, content)
      end

  (* Section 13: Tag section *)
  fun writeTagSection (out, tags: tagtype list) =
    if List.null tags then ()
    else
      let
        val content = buildSection (fn buf =>
          ( bufferOutputULEB128 (buf, List.length tags)
          ; List.app (fn {functype} =>
              ( bufferOutputByte (buf, 0wx00) (* attribute = exception *)
              ; bufferOutputULEB128 (buf, functype)
              )) tags
          ))
      in
        outputSection (out, 13, content)
      end

  (* Section 6: Global section *)
  fun writeGlobalSection (out, globals: global list) =
    if List.null globals then ()
    else
      let
        val content = buildSection (fn buf =>
          ( bufferOutputULEB128 (buf, List.length globals)
          ; List.app (fn {globaltype, init} =>
              ( bufferOutputGlobalType (buf, globaltype)
              ; bufferOutputExpr (buf, init)
              )) globals
          ))
      in
        outputSection (out, 6, content)
      end

  (* Section 7: Export section *)
  fun writeExportSection (out, exports: export list) =
    if List.null exports then ()
    else
      let
        val content = buildSection (fn buf =>
          ( bufferOutputULEB128 (buf, List.length exports)
          ; List.app (fn {name, desc} =>
              ( bufferOutputName (buf, name)
              ; (case desc of
                   ExportFunc idx => (bufferOutputByte (buf, 0wx00); bufferOutputULEB128 (buf, idx))
                 | ExportTable idx => (bufferOutputByte (buf, 0wx01); bufferOutputULEB128 (buf, idx))
                 | ExportMemory idx => (bufferOutputByte (buf, 0wx02); bufferOutputULEB128 (buf, idx))
                 | ExportGlobal idx => (bufferOutputByte (buf, 0wx03); bufferOutputULEB128 (buf, idx))
                 | ExportTag idx => (bufferOutputByte (buf, 0wx04); bufferOutputULEB128 (buf, idx)))
              )) exports
          ))
      in
        outputSection (out, 7, content)
      end

  (* Section 8: Start section *)
  fun writeStartSection (out, start: funcidx option) =
    case start of
      NONE => ()
    | SOME idx =>
        let
          val content = buildSection (fn buf =>
            bufferOutputULEB128 (buf, idx))
        in
          outputSection (out, 8, content)
        end

  (* Section 9: Element section *)
  fun writeElementSection (out, elems: elem list) =
    if List.null elems then ()
    else
      let
        val content = buildSection (fn buf =>
          ( bufferOutputULEB128 (buf, List.length elems)
          ; List.app (fn {elemtype, init, mode} =>
              (* Use the general form (kind 5/6/7) for expressiveness *)
              case mode of
                ElemActive {tableidx = 0, offset} =>
                  (* kind 0 or 4 *)
                  if List.all (fn [REF_FUNC _] => true | _ => false) init then
                    (* kind 0: active, table 0, funcidx list *)
                    ( bufferOutputULEB128 (buf, 0)
                    ; bufferOutputExpr (buf, offset)
                    ; bufferOutputULEB128 (buf, List.length init)
                    ; List.app (fn [REF_FUNC idx] => bufferOutputULEB128 (buf, idx) | _ => ()) init
                    )
                  else
                    (* kind 4: active, table 0, expr list *)
                    ( bufferOutputULEB128 (buf, 4)
                    ; bufferOutputExpr (buf, offset)
                    ; bufferOutputULEB128 (buf, List.length init)
                    ; List.app (fn e => bufferOutputExpr (buf, e)) init
                    )
              | ElemActive {tableidx, offset} =>
                  (* kind 6: active, explicit table, expr list *)
                  ( bufferOutputULEB128 (buf, 6)
                  ; bufferOutputULEB128 (buf, tableidx)
                  ; bufferOutputExpr (buf, offset)
                  ; bufferOutputRefType (buf, elemtype)
                  ; bufferOutputULEB128 (buf, List.length init)
                  ; List.app (fn e => bufferOutputExpr (buf, e)) init
                  )
              | ElemPassive =>
                  (* kind 5: passive, expr list *)
                  ( bufferOutputULEB128 (buf, 5)
                  ; bufferOutputRefType (buf, elemtype)
                  ; bufferOutputULEB128 (buf, List.length init)
                  ; List.app (fn e => bufferOutputExpr (buf, e)) init
                  )
              | ElemDeclarative =>
                  (* kind 7: declarative, expr list *)
                  ( bufferOutputULEB128 (buf, 7)
                  ; bufferOutputRefType (buf, elemtype)
                  ; bufferOutputULEB128 (buf, List.length init)
                  ; List.app (fn e => bufferOutputExpr (buf, e)) init
                  )
            ) elems
          ))
      in
        outputSection (out, 9, content)
      end

  (* Section 10: Code section *)
  fun writeCodeSection (out, funcs: func list) =
    if List.null funcs then ()
    else
      let
        val content = buildSection (fn buf =>
          ( bufferOutputULEB128 (buf, List.length funcs)
          ; List.app (fn {typeidx = _, locals, body} =>
              let
                (* Compress locals: run-length encode *)
                val compressedLocals =
                  let
                    fun compress [] = []
                      | compress (vt :: rest) =
                          let
                            fun count (n, []) = (n, [])
                              | count (n, vt' :: rest') =
                                  if vt = vt' then count (n + 1, rest')
                                  else (n, vt' :: rest')
                            val (n, rest') = count (1, rest)
                          in
                            (n, vt) :: compress rest'
                          end
                  in
                    compress locals
                  end
                (* Build function body *)
                val funcBody = buildSection (fn fbuf =>
                  ( bufferOutputULEB128 (fbuf, List.length compressedLocals)
                  ; List.app (fn (n, vt) =>
                      ( bufferOutputULEB128 (fbuf, n)
                      ; bufferOutputValType (fbuf, vt)
                      )) compressedLocals
                  ; bufferOutputExpr (fbuf, body)
                  ))
              in
                (* Output function body size, then the body *)
                bufferOutputULEB128 (buf, Word8Vector.length funcBody);
                bufferOutputBytes (buf, funcBody)
              end) funcs
          ))
      in
        outputSection (out, 10, content)
      end

  (* Section 11: Data section *)
  fun writeDataSection (out, datas: data list) =
    if List.null datas then ()
    else
      let
        val content = buildSection (fn buf =>
          ( bufferOutputULEB128 (buf, List.length datas)
          ; List.app (fn {init, mode} =>
              case mode of
                DataPassive =>
                  ( bufferOutputULEB128 (buf, 1) (* kind 1: passive *)
                  ; bufferOutputULEB128 (buf, Word8Vector.length init)
                  ; bufferOutputBytes (buf, init)
                  )
              | DataActive {memidx = 0, offset} =>
                  ( bufferOutputULEB128 (buf, 0) (* kind 0: active, memory 0 *)
                  ; bufferOutputExpr (buf, offset)
                  ; bufferOutputULEB128 (buf, Word8Vector.length init)
                  ; bufferOutputBytes (buf, init)
                  )
              | DataActive {memidx, offset} =>
                  ( bufferOutputULEB128 (buf, 2) (* kind 2: active, explicit memory *)
                  ; bufferOutputULEB128 (buf, memidx)
                  ; bufferOutputExpr (buf, offset)
                  ; bufferOutputULEB128 (buf, Word8Vector.length init)
                  ; bufferOutputBytes (buf, init)
                  )
            ) datas
          ))
      in
        outputSection (out, 11, content)
      end

  (* Section 12: Data count section (required when data segments are referenced) *)
  fun writeDataCountSection (out, datas: data list) =
    if List.null datas then ()
    else
      let
        val content = buildSection (fn buf =>
          bufferOutputULEB128 (buf, List.length datas))
      in
        outputSection (out, 12, content)
      end

  (* --- Main module writer --- *)

  fun writeModule (out, m: module) =
    let
      val {types, funcs, tables, mems, tags, globals, elems, datas, start, imports, exports} = m
    in
      (* Magic number: \0asm *)
      outputBytes (out, Word8Vector.fromList [0wx00, 0wx61, 0wx73, 0wx6D]);
      (* Version: 1 *)
      outputBytes (out, Word8Vector.fromList [0wx01, 0wx00, 0wx00, 0wx00]);
      (* Sections must be in order *)
      writeTypeSection (out, types);
      writeImportSection (out, imports);
      writeFunctionSection (out, funcs);
      writeTableSection (out, tables);
      writeMemorySection (out, mems);
      writeTagSection (out, tags);
      writeGlobalSection (out, globals);
      writeExportSection (out, exports);
      writeStartSection (out, start);
      writeElementSection (out, elems);
      writeDataCountSection (out, datas);
      writeCodeSection (out, funcs);
      writeDataSection (out, datas)
    end
end;
