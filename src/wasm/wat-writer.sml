(*
 * Copyright (c) 2026 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure WatWriter :>
sig
  val writeModule: TextIO.outstream * WasmSyntax.module -> unit
end =
struct
  open WasmSyntax

  fun output (out, s) = TextIO.output (out, s)
  fun outputIndent (out, level) =
    let
      fun loop 0 = ()
        | loop n =
            (output (out, "  "); loop (n - 1))
    in
      loop level
    end
  fun outputLine (out, level, s) =
    (outputIndent (out, level); output (out, s); output (out, "\n"))

  (* WAT uses '-' for negative, but SML's Int32.toString uses '~' *)
  fun int32ToString (n: Int32.int) =
    if n < 0 then "-" ^ String.extract (Int32.toString n, 1, NONE)
    else Int32.toString n
  fun int64ToString (n: Int64.int) =
    if n < 0 then "-" ^ String.extract (Int64.toString n, 1, NONE)
    else Int64.toString n

  fun sxToString S = "_s"
    | sxToString U = "_u"

  fun numtypeToString I32 = "i32"
    | numtypeToString I64 = "i64"
    | numtypeToString F32 = "f32"
    | numtypeToString F64 = "f64"

  fun packtypeToString I8 = "i8"
    | packtypeToString I16 = "i16"

  fun absheaptypeToString FUNC = "func"
    | absheaptypeToString EXTERN = "extern"
    | absheaptypeToString ANY = "any"
    | absheaptypeToString EQ = "eq"
    | absheaptypeToString I31 = "i31"
    | absheaptypeToString STRUCT = "struct"
    | absheaptypeToString ARRAY = "array"
    | absheaptypeToString HEAP_NONE = "none"
    | absheaptypeToString NOFUNC = "nofunc"
    | absheaptypeToString NOEXTERN = "noextern"

  fun heaptypeToString (AbsHeapType aht) = absheaptypeToString aht
    | heaptypeToString (TypeIdx idx) = Int.toString idx

  fun reftypeToString ({nullable = true, heaptype = AbsHeapType FUNC}) =
        "funcref"
    | reftypeToString ({nullable = true, heaptype = AbsHeapType EXTERN}) =
        "externref"
    | reftypeToString ({nullable = true, heaptype = AbsHeapType ANY}) = "anyref"
    | reftypeToString ({nullable = true, heaptype = AbsHeapType EQ}) = "eqref"
    | reftypeToString ({nullable = true, heaptype = AbsHeapType I31}) = "i31ref"
    | reftypeToString ({nullable = true, heaptype = AbsHeapType STRUCT}) =
        "structref"
    | reftypeToString ({nullable = true, heaptype = AbsHeapType ARRAY}) =
        "arrayref"
    | reftypeToString ({nullable = true, heaptype = AbsHeapType HEAP_NONE}) =
        "nullref"
    | reftypeToString ({nullable = true, heaptype = AbsHeapType NOFUNC}) =
        "nullfuncref"
    | reftypeToString ({nullable = true, heaptype = AbsHeapType NOEXTERN}) =
        "nullexternref"
    | reftypeToString ({nullable, heaptype}) =
        let val null_str = if nullable then "null " else ""
        in "(ref " ^ null_str ^ heaptypeToString heaptype ^ ")"
        end

  fun valtypeToString (NumType nt) = numtypeToString nt
    | valtypeToString (RefType rt) = reftypeToString rt

  fun storagetypeToString (ValStorageType vt) = valtypeToString vt
    | storagetypeToString (PackedStorageType pt) = packtypeToString pt

  fun fieldtypeToString ({mut = CONST, storagetype}) =
        storagetypeToString storagetype
    | fieldtypeToString ({mut = VAR, storagetype}) =
        "(mut " ^ storagetypeToString storagetype ^ ")"

  fun blocktypeToString BlockTypeNone = ""
    | blocktypeToString (BlockTypeVal vt) =
        " (result " ^ valtypeToString vt ^ ")"
    | blocktypeToString (BlockTypeIdx idx) =
        " (type " ^ Int.toString idx ^ ")"

  fun iunopToString CLZ = "clz"
    | iunopToString CTZ = "ctz"
    | iunopToString POPCNT = "popcnt"

  fun ibinopToString ADD = "add"
    | ibinopToString SUB = "sub"
    | ibinopToString MUL = "mul"
    | ibinopToString DIV_S = "div_s"
    | ibinopToString DIV_U = "div_u"
    | ibinopToString REM_S = "rem_s"
    | ibinopToString REM_U = "rem_u"
    | ibinopToString AND = "and"
    | ibinopToString OR = "or"
    | ibinopToString XOR = "xor"
    | ibinopToString SHL = "shl"
    | ibinopToString SHR_S = "shr_s"
    | ibinopToString SHR_U = "shr_u"
    | ibinopToString ROTL = "rotl"
    | ibinopToString ROTR = "rotr"

  fun funopToString ABS = "abs"
    | funopToString NEG = "neg"
    | funopToString CEIL = "ceil"
    | funopToString FLOOR = "floor"
    | funopToString TRUNC = "trunc"
    | funopToString NEAREST = "nearest"
    | funopToString SQRT = "sqrt"

  fun fbinopToString FADD = "add"
    | fbinopToString FSUB = "sub"
    | fbinopToString FMUL = "mul"
    | fbinopToString FDIV = "div"
    | fbinopToString FMIN = "min"
    | fbinopToString FMAX = "max"
    | fbinopToString FCOPYSIGN = "copysign"

  fun itestopToString EQZ = "eqz"

  fun irelopToString IEQ = "eq"
    | irelopToString INE = "ne"
    | irelopToString LT_S = "lt_s"
    | irelopToString LT_U = "lt_u"
    | irelopToString GT_S = "gt_s"
    | irelopToString GT_U = "gt_u"
    | irelopToString LE_S = "le_s"
    | irelopToString LE_U = "le_u"
    | irelopToString GE_S = "ge_s"
    | irelopToString GE_U = "ge_u"

  fun frelopToString FEQ = "eq"
    | frelopToString FNE = "ne"
    | frelopToString FLT = "lt"
    | frelopToString FGT = "gt"
    | frelopToString FLE = "le"
    | frelopToString FGE = "ge"

  (* Encode a byte as two hex digits *)
  fun byteToHex b =
    let
      val hi = Word8.>> (b, 0w4)
      val lo = Word8.andb (b, 0w15)
      fun hexDigit w =
        if w < 0w10 then chr (ord #"0" + Word8.toInt w)
        else chr (ord #"a" + Word8.toInt w - 10)
    in
      String.implode [hexDigit hi, hexDigit lo]
    end

  (* Escape a string for WAT string literal *)
  fun escapeString s =
    let
      fun escapeChar c =
        if c = #"\\" then "\\\\"
        else if c = #"\"" then "\\\""
        else if #" " <= c andalso c <= #"~" then String.str c
        else "\\" ^ byteToHex (Word8.fromInt (ord c))
    in
      String.translate escapeChar s
    end

  (* Format f32 as hex float *)
  fun f32ToString (w: Word32.word) : string =
    (* Output as raw hex encoding for exact representation *)
    "0x" ^ Word32.fmt StringCvt.HEX w

  (* Format f64 for WAT *)
  fun f64ToString (r: real) : string =
    if Real.isNan r then if Real.signBit r then "-nan" else "nan"
    else if Real.== (r, Real.posInf) then "inf"
    else if Real.== (r, Real.negInf) then "-inf"
    else let val s = Real.fmt (StringCvt.EXACT) r in s end

  fun writeInstrs (out, level, instrs) =
    List.app (fn i => writeInstr (out, level, i)) instrs
  and writeInstr (out, level, instr) =
    case instr of
    (* Control instructions *)
      UNREACHABLE => outputLine (out, level, "unreachable")
    | NOP => outputLine (out, level, "nop")
    | BLOCK (bt, body) =>
        ( outputLine (out, level, "block" ^ blocktypeToString bt)
        ; writeInstrs (out, level + 1, body)
        ; outputLine (out, level, "end")
        )
    | LOOP (bt, body) =>
        ( outputLine (out, level, "loop" ^ blocktypeToString bt)
        ; writeInstrs (out, level + 1, body)
        ; outputLine (out, level, "end")
        )
    | IF (bt, thenBody, []) =>
        ( outputLine (out, level, "if" ^ blocktypeToString bt)
        ; writeInstrs (out, level + 1, thenBody)
        ; outputLine (out, level, "end")
        )
    | IF (bt, thenBody, elseBody) =>
        ( outputLine (out, level, "if" ^ blocktypeToString bt)
        ; writeInstrs (out, level + 1, thenBody)
        ; outputLine (out, level, "else")
        ; writeInstrs (out, level + 1, elseBody)
        ; outputLine (out, level, "end")
        )
    | BR idx => outputLine (out, level, "br " ^ Int.toString idx)
    | BR_IF idx => outputLine (out, level, "br_if " ^ Int.toString idx)
    | BR_TABLE (labels, default) =>
        outputLine
          ( out
          , level
          , "br_table " ^ String.concatWith " " (List.map Int.toString labels)
            ^ " " ^ Int.toString default
          )
    | RETURN => outputLine (out, level, "return")
    | CALL idx => outputLine (out, level, "call " ^ Int.toString idx)
    | CALL_INDIRECT (tableidx, typeidx) =>
        outputLine
          ( out
          , level
          , "call_indirect " ^ Int.toString tableidx ^ " (type "
            ^ Int.toString typeidx ^ ")"
          )
    | CALL_REF typeidx =>
        outputLine (out, level, "call_ref " ^ Int.toString typeidx)
    | RETURN_CALL idx =>
        outputLine (out, level, "return_call " ^ Int.toString idx)
    | RETURN_CALL_REF typeidx =>
        outputLine (out, level, "return_call_ref " ^ Int.toString typeidx)
    (* Exception handling *)
    | TRY_TABLE (bt, catches, body) =>
        ( outputIndent (out, level)
        ; output (out, "try_table" ^ blocktypeToString bt)
        ; List.app (fn c => output (out, " " ^ catchClauseToString c)) catches
        ; output (out, "\n")
        ; writeInstrs (out, level + 1, body)
        ; outputLine (out, level, "end")
        )
    | THROW idx => outputLine (out, level, "throw " ^ Int.toString idx)
    | THROW_REF => outputLine (out, level, "throw_ref")
    (* Variable instructions *)
    | LOCAL_GET idx => outputLine (out, level, "local.get " ^ Int.toString idx)
    | LOCAL_SET idx => outputLine (out, level, "local.set " ^ Int.toString idx)
    | LOCAL_TEE idx => outputLine (out, level, "local.tee " ^ Int.toString idx)
    | GLOBAL_GET idx =>
        outputLine (out, level, "global.get " ^ Int.toString idx)
    | GLOBAL_SET idx =>
        outputLine (out, level, "global.set " ^ Int.toString idx)
    (* Numeric instructions *)
    | I32_CONST n => outputLine (out, level, "i32.const " ^ int32ToString n)
    | I64_CONST n => outputLine (out, level, "i64.const " ^ int64ToString n)
    | F32_CONST w => outputLine (out, level, "f32.const " ^ f32ToString w)
    | F64_CONST r => outputLine (out, level, "f64.const " ^ f64ToString r)
    | I32_UNOP opr => outputLine (out, level, "i32." ^ iunopToString opr)
    | I64_UNOP opr => outputLine (out, level, "i64." ^ iunopToString opr)
    | F32_UNOP opr => outputLine (out, level, "f32." ^ funopToString opr)
    | F64_UNOP opr => outputLine (out, level, "f64." ^ funopToString opr)
    | I32_BINOP opr => outputLine (out, level, "i32." ^ ibinopToString opr)
    | I64_BINOP opr => outputLine (out, level, "i64." ^ ibinopToString opr)
    | F32_BINOP opr => outputLine (out, level, "f32." ^ fbinopToString opr)
    | F64_BINOP opr => outputLine (out, level, "f64." ^ fbinopToString opr)
    | I32_TESTOP opr => outputLine (out, level, "i32." ^ itestopToString opr)
    | I64_TESTOP opr => outputLine (out, level, "i64." ^ itestopToString opr)
    | I32_RELOP opr => outputLine (out, level, "i32." ^ irelopToString opr)
    | I64_RELOP opr => outputLine (out, level, "i64." ^ irelopToString opr)
    | F32_RELOP opr => outputLine (out, level, "f32." ^ frelopToString opr)
    | F64_RELOP opr => outputLine (out, level, "f64." ^ frelopToString opr)
    (* Conversion instructions *)
    | I32_WRAP_I64 => outputLine (out, level, "i32.wrap_i64")
    | I64_EXTEND_I32 sx =>
        outputLine (out, level, "i64.extend_i32" ^ sxToString sx)
    | I32_TRUNC_F32 sx =>
        outputLine (out, level, "i32.trunc_f32" ^ sxToString sx)
    | I32_TRUNC_F64 sx =>
        outputLine (out, level, "i32.trunc_f64" ^ sxToString sx)
    | I64_TRUNC_F32 sx =>
        outputLine (out, level, "i64.trunc_f32" ^ sxToString sx)
    | I64_TRUNC_F64 sx =>
        outputLine (out, level, "i64.trunc_f64" ^ sxToString sx)
    | F32_CONVERT_I32 sx =>
        outputLine (out, level, "f32.convert_i32" ^ sxToString sx)
    | F32_CONVERT_I64 sx =>
        outputLine (out, level, "f32.convert_i64" ^ sxToString sx)
    | F64_CONVERT_I32 sx =>
        outputLine (out, level, "f64.convert_i32" ^ sxToString sx)
    | F64_CONVERT_I64 sx =>
        outputLine (out, level, "f64.convert_i64" ^ sxToString sx)
    | F32_DEMOTE_F64 => outputLine (out, level, "f32.demote_f64")
    | F64_PROMOTE_F32 => outputLine (out, level, "f64.promote_f32")
    | I32_REINTERPRET_F32 => outputLine (out, level, "i32.reinterpret_f32")
    | I64_REINTERPRET_F64 => outputLine (out, level, "i64.reinterpret_f64")
    | F32_REINTERPRET_I32 => outputLine (out, level, "f32.reinterpret_i32")
    | F64_REINTERPRET_I64 => outputLine (out, level, "f64.reinterpret_i64")
    | I32_EXTEND8_S => outputLine (out, level, "i32.extend8_s")
    | I32_EXTEND16_S => outputLine (out, level, "i32.extend16_s")
    | I64_EXTEND8_S => outputLine (out, level, "i64.extend8_s")
    | I64_EXTEND16_S => outputLine (out, level, "i64.extend16_s")
    | I64_EXTEND32_S => outputLine (out, level, "i64.extend32_s")
    (* Memory instructions *)
    | MEMORY_SIZE => outputLine (out, level, "memory.size")
    | MEMORY_GROW => outputLine (out, level, "memory.grow")
    | I32_LOAD {align, offset} =>
        let
          val s =
            (if offset = 0 then "" else " offset=" ^ Int.toString offset)
            ^
            (if align = 0 then
               ""
             else
               " align="
               ^ Int.toString (Word.toInt (Word.<< (0w1, Word.fromInt align))))
        in
          outputLine (out, level, "i32.load" ^ s)
        end
    | I32_LOAD8_U {align, offset} =>
        let
          val s =
            (if offset = 0 then "" else " offset=" ^ Int.toString offset)
            ^
            (if align = 0 then
               ""
             else
               " align="
               ^ Int.toString (Word.toInt (Word.<< (0w1, Word.fromInt align))))
        in
          outputLine (out, level, "i32.load8_u" ^ s)
        end
    | I32_STORE {align, offset} =>
        let
          val s =
            (if offset = 0 then "" else " offset=" ^ Int.toString offset)
            ^
            (if align = 0 then
               ""
             else
               " align="
               ^ Int.toString (Word.toInt (Word.<< (0w1, Word.fromInt align))))
        in
          outputLine (out, level, "i32.store" ^ s)
        end
    | I32_STORE8 {align, offset} =>
        let
          val s =
            (if offset = 0 then "" else " offset=" ^ Int.toString offset)
            ^
            (if align = 0 then
               ""
             else
               " align="
               ^ Int.toString (Word.toInt (Word.<< (0w1, Word.fromInt align))))
        in
          outputLine (out, level, "i32.store8" ^ s)
        end
    (* Reference instructions *)
    | REF_NULL ht => outputLine (out, level, "ref.null " ^ heaptypeToString ht)
    | REF_IS_NULL => outputLine (out, level, "ref.is_null")
    | REF_FUNC idx => outputLine (out, level, "ref.func " ^ Int.toString idx)
    | REF_EQ => outputLine (out, level, "ref.eq")
    | REF_AS_NON_NULL => outputLine (out, level, "ref.as_non_null")
    | REF_CAST rt => outputLine (out, level, "ref.cast " ^ reftypeToString rt)
    | REF_TEST rt => outputLine (out, level, "ref.test " ^ reftypeToString rt)
    (* i31 instructions *)
    | REF_I31 => outputLine (out, level, "ref.i31")
    | I31_GET sx => outputLine (out, level, "i31.get" ^ sxToString sx)
    (* Struct instructions *)
    | STRUCT_NEW idx =>
        outputLine (out, level, "struct.new " ^ Int.toString idx)
    | STRUCT_NEW_DEFAULT idx =>
        outputLine (out, level, "struct.new_default " ^ Int.toString idx)
    | STRUCT_GET (tidx, fidx) =>
        outputLine
          ( out
          , level
          , "struct.get " ^ Int.toString tidx ^ " " ^ Int.toString fidx
          )
    | STRUCT_GET_S (tidx, fidx) =>
        outputLine
          ( out
          , level
          , "struct.get_s " ^ Int.toString tidx ^ " " ^ Int.toString fidx
          )
    | STRUCT_GET_U (tidx, fidx) =>
        outputLine
          ( out
          , level
          , "struct.get_u " ^ Int.toString tidx ^ " " ^ Int.toString fidx
          )
    | STRUCT_SET (tidx, fidx) =>
        outputLine
          ( out
          , level
          , "struct.set " ^ Int.toString tidx ^ " " ^ Int.toString fidx
          )
    (* Array instructions *)
    | ARRAY_NEW idx => outputLine (out, level, "array.new " ^ Int.toString idx)
    | ARRAY_NEW_DEFAULT idx =>
        outputLine (out, level, "array.new_default " ^ Int.toString idx)
    | ARRAY_NEW_FIXED (idx, n) =>
        outputLine
          ( out
          , level
          , "array.new_fixed " ^ Int.toString idx ^ " " ^ Int.toString n
          )
    | ARRAY_NEW_DATA (tidx, didx) =>
        outputLine
          ( out
          , level
          , "array.new_data " ^ Int.toString tidx ^ " " ^ Int.toString didx
          )
    | ARRAY_GET idx => outputLine (out, level, "array.get " ^ Int.toString idx)
    | ARRAY_GET_S idx =>
        outputLine (out, level, "array.get_s " ^ Int.toString idx)
    | ARRAY_GET_U idx =>
        outputLine (out, level, "array.get_u " ^ Int.toString idx)
    | ARRAY_SET idx => outputLine (out, level, "array.set " ^ Int.toString idx)
    | ARRAY_LEN => outputLine (out, level, "array.len")
    | ARRAY_COPY (dst, src) =>
        outputLine
          ( out
          , level
          , "array.copy " ^ Int.toString dst ^ " " ^ Int.toString src
          )
    (* Extern conversions *)
    | EXTERN_INTERNALIZE => outputLine (out, level, "any.convert_extern")
    | EXTERN_EXTERNALIZE => outputLine (out, level, "extern.convert_any")
    (* Misc *)
    | SELECT NONE => outputLine (out, level, "select")
    | SELECT (SOME vts) =>
        outputLine
          ( out
          , level
          , "select (result "
            ^ String.concatWith " " (List.map valtypeToString vts) ^ ")"
          )
    | DROP => outputLine (out, level, "drop")
  and catchClauseToString (CATCH (tagidx, labelidx)) =
        "(catch " ^ Int.toString tagidx ^ " " ^ Int.toString labelidx ^ ")"
    | catchClauseToString (CATCH_REF (tagidx, labelidx)) =
        "(catch_ref " ^ Int.toString tagidx ^ " " ^ Int.toString labelidx ^ ")"
    | catchClauseToString (CATCH_ALL labelidx) =
        "(catch_all " ^ Int.toString labelidx ^ ")"
    | catchClauseToString (CATCH_ALL_REF labelidx) =
        "(catch_all_ref " ^ Int.toString labelidx ^ ")"

  fun writeFieldtype (out, ft) =
    output (out, "(field " ^ fieldtypeToString ft ^ ")")

  fun writeComptype (out, level, StructType fields) =
        ( outputIndent (out, level)
        ; output (out, "(struct")
        ; List.app (fn ft => (output (out, " "); writeFieldtype (out, ft)))
            fields
        ; output (out, ")")
        )
    | writeComptype (out, level, ArrayType ft) =
        ( outputIndent (out, level)
        ; output (out, "(array " ^ fieldtypeToString ft ^ ")")
        )
    | writeComptype (out, level, FuncType {params, results}) =
        ( outputIndent (out, level)
        ; output (out, "(func")
        ; (case params of
             [] => ()
           | _ =>
               output
                 ( out
                 , " (param "
                   ^ String.concatWith " " (List.map valtypeToString params)
                   ^ ")"
                 ))
        ; (case results of
             [] => ()
           | _ =>
               output
                 ( out
                 , " (result "
                   ^ String.concatWith " " (List.map valtypeToString results)
                   ^ ")"
                 ))
        ; output (out, ")")
        )

  fun writeSubtype (out, level, SubType {final, supertypes, body}) =
    case (final, supertypes) of
      (true, []) =>
        (* No sub declaration needed *)
        writeComptype (out, level, body)
    | _ =>
        ( outputIndent (out, level)
        ; output (out, if final then "(sub final" else "(sub")
        ; List.app (fn idx => output (out, " " ^ Int.toString idx)) supertypes
        ; output (out, " ")
        ; writeComptype (out, 0, body)
        ; output (out, ")")
        )

  fun writeRectype (out, level, typeCounter, [st]) =
        (* Single type: no rec group needed *)
        ( outputIndent (out, level)
        ; output (out, "(type ")
        ; writeSubtype (out, 0, st)
        ; output (out, ")\n")
        ; typeCounter := !typeCounter + 1
        )
    | writeRectype (out, level, typeCounter, sts) =
        ( outputLine (out, level, "(rec")
        ; List.app
            (fn st =>
               ( outputIndent (out, level + 1)
               ; output (out, "(type ")
               ; writeSubtype (out, 0, st)
               ; output (out, ")\n")
               ; typeCounter := !typeCounter + 1
               )) sts
        ; outputLine (out, level, ")")
        )

  fun writeGlobaltype (out, {mut = CONST, valtype = vt}) =
        output (out, valtypeToString vt)
    | writeGlobaltype (out, {mut = VAR, valtype = vt}) =
        output (out, "(mut " ^ valtypeToString vt ^ ")")

  fun writeLimits (out, {min, max = NONE}) =
        output (out, Int.toString min)
    | writeLimits (out, {min, max = SOME max}) =
        output (out, Int.toString min ^ " " ^ Int.toString max)

  fun writeTabletype (out, {limits, elemtype}) =
    (writeLimits (out, limits); output (out, " " ^ reftypeToString elemtype))

  fun writeImport (out, level, {module_name, name, desc}) =
    ( outputIndent (out, level)
    ; output
        ( out
        , "(import \"" ^ escapeString module_name ^ "\" \"" ^ escapeString name
          ^ "\" "
        )
    ; (case desc of
         ImportFunc typeidx =>
           output (out, "(func (type " ^ Int.toString typeidx ^ "))")
       | ImportTable tt =>
           ( output (out, "(table ")
           ; writeTabletype (out, tt)
           ; output (out, ")")
           )
       | ImportMemory {limits} =>
           ( output (out, "(memory ")
           ; writeLimits (out, limits)
           ; output (out, ")")
           )
       | ImportGlobal gt =>
           ( output (out, "(global ")
           ; writeGlobaltype (out, gt)
           ; output (out, ")")
           )
       | ImportTag {functype} =>
           output (out, "(tag (type " ^ Int.toString functype ^ "))"))
    ; output (out, ")\n")
    )

  fun writeFunc (out, level, {typeidx, locals, body}) =
    ( outputIndent (out, level)
    ; output (out, "(func")
    ; output (out, " (type " ^ Int.toString typeidx ^ ")")
    ; List.app (fn vt => output (out, " (local " ^ valtypeToString vt ^ ")"))
        locals
    ; output (out, "\n")
    ; writeInstrs (out, level + 1, body)
    ; outputLine (out, level, ")")
    )

  fun writeTable (out, level, {tabletype, init = _}) =
    ( outputIndent (out, level)
    ; output (out, "(table ")
    ; writeTabletype (out, tabletype)
    ; output (out, ")\n")
    )

  fun writeMemory (out, level, {limits}) =
    ( outputIndent (out, level)
    ; output (out, "(memory ")
    ; writeLimits (out, limits)
    ; output (out, ")\n")
    )

  fun writeGlobal (out, level, {globaltype, init}) =
    ( outputIndent (out, level)
    ; output (out, "(global ")
    ; writeGlobaltype (out, globaltype)
    ; output (out, "\n")
    ; writeInstrs (out, level + 1, init)
    ; outputLine (out, level, ")")
    )

  fun writeExport (out, level, {name, desc}) =
    ( outputIndent (out, level)
    ; output (out, "(export \"" ^ escapeString name ^ "\" ")
    ; (case desc of
         ExportFunc idx => output (out, "(func " ^ Int.toString idx ^ ")")
       | ExportTable idx => output (out, "(table " ^ Int.toString idx ^ ")")
       | ExportMemory idx => output (out, "(memory " ^ Int.toString idx ^ ")")
       | ExportGlobal idx => output (out, "(global " ^ Int.toString idx ^ ")")
       | ExportTag idx => output (out, "(tag " ^ Int.toString idx ^ ")"))
    ; output (out, ")\n")
    )

  fun writeDataSegment (out, level, {init, mode}) =
    let
      fun writeBytes v =
        let
          val len = Word8Vector.length v
          fun loop i =
            if i >= len then
              ()
            else
              ( output (out, "\\" ^ byteToHex (Word8Vector.sub (v, i)))
              ; loop (i + 1)
              )
        in
          loop 0
        end
    in
      outputIndent (out, level);
      output (out, "(data ");
      (case mode of
         DataPassive => ()
       | DataActive {memidx, offset} =>
           ( if memidx = 0 then ()
             else output (out, "(memory " ^ Int.toString memidx ^ ") ")
           ; output (out, "(offset ")
           ; (* Write offset expression inline *)
             List.app
               (fn i =>
                  case i of
                    I32_CONST n => output (out, "i32.const " ^ Int32.toString n)
                  | I64_CONST n => output (out, "i64.const " ^ Int64.toString n)
                  | _ => () (* Other constant exprs; simplified *)) offset
           ; output (out, ") ")
           ));
      output (out, "\"");
      writeBytes init;
      output (out, "\")\n")
    end

  fun writeElem (out, level, {elemtype, init, mode}) =
    ( outputIndent (out, level)
    ; output (out, "(elem ")
    ; (case mode of
         ElemPassive => ()
       | ElemActive {tableidx, offset} =>
           ( if tableidx = 0 then ()
             else output (out, "(table " ^ Int.toString tableidx ^ ") ")
           ; output (out, "(offset ")
           ; List.app
               (fn i =>
                  case i of
                    I32_CONST n => output (out, "i32.const " ^ Int32.toString n)
                  | _ => ()) offset
           ; output (out, ") ")
           )
       | ElemDeclarative => output (out, "declare "))
    ; output (out, reftypeToString elemtype)
    ; List.app
        (fn initExpr =>
           ( output (out, " (item ")
           ; List.app
               (fn i =>
                  case i of
                    REF_FUNC idx => output (out, "ref.func " ^ Int.toString idx)
                  | REF_NULL ht =>
                      output (out, "ref.null " ^ heaptypeToString ht)
                  | _ => ()) initExpr
           ; output (out, ")")
           )) init
    ; output (out, ")\n")
    )

  fun writeTag (out, level, {functype}) =
    outputLine (out, level, "(tag (type " ^ Int.toString functype ^ "))")

  fun writeModule (out, m: module) =
    let
      val
        { types
        , funcs
        , tables
        , mems
        , tags
        , globals
        , elems
        , datas
        , start
        , imports
        , exports
        } = m
      val typeCounter = ref 0
    in
      outputLine (out, 0, "(module");
      (* Types *)
      List.app (fn rt => writeRectype (out, 1, typeCounter, rt)) types;
      (* Imports *)
      List.app (fn imp => writeImport (out, 1, imp)) imports;
      (* Tags *)
      List.app (fn t => writeTag (out, 1, t)) tags;
      (* Functions *)
      List.app (fn f => writeFunc (out, 1, f)) funcs;
      (* Tables *)
      List.app (fn t => writeTable (out, 1, t)) tables;
      (* Memories *)
      List.app (fn mem => writeMemory (out, 1, mem)) mems;
      (* Globals *)
      List.app (fn g => writeGlobal (out, 1, g)) globals;
      (* Exports *)
      List.app (fn e => writeExport (out, 1, e)) exports;
      (* Start *)
      (case start of
         NONE => ()
       | SOME idx => outputLine (out, 1, "(start " ^ Int.toString idx ^ ")"));
      (* Elements *)
      List.app (fn e => writeElem (out, 1, e)) elems;
      (* Data *)
      List.app (fn d => writeDataSegment (out, 1, d)) datas;
      outputLine (out, 0, ")")
    end
end;
