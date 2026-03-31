(*
 * Copyright (c) 2026 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure WasmSyntax :>
sig
  type typeidx = int
  type funcidx = int
  type tableidx = int
  type memidx = int
  type globalidx = int
  type localidx = int
  type labelidx = int
  type tagidx = int
  type dataidx = int
  type fieldidx = int

  (* Packed types (for struct/array storage) *)
  datatype packtype =
    I8
  | I16

  (* Abstract heap types *)
  datatype absheaptype =
    FUNC
  | EXTERN
  | ANY
  | EQ
  | I31
  | STRUCT
  | ARRAY
  | HEAP_NONE
  | NOFUNC
  | NOEXTERN

  (* Heap types *)
  datatype heaptype =
    AbsHeapType of absheaptype
  | TypeIdx of typeidx

  (* Reference types *)
  type reftype = {nullable: bool, heaptype: heaptype}

  (* Number types *)
  datatype numtype =
    I32
  | I64
  | F32
  | F64

  (* Value types *)
  datatype valtype =
    NumType of numtype
  | RefType of reftype

  (* Storage types (for struct/array fields) *)
  datatype storagetype =
    ValStorageType of valtype
  | PackedStorageType of packtype

  (* Mutability *)
  datatype mut =
    CONST
  | VAR

  (* Field type (for struct fields and array element types) *)
  type fieldtype = {mut: mut, storagetype: storagetype}

  (* Struct type *)
  type structtype = fieldtype list

  (* Array type *)
  type arraytype = fieldtype

  (* Function type *)
  type functype = {params: valtype list, results: valtype list}

  (* Composite types *)
  datatype comptype =
    StructType of structtype
  | ArrayType of arraytype
  | FuncType of functype

  (* Sub type (with optional supertypes) *)
  datatype subtype =
    SubType of {final: bool, supertypes: typeidx list, body: comptype}

  (* Recursive type group *)
  type rectype = subtype list

  (* Limits (for tables and memories) *)
  type limits = {min: int, max: int option}

  (* Table type *)
  type tabletype = {limits: limits, elemtype: reftype}

  (* Memory type *)
  type memtype = {limits: limits}

  (* Global type *)
  type globaltype = {mut: mut, valtype: valtype}

  (* Tag type (for exception handling) *)
  type tagtype = {functype: funcidx}

  (* Block type *)
  datatype blocktype =
    BlockTypeNone
  | BlockTypeVal of valtype
  | BlockTypeIdx of typeidx

  (* Integer unary operators *)
  datatype iunop =
    CLZ
  | CTZ
  | POPCNT

  (* Integer binary operators *)
  datatype ibinop =
    ADD
  | SUB
  | MUL
  | DIV_S
  | DIV_U
  | REM_S
  | REM_U
  | AND
  | OR
  | XOR
  | SHL
  | SHR_S
  | SHR_U
  | ROTL
  | ROTR

  (* Float unary operators *)
  datatype funop =
    ABS
  | NEG
  | CEIL
  | FLOOR
  | TRUNC
  | NEAREST
  | SQRT

  (* Float binary operators *)
  datatype fbinop =
    FADD
  | FSUB
  | FMUL
  | FDIV
  | FMIN
  | FMAX
  | FCOPYSIGN

  (* Integer test operator *)
  datatype itestop =
    EQZ

  (* Integer relation operators *)
  datatype irelop =
    IEQ
  | INE
  | LT_S
  | LT_U
  | GT_S
  | GT_U
  | LE_S
  | LE_U
  | GE_S
  | GE_U

  (* Float relation operators *)
  datatype frelop =
    FEQ
  | FNE
  | FLT
  | FGT
  | FLE
  | FGE

  (* Sign extension for packed types *)
  datatype sx =
    S
  | U

  (* Instructions *)
  datatype instr =
  (* Control instructions *)
    UNREACHABLE
  | NOP
  | BLOCK of blocktype * instr list
  | LOOP of blocktype * instr list
  | IF of blocktype * instr list * instr list
  | BR of labelidx
  | BR_IF of labelidx
  | BR_TABLE of labelidx list * labelidx
  | RETURN
  | CALL of funcidx
  | CALL_INDIRECT of tableidx * typeidx
  | CALL_REF of typeidx
  | RETURN_CALL of funcidx
  | RETURN_CALL_REF of typeidx
  (* Exception handling instructions *)
  | TRY_TABLE of blocktype * catch_clause list * instr list
  | THROW of tagidx
  | THROW_REF
  (* Variable instructions *)
  | LOCAL_GET of localidx
  | LOCAL_SET of localidx
  | LOCAL_TEE of localidx
  | GLOBAL_GET of globalidx
  | GLOBAL_SET of globalidx
  (* Numeric instructions *)
  | I32_CONST of Int32.int
  | I64_CONST of Int64.int
  | F32_CONST of Word32.word
  | F64_CONST of real
  | I32_UNOP of iunop
  | I64_UNOP of iunop
  | F32_UNOP of funop
  | F64_UNOP of funop
  | I32_BINOP of ibinop
  | I64_BINOP of ibinop
  | F32_BINOP of fbinop
  | F64_BINOP of fbinop
  | I32_TESTOP of itestop
  | I64_TESTOP of itestop
  | I32_RELOP of irelop
  | I64_RELOP of irelop
  | F32_RELOP of frelop
  | F64_RELOP of frelop
  (* Conversion instructions *)
  | I32_WRAP_I64
  | I64_EXTEND_I32 of sx
  | I32_TRUNC_F32 of sx
  | I32_TRUNC_F64 of sx
  | I64_TRUNC_F32 of sx
  | I64_TRUNC_F64 of sx
  | F32_CONVERT_I32 of sx
  | F32_CONVERT_I64 of sx
  | F64_CONVERT_I32 of sx
  | F64_CONVERT_I64 of sx
  | F32_DEMOTE_F64
  | F64_PROMOTE_F32
  | I32_REINTERPRET_F32
  | I64_REINTERPRET_F64
  | F32_REINTERPRET_I32
  | F64_REINTERPRET_I64
  | I32_EXTEND8_S
  | I32_EXTEND16_S
  | I64_EXTEND8_S
  | I64_EXTEND16_S
  | I64_EXTEND32_S
  (* Reference instructions *)
  | REF_NULL of heaptype
  | REF_IS_NULL
  | REF_FUNC of funcidx
  | REF_EQ
  | REF_AS_NON_NULL
  | REF_CAST of reftype
  | REF_TEST of reftype
  (* i31 instructions *)
  | REF_I31
  | I31_GET of sx
  (* Struct instructions *)
  | STRUCT_NEW of typeidx
  | STRUCT_NEW_DEFAULT of typeidx
  | STRUCT_GET of typeidx * fieldidx
  | STRUCT_GET_S of typeidx * fieldidx
  | STRUCT_GET_U of typeidx * fieldidx
  | STRUCT_SET of typeidx * fieldidx
  (* Array instructions *)
  | ARRAY_NEW of typeidx
  | ARRAY_NEW_DEFAULT of typeidx
  | ARRAY_NEW_FIXED of typeidx * int
  | ARRAY_NEW_DATA of typeidx * dataidx
  | ARRAY_GET of typeidx
  | ARRAY_GET_S of typeidx
  | ARRAY_GET_U of typeidx
  | ARRAY_SET of typeidx
  | ARRAY_LEN
  | ARRAY_COPY of typeidx * typeidx
  (* Extern conversions *)
  | EXTERN_INTERNALIZE
  | EXTERN_EXTERNALIZE
  (* Select *)
  | SELECT of valtype list option
  (* Drop *)
  | DROP
  and catch_clause =
    CATCH of tagidx * labelidx
  | CATCH_REF of tagidx * labelidx
  | CATCH_ALL of labelidx
  | CATCH_ALL_REF of labelidx

  (* Expression (constant expression) *)
  type expr = instr list

  (* Function definition *)
  type func = {typeidx: typeidx, locals: valtype list, body: expr}

  (* Global definition *)
  type global = {globaltype: globaltype, init: expr}

  (* Import description *)
  datatype importdesc =
    ImportFunc of typeidx
  | ImportTable of tabletype
  | ImportMemory of memtype
  | ImportGlobal of globaltype
  | ImportTag of tagtype

  (* Import *)
  type import = {module_name: string, name: string, desc: importdesc}

  (* Export description *)
  datatype exportdesc =
    ExportFunc of funcidx
  | ExportTable of tableidx
  | ExportMemory of memidx
  | ExportGlobal of globalidx
  | ExportTag of tagidx

  (* Export *)
  type export = {name: string, desc: exportdesc}

  (* Table *)
  type table = {tabletype: tabletype, init: expr}

  (* Data segment *)
  datatype datamode =
    DataPassive
  | DataActive of {memidx: memidx, offset: expr}

  type data = {init: Word8Vector.vector, mode: datamode}

  (* Element segment *)
  datatype elemmode =
    ElemPassive
  | ElemActive of {tableidx: tableidx, offset: expr}
  | ElemDeclarative

  type elem = {elemtype: reftype, init: expr list, mode: elemmode}

  (* Module *)
  type module =
    { types: rectype list
    , funcs: func list
    , tables: table list
    , mems: memtype list
    , globals: global list
    , elems: elem list
    , datas: data list
    , start: funcidx option
    , imports: import list
    , exports: export list
    }

  (* Helper functions *)
  val i32: valtype
  val i64: valtype
  val f32: valtype
  val f64: valtype
  val funcref: valtype
  val externref: valtype
  val anyref: valtype
  val eqref: valtype
  val i31ref: valtype
  val structref: valtype
  val nullref: valtype
  val ref_: typeidx -> valtype
  val refnull: typeidx -> valtype
  val emptyModule: module
end =
struct
  type typeidx = int
  type funcidx = int
  type tableidx = int
  type memidx = int
  type globalidx = int
  type localidx = int
  type labelidx = int
  type tagidx = int
  type dataidx = int
  type fieldidx = int

  datatype packtype = I8 | I16

  datatype absheaptype =
    FUNC
  | EXTERN
  | ANY
  | EQ
  | I31
  | STRUCT
  | ARRAY
  | HEAP_NONE
  | NOFUNC
  | NOEXTERN

  datatype heaptype = AbsHeapType of absheaptype | TypeIdx of typeidx

  type reftype = {nullable: bool, heaptype: heaptype}

  datatype numtype = I32 | I64 | F32 | F64

  datatype valtype = NumType of numtype | RefType of reftype

  datatype storagetype =
    ValStorageType of valtype
  | PackedStorageType of packtype

  datatype mut = CONST | VAR

  type fieldtype = {mut: mut, storagetype: storagetype}

  type structtype = fieldtype list

  type arraytype = fieldtype

  type functype = {params: valtype list, results: valtype list}

  datatype comptype =
    StructType of structtype
  | ArrayType of arraytype
  | FuncType of functype

  datatype subtype =
    SubType of {final: bool, supertypes: typeidx list, body: comptype}

  type rectype = subtype list

  type limits = {min: int, max: int option}

  type tabletype = {limits: limits, elemtype: reftype}

  type memtype = {limits: limits}

  type globaltype = {mut: mut, valtype: valtype}

  type tagtype = {functype: funcidx}

  datatype blocktype =
    BlockTypeNone
  | BlockTypeVal of valtype
  | BlockTypeIdx of typeidx

  datatype iunop = CLZ | CTZ | POPCNT

  datatype ibinop =
    ADD
  | SUB
  | MUL
  | DIV_S
  | DIV_U
  | REM_S
  | REM_U
  | AND
  | OR
  | XOR
  | SHL
  | SHR_S
  | SHR_U
  | ROTL
  | ROTR

  datatype funop = ABS | NEG | CEIL | FLOOR | TRUNC | NEAREST | SQRT

  datatype fbinop = FADD | FSUB | FMUL | FDIV | FMIN | FMAX | FCOPYSIGN

  datatype itestop = EQZ

  datatype irelop =
    IEQ
  | INE
  | LT_S
  | LT_U
  | GT_S
  | GT_U
  | LE_S
  | LE_U
  | GE_S
  | GE_U

  datatype frelop = FEQ | FNE | FLT | FGT | FLE | FGE

  datatype sx = S | U

  datatype instr =
    UNREACHABLE
  | NOP
  | BLOCK of blocktype * instr list
  | LOOP of blocktype * instr list
  | IF of blocktype * instr list * instr list
  | BR of labelidx
  | BR_IF of labelidx
  | BR_TABLE of labelidx list * labelidx
  | RETURN
  | CALL of funcidx
  | CALL_INDIRECT of tableidx * typeidx
  | CALL_REF of typeidx
  | RETURN_CALL of funcidx
  | RETURN_CALL_REF of typeidx
  | TRY_TABLE of blocktype * catch_clause list * instr list
  | THROW of tagidx
  | THROW_REF
  | LOCAL_GET of localidx
  | LOCAL_SET of localidx
  | LOCAL_TEE of localidx
  | GLOBAL_GET of globalidx
  | GLOBAL_SET of globalidx
  | I32_CONST of Int32.int
  | I64_CONST of Int64.int
  | F32_CONST of Word32.word
  | F64_CONST of real
  | I32_UNOP of iunop
  | I64_UNOP of iunop
  | F32_UNOP of funop
  | F64_UNOP of funop
  | I32_BINOP of ibinop
  | I64_BINOP of ibinop
  | F32_BINOP of fbinop
  | F64_BINOP of fbinop
  | I32_TESTOP of itestop
  | I64_TESTOP of itestop
  | I32_RELOP of irelop
  | I64_RELOP of irelop
  | F32_RELOP of frelop
  | F64_RELOP of frelop
  | I32_WRAP_I64
  | I64_EXTEND_I32 of sx
  | I32_TRUNC_F32 of sx
  | I32_TRUNC_F64 of sx
  | I64_TRUNC_F32 of sx
  | I64_TRUNC_F64 of sx
  | F32_CONVERT_I32 of sx
  | F32_CONVERT_I64 of sx
  | F64_CONVERT_I32 of sx
  | F64_CONVERT_I64 of sx
  | F32_DEMOTE_F64
  | F64_PROMOTE_F32
  | I32_REINTERPRET_F32
  | I64_REINTERPRET_F64
  | F32_REINTERPRET_I32
  | F64_REINTERPRET_I64
  | I32_EXTEND8_S
  | I32_EXTEND16_S
  | I64_EXTEND8_S
  | I64_EXTEND16_S
  | I64_EXTEND32_S
  | REF_NULL of heaptype
  | REF_IS_NULL
  | REF_FUNC of funcidx
  | REF_EQ
  | REF_AS_NON_NULL
  | REF_CAST of reftype
  | REF_TEST of reftype
  | REF_I31
  | I31_GET of sx
  | STRUCT_NEW of typeidx
  | STRUCT_NEW_DEFAULT of typeidx
  | STRUCT_GET of typeidx * fieldidx
  | STRUCT_GET_S of typeidx * fieldidx
  | STRUCT_GET_U of typeidx * fieldidx
  | STRUCT_SET of typeidx * fieldidx
  | ARRAY_NEW of typeidx
  | ARRAY_NEW_DEFAULT of typeidx
  | ARRAY_NEW_FIXED of typeidx * int
  | ARRAY_NEW_DATA of typeidx * dataidx
  | ARRAY_GET of typeidx
  | ARRAY_GET_S of typeidx
  | ARRAY_GET_U of typeidx
  | ARRAY_SET of typeidx
  | ARRAY_LEN
  | ARRAY_COPY of typeidx * typeidx
  | EXTERN_INTERNALIZE
  | EXTERN_EXTERNALIZE
  | SELECT of valtype list option
  | DROP
  and catch_clause =
    CATCH of tagidx * labelidx
  | CATCH_REF of tagidx * labelidx
  | CATCH_ALL of labelidx
  | CATCH_ALL_REF of labelidx

  type expr = instr list

  type func = {typeidx: typeidx, locals: valtype list, body: expr}

  type global = {globaltype: globaltype, init: expr}

  datatype importdesc =
    ImportFunc of typeidx
  | ImportTable of tabletype
  | ImportMemory of memtype
  | ImportGlobal of globaltype
  | ImportTag of tagtype

  type import = {module_name: string, name: string, desc: importdesc}

  datatype exportdesc =
    ExportFunc of funcidx
  | ExportTable of tableidx
  | ExportMemory of memidx
  | ExportGlobal of globalidx
  | ExportTag of tagidx

  type export = {name: string, desc: exportdesc}

  type table = {tabletype: tabletype, init: expr}

  datatype datamode = DataPassive | DataActive of {memidx: memidx, offset: expr}

  type data = {init: Word8Vector.vector, mode: datamode}

  datatype elemmode =
    ElemPassive
  | ElemActive of {tableidx: tableidx, offset: expr}
  | ElemDeclarative

  type elem = {elemtype: reftype, init: expr list, mode: elemmode}

  type module =
    { types: rectype list
    , funcs: func list
    , tables: table list
    , mems: memtype list
    , globals: global list
    , elems: elem list
    , datas: data list
    , start: funcidx option
    , imports: import list
    , exports: export list
    }

  val i32 = NumType I32
  val i64 = NumType I64
  val f32 = NumType F32
  val f64 = NumType F64
  val funcref = RefType {nullable = true, heaptype = AbsHeapType FUNC}
  val externref = RefType {nullable = true, heaptype = AbsHeapType EXTERN}
  val anyref = RefType {nullable = true, heaptype = AbsHeapType ANY}
  val eqref = RefType {nullable = true, heaptype = AbsHeapType EQ}
  val i31ref = RefType {nullable = true, heaptype = AbsHeapType I31}
  val structref = RefType {nullable = true, heaptype = AbsHeapType STRUCT}
  val nullref = RefType {nullable = true, heaptype = AbsHeapType HEAP_NONE}
  fun ref_ idx =
    RefType {nullable = false, heaptype = TypeIdx idx}
  fun refnull idx =
    RefType {nullable = true, heaptype = TypeIdx idx}
  val emptyModule: module =
    { types = []
    , funcs = []
    , tables = []
    , mems = []
    , globals = []
    , elems = []
    , datas = []
    , start = NONE
    , imports = []
    , exports = []
    }
end;
