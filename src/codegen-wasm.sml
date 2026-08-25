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
    , importedFuncs: int StringMap.map StringMap.map ref
    (* module_name -> func_name -> funcIdx *)
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
    , taggedDataTypeIdx: WasmSyntax.typeidx (* $TaggedData: (struct (field i32) (field anyref)) *)
    , stringTypeIdx: WasmSyntax.typeidx (* $String: (array (mut i8)) *)
    , arrayTypeIdx: WasmSyntax.typeidx (* $Array: (array (mut anyref)) *)
    , refTypeIdx: WasmSyntax.typeidx (* $Ref: (struct (field (mut anyref))) *)
    , needsLinearMemory: bool ref
    }

  type FuncContext =
    { ctx: Context
    , nextLocalIdx: int ref
    , revLocalTypes: WasmSyntax.valtype list ref (* reversed *)
    }

  val labelToFieldIndex: Syntax.Label * WasmRepType.Ty Syntax.LabelMap.map
                         -> int

  val getTupleTypeIdx: Context -> int -> WasmSyntax.typeidx

  val emitBox: FSyntax.UnboxedTy * Context -> WasmSyntax.instr list
  val emitUnbox: FSyntax.UnboxedTy * Context -> WasmSyntax.instr list

  (* Entry point *)
  val initContext: unit -> Context
  val doProgram: Context
                 -> CSyntax.CVar
                 -> WasmRepType.Ty NSyntax.stat
                 -> ToFSyntax.export_entity
                 -> WasmSyntax.module
end =
struct
  exception CodeGenError of string

  structure F = FSyntax
  structure C = CSyntax
  structure N = NSyntax
  structure R = WasmRepType
  structure W = WasmSyntax

  type Context =
    { nextTypeIdx: int ref
    , nextFuncIdx: int ref
    , revTypes: W.rectype list ref
    , revFuncs: W.func list ref
    , revGlobals: W.global list ref
    , revImports: W.import list ref
    , importedFuncs: int StringMap.map StringMap.map ref
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
    , taggedDataTypeIdx: W.typeidx
    , stringTypeIdx: W.typeidx
    , arrayTypeIdx: W.typeidx
    , refTypeIdx: W.typeidx
    , needsLinearMemory: bool ref
    }

  type FuncContext =
    {ctx: Context, nextLocalIdx: int ref, revLocalTypes: W.valtype list ref}

  val anyRefType: W.reftype = {nullable = true, heaptype = W.AbsHeapType W.ANY}
  val anyref: W.valtype = W.RefType anyRefType
  val anyref_nn: W.valtype =
    W.RefType {nullable = false, heaptype = W.AbsHeapType W.ANY}

  (* ==================== Type Mapping ==================== *)

  (* Map WasmRepType.Ty to unboxed type tag, if applicable *)
  fun tyToUnboxedTy (R.Unboxed ubt) = SOME ubt
    | tyToUnboxedTy _ = NONE

  (* Convert WasmRepType.Ty to Wasm valtype for local variable allocation.
     After boxing (NSyntaxFromCpsWasm), unboxed types appear in certain
     positions; boxed types use anyref. *)
  fun tyToWasmType (R.Unboxed ubt) =
        (case ubt of
           F.UBTyInt64 => W.NumType W.I64
         | F.UBTyWord64 => W.NumType W.I64
         | F.UBTyReal => W.NumType W.F64
         | _ (* I32 family *) => W.NumType W.I32)
    | tyToWasmType _ = anyref (* boxed types use anyref *)

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
            {mut = W.CONST, storagetype = W.ValStorageType anyref})
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
    | F.UBTyWasmPtr => [W.STRUCT_NEW (#boxedI32TypeIdx ctx)]
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
      | F.UBTyWasmPtr =>
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

  (* ==================== Constructor Tag Index ==================== *)

  (* Compute the 0-based integer index of a constructor in its datatype.
     Constructors are indexed in sorted VId order (String.compare on the tag name). *)
  fun constructorTagIndex (info: Syntax.ValueConstructorInfo) : int =
    let
      val target = Syntax.MkVId (#tag info)
      val allCons = Syntax.VIdSet.listItems (#allConstructors info)
      fun findIdx [] _ =
            raise CodeGenError
              ("constructorTagIndex: tag not found: " ^ #tag info)
        | findIdx (vid :: rest) i =
            if Syntax.eqVId (vid, target) then i else findIdx rest (i + 1)
    in
      findIdx allCons 0
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
    { vars: (W.localidx * F.UnboxedTy option) TypedSyntax.VIdMap.map
    , continuations: cont_repr C.CVarMap.map
    }

  (* Convert an UnboxedTy option to the corresponding Wasm valtype *)
  fun ubtOptToWasmTy NONE = anyref
    | ubtOptToWasmTy (SOME F.UBTyInt32) = W.NumType W.I32
    | ubtOptToWasmTy (SOME F.UBTyWord32) = W.NumType W.I32
    | ubtOptToWasmTy (SOME F.UBTyWasmPtr) = W.NumType W.I32
    | ubtOptToWasmTy (SOME F.UBTyBool) = W.NumType W.I32
    | ubtOptToWasmTy (SOME F.UBTyChar) = W.NumType W.I32
    | ubtOptToWasmTy (SOME F.UBTyChar16) = W.NumType W.I32
    | ubtOptToWasmTy (SOME F.UBTyChar32) = W.NumType W.I32
    | ubtOptToWasmTy (SOME F.UBTyInt64) = W.NumType W.I64
    | ubtOptToWasmTy (SOME F.UBTyWord64) = W.NumType W.I64
    | ubtOptToWasmTy (SOME F.UBTyReal) = W.NumType W.F64

  val emptyEnv: Env =
    {vars = TypedSyntax.VIdMap.empty, continuations = C.CVarMap.empty}

  fun envWithVar (env: Env, v, idx, ubt_opt) : Env =
    { vars = TypedSyntax.VIdMap.insert (#vars env, v, (idx, ubt_opt))
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
      val params = closureRef :: List.tabulate (nParams, fn _ => anyref)
      val results = [anyref]
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
        {mut = W.CONST, storagetype = W.ValStorageType anyref})
      val subtype = W.SubType
        { final = false
        , supertypes = [#closureBaseTypeIdx ctx]
        , body = W.StructType (codeField :: freeVarFields)
        }
      val () = #revTypes ctx := [subtype] :: !(#revTypes ctx)
    in
      idx
    end

  (* Create a mutable closure struct type (for recursive closures that need backpatching).
   * nConstFreeVars outer free variable fields are CONST; nVarSlots self/sibling
   * reference fields are VAR (backpatched after construction). *)
  fun getMutClosureTypeIdx (ctx: Context) (nConstFreeVars: int) (nVarSlots: int)
    (funcTypeIdx: W.typeidx) =
    let
      val idx = allocTypeIdx ctx
      val codeField =
        { mut = W.CONST
        , storagetype = W.ValStorageType
            (W.RefType {nullable = false, heaptype = W.TypeIdx funcTypeIdx})
        }
      val constFreeVarFields = List.tabulate (nConstFreeVars, fn _ =>
        {mut = W.CONST, storagetype = W.ValStorageType anyref})
      val varSlotFields = List.tabulate (nVarSlots, fn _ =>
        {mut = W.VAR, storagetype = W.ValStorageType anyref})
      val subtype = W.SubType
        { final = false
        , supertypes = [#closureBaseTypeIdx ctx]
        , body = W.StructType (codeField :: constFreeVarFields @ varSlotFields)
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

  (* ==================== ForeignCallOp import pre-registration ==================== *)

  (* Convert a representation type to a Wasm valtype for a Wasm import
     parameter/result.
     Only primitive types are supported; others raise CodeGenError. *)
  fun foreignTyToWasmType (ty: R.Ty) : W.valtype =
    case tyToUnboxedTy ty of
      SOME F.UBTyInt64 => W.NumType W.I64
    | SOME F.UBTyWord64 => W.NumType W.I64
    | SOME F.UBTyReal => W.NumType W.F64
    | SOME _ (* I32 family *) => W.NumType W.I32
    | NONE =>
        raise CodeGenError
          ("_wasmImportFunction: unsupported argument/result type for Wasm import"
           ^ " (only primitive types i32/i64/f64 are supported)")

  (* Scan NSyntax to collect all (modName, fnName, paramWasmTys, resultWasmTys)
     from ForeignCallOp occurrences.  Results are deduplicated by (mod, fn). *)
  fun collectForeignCallOps (stat: R.Ty N.stat) :
    (string * string * W.valtype list * W.valtype list) list =
    let
      val seen: (string * string) list ref = ref []
      val result: (string * string * W.valtype list * W.valtype list) list ref =
        ref []
      fun addIfNew (m, f, ptys, rtys) =
        if List.exists (fn (m', f') => m = m' andalso f = f') (!seen) then ()
        else (seen := (m, f) :: !seen; result := (m, f, ptys, rtys) :: !result)
      fun scanExp (N.PrimOp {primOp = F.ForeignCallOp (m, f, n), tyargs, ...}) =
            let
              val paramTys = List.map foreignTyToWasmType
                (List.take (tyargs, n))
              val retTy = List.last tyargs
              val resultTys =
                case retTy of
                  R.Record fields =>
                    if Syntax.LabelMap.isEmpty fields then []
                    else [foreignTyToWasmType retTy]
                | _ => [foreignTyToWasmType retTy]
            in
              addIfNew (m, f, paramTys, resultTys)
            end
        | scanExp (N.PrimOp {args, ...}) = List.app scanExp args
        | scanExp (N.Value _) = ()
        | scanExp (N.Record fields) = Syntax.LabelMap.app scanExp fields
        | scanExp (N.ExnTag _) = ()
        | scanExp (N.Projection {record, ...}) = scanExp record
        | scanExp (N.Abs {body, ...}) = scanStat body
        | scanExp (N.LogicalAnd (a, b)) =
            (scanExp a; scanExp b)
        | scanExp (N.LogicalOr (a, b)) =
            (scanExp a; scanExp b)
      and scanDec (N.ValDec {exp, ...}) = scanExp exp
        | scanDec (N.RecDec defs) =
            List.app (fn d => scanStat (#body d)) defs
        | scanDec (N.ContDec {body, ...}) = scanStat body
        | scanDec (N.RecContDec defs) =
            List.app (fn (_, _, body) => scanStat body) defs
        | scanDec (N.ESImportDec _) = ()
      and scanStat (N.Let {decs, cont}) =
            (List.app scanDec decs; scanStat cont)
        | scanStat (N.App {applied, args, ...}) =
            (scanExp applied; List.app scanExp args)
        | scanStat (N.AppCont {args, ...}) = List.app scanExp args
        | scanStat (N.If {cond, thenCont, elseCont}) =
            (scanExp cond; scanStat thenCont; scanStat elseCont)
        | scanStat (N.Handle {body, handler = (_, hStat), ...}) =
            (scanStat body; scanStat hStat)
        | scanStat (N.Raise (_, e)) = scanExp e
        | scanStat N.Unreachable = ()
    in
      scanStat stat;
      List.rev (!result)
    end

  (* Register a Wasm function import, allocating its funcIdx.
     Must be called before any allocFuncIdx for module-defined functions. *)
  fun registerForeignImport (ctx: Context)
    ( modName: string
    , fnName: string
    , paramTys: W.valtype list
    , resultTys: W.valtype list
    ) : unit =
    let
      val modMap = !(#importedFuncs ctx)
      val fnMap =
        case StringMap.find (modMap, modName) of
          SOME m => m
        | NONE => StringMap.empty
    in
      if StringMap.inDomain (fnMap, fnName) then
        ()
      else
        let
          val typeIdx = getFuncTypeIdx ctx (paramTys, resultTys)
          val funcIdx =
            allocFuncIdx ctx (* import gets index before module funcs *)
          val newFnMap = StringMap.insert (fnMap, fnName, funcIdx)
          val () =
            #importedFuncs ctx := StringMap.insert (modMap, modName, newFnMap)
          val () =
            #revImports ctx
            :=
            {module_name = modName, name = fnName, desc = W.ImportFunc typeIdx}
            :: !(#revImports ctx)
        in
          ()
        end
    end

  (* Look up the funcIdx of a previously registered foreign import. *)
  fun lookupForeignImport (ctx: Context) (modName: string, fnName: string) : int =
    case StringMap.find (!(#importedFuncs ctx), modName) of
      SOME fnMap =>
        (case StringMap.find (fnMap, fnName) of
           SOME idx => idx
         | NONE =>
             raise CodeGenError
               ("ForeignCallOp: import not registered: " ^ modName ^ "."
                ^ fnName))
    | NONE =>
        raise CodeGenError
          ("ForeignCallOp: module not registered: " ^ modName ^ "." ^ fnName)

  fun doValue (fctx: FuncContext) (env: Env) (v: C.Value, acc: W.instr list) :
    W.instr list =
    case v of
      C.Var vid =>
        (case TypedSyntax.VIdMap.find (#vars env, vid) of
           SOME (idx, _) => W.LOCAL_GET idx :: acc
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
    | C.IntConst (Primitives.INT, _) =>
        raise CodeGenError "doValue: INT should have been lowered to I32"
    | C.IntConst (Primitives.INT_INF, _) =>
        raise CodeGenError "doValue: INT_INF not supported in Wasm"
    | C.WordConst (Primitives.W32, n) =>
        W.I32_CONST (Int32.fromLarge
          (Word32.toLargeIntX (Word32.fromLargeInt n))) :: acc
    | C.WordConst (Primitives.W64, n) =>
        W.I64_CONST (Int64.fromLarge
          (Word64.toLargeIntX (Word64.fromLargeInt n))) :: acc
    | C.WordConst (Primitives.WORD, _) =>
        raise CodeGenError "doValue: WORD should have been lowered to W32"
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

  (* String constants: store in data segment and create (array (mut i8)) via array.new_data.
     Instructions are prepended to acc in reverse execution order (since List.rev is applied later). *)
  and doStringConst ((fctx: FuncContext), s: string, acc: W.instr list) =
    let
      val ctx = #ctx fctx
      val strTypeIdx = #stringTypeIdx ctx
      val n = Int32.fromInt (String.size s)
    in
      if String.size s = 0 then
        (* array.new_default needs size on stack; produces a zero-length i8 array *)
        W.ARRAY_NEW_DEFAULT strTypeIdx :: W.I32_CONST 0 :: acc
      else
        let
          val bytes = Word8Vector.tabulate (String.size s, fn i =>
            Word8.fromInt (Char.ord (String.sub (s, i))))
          val dataIdx = List.length (!(#revDatas ctx))
          val () =
            #revDatas ctx
            := {init = bytes, mode = W.DataPassive} :: !(#revDatas ctx)
        in
          (* array.new_data $T $D: pops offset (i32) then length (i32) *)
          W.ARRAY_NEW_DATA (strTypeIdx, dataIdx) :: W.I32_CONST n
          :: W.I32_CONST 0 :: acc
        end
    end

  (* ==================== doExp ==================== *)

  and doExp (fctx: FuncContext) (env: Env) (exp: R.Ty N.exp, acc: W.instr list) :
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
                (* Struct fields are anyref; NSyntaxFromCpsWasm has already
                   boxed any field of unboxed type. *)
                val acc' =
                  Syntax.LabelMap.foldl (fn (e, a) => doExp fctx env (e, a)) acc
                    fields
              in
                W.STRUCT_NEW tupleIdx :: acc'
              end
          end
      | N.ExnTag {name, payloadTy = _} =>
          (* Create a fresh ExnTagType struct instance for unique identity via
             ref.eq; the field holds the exception name for General.exnName *)
          W.STRUCT_NEW (#exnTagTypeIdx ctx) :: doStringConst (fctx, name, acc)
      | N.Projection projection => doProjection fctx env (projection, acc)
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

  (* Emit a record/tuple projection.
     Tuple struct fields are always anyref, so struct.get returns anyref;
     NSyntaxFromCpsWasm unboxes the result where the field type is unboxed. *)
  and doProjection (fctx: FuncContext) (env: Env)
    ( {label, record, fieldTypes}:
        { label: Syntax.Label
        , record: R.Ty N.exp
        , fieldTypes: R.Ty Syntax.LabelMap.map
        }
    , acc: W.instr list
    ) : W.instr list =
    let
      val ctx = #ctx fctx
      val n = Syntax.LabelMap.numItems fieldTypes
      val tupleIdx = getTupleTypeIdx ctx n
      val fieldIdx = labelToFieldIndex (label, fieldTypes)
    in
      W.STRUCT_GET (tupleIdx, fieldIdx)
      :: W.REF_CAST {nullable = false, heaptype = W.TypeIdx tupleIdx}
      :: doExp fctx env (record, acc)
    end

  (* ==================== Closure generation (Abs) ==================== *)

  and doAbs (fctx: FuncContext) (env: Env)
    ( contParam: C.CVar
    , params: (C.Var * R.Ty) list
    , body: R.Ty N.stat
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
      val selfLocal = allocLocal innerFctx anyref (* param 0: closure ref *)
      val paramLocals =
        List.map (fn _ => allocLocal innerFctx anyref) params (* params *)

      (* Build environment for function body.
         Params arrive as anyref in the Wasm closure convention.
         If a param's FSyntax type corresponds to an unboxed numeric type (e.g. int32),
         allocate an unboxed local and emit preamble instructions to unbox. *)
      val innerEnv = emptyEnv
      val innerEnv = envWithCont (innerEnv, contParam, RETURN)
      val (revParamUnboxPreamble, innerEnv) =
        ListPair.foldl
          (fn ((v, ty), rawLocalIdx, (revAcc, e)) =>
             let
               val ubt = tyToUnboxedTy ty
               val wasmTy = ubtOptToWasmTy ubt
             in
               case ubt of
                 NONE => (revAcc, envWithVar (e, v, rawLocalIdx, NONE))
               | SOME ubty =>
                   let
                     val unboxedLocalIdx = allocLocal innerFctx wasmTy
                     val instrs =
                       W.LOCAL_SET unboxedLocalIdx
                       ::
                       List.revAppend
                         ( emitUnbox (ubty, ctx)
                         , W.LOCAL_GET rawLocalIdx :: revAcc
                         )
                   in
                     (instrs, envWithVar (e, v, unboxedLocalIdx, ubt))
                   end
             end) ([], innerEnv) (params, paramLocals)

      (* Add free variables: extract from closure struct.
       * Free vars with numeric types (i32/i64/f64) are stored boxed in the closure
       * struct (anyref), so we need to unbox them when loading into the inner function.
       * Build preamble as reverse accumulator. *)
      val (revPreamble, innerEnv) =
        let
          fun go ([], _, revAcc, e) = (revAcc, e)
            | go (fv :: rest, fieldIdx, revAcc, e) =
                let
                  (* Look up the outer unboxed type of this free variable *)
                  val outerUbt =
                    case TypedSyntax.VIdMap.find (#vars env, fv) of
                      SOME (_, ubt) => ubt
                    | NONE => NONE
                  val outerWasmTy = ubtOptToWasmTy outerUbt
                  val localIdx = allocLocal innerFctx outerWasmTy
                  val e' = envWithVar (e, fv, localIdx, outerUbt)
                  (* Base: load the (boxed) free variable from the closure struct *)
                  val baseInstrs =
                    W.STRUCT_GET (closureTypeIdx, fieldIdx)
                    ::
                    W.REF_CAST
                      {nullable = false, heaptype = W.TypeIdx closureTypeIdx}
                    :: W.LOCAL_GET selfLocal :: revAcc
                  (* If unboxed type, unbox it; otherwise store anyref directly *)
                  val fullInstrs =
                    case outerUbt of
                      NONE => W.LOCAL_SET localIdx :: baseInstrs
                    | SOME ubty =>
                        W.LOCAL_SET localIdx
                        :: List.revAppend (emitUnbox (ubty, ctx), baseInstrs)
                in
                  go (rest, fieldIdx + 1, fullInstrs, e')
                end
        in
          go
            ( freeVars
            , 1
            , revParamUnboxPreamble
            , innerEnv
            ) (* field 0 is code ptr *)
        end

      (* Generate body with preamble as initial accumulator.
         Append UNREACHABLE so functions that always exit via explicit RETURN
         (e.g., loops with internal returns) satisfy the validator. *)
      val bodyInstrs = List.rev
        (W.UNREACHABLE :: doStat innerFctx innerEnv (body, revPreamble))

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

      (* At call site: create the closure struct.
       * If a free variable has numeric type (stored unboxed in outer scope),
       * box it before storing in the closure struct (field type = anyref). *)
      val acc = W.REF_FUNC funcIdx :: acc
      val acc =
        List.foldl
          (fn (fv, a) =>
             case TypedSyntax.VIdMap.find (#vars env, fv) of
               SOME (idx, SOME ubty) =>
                 List.revAppend (emitBox (ubty, ctx), W.LOCAL_GET idx :: a)
             | SOME (idx, NONE) => W.LOCAL_GET idx :: a
             | NONE =>
                 raise CodeGenError
                   ("doAbs: free var not in scope: " ^ TypedSyntax.print_VId fv))
          acc freeVars
    in
      W.STRUCT_NEW closureTypeIdx :: acc
    end

  (* ==================== doStat ==================== *)

  and doStat (fctx: FuncContext) (env: Env)
    (stat: R.Ty N.stat, acc: W.instr list) : W.instr list =
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
          val eLocal = allocLocal fctx anyref
          (* Env for body: add successfulExitIn → innerOutRepr *)
          val bodyEnv = envWithCont (env, successfulExitIn, innerOutRepr)
          (* Env for handler: bind exception variable e to eLocal *)
          val handlerEnv = envWithVar (env, e, eLocal, NONE)
          (* Generate body and handler code *)
          val bodyCode = List.rev (doStat fctx bodyEnv (body, []))
          val handlerCode = List.rev (doStat fctx handlerEnv (h, []))
          val smlExnTagIdx = #smlExnTagIdx ctx
          (* Structure:
             block (result anyref)       ;; outerBlock: catch target (label 0 from try_table's outer context)
               try_table (catch $sml_exn 0)  ;; 0 = outerBlock (immediately enclosing, counted from outside try_table)
                 <body>                 ;; body uses bumpContRepr 2 labels (try_table=1, outerBlock=1)
                 unreachable            ;; body always transfers control
               end
               unreachable              ;; never reached
             end                        ;; exception (anyref) falls through here
             local.set eLocal           ;; bind caught exception
             <handlerCode>              ;; handler runs in original env *)
          val outerBlock = W.BLOCK
            ( W.BlockTypeVal anyref
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
        (* Pop exception value (anyref = $SmlExn) from stack and throw *)
        W.THROW (#smlExnTagIdx (#ctx fctx)) :: doExp fctx env (exp, acc)
    | N.Unreachable => W.UNREACHABLE :: acc

  (* ==================== Function application ==================== *)

  and doApp (fctx: FuncContext) (env: Env)
    ( applied: R.Ty N.exp
    , cont: C.CVar
    , args: R.Ty N.exp list
    , acc: W.instr list
    ) : W.instr list =
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
            val closureLocal = allocLocal fctx anyref
            val acc = emitClosureCall acc applied args funcTypeIdx closureLocal
          in
            W.RETURN_CALL_REF funcTypeIdx :: acc
          end
      | SOME (BREAK_TO {label, params}) =>
          let
            val funcTypeIdx = getClosureFuncTypeIdx ctx nArgs
            val closureLocal = allocLocal fctx anyref
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
            val closureLocal = allocLocal fctx anyref
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
    (applied: C.CVar, args: R.Ty N.exp list, acc: W.instr list) : W.instr list =
    let
      val contRepr = C.CVarMap.find (#continuations env, applied)
    in
      case contRepr of
        SOME RETURN =>
          (case args of
             [arg] => W.RETURN :: doExp fctx env (arg, acc)
           | [] => W.RETURN :: acc
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
            (* Push all args first (evaluate in order using original param values),
               then store in reverse order. This avoids the parallel assignment
               problem where storing param[i] before evaluating arg[i+1] would
               clobber a param value that a later arg depends on. *)
            val acc =
              List.foldl (fn (arg, a) => doExp fctx env (arg, a)) acc args
            val acc =
              List.foldl
                (fn (SOME p, a) => W.LOCAL_SET p :: a | (NONE, a) => W.DROP :: a)
                acc (List.rev params)
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
    (decs: R.Ty N.dec list, finalCont: R.Ty N.stat, acc: W.instr list) :
    W.instr list =
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
          (* The BLOCK we are about to emit adds a label level: bump the labels
             of the continuations already in scope so that `br`s to them from
             inside the block stay correct. *)
          val env' = envWithCont
            ( bumpEnvConts 1 env
            , name
            , BREAK_TO {label = 0, params = paramLocals}
            )
          (* Generate the code inside the block (rest of decs + final cont) *)
          val innerCode = List.rev
            (doLetDecs fctx env' (restDecs, finalCont, []))
          (* Build env for continuation body with param locals *)
          val bodyEnv =
            ListPair.foldl
              (fn ((SOME v, ty), SOME localIdx, e) =>
                 envWithVar (e, v, localIdx, tyToUnboxedTy ty)
                | (_, _, e) => e) env (params, paramLocals)
          val contBodyCode = List.rev (doStat fctx bodyEnv (body, []))
        in
          (* block ... end ; contBody *)
          List.revAppend
            (contBodyCode, W.BLOCK (W.BlockTypeNone, innerCode) :: acc)
        end
    | N.RecContDec defs :: restDecs =>
        (* RecContDec: mutually-recursive tail-recursive continuations.
           Common case: no more declarations, finalCont is initial AppCont to one of the defs.
           Generate:
             1. Set param locals from initial args
             2. LOOP containing each continuation body
                (AppCont to a rec cont = set params + BR 0 = loop back)
           Multiple continuations use a `which` local for dispatch. *)
        let
          (* Step 1: allocate locals (label=0 inside the loop is correct) *)
          val (env', _) = doRecContDec fctx env (defs, [])

          (* Helper: get param locals for a continuation by name *)
          fun getParamLocals k =
            case C.CVarMap.find (#continuations env', k) of
              SOME (CONTINUE_TO {params = pl, ...}) => pl
            | _ => raise CodeGenError "RecContDec: getParamLocals"

          (* Step 2: detect initial call pattern:
             finalCont = AppCont k args for k in defs.
             restDecs (decs between this RecContDec and finalCont) are processed
             before entering the loop. *)
          val initOpt =
            case finalCont of
              N.AppCont {applied = k, args = initArgs} =>
                (case List.find (fn (name, _, _) => name = k) defs of
                   SOME (_, params, _) => SOME (k, params, initArgs)
                 | NONE => NONE)
            | _ => NONE
        in
          case initOpt of
            SOME (initK, initParams, initArgs) =>
              let
                (* First: process restDecs (e.g., ValDecs that compute helper values
                   used inside the rec cont's body) before entering the loop. *)
                val (envAfterRest, acc) =
                  List.foldl (fn (dec, (e, a)) => doDec fctx e (dec, a))
                    (env, acc) restDecs

                val initParamLocals = getParamLocals initK
                (* Emit initial param assignments (fold over 3 lists simultaneously).
                   Use envAfterRest so initArgs can reference vars bound by restDecs. *)
                val acc =
                  let
                    fun go ([], [], [], a) = a
                      | go (lOpt :: lRest, _ :: pRest, arg :: aRest, a) =
                          let
                            val a' =
                              case lOpt of
                                SOME lIdx =>
                                  W.LOCAL_SET lIdx
                                  :: doExp fctx envAfterRest (arg, a)
                              | NONE =>
                                  W.DROP :: doExp fctx envAfterRest (arg, a)
                          in
                            go (lRest, pRest, aRest, a')
                          end
                      | go _ =
                          raise CodeGenError "RecContDec: param list mismatch"
                  in
                    go (initParamLocals, initParams, initArgs, acc)
                  end

                (* Generate which-local for multiple defs; for single def no dispatch *)
                val numDefs = List.length defs
                val whichLocalOpt =
                  case C.CVarMap.find (#continuations env', initK) of
                    SOME (CONTINUE_TO {which = SOME (wl, _), ...}) =>
                      if numDefs > 1 then SOME wl else NONE
                  | _ => NONE
                val acc =
                  case whichLocalOpt of
                    NONE => acc
                  | SOME wl =>
                      let
                        val initIdx =
                          case C.CVarMap.find (#continuations env', initK) of
                            SOME (CONTINUE_TO {which = SOME (_, idx), ...}) =>
                              idx
                          | _ =>
                              raise CodeGenError
                                "RecContDec: no which idx for initK"
                      in
                        W.LOCAL_SET wl :: W.I32_CONST (Int32.fromInt initIdx)
                        :: acc
                      end

                (* Build loop body: for each continuation body, generate code.
                   loopEnv: outer continuations bumped by 1 (for the LOOP block depth),
                   but this RecContDec's own CONTINUE_TOs stay at label=0 (BR 0 = loop back).
                   Also add params as vars so the loop body can read them.
                   Base off envAfterRest so vars bound by restDecs are visible inside the loop. *)
                val loopEnv =
                  let
                    val bumpedBase = bumpEnvConts 1 envAfterRest
                    (* Re-add CONTINUE_TO entries (label=0 = the LOOP) *)
                    val withConts =
                      List.foldl
                        (fn ((name, _, _), e) =>
                           let
                             val repr = valOf
                               (C.CVarMap.find (#continuations env', name))
                           in
                             envWithCont (e, name, repr)
                           end) bumpedBase defs
                    (* Add param vars: each def's params are in locals allocated by doRecContDec *)
                    val withParams =
                      List.foldl
                        (fn ((name, params, _), e) =>
                           let
                             val paramLocals = getParamLocals name
                           in
                             ListPair.foldl
                               (fn ((SOME v, ty), SOME idx, e) =>
                                  envWithVar (e, v, idx, tyToUnboxedTy ty)
                                 | (_, _, e) => e) e (params, paramLocals)
                           end) withConts defs
                  in
                    withParams
                  end

                val loopBodyCode =
                  if numDefs = 1 then
                    let val (_, _, body) = List.hd defs
                    in List.rev (doStat fctx loopEnv (body, []))
                    end
                  else
                    (* Multiple defs: dispatch on which local *)
                    let
                      val whichLocal =
                        case C.CVarMap.find (#continuations env', initK) of
                          SOME (CONTINUE_TO {which = SOME (wl, _), ...}) => wl
                        | _ => raise CodeGenError "RecContDec: no which local"
                      (* Generate body for each def, wrapped in blocks for dispatch *)
                      (* For now, use nested if-else dispatch *)
                      fun genDispatch [] = [W.UNREACHABLE]
                        | genDispatch [(name, _, body)] =
                            List.rev (doStat fctx loopEnv (body, []))
                        | genDispatch ((name, _, body) :: rest) =
                            let
                              val idx =
                                case C.CVarMap.find (#continuations env', name) of
                                  SOME (CONTINUE_TO {which = SOME (_, i), ...}) =>
                                    i
                                | _ =>
                                    raise CodeGenError
                                      "RecContDec: no which idx"
                              val bodyCode = List.rev
                                (doStat fctx loopEnv (body, []))
                              val restCode = genDispatch rest
                            in
                              [ W.UNREACHABLE (* simplified: dispatch not fully implemented *)
                              , W.IF (W.BlockTypeNone, bodyCode, restCode)
                              , W.I32_CONST (Int32.fromInt idx)
                              , W.LOCAL_GET whichLocal
                              , W.I32_RELOP W.IEQ
                              ]
                            end
                    in
                      genDispatch defs
                    end

                val acc = W.LOOP (W.BlockTypeNone, loopBodyCode) :: acc
              in
                acc
              end
          | NONE =>
              (* Fallback: just register continuations and continue with restDecs *)
              let val (env'', acc'') = doRecContDec fctx env (defs, acc)
              in doLetDecs fctx env'' (restDecs, finalCont, acc'')
              end
        end
    | dec :: restDecs =>
        let val (env', acc') = doDec fctx env (dec, acc)
        in doLetDecs fctx env' (restDecs, finalCont, acc')
        end

  and doDec (fctx: FuncContext) (env: Env) (dec: R.Ty N.dec, acc: W.instr list) :
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
    (exp: R.Ty N.exp, results: (C.Var option * R.Ty) list, acc: W.instr list) :
    Env * W.instr list =
    case results of
      [(SOME v, ty)] =>
        let
          val ubt = tyToUnboxedTy ty
          val localIdx = allocLocal fctx (ubtOptToWasmTy ubt)
          val acc = W.LOCAL_SET localIdx :: doExp fctx env (exp, acc)
          val env' = envWithVar (env, v, localIdx, ubt)
        in
          (env', acc)
        end
    | [(NONE, _)] => (env, doExp fctx env (exp, acc))
    | [] => (env, doExp fctx env (exp, acc))
    | _ =>
        (* Multiple results: evaluate exp (should be a tuple), project each *)
        raise CodeGenError "doValDec: multiple results not yet implemented"

  (* ==================== RecDec (recursive functions) ==================== *)

  and doRecDec (fctx: FuncContext) (env: Env)
    ( decs:
        { name: C.Var
        , contParam: C.CVar
        , params: (C.Var * R.Ty) list
        , body: R.Ty N.stat
        , resultTy: R.Ty
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
            val nOuterFV = List.length freeVars
            val nVarSlots = if selfIsUsed then 1 else 0

            val funcTypeIdx = getClosureFuncTypeIdx ctx nParams
            val closureTypeIdx =
              getMutClosureTypeIdx ctx nOuterFV nVarSlots funcTypeIdx

            (* Create a new context for the inner function *)
            val innerFctx = newFuncContext ctx

            val selfLocal = allocLocal innerFctx anyref
            val paramLocals =
              List.map (fn _ => allocLocal innerFctx anyref) params

            val innerEnv = emptyEnv
            val innerEnv = envWithCont (innerEnv, contParam, RETURN)
            val (revParamUnboxPreamble, innerEnv) =
              ListPair.foldl
                (fn ((v, ty), rawLocalIdx, (revAcc, e)) =>
                   let
                     val ubt = tyToUnboxedTy ty
                     val wasmTy = ubtOptToWasmTy ubt
                   in
                     case ubt of
                       NONE => (revAcc, envWithVar (e, v, rawLocalIdx, NONE))
                     | SOME ubty =>
                         let
                           val ul = allocLocal innerFctx wasmTy
                           val instrs =
                             W.LOCAL_SET ul
                             ::
                             List.revAppend
                               ( emitUnbox (ubty, ctx)
                               , W.LOCAL_GET rawLocalIdx :: revAcc
                               )
                         in
                           (instrs, envWithVar (e, v, ul, ubt))
                         end
                   end) ([], innerEnv) (params, paramLocals)

            (* Extract free vars from closure into reverse preamble.
             * Numeric free vars are stored boxed in the struct; unbox them. *)
            val (revPreamble, innerEnv, _) =
              List.foldl
                (fn (fv, (revAcc, e, fi)) =>
                   let
                     val outerUbt =
                       case TypedSyntax.VIdMap.find (#vars env, fv) of
                         SOME (_, ubt) => ubt
                       | NONE => NONE
                     val outerWasmTy = ubtOptToWasmTy outerUbt
                     val localIdx = allocLocal innerFctx outerWasmTy
                     val baseInstrs =
                       W.STRUCT_GET (closureTypeIdx, fi)
                       ::
                       W.REF_CAST
                         {nullable = false, heaptype = W.TypeIdx closureTypeIdx}
                       :: W.LOCAL_GET selfLocal :: revAcc
                     val fullInstrs =
                       case outerUbt of
                         NONE => W.LOCAL_SET localIdx :: baseInstrs
                       | SOME ubty =>
                           W.LOCAL_SET localIdx
                           :: List.revAppend (emitUnbox (ubty, ctx), baseInstrs)
                   in
                     ( fullInstrs
                     , envWithVar (e, fv, localIdx, outerUbt)
                     , fi + 1
                     )
                   end) (revParamUnboxPreamble, innerEnv, 1) freeVars

            (* If self is used as free var, extract it too (always anyref) *)
            val (revPreamble, innerEnv) =
              if selfIsUsed then
                let
                  val localIdx = allocLocal innerFctx anyref
                  val fi = 1 + List.length freeVars
                in
                  ( W.LOCAL_SET localIdx :: W.STRUCT_GET (closureTypeIdx, fi)
                    ::
                    W.REF_CAST
                      {nullable = false, heaptype = W.TypeIdx closureTypeIdx}
                    :: W.LOCAL_GET selfLocal :: revPreamble
                  , envWithVar (innerEnv, name, localIdx, NONE)
                  )
                end
              else
                (revPreamble, innerEnv)

            val bodyInstrs = List.rev
              (W.UNREACHABLE :: doStat innerFctx innerEnv (body, revPreamble))

            val totalLocals = !(#nextLocalIdx innerFctx)
            val nWasmParams = 1 + nParams
            val extraLocals = totalLocals - nWasmParams
            val allLocalTypes = List.rev (!(#revLocalTypes innerFctx))
            val localTypes = List.drop (allLocalTypes, nWasmParams)

            val funcIdx = allocFuncIdx ctx
            val func: W.func =
              {typeidx = funcTypeIdx, locals = localTypes, body = bodyInstrs}
            val () = #revFuncs ctx := func :: !(#revFuncs ctx)

            (* Create closure at call site.
             * Box numeric free vars before storing in closure struct. *)
            val closureLocal = allocLocal fctx anyref
            val acc = W.REF_FUNC funcIdx :: acc
            val acc =
              List.foldl
                (fn (fv, a) =>
                   case TypedSyntax.VIdMap.find (#vars env, fv) of
                     SOME (idx, SOME ubty) =>
                       List.revAppend
                         (emitBox (ubty, ctx), W.LOCAL_GET idx :: a)
                   | SOME (idx, NONE) => W.LOCAL_GET idx :: a
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

            val env' = envWithVar (env, name, closureLocal, NONE)
          in
            (env', acc)
          end
      | _ =>
          (* Multiple recursive functions: use mutable closures with cross-backpatching.
           *
           * Each function fi gets a closure struct:
           *   field 0      : funcref for fi (CONST)
           *   fields 1..ki : outer free vars of fi (mutable, boxed as needed)
           *   fields ki+1..ki+n : sibling closures f1..fn (mutable, initially null)
           *
           * After all closures are created we backpatch every sibling slot
           * in every closure with the actual closure reference.
           *)
          let
            val allNames = List.map #name decs
            val nameSet =
              List.foldl (fn (v, s) => TypedSyntax.VIdSet.add (s, v))
                TypedSyntax.VIdSet.empty allNames
            val nSiblings = List.length decs

            (* Compute outer free vars for each function
               (body FVs minus own params minus all mutual-rec names) *)
            val decsWithFV =
              List.map
                (fn dec =>
                   let
                     val {params, body, ...} = dec
                     val bodyFV = freeVarsStat (body, TypedSyntax.VIdSet.empty)
                     val paramSet =
                       List.foldl
                         (fn ((v, _), s) => TypedSyntax.VIdSet.add (s, v))
                         TypedSyntax.VIdSet.empty params
                     val freeVars =
                       TypedSyntax.VIdSet.listItems
                         (TypedSyntax.VIdSet.difference
                            ( TypedSyntax.VIdSet.difference (bodyFV, paramSet)
                            , nameSet
                            ))
                   in
                     (dec, freeVars)
                   end) decs

            (* Build each inner Wasm function and collect closure metadata *)
            val funcInfos =
              List.map
                (fn (dec, freeVars) =>
                   let
                     val {name, contParam, params, body, resultTy = _, attr = _} =
                       dec
                     val nParams = List.length params
                     val nOuterFV = List.length freeVars
                     val funcTypeIdx = getClosureFuncTypeIdx ctx nParams
                     val closureTypeIdx =
                       getMutClosureTypeIdx ctx nOuterFV nSiblings funcTypeIdx

                     val innerFctx = newFuncContext ctx
                     val selfLocal = allocLocal innerFctx anyref
                     val paramLocals =
                       List.map (fn _ => allocLocal innerFctx anyref) params

                     val innerEnv = emptyEnv
                     val innerEnv = envWithCont (innerEnv, contParam, RETURN)
                     val innerEnv =
                       ListPair.foldl
                         (fn ((v, _), localIdx, e) =>
                            envWithVar (e, v, localIdx, NONE)) innerEnv
                         (params, paramLocals)

                     (* Extract outer free vars from closure fields 1..nOuterFV *)
                     val (revPreamble, innerEnv, _) =
                       List.foldl
                         (fn (fv, (revAcc, e, fi)) =>
                            let
                              val outerUbt =
                                case TypedSyntax.VIdMap.find (#vars env, fv) of
                                  SOME (_, ubt) => ubt
                                | NONE => NONE
                              val outerWasmTy = ubtOptToWasmTy outerUbt
                              val localIdx = allocLocal innerFctx outerWasmTy
                              val baseInstrs =
                                W.STRUCT_GET (closureTypeIdx, fi)
                                ::
                                W.REF_CAST
                                  { nullable = false
                                  , heaptype = W.TypeIdx closureTypeIdx
                                  } :: W.LOCAL_GET selfLocal :: revAcc
                              val fullInstrs =
                                case outerUbt of
                                  NONE => W.LOCAL_SET localIdx :: baseInstrs
                                | SOME ubty =>
                                    W.LOCAL_SET localIdx
                                    ::
                                    List.revAppend
                                      (emitUnbox (ubty, ctx), baseInstrs)
                            in
                              ( fullInstrs
                              , envWithVar (e, fv, localIdx, outerUbt)
                              , fi + 1
                              )
                            end) ([], innerEnv, 1) freeVars

                     (* Extract sibling closures from fields nOuterFV+1..nOuterFV+nSiblings *)
                     val (revPreamble, innerEnv) =
                       let
                         fun extractSiblings ([], _, revAcc, e) = (revAcc, e)
                           | extractSiblings (sibName :: rest, fi, revAcc, e) =
                               let
                                 val localIdx = allocLocal innerFctx anyref
                                 val instrs =
                                   W.LOCAL_SET localIdx
                                   :: W.STRUCT_GET (closureTypeIdx, fi)
                                   ::
                                   W.REF_CAST
                                     { nullable = false
                                     , heaptype = W.TypeIdx closureTypeIdx
                                     } :: W.LOCAL_GET selfLocal :: revAcc
                               in
                                 extractSiblings
                                   ( rest
                                   , fi + 1
                                   , instrs
                                   , envWithVar (e, sibName, localIdx, NONE)
                                   )
                               end
                       in
                         extractSiblings
                           (allNames, 1 + nOuterFV, revPreamble, innerEnv)
                       end

                     val bodyInstrs = List.rev
                       (W.UNREACHABLE
                        :: doStat innerFctx innerEnv (body, revPreamble))
                     val allLocalTypes = List.rev (!(#revLocalTypes innerFctx))
                     val nWasmParams = 1 + nParams
                     val localTypes = List.drop (allLocalTypes, nWasmParams)

                     val funcIdx = allocFuncIdx ctx
                     val func: W.func =
                       { typeidx = funcTypeIdx
                       , locals = localTypes
                       , body = bodyInstrs
                       }
                     val () = #revFuncs ctx := func :: !(#revFuncs ctx)

                     val closureLocal = allocLocal fctx anyref
                   in
                     { name = name
                     , freeVars = freeVars
                     , nOuterFV = nOuterFV
                     , closureTypeIdx = closureTypeIdx
                     , funcIdx = funcIdx
                     , closureLocal = closureLocal
                     }
                   end) decsWithFV

            (* Create all closure structs (sibling slots initially null) *)
            val acc =
              List.foldl
                (fn ({freeVars, closureTypeIdx, funcIdx, closureLocal, ...}, a) =>
                   let
                     val a = W.REF_FUNC funcIdx :: a
                     val a =
                       List.foldl
                         (fn (fv, a') =>
                            case TypedSyntax.VIdMap.find (#vars env, fv) of
                              SOME (idx, SOME ubty) =>
                                List.revAppend
                                  (emitBox (ubty, ctx), W.LOCAL_GET idx :: a')
                            | SOME (idx, NONE) => W.LOCAL_GET idx :: a'
                            | NONE =>
                                raise CodeGenError
                                  "doRecDec: free var not in scope") a freeVars
                     (* Null placeholders for sibling closure slots *)
                     val a =
                       List.foldl
                         (fn (_, a') =>
                            W.REF_NULL (W.AbsHeapType W.HEAP_NONE) :: a') a
                         (List.tabulate (nSiblings, fn i => i))
                     val a = W.STRUCT_NEW closureTypeIdx :: a
                     val a = W.LOCAL_SET closureLocal :: a
                   in
                     a
                   end) acc funcInfos

            (* Backpatch: for each closure fi, set its nSiblings sibling fields
               to point to each fj's closure *)
            val acc =
              List.foldl
                (fn ({nOuterFV, closureTypeIdx, closureLocal, ...}, a) =>
                   let
                     fun backpatch ([], _, a') = a'
                       | backpatch
                           ({closureLocal = sibLocal, ...} :: rest, j, a') =
                           let
                             val fieldIdx = 1 + nOuterFV + j
                             val a' =
                               W.STRUCT_SET (closureTypeIdx, fieldIdx)
                               :: W.LOCAL_GET sibLocal
                               ::
                               W.REF_CAST
                                 { nullable = false
                                 , heaptype = W.TypeIdx closureTypeIdx
                                 } :: W.LOCAL_GET closureLocal :: a'
                           in
                             backpatch (rest, j + 1, a')
                           end
                   in
                     backpatch (funcInfos, 0, a)
                   end) acc funcInfos

            (* Add all names to the outer environment *)
            val env' =
              List.foldl
                (fn ({name, closureLocal, ...}, e) =>
                   envWithVar (e, name, closureLocal, NONE)) env funcInfos
          in
            (env', acc)
          end
    end

  (* ==================== ContDec (continuation) ==================== *)

  and doContDec (fctx: FuncContext) (env: Env)
    ( name: C.CVar
    , params: (C.Var option * R.Ty) list
    , body: R.Ty N.stat
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
    ( defs: (C.CVar * (C.Var option * R.Ty) list * R.Ty N.stat) list
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
    ( primOp: F.PrimOp
    , tyargs: R.Ty list
    , args: R.Ty N.exp list
    , acc: W.instr list
    ) : W.instr list =
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
      | (F.String7ConstOp s, _, _) => doStringConst (fctx, s, acc)
      | (F.String8ConstOp s, _, _) => doStringConst (fctx, s, acc)
      | (F.String16ConstOp _, _, _) =>
          raise CodeGenError "doPrimOp: String16ConstOp not yet implemented"
      | (F.String32ConstOp _, _, _) =>
          raise CodeGenError "doPrimOp: String32ConstOp not yet implemented"

      (* ---- Box/Unbox ---- *)
      (* Nesting a boxed value into an unboxed slot and vice versa (a common
         result of inlining a continuation, or of reading a struct field and
         storing it into another one) leaves a box/unbox round trip; emit the
         inner expression instead.  Boxed scalars have no observable identity,
         so dropping the reallocation is safe. *)
      | ( F.BoxOp ubt
        , _
        , [N.PrimOp {primOp = F.UnboxOp ubt', tyargs = _, args = [inner]}]
        ) =>
          if ubt = ubt' then
            doExp fctx env (inner, acc)
          else
            List.revAppend (emitBox (ubt, ctx), List.revAppend
              (emitUnbox (ubt', ctx), doExp fctx env (inner, acc)))
      | (F.BoxOp ubt, _, [arg]) =>
          List.revAppend (emitBox (ubt, ctx), doExp fctx env (arg, acc))
      | ( F.UnboxOp ubt
        , _
        , [N.PrimOp {primOp = F.BoxOp ubt', tyargs = _, args = [inner]}]
        ) =>
          if ubt = ubt' then
            doExp fctx env (inner, acc)
          else
            List.revAppend (emitUnbox (ubt, ctx), List.revAppend
              (emitBox (ubt', ctx), doExp fctx env (inner, acc)))
      | (F.UnboxOp ubt, _, [arg]) =>
          List.revAppend (emitUnbox (ubt, ctx), doExp fctx env (arg, acc))

      (* ---- Data type operations ---- *)
      | (F.ConstructValOp info, _, []) =>
          (case #representation info of
             Syntax.REP_BOXED =>
               (* No-payload boxed constructor: TaggedData{tag=idx, payload=null} *)
               let
                 val taggedDataIdx = #taggedDataTypeIdx ctx
                 val tagIdx = constructorTagIndex info
                 val acc = W.I32_CONST (Int32.fromInt tagIdx) :: acc
                 val acc = W.REF_NULL (W.AbsHeapType W.HEAP_NONE) :: acc
               in
                 W.STRUCT_NEW taggedDataIdx :: acc
               end
           | Syntax.REP_ENUM =>
               (* Enum constructor: encode as i31ref with integer tag index *)
               let val tagIdx = constructorTagIndex info
               in W.REF_I31 :: W.I32_CONST (Int32.fromInt tagIdx) :: acc
               end
           | Syntax.REP_UNIT => W.REF_NULL (W.AbsHeapType W.HEAP_NONE) :: acc
           | Syntax.REP_BOOL =>
               W.I32_CONST 0 :: acc (* Should not happen normally *)
           | _ =>
               raise CodeGenError
                 "doPrimOp: unexpected representation for ConstructValOp")
      | (F.ConstructValWithPayloadOp info, [_, _], [payload]) =>
          (* The payload is boxed by NSyntaxFromCpsWasm. *)
          (case #representation info of
             Syntax.REP_BOXED =>
               (* Boxed constructor with payload: TaggedData{tag=idx, payload=boxed} *)
               let
                 val taggedDataIdx = #taggedDataTypeIdx ctx
                 val tagIdx = constructorTagIndex info
                 val acc = W.I32_CONST (Int32.fromInt tagIdx) :: acc
                 val acc = doExp fctx env (payload, acc)
               in
                 W.STRUCT_NEW taggedDataIdx :: acc
               end
           | Syntax.REP_ALIAS =>
               (* REP_ALIAS: the result is the (boxed) payload itself *)
               doExp fctx env (payload, acc)
           | Syntax.REP_LIST =>
               doExp fctx env
                 (payload, acc) (* payload is already a cons cell from the IR *)
           | _ =>
               raise CodeGenError
                 "doPrimOp: unexpected representation for ConstructValWithPayloadOp")
      | (F.DataPayloadOp info, [_, _], [arg]) =>
          (* The payload is stored boxed; NSyntaxFromCpsWasm unboxes the
             result where the payload type is unboxed. *)
          (case #representation info of
             Syntax.REP_BOXED =>
               (* Extract payload from TaggedData: cast + struct.get(1) *)
               let
                 val taggedDataIdx = #taggedDataTypeIdx ctx
               in
                 W.STRUCT_GET (taggedDataIdx, 1)
                 ::
                 W.REF_CAST
                   {nullable = false, heaptype = W.TypeIdx taggedDataIdx}
                 :: doExp fctx env (arg, acc)
               end
           | Syntax.REP_ALIAS =>
               (* REP_ALIAS: the value is the payload (see ConstructValWithPayloadOp) *)
               doExp fctx env (arg, acc)
           | _ =>
               raise CodeGenError
                 "doPrimOp: unexpected representation for DataPayloadOp")
      | (F.DataTagAsInt32Op info, _, [arg]) =>
          (case #representation info of
             Syntax.REP_ENUM =>
               (* Enum: i31ref encodes tag index; cast to i31 + i31.get_u *)
               W.I31_GET W.U
               :: W.REF_CAST {nullable = false, heaptype = W.AbsHeapType W.I31}
               :: doExp fctx env (arg, acc)
           | Syntax.REP_BOXED =>
               (* Boxed: cast to $TaggedData + struct.get(0) to get i32 tag *)
               let
                 val taggedDataIdx = #taggedDataTypeIdx ctx
               in
                 W.STRUCT_GET (taggedDataIdx, 0)
                 ::
                 W.REF_CAST
                   {nullable = false, heaptype = W.TypeIdx taggedDataIdx}
                 :: doExp fctx env (arg, acc)
               end
           | _ =>
               raise CodeGenError
                 "doPrimOp: unexpected representation for DataTagAsInt32Op")
      | (F.DataTagAsStringOp _, _, [_]) =>
          raise CodeGenError "DataTagAsStringOp not supported for Wasm target"
      | (F.DataTagAsString16Op _, _, [_]) =>
          raise CodeGenError "DataTagAsString16Op not supported for Wasm target"
      | (F.ExnPayloadOp, [_], [arg]) =>
          (* Extract payload (field 1) from $SmlExn struct.  The payload is
             stored as anyref; NSyntaxFromCpsWasm unboxes the result where the
             payload type is unboxed.
             NOTE: acc is a reversed accumulator; instructions are prepended in
             reverse execution order. The desired execution order is:
               arg → ref.cast($SmlExn) → struct.get(5,1) *)
          let
            val smlExnTypeIdx = #smlExnTypeIdx ctx
          in
            W.STRUCT_GET (smlExnTypeIdx, 1)
            :: W.REF_CAST {nullable = false, heaptype = W.TypeIdx smlExnTypeIdx}
            :: doExp fctx env (arg, acc)
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
      | (F.ConstructExnWithPayloadOp, [_], [tag, payload]) =>
          (* Build $SmlExn struct with tag and payload (anyref);
             the payload is boxed by NSyntaxFromCpsWasm. *)
          let
            val smlExnTypeIdx = #smlExnTypeIdx ctx
            val acc = doExp fctx env (tag, acc)
            val acc = doExp fctx env (payload, acc)
          in
            W.STRUCT_NEW smlExnTypeIdx :: acc
          end
      | (F.RaiseOp _, _, _) =>
          raise CodeGenError "doPrimOp: RaiseOp should not appear in NSyntax"

      (* ---- List operations ---- *)
      | (F.ListOp, _, []) => W.REF_NULL (W.AbsHeapType W.HEAP_NONE) :: acc
      | (F.ListOp, _, elems) =>
          (* Build a cons-cell chain from left to right.
             The elements are boxed by NSyntaxFromCpsWasm. *)
          let
            val tupleIdx = getTupleTypeIdx ctx 2
            val acc =
              List.foldl (fn (elem, a) => doExp fctx env (elem, a)) acc elems
            val acc = W.REF_NULL (W.AbsHeapType W.HEAP_NONE) :: acc
            val acc =
              List.foldl (fn (_, a) => W.STRUCT_NEW tupleIdx :: a) acc elems
          in
            acc
          end

      (* ---- Vector operations ---- *)
      | (F.VectorOp, _, args) =>
          let
            val arrayTypeIdx = #arrayTypeIdx ctx
            val n = List.length args
            val argsAcc =
              List.foldl (fn (a, prev) => doExp fctx env (a, prev)) acc args
          in
            W.ARRAY_NEW_FIXED (arrayTypeIdx, n) :: argsAcc
          end

      (* ---- Vector/list/ref/array element access ----
         The element type of these containers is always boxed (anyref), so no
         boxing happens here: NSyntaxFromCpsWasm has boxed the operands and
         unboxes the results where the element type is unboxed. *)
      | (F.PrimCall (Primitives.Unsafe_Vector_sub _), [_], [v, idx]) =>
          let
            val arrayTypeIdx = #arrayTypeIdx ctx
            val castV =
              W.REF_CAST {nullable = false, heaptype = W.TypeIdx arrayTypeIdx}
          in
            W.ARRAY_GET arrayTypeIdx
            :: doExp fctx env (idx, castV :: doExp fctx env (v, acc))
          end

      | (F.PrimCall Primitives.List_unsafeTail, [_], [lst]) =>
          let
            val tupleIdx = getTupleTypeIdx ctx 2
          in
            W.STRUCT_GET (tupleIdx, 1)
            :: W.REF_CAST {nullable = false, heaptype = W.TypeIdx tupleIdx}
            :: doExp fctx env (lst, acc)
          end

      (* ---- Ref cells ---- *)
      | (F.PrimCall Primitives.Ref_ref, [_], [arg]) =>
          let val refTypeIdx = #refTypeIdx ctx
          in W.STRUCT_NEW refTypeIdx :: doExp fctx env (arg, acc)
          end
      | (F.PrimCall Primitives.Ref_set, [_], [r, v]) =>
          let
            val refTypeIdx = #refTypeIdx ctx
            val castRef =
              W.REF_CAST {nullable = false, heaptype = W.TypeIdx refTypeIdx}
            val accAfterRef = castRef :: doExp fctx env (r, acc)
          in
            W.STRUCT_SET (refTypeIdx, 0) :: doExp fctx env (v, accAfterRef)
          end
      | (F.PrimCall Primitives.Ref_read, [_], [r]) =>
          let
            val refTypeIdx = #refTypeIdx ctx
            val castRef =
              W.REF_CAST {nullable = false, heaptype = W.TypeIdx refTypeIdx}
          in
            W.STRUCT_GET (refTypeIdx, 0) :: castRef :: doExp fctx env (r, acc)
          end

      (* ---- Array operations ---- *)
      | (F.PrimCall (Primitives.Array_array _), [_], [n, init]) =>
          let
            val arrayTypeIdx = #arrayTypeIdx ctx
          in
            W.ARRAY_NEW arrayTypeIdx
            :: doExp fctx env (n, doExp fctx env (init, acc))
          end

      | (F.PrimCall (Primitives.Array_allocUninitialized _), [_], [n]) =>
          W.ARRAY_NEW_DEFAULT (#arrayTypeIdx ctx) :: doExp fctx env (n, acc)

      | (F.PrimCall (Primitives.Unsafe_Array_sub _), [_], [arr, idx]) =>
          let
            val arrayTypeIdx = #arrayTypeIdx ctx
            val castArr =
              W.REF_CAST {nullable = false, heaptype = W.TypeIdx arrayTypeIdx}
          in
            W.ARRAY_GET arrayTypeIdx
            :: doExp fctx env (idx, castArr :: doExp fctx env (arr, acc))
          end

      | (F.PrimCall (Primitives.Unsafe_Array_update _), [_], [arr, idx, v]) =>
          let
            val arrayTypeIdx = #arrayTypeIdx ctx
            val castArr =
              W.REF_CAST {nullable = false, heaptype = W.TypeIdx arrayTypeIdx}
            val arrAcc = castArr :: doExp fctx env (arr, acc)
            val idxAcc = doExp fctx env (idx, arrAcc)
          in
            W.ARRAY_SET arrayTypeIdx :: doExp fctx env (v, idxAcc)
          end

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

      (* ---- Wasm function imports ---- *)
      | (F.ForeignCallOp (modName, fnName, _), _, args) =>
          let
            val ctx = #ctx fctx
            val funcIdx = lookupForeignImport ctx (modName, fnName)
            (* The argument and result types of a Wasm import are primitive
               (see foreignTyToWasmType), so the arguments are passed as they
               are. *)
            val acc =
              List.foldl (fn (arg, a) => doExp fctx env (arg, a)) acc args
          in
            W.CALL funcIdx :: acc
          end

      | _ => raise CodeGenError "doPrimOp: unhandled primOp"
    end

  (* ==================== PrimCall ==================== *)

  and doPrimCall (fctx: FuncContext) (env: Env)
    (prim: Primitives.PrimOp, args: R.Ty N.exp list, acc: W.instr list) :
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
      | Primitives.Int_PLUS_wrapping Primitives.I32 =>
          doBinary [W.I32_BINOP W.ADD] args
      | Primitives.Int_MINUS_wrapping Primitives.I32 =>
          doBinary [W.I32_BINOP W.SUB] args
      | Primitives.Int_TIMES_wrapping Primitives.I32 =>
          doBinary [W.I32_BINOP W.MUL] args
      | Primitives.Int_quot_unchecked Primitives.I32 =>
          (* division by zero, overflow: trap *)
          doBinary [W.I32_BINOP W.DIV_S] args
      | Primitives.Int_rem_unchecked Primitives.I32 =>
          (* division by zero: trap *)
          doBinary [W.I32_BINOP W.REM_S] args
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

      (* ---- Int32 comparison ---- *)
      | Primitives.Int_EQUAL Primitives.I32 => doBinary [W.I32_RELOP W.IEQ] args
      | Primitives.Int_LT Primitives.I32 => doBinary [W.I32_RELOP W.LT_S] args
      | Primitives.Int_LE Primitives.I32 => doBinary [W.I32_RELOP W.LE_S] args
      | Primitives.Int_GT Primitives.I32 => doBinary [W.I32_RELOP W.GT_S] args
      | Primitives.Int_GE Primitives.I32 => doBinary [W.I32_RELOP W.GE_S] args

      (* ---- Int64 arithmetic ---- *)
      | Primitives.Int_PLUS_wrapping Primitives.I64 =>
          doBinary [W.I64_BINOP W.ADD] args
      | Primitives.Int_MINUS_wrapping Primitives.I64 =>
          doBinary [W.I64_BINOP W.SUB] args
      | Primitives.Int_TIMES_wrapping Primitives.I64 =>
          doBinary [W.I64_BINOP W.MUL] args
      | Primitives.Int_quot_unchecked Primitives.I64 =>
          (* division by zero, overflow: trap *)
          doBinary [W.I64_BINOP W.DIV_S] args
      | Primitives.Int_rem_unchecked Primitives.I64 =>
          (* division by zero: trap *)
          doBinary [W.I64_BINOP W.REM_S] args
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
      | Primitives.Word_div_unchecked Primitives.W32 =>
          (* division by zero: trap *)
          doBinary [W.I32_BINOP W.DIV_U] args
      | Primitives.Word_mod_unchecked Primitives.W32 =>
          (* division by zero: trap *)
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
      | Primitives.Word_LSHIFT_unchecked (Primitives.W32, Primitives.W32) =>
          doBinary [W.I32_BINOP W.SHL] args
      | Primitives.Word_RSHIFT_unchecked (Primitives.W32, Primitives.W32) =>
          doBinary [W.I32_BINOP W.SHR_U] args
      | Primitives.Word_ARSHIFT_unchecked (Primitives.W32, Primitives.W32) =>
          doBinary [W.I32_BINOP W.SHR_S] args

      (* ---- Word64 arithmetic ---- *)
      | Primitives.Word_PLUS Primitives.W64 => doBinary [W.I64_BINOP W.ADD] args
      | Primitives.Word_MINUS Primitives.W64 =>
          doBinary [W.I64_BINOP W.SUB] args
      | Primitives.Word_TIMES Primitives.W64 =>
          doBinary [W.I64_BINOP W.MUL] args
      | Primitives.Word_div_unchecked Primitives.W64 =>
          (* division by zero: trap *)
          doBinary [W.I64_BINOP W.DIV_U] args
      | Primitives.Word_mod_unchecked Primitives.W64 =>
          (* division by zero: trap *)
          doBinary [W.I64_BINOP W.REM_U] args
      | Primitives.Word_TILDE Primitives.W64 =>
          (case args of
             [arg] =>
               W.I64_BINOP W.SUB :: doExp fctx env (arg, W.I64_CONST 0 :: acc)
           | _ => raise CodeGenError "Word_TILDE: expected 1 arg")

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
      | Primitives.Word_LSHIFT_unchecked (Primitives.W64, Primitives.W64) =>
          doBinary [W.I64_BINOP W.SHL] args
      | Primitives.Word_RSHIFT_unchecked (Primitives.W64, Primitives.W64) =>
          doBinary [W.I64_BINOP W.SHR_U] args
      | Primitives.Word_ARSHIFT_unchecked (Primitives.W64, Primitives.W64) =>
          doBinary [W.I64_BINOP W.SHR_S] args

      (* ---- Word conversion ---- *)
      | Primitives.Word_toInt_unchecked (Primitives.W32, Primitives.I32) =>
          doUnary [] args (* no-op *)
      | Primitives.Word_toInt_unchecked (Primitives.W32, Primitives.I64) =>
          doUnary [W.I64_EXTEND_I32 W.U] args
      | Primitives.Word_toInt_unchecked (Primitives.W64, Primitives.I32) =>
          doUnary [W.I32_WRAP_I64] args
      | Primitives.Word_toInt_unchecked (Primitives.W64, Primitives.I64) =>
          doUnary [] args (* no-op *)
      | Primitives.Word_toIntX_unchecked (Primitives.W32, Primitives.I32) =>
          doUnary [] args (* no-op *)
      | Primitives.Word_toIntX_unchecked (Primitives.W32, Primitives.I64) =>
          doUnary [W.I64_EXTEND_I32 W.S] args
      | Primitives.Word_toIntX_unchecked (Primitives.W64, Primitives.I32) =>
          doUnary [W.I32_WRAP_I64] args
      | Primitives.Word_toIntX_unchecked (Primitives.W64, Primitives.I64) =>
          doUnary [] args (* no-op *)
      | Primitives.Word_fromInt (Primitives.W32, Primitives.I32) =>
          doUnary [] args (* no-op *)
      | Primitives.Word_fromInt (Primitives.W32, Primitives.I64) =>
          doUnary [W.I32_WRAP_I64] args
      | Primitives.Word_fromInt (Primitives.W64, Primitives.I32) =>
          doUnary [W.I64_EXTEND_I32 W.S] args
      | Primitives.Word_fromInt (Primitives.W64, Primitives.I64) =>
          doUnary [] args (* no-op *)
      | Primitives.Word_toWord (Primitives.W32, Primitives.W32) =>
          doUnary [] args (* no-op *)
      | Primitives.Word_toWord (Primitives.W32, Primitives.W64) =>
          doUnary [W.I64_EXTEND_I32 W.U] args
      | Primitives.Word_toWord (Primitives.W64, Primitives.W32) =>
          doUnary [W.I32_WRAP_I64] args
      | Primitives.Word_toWord (Primitives.W64, Primitives.W64) =>
          doUnary [] args (* no-op *)

      (* ---- Real (F64) arithmetic ---- *)
      | Primitives.Real_posInf => W.F64_CONST Real.posInf :: acc
      | Primitives.Real_negInf => W.F64_CONST Real.negInf :: acc
      | Primitives.Real_PLUS => doBinary [W.F64_BINOP W.FADD] args
      | Primitives.Real_MINUS => doBinary [W.F64_BINOP W.FSUB] args
      | Primitives.Real_TIMES => doBinary [W.F64_BINOP W.FMUL] args
      | Primitives.Real_DIVIDE => doBinary [W.F64_BINOP W.FDIV] args
      | Primitives.Real_TILDE => doUnary [W.F64_UNOP W.NEG] args
      | Primitives.Real_abs => doUnary [W.F64_UNOP W.ABS] args
      | Primitives.Real_Math_sqrt => doUnary [W.F64_UNOP W.SQRT] args
      | Primitives.Real_realFloor => doUnary [W.F64_UNOP W.FLOOR] args
      | Primitives.Real_realCeil => doUnary [W.F64_UNOP W.CEIL] args
      | Primitives.Real_realTrunc => doUnary [W.F64_UNOP W.TRUNC] args
      | Primitives.Real_realRound => doUnary [W.F64_UNOP W.NEAREST] args
      | Primitives.Real_minimum => doBinary [W.F64_BINOP W.FMIN] args
      | Primitives.Real_maximum => doBinary [W.F64_BINOP W.FMAX] args
      | Primitives.Real_copySign => doBinary [W.F64_BINOP W.FCOPYSIGN] args
      | Primitives.Real_fromInt Primitives.I32 =>
          doUnary [W.F64_CONVERT_I32 W.S] args
      | Primitives.Real_fromInt Primitives.I64 =>
          doUnary [W.F64_CONVERT_I64 W.S] args
      | Primitives.Real_fromWord Primitives.W32 =>
          doUnary [W.F64_CONVERT_I32 W.U] args
      | Primitives.Real_fromWord Primitives.W64 =>
          doUnary [W.F64_CONVERT_I64 W.U] args
      | Primitives.Real_trunc_unchecked Primitives.I32 =>
          doUnary [W.I32_TRUNC_F64 W.S] args
      | Primitives.Real_trunc_unchecked Primitives.I64 =>
          doUnary [W.I64_TRUNC_F64 W.S] args
      | Primitives.Real_truncAsWord_unchecked Primitives.W32 =>
          doUnary [W.I32_TRUNC_F64 W.U] args
      | Primitives.Real_truncAsWord_unchecked Primitives.W64 =>
          doUnary [W.I64_TRUNC_F64 W.U] args
      | Primitives.Real_reinterpretAsWord64 =>
          doUnary [W.F64_REINTERPRET_I64] args
      | Primitives.Real_reinterpretFromWord64 =>
          doUnary [W.I64_REINTERPRET_F64] args

      (* ---- Real comparison ---- *)
      | Primitives.Real_EQUAL => doBinary [W.F64_RELOP W.FEQ] args
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

      (* ---- String operations ---- *)

      | Primitives.String_size _ =>
          (case args of
             [str] =>
               let
                 val strTypeIdx = #stringTypeIdx ctx
               in
                 W.ARRAY_LEN
                 ::
                 W.REF_CAST {nullable = false, heaptype = W.TypeIdx strTypeIdx}
                 :: doExp fctx env (str, acc)
               end
           | _ => raise CodeGenError "String_size: expected 1 arg")

      | Primitives.String_str =>
          (case args of
             [charArg] =>
               let
                 val strTypeIdx = #stringTypeIdx ctx
                 val strRefNullTy =
                   W.RefType {nullable = true, heaptype = W.TypeIdx strTypeIdx}
                 val tmpLocal = allocLocal fctx strRefNullTy
               in
                 W.LOCAL_GET tmpLocal :: W.ARRAY_SET strTypeIdx
                 ::
                 doExp fctx env
                   ( charArg
                   , W.I32_CONST 0 :: W.LOCAL_TEE tmpLocal
                     :: W.ARRAY_NEW_DEFAULT strTypeIdx :: W.I32_CONST 1 :: acc
                   )
               end
           | _ => raise CodeGenError "String_str: expected 1 arg")

      | Primitives.String_fromString7 => doUnary [] args

      (* Low-level string/CharArray primitives for SML-side implementation *)

      | Primitives.Unsafe_CharVector_sub Primitives.I32 =>
          (case args of
             [str, idx] =>
               let
                 val strTypeIdx = #stringTypeIdx ctx
               in
                 W.ARRAY_GET_U strTypeIdx
                 ::
                 doExp fctx env
                   ( idx
                   , W.REF_CAST
                       {nullable = false, heaptype = W.TypeIdx strTypeIdx}
                     :: doExp fctx env (str, acc)
                   )
               end
           | _ => raise CodeGenError "Unsafe_CharVector_sub: expected 2 args")

      | Primitives.CharArray_alloc Primitives.I32 =>
          doUnary [W.ARRAY_NEW_DEFAULT (#stringTypeIdx ctx)] args

      | Primitives.Unsafe_CharArray_sub Primitives.I32 =>
          (case args of
             [str, idx] =>
               let
                 val strTypeIdx = #stringTypeIdx ctx
               in
                 W.ARRAY_GET_U strTypeIdx
                 ::
                 doExp fctx env
                   ( idx
                   , W.REF_CAST
                       {nullable = false, heaptype = W.TypeIdx strTypeIdx}
                     :: doExp fctx env (str, acc)
                   )
               end
           | _ => raise CodeGenError "Unsafe_CharVector_sub: expected 2 args")

      | Primitives.Unsafe_CharArray_update Primitives.I32 =>
          (case args of
             [arr, idx, charArg] =>
               let
                 val strTypeIdx = #stringTypeIdx ctx
               in
                 W.ARRAY_SET strTypeIdx
                 ::
                 doExp fctx env (charArg, doExp fctx env
                   ( idx
                   , W.REF_CAST
                       {nullable = false, heaptype = W.TypeIdx strTypeIdx}
                     :: doExp fctx env (arr, acc)
                   ))
               end
           | _ => raise CodeGenError "Unsafe_CharArray_update: expected 3 args")

      | Primitives.CharArray_copy Primitives.I32 =>
          (case args of
             [dst, dstOff, src, srcOff, len] =>
               let
                 val strTypeIdx = #stringTypeIdx ctx
                 val castStr =
                   W.REF_CAST
                     {nullable = false, heaptype = W.TypeIdx strTypeIdx}
               in
                 W.ARRAY_COPY (strTypeIdx, strTypeIdx)
                 ::
                 doExp fctx env (len, doExp fctx env
                   ( srcOff
                   , castStr
                     ::
                     doExp fctx env (src, doExp fctx env
                       (dstOff, castStr :: doExp fctx env (dst, acc)))
                   ))
               end
           | _ => raise CodeGenError "CharArray_copy: expected 5 args")

      | Primitives.CharArray_unsafeFreeze => doUnary [] args

      | Primitives.String_copyBytes Primitives.I32 =>
          (case args of
             [dst, dstOff, src, srcOff, len] =>
               let
                 val strTypeIdx = #stringTypeIdx ctx
                 val castStr =
                   W.REF_CAST
                     {nullable = false, heaptype = W.TypeIdx strTypeIdx}
               in
                 W.ARRAY_COPY (strTypeIdx, strTypeIdx)
                 ::
                 doExp fctx env (len, doExp fctx env
                   ( srcOff
                   , castStr
                     ::
                     doExp fctx env (src, doExp fctx env
                       (dstOff, castStr :: doExp fctx env (dst, acc)))
                   ))
               end
           | _ => raise CodeGenError "String_copyBytes: expected 5 args")

      (* ---- Array equality (reference equality) ---- *)
      | Primitives.Array_EQUAL =>
          (case args of
             [a, b] =>
               let
                 val castEq =
                   W.REF_CAST {nullable = true, heaptype = W.AbsHeapType W.EQ}
               in
                 W.REF_EQ :: castEq
                 :: doExp fctx env (b, castEq :: doExp fctx env (a, acc))
               end
           | _ => raise CodeGenError "Array_EQUAL: expected 2 args")

      (* ---- Ref cells ---- *)
      (* Ref_ref / Ref_set / Ref_read are handled in doPrimOp with type info. *)
      | Primitives.Ref_EQUAL =>
          (case args of
             [a, b] =>
               let
                 val castEq =
                   W.REF_CAST {nullable = true, heaptype = W.AbsHeapType W.EQ}
               in
                 W.REF_EQ :: castEq
                 :: doExp fctx env (b, castEq :: doExp fctx env (a, acc))
               end
           | _ => raise CodeGenError "Ref_EQUAL: expected 2 args")

      (* ---- List operations ---- *)
      | Primitives.List_cons =>
          (case args of
             [hd, tl] =>
               let
                 val tupleIdx = getTupleTypeIdx ctx 2
                 (* The head is boxed by NSyntaxFromCpsWasm *)
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
               let
                 val tupleIdx = getTupleTypeIdx ctx 2
               in
                 W.STRUCT_GET (tupleIdx, 0)
                 :: W.REF_CAST {nullable = false, heaptype = W.TypeIdx tupleIdx}
                 :: doExp fctx env (lst, acc)
               end
           | _ => raise CodeGenError "List_unsafeHead: expected 1 arg")
      | Primitives.List_unsafeTail =>
          (case args of
             [lst] =>
               let
                 val tupleIdx = getTupleTypeIdx ctx 2
               in
                 W.STRUCT_GET (tupleIdx, 1)
                 :: W.REF_CAST {nullable = false, heaptype = W.TypeIdx tupleIdx}
                 :: doExp fctx env (lst, acc)
               end
           | _ => raise CodeGenError "List_unsafeTail: expected 1 arg")

      (* ---- Array/Vector operations ---- *)
      | Primitives.Array_length _ =>
          (case args of
             [arr] =>
               W.ARRAY_LEN
               ::
               W.REF_CAST
                 {nullable = false, heaptype = W.TypeIdx (#arrayTypeIdx ctx)}
               :: doExp fctx env (arr, acc)
           | _ => raise CodeGenError "Array_length: expected 1 arg")

      | Primitives.Vector_length _ =>
          (case args of
             [v] =>
               W.ARRAY_LEN
               ::
               W.REF_CAST
                 {nullable = false, heaptype = W.TypeIdx (#arrayTypeIdx ctx)}
               :: doExp fctx env (v, acc)
           | _ => raise CodeGenError "Vector_length: expected 1 arg")

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

      (* ---- Exception operations ---- *)
      | Primitives.Exception_instanceof =>
          (* Check if exception e matches tag: cast e to $SmlExn, get tag field, ref.eq with tag arg.
             Both operands of ref.eq must be eqref, so cast each from anyref. *)
          (case args of
             [e, tag] =>
               let
                 val smlExnTypeIdx = #smlExnTypeIdx ctx
                 val castEq =
                   W.REF_CAST {nullable = true, heaptype = W.AbsHeapType W.EQ}
               in
                 W.REF_EQ :: castEq :: W.STRUCT_GET (smlExnTypeIdx, 0)
                 ::
                 W.REF_CAST
                   {nullable = false, heaptype = W.TypeIdx smlExnTypeIdx}
                 :: doExp fctx env (e, castEq :: doExp fctx env (tag, acc))
               end
           | _ => raise CodeGenError "Exception_instanceof: expected 2 args")

      (* ---- General exn name ---- *)
      | Primitives.General_exnName =>
          (* Cast e to $SmlExn, get the tag, cast it to $ExnTag and read the
             name field. *)
          (case args of
             [e] =>
               let
                 val smlExnTypeIdx = #smlExnTypeIdx ctx
                 val exnTagTypeIdx = #exnTagTypeIdx ctx
               in
                 W.STRUCT_GET (exnTagTypeIdx, 0)
                 ::
                 W.REF_CAST
                   {nullable = false, heaptype = W.TypeIdx exnTagTypeIdx}
                 :: W.STRUCT_GET (smlExnTypeIdx, 0)
                 ::
                 W.REF_CAST
                   {nullable = false, heaptype = W.TypeIdx smlExnTypeIdx}
                 :: doExp fctx env (e, acc)
               end
           | _ => raise CodeGenError "General_exnName: expected 1 arg")

      (* ---- mkFn / call helpers ---- *)
      | Primitives.mkFn2 => raise CodeGenError "mkFn2: not yet implemented"
      | Primitives.mkFn3 => raise CodeGenError "mkFn3: not yet implemented"
      | Primitives.call2 => raise CodeGenError "call2: not yet implemented"
      | Primitives.call3 => raise CodeGenError "call3: not yet implemented"

      (* Wasm linear memory operations *)
      | Primitives.Wasm_memory_loadWord8 Primitives.W32 =>
          ( #needsLinearMemory (#ctx fctx) := true
          ; case args of
              [ptr] =>
                W.I32_LOAD8_U {align = 0, offset = 0}
                :: doExp fctx env (ptr, acc)
            | _ => raise CodeGenError "Wasm_memory_loadWord8: expected 1 arg"
          )
      | Primitives.Wasm_memory_loadWord16 Primitives.W32 =>
          ( #needsLinearMemory (#ctx fctx) := true
          ; case args of
              [ptr] =>
                W.I32_LOAD16_U {align = 1, offset = 0}
                :: doExp fctx env (ptr, acc)
            | _ => raise CodeGenError "Wasm_memory_loadWord16: expected 1 arg"
          )
      | Primitives.Wasm_memory_loadWord32 Primitives.W32 =>
          ( #needsLinearMemory (#ctx fctx) := true
          ; case args of
              [ptr] =>
                W.I32_LOAD {align = 2, offset = 0} :: doExp fctx env (ptr, acc)
            | _ => raise CodeGenError "Wasm_memory_loadWord32: expected 1 arg"
          )
      | Primitives.Wasm_memory_loadWord64 =>
          ( #needsLinearMemory (#ctx fctx) := true
          ; case args of
              [ptr] =>
                W.I64_LOAD {align = 3, offset = 0} :: doExp fctx env (ptr, acc)
            | _ => raise CodeGenError "Wasm_memory_loadWord64: expected 1 arg"
          )
      | Primitives.Wasm_memory_loadChar8 =>
          ( #needsLinearMemory (#ctx fctx) := true
          ; case args of
              [ptr] =>
                W.I32_LOAD8_U {align = 0, offset = 0}
                :: doExp fctx env (ptr, acc)
            | _ => raise CodeGenError "Wasm_memory_loadChar8: expected 1 arg"
          )
      | Primitives.Wasm_memory_storeWord8 Primitives.W32 =>
          ( #needsLinearMemory (#ctx fctx) := true
          ; case args of
              [ptr, v] =>
                W.I32_STORE8 {align = 0, offset = 0}
                :: doExp fctx env (v, doExp fctx env (ptr, acc))
            | _ => raise CodeGenError "Wasm_memory_storeWord8: expected 2 args"
          )
      | Primitives.Wasm_memory_storeWord16 Primitives.W32 =>
          ( #needsLinearMemory (#ctx fctx) := true
          ; case args of
              [ptr, v] =>
                W.I32_STORE16 {align = 1, offset = 0}
                :: doExp fctx env (v, doExp fctx env (ptr, acc))
            | _ => raise CodeGenError "Wasm_memory_storeWord16: expected 2 args"
          )
      | Primitives.Wasm_memory_storeWord32 Primitives.W32 =>
          ( #needsLinearMemory (#ctx fctx) := true
          ; case args of
              [ptr, v] =>
                W.I32_STORE {align = 2, offset = 0}
                :: doExp fctx env (v, doExp fctx env (ptr, acc))
            | _ => raise CodeGenError "Wasm_memory_storeWord32: expected 2 args"
          )
      | Primitives.Wasm_memory_storeWord64 =>
          ( #needsLinearMemory (#ctx fctx) := true
          ; case args of
              [ptr, v] =>
                W.I32_STORE {align = 3, offset = 0}
                :: doExp fctx env (v, doExp fctx env (ptr, acc))
            | _ => raise CodeGenError "Wasm_memory_storeWord64: expected 2 args"
          )
      | Primitives.Wasm_memory_storeChar8 =>
          ( #needsLinearMemory (#ctx fctx) := true
          ; case args of
              [ptr, v] =>
                W.I32_STORE8 {align = 0, offset = 0}
                :: doExp fctx env (v, doExp fctx env (ptr, acc))
            | _ => raise CodeGenError "Wasm_memory_storeChar8: expected 2 args"
          )
      | Primitives.Wasm_ptr_add Primitives.W32 =>
          (case args of
             [ptr, off] =>
               W.I32_BINOP W.ADD
               :: doExp fctx env (off, doExp fctx env (ptr, acc))
           | _ => raise CodeGenError "Wasm_ptr_add: expected 2 args")
      | Primitives.Wasm_ptr_ofWord Primitives.W32 => doUnary [] args
      | Primitives.Wasm_ptr_toWord Primitives.W32 => doUnary [] args

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
      (* ExnTagType: (struct (field name anyref)), used for exception tag
         identity via ref.eq; the field holds the exception name as a $String
         so that General.exnName can retrieve it. *)
      val exnTagType = W.SubType
        { final = false
        , supertypes = []
        , body = W.StructType
            [{mut = W.CONST, storagetype = W.ValStorageType anyref}]
        }
      (* SmlExnType: struct (field tag anyref) (field payload anyref) *)
      val smlExnType = W.SubType
        { final = false
        , supertypes = []
        , body = W.StructType
            [ {mut = W.CONST, storagetype = W.ValStorageType anyref}
            , {mut = W.CONST, storagetype = W.ValStorageType anyref}
            ]
        }
      (* SmlExnFuncType: functype for the $sml_exn exception tag: (func (param anyref)) *)
      val smlExnFuncType = W.SubType
        { final = true
        , supertypes = []
        , body = W.FuncType {params = [anyref], results = []}
        }
      (* Module-defined tag for SML exceptions *)
      val smlExnTag: W.tagtype = {functype = smlExnFuncTypeIdx}
      val taggedDataTypeIdx = 7
      (* TaggedData: (struct (field i32) (field (ref null eq)))
         field 0: integer constructor tag index
         field 1: nullable anyref payload (null for no-payload constructors) *)
      val taggedDataType = W.SubType
        { final = false
        , supertypes = []
        , body = W.StructType
            [ {mut = W.CONST, storagetype = W.ValStorageType (W.NumType W.I32)}
            , {mut = W.CONST, storagetype = W.ValStorageType anyref}
            ]
        }
      val stringTypeIdx = 8
      (* String: (array (mut i8)) — shared representation for string and CharArray *)
      val stringType = W.SubType
        { final = false
        , supertypes = []
        , body =
            W.ArrayType {mut = W.VAR, storagetype = W.PackedStorageType W.I8}
        }
      val refTypeIdx = 9
      (* Ref: (struct (field (mut anyref))) — shared representation for all SML refs *)
      val refType = W.SubType
        { final = false
        , supertypes = []
        , body = W.StructType
            [{mut = W.VAR, storagetype = W.ValStorageType anyref}]
        }
      val arrayTypeIdx = 10
      (* Array: (array (mut anyref)) — polymorphic SML array *)
      val arrayType = W.SubType
        { final = false
        , supertypes = []
        , body =
            W.ArrayType {mut = W.VAR, storagetype = W.ValStorageType anyref}
        }
    in
      { nextTypeIdx = ref 11
      , nextFuncIdx = ref 0
      , revTypes = ref
          [ [arrayType]
          , [refType]
          , [stringType]
          , [taggedDataType]
          , [smlExnFuncType]
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
      , importedFuncs = ref StringMap.empty
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
      , taggedDataTypeIdx = taggedDataTypeIdx
      , stringTypeIdx = stringTypeIdx
      , arrayTypeIdx = arrayTypeIdx
      , refTypeIdx = refTypeIdx
      , needsLinearMemory = ref false
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

  (* Wasm valtype for an exported param or result unboxed type. *)
  fun exportPrimWasmTy (ubt: F.UnboxedTy) : W.valtype =
    case ubt of
      F.UBTyInt64 => W.NumType W.I64
    | F.UBTyWord64 => W.NumType W.I64
    | F.UBTyReal => W.NumType W.F64
    | _ => W.NumType W.I32

  (* Wasm valtype for an export param: NONE = anyref (boxed), SOME ubt = primitive. *)
  fun exportParamWasmTy (ubtOpt: F.UnboxedTy option) : W.valtype =
    case ubtOpt of
      NONE => anyref
    | SOME ubt => exportPrimWasmTy ubt

  (* Box an export param (already on stack) into anyref. NONE = already anyref. *)
  fun boxExportParam (ubtOpt: F.UnboxedTy option) (ctx: Context) : W.instr list =
    case ubtOpt of
      NONE => []
    | SOME ubt => emitBox (ubt, ctx)

  fun genExportWrapper (ctx: Context)
    { name: string
    , closureInstrs:
        W.instr list (* instructions to put the closure on the stack *)
    , paramUnboxedTys: F.UnboxedTy option list
    (* per-param: NONE = anyref, SOME ubt = primitive *)
    , resultUnboxedTy: F.UnboxedTy option
    (* SOME ubt = unbox to primitive; NONE = return as anyref *)
    } =
    let
      val funcTypeIdx = getClosureFuncTypeIdx ctx 1
      val closureLocalTy = anyref
      val nParams = List.length paramUnboxedTys
      val closureLocalIdx = nParams

      (* Instructions to save closure to local *)
      val setupInstrs = closureInstrs @ [W.LOCAL_SET closureLocalIdx]

      val argInstrs =
        case paramUnboxedTys of
          [] => [W.REF_NULL (W.AbsHeapType W.HEAP_NONE)]
        | [ubtOpt] => [W.LOCAL_GET 0] @ boxExportParam ubtOpt ctx
        | _ =>
            let
              val tupleTypeIdx = getTupleTypeIdx ctx nParams
              val (_, boxArgs) =
                List.foldl
                  (fn (ubtOpt, (i, acc)) =>
                     ( i + 1
                     , ([W.LOCAL_GET i] @ boxExportParam ubtOpt ctx) :: acc
                     )) (0, []) paramUnboxedTys
              val boxArgs = List.rev boxArgs
            in
              List.concat boxArgs @ [W.STRUCT_NEW tupleTypeIdx]
            end

      (* Determine result Wasm type and unboxing instructions *)
      val (resultWasmTy, unboxInstrs) =
        case resultUnboxedTy of
          SOME ubt => (exportPrimWasmTy ubt, emitUnbox (ubt, ctx))
        | NONE => (anyref, [])

      (* Stack order for call_ref: (ref $ClosureBase), arg, funcref *)
      val body =
        setupInstrs
        @
        [ W.LOCAL_GET closureLocalIdx
        , W.REF_CAST
            {nullable = false, heaptype = W.TypeIdx (#closureBaseTypeIdx ctx)}
        ] @ argInstrs
        @
        [ W.LOCAL_GET closureLocalIdx
        , W.REF_CAST
            {nullable = false, heaptype = W.TypeIdx (#closureBaseTypeIdx ctx)}
        , W.STRUCT_GET (#closureBaseTypeIdx ctx, 0)
        , W.REF_CAST {nullable = false, heaptype = W.TypeIdx funcTypeIdx}
        , W.CALL_REF funcTypeIdx
        ] @ unboxInstrs

      val wrapperFuncTypeIdx = getFuncTypeIdx ctx
        (List.map exportParamWasmTy paramUnboxedTys, [resultWasmTy])
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

  fun doProgram (ctx: Context) (returnCont: C.CVar) (program: R.Ty N.stat)
    (export: ToFSyntax.export_entity) : W.module =
    let
      (* Phase 1: pre-scan for ForeignCallOp and register all imports.
         This must happen before any allocFuncIdx so import indices come first. *)
      val foreignImports = collectForeignCallOps program
      val () = List.app (registerForeignImport ctx) foreignImports

      (* Fresh function context for the program entry function
         (exported as _start for executables, internal for library mode). *)
      val startCtx = newFuncContext ctx

      (* Set up environment with return continuation *)
      val env = envWithCont (emptyEnv, returnCont, RETURN)

      (* Initialize pre-defined exception values (Match, Bind, Div, Overflow, Size, Subscript).
         These appear as free variables in pattern-match failure closures. Each is initialized
         as a SmlExn struct: {tag = new ExnTagType(), payload = null}.
         The instructions go at the END of initAcc (= BEGINNING of execution order after reversal). *)
      val predefExns =
        [ (InitialEnv.VId_Match, "Match")
        , (InitialEnv.VId_Bind, "Bind")
        , (InitialEnv.VId_Div, "Div")
        , (InitialEnv.VId_Overflow, "Overflow")
        , (InitialEnv.VId_Size, "Size")
        , (InitialEnv.VId_Subscript, "Subscript")
        ]
      val (env, initAcc) =
        List.foldl
          (fn ((vid, name), (e, acc)) =>
             let
               val localIdx = allocLocal startCtx anyref
               (* Instructions in reverse (prepend last-first): set local, then create SmlExn, then null payload, then create tag from its name *)
               val acc =
                 W.LOCAL_SET localIdx :: W.STRUCT_NEW (#smlExnTypeIdx ctx)
                 :: W.REF_NULL (W.AbsHeapType W.HEAP_NONE)
                 :: W.STRUCT_NEW (#exnTagTypeIdx ctx)
                 :: doStringConst (startCtx, name, acc)
             in
               (envWithVar (e, vid, localIdx, NONE), acc)
             end) (env, []) predefExns

      (* Generate body instructions; trailing UNREACHABLE handles fall-through.
         The program's terminating AppCont (from F.ExitProgram, F.ExportValue,
         or F.ExportModule) emits the appropriate return shape, matched
         against the function type chosen below per export. *)
      val revBody = doStat startCtx env (program, initAcc)
      val bodyInstrs = List.rev (W.UNREACHABLE :: revBody)
      val localTypes = List.rev (!(#revLocalTypes startCtx))

      val () =
        case export of
          ToFSyntax.NO_EXPORT =>
            let
              (* WASI Command: _start has type [] -> []. *)
              val funcTypeIdx = getFuncTypeIdx ctx ([], [])
              val funcIdx = allocFuncIdx ctx
              val func: W.func =
                {typeidx = funcTypeIdx, locals = localTypes, body = bodyInstrs}
              val () = #revFuncs ctx := func :: !(#revFuncs ctx)
              val () =
                #revExports ctx
                :=
                {name = "_start", desc = W.ExportFunc funcIdx}
                :: !(#revExports ctx)
            in
              ()
            end
        | ToFSyntax.EXPORT_NAMED names =>
            let
              (* Internal main function returns the export record on the stack. *)
              val mainFuncTypeIdx = getFuncTypeIdx ctx ([], [anyref])
              val mainFuncIdx = allocFuncIdx ctx
              val mainFunc: W.func =
                { typeidx = mainFuncTypeIdx
                , locals = localTypes
                , body = bodyInstrs
                }
              val () = #revFuncs ctx := mainFunc :: !(#revFuncs ctx)

              val nFields = Vector.length names
              val tupleTypeIdx = getTupleTypeIdx ctx nFields
              (* Mutable global cache for the export record. *)
              val globalIdx = List.length (!(#revGlobals ctx))
              val () =
                #revGlobals ctx
                :=
                { globaltype = {mut = W.VAR, valtype = anyref}
                , init = [W.REF_NULL (W.AbsHeapType W.HEAP_NONE)]
                } :: !(#revGlobals ctx)

              (* WASI Reactor: _initialize has type [] -> []. The host must
                 call it once after instantiation, before any other export. *)
              val initFuncTypeIdx = getFuncTypeIdx ctx ([], [])
              val initFuncIdx = allocFuncIdx ctx
              val initBody = [W.CALL mainFuncIdx, W.GLOBAL_SET globalIdx]
              val initFunc: W.func =
                {typeidx = initFuncTypeIdx, locals = [], body = initBody}
              val () = #revFuncs ctx := initFunc :: !(#revFuncs ctx)
              val () =
                #revExports ctx
                :=
                {name = "_initialize", desc = W.ExportFunc initFuncIdx}
                :: !(#revExports ctx)

              val sorted = sortedExportNames names
              val closureInstrs = [W.GLOBAL_GET globalIdx]
              fun genWrappers _ [] = ()
                | genWrappers fieldIdx
                    ((_, name, {paramUnboxedTys, resultUnboxedTy}) :: rest) =
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
                        , paramUnboxedTys = paramUnboxedTys
                        , resultUnboxedTy = resultUnboxedTy
                        }
                    ; genWrappers (fieldIdx + 1) rest
                    )
            in
              genWrappers 0 sorted
            end
        | ToFSyntax.EXPORT_VALUE =>
            raise CodeGenError
              "Wasm backend: EXPORT_VALUE is not supported (use EXPORT_NAMED via `export structure ...`)"

      (* Collect all elem declarations needed for REF_FUNC.
         Import functions occupy indices 0..numImports-1; module-defined
         functions occupy numImports..nextFuncIdx-1. *)
      val numImports = List.length (!(#revImports ctx))
      val numModuleFuncs = !(#nextFuncIdx ctx) - numImports
      val allFuncIdxs = List.tabulate (numModuleFuncs, fn i => i + numImports)
      val elems =
        if null allFuncIdxs then
          []
        else
          [{ elemtype = {nullable = false, heaptype = W.AbsHeapType W.FUNC}
           , init = List.map (fn i => [W.REF_FUNC i]) allFuncIdxs
           , mode = W.ElemDeclarative
           }]
      val needsMem = !(#needsLinearMemory ctx)
      val memExports =
        if needsMem then [{name = "memory", desc = W.ExportMemory 0}] else []
    in
      { types = List.rev (!(#revTypes ctx))
      , funcs = List.rev (!(#revFuncs ctx))
      , tables = []
      , mems = if needsMem then [{limits = {min = 1, max = NONE}}] else []
      , tags = List.rev (!(#revTags ctx))
      , globals = List.rev (!(#revGlobals ctx))
      , elems = elems
      , datas = List.rev (!(#revDatas ctx))
      , start = NONE
      , imports = List.rev (!(#revImports ctx))
      , exports = List.rev (!(#revExports ctx)) @ memExports
      }
    end
end
