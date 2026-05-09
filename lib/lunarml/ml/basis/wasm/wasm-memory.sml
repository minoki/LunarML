(*
 * Copyright (c) 2026 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure WasmMemory :>
sig
  type ptr
  val add      : ptr * _Prim.Word32.word -> ptr
  val ofWord32 : _Prim.Word32.word -> ptr
  val toWord32 : ptr -> _Prim.Word32.word
  val load8u   : ptr -> word
  val load32   : ptr -> _Prim.Word32.word
  val store8   : ptr * word -> unit
  val store32  : ptr * _Prim.Word32.word -> unit
end =
struct
  type ptr = _Prim.Wasm.ptr
  fun add (p, n)    = _primCall "Wasm.ptr.add" (p, n)
  fun ofWord32 w    = _primCall "Wasm.ptr.ofWord32" (w)
  fun toWord32 p    = _primCall "Wasm.ptr.toWord32" (p)
  fun load8u p      = _primCall "Wasm.memory.load8_u" (p)
  fun load32 p      = _primCall "Wasm.memory.load32" (p)
  fun store8  (p, v) = _primCall "Wasm.memory.store8" (p, v)
  fun store32 (p, v) = _primCall "Wasm.memory.store32" (p, v)
end;
