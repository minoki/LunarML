(*
 * Copyright (c) 2026 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure WasmMemory :>
sig
  type ptr
  val add : ptr * Word32.word -> ptr
  val ofWord32 : Word32.word -> ptr
  val toWord32 : ptr -> Word32.word
  val loadWord8 : ptr -> Word8.word
  val loadWord16 : ptr -> Word16.word
  val loadWord32 : ptr -> Word32.word
  val loadWord64 : ptr -> Word64.word
  val loadChar8 : ptr -> char
  val storeWord8 : ptr * Word8.word -> unit
  val storeWord16 : ptr * Word16.word -> unit
  val storeWord32 : ptr * Word32.word -> unit
  val storeWord64 : ptr * Word64.word -> unit
  val storeChar8 : ptr * char -> unit
end =
struct
  type ptr = _Prim.Wasm.ptr
  fun add (p, n) = _primCall "Wasm.ptr.add" (p, WordImpl.word32ToWord n)
  fun ofWord32 w = _primCall "Wasm.ptr.ofWord" (WordImpl.word32ToWord w)
  fun toWord32 p = WordImpl.wordToWord32 (_primCall "Wasm.ptr.toWord" (p))
  fun loadWord8 p = WordImpl.uncheckedWordToWord8 (_primCall "Wasm.memory.loadWord8AsWord" (p))
  fun loadWord16 p = WordImpl.uncheckedWordToWord16 (_primCall "Wasm.memory.loadWord16AsWord" (p))
  fun loadWord32 p = WordImpl.wordToWord32 (_primCall "Wasm.memory.loadWord32AsWord" (p))
  fun loadWord64 p = _primCall "Wasm.memory.loadWord64" (p)
  fun loadChar8 p = _primCall "Wasm.memory.loadChar8" (p)
  fun storeWord8 (p, v) = _primCall "Wasm.memory.storeWordAsWord8" (p, WordImpl.word8ToWord v)
  fun storeWord16 (p, v) = _primCall "Wasm.memory.storeWordAsWord16" (p, WordImpl.word16ToWord v)
  fun storeWord32 (p, v) = _primCall "Wasm.memory.storeWordAsWord32" (p, WordImpl.word32ToWord v)
  fun storeWord64 (p, v) = _primCall "Wasm.memory.storeWord64" (p, v)
  fun storeChar8 (p, v) = _primCall "Wasm.memory.storeChar8" (p, v)
end;
