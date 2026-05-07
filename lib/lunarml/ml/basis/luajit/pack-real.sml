val PackRealImpl_ffi = Lua.call1WithEffect Lua.PrimEffect.pure Lua.Lib.require #[Lua.fromString "ffi"]
val PackRealImpl_ffi_abi = Lua.fieldWithEffect (PackRealImpl_ffi, "abi", Lua.PrimEffect.pure)
val PackRealImpl_ffi_cast = Lua.fieldWithEffect (PackRealImpl_ffi, "cast", Lua.PrimEffect.pure)
val PackRealImpl_ffi_typeof = Lua.fieldWithEffect (PackRealImpl_ffi, "typeof", Lua.PrimEffect.pure)
val PackRealImpl_ffi_string = Lua.fieldWithEffect (PackRealImpl_ffi, "string", Lua.PrimEffect.pure)
val PackRealImpl_ffi_copy = Lua.fieldWithEffect (PackRealImpl_ffi, "copy", Lua.PrimEffect.pure)
functor PackRealImpl (val isBigEndian : bool) :> PACK_REAL where type real = Real.real = struct
type real = Real.real
val bytesPerElem : int = 8
val isBigEndian = isBigEndian
local
val isNativeEndian = isBigEndian = Lua.unsafeFromValue (Lua.call1WithEffect Lua.PrimEffect.pure PackRealImpl_ffi_abi #[Lua.fromString "be"])
val buffer_type = Lua.call1WithEffect Lua.PrimEffect.pure PackRealImpl_ffi_typeof #[Lua.fromString "unsigned char[8]"]
val const_char_ptr = Lua.call1WithEffect Lua.PrimEffect.pure PackRealImpl_ffi_typeof #[Lua.fromString "const char*"]
val double_type = Lua.call1WithEffect Lua.PrimEffect.pure PackRealImpl_ffi_typeof #[Lua.fromString "double[1]"]
(* fun newBuffer () = Lua.call1 buffer_type #[] *)
val swapEndianIfNecessary : Word8Vector.vector -> Word8Vector.vector =
  if isNativeEndian then
    fn v => v
  else
    fn v => Lua.unsafeFromValue (Lua.call1 Lua.Lib.string.reverse #[Lua.unsafeToValue v])
in
fun toBytes (x : real) : Word8Vector.vector =
  let val dbuf = Lua.call1 double_type #[Lua.fromReal x]
      val ptr = Lua.call1 PackRealImpl_ffi_cast #[const_char_ptr, dbuf]
  in swapEndianIfNecessary (Lua.unsafeFromValue (Lua.call1 PackRealImpl_ffi_string #[ptr, Lua.fromInt bytesPerElem]))
  end
fun uncheckedFromBytes (v : Word8Vector.vector) : real =
  let val dbuf = Lua.call1 double_type #[]
      val () = Lua.call0 PackRealImpl_ffi_copy #[dbuf, Lua.unsafeToValue (swapEndianIfNecessary v), Lua.fromInt bytesPerElem]
  in Lua.unsafeFromValue (Lua.sub (dbuf, Lua.fromInt 0))
  end
fun fromBytes (v : Word8Vector.vector) : real =
  if Word8Vector.length v < bytesPerElem then
    raise Subscript
  else
    uncheckedFromBytes v
fun subVec (v : Word8Vector.vector, i : int) : real =
  if i < 0 orelse Word8Vector.length v < bytesPerElem * (i + 1) then
    raise Subscript
  else
    let val byteOffset = i * bytesPerElem
    in uncheckedFromBytes (Word8VectorSlice.vector (Word8VectorSlice.slice (v, byteOffset, SOME bytesPerElem)))
    end
fun subArr (a : Word8Array.array, i : int) : real =
  if i < 0 orelse Word8Array.length a < bytesPerElem * (i + 1) then
    raise Subscript
  else
    let val byteOffset = i * bytesPerElem
    in uncheckedFromBytes (Word8ArraySlice.vector (Word8ArraySlice.slice (a, byteOffset, SOME bytesPerElem)))
    end
fun update (a : Word8Array.array, i : int, x : real) =
  if i < 0 orelse Word8Array.length a < bytesPerElem * (i + 1) then
    raise Subscript
  else
    let val byteOffset = i * bytesPerElem
        val s = toBytes x
    in Word8Array.copyVec { src = s, dst = a, di = byteOffset }
    end
end
end;
structure PackRealBig = PackRealImpl (val isBigEndian = true);
structure PackRealLittle = PackRealImpl (val isBigEndian = false);
