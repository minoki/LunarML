functor PackRealImpl (val isBigEndian : bool) :> PACK_REAL where type real = Real.real = struct
type real = Real.real
val bytesPerElem : int = 8
val isBigEndian = isBigEndian
(* f: float, d: double, n: lua_Number *)
local val format = if isBigEndian then ">d" else "<d"
in
fun toBytes (x : real) : Word8Vector.vector =
  Lua.unsafeFromValue (Lua.call1 Lua.Lib.string.pack #[Lua.fromString format, Lua.fromReal x])
fun fromBytes (v : Word8Vector.vector) : real =
  if Word8Vector.length v < bytesPerElem then
    raise Subscript
  else
    Lua.unsafeFromValue (Lua.call1 Lua.Lib.string.unpack #[Lua.fromString format, Lua.unsafeToValue v])
fun subVec (v : Word8Vector.vector, i : int) : real =
  if i < 0 orelse Word8Vector.length v < bytesPerElem * (i + 1) then
    raise Subscript
  else
    let val byteOffset = i * bytesPerElem
    in Lua.unsafeFromValue (Lua.call1 Lua.Lib.string.unpack #[Lua.fromString format, Lua.unsafeToValue v, Lua.fromInt (byteOffset + 1)])
    end
fun subArr (a : Word8Array.array, i : int) : real =
  if i < 0 orelse Word8Array.length a < bytesPerElem * (i + 1) then
    raise Subscript
  else
    let val byteOffset = i * bytesPerElem
        val s = Word8ArraySlice.vector (Word8ArraySlice.slice (a, byteOffset, SOME bytesPerElem))
    in Lua.unsafeFromValue (Lua.call1 Lua.Lib.string.unpack #[Lua.fromString format, Lua.unsafeToValue s])
    end
fun update (a : Word8Array.array, i : int, x : real) =
  if i < 0 orelse Word8Array.length a < bytesPerElem * (i + 1) then
    raise Subscript
  else
    let val byteOffset = i * bytesPerElem
        val s = Lua.unsafeFromValue (Lua.call1 Lua.Lib.string.pack #[Lua.fromString format, Lua.fromReal x])
    in Word8Array.copyVec { src = s, dst = a, di = byteOffset }
    end
end
end;
structure PackRealBig = PackRealImpl (val isBigEndian = true);
structure PackRealLittle = PackRealImpl (val isBigEndian = false);
