functor PackRealImpl (val isBigEndian : bool) :> PACK_REAL where type real = Real.real = struct
type real = Real.real
val bytesPerElem : int = 8
val isBigEndian = isBigEndian
local val littleEndian = not isBigEndian
in
fun toBytes (x : real) : Word8Vector.vector =
  let val buffer = JavaScript.new JavaScript.Lib.ArrayBuffer #[JavaScript.fromInt bytesPerElem]
      val view = JavaScript.new JavaScript.Lib.DataView #[buffer]
      val _ = JavaScript.method (view, "setFloat64") #[JavaScript.fromInt 0, JavaScript.fromReal x, JavaScript.fromBool littleEndian]
  in JavaScript.unsafeFromValue (JavaScript.new JavaScript.Lib.Uint8Array #[buffer])
  end
fun fromBytes (v : Word8Vector.vector) : real =
  if Word8Vector.length v < bytesPerElem then
    raise Subscript
  else
    let val buffer = JavaScript.field (JavaScript.unsafeToValue v, "buffer")
        val view = JavaScript.new JavaScript.Lib.DataView #[buffer]
    in JavaScript.unsafeFromValue (JavaScript.method (view, "getFloat64") #[JavaScript.fromInt 0, JavaScript.fromBool littleEndian])
    end
fun subVec (v : Word8Vector.vector, i : int) : real =
  if i < 0 orelse Word8Vector.length v < bytesPerElem * (i + 1) then
    raise Subscript
  else
    let val buffer = JavaScript.field (JavaScript.unsafeToValue v, "buffer")
        val view = JavaScript.new JavaScript.Lib.DataView #[buffer]
        val byteOffset = i * bytesPerElem
    in JavaScript.unsafeFromValue (JavaScript.method (view, "getFloat64") #[JavaScript.fromInt byteOffset, JavaScript.fromBool littleEndian])
    end
fun subArr (a : Word8Array.array, i : int) : real =
  if i < 0 orelse Word8Array.length a < bytesPerElem * (i + 1) then
    raise Subscript
  else
    let val buffer = JavaScript.field (JavaScript.unsafeToValue a, "buffer")
        val view = JavaScript.new JavaScript.Lib.DataView #[buffer]
        val byteOffset = i * bytesPerElem
    in JavaScript.unsafeFromValue (JavaScript.method (view, "getFloat64") #[JavaScript.fromInt byteOffset, JavaScript.fromBool littleEndian])
    end
fun update (a : Word8Array.array, i : int, x : real) =
  if i < 0 orelse Word8Array.length a < bytesPerElem * (i + 1) then
    raise Subscript
  else
    let val buffer = JavaScript.field (JavaScript.unsafeToValue a, "buffer")
        val view = JavaScript.new JavaScript.Lib.DataView #[buffer]
        val byteOffset = i * bytesPerElem
    in ignore (JavaScript.method (view, "setFloat64") #[JavaScript.fromInt byteOffset, JavaScript.fromReal x, JavaScript.fromBool littleEndian])
    end
end
end;
structure PackRealBig = PackRealImpl (val isBigEndian = true);
structure PackRealLittle = PackRealImpl (val isBigEndian = false);
