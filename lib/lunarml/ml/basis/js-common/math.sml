structure Math :> MATH where type real = Real.real = struct
type real = real
val pi : real = JavaScript.unsafeFromValue JavaScript.Lib.Math.PI
val sqrt : real -> real = fn x => JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.Math.sqrt #[JavaScript.fromReal x])
val sin : real -> real = fn x => JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.Math.sin #[JavaScript.fromReal x])
val cos : real -> real = fn x => JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.Math.cos #[JavaScript.fromReal x])
val tan : real -> real = fn x => JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.Math.tan #[JavaScript.fromReal x])
val asin : real -> real = fn x => JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.Math.asin #[JavaScript.fromReal x])
val acos : real -> real = fn x => JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.Math.acos #[JavaScript.fromReal x])
val atan : real -> real = fn x => JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.Math.atan #[JavaScript.fromReal x])
val atan2 : real * real -> real = fn (y, x) => JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.Math.atan2 #[JavaScript.fromReal y, JavaScript.fromReal x])
val exp : real -> real = fn x => JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.Math.exp #[JavaScript.fromReal x])
val e = JavaScript.unsafeFromValue JavaScript.Lib.Math.E : real
val pow : real * real -> real = fn (x, y) => JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.Math.pow #[JavaScript.fromReal x, JavaScript.fromReal y])
val ln : real -> real = fn x => JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.Math.log #[JavaScript.fromReal x])
val log10 : real -> real = fn x => JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.Math.log10 #[JavaScript.fromReal x])
val sinh : real -> real = fn x => JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.Math.sinh #[JavaScript.fromReal x])
val cosh : real -> real = fn x => JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.Math.cosh #[JavaScript.fromReal x])
val tanh : real -> real = fn x => JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.Math.tanh #[JavaScript.fromReal x])
end; (* structure Math *)
