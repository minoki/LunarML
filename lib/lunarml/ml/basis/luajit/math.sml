structure Math :> MATH where type real = Real.real = struct
type real = real
val pi : real = Lua.unsafeFromValue (LunarML.assumeDiscardable Lua.field (Lua.Lib.math, "pi"))
val sqrt : real -> real = Lua.unsafeFromValue (LunarML.assumeDiscardable Lua.field (Lua.Lib.math, "sqrt"))
val sin : real -> real = Lua.unsafeFromValue (LunarML.assumeDiscardable Lua.field (Lua.Lib.math, "sin"))
val cos : real -> real = Lua.unsafeFromValue (LunarML.assumeDiscardable Lua.field (Lua.Lib.math, "cos"))
val tan : real -> real = Lua.unsafeFromValue (LunarML.assumeDiscardable Lua.field (Lua.Lib.math, "tan"))
val asin : real -> real = Lua.unsafeFromValue (LunarML.assumeDiscardable Lua.field (Lua.Lib.math, "asin"))
val acos : real -> real = Lua.unsafeFromValue (LunarML.assumeDiscardable Lua.field (Lua.Lib.math, "acos"))
val atan : real -> real = Lua.unsafeFromValue Lua.Lib.math.atan
val atan2 : real * real -> real = fn (y, x) => Lua.unsafeFromValue (Lua.call1 Lua.Lib.math.atan #[Lua.fromReal y, Lua.fromReal x])
val exp : real -> real = Lua.unsafeFromValue (LunarML.assumeDiscardable Lua.field (Lua.Lib.math, "exp"))
val e = LunarML.assumeDiscardable exp 1.0
val pow : real * real -> real = fn (x, y) => Lua.unsafeFromValue (Lua.pow (Lua.fromReal x, Lua.fromReal y))
val ln : real -> real = Lua.unsafeFromValue Lua.Lib.math.log
val log10 : real -> real = fn x => Lua.unsafeFromValue (Lua.call1 Lua.Lib.math.log #[Lua.fromReal x, Lua.fromInt 10])
val sinh : real -> real = LunarML.assumeDiscardable
                              (fn () => let val raw = Lua.field (Lua.Lib.math, "sinh")
                                        in if Lua.isNil raw then
                                               fn x => (exp x - exp (~ x)) / 2.0
                                           else
                                               Lua.unsafeFromValue raw : real -> real
                                        end
                              ) ()
val cosh : real -> real = LunarML.assumeDiscardable
                              (fn () => let val raw = Lua.field (Lua.Lib.math, "cosh")
                                        in if Lua.isNil raw then
                                               fn x => (exp x + exp (~ x)) / 2.0
                                           else
                                               Lua.unsafeFromValue raw : real -> real
                                        end
                              ) ()
val tanh : real -> real = LunarML.assumeDiscardable
                              (fn () => let val raw = Lua.field (Lua.Lib.math, "tanh")
                                        in if Lua.isNil raw then
                                               fn x => let val ex = exp x
                                                           val e_x = exp (~ x)
                                                       in (ex - e_x) / (ex + e_x)
                                                       end
                                           else
                                               Lua.unsafeFromValue raw : real -> real
                                        end
                              ) ()
end; (* structure Math *)
