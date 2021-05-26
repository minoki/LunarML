val export = "Hello world!" (* shadowed and not exported *)
val export = "Good bye world!" (* shadowed and not exported *)
fun add args = let val a = Vector.sub (args, 0)
                   val b = Vector.sub (args, 1)
               in Vector.fromList [Lua.fromInt (Lua.unsafeFromValue a + Lua.unsafeFromValue b)]
               end
val export = Lua.function add;
