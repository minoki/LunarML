Lua.call0 Lua.Lib.error #[Lua.fromString "Hello!", Lua.fromInt 0]
handle Lua.Error e => (print (Lua.typeof e ^ "\n"); print (Lua.unsafeFromValue e ^ "\n"));
