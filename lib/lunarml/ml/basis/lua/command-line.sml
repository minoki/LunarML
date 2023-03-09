structure CommandLine : sig
              val name : unit -> string
              val arguments : unit -> string list
          end = struct
local
    val luaarg = LunarML.assumeDiscardable Lua.global "arg"
in
val name : unit -> string = fn () => let val s = Lua.sub (luaarg, Lua.fromInt 0)
                                     in if Lua.isNil s then
                                            raise Fail "CommandLine.name: arg is not available"
                                        else
                                            Lua.unsafeFromValue s
                                     end
val arguments : unit -> string list = fn () => List.tabulate (Lua.unsafeFromValue (Lua.length luaarg), fn i => Lua.unsafeFromValue (Lua.sub (luaarg, Lua.fromInt (i + 1))) : string)
end
end; (* structure CommandLine *)
