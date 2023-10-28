signature TIMER = sig
    type cpu_timer
    type real_timer
    val startCPUTimer : unit -> cpu_timer
    val checkCPUTimes : cpu_timer -> { nongc : { usr : Time.time, sys : Time.time }
                                     , gc : { usr : Time.time, sys : Time.time }
                                     }
    val checkCPUTimer : cpu_timer -> { usr : Time.time, sys : Time.time }
    val checkGCTime : cpu_timer -> Time.time
    val totalCPUTimer : unit -> cpu_timer
    val startRealTimer : unit -> real_timer
    val checkRealTimer : real_timer -> Time.time
    val totalRealTimer : unit -> real_timer
end;
structure Timer :> TIMER = struct
type cpu_timer = real
type real_timer = Lua.value
fun startCPUTimer () = Lua.unsafeFromValue (Lua.call1 Lua.Lib.os.clock #[]) : real
fun checkCPUTimer t0 = let val t1 = Lua.unsafeFromValue (Lua.call1 Lua.Lib.os.clock #[]) : real
                       in { usr = Time.fromReal (t1 - t0), sys = Time.zeroTime }
                       end
fun checkCPUTimes t0 = { nongc = checkCPUTimer t0, gc = { usr = Time.zeroTime, sys = Time.zeroTime } }
fun checkGCTime t0 = Time.zeroTime
fun totalCPUTimer () = 0.0
fun startRealTimer () = Lua.call1 Lua.Lib.os.time #[]
fun checkRealTimer t0 = let val d = Lua.unsafeFromValue (Lua.call1 Lua.Lib.os.difftime #[Lua.call1 Lua.Lib.os.time #[], t0]) : real
                        in Time.fromReal (d * 1e6)
                        end
local
    val t0 = LunarML.assumeDiscardable (fn () => Lua.call1 Lua.Lib.os.time #[]) ()
in
fun totalRealTimer () = t0
end
end;
