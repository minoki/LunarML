structure Timer :> TIMER = struct
local
val performance = LunarML.assumeDiscardable JavaScript.global "performance"
fun performance_now (): real = JavaScript.unsafeFromValue (JavaScript.method (performance, "now") #[])
in
type cpu_timer = real
type real_timer = real
val startCPUTimer = performance_now
fun checkCPUTimer t0 = let val t1 = performance_now ()
                       in { usr = Time.fromReal ((t1 - t0) / 1000.0), sys = Time.zeroTime }
                       end
fun checkCPUTimes t0 = { nongc = checkCPUTimer t0, gc = { usr = Time.zeroTime, sys = Time.zeroTime } }
fun checkGCTime t0 = Time.zeroTime
fun totalCPUTimer () = 0.0
val startRealTimer = performance_now
fun checkRealTimer t0 = let val t1 = performance_now ()
                       in Time.fromReal ((t1 - t0) / 1000.0)
                       end
local
    val t0 = LunarML.assumeDiscardable performance_now ()
in
fun totalRealTimer () = t0
end
end
end;
