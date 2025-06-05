local
    _esImport [pure] { cpuUsage } from "node:process";
in
structure Timer :> TIMER = struct
type cpu_timer = JavaScript.value
fun startCPUTimer () = JavaScript.call cpuUsage #[]
fun checkCPUTimer prev = let val now = JavaScript.call cpuUsage #[prev]
                             val usr = JavaScript.unsafeFromValue (JavaScript.field (now, "user")) : Int54.int (* microseconds *)
                             val sys = JavaScript.unsafeFromValue (JavaScript.field (now, "system")) : Int54.int (* microseconds *)
                         in { usr = Time.fromMicroseconds (Int54.toLarge usr)
                            , sys = Time.fromMicroseconds (Int54.toLarge sys)
                            }
                         end
fun checkCPUTimes prev = { nongc = checkCPUTimer prev, gc = { usr = Time.zeroTime, sys = Time.zeroTime } }
fun checkGCTime _ = Time.zeroTime
fun totalCPUTimer () = JavaScript.undefined
type real_timer = Int54.int
fun startRealTimer () = JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.Date.now #[]) : Int54.int
fun checkRealTimer t0 = let val now = JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.Date.now #[]) : Int54.int
                        in Time.fromMilliseconds (Int54.toLarge (now - t0))
                        end
local
    val t0 = LunarML.assumeDiscardable (fn () => JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.Date.now #[]) : Int54.int) ()
in
fun totalRealTimer () = t0 (* or process.uptime()? *)
end
end
end; (* local *)
