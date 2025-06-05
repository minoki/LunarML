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
