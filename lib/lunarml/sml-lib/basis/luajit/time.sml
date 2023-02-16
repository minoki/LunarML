signature TIME = sig
    eqtype time
    exception Time
    val zeroTime : time
    val fromReal : LargeReal.real -> time
    val toReal : time -> LargeReal.real
    val toSeconds : time -> LargeInt.int
    val toMilliseconds : time -> LargeInt.int
    val toMicroseconds : time -> LargeInt.int
    val toNanoseconds : time -> LargeInt.int
    val fromSeconds : LargeInt.int -> time
    val fromMilliseconds : LargeInt.int -> time
    val fromMicroseconds : LargeInt.int -> time
    val fromNanoseconds : LargeInt.int -> time
    val + : time * time -> time
    val - : time * time -> time
    val compare : time * time -> order
    val < : time * time -> bool
    val <= : time * time -> bool
    val > : time * time -> bool
    val >= : time * time -> bool
    val now : unit -> time
    val fmt : int -> time -> string
    val toString : time -> string
    (*
    val scan : (char, 'a) StringCvt.reader -> (time, 'a) StringCvt.reader
    val fromString : string -> time option
    *)
end;
local
    val oslib = LunarML.assumeDiscardable Lua.global "os"
    val os_time = LunarML.assumeDiscardable Lua.field (oslib, "time")
    val os_difftime = LunarML.assumeDiscardable Lua.field (oslib, "difftime")
    val epoch = LunarML.assumeDiscardable (fn () =>
                                              let val t = Lua.newTable ()
                                              in Lua.setField (t, "year", Lua.fromInt 1970)
                                               ; Lua.setField (t, "month", Lua.fromInt 1)
                                               ; Lua.setField (t, "day", Lua.fromInt 1)
                                               ; Lua.setField (t, "hour", Lua.fromInt 0)
                                               ; Lua.call1 os_time #[t]
                                              end
                                          ) () (* local time; depends on time zone *)
in
structure TimeImpl :> sig
              structure Time : TIME
          end = struct
structure Time = struct
type time = Int54.int (* in microseconds *)
exception Time
val zeroTime : time = 0
fun fromReal (x : LargeReal.real) : time = Int54.fromLarge (LargeReal.toLargeInt IEEEReal.TO_ZERO (x * 1e6)) handle _ => raise Time
fun toReal (x : time) : LargeReal.real = LargeReal.fromLargeInt (Int54.toLarge x) / 1e6
end
fun fromLuaTime (x : Lua.value) : Time.time
    = let val d = Lua.unsafeFromValue (Lua.call1 os_difftime #[x, epoch]) : real
      in Time.fromReal d
      end
fun toLuaTime (x : Int54.int) : Lua.value
    = let val x = x div 1000_000
          val sec = x mod 60
          val min' = x div 60
          val min = min' mod 60
          val hour' = min' div 60
          val hour = hour' mod 60
          val day' = hour' div 24
          val t = Lua.newTable ()
      in Lua.setField (t, "year", Lua.fromInt 1970)
       ; Lua.setField (t, "month", Lua.fromInt 1)
       ; Lua.setField (t, "day", Lua.fromInt54 (day' + 1))
       ; Lua.setField (t, "hour", Lua.fromInt54 hour)
       ; Lua.setField (t, "min", Lua.fromInt54 min)
       ; Lua.setField (t, "sec", Lua.fromInt54 sec)
       ; Lua.call1 os_time #[t]
      end
structure Time = struct
open Time
local
    val Iquot = Int54.quot
    val Lquot = LargeInt.quot
    infix 7 Lquot Iquot
in
fun toSeconds (x : time) = Int54.toLarge (x Iquot 1000_000)
fun toMilliseconds (x : time) = Int54.toLarge (x Iquot 1000)
fun toMicroseconds (x : time) = Int54.toLarge x
fun toNanoseconds (x : time) = Int54.toLarge x * 1000
fun fromSeconds (x : LargeInt.int) : time = Int54.fromLarge x * 1000_000 handle Overflow => raise Time
fun fromMilliseconds (x : LargeInt.int) : time = Int54.fromLarge x * 1000 handle Overflow => raise Time
fun fromMicroseconds (x : LargeInt.int) : time = Int54.fromLarge x handle Overflow => raise Time
fun fromNanoseconds (x : LargeInt.int) : time = Int54.fromLarge (x Lquot 1000) handle Overflow => raise Time
end
val compare = Int54.compare
fun now () : time = fromLuaTime (Lua.call1 os_time #[])
fun fmt (n : int) (t : time) : string
    = if n < 0 then
          raise Size
      else if n = 0 then
          let val x = toReal t
          in Lua.unsafeFromValue (Lua.call1 Lua.Lib.string.format #[Lua.fromString "%d", Lua.fromReal x])
          end
      else (* n > 0 *)
          let val x = toReal t
              val fmt = Lua.unsafeFromValue (Lua.call1 Lua.Lib.string.format #[Lua.fromString "%%.%df", Lua.fromInt n])
          in Lua.unsafeFromValue (Lua.call1 Lua.Lib.string.format #[fmt, Lua.fromReal x])
          end
fun toString t = fmt 3 t
fun x + y = Int54.+ (x, y) handle Overflow => raise Time
fun x - y = Int54.- (x, y) handle Overflow => raise Time
val op < = Int54.<
val op <= = Int54.<=
val op > = Int54.>
val op >= = Int54.>=
end (* structure Time *)
end (* structure TimeImpl *)
end; (* local *)
structure Time = TimeImpl.Time;
