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
                                              in Lua.set (t, Lua.fromString "year", Lua.fromInt 1970)
                                               ; Lua.set (t, Lua.fromString "month", Lua.fromInt 1)
                                               ; Lua.set (t, Lua.fromString "day", Lua.fromInt 1)
                                               ; Lua.set (t, Lua.fromString "hour", Lua.fromInt 0)
                                               ; Lua.call1 os_time #[t]
                                              end
                                          ) () (* local time; depends on time zone *)
in
structure TimeImpl :> sig
              structure Time : TIME
          end = struct
structure Time = struct
type time = int (* in microseconds *)
exception Time
val zeroTime : time = 0
fun fromReal (x : LargeReal.real) : time = LargeReal.trunc (x * 1e6) handle _ => raise Time
fun toReal (x : time) : LargeReal.real = LargeReal.fromInt x / 1e6
end
fun fromLuaTime (x : Lua.value) : int
    = let val d = Lua.unsafeFromValue (Lua.call1 os_difftime #[x, epoch]) : real
      in Time.fromReal d
      end
fun toLuaTime (x : int) : Lua.value
    = let val x = x div 1000_000
          val sec = x mod 60
          val min' = x div 60
          val min = min' mod 60
          val hour' = min' div 60
          val hour = hour' mod 60
          val day' = hour' div 24
          val t = Lua.newTable ()
      in Lua.set (t, Lua.fromString "year", Lua.fromInt 1970)
       ; Lua.set (t, Lua.fromString "month", Lua.fromInt 1)
       ; Lua.set (t, Lua.fromString "day", Lua.fromInt (day' + 1))
       ; Lua.set (t, Lua.fromString "hour", Lua.fromInt hour)
       ; Lua.set (t, Lua.fromString "min", Lua.fromInt min)
       ; Lua.set (t, Lua.fromString "sec", Lua.fromInt sec)
       ; Lua.call1 os_time #[t]
      end
structure Time = struct
open Time
local
    val Iquot = Int.quot
    val Lquot = LargeInt.quot
    infix 7 Lquot Iquot
in
fun toSeconds (x : time) = LargeInt.fromInt (x Iquot 1000_000)
fun toMilliseconds (x : time) = LargeInt.fromInt (x Iquot 1000)
fun toMicroseconds (x : time) = LargeInt.fromInt x
fun toNanoseconds (x : time) = LargeInt.fromInt x * 1000
fun fromSeconds (x : LargeInt.int) : time = LargeInt.toInt x * 1000_000 handle Overflow => raise Time
fun fromMilliseconds (x : LargeInt.int) : time = LargeInt.toInt x * 1000 handle Overflow => raise Time
fun fromMicroseconds (x : LargeInt.int) : time = LargeInt.toInt x handle Overflow => raise Time
fun fromNanoseconds (x : LargeInt.int) : time = LargeInt.toInt (x Lquot 1000) handle Overflow => raise Time
end
val compare = Int.compare
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
fun x + y = Int.+ (x, y) handle Overflow => raise Time
fun x - y = Int.- (x, y) handle Overflow => raise Time
val op < = Int.<
val op <= = Int.<=
val op > = Int.>
val op >= = Int.>=
end (* structure Time *)
end (* structure TimeImpl *)
end; (* local *)
structure Time = TimeImpl.Time;
