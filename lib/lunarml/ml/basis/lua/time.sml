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
    (* Avoid 1970-01-01 00:00:00 in local time, which might be negative *)
    val epoch = LunarML.assumeDiscardable (fn () =>
                                              let val t = Lua.newTable ()
                                              in Lua.setField (t, "year", Lua.fromInt 2001)
                                               ; Lua.setField (t, "month", Lua.fromInt 1)
                                               ; Lua.setField (t, "day", Lua.fromInt 1)
                                               ; Lua.setField (t, "hour", Lua.fromInt 0)
                                               ; Lua.call1 Lua.Lib.os.time #[t]
                                              end
                                          ) () (* local time; depends on time zone *)
in
structure TimeImpl :> sig
              structure Time : TIME
              structure Date : sig
                            datatype weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun
                            datatype month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
                            type date
                            exception Date
                            val date : { year : int, month : month, day : int, hour : int, minute : int, second : int, offset : Time.time option } -> date
                            val year : date -> int
                            val month : date -> month
                            val day : date -> int
                            val hour : date -> int
                            val minute : date -> int
                            val second : date -> int
                            val weekDay : date -> weekday
                            val yearDay : date -> int
                            val offset : date -> Time.time option
                            val isDst : date -> bool option
                            val localOffset : unit -> Time.time
                            val fromTimeLocal : Time.time -> date
                            val fromTimeUniv : Time.time -> date
                            val toTime : date -> Time.time
                            val compare : date * date -> order
                            val fmt : string -> date -> string
                            val toString : date -> string
                            (*
                            val scan : (char, 'a) StringCvt.reader -> (date, 'a) StringCvt.reader
                            val fromString : string -> date option
                            *)
                        end
              val fromLuaTime : Lua.value -> Time.time
              val toLuaTime : Time.time -> Lua.value
          end = struct
structure Time = struct
type time = int (* in microseconds *)
exception Time
val zeroTime : time = 0
fun fromReal (x : LargeReal.real) : time = LargeReal.trunc (x * 1e6) handle _ => raise Time
fun toReal (x : time) : LargeReal.real = LargeReal.fromInt x / 1e6
end
fun fromLuaTime (x : Lua.value) : int
    = let val d = Lua.unsafeFromValue (Lua.call1 Lua.Lib.os.difftime #[x, epoch]) : real
      in Time.fromReal d
      end
fun toLuaTime (x : int) : Lua.value
    = let val x = x div 1000_000
          val sec = x mod 60
          val min' = x div 60
          val min = min' mod 60
          val hour' = min' div 60
          val hour = hour' mod 24
          val day' = hour' div 24
          val t = Lua.newTable ()
      in Lua.setField (t, "year", Lua.fromInt 2001)
       ; Lua.setField (t, "month", Lua.fromInt 1)
       ; Lua.setField (t, "day", Lua.fromInt (day' + 1))
       ; Lua.setField (t, "hour", Lua.fromInt hour)
       ; Lua.setField (t, "min", Lua.fromInt min)
       ; Lua.setField (t, "sec", Lua.fromInt sec)
       ; Lua.call1 Lua.Lib.os.time #[t]
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
fun now () : time = fromLuaTime (Lua.call1 Lua.Lib.os.time #[])
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
structure Date = struct
datatype weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun
datatype month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
type date = Lua.value
exception Date
fun monthToInt Jan = 1
  | monthToInt Feb = 2
  | monthToInt Mar = 3
  | monthToInt Apr = 4
  | monthToInt May = 5
  | monthToInt Jun = 6
  | monthToInt Jul = 7
  | monthToInt Aug = 8
  | monthToInt Sep = 9
  | monthToInt Oct = 10
  | monthToInt Nov = 11
  | monthToInt Dec = 12
fun date { year : int, month : month, day : int, hour : int, minute : int, second : int, offset : Time.time option }
    = let val t = Lua.newTable ()
          val () = Lua.setField (t, "year", Lua.fromInt year)
          val () = Lua.setField (t, "month", Lua.fromInt (monthToInt month))
          val () = Lua.setField (t, "day", Lua.fromInt day)
          val () = Lua.setField (t, "hour", Lua.fromInt hour)
          val () = Lua.setField (t, "min", Lua.fromInt minute)
          val () = Lua.setField (t, "sec", Lua.fromInt second)
          val u = Lua.call1 Lua.Lib.os.date #[Lua.fromString "*t", Lua.call1 Lua.Lib.os.time #[t]] (* TODO: error handling *)
          val () = Lua.setField (u, "offset", Lua.unsafeToValue offset)
      in u
      end
fun year t : int = Lua.unsafeFromValue (Lua.field (t, "year"))
fun monthAsInt t : int = Lua.unsafeFromValue (Lua.field (t, "month"))
fun month t : month = case monthAsInt t of
                          1 => Jan
                        | 2 => Feb
                        | 3 => Mar
                        | 4 => Apr
                        | 5 => May
                        | 6 => Jun
                        | 7 => Jul
                        | 8 => Aug
                        | 9 => Sep
                        | 10 => Oct
                        | 11 => Nov
                        | _ => Dec
fun day t : int = Lua.unsafeFromValue (Lua.field (t, "day"))
fun hour t : int = Lua.unsafeFromValue (Lua.field (t, "hour"))
fun minute t : int = Lua.unsafeFromValue (Lua.field (t, "min"))
fun second t : int = Lua.unsafeFromValue (Lua.field (t, "sec"))
fun weekDay t : weekday = case Lua.unsafeFromValue (Lua.field (t, "wday")) : int of
                              1 => Sun
                            | 2 => Mon
                            | 3 => Tue
                            | 4 => Wed
                            | 5 => Thu
                            | 6 => Fri
                            | _ => Sat
fun yearDay t : int = Lua.unsafeFromValue (Lua.field (t, "yday")) - 1
fun offset t : Time.time option = Lua.unsafeFromValue (Lua.field (t, "offset"))
fun isDst t : bool option = let val x = Lua.unsafeFromValue (Lua.field (t, "isdst"))
                            in if Lua.isNil x then
                                   NONE
                               else
                                   SOME (Lua.unsafeFromValue x) (* boolean *)
                            end
fun localOffset () : Time.time = let val now = Lua.call1 Lua.Lib.os.time #[]
                                     val t_utc = Lua.call1 Lua.Lib.os.time #[Lua.call1 Lua.Lib.os.date #[Lua.fromString "!*t", now]]
                                 in Time.fromReal (Lua.unsafeFromValue (Lua.call1 Lua.Lib.os.difftime #[t_utc, now]))
                                 end
fun fromTimeLocal t = let val u = Lua.call1 Lua.Lib.os.date #[Lua.fromString "*t", toLuaTime t]
                          val () = Lua.setField (u, "offset", Lua.unsafeToValue (NONE : Time.time option))
                      in u
                      end
fun fromTimeUniv t = let val u = Lua.call1 Lua.Lib.os.date #[Lua.fromString "!*t", toLuaTime t]
                         val () = Lua.setField (u, "offset", Lua.unsafeToValue (SOME Time.zeroTime : Time.time option))
                     in u
                     end
fun toTime date = let val t = fromLuaTime (Lua.call1 Lua.Lib.os.time #[date]) (* local time *)
                  in case offset date of
                         NONE => t
                       | SOME u => t + u
                  end
fun compare (date1, date2) = case Int.compare (year date1, year date2) of
                                 EQUAL => (case Int.compare (monthAsInt date1, monthAsInt date2) of
                                               EQUAL => (case Int.compare (day date1, day date2) of
                                                             EQUAL => (case Int.compare (hour date1, hour date2) of
                                                                           EQUAL => (case Int.compare (minute date1, minute date2) of
                                                                                         EQUAL => Int.compare (second date1, second date2)
                                                                                       | r => r
                                                                                    )
                                                                         | r => r
                                                                      )
                                                           | r => r
                                                        )
                                             | r => r
                                          )
                               | r => r
fun doFmt ("*t", date) = "*t"
  | doFmt (s, date) = Lua.unsafeFromValue (Lua.call1 Lua.Lib.os.date #[Lua.fromString s, toLuaTime (toTime date)]) : string
fun fmt s date = if String.sub (s, 0) = #"!" then
                     "!" ^ doFmt (String.extract (s, 1, NONE), date)
                 else
                     doFmt (s, date)
fun toString date = doFmt ("%a %b %d %H:%M:%S %Y", date)
end (* structure Date *)
end (* structure TimeImpl *)
end; (* local *)
structure Time = TimeImpl.Time;
signature DATE = sig
    datatype weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun
    datatype month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
    type date
    exception Date
    val date : { year : int, month : month, day : int, hour : int, minute : int, second : int, offset : Time.time option } -> date
    val year : date -> int
    val month : date -> month
    val day : date -> int
    val hour : date -> int
    val minute : date -> int
    val second : date -> int
    val weekDay : date -> weekday
    val yearDay : date -> int
    val offset : date -> Time.time option
    val isDst : date -> bool option
    val localOffset : unit -> Time.time
    val fromTimeLocal : Time.time -> date
    val fromTimeUniv : Time.time -> date
    val toTime : date -> Time.time
    val compare : date * date -> order
    val fmt : string -> date -> string
    val toString : date -> string
    (*
    val scan : (char, 'a) StringCvt.reader -> (date, 'a) StringCvt.reader
    val fromString : string -> date option
    *)
end;
structure Date : DATE = TimeImpl.Date;
