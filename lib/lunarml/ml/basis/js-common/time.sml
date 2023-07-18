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
structure Time :> TIME = struct
type time = Int54.int (* in milliseconds *)
exception Time
val zeroTime : time = 0
fun fromReal (x : LargeReal.real) : time = Int54.fromLarge (LargeReal.toLargeInt IEEEReal.TO_ZERO (x * 1e3)) handle _ => raise Time
fun toReal (x : time) : LargeReal.real = LargeReal.fromLargeInt (Int54.toLarge x) / 1e3
local
    val Iquot = Int54.quot
    val Lquot = LargeInt.quot
    infix 7 Lquot Iquot
in
fun toSeconds (x : time) = Int54.toLarge (x Iquot 1000)
fun toMilliseconds (x : time) = Int54.toLarge x
fun toMicroseconds (x : time) = Int54.toLarge x * 1000
fun toNanoseconds (x : time) = Int54.toLarge x * 1000_000
fun fromSeconds (x : LargeInt.int) : time = Int54.fromLarge x * 1000 handle Overflow => raise Time
fun fromMilliseconds (x : LargeInt.int) : time = Int54.fromLarge x handle Overflow => raise Time
fun fromMicroseconds (x : LargeInt.int) : time = Int54.fromLarge (x Lquot 1000) handle Overflow => raise Time
fun fromNanoseconds (x : LargeInt.int) : time = Int54.fromLarge (x Lquot 1000_000) handle Overflow => raise Time
end
val compare = Int54.compare
fun now () : time = JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.Date.now #[])
fun fmt (n : int) (t : time) : string
    = if n < 0 then
          raise Size
      else
          let val x = toReal t
          in JavaScript.encodeUtf8 (JavaScript.unsafeFromValue (JavaScript.method (JavaScript.fromReal x, "toFixed") #[JavaScript.fromInt n]) : WideString.string) (* TODO: sign *)
          end
fun toString t = fmt 3 t
fun x + y = Int54.+ (x, y) handle Overflow => raise Time
fun x - y = Int54.- (x, y) handle Overflow => raise Time
val op < = Int54.<
val op <= = Int54.<=
val op > = Int54.>
val op >= = Int54.>=
end;
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
structure Date :> DATE = struct
datatype weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun
datatype month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
type date = { year : int
            , month : int (* 0..11 *)
            , day : int (* 1..31 *)
            , hour : int
            , minute : int
            , second : int
            , weekDay : int (* 0 = Sun *)
            , offset : Time.time option
            , time : Time.time
            }
exception Date
fun monthToZeroBasedInt Jan = 0
  | monthToZeroBasedInt Feb = 1
  | monthToZeroBasedInt Mar = 2
  | monthToZeroBasedInt Apr = 3
  | monthToZeroBasedInt May = 4
  | monthToZeroBasedInt Jun = 5
  | monthToZeroBasedInt Jul = 6
  | monthToZeroBasedInt Aug = 7
  | monthToZeroBasedInt Sep = 8
  | monthToZeroBasedInt Oct = 9
  | monthToZeroBasedInt Nov = 10
  | monthToZeroBasedInt Dec = 11
fun fromJavaScript (t : JavaScript.value, offset : Time.time option) : date
    = { year = JavaScript.unsafeFromValue (JavaScript.method (t, "getFullYear") #[])
      , month = JavaScript.unsafeFromValue (JavaScript.method (t, "getMonth") #[])
      , day = JavaScript.unsafeFromValue (JavaScript.method (t, "getDate") #[])
      , hour = JavaScript.unsafeFromValue (JavaScript.method (t, "getHours") #[])
      , minute = JavaScript.unsafeFromValue (JavaScript.method (t, "getMinutes") #[])
      , second = JavaScript.unsafeFromValue (JavaScript.method (t, "getSeconds") #[])
      , weekDay = JavaScript.unsafeFromValue (JavaScript.method (t, "getDay") #[])
      , offset = offset
      , time = JavaScript.unsafeFromValue (JavaScript.method (t, "getTime") #[])
      }
fun fromJavaScriptUTC (t : JavaScript.value, offset : Time.time option) : date
    = { year = JavaScript.unsafeFromValue (JavaScript.method (t, "getUTCFullYear") #[])
      , month = JavaScript.unsafeFromValue (JavaScript.method (t, "getUTCMonth") #[])
      , day = JavaScript.unsafeFromValue (JavaScript.method (t, "getUTCDate") #[])
      , hour = JavaScript.unsafeFromValue (JavaScript.method (t, "getUTCHours") #[])
      , minute = JavaScript.unsafeFromValue (JavaScript.method (t, "getUTCMinutes") #[])
      , second = JavaScript.unsafeFromValue (JavaScript.method (t, "getUTCSeconds") #[])
      , weekDay = JavaScript.unsafeFromValue (JavaScript.method (t, "getUTCDay") #[])
      , offset = offset
      , time = JavaScript.unsafeFromValue (JavaScript.method (t, "getTime") #[])
      }
fun date { year : int, month : month, day : int, hour : int, minute : int, second : int, offset : Time.time option }
    = let val d = JavaScript.new JavaScript.Lib.Date #[JavaScript.fromInt year, JavaScript.fromInt (monthToZeroBasedInt month), JavaScript.fromInt day, JavaScript.fromInt hour, JavaScript.fromInt minute, JavaScript.fromInt second]
      in fromJavaScript (d, offset)
      end
fun year (d : date) : int = #year d
fun monthAsZeroBasedInt (d : date) : int = #month d
val months = #[Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec]
fun month t : month = Unsafe.Vector.sub (months, monthAsZeroBasedInt t)
fun day (d : date) : int = #day d
fun hour (d : date) : int = #hour d
fun minute (d : date) : int = #minute d
fun second (d : date) : int = #second d
fun weekDayWithSundayZero (d : date) : int = #weekDay d
val weekDays = #[Sun, Mon, Tue, Wed, Thu, Fri, Sat]
fun weekDay (d : date) : weekday = Unsafe.Vector.sub (weekDays, weekDayWithSundayZero d)
val yearDayOffsets = #[ ~1
                      , 31 - 1
                      , 31 + 28 - 1
                      , 31 + 28 + 31 - 1
                      , 31 + 28 + 31 + 30 - 1
                      , 31 + 28 + 31 + 30 + 31 - 1
                      , 31 + 28 + 31 + 30 + 31 + 30 - 1
                      , 31 + 28 + 31 + 30 + 31 + 30 + 31 - 1
                      , 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 - 1
                      , 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 - 1
                      , 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31 - 1
                      , 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31 + 30 - 1
                      ]
fun yearDay (d : date) : int = let val yr = year d
                                   val mh = monthAsZeroBasedInt d
                                   val dy = day d
                                   val x = dy + Unsafe.Vector.sub (yearDayOffsets, monthAsZeroBasedInt d)
                               in if mh >= 2 andalso (yr .Int.rem. 4) = 0 andalso ((yr .Int.rem. 100) <> 0 orelse (yr .Int.rem. 400) = 0) then (* leap year *)
                                      x + 1
                                  else
                                      x
                               end
fun offset (d : date) = #offset d
fun isDst (_ : date) : bool option = NONE
fun localOffset () : Time.time = let val min : Int54.int = JavaScript.unsafeFromValue (JavaScript.method (JavaScript.new JavaScript.Lib.Date #[], "getTimezoneOffset") #[])
                                 in Time.fromSeconds (Int54.toLarge min * 60)
                                 end
fun fromTimeLocal (t : Time.time) : date = fromJavaScript (JavaScript.new JavaScript.Lib.Date #[JavaScript.unsafeToValue (Int54.fromLarge (Time.toMilliseconds t))], NONE)
fun fromTimeUniv (t : Time.time) : date = fromJavaScriptUTC (JavaScript.new JavaScript.Lib.Date #[JavaScript.unsafeToValue (Int54.fromLarge (Time.toMilliseconds t))], SOME Time.zeroTime)
fun toTime (d : date) : Time.time = let val t = #time d
                                    in case #offset d of
                                           NONE => t
                                         | SOME u => Time.+ (t, u)
                                    end
fun compare (date1, date2) = case Int.compare (year date1, year date2) of
                                 EQUAL => (case Int.compare (monthAsZeroBasedInt date1, monthAsZeroBasedInt date2) of
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
local
    val abbreviatedMonthString = #["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]
    val fullMonthString = #["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    val abbreviatedWeekdayString = #["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]
    val fullWeekdayString = #["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"]
    fun asTwoDigits x = if x < 10 then "0" ^ Int.toString x else Int.toString x
    fun asThreeDigits x = if x < 10 then "00" ^ Int.toString x else if x < 100 then "0" ^ Int.toString x else Int.toString x
    fun hms date = asTwoDigits (hour date) ^ ":" ^ asTwoDigits (minute date) ^ ":" ^ asTwoDigits (second date)
    (* Try to mimick "C" locale *)
    fun formatOne (#"a", date) = Unsafe.Vector.sub (abbreviatedWeekdayString, weekDayWithSundayZero date)
      | formatOne (#"A", date) = Unsafe.Vector.sub (fullWeekdayString, weekDayWithSundayZero date)
      | formatOne (#"b", date) = Unsafe.Vector.sub (abbreviatedMonthString, monthAsZeroBasedInt date)
      | formatOne (#"B", date) = Unsafe.Vector.sub (fullMonthString, monthAsZeroBasedInt date)
      | formatOne (#"c", date) = let val a = Unsafe.Vector.sub (abbreviatedWeekdayString, weekDayWithSundayZero date)
                                     val b = Unsafe.Vector.sub (abbreviatedMonthString, monthAsZeroBasedInt date)
                                     val d = day date
                                     val e = if d < 10 then " " ^ Int.toString d else Int.toString d
                                     val T = hms date
                                     val Y = Int.toString (year date)
                                 in a ^ " " ^ b ^ " " ^ e ^ " " ^ T ^ " " ^ Y (* "%a %b %e %H:%M:%S %Y" *)
                                 end
      | formatOne (#"d", date) = asTwoDigits (day date)
      | formatOne (#"H", date) = asTwoDigits (hour date)
      | formatOne (#"I", date) = asTwoDigits (((hour date + 11) .Int.rem. 12) + 1)
      | formatOne (#"j", date) = asThreeDigits (yearDay date + 1)
      | formatOne (#"m", date) = asTwoDigits (monthAsZeroBasedInt date + 1)
      | formatOne (#"M", date) = asTwoDigits (minute date)
      | formatOne (#"p", date) = if hour date < 12 then "AM" else "PM"
      | formatOne (#"S", date) = asTwoDigits (second date)
      | formatOne (#"U", date) = let val weekNumber = raise Fail "Date.fmt \"%U\": not implemented yet" in asTwoDigits weekNumber end
      | formatOne (#"w", date) = Int.toString (weekDayWithSundayZero date)
      | formatOne (#"W", date) = let val weekNumber = raise Fail "Date.fmt \"%W\": not implemented yet" in asTwoDigits weekNumber end
      | formatOne (#"x", date) = asTwoDigits (monthAsZeroBasedInt date + 1) ^ "/" ^ asTwoDigits (day date) ^ "/" ^ asTwoDigits (year date .Int.rem. 100) (* "%m/%d/%y" *)
      | formatOne (#"X", date) = hms date (* "%H:%M:%S" *)
      | formatOne (#"y", date) = asTwoDigits (year date .Int.rem. 100)
      | formatOne (#"Y", date) = Int.toString (year date)
      | formatOne (#"Z", date) = "" (* time zone name: unavailable *)
      | formatOne (#"%", date) = "%"
      | formatOne (c, date) = String.str c (* undefined behavior in C, but defined in SML *)
    fun doFmt (s, date, revAcc) = case Substring.getc s of
                                      NONE => String.concat (List.rev revAcc)
                                    | SOME (#"%", s) => (case Substring.getc s of
                                                             NONE => String.concat (List.rev revAcc) (* invalid format *)
                                                           | SOME (c, s) => doFmt (s, date, formatOne (c, date) :: revAcc)
                                                        )
                                    | SOME (c, s) => doFmt (s, date, String.str c :: revAcc)
in
fun fmt s date = doFmt (Substring.full s, date, [])
fun toString date = let val a = Unsafe.Vector.sub (abbreviatedWeekdayString, weekDayWithSundayZero date)
                        val b = Unsafe.Vector.sub (abbreviatedMonthString, monthAsZeroBasedInt date)
                        val d = asTwoDigits (day date)
                        val T = hms date
                        val Y = Int.toString (year date)
                    in a ^ " " ^ b ^ " " ^ d ^ " " ^ T ^ " " ^ Y (* "%a %b %d %H:%M:%S %Y" *)
                    end
end (* local *)
end; (* structure Date *)
