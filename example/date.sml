fun monthToString Date.Jan = "Jan"
  | monthToString Date.Feb = "Feb"
  | monthToString Date.Mar = "Mar"
  | monthToString Date.Apr = "Apr"
  | monthToString Date.May = "May"
  | monthToString Date.Jun = "Jun"
  | monthToString Date.Jul = "Jul"
  | monthToString Date.Aug = "Aug"
  | monthToString Date.Sep = "Sep"
  | monthToString Date.Oct = "Oct"
  | monthToString Date.Nov = "Nov"
  | monthToString Date.Dec = "Dec"
fun weekdayToString Date.Mon = "Mon"
  | weekdayToString Date.Tue = "Tue"
  | weekdayToString Date.Wed = "Wed"
  | weekdayToString Date.Thu = "Thu"
  | weekdayToString Date.Fri = "Fri"
  | weekdayToString Date.Sat = "Sat"
  | weekdayToString Date.Sun = "Sun"
fun dumpDate d = ( print ("year=" ^ Int.toString (Date.year d) ^ "\n")
                 ; print ("month=" ^ monthToString (Date.month d) ^ "\n")
                 ; print ("day=" ^ Int.toString (Date.day d) ^ "\n")
                 ; print ("hour=" ^ Int.toString (Date.hour d) ^ "\n")
                 ; print ("minute=" ^ Int.toString (Date.minute d) ^ "\n")
                 ; print ("second=" ^ Int.toString (Date.second d) ^ "\n")
                 ; print ("weekDay=" ^ weekdayToString (Date.weekDay d) ^ "\n")
                 ; print ("yearDay=" ^ Int.toString (Date.yearDay d) ^ "\n")
                 ; print ("offset=" ^ (case Date.offset d of NONE => "NONE" | SOME f => Time.toString f) ^ "\n")
                 ; print ("isDst=" ^ (case Date.isDst d of NONE => "NONE" | SOME b => Bool.toString b) ^ "\n")
                 );
val () = dumpDate (Date.date { year = 2023, month = Date.Jul, day = 19, hour = 21, minute = 42, second = 60, offset = NONE });
val () = dumpDate (Date.date { year = 2000, month = Date.Dec, day = 31, hour = 23, minute = 59, second = 59, offset = NONE });
val () = dumpDate (Date.date { year = 2020, month = Date.Dec, day = 31, hour = 23, minute = 59, second = 59, offset = NONE });
val () = dumpDate (Date.date { year = 2100, month = Date.Dec, day = 31, hour = 23, minute = 59, second = 59, offset = NONE });
val now = Time.now ();
val () = dumpDate (Date.fromTimeLocal now);
val () = print (Date.toString (Date.fromTimeLocal now) ^ "\n");
val () = print (Date.fmt "%a %b %d %p %I:%M:%S %Y %Z" (Date.fromTimeLocal now) ^ "\n");
val () = dumpDate (Date.fromTimeUniv now);
val () = print (Date.toString (Date.fromTimeUniv now) ^ "\n");
val () = print (Date.fmt "%a %b %d %p %I:%M:%S %Y %Z" (Date.fromTimeUniv now) ^ "\n");
print ("localOffset=" ^ Time.toString (Date.localOffset ()) ^ "\n");
