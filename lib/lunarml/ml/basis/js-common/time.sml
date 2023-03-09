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
