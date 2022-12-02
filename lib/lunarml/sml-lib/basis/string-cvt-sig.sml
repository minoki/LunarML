signature STRING_CVT = sig
    datatype radix = BIN | OCT | DEC | HEX
    datatype realfmt = SCI of int option
                     | FIX of int option
                     | GEN of int option
           | EXACT
    type ('a, 'b) reader = 'b -> ('a * 'b) option
    val padLeft : char -> int -> string -> string
    val padRight : char -> int -> string -> string
    val splitl : (char -> bool) -> (char, 'a) reader -> 'a -> string * 'a
    val takel : (char -> bool) -> (char, 'a) reader -> 'a -> string
    val dropl : (char -> bool) -> (char, 'a) reader -> 'a -> 'a
    val skipWS : (char, 'a) reader -> 'a -> 'a
    type cs
    val scanString : ((char, cs) reader -> ('a, cs) reader) -> string -> 'a option
end;
