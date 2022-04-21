structure Bool = struct
fun not x = _primCall "Bool.not" (x)
end;

type unit = {}
datatype 'a option = NONE | SOME of 'a;

structure WideChar = struct
type char = _primType "WideChar.char"
fun x < y = _primCall "WideChar.<" (x, y)
fun x <= y = _primCall "WideChar.<=" (x, y)
fun x > y = _primCall "WideChar.>" (x, y)
fun x >= y = _primCall "WideChar.>=" (x, y)
end
_overload "Char" [WideChar.char] { < = WideChar.<
                                 , <= = WideChar.<=
                                 , > = WideChar.>
                                 , >= = WideChar.>=
                                 };

structure WideString = struct
type string = _primType "WideString.string"
type char = WideChar.char
fun x < y = _primCall "WideString.<" (x, y)
fun x <= y = _primCall "WideString.<=" (x, y)
fun x > y = _primCall "WideString.>" (x, y)
fun x >= y = _primCall "WideString.>=" (x, y)
fun x ^ y = _primCall "WideString.^" (x, y)
fun size x = _primCall "WideString.size" (x)
fun str (x : char) : string = _primCall "Unsafe.cast" (x)
end
_overload "String" [WideString.string] { < = WideString.<
                                       , <= = WideString.<=
                                       , > = WideString.>
                                       , >= = WideString.>=
                                       };

structure JavaScript : sig
              type value
              val sub : value * value -> value
              val field : value * WideString.string -> value
              val set : value * value * value -> unit
              val global : WideString.string -> value
              val call : value -> value vector -> value
              val fromBool : bool -> value
              val fromInt : int -> value
              val fromWord : word -> value
              val fromReal : real -> value
              val fromWideChar : WideChar.char -> value
              val fromWideString : WideString.string -> value
              val unsafeToValue : 'a -> value
              val unsafeFromValue : value -> 'a
          end = struct
open JavaScript (* type value, global, call *)
fun unsafeToValue x : value = _primCall "Unsafe.cast" (x)
fun unsafeFromValue (x : value) = _primCall "Unsafe.cast" (x)
val fromBool : bool -> value = unsafeToValue
val fromInt : int -> value = unsafeToValue
val fromWord : word -> value = unsafeToValue
val fromReal : real -> value = unsafeToValue
val fromWideChar : WideChar.char -> value = unsafeToValue
val fromWideString : WideString.string -> value = unsafeToValue
fun sub (obj, key) = _primCall "JavaScript.sub" (obj, key)
fun field (obj, key : WideString.string) = _primCall "JavaScript.sub" (obj, _primCall "Unsafe.cast" (key))
fun set (obj, key, value) = _primCall "JavaScript.set" (obj, key, value)
end;
