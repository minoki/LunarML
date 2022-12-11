datatype bin = O | I
structure Bin = struct
fun O + O = O
  | O + I = I
  | I + O = I
  | I + I = O
val op - = op +
fun O * _ = O
  | _ * O = O
  | I * I = I
fun id x = x
fun _ div O = raise Div
  | x div I = x
fun _ mod O = raise Div
  | x mod I = O
fun O < I = true
  | _ < _ = false
fun x <= y = x < y orelse x = y
fun x > y = y < x
fun x >= y = x > y orelse x = y
fun toString O = "O"
  | toString I = "I"
fun fromInt x = if Int.mod (x, 2) = 0 then
                    O
                else
                    I
end
_overload "Int" [bin] { + = Bin.+
                      , - = Bin.-
                      , * = Bin.*
                      , div = Bin.div
                      , mod = Bin.mod
                      , abs = Bin.id
                      , ~ = Bin.id
                      , < = Bin.<
                      , <= = Bin.<=
                      , > = Bin.>
                      , >= = Bin.>=
                      , fromInt = Bin.fromInt
                      , minInt = 0
                      , maxInt = 1
                      }
val () = print (Bin.toString (I + O) ^ "\n");
val () = print (Bin.toString (I * O) ^ "\n");
val () = print (Bin.toString (I * I) ^ "\n");
val () = print (Bin.toString (I + I) ^ "\n");
val () = print (Bin.toString (O div I) ^ "\n");
val () = print (Bool.toString (O < I) ^ "\n");
val () = print (Bool.toString (O >= I) ^ "\n");
