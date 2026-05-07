nonfix div mod
structure export = struct
fun div (x : int, y : int) = Int.div (x, y)
fun mod (x : int, y : int) = Int.mod (x, y)
fun quot (x : int, y : int) = Int.quot (x, y)
fun rem (x : int, y : int) = Int.rem (x, y)
end;
