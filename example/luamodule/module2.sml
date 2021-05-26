structure export = struct
val print = 123 (* shadowed and not exported *)
val hello = "Hello world!"
val print = TextIO.print
fun add (x, y) = x + y
val fun' = "fun!"
end;
