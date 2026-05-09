structure Real = struct
type real = real
fun abs x = _primCall "Real.abs" (x)
fun ~ x = _primCall "Real.~" (x)
fun x + y = _primCall "Real.+" (x, y)
fun x - y = _primCall "Real.-" (x, y)
fun x * y = _primCall "Real.*" (x, y)
fun x / y = _primCall "Real./" (x, y)
fun x < y = _primCall "Real.<" (x, y)
fun x <= y = _primCall "Real.<=" (x, y)
fun x > y = _primCall "Real.>" (x, y)
fun x >= y = _primCall "Real.>=" (x, y)
end
_overload "Real" [real] { + = Real.+
                        , - = Real.-
                        , * = Real.*
                        , / = Real./
                        , abs = Real.abs
                        , ~ = Real.~
                        , < = Real.<
                        , <= = Real.<=
                        , > = Real.>
                        , >= = Real.>=
                        };
