val f = fn x => let datatype t = T of 'a
                    type u = 'a
                in case T x of T y => y
                end;
print (f "Hello world!\n");
print (Int.toString (f 42) ^ "\n");
