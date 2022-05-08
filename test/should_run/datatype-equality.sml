datatype ('a,'b,'c) foo = A of 'a
                        | B of 'b
                        | C of 'c
val x = A (vector [1,2,3]) : (int vector, string, char) foo
val y = A (vector [1,2,3]) : (int vector, string, char) foo
val z = B "zzz" : (int vector, string, char) foo;
print (Bool.toString (x = y) ^ "\n");
print (Bool.toString (y = z) ^ "\n");
print (Bool.toString (x = z) ^ "\n");
