type s = char
type t = int
datatype u = U1 of s * t | U2
withtype s = s * t * u
     and t = string * s * t * u
val s : s = (#"a", 42, U2)
val t : t = ("foo", #"b", 37, U2)
val _ : u = U1 (s, t);
