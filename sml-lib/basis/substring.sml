structure Substring :> sig
              type substring
              type char = char
              type string = string
              val sub : substring * int -> char
              val size : substring -> int
              val base : substring -> string * int * int
              val full : string -> substring
              val string : substring -> string
              val getc : substring -> (char * substring) option
              val compare : substring * substring -> order
              val foldl : (char * 'a -> 'a) -> 'a -> substring -> 'a
              val foldr : (char * 'a -> 'a) -> 'a -> substring -> 'a
          end = struct
type char = char
type string = string
type substring = string * int * int (* the underlying string, the starting index, the size *)
fun sub ((s, i, z), j) = if 0 <= j andalso j < z then
                             String.sub (s, i + j)
                         else
                             raise Subscript
fun size (_, _, z) = z
fun base x = x
fun full s = (s, 0, String.size s)
fun string (s, i, z) = String.substring (s, i, z)
fun getc (s, i, z) = if z = 0 then
                         NONE
                     else
                         SOME (String.sub (s, i), (s, i + 1, z - 1))
fun compare (s, t) = String.compare (string s, string t)
fun foldl f init (s, i, z) = let val zz = i + z
                                 fun loop (j, acc) = if j >= zz then
                                                         acc
                                                     else
                                                         loop (j + 1, f (String.sub (s, j), acc))
                             in loop (i, init)
                             end
fun foldr f init (s, i, z) = let fun loop (j, acc) = if j < i then
                                                         acc
                                                     else
                                                         loop (j - 1, f (String.sub (s, j), acc))
                             in loop (i + z - 1, init)
                             end
end;
