structure Substring :> sig
              type substring
              type char = char
              type string = string
              val sub : substring * int -> char
              val size : substring -> int
              val base : substring -> string * int * int
              val full : string -> substring
              val string : substring -> string
              val isEmpty : substring -> bool
              val getc : substring -> (char * substring) option
              val first : substring -> char option
              val triml : int -> substring -> substring
              val trimr : int -> substring -> substring
              val slice : substring * int * int option -> substring
              val concat : substring list -> string
              val concatWith : string -> substring list -> string
              val isPrefix : string -> substring -> bool
              (* val isSuffix : string -> substring -> bool *)
              val compare : substring * substring -> order
              val splitl : (char -> bool) -> substring -> substring * substring
              val splitr : (char -> bool) -> substring -> substring * substring
              val dropl : (char -> bool) -> substring -> substring
              val dropr : (char -> bool) -> substring -> substring
              val takel : (char -> bool) -> substring -> substring
              val taker : (char -> bool) -> substring -> substring
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
fun isEmpty (s, i, z) = z = 0
fun getc (s, i, z) = if z = 0 then
                         NONE
                     else
                         SOME (String.sub (s, i), (s, i + 1, z - 1))
fun first (s, i, z) = if z = 0 then
                         NONE
                     else
                         SOME (String.sub (s, i))
fun triml k = if k < 0 then
                  raise Subscript
              else
                  fn (base, start, length) => if k < length then
                                                  (base, start + k, length - k)
                                              else
                                                  (base, start + length, 0)
fun trimr k = if k < 0 then
                  raise Subscript
              else
                  fn (base, start, length) => if k < length then
                                                  (base, start, length - k)
                                              else
                                                  (base, start, 0)
fun slice ((base, start, length), i, NONE) = if 0 <= i andalso i <= length then
                                                 (base, start + i, length - i)
                                             else
                                                 raise Subscript
  | slice ((base, start, length), i, SOME n) = if 0 <= i andalso 0 <= n andalso i + n <= length then
                                                   (base, start + i, n)
                                               else
                                                   raise Subscript
fun concat xs = String.concat (List.map string xs)
fun concatWith s xs = String.concatWith s (List.map string xs)
fun isPrefix s ss = String.isPrefix s (string ss)
(* fun isSuffix s ss = String.isSuffix s (string ss) *)
fun compare (s, t) = String.compare (string s, string t)
fun splitl f (s as (base, i, z)) = let fun loop j = if j >= z then
                                                        (s, (base, i + j, 0))
                                                    else if f (sub (s, j)) then
                                                        loop (j + 1)
                                                    else
                                                        let val j' = j - 1
                                                        in ((base, i, j'), (base, i + j', z - j'))
                                                        end
                                   in loop 0
                                   end
fun splitr f (s as (base, i, z)) = let fun loop j = if j < 0 then
                                                        ((base, i, 0), s)
                                                    else if f (sub (s, j)) then
                                                        loop (j - 1)
                                                    else
                                                        let val j' = j + 1
                                                        in ((base, i, j'), (base, i + j', z - j'))
                                                        end
                                   in loop (z - 1)
                                   end
fun dropl p s = #2 (splitl p s)
fun dropr p s = #1 (splitr p s)
fun takel p s = #1 (splitl p s)
fun taker p s = #2 (splitr p s)
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
