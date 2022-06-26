signature MONO_ARRAY = sig
    (*eq*)type array
    type elem
    type vector
    val maxLen : int
    val array : int * elem -> array
    val fromList : elem list -> array
    val tabulate : int * (int -> elem) -> array
    val length : array -> int
    val sub : array * int -> elem
    val update : array * int * elem -> unit
    val vector : array -> vector
    val appi : (int * elem -> unit) -> array -> unit
    val app : (elem -> unit) -> array -> unit
end;

signature MONO_ARRAY_SLICE = sig
    type elem
    type array
    type slice
    type vector
    type vector_slice
    val length : slice -> int
    val sub : slice * int -> elem
    val update : slice * int * elem -> unit
    val full : array -> slice
    val slice : array * int * int option -> slice
    val subslice : slice * int * int option -> slice
    val vector : slice -> vector
    val copy : { src : slice, dst : array, di : int } -> unit
end;

structure CharArrayAndArraySlice :> sig
              structure CharArray : MONO_ARRAY where type vector = CharVector.vector
                                               where type elem = char
              structure CharArraySlice : MONO_ARRAY_SLICE where type vector = CharVector.vector
                                                          where type vector_slice = CharVectorSlice.slice
                                                          where type array = CharArray.array
                                                          where type elem = char
          end
= struct
val Uint8Array = JavaScript.Lib.Uint8Array
structure CharArray = struct
type array = JavaScript.value (* TODO: equality *)
type elem = char
type vector = CharVector.vector
val maxLen = 0x7fffffff
fun array (n : int, x : elem) : array
    = if n < 0 then
          raise Size
      else
          let val a = JavaScript.new Uint8Array #[JavaScript.fromInt n]
          in JavaScript.method (a, "fill") #[JavaScript.unsafeToValue x]
           ; a
          end
fun fromList (xs : elem list) : array
    = let val n = List.length xs
          val a = JavaScript.new Uint8Array #[JavaScript.fromInt n]
          fun go (i, []) = ()
            | go (i, x :: xs) = ( JavaScript.set (a, JavaScript.fromInt i, JavaScript.unsafeToValue x)
                                ; go (i + 1, xs)
                                )
      in go (0, xs)
       ; a
      end
fun tabulate (n : int, f : int -> elem) : array
    = if n < 0 then
          raise Size
      else
          let val s = JavaScript.new Uint8Array #[JavaScript.fromInt n]
              fun go i = if i < n then
                             JavaScript.set (s, JavaScript.fromInt i, JavaScript.unsafeToValue (f i))
                         else
                             ()
          in go 0
           ; s
          end
fun length (a : array) : int = JavaScript.unsafeFromValue (JavaScript.field (a : array, "length"))
fun sub (a, i : int) : elem
    = if i < 0 orelse i >= length a then
          raise Subscript
      else
          JavaScript.unsafeFromValue (JavaScript.sub (a, JavaScript.fromInt i))
fun update (a, i : int, x : elem)
    = if i < 0 orelse i >= length a then
          raise Subscript
      else
          JavaScript.set (a, JavaScript.fromInt i, JavaScript.unsafeToValue x)
fun vector (a : array) : vector = JavaScript.unsafeFromValue (JavaScript.new Uint8Array #[JavaScript.unsafeToValue a])
fun appi (f : int * elem -> unit) (a : array) : unit
    = let val n = length a
          fun go i = if i >= n then
                         ()
                     else
                         ( f (i, JavaScript.unsafeFromValue (JavaScript.sub (a, JavaScript.fromInt i)))
                         ; go (i + 1)
                         )
      in go 0
      end
fun app (f : elem -> unit) (a : array) : unit
    = let val n = length a
          fun go i = if i >= n then
                         ()
                     else
                         ( f (JavaScript.unsafeFromValue (JavaScript.sub (a, JavaScript.fromInt i)))
                         ; go (i + 1)
                         )
      in go 0
      end
end
structure CharArraySlice = struct
type elem = char
type array = JavaScript.value (* TODO: equality *)
type slice = { base : array, start : int, length : int }
type vector = CharVector.vector
type vector_slice = CharVectorSlice.slice
fun length ({ length = n, ... } : slice) = n
fun sub ({ base, start, length } : slice, i) = if 0 <= i andalso i < length then
                                                   CharArray.sub (base, start + i)
                                               else
                                                   raise Subscript
fun update ({ base, start, length } : slice, i, x) = if 0 <= i andalso i < length then
                                                         CharArray.update (base, start + i)
                                                     else
                                                         raise Subscript
fun full (a : array) : slice = { base = a, start = 0, length = CharArray.length a }
fun slice (a, i, NONE) = if 0 <= i andalso i <= CharArray.length a then
                             { base = a, start = i, length = CharArray.length a - i }
                         else
                             raise Subscript
  | slice (a, i, SOME n) = if 0 <= i andalso 0 <= n andalso i + n <= CharArray.length a then
                               { base = a, start = i, length = n }
                           else
                               raise Subscript
fun subslice ({ base, start, length }, i, NONE) = if 0 <= i andalso i <= length then
                                                      { base = base, start = start + i, length = length - i }
                                                  else
                                                      raise Subscript
  | subslice ({ base, start, length }, i, SOME n) = if 0 <= i andalso 0 <= n andalso i + n <= length then
                                                        { base = base, start = start + i, length = n }
                                                    else
                                                        raise Subscript
fun vector ({ base, start, length } : slice) = CharVector.tabulate (length, fn i => CharArray.sub (base, start + i))
fun copy { src = { base, start, length }, dst, di }
    = let fun forward i = if i >= length then
                              ()
                          else
                              ( CharArray.update (dst, di + i, CharArray.sub (base, start + i))
                              ; forward (i + 1)
                              )
          fun backward i = if i < 0 then
                               ()
                           else
                               ( CharArray.update (dst, di + i, CharArray.sub (base, start + i))
                               ; backward (i - 1)
                               )
      in if start >= di then
             forward 0
         else
             backward (length - 1)
      end
end
end;
open CharArrayAndArraySlice;
