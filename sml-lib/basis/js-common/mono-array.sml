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
    val copy : { src : array, dst : array, di : int } -> unit
    val appi : (int * elem -> unit) -> array -> unit
    val app : (elem -> unit) -> array -> unit
    val modifyi : (int * elem -> elem) -> array -> unit
    val modify : (elem -> elem) -> array -> unit
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

structure Word8ArrayAndArraySlice :> sig
              structure Word8Array : MONO_ARRAY where type vector = Word8Vector.vector
                                                where type elem = Word8.word
              structure Word8ArraySlice : MONO_ARRAY_SLICE where type vector = Word8Vector.vector
                                                           where type vector_slice = Word8VectorSlice.slice
                                                           where type array = Word8Array.array
                                                           where type elem = Word8.word
              val eqWord8Array : Word8Array.array * Word8Array.array -> bool
          end
= struct
val Uint8Array = JavaScript.Lib.Uint8Array
structure Word8Array = struct
type array = JavaScript.value
type elem = Word8.word
type vector = Word8Vector.vector
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
          let val xs = List.tabulate (n, f) : elem list
              val s = JavaScript.new JavaScript.Lib.Uint8Array #[JavaScript.fromInt n]
              fun go (_, []) = ()
                | go (i, x :: xs) = ( JavaScript.set (s, JavaScript.fromInt i, JavaScript.unsafeToValue (x : elem))
                                    ; go (i + 1, xs)
                                    )
          in go (0, xs)
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
fun copy { src, dst, di } = let val srcLen = length src
                            in if 0 <= di andalso di + srcLen <= length dst then
                                   let fun loop i = if i >= srcLen then
                                                        ()
                                                    else
                                                        ( update (dst, di + i, sub (src, i))
                                                        ; loop (i + 1)
                                                        )
                                   in loop 0
                                   end
                               else
                                   raise Subscript
                            end
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
fun modifyi f arr = let val n = length arr
                        fun loop i = if i >= n then
                                         ()
                                     else
                                         let val x = sub (arr, i)
                                             val y = f (i, x)
                                         in update (arr, i, y)
                                          ; loop (i + 1)
                                         end
                    in loop 0
                    end
fun modify f arr = let val n = length arr
                       fun loop i = if i >= n then
                                        ()
                                    else
                                        let val x = sub (arr, i)
                                            val y = f x
                                        in update (arr, i, y)
                                         ; loop (i + 1)
                                        end
                   in loop 0
                   end
end
structure Word8ArraySlice = struct
type elem = Word8.word
type array = JavaScript.value
type slice = { base : array, start : int, length : int }
type vector = Word8Vector.vector
type vector_slice = Word8VectorSlice.slice
fun length ({ length = n, ... } : slice) = n
fun sub ({ base, start, length } : slice, i) = if 0 <= i andalso i < length then
                                                   Word8Array.sub (base, start + i)
                                               else
                                                   raise Subscript
fun update ({ base, start, length } : slice, i, x) = if 0 <= i andalso i < length then
                                                         Word8Array.update (base, start + i)
                                                     else
                                                         raise Subscript
fun full (a : array) : slice = { base = a, start = 0, length = Word8Array.length a }
fun slice (a, i, NONE) = if 0 <= i andalso i <= Word8Array.length a then
                             { base = a, start = i, length = Word8Array.length a - i }
                         else
                             raise Subscript
  | slice (a, i, SOME n) = if 0 <= i andalso 0 <= n andalso i + n <= Word8Array.length a then
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
fun vector ({ base, start, length } : slice) = Word8Vector.tabulate (length, fn i => Word8Array.sub (base, start + i))
fun copy { src = { base, start, length }, dst, di }
    = let fun forward i = if i >= length then
                              ()
                          else
                              ( Word8Array.update (dst, di + i, Word8Array.sub (base, start + i))
                              ; forward (i + 1)
                              )
          fun backward i = if i < 0 then
                               ()
                           else
                               ( Word8Array.update (dst, di + i, Word8Array.sub (base, start + i))
                               ; backward (i - 1)
                               )
      in if start >= di then
             forward 0
         else
             backward (length - 1)
      end
end
val eqWord8Array : Word8Array.array * Word8Array.array -> bool = JavaScript.===
end
open Word8ArrayAndArraySlice;
_equality Word8Array.array = eqWord8Array;

structure CharArrayAndArraySlice :> sig
              structure CharArray : MONO_ARRAY where type vector = CharVector.vector
                                               where type elem = char
              structure CharArraySlice : MONO_ARRAY_SLICE where type vector = CharVector.vector
                                                          where type vector_slice = CharVectorSlice.slice
                                                          where type array = CharArray.array
                                                          where type elem = char
              val eqCharArray : CharArray.array * CharArray.array -> bool
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
          let val xs = List.tabulate (n, f) : elem list
              val s = JavaScript.new JavaScript.Lib.Uint8Array #[JavaScript.fromInt n]
              fun go (_, []) = ()
                | go (i, x :: xs) = ( JavaScript.set (s, JavaScript.fromInt i, JavaScript.unsafeToValue (x : elem))
                                    ; go (i + 1, xs)
                                    )
          in go (0, xs)
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
fun copy { src, dst, di } = let val srcLen = length src
                            in if 0 <= di andalso di + srcLen <= length dst then
                                   let fun loop i = if i >= srcLen then
                                                        ()
                                                    else
                                                        ( update (dst, di + i, sub (src, i))
                                                        ; loop (i + 1)
                                                        )
                                   in loop 0
                                   end
                               else
                                   raise Subscript
                            end
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
fun modifyi f arr = let val n = length arr
                        fun loop i = if i >= n then
                                         ()
                                     else
                                         let val x = sub (arr, i)
                                             val y = f (i, x)
                                         in update (arr, i, y)
                                          ; loop (i + 1)
                                         end
                    in loop 0
                    end
fun modify f arr = let val n = length arr
                       fun loop i = if i >= n then
                                        ()
                                    else
                                        let val x = sub (arr, i)
                                            val y = f x
                                        in update (arr, i, y)
                                         ; loop (i + 1)
                                        end
                   in loop 0
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
val eqCharArray : CharArray.array * CharArray.array -> bool = JavaScript.===
end;
open CharArrayAndArraySlice;
_equality CharArray.array = eqCharArray;

signature MONO_ARRAY = sig
    eqtype array
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
    val copy : { src : array, dst : array, di : int } -> unit
    val appi : (int * elem -> unit) -> array -> unit
    val app : (elem -> unit) -> array -> unit
    val modifyi : (int * elem -> elem) -> array -> unit
    val modify : (elem -> elem) -> array -> unit
end;
structure Word8Array = Word8Array : MONO_ARRAY where type vector = Word8Vector.vector
                                               where type elem = Word8.word
structure CharArray = CharArray : MONO_ARRAY where type vector = CharVector.vector
                                             where type elem = char;
