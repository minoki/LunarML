signature MONO_VECTOR = sig
    type vector
    type elem
    val maxLen : int
    val fromList : elem list -> vector
    val tabulate : int * (int -> elem) -> vector
    val length : vector -> int
    val sub : vector * int -> elem
    val update : vector * int * elem -> vector
    val concat : vector list -> vector
    val appi : (int * elem -> unit) -> vector -> unit
    val app : (elem -> unit) -> vector -> unit
    val map : (elem -> elem) -> vector -> vector
    (* val mapi *)
    val foldli : (int * elem * 'a -> 'a) -> 'a -> vector -> 'a
    val foldri : (int * elem * 'a -> 'a) -> 'a -> vector -> 'a
    val foldl : (elem * 'a -> 'a) -> 'a -> vector -> 'a
    val foldr : (elem * 'a -> 'a) -> 'a -> vector -> 'a
    (* val findi *)
    (* val find *)
    val exists : (elem -> bool) -> vector -> bool
    val all : (elem -> bool) -> vector -> bool
    (* val collate *)
end

signature MONO_VECTOR_SLICE = sig
    type elem
    type vector
    type slice
    val length : slice -> int
    val sub : slice * int -> elem
    val full : vector -> slice
    val slice : vector * int * int option -> slice
    val subslice : slice * int * int option -> slice
    val base : slice -> vector * int * int
    val vector : slice -> vector
    val isEmpty : slice -> bool
    val getItem : slice -> (elem * slice) option
    val exists : (elem -> bool) -> slice -> bool
    val all : (elem -> bool) -> slice -> bool
end

signature WORD8_VECTOR_EXTRA = sig
    include MONO_VECTOR where type elem = Word8.word
    val bytesToString : vector -> string
    val stringToBytes : string -> vector
end

structure Word8VectorExtra :> WORD8_VECTOR_EXTRA
  = struct
  type vector = String.string
  type elem = Word8.word
  val maxLen = String.maxSize
  fun fromList xs = String.implode (List.map (fn x => Char.chr (Word8.toInt x)) xs)
  fun tabulate (n, f) = let val t = Lua.unsafeToValue (Vector.tabulate (n, Char.chr o Word8.toInt o f) : char Vector.vector)
                            val result = Vector.sub (Lua.call Lua.Lib.table.concat #[t], 0)
                        in Lua.unsafeFromValue result : string
                        end
  val length = String.size
  fun sub (x, i) = Word8.fromInt (Char.ord (String.sub (x, i)))
  fun update (v, i, x) = tabulate (length v, fn j => if i = j then x else sub (v, j))
  val concat = String.concat
  fun map f = String.map (fn x => Char.chr (Word8.toInt (f (Word8.fromInt (Char.ord x)))))
  fun appi f v = let val n = length v
                     fun loop i = if i >= n then
                                      ()
                                  else
                                      (f (i, sub (v, i)) : unit; loop (i + 1))
                 in loop 0
                 end
  fun app f v = let val n = length v
                    fun loop i = if i >= n then
                                     ()
                                 else
                                     (f (sub (v, i)) : unit; loop (i + 1))
                in loop 0
                end
  fun foldli f init v = let val len = length v
                            fun loop (i, acc) = if i >= len then
                                                    acc
                                                else
                                                    loop (i + 1, f (i, sub (v, i), acc))
                        in loop (0, init)
                        end
  fun foldri f init v = let fun loop (i, acc) = if i < 0 then
                                                    acc
                                                else
                                                    loop (i - 1, f (i, sub (v, i), acc))
                        in loop (length v - 1, init)
                        end
  fun foldl f init v = let val len = length v
                           fun loop (i, acc) = if i >= len then
                                                   acc
                                               else
                                                   loop (i + 1, f (sub (v, i), acc))
                       in loop (0, init)
                       end
  fun foldr f init v = let fun loop (i, acc) = if i < 0 then
                                                   acc
                                               else
                                                   loop (i - 1, f (sub (v, i), acc))
                       in loop (length v - 1, init)
                       end
  local fun go (f, v, i) = if i < length v then
                               f (sub (v, i)) orelse go (f, v, i + 1)
                           else
                               false
  in
  fun exists f v = go (f, v, 0)
  end
  local fun go (f, v, i) = if i < length v then
                             f (sub (v, i)) andalso go (f, v, i + 1)
                         else
                             true
  in
  fun all f v = go (f, v, 0)
  end
  fun bytesToString x = x
  fun stringToBytes x = x
end

structure Word8Vector : MONO_VECTOR where type elem = Word8.word = Word8VectorExtra

structure Word8VectorSlice :> MONO_VECTOR_SLICE where type elem = Word8.word
                                                where type vector = Word8Vector.vector
= struct
type elem = Word8.word
type vector = Word8Vector.vector
type slice = { base : vector, start : int, length : int }
val length : slice -> int = #length
fun sub ({ base, start, length }, i) = if 0 <= i andalso i < length then
                                           Word8Vector.sub (base, start + i)
                                       else
                                           raise Subscript
fun full a = { base = a, start = 0, length = Word8Vector.length a }
fun slice (a, i, NONE) = if 0 <= i andalso i <= Word8Vector.length a then
                             { base = a, start = i, length = Word8Vector.length a - i }
                         else
                             raise Subscript
  | slice (a, i, SOME n) = if 0 <= i andalso 0 <= n andalso i + n <= Word8Vector.length a then
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
fun base { base = b, start, length } = (b, start, length)
fun vector { base, start, length } = Word8Vector.tabulate (length, fn i => Word8Vector.sub (base, start + i))
fun isEmpty { base, start, length } = length = 0
fun getItem { base, start, length } = if length > 0 then
                                          SOME (Word8Vector.sub (base, start), { base = base, start = start + 1, length = length - 1 })
                                      else
                                          NONE
fun exists f { base, start, length } = let fun loop i = if i >= length then
                                                            false
                                                        else
                                                            if f (Word8Vector.sub (base, start + i)) then
                                                                true
                                                            else
                                                                loop (i + 1)
                                       in loop start
                                       end
fun all f { base, start, length } = let fun loop i = if i >= length then
                                                         true
                                                     else
                                                         if not (f (Word8Vector.sub (base, start + i))) then
                                                             false
                                                         else
                                                             loop (i + 1)
                                    in loop start
                                    end
end

structure CharVector :> MONO_VECTOR where type vector = String.string
                                    where type elem = char
  = struct
  type vector = String.string
  type elem = char
  val maxLen = String.maxSize
  val fromList = String.implode
  fun tabulate (n, f) = let val t = Lua.unsafeToValue (Vector.tabulate (n, f) : char Vector.vector)
                            val result = Vector.sub (Lua.call Lua.Lib.table.concat #[t], 0)
                        in Lua.unsafeFromValue result : string
                        end
  val length = String.size
  val sub = String.sub
  fun update (v, i, x) = tabulate (length v, fn j => if i = j then x else sub (v, j))
  val concat = String.concat
  val map = String.map
  fun appi f v = let val n = length v
                     fun loop i = if i >= n then
                                      ()
                                  else
                                      (f (i, sub (v, i)) : unit; loop (i + 1))
                 in loop 0
                 end
  fun app f v = let val n = length v
                    fun loop i = if i >= n then
                                     ()
                                 else
                                     (f (sub (v, i)) : unit; loop (i + 1))
                in loop 0
                end
  fun foldli f init v = let val len = length v
                            fun loop (i, acc) = if i >= len then
                                                    acc
                                                else
                                                    loop (i + 1, f (i, sub (v, i), acc))
                        in loop (0, init)
                        end
  fun foldri f init v = let fun loop (i, acc) = if i < 0 then
                                                    acc
                                                else
                                                    loop (i - 1, f (i, sub (v, i), acc))
                        in loop (length v - 1, init)
                        end
  fun foldl f init v = let val len = length v
                           fun loop (i, acc) = if i >= len then
                                                   acc
                                               else
                                                   loop (i + 1, f (sub (v, i), acc))
                       in loop (0, init)
                       end
  fun foldr f init v = let fun loop (i, acc) = if i < 0 then
                                                   acc
                                               else
                                                   loop (i - 1, f (sub (v, i), acc))
                       in loop (length v - 1, init)
                       end
  local fun go (f, v, i) = if i < length v then
                               f (sub (v, i)) orelse go (f, v, i + 1)
                           else
                               false
  in
  fun exists f v = go (f, v, 0)
  end
  local fun go (f, v, i) = if i < length v then
                             f (sub (v, i)) andalso go (f, v, i + 1)
                         else
                             true
  in
  fun all f v = go (f, v, 0)
  end
end;
