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
    (* val appi *)
    (* val app *)
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
end;

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

(* structure Word8Vector :> MONO_VECTOR where type elem = Word8.word *)

structure CharVector :> MONO_VECTOR where type vector = String.string
                                    where type elem = char
  = struct
  type vector = String.string
  type elem = char
  val maxLen = String.maxSize
  val fromList = String.implode
  fun tabulate (n, f) = if n < 0 then
                            raise Size
                        else
                            let val s = JavaScript.new JavaScript.Lib.Uint8Array #[JavaScript.fromInt n]
                                fun go i = if i < n then
                                               JavaScript.set (s, JavaScript.fromInt i, JavaScript.unsafeToValue (f i : elem))
                                           else
                                               ()
                            in go 0
                             ; JavaScript.unsafeFromValue s : vector
                            end
  val length = String.size
  val sub = String.sub
  fun update (v, i, x) = tabulate (length v, fn j => if i = j then x else sub (v, j))
  val concat = String.concat
  val map = String.map
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
