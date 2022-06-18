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

(* structure Word8Vector :> MONO_VECTOR where type elem = Word8.word *)

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
