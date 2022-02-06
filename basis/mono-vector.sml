signature MONO_VECTOR = sig
    type vector
    type elem
    val length : vector -> int
    val sub : vector * int -> elem
    val concat : vector list -> vector
    val map : (elem -> elem) -> vector -> vector
    val exists : (elem -> bool) -> vector -> bool
    val all : (elem -> bool) -> vector -> bool
end;

(* structure Word8Vector :> MONO_VECTOR where type elem = Word8.word *)

structure CharVector :> MONO_VECTOR where type vector = String.string
                                    where type elem = char
  = struct
  type vector = String.string
  type elem = char
  val length = String.size
  val sub = String.sub
  val concat = String.concat
  val map = String.map
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
