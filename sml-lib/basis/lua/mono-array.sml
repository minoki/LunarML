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

functor GenericMonoArrayAndArraySlice (type elem) : sig
            structure MonoArray : sig
                          type elem = elem
                          type array = elem Array.array
                          val maxLen : int
                          val array : int * elem -> array
                          val fromList : elem list -> array
                          val tabulate : int * (int -> elem) -> array
                          val length : array -> int
                          val sub : array * int -> elem
                          val update : array * int * elem -> unit
                          val copy : { src : array, dst : array, di : int } -> unit
                          val appi : (int * elem -> unit) -> array -> unit
                          val app : (elem -> unit) -> array -> unit
                          val modifyi : (int * elem -> elem) -> array -> unit
                          val modify : (elem -> elem) -> array -> unit
                      end
            structure MonoArraySlice : sig
                          type elem = elem
                          type array = elem Array.array
                          type slice = elem ArraySlice.slice
                          val length : slice -> int
                          val sub : slice * int -> elem
                          val update : slice * int * elem -> unit
                          val full : array -> slice
                          val slice : array * int * int option -> slice
                          val subslice : slice * int * int option -> slice
                          val copy : { src : slice, dst : array, di : int } -> unit
                          end
        end = struct
structure MonoArray = struct
open Array
type elem = elem
type array = elem Array.array
end
structure MonoArraySlice = struct
open ArraySlice
type elem = elem
type array = elem Array.array
type slice = elem ArraySlice.slice
end
end;

structure Word8ArrayAndArraySlice :> sig
              structure Word8Array : MONO_ARRAY where type vector = Word8Vector.vector
                                                where type elem = Word8.word
              structure Word8ArraySlice : MONO_ARRAY_SLICE where type vector = Word8Vector.vector
                                                           where type vector_slice = Word8VectorSlice.slice
                                                           where type array = Word8Array.array
                                                           where type elem = Word8.word
          end
  = struct
  structure Base = GenericMonoArrayAndArraySlice (type elem = Word8.word)
  structure Word8Array = struct
  open Base.MonoArray
  type vector = Word8Vector.vector
  fun vector (a : array) = Word8Vector.tabulate (length a, fn i => sub (a, i))
  end
  structure Word8ArraySlice = struct
  open Base.MonoArraySlice
  type vector = Word8Vector.vector
  type vector_slice = Word8VectorSlice.slice
  fun vector (a : slice) = Word8Vector.tabulate (ArraySlice.length a, fn i => ArraySlice.sub (a, i))
  end
end;
open Word8ArrayAndArraySlice;

structure CharArrayAndArraySlice :> sig
              structure CharArray : MONO_ARRAY where type vector = CharVector.vector
                                               where type elem = char
              structure CharArraySlice : MONO_ARRAY_SLICE where type vector = CharVector.vector
                                                          where type vector_slice = Substring.substring
                                                          where type array = CharArray.array
                                                          where type elem = char
          end
  = struct
  structure Base = GenericMonoArrayAndArraySlice (type elem = char)
  structure CharArray = struct
  open Base.MonoArray
  type vector = CharVector.vector
  fun vector (a : array) = let val results = Lua.call Lua.Lib.table.concat #[Lua.unsafeToValue a]
                           in Lua.unsafeFromValue (Vector.sub (results, 0)) : vector
                           end
  end
  structure CharArraySlice = struct
  open Base.MonoArraySlice
  type vector = CharVector.vector
  type vector_slice = Substring.substring
  fun vector (a : slice) = CharVector.tabulate (ArraySlice.length a, fn i => ArraySlice.sub (a, i))
  end
end;
open CharArrayAndArraySlice;
