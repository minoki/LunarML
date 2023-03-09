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
    val mapi : (int * elem -> elem) -> vector -> vector
    val map : (elem -> elem) -> vector -> vector
    val foldli : (int * elem * 'a -> 'a) -> 'a -> vector -> 'a
    val foldri : (int * elem * 'a -> 'a) -> 'a -> vector -> 'a
    val foldl : (elem * 'a -> 'a) -> 'a -> vector -> 'a
    val foldr : (elem * 'a -> 'a) -> 'a -> vector -> 'a
    val findi : (int * elem -> bool) -> vector -> (int * elem) option
    val find : (elem -> bool) -> vector -> elem option
    val exists : (elem -> bool) -> vector -> bool
    val all : (elem -> bool) -> vector -> bool
    val collate : (elem * elem -> order) -> vector * vector -> order

    (* https://github.com/SMLFamily/BasisLibrary/wiki/2015-003f-MONO_VECTOR *)
    val toList : vector -> elem list
    val append : vector * elem -> vector
    val prepend : elem * vector -> vector
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
    val concat : slice list -> vector
    val isEmpty : slice -> bool
    val getItem : slice -> (elem * slice) option
    val appi : (int * elem -> unit) -> slice -> unit
    val app : (elem -> unit) -> slice -> unit
    val mapi : (int * elem -> elem) -> slice -> vector
    val map : (elem -> elem) -> slice -> vector
    val foldli : (int * elem * 'b -> 'b) -> 'b -> slice -> 'b
    val foldri : (int * elem * 'b -> 'b) -> 'b -> slice -> 'b
    val foldl : (elem * 'b -> 'b) -> 'b -> slice -> 'b
    val foldr : (elem * 'b -> 'b) -> 'b -> slice -> 'b
    val findi : (int * elem -> bool) -> slice -> (int * elem) option
    val find : (elem -> bool) -> slice -> elem option
    val exists : (elem -> bool) -> slice -> bool
    val all : (elem -> bool) -> slice -> bool
    val collate : (elem * elem -> order) -> slice * slice -> order
end

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
    val copyVec : { src : vector, dst : array, di : int } -> unit
    val appi : (int * elem -> unit) -> array -> unit
    val app : (elem -> unit) -> array -> unit
    val modifyi : (int * elem -> elem) -> array -> unit
    val modify : (elem -> elem) -> array -> unit
    val foldli : (int * elem * 'b -> 'b) -> 'b -> array -> 'b
    val foldri : (int * elem * 'b -> 'b) -> 'b -> array -> 'b
    val foldl : (elem * 'b -> 'b) -> 'b -> array -> 'b
    val foldr : (elem * 'b -> 'b) -> 'b -> array -> 'b
    val findi : (int * elem -> bool) -> array -> (int * elem) option
    val find : (elem -> bool) -> array -> elem option
    val exists : (elem -> bool) -> array -> bool
    val all : (elem -> bool) -> array -> bool
    val collate : (elem * elem -> order) -> array * array -> order

    (* https://github.com/SMLFamily/BasisLibrary/wiki/2015-003h-MONO_ARRAY *)
    val toList : array -> elem list
    val fromVector : vector -> array
    val toVector : array -> vector (* = vector *)
end

signature MONO_ARRAY_NOEQTYPE = sig
    type array
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
    val copyVec : { src : vector, dst : array, di : int } -> unit
    val appi : (int * elem -> unit) -> array -> unit
    val app : (elem -> unit) -> array -> unit
    val modifyi : (int * elem -> elem) -> array -> unit
    val modify : (elem -> elem) -> array -> unit
    val foldli : (int * elem * 'b -> 'b) -> 'b -> array -> 'b
    val foldri : (int * elem * 'b -> 'b) -> 'b -> array -> 'b
    val foldl : (elem * 'b -> 'b) -> 'b -> array -> 'b
    val foldr : (elem * 'b -> 'b) -> 'b -> array -> 'b
    val findi : (int * elem -> bool) -> array -> (int * elem) option
    val find : (elem -> bool) -> array -> elem option
    val exists : (elem -> bool) -> array -> bool
    val all : (elem -> bool) -> array -> bool
    val collate : (elem * elem -> order) -> array * array -> order

    (* https://github.com/SMLFamily/BasisLibrary/wiki/2015-003h-MONO_ARRAY *)
    val toList : array -> elem list
    val fromVector : vector -> array
    val toVector : array -> vector (* = vector *)
end

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
    val base : slice -> array * int * int
    val vector : slice -> vector
    val copy : { src : slice, dst : array, di : int } -> unit
    val copyVec : { src : vector_slice, dst : array, di : int } -> unit
    val isEmpty : slice -> bool
    val getItem : slice -> (elem * slice) option
    val appi : (int * elem -> unit) -> slice -> unit
    val app : (elem -> unit) -> slice -> unit
    val modifyi : (int * elem -> elem) -> slice -> unit
    val modify : (elem -> elem) -> slice -> unit
    val foldli : (int * elem * 'b -> 'b) -> 'b -> slice -> 'b
    val foldri : (int * elem * 'b -> 'b) -> 'b -> slice -> 'b
    val foldl : (elem * 'b -> 'b) -> 'b -> slice -> 'b
    val foldr : (elem * 'b -> 'b) -> 'b -> slice -> 'b
    val findi : (int * elem -> bool) -> slice -> (int * elem) option
    val find : (elem -> bool) -> slice -> elem option
    val exists : (elem -> bool) -> slice -> bool
    val all : (elem -> bool) -> slice -> bool
    val collate : (elem * elem -> order) -> slice * slice -> order
end

signature MONO_SEQUENCE_PRIM = sig
    type elem
    type vector
    type array
    structure MonoVector : sig
                  val maxLen : int
                  val length : vector -> int
                  val unsafeSub : vector * int -> elem
                  val fromList : elem list -> vector
                  val unsafeFromListN : int * elem list -> vector
                  val unsafeFromListRevN : int * elem list -> vector
                  val concat : vector list -> vector
                  val sliceToVector : { base : vector, start : int, length : int } -> vector
                  val shallowSliceToVector : { base : vector, start : int, length : int } -> vector
              end
    structure MonoArray : sig
                  val maxLen : int
                  val eq : array * array -> bool
                  val length : array -> int
                  val unsafeCreate : int * elem -> array
                  val fromList : elem list -> array
                  val unsafeFromListN : int * elem list -> array
                  val unsafeSub : array * int -> elem
                  val unsafeUpdate : array * int * elem -> unit
              end
end

functor MonoSequence (P : MONO_SEQUENCE_PRIM) : sig
            structure MonoVector : MONO_VECTOR where type elem = P.elem where type vector = P.vector
            structure MonoVectorSlice : MONO_VECTOR_SLICE where type elem = P.elem where type vector = P.vector where type slice = { base : P.vector, start : int, length : int }
            structure MonoArray : MONO_ARRAY_NOEQTYPE where type elem = P.elem where type array = P.array where type vector = P.vector
            structure MonoArraySlice : MONO_ARRAY_SLICE where type elem = P.elem where type array = P.array where type vector = P.vector where type vector_slice = { base : P.vector, start : int, length : int } where type slice = { base : P.array, start : int, length : int }
        end = struct
structure MonoVector = struct
type elem = P.elem
type vector = P.vector
val maxLen = P.MonoVector.maxLen
val fromList = P.MonoVector.fromList
fun tabulate (n, f) = P.MonoVector.unsafeFromListN (n, List.tabulate (n, f))
val length = P.MonoVector.length
fun sub (v, i) = if 0 <= i andalso i < length v then
                     P.MonoVector.unsafeSub (v, i)
                 else
                     raise Subscript
fun update (v, i, x) = P.MonoVector.concat [P.MonoVector.shallowSliceToVector { base = v, start = 0, length = i }, P.MonoVector.fromList [x], P.MonoVector.shallowSliceToVector { base = v, start = i + 1, length = length v - i - 1 }]
val concat = P.MonoVector.concat
fun appi f v = let val n = length v
                   fun loop i = if i >= n then
                                    ()
                                else
                                    (f (i, P.MonoVector.unsafeSub (v, i)) : unit; loop (i + 1))
               in loop 0
               end
fun app f v = let val n = length v
                  fun loop i = if i >= n then
                                   ()
                               else
                                   (f (P.MonoVector.unsafeSub (v, i)) : unit; loop (i + 1))
              in loop 0
              end
fun mapi f v = let val n = length v
              in P.MonoVector.unsafeFromListN (n, List.tabulate (n, fn i => f (i, P.MonoVector.unsafeSub (v, i))))
              end
fun map f v = let val n = length v
              in P.MonoVector.unsafeFromListN (n, List.tabulate (n, fn i => f (P.MonoVector.unsafeSub (v, i))))
              end
fun foldli f init v = let val n = length v
                          fun loop (i, acc) = if i >= n then
                                                  acc
                                              else
                                                  loop (i + 1, f (i, P.MonoVector.unsafeSub (v, i), acc))
                      in loop (0, init)
                      end
fun foldri f init v = let fun loop (i, acc) = if i < 0 then
                                                  acc
                                              else
                                                  loop (i - 1, f (i, P.MonoVector.unsafeSub (v, i), acc))
                      in loop (length v - 1, init)
                      end
fun foldl f init v = let val n = length v
                         fun loop (i, acc) = if i >= n then
                                                 acc
                                             else
                                                 loop (i + 1, f (P.MonoVector.unsafeSub (v, i), acc))
                     in loop (0, init)
                     end
fun foldr f init v = let fun loop (i, acc) = if i < 0 then
                                                 acc
                                             else
                                                 loop (i - 1, f (P.MonoVector.unsafeSub (v, i), acc))
                     in loop (length v - 1, init)
                     end
fun findi f v = let val n = length v
                    fun loop i = if i >= n then
                                     NONE
                                 else
                                     let val x = P.MonoVector.unsafeSub (v, i)
                                     in if f (i, x) then
                                            SOME (i, x)
                                        else
                                            loop (i + 1)
                                     end
                in loop 0
                end
fun find f v = let val n = length v
                   fun loop i = if i >= n then
                                    NONE
                                else
                                    let val x = P.MonoVector.unsafeSub (v, i)
                                    in if f x then
                                           SOME x
                                       else
                                           loop (i + 1)
                                    end
               in loop 0
               end
fun exists f v = let val n = length v
                     fun loop i = if i >= n then
                                      false
                                  else
                                      f (P.MonoVector.unsafeSub (v, i)) orelse loop (i + 1)
                 in loop 0
                 end
fun all f v = let val n = length v
                  fun loop i = if i >= n then
                                   true
                               else
                                   f (P.MonoVector.unsafeSub (v, i)) andalso loop (i + 1)
              in loop 0
              end
fun collate compare (xs, ys) = let val xl = length xs
                                   val yl = length ys
                                   fun loop i = case (xl <= i, yl <= i) of
                                                    (true, true) => EQUAL
                                                  | (true, false) => LESS
                                                  | (false, true) => GREATER
                                                  | (false, false) => case compare (P.MonoVector.unsafeSub (xs, i), P.MonoVector.unsafeSub (ys, i)) of
                                                                          EQUAL => loop (i + 1)
                                                                        | t => t
                               in loop 0
                               end
fun toList a = foldr (op ::) [] a
fun append (v, x) = P.MonoVector.concat [v, P.MonoVector.fromList [x]]
fun prepend (x, v) = P.MonoVector.concat [P.MonoVector.fromList [x], v]
end
structure MonoVectorSlice = struct
type elem = P.elem
type vector = P.vector
type slice = { base : vector, start : int, length : int }
val length : slice -> int = #length
fun sub ({ base, start, length }, i) = if 0 <= i andalso i < length then
                                           P.MonoVector.unsafeSub (base, start + i)
                                       else
                                           raise Subscript
fun full v = { base = v, start = 0, length = P.MonoVector.length v }
fun slice (v, i, NONE) = let val n = P.MonoVector.length v
                         in if 0 <= i andalso i <= n then
                                { base = v, start = i, length = n - i }
                            else
                                raise Subscript
                         end
  | slice (v, i, SOME n) = if 0 <= i andalso 0 <= n andalso i + n <= P.MonoVector.length v then
                               { base = v, start = i, length = n }
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
val vector = P.MonoVector.sliceToVector
fun concat slices = P.MonoVector.concat (List.map P.MonoVector.shallowSliceToVector slices)
fun isEmpty { base, start, length } = length = 0
fun getItem { base, start, length } = if length > 0 then
                                          SOME (P.MonoVector.unsafeSub (base, start), { base = base, start = start + 1, length = length - 1 })
                                      else
                                          NONE
fun appi f { base, start, length } = let fun loop i = if i >= length then
                                                          ()
                                                      else
                                                          ( f (i, P.MonoVector.unsafeSub (base, start + i)) : unit
                                                          ; loop (i + 1)
                                                          )
                                     in loop 0
                                     end
fun app f { base, start, length } = let val n = start + length
                                        fun loop i = if i >= n then
                                                         ()
                                                     else
                                                         ( f (P.MonoVector.unsafeSub (base, i)) : unit
                                                         ; loop (i + 1)
                                                         )
                                    in loop start
                                    end
fun mapi f { base, start, length } = let fun loop (i, acc) = if i >= length then
                                                                 P.MonoVector.unsafeFromListRevN (length, acc)
                                                             else
                                                                 loop (i + 1, f (i, P.MonoVector.unsafeSub (base, start + i)) :: acc)
                                     in loop (0, [])
                                     end
fun map f { base, start, length } = let val n = start + length
                                        fun loop (i, acc) = if i >= n then
                                                                P.MonoVector.unsafeFromListRevN (length, acc)
                                                            else
                                                                loop (i + 1, f (P.MonoVector.unsafeSub (base, i)) :: acc)
                                     in loop (start, [])
                                     end
fun foldli f init { base, start, length } = let fun loop (i, acc) = if i >= length then
                                                                        acc
                                                                    else
                                                                        loop (i + 1, f (i, P.MonoVector.unsafeSub (base, start + i), acc))
                                            in loop (0, init)
                                            end
fun foldri f init { base, start, length } = let fun loop (i, acc) = if i < 0 then
                                                                        acc
                                                                    else
                                                                        loop (i - 1, f (i, P.MonoVector.unsafeSub (base, start + i), acc))
                                            in loop (length - 1, init)
                                            end
fun foldl f init { base, start, length } = let val n = start + length
                                               fun loop (i, acc) = if i >= n then
                                                                       acc
                                                                   else
                                                                       loop (i + 1, f (P.MonoVector.unsafeSub (base, i), acc))
                                           in loop (start, init)
                                           end
fun foldr f init { base, start, length } = let fun loop (i, acc) = if i < start then
                                                                       acc
                                                                   else
                                                                       loop (i - 1, f (P.MonoVector.unsafeSub (base, i), acc))
                                           in loop (start + length - 1, init)
                                           end
fun findi f { base, start, length } = let fun loop i = if i >= length then
                                                           NONE
                                                       else
                                                           let val x = P.MonoVector.unsafeSub (base, start + i)
                                                           in if f (i, x) then
                                                                  SOME (i, x)
                                                              else
                                                                  loop (i + 1)
                                                           end
                                      in loop 0
                                      end
fun find f { base, start, length } = let val n = start + length
                                         fun loop i = if i >= n then
                                                          NONE
                                                      else
                                                          let val x = P.MonoVector.unsafeSub (base, i)
                                                          in if f x then
                                                                 SOME x
                                                             else
                                                                 loop (i + 1)
                                                          end
                                     in loop start
                                     end
fun exists f { base, start, length } = let val n = start + length
                                           fun loop i = if i >= n then
                                                            false
                                                        else
                                                            f (P.MonoVector.unsafeSub (base, i)) orelse loop (i + 1)
                                       in loop start
                                       end
fun all f { base, start, length } = let val n = start + length
                                        fun loop i = if i >= n then
                                                         true
                                                     else
                                                         f (P.MonoVector.unsafeSub (base, i)) andalso loop (i + 1)
                                    in loop start
                                    end
fun collate compare ({ base, start, length }, { base = base', start = start', length = length' })
    = let val xn = start + length
          val yn = start' + length'
          fun loop (i, j) = case (xn <= i, yn <= j) of
                                (true, true) => EQUAL
                              | (true, false) => LESS
                              | (false, true) => GREATER
                              | (false, false) => case compare (P.MonoVector.unsafeSub (base, i), P.MonoVector.unsafeSub (base', j)) of
                                                      EQUAL => loop (i + 1, j + 1)
                                                    | t => t
      in loop (start, start')
      end
end
structure MonoArray = struct
type array = P.array
type elem = P.elem
type vector = P.vector
val maxLen = P.MonoArray.maxLen
fun array (n, init) = if n < 0 orelse maxLen < n then
                          raise Size
                      else
                          P.MonoArray.unsafeCreate (n, init)
val fromList = P.MonoArray.fromList
fun tabulate (n, f) = if maxLen < n then
                          raise Size
                      else
                          P.MonoArray.unsafeFromListN (n, List.tabulate (n, f))
val length = P.MonoArray.length
fun sub (a, i) = if 0 <= i andalso i < length a then
                     P.MonoArray.unsafeSub (a, i)
                 else
                     raise Subscript
fun update (a, i, x) = if 0 <= i andalso i < length a then
                           P.MonoArray.unsafeUpdate (a, i, x)
                       else
                           raise Subscript
(* copy: may be overridden with a more efficient implementation *)
fun copy { src, dst, di } = let val srcLen = length src
                            in if 0 <= di andalso di + srcLen <= length dst then
                                   let fun loop i = if i >= srcLen then
                                                        ()
                                                    else
                                                        ( P.MonoArray.unsafeUpdate (dst, di + i, P.MonoArray.unsafeSub (src, i))
                                                        ; loop (i + 1)
                                                        )
                                   in loop 0
                                   end
                               else
                                   raise Subscript
                            end
(* copyVec: may be overridden with a more efficient implementation *)
fun copyVec { src, dst, di } = let val srcLen = P.MonoVector.length src
                               in if 0 <= di andalso di + srcLen <= length dst then
                                      let fun loop i = if i >= srcLen then
                                                           ()
                                                       else
                                                           ( P.MonoArray.unsafeUpdate (dst, di + i, P.MonoVector.unsafeSub (src, i))
                                                           ; loop (i + 1)
                                                           )
                                      in loop 0
                                      end
                                  else
                                      raise Subscript
                               end
fun appi f a = let val n = length a
                   fun loop i = if i >= n then
                                    ()
                                else
                                    ( f (i, P.MonoArray.unsafeSub (a, i))
                                    ; loop (i + 1)
                                    )
               in loop 0
               end
fun app f a = let val n = length a
                  fun loop i = if i >= n then
                                   ()
                               else
                                   ( f (P.MonoArray.unsafeSub (a, i))
                                   ; loop (i + 1)
                                   )
              in loop 0
              end
fun modifyi f a = let val n = length a
                      fun loop i = if i >= n then
                                       ()
                                   else
                                       let val x = P.MonoArray.unsafeSub (a, i)
                                           val y = f (i, x)
                                       in P.MonoArray.unsafeUpdate (a, i, y)
                                        ; loop (i + 1)
                                       end
                  in loop 0
                  end
fun modify f a = let val n = length a
                     fun loop i = if i >= n then
                                      ()
                                  else
                                      let val x = P.MonoArray.unsafeSub (a, i)
                                          val y = f x
                                      in P.MonoArray.unsafeUpdate (a, i, y)
                                       ; loop (i + 1)
                                      end
                 in loop 0
                 end
fun foldli f init v = let val n = length v
                          fun loop (i, acc) = if i >= n then
                                                  acc
                                              else
                                                  loop (i + 1, f (i, P.MonoArray.unsafeSub (v, i), acc))
                      in loop (0, init)
                      end
fun foldri f init v = let fun loop (i, acc) = if i < 0 then
                                                  acc
                                              else
                                                  loop (i - 1, f (i, P.MonoArray.unsafeSub (v, i), acc))
                      in loop (length v - 1, init)
                      end
fun foldl f init v = let val n = length v
                         fun loop (i, acc) = if i >= n then
                                                 acc
                                             else
                                                 loop (i + 1, f (P.MonoArray.unsafeSub (v, i), acc))
                     in loop (0, init)
                     end
fun foldr f init v = let fun loop (i, acc) = if i < 0 then
                                                 acc
                                             else
                                                 loop (i - 1, f (P.MonoArray.unsafeSub (v, i), acc))
                     in loop (length v - 1, init)
                     end
fun findi f v = let val n = length v
                    fun loop i = if i >= n then
                                     NONE
                                 else
                                     let val x = P.MonoArray.unsafeSub (v, i)
                                     in if f (i, x) then
                                            SOME (i, x)
                                        else
                                            loop (i + 1)
                                     end
                in loop 0
                end
fun find f v = let val n = length v
                   fun loop i = if i >= n then
                                    NONE
                                else
                                    let val x = P.MonoArray.unsafeSub (v, i)
                                    in if f x then
                                           SOME x
                                       else
                                           loop (i + 1)
                                    end
               in loop 0
               end
fun exists f v = let val n = length v
                     fun loop i = if i >= n then
                                      false
                                  else
                                      f (P.MonoArray.unsafeSub (v, i)) orelse loop (i + 1)
                 in loop 0
                 end
fun all f v = let val n = length v
                  fun loop i = if i >= n then
                                   true
                               else
                                   f (P.MonoArray.unsafeSub (v, i)) andalso loop (i + 1)
              in loop 0
              end
fun collate compare (xs, ys) = let val xl = length xs
                                   val yl = length ys
                                   fun loop i = case (xl <= i, yl <= i) of
                                                    (true, true) => EQUAL
                                                  | (true, false) => LESS
                                                  | (false, true) => GREATER
                                                  | (false, false) => case compare (P.MonoArray.unsafeSub (xs, i), P.MonoArray.unsafeSub (ys, i)) of
                                                                          EQUAL => loop (i + 1)
                                                                        | t => t
                               in loop 0
                               end
fun toList a = foldr (op ::) [] a
fun vector a = P.MonoVector.unsafeFromListN (length a, toList a) (* may be overridden with a more efficient implementation *)
fun fromVector v = P.MonoArray.unsafeFromListN (P.MonoVector.length v, MonoVector.toList v) (* may be overridden with a more efficient implementation *)
val toVector = vector
end
structure MonoArraySlice = struct
type elem = P.elem
type array = P.array
type vector = P.vector
type slice = { base : array, start : int, length : int }
type vector_slice = MonoVectorSlice.slice
val length : slice -> int = #length
fun sub ({ base, start, length }, i) = if 0 <= i andalso i < length then
                                           P.MonoArray.unsafeSub (base, start + i)
                                       else
                                           raise Subscript
fun update ({ base, start, length }, i, x) = if 0 <= i andalso i < length then
                                                 P.MonoArray.unsafeUpdate (base, start + i, x)
                                             else
                                                 raise Subscript
fun full v = { base = v, start = 0, length = P.MonoArray.length v }
fun slice (v, i, NONE) = let val n = P.MonoArray.length v
                         in if 0 <= i andalso i <= n then
                                { base = v, start = i, length = n - i }
                            else
                                raise Subscript
                         end
  | slice (v, i, SOME n) = if 0 <= i andalso 0 <= n andalso i + n <= P.MonoArray.length v then
                               { base = v, start = i, length = n }
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
fun copy { src = { base, start, length }, dst, di }
    = let fun forward i = if i >= length then
                              ()
                          else
                              ( P.MonoArray.unsafeUpdate (dst, di + i, P.MonoArray.unsafeSub (base, start + i))
                              ; forward (i + 1)
                              )
          fun backward i = if i < 0 then
                               ()
                           else
                               ( P.MonoArray.unsafeUpdate (dst, di + i, P.MonoArray.unsafeSub (base, start + i))
                               ; backward (i - 1)
                               )
      in if di < 0 orelse P.MonoArray.length dst < di + length then
             raise Subscript
         else
             if start >= di then
                 forward 0
             else
                 backward (length - 1)
      end
fun copyVec { src = { base, start, length }, dst, di }
    = let fun forward i = if i >= length then
                              ()
                          else
                              ( P.MonoArray.unsafeUpdate (dst, di + i, P.MonoVector.unsafeSub (base, start + i))
                              ; forward (i + 1)
                              )
      in if di < 0 orelse P.MonoArray.length dst < di + length then
             raise Subscript
         else
             forward 0
      end
fun isEmpty { base, start, length } = length = 0
fun getItem { base, start, length } = if length > 0 then
                                          SOME (P.MonoArray.unsafeSub (base, start), { base = base, start = start + 1, length = length - 1 })
                                      else
                                          NONE
fun appi f { base, start, length } = let fun loop i = if i >= length then
                                                          ()
                                                      else
                                                          ( f (i, P.MonoArray.unsafeSub (base, start + i)) : unit
                                                          ; loop (i + 1)
                                                          )
                                     in loop 0
                                     end
fun app f { base, start, length } = let val n = start + length
                                        fun loop i = if i >= n then
                                                         ()
                                                     else
                                                         ( f (P.MonoArray.unsafeSub (base, i)) : unit
                                                         ; loop (i + 1)
                                                         )
                                    in loop start
                                    end
fun modifyi f { base, start, length } = let fun loop i = if i >= length then
                                                                    ()
                                                                else
                                                                    let val j = start + i
                                                                        val x = P.MonoArray.unsafeSub (base, j)
                                                                        val y = f (i, x)
                                                                    in P.MonoArray.unsafeUpdate (base, j, y)
                                                                     ; loop (i + 1)
                                                                    end
                                        in loop 0
                                        end
fun modify f { base, start, length } = let val n = start + length
                                           fun loop i = if i >= n then
                                                            ()
                                                        else
                                                            let val x = P.MonoArray.unsafeSub (base, i)
                                                                val y = f x
                                                            in P.MonoArray.unsafeUpdate (base, i, y)
                                                             ; loop (i + 1)
                                                            end
                                       in loop start
                                       end
fun foldli f init { base, start, length } = let fun loop (i, acc) = if i >= length then
                                                                        acc
                                                                    else
                                                                        loop (i + 1, f (i, P.MonoArray.unsafeSub (base, start + i), acc))
                                            in loop (0, init)
                                            end
fun foldri f init { base, start, length } = let fun loop (i, acc) = if i < 0 then
                                                                        acc
                                                                    else
                                                                        loop (i - 1, f (i, P.MonoArray.unsafeSub (base, start + i), acc))
                                            in loop (length - 1, init)
                                            end
fun foldl f init { base, start, length } = let val n = start + length
                                               fun loop (i, acc) = if i >= n then
                                                                       acc
                                                                   else
                                                                       loop (i + 1, f (P.MonoArray.unsafeSub (base, i), acc))
                                           in loop (start, init)
                                           end
fun foldr f init { base, start, length } = let fun loop (i, acc) = if i < start then
                                                                       acc
                                                                   else
                                                                       loop (i - 1, f (P.MonoArray.unsafeSub (base, i), acc))
                                           in loop (start + length - 1, init)
                                           end
fun findi f { base, start, length } = let fun loop i = if i >= length then
                                                           NONE
                                                       else
                                                           let val x = P.MonoArray.unsafeSub (base, start + i)
                                                           in if f (i, x) then
                                                                  SOME (i, x)
                                                              else
                                                                  loop (i + 1)
                                                           end
                                      in loop 0
                                      end
fun find f { base, start, length } = let val n = start + length
                                         fun loop i = if i >= n then
                                                          NONE
                                                      else
                                                          let val x = P.MonoArray.unsafeSub (base, i)
                                                          in if f x then
                                                                 SOME x
                                                             else
                                                                 loop (i + 1)
                                                          end
                                     in loop start
                                     end
fun exists f { base, start, length } = let val n = start + length
                                           fun loop i = if i >= n then
                                                            false
                                                        else
                                                            f (P.MonoArray.unsafeSub (base, i)) orelse loop (i + 1)
                                       in loop start
                                       end
fun all f { base, start, length } = let val n = start + length
                                        fun loop i = if i >= n then
                                                         true
                                                     else
                                                         f (P.MonoArray.unsafeSub (base, i)) andalso loop (i + 1)
                                    in loop start
                                    end
fun collate compare ({ base, start, length }, { base = base', start = start', length = length' })
    = let val xn = start + length
          val yn = start' + length'
          fun loop (i, j) = case (xn <= i, yn <= j) of
                                (true, true) => EQUAL
                              | (true, false) => LESS
                              | (false, true) => GREATER
                              | (false, false) => case compare (P.MonoArray.unsafeSub (base, i), P.MonoArray.unsafeSub (base', j)) of
                                                      EQUAL => loop (i + 1, j + 1)
                                                    | t => t
      in loop (start, start')
      end
fun vector a = P.MonoVector.unsafeFromListN (length a, foldr (op ::) [] a) (* may be overridden with a more efficient implementation *)
end
end;
