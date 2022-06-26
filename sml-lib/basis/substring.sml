local
    structure CharVectorSliceAndSubstring :> sig
                  structure CharVectorSlice : MONO_VECTOR_SLICE where type elem = char where type vector = string
                  structure Substring : sig
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
                            end
                  sharing type CharVectorSlice.slice = Substring.substring
              end = struct
    structure CharVectorSlice = struct
    type elem = char
    type vector = string
    type slice = { base : vector, start : int, length : int }
    val length : slice -> int = #length
    fun sub ({ base, start, length }, i) = if 0 <= i andalso i < length then
                                               CharVector.sub (base, start + i)
                                           else
                                               raise Subscript
    fun full a = { base = a, start = 0, length = CharVector.length a }
    fun slice (a, i, NONE) = if 0 <= i andalso i <= CharVector.length a then
                                 { base = a, start = i, length = CharVector.length a - i }
                             else
                                 raise Subscript
      | slice (a, i, SOME n) = if 0 <= i andalso 0 <= n andalso i + n <= CharVector.length a then
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
    fun vector { base, start, length } = String.substring (base, start, length)
    fun isEmpty { base, start, length } = length = 0
    fun getItem { base, start, length } = if length > 0 then
                                              SOME (CharVector.sub (base, start), { base = base, start = start + 1, length = length - 1 })
                                          else
                                              NONE
    fun exists f { base, start, length } = let fun loop i = if i >= length then
                                                                false
                                                            else
                                                                if f (CharVector.sub (base, start + i)) then
                                                                    true
                                                                else
                                                                    loop (i + 1)
                                           in loop start
                                           end
    fun all f { base, start, length } = let fun loop i = if i >= length then
                                                             true
                                                         else
                                                             if not (f (CharVector.sub (base, start + i))) then
                                                                 false
                                                             else
                                                                 loop (i + 1)
                                        in loop start
                                        end
    end
    structure Substring = struct
    type char = char
    type string = string
    type substring = CharVectorSlice.slice
    val sub = CharVectorSlice.sub
    val size = CharVectorSlice.length
    val base = CharVectorSlice.base
    val full = CharVectorSlice.full
    val string = CharVectorSlice.vector
    val isEmpty = CharVectorSlice.isEmpty
    val getc = CharVectorSlice.getItem
    fun first { base, start, length } = if length = 0 then
                                            NONE
                                        else
                                            SOME (String.sub (base, start))
    fun triml k = if k < 0 then
                      raise Subscript
                  else
                      fn { base, start, length } => if k < length then
                                                        { base = base, start = start + k, length = length - k }
                                                    else
                                                        { base = base, start = start + length, length = 0 }
    fun trimr k = if k < 0 then
                      raise Subscript
                  else
                      fn { base, start, length } => if k < length then
                                                        { base = base, start = start, length = length - k }
                                                    else
                                                        { base = base, start = start, length = 0 }
    val slice = CharVectorSlice.subslice
    fun concat xs = String.concat (List.map string xs)
    fun concatWith s xs = String.concatWith s (List.map string xs)
    fun isPrefix s ss = String.isPrefix s (string ss)
    (* fun isSuffix s ss = String.isSuffix s (string ss) *)
    fun compare (s, t) = String.compare (string s, string t)
    fun splitl f (s as { base, start, length }) = let fun loop j = if j >= length then
                                                                       (s, { base = base, start = start + j, length = 0 })
                                                                   else if f (sub (s, j)) then
                                                                       loop (j + 1)
                                                                   else
                                                                       let val j' = j - 1
                                                                       in ({ base = base, start = start, length = j' }, { base = base, start = start + j', length = length - j' })
                                                                       end
                                                  in loop 0
                                                  end
    fun splitr f (s as { base, start, length }) = let fun loop j = if j < 0 then
                                                                       ({ base = base, start = start, length = 0 }, s)
                                                                   else if f (sub (s, j)) then
                                                                       loop (j - 1)
                                                                   else
                                                                       let val j' = j + 1
                                                                       in ({ base = base, start = start, length = j' }, { base = base, start = start + j', length = length - j' })
                                                                       end
                                                  in loop (length - 1)
                                                  end
    fun dropl p s = #2 (splitl p s)
    fun dropr p s = #1 (splitr p s)
    fun takel p s = #1 (splitl p s)
    fun taker p s = #2 (splitr p s)
    fun foldl f init { base, start, length } = let val z = start + length
                                                   fun loop (j, acc) = if j >= z then
                                                                           acc
                                                                       else
                                                                           loop (j + 1, f (String.sub (base, j), acc))
                                               in loop (start, init)
                                               end
    fun foldr f init { base, start, length } = let fun loop (j, acc) = if j < start then
                                                                           acc
                                                                       else
                                                                           loop (j - 1, f (String.sub (base, j), acc))
                                               in loop (start + length - 1, init)
                                               end
    end (* structure Substring *)
    end
in
open CharVectorSliceAndSubstring
end;
