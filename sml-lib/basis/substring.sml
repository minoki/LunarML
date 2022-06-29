local
    structure CharSequenceAndSubstring :> sig
                  structure CharVector : MONO_VECTOR where type vector = String.string where type elem = char
                  structure CharVectorSlice : MONO_VECTOR_SLICE where type elem = char where type vector = String.string
                  structure CharArray : MONO_ARRAY where type elem = char where type vector = String.string
                  structure CharArraySlice : MONO_ARRAY_SLICE where type elem = char where type vector = String.string
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
                                val isSuffix : string -> substring -> bool
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
                  sharing type CharArraySlice.array = CharArray.array
                  sharing type CharArraySlice.vector_slice = CharVectorSlice.slice
                  sharing type CharVectorSlice.slice = Substring.substring
              end = struct
    local
        structure Prim : MONO_SEQUENCE_PRIM = struct
        type elem = char
        type vector = String.string
        type array = char Array.array
        structure MonoVector = struct
        val maxLen = String.maxSize
        val length = String.size
        val unsafeSub = String.sub (* TODO *)
        val fromList = String.implode
        fun unsafeFromListN (n, xs) = String.implode xs (* TODO *)
        fun unsafeFromListRevN (n, xs) = String.implodeRev xs (* TODO *)
        val concat = String.concat
        fun sliceToVector { base, start, length } = String.substring (base, start, length)
        val shallowSliceToVector = sliceToVector
        end
        structure MonoArray = struct
        val maxLen = Array.maxLen
        val eq = op = : array * array -> bool
        val length = Array.length
        val unsafeCreate = Array.array (* TODO *)
        val fromList = Array.fromList
        fun unsafeFromListN (n, xs) = fromList xs (* TODO *)
        val unsafeSub = Unsafe.Array.sub
        val unsafeUpdate = Unsafe.Array.update
        end
        end
        structure Base = MonoSequence (Prim)
    in
    structure CharVector = Base.MonoVector
    structure CharVectorSlice = Base.MonoVectorSlice
    structure CharArray = Base.MonoArray
    structure CharArraySlice = Base.MonoArraySlice
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
    val concat = CharVectorSlice.concat
    fun concatWith s xs = String.concatWith s (List.map string xs)
    fun isPrefix s ss = String.isPrefix s (string ss)
    fun isSuffix s ss = String.isSuffix s (string ss)
    fun compare (s, t) = String.compare (string s, string t)
    fun splitl f (s as { base, start, length }) = let fun loop j = if j >= length then
                                                                       (s, { base = base, start = start + j, length = 0 })
                                                                   else if f (sub (s, j)) then
                                                                       loop (j + 1)
                                                                   else
                                                                       ({ base = base, start = start, length = j }, { base = base, start = start + j, length = length - j })
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
    val foldl = CharVectorSlice.foldl
    val foldr = CharVectorSlice.foldr
    end (* structure Substring *)
    end (* local *)
    end (* structure CharSequenceAndSubstring *)
in
open CharSequenceAndSubstring
end;
