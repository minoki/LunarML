structure String = struct
type string = string
type char = char

val maxSize : int = 0x7fffffff

(* Low-level primitives implemented directly in Wasm codegen *)
fun size (s : string) : int = _primCall "String.size" (s)
fun str (c : char) : string = _primCall "String.str" (c)

(* Unsafe unchecked byte access: string * int -> char *)
fun unsafeSub (s : string, i : int) : char = _primCall "Unsafe.CharVector.sub" (s, i)

fun sub (s : string, i : int) : char =
  if i < 0 orelse size s <= i then
    raise Subscript
  else
    _primCall "Unsafe.CharVector.sub" (s, i)

fun x ^ y =
  let val n1 = size x
      val n2 = size y
      val buf = _primCall "CharArray.alloc" (n1 + n2)
  in
    _primCall "String.copyBytes" (buf, 0, x, 0, n1);
    _primCall "String.copyBytes" (buf, n1, y, 0, n2);
    _primCall "CharArray.unsafeFreeze" (buf)
  end

fun substring (s : string, start : int, len : int) =
  if start < 0 orelse len < 0 orelse size s < start + len then
    raise Subscript
  else
    let val buf = _primCall "CharArray.alloc" (len)
    in
      _primCall "String.copyBytes" (buf, 0, s, start, len);
      _primCall "CharArray.unsafeFreeze" (buf)
    end

fun implode (cs : char list) : string =
  let fun countChars ([], n) = n
        | countChars (_ :: rest, n) = countChars (rest, n + 1)
      val n = countChars (cs, 0)
      val buf = _primCall "CharArray.alloc" (n)
      fun fill ([], _) = ()
        | fill (c :: rest, i) =
            (_primCall "Unsafe.CharArray.update" (buf, i, c); fill (rest, i + 1))
  in
    fill (cs, 0);
    _primCall "CharArray.unsafeFreeze" (buf)
  end

fun implodeRev (cs : char list) : string =
  let fun countChars ([], n) = n
        | countChars (_ :: rest, n) = countChars (rest, n + 1)
      val n = countChars (cs, 0)
      val buf = _primCall "CharArray.alloc" (n)
      fun fill ([], _) = ()
        | fill (c :: rest, i) =
            (_primCall "Unsafe.CharArray.update" (buf, i, c); fill (rest, i - 1))
  in
    fill (cs, n - 1);
    _primCall "CharArray.unsafeFreeze" (buf)
  end

fun concat (ss : string list) : string =
  let fun totalLen ([], acc) = acc
        | totalLen (s :: rest, acc) = totalLen (rest, acc + size s)
      val n = totalLen (ss, 0)
      val buf = _primCall "CharArray.alloc" (n)
      fun copyAll ([], _) = ()
        | copyAll (s :: rest, off) =
            let val len = size s
            in
              _primCall "String.copyBytes" (buf, off, s, 0, len);
              copyAll (rest, off + len)
            end
  in
    copyAll (ss, 0);
    _primCall "CharArray.unsafeFreeze" (buf)
  end

fun concatRev (ss : string list) : string =
  let fun totalLen ([], acc) = acc
        | totalLen (s :: rest, acc) = totalLen (rest, acc + size s)
      val n = totalLen (ss, 0)
      val buf = _primCall "CharArray.alloc" (n)
      fun copyAll ([], _) = ()
        | copyAll (s :: rest, off) =
            let val len = size s
                val off = off - len
            in
              _primCall "String.copyBytes" (buf, off, s, 0, len);
              copyAll (rest, off)
            end
  in
    copyAll (ss, n);
    _primCall "CharArray.unsafeFreeze" (buf)
  end

fun concatWith (sep : string) (ss : string list) : string =
  let fun totalLen ([], acc) = acc
        | totalLen (s :: [], acc) = acc + size s
        | totalLen (s :: rest, acc) = totalLen (rest, acc + size s + size sep)
      val n = totalLen (ss, 0)
      val buf = _primCall "CharArray.alloc" (n)
      fun copyAll ([], _) = ()
        | copyAll (s :: [], off) =
            let val len = size s
            in
              _primCall "String.copyBytes" (buf, off, s, 0, len)
            end
        | copyAll (s :: rest, off) =
            let val len = size s
                val sepLen = size sep
            in
              _primCall "String.copyBytes" (buf, off, s, 0, len);
              _primCall "String.copyBytes" (buf, off + len, sep, 0, sepLen);
              copyAll (rest, off + len + sepLen)
            end
  in
    copyAll (ss, 0);
    _primCall "CharArray.unsafeFreeze" (buf)
  end

fun map (f : char -> char) (s : string) =
  let val buf = _primCall "CharArray.alloc" (size s)
      fun loop i = if i < size s then
                     (_primCall "Unsafe.CharArray.update" (buf, i, f (unsafeSub (s, i))); loop (i + 1))
                   else
                     ()
  in
    loop 0;
    _primCall "CharArray.unsafeFreeze" (buf)
  end

fun translate (f : char -> string) (s : string) =
  let fun collect (acc, i) = if i < size s then
                              collect (f (unsafeSub (s, i)) :: acc, i + 1)
                            else
                              acc
  in concatRev (collect ([], 0))
  end

fun explode (s : string) : char list =
  let fun collect (acc, i) = if i < 0 then
                               acc
                             else
                               collect (unsafeSub (s, i) :: acc, i - 1)
  in collect ([], size s - 1)
  end

fun tokens f s = let fun go (revTokens, acc, []) = List.rev (if List.null acc then revTokens else implodeRev acc :: revTokens)
                       | go (revTokens, acc, x :: xs) = if f x then
                                                            go (if List.null acc then revTokens else implodeRev acc :: revTokens, [], xs)
                                                        else
                                                            go (revTokens, x :: acc, xs)
                 in go ([], [], explode s)
                 end
fun fields f s = let fun go (revFields, acc, []) = List.rev (implodeRev acc :: revFields)
                       | go (revFields, acc, x :: xs) = if f x then
                                                            go (implodeRev acc :: revFields, [], xs)
                                                        else
                                                            go (revFields, x :: acc, xs)
                 in go ([], [], explode s)
                 end

(* Lexicographic comparison: -1, 0, or 1 *)
fun compareBytes (s : string, t : string) : int =
  let val n = size s
      val m = size t
      val minLen = if n < m then n else m
      fun loop i =
        if i >= minLen then
          if n < m then ~1
          else if n > m then 1
          else 0
        else
          let val cs = unsafeSub (s, i)
              val ct = unsafeSub (t, i)
          in
            if cs < ct then ~1
            else if cs > ct then 1
            else loop (i + 1)
          end
  in
    loop 0
  end

fun isPrefix (x : string) (y : string) =
  let fun loop i = i >= size x orelse (unsafeSub (x, i) = unsafeSub (y, i) andalso loop (i + 1))
  in size x <= size y andalso loop 0
  end

fun isSuffix (x : string) (y : string) =
  let fun loop i = i >= size x orelse (unsafeSub (x, i) = unsafeSub (y, size x + i) andalso loop (i + 1))
  in size x <= size y andalso loop 0
  end

fun x < y = Int.< (compareBytes (x, y), 0)
fun x <= y = Int.<= (compareBytes (x, y), 0)
fun x > y = Int.> (compareBytes (x, y), 0)
fun x >= y = Int.>= (compareBytes (x, y), 0)
fun compare (x, y) = Int.compare (compareBytes (x, y), 0)
end
_equality string = fn (x, y) =>
  let val n = String.size x
  in
    n = String.size y
    andalso
    let fun loop i =
          i >= n
          orelse
          (String.unsafeSub (x, i) = String.unsafeSub (y, i)
           andalso loop (i + 1))
    in loop 0 end
  end;
_overload "String" [string] { < = String.<
                             , <= = String.<=
                             , > = String.>
                             , >= = String.>=
                             , maxOrd = 255
                             };

(* CharArray type alias for string (same representation in WasmGC) *)
structure CharArray = struct
type array = _Prim.CharArray.array
fun alloc (n : int) : array = _primCall "CharArray.alloc" (n)
fun unsafeUpdate (arr : array, i : int, c : char) : unit =
  _primCall "Unsafe.CharArray.update" (arr, i, c)
end

(* UnsafeCharVector for use by other Basis code *)
structure UnsafeCharVector = struct
type vector = string
type elem = char
fun sub (v : vector, i : int) : elem = _primCall "Unsafe.CharVector.sub" (v, i)
end

(* UnsafeCharArray for use by other Basis code *)
structure UnsafeCharArray = struct
type array = CharArray.array
type elem = char
fun sub (a : array, i : int) : elem = _primCall "Unsafe.CharArray.sub" (a, i)
fun update (a : array, i : int, c : elem) : unit = _primCall "Unsafe.CharArray.update" (a, i, c)
end

val op ^ = String.^;
