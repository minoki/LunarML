structure String = struct
type string = string

(* Low-level primitives implemented directly in Wasm codegen *)
fun size (s : string) : int = _primCall "String.size" (s)
fun str (c : char) : string = _primCall "String.str" (c)

(* Unsafe unchecked byte access: string * int -> char *)
fun unsafeSub (s : string, i : int) : char = _primCall "Unsafe.CharVector.sub" (s, i)

fun x ^ y =
  let val n1 = size x
      val n2 = size y
      val buf = _primCall "CharArray.alloc" (n1 + n2)
  in
    _primCall "String.copyBytes" (buf, 0, x, 0, n1);
    _primCall "String.copyBytes" (buf, n1, y, 0, n2);
    Unsafe.cast buf
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
    Unsafe.cast buf
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
    Unsafe.cast buf
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

fun x < y = Int.< (compareBytes (x, y), 0)
fun x <= y = Int.<= (compareBytes (x, y), 0)
fun x > y = Int.> (compareBytes (x, y), 0)
fun x >= y = Int.>= (compareBytes (x, y), 0)
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
type array = string
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
type array = string
type elem = char
fun sub (a : array, i : int) : elem = _primCall "Unsafe.CharVector.sub" (a, i)
fun update (a : array, i : int, c : elem) : unit = _primCall "Unsafe.CharArray.update" (a, i, c)
end
