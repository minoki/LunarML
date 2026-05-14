structure Vector = struct
datatype vector = datatype vector
fun length vec = _primCall "Vector.length" (vec)
fun sub (vec, i) = if i < 0 orelse length vec <= i then
                       raise Subscript
                   else
                       Unsafe.Vector.sub (vec, i)
fun fromList xs =
  let
    val n = List.length xs
    val arr = _primCall "Array.allocUninitialized" (n)
    fun go (_, []) = _primCall "Unsafe.cast" (arr)
      | go (i, x :: xs) =
          ( Unsafe.Array.update (arr, i, x)
          ; go (_primCall "Int.+.wrapping" (i, 1), xs)
          )
  in
    go (0, xs)
  end
fun tabulate (n, f) =
  let
    val arr = _primCall "Array.allocUninitialized" (n)
    fun go i =
      if i >= n then
        _primCall "Unsafe.cast" (arr)
      else
        ( Unsafe.Array.update (arr, i, f i)
        ; go (_primCall "Int.+.wrapping" (i, 1))
        )
  in
    go 0
  end
fun concat vecs =
  let
    val total = List.foldl (fn (v, acc) => acc + length v) 0 vecs
    val arr = _primCall "Array.allocUninitialized" (total)
    fun copyVec (v, offset) =
      let
        val n = length v
        fun go i =
          if i >= n then
            ()
          else
            ( Unsafe.Array.update (arr, _primCall "Int.+.wrapping" (offset, i), Unsafe.Vector.sub (v, i))
            ; go (_primCall "Int.+.wrapping" (i, 1))
            )
      in
        go 0;
        _primCall "Int.+.wrapping" (offset, n)
      end
  in
    List.foldl copyVec 0 vecs;
    _primCall "Unsafe.cast" (arr)
  end
end;
_equality ''a vector = fn (x, y) =>
  let val n = Vector.length x
      fun go i = if i >= n then
                     true
                 else
                     Unsafe.Vector.sub (x, i) = Unsafe.Vector.sub (y, i) andalso go (i + 1)
  in n = Vector.length y andalso go 0
  end;
