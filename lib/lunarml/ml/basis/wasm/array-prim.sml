structure Array = struct
datatype array = datatype array
val maxLen : int = 0x7fffffff
fun length arr = _primCall "Array.length" (arr)
fun sub (arr, i) = if i < 0 orelse length arr <= i then
                       raise Subscript
                   else
                       Unsafe.Array.sub (arr, i)
fun update (arr, i, value) = if i < 0 orelse length arr <= i then
                                 raise Subscript
                             else
                                 Unsafe.Array.update (arr, i, value)
fun array (n, init) = _primCall "Array.array" (n, init)
fun fromList xs =
  let
    val n = List.length xs
    val arr = _primCall "Array.allocUninitialized" (n)
    fun go (_, []) = arr
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
        arr
      else
        (Unsafe.Array.update (arr, i, f i)
        ; go (_primCall "Int.+.wrapping" (i, 1))
        )
  in
    go 0
  end
fun vector arr =
  let
    val n = length arr
    val arr2 = _primCall "Array.allocUninitialized" (n)
    fun loop i =
      if i < n then
        ( Unsafe.Array.update (arr2, i, Unsafe.Array.sub (arr, i))
        ; loop (i + 1)
        )
      else
        ()
  in
    loop 0;
    _primCall "Unsafe.cast" (arr2)
  end
fun copyVec {src, dst, di} =
  let
    val m = Vector.length src
    val n = length dst
    fun loop i =
      if i >= m then
        ()
      else
        ( Unsafe.Array.update (dst, di + i, Unsafe.Vector.sub (src, i))
        ; loop (i + 1)
        )
  in
    if di < 0 orelse n < di + m then
      raise Subscript
    else
      loop 0
  end
end;
(* equality is defined in equal.sml *)
