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
end;
(* equality is defined in equal.sml *)
