structure export = struct
  val intArr = Array.array (5, 0)
  val () = Unsafe.Array.update (intArr, 0, 42)
  val () = Unsafe.Array.update (intArr, 1, 13)
  val () = Unsafe.Array.update (intArr, 4, 99)

  val tabArr = Array.tabulate (4, fn i => i * i)

  fun arrayLength () : int = Array.length intArr
  fun get0 () : int = Unsafe.Array.sub (intArr, 0)
  fun get1 () : int = Unsafe.Array.sub (intArr, 1)
  fun get4 () : int = Unsafe.Array.sub (intArr, 4)
  fun tab0 () : int = Unsafe.Array.sub (tabArr, 0)
  fun tab1 () : int = Unsafe.Array.sub (tabArr, 1)
  fun tab2 () : int = Unsafe.Array.sub (tabArr, 2)
  fun tab3 () : int = Unsafe.Array.sub (tabArr, 3)

  val boolArr = Array.array (3, false)
  val () = Unsafe.Array.update (boolArr, 0, true)
  val () = Unsafe.Array.update (boolArr, 2, true)

  fun boolGet0 () : bool = Unsafe.Array.sub (boolArr, 0)
  fun boolGet1 () : bool = Unsafe.Array.sub (boolArr, 1)
  fun boolGet2 () : bool = Unsafe.Array.sub (boolArr, 2)
end
