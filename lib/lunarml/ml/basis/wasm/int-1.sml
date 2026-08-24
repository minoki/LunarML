signature INTEGER = sig
    eqtype int
    (* val toLarge : int -> LargeInt.int *)
    (* val fromLarge : LargeInt.int -> int *)
    val toInt : int -> Int.int
    val fromInt : Int.int -> int
    val precision : Int.int option
    val minInt : int option
    val maxInt : int option
    val + : int * int -> int
    val - : int * int -> int
    val * : int * int -> int
    val div : int * int -> int
    val mod : int * int -> int
    val quot : int * int -> int
    val rem : int * int -> int
    val compare : int * int -> order
    val < : int * int -> bool
    val <= : int * int -> bool
    val > : int * int -> bool
    val >= : int * int -> bool
    val ~ : int -> int
    val abs : int -> int
    val min : int * int -> int
    val max : int * int -> int
    val sign : int -> Int.int
    val sameSign : int * int -> bool
    val fmt : StringCvt.radix -> int -> string
    val toString : int -> string
    (* val scan : StringCvt.radix -> (char, 'a) StringCvt.reader -> (int, 'a) StringCvt.reader; defined in scan-num.sml *)
    (* val fromString : string -> int option; defined in scan-num.sml *)
end;

structure Int : INTEGER where type int = int = struct
open Int (* +, -, *, div, mod, quot, rem, ~, abs, <, <=, >, >=, fromInt *)
(* toLarge, fromLarge *)
val toInt : int -> int = fn x => x
val precision : int option = SOME 32
val minInt : int option = SOME ~0x8000_0000
val maxInt : int option = SOME 0x7fff_ffff
val compare : int * int -> order = fn (x, y) => if x = y then
                                                    EQUAL
                                                else if x < y then
                                                    LESS
                                                else
                                                    GREATER
val min : int * int -> int = fn (x, y) => if x < y then
                                              x
                                          else
                                              y
val max : int * int -> int = fn (x, y) => if x < y then
                                              y
                                          else
                                              x
val sign : int -> int = fn x => if x > 0 then
                                    1
                                else if x < 0 then
                                    ~1
                                else
                                    0
val sameSign : int * int -> bool = fn (x, y) => sign x = sign y
local
  infix 6 +! -!
  fun x +! y = _primCall "Int.+.wrapping" (x, y)
  fun x -! y = _primCall "Int.-.wrapping" (x, y)
  fun ~! x = _primCall "Int.~.wrapping" (x)
  fun quot' (x, y) = _primCall "Int.quot.unchecked" (x, y)
  fun rem' (x, y) = _primCall "Int.rem.unchecked" (x, y)
  fun intToDigit i =
    _primCall "Char.chr.unchecked" (_primCall "Char.ord" (#"0") +! i)
  fun intToHexDigit i =
    if i < 10 then
      _primCall "Char.chr.unchecked" (_primCall "Char.ord" (#"0") +! i)
    else
      _primCall "Char.chr.unchecked" (_primCall "Char.ord" (#"A") -! 10 +! i)
  fun fmtBIN 0 = "0"
    | fmtBIN x =
        let
          val initialBufSize = 33
          val radix = 2
          val buf = _primCall "CharArray.alloc" (initialBufSize)
          fun goPositive (i, 0) = i +! 1
            | goPositive (i, x) =
                let val r = rem' (x, radix)
                in _primCall "Unsafe.CharArray.update" (buf, i, intToDigit r)
                 ; goPositive (i -! 1, quot' (x, radix))
                end
          fun goNegative (i, 0) =
                ( _primCall "Unsafe.CharArray.update" (buf, i, #"~")
                ; i
                )
            | goNegative (i, x) =
                let val r = ~! (rem' (x, radix))
                in _primCall "Unsafe.CharArray.update" (buf, i, intToDigit r)
                 ; goNegative (i -! 1, quot' (x, radix))
                end
          val i = if x < 0 then
                    goNegative (initialBufSize -! 1, x)
                  else
                    goPositive (initialBufSize -! 1, x)
          val n = initialBufSize -! i
          val buf2 = _primCall "CharArray.alloc" (n)
        in
          _primCall "CharArray.copy" (buf2, 0, buf, i, n)
        ; _primCall "CharArray.unsafeFreeze" (buf2)
        end
  fun fmtOCT 0 = "0"
    | fmtOCT x =
        let
          (* ~40000000000 *)
          val initialBufSize = 12
          val radix = 8
          val buf = _primCall "CharArray.alloc" (initialBufSize)
          fun goPositive (i, 0) = i +! 1
            | goPositive (i, x) =
                let val r = rem' (x, radix)
                in _primCall "Unsafe.CharArray.update" (buf, i, intToDigit r)
                 ; goPositive (i -! 1, quot' (x, radix))
                end
          fun goNegative (i, 0) =
                ( _primCall "Unsafe.CharArray.update" (buf, i, #"~")
                ; i
                )
            | goNegative (i, x) =
                let val r = ~! (rem' (x, radix))
                in _primCall "Unsafe.CharArray.update" (buf, i, intToDigit r)
                 ; goNegative (i -! 1, quot' (x, radix))
                end
          val i = if x < 0 then
                    goNegative (initialBufSize -! 1, x)
                  else
                    goPositive (initialBufSize -! 1, x)
          val n = initialBufSize -! i
          val buf2 = _primCall "CharArray.alloc" (n)
        in
          _primCall "CharArray.copy" (buf2, 0, buf, i, n)
        ; _primCall "CharArray.unsafeFreeze" (buf2)
        end
  fun fmtDEC 0 = "0"
    | fmtDEC x =
        let
          (* ~2147483648 *)
          val initialBufSize = 11
          val radix = 10
          val buf = _primCall "CharArray.alloc" (initialBufSize)
          fun goPositive (i, 0) = i +! 1
            | goPositive (i, x) =
                let val r = rem' (x, radix)
                in _primCall "Unsafe.CharArray.update" (buf, i, intToDigit r)
                 ; goPositive (i -! 1, quot' (x, radix))
                end
          fun goNegative (i, 0) =
                ( _primCall "Unsafe.CharArray.update" (buf, i, #"~")
                ; i
                )
            | goNegative (i, x) =
                let val r = ~! (rem' (x, radix))
                in _primCall "Unsafe.CharArray.update" (buf, i, intToDigit r)
                 ; goNegative (i -! 1, quot' (x, radix))
                end
          val i = if x < 0 then
                    goNegative (initialBufSize -! 1, x)
                  else
                    goPositive (initialBufSize -! 1, x)
          val n = initialBufSize -! i
          val buf2 = _primCall "CharArray.alloc" (n)
        in
          _primCall "CharArray.copy" (buf2, 0, buf, i, n)
        ; _primCall "CharArray.unsafeFreeze" (buf2)
        end
  fun fmtHEX 0 = "0"
    | fmtHEX x =
        let
          (* ~80000000 *)
          val initialBufSize = 9
          val radix = 16
          val buf = _primCall "CharArray.alloc" (initialBufSize)
          fun goPositive (i, 0) = i +! 1
            | goPositive (i, x) =
                let val r = rem' (x, radix)
                in _primCall "Unsafe.CharArray.update" (buf, i, intToHexDigit r)
                 ; goPositive (i -! 1, quot' (x, radix))
                end
          fun goNegative (i, 0) =
                ( _primCall "Unsafe.CharArray.update" (buf, i, #"~")
                ; i
                )
            | goNegative (i, x) =
                let val r = ~! (rem' (x, radix))
                in _primCall "Unsafe.CharArray.update" (buf, i, intToHexDigit r)
                 ; goNegative (i -! 1, quot' (x, radix))
                end
          val i = if x < 0 then
                    goNegative (initialBufSize -! 1, x)
                  else
                    goPositive (initialBufSize -! 1, x)
          val n = initialBufSize -! i
          val buf2 = _primCall "CharArray.alloc" (n)
        in
          _primCall "CharArray.copy" (buf2, 0, buf, i, n)
        ; _primCall "CharArray.unsafeFreeze" (buf2)
        end
in
  val toString = fmtDEC
  fun fmt StringCvt.BIN = fmtBIN
    | fmt StringCvt.OCT = fmtOCT
    | fmt StringCvt.DEC = fmtDEC
    | fmt StringCvt.HEX = fmtHEX
end
end; (* structure Int *)

structure Int64 : INTEGER where type int = Int64.int = struct
open Int64 (* +, -, *, div, mod, quot, rem, ~, abs, <, <=, >, >=, fromInt *)
(* toLarge, fromLarge *)
fun toInt (x : int) : Int.int =
    if ~0x8000_0000 <= x andalso x <= 0x7fff_ffff then
        _primCall "Int64.toInt.unchecked" (x)
    else
        raise Overflow
val precision : Int.int option = SOME 64
val minInt : int option = SOME ~0x8000_0000_0000_0000
val maxInt : int option = SOME 0x7fff_ffff_ffff_ffff
val compare : int * int -> order = fn (x, y) => if x = y then
                                                    EQUAL
                                                else if x < y then
                                                    LESS
                                                else
                                                    GREATER
val min : int * int -> int = fn (x, y) => if x < y then
                                              x
                                          else
                                              y
val max : int * int -> int = fn (x, y) => if x < y then
                                              y
                                          else
                                              x
val sign : int -> Int.int = fn x => if x > 0 then
                                        1
                                    else if x < 0 then
                                        ~1
                                    else
                                        0
val sameSign : int * int -> bool = fn (x, y) => sign x = sign y
local
  infix 6 +! -!
  fun x +! y = _primCall "Int.+.wrapping" (x, y)
  fun x -! y = _primCall "Int.-.wrapping" (x, y)
  fun ~! x = _primCall "Int64.~.wrapping" (x)
  fun quot' (x, y) = _primCall "Int64.quot.unchecked" (x, y)
  fun rem' (x, y) = _primCall "Int64.rem.unchecked" (x, y)
  fun intToDigit i =
    _primCall "Char.chr.unchecked" (_primCall "Char.ord" (#"0") +! _primCall "Int64.toInt.unchecked" (i))
  fun intToHexDigit i =
    if i < 10 then
      _primCall "Char.chr.unchecked" (_primCall "Char.ord" (#"0") +! _primCall "Int64.toInt.unchecked" (i))
    else
      _primCall "Char.chr.unchecked" (_primCall "Char.ord" (#"A") -! 10 +! _primCall "Int64.toInt.unchecked" (i))
  fun fmtBIN (0 : int) = "0"
    | fmtBIN x =
        let
          val initialBufSize = 65
          val radix = 2
          val buf = _primCall "CharArray.alloc" (initialBufSize)
          fun goPositive (i, 0) = i +! 1
            | goPositive (i, x) =
                let val r = rem' (x, radix)
                in _primCall "Unsafe.CharArray.update" (buf, i, intToDigit r)
                 ; goPositive (i -! 1, quot' (x, radix))
                end
          fun goNegative (i, 0) =
                ( _primCall "Unsafe.CharArray.update" (buf, i, #"~")
                ; i
                )
            | goNegative (i, x) =
                let val r = ~! (rem' (x, radix))
                in _primCall "Unsafe.CharArray.update" (buf, i, intToDigit r)
                 ; goNegative (i -! 1, quot' (x, radix))
                end
          val i = if x < 0 then
                    goNegative (initialBufSize -! 1, x)
                  else
                    goPositive (initialBufSize -! 1, x)
          val n = initialBufSize -! i
          val buf2 = _primCall "CharArray.alloc" (n)
        in
          _primCall "CharArray.copy" (buf2, 0, buf, i, n)
        ; _primCall "CharArray.unsafeFreeze" (buf2)
        end
  fun fmtOCT (0 : int) = "0"
    | fmtOCT x =
        let
          (* ~1_000_000_000_000_000_000_000 *)
          val initialBufSize = 23
          val radix = 8
          val buf = _primCall "CharArray.alloc" (initialBufSize)
          fun goPositive (i, 0) = i +! 1
            | goPositive (i, x) =
                let val r = rem' (x, radix)
                in _primCall "Unsafe.CharArray.update" (buf, i, intToDigit r)
                 ; goPositive (i -! 1, quot' (x, radix))
                end
          fun goNegative (i, 0) =
                ( _primCall "Unsafe.CharArray.update" (buf, i, #"~")
                ; i
                )
            | goNegative (i, x) =
                let val r = ~! (rem' (x, radix))
                in _primCall "Unsafe.CharArray.update" (buf, i, intToDigit r)
                 ; goNegative (i -! 1, quot' (x, radix))
                end
          val i = if x < 0 then
                    goNegative (initialBufSize -! 1, x)
                  else
                    goPositive (initialBufSize -! 1, x)
          val n = initialBufSize -! i
          val buf2 = _primCall "CharArray.alloc" (n)
        in
          _primCall "CharArray.copy" (buf2, 0, buf, i, n)
        ; _primCall "CharArray.unsafeFreeze" (buf2)
        end
  fun fmtDEC (0 : int) = "0"
    | fmtDEC x =
        let
          (* ~9_223_372_036_854_775_808 *)
          val initialBufSize = 20
          val radix = 10
          val buf = _primCall "CharArray.alloc" (initialBufSize)
          fun goPositive (i, 0) = i +! 1
            | goPositive (i, x) =
                let val r = rem' (x, radix)
                in _primCall "Unsafe.CharArray.update" (buf, i, intToDigit r)
                 ; goPositive (i -! 1, quot' (x, radix))
                end
          fun goNegative (i, 0) =
                ( _primCall "Unsafe.CharArray.update" (buf, i, #"~")
                ; i
                )
            | goNegative (i, x) =
                let val r = ~! (rem' (x, radix))
                in _primCall "Unsafe.CharArray.update" (buf, i, intToDigit r)
                 ; goNegative (i -! 1, quot' (x, radix))
                end
          val i = if x < 0 then
                    goNegative (initialBufSize -! 1, x)
                  else
                    goPositive (initialBufSize -! 1, x)
          val n = initialBufSize -! i
          val buf2 = _primCall "CharArray.alloc" (n)
        in
          _primCall "CharArray.copy" (buf2, 0, buf, i, n)
        ; _primCall "CharArray.unsafeFreeze" (buf2)
        end
  fun fmtHEX (0 : int) = "0"
    | fmtHEX x =
        let
          (* ~8000_0000_0000_0000 *)
          val initialBufSize = 17
          val radix = 16
          val buf = _primCall "CharArray.alloc" (initialBufSize)
          fun goPositive (i, 0) = i +! 1
            | goPositive (i, x) =
                let val r = rem' (x, radix)
                in _primCall "Unsafe.CharArray.update" (buf, i, intToHexDigit r)
                 ; goPositive (i -! 1, quot' (x, radix))
                end
          fun goNegative (i, 0) =
                ( _primCall "Unsafe.CharArray.update" (buf, i, #"~")
                ; i
                )
            | goNegative (i, x) =
                let val r = ~! (rem' (x, radix))
                in _primCall "Unsafe.CharArray.update" (buf, i, intToHexDigit r)
                 ; goNegative (i -! 1, quot' (x, radix))
                end
          val i = if x < 0 then
                    goNegative (initialBufSize -! 1, x)
                  else
                    goPositive (initialBufSize -! 1, x)
          val n = initialBufSize -! i
          val buf2 = _primCall "CharArray.alloc" (n)
        in
          _primCall "CharArray.copy" (buf2, 0, buf, i, n)
        ; _primCall "CharArray.unsafeFreeze" (buf2)
        end
in
  val toString = fmtDEC
  fun fmt StringCvt.BIN = fmtBIN
    | fmt StringCvt.OCT = fmtOCT
    | fmt StringCvt.DEC = fmtDEC
    | fmt StringCvt.HEX = fmtHEX
end
end; (* structure Int64 *)
