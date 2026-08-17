signature WORD = sig
    eqtype word
    val wordSize : int
    (* val toLarge : word -> LargeWord.word; defined in word.sml *)
    (* val toLargeX : word -> LargeWord.word; defined in word.sml *)
    (* val toLargeWord : word -> LargeWord.word; defined in word.sml *)
    (* val toLargeWordX : word -> LargeWord.word; defined in word.sml *)
    (* val fromLarge : LargeWord.word -> word; defined in word.sml *)
    (* val fromLargeWord : LargeWord.word -> word; defined in word.sml *)
    (* val toLargeInt *)
    (* val toLargeIntX *)
    (* val fromLargeInt *)
    val toInt : word -> int
    val toIntX : word -> int
    val fromInt : int -> word
    val andb : word * word -> word
    val orb : word * word -> word
    val xorb : word * word -> word
    val notb : word -> word
    val << : word * Word.word -> word
    val >> : word * Word.word -> word
    val ~>> : word * Word.word -> word
    val + : word * word -> word
    val - : word * word -> word
    val * : word * word -> word
    val div : word * word -> word
    val mod : word * word -> word
    val compare : word * word -> order
    val < : word * word -> bool
    val <= : word * word -> bool
    val > : word * word -> bool
    val >= : word * word -> bool
    val ~ : word -> word
    val min : word * word -> word
    val max : word * word -> word
    val fmt : StringCvt.radix -> word -> string
    val toString : word -> string
    (* val scan : StringCvt.radix -> (char, 'a) StringCvt.reader -> (word, 'a) StringCvt.reader; defined in scan-num.sml *)
    (* val fromString : string -> word option; defined in scan-num.sml *)
end;

structure Word :> WORD where type word = word = struct
open Word (* +, -, *, div, mod, ~, <, <=, >, >= *)
val wordSize : int = 32
(* toLarge, toLargeX, toLargeWord, toLargeWordX, fromLarge, fromLargeWord, toLargeInt, toLargeIntX, fromLargeInt *)
val toInt : word -> int = fn x => if x >= 0wx80000000 then
                                      raise Overflow
                                  else
                                      _primCall "Word.toInt.unchecked" (x)
val toIntX : word -> int = fn x => _primCall "Word.toIntX.unchecked" (x)
val fromInt : int -> word = fn x => _primCall "Word.fromInt" (x)
val andb : word * word -> word = fn (x, y) => _primCall "Word.andb" (x, y)
val orb : word * word -> word = fn (x, y) => _primCall "Word.orb" (x, y)
val xorb : word * word -> word = fn (x, y) => _primCall "Word.xorb" (x, y)
val notb : word -> word = fn x => _primCall "Word.notb" (x)
val << : word * word -> word = fn (x, y) => if y >= 0w32 then
                                                0w0
                                            else
                                                _primCall "Word.<<.unchecked" (x, y)
val >> : word * word -> word = fn (x, y) => if y >= 0w32 then
                                                0w0
                                            else
                                                _primCall "Word.>>.unchecked" (x, y)
val ~>> : word * word -> word = fn (x, y) => if y >= 0w31 then
                                                 _primCall "Word.~>>.unchecked" (x, 0w31)
                                             else
                                                 _primCall "Word.~>>.unchecked" (x, y)
val compare : word * word -> order = fn (x, y) => if x = y then
                                                      EQUAL
                                                  else if x < y then
                                                      LESS
                                                  else
                                                      GREATER
val min : word * word -> word = fn (x, y) => if x < y then
                                                 x
                                             else
                                                 y
val max : word * word -> word = fn (x, y) => if x < y then
                                                 y
                                             else
                                                 x
local
  infix 6 +! -!
  fun x +! y = _primCall "Int.+.wrapping" (x, y)
  fun x -! y = _primCall "Int.-.wrapping" (x, y)
  fun ~! x = _primCall "Int.~.wrapping" (x)
  fun div' (x, y) = _primCall "Word.div.unchecked" (x, y)
  fun mod' (x, y) = _primCall "Word.mod.unchecked" (x, y)
  fun wordToDigit i =
    _primCall "Char.chr.unchecked" (_primCall "Char.ord" (#"0") +! _primCall "Word.toInt.unchecked" (i))
  fun wordToHexDigit i =
    if i < 0w10 then
      _primCall "Char.chr.unchecked" (_primCall "Char.ord" (#"0") +! _primCall "Word.toInt.unchecked" (i))
    else
      _primCall "Char.chr.unchecked" (_primCall "Char.ord" (#"A") -! 10 +! _primCall "Word.toInt.unchecked" (i))
  fun fmtBIN 0w0 = "0"
    | fmtBIN x =
        let
          val initialBufSize = 32
          val radix = 0w2
          val buf = _primCall "CharArray.alloc" (initialBufSize)
          fun go (i, 0w0) = i +! 1
            | go (i, x) =
                let val r = mod' (x, radix)
                in _primCall "Unsafe.CharArray.update" (buf, i, wordToDigit r)
                 ; go (i -! 1, div' (x, radix))
                end
          val i = go (initialBufSize -! 1, x)
          val n = initialBufSize -! i
          val buf2 = _primCall "CharArray.alloc" (n)
        in
          _primCall "CharArray.copy" (buf2, 0, buf, i, n)
        ; _primCall "CharArray.unsafeFreeze" (buf2)
        end
  fun fmtOCT 0w0 = "0"
    | fmtOCT x =
        let
          (* 37777777777 *)
          val initialBufSize = 11
          val radix = 0w8
          val buf = _primCall "CharArray.alloc" (initialBufSize)
          fun go (i, 0w0) = i +! 1
            | go (i, x) =
                let val r = mod' (x, radix)
                in _primCall "Unsafe.CharArray.update" (buf, i, wordToDigit r)
                 ; go (i -! 1, div' (x, radix))
                end
          val i = go (initialBufSize -! 1, x)
          val n = initialBufSize -! i
          val buf2 = _primCall "CharArray.alloc" (n)
        in
          _primCall "CharArray.copy" (buf2, 0, buf, i, n)
        ; _primCall "CharArray.unsafeFreeze" (buf2)
        end
  fun fmtDEC 0w0 = "0"
    | fmtDEC x =
        let
          (* 4294967295 *)
          val initialBufSize = 10
          val radix = 0w10
          val buf = _primCall "CharArray.alloc" (initialBufSize)
          fun go (i, 0w0) = i +! 1
            | go (i, x) =
                let val r = mod' (x, radix)
                in _primCall "Unsafe.CharArray.update" (buf, i, wordToDigit r)
                 ; go (i -! 1, div' (x, radix))
                end
          val i = go (initialBufSize -! 1, x)
          val n = initialBufSize -! i
          val buf2 = _primCall "CharArray.alloc" (n)
        in
          _primCall "CharArray.copy" (buf2, 0, buf, i, n)
        ; _primCall "CharArray.unsafeFreeze" (buf2)
        end
  fun fmtHEX 0w0 = "0"
    | fmtHEX x =
        let
          (* ffffffff *)
          val initialBufSize = 8
          val radix = 0w16
          val buf = _primCall "CharArray.alloc" (initialBufSize)
          fun go (i, 0w0) = i +! 1
            | go (i, x) =
                let val r = mod' (x, radix)
                in _primCall "Unsafe.CharArray.update" (buf, i, wordToHexDigit r)
                 ; go (i -! 1, div' (x, radix))
                end
          val i = go (initialBufSize -! 1, x)
          val n = initialBufSize -! i
          val buf2 = _primCall "CharArray.alloc" (n)
        in
          _primCall "CharArray.copy" (buf2, 0, buf, i, n)
        ; _primCall "CharArray.unsafeFreeze" (buf2)
        end
in
  val toString = fmtHEX
  fun fmt StringCvt.BIN = fmtBIN
    | fmt StringCvt.OCT = fmtOCT
    | fmt StringCvt.DEC = fmtDEC
    | fmt StringCvt.HEX = fmtHEX
end
(* scan, fromString *)
end; (* structure Word *)

structure Word64 :> WORD where type word = Word64.word = struct
open Word64 (* +, -, *, div, mod, ~, <, <=, >, >= *)
val wordSize : int = 64
(* toLarge, toLargeX, toLargeWord, toLargeWordX, fromLarge, fromLargeWord, toLargeInt, toLargeIntX, fromLargeInt *)
val toInt : word -> int = fn x => if x >= 0wx80000000 then
                                      raise Overflow
                                  else
                                      _primCall "Word64.toInt.unchecked" (x)
val toIntX : word -> int = fn x => _primCall "Word64.toIntX.unchecked" (x)
val fromInt : int -> word = fn x => _primCall "Word64.fromInt" (x)
val andb : word * word -> word = fn (x, y) => _primCall "Word64.andb" (x, y)
val orb : word * word -> word = fn (x, y) => _primCall "Word64.orb" (x, y)
val xorb : word * word -> word = fn (x, y) => _primCall "Word64.xorb" (x, y)
val notb : word -> word = fn x => _primCall "Word64.notb" (x)
val << : word * Word.word -> word = fn (x, y) =>
    if y .Word.>=. 0w64 then
        0w0
    else
        _primCall "Word64.<<.unchecked.w64" (x, _primCall "Word.toWord64" (y))
val >> : word * Word.word -> word = fn (x, y) =>
    if y .Word.>=. 0w64 then
        0w0
    else
        _primCall "Word64.>>.unchecked.w64" (x, _primCall "Word.toWord64" (y))
val ~>> : word * Word.word -> word = fn (x, y) =>
    if y .Word.>=. 0w63 then
        _primCall "Word64.~>>.unchecked.w64" (x, 0w63)
    else
        _primCall "Word64.~>>.unchecked.w64" (x, _primCall "Word.toWord64" (y))
val compare : word * word -> order = fn (x, y) => if x = y then
                                                      EQUAL
                                                  else if x < y then
                                                      LESS
                                                  else
                                                      GREATER
val min : word * word -> word = fn (x, y) => if x < y then
                                                 x
                                             else
                                                 y
val max : word * word -> word = fn (x, y) => if x < y then
                                                 y
                                             else
                                                 x
local
  infix 6 +! -!
  fun x +! y = _primCall "Int.+.wrapping" (x, y)
  fun x -! y = _primCall "Int.-.wrapping" (x, y)
  fun ~! x = _primCall "Int.~.wrapping" (x)
  fun div' (x, y) = _primCall "Word64.div.unchecked" (x, y)
  fun mod' (x, y) = _primCall "Word64.mod.unchecked" (x, y)
  fun wordToDigit i =
    _primCall "Char.chr.unchecked" (_primCall "Char.ord" (#"0") +! _primCall "Word64.toInt.unchecked" (i))
  fun wordToHexDigit i =
    if i < 0w10 then
      _primCall "Char.chr.unchecked" (_primCall "Char.ord" (#"0") +! _primCall "Word64.toInt.unchecked" (i))
    else
      _primCall "Char.chr.unchecked" (_primCall "Char.ord" (#"A") -! 10 +! _primCall "Word64.toInt.unchecked" (i))
  fun fmtBIN 0w0 = "0"
    | fmtBIN x =
        let
          val initialBufSize = 64
          val radix = 0w2
          val buf = _primCall "CharArray.alloc" (initialBufSize)
          fun go (i, 0w0) = i +! 1
            | go (i, x) =
                let val r = mod' (x, radix)
                in _primCall "Unsafe.CharArray.update" (buf, i, wordToDigit r)
                 ; go (i -! 1, div' (x, radix))
                end
          val i = go (initialBufSize -! 1, x)
          val n = initialBufSize -! i
          val buf2 = _primCall "CharArray.alloc" (n)
        in
          _primCall "CharArray.copy" (buf2, 0, buf, i, n)
        ; _primCall "CharArray.unsafeFreeze" (buf2)
        end
  fun fmtOCT 0w0 = "0"
    | fmtOCT x =
        let
          (* 1777777777777777777777 *)
          val initialBufSize = 22
          val radix = 0w8
          val buf = _primCall "CharArray.alloc" (initialBufSize)
          fun go (i, 0w0) = i +! 1
            | go (i, x) =
                let val r = mod' (x, radix)
                in _primCall "Unsafe.CharArray.update" (buf, i, wordToDigit r)
                 ; go (i -! 1, div' (x, radix))
                end
          val i = go (initialBufSize -! 1, x)
          val n = initialBufSize -! i
          val buf2 = _primCall "CharArray.alloc" (n)
        in
          _primCall "CharArray.copy" (buf2, 0, buf, i, n)
        ; _primCall "CharArray.unsafeFreeze" (buf2)
        end
  fun fmtDEC 0w0 = "0"
    | fmtDEC x =
        let
          (* 18446744073709551615 *)
          val initialBufSize = 20
          val radix = 0w10
          val buf = _primCall "CharArray.alloc" (initialBufSize)
          fun go (i, 0w0) = i +! 1
            | go (i, x) =
                let val r = mod' (x, radix)
                in _primCall "Unsafe.CharArray.update" (buf, i, wordToDigit r)
                 ; go (i -! 1, div' (x, radix))
                end
          val i = go (initialBufSize -! 1, x)
          val n = initialBufSize -! i
          val buf2 = _primCall "CharArray.alloc" (n)
        in
          _primCall "CharArray.copy" (buf2, 0, buf, i, n)
        ; _primCall "CharArray.unsafeFreeze" (buf2)
        end
  fun fmtHEX 0w0 = "0"
    | fmtHEX x =
        let
          (* ffffffffffffffff *)
          val initialBufSize = 16
          val radix = 0w16
          val buf = _primCall "CharArray.alloc" (initialBufSize)
          fun go (i, 0w0) = i +! 1
            | go (i, x) =
                let val r = mod' (x, radix)
                in _primCall "Unsafe.CharArray.update" (buf, i, wordToHexDigit r)
                 ; go (i -! 1, div' (x, radix))
                end
          val i = go (initialBufSize -! 1, x)
          val n = initialBufSize -! i
          val buf2 = _primCall "CharArray.alloc" (n)
        in
          _primCall "CharArray.copy" (buf2, 0, buf, i, n)
        ; _primCall "CharArray.unsafeFreeze" (buf2)
        end
in
  val toString = fmtHEX
  fun fmt StringCvt.BIN = fmtBIN
    | fmt StringCvt.OCT = fmtOCT
    | fmt StringCvt.DEC = fmtDEC
    | fmt StringCvt.HEX = fmtHEX
end
(* scan, fromString *)
end; (* structure Word64 *)
