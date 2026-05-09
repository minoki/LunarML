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
    (*
    val fmt : StringCvt.radix -> word -> string
    val toString : word -> string
    *)
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
(* fmt, toString, scan, fromString *)
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
        _primCall "Word64.<<.unchecked" (x, y)
val >> : word * Word.word -> word = fn (x, y) =>
    if y .Word.>=. 0w64 then
        0w0
    else
        _primCall "Word64.>>.unchecked" (x, y)
val ~>> : word * Word.word -> word = fn (x, y) =>
    if y .Word.>=. 0w63 then
        _primCall "Word64.~>>.unchecked" (x, 0w31)
    else
        _primCall "Word64.~>>.unchecked" (x, y)
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
(* fmt, toString, scan, fromString *)
end; (* structure Word *)
