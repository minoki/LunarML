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
                                      Unsafe.cast x
val toIntX : word -> int = fn x => _primCall "Int32.toInt.unchecked" (JavaScript.toInt32 (JavaScript.fromWord x))
val fromInt : int -> word = fn x => Unsafe.cast (JavaScript.toUint32 (JavaScript.fromInt x))
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
                                                 if x >= 0wx80000000 then
                                                     0wxFFFFFFFF
                                                 else
                                                     0w0
                                             else
                                                 Unsafe.cast (JavaScript.toUint32 (JavaScript.>> (JavaScript.fromWord x, JavaScript.fromWord y)))
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
fun fmtBIN (x : word) : string =
  let val s = JavaScript.unsafeFromValue (JavaScript.method (JavaScript.fromWord x, "toString") #[JavaScript.fromInt 2])
  in JavaScript.encodeUtf8 s
  end
fun fmtOCT (x : word) : string =
  let val s = JavaScript.unsafeFromValue (JavaScript.method (JavaScript.fromWord x, "toString") #[JavaScript.fromInt 8])
  in JavaScript.encodeUtf8 s
  end
fun fmtDEC (x : word) : string =
  let val s = JavaScript.unsafeFromValue (JavaScript.method (JavaScript.fromWord x, "toString") #[])
  in JavaScript.encodeUtf8 s
  end
fun toString (x : word) : string =
  let val s = JavaScript.unsafeFromValue (JavaScript.method (JavaScript.fromWord x, "toString") #[JavaScript.fromInt 16])
      val s = JavaScript.method (JavaScript.fromString16 s, "toUpperCase") #[]
  in JavaScript.encodeUtf8 (JavaScript.unsafeFromValue s : String16.string)
  end
fun fmt StringCvt.BIN = fmtBIN
  | fmt StringCvt.OCT = fmtOCT
  | fmt StringCvt.DEC = fmtDEC
  | fmt StringCvt.HEX = toString
(* scan, fromString *)
end; (* structure Word *)
