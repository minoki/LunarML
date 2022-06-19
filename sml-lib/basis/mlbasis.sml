structure Word = struct
type word = word
fun ~ x = _primCall "Word.~" (x)
fun x + y = _primCall "Word.+" (x, y)
fun x - y = _primCall "Word.-" (x, y)
fun x * y = _primCall "Word.*" (x, y)
fun x div y = _primCall "call2" (_Prim.Word.div, x, y)
fun x mod y = _primCall "call2" (_Prim.Word.mod, x, y)
fun x < y = _primCall "call2" (_Prim.Word.<, x, y)
fun x > y = y < x
fun x <= y = Bool.not (y < x)
fun x >= y = Bool.not (x < y)
end
local
fun fromWord (x : word) = x
in
_equality word = fn (x, y) => _primCall "Word.=" (x, y);
_overload "Word" [word] { + = Word.+
                        , - = Word.-
                        , * = Word.*
                        , div = Word.div
                        , mod = Word.mod
                        , ~ = Word.~
                        , < = Word.<
                        , <= = Word.<=
                        , > = Word.>
                        , >= = Word.>=
                        , fromWord = fromWord
                        };
end

structure Real = struct
type real = real
fun ~ x = _primCall "Real.~" (x)
val abs = _Prim.Real.abs
fun x + y = _primCall "Real.+" (x, y)
fun x - y = _primCall "Real.-" (x, y)
fun x * y = _primCall "Real.*" (x, y)
fun x / y = _primCall "Real./" (x, y)
fun x < y = _primCall "Real.<" (x, y)
fun x <= y = _primCall "Real.<=" (x, y)
fun x > y = _primCall "Real.>" (x, y)
fun x >= y = _primCall "Real.>=" (x, y)
end
_overload "Real" [real] { + = Real.+
                        , - = Real.-
                        , * = Real.*
                        , / = Real./
                        , abs = Real.abs
                        , ~ = Real.~
                        , < = Real.<
                        , <= = Real.<=
                        , > = Real.>
                        , >= = Real.>=
                        };

structure Char = struct
type char = char
fun x < y = _primCall "Char.<" (x, y)
fun x <= y = _primCall "Char.<=" (x, y)
fun x > y = _primCall "Char.>" (x, y)
fun x >= y = _primCall "Char.>=" (x, y)
end
_equality char = fn (x, y) => _primCall "Char.=" (x, y);
_overload "Char" [char] { < = Char.<
                        , <= = Char.<=
                        , > = Char.>
                        , >= = Char.>=
                        };

structure String = struct
type string = string
fun x < y = _primCall "String.<" (x, y)
fun x <= y = _primCall "String.<=" (x, y)
fun x > y = _primCall "String.>" (x, y)
fun x >= y = _primCall "String.>=" (x, y)
fun x ^ y = _primCall "String.^" (x, y)
fun size x = _primCall "String.size" (x)
fun str (x : char) : string = _primCall "Unsafe.cast" (x)
end
_equality string = fn (x, y) => _primCall "String.=" (x, y);
_overload "String" [string] { < = String.<
                            , <= = String.<=
                            , > = String.>
                            , >= = String.>=
                            };

structure LunarML : sig
              val assumePure : 'a -> 'a
              val assumeDiscardable : 'a -> 'a
          end = struct
val assumePure = _Prim.assumePure
val assumeDiscardable = _Prim.assumeDiscardable
end

structure Lua : sig
              type value
              exception LuaError of value
              exception TypeError of string
              val sub : value * value -> value  (* t[k] *)
              val field : value * string -> value  (* t[k] *)
              val set : value * value * value -> unit  (* t[k] = v *)
              val global : string -> value  (* _ENV[name] *)
              val call : value -> value vector -> value vector  (* f(args) *)
              val method : value * string -> value vector -> value vector  (* f:name(args) *)
              val NIL : value  (* Lua nil *)
              val isNil : value -> bool  (* x == nil *)
              val isFalsy : value -> bool  (* not x *)
              val fromBool : bool -> value
              val fromInt : int -> value
              val fromWord : word -> value
              val fromReal : real -> value
              val fromChar : char -> value
              val fromString : string -> value
              val unsafeToValue : 'a -> value
              val unsafeFromValue : value -> 'a
              val newTable : unit -> value  (* {} *)
              val function : (value vector -> value vector) -> value
              val + : value * value -> value
              val - : value * value -> value
              val * : value * value -> value
              val / : value * value -> value
              val // : value * value -> value
              val % : value * value -> value
              val pow : value * value -> value  (* x ^ y *)
              val unm : value -> value  (* unary minus *)
              val andb : value * value -> value  (* x & y *)
              val orb : value * value -> value  (* x | y *)
              val xorb : value * value -> value  (* x ~ y *)
              val notb : value -> value  (* ~ x *)
              val << : value * value -> value
              val >> : value * value -> value
              val == : value * value -> bool
              val ~= : value * value -> bool
              val < : value * value -> bool
              val > : value * value -> bool
              val <= : value * value -> bool
              val >= : value * value -> bool
              val concat : value * value -> value  (* x .. y *)
              val length : value -> value  (* #x *)
              val typeof : value -> string (* type *)
              val checkString : value -> string
              val checkBoolean : value -> bool
              val checkInt : value -> int
              val checkWord : value -> word
              val checkReal : value -> real
              val optString : value -> string option
              structure Lib : sig
                            val assert : value
                            val error : value
                            val getmetatable : value
                            val math : value
                            val pairs : value
                            val pcall : value
                            val require : value
                            val setmetatable : value
                            val string : value
                            val table : value
                            val tonumber : value
                            val tostring : value
                            val type' : value
                            structure math : sig
                                          val abs : value
                                          val atan : value
                                          val ceil : value
                                          val floor : value
                                          val huge : value
                                          val log : value
                                          val maxinteger : value
                                          val mininteger : value
                                          val modf : value
                                          val type' : value
                                      end
                            structure string : sig
                                          val byte : value
                                          val char : value
                                          val find : value
                                          val format : value
                                          val gsub : value
                                          val match : value
                                          val sub : value
                                      end
                            structure table : sig
                                          val concat : value
                                          val pack : value
                                          val unpack : value
                                      end
                            val lfs : value option (* LuaFileSystem *)
                        end
          end = struct
type value = _Prim.Lua.value
exception LuaError = _Prim.Lua.LuaError
val global = _Prim.Lua.global
val call = _Prim.Lua.call
val method = _Prim.Lua.method
val NIL = _Prim.Lua.NIL
val newTable = _Prim.Lua.newTable
val function = _Prim.Lua.function
fun unsafeToValue x : value = _primCall "Unsafe.cast" (x)
fun unsafeFromValue (x : value) = _primCall "Unsafe.cast" (x)
val fromBool : bool -> value = unsafeToValue
val fromInt : int -> value = unsafeToValue
val fromWord : word -> value = unsafeToValue
val fromReal : real -> value = unsafeToValue
val fromString : string -> value = unsafeToValue
val fromChar : char -> value = unsafeToValue
fun sub (t, k) = _primCall "Lua.sub" (t, k)
fun field (t : value, name : string) = sub (t, fromString name)
fun set (t, k, v) = _primCall "Lua.set" (t, k, v)
fun isNil x = _primCall "Lua.isNil" (x)
fun isFalsy x = _primCall "Lua.isFalsy" (x)
fun x + y = _primCall "Lua.+" (x, y)
fun x - y = _primCall "Lua.-" (x, y)
fun x * y = _primCall "Lua.*" (x, y)
fun x / y = _primCall "Lua./" (x, y)
fun // (x, y) = _primCall "Lua.//" (x, y)
fun % (x, y) = _primCall "Lua.%" (x, y)
fun pow (x, y) = _primCall "Lua.pow" (x, y)
fun unm x = _primCall "Lua.unm" (x)
fun andb (x, y) = _primCall "Lua.andb" (x, y)
fun orb (x, y) = _primCall "Lua.orb" (x, y)
fun xorb (x, y) = _primCall "Lua.xorb" (x, y)
fun notb x = _primCall "Lua.notb" (x)
fun << (x, y) = _primCall "Lua.<<" (x, y)
fun >> (x, y) = _primCall "Lua.>>" (x, y)
fun == (x, y) = _primCall "Lua.==" (x, y)
fun ~= (x, y) = _primCall "Lua.~=" (x, y)
fun x < y = _primCall "Lua.<" (x, y)
fun x > y = _primCall "Lua.>" (x, y)
fun x <= y = _primCall "Lua.<=" (x, y)
fun x >= y = _primCall "Lua.>=" (x, y)
fun concat (x, y) = _primCall "Lua.concat" (x, y)
fun length x = _primCall "Lua.length" (x)
structure Lib = struct
val assert = _Prim.Lua.Lib.assert
val error = _Prim.Lua.Lib.error
val getmetatable = _Prim.Lua.Lib.getmetatable
val pairs = _Prim.Lua.Lib.pairs
val pcall = _Prim.Lua.Lib.pcall
val setmetatable = _Prim.Lua.Lib.setmetatable
val math = _Prim.Lua.Lib.math
val string = _Prim.Lua.Lib.string
val table = _Prim.Lua.Lib.table
val require = LunarML.assumeDiscardable (global "require")
val tonumber = LunarML.assumeDiscardable (global "tonumber")
val tostring = LunarML.assumeDiscardable (global "tostring")
val type' = LunarML.assumeDiscardable (global "type")
structure math = struct
val abs = _Prim.Lua.Lib.math.abs
val type' = _Prim.Lua.Lib.math.type'
val maxinteger = _Prim.Lua.Lib.math.maxinteger
val mininteger = _Prim.Lua.Lib.math.mininteger
val atan = LunarML.assumeDiscardable (field (math, "atan"))
val ceil = LunarML.assumeDiscardable (field (math, "ceil"))
val floor = LunarML.assumeDiscardable (field (math, "floor"))
val huge = LunarML.assumeDiscardable (field (math, "huge"))
val log = LunarML.assumeDiscardable (field (math, "log"))
val modf = LunarML.assumeDiscardable (field (math, "modf"))
end
structure string = struct
val format = _Prim.Lua.Lib.string.format
val byte = LunarML.assumeDiscardable (field (string, "byte"))
val char = LunarML.assumeDiscardable (field (string, "char"))
val find = LunarML.assumeDiscardable (field (string, "find"))
val gsub = LunarML.assumeDiscardable (field (string, "gsub"))
val match = LunarML.assumeDiscardable (field (string, "match"))
val sub = LunarML.assumeDiscardable (field (string, "sub"))
end
structure table = struct
val pack = _Prim.Lua.Lib.table.pack
val unpack = _Prim.Lua.Lib.table.unpack
val concat = LunarML.assumeDiscardable (field (table, "concat"))
end
val lfs = LunarML.assumeDiscardable (let val results = call pcall #[require, fromString "lfs"]
                                         val ok = unsafeFromValue (Vector.sub (results, 0)) : bool
                                     in if ok then
                                            SOME (Vector.sub (results, 1))
                                        else
                                            NONE
                                     end
                                    )
end
fun typeof x : string = let val results = call Lib.type' #[x]
                        in unsafeFromValue (Vector.sub (results, 0))
                        end
exception TypeError of string
fun checkString x : string = let val t = typeof x
                             in if t = "string" then
                                    unsafeFromValue x
                                else
                                    raise TypeError (String.^ ("expected a string, but got ", t))
                             end
fun checkBoolean x : bool = let val t = typeof x
                            in if t = "boolean" then
                                   unsafeFromValue x
                               else
                                   raise TypeError (String.^ ("expected a boolean, but got ", t))
                            end
fun checkInt x : int = let val results = call Lib.math.type' #[x]
                       in if == (Vector.sub (results, 0), fromString "integer") then
                              unsafeFromValue x
                          else
                              raise TypeError (String.^ ("expected an integer, but got ", typeof x))
                       end
fun checkWord x : word = let val results = call Lib.math.type' #[x]
                         in if == (Vector.sub (results, 0), fromString "integer") then
                                unsafeFromValue x
                            else
                                raise TypeError (String.^ ("expected an integer, but got ", typeof x))
                         end
fun checkReal x : real = let val results = call Lib.math.type' #[x]
                         in if == (Vector.sub (results, 0), fromString "float") then
                                unsafeFromValue x
                            else
                                raise TypeError (String.^ ("expected a real number, but got ", typeof x))
                         end
fun optString x : string option = let val t = typeof x
                                  in if t = "string" then
                                         SOME (unsafeFromValue x)
                                     else if t = "nil" then
                                         NONE
                                     else
                                         raise TypeError (String.^ ("expected a string, but got ", t))
                                  end
end;

structure StringCvt : sig
              datatype radix = BIN | OCT | DEC | HEX
              datatype realfmt = SCI of int option
                               | FIX of int option
                               | GEN of int option
                               | EXACT
              type ('a, 'b) reader = 'b -> ('a * 'b) option
          end = struct
datatype radix = BIN | OCT | DEC | HEX
datatype realfmt = SCI of int option
                 | FIX of int option
                 | GEN of int option
                 | EXACT
type ('a, 'b) reader = 'b -> ('a * 'b) option
end

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
    val sign : int -> int
    val sameSign : int * int -> bool
    val fmt : StringCvt.radix -> int -> string
    val toString : int -> string
    (* val scan : StringCvt.radix -> (char, 'a) StringCvt.reader -> (int, 'a) StringCvt.reader; defined in scan-num.sml *)
    (* val fromString : string -> int option; defined in scan-num.sml *)
end;

structure Int : INTEGER where type int = int = struct
open Int (* +, -, *, div, mod, ~, abs, <, <=, >, >=, fromInt *)
(* toLarge, fromLarge *)
val toInt : int -> int = fn x => x
val precision : int option = LunarML.assumeDiscardable
                                 (let fun computeWordSize (x : int, n) = if x = 0 then
                                                                             n
                                                                         else
                                                                             computeWordSize (Lua.unsafeFromValue (Lua.>> (Lua.fromInt x, Lua.fromInt 1)), n + 1)
                                  in SOME (computeWordSize (Lua.unsafeFromValue Lua.Lib.math.maxinteger, 1))
                                  end
                                 )
val minInt : int option = LunarML.assumeDiscardable (SOME (Lua.unsafeFromValue Lua.Lib.math.mininteger))
val maxInt : int option = LunarML.assumeDiscardable (SOME (Lua.unsafeFromValue Lua.Lib.math.maxinteger))
fun quot (x, y) = if (x >= 0 andalso y >= 0) orelse (x <= 0 andalso y <= 0) then
                      x div y (* raise Overflow if x = minInt and y = ~1, Div if y = 0 *)
                  else
                      if x = Lua.unsafeFromValue Lua.Lib.math.mininteger then
                          if y = 1 then
                              x
                          else
                              ~ (x div (~y)) (* y must be positive, so ~y cannot overflow *)
                      else
                          ~ ((~x) div y)
fun rem (x, y) = if y = ~1 then
                     0
                 else
                     x - quot (x, y) * y (* raise Div if y = 0 *)
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
fun fmt StringCvt.BIN x = raise Fail "StringCvt.BIN: not implemented yet"
  | fmt StringCvt.OCT x = if x >= 0 then
                              let val result = Lua.call Lua.Lib.string.format #[Lua.fromString "%o", Lua.fromInt x]
                              in Lua.unsafeFromValue (Vector.sub (result, 0))
                              end
                          else
                              let val result = Lua.call Lua.Lib.string.format #[Lua.fromString "~%o", Lua.unm (Lua.fromInt x)]
                              in Lua.unsafeFromValue (Vector.sub (result, 0))
                              end
  | fmt StringCvt.DEC x = let val result = Lua.call Lua.Lib.string.format #[Lua.fromString "%d", Lua.fromInt x]
                              val result = Lua.call Lua.Lib.string.gsub #[Vector.sub (result, 0), Lua.fromString "-", Lua.fromString "~"]
                          in Lua.unsafeFromValue (Vector.sub (result, 0))
                          end
  | fmt StringCvt.HEX x = if x >= 0 then
                              let val result = Lua.call Lua.Lib.string.format #[Lua.fromString "%X", Lua.fromInt x]
                              in Lua.unsafeFromValue (Vector.sub (result, 0))
                              end
                          else
                              let val result = Lua.call Lua.Lib.string.format #[Lua.fromString "~%X", Lua.unm (Lua.fromInt x)]
                              in Lua.unsafeFromValue (Vector.sub (result, 0))
                              end
fun toString (x : int) : string = let val result = Lua.call Lua.Lib.tostring #[Lua.fromInt x]
                                      val result = Lua.call Lua.Lib.string.gsub #[Vector.sub (result, 0), Lua.fromString "-", Lua.fromString "~"]
                                  in Lua.unsafeFromValue (Vector.sub (result, 0))
                                  end
(* scan *)
(*
fun fromString (s : string) : int option = let val result = Lua.call Lua.Lib.string.match #[Lua.fromString s, Lua.fromString "^%s*([%+~%-]?)([0-9]+)"]
                                           in if Lua.isNil (Vector.sub (result, 0)) then
                                                  NONE
                                              else
                                                  let val sign = Lua.unsafeFromValue (Vector.sub (result, 0)) : string
                                                      val digits = Lua.unsafeFromValue (Vector.sub (result, 1)) : string
                                                      val result' = if sign = "~" orelse sign = "-" then
                                                                        Lua.call Lua.Lib.tonumber #[Lua.fromString (String.^ ("-", digits))]
                                                                    else
                                                                        Lua.call Lua.Lib.tonumber #[Lua.fromString digits]
                                                  in SOME (Lua.unsafeFromValue (Vector.sub (result', 0)))
                                                  end
                                           end
*)
end; (* structure Int *)

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
val wordSize : int = LunarML.assumeDiscardable
                         (let fun computeWordSize (x : int, n : int) = if x = 0 then
                                                                           n
                                                                       else
                                                                           computeWordSize (Lua.unsafeFromValue (Lua.>> (Lua.fromInt x, Lua.fromInt 1)), Int.+ (n, 1))
                          in computeWordSize (Lua.unsafeFromValue Lua.Lib.math.maxinteger, 1)
                          end
                         )
(* toLarge, toLargeX, toLargeWord, toLargeWordX, fromLarge, fromLargeWord, toLargeInt, toLargeIntX, fromLargeInt *)
val toInt : word -> int = fn x => if Lua.< (Lua.fromWord x, Lua.fromWord 0w0) then
                                      raise Overflow
                                  else
                                      Lua.unsafeFromValue (Lua.fromWord x)
val toIntX : word -> int = fn x => Lua.unsafeFromValue (Lua.fromWord x)
val fromInt : int -> word = fn x => Lua.unsafeFromValue (Lua.fromInt x)
val andb : word * word -> word = fn (x, y) => Lua.unsafeFromValue (Lua.andb (Lua.fromWord x, Lua.fromWord y))
val orb : word * word -> word = fn (x, y) => Lua.unsafeFromValue (Lua.orb (Lua.fromWord x, Lua.fromWord y))
val xorb : word * word -> word = fn (x, y) => Lua.unsafeFromValue (Lua.xorb (Lua.fromWord x, Lua.fromWord y))
val notb : word -> word = fn x => Lua.unsafeFromValue (Lua.notb (Lua.fromWord x))
val << : word * word -> word = fn (x, y) => if y >= fromInt wordSize then
                                                0w0
                                            else
                                                Lua.unsafeFromValue (Lua.<< (Lua.fromWord x, Lua.fromWord y))
val >> : word * word -> word = fn (x, y) => if y >= fromInt wordSize then
                                                0w0
                                            else
                                                Lua.unsafeFromValue (Lua.>> (Lua.fromWord x, Lua.fromWord y))
val ~>> : word * word -> word = fn (x, y) => if y >= fromInt (Int.- (wordSize, 1)) then
                                                 if Lua.< (Lua.fromWord x, Lua.fromWord 0w0) then
                                                     ~(0w1)
                                                 else
                                                     0w0
                                             else
                                                 Lua.unsafeFromValue (Lua.// (Lua.fromWord x, Lua.fromWord (<< (0w1, y))))
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
fun fmt StringCvt.BIN x = raise Fail "StringCvt.BIN: not implemented yet"
  | fmt StringCvt.OCT x = let val result = Lua.call Lua.Lib.string.format #[Lua.fromString "%o", Lua.fromWord x]
                          in Lua.unsafeFromValue (Vector.sub (result, 0))
                          end
  | fmt StringCvt.DEC x = let val result = Lua.call Lua.Lib.string.format #[Lua.fromString "%u", Lua.fromWord x]
                          in Lua.unsafeFromValue (Vector.sub (result, 0))
                          end
  | fmt StringCvt.HEX x = let val result = Lua.call Lua.Lib.string.format #[Lua.fromString "%X", Lua.fromWord x]
                          in Lua.unsafeFromValue (Vector.sub (result, 0))
                          end
val toString : word -> string = fn x => Lua.unsafeFromValue (Vector.sub (Lua.call Lua.Lib.string.format #[Lua.fromString "%X", Lua.fromWord x], 0))
(* scan, fromString *)
end; (* structure Word *)

structure IEEEReal : sig
              exception Unordered
              datatype real_order = LESS | EQUAL | GREATER | UNORDERED
              datatype float_class = NAN | INF | ZERO | NORMAL | SUBNORMAL
              datatype rounding_mode = TO_NEAREST | TO_NEGINF | TO_POSINF | TO_ZERO
              type decimal_approx = { class : float_class, sign : bool, digits : int list, exp : int }
          end = struct
exception Unordered
datatype real_order = LESS | EQUAL | GREATER | UNORDERED
datatype float_class = NAN | INF | ZERO | NORMAL | SUBNORMAL
datatype rounding_mode = TO_NEAREST | TO_NEGINF | TO_POSINF | TO_ZERO
type decimal_approx = { class : float_class, sign : bool, digits : int list, exp : int }
end;

signature REAL = sig
    type real
    (* structure Math *)
    (* val radix : int *)
    (* val precision : int *)
    val maxFinite : real
    val minPos : real
    val minNormalPos : real
    val posInf : real
    val negInf : real
    val + : real * real -> real
    val - : real * real -> real
    val * : real * real -> real
    val / : real * real -> real
    (* val rem : real * real -> real *)
    (* val *+ : real * real * real -> real *)
    (* val *- : real * real * real -> real *)
    val ~ : real -> real
    val abs : real -> real
    (* val min : real * real -> real *)
    (* val max : real * real -> real *)
    val sign : real -> int
    val signBit : real -> bool
    val sameSign : real * real -> bool
    val copySign : real * real -> real
    val compare : real * real -> order
    val compareReal : real * real -> IEEEReal.real_order
    val < : real * real -> bool
    val <= : real * real -> bool
    val > : real * real -> bool
    val >= : real * real -> bool
    val == : real * real -> bool
    val != : real * real -> bool
    val ?= : real * real -> bool
    val unordered : real * real -> bool
    val isFinite : real -> bool
    val isNan : real -> bool
    val isNormal : real -> bool
    val class : real -> IEEEReal.float_class
    (* val toManExp : real -> { man : real, exp : int } *)
    (* val fromManExp : { man : real, exp : int } -> real *)
    (* val split : real -> { whole : real, frac : real } *)
    (* val realMod : real -> real *)
    (* val nextAfter : real * real -> real *)
    val checkFloat : real -> real
    val realFloor : real -> real
    val realCeil : real -> real
    val realTrunc : real -> real
    val realRound : real -> real
    val floor : real -> int
    val ceil : real -> int
    val trunc : real -> int
    val round : real -> int
    val toInt : IEEEReal.rounding_mode -> real -> int
    (* val toLargeInt : IEEEReal.rounding_mode -> real -> LargeInt.int *)
    val fromInt : int -> real
    (* val fromLargeInt : LargeInt.int -> real *)
    (* val toLarge : real -> LargeReal.real *)
    (* val fromLarge : IEEEReal.rounding_mode -> LargeReal.real -> real *)
    val fmt : StringCvt.realfmt -> real -> string
    val toString : real -> string
    (* val scan : (char, 'a) StringCvt.reader -> (real, 'a) StringCvt.reader; implemented in scan-num.sml *)
    (* val fromString : string -> real option; implemented in scan-num.sml *)
    (* val toDecimal : real -> IEEEReal.decimal_approx *)
    (* val fromDecimal : IEEEReal.decimal_approx -> real option *)
end;

structure Real : REAL where type real = real = struct
val posInf = Lua.unsafeFromValue Lua.Lib.math.huge : real
val negInf = Real.~ posInf
fun == (x, y) = Lua.== (Lua.fromReal x, Lua.fromReal y)
fun != (x, y) = Lua.~= (Lua.fromReal x, Lua.fromReal y)
infix 4 == !=
fun isNan x = x != x
fun ?= (x, y) = x == y orelse x != x orelse y != y (* EQUAL or UNORDERED *)
fun unordered (x, y) = x != x orelse y != y
fun isFinite x = negInf < x andalso x < posInf
val maxFinite = 0x1.fffffffffffffp1023 : real; (* approx. 1.7976931348623157e308; assuming binary64 *)
val minPos = 0x1p~1074 : real; (* approx. 5e~324; assuming binary64 *)
val minNormalPos = 0x1p~1022 : real; (* approx. 2.2250738585072014e~308; assuming binary64 *)
fun isNormal x = let val absX = abs x
                 in minNormalPos <= absX andalso absX < posInf
                 end
fun class x = if x == 0.0 then
                  IEEEReal.ZERO
              else
                  let val absX = abs x
                  in if absX < posInf then
                         (* normal or subnormal *)
                         if minNormalPos <= absX then
                             IEEEReal.NORMAL
                         else
                             IEEEReal.SUBNORMAL
                     else
                         (* infinity or NaN *)
                         if x != x then
                             IEEEReal.NAN
                         else
                             IEEEReal.INF
                  end
fun sign x = if x == 0.0 then
                 0
             else if x < 0.0 then
                 ~1
             else if x > 0.0 then
                 1
             else (* NaN *)
                 raise Domain
fun signBit x = if x < 0.0 then
                    true
                else if x > 0.0 then
                    false
                else
                    1.0 / x < 0.0 (* handle negative zero; NaN is not handled *)
fun sameSign (x, y) = signBit x = signBit y
fun copySign (x, y) = if signBit x = signBit y then
                          x
                      else
                          ~ x
fun compare (x, y) = if isNan x orelse isNan y then
                         raise IEEEReal.Unordered
                     else
                         if x < y then
                             LESS
                         else if x == y then
                             EQUAL
                         else
                             GREATER
fun compareReal (x, y) = if isNan x orelse isNan y then
                             IEEEReal.UNORDERED
                         else
                             if x < y then
                                 IEEEReal.LESS
                             else if x == y then
                                 IEEEReal.EQUAL
                             else
                                 IEEEReal.GREATER
fun checkFloat x = if isNan x then
                       raise Div
                   else if x == posInf orelse x == negInf then
                       raise Overflow
                   else
                       x
fun realFloor x = let val results = Lua.call Lua.Lib.math.floor #[Lua.fromReal x]
                      val result = Vector.sub (results, 0)
                      val result = Lua.unsafeFromValue (Lua.* (result, Lua.fromReal 1.0)) : real
                  in if result == 0.0 andalso 1.0 / x < 0.0 then
                         x (* negative zero *)
                     else
                         result
                  end
fun realCeil x = let val results = Lua.call Lua.Lib.math.ceil #[Lua.fromReal x]
                     val result = Vector.sub (results, 0)
                     val result = Lua.unsafeFromValue (Lua.* (result, Lua.fromReal 1.0)) : real
                 in if result == 0.0 andalso 1.0 / x < 0.0 then
                        ~0.0 (* negative zero *)
                    else
                        result
                 end
fun realTrunc x = let val results = Lua.call Lua.Lib.math.modf #[Lua.fromReal x]
                      val result = Vector.sub (results, 0)
                      val result = Lua.unsafeFromValue (Lua.* (result, Lua.fromReal 1.0)) : real
                  in if result == 0.0 andalso 1.0 / x < 0.0 then
                         ~0.0 (* negative zero *)
                     else
                         result
                  end
fun realRound x = let val results = Lua.call Lua.Lib.math.modf #[Lua.fromReal x]
                      val intPartRaw = Vector.sub (results, 0)
                      val intPartIsEven = Lua.== (Lua.% (intPartRaw, Lua.fromInt 2), Lua.fromInt 0)
                      val intPart = Lua.unsafeFromValue (Lua.* (intPartRaw, Lua.fromReal 1.0)) : real
                      val fracPart = Lua.unsafeFromValue (Vector.sub (results, 1)) : real
                      val absFracPart = abs fracPart
                  in if ~0.5 < fracPart andalso fracPart < 0.5 then
                         if intPart == 0.0 andalso 1.0 / x < 0.0 then
                             ~0.0 (* negative zero *)
                         else
                             (* intPart may be infinity *)
                             intPart
                     else if fracPart < ~0.5 orelse (fracPart == ~0.5 andalso not intPartIsEven)then
                         intPart - 1.0
                     else if fracPart > 0.5 orelse (fracPart == 0.5 andalso not intPartIsEven) then
                         intPart + 1.0
                     else (* ((fracPart == 0.5 orelse fracPart == ~0.5) andalso intPartIsEven) orelse isNan x *)
                         if x == ~0.5 then
                             ~0.0 (* negative zero *)
                         else
                             intPart
                  end
fun realFloor x = let val results = Lua.call Lua.Lib.math.floor #[Lua.fromReal x]
                      val result = Vector.sub (results, 0)
                      val result = Lua.unsafeFromValue (Lua.* (result, Lua.fromReal 1.0)) : real
                  in if result == 0.0 andalso 1.0 / x < 0.0 then
                         x (* negative zero *)
                     else
                         result
                  end
fun floor x = let val results = Lua.call Lua.Lib.math.floor #[Lua.fromReal x]
                  val result = Vector.sub (results, 0)
                  val isInteger = Lua.== (Vector.sub (Lua.call Lua.Lib.math.type' #[result], 0), Lua.fromString "integer")
              in if isInteger then
                     Lua.unsafeFromValue result : int
                 else
                     if isNan x then
                         raise Domain (* NaN *)
                     else
                         raise Overflow
              end
fun ceil x = let val results = Lua.call Lua.Lib.math.ceil #[Lua.fromReal x]
                 val result = Vector.sub (results, 0)
                 val isInteger = Lua.== (Vector.sub (Lua.call Lua.Lib.math.type' #[result], 0), Lua.fromString "integer")
             in if isInteger then
                    Lua.unsafeFromValue result : int
                else
                    if isNan x then
                        raise Domain (* NaN *)
                    else
                        raise Overflow
             end
fun trunc x = let val results = Lua.call Lua.Lib.math.modf #[Lua.fromReal x]
                  val result = Vector.sub (results, 0)
                  val isInteger = Lua.== (Vector.sub (Lua.call Lua.Lib.math.type' #[result], 0), Lua.fromString "integer")
              in if isInteger then
                     Lua.unsafeFromValue result : int
                 else
                     if isNan x then
                         raise Domain (* NaN *)
                     else
                         raise Overflow
              end
fun round x = let val results = Lua.call Lua.Lib.math.modf #[Lua.fromReal x]
                  val intPartRaw = Vector.sub (results, 0)
                  val isInteger = Lua.== (Vector.sub (Lua.call Lua.Lib.math.type' #[intPartRaw], 0), Lua.fromString "integer")
              in if isInteger then
                     let val intPartIsEven = Lua.== (Lua.% (intPartRaw, Lua.fromInt 2), Lua.fromInt 0)
                         val intPart = Lua.unsafeFromValue intPartRaw : int
                         val fracPart = Lua.unsafeFromValue (Vector.sub (results, 1)) : real
                         val absFracPart = abs fracPart
                     in if ~0.5 < fracPart andalso fracPart < 0.5 then
                            intPart
                        else if fracPart < ~0.5 orelse (fracPart == ~0.5 andalso not intPartIsEven)then
                            intPart - 1
                        else if fracPart > 0.5 orelse (fracPart == 0.5 andalso not intPartIsEven) then
                            intPart + 1
                        else (* ((fracPart == 0.5 orelse fracPart == ~0.5) andalso intPartIsEven) orelse isNan x *)
                            intPart
                     end
                 else
                     if isNan x then
                         raise Domain
                     else
                         raise Overflow
              end
fun toInt IEEEReal.TO_NEGINF = floor
  | toInt IEEEReal.TO_POSINF = ceil
  | toInt IEEEReal.TO_ZERO = trunc
  | toInt IEEEReal.TO_NEAREST = round
fun fromInt x = Lua.unsafeFromValue (Lua.* (Lua.fromInt x, Lua.fromReal 1.0)) : real
fun fmt (StringCvt.SCI prec) r = let val prec = Option.getOpt (prec, 6)
                                     val () = if prec < 0 then
                                                  raise Size
                                              else
                                                  ()
                                     val fmt = Vector.sub (Lua.call Lua.Lib.string.format #[Lua.fromString "%%.%dE", Lua.fromInt prec], 0)
                                     val result = Lua.call Lua.Lib.string.format #[fmt, Lua.fromReal r]
                                     val result = Lua.call Lua.Lib.string.gsub #[Vector.sub (result, 0), Lua.fromString "-", Lua.fromString "~"]
                                 in Lua.unsafeFromValue (Vector.sub (result, 0)) : string
                                 end
  | fmt (StringCvt.FIX prec) r = let val prec = Option.getOpt (prec, 6)
                                     val () = if prec < 0 then
                                                  raise Size
                                              else
                                                  ()
                                     val fmt = Vector.sub (Lua.call Lua.Lib.string.format #[Lua.fromString "%%.%df", Lua.fromInt prec], 0)
                                     val result = Lua.call Lua.Lib.string.format #[fmt, Lua.fromReal r]
                                     val result = Lua.call Lua.Lib.string.gsub #[Vector.sub (result, 0), Lua.fromString "-", Lua.fromString "~"]
                                 in Lua.unsafeFromValue (Vector.sub (result, 0)) : string
                                 end
  | fmt (StringCvt.GEN prec) r = let val prec = Option.getOpt (prec, 12)
                                     val () = if prec < 1 then
                                                  raise Size
                                              else
                                                  ()
                                     val fmt = Vector.sub (Lua.call Lua.Lib.string.format #[Lua.fromString "%%.%dG", Lua.fromInt prec], 0) (* TODO *)
                                     val result = Lua.call Lua.Lib.string.format #[fmt, Lua.fromReal r]
                                     val result = Lua.call Lua.Lib.string.gsub #[Vector.sub (result, 0), Lua.fromString "-", Lua.fromString "~"]
                                 in Lua.unsafeFromValue (Vector.sub (result, 0)) : string
                                 end
  | fmt StringCvt.EXACT r = raise Fail "Real.fmt StringCvt.EXACT: not implemented yet"
val toString = fmt (StringCvt.GEN NONE)
open Real (* +, -, *, /, ~, abs, <, <=, >, >= *)
end; (* structure Real *)

structure Math : sig
              type real = real
              val pi : real
              val e : real
              val sqrt : real -> real
              val sin : real -> real
              val cos : real -> real
              val tan : real -> real
              val asin : real -> real
              val acos : real -> real
              val atan : real -> real
              val atan2 : real * real -> real
              val exp : real -> real
              val pow : real * real -> real
              val ln : real -> real
              val log10 : real -> real
              val sinh : real -> real
              val cosh : real -> real
              val tanh : real -> real
          end = struct
type real = real
val pi : real = LunarML.assumeDiscardable (Lua.unsafeFromValue (Lua.field (Lua.Lib.math, "pi")))
val sqrt : real -> real = LunarML.assumeDiscardable (Lua.unsafeFromValue (Lua.field (Lua.Lib.math, "sqrt")))
val sin : real -> real = LunarML.assumeDiscardable (Lua.unsafeFromValue (Lua.field (Lua.Lib.math, "sin")))
val cos : real -> real = LunarML.assumeDiscardable (Lua.unsafeFromValue (Lua.field (Lua.Lib.math, "cos")))
val tan : real -> real = LunarML.assumeDiscardable (Lua.unsafeFromValue (Lua.field (Lua.Lib.math, "tan")))
val asin : real -> real = LunarML.assumeDiscardable (Lua.unsafeFromValue (Lua.field (Lua.Lib.math, "asin")))
val acos : real -> real = LunarML.assumeDiscardable (Lua.unsafeFromValue (Lua.field (Lua.Lib.math, "acos")))
val atan : real -> real = LunarML.assumeDiscardable (Lua.unsafeFromValue Lua.Lib.math.atan)
val atan2 : real * real -> real = fn (y, x) => Lua.unsafeFromValue (Vector.sub (Lua.call Lua.Lib.math.atan #[Lua.fromReal y, Lua.fromReal x], 0))
val exp : real -> real = LunarML.assumeDiscardable (Lua.unsafeFromValue (Lua.field (Lua.Lib.math, "exp")))
val e = LunarML.assumeDiscardable (exp 1.0)
val pow : real * real -> real = fn (x, y) => Lua.unsafeFromValue (Lua.pow (Lua.fromReal x, Lua.fromReal y))
val ln : real -> real = LunarML.assumeDiscardable (Lua.unsafeFromValue Lua.Lib.math.log)
val log10 : real -> real = fn x => Lua.unsafeFromValue (Vector.sub (Lua.call Lua.Lib.math.log #[Lua.fromReal x, Lua.fromInt 10], 0))
val sinh : real -> real = LunarML.assumeDiscardable (let val raw = Lua.field (Lua.Lib.math, "sinh")
                                                     in if Lua.isNil raw then
                                                            fn x => (exp x - exp (~ x)) / 2.0
                                                        else
                                                            Lua.unsafeFromValue raw : real -> real
                                                     end
                                                    )
val cosh : real -> real = LunarML.assumeDiscardable (let val raw = Lua.field (Lua.Lib.math, "cosh")
                                                     in if Lua.isNil raw then
                                                            fn x => (exp x + exp (~ x)) / 2.0
                                                        else
                                                            Lua.unsafeFromValue raw : real -> real
                                                     end
                                                    )
val tanh : real -> real = LunarML.assumeDiscardable (let val raw = Lua.field (Lua.Lib.math, "tanh")
                                                     in if Lua.isNil raw then
                                                            fn x => let val ex = exp x
                                                                        val e_x = exp (~ x)
                                                                    in (ex - e_x) / (ex + e_x)
                                                                    end
                                                        else
                                                            Lua.unsafeFromValue raw : real -> real
                                                     end
                                                    )
end; (* structure Math *)

structure String : sig
              type string = string
              type char = char
              val size : string -> int
              val sub : string * int -> char
              val extract : string * int * int option -> string
              val substring : string * int * int -> string
              val ^ : string * string -> string
              val concat : string list -> string
              val concatWith : string -> string list -> string
              val str : char -> string
              val implode : char list -> string
              val explode : string -> char list
              val map : (char -> char) -> string -> string
              val translate : (char -> string) -> string -> string
              val tokens : (char -> bool) -> string -> string list
              val fields : (char -> bool) -> string -> string list
              val isPrefix : string -> string -> bool
              val compare : string * string -> order
              val < : string * string -> bool
              val <= : string * string -> bool
              val > : string * string -> bool
              val >= : string * string -> bool
          end = struct
type string = string
type char = char
val size = String.size
val str = String.str
val op ^ = String.^
fun sub (s : string, i : int) : char = if i < 0 orelse size s <= i then
                                           raise Subscript
                                       else
                                           let val i' = i + 1
                                               val result = Lua.call Lua.Lib.string.sub #[Lua.fromString s, Lua.fromInt i', Lua.fromInt i']
                                           in Lua.unsafeFromValue (Vector.sub (result, 0))
                                           end
fun substring (s : string, i : int, j : int) : string = if i < 0 orelse j < 0 orelse size s < i + j then
                                                          raise Subscript
                                                      else
                                                          let val result = Lua.call Lua.Lib.string.sub #[Lua.fromString s, Lua.fromInt (i + 1), Lua.fromInt (i + j)]
                                                          in Lua.unsafeFromValue (Vector.sub (result, 0))
                                                          end
fun extract (s : string, i : int, NONE : int option) : string = if i < 0 orelse size s < i then
                                                                    raise Subscript
                                                                else
                                                                    let val result = Lua.call Lua.Lib.string.sub #[Lua.fromString s, Lua.fromInt (i + 1)]
                                                                    in Lua.unsafeFromValue (Vector.sub (result, 0))
                                                                    end
  | extract (s, i, SOME j) = substring (s, i, j)
fun concat (l : string list) : string = let val result = Lua.call Lua.Lib.table.concat #[Lua.unsafeToValue (Vector.fromList l)]
                                        in Lua.unsafeFromValue (Vector.sub (result, 0))
                                        end
fun concatWith (s : string) (l : string list) : string = let val result = Lua.call Lua.Lib.table.concat #[Lua.unsafeToValue (Vector.fromList l), Lua.fromString s]
                                                         in Lua.unsafeFromValue (Vector.sub (result, 0))
                                                         end
fun implode (l : char list) : string = let val result = Lua.call Lua.Lib.table.concat #[Lua.unsafeToValue (Vector.fromList l)]
                                       in Lua.unsafeFromValue (Vector.sub (result, 0))
                                       end
fun explode (s : string) : char list = Vector.foldr (op ::) [] (Vector.tabulate (size s, fn i => sub (s, i)))
fun map (f : char -> char) (s : string) : string = let val result = Lua.call Lua.Lib.string.gsub #[Lua.fromString s, Lua.fromString ".", Lua.unsafeToValue f]
                                                   in Lua.unsafeFromValue (Vector.sub (result, 0))
                                                   end
fun translate (f : char -> string) (s : string) : string = let val result = Lua.call Lua.Lib.string.gsub #[Lua.fromString s, Lua.fromString ".", Lua.unsafeToValue f]
                                                           in Lua.unsafeFromValue (Vector.sub (result, 0))
                                                           end
fun tokens f s = let fun go (revTokens, acc, []) = List.rev (if List.null acc then revTokens else implode (List.rev acc) :: revTokens)
                       | go (revTokens, acc, x :: xs) = if f x then
                                                            go (if List.null acc then revTokens else implode (List.rev acc) :: revTokens, [], xs)
                                                        else
                                                            go (revTokens, x :: acc, xs)
                 in go ([], [], explode s)
                 end
fun fields f s = let fun go (revFields, acc, []) = List.rev (implode (List.rev acc) :: revFields)
                       | go (revFields, acc, x :: xs) = if f x then
                                                            go (implode (List.rev acc) :: revFields, [], xs)
                                                        else
                                                            go (revFields, x :: acc, xs)
                 in go ([], [], explode s)
                 end
fun isPrefix prefix s = let val n = size prefix
                        in if n > size s then
                               false
                           else
                               substring (s, 0, n) = prefix
                        end
(* isSubstring, isSuffix, collate, toString, scan, fromString, toCString, fromCString *)
fun compare (s, t) = if s = t then
                         EQUAL
                     else if String.< (s, t) then
                         LESS
                     else
                         GREATER
open String (* size, ^, str, <, <=, >, >= *)
end (* structure String *)
val op ^ : string * string -> string = String.^
val concat : string list -> string = String.concat
val size : string -> int = String.size
val str : char -> string = String.str;

structure StringCvt :> sig
              datatype radix = BIN | OCT | DEC | HEX
              datatype realfmt = SCI of int option
                               | FIX of int option
                               | GEN of int option
                               | EXACT
              type ('a, 'b) reader = 'b -> ('a * 'b) option
              type cs
              val scanString : ((char, cs) reader -> ('a, cs) reader) -> string -> 'a option
          end where type radix = StringCvt.radix
              where type realfmt = StringCvt.realfmt = struct
open StringCvt
type cs = string * int (* the underlying string, the starting index *)
fun scanString scan s = case scan (fn (s, i) => if i < String.size s then
                                                    SOME (String.sub (s, i), (s, i + 1))
                                                else
                                                    NONE
                                  ) (s, 0) of
                            SOME (x, _) => SOME x
                          | NONE => NONE
end

signature CHAR = sig
    eqtype char
    eqtype string
    val minChar : char
    val maxChar : char
    val maxOrd : int
    val ord : char -> int
    val chr : int -> char
    val succ : char -> char
    val pred : char -> char
    val compare : char * char -> order
    val < : char * char -> bool
    val <= : char * char -> bool
    val > : char * char -> bool
    val >= : char * char -> bool
    val contains : string -> char -> bool
    val notContains : string -> char -> bool
    val isAscii : char -> bool
    val toLower : char -> char
    val toUpper : char -> char
    val isAlpha : char -> bool
    val isAlphaNum : char -> bool
    val isCntrl : char -> bool
    val isDigit : char -> bool
    val isGraph : char -> bool
    val isHexDigit : char -> bool
    val isLower : char -> bool
    val isPrint : char -> bool
    val isSpace : char -> bool
    val isPunct : char -> bool
    val isUpper : char -> bool
    val toString : char -> String.string
    (* val scan : (Char.char, 'a) StringCvt.reader -> (char, 'a) StringCvt.reader; implemented in scan-text.sml *)
    (* val fromString : String.string -> char option; implemented in scan-text.sml *)
    val toCString : char -> String.string
    (* val fromCString : String.string -> char option *)
end;

structure Char :> CHAR where type char = char where type string = String.string = struct
type char = char
type string = string
val minChar = #"\000"
val maxChar = #"\255"
val maxOrd = 255
val ord : char -> int = LunarML.assumeDiscardable (Lua.unsafeFromValue Lua.Lib.string.byte)
val chr : int -> char = fn x => if x < 0 orelse x > 255 then
                                    raise Chr
                                else
                                    Lua.unsafeFromValue (Vector.sub (Lua.call Lua.Lib.string.char #[Lua.fromInt x], 0))
fun succ c = chr (ord c + 1)
fun pred c = chr (ord c - 1)
fun compare (x : char, y : char) = if x = y then
                                       EQUAL
                                   else if x < y then
                                       LESS
                                   else
                                       GREATER
fun notContains (s : string) (c : char) : bool = let val result = Lua.call Lua.Lib.string.find #[Lua.fromString s, Lua.fromChar c, Lua.fromInt 1, Lua.fromBool true]
                                                 in Lua.isNil (Vector.sub (result, 0))
                                                 end
fun contains s c = not (notContains s c)
local
    fun charClass pattern (c : char) : bool = not (Lua.isNil (Vector.sub (Lua.call Lua.Lib.string.match #[Lua.fromChar c, Lua.fromString pattern], 0)))
in
val isAscii = LunarML.assumeDiscardable (charClass "^[\000-\127]$")
val isAlpha = LunarML.assumeDiscardable (charClass "^[A-Za-z]$")
val isAlphaNum = LunarML.assumeDiscardable (charClass "^[A-Za-z0-9]$")
val isCntrl = LunarML.assumeDiscardable (charClass "^%c$") (* TODO: locale *)
val isDigit = LunarML.assumeDiscardable (charClass "^[0-9]$")
val isGraph = LunarML.assumeDiscardable (charClass "^%g$") (* TODO: locale *)
val isHexDigit = LunarML.assumeDiscardable (charClass "^[A-Fa-f0-9]$")
val isLower = LunarML.assumeDiscardable (charClass "^[a-z]$")
val isPrint = LunarML.assumeDiscardable (charClass "^[^%c]$") (* TODO: locale *)
val isSpace = LunarML.assumeDiscardable (charClass "^[ \n\t\r\v\f]$")
val isPunct = LunarML.assumeDiscardable (charClass "^%p$") (* TODO: locale *)
val isUpper = LunarML.assumeDiscardable (charClass "^[A-Z]$")
end
(* string.lower and string.upper depends on the locale *)
val toLower = fn (c : char) => let val x = ord c
                               in if ord #"A" <= x andalso x <= ord #"Z" then
                                      chr (x - ord #"A" + ord #"a")
                                  else
                                      c
                               end
val toUpper = fn (c : char) => let val x = ord c
                               in if ord #"a" <= x andalso x <= ord #"z" then
                                      chr (x - ord #"a" + ord #"A")
                                  else
                                      c
                               end
fun toString #"\\" = "\\\\"
  | toString #"\"" = "\\\""
  | toString c = if isPrint c then
                     String.str c
                 else
                     case c of
                         #"\a" => "\\a"
                       | #"\b" => "\\b"
                       | #"\t" => "\\t"
                       | #"\n" => "\\n"
                       | #"\v" => "\\v"
                       | #"\f" => "\\f"
                       | #"\r" => "\\r"
                       | _ => let val x = ord c
                              in if x < 32 then
                                     "\\^" ^ String.str (chr (x + 64))
                                 else if x < 100 then
                                     "\\0" ^ Int.toString x
                                 else
                                     "\\" ^ Int.toString x
                                 (* TODO: x >= 1000 *)
                              end
fun toCString #"\\" = "\\\\"
  | toCString #"\"" = "\\\""
  | toCString #"?" = "\\?"
  | toCString #"'" = "\\'"
  | toCString c = if isPrint c then
                      String.str c
                  else
                      case c of
                          #"\a" => "\\a"
                        | #"\b" => "\\b"
                        | #"\t" => "\\t"
                        | #"\n" => "\\n"
                        | #"\v" => "\\v"
                        | #"\f" => "\\f"
                        | #"\r" => "\\r"
                        | _ => let val x = ord c
                                   val s = Int.fmt StringCvt.OCT x
                               in if x < 8 then
                                      "\\00" ^ s
                                  else if x < 64 then
                                      "\\0" ^ s
                                  else
                                      "\\" ^ s
                                  (* TODO: x >= 512 *)
                               end
open Char (* <, <=, >, >= *)
(* scan, fromString, toCString, fromCString *)
end (* structure Char *)
val chr = Char.chr
val ord = Char.ord;

signature STRING = sig
    eqtype string
    eqtype char
    val maxSize : int
    val size : string -> int
    val sub : string * int -> char
    val extract : string * int * int option -> string
    val substring : string * int * int -> string
    val ^ : string * string -> string
    val concat : string list -> string
    val concatWith : string -> string list -> string
    val str : char -> string
    val implode : char list -> string
    val explode : string -> char list
    val map : (char -> char) -> string -> string
    val translate : (char -> string) -> string -> string
    val tokens : (char -> bool) -> string -> string list
    val fields : (char -> bool) -> string -> string list
    val isPrefix : string -> string -> bool
    (* val isSubstring : string -> string -> bool *)
    (* val isSuffix : string -> string -> bool *)
    val compare : string * string -> order
    (* val collate : (char * char -> order) -> string * string -> order *)
    val < : string * string -> bool
    val <= : string * string -> bool
    val > : string * string -> bool
    val >= : string * string -> bool
    val toString : string -> string
    (* val scan : (Char.char, 'a) StringCvt.reader -> (string, 'a) StringCvt.reader; implemented in scan-text.sml *)
    (* val fromString : String.string -> string option; implemented in scan-text.sml *)
    val toCString : string -> String.string
    (* val fromCString : String.string -> string option *)
    (* from https://github.com/SMLFamily/BasisLibrary/wiki/2015-003d-STRING: *)
    (* val rev : string -> string *)
    (* val implodeRev : char list -> string *)
    (* val concatWithMap : string -> ('a -> string) -> 'a list -> string *)
end;

structure String :> STRING where type string = string where type char = Char.char = struct
open String
val maxSize = LunarML.assumeDiscardable (case Int.maxInt of SOME n => n | NONE => 0x7fffffff)
fun toString s = translate Char.toString s
fun toCString s = translate Char.toCString s
end;

structure Vector : sig
              datatype vector = datatype vector
              val maxLen : int
              val fromList : 'a list -> 'a vector
              val tabulate : int * (int -> 'a) -> 'a vector
              val length : 'a vector -> int
              val sub : 'a vector * int -> 'a
              val update : 'a vector * int * 'a -> 'a vector
              val concat : 'a vector list -> 'a vector
              val appi : (int * 'a -> unit) -> 'a vector -> unit
              val app : ('a -> unit) -> 'a vector -> unit
              val mapi : (int * 'a -> 'b) -> 'a vector -> 'b vector
              val map : ('a -> 'b) -> 'a vector -> 'b vector
              val foldli : (int * 'a * 'b -> 'b) -> 'b -> 'a vector -> 'b
              val foldri : (int * 'a * 'b -> 'b) -> 'b -> 'a vector -> 'b
              val foldl : ('a * 'b -> 'b) -> 'b -> 'a vector -> 'b
              val foldr : ('a * 'b -> 'b) -> 'b -> 'a vector -> 'b
              val findi : (int * 'a -> bool) -> 'a vector -> (int * 'a) option
              val find : ('a -> bool) -> 'a vector -> 'a option
              val exists : ('a -> bool) -> 'a vector -> bool
              val all : ('a -> bool) -> 'a vector -> bool
              val collate : ('a * 'a -> order) -> 'a vector * 'a vector -> order
          end = struct
open Vector
val maxLen = LunarML.assumeDiscardable (case Int.maxInt of SOME n => n | NONE => 0x7fffffff)
fun collate compare (xs, ys) = let val xl = length xs
                                   val yl = length ys
                                   fun go i = case (xl = i, yl = i) of
                                                  (true, true) => EQUAL
                                                | (true, false) => LESS
                                                | (false, true) => GREATER
                                                | (false, false) => case compare (Unsafe.Vector.sub (xs, i), Unsafe.Vector.sub (ys, i)) of
                                                                        EQUAL => go (i + 1)
                                                                      | t => t
                               in go 0
                               end
end


structure Array : sig
              datatype array = datatype array
              datatype vector = datatype vector
              val maxLen : int
              val array : int * 'a -> 'a array
              val fromList : 'a list -> 'a array
              val tabulate : int * (int -> 'a) -> 'a array
              val length : 'a array -> int
              val sub : 'a array * int -> 'a
              val update : 'a array * int * 'a -> unit
              val copy : { src : 'a array, dst : 'a array, di : int } -> unit
              val copyVec : { src : 'a vector, dst : 'a array, di : int } -> unit
              val appi : (int * 'a -> unit) -> 'a array -> unit
              val app : ('a -> unit) -> 'a array -> unit
              val modifyi : (int * 'a -> 'a) -> 'a array -> unit
              val modify : ('a -> 'a) -> 'a array -> unit
              val foldli : (int * 'a * 'b -> 'b) -> 'b -> 'a array -> 'b
              val foldri : (int * 'a * 'b -> 'b) -> 'b -> 'a array -> 'b
              val foldl : ('a * 'b -> 'b) -> 'b -> 'a array -> 'b
              val foldr : ('a * 'b -> 'b) -> 'b -> 'a array -> 'b
              val toList : 'a array -> 'a list
              val fromVector : 'a vector -> 'a array
              val toVector : 'a array -> 'a vector
          end = struct
datatype array = datatype array
datatype vector = datatype vector
val maxLen = Vector.maxLen
fun length arr = _primCall "Array.length" (arr)
fun sub (arr, i) = if i < 0 orelse length arr <= i then
                       raise Subscript
                   else
                       Unsafe.Array.sub (arr, i)
fun update (arr, i, value) = if i < 0 orelse length arr <= i then
                                 raise Subscript
                             else
                                 Unsafe.Array.update (arr, i, value)
fun copy { src, dst, di } = let val srcLen = length src
                            in if 0 <= di andalso di + srcLen <= length dst then
                                   let fun loop i = if i >= srcLen then
                                                        ()
                                                    else
                                                        ( Unsafe.Array.update (dst, di + i, Unsafe.Array.sub (src, i))
                                                        ; loop (i + 1)
                                                        )
                                   in loop 0
                                   end
                               else
                                   raise Subscript
                            end
fun copyVec { src, dst, di } = let val srcLen = Vector.length src
                               in if 0 <= di andalso di + Vector.length src <= length dst then
                                      let fun loop i = if i >= srcLen then
                                                           ()
                                                       else
                                                           ( Unsafe.Array.update (dst, di + i, Unsafe.Vector.sub (src, i))
                                                           ; loop (i + 1)
                                                           )
                                      in loop 0
                                      end
                                  else
                                      raise Subscript
                               end
fun appi f arr = let val n = length arr
                     fun loop i = if i >= n then
                                      ()
                                  else
                                      ( f (i, Unsafe.Array.sub (arr, i))
                                      ; loop (i + 1)
                                      )
                 in loop 0
                 end
fun app f arr = let val n = length arr
                    fun loop i = if i >= n then
                                     ()
                                 else
                                     ( f (Unsafe.Array.sub (arr, i))
                                     ; loop (i + 1)
                                     )
                in loop 0
                end
fun modifyi f arr = let val n = length arr
                        fun loop i = if i >= n then
                                         ()
                                     else
                                         let val x = Unsafe.Array.sub (arr, i)
                                             val y = f (i, x)
                                         in Unsafe.Array.update (arr, i, y)
                                          ; loop (i + 1)
                                         end
                    in loop 0
                    end
fun modify f arr = let val n = length arr
                       fun loop i = if i >= n then
                                        ()
                                    else
                                        let val x = Unsafe.Array.sub (arr, i)
                                            val y = f x
                                        in Unsafe.Array.update (arr, i, y)
                                         ; loop (i + 1)
                                        end
                   in loop 0
                   end
fun foldli f init arr = let val n = length arr
                            fun loop (i, acc) = if i >= n then
                                                    acc
                                                else
                                                    loop (i + 1, f (i, Unsafe.Array.sub (arr, i), acc))
                        in loop (0, init)
                        end
fun foldri f init arr = let fun loop (i, acc) = if i < 0 then
                                                    acc
                                                else
                                                    loop (i - 1, f (i, Unsafe.Array.sub (arr, i), acc))
                        in loop (length arr - 1, init)
                        end
fun foldl f init arr = let val n = length arr
                           fun loop (i, acc) = if i >= n then
                                                   acc
                                               else
                                                   loop (i + 1, f (Unsafe.Array.sub (arr, i), acc))
                       in loop (0, init)
                       end
fun foldr f init arr = let fun loop (i, acc) = if i < 0 then
                                                   acc
                                               else
                                                   loop (i - 1, f (Unsafe.Array.sub (arr, i), acc))
                       in loop (length arr - 1, init)
                       end
val array = _Prim.Array.array
val fromList = _Prim.Array.fromList
val tabulate = _Prim.Array.tabulate
fun toList a = List.tabulate (length a, fn i => Unsafe.Array.sub (a, i))
fun fromVector v = tabulate (Vector.length v, fn i => Unsafe.Vector.sub (v, i))
fun toVector a = Vector.tabulate (length a, fn i => Unsafe.Array.sub (a, i))
end; (* structure Array *)
