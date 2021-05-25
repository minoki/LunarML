fun x <> y = not (x = y);

structure Lua = struct
open LunarML.Lua (* type value, sub, set, global, call, method, NIL, isNil, isFalsy, unsafeToValue, unsafeFromValue, newTable, function *)
val fromBool : bool -> value = unsafeToValue
val fromInt : int -> value = unsafeToValue
val fromWord : word -> value = unsafeToValue
val fromReal : real -> value = unsafeToValue
val fromString : string -> value = unsafeToValue
fun field (t : value, name : string) = sub (t, fromString name)
end;

structure Vector = struct
datatype vector = datatype vector
open LunarML.Vector (* fromList, tabulate, length, sub *)
local
    fun foldl' (f, acc, vec, i) = if i >= length vec then
                                      acc
                                  else
                                      foldl' (f, f (sub (vec, i), acc), vec, i + 1)
    fun foldr' (f, acc, vec, i) = if i < 0 then
                                      acc
                                  else
                                      foldr' (f, f (sub (vec, i), acc), vec, i - 1)
in
fun foldl (f : 'a * 'b -> 'b) (init : 'b) (vec : 'a vector) : 'b = foldl' (f, init, vec, 0)
fun foldr (f : 'a * 'b -> 'b) (init : 'b) (vec : 'a vector) : 'b = foldr' (f, init, vec, length vec - 1)
end
end;
val vector : 'a list -> 'a vector = Vector.fromList;

(* General *)
structure General = struct
type unit = unit
type exn = exn
exception Bind = Bind
exception Match = Match
exception Chr
exception Div = Div
exception Domain
exception Fail = Fail
exception Overflow = Overflow
exception Size = Size
exception Span
exception Subscript = Subscript
(*
val exnName : exn -> string
val exnMessage : exn -> string
*)
datatype order = LESS | EQUAL | GREATER
val ! : 'a ref -> 'a = !
val op := : 'a ref * 'a -> unit = op :=
fun x before () = x
fun ignore _ = ()
fun (f o g) x = f (g x)
end;
open General;
(*
val op before : 'a * unit -> 'a = General.before;
val ignore : 'a -> unit = General.ignore;
val op o : ('b -> 'c) * ('a -> 'b) -> 'a -> 'c = General.o;
*)

datatype 'a option = NONE | SOME of 'a;

structure Bool = struct
datatype bool = datatype bool
val not = not
fun toString true = "true"
  | toString false = "false"
(* scan, fromString *)
end;

structure Int = struct
type int = int
open LunarML.Int (* +, -, *, div, mod, ~, abs, <, <=, >, >= *)
local
    val stringlib = LunarML.assumeDiscardable (Lua.global "string")
    val string_match = LunarML.assumeDiscardable (Lua.field (stringlib, "match"))
    val string_gsub = LunarML.assumeDiscardable (Lua.field (stringlib, "gsub"))
    val tostring = LunarML.assumeDiscardable (Lua.global "tostring")
    val tonumber = LunarML.assumeDiscardable (Lua.global "tonumber")
    val mathlib = LunarML.assumeDiscardable (Lua.global "math")
    val math_maxinteger = LunarML.assumeDiscardable (Lua.field (mathlib, "maxinteger"))
    val math_mininteger = LunarML.assumeDiscardable (Lua.field (mathlib, "mininteger"))
in
(* toLarge, fromLarge *)
val toInt : int -> int = fn x => x
val fromInt : int -> int = fn x => x
val precision : int option = LunarML.assumeDiscardable
                                 (let fun computeWordSize (x : int, n) = if x = 0 then
                                                                             n
                                                                         else
                                                                             computeWordSize (Lua.unsafeFromValue (Lua.>> (Lua.fromInt x, Lua.fromInt 1)), n + 1)
                                  in SOME (computeWordSize (Lua.unsafeFromValue math_maxinteger, 1))
                                  end
                                 )
val minInt : int option = LunarML.assumeDiscardable (SOME (Lua.unsafeFromValue math_mininteger))
val maxInt : int option = LunarML.assumeDiscardable (SOME (Lua.unsafeFromValue math_maxinteger))
(*
val quot : int * int -> int
val rem : int * int -> int
*)
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
(* fmt *)
fun toString (x : int) : string = let val result = Lua.call tostring (vector [Lua.fromInt x])
                                      val result = Lua.call string_gsub (vector [Vector.sub (result, 0), Lua.fromString "-", Lua.fromString "~"])
                                  in Lua.unsafeFromValue (Vector.sub (result, 0))
                                  end
(* scan *)
fun fromString (s : string) : int option = let val result = Lua.call string_match (vector [Lua.fromString s, Lua.fromString "^%s*([%+~%-]?)([0-9]+)"])
                                           in if Lua.isNil (Vector.sub (result, 0)) then
                                                  NONE
                                              else
                                                  let val sign = Lua.unsafeFromValue (Vector.sub (result, 0)) : string
                                                      val digits = Lua.unsafeFromValue (Vector.sub (result, 1)) : string
                                                      val result' = if sign = "~" orelse sign = "-" then
                                                                        Lua.call tonumber (vector [Lua.fromString (LunarML.String.^ ("-", digits))])
                                                                    else
                                                                        Lua.call tonumber (vector [Lua.fromString digits])
                                                  in SOME (Lua.unsafeFromValue (Vector.sub (result', 0)))
                                                  end
                                           end
end
end;

structure Word = struct
local
    val mathlib = LunarML.assumeDiscardable (Lua.global "math")
    val math_maxinteger = LunarML.assumeDiscardable (Lua.field (mathlib, "maxinteger"))
    val stringlib = LunarML.assumeDiscardable (Lua.global "string")
    val string_format = LunarML.assumeDiscardable (Lua.field (mathlib, "format"))
in
type word = word
open LunarML.Word (* +, -, *, div, mod, ~, <, <=, >, >= *)
val wordSize : int = LunarML.assumeDiscardable
                         (let fun computeWordSize (x : int, n : int) = if x = 0 then
                                                                           n
                                                                       else
                                                                           computeWordSize (Lua.unsafeFromValue (Lua.>> (Lua.fromInt x, Lua.fromInt 1)), Int.+ (n, 1))
                          in computeWordSize (Lua.unsafeFromValue math_maxinteger, 1)
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
(* fmt *)
val toString : word -> string = fn x => Lua.unsafeFromValue (Vector.sub (Lua.call string_format (vector [Lua.fromString "%X", Lua.fromWord x]), 0))
(* scan, fromString *)
end
end;

structure Real = struct
type real = real
open LunarML.Real (* +, -, *, /, ~, abs, <, <=, >, >= *)
end;

structure Math = struct
type real = real
local
    val mathlib = LunarML.assumeDiscardable (Lua.global "math")
    val math_atan = LunarML.assumeDiscardable (Lua.field (mathlib, "atan"))
    val math_log = LunarML.assumeDiscardable (Lua.field (mathlib, "log"))
in
val pi : real = LunarML.assumeDiscardable (Lua.unsafeFromValue (Lua.field (mathlib, "pi")))
(* val e : real *)
val sqrt : real -> real = LunarML.assumeDiscardable (Lua.unsafeFromValue (Lua.field (mathlib, "sqrt")))
val sin : real -> real = LunarML.assumeDiscardable (Lua.unsafeFromValue (Lua.field (mathlib, "sin")))
val cos : real -> real = LunarML.assumeDiscardable (Lua.unsafeFromValue (Lua.field (mathlib, "cos")))
val tan : real -> real = LunarML.assumeDiscardable (Lua.unsafeFromValue (Lua.field (mathlib, "tan")))
val asin : real -> real = LunarML.assumeDiscardable (Lua.unsafeFromValue (Lua.field (mathlib, "asin")))
val acos : real -> real = LunarML.assumeDiscardable (Lua.unsafeFromValue (Lua.field (mathlib, "acos")))
val atan : real -> real = LunarML.assumeDiscardable (Lua.unsafeFromValue math_atan)
val atan2 : real * real -> real = fn (y, x) => Lua.unsafeFromValue (Vector.sub (Lua.call math_atan (vector [Lua.fromReal y, Lua.fromReal x]), 0))
val exp : real -> real = LunarML.assumeDiscardable (Lua.unsafeFromValue (Lua.field (mathlib, "exp")))
val pow : real * real -> real = fn (x, y) => Lua.unsafeFromValue (Lua.pow (Lua.fromReal x, Lua.fromReal y))
val ln : real -> real = LunarML.assumeDiscardable (Lua.unsafeFromValue math_log)
val log10 : real -> real = fn x => Lua.unsafeFromValue (Vector.sub (Lua.call math_log (vector [Lua.fromReal x, Lua.fromInt 10]), 0))
(*
val sinh : real -> real
val cosh : real -> real
val tanh : real -> real
*)
end
end;

structure Char = struct
type char = char
type string = string
open LunarML.Char (* <, <=, >, >= *)
(* minChar, maxChar, maxOrd, ord, chr, succ, pred, compare, contains, notContains, isAscii, toLower, toUpper, isAlpha, isAlphaNum, isCntrl, isDigit, isGraph, isHexDigit, isLower, isPrint, isSpace, isPunct, isUpper, toString, scan, fromString, toCString, fromCString *)
end;

structure String = struct
type string = string
type char = char
val size = LunarML.String.size
val str = LunarML.String.str
val op ^ = LunarML.String.^
local
    val stringlib = LunarML.assumeDiscardable (Lua.global "string")
    val string_sub = LunarML.assumeDiscardable (Lua.field (stringlib, "sub"))
    val string_gsub = LunarML.assumeDiscardable (Lua.field (stringlib, "gsub"))
    val tablelib = LunarML.assumeDiscardable (Lua.global "table")
    val table_concat = LunarML.assumeDiscardable (Lua.field (tablelib, "concat"))
in
fun sub (s : string, i : int) : char = if i < 0 orelse size s <= i then
                                           raise Subscript
                                       else
                                           let val i' = i + 1
                                               val result = Lua.call string_sub (vector [Lua.fromString s, Lua.fromInt i', Lua.fromInt i'])
                                           in Lua.unsafeFromValue (Vector.sub (result, 0))
                                           end
fun substring (s : string, i : int, j : int) : string = if i < 0 orelse j < 0 orelse size s < i + j then
                                                          raise Subscript
                                                      else
                                                          let val result = Lua.call string_sub (vector [Lua.fromString s, Lua.fromInt (i + 1), Lua.fromInt (i + j)])
                                                          in Lua.unsafeFromValue (Vector.sub (result, 0))
                                                          end
fun extract (s : string, i : int, NONE : int option) : string = if i < 0 orelse size s < i then
                                                                    raise Subscript
                                                                else
                                                                    let val result = Lua.call string_sub (vector [Lua.fromString s, Lua.fromInt (i + 1)])
                                                                    in Lua.unsafeFromValue (Vector.sub (result, 0))
                                                                    end
  | extract (s, i, SOME j) = substring (s, i, j)
fun concat (l : string list) : string = let val result = Lua.call table_concat (vector [Lua.unsafeToValue (Vector.fromList l)])
                                        in Lua.unsafeFromValue (Vector.sub (result, 0))
                                        end
fun concatWith (s : string) (l : string list) : string = let val result = Lua.call table_concat (vector [Lua.unsafeToValue (Vector.fromList l), Lua.fromString s])
                                                         in Lua.unsafeFromValue (Vector.sub (result, 0))
                                                         end
fun implode (l : char list) : string = let val result = Lua.call table_concat (vector [Lua.unsafeToValue (Vector.fromList l)])
                                       in Lua.unsafeFromValue (Vector.sub (result, 0))
                                       end
fun explode (s : string) : char list = Vector.foldr (op ::) [] (Vector.tabulate (size s, fn i => sub (s, i)))
fun map (f : char -> char) (s : string) : string = let val result = Lua.call string_gsub (vector [Lua.fromString s, Lua.fromString ".", Lua.unsafeToValue f])
                                                   in Lua.unsafeFromValue (Vector.sub (result, 0))
                                                   end
fun translate (f : char -> string) (s : string) : string = let val result = Lua.call string_gsub (vector [Lua.fromString s, Lua.fromString ".", Lua.unsafeToValue f])
                                                           in Lua.unsafeFromValue (Vector.sub (result, 0))
                                                           end
(* tokens, fields, isPrefix, isSubstring, isSuffix, compare, collate, toString, scan, fromString, toCString, fromCString *)
open LunarML.String (* size, ^, str, <, <=, >, >= *)
end
end;
val op ^ : string * string -> string = String.^;
val size : string -> int = String.size;
val str : char -> string = String.str;

(* List *)
structure List = struct
datatype list = datatype list
exception Empty
fun [] @ ys = ys
  | (x :: xs) @ ys = x :: (xs @ ys)
fun app f [] = ()
  | app f (x :: xs) = (f x; app f xs)
fun foldl f init [] = init
  | foldl f init (x :: xs) = foldl f (f (x, init)) xs
fun foldr f init [] = init
  | foldr f init (x :: xs) = f (x, foldr f init xs)
fun hd [] = raise Empty
  | hd (x :: _) = x
local
    fun doLength (acc, []) = acc : int
      | doLength (acc, x :: xs) = doLength (acc + 1, xs)
in 
fun length xs = doLength (0, xs)
end
fun map f [] = []
  | map f (x :: xs) = f x :: map f xs
fun null [] = true
  | null _ = false
fun rev [] = []
  | rev (x :: xs) = rev xs @ [x]
fun tl [] = raise Empty
  | tl (_ :: xs) = xs
end;
val op @ : ('a list * 'a list) -> 'a list = List.@;
val app : ('a -> unit) -> 'a list -> unit = List.app;
val foldl : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b = List.foldl;
val foldr : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b = List.foldr;
val hd : 'a list -> 'a = List.hd;
val length : 'a list -> int = List.length;
val map : ('a -> 'b) -> 'a list -> 'b list = List.map;
val null : 'a list -> bool = List.null;
val rev : 'a list -> 'a list = List.rev;
val tl : 'a list -> 'a list = List.tl;

(* Option *)
structure Option = struct
datatype option = datatype option
exception Option
fun getOpt (NONE, default) = default
  | getOpt (SOME x, _) = x
fun isSome (SOME _) = true
  | isSome NONE = false
fun valOf (SOME x) = x
  | valOf NONE = raise Option
fun filter pred x = if pred x then
                        SOME x
                    else
                        NONE
fun join (SOME x) = x
  | join NONE = NONE
fun app f (SOME x) = f x
  | app f NONE = ()
fun map f (SOME x) = SOME (f x)
  | map f NONE = NONE
fun mapPartial f (SOME x) = f x
  | mapPartial f NONE = NONE
fun compose (f, g) x = case g x of
                           SOME y => SOME (f y)
                         | NONE => NONE
fun composePartial (f, g) x = case g x of
                                  SOME y => f y
                                | NONE => NONE
end;
val getOpt : 'a option * 'a -> 'a = Option.getOpt;
val isSome : 'a option -> bool = Option.isSome;
val valOf : 'a option -> 'a = Option.valOf;

structure Array = struct
datatype array = datatype array
datatype vector = datatype vector
open LunarML.Array (* array, fromList, tabulate, length, sub, update *)
end;

structure IO = struct
exception Io of { name : string
                , function : string
                , cause : exn
                }
end;

structure TextIO = struct
local
    datatype instream = Instream of Lua.value
    datatype outstream = Outstream of Lua.value
    val io = LunarML.assumeDiscardable (Lua.global "io")
    val io_open = LunarML.assumeDiscardable (Lua.field (io, "open"))
    val io_write = LunarML.assumeDiscardable (Lua.field (io, "write"))
in
(* IMPERATIVE_IO *)
type vector = string
type elem = char
type instream = instream
type outstream = outstream
fun input1 (Instream f) = let val result = Vector.sub (Lua.method (f, "read") (vector [Lua.fromInt 1]), 0)
                          in if Lua.isNil result then
                                 NONE
                             else
                                 SOME (Lua.unsafeFromValue result : elem)
                          end
fun inputN (Instream f, n : int) = if n < 0 then
                                       raise Size
                                   else
                                       let val result = Vector.sub (Lua.method (f, "read") (vector [Lua.fromInt n]), 0)
                                       in if Lua.isNil result then
                                              ""
                                          else
                                              (Lua.unsafeFromValue result : vector)
                                       end
fun inputAll (Instream f) = let val result = Vector.sub (Lua.method (f, "read") (vector [Lua.fromString "a"]), 0)
                            in Lua.unsafeFromValue result : vector
                            end
fun closeIn (Instream f) = (Lua.method (f, "close") (vector []); ())
fun output (Outstream f, s) = (Lua.method (f, "write") (vector [Lua.fromString s]); ())
fun output1 (Outstream f, c) = (Lua.method (f, "write") (vector [Lua.fromString (String.str c)]); ())
fun flushOut (Outstream f) = (Lua.method (f, "flush") (vector []); ())
fun closeOut (Outstream f) = (Lua.method (f, "close") (vector []); ())

(* TEXT_IO *)
fun inputLine (Instream f) = let val result = Vector.sub (Lua.method (f, "read") (vector [Lua.fromString "L"]), 0)
                             in if Lua.isNil result then
                                    NONE
                                else
                                    SOME (Lua.unsafeFromValue result : string)
                             end
(* outputsubstr : outstream * substring -> unit *)
fun openIn f = let val result = Lua.call io_open (vector [Lua.fromString f, Lua.fromString "r"])
               in if Lua.isNil (Vector.sub (result, 0)) then
                      raise IO.Io { name = f, function = "TextIO.openIn", cause = Fail (Lua.unsafeFromValue (Vector.sub (result, 1))) } (* TODO: cause *)
                  else
                      Instream (Vector.sub (result, 0))
               end
fun openOut f = let val result = Lua.call io_open (vector [Lua.fromString f, Lua.fromString "w"])
               in if Lua.isNil (Vector.sub (result, 0)) then
                      raise IO.Io { name = f, function = "TextIO.openOut", cause = Fail (Lua.unsafeFromValue (Vector.sub (result, 1))) } (* TODO: cause *)
                  else
                      Outstream (Vector.sub (result, 0))
               end
fun openAppend f = let val result = Lua.call io_open (vector [Lua.fromString f, Lua.fromString "a"])
                   in if Lua.isNil (Vector.sub (result, 0)) then
                          raise IO.Io { name = f, function = "TextIO.openAppend", cause = Fail (Lua.unsafeFromValue (Vector.sub (result, 1))) } (* TODO: cause *)
                      else
                          Outstream (Vector.sub (result, 0))
                   end
(* fun openString f *)
val stdIn = LunarML.assumeDiscardable (Instream (Lua.field (io, "stdin")))
val stdOut = LunarML.assumeDiscardable (Outstream (Lua.field (io, "stdout")))
val stdErr = LunarML.assumeDiscardable (Outstream (Lua.field (io, "stderr")))
fun print s = (Lua.call io_write (vector [Lua.fromString s]); ())
(* scanStream *)
end
end;
val print : string -> unit = TextIO.print;
