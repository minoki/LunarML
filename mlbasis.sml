fun x <> y = Bool.not (x = y);

structure Lua = struct
open Lua (* type value, sub, set, global, call, method, NIL, isNil, isFalsy, unsafeToValue, unsafeFromValue, newTable, function *)
val fromBool : bool -> value = unsafeToValue
val fromInt : int -> value = unsafeToValue
val fromWord : word -> value = unsafeToValue
val fromReal : real -> value = unsafeToValue
val fromString : string -> value = unsafeToValue
val fromChar : char -> value = unsafeToValue
fun field (t : value, name : string) = sub (t, fromString name)
end;

local
    val tonumber = LunarML.assumeDiscardable (Lua.global "tonumber")
    val tostring = LunarML.assumeDiscardable (Lua.global "tostring")
    val mathlib = LunarML.assumeDiscardable (Lua.global "math")
    val math_atan = LunarML.assumeDiscardable (Lua.field (mathlib, "atan"))
    val math_log = LunarML.assumeDiscardable (Lua.field (mathlib, "log"))
    val math_maxinteger = LunarML.assumeDiscardable (Lua.field (mathlib, "maxinteger"))
    val math_mininteger = LunarML.assumeDiscardable (Lua.field (mathlib, "mininteger"))
    val stringlib = LunarML.assumeDiscardable (Lua.global "string")
    val string_byte = LunarML.assumeDiscardable (Lua.field (stringlib, "byte"))
    val string_char = LunarML.assumeDiscardable (Lua.field (stringlib, "char"))
    val string_find = LunarML.assumeDiscardable (Lua.field (stringlib, "find"))
    val string_format = LunarML.assumeDiscardable (Lua.field (stringlib, "format"))
    val string_gsub = LunarML.assumeDiscardable (Lua.field (stringlib, "gsub"))
    val string_match = LunarML.assumeDiscardable (Lua.field (stringlib, "match"))
    val string_sub = LunarML.assumeDiscardable (Lua.field (stringlib, "sub"))
    val tablelib = LunarML.assumeDiscardable (Lua.global "table")
    val table_concat = LunarML.assumeDiscardable (Lua.field (tablelib, "concat"))
in

structure Vector = struct
datatype vector = datatype vector
open Vector (* fromList, tabulate, length, sub *)
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
end
val vector : 'a list -> 'a vector = Vector.fromList

(* General *)
structure General = struct
open General (* !, := *)
type unit = {}
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
fun x before () = x
fun ignore _ = ()
fun (f o g) x = f (g x)
end (* structure General *)
open General
(*
val op before : 'a * unit -> 'a = General.before;
val ignore : 'a -> unit = General.ignore;
val op o : ('b -> 'c) * ('a -> 'b) -> 'a -> 'c = General.o;
*)

datatype 'a option = NONE | SOME of 'a

structure Bool = struct
datatype bool = datatype bool
open Bool
fun toString true = "true"
  | toString false = "false"
(* scan, fromString *)
end (* structure Bool *)
val not : bool -> bool = Bool.not

structure Int = struct
type int = int
open Int (* +, -, *, div, mod, ~, abs, <, <=, >, >= *)
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
                                                                        Lua.call tonumber (vector [Lua.fromString (String.^ ("-", digits))])
                                                                    else
                                                                        Lua.call tonumber (vector [Lua.fromString digits])
                                                  in SOME (Lua.unsafeFromValue (Vector.sub (result', 0)))
                                                  end
                                           end
end (* structure Int *)

structure Word = struct
type word = word
open Word (* +, -, *, div, mod, ~, <, <=, >, >= *)
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
end (* structure Word *)

structure Real = struct
type real = real
open Real (* +, -, *, /, ~, abs, <, <=, >, >= *)
end (* structure Real *)

structure Math = struct
type real = real
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
end (* structure Math *)

structure Char = struct
type char = char
type string = string
val minChar = #"\000"
val maxChar = #"\255"
val maxOrd = 255
val ord : char -> int = Lua.unsafeFromValue string_byte
val chr : int -> char = fn x => if x < 0 orelse x > 255 then
                                    raise Chr
                                else
                                    Lua.unsafeFromValue (Vector.sub (Lua.call string_char (vector [Lua.fromInt x]), 0))
fun succ c = chr (ord c + 1)
fun pred c = chr (ord c - 1)
fun compare (x : char, y : char) = if x = y then
                                       EQUAL
                                   else if x < y then
                                       LESS
                                   else
                                       GREATER
fun notContains (s : string) (c : char) : bool = let val result = Lua.call string_find (vector [Lua.fromString s, Lua.fromChar c, Lua.fromInt 1, Lua.fromBool true])
                                                 in Lua.isNil (Vector.sub (result, 0))
                                                 end
fun contains s c = not (notContains s c)
local
    fun charClass pattern (c : char) : bool = not (Lua.isNil (Vector.sub (Lua.call string_match (vector [Lua.fromChar c, Lua.fromString pattern]), 0)))
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
open Char (* <, <=, >, >= *)
(* minChar, maxChar, maxOrd, ord, chr, succ, pred, compare, contains, notContains, isAscii, toLower, toUpper, isAlpha, isAlphaNum, isCntrl, isDigit, isGraph, isHexDigit, isLower, isPrint, isSpace, isPunct, isUpper, toString, scan, fromString, toCString, fromCString *)
end (* structure Char *)

structure String = struct
type string = string
type char = char
val size = String.size
val str = String.str
val op ^ = String.^
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
open String (* size, ^, str, <, <=, >, >= *)
end (* structure String *)
val op ^ : string * string -> string = String.^
val size : string -> int = String.size
val str : char -> string = String.str

structure List = struct
datatype list = datatype list
exception Empty
fun null [] = true
  | null _ = false
local
    fun doLength (acc, []) = acc : int
      | doLength (acc, x :: xs) = doLength (acc + 1, xs)
in 
fun length xs = doLength (0, xs)
end
fun [] @ ys = ys
  | (x :: xs) @ ys = x :: (xs @ ys)
fun hd [] = raise Empty
  | hd (x :: _) = x
fun tl [] = raise Empty
  | tl (_ :: xs) = xs
fun last [x] = x
  | last (_ :: xs) = last xs
  | last [] = raise Empty
fun getItem [] = NONE
  | getItem (x :: xs) = SOME (x, xs)
fun nth (x :: _, 0) = x
  | nth (_ :: xs, n) = nth (xs, n - 1)
  | nth ([], _) = raise Subscript
fun take (_, 0) = []
  | take (x :: xs, n) = x :: take (xs, n - 1)
  | take ([], _) = raise Subscript
fun drop (xs, 0) = xs
  | drop (_ :: xs, n) = drop (xs, n - 1)
  | drop ([], _) = raise Subscript
fun rev [] = []
  | rev (x :: xs) = rev xs @ [x]
fun revAppend ([], ys) = ys
  | revAppend (x :: xs, ys) = revAppend (xs, x :: ys)
fun app f [] = ()
  | app f (x :: xs) = (f x; app f xs)
fun map f [] = []
  | map f (x :: xs) = f x :: map f xs
fun mapPartial f [] = []
  | mapPartial f (x :: xs) = case f x of
                                 NONE => mapPartial f xs
                               | SOME y => y :: mapPartial f xs
fun find f [] = NONE
  | find f (x :: xs) = if f x then
                           SOME x
                       else
                           find f xs
fun filter f [] = []
  | filter f (x :: xs) = if f x then
                             x :: filter f xs
                         else
                             filter f xs
fun partition f [] = ([], [])
  | partition f (x :: xs) = if f x then
                                let val (l, r) = partition f xs
                                in (x :: l, r)
                                end
                            else
                                let val (l, r) = partition f xs
                                in (l, x :: r)
                                end
fun foldl f init [] = init
  | foldl f init (x :: xs) = foldl f (f (x, init)) xs
fun foldr f init [] = init
  | foldr f init (x :: xs) = f (x, foldr f init xs)
fun concat xs = foldr (op @) [] xs
fun exists f [] = false
  | exists f (x :: xs) = f x orelse exists f xs
fun all f [] = true
  | all f (x :: xs) = f x andalso all f xs
fun tabulate (n, f) = let fun go i = if i >= n then
                                         []
                                     else
                                         f i :: go (i + 1)
                      in go 0
                      end
fun collate compare ([], []) = EQUAL
  | collate compare (_ :: _, []) = GREATER
  | collate compare ([], _ :: _) = LESS
  | collate compare (x :: xs, y :: ys) = case compare x y of
                                             EQUAL => collate compare (xs, ys)
                                           | c => c
end (* structure List *)
val op @ : ('a list * 'a list) -> 'a list = List.@
val app : ('a -> unit) -> 'a list -> unit = List.app
val foldl : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b = List.foldl
val foldr : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b = List.foldr
val hd : 'a list -> 'a = List.hd
val length : 'a list -> int = List.length
val map : ('a -> 'b) -> 'a list -> 'b list = List.map
val null : 'a list -> bool = List.null
val rev : 'a list -> 'a list = List.rev
val tl : 'a list -> 'a list = List.tl

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
end (* structure Option *)
val getOpt : 'a option * 'a -> 'a = Option.getOpt
val isSome : 'a option -> bool = Option.isSome
val valOf : 'a option -> 'a = Option.valOf

structure Array = struct
datatype array = datatype array
datatype vector = datatype vector
open Array (* array, fromList, tabulate, length, sub, update *)
end (* structure Array *)

structure IO = struct
exception Io of { name : string
                , function : string
                , cause : exn
                }
end (* structure IO *)

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
end (* local *)
end (* structure TextIO *)
val print : string -> unit = TextIO.print

structure OS = struct
local
    val oslib = LunarML.assumeDiscardable (Lua.global "os")
    val os_execute = LunarML.assumeDiscardable (Lua.field (oslib, "execute"))
    val os_exit = LunarML.assumeDiscardable (Lua.field (oslib, "exit"))
    val os_getenv = LunarML.assumeDiscardable (Lua.field (oslib, "getenv"))
    val os_remove = LunarML.assumeDiscardable (Lua.field (oslib, "remove"))
    val os_rename = LunarML.assumeDiscardable (Lua.field (oslib, "rename"))
in
structure FileSys = struct
val remove : string -> unit = fn filename => ( Lua.call os_remove (vector [Lua.fromString filename])
                                             ; ()
                                             )
val rename : {old : string, new : string} -> unit = fn {old, new} => ( Lua.call os_rename (vector [Lua.fromString old, Lua.fromString new])
                                                                     ; ()
                                                                     )
end (* structure FileSys *)
structure IO = struct end
structure Path = struct end
structure Process = struct
type status = int
val success : status = 0
val failure : status = 1
val isSuccess : status -> bool = fn 0 => true | _ => false
val system : string -> status = fn command => let val result = Lua.call os_execute (vector [Lua.fromString command])
                                              in failure (* TODO *)
                                              end
(* val atExit : (unit -> unit) -> unit *)
val exit : status -> 'a = fn status => let val result = Lua.call os_exit (vector [Lua.fromInt status, Lua.fromBool true])
                                       in raise Fail "os.exit not available"
                                       end
val terminate : status -> 'a = fn status => let val result = Lua.call os_exit (vector [Lua.fromInt status, Lua.fromBool false])
                                            in raise Fail "os.exit not available"
                                            end
val getEnv : string -> string option = fn name => let val result = Lua.call os_getenv (vector [Lua.fromString name])
                                                  in if Lua.isNil (Vector.sub (result, 0)) then
                                                         NONE
                                                     else
                                                         SOME (Lua.unsafeFromValue (Vector.sub (result, 0)))
                                                  end
(* val sleep : Time.time -> unit *)
end (* structure Process *)
end (* local *)
(*
eqtype syserror
exception SysErr of string * syserror option
val errorMsg : syserror -> string
val errorName : syserror -> string
val syserror : string -> syserror option
*)
end (* structure OS *)

structure CommandLine = struct
local
    val luaarg = LunarML.assumeDiscardable (Lua.global "arg")
in
val name : unit -> string = fn () => let val s = Lua.sub (luaarg, Lua.fromInt 0)
                                     in if Lua.isNil s then
                                            raise Fail "CommandLine.name: arg is not available"
                                        else
                                            Lua.unsafeFromValue s
                                     end
val arguments : unit -> string list = fn () => List.tabulate (Lua.unsafeFromValue (Lua.length luaarg), fn i => Lua.unsafeFromValue (Lua.sub (luaarg, Lua.fromInt (i + 1))) : string)
end
end (* structure CommandLine *)

end; (* local *)
