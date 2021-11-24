fun x <> y = Bool.not (x = y);

type unit = {}
datatype 'a option = NONE | SOME of 'a;

structure Lua : sig
              type value
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
                                          val log : value
                                          val maxinteger : value
                                          val mininteger : value
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
open Lua (* type value, sub, set, global, call, method, NIL, isNil, isFalsy, unsafeToValue, unsafeFromValue, newTable, function *)
val fromBool : bool -> value = unsafeToValue
val fromInt : int -> value = unsafeToValue
val fromWord : word -> value = unsafeToValue
val fromReal : real -> value = unsafeToValue
val fromString : string -> value = unsafeToValue
val fromChar : char -> value = unsafeToValue
fun field (t : value, name : string) = sub (t, fromString name)
structure Lib = struct
open Lib
val require = LunarML.assumeDiscardable (global "require")
val tonumber = LunarML.assumeDiscardable (global "tonumber")
val tostring = LunarML.assumeDiscardable (global "tostring")
val type' = LunarML.assumeDiscardable (global "type")
structure math = struct
open math
val atan = LunarML.assumeDiscardable (field (math, "atan"))
val log = LunarML.assumeDiscardable (field (math, "log"))
end
structure string = struct
open string
val byte = LunarML.assumeDiscardable (field (string, "byte"))
val char = LunarML.assumeDiscardable (field (string, "char"))
val find = LunarML.assumeDiscardable (field (string, "find"))
val gsub = LunarML.assumeDiscardable (field (string, "gsub"))
val match = LunarML.assumeDiscardable (field (string, "match"))
val sub = LunarML.assumeDiscardable (field (string, "sub"))
end
structure table = struct
open table
val concat = LunarML.assumeDiscardable (field (table, "concat"))
end
val lfs = LunarML.assumeDiscardable (let val results = Lua.call pcall #[require, fromString "lfs"]
                                         val ok = unsafeFromValue (Vector.sub (results, 0)) : bool
                                     in if ok then
                                            SOME (Vector.sub (results, 1))
                                        else
                                            NONE
                                     end
                                    )
end
fun typeof x : string = let val results = Lua.call Lib.type' #[x]
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
fun checkInt x : int = let val results = Lua.call Lib.math.type' #[x]
                       in if == (Vector.sub (results, 0), fromString "integer") then
                              unsafeFromValue x
                          else
                              raise TypeError (String.^ ("expected an integer, but got ", typeof x))
                       end
fun checkWord x : word = let val results = Lua.call Lib.math.type' #[x]
                         in if == (Vector.sub (results, 0), fromString "integer") then
                                unsafeFromValue x
                            else
                                raise TypeError (String.^ ("expected an integer, but got ", typeof x))
                         end
fun checkReal x : real = let val results = Lua.call Lib.math.type' #[x]
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

structure Vector : sig
              datatype vector = datatype vector
              (* val maxLen : int; defined later *)
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
              (* val collate : ('a * 'a -> order) -> 'a vector * 'a vector -> order; defined later *)
          end = struct
datatype vector = datatype vector
open Vector (* tabulate, length, sub, concat *)
val fromList = _primVal "Vector.fromList"
fun update (vec, n, x) = tabulate (length vec, fn i => if i = n then
                                                           x
                                                       else
                                                           sub (vec, i)
                                  )
local
    fun foldli' (f, acc, vec, i) = if i >= length vec then
                                       acc
                                   else
                                       foldli' (f, f (i, sub (vec, i), acc), vec, i + 1)
    fun foldri' (f, acc, vec, i) = if i < 0 then
                                       acc
                                   else
                                       foldri' (f, f (i, sub (vec, i), acc), vec, i - 1)
in
fun foldli f init vec : 'b = foldli' (f, init, vec, 0)
fun foldri f init vec : 'b = foldri' (f, init, vec, length vec - 1)
end
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
fun appi f vec = let val n = length vec
                     fun go i = if i = n then
                                    ()
                                else
                                    ( f (i, sub (vec, i)) : unit
                                    ; go (i + 1)
                                    )
                 in go 0
                 end
fun app f vec = let val n = length vec
                    fun go i = if i = n then
                                   ()
                               else
                                   ( f (sub (vec, i)) : unit
                                   ; go (i + 1)
                                   )
                in go 0
                end
fun mapi f vec = tabulate (length vec, fn i => f (i, sub (vec, i)))
fun map f vec = tabulate (length vec, fn i => f (sub (vec, i)))
fun findi f vec = let val n = length vec
                      fun go i = if i = n then
                                     NONE
                                 else
                                     let val x = sub (vec, i)
                                     in if f (i, x) then
                                            SOME (i, x)
                                        else
                                            go (i + 1)
                                     end
                   in go 0
                   end
fun find f vec = let val n = length vec
                     fun go i = if i = n then
                                    NONE
                                else
                                    let val x = sub (vec, i)
                                    in if f x then
                                           SOME x
                                       else
                                           go (i + 1)
                                    end
                 in go 0
                 end
fun exists f vec = let val n = length vec
                       fun go i = if i = n then
                                      false
                                  else
                                      f (sub (vec, i)) orelse go (i + 1)
                   in go 0
                   end
fun all f vec = let val n = length vec
                    fun go i = if i = n then
                                   true
                               else
                                   f (sub (vec, i)) andalso go (i + 1)
                in go 0
                end
end
val vector : 'a list -> 'a vector = Vector.fromList;

structure General : sig
              type unit = {}
              type exn = exn
              exception Bind
              exception Match
              exception Chr
              exception Div
              exception Domain
              exception Fail of string
              exception Overflow
              exception Size
              exception Span
              exception Subscript
              datatype order = LESS | EQUAL | GREATER
              val ! : 'a ref -> 'a
              val := : 'a ref * 'a -> unit
              val before : 'a * unit -> 'a
              val ignore : 'a -> unit
              val o : ('b -> 'c) * ('a -> 'b) -> 'a -> 'c
          end = struct
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
open General;
(*
val op before : 'a * unit -> 'a = General.before;
val ignore : 'a -> unit = General.ignore;
val op o : ('b -> 'c) * ('a -> 'b) -> 'a -> 'c = General.o;
*)

structure Bool : sig
              datatype bool = datatype bool
              val not : bool -> bool
              val toString : bool -> string
          end = struct
datatype bool = datatype bool
open Bool
fun toString true = "true"
  | toString false = "false"
(* scan, fromString *)
end (* structure Bool *)
val not : bool -> bool = Bool.not;

structure Int : sig
              type int = int
              val toInt : int -> int
              val fromInt : int -> int
              val precision : int option
              val minInt : int option
              val maxInt : int option
              val + : int * int -> int
              val - : int * int -> int
              val * : int * int -> int
              val div : int * int -> int
              val mod : int * int -> int
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
              val toString : int -> string
              val fromString : string -> int option
          end = struct
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
                                  in SOME (computeWordSize (Lua.unsafeFromValue Lua.Lib.math.maxinteger, 1))
                                  end
                                 )
val minInt : int option = LunarML.assumeDiscardable (SOME (Lua.unsafeFromValue Lua.Lib.math.mininteger))
val maxInt : int option = LunarML.assumeDiscardable (SOME (Lua.unsafeFromValue Lua.Lib.math.maxinteger))
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
fun toString (x : int) : string = let val result = Lua.call Lua.Lib.tostring #[Lua.fromInt x]
                                      val result = Lua.call Lua.Lib.string.gsub #[Vector.sub (result, 0), Lua.fromString "-", Lua.fromString "~"]
                                  in Lua.unsafeFromValue (Vector.sub (result, 0))
                                  end
(* scan *)
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
end; (* structure Int *)

structure Word : sig
              type word = word
              val wordSize : int
              val toInt : word -> int
              val toIntX : word -> int
              val fromInt : int -> word
              val andb : word * word -> word
              val orb : word * word -> word
              val xorb : word * word -> word
              val notb : word -> word
              val << : word * word -> word
              val >> : word * word -> word
              val ~>> : word * word -> word
              val + : word * word -> word
              val - : word * word -> word
              val * : word * word -> word
              val div : word * word -> word
              val mod : word * word -> word
              val ~ : word -> word
              val compare : word * word -> order
              val < : word * word -> bool
              val <= : word * word -> bool
              val > : word * word -> bool
              val >= : word * word -> bool
              val min : word * word -> word
              val max : word * word -> word
              val toString : word -> string
          end = struct
type word = word
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
(* fmt *)
val toString : word -> string = fn x => Lua.unsafeFromValue (Vector.sub (Lua.call Lua.Lib.string.format #[Lua.fromString "%X", Lua.fromWord x], 0))
(* scan, fromString *)
end; (* structure Word *)

structure Real : sig
              type real = real
              val + : real * real -> real
              val - : real * real -> real
              val * : real * real -> real
              val / : real * real -> real
              val ~ : real -> real
              val abs : real -> real
              val < : real * real -> bool
              val <= : real * real -> bool
              val > : real * real -> bool
              val >= : real * real -> bool
          end = struct
type real = real
open Real (* +, -, *, /, ~, abs, <, <=, >, >= *)
end; (* structure Real *)

structure Math : sig
              type real = real
              val pi : real
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
          end = struct
type real = real
val pi : real = LunarML.assumeDiscardable (Lua.unsafeFromValue (Lua.field (Lua.Lib.math, "pi")))
(* val e : real *)
val sqrt : real -> real = LunarML.assumeDiscardable (Lua.unsafeFromValue (Lua.field (Lua.Lib.math, "sqrt")))
val sin : real -> real = LunarML.assumeDiscardable (Lua.unsafeFromValue (Lua.field (Lua.Lib.math, "sin")))
val cos : real -> real = LunarML.assumeDiscardable (Lua.unsafeFromValue (Lua.field (Lua.Lib.math, "cos")))
val tan : real -> real = LunarML.assumeDiscardable (Lua.unsafeFromValue (Lua.field (Lua.Lib.math, "tan")))
val asin : real -> real = LunarML.assumeDiscardable (Lua.unsafeFromValue (Lua.field (Lua.Lib.math, "asin")))
val acos : real -> real = LunarML.assumeDiscardable (Lua.unsafeFromValue (Lua.field (Lua.Lib.math, "acos")))
val atan : real -> real = LunarML.assumeDiscardable (Lua.unsafeFromValue Lua.Lib.math.atan)
val atan2 : real * real -> real = fn (y, x) => Lua.unsafeFromValue (Vector.sub (Lua.call Lua.Lib.math.atan #[Lua.fromReal y, Lua.fromReal x], 0))
val exp : real -> real = LunarML.assumeDiscardable (Lua.unsafeFromValue (Lua.field (Lua.Lib.math, "exp")))
val pow : real * real -> real = fn (x, y) => Lua.unsafeFromValue (Lua.pow (Lua.fromReal x, Lua.fromReal y))
val ln : real -> real = LunarML.assumeDiscardable (Lua.unsafeFromValue Lua.Lib.math.log)
val log10 : real -> real = fn x => Lua.unsafeFromValue (Vector.sub (Lua.call Lua.Lib.math.log #[Lua.fromReal x, Lua.fromInt 10], 0))
(*
val sinh : real -> real
val cosh : real -> real
val tanh : real -> real
*)
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
(* tokens, fields, isPrefix, isSubstring, isSuffix, collate, toString, scan, fromString, toCString, fromCString *)
fun compare (s, t) = if s = t then
                         EQUAL
                     else if String.< (s, t) then
                         LESS
                     else
                         GREATER
open String (* size, ^, str, <, <=, >, >= *)
end (* structure String *)
val op ^ : string * string -> string = String.^
val size : string -> int = String.size
val str : char -> string = String.str;

structure Substring :> sig
              type substring
              type char = char
              type string = string
              val sub : substring * int -> char
              val size : substring -> int
              val base : substring -> string * int * int
              val full : string -> substring
              val getc : substring -> (char * substring) option
          end = struct
type char = char
type string = string
type substring = string * int * int (* the underlying string, the starting index, the size *)
fun sub ((s, i, z), j) = if 0 <= j andalso j < z then
                             String.sub (s, i + j)
                         else
                             raise Subscript
fun size (_, _, z) = z
fun base x = x
fun full s = (s, 0, String.size s)
fun getc (s, i, z) = if z = 0 then
                         NONE
                     else
                         SOME (String.sub (s, i), (s, i + 1, z - 1))
end;

structure Char : sig
              type char = char
              type string = string
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
          end = struct
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
                         #"\\a" => "\\a"
                       | #"\\b" => "\\b"
                       | #"\\t" => "\\t"
                       | #"\\n" => "\\n"
                       | #"\\v" => "\\v"
                       | #"\\f" => "\\f"
                       | #"\\r" => "\\r"
                       | _ => let val x = ord c
                              in if x < 32 then
                                     "\\^" ^ String.str (chr (x + 64))
                                 else if x < 100 then
                                     "\\0" ^ Int.toString x
                                 else
                                     "\\" ^ Int.toString x
                                 (* TODO: x >= 1000 *)
                              end
open Char (* <, <=, >, >= *)
(* scan, fromString, toCString, fromCString *)
end (* structure Char *)
val chr = Char.chr
val ord = Char.ord;

structure List : sig
              datatype list = datatype list
              exception Empty
              val null : 'a list -> bool
              val length : 'a list -> int
              val @ : 'a list * 'a list -> 'a list
              val hd : 'a list -> 'a
              val tl : 'a list -> 'a list
              val last : 'a list -> 'a
              val getItem : 'a list -> ('a * 'a list) option
              val nth : 'a list * int -> 'a
              val take : 'a list * int -> 'a list
              val drop : 'a list * int -> 'a list
              val rev : 'a list -> 'a list
              val concat : 'a list list -> 'a list
              val revAppend : 'a list * 'a list -> 'a list
              val app : ('a -> unit) -> 'a list -> unit
              val map : ('a -> 'b) -> 'a list -> 'b list
              val mapPartial : ('a -> 'b option) -> 'a list -> 'b list
              val find : ('a -> bool) -> 'a list -> 'a option
              val filter : ('a -> bool) -> 'a list -> 'a list
              val partition : ('a -> bool) -> 'a list -> 'a list * 'a list
              val foldl : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b
              val foldr : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b
              val exists : ('a -> bool) -> 'a list -> bool
              val all : ('a -> bool) -> 'a list -> bool
              val tabulate : int * (int -> 'a) -> 'a list
              val collate : ('a * 'a -> order) -> 'a list * 'a list -> order
          end = struct
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
  | collate compare (x :: xs, y :: ys) = case compare (x, y) of
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
val tl : 'a list -> 'a list = List.tl;

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
                                                | (false, false) => case compare (sub (xs, i), sub (ys, i)) of
                                                                        EQUAL => go (i + 1)
                                                                      | t => t
                               in go 0
                               end
end

structure Option : sig
              datatype 'a option = NONE | SOME of 'a
              exception Option
              val getOpt : 'a option * 'a -> 'a
              val isSome : 'a option -> bool
              val valOf : 'a option -> 'a
              val filter : ('a -> bool) -> 'a -> 'a option
              val join : 'a option option -> 'a option
              val app : ('a -> unit) -> 'a option -> unit
              val map : ('a -> 'b) -> 'a option -> 'b option
              val mapPartial : ('a -> 'b option) -> 'a option -> 'b option
              val compose : ('a -> 'b) * ('c -> 'a option) -> 'c -> 'b option
              val composePartial : ('a -> 'b option) * ('c -> 'a option) -> 'c -> 'b option
          end = struct
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
val valOf : 'a option -> 'a = Option.valOf;

structure Array : sig
              datatype array = datatype array
              datatype vector = datatype vector
              val array : int * 'a -> 'a array
              val fromList : 'a list -> 'a array
              val tabulate : int * (int -> 'a) -> 'a array
              val length : 'a array -> int
              val sub : 'a array * int -> 'a
              val update : 'a array * int * 'a -> unit
          end = struct
datatype array = datatype array
datatype vector = datatype vector
open Array (* array, fromList, tabulate, length, sub, update *)
end; (* structure Array *)

structure IO : sig
              exception Io of { name : string
                              , function : string
                              , cause : exn
                              }
          end = struct
exception Io of { name : string
                , function : string
                , cause : exn
                }
end; (* structure IO *)

structure TextIO :> sig
              type instream
              type outstream
              type vector = string
              type elem = char
              val input1 : instream -> elem option
              val inputN : instream * int -> vector
              val inputAll : instream -> vector
              val closeIn : instream -> unit
              val output : outstream * vector -> unit
              val output1 : outstream * elem -> unit
              val flushOut : outstream -> unit
              val closeOut : outstream -> unit
              val inputLine : instream -> string option
              val openIn : string -> instream
              val openOut : string -> outstream
              val openAppend : string -> outstream
              val stdIn : instream
              val stdOut : outstream
              val stdErr : outstream
              val print : string -> unit
          end = struct
local
    val io = LunarML.assumeDiscardable (Lua.global "io")
    val io_open = LunarML.assumeDiscardable (Lua.field (io, "open"))
    val io_write = LunarML.assumeDiscardable (Lua.field (io, "write"))
    structure Instream :> sig
                  type instream
                  type vector = string
                  type elem = char
                  val input1 : instream -> elem option
                  val inputN : instream * int -> vector
                  val inputAll : instream -> vector
                  val closeIn : instream -> unit
                  val inputLine : instream -> string option
                  val openIn : string -> instream
                  val stdIn : instream
              end = struct
    type instream = Lua.value
    type vector = string
    type elem = char
    fun input1 f = let val result = Vector.sub (Lua.method (f, "read") (vector [Lua.fromInt 1]), 0)
                   in if Lua.isNil result then
                          NONE
                      else
                          SOME (Lua.unsafeFromValue result : elem)
                   end
    fun inputN (f, n : int) = if n < 0 then
                                  raise Size
                              else
                                  let val result = Vector.sub (Lua.method (f, "read") (vector [Lua.fromInt n]), 0)
                                  in if Lua.isNil result then
                                         ""
                                     else
                                         (Lua.unsafeFromValue result : vector)
                                  end
    fun inputAll f = let val result = Vector.sub (Lua.method (f, "read") (vector [Lua.fromString "a"]), 0)
                     in Lua.unsafeFromValue result : vector
                     end
    fun closeIn f = (Lua.method (f, "close") (vector []); ())

    (* TEXT_IO *)
    fun inputLine f = let val result = Vector.sub (Lua.method (f, "read") (vector [Lua.fromString "L"]), 0)
                      in if Lua.isNil result then
                             NONE
                         else
                             SOME (Lua.unsafeFromValue result : string)
                      end
    fun openIn f = let val result = Lua.call io_open #[Lua.fromString f, Lua.fromString "r"]
                   in if Lua.isNil (Vector.sub (result, 0)) then
                          raise IO.Io { name = f, function = "TextIO.openIn", cause = Fail (Lua.unsafeFromValue (Vector.sub (result, 1))) } (* TODO: cause *)
                      else
                          Vector.sub (result, 0)
                   end
    val stdIn = LunarML.assumeDiscardable (Lua.field (io, "stdin"))
    end
    structure Outstream :> sig
                  type outstream
                  type vector = string
                  type elem = char
                  val output : outstream * vector -> unit
                  val output1 : outstream * elem -> unit
                  val flushOut : outstream -> unit
                  val closeOut : outstream -> unit
                  val openOut : string -> outstream
                  val openAppend : string -> outstream
                  val stdOut : outstream
                  val stdErr : outstream
                  val print : string -> unit
              end = struct
    type outstream = Lua.value
    type vector = string
    type elem = char
    fun output (f, s) = (Lua.method (f, "write") (vector [Lua.fromString s]); ())
    fun output1 (f, c) = (Lua.method (f, "write") (vector [Lua.fromString (String.str c)]); ())
    fun flushOut f = (Lua.method (f, "flush") (vector []); ())
    fun closeOut f = (Lua.method (f, "close") (vector []); ())
    (* outputsubstr : outstream * substring -> unit *)
    fun openOut f = let val result = Lua.call io_open #[Lua.fromString f, Lua.fromString "w"]
                    in if Lua.isNil (Vector.sub (result, 0)) then
                           raise IO.Io { name = f, function = "TextIO.openOut", cause = Fail (Lua.unsafeFromValue (Vector.sub (result, 1))) } (* TODO: cause *)
                       else
                           Vector.sub (result, 0)
                    end
    fun openAppend f = let val result = Lua.call io_open #[Lua.fromString f, Lua.fromString "a"]
                       in if Lua.isNil (Vector.sub (result, 0)) then
                              raise IO.Io { name = f, function = "TextIO.openAppend", cause = Fail (Lua.unsafeFromValue (Vector.sub (result, 1))) } (* TODO: cause *)
                          else
                              Vector.sub (result, 0)
                       end
    val stdOut = LunarML.assumeDiscardable (Lua.field (io, "stdout"))
    val stdErr = LunarML.assumeDiscardable (Lua.field (io, "stderr"))
    fun print s = (Lua.call io_write #[Lua.fromString s]; ())
    end
in
open Instream
open Outstream
(* IMPERATIVE_IO *)
(* fun openString f *)
(* scanStream *)
end (* local *)
end (* structure TextIO *)
val print : string -> unit = TextIO.print;

structure OS :> sig
              structure FileSys : sig
                            val chDir : string -> unit (* requires LuaFileSystem *)
                            val getDir : unit -> string (* requires LuaFileSystem *)
                            val mkDir : string -> unit (* requires LuaFileSystem *)
                            val rmDir : string -> unit (* requires LuaFileSystem *)
                            val isDir : string -> bool (* requires LuaFileSystem *)
                            val isLink : string -> bool (* requires LuaFileSystem *)
                            val readLink : string -> string (* requires LuaFileSystem 1.7.0 or later *)
                            val remove : string -> unit
                            val rename : { old : string, new : string } -> unit
                        end
              structure IO : sig
                        end
              structure Path : sig
                        end
              structure Process : sig
                            type status
                            val success : status
                            val failure : status
                            val isSuccess : status -> bool
                            val system : string -> status
                            val exit : status -> 'a
                            val terminate : status -> 'a
                            val getEnv : string -> string option
                        end
              eqtype syserror
              exception SysErr of string * syserror option
          end = struct
type syserror = string
exception SysErr of string * syserror option
local
    val oslib = LunarML.assumeDiscardable (Lua.global "os")
    val os_execute = LunarML.assumeDiscardable (Lua.field (oslib, "execute"))
    val os_exit = LunarML.assumeDiscardable (Lua.field (oslib, "exit"))
    val os_getenv = LunarML.assumeDiscardable (Lua.field (oslib, "getenv"))
    val os_remove = LunarML.assumeDiscardable (Lua.field (oslib, "remove"))
    val os_rename = LunarML.assumeDiscardable (Lua.field (oslib, "rename"))
in
structure FileSys = struct
(*
type dirstream
val openDir : string -> dirstream : lfs.dir?
val readDir : dirstream -> string open : dir_obj:next()
val rewindDir : dirstream -> unit
val closeDir : dirstream -> unit : dir_obj:close()
val fullPath : string -> string
val realPath : string -> string
datatype access_mode = A_READ | A_WRITE | A_EXEC
val access : string * access_mode list -> bool
val tmpName : unit -> string : os.tmpname
eqtype file_id
val fileId : string -> file_id : lfs.attributes "ino"?
val hash : file_id -> word
val compare : file_id * file_id -> order
*)
fun use_lfs (field, f : Lua.value -> 'a -> 'b) = case Lua.Lib.lfs of
                                                     SOME lfs => f (Lua.field (lfs, field))
                                                   | NONE => fn _ => raise SysErr ("LuaFileSystem not available", NONE)
val chDir : string -> unit = LunarML.assumeDiscardable (use_lfs ("chdir", fn lfs_chdir =>
                                                                             fn path => let val results = Lua.call lfs_chdir #[Lua.fromString path]
                                                                                        in if Lua.isFalsy (Vector.sub (results, 0)) then
                                                                                               let val message = Lua.checkString (Vector.sub (results, 1))
                                                                                               in raise SysErr (message, SOME message)
                                                                                               end
                                                                                           else
                                                                                               ()
                                                                                        end
                                                                )
                                                       )
val getDir : unit -> string = LunarML.assumeDiscardable (use_lfs ("currentdir", fn lfs_currentdir =>
                                                                                   fn () => let val results = Lua.call lfs_currentdir #[]
                                                                                                val r0 = Vector.sub (results, 0)
                                                                                            in if Lua.typeof r0 = "string" then
                                                                                                   Lua.unsafeFromValue r0
                                                                                               else
                                                                                                   let val message = Lua.checkString (Vector.sub (results, 1))
                                                                                                   in raise SysErr (message, SOME message)
                                                                                                   end
                                                                                            end
                                                                 )
                                                        )
val mkDir : string -> unit = LunarML.assumeDiscardable (use_lfs ("mkdir", fn lfs_mkdir =>
                                                                             fn path => let val results = Lua.call lfs_mkdir #[Lua.fromString path]
                                                                                        in if Lua.isFalsy (Vector.sub (results, 0)) then
                                                                                               let val message = Lua.checkString (Vector.sub (results, 1))
                                                                                               in raise SysErr (message, SOME message)
                                                                                               end
                                                                                           else
                                                                                               ()
                                                                                        end
                                                                )
                                                       )
val rmDir : string -> unit = LunarML.assumeDiscardable (use_lfs ("rmdir", fn lfs_rmdir =>
                                                                             fn path => let val results = Lua.call lfs_rmdir #[Lua.fromString path]
                                                                                        in if Lua.isFalsy (Vector.sub (results, 0)) then
                                                                                               let val message = Lua.checkString (Vector.sub (results, 1))
                                                                                               in raise SysErr (message, SOME message)
                                                                                               end
                                                                                           else
                                                                                               ()
                                                                                        end
                                                                )
                                                       )
val isDir : string -> bool = LunarML.assumeDiscardable (use_lfs ("attributes", fn lfs_attributes =>
                                                                                  fn path => let val results = Lua.call lfs_attributes #[Lua.fromString path, Lua.fromString "mode"]
                                                                                                 val r0 = Vector.sub (results, 0)
                                                                                             in if Lua.isFalsy r0 then
                                                                                                    let val message = Lua.checkString (Vector.sub (results, 1))
                                                                                                    in raise SysErr (message, SOME message)
                                                                                                    end
                                                                                                else
                                                                                                    Lua.== (r0, Lua.fromString "directory")
                                                                                             end
                                                                )
                                                       )
val isLink : string -> bool = LunarML.assumeDiscardable (use_lfs ("symlinkattributes", fn lfs_symlinkattributes =>
                                                                                          fn path => let val results = Lua.call lfs_symlinkattributes #[Lua.fromString path, Lua.fromString "mode"]
                                                                                                         val r0 = Vector.sub (results, 0)
                                                                                                     in if Lua.isFalsy r0 then
                                                                                                            let val message = Lua.checkString (Vector.sub (results, 1))
                                                                                                            in raise SysErr (message, SOME message)
                                                                                                            end
                                                                                                        else
                                                                                                            Lua.== (r0, Lua.fromString "link")
                                                                                                     end
                                                                 )
                                                        )
val readLink : string -> string = LunarML.assumeDiscardable (use_lfs ("symlinkattributes", fn lfs_symlinkattributes =>
                                                                                              fn path => let val results = Lua.call lfs_symlinkattributes #[Lua.fromString path, Lua.fromString "target"]
                                                                                                             val r0 = Vector.sub (results, 0)
                                                                                                         in if Lua.isFalsy r0 then
                                                                                                                let val message = Lua.checkString (Vector.sub (results, 1))
                                                                                                                in raise SysErr (message, SOME message)
                                                                                                                end
                                                                                                            else
                                                                                                                Lua.checkString r0
                                                                                                         end
                                                                     )
                                                            )
(* fullPath, realPath *)
(*
val modTime : string -> Time.time = LunarML.assumeDiscardable (use_lfs ("attributes", fn lfs_attributes =>
                                                                                  fn path => let val results = Lua.call lfs_attributes #[Lua.fromString path, Lua.fromString "modification"]
                                                                                                 val r0 = Vector.sub (results, 0)
                                                                                             in if Lua.isFalsy r0 then
                                                                                                    let val message = Lua.checkString (Vector.sub (results, 1))
                                                                                                    in raise SysErr (message, SOME message)
                                                                                                    end
                                                                                                else
                                                                                                    raise Fail "modTime: not implemented yet"
                                                                                             end
                                                                       )
                                                              )
val fileSize : string -> Position.int = LunarML.assumeDiscardable (use_lfs ("attributes", fn lfs_attributes =>
                                                                                             fn path => let val results = Lua.call lfs_attributes #[Lua.fromString path, Lua.fromString "size"]
                                                                                                            val r0 = Vector.sub (results, 0)
                                                                                                        in if Lua.isFalsy r0 then
                                                                                                               let val message = Lua.checkString (Vector.sub (results, 1))
                                                                                                               in raise SysErr (message, SOME message)
                                                                                                               end
                                                                                                           else
                                                                                                               raise Fail "fileSize: not implemented yet"
                                                                                                        end
                                                                           )
                                                                  )
val setTime : string * Time.time option -> unit = LunarML.assumeDiscardable (use_lfs ("touch", fn lfs_touch =>
                                                                                                  (fn (path, NONE) => let val results = Lua.call lfs_touch #[Lua.fromString path]
                                                                                                                          val r0 = Vector.sub (results, 0)
                                                                                                                      in if Lua.isFalsy r0 then
                                                                                                                             let val message = Lua.checkString (Vector.sub (results, 1))
                                                                                                                             in raise SysErr (message, SOME message)
                                                                                                                             end
                                                                                                                         else
                                                                                                                             ()
                                                                                                                      end
                                                                                                  | (path, SOME t) => let val results = Lua.call lfs_touch #[Lua.fromString path, (* TODO *) t, (* TODO *) t]
                                                                                                                          val r0 = Vector.sub (results, 0)
                                                                                                                      in if Lua.isFalsy r0 then
                                                                                                                             let val message = Lua.checkString (Vector.sub (results, 1))
                                                                                                                             in raise SysErr (message, SOME message)
                                                                                                                             end
                                                                                                                         else
                                                                                                                             ()
                                                                                                                      end
                                                                                                  )
                                                                                     )
                                                                            )
*)
val remove : string -> unit = fn filename => ignore (Lua.call os_remove #[Lua.fromString filename])
val rename : {old : string, new : string} -> unit = fn {old, new} => ignore (Lua.call os_rename #[Lua.fromString old, Lua.fromString new])
end (* structure FileSys *)
structure IO = struct end
structure Path = struct end
structure Process = struct
type status = int
val success : status = 0
val failure : status = 1
val isSuccess : status -> bool = fn 0 => true | _ => false
val system : string -> status = fn command => let val result = Lua.call os_execute #[Lua.fromString command]
                                              in failure (* TODO *)
                                              end
(* val atExit : (unit -> unit) -> unit *)
val exit : status -> 'a = fn status => let val result = Lua.call os_exit #[Lua.fromInt status, Lua.fromBool true]
                                       in raise Fail "os.exit not available"
                                       end
val terminate : status -> 'a = fn status => let val result = Lua.call os_exit #[Lua.fromInt status, Lua.fromBool false]
                                            in raise Fail "os.exit not available"
                                            end
val getEnv : string -> string option = fn name => let val result = Lua.call os_getenv #[Lua.fromString name]
                                                  in if Lua.isNil (Vector.sub (result, 0)) then
                                                         NONE
                                                     else
                                                         SOME (Lua.unsafeFromValue (Vector.sub (result, 0)))
                                                  end
(* val sleep : Time.time -> unit : LuaSocket's socket.sleep or luaposix's posix.time.nanosleep or use native API (nanosleep or Sleep) via FFI or system command via os.execute or busy loop *)
end (* structure Process *)
end (* local *)
(*
eqtype syserror
exception SysErr of string * syserror option
val errorMsg : syserror -> string
val errorName : syserror -> string
val syserror : string -> syserror option
*)
end; (* structure OS *)

structure CommandLine : sig
              val name : unit -> string
              val arguments : unit -> string list
          end = struct
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
end; (* structure CommandLine *)
