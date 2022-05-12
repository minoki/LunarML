structure Bool = struct
fun not x = _primCall "Bool.not" (x)
end;
_equality bool = fn (x, y) => _primCall "Bool.=" (x, y);

type unit = {}
datatype 'a option = NONE | SOME of 'a;

structure Int = struct
type int = int
val ~ = _Prim.Int.~
val abs = _Prim.Int.abs
fun x + y = _primCall "call2" (_Prim.Int.+, x, y)
fun x - y = _primCall "call2" (_Prim.Int.-, x, y)
fun x * y = _primCall "call2" (_Prim.Int.*, x, y)
fun x div y = _primCall "call2" (_Prim.Int.div, x, y)
fun x mod y = _primCall "call2" (_Prim.Int.mod, x, y)
fun x < y = _primCall "Int.<" (x, y)
fun x <= y = _primCall "Int.<=" (x, y)
fun x > y = _primCall "Int.>" (x, y)
fun x >= y = _primCall "Int.>=" (x, y)
fun fromInt (x : int) = x
end
_equality int = fn (x, y) => _primCall "Int.=" (x, y);
_overload "Int" [int] { + = Int.+
                      , - = Int.-
                      , * = Int.*
                      , div = Int.div
                      , mod = Int.mod
                      , ~ = Int.~
                      , abs = Int.abs
                      , < = Int.<
                      , <= = Int.<=
                      , > = Int.>
                      , >= = Int.>=
                      , fromInt = Int.fromInt
                      };

structure Word = struct
type word = word
fun ~ x = _primCall "Word.~" (x)
fun x + y = _primCall "Word.+" (x, y)
fun x - y = _primCall "Word.-" (x, y)
fun x * y = _primCall "Word.*" (x, y)
fun x div y = _primCall "call2" (_Prim.Word.div, x, y)
fun x mod y = _primCall "call2" (_Prim.Word.mod, x, y)
fun x < y = _primCall "Word.<" (x, y)
fun x <= y = _primCall "Word.<=" (x, y)
fun x > y = _primCall "Word.>" (x, y)
fun x >= y = _primCall "Word.>=" (x, y)
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

structure WideChar = struct
type char = _Prim.WideChar.char
fun x < y = _primCall "WideChar.<" (x, y)
fun x <= y = _primCall "WideChar.<=" (x, y)
fun x > y = _primCall "WideChar.>" (x, y)
fun x >= y = _primCall "WideChar.>=" (x, y)
end
_equality WideChar.char = fn (x, y) => _primCall "WideChar.=" (x, y);
_overload "Char" [WideChar.char] { < = WideChar.<
                                 , <= = WideChar.<=
                                 , > = WideChar.>
                                 , >= = WideChar.>=
                                 };

structure String = struct
type string = string
fun x < y = _primCall "String.<" (x, y)
fun x <= y = Bool.not (y < x)
fun x > y = y < x
fun x >= y = Bool.not (x < y)
fun x ^ y = _primCall "String.^" (x, y)
fun size x = _primCall "String.size" (x)
fun str x = _primCall "String.str" (x)
end
_equality string = fn (x, y) => _primCall "String.=" (x, y);
_overload "String" [string] { < = String.<
                            , <= = String.<=
                            , > = String.>
                            , >= = String.>=
                            };

structure WideString = struct
type string = _Prim.WideString.string
type char = WideChar.char
fun x < y = _primCall "WideString.<" (x, y)
fun x <= y = _primCall "WideString.<=" (x, y)
fun x > y = _primCall "WideString.>" (x, y)
fun x >= y = _primCall "WideString.>=" (x, y)
fun x ^ y = _primCall "WideString.^" (x, y)
fun size x = _primCall "WideString.size" (x)
fun str (x : char) : string = _primCall "Unsafe.cast" (x)
end
_equality WideString.string = fn (x, y) => _primCall "WideString.=" (x, y);
_overload "String" [WideString.string] { < = WideString.<
                                       , <= = WideString.<=
                                       , > = WideString.>
                                       , >= = WideString.>=
                                       };

structure Vector = struct
fun length vec = _primCall "Vector.length" (vec)
fun sub (vec, i) = if i < 0 orelse length vec <= i then
                       raise Subscript
                   else
                       Unsafe.Vector.sub (vec, i)
val tabulate = _Prim.Vector.tabulate
val concat = _Prim.Vector.concat
end
_equality ''a vector = fn (x, y) => _primCall "Vector.=" (op = : ''a * ''a -> bool, x, y);

structure LunarML : sig
              val assumePure : 'a -> 'a
              val assumeDiscardable : 'a -> 'a
          end = struct
val assumePure = _Prim.assumePure
val assumeDiscardable = _Prim.assumeDiscardable
end

structure JavaScript : sig
              type value
              val sub : value * value -> value
              val field : value * WideString.string -> value
              val set : value * value * value -> unit
              val global : WideString.string -> value
              val call : value -> value vector -> value
              val new : value -> value vector -> value
              val method : value * WideString.string -> value vector -> value
              val fromBool : bool -> value
              val fromInt : int -> value
              val fromWord : word -> value
              val fromReal : real -> value
              val fromWideString : WideString.string -> value
              val unsafeToValue : 'a -> value
              val unsafeFromValue : value -> 'a
              val === : value * value -> bool
              val !== : value * value -> bool
              val < : value * value -> bool
              val > : value * value -> bool
              val <= : value * value -> bool
              val >= : value * value -> bool
              val + : value * value -> value
              val - : value * value -> value
              val * : value * value -> value
              val / : value * value -> value
              val % : value * value -> value
              val negate : value -> value
              val andb : value * value -> value
              val orb : value * value -> value
              val xorb : value * value -> value
              val notb : value -> value
              val << : value * value -> value
              val >> : value * value -> value
              val >>> : value * value -> value
              val ** : value * value -> value
              val isFalsy : value -> bool
              val typeof : value -> WideString.string
              val newObject : unit -> value
              val encodeUtf8 : WideString.string -> string
              val decodeUtf8 : string -> WideString.string
              val toInt32 : value -> int
              val toUint32 : value -> word
              val require : value (* Node.js *)
              structure Lib : sig
                            val Number : value
                            structure Number : sig
                                          val isFinite : value
                                          val isNaN : value
                                          val POSITIVE_INFINITY : value
                                          val NEGATIVE_INFINITY : value
                                          val MIN_VALUE : value
                                          val MAX_VALUE : value
                                          val NaN : value
                                      end
                            val Math : value
                            structure Math : sig
                                          val E : value
                                          val PI : value
                                          val abs : value
                                          val acos : value
                                          val acosh : value
                                          val asin : value
                                          val asinh : value
                                          val atan : value
                                          val atanh : value
                                          val atan2 : value
                                          val cbrt : value
                                          val ceil : value
                                          val clz32 : value
                                          val cos : value
                                          val cosh : value
                                          val exp : value
                                          val expm1 : value
                                          val floor : value
                                          val fround : value
                                          val hypot : value
                                          val imul : value
                                          val log : value
                                          val log1p : value
                                          val log10 : value
                                          val log2 : value
                                          val max : value
                                          val min : value
                                          val pow : value
                                          val random : value
                                          val round : value
                                          val sign : value
                                          val sin : value
                                          val sinh : value
                                          val sqrt : value
                                          val tan : value
                                          val tanh : value
                                          val trunc : value
                                      end
                            val BigInt : value
                            structure BigInt : sig
                                          val asIntN : value
                                          val asUintN : value
                                      end
                            val Uint8Array : value
                        end
          end = struct
type value = _Prim.JavaScript.value
val call = _Prim.JavaScript.call
val new = _Prim.JavaScript.new
val method = _Prim.JavaScript.method
val encodeUtf8 = _Prim.JavaScript.encodeUtf8
val decodeUtf8 = _Prim.JavaScript.decodeUtf8
val require = _Prim.JavaScript.require
fun unsafeToValue x : value = _primCall "Unsafe.cast" (x)
fun unsafeFromValue (x : value) = _primCall "Unsafe.cast" (x)
val fromBool : bool -> value = unsafeToValue
val fromInt : int -> value = unsafeToValue
val fromWord : word -> value = unsafeToValue
val fromReal : real -> value = unsafeToValue
val fromWideString : WideString.string -> value = unsafeToValue
fun sub (obj, key) = _primCall "JavaScript.sub" (obj, key)
fun field (obj, key : WideString.string) = _primCall "JavaScript.sub" (obj, _primCall "Unsafe.cast" (key))
fun set (obj, key, value) = _primCall "JavaScript.set" (obj, key, value)
fun global name = _primCall "JavaScript.global" (name)
fun isFalsy x = _primCall "JavaScript.isFalsy" (x)
fun x + y = _primCall "JavaScript.+" (x, y)
fun x - y = _primCall "JavaScript.-" (x, y)
fun x * y = _primCall "JavaScript.*" (x, y)
fun x / y = _primCall "JavaScript./" (x, y)
fun % (x, y) = _primCall "JavaScript.%" (x, y)
fun negate x = _primCall "JavaScript.negate" (x)
fun andb (x, y) = _primCall "JavaScript.andb" (x, y)
fun orb (x, y) = _primCall "JavaScript.orb" (x, y)
fun xorb (x, y) = _primCall "JavaScript.xorb" (x, y)
fun notb x = _primCall "JavaScript.notb" (x)
fun << (x, y) = _primCall "JavaScript.<<" (x, y)
fun >> (x, y) = _primCall "JavaScript.>>" (x, y)
fun >>> (x, y) = _primCall "JavaScript.>>>" (x, y)
fun === (x, y) = _primCall "JavaScript.===" (x, y)
fun !== (x, y) = _primCall "JavaScript.!==" (x, y)
fun x < y = _primCall "JavaScript.<" (x, y)
fun x > y = _primCall "JavaScript.>" (x, y)
fun x <= y = _primCall "JavaScript.<=" (x, y)
fun x >= y = _primCall "JavaScript.>=" (x, y)
fun ** (x, y) = _primCall "JavaScript.**" (x, y)
fun typeof x = _primCall "JavaScript.typeof" (x)
fun toInt32 x = unsafeFromValue (orb (x, fromInt 0)) : int
fun toUint32 x = unsafeFromValue (>>> (x, fromInt 0)) : word
structure Lib = struct
val Object = LunarML.assumeDiscardable (global "Object")
val Number = LunarML.assumeDiscardable (global "Number")
structure Number = struct
val isFinite = LunarML.assumeDiscardable (field (Number, "isFinite"))
val isNaN = LunarML.assumeDiscardable (field (Number, "isNaN"))
val POSITIVE_INFINITY = LunarML.assumeDiscardable (field (Number, "POSITIVE_INFINITY"))
val NEGATIVE_INFINITY = LunarML.assumeDiscardable (field (Number, "NEGATIVE_INFINITY"))
val MIN_VALUE = LunarML.assumeDiscardable (field (Number, "MIN_VALUE"))
val MAX_VALUE = LunarML.assumeDiscardable (field (Number, "MAX_VALUE"))
val NaN = LunarML.assumeDiscardable (field (Number, "NaN"))
end
val Math = LunarML.assumeDiscardable (global "Math")
structure Math = struct
val E = LunarML.assumeDiscardable (field (Math, "E"))
val PI = LunarML.assumeDiscardable (field (Math, "PI"))
val abs = LunarML.assumeDiscardable (field (Math, "abs"))
val acos = LunarML.assumeDiscardable (field (Math, "acos"))
val acosh = LunarML.assumeDiscardable (field (Math, "acosh"))
val asin = LunarML.assumeDiscardable (field (Math, "asin"))
val asinh = LunarML.assumeDiscardable (field (Math, "asinh"))
val atan = LunarML.assumeDiscardable (field (Math, "atan"))
val atanh = LunarML.assumeDiscardable (field (Math, "atanh"))
val atan2 = LunarML.assumeDiscardable (field (Math, "atan2"))
val cbrt = LunarML.assumeDiscardable (field (Math, "cbrt"))
val ceil = LunarML.assumeDiscardable (field (Math, "ceil"))
val clz32 = LunarML.assumeDiscardable (field (Math, "clz32"))
val cos = LunarML.assumeDiscardable (field (Math, "cos"))
val cosh = LunarML.assumeDiscardable (field (Math, "cosh"))
val exp = LunarML.assumeDiscardable (field (Math, "exp"))
val expm1 = LunarML.assumeDiscardable (field (Math, "expm1"))
val floor = LunarML.assumeDiscardable (field (Math, "floor"))
val fround = LunarML.assumeDiscardable (field (Math, "fround"))
val hypot = LunarML.assumeDiscardable (field (Math, "hypot"))
val imul = LunarML.assumeDiscardable (field (Math, "imul"))
val log = LunarML.assumeDiscardable (field (Math, "log"))
val log1p = LunarML.assumeDiscardable (field (Math, "log1p"))
val log10 = LunarML.assumeDiscardable (field (Math, "log10"))
val log2 = LunarML.assumeDiscardable (field (Math, "log2"))
val max = LunarML.assumeDiscardable (field (Math, "max"))
val min = LunarML.assumeDiscardable (field (Math, "min"))
val pow = LunarML.assumeDiscardable (field (Math, "pow"))
val random = LunarML.assumeDiscardable (field (Math, "random"))
val round = LunarML.assumeDiscardable (field (Math, "round"))
val sign = LunarML.assumeDiscardable (field (Math, "sign"))
val sin = LunarML.assumeDiscardable (field (Math, "sin"))
val sinh = LunarML.assumeDiscardable (field (Math, "sinh"))
val sqrt = LunarML.assumeDiscardable (field (Math, "sqrt"))
val tan = LunarML.assumeDiscardable (field (Math, "tan"))
val tanh = LunarML.assumeDiscardable (field (Math, "tanh"))
val trunc = LunarML.assumeDiscardable (field (Math, "trunc"))
end
val BigInt = LunarML.assumeDiscardable (global "BigInt")
structure BigInt = struct
val asIntN = LunarML.assumeDiscardable (field (BigInt, "asIntN"))
val asUintN = LunarML.assumeDiscardable (field (BigInt, "asUintN"))
end
val Uint8Array = LunarML.assumeDiscardable (global "Uint8Array")
end
fun newObject () = new Lib.Object #[]
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
open Vector (* tabulate, concat, length, sub *)
val fromList = _Prim.Vector.fromList
fun update (vec, n, x) = tabulate (length vec, fn i => if i = n then
                                                           x
                                                       else
                                                           Unsafe.Vector.sub (vec, i)
                                  )
local
    fun foldli' (f, acc, vec, i) = if i >= length vec then
                                       acc
                                   else
                                       foldli' (f, f (i, Unsafe.Vector.sub (vec, i), acc), vec, i + 1)
    fun foldri' (f, acc, vec, i) = if i < 0 then
                                       acc
                                   else
                                       foldri' (f, f (i, Unsafe.Vector.sub (vec, i), acc), vec, i - 1)
in
fun foldli f init vec : 'b = foldli' (f, init, vec, 0)
fun foldri f init vec : 'b = foldri' (f, init, vec, length vec - 1)
end
local
    fun foldl' (f, acc, vec, i) = if i >= length vec then
                                      acc
                                  else
                                      foldl' (f, f (Unsafe.Vector.sub (vec, i), acc), vec, i + 1)
    fun foldr' (f, acc, vec, i) = if i < 0 then
                                      acc
                                  else
                                      foldr' (f, f (Unsafe.Vector.sub (vec, i), acc), vec, i - 1)
in
fun foldl (f : 'a * 'b -> 'b) (init : 'b) (vec : 'a vector) : 'b = foldl' (f, init, vec, 0)
fun foldr (f : 'a * 'b -> 'b) (init : 'b) (vec : 'a vector) : 'b = foldr' (f, init, vec, length vec - 1)
end
fun appi f vec = let val n = length vec
                     fun go i = if i = n then
                                    ()
                                else
                                    ( f (i, Unsafe.Vector.sub (vec, i)) : unit
                                    ; go (i + 1)
                                    )
                 in go 0
                 end
fun app f vec = let val n = length vec
                    fun go i = if i = n then
                                   ()
                               else
                                   ( f (Unsafe.Vector.sub (vec, i)) : unit
                                   ; go (i + 1)
                                   )
                in go 0
                end
fun mapi f vec = tabulate (length vec, fn i => f (i, Unsafe.Vector.sub (vec, i)))
fun map f vec = tabulate (length vec, fn i => f (Unsafe.Vector.sub (vec, i)))
fun findi f vec = let val n = length vec
                      fun go i = if i = n then
                                     NONE
                                 else
                                     let val x = Unsafe.Vector.sub (vec, i)
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
                                    let val x = Unsafe.Vector.sub (vec, i)
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
                                      f (Unsafe.Vector.sub (vec, i)) orelse go (i + 1)
                   in go 0
                   end
fun all f vec = let val n = length vec
                    fun go i = if i = n then
                                   true
                               else
                                   f (Unsafe.Vector.sub (vec, i)) andalso go (i + 1)
                in go 0
                end
end

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
              val exnName : exn -> string
              datatype order = LESS | EQUAL | GREATER
              val ! : 'a ref -> 'a
              val := : 'a ref * 'a -> unit
              val before : 'a * unit -> 'a
              val ignore : 'a -> unit
              val o : ('b -> 'c) * ('a -> 'b) -> 'a -> 'c
          end = struct
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
val exnName = exnName
(*
val exnMessage : exn -> string
*)
datatype order = LESS | EQUAL | GREATER
fun ! (ref x) = x
fun x := y = _primCall "Ref.:=" (x, y)
fun x before () = x
fun ignore _ = ()
fun (f o g) x = f (g x)
end (* structure General *)
open General;

(* depends on int, option, General.Subscript, General.order *)
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
val op @ = List.@;

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
val precision : int option = SOME 32
val minInt : int option = SOME ~0x80000000
val maxInt : int option = SOME 0x7fffffff
fun quot (x, y) = _primCall "call2" (_Prim.Int.quot, x, y)
fun rem (x, y) = _primCall "call2" (_Prim.Int.rem, x, y)
val compare : int * int -> order = fn (x, y) => if x = y then
                                                    EQUAL
                                                else if x < y then
                                                    LESS
                                                else
                                                    GREATER
(* Maybe use Math.min/max? *)
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
fun fmt StringCvt.BIN x = let val s = if x >= 0 then
                                          JavaScript.unsafeFromValue (JavaScript.method (JavaScript.fromInt x, "toString") #[JavaScript.fromInt 2])
                                      else
                                          WideString.^ ("~", JavaScript.unsafeFromValue (JavaScript.method (JavaScript.negate (JavaScript.fromInt x), "toString") #[JavaScript.fromInt 2]))
                          in JavaScript.encodeUtf8 s
                          end
  | fmt StringCvt.OCT x = let val s = if x >= 0 then
                                          JavaScript.unsafeFromValue (JavaScript.method (JavaScript.fromInt x, "toString") #[JavaScript.fromInt 8])
                                      else
                                          WideString.^ ("~", JavaScript.unsafeFromValue (JavaScript.method (JavaScript.negate (JavaScript.fromInt x), "toString") #[JavaScript.fromInt 8]))
                          in JavaScript.encodeUtf8 s
                          end
  | fmt StringCvt.DEC x = let val s = if x >= 0 then
                                          JavaScript.unsafeFromValue (JavaScript.method (JavaScript.fromInt x, "toString") #[])
                                      else
                                          WideString.^ ("~", JavaScript.unsafeFromValue (JavaScript.method (JavaScript.negate (JavaScript.fromInt x), "toString") #[]))
                          in JavaScript.encodeUtf8 s
                          end
  | fmt StringCvt.HEX x = let val s = if x >= 0 then
                                          JavaScript.unsafeFromValue (JavaScript.method (JavaScript.fromInt x, "toString") #[JavaScript.fromInt 16])
                                      else
                                          WideString.^ ("~", JavaScript.unsafeFromValue (JavaScript.method (JavaScript.negate (JavaScript.fromInt x), "toString") #[JavaScript.fromInt 16]))
                              val s = JavaScript.method (JavaScript.fromWideString s, "toUpperCase") #[]
                          in JavaScript.encodeUtf8 (JavaScript.unsafeFromValue s : WideString.string)
                          end
fun toString (x : int) : string = fmt StringCvt.DEC x
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
val wordSize : int = 32
(* toLarge, toLargeX, toLargeWord, toLargeWordX, fromLarge, fromLargeWord, toLargeInt, toLargeIntX, fromLargeInt *)
val toInt : word -> int = fn x => if x >= 0wx80000000 then
                                      raise Overflow
                                  else
                                      Unsafe.cast x
val toIntX : word -> int = fn x => JavaScript.toInt32 (JavaScript.fromWord x)
val fromInt : int -> word = fn x => JavaScript.toUint32 (JavaScript.fromInt x)
val andb : word * word -> word = fn (x, y) => JavaScript.toUint32 (JavaScript.andb (JavaScript.fromWord x, JavaScript.fromWord y))
val orb : word * word -> word = fn (x, y) => JavaScript.toUint32 (JavaScript.orb (JavaScript.fromWord x, JavaScript.fromWord y))
val xorb : word * word -> word = fn (x, y) => JavaScript.toUint32 (JavaScript.xorb (JavaScript.fromWord x, JavaScript.fromWord y))
val notb : word -> word = fn x => JavaScript.toUint32 (JavaScript.notb (JavaScript.fromWord x))
val << : word * word -> word = fn (x, y) => if y >= 0w32 then
                                                0w0
                                            else
                                                JavaScript.toUint32 (JavaScript.<< (JavaScript.fromWord x, JavaScript.fromWord y))
val >> : word * word -> word = fn (x, y) => if y >= 0w32 then
                                                0w0
                                            else
                                                JavaScript.unsafeFromValue (JavaScript.>>> (JavaScript.fromWord x, JavaScript.fromWord y))
val ~>> : word * word -> word = fn (x, y) => if y >= 0w31 then
                                                 if x >= 0wx80000000 then
                                                     0wxFFFFFFFF
                                                 else
                                                     0w0
                                             else
                                                 JavaScript.toUint32 (JavaScript.>> (JavaScript.fromWord x, JavaScript.fromWord y))
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
fun fmt StringCvt.BIN x = let val s = JavaScript.unsafeFromValue (JavaScript.method (JavaScript.fromWord x, "toString") #[JavaScript.fromInt 2])
                          in JavaScript.encodeUtf8 s
                          end
  | fmt StringCvt.OCT x = let val s = JavaScript.unsafeFromValue (JavaScript.method (JavaScript.fromWord x, "toString") #[JavaScript.fromInt 8])
                          in JavaScript.encodeUtf8 s
                          end
  | fmt StringCvt.DEC x = let val s = JavaScript.unsafeFromValue (JavaScript.method (JavaScript.fromWord x, "toString") #[])
                          in JavaScript.encodeUtf8 s
                          end
  | fmt StringCvt.HEX x = let val s = JavaScript.unsafeFromValue (JavaScript.method (JavaScript.fromWord x, "toString") #[JavaScript.fromInt 16])
                              val s = JavaScript.method (JavaScript.fromWideString s, "toUpperCase") #[]
                          in JavaScript.encodeUtf8 (JavaScript.unsafeFromValue s : WideString.string)
                          end
fun toString (x : word) : string = fmt StringCvt.HEX x
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
val posInf = JavaScript.unsafeFromValue JavaScript.Lib.Number.POSITIVE_INFINITY : real
val negInf = JavaScript.unsafeFromValue JavaScript.Lib.Number.NEGATIVE_INFINITY : real
fun == (x, y) = JavaScript.=== (JavaScript.fromReal x, JavaScript.fromReal y)
fun != (x, y) = JavaScript.!== (JavaScript.fromReal x, JavaScript.fromReal y)
infix 4 == !=
fun isNan x = JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.Number.isNaN #[JavaScript.fromReal x]) : bool
fun ?= (x, y) = x == y orelse isNan x orelse isNan y (* EQUAL or UNORDERED *)
fun unordered (x, y) = isNan x orelse isNan y
fun isFinite x = JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.Number.isFinite #[JavaScript.fromReal x])
val maxFinite = JavaScript.unsafeFromValue JavaScript.Lib.Number.MAX_VALUE : real (* 0x1.fffffffffffffp1023; assuming binary64 *)
val minPos = JavaScript.unsafeFromValue JavaScript.Lib.Number.MIN_VALUE : real (* 0x1p-1074; assuming binary64 *)
val minNormalPos = 0x1p~1022 : real (* 0x1p-1022; assuming binary64 *)
fun isNormal x = isFinite x andalso minNormalPos <= abs x
fun class x = if x == 0.0 then
                  IEEEReal.ZERO
              else
                  if isFinite x then
                      (* normal or subnormal *)
                      if minNormalPos <= abs x then
                          IEEEReal.NORMAL
                      else
                          IEEEReal.SUBNORMAL
                  else
                      (* infinity or NaN *)
                      if isNan x then
                          IEEEReal.NAN
                      else
                          IEEEReal.INF
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
fun realFloor x = JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.Math.floor #[JavaScript.fromReal x]) : real
fun realCeil x = JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.Math.ceil #[JavaScript.fromReal x]) : real
fun realTrunc x = JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.Math.trunc #[JavaScript.fromReal x]) : real
(* round to nearest even; JavaScript's Math.round breaks ties by preferring the Number closer to +inf *)
fun realRound x = let val intPart = JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.Math.round #[JavaScript.fromReal x]) : real
                      val intPartIsEven = JavaScript.=== (JavaScript.% (JavaScript.fromReal intPart, JavaScript.fromReal 2.0), JavaScript.fromReal 0.0)
                      val fracPart = JavaScript.unsafeFromValue (JavaScript.% (JavaScript.fromReal x, JavaScript.fromReal 1.0)) : real
                  in if (fracPart == 0.5 orelse fracPart == ~0.5) andalso not intPartIsEven then
                         intPart - 1.0
                     else
                         intPart
                  end
fun resultToInt x = if isNan x then
                        raise Domain
                    else if x < ~0x80000000p0 orelse x > 0x7fffffffp0 then
                        raise Overflow
                    else
                        JavaScript.toInt32 (JavaScript.fromReal x)
fun floor x = resultToInt (realFloor x)
fun ceil x = resultToInt (realCeil x)
fun trunc x = resultToInt (realTrunc x)
fun round x = resultToInt (realRound x)
fun toInt IEEEReal.TO_NEGINF = floor
  | toInt IEEEReal.TO_POSINF = ceil
  | toInt IEEEReal.TO_ZERO = trunc
  | toInt IEEEReal.TO_NEAREST = round
fun fromInt (x : int) : real = Unsafe.cast x
fun fmt (StringCvt.SCI prec) r = let val prec = Option.getOpt (prec, 6)
                                     val () = if prec < 0 then
                                                  raise Size
                                              else
                                                  ()
                                     val result = JavaScript.method (JavaScript.fromReal r, "toExponential") #[JavaScript.fromInt prec] (* TODO: Is this OK? *)
                                     val result = JavaScript.method (result, "replaceAll") #[JavaScript.fromWideString "-", JavaScript.fromWideString "~"]
                                 in JavaScript.encodeUtf8 (JavaScript.unsafeFromValue result : WideString.string)
                                 end
  | fmt (StringCvt.FIX prec) r = let val prec = Option.getOpt (prec, 6)
                                     val () = if prec < 0 then
                                                  raise Size
                                              else
                                                  ()
                                     val result = JavaScript.method (JavaScript.fromReal r, "toFixed") #[JavaScript.fromInt prec] (* TODO: Is this OK? *)
                                     val result = JavaScript.method (result, "replaceAll") #[JavaScript.fromWideString "-", JavaScript.fromWideString "~"]
                                 in JavaScript.encodeUtf8 (JavaScript.unsafeFromValue result : WideString.string)
                                 end
  | fmt (StringCvt.GEN prec) r = let val prec = Option.getOpt (prec, 12)
                                     val () = if prec < 1 then
                                                  raise Size
                                              else
                                                  ()
                                     val result = JavaScript.method (JavaScript.fromReal r, "toPrecision") #[JavaScript.fromInt prec] (* TODO: Is this OK? *)
                                     val result = JavaScript.method (result, "replaceAll") #[JavaScript.fromWideString "-", JavaScript.fromWideString "~"]
                                     val result = JavaScript.method (result, "toUpperCase") #[]
                                 in JavaScript.encodeUtf8 (JavaScript.unsafeFromValue result : WideString.string)
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
val pi : real = LunarML.assumeDiscardable (JavaScript.unsafeFromValue JavaScript.Lib.Math.PI : real)
val sqrt : real -> real = LunarML.assumeDiscardable (JavaScript.unsafeFromValue JavaScript.Lib.Math.sqrt)
val sin : real -> real = LunarML.assumeDiscardable (JavaScript.unsafeFromValue JavaScript.Lib.Math.sin)
val cos : real -> real = LunarML.assumeDiscardable (JavaScript.unsafeFromValue JavaScript.Lib.Math.cos)
val tan : real -> real = LunarML.assumeDiscardable (JavaScript.unsafeFromValue JavaScript.Lib.Math.tan)
val asin : real -> real = LunarML.assumeDiscardable (JavaScript.unsafeFromValue JavaScript.Lib.Math.asin)
val acos : real -> real = LunarML.assumeDiscardable (JavaScript.unsafeFromValue JavaScript.Lib.Math.acos)
val atan : real -> real = LunarML.assumeDiscardable (JavaScript.unsafeFromValue JavaScript.Lib.Math.atan)
val atan2 : real * real -> real = fn (y, x) => JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.Math.atan2 #[JavaScript.fromReal y, JavaScript.fromReal x])
val exp : real -> real = LunarML.assumeDiscardable (JavaScript.unsafeFromValue JavaScript.Lib.Math.exp)
val e = LunarML.assumeDiscardable (JavaScript.unsafeFromValue JavaScript.Lib.Math.E : real)
val pow : real * real -> real = fn (x, y) => JavaScript.unsafeFromValue (JavaScript.call JavaScript.Lib.Math.pow #[JavaScript.fromReal x, JavaScript.fromReal y])
val ln : real -> real = LunarML.assumeDiscardable (JavaScript.unsafeFromValue JavaScript.Lib.Math.log)
val log10 : real -> real = LunarML.assumeDiscardable (JavaScript.unsafeFromValue JavaScript.Lib.Math.log10)
val sinh : real -> real = LunarML.assumeDiscardable (JavaScript.unsafeFromValue JavaScript.Lib.Math.sinh)
val cosh : real -> real = LunarML.assumeDiscardable (JavaScript.unsafeFromValue JavaScript.Lib.Math.cosh)
val tanh : real -> real = LunarML.assumeDiscardable (JavaScript.unsafeFromValue JavaScript.Lib.Math.tanh)
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
                                           JavaScript.unsafeFromValue (JavaScript.sub (JavaScript.unsafeToValue s, JavaScript.fromInt i)) : char
fun substring (s : string, i : int, j : int) : string = if i < 0 orelse j < 0 orelse size s < i + j then
                                                            raise Subscript
                                                        else
                                                            JavaScript.unsafeFromValue (JavaScript.method (JavaScript.unsafeToValue s, "subarray") #[JavaScript.fromInt i, JavaScript.fromInt (i + j)])
fun extract (s : string, i : int, NONE : int option) : string = if i < 0 orelse size s < i then
                                                                    raise Subscript
                                                                else
                                                                    JavaScript.unsafeFromValue (JavaScript.method (JavaScript.unsafeToValue s, "subarray") #[JavaScript.fromInt i, JavaScript.fromInt (size s)])
  | extract (s, i, SOME j) = substring (s, i, j)
val concat : string list -> string = _Prim.String.concat
fun concatWith (s : string) (l : string list) : string = _primCall "call2" (_Prim.String.concatWith, s, l)
val implode : char list -> string = _Prim.String.implode
fun explode (s : string) : char list = Vector.foldr (op ::) [] (Vector.tabulate (size s, fn i => sub (s, i)))
fun map (f : char -> char) (s : string) : string = let val s = JavaScript.unsafeToValue s
                                                   in JavaScript.unsafeFromValue (JavaScript.method (s, "map") #[JavaScript.unsafeToValue f])
                                                   end
fun translate (f : char -> string) (s : string) : string = _primCall "call2" (_Prim.String.translate, f, s)
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
val op ^ : string * string -> string = String.^;

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

structure Substring :> sig
              type substring
              type char = char
              type string = string
              val sub : substring * int -> char
              val size : substring -> int
              val base : substring -> string * int * int
              val full : string -> substring
              val string : substring -> string
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
fun string (s, i, z) = String.substring (s, i, z)
fun getc (s, i, z) = if z = 0 then
                         NONE
                     else
                         SOME (String.sub (s, i), (s, i + 1, z - 1))
end;

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
val ord : char -> int = Unsafe.cast
val chr : int -> char = fn x => if x < 0 orelse x > 255 then
                                    raise Chr
                                else
                                    Unsafe.cast x : char
fun succ c = chr (ord c + 1)
fun pred c = chr (ord c - 1)
fun compare (x : char, y : char) = if x = y then
                                       EQUAL
                                   else if x < y then
                                       LESS
                                   else
                                       GREATER
fun contains (s : string) (c : char) : bool = JavaScript.unsafeFromValue (JavaScript.method (JavaScript.unsafeToValue s, "includes") #[JavaScript.unsafeToValue c])
fun notContains s c = not (contains s c)
fun isAscii (c : char) = c <= #"\127"
fun isUpper (c : char) = #"A" <= c andalso c <= #"Z"
fun isLower (c : char) = #"a" <= c andalso c <= #"z"
fun isDigit (c : char) = #"0" <= c andalso c <= #"9"
fun isAlpha (c : char) = isUpper c orelse isLower c
fun isAlphaNum (c : char) = isAlpha c orelse isDigit c
fun isHexDigit (c : char) = isDigit c orelse (#"a" <= c andalso c <= #"f") orelse (#"A" <= c andalso c <= #"Z")
fun isGraph (c : char) = #"!" <= c andalso c <= #"~"
fun isPrint (c : char) = isGraph c orelse c = #" "
fun isPunct (c : char) = isGraph c andalso not (isAlphaNum c)
fun isCntrl (c : char) = isAscii c andalso not (isPrint c)
fun isSpace (c : char) = (#"\t" <= c andalso c <= #"\r") orelse c = #" "
fun toLower (c : char) = if isUpper c then
                             chr (ord c - (ord #"A" - ord #"a"))
                         else
                             c
fun toUpper (c : char) = if isLower c then
                             chr (ord c - (ord #"a" - ord #"A"))
                         else
                             c
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
val maxSize = 0x7fffffff
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
val maxLen = 0x7fffffff
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
              val copyVec : { src : 'a vector, dst : 'a array, di : int } -> unit
              val appi : (int * 'a -> unit) -> 'a array -> unit
              val app : ('a -> unit) -> 'a array -> unit
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
val array = _Prim.Array.array
val fromList = _Prim.Array.fromList
val tabulate = _Prim.Array.tabulate
end; (* structure Array *)
