# SML Basis Library

## Top-level - partial

```sml
infix  7  * / div mod
infix  6  + - ^
infixr 5  :: @
infix  4  = <> > >= < <=
infix  3  := o
infix  0  before

type unit = {}
eqtype int  (* primitive *)
eqtype word  (* primitive *)
type real  (* primitive *)
eqtype char  (* primitive *)
eqtype string  (* primitive *)
type substring = Substring.substring
type exn  (* primitive *)
eqtype 'a array  (* primitive *)
eqtype 'a vector  (* primitive *)
datatype 'a ref = ref of 'a  (* primitive *)
datatype bool = false | true
datatype option = datatype Option.option
datatype order = datatype General.order
datatype 'a list = nil | :: of 'a * 'a list

exception Bind = General.Bind
exception Chr = General.Chr
exception Div = General.Div
exception Domain = General.Domain
exception Fail = General.Fail
exception Match = General.Match
exception Overflow = General.Overflow
exception Size = General.Size
exception Span = General.Span
exception Subscript = General.Subscript
exception Empty = List.Empty
exception Option = Option.Option

val = : ''a * ''a -> bool
val <> : ''a * ''a -> bool
val abs : ∀'a:realint. 'a -> 'a  (* overloaded *)
val ~ : ∀'a:num. 'a -> 'a  (* overloaded *)
val + : ∀'a:num. 'a * 'a -> 'a  (* overloaded *)
val - : ∀'a:num. 'a * 'a -> 'a  (* overloaded *)
val * : ∀'a:num. 'a * 'a -> 'a  (* overloaded *)
val / : ∀'a:Real. 'a * 'a -> 'a  (* overloaded *)
val div : ∀'a:wordint. 'a * 'a -> 'a  (* overloaded *)
val mod : ∀'a:wordint. 'a * 'a -> 'a  (* overloaded *)
val < : ∀'a:numtxt. 'a * 'a -> bool  (* overloaded *)
val <= : ∀'a:numtxt. 'a * 'a -> bool  (* overloaded *)
val > : ∀'a:numtxt. 'a * 'a -> bool  (* overloaded *)
val >= : ∀'a:numtxt. 'a * 'a -> bool  (* overloaded *)

val ! : 'a ref -> 'a = General.!
val := : 'a ref * 'a -> unit = General.:=
val @ : ('a list * 'a list) -> 'a list = List.@
val ^ : string * string -> string = String.^
val app : ('a -> unit) -> 'a list -> unit = List.app
val before : 'a * unit -> 'a = General.before
val ceil : real -> int = Real.ceil
val chr : int -> char = Char.chr
val concat : string list -> string = String.concat
(* val exnMessage : not implemented yet *)
val exnName : exn -> string = General.exnName
val explode : string -> char list = String.explode
val floor : real -> int = Real.floor
val foldl : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b = List.foldl
val foldr : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b = List.foldr
val getOpt : 'a option * 'a -> 'a = Option.getOpt
val hd : 'a list -> 'a = List.hd
val ignore : 'a -> unit = General.ignore
val implode : char list -> string = String.implode
val isSome : 'a option -> bool = Option.isSome
val length : 'a list -> int = List.length
val map : ('a -> 'b) -> 'a list -> 'b list = List.map
val not : bool -> bool = Bool.not
val null : 'a list -> bool = List.null
val o : ('b -> 'c) * ('a -> 'b) -> 'a -> c = General.o
val ord : char -> int = Char.ord
val print : string -> unit = TextIO.print
val real : int -> real = Real.fromInt
(* val ref : defined as a constructor *)
val rev : 'a list -> 'a list = List.rev
val round : real -> int = Real.round
val size : string -> int = String.size
val str : char -> string = String.str
val substring : string * int * int -> string = String.substring
val tl : 'a list -> 'a list = List.tl
val trunc : real -> int = Real.trunc
(* val use : not supported *)
val valOf : 'a option -> 'a = Option.valOf
val vector : 'a list -> 'a vector = Vector.fromList;
```

## structure General - partial

```sml
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
  (* val exnMessage : exn -> string *)
  datatype order = LESS | EQUAL | GREATER
  val ! : 'a ref -> 'a
  val := : 'a ref * 'a -> unit
  val before : 'a * unit -> 'a
  val ignore : 'a -> unit
  val o : ('b -> 'c) * ('a -> 'b) -> 'a -> 'c
end
```

## structure StringCvt - complete

```sml
signature STRING_CVT = sig
  datatype radix = BIN | OCT | DEC | HEX
  datatype realfmt = SCI of int option
                   | FIX of int option
                   | GEN of int option
                   | EXACT
  type ('a,'b) reader = 'b -> ('a * 'b) option
  val padLeft : char -> int -> string -> string
  val padRight : char -> int -> string -> string
  val splitl : (char -> bool) -> (char, 'a) reader -> 'a -> string * 'a
  val takel : (char -> bool) -> (char, 'a) reader -> 'a -> string
  val dropl : (char -> bool) -> (char, 'a) reader -> 'a -> 'a
  val skipWS : (char, 'a) reader -> 'a -> 'a
  type cs
  val scanString : ((char, cs) reader -> ('a, cs) reader) -> string -> 'a option
end
structure StringCvt :> STRING_CVT
```

## structure Bool - complete

```sml
signature BOOL = sig
  datatype bool = datatype bool
  val not : bool -> bool
  val toString : bool -> string
  val scan : (char, 'a) StringCvt.reader -> (bool, 'a) StringCvt.reader
  val fromString : string -> bool option
end
structure Bool :> BOOL
```

## signature INTEGER (structure Int, Int8, Int16, Int32, Int54, Int64, LargeInt) / structure IntInf - complete

```sml
signature INTEGER = sig
  eqtype int
  val toLarge : int -> LargeInt.int
  val fromLarge : LargeInt.int -> int
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
  val scan : StringCvt.radix -> (char, 'a) StringCvt.reader -> (int, 'a) StringCvt.reader
  val fromString : string -> int option
end
signature INT_INF = sig
  include INTEGER
  val divMod : int * int -> int * int
  val quotRem : int * int -> int * int
  val pow : int * Int.int -> int
  val log2 : int -> Int.int
  val orb : int * int -> int
  val xorb : int * int -> int
  val andb : int * int -> int
  val notb : int -> int
  val << : int * Word.word -> int
  val ~>> : int * Word.word -> int
end
structure Int :> INTEGER where type int = int
structure Int8 :> INTEGER
structure Int16 :> INTEGER
structure Int32 :> INTEGER
structure Int54 :> INTEGER
structure Int64 :> INTEGER
structure IntInf :> INT_INF
structure LargeInt : INTEGER = IntInf
structure Position :> INTEGER
```

## signature WORD (structure Word, Word8, Word16, Word32, Word64, LargeWord) - complete

```sml
signature WORD = sig
  eqtype word
  val wordSize : int
  val toLarge : word -> LargeWord.word
  val toLargeX : word -> LargeWord.word
  val toLargeWord : word -> LargeWord.word
  val toLargeWordX : word -> LargeWord.word
  val fromLarge : LargeWord.word -> word
  val fromLargeWord : LargeWord.word -> word
  val toLargeInt : word -> LargeInt.int
  val toLargeIntX : word -> LargeInt.int
  val fromLargeInt : LargeInt.int -> word
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
  val scan : StringCvt.radix -> (char, 'a) StringCvt.reader -> (word, 'a) StringCvt.reader
  val fromString : string -> word option
end
structure Word :> WORD where type word = word
structure Word8 :> WORD
structure Word16 :> WORD
structure Word32 :> WORD
structure Word64 :> WORD
structure LargeWord = Word64
```

## structure IEEEReal - partial

```sml
signature IEEE_REAL = sig
  exception Unordered
  datatype real_order = LESS | EQUAL | GREATER | UNORDERED
  datatype float_class = NAN | INF | ZERO | NORMAL | SUBNORMAL
  datatype rounding_mode = TO_NEAREST | TO_NEGINF | TO_POSINF | TO_ZERO
  (* val setRoundingMode : rounding_mode -> unit *)
  (* val getRoundingMode : unit -> rounding_mode *)
  type decimal_approx = { class : float_class, sign : bool, digits : int list, exp : int }
  val toString : decimal_approx -> string
  (* val scan : (char, 'a) StringCvt.reader -> (decimal_approx, 'a) StringCvt.reader *)
  (* val fromString : string -> decimal_approx option *)
end
structure IEEEReal : IEEE_REAL
```

## signature REAL (structure Real, structure LargeReal) - partial

```sml
signature REAL = sig
  type real
  (* structure Math *)
  val radix : int
  val precision : int
  val maxFinite : real
  val minPos : real
  val minNormalPos : real
  val posInf : real
  val negInf : real
  val + : real * real -> real
  val - : real * real -> real
  val * : real * real -> real
  val / : real * real -> real
  val rem : real * real -> real
  (* val *+ : real * real * real -> real *)
  (* val *- : real * real * real -> real *)
  val ~ : real -> real
  val abs : real -> real
  val min : real * real -> real
  val max : real * real -> real
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
  val toManExp : real -> { man : real, exp : int }
  val fromManExp : { man : real, exp : int } -> real
  val split : real -> { whole : real, frac : real }
  val realMod : real -> real
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
  val toLargeInt : IEEEReal.rounding_mode -> real -> LargeInt.int
  val fromInt : int -> real
  val fromLargeInt : LargeInt.int -> real
  val toLarge : real -> LargeReal.real
  val fromLarge : IEEEReal.rounding_mode -> LargeReal.real -> real
  val fmt : StringCvt.realfmt -> real -> string
  val toString : real -> string
  val scan : (char, 'a) StringCvt.reader -> (real, 'a) StringCvt.reader
  val fromString : string -> real option
  (* val toDecimal : real -> IEEEReal.decimal_approx *)
  (* val fromDecimal : IEEEReal.decimal_approx -> real option *)
end
structure Real : REAL where type real = real
structure LargeReal = Real
```

## structure Math - complete

```sml
signature MATH = sig
  type real
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
end
structure Math :> MATH where type real = Real.real
```

## signature CHAR (structure Char, WideChar) - complete

```sml
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
  val scan : (Char.char, 'a) StringCvt.reader -> (char, 'a) StringCvt.reader
  val fromString : String.string -> char option
  val toCString : char -> String.string
  val fromCString : String.string -> char option
end
structure Char :> CHAR where type char = char where type string = String.string
structure WideChar :> CHAR where type string = WideString.string
```

On Lua backend, `WideChar.maxOrd` is 255 and `WideChar` is an opaque alias of `Char`.

On JavaScript backend, `WideChar.maxOrd` is 65535.

## signature STRING (structure String, WideString) - partial

```sml
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
  val isSuffix : string -> string -> bool
  val compare : string * string -> order
  (* val collate : (char * char -> order) -> string * string -> order *)
  val < : string * string -> bool
  val <= : string * string -> bool
  val > : string * string -> bool
  val >= : string * string -> bool
  val toString : string -> String.string
  val scan : (Char.char, 'a) StringCvt.reader -> (string, 'a) StringCvt.reader
  val fromString : String.string -> string option
  val toCString : string -> String.string
  val fromCString : String.string -> string option

  (* https://github.com/SMLFamily/BasisLibrary/wiki/2015-003d-STRING *)
  val implodeRev : char list -> string
end
structure String :> STRING where type string = string where type char = char
structure WideString :> STRING where type char = WideChar.char
```

## signature SUBSTRING (structure Substring, WideSubstring) - partial

```sml
signature SUBSTRING = sig
  type substring
  type char
  type string
  val sub : substring * int -> char
  val size : substring -> int
  val base : substring -> string * int * int
  val extract : string * int * int option -> substring
  val substring : string * int * int -> substring
  val full : string -> substring
  val string : substring -> string
  val isEmpty : substring -> bool
  val getc : substring -> (char * substring) option
  val first : substring -> char option
  val triml : int -> substring -> substring
  val trimr : int -> substring -> substring
  val slice : substring * int * int option -> substring
  val concat : substring list -> string
  val concatWith : string -> substring list -> string
  val explode : substring -> char list
  val isPrefix : string -> substring -> bool
  (* val isSubstring : string -> substring -> bool *)
  val isSuffix : string -> substring -> bool
  val compare : substring * substring -> order
  val collate : (char * char -> order) -> substring * substring -> order
  val splitl : (char -> bool) -> substring -> substring * substring
  val splitr : (char -> bool) -> substring -> substring * substring
  val splitAt : substring * int -> substring * substring
  val dropl : (char -> bool) -> substring -> substring
  val dropr : (char -> bool) -> substring -> substring
  val takel : (char -> bool) -> substring -> substring
  val taker : (char -> bool) -> substring -> substring
  (* val position : string -> substring -> substring * substring *)
  (* val span : substring * substring -> substring *)
  val translate : (char -> string) -> substring -> string
  val tokens : (char -> bool) -> substring -> substring list
  val fields : (char -> bool) -> substring -> substring list
  val app : (char -> unit) -> substring -> unit
  val foldl : (char * 'a -> 'a) -> 'a -> substring -> 'a
  val foldr : (char * 'a -> 'a) -> 'a -> substring -> 'a
end
structure Substring :> SUBSTRING where type substring = CharVectorSlice.slice
                                 where type string = String.string
                                 where type char = Char.char
structure WideSubstring :> SUBSTRING where type substring = WideCharVectorSlice.slice
                                     where type string = WideString.string
                                     where type char = WideChar.char
```

## signature TEXT (structure Text, WideText) - complete

```sml
signature TEXT = sig
  structure Char : CHAR
  structure String : STRING
  structure Substring : SUBSTRING
  structure CharVector : MONO_VECTOR
  structure CharArray : MONO_ARRAY
  structure CharVectorSlice : MONO_VECTOR_SLICE
  structure CharArraySlice : MONO_ARRAY_SLICE
  sharing type Char.char = String.char = Substring.char
                         = CharVector.elem = CharArray.elem
                         = CharVectorSlice.elem = CharArraySlice.elem
  sharing type Char.string = String.string = Substring.string
                           = CharVector.vector = CharArray.vector
                           = CharVectorSlice.vector = CharArraySlice.vector
  sharing type CharArray.array = CharArraySlice.array
  sharing type CharVectorSlice.slice = CharArraySlice.vector_slice
end

structure Text :> TEXT where type Char.char = Char.char
                       where type String.string = String.string
                       where type Substring.substring = Substring.substring
                       where type CharArray.array = CharArray.array
                       where type CharVectorSlice.slice = CharVectorSlice.slice
                       where type CharArraySlice.slice = CharArraySlice.slice
structure WideText :> TEXT where type Char.char = WideChar.char
                           where type String.string = WideString.string
                           where type Substring.substring = WideSubstring.substring
                           where type CharArray.array = WideCharArray.array
                           where type CharVectorSlice.slice = WideCharVectorSlice.slice
                           where type CharArraySlice.slice = WideCharArraySlice.slice
```

## structure List - complete

```sml
signature LIST = sig
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
end
structure List :> LIST
```

## structure ListPair - complete

```sml
signature LIST_PAIR = sig
  exception UnequalLengths
  val zip : 'a list * 'b list -> ('a * 'b) list
  val zipEq : 'a list * 'b list -> ('a * 'b) list
  val unzip : ('a * 'b) list -> 'a list * 'b list
  val app : ('a * 'b -> unit) -> 'a list * 'b list -> unit
  val appEq : ('a * 'b -> unit) -> 'a list * 'b list -> unit
  val map : ('a * 'b -> 'c) -> 'a list * 'b list -> 'c list
  val mapEq : ('a * 'b -> 'c) -> 'a list * 'b list -> 'c list
  val foldl : ('a * 'b * 'c -> 'c) -> 'c -> 'a list * 'b list -> 'c
  val foldr : ('a * 'b * 'c -> 'c) -> 'c -> 'a list * 'b list -> 'c
  val foldlEq : ('a * 'b * 'c -> 'c) -> 'c -> 'a list * 'b list -> 'c
  val foldrEq : ('a * 'b * 'c -> 'c) -> 'c -> 'a list * 'b list -> 'c
  val all : ('a * 'b -> bool) -> 'a list * 'b list -> bool
  val exists : ('a * 'b -> bool) -> 'a list * 'b list -> bool
  val allEq : ('a * 'b -> bool) -> 'a list * 'b list -> bool
end
structure ListPair :> LIST_PAIR
```

## structure Option - complete

```sml
signature OPTION = sig
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
end
structure Option :> OPTION
```

## structure Vector - complete

```sml
signature VECTOR = sig
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
end
structure Vector :> VECTOR
```

## structure VectorSlice - complete

```sml
signature VECTOR_SLICE = sig
  type 'a slice
  val length : 'a slice -> int
  val sub : 'a slice * int -> 'a
  val full : 'a Vector.vector -> 'a slice
  val slice : 'a Vector.vector * int * int option -> 'a slice
  val subslice : 'a slice * int * int option -> 'a slice
  val base : 'a slice -> 'a Vector.vector * int * int
  val vector : 'a slice -> 'a Vector.vector
  val concat : 'a slice list -> 'a Vector.vector
  val isEmpty : 'a slice -> bool
  val getItem : 'a slice -> ('a * 'a slice) option
  val appi : (int * 'a -> unit) -> 'a slice -> unit
  val app : ('a -> unit) -> 'a slice -> unit
  val mapi : (int * 'a -> 'b) -> 'a slice -> 'b Vector.vector
  val map : ('a -> 'b) -> 'a slice -> 'b Vector.vector
  val foldli : (int * 'a * 'b -> 'b) -> 'b -> 'a slice -> 'b
  val foldri : (int * 'a * 'b -> 'b) -> 'b -> 'a slice -> 'b
  val foldl : ('a * 'b -> 'b) -> 'b -> 'a slice -> 'b
  val foldr : ('a * 'b -> 'b) -> 'b -> 'a slice -> 'b
  val findi : (int * 'a -> bool) -> 'a slice -> (int * 'a) option
  val find : ('a -> bool) -> 'a slice -> 'a option
  val exists : ('a -> bool) -> 'a slice -> bool
  val all : ('a -> bool) -> 'a slice -> bool
  val collate : ('a * 'a -> order) -> 'a slice * 'a slice -> order
end
structure VectorSlice :> VECTOR_SLICE
```

## structure Array - complete

```sml
signature ARRAY = sig
  datatype array = datatype array
  datatype vector = datatype vector
  val maxLen : int
  val array : int * 'a -> 'a array
  val fromList : 'a list -> 'a array
  val tabulate : int * (int -> 'a) -> 'a array
  val length : 'a array -> int
  val sub : 'a array * int -> 'a
  val update : 'a array * int * 'a -> unit
  val vector : 'a array -> 'a vector
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
  val findi : (int * 'a -> bool) -> 'a array -> (int * 'a) option
  val find : ('a -> bool) -> 'a array -> 'a option
  val exists : ('a -> bool) -> 'a array -> bool
  val all : ('a -> bool) -> 'a array -> bool
  val collate : ('a * 'a -> order) -> 'a array * 'a array -> order

  (* https://github.com/SMLFamily/BasisLibrary/wiki/2015-003g-Array *)
  val toList : 'a array -> 'a list
  val fromVector : 'a vector -> 'a array
  val toVector : 'a array -> 'a vector
end
structure Array :> ARRAY
```

## structure ArraySlice - complete

```sml
signature ARRAY_SLICE = sig
  type 'a slice
  val length : 'a slice -> int
  val sub : 'a slice * int -> 'a
  val update : 'a slice * int * 'a -> unit
  val full : 'a Array.array -> 'a slice
  val slice : 'a Array.array * int * int option -> 'a slice
  val subslice : 'a slice * int * int option -> 'a slice
  val base : 'a slice -> 'a Array.array * int * int
  val vector : 'a slice -> 'a Vector.vector
  val copy : { src : 'a slice, dst : 'a Array.array, di : int } -> unit
  val copyVec : { src : 'a VectorSlice.slice, dst : 'a Array.array, di : int } -> unit
  val isEmpty : 'a slice -> bool
  val getItem : 'a slice -> ('a * 'a slice) option
  val appi : (int * 'a -> unit) -> 'a slice -> unit
  val app : ('a -> unit) -> 'a slice -> unit
  val modifyi : (int * 'a -> 'a) -> 'a slice -> unit
  val modify : ('a -> 'a) -> 'a slice -> unit
  val foldli : (int * 'a * 'b -> 'b) -> 'b -> 'a slice -> 'b
  val foldri : (int * 'a * 'b -> 'b) -> 'b -> 'a slice -> 'b
  val foldl : ('a * 'b -> 'b) -> 'b -> 'a slice -> 'b
  val foldr : ('a * 'b -> 'b) -> 'b -> 'a slice -> 'b
  val findi : (int * 'a -> bool) -> 'a slice -> (int * 'a) option
  val find : ('a -> bool) -> 'a slice -> 'a option
  val exists : ('a -> bool) -> 'a slice -> bool
  val all : ('a -> bool) -> 'a slice -> bool
  val collate : ('a * 'a -> order) -> 'a slice * 'a slice -> order
end
structure ArraySlice :> ARRAY_SLICE
```

## signature MONO_VECTOR (structure \{Char,WideChar,Bool,Int,Int8,Int16,Int32,Int64,Word,Word8,Word16,Word32,Word64,Real\}Vector) - complete

```sml
signature MONO_VECTOR = sig
  type vector
  type elem
  val maxLen : int
  val fromList : elem list -> vector
  val tabulate : int * (int -> elem) -> vector
  val length : vector -> int
  val sub : vector * int -> elem
  val update : vector * int * elem -> vector
  val concat : vector list -> vector
  val appi : (int * elem -> unit) -> vector -> unit
  val app : (elem -> unit) -> vector -> unit
  val mapi : (int * elem -> elem) -> vector -> vector
  val map : (elem -> elem) -> vector -> vector
  val foldli : (int * elem * 'a -> 'a) -> 'a -> vector -> 'a
  val foldri : (int * elem * 'a -> 'a) -> 'a -> vector -> 'a
  val foldl : (elem * 'a -> 'a) -> 'a -> vector -> 'a
  val foldr : (elem * 'a -> 'a) -> 'a -> vector -> 'a
  val findi : (int * elem -> bool) -> vector -> (int * elem) option
  val find : (elem -> bool) -> vector -> elem option
  val exists : (elem -> bool) -> vector -> bool
  val all : (elem -> bool) -> vector -> bool
  val collate : (elem * elem -> order) -> vector * vector -> order

  (* https://github.com/SMLFamily/BasisLibrary/wiki/2015-003f-MONO_VECTOR *)
  val toList : vector -> elem list
  val append : vector * elem -> vector
  val prepend : elem * vector -> vector
end
structure CharVector :> MONO_VECTOR where type vector = String.string
                                    where type elem = char
structure WideCharVector :> MONO_VECTOR where type vector = WideString.string
                                        where type elem = WideChar.char
structure BoolVector :> MONO_VECTOR where type elem = bool
structure IntVector :> MONO_VECTOR where type elem = Int.int
structure Int8Vector :> MONO_VECTOR where type elem = Int8.int
structure Int16Vector :> MONO_VECTOR where type elem = Int16.int
structure Int32Vector :> MONO_VECTOR where type elem = Int32.int
structure Int64Vector :> MONO_VECTOR where type elem = Int64.int
structure WordVector :> MONO_VECTOR where type elem = Word.word
structure Word8Vector :> MONO_VECTOR where type elem = Word8.word
structure Word16Vector :> MONO_VECTOR where type elem = Word16.word
structure Word32Vector :> MONO_VECTOR where type elem = Word32.word
structure Word64Vector :> MONO_VECTOR where type elem = Word64.word
structure RealVector :> MONO_VECTOR where type elem = real
```

## signature MONO_VECTOR_SLICE (structure \{Char,WideChar,Bool,Int,Int8,Int16,Int32,Int64,Word,Word8,Word16,Word32,Word64,Real\}VectorSlice) - complete

```sml
signature MONO_VECTOR_SLICE = sig
  type elem
  type vector
  type slice
  val length : slice -> int
  val sub : slice * int -> elem
  val full : vector -> slice
  val slice : vector * int * int option -> slice
  val subslice : slice * int * int option -> slice
  val base : slice -> vector * int * int
  val vector : slice -> vector
  val concat : slice list -> vector
  val isEmpty : slice -> bool
  val getItem : slice -> (elem * slice) option
  val appi : (int * elem -> unit) -> slice -> unit
  val app : (elem -> unit) -> slice -> unit
  val mapi : (int * elem -> elem) -> slice -> vector
  val map : (elem -> elem) -> slice -> vector
  val foldli : (int * elem * 'b -> 'b) -> 'b -> slice -> 'b
  val foldr : (elem * 'b -> 'b) -> 'b -> slice -> 'b
  val foldl : (elem * 'b -> 'b) -> 'b -> slice -> 'b
  val foldri : (int * elem * 'b -> 'b) -> 'b -> slice -> 'b
  val findi : (int * elem -> bool) -> slice -> (int * elem) option
  val find : (elem -> bool) -> slice -> elem option
  val exists : (elem -> bool) -> slice -> bool
  val all : (elem -> bool) -> slice -> bool
  val collate : (elem * elem -> order) -> slice * slice -> order
end
structure CharVectorSlice :> MONO_VECTOR_SLICE where type vector = CharVector.vector
                                               where type elem = char
                                               where type slice = Substring.substring
structure WideCharVectorSlice :> MONO_VECTOR_SLICE where type vector = WideCharVector.vector
                                                   where type elem = WideChar.char
                                                   where type slice = WideSubstring.substring
structure BoolVectorSlice :> MONO_VECTOR_SLICE where type vector = BoolVector.vector
                                               where type elem = bool
structure IntVectorSlice :> MONO_VECTOR_SLICE where type vector = IntVector.vector
                                              where type elem = Int.int
structure Int8VectorSlice :> MONO_VECTOR_SLICE where type vector = Int8Vector.vector
                                               where type elem = Int8.int
structure Int16VectorSlice :> MONO_VECTOR_SLICE where type vector = Int16Vector.vector
                                                where type elem = Int16.int
structure Int32VectorSlice :> MONO_VECTOR_SLICE where type vector = Int32Vector.vector
                                                where type elem = Int32.int
structure Int64VectorSlice :> MONO_VECTOR_SLICE where type vector = Int64Vector.vector
                                                where type elem = Int64.int
structure WordVectorSlice :> MONO_VECTOR_SLICE where type vector = WordVector.vector
                                               where type elem = Word.word
structure Word8VectorSlice :> MONO_VECTOR_SLICE where type vector = Word8Vector.vector
                                                where type elem = Word8.word
structure Word16VectorSlice :> MONO_VECTOR_SLICE where type vector = Word16Vector.vector
                                                 where type elem = Word16.word
structure Word32VectorSlice :> MONO_VECTOR_SLICE where type vector = Word32Vector.vector
                                                 where type elem = Word32.word
structure Word64VectorSlice :> MONO_VECTOR_SLICE where type vector = Word64Vector.vector
                                                 where type elem = Word64.word
structure RealVectorSlice :> MONO_VECTOR_SLICE where type vector = RealVector.vector
                                               where type elem = real
```

## signature MONO_ARRAY (structure \{Char,WideChar,Bool,Int,Int8,Int16,Int32,Int64,Word,Word8,Word16,Word32,Word64,Real\}Array) - complete

```sml
signature MONO_ARRAY = sig
  eqtype array
  type elem
  type vector
  val maxLen : int
  val array : int * elem -> array
  val fromList : elem list -> array
  val tabulate : int * (int -> elem) -> array
  val length : array -> int
  val sub : array * int -> elem
  val update : array * int * elem -> unit
  val vector : array -> vector
  val copy : { src : array, dst : array, di : int } -> unit
  val copyVec : { src : vector, dst : array, di : int } -> unit
  val appi : (int * elem -> unit) -> array -> unit
  val app : (elem -> unit) -> array -> unit
  val modifyi : (int * elem -> elem) -> array -> unit
  val modify : (elem -> elem) -> array -> unit
  val foldli : (int * elem * 'b -> 'b) -> 'b -> array -> 'b
  val foldri : (int * elem * 'b -> 'b) -> 'b -> array -> 'b
  val foldl : (elem * 'b -> 'b) -> 'b -> array -> 'b
  val foldr : (elem * 'b -> 'b) -> 'b -> array -> 'b
  val findi : (int * elem -> bool) -> array -> (int * elem) option
  val find : (elem -> bool) -> array -> elem option
  val exists : (elem -> bool) -> array -> bool
  val all : (elem -> bool) -> array -> bool
  val collate : (elem * elem -> order) -> array * array -> order

  (* https://github.com/SMLFamily/BasisLibrary/wiki/2015-003h-MONO_ARRAY *)
  val toList : array -> elem list
  val fromVector : vector -> array
  val toVector : array -> vector (* = vector *)
end
structure CharArray : MONO_ARRAY where type vector = CharVector.vector
                                 where type elem = char
structure WideCharArray : MONO_ARRAY where type vector = WideCharVector.vector
                                     where type elem = WideChar.char
structure BoolArray : MONO_ARRAY where type vector = BoolVector.vector
                                 where type elem = bool
structure IntArray : MONO_ARRAY where type vector = IntVector.vector
                                where type elem = Int.int
structure Int8Array : MONO_ARRAY where type vector = Int8Vector.vector
                                 where type elem = Int8.int
structure Int16Array : MONO_ARRAY where type vector = Int16Vector.vector
                                  where type elem = Int16.int
structure Int32Array : MONO_ARRAY where type vector = Int32Vector.vector
                                  where type elem = Int32.int
structure Int64Array : MONO_ARRAY where type vector = Int64Vector.vector
                                  where type elem = Int64.int
structure WordArray : MONO_ARRAY where type vector = WordVector.vector
                                 where type elem = Word.word
structure Word8Array : MONO_ARRAY where type vector = Word8Vector.vector
                                  where type elem = Word8.word
structure Word16Array : MONO_ARRAY where type vector = Word16Vector.vector
                                   where type elem = Word16.word
structure Word32Array : MONO_ARRAY where type vector = Word32Vector.vector
                                   where type elem = Word32.word
structure Word64Array : MONO_ARRAY where type vector = Word64Vector.vector
                                   where type elem = Word64.word
structure RealArray : MONO_ARRAY where type vector = RealVector.vector
                                 where type elem = real
```

## signature MONO_ARRAY_SLICE (structure \{Char,WideChar,Bool,Int,Int8,Int16,Int32,Int64,Word,Word8,Word16,Word32,Word64,Real\}ArraySlice) - complete

```sml
signature MONO_ARRAY_SLICE = sig
  type elem
  type array
  type slice
  type vector
  type vector_slice
  val length : slice -> int
  val sub : slice * int -> elem
  val update : slice * int * elem -> unit
  val full : array -> slice
  val slice : array * int * int option -> slice
  val subslice : slice * int * int option -> slice
  val base : slice -> array * int * int
  val vector : slice -> vector
  val copy : { src : slice, dst : array, di : int } -> unit
  val copyVec : { src : vector_slice, dst : array, di : int } -> unit
  val isEmpty : slice -> bool
  val getItem : slice -> (elem * slice) option
  val appi : (int * elem -> unit) -> slice -> unit
  val app : (elem -> unit) -> slice -> unit
  val modifyi : (int * elem -> elem) -> slice -> unit
  val modify : (elem -> elem) -> slice -> unit
  val foldli : (int * elem * 'b -> 'b) -> 'b -> slice -> 'b
  val foldr : (elem * 'b -> 'b) -> 'b -> slice -> 'b
  val foldl : (elem * 'b -> 'b) -> 'b -> slice -> 'b
  val foldri : (int * elem * 'b -> 'b) -> 'b -> slice -> 'b
  val findi : (int * elem -> bool) -> slice -> (int * elem) option
  val find : (elem -> bool) -> slice -> elem option
  val exists : (elem -> bool) -> slice -> bool
  val all : (elem -> bool) -> slice -> bool
  val collate : (elem * elem -> order) -> slice * slice -> order
end
structure CharArraySlice : MONO_ARRAY_SLICE where type vector = CharVector.vector
                                            where type vector_slice = CharVectorSlice.slice
                                            where type array = CharArray.array
                                            where type elem = char
structure WideCharArraySlice : MONO_ARRAY_SLICE where type vector = WideCharVector.vector
                                                where type vector_slice = WideCharVectorSlice.slice
                                                where type array = WideCharArray.array
                                                where type elem = WideChar.char
structure BoolArraySlice : MONO_ARRAY_SLICE where type vector = BoolVector.vector
                                            where type vector_slice = BoolVectorSlice.slice
                                            where type array = BoolArray.array
                                            where type elem = bool
structure IntArraySlice : MONO_ARRAY_SLICE where type vector = IntVector.vector
                                           where type vector_slice = IntVectorSlice.slice
                                           where type array = IntArray.array
                                           where type elem = Int.int
structure Int8ArraySlice : MONO_ARRAY_SLICE where type vector = Int8Vector.vector
                                            where type vector_slice = Int8VectorSlice.slice
                                            where type array = Int8Array.array
                                            where type elem = Int8.int
structure Int16ArraySlice : MONO_ARRAY_SLICE where type vector = Int16Vector.vector
                                             where type vector_slice = Int16VectorSlice.slice
                                             where type array = Int16Array.array
                                             where type elem = Int16.int
structure Int32ArraySlice : MONO_ARRAY_SLICE where type vector = Int32Vector.vector
                                             where type vector_slice = Int32VectorSlice.slice
                                             where type array = Int32Array.array
                                             where type elem = Int32.int
structure Int64ArraySlice : MONO_ARRAY_SLICE where type vector = Int64Vector.vector
                                             where type vector_slice = Int64VectorSlice.slice
                                             where type array = Int64Array.array
                                             where type elem = Int64.int
structure WordArraySlice : MONO_ARRAY_SLICE where type vector = WordVector.vector
                                             where type vector_slice = WordVectorSlice.slice
                                             where type array = WordArray.array
                                             where type elem = Word.word
structure Word8ArraySlice : MONO_ARRAY_SLICE where type vector = Word8Vector.vector
                                             where type vector_slice = Word8VectorSlice.slice
                                             where type array = Word8Array.array
                                             where type elem = Word8.word
structure Word16ArraySlice : MONO_ARRAY_SLICE where type vector = Word16Vector.vector
                                              where type vector_slice = Word16VectorSlice.slice
                                              where type array = Word16Array.array
                                              where type elem = Word16.word
structure Word32ArraySlice : MONO_ARRAY_SLICE where type vector = Word32Vector.vector
                                              where type vector_slice = Word32VectorSlice.slice
                                              where type array = Word32Array.array
                                              where type elem = Word32.word
structure Word64ArraySlice : MONO_ARRAY_SLICE where type vector = Word64Vector.vector
                                              where type vector_slice = Word64VectorSlice.slice
                                              where type array = Word64Array.array
                                              where type elem = Word64.word
structure RealArraySlice : MONO_ARRAY_SLICE where type vector = RealVector.vector
                                            where type vector_slice = RealVectorSlice.slice
                                            where type array = RealArray.array
                                            where type elem = real
```

## structure Byte - partial

```sml
signature BYTE = sig
  val byteToChar : Word8.word -> char
  val charToByte : char -> Word8.word
  val bytesToString : Word8Vector.vector -> string
  val stringToBytes : string -> Word8Vector.vector
  (* val unpackStringVec : Word8VectorSlice.slice -> string *)
  (* val unpackString : Word8ArraySlice.slice -> string *)
  (* val packString : Word8Array.array * int * substring -> unit *)
end
structure Byte :> BYTE
```

## signature PACK_WORD (structure PackWord8Big, PackWord8Little, PackWord16Big, PackWord16Little, PackWord32Big, PackWord32Little, PackWord64Big, PackWord64Little) - complete

```sml
signature PACK_WORD = sig
  val bytesPerElem : int
  val isBigEndian : bool
  val subVec : Word8Vector.vector * int -> LargeWord.word
  val subVecX : Word8Vector.vector * int -> LargeWord.word
  val subArr : Word8Array.array * int -> LargeWord.word
  val subArrX : Word8Array.array * int -> LargeWord.word
  val update : Word8Array.array * int * LargeWord.word -> unit
end
structure PackWord8Big :> PACK_WORD
structure PackWord8Little :> PACK_WORD
structure PackWord16Big :> PACK_WORD
structure PackWord16Little :> PACK_WORD
structure PackWord32Big :> PACK_WORD
structure PackWord32Little :> PACK_WORD
structure PackWord64Big :> PACK_WORD
structure PackWord64Little :> PACK_WORD
```

## structure IO - partial

```sml
structure IO : sig
  exception Io of { name : string
                  , function : string
                  , cause : exn
                  }
  (* exception BlockingNotSupported *)
  (* exception NonblockingNotSupported *)
  (* exception RandomAccessNotSupported *)
  (* exception ClosedStream *)
  (* datatype buffer_mode = NO_BUF | LINE_BUF | BLOCK_BUF *)
end
```

## structure TextIO - partial

```sml
structure TextIO : sig
  (* IMPERATIVE_IO *)
  (* structure StreamIO : STREAM_IO *)
  type vector = string
  type elem = char
  type instream
  type outstream
  val input : instream -> vector
  val input1 : instream -> elem option
  val inputN : instream * int -> vector
  val inputAll : instream -> vector
  (* val canInput : instream * int -> int option *)
  (* val lookahead : instream -> elem option *)
  val closeIn : instream -> unit
  val endOfStream : instream -> bool
  val output : outstream * vector -> unit
  val output1 : outstream * elem -> unit
  val flushOut : outstream -> unit
  val closeOut : outstream -> unit
  (* val mkInstream : StreamIO.instream -> instream *)
  (* val getInstream : instream -> StreamIO.instream *)
  (* val setInstream : instream * StreamIO.instream -> unit *)
  (* val mkOutstream : StreamIO.outstream -> outstream *)
  (* val getOutstream : outstream -> StreamIO.outstream *)
  (* val setOutstream : outstream * StreamIO.outstream -> unit *)
  (* val getPosOut : outstream -> StreamIO.out_pos *)
  (* val setPosOut : outstream * StreamIO.out_pos -> unit *)

  (* TEXT_IO *)
  (* structure StreamIO : TEXT_STREAM_IO where ... *)
  val inputLine : instream -> string option
  (* val outputSubstr : outstream * substring -> unit *)
  val openIn : string -> instream
  val openOut : string -> outstream
  val openAppend : string -> outstream
  val stdIn : instream
  val stdOut : outstream
  val stdErr : outstream
  val print : string -> unit
  (* val scanStream : ((Char.char, StreamIO.instream) StringCvt.reader -> ('a, StreamIO.instream) StringCvt.reader) -> instream -> 'a option *)
end
```

## structure OS - partial

```sml
structure OS : sig
  structure FileSys : sig
    (* type dirstream *)
    (* val openDir : string -> dirstream *)
    (* val readDir : dirstream -> string option *)
    (* val rewindDir : dirstream -> unit *)
    (* val closeDir : dirstream -> unit *)
    val chDir : string -> unit (* Lua backend: requires LuaFileSystem *)
    val getDir : unit -> string (* Lua backend: requires LuaFileSystem *)
    val mkDir : string -> unit (* Lua backend: requires LuaFileSystem *)
    val rmDir : string -> unit (* Lua backend: requires LuaFileSystem *)
    val isDir : string -> bool (* Lua backend: requires LuaFileSystem *)
    val isLink : string -> bool (* Lua backend: requires LuaFileSystem *)
    val readLink : string -> string (* Lua backend: requires LuaFileSystem 1.7.0 or later *)
    (* val fullPath : string -> string *)
    (* val realPath : string -> string *)
    (* val modTime : string -> Time.time *)
    (* val fileSize : string -> Position.int *)
    (* val setTime : string * Time.time option -> unit *)
    val remove : string -> unit
    val rename : { old : string, new : string } -> unit
    (* datatype access_mode = A_READ | A_WRITE | A_EXEC *)
    (* val access : string * access_mode list -> bool *)
    (* val tmpName : unit -> string *)
    (* eqtype file_id *)
    (* val fileId : string -> file_id *)
    (* val hash : file_id -> word *)
    (* val compare : file_id * file_id -> order *)
  end
  structure IO : sig
    (* eqtype iodesc *)
    (* val hash : iodesc -> word *)
    (* val compare : iodesc * iodesc -> order *)
    (* eqtype iodesc_kind *)
    (* val kind : iodesc -> iodesc_kind *)
    (* structure Kind *)
    (* eqtype poll_desc *)
    (* type poll_info *)
    (* val pollDesc : iodesc -> poll_desc option *)
    (* val pollToIODesc : poll_desc -> iodesc *)
    (* exception Poll *)
    (* val pollIn : poll_desc -> poll_desc *)
    (* val pollOut : poll_desc -> poll_desc *)
    (* val pollPri : polldesc -> poll_desc *)
    (* val poll : poll_desc list * Time.time option -> poll_info list *)
    (* val isIn : poll_info -> bool *)
    (* val isOut : poll_info -> bool *)
    (* val isPri : poll_info -> bool *)
    (* val infoToPollDesc : poll_info -> poll_desc *)
  end
  structure Path : sig
    (* currently Unix-style only *)
    exception Path
    exception InvalidArc
    val parentArc : string
    val currentArc : string
    val fromString : string -> { isAbs : bool, vol : string, arcs : string list }
    val toString : { isAbs : bool, vol : string, arcs : string list } -> string
    (* val validVolume : { isAbs : bool, vol : string } -> bool *)
    (* val getVolume : string -> string *)
    (* val getParent : string -> string *)
    val splitDirFile : string -> { dir : string, file : string }
    val joinDirFile : { dir : string, file : string } -> string
    val dir : string -> string
    val file : string -> string
    val splitBaseExt : string -> { base : string, ext : string option }
    val joinBaseExt : { base : string, ext : string option } -> string
    val base : string -> string
    val ext : string -> string option
    val mkCanonical : string -> string
    (* val isCanonical : string -> bool *)
    val mkAbsolute : { path : string, relativeTo : string } -> string
    val mkRelative : { path : string, relativeTo : string } -> string
    val isAbsolute : string -> bool
    val isRelative : string -> bool
    (* val isRoot : string -> bool *)
    val concat : string * string -> string
    (* val fromUnixPath : string -> string *)
    (* val toUnixPath : string -> string *)
  end
  structure Process : sig
    type status
    val success : status
    val failure : status
    val isSuccess : status -> bool
    val system : string -> status
    (* val atExit : (unit -> unit) -> unit *)
    val exit : status -> 'a
    val terminate : status -> 'a
    val getEnv : string -> string option
    (* val sleep : Time.time -> unit *)
  end
  eqtype syserror
  exception SysErr of string * syserror option
  (* val errorMsg : syserror -> string *)
  (* val errorName : syserror -> string *)
  (* val syserror : string -> syserror option *)
end
```

## structure CommandLine - complete

```sml
structure CommandLine : sig
  val name : unit -> string
  val arguments : unit -> string list
end
```

## structure Time - partial

```sml
signature TIME = sig
  eqtype time
  exception Time
  val zeroTime : time
  val fromReal : LargeReal.real -> time
  val toReal : time -> LargeReal.real
  val toSeconds : time -> LargeInt.int
  val toMilliseconds : time -> LargeInt.int
  val toMicroseconds : time -> LargeInt.int
  val toNanoseconds : time -> LargeInt.int
  val fromSeconds : LargeInt.int -> time
  val fromMilliseconds : LargeInt.int -> time
  val fromMicroseconds : LargeInt.int -> time
  val fromNanoseconds : LargeInt.int -> time
  val + : time * time -> time
  val - : time * time -> time
  val compare : time * time -> order
  val < : time * time -> bool
  val <= : time * time -> bool
  val > : time * time -> bool
  val >= : time * time -> bool
  val now : unit -> time
  val fmt : int -> time -> string
  val toString : time -> string
  (* val scan : (char, 'a) StringCvt.reader -> (time, 'a) StringCvt.reader *)
  (* val fromString : string -> time option *)
end
structure Time :> TIME
```

## structure Date - partial

```sml
signature DATE = sig
  datatype weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun
  datatype month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
  type date
  exception Date
  val date : { year : int, month : month, day : int, hour : int, minute : int, second : int, offset : Time.time option } -> date
  val year : date -> int
  val month : date -> month
  val day : date -> int
  val hour : date -> int
  val minute : date -> int
  val second : date -> int
  val weekDay : date -> weekday
  val offset : date -> Time.time option
  val isDst : date -> bool option
  val localOffset : unit -> Time.time
  val fromTimeLocal : Time.time -> date
  val fromTimeUniv : Time.time -> date
  val toTime : date -> Time.time
  val compare : date * date -> order
  val fmt : string -> date -> string
  val toString : date -> string
  (* val scan : (char, 'a) StringCvt.reader -> (date, 'a) StringCvt.reader *)
  (* val fromString : string -> date option *)
end
structure Date :> DATE
```

On Lua backend, `Date.fmt` is a thin wrapper of `os.date`. Therefore, LunarML's `Date.fmt` may accept or reject invalid specifiers.

On JavaScript backend, `Date.fmt` tries to mimick C locale.

## structure Timer - complete

```sml
signature TIMER = sig
  type cpu_timer
  type real_timer
  val startCPUTimer : unit -> cpu_timer
  val checkCPUTimes : cpu_timer -> { nongc : { usr : Time.time, sys : Time.time }
                                   , gc : { usr : Time.time, sys : Time.time }
                                   }
  val checkCPUTimer : cpu_timer -> { usr : Time.time, sys : Time.time }
  val checkGCTime : cpu_timer -> Time.time
  val totalCPUTimer : unit -> cpu_timer
  val startRealTimer : unit -> real_timer
  val checkRealTimer : real_timer -> Time.time
  val totalRealTimer : unit -> real_timer
end
structure Timer :> TIMER
```

The GC time returned by this structure is always zero.

## Not implemented yet

```sml
signature BIN_IO
structure BinIO :> BIN_IO
signature IMPERATIVE_IO
signature PRIM_IO
structure BinPrimIO :> PRIM_IO where ...
structure TextPrimIO :> PRIM_IO where ...
signature STREAM_IO
signature TEXT_IO
structure WideTextIO :> TEXT_IO
signature TEXT_STREAM_IO
```
