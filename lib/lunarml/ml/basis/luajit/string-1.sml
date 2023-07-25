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
              val isSuffix : string -> string -> bool
              val compare : string * string -> order
              val < : string * string -> bool
              val <= : string * string -> bool
              val > : string * string -> bool
              val >= : string * string -> bool
              val implodeRev : char list -> string
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
                                               val result = Lua.call1 Lua.Lib.string.byte #[Lua.fromString s, Lua.fromInt i']
                                           in Lua.unsafeFromValue result
                                           end
fun substring (s : string, i : int, j : int) : string = if i < 0 orelse j < 0 orelse size s < i + j then
                                                          raise Subscript
                                                      else
                                                          let val result = Lua.call1 Lua.Lib.string.sub #[Lua.fromString s, Lua.fromInt (i + 1), Lua.fromInt (i + j)]
                                                          in Lua.unsafeFromValue result
                                                          end
fun extract (s : string, i : int, NONE : int option) : string = if i < 0 orelse size s < i then
                                                                    raise Subscript
                                                                else
                                                                    let val result = Lua.call1 Lua.Lib.string.sub #[Lua.fromString s, Lua.fromInt (i + 1)]
                                                                    in Lua.unsafeFromValue result
                                                                    end
  | extract (s, i, SOME j) = substring (s, i, j)
fun concat (l : string list) : string = let val result = Lua.call1 Lua.Lib.table.concat #[Lua.unsafeToValue (Vector.fromList l)]
                                        in Lua.unsafeFromValue result
                                        end
fun concatWith (s : string) (l : string list) : string = let val result = Lua.call1 Lua.Lib.table.concat #[Lua.unsafeToValue (Vector.fromList l), Lua.fromString s]
                                                         in Lua.unsafeFromValue result
                                                         end
fun implode (l : char list) : string = let val result = Lua.call1 Lua.Lib.table.concat #[Lua.unsafeToValue (Vector.map String.str (Vector.fromList l))]
                                       in Lua.unsafeFromValue result
                                       end
fun implodeRev (l : char list) : string = let val result = Lua.call1 Lua.Lib.table.concat #[Lua.unsafeToValue (Vector.map String.str (Vector.fromList (List.rev l)))]
                                          in Lua.unsafeFromValue result
                                          end
fun explode (s : string) : char list = Vector.foldr (op ::) [] (Vector.tabulate (size s, fn i => sub (s, i)))
fun map (f : char -> char) (s : string) : string = let fun g (x : string) : string = String.str (f (Lua.unsafeFromValue (Lua.call1 Lua.Lib.string.byte #[Lua.fromString x])))
                                                       val result = Lua.call1 Lua.Lib.string.gsub #[Lua.fromString s, Lua.fromString ".", Lua.unsafeToValue g]
                                                   in Lua.unsafeFromValue result
                                                   end
fun translate (f : char -> string) (s : string) : string = let fun g (x : string) : string = f (Lua.unsafeFromValue (Lua.call1 Lua.Lib.string.byte #[Lua.fromString x]))
                                                               val result = Lua.call1 Lua.Lib.string.gsub #[Lua.fromString s, Lua.fromString ".", Lua.unsafeToValue g]
                                                           in Lua.unsafeFromValue result
                                                           end
fun tokens f s = let fun go (revTokens, acc, []) = List.rev (if List.null acc then revTokens else implodeRev acc :: revTokens)
                       | go (revTokens, acc, x :: xs) = if f x then
                                                            go (if List.null acc then revTokens else implodeRev acc :: revTokens, [], xs)
                                                        else
                                                            go (revTokens, x :: acc, xs)
                 in go ([], [], explode s)
                 end
fun fields f s = let fun go (revFields, acc, []) = List.rev (implodeRev acc :: revFields)
                       | go (revFields, acc, x :: xs) = if f x then
                                                            go (implodeRev acc :: revFields, [], xs)
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
fun isSuffix suffix s = let val n = size suffix
                            val m = size s
                        in if n > m then
                               false
                           else
                               substring (s, m - n, n) = suffix
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
