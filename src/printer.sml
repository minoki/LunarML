(*
 * Copyright (c) 2023 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure Printer :> sig
              datatype fragment = Fragment of string
                                | IncreaseIndent of int
                                | DecreaseIndent of int
                                | Indent
                     | LineTerminator
              val showParen : bool -> fragment list -> fragment list
              val build : fragment list -> string
              val sepBy : fragment list -> fragment list list -> fragment list
              val commaSep : fragment list list -> fragment list
              val commaSepV : fragment list vector -> fragment list
              val spaceSep : fragment list list -> fragment list
              val semicolonSep : fragment list list -> fragment list
          end = struct
datatype fragment = Fragment of string
                  | IncreaseIndent of int
                  | DecreaseIndent of int
                  | Indent
                  | LineTerminator
fun showParen true x = Fragment "(" :: x @ [ Fragment ")" ]
  | showParen false x = x
fun processIndent (indent, []) = []
  | processIndent (indent, Fragment s :: xs) = s :: processIndent (indent, xs)
  | processIndent (indent, IncreaseIndent n :: xs) = processIndent (indent + n, xs)
  | processIndent (indent, DecreaseIndent n :: xs) = processIndent (indent - n, xs)
  | processIndent (indent, Indent :: xs) = CharVector.tabulate (indent, fn _ => #" ") :: processIndent (indent, xs)
  | processIndent (indent, LineTerminator :: xs) = "\n" :: processIndent (indent, xs)
fun build xs = String.concat (processIndent (0, xs))
fun sepBy sep [] = []
  | sepBy sep [x] = x
  | sepBy sep (x :: xs) = x @ sep @ sepBy sep xs
val commaSep = sepBy [Fragment ", "]
fun commaSepV xs = commaSep (Vector.foldr (op ::) [] xs)
val spaceSep = sepBy [Fragment " "]
val semicolonSep = sepBy [Fragment "; "]
end
