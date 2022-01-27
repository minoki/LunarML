structure MLBSyntax = struct
type Path = string
type BasId = string
datatype BasDec = BasisDec of (BasId * BasExp) list
                | OpenDec of BasId list
                | LocalDec of BasDec list * BasDec list
                | StructureDec of (Syntax.StrId * Syntax.StrId) list
                | SignatureDec of (Syntax.SigId * Syntax.SigId) list
                | FunctorDec of (Syntax.FunId * Syntax.FunId) list
                | PathDec of Path (* path.sml, path.sig, path.fun, path.mlb *)
                | AnnotationDec of string list * BasDec list
                | PrimDec
                | PrimOverloadDec
     and BasExp = BasisExp of BasDec list
                | BasIdExp of BasId
                | LetExp of BasDec list * BasExp
structure StringMap = RedBlackMapFn (open String
                                     type ord_key = string
                                    )
structure BasMap = StringMap
(*
 * built-in path map:
 *   * SML_LIB
 *)
fun evalPath pathMap s
    = let fun go (s, acc) = case Substring.getc s of
                                NONE => String.concat (List.rev acc)
                              | SOME (#"/", s) => go (s, "/" :: acc)
                              | SOME (#"$", s) => (case Substring.getc s of
                                                       SOME (#"(", s) =>
                                                       let val (name, s) = Substring.splitl (fn c => c <> #")") s
                                                           val name = Substring.string name
                                                           val s = if Substring.isEmpty s then
                                                                       raise Fail ("unclosed path variable: " ^ name)
                                                                   else
                                                                       Substring.triml 1 s
                                                       in case StringMap.find (pathMap, name) of
                                                              NONE => raise Fail ("undefined path variable: " ^ name)
                                                            | SOME value => go (s, value :: acc)
                                                       end
                                                     | _ => go (s, "$" :: acc)
                                                  )
                              | SOME (c, s) => go (s, String.str c :: acc)
      in go (Substring.full s, [])
      end
end
