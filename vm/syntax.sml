structure USyntax = struct
datatype exp = UNIT
             | NIL
             | BOOL of bool
             | INT of Int64.int
             | WORD of Word64.word
             | REAL64 of Real64.real
             | STRING of string
             | VAR of string
             | TUPLE of exp list
             | LAMBDA of string * exp
             | APP of exp * exp
             | LET of stmt list * exp
             | IF of exp * exp * exp
             | RAISE of exp
             | HANDLE of exp * string * exp
             | PUSH_PROMPT of exp * exp
             | WITH_SUBCONT of exp * exp
             | PUSH_SUBCONT of exp * exp
             | ABORT of exp * exp
             | GET_DATA_TAG of exp
             | GET_DATA_PAYLOAD of exp
             | CONSTRUCT_DATA_WITHOUT_PAYLOAD of int
             | CONSTRUCT_DATA_WITH_PAYLOAD of int * exp
             | PRIMCALL0 of Prim0.t
             | PRIMCALL1 of Prim1.t * exp
             | PRIMCALL2 of Prim2.t * exp * exp
             | PRIMCALL3 of Prim3.t * exp * exp * exp
     and stmt = VAL of string * exp
              | VALREC of (string * exp) list (* RHS must be lambda *)
              | DO of exp
fun nameToString name = "%" ^ name
fun expToString UNIT = "unit"
  | expToString NIL = "nil"
  | expToString (BOOL b) = Bool.toString b
  | expToString (INT i) = Int64.toString i
  | expToString (WORD i) = "0wx" ^ Word64.toString i
  | expToString (REAL64 x) = Real64.toString x
  | expToString (STRING s) = "\"" ^ String.toString s ^ "\""
  | expToString (VAR name) = nameToString name
  | expToString (TUPLE xs) = "(tuple " ^ String.concatWith " " (List.map expToString xs) ^ ")"
  | expToString (LAMBDA (name, body)) = "(lambda " ^ nameToString name ^ " " ^ expToString body ^ ")"
  | expToString (APP (x, y)) = "(" ^ expToString x ^ " " ^ expToString y ^ ")"
  | expToString (LET (stmts, body)) = "(let (" ^ String.concatWith " " (List.map stmtToString stmts) ^ ") " ^ expToString body ^ ")"
  | expToString (IF (x, y, z)) = "(if " ^ expToString x ^ " " ^ expToString y ^ " " ^ expToString z ^ ")"
  | expToString (RAISE x) = "(raise " ^ expToString x ^ ")"
  | expToString (HANDLE (x, name, handler)) = "(handle " ^ expToString x ^ " " ^ nameToString name ^ " " ^ expToString handler ^ ")"
  | expToString (PUSH_PROMPT (x, y)) = "(push-prompt " ^ expToString x ^ " " ^ expToString y ^ ")"
  | expToString (WITH_SUBCONT (x, y)) = "(with-subcont " ^ expToString x ^ " " ^ expToString y ^ ")"
  | expToString (PUSH_SUBCONT (x, y)) = "(push-subcont " ^ expToString x ^ " " ^ expToString y ^ ")"
  | expToString (ABORT (x, y)) = "(abort " ^ expToString x ^ " " ^ expToString y ^ ")"
  | expToString (GET_DATA_TAG x) = "(get-data-tag " ^ expToString x ^ ")"
  | expToString (GET_DATA_PAYLOAD x) = "(get-data-payload " ^ expToString x ^ ")"
  | expToString (CONSTRUCT_DATA_WITHOUT_PAYLOAD n) = "(construct-data-without-payload " ^ Int.toString n ^ ")"
  | expToString (CONSTRUCT_DATA_WITH_PAYLOAD (n, x)) = "(construct-data-with-payload " ^ Int.toString n ^ " " ^ expToString x ^ ")"
  | expToString (PRIMCALL0 c) = "(prim0 " ^ Prim0.toString c ^ ")"
  | expToString (PRIMCALL1 (c, x)) = "(prim1 " ^ Prim1.toString c ^ " " ^ expToString x ^ ")"
  | expToString (PRIMCALL2 (c, x, y)) = "(prim2 " ^ Prim2.toString c ^ " " ^ expToString x ^ " " ^ expToString y ^ ")"
  | expToString (PRIMCALL3 (c, x, y, z)) = "(prim3 " ^ Prim3.toString c ^ " " ^ expToString x ^ " " ^ expToString y ^ " " ^ expToString z ^ ")"
and stmtToString (VAL (name, exp)) = "(" ^ nameToString name ^ " " ^ expToString exp ^ ")"
  | stmtToString (VALREC bindings) = "(rec " ^ String.concatWith " " (List.map (fn (name, exp) => "(" ^ nameToString name ^ " " ^ expToString exp ^ ")") bindings) ^ ")"
  | stmtToString (DO x) = "(_ " ^ expToString x ^ ")"
structure StringSet = RedBlackSetFn (open String; type ord_key = string)
fun freeVarsExp (bound : StringSet.set, UNIT, acc : StringSet.set) : StringSet.set = acc
  | freeVarsExp (bound, NIL, acc) = acc
  | freeVarsExp (bound, BOOL _, acc) = acc
  | freeVarsExp (bound, INT _, acc) = acc
  | freeVarsExp (bound, WORD _, acc) = acc
  | freeVarsExp (bound, REAL64 _, acc) = acc
  | freeVarsExp (bound, STRING _, acc) = acc
  | freeVarsExp (bound, VAR name, acc) = if StringSet.member (bound, name) then
                                             acc
                                         else
                                             StringSet.add (acc, name)
  | freeVarsExp (bound, TUPLE xs, acc) = List.foldl (fn (x, acc) => freeVarsExp (bound, x, acc)) acc xs
  | freeVarsExp (bound, LAMBDA (name, body), acc) = freeVarsExp (StringSet.add (bound, name), body, acc)
  | freeVarsExp (bound, APP (x, y), acc) = freeVarsExp (bound, x, freeVarsExp (bound, y, acc))
  | freeVarsExp (bound, LET (stmts, body), acc)
    = let val (bound, acc) = List.foldl (fn (VAL (name, x), (bound, acc)) => (StringSet.add (bound, name), freeVarsExp (bound, x, acc))
                                        | (VALREC bindings, (bound, acc)) => let val bound = List.foldl (fn ((name, _), bound) => StringSet.add (bound, name)) bound bindings
                                                                             in (bound, List.foldl (fn ((_, x), acc) => freeVarsExp (bound, x, acc)) acc bindings)
                                                                             end
                                        | (DO x, (bound, acc)) => (bound, freeVarsExp (bound, x, acc))
                                        ) (bound, acc) stmts
      in freeVarsExp (bound, body, acc)
      end
  | freeVarsExp (bound, IF (x, y, z), acc) = freeVarsExp (bound, x, freeVarsExp (bound, y, freeVarsExp (bound, z, acc)))
  | freeVarsExp (bound, RAISE x, acc) = freeVarsExp (bound, x, acc)
  | freeVarsExp (bound, HANDLE (x, name, handler), acc) = freeVarsExp (bound, x, freeVarsExp (StringSet.add (bound, name), handler, acc))
  | freeVarsExp (bound, PUSH_PROMPT (x, y), acc) = freeVarsExp (bound, x, freeVarsExp (bound, y, acc))
  | freeVarsExp (bound, WITH_SUBCONT (x, y), acc) = freeVarsExp (bound, x, freeVarsExp (bound, y, acc))
  | freeVarsExp (bound, PUSH_SUBCONT (x, y), acc) = freeVarsExp (bound, x, freeVarsExp (bound, y, acc))
  | freeVarsExp (bound, ABORT (x, y), acc) = freeVarsExp (bound, x, freeVarsExp (bound, y, acc))
  | freeVarsExp (bound, GET_DATA_TAG x, acc) = freeVarsExp (bound, x, acc)
  | freeVarsExp (bound, GET_DATA_PAYLOAD x, acc) = freeVarsExp (bound, x, acc)
  | freeVarsExp (bound, CONSTRUCT_DATA_WITHOUT_PAYLOAD _, acc) = acc
  | freeVarsExp (bound, CONSTRUCT_DATA_WITH_PAYLOAD (_, x), acc) = freeVarsExp (bound, x, acc)
  | freeVarsExp (bound, PRIMCALL0 _, acc) = acc
  | freeVarsExp (bound, PRIMCALL1 (_, x), acc) = freeVarsExp (bound, x, acc)
  | freeVarsExp (bound, PRIMCALL2 (_, x, y), acc) = freeVarsExp (bound, x, freeVarsExp (bound, y, acc))
  | freeVarsExp (bound, PRIMCALL3 (_, x, y, z), acc) = freeVarsExp (bound, x, freeVarsExp (bound, y, freeVarsExp (bound, z, acc)))
end;
