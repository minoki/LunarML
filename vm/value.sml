structure Value = struct
type prompt_tag = unit ref
type exn_tag = unit ref * string
datatype value = V_BOGUS
               | V_UNIT
               | V_FALSE
               | V_TRUE
               | V_NIL
               | V_INT of Int64.int
               | V_WORD of Word64.word
               | V_REAL64 of Real64.real
               | V_STRING of string
               | V_ARRAY of value array
               | V_REF of value ref
               | V_CONS of value * value
               | V_TUPLE of value vector
               | V_CLOSURE of { code : code
                              , free : value array
                              }
               | V_DATA of { tag : int, payload : value }
               | V_EXN of { tag : exn_tag, payload : value }
               | V_EXN_TAG of exn_tag
               | V_PROMPT_TAG of prompt_tag
               | V_SUBCONT of { stackSlice : value vector, frameSlice : frame vector }
     and frame = CALL_FRAME of { base : int
                               , return : int (* instruction pointer *)
                               }
               | EXN_FRAME of { base : int
                              , stackTop : int
                              , handler : int (* instruction pointer *)
                              }
               | CONT_PROMPT of { tag : prompt_tag
                                , stack : int
                                }
               | BOGUS_FRAME
withtype code = { instructions : Word8Vector.vector
                , constants : value vector
                , functions : Word8Vector.vector vector
                }
fun toString V_BOGUS = "<bogus>"
  | toString V_UNIT = "unit"
  | toString V_FALSE = "false"
  | toString V_TRUE = "true"
  | toString V_NIL = "nil"
  | toString (V_INT x) = Int64.toString x
  | toString (V_WORD x) = Word64.fmt StringCvt.DEC x
  | toString (V_REAL64 x) = Real64.toString x
  | toString (V_STRING s) = "\"" ^ String.toString s ^ "\""
  | toString (V_ARRAY a) = "<array>" (* TODO *)
  | toString (V_REF a) = "<ref>" (* TODO *)
  | toString (V_CONS a) = "<cons>" (* TODO *)
  | toString (V_TUPLE v) = "(tuple " ^ String.concatWith " " (List.map toString (Vector.foldr (op ::) [] v)) ^ ")"
  | toString (V_CLOSURE _) = "<closure>"
  | toString (V_DATA { tag, payload }) = "(data " ^ Int.toString tag ^ " " ^ toString payload ^ ")"
  | toString (V_EXN { tag = (_, name), payload }) = "(exn " ^ name ^ " " ^ toString payload ^ ")"
  | toString (V_EXN_TAG (_, name)) = "(exn-tag " ^ name ^ ")"
  | toString (V_PROMPT_TAG _) = "<prompt tag>"
  | toString (V_SUBCONT _) = "<subcont>"
val Match_tag = (ref (), "Match")
val Bind_tag = (ref (), "Bind")
val Overflow_tag = (ref (), "Overflow")
val Div_tag = (ref (), "Div")
val Size_tag = (ref (), "Size")
val Subscript_tag = (ref (), "Subscript")
val Fail_tag = (ref (), "Fail")
end;
