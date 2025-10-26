(*
 * Copyright (c) 2025 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure AlbireoSyntax = struct
type ident = string
datatype int_type = IT_32 | IT_64
datatype typ = I32 | I64 | F32 | F64 | V128 | REF of { nullable: bool, to: heap_typ }
     and heap_typ = HT_ANY | HT_I31 | HT_EXTERN
datatype bin_op = PlusOp | MinusOp | TimesOp | DivOp
datatype exp = IntConstExp of int * int_type option
             | VarExp of ident
             | BinExp of bin_op * exp * exp
datatype stmt = ReturnStat of exp list
datatype dec = FuncDec of { name : ident
                          , params : (ident * typ) list
                          , results : typ list
                          , body : stmt list
                          }
end;
