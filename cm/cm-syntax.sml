structure CMSyntax = struct
type cmid = string
type mlid = string
datatype mlns = STRUCTURE | SIGNATURE | FUNCTOR (* | FUNSIG *)
datatype arith_binop = MUL | DIV | MOD | PLUS | MINUS
datatype arith_exp = ANumber of int
                   | ACMId of cmid
                   | ANegate of arith_exp
                   | ABinary of arith_binop * arith_exp * arith_exp
datatype cmpop = LT | LE | GT | GE | A_EQ | A_NE
datatype bool_binop = B_EQ | B_NE | ANDALSO | ORELSE
datatype ppexp = PPDefinedCM of cmid
               | PPDefinedML of mlns * mlid
               | PPCompare of cmpop * arith_exp * arith_exp
               | PPNot of ppexp
               | PPBoolBinary of bool_binop * ppexp * ppexp
datatype 'a pplistelem = PPJust of 'a
                       | PPConditional of ppexp * 'a pplist * 'a pplist
                       | PPError of string
withtype 'a pplist = 'a pplistelem list
datatype export = MLSymbol of mlns * mlid (* TODO: difference, intersection, union, implicitset *)
datatype member = Member of { pathname : string } (* TODO: class, toolopts *)
structure MLSymbol = struct
type t = mlns * mlid
type ord_key = t
fun compare ((STRUCTURE, a), (STRUCTURE, b)) = String.compare (a, b)
  | compare ((STRUCTURE, _), _) = LESS
  | compare (_, (STRUCTURE, _)) = GREATER
  | compare ((SIGNATURE, a), (SIGNATURE, b)) = String.compare (a, b)
  | compare ((SIGNATURE, _), (FUNCTOR, _)) = LESS
  | compare ((FUNCTOR, _), (SIGNATURE, _)) = GREATER
  | compare ((FUNCTOR, a), (FUNCTOR, b)) = String.compare (a, b)
fun toString (STRUCTURE, a) = "structure " ^ a
  | toString (SIGNATURE, a) = "signature " ^ a
  | toString (FUNCTOR, a) = "functor " ^ a
end
structure MLSymbolSet = RedBlackSetFn (MLSymbol)
structure MLSymbolMap = RedBlackMapFn (MLSymbol)
end;
