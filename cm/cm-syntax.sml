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
datatype ppexp = PPNumber of int
               | PPDefinedCM of cmid
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
end;
