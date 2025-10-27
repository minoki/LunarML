(*
 * Copyright (c) 2025 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure AlbireoEmit :> sig
val emit : AlbireoSyntax.dec list -> string
end = struct
structure S = AlbireoSyntax
fun emitHeapTy S.HT_ANY = "any"
  | emitHeapTy S.HT_I31 = "i31"
  | emitHeapTy S.HT_EXTERN = "extern"
fun emitTy S.I32 = "i32"
  | emitTy S.I64 = "i64"
  | emitTy S.F32 = "f32"
  | emitTy S.F64 = "f64"
  | emitTy S.V128 = "v128"
  | emitTy (S.REF { nullable, to }) =
      if nullable then
        "(ref null " ^ emitHeapTy to ^ ")"
      else
        "(ref " ^ emitHeapTy to ^ ")"
fun compileExp (S.IntConstExp (i, SOME S.IT_64)) = ["i64.const " ^ CharVector.map (fn #"~" => #"-" | c => c) (Int.toString i)]
  | compileExp (S.IntConstExp (i, _)) = ["i32.const " ^ CharVector.map (fn #"~" => #"-" | c => c) (Int.toString i)]
  | compileExp (S.VarExp id) = ["local.get $" ^ id]
  | compileExp (S.BinExp (p, a, b)) =
      let val insn = case p of
                       S.PlusOp => "i32.add"
                     | S.MinusOp => "i32.sub"
                     | S.TimesOp => "i32.mul"
                     | S.DivOp => "i32.div_s"
      in compileExp a @ compileExp b @ [insn]
      end
fun emitDec (S.FuncDec { name, params, results, body }) =
  let val name' = "(export \"" ^ name ^ "\")"
      val params' = List.map (fn (id, ty) => "(param $" ^ id ^ " " ^ emitTy ty ^ ")") params
      val results' = List.map (fn ty => "(result " ^ emitTy ty ^ ")") results
      val body' = case body of
          [S.ReturnStat exps] => List.concat (List.map compileExp exps) @ ["return"]
        | _ => raise Fail "unsupported function body"
  in "(func " ^ name' ^ "\n" ^ String.concatWith "\n" (params' @ results' @ body') ^ ")"
  end
fun emit decs = "(module\n" ^ String.concatWith "\n" (List.map emitDec decs) ^ ")"
end;
