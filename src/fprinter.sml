structure FPrinter = struct
structure P = Printer
structure F = FSyntax
val showParen = P.showParen
fun doVId (Syntax.MkVId name) = [P.Fragment name]
  | doVId (Syntax.GeneratedVId (name, n)) = [P.Fragment (name ^ "@" ^ Int.toString n)]
fun doStrId (Syntax.MkStrId name) = [P.Fragment name]
fun doLabel (Syntax.NumericLabel n) = [P.Fragment (Int.toString n)]
  | doLabel (Syntax.IdentifierLabel x) = [P.Fragment x]
fun doTyVar (TypedSyntax.MkTyVar (name, n)) = [P.Fragment (name ^ "@" ^ Int.toString n)]
fun doKind prec F.TypeKind = [P.Fragment "Type"]
  | doKind prec (F.ArrowKind (k1, k2)) = showParen (prec >= 1) (doKind 1 k1 @ P.Fragment " -> " :: doKind 0 k2)
fun doTy prec (F.TyVar tv) = doTyVar tv
  | doTy prec (F.RecordType fields) = P.Fragment "{" :: P.commaSep (Syntax.LabelMap.foldri (fn (label, ty, xs) => (doLabel label @ P.Fragment ": " :: doTy 0 ty) :: xs) [] fields) @ [P.Fragment "}"]
  | doTy prec (F.AppType { applied, arg }) = showParen (prec >= 2) (doTy 1 applied @ P.Fragment " " :: doTy 2 arg)
  | doTy prec (F.FnType (ty1, ty2)) = showParen (prec >= 1) (doTy 1 ty1 @ P.Fragment " -> " :: doTy 0 ty2)
  | doTy prec (F.ForallType (tv, kind, ty)) = showParen (prec >= 1) (P.Fragment "forall " :: doTyVar tv @ P.Fragment  " : " :: doKind 0 kind @ P.Fragment ". " :: doTy 0 ty)
  | doTy prec (F.ExistsType (tv, kind, ty)) = showParen (prec >= 1) (P.Fragment "exists " :: doTyVar tv @ P.Fragment  " : " :: doKind 0 kind @ P.Fragment ". " :: doTy 0 ty)
  | doTy prec (F.TypeFn (tv, kind, ty)) = showParen (prec >= 1) (P.Fragment "fn " :: doTyVar tv @ P.Fragment  " : " :: doKind 0 kind @ P.Fragment ". " :: doTy 0 ty)
fun doPrimOp (F.IntConstOp x) = [P.Fragment ("int " ^ IntInf.toString x)]
  | doPrimOp (F.WordConstOp x) = [P.Fragment ("word " ^ IntInf.toString x)]
  | doPrimOp (F.RealConstOp x) = [P.Fragment ("real " ^ Numeric.Notation.toString "~" x)]
  | doPrimOp (F.StringConstOp x) = [P.Fragment ("string \"" ^ Vector.foldr (fn (c, acc) => StringElement.charToString (StringElement.CODEUNIT c) ^ acc) "\"" x)]
  | doPrimOp (F.CharConstOp x) = [P.Fragment ("char \"" ^ StringElement.charToString (StringElement.CODEUNIT x) ^ "\"")]
  | doPrimOp (F.RaiseOp span) = [P.Fragment "raise"]
  | doPrimOp F.ListOp = [P.Fragment "list"]
  | doPrimOp F.VectorOp = [P.Fragment "vector"]
  | doPrimOp (F.DataTagOp _) = [P.Fragment "data-tag"]
  | doPrimOp (F.DataPayloadOp _) = [P.Fragment "data-payload"]
  | doPrimOp F.ExnPayloadOp = [P.Fragment "exn-payload"]
  | doPrimOp (F.ConstructValOp _) = [P.Fragment "ConstructVal"]
  | doPrimOp (F.ConstructValWithPayloadOp _) = [P.Fragment "ConstructValWithPayload"]
  | doPrimOp F.ConstructExnOp = [P.Fragment "ConstructExn"]
  | doPrimOp F.ConstructExnWithPayloadOp = [P.Fragment "ConstructExnWithPayload"]
  | doPrimOp (F.PrimFnOp primOp) = [P.Fragment (Primitives.toString primOp)]
  | doPrimOp F.JsCallOp = [P.Fragment "JsCall"]
fun doPat prec (F.WildcardPat _) = [P.Fragment "_"]
  | doPat prec (F.SConPat { scon, ... }) = [P.Fragment (Syntax.print_SCon scon)]
  | doPat prec (F.VarPat (_, vid, ty)) = showParen (prec >= 1) (P.Fragment (TypedSyntax.print_VId vid) :: P.Fragment " : " :: doTy 0 ty)
  | doPat prec (F.RecordPat { sourceSpan, fields, ellipsis }) = P.Fragment "{" :: P.commaSep (List.foldr (fn ((label, pat), xs) => (doLabel label @ P.Fragment ": " :: doPat 0 pat) :: xs) (case ellipsis of SOME basePat => [P.Fragment "...=" :: doPat 0 basePat] | NONE => []) fields) @ [P.Fragment "}"]
  | doPat prec (F.ValConPat { sourceSpan = _, info, payload = NONE }) = showParen (prec >= 1) [P.Fragment (#tag info)]
  | doPat prec (F.ValConPat { sourceSpan = _, info, payload = SOME (payloadTy, payloadPat) }) = showParen (prec >= 1) (P.Fragment (#tag info) :: P.Fragment " " :: doPat 1 payloadPat)
  | doPat prec (F.ExnConPat { sourceSpan = _, tagPath, payload = NONE }) = showParen (prec >= 1) (doExp 0 tagPath)
  | doPat prec (F.ExnConPat { sourceSpan = _, tagPath, payload = SOME (payloadTy, payloadPat) }) = showParen (prec >= 1) (doExp 0 tagPath @ P.Fragment " " :: doPat 1 payloadPat)
  | doPat prec (F.LayeredPat (_, vid, ty, pat)) = showParen (prec >= 1) (P.Fragment (TypedSyntax.print_VId vid) :: P.Fragment " : " :: doTy 1 ty @ P.Fragment " as " :: doPat 1 pat)
  | doPat prec (F.VectorPat (_, pats, wildcard, elemTy)) = P.Fragment "#[" :: P.commaSep (Vector.foldr (fn (pat, xs) => doPat 0 pat :: xs) (if wildcard then [[P.Fragment "..."]] else []) pats) @ [P.Fragment "]"] (* elemTy? *)
(* precedence
 * atomexp ::= PrimExp | VarExp | RecordExp | ProjectionExp | StructExp | '(' exp ')' (* prec: 2 *)
 * appexp ::= atomexp
            | appexp atomexp
            | appexp '[' ty ']'
            | "#" slabel atomexp (* prec: 1 *)
 * exp ::= appexp
         | "let" dec "in" exp "end"
         | "_try" exp "handle" vid "=>" exp
         | "if" exp "then" exp "else" exp
         | "fn" vid "=>" exp
         | "fn type" tv ":" kind "=>" exp
         | "case" exp "of" matches (* prec: 0 *)
 *)
and doExp prec (F.PrimExp (primOp, types, exps)) = P.Fragment "_prim." :: doPrimOp primOp @ P.Fragment " [" :: P.commaSep (List.map (doTy 0) types) @ (P.Fragment "] (" :: P.commaSep (List.map (doExp 0) exps) @ [P.Fragment ")"])
  | doExp prec (F.VarExp vid) = [P.Fragment (TypedSyntax.print_VId vid)]
  | doExp prec (F.RecordExp fields) = P.Fragment "{" :: P.commaSep (List.foldr (fn ((label, exp), xs) => (doLabel label @ P.Fragment " = " :: doExp 0 exp) :: xs) [] fields) @ [P.Fragment "}"]
  | doExp prec (F.LetExp (dec, exp)) = showParen (prec >= 1) (P.Fragment "let " :: doDec dec @ P.Fragment " in " :: doExp 0 exp @ [P.Fragment " end"])
  | doExp prec (F.AppExp (applied, arg)) = showParen (prec >= 2) (doExp 1 applied @ P.Fragment " " :: doExp 2 arg)
  | doExp prec (F.HandleExp { body, exnName, handler }) = showParen (prec >= 1) (P.Fragment "_try " :: doExp 0 body @ P.Fragment " handle " :: P.Fragment (TypedSyntax.print_VId exnName) :: P.Fragment " => " :: doExp 0 handler)
  | doExp prec (F.IfThenElseExp (exp1, exp2, exp3)) = showParen (prec >= 1) (P.Fragment "if " :: doExp 0 exp1 @ P.Fragment " then " :: doExp 0 exp2 @ P.Fragment " else " :: doExp 0 exp3)
  | doExp prec (F.CaseExp (span, exp, ty, matches)) = showParen (prec >= 1) (P.Fragment "case " :: doExp 0 exp @ P.Fragment " : " :: doTy 0 ty @ P.Fragment " of" :: P.LineTerminator :: P.IncreaseIndent 2 :: List.foldr (fn ((pat, exp), rest) => P.Indent :: P.Fragment "| " :: doPat 0 pat @ P.Fragment " => " :: doExp 0 exp @ P.LineTerminator :: rest) [P.DecreaseIndent 2, P.Indent] matches)
  | doExp prec (F.FnExp (vid, ty, exp)) = showParen (prec >= 1) (P.Fragment "fn " :: P.Fragment (TypedSyntax.print_VId vid) :: P.Fragment " : " :: doTy 0 ty @ P.Fragment " => " :: doExp 0 exp)
  | doExp prec (F.ProjectionExp { label, record, fieldTypes }) = showParen (prec >= 2) (P.Fragment "#" :: doLabel label @ P.Fragment " " :: doExp 2 record)
  | doExp prec (F.TyAbsExp (tv, kind, exp)) = showParen (prec >= 1) (P.Fragment "fn type " :: doTyVar tv @ P.Fragment " : " :: doKind 0 kind @ P.Fragment " => " :: doExp 0 exp)
  | doExp prec (F.TyAppExp (exp, ty)) = showParen (prec >= 2) (doExp 1 exp @ P.Fragment " [" :: doTy 0 ty @ [P.Fragment "]"])
  | doExp prec (F.PackExp { payloadTy, exp, packageTy }) = showParen (prec >= 1) (P.Fragment "_pack (type " :: doTy 0 payloadTy @ P.Fragment ", " :: doExp 0 exp @ P.Fragment ") : " :: doTy 0 packageTy)
and doDec (F.ValDec (vid, SOME ty, exp)) = P.Fragment "val " :: P.Fragment (TypedSyntax.print_VId vid) :: P.Fragment " : " :: doTy 0 ty @ P.Fragment " = " :: doExp 0 exp
  | doDec (F.ValDec (vid, NONE, exp)) = P.Fragment "val " :: P.Fragment (TypedSyntax.print_VId vid) :: P.Fragment " : _ = " :: doExp 0 exp
  | doDec (F.RecValDec binds) = P.Fragment "val rec " :: P.IncreaseIndent 4 :: P.sepBy [P.LineTerminator, P.Indent, P.Fragment "and "] (List.map (fn (vid, ty, exp) => P.Fragment (TypedSyntax.print_VId vid) :: P.Fragment " : " :: doTy 0 ty @ P.Fragment " = " :: doExp 0 exp) binds) @ [P.DecreaseIndent 4]
  | doDec (F.UnpackDec (tv, kind, vid, ty, exp)) = P.Fragment "_unpack (type " :: doTyVar tv @ P.Fragment " : " :: doKind 0 kind @ P.Fragment ", " :: P.Fragment (TypedSyntax.print_VId vid) :: P.Fragment " : " :: doTy 0 ty @ P.Fragment ") = " :: doExp 0 exp
  | doDec (F.IgnoreDec exp) = P.Fragment "val _ = " :: doExp 0 exp
  | doDec (F.DatatypeDec datbinds) = P.Fragment "datatype " :: P.IncreaseIndent 5 :: P.sepBy [P.LineTerminator, P.Indent, P.Fragment "and "] (List.map (fn F.DatBind (tyvars, datty, conbinds) => doTyVar datty @ P.Fragment " " :: P.spaceSep (List.map doTyVar tyvars) @ P.Fragment " = " :: P.sepBy [P.Fragment " | "] (List.map (fn F.ConBind (vid, NONE) => [P.Fragment (TypedSyntax.print_VId vid)]
                                                                                                                                                                                                                                                                                                                            | F.ConBind (vid, SOME ty) => P.Fragment (TypedSyntax.print_VId vid) :: P.Fragment " of " :: doTy 0 ty
                                                                                                                                                                                                                                                                                                                            ) conbinds)) datbinds) @ [P.DecreaseIndent 5]
  | doDec (F.ExceptionDec { name, tagName, payloadTy = NONE }) = P.Fragment "exception " :: P.Fragment (TypedSyntax.print_VId tagName) :: []
  | doDec (F.ExceptionDec { name, tagName, payloadTy = SOME payloadTy }) = P.Fragment "exception " :: P.Fragment (TypedSyntax.print_VId tagName) :: P.Fragment " of " :: doTy 0 payloadTy
  | doDec (F.ExportValue exp) = P.Fragment "_export " :: doExp 0 exp
  | doDec (F.ExportModule fields) = P.Fragment "_export {" :: P.commaSepV (Vector.map (fn (name, exp) => P.Fragment name :: P.Fragment " = " :: doExp 0 exp) fields) @ [P.Fragment "}"]
  | doDec (F.GroupDec (NONE,decs)) = P.Fragment "_group" :: P.LineTerminator :: P.IncreaseIndent 2 :: List.concat (List.map (fn dec => P.Indent :: doDec dec @ [P.LineTerminator]) decs) @ [P.DecreaseIndent 2, P.Indent, P.Fragment "end"]
  | doDec (F.GroupDec (SOME vidset,decs)) = P.Fragment "_group {" :: P.commaSep (TypedSyntax.VIdSet.foldr (fn (vid, xs) => [P.Fragment (TypedSyntax.print_VId vid)] :: xs) [] vidset) @ [P.Fragment "}", P.LineTerminator, P.IncreaseIndent 2] @ List.concat (List.map (fn dec => P.Indent :: doDec dec @ [P.LineTerminator]) decs) @ [P.DecreaseIndent 2, P.Indent, P.Fragment "end"]
fun doDecs decs = List.concat (List.map (fn dec => P.Indent :: doDec dec @ [P.LineTerminator]) decs)
end
