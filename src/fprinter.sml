structure FPrinter = struct
structure P = Printer
structure F = FSyntax
val showParen = P.showParen
fun doVId (Syntax.MkVId name) = [P.Fragment name]
  | doVId (Syntax.GeneratedVId (name, n)) = [P.Fragment (name ^ "@" ^ Int.toString n)]
fun doStrId (Syntax.MkStrId name) = [P.Fragment name]
fun doLabel (Syntax.NumericLabel n) = [P.Fragment (Int.toString n)]
  | doLabel (Syntax.IdentifierLabel x) = [P.Fragment x]
fun doTyVar (USyntax.NamedTyVar (name, _, n)) = [P.Fragment (name ^ "@" ^ Int.toString n)]
  | doTyVar (USyntax.AnonymousTyVar n) = [P.Fragment ("_@" ^ Int.toString n)]
fun doKind prec F.TypeKind = [P.Fragment "Type"]
  | doKind prec (F.ArrowKind (k1, k2)) = showParen (prec >= 1) (doKind 1 k1 @ P.Fragment " -> " :: doKind 0 k2)
fun doTy prec (F.TyVar tv) = doTyVar tv
  | doTy prec (F.RecordType fields) = P.Fragment "{" :: P.commaSep (Syntax.LabelMap.foldri (fn (label, ty, xs) => (doLabel label @ P.Fragment ": " :: doTy 0 ty) :: xs) [] fields) @ [P.Fragment "}"]
  | doTy prec (F.AppType { applied, arg }) = showParen (prec >= 2) (doTy 1 applied @ P.Fragment " " :: doTy 2 arg)
  | doTy prec (F.FnType (ty1, ty2)) = showParen (prec >= 1) (doTy 1 ty1 @ P.Fragment " -> " :: doTy 0 ty2)
  | doTy prec (F.ForallType (tv, kind, ty)) = showParen (prec >= 1) (P.Fragment "forall " :: doTyVar tv @ P.Fragment  " : " :: doKind 0 kind @ P.Fragment ". " :: doTy 0 ty)
  | doTy prec (F.ExistsType (tv, kind, ty)) = showParen (prec >= 1) (P.Fragment "exists " :: doTyVar tv @ P.Fragment  " : " :: doKind 0 kind @ P.Fragment ". " :: doTy 0 ty)
  | doTy prec (F.TypeFn (tv, kind, ty)) = showParen (prec >= 1) (P.Fragment "fn " :: doTyVar tv @ P.Fragment  " : " :: doKind 0 kind @ P.Fragment ". " :: doTy 0 ty)
  | doTy prec (F.SigType { valMap, strMap, exnTags }) = P.spaceSep ([P.Fragment "sig"] :: Syntax.VIdMap.foldri (fn (vid, ty, xs) => (P.Fragment "val " :: doVId vid @ P.Fragment " : " :: doTy 0 ty) :: xs) (Syntax.StrIdMap.foldri (fn (strid, ty, xs) => (P.Fragment "structure " :: doStrId strid @ P.Fragment " : " :: doTy 0 ty) :: xs) (Syntax.VIdSet.foldr (fn (vid, xs) => (P.Fragment "exception " :: doVId vid) :: xs) [[P.Fragment "end"]] exnTags) strMap) valMap) (* Insert newlines? *)
fun doSLabel (F.ValueLabel vid) = P.Fragment "v:" :: doVId vid
  | doSLabel (F.StructLabel strid) = P.Fragment "s:" :: doStrId strid
  | doSLabel (F.ExnTagLabel vid) = P.Fragment "e:" :: doVId vid
fun doPath (F.Root vid) = [P.Fragment (USyntax.print_VId vid)]
  | doPath (F.Child (parent, label)) = doPath parent @ P.Fragment "." :: doSLabel label
fun doPat prec F.WildcardPat = [P.Fragment "_"]
  | doPat prec (F.SConPat scon) = [P.Fragment (Syntax.print_SCon scon)]
  | doPat prec (F.VarPat (vid, ty)) = showParen (prec >= 1) (P.Fragment (USyntax.print_VId vid) :: P.Fragment " : " :: doTy 0 ty)
  | doPat prec (F.RecordPat (fields, wildcard)) = P.Fragment "{" :: P.commaSep (List.foldr (fn ((label, pat), xs) => (doLabel label @ P.Fragment ": " :: doPat 0 pat) :: xs) (if wildcard then [[P.Fragment "..."]] else []) fields) @ [P.Fragment "}"]
  | doPat prec (F.ConPat (path, NONE, tyargs)) = showParen (prec >= 1) (doPath path @ P.Fragment "[" :: P.commaSep (List.map (doTy 0) tyargs) @ [P.Fragment "]"])
  | doPat prec (F.ConPat (path, SOME innerPat, tyargs)) = showParen (prec >= 1) (doPath path @ P.Fragment "[" :: P.commaSep (List.map (doTy 0) tyargs) @ P.Fragment "] " :: doPat 1 innerPat)
  | doPat prec (F.LayeredPat (vid, ty, pat)) = showParen (prec >= 1) (P.Fragment (USyntax.print_VId vid) :: P.Fragment " : " :: doTy 1 ty @ P.Fragment " as " :: doPat 1 pat)
  | doPat prec (F.VectorPat (pats, wildcard, elemTy)) = P.Fragment "#[" :: P.commaSep (Vector.foldr (fn (pat, xs) => doPat 0 pat :: xs) (if wildcard then [[P.Fragment "..."]] else []) pats) @ [P.Fragment "]"] (* elemTy? *)
fun doPrimOp (F.SConOp scon) = [P.Fragment (Syntax.print_SCon scon)]
  | doPrimOp (F.RaiseOp span) = [P.Fragment "raise"]
  | doPrimOp F.ListOp = [P.Fragment "list"]
  | doPrimOp F.VectorOp = [P.Fragment "vector"]
  | doPrimOp F.RecordEqualityOp = [P.Fragment "record-equality"]
  | doPrimOp F.DataTagOp = [P.Fragment "data-tag"]
  | doPrimOp F.DataPayloadOp = [P.Fragment "data-payload"]
  | doPrimOp F.Call2Op = [P.Fragment "call2"]
  | doPrimOp F.ExnInstanceofOp = [P.Fragment "exn-instanceof"]
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
fun doExp prec (F.PrimExp (primOp, types, exps)) = P.Fragment "_prim." :: doPrimOp primOp @ P.Fragment " [" :: P.commaSepV (Vector.map (doTy 0) types) @ (P.Fragment "] (" :: P.commaSepV (Vector.map (doExp 0) exps) @ [P.Fragment ")"])
  | doExp prec (F.VarExp vid) = [P.Fragment (USyntax.print_VId vid)]
  | doExp prec (F.RecordExp fields) = P.Fragment "{" :: P.commaSep (List.foldr (fn ((label, exp), xs) => (doLabel label @ P.Fragment " = " :: doExp 0 exp) :: xs) [] fields) @ [P.Fragment "}"]
  | doExp prec (F.LetExp (dec, exp)) = showParen (prec >= 1) (P.Fragment "let " :: doDec dec @ P.Fragment " in " :: doExp 0 exp @ [P.Fragment " end"])
  | doExp prec (F.AppExp (applied, arg)) = showParen (prec >= 2) (doExp 1 applied @ P.Fragment " " :: doExp 2 arg)
  | doExp prec (F.HandleExp { body, exnName, handler }) = showParen (prec >= 1) (P.Fragment "_try " :: doExp 0 body @ P.Fragment " handle " :: P.Fragment (USyntax.print_VId exnName) :: P.Fragment " => " :: doExp 0 handler)
  | doExp prec (F.IfThenElseExp (exp1, exp2, exp3)) = showParen (prec >= 1) (P.Fragment "if " :: doExp 0 exp1 @ P.Fragment " then " :: doExp 0 exp2 @ P.Fragment " else " :: doExp 0 exp3)
  | doExp prec (F.CaseExp (span, exp, ty, matches)) = showParen (prec >= 1) (P.Fragment "case " :: doExp 0 exp @ P.Fragment " : " :: doTy 0 ty @ P.Fragment " of" :: P.LineTerminator :: P.IncreaseIndent 2 :: List.foldr (fn ((pat, exp), rest) => P.Indent :: P.Fragment "| " :: doPat 0 pat @ P.Fragment " => " :: doExp 0 exp @ P.LineTerminator :: rest) [P.DecreaseIndent 2, P.Indent] matches)
  | doExp prec (F.FnExp (vid, ty, exp)) = showParen (prec >= 1) (P.Fragment "fn " :: P.Fragment (USyntax.print_VId vid) :: P.Fragment " : " :: doTy 0 ty @ P.Fragment " => " :: doExp 0 exp)
  | doExp prec (F.ProjectionExp { label, recordTy, fieldTy }) = P.Fragment "#" :: doLabel label
  | doExp prec (F.TyAbsExp (tv, kind, exp)) = showParen (prec >= 1) (P.Fragment "fn type " :: doTyVar tv @ P.Fragment " : " :: doKind 0 kind @ P.Fragment " => " :: doExp 0 exp)
  | doExp prec (F.TyAppExp (exp, ty)) = showParen (prec >= 2) (doExp 1 exp @ P.Fragment " [" :: doTy 0 ty @ [P.Fragment "]"])
  | doExp prec (F.StructExp { valMap, strMap, exnTagMap }) = P.spaceSep ([P.Fragment "struct"] :: Syntax.VIdMap.foldri (fn (vid, path, xs) => (P.Fragment "val " :: doVId vid @ P.Fragment " = " :: doPath path) :: xs) (Syntax.StrIdMap.foldri (fn (strid, path, xs) => (P.Fragment "structure " :: doStrId strid @ P.Fragment " = " :: doPath path) :: xs) (Syntax.VIdMap.foldri (fn (vid, path, xs) => (P.Fragment "exception " :: doVId vid @ P.Fragment ".tag = " :: doPath path) :: xs) [[P.Fragment "end"]] exnTagMap) strMap) valMap) (* Insert newlines? *)
  | doExp prec (F.SProjectionExp (exp, slabel)) = showParen (prec >= 2) (P.Fragment "#" :: doSLabel slabel @ P.Fragment " " :: doExp 2 exp)
  | doExp prec (F.PackExp { payloadTy, exp, packageTy }) = showParen (prec >= 1) (P.Fragment "_pack (type " :: doTy 0 payloadTy @ P.Fragment ", " :: doExp 0 exp @ P.Fragment ") : " :: doTy 0 packageTy)
and doDec (F.ValDec (F.SimpleBind (vid, ty, exp))) = P.Fragment "val " :: P.Fragment (USyntax.print_VId vid) :: P.Fragment " : " :: doTy 0 ty @ P.Fragment " = " :: doExp 0 exp
  | doDec (F.ValDec (F.TupleBind (binds, exp))) = P.Fragment "val (" :: P.commaSep (List.map (fn (vid, ty) => P.Fragment (USyntax.print_VId vid) :: P.Fragment " : " :: doTy 0 ty) binds) @ P.Fragment ") = " :: doExp 0 exp
  | doDec (F.RecValDec binds) = P.Fragment "val rec " :: P.IncreaseIndent 4 :: P.sepBy [P.LineTerminator, P.Indent, P.Fragment "and "] (List.map (fn (vid, ty, exp) => P.Fragment (USyntax.print_VId vid) :: P.Fragment " : " :: doTy 0 ty @ P.Fragment " = " :: doExp 0 exp) binds) @ [P.DecreaseIndent 4]
  | doDec (F.UnpackDec (tv, kind, vid, ty, exp)) = P.Fragment "_unpack (type " :: doTyVar tv @ P.Fragment " : " :: doKind 0 kind @ P.Fragment ", " :: P.Fragment (USyntax.print_VId vid) :: P.Fragment " : " :: doTy 0 ty @ P.Fragment ") = " :: doExp 0 exp
  | doDec (F.IgnoreDec exp) = P.Fragment "val _ = " :: doExp 0 exp
  | doDec (F.DatatypeDec datbinds) = P.Fragment "datatype " :: P.IncreaseIndent 5 :: P.sepBy [P.LineTerminator, P.Indent, P.Fragment "and "] (List.map (fn F.DatBind (tyvars, datty, conbinds) => doTyVar datty @ P.Fragment " " :: P.spaceSep (List.map doTyVar tyvars) @ P.Fragment " = " :: P.sepBy [P.Fragment " | "] (List.map (fn F.ConBind (vid, NONE) => [P.Fragment (USyntax.print_VId vid)]
                                                                                                                                                                                                                                                                                                                            | F.ConBind (vid, SOME ty) => P.Fragment (USyntax.print_VId vid) :: P.Fragment " of " :: doTy 0 ty
                                                                                                                                                                                                                                                                                                                            ) conbinds)) datbinds) @ [P.DecreaseIndent 5]
  | doDec (F.ExceptionDec { conName, tagName, payloadTy = NONE }) = P.Fragment "exception " :: P.Fragment (USyntax.print_VId conName) :: P.Fragment ", tag=" :: P.Fragment (USyntax.print_VId tagName) :: []
  | doDec (F.ExceptionDec { conName, tagName, payloadTy = SOME payloadTy }) = P.Fragment "exception " :: P.Fragment (USyntax.print_VId conName) :: P.Fragment " of " :: doTy 0 payloadTy @ P.Fragment ", tag=" :: P.Fragment (USyntax.print_VId tagName) :: []
  | doDec (F.ExceptionRepDec { conName, conPath, tagPath, payloadTy = NONE }) = P.Fragment "exception " :: P.Fragment (USyntax.print_VId conName) :: P.Fragment " = " :: doPath conPath @ P.Fragment ", tag=" :: doPath tagPath
  | doDec (F.ExceptionRepDec { conName, conPath, tagPath, payloadTy = SOME payloadTy }) = P.Fragment "exception " :: P.Fragment (USyntax.print_VId conName) :: P.Fragment " of " :: doTy 0 payloadTy @ P.Fragment " = " :: doPath conPath @ P.Fragment ", tag=" :: doPath tagPath
  | doDec (F.ExportValue exp) = P.Fragment "_export " :: doExp 0 exp
  | doDec (F.ExportModule fields) = P.Fragment "_export {" :: P.commaSepV (Vector.map (fn (name, exp) => P.Fragment name :: P.Fragment " = " :: doExp 0 exp) fields) @ [P.Fragment "}"]
  | doDec (F.GroupDec (NONE,decs)) = P.Fragment "_group" :: P.LineTerminator :: P.IncreaseIndent 2 :: List.concat (List.map (fn dec => P.Indent :: doDec dec @ [P.LineTerminator]) decs) @ [P.DecreaseIndent 2, P.Indent, P.Fragment "end"]
  | doDec (F.GroupDec (SOME vidset,decs)) = P.Fragment "_group {" :: P.commaSep (USyntax.VIdSet.foldr (fn (vid, xs) => [P.Fragment (USyntax.print_VId vid)] :: xs) [] vidset) @ [P.Fragment "}", P.LineTerminator, P.IncreaseIndent 2] @ List.concat (List.map (fn dec => P.Indent :: doDec dec @ [P.LineTerminator]) decs) @ [P.DecreaseIndent 2, P.Indent, P.Fragment "end"]
fun doDecs decs = List.concat (List.map (fn dec => P.Indent :: doDec dec @ [P.LineTerminator]) decs)
end
