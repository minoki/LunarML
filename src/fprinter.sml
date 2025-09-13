(*
 * Copyright (c) 2023 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure FPrinter :>
sig
  val doTyVar: FSyntax.TyVar -> Printer.fragment list
  val doKind: int -> FSyntax.Kind -> Printer.fragment list
  val doTy: int -> FSyntax.Ty -> Printer.fragment list
  val doPrimOp: FSyntax.PrimOp -> Printer.fragment list
  val doExp: int -> FSyntax.Exp -> Printer.fragment list
end =
struct
  structure P = Printer
  structure F = FSyntax
  val showParen = P.showParen
  (* fun doVId (Syntax.MkVId name) = [P.Fragment name]
    | doVId (Syntax.GeneratedVId (name, n)) = [P.Fragment (name ^ "@" ^ Int.toString n)] *)
  (* fun doStrId (Syntax.MkStrId name) = [P.Fragment name] *)
  fun doLabel (Syntax.NumericLabel n) =
        [P.Fragment (Int.toString n)]
    | doLabel (Syntax.IdentifierLabel x) = [P.Fragment x]
  fun doTyVar (TypedSyntax.MkTyVar (name, n)) =
    [P.Fragment (name ^ "@" ^ Int.toString n)]
  fun doKind _ F.TypeKind = [P.Fragment "Type"]
    | doKind prec (F.ArrowKind (k1, k2)) =
        showParen (prec >= 1) (doKind 1 k1 @ P.Fragment " -> " :: doKind 0 k2)
  fun doTy _ (F.TyVar tv) = doTyVar tv
    | doTy _ (F.RecordType fields) =
        P.Fragment "{"
        ::
        P.commaSep
          (Syntax.LabelMap.foldri
             (fn (label, ty, xs) =>
                (doLabel label @ P.Fragment ": " :: doTy 0 ty) :: xs) [] fields)
        @ [P.Fragment "}"]
    | doTy prec (F.AppType {applied, arg}) =
        showParen (prec >= 2) (doTy 1 applied @ P.Fragment " " :: doTy 2 arg)
    | doTy prec (F.MultiFnType ([param], result)) =
        showParen (prec >= 1)
          (doTy 1 param @ P.Fragment " -> " :: doTy 0 result)
    | doTy prec (F.MultiFnType (params, result)) =
        showParen (prec >= 1)
          (P.Fragment "("
           ::
           #2
             (List.foldr
                (fn (param, (first, acc)) =>
                   ( false
                   , doTy 1 param
                     @ (if first then acc else P.Fragment ", " :: acc)
                   )) (true, P.Fragment ") -> " :: doTy 0 result) params))
    | doTy prec (F.ForallType (tv, kind, ty)) =
        showParen (prec >= 1)
          (P.Fragment "forall "
           ::
           doTyVar tv
           @ P.Fragment " : " :: doKind 0 kind @ P.Fragment ". " :: doTy 0 ty)
    | doTy prec (F.ExistsType (tv, kind, ty)) =
        showParen (prec >= 1)
          (P.Fragment "exists "
           ::
           doTyVar tv
           @ P.Fragment " : " :: doKind 0 kind @ P.Fragment ". " :: doTy 0 ty)
    | doTy prec (F.TypeFn (tv, kind, ty)) =
        showParen (prec >= 1)
          (P.Fragment "fn "
           ::
           doTyVar tv
           @ P.Fragment " : " :: doKind 0 kind @ P.Fragment ". " :: doTy 0 ty)
    | doTy _ (F.AnyType _) = [P.Fragment "Any"]
    | doTy prec (ty as F.DelayedSubst _) =
        doTy prec (F.forceTy ty)
  fun doPrimOp (F.IntConstOp x) =
        [P.Fragment ("int " ^ IntInf.toString x)]
    | doPrimOp (F.WordConstOp x) =
        [P.Fragment ("word " ^ IntInf.toString x)]
    | doPrimOp (F.RealConstOp x) =
        [P.Fragment ("real " ^ Numeric.Notation.toString "~" x)]
    | doPrimOp (F.Char8ConstOp x) =
        [P.Fragment ("char8 \"" ^ Char.toString x ^ "\"")]
    | doPrimOp (F.Char16ConstOp x) =
        [P.Fragment
           ("char16 \"" ^ StringElement.charToString (StringElement.CODEUNIT x)
            ^ "\"")]
    | doPrimOp (F.String8ConstOp x) =
        [P.Fragment ("string8 \"" ^ String.toString x ^ "\"")]
    | doPrimOp (F.String16ConstOp x) =
        [P.Fragment
           ("string16 \""
            ^
            Vector.foldr
              (fn (c, acc) =>
                 StringElement.charToString (StringElement.CODEUNIT c) ^ acc)
              "\"" x)]
    | doPrimOp (F.RaiseOp _) = [P.Fragment "raise"]
    | doPrimOp F.ListOp = [P.Fragment "list"]
    | doPrimOp F.VectorOp = [P.Fragment "vector"]
    | doPrimOp (F.DataTagAsStringOp _) = [P.Fragment "data-tag-as-string"]
    | doPrimOp (F.DataTagAsString16Op _) = [P.Fragment "data-tag-as-string16"]
    | doPrimOp (F.DataPayloadOp _) = [P.Fragment "data-payload"]
    | doPrimOp F.ExnPayloadOp = [P.Fragment "exn-payload"]
    | doPrimOp (F.ConstructValOp _) = [P.Fragment "ConstructVal"]
    | doPrimOp (F.ConstructValWithPayloadOp _) =
        [P.Fragment "ConstructValWithPayload"]
    | doPrimOp F.ConstructExnOp = [P.Fragment "ConstructExn"]
    | doPrimOp F.ConstructExnWithPayloadOp =
        [P.Fragment "ConstructExnWithPayload"]
    | doPrimOp (F.PrimCall primOp) =
        [P.Fragment (Primitives.toString primOp)]
    | doPrimOp F.JsCallOp = [P.Fragment "JsCall"]
    | doPrimOp F.JsMethodOp = [P.Fragment "JsMethod"]
    | doPrimOp F.JsNewOp = [P.Fragment "JsNew"]
    | doPrimOp F.LuaCallOp = [P.Fragment "LuaCall"]
    | doPrimOp F.LuaCall1Op = [P.Fragment "LuaCall1"]
    | doPrimOp (F.LuaCallNOp _) = [P.Fragment "LuaCallN"]
    | doPrimOp (F.LuaMethodOp _) = [P.Fragment "LuaMethod"]
    | doPrimOp (F.LuaMethod1Op _) = [P.Fragment "LuaMethod1"]
    | doPrimOp (F.LuaMethodNOp _) = [P.Fragment "LuaMethodN"]
  fun doPat _ (F.WildcardPat _) = [P.Fragment "_"]
    | doPat _ (F.SConPat {scon = F.IntegerConstant x, ...}) =
        [P.Fragment (IntInf.toString x)]
    | doPat _ (F.SConPat {scon = F.WordConstant x, ...}) =
        [P.Fragment (IntInf.toString x)]
    | doPat _ (F.SConPat {scon = F.CharConstant x, ...}) =
        [P.Fragment (Char.toString x)]
    | doPat _ (F.SConPat {scon = F.Char16Constant x, ...}) =
        [P.Fragment (Int.toString x)]
    | doPat _ (F.SConPat {scon = F.StringConstant x, ...}) =
        [P.Fragment (String.toString x)]
    | doPat _ (F.SConPat {scon = F.String16Constant _, ...}) =
        [P.Fragment "<string16>"]
    | doPat prec (F.VarPat (_, vid, ty)) =
        showParen (prec >= 1)
          (P.Fragment (TypedSyntax.print_VId vid) :: P.Fragment " : "
           :: doTy 0 ty)
    | doPat _ (F.RecordPat {sourceSpan = _, fields, ellipsis, allFields = _}) =
        P.Fragment "{"
        ::
        P.commaSep
          (List.foldr
             (fn ((label, pat), xs) =>
                (doLabel label @ P.Fragment ": " :: doPat 0 pat) :: xs)
             (case ellipsis of
                SOME basePat => [P.Fragment "...=" :: doPat 0 basePat]
              | NONE => []) fields) @ [P.Fragment "}"]
    | doPat prec (F.ValConPat {sourceSpan = _, info, payload = NONE}) =
        showParen (prec >= 1) [P.Fragment (#tag info)]
    | doPat prec
        (F.ValConPat {sourceSpan = _, info, payload = SOME (_, payloadPat)}) =
        showParen (prec >= 1)
          (P.Fragment (#tag info) :: P.Fragment " " :: doPat 1 payloadPat)
    | doPat prec (F.ExnConPat {sourceSpan = _, predicate, payload = NONE}) =
        showParen (prec >= 1) (doExp 0 predicate)
    | doPat prec
        (F.ExnConPat
           { sourceSpan = _
           , predicate
           , payload = SOME (_, getPayload, payloadPat)
           }) =
        showParen (prec >= 1)
          (doExp 0 predicate
           @
           P.Fragment " "
           :: doExp 0 getPayload @ P.Fragment " " :: doPat 1 payloadPat)
    | doPat prec (F.LayeredPat (_, vid, ty, pat)) =
        showParen (prec >= 1)
          (P.Fragment (TypedSyntax.print_VId vid) :: P.Fragment " : "
           :: doTy 1 ty @ P.Fragment " as " :: doPat 1 pat)
    | doPat _ (F.VectorPat (_, pats, wildcard, _)) =
        P.Fragment "#["
        ::
        P.commaSep
          (Vector.foldr (fn (pat, xs) => doPat 0 pat :: xs)
             (if wildcard then [[P.Fragment "..."]] else []) pats)
        @ [P.Fragment "]"] (* elemTy? *)
    | doPat _ (F.BogusPat _) = [P.Fragment "<bogus pattern>"]
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
  and doExp _ (F.PrimExp (primOp, types, exps)) =
        P.Fragment "_prim."
        ::
        doPrimOp primOp
        @
        P.Fragment " ["
        ::
        P.commaSep (List.map (doTy 0) types)
        @
        (P.Fragment "] ("
         :: P.commaSep (List.map (doExp 0) exps) @ [P.Fragment ")"])
    | doExp _ (F.VarExp vid) =
        [P.Fragment (TypedSyntax.print_VId vid)]
    | doExp _ (F.RecordExp fields) =
        P.Fragment "{"
        ::
        P.commaSep
          (List.foldr
             (fn ((label, exp), xs) =>
                (doLabel label @ P.Fragment " = " :: doExp 0 exp) :: xs) []
             fields) @ [P.Fragment "}"]
    | doExp prec (F.LetExp (decs, exp)) =
        showParen (prec >= 1)
          (P.Fragment "let" :: P.IncreaseIndent 2 :: P.LineTerminator
           ::
           List.concat
             (List.map (fn dec => P.Indent :: doDec dec @ [P.LineTerminator])
                decs) (* (List.map doDec decs) *)
           @
           P.DecreaseIndent 2 :: P.Indent :: P.Fragment "in" :: P.LineTerminator
           :: P.IncreaseIndent 2 :: P.Indent
           ::
           doExp 0 exp
           @ [P.DecreaseIndent 2, P.LineTerminator, P.Indent, P.Fragment "end"])
    | doExp prec (F.MultiAppExp (applied, [arg])) =
        showParen (prec >= 2) (doExp 1 applied @ P.Fragment " " :: doExp 2 arg)
    | doExp prec (F.MultiAppExp (applied, args)) =
        showParen (prec >= 2)
          (doExp 1 applied
           @
           P.Fragment " ("
           ::
           List.foldr
             (fn (arg, acc) =>
                doExp 2 arg
                @ (if List.null acc then acc else P.Fragment " " :: acc)) []
             args @ [P.Fragment ")"])
    | doExp prec (F.HandleExp {body, exnName, handler, resultTy = _}) =
        showParen (prec >= 1)
          (P.Fragment "_try "
           ::
           doExp 0 body
           @
           P.Fragment " handle " :: P.Fragment (TypedSyntax.print_VId exnName)
           :: P.Fragment " => " :: doExp 0 handler)
    | doExp prec (F.IfThenElseExp (exp1, exp2, exp3)) =
        showParen (prec >= 1)
          (P.Fragment "if "
           ::
           doExp 0 exp1
           @
           P.Fragment " then "
           :: doExp 0 exp2 @ P.Fragment " else " :: doExp 0 exp3)
    | doExp prec
        (F.CaseExp
           { sourceSpan = _
           , subjectExp
           , subjectTy
           , matches
           , matchType = _
           , resultTy = _
           }) =
        showParen (prec >= 1)
          (P.Fragment "case "
           ::
           doExp 0 subjectExp
           @
           P.Fragment " : "
           ::
           doTy 0 subjectTy
           @
           P.Fragment " of" :: P.LineTerminator :: P.IncreaseIndent 2
           ::
           List.foldr
             (fn ((pat, exp), rest) =>
                P.Indent :: P.Fragment "| "
                ::
                doPat 0 pat
                @ P.Fragment " => " :: doExp 0 exp @ P.LineTerminator :: rest)
             [P.DecreaseIndent 2, P.Indent] matches)
    | doExp prec (F.MultiFnExp (params, exp)) =
        showParen (prec >= 1)
          (P.Fragment "fn ("
           ::
           List.foldr
             (fn ((vid, ty), acc) =>
                P.Fragment (TypedSyntax.print_VId vid) :: P.Fragment " : "
                ::
                doTy 0 ty
                @ (if List.null acc then acc else P.Fragment ", " :: acc)) []
             params @ P.Fragment ") => " :: doExp 0 exp)
    | doExp prec (F.ProjectionExp {label, record, fieldTypes = _}) =
        showParen (prec >= 2)
          (P.Fragment "#" :: doLabel label @ P.Fragment " " :: doExp 2 record)
    | doExp prec (F.TyAbsExp (tv, kind, exp)) =
        showParen (prec >= 1)
          (P.Fragment "fn type "
           ::
           doTyVar tv
           @
           P.Fragment " : " :: doKind 0 kind @ P.Fragment " => " :: doExp 0 exp)
    | doExp prec (F.TyAppExp (exp, ty)) =
        showParen (prec >= 2)
          (doExp 1 exp @ P.Fragment " [" :: doTy 0 ty @ [P.Fragment "]"])
    | doExp prec (F.PackExp {payloadTy, exp, packageTy}) =
        showParen (prec >= 1)
          (P.Fragment "_pack (type "
           ::
           doTy 0 payloadTy
           @
           P.Fragment ", "
           :: doExp 0 exp @ P.Fragment ") : " :: doTy 0 packageTy)
    | doExp _ (F.BogusExp _) = [P.Fragment "<bogus>"]
    | doExp _ F.ExitProgram = [P.Fragment "<exit program>"]
    | doExp _ (F.ExportValue exp) =
        P.Fragment "_export " :: doExp 2 exp
    | doExp _ (F.ExportModule fields) =
        P.Fragment "_export {"
        ::
        P.commaSepV
          (Vector.map
             (fn (name, exp) =>
                P.Fragment name :: P.Fragment " = " :: doExp 0 exp) fields)
        @ [P.Fragment "}"]
  and doDec (F.ValDec (vid, SOME ty, exp)) =
        P.Fragment "val " :: P.Fragment (TypedSyntax.print_VId vid)
        :: P.Fragment " : " :: doTy 0 ty @ P.Fragment " = " :: doExp 0 exp
    | doDec (F.ValDec (vid, NONE, exp)) =
        P.Fragment "val " :: P.Fragment (TypedSyntax.print_VId vid)
        :: P.Fragment " : _ = " :: doExp 0 exp
    | doDec (F.RecValDec binds) =
        P.Fragment "val rec " :: P.IncreaseIndent 4
        ::
        P.sepBy [P.LineTerminator, P.Indent, P.Fragment "and "]
          (List.map
             (fn (vid, ty, exp) =>
                P.Fragment (TypedSyntax.print_VId vid) :: P.Fragment " : "
                :: doTy 0 ty @ P.Fragment " = " :: doExp 0 exp) binds)
        @ [P.DecreaseIndent 4]
    | doDec (F.UnpackDec (tv, kind, vid, ty, exp)) =
        P.Fragment "_unpack (type "
        ::
        doTyVar tv
        @
        P.Fragment " : "
        ::
        doKind 0 kind
        @
        P.Fragment ", " :: P.Fragment (TypedSyntax.print_VId vid)
        :: P.Fragment " : " :: doTy 0 ty @ P.Fragment ") = " :: doExp 0 exp
    | doDec (F.IgnoreDec exp) =
        P.Fragment "val _ = " :: doExp 0 exp
    | doDec (F.DatatypeDec datbinds) =
        P.Fragment "datatype " :: P.IncreaseIndent 5
        ::
        P.sepBy [P.LineTerminator, P.Indent, P.Fragment "and "]
          (List.map
             (fn F.DatBind (tyvars, datty, conbinds) =>
                doTyVar datty
                @
                P.Fragment " "
                ::
                P.spaceSep (List.map doTyVar tyvars)
                @
                P.Fragment " = "
                ::
                P.sepBy [P.Fragment " | "]
                  (List.map
                     (fn F.ConBind (vid, NONE) =>
                        [P.Fragment (TypedSyntax.print_VId vid)]
                       | F.ConBind (vid, SOME ty) =>
                        P.Fragment (TypedSyntax.print_VId vid)
                        :: P.Fragment " of " :: doTy 0 ty) conbinds)) datbinds)
        @ [P.DecreaseIndent 5]
    | doDec (F.ExceptionDec {name = _, tagName, payloadTy = NONE}) =
        P.Fragment "exception " :: P.Fragment (TypedSyntax.print_VId tagName)
        :: []
    | doDec (F.ExceptionDec {name = _, tagName, payloadTy = SOME payloadTy}) =
        P.Fragment "exception " :: P.Fragment (TypedSyntax.print_VId tagName)
        :: P.Fragment " of " :: doTy 0 payloadTy
    | doDec (F.ESImportDec _) = [P.Fragment "_esImport"]
(* fun doDecs decs = List.concat (List.map (fn dec => P.Indent :: doDec dec @ [P.LineTerminator]) decs) *)
end
