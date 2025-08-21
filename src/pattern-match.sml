(*
 * Copyright (c) 2023 ARATA Mizuki
 * This file is part of LunarML.
 *)
(*
 * Check exhaustiveness and redundancy
 *
 * The algorithm is based on:
 *
 * * MARANGET, LUC. “Warnings for Pattern Matching.” Journal of Functional Programming, vol. 17, no. 3, 2007, pp. 387–421., doi:10.1017/S0956796807006223.
 *)
structure CheckPatternMatch :>
sig
  type Context =
    {options: LanguageOptions.options, messageHandler: Message.handler}
  val goDec: Context * FSyntax.Dec -> unit
end =
struct
  structure F = FSyntax
  structure Example =
  struct
    datatype example =
      ANY
    | SOME_INT
    | SOME_WORD
    | CHAR of char
    | CHAR16 of int
    | SOME_STRING
    | SOME_STRING16
    | RECORD of (Syntax.Label * example) list
    | VALCON of Syntax.VId * example option
    | SOME_EXNCON
    | VECTOR of example list
    fun toStringPrec (_, ANY) = "_"
      | toStringPrec (_, SOME_INT) = "<some integer>"
      | toStringPrec (_, SOME_WORD) = "<some word>"
      | toStringPrec (_, CHAR c) =
          "#\"" ^ Char.toString c ^ "\""
      | toStringPrec (_, CHAR16 i) =
          if i <= 127 then "#\"" ^ Char.toString (Char.chr i) ^ "\""
          else if i <= 0xff then "#\"\\u00" ^ Int.fmt StringCvt.HEX i ^ "\""
          else if i <= 0xfff then "#\"\\u0" ^ Int.fmt StringCvt.HEX i ^ "\""
          else "#\"\\u" ^ Int.fmt StringCvt.HEX i ^ "\""
      | toStringPrec (_, SOME_STRING) = "<some string>"
      | toStringPrec (_, SOME_STRING16) = "<some string>"
      | toStringPrec (_, RECORD fields) =
          let
            fun doTuple (_, [], acc) =
                  SOME (List.rev acc)
              | doTuple (i, (Syntax.NumericLabel j, x) :: fields, acc) =
                  if i = j then doTuple (i + 1, fields, x :: acc) else NONE
              | doTuple (_, _, _) = NONE
          in
            case doTuple (1, fields, []) of
              SOME elems =>
                "(" ^ String.concatWith ", " (List.map toString elems) ^ ")"
            | NONE =>
                "{"
                ^
                String.concatWith ", "
                  (List.map
                     (fn (Syntax.IdentifierLabel name, x) =>
                        name ^ "=" ^ toString x
                       | (Syntax.NumericLabel i, x) =>
                        Int.toString i ^ "=" ^ toString x) fields) ^ "}"
          end
      | toStringPrec (_, VALCON (vid, NONE)) = Syntax.getVIdName vid
      | toStringPrec (prec, VALCON (vid, SOME payload)) =
          if prec >= 1 then
            "(" ^ Syntax.getVIdName vid ^ " " ^ toStringPrec (1, payload) ^ ")"
          else
            Syntax.getVIdName vid ^ " " ^ toStringPrec (1, payload)
      | toStringPrec (_, SOME_EXNCON) = "<some exception>"
      | toStringPrec (_, VECTOR elems) =
          "#[" ^ String.concatWith "," (List.map toString elems) ^ "]"
    and toString x = toStringPrec (0, x)
  end
  structure CharSet = RedBlackSetFn (type ord_key = char; open Char)
  structure IntInfSet = RedBlackSetFn (type ord_key = IntInf.int; open IntInf)
  structure ConstructorSet =
  struct
    datatype set =
      VALCON of
        { seen: Syntax.VIdSet.set
        , all: Syntax.VIdSet.set
        , withPayload: Syntax.VIdSet.set
        }
    | EXNCON
    | INT of (* seen *) IntInfSet.set
    | WORD of (* seen *) IntInfSet.set
    | CHAR of (* seen *) CharSet.set
    | CHAR16 of (* seen *) IntRedBlackSet.set
    | STRING
    | STRING16
    | RECORD of Syntax.LabelSet.set
    | VECTOR of IntRedBlackSet.set * int option
    | EMPTY
    fun addValCon
          ( VALCON {seen, all, withPayload}
          , { tag
            , allConstructors
            , constructorsWithPayload = _
            , representation = _
            }
          , hasPayload
          ) =
          let
            val tag = Syntax.MkVId tag
          in
            VALCON
              { seen = Syntax.VIdSet.add (seen, tag)
              , all = Syntax.VIdSet.union (all, allConstructors)
              , withPayload =
                  if hasPayload then Syntax.VIdSet.add (withPayload, tag)
                  else withPayload
              }
          end
      | addValCon
          ( EMPTY
          , {tag, allConstructors, constructorsWithPayload, representation = _}
          , _
          ) =
          let
            val tag = Syntax.MkVId tag
          in
            VALCON
              { seen = Syntax.VIdSet.singleton tag
              , all = allConstructors
              , withPayload = constructorsWithPayload
              }
          end
      | addValCon _ =
          raise Fail "invalid pattern: value constructor"
    fun addSCon (INT seen, F.IntegerConstant x) =
          INT (IntInfSet.add (seen, x))
      | addSCon (EMPTY, F.IntegerConstant x) =
          INT (IntInfSet.singleton x)
      | addSCon (WORD seen, F.WordConstant x) =
          WORD (IntInfSet.add (seen, x))
      | addSCon (EMPTY, F.WordConstant x) =
          WORD (IntInfSet.singleton x)
      | addSCon (CHAR seen, F.CharConstant x) =
          CHAR (CharSet.add (seen, x))
      | addSCon (EMPTY, F.CharConstant x) =
          CHAR (CharSet.singleton x)
      | addSCon (CHAR16 seen, F.Char16Constant x) =
          CHAR16 (IntRedBlackSet.add (seen, x))
      | addSCon (EMPTY, F.Char16Constant x) =
          CHAR16 (IntRedBlackSet.singleton x)
      | addSCon (set as STRING, F.StringConstant _) = set
      | addSCon (EMPTY, F.StringConstant _) = STRING
      | addSCon (set as STRING16, F.String16Constant _) = set
      | addSCon (EMPTY, F.String16Constant _) = STRING16
      | addSCon _ = raise Fail "invalid pattern: scon"
    fun addExnCon (set as EXNCON) = set
      | addExnCon EMPTY = EXNCON
      | addExnCon _ = raise Fail "invalid pattern: exn"
    fun addRecord (set as RECORD fields, fields') =
          if Syntax.LabelSet.equal (fields, fields') then set
          else raise Fail "invalid pattern: record field"
      | addRecord (EMPTY, fields) = RECORD fields
      | addRecord _ = raise Fail "invalid pattern: record"
    fun addVector (VECTOR (seen, allAbove), n, false) =
          VECTOR (IntRedBlackSet.add (seen, n), allAbove)
      | addVector (VECTOR (seen, NONE), n, true) =
          VECTOR (seen, SOME n)
      | addVector (VECTOR (seen, SOME m), n, true) =
          VECTOR (seen, SOME (Int.min (m, n)))
      | addVector (EMPTY, n, false) =
          VECTOR (IntRedBlackSet.singleton n, NONE)
      | addVector (EMPTY, n, true) =
          VECTOR (IntRedBlackSet.empty, SOME n)
      | addVector _ = raise Fail "invalid pattern"
  (* fun isComplete (VALCON { seen, all, withPayload = _ }) = Syntax.VIdSet.equal (seen, all)
    | isComplete EXNCON = false
    | isComplete (INT _) = false (* TODO *)
    | isComplete (WORD _) = false (* TODO *)
    | isComplete (CHAR seen) = CharSet.numItems seen = 256
    | isComplete (CHAR16 seen) = IntRedBlackSet.numItems seen = 65536
    | isComplete STRING = false
    | isComplete STRING16 = false
    | isComplete (RECORD _) = true
    | isComplete (VECTOR (_, NONE)) = false
    | isComplete (VECTOR (seen, SOME n)) = let fun loop i = if i >= n then
                                                                true
                                                            else
                                                                IntRedBlackSet.member (seen, i) andalso loop (i + 1)
                                           in loop 0
                                           end
    | isComplete EMPTY = false *)
  end (* structure ConstructorSet *)
  fun collectConstructors (F.WildcardPat _, acc) = acc
    | collectConstructors (F.SConPat {scon, ...}, acc) =
        ConstructorSet.addSCon (acc, scon)
    | collectConstructors (F.VarPat _, acc) = acc
    | collectConstructors
        (F.RecordPat {sourceSpan = _, fields = _, ellipsis = _, allFields}, acc) =
        ConstructorSet.addRecord (acc, allFields)
    | collectConstructors (F.ValConPat {sourceSpan = _, info, payload}, acc) =
        ConstructorSet.addValCon (acc, info, Option.isSome payload)
    | collectConstructors
        (F.ExnConPat {sourceSpan = _, predicate = _, payload = _}, acc) =
        ConstructorSet.addExnCon acc
    | collectConstructors (F.LayeredPat (_, _, _, innerPat), acc) =
        collectConstructors (innerPat, acc)
    | collectConstructors (F.VectorPat (_, pats, ellipsis, _), acc) =
        ConstructorSet.addVector (acc, Vector.length pats, ellipsis)
    | collectConstructors (F.BogusPat _, acc) = acc
  fun specializeValCon (con, hasPayload) =
    let
      fun goPat (F.WildcardPat span, ps) =
            if hasPayload then [F.WildcardPat span :: ps] else [ps]
        | goPat (F.SConPat _, _) = [] (* should not occur *)
        | goPat (F.VarPat (span, _, _), ps) =
            if hasPayload then [F.WildcardPat span :: ps] else [ps]
        | goPat (F.RecordPat _, _) = [] (* should not occur *)
        | goPat
            ( F.ValConPat {sourceSpan = _, info = {tag, ...}, payload = NONE}
            , ps
            ) =
            if Syntax.MkVId tag = con andalso not hasPayload then [ps] else []
        | goPat
            ( F.ValConPat
                {sourceSpan = _, info = {tag, ...}, payload = SOME (_, pat)}
            , ps
            ) =
            if Syntax.MkVId tag = con andalso hasPayload then [pat :: ps]
            else []
        | goPat (F.ExnConPat _, _) = [] (* should not occur *)
        | goPat (F.LayeredPat (_, _, _, pat), ps) = goPat (pat, ps)
        | goPat (F.VectorPat _, _) = [] (* should not occur *)
        | goPat (F.BogusPat _, _) = []
      fun goMatrix [] = []
        | goMatrix ((p :: ps) :: rest) =
            goPat (p, ps) @ goMatrix rest
        | goMatrix _ = raise Fail "invalid pattern matrix"
    in
      goMatrix
    end
  fun sameExp (F.VarExp x, F.VarExp y) = x = y
    | sameExp
        ( F.ProjectionExp {label, record, fieldTypes = _}
        , F.ProjectionExp {label = label', record = record', fieldTypes = _}
        ) =
        label = label' andalso sameExp (record, record')
    | sameExp _ = false
  fun specializeExnCon (predicate, hasPayload) =
    let
      fun goPat (F.WildcardPat span, ps) =
            if hasPayload then [F.WildcardPat span :: ps] else [ps]
        | goPat (F.SConPat _, _) = [] (* should not occur *)
        | goPat (F.VarPat (span, _, _), ps) =
            if hasPayload then [F.WildcardPat span :: ps] else [ps]
        | goPat (F.RecordPat _, _) = [] (* should not occur *)
        | goPat (F.ValConPat _, _) = [] (* should not occur *)
        | goPat
            (F.ExnConPat {sourceSpan = _, predicate = p, payload = NONE}, ps) =
            if sameExp (predicate, p) andalso not hasPayload then [ps] else []
        | goPat
            ( F.ExnConPat
                {sourceSpan = _, predicate = p, payload = SOME (_, _, pat)}
            , ps
            ) =
            if sameExp (predicate, p) andalso hasPayload then [pat :: ps]
            else []
        | goPat (F.LayeredPat (_, _, _, pat), ps) = goPat (pat, ps)
        | goPat (F.VectorPat _, _) = [] (* should not occur *)
        | goPat (F.BogusPat _, _) = []
      fun goMatrix [] = []
        | goMatrix ((p :: ps) :: rest) =
            goPat (p, ps) @ goMatrix rest
        | goMatrix _ = raise Fail "invalid pattern matrix"
    in
      goMatrix
    end
  fun specializeSCon scon =
    let
      fun goPat (F.WildcardPat _, ps) = [ps]
        | goPat (F.SConPat {scon = scon', ...}, ps) =
            if scon' = scon then [ps] else []
        | goPat (F.VarPat (_, _, _), ps) = [ps]
        | goPat (F.RecordPat _, _) = [] (* should not occur *)
        | goPat (F.ValConPat _, _) = [] (* should not occur *)
        | goPat (F.ExnConPat _, _) = [] (* should not occur *)
        | goPat (F.LayeredPat (_, _, _, pat), ps) = goPat (pat, ps)
        | goPat (F.VectorPat _, _) = [] (* should not occur *)
        | goPat (F.BogusPat _, _) = []
      fun goMatrix [] = []
        | goMatrix ((p :: ps) :: rest) =
            goPat (p, ps) @ goMatrix rest
        | goMatrix _ = raise Fail "invalid pattern matrix"
    in
      goMatrix
    end
  fun getFieldFromList (label, fields, ellipsis) =
    let
      fun loop [] =
            (case ellipsis of
               NONE => raise Fail "invalid pattern: missing field"
             | SOME ellipsisPat => getFieldFromPat (label, ellipsisPat))
        | loop ((label', pat) :: rest) =
            if label = label' then pat else loop rest
    in
      loop fields
    end
  and getFieldFromPat (_, pat as F.WildcardPat _) = pat
    | getFieldFromPat (_, F.VarPat (span, _, _)) = F.WildcardPat span
    | getFieldFromPat
        (label, F.RecordPat {sourceSpan = _, fields, ellipsis, allFields = _}) =
        getFieldFromList (label, fields, ellipsis)
    | getFieldFromPat (label, F.LayeredPat (_, _, _, pat)) =
        getFieldFromPat (label, pat)
    | getFieldFromPat _ = raise Fail "invalid pattern: record"
  fun specializeRecord labels =
    let
      fun goPat (F.WildcardPat span, ps) =
            [List.tabulate (Syntax.LabelSet.numItems labels, fn _ =>
               F.WildcardPat span) @ ps]
        | goPat (F.SConPat _, _) = [] (* should not occur *)
        | goPat (F.VarPat (span, _, _), ps) =
            [List.tabulate (Syntax.LabelSet.numItems labels, fn _ =>
               F.WildcardPat span) @ ps]
        | goPat (F.RecordPat {sourceSpan = _, fields, ellipsis, allFields}, ps) =
            if Syntax.LabelSet.equal (labels, allFields) then
              [Syntax.LabelSet.foldr
                 (fn (label, ps) =>
                    getFieldFromList (label, fields, ellipsis) :: ps) ps labels]
            else
              []
        | goPat (F.ValConPat _, _) = [] (* should not occur *)
        | goPat (F.ExnConPat _, _) = [] (* should not occur *)
        | goPat (F.LayeredPat (_, _, _, pat), ps) = goPat (pat, ps)
        | goPat (F.VectorPat _, _) = [] (* should not occur *)
        | goPat (F.BogusPat _, _) = []
      fun goMatrix [] = []
        | goMatrix ((p :: ps) :: rest) =
            goPat (p, ps) @ goMatrix rest
        | goMatrix _ = raise Fail "invalid pattern matrix"
    in
      goMatrix
    end
  fun specializeVector n =
    let
      fun goPat (F.WildcardPat span, ps) =
            [List.tabulate (n, fn _ => F.WildcardPat span) @ ps]
        | goPat (F.SConPat _, _) = [] (* should not occur *)
        | goPat (F.VarPat (span, _, _), ps) =
            [List.tabulate (n, fn _ => F.WildcardPat span) @ ps]
        | goPat (F.RecordPat _, _) = [] (* should not occur *)
        | goPat (F.ValConPat _, _) = [] (* should not occur *)
        | goPat (F.ExnConPat _, _) = [] (* should not occur *)
        | goPat (F.LayeredPat (_, _, _, pat), ps) = goPat (pat, ps)
        | goPat (F.VectorPat (span, pats, ellipsis, _), ps) =
            let
              val m = Vector.length pats
            in
              if m = n then
                [Vector.foldr (op::) ps pats]
              else if m < n andalso ellipsis then
                [Vector.foldr (op::)
                   (List.tabulate (n - m, fn _ => F.WildcardPat span) @ ps) pats]
              else
                []
            end
        | goPat (F.BogusPat _, _) = []
      fun goMatrix [] = []
        | goMatrix ((p :: ps) :: rest) =
            goPat (p, ps) @ goMatrix rest
        | goMatrix _ = raise Fail "invalid pattern matrix"
    in
      goMatrix
    end
  local structure C = ConstructorSet
  in
    datatype completeness =
      COMPLETE of
        ((F.Pat list list -> F.Pat list list)
         * (* arity *) int
         * (Example.example list -> Example.example)) list
    | INCOMPLETE of Example.example option
    fun isComplete (C.VALCON {seen, all, withPayload}) =
          (let
             val x = Syntax.VIdSet.minItem
               (Syntax.VIdSet.difference (all, seen))
             val payload =
               if Syntax.VIdSet.member (withPayload, x) then SOME Example.ANY
               else NONE
           in
             INCOMPLETE (SOME (Example.VALCON (x, payload)))
           end
           handle List.Empty =>
             COMPLETE
               (Syntax.VIdSet.foldr
                  (fn (vid, xs) =>
                     let
                       val hasPayload = Syntax.VIdSet.member (withPayload, vid)
                       val (arity, construct) =
                         if hasPayload then
                           ( 1
                           , fn [x] => Example.VALCON (vid, SOME x)
                              | _ => raise Fail "invalid payload"
                           )
                         else
                           ( 0
                           , fn [] => Example.VALCON (vid, NONE)
                              | _ => raise Fail "invalid payload"
                           )
                     in
                       (specializeValCon (vid, hasPayload), arity, construct)
                       :: xs
                     end) [] all))
      | isComplete C.EXNCON =
          INCOMPLETE (SOME Example.SOME_EXNCON)
      | isComplete (C.INT _) =
          INCOMPLETE (SOME Example.SOME_INT) (* TODO *)
      | isComplete (C.WORD _) =
          INCOMPLETE (SOME Example.SOME_WORD) (* TODO *)
      | isComplete (C.CHAR seen) =
          let
            fun loop i =
              if i >= 256 then
                COMPLETE (List.tabulate (256, fn i =>
                  ( specializeSCon (F.CharConstant (chr i))
                  , 0
                  , fn [] => Example.CHAR (chr i)
                     | _ => raise Fail "invalid payload"
                  )))
              else
                let
                  val c = Char.chr i
                in
                  if CharSet.member (seen, c) then loop (i + 1)
                  else INCOMPLETE (SOME (Example.CHAR (chr i)))
                end
          in
            loop 0
          end
      | isComplete (C.CHAR16 seen) =
          let
            fun loop i =
              if i >= 65536 then
                COMPLETE (List.tabulate (65536, fn i =>
                  ( specializeSCon (F.Char16Constant i)
                  , 0
                  , fn [] => Example.CHAR16 i
                     | _ => raise Fail "invalid payload"
                  )))
              else if IntRedBlackSet.member (seen, i) then
                loop (i + 1)
              else
                INCOMPLETE (SOME (Example.CHAR16 i))
          in
            loop 0
          end
      | isComplete C.STRING =
          INCOMPLETE (SOME Example.SOME_STRING)
      | isComplete C.STRING16 =
          INCOMPLETE (SOME Example.SOME_STRING16)
      | isComplete (C.RECORD labels) =
          let
            fun construct fields =
              Example.RECORD
                (ListPair.zipEq (Syntax.LabelSet.toList labels, fields))
          in
            COMPLETE
              [( specializeRecord labels
               , Syntax.LabelSet.numItems labels
               , construct
               )]
          end
      | isComplete (C.VECTOR (seen, NONE)) =
          let
            fun loop i =
              if IntRedBlackSet.member (seen, i) then
                loop (i + 1)
              else
                INCOMPLETE (SOME (Example.VECTOR
                  (List.tabulate (i, fn _ => Example.ANY))))
          in
            loop 0
          end
      | isComplete (C.VECTOR (seen, SOME n)) =
          let
            fun loop i =
              if i >= n then
                COMPLETE (List.tabulate (i, fn j =>
                  (specializeVector j, j, Example.VECTOR)))
              else if IntRedBlackSet.member (seen, i) then
                loop (i + 1)
              else
                INCOMPLETE (SOME (Example.VECTOR
                  (List.tabulate (i, fn _ => Example.ANY))))
          in
            loop 0
          end
      | isComplete C.EMPTY = INCOMPLETE NONE
  end
  local
    fun defaultMatrix' ([], acc) = List.rev acc
      | defaultMatrix' ([] :: _, acc) = List.rev acc
      | defaultMatrix' ((F.WildcardPat _ :: ps) :: matrix, acc) =
          defaultMatrix' (matrix, ps :: acc)
      | defaultMatrix' ((F.SConPat _ :: _) :: matrix, acc) =
          defaultMatrix' (matrix, acc) (* no row *)
      | defaultMatrix' ((F.VarPat _ :: ps) :: matrix, acc) =
          defaultMatrix' (matrix, ps :: acc)
      | defaultMatrix' ((F.RecordPat _ :: _) :: matrix, acc) =
          defaultMatrix' (matrix, acc) (* no row *)
      | defaultMatrix' ((F.ValConPat _ :: _) :: matrix, acc) =
          defaultMatrix' (matrix, acc) (* no row *)
      | defaultMatrix' ((F.ExnConPat _ :: _) :: matrix, acc) =
          defaultMatrix' (matrix, acc) (* no row *)
      | defaultMatrix' ((F.LayeredPat (_, _, _, innerPat) :: ps) :: matrix, acc) =
          defaultMatrix' ((innerPat :: ps) :: matrix, acc)
      | defaultMatrix' ((F.VectorPat _ :: _) :: matrix, acc) =
          defaultMatrix' (matrix, acc) (* no row *)
      | defaultMatrix' ((F.BogusPat _ :: _) :: matrix, acc) =
          defaultMatrix' (matrix, acc) (* is this ok? *)
  in
    fun defaultMatrix matrix = defaultMatrix' (matrix, [])
  end
  fun nonMatching ([], n) : (Example.example list) option =
        SOME (List.tabulate (n, fn _ => Example.ANY))
    | nonMatching (_ :: _, 0) = NONE
    | nonMatching (matrix, n) =
        let
          val firsts = List.map List.hd matrix
          val constructors =
            List.foldl collectConstructors ConstructorSet.EMPTY firsts
        in
          case isComplete constructors of
            COMPLETE specializers =>
              let
                fun go [] = NONE
                  | go ((specialize, arity, construct) :: ss) =
                      case nonMatching (specialize matrix, arity + n - 1) of
                        SOME rs =>
                          let val (rs0, rs1) = ListUtil.splitAt (rs, arity)
                          in SOME (construct rs0 :: rs1)
                          end
                      | NONE => go ss
              in
                go specializers
              end
          | INCOMPLETE example =>
              (case nonMatching (defaultMatrix matrix, n - 1) of
                 NONE => NONE
               | SOME ps =>
                   (case example of
                      NONE => SOME (Example.ANY :: ps)
                    | SOME x => SOME (x :: ps)))
        end
  (*: val useful : (* matrix *) F.Pat list list * (* vector *) F.Pat list -> bool *)
  fun useful ([], _) = true
    | useful (_ (* should be ([] :: _) *), []) = false
    | useful (matrix, q :: qs) =
        let
          fun wildcard () =
            let
              val firsts = List.map List.hd matrix
              val constructors =
                List.foldl collectConstructors ConstructorSet.EMPTY firsts
            in
              case isComplete constructors of
                COMPLETE specializers =>
                  List.exists
                    (fn (specialize, arity, _) =>
                       useful
                         ( specialize matrix
                         , List.tabulate (arity, fn _ =>
                             F.WildcardPat SourcePos.nullSpan) @ qs
                         )) specializers
              | INCOMPLETE _ => useful (defaultMatrix matrix, qs)
            end
          fun goPat (F.WildcardPat _) = wildcard ()
            | goPat (F.SConPat {scon, ...}) =
                useful (specializeSCon scon matrix, qs)
            | goPat (F.VarPat _) = wildcard ()
            | goPat (F.RecordPat {sourceSpan = _, fields, ellipsis, allFields}) =
                useful
                  ( specializeRecord allFields matrix
                  , Syntax.LabelSet.foldr
                      (fn (label, qs) =>
                         getFieldFromList (label, fields, ellipsis) :: qs) qs
                      allFields
                  )
            | goPat
                (F.ValConPat {sourceSpan = _, info = {tag, ...}, payload = NONE}) =
                useful (specializeValCon (Syntax.MkVId tag, false) matrix, qs)
            | goPat
                (F.ValConPat
                   { sourceSpan = _
                   , info = {tag, ...}
                   , payload = SOME (_, innerPat)
                   }) =
                useful
                  ( specializeValCon (Syntax.MkVId tag, true) matrix
                  , innerPat :: qs
                  )
            | goPat (F.ExnConPat {sourceSpan = _, predicate, payload = NONE}) =
                useful (specializeExnCon (predicate, false) matrix, qs)
            | goPat
                (F.ExnConPat
                   {sourceSpan = _, predicate, payload = SOME (_, _, innerPat)}) =
                useful
                  (specializeExnCon (predicate, true) matrix, innerPat :: qs)
            | goPat (F.LayeredPat (_, _, _, pat)) = goPat pat
            | goPat (F.VectorPat (_, pats, _, _)) =
                useful
                  ( specializeVector (Vector.length pats) matrix
                  , Vector.foldr (op::) qs pats
                  )
            | goPat (F.BogusPat _) = true (* is this ok? *)
        in
          goPat q
        end
  datatype message_type = WARNING | ERROR
  type Context =
    {options: LanguageOptions.options, messageHandler: Message.handler}
  fun emitWarningOrError (ctx: Context, spans, message, WARNING) =
        Message.warning (#messageHandler ctx, spans, "code generator", message)
    | emitWarningOrError (ctx, spans, message, ERROR) =
        Message.error (#messageHandler ctx, spans, "code generator", message)
  fun checkExhaustiveness (ctx, span, matches, mtype) =
    let
      val matrix = List.map (fn (pat, _) => [pat]) matches
    in
      case nonMatching (matrix, 1) of
        NONE => () (* exhaustive *)
      | SOME [example] =>
          emitWarningOrError
            ( ctx
            , [span]
            , "pattern match is non-exhaustive. Example of non-matching value: "
              ^ Example.toString example
            , mtype
            ) (* non-exhaustive *)
      | SOME _ => raise Fail "invalid number of examples"
    end
  fun checkRedundancy (ctx, matches, mtype) =
    let (* val matrix = List.map (fn (pat, _) => [pat]) matches *)
      fun go ([], _) = ()
        | go ((pat, _) :: matches, seen) =
            if useful (seen, [pat]) then (* the order of matrix (seen) is reversed, but should not affect usefulness *)
              go (matches, [pat] :: seen)
            else
              emitWarningOrError
                ( ctx
                , [F.getSourceSpanOfPat pat]
                , "redundant pattern found"
                , mtype
                )
    in
      go (matches, [])
    end
  fun goExp (ctx, F.PrimExp (_, _, exps)) =
        List.app (fn x => goExp (ctx, x)) exps
    | goExp (_, F.VarExp _) = ()
    | goExp (ctx, F.RecordExp fields) =
        List.app (fn (_, exp) => goExp (ctx, exp)) fields
    | goExp (ctx, F.LetExp (decs, exp)) =
        (List.app (fn dec => goDec (ctx, dec)) decs; goExp (ctx, exp))
    | goExp (ctx, F.MultiAppExp (f, args)) =
        (goExp (ctx, f); List.app (fn arg => goExp (ctx, arg)) args)
    | goExp (ctx, F.HandleExp {body, exnName = _, handler, resultTy = _}) =
        (goExp (ctx, body); goExp (ctx, handler))
    | goExp (ctx, F.IfThenElseExp (x, y, z)) =
        (goExp (ctx, x); goExp (ctx, y); goExp (ctx, z))
    | goExp
        ( ctx
        , F.CaseExp
            { sourceSpan
            , subjectExp
            , subjectTy = _
            , matches
            , matchType
            , resultTy = _
            }
        ) =
        ( goExp (ctx, subjectExp)
        ; List.app (fn (_, exp) => goExp (ctx, exp)) matches
        ; let
            val opt =
              case matchType of
                TypedSyntax.CASE => #nonexhaustiveMatch (#options ctx)
              | TypedSyntax.VAL => #nonexhaustiveBind (#options ctx)
              | TypedSyntax.HANDLE => #nonexhaustiveRaise (#options ctx)
          in
            case opt of
              LanguageOptions.IGNORE => ()
            | LanguageOptions.WARN =>
                checkExhaustiveness (ctx, sourceSpan, matches, WARNING)
            | LanguageOptions.ERROR =>
                checkExhaustiveness (ctx, sourceSpan, matches, ERROR)
          end
        ; let
            val opt =
              case matchType of
                TypedSyntax.CASE => #redundantMatch (#options ctx)
              | TypedSyntax.VAL => #redundantBind (#options ctx)
              | TypedSyntax.HANDLE => #redundantRaise (#options ctx)
          in
            case opt of
              LanguageOptions.IGNORE => ()
            | LanguageOptions.WARN => checkRedundancy (ctx, matches, WARNING)
            | LanguageOptions.ERROR => checkRedundancy (ctx, matches, ERROR)
          end
        )
    | goExp (ctx, F.MultiFnExp (_, body)) = goExp (ctx, body)
    | goExp (ctx, F.ProjectionExp {label = _, record, fieldTypes = _}) =
        goExp (ctx, record)
    | goExp (ctx, F.TyAbsExp (_, _, exp)) = goExp (ctx, exp)
    | goExp (ctx, F.TyAppExp (exp, _)) = goExp (ctx, exp)
    | goExp (ctx, F.PackExp {payloadTy = _, exp, packageTy = _}) =
        goExp (ctx, exp)
    | goExp (_, F.BogusExp _) = ()
    | goExp (_, F.ExitProgram) = ()
    | goExp (ctx, F.ExportValue exp) = goExp (ctx, exp)
    | goExp (ctx, F.ExportModule fields) =
        Vector.app (fn (_, exp) => goExp (ctx, exp)) fields
  and goDec (ctx, F.ValDec (_, _, exp)) = goExp (ctx, exp)
    | goDec (ctx, F.RecValDec decs) =
        List.app (fn (_, _, exp) => goExp (ctx, exp)) decs
    | goDec (ctx, F.UnpackDec (_, _, _, _, exp)) = goExp (ctx, exp)
    | goDec (ctx, F.IgnoreDec exp) = goExp (ctx, exp)
    | goDec (_, F.DatatypeDec _) = ()
    | goDec (_, F.ExceptionDec _) = ()
    | goDec (_, F.ESImportDec _) = ()
end; (* structure CheckPatternMatch *)
