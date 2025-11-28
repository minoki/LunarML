(*
 * Copyright (c) 2023 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure CodeGenLua :>
sig
  exception CodeGenError of string
  datatype target_lua_version = LUA5_3 | LUAJIT
  type Context =
    { nextLuaId: int ref
    , targetLuaVersion: target_lua_version
    , hasDelimitedContinuations: bool
    }
  val doProgram: Context -> CSyntax.CVar -> NSyntax.Stat -> LuaSyntax.Block
  val doProgramWithContinuations: Context
                                  -> CSyntax.CVar
                                  -> NSyntax.Stat
                                  -> LuaSyntax.Block
end =
struct
  exception CodeGenError of string

  datatype target_lua_version = LUA5_3 | LUAJIT
  type Context =
    { nextLuaId: int ref
    , targetLuaVersion: target_lua_version
    , hasDelimitedContinuations: bool
    }

  val builtins =
    let
      open InitialEnv
    in
      List.foldl
        (fn ((vid, name), map) => TypedSyntax.VIdMap.insert (map, vid, name))
        TypedSyntax.VIdMap.empty
        [ (* exn *) (VId_Match, "_Match")
        , (VId_Bind, "_Bind")
        , (VId_Div, "_Div")
        , (VId_Overflow, "_Overflow")
        , (VId_Size, "_Size")
        , (VId_Subscript, "_Subscript")
        , (VId_Fail, "_Fail")
        , (VId_Match_tag, "_Match_tag")
        , (VId_Bind_tag, "_Bind_tag")
        , (VId_Div_tag, "_Div_tag")
        , (VId_Overflow_tag, "_Overflow_tag")
        , (VId_Size_tag, "_Size_tag")
        , (VId_Subscript_tag, "_Subscript_tag")
        , (VId_Fail_tag, "_Fail_tag")
        , (VId_Match_predicate, "_isMatch")
        , (VId_Bind_predicate, "_isBind")
        , (VId_Div_predicate, "_isDiv")
        , (VId_Overflow_predicate, "_isOverflow")
        , (VId_Size_predicate, "_isSize")
        , (VId_Subscript_predicate, "_isSubscript")
        , (VId_Fail_predicate, "_isFail")
        , (VId_Fail_payload, "_Fail_payload")
        , (VId_exnName, "_exnName")
        (* Overloaded: VId_abs, VId_TILDE, VId_div, VId_mod, VId_TIMES, VId_DIVIDE, VId_PLUS, VId_MINUS, VId_LT, VId_GT, VId_LE, VId_GE *)
        (* int *)
        , (VId_Int_abs, "_Int_abs") (* may raise Overflow *)
        , (VId_Int_TILDE, "_Int_negate") (* may raise Overflow *)
        (* real *)
        , (VId_Real_abs, "math_abs") (* Lua math.abs *)
        (* Vector and Array *)
        , (VId_Vector_tabulate, "_VectorOrArray_tabulate")
        , (VId_Vector_concat, "_Vector_concat")
        , (VId_Vector_fromList, "_VectorOrArray_fromList")
        , (VId_Array_fromList, "_VectorOrArray_fromList")
        , (VId_Array_tabulate, "_VectorOrArray_tabulate")
        (* Delimited continuations *)
        , (VId_DelimCont_pushPrompt, "_pushPrompt")
        , (VId_DelimCont_withSubCont, "_withSubCont")
        , (VId_DelimCont_pushSubCont, "_pushSubCont")
        (* Lua interface *)
        , (VId_Lua_Error, "_id")
        , (VId_Lua_Error_predicate, "_isError")
        , (VId_Lua_Error_payload, "_id")
        , (VId_Lua_NIL, "nil") (* literal *)
        , (VId_Lua_function, "_Lua_function")
        , (VId_Lua_newTableWith, "_newTableWith")
        , (VId_Lua_Lib_assert, "assert")
        , (VId_Lua_Lib_error, "error")
        , (VId_Lua_Lib_getmetatable, "getmetatable")
        , (VId_Lua_Lib_pairs, "pairs")
        , (VId_Lua_Lib_pcall, "pcall")
        , (VId_Lua_Lib_setmetatable, "setmetatable")
        , (VId_Lua_Lib_math, "math")
        , (VId_Lua_Lib_math_abs, "math_abs")
        , (VId_Lua_Lib_math_type, "math_type")
        , (VId_Lua_Lib_math_maxinteger, "math_maxinteger")
        , (VId_Lua_Lib_math_mininteger, "math_mininteger")
        , (VId_Lua_Lib_math_ult, "math_ult")
        , (VId_Lua_Lib_string, "string")
        , (VId_Lua_Lib_string_char, "string_char")
        , (VId_Lua_Lib_string_format, "string_format")
        , (VId_Lua_Lib_table, "table")
        , (VId_Lua_Lib_table_concat, "table_concat")
        , (VId_Lua_Lib_table_pack, "table_pack")
        , (VId_Lua_Lib_table_unpack, "table_unpack")
        ]
    end
  val builtinsLuaJIT =
    let
      open InitialEnv
    in
      List.foldl
        (fn ((vid, name), map) => TypedSyntax.VIdMap.insert (map, vid, name))
        TypedSyntax.VIdMap.empty
        [ (* exn *) (VId_Match, "_Match")
        , (VId_Bind, "_Bind")
        , (VId_Div, "_Div")
        , (VId_Overflow, "_Overflow")
        , (VId_Size, "_Size")
        , (VId_Subscript, "_Subscript")
        , (VId_Fail, "_Fail")
        , (VId_Match_tag, "_Match_tag")
        , (VId_Bind_tag, "_Bind_tag")
        , (VId_Div_tag, "_Div_tag")
        , (VId_Overflow_tag, "_Overflow_tag")
        , (VId_Size_tag, "_Size_tag")
        , (VId_Subscript_tag, "_Subscript_tag")
        , (VId_Fail_tag, "_Fail_tag")
        , (VId_Match_predicate, "_isMatch")
        , (VId_Bind_predicate, "_isBind")
        , (VId_Div_predicate, "_isDiv")
        , (VId_Overflow_predicate, "_isOverflow")
        , (VId_Size_predicate, "_isSize")
        , (VId_Subscript_predicate, "_isSubscript")
        , (VId_Fail_predicate, "_isFail")
        , (VId_Fail_payload, "_Fail_payload")
        , (VId_exnName, "_exnName")
        (* Overloaded: VId_abs, VId_TILDE, VId_div, VId_mod, VId_TIMES, VId_DIVIDE, VId_PLUS, VId_MINUS, VId_LT, VId_GT, VId_LE, VId_GE *)
        (* int *)
        , (VId_Int_abs, "_Int54_abs") (* may raise Overflow *)
        , (VId_Int_TILDE, "_Int54_negate") (* may raise Overflow *)
        (* real *)
        , (VId_Real_abs, "math_abs") (* Lua math.abs *)
        (* Vector and Array *)
        , (VId_Vector_tabulate, "_VectorOrArray_tabulate")
        , (VId_Vector_concat, "_Vector_concat")
        , (VId_Vector_fromList, "_VectorOrArray_fromList")
        , (VId_Array_fromList, "_VectorOrArray_fromList")
        , (VId_Array_tabulate, "_VectorOrArray_tabulate")
        (* Lua interface *)
        , (VId_Lua_Error, "_id")
        , (VId_Lua_Error_predicate, "_isError")
        , (VId_Lua_Error_payload, "_id")
        , (VId_Lua_NIL, "nil") (* literal *)
        , (VId_Lua_function, "_Lua_function")
        , (VId_Lua_newTableWith, "_newTableWith")
        , (VId_Lua_Lib_assert, "assert")
        , (VId_Lua_Lib_error, "error")
        , (VId_Lua_Lib_getmetatable, "getmetatable")
        , (VId_Lua_Lib_pairs, "pairs")
        , (VId_Lua_Lib_pcall, "pcall")
        , (VId_Lua_Lib_setmetatable, "setmetatable")
        , (VId_Lua_Lib_math, "math")
        , (VId_Lua_Lib_math_abs, "math_abs")
        , (VId_Lua_Lib_string, "string")
        , (VId_Lua_Lib_string_char, "string_char")
        , (VId_Lua_Lib_string_format, "string_format")
        , (VId_Lua_Lib_table, "table")
        , (VId_Lua_Lib_table_concat, "table_concat")
        , (VId_Lua_Lib_table_pack, "table_pack")
        , (VId_Lua_Lib_table_unpack, "table_unpack")
        , (VId_Lua_Lib_bit, "bit")
        , (VId_Lua_Lib_bit_bnot, "bit_bnot")
        , (VId_Lua_Lib_bit_band, "bit_band")
        , (VId_Lua_Lib_bit_bor, "bit_bor")
        , (VId_Lua_Lib_bit_bxor, "bit_bxor")
        , (VId_Lua_Lib_bit_lshift, "bit_lshift")
        , (VId_Lua_Lib_bit_rshift, "bit_rshift")
        ]
    end
  fun VIdToLua (ctx: Context, vid as TypedSyntax.MkVId (name, n)) =
    if n < 0 then
      case #targetLuaVersion ctx of
        LUA5_3 =>
          (case TypedSyntax.VIdMap.find (builtins, vid) of
             NONE =>
               raise Fail
                 ("the built-in symbol "
                  ^ Syntax.SourceName.getStringWithDefault (name, "?") ^ "@"
                  ^ Int.toString n ^ " is not supported by Lua backend")
           | SOME luaName => LuaSyntax.PredefinedId luaName)
      | LUAJIT =>
          (case TypedSyntax.VIdMap.find (builtinsLuaJIT, vid) of
             NONE =>
               raise Fail
                 ("the built-in symbol "
                  ^ Syntax.SourceName.getStringWithDefault (name, "?") ^ "@"
                  ^ Int.toString n ^ " is not supported by LuaJIT backend")
           | SOME luaName => LuaSyntax.PredefinedId luaName)
    else
      LuaSyntax.UserDefinedId vid

  fun LabelToTableKey (Syntax.NumericLabel n) = LuaSyntax.IntKey n
    | LabelToTableKey (Syntax.IdentifierLabel s) = LuaSyntax.StringKey s

  fun genSym (ctx: Context) =
    let
      val n = !(#nextLuaId ctx)
      val _ = #nextLuaId ctx := n + 1
    in
      TypedSyntax.MkVId (Syntax.SourceName.fromString "tmp", n)
    end
  fun genSymWithName (ctx: Context, name: string) =
    let
      val n = !(#nextLuaId ctx)
      val _ = #nextLuaId ctx := n + 1
    in
      TypedSyntax.MkVId (Syntax.SourceName.fromString name, n)
    end

  structure F = FSyntax
  structure C = CSyntax
  structure N = NSyntax
  structure L =
  struct
    fun ConstStat (vid: TypedSyntax.VId, e: LuaSyntax.Exp) =
      LuaSyntax.LocalStat ([(vid, LuaSyntax.CONST)], [e])
    open LuaSyntax
  end

  fun ExpStat (L.CallExp (f, args)) =
        [L.CallStat (f, args)]
    | ExpStat (L.MethodExp (self, name, args)) =
        [L.MethodStat (self, name, args)]
    | ExpStat (L.ConstExp _) = []
    | ExpStat e =
        [L.CallStat (L.VarExp (L.PredefinedId "_id"), vector [e])]

  fun TableUnpackN (x as L.VarExp _) =
        L.CallExp (L.VarExp (L.PredefinedId "table_unpack"), vector
          [ x
          , L.ConstExp (L.Numeral "1")
          , L.IndexExp (x, L.ConstExp (L.LiteralString "n"))
          ])
    | TableUnpackN (x as L.CallExp (L.VarExp (L.PredefinedId "table_pack"), a)) =
        if Vector.length a = 1 then Vector.sub (a, 0)
        else L.CallExp (L.VarExp (L.PredefinedId "table_unpackN"), vector [x])
    | TableUnpackN x =
        L.CallExp (L.VarExp (L.PredefinedId "table_unpackN"), vector [x])

  fun MethodExp (obj, name, args) =
    let
      val name' =
        case name of
          L.ConstExp (L.LiteralString name) =>
            if LuaWriter.isLuaIdentifier name then SOME name else NONE
        | _ => NONE
    in
      case name' of
        SOME name => L.MethodExp (obj, name, Vector.fromList args)
      | NONE =>
          case obj of
            L.VarExp _ =>
              L.CallExp (L.IndexExp (obj, name), vector (obj :: args))
          | _ =>
              L.CallExp (L.VarExp (L.PredefinedId "_method"), vector
                (obj :: name :: args))
    end

  fun MethodStat (obj, name, args) =
    let
      val name' =
        case name of
          L.ConstExp (L.LiteralString name) =>
            if LuaWriter.isLuaIdentifier name then SOME name else NONE
        | _ => NONE
    in
      case name' of
        SOME name => L.MethodStat (obj, name, Vector.fromList args)
      | NONE =>
          case obj of
            L.VarExp _ =>
              L.CallStat (L.IndexExp (obj, name), vector (obj :: args))
          | _ =>
              L.CallStat (L.VarExp (L.PredefinedId "_method"), vector
                (obj :: name :: args))
    end

  datatype cont_type =
    GOTO of {label: TypedSyntax.VId, params: (L.Id option) list}
  | RETURN
  type Env = {continuations: cont_type C.CVarMap.map}

  datatype prim_effect = datatype Primitives.prim_effect

  fun applyCont
    ( _: Context
    , env: Env
    , defaultCont: C.CVar option
    , cont: C.CVar
    , args: L.Exp list
    ) =
    case C.CVarMap.find (#continuations env, cont) of
      SOME (GOTO {label, params = []}) =>
        if defaultCont = SOME cont then [] else [L.GotoStat label]
    | SOME (GOTO {label, params}) =>
        let
          val (params', args') =
            ListPair.foldrEq
              (fn (SOME p, a, (pp, aa)) => (p :: pp, a :: aa)
                | (NONE, _, acc) => acc) ([], []) (params, args)
        in
          if defaultCont = SOME cont then L.MultiAssignStat (params', args')
          else L.MultiAssignStat (params', args') @ [L.GotoStat label]
        end
    | SOME RETURN =>
        if List.null args andalso defaultCont = SOME cont then []
        else [L.ReturnStat (vector args)]
    | NONE => raise CodeGenError "undefined continuation"

  fun doLabel cname =
    TypedSyntax.MkVId (Syntax.SourceName.fromString "cont", C.CVar.toInt cname)

  fun doValue ctx (C.Var vid) =
        (case VIdToLua (ctx, vid) of
           L.PredefinedId "nil" => L.ConstExp L.Nil
         | L.PredefinedId "false" => L.ConstExp L.False
         | L.PredefinedId "true" => L.ConstExp L.True
         | id => L.VarExp id)
    | doValue _ C.Unit = L.ConstExp L.Nil
    | doValue _ C.Nil = L.ConstExp L.Nil (* empty list *)
    | doValue _ (C.TypedNil _) = L.ConstExp L.Nil (* empty list *)
    | doValue _ (C.BoolConst false) = L.ConstExp L.False
    | doValue _ (C.BoolConst true) = L.ConstExp L.True
    | doValue _ (C.IntConst (Primitives.INT, x)) =
        if x < 0 then
          if x = ~0x8000000000000000 then
            L.BinExp
              ( L.MINUS
              , L.UnaryExp (L.NEGATE, L.ConstExp (L.Numeral (LargeInt.toString
                  (~(x + 1)))))
              , L.ConstExp (L.Numeral "1")
              )
          else
            L.UnaryExp (L.NEGATE, L.ConstExp (L.Numeral
              (LargeInt.toString (~x))))
        else
          L.ConstExp (L.Numeral (LargeInt.toString x))
    | doValue _ (C.IntConst (Primitives.I32, x)) =
        if x < 0 then
          L.UnaryExp (L.NEGATE, L.ConstExp (L.Numeral (LargeInt.toString (~x))))
        else
          L.ConstExp (L.Numeral (LargeInt.toString x))
    | doValue _ (C.IntConst (Primitives.I54, x)) =
        if x < 0 then
          L.UnaryExp (L.NEGATE, L.ConstExp (L.Numeral (LargeInt.toString (~x))))
        else
          L.ConstExp (L.Numeral (LargeInt.toString x))
    | doValue ctx (C.IntConst (Primitives.I64, x)) =
        let
          val suffix =
            case #targetLuaVersion ctx of
              LUA5_3 => ""
            | LUAJIT => "LL"
        in
          if x < 0 then
            if x = ~0x8000000000000000 then
              L.BinExp
                ( L.MINUS
                , L.UnaryExp (L.NEGATE, L.ConstExp (L.Numeral
                    (LargeInt.toString (~(x + 1)) ^ suffix)))
                , L.ConstExp (L.Numeral ("1" ^ suffix))
                )
            else
              L.UnaryExp (L.NEGATE, L.ConstExp (L.Numeral
                (LargeInt.toString (~x) ^ suffix)))
          else
            L.ConstExp (L.Numeral (LargeInt.toString x ^ suffix))
        end
    | doValue _ (C.IntConst (Primitives.INT_INF, _)) =
        raise CodeGenError "IntInf is not natively supported by Lua backend"
    | doValue _ (C.WordConst (Primitives.WORD, x)) =
        L.ConstExp (L.Numeral ("0x" ^ LargeInt.fmt StringCvt.HEX x))
    | doValue _ (C.WordConst (Primitives.W32, x)) =
        L.ConstExp (L.Numeral ("0x" ^ LargeInt.fmt StringCvt.HEX x))
    | doValue ctx (C.WordConst (Primitives.W64, x)) =
        let
          val suffix =
            case #targetLuaVersion ctx of
              LUA5_3 => ""
            | LUAJIT => "ULL"
        in
          L.ConstExp (L.Numeral ("0x" ^ LargeInt.fmt StringCvt.HEX x ^ suffix))
        end
    | doValue _ (C.CharConst c) =
        L.ConstExp (L.Numeral (Int.toString (Char.ord c)))
    | doValue _ (C.Char7Const c) =
        L.ConstExp (L.Numeral (Int.toString (Char.ord c)))
    | doValue _ (C.Char16Const i) =
        L.ConstExp (L.Numeral (Int.toString i))
    | doValue _ (C.Char32Const i) =
        L.ConstExp (L.Numeral (Int.toString i))
    | doValue _ (C.UCharConst i) =
        L.ConstExp (L.Numeral (Int.toString i))
    | doValue _ (C.StringConst s) =
        L.ConstExp (L.LiteralString s)
    | doValue _ (C.String7Const s) =
        L.ConstExp (L.LiteralString s)
    | doValue _ (C.String16Const s) =
        L.ConstExp (L.LiteralString (String.implode
          (Vector.foldr
             (fn (i, acc) => Char.chr (i div 256) :: Char.chr (i mod 256) :: acc)
             [] s))) (* big endian *)
    | doValue _ (C.String32Const s) =
        L.ConstExp (L.LiteralString (String.implode
          (Vector.foldr
             (fn (i, acc) =>
                let
                  val (i3, j3) = (i div 0x1000000, i mod 0x1000000)
                  val (i2, j2) = (j3 div 0x10000, j3 mod 0x10000)
                  val (i1, i0) = (j2 div 0x100, j2 mod 0x100)
                in
                  Char.chr i3 :: Char.chr i2 :: Char.chr i1 :: Char.chr i0
                  :: acc
                end) [] s))) (* big endian *)
    | doValue _ (C.UStringConst s) =
        L.ConstExp (L.LiteralString (StringElement.encode8bit
          (Vector.map StringElement.UNICODE_SCALAR s)))
    | doValue _ (C.PrimEffect _) = L.ConstExp L.Nil
    | doValue ctx (C.Cast {value, ...}) = doValue ctx value
    | doValue ctx (C.Pack {value, ...}) = doValue ctx value

  (*:
  val doExp : Context * Env * N.Exp -> L.Exp
  val doDecs : Context * Env * C.CVar option * N.Dec list * N.Stat * L.Stat list -> L.Stat list
  and doStat : Context * Env * C.CVar option * N.Stat -> L.Stat list
   *)
  fun doExp (ctx, _, N.Value v) = doValue ctx v
    | doExp (ctx, env, N.PrimOp {primOp, tyargs = _, args}) =
        (case (primOp, args) of
           (F.RealConstOp x, _) =>
             if Numeric.Notation.isNegative x then
               case (#targetLuaVersion ctx, Numeric.Notation.isNegativeZero x) of
                 (LUAJIT, true) => L.VarExp (L.PredefinedId "NEGATIVE_ZERO")
               | _ =>
                   L.UnaryExp (L.NEGATE, L.ConstExp (L.Numeral
                     (Numeric.Notation.toString "-" (Numeric.Notation.abs x))))
             else
               L.ConstExp (L.Numeral (Numeric.Notation.toString "-" x))
         | (F.ListOp, []) => L.ConstExp L.Nil
         | (F.ListOp, xs) =>
             let
               fun doFields (_, []) = []
                 | doFields (i, y :: ys) =
                     (L.IntKey i, doExp (ctx, env, y)) :: doFields (i + 1, ys)
               fun cons (x, xs) =
                 L.TableExp (vector [(L.IntKey 1, x), (L.IntKey 2, xs)])
             in
               if List.length xs <= 4 then
                 List.foldr cons (L.ConstExp L.Nil)
                   (List.map (fn x => doExp (ctx, env, x)) xs)
               else
                 L.CallExp (L.VarExp (L.PredefinedId "_list"), vector
                   [L.TableExp (vector
                      (( L.StringKey "n"
                       , L.ConstExp (L.Numeral (Int.toString (List.length xs)))
                       ) :: doFields (1, xs)))])
             end
         | (F.VectorOp, xs) =>
             let
               fun doFields (_, []) = []
                 | doFields (i, y :: ys) =
                     (L.IntKey i, doExp (ctx, env, y)) :: doFields (i + 1, ys)
             in
               L.TableExp (vector
                 (( L.StringKey "n"
                  , L.ConstExp (L.Numeral (Int.toString (List.length xs)))
                  ) :: doFields (1, xs)))
             end
         | (F.DataTagAsStringOp info, [exp]) =>
             (case #representation info of
                Syntax.REP_BOXED =>
                  L.IndexExp
                    (doExp (ctx, env, exp), L.ConstExp (L.LiteralString "tag"))
              | Syntax.REP_ENUM => doExp (ctx, env, exp)
              | _ =>
                  raise CodeGenError
                    "unexpected datatype representation for DataTagAsStringOp")
         | (F.DataPayloadOp info, [exp]) =>
             (case #representation info of
                Syntax.REP_BOXED =>
                  L.IndexExp
                    ( doExp (ctx, env, exp)
                    , L.ConstExp (L.LiteralString "payload")
                    )
              | Syntax.REP_ALIAS => doExp (ctx, env, exp)
              | _ =>
                  raise CodeGenError
                    "unexpected datatype representation for DataPayloadOp")
         | (F.ExnPayloadOp, [exp]) =>
             L.IndexExp
               (doExp (ctx, env, exp), L.ConstExp (L.LiteralString "payload"))
         | (F.ConstructValOp info, []) =>
             let
               val tag = #tag info
             in
               case #representation info of
                 Syntax.REP_BOXED =>
                   L.TableExp (vector
                     [(L.StringKey "tag", L.ConstExp (L.LiteralString tag))])
               | Syntax.REP_ENUM => L.ConstExp (L.LiteralString tag)
               | Syntax.REP_UNIT => L.ConstExp L.Nil
               | _ =>
                   raise CodeGenError
                     "unexpected datatype representation for ConstructValOp"
             end
         | (F.ConstructValWithPayloadOp info, [payload]) =>
             let
               val tag = #tag info
               val payload = doExp (ctx, env, payload)
             in
               case #representation info of
                 Syntax.REP_BOXED =>
                   L.TableExp (vector
                     [ (L.StringKey "tag", L.ConstExp (L.LiteralString tag))
                     , (L.StringKey "payload", payload)
                     ])
               | Syntax.REP_ALIAS => payload
               | _ =>
                   raise CodeGenError
                     "unexpected datatype representation for ConstructValWithPayloadOp"
             end
         | (F.ConstructExnOp, [tag]) =>
             let
               val tag = doExp (ctx, env, tag)
             in
               L.CallExp (L.VarExp (L.PredefinedId "setmetatable"), vector
                 [ L.TableExp (vector [(L.StringKey "tag", tag)])
                 , L.VarExp (L.PredefinedId "_exn_meta")
                 ])
             end
         | (F.ConstructExnWithPayloadOp, [tag, payload]) =>
             let
               val tag = doExp (ctx, env, tag)
               val payload = doExp (ctx, env, payload)
             in
               L.CallExp (L.VarExp (L.PredefinedId "setmetatable"), vector
                 [ L.TableExp (vector
                     [ (L.StringKey "tag", tag)
                     , (L.StringKey "payload", payload)
                     ])
                 , L.VarExp (L.PredefinedId "_exn_meta")
                 ])
             end
         | (F.RaiseOp _, _) => raise CodeGenError "unexpected RaiseOp"
         | (F.PrimCall prim, args) =>
             let
               fun doUnary f =
                 case args of
                   [a] => f (doExp (ctx, env, a))
                 | _ =>
                     raise CodeGenError
                       ("primop " ^ Primitives.toString prim
                        ^ ": invalid number of arguments")
               fun doUnaryExp (f, _ (* purity *)) = doUnary f
               fun doBinaryRaw f =
                 case args of
                   [a, b] => f (a, b)
                 | _ =>
                     raise CodeGenError
                       ("primop " ^ Primitives.toString prim
                        ^ ": invalid number of arguments")
               fun doBinary f =
                 doBinaryRaw (fn (a, b) =>
                   f (doExp (ctx, env, a), doExp (ctx, env, b)))
               fun doTernary f =
                 case args of
                   [a, b, c] =>
                     f ( doExp (ctx, env, a)
                       , doExp (ctx, env, b)
                       , doExp (ctx, env, c)
                       )
                 | _ =>
                     raise CodeGenError
                       ("primop " ^ Primitives.toString prim
                        ^ ": invalid number of arguments")
               fun doBinaryExpRaw (f, _ (* purity *)) = doBinaryRaw f
               fun doBinaryExp (f, _ (* purity *)) = doBinary f
               fun doBinaryOp (binop, purity) =
                 doBinaryExp (fn (a, b) => L.BinExp (binop, a, b), purity)
               fun doUnaryExpE f =
                 case args of
                   [a, _] => f (doExp (ctx, env, a))
                 | _ =>
                     raise CodeGenError
                       ("primop " ^ Primitives.toString prim
                        ^ ": invalid number of arguments")
               fun doBinaryExpE f =
                 case args of
                   [a, b, _] => f (doExp (ctx, env, a), doExp (ctx, env, b))
                 | _ =>
                     raise CodeGenError
                       ("primop " ^ Primitives.toString prim
                        ^ ": invalid number of arguments")
               fun doBinaryOpE binop =
                 doBinaryExpE (fn (a, b) => L.BinExp (binop, a, b))
               fun newTableWithImpl entries =
                 let
                   fun fallback () =
                     L.CallExp
                       ( L.VarExp (L.PredefinedId "_newTableWith")
                       , vector [doExp (ctx, env, entries)]
                       )
                   fun extract (N.Record fields) =
                         (case
                            ( Syntax.LabelMap.find
                                (fields, Syntax.NumericLabel 1)
                            , Syntax.LabelMap.find
                                (fields, Syntax.NumericLabel 2)
                            )
                          of
                            (SOME (N.Value (C.StringConst key)), SOME value) =>
                              SOME (key, value)
                          | _ => NONE)
                     | extract _ = NONE
                   fun uniqueKeys (_, []) = true
                     | uniqueKeys (set, (key, _) :: entries) =
                         if StringSet.member (set, key) then false
                         else uniqueKeys (StringSet.add (set, key), entries)
                 in
                   case entries of
                     N.PrimOp {primOp = FSyntax.VectorOp, tyargs = _, args} =>
                       (case ListUtil.mapOption extract args of
                          SOME entries =>
                            if uniqueKeys (StringSet.empty, entries) then
                              L.TableExp
                                (Vector.map
                                   (fn (key, value) =>
                                      (L.StringKey key, doExp (ctx, env, value)))
                                   (Vector.fromList entries))
                            else
                              fallback ()
                        | NONE => fallback ())
                   | _ => fallback ()
                 end
             in
               case prim of
                 Primitives.mkFn2 =>
                   doUnary (fn f =>
                     L.CallExp (L.VarExp (L.PredefinedId "_mkFn2"), vector [f]))
               | Primitives.mkFn3 =>
                   doUnary (fn f =>
                     L.CallExp (L.VarExp (L.PredefinedId "_mkFn3"), vector [f]))
               | Primitives.List_cons =>
                   doBinaryExp
                     ( fn (x, xs) =>
                         L.TableExp (vector [(L.IntKey 1, x), (L.IntKey 2, xs)])
                     , PURE
                     )
               | Primitives.List_null =>
                   doUnaryExp
                     (fn a => L.BinExp (L.EQUAL, a, L.ConstExp L.Nil), PURE)
               | Primitives.List_unsafeHead =>
                   doUnaryExp
                     ( fn xs => L.IndexExp (xs, L.ConstExp (L.Numeral "1"))
                     , PURE
                     )
               | Primitives.List_unsafeTail =>
                   doUnaryExp
                     ( fn xs => L.IndexExp (xs, L.ConstExp (L.Numeral "2"))
                     , PURE
                     )
               | Primitives.General_exnName =>
                   doUnaryExp
                     ( fn x =>
                         L.CallExp
                           (L.VarExp (L.PredefinedId "_exnName"), vector [x])
                     , PURE
                     )
               | Primitives.Ref_ref =>
                   doUnaryExp
                     ( fn x =>
                         (* REPRESENTATION_OF_REF *)
                         L.TableExp (vector [(L.IntKey 1, x)])
                     , DISCARDABLE
                     )
               | Primitives.Ref_EQUAL => doBinaryOp (L.EQUAL, PURE)
               | Primitives.Ref_set => raise CodeGenError "unexpected Ref.set"
               | Primitives.Ref_read =>
                   doUnaryExp
                     ( fn a =>
                         (* REPRESENTATION_OF_REF *)
                         L.IndexExp (a, L.ConstExp (L.Numeral "1"))
                     , DISCARDABLE
                     )
               | Primitives.Bool_EQUAL => doBinaryOp (L.EQUAL, PURE)
               | Primitives.Bool_not =>
                   doUnaryExp (fn a => L.UnaryExp (L.NOT, a), PURE)
               | Primitives.Int_EQUAL _ => doBinaryOp (L.EQUAL, PURE)
               | Primitives.Int_PLUS i =>
                   (case (#targetLuaVersion ctx, i) of
                      (LUA5_3, Primitives.INT) =>
                        doBinaryExp
                          ( fn (a, b) =>
                              L.CallExp
                                ( L.VarExp (L.PredefinedId "_Int_add")
                                , vector [a, b]
                                )
                          , IMPURE
                          )
                    | (LUAJIT, Primitives.I54) =>
                        doBinaryExp
                          ( fn (a, b) =>
                              L.CallExp
                                ( L.VarExp (L.PredefinedId "_Int54_add")
                                , vector [a, b]
                                )
                          , IMPURE
                          )
                    | _ =>
                        raise CodeGenError
                          ("primop " ^ Primitives.toString prim
                           ^ " is not supported on this target"))
               | Primitives.Int_PLUS_wrapping i =>
                   (case (#targetLuaVersion ctx, i) of
                      (LUAJIT, Primitives.I64) => doBinaryOp (L.PLUS, PURE)
                    | _ =>
                        raise CodeGenError
                          ("primop " ^ Primitives.toString prim
                           ^ " is not supported on this target"))
               | Primitives.Int_MINUS i =>
                   (case (#targetLuaVersion ctx, i) of
                      (LUA5_3, Primitives.INT) =>
                        doBinaryExp
                          ( fn (a, b) =>
                              L.CallExp
                                ( L.VarExp (L.PredefinedId "_Int_sub")
                                , vector [a, b]
                                )
                          , IMPURE
                          )
                    | (LUAJIT, Primitives.I54) =>
                        doBinaryExp
                          ( fn (a, b) =>
                              L.CallExp
                                ( L.VarExp (L.PredefinedId "_Int54_sub")
                                , vector [a, b]
                                )
                          , IMPURE
                          )
                    | _ =>
                        raise CodeGenError
                          ("primop " ^ Primitives.toString prim
                           ^ " is not supported on this target"))
               | Primitives.Int_MINUS_wrapping i =>
                   (case (#targetLuaVersion ctx, i) of
                      (LUAJIT, Primitives.I64) => doBinaryOp (L.MINUS, PURE)
                    | _ =>
                        raise CodeGenError
                          ("primop " ^ Primitives.toString prim
                           ^ " is not supported on this target"))
               | Primitives.Int_TIMES i =>
                   (case (#targetLuaVersion ctx, i) of
                      (LUA5_3, Primitives.INT) =>
                        doBinaryExp
                          ( fn (a, b) =>
                              L.CallExp
                                ( L.VarExp (L.PredefinedId "_Int_mul")
                                , vector [a, b]
                                )
                          , IMPURE
                          )
                    | (LUAJIT, Primitives.I54) =>
                        doBinaryExp
                          ( fn (a, b) =>
                              L.CallExp
                                ( L.VarExp (L.PredefinedId "_Int54_mul")
                                , vector [a, b]
                                )
                          , IMPURE
                          )
                    | _ =>
                        raise CodeGenError
                          ("primop " ^ Primitives.toString prim
                           ^ " is not supported on this target"))
               | Primitives.Int_TIMES_wrapping i =>
                   (case (#targetLuaVersion ctx, i) of
                      (LUAJIT, Primitives.I64) => doBinaryOp (L.TIMES, PURE)
                    | _ =>
                        raise CodeGenError
                          ("primop " ^ Primitives.toString prim
                           ^ " is not supported on this target"))
               | Primitives.Int_div i =>
                   (case (#targetLuaVersion ctx, i) of
                      (LUA5_3, Primitives.INT) =>
                        doBinaryExp
                          ( fn (a, b) =>
                              L.CallExp
                                ( L.VarExp (L.PredefinedId "_Int_div")
                                , vector [a, b]
                                )
                          , IMPURE
                          )
                    | (LUAJIT, Primitives.I54) =>
                        doBinaryExp
                          ( fn (a, b) =>
                              L.CallExp
                                ( L.VarExp (L.PredefinedId "_Int54_div")
                                , vector [a, b]
                                )
                          , IMPURE
                          )
                    | _ =>
                        raise CodeGenError
                          ("primop " ^ Primitives.toString prim
                           ^ " is not supported on this target"))
               | Primitives.Int_div_unchecked i =>
                   (case (#targetLuaVersion ctx, i) of
                      (LUA5_3, Primitives.INT) => doBinaryOp (L.INTDIV, PURE)
                    | (LUAJIT, Primitives.I32) =>
                        doBinaryExp
                          ( fn (a, b) =>
                              L.CallExp
                                ( L.VarExp (L.PredefinedId "math_floor")
                                , vector [L.BinExp (L.DIV, a, b)]
                                )
                          , PURE
                          )
                    | (LUAJIT, Primitives.I54) =>
                        doBinaryExp
                          ( fn (a, b) =>
                              L.CallExp
                                ( L.VarExp (L.PredefinedId "math_floor")
                                , vector [L.BinExp (L.DIV, a, b)]
                                )
                          , PURE
                          )
                    | _ =>
                        raise CodeGenError
                          ("primop " ^ Primitives.toString prim
                           ^ " is not supported on this target"))
               | Primitives.Int_mod i =>
                   (case (#targetLuaVersion ctx, i) of
                      (LUA5_3, Primitives.INT) =>
                        doBinaryExp
                          ( fn (a, b) =>
                              L.CallExp
                                ( L.VarExp (L.PredefinedId "_Int_mod")
                                , vector [a, b]
                                )
                          , IMPURE
                          )
                    | (LUAJIT, Primitives.I54) =>
                        doBinaryExp
                          ( fn (a, b) =>
                              L.CallExp
                                ( L.VarExp (L.PredefinedId "_Int54_mod")
                                , vector [a, b]
                                )
                          , IMPURE
                          )
                    | _ =>
                        raise CodeGenError
                          ("primop " ^ Primitives.toString prim
                           ^ " is not supported on this target"))
               | Primitives.Int_mod_unchecked i =>
                   (case (#targetLuaVersion ctx, i) of
                      (LUA5_3, Primitives.INT) => doBinaryOp (L.MOD, PURE)
                    | (LUAJIT, Primitives.I32) => doBinaryOp (L.MOD, PURE)
                    | (LUAJIT, Primitives.I54) =>
                        doBinaryExp
                          ( fn (a, b) =>
                              L.CallExp
                                ( L.VarExp (L.PredefinedId "_Int54_mod")
                                , vector [a, b]
                                )
                          , PURE
                          )
                    | _ =>
                        raise CodeGenError
                          ("primop " ^ Primitives.toString prim
                           ^ " is not supported on this target"))
               | Primitives.Int_quot i =>
                   (case (#targetLuaVersion ctx, i) of
                      (LUA5_3, Primitives.INT) =>
                        raise CodeGenError
                          "primop Int.quot is not supported on this target"
                    | (LUAJIT, Primitives.I54) =>
                        doBinaryExp
                          ( fn (a, b) =>
                              L.CallExp
                                ( L.VarExp (L.PredefinedId "_Int54_quot")
                                , vector [a, b]
                                )
                          , IMPURE
                          )
                    | _ =>
                        raise CodeGenError
                          ("primop " ^ Primitives.toString prim
                           ^ " is not supported on this target"))
               | Primitives.Int_quot_unchecked i =>
                   (case (#targetLuaVersion ctx, i) of
                      (LUA5_3, Primitives.INT) =>
                        raise CodeGenError
                          "primop Int.quot.unchecked is not supported on this target"
                    | (LUAJIT, Primitives.I32) =>
                        doBinaryExp
                          ( fn (a, b) =>
                              L.SingleValueExp
                                (L.CallExp
                                   ( L.VarExp (L.PredefinedId "math_modf")
                                   , vector [L.BinExp (L.DIV, a, b)]
                                   ))
                          , PURE
                          )
                    | (LUAJIT, Primitives.I54) =>
                        doBinaryExp
                          ( fn (a, b) =>
                              L.SingleValueExp
                                (L.CallExp
                                   ( L.VarExp (L.PredefinedId "math_modf")
                                   , vector [L.BinExp (L.DIV, a, b)]
                                   ))
                          , PURE
                          )
                    | (LUAJIT, Primitives.I64) => doBinaryOp (L.DIV, PURE)
                    | _ =>
                        raise CodeGenError
                          ("primop " ^ Primitives.toString prim
                           ^ " is not supported on this target"))
               | Primitives.Int_rem_unchecked i =>
                   (case (#targetLuaVersion ctx, i) of
                      (LUAJIT, Primitives.I64) => doBinaryOp (L.MOD, PURE)
                    | _ =>
                        raise CodeGenError
                          ("primop " ^ Primitives.toString prim
                           ^ " is not supported on this target"))
               | Primitives.Int_TILDE i =>
                   (case (#targetLuaVersion ctx, i) of
                      (LUA5_3, Primitives.INT) =>
                        doUnaryExp
                          ( fn a =>
                              L.CallExp
                                ( L.VarExp (L.PredefinedId "_Int_negate")
                                , vector [a]
                                )
                          , IMPURE
                          )
                    | (LUAJIT, Primitives.I54) =>
                        doUnaryExp
                          ( fn a =>
                              L.CallExp
                                ( L.VarExp (L.PredefinedId "_Int54_negate")
                                , vector [a]
                                )
                          , IMPURE
                          )
                    | _ =>
                        raise CodeGenError
                          ("primop " ^ Primitives.toString prim
                           ^ " is not supported on this target"))
               | Primitives.Int_TILDE_unchecked i =>
                   (case (#targetLuaVersion ctx, i) of
                      (LUA5_3, Primitives.INT) =>
                        doUnaryExp (fn a => L.UnaryExp (L.NEGATE, a), PURE)
                    | (LUAJIT, Primitives.I54) =>
                        doUnaryExp
                          ( fn a =>
                              L.BinExp (L.MINUS, L.ConstExp (L.Numeral "0"), a)
                          , PURE
                          ) (* Should we avoid negative zero? *)
                    | (LUAJIT, Primitives.I64) =>
                        doUnaryExp (fn a => L.UnaryExp (L.NEGATE, a), PURE)
                    | _ =>
                        raise CodeGenError
                          ("primop " ^ Primitives.toString prim
                           ^ " is not supported on this target"))
               | Primitives.Int_TILDE_wrapping i =>
                   (case (#targetLuaVersion ctx, i) of
                      (LUA5_3, Primitives.INT) =>
                        doUnaryExp (fn a => L.UnaryExp (L.NEGATE, a), PURE)
                    | (LUAJIT, Primitives.I64) =>
                        doUnaryExp (fn a => L.UnaryExp (L.NEGATE, a), PURE)
                    | _ =>
                        raise CodeGenError
                          ("primop " ^ Primitives.toString prim
                           ^ " is not supported on this target"))
               | Primitives.Int_abs i =>
                   (case (#targetLuaVersion ctx, i) of
                      (LUA5_3, Primitives.INT) =>
                        doUnaryExp
                          ( fn a =>
                              L.CallExp
                                ( L.VarExp (L.PredefinedId "_Int_abs")
                                , vector [a]
                                )
                          , IMPURE
                          )
                    | (LUAJIT, Primitives.I54) =>
                        doUnaryExp
                          ( fn a =>
                              L.CallExp
                                ( L.VarExp (L.PredefinedId "_Int54_abs")
                                , vector [a]
                                )
                          , IMPURE
                          )
                    | _ =>
                        raise CodeGenError
                          ("primop " ^ Primitives.toString prim
                           ^ " is not supported on this target"))
               | Primitives.Int_LT _ => doBinaryOp (L.LT, PURE)
               | Primitives.Int_GT _ => doBinaryOp (L.GT, PURE)
               | Primitives.Int_LE _ => doBinaryOp (L.LE, PURE)
               | Primitives.Int_GE _ => doBinaryOp (L.GE, PURE)
               | Primitives.Int_toInt_unchecked (i1, i2) =>
                   (case (#targetLuaVersion ctx, i1, i2) of
                      (_, Primitives.I32, Primitives.I32) =>
                        doUnaryExp (fn a => a, PURE)
                    | (_, Primitives.I54, Primitives.I54) =>
                        doUnaryExp (fn a => a, PURE)
                    | (_, Primitives.I64, Primitives.I64) =>
                        doUnaryExp (fn a => a, PURE)
                    | (_, Primitives.INT, Primitives.INT) =>
                        doUnaryExp (fn a => a, PURE)
                    | (_, Primitives.I32, Primitives.I54) =>
                        doUnaryExp (fn a => a, PURE)
                    | (_, Primitives.I54, Primitives.I32) =>
                        doUnaryExp (fn a => a, PURE)
                    | (LUAJIT, Primitives.I32, Primitives.I64) =>
                        doUnaryExp
                          ( fn a =>
                              L.CallExp
                                ( L.VarExp (L.PredefinedId "int64_t")
                                , vector [a]
                                )
                          , PURE
                          )
                    | (LUAJIT, Primitives.I54, Primitives.I64) =>
                        doUnaryExp
                          ( fn a =>
                              L.CallExp
                                ( L.VarExp (L.PredefinedId "int64_t")
                                , vector [a]
                                )
                          , PURE
                          )
                    | (LUAJIT, Primitives.I64, Primitives.I32) =>
                        doUnaryExp
                          ( fn a =>
                              L.CallExp
                                ( L.VarExp (L.PredefinedId "tonumber")
                                , vector [a]
                                )
                          , PURE
                          )
                    | (LUAJIT, Primitives.I64, Primitives.I54) =>
                        doUnaryExp
                          ( fn a =>
                              L.CallExp
                                ( L.VarExp (L.PredefinedId "tonumber")
                                , vector [a]
                                )
                          , PURE
                          )
                    | _ =>
                        raise CodeGenError
                          ("primop " ^ Primitives.toString prim
                           ^ " is not supported on this target"))
               | Primitives.Word_EQUAL _ => doBinaryOp (L.EQUAL, PURE)
               | Primitives.Word_PLUS w =>
                   (case (#targetLuaVersion ctx, w) of
                      (LUA5_3, Primitives.WORD) => doBinaryOp (L.PLUS, PURE)
                    | (LUAJIT, Primitives.W32) =>
                        doBinaryExp
                          ( fn (a, b) =>
                              L.BinExp
                                ( L.MOD
                                , L.BinExp (L.PLUS, a, b)
                                , L.ConstExp (L.Numeral "0x100000000")
                                )
                          , PURE
                          )
                    | (LUAJIT, Primitives.W64) => doBinaryOp (L.PLUS, PURE)
                    | _ =>
                        raise CodeGenError
                          ("primop " ^ Primitives.toString prim
                           ^ " is not supported on this target"))
               | Primitives.Word_MINUS w =>
                   (case (#targetLuaVersion ctx, w) of
                      (LUA5_3, Primitives.WORD) => doBinaryOp (L.MINUS, PURE)
                    | (LUAJIT, Primitives.W32) =>
                        doBinaryExp
                          ( fn (a, b) =>
                              L.BinExp
                                ( L.MOD
                                , L.BinExp (L.MINUS, a, b)
                                , L.ConstExp (L.Numeral "0x100000000")
                                )
                          , PURE
                          )
                    | (LUAJIT, Primitives.W64) => doBinaryOp (L.MINUS, PURE)
                    | _ =>
                        raise CodeGenError
                          ("primop " ^ Primitives.toString prim
                           ^ " is not supported on this target"))
               | Primitives.Word_TIMES w =>
                   (case (#targetLuaVersion ctx, w) of
                      (LUA5_3, Primitives.WORD) => doBinaryOp (L.TIMES, PURE)
                    | (LUAJIT, Primitives.W32) =>
                        doBinaryExp
                          ( fn (a, b) =>
                              L.CallExp
                                ( L.VarExp (L.PredefinedId "_Word32_mul")
                                , vector [a, b]
                                )
                          , PURE
                          )
                    | (LUAJIT, Primitives.W64) => doBinaryOp (L.TIMES, PURE)
                    | _ =>
                        raise CodeGenError
                          ("primop " ^ Primitives.toString prim
                           ^ " is not supported on this target"))
               | Primitives.Word_TILDE w =>
                   (case (#targetLuaVersion ctx, w) of
                      (LUA5_3, Primitives.WORD) =>
                        doUnaryExp (fn a => L.UnaryExp (L.NEGATE, a), PURE)
                    | (LUAJIT, Primitives.W32) =>
                        doUnaryExp
                          ( fn a =>
                              L.BinExp
                                ( L.MOD
                                , L.UnaryExp (L.NEGATE, a)
                                , L.ConstExp (L.Numeral "0x100000000")
                                )
                          , PURE
                          )
                    | (LUAJIT, Primitives.W64) =>
                        doUnaryExp (fn a => L.UnaryExp (L.NEGATE, a), PURE)
                    | _ =>
                        raise CodeGenError
                          ("primop " ^ Primitives.toString prim
                           ^ " is not supported on this target"))
               | Primitives.Word_div w =>
                   (case (#targetLuaVersion ctx, w) of
                      (LUA5_3, Primitives.WORD) =>
                        doBinaryExp
                          ( fn (a, b) =>
                              L.CallExp
                                ( L.VarExp (L.PredefinedId "_Word_div")
                                , vector [a, b]
                                )
                          , IMPURE
                          )
                    | _ =>
                        raise CodeGenError
                          ("primop " ^ Primitives.toString prim
                           ^ " is not supported on this target"))
               | Primitives.Word_mod w =>
                   (case (#targetLuaVersion ctx, w) of
                      (LUA5_3, Primitives.WORD) =>
                        doBinaryExp
                          ( fn (a, b) =>
                              L.CallExp
                                ( L.VarExp (L.PredefinedId "_Word_mod")
                                , vector [a, b]
                                )
                          , IMPURE
                          )
                    | _ =>
                        raise CodeGenError
                          ("primop " ^ Primitives.toString prim
                           ^ " is not supported on this target"))
               | Primitives.Word_div_unchecked w =>
                   (case (#targetLuaVersion ctx, w) of
                      (LUA5_3, Primitives.WORD) =>
                        doBinaryExpRaw
                          ( fn ( a
                               , b as
                                   N.Value (C.WordConst (Primitives.WORD, b'))
                               ) =>
                              if b' mod 2 = 0 then
                                let
                                  fun shiftAmount (0, amount) =
                                        (0, amount) (* should not occur *)
                                    | shiftAmount (n, amount) =
                                        if n mod 2 = 0 then
                                          shiftAmount (n div 2, amount + 1)
                                        else
                                          (n, amount)
                                  val (d, shift) = shiftAmount (b', 0)
                                  val a' =
                                    L.BinExp
                                      ( L.RSHIFT
                                      , doExp (ctx, env, a)
                                      , L.ConstExp
                                          (L.Numeral (Int.toString shift))
                                      )
                                in
                                  if d = 1 then
                                    a'
                                  else
                                    L.BinExp (L.INTDIV, a', L.ConstExp
                                      (L.Numeral (IntInf.toString d)))
                                end
                              else
                                L.CallExp
                                  ( L.VarExp (L.PredefinedId "_Word_div")
                                  , vector
                                      [doExp (ctx, env, a), doExp (ctx, env, b)]
                                  )
                             | (a, b) =>
                              L.CallExp
                                ( L.VarExp (L.PredefinedId "_Word_div")
                                , vector
                                    [doExp (ctx, env, a), doExp (ctx, env, b)]
                                )
                          , PURE
                          )
                    | (LUAJIT, Primitives.W32) =>
                        doBinaryExp
                          ( fn (a, b) =>
                              L.CallExp
                                ( L.VarExp (L.PredefinedId "math_floor")
                                , vector [L.BinExp (L.DIV, a, b)]
                                )
                          , PURE
                          )
                    | (LUAJIT, Primitives.W64) => doBinaryOp (L.DIV, PURE)
                    | _ =>
                        raise CodeGenError
                          ("primop " ^ Primitives.toString prim
                           ^ " is not supported on this target"))
               | Primitives.Word_mod_unchecked w =>
                   (case (#targetLuaVersion ctx, w) of
                      (LUA5_3, Primitives.WORD) =>
                        doBinaryExp
                          ( fn (a, b) =>
                              L.CallExp
                                ( L.VarExp (L.PredefinedId "_Word_mod")
                                , vector [a, b]
                                )
                          , PURE
                          )
                    | (LUAJIT, Primitives.W32) => doBinaryOp (L.MOD, PURE)
                    | (LUAJIT, Primitives.W64) => doBinaryOp (L.MOD, PURE)
                    | _ =>
                        raise CodeGenError
                          ("primop " ^ Primitives.toString prim
                           ^ " is not supported on this target"))
               | Primitives.Word_LT w =>
                   (case (#targetLuaVersion ctx, w) of
                      (LUA5_3, Primitives.WORD) =>
                        doBinaryExp
                          ( fn (a, b) =>
                              L.CallExp
                                ( L.VarExp (L.PredefinedId "math_ult")
                                , vector [a, b]
                                )
                          , PURE
                          )
                    | (LUAJIT, Primitives.W32) => doBinaryOp (L.LT, PURE)
                    | (LUAJIT, Primitives.W64) => doBinaryOp (L.LT, PURE)
                    | _ =>
                        raise CodeGenError
                          ("primop " ^ Primitives.toString prim
                           ^ " is not supported on this target"))
               | Primitives.Word_LE w =>
                   (case (#targetLuaVersion ctx, w) of
                      (LUA5_3, Primitives.WORD) =>
                        doBinaryExp
                          ( fn (a, b) =>
                              L.UnaryExp (L.NOT, L.CallExp
                                ( L.VarExp (L.PredefinedId "math_ult")
                                , vector [b, a]
                                ))
                          , PURE
                          )
                    | (LUAJIT, Primitives.W32) => doBinaryOp (L.LE, PURE)
                    | (LUAJIT, Primitives.W64) => doBinaryOp (L.LE, PURE)
                    | _ =>
                        raise CodeGenError
                          ("primop " ^ Primitives.toString prim
                           ^ " is not supported on this target"))
               | Primitives.Word_GT w =>
                   (case (#targetLuaVersion ctx, w) of
                      (LUA5_3, Primitives.WORD) =>
                        doBinaryExp
                          ( fn (a, b) =>
                              L.CallExp
                                ( L.VarExp (L.PredefinedId "math_ult")
                                , vector [b, a]
                                )
                          , PURE
                          )
                    | (LUAJIT, Primitives.W32) => doBinaryOp (L.GT, PURE)
                    | (LUAJIT, Primitives.W64) => doBinaryOp (L.GT, PURE)
                    | _ =>
                        raise CodeGenError
                          ("primop " ^ Primitives.toString prim
                           ^ " is not supported on this target"))
               | Primitives.Word_GE w =>
                   (case (#targetLuaVersion ctx, w) of
                      (LUA5_3, Primitives.WORD) =>
                        doBinaryExp
                          ( fn (a, b) =>
                              L.UnaryExp (L.NOT, L.CallExp
                                ( L.VarExp (L.PredefinedId "math_ult")
                                , vector [a, b]
                                ))
                          , PURE
                          )
                    | (LUAJIT, Primitives.W32) => doBinaryOp (L.GE, PURE)
                    | (LUAJIT, Primitives.W64) => doBinaryOp (L.GE, PURE)
                    | _ =>
                        raise CodeGenError
                          ("primop " ^ Primitives.toString prim
                           ^ " is not supported on this target"))
               | Primitives.Word_notb w =>
                   (case (#targetLuaVersion ctx, w) of
                      (LUA5_3, Primitives.WORD) =>
                        doUnaryExp (fn a => L.UnaryExp (L.BITNOT, a), PURE)
                    | (LUAJIT, Primitives.W32) =>
                        doUnaryExp
                          ( fn a =>
                              L.BinExp
                                ( L.MOD
                                , L.CallExp
                                    ( L.VarExp (L.PredefinedId "bit_bnot")
                                    , vector [a]
                                    )
                                , L.ConstExp (L.Numeral "0x100000000")
                                )
                          , PURE
                          )
                    | (LUAJIT, Primitives.W64) =>
                        doUnaryExp
                          ( fn a =>
                              L.CallExp
                                ( L.VarExp (L.PredefinedId "bit_bnot")
                                , vector [a]
                                )
                          , PURE
                          )
                    | _ =>
                        raise CodeGenError
                          ("primop " ^ Primitives.toString prim
                           ^ " is not supported on this target"))
               | Primitives.Word_andb w =>
                   (case (#targetLuaVersion ctx, w) of
                      (LUA5_3, Primitives.WORD) => doBinaryOp (L.BITAND, PURE)
                    | (LUAJIT, Primitives.W32) =>
                        doBinaryExp
                          ( fn (a, b) =>
                              L.BinExp
                                ( L.MOD
                                , L.CallExp
                                    ( L.VarExp (L.PredefinedId "bit_band")
                                    , vector [a, b]
                                    )
                                , L.ConstExp (L.Numeral "0x100000000")
                                )
                          , PURE
                          )
                    | (LUAJIT, Primitives.W64) =>
                        doBinaryExp
                          ( fn (a, b) =>
                              L.CallExp
                                ( L.VarExp (L.PredefinedId "bit_band")
                                , vector [a, b]
                                )
                          , PURE
                          )
                    | _ =>
                        raise CodeGenError
                          ("primop " ^ Primitives.toString prim
                           ^ " is not supported on this target"))
               | Primitives.Word_orb w =>
                   (case (#targetLuaVersion ctx, w) of
                      (LUA5_3, Primitives.WORD) => doBinaryOp (L.BITOR, PURE)
                    | (LUAJIT, Primitives.W32) =>
                        doBinaryExp
                          ( fn (a, b) =>
                              L.BinExp
                                ( L.MOD
                                , L.CallExp
                                    ( L.VarExp (L.PredefinedId "bit_bor")
                                    , vector [a, b]
                                    )
                                , L.ConstExp (L.Numeral "0x100000000")
                                )
                          , PURE
                          )
                    | (LUAJIT, Primitives.W64) =>
                        doBinaryExp
                          ( fn (a, b) =>
                              L.CallExp
                                ( L.VarExp (L.PredefinedId "bit_bor")
                                , vector [a, b]
                                )
                          , PURE
                          )
                    | _ =>
                        raise CodeGenError
                          ("primop " ^ Primitives.toString prim
                           ^ " is not supported on this target"))
               | Primitives.Word_xorb w =>
                   (case (#targetLuaVersion ctx, w) of
                      (LUA5_3, Primitives.WORD) => doBinaryOp (L.BITXOR, PURE)
                    | (LUAJIT, Primitives.W32) =>
                        doBinaryExp
                          ( fn (a, b) =>
                              L.BinExp
                                ( L.MOD
                                , L.CallExp
                                    ( L.VarExp (L.PredefinedId "bit_bxor")
                                    , vector [a, b]
                                    )
                                , L.ConstExp (L.Numeral "0x100000000")
                                )
                          , PURE
                          )
                    | (LUAJIT, Primitives.W64) =>
                        doBinaryExp
                          ( fn (a, b) =>
                              L.CallExp
                                ( L.VarExp (L.PredefinedId "bit_bxor")
                                , vector [a, b]
                                )
                          , PURE
                          )
                    | _ =>
                        raise CodeGenError
                          ("primop " ^ Primitives.toString prim
                           ^ " is not supported on this target"))
               | Primitives.Word_LSHIFT_unchecked (w1, w2) =>
                   (case (#targetLuaVersion ctx, w1, w2) of
                      (LUA5_3, Primitives.WORD, Primitives.WORD) =>
                        doBinaryOp (L.LSHIFT, PURE)
                    | (LUAJIT, Primitives.W32, Primitives.W32) =>
                        doBinaryExp
                          ( fn (a, b) =>
                              L.BinExp
                                ( L.MOD
                                , L.CallExp
                                    ( L.VarExp (L.PredefinedId "bit_lshift")
                                    , vector [a, b]
                                    )
                                , L.ConstExp (L.Numeral "0x100000000")
                                )
                          , PURE
                          )
                    | (LUAJIT, Primitives.W64, Primitives.W32) =>
                        doBinaryExp
                          ( fn (a, b) =>
                              L.CallExp
                                ( L.VarExp (L.PredefinedId "bit_lshift")
                                , vector [a, b]
                                )
                          , PURE
                          )
                    | _ =>
                        raise CodeGenError
                          ("primop " ^ Primitives.toString prim
                           ^ " is not supported on this target"))
               | Primitives.Word_RSHIFT_unchecked (w1, w2) =>
                   (case (#targetLuaVersion ctx, w1, w2) of
                      (LUA5_3, Primitives.WORD, Primitives.WORD) =>
                        doBinaryOp (L.RSHIFT, PURE)
                    | (LUAJIT, Primitives.W32, Primitives.W32) =>
                        doBinaryExp
                          ( fn (a, b) =>
                              L.BinExp
                                ( L.MOD
                                , L.CallExp
                                    ( L.VarExp (L.PredefinedId "bit_rshift")
                                    , vector [a, b]
                                    )
                                , L.ConstExp (L.Numeral "0x100000000")
                                )
                          , PURE
                          )
                    | (LUAJIT, Primitives.W64, Primitives.W32) =>
                        doBinaryExp
                          ( fn (a, b) =>
                              L.CallExp
                                ( L.VarExp (L.PredefinedId "bit_rshift")
                                , vector [a, b]
                                )
                          , PURE
                          )
                    | _ =>
                        raise CodeGenError
                          ("primop " ^ Primitives.toString prim
                           ^ " is not supported on this target"))
               | Primitives.Real_PLUS => doBinaryOp (L.PLUS, PURE)
               | Primitives.Real_MINUS => doBinaryOp (L.MINUS, PURE)
               | Primitives.Real_TIMES =>
                   (case #targetLuaVersion ctx of
                      LUA5_3 => doBinaryOp (L.TIMES, PURE)
                    | LUAJIT =>
                        doBinaryExp
                          ( fn (a, b) =>
                              L.CallExp
                                ( L.VarExp (L.PredefinedId "_Real_mul")
                                , vector [a, b]
                                )
                          , PURE
                          ))
               | Primitives.Real_DIVIDE => doBinaryOp (L.DIV, PURE)
               | Primitives.Real_TILDE =>
                   doUnaryExp
                     ( fn a =>
                         case #targetLuaVersion ctx of
                           LUA5_3 => L.UnaryExp (L.NEGATE, a)
                         | LUAJIT =>
                             L.BinExp
                               ( L.MINUS
                               , L.VarExp (L.PredefinedId "NEGATIVE_ZERO")
                               , a
                               )
                     , PURE
                     )
               | Primitives.Real_abs =>
                   doUnaryExp
                     ( fn x =>
                         L.CallExp
                           (L.VarExp (L.PredefinedId "math_abs"), vector [x])
                     , PURE
                     )
               | Primitives.Real_LT => doBinaryOp (L.LT, PURE)
               | Primitives.Real_GT => doBinaryOp (L.GT, PURE)
               | Primitives.Real_LE => doBinaryOp (L.LE, PURE)
               | Primitives.Real_GE => doBinaryOp (L.GE, PURE)
               | Primitives.Char_EQUAL => doBinaryOp (L.EQUAL, PURE)
               | Primitives.Char_LT => doBinaryOp (L.LT, PURE)
               | Primitives.Char_GT => doBinaryOp (L.GT, PURE)
               | Primitives.Char_LE => doBinaryOp (L.LE, PURE)
               | Primitives.Char_GE => doBinaryOp (L.GE, PURE)
               | Primitives.Char_ord w =>
                   (case (#targetLuaVersion ctx, w) of
                      (LUA5_3, Primitives.INT) =>
                        doUnaryExp (fn a => a, PURE) (* no-op *)
                    | (LUAJIT, Primitives.I32) =>
                        doUnaryExp (fn a => a, PURE) (* no-op *)
                    | (LUAJIT, Primitives.I54) =>
                        doUnaryExp (fn a => a, PURE) (* no-op *)
                    | _ =>
                        raise CodeGenError
                          ("primop " ^ Primitives.toString prim
                           ^ " is not supported on this target"))
               | Primitives.Char_chr_unchecked w =>
                   (case (#targetLuaVersion ctx, w) of
                      (LUA5_3, Primitives.INT) =>
                        doUnaryExp (fn a => a, PURE) (* no-op *)
                    | (LUAJIT, Primitives.I32) =>
                        doUnaryExp (fn a => a, PURE) (* no-op *)
                    | (LUAJIT, Primitives.I54) =>
                        doUnaryExp (fn a => a, PURE) (* no-op *)
                    | _ =>
                        raise CodeGenError
                          ("primop " ^ Primitives.toString prim
                           ^ " is not supported on this target"))
               | Primitives.Char_fromChar7 =>
                   doUnaryExp (fn a => a, PURE) (* no-op *)
               | Primitives.String_EQUAL => doBinaryOp (L.EQUAL, PURE)
               | Primitives.String_LT => doBinaryOp (L.LT, PURE)
               | Primitives.String_GT => doBinaryOp (L.GT, PURE)
               | Primitives.String_LE => doBinaryOp (L.LE, PURE)
               | Primitives.String_GE => doBinaryOp (L.GE, PURE)
               | Primitives.String_HAT => doBinaryOp (L.CONCAT, PURE)
               | Primitives.String_size _ =>
                   doUnaryExp (fn a => L.UnaryExp (L.LENGTH, a), PURE)
               | Primitives.String_str =>
                   doUnaryExp
                     ( fn a =>
                         L.CallExp
                           (L.VarExp (L.PredefinedId "string_char"), vector [a])
                     , PURE
                     )
               | Primitives.String_fromString7 =>
                   doUnaryExp (fn a => a, PURE) (* no-op *)
               | Primitives.Char7_EQUAL => doBinaryOp (L.EQUAL, PURE)
               | Primitives.Char7_LT => doBinaryOp (L.LT, PURE)
               | Primitives.Char7_GT => doBinaryOp (L.GT, PURE)
               | Primitives.Char7_LE => doBinaryOp (L.LE, PURE)
               | Primitives.Char7_GE => doBinaryOp (L.GE, PURE)
               | Primitives.Char7_ord w =>
                   (case (#targetLuaVersion ctx, w) of
                      (LUA5_3, Primitives.INT) =>
                        doUnaryExp (fn a => a, PURE) (* no-op *)
                    | (LUAJIT, Primitives.I32) =>
                        doUnaryExp (fn a => a, PURE) (* no-op *)
                    | (LUAJIT, Primitives.I54) =>
                        doUnaryExp (fn a => a, PURE) (* no-op *)
                    | _ =>
                        raise CodeGenError
                          ("primop " ^ Primitives.toString prim
                           ^ " is not supported on this target"))
               | Primitives.Char7_chr_unchecked w =>
                   (case (#targetLuaVersion ctx, w) of
                      (LUA5_3, Primitives.INT) =>
                        doUnaryExp (fn a => a, PURE) (* no-op *)
                    | (LUAJIT, Primitives.I32) =>
                        doUnaryExp (fn a => a, PURE) (* no-op *)
                    | (LUAJIT, Primitives.I54) =>
                        doUnaryExp (fn a => a, PURE) (* no-op *)
                    | _ =>
                        raise CodeGenError
                          ("primop " ^ Primitives.toString prim
                           ^ " is not supported on this target"))
               | Primitives.Char16_EQUAL => doBinaryOp (L.EQUAL, PURE)
               | Primitives.Char16_LT => doBinaryOp (L.LT, PURE)
               | Primitives.Char16_GT => doBinaryOp (L.GT, PURE)
               | Primitives.Char16_LE => doBinaryOp (L.LE, PURE)
               | Primitives.Char16_GE => doBinaryOp (L.GE, PURE)
               | Primitives.Char16_ord w =>
                   (case (#targetLuaVersion ctx, w) of
                      (LUA5_3, Primitives.INT) =>
                        doUnaryExp (fn a => a, PURE) (* no-op *)
                    | (LUAJIT, Primitives.I32) =>
                        doUnaryExp (fn a => a, PURE) (* no-op *)
                    | (LUAJIT, Primitives.I54) =>
                        doUnaryExp (fn a => a, PURE) (* no-op *)
                    | _ =>
                        raise CodeGenError
                          ("primop " ^ Primitives.toString prim
                           ^ " is not supported on this target"))
               | Primitives.Char16_chr_unchecked w =>
                   (case (#targetLuaVersion ctx, w) of
                      (LUA5_3, Primitives.INT) =>
                        doUnaryExp (fn a => a, PURE) (* no-op *)
                    | (LUAJIT, Primitives.I32) =>
                        doUnaryExp (fn a => a, PURE) (* no-op *)
                    | (LUAJIT, Primitives.I54) =>
                        doUnaryExp (fn a => a, PURE) (* no-op *)
                    | _ =>
                        raise CodeGenError
                          ("primop " ^ Primitives.toString prim
                           ^ " is not supported on this target"))
               | Primitives.Char16_fromChar7 =>
                   doUnaryExp (fn a => a, PURE) (* no-op *)
               | Primitives.Char32_EQUAL => doBinaryOp (L.EQUAL, PURE)
               | Primitives.Char32_LT => doBinaryOp (L.LT, PURE)
               | Primitives.Char32_GT => doBinaryOp (L.GT, PURE)
               | Primitives.Char32_LE => doBinaryOp (L.LE, PURE)
               | Primitives.Char32_GE => doBinaryOp (L.GE, PURE)
               | Primitives.Char32_ord w =>
                   (case (#targetLuaVersion ctx, w) of
                      (LUA5_3, Primitives.INT) =>
                        doUnaryExp (fn a => a, PURE) (* no-op *)
                    | (LUAJIT, Primitives.I32) =>
                        doUnaryExp (fn a => a, PURE) (* no-op *)
                    | (LUAJIT, Primitives.I54) =>
                        doUnaryExp (fn a => a, PURE) (* no-op *)
                    | _ =>
                        raise CodeGenError
                          ("primop " ^ Primitives.toString prim
                           ^ " is not supported on this target"))
               | Primitives.Char32_chr_unchecked w =>
                   (case (#targetLuaVersion ctx, w) of
                      (LUA5_3, Primitives.INT) =>
                        doUnaryExp (fn a => a, PURE) (* no-op *)
                    | (LUAJIT, Primitives.I32) =>
                        doUnaryExp (fn a => a, PURE) (* no-op *)
                    | (LUAJIT, Primitives.I54) =>
                        doUnaryExp (fn a => a, PURE) (* no-op *)
                    | _ =>
                        raise CodeGenError
                          ("primop " ^ Primitives.toString prim
                           ^ " is not supported on this target"))
               | Primitives.Char32_fromChar7 =>
                   doUnaryExp (fn a => a, PURE) (* no-op *)
               | Primitives.Char32_fromUChar =>
                   doUnaryExp (fn a => a, PURE) (* no-op *)
               | Primitives.UChar_EQUAL => doBinaryOp (L.EQUAL, PURE)
               | Primitives.UChar_LT => doBinaryOp (L.LT, PURE)
               | Primitives.UChar_GT => doBinaryOp (L.GT, PURE)
               | Primitives.UChar_LE => doBinaryOp (L.LE, PURE)
               | Primitives.UChar_GE => doBinaryOp (L.GE, PURE)
               | Primitives.UChar_ord w =>
                   (case (#targetLuaVersion ctx, w) of
                      (LUA5_3, Primitives.INT) =>
                        doUnaryExp (fn a => a, PURE) (* no-op *)
                    | (LUAJIT, Primitives.I32) =>
                        doUnaryExp (fn a => a, PURE) (* no-op *)
                    | (LUAJIT, Primitives.I54) =>
                        doUnaryExp (fn a => a, PURE) (* no-op *)
                    | _ =>
                        raise CodeGenError
                          ("primop " ^ Primitives.toString prim
                           ^ " is not supported on this target"))
               | Primitives.UChar_chr_unchecked w =>
                   (case (#targetLuaVersion ctx, w) of
                      (LUA5_3, Primitives.INT) =>
                        doUnaryExp (fn a => a, PURE) (* no-op *)
                    | (LUAJIT, Primitives.I32) =>
                        doUnaryExp (fn a => a, PURE) (* no-op *)
                    | (LUAJIT, Primitives.I54) =>
                        doUnaryExp (fn a => a, PURE) (* no-op *)
                    | _ =>
                        raise CodeGenError
                          ("primop " ^ Primitives.toString prim
                           ^ " is not supported on this target"))
               | Primitives.UChar_fromChar7 =>
                   doUnaryExp (fn a => a, PURE) (* no-op *)
               | Primitives.String7_EQUAL => doBinaryOp (L.EQUAL, PURE)
               | Primitives.String7_LT => doBinaryOp (L.LT, PURE)
               | Primitives.String7_GT => doBinaryOp (L.GT, PURE)
               | Primitives.String7_LE => doBinaryOp (L.LE, PURE)
               | Primitives.String7_GE => doBinaryOp (L.GE, PURE)
               | Primitives.String7_HAT => doBinaryOp (L.CONCAT, PURE)
               | Primitives.String7_size _ =>
                   doUnaryExp (fn a => L.UnaryExp (L.LENGTH, a), PURE)
               | Primitives.String7_str =>
                   doUnaryExp
                     ( fn a =>
                         L.CallExp
                           (L.VarExp (L.PredefinedId "string_char"), vector [a])
                     , PURE
                     )
               | Primitives.String16_EQUAL => doBinaryOp (L.EQUAL, PURE)
               | Primitives.String16_LT =>
                   doBinaryOp (L.LT, PURE) (* big endian *)
               | Primitives.String16_GT =>
                   doBinaryOp (L.GT, PURE) (* big endian *)
               | Primitives.String16_LE =>
                   doBinaryOp (L.LE, PURE) (* big endian *)
               | Primitives.String16_GE =>
                   doBinaryOp (L.GE, PURE) (* big endian *)
               | Primitives.String16_HAT => doBinaryOp (L.CONCAT, PURE)
               | Primitives.String16_size _ =>
                   (case #targetLuaVersion ctx of
                      LUA5_3 =>
                        doUnaryExp
                          ( fn a =>
                              L.BinExp
                                ( L.INTDIV
                                , L.UnaryExp (L.LENGTH, a)
                                , L.ConstExp (L.Numeral "2")
                                )
                          , PURE
                          )
                    | LUAJIT =>
                        doUnaryExp
                          ( fn a =>
                              L.BinExp
                                ( L.DIV
                                , L.UnaryExp (L.LENGTH, a)
                                , L.ConstExp (L.Numeral "2")
                                )
                          , PURE
                          ))
               | Primitives.String32_EQUAL => doBinaryOp (L.EQUAL, PURE)
               | Primitives.String32_LT =>
                   doBinaryOp (L.LT, PURE) (* big endian *)
               | Primitives.String32_GT =>
                   doBinaryOp (L.GT, PURE) (* big endian *)
               | Primitives.String32_LE =>
                   doBinaryOp (L.LE, PURE) (* big endian *)
               | Primitives.String32_GE =>
                   doBinaryOp (L.GE, PURE) (* big endian *)
               | Primitives.String32_HAT => doBinaryOp (L.CONCAT, PURE)
               | Primitives.String32_size _ =>
                   (case #targetLuaVersion ctx of
                      LUA5_3 =>
                        doUnaryExp
                          ( fn a =>
                              L.BinExp
                                ( L.INTDIV
                                , L.UnaryExp (L.LENGTH, a)
                                , L.ConstExp (L.Numeral "4")
                                )
                          , PURE
                          )
                    | LUAJIT =>
                        doUnaryExp
                          ( fn a =>
                              L.BinExp
                                ( L.DIV
                                , L.UnaryExp (L.LENGTH, a)
                                , L.ConstExp (L.Numeral "4")
                                )
                          , PURE
                          ))
               | Primitives.UString_EQUAL => doBinaryOp (L.EQUAL, PURE)
               | Primitives.UString_LT => doBinaryOp (L.LT, PURE)
               | Primitives.UString_GT => doBinaryOp (L.GT, PURE)
               | Primitives.UString_LE => doBinaryOp (L.LE, PURE)
               | Primitives.UString_GE => doBinaryOp (L.GE, PURE)
               | Primitives.UString_HAT => doBinaryOp (L.CONCAT, PURE)
               | Primitives.UString_fromString7 =>
                   doUnaryExp (fn a => a, PURE) (* no-op *)
               | Primitives.UString_uncheckedFromUtf8 =>
                   doUnaryExp (fn a => a, PURE) (* no-op *)
               | Primitives.UString_encodeUtf8 =>
                   doUnaryExp (fn a => a, PURE) (* no-op *)
               | Primitives.Vector_length _ =>
                   doUnaryExp
                     ( fn a => L.IndexExp (a, L.ConstExp (L.LiteralString "n"))
                     , PURE
                     )
               | Primitives.Vector_fromList =>
                   doUnaryExp
                     ( fn xs =>
                         L.CallExp
                           ( L.VarExp (L.PredefinedId "_VectorOrArray_fromList")
                           , vector [xs]
                           )
                     , PURE
                     )
               | Primitives.Vector_concat =>
                   doUnaryExp
                     ( fn a =>
                         L.CallExp
                           ( L.VarExp (L.PredefinedId "_Vector_concat")
                           , vector [a]
                           )
                     , PURE
                     )
               | Primitives.Vector_unsafeFromListRevN _ =>
                   doBinaryExp
                     ( fn (n, xs) =>
                         L.CallExp
                           ( L.VarExp
                               (L.PredefinedId "_Vector_unsafeFromListRevN")
                           , vector [n, xs]
                           )
                     , PURE
                     )
               | Primitives.Array_EQUAL => doBinaryOp (L.EQUAL, PURE)
               | Primitives.Array_length _ =>
                   doUnaryExp
                     ( fn a => L.IndexExp (a, L.ConstExp (L.LiteralString "n"))
                     , PURE
                     )
               | Primitives.Array_fromList =>
                   doUnaryExp
                     ( fn xs =>
                         L.CallExp
                           ( L.VarExp (L.PredefinedId "_VectorOrArray_fromList")
                           , vector [xs]
                           )
                     , PURE
                     )
               | Primitives.Array_array _ =>
                   doBinaryExp
                     ( fn (n, init) =>
                         L.CallExp
                           ( L.VarExp (L.PredefinedId "_Array_array")
                           , vector [n, init]
                           )
                     , IMPURE
                     )
               | Primitives.Unsafe_Vector_sub _ =>
                   doBinaryExp
                     ( fn (vec, i) =>
                         L.IndexExp (vec, L.BinExp
                           (L.PLUS, i, L.ConstExp (L.Numeral "1")))
                     , PURE
                     )
               | Primitives.Unsafe_Array_sub _ =>
                   doBinaryExp
                     ( fn (arr, i) =>
                         L.IndexExp (arr, L.BinExp
                           (L.PLUS, i, L.ConstExp (L.Numeral "1")))
                     , IMPURE
                     )
               | Primitives.Unsafe_Array_update _ =>
                   raise CodeGenError "unexpected Unsafe.Array.update"
               | Primitives.Exception_instanceof =>
                   doBinaryExp
                     ( fn (e, tag) =>
                         L.CallExp
                           ( L.VarExp (L.PredefinedId "__exn_instanceof")
                           , vector [e, tag]
                           )
                     , PURE
                     )
               | Primitives.Lua_sub =>
                   doBinaryExpE (fn (a, b) => L.IndexExp (a, b))
               | Primitives.Lua_set => raise CodeGenError "unexpected Lua.set"
               | Primitives.Lua_isNil =>
                   doUnaryExp
                     (fn a => L.BinExp (L.EQUAL, a, L.ConstExp L.Nil), PURE)
               | Primitives.Lua_EQUAL => doBinaryOpE L.EQUAL
               | Primitives.Lua_NOTEQUAL => doBinaryOpE L.NOTEQUAL
               | Primitives.Lua_LT => doBinaryOpE L.LT
               | Primitives.Lua_GT => doBinaryOpE L.GT
               | Primitives.Lua_LE => doBinaryOpE L.LE
               | Primitives.Lua_GE => doBinaryOpE L.GE
               | Primitives.Lua_PLUS => doBinaryOpE L.PLUS
               | Primitives.Lua_MINUS => doBinaryOpE L.MINUS
               | Primitives.Lua_TIMES => doBinaryOpE L.TIMES
               | Primitives.Lua_DIVIDE => doBinaryOpE L.DIV
               | Primitives.Lua_INTDIV => doBinaryOpE L.INTDIV
               | Primitives.Lua_MOD => doBinaryOpE L.MOD
               | Primitives.Lua_pow => doBinaryOpE L.POW
               | Primitives.Lua_negate =>
                   doUnaryExpE (fn a => L.UnaryExp (L.NEGATE, a))
               | Primitives.Lua_andb =>
                   doBinaryOpE L.BITAND (* not used on LuaJIT *)
               | Primitives.Lua_orb =>
                   doBinaryOpE L.BITOR (* not used on LuaJIT *)
               | Primitives.Lua_xorb =>
                   doBinaryOpE L.BITXOR (* not used on LuaJIT *)
               | Primitives.Lua_notb =>
                   doUnaryExpE (fn a =>
                     L.UnaryExp (L.BITNOT, a)) (* not used on LuaJIT *)
               | Primitives.Lua_LSHIFT =>
                   doBinaryOpE L.LSHIFT (* not used on LuaJIT *)
               | Primitives.Lua_RSHIFT =>
                   doBinaryOpE L.RSHIFT (* not used on LuaJIT *)
               | Primitives.Lua_concat => doBinaryOpE L.CONCAT
               | Primitives.Lua_length =>
                   doUnaryExpE (fn a => L.UnaryExp (L.LENGTH, a))
               | Primitives.Lua_isFalsy =>
                   doUnaryExp (fn a => L.UnaryExp (L.NOT, a), PURE)
               | Primitives.Lua_call =>
                   doTernary (fn (f, args, _) =>
                     L.CallExp (L.VarExp (L.PredefinedId "table_pack"), vector
                       [L.CallExp (f, vector [TableUnpackN args])]))
               | Primitives.Lua_call1 =>
                   doTernary (fn (f, args, _) =>
                     L.CallExp (f, vector [TableUnpackN args]))
               | Primitives.Lua_call2 =>
                   raise CodeGenError "unexpected Lua.call2"
               | Primitives.Lua_call3 =>
                   raise CodeGenError "unexpected Lua.call3"
               | Primitives.Lua_call4 =>
                   raise CodeGenError "unexpected Lua.call4"
               | Primitives.Lua_call5 =>
                   raise CodeGenError "unexpected Lua.call5"
               | Primitives.Lua_call6 =>
                   raise CodeGenError "unexpected Lua.call6"
               | Primitives.Lua_call7 =>
                   raise CodeGenError "unexpected Lua.call7"
               | Primitives.Lua_call8 =>
                   raise CodeGenError "unexpected Lua.call8"
               | Primitives.Lua_call9 =>
                   raise CodeGenError "unexpected Lua.call9"
               | Primitives.Lua_method =>
                   doTernary (fn (obj, name, args) =>
                     L.CallExp (L.VarExp (L.PredefinedId "table_pack"), vector
                       [MethodExp (obj, name, [TableUnpackN args])]))
               | Primitives.Lua_method1 =>
                   doTernary (fn (obj, name, args) =>
                     MethodExp (obj, name, [TableUnpackN args]))
               | Primitives.Lua_method2 =>
                   raise CodeGenError "unexpected Lua.method2"
               | Primitives.Lua_method3 =>
                   raise CodeGenError "unexpected Lua.method3"
               | Primitives.Lua_method4 =>
                   raise CodeGenError "unexpected Lua.method4"
               | Primitives.Lua_method5 =>
                   raise CodeGenError "unexpected Lua.method5"
               | Primitives.Lua_method6 =>
                   raise CodeGenError "unexpected Lua.method6"
               | Primitives.Lua_method7 =>
                   raise CodeGenError "unexpected Lua.method7"
               | Primitives.Lua_method8 =>
                   raise CodeGenError "unexpected Lua.method8"
               | Primitives.Lua_method9 =>
                   raise CodeGenError "unexpected Lua.method9"
               | Primitives.Lua_global =>
                   doUnaryExp
                     ( fn x =>
                         case #targetLuaVersion ctx of
                           LUA5_3 =>
                             L.IndexExp (L.VarExp (L.PredefinedId "_ENV"), x)
                         | LUAJIT =>
                             L.IndexExp (L.VarExp (L.PredefinedId "_G"), x)
                     , DISCARDABLE
                     )
               | Primitives.Lua_setGlobal =>
                   raise CodeGenError "unexpected Lua.setGlobal"
               | Primitives.Lua_newTable => L.TableExp (vector [])
               | Primitives.Lua_newTableWith =>
                   (case args of
                      [entries] => newTableWithImpl entries
                    | _ =>
                        raise CodeGenError
                          ("primop " ^ Primitives.toString prim
                           ^ ": invalid number of arguments"))
               | Primitives.Lua_newTableWithMetatable =>
                   (case args of
                      [entries, meta] =>
                        L.CallExp
                          ( L.VarExp (L.PredefinedId "setmetatable")
                          , vector
                              [newTableWithImpl entries, doExp (ctx, env, meta)]
                          )
                    | _ =>
                        raise CodeGenError
                          ("primop " ^ Primitives.toString prim
                           ^ ": invalid number of arguments"))
               | Primitives.DelimCont_newPromptTag => L.TableExp (vector [])
               | Primitives.assumeDiscardable =>
                   doBinaryExp
                     (fn (f, arg) => L.CallExp (f, vector [arg]), IMPURE)
               | _ =>
                   raise CodeGenError
                     ("primop " ^ Primitives.toString prim
                      ^ " is not supported on Lua backend")
             end
         | (F.LuaCallOp, _ :: f :: args) =>
             L.CallExp (L.VarExp (L.PredefinedId "table_pack"), vector
               [L.CallExp
                  ( doExp (ctx, env, f)
                  , Vector.map (fn x => doExp (ctx, env, x)) (vector args)
                  )])
         | (F.LuaCall1Op, _ :: f :: args) =>
             L.CallExp
               ( doExp (ctx, env, f)
               , Vector.map (fn x => doExp (ctx, env, x)) (vector args)
               )
         | (F.LuaCallNOp n, _) =>
             raise CodeGenError ("unexpected Lua.call" ^ Int.toString n)
         | (F.LuaMethodOp name, _ :: obj :: args) =>
             L.CallExp (L.VarExp (L.PredefinedId "table_pack"), vector
               [L.MethodExp
                  ( doExp (ctx, env, obj)
                  , name
                  , Vector.map (fn x => doExp (ctx, env, x)) (vector args)
                  )])
         | (F.LuaMethod1Op name, _ :: obj :: args) =>
             L.MethodExp
               ( doExp (ctx, env, obj)
               , name
               , Vector.map (fn x => doExp (ctx, env, x)) (vector args)
               )
         | (F.LuaMethodNOp (_, n), _) =>
             raise CodeGenError ("unexpected Lua.method" ^ Int.toString n)
         | (F.JsCallOp, _) =>
             raise CodeGenError "JsCallOp is not supported on Lua backend"
         | (F.JsMethodOp, _) =>
             raise CodeGenError "JsMethodOp is not supported on Lua backend"
         | (F.JsNewOp, _) =>
             raise CodeGenError "JsNewOp is not supported on Lua backend"
         | (primOp, _) =>
             raise CodeGenError
               ("primop " ^ Printer.build (FPrinter.doPrimOp primOp)
                ^ " not implemented yet"))
    | doExp (ctx, env, N.Record fields) = (* non-empty record *)
        let
          val fields =
            Syntax.LabelMap.foldri
              (fn (label, v, acc) =>
                 (LabelToTableKey label, doExp (ctx, env, v)) :: acc) [] fields
        in
          L.TableExp (vector fields)
        end
    | doExp (_, _, N.ExnTag {name, payloadTy = _}) =
        L.TableExp (vector [(L.IntKey 1, L.ConstExp (L.LiteralString name))])
    | doExp (ctx, env, N.Projection {label, record, fieldTypes = _}) =
        let
          val label =
            case label of
              Syntax.NumericLabel n => L.ConstExp (L.Numeral (Int.toString n))
            | Syntax.IdentifierLabel s => L.ConstExp (L.LiteralString s)
        in
          L.IndexExp (doExp (ctx, env, record), label)
        end
    | doExp (ctx, _, N.Abs {contParam, params, body, attr = _}) =
        let
          val env' = {continuations = C.CVarMap.singleton (contParam, RETURN)}
        in
          L.FunctionExp
            ( Vector.map (fn vid => VIdToLua (ctx, vid)) (vector params)
            , vector (doStat (ctx, env', SOME contParam, body))
            )
        end
    | doExp (ctx, env, N.LogicalAnd (x, y)) =
        L.BinExp (L.AND, doExp (ctx, env, x), doExp (ctx, env, y))
    | doExp (ctx, env, N.LogicalOr (x, y)) =
        L.BinExp (L.OR, doExp (ctx, env, x), doExp (ctx, env, y))
  and doDecs (ctx, env, defaultCont, decs, finalExp, revStats: L.Stat list) =
    (case decs of
       [] => List.revAppend (revStats, doStat (ctx, env, defaultCont, finalExp))
     | dec :: decs =>
         let
           fun pure (NONE, _) =
                 doDecs (ctx, env, defaultCont, decs, finalExp, revStats)
             | pure (SOME result, exp) =
                 doDecs
                   ( ctx
                   , env
                   , defaultCont
                   , decs
                   , finalExp
                   , L.ConstStat (result, exp) :: revStats
                   )
           fun discardable (NONE, _) =
                 doDecs (ctx, env, defaultCont, decs, finalExp, revStats)
             | discardable (SOME result, exp) =
                 doDecs
                   ( ctx
                   , env
                   , defaultCont
                   , decs
                   , finalExp
                   , L.ConstStat (result, exp) :: revStats
                   )
           fun impure (NONE, exp) =
                 doDecs
                   ( ctx
                   , env
                   , defaultCont
                   , decs
                   , finalExp
                   , List.revAppend (ExpStat exp, revStats)
                   )
             | impure (SOME result, exp) =
                 doDecs
                   ( ctx
                   , env
                   , defaultCont
                   , decs
                   , finalExp
                   , L.ConstStat (result, exp) :: revStats
                   )
         in
           case dec of
             N.ValDec
               { exp = N.PrimOp {primOp as F.PrimCall prim, tyargs, args}
               , results
               } =>
               let
                 fun action ([], stmt) =
                       doDecs
                         ( ctx
                         , env
                         , defaultCont
                         , decs
                         , finalExp
                         , stmt :: revStats
                         )
                   | action (_ :: _, stmt) =
                       raise CodeGenError
                         ("primop " ^ Primitives.toString prim
                          ^ ": unexpected number of results for an action")
                 fun doUnary f =
                   case args of
                     [a] => f (doExp (ctx, env, a))
                   | _ =>
                       raise CodeGenError
                         ("primop " ^ Primitives.toString prim
                          ^ ": invalid number of arguments")
                 fun doUnaryExp (f, purity) =
                   doUnary (fn a =>
                     case results of
                       [result] =>
                         (case purity of
                            PURE => pure (result, f a)
                          | DISCARDABLE => discardable (result, f a)
                          | IMPURE => impure (result, f a))
                     | _ => raise CodeGenError "unexpected number of results")
                 fun doBinaryRaw f =
                   case args of
                     [a, b] => f (a, b)
                   | _ =>
                       raise CodeGenError
                         ("primop " ^ Primitives.toString prim
                          ^ ": invalid number of arguments")
                 fun doBinary f =
                   doBinaryRaw (fn (a, b) =>
                     f (doExp (ctx, env, a), doExp (ctx, env, b)))
                 fun doBinaryExpRaw (f, purity) =
                   doBinaryRaw (fn (a, b) =>
                     case results of
                       [result] =>
                         (case purity of
                            PURE => pure (result, f (a, b))
                          | DISCARDABLE => discardable (result, f (a, b))
                          | IMPURE => impure (result, f (a, b)))
                     | _ => raise CodeGenError "unexpected number of results")
                 fun doBinaryExp (f, purity) =
                   doBinary (fn (a, b) =>
                     case results of
                       [result] =>
                         (case purity of
                            PURE => pure (result, f (a, b))
                          | DISCARDABLE => discardable (result, f (a, b))
                          | IMPURE => impure (result, f (a, b)))
                     | _ => raise CodeGenError "unexpected number of results")
                 fun doBinaryOp (binop, purity) =
                   doBinaryExp (fn (a, b) => L.BinExp (binop, a, b), purity)
                 fun doTernary f =
                   case args of
                     [a, b, c] =>
                       f ( doExp (ctx, env, a)
                         , doExp (ctx, env, b)
                         , doExp (ctx, env, c)
                         )
                   | _ =>
                       raise CodeGenError
                         ("primop " ^ Primitives.toString prim
                          ^ ": invalid number of arguments")
                 fun getPrimEffect (N.Value (C.PrimEffect e)) = e
                   | getPrimEffect _ = IMPURE
                 fun doUnaryExpE f =
                   case args of
                     [a, e] =>
                       (case results of
                          [result] =>
                            (case getPrimEffect e of
                               PURE => pure (result, f (doExp (ctx, env, a)))
                             | DISCARDABLE =>
                                 discardable (result, f (doExp (ctx, env, a)))
                             | IMPURE =>
                                 impure (result, f (doExp (ctx, env, a))))
                        | _ => raise CodeGenError "unexpected number of results")
                   | _ =>
                       raise CodeGenError
                         ("primop " ^ Primitives.toString prim
                          ^ ": invalid number of arguments")
                 fun doUnaryOpE unop =
                   doUnaryExpE (fn a => L.UnaryExp (unop, a))
                 fun doBinaryExpE f =
                   case args of
                     [a, b, e] =>
                       (case results of
                          [result] =>
                            (case getPrimEffect e of
                               PURE =>
                                 pure (result, f
                                   (doExp (ctx, env, a), doExp (ctx, env, b)))
                             | DISCARDABLE =>
                                 discardable (result, f
                                   (doExp (ctx, env, a), doExp (ctx, env, b)))
                             | IMPURE =>
                                 impure (result, f
                                   (doExp (ctx, env, a), doExp (ctx, env, b))))
                        | _ => raise CodeGenError "unexpected number of results")
                   | _ =>
                       raise CodeGenError
                         ("primop " ^ Primitives.toString prim
                          ^ ": invalid number of arguments")
                 fun doBinaryOpE binop =
                   doBinaryExpE (fn (a, b) => L.BinExp (binop, a, b))
                 fun doCall n =
                   doTernary (fn (f, args, _) =>
                     let
                       val arg = vector [TableUnpackN args]
                       val stat =
                         if List.length results = n then
                           if List.exists Option.isSome results then
                             let
                               fun makeResultVar (SOME r) = (r, L.CONST)
                                 | makeResultVar NONE = (genSym ctx, L.CONST)
                             in
                               L.LocalStat
                                 ( List.map makeResultVar results
                                 , [L.CallExp (f, arg)]
                                 )
                             end
                           else
                             L.CallStat (f, arg)
                         else
                           raise CodeGenError "unexpected number of results"
                     in
                       doDecs
                         ( ctx
                         , env
                         , defaultCont
                         , decs
                         , finalExp
                         , stat :: revStats
                         )
                     end)
                 fun doMethod n =
                   doTernary (fn (obj, name, args) =>
                     let
                       val arg = [TableUnpackN args]
                       val stat =
                         if List.length results = n then
                           if List.exists Option.isSome results then
                             let
                               fun makeResultVar (SOME r) = (r, L.CONST)
                                 | makeResultVar NONE = (genSym ctx, L.CONST)
                             in
                               L.LocalStat
                                 ( List.map makeResultVar results
                                 , [MethodExp (obj, name, arg)]
                                 )
                             end
                           else
                             MethodStat (obj, name, arg)
                         else
                           raise CodeGenError "unexpected number of results"
                     in
                       doDecs
                         ( ctx
                         , env
                         , defaultCont
                         , decs
                         , finalExp
                         , stat :: revStats
                         )
                     end)
               in
                 case prim of
                   Primitives.Ref_set =>
                     doBinary (fn (a, b) =>
                       (* REPRESENTATION_OF_REF *)
                       action (results, L.AssignStat
                         ([L.IndexExp (a, L.ConstExp (L.Numeral "1"))], [b])))
                 | Primitives.Unsafe_Array_update _ =>
                     doTernary (fn (arr, i, v) =>
                       action (results, L.AssignStat
                         ( [L.IndexExp (arr, L.BinExp
                              (L.PLUS, i, L.ConstExp (L.Numeral "1")))]
                         , [v]
                         )))
                 | Primitives.Lua_set =>
                     doTernary (fn (a, b, c) =>
                       action (results, L.AssignStat ([L.IndexExp (a, b)], [c])))
                 | Primitives.Lua_call =>
                     doTernary (fn (f, args, _) =>
                       let
                         val arg = vector [TableUnpackN args]
                       in
                         case results of
                           [SOME result] =>
                             doDecs
                               ( ctx
                               , env
                               , defaultCont
                               , decs
                               , finalExp
                               , L.ConstStat
                                   ( result
                                   , L.CallExp
                                       ( L.VarExp (L.PredefinedId "table_pack")
                                       , vector [L.CallExp (f, arg)]
                                       )
                                   ) :: revStats
                               )
                         | [NONE] =>
                             doDecs
                               ( ctx
                               , env
                               , defaultCont
                               , decs
                               , finalExp
                               , L.CallStat (f, arg) :: revStats
                               )
                         | _ =>
                             raise CodeGenError "unexpected number of results"
                       end)
                 | Primitives.Lua_call1 => doCall 1
                 | Primitives.Lua_call2 => doCall 2
                 | Primitives.Lua_call3 => doCall 3
                 | Primitives.Lua_call4 => doCall 4
                 | Primitives.Lua_call5 => doCall 5
                 | Primitives.Lua_call6 => doCall 6
                 | Primitives.Lua_call7 => doCall 7
                 | Primitives.Lua_call8 => doCall 8
                 | Primitives.Lua_call9 => doCall 9
                 | Primitives.Lua_method =>
                     doTernary (fn (obj, name, args) =>
                       let
                         val arg = [TableUnpackN args]
                         val stat =
                           case results of
                             [SOME result] =>
                               L.ConstStat
                                 ( result
                                 , L.CallExp
                                     ( L.VarExp (L.PredefinedId "table_pack")
                                     , vector [MethodExp (obj, name, arg)]
                                     )
                                 )
                           | [NONE] => MethodStat (obj, name, arg)
                           | _ =>
                               raise CodeGenError "unexpected number of results"
                       in
                         doDecs
                           ( ctx
                           , env
                           , defaultCont
                           , decs
                           , finalExp
                           , stat :: revStats
                           )
                       end)
                 | Primitives.Lua_method1 => doMethod 1
                 | Primitives.Lua_method2 => doMethod 2
                 | Primitives.Lua_method3 => doMethod 3
                 | Primitives.Lua_method4 => doMethod 4
                 | Primitives.Lua_method5 => doMethod 5
                 | Primitives.Lua_method6 => doMethod 6
                 | Primitives.Lua_method7 => doMethod 7
                 | Primitives.Lua_method8 => doMethod 8
                 | Primitives.Lua_method9 => doMethod 9
                 | Primitives.Lua_setGlobal =>
                     doBinary (fn (name, value) =>
                       let
                         val t =
                           case #targetLuaVersion ctx of
                             LUA5_3 =>
                               L.IndexExp
                                 (L.VarExp (L.PredefinedId "_ENV"), name)
                           | LUAJIT =>
                               L.IndexExp (L.VarExp (L.PredefinedId "_G"), name)
                         val stmt = L.AssignStat ([t], [value])
                       in
                         action (results, stmt)
                       end)
                 | _ =>
                     (case results of
                        [result] =>
                          impure (result, doExp
                            ( ctx
                            , env
                            , N.PrimOp
                                {primOp = primOp, tyargs = tyargs, args = args}
                            ))
                      | _ => raise CodeGenError "unexpected number of results")
               end
           | N.ValDec
               { exp =
                   N.PrimOp
                     {primOp = F.LuaCallOp, tyargs = _, args = _ :: f :: args}
               , results = [result]
               } =>
               let
                 val stat =
                   case result of
                     NONE =>
                       L.CallStat
                         ( doExp (ctx, env, f)
                         , Vector.map (fn x => doExp (ctx, env, x))
                             (vector args)
                         )
                   | SOME result =>
                       L.ConstStat
                         ( result
                         , L.CallExp
                             ( L.VarExp (L.PredefinedId "table_pack")
                             , vector
                                 [L.CallExp
                                    ( doExp (ctx, env, f)
                                    , Vector.map (fn x => doExp (ctx, env, x))
                                        (vector args)
                                    )]
                             )
                         )
               in
                 doDecs
                   (ctx, env, defaultCont, decs, finalExp, stat :: revStats)
               end
           | N.ValDec
               { exp =
                   N.PrimOp
                     {primOp = F.LuaCall1Op, tyargs = _, args = _ :: f :: args}
               , results = [result]
               } =>
               let
                 val stat =
                   case result of
                     NONE =>
                       L.CallStat
                         ( doExp (ctx, env, f)
                         , Vector.map (fn x => doExp (ctx, env, x))
                             (vector args)
                         )
                   | SOME result =>
                       L.ConstStat (result, L.CallExp
                         ( doExp (ctx, env, f)
                         , Vector.map (fn x => doExp (ctx, env, x))
                             (vector args)
                         ))
               in
                 doDecs
                   (ctx, env, defaultCont, decs, finalExp, stat :: revStats)
               end
           | N.ValDec
               { exp =
                   N.PrimOp
                     { primOp = F.LuaCallNOp n
                     , tyargs = _
                     , args = _ :: f :: args
                     }
               , results
               } =>
               let
                 val stat =
                   if List.exists Option.isSome results then
                     let
                       fun makeResultVar (SOME r) = (r, L.CONST)
                         | makeResultVar NONE = (genSym ctx, L.CONST)
                     in
                       L.LocalStat
                         ( List.map makeResultVar results
                         , [L.CallExp
                              ( doExp (ctx, env, f)
                              , Vector.map (fn x => doExp (ctx, env, x))
                                  (vector args)
                              )]
                         )
                     end
                   else
                     L.CallStat
                       ( doExp (ctx, env, f)
                       , Vector.map (fn x => doExp (ctx, env, x)) (vector args)
                       )
               in
                 doDecs
                   (ctx, env, defaultCont, decs, finalExp, stat :: revStats)
               end
           | N.ValDec
               { exp =
                   N.PrimOp
                     { primOp = F.LuaMethodOp name
                     , tyargs = _
                     , args = _ :: obj :: args
                     }
               , results = [result]
               } =>
               let
                 val stat =
                   case result of
                     NONE =>
                       L.MethodStat
                         ( doExp (ctx, env, obj)
                         , name
                         , Vector.map (fn x => doExp (ctx, env, x))
                             (vector args)
                         )
                   | SOME result =>
                       L.ConstStat
                         ( result
                         , L.CallExp
                             ( L.VarExp (L.PredefinedId "table_pack")
                             , vector
                                 [L.MethodExp
                                    ( doExp (ctx, env, obj)
                                    , name
                                    , Vector.map (fn x => doExp (ctx, env, x))
                                        (vector args)
                                    )]
                             )
                         )
               in
                 doDecs
                   (ctx, env, defaultCont, decs, finalExp, stat :: revStats)
               end
           | N.ValDec
               { exp =
                   N.PrimOp
                     { primOp = F.LuaMethod1Op name
                     , tyargs = _
                     , args = _ :: obj :: args
                     }
               , results = [result]
               } =>
               let
                 val stat =
                   case result of
                     NONE =>
                       L.MethodStat
                         ( doExp (ctx, env, obj)
                         , name
                         , Vector.map (fn x => doExp (ctx, env, x))
                             (vector args)
                         )
                   | SOME result =>
                       L.ConstStat (result, L.MethodExp
                         ( doExp (ctx, env, obj)
                         , name
                         , Vector.map (fn x => doExp (ctx, env, x))
                             (vector args)
                         ))
               in
                 doDecs
                   (ctx, env, defaultCont, decs, finalExp, stat :: revStats)
               end
           | N.ValDec
               { exp =
                   N.PrimOp
                     { primOp = F.LuaMethodNOp (name, n)
                     , tyargs = _
                     , args = _ :: obj :: args
                     }
               , results
               } =>
               let
                 val stat =
                   if List.exists Option.isSome results then
                     let
                       fun makeResultVar (SOME r) = (r, L.CONST)
                         | makeResultVar NONE = (genSym ctx, L.CONST)
                     in
                       L.LocalStat
                         ( List.map makeResultVar results
                         , [L.MethodExp
                              ( doExp (ctx, env, obj)
                              , name
                              , Vector.map (fn x => doExp (ctx, env, x))
                                  (vector args)
                              )]
                         )
                     end
                   else
                     L.MethodStat
                       ( doExp (ctx, env, obj)
                       , name
                       , Vector.map (fn x => doExp (ctx, env, x)) (vector args)
                       )
               in
                 doDecs
                   (ctx, env, defaultCont, decs, finalExp, stat :: revStats)
               end
           | N.ValDec {exp, results} =>
               (case results of
                  [result] => impure (result, doExp (ctx, env, exp))
                | _ => raise CodeGenError "unexpected number of results")
           | N.RecDec defs =>
               let
                 val (decs', assignments) =
                   List.foldr
                     (fn ( {name, contParam, params, body, attr = _}
                         , (decs, assignments)
                         ) =>
                        let
                          val env' =
                            {continuations =
                               C.CVarMap.singleton (contParam, RETURN)}
                          val dec = (name, L.LATE_INIT)
                          val assignment = L.AssignStat
                            ( [L.VarExp (VIdToLua (ctx, name))]
                            , [L.FunctionExp
                                 ( Vector.map (fn vid => VIdToLua (ctx, vid))
                                     (vector params)
                                 , vector
                                     (doStat (ctx, env', SOME contParam, body))
                                 )]
                            )
                        in
                          (dec :: decs, assignment :: assignments)
                        end) ([], []) defs
               in
                 doDecs (ctx, env, defaultCont, decs, finalExp, List.revAppend
                   (assignments, L.LocalStat (decs', []) :: revStats))
               end
           | N.ContDec {name, params, body} =>
               (case (decs, finalExp) of
                  ([], N.App {applied, cont, args, attr = _}) =>
                    if cont = name then
                      let
                        val stat =
                          if List.exists Option.isSome params then
                            L.LocalStat
                              ( List.map
                                  (fn SOME p => (p, L.CONST)
                                    | NONE => (genSym ctx, L.CONST)) params
                              , [L.CallExp
                                   ( doExp (ctx, env, applied)
                                   , Vector.map (fn x => doExp (ctx, env, x))
                                       (vector args)
                                   )]
                              )
                          else
                            L.CallStat
                              ( doExp (ctx, env, applied)
                              , Vector.map (fn x => doExp (ctx, env, x))
                                  (vector args)
                              )
                      in
                        List.revAppend
                          ( revStats
                          , stat :: doStat (ctx, env, defaultCont, body)
                          )
                      end
                    else
                      List.revAppend
                        ( revStats
                        , doStat (ctx, env, defaultCont, finalExp)
                        ) (* dead continuation elimination *)
                | _ =>
                    let
                      val label = doLabel name
                      val env' =
                        {continuations =
                           C.CVarMap.insert (#continuations env, name, GOTO
                             { label = label
                             , params =
                                 List.map
                                   (Option.map (fn p => VIdToLua (ctx, p)))
                                   params
                             })}
                      val decs' =
                        let
                          val params' =
                            List.mapPartial
                              (Option.map (fn p => (p, L.LATE_INIT))) params
                        in
                          if List.null params' then []
                          else [L.LocalStat (params', [])]
                        end
                    in
                      List.revAppend
                        ( revStats
                        , decs'
                          @
                          L.makeDoStat
                            { loopLike = false
                            , body = doDecs
                                (ctx, env', SOME name, decs, finalExp, [])
                            }
                          @
                          L.LabelStat label
                          :: doStat (ctx, env, defaultCont, body)
                        ) (* enclose with do statement? *)
                    end)
           | N.RecContDec defs =>
               let
                 datatype init =
                   INIT_WITH_VALUES of C.CVar * N.Exp list
                 | NO_INIT
                 val init =
                   case (decs, finalExp) of
                     ([], N.AppCont {applied, args}) =>
                       if
                         List.exists
                           (fn (name, params, _) =>
                              name = applied
                              andalso List.all Option.isSome params) defs
                       then INIT_WITH_VALUES (applied, args)
                       else NO_INIT
                   | _ => NO_INIT
                 val maxParams =
                   List.foldl
                     (fn ((_, params, _), n) =>
                        Int.max (n, List.length
                          (List.filter Option.isSome params))) 0 defs
                 val commonParams = List.tabulate (maxParams, fn _ =>
                   genSym ctx)
                 fun mapCommonParams params =
                   List.rev (#2
                     (List.foldl
                        (fn (SOME _, (c :: rest, acc)) => (rest, SOME c :: acc)
                          | (_, (rest, acc)) => (rest, NONE :: acc))
                        (commonParams, []) params))
                 val env' =
                   {continuations =
                      List.foldl
                        (fn ((name, params, _), m) =>
                           C.CVarMap.insert (m, name, GOTO
                             { label = doLabel name
                             , params =
                                 List.map (Option.map L.UserDefinedId)
                                   (mapCommonParams params)
                             })) (#continuations env) defs}
                 val initAndRest =
                   case init of
                     INIT_WITH_VALUES (initCont, args) =>
                       let
                         val args' = List.map (fn x => doExp (ctx, env, x)) args
                         val decs' =
                           if maxParams > 0 then
                             [L.LocalStat
                                ( List.map (fn v => (v, L.MUTABLE)) commonParams
                                , args'
                                )]
                           else
                             []
                         val jump =
                           case defs of
                             [_] => []
                           | _ => [L.GotoStat (doLabel initCont)]
                       in
                         decs' @ jump
                       end
                   | NO_INIT =>
                       let
                         val decs' =
                           if maxParams > 0 then
                             [L.LocalStat
                                ( List.map (fn v => (v, L.MUTABLE)) commonParams
                                , []
                                )]
                           else
                             []
                       in
                         decs'
                         @
                         L.makeDoStat
                           { loopLike = false
                           , body = doDecs (ctx, env', NONE, decs, finalExp, [])
                           }
                       end
                 val conts =
                   List.map
                     (fn (name, params, body) =>
                        let
                          val dec =
                            let
                              val params' =
                                List.mapPartial
                                  (Option.map (fn v => (v, L.CONST))) params
                            in
                              if List.null params' then
                                []
                              else
                                [L.LocalStat
                                   ( params'
                                   , List.mapPartial
                                       (Option.map (L.VarExp o L.UserDefinedId))
                                       (mapCommonParams params)
                                   )]
                            end
                        in
                          L.LabelStat (doLabel name)
                          ::
                          L.makeDoStat
                            { loopLike = true
                            , body = dec @ doStat (ctx, env', NONE, body)
                            }
                        end) defs
               in
                 List.revAppend (revStats, initAndRest @ List.concat conts)
               end
           | N.ESImportDec _ =>
               raise CodeGenError "_esImport is not supported by Lua backend"
         end)
  and doStat
        (ctx: Context, env: Env, defaultCont: C.CVar option, N.Let {decs, cont}) =
        doDecs (ctx, env, defaultCont, decs, cont, [])
    | doStat (ctx, env, _, N.App {applied, cont, args, attr = _}) =
        (case C.CVarMap.find (#continuations env, cont) of
           SOME (GOTO {label, params}) =>
             let
               val callAndAssign =
                 if List.exists Option.isSome params then
                   if List.all Option.isSome params then
                     [L.AssignStat
                        ( List.map (L.VarExp o Option.valOf) params
                        , [L.CallExp
                             ( doExp (ctx, env, applied)
                             , Vector.map (fn x => doExp (ctx, env, x))
                                 (vector args)
                             )]
                        )]
                   else
                     let
                       val dummy = genSym ctx
                     in
                       [ L.LocalStat ([(dummy, L.LATE_INIT)], [])
                       , L.AssignStat
                           ( List.map
                               (fn SOME p => L.VarExp p
                                 | NONE => L.VarExp (L.UserDefinedId dummy))
                               params
                           , [L.CallExp
                                ( doExp (ctx, env, applied)
                                , Vector.map (fn x => doExp (ctx, env, x))
                                    (vector args)
                                )]
                           )
                       ]
                     end
                 else
                   [L.CallStat
                      ( doExp (ctx, env, applied)
                      , Vector.map (fn x => doExp (ctx, env, x)) (vector args)
                      )]
             in
               callAndAssign @ [L.GotoStat label]
             end
         | SOME RETURN =>
             [L.ReturnStat (vector
                [L.CallExp
                   ( doExp (ctx, env, applied)
                   , Vector.map (fn x => doExp (ctx, env, x)) (vector args)
                   )])] (* tail call *)
         | NONE => raise CodeGenError "undefined continuation")
    | doStat (ctx, env, defaultCont, N.AppCont {applied, args}) =
        applyCont
          ( ctx
          , env
          , defaultCont
          , applied
          , List.map (fn x => doExp (ctx, env, x)) args
          )
    | doStat (ctx, env, defaultCont, N.If {cond, thenCont, elseCont}) =
        let
          fun containsNestedBlock (N.Let {decs, cont}) =
                List.exists containsNestedBlockDec decs
                orelse containsNestedBlock cont
            | containsNestedBlock (N.App _) = false
            | containsNestedBlock (N.AppCont _) = false
            | containsNestedBlock (N.If _) = true
            | containsNestedBlock (N.Handle _) = true
            | containsNestedBlock (N.Raise _) = false
            | containsNestedBlock N.Unreachable = false
          and containsNestedBlockDec (N.ValDec _) = false
            | containsNestedBlockDec (N.RecDec _) = true
            | containsNestedBlockDec (N.ContDec _) = true
            | containsNestedBlockDec (N.RecContDec _) = true
            | containsNestedBlockDec (N.ESImportDec _) =
                false (* cannot occur *)
          fun isSimpleIf (N.If {cond = _, thenCont, elseCont}) =
                not (containsNestedBlock thenCont) andalso isSimpleIf elseCont
            | isSimpleIf stat =
                not (containsNestedBlock stat)
        in
          if N.containsApp thenCont then
            let
              val thenLabel = genSymWithName (ctx, "then")
            in
              if isSimpleIf elseCont then
                L.IfStat
                  ( doExp (ctx, env, cond)
                  , vector [L.GotoStat thenLabel]
                  , vector (doStat (ctx, env, NONE, elseCont))
                  ) :: L.LabelStat thenLabel
                ::
                L.makeDoStat
                  { loopLike = false
                  , body = doStat (ctx, env, defaultCont, thenCont)
                  }
              else
                let
                  val elseLabel = genSymWithName (ctx, "else")
                in
                  L.IfStat
                    ( doExp (ctx, env, cond)
                    , vector [L.GotoStat thenLabel]
                    , vector [L.GotoStat elseLabel]
                    ) :: L.LabelStat thenLabel
                  ::
                  L.makeDoStat
                    {loopLike = false, body = doStat (ctx, env, NONE, thenCont)}
                  @
                  L.LabelStat elseLabel
                  :: doStat (ctx, env, defaultCont, elseCont)
                end
            end
          else if isSimpleIf elseCont then
            [L.IfStat
               ( doExp (ctx, env, cond)
               , vector (doStat (ctx, env, defaultCont, thenCont))
               , vector (doStat (ctx, env, defaultCont, elseCont))
               )]
          else
            L.IfStat
              ( doExp (ctx, env, cond)
              , vector (doStat (ctx, env, NONE, thenCont))
              , vector []
              ) :: doStat (ctx, env, defaultCont, elseCont)
        end
    | doStat
        ( ctx
        , env
        , defaultCont
        , N.Handle {body, handler = (e, h), successfulExitIn, successfulExitOut}
        ) =
        let
          val env' =
            {continuations = C.CVarMap.singleton (successfulExitIn, RETURN)}
          val status = genSymWithName (ctx, "status")
          val resultOrError = e
          val functionExp = L.FunctionExp (vector [], vector
            (doStat (ctx, env', NONE, body)))
          val pcallId =
            if #hasDelimitedContinuations ctx then L.PredefinedId "_handle"
            else L.PredefinedId "pcall"
        in
          [ L.LocalStat
              ( [(status, L.CONST), (resultOrError, L.CONST)]
              , [L.CallExp (L.VarExp pcallId, vector [functionExp])]
              )
          , L.IfStat
              ( L.UnaryExp (L.NOT, L.VarExp (L.UserDefinedId status))
              , vector (doStat (ctx, env, defaultCont, h))
              , vector (applyCont
                  ( ctx
                  , env
                  , defaultCont
                  , successfulExitOut
                  , [L.VarExp (L.UserDefinedId e)]
                  ))
              )
          ]
        end
    | doStat (ctx, env, _, N.Raise ({start as {file, line, column}, ...}, exp)) =
        let
          val exp = doExp (ctx, env, exp)
          val locationInfo =
            if start = SourcePos.nullPos then
              L.ConstExp L.Nil
            else
              L.ConstExp (L.LiteralString
                (OS.Path.file file ^ ":" ^ Int.toString line ^ ":"
                 ^ Int.toString column))
        in
          [L.CallStat
             (L.VarExp (L.PredefinedId "_raise"), vector [exp, locationInfo])]
        end
    | doStat (_, _, _, N.Unreachable) = []

  fun doProgram ctx cont program =
    let val env = {continuations = C.CVarMap.singleton (cont, RETURN)}
    in vector (doStat (ctx, env, SOME cont, program))
    end
  fun doProgramWithContinuations ctx cont program =
    let
      val env = {continuations = C.CVarMap.singleton (cont, RETURN)}
      val func = L.FunctionExp (vector [], vector
        (doStat (ctx, env, SOME cont, program)))
    in
      vector
        [L.ReturnStat (vector
           [L.CallExp (L.VarExp (L.PredefinedId "_run"), vector [func])])]
    end
end;
