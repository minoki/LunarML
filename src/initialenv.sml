(*
 * Copyright (c) 2021 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure InitialEnv :>
sig
  val initialFixityEnv: Fixity.Env
  val VId_true: TypedSyntax.VId
  val VId_false: TypedSyntax.VId
  val VId_nil: TypedSyntax.VId
  val VId_Match: TypedSyntax.VId
  val VId_Bind: TypedSyntax.VId
  val VId_Div: TypedSyntax.VId
  val VId_Overflow: TypedSyntax.VId
  val VId_Size: TypedSyntax.VId
  val VId_Subscript: TypedSyntax.VId
  val VId_Fail: TypedSyntax.VId
  val VId_Match_tag: TypedSyntax.VId
  val VId_Bind_tag: TypedSyntax.VId
  val VId_Div_tag: TypedSyntax.VId
  val VId_Overflow_tag: TypedSyntax.VId
  val VId_Size_tag: TypedSyntax.VId
  val VId_Subscript_tag: TypedSyntax.VId
  val VId_Fail_tag: TypedSyntax.VId
  val VId_Match_predicate: TypedSyntax.VId
  val VId_Bind_predicate: TypedSyntax.VId
  val VId_Div_predicate: TypedSyntax.VId
  val VId_Overflow_predicate: TypedSyntax.VId
  val VId_Size_predicate: TypedSyntax.VId
  val VId_Subscript_predicate: TypedSyntax.VId
  val VId_Fail_predicate: TypedSyntax.VId
  val VId_Fail_payload: TypedSyntax.VId
  val VId_exnName: TypedSyntax.VId
  val VId_abs: TypedSyntax.VId
  val VId_TILDE: TypedSyntax.VId
  val VId_div: TypedSyntax.VId
  val VId_mod: TypedSyntax.VId
  val VId_TIMES: TypedSyntax.VId
  val VId_DIVIDE: TypedSyntax.VId
  val VId_PLUS: TypedSyntax.VId
  val VId_MINUS: TypedSyntax.VId
  val VId_LT: TypedSyntax.VId
  val VId_GT: TypedSyntax.VId
  val VId_LE: TypedSyntax.VId
  val VId_GE: TypedSyntax.VId
  val VId_Int_TILDE: TypedSyntax.VId
  val VId_Int_abs: TypedSyntax.VId
  val VId_Real_abs: TypedSyntax.VId
  val VId_Vector_tabulate: TypedSyntax.VId
  val VId_Vector_concat: TypedSyntax.VId
  val VId_Vector_fromList: TypedSyntax.VId
  val VId_Array_fromList: TypedSyntax.VId
  val VId_Array_tabulate: TypedSyntax.VId
  val VId_Lua_NIL: TypedSyntax.VId
  val VId_Lua_function: TypedSyntax.VId
  val VId_Lua_newTableWith: TypedSyntax.VId
  val VId_Lua_Lib_assert: TypedSyntax.VId
  val VId_Lua_Lib_error: TypedSyntax.VId
  val VId_Lua_Lib_getmetatable: TypedSyntax.VId
  val VId_Lua_Lib_pairs: TypedSyntax.VId
  val VId_Lua_Lib_pcall: TypedSyntax.VId
  val VId_Lua_Lib_setmetatable: TypedSyntax.VId
  val VId_Lua_Lib_math: TypedSyntax.VId
  val VId_Lua_Lib_math_abs: TypedSyntax.VId
  val VId_Lua_Lib_math_type: TypedSyntax.VId
  val VId_Lua_Lib_math_maxinteger: TypedSyntax.VId
  val VId_Lua_Lib_math_mininteger: TypedSyntax.VId
  val VId_Lua_Lib_math_ult: TypedSyntax.VId
  val VId_Lua_Lib_string: TypedSyntax.VId
  val VId_Lua_Lib_string_char: TypedSyntax.VId
  val VId_Lua_Lib_string_format: TypedSyntax.VId
  val VId_Lua_Lib_table: TypedSyntax.VId
  val VId_Lua_Lib_table_concat: TypedSyntax.VId
  val VId_Lua_Lib_table_pack: TypedSyntax.VId
  val VId_Lua_Lib_table_unpack: TypedSyntax.VId
  val VId_Lua_Lib_bit: TypedSyntax.VId
  val VId_Lua_Lib_bit_bnot: TypedSyntax.VId
  val VId_Lua_Lib_bit_band: TypedSyntax.VId
  val VId_Lua_Lib_bit_bor: TypedSyntax.VId
  val VId_Lua_Lib_bit_bxor: TypedSyntax.VId
  val VId_Lua_Lib_bit_lshift: TypedSyntax.VId
  val VId_Lua_Lib_bit_rshift: TypedSyntax.VId
  val VId_Lua_Error: TypedSyntax.VId
  val VId_Lua_Error_predicate: TypedSyntax.VId
  val VId_Lua_Error_payload: TypedSyntax.VId
  val VId_JavaScript_undefined: TypedSyntax.VId
  val VId_JavaScript_null: TypedSyntax.VId
  val VId_JavaScript_function: TypedSyntax.VId
  val VId_JavaScript_encodeUtf8: TypedSyntax.VId
  val VId_JavaScript_decodeUtf8: TypedSyntax.VId
  val VId_JavaScript_Error: TypedSyntax.VId
  val VId_JavaScript_Error_predicate: TypedSyntax.VId
  val VId_JavaScript_Error_payload: TypedSyntax.VId
  val VId_JavaScript_wrapThenable: TypedSyntax.VId
  val VId_JavaScript_unwrapThenable: TypedSyntax.VId
  val VId_String_concat: TypedSyntax.VId
  val VId_String_concatWith: TypedSyntax.VId
  val VId_String_implode: TypedSyntax.VId
  val VId_String_translate: TypedSyntax.VId
  val VId_DelimCont_pushPrompt: TypedSyntax.VId
  val VId_DelimCont_withSubCont: TypedSyntax.VId
  val VId_DelimCont_pushSubCont: TypedSyntax.VId
  val VId_DelimCont_topLevel: TypedSyntax.VId
  val VId_PrimEffect_pure: TypedSyntax.VId
  val VId_PrimEffect_discardable: TypedSyntax.VId
  val VId_PrimEffect_impure: TypedSyntax.VId
  val initialValEnv:
    (TypedSyntax.PureTypeScheme
     * Syntax.ValueConstructorInfo Syntax.IdStatus
     * TypedSyntax.VId) Syntax.VIdMap.map
  val initialEnv: Typing.Env
  val primOverloadEnv: Typing.Env
  val initialTyNameSet: TypedSyntax.TyNameSet.set
end =
struct
  structure PT = PrimTypes
  val initialFixityEnv: Fixity.Env =
    let
      fun mkValConMap xs =
        List.foldl
          (fn (n, m) =>
             Syntax.VIdMap.insert
               (m, Syntax.MkVId n, Syntax.ValueConstructor ()))
          Syntax.VIdMap.empty xs
      fun mkExConMap xs =
        List.foldl
          (fn (n, m) =>
             Syntax.VIdMap.insert
               (m, Syntax.MkVId n, Syntax.ExceptionConstructor))
          Syntax.VIdMap.empty xs
      fun mkTyConMap xs =
        List.foldl
          (fn ((n, y), m) => Syntax.TyConMap.insert (m, Syntax.MkTyCon n, y))
          Syntax.TyConMap.empty xs
      val boolConMap = mkValConMap ["true", "false"]
      val refConMap = mkValConMap ["ref"]
      val listConMap = mkValConMap ["nil", "::"]
    in
      { fixityMap = Syntax.VIdMap.empty
      , dottedFixityMap = Syntax.VIdMap.empty
      , idStatusMap =
          { valMap =
              List.foldl (Syntax.VIdMap.unionWith #2)
                (mkExConMap
                   [ "Match"
                   , "Bind"
                   , "Div"
                   , "Overflow"
                   , "Size"
                   , "Subscript"
                   , "Fail"
                   , "_Prim.Lua.Error"
                   ]) [boolConMap, refConMap, listConMap]
          , tyConMap =
              mkTyConMap
                [("bool", boolConMap), ("ref", refConMap), ("list", listConMap)]
          , strMap = Syntax.StrIdMap.empty
          }
      , sigMap = Syntax.SigIdMap.empty
      , funMap = Syntax.FunIdMap.empty
      }
    end

  val vidCounter = ref ~3
  fun newVId name =
    let val n = !vidCounter
    in vidCounter := n - 1; TypedSyntax.MkVId (name, n)
    end

  (* Ref *)
  (* val VId_ref = Typing.VId_ref *)

  (* Bool *)
  val VId_true = newVId "true"
  val VId_false = newVId "false"

  (* List *)
  val VId_nil = newVId "nil"
  (* val VId_DCOLON = Typing.VId_DCOLON *)

  (* Exception *)
  val VId_Match = newVId "Match"
  val VId_Bind = Typing.VId_Bind (* TypedSyntax.MkVId ("Bind", ~1) *)
  val VId_Div = newVId "Div"
  val VId_Overflow = newVId "Overflow"
  val VId_Size = newVId "Size"
  val VId_Subscript = newVId "Subscript"
  val VId_Fail = newVId "Fail"
  val VId_Match_tag = newVId "Match"
  val VId_Bind_tag = newVId "Bind"
  val VId_Div_tag = newVId "Div"
  val VId_Overflow_tag = newVId "Overflow"
  val VId_Size_tag = newVId "Size"
  val VId_Subscript_tag = newVId "Subscript"
  val VId_Fail_tag = newVId "Fail"
  val VId_Match_predicate = newVId "isMatch"
  val VId_Bind_predicate = newVId "isBind"
  val VId_Div_predicate = newVId "isDiv"
  val VId_Overflow_predicate = newVId "isOverflow"
  val VId_Size_predicate = newVId "isSize"
  val VId_Subscript_predicate = newVId "isSubscript"
  val VId_Fail_predicate = newVId "isFail"
  val VId_Fail_payload = newVId "Fail.payload"
  val VId_exnName = newVId "_Prim.General.exnName"

  (* Overloaded *)
  val VId_abs = newVId "abs"
  val VId_TILDE = newVId "~"
  val VId_div = newVId "div"
  val VId_mod = newVId "mod"
  val VId_TIMES = newVId "*"
  val VId_DIVIDE = newVId "/"
  val VId_PLUS = newVId "+"
  val VId_MINUS = newVId "-"
  val VId_LT = newVId "<"
  val VId_GT = newVId ">"
  val VId_LE = newVId "<="
  val VId_GE = newVId ">="

  (* Int *)
  val VId_Int_TILDE = newVId "_Prim.Int.~"
  val VId_Int_abs = newVId "_Prim.Int.abs"

  (* Real *)
  val VId_Real_abs = newVId "_Prim.Real.abs"

  (* Vector *)
  val VId_Vector_tabulate = newVId "_Prim.Vector.tabulate"
  val VId_Vector_concat = newVId "_Prim.Vector.concat"
  val VId_Vector_fromList = newVId "_Prim.Vector.fromList"

  (* Array *)
  val VId_Array_fromList = newVId "_Prim.Array.fromList"
  val VId_Array_tabulate = newVId "_Prim.Array.tabulate"

  (* Lua interface *)
  val VId_Lua_NIL = newVId "_Prim.Lua.NIL"
  val VId_Lua_function = newVId "_Prim.Lua.function"
  val VId_Lua_newTableWith = newVId "_Prim.Lua.newTableWith"
  val VId_Lua_Lib_assert = newVId "_Prim.Lua.Lib.assert"
  val VId_Lua_Lib_error = newVId "_Prim.Lua.Lib.error"
  val VId_Lua_Lib_getmetatable = newVId "_Prim.Lua.Lib.getmetatable"
  val VId_Lua_Lib_pairs = newVId "_Prim.Lua.Lib.pairs"
  val VId_Lua_Lib_pcall = newVId "_Prim.Lua.Lib.pcall"
  val VId_Lua_Lib_setmetatable = newVId "_Prim.Lua.Lib.setmetatable"
  val VId_Lua_Lib_math = newVId "_Prim.Lua.Lib.math"
  val VId_Lua_Lib_math_abs = newVId "_Prim.Lua.Lib.math.abs"
  val VId_Lua_Lib_math_type = newVId "_Prim.Lua.Lib.math.type'"
  val VId_Lua_Lib_math_maxinteger = newVId "_Prim.Lua.Lib.math.maxinteger"
  val VId_Lua_Lib_math_mininteger = newVId "_Prim.Lua.Lib.math.mininteger"
  val VId_Lua_Lib_math_ult = newVId "_Prim.Lua.Lib.math.ult"
  val VId_Lua_Lib_string = newVId "_Prim.Lua.Lib.string"
  val VId_Lua_Lib_string_char = newVId "_Prim.Lua.Lib.string.char"
  val VId_Lua_Lib_string_format = newVId "_Prim.Lua.Lib.string.format"
  val VId_Lua_Lib_table = newVId "_Prim.Lua.Lib.table"
  val VId_Lua_Lib_table_concat = newVId "_Prim.Lua.Lib.table.concat"
  val VId_Lua_Lib_table_pack = newVId "_Prim.Lua.Lib.table.pack"
  val VId_Lua_Lib_table_unpack = newVId "_Prim.Lua.Lib.table.unpack"
  val VId_Lua_Lib_bit = newVId "_Prim.Lua.Lib.bit" (* LuaJIT *)
  val VId_Lua_Lib_bit_bnot = newVId "_Prim.Lua.Lib.bit.bnot" (* LuaJIT *)
  val VId_Lua_Lib_bit_band = newVId "_Prim.Lua.Lib.bit.band" (* LuaJIT *)
  val VId_Lua_Lib_bit_bor = newVId "_Prim.Lua.Lib.bit.bor" (* LuaJIT *)
  val VId_Lua_Lib_bit_bxor = newVId "_Prim.Lua.Lib.bit.bxor" (* LuaJIT *)
  val VId_Lua_Lib_bit_lshift = newVId "_Prim.Lua.Lib.bit.lshift" (* LuaJIT *)
  val VId_Lua_Lib_bit_rshift = newVId "_Prim.Lua.Lib.bit.rshift" (* LuaJIT *)
  val VId_Lua_Error = newVId "_Prim.Lua.Error"
  val VId_Lua_Error_predicate = newVId "_Prim.Lua.isError"
  val VId_Lua_Error_payload = newVId "_Prim.Lua.Error.payload"

  (* JavaScript interface *)
  val VId_JavaScript_undefined = newVId "_Prim.JavaScript.undefined"
  val VId_JavaScript_null = newVId "_Prim.JavaScript.null"
  val VId_JavaScript_function = newVId "_Prim.JavaScript.function"
  val VId_JavaScript_encodeUtf8 = newVId "_Prim.JavaScript.encodeUtf8"
  val VId_JavaScript_decodeUtf8 = newVId "_Prim.JavaScript.decodeUtf8"
  val VId_JavaScript_Error = newVId "_Prim.JavaScript.Error"
  val VId_JavaScript_Error_predicate = newVId "_Prim.JavaScript.isError"
  val VId_JavaScript_Error_payload = newVId "_Prim.JavaScript.Error.payload"
  val VId_JavaScript_wrapThenable = newVId "_Prim.JavaScript.wrapThenable"
  val VId_JavaScript_unwrapThenable = newVId "_Prim.JavaScript.unwrapThenable"

  (* Other primitives *)
  val VId_String_concat = newVId "_Prim.String.concat"
  val VId_String_concatWith = newVId "_Prim.String.concatWith"
  val VId_String_implode = newVId "_Prim.String.implode"
  val VId_String_translate = newVId "_Prim.String.translate"

  val VId_DelimCont_pushPrompt = newVId "_Prim.DelimCont.pushPrompt"
  val VId_DelimCont_withSubCont = newVId "_Prim.DelimCont.withSubCont"
  val VId_DelimCont_pushSubCont = newVId "_Prim.DelimCont.pushSubCont"
  val VId_DelimCont_topLevel = newVId "_Prim.DelimCont.topLevel"

  val VId_PrimEffect_pure = newVId "PrimEffect.pure"
  val VId_PrimEffect_discardable = newVId "PrimEffect.discardable"
  val VId_PrimEffect_impure = newVId "PrimEffect.impure"

  local
    open Typing
    fun mkValConMap (cons, rep) =
      let
        val allConstructors =
          List.foldl
            (fn ((vid, _), set) => Syntax.VIdSet.add (set, Syntax.MkVId vid))
            Syntax.VIdSet.empty cons
        val constructorsWithPayload =
          List.foldl
            (fn ((vid, TypedSyntax.TypeScheme (_, TypedSyntax.FnType _)), set) =>
               Syntax.VIdSet.add (set, Syntax.MkVId vid)
              | (_, set) => set) Syntax.VIdSet.empty cons
      in
        List.foldl
          (fn ((vid, tysc), m) =>
             let
               val idstatus = Syntax.ValueConstructor
                 { tag = vid
                 , allConstructors = allConstructors
                 , constructorsWithPayload = constructorsWithPayload
                 , representation = rep
                 }
             in
               Syntax.VIdMap.insert (m, Syntax.MkVId vid, (tysc, idstatus))
             end) Syntax.VIdMap.empty cons
      end
    fun mkTopValConMap (cons, rep) =
      let
        val allConstructors =
          List.foldl
            (fn ((vid, _, _), set) => Syntax.VIdSet.add (set, Syntax.MkVId vid))
            Syntax.VIdSet.empty cons
        val constructorsWithPayload =
          List.foldl
            (fn ( (vid, _, TypedSyntax.TypeScheme (_, TypedSyntax.FnType _))
                , set
                ) => Syntax.VIdSet.add (set, Syntax.MkVId vid)
              | (_, set) => set) Syntax.VIdSet.empty cons
      in
        List.foldl
          (fn ((vid, conid, tysc), m) =>
             let
               val idstatus = Syntax.ValueConstructor
                 { tag = vid
                 , allConstructors = allConstructors
                 , constructorsWithPayload = constructorsWithPayload
                 , representation = rep
                 }
             in
               Syntax.VIdMap.insert
                 (m, Syntax.MkVId vid, (tysc, idstatus, conid))
             end) Syntax.VIdMap.empty cons
      end
    val tyVarA = TypedSyntax.MkTyVar ("'a", 0)
    val tyVarB = TypedSyntax.MkTyVar ("'b", 1)
    val tyVarC = TypedSyntax.MkTyVar ("'c", 2)
    val tyVarD = TypedSyntax.MkTyVar ("'d", 3)
    val TypeFunction = TypedSyntax.TypeFunction
    val TypeScheme = TypedSyntax.TypeScheme
    val emptyValEnv = TypedSyntax.emptyValEnv
    fun mkTyVar tv = TypedSyntax.TyVar (SourcePos.nullSpan, tv)
    val tyA: TypedSyntax.PureTy = mkTyVar tyVarA
    val tyB: TypedSyntax.PureTy = mkTyVar tyVarB
    val tyC: TypedSyntax.PureTy = mkTyVar tyVarC
    val tyD: TypedSyntax.PureTy = mkTyVar tyVarD
    infixr -->
    fun a --> b = TypedSyntax.FnType (SourcePos.nullSpan, a, b)
  in
    val
      initialValEnv:
        (TypedSyntax.PureTypeScheme
         * Syntax.ValueConstructorInfo Syntax.IdStatus
         * TypedSyntax.VId) Syntax.VIdMap.map =
      List.foldl (Syntax.VIdMap.unionWith #2) Syntax.VIdMap.empty
        [ mkTopValConMap
            ( [( "ref"
               , VId_ref
               , TypeScheme ([(tyVarA, NONE)], tyA --> PT.ref_ tyA)
               ) (* forall 'a. 'a -> 'a ref *)]
            , Syntax.REP_REF
            )
        , mkTopValConMap
            ( [ ("true", VId_true, TypeScheme ([], PT.bool))
              , ("false", VId_false, TypeScheme ([], PT.bool))
              ]
            , Syntax.REP_BOOL
            )
        , mkTopValConMap
            ( [ ( "nil"
                , VId_nil
                , TypeScheme ([(tyVarA, NONE)], PT.list tyA)
                ) (* forall 'a. 'a list *)
              , ( "::"
                , VId_DCOLON
                , TypeScheme
                    ( [(tyVarA, NONE)]
                    , PT.pair (tyA, PT.list tyA) --> PT.list tyA
                    )
                ) (* forall 'a. 'a * 'a list -> 'a list *)
              ]
            , Syntax.REP_LIST
            )
        , List.foldl
            (fn ((name, vid, tysc), m) =>
               Syntax.VIdMap.insert
                 ( m
                 , Syntax.MkVId name
                 , (tysc, Syntax.ExceptionConstructor, vid)
                 )) Syntax.VIdMap.empty
            [ ("Match", VId_Match, TypeScheme ([], PT.exn))
            , ("Bind", VId_Bind, TypeScheme ([], PT.exn))
            , ("Div", VId_Div, TypeScheme ([], PT.exn))
            , ("Overflow", VId_Overflow, TypeScheme ([], PT.exn))
            , ("Size", VId_Size, TypeScheme ([], PT.exn))
            , ("Subscript", VId_Subscript, TypeScheme ([], PT.exn))
            , ("Fail", VId_Fail, TypeScheme ([], PT.string --> PT.exn))
            , ( "_Prim.Lua.Error"
              , VId_Lua_Error
              , TypeScheme ([], PT.lua_value --> PT.exn)
              )
            , ( "_Prim.JavaScript.Error"
              , VId_JavaScript_Error
              , TypeScheme ([], PT.js_value --> PT.exn)
              )
            ]
        , List.foldl
            (fn ((name, vid, tysc), m) =>
               Syntax.VIdMap.insert
                 (m, Syntax.MkVId name, (tysc, Syntax.ValueVariable, vid)))
            Syntax.VIdMap.empty
            [ ("_Prim.Match.tag", VId_Match_tag, TypeScheme ([], PT.exntag))
            , ("_Prim.Bind.tag", VId_Bind_tag, TypeScheme ([], PT.exntag))
            , ("_Prim.Div.tag", VId_Div_tag, TypeScheme ([], PT.exntag))
            , ( "_Prim.Overflow.tag"
              , VId_Overflow_tag
              , TypeScheme ([], PT.exntag)
              )
            , ("_Prim.Size.tag", VId_Size_tag, TypeScheme ([], PT.exntag))
            , ( "_Prim.Subscript.tag"
              , VId_Subscript_tag
              , TypeScheme ([], PT.exntag)
              )
            , ("_Prim.Fail.tag", VId_Fail_tag, TypeScheme ([], PT.exntag))
            , ( "_Prim.isMatch"
              , VId_Match_predicate
              , TypeScheme ([], PT.exn --> PT.bool)
              )
            , ( "_Prim.isBind"
              , VId_Bind_predicate
              , TypeScheme ([], PT.exn --> PT.bool)
              )
            , ( "_Prim.isDiv"
              , VId_Div_predicate
              , TypeScheme ([], PT.exn --> PT.bool)
              )
            , ( "_Prim.isOverflow"
              , VId_Overflow_predicate
              , TypeScheme ([], PT.exn --> PT.bool)
              )
            , ( "_Prim.isSize"
              , VId_Size_predicate
              , TypeScheme ([], PT.exn --> PT.bool)
              )
            , ( "_Prim.isSubscript"
              , VId_Subscript_predicate
              , TypeScheme ([], PT.exn --> PT.bool)
              )
            , ( "_Prim.isFail"
              , VId_Fail_predicate
              , TypeScheme ([], PT.exn --> PT.bool)
              )
            , ( "_Prim.Lua.isError"
              , VId_Lua_Error_predicate
              , TypeScheme ([], PT.exn --> PT.bool)
              )
            , ( "_Prim.JavaScript.isError"
              , VId_JavaScript_Error_predicate
              , TypeScheme ([], PT.exn --> PT.bool)
              )
            , ( "_Prim.Fail.payload"
              , VId_Fail_payload
              , TypeScheme ([], PT.exn --> PT.string)
              )
            , ( "_Prim.Lua.Error.payload"
              , VId_Lua_Error_payload
              , TypeScheme ([], PT.exn --> PT.lua_value)
              )
            , ( "_Prim.JavaScript.Error.payload"
              , VId_JavaScript_Error_payload
              , TypeScheme ([], PT.exn --> PT.js_value)
              )
            , ( "_Prim.General.exnName"
              , VId_exnName
              , TypeScheme ([], PT.exn --> PT.string)
              )
            , ( "_Prim.Vector.fromList"
              , VId_Vector_fromList
              , TypeScheme ([(tyVarA, NONE)], PT.list tyA --> PT.vector tyA)
              )
            , ("_Prim.Int.~", VId_Int_TILDE, TypeScheme ([], PT.int --> PT.int))
            , ("_Prim.Int.abs", VId_Int_abs, TypeScheme ([], PT.int --> PT.int))
            , ( "_Prim.Real.abs"
              , VId_Real_abs
              , TypeScheme ([], PT.real --> PT.real)
              )
            , ( "_Prim.String.concat"
              , VId_String_concat
              , TypeScheme ([], PT.list PT.string --> PT.string)
              )
            , ( "_Prim.String.concatWith"
              , VId_String_concatWith
              , TypeScheme
                  ([], PT.function2 (PT.string, PT.list PT.string) PT.string)
              )
            , ( "_Prim.String.implode"
              , VId_String_implode
              , TypeScheme ([], PT.list PT.char --> PT.string)
              )
            , ( "_Prim.String.translate"
              , VId_String_translate
              , TypeScheme
                  ( []
                  , PT.function2 (PT.char --> PT.string, PT.string) PT.string
                  )
              )
            , ( "_Prim.Vector.tabulate"
              , VId_Vector_tabulate
              , TypeScheme
                  ( [(tyVarA, NONE)]
                  , PT.pair (PT.int, PT.int --> tyA) --> PT.vector tyA
                  )
              )
            , ( "_Prim.Vector.concat"
              , VId_Vector_concat
              , TypeScheme
                  ([(tyVarA, NONE)], PT.list (PT.vector tyA) --> PT.vector tyA)
              )
            , ( "_Prim.Array.fromList"
              , VId_Array_fromList
              , TypeScheme ([(tyVarA, NONE)], PT.list tyA --> PT.array tyA)
              )
            , ( "_Prim.Array.tabulate"
              , VId_Array_tabulate
              , TypeScheme
                  ( [(tyVarA, NONE)]
                  , PT.pair (PT.int, PT.int --> tyA) --> PT.array tyA
                  )
              )
            , ("_Prim.Lua.NIL", VId_Lua_NIL, TypeScheme ([], PT.lua_value))
            , ( "_Prim.Lua.function"
              , VId_Lua_function
              , TypeScheme
                  ( []
                  , (PT.vector PT.lua_value --> PT.vector PT.lua_value)
                    --> PT.lua_value
                  )
              )
            , ( "_Prim.Lua.newTableWith"
              , VId_Lua_newTableWith
              , TypeScheme
                  ( []
                  , PT.vector (PT.pair (PT.string, PT.lua_value))
                    --> PT.lua_value
                  )
              )
            , ( "_Prim.Lua.Lib.assert"
              , VId_Lua_Lib_assert
              , TypeScheme ([], PT.lua_value)
              )
            , ( "_Prim.Lua.Lib.error"
              , VId_Lua_Lib_error
              , TypeScheme ([], PT.lua_value)
              )
            , ( "_Prim.Lua.Lib.getmetatable"
              , VId_Lua_Lib_getmetatable
              , TypeScheme ([], PT.lua_value)
              )
            , ( "_Prim.Lua.Lib.pairs"
              , VId_Lua_Lib_pairs
              , TypeScheme ([], PT.lua_value)
              )
            , ( "_Prim.Lua.Lib.pcall"
              , VId_Lua_Lib_pcall
              , TypeScheme ([], PT.lua_value)
              )
            , ( "_Prim.Lua.Lib.setmetatable"
              , VId_Lua_Lib_setmetatable
              , TypeScheme ([], PT.lua_value)
              )
            , ( "_Prim.Lua.Lib.math"
              , VId_Lua_Lib_math
              , TypeScheme ([], PT.lua_value)
              )
            , ( "_Prim.Lua.Lib.string"
              , VId_Lua_Lib_string
              , TypeScheme ([], PT.lua_value)
              )
            , ( "_Prim.Lua.Lib.table"
              , VId_Lua_Lib_table
              , TypeScheme ([], PT.lua_value)
              )
            , ( "_Prim.Lua.Lib.math.abs"
              , VId_Lua_Lib_math_abs
              , TypeScheme ([], PT.lua_value)
              )
            , ( "_Prim.Lua.Lib.math.type'"
              , VId_Lua_Lib_math_type
              , TypeScheme ([], PT.lua_value)
              )
            , ( "_Prim.Lua.Lib.math.maxinteger"
              , VId_Lua_Lib_math_maxinteger
              , TypeScheme ([], PT.lua_value)
              )
            , ( "_Prim.Lua.Lib.math.mininteger"
              , VId_Lua_Lib_math_mininteger
              , TypeScheme ([], PT.lua_value)
              )
            , ( "_Prim.Lua.Lib.math.ult"
              , VId_Lua_Lib_math_ult
              , TypeScheme ([], PT.lua_value)
              )
            , ( "_Prim.Lua.Lib.string.char"
              , VId_Lua_Lib_string_char
              , TypeScheme ([], PT.lua_value)
              )
            , ( "_Prim.Lua.Lib.string.format"
              , VId_Lua_Lib_string_format
              , TypeScheme ([], PT.lua_value)
              )
            , ( "_Prim.Lua.Lib.table.concat"
              , VId_Lua_Lib_table_concat
              , TypeScheme ([], PT.lua_value)
              )
            , ( "_Prim.Lua.Lib.table.pack"
              , VId_Lua_Lib_table_pack
              , TypeScheme ([], PT.lua_value)
              )
            , ( "_Prim.Lua.Lib.table.unpack"
              , VId_Lua_Lib_table_unpack
              , TypeScheme ([], PT.lua_value)
              )
            , ( "_Prim.Lua.Lib.bit"
              , VId_Lua_Lib_bit
              , TypeScheme ([], PT.lua_value)
              )
            , ( "_Prim.Lua.Lib.bit.bnot"
              , VId_Lua_Lib_bit_bnot
              , TypeScheme ([], PT.lua_value)
              )
            , ( "_Prim.Lua.Lib.bit.band"
              , VId_Lua_Lib_bit_band
              , TypeScheme ([], PT.lua_value)
              )
            , ( "_Prim.Lua.Lib.bit.bor"
              , VId_Lua_Lib_bit_bor
              , TypeScheme ([], PT.lua_value)
              )
            , ( "_Prim.Lua.Lib.bit.bxor"
              , VId_Lua_Lib_bit_bxor
              , TypeScheme ([], PT.lua_value)
              )
            , ( "_Prim.Lua.Lib.bit.lshift"
              , VId_Lua_Lib_bit_lshift
              , TypeScheme ([], PT.lua_value)
              )
            , ( "_Prim.Lua.Lib.bit.rshift"
              , VId_Lua_Lib_bit_rshift
              , TypeScheme ([], PT.lua_value)
              )
            , ( "_Prim.JavaScript.undefined"
              , VId_JavaScript_undefined
              , TypeScheme ([], PT.js_value)
              )
            , ( "_Prim.JavaScript.null"
              , VId_JavaScript_null
              , TypeScheme ([], PT.js_value)
              )
            , ( "_Prim.JavaScript.function"
              , VId_JavaScript_function
              , TypeScheme
                  ([], (PT.vector PT.js_value --> PT.js_value) --> PT.js_value)
              )
            , ( "_Prim.JavaScript.encodeUtf8"
              , VId_JavaScript_encodeUtf8
              , TypeScheme ([], PT.string16 --> PT.string)
              )
            , ( "_Prim.JavaScript.decodeUtf8"
              , VId_JavaScript_decodeUtf8
              , TypeScheme ([], PT.string --> PT.string16)
              )
            , ( "_Prim.JavaScript.wrapThenable"
              , VId_JavaScript_wrapThenable
              , TypeScheme ([], PT.js_value)
              )
            , ( "_Prim.JavaScript.unwrapThenable"
              , VId_JavaScript_unwrapThenable
              , TypeScheme ([], PT.js_value)
              )
            , ( "_Prim.DelimCont.pushPrompt"
              , VId_DelimCont_pushPrompt
              , TypeScheme
                  ( [(tyVarA, NONE)]
                  , PT.function2 (PT.prompt_tag tyA, PT.unit --> tyA) tyA
                  )
              )
            , ( "_Prim.DelimCont.withSubCont"
              , VId_DelimCont_withSubCont
              , TypeScheme
                  ( [(tyVarA, NONE), (tyVarB, NONE)]
                  , PT.function2
                      (PT.prompt_tag tyB, PT.subcont (tyA, tyB) --> tyB) tyA
                  )
              )
            , ( "_Prim.DelimCont.pushSubCont"
              , VId_DelimCont_pushSubCont
              , TypeScheme
                  ( [(tyVarA, NONE), (tyVarB, NONE)]
                  , PT.function2 (PT.subcont (tyA, tyB), PT.unit --> tyA) tyB
                  )
              )
            , ( "_Prim.DelimCont.topLevel"
              , VId_DelimCont_topLevel
              , TypeScheme ([], PT.prompt_tag PT.unit)
              )
            , ( "_Prim.PrimEffect.pure"
              , VId_PrimEffect_pure
              , TypeScheme ([], PT.prim_effect)
              )
            , ( "_Prim.PrimEffect.discardable"
              , VId_PrimEffect_discardable
              , TypeScheme ([], PT.prim_effect)
              )
            , ( "_Prim.PrimEffect.impure"
              , VId_PrimEffect_impure
              , TypeScheme ([], PT.prim_effect)
              )
            ]
        ]
    val initialTyConMap: TypedSyntax.TypeStructure Syntax.TyConMap.map =
      List.foldl
        (fn ((name, tystr), m) =>
           Syntax.TyConMap.insert (m, Syntax.MkTyCon name, tystr))
        Syntax.TyConMap.empty
        [ ( "bool"
          , { typeFunction = TypeFunction ([], PT.bool)
            , valEnv = mkValConMap
                ( [ ("true", TypeScheme ([], PT.bool))
                  , ("false", TypeScheme ([], PT.bool))
                  ]
                , Syntax.REP_BOOL
                )
            }
          )
        , ( "int"
          , {typeFunction = TypeFunction ([], PT.int), valEnv = emptyValEnv}
          )
        , ( "word"
          , {typeFunction = TypeFunction ([], PT.word), valEnv = emptyValEnv}
          )
        , ( "real"
          , {typeFunction = TypeFunction ([], PT.real), valEnv = emptyValEnv}
          )
        , ( "string"
          , {typeFunction = TypeFunction ([], PT.string), valEnv = emptyValEnv}
          )
        , ( "char"
          , {typeFunction = TypeFunction ([], PT.char), valEnv = emptyValEnv}
          )
        , ( "list"
          , { typeFunction = TypeFunction ([tyVarA], PT.list tyA)
            , valEnv = mkValConMap
                ( [ ("nil", TypeScheme ([(tyVarA, NONE)], PT.list tyA))
                  , ( "::"
                    , TypeScheme
                        ( [(tyVarA, NONE)]
                        , PT.pair (tyA, PT.list tyA) --> PT.list tyA
                        )
                    )
                  ]
                , Syntax.REP_LIST
                )
            }
          )
        , ( "ref"
          , { typeFunction = TypeFunction ([tyVarA], PT.ref_ tyA)
            , valEnv = mkValConMap
                ( [("ref", TypeScheme ([(tyVarA, NONE)], tyA --> PT.ref_ tyA))]
                , Syntax.REP_REF
                )
            }
          )
        , ( "exn"
          , {typeFunction = TypeFunction ([], PT.exn), valEnv = emptyValEnv}
          )
        , ( "array"
          , { typeFunction = TypeFunction ([tyVarA], PT.array tyA)
            , valEnv = emptyValEnv
            }
          )
        , ( "vector"
          , { typeFunction = TypeFunction ([tyVarA], PT.vector tyA)
            , valEnv = emptyValEnv
            }
          )
        , ( "_Prim.Char16.char"
          , {typeFunction = TypeFunction ([], PT.char16), valEnv = emptyValEnv}
          )
        , ( "_Prim.String16.string"
          , { typeFunction = TypeFunction ([], PT.string16)
            , valEnv = emptyValEnv
            }
          )
        , ( "_Prim.Int32.int"
          , {typeFunction = TypeFunction ([], PT.int32), valEnv = emptyValEnv}
          )
        , ( "_Prim.Int54.int"
          , {typeFunction = TypeFunction ([], PT.int54), valEnv = emptyValEnv}
          )
        , ( "_Prim.Int64.int"
          , {typeFunction = TypeFunction ([], PT.int64), valEnv = emptyValEnv}
          )
        , ( "_Prim.IntInf.int"
          , {typeFunction = TypeFunction ([], PT.intInf), valEnv = emptyValEnv}
          )
        , ( "_Prim.Word32.word"
          , {typeFunction = TypeFunction ([], PT.word32), valEnv = emptyValEnv}
          )
        , ( "_Prim.Word64.word"
          , {typeFunction = TypeFunction ([], PT.word64), valEnv = emptyValEnv}
          )
        , ( "_Prim.Function2.function2"
          , { typeFunction = TypeFunction
                ([tyVarA, tyVarB, tyVarC], PT.function2 (tyA, tyB) tyC)
            , valEnv = emptyValEnv
            }
          )
        , ( "_Prim.Function3.function3"
          , { typeFunction = TypeFunction
                ( [tyVarA, tyVarB, tyVarC, tyVarD]
                , PT.function3 (tyA, tyB, tyC) tyD
                )
            , valEnv = emptyValEnv
            }
          )
        , ( "_Prim.Lua.value"
          , { typeFunction = TypeFunction ([], PT.lua_value)
            , valEnv = emptyValEnv
            }
          )
        , ( "_Prim.JavaScript.value"
          , { typeFunction = TypeFunction ([], PT.js_value)
            , valEnv = emptyValEnv
            }
          )
        , ( "_Prim.DelimCont.prompt_tag"
          , { typeFunction = TypeFunction ([tyVarA], PT.prompt_tag tyA)
            , valEnv = emptyValEnv
            }
          )
        , ( "_Prim.DelimCont.subcont"
          , { typeFunction = TypeFunction
                ([tyVarA, tyVarB], PT.subcont (tyA, tyB))
            , valEnv = emptyValEnv
            }
          )
        , ( "_Prim.PrimEffect.prim_effect"
          , { typeFunction = TypeFunction ([], PT.prim_effect)
            , valEnv = emptyValEnv
            }
          )
        ]
    val initialTyNameMap: Typing.TyNameAttr TypedSyntax.TyNameMap.map =
      List.foldl TypedSyntax.TyNameMap.insert' TypedSyntax.TyNameMap.empty
        [ ( PT.Names.bool
          , {arity = 0, admitsEquality = false (* true *), overloadClass = NONE}
          )
        , ( PT.Names.int
          , { arity = 0
            , admitsEquality = false (* true *)
            , overloadClass = NONE (* SOME Syntax.CLASS_INT *)
            }
          )
        , ( PT.Names.int32
          , { arity = 0
            , admitsEquality = false (* true *)
            , overloadClass = NONE (* SOME Syntax.CLASS_INT *)
            }
          )
        , ( PT.Names.int54
          , { arity = 0
            , admitsEquality = false (* true *)
            , overloadClass = NONE (* SOME Syntax.CLASS_INT *)
            }
          )
        , ( PT.Names.int64
          , { arity = 0
            , admitsEquality = false (* true *)
            , overloadClass = NONE (* SOME Syntax.CLASS_INT *)
            }
          )
        , ( PT.Names.intInf
          , { arity = 0
            , admitsEquality = false (* true *)
            , overloadClass = NONE (* SOME Syntax.CLASS_INT *)
            }
          )
        , ( PT.Names.word
          , { arity = 0
            , admitsEquality = false (* true *)
            , overloadClass = NONE (* SOME Syntax.CLASS_WORD *)
            }
          )
        , ( PT.Names.word32
          , { arity = 0
            , admitsEquality = false (* true *)
            , overloadClass = NONE (* SOME Syntax.CLASS_WORD *)
            }
          )
        , ( PT.Names.word64
          , { arity = 0
            , admitsEquality = false (* true *)
            , overloadClass = NONE (* SOME Syntax.CLASS_WORD *)
            }
          )
        , ( PT.Names.real
          , { arity = 0
            , admitsEquality = false
            , overloadClass = NONE (* SOME Syntax.CLASS_REAL *)
            }
          )
        , ( PT.Names.char
          , { arity = 0
            , admitsEquality = false (* true *)
            , overloadClass = NONE (* SOME Syntax.CLASS_CHAR *)
            }
          )
        , ( PT.Names.char16
          , { arity = 0
            , admitsEquality = false (* true *)
            , overloadClass = NONE (* SOME Syntax.CLASS_CHAR *)
            }
          )
        , ( PT.Names.string
          , { arity = 0
            , admitsEquality = false (* true *)
            , overloadClass = NONE (* SOME Syntax.CLASS_STRING *)
            }
          )
        , ( PT.Names.string16
          , { arity = 0
            , admitsEquality = false (* true *)
            , overloadClass = NONE (* SOME Syntax.CLASS_STRING *)
            }
          )
        , ( PT.Names.list
          , {arity = 1, admitsEquality = false (* true *), overloadClass = NONE}
          )
        , ( PT.Names.ref_
          , { arity = 1
            , admitsEquality = false (* must be handled specially *)
            , overloadClass = NONE
            }
          )
        , ( PT.Names.exn
          , {arity = 0, admitsEquality = false, overloadClass = NONE}
          )
        , ( PT.Names.array
          , { arity = 1
            , admitsEquality = false (* must be handled specially *)
            , overloadClass = NONE
            }
          )
        , ( PT.Names.vector
          , {arity = 1, admitsEquality = false (* true *), overloadClass = NONE}
          )
        , ( PT.Names.lua_value
          , {arity = 0, admitsEquality = false, overloadClass = NONE}
          )
        , ( PT.Names.js_value
          , {arity = 0, admitsEquality = false, overloadClass = NONE}
          )
        , ( PT.Names.function2
          , {arity = 3, admitsEquality = false, overloadClass = NONE}
          )
        , ( PT.Names.function3
          , {arity = 4, admitsEquality = false, overloadClass = NONE}
          )
        , ( PT.Names.prompt_tag
          , {arity = 1, admitsEquality = false, overloadClass = NONE}
          )
        , ( PT.Names.subcont
          , {arity = 2, admitsEquality = false, overloadClass = NONE}
          )
        , ( PT.Names.prim_effect
          , {arity = 0, admitsEquality = false, overloadClass = NONE}
          )
        ]
    val initialEnv: Typing.Env =
      { valMap =
          Syntax.VIdMap.map
            (fn (tysc, ids, vid) =>
               ( TypedSyntax.thawPureTypeScheme tysc
               , ids
               , TypedSyntax.MkShortVId vid
               )) initialValEnv
      , tyConMap = initialTyConMap
      , tyNameMap = initialTyNameMap
      , strMap = Syntax.StrIdMap.empty
      , sigMap = Syntax.SigIdMap.empty
      , funMap = Syntax.FunIdMap.empty
      , boundTyVars = Syntax.TyVarMap.empty
      }
  end

  val primOverloadEnv: Typing.Env =
    let
      open Typing
      val TypeScheme = TypedSyntax.TypeScheme
      fun mkTyVar tv = TypedSyntax.TyVar (SourcePos.nullSpan, tv)
      val tyVarA = TypedSyntax.MkTyVar ("'a", 0)
      val tyA = mkTyVar tyVarA
      infixr -->
      fun a --> b = TypedSyntax.FnType (SourcePos.nullSpan, a, b)
    in
      { valMap =
          List.foldl
            (fn ((name, vid, tysc), m) =>
               Syntax.VIdMap.insert
                 ( m
                 , Syntax.MkVId name
                 , (tysc, Syntax.ValueVariable, TypedSyntax.MkShortVId vid)
                 )) Syntax.VIdMap.empty
            [ ( "abs"
              , VId_abs
              , TypeScheme
                  ([(tyVarA, SOME TypedSyntax.IsSignedReal)], tyA --> tyA)
              ) (* forall 'a:realint. 'a -> 'a,        default: int -> int *)
            , ( "~"
              , VId_TILDE
              , TypeScheme ([(tyVarA, SOME TypedSyntax.IsRing)], tyA --> tyA)
              ) (* forall 'a:num.     'a -> 'a,        default: int -> int *)
            , ( "div"
              , VId_div
              , TypeScheme
                  ( [(tyVarA, SOME TypedSyntax.IsIntegral)]
                  , PT.pair (tyA, tyA) --> tyA
                  )
              ) (* forall 'a:wordint. 'a * 'a -> 'a,   default: int * int -> int *)
            , ( "mod"
              , VId_mod
              , TypeScheme
                  ( [(tyVarA, SOME TypedSyntax.IsIntegral)]
                  , PT.pair (tyA, tyA) --> tyA
                  )
              ) (* forall 'a:wordint. 'a * 'a -> 'a,   default: int * int -> int *)
            , ( "*"
              , VId_TIMES
              , TypeScheme
                  ( [(tyVarA, SOME TypedSyntax.IsRing)]
                  , PT.pair (tyA, tyA) --> tyA
                  )
              ) (* forall 'a:num.     'a * 'a -> 'a,   default: int * int -> int *)
            , ( "/"
              , VId_DIVIDE
              , TypeScheme
                  ( [(tyVarA, SOME TypedSyntax.IsReal)]
                  , PT.pair (tyA, tyA) --> tyA
                  )
              ) (* forall 'a:Real.    'a * 'a -> 'a,   default: real * real -> real *)
            , ( "+"
              , VId_PLUS
              , TypeScheme
                  ( [(tyVarA, SOME TypedSyntax.IsRing)]
                  , PT.pair (tyA, tyA) --> tyA
                  )
              ) (* forall 'a:num.     'a * 'a -> 'a,   default: int * int -> int *)
            , ( "-"
              , VId_MINUS
              , TypeScheme
                  ( [(tyVarA, SOME TypedSyntax.IsRing)]
                  , PT.pair (tyA, tyA) --> tyA
                  )
              ) (* forall 'a:num.     'a * 'a -> 'a,   default: int * int -> int *)
            , ( "<"
              , VId_LT
              , TypeScheme
                  ( [(tyVarA, SOME TypedSyntax.IsOrdered)]
                  , PT.pair (tyA, tyA) --> PT.bool
                  )
              ) (* forall 'a:numtxt.  'a * 'a -> bool, default: int * int -> bool *)
            , ( ">"
              , VId_GT
              , TypeScheme
                  ( [(tyVarA, SOME TypedSyntax.IsOrdered)]
                  , PT.pair (tyA, tyA) --> PT.bool
                  )
              ) (* forall 'a:numtxt.  'a * 'a -> bool, default: int * int -> bool *)
            , ( "<="
              , VId_LE
              , TypeScheme
                  ( [(tyVarA, SOME TypedSyntax.IsOrdered)]
                  , PT.pair (tyA, tyA) --> PT.bool
                  )
              ) (* forall 'a:numtxt.  'a * 'a -> bool, default: int * int -> bool *)
            , ( ">="
              , VId_GE
              , TypeScheme
                  ( [(tyVarA, SOME TypedSyntax.IsOrdered)]
                  , PT.pair (tyA, tyA) --> PT.bool
                  )
              ) (* forall 'a:numtxt.  'a * 'a -> bool, default: int * int -> bool *)
            ]
      , tyConMap = Syntax.TyConMap.empty
      , tyNameMap = TypedSyntax.TyNameMap.empty
      , strMap = Syntax.StrIdMap.empty
      , sigMap = Syntax.SigIdMap.empty
      , funMap = Syntax.FunIdMap.empty
      , boundTyVars = Syntax.TyVarMap.empty
      }
    end

  val initialTyNameSet =
    let
      open Typing
    in
      TypedSyntax.TyNameSet.fromList
        [ PT.Names.int
        , PT.Names.int32
        , PT.Names.int54
        , PT.Names.int64
        , PT.Names.intInf
        , PT.Names.word
        , PT.Names.word32
        , PT.Names.word64
        , PT.Names.real
        , PT.Names.char
        , PT.Names.char16
        , PT.Names.string
        , PT.Names.string16
        , PT.Names.exn
        , PT.Names.bool
        , PT.Names.ref_
        , PT.Names.list
        , PT.Names.array
        , PT.Names.vector
        , PT.Names.lua_value
        , PT.Names.js_value
        , PT.Names.function2
        , PT.Names.function3
        , PT.Names.prompt_tag
        , PT.Names.subcont
        , PT.Names.prim_effect
        ]
    end
end
