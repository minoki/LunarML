(*
 * Copyright (c) 2024 ARATA Mizuki
 * This file is part of LunarML.
 *)
(* This module does:
 *  * Inlining
 *  * Constant folding
 *)
structure CpsInline:
sig
  val goStat: CpsSimplify.Context * CSyntax.Stat -> CSyntax.Stat
end =
struct
  local structure F = FSyntax structure C = CSyntax structure P = Primitives
  (* datatype frequency = datatype CpsUsageAnalysis.frequency *)
  in
    type Context = CpsSimplify.Context
    datatype simplify_result =
      VALUE of C.Value
    | SIMPLE_EXP of C.SimpleExp
    | NOT_SIMPLIFIED
    type value_info =
      {exp: C.SimpleExp option (* , isDiscardableFunction : bool *)}
    type env = value_info TypedSyntax.VIdMap.map
    fun isInt32 (x: IntInf.int) = ~0x80000000 <= x andalso x <= 0x7fffffff
    fun isInt54 (x: IntInf.int) =
      ~0x20000000000000 <= x andalso x <= 0x1fffffffffffff
    fun isInt64 (x: IntInf.int) =
      ~0x8000000000000000 <= x andalso x <= 0x7fffffffffffffff
    fun isInt (P.INT, _) = false
      | isInt (P.I32, x) = isInt32 x
      | isInt (P.I54, x) = isInt54 x
      | isInt (P.I64, x) = isInt64 x
      | isInt (P.INT_INF, _) = true
    fun wraparound (P.INT, x: IntInf.int) = x
      | wraparound (P.I32, x) =
          (x + 0x80000000) mod 0x100000000 - 0x80000000
      | wraparound (P.I54, x) =
          (x + 0x20000000000000) mod 0x40000000000000 - 0x20000000000000
      | wraparound (P.I64, x) =
          (x + 0x8000000000000000) mod 0x10000000000000000 - 0x8000000000000000
      | wraparound (P.INT_INF, x) = x
    fun min3 (x, y, z) =
      IntInf.min (x, IntInf.min (y, z))
    fun max3 (x, y, z) =
      IntInf.max (x, IntInf.max (y, z))
    fun simplifySimpleExp (_: Context, _: env, C.Record _) = NOT_SIMPLIFIED
      | simplifySimpleExp (ctx, env, C.PrimOp {primOp, tyargs, args}) =
          (case (primOp, args) of
             (F.ListOp, []) =>
               (case tyargs of
                  [elemTy] => VALUE (C.TypedNil elemTy) (* empty list *)
                | _ => raise Fail "invalid ListOp")
           | (F.PrimCall P.JavaScript_call, [f, C.Var args, e]) =>
               (case TypedSyntax.VIdMap.find (env, args) of
                  SOME
                    { exp =
                        SOME (C.PrimOp {primOp = F.VectorOp, tyargs = _, args})
                    , ...
                    } =>
                    SIMPLE_EXP (C.PrimOp
                      {primOp = F.JsCallOp, tyargs = [], args = e :: f :: args})
                | _ => NOT_SIMPLIFIED)
           | (F.PrimCall P.JavaScript_method, [obj, name, C.Var args, e]) =>
               (case TypedSyntax.VIdMap.find (env, args) of
                  SOME
                    { exp =
                        SOME (C.PrimOp {primOp = F.VectorOp, tyargs = _, args})
                    , ...
                    } =>
                    SIMPLE_EXP (C.PrimOp
                      { primOp = F.JsMethodOp
                      , tyargs = []
                      , args = e :: obj :: name :: args
                      })
                | _ => NOT_SIMPLIFIED)
           | (F.PrimCall P.JavaScript_new, [ctor, C.Var args, e]) =>
               (case TypedSyntax.VIdMap.find (env, args) of
                  SOME
                    { exp =
                        SOME (C.PrimOp {primOp = F.VectorOp, tyargs = _, args})
                    , ...
                    } =>
                    SIMPLE_EXP (C.PrimOp
                      { primOp = F.JsNewOp
                      , tyargs = []
                      , args = e :: ctor :: args
                      })
                | _ => NOT_SIMPLIFIED)
           | (F.PrimCall P.Lua_call, [ctor, C.Var args, e]) =>
               (case TypedSyntax.VIdMap.find (env, args) of
                  SOME
                    { exp =
                        SOME (C.PrimOp {primOp = F.VectorOp, tyargs = _, args})
                    , ...
                    } =>
                    SIMPLE_EXP (C.PrimOp
                      { primOp = F.LuaCallOp
                      , tyargs = []
                      , args = e :: ctor :: args
                      })
                | _ => NOT_SIMPLIFIED)
           | (F.PrimCall P.Lua_call1, [ctor, C.Var args, e]) =>
               (case TypedSyntax.VIdMap.find (env, args) of
                  SOME
                    { exp =
                        SOME (C.PrimOp {primOp = F.VectorOp, tyargs = _, args})
                    , ...
                    } =>
                    SIMPLE_EXP (C.PrimOp
                      { primOp = F.LuaCall1Op
                      , tyargs = []
                      , args = e :: ctor :: args
                      })
                | _ => NOT_SIMPLIFIED)
           | (F.PrimCall P.Lua_call2, [ctor, C.Var args, e]) =>
               (case TypedSyntax.VIdMap.find (env, args) of
                  SOME
                    { exp =
                        SOME (C.PrimOp {primOp = F.VectorOp, tyargs = _, args})
                    , ...
                    } =>
                    SIMPLE_EXP (C.PrimOp
                      { primOp = F.LuaCallNOp 2
                      , tyargs = []
                      , args = e :: ctor :: args
                      })
                | _ => NOT_SIMPLIFIED)
           | (F.PrimCall P.Lua_call3, [ctor, C.Var args, e]) =>
               (case TypedSyntax.VIdMap.find (env, args) of
                  SOME
                    { exp =
                        SOME (C.PrimOp {primOp = F.VectorOp, tyargs = _, args})
                    , ...
                    } =>
                    SIMPLE_EXP (C.PrimOp
                      { primOp = F.LuaCallNOp 3
                      , tyargs = []
                      , args = e :: ctor :: args
                      })
                | _ => NOT_SIMPLIFIED)
           | (F.PrimCall P.Lua_call4, [ctor, C.Var args, e]) =>
               (case TypedSyntax.VIdMap.find (env, args) of
                  SOME
                    { exp =
                        SOME (C.PrimOp {primOp = F.VectorOp, tyargs = _, args})
                    , ...
                    } =>
                    SIMPLE_EXP (C.PrimOp
                      { primOp = F.LuaCallNOp 4
                      , tyargs = []
                      , args = e :: ctor :: args
                      })
                | _ => NOT_SIMPLIFIED)
           | (F.PrimCall P.Lua_call5, [ctor, C.Var args, e]) =>
               (case TypedSyntax.VIdMap.find (env, args) of
                  SOME
                    { exp =
                        SOME (C.PrimOp {primOp = F.VectorOp, tyargs = _, args})
                    , ...
                    } =>
                    SIMPLE_EXP (C.PrimOp
                      { primOp = F.LuaCallNOp 5
                      , tyargs = []
                      , args = e :: ctor :: args
                      })
                | _ => NOT_SIMPLIFIED)
           | (F.PrimCall P.Lua_call6, [ctor, C.Var args, e]) =>
               (case TypedSyntax.VIdMap.find (env, args) of
                  SOME
                    { exp =
                        SOME (C.PrimOp {primOp = F.VectorOp, tyargs = _, args})
                    , ...
                    } =>
                    SIMPLE_EXP (C.PrimOp
                      { primOp = F.LuaCallNOp 6
                      , tyargs = []
                      , args = e :: ctor :: args
                      })
                | _ => NOT_SIMPLIFIED)
           | (F.PrimCall P.Lua_call7, [ctor, C.Var args, e]) =>
               (case TypedSyntax.VIdMap.find (env, args) of
                  SOME
                    { exp =
                        SOME (C.PrimOp {primOp = F.VectorOp, tyargs = _, args})
                    , ...
                    } =>
                    SIMPLE_EXP (C.PrimOp
                      { primOp = F.LuaCallNOp 7
                      , tyargs = []
                      , args = e :: ctor :: args
                      })
                | _ => NOT_SIMPLIFIED)
           | (F.PrimCall P.Lua_call8, [ctor, C.Var args, e]) =>
               (case TypedSyntax.VIdMap.find (env, args) of
                  SOME
                    { exp =
                        SOME (C.PrimOp {primOp = F.VectorOp, tyargs = _, args})
                    , ...
                    } =>
                    SIMPLE_EXP (C.PrimOp
                      { primOp = F.LuaCallNOp 8
                      , tyargs = []
                      , args = e :: ctor :: args
                      })
                | _ => NOT_SIMPLIFIED)
           | (F.PrimCall P.Lua_call9, [ctor, C.Var args, e]) =>
               (case TypedSyntax.VIdMap.find (env, args) of
                  SOME
                    { exp =
                        SOME (C.PrimOp {primOp = F.VectorOp, tyargs = _, args})
                    , ...
                    } =>
                    SIMPLE_EXP (C.PrimOp
                      { primOp = F.LuaCallNOp 9
                      , tyargs = []
                      , args = e :: ctor :: args
                      })
                | _ => NOT_SIMPLIFIED)
           | ( F.PrimCall P.Lua_method
             , [ctor, C.StringConst name, C.Var args, e]
             ) =>
               if LuaWriter.isLuaIdentifier name then
                 case TypedSyntax.VIdMap.find (env, args) of
                   SOME
                     { exp =
                         SOME (C.PrimOp {primOp = F.VectorOp, tyargs = _, args})
                     , ...
                     } =>
                     SIMPLE_EXP (C.PrimOp
                       { primOp = F.LuaMethodOp name
                       , tyargs = []
                       , args = e :: ctor :: args
                       })
                 | _ => NOT_SIMPLIFIED
               else
                 NOT_SIMPLIFIED
           | ( F.PrimCall P.Lua_method1
             , [ctor, C.StringConst name, C.Var args, e]
             ) =>
               if LuaWriter.isLuaIdentifier name then
                 case TypedSyntax.VIdMap.find (env, args) of
                   SOME
                     { exp =
                         SOME (C.PrimOp {primOp = F.VectorOp, tyargs = _, args})
                     , ...
                     } =>
                     SIMPLE_EXP (C.PrimOp
                       { primOp = F.LuaMethod1Op name
                       , tyargs = []
                       , args = e :: ctor :: args
                       })
                 | _ => NOT_SIMPLIFIED
               else
                 NOT_SIMPLIFIED
           | ( F.PrimCall P.Lua_method2
             , [ctor, C.StringConst name, C.Var args, e]
             ) =>
               if LuaWriter.isLuaIdentifier name then
                 case TypedSyntax.VIdMap.find (env, args) of
                   SOME
                     { exp =
                         SOME (C.PrimOp {primOp = F.VectorOp, tyargs = _, args})
                     , ...
                     } =>
                     SIMPLE_EXP (C.PrimOp
                       { primOp = F.LuaMethodNOp (name, 2)
                       , tyargs = []
                       , args = e :: ctor :: args
                       })
                 | _ => NOT_SIMPLIFIED
               else
                 NOT_SIMPLIFIED
           | ( F.PrimCall P.Lua_method3
             , [ctor, C.StringConst name, C.Var args, e]
             ) =>
               if LuaWriter.isLuaIdentifier name then
                 case TypedSyntax.VIdMap.find (env, args) of
                   SOME
                     { exp =
                         SOME (C.PrimOp {primOp = F.VectorOp, tyargs = _, args})
                     , ...
                     } =>
                     SIMPLE_EXP (C.PrimOp
                       { primOp = F.LuaMethodNOp (name, 3)
                       , tyargs = []
                       , args = e :: ctor :: args
                       })
                 | _ => NOT_SIMPLIFIED
               else
                 NOT_SIMPLIFIED
           | ( F.PrimCall P.Lua_method4
             , [ctor, C.StringConst name, C.Var args, e]
             ) =>
               if LuaWriter.isLuaIdentifier name then
                 case TypedSyntax.VIdMap.find (env, args) of
                   SOME
                     { exp =
                         SOME (C.PrimOp {primOp = F.VectorOp, tyargs = _, args})
                     , ...
                     } =>
                     SIMPLE_EXP (C.PrimOp
                       { primOp = F.LuaMethodNOp (name, 4)
                       , tyargs = []
                       , args = e :: ctor :: args
                       })
                 | _ => NOT_SIMPLIFIED
               else
                 NOT_SIMPLIFIED
           | ( F.PrimCall P.Lua_method5
             , [ctor, C.StringConst name, C.Var args, e]
             ) =>
               if LuaWriter.isLuaIdentifier name then
                 case TypedSyntax.VIdMap.find (env, args) of
                   SOME
                     { exp =
                         SOME (C.PrimOp {primOp = F.VectorOp, tyargs = _, args})
                     , ...
                     } =>
                     SIMPLE_EXP (C.PrimOp
                       { primOp = F.LuaMethodNOp (name, 5)
                       , tyargs = []
                       , args = e :: ctor :: args
                       })
                 | _ => NOT_SIMPLIFIED
               else
                 NOT_SIMPLIFIED
           | ( F.PrimCall P.Lua_method6
             , [ctor, C.StringConst name, C.Var args, e]
             ) =>
               if LuaWriter.isLuaIdentifier name then
                 case TypedSyntax.VIdMap.find (env, args) of
                   SOME
                     { exp =
                         SOME (C.PrimOp {primOp = F.VectorOp, tyargs = _, args})
                     , ...
                     } =>
                     SIMPLE_EXP (C.PrimOp
                       { primOp = F.LuaMethodNOp (name, 6)
                       , tyargs = []
                       , args = e :: ctor :: args
                       })
                 | _ => NOT_SIMPLIFIED
               else
                 NOT_SIMPLIFIED
           | ( F.PrimCall P.Lua_method7
             , [ctor, C.StringConst name, C.Var args, e]
             ) =>
               if LuaWriter.isLuaIdentifier name then
                 case TypedSyntax.VIdMap.find (env, args) of
                   SOME
                     { exp =
                         SOME (C.PrimOp {primOp = F.VectorOp, tyargs = _, args})
                     , ...
                     } =>
                     SIMPLE_EXP (C.PrimOp
                       { primOp = F.LuaMethodNOp (name, 7)
                       , tyargs = []
                       , args = e :: ctor :: args
                       })
                 | _ => NOT_SIMPLIFIED
               else
                 NOT_SIMPLIFIED
           | ( F.PrimCall P.Lua_method8
             , [ctor, C.StringConst name, C.Var args, e]
             ) =>
               if LuaWriter.isLuaIdentifier name then
                 case TypedSyntax.VIdMap.find (env, args) of
                   SOME
                     { exp =
                         SOME (C.PrimOp {primOp = F.VectorOp, tyargs = _, args})
                     , ...
                     } =>
                     SIMPLE_EXP (C.PrimOp
                       { primOp = F.LuaMethodNOp (name, 8)
                       , tyargs = []
                       , args = e :: ctor :: args
                       })
                 | _ => NOT_SIMPLIFIED
               else
                 NOT_SIMPLIFIED
           | ( F.PrimCall P.Lua_method9
             , [ctor, C.StringConst name, C.Var args, e]
             ) =>
               if LuaWriter.isLuaIdentifier name then
                 case TypedSyntax.VIdMap.find (env, args) of
                   SOME
                     { exp =
                         SOME (C.PrimOp {primOp = F.VectorOp, tyargs = _, args})
                     , ...
                     } =>
                     SIMPLE_EXP (C.PrimOp
                       { primOp = F.LuaMethodNOp (name, 9)
                       , tyargs = []
                       , args = e :: ctor :: args
                       })
                 | _ => NOT_SIMPLIFIED
               else
                 NOT_SIMPLIFIED
           | (F.DataTagAsStringOp _, [C.Var x]) =>
               (case TypedSyntax.VIdMap.find (env, x) of
                  SOME
                    { exp =
                        SOME
                          (C.PrimOp {primOp = F.ConstructValOp {tag, ...}, ...})
                    , ...
                    } => VALUE (C.StringConst tag)
                | SOME
                    { exp =
                        SOME
                          (C.PrimOp
                             { primOp = F.ConstructValWithPayloadOp {tag, ...}
                             , ...
                             })
                    , ...
                    } => VALUE (C.StringConst tag)
                | _ => NOT_SIMPLIFIED)
           | (F.DataTagAsString16Op _, [C.Var x]) =>
               (case TypedSyntax.VIdMap.find (env, x) of
                  SOME
                    { exp =
                        SOME
                          (C.PrimOp {primOp = F.ConstructValOp {tag, ...}, ...})
                    , ...
                    } =>
                    VALUE
                      (C.String16Const
                         (Vector.tabulate (String.size tag, fn i =>
                            ord (String.sub (tag, i))))) (* Assume tag is ASCII *)
                | SOME
                    { exp =
                        SOME
                          (C.PrimOp
                             { primOp = F.ConstructValWithPayloadOp {tag, ...}
                             , ...
                             })
                    , ...
                    } =>
                    VALUE
                      (C.String16Const
                         (Vector.tabulate (String.size tag, fn i =>
                            ord (String.sub (tag, i))))) (* Assume tag is ASCII *)
                | _ => NOT_SIMPLIFIED)
           | (F.DataPayloadOp {tag, ...}, [C.Var x]) =>
               (case TypedSyntax.VIdMap.find (env, x) of
                  SOME
                    { exp =
                        SOME
                          (C.PrimOp
                             { primOp =
                                 F.ConstructValWithPayloadOp {tag = tag', ...}
                             , args = [payload]
                             , ...
                             })
                    , ...
                    } => if tag = tag' then VALUE payload else NOT_SIMPLIFIED
                | _ => NOT_SIMPLIFIED)
           | (F.PrimCall P.Bool_EQUAL, [x, C.BoolConst true]) => VALUE x
           | (F.PrimCall P.Bool_EQUAL, [C.BoolConst true, x]) => VALUE x
           | (F.PrimCall P.Bool_EQUAL, [x, C.BoolConst false]) =>
               SIMPLE_EXP
                 (C.PrimOp
                    {primOp = F.PrimCall P.Bool_not, tyargs = [], args = [x]})
           | (F.PrimCall P.Bool_EQUAL, [C.BoolConst false, x]) =>
               SIMPLE_EXP
                 (C.PrimOp
                    {primOp = F.PrimCall P.Bool_not, tyargs = [], args = [x]})
           | (F.PrimCall P.Bool_not, [C.BoolConst x]) =>
               VALUE (C.BoolConst (not x))
           | (F.PrimCall P.Bool_not, [C.Var x]) =>
               (case TypedSyntax.VIdMap.find (env, x) of
                  SOME
                    { exp =
                        SOME
                          (C.PrimOp
                             { primOp = F.PrimCall P.Bool_not
                             , tyargs = _
                             , args = [v]
                             })
                    , ...
                    } => VALUE v
                | _ => NOT_SIMPLIFIED)
           | (F.PrimCall P.String_EQUAL, [C.StringConst x, C.StringConst y]) =>
               VALUE (C.BoolConst (x = y))
           | (F.PrimCall P.String_HAT, [C.StringConst x, C.StringConst y]) =>
               VALUE (C.StringConst (x ^ y))
           | ( F.PrimCall P.String16_EQUAL
             , [C.String16Const x, C.String16Const y]
             ) => VALUE (C.BoolConst (x = y))
           | (F.PrimCall P.String16_HAT, [C.String16Const x, C.String16Const y]) =>
               VALUE (C.String16Const (Vector.concat [x, y]))
           | ( F.PrimCall (P.Int_EQUAL w)
             , [C.IntConst (w', x), C.IntConst (w'', y)]
             ) =>
               if w = w' andalso w = w'' then VALUE (C.BoolConst (x = y))
               else NOT_SIMPLIFIED
           | ( F.PrimCall (P.Int_LT w)
             , [C.IntConst (w', x), C.IntConst (w'', y)]
             ) =>
               if w = w' andalso w = w'' then VALUE (C.BoolConst (x < y))
               else NOT_SIMPLIFIED
           | ( F.PrimCall (P.Int_LE w)
             , [C.IntConst (w', x), C.IntConst (w'', y)]
             ) =>
               if w = w' andalso w = w'' then VALUE (C.BoolConst (x <= y))
               else NOT_SIMPLIFIED
           | ( F.PrimCall (P.Int_GT w)
             , [C.IntConst (w', x), C.IntConst (w'', y)]
             ) =>
               if w = w' andalso w = w'' then VALUE (C.BoolConst (x > y))
               else NOT_SIMPLIFIED
           | ( F.PrimCall (P.Int_GE w)
             , [C.IntConst (w', x), C.IntConst (w'', y)]
             ) =>
               if w = w' andalso w = w'' then VALUE (C.BoolConst (x >= y))
               else NOT_SIMPLIFIED
           | ( F.PrimCall (P.Int_PLUS w)
             , [C.IntConst (w', x), C.IntConst (w'', y)]
             ) =>
               if w = w' andalso w = w'' then
                 let
                   val z = x + y
                 in
                   if
                     isInt (w, z)
                     orelse (min3 (x, y, 0) <= z andalso z <= max3 (x, y, 0))
                   then VALUE (C.IntConst (w, z))
                   else NOT_SIMPLIFIED
                 end
               else
                 NOT_SIMPLIFIED
           | (F.PrimCall (P.Int_PLUS w), [C.IntConst (w', 0), y]) =>
               if w = w' then VALUE y else NOT_SIMPLIFIED
           | (F.PrimCall (P.Int_PLUS w), [x, C.IntConst (w', 0)]) =>
               if w = w' then VALUE x else NOT_SIMPLIFIED
           | ( F.PrimCall (P.Int_PLUS_wrapping w)
             , [C.IntConst (w', x), C.IntConst (w'', y)]
             ) =>
               if w <> P.INT andalso w = w' andalso w = w'' then
                 VALUE (C.IntConst (w, wraparound (w, x + y)))
               else
                 NOT_SIMPLIFIED
           | (F.PrimCall (P.Int_PLUS_wrapping w), [C.IntConst (w', 0), y]) =>
               if w = w' then VALUE y else NOT_SIMPLIFIED
           | (F.PrimCall (P.Int_PLUS_wrapping w), [x, C.IntConst (w', 0)]) =>
               if w = w' then VALUE x else NOT_SIMPLIFIED
           | ( F.PrimCall (P.Int_MINUS w)
             , [C.IntConst (w', x), C.IntConst (w'', y)]
             ) =>
               if w = w' andalso w = w'' then
                 let
                   val z = x - y
                 in
                   if
                     isInt (w, z)
                     orelse (min3 (x, y, 0) <= z andalso z <= max3 (x, y, 0))
                   then VALUE (C.IntConst (w, z))
                   else NOT_SIMPLIFIED
                 end
               else
                 NOT_SIMPLIFIED
           | (F.PrimCall (P.Int_MINUS w), [C.IntConst (w', 0), y]) =>
               if w = w' then
                 SIMPLE_EXP (C.PrimOp
                   { primOp = F.PrimCall (P.Int_TILDE w)
                   , tyargs = []
                   , args = [y]
                   })
               else
                 NOT_SIMPLIFIED
           | (F.PrimCall (P.Int_MINUS w), [x, C.IntConst (w', 0)]) =>
               if w = w' then VALUE x else NOT_SIMPLIFIED
           | ( F.PrimCall (P.Int_MINUS_wrapping w)
             , [C.IntConst (w', x), C.IntConst (w'', y)]
             ) =>
               if w = w' andalso w = w'' then
                 VALUE (C.IntConst (w, wraparound (w, x - y)))
               else
                 NOT_SIMPLIFIED
           | (F.PrimCall (P.Int_MINUS_wrapping w), [C.IntConst (w', 0), y]) =>
               if w = w' then
                 SIMPLE_EXP (C.PrimOp
                   { primOp = F.PrimCall (P.Int_TILDE_wrapping w)
                   , tyargs = []
                   , args = [y]
                   })
               else
                 NOT_SIMPLIFIED
           | (F.PrimCall (P.Int_MINUS_wrapping w), [x, C.IntConst (w', 0)]) =>
               if w = w' then VALUE x else NOT_SIMPLIFIED
           | (F.PrimCall (P.Int_TIMES w), [zero as C.IntConst (w', 0), _]) =>
               if w = w' then VALUE zero else NOT_SIMPLIFIED
           | (F.PrimCall (P.Int_TIMES w), [_, zero as C.IntConst (w', 0)]) =>
               if w = w' then VALUE zero else NOT_SIMPLIFIED
           | (F.PrimCall (P.Int_TIMES w), [C.IntConst (w', 1), y]) =>
               if w = w' then VALUE y else NOT_SIMPLIFIED
           | (F.PrimCall (P.Int_TIMES w), [x, C.IntConst (w', 1)]) =>
               if w = w' then VALUE x else NOT_SIMPLIFIED
           | ( F.PrimCall (P.Int_TIMES w)
             , [x as C.IntConst (w', x'), y as C.IntConst (w'', y')]
             ) =>
               if w = w' andalso w = w'' then
                 let
                   val z = x' * y'
                 in
                   if
                     isInt (w, z)
                     orelse
                     (min3 (x', y', 0) <= z andalso z <= max3 (x', y', 0))
                   then
                     VALUE (C.IntConst (w, z))
                   else if
                     x' = ~1
                   then
                     SIMPLE_EXP (C.PrimOp
                       { primOp = F.PrimCall (P.Int_TILDE w)
                       , tyargs = []
                       , args = [y]
                       })
                   else if
                     y' = ~1
                   then
                     SIMPLE_EXP (C.PrimOp
                       { primOp = F.PrimCall (P.Int_TILDE w)
                       , tyargs = []
                       , args = [x]
                       })
                   else
                     NOT_SIMPLIFIED
                 end
               else
                 NOT_SIMPLIFIED
           | (F.PrimCall (P.Int_TIMES w), [C.IntConst (w', ~1), y]) =>
               if w = w' then
                 SIMPLE_EXP (C.PrimOp
                   { primOp = F.PrimCall (P.Int_TILDE w)
                   , tyargs = []
                   , args = [y]
                   })
               else
                 NOT_SIMPLIFIED
           | (F.PrimCall (P.Int_TIMES w), [x, C.IntConst (w', ~1)]) =>
               if w = w' then
                 SIMPLE_EXP (C.PrimOp
                   { primOp = F.PrimCall (P.Int_TILDE w)
                   , tyargs = []
                   , args = [x]
                   })
               else
                 NOT_SIMPLIFIED
           | ( F.PrimCall (P.Int_TIMES_wrapping w)
             , [zero as C.IntConst (w', 0), _]
             ) => if w = w' then VALUE zero else NOT_SIMPLIFIED
           | ( F.PrimCall (P.Int_TIMES_wrapping w)
             , [_, zero as C.IntConst (w', 0)]
             ) => if w = w' then VALUE zero else NOT_SIMPLIFIED
           | (F.PrimCall (P.Int_TIMES_wrapping w), [C.IntConst (w', 1), y]) =>
               if w = w' then VALUE y else NOT_SIMPLIFIED
           | (F.PrimCall (P.Int_TIMES_wrapping w), [x, C.IntConst (w', 1)]) =>
               if w = w' then VALUE x else NOT_SIMPLIFIED
           | ( F.PrimCall (P.Int_TIMES_wrapping w)
             , [C.IntConst (w', x'), C.IntConst (w'', y')]
             ) =>
               if w = w' andalso w = w'' then
                 VALUE (C.IntConst (w, wraparound (w, x' * y')))
               else
                 NOT_SIMPLIFIED
           | (F.PrimCall (P.Int_TIMES_wrapping w), [C.IntConst (w', ~1), y]) =>
               if w = w' then
                 SIMPLE_EXP (C.PrimOp
                   { primOp = F.PrimCall (P.Int_TILDE_wrapping w)
                   , tyargs = []
                   , args = [y]
                   })
               else
                 NOT_SIMPLIFIED
           | (F.PrimCall (P.Int_TIMES_wrapping w), [x, C.IntConst (w', ~1)]) =>
               if w = w' then
                 SIMPLE_EXP (C.PrimOp
                   { primOp = F.PrimCall (P.Int_TILDE_wrapping w)
                   , tyargs = []
                   , args = [x]
                   })
               else
                 NOT_SIMPLIFIED
           | (F.PrimCall (P.Int_div w), [x, y as C.IntConst (w', y')]) =>
               if w = w' then
                 if y' = 1 then
                   VALUE x
                 else if y' = ~1 then
                   SIMPLE_EXP (C.PrimOp
                     { primOp = F.PrimCall (P.Int_TILDE w)
                     , tyargs = []
                     , args = [x]
                     })
                 else if y' <> 0 then
                   case x of
                     C.IntConst (w'', x') =>
                       if w = w'' then VALUE (C.IntConst (w, x' div y'))
                       else NOT_SIMPLIFIED
                   | _ =>
                       SIMPLE_EXP (C.PrimOp
                         { primOp = F.PrimCall (P.Int_div_unchecked w)
                         , tyargs = []
                         , args = [x, y]
                         })
                 else
                   NOT_SIMPLIFIED
               else
                 NOT_SIMPLIFIED
           | (F.PrimCall (P.Int_mod w), [x, y as C.IntConst (w', y')]) =>
               if w = w' then
                 if y' = 1 orelse y' = ~1 then
                   VALUE (C.IntConst (w, 0))
                 else if y' <> 0 then
                   case x of
                     C.IntConst (w'', x') =>
                       if w = w'' then VALUE (C.IntConst (w, x' mod y'))
                       else NOT_SIMPLIFIED
                   | _ =>
                       SIMPLE_EXP (C.PrimOp
                         { primOp = F.PrimCall (P.Int_mod_unchecked w)
                         , tyargs = []
                         , args = [x, y]
                         })
                 else
                   NOT_SIMPLIFIED
               else
                 NOT_SIMPLIFIED
           | (F.PrimCall (P.Int_quot w), [x, y as C.IntConst (w', y')]) =>
               if w = w' then
                 if y' = 1 then
                   VALUE x
                 else if y' = ~1 then
                   SIMPLE_EXP (C.PrimOp
                     { primOp = F.PrimCall (P.Int_TILDE w)
                     , tyargs = []
                     , args = [x]
                     })
                 else if y' <> 0 then
                   case x of
                     C.IntConst (w'', x') =>
                       if w = w'' then
                         VALUE (C.IntConst (w, IntInf.quot (x', y')))
                       else
                         NOT_SIMPLIFIED
                   | _ =>
                       SIMPLE_EXP (C.PrimOp
                         { primOp = F.PrimCall (P.Int_quot_unchecked w)
                         , tyargs = []
                         , args = [x, y]
                         })
                 else
                   NOT_SIMPLIFIED
               else
                 NOT_SIMPLIFIED
           | (F.PrimCall (P.Int_rem w), [x, y as C.IntConst (w', y')]) =>
               if w = w' then
                 if y' = 1 orelse y' = ~1 then
                   VALUE (C.IntConst (w, 0))
                 else if y' <> 0 then
                   case x of
                     C.IntConst (w'', x') =>
                       if w = w'' then
                         VALUE (C.IntConst (w, IntInf.rem (x', y')))
                       else
                         NOT_SIMPLIFIED
                   | _ =>
                       SIMPLE_EXP (C.PrimOp
                         { primOp = F.PrimCall (P.Int_rem_unchecked w)
                         , tyargs = []
                         , args = [x, y]
                         })
                 else
                   NOT_SIMPLIFIED
               else
                 NOT_SIMPLIFIED
           | (F.PrimCall (P.Int_TILDE_wrapping w), [C.IntConst (w', x)]) =>
               if w = w' then VALUE (C.IntConst (w, wraparound (w, ~x)))
               else NOT_SIMPLIFIED
           | ( F.PrimCall (P.Int_toInt_unchecked (w1, w2))
             , [x as C.IntConst (w', x')]
             ) =>
               if w1 = w' then
                 if w1 = w2 then VALUE x (* no op *)
                 else if isInt (w2, x') then VALUE (C.IntConst (w2, x'))
                 else NOT_SIMPLIFIED
               else
                 NOT_SIMPLIFIED
           | (F.PrimCall (P.Int_toInt_unchecked (w1, w2)), [x]) =>
               if w1 = w2 then VALUE x else NOT_SIMPLIFIED
           | (F.PrimCall (P.Word_div w), [x, y as C.WordConst (w', y')]) =>
               if w = w' then
                 if y' = 1 then
                   VALUE x
                 else if y' <> 0 then
                   case x of
                     C.WordConst (w'', x') =>
                       if w = w'' then VALUE (C.WordConst (w, x' div y'))
                       else NOT_SIMPLIFIED
                   | _ =>
                       SIMPLE_EXP (C.PrimOp
                         { primOp = F.PrimCall (P.Word_div_unchecked w)
                         , tyargs = []
                         , args = [x, y]
                         })
                 else
                   NOT_SIMPLIFIED
               else
                 NOT_SIMPLIFIED
           | (F.PrimCall (P.Word_mod w), [x, y as C.WordConst (w', y')]) =>
               if w = w' then
                 if y' = 1 then
                   VALUE (C.WordConst (w, 0))
                 else if y' <> 0 then
                   case x of
                     C.WordConst (w'', x') =>
                       if w = w'' then VALUE (C.WordConst (w, x' mod y'))
                       else NOT_SIMPLIFIED
                   | _ =>
                       SIMPLE_EXP (C.PrimOp
                         { primOp = F.PrimCall (P.Word_mod_unchecked w)
                         , tyargs = []
                         , args = [x, y]
                         })
                 else
                   NOT_SIMPLIFIED
               else
                 NOT_SIMPLIFIED
           | ( F.PrimCall (P.Word_andb w)
             , [C.WordConst (w', x), C.WordConst (w'', y)]
             ) =>
               if w = w' andalso w = w'' then
                 VALUE (C.WordConst (w, IntInf.andb (x, y)))
               else
                 NOT_SIMPLIFIED
           | (F.PrimCall (P.Word_andb w), [x as C.WordConst (w', 0), _]) =>
               if w = w' then VALUE x else NOT_SIMPLIFIED
           | (F.PrimCall (P.Word_andb w), [_, y as C.WordConst (w', 0)]) =>
               if w = w' then VALUE y else NOT_SIMPLIFIED
           | ( F.PrimCall (P.Word_orb w)
             , [C.WordConst (w', x), C.WordConst (w'', y)]
             ) =>
               if w = w' andalso w = w'' then
                 VALUE (C.WordConst (w, IntInf.orb (x, y)))
               else
                 NOT_SIMPLIFIED
           | (F.PrimCall (P.Word_orb w), [C.WordConst (w', 0), y]) =>
               if w = w' then VALUE y else NOT_SIMPLIFIED
           | (F.PrimCall (P.Word_orb w), [x, C.WordConst (w', 0)]) =>
               if w = w' then VALUE x else NOT_SIMPLIFIED
           | ( F.PrimCall (P.Word_xorb w)
             , [C.WordConst (w', x), C.WordConst (w'', y)]
             ) =>
               if w = w' andalso w = w'' then
                 VALUE (C.WordConst (w, IntInf.xorb (x, y)))
               else
                 NOT_SIMPLIFIED
           | (F.PrimCall (P.Word_xorb w), [C.WordConst (w', 0), y]) =>
               if w = w' then VALUE y else NOT_SIMPLIFIED
           | (F.PrimCall (P.Word_xorb w), [x, C.WordConst (w', 0)]) =>
               if w = w' then VALUE x else NOT_SIMPLIFIED
           | (F.PrimCall (P.Char_ord w), [C.CharConst c]) =>
               VALUE (C.IntConst (w, Int.toLarge (Char.ord c)))
           | (F.PrimCall (P.Char_chr_unchecked w), [C.IntConst (w', c)]) =>
               if w = w' andalso 0 <= c andalso c <= 255 then
                 VALUE (C.CharConst (Char.chr (Int.fromLarge c)))
               else
                 NOT_SIMPLIFIED
           | (F.PrimCall (P.Char16_ord w), [C.Char16Const c]) =>
               VALUE (C.IntConst (w, Int.toLarge c))
           | (F.PrimCall (P.Char16_chr_unchecked w), [C.IntConst (w', c)]) =>
               if w = w' andalso 0 <= c andalso c <= 0xffff then
                 VALUE (C.Char16Const (Int.fromLarge c))
               else
                 NOT_SIMPLIFIED
           | (F.PrimCall P.Vector_fromList, [C.TypedNil _]) =>
               SIMPLE_EXP (C.PrimOp
                 {primOp = F.VectorOp, tyargs = [List.hd tyargs], args = []})
           | (F.PrimCall P.Vector_fromList, [C.Var v]) =>
               (case TypedSyntax.VIdMap.find (env, v) of
                  SOME {exp = SOME (C.PrimOp {primOp = F.ListOp, tyargs, args})} =>
                    SIMPLE_EXP
                      (C.PrimOp
                         {primOp = F.VectorOp, tyargs = tyargs, args = args})
                | _ => NOT_SIMPLIFIED)
           | (F.PrimCall P.mkFn2, [C.Var f]) =>
               let
                 val l = CpsSimplify.genContSym ctx
                 val a = CpsSimplify.newVId (ctx, "a")
                 val b = CpsSimplify.newVId (ctx, "b")
                 val t = CpsSimplify.newVId (ctx, "t")
                 val (tyA, tyB, resultTy) =
                   case tyargs of
                     [tyA, tyB, resultTy] => (tyA, tyB, resultTy)
                   | _ => raise Fail "invalid mkFn2"
               in
                 SIMPLE_EXP (C.Abs
                   { contParam = l
                   , tyParams = []
                   , params = [(a, tyA), (b, tyB)]
                   , body = C.Let
                       { decs =
                           [C.ValDec
                              { exp =
                                  C.Record
                                    (List.foldl Syntax.LabelMap.insert'
                                       Syntax.LabelMap.empty
                                       [ (Syntax.NumericLabel 1, C.Var a)
                                       , (Syntax.NumericLabel 2, C.Var b)
                                       ])
                              , results = [(SOME t, F.TupleType [tyA, tyB])]
                              }]
                       , cont = C.App
                           { applied = C.Var f
                           , cont = l
                           , tyArgs = []
                           , args = [C.Var t]
                           , attr = {typeOnly = false}
                           }
                       }
                   , resultTy = resultTy
                   , attr = {alwaysInline = false, typeOnly = false}
                   })
               end
           | (F.PrimCall P.mkFn3, [C.Var f]) =>
               let
                 val l = CpsSimplify.genContSym ctx
                 val a = CpsSimplify.newVId (ctx, "a")
                 val b = CpsSimplify.newVId (ctx, "b")
                 val c = CpsSimplify.newVId (ctx, "c")
                 val t = CpsSimplify.newVId (ctx, "t")
                 val (tyA, tyB, tyC, resultTy) =
                   case tyargs of
                     [tyA, tyB, tyC, resultTy] => (tyA, tyB, tyC, resultTy)
                   | _ => raise Fail "invalid mkFn2"
               in
                 SIMPLE_EXP (C.Abs
                   { contParam = l
                   , tyParams = []
                   , params = [(a, tyA), (b, tyB), (c, tyC)]
                   , body = C.Let
                       { decs =
                           [C.ValDec
                              { exp =
                                  C.Record
                                    (List.foldl Syntax.LabelMap.insert'
                                       Syntax.LabelMap.empty
                                       [ (Syntax.NumericLabel 1, C.Var a)
                                       , (Syntax.NumericLabel 2, C.Var b)
                                       , (Syntax.NumericLabel 3, C.Var c)
                                       ])
                              , results =
                                  [(SOME t, F.TupleType [tyA, tyB, tyC])]
                              }]
                       , cont = C.App
                           { applied = C.Var f
                           , cont = l
                           , tyArgs = []
                           , args = [C.Var t]
                           , attr = {typeOnly = false}
                           }
                       }
                   , resultTy = resultTy
                   , attr = {alwaysInline = false, typeOnly = false}
                   })
               end
           | _ => NOT_SIMPLIFIED)
      | simplifySimpleExp (_, _, C.ExnTag _) = NOT_SIMPLIFIED
      | simplifySimpleExp (_, env, C.Projection {label, record, fieldTypes = _}) =
          (case record of
             C.Var v =>
               (case TypedSyntax.VIdMap.find (env, v) of
                  SOME {exp = SOME (C.Record fields), ...} =>
                    (case Syntax.LabelMap.find (fields, label) of
                       SOME w => VALUE w
                     | NONE => NOT_SIMPLIFIED)
                | _ => NOT_SIMPLIFIED)
           | _ => NOT_SIMPLIFIED)
      | simplifySimpleExp
          ( _
          , _
          , C.Abs
              { contParam = _
              , tyParams = _
              , params = _
              , body = _
              , resultTy = _
              , attr = _
              }
          ) = NOT_SIMPLIFIED (* TODO: Try eta conversion *)
    and simplifyDec (ctx: Context)
      (dec, (env, cenv, tysubst, subst, csubst, acc: C.Dec list)) =
      case dec of
        C.ValDec {exp, results} =>
          let
            val exp = CpsSimplify.substSimpleExp (tysubst, subst, csubst, exp)
            val results =
              List.map (fn (v, ty) => (v, CpsSimplify.substTy tysubst ty))
                results
          in
            case simplifySimpleExp (ctx, env, exp) of
              VALUE v =>
                let
                  val () = #simplificationOccurred ctx := true
                  val subst =
                    case results of
                      [(SOME result, _)] =>
                        TypedSyntax.VIdMap.insert (subst, result, v)
                    | [(NONE, _)] => subst
                    | _ => subst (* should not occur *)
                in
                  (env, cenv, tysubst, subst, csubst, acc)
                end
            | simplified =>
                let
                  val () =
                    case simplified of
                      SIMPLE_EXP _ => #simplificationOccurred ctx := true
                    | VALUE _ =>
                        #simplificationOccurred ctx
                        := true (* shoud not occur *)
                    | NOT_SIMPLIFIED => ()
                  val exp =
                    case simplified of
                      SIMPLE_EXP exp => exp
                    | _ => exp
                in
                  case (exp, results) of
                    ( C.Abs
                        { contParam
                        , tyParams
                        , params
                        , body
                        , resultTy
                        , attr as {alwaysInline, ...}
                        }
                    , [(SOME result, ty)]
                    ) =>
                      let
                        val body = simplifyStat
                          (ctx, env, cenv, tysubst, subst, csubst, body)
                        val exp = C.Abs
                          { contParam = contParam
                          , tyParams = tyParams
                          , params = params
                          , body = body
                          , resultTy = resultTy
                          , attr = attr
                          }
                        val env = TypedSyntax.VIdMap.insert
                          ( env
                          , result
                          , {exp =
                               if
                                 alwaysInline
                                 orelse CpsSimplify.sizeOfStat (body, 10) >= 0
                               then (* Inline small functions *)
                                 SOME exp
                               else
                                 NONE (*, isDiscardableFunction = isDiscardableExp (env, body) *)}
                          )
                        val dec = C.ValDec
                          {exp = exp, results = [(SOME result, ty)]}
                      in
                        (env, cenv, tysubst, subst, csubst, dec :: acc)
                      end
                  | _ =>
                      (case (C.isDiscardable exp, results) of
                         (true, [(NONE, _)]) =>
                           (env, cenv, tysubst, subst, csubst, acc)
                       | (_, [(SOME result, ty)]) =>
                           let
                             val dec = C.ValDec
                               {exp = exp, results = [(SOME result, ty)]}
                             val env = TypedSyntax.VIdMap.insert
                               ( env
                               , result
                               , {exp =
                                    SOME
                                      exp (*, isDiscardableFunction = false *)}
                               )
                           in
                             (env, cenv, tysubst, subst, csubst, dec :: acc)
                           end
                       | _ =>
                           let val dec = C.ValDec {exp = exp, results = results}
                           in (env, cenv, tysubst, subst, csubst, dec :: acc)
                           end)
                end
          end
      | C.RecDec defs =>
          let
            val defs =
              List.map
                (fn {name, contParam, tyParams, params, body, resultTy, attr} =>
                   { name = name
                   , contParam = contParam
                   , tyParams = tyParams
                   , params =
                       List.map
                         (fn (v, ty) => (v, CpsSimplify.substTy tysubst ty))
                         params
                   , body = simplifyStat
                       (ctx, env, cenv, tysubst, subst, csubst, body)
                   , resultTy = CpsSimplify.substTy tysubst resultTy
                   , attr = attr
                   }) defs
          in
            (env, cenv, tysubst, subst, csubst, C.RecDec defs :: acc)
          end
      | C.UnpackDec {tyVar, kind, vid, unpackedTy, package} =>
          (case CpsSimplify.substValue (tysubst, subst) package of
             C.Pack {value, payloadTy, packageTy = _} =>
               let
                 val subst = TypedSyntax.VIdMap.insert (subst, vid, value)
                 val tysubst =
                   TypedSyntax.TyVarMap.insert (tysubst, tyVar, payloadTy)
               in
                 (env, cenv, tysubst, subst, csubst, acc)
               end
           | package =>
               ( env
               , cenv
               , tysubst
               , subst
               , csubst
               , C.UnpackDec
                   { tyVar = tyVar
                   , kind = kind
                   , vid = vid
                   , unpackedTy = CpsSimplify.substTy tysubst unpackedTy
                   , package = package
                   } :: acc
               ))
      | C.ContDec {name, params, body, attr as {alwaysInline}} =>
          let
            val params =
              List.map (fn (v, ty) => (v, CpsSimplify.substTy tysubst ty))
                params
            val body = simplifyStat
              (ctx, env, cenv, tysubst, subst, csubst, body)
            val cenv = C.CVarMap.insert
              ( cenv
              , name
              , ( params
                , if alwaysInline orelse CpsSimplify.sizeOfStat (body, 3) >= 0 then (* Inline very small continuations *)
                    SOME body
                  else
                    NONE
                )
              )
            val dec = C.ContDec
              {name = name, params = params, body = body, attr = attr}
          in
            (env, cenv, tysubst, subst, csubst, dec :: acc)
          end
      | C.RecContDec defs =>
          let
            val dec = C.RecContDec
              (List.map
                 (fn (name, params, body) =>
                    ( name
                    , List.map
                        (fn (v, ty) => (v, CpsSimplify.substTy tysubst ty))
                        params
                    , simplifyStat
                        (ctx, env, cenv, tysubst, subst, csubst, body)
                    )) defs)
          in
            (env, cenv, tysubst, subst, csubst, dec :: acc)
          end
      | C.DatatypeDec _ => (env, cenv, tysubst, subst, csubst, dec :: acc)
      | C.ESImportDec {pure, specs, moduleName} =>
          let
            val dec = C.ESImportDec
              { pure = pure
              , specs =
                  List.map
                    (fn (n, v, ty) => (n, v, CpsSimplify.substTy tysubst ty))
                    specs
              , moduleName = moduleName
              }
          in
            (env, cenv, tysubst, subst, csubst, dec :: acc)
          end
    and simplifyStat
      ( ctx: Context
      , env: value_info TypedSyntax.VIdMap.map
      , cenv: ((C.Var option * F.Ty) list * C.Stat option) C.CVarMap.map
      , tysubst: FSyntax.Ty TypedSyntax.TyVarMap.map
      , subst: C.Value TypedSyntax.VIdMap.map
      , csubst: C.CVar C.CVarMap.map
      , e
      ) =
      case e of
        C.Let {decs, cont} =>
          let
            val (env, cenv, tysubst, subst, csubst, revDecs) =
              List.foldl (simplifyDec ctx)
                (env, cenv, tysubst, subst, csubst, []) decs
          in
            CpsTransform.prependRevDecs (revDecs, simplifyStat
              (ctx, env, cenv, tysubst, subst, csubst, cont))
          end
      | C.App {applied, cont, tyArgs, args, attr} =>
          let
            val applied = CpsSimplify.substValue (tysubst, subst) applied
            val cont = CpsSimplify.substCVar csubst cont
            val tyArgs = List.map (CpsSimplify.substTy tysubst) tyArgs
            val args = List.map (CpsSimplify.substValue (tysubst, subst)) args
          in
            case applied of
              C.Var applied =>
                (case TypedSyntax.VIdMap.find (env, applied) of
                   SOME
                     { exp =
                         SOME
                           (C.Abs
                              { contParam
                              , tyParams
                              , params
                              , body
                              , resultTy = _
                              , attr = _
                              })
                     , ...
                     } =>
                     let
                       val () = #simplificationOccurred ctx := true
                       val tysubst =
                         ListPair.foldlEq
                           (fn ((tv, _), a, tysubst) =>
                              TypedSyntax.TyVarMap.insert (tysubst, tv, a))
                           tysubst (tyParams, tyArgs)
                         handle ListPair.UnequalLengths =>
                           raise Fail
                             ("inliner: arity mismatch in type application: ("
                              ^ TypedSyntax.print_VId applied ^ ")")
                       val subst =
                         ListPair.foldlEq
                           (fn ((p, _), a, subst) =>
                              TypedSyntax.VIdMap.insert (subst, p, a)) subst
                           (params, args)
                         handle ListPair.UnequalLengths =>
                           raise Fail
                             ("inliner: arity mismatch in function application ("
                              ^ TypedSyntax.print_VId applied ^ ")")
                       val csubst = C.CVarMap.insert (csubst, contParam, cont)
                     in
                       CpsSimplify.alphaConvert
                         (ctx, tysubst, subst, csubst, body)
                     end
                 (*
                 | SOME { exp = _, isDiscardableFunction = true } =>
                 (case C.CVarMap.find (cenv, cont) of
                  SOME (params, _) => if not (List.exists Option.isSome params) then
                                          ( #simplificationOccurred ctx := true
                                          ; C.AppCont { applied = cont, args = List.map (fn _ => C.Unit (* dummy *)) params }
                                          )
                                      else
                                          C.App { applied = C.Var applied, cont = cont, args = args }
                 | _ => C.App { applied = C.Var applied, cont = cont, args = args }
                 )
                 *)
                 | _ =>
                     C.App
                       { applied = C.Var applied
                       , cont = cont
                       , tyArgs = tyArgs
                       , args = args
                       , attr = attr
                       })
            | C.Nil =>
                (case (tyArgs, args, attr) of
                   ([ty], [], {typeOnly = true, ...}) =>
                     ( #simplificationOccurred ctx := true
                     ; C.AppCont {applied = cont, args = [C.TypedNil ty]}
                     )
                 | _ =>
                     C.App
                       { applied = applied
                       , cont = cont
                       , tyArgs = tyArgs
                       , args = args
                       , attr = attr
                       } (* should not occur *))
            | _ =>
                C.App
                  { applied = applied
                  , cont = cont
                  , tyArgs = tyArgs
                  , args = args
                  , attr = attr
                  } (* should not occur *)
          end
      | C.AppCont {applied, args} =>
          let
            val applied = CpsSimplify.substCVar csubst applied
            val args = List.map (CpsSimplify.substValue (tysubst, subst)) args
          in
            case C.CVarMap.find (cenv, applied) of
              SOME (params, SOME body) =>
                let
                  val () = #simplificationOccurred ctx := true
                  val subst =
                    ListPair.foldlEq
                      (fn ((SOME p, _), a, subst) =>
                         TypedSyntax.VIdMap.insert (subst, p, a)
                        | ((NONE, _), _, subst) => subst) subst (params, args)
                    handle ListPair.UnequalLengths =>
                      raise Fail
                        "inliner: arity mismatch in continuation application"
                in
                  CpsSimplify.alphaConvert (ctx, tysubst, subst, csubst, body)
                end
            | _ => C.AppCont {applied = applied, args = args}
          end
      | C.If {cond, thenCont, elseCont} =>
          (case CpsSimplify.substValue (tysubst, subst) cond of
             C.BoolConst true =>
               ( #simplificationOccurred ctx := true
               ; simplifyStat (ctx, env, cenv, tysubst, subst, csubst, thenCont)
               )
           | C.BoolConst false =>
               ( #simplificationOccurred ctx := true
               ; simplifyStat (ctx, env, cenv, tysubst, subst, csubst, elseCont)
               )
           | cond =>
               C.If
                 { cond = cond
                 , thenCont = simplifyStat
                     (ctx, env, cenv, tysubst, subst, csubst, thenCont)
                 , elseCont = simplifyStat
                     (ctx, env, cenv, tysubst, subst, csubst, elseCont)
                 })
      | C.Handle
          { body
          , handler = (e, h)
          , successfulExitIn
          , successfulExitOut
          , resultTy
          } =>
          C.Handle
            { body = simplifyStat
                ( ctx
                , env
                , C.CVarMap.empty (* do not inline across 'handle' *)
                , tysubst
                , subst
                , csubst
                , body
                )
            , handler = (e, simplifyStat
                (ctx, env, cenv, tysubst, subst, csubst, h))
            , successfulExitIn = successfulExitIn
            , successfulExitOut = CpsSimplify.substCVar csubst successfulExitOut
            , resultTy = CpsSimplify.substTy tysubst resultTy
            }
      | C.Raise (span, x) =>
          C.Raise (span, CpsSimplify.substValue (tysubst, subst) x)
      | C.Unreachable => e
    local val nextCont = ref ~1 val nextVId = ref ~1000 val nextTyVar = ref ~100
    in
      fun newCont () =
        let val i = !nextCont
        in nextCont := i - 1; C.CVar.fromInt i
        end
      fun newVId name =
        let val i = !nextVId
        in nextVId := i - 1; TypedSyntax.MkVId (name, i)
        end
      fun newTyVar name =
        let val i = !nextTyVar
        in nextTyVar := i - 1; TypedSyntax.MkTyVar (name, i)
        end
      fun TyAbs f =
        let
          val k = newCont ()
          val tv = newTyVar "'a"
          val (resultTy, body) = f (FSyntax.TyVar tv)
        in
          C.Abs
            { contParam = k
            , tyParams = [(tv, FSyntax.TypeKind)]
            , params = []
            , body =
                let
                  val result = newVId "a"
                in
                  C.Let
                    { decs =
                        [C.ValDec
                           {exp = body, results = [(SOME result, resultTy)]}]
                    , cont = C.AppCont {applied = k, args = [C.Var result]}
                    }
                end
            , resultTy = resultTy
            , attr = {alwaysInline = true, typeOnly = true}
            }
        end
    end
    val General_exnName =
      let
        val k = newCont ()
        val e = newVId "e"
        val result = newVId "a"
      in
        C.Abs
          { contParam = k
          , tyParams = []
          , params = [(e, FSyntax.Types.exn)]
          , body = C.Let
              { decs =
                  [C.ValDec
                     { exp = C.PrimOp
                         { primOp = FSyntax.PrimCall Primitives.General_exnName
                         , tyargs = []
                         , args = [C.Var e]
                         }
                     , results = [(SOME result, FSyntax.Types.string)]
                     }]
              , cont = C.AppCont {applied = k, args = [C.Var result]}
              }
          , resultTy = FSyntax.Types.string
          , attr = {alwaysInline = true, typeOnly = false}
          }
      end
    val Real_abs =
      let
        val k = newCont ()
        val x = newVId "x"
        val result = newVId "a"
      in
        C.Abs
          { contParam = k
          , tyParams = []
          , params = [(x, FSyntax.Types.real)]
          , body = C.Let
              { decs =
                  [C.ValDec
                     { exp = C.PrimOp
                         { primOp = FSyntax.PrimCall Primitives.Real_abs
                         , tyargs = []
                         , args = [C.Var x]
                         }
                     , results = [(SOME result, FSyntax.Types.real)]
                     }]
              , cont = C.AppCont {applied = k, args = [C.Var result]}
              }
          , resultTy = FSyntax.Types.real
          , attr = {alwaysInline = true, typeOnly = false}
          }
      end
    val String_concat =
      let
        val k = newCont ()
        val xs = newVId "xs"
        val result = newVId "a"
      in
        C.Abs
          { contParam = k
          , tyParams = []
          , params = [(xs, FSyntax.Types.list FSyntax.Types.string)]
          , body = C.Let
              { decs =
                  [C.ValDec
                     { exp = C.PrimOp
                         { primOp = FSyntax.PrimCall Primitives.String_concat
                         , tyargs = []
                         , args = [C.Var xs]
                         }
                     , results = [(SOME result, FSyntax.Types.string)]
                     }]
              , cont = C.AppCont {applied = k, args = [C.Var result]}
              }
          , resultTy = FSyntax.Types.string
          , attr = {alwaysInline = true, typeOnly = false}
          }
      end
    val String_implode =
      let
        val k = newCont ()
        val xs = newVId "xs"
        val result = newVId "a"
      in
        C.Abs
          { contParam = k
          , tyParams = []
          , params = [(xs, FSyntax.Types.list FSyntax.Types.char)]
          , body = C.Let
              { decs =
                  [C.ValDec
                     { exp = C.PrimOp
                         { primOp = FSyntax.PrimCall Primitives.String_implode
                         , tyargs = []
                         , args = [C.Var xs]
                         }
                     , results = [(SOME result, FSyntax.Types.string)]
                     }]
              , cont = C.AppCont {applied = k, args = [C.Var result]}
              }
          , resultTy = FSyntax.Types.string
          , attr = {alwaysInline = true, typeOnly = false}
          }
      end
    val Vector_fromList =
      let
        val k = newCont ()
        val xs = newVId "xs"
        val result = newVId "v"
      in
        TyAbs (fn ty =>
          ( F.MultiFnType ([FSyntax.Types.list ty], FSyntax.Types.vector ty)
          , C.Abs
              { contParam = k
              , tyParams = []
              , params = [(xs, FSyntax.Types.list ty)]
              , body = C.Let
                  { decs =
                      [C.ValDec
                         { exp = C.PrimOp
                             { primOp =
                                 FSyntax.PrimCall Primitives.Vector_fromList
                             , tyargs = [ty]
                             , args = [C.Var xs]
                             }
                         , results = [(SOME result, FSyntax.Types.vector ty)]
                         }]
                  , cont = C.AppCont {applied = k, args = [C.Var result]}
                  }
              , resultTy = FSyntax.Types.vector ty
              , attr = {alwaysInline = true, typeOnly = false}
              }
          ))
      end
    val Vector_concat =
      let
        val k = newCont ()
        val xs = newVId "xs"
        val result = newVId "v"
      in
        TyAbs (fn ty =>
          ( F.MultiFnType
              ( [FSyntax.Types.list (FSyntax.Types.vector ty)]
              , FSyntax.Types.vector ty
              )
          , C.Abs
              { contParam = k
              , tyParams = []
              , params = [(xs, FSyntax.Types.list (FSyntax.Types.vector ty))]
              , body = C.Let
                  { decs =
                      [C.ValDec
                         { exp = C.PrimOp
                             { primOp =
                                 FSyntax.PrimCall Primitives.Vector_concat
                             , tyargs = [ty]
                             , args = [C.Var xs]
                             }
                         , results = [(SOME result, FSyntax.Types.vector ty)]
                         }]
                  , cont = C.AppCont {applied = k, args = [C.Var result]}
                  }
              , resultTy = FSyntax.Types.vector ty
              , attr = {alwaysInline = true, typeOnly = false}
              }
          ))
      end
    val Array_fromList =
      let
        val k = newCont ()
        val xs = newVId "xs"
        val result = newVId "a"
      in
        TyAbs (fn ty =>
          ( F.MultiFnType ([FSyntax.Types.list ty], FSyntax.Types.array ty)
          , C.Abs
              { contParam = k
              , tyParams = []
              , params = [(xs, FSyntax.Types.list ty)]
              , body = C.Let
                  { decs =
                      [C.ValDec
                         { exp = C.PrimOp
                             { primOp =
                                 FSyntax.PrimCall Primitives.Array_fromList
                             , tyargs = [ty]
                             , args = [C.Var xs]
                             }
                         , results = [(SOME result, FSyntax.Types.array ty)]
                         }]
                  , cont = C.AppCont {applied = k, args = [C.Var result]}
                  }
              , resultTy = FSyntax.Types.array ty
              , attr = {alwaysInline = true, typeOnly = false}
              }
          ))
      end
    fun Exception_predicate tag =
      let
        val k = newCont ()
        val e = newVId "e"
        val result = newVId "b"
      in
        C.Abs
          { contParam = k
          , tyParams = []
          , params = [(e, FSyntax.Types.exn)]
          , body = C.Let
              { decs =
                  [C.ValDec
                     { exp = C.PrimOp
                         { primOp = FSyntax.PrimCall
                             Primitives.Exception_instanceof
                         , tyargs = []
                         , args = [C.Var e, C.Var tag]
                         }
                     , results = [(SOME result, FSyntax.Types.bool)]
                     }]
              , cont = C.AppCont {applied = k, args = [C.Var result]}
              }
          , resultTy = FSyntax.Types.bool
          , attr = {alwaysInline = true, typeOnly = false}
          }
      end
    val Fail_construct =
      let
        val k = newCont ()
        val message = newVId "message"
        val result = newVId "e"
      in
        C.Abs
          { contParam = k
          , tyParams = []
          , params = [(message, FSyntax.Types.string)]
          , body = C.Let
              { decs =
                  [C.ValDec
                     { exp = C.PrimOp
                         { primOp = FSyntax.ConstructExnWithPayloadOp
                         , tyargs = [FSyntax.Types.string]
                         , args = [C.Var InitialEnv.VId_Fail_tag, C.Var message]
                         }
                     , results = [(SOME result, FSyntax.Types.exn)]
                     }]
              , cont = C.AppCont {applied = k, args = [C.Var result]}
              }
          , resultTy = FSyntax.Types.exn
          , attr = {alwaysInline = true, typeOnly = false}
          }
      end
    val Fail_payload =
      let
        val k = newCont ()
        val e = newVId "e"
        val result = newVId "message"
      in
        C.Abs
          { contParam = k
          , tyParams = []
          , params = [(e, FSyntax.Types.exn)]
          , body = C.Let
              { decs =
                  [C.ValDec
                     { exp = C.PrimOp
                         { primOp = FSyntax.ExnPayloadOp
                         , tyargs = [FSyntax.Types.string]
                         , args = [C.Var e]
                         }
                     , results = [(SOME result, FSyntax.Types.string)]
                     }]
              , cont = C.AppCont {applied = k, args = [C.Var result]}
              }
          , resultTy = FSyntax.Types.string
          , attr = {alwaysInline = true, typeOnly = false}
          }
      end
    val Lua_newTableWith =
      let
        val k = newCont ()
        val entries = newVId "entries"
        val result = newVId "t"
      in
        C.Abs
          { contParam = k
          , tyParams = []
          , params =
              [( entries
               , FSyntax.Types.vector
                   (FSyntax.PairType
                      (FSyntax.Types.string, FSyntax.Types.lua_value))
               )]
          , body = C.Let
              { decs =
                  [C.ValDec
                     { exp = C.PrimOp
                         { primOp = FSyntax.PrimCall Primitives.Lua_newTableWith
                         , tyargs = []
                         , args = [C.Var entries]
                         }
                     , results = [(SOME result, FSyntax.Types.lua_value)]
                     }]
              , cont = C.AppCont {applied = k, args = [C.Var result]}
              }
          , resultTy = FSyntax.Types.lua_value
          , attr = {alwaysInline = true, typeOnly = false}
          }
      end
    val JavaScript_function =
      let
        val k = newCont ()
        val xs = newVId "xs"
        val result = newVId "a"
      in
        C.Abs
          { contParam = k
          , tyParams = []
          , params =
              [( xs
               , FSyntax.FnType
                   ( FSyntax.Types.vector FSyntax.Types.js_value
                   , FSyntax.Types.js_value
                   )
               )]
          , body = C.Let
              { decs =
                  [C.ValDec
                     { exp = C.PrimOp
                         { primOp = FSyntax.PrimCall
                             Primitives.JavaScript_function
                         , tyargs = []
                         , args = [C.Var xs]
                         }
                     , results = [(SOME result, FSyntax.Types.js_value)]
                     }]
              , cont = C.AppCont {applied = k, args = [C.Var result]}
              }
          , resultTy = FSyntax.Types.js_value
          , attr = {alwaysInline = true, typeOnly = false}
          }
      end
    val JavaScript_encodeUtf8 =
      let
        val k = newCont ()
        val xs = newVId "xs"
        val result = newVId "a"
      in
        C.Abs
          { contParam = k
          , tyParams = []
          , params = [(xs, FSyntax.Types.string16)]
          , body = C.Let
              { decs =
                  [C.ValDec
                     { exp = C.PrimOp
                         { primOp = FSyntax.PrimCall
                             Primitives.JavaScript_encodeUtf8
                         , tyargs = []
                         , args = [C.Var xs]
                         }
                     , results = [(SOME result, FSyntax.Types.string)]
                     }]
              , cont = C.AppCont {applied = k, args = [C.Var result]}
              }
          , resultTy = FSyntax.Types.string
          , attr = {alwaysInline = true, typeOnly = false}
          }
      end
    val JavaScript_decodeUtf8 =
      let
        val k = newCont ()
        val xs = newVId "xs"
        val result = newVId "a"
      in
        C.Abs
          { contParam = k
          , tyParams = []
          , params = [(xs, FSyntax.Types.string)]
          , body = C.Let
              { decs =
                  [C.ValDec
                     { exp = C.PrimOp
                         { primOp = FSyntax.PrimCall
                             Primitives.JavaScript_decodeUtf8
                         , tyargs = []
                         , args = [C.Var xs]
                         }
                     , results = [(SOME result, FSyntax.Types.string16)]
                     }]
              , cont = C.AppCont {applied = k, args = [C.Var result]}
              }
          , resultTy = FSyntax.Types.string16
          , attr = {alwaysInline = true, typeOnly = false}
          }
      end
    val initialEnv: value_info TypedSyntax.VIdMap.map =
      List.foldl TypedSyntax.VIdMap.insert' TypedSyntax.VIdMap.empty
        [ (InitialEnv.VId_exnName, {exp = SOME General_exnName})
        , (InitialEnv.VId_Real_abs, {exp = SOME Real_abs})
        , (InitialEnv.VId_String_concat, {exp = SOME String_concat})
        , (InitialEnv.VId_String_implode, {exp = SOME String_implode})
        , (InitialEnv.VId_Vector_fromList, {exp = SOME Vector_fromList})
        , (InitialEnv.VId_Vector_concat, {exp = SOME Vector_concat})
        , (InitialEnv.VId_Array_fromList, {exp = SOME Array_fromList})
        , ( InitialEnv.VId_Match_predicate
          , {exp = SOME (Exception_predicate InitialEnv.VId_Match_tag)}
          )
        , ( InitialEnv.VId_Bind_predicate
          , {exp = SOME (Exception_predicate InitialEnv.VId_Bind_tag)}
          )
        , ( InitialEnv.VId_Div_predicate
          , {exp = SOME (Exception_predicate InitialEnv.VId_Div_tag)}
          )
        , ( InitialEnv.VId_Overflow_predicate
          , {exp = SOME (Exception_predicate InitialEnv.VId_Overflow_tag)}
          )
        , ( InitialEnv.VId_Size_predicate
          , {exp = SOME (Exception_predicate InitialEnv.VId_Size_tag)}
          )
        , ( InitialEnv.VId_Subscript_predicate
          , {exp = SOME (Exception_predicate InitialEnv.VId_Subscript_tag)}
          )
        , ( InitialEnv.VId_Fail_predicate
          , {exp = SOME (Exception_predicate InitialEnv.VId_Fail_tag)}
          )
        , (InitialEnv.VId_Fail_payload, {exp = SOME Fail_payload})
        , (InitialEnv.VId_Fail, {exp = SOME Fail_construct})
        , (InitialEnv.VId_Lua_newTableWith, {exp = SOME Lua_newTableWith})
        , (InitialEnv.VId_JavaScript_function, {exp = SOME JavaScript_function})
        , ( InitialEnv.VId_JavaScript_encodeUtf8
          , {exp = SOME JavaScript_encodeUtf8}
          )
        , ( InitialEnv.VId_JavaScript_decodeUtf8
          , {exp = SOME JavaScript_decodeUtf8}
          )
        ]
    fun goStat (ctx: CpsSimplify.Context, exp) =
      simplifyStat
        ( ctx
        , initialEnv
        , C.CVarMap.empty
        , TypedSyntax.TyVarMap.empty
        , TypedSyntax.VIdMap.empty
        , C.CVarMap.empty
        , exp
        )
  end (* local *)
end; (* structure CpsInline *)
