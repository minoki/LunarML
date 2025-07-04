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
  val goCExp: CpsSimplify.Context * CSyntax.CExp -> CSyntax.CExp
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
    fun simplifySimpleExp (_: env, C.Record _) = NOT_SIMPLIFIED
      | simplifySimpleExp (env, C.PrimOp {primOp, tyargs, args}) =
          (case (primOp, args) of
             (F.ListOp, []) => VALUE C.Nil (* empty list *)
           | (F.PrimCall P.JavaScript_call, [f, C.Var args]) =>
               (case TypedSyntax.VIdMap.find (env, args) of
                  SOME
                    { exp =
                        SOME (C.PrimOp {primOp = F.VectorOp, tyargs = _, args})
                    , ...
                    } =>
                    SIMPLE_EXP
                      (C.PrimOp
                         {primOp = F.JsCallOp, tyargs = [], args = f :: args})
                | _ => NOT_SIMPLIFIED)
           | (F.PrimCall P.JavaScript_method, [obj, name, C.Var args]) =>
               (case TypedSyntax.VIdMap.find (env, args) of
                  SOME
                    { exp =
                        SOME (C.PrimOp {primOp = F.VectorOp, tyargs = _, args})
                    , ...
                    } =>
                    SIMPLE_EXP (C.PrimOp
                      { primOp = F.JsMethodOp
                      , tyargs = []
                      , args = obj :: name :: args
                      })
                | _ => NOT_SIMPLIFIED)
           | (F.PrimCall P.JavaScript_new, [ctor, C.Var args]) =>
               (case TypedSyntax.VIdMap.find (env, args) of
                  SOME
                    { exp =
                        SOME (C.PrimOp {primOp = F.VectorOp, tyargs = _, args})
                    , ...
                    } =>
                    SIMPLE_EXP
                      (C.PrimOp
                         {primOp = F.JsNewOp, tyargs = [], args = ctor :: args})
                | _ => NOT_SIMPLIFIED)
           | (F.PrimCall P.Lua_call, [ctor, C.Var args]) =>
               (case TypedSyntax.VIdMap.find (env, args) of
                  SOME
                    { exp =
                        SOME (C.PrimOp {primOp = F.VectorOp, tyargs = _, args})
                    , ...
                    } =>
                    SIMPLE_EXP
                      (C.PrimOp
                         { primOp = F.LuaCallOp
                         , tyargs = []
                         , args = ctor :: args
                         })
                | _ => NOT_SIMPLIFIED)
           | (F.PrimCall P.Lua_call1, [ctor, C.Var args]) =>
               (case TypedSyntax.VIdMap.find (env, args) of
                  SOME
                    { exp =
                        SOME (C.PrimOp {primOp = F.VectorOp, tyargs = _, args})
                    , ...
                    } =>
                    SIMPLE_EXP
                      (C.PrimOp
                         { primOp = F.LuaCall1Op
                         , tyargs = []
                         , args = ctor :: args
                         })
                | _ => NOT_SIMPLIFIED)
           | (F.PrimCall P.Lua_call2, [ctor, C.Var args]) =>
               (case TypedSyntax.VIdMap.find (env, args) of
                  SOME
                    { exp =
                        SOME (C.PrimOp {primOp = F.VectorOp, tyargs = _, args})
                    , ...
                    } =>
                    SIMPLE_EXP
                      (C.PrimOp
                         { primOp = F.LuaCallNOp 2
                         , tyargs = []
                         , args = ctor :: args
                         })
                | _ => NOT_SIMPLIFIED)
           | (F.PrimCall P.Lua_call3, [ctor, C.Var args]) =>
               (case TypedSyntax.VIdMap.find (env, args) of
                  SOME
                    { exp =
                        SOME (C.PrimOp {primOp = F.VectorOp, tyargs = _, args})
                    , ...
                    } =>
                    SIMPLE_EXP
                      (C.PrimOp
                         { primOp = F.LuaCallNOp 3
                         , tyargs = []
                         , args = ctor :: args
                         })
                | _ => NOT_SIMPLIFIED)
           | (F.PrimCall P.Lua_call4, [ctor, C.Var args]) =>
               (case TypedSyntax.VIdMap.find (env, args) of
                  SOME
                    { exp =
                        SOME (C.PrimOp {primOp = F.VectorOp, tyargs = _, args})
                    , ...
                    } =>
                    SIMPLE_EXP
                      (C.PrimOp
                         { primOp = F.LuaCallNOp 4
                         , tyargs = []
                         , args = ctor :: args
                         })
                | _ => NOT_SIMPLIFIED)
           | (F.PrimCall P.Lua_call5, [ctor, C.Var args]) =>
               (case TypedSyntax.VIdMap.find (env, args) of
                  SOME
                    { exp =
                        SOME (C.PrimOp {primOp = F.VectorOp, tyargs = _, args})
                    , ...
                    } =>
                    SIMPLE_EXP
                      (C.PrimOp
                         { primOp = F.LuaCallNOp 5
                         , tyargs = []
                         , args = ctor :: args
                         })
                | _ => NOT_SIMPLIFIED)
           | (F.PrimCall P.Lua_call6, [ctor, C.Var args]) =>
               (case TypedSyntax.VIdMap.find (env, args) of
                  SOME
                    { exp =
                        SOME (C.PrimOp {primOp = F.VectorOp, tyargs = _, args})
                    , ...
                    } =>
                    SIMPLE_EXP
                      (C.PrimOp
                         { primOp = F.LuaCallNOp 6
                         , tyargs = []
                         , args = ctor :: args
                         })
                | _ => NOT_SIMPLIFIED)
           | (F.PrimCall P.Lua_call7, [ctor, C.Var args]) =>
               (case TypedSyntax.VIdMap.find (env, args) of
                  SOME
                    { exp =
                        SOME (C.PrimOp {primOp = F.VectorOp, tyargs = _, args})
                    , ...
                    } =>
                    SIMPLE_EXP
                      (C.PrimOp
                         { primOp = F.LuaCallNOp 7
                         , tyargs = []
                         , args = ctor :: args
                         })
                | _ => NOT_SIMPLIFIED)
           | (F.PrimCall P.Lua_call8, [ctor, C.Var args]) =>
               (case TypedSyntax.VIdMap.find (env, args) of
                  SOME
                    { exp =
                        SOME (C.PrimOp {primOp = F.VectorOp, tyargs = _, args})
                    , ...
                    } =>
                    SIMPLE_EXP
                      (C.PrimOp
                         { primOp = F.LuaCallNOp 8
                         , tyargs = []
                         , args = ctor :: args
                         })
                | _ => NOT_SIMPLIFIED)
           | (F.PrimCall P.Lua_call9, [ctor, C.Var args]) =>
               (case TypedSyntax.VIdMap.find (env, args) of
                  SOME
                    { exp =
                        SOME (C.PrimOp {primOp = F.VectorOp, tyargs = _, args})
                    , ...
                    } =>
                    SIMPLE_EXP
                      (C.PrimOp
                         { primOp = F.LuaCallNOp 9
                         , tyargs = []
                         , args = ctor :: args
                         })
                | _ => NOT_SIMPLIFIED)
           | (F.PrimCall P.Lua_method, [ctor, C.StringConst name, C.Var args]) =>
               if LuaWriter.isLuaIdentifier name then
                 case TypedSyntax.VIdMap.find (env, args) of
                   SOME
                     { exp =
                         SOME (C.PrimOp {primOp = F.VectorOp, tyargs = _, args})
                     , ...
                     } =>
                     SIMPLE_EXP
                       (C.PrimOp
                          { primOp = F.LuaMethodOp name
                          , tyargs = []
                          , args = ctor :: args
                          })
                 | _ => NOT_SIMPLIFIED
               else
                 NOT_SIMPLIFIED
           | (F.PrimCall P.Lua_method1, [ctor, C.StringConst name, C.Var args]) =>
               if LuaWriter.isLuaIdentifier name then
                 case TypedSyntax.VIdMap.find (env, args) of
                   SOME
                     { exp =
                         SOME (C.PrimOp {primOp = F.VectorOp, tyargs = _, args})
                     , ...
                     } =>
                     SIMPLE_EXP
                       (C.PrimOp
                          { primOp = F.LuaMethod1Op name
                          , tyargs = []
                          , args = ctor :: args
                          })
                 | _ => NOT_SIMPLIFIED
               else
                 NOT_SIMPLIFIED
           | (F.PrimCall P.Lua_method2, [ctor, C.StringConst name, C.Var args]) =>
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
                       , args = ctor :: args
                       })
                 | _ => NOT_SIMPLIFIED
               else
                 NOT_SIMPLIFIED
           | (F.PrimCall P.Lua_method3, [ctor, C.StringConst name, C.Var args]) =>
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
                       , args = ctor :: args
                       })
                 | _ => NOT_SIMPLIFIED
               else
                 NOT_SIMPLIFIED
           | (F.PrimCall P.Lua_method4, [ctor, C.StringConst name, C.Var args]) =>
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
                       , args = ctor :: args
                       })
                 | _ => NOT_SIMPLIFIED
               else
                 NOT_SIMPLIFIED
           | (F.PrimCall P.Lua_method5, [ctor, C.StringConst name, C.Var args]) =>
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
                       , args = ctor :: args
                       })
                 | _ => NOT_SIMPLIFIED
               else
                 NOT_SIMPLIFIED
           | (F.PrimCall P.Lua_method6, [ctor, C.StringConst name, C.Var args]) =>
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
                       , args = ctor :: args
                       })
                 | _ => NOT_SIMPLIFIED
               else
                 NOT_SIMPLIFIED
           | (F.PrimCall P.Lua_method7, [ctor, C.StringConst name, C.Var args]) =>
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
                       , args = ctor :: args
                       })
                 | _ => NOT_SIMPLIFIED
               else
                 NOT_SIMPLIFIED
           | (F.PrimCall P.Lua_method8, [ctor, C.StringConst name, C.Var args]) =>
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
                       , args = ctor :: args
                       })
                 | _ => NOT_SIMPLIFIED
               else
                 NOT_SIMPLIFIED
           | (F.PrimCall P.Lua_method9, [ctor, C.StringConst name, C.Var args]) =>
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
                       , args = ctor :: args
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
           | (F.PrimCall P.Vector_fromList, [C.Nil]) =>
               SIMPLE_EXP (C.PrimOp
                 {primOp = F.VectorOp, tyargs = [List.hd tyargs], args = []})
           | (F.PrimCall P.Vector_fromList, [C.Var v]) =>
               (case TypedSyntax.VIdMap.find (env, v) of
                  SOME {exp = SOME (C.PrimOp {primOp = F.ListOp, tyargs, args})} =>
                    SIMPLE_EXP
                      (C.PrimOp
                         {primOp = F.VectorOp, tyargs = tyargs, args = args})
                | _ => NOT_SIMPLIFIED)
           | _ => NOT_SIMPLIFIED)
      | simplifySimpleExp (_, C.ExnTag _) = NOT_SIMPLIFIED
      | simplifySimpleExp (env, C.Projection {label, record, fieldTypes = _}) =
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
          (_, C.Abs {contParam = _, params = _, body = _, attr = _}) =
          NOT_SIMPLIFIED (* TODO: Try eta conversion *)
    and simplifyDec (ctx: Context)
      (dec, (env, cenv, subst, csubst, acc: C.Dec list)) =
      case dec of
        C.ValDec {exp, results} =>
          let
            val exp = CpsSimplify.substSimpleExp (subst, csubst, exp)
          in
            case simplifySimpleExp (env, exp) of
              VALUE v =>
                let
                  val () = #simplificationOccurred ctx := true
                  val subst =
                    case results of
                      [SOME result] =>
                        TypedSyntax.VIdMap.insert (subst, result, v)
                    | [NONE] => subst
                    | _ => subst (* should not occur *)
                in
                  (env, cenv, subst, csubst, acc)
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
                    ( C.Abs {contParam, params, body, attr as {alwaysInline}}
                    , [SOME result]
                    ) =>
                      let
                        val body = simplifyCExp
                          (ctx, env, cenv, subst, csubst, body)
                        val exp = C.Abs
                          { contParam = contParam
                          , params = params
                          , body = body
                          , attr = attr
                          }
                        val env = TypedSyntax.VIdMap.insert
                          ( env
                          , result
                          , {exp =
                               if
                                 alwaysInline
                                 orelse CpsSimplify.sizeOfCExp (body, 10) >= 0
                               then (* Inline small functions *)
                                 SOME exp
                               else
                                 NONE (*, isDiscardableFunction = isDiscardableExp (env, body) *)}
                          )
                        val dec = C.ValDec {exp = exp, results = [SOME result]}
                      in
                        (env, cenv, subst, csubst, dec :: acc)
                      end
                  | _ =>
                      (case (C.isDiscardable exp, results) of
                         (true, [NONE]) => (env, cenv, subst, csubst, acc)
                       | (_, [SOME result]) =>
                           let
                             val dec = C.ValDec
                               {exp = exp, results = [SOME result]}
                             val env = TypedSyntax.VIdMap.insert
                               ( env
                               , result
                               , {exp =
                                    SOME
                                      exp (*, isDiscardableFunction = false *)}
                               )
                           in
                             (env, cenv, subst, csubst, dec :: acc)
                           end
                       | _ =>
                           let val dec = C.ValDec {exp = exp, results = results}
                           in (env, cenv, subst, csubst, dec :: acc)
                           end)
                end
          end
      | C.RecDec defs =>
          let
            val defs =
              List.map
                (fn {name, contParam, params, body, attr} =>
                   { name = name
                   , contParam = contParam
                   , params = params
                   , body = simplifyCExp (ctx, env, cenv, subst, csubst, body)
                   , attr = attr
                   }) defs
          in
            (env, cenv, subst, csubst, C.RecDec defs :: acc)
          end
      | C.ContDec {name, params, body, attr as {alwaysInline}} =>
          let
            val body = simplifyCExp (ctx, env, cenv, subst, csubst, body)
            val cenv = C.CVarMap.insert
              ( cenv
              , name
              , ( params
                , if alwaysInline orelse CpsSimplify.sizeOfCExp (body, 3) >= 0 then (* Inline very small continuations *)
                    SOME body
                  else
                    NONE
                )
              )
            val dec = C.ContDec
              {name = name, params = params, body = body, attr = attr}
          in
            (env, cenv, subst, csubst, dec :: acc)
          end
      | C.RecContDec defs =>
          let
            val dec = C.RecContDec
              (List.map
                 (fn (name, params, body) =>
                    ( name
                    , params
                    , simplifyCExp (ctx, env, cenv, subst, csubst, body)
                    )) defs)
          in
            (env, cenv, subst, csubst, dec :: acc)
          end
      | C.ESImportDec _ => (env, cenv, subst, csubst, dec :: acc)
    and simplifyCExp
      ( ctx: Context
      , env: value_info TypedSyntax.VIdMap.map
      , cenv: ((C.Var option) list * C.CExp option) C.CVarMap.map
      , subst: C.Value TypedSyntax.VIdMap.map
      , csubst: C.CVar C.CVarMap.map
      , e
      ) =
      case e of
        C.Let {decs, cont} =>
          let
            val (env, cenv, subst, csubst, revDecs) =
              List.foldl (simplifyDec ctx) (env, cenv, subst, csubst, []) decs
          in
            CpsTransform.prependRevDecs (revDecs, simplifyCExp
              (ctx, env, cenv, subst, csubst, cont))
          end
      | C.App {applied, cont, args, attr} =>
          let
            val applied = CpsSimplify.substValue subst applied
            val cont = CpsSimplify.substCVar csubst cont
            val args = List.map (CpsSimplify.substValue subst) args
          in
            case applied of
              C.Var applied =>
                (case TypedSyntax.VIdMap.find (env, applied) of
                   SOME
                     { exp = SOME (C.Abs {contParam, params, body, attr = _})
                     , ...
                     } =>
                     let
                       val () = #simplificationOccurred ctx := true
                       val subst =
                         ListPair.foldlEq
                           (fn (p, a, subst) =>
                              TypedSyntax.VIdMap.insert (subst, p, a)) subst
                           (params, args)
                       val csubst = C.CVarMap.insert (csubst, contParam, cont)
                     in
                       CpsSimplify.alphaConvert (ctx, subst, csubst, body)
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
                       , args = args
                       , attr = attr
                       })
            | _ =>
                C.App
                  { applied = applied
                  , cont = cont
                  , args = args
                  , attr = attr
                  } (* should not occur *)
          end
      | C.AppCont {applied, args} =>
          let
            val applied = CpsSimplify.substCVar csubst applied
            val args = List.map (CpsSimplify.substValue subst) args
          in
            case C.CVarMap.find (cenv, applied) of
              SOME (params, SOME body) =>
                let
                  val () = #simplificationOccurred ctx := true
                  val subst =
                    ListPair.foldlEq
                      (fn (SOME p, a, subst) =>
                         TypedSyntax.VIdMap.insert (subst, p, a)
                        | (NONE, _, subst) => subst) subst (params, args)
                in
                  CpsSimplify.alphaConvert (ctx, subst, csubst, body)
                end
            | _ => C.AppCont {applied = applied, args = args}
          end
      | C.If {cond, thenCont, elseCont} =>
          (case CpsSimplify.substValue subst cond of
             C.BoolConst true =>
               ( #simplificationOccurred ctx := true
               ; simplifyCExp (ctx, env, cenv, subst, csubst, thenCont)
               )
           | C.BoolConst false =>
               ( #simplificationOccurred ctx := true
               ; simplifyCExp (ctx, env, cenv, subst, csubst, elseCont)
               )
           | cond =>
               C.If
                 { cond = cond
                 , thenCont = simplifyCExp
                     (ctx, env, cenv, subst, csubst, thenCont)
                 , elseCont = simplifyCExp
                     (ctx, env, cenv, subst, csubst, elseCont)
                 })
      | C.Handle {body, handler = (e, h), successfulExitIn, successfulExitOut} =>
          C.Handle
            { body = simplifyCExp
                ( ctx
                , env
                , C.CVarMap.empty (* do not inline across 'handle' *)
                , subst
                , csubst
                , body
                )
            , handler = (e, simplifyCExp (ctx, env, cenv, subst, csubst, h))
            , successfulExitIn = successfulExitIn
            , successfulExitOut = CpsSimplify.substCVar csubst successfulExitOut
            }
      | C.Raise (span, x) => C.Raise (span, CpsSimplify.substValue subst x)
      | C.Unreachable => e
    local val nextCont = ref ~1 val nextVId = ref ~1000
    in
      fun newCont () =
        let val i = !nextCont
        in nextCont := i - 1; C.CVar.fromInt i
        end
      fun newVId name =
        let val i = !nextVId
        in nextVId := i - 1; TypedSyntax.MkVId (name, i)
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
          , params = [e]
          , body = C.Let
              { decs =
                  [C.ValDec
                     { exp = C.PrimOp
                         { primOp = FSyntax.PrimCall Primitives.General_exnName
                         , tyargs = []
                         , args = [C.Var e]
                         }
                     , results = [SOME result]
                     }]
              , cont = C.AppCont {applied = k, args = [C.Var result]}
              }
          , attr = {alwaysInline = true}
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
          , params = [x]
          , body = C.Let
              { decs =
                  [C.ValDec
                     { exp = C.PrimOp
                         { primOp = FSyntax.PrimCall Primitives.Real_abs
                         , tyargs = []
                         , args = [C.Var x]
                         }
                     , results = [SOME result]
                     }]
              , cont = C.AppCont {applied = k, args = [C.Var result]}
              }
          , attr = {alwaysInline = true}
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
          , params = [xs]
          , body = C.Let
              { decs =
                  [C.ValDec
                     { exp = C.PrimOp
                         { primOp = FSyntax.PrimCall Primitives.String_concat
                         , tyargs = []
                         , args = [C.Var xs]
                         }
                     , results = [SOME result]
                     }]
              , cont = C.AppCont {applied = k, args = [C.Var result]}
              }
          , attr = {alwaysInline = true}
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
          , params = [xs]
          , body = C.Let
              { decs =
                  [C.ValDec
                     { exp = C.PrimOp
                         { primOp = FSyntax.PrimCall Primitives.String_implode
                         , tyargs = []
                         , args = [C.Var xs]
                         }
                     , results = [SOME result]
                     }]
              , cont = C.AppCont {applied = k, args = [C.Var result]}
              }
          , attr = {alwaysInline = true}
          }
      end
    val Vector_fromList =
      let
        val k = newCont ()
        val xs = newVId "xs"
        val result = newVId "v"
        val ty = FSyntax.RecordType Syntax.LabelMap.empty (* dummy *)
      in
        C.Abs
          { contParam = k
          , params = [xs]
          , body = C.Let
              { decs =
                  [C.ValDec
                     { exp = C.PrimOp
                         { primOp = FSyntax.PrimCall Primitives.Vector_fromList
                         , tyargs = [ty]
                         , args = [C.Var xs]
                         }
                     , results = [SOME result]
                     }]
              , cont = C.AppCont {applied = k, args = [C.Var result]}
              }
          , attr = {alwaysInline = true}
          }
      end
    val Vector_concat =
      let
        val k = newCont ()
        val xs = newVId "xs"
        val result = newVId "v"
        val ty = FSyntax.RecordType Syntax.LabelMap.empty (* dummy *)
      in
        C.Abs
          { contParam = k
          , params = [xs]
          , body = C.Let
              { decs =
                  [C.ValDec
                     { exp = C.PrimOp
                         { primOp = FSyntax.PrimCall Primitives.Vector_concat
                         , tyargs = [ty]
                         , args = [C.Var xs]
                         }
                     , results = [SOME result]
                     }]
              , cont = C.AppCont {applied = k, args = [C.Var result]}
              }
          , attr = {alwaysInline = true}
          }
      end
    val Array_fromList =
      let
        val k = newCont ()
        val xs = newVId "xs"
        val result = newVId "a"
        val ty = FSyntax.RecordType Syntax.LabelMap.empty (* dummy *)
      in
        C.Abs
          { contParam = k
          , params = [xs]
          , body = C.Let
              { decs =
                  [C.ValDec
                     { exp = C.PrimOp
                         { primOp = FSyntax.PrimCall Primitives.Array_fromList
                         , tyargs = [ty]
                         , args = [C.Var xs]
                         }
                     , results = [SOME result]
                     }]
              , cont = C.AppCont {applied = k, args = [C.Var result]}
              }
          , attr = {alwaysInline = true}
          }
      end
    fun Exception_predicate tag =
      let
        val k = newCont ()
        val e = newVId "e"
        val result = newVId "b"
      in
        C.Abs
          { contParam = k
          , params = [e]
          , body = C.Let
              { decs =
                  [C.ValDec
                     { exp = C.PrimOp
                         { primOp = FSyntax.PrimCall
                             Primitives.Exception_instanceof
                         , tyargs = []
                         , args = [C.Var e, C.Var tag]
                         }
                     , results = [SOME result]
                     }]
              , cont = C.AppCont {applied = k, args = [C.Var result]}
              }
          , attr = {alwaysInline = true}
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
          , params = [message]
          , body = C.Let
              { decs =
                  [C.ValDec
                     { exp = C.PrimOp
                         { primOp = FSyntax.ConstructExnWithPayloadOp
                         , tyargs = [FSyntax.TyVar Typing.primTyName_string]
                         , args = [C.Var InitialEnv.VId_Fail_tag, C.Var message]
                         }
                     , results = [SOME result]
                     }]
              , cont = C.AppCont {applied = k, args = [C.Var result]}
              }
          , attr = {alwaysInline = true}
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
          , params = [e]
          , body = C.Let
              { decs =
                  [C.ValDec
                     { exp = C.PrimOp
                         { primOp = FSyntax.ExnPayloadOp
                         , tyargs = [FSyntax.TyVar Typing.primTyName_string]
                         , args = [C.Var e]
                         }
                     , results = [SOME result]
                     }]
              , cont = C.AppCont {applied = k, args = [C.Var result]}
              }
          , attr = {alwaysInline = true}
          }
      end
    val JavaScript_function =
      let
        val k = newCont ()
        val xs = newVId "xs"
        val result = newVId "a"
        val ty = FSyntax.RecordType Syntax.LabelMap.empty (* dummy *)
      in
        C.Abs
          { contParam = k
          , params = [xs]
          , body = C.Let
              { decs =
                  [C.ValDec
                     { exp = C.PrimOp
                         { primOp = FSyntax.PrimCall
                             Primitives.JavaScript_function
                         , tyargs = [ty]
                         , args = [C.Var xs]
                         }
                     , results = [SOME result]
                     }]
              , cont = C.AppCont {applied = k, args = [C.Var result]}
              }
          , attr = {alwaysInline = true}
          }
      end
    val JavaScript_encodeUtf8 =
      let
        val k = newCont ()
        val xs = newVId "xs"
        val result = newVId "a"
        val ty = FSyntax.RecordType Syntax.LabelMap.empty (* dummy *)
      in
        C.Abs
          { contParam = k
          , params = [xs]
          , body = C.Let
              { decs =
                  [C.ValDec
                     { exp = C.PrimOp
                         { primOp = FSyntax.PrimCall
                             Primitives.JavaScript_encodeUtf8
                         , tyargs = [ty]
                         , args = [C.Var xs]
                         }
                     , results = [SOME result]
                     }]
              , cont = C.AppCont {applied = k, args = [C.Var result]}
              }
          , attr = {alwaysInline = true}
          }
      end
    val JavaScript_decodeUtf8 =
      let
        val k = newCont ()
        val xs = newVId "xs"
        val result = newVId "a"
        val ty = FSyntax.RecordType Syntax.LabelMap.empty (* dummy *)
      in
        C.Abs
          { contParam = k
          , params = [xs]
          , body = C.Let
              { decs =
                  [C.ValDec
                     { exp = C.PrimOp
                         { primOp = FSyntax.PrimCall
                             Primitives.JavaScript_decodeUtf8
                         , tyargs = [ty]
                         , args = [C.Var xs]
                         }
                     , results = [SOME result]
                     }]
              , cont = C.AppCont {applied = k, args = [C.Var result]}
              }
          , attr = {alwaysInline = true}
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
        , (InitialEnv.VId_JavaScript_function, {exp = SOME JavaScript_function})
        , ( InitialEnv.VId_JavaScript_encodeUtf8
          , {exp = SOME JavaScript_encodeUtf8}
          )
        , ( InitialEnv.VId_JavaScript_decodeUtf8
          , {exp = SOME JavaScript_decodeUtf8}
          )
        ]
    fun goCExp (ctx: CpsSimplify.Context, exp) =
      simplifyCExp
        ( ctx
        , initialEnv
        , C.CVarMap.empty
        , TypedSyntax.VIdMap.empty
        , C.CVarMap.empty
        , exp
        )
  end (* local *)
end; (* structure CpsInline *)
