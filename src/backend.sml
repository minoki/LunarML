(*
 * Copyright (c) 2024 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure Backend:
sig
  datatype lua_runtime = LUA_PLAIN | LUA_CONTINUATIONS
  datatype code_style = DIRECT_STYLE | CPS
  datatype backend =
    BACKEND_LUA of lua_runtime
  | BACKEND_LUAJIT
  | BACKEND_JS of code_style
end =
struct
  datatype lua_runtime = LUA_PLAIN | LUA_CONTINUATIONS
  datatype code_style = DIRECT_STYLE | CPS
  datatype backend =
    BACKEND_LUA of lua_runtime
  | BACKEND_LUAJIT
  | BACKEND_JS of code_style
end;
