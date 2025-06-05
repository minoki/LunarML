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
  | BACKEND_JS of {style: code_style, os: string, default_ext: string}
end =
struct
  (*
   * Our possible TARGET_OS:
   *   * lua
   *   * node
   *   * web
   *
   * MLton's TARGET_OS:
   *   * aix
   *   * cygwin
   *   * darwin
   *   * freebsd
   *   * hurd
   *   * hpux
   *   * linux
   *   * mingw
   *   * netbsd
   *   * openbsd
   *   * solaris
   *   * wasi
   *)
  datatype lua_runtime =
    LUA_PLAIN
  | LUA_CONTINUATIONS
  datatype code_style = DIRECT_STYLE | CPS
  datatype backend =
    BACKEND_LUA of lua_runtime
  | BACKEND_LUAJIT
  | BACKEND_JS of {style: code_style, os: string, default_ext: string}
end;
