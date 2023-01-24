local assert = assert
local error = error
local getmetatable = getmetatable
local pairs = pairs
local pcall = pcall
local setmetatable = setmetatable
local math = math
local math_abs = math.abs
local math_floor = math.floor
local math_modf = math.modf
local string = string
local string_char = string.char
local string_format = string.format
local table = table
local select = select
local table_pack = table.pack or function(...) return { n = select("#", ...), ... } end
local table_unpack = table.unpack or unpack

local function _id(x)
  return x
end

local _exn_meta = {}
function _exn_meta:__tostring()
  local traceback = self.traceback
  if traceback then
    traceback = "\n" .. traceback
  else
    traceback = ""
  end
  return string_format("%s: %s%s", self.location or "<no location info>", self.tag[1], traceback)
end
local _Match_tag = { "Match" }
local _Match = setmetatable({ tag = _Match_tag }, _exn_meta)
local _Bind_tag = { "Bind" }
local _Bind = setmetatable({ tag = _Bind_tag }, _exn_meta)
local _Overflow_tag = { "Overflow" }
local _Overflow = setmetatable({ tag = _Overflow_tag }, _exn_meta)
local _Div_tag = { "Div" }
local _Div = setmetatable({ tag = _Div_tag }, _exn_meta)
local _Size_tag = { "Size" }
local _Size = setmetatable({ tag = _Size_tag }, _exn_meta)
local _Subscript_tag = { "Subscript" }
local _Subscript = setmetatable({ tag = _Subscript_tag }, _exn_meta)
local _Fail_tag = { "Fail" }
local function _Fail(message)
  return setmetatable({ tag = _Fail_tag, payload = message }, _exn_meta)
end
local _LuaError_tag = { "LuaError" }
local function _LuaError(x)
  return setmetatable({ tag = _LuaError_tag, payload = x }, _exn_meta)
end

local function _handle(f)
  local success, result = pcall(f)
  if not success and getmetatable(result) ~= _exn_meta then
    result = _LuaError(result)
  end
  return success, result
end

local function _exnName(e)
  return e.tag[1]
end

local function __exn_instanceof(e, tag)
  return e.tag == tag
end

local function _raise(x, location)
  local e
  if x.tag == _LuaError_tag then
    e = x.payload
  elseif location ~= nil then
    local traceback = debug.traceback(nil, 2)
    e = setmetatable({ tag = x.tag, payload = x.payload, location = location, traceback = traceback }, _exn_meta)
  else
    e = x
  end
  error(e, 1)
end

local MIN_INT32 = -0x80000000
local MAX_INT32 = 0x7fffffff

-- Int
local function __Int_add(x, y)
  local z = x + y
  if z < MIN_INT32 or MAX_INT32 < z then
    _raise(_Overflow, "Int.+")
  else
    return z
  end
end
local function __Int_sub(x, y)
  local z = x - y
  if z < MIN_INT32 or MAX_INT32 < z then
    _raise(_Overflow, "Int.-")
  else
    return z
  end
end
local function __Int_mul(x, y)
  local z = x * y
  if z < MIN_INT32 or MAX_INT32 < z then
    _raise(_Overflow, "Int.*")
  else
    return z
  end
end
local function __Int_div(x, y)
  if y == 0 then
    _raise(_Div, "Int.div")
  elseif x == MIN_INT32 and y == -1 then
    _raise(_Overflow, "Int.div")
  end
  return math_floor(x / y)
end
local function __Int_quot(x, y)
  if y == 0 then
    _raise(_Div, "Int.quot")
  elseif x == MIN_INT32 and y == -1 then
    _raise(_Overflow, "Int.quot")
  end
  return (math_modf(x / y))
end
local function __Int_mod(x, y)
  if y == 0 then
    _raise(_Div, "Int.mod")
  end
  return x % y
end
local function _Int_negate(x)
  if x == MIN_INT32 then
    _raise(_Overflow, "Int.~")
  end
  return - x
end
local function _Int_abs(x)
  if x == MIN_INT32 then
    _raise(_Overflow, "Int.abs")
  end
  return math_abs(x)
end

-- Word
local function __Word_add(x, y)
  return (x + y) % 0x100000000
end
local function __Word_sub(x, y)
  return (x - y) % 0x100000000
end
local __Word_mul
do
  local bit = require "bit"
  local tobit = bit.tobit
  --[[
  local band = bit.band
  local lshift = bit.lshift
  local rshift = bit.rshift
  ]]
  local ffi = require "ffi"
  local uint32_t = ffi.typeof("uint32_t")
  function __Word_mul(x, y)
    return tobit(uint32_t(x) * uint32_t(y)) % 0x100000000
    --[[
    local x_lo = band(x, 0xffff)
    local x_hi = rshift(x, 16)
    local y_lo = band(y, 0xffff)
    local y_hi = rshift(y, 16)
    local lolo = x_lo * y_lo
    local lohi = x_lo * y_hi + x_hi * y_lo
    return (lolo + lshift(band(lohi, 0xffff), 16)) % 0x100000000
    ]]
  end
end
local function __Word_div(x, y)
  if y == 0 then
    _raise(_Div, "Word.div")
  else
    return math_floor(x / y)
  end
end
local function __Word_mod(x, y)
  if y == 0 then
    _raise(_Div, "Word.mod")
  else
    return x % y
  end
end
local function _Word_negate(x)
  return (- x) % 0x100000000
end
local function __Word_LT(x, y)
  return x < y
end

-- Real
local NEGATIVE_ZERO = 0 / (-1)
--[[
local function _Real_TILDE(x)
  return NEGATIVE_ZERO - x
end
]]
local function __Real_mul(x, y)
  local z = x * y
  if z == 0 then
    if x < 0 then
      return NEGATIVE_ZERO * y
    elseif y < 0 then
      return NEGATIVE_ZERO * x
    end
  end
  return z
end

-- List
local function _list(t)
  local xs = nil
  for i = t.n, 1, -1 do
    xs = { t[i], xs }
  end
  return xs
end

-- Vector/Array
local function _Array_array(t)
  local n, init = t[1], t[2]
  if n < 0 then -- or maxLen < n
    _raise(_Size, "Array.array")
  end
  local t = { n = n }
  for i = 1, n do
    t[i] = init
  end
  return t
end
local function _VectorOrArray_fromList(xs)
  local t = {}
  local n = 0
  while xs ~= nil do
    n = n + 1
    t[n] = xs[1]
    xs = xs[2]
  end
  t.n = n
  return t
end
local function _Vector_unsafeFromListRevN(n, xs)
  local t = { n = n }
  while xs ~= nil do
    t[n] = xs[1]
    xs = xs[2]
    n = n - 1
  end
  -- n must be 0
  return t
end
local function _VectorOrArray_tabulate(t)
  local n, f = t[1], t[2]
  if n < 0 then -- or maxLen < n
    _raise(_Size, "(Vector|Array).tabulate")
  end
  local t = { n = n }
  for i = 1, n do
    t[i] = f(i - 1)
  end
  return t
end

-- Vector
local function _Vector_concat(xs)
  local n = 0
  local t = {}
  while xs ~= nil do
    local u = xs[1]
    local m = u.n
    for i = 1,m do
      t[n + i] = u[i]
    end
    n = n + m
    xs = xs[2]
  end
  t.n = n
  return t
end

-- Lua interface
local function _Lua_global(name)
  return _G[name]
end
local function _Lua_call(f)
  return function(v)
    return table_pack(f(table_unpack(v, 1, v.n)))
  end
end
local function _Lua_method(t)
  local self, name = t[1], t[2]
  return function(v)
    return table_pack(self[name](self, table_unpack(v, 1, v.n)))
  end
end
local function _Lua_newTable()
  return {}
end
local function _Lua_function(f)
  return function(...)
    local r = f(table_pack(...))
    return table_unpack(r, 1, r.n)
  end
end
