--BEGIN _G
local _G = _G
--END
--BEGIN assert
local assert = assert
--END
--BEGIN error
local error = error
--END
--BEGIN getmetatable
local getmetatable = getmetatable
--END
--BEGIN ipairs
local ipairs = ipairs
--END
--BEGIN pairs
local pairs = pairs
--END
--BEGIN pcall
local pcall = pcall
--END
--BEGIN setmetatable
local setmetatable = setmetatable
--END
--BEGIN math
local math = math
--END
--BEGIN math_abs: math
local math_abs = math.abs
--END
--BEGIN math_floor: math
local math_floor = math.floor
--END
--BEGIN math_fmod: math
local math_fmod = math.fmod
--END
--BEGIN math_modf: math
local math_modf = math.modf
--END
--BEGIN string
local string = string
--END
--BEGIN string_char: string
local string_char = string.char
--END
--BEGIN string_format: string
local string_format = string.format
--END
--BEGIN table
local table = table
--END
--BEGIN select
local select = select
--END
--BEGIN table_concat: table
local table_concat = table.concat
--END
--BEGIN table_pack: select
local table_pack = table.pack or function(...) return { n = select("#", ...), ... } end
--END
--BEGIN table_unpack: table
local table_unpack = table.unpack or unpack
--END
--BEGIN table_unpackN: table_unpack
local function table_unpackN(t)
  return table_unpack(t, 1, t.n)
end
--END
--BEGIN table_move: table
local table_move = table.move
--END
--BEGIN tonumber
local tonumber = tonumber
--END
--BEGIN bit
local bit = require "bit"
--END
--BEGIN bit_bnot: bit
local bit_bnot = bit.bnot
--END
--BEGIN bit_band: bit
local bit_band = bit.band
--END
--BEGIN bit_bor: bit
local bit_bor = bit.bor
--END
--BEGIN bit_bxor: bit
local bit_bxor = bit.bxor
--END
--BEGIN bit_lshift: bit
local bit_lshift = bit.lshift
--END
--BEGIN bit_rshift: bit
local bit_rshift = bit.rshift
--END
--BEGIN ffi
local ffi = require "ffi"
--END
--BEGIN int64_t: ffi
local int64_t = ffi.typeof("int64_t")
--END
--BEGIN uint64_t: ffi
local uint64_t = ffi.typeof("uint64_t")
--END

--BEGIN _id
local function _id(x)
  return x
end
--END

--BEGIN _method
local function _method(obj, name, ...)
  return obj[name](obj, ...)
end
--END

--BEGIN _mkFn2
local function _mkFn2(f)
  return function(a, b)
    return f({a, b})
  end
end
--END
--BEGIN _mkFn3
local function _mkFn3(f)
  return function(a, b, c)
    return f({a, b, c})
  end
end
--END

--BEGIN _exn_meta: string_format
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
--END
--BEGIN _Match_tag
local _Match_tag = { "Match" }
--END
--BEGIN _Match: setmetatable _Match_tag _exn_meta
local _Match = setmetatable({ tag = _Match_tag }, _exn_meta)
--END
--BEGIN _isMatch: getmetatable _Match_tag _exn_meta
local function _isMatch(e)
  return getmetatable(e) == _exn_meta and e.tag == _Match_tag
end
--END
--BEGIN _Bind_tag
local _Bind_tag = { "Bind" }
--END
--BEGIN _Bind: setmetatable _Bind_tag _exn_meta
local _Bind = setmetatable({ tag = _Bind_tag }, _exn_meta)
--END
--BEGIN _isBind: getmetatable _Bind_tag _exn_meta
local function _isBind(e)
  return getmetatable(e) == _exn_meta and e.tag == _Bind_tag
end
--END
--BEGIN _Overflow_tag
local _Overflow_tag = { "Overflow" }
--END
--BEGIN _Overflow: setmetatable _Overflow_tag _exn_meta
local _Overflow = setmetatable({ tag = _Overflow_tag }, _exn_meta)
--END
--BEGIN _isOverflow: getmetatable _Overflow_tag _exn_meta
local function _isOverflow(e)
  return getmetatable(e) == _exn_meta and e.tag == _Overflow_tag
end
--END
--BEGIN _Div_tag
local _Div_tag = { "Div" }
--END
--BEGIN _Div: setmetatable _Div_tag _exn_meta
local _Div = setmetatable({ tag = _Div_tag }, _exn_meta)
--END
--BEGIN _isDiv: getmetatable _Div_tag _exn_meta
local function _isDiv(e)
  return getmetatable(e) == _exn_meta and e.tag == _Div_tag
end
--END
--BEGIN _Size_tag
local _Size_tag = { "Size" }
--END
--BEGIN _Size: setmetatable _Size_tag _exn_meta
local _Size = setmetatable({ tag = _Size_tag }, _exn_meta)
--END
--BEGIN _isSize: getmetatable _Size_tag _exn_meta
local function _isSize(e)
  return getmetatable(e) == _exn_meta and e.tag == _Size_tag
end
--END
--BEGIN _Subscript_tag
local _Subscript_tag = { "Subscript" }
--END
--BEGIN _Subscript: setmetatable _Subscript_tag _exn_meta
local _Subscript = setmetatable({ tag = _Subscript_tag }, _exn_meta)
--END
--BEGIN _isSubscript: getmetatable _Subscript_tag _exn_meta
local function _isSubscript(e)
  return getmetatable(e) == _exn_meta and e.tag == _Subscript_tag
end
--END
--BEGIN _Fail_tag
local _Fail_tag = { "Fail" }
--END
--BEGIN _Fail: setmetatable _Fail_tag _exn_meta
local function _Fail(message)
  return setmetatable({ tag = _Fail_tag, payload = message }, _exn_meta)
end
--END
--BEGIN _isFail: getmetatable _Fail_tag _exn_meta
local function _isFail(e)
  return getmetatable(e) == _exn_meta and e.tag == _Fail_tag
end
--END
--BEGIN _Fail_payload
local function _Fail_payload(e)
  return e.payload
end
--END
--BEGIN _isError: getmetatable _exn_meta
local function _isError(e)
  return getmetatable(e) ~= _exn_meta
end
--END

--BEGIN _exnName: getmetatable _exn_meta
local function _exnName(e)
  if getmetatable(e) == _exn_meta then
    return e.tag[1]
  else
    return "Error"
  end
end
--END

--BEGIN __exn_instanceof: getmetatable _exn_meta
local function __exn_instanceof(e, tag)
  return getmetatable(e) == _exn_meta and e.tag == tag
end
--END

--BEGIN _raise: getmetatable setmetatable _exn_meta error
local function _raise(x, location)
  local e
  if getmetatable(x) == _exn_meta and location ~= nil then
    local traceback = debug.traceback(nil, 2)
    e = setmetatable({ tag = x.tag, payload = x.payload, location = location, traceback = traceback }, _exn_meta)
  else
    e = x
  end
  error(e, 1)
end
--END

--BEGIN MIN_INT54
local MIN_INT54 = -0x20000000000000
--END
--BEGIN MAX_INT54
local MAX_INT54 = 0x1fffffffffffff
--END

-- Int
--BEGIN _Int54_add: MIN_INT54 MAX_INT54 _raise _Overflow
local function _Int54_add(x, y)
  local z = x + y
  if (MIN_INT54 < z and z <= MAX_INT54) or (z == MIN_INT54 and x % 2 == y % 2) then
    return z
  else
    _raise(_Overflow, "Int.+")
  end
end
--END
--BEGIN _Int54_sub: MIN_INT54 MAX_INT54 _raise _Overflow
local function _Int54_sub(x, y)
  local z = x - y
  if (MIN_INT54 < z and z <= MAX_INT54) or (z == MIN_INT54 and x % 2 == y % 2) then
    return z
  else
    _raise(_Overflow, "Int.-")
  end
end
--END
--BEGIN _Int54_mul: MIN_INT54 MAX_INT54 _raise _Overflow
local function _Int54_mul(x, y)
  local z = 0 + x * y
  if (MIN_INT54 < z and z <= MAX_INT54) or (z == MIN_INT54 and (x % 2 == 0 or y % 2 == 0)) then
    return z
  else
    _raise(_Overflow, "Int.*")
  end
end
--BEGIN _Int54_div: _raise _Div MIN_INT54 _Overflow math_floor
local function _Int54_div(x, y)
  if y == 0 then
    _raise(_Div, "Int.div")
  elseif x == MIN_INT54 and y == -1 then
    _raise(_Overflow, "Int.div")
  end
  return 0 + math_floor(x / y)
end
--BEGIN _Int54_quot: _raise _Div MIN_INT54 _Overflow math_modf
local function _Int54_quot(x, y)
  if y == 0 then
    _raise(_Div, "Int.quot")
  elseif x == MIN_INT54 and y == -1 then
    _raise(_Overflow, "Int.quot")
  end
  return (math_modf(x / y))
end
--END
--BEGIN _Int54_mod: _raise _Div math_fmod
local function _Int54_mod(x, y)
  if y == 0 then
    _raise(_Div, "Int.mod")
  end
  -- For floating-point numbers, x % y is defined as x - math.floor(x/y)*y, which may not be mathematically correct (e.g. (-9007199254740992) % 3 should be 1, but yields 0 on Lua 5.1/5.2/LuaJIT)
  local r = math_fmod(x, y)
  if r == 0 or x * y >= 0 then
    return r
  else
    return r + y
  end
end
--END
--BEGIN _Int54_negate: MIN_INT54 _raise _Overflow
local function _Int54_negate(x)
  if x == MIN_INT54 then
    _raise(_Overflow, "Int.~")
  end
  return - x
end
--END
--BEGIN _Int54_abs: MIN_INT54 _raise _Overflow math_abs
local function _Int54_abs(x)
  if x == MIN_INT54 then
    _raise(_Overflow, "Int.abs")
  end
  return math_abs(x)
end
--END

-- Word
--BEGIN _Word32_mul: bit ffi
local _Word32_mul
do
  local tobit = bit.tobit
  local uint32_t = ffi.typeof("uint32_t")
  function _Word32_mul(x, y)
    return tobit(uint32_t(x) * uint32_t(y)) % 0x100000000
    --[[
    local x_lo = bit_band(x, 0xffff)
    local x_hi = bit_rshift(x, 16)
    local y_lo = bit_band(y, 0xffff)
    local y_hi = bit_rshift(y, 16)
    local lolo = x_lo * y_lo
    local lohi = x_lo * y_hi + x_hi * y_lo
    return (lolo + bit_lshift(bit_band(lohi, 0xffff), 16)) % 0x100000000
    ]]
  end
end
--END

-- Real
--BEGIN NEGATIVE_ZERO
local NEGATIVE_ZERO = 0 / (-1)
--END
--[[
local function _Real_TILDE(x)
  return NEGATIVE_ZERO - x
end
]]
--BEGIN _Real_mul: NEGATIVE_ZERO
local function _Real_mul(x, y)
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
--END

-- List
--BEGIN _list
local function _list(t)
  local xs = nil
  for i = t.n, 1, -1 do
    xs = { t[i], xs }
  end
  return xs
end
--END

-- Vector/Array
--BEGIN _Array_array: _raise _Size
local function _Array_array(n, init)
  if n < 0 then -- or maxLen < n
    _raise(_Size, "Array.array")
  end
  local t = { n = n }
  for i = 1, n do
    t[i] = init
  end
  return t
end
--END
--BEGIN _VectorOrArray_fromList
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
--END
--BEGIN _Vector_unsafeFromListRevN
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
--END
--BEGIN _VectorOrArray_tabulate: _raise _Size
local function _VectorOrArray_tabulate(n, f)
  if n < 0 then -- or maxLen < n
    _raise(_Size, "(Vector|Array).tabulate")
  end
  local t = { n = n }
  for i = 1, n do
    t[i] = f(i - 1)
  end
  return t
end
--END

-- Vector
--BEGIN _Vector_concat: table_move
local _Vector_concat
if table_move == nil then
  function _Vector_concat(xs)
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
else
  function _Vector_concat(xs)
    local n = 0
    local t = {}
    while xs ~= nil do
      local u = xs[1]
      local m = u.n
      table_move(u, 1, m, n + 1, t)
      n = n + m
      xs = xs[2]
    end
    t.n = n
    return t
  end
end
--END

-- Lua interface
--BEGIN _Lua_function: table_pack table_unpack
local function _Lua_function(f)
  return function(...)
    local r = f(table_pack(...))
    return table_unpack(r, 1, r.n)
  end
end
--END
--BEGIN _newTableWith: ipairs
local function _newTableWith(entries)
  local t = {}
  for _, p in ipairs(entries) do
    t[p[1]] = p[2]
  end
  return t
end
--END
