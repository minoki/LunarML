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
--BEGIN math_type: math
local math_type = math.type
--END
--BEGIN math_maxinteger: math
local math_maxinteger = math.maxinteger
--END
--BEGIN math_mininteger: math
local math_mininteger = math.mininteger
--END
--BEGIN math_ult: math
local math_ult = math.ult
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
--BEGIN table_concat: table
local table_concat = table.concat
--END
--BEGIN table_pack: table
local table_pack = table.pack
--END
--BEGIN table_unpack: table
local table_unpack = table.unpack
--END
--BEGIN table_unpackN: table_unpack
local function table_unpackN(t)
  return table_unpack(t, 1, t.n)
end
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

-- Int
--BEGIN _Int_add: _raise _Overflow
local function _Int_add(x, y)
  -- assert(math_type(x) == "integer")
  -- assert(math_type(y) == "integer")
  local z = x + y
  if y > 0 and z < x then
    _raise(_Overflow, "Int.+")
  elseif y < 0 and z > x then
    _raise(_Overflow, "Int.+")
  else
    return z
  end
end
--END
--BEGIN _Int_sub: _raise _Overflow
local function _Int_sub(x, y)
  -- assert(math_type(x) == "integer")
  -- assert(math_type(y) == "integer")
  local z = x - y
  if y < 0 and z < x then
    _raise(_Overflow, "Int.-")
  elseif y > 0 and x < z then
    _raise(_Overflow, "Int.-")
  else
    return z
  end
end
--END
--BEGIN _Int_mul: _raise _Overflow
local function _Int_mul(x, y)
  -- assert(math_type(x) == "integer")
  -- assert(math_type(y) == "integer")
  local z = x * y
  if (x ~= 0 and z // x ~= y) or (y ~= 0 and z // y ~= x) then
    _raise(_Overflow, "Int.*")
  else
    return z
  end
end
--END
--BEGIN _Int_div: _raise _Div math_mininteger _Overflow
local function _Int_div(x, y)
  -- assert(math_type(x) == "integer")
  -- assert(math_type(y) == "integer")
  if y == 0 then
    _raise(_Div, "Int.div")
  elseif x == math_mininteger and y == -1 then
    _raise(_Overflow, "Int.div")
  end
  return x // y
end
--END
--BEGIN _Int_mod: _raise _Div
local function _Int_mod(x, y)
  -- assert(math_type(x) == "integer")
  -- assert(math_type(y) == "integer")
  if y == 0 then
    _raise(_Div, "Int.mod")
  end
  return x % y
end
--END
--BEGIN _Int_negate: math_mininteger _raise _Overflow
local function _Int_negate(x)
  -- assert(math_type(x) == "integer")
  if x == math_mininteger then
    _raise(_Overflow, "Int.~")
  end
  return - x
end
--END
--BEGIN _Int_abs: math_mininteger _raise _Overflow math_abs
local function _Int_abs(x)
  -- assert(math_type(x) == "integer")
  if x == math_mininteger then
    _raise(_Overflow, "Int.abs")
  end
  return math_abs(x)
end
--END

-- Word
--BEGIN _Word_div: _raise _Div math_ult
local function _Word_div(x, y)
  -- assert(math_type(x) == "integer")
  -- assert(math_type(y) == "integer")
  if y == 0 then
    _raise(_Div, "Word.div")
  elseif y > 0 then
    if x >= 0 then
      return x // y
    else -- x < 0
      -- Algorithm from Programming in Lua, 4th ed.
      local q = ((x >> 1) // y) << 1
      local r = x - q * y
      if math_ult(r, y) then
        return q
      else
        return q + 1
      end
    end
  else -- y < 0
    if math_ult(x, y) then
      return 0
    else
      return 1
    end
  end
end
--END
--BEGIN _Word_mod: _raise _Div math_ult
local function _Word_mod(x, y)
  -- assert(math_type(x) == "integer")
  -- assert(math_type(y) == "integer")
  if y == 0 then
    _raise(_Div, "Word.mod")
  elseif y > 0 then
    if x >= 0 then
      return x % y
    else -- x < 0
      local q = ((x >> 1) // y) << 1
      local r = x - q * y
      if math_ult(r, y) then
        return r
      else
        return r - y
      end
    end
  else -- y < 0
    if math_ult(x, y) then
      return x
    else
      return x - y
    end
  end
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
--BEGIN _Vector_concat
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
