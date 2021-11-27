local assert = assert
local error = error
local getmetatable = getmetatable
local pairs = pairs
local pcall = pcall
local setmetatable = setmetatable
local math = math
local math_abs = math.abs
local math_type = math.type
local math_maxinteger = math.maxinteger
local math_mininteger = math.mininteger
local string = string
local string_format = string.format
local table = table
local table_pack = table.pack
local table_unpack = table.unpack

local function _Unit_EQUAL(t)
  return true
end
local function _Record_EQUAL(fields)
  return function(t)
    local a, b = t[1], t[2]
    for k,eq in pairs(fields) do
      if not eq({a[k], b[k]}) then
        return false
      end
    end
    return true
  end
end
local function _EQUAL(t)
  return t[1] == t[2]
end

local function _id(x)
  return x
end

local _exn_meta = {}
function _exn_meta:__tostring()
  return string_format("%s: %s", self.location or "<no location info>", self.tag[1])
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

local function __exn_instanceof(e, tag)
  return getmetatable(e) == _exn_meta and e.tag == tag
end

local function _raise(x, location)
  local e = setmetatable({ tag = x.tag, payload = x.payload, location = location }, _exn_meta)
  error(e, 1)
end

-- Int
local function __Int_add(x, y)
  assert(math_type(x) == "integer")
  assert(math_type(y) == "integer")
  local z = x + y
  if y > 0 and z < x then
    _raise(_Overflow, "Int.+")
  elseif y < 0 and z < x then
    _raise(_Overflow, "Int.+")
  else
    return z
  end
end
local function __Int_sub(x, y)
  assert(math_type(x) == "integer")
  assert(math_type(y) == "integer")
  local z = x - y
  if y < 0 and z < x then
    _raise(_Overflow, "Int.-")
  elseif y > 0 and x < z then
    _raise(_Overflow, "Int.-")
  else
    return z
  end
end
local function __Int_mul(x, y)
  assert(math_type(x) == "integer")
  assert(math_type(y) == "integer")
  local z = x * y
  if (x ~= 0 and z // x ~= y) or (y ~= 0 and z // y ~= x) then
    _raise(_Overflow, "Int.*")
  else
    return z
  end
end
local function __Int_div(x, y)
  assert(math_type(x) == "integer")
  assert(math_type(y) == "integer")
  if y == 0 then
    _raise(_Div, "Int.div")
  elseif x == math.mininteger and y == -1 then
    _raise(_Overflow, "Int.div")
  end
  return x // y
end
local function __Int_mod(x, y)
  assert(math_type(x) == "integer")
  assert(math_type(y) == "integer")
  if y == 0 then
    _raise(_Div, "Int.mod")
  end
  return x % y
end
local function _Int_negate(x)
  assert(math_type(x) == "integer")
  if x == math_mininteger then
    _raise(_Overflow, "Int.~")
  end
  return - x
end
local function _Int_abs(x)
  assert(math_type(x) == "integer")
  if x == math_mininteger then
    _raise(_Overflow, "Int.abs")
  end
  return math.abs(x)
end

-- Word
local function __Word_div(x, y)
  assert(math_type(x) == "integer")
  assert(math_type(y) == "integer")
  if y == 0 then
    _raise(_Div, "Word.div")
  elseif y > 0 then
    if x >= 0 then
      return x // y
    else -- x < 0
      local x1 = (x + math_maxinteger) + 1 -- x + 2^63
      local u1 = ((math_maxinteger % y) + 1) % y -- 2^63 % y
      local v1 = math_maxinteger // y
      if math_maxinteger % y == y - 1 then
        v1 = v1 + 1
      end
      -- v1 == 2^63 // y
      if x1 % y < y - u1 then
        return x1 // y + v1
      else
        return x1 // y + v1 + 1
      end
    end
  else -- y < 0
    if x >= 0 then
      -- x < 2^63 and y + 2^64 > 2^63
      -- Therefore, x // (y + 2^64) = 0
      return 0
    else -- x < 0
      -- (x + 2^64) // (y + 2^64)
      if x >= y then
        -- (x + 2^64) // (y + 2^64)
        return 1
      else
        return 0
      end
    end
  end
end
local function __Word_mod(x, y)
  assert(math_type(x) == "integer")
  assert(math_type(y) == "integer")
  if y == 0 then
    _raise(_Div, "Word.mod")
  elseif y > 0 then
    if x >= 0 then
      return x % y
    else -- x < 0
      local x1 = ((x + math_maxinteger) + 1) % y -- (x + 2^63) % y
      local u1 = ((math_maxinteger % y) + 1) % y -- 2^63 % y
      if y <= math_maxinteger // 2 + 1 then -- y <= 2^62
        -- x1 + u1 <= 2*(y-1) < 2^63
        return (x1 + u1) % y -- (x + 2^64) % y
      else
        -- 2^62 < y < 2^63
        -- math.maxinteger % y == math.maxinteger - y
        -- u1 = 2^63 - y <= 2^63 - (2^62 - 1) = 2^62 + 1
        -- 0 <= (2^63 - 1) - y <= (2^63 - 1) - (2^62 - 1) = 2^62
        -- 0 <= u1 - 1 <= 2^62
        -- 2^64 % y = ((2^63 % y) + (2^63 % y)) % y
        --          = ((2^63 - y) + (2^63 - y)) % y
        --          = (u1 + u1) % y -- u1
        local y2 = y // 2
        if x1 <= y2 then
          if u1 <= y2 then
            -- x1 + u1 <= 2 * (y // 2) <= y
            return (x1 + u1) % y
          else -- y2 < u1
            -- y - u1 <= y2
            -- -y2 <= x1 - (y - u1) <= y2
            return (x1 - (y - u1)) % y
          end
        else -- y2 < x1
          -- y - x1 <= y2
          if u1 <= y2 then
            return (u1 - (y - x1)) % y
          else -- y2 < u1
            local y3 = y - y2
            return ((x1 - y2) + (u1 - y3)) % y
          end
        end
      end
    end
  else -- y < 0
    if x >= 0 then
      -- x < 2^63 and y + 2^64 > 2^63
      return x
    else -- x < 0
      -- (x + 2^64) % (y + 2^64)
      if x >= y then
        return x - y
      else -- x < y
        return x
      end
    end
  end
end
local __Word_LT = math.ult

-- List
local _nil = { tag = "nil" }
local function _cons(t)
  return { tag = "::", payload = t }
end
local function _List_EQUAL(eq)
  local function go(a, b)
    local at, bt = a.tag, b.tag
    if at ~= bt then
      return false
    elseif at == "nil" then
      return true
    elseif eq({a.payload[1], b.payload[1]}) then
      return go(a.payload[2], b.payload[2])
    else
      return false
    end
  end
  return function(t)
    return go(t[1], t[2])
  end
end
local function _list(t)
  local xs = _nil
  for i = t.n, 1, -1 do
    xs = { tag = "::", payload = { t[i], xs } }
  end
  return xs
end

-- Ref
local function _ref(x)
  return { tag = "ref", payload = x }
end

-- Array
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
  while xs.tag == "::" do
    n = n + 1
    t[n] = xs.payload[1]
    xs = xs.payload[2]
  end
  t.n = n
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
local function _VectorOrArray_sub(t)
  local a, i = t[1], t[2]
  if i < 0 or a.n <= i then
    _raise(_Subscript, "(Vector|Array).sub")
  end
  return a[i+1]
end
local function _Array_update(t)
  local a, i, x = t[1], t[2], t[3]
  if i < 0 or a.n <= i then
    _raise(_Subscript, "Array.update")
  end
  a[i+1] = x
  return nil
end

-- Vector
local function _Vector_EQUAL(eq)
  local function go(a, b)
    local n = a.n
    if n ~= b.n then
      return false
    end
    for i = 1, n do
      if not eq({a[i], b[i]}) then
        return false
      end
    end
    return true
  end
  return function(t)
    return go(t[1], t[2])
  end
end
local function _Vector_concat(xs)
  local n = 0
  local t = {}
  while xs.tag == "::" do
    local p = xs.payload
    local u = p[1]
    local m = u.n
    for i = 1,m do
      t[n + i] = u[i]
    end
    n = n + m
    xs = p[2]
  end
  t.n = n
  return t
end

-- Lua interface
local function _Lua_global(name)
  return _ENV[name]
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
