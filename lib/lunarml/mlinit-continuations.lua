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
local math_ult = math.ult
local string = string
local string_char = string.char
local string_format = string.format
local table = table
local table_pack = table.pack
local table_unpack = table.unpack

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

-- Int
local function __Int_add(x, y)
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
local function __Int_sub(x, y)
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
local function __Int_mul(x, y)
  -- assert(math_type(x) == "integer")
  -- assert(math_type(y) == "integer")
  local z = x * y
  if (x ~= 0 and z // x ~= y) or (y ~= 0 and z // y ~= x) then
    _raise(_Overflow, "Int.*")
  else
    return z
  end
end
local function __Int_div(x, y)
  -- assert(math_type(x) == "integer")
  -- assert(math_type(y) == "integer")
  if y == 0 then
    _raise(_Div, "Int.div")
  elseif x == math_mininteger and y == -1 then
    _raise(_Overflow, "Int.div")
  end
  return x // y
end
local function __Int_mod(x, y)
  -- assert(math_type(x) == "integer")
  -- assert(math_type(y) == "integer")
  if y == 0 then
    _raise(_Div, "Int.mod")
  end
  return x % y
end
local function _Int_negate(x)
  -- assert(math_type(x) == "integer")
  if x == math_mininteger then
    _raise(_Overflow, "Int.~")
  end
  return - x
end
local function _Int_abs(x)
  -- assert(math_type(x) == "integer")
  if x == math_mininteger then
    _raise(_Overflow, "Int.abs")
  end
  return math_abs(x)
end

-- Word
local function __Word_div(x, y)
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
local function __Word_mod(x, y)
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
local __Word_LT = math_ult

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

-- Delimited continuations
local _handle, _withSubCont, _pushSubCont, _run
do
  local ipairs = ipairs
  local table_insert = table.insert
  local table_remove = table.remove
  local table_move = table.move
  local coroutine = coroutine
  local coroutine_create = coroutine.create
  local coroutine_yield = coroutine.yield
  local coroutine_resume = coroutine.resume
  local _depth = 0
  function _handle(f)
    local success, result
    if _depth > 150 then
      local c = coroutine_create(function()
          return "return", f()
      end)
      local olddepth = _depth
      _depth = 0
      success, result = coroutine_yield("handle", c)
      _depth = olddepth
    else
      local olddepth = _depth
      _depth = olddepth + 1
      success, result = pcall(f)
      _depth = olddepth
    end
    if not success and getmetatable(result) ~= _exn_meta then
      result = _LuaError(result)
    end
    return success, result
  end
  function _pushPrompt(tag, f)
    local co = coroutine_create(function()
        return "return", f(nil)
    end)
    local status, a = coroutine_yield("prompt", tag, co)
    if status then
      return a
    else
      error(a, 0)
    end
  end
  function _withSubCont(tag, f)
    local a = coroutine_yield("capture", tag, f)
    return a()
  end
  function _pushSubCont(subcont, f)
    if not subcont[1] then
      error("cannot resume captured continuation multiple times")
    end
    local slice = subcont[1]
    subcont[1] = nil
    local status, a = coroutine_yield("push-subcont", slice, f)
    if status then
      return a
    else
      error(a, 0)
    end
  end
  function _run(f)
    local co = coroutine_create(function()
        return "return", f()
    end)
    local stack = {{co, nil}}
    -- stack[i][1]: coroutine, stack[i][2]: prompt tag or nil
    local values = {}
    while true do
      ::continue::
      local status, a, b, c = coroutine_resume(stack[#stack][1], table_unpack(values))
      if status then
        if a == "return" then
          if #stack == 1 then
            return b
          else
            table_remove(stack)
            values = {true, b}
          end
        elseif a == "handle" then
          -- b: the new coroutine
          table_insert(stack, {b, nil})
          values = {}
        elseif a == "prompt" then
          -- b: tag
          -- c: the new coroutine
          table_insert(stack, {c, b})
          values = {}
        elseif a == "capture" then
          -- b: tag
          -- c: callback
          for i = #stack, 1, -1 do
            if stack[i][2] == b then
              local slice = {}
              table_move(stack, i, #stack, 1, slice)
              -- slice[1], ... = stack[i+1], ..., stack[#stack]
              for j = i, #stack do
                stack[j] = nil
              end
              local subcont = {slice}
              local co = coroutine_create(function()
                  return "return", c(subcont)
              end)
              table_insert(stack, {co, nil})
              values = {}
              goto continue
            end
          end
          error("prompt not found")
        elseif a == "abort" then
          -- b: tag
          -- c: value
          for i = #stack, 1, -1 do
            if stack[i][2] == b then
              for j = i, #stack do
                stack[j] = nil
              end
              values = {true, c}
              goto continue
            end
          end
          error("prompt not found")
        elseif a == "push-subcont" then
          -- b: slice
          -- c: action
          for _, v in ipairs(b) do
            table.insert(stack, v)
          end
          values = {c}
        else
          error("unexpected result from the function: " .. tostring(a))
        end
      else
        if #stack == 1 then
          error(a, 0)
        else
          table_remove(stack)
          values = {false, a}
        end
      end
    end
  end
end
