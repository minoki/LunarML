local function _recordEqual(fields)
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
local function _EQUAL_prim(t)
  return t[1] == t[2]
end
local function _LT_prim(t)
  return t[1] < t[2]
end
local function _GT_prim(t)
  return t[1] > t[2]
end
local function _LE_prim(t)
  return t[1] <= t[2]
end
local function _GE_prim(t)
  return t[1] >= t[2]
end

local _Match = "Match"
local _Bind = "Bind"
local _Overflow = "Overflow"
local _Div = "Div"

local function _raise(x)
  error(x, 1)
end

-- Int
local function __add_int(x, y)
  assert(math.type(x) == "integer")
  assert(math.type(y) == "integer")
  local z = x + y
  if y > 0 and z < x then
    error(_Overflow)
  elseif y < 0 and z < x then
    error(_Overflow)
  else
    return z
  end
end
local function _add_int(t)
  return __add_int(t[1], t[2])
end
local function __sub_int(x, y)
  assert(math.type(x) == "integer")
  assert(math.type(y) == "integer")
  local z = x - y
  if y < 0 and z < x then
    error(_Overflow)
  elseif y > 0 and x < z then
    error(_Overflow)
  else
    return z
  end
end
local function _sub_int(t)
  return __sub_int(t[1], t[2])
end
local function __mul_int(x, y)
  assert(math.type(x) == "integer")
  assert(math.type(y) == "integer")
  local z = x * y
  if (x ~= 0 and z // x ~= y) or (y ~= 0 and z // y ~= x) then
    error(_Overflow)
  else
    return z
  end
end
local function _mul_int(t)
  return __mul_int(t[1], t[2])
end
local function __div_int(x, y)
  assert(math.type(x) == "integer")
  assert(math.type(y) == "integer")
  if y == 0 then
    error(_Div)
  elseif x == math.mininteger and y == -1 then
    error(_Overflow)
  end
  return x // y
end
local function _div_int(t)
  return __div_int(t[1], t[2])
end
local function __mod_int(x, y)
  assert(math.type(x) == "integer")
  assert(math.type(y) == "integer")
  if y == 0 then
    error(_Div)
  end
  return x % y
end
local function _mod_int(t)
  return __mod_int(t[1], t[2])
end
local function _negate_int(x)
  assert(math.type(x) == "integer")
  if x == math.mininteger then
    error(_Overflow)
  end
  return - x
end
local function _abs_int(x)
  assert(math.type(x) == "integer")
  if x == math.mininteger then
    error(_Overflow)
  end
  return math.abs(x)
end

-- Word
local function _add_word(t)
  return t[1] + t[2]
end
local function _sub_word(t)
  return t[1] - t[2]
end
local function _mul_word(t)
  return t[1] * t[2]
end
local function __div_word(x, y)
  assert(math.type(x) == "integer")
  assert(math.type(y) == "integer")
  if y == 0 then
    error(_Div)
  elseif y > 0 then
    if x >= 0 then
      return x // y
    else -- x < 0
      local x1 = (x + math.maxinteger) + 1 -- x + 2^63
      local u1 = ((math.maxinteger % y) + 1) % y -- 2^63 % y
      local v1 = math.maxinteger // y
      if math.maxinteger % y == y - 1 then
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
local function _div_word(t)
  return __div_word(t[1], t[2])
end
local function __mod_word(x, y)
  assert(math.type(x) == "integer")
  assert(math.type(y) == "integer")
  if y == 0 then
    error(_Div)
  elseif y > 0 then
    if x >= 0 then
      return x % y
    else -- x < 0
      local x1 = ((x + math.maxinteger) + 1) % y -- (x + 2^63) % y
      local u1 = ((math.maxinteger % y) + 1) % y -- 2^63 % y
      if y <= math.maxinteger // 2 + 1 then -- y <= 2^62
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
local function _mod_word(t)
  return __mod_word(t[1], t[2])
end
local __LT_word = math.ult
local function _LT_word(t)
  return __LT_word(t[1], t[2])
end
local function __GT_word(x, y)
  return __LT_word(y, x)
end
local function _GT_word(t)
  return __LT_word(t[2], t[1])
end
local function __LE_word(x, y)
  return x == y or __LT_word(x, y)
end
local function _LE_word(t)
  return __LE_word(t[1], t[2])
end
local function __GE_word(x, y)
  return __LE_word(y, x)
end
local function _GE_word(t)
  return __LE_word(t[2], t[1])
end

-- Real
local function _add_real(t)
  return t[1] + t[2]
end
local function _sub_real(t)
  return t[1] - t[2]
end
local function _mul_real(t)
  return t[1] * t[2]
end
local function _divide_real(t)
  return t[1] / t[2]
end
local _abs_real = math.abs
local function _negate_real(x)
  return -x
end

-- List
local _nil = { tag = "nil" }
local function _cons(t)
  return { tag = "::", payload = t }
end
local function _EQUAL_list(eq)
  local function go(a, b)
    local at, bt = a.tag, b.tag
    if at ~= bt then
      return false
    elseif at == "nil" then
      return true
    elseif eq(a.payload[1], b.payload[1]) then
      return go(a.payload[2], b.payload[2])
    else
      return false
    end
  end
  return function(t)
    return go(t[1], t[2])
  end
end

-- Ref
local function _ref(x)
  return { tag = "ref", payload = x }
end
local function _set(t)
  t[1].payload = t[2]
end

local function _print(x)
  io.write(x)
  return {}
end

local function _Int_toString(x)
  return string.gsub(tostring(x), "-", "~")
end

local function _string_append(t)
  return t[1] .. t[2]
end

local function _not(x)
  return not x
end
