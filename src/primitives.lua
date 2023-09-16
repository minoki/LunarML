local TV = {
  a = {"tyA", "tyVarA"},
  b = {"tyB", "tyVarB"},
  c = {"tyC", "tyVarC"},
  d = {"tyD", "tyVarD"},
  eqA = {"tyEqA", "tyVarEqA", "IsEqType"},
}
local PRIMITIVES
do
  local string_format = string.format
  local unit = {"unit"}
  local bool = {"bool"}
  -- local int = {"int"}
  local intA = {"intA"}
  local intB = {"intB"}
  local int32 = {"int32"}
  local int54 = {"int54"}
  local int64 = {"int64"}
  -- local word = {"word"}
  local wordA = {"wordA"}
  local wordB = {"wordB"}
  local word32 = {"word32"}
  local word64 = {"word64"}
  local real = {"real"}
  local char = {"char"}
  local char16 = {"char16"}
  local string = {"string"}
  local string16 = {"string16"}
  local intInf = {"intInf"}
  local exn = {"exn"}
  local exntag = {"exntag"}
  local LuaValue = {"LuaValue"}
  local JSValue = {"JavaScriptValue"}
  local ref = function(payloadTy) return {string_format("refOf (%s)", payloadTy[1])} end
  local list = function(elemTy) return {string_format("listOf (%s)", elemTy[1])} end
  local vector = function(elemTy) return {string_format("vectorOf (%s)", elemTy[1])} end
  local array = function(elemTy) return {string_format("arrayOf (%s)", elemTy[1])} end
  local pair = function(ty1, ty2) return {string_format("pairOf (%s, %s)", ty1[1], ty2[1])} end
  local tuple = function(types)
    local t = {}
    for _, v in pairs(types) do
      table.insert(t, v[1])
    end
    return {string_format("tupleOf [%s]", table.concat(t, ", "))}
  end
  local function1 = function(resultTy, arg1Ty) return {string_format("function1Of (%s, %s)", resultTy[1], arg1Ty[1])} end
  local function2 = function(resultTy, arg1Ty, arg2Ty) return {string_format("function2Of (%s, %s, %s)", resultTy[1], arg1Ty[1], arg2Ty[1])} end
  local function3 = function(resultTy, arg1Ty, arg2Ty, arg3Ty) return {string_format("function3Of (%s, %s, %s, %s)", resultTy[1], arg1Ty[1], arg2Ty[1], arg3Ty[1])} end
  local promptTag = function(ty) return {string_format("promptTagOf (%s)", ty[1])} end
  local subcont = function(ty1, ty2) return {string_format("subcontOf (%s, %s)", ty1[1], ty2[1])} end
  local function Binary(a, b)
    return function(result)
      return { vars = {}, args = {a, b}, result = result }
    end
  end
  local function HomoUnary(a)
    return { vars = {}, args = {a}, result = a }
  end
  local function HomoBinary(a)
    return { vars = {}, args = {a, a}, result = a }
  end
  local function Compare(a)
    return { vars = {}, args = {a, a}, result = bool }
  end

  PRIMITIVES = {
    {
      name = "=",
      srcname = "EQUAL",
      type = { vars = {TV.eqA}, args = {TV.eqA, TV.eqA}, result = bool },
      mayraise = false,
      discardable = true,
    },
    {
      name = "call2",
      srcname = "call2",
      type = { vars = {TV.a, TV.b, TV.c}, args = {function2(TV.a, TV.b, TV.c), TV.b, TV.c}, result = TV.a },
      mayraise = true,
      discardable = false,
    },
    {
      name = "call3",
      srcname = "call3",
      type = { vars = {TV.a, TV.b, TV.c, TV.d}, args = {function3(TV.a, TV.b, TV.c, TV.d), TV.b, TV.c, TV.d}, result = TV.a },
      mayraise = true,
      discardable = false,
    },
    {
      name = "List.::",
      srcname = "List_cons",
      type = { vars = {TV.a}, args = {TV.a, list(TV.a)}, result = list(TV.a) },
      mayraise = false,
      discardable = true,
    },
    {
      name = "List.null",
      srcname = "List_null",
      type = { vars = {TV.a}, args = {list(TV.a)}, result = bool },
      mayraise = false,
      discardable = true,
    },
    {
      name = "List.unsafeHead",
      srcname = "List_unsafeHead",
      type = { vars = {TV.a}, args = {list(TV.a)}, result = TV.a },
      mayraise = false, -- partial
      discardable = true,
    },
    {
      name = "List.unsafeTail",
      srcname = "List_unsafeTail",
      type = { vars = {TV.a}, args = {list(TV.a)}, result = list(TV.a) },
      mayraise = false, -- partial
      discardable = true,
    },
    {
      name = "Ref.ref",
      srcname = "Ref_ref",
      type = { vars = {TV.a}, args = {TV.a}, result = ref(TV.a) },
      mayraise = false,
      discardable = true,
    },
    {
      name = "Ref.=",
      srcname = "Ref_EQUAL",
      type = { vars = {TV.a}, args = {ref(TV.a), ref(TV.a)}, result = bool },
      mayraise = false,
      discardable = true,
    },
    {
      name = "Ref.:=",
      srcname = "Ref_set",
      type = { vars = {TV.a}, args = {ref(TV.a), TV.a}, result = unit },
      mayraise = false,
      discardable = false,
    },
    {
      name = "Ref.!",
      srcname = "Ref_read",
      type = { vars = {TV.a}, args = {ref(TV.a)}, result = TV.a },
      mayraise = false,
      discardable = true,
    },
    {
      name = "Bool.=",
      srcname = "Bool_EQUAL",
      type = Compare(bool),
      mayraise = false,
      discardable = true,
    },
    {
      name = "Bool.not",
      srcname = "Bool_not",
      type = HomoUnary(bool),
      mayraise = false,
      discardable = true,
    },
    {
      name = "Int{i}.=",
      srcname = "Int_EQUAL",
      type = Compare(intA),
      mayraise = false,
      discardable = true,
    },
    {
      name = "Int{i}.+",
      srcname = "Int_PLUS",
      type = HomoBinary(intA),
      mayraise = true,
      discardable = false,
    },
    {
      name = "Int{i}.+.wrapping",
      srcname = "Int_PLUS_wrapping",
      type = HomoBinary(intA),
      mayraise = false,
      discardable = true,
    },
    {
      name = "Int{i}.-",
      srcname = "Int_MINUS",
      type = HomoBinary(intA),
      mayraise = true,
      discardable = false,
    },
    {
      name = "Int{i}.-.wrapping",
      srcname = "Int_MINUS_wrapping",
      type = HomoBinary(intA),
      mayraise = false,
      discardable = true,
    },
    {
      name = "Int{i}.*",
      srcname = "Int_TIMES",
      type = HomoBinary(intA),
      mayraise = true,
      discardable = false,
    },
    {
      name = "Int{i}.*.wrapping",
      srcname = "Int_TIMES_wrapping",
      type = HomoBinary(intA),
      mayraise = false,
      discardable = true,
    },
    {
      name = "Int{i}.div",
      srcname = "Int_div",
      type = HomoBinary(intA),
      mayraise = true,
      discardable = false,
    },
    {
      name = "Int{i}.div.unchecked",
      srcname = "Int_div_unchecked",
      type = HomoBinary(intA),
      mayraise = false,
      discardable = true,
    },
    {
      name = "Int{i}.mod",
      srcname = "Int_mod",
      type = HomoBinary(intA),
      mayraise = true,
      discardable = false,
    },
    {
      name = "Int{i}.mod.unchecked",
      srcname = "Int_mod_unchecked",
      type = HomoBinary(intA),
      mayraise = false,
      discardable = true,
    },
    {
      name = "Int{i}.quot",
      srcname = "Int_quot",
      type = HomoBinary(intA),
      mayraise = true,
      discardable = false,
    },
    {
      name = "Int{i}.quot.unchecked",
      srcname = "Int_quot_unchecked",
      type = HomoBinary(intA),
      mayraise = false,
      discardable = true,
    },
    {
      name = "Int{i}.rem",
      srcname = "Int_rem",
      type = HomoBinary(intA),
      mayraise = true,
      discardable = false,
    },
    {
      name = "Int{i}.rem.unchecked",
      srcname = "Int_rem_unchecked",
      type = HomoBinary(intA),
      mayraise = false,
      discardable = true,
    },
    {
      name = "Int{i}.~",
      srcname = "Int_TILDE",
      type = HomoUnary(intA),
      mayraise = true,
      discardable = false,
    },
    {
      name = "Int{i}.~.unchecked",
      srcname = "Int_TILDE_unchecked",
      type = HomoUnary(intA),
      mayraise = false,
      discardable = true,
    },
    {
      name = "Int{i}.~.wrapping",
      srcname = "Int_TILDE_wrapping",
      type = HomoUnary(intA),
      mayraise = false,
      discardable = true,
    },
    {
      name = "Int{i}.abs",
      srcname = "Int_abs",
      type = HomoUnary(intA),
      mayraise = true,
      discardable = false,
    },
    {
      name = "Int{i}.<",
      srcname = "Int_LT",
      type = Compare(intA),
      mayraise = false,
      discardable = true,
    },
    {
      name = "Int{i}.<=",
      srcname = "Int_LE",
      type = Compare(intA),
      mayraise = false,
      discardable = true,
    },
    {
      name = "Int{i}.>",
      srcname = "Int_GT",
      type = Compare(intA),
      mayraise = false,
      discardable = true,
    },
    {
      name = "Int{i}.>=",
      srcname = "Int_GE",
      type = Compare(intA),
      mayraise = false,
      discardable = true,
    },
    {
      name = "Int{i}.toInt{i}.unchecked",
      srcname = "Int_toInt_unchecked",
      type = { vars = {}, args = {intA}, result = intB },
      mayraise = false,
      discardable = true,
    },
    {
      name = "Word{w}.=",
      srcname = "Word_EQUAL",
      type = Compare(wordA),
      mayraise = false,
      discardable = true,
    },
    {
      name = "Word{w}.+",
      srcname = "Word_PLUS",
      type = HomoBinary(wordA),
      mayraise = false,
      discardable = true,
    },
    {
      name = "Word{w}.-",
      srcname = "Word_MINUS",
      type = HomoBinary(wordA),
      mayraise = false,
      discardable = true,
    },
    {
      name = "Word{w}.*",
      srcname = "Word_TIMES",
      type = HomoBinary(wordA),
      mayraise = false,
      discardable = true,
    },
    {
      name = "Word{w}.~",
      srcname = "Word_TILDE",
      type = HomoUnary(wordA),
      mayraise = false,
      discardable = true,
    },
    {
      name = "Word{w}.div",
      srcname = "Word_div",
      type = HomoBinary(wordA),
      mayraise = true,
      discardable = false,
    },
    {
      name = "Word{w}.mod",
      srcname = "Word_mod",
      type = HomoBinary(wordA),
      mayraise = true,
      discardable = false,
    },
    {
      name = "Word{w}.div.unchecked",
      srcname = "Word_div_unchecked",
      type = HomoBinary(wordA),
      mayraise = false,
      discardable = true,
    },
    {
      name = "Word{w}.mod.unchecked",
      srcname = "Word_mod_unchecked",
      type = HomoBinary(wordA),
      mayraise = false,
      discardable = true,
    },
    {
      name = "Word{w}.<",
      srcname = "Word_LT",
      type = Compare(wordA),
      mayraise = false,
      discardable = true,
    },
    {
      name = "Word{w}.<=",
      srcname = "Word_LE",
      type = Compare(wordA),
      mayraise = false,
      discardable = true,
    },
    {
      name = "Word{w}.>",
      srcname = "Word_GT",
      type = Compare(wordA),
      mayraise = false,
      discardable = true,
    },
    {
      name = "Word{w}.>=",
      srcname = "Word_GE",
      type = Compare(wordA),
      mayraise = false,
      discardable = true,
    },
    {
      name = "Word{w}.notb",
      srcname = "Word_notb",
      type = HomoUnary(wordA),
      mayraise = false,
      discardable = true,
    },
    {
      name = "Word{w}.andb",
      srcname = "Word_andb",
      type = HomoBinary(wordA),
      mayraise = false,
      discardable = true,
    },
    {
      name = "Word{w}.orb",
      srcname = "Word_orb",
      type = HomoBinary(wordA),
      mayraise = false,
      discardable = true,
    },
    {
      name = "Word{w}.xorb",
      srcname = "Word_xorb",
      type = HomoBinary(wordA),
      mayraise = false,
      discardable = true,
    },
    { -- the amount must be less than wordSize
      name = "Word{w}.<<.unchecked{.w}",
      srcname = "Word_LSHIFT_unchecked",
      type = { vars = {}, args = {wordA, wordB}, result = wordA },
      mayraise = false,
      discardable = true,
    },
    { -- the amount must be less than wordSize
      name = "Word{w}.>>.unchecked{.w}",
      srcname = "Word_RSHIFT_unchecked",
      type = { vars = {}, args = {wordA, wordB}, result = wordA },
      mayraise = false,
      discardable = true,
    },
    {
      name = "Real.+",
      srcname = "Real_PLUS",
      type = HomoBinary(real),
      mayraise = false,
      discardable = true,
    },
    {
      name = "Real.-",
      srcname = "Real_MINUS",
      type = HomoBinary(real),
      mayraise = false,
      discardable = true,
    },
    {
      name = "Real.*",
      srcname = "Real_TIMES",
      type = HomoBinary(real),
      mayraise = false,
      discardable = true,
    },
    {
      name = "Real./",
      srcname = "Real_DIVIDE",
      type = HomoBinary(real),
      mayraise = false,
      discardable = true,
    },
    {
      name = "Real.~",
      srcname = "Real_TILDE",
      type = HomoUnary(real),
      mayraise = false,
      discardable = true,
    },
    {
      name = "Real.<",
      srcname = "Real_LT",
      type = Compare(real),
      mayraise = false,
      discardable = true,
    },
    {
      name = "Real.<=",
      srcname = "Real_LE",
      type = Compare(real),
      mayraise = false,
      discardable = true,
    },
    {
      name = "Real.>",
      srcname = "Real_GT",
      type = Compare(real),
      mayraise = false,
      discardable = true,
    },
    {
      name = "Real.>=",
      srcname = "Real_GE",
      type = Compare(real),
      mayraise = false,
      discardable = true,
    },
    {
      name = "Char.=",
      srcname = "Char_EQUAL",
      type = Compare(char),
      mayraise = false,
      discardable = true,
    },
    {
      name = "Char.<",
      srcname = "Char_LT",
      type = Compare(char),
      mayraise = false,
      discardable = true,
    },
    {
      name = "Char.<=",
      srcname = "Char_LE",
      type = Compare(char),
      mayraise = false,
      discardable = true,
    },
    {
      name = "Char.>",
      srcname = "Char_GT",
      type = Compare(char),
      mayraise = false,
      discardable = true,
    },
    {
      name = "Char.>=",
      srcname = "Char_GE",
      type = Compare(char),
      mayraise = false,
      discardable = true,
    },
    {
      name = "Char.ord{.i}",
      srcname = "Char_ord",
      type = { vars = {}, args = {char}, result = intA },
      mayraise = false,
      discardable = true,
    },
    {
      name = "Char.chr.unchecked{.i}",
      srcname = "Char_chr_unchecked",
      type = { vars = {}, args = {intA}, result = char },
      mayraise = false,
      discardable = true,
    },
    {
      name = "Char16.=",
      srcname = "Char16_EQUAL",
      type = Compare(char16),
      mayraise = false,
      discardable = true,
    },
    {
      name = "Char16.<",
      srcname = "Char16_LT",
      type = Compare(char16),
      mayraise = false,
      discardable = true,
    },
    {
      name = "Char16.<=",
      srcname = "Char16_LE",
      type = Compare(char16),
      mayraise = false,
      discardable = true,
    },
    {
      name = "Char16.>",
      srcname = "Char16_GT",
      type = Compare(char16),
      mayraise = false,
      discardable = true,
    },
    {
      name = "Char16.>=",
      srcname = "Char16_GE",
      type = Compare(char16),
      mayraise = false,
      discardable = true,
    },
    {
      name = "Char16.ord{.i}",
      srcname = "Char16_ord",
      type = { vars = {}, args = {char16}, result = intA },
      mayraise = false,
      discardable = true,
    },
    {
      name = "Char16.chr.unchecked{.i}",
      srcname = "Char16_chr_unchecked",
      type = { vars = {}, args = {intA}, result = char16 },
      mayraise = false,
      discardable = true,
    },
    {
      name = "String.=",
      srcname = "String_EQUAL",
      type = Compare(string),
      mayraise = false,
      discardable = true,
    },
    {
      name = "String.<",
      srcname = "String_LT",
      type = Compare(string),
      mayraise = false,
      discardable = true,
    },
    {
      name = "String.<=",
      srcname = "String_LE",
      type = Compare(string),
      mayraise = false,
      discardable = true,
    },
    {
      name = "String.>",
      srcname = "String_GT",
      type = Compare(string),
      mayraise = false,
      discardable = true,
    },
    {
      name = "String.>=",
      srcname = "String_GE",
      type = Compare(string),
      mayraise = false,
      discardable = true,
    },
    {
      name = "String.^",
      srcname = "String_HAT",
      type = HomoBinary(string),
      mayraise = true, -- Size
      discardable = false, -- Size
    },
    {
      name = "String.size{.i}",
      srcname = "String_size",
      type = { vars = {}, args = {string}, result = intA },
      mayraise = false,
      discardable = true,
    },
    {
      name = "String.str",
      srcname = "String_str",
      type = { vars = {}, args = {char}, result = string },
      mayraise = false,
      discardable = true,
    },
    {
      name = "String16.=",
      srcname = "String16_EQUAL",
      type = Compare(string16),
      mayraise = false,
      discardable = true,
    },
    {
      name = "String16.<",
      srcname = "String16_LT",
      type = Compare(string16),
      mayraise = false,
      discardable = true,
    },
    {
      name = "String16.<=",
      srcname = "String16_LE",
      type = Compare(string16),
      mayraise = false,
      discardable = true,
    },
    {
      name = "String16.>",
      srcname = "String16_GT",
      type = Compare(string16),
      mayraise = false,
      discardable = true,
    },
    {
      name = "String16.>=",
      srcname = "String16_GE",
      type = Compare(string16),
      mayraise = false,
      discardable = true,
    },
    {
      name = "String16.^",
      srcname = "String16_HAT",
      type = HomoBinary(string16),
      mayraise = true, -- Size
      discardable = false, -- Size
    },
    {
      name = "String16.size{.i}",
      srcname = "String16_size",
      type = { vars = {}, args = {string16}, result = intA },
      mayraise = false,
      discardable = true,
    },
    {
      name = "String16.str",
      srcname = "String16_str",
      type = { vars = {}, args = {char16}, result = string16 },
      mayraise = false,
      discardable = true,
    },
    {
      name = "IntInf.andb",
      srcname = "IntInf_andb",
      type = HomoBinary(intInf),
      mayraise = false,
      discardable = true,
    },
    {
      name = "IntInf.orb",
      srcname = "IntInf_orb",
      type = HomoBinary(intInf),
      mayraise = false,
      discardable = true,
    },
    {
      name = "IntInf.xorb",
      srcname = "IntInf_xorb",
      type = HomoBinary(intInf),
      mayraise = false,
      discardable = true,
    },
    {
      name = "IntInf.notb",
      srcname = "IntInf_notb",
      type = HomoUnary(intInf),
      mayraise = false,
      discardable = true,
    },
    {
      name = "Vector.length{.i}",
      srcname = "Vector_length",
      type = { vars = {TV.a}, args = {vector(TV.a)}, result = intA },
      mayraise = false,
      discardable = true,
    },
    {
      name = "Vector.unsafeFromListRevN{.i}",
      srcname = "Vector_unsafeFromListRevN",
      type = { vars = {TV.a}, args = {intA, list(TV.a)}, result = vector(TV.a) },
      mayraise = false,
      discardable = true,
    },
    {
      name = "Array.=",
      srcname = "Array_EQUAL",
      type = { vars = {TV.a}, args = {array(TV.a), array(TV.a)}, result = bool },
      mayraise = false,
      discardable = true,
    },
    {
      name = "Array.length{.i}",
      srcname = "Array_length",
      type = { vars = {TV.a}, args = {array(TV.a)}, result = intA },
      mayraise = false,
      discardable = true,
    },
    {
      name = "Unsafe.cast",
      srcname = "Unsafe_cast",
      type = { vars = {TV.a, TV.b}, args = {TV.a}, result = TV.b },
      mayraise = false,
      discardable = true,
    },
    {
      name = "Unsafe.Vector.sub{.i}",
      srcname = "Unsafe_Vector_sub",
      type = { vars = {TV.a}, args = {vector(TV.a), intA}, result = TV.a },
      mayraise = false,
      discardable = true,
    },
    {
      name = "Unsafe.Array.sub{.i}",
      srcname = "Unsafe_Array_sub",
      type = { vars = {TV.a}, args = {array(TV.a), intA}, result = TV.a },
      mayraise = false,
      discardable = true,
    },
    {
      name = "Unsafe.Array.update{.i}",
      srcname = "Unsafe_Array_update",
      type = { vars = {TV.a}, args = {array(TV.a), intA, TV.a}, result = unit },
      mayraise = false,
      discardable = false,
    },
    {
      name = "Exception.instanceof",
      srcname = "Exception_instanceof",
      type = { vars = {}, args = {exn, exntag}, result = bool },
      mayraise = false,
      discardable = true,
    },
    {
      name = "DelimCont.newPromptTag",
      srcname = "DelimCont_newPromptTag",
      type = { vars = {TV.a}, args = {}, result = promptTag(TV.a) },
      mayraise = false,
      discardable = true,
    },
    {
      name = "DelimCont.pushPrompt",
      srcname = "DelimCont_pushPrompt",
      type = { vars = {TV.a}, args = {promptTag(TV.a), function1(TV.a, unit)}, result = TV.a },
      mayraise = true,
      discardable = false,
    },
    {
      name = "DelimCont.withSubCont",
      srcname = "DelimCont_withSubCont",
      type = { vars = {TV.a, TV.b}, args = {promptTag(TV.b), function1(TV.b, subcont(TV.a, TV.b))}, result = TV.a },
      mayraise = true,
      discardable = false,
    },
    {
      name = "DelimCont.pushSubCont",
      srcname = "DelimCont_pushSubCont",
      type = { vars = {TV.a, TV.b}, args = {subcont(TV.a, TV.b), function1(TV.a, unit)}, result = TV.b },
      mayraise = true,
      discardable = false,
    },
    {
      name = "assumeDiscardable",
      srcname = "assumeDiscardable",
      type = { vars = {TV.a, TV.b}, args = {function1(TV.b, TV.a), TV.a}, result = TV.b },
      mayraise = true,
      discardable = true,
    },
    {
      name = "unreachable",
      srcname = "unreachable",
      type = { vars = {TV.a}, args = {}, result = TV.a },
      mayraise = true,
      discardable = false,
    },

    --
    -- Lua backend
    --
    {
      name = "Lua.sub",
      srcname = "Lua_sub",
      type = HomoBinary(LuaValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.set",
      srcname = "Lua_set",
      type = { vars = {}, args = {LuaValue, LuaValue, LuaValue}, result = unit },
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.isNil",
      srcname = "Lua_isNil",
      type = { vars = {}, args = {LuaValue}, result = bool },
      mayraise = false,
      discardable = true,
    },
    {
      name = "Lua.==",
      srcname = "Lua_EQUAL",
      type = Compare(LuaValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.~=",
      srcname = "Lua_NOTEQUAL",
      type = Compare(LuaValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.<",
      srcname = "Lua_LT",
      type = Compare(LuaValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.<=",
      srcname = "Lua_LE",
      type = Compare(LuaValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.>",
      srcname = "Lua_GT",
      type = Compare(LuaValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.>=",
      srcname = "Lua_GE",
      type = Compare(LuaValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.+",
      srcname = "Lua_PLUS",
      type = HomoBinary(LuaValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.-",
      srcname = "Lua_MINUS",
      type = HomoBinary(LuaValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.*",
      srcname = "Lua_TIMES",
      type = HomoBinary(LuaValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua./",
      srcname = "Lua_DIVIDE",
      type = HomoBinary(LuaValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.//",
      srcname = "Lua_INTDIV",
      type = HomoBinary(LuaValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.%",
      srcname = "Lua_MOD",
      type = HomoBinary(LuaValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.pow",
      srcname = "Lua_pow",
      type = HomoBinary(LuaValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.negate",
      srcname = "Lua_negate",
      type = HomoUnary(LuaValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.andb",
      srcname = "Lua_andb",
      type = HomoBinary(LuaValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.orb",
      srcname = "Lua_orb",
      type = HomoBinary(LuaValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.xorb",
      srcname = "Lua_xorb",
      type = HomoBinary(LuaValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.notb",
      srcname = "Lua_notb",
      type = HomoUnary(LuaValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.<<",
      srcname = "Lua_LSHIFT",
      type = HomoBinary(LuaValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.>>",
      srcname = "Lua_RSHIFT",
      type = HomoBinary(LuaValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.concat",
      srcname = "Lua_concat",
      type = HomoBinary(LuaValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.length",
      srcname = "Lua_length",
      type = HomoUnary(LuaValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.isFalsy",
      srcname = "Lua_isFalsy",
      type = { vars = {}, args = {LuaValue}, result = bool },
      mayraise = false,
      discardable = true,
    },
    {
      name = "Lua.call",
      srcname = "Lua_call",
      type = { vars = {}, args = {LuaValue, vector(LuaValue)}, result = vector(LuaValue) },
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.call1",
      srcname = "Lua_call1",
      type = { vars = {}, args = {LuaValue, vector(LuaValue)}, result = LuaValue },
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.call2",
      srcname = "Lua_call2",
      type = { vars = {}, args = {LuaValue, vector(LuaValue)}, result = pair(LuaValue, LuaValue) },
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.call3",
      srcname = "Lua_call3",
      type = { vars = {}, args = {LuaValue, vector(LuaValue)}, result = tuple{LuaValue, LuaValue, LuaValue} },
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.method",
      srcname = "Lua_method",
      type = { vars = {}, args = {LuaValue, string, vector(LuaValue)}, result = vector(LuaValue) },
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.global",
      srcname = "Lua_global",
      type = { vars = {}, args = {string}, result = LuaValue },
      mayraise = false, -- assume that __index is not set on the global table
      discardable = true, -- assume that __index is not set on the global table
    },
    {
      name = "Lua.setGlobal",
      srcname = "Lua_setGlobal",
      type = { vars = {}, args = {string, LuaValue}, result = unit },
      mayraise = false, -- assume that __newindex is not set on the global table
      discardable = false,
    },
    {
      name = "Lua.newTable",
      srcname = "Lua_newTable",
      type = { vars = {}, args = {}, result = LuaValue },
      mayraise = false,
      discardable = true,
    },

    --
    -- JavaScript backend
    --
    {
      name = "JavaScript.sub",
      srcname = "JavaScript_sub",
      type = HomoBinary(JSValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "JavaScript.set",
      srcname = "JavaScript_set",
      type = { vars = {}, args = {JSValue, JSValue, JSValue}, result = unit },
      mayraise = true,
      discardable = false,
    },
    {
      name = "JavaScript.===",
      srcname = "JavaScript_EQUAL",
      type = Compare(JSValue),
      mayraise = false,
      discardable = true,
    },
    {
      name = "JavaScript.!==",
      srcname = "JavaScript_NOTEQUAL",
      type = Compare(JSValue),
      mayraise = false,
      discardable = true,
    },
    {
      name = "JavaScript.<",
      srcname = "JavaScript_LT",
      type = Compare(JSValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "JavaScript.<=",
      srcname = "JavaScript_LE",
      type = Compare(JSValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "JavaScript.>",
      srcname = "JavaScript_GT",
      type = Compare(JSValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "JavaScript.>=",
      srcname = "JavaScript_GE",
      type = Compare(JSValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "JavaScript.+",
      srcname = "JavaScript_PLUS",
      type = HomoBinary(JSValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "JavaScript.-",
      srcname = "JavaScript_MINUS",
      type = HomoBinary(JSValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "JavaScript.*",
      srcname = "JavaScript_TIMES",
      type = HomoBinary(JSValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "JavaScript./",
      srcname = "JavaScript_DIVIDE",
      type = HomoBinary(JSValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "JavaScript.%",
      srcname = "JavaScript_MOD",
      type = HomoBinary(JSValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "JavaScript.negate",
      srcname = "JavaScript_negate",
      type = HomoUnary(JSValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "JavaScript.andb",
      srcname = "JavaScript_andb",
      type = HomoBinary(JSValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "JavaScript.orb",
      srcname = "JavaScript_orb",
      type = HomoBinary(JSValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "JavaScript.xorb",
      srcname = "JavaScript_xorb",
      type = HomoBinary(JSValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "JavaScript.notb",
      srcname = "JavaScript_notb",
      type = HomoUnary(JSValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "JavaScript.<<",
      srcname = "JavaScript_LSHIFT",
      type = HomoBinary(JSValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "JavaScript.>>",
      srcname = "JavaScript_RSHIFT",
      type = HomoBinary(JSValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "JavaScript.>>>",
      srcname = "JavaScript_URSHIFT",
      type = HomoBinary(JSValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "JavaScript.**",
      srcname = "JavaScript_EXP",
      type = HomoBinary(JSValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "JavaScript.isFalsy",
      srcname = "JavaScript_isFalsy",
      type = { vars = {}, args = {JSValue}, result = bool },
      mayraise = false,
      discardable = true,
    },
    {
      name = "JavaScript.typeof",
      srcname = "JavaScript_typeof",
      type = { vars = {}, args = {JSValue}, result = string16 },
      mayraise = false,
      discardable = true,
    },
    {
      name = "JavaScript.global",
      srcname = "JavaScript_global",
      type = { vars = {}, args = {string16}, result = JSValue },
      mayraise = true,
      discardable = false,
    },
    {
      name = "JavaScript.setGlobal",
      srcname = "JavaScript_setGlobal",
      type = { vars = {}, args = {string16, JSValue}, result = unit },
      mayraise = true,
      discardable = false,
    },
    {
      name = "JavaScript.call",
      srcname = "JavaScript_call",
      type = { vars = {}, args = {JSValue, vector(JSValue)}, result = JSValue },
      mayraise = true,
      discardable = false,
    },
    {
      name = "JavaScript.method",
      srcname = "JavaScript_method",
      type = { vars = {}, args = {JSValue, string16, vector(JSValue)}, result = JSValue },
      mayraise = true,
      discardable = false,
    },
    {
      name = "JavaScript.new",
      srcname = "JavaScript_new",
      type = { vars = {}, args = {JSValue, vector(JSValue)}, result = JSValue },
      mayraise = true,
      discardable = false,
    },
  }
end

local f = io.open(arg[1] or "primitives.sml", "w")

f:write[[
(* -*- mode: sml; mode: read-only -*- *)
(* This file was generated by primitives.lua *)
structure Primitives = struct
datatype int_width = INT | I32 | I54 | I64 | INT_INF
datatype word_width = WORD | W32 | W64
]]

for i, p in ipairs(PRIMITIVES) do
  local head
  if i == 1 then
    head = "datatype PrimOp = "
  else
    head = "                | "
  end
  if p.name:find("{%.?%a}") then
    local params = {}
    for m in p.name:gmatch("{%.?%a}") do
      if m == "{i}" or m == "{.i}" then
        table.insert(params, "int_width")
      elseif m == "{w}" or m == "{.w}" then
        table.insert(params, "word_width")
      else
        error("invalid parameter specification in " .. p.name)
      end
    end
    f:write(string.format("%s%s of %s (* %s *)\n", head, p.srcname, table.concat(params, " * "), p.name))
  else
    f:write(string.format("%s%s (* %s *)\n", head, p.srcname, p.name))
  end
end

function cartesian_product(xs, i)
  i = i or 1
  if #xs < i then
    return {{}}
  else
    local ys = cartesian_product(xs, i + 1)
    local results = {}
    for _, v in ipairs(xs[i]) do
      for j, w in ipairs(ys) do
        local t = {v}
        table.move(w, 1, #w, 2, t)
        table.insert(results, t)
      end
    end
    return results
  end
end

for i, p in ipairs(PRIMITIVES) do
  local params = {}
  for m in p.name:gmatch("{%.?%a}") do
    if m == "{i}" or m == "{.i}" then
      table.insert(params, {{"INT", "", ""}, {"I32", "32", ".i32"}, {"I54", "54", ".i54"}, {"I64", "64", ".i64"}, {"INT_INF", "Inf", ".intInf"}})
    elseif m == "{w}" or m == "{.w}" then
      table.insert(params, {{"WORD", "", ""}, {"W32", "32", ".w32"}, {"W64", "64", ".w64"}})
    else
      error("invalid parameter specification in " .. p.name)
    end
  end
  for j, pr in ipairs(cartesian_product(params)) do
    local head
    if i == 1 and j == 1 then
      head = "fun toString "
    else
      head = "  | toString "
    end
    if #pr == 0 then
      f:write(string.format("%s%s = %q\n", head, p.srcname, p.name))
    else
      local a = {}
      for k, t in ipairs(pr) do
        a[k] = t[1]
      end
      local aa = table.concat(a, ", ")
      if #a > 1 then
        aa = "(" .. aa .. ")"
      end
      local name = {}
      local k = 1
      for pre, m, post in p.name:gmatch("([^{}]*)({%.?%a})([^{}]*)") do
        table.insert(name, pre)
        if m == "{i}" or m == "{w}" then
          table.insert(name, pr[k][2])
        elseif m == "{.i}" or m == "{.w}" then
          table.insert(name, pr[k][3])
        else
          error("invalid parameter specification in " .. p.name)
        end
        k = k + 1
        table.insert(name, post)
      end
      f:write(string.format("%s(%s %s) = %q\n", head, p.srcname, aa, table.concat(name)))
    end
  end
end

for i, p in ipairs(PRIMITIVES) do
  local params = {}
  for m in p.name:gmatch("{%.?%a}") do
    if m == "{i}" or m == "{.i}" then
      table.insert(params, {{"INT", "", ""}, {"I32", "32", ".i32"}, {"I54", "54", ".i54"}, {"I64", "64", ".i64"}, {"INT_INF", "Inf", ".intInf"}})
    elseif m == "{w}" or m == "{.w}" then
      table.insert(params, {{"WORD", "", ""}, {"W32", "32", ".w32"}, {"W64", "64", ".w64"}})
    else
      error("invalid parameter specification in " .. p.name)
    end
  end
  for j, pr in ipairs(cartesian_product(params)) do
    local head
    if i == 1 and j == 1 then
      head = "fun fromString "
    else
      head = "  | fromString "
    end
    if #pr == 0 then
      f:write(string.format("%s%q = SOME %s\n", head, p.name, p.srcname))
    else
      local a = {}
      for k, t in ipairs(pr) do
        a[k] = t[1]
      end
      local aa = table.concat(a, ", ")
      if #a > 1 then
        aa = "(" .. aa .. ")"
      end
      local name = {}
      local k = 1
      for pre, m, post in p.name:gmatch("([^{}]*)({%.?%a})([^{}]*)") do
        table.insert(name, pre)
        if m == "{i}" or m == "{w}" then
          table.insert(name, pr[k][2])
        elseif m == "{.i}" or m == "{.w}" then
          table.insert(name, pr[k][3])
        else
          error("invalid parameter specification in " .. p.name)
        end
        k = k + 1
        table.insert(name, post)
      end
      f:write(string.format("%s%q = SOME (%s %s)\n", head, table.concat(name), p.srcname, aa))
    end
  end
end
f:write("  | fromString _ = NONE\n")

f:write [[
fun mayRaise (Int_PLUS INT_INF) = false
  | mayRaise (Int_MINUS INT_INF) = false
  | mayRaise (Int_TIMES INT_INF) = false
  | mayRaise (Int_TILDE INT_INF) = false
  | mayRaise (Int_abs INT_INF) = false
]]
for i, p in ipairs(PRIMITIVES) do
  local head = "  | mayRaise "
  if p.name:find("{%.?%a}") then
    f:write(string.format("%s(%s _) = %s\n", head, p.srcname, tostring(p.mayraise)))
  else
    f:write(string.format("%s%s = %s\n", head, p.srcname, tostring(p.mayraise)))
  end
end

f:write [[
fun isDiscardable (Int_PLUS INT_INF) = true
  | isDiscardable (Int_MINUS INT_INF) = true
  | isDiscardable (Int_TIMES INT_INF) = true
  | isDiscardable (Int_TILDE INT_INF) = true
  | isDiscardable (Int_abs INT_INF) = true
]]
for i, p in ipairs(PRIMITIVES) do
  local head = "  | isDiscardable "
  if p.name:find("{%.?%a}") then
    f:write(string.format("%s(%s _) = %s\n", head, p.srcname, tostring(p.discardable)))
  else
    f:write(string.format("%s%s = %s\n", head, p.srcname, tostring(p.discardable)))
  end
end

f:write [[
fun fixIntWord { int, word }
  = let fun fixInt INT = int
          | fixInt i = i
        fun fixWord WORD = word
          | fixWord w = w
]]
local initial = true
for i, p in ipairs(PRIMITIVES) do
  if p.name:find("{%.?%a}") then
    local head
    if initial then
      head = "    in fn "
      initial = false
    else
      head = "        | "
    end
    local params = {}
    local args = {}
    local j = 1
    for m in p.name:gmatch("{%.?%a}") do
      if m == "{i}" or m == "{.i}" then
        table.insert(params, "a" .. j)
        table.insert(args, "fixInt a" .. j)
      elseif m == "{w}" or m == "{.w}" then
        table.insert(params, "a" .. j)
        table.insert(args, "fixWord a" .. j)
      else
        error("invalid parameter specification in " .. p.name)
      end
      j = j + 1
    end
    local pp = table.concat(params, ", ")
    if #params > 1 then
      pp = "(" .. pp .. ")"
    end
    f:write(string.format("%s%s %s => %s (%s)\n", head, p.srcname, pp, p.srcname, table.concat(args, ", ")))
  end
end
f:write [[
        | p => p
    end
]]

f:write[[
end;

functor TypeOfPrimitives (type ty
                          type tv
                          type constraint
                          val tyVarA : tv
                          val tyVarB : tv
                          val tyVarC : tv
                          val tyVarD : tv
                          val tyVarEqA : tv
                          val tyA : ty
                          val tyB : ty
                          val tyC : ty
                          val tyD : ty
                          val tyEqA : ty
                          val unit : ty
                          val bool : ty
                          val int : ty
                          val int32 : ty
                          val int54 : ty
                          val int64 : ty
                          val intInf : ty
                          val word : ty
                          val word32 : ty
                          val word64 : ty
                          val real : ty
                          val char : ty
                          val char16 : ty
                          val string : ty
                          val string16 : ty
                          val exn : ty
                          val exntag : ty
                          val LuaValue : ty
                          val JavaScriptValue : ty
                          val refOf : ty -> ty
                          val listOf : ty -> ty
                          val vectorOf : ty -> ty
                          val arrayOf : ty -> ty
                          val pairOf : ty * ty -> ty
                          val tupleOf : ty list -> ty
                          val function1Of : ty * ty -> ty
                          val function2Of : ty * ty * ty -> ty
                          val function3Of : ty * ty * ty * ty -> ty
                          val promptTagOf : ty -> ty
                          val subcontOf : ty * ty -> ty
                          val IsEqType : constraint
                         ) : sig
                               val typeOf : Primitives.PrimOp -> { vars : (tv * constraint list) list, args : ty vector, result : ty }
                             end = struct
]]

for i, p in ipairs(PRIMITIVES) do
  local head
  if i == 1 then
    head = "fun typeOf "
  else
    head = "  | typeOf "
  end
  local typeVariables = {}
  for _, t in ipairs(p.type.vars) do
    local ct = ""
    if t[3] then
      ct = t[3]
    end
    table.insert(typeVariables, "(" .. t[2] .. ", [" .. ct .. "])")
  end
  local params = {}
  for m in p.name:gmatch("{%.?%a}") do
    if m == "{i}" or m == "{.i}" then
      table.insert(params, {{"int", "INT", "int"}, {"int", "I32", "int32"}, {"int", "I54", "int54"}, {"int", "I64", "int64"}, {"int", "INT_INF", "intInf"}})
    elseif m == "{w}" or m == "{.w}" then
      table.insert(params, {{"word", "WORD", "word"}, {"word", "W32", "word32"}, {"word", "W64", "word64"}})
    else
      error("invalid parameter specification in " .. p.name)
    end
  end
  for j, pr in ipairs(cartesian_product(params)) do
    if #pr == 0 then
      local argTypes = {}
      for _, t in ipairs(p.type.args) do
        table.insert(argTypes, t[1])
      end
      local resultType = p.type.result[1]
      f:write(string.format("%sPrimitives.%s = { vars = [%s], args = vector [%s], result = %s }\n", head, p.srcname, table.concat(typeVariables, ", "), table.concat(argTypes, ", "), resultType))
    else
      local actualInt, actualIntB, actualWord, actualWordB
      for _, u in ipairs(pr) do
        if u[1] == "int" then
          if actualInt == nil then
            actualInt = u[3]
          else
            actualIntB = u[3]
          end
        elseif u[1] == "word" then
          if actualWord == nil then
            actualWord = u[3]
          else
            actualWordB = u[3]
          end
        else
          error("invalid")
        end
      end
      local argTypes = {}
      for _, t in ipairs(p.type.args) do
        if t[1] == "intA" then
          table.insert(argTypes, actualInt)
        elseif t[1] == "intB" then
          table.insert(argTypes, actualIntB)
        elseif t[1] == "wordA" then
          table.insert(argTypes, actualWord)
        elseif t[1] == "wordB" then
          table.insert(argTypes, actualWordB)
        else
          table.insert(argTypes, t[1])
        end
      end
      local resultType
      if p.type.result[1] == "intA" then
        resultType = actualInt
      elseif p.type.result[1] == "intB" then
        resultType = actualIntB
      elseif p.type.result[1] == "wordA" then
        resultType = actualWord
      elseif p.type.result[1] == "wordB" then
        resultType = actualWordB
      else
        resultType = p.type.result[1]
      end
      local a = {}
      for k, t in ipairs(pr) do
        a[k] = "Primitives." .. t[2]
      end
      local aa = table.concat(a, ", ")
      if #a > 1 then
        aa = "(" .. aa .. ")"
      end
      f:write(string.format("%s(Primitives.%s %s) = { vars = [%s], args = vector [%s], result = %s }\n", head, p.srcname, aa, table.concat(typeVariables, ", "), table.concat(argTypes, ", "), resultType))
    end
  end
end

f:write[[
end;
]]

f:close()
