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
  local char7 = {"char7"}
  local char16 = {"char16"}
  local char32 = {"char32"}
  local string = {"string"}
  local string7 = {"string7"}
  local string16 = {"string16"}
  local string32 = {"string32"}
  local intInf = {"intInf"}
  local exn = {"exn"}
  local exntag = {"exntag"}
  local prim_effect = {"prim_effect"}
  local LuaValue = {"LuaValue"}
  local JSValue = {"JavaScriptValue"}
  local ref = function(payloadTy) return {string_format("refOf (%s)", payloadTy[1])} end
  local list = function(elemTy) return {string_format("listOf (%s)", elemTy[1])} end
  local vector = function(elemTy) return {string_format("vectorOf (%s)", elemTy[1])} end
  local array = function(elemTy) return {string_format("arrayOf (%s)", elemTy[1])} end
  local pair = function(ty1, ty2) return {string_format("pairOf (%s, %s)", ty1[1], ty2[1])} end
  local tuple = function(types)
    local t = {}
    for _, v in ipairs(types) do
      table.insert(t, v[1])
    end
    return {string_format("tupleOf [%s]", table.concat(t, ", "))}
  end
  local function1 = function(arg1Ty, resultTy) return {string_format("function1Of (%s, %s)", arg1Ty[1], resultTy[1])} end
  local function2 = function(arg1Ty, arg2Ty, resultTy) return {string_format("function2Of (%s, %s, %s)", arg1Ty[1], arg2Ty[1], resultTy[1])} end
  local function3 = function(arg1Ty, arg2Ty, arg3Ty, resultTy) return {string_format("function3Of (%s, %s, %s, %s)", arg1Ty[1], arg2Ty[1], arg3Ty[1], resultTy[1])} end
  local promptTag = function(ty) return {string_format("promptTagOf (%s)", ty[1])} end
  local subcont = function(ty1, ty2) return {string_format("subcontOf (%s, %s)", ty1[1], ty2[1])} end
  local function Binary(a, b)
    return function(result)
      return { vars = {}, args = {a, b}, results = {result} }
    end
  end
  local function HomoUnary(a)
    return { vars = {}, args = {a}, results = {a} }
  end
  local function HomoUnaryE(a)
    return { vars = {}, args = {a, prim_effect}, results = {a} }
  end
  local function HomoBinary(a)
    return { vars = {}, args = {a, a}, results = {a} }
  end
  local function HomoBinaryE(a)
    return { vars = {}, args = {a, a, prim_effect}, results = {a} }
  end
  local function Compare(a)
    return { vars = {}, args = {a, a}, results = {bool} }
  end
  local function CompareE(a)
    return { vars = {}, args = {a, a, prim_effect}, results = {bool} }
  end

  PRIMITIVES = {
    {
      name = "=",
      srcname = "EQUAL",
      type = { vars = {TV.eqA}, args = {TV.eqA, TV.eqA}, results = {bool} },
      mayraise = false,
      discardable = true,
    },
    {
      name = "mkFn2",
      srcname = "mkFn2",
      type = { vars = {TV.a, TV.b, TV.c}, args = {function1(pair(TV.a, TV.b), TV.c)}, results = {function2(TV.a, TV.b, TV.c)} },
      mayraise = true,
      discardable = false,
    },
    {
      name = "mkFn3",
      srcname = "mkFn3",
      type = { vars = {TV.a, TV.b, TV.c, TV.d}, args = {function1(tuple{TV.a, TV.b, TV.c}, TV.d)}, results = {function3(TV.a, TV.b, TV.c, TV.d)} },
      mayraise = true,
      discardable = false,
    },
    {
      name = "call2",
      srcname = "call2",
      type = { vars = {TV.a, TV.b, TV.c}, args = {function2(TV.a, TV.b, TV.c), TV.a, TV.b}, results = {TV.c} },
      mayraise = true,
      discardable = false,
    },
    {
      name = "call3",
      srcname = "call3",
      type = { vars = {TV.a, TV.b, TV.c, TV.d}, args = {function3(TV.a, TV.b, TV.c, TV.d), TV.a, TV.b, TV.c}, results = {TV.d} },
      mayraise = true,
      discardable = false,
    },
    {
      name = "List.::",
      srcname = "List_cons",
      type = { vars = {TV.a}, args = {TV.a, list(TV.a)}, results = {list(TV.a)} },
      mayraise = false,
      discardable = true,
    },
    {
      name = "List.null",
      srcname = "List_null",
      type = { vars = {TV.a}, args = {list(TV.a)}, results = {bool} },
      mayraise = false,
      discardable = true,
    },
    {
      name = "List.unsafeHead",
      srcname = "List_unsafeHead",
      type = { vars = {TV.a}, args = {list(TV.a)}, results = {TV.a} },
      mayraise = false, -- partial
      discardable = true,
    },
    {
      name = "List.unsafeTail",
      srcname = "List_unsafeTail",
      type = { vars = {TV.a}, args = {list(TV.a)}, results = {list(TV.a)} },
      mayraise = false, -- partial
      discardable = true,
    },
    {
      name = "General.exnName",
      srcname = "General_exnName",
      type = { vars = {}, args = {exn}, results = {string} },
      mayraise = false,
      discardable = true,
    },
    {
      name = "Ref.ref",
      srcname = "Ref_ref",
      type = { vars = {TV.a}, args = {TV.a}, results = {ref(TV.a)} },
      mayraise = false,
      discardable = true,
    },
    {
      name = "Ref.=",
      srcname = "Ref_EQUAL",
      type = { vars = {TV.a}, args = {ref(TV.a), ref(TV.a)}, results = {bool} },
      mayraise = false,
      discardable = true,
    },
    {
      name = "Ref.:=",
      srcname = "Ref_set",
      type = { vars = {TV.a}, args = {ref(TV.a), TV.a}, results = {} },
      mayraise = false,
      discardable = false,
    },
    {
      name = "Ref.!",
      srcname = "Ref_read",
      type = { vars = {TV.a}, args = {ref(TV.a)}, results = {TV.a} },
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
      type = { vars = {}, args = {intA}, results = {intB} },
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
      type = { vars = {}, args = {wordA, wordB}, results = {wordA} },
      mayraise = false,
      discardable = true,
    },
    { -- the amount must be less than wordSize
      name = "Word{w}.>>.unchecked{.w}",
      srcname = "Word_RSHIFT_unchecked",
      type = { vars = {}, args = {wordA, wordB}, results = {wordA} },
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
      name = "Real.abs",
      srcname = "Real_abs",
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
      type = { vars = {}, args = {char}, results = {intA} },
      mayraise = false,
      discardable = true,
    },
    {
      name = "Char.chr.unchecked{.i}",
      srcname = "Char_chr_unchecked",
      type = { vars = {}, args = {intA}, results = {char} },
      mayraise = false,
      discardable = true,
    },
    {
      name = "Char7.=",
      srcname = "Char7_EQUAL",
      type = Compare(char7),
      mayraise = false,
      discardable = true,
    },
    {
      name = "Char7.<",
      srcname = "Char7_LT",
      type = Compare(char7),
      mayraise = false,
      discardable = true,
    },
    {
      name = "Char7.<=",
      srcname = "Char7_LE",
      type = Compare(char7),
      mayraise = false,
      discardable = true,
    },
    {
      name = "Char7.>",
      srcname = "Char7_GT",
      type = Compare(char7),
      mayraise = false,
      discardable = true,
    },
    {
      name = "Char7.>=",
      srcname = "Char7_GE",
      type = Compare(char7),
      mayraise = false,
      discardable = true,
    },
    {
      name = "Char7.ord{.i}",
      srcname = "Char7_ord",
      type = { vars = {}, args = {char7}, results = {intA} },
      mayraise = false,
      discardable = true,
    },
    {
      name = "Char7.chr.unchecked{.i}",
      srcname = "Char7_chr_unchecked",
      type = { vars = {}, args = {intA}, results = {char7} },
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
      type = { vars = {}, args = {char16}, results = {intA} },
      mayraise = false,
      discardable = true,
    },
    {
      name = "Char16.chr.unchecked{.i}",
      srcname = "Char16_chr_unchecked",
      type = { vars = {}, args = {intA}, results = {char16} },
      mayraise = false,
      discardable = true,
    },
    {
      name = "Char32.=",
      srcname = "Char32_EQUAL",
      type = Compare(char32),
      mayraise = false,
      discardable = true,
    },
    {
      name = "Char32.<",
      srcname = "Char32_LT",
      type = Compare(char32),
      mayraise = false,
      discardable = true,
    },
    {
      name = "Char32.<=",
      srcname = "Char32_LE",
      type = Compare(char32),
      mayraise = false,
      discardable = true,
    },
    {
      name = "Char32.>",
      srcname = "Char32_GT",
      type = Compare(char32),
      mayraise = false,
      discardable = true,
    },
    {
      name = "Char32.>=",
      srcname = "Char32_GE",
      type = Compare(char32),
      mayraise = false,
      discardable = true,
    },
    {
      name = "Char32.ord{.i}",
      srcname = "Char32_ord",
      type = { vars = {}, args = {char32}, results = {intA} },
      mayraise = false,
      discardable = true,
    },
    {
      name = "Char32.chr.unchecked{.i}",
      srcname = "Char32_chr_unchecked",
      type = { vars = {}, args = {intA}, results = {char32} },
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
      mayraise = false, -- ignore Size
      discardable = true, -- ignore Size
    },
    {
      name = "String.size{.i}",
      srcname = "String_size",
      type = { vars = {}, args = {string}, results = {intA} },
      mayraise = false,
      discardable = true,
    },
    {
      name = "String.str",
      srcname = "String_str",
      type = { vars = {}, args = {char}, results = {string} },
      mayraise = false,
      discardable = true,
    },
    {
      name = "String.concat",
      srcname = "String_concat",
      type = { vars = {}, args = {list(string)}, results = {string} },
      mayraise = false,
      discardable = true,
    },
    {
      name = "String.implode",
      srcname = "String_implode",
      type = { vars = {}, args = {list(char)}, results = {string} },
      mayraise = false,
      discardable = true,
    },
    {
      name = "String7.=",
      srcname = "String7_EQUAL",
      type = Compare(string7),
      mayraise = false,
      discardable = true,
    },
    {
      name = "String7.<",
      srcname = "String7_LT",
      type = Compare(string7),
      mayraise = false,
      discardable = true,
    },
    {
      name = "String7.<=",
      srcname = "String7_LE",
      type = Compare(string7),
      mayraise = false,
      discardable = true,
    },
    {
      name = "String7.>",
      srcname = "String7_GT",
      type = Compare(string7),
      mayraise = false,
      discardable = true,
    },
    {
      name = "String7.>=",
      srcname = "String7_GE",
      type = Compare(string7),
      mayraise = false,
      discardable = true,
    },
    {
      name = "String7.^",
      srcname = "String7_HAT",
      type = HomoBinary(string7),
      mayraise = false, -- ignore Size
      discardable = true, -- ignore Size
    },
    {
      name = "String7.size{.i}",
      srcname = "String7_size",
      type = { vars = {}, args = {string7}, results = {intA} },
      mayraise = false,
      discardable = true,
    },
    {
      name = "String7.str",
      srcname = "String7_str",
      type = { vars = {}, args = {char7}, results = {string7} },
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
      mayraise = false, -- ignore Size
      discardable = true, -- ignore Size
    },
    {
      name = "String16.size{.i}",
      srcname = "String16_size",
      type = { vars = {}, args = {string16}, results = {intA} },
      mayraise = false,
      discardable = true,
    },
    {
      name = "String16.str",
      srcname = "String16_str",
      type = { vars = {}, args = {char16}, results = {string16} },
      mayraise = false,
      discardable = true,
    },
    {
      name = "String32.=",
      srcname = "String32_EQUAL",
      type = Compare(string32),
      mayraise = false,
      discardable = true,
    },
    {
      name = "String32.<",
      srcname = "String32_LT",
      type = Compare(string32),
      mayraise = false,
      discardable = true,
    },
    {
      name = "String32.<=",
      srcname = "String32_LE",
      type = Compare(string32),
      mayraise = false,
      discardable = true,
    },
    {
      name = "String32.>",
      srcname = "String32_GT",
      type = Compare(string32),
      mayraise = false,
      discardable = true,
    },
    {
      name = "String32.>=",
      srcname = "String32_GE",
      type = Compare(string32),
      mayraise = false,
      discardable = true,
    },
    {
      name = "String32.^",
      srcname = "String32_HAT",
      type = HomoBinary(string32),
      mayraise = false, -- ignore Size
      discardable = true, -- ignore Size
    },
    {
      name = "String32.size{.i}",
      srcname = "String32_size",
      type = { vars = {}, args = {string32}, results = {intA} },
      mayraise = false,
      discardable = true,
    },
    {
      name = "String32.str",
      srcname = "String32_str",
      type = { vars = {}, args = {char32}, results = {string32} },
      mayraise = false,
      discardable = true,
    },
    {
      name = "String32.concat",
      srcname = "String32_concat",
      type = { vars = {}, args = {list(string32)}, results = {string32} },
      mayraise = false,
      discardable = true,
    },
    {
      name = "String32.implode",
      srcname = "String32_implode",
      type = { vars = {}, args = {list(char32)}, results = {string32} },
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
      type = { vars = {TV.a}, args = {vector(TV.a)}, results = {intA} },
      mayraise = false,
      discardable = true,
    },
    {
      name = "Vector.fromList",
      srcname = "Vector_fromList",
      type = { vars = {TV.a}, args = {list(TV.a)}, results = {vector(TV.a)} },
      mayraise = false,
      discardable = true,
    },
    {
      name = "Vector.concat",
      srcname = "Vector_concat",
      type = { vars = {TV.a}, args = {list(vector(TV.a))}, results = {vector(TV.a)} },
      mayraise = false,
      discardable = true,
    },
    {
      name = "Vector.unsafeFromListRevN{.i}",
      srcname = "Vector_unsafeFromListRevN",
      type = { vars = {TV.a}, args = {intA, list(TV.a)}, results = {vector(TV.a)} },
      mayraise = false,
      discardable = true,
    },
    {
      name = "Array.=",
      srcname = "Array_EQUAL",
      type = { vars = {TV.a}, args = {array(TV.a), array(TV.a)}, results = {bool} },
      mayraise = false,
      discardable = true,
    },
    {
      name = "Array.length{.i}",
      srcname = "Array_length",
      type = { vars = {TV.a}, args = {array(TV.a)}, results = {intA} },
      mayraise = false,
      discardable = true,
    },
    {
      name = "Array.fromList",
      srcname = "Array_fromList",
      type = { vars = {TV.a}, args = {list(TV.a)}, results = {array(TV.a)} },
      mayraise = false,
      discardable = true,
    },
    {
      name = "Array.array{.i}",
      srcname = "Array_array",
      type = { vars = {TV.a}, args = {intA, TV.a}, results = {array(TV.a)} },
      mayraise = true,
      discardable = false, -- There is a special rule in CSyntax.isDiscardable
    },
    {
      name = "Unsafe.cast",
      srcname = "Unsafe_cast",
      type = { vars = {TV.a, TV.b}, args = {TV.a}, results = {TV.b} },
      mayraise = false,
      discardable = true,
    },
    {
      name = "Unsafe.Vector.sub{.i}",
      srcname = "Unsafe_Vector_sub",
      type = { vars = {TV.a}, args = {vector(TV.a), intA}, results = {TV.a} },
      mayraise = false,
      discardable = true,
    },
    {
      name = "Unsafe.Array.sub{.i}",
      srcname = "Unsafe_Array_sub",
      type = { vars = {TV.a}, args = {array(TV.a), intA}, results = {TV.a} },
      mayraise = false,
      discardable = true,
    },
    {
      name = "Unsafe.Array.update{.i}",
      srcname = "Unsafe_Array_update",
      type = { vars = {TV.a}, args = {array(TV.a), intA, TV.a}, results = {} },
      mayraise = false,
      discardable = false,
    },
    {
      name = "Exception.instanceof",
      srcname = "Exception_instanceof",
      type = { vars = {}, args = {exn, exntag}, results = {bool} },
      mayraise = false,
      discardable = true,
    },
    {
      name = "DelimCont.newPromptTag",
      srcname = "DelimCont_newPromptTag",
      type = { vars = {TV.a}, args = {}, results = {promptTag(TV.a)} },
      mayraise = false,
      discardable = true,
    },
    --[[
    {
      name = "DelimCont.pushPrompt",
      srcname = "DelimCont_pushPrompt",
      type = { vars = {TV.a}, args = {promptTag(TV.a), function1(unit, TV.a)}, results = {TV.a} },
      mayraise = true,
      discardable = false,
    },
    {
      name = "DelimCont.withSubCont",
      srcname = "DelimCont_withSubCont",
      type = { vars = {TV.a, TV.b}, args = {promptTag(TV.b), function1(subcont(TV.a, TV.b), TV.b)}, results = {TV.a} },
      mayraise = true,
      discardable = false,
    },
    {
      name = "DelimCont.pushSubCont",
      srcname = "DelimCont_pushSubCont",
      type = { vars = {TV.a, TV.b}, args = {subcont(TV.a, TV.b), function1(unit, TV.a)}, results = {TV.b} },
      mayraise = true,
      discardable = false,
    },
    ]]
    {
      name = "assumeDiscardable",
      srcname = "assumeDiscardable",
      type = { vars = {TV.a, TV.b}, args = {function1(TV.a, TV.b), TV.a}, results = {TV.b} },
      mayraise = true,
      discardable = true,
    },
    {
      name = "unreachable",
      srcname = "unreachable",
      type = { vars = {TV.a}, args = {}, results = {TV.a} },
      mayraise = true,
      discardable = false,
    },

    --
    -- Lua backend
    --
    {
      name = "Lua.sub",
      srcname = "Lua_sub",
      type = HomoBinaryE(LuaValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.set",
      srcname = "Lua_set",
      type = { vars = {}, args = {LuaValue, LuaValue, LuaValue}, results = {} },
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.isNil",
      srcname = "Lua_isNil",
      type = { vars = {}, args = {LuaValue}, results = {bool} },
      mayraise = false,
      discardable = true,
    },
    {
      name = "Lua.==",
      srcname = "Lua_EQUAL",
      type = CompareE(LuaValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.~=",
      srcname = "Lua_NOTEQUAL",
      type = CompareE(LuaValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.<",
      srcname = "Lua_LT",
      type = CompareE(LuaValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.<=",
      srcname = "Lua_LE",
      type = CompareE(LuaValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.>",
      srcname = "Lua_GT",
      type = CompareE(LuaValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.>=",
      srcname = "Lua_GE",
      type = CompareE(LuaValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.+",
      srcname = "Lua_PLUS",
      type = HomoBinaryE(LuaValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.-",
      srcname = "Lua_MINUS",
      type = HomoBinaryE(LuaValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.*",
      srcname = "Lua_TIMES",
      type = HomoBinaryE(LuaValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua./",
      srcname = "Lua_DIVIDE",
      type = HomoBinaryE(LuaValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.//",
      srcname = "Lua_INTDIV",
      type = HomoBinaryE(LuaValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.%",
      srcname = "Lua_MOD",
      type = HomoBinaryE(LuaValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.pow",
      srcname = "Lua_pow",
      type = HomoBinaryE(LuaValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.negate",
      srcname = "Lua_negate",
      type = HomoUnaryE(LuaValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.andb",
      srcname = "Lua_andb",
      type = HomoBinaryE(LuaValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.orb",
      srcname = "Lua_orb",
      type = HomoBinaryE(LuaValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.xorb",
      srcname = "Lua_xorb",
      type = HomoBinaryE(LuaValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.notb",
      srcname = "Lua_notb",
      type = HomoUnaryE(LuaValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.<<",
      srcname = "Lua_LSHIFT",
      type = HomoBinaryE(LuaValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.>>",
      srcname = "Lua_RSHIFT",
      type = HomoBinaryE(LuaValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.concat",
      srcname = "Lua_concat",
      type = HomoBinaryE(LuaValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.length",
      srcname = "Lua_length",
      type = HomoUnaryE(LuaValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.isFalsy",
      srcname = "Lua_isFalsy",
      type = { vars = {}, args = {LuaValue}, results = {bool} },
      mayraise = false,
      discardable = true,
    },
    {
      name = "Lua.call",
      srcname = "Lua_call",
      type = { vars = {}, args = {LuaValue, vector(LuaValue), prim_effect}, results = {vector(LuaValue)} },
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.call1",
      srcname = "Lua_call1",
      type = { vars = {}, args = {LuaValue, vector(LuaValue), prim_effect}, results = {LuaValue} },
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.call2",
      srcname = "Lua_call2",
      type = { vars = {}, args = {LuaValue, vector(LuaValue), prim_effect}, results = {LuaValue, LuaValue} },
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.call3",
      srcname = "Lua_call3",
      type = { vars = {}, args = {LuaValue, vector(LuaValue), prim_effect}, results = {LuaValue, LuaValue, LuaValue} },
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.call4",
      srcname = "Lua_call4",
      type = { vars = {}, args = {LuaValue, vector(LuaValue), prim_effect}, results = {LuaValue, LuaValue, LuaValue, LuaValue} },
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.call5",
      srcname = "Lua_call5",
      type = { vars = {}, args = {LuaValue, vector(LuaValue), prim_effect}, results = {LuaValue, LuaValue, LuaValue, LuaValue, LuaValue} },
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.call6",
      srcname = "Lua_call6",
      type = { vars = {}, args = {LuaValue, vector(LuaValue), prim_effect}, results = {LuaValue, LuaValue, LuaValue, LuaValue, LuaValue, LuaValue} },
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.call7",
      srcname = "Lua_call7",
      type = { vars = {}, args = {LuaValue, vector(LuaValue), prim_effect}, results = {LuaValue, LuaValue, LuaValue, LuaValue, LuaValue, LuaValue, LuaValue} },
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.call8",
      srcname = "Lua_call8",
      type = { vars = {}, args = {LuaValue, vector(LuaValue), prim_effect}, results = {LuaValue, LuaValue, LuaValue, LuaValue, LuaValue, LuaValue, LuaValue, LuaValue} },
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.call9",
      srcname = "Lua_call9",
      type = { vars = {}, args = {LuaValue, vector(LuaValue), prim_effect}, results = {LuaValue, LuaValue, LuaValue, LuaValue, LuaValue, LuaValue, LuaValue, LuaValue, LuaValue} },
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.method",
      srcname = "Lua_method",
      type = { vars = {}, args = {LuaValue, string, vector(LuaValue), prim_effect}, results = {vector(LuaValue)} },
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.method1",
      srcname = "Lua_method1",
      type = { vars = {}, args = {LuaValue, string, vector(LuaValue), prim_effect}, results = {LuaValue} },
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.method2",
      srcname = "Lua_method2",
      type = { vars = {}, args = {LuaValue, string, vector(LuaValue), prim_effect}, results = {LuaValue, LuaValue} },
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.method3",
      srcname = "Lua_method3",
      type = { vars = {}, args = {LuaValue, string, vector(LuaValue), prim_effect}, results = {LuaValue, LuaValue, LuaValue} },
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.method4",
      srcname = "Lua_method4",
      type = { vars = {}, args = {LuaValue, string, vector(LuaValue), prim_effect}, results = {LuaValue, LuaValue, LuaValue, LuaValue} },
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.method5",
      srcname = "Lua_method5",
      type = { vars = {}, args = {LuaValue, string, vector(LuaValue), prim_effect}, results = {LuaValue, LuaValue, LuaValue, LuaValue, LuaValue} },
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.method6",
      srcname = "Lua_method6",
      type = { vars = {}, args = {LuaValue, string, vector(LuaValue), prim_effect}, results = {LuaValue, LuaValue, LuaValue, LuaValue, LuaValue, LuaValue,} },
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.method7",
      srcname = "Lua_method7",
      type = { vars = {}, args = {LuaValue, string, vector(LuaValue), prim_effect}, results = {LuaValue, LuaValue, LuaValue, LuaValue, LuaValue, LuaValue, LuaValue} },
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.method8",
      srcname = "Lua_method8",
      type = { vars = {}, args = {LuaValue, string, vector(LuaValue), prim_effect}, results = {LuaValue, LuaValue, LuaValue, LuaValue, LuaValue, LuaValue, LuaValue, LuaValue} },
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.method9",
      srcname = "Lua_method9",
      type = { vars = {}, args = {LuaValue, string, vector(LuaValue), prim_effect}, results = {LuaValue, LuaValue, LuaValue, LuaValue, LuaValue, LuaValue, LuaValue, LuaValue, LuaValue} },
      mayraise = true,
      discardable = false,
    },
    {
      name = "Lua.global",
      srcname = "Lua_global",
      type = { vars = {}, args = {string}, results = {LuaValue} },
      mayraise = false, -- assume that __index is not set on the global table
      discardable = true, -- assume that __index is not set on the global table
    },
    {
      name = "Lua.setGlobal",
      srcname = "Lua_setGlobal",
      type = { vars = {}, args = {string, LuaValue}, results = {} },
      mayraise = false, -- assume that __newindex is not set on the global table
      discardable = false,
    },
    {
      name = "Lua.newTable",
      srcname = "Lua_newTable",
      type = { vars = {}, args = {}, results = {LuaValue} },
      mayraise = false,
      discardable = true,
    },
    {
      name = "Lua.newTableWith",
      srcname = "Lua_newTableWith",
      type = { vars = {}, args = {vector(pair(string, LuaValue))}, results = {LuaValue} },
      mayraise = false,
      discardable = true,
    },
    {
      name = "Lua.newTableWithMetatable",
      srcname = "Lua_newTableWithMetatable",
      type = { vars = {}, args = {vector(pair(string, LuaValue)), LuaValue}, results = {LuaValue} },
      mayraise = false,
      discardable = true,
    },

    --
    -- JavaScript backend
    --
    {
      name = "JavaScript.sub",
      srcname = "JavaScript_sub",
      type = HomoBinaryE(JSValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "JavaScript.set",
      srcname = "JavaScript_set",
      type = { vars = {}, args = {JSValue, JSValue, JSValue}, results = {} },
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
      type = CompareE(JSValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "JavaScript.<=",
      srcname = "JavaScript_LE",
      type = CompareE(JSValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "JavaScript.>",
      srcname = "JavaScript_GT",
      type = CompareE(JSValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "JavaScript.>=",
      srcname = "JavaScript_GE",
      type = CompareE(JSValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "JavaScript.+",
      srcname = "JavaScript_PLUS",
      type = HomoBinaryE(JSValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "JavaScript.-",
      srcname = "JavaScript_MINUS",
      type = HomoBinaryE(JSValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "JavaScript.*",
      srcname = "JavaScript_TIMES",
      type = HomoBinaryE(JSValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "JavaScript./",
      srcname = "JavaScript_DIVIDE",
      type = HomoBinaryE(JSValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "JavaScript.%",
      srcname = "JavaScript_MOD",
      type = HomoBinaryE(JSValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "JavaScript.negate",
      srcname = "JavaScript_negate",
      type = HomoUnaryE(JSValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "JavaScript.andb",
      srcname = "JavaScript_andb",
      type = HomoBinaryE(JSValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "JavaScript.orb",
      srcname = "JavaScript_orb",
      type = HomoBinaryE(JSValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "JavaScript.xorb",
      srcname = "JavaScript_xorb",
      type = HomoBinaryE(JSValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "JavaScript.notb",
      srcname = "JavaScript_notb",
      type = HomoUnaryE(JSValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "JavaScript.<<",
      srcname = "JavaScript_LSHIFT",
      type = HomoBinaryE(JSValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "JavaScript.>>",
      srcname = "JavaScript_RSHIFT",
      type = HomoBinaryE(JSValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "JavaScript.>>>",
      srcname = "JavaScript_URSHIFT",
      type = HomoBinaryE(JSValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "JavaScript.**",
      srcname = "JavaScript_EXP",
      type = HomoBinaryE(JSValue),
      mayraise = true,
      discardable = false,
    },
    {
      name = "JavaScript.isFalsy",
      srcname = "JavaScript_isFalsy",
      type = { vars = {}, args = {JSValue}, results = {bool} },
      mayraise = false,
      discardable = true,
    },
    {
      name = "JavaScript.isNullOrUndefined",
      srcname = "JavaScript_isNullOrUndefined",
      type = { vars = {}, args = {JSValue}, results = {bool} },
      mayraise = false,
      discardable = true,
    },
    {
      name = "JavaScript.typeof",
      srcname = "JavaScript_typeof",
      type = { vars = {}, args = {JSValue}, results = {string16} },
      mayraise = false,
      discardable = true,
    },
    {
      name = "JavaScript.global",
      srcname = "JavaScript_global",
      type = { vars = {}, args = {string16}, results = {JSValue} },
      mayraise = true,
      discardable = true,
    },
    {
      name = "JavaScript.setGlobal",
      srcname = "JavaScript_setGlobal",
      type = { vars = {}, args = {string16, JSValue}, results = {} },
      mayraise = true,
      discardable = false,
    },
    {
      name = "JavaScript.call",
      srcname = "JavaScript_call",
      type = { vars = {}, args = {JSValue, vector(JSValue), prim_effect}, results = {JSValue} },
      mayraise = true,
      discardable = false,
    },
    {
      name = "JavaScript.method",
      srcname = "JavaScript_method",
      type = { vars = {}, args = {JSValue, string16, vector(JSValue), prim_effect}, results = {JSValue} },
      mayraise = true,
      discardable = false,
    },
    {
      name = "JavaScript.new",
      srcname = "JavaScript_new",
      type = { vars = {}, args = {JSValue, vector(JSValue), prim_effect}, results = {JSValue} },
      mayraise = true,
      discardable = false,
    },
    {
      name = "JavaScript.function",
      srcname = "JavaScript_function",
      type = { vars = {}, args = {function1(vector(JSValue), JSValue)}, results = {JSValue} },
      mayraise = false,
      discardable = true,
    },
    {
      name = "JavaScript.encodeUtf8",
      srcname = "JavaScript_encodeUtf8",
      type = { vars = {}, args = {string16}, results = {string} },
      mayraise = true,
      discardable = true,
    },
    {
      name = "JavaScript.decodeUtf8",
      srcname = "JavaScript_decodeUtf8",
      type = { vars = {}, args = {string}, results = {string16} },
      mayraise = true,
      discardable = true,
    },
  }
  --[[
  local PRIMITIVES_cooked = {}
  for i, v in ipairs(PRIMITIVES) do
    table.insert(PRIMITIVES_cooked, v)
    if v.type.args[#v.type.args] == prim_effect then
      local args = {}
      for i = 1, #v.type.args - 1 do
        args[i] = v.type.args[i]
      end
      table.insert(PRIMITIVES_cooked, {
        name = v.name .. "{.e}",
        srcname = v.srcname .. "_E",
        type = { vars = v.type.vars, args = args, results = v.type.results },
        mayraise = v.mayraise,
        discardable = v.discardable,
      })
    end
  end
  PRIMITIVES = PRIMITIVES_cooked
  ]]
end

local f = io.open(arg[1] or "primitives.sml", "w")

f:write[[
(* -*- mode: sml; mode: read-only -*- *)
(* This file was generated by primitives.lua *)
structure Primitives = struct
datatype int_width = INT | I32 | I54 | I64 | INT_INF
datatype word_width = WORD | W32 | W64
datatype prim_effect = PURE | DISCARDABLE | IMPURE
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
fun isDiscardablePE PURE = true
  | isDiscardablePE DISCARDABLE = true
  | isDiscardablePE IMPURE = false
fun isDiscardableWithArgs (Int_PLUS INT_INF, _) = true
  | isDiscardableWithArgs (Int_MINUS INT_INF, _) = true
  | isDiscardableWithArgs (Int_TIMES INT_INF, _) = true
  | isDiscardableWithArgs (Int_TILDE INT_INF, _) = true
  | isDiscardableWithArgs (Int_abs INT_INF, _) = true
]]
for i, p in ipairs(PRIMITIVES) do
  local head = "  | isDiscardableWithArgs "
  local patterns = {}
  local rhs = tostring(p.discardable)
  for _, t in ipairs(p.type.args) do
    if t[1] == "prim_effect" then
      table.insert(patterns, "e")
      rhs = "isDiscardablePE e"
    else
      table.insert(patterns, "_")
    end
  end
  local pattern_text = table.concat(patterns, ", ")
  if p.name:find("{%.?%a}") then
    f:write(string.format("%s(%s _, [%s]) = %s\n", head, p.srcname, pattern_text, rhs))
  else
    f:write(string.format("%s(%s, [%s]) = %s\n", head, p.srcname, pattern_text, rhs))
  end
end
f:write("  | isDiscardableWithArgs _ = false (* should not occur *)\n")

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


for i, p in ipairs(PRIMITIVES) do
  local head
  if i == 1 then
    head = "fun returnArity "
  else
    head = "  | returnArity "
  end
  if p.name:find("{%.?%a}") then
    f:write(string.format("%s(%s _) = %d\n", head, p.srcname, #p.type.results))
  else
    f:write(string.format("%s%s = %d\n", head, p.srcname, #p.type.results))
  end
end

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
                          val char7 : ty
                          val char16 : ty
                          val char32 : ty
                          val string : ty
                          val string7 : ty
                          val string16 : ty
                          val string32 : ty
                          val exn : ty
                          val exntag : ty
                          val LuaValue : ty
                          val JavaScriptValue : ty
                          val prim_effect : ty
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
                          val Unconstrained : constraint
                          val IsEqType : constraint
                         ) : sig
                               val typeOf : Primitives.PrimOp -> { vars : (tv * constraint) list, args : ty vector, results : ty list }
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
    local ct = "Unconstrained"
    if t[3] then
      ct = t[3]
    end
    table.insert(typeVariables, "(" .. t[2] .. ", " .. ct .. ")")
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
      local resultTypes = {}
      for _, t in ipairs(p.type.results) do
        table.insert(resultTypes, t[1])
      end
      f:write(string.format("%sPrimitives.%s = { vars = [%s], args = vector [%s], results = [%s] }\n", head, p.srcname, table.concat(typeVariables, ", "), table.concat(argTypes, ", "), table.concat(resultTypes, ", ")))
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
        elseif u[1] == "prim_effect" then
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
      local resultTypes = {}
      for _, t in ipairs(p.type.results) do
        if t[1] == "intA" then
          table.insert(resultTypes, actualInt)
        elseif t[1] == "intB" then
          table.insert(resultTypes, actualIntB)
        elseif t[1] == "wordA" then
          table.insert(resultTypes, actualWord)
        elseif t[1] == "wordB" then
          table.insert(resultTypes, actualWordB)
        else
          table.insert(resultTypes, t[1])
        end
      end
      local a = {}
      for k, t in ipairs(pr) do
        if t[2] == "_" then
          a[k] = "_"
        else
          a[k] = "Primitives." .. t[2]
        end
      end
      local aa = table.concat(a, ", ")
      if #a > 1 then
        aa = "(" .. aa .. ")"
      end
      f:write(string.format("%s(Primitives.%s %s) = { vars = [%s], args = vector [%s], results = [%s] }\n", head, p.srcname, aa, table.concat(typeVariables, ", "), table.concat(argTypes, ", "), table.concat(resultTypes, ", ")))
    end
  end
end

f:write[[
end;
]]

f:close()
