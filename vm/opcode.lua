local OPCODE_MAP = {}
local OPCODE_TABLE = {}
local NEXT_OPCODE = 0
local function opcode(name, operand)
  OPCODE_MAP[name] = NEXT_OPCODE
  OPCODE_TABLE[NEXT_OPCODE] = {name = name, operand = operand}
  NEXT_OPCODE = NEXT_OPCODE + 1
end
local word8 = "word8"
local word16be = "word16be"
local int8 = "int8"
local int16be = "int16be"
opcode("HALT", {})
opcode("RETURN", {})
opcode("CALL", {})
opcode("TAILCALL", {})
opcode("CALL_WITH_TUPLE", {word8}) -- number of arguments
opcode("TAILCALL_WITH_TUPLE", {word8}) -- number of arguments
opcode("JUMP_IF_FALSE_NEAR", {word8}) -- offset
opcode("JUMP_IF_FALSE_FAR", {word16be}) -- offset
opcode("JUMP_NEAR", {int8}) -- offset
opcode("JUMP_FAR", {int16be}) -- offset
opcode("CLOSURE", {word8, word16be}) -- number of free variables, code
opcode("UPDATE_CLOSURE", {word8, word8, word8}) -- target closure, free index in the closure, new value
opcode("RAISE", {})
opcode("ENTER_HANDLE", {word16be}) -- offset
opcode("LEAVE_HANDLE", {word16be}) -- offset
opcode("POP", {})
opcode("POP_EXCEPT_TOP", {word8}) -- number of dropped values
opcode("PUSH_LOCAL", {word8}) -- index relative to base
opcode("PUSH_FREE", {word8}) -- index
opcode("PUSH_UNIT", {})
opcode("PUSH_FALSE", {})
opcode("PUSH_TRUE", {})
opcode("PUSH_NIL", {})
opcode("PUSH_SMALL_INT", {int8})
opcode("PUSH_SMALL_WORD", {word8})
opcode("PUSH_CONST", {word16be})
opcode("PACK_TUPLE", {word8})
opcode("UNPACK_TUPLE", {word8})
opcode("GET_DATA_TAG", {})
opcode("GET_DATA_PAYLOAD", {})
opcode("CONSTRUCT_DATA_WITHOUT_PAYLOAD", {word8})
opcode("CONSTRUCT_DATA_WITH_PAYLOAD", {word8})
opcode("PUSH_PROMPT", {})
opcode("WITH_SUBCONT", {})
opcode("PUSH_SUBCONT", {})
opcode("ABORT", {})
opcode("PRIMCALL0", {word8}) -- nullary primitive call
opcode("PRIMCALL1", {word8}) -- unary primitive call
opcode("PRIMCALL2", {word8}) -- binary primitive call
opcode("PRIMCALL3", {word8}) -- ternary primitive call

local PRIM0_MAP, PRIM0_TABLE, NEXT_PRIM0 = {}, {}, 0
local function primop0(srcname, name)
  PRIM0_MAP[name] = NEXT_PRIM0
  PRIM0_TABLE[NEXT_PRIM0] = {srcname = srcname, name = name}
  NEXT_PRIM0 = NEXT_PRIM0 + 1
end
local PRIM1_MAP, PRIM1_TABLE, NEXT_PRIM1 = {}, {}, 0
local function primop1(srcname, name)
  PRIM1_MAP[name] = NEXT_PRIM1
  PRIM1_TABLE[NEXT_PRIM1] = {srcname = srcname, name = name}
  NEXT_PRIM1 = NEXT_PRIM1 + 1
end
local PRIM2_MAP, PRIM2_TABLE, NEXT_PRIM2 = {}, {}, 0
local function primop2(srcname, name)
  PRIM2_MAP[name] = NEXT_PRIM2
  PRIM2_TABLE[NEXT_PRIM2] = {srcname = srcname, name = name}
  NEXT_PRIM2 = NEXT_PRIM2 + 1
end
local PRIM3_MAP, PRIM3_TABLE, NEXT_PRIM3 = {}, {}, 0
local function primop3(srcname, name)
  PRIM3_MAP[name] = NEXT_PRIM3
  PRIM3_TABLE[NEXT_PRIM3] = {srcname = srcname, name = name}
  NEXT_PRIM3 = NEXT_PRIM3 + 1
end
primop0("NEW_PROMPT_TAG", "DelimCont.newPromptTag")
primop2("EXN_INSTANCEOF", "Exception.instanceof")
primop1("EXN_NEWTAG", "Exception.newTag")
primop0("MATCH_TAG", "Match.tag")
primop0("MATCH", "Match")
primop0("BIND_TAG", "Bind.tag")
primop0("BIND", "Bind")
primop0("OVERFLOW_TAG", "Overflow.tag")
primop0("OVERFLOW", "Overflow")
primop0("DIV_TAG", "Div.tag")
primop0("DIV", "Div")
primop0("SIZE_TAG", "Size.tag")
primop0("SIZE", "Size")
primop0("SUBSCRIPT_TAG", "Subscript.tag")
primop0("SUBSCRIPT", "Subscript")
primop0("FAIL_TAG", "Fail.tag")
primop1("FAIL", "Fail")
primop1("REF_READ", "Ref.!")
primop1("REF_NEW", "Ref.ref")
primop2("REF_EQUAL", "Ref.=")
primop2("REF_SET", "Ref.:=")
primop2("BOOL_EQUAL", "Bool.=")
primop1("BOOL_NOT", "Bool.not")
primop2("INT_EQUAL", "Int.=")
primop2("INT_LT", "Int.<")
primop2("INT_LE", "Int.<=")
primop2("INT_GT", "Int.>")
primop2("INT_GE", "Int.>=")
for _, n in ipairs({"8", "16", "32", "64"}) do
  primop2("INT"..n.."_ADD", "Int"..n..".+")
  primop2("INT"..n.."_SUB", "Int"..n..".-")
  primop2("INT"..n.."_MUL", "Int"..n..".*")
  primop2("INT"..n.."_DIV", "Int"..n..".div")
  primop2("INT"..n.."_MOD", "Int"..n..".mod")
  primop2("INT"..n.."_QUOT", "Int"..n..".quot")
  primop2("INT"..n.."_REM", "Int"..n..".rem")
  primop1("INT"..n.."_NEGATE", "Int"..n..".~")
  primop1("INT"..n.."_ABS", "Int"..n..".abs")
end
primop1("INT_FMT_DEC", "Int.fmt.DEC")
primop2("WORD_EQUAL", "Word.=")
primop2("WORD_LT", "Word.<")
primop2("WORD_LE", "Word.<=")
primop2("WORD_GT", "Word.>")
primop2("WORD_GE", "Word.>=")
for _, n in ipairs({"8", "16", "32", "64"}) do
  primop2("WORD"..n.."_ADD", "Word"..n..".+")
  primop2("WORD"..n.."_SUB", "Word"..n..".-")
  primop2("WORD"..n.."_MUL", "Word"..n..".*")
  primop2("WORD"..n.."_DIV", "Word"..n..".div")
  primop2("WORD"..n.."_MOD", "Word"..n..".mod")
  primop1("WORD"..n.."_NEGATE", "Word"..n..".~")
  primop1("WORD"..n.."_NOTB", "Word"..n..".notb")
  primop1("WORD"..n.."_LSHIFT", "Word"..n..".<<")
  primop1("WORD"..n.."_RSHIFT", "Word"..n..".>>")
  primop1("WORD"..n.."_ARSHIFT", "Word"..n..".~>>")
end
primop2("WORD_ANDB", "Word.andb")
primop2("WORD_ORB", "Word.orb")
primop2("WORD_XORB", "Word.xorb")
primop1("TEXTIO_PRINT", "TextIO.print")

local function translate(content)
  content = content:gsub("OPCODE%.(%a[%w_]*)", function(name)
                           local value = OPCODE_MAP[name]
                           if value == nil then
                             error("undefined opcode: " .. name)
                           end
                           return string.format("0w%d (* %s *)", value, name)
  end)
  content = content:gsub("PRIM0%s*\"([^\"]*)\"", function(name)
                           local value = PRIM0_MAP[name]
                           if value == nil then
                             error("undefined primitive: " .. name)
                           end
                           return string.format("0w%d (* %s/0 *)", value, name)
  end)
  content = content:gsub("PRIM1%s*\"([^\"]*)\"", function(name)
                           local value = PRIM1_MAP[name]
                           if value == nil then
                             error("undefined primitive: " .. name)
                           end
                           return string.format("0w%d (* %s/1 *)", value, name)
  end)
  content = content:gsub("PRIM2%s*\"([^\"]*)\"", function(name)
                           local value = PRIM2_MAP[name]
                           if value == nil then
                             error("undefined primitive: " .. name)
                           end
                           return string.format("0w%d (* %s/2 *)", value, name)
  end)
  content = content:gsub("PRIM3%s*\"([^\"]*)\"", function(name)
                           local value = PRIM3_MAP[name]
                           if value == nil then
                             error("undefined primitive: " .. name)
                           end
                           return string.format("0w%d (* %s/3 *)", value, name)
  end)
  return content
end

if #arg == 0 then
  io.stderr:write[[
Usage:
lua opcode.lua translate input.sml.in output.sml
lua opcode.lua gen-primitives primitives.sml
lua opcode.lua gen-disasm disasm.sml
]]
end
if arg[1] == "translate" then
  assert(#arg == 3)
  local f = assert(io.open(arg[2], "r"))
  local content = f:read("a")
  f:close()
  content = translate(content)
  local out = assert(io.open(arg[3], "wb"))
  out:write("(* This file was auto-generated by opcode.lua. Do not edit by hand! *)(* -*- mode: sml; mode: read-only -*- *)") -- No newline so that the line numbers match
  out:write(content)
  out:close()
elseif arg[1] == "gen-primitives" then
  local out = assert(io.open(arg[2], "wb"))
  out:write([[
(* -*- mode: sml; mode: read-only -*- *)
(* This file was generated by opcode.lua. Do not edit by hand! *)
]])
  local NEXT = {[0] = NEXT_PRIM0, [1] = NEXT_PRIM1, [2] = NEXT_PRIM2}
  local TABLE = {[0] = PRIM0_TABLE, [1] = PRIM1_TABLE, [2] = PRIM2_TABLE}
  for arity = 0, 2 do
    out:write(string.format("structure Prim%d = struct\n", arity))
    local n = NEXT[arity]
    local b = TABLE[arity]
    for i = 0, n - 1 do
      local h = "datatype t = "
      if i ~= 0 then
        h = string.rep(" ", #h - 2) .. "| "
      end
      local t = b[i]
      out:write(string.format("%s%s (* %s *)\n", h, t.srcname, t.name))
    end
    for i = 0, n - 1 do
      local f = "fun toByte %s : Word8.word = 0w%d\n"
      if i ~= 0 then
        f = "  | toByte %s = 0w%d\n"
      end
      local t = b[i]
      out:write(string.format(f, t.srcname, i))
    end
    for i = 0, n - 1 do
      local f = "fun fromByte (0w%d : Word8.word) = SOME %s\n"
      if i ~= 0 then
        f = "  | fromByte 0w%d = SOME %s\n"
      end
      local t = b[i]
      out:write(string.format(f, i, t.srcname))
    end
    out:write("  | fromByte _ = NONE\n")
    for i = 0, n - 1 do
      local h = "fun toString "
      if i ~= 0 then
        h = "  | toString "
      end
      local t = b[i]
      out:write(string.format("%s%s = %q\n", h, t.srcname, t.name))
    end
    for i = 0, n - 1 do
      local h = "fun fromString "
      if i ~= 0 then
        h = "  | fromString "
      end
      local t = b[i]
      out:write(string.format("%s%q = SOME %s\n", h, t.name, t.srcname))
    end
    out:write("  | fromString _ = NONE\n")
    out:write("end;\n")
  end
  out:write([[
structure Prim3 = struct
type t = unit
fun toByte (_ : t) : Word8.word = 0w0
fun fromByte (_ : Word8.word) : t option = NONE
fun toString (_ : t) = ""
fun fromString (_ : string) : t option = NONE
end;
]])
  out:close()
elseif arg[1] == "gen-disasm" then
  local out = assert(io.open(arg[2], "wb"))
  out:write([[
(* -*- mode: sml; mode: read-only -*- *)
(* This file was generated by opcode.lua. Do not edit by hand! *)
structure Disasm = struct
fun disasm (instructions : Word8Vector.vector)
  = let fun loop ip
          = if ip >= Word8Vector.length instructions then
                ()
            else
                let val opcode = Word8Vector.sub (instructions, ip)
                    val ip = ip + 1
                in case opcode of
]])
  for i = 0, 255 do
    local sep = "|"
    if i == 0 then
      sep = " "
    end
    local info = OPCODE_TABLE[i]
    if info == nil then
      out:write(string.format("                     %s 0w%d => (print \"Invalid opcode: %d\\n\"; loop ip)\n", sep, i, i))
    elseif #info.operand == 0 then
      out:write(string.format("                     %s 0w%d => (print \"%s\\n\"; loop ip)\n", sep, i, info.name))
    else
      local init, show = {}, {}
      local ip_offset = 0
      for i, v in ipairs(info.operand) do
        if v == "word8" then
          init[i] = string.format("val x%d = Word8Vector.sub (instructions, ip + %d)", i, i - 1 + ip_offset)
          show[i] = string.format('Word8.fmt StringCvt.DEC x%d', i)
          ip_offset = ip_offset + 1
        elseif v == "word16be" then
          init[i] = string.format("val x%d = 0w256 * Word16.fromLarge (Word8.toLarge (Word8Vector.sub (instructions, ip + %d))) + Word16.fromLarge (Word8.toLarge (Word8Vector.sub (instructions, ip + %d)))", i, i - 1 + ip_offset, i + ip_offset)
          show[i] = string.format('Word16.fmt StringCvt.DEC x%d', i)
          ip_offset = ip_offset + 2
        elseif v == "int8" then
          init[i] = string.format("val x%d = Word8.toIntX (Word8Vector.sub (instructions, ip + %d))", i, i - 1 + ip_offset)
          show[i] = string.format('Int.toString x%d', i)
          ip_offset = ip_offset + 1
        elseif v == "int16be" then
          init[i] = string.format("val x%d = Word16.toIntX (0w256 * Word16.fromLarge (Word8.toLarge (Word8Vector.sub (instructions, ip + %d))) + Word16.fromLarge (Word8.toLarge (Word8Vector.sub (instructions, ip + %d))))", i, i - 1 + ip_offset, i + ip_offset)
          show[i] = string.format('Int.toString x%d', i)
          ip_offset = ip_offset + 2
        else
          error("Unknown operand type")
        end
      end
      out:write(string.format('                     %s 0w%d => let %s in print ("%s " ^ %s ^ "\\n"); loop (ip + %d) end\n', sep, i, table.concat(init, " "), info.name, table.concat(show, ' ^ ", " ^ '), ip_offset))
    end
  end
  out:write([[
                end
    in loop 0
    end
end;
]])
  out:close()
else
  io.stderr:write("Unknown command\n")
end
