local options = {
  -- {"allowDoDecls", "bool"},
  {"allowExtendedNumConsts", "bool"},
  {"allowExtendedTextConsts", "bool"},
  -- {"allowLineComments", "bool"},
  -- {"allowOptBar", "bool"},
  -- {"allowOptSemicolon", "bool"},
  -- {"allowOrPats", "bool"},
  -- {"allowRecordPunExps", "bool"},
  {"allowSigWithtype", "bool"},
  {"allowVectorExps", "bool"},
  {"allowVectorPats", "bool"},
  -- {"allowPatternGuards", "bool"},
  -- {"allowConjunctivePats", "bool"},
  -- {"allowNestedMatches", "bool"},
  {"allowRecordExtension", "bool"},
  {"allowRecordUpdate", "bool"},
  {"allowUnicodeEncodedStrings", "bool"},
  {"allowHexFloatConsts", "bool"},
  {"allowValRecTyVars", "bool"},
  {"allowValTyVarsRec", "bool"},
  {"allowFreeTyVarsInDatatypeDec", "bool"},
  {"allowWhereAndType", "bool"},
  {"allowPrim", "bool"},
  {"allowBindEqual", "bool"},
}
local f = assert(io.open(arg[1] or "language-options.sml", "wb"))
f:write[[
structure LanguageOptions = struct
]]
for i, v in ipairs(options) do
  local head
  if i == 1 then
    head = "type options = { "
  else
    head = "               , "
  end
  f:write(string.format("%s%s : %s\n", head, v[1], v[2]))
end
f:write("               }\n")
for i, v in ipairs(options) do
  local name = v[1]
  local Name = string.upper(string.sub(name, 1, 1)) .. string.sub(name, 2)
  local ty = v[2]
  local getFields = {}
  local allFields = {}
  for j, w in ipairs(options) do
    if w[1] == name then
      table.insert(getFields, w[1] .. " = _")
    else
      table.insert(getFields, w[1])
    end
    table.insert(allFields, w[1] .. " = " .. w[1])
  end
  f:write(string.format("fun set%s (%s : %s) ({ %s } : options) = { %s }\n", Name, name, ty, table.concat(getFields, ", "), table.concat(allFields, ", ")))
end
for i, v in ipairs(options) do
  local name = v[1]
  local Name = string.upper(string.sub(name, 1, 1)) .. string.sub(name, 2)
  local ty = v[2]
  local setter = "set" .. Name
  local head
  if i == 1 then
    head = "fun"
  else
    head = "  |"
  end
  f:write(string.format("%s setByName %q = SOME %s\n", head, name, setter))
end
f:write("  | setByName (_ : string) : (bool -> options -> options) option = NONE\n")
f:write[[
val default : options = { allowExtendedNumConsts = true
                        , allowExtendedTextConsts = true
                        , allowSigWithtype = true
                        , allowVectorExps = true
                        , allowVectorPats = true
                        , allowRecordExtension = true
                        , allowRecordUpdate = true
                        , allowUnicodeEncodedStrings = true
                        , allowHexFloatConsts = true
                        , allowValRecTyVars = true
                        , allowValTyVarsRec = true
                        , allowFreeTyVarsInDatatypeDec = true
                        , allowWhereAndType = true
                        , allowPrim = false
                        , allowBindEqual = false
                        }
fun setSuccessorML value = setAllowExtendedNumConsts value
                           o setAllowExtendedTextConsts value
                           o setAllowSigWithtype value
                           o setAllowRecordExtension value
                           o setAllowRecordUpdate value
                           o setAllowValRecTyVars value
                           o setAllowValTyVarsRec (not value)
                           o setAllowFreeTyVarsInDatatypeDec (not value)
                           o setAllowWhereAndType (not value)
end;
]]
