#!/usr/bin/env lua
local progname = arg[0]
local testdir
if progname:find("[/\\]") then
  testdir = progname:gsub("[/\\][^/\\]+$", "")
else
  testdir = "."
end
local compiler = arg[1] or "../lunarml"
local lua_interpreter = arg[2] or "lua"
function compile(file)
  local h = io.popen(string.format("\"%s\" \"%s\" 2>&1", compiler, file), "r")
  local output = h:read("a")
  local succ = h:close()
  assert(type(succ) == "boolean" or succ == nil, "Use Lua 5.2 or later")
  return succ, output
end
function split_join_lines(str)
   -- Avoid unexpected comparison results caused by differing line endings when running on WSL.
   function split(inputstr, sep)
      local lines = {}
      for line, s in string.gmatch(inputstr, sep) do
         table.insert(lines, line)
         if s == "" then return lines end
      end
      return lines
   end
   function join(lines, sep)
      return table.concat(lines, sep)
   end
   return join(split(str, "[^\r\n]+"), "\n")
end
function compile_and_run(file)
  local compile_succ, output = compile(file)
  if not compile_succ then
    return false, output
  end
  local luafile = file:gsub("%.sml$", ".lua")
  local h = assert(io.popen(string.format("\"%s\" \"%s\"", lua_interpreter, luafile), "r"))
  local actual_output = split_join_lines(h:read("a"))
  h:close()
  local expected_output_file = file:gsub("%.sml$", ".stdout")
  local h = assert(io.open(expected_output_file, "r"))
  local expected_output = split_join_lines(h:read("a"))
  h:close()
  if actual_output == expected_output then
    return true
  else
    if expected_output:sub(-1) ~= "\n" then
      expected_output = expected_output .. "[EOF]\n"
    end
    if actual_output:sub(-1) ~= "\n" then
      actual_output = actual_output .. "[EOF]\n"
    end
    io.stderr:write(string.format("%s:\n--- Expected output:\n%s--- Actual output:\n%s---\n", file, expected_output, actual_output))
    return false
  end
end
local should_run = {
  "fizzbuzz.sml",
  "fun.sml",
  "fib_slow.sml",
  "fib_linear.sml",
  "fib_monoid.sml",
  "listrev.sml",
  "prime_trialdiv.sml",
  "prime_eratos.sml",
  "fix_by_datatype.sml",
  "fix_by_ref.sml",
  "prime_as_typevar.sml",
  "open.sml",
  "exception.sml",
  "local_exception.sml",
  "local_datatype.sml",
  "equality.sml",
  "xorshift64.sml",
  "signature1.sml",
  "signature2.sml",
  "signature3.sml",
  "signature4.sml",
  "signature5.sml",
}
for _,f in ipairs(should_run) do
  local file = testdir .. "/should_run/" .. f
  print("Running should_run/" .. f .. "...")
  local succ, output = compile_and_run(file)
  if not succ then
    io.stderr:write(string.format("%s failed to compile with:\n%s", file, output))
    os.exit(1)
  end
end
local should_compile = {
  "signature_sharing1.sml",
  "signature_sharing2.sml",
}
for _,f in ipairs(should_compile) do
  local file = testdir .. "/should_compile/" .. f
  print("Compiling should_compile/" .. f .. "...")
  if not compile(file) then
    io.stderr:write(string.format("%s should compile, but it did not!\n", file))
    os.exit(1)
  end
end
local should_run = {
  "general.sml",
  "string.sml",
  "word.sml",
  "word_law.sml",
}
for _,f in ipairs(should_run) do
  local file = testdir .. "/mlbasis/should_run/" .. f
  print("Running mlbasis/should_run/" .. f .. "...")
  local succ, output = compile_and_run(file)
  if not succ then
    io.stderr:write(string.format("%s failed to compile with:\n%s", file, output))
    os.exit(1)
  end
end
local should_run = {
  "nil_in_vector.sml",
}
for _,f in ipairs(should_run) do
  local file = testdir .. "/lua/should_run/" .. f
  print("Running lua/should_run/" .. f .. "...")
  local succ, output = compile_and_run(file)
  if not succ then
    io.stderr:write(string.format("%s failed to compile with:\n%s", file, output))
    os.exit(1)
  end
end
local should_compile = {
  "typealias_in_signature.sml",
}
for _,f in ipairs(should_compile) do
  local file = testdir .. "/successor_ml/should_compile/" .. f
  print("Compiling successor_ml/should_compile/" .. f .. "...")
  if not compile(file) then
    io.stderr:write(string.format("%s should compile, but it did not!\n", file))
    os.exit(1)
  end
end
local should_not_compile = {
  "fixity.sml",
  "generalization.sml",
  "pat.sml",
  "val_rec_override.sml",
  "typevar_unification.sml",
  "typevar_scope.sml",
  "local_datatype_1.sml",
  "local_datatype_2.sml",
  "local_datatype_3.sml",
  "ref.sml",
  "equality_real.sml",
  "equality_fn.sml",
  "equality_exn.sml",
  "equality_tyvar.sml",
  "nonfree_tyvar.sml",
  "signature1.sml",
  "signature2.sml",
  "signature3.sml",
  "signature4.sml",
  "signature5.sml",
  "signature_sharing1.sml",
  "signature_sharing2.sml",
}
for _,f in ipairs(should_not_compile) do
  local file = testdir .. "/should_not_compile/" .. f
  print("Compiling should_not_compile/" .. f .. "...")
  if compile(file) then
    io.stderr:write(string.format("%s should not compile, but it did!\n", file))
    os.exit(1)
  end
end
local should_not_compile = {
  "typealias_in_signature.sml",
}
for _,f in ipairs(should_not_compile) do
  local file = testdir .. "/successor_ml/should_not_compile/" .. f
  print("Compiling successor_ml/should_not_compile/" .. f .. "...")
  if compile(file) then
    io.stderr:write(string.format("%s should not compile, but it did!\n", file))
    os.exit(1)
  end
end
