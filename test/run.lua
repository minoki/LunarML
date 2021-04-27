#!/usr/bin/env lua
local progname = arg[0]
local testdir
if progname:find("[/\\]") then
  testdir = progname:gsub("[/\\][^/\\]+$", "")
else
  testdir = "."
end
local compiler = arg[1] or "../DamepoML"
local lua_interpreter = arg[2] or "lua"
function compile(file)
  local h = io.popen(string.format("\"%s\" \"%s\" 2>&1", compiler, file), "r")
  local output = h:read("a")
  local succ = h:close()
  assert(type(succ) == "boolean" or succ == nil, "Use Lua 5.2 or later")
  return succ, output
end
function compile_and_run(file)
  local compile_succ, output = compile(file)
  if not compile(file) then
    return false, output
  end
  local luafile = file:gsub("%.sml$", ".lua")
  local h = assert(io.popen(string.format("\"%s\" \"%s\"", lua_interpreter, luafile), "r"))
  local actual_output = h:read("a")
  h:close()
  local expected_output_file = file:gsub("%.sml$", ".stdout")
  local h = assert(io.open(expected_output_file, "r"))
  local expected_output = h:read("a")
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
}
for _,f in ipairs(should_run) do
  local file = testdir .. "/should_run/" .. f
  local succ, output = compile_and_run(file)
  if not succ then
    io.stderr:write(string.format("%s failed to compile with:\n%s", file, output))
    os.exit(1)
  end
end
local should_not_compile = {
  "fixity.sml",
  "generalization.sml",
  "pat.sml",
  "val_rec_override.sml",
}
for _,f in ipairs(should_not_compile) do
  local file = testdir .. "/should_not_compile/" .. f
  if compile(file) then
    io.stderr:write(string.format("%s should not compile, but it did!\n", file))
    os.exit(1)
  end
end
