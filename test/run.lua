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
function normalize_line_ending(s)
  -- CRLF -> LF
  return (string.gsub(s, "\r\n", "\n"))
end
assert(normalize_line_ending("foo\r\nbar\r\n") == "foo\nbar\n")
function compile_and_run(file)
  local compile_succ, output = compile(file)
  if not compile_succ then
    return false, output
  end
  local luafile = file:gsub("%.sml$", ".lua")
  local h = assert(io.popen(string.format("\"%s\" \"%s\"", lua_interpreter, luafile), "r"))
  local actual_output = normalize_line_ending(h:read("a"))
  h:close()
  local expected_output_file = file:gsub("%.sml$", ".stdout")
  local h = assert(io.open(expected_output_file, "r"))
  local expected_output = normalize_line_ending(h:read("a"))
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
function should_run(dir)
  return function(files)
    for _,f in ipairs(files) do
      local file = testdir .. "/" .. dir .. f
      print("Running " .. dir .. f .. "...")
      local succ, output = compile_and_run(file)
      if not succ then
        io.stderr:write(string.format("%s failed to compile with:\n%s", file, output))
        os.exit(1)
      end
    end
  end
end
function should_compile(dir)
  return function(files)
    for _,f in ipairs(files) do
      local file = testdir .. "/" .. dir .. f
      print("Compiling " .. dir .. f .. "...")
      if not compile(file) then
        io.stderr:write(string.format("%s should compile, but it did not!\n", file))
        os.exit(1)
      end
    end
  end
end
function should_not_compile(dir)
  return function(files)
    for _,f in ipairs(files) do
      local file = testdir .. "/" .. dir .. f
      print("Compiling " .. dir .. f .. "...")
      if compile(file) then
        io.stderr:write(string.format("%s should not compile, but it did!\n", file))
        os.exit(1)
      end
    end
  end
end
should_run "should_run/" {
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
  "opaque1.sml",
  "opaque2.sml",
  "abstype.sml",
  "functor1.sml",
  "functor2.sml",
  "functor3.sml",
  "parser_combinator.sml",
  "val_projection.sml",
}
should_compile "should_compile/" {
  "signature_sharing1.sml",
  "signature_sharing2.sml",
  "signature_sharing3.sml",
  "withtype.sml",
  "sharing_structures.sml",
}
should_not_compile "should_not_compile/" {
  "fixity.sml",
  "pat.sml",
  "typevar_unification.sml",
  "typevar_scope.sml",
  "local_datatype_1.sml",
  "local_datatype_2.sml",
  "local_datatype_3.sml",
  "datatype_scope.sml",
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
  "vector.sml",
  "syntax_expression_row_label.sml",
  "syntax_pattern_row_label.sml",
  "syntax_type_expression_row_label.sml",
  "syntax_valbind.sml",
  "syntax_typbind.sml",
  "syntax_datbind.sml",
  "syntax_datbind2.sml",
  "syntax_exbind.sml",
  "opaque1.sml",
  "opaque2.sml",
  "abstype.sml",
  "sharing_structures1.sml",
  "sharing_structures2.sml",
  "functor1.sml",
}
should_run "mlbasis/should_run/" {
  "general.sml",
  "string.sml",
  "word.sml",
  "word_law.sml",
  "vector.sml",
  "overflow.sml",
  "int-inf-fib-hex.sml",
  "int-inf-fib.sml",
  "int-inf-fib-monoid.sml",
}
should_run "lua/should_run/" {
  "nil_in_vector.sml",
}
should_compile "successor_ml/should_compile/" {
  "typealias_in_signature.sml",
  "withtype_in_signature.sml",
  "abstype.sml",
}
should_not_compile "successor_ml/should_not_compile/" {
  "nonexhaustive_bind.sml",
  "val_rec_override.sml",
  "typealias_in_signature.sml",
}
should_run "extension/should_run/" {
  "vector_exp.sml",
  "overload.sml",
}
should_compile "extension/should_compile/" {
  "vector_exp_generalize.sml",
}
