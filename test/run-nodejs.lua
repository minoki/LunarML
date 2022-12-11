#!/usr/bin/env lua
local progname = arg[0]
local testdir
if progname:find("[/\\]") then
  testdir = progname:gsub("[/\\][^/\\]+$", "")
else
  testdir = "."
end
local compiler = arg[1] or "../bin/lunarml"
local js_interpreter = arg[2] or "node"
local cps_mode = arg[3] == "cps"
local outext = cps_mode and ".cps.js" or ".js"
function compile(file, outfile)
  local h
  if cps_mode then
    h = io.popen(string.format("\"%s\" compile --js-cps --output \"%s\" \"%s\" 2>&1", compiler, outfile, file), "r")
  else
    h = io.popen(string.format("\"%s\" compile --js --output \"%s\" \"%s\" 2>&1", compiler, outfile, file), "r")
  end
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
  local jsfile = file:gsub("%.sml$", outext):gsub("%.mlb$", outext)
  local compile_succ, output = compile(file, jsfile)
  if not compile_succ then
    return false, output
  end
  local h = assert(io.popen(string.format("\"%s\" \"%s\"", js_interpreter, jsfile), "r"))
  local actual_output = normalize_line_ending(h:read("a"))
  h:close()
  local expected_output_file = file:gsub("%.sml$", ".stdout"):gsub("%.mlb$", ".stdout")
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
      local jsfile = file:gsub("%.sml$", outext):gsub("%.mlb$", outext)
      if not compile(file, jsfile) then
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
      local jsfile = file:gsub("%.sml$", outext):gsub("%.mlb$", outext)
      if compile(file, jsfile) then
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
  -- "xorshift64.sml",
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
  "functor_exception.sml",
  "functor_exception_2.sml",
  "val-rec.sml",
  "datatype-equality.sml",
}
should_compile "should_compile/" {
  "signature_sharing1.sml",
  "signature_sharing2.sml",
  "signature_sharing3.sml",
  "withtype.sml",
  "sharing_structures.sml",
  "generalization1.sml",
  "generalization2.sml",
  "record.sml",
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
  "space_in_qualified_id.sml",
  "duplicate_datbind.sml",
  "duplicate_exbind.sml",
  "duplicate_typbind.sml",
  "duplicate_val.sml",
  "reserved_datbind.sml",
  "reserved_datdesc.sml",
  "reserved_exbind.sml",
  "reserved_exdesc.sml",
  "reserved_exrep.sml",
  "reserved_valdesc.sml",
  "invalid-bound-name1.sml",
  "invalid-bound-name2.sml",
  "flex-record.sml",
  "int-constant.sml",
  "int-constant-2.sml",
  "word-constant.sml",
  "word-constant-2.sml",
}
should_run "mlbasis/should_run/" {
  "general.sml",
  "string.sml",
  "string_fields.sml",
  "word.sml",
  "word_law.sml",
  "word8.sml",
  "word16.sml",
  "word32.sml",
  "word64.sml",
  "word8-law.sml",
  "word16-law.sml",
  "word32-law.sml",
  "word64-law.sml",
  "word_shift.sml",
  "xorshift64-word64.sml",
  "vector.sml",
  "overflow.sml",
  "int8.sml",
  "int16.sml",
  "int32.sml",
  "int64.sml",
  "int-inf.sml",
  "int-inf-fib-hex.sml",
  "int-inf-fib.sml",
  "int-inf-fib-monoid.sml",
  "int-inf-factorial-hex.sml",
  -- "int-inf-factorial.sml",
  "int-inf-wilson.sml",
  "seki-bernoulli.sml",
  "exn-name.sml",
  "real.sml",
  "real-intinf.sml",
  "substring.sml",
  -- "os-path.sml",
}
should_run "successor_ml/should_run/" {
  "record_extension_pattern.sml",
  "record_extension_pattern_2.sml",
  "record_extension_pattern_3.sml",
  "record_extension_expression.sml",
  "record_extension.sml",
  "record_extension_2.sml",
  "record_update.sml",
  "num-underscore.sml",
  "binary.sml",
}
should_compile "successor_ml/should_compile/" {
  "typealias_in_signature.sml",
  "withtype_in_signature.sml",
  "abstype.sml",
  "record_extension_type.sml",
}
should_not_compile "successor_ml/should_not_compile/" {
  "nonexhaustive_bind.sml",
  "val_rec_override.sml",
  "typealias_in_signature.sml",
  "record_extension_pattern_1.sml",
  "record_extension_pattern_2.sml",
  "record_extension_pattern.sml",
  "record_extension_expression.sml",
  "record_extension_type.sml",
  "num-underscore-1.sml",
  "num-underscore-2.sml",
}
should_run "extension/should_run/" {
  "vector_exp.sml",
  "overload.mlb",
  "helloja.sml",
}
should_compile "extension/should_compile/" {
  "vector_exp_generalize.sml",
}
if cps_mode then
  should_run "cps/should_run/" {
    "product.sml",
    "nondet.sml",
    "exception.sml",
  }
end
