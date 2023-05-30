LUA = lua
LUAJIT = luajit
NODE = node

all: bin/lunarml

sources = \
  pluto/token-stream.sig \
  pluto/string-stream.sig \
  pluto/string-stream.sml \
  pluto/parser-combinator.sig \
  pluto/parser-combinator.fun \
  pluto/char-parser.sig \
  pluto/char-parser.fun \
  src/list-util.sml \
  src/vector-util.sml \
  src/numeric.sml \
  src/text.sml \
  src/strongly-connected-components.sml \
  src/language-options.sml \
  src/target-info.sml \
  src/primitives.sml \
  src/syntax.grm.sig \
  src/sourcepos.sml \
  src/tokenizer.sml \
  src/syntax.sml \
  src/syntax.grm.sml \
  src/parser.sml \
  src/postparsing.sml \
  src/typed.sml \
  src/typing.sml \
  src/initialenv.sml \
  src/printer.sml \
  src/fsyntax.sml \
  src/fprinter.sml \
  src/pattern-match.sml \
  src/ftransform.sml \
  src/cps.sml \
  src/lua-syntax.sml \
  src/lua-transform.sml \
  src/codegen-lua.sml \
  src/js-syntax.sml \
  src/js-transform.sml \
  src/codegen-js.sml \
  src/mlb-syntax.sml \
  src/mlb-parser.sml \
  src/driver.sml \
  src/mlb-eval.sml \
  src/command-line-settings.sml \
  src/main.sml

typecheck:
	mlton -stop tc LunarML.mlb

bin/lunarml: LunarML.mlb $(sources)
	mlton -output $@ LunarML.mlb

bin/lunarml.gen2.lua: bin/lunarml LunarML.mlb $(sources)
	bin/lunarml compile -o $@ LunarML.mlb

bin/lunarml.gen2-luajit.lua: bin/lunarml LunarML.mlb $(sources)
	bin/lunarml compile --luajit -o $@ LunarML.mlb

bin/lunarml.gen2.js: bin/lunarml LunarML.mlb $(sources)
	bin/lunarml compile --js-cps -o $@ LunarML.mlb

src/syntax.grm.sml src/syntax.grm.sig: src/syntax.grm
	mlyacc $<

src/language-options.sml: src/language-options.lua
	$(LUA) src/language-options.lua $@

src/primitives.sml: src/primitives.lua
	$(LUA) src/primitives.lua $@ > /dev/null

src/command-line-settings.sml: util/record.lua Makefile
	$(LUA) util/record.lua CommandLineSettings "subcommand,output,outputMode,dump,optimizationLevel,backend,libDir,printTimings" > $@

test: test-lua

test-lua: bin/lunarml
	$(MAKE) -C test VARIANT=lua

test-lua-continuations: bin/lunarml
	$(MAKE) -C test VARIANT=lua-continuations LUA=$(LUA)

test-luajit: bin/lunarml
	$(MAKE) -C test VARIANT=luajit LUAJIT=$(LUAJIT)

test-nodejs: bin/lunarml
	$(MAKE) -C test VARIANT=nodejs NODE=$(NODE)

test-nodejs-cps: bin/lunarml
	$(MAKE) -C test VARIANT=nodejs-cps NODE=$(NODE)

validate-lua: bin/lunarml
	bin/lunarml compile -o lunarml.gen2.lua --print-timings LunarML.mlb
	$(LUA) lunarml.gen2.lua -Blib/lunarml compile -o lunarml.gen3.lua --print-timings LunarML.mlb
	diff --report-identical-files lunarml.gen2.lua lunarml.gen3.lua

validate-luajit: bin/lunarml
	bin/lunarml compile -o lunarml.gen2-luajit.lua --luajit --print-timings LunarML.mlb
	$(LUAJIT) lunarml.gen2-luajit.lua -Blib/lunarml compile -o lunarml.gen3-luajit.lua --luajit --print-timings LunarML.mlb
	diff --report-identical-files lunarml.gen2-luajit.lua lunarml.gen3-luajit.lua

validate-js: bin/lunarml
	bin/lunarml compile -o lunarml.gen2.js --js-cps --print-timings LunarML.mlb
	$(NODE) lunarml.gen2.js -Blib/lunarml compile -o lunarml.gen3.js --js-cps --print-timings LunarML.mlb
	diff --report-identical-files lunarml.gen2.js lunarml.gen3.js

verify-lua: bin/lunarml bin/lunarml.gen2.lua
	$(MAKE) -C test verify-lua VARIANT=lua LUA=$(LUA) LUNARML_GEN2="$(LUA) ../bin/lunarml.gen2.lua" VARIANT_GEN2=gen2

verify-luajit: bin/lunarml bin/lunarml.gen2-luajit.lua
	$(MAKE) -C test verify-luajit VARIANT=luajit LUAJIT=$(LUAJIT) LUNARML_GEN2="$(LUAJIT) ../bin/lunarml.gen2-luajit.lua" VARIANT_GEN2=gen2-luajit

verify-js: bin/lunarml bin/lunarml.gen2.js
	$(MAKE) -C test verify-nodejs-cps VARIANT=nodejs-cps NODE=$(NODE) LUNARML_GEN2="$(NODE) ../bin/lunarml.gen2.js" VARIANT_GEN2=gen2

.PHONY: all typecheck test test-lua test-lua-continuations test-luajit test-nodejs test-nodejs-cps validate-lua validate-luajit validate-js verify-lua verify-luajit verify-js
