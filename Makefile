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
  src/numeric.sml \
  src/text.sml \
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
  src/ftransform.sml \
  src/cps.sml \
  src/lua-syntax.sml \
  src/lua-transform.sml \
  src/codegen-lua.sml \
  src/js-syntax.sml \
  src/js-transform.sml \
  src/codegen-js.sml \
  src/codegen-js-cps.sml \
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

bin/lunarml.gen2: bin/lunarml LunarML.mlb $(sources)
	bin/lunarml compile -o lunarml.gen2.lua LunarML.mlb
	echo "#!/usr/bin/env lua" > $@
	cat lunarml.gen2.lua >> $@
	chmod +x $@

src/syntax.grm.sml src/syntax.grm.sig: src/syntax.grm
	mlyacc $<

src/language-options.sml: src/language-options.lua
	$(LUA) src/language-options.lua $@

src/primitives.sml: src/primitives.lua
	$(LUA) src/primitives.lua $@ > /dev/null

src/command-line-settings.sml: util/record.lua Makefile
	$(LUA) util/record.lua CommandLineSettings "subcommand,output,outputMode,dump,optimizationLevel,backend,libDir" > $@

test: bin/lunarml
	$(LUA) test/run.lua bin/lunarml $(LUA)

test-lua-continuations: bin/lunarml
	$(LUA) test/run.lua bin/lunarml $(LUA) continuations

test-luajit: bin/lunarml
	$(LUA) test/run.lua bin/lunarml $(LUAJIT) luajit

test-nodejs: bin/lunarml
	$(LUA) test/run-nodejs.lua bin/lunarml $(NODE)

test-nodejs-cps: bin/lunarml
	$(LUA) test/run-nodejs.lua bin/lunarml $(NODE) cps

validate-lua: bin/lunarml
	bin/lunarml compile -o lunarml.gen2.lua LunarML.mlb
	$(LUA) lunarml.gen2.lua -Blib/lunarml compile -o lunarml.gen3.lua LunarML.mlb
	diff --report-identical-files lunarml.gen2.lua lunarml.gen3.lua

validate-luajit: bin/lunarml
	bin/lunarml compile -o lunarml.gen2-luajit.lua LunarML.mlb
	$(LUAJIT) lunarml.gen2-luajit.lua -Blib/lunarml compile -o lunarml.gen3-luajit.lua LunarML.mlb
	diff --report-identical-files lunarml.gen2-luajit.lua lunarml.gen3-luajit.lua

validate-js: bin/lunarml
	bin/lunarml compile -o lunarml.gen2.js --js-cps LunarML.mlb
	$(NODE) lunarml.gen2.js -Blib/lunarml compile -o lunarml.gen3.js --js-cps LunarML.mlb
	diff --report-identical-files lunarml.gen2.js lunarml.gen3.js

.PHONY: all typecheck test test-lua-continuations test-luajit test-nodejs test-nodejs-cps validate-lua validate-luajit validate-js
