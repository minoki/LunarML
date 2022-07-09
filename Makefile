LUA = lua
LUAJIT = luajit
NODE = node

all: lunarml

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
  src/main.sml

lunarml: LunarML.mlb $(sources)
	mlton -output $@ LunarML.mlb

lunarml.gen2: lunarml LunarML.mlb $(sources)
	./lunarml -o lunarml.gen2.lua LunarML.mlb
	echo "#!/usr/bin/env lua" > $@
	cat lunarml.gen2.lua >> $@
	chmod +x $@

src/syntax.grm.sml src/syntax.grm.sig: src/syntax.grm
	mlyacc $<

src/language-options.sml: src/language-options.lua
	$(LUA) src/language-options.lua $@

src/primitives.sml: src/primitives.lua
	$(LUA) src/primitives.lua $@ > /dev/null

test: lunarml
	$(LUA) test/run.lua ./lunarml $(LUA)

test-stackless-handle: lunarml
	$(LUA) test/run.lua ./lunarml $(LUA) stackless-handle

test-luajit: lunarml
	$(LUA) test/run.lua ./lunarml $(LUAJIT) luajit

test-nodejs: lunarml
	$(LUA) test/run-nodejs.lua ./lunarml $(NODE)

test-nodejs-cps: lunarml
	$(LUA) test/run-nodejs.lua ./lunarml $(NODE) cps

.PHONY: all test test-nodejs
