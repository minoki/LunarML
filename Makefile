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
  src/language-options-record.sml \
  src/language-options.sml \
  src/target-info.sml \
  src/primitives.sml \
  src/syntax.grm.sig \
  src/sourcepos.sml \
  src/message.sml \
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

typecheck: src/lunarml.mlb $(sources)
	mlton -stop tc $<

bin/lunarml: src/lunarml.mlb $(sources)
	mlton -output $@ $<

bin/lunarml.gen2.lua: src/lunarml-lunarml.mlb bin/lunarml $(sources)
	bin/lunarml compile -o $@ $<

bin/lunarml.gen2-luajit.lua: src/lunarml-lunarml.mlb bin/lunarml $(sources)
	bin/lunarml compile --luajit -o $@ $<

bin/lunarml.gen2.mjs: src/lunarml-lunarml.mlb bin/lunarml $(sources)
	bin/lunarml compile --nodejs-cps -o $@ $<

src/syntax.grm.sml src/syntax.grm.sig: src/syntax.grm
	mlyacc $<

src/primitives.sml: src/primitives.lua
	$(LUA) src/primitives.lua $@ > /dev/null

src/command-line-settings.sml: src/command-line-settings.lua util/record.lua
	$(LUA) util/record.lua $< > $@

src/language-options-record.sml: src/language-options-record.lua util/record.lua
	$(LUA) util/record.lua $< > $@

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
	bin/lunarml compile -o lunarml.gen2.lua --print-timings src/lunarml-lunarml.mlb
	$(LUA) lunarml.gen2.lua -Blib/lunarml compile -o lunarml.gen3.lua --print-timings src/lunarml-lunarml.mlb
	diff --report-identical-files lunarml.gen2.lua lunarml.gen3.lua

validate-luajit: bin/lunarml
	bin/lunarml compile -o lunarml.gen2-luajit.lua --luajit --print-timings src/lunarml-lunarml.mlb
	$(LUAJIT) lunarml.gen2-luajit.lua -Blib/lunarml compile -o lunarml.gen3-luajit.lua --luajit --print-timings src/lunarml-lunarml.mlb
	diff --report-identical-files lunarml.gen2-luajit.lua lunarml.gen3-luajit.lua

validate-js: bin/lunarml
	bin/lunarml compile -o lunarml.gen2.mjs --nodejs-cps --print-timings src/lunarml-lunarml.mlb
	$(NODE) lunarml.gen2.mjs -Blib/lunarml compile -o lunarml.gen3.mjs --nodejs-cps --print-timings src/lunarml-lunarml.mlb
	diff --report-identical-files lunarml.gen2.mjs lunarml.gen3.mjs

verify-lua: bin/lunarml bin/lunarml.gen2.lua
	$(MAKE) -C test verify-lua VARIANT=lua LUA=$(LUA) LUNARML_GEN2="$(LUA) ../bin/lunarml.gen2.lua" VARIANT_GEN2=gen2

verify-luajit: bin/lunarml bin/lunarml.gen2-luajit.lua
	$(MAKE) -C test verify-luajit VARIANT=luajit LUAJIT=$(LUAJIT) LUNARML_GEN2="$(LUAJIT) ../bin/lunarml.gen2-luajit.lua" VARIANT_GEN2=gen2-luajit

verify-js: bin/lunarml bin/lunarml.gen2.mjs
	$(MAKE) -C test verify-nodejs-cps VARIANT=nodejs-cps NODE=$(NODE) LUNARML_GEN2="$(NODE) ../bin/lunarml.gen2.mjs" VARIANT_GEN2=gen2

.PHONY: all typecheck test test-lua test-lua-continuations test-luajit test-nodejs test-nodejs-cps validate-lua validate-luajit validate-js verify-lua verify-luajit verify-js
