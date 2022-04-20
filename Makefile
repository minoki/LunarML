LUA = lua

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
  src/target-info.sml \
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
  src/lua-syntax.sml \
  src/lua-transform.sml \
  src/codegen_lua.sml \
  src/js-syntax.sml \
  src/codegen-js.sml \
  src/mlb-syntax.sml \
  src/mlb-parser.sml \
  src/driver.sml \
  src/mlb-eval.sml \
  src/main.sml

lunarml: LunarML.mlb $(sources)
	mlton -output $@ LunarML.mlb

src/syntax.grm.sml src/syntax.grm.sig: src/syntax.grm
	mlyacc $<

test: lunarml
	$(LUA) test/run.lua ./lunarml $(LUA)

.PHONY: all test
