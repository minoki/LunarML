LUA = lua

all: lunarml

sources = \
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
  src/fsyntax.sml \
  src/ftransform.sml \
  src/codegen_lua.sml \
  src/driver.sml \
  src/main.sml

lunarml: LunarML.mlb $(sources)
	mlton -output $@ LunarML.mlb

src/syntax.grm.sml src/syntax.grm.sig: src/syntax.grm
	mlyacc $<

test: lunarml
	$(LUA) test/run.lua ./lunarml $(LUA)

.PHONY: all test
