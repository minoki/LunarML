all: DamepoML

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

DamepoML: DamepoML.mlb $(sources)
	mlton DamepoML.mlb

src/syntax.grm.sml src/syntax.grm.sig: src/syntax.grm
	mlyacc $<

.PHONY: all
