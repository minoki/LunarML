all: DamepoML

sources = \
  syntax.grm.sig \
  tokenizer.sml \
  syntax.sml \
  syntax.grm.sml \
  parser.sml \
  postparsing.sml \
  typed.sml \
  typing.sml \
  initialenv.sml \
  fsyntax.sml \
  ftransform.sml \
  codegen_lua.sml \
  driver.sml \
  main.sml

DamepoML: DamepoML.mlb $(sources)
	mlton DamepoML.mlb

syntax.grm.sml syntax.grm.sig: syntax.grm
	mlyacc $<

.PHONY: all
