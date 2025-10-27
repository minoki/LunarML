MLTON = mlton
MLLEX = mllex
MLYACC = mlyacc

all: albireo

tokenize.lex.sml: tokenize.lex
	$(MLLEX) $<

parse.grm.sml parse.grm.sig: parse.grm
	$(MLYACC) $<

sources = \
  syntax.sml \
  parse.grm.sig \
  tokenize.lex.sml \
  parse.grm.sml \
  parse.sml \
  emit.sml \
  main.sml

albireo: albireo.mlb $(sources)
	$(MLTON) $<
