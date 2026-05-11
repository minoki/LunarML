.PHONY: all clean test

LUNARML = ../../../bin/lunarml

all: array-ops.wasm

array-ops.wasm: array-ops.mlb array-ops.sml
	$(LUNARML) compile --wasm --lib -o $@ $<

test: array-ops.wasm
	node test-array-ops.mjs

clean:
	$(RM) array-ops.wasm array-ops.wat
