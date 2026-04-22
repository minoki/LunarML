.PHONY: all clean test

LUNARML = ../../../bin/lunarml

all: string-ops.wasm

string-ops.wasm: string-ops.mlb string-ops.sml
	$(LUNARML) compile --wasm --lib -o $@ $<

test: string-ops.wasm
	node test-string-ops.mjs

clean:
	$(RM) string-ops.wasm string-ops.wat
