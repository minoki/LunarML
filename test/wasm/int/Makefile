.PHONY: all clean test test-js

LUNARML = ../../../bin/lunarml

all: int-divmod.wasm int-divmod.mjs

int-divmod.mjs: int-divmod.sml
	$(LUNARML) compile --nodejs --lib -o $@ $<

int-divmod.wasm: int-divmod.mlb int-divmod.sml
	$(LUNARML) compile --wasm --lib -o $@ $<

test: int-divmod.wasm
	node test.mjs

test-js: int-divmod.mjs
	node test-js.mjs

clean:
	$(RM) int-divmod.wasm int-divmod.mjs
