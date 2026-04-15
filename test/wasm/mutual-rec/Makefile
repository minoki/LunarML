.PHONY: all clean test

lunarml = ../../../bin/lunarml

all: mutual-rec.wasm mutual-rec-closure.wasm

mutual-rec.wasm: mutual-rec.sml mutual-rec.mlb
	$(lunarml) compile --wasm --lib -o $@ mutual-rec.mlb

mutual-rec-closure.wasm: mutual-rec-closure.sml mutual-rec-closure.mlb
	$(lunarml) compile --wasm --lib -o $@ mutual-rec-closure.mlb

test: mutual-rec.wasm mutual-rec-closure.wasm
	node test-mutual-rec.mjs
	node test-mutual-rec-closure.mjs

clean:
	$(RM) mutual-rec.wasm mutual-rec-closure.wasm
