.PHONY: all clean test

all: fact-fib.wasm

fact-fib.wat: fact-fib.mlb fact-fib.sml ../../src/wasm/syntax.sml ../../src/wasm/wat-writer.sml
	mlton fact-fib.mlb
	./fact-fib

fact-fib.wasm: fact-fib.wat
	wat2wasm fact-fib.wat -o fact-fib.wasm

test: fact-fib.wasm
	node test-fact-fib.mjs

clean:
	$(RM) fact-fib fact-fib.wat fact-fib.wasm
