.PHONY: all clean test test-wat test-binary

all: fact-fib.wasm

# Build the test generator binary
fact-fib: fact-fib.mlb fact-fib.sml ../../src/wasm/syntax.sml ../../src/wasm/wat-writer.sml ../../src/wasm/writer.sml
	mlton fact-fib.mlb

# WAT path: generate .wat then convert to .wasm via wat2wasm
fact-fib.wat: fact-fib
	./fact-fib

fact-fib-from-wat.wasm: fact-fib.wat
	wat2wasm fact-fib.wat -o fact-fib-from-wat.wasm --enable-gc --enable-tail-call --enable-exceptions

# Binary path: generate .wasm directly
fact-fib.wasm: fact-fib
	./fact-fib --binary

# Test using the directly generated .wasm binary
test: test-binary

test-binary: fact-fib.wasm
	node test-fact-fib.mjs

# Test using the WAT-generated .wasm (via wat2wasm)
test-wat: fact-fib-from-wat.wasm
	node test-fact-fib.mjs --wat

clean:
	$(RM) fact-fib fact-fib.wat fact-fib.wasm fact-fib-from-wat.wasm
