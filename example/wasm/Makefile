lunarml = lunarml

all: fact-fib.wasm fact-fib-from-wat.wasm
.PHONY: all

fact-fib.wasm: fact-fib.mlb fact-fib.sml
	$(lunarml) compile --wasm --lib -o $@ $<

fact-fib.wat: fact-fib.mlb fact-fib.sml
	$(lunarml) compile --wasm-wat --lib -o $@ $<

fact-fib-from-wat.wasm: fact-fib.wat
	wasm-tools parse $< -o $@

clean:
	-rm fact-fib.wasm fact-fib.wat fact-fib-from-wat.wasm
.PHONY: clean
