import { readFile } from "node:fs/promises";

const wasm = await WebAssembly.compile(await readFile("mutual-rec-closure.wasm"));
const { exports } = await WebAssembly.instantiate(wasm);
exports._initialize();

let ok = true;
function check(label, actual, expected) {
  if (actual !== expected) {
    console.error(`FAIL: ${label}: expected ${expected}, got ${actual}`);
    ok = false;
  }
}

// evenFromN n: returns n if n is even, 0 if n is odd
// Uses internal mutual recursion go_even/go_odd capturing outer 'n'
check("evenFromN(0)", exports.evenFromN(0), 0);   // even: returns 0
check("evenFromN(1)", exports.evenFromN(1), 0);   // odd: returns 0
check("evenFromN(2)", exports.evenFromN(2), 2);   // even: returns 2
check("evenFromN(3)", exports.evenFromN(3), 0);   // odd: returns 0
check("evenFromN(4)", exports.evenFromN(4), 4);   // even: returns 4
check("evenFromN(5)", exports.evenFromN(5), 0);   // odd: returns 0
check("evenFromN(10)", exports.evenFromN(10), 10); // even: returns 10
check("evenFromN(11)", exports.evenFromN(11), 0); // odd: returns 0

// tripleCountDown n: always returns 0 (fa/fb/fc all return n when they hit 0,
// and fa n - n = 0)
check("tripleCountDown(0)", exports.tripleCountDown(0), 0);
check("tripleCountDown(1)", exports.tripleCountDown(1), 0);
check("tripleCountDown(2)", exports.tripleCountDown(2), 0);
check("tripleCountDown(3)", exports.tripleCountDown(3), 0);
check("tripleCountDown(6)", exports.tripleCountDown(6), 0);
check("tripleCountDown(9)", exports.tripleCountDown(9), 0);

if (ok) {
  console.log("All tests passed.");
} else {
  process.exit(1);
}
