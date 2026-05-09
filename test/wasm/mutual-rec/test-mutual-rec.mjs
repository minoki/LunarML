import { readFile } from "node:fs/promises";

const wasm = await WebAssembly.compile(await readFile("mutual-rec.wasm"));
const { exports } = await WebAssembly.instantiate(wasm);
exports._initialize();

let ok = true;
function check(label, actual, expected) {
  if (actual !== expected) {
    console.error(`FAIL: ${label}: expected ${expected}, got ${actual}`);
    ok = false;
  }
}

// Two-function mutual recursion: even/odd
// (returns 1 for true, 0 for false)
check("even(0)", exports.even(0), 1);
check("even(1)", exports.even(1), 0);
check("even(2)", exports.even(2), 1);
check("even(10)", exports.even(10), 1);
check("even(11)", exports.even(11), 0);
check("odd(0)", exports.odd(0), 0);
check("odd(1)", exports.odd(1), 1);
check("odd(7)", exports.odd(7), 1);
check("odd(8)", exports.odd(8), 0);

// Three-function mutual recursion: countDown (f3a -> f3b -> f3c -> f3a -> ...)
check("countDown(0)", exports.countDown(0), 0);
check("countDown(3)", exports.countDown(3), 0);
check("countDown(6)", exports.countDown(6), 0);
check("countDown(9)", exports.countDown(9), 0);
check("countDown(10)", exports.countDown(10), 0);

if (ok) {
  console.log("All tests passed.");
} else {
  process.exit(1);
}
