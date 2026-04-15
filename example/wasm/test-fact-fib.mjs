import { readFile } from "node:fs/promises";

const useWat = process.argv.includes("--wat");
const wasmFile = useWat ? "fact-fib-from-wat.wasm" : "fact-fib.wasm";
const wasm = await WebAssembly.compile(await readFile(wasmFile));
const { exports } = await WebAssembly.instantiate(wasm);

let ok = true;
function check(label, actual, expected) {
  if (actual !== expected) {
    console.error(`FAIL: ${label}: expected ${expected}, got ${actual}`);
    ok = false;
  }
}

// factorial tests
check("factorial(0)", exports.factorial(0), 1);
check("factorial(1)", exports.factorial(1), 1);
check("factorial(5)", exports.factorial(5), 120);
check("factorial(10)", exports.factorial(10), 3628800);
check("factorial(12)", exports.factorial(12), 479001600);

// fibonacci tests
check("fib(0)", exports.fib(0), 0);
check("fib(1)", exports.fib(1), 1);
check("fib(2)", exports.fib(2), 1);
check("fib(5)", exports.fib(5), 5);
check("fib(10)", exports.fib(10), 55);
check("fib(20)", exports.fib(20), 6765);

// even/odd
check("even(0)", exports.even(0), 1);
check("even(1)", exports.even(1), 0);
check("even(2)", exports.even(2), 1);
check("even(3)", exports.even(3), 0);
check("odd(0)", exports.odd(0), 0);
check("odd(1)", exports.odd(1), 1);
check("odd(2)", exports.odd(2), 0);
check("odd(3)", exports.odd(3), 1);

if (ok) {
  console.log("All tests passed.");
} else {
  process.exit(1);
}
