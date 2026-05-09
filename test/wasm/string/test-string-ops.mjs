import { readFile } from "node:fs/promises";

const wasm = await WebAssembly.compile(await readFile("string-ops.wasm"));
const { exports } = await WebAssembly.instantiate(wasm);
exports._initialize();

let ok = true;
function check(label, actual, expected) {
  if (actual !== expected) {
    console.error(`FAIL: ${label}: expected ${expected}, got ${actual}`);
    ok = false;
  } else {
    console.log(`PASS: ${label}: ${actual}`);
  }
}

// String.size "Hello, World!" = 13
check("helloLen()", exports.helloLen(), 13);

// String.size ("abc" ^ "!") = 4
check("concatLen()", exports.concatLen(), 4);

// String equality
check("strEqSame()", exports.strEqSame(), 1);
check("strEqDiff()", exports.strEqDiff(), 0);
check("strEqLenDiff()", exports.strEqLenDiff(), 0);
check("strEqEmpty()", exports.strEqEmpty(), 1);

// String.<
check("strLtLess()", exports.strLtLess(), 1);
check("strLtEqual()", exports.strLtEqual(), 0);
check("strLtPrefix()", exports.strLtPrefix(), 1);
check("strLtLonger()", exports.strLtLonger(), 0);

// String.str
check("strFromChar()", exports.strFromChar(), 1);

// String.concat
check("concatList()", exports.concatList(), 13);

// String.implode
check("implodeTest()", exports.implodeTest(), 3);

// Edge cases
check("concatEmpty()", exports.concatEmpty(), 0);
check("concatWithEmpty()", exports.concatWithEmpty(), 3);
check("multiConcat()", exports.multiConcat(), 10);

if (ok) {
  console.log("All tests passed.");
} else {
  process.exit(1);
}
