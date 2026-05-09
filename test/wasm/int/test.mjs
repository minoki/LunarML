import { readFile } from "node:fs/promises";

const wasm = await WebAssembly.compile(await readFile("int-divmod.wasm"));
const { exports } = await WebAssembly.instantiate(wasm);
exports._initialize();
const { div, mod, quot, rem } = exports;

console.log(div(7, 3));
console.log(div(7, -3));
console.log(div(-7, 3));
console.log(div(-7, -3));
console.log(mod(7, 3));
console.log(mod(7, -3));
console.log(mod(-7, 3));
console.log(mod(-7, -3));
console.log(quot(7, 3));
console.log(quot(7, -3));
console.log(quot(-7, 3));
console.log(quot(-7, -3));
console.log(rem(7, 3));
console.log(rem(7, -3));
console.log(rem(-7, 3));
console.log(rem(-7, -3));
