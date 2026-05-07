import { readFile } from "node:fs/promises";

const wasm = await WebAssembly.compile(await readFile("int-divmod.wasm"));
const { exports: { div, mod, quot, rem } } = await WebAssembly.instantiate(wasm);

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
