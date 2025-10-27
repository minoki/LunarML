import { readFileSync } from "node:fs";
const wasmBuffer = readFileSync("test.wasm");
const wasmModule = await WebAssembly.instantiate(wasmBuffer);
const { foo } = wasmModule.instance.exports;
console.log(foo(1));
console.log(foo(2));
console.log(foo(3));
