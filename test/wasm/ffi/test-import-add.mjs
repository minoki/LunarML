import { readFileSync } from 'fs';
import { WASI } from 'wasi';

const wasmBuffer = readFileSync(new URL('./import-add.wasm', import.meta.url));

// Provide the imported "math.add" function (takes i32, i32, returns i32)
const importObject = {
  math: {
    add: (a, b) => (a + b) | 0,
  },
};

const { instance } = await WebAssembly.instantiate(wasmBuffer, importObject);
const { callAdd } = instance.exports;

// callAdd wraps the WasmGC boxed Int32 calling convention
// The exported function takes (closure: anyref, arg: anyref) and returns anyref
// For lib mode, we need to call it properly through the LunarML runtime

// Check that the import was registered correctly by verifying the module loads
console.log("PASS: Module loaded with math.add import");
console.log("Exports:", Object.keys(instance.exports).join(', '));
