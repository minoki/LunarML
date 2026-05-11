import { readFileSync } from 'fs';

const wasmPath = process.argv[2] || 'array-ops.wasm';
const bytes = readFileSync(wasmPath);

const { instance } = await WebAssembly.instantiate(bytes, {});
const exp = instance.exports;
exp._initialize();

function check(name, got, expected) {
  if (got !== expected) {
    console.error(`FAIL ${name}: expected ${expected}, got ${got}`);
    process.exit(1);
  } else {
    console.log(`PASS ${name}: ${got}`);
  }
}

check('arrayLength', exp['arrayLength'](), 5);
check('get0', exp['get0'](), 42);
check('get1', exp['get1'](), 13);
check('get4', exp['get4'](), 99);
check('tab0', exp['tab0'](), 0);   // 0*0
check('tab1', exp['tab1'](), 1);   // 1*1
check('tab2', exp['tab2'](), 4);   // 2*2
check('tab3', exp['tab3'](), 9);   // 3*3

check('boolGet0', exp['boolGet0'](), 1);  // true
check('boolGet1', exp['boolGet1'](), 0);  // false
check('boolGet2', exp['boolGet2'](), 1);  // true

console.log('All tests passed!');
