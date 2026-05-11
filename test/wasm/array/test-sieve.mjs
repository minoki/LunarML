import { readFileSync } from 'fs';

const bytes = readFileSync('sieve.wasm');
const { instance } = await WebAssembly.instantiate(bytes, {});
const exp = instance.exports;
exp._initialize();

let ok = true;
function check(name, got, expected) {
  if (got !== expected) {
    console.error(`FAIL ${name}: expected ${expected}, got ${got}`);
    ok = false;
  } else {
    console.log(`PASS ${name}: ${got}`);
  }
}

check('primesBelow100', exp['primesBelow100'](), 25);
check('primesBelow1000', exp['primesBelow1000'](), 168);

if (ok) console.log('All tests passed!');
else process.exit(1);
