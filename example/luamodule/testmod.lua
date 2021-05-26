local module1 = require "module1"
local module2 = require "module2"
assert(module1(42, 37) == 79)
assert(module2.add({3, 7}) == 10)
module2.print(module2.hello .. "\n")
print(module2.fun)
