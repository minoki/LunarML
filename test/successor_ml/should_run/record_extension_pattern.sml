val { a, b, ... = x } = { a = 1, b = "b", c = "foo", d = 0w42 };
print (#c x ^ "\n");
print (Word.toString (#d x) ^ "\n");
