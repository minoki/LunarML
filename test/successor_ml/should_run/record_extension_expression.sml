val x = { a = "a", b = 42 }
fun f x = { c = 0wxFF, ... = x }
val y = f x
val z = { 1 = 100, 2 = 200, ... = y };
print (Int.toString (#1 z) ^ "\n");
print (Int.toString (#2 z) ^ "\n");
print (#a z ^ "\n");
print (Int.toString (#b z) ^ "\n");
print (Word.toString (#c z) ^ "\n");
print (Bool.toString (z = { 1 = 100, 2 = 200, a = "a", b = 42, c = 0wxFF }) ^ "\n");
