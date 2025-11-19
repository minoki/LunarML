print (Bool.toString (String32.isSubstring "" "") ^ "\n");
print (Bool.toString (String32.isSubstring "" "Standard ML") ^ "\n");
print (Bool.toString (String32.isSubstring "Standard ML" "") ^ "\n");
print (Bool.toString (String32.isSubstring "fox" "the quick brown fox jumps over the lazy dog.") ^ "\n");
print (Bool.toString (String32.isSubstring "abc" "") ^ "\n");
print (Bool.toString (String32.isSubstring "and" "Standard ML") ^ "\n");
print (Bool.toString (String32.isSubstring "abc" "Standard ML") ^ "\n");
print (Bool.toString (String32.isSubstring "Standard" "Standard ML") ^ "\n");
print (Bool.toString (String32.isSubstring "ML" "Standard ML") ^ "\n");
