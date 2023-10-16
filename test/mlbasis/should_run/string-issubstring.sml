print (Bool.toString (String.isSubstring "" "") ^ "\n");
print (Bool.toString (String.isSubstring "" "Standard ML") ^ "\n");
print (Bool.toString (String.isSubstring "Standard ML" "") ^ "\n");
print (Bool.toString (String.isSubstring "fox" "the quick brown fox jumps over the lazy dog.") ^ "\n");
print (Bool.toString (String.isSubstring "abc" "") ^ "\n");
print (Bool.toString (String.isSubstring "and" "Standard ML") ^ "\n");
print (Bool.toString (String.isSubstring "abc" "Standard ML") ^ "\n");
print (Bool.toString (String.isSubstring "Standard" "Standard ML") ^ "\n");
print (Bool.toString (String.isSubstring "ML" "Standard ML") ^ "\n");
