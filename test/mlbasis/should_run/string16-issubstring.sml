print (Bool.toString (String16.isSubstring "" "") ^ "\n");
print (Bool.toString (String16.isSubstring "" "Standard ML") ^ "\n");
print (Bool.toString (String16.isSubstring "Standard ML" "") ^ "\n");
print (Bool.toString (String16.isSubstring "fox" "the quick brown fox jumps over the lazy dog.") ^ "\n");
print (Bool.toString (String16.isSubstring "abc" "") ^ "\n");
print (Bool.toString (String16.isSubstring "and" "Standard ML") ^ "\n");
print (Bool.toString (String16.isSubstring "abc" "Standard ML") ^ "\n");
print (Bool.toString (String16.isSubstring "Standard" "Standard ML") ^ "\n");
print (Bool.toString (String16.isSubstring "ML" "Standard ML") ^ "\n");
