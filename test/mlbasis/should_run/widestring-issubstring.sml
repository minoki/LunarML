print (Bool.toString (WideString.isSubstring "" "") ^ "\n");
print (Bool.toString (WideString.isSubstring "" "Standard ML") ^ "\n");
print (Bool.toString (WideString.isSubstring "Standard ML" "") ^ "\n");
print (Bool.toString (WideString.isSubstring "fox" "the quick brown fox jumps over the lazy dog.") ^ "\n");
print (Bool.toString (WideString.isSubstring "abc" "") ^ "\n");
print (Bool.toString (WideString.isSubstring "and" "Standard ML") ^ "\n");
print (Bool.toString (WideString.isSubstring "abc" "Standard ML") ^ "\n");
print (Bool.toString (WideString.isSubstring "Standard" "Standard ML") ^ "\n");
print (Bool.toString (WideString.isSubstring "ML" "Standard ML") ^ "\n");
