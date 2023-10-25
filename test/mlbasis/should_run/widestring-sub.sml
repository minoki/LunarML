print (WideChar.toString (WideString.sub ("foo", 0)) ^ "\n");
print (WideChar.toString (WideString.sub ("foo", 1)) ^ "\n");
print (WideChar.toString (WideString.sub ("foo", 2)) ^ "\n");
print (WideChar.toString (WideString.sub ("foo", ~1)) ^ "\n" handle Subscript => "Subscript\n");
print (WideChar.toString (WideString.sub ("foo", 3)) ^ "\n" handle Subscript => "Subscript\n");
print (WideChar.toString (WideString.sub ("foo", 4)) ^ "\n" handle Subscript => "Subscript\n");
