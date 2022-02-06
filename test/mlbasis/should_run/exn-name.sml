print (exnName Div ^ "\n");
print (General.exnName (Fail "foo") ^ "\n");
exception FooBarBaz;
print (General.exnName FooBarBaz ^ "\n");
