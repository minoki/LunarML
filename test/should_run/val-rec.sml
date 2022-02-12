fun foo false = foo false
  | foo true = "foo"
and bar false = bar false
  | bar true = "bar"
and baz cond = (if cond then "" else baz cond ; foo cond);

print (baz true ^ "\n");
print (bar true ^ "\n");
