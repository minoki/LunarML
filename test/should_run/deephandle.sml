fun f i =
  if i = 0 then
    raise Fail "Hello!"
  else
    f (i - 1) handle e => raise e;
f 500 handle Fail s => print (s ^ "\n");
