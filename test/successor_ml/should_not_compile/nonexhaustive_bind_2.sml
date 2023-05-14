let val { ... = { a = SOME f } } = { a = SOME (fn x => x) }
in f 1;
   f "y"
end;
