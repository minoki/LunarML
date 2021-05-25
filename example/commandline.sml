val name = CommandLine.name ();
val args = CommandLine.arguments ();
print ("name = " ^ name ^ "\n");
List.foldl (fn (x, i) => (print ("arg[" ^ Int.toString i ^ "] = " ^ x ^ "\n"); i + 1)) 0 args;
