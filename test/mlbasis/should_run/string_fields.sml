print (String.concatWith "|" (String.fields (fn c => c = #"|") "|abc||def") ^ "\n");
