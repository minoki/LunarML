print (String32.toString (String32.concatWith "|" (String32.fields (fn c => c = #"|") "|abc||def")) ^ "\n");
