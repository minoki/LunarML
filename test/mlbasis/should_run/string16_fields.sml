print (String16.toString (String16.concatWith "|" (String16.fields (fn c => c = #"|") "|abc||def")) ^ "\n");
