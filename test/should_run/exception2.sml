fun main () = ((raise Fail "exception 1")
               handle Fail x => ( print (x ^ "\n")
                                ; raise Fail "exception 2"
                                )
              )
              handle Fail y => print (y ^ "\n");
main ();
