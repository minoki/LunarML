fun foo x = let exception Foo
            in if x then
                   raise Foo
               else
                   foo true handle Foo => print "Caught!\n"
            end;
foo false handle _ => print "Not caught!\n";
