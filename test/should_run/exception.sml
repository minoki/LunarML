exception Foo;
(raise Foo) handle Foo => print "Hello ";
exception Bar of string;
exception Baz = Bar;
(raise Bar "world!\n") handle Baz s => print s;
