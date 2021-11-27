case Int.maxInt of
    SOME maxInt => ( (maxInt + 1; print "maxInt (1)\n") handle Overflow => ()
                   ; (1 + maxInt; print "maxInt (2)\n") handle Overflow => ()
                   ; (maxInt - ~1; print "maxInt (3)\n") handle Overflow => ()
                   ; (maxInt * 2; print "maxInt (4)\n") handle Overflow => ()
                   ; (2 * maxInt; print "maxInt (5)\n") handle Overflow => ()
                   ; (maxInt * maxInt; print "maxInt (6)\n") handle Overflow => ()
                   )
  | NONE => ();
case Int.minInt of
    SOME minInt => ( (minInt + ~1; print "minInt (1)\n") handle Overflow => ()
                   ; (~1 + minInt; print "minInt (2)\n") handle Overflow => ()
                   ; (minInt - 1; print "minInt (3)\n") handle Overflow => ()
                   ; (minInt * ~1; print "minInt (4)\n") handle Overflow => ()
                   ; (~1 * minInt; print "minInt (5)\n") handle Overflow => ()
                   ; (minInt * minInt; print "minInt (6)\n") handle Overflow => ()
                   ; (minInt div ~1; print "minInt (7)\n") handle Overflow => ()
                   ; (~ minInt; print "minInt (8)\n") handle Overflow => ()
                   ; (abs minInt; print "minInt (9)\n") handle Overflow => ()
                   )
  | NONE => ();
