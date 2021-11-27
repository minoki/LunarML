val quot = Int.quot
val rem = Int.rem
infix 7 quot rem;
case Int.maxInt of
    SOME maxInt => let val () = (maxInt + 1; print "maxInt (1)\n") handle Overflow => ()
                       val () = (1 + maxInt; print "maxInt (2)\n") handle Overflow => ()
                       val () = (maxInt - ~1; print "maxInt (3)\n") handle Overflow => ()
                       val () = (maxInt * 2; print "maxInt (4)\n") handle Overflow => ()
                       val () = (2 * maxInt; print "maxInt (5)\n") handle Overflow => ()
                       val () = (maxInt * maxInt; print "maxInt (6)\n") handle Overflow => ()
                       val _ = maxInt * ~1
                       val _ = ~1 * maxInt
                       val _ = maxInt div 1
                       val _ = maxInt div 2
                       val _ = maxInt div ~1
                       val _ = maxInt div ~2
                       val 0 = maxInt mod 1
                       val 0 = maxInt mod ~1
                       val _ = maxInt quot 1
                       val _ = maxInt quot 2
                       val _ = maxInt quot ~1
                       val _ = maxInt quot ~2
                       val 0 = maxInt rem 1
                       val 0 = maxInt rem ~1
                   in ()
                   end
  | NONE => ();
case Int.minInt of
    SOME minInt => let val () = (minInt + ~1; print "minInt (1)\n") handle Overflow => ()
                       val () = (~1 + minInt; print "minInt (2)\n") handle Overflow => ()
                       val () = (minInt - 1; print "minInt (3)\n") handle Overflow => ()
                       val () = (minInt * ~1; print "minInt (4)\n") handle Overflow => ()
                       val () = (~1 * minInt; print "minInt (5)\n") handle Overflow => ()
                       val () = (minInt * minInt; print "minInt (6)\n") handle Overflow => ()
                       val () = (minInt div ~1; print "minInt (7)\n") handle Overflow => ()
                       val () = (~ minInt; print "minInt (8)\n") handle Overflow => ()
                       val () = (abs minInt; print "minInt (9)\n") handle Overflow => ()
                       val () = (minInt quot ~1; print "minInt (10)\n") handle Overflow => ()
                       val _ = minInt div 1
                       val _ = minInt div 2
                       val _ = minInt div ~2
                       val _ = 1 div minInt
                       val 1 = minInt div minInt
                       val 0 = minInt mod 1
                       val 0 = minInt mod ~1
                       val 0 = minInt mod minInt
                       val _ = minInt quot 1
                       val _ = minInt quot 2
                       val _ = minInt quot ~2
                       val 0 = minInt rem 1
                       val 0 = minInt rem ~1
                   in ()
                   end
  | NONE => ();
