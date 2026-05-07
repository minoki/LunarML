fun dumpWord8 x = if x <= 0wxF then "0" ^ Word8.fmt StringCvt.HEX x else Word8.fmt StringCvt.HEX x
fun dumpWord8Vector v = String.concatWith " " (Word8Vector.foldr (fn (x, acc) => dumpWord8 x :: acc) [] v)
fun dumpWord8Array v = String.concatWith " " (Word8Vector.foldr (fn (x, acc) => dumpWord8 x :: acc) [] v);
fun okBig (x : real) = 
  Real.== (PackRealBig.fromBytes (PackRealBig.toBytes x), x)
fun okLittle (x : real) = 
  Real.== (PackRealLittle.fromBytes (PackRealLittle.toBytes x), x);

print (dumpWord8Vector (PackRealBig.toBytes 0.0) ^ "\n");
print (dumpWord8Vector (PackRealBig.toBytes 1.0) ^ "\n");
print (dumpWord8Vector (PackRealBig.toBytes ~1.1) ^ "\n");
print (dumpWord8Vector (PackRealLittle.toBytes 0.0) ^ "\n");
print (dumpWord8Vector (PackRealLittle.toBytes 1.0) ^ "\n");
print (dumpWord8Vector (PackRealLittle.toBytes ~1.1) ^ "\n");

print (Bool.toString (okBig 5.0) ^ "\n");
print (Bool.toString (okBig 1.3e~150) ^ "\n");
print (Bool.toString (okBig ~4.4e200) ^ "\n");
print (Bool.toString (okLittle 5.0) ^ "\n");
print (Bool.toString (okLittle 1.3e~150) ^ "\n");
print (Bool.toString (okLittle ~4.4e200) ^ "\n");

val short = Word8Vector.tabulate (7, fn _ => 0w0);
print ((PackRealBig.fromBytes short; "BAD\n") handle Subscript => "GOOD\n");
print ((PackRealLittle.fromBytes short; "BAD\n") handle Subscript => "GOOD\n");

val vBig = Word8Vector.fromList [0wx40, 0wx09, 0wx21, 0wxfb, 0wx54, 0wx44, 0wx2d, 0wx18, 0wxff, 0wxf0, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00];
print (Real.toString (PackRealBig.subVec (vBig, 0)) ^ "\n");
print (Real.toString (PackRealBig.subVec (vBig, 1)) ^ "\n");
print ((PackRealBig.subVec (vBig, 2); "BAD\n") handle Subscript => "GOOD\n");
print ((PackRealBig.subVec (vBig, ~1); "BAD\n") handle Subscript => "GOOD\n");

val aBig = Word8Array.array (16, 0w0);
Word8Array.copyVec { src = vBig, dst = aBig, di = 0 };
print (Real.toString (PackRealBig.subArr (aBig, 0)) ^ "\n");
print (Real.toString (PackRealBig.subArr (aBig, 1)) ^ "\n");
print ((PackRealBig.subArr (aBig, 2); "BAD\n") handle Subscript => "GOOD\n");
print ((PackRealBig.subArr (aBig, ~1); "BAD\n") handle Subscript => "GOOD\n");

PackRealBig.update (aBig, 1, 5.5);
print (Real.toString (PackRealBig.subArr (aBig, 1)) ^ "\n");

val vLittle = Word8Vector.fromList [0wx40, 0wx09, 0wx21, 0wxfb, 0wx54, 0wx44, 0wx2d, 0wx18, 0wxff, 0wxf0, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00];
print (Real.toString (PackRealLittle.subVec (vLittle, 0)) ^ "\n");
print (Real.toString (PackRealLittle.subVec (vLittle, 1)) ^ "\n");
print ((PackRealLittle.subVec (vLittle, 2); "BAD\n") handle Subscript => "GOOD\n");
print ((PackRealLittle.subVec (vLittle, ~1); "BAD\n") handle Subscript => "GOOD\n");

val aLittle = Word8Array.array (16, 0w0);
Word8Array.copyVec { src = vLittle, dst = aLittle, di = 0 };
print (Real.toString (PackRealLittle.subArr (aLittle, 0)) ^ "\n");
print (Real.toString (PackRealLittle.subArr (aLittle, 1)) ^ "\n");
print ((PackRealLittle.subArr (aLittle, 2); "BAD\n") handle Subscript => "GOOD\n");
print ((PackRealLittle.subArr (aLittle, ~1); "BAD\n") handle Subscript => "GOOD\n");

PackRealLittle.update (aLittle, 1, 5.5);
print (Real.toString (PackRealLittle.subArr (aLittle, 1)) ^ "\n");
