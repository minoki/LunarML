infix 4 ==
val op == = Real.==
val NaN = 0.0 / 0.0;
fun sameValue (x, y) = if x == 0.0 andalso y == 0.0 then
                           Real.signBit x = Real.signBit y
                       else
                           x == y orelse (Real.isNan x andalso Real.isNan y);
datatype 'a result = VALUE of 'a
                   | OVERFLOW
                   | DOMAIN
fun resultToString (VALUE x) = LargeInt.toString x
  | resultToString OVERFLOW = "Overflow"
  | resultToString DOMAIN = "DOMAIN"
val maxFiniteAsLargeInt = IntInf.<< (0x1_ffff_ffff_ffff_f, 0w1023 - 0w52);
val () = List.app (fn (s, x, { round, floor, ceil, trunc }) =>
                      ( let val actual = VALUE (Real.toLargeInt IEEEReal.TO_NEAREST x)
                                         handle Overflow => OVERFLOW
                                              | Domain => DOMAIN
                        in if round = actual then
                               print ("toLargeInt TO_NEAREST " ^ s ^ ": OK\n")
                           else
                               print ("toLargeInt TO_NEAREST " ^ s ^ ": mismatch (expected " ^ resultToString round ^ ", but got " ^ resultToString actual ^ ")\n")
                        end
                      ; let val actual = VALUE (Real.toLargeInt IEEEReal.TO_NEGINF x)
                                         handle Overflow => OVERFLOW
                                              | Domain => DOMAIN
                        in if floor = actual then
                               print ("toLargeInt TO_NEGINF " ^ s ^ ": OK\n")
                           else
                               print ("toLargeInt TO_NEGINF " ^ s ^ ": mismatch (expected " ^ resultToString floor ^ ", but got " ^ resultToString actual ^ ")\n")
                        end
                      ; let val actual = VALUE (Real.toLargeInt IEEEReal.TO_POSINF x)
                                         handle Overflow => OVERFLOW
                                              | Domain => DOMAIN
                        in if ceil = actual then
                               print ("toLargeInt TO_POSINF " ^ s ^ ": OK\n")
                           else
                               print ("toLargeInt TO_POSINF " ^ s ^ ": mismatch (expected " ^ resultToString ceil ^ ", but got " ^ resultToString actual ^ ")\n")
                        end
                      ; let val actual = VALUE (Real.toLargeInt IEEEReal.TO_ZERO x)
                                         handle Overflow => OVERFLOW
                                              | Domain => DOMAIN
                        in if trunc = actual then
                               print ("toLargeInt TO_ZERO " ^ s ^ ": OK\n")
                           else
                               print ("toLargeInt TO_ZERO " ^ s ^ ": mismatch (expected " ^ resultToString trunc ^ ", but got " ^ resultToString actual ^ ")\n")
                        end
                      )
                  )
                  [("negInf", Real.negInf, { round = OVERFLOW, floor = OVERFLOW, ceil = OVERFLOW, trunc = OVERFLOW })
                  ,("~maxFinite", ~Real.maxFinite, { round = VALUE (~maxFiniteAsLargeInt), floor = VALUE (~maxFiniteAsLargeInt), ceil = VALUE (~maxFiniteAsLargeInt), trunc = VALUE (~maxFiniteAsLargeInt) })
                  ,("~3.5", ~3.5, { round = VALUE ~4, floor = VALUE ~4, ceil = VALUE ~3, trunc = VALUE ~3 })
                  ,("~3.25", ~3.25, { round = VALUE ~3, floor = VALUE ~4, ceil = VALUE ~3, trunc = VALUE ~3 })
                  ,("~3.0", ~3.0, { round = VALUE ~3, floor = VALUE ~3, ceil = VALUE ~3, trunc = VALUE ~3 })
                  ,("~2.75", ~2.75, { round = VALUE ~3, floor = VALUE ~3, ceil = VALUE ~2, trunc = VALUE ~2 })
                  ,("~2.5", ~2.5, { round = VALUE ~2, floor = VALUE ~3, ceil = VALUE ~2, trunc = VALUE ~2 })
                  ,("~2.25", ~2.25, { round = VALUE ~2, floor = VALUE ~3, ceil = VALUE ~2, trunc = VALUE ~2 })
                  ,("~2.0", ~2.0, { round = VALUE ~2, floor = VALUE ~2, ceil = VALUE ~2, trunc = VALUE ~2 })
                  ,("~1.75", ~1.75, { round = VALUE ~2, floor = VALUE ~2, ceil = VALUE ~1, trunc = VALUE ~1 })
                  ,("~1.5", ~1.5, { round = VALUE ~2, floor = VALUE ~2, ceil = VALUE ~1, trunc = VALUE ~1 })
                  ,("~1.25", ~1.25, { round = VALUE ~1, floor = VALUE ~2, ceil = VALUE ~1, trunc = VALUE ~1 })
                  ,("~1.0", ~1.0, { round = VALUE ~1, floor = VALUE ~1, ceil = VALUE ~1, trunc = VALUE ~1 })
                  ,("~0.75", ~0.75, { round = VALUE ~1, floor = VALUE ~1, ceil = VALUE 0, trunc = VALUE 0 })
                  ,("~0.5", ~0.5, { round = VALUE 0, floor = VALUE ~1, ceil = VALUE 0, trunc = VALUE 0 })
                  ,("~0.25", ~0.25, { round = VALUE 0, floor = VALUE ~1, ceil = VALUE 0, trunc = VALUE 0 })
                  ,("~minNormalPos", ~Real.minNormalPos, { round = VALUE 0, floor = VALUE ~1, ceil = VALUE 0, trunc = VALUE 0 })
                  ,("~minPos", ~Real.minPos, { round = VALUE 0, floor = VALUE ~1, ceil = VALUE 0, trunc = VALUE 0 })
                  ,("~0.0", ~0.0, { round = VALUE 0, floor = VALUE 0, ceil = VALUE 0, trunc = VALUE 0 })
                  ,("0.0", 0.0, { round = VALUE 0, floor = VALUE 0, ceil = VALUE 0, trunc = VALUE 0 })
                  ,("minPos", Real.minPos, { round = VALUE 0, floor = VALUE 0, ceil = VALUE 1, trunc = VALUE 0 })
                  ,("minNormalPos", Real.minNormalPos, { round = VALUE 0, floor = VALUE 0, ceil = VALUE 1, trunc = VALUE 0 })
                  ,("0.25", 0.25, { round = VALUE 0, floor = VALUE 0, ceil = VALUE 1, trunc = VALUE 0 })
                  ,("0.5", 0.5, { round = VALUE 0, floor = VALUE 0, ceil = VALUE 1, trunc = VALUE 0 })
                  ,("0.75", 0.75, { round = VALUE 1, floor = VALUE 0, ceil = VALUE 1, trunc = VALUE 0 })
                  ,("1.0", 1.0, { round = VALUE 1, floor = VALUE 1, ceil = VALUE 1, trunc = VALUE 1 })
                  ,("1.25", 1.25, { round = VALUE 1, floor = VALUE 1, ceil = VALUE 2, trunc = VALUE 1 })
                  ,("1.5", 1.5, { round = VALUE 2, floor = VALUE 1, ceil = VALUE 2, trunc = VALUE 1 })
                  ,("1.75", 1.75, { round = VALUE 2, floor = VALUE 1, ceil = VALUE 2, trunc = VALUE 1 })
                  ,("2.0", 2.0, { round = VALUE 2, floor = VALUE 2, ceil = VALUE 2, trunc = VALUE 2 })
                  ,("2.25", 2.25, { round = VALUE 2, floor = VALUE 2, ceil = VALUE 3, trunc = VALUE 2 })
                  ,("2.5", 2.5, { round = VALUE 2, floor = VALUE 2, ceil = VALUE 3, trunc = VALUE 2 })
                  ,("2.75", 2.75, { round = VALUE 3, floor = VALUE 2, ceil = VALUE 3, trunc = VALUE 2 })
                  ,("3.0", 3.0, { round = VALUE 3, floor = VALUE 3, ceil = VALUE 3, trunc = VALUE 3 })
                  ,("3.25", 3.25, { round = VALUE 3, floor = VALUE 3, ceil = VALUE 4, trunc = VALUE 3 })
                  ,("3.5", 3.5, { round = VALUE 4, floor = VALUE 3, ceil = VALUE 4, trunc = VALUE 3 })
                  ,("maxFinite", Real.maxFinite, { round = VALUE maxFiniteAsLargeInt, floor = VALUE maxFiniteAsLargeInt, ceil = VALUE maxFiniteAsLargeInt, trunc = VALUE maxFiniteAsLargeInt })
                  ,("posInf", Real.posInf, { round = OVERFLOW, floor = OVERFLOW, ceil = OVERFLOW, trunc = OVERFLOW })
                  ,("NaN", NaN, { round = DOMAIN, floor = DOMAIN, ceil = DOMAIN, trunc = DOMAIN })
                  ];
